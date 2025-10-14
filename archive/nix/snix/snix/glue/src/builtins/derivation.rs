//! Implements `builtins.derivation`, the core of what makes Nix build packages.
use crate::builtins::DerivationError;
use crate::known_paths::KnownPaths;
use crate::snix_store_io::SnixStoreIO;
use bstr::BString;
use nix_compat::derivation::{Derivation, Output};
use nix_compat::nixhash::{CAHash, HashAlgo, NixHash};
use nix_compat::store_path::{StorePath, StorePathRef};
use snix_eval::builtin_macros::builtins;
use snix_eval::generators::{self, GenCo, emit_warning_kind};
use snix_eval::{
    AddContext, ErrorKind, NixAttrs, NixContext, NixContextElement, Value, WarningKind,
};
use std::collections::{BTreeSet, btree_map};
use std::rc::Rc;

// Constants used for strangely named fields in derivation inputs.
const STRUCTURED_ATTRS: &str = "__structuredAttrs";
const IGNORE_NULLS: &str = "__ignoreNulls";

/// Populate the inputs of a derivation from the build references
/// found when scanning the derivation's parameters and extracting their contexts.
fn populate_inputs(drv: &mut Derivation, full_context: NixContext, known_paths: &KnownPaths) {
    for element in full_context.iter() {
        match element {
            NixContextElement::Plain(source) => {
                let sp = StorePathRef::from_absolute_path(source.as_bytes())
                    .expect("invalid store path")
                    .to_owned();
                drv.input_sources.insert(sp);
            }

            NixContextElement::Single {
                name,
                derivation: derivation_str,
            } => {
                // TODO: b/264
                // We assume derivations to be passed validated, so ignoring rest
                // and expecting parsing is ok.
                let (derivation, _rest) =
                    StorePath::from_absolute_path_full(derivation_str).expect("valid store path");

                #[cfg(debug_assertions)]
                assert!(
                    _rest.iter().next().is_none(),
                    "Extra path not empty for {derivation_str}"
                );

                match drv.input_derivations.entry(derivation.clone()) {
                    btree_map::Entry::Vacant(entry) => {
                        entry.insert(BTreeSet::from([name.clone()]));
                    }

                    btree_map::Entry::Occupied(mut entry) => {
                        entry.get_mut().insert(name.clone());
                    }
                }
            }

            NixContextElement::Derivation(drv_path) => {
                let (derivation, _rest) =
                    StorePath::from_absolute_path_full(drv_path).expect("valid store path");

                #[cfg(debug_assertions)]
                assert!(
                    _rest.iter().next().is_none(),
                    "Extra path not empty for {drv_path}"
                );

                // We need to know all the outputs *names* of that derivation.
                let output_names = known_paths
                    .get_drv_by_drvpath(&derivation)
                    .expect("no known derivation associated to that derivation path")
                    .outputs
                    .keys();

                // FUTUREWORK(performance): ideally, we should be able to clone
                // cheaply those outputs rather than duplicate them all around.
                match drv.input_derivations.entry(derivation.clone()) {
                    btree_map::Entry::Vacant(entry) => {
                        entry.insert(output_names.cloned().collect());
                    }

                    btree_map::Entry::Occupied(mut entry) => {
                        entry.get_mut().extend(output_names.cloned());
                    }
                }

                drv.input_sources.insert(derivation);
            }
        }
    }
}

/// Populate the output configuration of a derivation based on the
/// parameters passed to the call, configuring a fixed-output derivation output
/// if necessary.
///
/// This function handles all possible combinations of the
/// parameters, including invalid ones.
///
/// Due to the support for SRI hashes, and how these are passed along to
/// builtins.derivation, outputHash and outputHashAlgo can have values which
/// need to be further modified before constructing the Derivation struct.
///
/// If outputHashAlgo is an SRI hash, outputHashAlgo must either be an empty
/// string, or the hash algorithm as specified in the (single) SRI (entry).
/// SRI strings with multiple hash algorithms are not supported.
///
/// In case an SRI string was used, the (single) fixed output is populated
/// with the hash algo name, and the hash digest is populated with the
/// (lowercase) hex encoding of the digest.
///
/// These values are only rewritten for the outputs, not what's passed to env.
///
/// The return value may optionally contain a warning.
fn handle_fixed_output(
    drv: &mut Derivation,
    hash_str: Option<String>,      // in nix: outputHash
    hash_algo_str: Option<String>, // in nix: outputHashAlgo
    hash_mode_str: Option<String>, // in nix: outputHashmode
) -> Result<Option<WarningKind>, ErrorKind> {
    // If outputHash is provided, ensure hash_algo_str is compatible.
    // If outputHash is not provided, do nothing.
    if let Some(hash_str) = hash_str {
        // treat an empty algo as None
        let hash_algo_str = match hash_algo_str {
            Some(s) if s.is_empty() => None,
            Some(s) => Some(s),
            None => None,
        };

        let hash_algo = hash_algo_str
            .map(|s| HashAlgo::try_from(s.as_str()))
            .transpose()
            .map_err(DerivationError::InvalidOutputHash)?;

        // construct a NixHash.
        let nixhash =
            NixHash::from_str(&hash_str, hash_algo).map_err(DerivationError::InvalidOutputHash)?;
        let algo = nixhash.algo();

        // construct the fixed output.
        drv.outputs.insert(
            "out".to_string(),
            Output {
                path: None,
                ca_hash: match hash_mode_str.as_deref() {
                    None | Some("flat") => Some(CAHash::Flat(nixhash)),
                    Some("recursive") => Some(CAHash::Nar(nixhash)),
                    Some(other) => {
                        return Err(DerivationError::InvalidOutputHashMode(other.to_string()))?;
                    }
                },
            },
        );

        // Peek at hash_str once more.
        // If it was a SRI hash, but is not using the correct length, this means
        // the padding was wrong. Emit a warning in that case.
        let sri_prefix = format!("{algo}-");
        if let Some(rest) = hash_str.strip_prefix(&sri_prefix) {
            if data_encoding::BASE64.encode_len(algo.digest_length()) != rest.len() {
                return Ok(Some(WarningKind::SRIHashWrongPadding));
            }
        }
    }
    Ok(None)
}

#[builtins(state = "Rc<SnixStoreIO>")]
pub(crate) mod derivation_builtins {
    use std::collections::BTreeMap;

    use bstr::ByteSlice;

    use nix_compat::derivation::validate_output_name;
    use nix_compat::store_path::hash_placeholder;
    use snix_eval::generators::Gen;
    use snix_eval::{NixContext, NixContextElement, NixString, try_cek_to_value};

    use crate::builtins::utils::{select_string, strong_importing_coerce_to_string};
    use crate::fetchurl::fetchurl_derivation_to_fetch;

    use super::*;

    #[builtin("placeholder")]
    async fn builtin_placeholder(co: GenCo, input: Value) -> Result<Value, ErrorKind> {
        if input.is_catchable() {
            return Ok(input);
        }

        let nix_string = input
            .to_str()
            .context("looking at output name in builtins.placeholder")?;
        let output_name = nix_string.to_str()?;

        // Validate the output name using the same rules as derivations
        validate_output_name(output_name).map_err(|e| {
            ErrorKind::Abort(format!("invalid output name in builtins.placeholder: {e}"))
        })?;

        let placeholder = hash_placeholder(output_name);

        Ok(placeholder.into())
    }

    /// Strictly construct a Nix derivation from the supplied arguments.
    ///
    /// This is considered an internal function, users usually want to
    /// use the higher-level `builtins.derivation` instead.
    #[builtin("derivationStrict")]
    async fn builtin_derivation_strict(
        state: Rc<SnixStoreIO>,
        co: GenCo,
        input: Value,
    ) -> Result<Value, ErrorKind> {
        if input.is_catchable() {
            return Ok(input);
        }

        let input = input.to_attrs()?;
        let name = generators::request_force(&co, input.select_required("name")?.clone()).await;

        if name.is_catchable() {
            return Ok(name);
        }

        let name = name.to_str().context("determining derivation name")?;
        if name.is_empty() {
            return Err(ErrorKind::Abort("derivation has empty name".to_string()));
        }
        let name = name.to_str()?;

        let mut drv = Derivation::default();
        drv.outputs.insert("out".to_string(), Default::default());
        let mut input_context = NixContext::new();

        /// Inserts a key and value into the drv.environment BTreeMap, and fails if the
        /// key did already exist before.
        fn insert_env(
            drv: &mut Derivation,
            k: &str, /* TODO: non-utf8 env keys */
            v: BString,
        ) -> Result<(), DerivationError> {
            if drv.environment.insert(k.into(), v).is_some() {
                return Err(DerivationError::DuplicateEnvVar(k.into()));
            }
            Ok(())
        }

        // Check whether null attributes should be ignored or passed through.
        let ignore_nulls = match input.select(IGNORE_NULLS) {
            Some(b) => generators::request_force(&co, b.clone()).await.as_bool()?,
            None => false,
        };

        // peek at the STRUCTURED_ATTRS argument.
        // If it's set and true, provide a BTreeMap that gets populated while looking at the arguments.
        // We need it to be a BTreeMap, so iteration order of keys is reproducible.
        let mut structured_attrs: Option<BTreeMap<String, serde_json::Value>> =
            match input.select(STRUCTURED_ATTRS) {
                Some(b) => generators::request_force(&co, b.clone())
                    .await
                    .as_bool()?
                    .then_some(Default::default()),
                None => None,
            };

        // Look at the arguments passed to builtins.derivationStrict.
        // Some set special fields in the Derivation struct, some change
        // behaviour of other functionality.
        for (arg_name, arg_value) in input.clone().into_iter_sorted() {
            let arg_name = arg_name.to_str()?;
            // force the current value.
            let value = generators::request_force(&co, arg_value).await;

            // filter out nulls if ignore_nulls is set.
            if ignore_nulls && matches!(value, Value::Null) {
                continue;
            }

            match arg_name {
                // Command line arguments to the builder.
                // These are only set in drv.arguments.
                "args" => {
                    for arg in value.to_list()? {
                        let s =
                            try_cek_to_value!(strong_importing_coerce_to_string(&co, arg).await);
                        input_context.mimic(&s);
                        drv.arguments.push(s.to_str()?.to_owned())
                    }
                }

                // If outputs is set, remove the original default `out` output,
                // and replace it with the list of outputs.
                "outputs" => {
                    let outputs = value
                        .to_list()
                        .context("looking at the `outputs` parameter of the derivation")?;

                    // Remove the original default `out` output.
                    drv.outputs.clear();

                    let mut output_names = Vec::with_capacity(outputs.len());

                    for output in outputs {
                        let output_name = generators::request_force(&co, output)
                            .await
                            .to_str()
                            .context("determining output name")?;

                        input_context.mimic(&output_name);

                        // Populate drv.outputs
                        if drv
                            .outputs
                            .insert(output_name.to_str()?.to_owned(), Default::default())
                            .is_some()
                        {
                            Err(DerivationError::DuplicateOutput(
                                output_name.to_str_lossy().into_owned(),
                            ))?
                        }
                        output_names.push(output_name.to_str()?.to_owned());
                    }

                    match structured_attrs.as_mut() {
                        // add outputs to the json itself (as a list of strings)
                        Some(structured_attrs) => {
                            structured_attrs.insert(arg_name.into(), output_names.into());
                        }
                        // add drv.environment["outputs"] as a space-separated list
                        None => {
                            insert_env(&mut drv, arg_name, output_names.join(" ").into())?;
                        }
                    }
                    // drv.environment[$output_name] is added after the loop,
                    // with whatever is in drv.outputs[$output_name].
                }

                // handle builder and system.
                "builder" | "system" => {
                    let val_str =
                        try_cek_to_value!(strong_importing_coerce_to_string(&co, value).await);
                    input_context.mimic(&val_str);

                    if arg_name == "builder" {
                        val_str.to_str()?.clone_into(&mut drv.builder);
                    } else {
                        val_str.to_str()?.clone_into(&mut drv.system);
                    }

                    // Either populate drv.environment or structured_attrs.
                    if let Some(ref mut structured_attrs) = structured_attrs {
                        // No need to check for dups, we only iterate over every attribute name once
                        structured_attrs
                            .insert(arg_name.to_owned(), val_str.to_str()?.to_owned().into());
                    } else {
                        insert_env(&mut drv, arg_name, val_str.as_bytes().into())?;
                    }
                }

                // Don't add STRUCTURED_ATTRS if enabled.
                STRUCTURED_ATTRS if structured_attrs.is_some() => continue,
                // IGNORE_NULLS is always skipped, even if it's not set to true.
                IGNORE_NULLS => continue,

                // all other args.
                _ => {
                    // In SA case, force and add to structured attrs.
                    // In non-SA case, coerce to string and add to env.
                    if let Some(ref mut structured_attrs) = structured_attrs {
                        let val = generators::request_force(&co, value).await;
                        if val.is_catchable() {
                            return Ok(val);
                        }

                        let (val_json, context) = val.into_contextful_json(&co).await?;
                        input_context.extend(context.into_iter());

                        // No need to check for dups, we only iterate over every attribute name once
                        structured_attrs.insert(arg_name.to_owned(), val_json);
                    } else {
                        let val_str =
                            try_cek_to_value!(strong_importing_coerce_to_string(&co, value).await);
                        input_context.mimic(&val_str);

                        insert_env(&mut drv, arg_name, val_str.as_bytes().into())?;
                    }
                }
            }
        }
        // end of per-argument loop

        // Configure fixed-output derivations if required.
        {
            let output_hash = try_cek_to_value!(
                select_string(&co, &input, "outputHash")
                    .await
                    .context("evaluating the `outputHash` parameter")?
            );
            let output_hash_algo = try_cek_to_value!(
                select_string(&co, &input, "outputHashAlgo")
                    .await
                    .context("evaluating the `outputHashAlgo` parameter")?
            );
            let output_hash_mode = try_cek_to_value!(
                select_string(&co, &input, "outputHashMode")
                    .await
                    .context("evaluating the `outputHashMode` parameter")?
            );

            if let Some(warning) =
                handle_fixed_output(&mut drv, output_hash, output_hash_algo, output_hash_mode)?
            {
                emit_warning_kind(&co, warning).await;
            }
        }

        // Each output name needs to exist in the environment, at this
        // point initialised as an empty string, as the ATerm serialization of that is later
        // used for the output path calculation (which will also update output
        // paths post-calculation, both in drv.environment and drv.outputs)
        for output in drv.outputs.keys() {
            if drv
                .environment
                .insert(output.to_string(), String::new().into())
                .is_some()
            {
                emit_warning_kind(&co, WarningKind::ShadowedOutput(output.to_string())).await;
            }
        }

        if let Some(structured_attrs) = structured_attrs {
            // configure __json
            drv.environment.insert(
                "__json".to_string(),
                BString::from(serde_json::to_string(&structured_attrs)?),
            );
        }

        let mut known_paths = state.as_ref().known_paths.borrow_mut();
        populate_inputs(&mut drv, input_context, &known_paths);

        // At this point, derivation fields are fully populated from
        // eval data structures.
        drv.validate(false)
            .map_err(DerivationError::InvalidDerivation)?;

        // Calculate the hash_derivation_modulo for the current derivation..
        debug_assert!(
            drv.outputs.values().all(|output| { output.path.is_none() }),
            "outputs should still be unset"
        );

        // Mutate the Derivation struct and set output paths
        drv.calculate_output_paths(
            name,
            // This one is still intermediate (so not added to known_paths),
            // as the outputs are still unset.
            &drv.hash_derivation_modulo(|drv_path| {
                *known_paths
                    .get_hash_derivation_modulo(&drv_path.to_owned())
                    .unwrap_or_else(|| panic!("{drv_path} not found"))
            }),
        )
        .map_err(DerivationError::InvalidDerivation)?;

        let drv_path = drv
            .calculate_derivation_path(name)
            .map_err(DerivationError::InvalidDerivation)?;

        // Assemble the attrset to return from this builtin.
        let out = Value::Attrs(Box::new(NixAttrs::from_iter(
            drv.outputs
                .iter()
                .map(|(name, output)| {
                    (
                        name.clone(),
                        NixString::new_context_from(
                            NixContextElement::Single {
                                name: name.clone(),
                                derivation: drv_path.to_absolute_path(),
                            }
                            .into(),
                            output.path.as_ref().unwrap().to_absolute_path(),
                        ),
                    )
                })
                .chain(std::iter::once((
                    "drvPath".to_owned(),
                    NixString::new_context_from(
                        NixContextElement::Derivation(drv_path.to_absolute_path()).into(),
                        drv_path.to_absolute_path(),
                    ),
                ))),
        )));

        // If the derivation is a fake derivation (builtin:fetchurl),
        // synthesize a [Fetch] and add it there, too.
        if drv.builder == "builtin:fetchurl" {
            let (name, fetch) =
                fetchurl_derivation_to_fetch(&drv).map_err(|e| ErrorKind::SnixError(Rc::new(e)))?;

            known_paths
                .add_fetch(fetch, &name)
                .map_err(|e| ErrorKind::SnixError(Rc::new(e)))?;
        }

        // Register the Derivation in known_paths.
        known_paths.add_derivation(drv_path, drv);

        Ok(out)
    }
}
