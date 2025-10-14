---
title: "Store Configuration / Composition"
slug: store-configuration-composition
description: ""
summary: ""
date: 2025-05-03T16:00:33+00:00
lastmod: 2025-05-03T16:00:33+00:00
draft: false
weight: 15
toc: true
---

Currently, snix-store, snix-cli and other CLI endpoints expose three different
`--*-service-addr` CLI args, describing how to talk to the three different
stores.

Depending on the CLI entrypoint, they have different defaults:

 - `snix-cli` defaults to in-memory variants (`ServiceUrlsMemory`).
 - `snix-store daemon` defaults to using a local filesystem-based backend for
   blobs, and redb backends for `DirectoryService` and `PathInfoService`
   (`ServiceUrls`).
 - other `snix-store` entrypoints, as well as `nar-bridge` default to talking to
   a `snix-store` gRPC daemon (`ServiceUrlsGrpc`).

The exact config and paths can be inspected by invoking `--help` on each of
these entrypoints, and it's of course possible to change this config, for
example in case everything should be done from a single binary, without a daemon
in between.

There currently is no caching on the client side wired up yet, and some (known)
unnecessary roundtrips (which can be removed after some refactoring), so for
everything except testing purposes you might want to directly connect to the
data stores, or use Store Composition to have caching, (and describe more
complicated fetch-through configs).

## Store Composition
Internally, `snix-[ca]store` supports composing multiple instances of
`BlobService`, `DirectoryService` (and `PathInfoService`) together.

This allows describing more complicated "hierarchies"/"tiers" of different
service types, by combining different storage backend/substituters/
caches in a DAG of some sort, with the root still exposing the same (trait)
interface as a single store.

The three individual URLs exposed in the CLI currently are internally converted
to a composition with just one instance of each store (at the "root" name).

Keep in mind the config format is very granular and low-level, and due to this,
a potential subject to larger breaking and unannounced changes, which is why we
it is not exposed by default yet.

In the long term, for "user-facing" configuration, we might want to expose a
more opinionated middle ground between only a single instance and the super
granular store composition instead.

For example, users could configure things like "a list of substituters"
and "caching args", and internally this could be transformed to a low-level
composition - potentially leaving this granular format for library/power users
only.

### CLI usage
However, if you're okay with these caveats, and want to configure some caching
today, using the existing CLI entrypoints, you can enable the
`xp-composition-cli` feature flag in the `snix-store` crate.

With `cargo`, this can be enabled by passing
`--features snix-store/xp-composition-cli` to a `cargo build` / `cargo run`
invocation. [^1]

If enabled, CLI entrypoints get a `--experimental-store-composition` arg, which
accepts a TOML file describing a composition for all three stores (causing the
other `--*-service-addr` args to be ignored if set).

It expects all BlobService instances to be inside a `blobservices` namespace/
attribute, (`DirectoryService`s in `directoryservices`, and `PathInfoService`s
in `pathinfoservices` respectively), and requires one named "root".

### Library usage
The store composition code and documentation and examples can be viewed at
[`snix_castore::composition`][rustdoc-castore-composition] and
[`snix_store::composition`][rustdoc-store-composition].
Make sure to check them out.

A global "registry" can be used to make other (out-of-tree) "types" of stores
known to the composition machinery.

In terms of config format, you're also not required to use TOML, but anything
`serde` can deserialize.

### Composition config format
Below examples are in the format accepted by the CLI, using the
`blobservices` / `directoryservices` / `pathinfoservices` namespace/attribute to
describe all three services.

However, as expressed above, for library users this doesn't need to be TOML (but
anything serde can deserialize), and the composition hierarchy needs to be built
separately for each `{Blob,Directory,Pathinfo}Service`, dropping the namespaces
present in the TOML.

#### Example: combined remote/local blobservice
This fetches blobs from a local store. If not found there, a remote store is
queried, and results are returned to the client and inserted into the local
store, to make subsequent lookups not query the remote again.

```toml
[blobservices.root]
type = "combined"
near = "near"
far = "far"

[blobservices.near]
type = "objectstore"
object_store_url = "file:///tmp/snix/blobservice"
object_store_options = {}

[blobservices.far]
type = "grpc"
url = "grpc+http://[::1]:8000"

# […] directoryservices/pathinfoservices go here […]
```

### Example: LRU cache wrapping pathinfoservice
This keeps the last 1000 requested `PathInfo`s around in a local cache.
```toml
[pathinfoservices.root]
type = "cache"
near = "near"
far = "far"

[pathinfoservices.near]
type = "lru"
capacity = 1000

[pathinfoservices.far]
type = "grpc"
url = "grpc+http://localhost:8000"

# […] blobservices/directoryservices go here […]
```

### Example: Self-contained fetch-through snIx-store for `cache.nixos.org`.
This provides a `PathInfoService` "containing" `PathInfo` that are in
`cache.nixos.org`.

To construct the `PathInfo` initially, we need to ingest the NAR to add missing
castore contents to `BlobService` / `DirectoryService` and return the resulting
root node.

To not do this every time, the resulting `PathInfo` is saved in a local (`redb`)
database.

This also showcases how PathInfo services can refer to other store types (blob
services, directory services).

```toml
[blobservices.root]
type = "objectstore"
object_store_url = "file:///var/lib/snix-castore/blobs"
object_store_options = {}

[directoryservices.root]
type = "redb"
is_temporary = false
path = "/var/lib/snix-castore/directories.redb"

[pathinfoservices.root]
type = "cache"
near = "redb"
far = "cache-nixos-org"

[pathinfoservices.redb]
type = "redb"
is_temporary = false
path = "/var/lib/snix-store/pathinfo.redb"

[pathinfoservices.cache-nixos-org]
type = "nix"
base_url = "https://cache.nixos.org"
public_keys = ["cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="]
blob_service = "root"
directory_service = "root"
```

[rustdoc-castore-composition]: https://snix.dev/rustdoc/snix_castore/composition/index.html
[rustdoc-store-composition]: https://snix.dev/rustdoc/snix_store/composition/index.html
[^1]: In some leaf binary crates, this can also be controlled via the `xp-store-composition-cli` feature in the leaf crate itself.
