use futures::stream::BoxStream;
use futures::{StreamExt, TryStreamExt};
use nix_compat::store_path::StorePathRef;
use snix_castore::fs::{RootNodes, SnixStoreFs};
use snix_castore::{Error, Node, PathComponent};
use snix_castore::{blobservice::BlobService, directoryservice::DirectoryService};
use tonic::async_trait;

use super::PathInfoService;

/// Helper to construct a [SnixStoreFs] from a [BlobService], [DirectoryService]
/// and [PathInfoService].
/// This avoids users to have to interact with the wrapper struct directly, as
/// it leaks into the type signature of SnixStoreFS.
pub fn make_fs<BS, DS, PS>(
    blob_service: BS,
    directory_service: DS,
    path_info_service: PS,
    list_root: bool,
    show_xattr: bool,
) -> SnixStoreFs<BS, DS, RootNodesWrapper<PS>>
where
    BS: BlobService + Send + Clone + 'static,
    DS: DirectoryService + Send + Clone + 'static,
    PS: PathInfoService + Send + Sync + Clone + 'static,
{
    SnixStoreFs::new(
        blob_service,
        directory_service,
        RootNodesWrapper(path_info_service),
        list_root,
        show_xattr,
    )
}

/// Wrapper to satisfy Rust's orphan rules for trait implementations, as
/// RootNodes is coming from the [snix-castore] crate.
#[doc(hidden)]
#[derive(Clone, Debug)]
pub struct RootNodesWrapper<T>(pub(crate) T);

/// Implements root node lookup for any [PathInfoService]. This represents a flat
/// directory structure like /nix/store where each entry in the root filesystem
/// directory corresponds to a CA node.
#[cfg(any(feature = "fuse", feature = "virtiofs"))]
#[async_trait]
impl<T> RootNodes for RootNodesWrapper<T>
where
    T: PathInfoService,
{
    async fn get_by_basename(&self, name: &PathComponent) -> Result<Option<Node>, Error> {
        let Ok(store_path) = StorePathRef::from_bytes(name.as_ref()) else {
            return Ok(None);
        };

        Ok(self
            .0
            .get(*store_path.digest())
            .await?
            .map(|path_info| path_info.node))
    }

    fn list(&self) -> BoxStream<'static, Result<(PathComponent, Node), Error>> {
        self.0
            .list()
            .map_ok(|path_info| {
                let name = path_info
                    .store_path
                    .to_string()
                    .as_str()
                    .try_into()
                    .expect("Snix bug: StorePath must be PathComponent");
                (name, path_info.node)
            })
            .boxed()
    }
}
