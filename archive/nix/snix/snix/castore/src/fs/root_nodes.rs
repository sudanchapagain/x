use std::collections::BTreeMap;

use crate::nodes::Directory;
use crate::{Error, Node, path::PathComponent};
use futures::StreamExt;
use futures::stream::BoxStream;
use tonic::async_trait;

/// Provides an interface for looking up root nodes  in snix-castore by given
/// a lookup key (usually the basename), and optionally allow a listing.
#[async_trait]
pub trait RootNodes {
    /// Looks up a root CA node based on the basename of the node in the root
    /// directory of the filesystem.
    async fn get_by_basename(&self, name: &PathComponent) -> Result<Option<Node>, Error>;

    /// Lists all root CA nodes in the filesystem, as a tuple of (base)name
    /// and Node.
    /// An error can be returned in case listing is not allowed.
    fn list(&self) -> BoxStream<'static, Result<(PathComponent, Node), Error>>;
}

#[async_trait]
/// Implements RootNodes for something deref'ing to a BTreeMap of Nodes, where
/// the key is the node name.
impl<T> RootNodes for T
where
    T: AsRef<BTreeMap<PathComponent, Node>> + Send + Sync,
{
    async fn get_by_basename(&self, name: &PathComponent) -> Result<Option<Node>, Error> {
        Ok(self.as_ref().get(name).cloned())
    }

    fn list(&self) -> BoxStream<'static, Result<(PathComponent, Node), Error>> {
        let data = self.as_ref().to_owned();
        futures::stream::iter(data.into_iter().map(Ok)).boxed()
    }
}

#[async_trait]
impl RootNodes for Directory {
    async fn get_by_basename(&self, name: &PathComponent) -> Result<Option<Node>, Error> {
        Ok(self
            .nodes()
            .find(|(key, _)| *key == name)
            .map(|(_, node)| node.clone()))
    }

    fn list(&self) -> BoxStream<'static, Result<(PathComponent, Node), Error>> {
        let data = self.to_owned();
        futures::stream::iter(data.into_nodes().map(Ok)).boxed()
    }
}
