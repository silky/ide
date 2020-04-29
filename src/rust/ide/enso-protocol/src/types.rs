//! Common types of JSON-RPC-based Enso services used by both Project Manager and File Manager.

use crate::prelude::*;

use serde::Serialize;
use serde::Deserialize;

/// Time in UTC time zone.
pub type UTCDateTime = chrono::DateTime<chrono::FixedOffset>;

// === Uuid ===

/// Uuid.
#[derive(Default,Debug,Clone,Copy,Serialize,Deserialize,PartialEq,Shrinkwrap,Eq,Hash)]
pub struct Uuid {
    #[allow(missing_docs)]
    #[serde(flatten)]
    pub id : uuid::Uuid
}

impl AsRef<Uuid> for Uuid {
    fn as_ref(&self) -> &Uuid {
        self
    }
}
