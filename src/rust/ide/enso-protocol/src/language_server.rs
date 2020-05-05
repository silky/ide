//! Client library for the Language Server part of the Enso Protocol.
//!
//! Please refer to https://github.com/luna/enso/blob/master/doc/language-server/specification/enso-protocol.md#protocol-message-specification---language-server
//! for the full protocol documentation and discussion on the types and terms used here.
//!
//! Also, the Enso Protocol specification is source for many names and comments used here.
//! This file tries to follow the scheme of the protocol specification.

use crate::prelude::*;

use crate::types::UTCDateTime;

use json_rpc::api::Result;
use json_rpc::Handler;
use json_rpc::make_rpc_methods;
use futures::Stream;
use serde::Serialize;
use serde::Deserialize;
use std::future::Future;
use uuid::Uuid;



// =============
// === Event ===
// =============

/// Event emitted by the Language Server `Client`.
pub type Event = json_rpc::handler::Event<Notification>;



// ============
// === Path ===
// ============

/// A path is a representation of a path relative to a specified content root.
// FIXME [mwu] Consider rename to something like `FilePath`, see https://github.com/luna/enso/issues/708
#[derive(Clone,Debug,Serialize,Deserialize,Hash,PartialEq,Eq)]
#[serde(rename_all = "camelCase")]
pub struct Path {
    /// Path's root id.
    pub root_id:Uuid,
    /// Path's segments.
    pub segments:Vec<String>,
}

impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "//{}/", self.root_id)?;
        write!(f, "{}", self.segments.join("/"))
    }
}



// ====================
// === Notification ===
// ====================

/// Notification generated by the File Manager.
#[derive(Clone,Debug,PartialEq)]
#[derive(Serialize,Deserialize)]
#[serde(tag="method", content="params")]
pub enum Notification {
    /// Filesystem event occurred for a watched path.
    #[serde(rename = "file/event")]
    FileEvent {
        /// The `file/event` notification input wrapper.
        /// The serialization format requires the information to be wrapped into a field named "event".
        /// This behavior is currently not specified by the specification and the issue has been raised
        /// to address this: https://github.com/luna/enso/issues/707
        // TODO [mwu] Update as the issue is resolved on way or another.
        event:FileEvent,
    }
}



// =================
// === FileEvent ===
// =================

/// The `file/event` notification parameters.
#[derive(Clone,Debug,PartialEq)]
#[derive(Serialize,Deserialize)]
#[allow(missing_docs)]
pub struct FileEvent {
    pub path : Path,
    pub kind : FileEventKind,
}

/// Describes kind of filesystem event (was the file created or deleted, etc.)
#[derive(Clone,Copy,Debug,PartialEq)]
#[derive(Serialize,Deserialize)]
#[allow(missing_docs)]
pub enum FileEventKind {
    Added,
    Removed,
    Modified,
}



// ======================
// === FileAttributes ===
// ======================

/// Attributes of the file in the filesystem.
#[derive(Clone,Debug,PartialEq,Eq,Hash)]
#[derive(Serialize,Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FileAttributes {
    /// When the file was created.
    pub creation_time:UTCDateTime,
    /// When the file was last accessed.
    pub last_access_time:UTCDateTime,
    /// When the file was last modified.
    pub last_modified_time:UTCDateTime,
    /// What kind of file is this.
    pub kind:FileSystemObject,
    /// Size of the file in bytes.
    /// (size of files not being `RegularFile`s is unspecified).
    pub byte_size:u64,
}

/// A representation of what kind of type a filesystem object can be.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
#[serde(tag = "type")]
#[allow(missing_docs)]
pub enum FileSystemObject {
    Directory {
        name:String,
        path:Path,
    },
    /// A directory which contents have been truncated, i.e. with its subtree not listed
    /// any further due to depth limit being reached.
    // FIXME: To be clarified in https://github.com/luna/enso/issues/708
    DirectoryTruncated {
        name:String,
        path:Path,
    },
    File {
        name:String,
        path:Path,
    },
    /// Represents other, potenatially unrecognized object. Example is a broken symbolic link.
    // FIXME: To be clarified in https://github.com/luna/enso/issues/708
    Other {
        name:String,
        path:Path,
    },
    /// Represents a symbolic link that creates a loop.
    SymlinkLoop {
        name:String,
        path:Path,
        /// A target of the symlink. Since it is a loop, target is a subpath of the symlink.
        target: Path,
    }
}



// =================
// === Responses ===
// =================

/// Helper structures wrapping RPC method result types.
pub mod response {
    use super::*;

    /// Response of `init_protocol_connection` method.
    #[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct InitProtocolConnection {
        /// List of Root IDs.
        pub content_roots:Vec<Uuid>,
    }

    /// Response of `file_read` method.
    #[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
    pub struct Read {
        #[allow(missing_docs)]
        pub contents:String,
    }

    /// Response of `file_exists` method.
    #[derive(Hash,Debug,Clone,Copy,PartialEq,Eq,Serialize,Deserialize)]
    pub struct FileExists {
        #[allow(missing_docs)]
        pub exists:bool,
    }

    /// Response of `file_lst` method.
    #[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
    pub struct FileList {
        #[allow(missing_docs)]
        pub paths:Vec<FileSystemObject>,
    }

    /// Response of `file_info` method.
    #[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
    pub struct FileInfo {
        #[allow(missing_docs)]
        pub attributes: FileAttributes,
    }
}



// =======================
// === RegisterOptions ===
// =======================

/// `capability/acquire` takes method and options specific to the method. This type represents the
/// options. The used variant must match the method. See for details:
/// https://github.com/luna/enso/blob/master/doc/language-server/specification/enso-protocol.md#capabilities
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
#[serde(untagged)]
#[allow(missing_docs)]
pub enum RegisterOptions {
    ReceivesTreeUpdates(ReceivesTreeUpdates)
}

/// `RegisterOptions`' to receive file system tree updates.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
pub struct ReceivesTreeUpdates {
    #[allow(missing_docs)]
    pub path:Path,
}



// ====================
// === API & Client ===
// ====================

make_rpc_methods! {
/// An interface containing all the available file management operations.
trait API {
    /// Initialize the connection used to send the textual protocol messages. This initialisation
    /// is important such that the client identifier can be correlated between the textual and data
    /// connections.
    #[MethodInput=InitProtocolInput,rpc_name="session/initProtocolConnection",
    result=init_protocol_connection_result,set_result=set_init_protocol_connection_result]
    fn init_protocol_connection(&self, client_id:Uuid) -> response::InitProtocolConnection;

    /// Copy a specified file system object to another location.
    #[MethodInput=CopyFileInput,rpc_name="file/copy",result=copy_file_result,
    set_result=set_copy_file_result]
    fn copy_file(&self, from:Path, to:Path) -> ();

    /// Delete the specified file system object.
    #[MethodInput=DeleteFileInput,rpc_name="file/delete",result=delete_file_result,
    set_result=set_delete_file_result]
    fn delete_file(&self, path:Path) -> ();

    /// Check if file system object exists.
    #[MethodInput=FileExistsInput,rpc_name="file/exists",result=file_exists_result,
    set_result=set_file_exists_result]
    fn file_exists(&self, path:Path) -> response::FileExists;

    /// List all file-system objects in the specified path.
    #[MethodInput=FileListInput,rpc_name="file/list",result=file_list_result,
    set_result=set_file_list_result]
    fn file_list(&self, path:Path) -> response::FileList;

    /// Move file system object to another location.
    #[MethodInput=MoveFileInput,rpc_name="file/move",result=move_file_result,
    set_result=set_move_file_result]
    fn move_file(&self, from:Path, to:Path) -> ();

    /// Reads file's content as a String.
    #[MethodInput=ReadFileInput,rpc_name="file/read",result=file_read_result,
    set_result=set_file_read_result]
    fn read_file(&self, path:Path) -> response::Read;

    /// Gets file system object's attributes information.
    #[MethodInput=FileInfoInput,rpc_name="file/info",result=file_info_result,
    set_result=set_file_info_result]
    fn file_info(&self, path:Path) -> response::FileInfo;

    /// Creates the specified file system object.
    #[MethodInput=CreateInput,rpc_name="file/create",result=create_result,
    set_result=set_create_result]
    fn create_file(&self, object:FileSystemObject) -> ();

    /// Writes String contents to a file in the specified path.
    #[MethodInput=FileWriteInput,rpc_name="file/write",result=file_write_result,
    set_result=set_file_write_result]
    fn write_file(&self, path:Path, contents:String) -> ();

    /// Acquire capability permission.
    #[MethodInput=AcquireCapabilityInput,rpc_name="capability/acquire",
    result=acquire_capability_result,set_result=set_acquire_capability_result]
    fn acquire_capability(&self, method:String, register_options:RegisterOptions) -> ();
}}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use futures::task::LocalSpawnExt;
    use json_rpc::messages::Message;
    use json_rpc::messages::RequestMessage;
    use json_rpc::test_util::transport::mock::MockTransport;
    use serde_json::json;
    use serde_json::Value;
    use std::future::Future;
    use utils::test::poll_future_output;
    use utils::test::poll_stream_output;

    struct Fixture {
        transport : MockTransport,
        client    : Client,
        executor  : futures::executor::LocalPool,
    }

    fn setup_language_server() -> Fixture {
        let transport = MockTransport::new();
        let client    = Client::new(transport.clone());
        let executor  = futures::executor::LocalPool::new();
        executor.spawner().spawn_local(client.runner()).unwrap();
        Fixture {transport,client,executor}
    }

    #[test]
    fn test_file_event_notification() {
        let mut fixture = setup_language_server();
        let mut events  = Box::pin(fixture.client.events());
        assert!(poll_stream_output(&mut events).is_none());

        let root_id = uuid::Uuid::parse_str("00000000-0000-0000-0000-000000000000");
        let root_id = root_id.expect("Couldn't parse uuid.");
        let expected_event = FileEvent {
            path : Path{root_id,segments:vec!["Main.txt".into()]},
            kind : FileEventKind::Modified,
        };
        let notification_text = r#"{
            "jsonrpc": "2.0",
            "method": "file/event",
            "params": {
                "event" : {
                    "path" : {
                        "rootId"   : "00000000-0000-0000-0000-000000000000",
                        "segments" : ["Main.txt"]
                    },
                    "kind" : "Modified"
                }
            }
        }"#;
        fixture.transport.mock_peer_message_text(notification_text);
        assert!(poll_stream_output(&mut events).is_none());

        fixture.executor.run_until_stalled();

        let event = poll_stream_output(&mut events);
        if let Some(Event::Notification(n)) = event {
            assert_eq!(n, Notification::FileEvent {event:expected_event});
        } else {
            panic!("expected notification event");
        }
    }

    /// This function tests making a request using file manager. It
    /// * creates FM client and uses `make_request` to make a request,
    /// * checks that request is made for `expected_method`,
    /// * checks that request input is `expected_input`,
    /// * mocks receiving a response from server with `result` and
    /// * checks that FM-returned Future yields `expected_output`.
    fn test_request<Fun, Fut, T>
    ( make_request:Fun
    , expected_method:&str
    , expected_input:Value
    , result:Value
    , expected_output:T )
    where Fun : FnOnce(&mut Client) -> Fut,
          Fut : Future<Output = Result<T>>,
          T   : Debug + PartialEq {
        let mut fixture        = setup_language_server();
        let mut request_future = Box::pin(make_request(&mut fixture.client));

        let request = fixture.transport.expect_message::<RequestMessage<Value>>();
        assert_eq!(request.method, expected_method);
        assert_eq!(request.params, expected_input);

        let response = Message::new_success(request.id, result);
        fixture.transport.mock_peer_message(response);
        fixture.executor.run_until_stalled();
        let output = poll_future_output(&mut request_future).unwrap().unwrap();
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_file_requests() {
        let root_id   = uuid::Uuid::parse_str("00000000-0000-0000-0000-000000000000");
        let root_id   = root_id.expect("Couldn't parse uuid.");
        let main      = Path { root_id, segments: vec!["Main.txt".into()] };
        let target    = Path { root_id, segments: vec!["Target.txt".into()] };
        let path_main = json!({"path" : {
                "rootId"   : "00000000-0000-0000-0000-000000000000",
                "segments" : ["Main.txt"]
            }
        });
        let from_main_to_target = json!({
            "from" : {
                "rootId"   : "00000000-0000-0000-0000-000000000000",
                "segments" : ["Main.txt"]
            },
            "to" : {
                "rootId"   : "00000000-0000-0000-0000-000000000000",
                "segments" : ["Target.txt"]
            }
        });
        let file_exists_json = json!({"exists":true});
        let unit_json = json!(null);

        test_request(
            |client| client.copy_file(main.clone(), target.clone()),
            "file/copy",
            from_main_to_target.clone(),
            unit_json.clone(),
            ());
        test_request(
            |client| client.delete_file(main.clone()),
            "file/delete",
            path_main.clone(),
            unit_json.clone(),
            ());
        test_request(
            |client| client.file_exists(main.clone()),
            "file/exists",
            path_main.clone(),
            file_exists_json,
            response::FileExists { exists: true });

        let list_response_json = json!({
            "paths" : [
                {
                    "type" : "File",
                    "name" : "foo.txt",
                    "path" : {
                        "rootId"   : "00000000-0000-0000-0000-000000000000",
                        "segments" : []
                    }
                },
                {
                    "type" : "File",
                    "name" : "bar.txt",
                    "path" : {
                        "rootId"   : "00000000-0000-0000-0000-000000000000",
                        "segments" : []
                    }
                }
            ]
        });
        let list_response_value = response::FileList {
            paths: vec![
                FileSystemObject::File {
                    name : "foo.txt".into(),
                    path : Path { root_id, segments: default() }
                },
                FileSystemObject::File {
                    name : "bar.txt".into(),
                    path : Path { root_id, segments: default() }
                }
            ]
        };
        test_request(
            |client| client.file_list(main.clone()),
            "file/list",
            path_main.clone(),
            list_response_json,
            list_response_value);
        test_request(
            |client| client.move_file(main.clone(), target.clone()),
            "file/move",
            from_main_to_target.clone(),
            unit_json.clone(),
            ());

        let read_response_json = json!({"contents":"Hello world!"});
        let read_response = response::Read { contents: "Hello world!".into() };
        test_request(
            |client| client.read_file(main.clone()),
            "file/read",
            path_main.clone(),
            read_response_json,
            read_response);

        let parse_rfc3339 = |s| {
            chrono::DateTime::parse_from_rfc3339(s).unwrap()
        };
        let file_system_object = FileSystemObject::File {
            name: "test.txt".into(),
            path: Path {
                root_id,
                segments: default()
            }
        };
        let file_system_object_json = json!({
            "type" : "File",
            "name" : "test.txt",
            "path" : {
                "rootId"   : "00000000-0000-0000-0000-000000000000",
                "segments" : []
            }
        });
        let expected_attributes = response::FileInfo { attributes : FileAttributes {
            creation_time      : parse_rfc3339("2020-01-07T21:25:26Z"),
            last_access_time   : parse_rfc3339("2020-01-21T22:16:51.123994500+00:00"),
            last_modified_time : parse_rfc3339("2020-01-07T21:25:26Z"),
            kind               : file_system_object.clone(),
            byte_size          : 125125,
        }};
        let sample_attributes_json = json!({ "attributes" : {
            "creationTime"     : "2020-01-07T21:25:26Z",
            "lastAccessTime"   : "2020-01-21T22:16:51.123994500+00:00",
            "lastModifiedTime" : "2020-01-07T21:25:26Z",
            "kind"             : file_system_object_json,
            "byteSize" : 125125
        }});
        test_request(
            |client| client.file_info(main.clone()),
            "file/info",
            path_main.clone(),
            sample_attributes_json,
            expected_attributes);
        let create_file_json = json!({
            "object" : file_system_object_json
        });
        test_request(
            |client| client.create_file(file_system_object),
            "file/create",
            create_file_json.clone(),
            unit_json.clone(),
            ());
        test_request(
            |client| client.write_file(main.clone(), "Hello world!".into()),
            "file/write",
            json!({
                "path" : {
                    "rootId"   : "00000000-0000-0000-0000-000000000000",
                    "segments" : ["Main.txt"]
                },
                "contents" : "Hello world!"
            }),
            unit_json.clone(),
            ());
        let init_protocol_connection_response = response::InitProtocolConnection {
            content_roots : vec![uuid::Uuid::default()]
        };
        test_request(
            |client| client.init_protocol_connection(uuid::Uuid::default()),
            "session/initProtocolConnection",
            json!({
                "clientId" : "00000000-0000-0000-0000-000000000000"
            }),
            json!({
                "contentRoots" : ["00000000-0000-0000-0000-000000000000"]
            }),
            init_protocol_connection_response
        );
        let path                  = Path{root_id,segments:default()};
        let receives_tree_updates = ReceivesTreeUpdates{path};
        let options               = RegisterOptions::ReceivesTreeUpdates(receives_tree_updates);
        test_request(
            |client| client.acquire_capability("receivesTreeUpdates".into(),options),
            "capability/acquire",
            json!({
                "method"          : "receivesTreeUpdates",
                "registerOptions" : {
                    "path" : {
                        "rootId"   : "00000000-0000-0000-0000-000000000000",
                        "segments" : []
                    }
                }
            }),
            unit_json,
            ()
        );
    }
}
