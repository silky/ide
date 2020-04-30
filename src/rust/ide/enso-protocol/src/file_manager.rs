//! Client library for the JSON-RPC-based File Manager service.

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

/// Event emitted by the File Manager `Client`.
pub type Event = json_rpc::handler::Event<Notification>;



// ============
// === Path ===
// ============

/// A path is a representation of a path relative to a specified content root.
#[derive(Clone,Debug,Serialize,Deserialize,Hash,PartialEq,Eq)]
#[serde(rename_all = "camelCase")]
pub struct Path {
    /// Path's root id.
    pub root_id : Uuid,
    /// Path's segments.
    pub segments : Vec<String>
}

impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = write!(f, "{}", self.root_id);
        for segment in &self.segments {
            write!(f, "/{}", segment)?
        }
        result
    }
}



// ====================
// === Notification ===
// ====================

/// Notification generated by the File Manager.
#[derive(Clone,Debug,PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(tag="method", content="params")]
pub enum Notification {
    /// Filesystem event occurred for a watched path.
    #[serde(rename = "file/event")]
    FilesystemEvent(FilesystemEvent),
}



// =======================
// === FilesystemEvent ===
// =======================

/// Filesystem event notification, generated by an active file watch.
#[derive(Clone,Debug,PartialEq)]
#[derive(Serialize, Deserialize)]
pub struct FilesystemEvent {
    #[allow(missing_docs)]
    // The serialization format requires the information to be wrapped around an "event" property.
    pub event : FilesystemEventInfo
}



// ===========================
// === FilesystemEventInfo ===
// ===========================

/// FilesystemEvent's kind and path of the file that the event is about.
#[derive(Clone,Debug,PartialEq)]
#[derive(Serialize, Deserialize)]
#[allow(missing_docs)]
pub struct FilesystemEventInfo {
    pub path : Path,
    pub kind : FilesystemEventKind
}


/// Describes kind of filesystem event (was the file created or deleted, etc.)
#[derive(Clone,Copy,Debug,PartialEq)]
#[derive(Serialize, Deserialize)]
pub enum FilesystemEventKind {
    /// A new file under path was added.
    Added,
    /// Existing file under path was removed.
    Removed,
    /// File under path was modified.
    Modified
}



// ==================
// === Attributes ===
// ==================

/// Attributes of the file in the filesystem.
#[derive(Clone,Copy,Debug,PartialEq)]
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Attributes{
    /// When the file was created.
    pub creation_time      : UTCDateTime,
    /// When the file was last accessed.
    pub last_access_time   : UTCDateTime,
    /// When the file was last modified.
    pub last_modified_time : UTCDateTime,
    /// What kind of file is this.
    pub file_kind          : FileKind,
    /// Size of the file in bytes.
    /// (size of files not being `RegularFile`s is unspecified).
    pub byte_size          : u64
}

/// What kind of file (regular, directory, symlink) is this.
#[derive(Clone,Copy,Debug,PartialEq)]
#[derive(Serialize, Deserialize)]
pub enum FileKind {
    /// File being a directory.
    Directory,
    /// File being a symbolic link.
    SymbolicLink,
    /// File being a regular file with opaque content.
    RegularFile,
    /// File being none of the above, e.g. a physical device or a pipe.
    Other
}

/// A representation of what kind of type a filesystem object can be.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
#[serde(tag = "type")]
pub enum FileSystemObject {
    /// Represents a directory.
    Directory(Object),
    /// Represents a directory which contents have been truncated
    DirectoryTruncated(Object),
    /// Represents a file.
    File(Object),
    /// Represents unrecognized object. Example is a broken symbolic link.
    Other(Object),
    /// Represents a symbolic link that creates a loop.
    SymlinkLoop(SymlinkLoop)
}

/// Represents an object.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
pub struct Object {
    /// Name of the object.
    pub name : String,
    /// Path to the object.
    pub path : Path
}

/// Represents a symbolic link that creates a loop.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
pub struct SymlinkLoop {
    /// Name of the symlink.
    pub name : String,
    /// Path to the symlink.
    pub path : Path,
    /// A target of the symlink. Since it is a loop, target is a subpath of the symlink.
    pub target : Path
}

/// Response of `init_protocol_connection` method.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitProtocolConnectionResponse {
    /// List of Root IDs.
    pub content_roots : Vec<Uuid>
}

/// Response of `file_read` method.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
pub struct ReadResponse {
    #[allow(missing_docs)]
    pub contents : String
}

/// Response of `file_exists` method.
#[derive(Hash,Debug,Clone,Copy,PartialEq,Eq,Serialize,Deserialize)]
pub struct FileExistsResponse {
    #[allow(missing_docs)]
    pub exists : bool
}

/// `RegisterOptions`' to receive file system tree updates.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
pub struct ReceivesTreeUpdates {
    #[allow(missing_docs)]
    pub path : Path
}

/// `acquire_capability`'s register options.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
#[serde(untagged)]
#[allow(missing_docs)]
pub enum RegisterOptions {
    ReceivesTreeUpdates(ReceivesTreeUpdates)
}

make_rpc_methods! {
/// An interface containing all the available file management operations.
trait API {
    /// Initialize the connection used to send the textual protocol messages. This initialisation
    /// is important such that the client identifier can be correlated between the textual and data
    /// connections.
    #[MethodInput=InitProtocolInput,rpc_name="session/initProtocolConnection",
    result=init_protocol_connection_result,set_result=set_init_protocol_connection_result]
    fn init_protocol_connection(&self, client_id:Uuid) -> InitProtocolConnectionResponse;

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
    fn file_exists(&self, path:Path) -> FileExistsResponse;

    /// List all file-system objects in the specified path.
    #[MethodInput=FileListInput,rpc_name="file/list",result=file_list_result,
    set_result=set_file_list_result]
    fn file_list(&self, path:Path) -> Vec<Path>;

    /// Move file system object to another location.
    #[MethodInput=MoveFileInput,rpc_name="file/move",result=move_file_result,
    set_result=set_move_file_result]
    fn move_file(&self, from:Path, to:Path) -> ();

    /// Reads file's content as a String.
    #[MethodInput=ReadFileInput,rpc_name="file/read",result=file_read_result,
    set_result=set_file_read_result]
    fn read_file(&self, path:Path) -> ReadResponse;

    /// Gets file system object's status.
    #[MethodInput=FileStatusInput,rpc_name="file/status",result=file_status_result,
    set_result=set_file_status_result]
    fn file_status(&self, path:Path) -> Attributes;

    /// Adds a content root to the active project.
    #[MethodInput=AddRootInput,rpc_name="file/addRoot",result=add_root_result,
    set_result=set_add_root_result]
    fn add_root(&self, absolute_path:Vec<String>, id:Uuid) -> ();

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
    use super::FileKind::RegularFile;

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

    fn setup_file_manager() -> Fixture {
        let transport = MockTransport::new();
        let client    = Client::new(transport.clone());
        let executor  = futures::executor::LocalPool::new();
        executor.spawner().spawn_local(client.runner()).unwrap();
        Fixture {transport,client,executor}
    }

    #[test]
    fn test_notification() {
        let mut fixture = setup_file_manager();
        let mut events  = Box::pin(fixture.client.events());
        assert!(poll_stream_output(&mut events).is_none());

        let root_id = uuid::Uuid::parse_str("00000000-0000-0000-0000-000000000000");
        let root_id = root_id.expect("Couldn't parse uuid.");
        let event   = FilesystemEventInfo {
            path : Path{root_id,segments:vec!["Main.txt".into()]},
            kind : FilesystemEventKind::Modified,
        };
        let expected_notification = FilesystemEvent {event};
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
            assert_eq!(n, Notification::FilesystemEvent(expected_notification));
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
        let mut fixture        = setup_file_manager();
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
    fn test_requests() {
        let root_id             = uuid::Uuid::parse_str("00000000-0000-0000-0000-000000000000");
        let root_id             = root_id.expect("Couldn't parse uuid.");
        let main                = Path{root_id,segments:vec!["Main.txt".into()]};
        let target              = Path{root_id,segments:vec!["Target.txt".into()]};
        let path_main           = json!({"path" : {
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
        let true_json = json!(true);
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
        // test_request(
        //     |client| client.file_exists(main.clone()),
        //     "file/exists",
        //     path_main.clone(),
        //     true_json,
        //     true);
        //
        // let list_response_json  = json!([          "Bar.txt",           "Foo.txt" ]);
        // let list_response_value = vec!  [Path::new("Bar.txt"),Path::new("Foo.txt")];
        // test_request(
        //     |client| client.list(main.clone()),
        //     "file/list",
        //     path_main.clone(),
        //     list_response_json,
        //     list_response_value);
        // test_request(
        //     |client| client.move_directory(main.clone(), target.clone()),
        //     "file/move",
        //     from_main_to_target.clone(),
        //     unit_json.clone(),
        //     ());
        // test_request(
        //     |client| client.move_file(main.clone(), target.clone()),
        //     "file/move",
        //     from_main_to_target.clone(),
        //     unit_json.clone(),
        //     ());
        // test_request(
        //     |client| client.read(main.clone()),
        //     "file/read",
        //     path_main.clone(),
        //     json!("Hello world!"),
        //     "Hello world!".into());
        //
        // let parse_rfc3339 = |s| {
        //     chrono::DateTime::parse_from_rfc3339(s).unwrap()
        // };
        // let expected_attributes = Attributes {
        //     creation_time      : parse_rfc3339("2020-01-07T21:25:26Z"),
        //     last_access_time   : parse_rfc3339("2020-01-21T22:16:51.123994500+00:00"),
        //     last_modified_time : parse_rfc3339("2020-01-07T21:25:26Z"),
        //     file_kind          : RegularFile,
        //     byte_size          : 125125,
        // };
        // let sample_attributes_json = json!({
        //     "creationTime"      : "2020-01-07T21:25:26Z",
        //     "lastAccessTime"    : "2020-01-21T22:16:51.123994500+00:00",
        //     "lastModifiedTime"  : "2020-01-07T21:25:26Z",
        //     "fileKind"          : "RegularFile",
        //     "byteSize"          : 125125
        // });
        // test_request(
        //     |client| client.status(main.clone()),
        //     "file/status",
        //     path_main.clone(),
        //     sample_attributes_json,
        //     expected_attributes);
        // test_request(
        //     |client| client.touch(main.clone()),
        //     "file/touch",
        //     path_main.clone(),
        //     unit_json.clone(),
        //     ());
        // test_request(
        //     |client| client.write(main.clone(), "Hello world!".into()),
        //     "file/write",
        //     json!({"path" : "./Main.txt", "contents" : "Hello world!"}),
        //     unit_json.clone(),
        //     ());
        //
        // let uuid_value = uuid::Uuid::parse_str("02723954-fbb0-4641-af53-cec0883f260a").unwrap();
        // let uuid_json  = json!("02723954-fbb0-4641-af53-cec0883f260a");
        // test_request(
        //     |client| client.create_watch(main.clone()),
        //     "file/createWatch",
        //     path_main.clone(),
        //     uuid_json.clone(),
        //     uuid_value);
        // let watch_id   = json!({
        //     "watchId" : "02723954-fbb0-4641-af53-cec0883f260a"
        // });
        // test_request(
        //     |client| client.delete_watch(uuid_value.clone()),
        //     "file/deleteWatch",
        //     watch_id.clone(),
        //     unit_json.clone(),
        //     ());
    }
}