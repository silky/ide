//! Language Server integration tests.
//!
//! They are disabled by default, as there is no CI infrastructure to run them with Lanaguage
//! Server. To run tests manually, uncomment the `#[wasm_bindgen_test::wasm_bindgen_test(async)]`
//! attributes and use wasm-bindgen test.
//!
//! Note that running Lanugage Server is expected at `SERVER_ENDPOINT` (by default localhost:30616).
//! To run the language server manually run in the `enso` repository e.g.
//! ```
//! sbt "runner/run --server --root-id 6f7d58dd-8ee8-44cf-9ab7-9f0454033641 --path $HOME/ensotmp --rpc-port 30616"
//! ```

use ide::prelude::*;

use enso_protocol::language_server::*;
use enso_protocol::types::*;
use ide::controller::Project;
use ide::model::execution_context::Visualization;
use ide::transport::web::WebSocket;
use ide::view::project::INITIAL_MODULE_NAME;
use std::time::Duration;
#[allow(unused_imports)]
use wasm_bindgen_test::wasm_bindgen_test;
use wasm_bindgen_test::wasm_bindgen_test_configure;

/// The endpoint at which the Language Server should be accepting WS connections.
const SERVER_ENDPOINT:&str = "ws://localhost:30616";

const PACKAGE_YAML:&str = r#"
maintainer: ''
license: ''
name: Test
version: ''
author: ''
"#;

const MAIN_CODE:&str = r#"
main =
    x = 6
    y = x.foo 5
    z = y + 5
    z

Number.foo = x ->
    y = this + 3
    z = y * x
    z



#### METADATA ####
[[{"index": {"value": 98}, "size": {"value": 5}}, "5fc0c11d-bd83-4ca3-b847-b8e362f7658c"],[{"index": {"value": 81}, "size": {"value": 8}}, "1cda3676-bd62-41f8-b6a1-a1e1b7c73d18"],[{"index": {"value": 42}, "size": {"value": 5}}, "899a11e5-4d2b-43dc-a867-2f2ef2d2ba62"],[{"index": {"value": 26}, "size": {"value": 7}}, "37f284d4-c593-4e65-a4be-4948fbd2adfb"],[{"index": {"value": 16}, "size": {"value": 1}}, "c553533e-a2b9-4305-9f12-b8fe7781f933"]]
[]"#;

const VISUALISATION_CODE:&str = r#"
    encode = x -> x.to_text

    incAndEncode = x -> here.encode x+1
"#;

wasm_bindgen_test_configure!(run_in_browser);

//#[wasm_bindgen_test::wasm_bindgen_test(async)]
#[allow(dead_code)]
async fn file_operations() {
    let ws        = WebSocket::new_opened(default(),SERVER_ENDPOINT).await;
    let ws        = ws.expect("Couldn't connect to WebSocket server.");
    let client    = Client::new(ws);
    let _executor = ide::ide::setup_global_executor();

    executor::global::spawn(client.runner());

    let client_id = uuid::Uuid::new_v4();
    let session   = client.init_protocol_connection(&client_id).await;
    let session   = session.expect("Couldn't initialize session.");
    let root_id   = session.content_roots[0];

    let file      = Path{root_id,segments:vec!["src".into(),"Main.enso".into()]};
    let contents  = MAIN_CODE.to_string();
    let result    = client.write_file(&file,&contents).await;
    result.expect("Couldn't write main code file.");

    let visualisation_file = Path{root_id,segments:vec!["src".into(),"Visualisation.enso".into()]};
    let contents           = VISUALISATION_CODE.to_string();
    let response           = client.write_file(&visualisation_file,&contents).await;
    response.expect("Couldn't write visualisation file.");

    let package_file = Path{root_id,segments:vec!["package.yaml".into()]};
    let contents     = PACKAGE_YAML.to_string();
    let response     = client.write_file(&package_file,&contents).await;
    response.expect("Couldn't write yaml file.");

    let execution_context    = client.create_execution_context().await;
    let execution_context    = execution_context.expect("Couldn't create execution context.");
    let execution_context_id = execution_context.context_id;

    let defined_on_type = "Main".to_string();
    let name            = "main".to_string();
    let method_pointer  = MethodPointer{file,defined_on_type,name};
    let positional_arguments_expressions = default();
    let this_argument_expression         = default();
    let explicit_call                    = ExplicitCall
        {method_pointer,positional_arguments_expressions,this_argument_expression};
    let stack_item = StackItem::ExplicitCall(explicit_call);
    let response   = client.push_to_execution_context(&execution_context_id,&stack_item).await;
    response.expect("Couldn't push execution context.");

    let response = client.pop_from_execution_context(&execution_context_id).await;
    response.expect("Couldn't pop execution context.");

    let visualisation_id     = uuid::Uuid::new_v4();
    let expression_id        = uuid::Uuid::parse_str("c553533e-a2b9-4305-9f12-b8fe7781f933");
    let expression_id        = expression_id.expect("Couldn't parse expression id.");
    let expression           = "x -> here.encode x".to_string();
    let visualisation_module = "Test.Visualisation".to_string();
    let visualisation_config = VisualisationConfiguration
    {execution_context_id,expression,visualisation_module};
    let response = client.attach_visualisation
        (&visualisation_id,&expression_id,&visualisation_config);
    response.await.expect("Couldn't attach visualisation.");

    let expression           = "x -> here.incAndEncode".to_string();
    let visualisation_module = "Test.Visualisation".to_string();
    let visualisation_config = VisualisationConfiguration
    {execution_context_id,expression,visualisation_module};
    let response = client.modify_visualisation(&visualisation_id,&visualisation_config).await;
    response.expect("Couldn't modify visualisation.");

    let response = client.detach_visualisation
        (&execution_context_id,&visualisation_id,&expression_id).await;
    response.expect("Couldn't detach visualisation.");

    let response = client.destroy_execution_context(&execution_context_id).await;
    response.expect("Couldn't destroy execution context.");

    let path      = Path{root_id, segments:vec!["foo".into()]};
    let name      = "text.txt".into();
    let object    = FileSystemObject::File {name,path};
    client.create_file(&object).await.expect("Couldn't create file.");

    let file_path = Path{root_id, segments:vec!["foo".into(),"text.txt".into()]};
    let contents  = "Hello world!".to_string();
    let result    = client.write_file(&file_path,&contents).await;
    result.expect("Couldn't write file.");

    let response = client.file_info(&file_path).await.expect("Couldn't get status.");
    assert_eq!(response.attributes.byte_size,12);
    assert_eq!(response.attributes.kind,object);

    let response = client.file_list(&Path{root_id,segments:vec!["foo".into()]}).await;
    let response = response.expect("Couldn't get file list");
    assert!(response.paths.iter().any(|file_system_object| object == *file_system_object));

    let read = client.read_file(&file_path).await.expect("Couldn't read contents.");
    assert_eq!(contents,read.contents);

    let new_path = Path{root_id,segments:vec!["foo".into(),"new_text.txt".into()]};
    client.copy_file(&file_path,&new_path).await.expect("Couldn't copy file");
    let read = client.read_file(&new_path).await.expect("Couldn't read contents.");
    assert_eq!(contents,read.contents);

    let move_path = Path{root_id,segments:vec!["foo".into(),"moved_text.txt".into()]};
    let file      = client.file_exists(&move_path).await;
    let file      = file.expect("Couldn't check if file exists.");
    if file.exists {
        client.delete_file(&move_path).await.expect("Couldn't delete file");
        let file = client.file_exists(&move_path).await;
        let file = file.expect("Couldn't check if file exists.");
        assert_eq!(file.exists,false);
    }

    client.move_file(&new_path,&move_path).await.expect("Couldn't move file");
    let read = client.read_file(&move_path).await.expect("Couldn't read contents");
    assert_eq!(contents,read.contents);

    let register_options        = RegisterOptions::Path{path:move_path.clone()};
    let method                  = "text/canEdit".to_string();
    let capability_registration = CapabilityRegistration {method,register_options};
    let response = client.open_text_file(&move_path).await;
    let response = response.expect("Couldn't open text file.");
    assert_eq!(response.content, "Hello world!");
    assert_eq!(response.write_capability, Some(capability_registration));

    let start       = Position{line:0,character:5};
    let end         = Position{line:0,character:5};
    let range       = TextRange{start,end};
    let text        = ",".to_string();
    let text_edit   = TextEdit{range,text};
    let edits       = vec![text_edit];
    let old_version = Sha3_224::new(b"Hello world!");
    let new_version = Sha3_224::new(b"Hello, world!");
    let path        = move_path.clone();
    let edit        = FileEdit {path,edits,old_version,new_version:new_version.clone()};
    client.apply_text_file_edit(&edit).await.expect("Couldn't apply edit.");

    let future = client.save_text_file(&move_path,&new_version).await;
    future.expect("Couldn't save file.");

    client.close_text_file(&move_path).await.expect("Couldn't close text file.");

    let read = client.read_file(&move_path).await.expect("Couldn't read contents.");
    assert_eq!("Hello, world!".to_string(),read.contents);
}

//#[wasm_bindgen_test::wasm_bindgen_test(async)]
#[allow(dead_code)]
async fn file_events() {
    let ws         = WebSocket::new_opened(default(),SERVER_ENDPOINT).await;
    let ws         = ws.expect("Couldn't connect to WebSocket server.");
    let client     = Client::new(ws);
    let mut stream = client.events();
    let _executor  = ide::ide::setup_global_executor();

    executor::global::spawn(client.runner());

    let client_id = uuid::Uuid::default();
    let session   = client.init_protocol_connection(&client_id).await;
    let session   = session.expect("Couldn't initialize session.");
    let root_id   = session.content_roots[0];

    let path      = Path{root_id,segments:vec!["test.txt".into()]};
    let file      = client.file_exists(&path).await;
    let file      = file.expect("Couldn't check if file exists.");
    if file.exists {
        client.delete_file(&path).await.expect("Couldn't delete file");
        let file = client.file_exists(&path).await;
        let file = file.expect("Couldn't check if file exists.");
        assert_eq!(file.exists,false);
    }

    let path       = Path{root_id, segments:vec![]};
    let options    = RegisterOptions::Path{path};
    let capability = client.acquire_capability(&"receivesTreeUpdates".to_string(),&options).await;
    capability.expect("Couldn't acquire receivesTreeUpdates capability.");

    let path      = Path{root_id, segments:vec![]};
    let name      = "test.txt".into();
    let object    = FileSystemObject::File {name,path:path.clone()};
    client.create_file(&object).await.expect("Couldn't create file.");

    let path         = Path{root_id,segments:vec!["test.txt".into()]};
    let kind         = FileEventKind::Added;
    let event        = FileEvent {path,kind};
    let notification = Notification::FileEvent {event};

    let event = stream.next().await.expect("Couldn't get any notification.");
    if let Event::Notification(incoming_notification) = event {
        assert_eq!(incoming_notification,notification);
    } else {
        panic!("Incoming event isn't a notification.");
    }
}

/// This procedure sets up the project, testing:
/// * using project picker to open (or create) a project
/// * establishing a binary protocol connection with Language Server
async fn setup_project() -> Project {
    ensogl_system_web::set_stdout();
    let logger = Logger::new("Test");
    info!(logger,"Setting up the project.");
    let endpoint         = ide::constants::PROJECT_MANAGER_ENDPOINT;
    let ws               = WebSocket::new_opened(logger.clone_ref(),endpoint).await.unwrap();
    let pm               = ide::IdeInitializer::setup_project_manager(ws);
    let name             = ide::constants::DEFAULT_PROJECT_NAME;
    let project_metadata = ide::IdeInitializer::get_most_recent_project_or_create_new
        (&logger,&pm,name).await.expect("Couldn't get most recent or create new project.");
    let error_msg = "Couldn't open project";
    ide::IdeInitializer::open_project(&logger,&pm,&project_metadata).await.expect(error_msg)
}

//#[wasm_bindgen_test::wasm_bindgen_test(async)]
#[allow(dead_code)]
/// This integration test covers writing and reading a file using the binary protocol
async fn binary_protocol_test() {
    let _guard   = ide::ide::setup_global_executor();
    let project  = setup_project().await;
    println!("Got project: {:?}",project);
    let path     = Path::new(project.language_server_rpc.content_root(), &["test_file.txt"]);
    let contents = "Hello!".as_bytes();
    let written  = project.language_server_bin.write_file(&path,contents).await.unwrap();
    println!("Written: {:?}", written);
    let read_back = project.language_server_bin.read_file(&path).await.unwrap();
    println!("Read back: {:?}", read_back);
    assert_eq!(contents, read_back.as_slice());
}

/// The future that tests attaching visualization and routing its updates.
async fn binary_visualization_updates_test_hlp() {
    let project  = setup_project().await;
    println!("Got project: {:?}", project);

    let expression = "x -> x.json_serialize";

    use ensogl::system::web::sleep;
    use ide::view::project::MAIN_DEFINITION_NAME;
    use double_representation::definition::Id as DefinitionId;

    let module_path = project.module_path_from_qualified_name(&[INITIAL_MODULE_NAME]).unwrap();
    let module_qualified_name = project.qualified_module_name(&module_path);
    let module                = project.module_controller(module_path).await.unwrap();
    println!("Got module: {:?}", module);
    let function_id           = DefinitionId::new_plain_name(MAIN_DEFINITION_NAME);
    let graph_executed        = module.executed_graph_controller_unchecked(
        function_id,&project).await.unwrap();

    let the_node = graph_executed.nodes().unwrap()[0].info.clone();
    graph_executed.set_expression(the_node.id(), "10+20").unwrap();

    // We must yield control for a moment, so the text edit is applied.
    sleep(Duration::from_millis(1)).await;

    println!("Main graph: {:?}", graph_executed);
    println!("The code is: {:?}", module.code());
    println!("Main node: {:?} with {}", the_node, the_node.expression().repr());

    let visualization = Visualization::new(the_node.id(),expression,module_qualified_name);
    let     stream    = graph_executed.attach_visualization(visualization.clone()).await.unwrap();
    println!("Attached the visualization {}", visualization.id);
    let mut stream    = stream.boxed_local();
    let first_event = stream.next().await.unwrap(); // await update
    assert_eq!(first_event.as_ref(), "30".as_bytes());
}

//#[wasm_bindgen_test]
#[allow(dead_code)]
/// This integration test covers attaching visualizations and receiving their updates.
fn binary_visualization_updates_test() {
    let executor = ide::executor::web::EventLoopExecutor::new_running();
    ide::executor::global::set_spawner(executor.spawner.clone());
    executor.spawn_local(binary_visualization_updates_test_hlp()).unwrap();
    std::mem::forget(executor);
}
