//! Code for retrieving graph description from AST.

use crate::prelude::*;

use crate::double_representation::definition;
use crate::double_representation::definition::DefinitionInfo;
use crate::double_representation::definition::DefinitionName;
use crate::double_representation::definition::DefinitionProvider;
use crate::double_representation::node::NodeInfo;

use ast::Ast;
use ast::Block;
use ast::BlockLine;
use ast::BlockType;
use ast::known;
use utils::fail::FallibleResult;
use utils::vec::pop_front;



// =============
// === Error ===
// =============

#[derive(Fail,Debug)]
#[fail(display="ID was not found.")]
struct IdNotFound {id:ast::ID}

#[derive(Fail,Debug)]
#[fail(display="Cannot set Block lines because no line with Some(Ast) was found. Block must have \
at least one non-empty line.")]
struct MissingLineWithAst;



// ====================
// === LocationHint ===
// ====================

/// Describes the desired position of the node's line in the graph's code block.
#[derive(Clone,Copy,Debug)]
pub enum LocationHint {
    /// Try placing this node's line before the line described by id.
    Before(ast::ID),
    /// Try placing this node's line after the line described by id.
    After(ast::ID),
    /// Try placing this node's line at the start of the graph's code block.
    Start,
    /// Try placing this node's line at the end of the graph's code block.
    End,
}



// ================
// === Graph Id ===
// ================

/// Crumb describes step that needs to be done when going from context (for graph being a module)
/// to the target.
// TODO [mwu]
//  Currently we support only entering named definitions.
pub type Crumb = DefinitionName;

/// Identifies graph in the module.
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
pub struct Id {
    /// Sequence of traverses from module root up to the identified graph.
    pub crumbs : Vec<Crumb>,
}

impl Id {
    /// Creates a new graph identifier consisting of a single crumb.
    pub fn new_single_crumb(crumb:DefinitionName) -> Id {
        let crumbs = vec![crumb];
        Id {crumbs}
    }
}


// ===============================
// === Finding Graph In Module ===
// ===============================

#[derive(Fail,Clone,Debug)]
#[fail(display="Definition ID was empty")]
struct CannotFindDefinition(Id);

#[derive(Fail,Clone,Debug)]
#[fail(display="Definition ID was empty")]
struct EmptyDefinitionId;

/// Looks up graph in the module.
pub fn traverse_for_definition
(ast:ast::known::Module, id:&Id) -> FallibleResult<DefinitionInfo> {
    let err            = || CannotFindDefinition(id.clone());
    let mut crumb_iter = id.crumbs.iter();
    let first_crumb    = crumb_iter.next().ok_or(EmptyDefinitionId)?;
    let mut definition = ast.find_definition(first_crumb).ok_or_else(err)?;
    for crumb in crumb_iter {
        definition = definition.find_definition(crumb).ok_or_else(err)?;
    }
    Ok(definition)
}



// ==========================
// === Block constructors ===
// ==========================

/// Gets all block lines.
pub fn get_all_block_lines<T:Clone>(block:&Block<T>) -> Vec<BlockLine<Option<T>>> {
    let mut lines = Vec::new();
    for off in &block.empty_lines {
        let elem = None;
        let off  = *off;
        lines.push(BlockLine{elem,off})
    }

    let first_line = block.first_line.clone();
    let elem       = Some(first_line.elem);
    let off        = first_line.off;
    lines.push(BlockLine{elem,off});

    for line in &block.lines {
        lines.push(line.clone())
    }
    lines
}

/// Creates a new block with the provided lines.
pub fn new_block_with_lines<T>(mut lines:Vec<BlockLine<Option<T>>>) -> FallibleResult<Block<T>> {
    let mut empty_lines = Vec::new();
    let mut line        = pop_front(&mut lines).ok_or(MissingLineWithAst)?;
    while let None = line.elem {
        empty_lines.push(line.off);
        line = pop_front(&mut lines).ok_or(MissingLineWithAst)?;
    }
    let elem       = line.elem.ok_or(MissingLineWithAst)?;
    let off        = line.off;
    let first_line = BlockLine {elem,off};
    let indent     = crate::double_representation::INDENT;
    let is_orphan  = false;
    let ty         = BlockType::Discontinuous {};
    Ok(Block {empty_lines,first_line,lines,indent,is_orphan,ty})
}




// =================
// === GraphInfo ===
// =================

/// Description of the graph, based on information available in AST.
#[derive(Clone,Debug)]
pub struct GraphInfo {
    source:DefinitionInfo,
}

impl GraphInfo {
    /// Describe graph of the given definition.
    pub fn from_definition(source:DefinitionInfo) -> GraphInfo {
        GraphInfo {source}
    }

    /// Lists nodes in the given binding's ast (infix expression).
    fn from_function_binding(ast:known::Infix) -> Vec<NodeInfo> {
        let body = ast.rarg.clone();
        if let Ok(body_block) = known::Block::try_new(body.clone()) {
            block_nodes(&body_block)
        } else {
            expression_node(body)
        }
    }

    /// Gets all known nodes in this graph (does not include special pseudo-nodes like graph
    /// inputs and outputs).
    pub fn nodes(&self) -> Vec<NodeInfo> {
        Self::from_function_binding(self.source.ast.clone())
    }

    fn is_node_by_id(line:&BlockLine<Option<Ast>>, id:ast::ID) -> bool {
        let node_info  = line.elem.as_ref().and_then(NodeInfo::from_line_ast);
        let id_matches = node_info.map(|node| node.id() == id);
        id_matches.unwrap_or(false)
    }

    /// Searches for `NodeInfo` with the associated `id` index in `lines`. Returns an error if
    /// the Id is not found.
    pub fn find_node_index_in_lines
    (lines:&[BlockLine<Option<Ast>>], id:ast::ID) -> FallibleResult<usize> {
        let position = lines.iter().position(|line| Self::is_node_by_id(&line,id));
        position.ok_or(IdNotFound{id}.into())
    }

    fn get_all_lines(infix:&known::Infix) -> FallibleResult<Vec<BlockLine<Option<Ast>>>> {
        if let Ok(block) = known::Block::try_from(infix.rarg.clone()) {
            Ok(get_all_block_lines(&block))
        } else {
            let elem = Some(infix.rarg.clone());
            let off  = 0;
            Ok(vec![BlockLine{elem,off}])
        }
    }

    fn visit_and_maybe_modify
    ( infix         : &known::Infix
    , block_line    : &BlockLine<Option<Ast>>
    , location_hint : &LocationHint) -> FallibleResult<known::Infix> {
        let mut lines = Self::get_all_lines(&infix)?;

        let index = match location_hint {
            LocationHint::Start      => Ok(0),
            LocationHint::End        => Ok(lines.len()),
            LocationHint::Before(id) => Self::find_node_index_in_lines(&lines,*id),
            LocationHint::After(id)  => {
                Self::find_node_index_in_lines(&lines,*id).map(|index| index + 1)
            }
        };

        if let Ok(index) = index {
            let block_line = block_line.clone();
            lines.insert(index, block_line);

            let block = new_block_with_lines(lines)?;
            let rarg = Ast::new(block, None);
            let infix = infix.deref().clone();
            Ok(known::KnownAst::new(ast::Infix { rarg, ..infix }, None))
        } else {
            let lines = lines.iter().map(|line| {
                if let Some(elem) = &line.elem {
                    if let Ok(infix) = known::Infix::try_from(elem) {
                        let ast = Self::visit_and_maybe_modify(&infix,&block_line,&location_hint);
                        let infix = ast.unwrap_or(infix);

                        let elem = Some(infix.clone().into());
                        let off  = line.off;
                        BlockLine{elem,off}
                    } else {
                        line.clone()
                    }
                } else {
                    line.clone()
                }
            }).collect();

            let block = new_block_with_lines(lines)?;
            let rarg = Ast::new(block, None);
            let infix = infix.deref().clone();
            Ok(known::KnownAst::new(ast::Infix { rarg, ..infix }, None))
            //Err(index.unwrap_err())
        }
    }

    /// Adds a new node to this graph.
    pub fn add_node
    (&mut self, line_ast:Ast, location_hint:LocationHint) -> FallibleResult<()> {
        let elem = Some(line_ast);
        let off  = 0;
        let block_line = BlockLine{elem,off};

        let ast = Self::visit_and_maybe_modify(&self.source.ast,&block_line,&location_hint);
        ast.map(|ast| self.source.ast = ast)?;
        Ok(())
    }

    /// Removes the node from graph.
    pub fn remove_node(&mut self, _node_id:ast::ID) -> FallibleResult<()> {
        todo!()
    }

    /// Sets expression of the given node.
    pub fn edit_node(&self, _node_id:ast::ID, _new_expression:impl Str) -> FallibleResult<()> {
        todo!()
    }
}



// =====================
// === Listing nodes ===
// =====================

/// Collects information about nodes in given code `Block`.
pub fn block_nodes(ast:&known::Block) -> Vec<NodeInfo> {
    ast.iter().flat_map(|line_ast| {
        // If this can be a definition, then don't treat it as a node.
        match definition::DefinitionInfo::from_line_ast(line_ast, definition::ScopeKind::NonRoot) {
            None    => NodeInfo::from_line_ast(line_ast),
            Some(_) => None
        }
    }).collect()
}

/// Collects information about nodes in given trivial definition body.
pub fn expression_node(ast:Ast) -> Vec<NodeInfo> {
    NodeInfo::new_expression(ast).into_iter().collect()
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::double_representation::definition::DefinitionName;
    use crate::double_representation::definition::DefinitionProvider;

    use ast::HasRepr;
    use parser::api::IsParser;
    use wasm_bindgen_test::wasm_bindgen_test;
    use ast::test_utils::expect_single_line;

    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    /// Takes a program with main definition in root and returns main's graph.
    fn main_graph(parser:&mut impl IsParser, program:impl Str) -> GraphInfo {
        let module = parser.parse_module(program.into(), default()).unwrap();
        let name   = DefinitionName::new_plain("main");
        let main   = module.find_definition(&name).unwrap();
        GraphInfo::from_definition(main)
    }

    #[wasm_bindgen_test]
    fn detect_a_node() {
        let mut parser = parser::Parser::new_or_panic();
        // Each of these programs should have a `main` definition with a single `2+2` node.
        let programs = vec![
            "main = 2+2",
            "main = \n    2+2",
            "main = \n    foo = 2+2",
            "main = \n    foo = 2+2\n    bar b = 2+2", // `bar` is a definition, not a node
        ];
        for program in programs {
            let graph = main_graph(&mut parser, program);
            let nodes = graph.nodes();
            assert_eq!(nodes.len(), 1);
            let node = &nodes[0];
            assert_eq!(node.expression().repr(), "2+2");
            let _ = node.id(); // just to make sure it is available
        }
    }

    fn create_node_ast(parser:&mut impl IsParser, expression:&str) -> (Ast,ast::ID) {
        let node_ast = parser.parse(expression.to_string(), default()).unwrap();
        let line_ast = expect_single_line(&node_ast).clone();
        let id       = line_ast.id.expect("line_ast should have an ID");
        (line_ast,id)
    }

    #[wasm_bindgen_test]
    fn add_node_to_graph_with_single_line() {
        let program = "main = print \"hello\"";
        let mut parser = parser::Parser::new_or_panic();
        let mut graph = main_graph(&mut parser, program);

        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].expression().repr(), "print \"hello\"");

        let expr0 = "a + 2";
        let expr1 = "b + 3";
        let (line_ast0,id0) = create_node_ast(&mut parser, expr0);
        let (line_ast1,id1) = create_node_ast(&mut parser, expr1);

        assert!(graph.add_node(line_ast0, LocationHint::Start).is_ok());
        assert_eq!(graph.nodes().len(), 2);
        assert!(graph.add_node(line_ast1, LocationHint::Before(graph.nodes()[0].id())).is_ok());

        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 3);
        assert_eq!(nodes[0].expression().repr(), expr1);
        assert_eq!(nodes[0].id(), id1);
        assert_eq!(nodes[1].expression().repr(), expr0);
        assert_eq!(nodes[1].id(), id0);
        assert_eq!(nodes[2].expression().repr(), "print \"hello\"");
    }

    #[wasm_bindgen_test]
    fn add_node_to_graph_with_multiple_lines() {
        // TODO [dg] Also add test for binding node when it's possible to update its id.
        let program = r#"
main =

    foo = node

    add a b =
        a + b

    print "hello"

"#;
        let mut parser = parser::Parser::new_or_panic();
        let mut graph = main_graph(&mut parser, program);

        let (line_ast0,id0) = create_node_ast(&mut parser, "4 + 4");
        let (line_ast1,id1) = create_node_ast(&mut parser, "a + b");
        let (line_ast2,id2) = create_node_ast(&mut parser, "x * x");
        let (line_ast3,id3) = create_node_ast(&mut parser, "x / x");
        let (line_ast4,id4) = create_node_ast(&mut parser, "a * 2");

        ensogl::system::web::set_stdout();

        let definition  = graph.source.list_definitions()[0].clone();
        let child_graph = GraphInfo::from_definition(definition);
        let child_nodes = child_graph.nodes();
        assert_eq!(child_nodes.len(), 1);

        assert_eq!(graph.nodes().len(), 2);

        assert!(graph.add_node(line_ast4, LocationHint::Before(child_nodes[0].id())).is_ok());
        assert!(graph.add_node(line_ast0, LocationHint::Start).is_ok());
        assert!(graph.add_node(line_ast1, LocationHint::Before(graph.nodes()[0].id())).is_ok());
        assert!(graph.add_node(line_ast2, LocationHint::After(graph.nodes()[1].id())).is_ok());
        assert!(graph.add_node(line_ast3, LocationHint::End).is_ok());

        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 6);
        assert_eq!(nodes[0].expression().repr(), "a + b");
        assert_eq!(nodes[0].id(), id1);
        assert_eq!(nodes[1].expression().repr(), "4 + 4");
        assert_eq!(nodes[1].id(), id0);
        assert_eq!(nodes[2].expression().repr(), "x * x");
        assert_eq!(nodes[2].id(), id2);
        assert_eq!(nodes[3].expression().repr(), "node");
        assert_eq!(nodes[4].expression().repr(), "print \"hello\"");
        assert_eq!(nodes[5].expression().repr(), "x / x");
        assert_eq!(nodes[5].id(), id3);

        let add_def   = graph.source.list_definitions()[0].clone();
        let add_graph = GraphInfo::from_definition(add_def);
        let nodes     = add_graph.nodes();
        assert_eq!(nodes.len(), 2);
        assert_eq!(nodes[0].expression().repr(), "a * 2");
        assert_eq!(nodes[0].id(), id4);
        assert_eq!(nodes[1].expression().repr(), "a + b");
    }

    #[wasm_bindgen_test]
    fn multiple_node_graph() {
        let mut parser = parser::Parser::new_or_panic();
        let program = r"
main =
    foo = node
    foo a = not_node
    Int.= a = node
    node
";
        let graph = main_graph(&mut parser, program);
        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 2);
        for node in nodes.iter() {
            assert_eq!(node.expression().repr(), "node");
        }
    }
}
