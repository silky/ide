//! This module contains functionality that allows the usage of JavaScript to define visualizations.

use crate::prelude::*;

use crate::component::visualization:: VisualisationFrp;
use crate::component::visualization::Data;
use crate::component::visualization::DataError;
use crate::component::visualization::DataRenderer;
use crate::component::visualization::DataType;

use ensogl::display::DomScene;
use ensogl::display::DomSymbol;
use ensogl::display;
use ensogl::system::web::JsValue;
use ensogl::system::web;
use js_sys;
use nalgebra::Vector2;

/// Example of the visualisation JS wrapper API usage
// TODO remove once we have proper visualizations.
pub fn make_sample_js_bubble_chart() -> JsVisualizationGeneric {
    // TODO just some basic JS. Should probably be a nice d3 example instead.
    let fn_set_data = r#"{
        var xmlns = "http://www.w3.org/2000/svg";

        const element = document.getElementById("vis-svg");
        if (element != null) {
            element.parentNode.removeChild(element);
        }

        const parent = document.getElementById("vis");
        var svgElem = document.createElementNS(xmlns, "svg");
        svgElem.setAttributeNS(null, "id", "vis-svg");
        svgElem.setAttributeNS(null, "viewBox", "0 0 " + 100 + " " + 100);
        svgElem.setAttributeNS(null, "width", 100);
        svgElem.setAttributeNS(null, "height", 100);
        parent.appendChild(svgElem);

        data.forEach(data => {
            const bubble = document.createElementNS(xmlns,"circle");
            bubble.setAttributeNS(null,"stroke","black");
            bubble.setAttributeNS(null,"fill","red");
            bubble.setAttributeNS(null,"r", data[2]);
            bubble.setAttributeNS(null,"cx",data[0]);
            bubble.setAttributeNS(null,"cy",data[1]);
            svgElem.appendChild(bubble);
        });

    }
    "#;

    let fn_set_size = r#"{
        const svg = document.getElementById("vis-svg");
        if (svg == null) {
            return;
        }
        const width  = arguments[0];
        const height = arguments[1];
        svgElem.setAttributeNS(null, "viewBox", "0 0 " + width + " " + height);
        svgElem.setAttributeNS(null, "width",   width);
        svgElem.setAttributeNS(null, "height",  height);
    }"#;

    JsVisualizationGeneric::new(fn_set_data,fn_set_size).unwrap()
}

/// Error types that indicate some problem on the JavaScript side of the functionality.
/// TODO[mm] use!
#[derive(Copy,Clone,Debug)]
pub enum Error {}

/// `JsVisualizationGeneric` allows the use of arbitrary javascript to create visualisations.
///
/// TODO describe API
/// TODO add hooks for JS into the scene tree (i.e., define where to hook into the scene)
/// TODO add hooks for status messages form the JS side,
/// TODO add full callback functionality
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub struct JsVisualizationGeneric {
    set_data    : js_sys::Function,
    set_size    : js_sys::Function,
    /// Root node of this visualisation.
    pub content : Rc<DomSymbol>,
}

impl JsVisualizationGeneric {
    /// Constructor.
    ///
    /// `fn_set_data` and `fn_set_data` need to be strings that contain valid JavaScript code. This
    /// code will be executed as the function body of the respective functions. Functions arguments
    /// are available in the body through the `arguments` variable, which is an array of arguments
    /// passed to the function.
    ///
    /// TODO define exact arguments and API for all functions.
    pub fn new(fn_set_data:&str, fn_set_size:&str) -> Result<Self,Error> {
        let set_data = js_sys::Function::new_with_args(&"data", fn_set_data);
        let set_size = js_sys::Function::new_no_args(fn_set_size);

        let div = web::create_div();
        let symbol = DomSymbol::new(&div);
        symbol.dom().set_attribute("id","vis").unwrap();
        symbol.dom().style().set_property("overflow","hidden").unwrap();

        let content = Rc::new(symbol);

        Ok(JsVisualizationGeneric { set_data,set_size,content })
    }

    /// Hooks the root node int the given scene.
    ///
    /// MUST be called to make this visualisation visible.
    /// TODO[mm] find a better mechanism to ensure this. Probably through the registry later on.
    pub fn set_dom_layer(&self, scene:&DomScene) {
        scene.manage(&self.content);
    }
}

impl DataRenderer for JsVisualizationGeneric {
    fn init(&self, _frp:&VisualisationFrp) {
    }

    fn valid_input_types(&self) -> Vec<DataType> {
        unimplemented!()
    }

    fn set_data(&self, data: Data) -> Result<Data,DataError> {
        // TODO proper error handling.
        let context = JsValue::NULL;
        // FIXME[mm] this is NOT how this is supposed to work.
        let data_internal: Rc<Vec<Vector3<f32>>> = data.as_binary()?;
        let data_json = JsValue::from_serde(&data_internal).unwrap();
        self.set_data.call1(&context,&data_json).unwrap();
        Ok(data)
    }

    fn set_size(&self, size: Vector2<f32>) {
        // TODO proper error handling.
        let context   = JsValue::NULL;
        let data_json = JsValue::from_serde(&size).unwrap();
        self.set_size.call1(&context, &data_json).unwrap();
    }
}

impl display::Object for JsVisualizationGeneric {
    fn display_object(&self) -> &display::object::Instance {
        &self.content.display_object()
    }
}
