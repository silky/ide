#![allow(missing_docs)]

use crate::prelude::*;

use crate::display::world::WorldData;
use crate::display::object::DisplayObjectOps;
use crate::display::shape::text::glyph::font::FontRegistry;
use crate::display::shape::text::text_field::TextField;
use crate::display::shape::text::text_field::TextFieldProperties;
use crate::display::world::*;
use crate::system::web;
use crate::system::web::forward_panic_hook_to_console;


use basegl_system_web::set_stdout;
use nalgebra::Vector2;
use nalgebra::Vector4;
use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsCast;
use web_sys::MouseEvent;
use failure::_core::cell::RefCell;
use wasm_bindgen::prelude::*;
use crate::system::web::text_input::KeyboardBinding;


const TEXT:&str =
"To be, or not to be, that is the question:
Whether 'tis nobler in the mind to suffer
The slings and arrows of outrageous fortune,
Or to take arms against a sea of troubles
And by opposing end them. To die—to sleep,
No more; and by a sleep to say we end
The heart-ache and the thousand natural shocks
That flesh is heir to: 'tis a consummation
Devoutly to be wish'd.";


#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_text_selecting() {
    forward_panic_hook_to_console();
    set_stdout();
    basegl_core_msdf_sys::run_once_initialized(|| {
        let world     = &WorldData::new("canvas");
        let scene     = world.scene();
        let camera    = scene.camera();
        let screen    = camera.screen();
        let mut fonts = FontRegistry::new();
        let font_id   = fonts.load_embedded_font("DejaVuSansMono").unwrap();

        let properties = TextFieldProperties {
            font_id,
            text_size  : 16.0,
            base_color : Vector4::new(0.0, 0.0, 0.0, 1.0),
            size       : Vector2::new(200.0, 200.0)
        };

        let mut text_field = TextField::new(&world,TEXT,properties,&mut fonts);
        text_field.set_position(Vector3::new(10.0, 600.0, 0.0));
        text_field.jump_cursor(Vector2::new(50.0, -40.0),false,&mut fonts);
        world.add_child(&text_field);
        text_field.update();

        let text_field_rc       = Rc::new(RefCell::new(text_field));
        let text_field_on_click = text_field_rc.clone_rc();
        let text_field_on_copy  = text_field_rc.clone_rc();
        let text_field_on_paste = text_field_rc.clone_rc();

        let fonts_rc       = Rc::new(RefCell::new(fonts));
        let fonts_on_click = fonts_rc.clone_rc();
        let fonts_on_paste = fonts_rc.clone_rc();

        let c: Closure<dyn FnMut(JsValue)> = Closure::wrap(Box::new(move |val:JsValue| {
            let mut fonts      = fonts_on_click.borrow_mut();
            let mut text_field = text_field_on_click.borrow_mut();
            let val = val.unchecked_into::<MouseEvent>();
            let x = val.x() as f32 - 10.0;
            let y = (screen.height - val.y() as f32) - 600.0;
            text_field.jump_cursor(Vector2::new(x,y),true,&mut fonts);
        }));
        web::document().unwrap().add_event_listener_with_callback
        ("click",c.as_ref().unchecked_ref()).unwrap();
        c.forget();

        let keyboard = KeyboardBinding::new(move || {
            let text_field = text_field_on_copy.borrow();
            text_field.get_selected_text()
        }, move |pasted| {
            let mut fonts      = fonts_on_paste.borrow_mut();
            let mut text_field = text_field_on_paste.borrow_mut();
            text_field.edit(pasted.as_str(),&mut fonts);
        }, |_| {});

        world.on_frame(move |_| { let _keep_alive = &keyboard; }).forget();
    });
}
