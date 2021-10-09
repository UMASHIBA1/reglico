use wasm_bindgen::prelude::*;use web_sys::console;#[cfg(feature = "wee_alloc")]#[global_allocator]
    static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    #[wasm_bindgen(start)]
    pub fn main_js() -> Result<(), JsValue> {
        #[cfg(debug_assertions)]
        console_error_panic_hook::set_once();
    fn add(a:f32,b:f32)->f32{a+b}let total=add(1.0,2.0);
    Ok(())
    }