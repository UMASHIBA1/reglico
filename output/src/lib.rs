use wasm_bindgen::prelude::*;use web_sys::console;#[cfg(feature = "wee_alloc")]#[global_allocator]
    static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    #[wasm_bindgen(start)]
    pub fn main_js() -> Result<(), JsValue> {
        #[cfg(debug_assertions)]
        console_error_panic_hook::set_once();
    
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}
macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

fn console_log(value: f32) {
    console_log!("{}", value);
}
fn fib(n:f32)->f32{if n<=1.0 {n}fib(n-1.0)+fib(n-2.0)}console_log(fib(40.0));
    Ok(())
    }