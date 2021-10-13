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

fn console_log(value: i64) {
    console_log!("{}", value);
}

fn performance_now() -> i64 {
    let window = web_sys::window().expect("should have a window in this context");
    let performance = window
        .performance()
        .expect("performance should be available");

    let now_i64 = performance.now() as i64;
    now_i64
}
fn fib(n:i64)->i64{if n<=1 {return n;}return fib(n-1)+fib(n-2);}console_log(performance_now());console_log(fib(40));console_log(performance_now());
    Ok(())
    }