#!/usr/bin/env rust-script

//! ```cargo
//! [dependencies]
//! rodio = "0.19.0"
//! ```

use rodio::{
    source::{SineWave, Source},
    OutputStream, Sink,
};
use std::time::Duration;

fn main() {
    let (_stream, stream_handle) = OutputStream::try_default().unwrap();

    let sink = Sink::try_new(&stream_handle).unwrap();

    let source = SineWave::new(440.0)
        .take_duration(Duration::from_secs_f32(0.2))
        .amplify(0.10);

    loop {
        println!("boop");

        sink.append(source.clone());

        sink.sleep_until_end();

        std::thread::sleep(Duration::from_millis(750));
    }
}
