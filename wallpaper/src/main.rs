mod cli;
mod img;
mod noise_field;
mod noise_functions;
mod utils;
use clap::Parser;
use cli::Args;
use img::build_image;
use noise_field::base_noise;

fn main() {
    let args = Args::parse();
    let raw = base_noise(&args);
    let imgbuf = build_image(raw, args.width as u32, args.height as u32);
    let default_path = format!("{}/noise.png", std::env::var("HOME").unwrap());
    imgbuf.save(args.output.unwrap_or(default_path)).unwrap();
}
