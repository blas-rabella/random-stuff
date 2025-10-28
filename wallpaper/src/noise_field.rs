use colorgrad::Color;

use crate::{cli::Args, img::get_color_grad, noise_functions::get_noise_function, utils::remap};

pub fn base_noise(args: &Args) -> Vec<Color> {
    let grad = get_color_grad(&args.color).sharp(args.divisions as usize, 0.01);
    let ns = get_noise_function(&args.noise_function);
    let scale = args.scale;

    let mut img: Vec<Color> = vec![Color::default(); args.height * args.width];
    for y in 0..args.height {
        for x in 0..args.width {
            let raw_noise = ns.get([x as f64 * scale, y as f64 * scale]);
            let normalized = remap(raw_noise, -0.5, 0.5, 0.0, 1.0);
            img[x + y * args.width] = grad.at(normalized)
        }
    }
    img
}
