use colorgrad::{Color, Gradient};
use image::{ImageBuffer, Rgba};

use crate::cli::ColorScheme;

pub fn build_image(data: Vec<Color>, width: u32, height: u32) -> ImageBuffer<Rgba<u8>, Vec<u8>> {
    let img_buf = image::ImageBuffer::from_fn(width, height, |x, y| {
        image::Rgba(data[(x + y * width) as usize].to_rgba8())
    });
    img_buf
}

pub fn get_color_grad(color_scheme: &ColorScheme) -> Gradient {
    match color_scheme {
        ColorScheme::Viridis => colorgrad::viridis(),
        ColorScheme::Magma => colorgrad::magma(),
        ColorScheme::Inferno => colorgrad::inferno(),
        ColorScheme::Rainbow => colorgrad::rainbow(),
        ColorScheme::Monochrome => colorgrad::greys(),
    }
}
