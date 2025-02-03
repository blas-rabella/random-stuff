use noise::NoiseFn;
use wallpaper_rs::{Desktop, DesktopEnvt};
// #2e3440 #3b4252 #434c5e #4c566a #d8dee9 #e5e9f0 #eceff4
//
fn main() {
    let scale = 0.00095;
    // let grad = colorgrad::CustomGradient::new()
    //     .html_colors(&[
    //         "#181825", "#1e1e2e", "#b4befe", "#89b4fa", "#74c7ec", "#89dceb",
    //         "#94e2d5", // "#2e3440", " #3b4252", "#434c5e", "#4c566a", "#d8dee9", "#e5e9f0", "#eceff4",
    //     ])
    //     .build()
    //     .unwrap()
    //     .sharp(7, 0.01);
    let grad = colorgrad::viridis().sharp(14, 0.01);
    let ns = noise::OpenSimplex::new(rand::random());
    let mut imgbuf = image::ImageBuffer::new(4048, 2048);

    for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
        let t = ns.get([x as f64 * scale, y as f64 * scale]);
        let rgba = grad.at(remap(t, -0.5, 0.5, 0.0, 1.0)).to_rgba8();
        *pixel = image::Rgba(rgba);
    }

    imgbuf.save("/Users/b.rabella/noise.png").unwrap();
    let desktop = DesktopEnvt::new().unwrap();
    desktop.set_wallpaper("/Users/b.rabella/noise.png").unwrap();
    println!("{:?}", desktop.get_wallpaper());
}

// Map t which is in range [a, b] to range [c, d]
fn remap(t: f64, a: f64, b: f64, c: f64, d: f64) -> f64 {
    (t - a) * ((d - c) / (b - a)) + c
}
