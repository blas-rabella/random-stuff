use noise::NoiseFn;
// #2e3440 #3b4252 #434c5e #4c566a #d8dee9 #e5e9f0 #eceff4
fn main() {
    let scale = 0.002;
    // let grad = colorgrad::CustomGradient::new()
    //     .html_colors(&[
    //         "#2e3440", " #3b4252", "#434c5e", "#4c566a", "#d8dee9", "#e5e9f0", "#eceff4",
    //     ])
    //     .build()
    //     .unwrap()
    //     .sharp(7, 0.0);
    let grad = colorgrad::viridis().sharp(16, 0.01);
    let ns = noise::OpenSimplex::new(rand::random());
    let mut imgbuf = image::ImageBuffer::new(4048, 2048);

    for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
        let t = ns.get([x as f64 * scale, y as f64 * scale]);
        let rgba = grad.at(remap(t, -0.5, 0.5, 0.0, 1.0)).to_rgba8();
        *pixel = image::Rgba(rgba);
    }

    imgbuf.save("/home/b.rabella/noise.png").unwrap();
}

// Map t which is in range [a, b] to range [c, d]
fn remap(t: f64, a: f64, b: f64, c: f64, d: f64) -> f64 {
    (t - a) * ((d - c) / (b - a)) + c
}
