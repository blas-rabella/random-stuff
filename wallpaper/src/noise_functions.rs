use noise::NoiseFn;

use crate::cli::NoiseFunction;

pub fn get_noise_function(noise_function: &NoiseFunction) -> Box<dyn NoiseFn<f64, 2>> {
    match noise_function {
        NoiseFunction::Perlin => Box::new(noise::Perlin::new(rand::random())),
        NoiseFunction::OpenSimplex => Box::new(noise::OpenSimplex::new(rand::random())),
    }
}
