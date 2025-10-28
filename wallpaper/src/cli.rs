use clap::{Parser, ValueEnum};

// A noise based wallpaper generator
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    //Scaling factor for the noise function
    #[arg(short, long)]
    pub scale: f64,
    //Number of levels to discretize the noise funciton
    #[arg(short, long)]
    pub divisions: u8,
    //Width of the output image
    #[arg(long, default_value_t = 4096)]
    pub width: usize,
    //Height of the output image
    #[arg(long, default_value_t = 2048)]
    pub height: usize,
    //Output path of the image
    #[arg(short, long)]
    pub output: Option<String>,
    //Color palette to use
    #[arg(short, long, value_enum, default_value_t = ColorScheme::Inferno)]
    pub color: ColorScheme,
    //Noise function to use
    #[arg(short, long, value_enum, default_value_t = NoiseFunction::OpenSimplex)]
    pub noise_function: NoiseFunction,
}

#[derive(Debug, Clone, ValueEnum)]
pub enum ColorScheme {
    Viridis,
    Magma,
    Inferno,
    Rainbow,
    Monochrome,
}

#[derive(Debug, Clone, ValueEnum)]
pub enum NoiseFunction {
    Perlin,
    OpenSimplex,
}
