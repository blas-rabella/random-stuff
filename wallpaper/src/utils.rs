pub fn remap(t: f64, a: f64, b: f64, c: f64, d: f64) -> f64 {
    (t - a) * ((d - c) / (b - a)) + c
}

pub fn discretize(val: f64, steps: &Vec<f64>) -> f64 {
    let (i, _) = steps
        .iter()
        .enumerate()
        .map(|(i, &x)| (i, (val - x).abs()))
        .min_by(|x, y| x.1.total_cmp(&y.1))
        .unwrap();
    steps[i]
}
