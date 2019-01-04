#[macro_use]
extern crate criterion;

extern crate grains;

use criterion::Criterion;

fn benchmark(c: &mut Criterion) {
    c.bench_function("square 34", |b| b.iter(|| grains::square(34)));
    c.bench_function("total", |b| b.iter(|| grains::total()));
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
