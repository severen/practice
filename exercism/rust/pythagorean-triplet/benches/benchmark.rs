#[macro_use]
extern crate criterion;

extern crate pythagorean_triplet;

use criterion::Criterion;

fn benchmark(c: &mut Criterion) {
    c.bench_function("find", |b| b.iter(|| pythagorean_triplet::find()));
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
