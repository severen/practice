pub fn insertion_sort(xs: &mut [impl Ord]) {
    for i in 0..xs.len() {
        for j in (0..i).rev() {
            if xs[j] > xs[j + 1] {
                xs.swap(j, j + 1);
            } else {
                break;
            }
        }
    }
}
