mod insertion_sort;

use crate::insertion_sort::insertion_sort;

fn main() {
  let mut nums = [2, 1, 3, 5, 6, 7, 4];
  insertion_sort(&mut nums);
  dbg!(&nums);
}
