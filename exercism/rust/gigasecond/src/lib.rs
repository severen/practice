extern crate chrono;

use chrono::{DateTime, Duration, TimeZone};

/// Returns a Utc DateTime one billion seconds after start.
pub fn after<Tz: TimeZone>(start: DateTime<Tz>) -> DateTime<Tz> {
    start + Duration::seconds(10i64.pow(9))
}
