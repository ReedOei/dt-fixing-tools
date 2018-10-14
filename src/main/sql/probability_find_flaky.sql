select max(100.0 * cast(with_failures as float) / t)
from
(
  select sum(case when random_found > 0 then 1 else 0 end) as with_failures, count(*) as t
  from detection_round_failures
  group by round_type
) i