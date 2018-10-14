select round(max(100.0 * cast(with_failures as float) / t), 1)
from
(
  select sum(case when flaky_found > 0 then 1 else 0 end) as with_failures, count(*) as t
  from detection_round_failures
  where round_type <> 'flaky'
) i