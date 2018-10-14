select 100.0 * cast(with_failures as float) / total
from
(
  select sum(case when failures > 0 then 1 else 0 end) as with_failures, count(*) as total
  from detection_round_failures
  where round_type like '%random%'
) i
