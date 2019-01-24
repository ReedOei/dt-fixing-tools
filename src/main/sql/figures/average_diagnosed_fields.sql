select ifnull(avg(fields), 0)
from diagnosis_info
where fields >= ? and fields <= ?;
