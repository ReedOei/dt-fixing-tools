select count(*)
from diagnosis_info
where fields >= ? and fields <= ?;
