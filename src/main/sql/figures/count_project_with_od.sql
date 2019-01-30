select distinct slug
from od_classification odc
inner join subject s on s.name = odc.subject_name;
