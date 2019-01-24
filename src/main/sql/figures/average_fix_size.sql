select avg(patch_line_count)
from test_patch
where succeeded = 1 and patch_line_count > 0;
