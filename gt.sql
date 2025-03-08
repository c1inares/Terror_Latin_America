use Terror_Latin_America;

#overview
select * from GT;

#only attacks from El Salvador and Guatemala
DELETE FROM GT
WHERE country NOT IN (61, 83);

#delete certain columns
ALTER TABLE GT
DROP COLUMN motive;
ALTER TABLE GT
DROP COLUMN kidhijcountry;
ALTER TABLE GT
DROP COLUMN hostkidoutcome_txt;

CREATE TEMPORARY TABLE GT_temp AS
SELECT gname
FROM GT
GROUP BY gname
HAVING COUNT(*) >= 15; #only keep gname with at least 15 counts

#set gname with less than 15 counts as 'Other'
UPDATE GT
SET GT.gname = 'Other'
WHERE GT.gname NOT IN (SELECT gname FROM GT_temp);

DROP TEMPORARY TABLE GT_temp;

#check
Select DISTINCT gname 
from GT;