DROP TABLE IF EXISTS links;
DROP TABLE IF EXISTS parsed;
DROP TABLE IF EXISTS swapped;
CREATE TABLE links (link STRING);
LOAD DATA INPATH 'links.json' OVERWRITE INTO TABLE links;

CREATE TABLE parsed AS
SELECT regexp_extract(link, '\\["(.*?)", "(.*?)"\\]', 1) AS left, regexp_extract(link, '\\["(.*?)", "(.*?)"\\]', 2) AS right, "1" FROM links;

CREATE TABLE swapped AS
SELECT left AS left, right AS right, "0" AS sw FROM parsed WHERE left > right 
UNION ALL 
SELECT right AS left, left AS right, "1" AS sw FROM parsed WHERE left < right;

SELECT * FROM swapped a WHERE NOT EXISTS (select * from swapped b where a.left = b.left AND a.right = b.right AND b.sw = "1")
UNION ALL
SELECT * FROM swapped a WHERE NOT EXISTS (select * from swapped b where a.left = b.left AND a.right = b.right AND b.sw = "0");


