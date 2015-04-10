DROP TABLE IF EXISTS links;
DROP TABLE IF EXISTS parsed;
DROP TABLE IF EXISTS result;
CREATE TABLE links (link STRING);
LOAD DATA INPATH 'links.json' OVERWRITE INTO TABLE links;

CREATE TABLE parsed AS
	SELECT regexp_extract(link, '\\["(.*?)", "(.*?)"\\]', 1) AS left, 
		regexp_extract(link, '\\["(.*?)", "(.*?)"\\]', 2) AS right 
	FROM links;

CREATE TABLE result AS
	SELECT * FROM parsed a 
	WHERE NOT EXISTS 
		(SELECT * FROM parsed b 
		WHERE a.left = b.right AND a.right = b.left);

SELECT * FROM result;
