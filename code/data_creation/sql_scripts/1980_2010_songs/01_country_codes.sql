-- Get the codes from the United states and canada as well as top 15 European countries 
-- save as table in created_tables schema

CREATE TABLE created_tables.area_country_codes AS
SELECT id, name
FROM musicbrainz.area
-- Country names without codes: Deutschland
WHERE name IN ('United States',
			   'Canada',
			   'Germany',
			   'United Kingdom',
			  'France',
			  'Italy',
			  'Spain',
			  'Netherlands',
			  'Sweden',
			  'Russia',
			  'Norway',
			  'Switzerland',
			  'Austria',
			  'Belgium',
			  'Denmark',
			  'Portugal',
			  'Ireland');
			  
-- Then, after the table creation, rename the columns
ALTER TABLE created_tables.area_country_codes
RENAME COLUMN id TO id_country;

ALTER TABLE created_tables.area_country_codes
RENAME COLUMN name TO name_country;