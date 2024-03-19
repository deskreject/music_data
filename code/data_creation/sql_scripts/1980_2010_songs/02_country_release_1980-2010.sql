-- Select the release ids from the release_country table filtering on the id_country column of the created_tables.area_country_codes
DROP TABLE IF EXISTS created_tables.country_release_1980_2010;
CREATE TABLE created_tables.country_release_1980_2010 AS
SELECT rc.release AS id_release, 
       rc.date_year, 
       rc.date_month, 
       rc.date_day, 
       ac.id_country,
	   ac.name_country
FROM musicbrainz.release_country rc
JOIN created_tables.area_country_codes ac ON rc.country = ac.id_country
WHERE rc.date_year >= 1980 AND rc.date_year <= 2010;
