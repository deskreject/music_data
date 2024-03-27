-- Step 1.1: Create a new table with an additional date_year column
DROP TABLE IF EXISTS created_tables.temp_release_recordings; 
CREATE TABLE created_tables.temp_release_recordings AS
SELECT rru.*, rc.date_year
FROM created_tables.release_recordings_unique_80_10 rru
LEFT JOIN musicbrainz.release_country rc ON rc.release = rru.id_release;

-- Step 2: Create a filtered table with specific columns
DROP TABLE IF EXISTS created_tables.export_ad_release_recordings_98_05; 
CREATE TABLE created_tables.export_ad_release_recordings_98_05 AS
SELECT mbid_recording, name_recording, name_artist_credit, isrc, date_year
FROM created_tables.temp_release_recordings
WHERE date_year BETWEEN 1998 AND 2005;

DROP TABLE IF EXISTS created_tables.temp_release_recordings;