SELECT rl.release AS id_release, 
       rl.label AS id_label
INTO created_tables.intermediate_release_label -- this will create a new table to hold the intermediate results
FROM musicbrainz.release_label rl
WHERE rl.release IN (SELECT id_release FROM created_tables.country_release_1980_2010);
