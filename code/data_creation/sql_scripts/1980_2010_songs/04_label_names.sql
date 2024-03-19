DROP TABLE IF EXISTS created_tables.label_names;
CREATE TABLE created_tables.label_names AS
SELECT irl.id_release, 
       l.id AS id_label,
       l.name AS name_label, 
       l.area AS id_area_label, 
       l.type AS id_type_label -- Replace 'type' with the actual column if different.
FROM created_tables.intermediate_release_label irl
JOIN musicbrainz.label l ON irl.id_label = l.id;
