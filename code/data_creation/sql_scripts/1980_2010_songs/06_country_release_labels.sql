-- left join the label names and remove all releases without a label name
DROP TABLE IF EXISTS created_tables.country_release_label_80_10;
CREATE TABLE created_tables.country_release_label_80_10 AS
SELECT crt.*, 
       ln.name_label, 
       ln.name_label_type
FROM created_tables.country_release_1980_2010 crt
LEFT JOIN created_tables.label_names ln ON crt.id_release = ln.id_release;
-- remove the rows where name_label is NULL
DELETE FROM created_tables.country_release_label_80_10
WHERE name_label IS NULL;
