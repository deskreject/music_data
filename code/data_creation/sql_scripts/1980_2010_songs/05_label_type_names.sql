-- join in the label type name to the label_names table
-- Step 1 and 2: Join and select the desired columns with the renamed column
CREATE TABLE created_tables.temp_label_names AS
SELECT ln.*, 
       lt.name AS name_label_type
FROM created_tables.label_names ln
LEFT JOIN musicbrainz.label_type lt ON ln.id_type_label = lt.id;

-- Step 3: Drop the old label_names table (if you are sure you want to replace it)
DROP TABLE created_tables.label_names;

-- Step 4: Create the final label_names table without the id_type_label column
CREATE TABLE created_tables.label_names AS
SELECT id_release, 
       id_label, 
       name_label, 
       id_area_label, 
       name_label_type
FROM created_tables.temp_label_names;

-- Step 5: Drop the temporary table
DROP TABLE created_tables.temp_label_names;
