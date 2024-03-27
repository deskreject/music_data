-- add in the mbid_release, name_release, and release_type name to the table country_release_label_80_10
CREATE TABLE created_tables.tmp_country_release_label_80_10 AS
SELECT
    crl.*,
    ri.mbid_release,
    ri.name_release,
    ri.name_release_g_type,
	ri.id_artist_credit
FROM created_tables.country_release_label_80_10 crl
LEFT JOIN created_tables.release_group_type ri ON crl.id_release = ri.id_release;
DROP TABLE created_tables.country_release_label_80_10;
ALTER TABLE created_tables.tmp_country_release_label_80_10
RENAME TO country_release_label_80_10;

