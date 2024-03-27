-- left join the label names and remove all releases without a label name
-- requires the "country_release_label_80_10" table
DROP TABLE IF EXISTS created_tables.release_group_type;
CREATE TABLE created_tables.release_group_type AS
SELECT
    r.id AS id_release,
    r.gid AS mbid_release,
    r.name AS name_release,
	r.artist_credit AS id_artist_credit,
    r.release_group AS id_release_group,
    rg.name AS name_release_group,
    rg.type AS id_type_release_group,
    rgpt.name AS name_release_g_type
FROM
    musicbrainz.release r
LEFT JOIN musicbrainz.release_group rg ON r.release_group = rg.id
LEFT JOIN musicbrainz.release_group_primary_type rgpt ON rg.type = rgpt.id
WHERE
    r.id IN (SELECT id_release FROM created_tables.country_release_label_80_10);

