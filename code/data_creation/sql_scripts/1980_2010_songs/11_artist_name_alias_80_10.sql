DROP TABLE IF EXISTS created_tables.artist_names_aliases_80_10
CREATE TABLE created_tables.artist_names_alias_80_10 AS
SELECT 
    rru.id_artist_credit,
    acn.artist AS id_artist,
    acn.name AS name_artist_credit, -- Assuming you need artist_credit's name, not provided in your steps but mentioned in your final table structure.
    a.name AS name_artist,
    aa.name AS name_alias
FROM 
    (SELECT DISTINCT id_artist_credit FROM created_tables.release_recordings_unique_80_10) rru
LEFT JOIN musicbrainz.artist_credit_name acn ON acn.artist_credit = rru.id_artist_credit
LEFT JOIN musicbrainz.artist a ON a.id = acn.artist
LEFT JOIN musicbrainz.artist_alias aa ON aa.artist = a.id;
