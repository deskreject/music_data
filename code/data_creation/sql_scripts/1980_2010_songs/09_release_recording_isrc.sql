-- creating the recordings table with 
-- recording name, medium name, medium format, artist credit name 
-- additionally, a number of ids for later joining/matching
DROP TABLE IF EXISTS created_tables.release_recordings_80_10;
CREATE TABLE created_tables.release_recordings_80_10 AS
SELECT
    crl.id_release AS id_release,
    m.id AS id_medium,
    m.name AS name_medium,
    mt.name AS name_medium_format,
    t.id AS id_track,
    t.gid AS mbid_track,
    t.recording AS id_recording,
    t.name AS name_track,
    r.gid AS mbid_recording,
    r.name AS name_recording,
    r.artist_credit AS id_artist_credit,
    ac.name AS name_artist_credit,
    i.isrc
FROM created_tables.country_release_label_80_10 crl
JOIN musicbrainz.medium m ON crl.id_release = m.release
JOIN musicbrainz.track t ON m.id = t.medium
JOIN musicbrainz.recording r ON t.recording = r.id
LEFT JOIN musicbrainz.isrc i ON r.id = i.recording
LEFT JOIN musicbrainz.medium_format mt ON m.format = mt.id
LEFT JOIN musicbrainz.artist_credit ac ON r.artist_credit = ac.id;
