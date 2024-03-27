-- Drop the temp table if it exists
DROP TABLE IF EXISTS temp_subset;

-- Create the target table with unique values based on your criteria
CREATE TABLE created_tables.release_recordings_unique_80_10 AS
WITH Concatenated AS (
    SELECT
        id_release,
        name_medium_format,
        id_track,
        mbid_track,
        id_recording,
        mbid_recording,
        COALESCE(name_recording, 'NULL') || '_' || 
        COALESCE(name_artist_credit, 'NULL') || '_' || 
        COALESCE(isrc, 'NULL') AS unique_concat,
        name_recording,
        id_artist_credit,
        name_artist_credit,
        isrc
    FROM created_tables.release_recordings_80_10
)
SELECT DISTINCT ON (unique_concat)
    id_release,
    name_medium_format,
    id_track,
    mbid_track,
    id_recording,
    mbid_recording,
    name_recording,
    id_artist_credit,
    name_artist_credit,
    isrc
FROM Concatenated
ORDER BY unique_concat, id_release;
