-- Add indexes to improve the query performance
CREATE INDEX IF NOT EXISTS idx_name_recording ON created_tables.release_recordings_80_10 (name_recording);
CREATE INDEX IF NOT EXISTS idx_name_artist_credit ON created_tables.release_recordings_80_10 (name_artist_credit);
CREATE INDEX IF NOT EXISTS idx_isrc ON created_tables.release_recordings_80_10 (isrc);
