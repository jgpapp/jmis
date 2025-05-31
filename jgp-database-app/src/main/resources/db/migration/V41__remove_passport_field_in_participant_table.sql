ALTER TABLE participants DROP COLUMN passport;
ALTER TABLE participants
ALTER COLUMN location_latitude TYPE DECIMAL(19,10);
ALTER TABLE participants
ALTER COLUMN location_longitude TYPE DECIMAL(19,10);