ALTER TABLE participants ADD COLUMN participant_name VARCHAR(50) DEFAULT NULL;
UPDATE participants set participant_name = business_name;