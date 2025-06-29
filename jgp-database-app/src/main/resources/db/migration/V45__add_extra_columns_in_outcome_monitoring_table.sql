ALTER TABLE participants ADD COLUMN alternative_phone_number VARCHAR(50) DEFAULT NULL;
ALTER TABLE outcome_monitoring ADD COLUMN is_approved BOOLEAN DEFAULT false;
ALTER TABLE outcome_monitoring ADD COLUMN approval_by_id BIGINT DEFAULT NULL;
ALTER TABLE outcome_monitoring ADD COLUMN date_approved date DEFAULT NULL;