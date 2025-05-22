ALTER TABLE mentor_ships ADD COLUMN is_approved BOOLEAN DEFAULT false;
ALTER TABLE mentor_ships ADD COLUMN approval_by_id BIGINT DEFAULT NULL;
ALTER TABLE mentor_ships ADD COLUMN date_approved date DEFAULT NULL;
ALTER TABLE mentor_ships ADD CONSTRAINT M_MENTORSHIP_ON_APPROVED_BY FOREIGN KEY (approval_by_id) REFERENCES users(id);