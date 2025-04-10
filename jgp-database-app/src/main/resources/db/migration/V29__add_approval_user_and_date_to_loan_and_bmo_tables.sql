ALTER TABLE bmo_participants_data ADD COLUMN approval_by_id BIGINT DEFAULT NULL;
ALTER TABLE bmo_participants_data ADD COLUMN date_approved date DEFAULT NULL;
ALTER TABLE bmo_participants_data ADD CONSTRAINT M_BMO_ON_APPROVED_BY FOREIGN KEY (approval_by_id) REFERENCES users(id);
ALTER TABLE loans ADD COLUMN approval_by_id BIGINT DEFAULT NULL;
ALTER TABLE loans ADD COLUMN date_approved date DEFAULT NULL;
ALTER TABLE loans ADD CONSTRAINT M_LOAN_ON_APPROVED_BY FOREIGN KEY (approval_by_id) REFERENCES users(id);