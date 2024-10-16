ALTER TABLE clients RENAME TO participants;
ALTER TABLE participants ADD COLUMN is_eligible BOOLEAN NULL;
ALTER TABLE bmo_client_data RENAME TO bmo_participants_data;
ALTER TABLE bmo_participants_data RENAME COLUMN client_id TO participant_id;

CREATE TABLE loans (
	id                BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
	date_created timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modified timestamp NULL,
	partner_id BIGINT NULL,
    participant_id BIGINT NULL,
    pipeline_source VARCHAR(255) NULL,
    loan_number VARCHAR(255) NULL,
    loan_amount_applied DECIMAL(19,4) NULL,
    loan_amount_approved DECIMAL(19,4) NULL,
    loan_status VARCHAR(255) NULL,
    loan_duration INT NULL,
    loan_amount_accessed DECIMAL(19,4) NULL,
    loan_outstanding_amount DECIMAL(19,4) NULL,
    loan_quality VARCHAR(255) NULL,
    unique_values VARCHAR(255) NULL,
    date_applied date NULL,
    date_disbursed date NULL,
    is_repeat_customer BOOLEAN NULL,
    date_recorded_by_partner date NULL,
    date_added_to_db date NULL,
	created_by_id BIGINT NULL,
	last_modified_by_id BIGINT NULL,
	CONSTRAINT loan_pkey PRIMARY KEY (id),
	CONSTRAINT M_LOAN_ON_MODIFIED_BY FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
	CONSTRAINT M_LOAN_ON_CREATED_BY FOREIGN KEY (created_by_id) REFERENCES users(id),
	CONSTRAINT M_LOAN_ON_PARTNER FOREIGN KEY (partner_id) REFERENCES partners(id),
	CONSTRAINT M_LOAN_ON_PARTICIPANT FOREIGN KEY (participant_id) REFERENCES participants(id)
);