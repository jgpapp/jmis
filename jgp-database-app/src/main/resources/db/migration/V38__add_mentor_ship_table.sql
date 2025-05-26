CREATE TABLE mentor_ships (
	id                BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
	date_created timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modified timestamp NULL,
	partner_id BIGINT NULL,
    participant_id BIGINT NULL,
    mentor_ship_date date NOT NULL,
    mentor_ship_org VARCHAR(255) NULL,
    bmo_member_ship VARCHAR(255) NULL,
    mentor_ship_delivery_mode VARCHAR(255) NULL,
    business_situation VARCHAR(255) NULL,
    new_hires_due_to_loan INT NULL,
    revenue_due_to_train DECIMAL(19,4) NULL,
    useful_training_topics VARCHAR(255) NULL,
    support_needed_areas VARCHAR(255) NULL,
    msme_sessions_covered VARCHAR(255) NULL,
    sme_sessions_covered VARCHAR(255) NULL,
    identified_business_gaps VARCHAR(255) NULL,
    agreed_action_for_gap_one VARCHAR(255) NULL,
    additional_needed_support VARCHAR(255) NULL,
	created_by_id BIGINT NULL,
	last_modified_by_id BIGINT NULL,
	CONSTRAINT mentor_pkey PRIMARY KEY (id),
	CONSTRAINT M_MENTOR_ON_MODIFIED_BY FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
	CONSTRAINT M_MENTOR_ON_CREATED_BY FOREIGN KEY (created_by_id) REFERENCES users(id),
	CONSTRAINT M_MENTOR_ON_PARTNER FOREIGN KEY (partner_id) REFERENCES partners(id),
	CONSTRAINT M_MENTOR_ON_PARTICIPANT FOREIGN KEY (participant_id) REFERENCES participants(id)
);

ALTER TABLE participants ADD COLUMN location_sub_county VARCHAR(100) DEFAULT NULL;
ALTER TABLE participants ADD COLUMN location_latitude DECIMAL(19,4) DEFAULT NULL;
ALTER TABLE participants ADD COLUMN location_longitude DECIMAL(19,4) DEFAULT NULL;

