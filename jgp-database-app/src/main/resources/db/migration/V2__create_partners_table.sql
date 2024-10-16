CREATE TABLE partners (
	id                BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
	date_created timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modified timestamp NULL,
	partner_name VARCHAR(255) NULL,
	type VARCHAR(255) NULL,
	created_by_id BIGINT NULL,
	last_modified_by_id BIGINT NULL,
	CONSTRAINT name_unique UNIQUE (partner_name),
	CONSTRAINT partners_pkey PRIMARY KEY (id),
	CONSTRAINT M_USER_ON_MODIFIED_BY FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
	CONSTRAINT M_USER_ON_CREATED_BY FOREIGN KEY (created_by_id) REFERENCES users(id)
);

ALTER TABLE users ADD COLUMN partner_id BIGINT NULL;
ALTER TABLE users ADD CONSTRAINT M_PARTNER_ON_USER FOREIGN KEY (partner_id) REFERENCES partners(id)
