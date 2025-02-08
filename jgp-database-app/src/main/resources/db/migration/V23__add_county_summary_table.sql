
CREATE TABLE county_summary (
	id                BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
	date_created timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modified timestamp NULL,
	partner_id BIGINT NULL,
	county_code VARCHAR(255) NULL,
	businesses_trained INT NULL,
	businesses_loaned INT NULL,
	amount_disbursed DECIMAL(19,4) NULL,
	out_standing_amount DECIMAL(19,4) NULL,
	data_date date NULL,
	created_by_id BIGINT NULL,
	last_modified_by_id BIGINT NULL,
	CONSTRAINT unique_county_summary UNIQUE (partner_id, county_code, data_date),
	CONSTRAINT county_summary_pkey PRIMARY KEY (id),
	CONSTRAINT M_COUNTY_SUMMARY_ON_PARTNER FOREIGN KEY (partner_id) REFERENCES partners(id),
	CONSTRAINT M_USER_ON_MODIFIED_BY FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
	CONSTRAINT M_USER_ON_CREATED_BY FOREIGN KEY (created_by_id) REFERENCES users(id)
);

CREATE INDEX IF NOT EXISTS COUNTY_CODE_AND_DATA_DATE_ON_COUNTY_SUMMARY ON county_summary(partner_id, county_code, data_date);