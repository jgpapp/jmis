
CREATE TABLE user_audit_logs (
	id                BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
	date_created timestamp DEFAULT CURRENT_DATE NOT NULL,
	last_modified timestamp NULL,
    username VARCHAR(100) NOT NULL,
    action VARCHAR(100) NOT NULL,
    details TEXT NOT NULL ,
    resource_id BIGINT NOT NULL ,
    log_time timestamp DEFAULT CURRENT_TIMESTAMP,
    is_deleted BOOLEAN DEFAULT false,
	created_by_id BIGINT NULL,
	last_modified_by_id BIGINT NULL,
	CONSTRAINT log_pkey PRIMARY KEY (id),
    CONSTRAINT user_log_time_unique UNIQUE (username, log_time)
);

