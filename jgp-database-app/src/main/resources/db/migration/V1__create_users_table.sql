-- public.users definition

-- Drop table

-- DROP TABLE users;

CREATE TABLE users (
	id                BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
	date_created timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modified timestamp NULL,
	cell_phone VARCHAR(255) NULL,
	designation VARCHAR(255) NULL,
	first_name VARCHAR(255) NULL,
	force_change_pass BOOLEAN NOT NULL,
	is_active BOOLEAN NOT NULL,
	is_admin BOOLEAN NOT NULL,
	last_name VARCHAR(255) NULL,
	"password" VARCHAR(255) NOT NULL,
	email_address VARCHAR(255) NOT NULL,
	created_by_id BIGINT NULL,
	last_modified_by_id BIGINT NULL,
	CONSTRAINT email_unique UNIQUE (email_address),
	CONSTRAINT users_pkey PRIMARY KEY (id),
	CONSTRAINT fk4yi7iyejvdeanf2jmu8i6dkc8 FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
	CONSTRAINT fk8nakkftyppd62ke6tv7oo5a92 FOREIGN KEY (created_by_id) REFERENCES users(id)
);