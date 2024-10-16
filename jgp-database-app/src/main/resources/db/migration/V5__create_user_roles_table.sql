CREATE TABLE user_roles (
	id                BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
	date_created timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modified timestamp NULL,
    role_name VARCHAR(255) NULL,
    description VARCHAR(255) NULL,
	created_by_id BIGINT NULL,
	last_modified_by_id BIGINT NULL,
	CONSTRAINT role_pkey PRIMARY KEY (id),
	CONSTRAINT M_USER_ON_MODIFIED_BY FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
	CONSTRAINT M_USER_ON_CREATED_BY FOREIGN KEY (created_by_id) REFERENCES users(id)
);

CREATE TABLE appuser_role (
	appuser_id BIGINT NULL,
	role_id BIGINT NULL,
	CONSTRAINT user_role_pkey PRIMARY KEY (appuser_id, role_id),
	CONSTRAINT M_USER_ROLE_ON_USER FOREIGN KEY (appuser_id) REFERENCES users(id),
    CONSTRAINT M_USER_ROLE_ON_ROLE FOREIGN KEY (role_id) REFERENCES user_roles(id)
);

CREATE TABLE permission (
	id                BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
	date_created timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modified timestamp NULL,
    code VARCHAR(255) NULL,
    entity_name VARCHAR(255) NULL,
    action_name VARCHAR(255) NULL,
	created_by_id BIGINT NULL,
	last_modified_by_id BIGINT NULL,
	CONSTRAINT perm_pkey PRIMARY KEY (id),
	CONSTRAINT M_USER_ON_MODIFIED_BY FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
	CONSTRAINT M_USER_ON_CREATED_BY FOREIGN KEY (created_by_id) REFERENCES users(id)
);

CREATE TABLE role_permission (
	role_id BIGINT NULL,
	permission_id BIGINT NULL,
	CONSTRAINT role_perm_pkey PRIMARY KEY (role_id, permission_id),
	CONSTRAINT M_ROLE_PERM_ON_ROLE FOREIGN KEY (role_id) REFERENCES user_roles(id),
    CONSTRAINT M_ROLE_PERM_ON_PERM FOREIGN KEY (permission_id) REFERENCES permission(id)
);

ALTER TABLE users ADD COLUMN town VARCHAR(255) NULL