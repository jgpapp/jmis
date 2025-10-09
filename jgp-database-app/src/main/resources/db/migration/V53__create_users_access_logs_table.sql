-- public.system_user_access_logs definition

CREATE TABLE system_user_access_logs (
     id                BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
     username VARCHAR(100) NOT NULL,
     ip_address VARCHAR(100),
     login_time TIME NOT NULL,
     login_date DATE NOT NULL,
     login_hour INTEGER NOT NULL,
     date_created timestamp DEFAULT CURRENT_TIMESTAMP,
     is_deleted BOOLEAN DEFAULT false,
     last_modified timestamp NULL,
     created_by_id BIGINT NULL,
     last_modified_by_id BIGINT NULL,
     CONSTRAINT access_pkey PRIMARY KEY (id),
     CONSTRAINT uk_username_login_date_login_hour UNIQUE (username, login_date, login_hour),
     CONSTRAINT M_USER_ACCESS_ON_MODIFIED_BY FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
     CONSTRAINT M_USER_ACCESS_ON_CREATED_BY FOREIGN KEY (created_by_id) REFERENCES users(id)
);