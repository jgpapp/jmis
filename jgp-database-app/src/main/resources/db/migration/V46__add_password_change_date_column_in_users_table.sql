update users set last_modified BIGINT = CURRENT_TIMESTAMP where last_modified IS NULL;
ALTER TABLE users ADD COLUMN date_password_changed date DEFAULT CURRENT_DATE;