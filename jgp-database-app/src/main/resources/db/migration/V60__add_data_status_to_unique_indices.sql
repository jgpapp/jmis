
ALTER TABLE appuser_role ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE data_summary ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE import_document ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE jgp_document ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE loan_transactions ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE loans ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE mentor_ships ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE outcome_monitoring ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE participants ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE partners ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE permission ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE role_permission ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE system_user_access_logs ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE ta_participants_data ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE user_audit_logs ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE users ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';
ALTER TABLE user_roles ADD COLUMN IF NOT EXISTS data_status VARCHAR(255) DEFAULT 'APPROVED';

UPDATE appuser_role SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE data_summary SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE import_document SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE jgp_document SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE loan_transactions SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE loans SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE mentor_ships SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE outcome_monitoring SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE participants SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE partners SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE permission SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE role_permission SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE system_user_access_logs SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE ta_participants_data SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE user_audit_logs SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE users SET data_status = 'DELETED' WHERE is_deleted = true;
UPDATE user_roles SET data_status = 'DELETED' WHERE is_deleted = true;

DROP INDEX IF EXISTS client_unique;
DROP INDEX IF EXISTS unique_loan;
ALTER TABLE data_summary DROP CONSTRAINT IF EXISTS unique_county_summary;
DROP INDEX IF EXISTS name_unique;
DROP INDEX IF EXISTS "UNIQUE_PERMISSION";
ALTER TABLE system_user_access_logs DROP CONSTRAINT IF EXISTS uk_username_login_date_login_hour;
DROP INDEX IF EXISTS unique_bmo_participant_data;
ALTER TABLE user_audit_logs DROP CONSTRAINT IF EXISTS user_log_time_unique;
DROP INDEX IF EXISTS UNIQUE_ROLE;
DROP INDEX IF EXISTS unique_role;
DROP INDEX IF EXISTS email_unique;

CREATE UNIQUE INDEX unique_participant ON participants (jgp_id) WHERE data_status = 'APPROVED';
CREATE UNIQUE INDEX unique_loan ON loans (partner_id, participant_id, date_disbursed, loan_number) WHERE data_status = 'APPROVED';
CREATE UNIQUE INDEX unique_data_summary on data_summary (partner_id, gender_category, summary_date) WHERE data_status = 'APPROVED';
CREATE UNIQUE INDEX name_unique ON partners (partner_name) WHERE data_status = 'APPROVED';
CREATE UNIQUE INDEX "UNIQUE_PERMISSION" ON "permission" ("code") WHERE data_status = 'APPROVED';
CREATE UNIQUE INDEX uk_username_login_date_login_hour ON system_user_access_logs (username, login_date, login_hour) WHERE data_status = 'APPROVED';
CREATE UNIQUE INDEX unique_ta_participant_data ON ta_participants_data (partner_id, participant_id, date_partner_recorded) WHERE data_status = 'APPROVED';
CREATE UNIQUE INDEX user_log_time_unique ON user_audit_logs (username, log_time) WHERE data_status = 'APPROVED';
CREATE UNIQUE INDEX UNIQUE_ROLE ON user_roles (role_name) WHERE data_status = 'APPROVED';
CREATE UNIQUE INDEX email_unique ON users (email_address) WHERE data_status = 'APPROVED';

ALTER TABLE appuser_role DROP COLUMN is_deleted;
ALTER TABLE data_summary DROP COLUMN is_deleted;
ALTER TABLE import_document DROP COLUMN is_deleted;
ALTER TABLE jgp_document DROP COLUMN is_deleted;
ALTER TABLE loan_transactions DROP COLUMN is_deleted;
ALTER TABLE loans DROP COLUMN is_deleted;
ALTER TABLE mentor_ships DROP COLUMN is_deleted;
ALTER TABLE outcome_monitoring DROP COLUMN is_deleted;
ALTER TABLE participants DROP COLUMN is_deleted;
ALTER TABLE partners DROP COLUMN is_deleted;
ALTER TABLE permission DROP COLUMN is_deleted;
ALTER TABLE role_permission DROP COLUMN is_deleted;
ALTER TABLE system_user_access_logs DROP COLUMN is_deleted;
ALTER TABLE ta_participants_data DROP COLUMN is_deleted;
ALTER TABLE user_audit_logs DROP COLUMN is_deleted;
ALTER TABLE users DROP COLUMN is_deleted;
ALTER TABLE user_roles DROP COLUMN is_deleted;









