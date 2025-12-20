-- public.system_user_access_logs definition

ALTER TABLE bmo_participants_data RENAME TO ta_participants_data;
CREATE SEQUENCE IF NOT EXISTS ta_participants_data_seq INCREMENT BY 50;
SELECT setval('ta_participants_data_seq', COALESCE((SELECT MAX(id) FROM ta_participants_data), 1000001) + 100, false);
ALTER TABLE ta_participants_data ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE ta_participants_data ALTER COLUMN id SET DEFAULT nextval('ta_participants_data_seq');

CREATE SEQUENCE IF NOT EXISTS data_summary_seq INCREMENT BY 50;
SELECT setval('data_summary_seq', COALESCE((SELECT MAX(id) FROM data_summary), 1000001) + 100, false);
ALTER TABLE data_summary ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE data_summary ALTER COLUMN id SET DEFAULT nextval('data_summary_seq');

CREATE SEQUENCE IF NOT EXISTS partners_seq INCREMENT BY 50;
SELECT setval('partners_seq', COALESCE((SELECT MAX(id) FROM partners), 1000001) + 100, false);
ALTER TABLE partners ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE partners ALTER COLUMN id SET DEFAULT nextval('partners_seq');

CREATE SEQUENCE IF NOT EXISTS participants_seq INCREMENT BY 50;
SELECT setval('participants_seq', COALESCE((SELECT MAX(id) FROM participants), 1000001) + 100, false);
ALTER TABLE participants ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE participants ALTER COLUMN id SET DEFAULT nextval('participants_seq');

CREATE SEQUENCE IF NOT EXISTS user_audit_logs_seq INCREMENT BY 50;
SELECT setval('user_audit_logs_seq', COALESCE((SELECT MAX(id) FROM user_audit_logs), 1000001) + 100, false);
ALTER TABLE user_audit_logs ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE user_audit_logs ALTER COLUMN id SET DEFAULT nextval('user_audit_logs_seq');

CREATE SEQUENCE IF NOT EXISTS system_user_access_logs_seq INCREMENT BY 50;
SELECT setval('system_user_access_logs_seq', COALESCE((SELECT MAX(id) FROM system_user_access_logs), 1000001) + 100, false);
ALTER TABLE system_user_access_logs ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE system_user_access_logs ALTER COLUMN id SET DEFAULT nextval('system_user_access_logs_seq');

CREATE SEQUENCE IF NOT EXISTS loans_seq INCREMENT BY 50;
SELECT setval('loans_seq', COALESCE((SELECT MAX(id) FROM loans), 1000001) + 100, false);
ALTER TABLE loans ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE loans ALTER COLUMN id SET DEFAULT nextval('loans_seq');

CREATE SEQUENCE IF NOT EXISTS permission_seq INCREMENT BY 50;
SELECT setval('permission_seq', COALESCE((SELECT MAX(id) FROM permission), 1000001) + 100, false);
ALTER TABLE permission ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE permission ALTER COLUMN id SET DEFAULT nextval('permission_seq');

CREATE SEQUENCE IF NOT EXISTS outcome_monitoring_seq INCREMENT BY 50;
SELECT setval('outcome_monitoring_seq', COALESCE((SELECT MAX(id) FROM outcome_monitoring), 1000001) + 100, false);
ALTER TABLE outcome_monitoring ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE outcome_monitoring ALTER COLUMN id SET DEFAULT nextval('outcome_monitoring_seq');

CREATE SEQUENCE IF NOT EXISTS users_seq INCREMENT BY 50;
SELECT setval('users_seq', COALESCE((SELECT MAX(id) FROM users), 1000001) + 100, false);
ALTER TABLE users ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE users ALTER COLUMN id SET DEFAULT nextval('users_seq');

CREATE SEQUENCE IF NOT EXISTS import_document_seq INCREMENT BY 50;
SELECT setval('import_document_seq', COALESCE((SELECT MAX(id) FROM import_document), 1000001) + 100, false);
ALTER TABLE import_document ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE import_document ALTER COLUMN id SET DEFAULT nextval('import_document_seq');

CREATE SEQUENCE IF NOT EXISTS mentor_ships_seq INCREMENT BY 50;
SELECT setval('mentor_ships_seq', COALESCE((SELECT MAX(id) FROM mentor_ships), 1000001) + 100, false);
ALTER TABLE mentor_ships ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE mentor_ships ALTER COLUMN id SET DEFAULT nextval('mentor_ships_seq');

CREATE SEQUENCE IF NOT EXISTS jgp_document_seq INCREMENT BY 50;
SELECT setval('jgp_document_seq', COALESCE((SELECT MAX(id) FROM jgp_document), 1000001) + 100, false);
ALTER TABLE jgp_document ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE jgp_document ALTER COLUMN id SET DEFAULT nextval('jgp_document_seq');

CREATE SEQUENCE IF NOT EXISTS partners_seq INCREMENT BY 50;
SELECT setval('partners_seq', COALESCE((SELECT MAX(id) FROM partners), 1000001) + 100, false);
ALTER TABLE partners ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE partners ALTER COLUMN id SET DEFAULT nextval('partners_seq');

CREATE SEQUENCE IF NOT EXISTS user_roles_seq INCREMENT BY 50;
SELECT setval('user_roles_seq', COALESCE((SELECT MAX(id) FROM user_roles), 1000001) + 100, false);
ALTER TABLE user_roles ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE user_roles ALTER COLUMN id SET DEFAULT nextval('user_roles_seq');

CREATE SEQUENCE IF NOT EXISTS loan_transactions_seq INCREMENT BY 50;
SELECT setval('loan_transactions_seq', COALESCE((SELECT MAX(id) FROM loan_transactions), 1000001) + 100, false);
ALTER TABLE loan_transactions ALTER COLUMN id DROP IDENTITY IF EXISTS;
ALTER TABLE loan_transactions ALTER COLUMN id SET DEFAULT nextval('loan_transactions_seq');