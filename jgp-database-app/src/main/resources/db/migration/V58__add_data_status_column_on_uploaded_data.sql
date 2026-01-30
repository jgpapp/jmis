
ALTER TABLE mentor_ships ADD COLUMN data_status VARCHAR(200);
ALTER TABLE ta_participants_data ADD COLUMN data_status VARCHAR(200);
ALTER TABLE loans ADD COLUMN data_status VARCHAR(200);
ALTER TABLE loan_transactions ADD COLUMN data_status VARCHAR(200);
ALTER TABLE outcome_monitoring ADD COLUMN data_status VARCHAR(200);
ALTER TABLE participants ADD COLUMN data_status VARCHAR(200);

UPDATE mentor_ships set data_status = 'APPROVED' where is_approved = true;
UPDATE mentor_ships set data_status = 'PENDING_APPROVAL' where is_approved = false;

UPDATE ta_participants_data set data_status = 'APPROVED' where data_is_approved = true;
UPDATE ta_participants_data set data_status = 'PENDING_APPROVAL' where data_is_approved = false;

UPDATE loans set data_status = 'APPROVED' where data_is_approved = true;
UPDATE loans set data_status = 'PENDING_APPROVAL' where data_is_approved = false;

UPDATE loan_transactions set data_status = 'APPROVED' where is_approved = true;
UPDATE loan_transactions set data_status = 'PENDING_APPROVAL' where is_approved = false;

UPDATE outcome_monitoring set data_status = 'APPROVED' where is_approved = true;
UPDATE outcome_monitoring set data_status = 'PENDING_APPROVAL' where is_approved = false;

UPDATE participants set data_status = 'APPROVED';

ALTER TABLE mentor_ships DROP COLUMN is_approved;
ALTER TABLE ta_participants_data DROP COLUMN data_is_approved;
ALTER TABLE loans DROP COLUMN data_is_approved;
ALTER TABLE loan_transactions DROP COLUMN is_approved;
ALTER TABLE outcome_monitoring DROP COLUMN is_approved;
