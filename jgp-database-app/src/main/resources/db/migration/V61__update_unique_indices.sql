
DROP INDEX IF EXISTS unique_participant;
CREATE UNIQUE INDEX unique_participant ON participants (jgp_id) WHERE data_status IN ('APPROVED', 'PENDING_APPROVAL');
DROP INDEX IF EXISTS unique_loan;
CREATE UNIQUE INDEX unique_loan ON loans (partner_id, participant_id, date_disbursed, loan_number) WHERE data_status IN ('APPROVED', 'PENDING_APPROVAL');
DROP INDEX IF EXISTS unique_ta_participant_data;
CREATE UNIQUE INDEX unique_ta_participant_data ON ta_participants_data (partner_id, participant_id, date_partner_recorded) WHERE data_status IN ('APPROVED', 'PENDING_APPROVAL');









