ALTER TABLE loans DROP CONSTRAINT IF EXISTS unique_loan;
ALTER TABLE loans ADD CONSTRAINT unique_loan UNIQUE (partner_id, participant_id, date_disbursed, loan_number);