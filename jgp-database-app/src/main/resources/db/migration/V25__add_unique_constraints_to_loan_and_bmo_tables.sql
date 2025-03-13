
ALTER TABLE loans ADD CONSTRAINT unique_loan UNIQUE (partner_id, participant_id, date_disbursed);
ALTER TABLE bmo_participants_data ADD CONSTRAINT unique_bmo_participant_data UNIQUE (partner_id, participant_id, date_partner_recorded);