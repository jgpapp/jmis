CREATE INDEX IF NOT EXISTS PARTNER_ON_LOAN
ON loans(partner_id);

CREATE INDEX IF NOT EXISTS PARTNER_AND_DATE_DISBURSED_ON_LOAN
ON loans(date_disbursed);

CREATE INDEX IF NOT EXISTS PARTNER_AND_DATE_DISBURSED_AND_AND_PARTICIPANT_ON_LOAN
ON loans(partner_id, date_disbursed, participant_id);

CREATE INDEX IF NOT EXISTS PARTNER_ON_BMO
ON bmo_participants_data(partner_id);

CREATE INDEX IF NOT EXISTS DATE_RECORDED_ON_BMO
ON bmo_participants_data(date_partner_recorded);

CREATE INDEX IF NOT EXISTS PARTNER_AND_DATE_RECORDED_AND_PARTICIPANT_ON_BMO
ON bmo_participants_data(partner_id, date_partner_recorded, participant_id);