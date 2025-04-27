ALTER TABLE participants DROP COLUMN is_business_registered;
ALTER TABLE participants DROP COLUMN has_bmo_membership;
ALTER TABLE participants DROP COLUMN bmo_membership;
ALTER TABLE participants DROP COLUMN corp_pin_number;
ALTER TABLE loan_transactions ADD COLUMN is_given_in_tranches BOOLEAN DEFAULT false;