
DELETE FROM data_summary;

ALTER TABLE data_summary DROP CONSTRAINT IF EXISTS county_code_and_data_date_on_county_summary;
ALTER TABLE data_summary DROP CONSTRAINT IF EXISTS unique_county_summary;

ALTER TABLE data_summary DROP COLUMN data_year;
ALTER TABLE data_summary DROP COLUMN data_month;

ALTER TABLE data_summary ADD COLUMN amount_repaid DECIMAL(19, 4);
ALTER TABLE data_summary ADD COLUMN summary_date DATE;

ALTER TABLE data_summary ADD CONSTRAINT unique_county_summary UNIQUE (partner_id, gender_category, summary_date);
