
ALTER TABLE county_summary DROP CONSTRAINT IF EXISTS unique_county_summary;
DROP INDEX COUNTY_CODE_AND_DATA_DATE_ON_COUNTY_SUMMARY;
ALTER TABLE county_summary ADD COLUMN data_year INT NULL;
ALTER TABLE county_summary ADD COLUMN data_month INT NULL;
CREATE INDEX IF NOT EXISTS COUNTY_CODE_AND_DATA_DATE_ON_COUNTY_SUMMARY ON county_summary(partner_id, county_code, data_year, data_month);
ALTER TABLE county_summary ADD CONSTRAINT unique_county_summary UNIQUE (partner_id, county_code, data_year, data_month);