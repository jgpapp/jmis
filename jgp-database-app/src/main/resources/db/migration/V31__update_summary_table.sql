ALTER TABLE county_summary RENAME COLUMN county_code TO gender_category;
ALTER TABLE county_summary RENAME TO data_summary;
DELETE FROM data_summary;