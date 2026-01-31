
DELETE from data_summary;
ALTER TABLE data_summary ADD COLUMN year_number int not null;
ALTER TABLE data_summary ADD COLUMN year_quarter VARCHAR(200) not null;
ALTER TABLE data_summary ADD COLUMN quarter_number int not null;
ALTER TABLE data_summary ADD COLUMN month_number int not null;
ALTER TABLE data_summary ADD COLUMN week_number int not null;


