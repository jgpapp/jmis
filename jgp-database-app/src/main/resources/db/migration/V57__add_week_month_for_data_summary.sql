
ALTER TABLE data_summary ADD COLUMN summary_year VARCHAR(200);
ALTER TABLE data_summary ADD COLUMN summary_month VARCHAR(200);
ALTER TABLE data_summary ADD COLUMN summary_week VARCHAR(200);

UPDATE data_summary set summary_year = EXTRACT(YEAR FROM summary_date)::VARCHAR,
    summary_month = TO_CHAR(DATE_TRUNC('month', summary_date), 'FMMonth-YYYY'),
    summary_week = TO_CHAR(DATE_TRUNC('week', summary_date), 'Mon DD') || ' - ' || TO_CHAR(DATE_TRUNC('week', summary_date) + INTERVAL '6 days', 'Mon DD');
