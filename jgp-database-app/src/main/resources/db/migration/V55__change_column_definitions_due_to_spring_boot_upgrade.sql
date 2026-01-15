
ALTER TABLE jgp_document
ALTER COLUMN parent_entity_id TYPE BIGINT;

ALTER TABLE jgp_document
ALTER COLUMN doc_size TYPE BIGINT;

ALTER TABLE participants
ALTER COLUMN worst_monthly_revenue TYPE DECIMAL(19, 4) USING worst_monthly_revenue::numeric(19, 4);