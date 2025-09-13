ALTER TABLE import_document ADD COLUMN is_deleted BOOLEAN DEFAULT false;
ALTER TABLE appuser_role ADD COLUMN is_deleted BOOLEAN DEFAULT false;
ALTER TABLE data_summary ADD COLUMN is_deleted BOOLEAN DEFAULT false;
ALTER TABLE loan_transactions ADD COLUMN is_deleted BOOLEAN DEFAULT false;
ALTER TABLE participants ADD COLUMN is_deleted BOOLEAN DEFAULT false;
ALTER TABLE partners ADD COLUMN is_deleted BOOLEAN DEFAULT false;
ALTER TABLE permission ADD COLUMN is_deleted BOOLEAN DEFAULT false;
ALTER TABLE role_permission ADD COLUMN is_deleted BOOLEAN DEFAULT false;
ALTER TABLE user_roles ADD COLUMN is_deleted BOOLEAN DEFAULT false;
ALTER TABLE users ADD COLUMN is_deleted BOOLEAN DEFAULT false;
ALTER TABLE jgp_document ADD COLUMN is_deleted BOOLEAN DEFAULT false,
    ADD COLUMN import_entity_type VARCHAR(100) DEFAULT NULL;
ALTER TABLE bmo_participants_data ADD COLUMN upload_doc_id BIGINT DEFAULT NULL,
    ADD COLUMN is_deleted BOOLEAN DEFAULT false,
    ADD CONSTRAINT M_DOCUMENT_ON_BMO FOREIGN KEY (upload_doc_id) REFERENCES jgp_document(id);
ALTER TABLE loans ADD COLUMN upload_doc_id BIGINT DEFAULT NULL,
    ADD COLUMN is_deleted BOOLEAN DEFAULT false,
    ADD CONSTRAINT M_DOCUMENT_ON_LOAN FOREIGN KEY (upload_doc_id) REFERENCES jgp_document(id);
ALTER TABLE outcome_monitoring ADD COLUMN upload_doc_id BIGINT DEFAULT NULL,
    ADD COLUMN is_deleted BOOLEAN DEFAULT false,
    ADD CONSTRAINT M_DOCUMENT_ON_MONITOR FOREIGN KEY (upload_doc_id) REFERENCES jgp_document(id);
ALTER TABLE mentor_ships ADD COLUMN upload_doc_id BIGINT DEFAULT NULL,
    ADD COLUMN is_deleted BOOLEAN DEFAULT false,
    ADD CONSTRAINT M_DOCUMENT_ON_MENTOR FOREIGN KEY (upload_doc_id) REFERENCES jgp_document(id);