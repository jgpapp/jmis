ALTER TABLE loan_transactions ADD COLUMN approval_by_id BIGINT DEFAULT NULL;
ALTER TABLE loan_transactions ADD COLUMN date_approved date DEFAULT NULL;
ALTER TABLE loan_transactions ADD CONSTRAINT M_LOAN_TXN_ON_APPROVED_BY FOREIGN KEY (approval_by_id) REFERENCES users(id);