INSERT INTO permission (entity_name, action_name, code)VALUES('SYSTEM_RESOURCE', 'UPLOAD', 'SYSTEM_RESOURCE_UPLOAD');

ALTER TABLE loans DROP COLUMN is_repeat_customer;
ALTER TABLE loans DROP COLUMN tranch_amount_allocated;
ALTER TABLE loans DROP COLUMN tranch_amount_disbursed;
ALTER TABLE loans DROP COLUMN loan_type;
ALTER TABLE loans DROP COLUMN loan_amount_applied;
ALTER TABLE loans DROP COLUMN loan_amount_approved;
ALTER TABLE loans DROP COLUMN loan_amount_usd;
ALTER TABLE loans RENAME COLUMN loan_amount_accessed TO loan_amount;
ALTER TABLE participants ADD COLUMN pre_payment DECIMAL(19,4) NULL;

CREATE TABLE loan_transactions (
	id                BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
	date_created timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modified timestamp NULL,
	loan_id BIGINT NULL,
    amount DECIMAL(19,4) NULL,
    out_standing_amount DECIMAL(19,4) NULL,
    tranch VARCHAR(255) NULL,
    transaction_type VARCHAR(255) NULL,
    transaction_date date DEFAULT CURRENT_DATE,
	created_by_id BIGINT NULL,
	last_modified_by_id BIGINT NULL,
	CONSTRAINT loanTransaction_pkey PRIMARY KEY (id),
    CONSTRAINT M_LOAN_ON_LOAN_TRANSACTION FOREIGN KEY (loan_id) REFERENCES loans(id),
	CONSTRAINT M_MODIFIED_BY_ON_LOAN_TRANSACTION FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
	CONSTRAINT M_CREATED_BY_ON_LOAN_TRANSACTION FOREIGN KEY (created_by_id) REFERENCES users(id)
);


INSERT INTO loan_transactions(loan_id, amount, out_standing_amount, tranch, transaction_type)
SELECT l.id, l.loan_amount, l.loan_outstanding_amount, 'Full Loan' as tranch, 'DISBURSEMENT' as transaction_type from loans l;
