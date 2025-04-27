UPDATE loan_transactions lt
SET transaction_date = l.date_disbursed,
is_approved = l.data_is_approved
FROM loans l
WHERE lt.loan_id = l.id;
