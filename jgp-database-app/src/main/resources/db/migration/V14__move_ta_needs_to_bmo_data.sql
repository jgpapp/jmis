ALTER TABLE bmo_participants_data ADD COLUMN ta_needs VARCHAR(255) NULL;
UPDATE bmo_participants_data set ta_needs = (SELECT p.ta_needs from participants p where p.id = participant_id);
ALTER TABLE participants DROP COLUMN ta_needs;
INSERT INTO permission (entity_name, action_name, code)VALUES('DASHBOARD', 'VIEW_WITH_PARTNER_FILTER', 'DASHBOARD_VIEW_WITH_PARTNER_FILTER');
INSERT INTO permission (entity_name, action_name, code)VALUES('USERS', 'LOCK', 'USERS_LOCK');
INSERT INTO permission (entity_name, action_name, code)VALUES('USERS', 'UNLOCK', 'USERS_UNLOCK');