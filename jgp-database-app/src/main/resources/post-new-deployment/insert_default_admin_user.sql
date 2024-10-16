-- public.users definition

-- Drop table

-- ADD DEFAULT users;

INSERT INTO user_roles
(role_name, description)
VALUES('Super User', 'All functions');

INSERT INTO users
(cell_phone, designation, first_name, force_change_pass, is_active, last_name, "password", email_address)
VALUES('5464336455', 'Administrator', 'Admin', false, true, 'User', '$2a$10$O4gp07pGBTqthaxYkDEglOaBEY65reC1409H/DQMvmjA1CxZcNAYW', 'admin@admin.com');

INSERT INTO role_permission
(role_id, permission_id)
VALUES((SELECT rl.id from user_roles rl where rl.role_name = 'Super User'), (SELECT perm.id from permission perm where perm.code = 'ALL_FUNCTIONS'));

INSERT INTO appuser_role
(appuser_id, role_id)
VALUES((SELECT u.id from users u where u.email_address = 'admin@admin.com'), (SELECT rl.id from user_roles rl where rl.role_name = 'Super User'));