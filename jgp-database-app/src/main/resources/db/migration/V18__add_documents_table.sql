
CREATE TABLE jgp_document (
  id BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
  parent_entity_type varchar(50) NOT NULL,
  parent_entity_id INT NOT NULL DEFAULT '0',
  doc_name varchar(250) NOT NULL,
  file_name varchar(250) NOT NULL,
  doc_size INT DEFAULT '0',
  type varchar(50) DEFAULT NULL,
  description varchar(1000) DEFAULT NULL,
  location varchar(500) NOT NULL DEFAULT '0',
  date_created timestamp DEFAULT CURRENT_TIMESTAMP,
  last_modified timestamp NULL,
  PRIMARY KEY (`id`)
);


