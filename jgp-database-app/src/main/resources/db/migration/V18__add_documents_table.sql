
CREATE TABLE jgp_document (
  id BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
  parent_entity_type varchar(200) NOT NULL,
  parent_entity_id INT NOT NULL DEFAULT '0',
  doc_name varchar(250) NOT NULL,
  file_name varchar(250) NOT NULL,
  doc_size INT DEFAULT '0',
  type varchar(200) DEFAULT NULL,
  description varchar(1000) DEFAULT NULL,
  location varchar(500) NOT NULL DEFAULT '0',
  date_created timestamp DEFAULT CURRENT_TIMESTAMP,
  last_modified timestamp NULL,
  created_by_id BIGINT NULL,
  last_modified_by_id BIGINT NULL,
  PRIMARY KEY (id),
  CONSTRAINT M_DOCUMENT_ON_MODIFIED_BY FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
  CONSTRAINT M_DOCUMENT_ON_CREATED_BY FOREIGN KEY (created_by_id) REFERENCES users(id)
);

CREATE TABLE import_document (
  id BIGINT GENERATED ALWAYS AS IDENTITY     NOT NULL,
  document_id BIGINT NULL,
  import_time timestamp NULL,
  end_time timestamp NULL,
  completed BOOLEAN DEFAULT false,
  entity_type INT NULL,
  total_records INT NULL,
  success_count INT NULL,
  failure_count INT NULL,
  date_created timestamp DEFAULT CURRENT_TIMESTAMP,
  last_modified timestamp NULL,
  created_by_id BIGINT NULL,
  last_modified_by_id BIGINT NULL,
  PRIMARY KEY (id),
  CONSTRAINT IMPORT_DOCUMENT_ON_MODIFIED_BY FOREIGN KEY (last_modified_by_id) REFERENCES users(id),
  CONSTRAINT IMPORT_DOCUMENT_ON_CREATED_BY FOREIGN KEY (created_by_id) REFERENCES users(id),
  CONSTRAINT IMPORT_DOCUMENT_ON_DOCUMENT FOREIGN KEY (document_id) REFERENCES jgp_document(id)
);


