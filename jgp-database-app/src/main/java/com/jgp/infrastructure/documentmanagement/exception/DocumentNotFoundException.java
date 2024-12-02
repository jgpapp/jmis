
package com.jgp.infrastructure.documentmanagement.exception;

import org.springframework.dao.EmptyResultDataAccessException;

public class DocumentNotFoundException extends RuntimeException {

    public DocumentNotFoundException(final String entityType, final Long entityId, final Long id) {
        super("Document with identifier " + id + " does not exist for the " + entityType + " with Identifier " + entityId);
    }

    public DocumentNotFoundException(String entityType, Long entityId, Long id, EmptyResultDataAccessException e) {
        super("Document with identifier " + id + " does not exist for the " + entityType + " with Identifier " + entityId, e);
    }

    public DocumentNotFoundException(String entityType, Long entityId, EmptyResultDataAccessException e) {
        super("Document with identifier does not exist for the " + entityType + " with Identifier " + entityId, e);
    }

}
