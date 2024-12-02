
package com.jgp.infrastructure.documentmanagement.exception;

/**
 * A {@link RuntimeException} thrown when document management functionality is invoked for invalid Entity Types
 */
public class InvalidEntityTypeForDocumentManagementException extends RuntimeException {

    public InvalidEntityTypeForDocumentManagementException(final String entityType) {
        super("Document Management is not support for the Entity Type: " + entityType);
    }
}
