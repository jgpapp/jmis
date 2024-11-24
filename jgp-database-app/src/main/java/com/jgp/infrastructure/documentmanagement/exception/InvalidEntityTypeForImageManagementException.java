
package com.jgp.infrastructure.documentmanagement.exception;


/**
 * Runtime exception for invalid image types
 */
public class InvalidEntityTypeForImageManagementException extends RuntimeException {

    public InvalidEntityTypeForImageManagementException(String imageType) {
        super("Image Management is not support for the Entity Type: " + imageType);
    }
}
