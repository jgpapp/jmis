
package com.jgp.infrastructure.documentmanagement.exception;


public class ContentManagementException extends RuntimeException {

    public ContentManagementException(final String filename, final String message) {
        super("Error while manipulating file " + filename + " due to a ContentRepository issue " + message);
    }

    public ContentManagementException(final String name, final Long fileSize, final int maxFileSize) {
        super("Unable to save the document with name" + name + " since its file Size of "
                + fileSize / (1024 * 1024) + " MB exceeds the max permissable file size  of " + maxFileSize + " MB");
    }

    public ContentManagementException(String filename, String message, Exception exception) {
        super("Error while manipulating file " + filename + " due to a ContentRepository issue " + message, exception);
    }
}
