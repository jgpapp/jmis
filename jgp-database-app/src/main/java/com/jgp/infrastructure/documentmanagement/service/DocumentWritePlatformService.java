
package com.jgp.infrastructure.documentmanagement.service;


import com.jgp.infrastructure.core.domain.Base64EncodedFile;
import com.jgp.infrastructure.documentmanagement.command.DocumentCommand;

import java.io.InputStream;

public interface DocumentWritePlatformService {

    Long createDocument(DocumentCommand documentCommand, InputStream inputStream);

    void updateDocument(DocumentCommand documentCommand, InputStream inputStream);

    void deleteDocument(DocumentCommand documentCommand);

    Long createInternalDocument(String entityType, Long entityId, Long fileSize, InputStream inputStream, String mimeType, String name,
            String description, String fileName);

    Long createDocument(Base64EncodedFile base64EncodedFile, String entityType, Long entityId, String name, String fileName,
            String docType);

    Long updateDocument(Base64EncodedFile base64EncodedFile, DocumentCommand documentCommand);
}
