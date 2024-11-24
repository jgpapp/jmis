
package com.jgp.infrastructure.documentmanagement.service;


import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import com.jgp.infrastructure.documentmanagement.data.FileData;

import java.util.Collection;

public interface DocumentReadPlatformService {

    Collection<DocumentData> retrieveAllDocuments(String entityType, Long entityId);

    FileData retrieveFileData(String entityType, Long entityId, Long documentId);

    DocumentData retrieveDocument(String entityType, Long entityId, Long documentId);

    DocumentData retrieveTopDocument(String entityType, Long entityId);

}
