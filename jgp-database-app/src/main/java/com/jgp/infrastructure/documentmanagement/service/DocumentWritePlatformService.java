
package com.jgp.infrastructure.documentmanagement.service;


import com.jgp.infrastructure.bulkimport.data.DocumentDto;
import com.jgp.infrastructure.documentmanagement.command.DocumentCommand;

import java.io.InputStream;

public interface DocumentWritePlatformService {

    Long createDocument(DocumentCommand documentCommand, InputStream inputStream);

    void updateDocument(DocumentCommand documentCommand, InputStream inputStream);

    void deleteDocument(DocumentCommand documentCommand);

    Long createInternalDocument(DocumentDto documentData);
}
