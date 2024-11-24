/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package com.jgp.infrastructure.documentmanagement.service;

import org.apache.fineract.infrastructure.core.data.CommandProcessingResult;
import org.apache.fineract.infrastructure.core.domain.Base64EncodedFile;
import org.apache.fineract.infrastructure.core.exception.PlatformDataIntegrityException;
import org.apache.fineract.infrastructure.documentmanagement.command.DocumentCommand;
import org.apache.fineract.infrastructure.documentmanagement.command.DocumentCommandValidator;
import org.apache.fineract.infrastructure.documentmanagement.contentrepository.ContentRepository;
import org.apache.fineract.infrastructure.documentmanagement.contentrepository.ContentRepositoryFactory;
import org.apache.fineract.infrastructure.documentmanagement.domain.Document;
import org.apache.fineract.infrastructure.documentmanagement.domain.DocumentRepository;
import org.apache.fineract.infrastructure.documentmanagement.domain.StorageType;
import org.apache.fineract.infrastructure.documentmanagement.exception.ContentManagementException;
import org.apache.fineract.infrastructure.documentmanagement.exception.DocumentNotFoundException;
import org.apache.fineract.infrastructure.documentmanagement.exception.InvalidEntityTypeForDocumentManagementException;
import org.apache.fineract.infrastructure.security.service.PlatformSecurityContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.orm.jpa.JpaSystemException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.InputStream;

@Service
public class DocumentWritePlatformServiceJpaRepositoryImpl implements DocumentWritePlatformService {

    private static final Logger LOG = LoggerFactory.getLogger(DocumentWritePlatformServiceJpaRepositoryImpl.class);

    private final PlatformSecurityContext context;
    private final DocumentRepository documentRepository;
    private final ContentRepositoryFactory contentRepositoryFactory;

    private static final String DOCUMENT_ERROR_MESSAGE_CODE = "error.msg.document.unknown.data.integrity.issue";
    private static final String DOCUMENT_ERROR_MESSAGE = "Unknown data integrity issue with resource.";

    @Autowired
    public DocumentWritePlatformServiceJpaRepositoryImpl(final PlatformSecurityContext context, final DocumentRepository documentRepository,
            final ContentRepositoryFactory documentStoreFactory) {
        this.context = context;
        this.documentRepository = documentRepository;
        this.contentRepositoryFactory = documentStoreFactory;
    }

    @Transactional
    @Override
    public Long createDocument(final DocumentCommand documentCommand, final InputStream inputStream) {
        try {
            this.context.authenticatedUser();

            final DocumentCommandValidator validator = new DocumentCommandValidator(documentCommand);

            validateParentEntityType(documentCommand);

            validator.validateForCreate();

            final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();

            final String fileLocation = contentRepository.saveFile(inputStream, documentCommand);

            final Document document = Document.createNew(documentCommand.getParentEntityType(), documentCommand.getParentEntityId(),
                    documentCommand.getName(), documentCommand.getFileName(), documentCommand.getSize(), documentCommand.getType(),
                    documentCommand.getDescription(), fileLocation, contentRepository.getStorageType());

            this.documentRepository.saveAndFlush(document);

            return document.getId();
        } catch (final JpaSystemException | DataIntegrityViolationException dve) {
            LOG.error("Error occured.", dve);
            throw new PlatformDataIntegrityException(DOCUMENT_ERROR_MESSAGE_CODE, DOCUMENT_ERROR_MESSAGE, dve);
        }
    }

    @Transactional
    @Override
    public Long createInternalDocument(final String entityType, final Long entityId, final Long fileSize, final InputStream inputStream,
            final String mimeType, final String name, final String description, final String fileName) {

        final DocumentCommand documentCommand = new DocumentCommand(null, null, entityType, entityId, name, fileName, fileSize, mimeType,
                description, null);

        final Long documentId = createDocument(documentCommand, inputStream);

        return documentId;

    }

    @Override
    public Long createDocument(Base64EncodedFile base64EncodedFile, String entityType, Long entityId, String name, String fileName,
            String docType) {
        checkValidityOfEntityType(entityType);
        try {
            this.context.authenticatedUser();

            final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();

            final String fileLocation = contentRepository.saveFile(base64EncodedFile, entityId, fileName, entityType);

            final Document document = Document.createNew(entityType, entityId, name, fileName, base64EncodedFile.getSize(), docType,
                    fileName, fileLocation, contentRepository.getStorageType());

            this.documentRepository.save(document);

            return document.getId();
        } catch (final JpaSystemException | DataIntegrityViolationException dve) {
            LOG.error("Error occured.", dve);
            throw new PlatformDataIntegrityException(DOCUMENT_ERROR_MESSAGE_CODE, DOCUMENT_ERROR_MESSAGE, dve);
        }
    }

    @Override
    @Transactional
    public Long updateDocument(Base64EncodedFile base64EncodedFile, DocumentCommand documentCommand) {
        CommandProcessingResult updatedDocument = updateDocument(documentCommand, null, base64EncodedFile);
        if (updatedDocument.resourceId() != null) {
            return updatedDocument.resourceId();
        }
        return null;
    }

    @Override
    @Transactional
    public CommandProcessingResult updateDocument(final DocumentCommand documentCommand, final InputStream inputStream) {
        return updateDocument(documentCommand, inputStream, null);
    }

    public CommandProcessingResult updateDocument(final DocumentCommand documentCommand, final InputStream inputStream,
            final Base64EncodedFile base64EncodedFile) {
        try {
            this.context.authenticatedUser();

            String oldLocation = null;
            final DocumentCommandValidator validator = new DocumentCommandValidator(documentCommand);
            validator.validateForUpdate();
            // TODO check if entity id is valid and within data scope for the
            // user
            final Document documentForUpdate = this.documentRepository.findById(documentCommand.getId())
                    .orElseThrow(() -> new DocumentNotFoundException(documentCommand.getParentEntityType(),
                            documentCommand.getParentEntityId(), documentCommand.getId()));

            final StorageType documentStoreType = documentForUpdate.storageType();
            oldLocation = documentForUpdate.getLocation();
            if (inputStream != null && documentCommand.isFileNameChanged()) {
                final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();
                documentCommand.setLocation(contentRepository.saveFile(inputStream, documentCommand));
                documentCommand.setStorageType(contentRepository.getStorageType().getValue());
            } else if (base64EncodedFile != null && documentCommand.isFileNameChanged()) {
                final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();
                documentCommand.setLocation(contentRepository.saveFile(base64EncodedFile, documentCommand.getParentEntityId(),
                        documentCommand.getFileName(), documentCommand.getParentEntityType()));
                documentCommand.setStorageType(contentRepository.getStorageType().getValue());
            }

            documentForUpdate.update(documentCommand);

            if ((inputStream != null || base64EncodedFile != null) && documentCommand.isFileNameChanged()) {
                final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository(documentStoreType);
                contentRepository.deleteFile(oldLocation);
            }

            this.documentRepository.saveAndFlush(documentForUpdate);

            return new CommandProcessingResult(documentForUpdate.getId());
        } catch (final JpaSystemException | DataIntegrityViolationException dve) {
            LOG.error("Error occured.", dve);
            throw new PlatformDataIntegrityException(DOCUMENT_ERROR_MESSAGE_CODE, DOCUMENT_ERROR_MESSAGE, dve);
        } catch (final ContentManagementException cme) {
            LOG.error("Error occured.", cme);
            throw new ContentManagementException(documentCommand.getName(), cme.getMessage(), cme);
        }
    }

    @Transactional
    @Override
    public CommandProcessingResult deleteDocument(final DocumentCommand documentCommand) {
        this.context.authenticatedUser();

        validateParentEntityType(documentCommand);
        // TODO: Check document is present under this entity Id
        final Document document = this.documentRepository.findById(documentCommand.getId())
                .orElseThrow(() -> new DocumentNotFoundException(documentCommand.getParentEntityType(), documentCommand.getParentEntityId(),
                        documentCommand.getId()));
        this.documentRepository.delete(document);

        final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository(document.storageType());
        contentRepository.deleteFile(document.getLocation());
        return new CommandProcessingResult(document.getId());
    }

    private void validateParentEntityType(final DocumentCommand documentCommand) {
        checkValidityOfEntityType(documentCommand.getParentEntityType());
    }

    private void checkValidityOfEntityType(String entityType) {
        if (!checkValidEntityType(entityType)) {
            throw new InvalidEntityTypeForDocumentManagementException(entityType);
        }
    }

    private static boolean checkValidEntityType(final String entityType) {
        for (final DocumentManagementEntity entities : DocumentManagementEntity.values()) {
            if (entities.name().equalsIgnoreCase(entityType)) {
                return true;
            }
        }
        return false;
    }

    /*** Entities for document Management **/
    public enum DocumentManagementEntity {

        CLIENTS, CLIENT_IDENTIFIERS, STAFF, LOANS, SAVINGS, GROUPS, IMPORT;

        @Override
        public String toString() {
            return name().toString().toLowerCase();
        }
    }
}
