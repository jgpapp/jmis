
package com.jgp.infrastructure.documentmanagement.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.infrastructure.core.domain.Base64EncodedFile;
import com.jgp.infrastructure.documentmanagement.command.DocumentCommand;
import com.jgp.infrastructure.documentmanagement.contentrepository.ContentRepository;
import com.jgp.infrastructure.documentmanagement.contentrepository.ContentRepositoryFactory;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.infrastructure.documentmanagement.domain.DocumentRepository;
import com.jgp.infrastructure.documentmanagement.exception.ContentManagementException;
import com.jgp.infrastructure.documentmanagement.exception.DocumentNotFoundException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.orm.jpa.JpaSystemException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.InputStream;

@Slf4j
@Service
public class DocumentWritePlatformServiceJpaRepositoryImpl implements DocumentWritePlatformService {

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
            final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();

            final String fileLocation = contentRepository.saveFile(inputStream, documentCommand);

            final Document document = Document.createNew(documentCommand.getParentEntityType(), documentCommand.getParentEntityId(),
                    documentCommand.getName(), documentCommand.getFileName(), documentCommand.getSize(), documentCommand.getType(),
                    documentCommand.getDescription(), fileLocation);

            this.documentRepository.saveAndFlush(document);

            return document.getId();
        } catch (final JpaSystemException | DataIntegrityViolationException dve) {
            log.error("Error occured.", dve);
            throw new ContentManagementException(DOCUMENT_ERROR_MESSAGE_CODE, DOCUMENT_ERROR_MESSAGE, dve);
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

            final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();

            final String fileLocation = contentRepository.saveFile(base64EncodedFile, entityId, fileName, entityType);

            final Document document = Document.createNew(entityType, entityId, name, fileName, base64EncodedFile.size(), docType,
                    fileName, fileLocation);

            this.documentRepository.save(document);

            return document.getId();
        } catch (final JpaSystemException | DataIntegrityViolationException dve) {
            log.error("Error occured.", dve);
            throw new ContentManagementException(DOCUMENT_ERROR_MESSAGE_CODE, DOCUMENT_ERROR_MESSAGE, dve);
        }
    }

    @Override
    @Transactional
    public Long updateDocument(Base64EncodedFile base64EncodedFile, DocumentCommand documentCommand) {
        final var updatedDocument = updateDocument(documentCommand, null, base64EncodedFile);
        if (updatedDocument.getId() != null) {
            return updatedDocument.getId();
        }
        return null;
    }

    @Override
    @Transactional
    public void updateDocument(final DocumentCommand documentCommand, final InputStream inputStream) {
        this.updateDocument(documentCommand, inputStream, null);
    }

    public Document updateDocument(final DocumentCommand documentCommand, final InputStream inputStream,
            final Base64EncodedFile base64EncodedFile) {
        try {

            String oldLocation = null;
            // TODO check if entity id is valid and within data scope for the
            // user
            final Document documentForUpdate = this.documentRepository.findById(documentCommand.getId())
                    .orElseThrow(() -> new DocumentNotFoundException(documentCommand.getParentEntityType(),
                            documentCommand.getParentEntityId(), documentCommand.getId()));

            oldLocation = documentForUpdate.getLocation();
            if (inputStream != null && documentCommand.isFileNameChanged()) {
                final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();
                documentCommand.setLocation(contentRepository.saveFile(inputStream, documentCommand));
            } else if (base64EncodedFile != null && documentCommand.isFileNameChanged()) {
                final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();
                documentCommand.setLocation(contentRepository.saveFile(base64EncodedFile, documentCommand.getParentEntityId(),
                        documentCommand.getFileName(), documentCommand.getParentEntityType()));
            }

            documentForUpdate.update(documentCommand);

            if ((inputStream != null || base64EncodedFile != null) && documentCommand.isFileNameChanged()) {
                final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();
                contentRepository.deleteFile(oldLocation);
            }

            this.documentRepository.saveAndFlush(documentForUpdate);

            return documentForUpdate;
        } catch (final JpaSystemException | DataIntegrityViolationException dve) {
            log.error("Error occured.", dve);
            throw new ContentManagementException(DOCUMENT_ERROR_MESSAGE_CODE, DOCUMENT_ERROR_MESSAGE, dve);
        } catch (final ContentManagementException cme) {
            log.error("Error occured.", cme);
            throw new ContentManagementException(documentCommand.getName(), cme.getMessage(), cme);
        }
    }

    @Transactional
    @Override
    public void deleteDocument(final DocumentCommand documentCommand) {

        validateParentEntityType(documentCommand);
        // TODO: Check document is present under this entity Id
        final Document document = this.documentRepository.findById(documentCommand.getId())
                .orElseThrow(() -> new DocumentNotFoundException(documentCommand.getParentEntityType(), documentCommand.getParentEntityId(),
                        documentCommand.getId()));
        this.documentRepository.delete(document);

        final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();
        contentRepository.deleteFile(document.getLocation());
    }

    private void validateParentEntityType(final DocumentCommand documentCommand) {
        checkValidityOfEntityType(documentCommand.getParentEntityType());
    }

    private void checkValidityOfEntityType(String entityType) {
        if (!checkValidEntityType(entityType)) {
            throw new ContentManagementException(entityType);
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
            return name().toLowerCase();
        }
    }
}
