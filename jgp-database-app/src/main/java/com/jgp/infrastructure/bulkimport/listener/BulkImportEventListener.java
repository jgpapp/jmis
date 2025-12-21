package com.jgp.infrastructure.bulkimport.listener;

import com.jgp.authentication.service.UserService;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.domain.ImportDocument;
import com.jgp.infrastructure.bulkimport.domain.ImportDocumentRepository;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.event.DataUploadedEvent;
import com.jgp.infrastructure.bulkimport.exception.DataImportException;
import com.jgp.infrastructure.bulkimport.importhandler.ImportHandler;
import com.jgp.infrastructure.bulkimport.importhandler.LoanImportHandler;
import com.jgp.infrastructure.bulkimport.importhandler.MentorshipImportHandler;
import com.jgp.infrastructure.bulkimport.importhandler.MonitoringImportHandler;
import com.jgp.infrastructure.bulkimport.importhandler.TAImportHandler;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.infrastructure.documentmanagement.command.DocumentCommand;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.infrastructure.documentmanagement.service.DocumentWritePlatformService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URLConnection;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Slf4j
public class BulkImportEventListener {

    private static final String TA_IMPORT_HANDLER_BEAN = "TAImportHandler";
    private static final String LOAN_IMPORT_HANDLER_BEAN = "loanImportHandler";
    private static final String MENTORSHIP_IMPORT_HANDLER_BEAN = "mentorshipImportHandler";
    private static final String MONITORING_IMPORT_HANDLER_BEAN = "monitoringImportHandler";
    private static final String UNSUPPORTED_ENTITY_TYPE_ERROR = "Unsupported entity type: ";

    private final ApplicationContext applicationContext;
    private final ImportDocumentRepository importRepository;
    private final DocumentWritePlatformService documentService;
    private final ImportProgressService importProgressService;
    private final UserService userService;

    @Value("${jgp.notification.enabled}")
    private boolean notificationEnabled;

    @EventListener
   public void handleBulkImportEvent(BulkImportEvent bulkImportEvent) {
        Objects.requireNonNull(bulkImportEvent, "BulkImportEvent cannot be null");
        try {
            this.importProgressService.sendProgressUpdate(bulkImportEvent.importProgressUUID());
            final ImportDocument importDocument = fetchImportDocument(bulkImportEvent.importId());
            final GlobalEntityType entityType = GlobalEntityType.fromInt(importDocument.getEntityType());
            final ImportHandler importHandler = resolveImportHandler(entityType);

            final var importCount = processImport(importHandler, bulkImportEvent);
            updateImportDocument(importDocument, importCount);

            if (importCount.getSuccessCount() > 0) {
                notifyDataReviewers(entityType, bulkImportEvent.appDomainForNotification());
            }

            updateDocumentWithWorkbook(importDocument, bulkImportEvent.workbook(), entityType);

        } catch (Exception exception) {
            log.error("Error processing bulk import event for import ID: {}",
                    bulkImportEvent.importId(), exception);
            throw new DataImportException("Failed to process bulk import", exception);
        }finally {
            importProgressService.updateStepAndSendProgress(bulkImportEvent.importProgressUUID(), TemplatePopulateImportConstants.EXCEL_UPLOAD_COMPLETED_STEP);
        }
    }

    /**
     * Fetches the ImportDocument by its ID, ensuring it is not marked as deleted.
     *
     * @param importId the ID of the import document
     * @return the corresponding ImportDocument
     * @throws IllegalArgumentException if the import document is not found or is deleted
     */
    private ImportDocument fetchImportDocument(Long importId) {
        return this.importRepository.findById(importId)
                .filter(doc -> Boolean.FALSE.equals(doc.getIsDeleted()))
                .orElseThrow(() -> new IllegalArgumentException("Import document not found: " + importId));
    }

    /**
     * Resolves the appropriate ImportHandler based on the provided GlobalEntityType.
     *
     * @param entityType the type of entity for which the import handler is needed
     * @return the corresponding ImportHandler
     * @throws IllegalArgumentException if the entity type is unsupported
     */
    private ImportHandler resolveImportHandler(GlobalEntityType entityType) {
        return switch (entityType) {
            case TA_IMPORT_TEMPLATE ->
                    this.applicationContext.getBean(TA_IMPORT_HANDLER_BEAN, ImportHandler.class);
            case LOAN_IMPORT_TEMPLATE ->
                    this.applicationContext.getBean(LOAN_IMPORT_HANDLER_BEAN, ImportHandler.class);
            case MENTORSHIP_IMPORT_TEMPLATE ->
                    this.applicationContext.getBean(MENTORSHIP_IMPORT_HANDLER_BEAN, ImportHandler.class);
            case MONITORING_IMPORT_TEMPLATE ->
                    this.applicationContext.getBean(MONITORING_IMPORT_HANDLER_BEAN, ImportHandler.class);
            default -> throw new IllegalArgumentException(UNSUPPORTED_ENTITY_TYPE_ERROR + entityType);
        };
    }

    /**
     * Processes the import using the specified ImportHandler and BulkImportEvent.
     *
     * @param importHandler    the handler responsible for processing the import
     * @param bulkImportEvent  the event containing import details
     * @return the Count object containing import results
     * @throws DataImportException if the import processing fails
     */
    private Count processImport(ImportHandler importHandler, BulkImportEvent bulkImportEvent) {
        try {
            final CompletableFuture<Count> countFuture = importHandler.process(bulkImportEvent);
            return countFuture.get();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new DataImportException("Import processing was interrupted", e);
        } catch (Exception e) {
            log.error("Failed to process import", e);
            throw new DataImportException("Import processing failed", e);
        }
    }

    /**
     * Updates the ImportDocument with the results of the import process.
     *
     * @param importDocument the document to be updated
     * @param count          the Count object containing import results
     */
    private void updateImportDocument(ImportDocument importDocument, Count count) {
        final LocalDateTime completedTime = LocalDateTime.now(ZoneId.systemDefault());
        importDocument.update(completedTime, count.getTotalCount(),
                count.getSuccessCount(), count.getErrorCount());
        this.importRepository.saveAndFlush(importDocument);
    }

    /**
     * Updates the document associated with the ImportDocument using the provided Workbook.
     *
     * @param importDocument the import document containing the document to be updated
     * @param workbook       the workbook containing updated data
     * @param entityType     the type of entity for the document
     */
    private void updateDocumentWithWorkbook(ImportDocument importDocument, Workbook workbook,
                                            GlobalEntityType entityType) {
        final Document document = importDocument.getDocument();
        final byte[] workbookBytes = convertWorkbookToBytes(workbook);

        final DocumentCommand documentCommand = buildDocumentCommand(document, entityType);

        try (ByteArrayInputStream inputStream = new ByteArrayInputStream(workbookBytes)) {
            this.documentService.updateDocument(documentCommand, inputStream);
        } catch (IOException exception) {
            log.error("Failed to update document: {}", document.getId(), exception);
            throw new DataImportException("Document update failed", exception);
        }
    }

    /**
     * Converts the given Workbook to a byte array.
     *
     * @param workbook the workbook to convert
     * @return a byte array representation of the workbook
     * @throws DataImportException if the conversion fails
     */
    private byte[] convertWorkbookToBytes(Workbook workbook) {
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            workbook.write(outputStream);
            return outputStream.toByteArray();
        } catch (IOException exception) {
            log.error("Failed to convert workbook to bytes", exception);
            throw new DataImportException("Workbook conversion failed", exception);
        }
    }

    /**
     * Builds a DocumentCommand for updating the document.
     *
     * @param document   the document to be updated
     * @param entityType the type of entity for the document
     * @return the constructed DocumentCommand
     */
    private DocumentCommand buildDocumentCommand(Document document, GlobalEntityType entityType) {
        final Set<String> modifiedParams = Set.of("fileName", "size", "type", "location");
        final String contentType = URLConnection.guessContentTypeFromName(document.getFileName());

        return new DocumentCommand(
                modifiedParams,
                document.getId(),
                entityType.name(),
                null,
                document.getName(),
                document.getFileName(),
                document.getSize(),
                contentType,
                null,
                null,
                entityType
        );
    }

    /**
     * Notifies data reviewers about the uploaded data.
     *
     * @param entityType the type of entity that was uploaded
     * @param appDomain  the application domain for notification
     */
    private void notifyDataReviewers(GlobalEntityType entityType, String appDomain) {
        if (!notificationEnabled) {
            return;
        }

        final var currentUser = this.userService.currentUser();
        if (Objects.isNull(currentUser) || Objects.isNull(currentUser.getPartner())
                || Objects.isNull(entityType)) {
            log.debug("Skipping notification - missing required data");
            return;
        }

        final Long partnerId = currentUser.getPartner().getId();
        this.applicationContext.publishEvent(new DataUploadedEvent(partnerId, entityType, appDomain));
    }
}
