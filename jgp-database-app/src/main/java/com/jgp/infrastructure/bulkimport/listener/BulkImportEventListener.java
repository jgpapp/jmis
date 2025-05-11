package com.jgp.infrastructure.bulkimport.listener;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.jgp.authentication.domain.AppUser;
import com.jgp.authentication.service.UserService;
import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.domain.ImportDocument;
import com.jgp.infrastructure.bulkimport.domain.ImportDocumentRepository;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.event.DataUploadedEvent;
import com.jgp.infrastructure.bulkimport.importhandler.ImportHandler;
import com.jgp.infrastructure.bulkimport.importhandler.LoanImportHandler;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.infrastructure.documentmanagement.command.DocumentCommand;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.infrastructure.documentmanagement.service.DocumentWritePlatformService;
import com.jgp.notification.service.EmailService;
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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutionException;

@Service
@RequiredArgsConstructor
@Slf4j
public class BulkImportEventListener {

    private final ApplicationContext applicationContext;
    private final ImportDocumentRepository importRepository;
    private final DocumentWritePlatformService documentService;
    private final ImportProgressService importProgressService;
    private final UserService userService;

    @Value("${jgp.notification.enabled}")
    private boolean notificationEnabled;

    @EventListener
   public void handleBulkImportEvent(BulkImportEvent bulkImportEvent){
        try {
            this.importProgressService.sendProgressUpdate(bulkImportEvent.importProgressUUID());
        } catch (JsonProcessingException | ExecutionException e) {
            log.error("Problem Updating Progress: {}", e.getMessage());
        }
        final ImportDocument importDocument = this.importRepository.findById(bulkImportEvent.importId()).orElseThrow();
        final GlobalEntityType entityType = GlobalEntityType.fromInt(importDocument.getEntityType());
        ImportHandler importHandler = switch (entityType) {
            case GlobalEntityType.TA_IMPORT_TEMPLATE -> this.applicationContext.getBean("BMOImportHandler", ImportHandler.class);
            case GlobalEntityType.LOAN_IMPORT_TEMPLATE -> this.applicationContext.getBean("loanImportHandler", LoanImportHandler.class);
            default -> throw new IllegalArgumentException("Unable to find requested resource");
        };


        final var count = importHandler.process(bulkImportEvent);
        final var successfullyUploadedCount = count.getSuccessCount();
        final Workbook workbook = bulkImportEvent.workbook();
        importDocument.update(LocalDateTime.now(ZoneId.systemDefault()), count.getTotalCount(), successfullyUploadedCount, count.getErrorCount());
        this.importRepository.saveAndFlush(importDocument);

        if (successfullyUploadedCount > 0){
            notifyDataReviewers(entityType);
        }
        final Set<String> modifiedParams = new HashSet<>();
        modifiedParams.add("fileName");
        modifiedParams.add("size");
        modifiedParams.add("type");
        modifiedParams.add("location");
        Document document = importDocument.getDocument();

        DocumentCommand documentCommand = new DocumentCommand(modifiedParams, document.getId(), entityType.name(), null, document.getName(),
                document.getFileName(), document.getSize(), URLConnection.guessContentTypeFromName(document.getFileName()), null, null);

        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try {
            try {
                workbook.write(bos);
            } finally {
                bos.close();
            }
        } catch (IOException io) {
            log.error("Problem occurred in onApplicationEvent function", io);
        }
        byte[] bytes = bos.toByteArray();
        ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
        this.documentService.updateDocument(documentCommand, bis);
    }


    private void notifyDataReviewers(final GlobalEntityType entityType){
        if (!notificationEnabled) {
            return;
        }
        final var currentUser = this.userService.currentUser();
        if (null == currentUser){
            return;
        }
        final var currentUserPartner = currentUser.getPartner();
        if (null == currentUserPartner){
            return;
        }
        if (null == entityType){
            return;
        }
        this.applicationContext.publishEvent(new DataUploadedEvent(currentUserPartner.getId(), entityType));
    }
}
