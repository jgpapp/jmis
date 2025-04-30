
package com.jgp.infrastructure.bulkimport.service;


import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.data.ImportData;
import com.jgp.infrastructure.bulkimport.data.ResourceType;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import jakarta.validation.constraints.NotNull;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.multipart.MultipartFile;

import java.util.Collection;

public interface BulkImportWorkbookService {

    Long importWorkbook(String entityType, MultipartFile fileDetail, String importProgressUUID, String updateParticipantInfo);

    void importFileToDirectory(String entityType, ResourceType resourceType, MultipartFile fileDetail);

    Collection<ImportData> getImports(GlobalEntityType type);

    Page<ImportData> getImports(@NotNull GlobalEntityType type, Long partnerId, Pageable pageable);

    Page<ImportData> getImportById(@NotNull Long importDocumentId);

    DocumentData getOutputTemplateLocation(String importDocumentId);

    ResponseEntity<?> getOutputTemplate(String importDocumentId, String fileType);

}
