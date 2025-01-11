
package com.jgp.infrastructure.bulkimport.service;


import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.data.ImportData;
import com.jgp.infrastructure.bulkimport.data.ImportProgress;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import jakarta.validation.constraints.NotNull;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.Collection;

public interface BulkImportWorkbookService {

    Long importWorkbook(String entityType, MultipartFile fileDetail, String importProgressUUID);

    ImportProgress getImportProgress(@NotNull String importDocumentId);

    Collection<ImportData> getImports(GlobalEntityType type);

    Page<ImportData> getImports(@NotNull GlobalEntityType type, @NotNull Long partnerId, Pageable pageable);

    Page<ImportData> getImportById(@NotNull Long importDocumentId);

    DocumentData getOutputTemplateLocation(String importDocumentId);

    ResponseEntity<?> getOutputTemplate(String importDocumentId);

}
