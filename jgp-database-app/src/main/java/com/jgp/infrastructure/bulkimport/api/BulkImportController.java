package com.jgp.infrastructure.bulkimport.api;

import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.data.ImportData;
import com.jgp.infrastructure.bulkimport.exception.ImportTypeNotFoundException;
import com.jgp.infrastructure.bulkimport.service.BulkImportWorkbookService;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Locale;
import java.util.Objects;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/imports")
public class BulkImportController {

    private final BulkImportWorkbookService bulkImportWorkbookService;

    @GetMapping
    public ResponseEntity<Page<ImportData>> retrieveImportDocuments(@RequestParam("entityType") final String entityType,
                                                                    @RequestParam(name = "partnerId", required = false) Long partnerId,
                                                                    @RequestParam(name = "importDocumentId", required = false) Long importDocumentId,
                                                                    @RequestParam(name = "pageNumber", defaultValue = "1") Integer pageNumber,
                                                                    @RequestParam(name = "pageSize", defaultValue = "200") Integer pageSize) {

        final GlobalEntityType type = null == entityType ? null : GlobalEntityType.valueOf(entityType.toUpperCase(Locale.getDefault()));
        if (type == null) {
            throw new ImportTypeNotFoundException();
        }
        final var sortedByDateCreated =
                PageRequest.of(pageNumber, pageSize, Sort.by("dateCreated").descending());
        if (Objects.nonNull(importDocumentId)){
            return new ResponseEntity<>(this.bulkImportWorkbookService.getImportById(importDocumentId), HttpStatus.OK) ;
        }
        return new ResponseEntity<>(this.bulkImportWorkbookService.getImports(type, partnerId, sortedByDateCreated), HttpStatus.OK) ;

    }


    @GetMapping("getOutputTemplateLocation")
    public String retrieveOutputTemplateLocation(@RequestParam("importDocumentId") final String importDocumentId) {
        final DocumentData documentData = this.bulkImportWorkbookService.getOutputTemplateLocation(importDocumentId);
        return Objects.nonNull(documentData) ? documentData.getLocation() : "";
    }

    @GetMapping("downloadOutputTemplate")
    public ResponseEntity<?> getOutputTemplate(@RequestParam("importDocumentId") final String importDocumentId) {
        return bulkImportWorkbookService.getOutputTemplate(importDocumentId);
    }
}
