package com.jgp.infrastructure.bulkimport.api;

import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.data.ImportData;
import com.jgp.infrastructure.bulkimport.data.ImportRequestDto;
import com.jgp.infrastructure.bulkimport.data.ResourceType;
import com.jgp.infrastructure.bulkimport.exception.ImportTypeNotFoundException;
import com.jgp.infrastructure.bulkimport.service.BulkImportWorkbookPopulatorService;
import com.jgp.infrastructure.bulkimport.service.BulkImportWorkbookService;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import com.jgp.shared.dto.ApiResponseDto;
import com.jgp.util.CommonUtil;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.util.Locale;
import java.util.Objects;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/imports")
public class BulkImportController {

    private final BulkImportWorkbookService bulkImportWorkbookService;
    private final BulkImportWorkbookPopulatorService bulkImportWorkbookPopulatorService;


    @GetMapping("template/download/{entityType}")
    public ResponseEntity<?> downloadDataTemplate(HttpServletResponse response, @PathVariable("entityType") String entityType) {
        return this.bulkImportWorkbookPopulatorService.getTemplate(entityType, response);
    }

    @PostMapping("upload-template/{entityType}/{documentProgressId}/{updateParticipantInfo}")
    public ResponseEntity<ApiResponseDto> createBMOParticipantData(
            @RequestParam("excelFile") MultipartFile excelFile,
            @RequestParam("appDomain") String appDomain,
            @PathVariable("entityType") String entityType,
            @PathVariable("documentProgressId") String documentProgressId,
            @PathVariable("updateParticipantInfo") String updateParticipantInfo) {
        if (excelFile.isEmpty() || "INVALID".equalsIgnoreCase(entityType)) {
            return new ResponseEntity<>(new ApiResponseDto(false, CommonUtil.NO_FILE_TO_UPLOAD), HttpStatus.BAD_REQUEST);
        }
        return new ResponseEntity<>(new ApiResponseDto(true, this.bulkImportWorkbookService.importWorkbook(new ImportRequestDto(entityType, excelFile, documentProgressId, updateParticipantInfo, appDomain))+""), HttpStatus.CREATED);
    }

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

    @GetMapping("downloadOutputTemplate/{file-type}")
    public ResponseEntity<?> getOutputTemplate(@RequestParam("importDocumentId") final String importDocumentId, @PathVariable("file-type") String fileType) {
        return bulkImportWorkbookService.getOutputTemplate(importDocumentId, fileType);
    }

    @GetMapping("downloadFile/{file-document-id}")
    public ResponseEntity<?> downloadFile(@PathVariable("file-document-id") String importDocumentId) {
        return bulkImportWorkbookService.downloadFile(importDocumentId);
    }

    @PostMapping("upload-resource-file/{resource-type}")
    public ResponseEntity<ApiResponseDto> uploadResourceFile(@RequestParam("fileDetail") MultipartFile fileDetail, @PathVariable("resource-type") ResourceType resourceType) {
        if (fileDetail.isEmpty()) {
            return new ResponseEntity<>(new ApiResponseDto(false, CommonUtil.NO_FILE_TO_UPLOAD), HttpStatus.BAD_REQUEST);
        }
        bulkImportWorkbookService.importFileToDirectory(GlobalEntityType.RESOURCES_IMPORT, resourceType, fileDetail);

        return new ResponseEntity<>(new ApiResponseDto(true, "Document successfully uploaded !!"), HttpStatus.CREATED);
    }

    @DeleteMapping("delete-resource-file/{import-id}")
    public ResponseEntity<ApiResponseDto> deleteResourceFile( @PathVariable("import-id") Long importId, @RequestParam(name = "delete-associated-data") Boolean deleteAssociatedData) {
        bulkImportWorkbookService.deleteFileFromDbAndDirectory(importId, deleteAssociatedData);
        return new ResponseEntity<>(new ApiResponseDto(true, "Document successfully deleted !!"), HttpStatus.NO_CONTENT);
    }
}
