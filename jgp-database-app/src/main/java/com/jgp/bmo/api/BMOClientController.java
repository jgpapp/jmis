package com.jgp.bmo.api;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.jgp.bmo.dto.BMOClientDto;
import com.jgp.bmo.dto.BMOParticipantSearchCriteria;
import com.jgp.bmo.service.BMOClientDataService;
import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.data.ImportProgress;
import com.jgp.infrastructure.bulkimport.service.BulkImportWorkbookPopulatorService;
import com.jgp.infrastructure.bulkimport.service.BulkImportWorkbookService;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.shared.dto.ApiResponseDto;
import com.jgp.util.CommonUtil;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.concurrent.ExecutionException;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/bmos")
public class BMOClientController {

    private final BMOClientDataService bmoDataService;
    private final BulkImportWorkbookPopulatorService bulkImportWorkbookPopulatorService;
    private final BulkImportWorkbookService bulkImportWorkbookService;
    private final ImportProgressService importProgressService;

    @GetMapping
    public ResponseEntity<List<BMOClientDto>> getAvailableBMODataRecords(@RequestParam(name = "partnerId", required = false) Long partnerId,
                                                                         @RequestParam(name = "participantId", required = false) Long participantId,
                                                                        @RequestParam(name = "approvedByPartner", required = false) Boolean approvedByPartner,
                                                                        @RequestParam(name = "pageNumber", defaultValue = "1") Integer pageNumber,
                                                                          @RequestParam(name = "pageSize", defaultValue = "200") Integer pageSize){
        final var sortedByDateCreated =
                PageRequest.of(pageNumber - 1, pageSize, Sort.by("dateCreated").descending());
        return new ResponseEntity<>(this.bmoDataService.getBMODataRecords(new BMOParticipantSearchCriteria(partnerId, participantId, approvedByPartner), sortedByDateCreated), HttpStatus.OK);
    }

    @PostMapping("upload-template/{documentProgressId}")
    public ResponseEntity<ApiResponseDto> createBMOParticipantData(@RequestParam("excelFile") MultipartFile excelFile, @PathVariable("documentProgressId") String documentProgressId) {
        if (excelFile.isEmpty()) {
            return new ResponseEntity<>(new ApiResponseDto(false, CommonUtil.NO_FILE_TO_UPLOAD), HttpStatus.BAD_REQUEST);
        }
        return new ResponseEntity<>(new ApiResponseDto(true, this.bulkImportWorkbookService.importWorkbook(GlobalEntityType.TA_IMPORT_TEMPLATE.name(), excelFile, documentProgressId)+""), HttpStatus.CREATED);
    }

    @GetMapping("import-progress/{documentId}")
    public ResponseEntity<ImportProgress> getProgress(@PathVariable("documentId") String documentId) {
        ImportProgress progress = this.bulkImportWorkbookService.getImportProgress(documentId);
        if (progress == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok(progress);
    }

    @GetMapping("template/download")
    public ResponseEntity<?> downloadDataTemplate(HttpServletResponse response) {
        return bulkImportWorkbookPopulatorService.getTemplate(GlobalEntityType.TA_IMPORT_TEMPLATE.toString(), response);
    }

    @GetMapping("{bmoId}")
    public ResponseEntity<BMOClientDto> getBMOParticipantData(@PathVariable("bmoId") Long bmoId){
        return new ResponseEntity<>(this.bmoDataService.findBMODataById(bmoId), HttpStatus.OK);
    }

    @PostMapping("approve-or-reject")
    public ResponseEntity<ApiResponseDto> approvedBMOParticipantsData(@RequestBody List<Long> loanIds, @RequestParam(name = "approved") Boolean approved) {
        this.bmoDataService.approvedBMOParticipantsData(loanIds, approved);
        return new ResponseEntity<>(new ApiResponseDto(true, CommonUtil.RESOURCE_UPDATED), HttpStatus.OK);
    }

}
