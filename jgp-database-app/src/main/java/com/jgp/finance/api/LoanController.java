package com.jgp.finance.api;

import com.jgp.finance.domain.Loan;
import com.jgp.finance.dto.LoanDto;
import com.jgp.finance.dto.LoanSearchCriteria;
import com.jgp.finance.service.LoanService;
import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.service.BulkImportWorkbookPopulatorService;
import com.jgp.infrastructure.bulkimport.service.BulkImportWorkbookService;
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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/loans")
public class LoanController {

    private final LoanService loanService;
    private final BulkImportWorkbookPopulatorService bulkImportWorkbookPopulatorService;
    private final BulkImportWorkbookService bulkImportWorkbookService;

    @GetMapping
    public ResponseEntity<Page<LoanDto>> getAvailableLoanRecords(@RequestParam(name = "partnerId", required = false) Long partnerId,
                                                                 @RequestParam(name = "participantId", required = false) Long participantId,
                                                                 @RequestParam(name = "status", required = false) Loan.LoanStatus status,
                                                                 @RequestParam(name = "quality", required = false) Loan.LoanQuality quality,
                                                                 @RequestParam(name = "approvedByPartner", required = false) Boolean approvedByPartner,
                                                                 @RequestParam(name = "pageNumber", defaultValue = "0") Integer pageNumber,
                                                                 @RequestParam(name = "pageSize", defaultValue = "10") Integer pageSize){
        final var sortedByDateCreated =
                PageRequest.of(pageNumber, pageSize, Sort.by("dateCreated").descending());
        return new ResponseEntity<>(this.loanService.getLoans(new LoanSearchCriteria(partnerId, participantId, status, quality, approvedByPartner, null, null), sortedByDateCreated), HttpStatus.OK);
    }

    @PostMapping("upload-template/{documentProgressId}")
    public ResponseEntity<ApiResponseDto> uploadLoansData(@RequestParam("excelFile") MultipartFile excelFile, @PathVariable("documentProgressId") String documentProgressId) {
        if (excelFile.isEmpty()) {
            return new ResponseEntity<>(new ApiResponseDto(false, CommonUtil.NO_FILE_TO_UPLOAD), HttpStatus.BAD_REQUEST);
        }
        return new ResponseEntity<>(new ApiResponseDto(true, this.bulkImportWorkbookService.importWorkbook(GlobalEntityType.LOAN_IMPORT_TEMPLATE.name(), excelFile, documentProgressId)+""), HttpStatus.CREATED);
    }

    @GetMapping("template/download")
    public ResponseEntity<?> downloadDataTemplate(HttpServletResponse response) {
        return bulkImportWorkbookPopulatorService.getTemplate(GlobalEntityType.LOAN_IMPORT_TEMPLATE.toString(), response);
    }

    @GetMapping("{loanId}")
    public ResponseEntity<LoanDto> getLoan(@PathVariable("loanId") Long loanId){
        return new ResponseEntity<>(this.loanService.findLoanById(loanId), HttpStatus.OK);
    }

    @PostMapping("approve-or-reject")
    public ResponseEntity<ApiResponseDto> approveLoansData(@RequestBody List<Long> loanIds, @RequestParam(name = "approved") Boolean approved) {
        this.loanService.approvedParticipantsLoansData(loanIds, approved);
        return new ResponseEntity<>(new ApiResponseDto(true, CommonUtil.RESOURCE_UPDATED), HttpStatus.OK);
    }
}
