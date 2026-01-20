package com.jgp.infrastructure.bulkimport.importhandler;

import com.google.common.collect.Lists;
import com.jgp.authentication.service.UserService;
import com.jgp.finance.domain.Loan;
import com.jgp.finance.domain.LoanTransaction;
import com.jgp.finance.dto.LoanRequestDto;
import com.jgp.finance.service.LoanService;
import com.jgp.infrastructure.bulkimport.constants.LoanConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.data.ExcelTemplateProcessingResult;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.exception.InvalidDataException;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantRequestDto;
import com.jgp.participant.service.ParticipantService;
import com.jgp.patner.service.PartnerService;
import com.jgp.shared.validator.DataValidator;
import com.jgp.shared.validator.LoanValidator;
import com.jgp.shared.validator.ParticipantValidator;
import jakarta.annotation.PreDestroy;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;


@Service
@Slf4j
@RequiredArgsConstructor
public class LoanImportHandler implements ImportHandler {

    private final LoanService loanService;
    private final ParticipantService participantService;
    private final UserService userService;
    private final ImportProgressService importProgressService;
    private final PartnerService partnerService;
    List<LoanRequestDto> loanDataList;
    private Workbook workbook;
    private Sheet loanSheet;
    private Map<Integer, String> rowErrorMap;
    private String documentImportProgressUUId;
    private Boolean updateParticipantInfo;
    private Document document;
    private Long currentPartnerId;
    private final AtomicInteger currentStepProgress = new AtomicInteger(0);


    @Override
    public CompletableFuture<Count> process(BulkImportEvent bulkImportEvent) {
        log.info("Starting Loan import process for document: {}", bulkImportEvent.document().getId());
        this.workbook = bulkImportEvent.workbook();
        this.loanSheet = workbook.getSheet(TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        this.loanDataList = new ArrayList<>();
        this.rowErrorMap = new ConcurrentHashMap<>();
        this.documentImportProgressUUId = bulkImportEvent.importProgressUUID();
        this.updateParticipantInfo = bulkImportEvent.updateParticipantInfo();
        this.document = bulkImportEvent.document();
        this.currentPartnerId = getCurrentPartnerId(userService);
        readExcelFile();
        return processChunks();
    }

    public void readExcelFile() {
        if (loanSheet == null) {
            log.error("Sheet '{}' not found in workbook", TemplatePopulateImportConstants.LOAN_SHEET_NAME);
            throw new InvalidDataException("Required sheet not found: " + TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        }

        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(loanSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);
        if (noOfEntries == null || noOfEntries == 0) {
            log.warn("No data rows found in sheet");
            importProgressService.updateTotal(documentImportProgressUUId, 0);
            return;
        }

        log.info("Starting to read {} rows from loans sheet", noOfEntries);
        importProgressService.updateTotal(documentImportProgressUUId, noOfEntries);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_READING_STEP);

        boolean headerSkipped = false;
        currentStepProgress.set(0); // Reset counter

        for (Row row : loanSheet) {
            // Skip header row
            if (!headerSkipped) {
                headerSkipped = true;
                continue;
            }

            if (row != null && ImportHandlerUtils.isNotImported(row, LoanConstants.STATUS_COL)) {
                try {
                    loanDataList.add(readLoanData(row));
                    int processedRows = currentStepProgress.incrementAndGet();
                    updateProgressInBulk(importProgressService, documentImportProgressUUId, processedRows);
                } catch (Exception ex) {
                    log.error("Error reading row {}: {}", row.getRowNum(), ex.getMessage());
                    rowErrorMap.put(row.getRowNum(), "Error reading row: " + ex.getMessage());
                }
            }
        }
        // Final progress update
        importProgressService.sendProgressUpdate(documentImportProgressUUId, currentStepProgress.get());

        log.info("Successfully read {} rows from sheet", currentStepProgress.get());
    }

    private LoanRequestDto readLoanData(Row row) {
        final var pipeLineSource = ImportHandlerUtils.readAsString(LoanConstants.PIPELINE_SOURCE, row);
        final var applicationDate = DataValidator.validateLocalDate(LoanConstants.DATE_APPLIED, row, rowErrorMap, "Application Date", true);
        final var dateDisbursed = DataValidator.validateLocalDate(LoanConstants.DATE_DISBURSED, row, rowErrorMap, "Date Disbursed", true);
        final var amountApproved = DataValidator.validateTemplateDoubleValue(LoanConstants.LOAN_AMOUNT_KES, row, "amount approved", rowErrorMap, true);
        final var loanAmount = null == amountApproved ? BigDecimal.ZERO : BigDecimal.valueOf(amountApproved);
        final var loanDuration = DataValidator.validateTemplateIntegerValue(LoanConstants.LOAN_DURATION, row, "loan duration", rowErrorMap, true);
        final var outStandingAmountDouble = DataValidator.validateTemplateDoubleValue(LoanConstants.OUT_STANDING_AMOUNT, row, "out-standing amount", rowErrorMap, true);
        final var outStandingAmount = null == outStandingAmountDouble ? BigDecimal.ZERO : BigDecimal.valueOf(outStandingAmountDouble);
        var loanQuality = ImportHandlerUtils.readAsString(LoanConstants.LOAN_QUALITY, row);
        loanQuality = LoanValidator.validateLoanQuality(loanQuality, row, rowErrorMap);
        var loanQualityEnum = Loan.LoanQuality.NORMAL;
        if (null == rowErrorMap.get(row.getRowNum())){
            loanQualityEnum = (null != loanQuality) ? Loan.LoanQuality.valueOf(loanQuality.toUpperCase()) : Loan.LoanQuality.NORMAL;
        }
        final var recordedToJGPDBOnDate = DataValidator.validateLocalDate(LoanConstants.DATE_RECORDED_TO_JGP_DB_COL, row, rowErrorMap, "Date Recorded To JGP DB", true);
        final var loanAmountRepaidDouble = DataValidator.validateTemplateDoubleValue(LoanConstants.REPAID_LOAN_AMOUNT, row, "amount repaid", rowErrorMap, true);
        final var loanAmountRepaid = null == loanAmountRepaidDouble ? BigDecimal.ZERO : BigDecimal.valueOf(loanAmountRepaidDouble);
        final var tranchAmountDouble = DataValidator.validateTemplateDoubleValue(LoanConstants.TRANCH_AMOUNT_COL, row, "tranch amount", rowErrorMap, false);
        final var tranchAmount = null == tranchAmountDouble ? BigDecimal.ZERO : BigDecimal.valueOf(tranchAmountDouble);
        var tranchAllocated = ImportHandlerUtils.readAsString(LoanConstants.TRANCH_ALLOCATED_COL, row);
        tranchAllocated = LoanValidator.validateTranchAllocated(tranchAllocated, tranchAmount, row, rowErrorMap);
        var loanerType = ImportHandlerUtils.readAsString(LoanConstants.LOANER_TYPE_COL, row);
        loanerType = LoanValidator.validateLoanerType(loanerType, row, rowErrorMap);
        var loanProduct = ImportHandlerUtils.readAsString(LoanConstants.LOAN_PRODUCT_COL, row);
        loanProduct = LoanValidator.validateAndNormalizeLoanProduct(loanProduct, row, rowErrorMap);
        var loanIdentifier = ImportHandlerUtils.readAsString(LoanConstants.LOAN_IDENTIFIER_COL, row);
        if (null == rowErrorMap.get(row.getRowNum()) && null == loanIdentifier){
            rowErrorMap.put(row.getRowNum(), "Loan Identifier is required !!");
        }

        String jgpId = ImportHandlerUtils.readAsString(LoanConstants.JGP_ID_COL, row);

        final var transactionAmount = tranchAmount.compareTo(BigDecimal.ZERO) <= 0 ? loanAmount : tranchAmount;
        if (null == rowErrorMap.get(row.getRowNum()) && transactionAmount.compareTo(BigDecimal.ZERO) < 1){
            rowErrorMap.put(row.getRowNum(), "Loan Amount and Tranch Amount can not be both 0!!");
        }
        if (null == rowErrorMap.get(row.getRowNum()) && loanAmount.compareTo(BigDecimal.ZERO) > 0 && tranchAmount.compareTo(loanAmount) >= 0){
            rowErrorMap.put(row.getRowNum(), "Loan Amount Must Be Greater Than Tranch Amount If Both Are Provided!!");
        }
        return LoanRequestDto.builder()
                .participantRequestDto(getParticipantDto(row))
                .loanNumber(loanIdentifier)
                .pipeLineSource(pipeLineSource)
                .loanAmount(loanAmount)
                .loanOutStandingAmount(outStandingAmount)
                .loanAmountRepaid(loanAmountRepaid)
                .loanerType(loanerType)
                .loanProduct(loanProduct)
                .loanDuration(loanDuration)
                .dateApplied(applicationDate)
                .dateRecordedByPartner(null)
                .dateAddedToDB(recordedToJGPDBOnDate)
                .dateDisbursed(dateDisbursed)
                .loanStatus(Loan.LoanStatus.APPROVED)
                .loanQuality(loanQualityEnum)
                .uniqueValues(loanIdentifier + "_" + jgpId + "_" + dateDisbursed)
                .rowIndex(row.getRowNum())
                .rowErrorMessage(null)
                .partner(Objects.nonNull(userService.currentUser()) ? userService.currentUser().getPartner() : null)
                .createdBy(userService.currentUser())
                .document(this.document)
                .loanTransaction(new LoanTransaction(LoanTransaction.TransactionType.DISBURSEMENT,
                        null != tranchAllocated ? tranchAllocated : "Full Loan", dateDisbursed, transactionAmount,
                        outStandingAmount, userService.currentUser(), null != tranchAllocated))
                .build();
    }

    private ParticipantRequestDto getParticipantDto(Row row){
        String participantName = ImportHandlerUtils.readAsString(LoanConstants.PARTICIPANT_NAME_COL, row);
        String businessName = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_NAME_COL, row);
        String businessRegNumber = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_REGISTRATION_NUMBER_COL, row);
        String jgpId = ImportHandlerUtils.readAsString(LoanConstants.JGP_ID_COL, row);
        final var phoneNumber = DataValidator.validatePhoneNumber(LoanConstants.BUSINESS_PHONE_NUMBER_COL, row, rowErrorMap);
        var gender = ImportHandlerUtils.readAsString(LoanConstants.GENDER_COL, row);
        gender = ParticipantValidator.validateGender(gender, row, rowErrorMap);
        var age = DataValidator.validateTemplateIntegerValue(LoanConstants.AGE_COL, row, "age", rowErrorMap, false);
        age = ParticipantValidator.validateParticipantAge(age, row, rowErrorMap);
        var locationCounty = DataValidator.validateCountyName(LoanConstants.BUSINESS_LOCATION_COL, row, rowErrorMap);
        final var industrySector = ImportHandlerUtils.readAsString(LoanConstants.INDUSTRY_SECTOR_COL, row);

        final var totalRegularEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.TOTAL_REGULAR_EMPLOYEES_COL, row, "total regular employees", rowErrorMap, true);
        if ((null == totalRegularEmployees || totalRegularEmployees < 1) && null == rowErrorMap.get(row.getRowNum())){
            rowErrorMap.put(row.getRowNum(), "Regular Employees Must Be Greater Than 0 !!");
        }
        final var youthRegularEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.YOUTH_REGULAR_EMPLOYEES_COL, row, "youth regular employees", rowErrorMap, false);
        final var totalCasualEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.TOTAL_CASUAL_EMPLOYEES_COL, row, "total casual employees", rowErrorMap, false);
        final var youthCasualEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.YOUTH_CASUAL_EMPLOYEES_COL, row, "youth casual employees", rowErrorMap, false);

        return ParticipantRequestDto.builder()
                .phoneNumber(phoneNumber).businessLocation(locationCounty.getCountyName()).businessName(businessName)
                .ownerGender(gender).ownerAge(age).industrySector(industrySector).businessSegment("Other")
                .totalRegularEmployees(totalRegularEmployees)
                .youthRegularEmployees(youthRegularEmployees).totalCasualEmployees(totalCasualEmployees)
                .youthCasualEmployees(youthCasualEmployees).jgpId(jgpId)
                .locationCountyCode(locationCounty.getCountyCode())
                .businessRegNumber(businessRegNumber).participantName(participantName).rowIndex(row.getRowNum()).build();
    }

    /**
     * Processes all chunks of TA data asynchronously.
     * Steps: 1) Validate chunks in parallel, 2) Store to database in parallel, 3) Write results to workbook sequentially
     * @return CompletableFuture containing count of total, success and failure records
     */
    @Async
    public CompletableFuture<Count> processChunks() {
        // Early return if no data to process
        if (loanDataList.isEmpty()) {
            log.warn("No Loan data to process");
            return CompletableFuture.completedFuture(Count.instance(0, 0, 0));
        }
        final var loanDataSize = loanDataList.size();

        final var existingParticipants = participantService.findParticipantsByJGPIDs(
                loanDataList.stream()
                        .map(LoanRequestDto::participantRequestDto)
                        .map(ParticipantRequestDto::jgpId)
                        .filter(Objects::nonNull)
                        .distinct()
                        .toList()
        );

        // 1. Split the loanDataList into smaller chunks
        final var chunks = Lists.partition(loanDataList, CHUNK_SIZE);
        log.info("Processing {} records in {} chunks", loanDataList.size(), chunks.size());

        // 2. Validate each chunk asynchronously
        currentStepProgress.set(0); // Reset counter
        importProgressService.resetEveryThingToZero(documentImportProgressUUId);
        importProgressService.updateTotal(documentImportProgressUUId, loanDataSize);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_VALIDATING_STEP);

        final var validatedFutures = chunks.stream()
                .map(chunk -> CompletableFuture.supplyAsync(() -> validateSingleChunk(chunk), IMPORT_EXECUTOR))
                .toList();

        CompletableFuture.allOf(validatedFutures.toArray(new CompletableFuture[0])).join();

        // Wait for UI to sync before moving to next step
        sleep(log);

        // STORING DATA STEP
        currentStepProgress.set(0); // Reset counter
        importProgressService.resetEveryThingToZero(documentImportProgressUUId);
        importProgressService.updateTotal(documentImportProgressUUId, loanDataSize);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_STORING_STEP);

        // 3. Storing TA data - process in parallel, collect results
        final var storingFutures = chunks.stream()
                .map(chunk -> CompletableFuture.supplyAsync(() -> storeDataWithoutWritingToWorkbook(chunk, existingParticipants), IMPORT_EXECUTOR))
                .toList();

        CompletableFuture.allOf(storingFutures.toArray(new CompletableFuture[0])).join();

        // Wait for all futures to complete and collect results
        final var allResults = storingFutures.stream()
                .map(CompletableFuture::join)
                .flatMap(List::stream)
                .toList();

        // Wait for UI to sync before moving to next step
        sleep(log);

        // 4. Write results to workbook sequentially (not thread-safe)
        currentStepProgress.set(0); // Reset counter
        importProgressService.resetEveryThingToZero(documentImportProgressUUId);
        importProgressService.updateTotal(documentImportProgressUUId, loanDataSize);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_STATUS_STEP);
        setReportHeaders(loanSheet, LoanConstants.STATUS_COL, LoanConstants.FAILURE_COL);

        // Count successes and failures while writing results
        long successCount = allResults.stream().filter(ExcelTemplateProcessingResult::success).count();
        long failureCount = allResults.size() - successCount;

        for (var result : allResults) {
            writeResultToWorkbook(result, LoanConstants.STATUS_COL, LoanConstants.FAILURE_COL);
            int processedRows = currentStepProgress.incrementAndGet();
            updateProgressInBulk(importProgressService, documentImportProgressUUId, processedRows);
        }
        // Final progress update
        importProgressService.sendProgressUpdate(documentImportProgressUUId, currentStepProgress.get());

        log.info("Finished Import - Total: {}, Success: {}, Failed: {} at {}",
                loanDataList.size(), successCount, failureCount, LocalDateTime.now(ZoneId.systemDefault()));

        return CompletableFuture.completedFuture(Count.instance(loanDataList.size(), (int) successCount, (int) failureCount));
    }

    /**
     * Associates a participant to the given loanDataDto based on the provided participant map.
     *
     * @param loanDataDto      the loanDataDto DTO
     * @param participantMap a map of participants keyed by their JGP IDs
     * @return the loanDataDto object with the associated participant
     */
    private Loan associateParticipantToLoanData(LoanRequestDto loanDataDto, Map<String, Participant> participantMap) {
        final var participantDto = loanDataDto.participantRequestDto();
        final var loan = new Loan(loanDataDto);
        final var participant = this.participantService.createOrUpdateParticipant(participantDto, participantMap, this.updateParticipantInfo);
        if (Objects.isNull(participant)) {
            rowErrorMap.put(loanDataDto.rowIndex(), PARTICIPANT_ASSOCIATION_ERROR);
        }
        loan.setParticipant(participant);
        final var partner = Objects.nonNull(currentPartnerId) ? partnerService.findPartnerById(currentPartnerId) : null;
        if (Objects.isNull(partner)) {
            rowErrorMap.put(loanDataDto.rowIndex(), PARTNER_ASSOCIATION_ERROR);
        }
        loan.setPartner(partner);
        return loan;
    }

    /**
     * Validates a single chunk of LoanData.
     *
     * @param chunk the list of LoanData to validate
     * @return a list of valid LoanData
     */
    private List<LoanRequestDto> validateSingleChunk(List<LoanRequestDto> chunk) {
        List<LoanRequestDto> validData = new ArrayList<>();
        for (LoanRequestDto loanData : chunk) {
            final var participantDto = loanData.participantRequestDto();
            try {
                ParticipantValidator.validateParticipant(participantDto, rowErrorMap);
                LoanValidator.validateLoan(loanData, rowErrorMap);
                validData.add(loanData);
                int processedRows = currentStepProgress.incrementAndGet();
                updateProgressInBulk(importProgressService, documentImportProgressUUId, processedRows);
            } catch (RuntimeException ex) {
                log.error("Problem occurred when validating data: {}", ex.getMessage());
                var errorMessage = ImportHandlerUtils.getErrorMessage(ex);
                rowErrorMap.put(loanData.rowIndex(), errorMessage);
            }
        }
        // Final progress update for the chunk
        importProgressService.sendProgressUpdate(documentImportProgressUUId, currentStepProgress.get());
        return validData;
    }

    private List<ExcelTemplateProcessingResult> storeDataWithoutWritingToWorkbook(List<LoanRequestDto> chunk, Map<String, Participant> participantMap) {
        List<ExcelTemplateProcessingResult> results = new ArrayList<>();
        for (LoanRequestDto loanData : chunk) {
            var result = storeSingleDataWithoutWritingToWorkbook(loanData, participantMap);
            results.add(result);
            int processedRows = currentStepProgress.incrementAndGet();
            updateProgressInBulk(importProgressService, documentImportProgressUUId, processedRows);
        }
        // Final progress update for the chunk
        importProgressService.sendProgressUpdate(documentImportProgressUUId, currentStepProgress.get());
        return results;
    }


    /**
     * Stores data to database without writing to workbook (thread-safe for parallel execution)
     */
    private ExcelTemplateProcessingResult storeSingleDataWithoutWritingToWorkbook(LoanRequestDto loanData, Map<String, Participant> participantMap) {
        Row row = loanSheet.getRow(loanData.rowIndex());

        try {
            final var dataWithParticipant = associateParticipantToLoanData(loanData, participantMap);
            final var validationError = rowErrorMap.get(row.getRowNum());
            if (Objects.nonNull(validationError)) {
                throw new InvalidDataException(validationError);
            }
            this.loanService.createOrUpdateLoan(dataWithParticipant);
            return new ExcelTemplateProcessingResult(row, true, null);
        } catch (RuntimeException ex) {
            log.error("Problem occurred when uploading TA: {}", ex.getMessage());
            var errorMessage = ImportHandlerUtils.getErrorMessage(ex);
            if (errorMessage.contains("unique_loan") || errorMessage.contains("Duplicate Disbursement On Same Day")){
                errorMessage = "Row with same partner/participant/disburse date/Loan Identifier already exist !!";
            }
            return new ExcelTemplateProcessingResult(row, false, errorMessage);
        }
    }

    @PreDestroy
    public void cleanup() {
        try {
            workbook.close();
        } catch (IOException e) {
            log.error("Error while closing workbook", e);
        }
        IMPORT_EXECUTOR.shutdown();
    }
}
