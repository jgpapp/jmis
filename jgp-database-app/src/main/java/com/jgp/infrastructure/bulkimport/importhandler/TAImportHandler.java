package com.jgp.infrastructure.bulkimport.importhandler;

import com.google.common.collect.Lists;
import com.jgp.authentication.service.UserService;
import com.jgp.bmo.domain.TAData;
import com.jgp.bmo.dto.TARequestDto;
import com.jgp.bmo.service.TADataService;
import com.jgp.infrastructure.bulkimport.data.ExcelTemplateProcessingResult;
import com.jgp.infrastructure.bulkimport.exception.InvalidDataException;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantRequestDto;
import com.jgp.participant.service.ParticipantService;
import com.jgp.infrastructure.bulkimport.constants.BMOConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.shared.validator.DataValidator;
import com.jgp.shared.validator.ParticipantValidator;
import com.jgp.shared.validator.TAValidator;
import jakarta.annotation.PreDestroy;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

@Service
@Slf4j
@RequiredArgsConstructor
public class TAImportHandler implements ImportHandler {

    private static final String YES = "YES";
    private static final String DUPLICATE_ENTRY_ERROR = "unique_bmo_participant_data";
    private static final String DUPLICATE_ENTRY_MESSAGE = "Row with same partner/participant/training date already exists!";
    private final AtomicInteger currentStepProgress = new AtomicInteger(0);

    private final TADataService taDataService;
    private final ParticipantService participantService;
    private final UserService userService;
    private final ImportProgressService importProgressService;

    private List<TARequestDto> taDataList;
    private Workbook workbook;
    private Map<Integer, String> rowErrorMap;
    private String documentImportProgressUUId;
    private Boolean updateParticipantInfo;
    private Document document;


    @Override
    public CompletableFuture<Count> process(BulkImportEvent bulkImportEvent) {
        log.info("Starting TA import process for document: {}", bulkImportEvent.document().getId());
        this.workbook = bulkImportEvent.workbook();
        this.taDataList = new ArrayList<>();
        this.rowErrorMap = new ConcurrentHashMap<>();
        this.documentImportProgressUUId = bulkImportEvent.importProgressUUID();
        this.updateParticipantInfo = bulkImportEvent.updateParticipantInfo();
        this.document = bulkImportEvent.document();
        readExcelFile();
        return processChunks();
    }


    public void readExcelFile() {
        Sheet taSheet = workbook.getSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        if (taSheet == null) {
            log.error("Sheet '{}' not found in workbook", TemplatePopulateImportConstants.BMO_SHEET_NAME);
            throw new InvalidDataException("Required sheet not found: " + TemplatePopulateImportConstants.BMO_SHEET_NAME);
        }

        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(taSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);
        if (noOfEntries == null || noOfEntries == 0) {
            log.warn("No data rows found in sheet");
            importProgressService.updateTotal(documentImportProgressUUId, 0);
            return;
        }

        log.info("Starting to read {} rows from sheet", noOfEntries);
        importProgressService.updateTotal(documentImportProgressUUId, noOfEntries);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_READING_STEP);

        boolean headerSkipped = false;
        currentStepProgress.set(0); // Reset counter

        for (Row row : taSheet) {
            // Skip header row
            if (!headerSkipped) {
                headerSkipped = true;
                continue;
            }

            if (row != null && ImportHandlerUtils.isNotImported(row, BMOConstants.STATUS_COL)) {
                try {
                    taDataList.add(readTAData(row));
                    int processedRows = currentStepProgress.incrementAndGet();
                    updateProgressInBulk(processedRows);
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


    private TARequestDto readTAData(Row row) {
        Boolean isApplicantEligible = YES.equalsIgnoreCase(ImportHandlerUtils.readAsString(BMOConstants.IS_APPLICANT_ELIGIBLE_COL, row));
        Boolean isRecommendedForFinance = YES.equalsIgnoreCase(ImportHandlerUtils.readAsString(BMOConstants.RECOMMENDED_FOR_FINANCE_COL, row));
        LocalDate pipelineDecisionDate = DataValidator.validateLocalDate(BMOConstants.DATE_OF_PIPELINE_DECISION_COL, row, rowErrorMap, "Date of Pipeline Decision", false);
        String referredFIBusiness = ImportHandlerUtils.readAsString(BMOConstants.REFERRED_FI_BUSINESS_COL, row);
        LocalDate dateRecordedByPartner = DataValidator.validateLocalDate(BMOConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL, row, rowErrorMap, "Date Recorded By Partner", true);

        String taNeeds = ImportHandlerUtils.readAsString(BMOConstants.TA_NEEDS_COL, row);
        taNeeds = Objects.nonNull(taNeeds) ? TAValidator.validateTANeeds(taNeeds, row, rowErrorMap) : null;

        final String trainingPartner = ImportHandlerUtils.readAsString(BMOConstants.TRAINING_PARTNER, row);

        String taDeliveryMode = ImportHandlerUtils.readAsString(BMOConstants.TA_DELIVERY_MODE, row);
        taDeliveryMode = TAValidator.validateTADeliveryMode(taDeliveryMode, row, rowErrorMap);

        final String otherTaNeeds = ImportHandlerUtils.readAsString(BMOConstants.OTHER_TA_NEEDS_COL, row);

        String taType = ImportHandlerUtils.readAsString(BMOConstants.TYPE_OF_TA_COL, row);
        taType = TAValidator.validateTATypes(taType, row, rowErrorMap);

        final LocalDate currentDate = LocalDate.now(ZoneId.systemDefault());
        final String normalizedTaNeeds = normalizeStringValues(taNeeds);

        final var currentUser = userService.currentUser();
        return TARequestDto.builder()
                .dateFormSubmitted(currentDate)
                .isApplicantEligible(isApplicantEligible)
                .tasAttended(0)
                .taSessionsAttended(0)
                .isRecommendedForFinance(isRecommendedForFinance)
                .decisionDate(pipelineDecisionDate)
                .fiBusinessReferred(referredFIBusiness)
                .dateRecordedByPartner(dateRecordedByPartner)
                .dateRecordedToJGPDB(currentDate)
                .taNeeds(normalizedTaNeeds)
                .trainingPartner(trainingPartner)
                .taDeliveryMode(taDeliveryMode)
                .otherTaNeeds(otherTaNeeds)
                .taType(taType)
                .createdBy(currentUser)
                .document(document)
                .row(row)
                .rowErrorMap(rowErrorMap)
                .rowIndex(row.getRowNum())
                .rowErrorMessage(rowErrorMap.get(row.getRowNum()))
                .participantRequestDto(getParticipantDto(row))
                .partner(Objects.nonNull(currentUser) ? currentUser.getPartner() : null)
                .build();
    }

    /**
     * Extracts ParticipantDto from the given row
     */
    private ParticipantRequestDto getParticipantDto(Row row) {
        final String participantName = ImportHandlerUtils.readAsString(BMOConstants.PARTICIPANT_NAME_COL, row);
        final String jgpId = ImportHandlerUtils.readAsString(BMOConstants.JGP_ID_COL, row);
        final String phoneNumber = DataValidator.validatePhoneNumber(BMOConstants.BUSINESS_PHONE_NUMBER_COL, row, rowErrorMap);
        final String alternativePhoneNumber = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_ALTERNATIVE_PHONE_NUMBER_COL, row);

        String gender = ImportHandlerUtils.readAsString(BMOConstants.GENDER_COL, row);
        gender = ParticipantValidator.validateGender(gender, row, rowErrorMap);

        Integer age = DataValidator.validateTemplateIntegerValue(BMOConstants.AGE_COL, row, "Age", rowErrorMap, false);
        age = ParticipantValidator.validateParticipantAge(age, row, rowErrorMap);

        var locationCounty = DataValidator.validateCountyName(BMOConstants.BUSINESS_LOCATION_COL, row, rowErrorMap);
        final String businessLocationSubCounty = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_SUB_COUNTY_LOCATION_COL, row);
        final String industrySector = ImportHandlerUtils.readAsString(BMOConstants.INDUSTRY_SECTOR_COL, row);

        String businessSegment = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_SEGMENT_COL, row);
        businessSegment = TAValidator.validateBusinessSegment(businessSegment, row, rowErrorMap);

        final String registrationNumber = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_REGISTRATION_NUMBER_COL, row);

        final Double bestMonthlyRevenueD = DataValidator.validateTemplateDoubleValue(BMOConstants.BEST_MONTH_MONTHLY_REVENUE_COL, row, "Best monthly revenue", rowErrorMap, true);
        final BigDecimal bestMonthlyRevenue = Objects.nonNull(bestMonthlyRevenueD) ? BigDecimal.valueOf(bestMonthlyRevenueD) : null;

        final Double worstMonthlyRevenueD = DataValidator.validateTemplateDoubleValue(BMOConstants.WORST_MONTH_MONTHLY_REVENUE_COL, row, "worst monthly revenue", rowErrorMap, true);
        final BigDecimal worstMonthlyRevenue = Objects.nonNull(worstMonthlyRevenueD) ? BigDecimal.valueOf(worstMonthlyRevenueD) : null;

        final Integer totalRegularEmployees = DataValidator.validateTemplateIntegerValue(BMOConstants.TOTAL_REGULAR_EMPLOYEES_COL, row, "total regular employees", rowErrorMap, true);
        final Integer youthRegularEmployees = DataValidator.validateTemplateIntegerValue(BMOConstants.YOUTH_REGULAR_EMPLOYEES_COL, row, "youth regular employees", rowErrorMap, false);
        final Integer totalCasualEmployees = DataValidator.validateTemplateIntegerValue(BMOConstants.TOTAL_CASUAL_EMPLOYEES_COL, row, "total casual employees", rowErrorMap, false);
        final Integer youthCasualEmployees = DataValidator.validateTemplateIntegerValue(BMOConstants.YOUTH_CASUAL_EMPLOYEES_COL, row, "youth casual employees", rowErrorMap, false);

        String sampleRecordsKept = ImportHandlerUtils.readAsString(BMOConstants.SAMPLE_RECORDS_KEPT_COL, row);
        sampleRecordsKept = Objects.nonNull(sampleRecordsKept) ? TAValidator.validateSampleRecords(sampleRecordsKept, row, rowErrorMap) : null;

        final String personWithDisability = ImportHandlerUtils.readAsString(BMOConstants.PERSON_WITH_DISABILITY_COL, row);
        ParticipantValidator.validatePersonWithDisability(personWithDisability, row, rowErrorMap);

        final String refugeeStatus = ImportHandlerUtils.readAsString(BMOConstants.REFUGEE_STATUS_COL, row);
        ParticipantValidator.validateRefugeeStatus(refugeeStatus, row, rowErrorMap);

        return ParticipantRequestDto.builder()
                .phoneNumber(phoneNumber)
                .alternativePhoneNumber(alternativePhoneNumber)
                .bestMonthlyRevenue(bestMonthlyRevenue)
                .businessLocation(locationCounty.getCountyName())
                .locationSubCounty(businessLocationSubCounty)
                .participantName(participantName)
                .ownerGender(gender)
                .ownerAge(age)
                .industrySector(industrySector)
                .businessSegment(businessSegment)
                .worstMonthlyRevenue(worstMonthlyRevenue)
                .totalRegularEmployees(totalRegularEmployees)
                .youthRegularEmployees(youthRegularEmployees)
                .totalCasualEmployees(totalCasualEmployees)
                .youthCasualEmployees(youthCasualEmployees)
                .sampleRecords(parseSampleRecords(sampleRecordsKept))
                .personWithDisability(convertYesNoToCapitalized(personWithDisability))
                .refugeeStatus(convertYesNoToCapitalized(refugeeStatus))
                .jgpId(jgpId)
                .locationCountyCode(locationCounty.getCountyCode())
                .businessRegNumber(registrationNumber)
                .row(row)
                .rowErrorMap(rowErrorMap)
                .build();
    }

    /**
     * Parses comma-separated sample records string into a list
     */
    private List<String> parseSampleRecords(String sampleRecordsKept) {
        return Objects.nonNull(sampleRecordsKept)
                ? Arrays.stream(sampleRecordsKept.split(","))
                        .map(String::trim)
                        .filter(s -> !s.isEmpty())
                        .toList()
                : null;
    }

    private String convertYesNoToCapitalized(String value) {
        return YES.equalsIgnoreCase(value) ? "Yes" : "No";
    }

    /**
     * Processes all chunks of TA data asynchronously.
     * Steps: 1) Validate chunks in parallel, 2) Store to database in parallel, 3) Write results to workbook sequentially
     * @return CompletableFuture containing count of total, success and failure records
     */
    @Async
    public CompletableFuture<Count> processChunks() {
        // Early return if no data to process
        if (taDataList.isEmpty()) {
            log.warn("No TA data to process");
            return CompletableFuture.completedFuture(Count.instance(0, 0, 0));
        }
        final var taDataSize = taDataList.size();

        final var existingParticipants = participantService.findParticipantsByJGPIDs(
                taDataList.stream()
                        .map(TARequestDto::participantRequestDto)
                        .map(ParticipantRequestDto::jgpId)
                        .filter(Objects::nonNull)
                        .distinct()
                        .toList()
        );

        // 1. Split the taDataList into smaller chunks
        final var chunks = Lists.partition(taDataList, CHUNK_SIZE);
        log.info("Processing {} records in {} chunks", taDataList.size(), chunks.size());

        // 2. Validate each chunk asynchronously
        currentStepProgress.set(0); // Reset counter
        importProgressService.resetEveryThingToZero(documentImportProgressUUId);
        importProgressService.updateTotal(documentImportProgressUUId, taDataSize);
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
        importProgressService.updateTotal(documentImportProgressUUId, taDataSize);
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
        importProgressService.updateTotal(documentImportProgressUUId, taDataSize);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_STATUS_STEP);
        Sheet taSheet = workbook.getSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        setReportHeaders(taSheet, BMOConstants.STATUS_COL, BMOConstants.FAILURE_COL);

        // Count successes and failures while writing results
        long successCount = allResults.stream().filter(ExcelTemplateProcessingResult::success).count();
        long failureCount = allResults.size() - successCount;

        for (var result : allResults) {
            writeResultToWorkbook(result, BMOConstants.STATUS_COL, BMOConstants.FAILURE_COL);
            int processedRows = currentStepProgress.incrementAndGet();
            updateProgressInBulk(processedRows);
        }
        // Final progress update
        importProgressService.sendProgressUpdate(documentImportProgressUUId, currentStepProgress.get());

        log.info("Finished Import - Total: {}, Success: {}, Failed: {} at {}",
                taDataList.size(), successCount, failureCount, LocalDateTime.now(ZoneId.systemDefault()));

        return CompletableFuture.completedFuture(Count.instance(taDataList.size(), (int) successCount, (int) failureCount));
    }

    /**
     * Associates a participant to the given TA data based on the provided participant map.
     *
     * @param taDataDto      the TA data DTO
     * @param participantMap a map of participants keyed by their JGP IDs
     * @return the TAData object with the associated participant
     */
    private TAData associateParticipantToTAData(TARequestDto taDataDto, Map<String, Participant> participantMap) {
        final var participantDto = taDataDto.participantRequestDto();
        final var taData = new TAData(taDataDto);
        final var participant = this.participantService.createOrUpdateParticipant(participantDto, participantMap, this.updateParticipantInfo);
        if (Objects.isNull(participant)) {
            taDataDto.rowErrorMap().put(taDataDto.row().getRowNum(), PARTICIPANT_ASSOCIATION_ERROR);
        }
        taData.setParticipant(participant);
        return taData;
    }

    /**
     * Validates a single chunk of TAData.
     *
     * @param chunk the list of TAData to validate
     * @return a list of valid TAData
     */
    private List<TARequestDto> validateSingleChunk(List<TARequestDto> chunk) {
        List<TARequestDto> validData = new ArrayList<>();
        for (TARequestDto taData : chunk) {
            final var participantDto = taData.participantRequestDto();
            try {
                final var validator = DataValidator.getValidator();
                ParticipantValidator.validateParticipant(participantDto, validator);
                TAValidator.validateTAData(taData, validator);
                validData.add(taData);
                int processedRows = currentStepProgress.incrementAndGet();
                updateProgressInBulk(processedRows);
            } catch (RuntimeException ex) {
                log.error("Problem occurred when validating participant: {}", ex.getMessage());
                var errorMessage = ImportHandlerUtils.getErrorMessage(ex);
                taData.rowErrorMap().put(taData.row().getRowNum(), errorMessage);
            }
        }
        // Final progress update for the chunk
        importProgressService.sendProgressUpdate(documentImportProgressUUId, currentStepProgress.get());
        return validData;
    }

    private List<ExcelTemplateProcessingResult> storeDataWithoutWritingToWorkbook(List<TARequestDto> chunk, Map<String, Participant> participantMap) {
        List<ExcelTemplateProcessingResult> results = new ArrayList<>();
        for (TARequestDto taData : chunk) {
            var result = storeSingleDataWithoutWritingToWorkbook(taData, participantMap);
            results.add(result);
            int processedRows = currentStepProgress.incrementAndGet();
            updateProgressInBulk(processedRows);
        }
        // Final progress update for the chunk
        importProgressService.sendProgressUpdate(documentImportProgressUUId, currentStepProgress.get());
        return results;
    }


    /**
     * Stores data to database without writing to workbook (thread-safe for parallel execution)
     */
    private ExcelTemplateProcessingResult storeSingleDataWithoutWritingToWorkbook(TARequestDto taData, Map<String, Participant> participantMap) {
        Row row = taData.row();

        try {
            final var dataWithParticipant = associateParticipantToTAData(taData, participantMap);
            final var validationError = taData.rowErrorMap().get(row.getRowNum());
            if (Objects.nonNull(validationError)) {
                throw new InvalidDataException(validationError);
            }
            this.taDataService.createBMOData(List.of(dataWithParticipant));
            return new ExcelTemplateProcessingResult(row, true, null);
        } catch (RuntimeException ex) {
            log.error("Problem occurred when uploading TA: {}", ex.getMessage());
            var errorMessage = ImportHandlerUtils.getErrorMessage(ex);
            if (errorMessage.contains(DUPLICATE_ENTRY_ERROR)) {
                errorMessage = DUPLICATE_ENTRY_MESSAGE;
            }
            return new ExcelTemplateProcessingResult(row, false, errorMessage);
        }
    }

    /**
     * Updates progress in bulk after processing a certain number of records
     * @param processedCount processedCount
     */
    private void updateProgressInBulk(int processedCount) {
        if (processedCount % BULK_SIZE_FOR_PROGRESS_UPDATE == 0) {
            importProgressService.sendProgressUpdate(documentImportProgressUUId, processedCount);
        }
    }

    @PreDestroy
    public void cleanup() {
        IMPORT_EXECUTOR.shutdown();
    }
}
