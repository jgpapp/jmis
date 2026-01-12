package com.jgp.infrastructure.bulkimport.importhandler;

import com.google.common.collect.Lists;
import com.jgp.authentication.service.UserService;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.data.ExcelTemplateProcessingResult;
import com.jgp.infrastructure.bulkimport.data.MonitoringConstants;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.exception.InvalidDataException;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.monitoring.domain.OutComeMonitoring;
import com.jgp.monitoring.dto.OutComeMonitoringRequestDto;
import com.jgp.monitoring.service.OutComeMonitoringService;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.service.ParticipantService;
import com.jgp.shared.validator.DataValidator;
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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

@Service
@Slf4j
@RequiredArgsConstructor
public class MonitoringImportHandler implements ImportHandler {
    private final ImportProgressService importProgressService;
    private final ParticipantService participantService;
    private final OutComeMonitoringService outComeMonitoringService;
    private final UserService userService;
    private Workbook workbook;
    private Sheet monitoringSheet;
    private List<OutComeMonitoringRequestDto> monitoringDataList;
    private Map<Integer, String> rowErrorMap;
    private String documentImportProgressUUId;
    private Document document;
    private final AtomicInteger currentStepProgress = new AtomicInteger(0);

    @Override
    public CompletableFuture<Count> process(BulkImportEvent bulkImportEvent) {
        log.info("Starting Monitoring import process for document: {}", bulkImportEvent.document().getId());
        this.workbook = bulkImportEvent.workbook();
        this.monitoringSheet = workbook.getSheet(TemplatePopulateImportConstants.MONITORING_SHEET_NAME);
        this.monitoringDataList = new ArrayList<>();
        this.rowErrorMap = new ConcurrentHashMap<>();
        this.documentImportProgressUUId = bulkImportEvent.importProgressUUID();
        this.document = bulkImportEvent.document();
        readExcelFile();
        return processChunks();
    }

    public void readExcelFile() {
        if (monitoringSheet == null) {
            log.error("Sheet '{}' not found in workbook", TemplatePopulateImportConstants.MONITORING_SHEET_NAME);
            throw new InvalidDataException("Required sheet not found: " + TemplatePopulateImportConstants.MONITORING_SHEET_NAME);
        }

        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(monitoringSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);
        if (noOfEntries == null || noOfEntries == 0) {
            log.warn("No data rows found in sheet");
            importProgressService.updateTotal(documentImportProgressUUId, 0);
            return;
        }

        log.info("Starting to read {} rows from monitoring sheet", noOfEntries);
        importProgressService.updateTotal(documentImportProgressUUId, noOfEntries);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_READING_STEP);

        boolean headerSkipped = false;
        currentStepProgress.set(0); // Reset counter

        for (Row row : monitoringSheet) {
            // Skip header row
            if (!headerSkipped) {
                headerSkipped = true;
                continue;
            }

            if (row != null && ImportHandlerUtils.isNotImported(row, MonitoringConstants.STATUS_COL)) {
                try {
                    monitoringDataList.add(readMonitoringData(row));
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

    private OutComeMonitoringRequestDto readMonitoringData(Row row) {
        final var jgpId = ImportHandlerUtils.readAsString(MonitoringConstants.JGP_ID_COL, row);
        final var locationLatDouble = DataValidator.validateTemplateDoubleValue(MonitoringConstants.LOCATION_LATITUDE_COL, row, "location latitude", rowErrorMap, false);
        final var locationLangDouble = DataValidator.validateTemplateDoubleValue(MonitoringConstants.LOCATION_LONGITUDE_COL, row, "location longitude", rowErrorMap, locationLatDouble != null);
        final var revenueChangeDouble = DataValidator.validateTemplateDoubleValue(MonitoringConstants.REVENUE_CHANGE_COL, row, "revenue change", rowErrorMap, false);
        var locationCounty = DataValidator.validateCountyName(MonitoringConstants.COUNTY_NAME_COL, row, rowErrorMap);
        var surveyDate = DataValidator.validateLocalDate(MonitoringConstants.SURVEY_DATE_COL, row, rowErrorMap, "Survey Date", true);
        return OutComeMonitoringRequestDto.builder()
            .surveyDate(surveyDate)
            .surveyLanguage(ImportHandlerUtils.readAsString(MonitoringConstants.SURVEY_LANGUAGE_COL, row))
            .consented(ImportHandlerUtils.readAsString(MonitoringConstants.CONSENTED_COL, row))
            .locationLatitude(locationLatDouble == null ? null : BigDecimal.valueOf(locationLatDouble))
            .locationLongitude(locationLangDouble == null ? null : BigDecimal.valueOf(locationLangDouble))
            .age(DataValidator.validateTemplateIntegerValue(MonitoringConstants.AGE_COL, row, "age", rowErrorMap, true))
            .genderCategory(ImportHandlerUtils.readAsString(MonitoringConstants.GENDER_CATEGORY_COL, row))
            .segment(ImportHandlerUtils.readAsString(MonitoringConstants.SEGMENT_COL, row))
            .partner(ImportHandlerUtils.readAsString(MonitoringConstants.PARTNER_COL, row))
            .gender(ImportHandlerUtils.readAsString(MonitoringConstants.GENDER_COL, row))
            .region(ImportHandlerUtils.readAsString(MonitoringConstants.REGION_COL, row))
            .countyCode(locationCounty.getCountyCode())
            .countyName(locationCounty.getCountyName())
            .businessSetting(ImportHandlerUtils.readAsString(MonitoringConstants.BUSINESS_SETTING_COL, row))
            .businessAgeCategory(ImportHandlerUtils.readAsString(MonitoringConstants.BUSINESS_AGE_CATEGORY_COL, row))
            .groupMembership(ImportHandlerUtils.readAsString(MonitoringConstants.GROUP_MEMBERSHIP_COL, row))
            .educationLevel(ImportHandlerUtils.readAsString(MonitoringConstants.EDUCATION_LEVEL_COL, row))
            .businessAge(DataValidator.validateTemplateIntegerValue(MonitoringConstants.BUSINESS_AGE_COL, row, "business age", rowErrorMap, false))
            .regularEmployees(DataValidator.validateTemplateIntegerValue(MonitoringConstants.REGULAR_EMPLOYEES_COL, row, "regular employees", rowErrorMap, false))
            .casualEmployees(DataValidator.validateTemplateIntegerValue(MonitoringConstants.CASUAL_EMPLOYEES_COL, row, "casual employees", rowErrorMap, false))
            .householdIncomeChange(ImportHandlerUtils.readAsString(MonitoringConstants.HOUSEHOLD_INCOME_CHANGE_COL, row))
            .financialStability(ImportHandlerUtils.readAsString(MonitoringConstants.FINANCIAL_STABILITY_COL, row))
            .qualityOfLife(ImportHandlerUtils.readAsString(MonitoringConstants.QUALITY_OF_LIFE_COL, row))
            .empowerment(ImportHandlerUtils.readAsString(MonitoringConstants.EMPOWERMENT_COL, row))
            .voiceInCommunity(ImportHandlerUtils.readAsString(MonitoringConstants.VOICE_IN_COMMUNITY_COL, row))
            .respectInCommunity(ImportHandlerUtils.readAsString(MonitoringConstants.RESPECT_IN_COMMUNITY_COL, row))
            .reliableIncome(ImportHandlerUtils.readAsString(MonitoringConstants.RELIABLE_INCOME_COL, row))
            .reputableWork(ImportHandlerUtils.readAsString(MonitoringConstants.REPUTABLE_WORK_COL, row))
            .senseOfPurpose(ImportHandlerUtils.readAsString(MonitoringConstants.SENSE_OF_PURPOSE_COL, row))
            .businessSectorGrowth(ImportHandlerUtils.readAsString(MonitoringConstants.BUSINESS_SECTOR_GROWTH_COL, row))
            .communityGrowth(ImportHandlerUtils.readAsString(MonitoringConstants.COMMUNITY_GROWTH_COL, row))
            .workOpportunities(ImportHandlerUtils.readAsString(MonitoringConstants.WORK_OPPORTUNITIES_COL, row))
            .incomeRegularity(ImportHandlerUtils.readAsString(MonitoringConstants.INCOME_REGULARITY_COL, row))
            .incomeSufficiency(ImportHandlerUtils.readAsString(MonitoringConstants.INCOME_SUFFICIENCY_COL, row))
            .incomePredictability(ImportHandlerUtils.readAsString(MonitoringConstants.INCOME_PREDICTABILITY_COL, row))
            .financialSecurity(ImportHandlerUtils.readAsString(MonitoringConstants.FINANCIAL_SECURITY_COL, row))
            .communityGroups(ImportHandlerUtils.readAsString(MonitoringConstants.COMMUNITY_GROUPS_COL, row))
            .leadershipRole(ImportHandlerUtils.readAsString(MonitoringConstants.LEADERSHIP_ROLE_COL, row))
            .decisionMakingConfidence(ImportHandlerUtils.readAsString(MonitoringConstants.DECISION_MAKING_CONFIDENCE_COL, row))
            .communityChange(ImportHandlerUtils.readAsString(MonitoringConstants.COMMUNITY_CHANGE_COL, row))
            .communityIssues(ImportHandlerUtils.readAsString(MonitoringConstants.COMMUNITY_ISSUES_COL, row))
            .satisfactionEducation(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_EDUCATION_COL, row))
            .satisfactionRelationships(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_RELATIONSHIPS_COL, row))
            .satisfactionBusinessType(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_BUSINESS_TYPE_COL, row))
            .satisfactionIncome(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_INCOME_COL, row))
            .satisfactionHousing(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_HOUSING_COL, row))
            .satisfactionHealthcare(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_HEALTHCARE_COL, row))
            .satisfactionWater(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_WATER_COL, row))
            .satisfactionFood(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_FOOD_COL, row))
            .satisfactionNutrition(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_NUTRITION_COL, row))
            .satisfactionLife(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_LIFE_COL, row))
            .satisfactionInformation(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_INFORMATION_COL, row))
            .satisfactionLeisure(ImportHandlerUtils.readAsString(MonitoringConstants.SATISFACTION_LEISURE_COL, row))
            .jgpInterventions(ImportHandlerUtils.readAsString(MonitoringConstants.JGP_INTERVENTIONS_COL, row))
            .technicalTraining(ImportHandlerUtils.readAsString(MonitoringConstants.TECHNICAL_TRAINING_COL, row))
            .newPractices(ImportHandlerUtils.readAsString(MonitoringConstants.NEW_PRACTICES_COL, row))
            .improvedPractices(ImportHandlerUtils.readAsString(MonitoringConstants.IMPROVED_PRACTICES_COL, row))
            .trainingImprovements(ImportHandlerUtils.readAsString(MonitoringConstants.TRAINING_IMPROVEMENTS_COL, row))
            .businessChanges(ImportHandlerUtils.readAsString(MonitoringConstants.BUSINESS_CHANGES_COL, row))
            .profitabilityGrowth(ImportHandlerUtils.readAsString(MonitoringConstants.PROFITABILITY_GROWTH_COL, row))
            .revenueChange(revenueChangeDouble == null ? null : BigDecimal.valueOf(revenueChangeDouble))
            .loanApplication(ImportHandlerUtils.readAsString(MonitoringConstants.LOAN_APPLICATION_COL, row))
            .numberOfLoans(DataValidator.validateTemplateIntegerValue(MonitoringConstants.NUMBER_OF_LOANS_COL, row, "number of loans", rowErrorMap, false))
            .loanPlatform(ImportHandlerUtils.readAsString(MonitoringConstants.LOAN_PLATFORM_COL, row))
            .externalFinancing(ImportHandlerUtils.readAsString(MonitoringConstants.EXTERNAL_FINANCING_COL, row))
            .financingSources(ImportHandlerUtils.readAsString(MonitoringConstants.FINANCING_SOURCES_COL, row))
            .jgpImpact(ImportHandlerUtils.readAsString(MonitoringConstants.JGP_IMPACT_COL, row))
            .changesWithoutJgp(ImportHandlerUtils.readAsString(MonitoringConstants.CHANGES_WITHOUT_JGP_COL, row))
            .marketAccess(ImportHandlerUtils.readAsString(MonitoringConstants.MARKET_ACCESS_COL, row))
            .businessOpportunities(ImportHandlerUtils.readAsString(MonitoringConstants.BUSINESS_OPPORTUNITIES_COL, row))
            .marketChallenges(ImportHandlerUtils.readAsString(MonitoringConstants.MARKET_CHALLENGES_COL, row))
            .rowIndex(row.getRowNum()).document(this.document).participantJgpId(jgpId)
            .build();
    }

    /**
     * Processes all chunks of TA data asynchronously.
     * Steps: 1) Validate chunks in parallel, 2) Store to database in parallel, 3) Write results to workbook sequentially
     * @return CompletableFuture containing count of total, success and failure records
     */
    @Async
    public CompletableFuture<Count> processChunks() {
        // Early return if no data to process
        if (monitoringDataList.isEmpty()) {
            log.warn("No TA data to process");
            return CompletableFuture.completedFuture(Count.instance(0, 0, 0));
        }
        final var taDataSize = monitoringDataList.size();

        final var existingParticipants = participantService.findParticipantsByJGPIDs(
                monitoringDataList.stream()
                        .map(OutComeMonitoringRequestDto::participantJgpId)
                        .filter(Objects::nonNull)
                        .distinct()
                        .toList()
        );

        // 1. Split the monitoringDataList into smaller chunks
        final var chunks = Lists.partition(monitoringDataList, CHUNK_SIZE);
        log.info("Processing {} records in {} chunks", monitoringDataList.size(), chunks.size());

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
        setReportHeaders(monitoringSheet, MonitoringConstants.STATUS_COL, MonitoringConstants.FAILURE_COL);

        // Count successes and failures while writing results
        long successCount = allResults.stream().filter(ExcelTemplateProcessingResult::success).count();
        long failureCount = allResults.size() - successCount;

        for (var result : allResults) {
            writeResultToWorkbook(result, MonitoringConstants.STATUS_COL, MonitoringConstants.FAILURE_COL);
            int processedRows = currentStepProgress.incrementAndGet();
            updateProgressInBulk(importProgressService, documentImportProgressUUId, processedRows);
        }
        // Final progress update
        importProgressService.sendProgressUpdate(documentImportProgressUUId, currentStepProgress.get());

        log.info("Finished Import - Total: {}, Success: {}, Failed: {} at {}",
                monitoringDataList.size(), successCount, failureCount, LocalDateTime.now(ZoneId.systemDefault()));

        return CompletableFuture.completedFuture(Count.instance(monitoringDataList.size(), (int) successCount, (int) failureCount));
    }

    /**
     * Associates a participant to the given monitoringDataDto based on the provided participant map.
     *
     * @param monitoringDataDto      the monitoringDataDto
     * @param participantMap a map of participants keyed by their JGP IDs
     * @return the TAData object with the associated participant
     */
    private OutComeMonitoring associateParticipantToMonitoringData(OutComeMonitoringRequestDto monitoringDataDto, Map<String, Participant> participantMap) {
        final var monitoringData = new OutComeMonitoring(monitoringDataDto);
        final var participant = participantMap.get(monitoringDataDto.participantJgpId());
        if (Objects.isNull(participant)) {
            rowErrorMap.put(monitoringDataDto.rowIndex(), PARTICIPANT_ASSOCIATION_ERROR);
        }
        monitoringData.setParticipant(participant);
        return monitoringData;
    }

    /**
     * Validates a single chunk of monitoringDataDto.
     *
     * @param chunk the list of monitoringDataDto to validate
     * @return a list of valid monitoringDataDto
     */
    private List<OutComeMonitoringRequestDto> validateSingleChunk(List<OutComeMonitoringRequestDto> chunk) {
        List<OutComeMonitoringRequestDto> validData = new ArrayList<>();
        for (OutComeMonitoringRequestDto monitoringDto : chunk) {
            try {
                DataValidator.validateMonitoringData(monitoringDto, rowErrorMap);
                validData.add(monitoringDto);
                int processedRows = currentStepProgress.incrementAndGet();
                updateProgressInBulk(importProgressService, documentImportProgressUUId, processedRows);
            } catch (RuntimeException ex) {
                log.error("Problem occurred when validating data: {}", ex.getMessage());
                var errorMessage = ImportHandlerUtils.getErrorMessage(ex);
                rowErrorMap.put(monitoringDto.rowIndex(), errorMessage);
            }
        }
        // Final progress update for the chunk
        importProgressService.sendProgressUpdate(documentImportProgressUUId, currentStepProgress.get());
        return validData;
    }

    private List<ExcelTemplateProcessingResult> storeDataWithoutWritingToWorkbook(List<OutComeMonitoringRequestDto> chunk, Map<String, Participant> participantMap) {
        List<ExcelTemplateProcessingResult> results = new ArrayList<>();
        for (OutComeMonitoringRequestDto monitoringData : chunk) {
            var result = storeSingleDataWithoutWritingToWorkbook(monitoringData, participantMap);
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
    private ExcelTemplateProcessingResult storeSingleDataWithoutWritingToWorkbook(OutComeMonitoringRequestDto monitoringData, Map<String, Participant> participantMap) {
        Row row = monitoringSheet.getRow(monitoringData.rowIndex());

        try {
            final var dataWithParticipant = associateParticipantToMonitoringData(monitoringData, participantMap);
            final var validationError = rowErrorMap.get(row.getRowNum());
            if (Objects.nonNull(validationError)) {
                throw new InvalidDataException(validationError);
            }
            this.outComeMonitoringService.createOutComeMonitoring(dataWithParticipant);
            return new ExcelTemplateProcessingResult(row, true, null);
        } catch (RuntimeException ex) {
            log.error("Problem occurred when uploading Monitoring Data: {}", ex.getMessage());
            var errorMessage = ImportHandlerUtils.getErrorMessage(ex);
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

