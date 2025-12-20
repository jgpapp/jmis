package com.jgp.infrastructure.bulkimport.importhandler;

import com.google.common.collect.Lists;
import com.jgp.authentication.service.UserService;
import com.jgp.bmo.domain.TAData;
import com.jgp.bmo.service.TADataService;
import com.jgp.infrastructure.bulkimport.exception.InvalidDataException;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.participant.service.ParticipantService;
import com.jgp.infrastructure.bulkimport.constants.BMOConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.shared.validator.DataValidator;
import com.jgp.shared.validator.ParticipantValidator;
import com.jgp.shared.validator.TAValidator;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.IndexedColors;
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@Service
@Slf4j
@RequiredArgsConstructor
public class TAImportHandler implements ImportHandler {

    private static final String YES = "YES";
    private static final String PARTICIPANT_ASSOCIATION_ERROR = "Cannot associate data to a participant!";
    private static final String DUPLICATE_ENTRY_ERROR = "unique_bmo_participant_data";
    private static final String DUPLICATE_ENTRY_MESSAGE = "Row with same partner/participant/training date already exists!";

    private final TADataService taDataService;
    private final ParticipantService participantService;
    private final UserService userService;
    private final ImportProgressService importProgressService;

    private List<TAData> taDataList;
    private Workbook workbook;
    private Map<Row, String> rowErrorMap;
    private String documentImportProgressUUId;
    private Boolean updateParticipantInfo;
    private Document document;

    @Override
    public CompletableFuture<Count> process(BulkImportEvent bulkImportEvent) {
        this.workbook = bulkImportEvent.workbook();
        this.taDataList = new ArrayList<>();
        this.rowErrorMap = new HashMap<>();
        this.documentImportProgressUUId = bulkImportEvent.importProgressUUID();
        this.updateParticipantInfo = bulkImportEvent.updateParticipantInfo();
        this.document = bulkImportEvent.document();
        readExcelFile();
        return processChunks();
    }


    public void readExcelFile() {
        Sheet taSheet = workbook.getSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(taSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);

        importProgressService.updateTotal(documentImportProgressUUId, noOfEntries);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_READING_STEP);
        boolean headerSkipped = false;
        for (Row row : taSheet) {
            // Skip header row
            if (!headerSkipped) {
                headerSkipped = true;
                continue;
            }

            if (Objects.nonNull(row) && ImportHandlerUtils.isNotImported(row, BMOConstants.STATUS_COL)) {
                taDataList.add(readTAData(row));
                importProgressService.incrementAndSendProgressUpdate(documentImportProgressUUId);
            }
        }
    }


    private TAData readTAData(Row row) {
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
        final String normalizedTaNeeds = Objects.nonNull(taNeeds)
                ? Arrays.stream(taNeeds.split(","))
                        .map(String::trim)
                        .collect(Collectors.joining(","))
                : null;

        return new TAData(
                Objects.nonNull(userService.currentUser()) ? userService.currentUser().getPartner() : null,
                null,
                currentDate,
                isApplicantEligible,
                0,
                0,
                isRecommendedForFinance,
                pipelineDecisionDate,
                referredFIBusiness,
                dateRecordedByPartner,
                currentDate,
                normalizedTaNeeds,
                trainingPartner,
                taDeliveryMode,
                otherTaNeeds,
                taType,
                userService.currentUser(),
                document,
                row,
                rowErrorMap,
                row.getRowNum(),
                rowErrorMap.get(row),
                getParticipantDto(row)
        );
    }

    private ParticipantDto getParticipantDto(Row row) {
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

        return ParticipantDto.builder()
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
                .sampleRecords(Objects.nonNull(sampleRecordsKept)
                        ? Arrays.stream(sampleRecordsKept.split(",")).map(String::trim).toList()
                        : null)
                .personWithDisability(convertYesNoToCapitalized(personWithDisability))
                .refugeeStatus(convertYesNoToCapitalized(refugeeStatus))
                .jgpId(jgpId)
                .locationCountyCode(locationCounty.getCountyCode())
                .businessRegNumber(registrationNumber)
                .row(row)
                .rowErrorMap(rowErrorMap)
                .build();
    }

    private String convertYesNoToCapitalized(String value) {
        return YES.equalsIgnoreCase(value) ? "Yes" : "No";
    }

    @Async
    public CompletableFuture<Count> processChunks() {
        final var existingParticipants = participantService.findParticipantsByJGPIDs(
                taDataList.stream()
                        .map(TAData::getParticipantDto)
                        .map(ParticipantDto::jgpId)
                        .filter(Objects::nonNull)
                        .distinct()
                        .toList()
        );

        // 1. Split the taDataList into smaller chunks
        final var chunks = Lists.partition(taDataList, CHUNK_SIZE);

        // 2. Validate each chunk asynchronously
        importProgressService.resetEveryThingToZero(documentImportProgressUUId);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_VALIDATING_STEP);

        final var validatedFutures = chunks.stream()
                .map(chunk -> CompletableFuture.supplyAsync(() -> validateSingleChunk(chunk)))
                .toList();

        CompletableFuture.allOf(validatedFutures.toArray(new CompletableFuture[0])).join();

        importProgressService.resetEveryThingToZero(documentImportProgressUUId);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_STORING_STEP);

        // 3. Create or update participants and associate them with TA data
        final var allParticipantsToUpdate = createOrUpdateParticipants(existingParticipants);
        final var participantMap = allParticipantsToUpdate.stream()
                .collect(Collectors.toMap(Participant::getJgpId, p -> p));

        final var associateFutures = chunks.stream()
                .map(chunk -> CompletableFuture.supplyAsync(() -> chunk.stream()
                        .map(taData -> associateParticipantToTAData(taData, participantMap))
                        .collect(Collectors.toList())))
                .toList();

        final var validatedAndAssociatedFutures = associateFutures.stream()
                .map(CompletableFuture::join)
                .flatMap(List::stream)
                .toList();

        taDataService.createBMOData(validatedAndAssociatedFutures);

        importProgressService.resetEveryThingToZero(documentImportProgressUUId);
        importProgressService.updateStepAndSendProgress(documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_STATUS_STEP);

        chunks.forEach(chunk -> chunk.forEach(this::saveTAData));

        finish();

        long successCount = taDataList.stream()
                .filter(taData -> rowErrorMap.get(taData.getRow()) == null && taData.getParticipant() != null)
                .count();
        long errorCount = taDataList.size() - successCount;

        return CompletableFuture.completedFuture(Count.instance(taDataList.size(), (int) successCount, (int) errorCount));
    }

    private TAData associateParticipantToTAData(TAData taData, Map<String, Participant> participantMap) {
        final var participantDto = taData.getParticipantDto();
        taData.setParticipant(participantMap.get(participantDto.jgpId()));
        return taData;
    }

    private List<Participant> createOrUpdateParticipants(Map<String, Participant> existingParticipants) {
        final var allAffectedParticipants = new ArrayList<Participant>();

        if (Boolean.TRUE.equals(updateParticipantInfo)) {
            final var participantsToUpdateMap = taDataList
                    .stream()
                    .filter(ta -> Objects.isNull(ta.getRowErrorMap().get(ta.getRow())))
                    .map(TAData::getParticipantDto)
                    .filter(p -> p.jgpId() != null && existingParticipants.containsKey(p.jgpId()))
                    .collect(Collectors.toMap(
                            p -> existingParticipants.get(p.jgpId()).getId(),
                            p -> p,
                            (p1, p2) -> p1
                    ));
            allAffectedParticipants.addAll(participantService.updateParticipants(participantsToUpdateMap));
        }

        final var participantsToCreate = taDataList
                .stream()
                .filter(ta -> Objects.isNull(ta.getRowErrorMap().get(ta.getRow())))
                .map(TAData::getParticipantDto)
                .filter(p -> p.jgpId() == null || !existingParticipants.containsKey(p.jgpId()))
                .toList();
        allAffectedParticipants.addAll(participantService.createParticipants(participantsToCreate));

        return allAffectedParticipants;
    }

    /**
     * Validates a single chunk of TAData.
     *
     * @param chunk the list of TAData to validate
     * @return a list of valid TAData
     */
    private List<TAData> validateSingleChunk(List<TAData> chunk) {
        List<TAData> validData = new ArrayList<>();
        for (TAData taData : chunk) {
            Row row = taData.getRow();
            final var participantDto = taData.getParticipantDto();
            try {
                final var validator = DataValidator.getValidator();
                ParticipantValidator.validateParticipant(participantDto, validator);
                TAValidator.validateTAData(taData, validator);
                validData.add(taData);
            } catch (RuntimeException ex) {
                log.error("Problem occurred when validating participant: {}", ex.getMessage());
                var errorMessage = ImportHandlerUtils.getErrorMessage(ex);
                rowErrorMap.put(row, errorMessage);
            } finally {
                importProgressService.incrementAndSendProgressUpdate(documentImportProgressUUId);
            }
        }
        return validData;
    }


    public void finish() {
        Sheet taSheet = workbook.getSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        setReportHeaders(taSheet, BMOConstants.STATUS_COL, BMOConstants.FAILURE_COL);
        log.info("Finished Import := {}", LocalDateTime.now(ZoneId.systemDefault()));
    }

    private void saveTAData(TAData taData) {
        Row row = taData.getRow();
        Cell errorReportCell = row.createCell(BMOConstants.FAILURE_COL);
        Cell statusCell = row.createCell(BMOConstants.STATUS_COL);

        if (Objects.isNull(rowErrorMap.get(row)) && Objects.isNull(taData.getParticipant())) {
            rowErrorMap.put(row, PARTICIPANT_ASSOCIATION_ERROR);
        }

        try {
            final var validationError = rowErrorMap.get(row);
            if (Objects.nonNull(validationError)) {
                throw new InvalidDataException(validationError);
            }
            statusCell.setCellValue(TemplatePopulateImportConstants.STATUS_CELL_IMPORTED);
            statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.LIGHT_GREEN));
        } catch (RuntimeException ex) {
            log.error("Problem occurred when uploading TA: {}", ex.getMessage());
            var errorMessage = ImportHandlerUtils.getErrorMessage(ex);
            if (errorMessage.contains(DUPLICATE_ENTRY_ERROR)) {
                errorMessage = DUPLICATE_ENTRY_MESSAGE;
            }
            writeGroupErrorMessage(errorMessage, workbook, statusCell, errorReportCell);
        } finally {
            importProgressService.incrementAndSendProgressUpdate(documentImportProgressUUId);
        }
    }

}
