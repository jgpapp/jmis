package com.jgp.infrastructure.bulkimport.importhandler;

import com.google.common.collect.Lists;
import com.jgp.authentication.service.UserService;
import com.jgp.bmo.domain.Mentorship;
import com.jgp.bmo.dto.MentorshipRequestDto;
import com.jgp.bmo.dto.TARequestDto;
import com.jgp.bmo.service.MentorshipService;
import com.jgp.infrastructure.bulkimport.constants.BMOConstants;
import com.jgp.infrastructure.bulkimport.constants.MentorShipConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.data.ExcelTemplateProcessingResult;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.exception.InvalidDataException;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.participant.dto.ParticipantRequestDto;
import com.jgp.participant.service.ParticipantService;
import com.jgp.shared.validator.DataValidator;
import com.jgp.shared.validator.ParticipantValidator;
import com.jgp.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

@Service
@Slf4j
@RequiredArgsConstructor
public class MentorshipImportHandlerV2 implements ImportHandler {
    private final ImportProgressService importProgressService;
    private final MentorshipService mentorshipService;
    private final ParticipantService participantService;
    private final UserService userService;
    List<Mentorship> mentorshipDataList;
    private Workbook workbook;
    private Map<Integer, String> rowErrorMap;
    private Map<Long, ParticipantRequestDto> participantDtoMap;
    private String documentImportProgressUUId;
    private Boolean updateParticipantInfo;
    private Document document;
    private static final String OTHER = "OTHER";
    private final AtomicInteger currentStepProgress = new AtomicInteger(0);


    @Override
    public CompletableFuture<Count> process(BulkImportEvent bulkImportEvent) {
        log.info("Starting Mentorship import process for document: {}", bulkImportEvent.document().getId());
        this.workbook = bulkImportEvent.workbook();
        this.mentorshipDataList = new ArrayList<>();
        this.rowErrorMap = new ConcurrentHashMap<>();
        this.documentImportProgressUUId = bulkImportEvent.importProgressUUID();
        this.updateParticipantInfo = bulkImportEvent.updateParticipantInfo();
        this.document = bulkImportEvent.document();
        readExcelFile();
        return processChunks();
    }

    public void saveMentorshipData(Mentorship mentorship) {
        this.mentorshipService.saveMentorshipWithParticipant(mentorship, this.updateParticipantInfo, this.participantDtoMap);
    }

    public void readExcelFile() {
        Sheet mentorshipSheet = workbook.getSheet(TemplatePopulateImportConstants.MENTOR_SHIP_SHEET_NAME);
        if (mentorshipSheet == null) {
            log.error("Sheet '{}' not found in workbook", TemplatePopulateImportConstants.MENTOR_SHIP_SHEET_NAME);
            throw new InvalidDataException("Required sheet not found: " + TemplatePopulateImportConstants.MENTOR_SHIP_SHEET_NAME);
        }

        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(mentorshipSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);
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

        for (Row row : mentorshipSheet) {
            // Skip header row
            if (!headerSkipped) {
                headerSkipped = true;
                continue;
            }

            if (row != null && ImportHandlerUtils.isNotImported(row, MentorShipConstants.STATUS_COL)) {
                try {
                    mentorshipDataList.add(readMentorShipData(row));
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


    private Mentorship readMentorShipData(Row row) {
        final var mentorShipDate = DataValidator.validateLocalDate(MentorShipConstants.MENTORSHIP_DATE_COL, row, rowErrorMap, "Mentorship Date", true);
        final var mentorShipOrg = ImportHandlerUtils.readAsString(MentorShipConstants.MENTOR_ORGANIZATION_COL, row);
        var bmoMembership = ImportHandlerUtils.readAsString(MentorShipConstants.BMO_MEMBERSHIP_COL, row);
        if (StringUtils.isNotBlank(bmoMembership) && OTHER.equals(bmoMembership.toUpperCase(Locale.ROOT))){
            bmoMembership =ImportHandlerUtils.readAsString(MentorShipConstants.OTHER_BMO_MEMBERSHIP_COL, row);
        }
        if (Objects.isNull(bmoMembership)){
            rowErrorMap.put(row.getRowNum(), "BMO Membership is required !!");
        }
        final var deliveryMode = ImportHandlerUtils.readAsString(MentorShipConstants.MENTORSHIP_DELIVERY_MODE_COL, row);
        final var jgpID = ImportHandlerUtils.readAsString(MentorShipConstants.JGP_ID_COL, row);
        final var participant = null == jgpID ? null : this.participantService.findOneParticipantByJGPID(jgpID).orElse(null);
        if (Objects.isNull(participant)){
            rowErrorMap.put(row.getRowNum(), "Participant can not be found by Id");
        }
        final var businessSituation = ImportHandlerUtils.readAsString(MentorShipConstants.BUSINESS_SITUATION_COL, row);
        if (null == rowErrorMap.get(row.getRowNum()) && null == businessSituation){
            rowErrorMap.put(row.getRowNum(), "Business Situation is required !!");
        }
        final var didHireMoreEmployees = ImportHandlerUtils.readAsString(MentorShipConstants.LOAN_MADE_HIRE_MORE_COL, row);
        Integer numberOfMoreEmployees = null;
        if ("YES".equalsIgnoreCase(didHireMoreEmployees)){
            numberOfMoreEmployees = DataValidator.validateTemplateIntegerValue(MentorShipConstants.NEW_EMPLOYEES_18_35_COL, row, "number of new employees", rowErrorMap, true);
            if ((null == numberOfMoreEmployees || numberOfMoreEmployees < 1) && null == rowErrorMap.get(row.getRowNum())){
                rowErrorMap.put(row.getRowNum(), "New hires 18-35 must be greater than 0");
            }
        }

        final var didRevenueIncrease = ImportHandlerUtils.readAsString(MentorShipConstants.DID_TRAINING_CONTRIBUTE_TO_REVENUE_COL, row);
        Double revenueIncreaseDouble = null;
        if ("YES".equalsIgnoreCase(didRevenueIncrease)){
            revenueIncreaseDouble = DataValidator.validateTemplateDoubleValue(MentorShipConstants.REVENUE_INCREASE_PERCENT_COL, row, "revenue increase", rowErrorMap, true);
            if ((null == revenueIncreaseDouble || revenueIncreaseDouble < 1) &&  null == rowErrorMap.get(row.getRowNum())){
                rowErrorMap.put(row.getRowNum(), "Revenue increase must be greater than 0");
            }
        }

        final var revenueIncrease = null == revenueIncreaseDouble ? BigDecimal.ZERO : BigDecimal.valueOf(revenueIncreaseDouble);

        var usefulTopics = ImportHandlerUtils.readAsString(MentorShipConstants.USEFUL_TRAINING_TOPICS_COL, row);
        if (StringUtils.isNotBlank(usefulTopics) && usefulTopics.toUpperCase(Locale.ROOT).contains(OTHER)){
            var otherUsefulTopic = ImportHandlerUtils.readAsString(MentorShipConstants.OTHER_USEFUL_TRAINING_TOPICS_COL, row);
            usefulTopics = null == otherUsefulTopic ? usefulTopics : usefulTopics.concat(",").concat(otherUsefulTopic);
        }

        var areasNeedingSupport = ImportHandlerUtils.readAsString(MentorShipConstants.AREAS_NEEDING_SUPPORT, row);
        if (StringUtils.isNotBlank(areasNeedingSupport) && areasNeedingSupport.toUpperCase(Locale.ROOT).contains(OTHER)){
            var otherAreasNeedingSupport = ImportHandlerUtils.readAsString(MentorShipConstants.OTHER_AREA_NEEDING_SUPPORT, row);
            areasNeedingSupport = null == otherAreasNeedingSupport ? areasNeedingSupport : areasNeedingSupport.concat(",").concat(otherAreasNeedingSupport);
        }

        final var msmeCovered = ImportHandlerUtils.readAsString(MentorShipConstants.MSME_SESSIONS_COVERED_COL, row);
        final var smeCovered = ImportHandlerUtils.readAsString(MentorShipConstants.SME_SESSIONS_COVERED_COL, row);
        final var businessGaps = ImportHandlerUtils.readAsString(MentorShipConstants.IDENTIFIED_BUSINESS_GAPS, row);
        if (null == businessGaps){
            rowErrorMap.put(row.getRowNum(), "Business Gaps must be provided!!");
        } else if (businessGaps.split(",").length < 3) {
            rowErrorMap.put(row.getRowNum(), "At least 3 Business Gaps must be provided !!");
        }
        final var businessGapsAgreedAction = ImportHandlerUtils.readAsString(MentorShipConstants.AGREED_ACTION_FOR_GAP_1, row);
        final var additionalSupport = ImportHandlerUtils.readAsString(MentorShipConstants.ADDITIONAL_SUPPORT_NEEDED, row);

        var mentorShipData = MentorshipRequestDto.builder()
                .mentorShipDate(mentorShipDate).mentorShipOrganization(mentorShipOrg).bmoMemberShip(bmoMembership)
                .mentorShipDeliveryMode(deliveryMode).businessSituation(businessSituation)
                .newHiresBecauseOfLoan(Objects.nonNull(numberOfMoreEmployees) ? numberOfMoreEmployees : 0)
                .revenueIncreaseDueToTraining(revenueIncrease).usefulTrainingTopics(usefulTopics).supportNeededAreas(areasNeedingSupport)
                .msmeSessionsCovered(msmeCovered).smeSessionsCovered(smeCovered).identifiedBusinessGaps(String.join(",", businessGaps))
                .agreedActionForGapOne(businessGapsAgreedAction).additionalNeededSupport(additionalSupport)
                .build();


        if (Objects.nonNull(participant)) {
            this.participantDtoMap.put(participant.getId(), getParticipantDto(row));
        }


        return new Mentorship(Objects.nonNull(userService.currentUser()) ? userService.currentUser().getPartner() : null,
                participant, this.document, row.getRowNum(), userService.currentUser(), mentorShipData);
    }

    private ParticipantRequestDto getParticipantDto(Row row){
        final var participantName = ImportHandlerUtils.readAsString(MentorShipConstants.PARTICIPANT_NAME_COL, row);
        if (null == participantName && null == rowErrorMap.get(row.getRowNum())){
            rowErrorMap.put(row.getRowNum(), "Participant Name Is Required !!");
        }
        final var businessName = ImportHandlerUtils.readAsString(MentorShipConstants.BUSINESS_NAME_COL, row);

        final var jgpId = ImportHandlerUtils.readAsString(MentorShipConstants.JGP_ID_COL, row);
        final var phoneNumber = ImportHandlerUtils.readAsString(MentorShipConstants.BUSINESS_PHONE_NUMBER_COL, row);
        if (null == phoneNumber && null == rowErrorMap.get(row.getRowNum())){
            rowErrorMap.put(row.getRowNum(), "Phone Number Is Required !!");
        }
        var gender = ImportHandlerUtils.readAsString(MentorShipConstants.MENTEE_GENDER_COL, row);
        gender = ParticipantValidator.validateGender(gender, row, rowErrorMap);
        var age = DataValidator.validateTemplateIntegerValue(MentorShipConstants.MENTEE_AGE_COL, row, "age", rowErrorMap, false);
        age = ParticipantValidator.validateParticipantAge(age, row, rowErrorMap);
        final var personWithDisability = ImportHandlerUtils.readAsString(MentorShipConstants.IS_MENTEE_DISABLED, row);
        ParticipantValidator.validatePersonWithDisability(personWithDisability, row, rowErrorMap);
        String personWithDisabilityType = null;
        if ("YES".equalsIgnoreCase(personWithDisability)){
            personWithDisabilityType = ImportHandlerUtils.readAsString(MentorShipConstants.MENTEE_DISABILITY_TYPE, row);
            ParticipantValidator.validateDisabilityType(personWithDisabilityType, row, rowErrorMap);
        }
        final var financier = ImportHandlerUtils.readAsString(MentorShipConstants.BUSINESS_FINANCIER_COL, row);
        ParticipantValidator.validateFinanciers(financier, row, rowErrorMap);
        var businessLocation = DataValidator.validateCountyName(MentorShipConstants.BUSINESS_COUNTY_LOCATION_COL, row, rowErrorMap);
        if (null == businessLocation && null == rowErrorMap.get(row.getRowNum())){
            rowErrorMap.put(row.getRowNum(), "Business Location Is Required !!");
            businessLocation = CommonUtil.KenyanCounty.UNKNOWN;
        }
        final var businessLocationSubCounty = ImportHandlerUtils.readAsString(MentorShipConstants.BUSINESS_SUB_COUNTY_LOCATION_COL, row);
        if (null == businessLocationSubCounty && null == rowErrorMap.get(row.getRowNum())){
            rowErrorMap.put(row.getRowNum(), "Business Location SubCounty Is Required !!");
        }

        final var businessLocationDoubleLatitude = DataValidator.validateTemplateDoubleValue(MentorShipConstants.GEO_LOCATION_LATITUDE, row, "location latitude", rowErrorMap, false);
        final var businessLocationLatitude = Objects.nonNull(businessLocationDoubleLatitude) ? BigDecimal.valueOf(businessLocationDoubleLatitude) : null;
        final var businessLocationDoubleLongitude = DataValidator.validateTemplateDoubleValue(MentorShipConstants.GEO_LOCATION_LONGITUDE, row, "location longitude", rowErrorMap, Objects.nonNull(businessLocationDoubleLatitude));
        final var businessLocationLongitude = Objects.nonNull(businessLocationDoubleLongitude) ? BigDecimal.valueOf(businessLocationDoubleLongitude) : null;

        var industrySector = ImportHandlerUtils.readAsString(MentorShipConstants.BUSINESS_CATEGORY_COL, row);
        if (StringUtils.isNotBlank(industrySector) && OTHER.equals(industrySector.toUpperCase(Locale.ROOT))){
            industrySector =ImportHandlerUtils.readAsString(MentorShipConstants.OTHER_BUSINESS_CATEGORY_COL, row);
        }
        if (null == industrySector && null == rowErrorMap.get(row.getRowNum())){
            rowErrorMap.put(row.getRowNum(), "Business Category Is Required !!");
        }

        final var businessSegment = ImportHandlerUtils.readAsString(MentorShipConstants.BUSINESS_SEGMENT, row);
        if (null == businessSegment && null == rowErrorMap.get(row.getRowNum())){
            rowErrorMap.put(row.getRowNum(), "Business Segment Is Required !!");
        }


        return ParticipantRequestDto.builder()
                .phoneNumber(phoneNumber).businessLocation(null != businessLocation ? businessLocation.getCountyName() : null).businessName(businessName)
                .ownerGender(gender).ownerAge(age).industrySector(industrySector).jgpId(jgpId)
                .locationCountyCode(null != businessLocation ? businessLocation.getCountyCode() : null)
                .locationSubCounty(businessLocationSubCounty).locationLatitude(businessLocationLatitude)
                .locationLongitude(businessLocationLongitude).businessSegment(businessSegment)
                .participantName(participantName).businessFinancier(financier)
                .personWithDisability(personWithDisability).disabilityType(personWithDisabilityType).build();
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


    public Count importEntity() {
        Sheet mentorShipSheet = workbook.getSheet(TemplatePopulateImportConstants.MENTOR_SHIP_SHEET_NAME);
        int successCount = 0;
        int errorCount = 0;
        String errorMessage = "";
        var loanDataSize = mentorshipDataList.size();
        importProgressService.resetEveryThingToZero(this.documentImportProgressUUId);
        for (int i = 0; i < loanDataSize; i++) {
            final var mentorshipData = mentorshipDataList.get(i);
            Row row = mentorShipSheet.getRow(mentorshipData.getRowIndex());
            Cell errorReportCell = row.createCell(MentorShipConstants.FAILURE_COL);
            Cell statusCell = row.createCell(MentorShipConstants.STATUS_COL);
            if (null == rowErrorMap.get(row.getRowNum()) && Objects.isNull(mentorshipData.getParticipant())){
                rowErrorMap.put(row.getRowNum(), "Can not associate mentorship Data data to a participant !!");
            }
            try {

                final var validationError = rowErrorMap.get(row.getRowNum());
                if (null != validationError){
                    throw new InvalidDataException(validationError);
                }
                this.saveMentorshipData(mentorshipData);
                statusCell.setCellValue(TemplatePopulateImportConstants.STATUS_CELL_IMPORTED);
                statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.LIGHT_GREEN));
                successCount++;
            } catch (RuntimeException ex) {
                errorCount++;
                log.error("Problem occurred When Uploading Mentoring Data: {}", ex.getMessage());
                errorMessage = ImportHandlerUtils.getErrorMessage(ex);
                writeGroupErrorMessage(errorMessage, workbook, statusCell, errorReportCell);
            }finally {
                this.importProgressService.sendProgressUpdate(this.documentImportProgressUUId);
            }
        }
        setReportHeaders(mentorShipSheet, MentorShipConstants.STATUS_COL, MentorShipConstants.FAILURE_COL);
        log.info("Finished Import Finished := {}", LocalDateTime.now(ZoneId.systemDefault()));
        return Count.instance(loanDataSize, successCount, errorCount);
    }
}
