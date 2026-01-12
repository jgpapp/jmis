package com.jgp.infrastructure.bulkimport.importhandler;

import com.jgp.authentication.service.UserService;
import com.jgp.bmo.domain.Mentorship;
import com.jgp.bmo.dto.MentorshipRequestDto;
import com.jgp.bmo.service.MentorshipService;
import com.jgp.infrastructure.bulkimport.constants.MentorShipConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
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
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

@Service
@Slf4j
@RequiredArgsConstructor
public class MentorshipImportHandlerV1 implements ImportHandler {
    @Override
    public CompletableFuture<Count> process(BulkImportEvent bulkImportEvent) {
        return null;
    }
    /*private final ImportProgressService importProgressService;
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


    @Override
    public CompletableFuture<Count> process(BulkImportEvent bulkImportEvent) {
        this.workbook = bulkImportEvent.workbook();
        mentorshipDataList = new ArrayList<>();
        this.rowErrorMap = new ConcurrentHashMap<>();
        this.participantDtoMap = new HashMap<>();
        this.documentImportProgressUUId = bulkImportEvent.importProgressUUID();
        this.updateParticipantInfo = bulkImportEvent.updateParticipantInfo();
        this.document = bulkImportEvent.document();
        readExcelFile();
        return CompletableFuture.completedFuture(importEntity());
    }

    public void saveMentorshipData(Mentorship mentorship) {
        this.mentorshipService.saveMentorshipWithParticipant(mentorship, this.updateParticipantInfo, this.participantDtoMap);
    }

    public void readExcelFile() {
        Sheet loanSheet = workbook.getSheet(TemplatePopulateImportConstants.MENTOR_SHIP_SHEET_NAME);
        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(loanSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);
        importProgressService.updateTotal(this.documentImportProgressUUId, noOfEntries);

        this.importProgressService.updateStepAndSendProgress(this.documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_READING_STEP);
        for (int rowIndex = 1; rowIndex <= noOfEntries; rowIndex++) {
            Row row;
            row = loanSheet.getRow(rowIndex);
            if (null != row && ImportHandlerUtils.isNotImported(row, MentorShipConstants.STATUS_COL)) {
                mentorshipDataList.add(readMentorShipData(row));
                this.importProgressService.sendProgressUpdate(this.documentImportProgressUUId);
            }
        }
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
    }*/
}
