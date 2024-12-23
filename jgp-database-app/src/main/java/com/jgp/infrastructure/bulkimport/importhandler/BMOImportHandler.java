package com.jgp.infrastructure.bulkimport.importhandler;

import com.jgp.authentication.service.UserService;
import com.jgp.bmo.domain.BMOParticipantData;
import com.jgp.bmo.service.BMOClientDataService;
import com.jgp.infrastructure.bulkimport.exception.InvalidDataException;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.participant.service.ParticipantService;
import com.jgp.infrastructure.bulkimport.constants.BMOConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.util.CommonUtil;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ExecutionException;

@Service
@Slf4j
@RequiredArgsConstructor
public class BMOImportHandler implements ImportHandler {

    private final BMOClientDataService bmoDataService;
    private final ParticipantService clientService;
    private final UserService userService;
    private final ImportProgressService importProgressService;
    List<BMOParticipantData> bmoDataList;
    private Workbook workbook;
    private List<String> statuses;
    private Map<Row, String> rowErrorMap;
    private Long documentImportId;

    @Override
    public Count process(BulkImportEvent bulkImportEvent) {
        this.workbook = bulkImportEvent.workbook();
        this.bmoDataList = new ArrayList<>();
        this.statuses = new ArrayList<>();
        this.rowErrorMap = new HashMap<>();
        this.documentImportId = bulkImportEvent.importId();
        readExcelFile();
        return importEntity();
    }

    @Override
    public void updateImportProgress(Long importId, boolean updateTotal, int total) {
        try {
            if (updateTotal){
                importProgressService.updateTotal(importId, total);
            }else {
                importProgressService.incrementProcessedProgress(importId);
            }
            var p = importProgressService.getImportProgress(importId);

            log.info("Progress On Update> {}", p.getProcessed());
        } catch (ExecutionException e) {
            log.error("Error : {}", e.getMessage(), e);
        }
    }

    @Override
    public void markImportAsFinished(Long importId) {
        try {
            importProgressService.markImportAsFinished(importId);
        } catch (ExecutionException e) {
            log.error("Error : {}", e.getMessage(), e);
        }
    }


    public void readExcelFile() {
        Sheet bmoSheet = workbook.getSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(bmoSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);
        updateImportProgress(this.documentImportId, true, noOfEntries);

        for (int rowIndex = 1; rowIndex <= noOfEntries; rowIndex++) {
            Row row;
            row = bmoSheet.getRow(rowIndex);
            if (ImportHandlerUtils.isNotImported(row, BMOConstants.STATUS_COL)) {
                bmoDataList.add(readBMOData(row));
            }
        }
    }


    private BMOParticipantData readBMOData(Row row) {
        String status = ImportHandlerUtils.readAsString(BMOConstants.STATUS_COL, row);
        LocalDate appFormSubmittedDate = ImportHandlerUtils.readAsDate(BMOConstants.APPLICATION_FORM_SUBMITTED_DATE_COL, row);
        Boolean isApplicantEligible = "YES".equalsIgnoreCase(ImportHandlerUtils.readAsString(BMOConstants.IS_APPLICANT_ELIGIBLE_COL, row));
        Integer numberOfTAsAttended = ImportHandlerUtils.readAsInt(BMOConstants.NUMBER_TAS_ATTENDED_COL, row);
        Integer taSessionsAttended = ImportHandlerUtils.readAsInt(BMOConstants.NUMBER_TA_SESSION_ATTENDED_COL, row);
        Boolean isRecommendedForFinance = "YES".equalsIgnoreCase(ImportHandlerUtils.readAsString(BMOConstants.RECOMMENDED_FOR_FINANCE_COL, row));
        LocalDate pipelineDecisionDate = ImportHandlerUtils.readAsDate(BMOConstants.DATE_OF_PIPELINE_DECISION_COL, row);
        String referredFIBusiness = ImportHandlerUtils.readAsString(BMOConstants.REFERRED_FI_BUSINESS_COL, row);
        LocalDate dateRecordedByPartner = ImportHandlerUtils.readAsDate(BMOConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL, row);
        final var taNeeds = ImportHandlerUtils.readAsString(BMOConstants.TA_NEEDS_COL, row);
        final var trainingPartner = ImportHandlerUtils.readAsString(BMOConstants.TRAINING_PARTNER, row);
        final var taDeliveryMode = ImportHandlerUtils.readAsString(BMOConstants.TA_DELIVERY_MODE, row);
        final var otherTaNeeds = ImportHandlerUtils.readAsString(BMOConstants.OTHER_TA_NEEDS_COL, row);
        final var taType = ImportHandlerUtils.readAsString(BMOConstants.TYPE_OF_TA_COL, row);

        statuses.add(status);
        final var clientDto = getParticipantDto(row);
        String jgpId = ImportHandlerUtils.readAsString(BMOConstants.JGP_ID_COL, row);
        var existingClient = Optional.<Participant>empty();
        if (null == jgpId){
            rowErrorMap.put(row, "JGP Id is required !!");
        }else {
            existingClient = this.clientService.findOneByJGPID(jgpId);
        }

        final var taData = new BMOParticipantData(Objects.nonNull(userService.currentUser()) ? userService.currentUser().getPartner() : null,
                null,
                appFormSubmittedDate, isApplicantEligible, numberOfTAsAttended,
                taSessionsAttended, isRecommendedForFinance, pipelineDecisionDate,
                referredFIBusiness, dateRecordedByPartner, LocalDate.now(ZoneId.systemDefault()), taNeeds,
                row.getRowNum(), trainingPartner, taDeliveryMode, otherTaNeeds, taType, rowErrorMap.get(row));

        if (null == rowErrorMap.get(row)){
            validateTAData(taData, row);
        }

        validateParticipant(clientDto, row);
        if (existingClient.isEmpty() && null == rowErrorMap.get(row)){
            existingClient = Optional.of(this.clientService.createClient(clientDto));
        }

        existingClient.ifPresent(taData::setParticipant);

        return taData;

    }

    private ParticipantDto getParticipantDto(Row row){
        final var businessName = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_NAME_COL, row);
        final var jgpId = ImportHandlerUtils.readAsString(BMOConstants.JGP_ID_COL, row);
        final var phoneNumber = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_PHONE_NUMBER_COL, row);
        final var gender = ImportHandlerUtils.readAsString(BMOConstants.GENDER_COL, row);
        final var age = ImportHandlerUtils.readAsInt(BMOConstants.AGE_COL, row);
        final var businessLocation = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_LOCATION_COL, row);
        final var locationCountyCode = CommonUtil.KenyanCounty.getKenyanCountyFromName(businessLocation);
        final var industrySector = ImportHandlerUtils.readAsString(BMOConstants.INDUSTRY_SECTOR_COL, row);
        final var businessSegment = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_SEGMENT_COL, row);
        final var registrationNumber = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_REG_NUMBER, row);
        final var bestMonthlyRevenueD = ImportHandlerUtils.readAsDouble(BMOConstants.BEST_MONTH_MONTHLY_REVENUE_COL, row);
        final var bestMonthlyRevenue = Objects.nonNull(bestMonthlyRevenueD) ? BigDecimal.valueOf(bestMonthlyRevenueD) : null;
        final var worstMonthlyRevenueD = ImportHandlerUtils.readAsDouble(BMOConstants.WORST_MONTH_MONTHLY_REVENUE_COL, row);
        final var worstMonthlyRevenue = Objects.nonNull(worstMonthlyRevenueD) ? BigDecimal.valueOf(worstMonthlyRevenueD) : null;
        final var totalRegularEmployees = ImportHandlerUtils.readAsInt(BMOConstants.TOTAL_REGULAR_EMPLOYEES_COL, row);
        final var youthRegularEmployees = ImportHandlerUtils.readAsInt(BMOConstants.YOUTH_REGULAR_EMPLOYEES_COL, row);
        final var totalCasualEmployees = ImportHandlerUtils.readAsInt(BMOConstants.TOTAL_CASUAL_EMPLOYEES_COL, row);
        final var youthCasualEmployees = ImportHandlerUtils.readAsInt(BMOConstants.YOUTH_CASUAL_EMPLOYEES_COL, row);
        final var sampleRecordsKept = ImportHandlerUtils.readAsString(BMOConstants.SAMPLE_RECORDS_KEPT_COL, row);
        final var personWithDisability = ImportHandlerUtils.readAsString(BMOConstants.PERSON_WITH_DISABILITY_COL, row);
        final var refugeeStatus = ImportHandlerUtils.readAsString(BMOConstants.REFUGEE_STATUS_COL, row);

        return ParticipantDto.builder()
                .phoneNumber(phoneNumber).bestMonthlyRevenue(bestMonthlyRevenue).bmoMembership(null)
                .hasBMOMembership(Boolean.TRUE).businessLocation(businessLocation).businessName(businessName)
                .ownerGender(gender).ownerAge(age).industrySector(industrySector).businessSegment(businessSegment)
                .registrationNumber(registrationNumber).isBusinessRegistered(null != registrationNumber)
                .worstMonthlyRevenue(worstMonthlyRevenue).totalRegularEmployees(totalRegularEmployees)
                .youthRegularEmployees(youthRegularEmployees).totalCasualEmployees(totalCasualEmployees)
                .youthCasualEmployees(youthCasualEmployees).sampleRecords(sampleRecordsKept)
                .personWithDisability(personWithDisability).refugeeStatus(refugeeStatus).jgpId(jgpId)
                .locationCountyCode(locationCountyCode.isPresent() ? locationCountyCode.get().getCountyCode() : "999").build();

    }

    public Count importEntity() {
        Sheet groupSheet = workbook.getSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        int successCount = 0;
        int errorCount = 0;
        int progressLevel = 0;
        String errorMessage = "";
        var bmoDataSize = bmoDataList.size();
        for (int i = 0; i < bmoDataSize; i++) {
            Row row = groupSheet.getRow(bmoDataList.get(i).getRowIndex());
            Cell errorReportCell = row.createCell(BMOConstants.FAILURE_COL);
            Cell statusCell = row.createCell(BMOConstants.STATUS_COL);
            try {
                String status = statuses.get(i);
                progressLevel = getProgressLevel(status);
                final var validationError = rowErrorMap.get(row);
                if (null != validationError){
                    throw new InvalidDataException(validationError);
                }
                if (progressLevel == 0) {
                    this.bmoDataService.createBMOData(List.of(bmoDataList.get(i)));
                    progressLevel = 1;
                }
                statusCell.setCellValue(TemplatePopulateImportConstants.STATUS_CELL_IMPORTED);
                statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.LIGHT_GREEN));
                successCount++;
            } catch (RuntimeException ex) {
                errorCount++;
                //log.error("Problem occurred in importEntity function", ex);
                errorMessage = ImportHandlerUtils.getErrorMessage(ex);
                writeGroupErrorMessage(errorMessage, progressLevel, statusCell, errorReportCell);
            }
            updateImportProgress(this.documentImportId, false, 0);
        }
        setReportHeaders(groupSheet);
        log.info("Finished Import Finished := {}", LocalDateTime.now(ZoneId.systemDefault()));
        markImportAsFinished(this.documentImportId);
        return Count.instance(successCount, errorCount);
    }

    private void writeGroupErrorMessage(String errorMessage, int progressLevel, Cell statusCell, Cell errorReportCell) {
        String status = "";
        if (progressLevel == 0) {
            status = TemplatePopulateImportConstants.STATUS_CREATION_FAILED;
        } else if (progressLevel == 1) {
            status = TemplatePopulateImportConstants.STATUS_MEETING_FAILED;
        }
        statusCell.setCellValue(status);
        statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.RED));
        errorReportCell.setCellValue(errorMessage);
    }

    private void setReportHeaders(Sheet bmpSheet) {
        ImportHandlerUtils.writeString(BMOConstants.STATUS_COL, bmpSheet.getRow(TemplatePopulateImportConstants.ROWHEADER_INDEX),
                TemplatePopulateImportConstants.STATUS_COL_REPORT_HEADER);
        ImportHandlerUtils.writeString(BMOConstants.FAILURE_COL, bmpSheet.getRow(TemplatePopulateImportConstants.ROWHEADER_INDEX),
                TemplatePopulateImportConstants.FAILURE_COL_REPORT_HEADER);
    }


    private int getProgressLevel(String status) {
        if (status == null || status.equals(TemplatePopulateImportConstants.STATUS_CREATION_FAILED)) {
            return 0;
        } else if (status.equals(TemplatePopulateImportConstants.STATUS_MEETING_FAILED)) {
            return 1;
        }
        return 0;
    }


    private void validateParticipant(ParticipantDto participantDto, Row row) {
        // Create a Validator instance
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        Validator validator = factory.getValidator();

        // Validate the object
        Set<ConstraintViolation<ParticipantDto>> violations = validator.validate(participantDto);

        // Get the first error, if any
        if (!violations.isEmpty()) {
            ConstraintViolation<ParticipantDto> firstViolation = violations.iterator().next();
            rowErrorMap.put(row, firstViolation.getMessage());
        }

        if (null == rowErrorMap.get(row)){
            if (!CommonUtil.isStringValueLengthValid(participantDto.jgpId(), 5, 10)){
                rowErrorMap.put(row, "JGP ID must be 5-10 characters !!");
            }
            if (!CommonUtil.isStringValueLengthValid(participantDto.phoneNumber(), 9, 12)){
                rowErrorMap.put(row, "JGP ID must be 5-10 characters !!");
            }
        }
    }

    private void validateTAData(BMOParticipantData bmoParticipantData, Row row) {
        // Create a Validator instance
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        Validator validator = factory.getValidator();

        // Validate the object
        Set<ConstraintViolation<BMOParticipantData>> violations = validator.validate(bmoParticipantData);

        // Get the first error, if any
        if (!violations.isEmpty()) {
            ConstraintViolation<BMOParticipantData> firstViolation = violations.iterator().next();
            rowErrorMap.put(row, firstViolation.getMessage());
        }
    }
}
