package com.jgp.infrastructure.bulkimport.importhandler;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.jgp.authentication.service.UserService;
import com.jgp.infrastructure.bulkimport.constants.MentorShipConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.data.MonitoringConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.exception.InvalidDataException;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.monitoring.domain.OutComeMonitoring;
import com.jgp.monitoring.dto.OutComeMonitoringDto;
import com.jgp.monitoring.service.OutComeMonitoringService;
import com.jgp.participant.service.ParticipantService;
import com.jgp.shared.validator.DataValidator;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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
import java.util.concurrent.ExecutionException;

@Service
@Slf4j
@RequiredArgsConstructor
public class MonitoringImportHandler implements ImportHandler {
    private final ImportProgressService importProgressService;
    private final ParticipantService participantService;
    private final OutComeMonitoringService outComeMonitoringService;
    private final UserService userService;
    private Workbook workbook;
    private List<OutComeMonitoring> monitoringDataList;
    private Map<Row, String> rowErrorMap;
    private String documentImportProgressUUId;
    private List<String> statuses;

    @Override
    public void updateImportProgress(String importId, boolean updateTotal, int total) {
        try {
            if (updateTotal){
                importProgressService.updateTotal(importId, total);
            }else {
                importProgressService.incrementProcessedProgress(importId);
            }
        } catch (ExecutionException e) {
            log.error("Error : {}", e.getMessage(), e);
        }
    }

    @Override
    public Count process(BulkImportEvent bulkImportEvent) {
        this.workbook = bulkImportEvent.workbook();
        this.monitoringDataList = new ArrayList<>();
        this.rowErrorMap = new HashMap<>();
        statuses = new ArrayList<>();
        this.documentImportProgressUUId = bulkImportEvent.importProgressUUID();
        readExcelFile();
        return importEntity();
    }

    public void readExcelFile() {
        Sheet sheet = workbook.getSheet("Monitoring");
        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(sheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);
        for (int rowIndex = 1; rowIndex <= noOfEntries; rowIndex++) {
            Row row = sheet.getRow(rowIndex);
            if (row != null && ImportHandlerUtils.isNotImported(row, MonitoringConstants.STATUS_COL)) {
                monitoringDataList.add(readMonitoringData(row));
            }
        }
        updateImportProgress(this.documentImportProgressUUId, true, monitoringDataList.size());
    }

    private OutComeMonitoring readMonitoringData(Row row) {
        final var status = ImportHandlerUtils.readAsString(MentorShipConstants.STATUS_COL, row);
        final var jgpID = ImportHandlerUtils.readAsString(MonitoringConstants.JGP_ID_COL, row);
        final var participant = (null == jgpID ? null : this.participantService.findOneParticipantByJGPID(jgpID).orElse(null));
        final var locationLatDouble = DataValidator.validateTemplateDoubleValue(MonitoringConstants.LOCATION_LATITUDE_COL, row, rowErrorMap);
        final var locationLangDouble = DataValidator.validateTemplateDoubleValue(MonitoringConstants.LOCATION_LONGITUDE_COL, row, rowErrorMap);
        final var revenueChangeDouble = DataValidator.validateTemplateDoubleValue(MonitoringConstants.REVENUE_CHANGE_COL, row, rowErrorMap);
        final var monitoringDto = OutComeMonitoringDto.builder()
            .surveyDate(ImportHandlerUtils.readAsDate(MonitoringConstants.SURVEY_DATE_COL, row))
            .surveyLanguage(ImportHandlerUtils.readAsString(MonitoringConstants.SURVEY_LANGUAGE_COL, row))
            .consented("YES".equalsIgnoreCase(ImportHandlerUtils.readAsString(MonitoringConstants.CONSENTED_COL, row)))
            .locationLatitude(locationLatDouble == null ? null : BigDecimal.valueOf(locationLatDouble))
            .locationLongitude(locationLangDouble == null ? null : BigDecimal.valueOf(locationLangDouble))
            .age(ImportHandlerUtils.readAsInt(MonitoringConstants.AGE_COL, row))
            .genderCategory(ImportHandlerUtils.readAsString(MonitoringConstants.GENDER_CATEGORY_COL, row))
            .segment(ImportHandlerUtils.readAsString(MonitoringConstants.SEGMENT_COL, row))
            .partner(ImportHandlerUtils.readAsString(MonitoringConstants.PARTNER_COL, row))
            .gender(ImportHandlerUtils.readAsString(MonitoringConstants.GENDER_COL, row))
            .region(ImportHandlerUtils.readAsString(MonitoringConstants.REGION_COL, row))
            .countyCode(ImportHandlerUtils.readAsString(MonitoringConstants.COUNTY_CODE_COL, row))
            .countyName(ImportHandlerUtils.readAsString(MonitoringConstants.COUNTY_NAME_COL, row))
            .businessSetting(ImportHandlerUtils.readAsString(MonitoringConstants.BUSINESS_SETTING_COL, row))
            .businessAgeCategory(ImportHandlerUtils.readAsString(MonitoringConstants.BUSINESS_AGE_CATEGORY_COL, row))
            .groupMembership(ImportHandlerUtils.readAsString(MonitoringConstants.GROUP_MEMBERSHIP_COL, row))
            .educationLevel(ImportHandlerUtils.readAsString(MonitoringConstants.EDUCATION_LEVEL_COL, row))
            .businessAge(ImportHandlerUtils.readAsInt(MonitoringConstants.BUSINESS_AGE_COL, row))
            .regularEmployees(ImportHandlerUtils.readAsInt(MonitoringConstants.REGULAR_EMPLOYEES_COL, row))
            .casualEmployees(ImportHandlerUtils.readAsInt(MonitoringConstants.CASUAL_EMPLOYEES_COL, row))
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
            .numberOfLoans(ImportHandlerUtils.readAsInt(MonitoringConstants.NUMBER_OF_LOANS_COL, row))
            .loanPlatform(ImportHandlerUtils.readAsString(MonitoringConstants.LOAN_PLATFORM_COL, row))
            .externalFinancing(ImportHandlerUtils.readAsString(MonitoringConstants.EXTERNAL_FINANCING_COL, row))
            .financingSources(ImportHandlerUtils.readAsString(MonitoringConstants.FINANCING_SOURCES_COL, row))
            .jgpImpact(ImportHandlerUtils.readAsString(MonitoringConstants.JGP_IMPACT_COL, row))
            .changesWithoutJgp(ImportHandlerUtils.readAsString(MonitoringConstants.CHANGES_WITHOUT_JGP_COL, row))
            .marketAccess(ImportHandlerUtils.readAsString(MonitoringConstants.MARKET_ACCESS_COL, row))
            .businessOpportunities(ImportHandlerUtils.readAsString(MonitoringConstants.BUSINESS_OPPORTUNITIES_COL, row))
            .marketChallenges(ImportHandlerUtils.readAsString(MonitoringConstants.MARKET_CHALLENGES_COL, row))
            .build();
        var monitoring = new OutComeMonitoring(monitoringDto, participant, row.getRowNum());
        monitoring.setCreatedBy(userService.currentUser());
        statuses.add(status);
        return monitoring;
    }

    private Count importEntity() {
        Sheet monitoringSheet = workbook.getSheet(TemplatePopulateImportConstants.MONITORING_SHEET_NAME);
        int successCount = 0;
        int errorCount = 0;
        int progressLevel = 0;
        String errorMessage = "";
        var loanDataSize = monitoringDataList.size();
        for (int i = 0; i < loanDataSize; i++) {
            final var monitoringData = monitoringDataList.get(i);
            Row row = monitoringSheet.getRow(monitoringData.getRowIndex());
            Cell errorReportCell = row.createCell(MentorShipConstants.FAILURE_COL);
            Cell statusCell = row.createCell(MentorShipConstants.STATUS_COL);
            if (null == rowErrorMap.get(row) && Objects.isNull(monitoringData.getParticipant())){
                rowErrorMap.put(row, "Can not associate mentorship Data data to a participant !!");
            }
            try {
                String status = statuses.get(i);
                progressLevel = getProgressLevel(status);

                final var validationError = rowErrorMap.get(row);
                if (null != validationError){
                    throw new InvalidDataException(validationError);
                }
                if (progressLevel == 0) {
                    this.outComeMonitoringService.createOutComeMonitoring(monitoringData);
                    progressLevel = 1;
                }
                statusCell.setCellValue(TemplatePopulateImportConstants.STATUS_CELL_IMPORTED);
                statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.LIGHT_GREEN));
                successCount++;
            } catch (RuntimeException ex) {
                errorCount++;
                log.error("Problem occurred When Uploading Monitoring Data: {}", ex.getMessage());
                errorMessage = ImportHandlerUtils.getErrorMessage(ex);
                if (errorMessage.contains("unique_loan") || errorMessage.contains("Duplicate Disbursement On Same Day")){
                    errorMessage = "Row with same partner/participant/disburse date already exist !!";
                }
                writeGroupErrorMessage(errorMessage, progressLevel, statusCell, errorReportCell);
            }finally {
                updateImportProgress(this.documentImportProgressUUId, false, 0);
                try {
                    this.importProgressService.sendProgressUpdate(this.documentImportProgressUUId);
                } catch (JsonProcessingException | ExecutionException e) {
                    log.error("Problem Updating Progress: {}", e.getMessage());
                }
            }
        }
        setReportHeaders(monitoringSheet);
        log.info("Finished Import Finished := {}", LocalDateTime.now(ZoneId.systemDefault()));
        return Count.instance(loanDataSize, successCount, errorCount);
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
        ImportHandlerUtils.writeString(MentorShipConstants.STATUS_COL, bmpSheet.getRow(TemplatePopulateImportConstants.ROWHEADER_INDEX),
                TemplatePopulateImportConstants.STATUS_COL_REPORT_HEADER);
        ImportHandlerUtils.writeString(MentorShipConstants.FAILURE_COL, bmpSheet.getRow(TemplatePopulateImportConstants.ROWHEADER_INDEX),
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
}

