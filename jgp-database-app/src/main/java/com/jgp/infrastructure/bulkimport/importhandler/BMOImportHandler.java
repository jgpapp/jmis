package com.jgp.infrastructure.bulkimport.importhandler;

import com.jgp.authentication.service.UserService;
import com.jgp.bmo.domain.BMOParticipantData;
import com.jgp.bmo.service.BMOClientDataService;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.participant.service.ParticipantService;
import com.jgp.infrastructure.bulkimport.constants.BMOConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.util.CommonUtil;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
@Slf4j
@RequiredArgsConstructor
public class BMOImportHandler implements ImportHandler {

    private final BMOClientDataService bmoDataService;
    private final ParticipantService clientService;
    private final UserService userService;
    List<BMOParticipantData> bmoDataList;
    private Workbook workbook;
    private List<String> statuses;

    @Override
    public Count process(BulkImportEvent bulkImportEvent) {
        this.workbook = bulkImportEvent.workbook();
        bmoDataList = new ArrayList<>();
        statuses = new ArrayList<>();
        readExcelFile();
        return importEntity();
    }


    public void readExcelFile() {
        Sheet bmoSheet = workbook.getSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(bmoSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);
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
        LocalDate recordedToJGPDBOnDate = ImportHandlerUtils.readAsDate(BMOConstants.DATE_RECORDED_TO_JGP_DB_COL, row);
        final var taNeeds = ImportHandlerUtils.readAsString(BMOConstants.TA_NEEDS_COL, row);

        statuses.add(status);
        return new BMOParticipantData(Objects.nonNull(userService.currentUser()) ? userService.currentUser().getPartner() : null,
                getParticipant(row),
                appFormSubmittedDate, isApplicantEligible, numberOfTAsAttended,
                taSessionsAttended, isRecommendedForFinance, pipelineDecisionDate,
                referredFIBusiness, dateRecordedByPartner, recordedToJGPDBOnDate, taNeeds, row.getRowNum());
    }

    private Participant getParticipant(Row row){
        String businessName = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_NAME_COL, row);
        String jgpId = ImportHandlerUtils.readAsString(BMOConstants.JGP_ID_COL, row);
        if (null == jgpId){
            return null;
        }
        final var existingClient = this.clientService.findOneByJGPID(jgpId);
        if (existingClient.isPresent()){
            return existingClient.get();
        }
        final var phoneNumber = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_PHONE_NUMBER_COL, row);
        final var gender = ImportHandlerUtils.readAsString(BMOConstants.GENDER_COL, row);
        final var age = ImportHandlerUtils.readAsInt(BMOConstants.AGE_COL, row);
        final var businessLocation = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_LOCATION_COL, row);
        final var locationCountyCode = CommonUtil.KenyanCounty.getKenyanCountyFromName(businessLocation);
        final var industrySector = ImportHandlerUtils.readAsString(BMOConstants.INDUSTRY_SECTOR_COL, row);
        final var businessSegment = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_SEGMENT_COL, row);
        final var registrationNumber = ImportHandlerUtils.readAsString(BMOConstants.BUSINESS_REG_NUMBER, row);
        final var bmoMembership = ImportHandlerUtils.readAsString(BMOConstants.MEMBERSHIP_BMO_COL, row);
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

        final var clientDto = ParticipantDto.builder()
                .phoneNumber(phoneNumber).bestMonthlyRevenue(bestMonthlyRevenue).bmoMembership(bmoMembership)
                .hasBMOMembership(null != bmoMembership).businessLocation(businessLocation).businessName(businessName)
                .ownerGender(gender).ownerAge(age).industrySector(industrySector).businessSegment(businessSegment)
                .registrationNumber(registrationNumber).isBusinessRegistered(null != registrationNumber)
                .worstMonthlyRevenue(worstMonthlyRevenue).totalRegularEmployees(totalRegularEmployees)
                .youthRegularEmployees(youthRegularEmployees).totalCasualEmployees(totalCasualEmployees)
                .youthCasualEmployees(youthCasualEmployees).sampleRecords(sampleRecordsKept)
                .personWithDisability(personWithDisability).refugeeStatus(refugeeStatus).jgpId(jgpId)
                .locationCountyCode(locationCountyCode.isPresent() ? locationCountyCode.get().getCountyCode() : "999").build();
        return this.clientService.createClient(clientDto);
    }

    public Count importEntity() {
        Sheet groupSheet = workbook.getSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        int successCount = 0;
        int errorCount = 0;
        int progressLevel = 0;
        String errorMessage = "";
        for (int i = 0; i < bmoDataList.size(); i++) {
            Row row = groupSheet.getRow(bmoDataList.get(i).getRowIndex());
            Cell errorReportCell = row.createCell(BMOConstants.FAILURE_COL);
            Cell statusCell = row.createCell(BMOConstants.STATUS_COL);
            try {
                String status = statuses.get(i);
                progressLevel = getProgressLevel(status);

                if (progressLevel == 0) {
                    this.bmoDataService.createBMOData(List.of(bmoDataList.get(i)));
                    progressLevel = 1;
                }
                statusCell.setCellValue(TemplatePopulateImportConstants.STATUS_CELL_IMPORTED);
                statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.LIGHT_GREEN));
                successCount++;
            } catch (RuntimeException ex) {
                errorCount++;
                log.error("Problem occurred in importEntity function", ex);
                errorMessage = ImportHandlerUtils.getErrorMessage(ex);
                writeGroupErrorMessage(errorMessage, progressLevel, statusCell, errorReportCell);
            }
        }
        setReportHeaders(groupSheet);
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
}
