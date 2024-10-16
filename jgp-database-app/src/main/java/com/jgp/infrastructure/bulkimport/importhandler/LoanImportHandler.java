package com.jgp.infrastructure.bulkimport.importhandler;

import com.jgp.authentication.service.UserService;
import com.jgp.finance.domain.Loan;
import com.jgp.finance.service.LoanService;
import com.jgp.infrastructure.bulkimport.constants.BMOConstants;
import com.jgp.infrastructure.bulkimport.constants.LoanConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.participant.service.ParticipantService;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


@Service
@Slf4j
@RequiredArgsConstructor
public class LoanImportHandler implements ImportHandler {

    private final LoanService loanService;
    private final ParticipantService clientService;
    private final UserService userService;
    List<Loan> loanDataList;
    private Workbook workbook;
    private List<String> statuses;

    @Override
    public Count process(BulkImportEvent bulkImportEvent) {
        this.workbook = bulkImportEvent.workbook();
        loanDataList = new ArrayList<>();
        statuses = new ArrayList<>();
        readExcelFile();
        return importEntity();
    }

    public void readExcelFile() {
        Sheet loanSheet = workbook.getSheet(TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(loanSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);
        for (int rowIndex = 1; rowIndex <= noOfEntries; rowIndex++) {
            Row row;
            row = loanSheet.getRow(rowIndex);
            if (ImportHandlerUtils.isNotImported(row, LoanConstants.STATUS_COL)) {
                loanDataList.add(readLoanData(row));
            }
        }
    }

    private Loan readLoanData(Row row) {
        final var status = ImportHandlerUtils.readAsString(LoanConstants.STATUS_COL, row);
        final var pipeLineSource = ImportHandlerUtils.readAsString(LoanConstants.PIPELINE_SOURCE, row);
        final var loanStatus = ImportHandlerUtils.readAsString(LoanConstants.LOAN_STATUS, row);
        final var loanStatusEnum = null != loanStatus ? Loan.LoanStatus.valueOf(loanStatus.toUpperCase()) : Loan.LoanStatus.NEW;
        final var applicationDate = ImportHandlerUtils.readAsDate(LoanConstants.DATE_APPLIED, row);
        final var dateDisbursed = ImportHandlerUtils.readAsDate(LoanConstants.DATE_DISBURSED, row);
        final var amountAccessed = ImportHandlerUtils.readAsDouble(LoanConstants.VALUE_ACCESSED, row);
        final var valueAccessed = BigDecimal.valueOf(amountAccessed);
        final var loanDuration = ImportHandlerUtils.readAsInt(LoanConstants.LOAN_DURATION, row);
        final var outStandingAmountDouble = ImportHandlerUtils.readAsDouble(LoanConstants.OUT_STANDING_AMOUNT, row);
        final var outStandingAmount = BigDecimal.valueOf(outStandingAmountDouble);
        final var loanQuality = ImportHandlerUtils.readAsString(LoanConstants.LOAN_QUALITY, row);
        final var loanQualityEnum = null != loanQuality ? Loan.LoanQuality.valueOf(loanQuality.toUpperCase()) : Loan.LoanQuality.NORMAL;
        final var dateRecordedByPartner = ImportHandlerUtils.readAsDate(LoanConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL, row);
        final var recordedToJGPDBOnDate = ImportHandlerUtils.readAsDate(LoanConstants.DATE_RECORDED_TO_JGP_DB_COL, row);
        final var uniqueValues = ImportHandlerUtils.readAsString(LoanConstants.UNIQUE_VALUES, row);
        statuses.add(status);

        return new Loan(Objects.nonNull(userService.currentUser()) ? userService.currentUser().getPartner() : null,
                getParticipant(row), "1001", pipeLineSource, loanQualityEnum, loanStatusEnum, applicationDate, dateDisbursed, valueAccessed,
                loanDuration, outStandingAmount,  dateRecordedByPartner, uniqueValues, recordedToJGPDBOnDate, row.getRowNum());
    }

    private Participant getParticipant(Row row){
        String businessName = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_NAME_COL, row);
        String jgpId = ImportHandlerUtils.readAsString(LoanConstants.JGP_ID_COL, row);
        if (null == jgpId){
            return null;
        }
        final var existingClient = this.clientService.findOneByJGPID(jgpId);
        if (existingClient.isPresent()){
            return existingClient.get();
        }

        final var phoneNumber = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_PHONE_NUMBER_COL, row);
        final var gender = ImportHandlerUtils.readAsString(LoanConstants.GENDER_COL, row);
        final var age = ImportHandlerUtils.readAsInt(LoanConstants.AGE_COL, row);
        final var businessLocation = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_LOCATION_COL, row);
        final var locationCountyCode = CommonUtil.KenyanCounty.getKenyanCountyFromName(businessLocation);
        final var industrySector = ImportHandlerUtils.readAsString(LoanConstants.INDUSTRY_SECTOR_COL, row);
        final var businessSegment = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_SEGMENT_COL, row);
        final var clientDto = ParticipantDto.builder().businessLocation(businessLocation).businessName(businessName)
                .ownerGender(gender).ownerAge(age).isBusinessRegistered(true).jgpId(jgpId)
                .phoneNumber(phoneNumber).industrySector(industrySector).businessSegment(businessSegment)
                .locationCountyCode(locationCountyCode.isPresent() ? locationCountyCode.get().getCountyCode() : "999").build();
        return this.clientService.createClient(clientDto);
    }

    public Count importEntity() {
        Sheet groupSheet = workbook.getSheet(TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        int successCount = 0;
        int errorCount = 0;
        int progressLevel = 0;
        String errorMessage = "";
        for (int i = 0; i < loanDataList.size(); i++) {
            Row row = groupSheet.getRow(loanDataList.get(i).getRowIndex());
            Cell errorReportCell = row.createCell(BMOConstants.FAILURE_COL);
            Cell statusCell = row.createCell(BMOConstants.STATUS_COL);
            try {
                String status = statuses.get(i);
                progressLevel = getProgressLevel(status);

                if (progressLevel == 0) {
                    this.loanService.createLoans(List.of(loanDataList.get(i)));
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
