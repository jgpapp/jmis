package com.jgp.infrastructure.bulkimport.importhandler;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.jgp.authentication.service.UserService;
import com.jgp.finance.domain.Loan;
import com.jgp.finance.domain.LoanTransaction;
import com.jgp.finance.service.LoanService;
import com.jgp.infrastructure.bulkimport.constants.LoanConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.exception.InvalidDataException;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.participant.service.ParticipantService;
import com.jgp.shared.validator.DataValidator;
import com.jgp.shared.validator.LoanValidator;
import com.jgp.shared.validator.ParticipantValidator;
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
import java.util.concurrent.ExecutionException;


@Service
@Slf4j
@RequiredArgsConstructor
public class LoanImportHandler implements ImportHandler {

    private final LoanService loanService;
    private final ParticipantService participantService;
    private final UserService userService;
    private final ImportProgressService importProgressService;
    List<Loan> loanDataList;
    private Workbook workbook;
    private List<String> statuses;
    private Map<Row, String> rowErrorMap;
    private String documentImportProgressUUId;
    private Boolean updateParticipantInfo;

    @Override
    public Count process(BulkImportEvent bulkImportEvent) {
        this.workbook = bulkImportEvent.workbook();
        loanDataList = new ArrayList<>();
        statuses = new ArrayList<>();
        this.rowErrorMap = new HashMap<>();
        this.documentImportProgressUUId = bulkImportEvent.importProgressUUID();
        this.updateParticipantInfo = bulkImportEvent.updateParticipantInfo();
        readExcelFile();
        return importEntity();
    }

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


    public void readExcelFile() {
        Sheet loanSheet = workbook.getSheet(TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(loanSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);

        for (int rowIndex = 1; rowIndex <= noOfEntries; rowIndex++) {
            Row row;
            row = loanSheet.getRow(rowIndex);
            if (null != row && ImportHandlerUtils.isNotImported(row, LoanConstants.STATUS_COL)) {
                loanDataList.add(readLoanData(row));
            }
        }
        updateImportProgress(this.documentImportProgressUUId, true, loanDataList.size());
    }

    private Loan readLoanData(Row row) {
        final var status = ImportHandlerUtils.readAsString(LoanConstants.STATUS_COL, row);
        final var pipeLineSource = ImportHandlerUtils.readAsString(LoanConstants.PIPELINE_SOURCE, row);
        final var applicationDate = DataValidator.validateLocalDate(LoanConstants.DATE_APPLIED, row, rowErrorMap, "Application Date");
        final var dateDisbursed = DataValidator.validateLocalDate(LoanConstants.DATE_DISBURSED, row, rowErrorMap, "Date Disbursed");
        final var amountApproved = DataValidator.validateTemplateDoubleValue(LoanConstants.LOAN_AMOUNT_KES, row, rowErrorMap);
        final var loanAmount = null == amountApproved ? BigDecimal.ZERO : BigDecimal.valueOf(amountApproved);
        final var loanDuration = DataValidator.validateTemplateIntegerValue(LoanConstants.LOAN_DURATION, row, rowErrorMap);
        final var outStandingAmountDouble = DataValidator.validateTemplateDoubleValue(LoanConstants.OUT_STANDING_AMOUNT, row, rowErrorMap);
        final var outStandingAmount = null == outStandingAmountDouble ? BigDecimal.ZERO : BigDecimal.valueOf(outStandingAmountDouble);
        var loanQuality = ImportHandlerUtils.readAsString(LoanConstants.LOAN_QUALITY, row);
        loanQuality = LoanValidator.validateLoanQuality(loanQuality, row, rowErrorMap);
        var loanQualityEnum = Loan.LoanQuality.NORMAL;
        if (null == rowErrorMap.get(row)){
            loanQualityEnum = (null != loanQuality) ? Loan.LoanQuality.valueOf(loanQuality.toUpperCase()) : Loan.LoanQuality.NORMAL;
        }
        final var recordedToJGPDBOnDate = DataValidator.validateLocalDate(LoanConstants.DATE_RECORDED_TO_JGP_DB_COL, row, rowErrorMap, "Date Recorded To JGP DB");
        final var loanAmountRepaidDouble = DataValidator.validateTemplateDoubleValue(LoanConstants.REPAID_LOAN_AMOUNT, row, rowErrorMap);
        final var loanAmountRepaid = null == loanAmountRepaidDouble ? BigDecimal.ZERO : BigDecimal.valueOf(loanAmountRepaidDouble);
        final var tranchAmountDouble = DataValidator.validateTemplateDoubleValue(LoanConstants.TRANCH_AMOUNT_COL, row, rowErrorMap);
        final var tranchAmount = null == tranchAmountDouble ? BigDecimal.ZERO : BigDecimal.valueOf(tranchAmountDouble);
        var tranchAllocated = ImportHandlerUtils.readAsString(LoanConstants.TRANCH_ALLOCATED_COL, row);
        tranchAllocated = LoanValidator.validateTranchAllocated(tranchAllocated, tranchAmount, row, rowErrorMap);
        var loanerType = ImportHandlerUtils.readAsString(LoanConstants.LOANER_TYPE_COL, row);
        loanerType = LoanValidator.validateLoanerType(loanerType, row, rowErrorMap);
        var loanProduct = ImportHandlerUtils.readAsString(LoanConstants.LOAN_PRODUCT_COL, row);
        loanProduct = LoanValidator.validateAndNormalizeLoanProduct(loanProduct, row, rowErrorMap);
        var loanIdentifier = ImportHandlerUtils.readAsString(LoanConstants.LOAN_IDENTIFIER_COL, row);
        if (null == rowErrorMap.get(row) && null == loanIdentifier){
            rowErrorMap.put(row, "Loan Identifier is required !!");
        }

        statuses.add(status);
        String jgpId = ImportHandlerUtils.readAsString(LoanConstants.JGP_ID_COL, row);
        var existingParticipant = Optional.<Participant>empty();
        if (null == jgpId){
            rowErrorMap.put(row, "JGP Id is required !!");
        }else {
            existingParticipant = this.participantService.findOneParticipantByJGPID(jgpId);
        }

        var loanData = new Loan(Objects.nonNull(userService.currentUser()) ? userService.currentUser().getPartner() : null,
                null, loanIdentifier, pipeLineSource, loanQualityEnum, Loan.LoanStatus.APPROVED, applicationDate, dateDisbursed, loanAmount,
                loanDuration, outStandingAmount, LocalDate.now(ZoneId.systemDefault()), null, recordedToJGPDBOnDate,
                loanAmountRepaid, loanerType, loanProduct, userService.currentUser(), row.getRowNum());
        final var transactionAmount = tranchAmount.compareTo(BigDecimal.ZERO) <= 0 ? loanAmount : tranchAmount;
        if (null == rowErrorMap.get(row) && transactionAmount.compareTo(BigDecimal.ZERO) < 1){
            rowErrorMap.put(row, "Loan Amount and Tranch Amount can not be both 0!!");
        }
        if (null == rowErrorMap.get(row) && loanAmount.compareTo(BigDecimal.ZERO) > 0 && tranchAmount.compareTo(loanAmount) >= 0){
            rowErrorMap.put(row, "Loan Amount Must Be Greater Than Tranch Amount If Both Are Provided!!");
        }
        loanData.addLoanTransaction(new LoanTransaction(loanData, LoanTransaction.TransactionType.DISBURSEMENT,
                null != tranchAllocated ? tranchAllocated : "Full Loan", dateDisbursed, transactionAmount,
                outStandingAmount, userService.currentUser(), null != tranchAllocated));

        if (null == rowErrorMap.get(row)){
            LoanValidator.validateLoan(loanData, row, rowErrorMap);
        }
        final var participantDto = getParticipantDto(row);
        if (Boolean.TRUE.equals(this.updateParticipantInfo)) {
            existingParticipant.ifPresent(participant -> this.participantService.updateParticipant(participant.getId(), participantDto));
        }
        if (existingParticipant.isEmpty() && null == rowErrorMap.get(row)){
            ParticipantValidator.validateParticipant(participantDto, row, rowErrorMap);
            if (null == rowErrorMap.get(row)){
                existingParticipant = Optional.of(this.participantService.createParticipant(participantDto));
            }
        }

        existingParticipant.ifPresent(loanData::setParticipant);

        return loanData;
    }

    private ParticipantDto getParticipantDto(Row row){
        String participantName = ImportHandlerUtils.readAsString(LoanConstants.PARTICIPANT_NAME_COL, row);
        if (null == participantName && null == rowErrorMap.get(row)){
            rowErrorMap.put(row, "Participant Name Is Required !!");
        }
        String businessName = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_NAME_COL, row);
        String businessRegNumber = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_REGISTRATION_NUMBER_COL, row);
        String jgpId = ImportHandlerUtils.readAsString(LoanConstants.JGP_ID_COL, row);
        final var phoneNumber = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_PHONE_NUMBER_COL, row);
        var gender = ImportHandlerUtils.readAsString(LoanConstants.GENDER_COL, row);
        gender = ParticipantValidator.validateGender(gender, row, rowErrorMap);
        var age = DataValidator.validateTemplateIntegerValue(LoanConstants.AGE_COL, row, rowErrorMap);
        age = ParticipantValidator.validateParticipantAge(age, row, rowErrorMap);
        var locationCounty = DataValidator.validateCountyName(LoanConstants.BUSINESS_LOCATION_COL, row, rowErrorMap);
        final var industrySector = ImportHandlerUtils.readAsString(LoanConstants.INDUSTRY_SECTOR_COL, row);

        final var totalRegularEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.TOTAL_REGULAR_EMPLOYEES_COL, row, rowErrorMap);
        if ((null == totalRegularEmployees || totalRegularEmployees < 1) && null == rowErrorMap.get(row)){
            rowErrorMap.put(row, "Regular Employees Must Be Greater Than 0 !!");
        }
        final var youthRegularEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.YOUTH_REGULAR_EMPLOYEES_COL, row, rowErrorMap);
        final var totalCasualEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.TOTAL_CASUAL_EMPLOYEES_COL, row, rowErrorMap);
        final var youthCasualEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.YOUTH_CASUAL_EMPLOYEES_COL, row, rowErrorMap);

        return ParticipantDto.builder()
                .phoneNumber(phoneNumber).businessLocation(locationCounty.getCountyName()).businessName(businessName)
                .ownerGender(gender).ownerAge(age).industrySector(industrySector).businessSegment("Other")
                .totalRegularEmployees(totalRegularEmployees)
                .youthRegularEmployees(youthRegularEmployees).totalCasualEmployees(totalCasualEmployees)
                .youthCasualEmployees(youthCasualEmployees).jgpId(jgpId)
                .locationCountyCode(locationCounty.getCountyCode())
                .businessRegNumber(businessRegNumber).participantName(participantName).build();
    }

    public Count importEntity() {
        Sheet loansSheet = workbook.getSheet(TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        int successCount = 0;
        int errorCount = 0;
        int progressLevel = 0;
        String errorMessage = "";
        var loanDataSize = loanDataList.size();
        for (int i = 0; i < loanDataSize; i++) {
            final var loanData = loanDataList.get(i);
            Row row = loansSheet.getRow(loanData.getRowIndex());
            Cell errorReportCell = row.createCell(LoanConstants.FAILURE_COL);
            Cell statusCell = row.createCell(LoanConstants.STATUS_COL);
            if (null == rowErrorMap.get(row) && Objects.isNull(loanData.getParticipant())){
                rowErrorMap.put(row, "Can not associate loan data to a participant !!");
            }
            try {
                String status = statuses.get(i);
                progressLevel = getProgressLevel(status);

                final var validationError = rowErrorMap.get(row);
                if (null != validationError){
                    throw new InvalidDataException(validationError);
                }
                if (progressLevel == 0) {
                    this.loanService.createOrUpdateLoan(loanData);
                    progressLevel = 1;
                }
                statusCell.setCellValue(TemplatePopulateImportConstants.STATUS_CELL_IMPORTED);
                statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.LIGHT_GREEN));
                successCount++;
            } catch (RuntimeException ex) {
                errorCount++;
                log.error("Problem occurred When Uploading Lending Data: {}", ex.getMessage());
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
        setReportHeaders(loansSheet);
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
        ImportHandlerUtils.writeString(LoanConstants.STATUS_COL, bmpSheet.getRow(TemplatePopulateImportConstants.ROWHEADER_INDEX),
                TemplatePopulateImportConstants.STATUS_COL_REPORT_HEADER);
        ImportHandlerUtils.writeString(LoanConstants.FAILURE_COL, bmpSheet.getRow(TemplatePopulateImportConstants.ROWHEADER_INDEX),
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
