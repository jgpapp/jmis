package com.jgp.infrastructure.bulkimport.importhandler;

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
import com.jgp.infrastructure.documentmanagement.domain.Document;
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
    private Map<Row, String> rowErrorMap;
    private String documentImportProgressUUId;
    private Boolean updateParticipantInfo;
    private Document document;

    @Override
    public Count process(BulkImportEvent bulkImportEvent) {
        this.workbook = bulkImportEvent.workbook();
        loanDataList = new ArrayList<>();
        this.rowErrorMap = new HashMap<>();
        this.documentImportProgressUUId = bulkImportEvent.importProgressUUID();
        this.updateParticipantInfo = bulkImportEvent.updateParticipantInfo();
        this.document = bulkImportEvent.document();
        readExcelFile();
        return importEntity();
    }


    public void readExcelFile() {
        Sheet loanSheet = workbook.getSheet(TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        Integer noOfEntries = ImportHandlerUtils.getNumberOfRows(loanSheet, TemplatePopulateImportConstants.FIRST_COLUMN_INDEX);
        importProgressService.updateTotal(this.documentImportProgressUUId, noOfEntries);

        this.importProgressService.updateStep(this.documentImportProgressUUId, TemplatePopulateImportConstants.EXCEL_UPLOAD_READING_STEP);
        for (int rowIndex = 1; rowIndex <= noOfEntries; rowIndex++) {
            Row row;
            row = loanSheet.getRow(rowIndex);
            if (null != row && ImportHandlerUtils.isNotImported(row, LoanConstants.STATUS_COL)) {
                loanDataList.add(readLoanData(row));
                this.importProgressService.incrementAndSendProgressUpdate(this.documentImportProgressUUId);
            }
        }
    }

    private Loan readLoanData(Row row) {
        final var pipeLineSource = ImportHandlerUtils.readAsString(LoanConstants.PIPELINE_SOURCE, row);
        final var applicationDate = DataValidator.validateLocalDate(LoanConstants.DATE_APPLIED, row, rowErrorMap, "Application Date", true);
        final var dateDisbursed = DataValidator.validateLocalDate(LoanConstants.DATE_DISBURSED, row, rowErrorMap, "Date Disbursed", true);
        final var amountApproved = DataValidator.validateTemplateDoubleValue(LoanConstants.LOAN_AMOUNT_KES, row, "amount approved", rowErrorMap, true);
        final var loanAmount = null == amountApproved ? BigDecimal.ZERO : BigDecimal.valueOf(amountApproved);
        final var loanDuration = DataValidator.validateTemplateIntegerValue(LoanConstants.LOAN_DURATION, row, "loan duration", rowErrorMap, true);
        final var outStandingAmountDouble = DataValidator.validateTemplateDoubleValue(LoanConstants.OUT_STANDING_AMOUNT, row, "out-standing amount", rowErrorMap, true);
        final var outStandingAmount = null == outStandingAmountDouble ? BigDecimal.ZERO : BigDecimal.valueOf(outStandingAmountDouble);
        var loanQuality = ImportHandlerUtils.readAsString(LoanConstants.LOAN_QUALITY, row);
        loanQuality = LoanValidator.validateLoanQuality(loanQuality, row, rowErrorMap);
        var loanQualityEnum = Loan.LoanQuality.NORMAL;
        if (null == rowErrorMap.get(row)){
            loanQualityEnum = (null != loanQuality) ? Loan.LoanQuality.valueOf(loanQuality.toUpperCase()) : Loan.LoanQuality.NORMAL;
        }
        final var recordedToJGPDBOnDate = DataValidator.validateLocalDate(LoanConstants.DATE_RECORDED_TO_JGP_DB_COL, row, rowErrorMap, "Date Recorded To JGP DB", true);
        final var loanAmountRepaidDouble = DataValidator.validateTemplateDoubleValue(LoanConstants.REPAID_LOAN_AMOUNT, row, "amount repaid", rowErrorMap, true);
        final var loanAmountRepaid = null == loanAmountRepaidDouble ? BigDecimal.ZERO : BigDecimal.valueOf(loanAmountRepaidDouble);
        final var tranchAmountDouble = DataValidator.validateTemplateDoubleValue(LoanConstants.TRANCH_AMOUNT_COL, row, "tranch amount", rowErrorMap, false);
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
                loanAmountRepaid, loanerType, loanProduct, userService.currentUser(), this.document, row.getRowNum());
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
            ParticipantValidator.validateParticipant(participantDto, DataValidator.getValidator());
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
        final var phoneNumber = DataValidator.validatePhoneNumber(LoanConstants.BUSINESS_PHONE_NUMBER_COL, row, rowErrorMap);
        var gender = ImportHandlerUtils.readAsString(LoanConstants.GENDER_COL, row);
        gender = ParticipantValidator.validateGender(gender, row, rowErrorMap);
        var age = DataValidator.validateTemplateIntegerValue(LoanConstants.AGE_COL, row, "age", rowErrorMap, false);
        age = ParticipantValidator.validateParticipantAge(age, row, rowErrorMap);
        var locationCounty = DataValidator.validateCountyName(LoanConstants.BUSINESS_LOCATION_COL, row, rowErrorMap);
        final var industrySector = ImportHandlerUtils.readAsString(LoanConstants.INDUSTRY_SECTOR_COL, row);

        final var totalRegularEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.TOTAL_REGULAR_EMPLOYEES_COL, row, "total regular employees", rowErrorMap, true);
        if ((null == totalRegularEmployees || totalRegularEmployees < 1) && null == rowErrorMap.get(row)){
            rowErrorMap.put(row, "Regular Employees Must Be Greater Than 0 !!");
        }
        final var youthRegularEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.YOUTH_REGULAR_EMPLOYEES_COL, row, "youth regular employees", rowErrorMap, false);
        final var totalCasualEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.TOTAL_CASUAL_EMPLOYEES_COL, row, "total casual employees", rowErrorMap, false);
        final var youthCasualEmployees = DataValidator.validateTemplateIntegerValue(LoanConstants.YOUTH_CASUAL_EMPLOYEES_COL, row, "youth casual employees", rowErrorMap, false);

        return ParticipantDto.builder()
                .phoneNumber(phoneNumber).businessLocation(locationCounty.getCountyName()).businessName(businessName)
                .ownerGender(gender).ownerAge(age).industrySector(industrySector).businessSegment("Other")
                .totalRegularEmployees(totalRegularEmployees)
                .youthRegularEmployees(youthRegularEmployees).totalCasualEmployees(totalCasualEmployees)
                .youthCasualEmployees(youthCasualEmployees).jgpId(jgpId)
                .locationCountyCode(locationCounty.getCountyCode())
                .businessRegNumber(businessRegNumber).participantName(participantName).row(row).rowErrorMap(rowErrorMap).build();
    }

    public Count importEntity() {
        Sheet loansSheet = workbook.getSheet(TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        int successCount = 0;
        int errorCount = 0;
        String errorMessage = "";
        var loanDataSize = loanDataList.size();
        importProgressService.resetEveryThingToZero(this.documentImportProgressUUId);
        for (int i = 0; i < loanDataSize; i++) {
            final var loanData = loanDataList.get(i);
            Row row = loansSheet.getRow(loanData.getRowIndex());
            Cell errorReportCell = row.createCell(LoanConstants.FAILURE_COL);
            Cell statusCell = row.createCell(LoanConstants.STATUS_COL);
            if (null == rowErrorMap.get(row) && Objects.isNull(loanData.getParticipant())){
                rowErrorMap.put(row, "Can not associate loan data to a participant !!");
            }
            try {
                final var validationError = rowErrorMap.get(row);
                if (null != validationError){
                    throw new InvalidDataException(validationError);
                }
                this.loanService.createOrUpdateLoan(loanData);
                statusCell.setCellValue(TemplatePopulateImportConstants.STATUS_CELL_IMPORTED);
                statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.LIGHT_GREEN));
                successCount++;
            } catch (RuntimeException ex) {
                errorCount++;
                log.error("Problem occurred When Uploading Lending Data: {}", ex.getMessage());
                errorMessage = ImportHandlerUtils.getErrorMessage(ex);
                if (errorMessage.contains("unique_loan") || errorMessage.contains("Duplicate Disbursement On Same Day")){
                    errorMessage = "Row with same partner/participant/disburse date/Loan Identifier already exist !!";
                }
                writeGroupErrorMessage(errorMessage, workbook, statusCell, errorReportCell);
            }finally {
                this.importProgressService.incrementAndSendProgressUpdate(this.documentImportProgressUUId);
            }
        }
        setReportHeaders(loansSheet, LoanConstants.STATUS_COL, LoanConstants.FAILURE_COL);
        log.info("Finished Import Finished := {}", LocalDateTime.now(ZoneId.systemDefault()));
        return Count.instance(loanDataSize, successCount, errorCount);
    }
}
