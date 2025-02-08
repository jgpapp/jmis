package com.jgp.infrastructure.bulkimport.importhandler;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.jgp.authentication.service.UserService;
import com.jgp.finance.domain.Loan;
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

    @Override
    public Count process(BulkImportEvent bulkImportEvent) {
        this.workbook = bulkImportEvent.workbook();
        loanDataList = new ArrayList<>();
        statuses = new ArrayList<>();
        this.rowErrorMap = new HashMap<>();
        this.documentImportProgressUUId = bulkImportEvent.importProgressUUID();
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
            if (ImportHandlerUtils.isNotImported(row, LoanConstants.STATUS_COL)) {
                loanDataList.add(readLoanData(row));
            }
        }
        updateImportProgress(this.documentImportProgressUUId, true, loanDataList.size());
    }

    private Loan readLoanData(Row row) {
        final var status = ImportHandlerUtils.readAsString(LoanConstants.STATUS_COL, row);
        final var pipeLineSource = ImportHandlerUtils.readAsString(LoanConstants.PIPELINE_SOURCE, row);
        final var applicationDate = ImportHandlerUtils.readAsDate(LoanConstants.DATE_APPLIED, row);
        final var dateDisbursed = ImportHandlerUtils.readAsDate(LoanConstants.DATE_DISBURSED, row);
        final var amountAccessed = ImportHandlerUtils.readAsDouble(LoanConstants.LOAN_AMOUNT_KES, row);
        final var valueAccessed = BigDecimal.valueOf(amountAccessed);
        final var loanDuration = ImportHandlerUtils.readAsInt(LoanConstants.LOAN_DURATION, row);
        final var outStandingAmountDouble = ImportHandlerUtils.readAsDouble(LoanConstants.OUT_STANDING_AMOUNT, row);
        final var outStandingAmount = BigDecimal.valueOf(outStandingAmountDouble);
        final var accessedPlusOutStanding = valueAccessed.add(outStandingAmount);
        if (null == rowErrorMap.get(row) && accessedPlusOutStanding.compareTo(BigDecimal.ZERO) < 1){
            rowErrorMap.put(row, "Loan Accessed and Outstanding can not be both 0! !");
        }
        final var loanQuality = ImportHandlerUtils.readAsString(LoanConstants.LOAN_QUALITY, row);
        final var loanQualityEnum = null != loanQuality ? Loan.LoanQuality.valueOf(loanQuality.toUpperCase()) : Loan.LoanQuality.NORMAL;
        final var recordedToJGPDBOnDate = ImportHandlerUtils.readAsDate(LoanConstants.DATE_RECORDED_TO_JGP_DB_COL, row);
        final var loanAmountRepaidDouble = ImportHandlerUtils.readAsDouble(LoanConstants.REPAID_LOAN_AMOUNT, row);
        final var loanAmountRepaid = BigDecimal.valueOf(loanAmountRepaidDouble);
        final var tranchAllocatedAmount = ImportHandlerUtils.readAsString(LoanConstants.TRANCH_AMOUNT_ALLOCATED_COL, row);
        validateTranchAllocated(tranchAllocatedAmount, row);
        final var tranchAmountAllocated = BigDecimal.ONE;
        final var tranchAmountDisbursedDouble = ImportHandlerUtils.readAsDouble(LoanConstants.TRANCH_AMOUNT_DISBURSED_COL, row);
        final var tranchAmountDisbursed = BigDecimal.valueOf(tranchAmountDisbursedDouble);
        final var loanerType = ImportHandlerUtils.readAsString(LoanConstants.LOANER_TYPE_COL, row);
        final var loanProduct = ImportHandlerUtils.readAsString(LoanConstants.LOAN_PRODUCT_COL, row);

        statuses.add(status);
        String jgpId = ImportHandlerUtils.readAsString(LoanConstants.JGP_ID_COL, row);
        var existingParticipant = Optional.<Participant>empty();
        if (null == jgpId){
            rowErrorMap.put(row, "JGP Id is required !!");
        }else {
            existingParticipant = this.participantService.findOneParticipantByJGPID(jgpId);
        }

        var loanData = new Loan(Objects.nonNull(userService.currentUser()) ? userService.currentUser().getPartner() : null,
                null, "1001", pipeLineSource, loanQualityEnum, Loan.LoanStatus.APPROVED, applicationDate, dateDisbursed, valueAccessed,
                loanDuration, outStandingAmount, LocalDate.now(ZoneId.systemDefault()), null, recordedToJGPDBOnDate,
                loanAmountRepaid, loanerType, tranchAmountAllocated, tranchAmountDisbursed, loanProduct, row.getRowNum());

        if (null == rowErrorMap.get(row)){
            validateLoan(loanData, row);
        }

        if (existingParticipant.isEmpty() && null == rowErrorMap.get(row)){
            final var participantDto = getParticipantDto(row);
            validateParticipant(participantDto, row);
            if (null == rowErrorMap.get(row)){
                existingParticipant = Optional.of(this.participantService.createParticipant(participantDto));
            }
        }

        existingParticipant.ifPresent(loanData::setParticipant);

        return loanData;
    }

    private ParticipantDto getParticipantDto(Row row){
        String businessName = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_NAME_COL, row);
        String jgpId = ImportHandlerUtils.readAsString(LoanConstants.JGP_ID_COL, row);
        final var phoneNumber = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_PHONE_NUMBER_COL, row);
        final var gender = ImportHandlerUtils.readAsString(LoanConstants.GENDER_COL, row);
        final var age = ImportHandlerUtils.readAsInt(LoanConstants.AGE_COL, row);
        final var businessLocation = ImportHandlerUtils.readAsString(LoanConstants.BUSINESS_LOCATION_COL, row);
        final var locationCountyCode = CommonUtil.KenyanCounty.getKenyanCountyFromName(businessLocation);
        final var industrySector = ImportHandlerUtils.readAsString(LoanConstants.INDUSTRY_SECTOR_COL, row);

        final var totalRegularEmployees = ImportHandlerUtils.readAsInt(LoanConstants.TOTAL_REGULAR_EMPLOYEES_COL, row);
        if ((null == totalRegularEmployees || totalRegularEmployees < 1) && null == rowErrorMap.get(row)){
            rowErrorMap.put(row, "Regular Employees Must Be Greater Than 0 !!");
        }
        final var youthRegularEmployees = ImportHandlerUtils.readAsInt(LoanConstants.YOUTH_REGULAR_EMPLOYEES_COL, row);
        final var totalCasualEmployees = ImportHandlerUtils.readAsInt(LoanConstants.TOTAL_CASUAL_EMPLOYEES_COL, row);
        final var youthCasualEmployees = ImportHandlerUtils.readAsInt(LoanConstants.YOUTH_CASUAL_EMPLOYEES_COL, row);

        return ParticipantDto.builder()
                .phoneNumber(phoneNumber).bmoMembership(null)
                .hasBMOMembership(Boolean.TRUE).businessLocation(businessLocation).businessName(businessName)
                .ownerGender(gender).ownerAge(age).industrySector(industrySector).businessSegment("Other")
                .totalRegularEmployees(totalRegularEmployees)
                .youthRegularEmployees(youthRegularEmployees).totalCasualEmployees(totalCasualEmployees)
                .youthCasualEmployees(youthCasualEmployees).jgpId(jgpId)
                .locationCountyCode(locationCountyCode.isPresent() ? locationCountyCode.get().getCountyCode() : "999").build();
    }

    public Count importEntity() {
        Sheet groupSheet = workbook.getSheet(TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        int successCount = 0;
        int errorCount = 0;
        int progressLevel = 0;
        String errorMessage = "";
        var loanDataSize = loanDataList.size();
        for (int i = 0; i < loanDataSize; i++) {
            final var loanData = loanDataList.get(i);
            Row row = groupSheet.getRow(loanData.getRowIndex());
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
                    this.loanService.createLoans(List.of(loanData));
                    progressLevel = 1;
                }
                statusCell.setCellValue(TemplatePopulateImportConstants.STATUS_CELL_IMPORTED);
                statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.LIGHT_GREEN));
                successCount++;
            } catch (RuntimeException ex) {
                errorCount++;
                log.error("Problem occurred When Uploading Lending Data: {}", ex.getMessage());
                errorMessage = ImportHandlerUtils.getErrorMessage(ex);
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
        setReportHeaders(groupSheet);
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

    private void validateParticipant(ParticipantDto participantDto, Row row) {
        // Create a Validator instance
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        Validator validator = factory.getValidator();

        // Validate the object
        Set<ConstraintViolation<ParticipantDto>> violations = validator.validate(participantDto);

        // Get the first error, if any
        if (null == rowErrorMap.get(row) && !violations.isEmpty()) {
            ConstraintViolation<ParticipantDto> firstViolation = violations.iterator().next();
            rowErrorMap.put(row, firstViolation.getMessage());
        }

        if (null == rowErrorMap.get(row) && !CommonUtil.isStringValueLengthValid(participantDto.jgpId(), 5, 10)){
            rowErrorMap.put(row, "JGP ID must be 5-10 characters !!");
        }
        if (null == rowErrorMap.get(row) && !CommonUtil.isStringValueLengthValid(participantDto.phoneNumber(), 9, 12)){
            rowErrorMap.put(row, "Phone number must be 9-12 digits !!");
        }
    }

    private void validateLoan(Loan loan, Row row) {
        // Create a Validator instance
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        Validator validator = factory.getValidator();

        // Validate the object
        Set<ConstraintViolation<Loan>> violations = validator.validate(loan);

        // Get the first error, if any
        if (null == rowErrorMap.get(row) && !violations.isEmpty()) {
            ConstraintViolation<Loan> firstViolation = violations.iterator().next();
            rowErrorMap.put(row, firstViolation.getMessage());
        }
    }

    private void validateTranchAllocated(String value, Row row) {
        final var deliveryModes = Set.of("TRANCH 1", "TRANCH 2", "TRANCH 3", "NOT APPLICABLE");
        if (null == rowErrorMap.get(row) && null != value && !deliveryModes.contains(value.toUpperCase())){
            rowErrorMap.put(row, "Invalid Value for Allocated Tranch (Must be Tranch 1/Tranch 2/Tranch 3/Not Applicable) !!");
        }
    }
}
