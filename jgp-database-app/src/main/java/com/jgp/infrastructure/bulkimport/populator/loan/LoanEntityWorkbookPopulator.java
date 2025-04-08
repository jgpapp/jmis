package com.jgp.infrastructure.bulkimport.populator.loan;

import com.jgp.infrastructure.bulkimport.constants.BMOConstants;
import com.jgp.infrastructure.bulkimport.constants.LoanConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.populator.AbstractWorkbookPopulator;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.SpreadsheetVersion;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFDataValidationHelper;

@Slf4j
public class LoanEntityWorkbookPopulator extends AbstractWorkbookPopulator {

    @Override
    public void populate(Workbook workbook) {
        Sheet loanSheet = workbook.createSheet(TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        setLayout(loanSheet);
    }

    private void setLayout(Sheet worksheet) {
        Row rowHeader = worksheet.createRow(TemplatePopulateImportConstants.ROWHEADER_INDEX);
        rowHeader.setHeight(TemplatePopulateImportConstants.ROW_HEADER_HEIGHT);
        worksheet.setColumnWidth(LoanConstants.PARTICIPANT_NAME_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.BUSINESS_NAME_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.CORP_PIN_NUMBER, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.JGP_ID_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.BUSINESS_PHONE_NUMBER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.GENDER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.AGE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.PASS_PORT_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.BUSINESS_LOCATION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.INDUSTRY_SECTOR_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.PIPELINE_SOURCE, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.DATE_APPLIED, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.DATE_DISBURSED, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.LOAN_AMOUNT_KES, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.LOAN_DURATION, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.OUT_STANDING_AMOUNT, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.REPAID_LOAN_AMOUNT, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.LOAN_QUALITY, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.TOTAL_REGULAR_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.YOUTH_REGULAR_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.TOTAL_CASUAL_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.YOUTH_CASUAL_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.LOANER_TYPE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.DATE_RECORDED_TO_JGP_DB_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.LOAN_PRODUCT_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.TRANCH_AMOUNT_ALLOCATED_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.TRANCH_AMOUNT_DISBURSED_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);


        writeString(LoanConstants.PARTICIPANT_NAME_COL, rowHeader, "Participant Name*");
        writeString(LoanConstants.BUSINESS_NAME_COL, rowHeader, "Business Name");
        writeString(LoanConstants.CORP_PIN_NUMBER, rowHeader, "Corporation PIN Number");
        writeString(LoanConstants.JGP_ID_COL, rowHeader, "Unique JGP ID (National ID)*");
        writeString(LoanConstants.BUSINESS_PHONE_NUMBER_COL, rowHeader, "Business phone number*");
        writeString(LoanConstants.GENDER_COL, rowHeader, "Gender of owner*");
        writeString(LoanConstants.AGE_COL, rowHeader, "Age of owner (full years)*");
        writeString(LoanConstants.PASS_PORT_COL, rowHeader, "Passport");
        writeString(LoanConstants.BUSINESS_LOCATION_COL, rowHeader, "Business Location(County)*");
        writeString(LoanConstants.INDUSTRY_SECTOR_COL, rowHeader, "Industry sector*(Agriculture, Artists/artisans, Manufacturing, Trading & Retail, Other)");
        writeString(LoanConstants.PIPELINE_SOURCE, rowHeader, "Pipeline source*");
        writeString(LoanConstants.DATE_APPLIED, rowHeader, "Date loan application(yyyy-MM-dd)*");
        writeString(LoanConstants.DATE_DISBURSED, rowHeader, "Date loan disbursed(yyyy-MM-dd)*");
        writeString(LoanConstants.LOAN_AMOUNT_KES, rowHeader, "Loan Amount (KES)*");
        writeString(LoanConstants.LOAN_DURATION, rowHeader, "Loan duration (months)*");
        writeString(LoanConstants.OUT_STANDING_AMOUNT, rowHeader, "Outstanding amount*");
        writeString(LoanConstants.REPAID_LOAN_AMOUNT, rowHeader, "Repaid Loan Amount (KES)");
        writeString(LoanConstants.LOAN_QUALITY, rowHeader, "Loan quality*(Normal, Watch, Substandard, Doubtful and Loss)");
        writeString(LoanConstants.TOTAL_REGULAR_EMPLOYEES_COL, rowHeader, "Total number of regular employees including owner*");
        writeString(LoanConstants.YOUTH_REGULAR_EMPLOYEES_COL, rowHeader, "Regular, of which are youth (18-35)*");
        writeString(LoanConstants.TOTAL_CASUAL_EMPLOYEES_COL, rowHeader, "Total number of casual employees excluding owner*");
        writeString(LoanConstants.YOUTH_CASUAL_EMPLOYEES_COL, rowHeader, "Casual, of which are youth (18-35)*");
        writeString(LoanConstants.LOANER_TYPE_COL, rowHeader, "Loaner Type");
        writeString(LoanConstants.DATE_RECORDED_TO_JGP_DB_COL, rowHeader, "Date added to JGP database(yyyy-MM-dd)*");
        writeString(LoanConstants.LOAN_PRODUCT_COL, rowHeader, "Loan product* (Working Capital/Asset Finance/Stahimili/Purchase Order/Consignment Finance/Shariah Compliant)");
        writeString(LoanConstants.TRANCH_AMOUNT_ALLOCATED_COL, rowHeader, "Tranch amount allocated(Tranch 1/Tranch 2/Tranch 3/Not Applicable)");
        writeString(LoanConstants.TRANCH_AMOUNT_DISBURSED_COL, rowHeader, "Tranch Amount Disbursed (KES)");


    }

    private void setRules(Sheet worksheet){
        try {
            CellRangeAddressList isRefugeeRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.REFUGEE_STATUS_COL, BMOConstants.REFUGEE_STATUS_COL);

            DataValidationHelper validationHelper = new XSSFDataValidationHelper((org.apache.poi.xssf.usermodel.XSSFSheet) worksheet);

            DataValidationConstraint isRefugeeConstraint = validationHelper.createExplicitListConstraint(new String[] { "Yes", "No" });

            DataValidation isRefugeeValidation = validationHelper.createValidation(isRefugeeConstraint, isRefugeeRange);

            worksheet.addValidationData(isRefugeeValidation);
        } catch (Exception e) {
            log.error("Error setting BMO template rules: {}", e.getMessage(), e);
        }
    }
}
