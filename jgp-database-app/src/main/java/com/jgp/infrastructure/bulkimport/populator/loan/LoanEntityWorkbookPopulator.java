package com.jgp.infrastructure.bulkimport.populator.loan;

import com.jgp.infrastructure.bulkimport.constants.LoanConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.populator.AbstractWorkbookPopulator;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

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
        worksheet.setColumnWidth(LoanConstants.BUSINESS_REGISTRATION_NUMBER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.JGP_ID_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.BUSINESS_PHONE_NUMBER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.GENDER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.AGE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
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
        worksheet.setColumnWidth(LoanConstants.TRANCH_ALLOCATED_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.TRANCH_AMOUNT_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.LOAN_IDENTIFIER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);


        writeString(LoanConstants.PARTICIPANT_NAME_COL, rowHeader, "Participant Name*");
        writeString(LoanConstants.BUSINESS_NAME_COL, rowHeader, "Business Name");
        writeString(LoanConstants.BUSINESS_REGISTRATION_NUMBER_COL, rowHeader, "Business Registration Number");
        writeString(LoanConstants.JGP_ID_COL, rowHeader, "JGP ID (National ID/Passport/Alien Number)*");
        writeString(LoanConstants.BUSINESS_PHONE_NUMBER_COL, rowHeader, "Business phone number*");
        writeString(LoanConstants.GENDER_COL, rowHeader, "Gender of owner* (Male/Female/Intersex)");
        writeString(LoanConstants.AGE_COL, rowHeader, "Age of owner (full years)*");
        writeString(LoanConstants.BUSINESS_LOCATION_COL, rowHeader, "Business Location* (Valid County name e.g. Nairobi, Kisumu, Mombasa, etc.)");
        writeString(LoanConstants.INDUSTRY_SECTOR_COL, rowHeader, "Industry sector*(Agriculture, Artists/artisans, Manufacturing, Trading & Retail, Other)");
        writeString(LoanConstants.PIPELINE_SOURCE, rowHeader, "Pipeline source*");
        writeString(LoanConstants.DATE_APPLIED, rowHeader, "Date loan application(yyyy-MM-dd example:2000-04-20)*");
        setDateColumnFormat(worksheet.getWorkbook(), TemplatePopulateImportConstants.LOAN_SHEET_NAME, LoanConstants.DATE_APPLIED);
        writeString(LoanConstants.DATE_DISBURSED, rowHeader, "Date loan disbursed(yyyy-MM-dd example:2000-04-20)*");
        setDateColumnFormat(worksheet.getWorkbook(), TemplatePopulateImportConstants.LOAN_SHEET_NAME, LoanConstants.DATE_DISBURSED);
        writeString(LoanConstants.LOAN_AMOUNT_KES, rowHeader, "Loan Amount (KES)*");
        writeString(LoanConstants.LOAN_DURATION, rowHeader, "Loan duration (months)*");
        writeString(LoanConstants.OUT_STANDING_AMOUNT, rowHeader, "Outstanding amount*");
        writeString(LoanConstants.REPAID_LOAN_AMOUNT, rowHeader, "Repaid Loan Amount (KES)");
        writeString(LoanConstants.LOAN_QUALITY, rowHeader, "Loan quality*(Normal, Watch, Substandard, Doubtful and Loss)");
        writeString(LoanConstants.TOTAL_REGULAR_EMPLOYEES_COL, rowHeader, "Total number of regular employees including owner*");
        writeString(LoanConstants.YOUTH_REGULAR_EMPLOYEES_COL, rowHeader, "Regular, of which are youth (18-35)*");
        writeString(LoanConstants.TOTAL_CASUAL_EMPLOYEES_COL, rowHeader, "Total number of casual employees excluding owner*");
        writeString(LoanConstants.YOUTH_CASUAL_EMPLOYEES_COL, rowHeader, "Casual, of which are youth (18-35)*");
        writeString(LoanConstants.LOANER_TYPE_COL, rowHeader, "Loaner Type* (New/Repeat)");
        writeString(LoanConstants.DATE_RECORDED_TO_JGP_DB_COL, rowHeader, "Date added to JGP database(yyyy-MM-dd example:2000-04-20)*");
        writeString(LoanConstants.LOAN_PRODUCT_COL, rowHeader, "Loan product* (Working Capital/Asset Finance/Stahimili/Purchase Order/Consignment Finance/Shariah Compliant)");
        writeString(LoanConstants.TRANCH_ALLOCATED_COL, rowHeader, "Tranch Allocated (Tranch 1/Tranch 2/Tranch 3/Tranch 4/Tranch 5/Not Applicable)");
        writeString(LoanConstants.TRANCH_AMOUNT_COL, rowHeader, "Tranch Amount (KES)");
        writeString(LoanConstants.LOAN_IDENTIFIER_COL, rowHeader, "Loan Identifier* (Unique identifier for the loan, e.g. Loan12345)*");
    }
}
