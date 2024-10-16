package com.jgp.infrastructure.bulkimport.populator.loan;

import com.jgp.infrastructure.bulkimport.constants.LoanConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.populator.AbstractWorkbookPopulator;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

public class LoanEntityWorkbookPopulator extends AbstractWorkbookPopulator {

    @Override
    public void populate(Workbook workbook, String dateFormat) {
        Sheet loanSheet = workbook.createSheet(TemplatePopulateImportConstants.LOAN_SHEET_NAME);
        setLayout(loanSheet, dateFormat);
        //setRules(loanSheet);
    }

    private void setLayout(Sheet worksheet, String dateFormat) {
        Row rowHeader = worksheet.createRow(TemplatePopulateImportConstants.ROWHEADER_INDEX);
        rowHeader.setHeight(TemplatePopulateImportConstants.ROW_HEADER_HEIGHT);
        worksheet.setColumnWidth(LoanConstants.BUSINESS_NAME_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.JGP_ID_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.BUSINESS_PHONE_NUMBER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.GENDER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.AGE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.BUSINESS_LOCATION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.INDUSTRY_SECTOR_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.BUSINESS_SEGMENT_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.PIPELINE_SOURCE, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.LOAN_STATUS, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.DATE_APPLIED, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.DATE_DISBURSED, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.VALUE_ACCESSED, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.LOAN_DURATION, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.OUT_STANDING_AMOUNT, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.LOAN_QUALITY, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.UNIQUE_VALUES, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(LoanConstants.DATE_RECORDED_TO_JGP_DB_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);

        writeString(LoanConstants.BUSINESS_NAME_COL, rowHeader, "Name of business*");
        writeString(LoanConstants.JGP_ID_COL, rowHeader, "Unique JGP ID (National ID)*");
        writeString(LoanConstants.BUSINESS_PHONE_NUMBER_COL, rowHeader, "Business phone number");
        writeString(LoanConstants.GENDER_COL, rowHeader, "Gender of owner");
        writeString(LoanConstants.AGE_COL, rowHeader, "Age of owner (full years)");
        writeString(LoanConstants.BUSINESS_LOCATION_COL, rowHeader, "Business Location(County)*");
        writeString(LoanConstants.INDUSTRY_SECTOR_COL, rowHeader, "Industry sector");
        writeString(LoanConstants.BUSINESS_SEGMENT_COL, rowHeader, "Business segment");
        writeString(LoanConstants.PIPELINE_SOURCE, rowHeader, "Pipeline source");
        writeString(LoanConstants.LOAN_STATUS, rowHeader, "Decision on loan");
        writeString(LoanConstants.DATE_APPLIED, rowHeader, "Date loan application(yyyy-MM-dd)");
        writeString(LoanConstants.DATE_DISBURSED, rowHeader, "Date loan disbursed(yyyy-MM-dd)");
        writeString(LoanConstants.VALUE_ACCESSED, rowHeader, "Amount Accessed (KES)");
        writeString(LoanConstants.LOAN_DURATION, rowHeader, "Loan duration (months)");
        writeString(LoanConstants.OUT_STANDING_AMOUNT, rowHeader, "Outstanding amount");
        writeString(LoanConstants.LOAN_QUALITY, rowHeader, "Loan quality");
        writeString(LoanConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL, rowHeader, "Date recorded by partner(yyyy-MM-dd)");
        writeString(LoanConstants.UNIQUE_VALUES, rowHeader, "Unique values");
        writeString(LoanConstants.DATE_RECORDED_TO_JGP_DB_COL, rowHeader, "Date added to JGP database(yyyy-MM-dd)");

    }
}
