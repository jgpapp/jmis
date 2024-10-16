package com.jgp.infrastructure.bulkimport.populator.bmo;

import com.jgp.infrastructure.bulkimport.constants.BMOConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.populator.AbstractWorkbookPopulator;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

public class BMOEntityWorkbookPopulator extends AbstractWorkbookPopulator {

    @Override
    public void populate(Workbook workbook, String dateFormat) {
        Sheet bmoSheet = workbook.createSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        setLayout(bmoSheet, dateFormat);
        //setRules(bmoSheet);
    }

    private void setLayout(Sheet worksheet, String dateFormat) {
        Row rowHeader = worksheet.createRow(TemplatePopulateImportConstants.ROWHEADER_INDEX);
        rowHeader.setHeight(TemplatePopulateImportConstants.ROW_HEADER_HEIGHT);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_NAME_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.JGP_ID_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_PHONE_NUMBER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.GENDER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.AGE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_LOCATION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.INDUSTRY_SECTOR_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_SEGMENT_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_REG_NUMBER, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.MEMBERSHIP_BMO_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BEST_MONTH_MONTHLY_REVENUE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.WORST_MONTH_MONTHLY_REVENUE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TOTAL_REGULAR_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.YOUTH_REGULAR_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TOTAL_CASUAL_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.YOUTH_CASUAL_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.SAMPLE_RECORDS_KEPT_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TA_NEEDS_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.PERSON_WITH_DISABILITY_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.REFUGEE_STATUS_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.APPLICATION_FORM_SUBMITTED_DATE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.IS_APPLICANT_ELIGIBLE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.NUMBER_TAS_ATTENDED_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.NUMBER_TA_SESSION_ATTENDED_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.RECOMMENDED_FOR_FINANCE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.DATE_OF_PIPELINE_DECISION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.REFERRED_FI_BUSINESS_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.DATE_RECORDED_TO_JGP_DB_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);

        writeString(BMOConstants.BUSINESS_NAME_COL, rowHeader, "Name of business*");
        writeString(BMOConstants.JGP_ID_COL, rowHeader, "Unique JGP ID (National ID)*");
        writeString(BMOConstants.BUSINESS_PHONE_NUMBER_COL, rowHeader, "Business phone number");
        writeString(BMOConstants.GENDER_COL, rowHeader, "Gender of owner");
        writeString(BMOConstants.AGE_COL, rowHeader, "Age of owner (full years)");
        writeString(BMOConstants.BUSINESS_LOCATION_COL, rowHeader, "Business Location (County)*");
        writeString(BMOConstants.INDUSTRY_SECTOR_COL, rowHeader, "Industry sector");
        writeString(BMOConstants.BUSINESS_SEGMENT_COL, rowHeader, "Business segment");
        writeString(BMOConstants.BUSINESS_REG_NUMBER, rowHeader, "Business registration number");
        writeString(BMOConstants.MEMBERSHIP_BMO_COL, rowHeader, "BMO Membership");
        writeString(BMOConstants.BEST_MONTH_MONTHLY_REVENUE_COL, rowHeader, "Monthly revenues in best month (KES)");
        writeString(BMOConstants.WORST_MONTH_MONTHLY_REVENUE_COL, rowHeader, "Monthly revenues in worst month (KES)");
        writeString(BMOConstants.TOTAL_REGULAR_EMPLOYEES_COL, rowHeader, "Total number of regular employees including owner");
        writeString(BMOConstants.YOUTH_REGULAR_EMPLOYEES_COL, rowHeader, "Regular, of which are youth (18-35)");
        writeString(BMOConstants.TOTAL_CASUAL_EMPLOYEES_COL, rowHeader, "Total number of casual employees excluding owner");
        writeString(BMOConstants.YOUTH_CASUAL_EMPLOYEES_COL, rowHeader, "Casual, of which are youth (18-35)");
        writeString(BMOConstants.SAMPLE_RECORDS_KEPT_COL, rowHeader, "Sample records kept");
        writeString(BMOConstants.TA_NEEDS_COL, rowHeader, "TA needs");
        writeString(BMOConstants.PERSON_WITH_DISABILITY_COL, rowHeader, "Person with Disability");

        writeString(BMOConstants.REFUGEE_STATUS_COL, rowHeader, "Refugee status");
        writeString(BMOConstants.APPLICATION_FORM_SUBMITTED_DATE_COL, rowHeader, "Date submitted (yyyy-MM-dd)");
        writeString(BMOConstants.IS_APPLICANT_ELIGIBLE_COL, rowHeader, "Is applicant eligible?");
        writeString(BMOConstants.NUMBER_TAS_ATTENDED_COL, rowHeader, "Number TAs attended");
        writeString(BMOConstants.NUMBER_TA_SESSION_ATTENDED_COL, rowHeader, "Number TA sessions attended");
        writeString(BMOConstants.RECOMMENDED_FOR_FINANCE_COL, rowHeader, "Recommended for finance");
        writeString(BMOConstants.DATE_OF_PIPELINE_DECISION_COL, rowHeader, "Pipeline Decision Date (yyyy-MM-dd)");
        writeString(BMOConstants.REFERRED_FI_BUSINESS_COL, rowHeader, "FI business is referred to");
        writeString(BMOConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL, rowHeader, "Date recorded by partner(yyyy-MM-dd)");
        writeString(BMOConstants.DATE_RECORDED_TO_JGP_DB_COL, rowHeader, "Date added to JGP database(yyyy-MM-dd)");

    }
}
