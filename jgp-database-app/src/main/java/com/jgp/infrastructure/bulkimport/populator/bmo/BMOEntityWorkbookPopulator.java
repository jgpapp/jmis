package com.jgp.infrastructure.bulkimport.populator.bmo;

import com.jgp.infrastructure.bulkimport.constants.BMOConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.populator.AbstractWorkbookPopulator;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

public class BMOEntityWorkbookPopulator extends AbstractWorkbookPopulator {

    @Override
    public void populate(Workbook workbook) {
        Sheet bmoSheet = workbook.createSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        setLayout(bmoSheet);
    }

    private void setLayout(Sheet worksheet) {
        Row rowHeader = worksheet.createRow(TemplatePopulateImportConstants.ROWHEADER_INDEX);
        rowHeader.setHeight(TemplatePopulateImportConstants.ROW_HEADER_HEIGHT);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_NAME_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TRAINING_PARTNER, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.JGP_ID_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_PHONE_NUMBER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.GENDER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.AGE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_LOCATION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.INDUSTRY_SECTOR_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_SEGMENT_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TA_DELIVERY_MODE, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_IS_REGISTERED, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BEST_MONTH_MONTHLY_REVENUE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.WORST_MONTH_MONTHLY_REVENUE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TOTAL_REGULAR_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.YOUTH_REGULAR_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TOTAL_CASUAL_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.YOUTH_CASUAL_EMPLOYEES_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.SAMPLE_RECORDS_KEPT_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TA_NEEDS_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.OTHER_TA_NEEDS_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TYPE_OF_TA_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.PERSON_WITH_DISABILITY_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.REFUGEE_STATUS_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.IS_APPLICANT_ELIGIBLE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.RECOMMENDED_FOR_FINANCE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.DATE_OF_PIPELINE_DECISION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.REFERRED_FI_BUSINESS_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);

        writeString(BMOConstants.BUSINESS_NAME_COL, rowHeader, "Name of business*");
        writeString(BMOConstants.TRAINING_PARTNER, rowHeader, "Training Partner*");
        writeString(BMOConstants.JGP_ID_COL, rowHeader, "Unique JGP ID (National ID)*");
        writeString(BMOConstants.BUSINESS_PHONE_NUMBER_COL, rowHeader, "Business phone number");
        writeString(BMOConstants.GENDER_COL, rowHeader, "Gender of owner*");
        writeString(BMOConstants.AGE_COL, rowHeader, "Age of owner (full years)*");
        writeString(BMOConstants.BUSINESS_LOCATION_COL, rowHeader, "Business Location (County)*");
        writeString(BMOConstants.INDUSTRY_SECTOR_COL, rowHeader, "Industry sector(Agriculture, Artists/artisans, Manufacturing, Trading & Retail, Other)");
        writeString(BMOConstants.BUSINESS_SEGMENT_COL, rowHeader, "Business segment*");
        writeString(BMOConstants.TA_DELIVERY_MODE, rowHeader, "TA delivery mode*(In person/Virtual/Mixed)");
        writeString(BMOConstants.BUSINESS_IS_REGISTERED, rowHeader, "Is your business registered?* (Yes/No)");
        writeString(BMOConstants.BEST_MONTH_MONTHLY_REVENUE_COL, rowHeader, "Monthly revenues in best month (KES)");
        writeString(BMOConstants.WORST_MONTH_MONTHLY_REVENUE_COL, rowHeader, "Monthly revenues in worst month (KES)");
        writeString(BMOConstants.TOTAL_REGULAR_EMPLOYEES_COL, rowHeader, "Total number of regular employees including owner*");
        writeString(BMOConstants.YOUTH_REGULAR_EMPLOYEES_COL, rowHeader, "Regular, of which are youth (18-35)*");
        writeString(BMOConstants.TOTAL_CASUAL_EMPLOYEES_COL, rowHeader, "Total number of casual employees excluding owner*");
        writeString(BMOConstants.YOUTH_CASUAL_EMPLOYEES_COL, rowHeader, "Casual, of which are youth (18-35)*");
        writeString(BMOConstants.SAMPLE_RECORDS_KEPT_COL, rowHeader, "Sample records kept*(Purchase record, Record of sales, Delivery records, Record of expenses, Receipts, Other)");
        writeString(BMOConstants.TA_NEEDS_COL, rowHeader, "TA needs*");
        writeString(BMOConstants.OTHER_TA_NEEDS_COL, rowHeader, "Other TA Needs");
        writeString(BMOConstants.TYPE_OF_TA_COL, rowHeader, "Type of TA*(Post-lending/Pre-lending/Non-lending/Mentorship/Voucher scheme)");
        writeString(BMOConstants.PERSON_WITH_DISABILITY_COL, rowHeader, "Person with Disability*(Yes/No)");

        writeString(BMOConstants.REFUGEE_STATUS_COL, rowHeader, "Refugee status*(Yes/No)");
        writeString(BMOConstants.IS_APPLICANT_ELIGIBLE_COL, rowHeader, "Is applicant eligible?");
        writeString(BMOConstants.RECOMMENDED_FOR_FINANCE_COL, rowHeader, "Recommended for finance");
        writeString(BMOConstants.DATE_OF_PIPELINE_DECISION_COL, rowHeader, "Pipeline Decision Date (yyyy-MM-dd)");
        writeString(BMOConstants.REFERRED_FI_BUSINESS_COL, rowHeader, "FI business is referred to*");
        writeString(BMOConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL, rowHeader, "Date recorded by partner(yyyy-MM-dd)*");

    }
}
