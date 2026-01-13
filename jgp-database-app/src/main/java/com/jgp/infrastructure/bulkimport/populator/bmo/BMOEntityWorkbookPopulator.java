package com.jgp.infrastructure.bulkimport.populator.bmo;

import com.jgp.infrastructure.bulkimport.constants.BMOConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.populator.AbstractWorkbookPopulator;
import com.jgp.infrastructure.bulkimport.populator.TemplateGuidePopulator;
import com.jgp.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.SpreadsheetVersion;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFDataValidationHelper;

import java.util.List;

@Slf4j
public class BMOEntityWorkbookPopulator extends AbstractWorkbookPopulator {

    @Override
    public void populate(Workbook workbook) {
        Sheet bmoSheet = workbook.createSheet(TemplatePopulateImportConstants.BMO_SHEET_NAME);
        setLayout(bmoSheet);
        TemplateGuidePopulator.populateGuide(workbook, GlobalEntityType.TA_IMPORT_TEMPLATE);
    }

    private void setLayout(Sheet worksheet) {
        Row rowHeader = worksheet.createRow(TemplatePopulateImportConstants.ROWHEADER_INDEX);
        rowHeader.setHeight(TemplatePopulateImportConstants.ROW_HEADER_HEIGHT);
        worksheet.setColumnWidth(BMOConstants.PARTICIPANT_NAME_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TRAINING_PARTNER, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.JGP_ID_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_PHONE_NUMBER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_ALTERNATIVE_PHONE_NUMBER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.GENDER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.AGE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_LOCATION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_SUB_COUNTY_LOCATION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.INDUSTRY_SECTOR_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_SEGMENT_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.TA_DELIVERY_MODE, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(BMOConstants.BUSINESS_REGISTRATION_NUMBER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
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

        writeString(BMOConstants.PARTICIPANT_NAME_COL, rowHeader, "Participant Name*");
        writeString(BMOConstants.TRAINING_PARTNER, rowHeader, "Training Partner*");
        writeString(BMOConstants.JGP_ID_COL, rowHeader, "JGP ID (National ID/Passport/Alien Number)*");
        writeString(BMOConstants.BUSINESS_PHONE_NUMBER_COL, rowHeader, "Business phone number");
        writeString(BMOConstants.BUSINESS_ALTERNATIVE_PHONE_NUMBER_COL, rowHeader, "Alternative phone number");
        writeString(BMOConstants.GENDER_COL, rowHeader, "Gender of owner* (Male/Female/Intersex)");
        writeString(BMOConstants.AGE_COL, rowHeader, "Age of owner (full years)*");
        writeString(BMOConstants.BUSINESS_LOCATION_COL, rowHeader, "Business Location* (Valid County name e.g. Nairobi, Kisumu, Mombasa, etc.)");
        writeString(BMOConstants.BUSINESS_SUB_COUNTY_LOCATION_COL, rowHeader, "Sub-County");
        writeString(BMOConstants.INDUSTRY_SECTOR_COL, rowHeader, "Industry sector(Agriculture, Artists/artisans, Manufacturing, Trading & Retail, Other)");
        writeString(BMOConstants.BUSINESS_SEGMENT_COL, rowHeader, "Business segment*(Micro/SME)");
        writeString(BMOConstants.TA_DELIVERY_MODE, rowHeader, "TA delivery mode*(In person/Virtual/Mixed)");
        writeString(BMOConstants.BUSINESS_REGISTRATION_NUMBER_COL, rowHeader, "Business Registration Number");
        writeString(BMOConstants.BEST_MONTH_MONTHLY_REVENUE_COL, rowHeader, "Monthly revenues in best month (KES)");
        writeString(BMOConstants.WORST_MONTH_MONTHLY_REVENUE_COL, rowHeader, "Monthly revenues in worst month (KES)");
        writeString(BMOConstants.TOTAL_REGULAR_EMPLOYEES_COL, rowHeader, "Total number of regular employees including owner*");
        writeString(BMOConstants.YOUTH_REGULAR_EMPLOYEES_COL, rowHeader, "Regular, of which are youth (18-35)*");
        writeString(BMOConstants.TOTAL_CASUAL_EMPLOYEES_COL, rowHeader, "Total number of casual employees excluding owner*");
        writeString(BMOConstants.YOUTH_CASUAL_EMPLOYEES_COL, rowHeader, "Casual, of which are youth (18-35)*");
        writeString(BMOConstants.SAMPLE_RECORDS_KEPT_COL, rowHeader, "Sample records kept*(Purchase record/Record of sales/Delivery records/Record of expenses/Receipts/Other)");
        writeString(BMOConstants.TA_NEEDS_COL, rowHeader, "TA needs*(Financial Literacy/Record Keeping/Digitization/Market Access/Other)");
        writeString(BMOConstants.OTHER_TA_NEEDS_COL, rowHeader, "Other TA Needs");
        writeString(BMOConstants.TYPE_OF_TA_COL, rowHeader, "Type of TA*(Post-lending/Pre-lending/Non-lending/Mentorship/Voucher scheme)");
        writeString(BMOConstants.PERSON_WITH_DISABILITY_COL, rowHeader, "Person with Disability*(Yes/No)");

        writeString(BMOConstants.REFUGEE_STATUS_COL, rowHeader, "Refugee status*(Yes/No)");
        writeString(BMOConstants.IS_APPLICANT_ELIGIBLE_COL, rowHeader, "Is applicant eligible?(Yes/No)");
        writeString(BMOConstants.RECOMMENDED_FOR_FINANCE_COL, rowHeader, "Recommended for finance (Yes/No)");
        writeString(BMOConstants.DATE_OF_PIPELINE_DECISION_COL, rowHeader, "Pipeline Decision Date (yyyy-MM-dd example:2000-04-20)");
        setDateColumnFormat(worksheet.getWorkbook(), TemplatePopulateImportConstants.BMO_SHEET_NAME, BMOConstants.DATE_OF_PIPELINE_DECISION_COL);
        writeString(BMOConstants.REFERRED_FI_BUSINESS_COL, rowHeader, "FI business is referred to*");
        writeString(BMOConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL, rowHeader, "Training date(yyyy-MM-dd example:2000-04-20)*");
        setDateColumnFormat(worksheet.getWorkbook(), TemplatePopulateImportConstants.BMO_SHEET_NAME, BMOConstants.DATE_RECORD_ENTERED_BY_PARTNER_COL);

    }

    private void setRules(Sheet worksheet){
        try {
            CellRangeAddressList isRefugeeRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.REFUGEE_STATUS_COL, BMOConstants.REFUGEE_STATUS_COL);
            CellRangeAddressList genderRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.GENDER_COL, BMOConstants.GENDER_COL);
            CellRangeAddressList countyRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.BUSINESS_LOCATION_COL, BMOConstants.BUSINESS_LOCATION_COL);

            CellRangeAddressList industrySectorRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.INDUSTRY_SECTOR_COL, BMOConstants.INDUSTRY_SECTOR_COL);
            CellRangeAddressList businessSegmentRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.BUSINESS_SEGMENT_COL, BMOConstants.BUSINESS_SEGMENT_COL);
            CellRangeAddressList taDeliveryModeRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.TA_DELIVERY_MODE, BMOConstants.TA_DELIVERY_MODE);
            CellRangeAddressList taTypeRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.TYPE_OF_TA_COL, BMOConstants.TYPE_OF_TA_COL);
            CellRangeAddressList personWithDisabilityRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.PERSON_WITH_DISABILITY_COL, BMOConstants.PERSON_WITH_DISABILITY_COL);
            CellRangeAddressList applicantEligibleRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.IS_APPLICANT_ELIGIBLE_COL, BMOConstants.IS_APPLICANT_ELIGIBLE_COL);
            CellRangeAddressList recommendedForFinanceRange = new CellRangeAddressList(1, SpreadsheetVersion.EXCEL97.getLastRowIndex(),
                    BMOConstants.RECOMMENDED_FOR_FINANCE_COL, BMOConstants.RECOMMENDED_FOR_FINANCE_COL);

            DataValidationHelper validationHelper = new XSSFDataValidationHelper((org.apache.poi.xssf.usermodel.XSSFSheet) worksheet);
            final var counties = CommonUtil.getKenyanCountiesMap().values().stream().toList();

            setNames(worksheet, counties);

            DataValidationConstraint yesNoConstraint = validationHelper.createExplicitListConstraint(new String[] { "Yes", "No" });
            DataValidationConstraint genderConstraint = validationHelper.createExplicitListConstraint(new String[] { "Male", "Female", "Other" });
            DataValidationConstraint countyNameConstraint = validationHelper.createFormulaListConstraint("County");
            DataValidationConstraint industrySectorConstraint = validationHelper.createExplicitListConstraint(new String[] { "Agriculture", "Artists/artisans", "Manufacturing", " Trading & Retail", "Other" });
            DataValidationConstraint businessSegmentsConstraint = validationHelper.createExplicitListConstraint(new String[] { "Micro", "SME" });
            DataValidationConstraint taDeliveryModeConstraint = validationHelper.createExplicitListConstraint(new String[] { "In person", "Virtual", "Mixed" });
            DataValidationConstraint taTypeConstraint = validationHelper.createExplicitListConstraint(new String[] { "Post-lending", "Pre-lending", "Non-lending", "Mentorship", "Voucher scheme" });

            DataValidation isRefugeeValidation = validationHelper.createValidation(yesNoConstraint, isRefugeeRange);
            DataValidation genderValidation = validationHelper.createValidation(genderConstraint, genderRange);
            DataValidation countyValidation = validationHelper.createValidation(countyNameConstraint, countyRange);
            DataValidation industrySectorValidation = validationHelper.createValidation(industrySectorConstraint, industrySectorRange);
            DataValidation businessSegmentValidation = validationHelper.createValidation(businessSegmentsConstraint, businessSegmentRange);
            DataValidation taDeliveryModeValidation = validationHelper.createValidation(taDeliveryModeConstraint, taDeliveryModeRange);
            DataValidation taTypeValidation = validationHelper.createValidation(taTypeConstraint, taTypeRange);
            DataValidation personWithDisabilityValidation = validationHelper.createValidation(yesNoConstraint, personWithDisabilityRange);
            DataValidation applicantEligibleValidation = validationHelper.createValidation(yesNoConstraint, applicantEligibleRange);
            DataValidation recommendedForFinanceValidation = validationHelper.createValidation(yesNoConstraint, recommendedForFinanceRange);

            worksheet.addValidationData(isRefugeeValidation);
            worksheet.addValidationData(genderValidation);
            worksheet.addValidationData(countyValidation);
            worksheet.addValidationData(industrySectorValidation);
            worksheet.addValidationData(businessSegmentValidation);
            worksheet.addValidationData(taDeliveryModeValidation);
            worksheet.addValidationData(taTypeValidation);
            worksheet.addValidationData(personWithDisabilityValidation);
            worksheet.addValidationData(applicantEligibleValidation);
            worksheet.addValidationData(recommendedForFinanceValidation);
        } catch (Exception e) {
            log.error("Error setting BMO template rules: {}", e.getMessage(), e);
        }
    }

    private void setNames(Sheet workSheet, List<String> counties) {
        Workbook bmoWorkbook = workSheet.getWorkbook();
        Sheet optionsSheet = bmoWorkbook.createSheet(TemplatePopulateImportConstants.OPTIONS_SHEET_NAME);
        Name officeGroup = bmoWorkbook.createName();
        officeGroup.setNameName("County");
        officeGroup.setRefersToFormula(TemplatePopulateImportConstants.OPTIONS_SHEET_NAME + "!$A$1:$A$" + (counties.size() + 1));


        for (int i = 0; i < counties.size(); i++) {
            Row row = optionsSheet.createRow(i);
            row.createCell(0).setCellValue(counties.get(i));
        }
    }
}
