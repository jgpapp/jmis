package com.jgp.infrastructure.bulkimport.populator.mentorship;

import com.jgp.infrastructure.bulkimport.constants.MentorShipConstants;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.populator.AbstractWorkbookPopulator;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

@Slf4j
public class MentorshipWorkbookPopulator extends AbstractWorkbookPopulator {

    @Override
    public void populate(Workbook workbook) {
        Sheet loanSheet = workbook.createSheet(TemplatePopulateImportConstants.MENTOR_SHIP_SHEET_NAME);
        setLayout(loanSheet);
    }

    private void setLayout(Sheet worksheet) {
        Row rowHeader = worksheet.createRow(TemplatePopulateImportConstants.ROWHEADER_INDEX);
        rowHeader.setHeight(TemplatePopulateImportConstants.ROW_HEADER_HEIGHT);
        worksheet.setColumnWidth(MentorShipConstants.MENTORSHIP_DATE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.MENTOR_ORGANIZATION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.BMO_MEMBERSHIP_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.OTHER_BMO_MEMBERSHIP_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.MENTORSHIP_DELIVERY_MODE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.PARTICIPANT_NAME_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.BUSINESS_NAME_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.JGP_ID_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.BUSINESS_PHONE_NUMBER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.MENTEE_AGE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.MENTEE_GENDER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.IS_MENTEE_DISABLED, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.MENTEE_DISABILITY_TYPE, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.BUSINESS_COUNTY_LOCATION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.BUSINESS_SUB_COUNTY_LOCATION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.GEO_LOCATION_LATITUDE, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.GEO_LOCATION_LONGITUDE, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.BUSINESS_CATEGORY_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.OTHER_BUSINESS_CATEGORY_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.BUSINESS_SITUATION_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.BUSINESS_FINANCIER_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.LOAN_MADE_HIRE_MORE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.NEW_EMPLOYEES_18_35_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.DID_TRAINING_CONTRIBUTE_TO_REVENUE_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.REVENUE_INCREASE_PERCENT_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.USEFUL_TRAINING_TOPICS_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.OTHER_USEFUL_TRAINING_TOPICS_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.AREAS_NEEDING_SUPPORT, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.OTHER_AREA_NEEDING_SUPPORT, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.BUSINESS_SEGMENT, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.MSME_SESSIONS_COVERED_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.SME_SESSIONS_COVERED_COL, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.IDENTIFIED_BUSINESS_GAPS, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.AGREED_ACTION_FOR_GAP_1, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);
        worksheet.setColumnWidth(MentorShipConstants.ADDITIONAL_SUPPORT_NEEDED, TemplatePopulateImportConstants.MEDIUM_COL_SIZE);


        writeString(MentorShipConstants.MENTORSHIP_DATE_COL, rowHeader, "Mentorship Date*(yyyy-MM-dd example:2000-04-20)");
        setDateColumnFormat(worksheet.getWorkbook(), TemplatePopulateImportConstants.MENTOR_SHIP_SHEET_NAME, MentorShipConstants.MENTORSHIP_DATE_COL);
        writeString(MentorShipConstants.MENTOR_ORGANIZATION_COL, rowHeader, "Mentor Organization* (JETs/KEPSA/4G/GBF/KNCCI/PBP/GROOTS)");
        writeString(MentorShipConstants.BMO_MEMBERSHIP_COL, rowHeader, "BMO Membership* (KNCCI/KEPSA/PBP/GROOTS/Other)");
        writeString(MentorShipConstants.OTHER_BMO_MEMBERSHIP_COL, rowHeader, "If other, specify* (BMO membership)");
        writeString(MentorShipConstants.MENTORSHIP_DELIVERY_MODE_COL, rowHeader, "Mentorship delivery mode* (In person/Virtual/Mixed)");
        writeString(MentorShipConstants.PARTICIPANT_NAME_COL, rowHeader, "Mentee name* (Participant name)");
        writeString(MentorShipConstants.BUSINESS_NAME_COL, rowHeader, "Name of Business*");
        writeString(MentorShipConstants.JGP_ID_COL, rowHeader, "JGP ID (National ID/Passport/Alien Number)*");
        writeString(MentorShipConstants.BUSINESS_PHONE_NUMBER_COL, rowHeader, "Mobile phone Number*");
        writeString(MentorShipConstants.MENTEE_AGE_COL, rowHeader, "Age of Mentee* (full years)");
        writeString(MentorShipConstants.MENTEE_GENDER_COL, rowHeader, "Gender of Mentee* (Male/Female/Intersex)");
        writeString(MentorShipConstants.IS_MENTEE_DISABLED, rowHeader, "Do you have a disability?*(Yes/No)");
        writeString(MentorShipConstants.MENTEE_DISABILITY_TYPE, rowHeader, "Type of Disability/Impairment ?*(Visual impairment/Hearing impairment/Speech impairment/Physical impairment/Intellectual impairment/Psychosocial impairment/Multiple impairments)");
        writeString(MentorShipConstants.BUSINESS_COUNTY_LOCATION_COL, rowHeader, "Business Location*(Valid County name e.g. Nairobi, Kisumu, Mombasa, etc.)");
        writeString(MentorShipConstants.BUSINESS_SUB_COUNTY_LOCATION_COL, rowHeader, "Sub-County*");
        writeString(MentorShipConstants.GEO_LOCATION_LATITUDE, rowHeader, "Geo Location Latitude");
        writeString(MentorShipConstants.GEO_LOCATION_LONGITUDE, rowHeader, "Geo Location Longitude");
        writeString(MentorShipConstants.BUSINESS_CATEGORY_COL, rowHeader, "Business Category*(Agriculture, Artists/artisans, Manufacturing, Trading & Retail, Other)");
        writeString(MentorShipConstants.OTHER_BUSINESS_CATEGORY_COL, rowHeader, "If other specify (Business Category)");
        writeString(MentorShipConstants.BUSINESS_SITUATION_COL, rowHeader, "Business Situation* (I attended the training but did not receive a loan, I received a loan but did not attend the training, I attended both the training and received a loan)");
        writeString(MentorShipConstants.BUSINESS_FINANCIER_COL, rowHeader, "Financier*(GBF/4G/PBP)");
        writeString(MentorShipConstants.LOAN_MADE_HIRE_MORE_COL, rowHeader, "Has the loan contributed to the ability to hire new employees or expand the team?(Yes/No)");
        writeString(MentorShipConstants.NEW_EMPLOYEES_18_35_COL, rowHeader, "If yes, please state the number of new employees hired (age between 18-35).");
        writeString(MentorShipConstants.DID_TRAINING_CONTRIBUTE_TO_REVENUE_COL, rowHeader, "Has the technical training contributed to an increase in your business revenue?(Yes/No)");
        writeString(MentorShipConstants.REVENUE_INCREASE_PERCENT_COL, rowHeader, "If yes, please estimate the percentage increase in revenue since the training.");
        writeString(MentorShipConstants.USEFUL_TRAINING_TOPICS_COL, rowHeader, "Which training topics have been the most useful to you? (Financial literacy/Record Keeping/Digital Skills/Market access/Artisan Development/Others)");
        writeString(MentorShipConstants.OTHER_USEFUL_TRAINING_TOPICS_COL, rowHeader, "If others, specify (training topics)");
        writeString(MentorShipConstants.AREAS_NEEDING_SUPPORT, rowHeader, "Which areas require further clarification or additional support?(Financial literacy/Record Keeping/Digital Skills/Market access/Artisan Development/Others)");
        writeString(MentorShipConstants.OTHER_AREA_NEEDING_SUPPORT, rowHeader, "If other specify (areas of support)");
        writeString(MentorShipConstants.BUSINESS_SEGMENT, rowHeader, "Business segment (SME/Micro (MSME))");
        writeString(MentorShipConstants.MSME_SESSIONS_COVERED_COL, rowHeader, "MSME Sessions Covered (Session 1/Session 2)");
        writeString(MentorShipConstants.SME_SESSIONS_COVERED_COL, rowHeader, "SME Sessions Covered (Session 1/Session 2/Session 3/Session 4/Session 5)");
        writeString(MentorShipConstants.IDENTIFIED_BUSINESS_GAPS, rowHeader, "What are the top business gaps or needs identified by both mentees and mentors?*(Please separate multiple gaps by a comma)");
        writeString(MentorShipConstants.AGREED_ACTION_FOR_GAP_1, rowHeader, "What actions did the mentor and mentee agree upon to address and close the identified and prioritized business gaps?");
        writeString(MentorShipConstants.ADDITIONAL_SUPPORT_NEEDED, rowHeader, "What additional support or resources would be most helpful for your business at this point?");


    }
}
