package com.jgp.infrastructure.bulkimport.populator;

import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.LinkedHashMap;
import java.util.Map;

public class TemplateGuidePopulator {

    private static final Integer KEY_COLUMN_INDEX = 0;
    private static final Integer VALUE_COLUMN_INDEX = 1;

    private TemplateGuidePopulator() {
    }

    public static void populateGuide(Workbook workbook, GlobalEntityType globalEntityType) {
        Sheet guideSheet = workbook.createSheet(TemplatePopulateImportConstants.GUIDE_SHEET_NAME);
        guideSheet.setColumnWidth(KEY_COLUMN_INDEX, TemplatePopulateImportConstants.LARGE_COL_SIZE);
        guideSheet.setColumnWidth(VALUE_COLUMN_INDEX, TemplatePopulateImportConstants.EXTRALARGE_COL_SIZE);
        setLayout(guideSheet, globalEntityType);
    }

    private static void setLayout(Sheet sheet, GlobalEntityType globalEntityType) {
        Map<String, String> rulesMap = switch (globalEntityType){
            case TA_IMPORT_TEMPLATE -> getTATemplateRules();
            case LOAN_IMPORT_TEMPLATE -> getLendingTemplateRules();
            case MENTORSHIP_IMPORT_TEMPLATE -> getMentorshipTemplateRules();
            case MONITORING_IMPORT_TEMPLATE -> getOutComeMonitoringTemplateRules();
            default -> Map.of();
        };
        rulesMap.forEach((key, value) -> {
            var row = sheet.createRow(sheet.getLastRowNum() + 1);
            row.setHeight(TemplatePopulateImportConstants.ROW_HEADER_HEIGHT);
            var keyCell = row.createCell(KEY_COLUMN_INDEX);
            keyCell.setCellValue(key);
            var valueCell = row.createCell(VALUE_COLUMN_INDEX);
            valueCell.setCellValue(value);
        });
    }

    private  static Map<String, String> getTATemplateRules() {
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put("Participant Name", "The name of the participant. This field is mandatory.");
        map.put("Training Partner", "The name of the training partner. This field is mandatory.");

        return map;

    }

    private  static Map<String, String> getLendingTemplateRules() {
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put("Participant Name", "The name of the participant. This field is mandatory.");
        map.put("Business Name", "The name of the business. Not mandatory.");

        return map;
    }

    private  static Map<String, String> getMentorshipTemplateRules() {
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put("Mentorship Date", "The date of the mentorship in the format YYYY-MM-DD example:2000-04-20. This field is mandatory.");
        map.put("Mentor Organization", "The name of the mentor organization. Can be one of JETs/KEPSA/4G/GBF/KNCCI/PBP/GROOTS. This field is mandatory.");

        return map;
    }

    private  static Map<String, String> getOutComeMonitoringTemplateRules() {
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put("Survey Date", "The date of the survey in the format YYYY-MM-DD example:2000-04-20. This field is mandatory.");
        map.put("Survey Language", "The name of the survey language. This field is mandatory.");

        return map;
    }
}
