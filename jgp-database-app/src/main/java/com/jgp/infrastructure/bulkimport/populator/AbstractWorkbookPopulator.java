package com.jgp.infrastructure.bulkimport.populator;

import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFDataValidationHelper;
import org.apache.poi.xssf.usermodel.XSSFSheet;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.util.regex.Pattern;

public abstract class AbstractWorkbookPopulator implements WorkbookPopulator {

    private static final Pattern NAME_REGEX = Pattern.compile("[ @#&()<>,;.:$£€§°'\\\\/=!\\?\\-\\+\\*\"\\[\\]]");

    protected void writeInt(int colIndex, Row row, int value) {
        row.createCell(colIndex).setCellValue(value);
    }

    protected void writeLong(int colIndex, Row row, long value) {
        row.createCell(colIndex).setCellValue((double) value);
    }

    protected void writeString(int colIndex, Row row, String value) {
        row.createCell(colIndex).setCellValue(value);
    }

    protected void writeBoolean(int colIndex, Row row, Boolean value) {
        row.createCell(colIndex).setCellValue(value);
    }

    protected void writeDouble(int colIndex, Row row, double value) {
        row.createCell(colIndex).setCellValue(value);
    }

    protected void writeFormula(int colIndex, Row row, String formula) {
        row.createCell(colIndex).setCellFormula(formula);
    }

    protected void writeDate(int colIndex, Row row, String value, CellStyle dateCellStyle, String dateFormat) {
        try {
            DateTimeFormatter formatinDB;
            if (value.matches("\\d{4}-\\d{1,2}-\\d{1,2}")) {
                formatinDB = new DateTimeFormatterBuilder().appendPattern("yyyy-M-d").toFormatter();
            } else if (value.matches("\\d{1,2}/\\d{1,2}/\\d{4}")) {
                formatinDB = new DateTimeFormatterBuilder().appendPattern("d/M/yyyy").toFormatter();
            } else if (value.matches("\\d{1,2} \\w{3,12} \\d{4}")) {
                formatinDB = new DateTimeFormatterBuilder().appendPattern("d MMMM yyyy").toFormatter();
            } else {
                throw new IllegalArgumentException("Unrecognised format of date value: " + value);
            }
            LocalDate date1 = LocalDate.parse(value, formatinDB);
            DateTimeFormatter expectedFormat = new DateTimeFormatterBuilder().appendPattern(dateFormat).toFormatter();
            row.createCell(colIndex).setCellValue(expectedFormat.format(date1));
            row.getCell(colIndex).setCellStyle(dateCellStyle);
        } catch (DateTimeParseException pe) {
            throw new IllegalArgumentException(pe);
        }
    }

    protected void writeBigDecimal(int colIndex, Row row, BigDecimal value) {
        row.createCell(colIndex).setCellValue(((value != null) ? value.doubleValue() : 0));
    }

    /**
     * See {@link Name#setNameName(String)} and https://issues.apache.org/jira/browse/FINERACT-1256.
     */
    protected void setSanitized(Name poiName, String roughName) {
        String sanitized = NAME_REGEX.matcher(roughName.trim()).replaceAll("_");
        poiName.setNameName(sanitized);
    }

    protected void setDateColumnFormat(Workbook workbook, String workSheetName, int colIndex) {
        CreationHelper creationHelper = workbook.getCreationHelper();
        CellStyle dateCellStyle = workbook.createCellStyle();
        short dateFormat = creationHelper.createDataFormat().getFormat("yyyy-MM-dd");
        dateCellStyle.setDataFormat(dateFormat);
        Sheet workSheet = workbook.getSheet(workSheetName);
        if (null != workSheet) {
           workSheet.setDefaultColumnStyle(colIndex, dateCellStyle);
        }else {
            return;
        }
        DataValidation validation = createDateColumnValidation(colIndex, (XSSFSheet) workSheet);
        workSheet.addValidationData(validation);
    }

    private static DataValidation createDateColumnValidation(int colIndex, XSSFSheet workSheet) {
        DataValidationHelper dvHelper = new XSSFDataValidationHelper(workSheet);
        DataValidationConstraint dvConstraint = dvHelper.createDateConstraint(
                DataValidationConstraint.OperatorType.BETWEEN,
                "1900-01-01", "9999-12-31", "yyyy-MM-dd");
        CellRangeAddressList addressList = new CellRangeAddressList(1, 1048575, colIndex, colIndex); // A2:A1048576
        DataValidation validation = dvHelper.createValidation(dvConstraint, addressList);
        validation.setSuppressDropDownArrow(true);
        validation.setShowErrorBox(true);
        return validation;
    }
}
