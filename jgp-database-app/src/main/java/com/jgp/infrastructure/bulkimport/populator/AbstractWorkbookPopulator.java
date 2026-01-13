package com.jgp.infrastructure.bulkimport.populator;

import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFDataValidationHelper;
import org.apache.poi.xssf.usermodel.XSSFSheet;

public abstract class AbstractWorkbookPopulator implements WorkbookPopulator {

    private static final int MAX_EXCEL_ROWS = 1048575;

    protected void writeString(int colIndex, Row row, String value) {
        row.createCell(colIndex).setCellValue(value);
    }

    protected void setDateColumnFormat(Workbook workbook, String workSheetName, int colIndex) {
        if (workbook == null || workSheetName == null) {
            return;
        }

        Sheet workSheet = workbook.getSheet(workSheetName);
        if (workSheet == null) {
            return;
        }

        if (!(workSheet instanceof XSSFSheet)) {
            throw new IllegalArgumentException("Only XLSX format is supported for date validation");
        }
        CreationHelper creationHelper = workbook.getCreationHelper();
        CellStyle dateCellStyle = workbook.createCellStyle();
        short dateFormat = creationHelper.createDataFormat().getFormat("yyyy-MM-dd");
        dateCellStyle.setDataFormat(dateFormat);

        workSheet.setDefaultColumnStyle(colIndex, dateCellStyle);

        DataValidation validation = createDateColumnValidation(colIndex, (XSSFSheet) workSheet);
        workSheet.addValidationData(validation);
    }

    private static DataValidation createDateColumnValidation(int colIndex, XSSFSheet workSheet) {
        DataValidationHelper dvHelper = new XSSFDataValidationHelper(workSheet);
        DataValidationConstraint dvConstraint = dvHelper.createDateConstraint(
                DataValidationConstraint.OperatorType.BETWEEN,
                "1900-01-01", "9999-12-31", "yyyy-MM-dd");
        CellRangeAddressList addressList = new CellRangeAddressList(1, MAX_EXCEL_ROWS, colIndex, colIndex);
        DataValidation validation = dvHelper.createValidation(dvConstraint, addressList);
        validation.setSuppressDropDownArrow(true);
        validation.setShowErrorBox(true);
        return validation;
    }
}
