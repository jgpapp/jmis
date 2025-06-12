package com.jgp.shared.validator;

import com.jgp.infrastructure.bulkimport.importhandler.ImportHandlerUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Row;

import java.util.Map;

@Slf4j
public class DataValidator {

    private DataValidator() {
    }

    public static Double validateTemplateDoubleValue(int column, Row row, Map<Row, String> rowErrorMap) {
        try {
            return ImportHandlerUtils.readAsDouble(column, row);
        } catch (Exception e) {
            log.error("Invalid value for one colum", e);
            rowErrorMap.put(row, "Invalid Value for one/more columns that should be a number !!");
        }
        return null;
    }

    public static Integer validateTemplateIntegerValue(int column, Row row, Map<Row, String> rowErrorMap) {
        try {
            return ImportHandlerUtils.readAsInt(column, row);
        } catch (Exception e) {
            log.error("Invalid value for one colum", e);
            rowErrorMap.put(row, "Invalid Value for one/more columns that should be a number !!");
        }
        return null;
    }
}
