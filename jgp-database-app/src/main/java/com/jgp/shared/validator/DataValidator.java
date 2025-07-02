package com.jgp.shared.validator;

import com.jgp.infrastructure.bulkimport.importhandler.ImportHandlerUtils;
import com.jgp.monitoring.dto.OutComeMonitoringRequestDto;
import com.jgp.monitoring.dto.OutComeMonitoringResponseDto;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Row;

import java.util.Map;
import java.util.Set;

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

    public static void validateMonitoringData(OutComeMonitoringRequestDto dto, Row row, Map<Row, String> rowErrorMap) {
        // Create a Validator instance
        Validator validator;
        try (ValidatorFactory factory = Validation.buildDefaultValidatorFactory()) {
            validator = factory.getValidator();
        }

        // Validate the object
        Set<ConstraintViolation<OutComeMonitoringRequestDto>> violations = validator.validate(dto);

        // Get the first error, if any
        if (null == rowErrorMap.get(row) && !violations.isEmpty()) {
            ConstraintViolation<OutComeMonitoringRequestDto> firstViolation = violations.iterator().next();
            rowErrorMap.put(row, firstViolation.getMessage());
        }
    }
}
