package com.jgp.shared.validator;

import com.google.i18n.phonenumbers.PhoneNumberUtil;
import com.google.i18n.phonenumbers.Phonenumber;
import com.jgp.infrastructure.bulkimport.importhandler.ImportHandlerUtils;
import com.jgp.monitoring.dto.OutComeMonitoringRequestDto;
import com.jgp.util.CommonUtil;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.text.WordUtils;
import org.apache.poi.ss.usermodel.Row;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeParseException;
import java.util.Map;
import java.util.Set;

@Slf4j
public class DataValidator {

    private DataValidator() {
    }

    public static Double validateTemplateDoubleValue(int column, Row row, String columnName, Map<Integer, String> rowErrorMap, boolean isRequired) {
        try {
            final var valAsString = ImportHandlerUtils.readAsString(column, row);
            if (null == valAsString || valAsString.isBlank()) {
                if (isRequired){
                    rowErrorMap.put(row.getRowNum(), String.format("%s is required field !!", WordUtils.capitalizeFully(columnName)));
                }
                return null;
            }
            ImportHandlerUtils.validateCellDoesNotContainFormular(row.getCell(column), rowErrorMap);
            return ImportHandlerUtils.readAsDouble(column, row);
        } catch (Exception e) {
            log.error("Invalid value for one column: {}", WordUtils.capitalizeFully(columnName), e);
            rowErrorMap.put(row.getRowNum(), String.format("Invalid Value for %s that should be a number !!", WordUtils.capitalizeFully(columnName)));
        }
        return null;
    }

    public static Integer validateTemplateIntegerValue(int column, Row row, String columnName, Map<Integer, String> rowErrorMap, boolean isRequired) {
        try {
            final var valAsString = ImportHandlerUtils.readAsString(column, row);
            if (null == valAsString || valAsString.isBlank()) {
                if (isRequired){
                    rowErrorMap.put(row.getRowNum(), String.format("%s is required field !", WordUtils.capitalizeFully(columnName)));
                }
                return null;
            }
            return ImportHandlerUtils.readAsInt(column, row);
        } catch (Exception e) {
            log.error("Invalid number for one column: {}", WordUtils.capitalizeFully(columnName), e);
            rowErrorMap.put(row.getRowNum(), String.format("Invalid Value for %s that should be a number !!", WordUtils.capitalizeFully(columnName)));
        }
        return null;
    }

    public static void validateMonitoringData(OutComeMonitoringRequestDto dto, Map<Integer, String> rowErrorMap) {
        // Create a Validator instance
        Validator validator = getValidator();

        // Validate the object
        Set<ConstraintViolation<OutComeMonitoringRequestDto>> violations = validator.validate(dto);

        // Get the first error, if any
        if (null == rowErrorMap.get(dto.rowIndex()) && !violations.isEmpty()) {
            ConstraintViolation<OutComeMonitoringRequestDto> firstViolation = violations.iterator().next();
            rowErrorMap.put(dto.rowIndex(), firstViolation.getMessage());
        }
    }

    public static CommonUtil.KenyanCounty validateCountyName(int column, Row row, Map<Integer, String> rowErrorMap) {
            final var countyName = ImportHandlerUtils.readAsString(column, row);
            final var locationCounty = CommonUtil.KenyanCounty.getKenyanCountyFromName(countyName).orElse(CommonUtil.KenyanCounty.UNKNOWN);
            if (CommonUtil.KenyanCounty.UNKNOWN.equals(locationCounty)) {
                rowErrorMap.put(row.getRowNum(), "Invalid county name !!");
                return CommonUtil.KenyanCounty.UNKNOWN;
            }
            return locationCounty;
    }

    public static LocalDate validateLocalDate(int column, Row row, Map<Integer, String> rowErrorMap, String dateFieldName, boolean isRequired) {
        try {
            final var formattedDate = ImportHandlerUtils.readAsISOFormattedDate(column, row);
            if (null == formattedDate && isRequired) {
                rowErrorMap.put(row.getRowNum(), String.format("%s is required field !!", WordUtils.capitalizeFully(dateFieldName)));
                return  LocalDate.now(ZoneId.systemDefault());
            }
            return formattedDate;
        } catch (DateTimeParseException e) {
            log.error("Invalid value for {} colum", dateFieldName, e);
            rowErrorMap.put(row.getRowNum(), String.format("%s must be formatted as 'yyyy-MM-dd' !!", WordUtils.capitalizeFully(dateFieldName)));
            return  LocalDate.now(ZoneId.systemDefault());
        }
    }

    public static String validatePhoneNumber(int column, Row row, Map<Integer, String> rowErrorMap) {
        try {
            var phoneNumber = ImportHandlerUtils.readAsString(column, row);
            if (null == phoneNumber || phoneNumber.isBlank()) {
                rowErrorMap.put(row.getRowNum(), "Phone number is required !!");
                return null;
            }
            if (!phoneNumber.startsWith("+254") && !phoneNumber.startsWith("254")) {
                rowErrorMap.put(row.getRowNum(), "Phone number must start with +254 or 254 !!");
                return phoneNumber;
            }
            if (!phoneNumber.startsWith("+")) {
                phoneNumber = String.format("+%s", phoneNumber);
            }
            PhoneNumberUtil phoneNumberUtil = PhoneNumberUtil.getInstance();
            Phonenumber.PhoneNumber phone = phoneNumberUtil.parse(phoneNumber, Phonenumber.PhoneNumber.CountryCodeSource.UNSPECIFIED.name());
            if (!phoneNumberUtil.isValidNumber(phone)) {
                rowErrorMap.put(row.getRowNum(), "Invalid Kenyan phone number provided !!");
                return phoneNumber;
            }
            // Verify it's a Kenyan number (country code 254)
            if (phone.getCountryCode() != 254) {
                rowErrorMap.put(row.getRowNum(), "Phone number must be a valid Kenyan number (country code +254) !!");
                return phoneNumber;
            }
            return phoneNumber;
        } catch (Exception e) {
            log.error("Invalid value for phone number column", e);
            rowErrorMap.put(row.getRowNum(), e.getMessage());
        }
        return null;
    }

    public static Validator getValidator() {
        Validator validator;
        try (ValidatorFactory factory = Validation.buildDefaultValidatorFactory()) {
            validator = factory.getValidator();
        }
        return validator;
    }
}
