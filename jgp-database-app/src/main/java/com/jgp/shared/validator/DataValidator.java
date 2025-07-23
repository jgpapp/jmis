package com.jgp.shared.validator;

import com.google.i18n.phonenumbers.PhoneNumberUtil;
import com.google.i18n.phonenumbers.Phonenumber;
import com.jgp.infrastructure.bulkimport.importhandler.ImportHandlerUtils;
import com.jgp.monitoring.dto.OutComeMonitoringRequestDto;
import com.jgp.monitoring.dto.OutComeMonitoringResponseDto;
import com.jgp.shared.exception.DataRulesViolationException;
import com.jgp.shared.exception.ResourceNotFound;
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
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
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

    public static CommonUtil.KenyanCounty validateCountyName(int column, Row row, Map<Row, String> rowErrorMap) {
        try {
            final var countyName = ImportHandlerUtils.readAsString(column, row);
            final var locationCounty = CommonUtil.KenyanCounty.getKenyanCountyFromName(countyName).orElse(CommonUtil.KenyanCounty.UNKNOWN);
            if (CommonUtil.KenyanCounty.UNKNOWN.equals(locationCounty)) {
                throw new DataRulesViolationException("Invalid county name !!");
            }
            return locationCounty;
        } catch (Exception e) {
            log.error("Invalid value for county colum", e);
            rowErrorMap.put(row, e.getMessage());
        }
        return CommonUtil.KenyanCounty.UNKNOWN;
    }

    public static LocalDate validateLocalDate(int column, Row row, Map<Row, String> rowErrorMap, String dateFieldName, boolean isRequired) {
        try {
            final var formattedDate = ImportHandlerUtils.readAsISOFormattedDate(column, row);
            if (null == formattedDate && isRequired) {
                rowErrorMap.put(row, String.format("%s is required field !!", WordUtils.capitalizeFully(dateFieldName)));
                return  LocalDate.now(ZoneId.systemDefault());
            }
            return formattedDate;
        } catch (DateTimeParseException e) {
            log.error("Invalid value for {} colum", dateFieldName, e);
            rowErrorMap.put(row, String.format("%s must be formatted as 'yyyy-MM-dd' !!", WordUtils.capitalizeFully(dateFieldName)));
            return  LocalDate.now(ZoneId.systemDefault());
        }
    }

    public static String validatePhoneNumber(int column, Row row, Map<Row, String> rowErrorMap) {
        try {
            var phoneNumber = ImportHandlerUtils.readAsString(column, row);
            if (null == phoneNumber || phoneNumber.isBlank()) {
                rowErrorMap.put(row, "Phone number is required !!");
                return null;
            }
            if (!phoneNumber.startsWith("+254") && !phoneNumber.startsWith("254")) {
                rowErrorMap.put(row, "Phone number must start with +254 or 254 !!");
                return phoneNumber;
            }
            if (!phoneNumber.startsWith("+")) {
                phoneNumber = String.format("+%s", phoneNumber);
            }
            PhoneNumberUtil phoneNumberUtil = PhoneNumberUtil.getInstance();
            Phonenumber.PhoneNumber phone = phoneNumberUtil.parse(phoneNumber, Phonenumber.PhoneNumber.CountryCodeSource.UNSPECIFIED.name());
            if (!phoneNumberUtil.isValidNumber(phone)) {
                rowErrorMap.put(row, "Invalid Phone number provided !!");
            }
            return phoneNumber;
        } catch (Exception e) {
            log.error("Invalid value for phone number column", e);
            rowErrorMap.put(row, e.getMessage());
        }
        return null;
    }
}
