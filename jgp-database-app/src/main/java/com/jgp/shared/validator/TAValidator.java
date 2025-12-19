package com.jgp.shared.validator;

import com.jgp.bmo.domain.TAData;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Row;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class TAValidator {
    private static final String VALUE_REGEX = "[^a-zA-Z ]+";
    private TAValidator() {
    }

    public static void validateTAData(TAData taData, Validator validator) {
        // Validate the object
        Set<ConstraintViolation<TAData>> violations = validator.validate(taData);

        // Get the first error, if any
        if (!violations.isEmpty()) {
            ConstraintViolation<TAData> firstViolation = violations.iterator().next();
            taData.getRowErrorMap().put(taData.getRow(), firstViolation.getMessage());
        }
    }

    public static String validateTADeliveryMode(String value, Row row, Map<Row, String> rowErrorMap){
        final var deliveryModes = Set.of("in person", "virtual", "mixed");
        var modifiedValue = null == value ? null : value.replaceAll(VALUE_REGEX, "").replaceAll("\\s+", " ").toLowerCase().trim();
        if (null == rowErrorMap.get(row) && (null == modifiedValue || !deliveryModes.contains(modifiedValue))){
            rowErrorMap.put(row, "Invalid Delivery Mode (Must be In person/Virtual/Mixed) !!");
        }else {
            return StringUtils.capitalize(modifiedValue);
        }
        return null;
    }

    public static String validateTATypes(String value, Row row, Map<Row, String> rowErrorMap){
        final var deliveryModes = Set.of("post-lending", "pre-lending", "non-lending", "mentorship", "voucher scheme");
        var modifiedValue = null == value ? null : value.replaceAll("[^a-zA-Z\\s-]+", "").replaceAll("\\s+", " ").toLowerCase().trim();
        if (null == rowErrorMap.get(row) && (null == modifiedValue || !deliveryModes.contains(modifiedValue))){
            rowErrorMap.put(row, "Invalid TA Type (Must be Post-lending/Pre-lending/Non-lending/Mentorship/Voucher scheme) !!");
        }else {
            return StringUtils.capitalize(modifiedValue);
        }
        return null;
    }

    public static String validateTANeeds(String value, Row row, Map<Row, String> rowErrorMap){
        var taNeeds = new java.util.HashSet<>(Set.of("FINANCIAL LITERACY", "RECORD KEEPING", "DIGITIZATION", "MARKET ACCESS", "OTHER"));
        var valueArray = Arrays.stream(value.split(",")).map(str -> str.replaceAll(VALUE_REGEX, "").replaceAll("\\s+", " ").trim()).map(String::toUpperCase).collect(Collectors.toSet());
        if (null == rowErrorMap.get(row) && (valueArray.isEmpty() || !taNeeds.containsAll(valueArray))){
            rowErrorMap.put(row, "Invalid Value for TA needs (Must be Financial Literacy/Record Keeping/Digitization/Market Access/Other) !!");
        }else {
            taNeeds.retainAll(valueArray);
            return taNeeds.stream().map(String::toLowerCase).map(StringUtils::capitalize).collect(Collectors.joining(","));
        }
        return null;
    }

    public static String validateSampleRecords(String value, Row row, Map<Row, String> rowErrorMap){
        var sampleRecords = new java.util.HashSet<>(Set.of("PURCHASE RECORD", "RECORD OF SALES", "DELIVERY RECORDS", "RECORD OF EXPENSES", "RECEIPTS", "OTHER"));
        var valueArray = Arrays.stream(value.split(",")).map(str -> str.replaceAll(VALUE_REGEX, "").replaceAll("\\s+", " ").trim()).map(String::toUpperCase).collect(Collectors.toSet());
        if (null == rowErrorMap.get(row) && (valueArray.isEmpty() || !sampleRecords.containsAll(valueArray))){
            rowErrorMap.put(row, "Invalid Value for Sample Records (Must be Purchase record/Record of sales/Delivery records/Record of expenses/Receipts/Other) !!");
        }else {
            sampleRecords.retainAll(valueArray);
            return sampleRecords.stream().map(String::toLowerCase).map(StringUtils::capitalize).collect(Collectors.joining(","));
        }
        return null;
    }

    public static String validateBusinessSegment(String value, Row row, Map<Row, String> rowErrorMap){
        if (null == value){
            rowErrorMap.put(row, "Business segment is required !!");
            return null;
        }else {
            final var businessSegments = Set.of("MICRO", "SME");
            if (null == rowErrorMap.get(row) && !businessSegments.contains(value.toUpperCase())){
                rowErrorMap.put(row, "Invalid Value for Business segment (Must be Micro/SME) !!");
                return  null;
            }
            return value.equalsIgnoreCase("SME") ? "SME" : "Micro";
        }

    }
}
