package com.jgp.shared.validator;

import com.jgp.finance.domain.Loan;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Row;

import java.math.BigDecimal;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class LoanValidator {

    private LoanValidator() {
    }

    public static void validateLoan(Loan loan, Row row, Map<Row, String> rowErrorMap) {
        // Create a Validator instance
        Validator validator;
        try (ValidatorFactory factory = Validation.buildDefaultValidatorFactory()) {
            validator = factory.getValidator();
        }

        // Validate the object
        Set<ConstraintViolation<Loan>> violations = validator.validate(loan);

        // Get the first error, if any
        if (null == rowErrorMap.get(row) && !violations.isEmpty()) {
            ConstraintViolation<Loan> firstViolation = violations.iterator().next();
            rowErrorMap.put(row, firstViolation.getMessage());
        }
    }

    public static String validateTranchAllocated(String allocatedTranch, BigDecimal tranchAmount, Row row, Map<Row, String> rowErrorMap) {
        final var deliveryModes = Set.of("TRANCH 1", "TRANCH 2", "TRANCH 3", "TRANCH 4", "TRANCH 5", "NOT APPLICABLE");
        var modifiedValue = null == allocatedTranch ? null : allocatedTranch.replaceAll("[^a-zA-Z1-9 ]+", "").replaceAll("\\s+", " ").toUpperCase().trim();
        if (null == rowErrorMap.get(row) && ((null == modifiedValue && Objects.nonNull(tranchAmount) && tranchAmount.compareTo(BigDecimal.ZERO) > 0) || (null != modifiedValue && !deliveryModes.contains(modifiedValue)))){
            rowErrorMap.put(row, "Invalid Value for Allocated Tranch (Must be Tranch 1/Tranch 2/Tranch 3/Tranch 4/Tranch 5/Not Applicable) !!");
        }else {
            return null != modifiedValue ? StringUtils.capitalize(modifiedValue.toLowerCase(Locale.ROOT)) : null;
        }
        return null;
    }

    public static String validateLoanQuality(String value, Row row, Map<Row, String> rowErrorMap) {
        final var loanQualities = Set.of("NORMAL", "WATCH", "SUBSTANDARD", "DOUBTFUL", "LOSS");
        var modifiedValue = null == value ? null : value.replaceAll("[^a-zA-Z]+", "").toUpperCase().trim();
        if (null == rowErrorMap.get(row) && null != modifiedValue && !loanQualities.contains(modifiedValue)){
            rowErrorMap.put(row, "Invalid Value for Loan quality (Must be Normal/Watch/Substandard/Doubtful/Loss) !!");
        }else {
            return null != modifiedValue ? StringUtils.capitalize(modifiedValue.toLowerCase(Locale.ROOT)) : null;
        }
        return null;
    }

    public static String validateLoanProduct(String value, Row row, Map<Row, String> rowErrorMap) {
        final var loanProducts = Set.of("WORKING CAPITAL", "ASSET FINANCE", "STAHIMILI", "PURCHASE ORDER", "CONSIGNMENT FINANCE", "SHARIAH COMPLIANT");
        var modifiedValue = null == value ? null : value.replaceAll("[^a-zA-Z ]+", "").replaceAll("\\s+", " ").toUpperCase().trim();
        if (null == rowErrorMap.get(row) && null != modifiedValue && !loanProducts.contains(modifiedValue)){
            rowErrorMap.put(row, "Invalid Value for Loan quality (Must be Working Capital/Asset Finance/Stahimili/Purchase Order/Consignment Finance/Shariah Compliant) !!");
        }else {
            return null != modifiedValue ? StringUtils.capitalize(modifiedValue.toLowerCase(Locale.ROOT)) : null;
        }
        return null;
    }

    public static String validateLoanerType(String value, Row row, Map<Row, String> rowErrorMap) {
        final var loanProducts = Set.of("NEW", "REPEAT");
        var modifiedValue = null == value ? null : value.replaceAll("[^a-zA-Z]+", "").replaceAll("\\s+", "").toUpperCase().trim();
        if (null == rowErrorMap.get(row) && null == modifiedValue){
            rowErrorMap.put(row, "Invalid Value for Loaner Type (Must be New/Repeat) !!");
        }
        if (null == rowErrorMap.get(row) && null != modifiedValue && !loanProducts.contains(modifiedValue)){
            rowErrorMap.put(row, "Invalid Value for Loaner Type (Must be New/Repeat) !!");
        }
        return null != modifiedValue ? StringUtils.capitalize(modifiedValue.toLowerCase(Locale.ROOT)) : null;
    }
}
