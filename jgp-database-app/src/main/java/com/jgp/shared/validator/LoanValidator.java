package com.jgp.shared.validator;

import com.jgp.finance.dto.LoanRequestDto;
import com.jgp.util.CommonUtil;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validator;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.WordUtils;
import org.apache.poi.ss.usermodel.Row;

import java.math.BigDecimal;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class LoanValidator {

    private LoanValidator() {
    }

    public static void validateLoan(LoanRequestDto loan, Map<Integer, String> rowErrorMap) {
        // Create a Validator instance
        Validator validator = DataValidator.getValidator();

        // Validate the object
        Set<ConstraintViolation<LoanRequestDto>> violations = validator.validate(loan);

        // Get the first error, if any
        if (null == rowErrorMap.get(loan.rowIndex()) && !violations.isEmpty()) {
            ConstraintViolation<LoanRequestDto> firstViolation = violations.iterator().next();
            rowErrorMap.put(loan.rowIndex(), firstViolation.getMessage());
        }
    }

    public static String validateTranchAllocated(String allocatedTranch, BigDecimal tranchAmount, Row row, Map<Integer, String> rowErrorMap) {
        final var deliveryModes = Set.of("TRANCH 1", "TRANCH 2", "TRANCH 3", "TRANCH 4", "TRANCH 5", "NOT APPLICABLE");
        var modifiedValue = null == allocatedTranch ? null : allocatedTranch.replaceAll("[^a-zA-Z1-9 ]+", "").replaceAll("\\s+", " ").toUpperCase().trim();
        if (null == rowErrorMap.get(row.getRowNum()) && ((null == modifiedValue && Objects.nonNull(tranchAmount) && tranchAmount.compareTo(BigDecimal.ZERO) > 0) || (null != modifiedValue && !deliveryModes.contains(modifiedValue)))){
            rowErrorMap.put(row.getRowNum(), "Invalid Value for Allocated Tranch (Must be Tranch 1/Tranch 2/Tranch 3/Tranch 4/Tranch 5/Not Applicable) !!");
        }else {
            return null != modifiedValue ? StringUtils.capitalize(modifiedValue.toLowerCase(Locale.ROOT)) : null;
        }
        return null;
    }

    public static String validateLoanQuality(String value, Row row, Map<Integer, String> rowErrorMap) {
        final var loanQualities = Set.of("normal", "watch", "substandard", "doubtful", "loss");
        var modifiedValue = null == value ? null : CommonUtil.sanitizeString(value).toLowerCase();
        if (null == rowErrorMap.get(row.getRowNum()) && null != modifiedValue && !loanQualities.contains(modifiedValue)){
            rowErrorMap.put(row.getRowNum(), "Invalid Value for Loan quality (Must be Normal/Watch/Substandard/Doubtful/Loss) !!");
        }else {
            return null != modifiedValue ? StringUtils.capitalize(modifiedValue) : null;
        }
        return null;
    }

    public static String validateAndNormalizeLoanProduct(String value, Row row, Map<Integer, String> rowErrorMap) {
        final var loanProducts = Set.of("working capital", "asset finance", "stahimili", "purchase order", "consignment finance", "shariah compliant");
        var modifiedValue = (null == value) ? null : CommonUtil.sanitizeString(value).toLowerCase();
        if (null == rowErrorMap.get(row.getRowNum()) && null != modifiedValue && !loanProducts.contains(modifiedValue)){
            rowErrorMap.put(row.getRowNum(), "Invalid Value for Loan Product (Must be Working Capital/Asset Finance/Stahimili/Purchase Order/Consignment Finance/Shariah Compliant) !!");
        }else {
            return null != modifiedValue ? WordUtils.capitalizeFully(modifiedValue) : null;
        }
        return null;
    }

    public static String validateLoanerType(String value, Row row, Map<Integer, String> rowErrorMap) {
        final var loanProducts = Set.of("new", "repeat");
        var modifiedValue = null == value ? null : CommonUtil.sanitizeString(value).toLowerCase();
        if (null == rowErrorMap.get(row.getRowNum()) && null == modifiedValue){
            rowErrorMap.put(row.getRowNum(), "Invalid Value for Loaner Type (Must be New/Repeat) !!");
        }
        if (null == rowErrorMap.get(row.getRowNum()) && null != modifiedValue && !loanProducts.contains(modifiedValue)){
            rowErrorMap.put(row.getRowNum(), "Invalid Value for Loaner Type (Must be New/Repeat) !!");
        }
        return null != modifiedValue ? StringUtils.capitalize(modifiedValue) : null;
    }
}
