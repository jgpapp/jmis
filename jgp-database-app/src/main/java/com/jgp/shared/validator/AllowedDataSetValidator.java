package com.jgp.shared.validator;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

public class AllowedDataSetValidator implements ConstraintValidator<AllowedDataSet, String> {

    private Set<String> allowed;
    private boolean ignoreCase;

    @Override
    public void initialize(AllowedDataSet constraintAnnotation) {
        ignoreCase = constraintAnnotation.ignoreCase();
        allowed = Arrays.stream(constraintAnnotation.allowed())
                .map(val -> ignoreCase ? val.toLowerCase() : val)
                .collect(Collectors.toSet());
    }

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {
        if (value == null || value.isBlank()) return true;
        String[] items = value.split(",");
        for (String item : items) {
            String trimmed = item.trim();
            String check = ignoreCase ? trimmed.toLowerCase() : trimmed;
            if (!allowed.contains(check)) return false;
        }
        return true;
    }
}
