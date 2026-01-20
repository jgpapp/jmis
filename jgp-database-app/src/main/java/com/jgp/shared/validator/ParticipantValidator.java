package com.jgp.shared.validator;

import com.jgp.participant.dto.ParticipantRequestDto;
import com.jgp.util.CommonUtil;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validator;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Row;

import java.util.Locale;
import java.util.Map;
import java.util.Set;

public class ParticipantValidator {

    private ParticipantValidator() {
    }

    public static String validateGender(String value, Row row, Map<Integer, String> rowErrorMap){
        if (null == value){
            rowErrorMap.put(row.getRowNum(), "Gender is required !!");
            return null;
        }else {
            final var genders = Set.of("MALE", "FEMALE", "INTERSEX");
            var modifiedValue = value.replaceAll("[^a-zA-Z]+", "").replaceAll("\\s+", "").toUpperCase(Locale.ROOT).trim();
            if (null == rowErrorMap.get(row.getRowNum()) && !genders.contains(modifiedValue)){
                rowErrorMap.put(row.getRowNum(), "Invalid Value for Gender (Must be Male/Female/Intersex) !!");
                return  null;
            }
            return StringUtils.capitalize(modifiedValue.toLowerCase(Locale.ROOT));
        }

    }

    public static int validateParticipantAge(Integer value, Row row, Map<Integer, String> rowErrorMap){
        if (null == value){
            rowErrorMap.put(row.getRowNum(), "Age is required !!");
            return 0;
        }else {
            if (null == rowErrorMap.get(row.getRowNum()) && (value < 18 || value > 150)){
                rowErrorMap.put(row.getRowNum(), "Entered Age is invalid !!");
                return  0;
            }
            return value;
        }

    }

    public static void validatePersonWithDisability(String value, Row row, Map<Integer, String> rowErrorMap){
        final var deliveryModes = Set.of("YES", "NO");
        if (null == rowErrorMap.get(row.getRowNum()) && (null == value || !deliveryModes.contains(value.toUpperCase()))){
            rowErrorMap.put(row.getRowNum(), "Invalid Value for Person With Disability (Must be Yes/No) !!");
        }
    }

    public static void validateFinanciers(String value, Row row, Map<Integer, String> rowErrorMap){
        final var financiers = Set.of("GBF", "4G", "PBP");
        if (null == rowErrorMap.get(row.getRowNum()) && null != value && !financiers.contains(value.toUpperCase())){
            rowErrorMap.put(row.getRowNum(), "Invalid Value for financier (Must be GBF/4G/PBP) !!");
        }
    }

    public static void validateRefugeeStatus(String value, Row row, Map<Integer, String> rowErrorMap){
        final var deliveryModes = Set.of("YES", "NO");
        if (null == rowErrorMap.get(row.getRowNum()) && (null == value || !deliveryModes.contains(value.toUpperCase()))){
            rowErrorMap.put(row.getRowNum(), "Invalid Value for Refugee Status (Must be Yes/No) !!");
        }
    }

    public static void validateDisabilityType(String value, Row row, Map<Integer, String> rowErrorMap){
        final var disabilityTypes = Set.of("visual impairment", "hearing impairment", "speech impairment", "physical impairment", "intellectual impairment", "psychosocial impairment", "multiple impairments");
        if (null == rowErrorMap.get(row.getRowNum()) && (null == value || !disabilityTypes.contains(value.toLowerCase(Locale.ROOT)))){
            rowErrorMap.put(row.getRowNum(), "Invalid Value for Refugee Status (Must be Visual impairment/Hearing impairment/Speech impairment/Physical impairment/Intellectual impairment/Psychosocial impairment/Multiple impairments) !!");
        }
    }

    public static void validateParticipant(ParticipantRequestDto participantRequestDto, Map<Integer, String> rowErrorMap) {

        final var validator = DataValidator.getValidator();

        // Validate the object
        Set<ConstraintViolation<ParticipantRequestDto>> violations = validator.validate(participantRequestDto);

        // Get the first error, if any
        if (!violations.isEmpty()) {
            ConstraintViolation<ParticipantRequestDto> firstViolation = violations.iterator().next();
            rowErrorMap.put(participantRequestDto.rowIndex(), firstViolation.getMessage());
        }

    }
}
