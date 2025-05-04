package com.jgp.infrastructure.bulkimport.data;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.HashMap;
import java.util.Map;

@Getter
@RequiredArgsConstructor
public enum GlobalEntityType {

    INVALID(0, "invalid", "invalid"),
    LOAN_IMPORT_TEMPLATE(1, "Loans Template", "Lending Data"),
    TA_IMPORT_TEMPLATE(2, "TA Template", "Technical Assistance Data"),
    RESOURCES_IMPORT(3, "Resources Import", "Resources Import");

    private final Integer value;
    private final String code;
    private final String dataType;

    private static final Map<Integer, GlobalEntityType> intToEnumMap = new HashMap<>();
    private static final Map<String, GlobalEntityType> stringToEnumMap = new HashMap<>();
    private static int minValue;
    private static int maxValue;

    static {
        int i = 0;
        for (final GlobalEntityType entityType : GlobalEntityType.values()) {
            if (i == 0) {
                minValue = entityType.value;
            }
            intToEnumMap.put(entityType.value, entityType);
            stringToEnumMap.put(entityType.code, entityType);
            if (minValue >= entityType.value) {
                minValue = entityType.value;
            }
            if (maxValue < entityType.value) {
                maxValue = entityType.value;
            }
            i = i + 1;
        }
    }


    public static GlobalEntityType fromInt(final int i) {
        return intToEnumMap.get(i);
    }

    public static GlobalEntityType fromCode(final String key) {
        return stringToEnumMap.get(key);
    }

    @Override
    public String toString() {
        return name();
    }
}
