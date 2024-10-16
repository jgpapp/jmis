package com.jgp.infrastructure.bulkimport.data;

import java.util.HashMap;
import java.util.Map;

public enum GlobalEntityType {

    INVALID(0, "invalid"),
    LOAN_IMPORT_TEMPLATE(1, "Loans Template"),
    BMO_IMPORT_TEMPLATE(2, "BMO Template");

    private final Integer value;
    private final String code;

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

    GlobalEntityType(final Integer value, final String code) {
        this.value = value;
        this.code = code;
    }

    public Integer getValue() {
        return this.value;
    }

    public String getCode() {
        return this.code;
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
