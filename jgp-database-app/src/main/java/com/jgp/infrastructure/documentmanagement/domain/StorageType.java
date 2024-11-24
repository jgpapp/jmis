
package com.jgp.infrastructure.documentmanagement.domain;

import lombok.Getter;

import java.util.HashMap;
import java.util.Map;

@Getter
public enum StorageType {

    FILE_SYSTEM(1), S3(2);

    private final Integer value;

    StorageType(final Integer value) {
        this.value = value;
    }

    private static final Map<Integer, StorageType> intToEnumMap = new HashMap<>();

    static {
        for (final StorageType type : StorageType.values()) {
            intToEnumMap.put(type.value, type);
        }
    }

    public static StorageType fromInt(final int i) {
        return intToEnumMap.get(i);
    }
}
