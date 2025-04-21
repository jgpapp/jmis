package com.jgp.infrastructure.bulkimport.data;

import lombok.Getter;

@Getter
public enum ResourceType {
    USER_MANUAL("User Manual");

    private final String name;

    ResourceType(String name) {
        this.name = name;
    }
}
