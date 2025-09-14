package com.jgp.authentication.domain;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.text.WordUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class UserAuditOperationConstants {

    public static final String CREATE_ROLE = "CREATE_ROLE";
    public static final String UPDATE_ROLE = "UPDATE_ROLE";
    public static final String DELETE_ROLE = "DELETE_ROLE";
    public static final String CREATE_USER = "CREATE_USER";
    public static final String UPDATE_USER = "UPDATE_USER";
    public static final String UPDATE_ROLE_PERMISSIONS = "UPDATE_ROLE_PERMISSIONS";
    public static final String CREATE_PARTICIPANT = "CREATE_PARTICIPANT";
    public static  final String UPDATE_PARTICIPANT = "UPDATE_PARTICIPANT";
    public static final String CREATE_PARTNER = "CREATE_PARTNER";
    public static final String UPDATE_PARTNER = "UPDATE_PARTNER";
    public static final String DELETE_PARTNER = "DELETE_PARTNER";

    public static final List<String> OPERATIONS_LIST = List.of(
            CREATE_ROLE, UPDATE_ROLE, DELETE_ROLE, CREATE_USER, UPDATE_USER,
            UPDATE_ROLE_PERMISSIONS, DELETE_ROLE, CREATE_PARTICIPANT
    );

    public UserAuditOperationConstants() {
        // Private constructor to prevent instantiation
    }

    public static String getOperationsJsonList() {
        List<Map<String, String>> jsonList = OPERATIONS_LIST.stream()
                .map(op -> {
                    Map<String, String> map = new HashMap<>();
                    map.put("value", op);
                    map.put("display", WordUtils.capitalizeFully(op.replace("_", " ")));
                    return map;
                })
                .toList();
        try {
            return new ObjectMapper().writeValueAsString(jsonList);
        } catch (Exception e) {
            throw new RuntimeException("Failed to serialize operations list", e);
        }
    }

}
