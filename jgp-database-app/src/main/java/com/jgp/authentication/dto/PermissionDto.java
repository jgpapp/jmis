package com.jgp.authentication.dto;

public record PermissionDto(
        Long id,
        String code,
        String entityName,
        String actionName,
        boolean selected
) {
}
