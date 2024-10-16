package com.jgp.authentication.dto;

import java.util.List;

public record RoleDto(
        Long id,
        String roleName,
        String description,
        List<String> permissions
) {
}
