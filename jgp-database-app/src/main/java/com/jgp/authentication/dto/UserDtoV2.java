package com.jgp.authentication.dto;

import java.util.Set;

public record UserDtoV2(
        Long id,
        String firstName,
        String lastName,
        String gender,
        String image,
        Set<String> userRoles,
        String partnerName,
        Long partnerId,
        String designation,
        String username,
        String cellPhone,
        String town
) {
}
