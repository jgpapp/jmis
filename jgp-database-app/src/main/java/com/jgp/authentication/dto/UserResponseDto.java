package com.jgp.authentication.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;

import java.time.LocalDate;

public record UserResponseDto(
        Long id, @NotBlank String displayName, @NotBlank String partnerName,
        @Email String email, String town,
        @NotBlank String designation, @NotBlank String cellPhone, String isActive, LocalDate dateCreated
) {
}
