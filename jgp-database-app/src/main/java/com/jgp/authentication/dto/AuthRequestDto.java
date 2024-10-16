package com.jgp.authentication.dto;


import jakarta.validation.constraints.NotBlank;

public record AuthRequestDto(
        @NotBlank
        String username,
        @NotBlank
        String password
) {
}
