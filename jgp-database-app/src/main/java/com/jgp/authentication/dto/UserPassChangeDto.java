package com.jgp.authentication.dto;

import jakarta.validation.constraints.NotBlank;

import java.io.Serializable;

public record UserPassChangeDto(
        @NotBlank
        String password,
        @NotBlank
        String newPass,
        @NotBlank
        String passConfirm
) implements Serializable{
}
