package com.jgp.authentication.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;

import java.io.Serializable;

public record UserDto(Long id, @NotBlank String firstName, @NotBlank String lastName, @Email String username,
                      @NotBlank String designation, @NotBlank String town, @NotBlank String cellPhone, boolean isActive, Long partnerId, String partnerName) implements Serializable {
}
