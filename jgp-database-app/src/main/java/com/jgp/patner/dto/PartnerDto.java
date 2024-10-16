package com.jgp.patner.dto;

import jakarta.validation.constraints.NotBlank;

import java.io.Serializable;

public record PartnerDto(Long id, @NotBlank String partnerName, @NotBlank String type, String typeEnum) implements Serializable {
}
