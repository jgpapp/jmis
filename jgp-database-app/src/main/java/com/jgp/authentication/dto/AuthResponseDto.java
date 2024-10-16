package com.jgp.authentication.dto;

public record AuthResponseDto(
        boolean success,
        String message,
        String authToken
) {
}
