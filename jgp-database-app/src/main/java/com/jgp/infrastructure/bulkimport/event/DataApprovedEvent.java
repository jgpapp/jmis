package com.jgp.infrastructure.bulkimport.event;

import jakarta.validation.constraints.NotNull;

import java.time.LocalDate;
import java.util.Set;

public record DataApprovedEvent(@NotNull Long partnerId, Set<LocalDate> dates) {
}
