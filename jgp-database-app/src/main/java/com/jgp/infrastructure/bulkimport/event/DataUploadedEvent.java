package com.jgp.infrastructure.bulkimport.event;

import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import jakarta.validation.constraints.NotNull;

public record DataUploadedEvent(@NotNull Long partnerId, @NotNull GlobalEntityType entityType) {
}
