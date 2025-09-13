package com.jgp.infrastructure.bulkimport.event;

import com.jgp.infrastructure.documentmanagement.domain.Document;
import jakarta.validation.constraints.NotNull;

public record DataImportDocumentDeletedEvent(@NotNull Document document, boolean deleteAssociatedData) {
}
