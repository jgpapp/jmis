package com.jgp.infrastructure.bulkimport.data;

import java.io.InputStream;
import java.nio.file.Path;

public record PublishWorkbookImportEventRequestDto(
        Integer primaryColumn,
        Path tempFilePath,
        InputStream clonedInputStreamWorkbook,
        GlobalEntityType entityType,
        String importProgressUUID,
        String updateParticipantInfo,
        String appDomainForNotification
) {
}
