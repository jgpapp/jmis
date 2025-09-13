package com.jgp.infrastructure.bulkimport.data;

import java.io.InputStream;

public record DocumentDto(
        GlobalEntityType importEntityType,
        String entityType,
        Long entityId,
        Long fileSize,
        InputStream inputStream,
        String mimeType,
        String name,
        String description,
        String fileName
) {
}
