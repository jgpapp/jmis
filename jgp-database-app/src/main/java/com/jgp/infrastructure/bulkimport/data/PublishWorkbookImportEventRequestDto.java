package com.jgp.infrastructure.bulkimport.data;

import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;

public record PublishWorkbookImportEventRequestDto(
        Integer primaryColumn,
        MultipartFile fileDetail,
        InputStream clonedInputStreamWorkbook,
        GlobalEntityType entityType,
        Workbook workbook,
        String importProgressUUID,
        String updateParticipantInfo,
        String appDomainForNotification
) {
}
