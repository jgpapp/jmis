package com.jgp.infrastructure.bulkimport.event;

import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import org.apache.poi.ss.usermodel.Workbook;

public record BulkImportEvent(
        Workbook workbook,
        Document document,
        GlobalEntityType entityType,
        Long importId,
        String importProgressUUID,
        Boolean updateParticipantInfo,
        String appDomainForNotification){

}
