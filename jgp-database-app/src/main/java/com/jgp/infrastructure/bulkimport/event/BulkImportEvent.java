package com.jgp.infrastructure.bulkimport.event;

import org.apache.poi.ss.usermodel.Workbook;

public record BulkImportEvent(Workbook workbook, String entityType, Long importId, String importProgressUUID, Boolean updateParticipantInfo) {

}
