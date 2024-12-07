package com.jgp.infrastructure.bulkimport.event;

import com.jgp.infrastructure.bulkimport.data.ImportProgress;
import org.apache.poi.ss.usermodel.Workbook;

public record BulkImportEvent(Workbook workbook, String entityType, Long importId, ImportProgress importProgress) {

}
