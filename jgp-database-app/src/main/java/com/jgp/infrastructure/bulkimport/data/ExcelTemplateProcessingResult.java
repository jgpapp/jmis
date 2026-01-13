package com.jgp.infrastructure.bulkimport.data;

import org.apache.poi.ss.usermodel.Row;

/**
 * Represents the result of processing a single row in an Excel template during bulk import.
 *
 * @param row          The Excel row that was processed.
 * @param success      Indicates whether the processing was successful.
 * @param errorMessage An error message if the processing failed; null if successful.
 */
public record ExcelTemplateProcessingResult(
        Row row, boolean success, String errorMessage
) {
}
