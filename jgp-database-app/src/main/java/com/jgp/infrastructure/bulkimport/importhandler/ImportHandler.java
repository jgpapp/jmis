package com.jgp.infrastructure.bulkimport.importhandler;

import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.data.ExcelTemplateProcessingResult;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.concurrent.CompletableFuture;

public interface ImportHandler {

    String PARTICIPANT_ASSOCIATION_ERROR = "Cannot associate data to a participant!";
    int CHUNK_SIZE = 100; // Default chunk size for parallel processing

    /**
     * Process the bulk import event asynchronously
     */
    CompletableFuture<Count> process(BulkImportEvent bulkImportEvent);

    /**
     * Writes error message to the specified cells in the workbook
     */
    default void writeGroupErrorMessage(String errorMessage, Workbook workbook, Cell statusCell, Cell errorReportCell) {
        String status = TemplatePopulateImportConstants.STATUS_CREATION_FAILED;
        statusCell.setCellValue(status);
        statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.RED));
        errorReportCell.setCellValue(errorMessage);
    }

    /**
     * Sets the report headers in the specified sheet
     */
    default void setReportHeaders(Sheet currentSheet, int statusColIndex, int failureColIndex) {
        Row headerRow = currentSheet.getRow(0);
        if (headerRow == null) {
            headerRow = currentSheet.createRow(0);
        }

        Cell statusHeaderCell = headerRow.createCell(statusColIndex);
        statusHeaderCell.setCellValue("Status");
        statusHeaderCell.setCellStyle(ImportHandlerUtils.getCellStyle(currentSheet.getWorkbook(), IndexedColors.LIGHT_BLUE));

        Cell failureHeaderCell = headerRow.createCell(failureColIndex);
        failureHeaderCell.setCellValue("Failure Report");
        failureHeaderCell.setCellStyle(ImportHandlerUtils.getCellStyle(currentSheet.getWorkbook(), IndexedColors.LIGHT_BLUE));
    }

    /**
     * Writes processing result to workbook (must be called sequentially, not thread-safe)
     */
    default void writeResultToWorkbook(ExcelTemplateProcessingResult result, int statusColIndex, int failureColIndex) {
        Row row = result.row();
        Cell errorReportCell = row.createCell(failureColIndex);
        Cell statusCell = row.createCell(statusColIndex);

        if (result.success()) {
            statusCell.setCellValue(TemplatePopulateImportConstants.STATUS_CELL_IMPORTED);
            statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(row.getSheet().getWorkbook(), IndexedColors.LIGHT_GREEN));
        } else {
            writeGroupErrorMessage(result.errorMessage(), row.getSheet().getWorkbook(), statusCell, errorReportCell);
        }
    }
}
