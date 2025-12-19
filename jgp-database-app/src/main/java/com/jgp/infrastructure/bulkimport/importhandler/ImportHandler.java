package com.jgp.infrastructure.bulkimport.importhandler;

import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.concurrent.CompletableFuture;

public interface ImportHandler {

    int CHUNK_SIZE = 1_000;

    CompletableFuture<Count> process(BulkImportEvent bulkImportEvent);

    default void writeGroupErrorMessage(String errorMessage, Workbook workbook, Cell statusCell, Cell errorReportCell) {
        String status = TemplatePopulateImportConstants.STATUS_CREATION_FAILED;
        statusCell.setCellValue(status);
        statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.RED));
        errorReportCell.setCellValue(errorMessage);
    }

    default void setReportHeaders(Sheet currentSheet, int statusColIndex, int failureColIndex) {
        ImportHandlerUtils.writeString(statusColIndex, currentSheet.getRow(TemplatePopulateImportConstants.ROWHEADER_INDEX),
                TemplatePopulateImportConstants.STATUS_COL_REPORT_HEADER);
        ImportHandlerUtils.writeString(failureColIndex, currentSheet.getRow(TemplatePopulateImportConstants.ROWHEADER_INDEX),
                TemplatePopulateImportConstants.FAILURE_COL_REPORT_HEADER);
    }
}
