package com.jgp.infrastructure.bulkimport.service;

import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.populator.WorkbookPopulator;
import com.jgp.infrastructure.bulkimport.populator.bmo.BMOEntityWorkbookPopulator;
import com.jgp.infrastructure.bulkimport.populator.loan.LoanEntityWorkbookPopulator;
import com.jgp.shared.exception.ResourceNotFound;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ooxml.POIXMLProperties;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.LocalDate;

@Service
@Slf4j
public class BulkImportWorkbookPopulatorServiceImpl implements BulkImportWorkbookPopulatorService {


    @Override
    public ResponseEntity<?> getTemplate(String entityType, HttpServletResponse response) {
        WorkbookPopulator populator = null;
        final XSSFWorkbook workbook = new XSSFWorkbook();
        if (entityType != null) {
            if (entityType.trim().equalsIgnoreCase(GlobalEntityType.LOAN_IMPORT_TEMPLATE.toString())) {
                populator = new LoanEntityWorkbookPopulator();
            } else if (entityType.trim().equalsIgnoreCase(GlobalEntityType.TA_IMPORT_TEMPLATE.toString())) {
                populator = new BMOEntityWorkbookPopulator();
            }  else {
                throw new ResourceNotFound(HttpStatus.NOT_FOUND);
            }
            populator.populate(workbook, "yyyy-MM-dd");
            return buildResponse(workbook, entityType, response);
        } else {
            throw new ResourceNotFound(HttpStatus.NOT_FOUND);
        }
    }

    private ResponseEntity<?> buildResponse(final XSSFWorkbook workbook, final String entity, HttpServletResponse response) {
        String filename = String.format("%s_%s.xlsx", entity, LocalDate.now());
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            workbook.write(baos);
        } catch (IOException e) {
            log.error("Problem occurred in buildResponse function", e);
        }

        // Set response headers
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_OCTET_STREAM);
        headers.add("Content-Disposition", "attachment; filename=\"" + filename + "\"");

        return ResponseEntity.ok()
                .headers(headers)
                .body(baos.toByteArray());
    }


}
