
package com.jgp.infrastructure.bulkimport.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.data.ImportData;
import com.jgp.infrastructure.bulkimport.data.ImportProgress;
import com.jgp.infrastructure.bulkimport.domain.ImportDocument;
import com.jgp.infrastructure.bulkimport.domain.ImportDocumentRepository;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.importhandler.ImportHandlerUtils;
import com.jgp.infrastructure.core.domain.JdbcSupport;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.infrastructure.documentmanagement.domain.DocumentRepository;
import com.jgp.infrastructure.documentmanagement.exception.InvalidEntityTypeForDocumentManagementException;
import com.jgp.infrastructure.documentmanagement.mapper.ImportDocumentMapper;
import com.jgp.infrastructure.documentmanagement.service.DocumentWritePlatformService;
import com.jgp.infrastructure.documentmanagement.service.DocumentWritePlatformServiceJpaRepositoryImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.multipart.MultipartFile;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLConnection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;

@Service
@Slf4j
@RequiredArgsConstructor
@Validated
public class BulkImportWorkbookServiceImpl implements BulkImportWorkbookService {

    private final ApplicationContext applicationContext;
    private final PlatformSecurityContext securityContext;
    private final DocumentWritePlatformService documentWritePlatformService;
    private final DocumentRepository documentRepository;
    private final ImportDocumentRepository importDocumentRepository;
    private final JdbcTemplate jdbcTemplate;
    private final ImportDocumentMapper importDocumentMapper;
    private final ImportProgressService importProgressService;
    private static final String SELECT_LITERAL = "select ";



    @Override
    public Long importWorkbook(String entity, MultipartFile fileDetail, String importProgressUUID) {
        try {
            if (entity != null && fileDetail != null) {

                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                IOUtils.copy(fileDetail.getInputStream(), baos);
                final byte[] bytes = baos.toByteArray();
                final BufferedInputStream bis = new BufferedInputStream(new ByteArrayInputStream(bytes));
                Workbook workbook = new XSSFWorkbook(fileDetail.getInputStream());
                GlobalEntityType entityType = null;
                int primaryColumn = 0;
                if (entity.trim().equalsIgnoreCase(GlobalEntityType.TA_IMPORT_TEMPLATE.toString())) {
                    entityType = GlobalEntityType.TA_IMPORT_TEMPLATE;
                } else if (entity.trim().equalsIgnoreCase(GlobalEntityType.LOAN_IMPORT_TEMPLATE.toString())) {
                    entityType = GlobalEntityType.LOAN_IMPORT_TEMPLATE;
                }  else {
                    workbook.close();
                    throw new InvalidEntityTypeForDocumentManagementException("Unable to find requested resource");

                }
                return publishEvent(primaryColumn, fileDetail, bis, entityType, workbook, importProgressUUID);
            }
            throw new InvalidEntityTypeForDocumentManagementException("One or more of the given parameters not found");
        } catch (IOException e) {
            log.error("Problem occurred in importWorkbook function", e);
            throw new InvalidEntityTypeForDocumentManagementException("IO exception occured with " + fileDetail.getOriginalFilename() + " " + e.getMessage());

        }
    }

    @Override
    public ImportProgress getImportProgress(String importDocumentId) {
        ImportProgress progress = null;
        try {
            progress = importProgressService.getImportProgress(importDocumentId);
            log.info("Progress On Query> {}", progress.getProcessed());

            return progress;
        } catch (ExecutionException e) {
            log.error("Error : {}", e.getMessage(), e);
        }finally {
            if (Objects.nonNull(progress) && progress.getTotal() == progress.getProcessed()){
                progress.setProgressAsFinished(2);
            }
        }
        return new ImportProgress();

    }

    private Long publishEvent(final Integer primaryColumn, final MultipartFile fileDetail,
            final InputStream clonedInputStreamWorkbook, final GlobalEntityType entityType, final Workbook workbook, String importProgressUUID) {

        final String fileName = fileDetail.getOriginalFilename();

        final Long documentId = this.documentWritePlatformService.createInternalDocument(
                DocumentWritePlatformServiceJpaRepositoryImpl.DocumentManagementEntity.IMPORT.name(),
                this.securityContext.getAuthenticatedUserIfPresent().getId(), null, clonedInputStreamWorkbook,
                URLConnection.guessContentTypeFromName(fileName), fileName, null, fileName);
        final Document document = this.documentRepository.findById(documentId).orElse(null);

        final ImportDocument importDocument = ImportDocument.instance(document, LocalDateTime.now(ZoneId.systemDefault()), entityType.getValue(),
                ImportHandlerUtils.getNumberOfRows(workbook.getSheetAt(0), primaryColumn), this.securityContext.getAuthenticatedUserIfPresent().getPartner());
        this.importDocumentRepository.saveAndFlush(importDocument);

        BulkImportEvent event = new BulkImportEvent(workbook, entityType.name(), importDocument.getId(), importProgressUUID);
        applicationContext.publishEvent(event);
        log.info("Return import ID := {}", LocalDateTime.now(ZoneId.systemDefault()));
        return importDocument.getId();
    }

    @Override
    public Collection<ImportData> getImports(GlobalEntityType type) {

        final ImportMapper rm = new ImportMapper();
        final String sql = SELECT_LITERAL + rm.schema() + " order by i.id desc";

        return this.jdbcTemplate.query(sql, rm, type.getValue()); // NOSONAR
    }

    @Override
    public Page<ImportData> getImports(GlobalEntityType type, Long partnerId, Pageable pageable) {
        final var imports = this.importDocumentRepository.findByPartnerIdAndEntityType(partnerId, type.getValue(), pageable);
        return new PageImpl<>(this.importDocumentMapper.toDto(imports.getContent()), pageable, imports.getTotalElements());
    }

    @Override
    public Page<ImportData> getImportById(Long importDocumentId) {
        final var importDoc = this.importDocumentRepository.findById(importDocumentId).orElse(null);
        return Objects.nonNull(importDoc) ? new PageImpl<>(List.of(this.importDocumentMapper.toDto(importDoc))) : null;
    }

    private static final class ImportMapper implements RowMapper<ImportData> {

        public String schema() {
            return "i.id as id, i.document_id as documentId, d.doc_name as name, i.import_time as importTime, i.end_time as endTime, " +
                    "i.completed as completed, i.total_records as totalRecords, i.success_count as successCount, " +
                    "i.failure_count as failureCount, i.created_by_id as createdBy " +
                    "from import_document i inner join jgp_document d on i.document_id=d.id " + "where i.entity_type= ? ";
        }

        @Override
        public ImportData mapRow(final ResultSet rs, @SuppressWarnings("unused") final int rowNum) throws SQLException {

            final Long id = rs.getLong("id");
            final Long documentId = rs.getLong("documentId");
            final String name = rs.getString("name");
            final LocalDateTime importTime = JdbcSupport.getLocalDateTime(rs, "importTime");
            final LocalDateTime endTime = JdbcSupport.getLocalDateTime(rs, "endTime");
            final Boolean completed = rs.getBoolean("completed");
            final Integer totalRecords = JdbcSupport.getInteger(rs, "totalRecords");
            final Integer successCount = JdbcSupport.getInteger(rs, "successCount");
            final Integer failureCount = JdbcSupport.getInteger(rs, "failureCount");
            final Long createdBy = rs.getLong("createdBy");

            return new ImportData(id, documentId, name, importTime, endTime, completed, createdBy, totalRecords, successCount,
                    failureCount);
        }
    }

    @Override
    public DocumentData getOutputTemplateLocation(String importDocumentId) {
        final ImportTemplateLocationMapper importTemplateLocationMapper = new ImportTemplateLocationMapper();
        final String sql = SELECT_LITERAL + importTemplateLocationMapper.schema();

        return this.jdbcTemplate.queryForObject(sql, importTemplateLocationMapper, Integer.parseInt(importDocumentId)); // NOSONAR
    }

    @Override
    public ResponseEntity<?> getOutputTemplate(String importDocumentId) {
        final ImportTemplateLocationMapper importTemplateLocationMapper = new ImportTemplateLocationMapper();
        final String sql = SELECT_LITERAL + importTemplateLocationMapper.schema();
        DocumentData documentData = this.jdbcTemplate.queryForObject(sql, importTemplateLocationMapper,
                Integer.parseInt(importDocumentId)); // NOSONAR
        return (documentData != null) ? buildResponse(documentData) : null;
    }

    private ResponseEntity<?> buildResponse(DocumentData documentData) {
        String fileName = "Output" + documentData.fileName();
        String fileLocation = documentData.fileLocation();
        File file = new File(fileLocation);
        HttpHeaders headers = new HttpHeaders();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            InputStream initialStream = new FileInputStream(file);
            byte[] targetArray = IOUtils.toByteArray(initialStream);
            baos.write(targetArray);
            // Set response headers
            headers.setContentType(MediaType.APPLICATION_OCTET_STREAM);
            headers.add("Content-Disposition", "attachment; filename=\"" + fileName + "\"");
            headers.add("Content-Type", "application/vnd.ms-excel");

        } catch (IOException e) {
            log.error("Problem occurred in buildResponse function", e);
        }
        return ResponseEntity.ok()
                .headers(headers)
                .body(baos.toByteArray());
    }

    private static final class ImportTemplateLocationMapper implements RowMapper<DocumentData> {

        public String schema() {
            return "d.location,d.file_name " + "from import_document i inner join jgp_document d on i.document_id=d.id " +
                    "where i.id= ? ";
        }

        @Override
        public DocumentData mapRow(ResultSet rs, @SuppressWarnings("unused") int rowNum) throws SQLException {
            final String location = rs.getString("location");
            final String fileName = rs.getString("file_name");
            return new DocumentData(null, null, null, null, fileName, null, null, null, location);
        }
    }
}
