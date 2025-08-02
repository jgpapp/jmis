
package com.jgp.infrastructure.bulkimport.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.data.ImportData;
import com.jgp.infrastructure.bulkimport.data.ImportRequestDto;
import com.jgp.infrastructure.bulkimport.data.PublishWorkbookImportEventRequestDto;
import com.jgp.infrastructure.bulkimport.data.ResourceType;
import com.jgp.infrastructure.bulkimport.domain.ImportDocument;
import com.jgp.infrastructure.bulkimport.domain.ImportDocumentRepository;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.importhandler.ImportHandlerUtils;
import com.jgp.infrastructure.core.domain.JdbcSupport;
import com.jgp.infrastructure.documentmanagement.contentrepository.ContentRepository;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.infrastructure.documentmanagement.domain.DocumentRepository;
import com.jgp.infrastructure.documentmanagement.exception.InvalidEntityTypeForDocumentManagementException;
import com.jgp.infrastructure.documentmanagement.mapper.ImportDocumentMapper;
import com.jgp.infrastructure.documentmanagement.service.DocumentWritePlatformService;
import com.jgp.infrastructure.documentmanagement.service.DocumentWritePlatformServiceJpaRepositoryImpl;
import com.jgp.shared.dto.ApiResponseDto;
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
import org.springframework.http.HttpStatus;
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
    private final ContentRepository contentRepository;
    private static final String SELECT_LITERAL = "select ";
    private static final String CONTENT_TYPE = "Content-Type";



    @Override
    public Long importWorkbook(ImportRequestDto dto) {
        try {
            if (dto.entity() != null && dto.fileDetail() != null) {

                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                IOUtils.copy(dto.fileDetail().getInputStream(), baos);
                final byte[] bytes = baos.toByteArray();
                final BufferedInputStream bis = new BufferedInputStream(new ByteArrayInputStream(bytes));
                Workbook workbook = new XSSFWorkbook(dto.fileDetail().getInputStream());
                GlobalEntityType entityType = null;
                int primaryColumn = 0;
                if (dto.entity().trim().equalsIgnoreCase(GlobalEntityType.TA_IMPORT_TEMPLATE.toString())) {
                    entityType = GlobalEntityType.TA_IMPORT_TEMPLATE;
                } else if (dto.entity().trim().equalsIgnoreCase(GlobalEntityType.LOAN_IMPORT_TEMPLATE.toString())) {
                    entityType = GlobalEntityType.LOAN_IMPORT_TEMPLATE;
                } else if (dto.entity().trim().equalsIgnoreCase(GlobalEntityType.MENTORSHIP_IMPORT_TEMPLATE.toString())) {
                    entityType = GlobalEntityType.MENTORSHIP_IMPORT_TEMPLATE;
                } else if (dto.entity().trim().equalsIgnoreCase(GlobalEntityType.MONITORING_IMPORT_TEMPLATE.toString())) {
                    entityType = GlobalEntityType.MONITORING_IMPORT_TEMPLATE;
                } else {
                    workbook.close();
                    throw new InvalidEntityTypeForDocumentManagementException("Unable to find requested resource");

                }
                return publishEvent(new PublishWorkbookImportEventRequestDto(primaryColumn, dto.fileDetail(), bis, entityType, workbook, dto.importProgressUUID(), dto.updateParticipantInfo(), dto.appDomainForNotification()));
            }
            throw new InvalidEntityTypeForDocumentManagementException("One or more of the given parameters not found");
        } catch (IOException e) {
            log.error("Problem occurred in importWorkbook function", e);
            throw new InvalidEntityTypeForDocumentManagementException("IO exception occurred with " + dto.fileDetail().getOriginalFilename() + " " + e.getMessage());

        }
    }

    @Override
    public void importFileToDirectory(String entityType, ResourceType resourceType, MultipartFile fileDetail) {
        final var fileName = fileDetail.getOriginalFilename();

        final var baos = new ByteArrayOutputStream();
        try {
            IOUtils.copy(fileDetail.getInputStream(), baos);
        } catch (IOException e) {
            throw new IllegalArgumentException(e);
        }
        final byte[] bytes = baos.toByteArray();
        final BufferedInputStream inputStream = new BufferedInputStream(new ByteArrayInputStream(bytes));

        final Long documentId = this.documentWritePlatformService.createInternalDocument(
                DocumentWritePlatformServiceJpaRepositoryImpl.DocumentManagementEntity.IMPORT.name(),
                this.securityContext.getAuthenticatedUserIfPresent().getId(), null, inputStream,
                URLConnection.guessContentTypeFromName(fileName), fileName, null, fileName);
        final Document document = this.documentRepository.findById(documentId).orElse(null);

        final ImportDocument importDocument = ImportDocument.instance(document, LocalDateTime.now(ZoneId.systemDefault()), GlobalEntityType.RESOURCES_IMPORT.getValue(),
                0, this.securityContext.getAuthenticatedUserIfPresent().getPartner());
        this.importDocumentRepository.saveAndFlush(importDocument);
    }

    @Override
    public void deleteFileFromDbAndDirectory(Long importDocumentId) {
        final var importDoc = this.importDocumentRepository.findById(importDocumentId).orElse(null);
        final var doc = Objects.nonNull(importDoc) ? importDoc.getDocument() : null;
        if (Objects.nonNull(importDoc)){
            this.importDocumentRepository.deleteById(importDoc.getId());
        }
        if (Objects.nonNull(doc) && null != doc.getLocation()) {
            this.contentRepository.deleteFile(doc.getLocation());
            this.documentRepository.deleteById(doc.getId());
        }


    }


    private Long publishEvent(final PublishWorkbookImportEventRequestDto dto) {

        final String fileName = dto.fileDetail().getOriginalFilename();

        final Long documentId = this.documentWritePlatformService.createInternalDocument(
                DocumentWritePlatformServiceJpaRepositoryImpl.DocumentManagementEntity.IMPORT.name(),
                this.securityContext.getAuthenticatedUserIfPresent().getId(), null, dto.clonedInputStreamWorkbook(),
                URLConnection.guessContentTypeFromName(fileName), fileName, null, fileName);
        final Document document = this.documentRepository.findById(documentId).orElse(null);

        try(Workbook workbook = dto.workbook()) {
            final ImportDocument importDocument = ImportDocument.instance(document, LocalDateTime.now(ZoneId.systemDefault()), dto.entityType().getValue(),
                    ImportHandlerUtils.getNumberOfRows(workbook.getSheetAt(0), dto.primaryColumn()), this.securityContext.getAuthenticatedUserIfPresent().getPartner());
            this.importDocumentRepository.saveAndFlush(importDocument);

            final var importDocumentId = importDocument.getId();
            final var appDocumentURL = null == dto.appDomainForNotification() ? "localhost" : dto.appDomainForNotification().concat(importDocumentId.toString()).concat("/").concat(dto.entityType().name());
            BulkImportEvent event = new BulkImportEvent(workbook, dto.entityType().name(), importDocument.getId(), dto.importProgressUUID(), "YES".equalsIgnoreCase(dto.updateParticipantInfo()), appDocumentURL);
            applicationContext.publishEvent(event);
            return importDocumentId;
        } catch (IOException e) {
            log.error("Problem occurred in publishEvent function", e);
        }
        return -1L;
    }

    @Override
    public Collection<ImportData> getImports(GlobalEntityType type) {

        final ImportMapper rm = new ImportMapper();
        final String sql = SELECT_LITERAL + rm.schema() + " order by i.id desc";

        return this.jdbcTemplate.query(sql, rm, type.getValue()); // NOSONAR
    }

    @Override
    public Page<ImportData> getImports(GlobalEntityType type, Long partnerId, Pageable pageable) {
        Page<ImportDocument> imports;
        if (Objects.nonNull(partnerId)){
            imports = this.importDocumentRepository.findByPartnerIdAndEntityType(partnerId, type.getValue(), pageable);
        }else {
            imports = this.importDocumentRepository.findByEntityType(type.getValue(), pageable);
        }
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
        final String sql = SELECT_LITERAL + ImportTemplateLocationMapper.IMPORT_DOCUMENT_SCHEMA;

        return this.jdbcTemplate.queryForObject(sql, importTemplateLocationMapper, Integer.parseInt(importDocumentId)); // NOSONAR
    }

    @Override
    public ResponseEntity<?> getOutputTemplate(String importDocumentId, String fileType) {
        final ImportTemplateLocationMapper importTemplateLocationMapper = new ImportTemplateLocationMapper();
        final String sql = SELECT_LITERAL + ImportTemplateLocationMapper.IMPORT_DOCUMENT_SCHEMA;
        DocumentData documentData = this.jdbcTemplate.queryForObject(sql, importTemplateLocationMapper,
                Integer.parseInt(importDocumentId)); // NOSONAR
        return (documentData != null) ? buildResponse(documentData, fileType) : null;
    }

    @Override
    public ResponseEntity<?> downloadFile(String importDocumentId) {
        final ImportTemplateLocationMapper importTemplateLocationMapper = new ImportTemplateLocationMapper();
        final String sql = SELECT_LITERAL + ImportTemplateLocationMapper.IMPORT_DOCUMENT_SCHEMA;
        DocumentData documentData = this.jdbcTemplate.queryForObject(sql, importTemplateLocationMapper,
                Integer.parseInt(importDocumentId)); // NOSONAR

        if (Objects.isNull(documentData)){
            return new ResponseEntity<>(new ApiResponseDto(false, "No such file exist"), HttpStatus.BAD_REQUEST);
        }

        String fileName = documentData.fileName();
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
            headers.add(CONTENT_TYPE, documentData.contentType());

        } catch (IOException e) {
            log.error("Problem occurred in buildResponse function", e);
        }
        return ResponseEntity.ok()
                .headers(headers)
                .body(baos.toByteArray());
    }

    private ResponseEntity<?> buildResponse(DocumentData documentData, String fileType) {
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
            if (fileType.equals("EXCEL")) {
                headers.add(CONTENT_TYPE, "application/vnd.ms-excel");
            } else if (fileType.equals("PDF")) {
                headers.add(CONTENT_TYPE, "application/pdf");
            }

        } catch (IOException e) {
            log.error("Problem occurred in buildResponse function", e);
        }
        return ResponseEntity.ok()
                .headers(headers)
                .body(baos.toByteArray());
    }

    private static final class ImportTemplateLocationMapper implements RowMapper<DocumentData> {

        public static final String IMPORT_DOCUMENT_SCHEMA = "d.location,d.file_name,d.type from import_document i inner join jgp_document d on i.document_id=d.id where d.id= ? ";

        @Override
        public DocumentData mapRow(ResultSet rs, @SuppressWarnings("unused") int rowNum) throws SQLException {
            final String location = rs.getString("location");
            final String fileName = rs.getString("file_name");
            final String fileContentType = rs.getString("type");
            return new DocumentData(null, null, null, null, fileName, null, fileContentType, null, location);
        }
    }
}
