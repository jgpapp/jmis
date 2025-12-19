
package com.jgp.infrastructure.bulkimport.service;

import com.github.pjfanning.xlsx.StreamingReader;
import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.infrastructure.bulkimport.data.DocumentDto;
import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.data.ImportData;
import com.jgp.infrastructure.bulkimport.data.ImportRequestDto;
import com.jgp.infrastructure.bulkimport.data.PublishWorkbookImportEventRequestDto;
import com.jgp.infrastructure.bulkimport.data.ResourceType;
import com.jgp.infrastructure.bulkimport.domain.ImportDocument;
import com.jgp.infrastructure.bulkimport.domain.ImportDocumentRepository;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.event.DataImportDocumentDeletedEvent;
import com.jgp.infrastructure.bulkimport.exception.DataImportException;
import com.jgp.infrastructure.bulkimport.importhandler.ImportHandlerUtils;
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
import org.springframework.cache.annotation.CacheEvict;
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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;
import java.util.Objects;

@Service
@Slf4j
@RequiredArgsConstructor
@Validated
public class BulkImportWorkbookServiceImpl implements BulkImportWorkbookService {

    private static final String SELECT_LITERAL = "select ";
    private static final String CONTENT_TYPE_HEADER = "Content-Type";
    private static final String CONTENT_DISPOSITION_HEADER = "Content-Disposition";
    private static final String EXCEL_CONTENT_TYPE = "application/vnd.ms-excel";
    private static final String PDF_CONTENT_TYPE = "application/pdf";
    private static final String UPDATE_PARTICIPANT_FLAG = "YES";
    private static final String DEFAULT_DOMAIN = "localhost";
    private static final String FILE_NOT_FOUND_MESSAGE = "No such file exist";
    private static final String INVALID_ENTITY_TYPE_MESSAGE = "Unable to find requested resource";
    private static final String MISSING_PARAMETERS_MESSAGE = "One or more of the given parameters not found";
    private static final int STREAMING_ROW_CACHE_SIZE = 200;
    private static final int STREAMING_BUFFER_SIZE = 4096;
    private static final int PRIMARY_COLUMN_INDEX = 0;

    private final ApplicationContext applicationContext;
    private final PlatformSecurityContext securityContext;
    private final DocumentWritePlatformService documentWritePlatformService;
    private final DocumentRepository documentRepository;
    private final ImportDocumentRepository importDocumentRepository;
    private final JdbcTemplate jdbcTemplate;
    private final ImportDocumentMapper importDocumentMapper;
    private final ContentRepository contentRepository;



    @Override
    @CacheEvict(cacheNames = {
            "operationalCounties", "highLevelSummary", "loanDisbursedByGenderSummary",
            "countySummary", "loanedBusinessesByGenderSummary", "loanDisbursedByIndustrySectorSummary",
            "loanDisbursedByIndustrySegmentSummary", "loanDisbursedTopFourPartnersSummary",
            "loanDisbursedTopFourCountiesSummary", "businessTrainedTopFourCountiesSummary",
            "businessOwnersTrainedByGenderSummary", "pLWDAndRefugeeBusinessOwnersTrainedByGenderSummary",
            "disabledBusinessOwnersTrainedByGenderSummary", "refugeeBusinessOwnersTrainedByGenderSummary",
            "loanDisbursedByPipelineSourceSummary", "mentorshipByGivenFieldSummary",
            "loansDisbursedByQualitySummary", "systemUserLoginSummary", "taNeedsByGenderSummary",
            "businessCategoryByCountySummary", "taTrainingBySectorSummary",
            "loansDisbursedByLoanProductSummary", "outcomeMonitoringSummary",
            "participantsEmployeesSummary", "taTrainingBySegmentSummary",
            "participantsMentorshipDeliveryModeSummary", "trainingByPartnerByGenderSummary",
            "loanDisbursedByLoanProductByGenderSummary", "lastThreeYearsAccessedLoanPerPartnerSummary",
            "lastThreeYearsAccessedLoanAmountPerPartnerYearly", "lastThreeYearsAccessedLoansCountPerPartnerYearly",
            "lastThreeYearsTrainedBusinessesPerPartnerYearly", "taTypeTrainedBusinesses",
            "loansAccessedVsOutStandingByPartnerSummary", "loansAccessedVsOutStandingByGenderSummary",
            "dataSummary", "dataSummaryMap", "performanceSummary"
    }, allEntries = true)
    public Long importWorkbook(ImportRequestDto request) {
        validateImportRequest(request);

        try {
            final Path tempFilePath = createTempFile(request.fileDetail());
            final byte[] fileBytes = readFileBytes(tempFilePath);
            final GlobalEntityType entityType = resolveEntityType(request.entity());

            final PublishWorkbookImportEventRequestDto eventRequest = createEventRequest(
                    tempFilePath, fileBytes, entityType, request
            );

            return publishEvent(eventRequest);

        } catch (IOException exception) {
            log.error("Failed to import workbook: {}", request.fileDetail().getOriginalFilename(), exception);
            throw new InvalidEntityTypeForDocumentManagementException(
                    "IO exception occurred with " + request.fileDetail().getOriginalFilename()
            );
        }
    }

    @Override
    public void importFileToDirectory(GlobalEntityType entityType, ResourceType resourceType,
                                      MultipartFile fileDetail) {
        Objects.requireNonNull(fileDetail, "File detail cannot be null");
        Objects.requireNonNull(entityType, "Entity type cannot be null");

        final String fileName = fileDetail.getOriginalFilename();

        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
             InputStream inputStream = fileDetail.getInputStream()) {

            IOUtils.copy(inputStream, outputStream);
            final byte[] fileBytes = outputStream.toByteArray();

            final Long documentId = createDocument(entityType, fileName, fileBytes);
            final Document document = fetchDocument(documentId);

            final ImportDocument importDocument = createImportDocument(document, entityType);
            this.importDocumentRepository.saveAndFlush(importDocument);

        } catch (IOException exception) {
            log.error("Failed to import file to directory: {}", fileName, exception);
            throw new DataImportException("File import failed", exception);
        }
    }

    @Override
    public void deleteFileFromDbAndDirectory(Long importDocumentId, boolean deleteAssociatedData) {
        Objects.requireNonNull(importDocumentId, "Import document ID cannot be null");

        final ImportDocument importDocument = fetchImportDocument(importDocumentId);
        if (Objects.isNull(importDocument)) {
            return;
        }

        markImportDocumentAsDeleted(importDocument);

        final Document document = importDocument.getDocument();
        if (Objects.nonNull(document)) {
            deleteDocumentFile(document);
            this.applicationContext.publishEvent(
                    new DataImportDocumentDeletedEvent(document, deleteAssociatedData)
            );
        }
    }

    @Override
    public Page<ImportData> getImports(GlobalEntityType type, Long partnerId, Pageable pageable) {
        Objects.requireNonNull(type, "Entity type cannot be null");
        Objects.requireNonNull(pageable, "Pageable cannot be null");

        final Page<ImportDocument> imports = Objects.nonNull(partnerId)
                ? this.importDocumentRepository.findByPartnerIdAndEntityTypeAndIsDeletedFalse(
                partnerId, type.getValue(), pageable)
                : this.importDocumentRepository.findByEntityTypeAndIsDeletedFalse(
                type.getValue(), pageable);

        final List<ImportData> importDataList = this.importDocumentMapper.toDto(imports.getContent());
        return new PageImpl<>(importDataList, pageable, imports.getTotalElements());
    }

    @Override
    public Page<ImportData> getImportById(Long importDocumentId) {
        Objects.requireNonNull(importDocumentId, "Import document ID cannot be null");

        final ImportDocument importDocument = fetchImportDocument(importDocumentId);
        if (Objects.isNull(importDocument)) {
            return Page.empty();
        }

        final ImportData importData = this.importDocumentMapper.toDto(importDocument);
        return new PageImpl<>(List.of(importData));
    }

    @Override
    public DocumentData getOutputTemplateLocation(String importDocumentId) {
        Objects.requireNonNull(importDocumentId, "Import document ID cannot be null");

        final String sql = SELECT_LITERAL + ImportTemplateLocationMapper.IMPORT_DOCUMENT_SCHEMA;
        return this.jdbcTemplate.queryForObject(sql,
                new ImportTemplateLocationMapper(),
                Integer.parseInt(importDocumentId));
    }

    @Override
    public ResponseEntity<?> getOutputTemplate(String importDocumentId, String fileType) {
        Objects.requireNonNull(importDocumentId, "Import document ID cannot be null");
        Objects.requireNonNull(fileType, "File type cannot be null");

        final DocumentData documentData = getOutputTemplateLocation(importDocumentId);
        if (Objects.isNull(documentData)) {
            return ResponseEntity.notFound().build();
        }

        return buildFileResponse(documentData, "Output" + documentData.fileName(), fileType);
    }

    @Override
    public ResponseEntity<?> downloadFile(String importDocumentId) {
        Objects.requireNonNull(importDocumentId, "Import document ID cannot be null");

        final DocumentData documentData = getOutputTemplateLocation(importDocumentId);
        if (Objects.isNull(documentData)) {
            return ResponseEntity.badRequest()
                    .body(new ApiResponseDto(false, FILE_NOT_FOUND_MESSAGE));
        }

        return buildFileResponse(documentData, documentData.fileName(), documentData.contentType());
    }

    // ==================== Private Helper Methods ====================

    /**
     * Validates the import request to ensure required parameters are present.
     *
     * @param request the import request DTO
     * @throws InvalidEntityTypeForDocumentManagementException if required parameters are missing
     */
    private void validateImportRequest(ImportRequestDto request) {
        if (Objects.isNull(request.entity()) || Objects.isNull(request.fileDetail())) {
            throw new InvalidEntityTypeForDocumentManagementException(MISSING_PARAMETERS_MESSAGE);
        }
    }

    /**
     * Creates a temporary file from the uploaded multipart file.
     *
     * @param file the multipart file to be saved temporarily
     * @return the path to the created temporary file
     * @throws IOException if an I/O error occurs during file creation
     */
    private Path createTempFile(MultipartFile file) throws IOException {
        final Path tempDir = Paths.get(System.getProperty("java.io.tmpdir"));
        final Path tempFilePath = Files.createTempFile(tempDir, "upload_", file.getOriginalFilename());
        file.transferTo(tempFilePath);
        return tempFilePath;
    }

    /**
     * Reads the bytes from a file at the specified path.
     *
     * @param filePath the path to the file
     * @return a byte array containing the file's contents
     * @throws IOException if an I/O error occurs during file reading
     */
    private byte[] readFileBytes(Path filePath) throws IOException {
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
             InputStream inputStream = Files.newInputStream(filePath)) {
            IOUtils.copy(inputStream, outputStream);
            return outputStream.toByteArray();
        }
    }

    /**
     * Resolves the string representation of an entity type to its corresponding enum value.
     *
     * @param entityTypeString the string representation of the entity type
     * @return the corresponding GlobalEntityType enum value
     * @throws InvalidEntityTypeForDocumentManagementException if the entity type is invalid
     */
    private GlobalEntityType resolveEntityType(String entityTypeString) {
        final String normalizedType = entityTypeString.trim().toUpperCase();

        return switch (normalizedType) {
            case "TA_IMPORT_TEMPLATE" -> GlobalEntityType.TA_IMPORT_TEMPLATE;
            case "LOAN_IMPORT_TEMPLATE" -> GlobalEntityType.LOAN_IMPORT_TEMPLATE;
            case "MENTORSHIP_IMPORT_TEMPLATE" -> GlobalEntityType.MENTORSHIP_IMPORT_TEMPLATE;
            case "MONITORING_IMPORT_TEMPLATE" -> GlobalEntityType.MONITORING_IMPORT_TEMPLATE;
            default -> throw new InvalidEntityTypeForDocumentManagementException(
                    INVALID_ENTITY_TYPE_MESSAGE);
        };
    }

    /**
     * Publishes a bulk import event for the given request.
     *
     * @param request the publish workbook import event request DTO
     * @return the ID of the created import document, or -1L if an error occurs
     */
    private Long publishEvent(PublishWorkbookImportEventRequestDto request) {
        final String fileName = request.tempFilePath().getFileName().toString();
        final String contentType = URLConnection.guessContentTypeFromName(fileName);

        final Long documentId = createDocument(request.entityType(), fileName,
                request.clonedInputStreamWorkbook(), contentType);
        final Document document = fetchDocument(documentId);

        try (Workbook workbook = openWorkbook(request.tempFilePath())) {
            final int rowCount = ImportHandlerUtils.getNumberOfRows(
                    workbook.getSheetAt(0), request.primaryColumn());

            final ImportDocument importDocument = createImportDocument(document, request.entityType(), rowCount);
            this.importDocumentRepository.saveAndFlush(importDocument);

            final String appDocumentURL = buildAppDocumentURL(request, importDocument);
            final boolean shouldUpdateParticipant = UPDATE_PARTICIPANT_FLAG.equalsIgnoreCase(
                    request.updateParticipantInfo());

            final BulkImportEvent event = new BulkImportEvent(
                    workbook,
                    document,
                    request.entityType(),
                    importDocument.getId(),
                    request.importProgressUUID(),
                    shouldUpdateParticipant,
                    appDocumentURL
            );

            this.applicationContext.publishEvent(event);
            return importDocument.getId();

        } catch (IOException exception) {
            log.error("Failed to publish import event for file: {}", fileName, exception);
            return -1L;
        }
    }

    /**
     * Creates a publish workbook import event request DTO.
     *
     * @param tempFilePath the path to the temporary file
     * @param fileBytes    the byte array of the file contents
     * @param entityType   the global entity type
     * @param request      the import request DTO
     * @return the created PublishWorkbookImportEventRequestDto
     */
    private PublishWorkbookImportEventRequestDto createEventRequest(
            Path tempFilePath, byte[] fileBytes, GlobalEntityType entityType, ImportRequestDto request) {

        final BufferedInputStream inputStream = new BufferedInputStream(new ByteArrayInputStream(fileBytes));

        return new PublishWorkbookImportEventRequestDto(
                PRIMARY_COLUMN_INDEX,
                tempFilePath,
                inputStream,
                entityType,
                request.importProgressUUID(),
                request.updateParticipantInfo(),
                request.appDomainForNotification()
        );
    }

    /**
     * Opens a workbook from the specified file path using streaming reader.
     *
     * @param filePath the path to the file
     * @return the opened Workbook
     * @throws IOException if an I/O error occurs during workbook opening
     */
    private Workbook openWorkbook(Path filePath) throws IOException {
        return StreamingReader.builder()
                .rowCacheSize(STREAMING_ROW_CACHE_SIZE)
                .bufferSize(STREAMING_BUFFER_SIZE)
                .open(Files.newInputStream(filePath));
    }

    /**
     * Builds the application document URL for notification purposes.
     *
     * @param request        the publish workbook import event request DTO
     * @param importDocument the import document
     * @return the constructed application document URL
     */
    private String buildAppDocumentURL(PublishWorkbookImportEventRequestDto request,
                                       ImportDocument importDocument) {
        final String domain = Objects.isNull(request.appDomainForNotification())
                ? DEFAULT_DOMAIN
                : request.appDomainForNotification();

        return domain + importDocument.getId() + "/" + request.entityType().name();
    }

    /** Creates a document in the document management system.
     *
     * @param entityType   the global entity type
     * @param fileName     the name of the file
     * @param inputStream  the input stream of the file content
     * @param contentType  the content type of the file
     * @return the ID of the created document
     */
    private Long createDocument(GlobalEntityType entityType, String fileName,
                                InputStream inputStream, String contentType) {
        final DocumentDto documentDto = new DocumentDto(
                entityType,
                DocumentWritePlatformServiceJpaRepositoryImpl.DocumentManagementEntity.IMPORT.name(),
                this.securityContext.getAuthenticatedUserIfPresent().getId(),
                null,
                inputStream,
                contentType,
                fileName,
                null,
                fileName
        );

        return this.documentWritePlatformService.createInternalDocument(documentDto);
    }

    /**
     * Creates a document in the document management system from byte array.
     *
     * @param entityType the global entity type
     * @param fileName   the name of the file
     * @param fileBytes  the byte array of the file content
     * @return the ID of the created document
     */
    private Long createDocument(GlobalEntityType entityType, String fileName, byte[] fileBytes) {
        final String contentType = URLConnection.guessContentTypeFromName(fileName);
        try (BufferedInputStream inputStream = new BufferedInputStream(new ByteArrayInputStream(fileBytes))) {
            return createDocument(entityType, fileName, inputStream, contentType);
        } catch (IOException exception) {
            throw new DataImportException("Failed to create document", exception);
        }
    }

    /**
     * Fetches a document by its ID, ensuring it is not marked as deleted.
     *
     * @param documentId the ID of the document to fetch
     * @return the fetched Document
     * @throws IllegalStateException if the document is not found or is deleted
     */
    private Document fetchDocument(Long documentId) {
        return this.documentRepository.findById(documentId)
                .filter(doc -> Boolean.FALSE.equals(doc.getIsDeleted()))
                .orElseThrow(() -> new IllegalStateException("Document not found: " + documentId));
    }

    /**
     * Fetches an import document by its ID, ensuring it is not marked as deleted.
     *
     * @param importDocumentId the ID of the import document to fetch
     * @return the fetched ImportDocument, or null if not found or deleted
     */
    private ImportDocument fetchImportDocument(Long importDocumentId) {
        return this.importDocumentRepository.findById(importDocumentId)
                .filter(doc -> Boolean.FALSE.equals(doc.getIsDeleted()))
                .orElse(null);
    }

    /** Creates an import document entity.
     *
     * @param document   the associated document
     * @param entityType the global entity type
     * @return the created ImportDocument
     */
    private ImportDocument createImportDocument(Document document, GlobalEntityType entityType) {
        return ImportDocument.instance(
                document,
                LocalDateTime.now(ZoneId.systemDefault()),
                entityType.getValue(),
                0,
                this.securityContext.getAuthenticatedUserIfPresent().getPartner()
        );
    }

    /** Creates an import document entity with row count.
     *
     * @param document   the associated document
     * @param entityType the global entity type
     * @param rowCount   the number of rows in the import
     * @return the created ImportDocument
     */
    private ImportDocument createImportDocument(Document document, GlobalEntityType entityType, int rowCount) {
        return ImportDocument.instance(
                document,
                LocalDateTime.now(ZoneId.systemDefault()),
                entityType.getValue(),
                rowCount,
                this.securityContext.getAuthenticatedUserIfPresent().getPartner()
        );
    }

    /** Marks an import document as deleted and saves the change.
     *
     * @param importDocument the import document to mark as deleted
     */
    private void markImportDocumentAsDeleted(ImportDocument importDocument) {
        importDocument.setIsDeleted(true);
        this.importDocumentRepository.save(importDocument);
    }

    /** Deletes the physical file associated with a document and marks the document as deleted.
     *
     * @param document the document whose file is to be deleted
     */
    private void deleteDocumentFile(Document document) {
        if (Objects.nonNull(document.getLocation())) {
            this.contentRepository.deleteFile(document.getLocation());
            document.setIsDeleted(true);
            this.documentRepository.save(document);
        }
    }

    /**
     * Builds a file response entity for downloading a file.
     *
     * @param documentData the document data containing file location and metadata
     * @param fileName     the name of the file to be downloaded
     * @param fileType     the requested file type
     * @return the ResponseEntity containing the file bytes and headers
     */
    private ResponseEntity<?> buildFileResponse(DocumentData documentData, String fileName, String fileType) {
        final File file = new File(documentData.fileLocation());
        if (!file.exists()) {
            return ResponseEntity.notFound().build();
        }

        try (FileInputStream fileInputStream = new FileInputStream(file);
             ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {

            final byte[] fileBytes = IOUtils.toByteArray(fileInputStream);
            outputStream.write(fileBytes);

            final HttpHeaders headers = buildResponseHeaders(fileName, fileType, documentData.contentType());

            return ResponseEntity.ok()
                    .headers(headers)
                    .body(outputStream.toByteArray());

        } catch (IOException exception) {
            log.error("Failed to build file response for: {}", fileName, exception);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Builds HTTP headers for the file response.
     *
     * @param fileName            the name of the file
     * @param fileType            the requested file type
     * @param defaultContentType  the default content type if fileType is not recognized
     * @return the constructed HttpHeaders
     */
    private HttpHeaders buildResponseHeaders(String fileName, String fileType, String defaultContentType) {
        final HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_OCTET_STREAM);
        headers.add(CONTENT_DISPOSITION_HEADER, "attachment; filename=\"" + fileName + "\"");

        final String contentType = resolveContentType(fileType, defaultContentType);
        headers.add(CONTENT_TYPE_HEADER, contentType);

        return headers;
    }

    /**
     * Resolves the content type based on the requested file type.
     *
     * @param fileType            the requested file type
     * @param defaultContentType  the default content type if fileType is not recognized
     * @return the resolved content type
     */
    private String resolveContentType(String fileType, String defaultContentType) {
        if (Objects.isNull(fileType)) {
            return defaultContentType;
        }

        return switch (fileType.toUpperCase()) {
            case "EXCEL" -> EXCEL_CONTENT_TYPE;
            case "PDF" -> PDF_CONTENT_TYPE;
            default -> defaultContentType;
        };
    }

    // ==================== Inner Classes ====================

    /**
     * RowMapper implementation to map SQL result set to DocumentData for import template location.
     */
    private static final class ImportTemplateLocationMapper implements RowMapper<DocumentData> {

        public static final String IMPORT_DOCUMENT_SCHEMA = """
                        d.location, d.file_name, d.type \
                        from import_document i \
                        inner join jgp_document d on i.document_id = d.id \
                        where d.id = ? and i.is_deleted = false""";

        @Override
        public DocumentData mapRow(ResultSet resultSet, int rowNum) throws SQLException {
            final String location = resultSet.getString("location");
            final String fileName = resultSet.getString("file_name");
            final String contentType = resultSet.getString("type");

            return new DocumentData(null, null, null, null, fileName, null,
                    contentType, null, location);
        }
    }
}
