
package com.jgp.infrastructure.documentmanagement.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.infrastructure.core.domain.JdbcSupport;
import com.jgp.infrastructure.documentmanagement.contentrepository.ContentRepository;
import com.jgp.infrastructure.documentmanagement.contentrepository.ContentRepositoryFactory;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import com.jgp.infrastructure.documentmanagement.data.FileData;
import com.jgp.infrastructure.documentmanagement.exception.DocumentNotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Service;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;

@Service
@RequiredArgsConstructor
public class DocumentReadPlatformServiceImpl implements DocumentReadPlatformService {

    private final JdbcTemplate jdbcTemplate;
    private final PlatformSecurityContext context;
    private final ContentRepositoryFactory contentRepositoryFactory;
    private static final String SELECT_LITERAL = "select ";

    @Override
    public Collection<DocumentData> retrieveAllDocuments(final String entityType, final Long entityId) {
        final DocumentMapper mapper = new DocumentMapper(true, true);
        final String sql = SELECT_LITERAL + mapper.schema() + " order by d.id";
        return this.jdbcTemplate.query(sql, mapper, new Object[] { entityType, entityId }); // NOSONAR
    }

    @Override
    public FileData retrieveFileData(final String entityType, final Long entityId, final Long documentId) {
        try {
            final DocumentMapper mapper = new DocumentMapper(false, false);
            final DocumentData documentData = fetchDocumentDetails(entityType, entityId, documentId, mapper);
            final ContentRepository contentRepository = this.contentRepositoryFactory.getRepository();
            return contentRepository.fetchFile(documentData);
        } catch (final EmptyResultDataAccessException e) {
            throw new DocumentNotFoundException(entityType, entityId, documentId, e);
        }
    }

    @Override
    public DocumentData retrieveDocument(final String entityType, final Long entityId, final Long documentId) {
        try {
            final DocumentMapper mapper = new DocumentMapper(true, true);
            return fetchDocumentDetails(entityType, entityId, documentId, mapper);
        } catch (final EmptyResultDataAccessException e) {
            throw new DocumentNotFoundException(entityType, entityId, documentId, e);
        }
    }

    private DocumentData fetchDocumentDetails(final String entityType, final Long entityId, final Long documentId,
            final DocumentMapper mapper) {
        final String sql = SELECT_LITERAL + mapper.schema() + " and d.id=? ";
        return this.jdbcTemplate.queryForObject(sql, mapper, new Object[] { entityType, entityId, documentId }); // NOSONAR
    }

    private static final class DocumentMapper implements RowMapper<DocumentData> {

        private final boolean hideLocation;
        private final boolean hideStorageType;

        DocumentMapper(final boolean hideLocation, final boolean hideStorageType) {
            this.hideLocation = hideLocation;
            this.hideStorageType = hideStorageType;
        }

        public String schema() {
            return "d.id as id, d.parent_entity_type as parentEntityType, d.parent_entity_id as parentEntityId, d.name as name, "
                    + " d.file_name as fileName, d.size as fileSize, d.type as fileType, "
                    + " d.description as description, d.location as location," + " d.storage_type_enum as storageType"
                    + " from m_document d where d.parent_entity_type=? and d.parent_entity_id=?";
        }

        @Override
        public DocumentData mapRow(final ResultSet rs, @SuppressWarnings("unused") final int rowNum) throws SQLException {
            final Long id = JdbcSupport.getLong(rs, "id");
            final Long parentEntityId = JdbcSupport.getLong(rs, "parentEntityId");
            final Long fileSize = JdbcSupport.getLong(rs, "fileSize");
            final String parentEntityType = rs.getString("parentEntityType");
            final String name = rs.getString("name");
            final String fileName = rs.getString("fileName");
            final String fileType = rs.getString("fileType");
            final String description = rs.getString("description");
            String location = null;
            Integer storageType = null;
            if (!this.hideLocation) {
                location = rs.getString("location");
            }
            if (!this.hideStorageType) {
                storageType = rs.getInt("storageType");
            }
            return new DocumentData(id, parentEntityType, parentEntityId, name, fileName, fileSize, fileType, description, location);
        }
    }

    @Override
    public DocumentData retrieveTopDocument(String entityType, Long entityId) {
        return fetchTopDocumentDetails(entityType, entityId, new DocumentMapper(false, false));
    }

    public DocumentData fetchTopDocumentDetails(final String entityType, final Long entityId, final DocumentMapper mapper) {
        try {
            final String sql = SELECT_LITERAL + mapper.schema() + " order by id desc limit 1";
            return this.jdbcTemplate.queryForObject(sql, mapper, entityType, entityId);
        } catch (final EmptyResultDataAccessException e) {
            return null;
        }
    }
}
