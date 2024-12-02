package com.jgp.infrastructure.documentmanagement.mapper;

import com.jgp.infrastructure.bulkimport.data.ImportData;
import com.jgp.infrastructure.bulkimport.domain.ImportDocument;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.NullValueCheckStrategy;
import org.mapstruct.NullValueMappingStrategy;

import java.util.List;

@Mapper(componentModel = "spring", nullValueMappingStrategy = NullValueMappingStrategy.RETURN_DEFAULT, nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface ImportDocumentMapper {

    @Mapping(target = "importId", expression = "java(importDocument.getId())")
    @Mapping(target = "documentId", expression = "java(null != importDocument.getDocument() ? importDocument.getDocument().getId() : null)")
    @Mapping(target = "name", expression = "java(null != importDocument.getDocument() ? importDocument.getDocument().getName() : null)")
    @Mapping(target = "importTime", expression = "java(importDocument.getImportTime())")
    @Mapping(target = "endTime", expression = "java(importDocument.getEndTime())")
    @Mapping(target = "completed", expression = "java(importDocument.getCompleted())")
    @Mapping(target = "createdBy", expression = "java(null != importDocument.getCreatedBy() ? importDocument.getCreatedBy().getId() : null)")
    @Mapping(target = "totalRecords", expression = "java(importDocument.getTotalRecords())")
    @Mapping(target = "successCount", expression = "java(importDocument.getSuccessCount())")
    @Mapping(target = "failureCount", expression = "java(importDocument.getFailureCount())")
    ImportData toDto(ImportDocument importDocument);

    List<ImportData> toDto(List<ImportDocument> importDocuments);
}
