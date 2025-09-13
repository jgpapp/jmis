
package com.jgp.infrastructure.documentmanagement.api;


import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.infrastructure.documentmanagement.command.DocumentCommand;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import com.jgp.infrastructure.documentmanagement.data.FileData;
import com.jgp.infrastructure.documentmanagement.service.DocumentReadPlatformService;
import com.jgp.infrastructure.documentmanagement.service.DocumentWritePlatformService;
import com.jgp.shared.dto.ApiResponseDto;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collection;

@Component
@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/documents/{entityType}/{entityId}")
public class DocumentManagementApiResource {

    private final PlatformSecurityContext context;
    private final DocumentReadPlatformService documentReadPlatformService;
    private final DocumentWritePlatformService documentWritePlatformService;


    @GetMapping
    public ResponseEntity<Collection<DocumentData>> retrieveAllDocuments(@PathVariable("entityType") final String entityType,
                                                              @PathVariable("entityId") final Long entityId) {
        return new ResponseEntity<>(this.documentReadPlatformService.retrieveAllDocuments(entityType, entityId), HttpStatus.OK);
    }


    @GetMapping("/{documentId}")
    public ResponseEntity<DocumentData> getDocument(@PathVariable("entityType")  final String entityType,
            @PathVariable("entityId") final Long entityId,
            @PathVariable("documentId") final Long documentId) {
        return new ResponseEntity<>(this.documentReadPlatformService.retrieveDocument(entityType, entityId, documentId), HttpStatus.OK);
    }


    @GetMapping("/{documentId}/attachment")
    public ResponseEntity<?> downloadFile(@PathVariable("entityType") final String entityType,
            @PathVariable("entityId") final Long entityId,
            @PathVariable("documentId") final Long documentId) {
        final FileData fileData = this.documentReadPlatformService.retrieveFileData(entityType, entityId, documentId);
        return ContentResources.fileDataToResponse(fileData, "attachment");
    }

    @DeleteMapping("{documentId}")
    public ResponseEntity<ApiResponseDto> deleteDocument(@PathVariable("entityType")  final String entityType,
            @PathVariable("entityId")  final Long entityId,
            @PathVariable("documentId") final Long documentId) {

        final DocumentCommand documentCommand = new DocumentCommand(null, documentId, entityType, entityId, null, null, null, null, null,
                null, null);

        this.documentWritePlatformService.deleteDocument(documentCommand);

        return new ResponseEntity<>(new ApiResponseDto(true, "Document deleted !!"), HttpStatus.NO_CONTENT);
    }

}
