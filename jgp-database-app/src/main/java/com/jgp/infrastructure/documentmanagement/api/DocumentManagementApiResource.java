
package com.jgp.infrastructure.documentmanagement.api;


import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.infrastructure.core.domain.Base64EncodedFile;
import com.jgp.infrastructure.documentmanagement.contentrepository.ContentRepositoryUtils;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import com.jgp.infrastructure.documentmanagement.service.DocumentReadPlatformService;
import com.jgp.infrastructure.documentmanagement.service.DocumentWritePlatformService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

@Component
@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/documents/{entityType}/{entityId}")
public class DocumentManagementApiResource {

    private final Set<String> responseDataParameters = new HashSet<>(
            Arrays.asList("id", "parentEntityType", "parentEntityId", "name", "fileName", "size", "type", "description"));

    private final String systemEntityType = "DOCUMENT";

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

    @GET
    @Path("{documentId}/attachment")
    public Response downloadFile(@PathParam("entityType") @Parameter(description = "entityType") final String entityType,
            @PathParam("entityId") @Parameter(description = "entityId") final Long entityId,
            @PathParam("documentId") @Parameter(description = "documentId") final Long documentId) {
        if (entityType != null && !entityType.equalsIgnoreCase("client_identifiers") && !entityType.equalsIgnoreCase("clients"))
            this.context.authenticatedUser().validateHasReadPermission(this.systemEntityType);
        final FileData fileData = this.documentReadPlatformService.retrieveFileData(entityType, entityId, documentId);
        return ContentResources.fileDataToResponse(fileData, "attachment");
    }

    @DELETE
    @Path("{documentId}")
    @Consumes({ MediaType.APPLICATION_JSON })
    @Produces({ MediaType.APPLICATION_JSON })
    @Operation(summary = "Remove a Document", description = "")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(schema = @Schema(implementation = DocumentManagementApiResourceSwagger.DeleteEntityTypeEntityIdDocumentsResponse.class))) })
    public String deleteDocument(@PathParam("entityType") @Parameter(description = "entityType") final String entityType,
            @PathParam("entityId") @Parameter(description = "entityId") final Long entityId,
            @PathParam("documentId") @Parameter(description = "documentId") final Long documentId) {

        final DocumentCommand documentCommand = new DocumentCommand(null, documentId, entityType, entityId, null, null, null, null, null,
                null);

        final CommandProcessingResult documentIdentifier = this.documentWritePlatformService.deleteDocument(documentCommand);

        return this.toApiJsonSerializer.serialize(documentIdentifier);
    }

    /**
     * Upload document as a Data URL (essentially a base64 encoded stream)
     */
    @POST
    @Consumes({ MediaType.TEXT_PLAIN, MediaType.TEXT_HTML, MediaType.APPLICATION_JSON })
    @Produces(MediaType.APPLICATION_JSON)
    public String createDocument(@PathParam("entityType") final String entityType, @PathParam("entityId") final Long entityId,
            @RequestBody DocumentManagementApiResourceSwagger.Base64DocumentUploadRequest documentUploadRequest) {
        Base64EncodedFile base64EncodedFile = ContentRepositoryUtils.extractFileFromDataURL(documentUploadRequest.getDocumentString());
        final Long documentId = this.documentWritePlatformService.createDocument(base64EncodedFile, entityType, entityId,
                documentUploadRequest.getName(), documentUploadRequest.getName(), base64EncodedFile.getFileType());

        return this.toApiJsonSerializer.serialize(CommandProcessingResult.resourceResult(documentId, null));
    }
}
