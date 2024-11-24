package com.jgp.infrastructure.bulkimport.api;

import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.data.ImportData;
import com.jgp.infrastructure.bulkimport.service.BulkImportWorkbookService;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.Collection;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/imports")
public class BulkImportController {

    private final BulkImportWorkbookService bulkImportWorkbookService;

    @GetMapping
    public String retrieveImportDocuments(@RequestParam("entityType") final String entityType) {
        Collection<ImportData> importData = new ArrayList<>();
        if (entityType.equals(GlobalEntityType.CLIENT.getCode())) {
            final Collection<ImportData> importForClientEntity = this.bulkImportWorkbookService.getImports(GlobalEntityType.CLIENTS_ENTTTY);
            final Collection<ImportData> importForClientPerson = this.bulkImportWorkbookService.getImports(GlobalEntityType.CLIENTS_PERSON);
            if (importForClientEntity != null) {
                importData.addAll(importForClientEntity);
            }
            if (importForClientPerson != null) {
                importData.addAll(importForClientPerson);
            }
        } else {
            final GlobalEntityType type = GlobalEntityType.fromCode(entityType);
            if (type == null) {
                throw new ImportTypeNotFoundException(entityType);
            }
            importData = this.bulkImportWorkbookService.getImports(type);
        }
        final ApiRequestJsonSerializationSettings settings = this.apiRequestParameterHelper.process(uriInfo.getQueryParameters());
        return this.toApiJsonSerializer.serialize(settings, importData);
    }

    @GET
    @Path("getOutputTemplateLocation")
    public String retriveOutputTemplateLocation(@QueryParam("importDocumentId") final String importDocumentId) {
        this.context.authenticatedUser().validateHasReadPermission(this.resourceNameForPermissions);
        final DocumentData documentData = this.bulkImportWorkbookService.getOutputTemplateLocation(importDocumentId);
        return this.toApiJsonSerializer.serialize(documentData.fileLocation());
    }

    @GET
    @Path("downloadOutputTemplate")
    @Produces("application/vnd.ms-excel")
    public Response getOutputTemplate(@QueryParam("importDocumentId") final String importDocumentId) {
        return bulkImportWorkbookService.getOutputTemplate(importDocumentId);
    }
}
