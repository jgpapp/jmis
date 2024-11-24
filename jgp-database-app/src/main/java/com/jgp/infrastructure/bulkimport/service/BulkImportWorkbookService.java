
package com.jgp.infrastructure.bulkimport.service;


import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.data.ImportData;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;

import java.io.InputStream;
import java.util.Collection;

public interface BulkImportWorkbookService {

    Long importWorkbook(String entityType, InputStream inputStream, FormDataContentDisposition fileDetail, String locale,
            String dateFormat);

    Long importWorkbook(String entityType, InputStream inputStream, FormDataContentDisposition fileDetail, String locale, String dateFormat,
            Long countryId);

    Collection<ImportData> getImports(GlobalEntityType type);

    DocumentData getOutputTemplateLocation(String importDocumentId);

    Response getOutputTemplate(String importDocumentId);

}
