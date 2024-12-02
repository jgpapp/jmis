
package com.jgp.infrastructure.documentmanagement.api;

import com.google.common.io.ByteSource;
import com.jgp.infrastructure.documentmanagement.data.FileData;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

@Slf4j
final class ContentResources {

    private ContentResources() {}

    static ResponseEntity<?> fileDataToResponse(FileData fileData, String fileName, String dispositionType) {

        HttpHeaders headers = new HttpHeaders();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            ByteSource byteSource = fileData.getByteSource();
            // TODO Where is this InputStream closed?! It needs to be AFTER it's read by JAX-RS.. how to do that?
            InputStream initialStream = byteSource.openBufferedStream();
            byte[] targetArray = IOUtils.toByteArray(initialStream);
            baos.write(targetArray);
            // Set response headers
            headers.setContentType(MediaType.APPLICATION_OCTET_STREAM);
            headers.add("Content-Disposition", dispositionType + "; filename=\"" + fileName + "\"");
            headers.add("Content-Length", byteSource.sizeIfKnown().or(-1L)+"");
            headers.add("Content-Type", fileData.contentType());

        } catch (IOException e) {
            log.error("Problem occurred in buildResponse function", e);
        }
        return ResponseEntity.ok()
                .headers(headers)
                .body(baos.toByteArray());

    }

    static ResponseEntity<?> fileDataToResponse(FileData fileData, String dispositionType) {
        return fileDataToResponse(fileData, fileData.name(), dispositionType);
    }
}
