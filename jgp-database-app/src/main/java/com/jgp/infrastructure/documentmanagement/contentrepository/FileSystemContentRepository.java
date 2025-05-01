
package com.jgp.infrastructure.documentmanagement.contentrepository;

import com.google.common.io.Files;
import com.jgp.infrastructure.core.domain.Base64EncodedFile;
import com.jgp.infrastructure.core.domain.Base64EncodedImage;
import com.jgp.infrastructure.documentmanagement.command.DocumentCommand;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import com.jgp.infrastructure.documentmanagement.data.FileData;
import com.jgp.infrastructure.documentmanagement.data.ImageData;
import com.jgp.infrastructure.documentmanagement.exception.ContentManagementException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Service;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.Base64;

@Service
@Slf4j
public class FileSystemContentRepository implements ContentRepository {

    public static final String JGP_BASE_DIR = System.getProperty("user.home") + File.separator + ".jgp_documents";

    private final FileSystemContentPathSanitizer pathSanitizer;

    public FileSystemContentRepository(final FileSystemContentPathSanitizer pathSanitizer) {
        this.pathSanitizer = pathSanitizer;
    }

    @Override
    public String saveFile(final InputStream uploadedInputStream, final DocumentCommand documentCommand) {
        final String fileName = documentCommand.getFileName();
        ContentRepositoryUtils.validateFileSizeWithinPermissibleRange(documentCommand.getSize(), fileName);

        final String fileLocation = generateFileParentDirectory(documentCommand.getParentEntityType(), documentCommand.getParentEntityId())
                + File.separator + fileName;

        writeFileToFileSystem(fileName, uploadedInputStream, fileLocation);
        return fileLocation;
    }

    @Override
    public String saveImage(final InputStream uploadedInputStream, final Long resourceId, final String imageName, final Long fileSize) {
        ContentRepositoryUtils.validateFileSizeWithinPermissibleRange(fileSize, imageName);
        final String fileLocation = generateClientImageParentDirectory(resourceId) + File.separator + imageName;
        writeFileToFileSystem(imageName, uploadedInputStream, fileLocation);
        return fileLocation;
    }

    @Override
    public String saveImage(final Base64EncodedImage base64EncodedImage, final Long resourceId, final String imageName) {
        final String fileLocation = generateClientImageParentDirectory(resourceId) + File.separator + imageName
                + base64EncodedImage.fileExtension();
        String base64EncodedImageString = base64EncodedImage.base64EncodedString();
        try {
            final InputStream toUploadInputStream = new ByteArrayInputStream(Base64.getMimeDecoder().decode(base64EncodedImageString));
            writeFileToFileSystem(imageName, toUploadInputStream, fileLocation);
            return fileLocation;
        } catch (IllegalArgumentException iae) {
            log.error("IllegalArgumentException due to invalid Base64 encoding: {}", base64EncodedImageString, iae);
            throw iae;
        }
    }

    @Override
    public String saveFile(final Base64EncodedFile base64EncodedFile, final Long resourceId, final String fileName,
                           final String parentEntityType) {
        final String fileLocation = generateFileParentDirectory(parentEntityType, resourceId) + File.separator + fileName
                + base64EncodedFile.fileExtension();

        String base64EncodedFileString = base64EncodedFile.base64EncodedString();
        try {
            final InputStream toUploadInputStream = new ByteArrayInputStream(Base64.getMimeDecoder().decode(base64EncodedFileString));
            writeFileToFileSystem(fileName, toUploadInputStream, fileLocation);
            return fileLocation;
        } catch (IllegalArgumentException iae) {
            log.error("IllegalArgumentException due to invalid Base64 encoding: {}", base64EncodedFileString, iae);
            throw iae;
        }
    }

    @Override
    public void deleteImage(final String location) {
        deleteFileInternal(location);
    }

    @Override
    public void deleteFile(final String documentPath) {
        deleteFileInternal(documentPath);
    }

    private void deleteFileInternal(final String documentPath) {
        String path = pathSanitizer.sanitize(documentPath);
        try {
            java.nio.file.Files.delete(Paths.get(path));
        } catch (IOException e) {
            log.warn("Unable to delete file {}", e.getMessage());
        }

    }

    @Override
    public FileData fetchFile(final DocumentData documentData) {
        String path = pathSanitizer.sanitize(documentData.fileLocation());

        final File file = new File(path);
        return new FileData(Files.asByteSource(file), file.getName(), documentData.contentType());
    }

    @Override
    public FileData fetchImage(final ImageData imageData) {
        String path = pathSanitizer.sanitize(imageData.location());

        final File file = new File(path);
        return new FileData(Files.asByteSource(file), imageData.getEntityDisplayName(), imageData.contentType().getValue());
    }

    /**
     * Generate the directory path for storing the new document
     */
    private String generateFileParentDirectory(final String entityType, final Long entityId) {
        return FileSystemContentRepository.JGP_BASE_DIR + File.separator  + File.separator + "documents" + File.separator
                + entityType + File.separator + entityId + File.separator + ContentRepositoryUtils.generateRandomString();
    }

    /**
     * Generate directory path for storing new Image
     */
    private String generateClientImageParentDirectory(final Long resourceId) {
        return FileSystemContentRepository.JGP_BASE_DIR + File.separator + File.separator + "images" + File.separator
                + "clients" + File.separator + resourceId;
    }

    /**
     * Recursively create the directory if it does not exist.
     */
    private void makeDirectories(final String uploadDocumentLocation) throws IOException {
        Files.createParentDirs(new File(uploadDocumentLocation));
    }

    private void writeFileToFileSystem(final String fileName, final InputStream uploadedInputStream, final String fileLocation) {
        try (BufferedInputStream bis = new BufferedInputStream(uploadedInputStream)) {
            String sanitizedPath = pathSanitizer.sanitize(fileLocation, bis);
            makeDirectories(sanitizedPath);
            FileUtils.copyInputStreamToFile(bis, new File(sanitizedPath)); // NOSONAR
        } catch (final IOException ioException) {
            log.warn("writeFileToFileSystem() IOException (logged because cause is not propagated in ContentManagementException)",
                    ioException);
            throw new ContentManagementException(fileName, ioException.getMessage(), ioException);
        }
    }
}
