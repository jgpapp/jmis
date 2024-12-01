
package com.jgp.infrastructure.documentmanagement.contentrepository;

import com.jgp.infrastructure.core.domain.Base64EncodedFile;
import com.jgp.infrastructure.core.domain.Base64EncodedImage;
import com.jgp.infrastructure.documentmanagement.command.DocumentCommand;
import com.jgp.infrastructure.documentmanagement.data.DocumentData;
import com.jgp.infrastructure.documentmanagement.data.FileData;
import com.jgp.infrastructure.documentmanagement.data.ImageData;

import java.io.InputStream;

/**
 * Repository which stores Files (AKA Documents) and Images.
 */
public interface ContentRepository {

    Integer MAX_FILE_UPLOAD_SIZE_IN_MB = 5;

    Integer MAX_IMAGE_UPLOAD_SIZE_IN_MB = 1;

    String saveFile(InputStream uploadedInputStream, DocumentCommand documentCommand);

    void deleteFile(String documentPath);

    FileData fetchFile(DocumentData documentData);

    String saveImage(InputStream uploadedInputStream, Long resourceId, String imageName, Long fileSize);

    String saveImage(Base64EncodedImage base64EncodedImage, Long resourceId, String imageName);

    void deleteImage(String location);

    FileData fetchImage(ImageData imageData);


    /**
     * Method to save base64-encoded files
     *
     * @param base64EncodedFile
     *            The file to be saved
     * @param parentEntityId
     *            ID of the entity that the file is associated with
     * @param fileName
     *            Name of the file
     * @param parentEntityType
     *            Type of entity the file belongs to
     */
    String saveFile(Base64EncodedFile base64EncodedFile, Long parentEntityId, String fileName, String parentEntityType);

}
