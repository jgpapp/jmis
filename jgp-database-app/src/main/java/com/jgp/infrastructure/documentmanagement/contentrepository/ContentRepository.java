/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package com.jgp.infrastructure.documentmanagement.contentrepository;

import org.apache.fineract.infrastructure.core.domain.Base64EncodedFile;
import org.apache.fineract.infrastructure.core.domain.Base64EncodedImage;
import org.apache.fineract.infrastructure.documentmanagement.command.DocumentCommand;
import org.apache.fineract.infrastructure.documentmanagement.data.DocumentData;
import org.apache.fineract.infrastructure.documentmanagement.data.FileData;
import org.apache.fineract.infrastructure.documentmanagement.data.ImageData;
import org.apache.fineract.infrastructure.documentmanagement.domain.StorageType;

import java.io.InputStream;

/**
 * Repository which stores Files (AKA Documents) and Images.
 */
public interface ContentRepository {

    // TODO:Vishwas Need to move these settings to the Database
    Integer MAX_FILE_UPLOAD_SIZE_IN_MB = 5;

    // TODO:Vishwas Need to move these settings to the Database
    Integer MAX_IMAGE_UPLOAD_SIZE_IN_MB = 1;

    String saveFile(InputStream uploadedInputStream, DocumentCommand documentCommand);

    void deleteFile(String documentPath);

    FileData fetchFile(DocumentData documentData);

    String saveImage(InputStream uploadedInputStream, Long resourceId, String imageName, Long fileSize);

    String saveImage(Base64EncodedImage base64EncodedImage, Long resourceId, String imageName);

    void deleteImage(String location);

    FileData fetchImage(ImageData imageData);

    StorageType getStorageType();

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
     * @return
     */
    String saveFile(Base64EncodedFile base64EncodedFile, Long parentEntityId, String fileName, String parentEntityType);

}
