package com.jgp.infrastructure.bulkimport.data;

import org.springframework.web.multipart.MultipartFile;

public record ImportRequestDto(
        String entity,
        MultipartFile fileDetail,
        String importProgressUUID,
        String updateParticipantInfo,
        String appDomainForNotification
) {
}
