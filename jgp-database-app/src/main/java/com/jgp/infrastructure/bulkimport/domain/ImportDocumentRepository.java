
package com.jgp.infrastructure.bulkimport.domain;

import org.jspecify.annotations.NonNull;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface ImportDocumentRepository extends JpaRepository<ImportDocument, Long>, JpaSpecificationExecutor<ImportDocument> {

    Page<ImportDocument> findByPartnerIdAndEntityTypeAndIsDeletedFalse(@NonNull Long partnerId, Integer entityType, Pageable pageable);

    Page<ImportDocument> findByEntityTypeAndIsDeletedFalse(@NonNull Integer entityType, Pageable pageable);
}
