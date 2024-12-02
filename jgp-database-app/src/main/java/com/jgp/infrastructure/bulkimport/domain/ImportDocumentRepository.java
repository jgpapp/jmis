
package com.jgp.infrastructure.bulkimport.domain;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.lang.NonNull;

public interface ImportDocumentRepository extends JpaRepository<ImportDocument, Long>, JpaSpecificationExecutor<ImportDocument> {
    Page<ImportDocument> findByPartnerIdAndEntityType(@NonNull Long partnerId, @NonNull Integer entityType, Pageable pageable);
    // no added behaviour

}
