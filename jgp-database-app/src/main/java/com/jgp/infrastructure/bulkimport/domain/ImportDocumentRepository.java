
package com.jgp.infrastructure.bulkimport.domain;

import org.jspecify.annotations.NonNull;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface ImportDocumentRepository extends JpaRepository<ImportDocument, Long>, JpaSpecificationExecutor<ImportDocument> {

    @EntityGraph(attributePaths = {"document", "partner", "createdBy"})
    @Query("SELECT i FROM ImportDocument i WHERE i.id = :id AND i.isDeleted = false")
    Optional<ImportDocument> findByIdWithRelations(@Param("id") Long importDocumentId);

    Page<ImportDocument> findByPartnerIdAndEntityTypeAndIsDeletedFalse(@NonNull Long partnerId, Integer entityType, Pageable pageable);

    Page<ImportDocument> findByEntityTypeAndIsDeletedFalse(@NonNull Integer entityType, Pageable pageable);
}
