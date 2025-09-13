package com.jgp.bmo.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.List;

@Repository
public interface BMOClientDataRepository extends JpaRepository<BMOParticipantData, Long>, JpaSpecificationExecutor<BMOParticipantData>, QuerydslPredicateExecutor<BMOParticipantData> {

    @Transactional
    @Modifying
    @Query(value = "update bmo_participants_data b set is_deleted = true where b.id in ?1", nativeQuery = true)
    void deleteTADataByIds(@NonNull Collection<Long> ids);

    List<BMOParticipantData> findByDocumentIdAndIsDeleted(@NonNull Long documentId, @NonNull Boolean isDeleted);


}
