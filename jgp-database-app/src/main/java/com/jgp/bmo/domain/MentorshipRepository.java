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
public interface MentorshipRepository extends JpaRepository<Mentorship, Long> , JpaSpecificationExecutor<Mentorship>, QuerydslPredicateExecutor<Mentorship> {

    @Query("select b from Mentorship b where b.participant.id = ?1")
    List<Mentorship> findByParticipantId(@NonNull Long id);


    @Transactional
    @Modifying
    @Query(value = "update mentor_ships m set is_deleted = true where m.id in ?1", nativeQuery = true)
    void deleteMentorshipDataByIds(@NonNull Collection<Long> ids);

    List<Mentorship> findByDocumentIdAndIsDeleted(@NonNull Long id, @NonNull Boolean isDeleted);

    List<Mentorship> findByDocumentId(@NonNull Long id);
}
