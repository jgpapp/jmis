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
    @Query("delete from Mentorship b where b.id in ?1")
    void deleteMentorshipDataByIds(@NonNull Collection<Long> ids);
}
