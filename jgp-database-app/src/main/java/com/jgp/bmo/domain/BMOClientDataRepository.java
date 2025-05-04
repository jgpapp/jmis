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
    @Query("select b from BMOParticipantData b where b.participant.id = ?1")
    List<BMOParticipantData> findByParticipantId(@NonNull Long id);


    @Transactional
    @Modifying
    @Query("delete from BMOParticipantData b where b.id in ?1")
    void deleteTADataByIds(@NonNull Collection<Long> ids);

}
