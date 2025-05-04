package com.jgp.participant.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.lang.NonNull;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

public interface ParticipantRepository extends JpaRepository<Participant, Long> , JpaSpecificationExecutor<Participant>, QuerydslPredicateExecutor<Participant> {

    Optional<Participant> findByJgpId(String jgpId);

    @Transactional
    @Modifying
    @Query(value = """
            delete from participants p\s
            where not exists (select 1 from bmo_participants_data bpd where bpd.participant_id = p.id)\s
            and not exists (select 1 from loans l where l.participant_id = p.id)\s
            and p.id in ?1""", nativeQuery = true)
    void deleteParticipantsByIds(@NonNull List<Long> participantIds);


}
