package com.jgp.participant.domain;

import org.jspecify.annotations.NonNull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface ParticipantRepository extends JpaRepository<Participant, Long> , JpaSpecificationExecutor<Participant>, QuerydslPredicateExecutor<Participant> {

    Optional<Participant> findByJgpId(String jgpId);

    @Transactional
    @Modifying
    @Query(value = """
            delete from participants p \s
            where p.id in ?1 and not exists (select 1 from ta_participants_data bpd where bpd.participant_id = p.id)\s
            and not exists (select 1 from loans l where l.participant_id = p.id)\s
            and not exists (select 1 from mentor_ships m where m.participant_id = p.id)\s
            and not exists (select 1 from outcome_monitoring om where om.participant_id = p.id)\s
            """, nativeQuery = true)
    void deleteParticipantsByIds(@NonNull List<Long> participantIds);


    @Query("SELECT DISTINCT p.locationCountyCode from Participant p")
    Set<String> getParticipantOperationCounties();

    List<Participant> findByJgpIdInAndIsDeletedFalse(@NonNull Collection<String> jgpIds);


}
