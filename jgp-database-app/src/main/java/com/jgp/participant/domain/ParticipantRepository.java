package com.jgp.participant.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;

import java.util.Optional;

public interface ParticipantRepository extends JpaRepository<Participant, Long> , JpaSpecificationExecutor<Participant>, QuerydslPredicateExecutor<Participant> {

    Optional<Participant> findByJgpId(String jgpId);


}
