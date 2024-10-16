package com.jgp.bmo.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface BMOClientDataRepository extends JpaRepository<BMOParticipantData, Long>, JpaSpecificationExecutor<BMOParticipantData>, QuerydslPredicateExecutor<BMOParticipantData> {
}
