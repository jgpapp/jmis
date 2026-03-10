package com.jgp.patner.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;


@Repository
public interface PartnerRepository extends JpaRepository<Partner, Long> {

    @Query("select p from Partner p where p.dataStatus = 'APPROVED'")
    List<Partner> findAllPartners();


}
