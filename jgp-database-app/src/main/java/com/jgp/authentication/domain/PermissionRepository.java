package com.jgp.authentication.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Collection;

public interface PermissionRepository extends JpaRepository<Permission, Long> {

    @Query("SELECT p FROM Permission p WHERE LOWER(TRIM(BOTH FROM p.code)) = LOWER(TRIM(BOTH FROM ?1))")
    Permission findOneByCode(String code);

    @Query("SELECT p FROM Permission p WHERE p.code IN ?1")
    Collection<Permission> findAllByCodes(Collection<String> codes);

    Permission findOneByCodeIgnoreCase(String code);
}
