package com.jgp.authentication.domain;

import jakarta.annotation.Nonnull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface AppUserRepository extends JpaRepository<AppUser, Long>, JpaSpecificationExecutor<AppUser>, PlatformUserRepository,
        QuerydslPredicateExecutor<AppUser> {

    Optional<AppUser> findByUsername(@Nonnull String username);

    @Query("select a from AppUser a where a.partner.id = ?1")
    List<AppUser> findUsersByPartnerId(@NonNull Long id);


}
