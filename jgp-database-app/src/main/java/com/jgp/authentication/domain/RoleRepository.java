package com.jgp.authentication.domain;

import com.jgp.shared.domain.DataStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface RoleRepository extends JpaRepository<Role, Long> {

    @Query("SELECT COUNT(a) FROM AppUser a JOIN a.roles r WHERE r.id = :roleId AND a.isActive = true AND a.dataStatus = 'APPROVED'")
    Integer getCountOfRolesAssociatedWithUsers(@Param("roleId") Long roleId);

    @Query("SELECT role FROM Role role WHERE LOWER(role.roleName) = LOWER(:roleName) AND role.dataStatus = 'APPROVED'")
    Role getRoleByName(@Param("roleName") String roleName);

    @Query("SELECT role FROM Role role WHERE LOWER(role.roleName) IN :roleNames AND role.dataStatus = 'APPROVED'")
    List<Role> getRolesByNames(@Param("roleNames") List<String> roleNames);

    @Query("select r from Role r where r.dataStatus = 'APPROVED'")
    List<Role> findAllRoles();


}
