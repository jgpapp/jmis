package com.jgp.authentication.mapper;

import com.jgp.authentication.domain.Role;
import com.jgp.authentication.dto.RoleDto;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.NullValueCheckStrategy;
import org.mapstruct.NullValueMappingStrategy;

import java.util.List;

@Mapper(componentModel = "spring", nullValueMappingStrategy = NullValueMappingStrategy.RETURN_DEFAULT, nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface RoleMapper {

    @Mapping(target = "id", expression = "java(role.getId())")
    @Mapping(target = "roleName", expression = "java(role.getRoleName())")
    @Mapping(target = "description", expression = "java(role.getDescription())")
    @Mapping(target = "permissions", expression = "java(role.getPermissions().stream().map(com.jgp.authentication.domain.Permission::getCode).toList())")
    RoleDto toDto(Role role);

    List<RoleDto> toDtoList(List<Role> roles);
}
