package com.jgp.authentication.mapper;

import com.jgp.authentication.domain.Permission;
import com.jgp.authentication.dto.PermissionDto;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.NullValueCheckStrategy;
import org.mapstruct.NullValueMappingStrategy;

import java.util.List;

@Mapper(componentModel = "spring", nullValueMappingStrategy = NullValueMappingStrategy.RETURN_DEFAULT, nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface PermissionMapper {

    @Mapping(target = "id", expression = "java(permission.getId())")
    @Mapping(target = "code", expression = "java(permission.getCode())")
    @Mapping(target = "entityName", expression = "java(permission.getEntityName())")
    @Mapping(target = "actionName", expression = "java(permission.getActionName())")
    @Mapping(target = "selected", ignore = true)
    PermissionDto toDto(Permission permission);

    List<PermissionDto> toDtoList(List<Permission> permissions);
}
