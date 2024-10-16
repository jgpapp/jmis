package com.jgp.authentication.api;

import com.jgp.authentication.dto.PermissionDto;
import com.jgp.authentication.dto.RoleDto;
import com.jgp.authentication.service.RoleService;
import com.jgp.shared.dto.ApiResponseDto;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collection;
import java.util.List;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/roles")
public class RoleController {

    private final RoleService roleService;

    @GetMapping
    public ResponseEntity<Collection<RoleDto>> getAvailableUserRoles(@RequestParam(value = "user-id", required = false) Long userId){
        return new ResponseEntity<>(this.roleService.retrieveAppUserRoles(userId), HttpStatus.OK);
    }

    @PostMapping
    public ResponseEntity<ApiResponseDto> createRole(@Valid @RequestBody RoleDto roleDto){
        this.roleService.createRole(roleDto);
        return new ResponseEntity<>(new ApiResponseDto(true, "Role created !!"), HttpStatus.CREATED);
    }

    @PutMapping("{roleId}")
    public ResponseEntity<ApiResponseDto> updateRole(@PathVariable("roleId") Long roleId, @Valid @RequestBody RoleDto roleDto){
        this.roleService.updateRole(roleId, roleDto);
        return new ResponseEntity<>(new ApiResponseDto(true, "Role updated !!"), HttpStatus.OK);
    }

    @PutMapping("{roleId}/update-permissions")
    public ResponseEntity<ApiResponseDto> updateRolePermissions(@PathVariable("roleId") Long roleId, @RequestBody List<String> permissionCodes){
        this.roleService.updateRolePermissions(roleId, permissionCodes);
        return new ResponseEntity<>(new ApiResponseDto(true, "Role Permissions updated !!"), HttpStatus.OK);
    }

    @GetMapping("{roleId}")
    public ResponseEntity<RoleDto> getUser(@PathVariable("roleId") Long roleId){
        return new ResponseEntity<>(this.roleService.retrieveOne(roleId), HttpStatus.OK);
    }

    @GetMapping("{roleId}/role-permissions")
    public ResponseEntity<Collection<PermissionDto>> getAvailableRolePermissions(@PathVariable("roleId") Long roleId){
        return new ResponseEntity<>(this.roleService.retrieveRolesPermission(roleId), HttpStatus.OK);
    }

    @DeleteMapping("{roleId}")
    public ResponseEntity<ApiResponseDto> deleteRole(@PathVariable("roleId") Long roleId){
        this.roleService.deleteRole(roleId);
        return new ResponseEntity<>(new ApiResponseDto(true, "Role deleted !!"), HttpStatus.NO_CONTENT);
    }
}
