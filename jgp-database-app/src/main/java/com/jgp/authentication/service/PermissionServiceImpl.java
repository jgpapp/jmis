package com.jgp.authentication.service;

import com.jgp.authentication.domain.PermissionRepository;
import com.jgp.authentication.dto.PermissionDto;
import lombok.RequiredArgsConstructor;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Service;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;

@Service
@RequiredArgsConstructor
public class PermissionServiceImpl implements PermissionService {

    private final PermissionRepository permissionRepository;
    private final JdbcTemplate jdbcTemplate;

    @Override
    public Collection<PermissionDto> retrieveAllPermissions() {
        final var permissionDtoRowMapper = new PermissionDtoMapper();
        final String sql = permissionDtoRowMapper.permissionSchema();
        return this.jdbcTemplate.query(sql, permissionDtoRowMapper);
    }

    @Override
    public Collection<PermissionDto> retrieveAllRolePermissions(Long roleId) {
        final var permissionDtoRowMapper = new PermissionDtoMapper();
        final String sql = permissionDtoRowMapper.rolePermissionSchema();
        return this.jdbcTemplate.query(sql, permissionDtoRowMapper, roleId);
    }

    private static final class PermissionDtoMapper implements RowMapper<PermissionDto> {

        @Override
        public PermissionDto mapRow(final ResultSet rs, @SuppressWarnings("unused") final int rowNum) throws SQLException {

            final String code = rs.getString("code");
            final String entityName = rs.getString("entityName");
            final String actionName = rs.getString("actionName");
            final boolean selected = rs.getBoolean("selected");

            return new PermissionDto(0L, code, entityName, actionName, selected);
        }

        public String permissionSchema() {
            return "select p.code, p.entity_name as entityName, p.action_name as actionName, true as selected"
                    + " from permission p order by coalesce(entity_name, ''), p.code";
        }

        public String rolePermissionSchema() {
            return "select p.code, p.entity_name as entityName, p.action_name as actionName, rp.role_id IS NOT NULL as selected "
                    + " from permission p left join role_permission rp on rp.permission_id = p.id and rp.role_id = ? "
                    + " order by COALESCE(entity_name, ''), p.code";
        }
    }
}
