package com.jgp.infrastructure.core.domain;

import org.springframework.jdbc.support.JdbcUtils;

import java.math.BigDecimal;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class JdbcSupport {

    private JdbcSupport() {}

    public static ZonedDateTime getDateTime(final ResultSet rs, final String columnName) throws SQLException {
        ZonedDateTime dateTime = null;
        final Timestamp dateValue = rs.getTimestamp(columnName);
        if (dateValue != null) {
            dateTime = ZonedDateTime.ofInstant(dateValue.toInstant(), ZoneId.systemDefault());
        }
        return dateTime;
    }

    public static LocalDateTime getLocalDateTime(final ResultSet rs, final String columnName) throws SQLException {
        LocalDateTime dateTime = null;
        final Timestamp dateValue = rs.getTimestamp(columnName);
        if (dateValue != null) {
            dateTime = dateValue.toLocalDateTime();
        }
        return dateTime;
    }

    public static LocalDate getLocalDate(final ResultSet rs, final String columnName) throws SQLException {
        LocalDate localDate = null;
        final Date dateValue = rs.getDate(columnName);
        if (dateValue != null) {
            localDate = dateValue.toLocalDate();
        }
        return localDate;
    }

    public static LocalTime getLocalTime(final ResultSet rs, final String columnName) throws SQLException {
        LocalTime localTime = null;
        final Timestamp timeValue = rs.getTimestamp(columnName);
        if (timeValue != null) {
            localTime = timeValue.toLocalDateTime().toLocalTime();
        }
        return localTime;
    }

    public static Long getLong(final ResultSet rs, final String columnName) throws SQLException {
        return (Long) JdbcUtils.getResultSetValue(rs, rs.findColumn(columnName), Long.class);
    }

    public static Integer getInteger(final ResultSet rs, final String columnName) throws SQLException {
        return (Integer) JdbcUtils.getResultSetValue(rs, rs.findColumn(columnName), Integer.class);
    }

    public static Integer getIntegerDefaultToNullIfZero(final ResultSet rs, final String columnName) throws SQLException {
        final Integer value = (Integer) JdbcUtils.getResultSetValue(rs, rs.findColumn(columnName), Integer.class);
        return defaultToNullIfZero(value);
    }

    public static Long getLongDefaultToNullIfZero(final ResultSet rs, final String columnName) throws SQLException {
        final Long value = (Long) JdbcUtils.getResultSetValue(rs, rs.findColumn(columnName), Long.class);
        return defaultToNullIfZero(value);
    }

    private static Integer defaultToNullIfZero(final Integer value) {
        Integer result = value;
        if (result != null && Integer.valueOf(0).equals(value)) {
            result = null;
        }
        return result;
    }

    private static Long defaultToNullIfZero(final Long value) {
        Long result = value;
        if (result != null && Long.valueOf(0).equals(value)) {
            result = null;
        }
        return result;
    }

    public static BigDecimal getBigDecimalDefaultToZeroIfNull(final ResultSet rs, final String columnName) throws SQLException {
        final BigDecimal value = rs.getBigDecimal(columnName);
        return defaultToZeroIfNull(value);
    }

    private static BigDecimal defaultToZeroIfNull(final BigDecimal value) {
        BigDecimal result = BigDecimal.ZERO;
        if (value != null) {
            result = value;
        }
        return result;
    }

    public static BigDecimal getBigDecimalDefaultToNullIfZero(final ResultSet rs, final String columnName) throws SQLException {
        final BigDecimal value = rs.getBigDecimal(columnName);
        return defaultToNullIfZero(value);
    }

    private static BigDecimal defaultToNullIfZero(final BigDecimal value) {
        BigDecimal result = value;
        if (value != null && BigDecimal.ZERO.compareTo(value) == 0) {
            result = null;
        }
        return result;
    }
}
