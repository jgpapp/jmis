package com.jgp.util;

import java.math.BigDecimal;
import java.text.NumberFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;

import com.jgp.dashboard.dto.DashboardSearchCriteria;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

/**
 *
 * @author simiyu
 */
@Slf4j
public abstract class CommonUtil {

    public static final String NO_RESOURCE_FOUND_WITH_ID = "No resource found with Id";
    public static final String RESOURCE_CREATED = "Resource Created";
    public static final String RESOURCE_UPDATED = "Resource Updated";
    public static final String RESOURCE_DELETED = "Resource Deleted";

    public static final String NO_FILE_TO_UPLOAD = "No File To Upload Was Found!";

    public static final String STATUS_CELL_IMPORTED = "Imported";

    @Value("${jgp.dashboard.default.view.period.in.months}")
    private Integer jgpDashboardDefaultViewPeriodInMonths;


    private CommonUtil() {
    }

    public static NumberFormat getNumberFormat() {
        var nf = NumberFormat.getInstance();
        nf.setGroupingUsed(true);
        nf.setMaximumFractionDigits(2);
        nf.setMinimumFractionDigits(0);
        return nf;
    }


    public static String getConvertedCountyCode(String countyCode) {
        if (countyCode.length() == 1) {
            countyCode = String.format("00%s", countyCode);
        }
        if (countyCode.length() == 2) {
            countyCode = String.format("0%s", countyCode);
        }
        return countyCode;
    }

    public static String getCountyByCode(String code) {
        final var sanitizedCode = getConvertedCountyCode(code);
        return getKenyanCountiesMap().get(sanitizedCode);
    }


    public static Map<String, String> getKenyanCountiesMap() {
        Map<String, String> map = new HashMap<>();
        map.put("001", "Mombasa");
        map.put("002", "Kwale");
        map.put("003", "Kilifi");
        map.put("004", "Tana River");
        map.put("005", "Lamu");
        map.put("006", "Taita–Taveta");
        map.put("007", "Garissa");
        map.put("008", "Wajir");
        map.put("009", "Mandera");
        map.put("010", "Marsabit");
        map.put("011", "Isiolo");
        map.put("012", "Meru");
        map.put("013", "Tharaka-Nithi");
        map.put("014", "Embu");
        map.put("015", "Kitui");
        map.put("016", "Machakos");
        map.put("017", "Makueni");
        map.put("018", "Nyandarua");
        map.put("019", "Nyeri");
        map.put("020", "Kirinyaga");
        map.put("021", "Murang'a");
        map.put("022", "Kiambu");
        map.put("023", "Turkana");
        map.put("024", "West Pokot");
        map.put("025", "Samburu");
        map.put("026", "Trans-Nzoia");
        map.put("027", "Uasin Gishu");
        map.put("028", "Elgeyo-Marakwet");
        map.put("029", "Nandi");
        map.put("030", "Baringo");
        map.put("031", "Laikipia");
        map.put("032", "Nakuru");
        map.put("033", "Narok");
        map.put("034", "Kajiado");
        map.put("035", "Kericho");
        map.put("036", "Bomet");
        map.put("037", "Kakamega");
        map.put("038", "Vihiga");
        map.put("039", "Bungoma");
        map.put("040", "Busia");
        map.put("041", "Siaya");
        map.put("042", "Kisumu");
        map.put("043", "Homa Bay");
        map.put("044", "Migori");
        map.put("045", "Kisii");
        map.put("046", "Nyamira");
        map.put("047", "Nairobi");
        return map;
    }


    @RequiredArgsConstructor
    @Getter
    public enum KenyanCounty {
        MOMBASA("1", "Mombasa", new BigDecimal("-4.0437"), new BigDecimal("39.6682")),
        KWALE("2", "Kwale", new BigDecimal("-4.1793"), new BigDecimal("39.4589")),
        KILIFI("3", "Kilifi", new BigDecimal("-3.6334"), new BigDecimal("39.9213")),
        TANA_RIVER("4", "Tana River", new BigDecimal("-1.4883"), new BigDecimal("40.0305")),
        LAMU("5", "Lamu", new BigDecimal("-2.2709"), new BigDecimal("40.9015")),
        TAITA_TAVETA("6", "Taita–Taveta", new BigDecimal("-3.3986"), new BigDecimal("38.3687")),
        GARISSA("7", "Garissa", new BigDecimal("-0.4578"), new BigDecimal("39.6467")),
        WAJIR("8", "Wajir", new BigDecimal("1.7479"), new BigDecimal("40.0634")),
        MANDERA("9", "Mandera", new BigDecimal("3.9360"), new BigDecimal("41.8317")),
        MARSABIT("10", "Marsabit", new BigDecimal("2.3276"), new BigDecimal("37.9892")),
        ISIOLO("11", "Isiolo", new BigDecimal("0.3547"), new BigDecimal("37.5878")),
        MERU("12", "Meru", new BigDecimal("0.0463"), new BigDecimal("37.6482")),
        THARAKA_NITHI("13", "Tharaka-Nithi", new BigDecimal("-0.3204"), new BigDecimal("37.6481")),
        EMBU("14", "Embu", new BigDecimal("-0.5367"), new BigDecimal("37.4583")),
        KITUI("15", "Kitui", new BigDecimal("-1.3786"), new BigDecimal("38.0076")),
        MACHAKOS("16", "Machakos", new BigDecimal("-1.5204"), new BigDecimal("37.2625")),
        MAKUENI("17", "Makueni", new BigDecimal("-1.9701"), new BigDecimal("37.6256")),
        NYANDARUA("18", "Nyandarua", new BigDecimal("-0.2635"), new BigDecimal("36.3861")),
        NYERI("19", "Nyeri", new BigDecimal("-0.4215"), new BigDecimal("36.9535")),
        KIRINYAGA("20", "Kirinyaga", new BigDecimal("-0.5186"), new BigDecimal("37.3753")),
        MURANG_A("21", "Murang'a", new BigDecimal("-0.7107"), new BigDecimal("37.1472")),
        KIAMBU("22", "Kiambu", new BigDecimal("-1.1714"), new BigDecimal("36.8378")),
        TURKANA("23", "Turkana", new BigDecimal("3.1166"), new BigDecimal("35.5976")),
        WEST_POKOT("24", "West Pokot", new BigDecimal("1.2384"), new BigDecimal("35.1118")),
        SAMBURU("25", "Samburu", new BigDecimal("1.0963"), new BigDecimal("36.5651")),
        TRANS_NZOIA("26", "Trans-Nzoia", new BigDecimal("0.9634"), new BigDecimal("35.0033")),
        UASIN_GISHU("27", "Uasin Gishu", new BigDecimal("0.5167"), new BigDecimal("35.2667")),
        ELGEYO_MARAKWET("28", "Elgeyo-Marakwet", new BigDecimal("0.6729"), new BigDecimal("35.2934")),
        NANDI("29", "Nandi", new BigDecimal("0.2030"), new BigDecimal("35.1009")),
        BARINGO("30", "Baringo", new BigDecimal("0.4851"), new BigDecimal("35.7369")),
        LAIKIPIA("31", "Laikipia", new BigDecimal("0.0336"), new BigDecimal("37.0734")),
        NAKURU("32", "Nakuru", new BigDecimal("-0.2831"), new BigDecimal("36.0743")),
        NAROK("33", "Narok", new BigDecimal("-1.0827"), new BigDecimal("35.8927")),
        KAJIADO("34", "Kajiado", new BigDecimal("-1.8596"), new BigDecimal("36.7876")),
        KERICHO("35", "Kericho", new BigDecimal("-0.3686"), new BigDecimal("35.2891")),
        BOMET("36", "Bomet", new BigDecimal("-0.7938"), new BigDecimal("35.3414")),
        KAKAMEGA("37", "Kakamega", new BigDecimal("0.2818"), new BigDecimal("34.7505")),
        VIHIGA("38", "Vihiga", new BigDecimal("0.0526"), new BigDecimal("34.7212")),
        BUNGOMA("39", "Bungoma", new BigDecimal("0.5693"), new BigDecimal("34.5606")),
        BUSIA("40", "Busia", new BigDecimal("0.4633"), new BigDecimal("34.1084")),
        SIAYA("41", "Siaya", new BigDecimal("0.0560"), new BigDecimal("34.2981")),
        KISUMU("42", "Kisumu", new BigDecimal("-0.1022"), new BigDecimal("34.7617")),
        HOMA_BAY("43", "Homa Bay", new BigDecimal("-0.5285"), new BigDecimal("34.4608")),
        MIGORI("44", "Migori", new BigDecimal("-1.0667"), new BigDecimal("34.4667")),
        KISII("45", "Kisii", new BigDecimal("-0.6787"), new BigDecimal("34.7656")),
        NYAMIRA("46", "Nyamira", new BigDecimal("-0.5732"), new BigDecimal("34.9392")),
        NAIROBI("47", "Nairobi", new BigDecimal("-1.2921"), new BigDecimal("36.8219")),
        UNKNOWN("999", "Unknown", null, null);

        private final String countyCode;
        private final String countyName;
        private final BigDecimal approximateCenterLatitude;
        private final BigDecimal approximateCenterLongitude;

        public static Optional<KenyanCounty> getKenyanCountyFromName(String name) {
            if (null == name) {
                return Optional.empty();
            }
            var county = Arrays.stream(KenyanCounty.values())
                    .filter(kc -> {
                        final var startWith = kc.countyName.split("[^a-zA-Z0-9']")[0];
                        return name.equalsIgnoreCase(kc.countyName) || name.toUpperCase().startsWith(startWith.toUpperCase());
                    })
                    .findAny();
            return county.isPresent() ? county : Optional.of(KenyanCounty.UNKNOWN);
        }
    }

    public static boolean isStringValueLengthNotValid(String stringValue, int min, int max) {
        if (null == stringValue) {
            return false;
        }
        final var strLength = stringValue.length();
        return strLength < min || strLength > max;
    }

    public static String sanitizeString(String input) {
        if (input == null) {
            return null;
        }
        // Step 1: Trim leading and trailing whitespace
        var trimmedString = input.trim();

        // Step 2: Replace any non-alphabet characters with a single space
        // Using regex: [^a-zA-Z] matches any character that is NOT an alphabet (uppercase or lowercase)
        var nonAlphabetsReplaced = trimmedString.replaceAll("[^a-zA-Z]", " ");

        // Step 3: Replace all sequences of spaces with a single space
        // Using regex: \\s+ matches one or more whitespace characters
        return nonAlphabetsReplaced.replaceAll("\\s+", " ");
    }

    public static boolean stringDoesNotContainOnlyDigits(String input) {
        return !Pattern.compile(".*\\d.*")
                .matcher(input)
                .matches();
    }

    public static String defaultToOtherIfStringIsNull(String value) {
        return null != value ? value : "Other";
    }

    public static Pair<Integer, Integer> getAgeRangeFromAgeGroup(String ageGroup) {
        if (null == ageGroup) {
            return Pair.of(0, 0);
        } else if ("18-24".equalsIgnoreCase(ageGroup)) {
            return Pair.of(18, 24);
        } else if ("25-34".equalsIgnoreCase(ageGroup)) {
            return Pair.of(25, 34);
        } else if ("35+".equalsIgnoreCase(ageGroup)) {
            return Pair.of(35, 150);
        } else {
            return Pair.of(0, 0); // Default case
        }
    }

    public static MapSqlParameterSource getCommonMapSqlParameterSource(DashboardSearchCriteria dashboardSearchCriteria) {
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)) {
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        MapSqlParameterSource parameters = new MapSqlParameterSource("fromDate", fromDate);
        parameters.addValue("toDate", toDate);
        return parameters;

    }

    public static Pair<LocalDate, LocalDate> getDefaultQueryDates() {
        final var dateToday = LocalDate.now();
        return new ImmutablePair<>(LocalDate.now(ZoneId.systemDefault()).minusMonths(6), dateToday);
    }

}
