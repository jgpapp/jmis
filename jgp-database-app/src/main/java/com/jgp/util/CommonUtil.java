package com.jgp.util;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.Locale;
import java.util.Optional;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author simiyu
 */
@Slf4j
public abstract class CommonUtil {

    public static final String NO_RESOURCE_FOUND_WITH_ID = "No resource found with Id";
    public static final String RESOURCE_CREATED = "Resource Created";
    public static final String RESOURCE_UPDATED = "Resource Updated";

    public static final String NO_FILE_TO_UPLOAD = "No File To Upload Was Found!";

    public static final String STATUS_CELL_IMPORTED = "Imported";

    public static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();

    private CommonUtil() {
    }

    public static String getCountyByCode(String code){
        return getKenyanCountiesMap().get(code);
    }

    public static Map<String, String> getKenyanCountiesMap(){
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
        map.put("021", "Murang\'a");
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
        MOMBASA("1", "Mombasa"),
        KWALE("2", "Kwale"),
        KILIFI("3", "Kilifi"),
        TANA_RIVER("4", "Tana River"),
        LAMU("5", "Lamu"),
        TAITA_TAVETA("6", "Taita–Taveta"),
        GARISSA("7", "Garissa"),
        WAJIR("8", "Wajir"),
        MANDERA("9", "Mandera"),
        MARSABIT("10", "Marsabit"),
        ISIOLO("11", "Isiolo"),
        MERU("12", "Meru"),
        THARAKA_NITHI("13", "Tharaka-Nithi"),
        EMBU("14", "Embu"),
        KITUI("15", "Kitui"),
        MACHAKOS("16", "Machakos"),
        MAKUENI("17", "Makueni"),
        NYANDARUA("18", "Nyandarua"),
        NYERI("19", "Nyeri"),
        KIRINYAGA("20", "Kirinyaga"),
        MURANG_A("21", "Murang\'a"),
        KIAMBU("22", "Kiambu"),
        TURKANA("23", "Turkana"),
        WEST_POKOT("24", "West Pokot"),
        SAMBURU("25", "Samburu"),
        TRANS_NZOIA("26", "Trans-Nzoia"),
        UASIN_GISHU("27", "Uasin Gishu"),
        ELGEYO_MARAKWET("28", "Elgeyo-Marakwet"),
        NANDI("29", "Nandi"),
        BARINGO("30", "Baringo"),
        LAIKIPIA("31", "Laikipia"),
        NAKURU("32", "Nakuru"),
        NAROK("33", "Narok"),
        KAJIADO("34", "Kajiado"),
        KERICHO("35", "Kericho"),
        BOMET("36", "Bomet"),
        KAKAMEGA("37", "Kakamega"),
        VIHIGA("38", "Vihiga"),
        BUNGOMA("39", "Bungoma"),
        BUSIA("40", "Busia"),
        SIAYA("41", "Siaya"),
        KISUMU("42", "Kisumu"),
        HOMA_BAY("43", "Homa Bay"),
        MIGORI("44", "Migori"),
        KISII("45", "Kisii"),
        NYAMIRA("46", "Nyamira"),
        NAIROBI("47", "Nairobi"),
        UNKNOWN("999", "Unknown");

        private final String countyCode;
        private final String countyName;

        public static Optional<KenyanCounty> getKenyanCountyFromName(String name) {
            if (null == name) {
                return Optional.empty();
            }
            var county =  Arrays.stream(KenyanCounty.values())
                    .filter(kc -> {
                        final var startWith = kc.countyName.split("[^a-zA-Z0-9']")[0];
                        return name.equalsIgnoreCase(kc.countyName) || name.toUpperCase().startsWith(startWith.toUpperCase());
                    })
                    .findAny();
            return county.isPresent() ? county : Optional.of(KenyanCounty.UNKNOWN);
        }

        public static Optional<KenyanCounty> getKenyanCountyFromCode(String code) {
            if (null == code) {
                return Optional.empty();
            }
            var county =  Arrays.stream(KenyanCounty.values())
                    .filter(kc -> code.equals(kc.countyCode))
                    .findAny();
            return county.isPresent() ? county : Optional.of(KenyanCounty.UNKNOWN);
        }


    }

    public static boolean isStringValueLengthNotValid(String stringValue, int min, int max){
        if (null == stringValue){
            return false;
        }
        final var strLength = stringValue.length();
        return strLength < min || strLength > max;
    }

    public static String defaultToOtherIfStringIsNull(String value){
        return null != value ? value : "Other";
    }

    public static BigDecimal getPercentage(BigDecimal value, BigDecimal total){
        if (value.compareTo(BigDecimal.ZERO) <= 0 || total.compareTo(BigDecimal.ZERO) <= 0){
            return BigDecimal.ZERO;
        }
        return (value.divide(total, 2, RoundingMode.HALF_UP)).multiply(BigDecimal.valueOf(100));
    }
}
