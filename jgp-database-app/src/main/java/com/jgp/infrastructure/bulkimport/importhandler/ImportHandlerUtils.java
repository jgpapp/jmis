
package com.jgp.infrastructure.bulkimport.importhandler;

import com.google.common.base.Splitter;
import com.jgp.shared.dto.ApiParameterError;
import com.jgp.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.CellValue;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.FormulaEvaluator;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.SheetVisibility;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellReference;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Objects;

@Slf4j
public final class ImportHandlerUtils {

    private ImportHandlerUtils() {

    }

    public static Integer getNumberOfRows(Sheet sheet, int primaryColumn) {
        int noOfEntries = 0;
        // getLastRowNum and getPhysicalNumberOfRows showing false values
        // sometimes
        while (sheet.getRow(noOfEntries + 1) != null && sheet.getRow(noOfEntries + 1).getCell(primaryColumn) != null && CellType.BLANK != sheet.getRow(noOfEntries + 1).getCell(primaryColumn).getCellType()) {
            noOfEntries++;
        }

        return noOfEntries;
    }

    public static boolean isNotImported(Row row, int statusColumn) {
        if (readAsString(statusColumn, row) != null) {
            return !CommonUtil.STATUS_CELL_IMPORTED.equals(readAsString(statusColumn, row));
        } else {
            return true;
        }
    }

    public static Long readAsLong(int colIndex, Row row) {
        Cell c = row.getCell(colIndex);
        if (c == null || c.getCellType() == CellType.BLANK) {
            return null;
        }
        FormulaEvaluator eval = row.getSheet().getWorkbook().getCreationHelper().createFormulaEvaluator();
        if (c.getCellType() == CellType.FORMULA) {
            if (eval != null) {
                CellValue val = null;
                try {
                    val = eval.evaluate(c);
                } catch (NullPointerException npe) {
                    return null;
                }
                return ((Double) val.getNumberValue()).longValue();
            }
        } else if (c.getCellType() == CellType.NUMERIC) {
            return ((Double) c.getNumericCellValue()).longValue();
        } else {
            return Long.parseLong(row.getCell(colIndex).getStringCellValue());
        }
        return null;
    }

    public static String readAsString(int colIndex, Row row) {

        Cell c = row.getCell(colIndex);
        if (c == null || c.getCellType() == CellType.BLANK) {
            return null;
        }
        FormulaEvaluator eval = row.getSheet().getWorkbook().getCreationHelper().createFormulaEvaluator();
        if (c.getCellType() == CellType.FORMULA) {
            if (eval != null) {
                CellValue val = null;
                try {
                    val = eval.evaluate(c);
                } catch (NullPointerException npe) {
                    return null;
                }

                String res = trimEmptyDecimalPortion(val.getStringValue());
                if (res != null) {
                    if (!res.isEmpty()) {
                        return res.trim();
                    } else {
                        return null;
                    }
                } else {
                    return null;
                }
            } else {
                return null;
            }
        } else if (c.getCellType() == CellType.STRING) {
            String res = trimEmptyDecimalPortion(c.getStringCellValue().trim());
            return null == res ? null : res.trim();

        } else if (c.getCellType() == CellType.NUMERIC) {
            return ((Double) row.getCell(colIndex).getNumericCellValue()).longValue() + "";
        } else if (c.getCellType() == CellType.BOOLEAN) {
            return c.getBooleanCellValue() + "";
        } else {
            return null;
        }
    }

    public static String trimEmptyDecimalPortion(String result) {
        if (result != null && result.endsWith(".0")) {
            return Splitter.on("\\.").split(result).iterator().next();
        } else {
            return result;
        }
    }

    public static LocalDate readAsDate(int colIndex, Row row) {
        Cell c = row.getCell(colIndex);
        if (c == null || c.getCellType() == CellType.BLANK) {
            return null;
        } else if (c.getCellType() == CellType.STRING) {
            String res = trimEmptyDecimalPortion(c.getStringCellValue().trim());
            return Objects.nonNull(res) ? LocalDate.parse(res.trim()) : null;

        }
        return c.getLocalDateTimeCellValue().atZone(ZoneId.systemDefault()).toLocalDate();
    }

    public static LocalDate readAsISOFormattedDate(int colIndex, Row row) {
        Cell c = row.getCell(colIndex);

        if (c == null || c.getCellType() == CellType.BLANK) {
            return null;
        } else {
            DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            DataFormatter formatter = new DataFormatter();

            String cellValue;
            if (c.getCellType() == CellType.FORMULA) {
                cellValue = c.getCellFormula(); // Get the formula string itself
            } else {
                cellValue = formatter.formatCellValue(c); // For other cell types, format as usual
            }
            String res = trimEmptyDecimalPortion(cellValue.trim());
            return Objects.nonNull(res) ? LocalDate.parse(res.trim().replaceAll("\\s+", ""), dateFormatter) : null;

        }
    }

    public static Boolean readAsBoolean(int colIndex, Row row) {
        Cell c = row.getCell(colIndex);
        if (c == null || c.getCellType() == CellType.BLANK) {
            return false;
        }
        FormulaEvaluator eval = row.getSheet().getWorkbook().getCreationHelper().createFormulaEvaluator();
        if (c.getCellType() == CellType.FORMULA) {
            if (eval != null) {
                CellValue val = null;
                try {
                    val = eval.evaluate(c);
                } catch (NullPointerException npe) {
                    return false;
                }
                return val.getBooleanValue();
            }
            return false;
        } else if (c.getCellType() == CellType.BOOLEAN) {
            return c.getBooleanCellValue();
        } else {
            String booleanString = row.getCell(colIndex).getStringCellValue().trim();
            if (booleanString.equalsIgnoreCase("TRUE")) {
                return true;
            } else {
                return false;
            }
        }
    }

    public static Integer readAsInt(int colIndex, Row row) {
        Cell c = row.getCell(colIndex);
        if (c == null || c.getCellType() == CellType.BLANK) {
            return null;
        }
        FormulaEvaluator eval = row.getSheet().getWorkbook().getCreationHelper().createFormulaEvaluator();
        if (c.getCellType() == CellType.FORMULA) {
            if (eval != null) {
                CellValue val = null;
                try {
                    val = eval.evaluate(c);
                } catch (NullPointerException npe) {
                    return null;
                }
                return ((Double) val.getNumberValue()).intValue();
            }
            return null;
        } else if (c.getCellType() == CellType.NUMERIC) {
            return ((Double) c.getNumericCellValue()).intValue();
        } else {
            return Integer.parseInt(row.getCell(colIndex).getStringCellValue().trim());
        }
    }

    public static Double readAsDouble(int colIndex, Row row) {
        Cell c = row.getCell(colIndex);
        if (c == null || c.getCellType() == CellType.BLANK) {
            return 0.0;
        }
        FormulaEvaluator eval = row.getSheet().getWorkbook().getCreationHelper().createFormulaEvaluator();
        if (c.getCellType() == CellType.FORMULA) {
            if (eval != null) {
                CellValue val = null;
                try {
                    val = eval.evaluate(c);
                } catch (NullPointerException npe) {
                    return 0.0;
                }
                return val.getNumberValue();
            } else {
                return 0.0;
            }
        } else if (c.getCellType() == CellType.NUMERIC) {
            return row.getCell(colIndex).getNumericCellValue();
        } else {
            return Double.parseDouble(row.getCell(colIndex).getStringCellValue());
        }
    }

    public static void writeString(int colIndex, Row row, String value) {
        if (value != null) {
            row.createCell(colIndex).setCellValue(value);
        }
    }

    public static CellStyle getCellStyle(Workbook workbook, IndexedColors color) {
        CellReference cellReference = new CellReference("A1");
        Sheet predefined = workbook.getSheet(color.toString());
        // if we have already defined this style, return it and don't create
        // another one
        if (predefined != null) {
            Row row = predefined.getRow(cellReference.getRow());
            Cell cell = row.getCell(cellReference.getCol());
            return cell.getCellStyle();
        }
        CellStyle style = workbook.createCellStyle();
        style.setFillForegroundColor(color.getIndex());
        style.setFillPattern(FillPatternType.SOLID_FOREGROUND);

        Sheet cache = workbook.createSheet(color.toString());
        workbook.setSheetVisibility(workbook.getSheetIndex(cache), SheetVisibility.VERY_HIDDEN);
        Row row = cache.createRow(cellReference.getRow());
        Cell cell = row.createCell(cellReference.getCol());
        cell.setCellStyle(style);

        return style;
    }

    public static String getDefaultUserMessages(List<ApiParameterError> apiParamErrors) {
        StringBuilder defaultUserMessages = new StringBuilder();
        for (ApiParameterError error : apiParamErrors) {
            defaultUserMessages = defaultUserMessages.append(error.getDefaultUserMessage()).append('\t');
        }
        return defaultUserMessages.toString();
    }

    public static String getErrorList(List<String> errorList) {
        StringBuilder errors = new StringBuilder();
        for (String error : errorList) {
            errors = errors.append(error);
        }
        return errors.toString();
    }

    public static void writeErrorMessage(Sheet sheet, Integer rowIndex, String errorMessage, int statusColumn) {
        Cell statusCell = sheet.getRow(rowIndex).createCell(statusColumn);
        statusCell.setCellValue(errorMessage);
        statusCell.setCellStyle(getCellStyle(sheet.getWorkbook(), IndexedColors.RED));
    }

    public static String getErrorMessage(Exception re) {
        if (re.getMessage() != null) {
            return re.getMessage();
        } else {
            return re.getClass().getCanonicalName();
        }
    }


}
