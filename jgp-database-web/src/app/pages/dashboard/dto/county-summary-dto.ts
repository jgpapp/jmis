export interface CountySummaryDto {
    countyCode: string | undefined;
    countyName: string | undefined;
    approximateCenterLatitude: number | undefined;
    approximateCenterLongitude: number | undefined;
    businessesTrained: number | undefined;
    businessesLoaned: number | undefined;
    amountDisbursed: number | undefined;
    businessesMentored: number | undefined;
}