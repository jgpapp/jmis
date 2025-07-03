package com.jgp.monitoring.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;

@Builder
public record OutComeMonitoringRequestDto(
        @NotNull(message = "Survey date is required")
        LocalDate surveyDate,
        @NotBlank(message = "Survey language is required")
        String surveyLanguage,
        //@Pattern(regexp = "Yes|No", message = "Consented must be Yes|No")
        @NotBlank(message = "Consented is required")
        String consented,
        BigDecimal locationLatitude,
        BigDecimal locationLongitude,
        @Min(value = 18, message = "Age must be at least 18")
        @NotNull(message = "Age is required")
        Integer age,
        //@Pattern(regexp = "Men 36+|Young Women|Young Men|Women 36+", message = "Gender category must be Men 36+|Young Women|Young Men|Women 36+")
        @NotBlank(message = "Gender category is required")
        String genderCategory,
        //@Pattern(regexp = "Micro|SME", message = "Segment must be Micro|SME")
        @NotBlank(message = "Segment is required")
        String segment,
        //@Pattern(regexp = "KEPSA|PBP|KNCCI|4G|GROOTS|GBF|Don't Know", message = "Partner must be KEPSA|PBP|KNCCI|4G|GROOTS|GBF|Don't Know")
        @NotBlank(message = "Partner is required")
        String partner,
        //@Pattern(regexp = "MALE|FEMALE|DON'T KNOW|REFUSED", message = "Gender must be MALE, FEMALE, DON'T KNOW or REFUSED")
        @NotBlank(message = "Gender is required")
        String gender,
        //@Pattern(regexp = "Central|Coast|Lower Eastern|Nairobi|North Eastern|North Rift|South Rift|Nyanza|Upper Eastern|Western", message = "Region must be Central|Coast|Lower Eastern|Nairobi|North Eastern|North Rift|South Rift|Nyanza|Upper Eastern|Western")
        @NotBlank(message = "Region is required")
        String region,
        String countyCode,
        @NotBlank(message = "County name is required")
        String countyName,
        //@Pattern(regexp = "Urban|Rural|DON'T KNOW|REFUSED", message = "Business setting must be Urban|Rural|DON'T KNOW|REFUSED")
        @NotBlank(message = "Business setting is required")
        String businessSetting,
        //@Pattern(regexp = "10+ Years|1-5 Years|6-10 Years", message = "Business age category must be 10+ Years|1-5 Years|6-10 Years")
        @NotBlank(message = "Business age category is required")
        String businessAgeCategory,
        //@Pattern(regexp = "Persons with disability|Refugee|None of the above|DON'T KNOW|REFUSED", message = "Group must be Persons with disability|Refugee|None of the above|DON'T KNOW|REFUSED")
        String groupMembership,
        //@Pattern(regexp = "No formal education|Primary school|Secondary school|Tertiary education|College|certificate/diploma|Graduate degree|ost graduate degree|DON'T KNOW|REFUSED", message = "Education level must be No formal education|Primary school|Secondary school|Tertiary education|College|certificate/diploma|Graduate degree|ost graduate degree|DON'T KNOW|REFUSED")
        String educationLevel,
        //@Min(value = 0, message = "Business age must be at least 0")
        Integer businessAge,
        //@Min(value = 0, message = "Regular employees must be at least 0")
        Integer regularEmployees,
        //@Min(value = 0, message = "Casual employees must be at least 0")
        Integer casualEmployees,
        //@Pattern(regexp = "Increased a lot|Increased|Stayed the same|Decreased|Decreased a lot|DON'T KNOW|REFUSED", message = "Income change must be Increased a lot|Increased|Stayed the same|Decreased|Decreased a lot|DON'T KNOW|REFUSED")
        String householdIncomeChange,
        //@Pattern(regexp = "Greatly improved|Somewhat improved|No change|Somewhat worse|Significantly declined|DON'T KNOW|REFUSED", message = "Financial Stability must be Greatly improved|Somewhat improved|No change|Somewhat worse|Significantly declined|DON'T KNOW|REFUSED")
        String financialStability,
        //@Pattern(regexp = "Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED", message = "Quality of life improved must be Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED")
        String qualityOfLife,
        //@Pattern(regexp = "Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED", message = "Feel more empowered must be Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED")
        String empowerment,
        //@Pattern(regexp = "Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED", message = "Voice heard in community must be Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED")
        String voiceInCommunity,
        //@Pattern(regexp = "Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED", message = "Feel respected in community must be Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED")
        String respectInCommunity,
        //@Pattern(regexp = "Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED", message = "Reliable income must be Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED")
        String reliableIncome,
        //@Pattern(regexp = "Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED", message = "Work is reputable must be Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED")
        String reputableWork,
        //@Pattern(regexp = "Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED", message = "Sense of purpose must be Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED")
        String senseOfPurpose,
        //@Pattern(regexp = "Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED", message = "Participate in business sector growth must be Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED")
        String businessSectorGrowth,
        //@Pattern(regexp = "Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED", message = "Participate in community growth must be Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED")
        String communityGrowth,
        //@Pattern(regexp = "Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED", message = "Access work opportunities must be Strongly Disagree|Disagree|Neutral|Agree|Strongly Agree|DON'T KNOW|REFUSED")
        String workOpportunities,
        //@Pattern(regexp = "Every week|At least once every two weeks|Once a month|Occasionally (less than once a month)|Not at all|Don't know|Refused", message = "Regularity of business income must be Every week|At least once every two weeks|Once a month|Occasionally (less than once a month)|Not at all|Don't know|Refused")
        String incomeRegularity,
        //@Pattern(regexp = "Always|Most of the time|Sometimes|Rarely|Never|Don't know|Refused", message = "Income meets basic needs must be Always|Most of the time|Sometimes|Rarely|Never|Don't know|Refused")
        String incomeSufficiency,
        //@Pattern(regexp = "Very predictable|Somewhat predictable|Not predictable at all|Don't know|Refused", message = "Predictability of business income must be Very predictable|Somewhat predictable|Not predictable at all|Don't know|Refused")
        String incomePredictability,
        //@Pattern(regexp = "Not at all secure|Slightly secure|Moderately secure|Secure|Very secure|Don't know|Refused", message = "Financial security from work must be Not at all secure|Slightly secure|Moderately secure|Secure|Very secure|Don't know|Refused")
        String financialSecurity,
        //@AllowedDataSet(
        //        allowed = { "Business or farmer association", "Women's self-help group", "NGO", "Religious group", "Political party", "Village development committee", "Local governance committee", "Other village committee", "Cooperative", "Other [specify]", "None of the above", "DON'T KNOW", "REFUSED" },
        //        message = "Membership in community groups must be a comma-separated list of Business or farmer association|Women's self-help group|NGO|Religious group|Political party|Village development committee|Local governance committee|Other village committee|Cooperative|Other [specify]|None of the above|DON'T KNOW|REFUSED"
        //)
        String communityGroups,
        //@Pattern(regexp = "Yes|No|DON'T KNOW|REFUSED", message = "Leadership position in groups must be Yes|No|DON'T KNOW|REFUSED")
        String leadershipRole,
        //@Pattern(regexp = "Yes|No|DON'T KNOW|REFUSED", message = "Confidence in household decision-making must be Yes|No|DON'T KNOW|REFUSED")
        String decisionMakingConfidence,
        //@Pattern(regexp = "Yes- very easily|Yes- fairly easily|Yes- but with a little difficulty|Yes- but with a great deal of difficulty|No- not at all|DON'T KNOW|REFUSED", message = "Ability to change community must be Yes- very easily|Yes- fairly easily|Yes- but with a little difficulty|Yes- but with a great deal of difficulty|No- not at all|DON'T KNOW|REFUSED")
        String communityChange,
        String communityIssues,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with access to education must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionEducation,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with relationships must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionRelationships,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with business type must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionBusinessType,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with earnings/income must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionIncome,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with housing must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionHousing,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with healthcare access must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionHealthcare,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with clean water access must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionWater,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with food must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionFood,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with nutrition must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionNutrition,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with life as a whole must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionLife,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with access to information must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionInformation,
        //@Pattern(regexp = "Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED", message = "Satisfaction with leisure activities must be Very Dissatisfied|Dissatisfied|Neutral|Satisfied|Very Satisfied|DON'T KNOW|REFUSED")
        String satisfactionLeisure,
        //@AllowedDataSet(
        //        allowed = { "Technical Assistance Training", "Financial Access", "Market Access", "Other [specify]", "DON'T KNOW","REFUSED" },
        //        message = "Jiinue interventions received must be a comma-separated list of Technical Assistance Training|Financial Access|Market Access|Other [specify]|DON'T KNOW|REFUSED"
        //)
        String jgpInterventions,
        //@AllowedDataSet(
        //        allowed = { "Buying & procurement", "Staff training", "Business digitization", "Record keeping", "Budgeting", "Sales & Marketing", "Pre-lending", "Post-lending", "Specialized training", "Mentorship & coaching", "Taxation", "Digitization/Digitalization", "Other [specify]", "DON'T KNOW", "REFUSED" },
        //        message = "Technical training sessions attended must be a comma-separated list of Buying & procurement|Staff training|Business digitization|Record keeping|Budgeting|Sales & Marketing|Pre-lending|Post-lending|Specialized training|Mentorship & coaching|Taxation|Digitization/Digitalization|Other [specify]|DON'T KNOW|REFUSED"
        //)
        String technicalTraining,
        //@AllowedDataSet(
        //        allowed = { "Buying & procurement", "Staff training", "Business digitization", "Record keeping", "Budgeting", "Sales & Marketing", "Pre-lending", "Post-lending", "Specialized training", "Mentorship & coaching", "Taxation", "Digitization/Digitalization", "Other [specify]", "DON'T KNOW", "REFUSED" },
        //        message = "New business practices implemented must be a comma-separated list of Buying & procurement|Staff training|Business digitization|Record keeping|Budgeting|Sales & Marketing|Pre-lending|Post-lending|Specialized training|Mentorship & coaching|Taxation|Digitization/Digitalization|Other [specify]|DON'T KNOW|REFUSED"
        //)
        String newPractices,
        //@AllowedDataSet(
        //        allowed = { "Buying & procurement", "Staff training", "Business digitization", "Record keeping", "Budgeting", "Sales & Marketing", "Pre-lending", "Post-lending", "Specialized training", "Mentorship & coaching", "Taxation", "Digitization/Digitalization", "Other [specify]", "DON'T KNOW", "REFUSED" },
        //        message = "Improved business practices must be a comma-separated list of Buying & procurement|Staff training|Business digitization|Record keeping|Budgeting|Sales & Marketing|Pre-lending|Post-lending|Specialized training|Mentorship & coaching|Taxation|Digitization/Digitalization|Other [specify]|DON'T KNOW|REFUSED"
        //)
        String improvedPractices,
        String trainingImprovements,
        //@Pattern(regexp = "Employed more regular employees|Employed more casual employees [new work]|Change in revenue|Opened additional business [improved work]|Remained afloat [sustained work]|Other [specify]|None [has not observed any changes]|DON'T KNOW|REFUSED", message = "Observed business changes must be Employed more regular employees|Employed more casual employees [new work]|Change in revenue|Opened additional business [improved work]|Remained afloat [sustained work]|Other [specify]|None [has not observed any changes]|DON'T KNOW|REFUSED")
        String businessChanges,
        //@Pattern(regexp = "Significant increase|Moderate increase|No significant change|Decrease|DON'T KNOW|REFUSED", message = "Sustained profitability and income growth must be Significant increase|Moderate increase|No significant change|Decrease|DON'T KNOW|REFUSED")
        String profitabilityGrowth,
        BigDecimal revenueChange,
        //@Pattern(regexp = "Yes|No|DON'T KNOW|REFUSED", message = "Applied for JGP loan must be Yes|No|DON'T KNOW|REFUSED")
        String loanApplication,
        Integer numberOfLoans,
        //@Pattern(regexp = "USSD|Lending system|Physical visit|Other [specify]|DON'T KNOW|REFUSED", message = "Platform used for loan application must be USSD|Lending system|Physical visit|Other [specify]|DON'T KNOW|REFUSED")
        String loanPlatform,
        //@Pattern(regexp = "Yes|No|REFUSED", message = "Accessed financing outside JGP must be Yes|No|REFUSED")
        String externalFinancing,
        //@AllowedDataSet(
        //        allowed = { "Bank", "SACCO", "MFIs", "Family & friends", "Informal group", "Govt programs", "Digital & mobile loans", "Other [specify]", "DON'T KNOW", "REFUSED" },
        //        message = "Sources of external financing must be a comma-separated list of Bank|SACCO|MFIs|Family & friends|Informal group|Govt programs|Digital & mobile loans|Other [specify]|DON'T KNOW|REFUSED"
        //)
        String financingSources,
        //@Pattern(regexp = "Not at all|To a very small extent|To a small extent|To a moderate extent|To a great extent|DON'T KNOW|REFUSED", message = "Role of JGP in business changes must be Not at all|To a very small extent|To a small extent|To a moderate extent|To a great extent|DON'T KNOW|REFUSED")
        String jgpImpact,
        //@Pattern(regexp = "Yes|No|DON'T KNOW|REFUSED", message = "Changes without JGP must be Yes|No|DON'T KNOW|REFUSED")
        String changesWithoutJgp,
        //@AllowedDataSet(
        //        allowed = { "PO finance", "Marketing & sales training", "New business opportunities", "Isoko", "Drop shipping", "Digital catalog", "Other [specify]", "DON'T KNOW", "REFUSED" },
        //        message = "Market access interventions received must be a comma-separated list of PO finance|Marketing & sales training|New business opportunities|Isoko|Drop shipping|Digital catalog|Other [specify]|DON'T KNOW|REFUSED"
        //)
        String marketAccess,
        //@AllowedDataSet(
        //        allowed = { "New clients/customers", "New suppliers", "Purchase agreements", "Business relationships", "Other [specify]", "None of the above", "DON'T KNOW", "REFUSED" },
        //        message = "New business opportunities must be a comma-separated list of New clients/customers|New suppliers|Purchase agreements|Business relationships|Other [specify]|None of the above|DON'T KNOW|REFUSED"
        //)
        String businessOpportunities,
        String marketChallenges
) {}
