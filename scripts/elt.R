# Data preparation; save results to 'data/clean_data.RData'

library(tidyverse)
library(usmap)

# ANES 2020 Time Series Study ####
# The largest part of this prep is recoding the numeric data to human-intelligible data
Raw2022 <- read_csv('data/2022/anes_timeseries_2020_csv_20220210.csv', col_types = c(
	version = 'c',
	.default = 'd'
	)) %>%
	filter(V200008 != 6)  # filter out incorrect respondents from validation

Data <-
	Raw2022 %>%
		select(
			weight = V200010b,
			state = V201014b,
			'HOW OFTEN DOES R PAY ATTENTION TO POLITICS AND ELECTIONS' = V201005,
			'PARTY OF REGISTRATION' = V201018,
			'FEELING THERMOMETER: DEMOCRATIC PARTY' = V201156,
			'FEELING THERMOMETER: REPUBLICAN PARTY' = V201157,
			'7pt scale: liberal - conservative' = V201200,
			'7pt scale: Government should provide many fewer services - Government should provide many more services' = V201246,
			'7pt scale: Greatly decrease defense spending - Greatly increase defense spending' = V201249,
			'7pt scale: Government insurance plan - Private insurance plan' = V201252,
			'7pt scale: Government should see to jobs and standard of living - Government should let each person get ahead on own' = V201255,
			'7pt scale: Government should help blacks - Blacks should help themselves' = V201258,
			'7pt scale: Tougher regulations on business needed to protect environment - Regulations to protect environment already too much a burden on business' = V201262,
			'PRE: STD ABORTION: SELF-PLACEMENT' = V201336,
			'SERVICES TO SAME SEX COUPLES' = V201406,
			'TRANSGENDER POLICY' = V201409,
			'FAVOR LAWS TO PROTECT GAYS AND LESBIANS AGAINST JOB DISCRIMINATION' = V201412,
			'US GOVERNMENT POLICY TOWARD UNAUTHORIZED IMMIGRANTS' = V201417
			) %>%
		mutate(
			state = factor(state, ordered = F),
			state = fct_recode(state,
				NULL = '-9',
				NULL = '-8',
				NULL = '86',
				NULL = '-1',
				
				'Alabama' = '1',
				'Alaska' = '2',
				'Arizona' = '4',
				'Arkansas' = '5',
				'California' = '6',
				'Colorado' = '8',
				'Connecticut' = '9', 
				'Delaware' = '10',
				'District of Columbia' = '11',
				'Florida' = '12',
				'Georgia' = '13',
				'Hawaii' = '15',
				'Idaho' = '16',
				'Illinois' = '17',
				'Indiana' = '18',
				'Iowa' = '19',
				'Kansas' = '20',
				'Kentucky' = '21',
				'Louisiana' = '22',
				'Maine' = '23',
				'Maryland' = '24',
				'Massachusetts' = '25',
				'Michigan' = '26',
				'Minnesota' = '27',
				'Mississippi' = '28',
				'Missouri' = '29',
				'Montana' = '30',
				'Nebraska' = '31',
				'Nevada' = '32',
				'New Hampshire' = '33',
				'New Jersey' = '34',
				'New Mexico' = '35',
				'New York' = '36',
				'North Carolina' = '37',
				'North Dakota' = '38',
				'Ohio' = '39',
				'Oklahoma' = '40',
				'Oregon' = '41',
				'Pennsylvania' = '42',
				'Rhode Island' = '44',
				'South Carolina' = '45',
				'South Dakota' = '46',
				'Tennessee' = '47',
				'Texas' = '48',
				'Utah' = '49',
				'Vermont' = '50',
				'Virginia' = '51',
				'Washington' = '53',
				'West Virginia' = '54',
				'Wisconsin' = '55',
				'Wyoming' = '56'),
			#
			`HOW OFTEN DOES R PAY ATTENTION TO POLITICS AND ELECTIONS` = 
			factor(`HOW OFTEN DOES R PAY ATTENTION TO POLITICS AND ELECTIONS`, 
								levels = seq(1, 5), ordered = T),
			`HOW OFTEN DOES R PAY ATTENTION TO POLITICS AND ELECTIONS` = 
				fct_recode(`HOW OFTEN DOES R PAY ATTENTION TO POLITICS AND ELECTIONS`, 
					'Always' = '1',
					'Most of the time' = '2',
					'About half of the time' = '3',
					'Some of the time' = '4',
					'Never' = '5'
				),
			
			`PARTY OF REGISTRATION` = factor(`PARTY OF REGISTRATION`, 
																			 ordered = F, levels = c(1, 2, 4, 5)),
			
			`PARTY OF REGISTRATION` = fct_recode(`PARTY OF REGISTRATION`, 
				'Democratic party' = '1',
				'Republican party' = '2',
				"'None or 'independent'" = '4',
				'Other' = '5'
				),
			
			`FEELING THERMOMETER: DEMOCRATIC PARTY` = case_when(
				between(`FEELING THERMOMETER: DEMOCRATIC PARTY`, 0, 100) ~  `FEELING THERMOMETER: DEMOCRATIC PARTY`,
				T ~ NA_real_),
			
			`FEELING THERMOMETER: REPUBLICAN PARTY` = case_when(
				between(`FEELING THERMOMETER: REPUBLICAN PARTY`, 0, 100) ~  `FEELING THERMOMETER: REPUBLICAN PARTY`,
				T ~ NA_real_),
			
				`7pt scale: liberal - conservative` = factor(
					`7pt scale: liberal - conservative`, levels = seq(1, 7)
					),
				`7pt scale: liberal - conservative` = fct_recode(
					`7pt scale: liberal - conservative`,
					'Extremely liberal' = '1',
					'Liberal' = '2',
					'Slightly liberal' = '3', 
					'Moderate' = '4', 
					'Slightly conservative' = '5',
					'Conservative' = '6',
					'Extremely conservative' = '7'
			),
			
			`7pt scale: Government should provide many fewer services - Government should provide many more services` = factor(
				`7pt scale: Government should provide many fewer services - Government should provide many more services`, levels = seq(1, 7)),
			`7pt scale: Government should provide many fewer services - Government should provide many more services` = fct_recode(
				`7pt scale: Government should provide many fewer services - Government should provide many more services`,
				'Government should provide many fewer services' = '1',
				'Government should provide many more services' = '7'
			),
			
			`7pt scale: Greatly decrease defense spending - Greatly increase defense spending` = factor(
				`7pt scale: Greatly decrease defense spending - Greatly increase defense spending`, levels = seq(1, 7)),
			`7pt scale: Greatly decrease defense spending - Greatly increase defense spending` = fct_recode(
				`7pt scale: Greatly decrease defense spending - Greatly increase defense spending`, 
				'Greatly decrease defense spending' = '1',
				'Greatly increase defense spending' = '7'),
			
			`7pt scale: Government insurance plan - Private insurance plan` = factor(
				`7pt scale: Government insurance plan - Private insurance plan`, 
				levels = seq(1, 7)),
			`7pt scale: Government insurance plan - Private insurance plan` = fct_recode(
				`7pt scale: Government insurance plan - Private insurance plan`,
				'Government insurance plan' = '1',
				'Private insurance plan' = '7'),
			
			`7pt scale: Government should see to jobs and standard of living - Government should let each person get ahead on own` = factor(
				`7pt scale: Government should see to jobs and standard of living - Government should let each person get ahead on own`, 
				levels = seq(1, 7)),
			`7pt scale: Government should see to jobs and standard of living - Government should let each person get ahead on own` = fct_recode(
				`7pt scale: Government should see to jobs and standard of living - Government should let each person get ahead on own`,
				'Government should see to jobs and standard of living' = '1',
				'Government should let each person get ahead on own' = '7'),
		
			`7pt scale: Government should help blacks - Blacks should help themselves` = factor(
				`7pt scale: Government should help blacks - Blacks should help themselves`,
				levels = seq(1, 7)),
			`7pt scale: Government should help blacks - Blacks should help themselves` = fct_recode(
				`7pt scale: Government should help blacks - Blacks should help themselves`,
				'Government should help blacks' = '1',
				'Blacks should help themselves' = '7'),
			
			`7pt scale: Tougher regulations on business needed to protect environment - Regulations to protect environment already too much a burden on business` = factor(
				`7pt scale: Tougher regulations on business needed to protect environment - Regulations to protect environment already too much a burden on business`, 
				levels = seq(1, 7)),
			`7pt scale: Tougher regulations on business needed to protect environment - Regulations to protect environment already too much a burden on business` = fct_recode(
				`7pt scale: Tougher regulations on business needed to protect environment - Regulations to protect environment already too much a burden on business`,
				'Tougher regulations on business needed to protect environment' = '1',
				'Regulations to protect environment already too much a burden on business' = '7'
			),
			
			`PRE: STD ABORTION: SELF-PLACEMENT` = factor(
				`PRE: STD ABORTION: SELF-PLACEMENT`,
				levels = seq(1, 4)),
			`PRE: STD ABORTION: SELF-PLACEMENT` = fct_recode(
				`PRE: STD ABORTION: SELF-PLACEMENT`,
				'By law, abortion should never be permitted' = '1',
				'The law should permit abortion only in case of rape, incest, or when the woman’s life is in danger' = '2',
				'The law should permit abortion other than for rape/incest/danger to woman but only after need clearly established' = '3',
				'By law, a woman should always be able to obtain an abortion as a matter of personal choice' = '4'
			),
			
			`SERVICES TO SAME SEX COUPLES` = factor(
				`SERVICES TO SAME SEX COUPLES`, 
				 levels = c(1, 2)),
			`SERVICES TO SAME SEX COUPLES` = fct_recode(
				`SERVICES TO SAME SEX COUPLES`,
					'Should be allowed to refuse' = '1',
					'Should be required to provide services' = '2'),
			
			`TRANSGENDER POLICY` = factor(`TRANSGENDER POLICY`, levels = c(1, 2)),
			`TRANSGENDER POLICY` = fct_recode(`TRANSGENDER POLICY`, 
				'Have to use the bathrooms of the gender they were born as' = '1',
				'Be allowed to use the bathrooms of their identified gender' = '2'),
			
			`FAVOR LAWS TO PROTECT GAYS AND LESBIANS AGAINST JOB DISCRIMINATION` = factor(`FAVOR LAWS TO PROTECT GAYS AND LESBIANS AGAINST JOB DISCRIMINATION`,
				levels = c(1, 2)),
			`FAVOR LAWS TO PROTECT GAYS AND LESBIANS AGAINST JOB DISCRIMINATION` = fct_recode(`FAVOR LAWS TO PROTECT GAYS AND LESBIANS AGAINST JOB DISCRIMINATION`,
				'Favor' = '1', 
				'Oppose' = '2'),
			
			`US GOVERNMENT POLICY TOWARD UNAUTHORIZED IMMIGRANTS`	= factor(`US GOVERNMENT POLICY TOWARD UNAUTHORIZED IMMIGRANTS`, 
				levels = seq(1, 4)),
			`US GOVERNMENT POLICY TOWARD UNAUTHORIZED IMMIGRANTS`	= fct_recode(`US GOVERNMENT POLICY TOWARD UNAUTHORIZED IMMIGRANTS`, 
				'Make all unauthorized immigrants felons and send them back to their home country' = '1',
				'Have a guest worker program that allows unauthorized immigrants to remain in US to work but only for limited time' = '2',
				'Allow unauthorized immigrants to remain in US & eventually qualify for citizenship but only if they meet requirements' = '3',
				'Allow unauthorized immigrants to remain in US & eventually qualify for citizenship without penalties' = '4'),
		)
####


# USA map data ####
# Load the 'usmap' package's table which describes the map of the USA
USAMap <- 
	usmap::us_map(regions = "states") %>%
		as_tibble %>%
		rename(state = full)
####


# State governments ####
# Use Wikipedia data to see which governments have state legislatures 
# and governors of the same party
# https://en.wikipedia.org/wiki/List_of_United_States_state_legislatures 
# retrieved May 9 2022
RawStateGovs <- read_csv('data/us_state_govs.csv', col_types = cols(.default = 'c'))

IntermediateStateGovs <-
	RawStateGovs %>%
		select(state = State, governor = gov, lower_house = lower, upper_house = upper) %>%
		gather(variable, value, -state) %>%
		mutate(value = str_to_lower(value),
					 value = str_replace_all(value,
					 												c('r' = 'Republican party',
					 													'd' = 'Democratic party'))
					 )

UnifiedStateGovs <-
	IntermediateStateGovs %>%
		count(state, value) %>%
		filter(n == 3) %>%
		select(state) %>%
		mutate(is_state_gov_unified = TRUE)

StateGovs <-
	IntermediateStateGovs %>%
		filter(variable == 'governor') %>%
		select(state, value) %>%
		left_join(UnifiedStateGovs, by = 'state') %>%
		mutate(
			is_state_gov_unified = replace_na(is_state_gov_unified, FALSE),
			party = if_else(is_state_gov_unified, value, NA_character_)
		) %>%
		select(state, is_state_gov_unified, party)
####

save(Data, StateGovs, USAMap, file = 'data/clean_data.RData')