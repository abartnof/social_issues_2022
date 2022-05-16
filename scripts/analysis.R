#library(MASS)
#library(lavaan)
library(conflicted)
library(likert)
library(psych)
library(skimr)
library(tidyverse)
library(usmap)
conflicted::conflict_prefer('select', 'dplyr')
conflicted::conflict_prefer('filter', 'dplyr')

load(file = 'data/clean_data.RData')

# Overall stats 
StatewisePartyProp <-
	Data %>%
		select(state, party = 'PARTY OF REGISTRATION', weight) %>%
		filter(party %in% c('Democratic party', 'Republican party')) %>%
		drop_na %>%
		group_by(state, party) %>%
		summarize(subtotal = sum(weight)) %>%
		ungroup %>%
		group_by(state) %>%
		mutate(prop = subtotal / sum(subtotal)) %>%
		ungroup %>%
		mutate(
			binned_prop = cut_width(prop, width = 0.25, boundary = 0, ordered_result = TRUE)) %>%
		filter(party == 'Democratic party') %>%
		select(state, prop_democratic_party = prop, binned_prop)

USAMap %>%
	left_join(StatewisePartyProp, by = 'state') %>%
	ggplot(aes(x = x, y = y, fill = binned_prop, group = group)) +
	geom_polygon() +
	scale_fill_manual(na.value = 'grey', values = c('darkred', 'pink', 'lightblue', 'darkblue'),
										breaks = levels(StatewisePartyProp$binned_prop),
										labels = c('0% - 25%', '25% - 50%', '50% - 75%', '75% - 100%')) +
	theme_void() +
	theme(legend.position = 'bottom') +
	labs(fill = 'Proportion', title = 'Proportion of Democrats per voter registered as either a Democratic or Republican')

# Compare to unified state govs

USAMap %>% 
	left_join(StateGovs, by = 'state') %>%
	mutate(
		party = if_else(is_state_gov_unified, party, '(Mixed)'),
		party = ordered(party, levels = c('Democratic party', 'Republican party', '(Mixed)'))) %>%
	ggplot(aes(x = x, y = y, fill = party, group = group)) +
	geom_polygon() +
	scale_fill_manual(values = c('blue', 'red', 'purple'),
										breaks = c('Democratic party', 'Republican party', '(Mixed)')) +
	theme_void() +
	theme(legend.position = 'bottom') +
	labs(title = 'States with unified local governments', fill = '')

# sanity check- party registration v thermometers
Data %>%
	select(
		weight,
		#attention = 'HOW OFTEN DOES R PAY ATTENTION TO POLITICS AND ELECTIONS',
		party = 'PARTY OF REGISTRATION',
		'Democratic Party' = 'FEELING THERMOMETER: DEMOCRATIC PARTY',
		'Republican Party' = 'FEELING THERMOMETER: REPUBLICAN PARTY'
	) %>%
	drop_na %>%
	gather(target, response, -party, -weight) %>%
	group_by(party, target) %>%
	summarize(
		weighted_mean = weighted.mean(x = response, w = weight) - 50) %>%
	ungroup %>%
	ggplot(aes(x = party, fill = target, group = target, y = weighted_mean)) +
	geom_hline(yintercept = 0, linetype = 'dashed') +
	geom_col(position = position_dodge())  +
	coord_flip(ylim = c(-50, 50)) +
	scale_y_continuous(breaks = seq(-50, 50, by = 50), 
										 labels = c('0\nNegative', '50\nNeutral', '100\nPositive')) +
	scale_x_discrete(limits = rev) +
	scale_fill_manual(values = c('darkblue', 'darkred')) +
	theme_light() +
	theme(panel.grid.major.y = element_blank(),
				legend.position = 'bottom') +
	labs(x = 'Respondent\'s affiliation', y = 'Favorability', fill = '')
#	
# Is there a relationship between paying attention to politics and party of reg?
ModelMe <-
	Data %>%
		select(
			attention = 'HOW OFTEN DOES R PAY ATTENTION TO POLITICS AND ELECTIONS',
			party = 'PARTY OF REGISTRATION',
			weight
		) %>%
	drop_na %>%
	mutate(
		party = fct_rev(party),
		weight = round(weight * 100),
		attention = reverse.levels(attention)) %>%
	uncount(weight) %>%
	rename('How often do you pay attention to what’s going on in government and politics?' = attention)

Items <- as.data.frame(ModelMe['How often do you pay attention to what’s going on in government and politics?'])
gg <- ModelMe$party
plot(likert(items = Items, grouping = gg))
#

# How well do the thermometer questions hang together on different numbers of latent variables
## 1

WeightAndAbstractItems <-
	Data %>%
		select(
			weight,
			starts_with('7pt')
		) %>%
		drop_na(weight)

# I'm not sure how to weight using fa.parallel, so replicate rows in terms of weight
ModelMe <-
	WeightAndAbstractItems %>%
		mutate(weight = round(weight * 100)) %>%
		uncount(weight) %>%
		mutate_all(as.integer)

Cor <-
	ModelMe %>%
		cor(method = 's', use = 'pairwise.complete.obs')
#
# four factors is ideal
fa.parallel(Cor, n.obs = nrow(ModelMe), fa = 'fa')

# fit FA with one component
fa_fit1 <- fa(Cor, nfactors = 1, n.obs = nrow(Cor))
fa_fit1$loadings

# fit FA with four components
fa_fit4 <- fa(Cor, nfactors = 4, n.obs = nrow(Cor), rotate = 'promax')

fa_fit4$loadings %>%
	unclass %>%
	as.data.frame %>%
	rownames_to_column(var = 'manifest_var') %>%
	gather(latent_var, loading, -manifest_var) %>%
	mutate(loading = round(loading, 1)) %>%
	group_by(manifest_var) %>%
	mutate(is_primary_latent_var = abs(loading) == max(abs(loading))) %>%
	ungroup %>%
	ggplot(aes(x = latent_var, y = manifest_var, fill = is_primary_latent_var, label = loading)) +
	geom_tile() +
	geom_text() +
	scale_fill_manual(values = c('white', 'lightblue')) +
	scale_y_discrete(limits = rev, labels = label_wrap_gen(width = 30)) +
	theme_minimal() +
	theme(axis.ticks = element_blank(),
				panel.grid = element_blank(),
				text = element_text(size = 7),
				legend.position = 'none') +
	labs(fill = '', 
			 x = 'Latent variable', 
			 y = 'Manifest variable')
#
anova(fa_fit1, fa_fit4)  # best to stick with 1 factor


# Look at the culture issues, overall and broken out by the four party affiliations ####

## Abortion
Items <-
	Data %>%
		select(weight, Abortion = 'PRE: STD ABORTION: SELF-PLACEMENT') %>%
		drop_na %>%
		mutate(weight = round(weight * 100)) %>%
		uncount(weight) %>%
		as.data.frame
plot(likert(items = Items)) +
	guides(fill = guide_legend(nrow = 2))

ModelMe <-
	Data %>%
		select(party = 'PARTY OF REGISTRATION', weight, Abortion = 'PRE: STD ABORTION: SELF-PLACEMENT') %>%
		drop_na %>%
		mutate(weight = round(weight * 100)) %>%
		uncount(weight)
Items <- as.data.frame(ModelMe['Abortion'])
gg <- ModelMe$party
plot(likert(items = Items, grouping = gg)) +
	scale_x_discrete(limits = rev) +
	guides(fill = guide_legend(nrow = 2))

## Services to same-sex couples
Items <-
	Data %>%
		select(weight, 'Services to same-sex couples' = 'SERVICES TO SAME SEX COUPLES') %>%
		drop_na %>%
		mutate(
			weight = round(weight * 100)) %>%
		uncount(weight) %>%
		as.data.frame
plot(likert(items = Items)) +
	guides(fill = guide_legend(nrow = 1))

ModelMe <-
	Data %>%
		select(party = 'PARTY OF REGISTRATION', weight, 'Services to same-sex couples' = 'SERVICES TO SAME SEX COUPLES') %>%
		drop_na %>%
		mutate(
			weight = round(weight * 100)) %>%
		uncount(weight)
Items <- as.data.frame(ModelMe['Services to same-sex couples'])
gg <- ModelMe$party
plot(likert(items = Items, grouping = gg)) +
	scale_x_discrete(limits = rev) +
	guides(fill = guide_legend(nrow = 1))

## Transgender policy
Items <-
	Data %>%
		select(weight, 'Transgender policy' = 'TRANSGENDER POLICY') %>%
		drop_na %>%
		mutate(weight = round(weight * 100)) %>%
		uncount(weight) %>%
		as.data.frame
plot(likert(items = Items)) +
	guides(fill = guide_legend(nrow = 1))

ModelMe <-
	Data %>%
		select(party = 'PARTY OF REGISTRATION', weight, 'Transgender policy' = 'TRANSGENDER POLICY') %>%
		drop_na %>%
		mutate(
			weight = round(weight * 100)) %>%
		uncount(weight)
Items <- as.data.frame(ModelMe['Transgender policy'])
gg <- ModelMe$party
plot(likert(items = Items, grouping = gg)) +
	scale_x_discrete(limits = rev) +
	guides(fill = guide_legend(nrow = 1))

# Laws to protect gays and lesbians against job descrimination
Items <-
	Data %>%
		select(weight, 'Laws to protect gays and lesbians against job discrimination' = 'FAVOR LAWS TO PROTECT GAYS AND LESBIANS AGAINST JOB DISCRIMINATION') %>%
		drop_na %>%
		mutate(`Laws to protect gays and lesbians against job discrimination` = reverse.levels(`Laws to protect gays and lesbians against job discrimination`),
			weight = round(weight * 100)) %>%
		uncount(weight) %>%
		as.data.frame
plot(likert(items = Items)) +
	guides(fill = guide_legend(nrow = 1))

ModelMe <-
	Data %>%
		select(party = 'PARTY OF REGISTRATION', weight, 'Laws to protect gays and lesbians against job discrimination' = 'FAVOR LAWS TO PROTECT GAYS AND LESBIANS AGAINST JOB DISCRIMINATION') %>%
		drop_na %>%
		mutate(`Laws to protect gays and lesbians against job discrimination` = reverse.levels(`Laws to protect gays and lesbians against job discrimination`),
			weight = round(weight * 100)) %>%
		uncount(weight)
Items <- as.data.frame(ModelMe['Laws to protect gays and lesbians against job discrimination'])
gg <- ModelMe$party
plot(likert(items = Items, grouping = gg)) +
	scale_x_discrete(limits = rev) +
	guides(fill = guide_legend(nrow = 1))

# Unauthorized immigrants
Items <-
	Data %>%
		select(weight, 'Policy toward unauthorized immigrants' = 'US GOVERNMENT POLICY TOWARD UNAUTHORIZED IMMIGRANTS') %>%
		drop_na %>%
		mutate(
			weight = round(weight * 100)) %>%
		uncount(weight) %>%
		as.data.frame
plot(likert(items = Items)) +
	guides(fill = guide_legend(nrow = 1))

ModelMe <-
	Data %>%
		select(party = 'PARTY OF REGISTRATION', weight, 'Policy toward unauthorized immigrants' = 'US GOVERNMENT POLICY TOWARD UNAUTHORIZED IMMIGRANTS') %>%
		drop_na %>%
		mutate(
			weight = round(weight * 100)) %>%
		uncount(weight)
Items <- as.data.frame(ModelMe['Policy toward unauthorized immigrants'])
gg <- ModelMe$party
plot(likert(items = Items, grouping = gg)) +
	scale_x_discrete(limits = rev) +
	guides(fill = guide_legend(nrow = 1))
#

# Compare abortion feelings per state

Data %>%
	select(
		weight,
		state,
		abortion = contains('ABORTION')) %>%
	drop_na %>%
	mutate(
		no_abortion = abortion == 'By law, abortion should never be permitted',
		weight = round(weight * 100)) %>%
	uncount(weight) %>%
	group_by(state) %>%
	summarize(prop_no_abortion = mean(no_abortion)) %>%
	ungroup %>%
	mutate(binned_no_abortion = cut_width(prop_no_abortion, width = 0.1, boundary = 0, ordered_levels = T)) %>%
	right_join(USAMap, by = 'state') %>%
	ggplot(aes(x = x, y = y, group = group, fill = binned_no_abortion)) +
	geom_polygon(color = 'black') +
	scale_fill_viridis_d(
										breaks = c('[0,0.1]', '(0.1,0.2]', '(0.2,0.3]', '(0.4,0.5]'),
										labels = c('0% - 10%', '10% - 20%', '20% - 30%', '40% - 50%'), 
										na.value = NA) +
	theme_void() +
	labs(fill = '', title = 'Respondents who indicated that by law, abortion should never be permitted')

# Which states have the biggest disagreement between republican voters and the rest wrt abortion	

national_weighted_mean_no_abortion <-
	Data %>%
		select(
			weight,
			abortion = contains('ABORTION')) %>%
		mutate(no_abortion = abortion == 'By law, abortion should never be permitted') %>%
		drop_na(no_abortion, weight) %>%
		with(., weighted.mean(x = no_abortion, w = weight))


TempTableStateGovsSimplified <-
	StateGovs %>%
		mutate(
			party = if_else(is_state_gov_unified, party, '(Mixed)'),
			party = ordered(party, c('Democratic party', 'Republican party', '(Mixed)'))) %>%
		select(state, state_gov_party = party)

Data %>%
	select(
		party = 'PARTY OF REGISTRATION',
		weight,
		state,
		abortion = contains('ABORTION')) %>%
	drop_na %>%
	mutate(
		no_abortion = abortion == 'By law, abortion should never be permitted',
		party2 = if_else(party == 'Republican party', 'Republican', 'Non-republican')
	) %>%
	select(party2, state, weight, no_abortion) %>%
	drop_na %>%
	group_by(state, party2) %>%
	summarize(weighted_mean = weighted.mean(x = no_abortion, w = weight)) %>%
	ungroup %>%
	spread(party2, weighted_mean) %>%
	mutate(
		difference = `Republican` - `Non-republican`,
	) %>%
	gather(party2, no_abortion, -state, -difference) %>%
	left_join(TempTableStateGovsSimplified, by = 'state') %>%
	drop_na(state_gov_party) %>%
	arrange(state_gov_party, difference) %>%
	mutate(state = fct_inorder(state),
				 my_sign = if_else(difference > 0, '+', ''),
				 difference_str = str_c(my_sign, scales::percent(difference, 2)),
				 difference_str = if_else(party2 == 'Republican', difference_str, NA_character_)
	) %>%
	ggplot(aes(x = state, y = no_abortion, fill = party2, group = party2, label = difference_str)) +
	geom_hline(yintercept = national_weighted_mean_no_abortion, linetype = 'dashed', color = 'grey') +
	geom_col(position = 'dodge') +
	geom_text(position = position_dodge(width = 0.9), hjust = 0) +
	facet_wrap(~state_gov_party, scales = 'free_y') +
	coord_flip(ylim = c(0, 1)) +
	scale_fill_manual(values = c('grey30', 'red')) +
	scale_y_continuous(labels = scales::percent_format(1), breaks = c(0, 0.5, 1)) +
	theme_classic() +
	theme(legend.position = 'bottom',
				axis.ticks = element_blank()
				 ) +
	labs(fill = 'Respondent\'s party',
			 x = '',
			 y = '',
			 title = 'Respondents who endorsed the idea that by law, abortion should never be permitted')
#