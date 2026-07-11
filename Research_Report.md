# Social Media Use and Affective Polarization Among Young Spanish Adults: A Panel Data Analysis

> This report is an English research summary of my Bachelor's Thesis.  
> The complete thesis, written in Spanish, is available in the `docs` folder.

## Abstract

This report examines whether informational social media use is associated with affective polarization (AP) among young Spanish adults (18–35), and whether that relationship is moderated by ideological position and sex. The analysis draws on the Spanish subsample of the TriPol panel survey (N = 1,289 at Wave 1; n = 300 young adults), collected across three waves between 2021 and 2023. Affective polarization is measured with the WAPD index (Weighted Affective Party Distance), which captures the net affective distance between a respondent's preferred party and the remaining parties in a multiparty system. The empirical strategy combines cross-sectional OLS regression, a Cross-Lagged Panel Design (CLPD) linking Wave 1 predictors to Wave 3 outcomes, interaction models, and a set of robustness checks (heteroscedasticity-consistent errors, non-linearity tests, attrition correction via inverse probability weighting, and a Chow test comparing the young subsample against an older reference group).

Young Spanish adults show clear, statistically significant, and substantively meaningful levels of affective polarization across all three waves, confirming the first hypothesis. Social media use, however, shows no robust association with AP once demographic and ideological controls are introduced — neither cross-sectionally, longitudinally, nor as a moderator of ideological extremity. Ideological extremism (distance from the centre of the left-right scale) emerges instead as the strongest and most consistent predictor, with its explanatory weight increasing between waves. A post-hoc power analysis indicates that the young subsample (N ≈ 126–172 depending on the model) is underpowered to detect small effects of social media use, so the null results for the social-media hypotheses should be read as inconclusive rather than as evidence of no effect. A supplementary model suggests the effect of social media use may be conditional on perceived party-system polarization rather than direct. No statistically robust evidence supports a differentiated effect for young adults relative to older respondents.

The findings suggest that, in this sample, ideological extremity — not exposure to social media — is the more relevant lever for understanding affective polarization among young adults, a result that qualifies part of the public debate around echo chambers and filter bubbles.

---

## 1. Introduction

Affective polarization — the tendency of citizens to feel warmly toward their own political group and coldly toward rival groups, independent of the actual distance between their policy positions — has become one of the most discussed constructs in contemporary political behaviour research. Originally documented in the two-party context of the United States, it has since been shown to operate in European multiparty systems as well, including Spain, where party-system fragmentation over the last decade has raised questions about how citizens emotionally relate to political rivals.

A parallel debate concerns the role of social media as the dominant channel through which many citizens now consume political information. Two competing narratives dominate this discussion: one holds that algorithmic curation and selective exposure push users into ideological echo chambers, intensifying hostility toward outgroups; the other argues that digital platforms expose users to more ideological diversity than is commonly assumed, and that any polarizing effect is conditional on individual usage patterns rather than a structural property of the platforms themselves.

Young adults are a natural population in which to study this question. They report the highest rates of social media use as a news source of any age group, and — per the "impressionable years" hypothesis — their political identities are still consolidating, which theoretically makes them more susceptible to environmental influences such as digital information exposure.

**Research question:** To what extent does informational social media use affect affective polarization among young Spanish adults, and is this relationship moderated by ideological position and sex?

**Objectives:** (1) establish whether young Spanish adults show positive levels of affective polarization; (2) test whether social media use predicts affective polarization, cross-sectionally and longitudinally; (3) test whether this relationship is moderated by ideological extremity; (4) compare the structure of determinants of affective polarization between young adults and an older reference group; and (5) assess whether social media use is a stronger predictor for young adults specifically.

---

## 2. Literature Background

**Affective polarization.** The concept emerged from the observation that rising hostility between partisans in the US did not correspond to a proportional increase in ideological distance at the mass level (Fiorina et al., 2004) — the growing conflict was affective, not (only) programmatic (Iyengar & Westwood, 2014). Social identity theory (Tajfel & Turner, 1979) offers the standard explanatory mechanism: group membership generates near-automatic favourable evaluations of the ingroup and unfavourable evaluations of the outgroup, and this dynamic intensifies when political identity aligns with other social identities (Mason, 2018). In Spain specifically, Torcal and colleagues (Torcal & Montero, 2006; Comellas & Torcal, 2023) developed the WAPD/TRI-POL framework used in this study, adapting the concept to multiparty competition, where affective evaluations are distributed across several relevant parties rather than a single binary opponent.

**Social media and political information.** The shift from centralized mass media to algorithmically curated digital platforms has reopened the question of how information environments shape political attitudes. The "fragmentation" thesis (Sunstein, 2001; Pariser, 2011; Stroud, 2011) argues that selective exposure and algorithmic personalization narrow the range of viewpoints citizens encounter, reinforcing echo chambers. The competing "diversity" thesis (Barberá, 2014; Prior, 2007) finds that ideological segregation on social platforms, while real, is smaller than commonly assumed, and that incidental exposure to cross-cutting content is more frequent than the echo-chamber narrative implies. A reasonable synthesis treats social media less as a uniform cause of polarization and more as an environment that reflects and amplifies pre-existing social and identity-based divisions (Boutyline & Willer, 2016) — meaning its effect, if any, is likely conditional rather than direct.

**Ideological extremism.** A distinct strand of research separates ideological *position* from ideological *extremity*. Van Erkel and Turkenburg (2022) show that in multiparty systems, hostility toward rival parties is driven by distance from the centre of the left-right axis, not by which side of the axis a person occupies. This distinction is theoretically important for the present study because it implies that left-right position and extremism can (and often do) behave very differently as predictors.

**Why young adults.** The "impressionable years" hypothesis (Krosnick & Alwin, 1989) holds that political attitudes are especially malleable between adolescence and the mid-twenties, and stabilize progressively afterward. Combined with young adults' comparatively high social media use, this motivates treating them as a distinct analytical group rather than assuming effects generalize uniformly across the adult population.

---

## 3. Data

The data come from the Spanish subsample of the **TriPol project**, a multi-country research programme studying the relationship between affective polarization, political distrust, and elite party competition in Spain, Portugal, Italy, Chile, and Argentina. This report uses only the Spanish panel survey component (TriPol also includes passive-metering and computer-assisted text analysis modules, which are outside the scope of this study).

The sample was recruited through a quota design on the NetQuest online panel, with quotas on sex, age, and education ensuring proportional representativeness of the Spanish population. **Wave 1 totals N = 1,289** respondents, surveyed across **three waves** (2021–2023). The sample is split into a **young-adult group (18–35, n = 300)** and an **older reference group (36+, n = 989)**, a split defined a priori on theoretical grounds (the impressionable-years hypothesis) rather than derived post hoc from the data. A structural limitation of the questionnaire is that the social-media-use item was not administered in Wave 2, which restricts the longitudinal analysis to the Wave 1 → Wave 3 interval.

**The dataset itself is not included in this repository.** It belongs to the TriPol research consortium and is subject to its own access and redistribution terms; researchers interested in replication should contact the TriPol project directly.

---

## 4. Variables

**Dependent variable — Affective Polarization (WAPD).** The WAPD (Weighted Affective Party Distance) index (Wagner, 2021) computes, for each respondent, a weighted affective distance toward all relevant parties, using vote intention to weight sympathy toward preferred parties and antipathy toward rivals. It ranges from 0 (no affective distance) to 10 (maximum polarization). The index used here is the average of its two precomputed components (WAPDV and WAPDL); convergent validity between the two components was confirmed (r = .70, .68, and .67 across the three waves), justifying the averaging.

**Independent variable — Social media use.** Measured with a single questionnaire item (frequency of using social media to get informed about current affairs), on an 8-point ordinal scale from "never" to "several times a day." This is a self-reported, general-purpose measure — it does not distinguish between platforms or content types, a limitation discussed further below. Available in Waves 1 and 3 only.

**Controls:**
- **Ideological position** — self-placement on a 0–10 left-right scale.
- **Ideological extremism** — absolute distance from the scale midpoint (5), independent of direction; the theoretically preferred operationalization for testing group-differentiation mechanisms.
- **Sex** — binary (male/female).
- **Education** — four ordered categories (none, primary, secondary, university).
- **Age** — continuous, included within each subgroup model.
- A secondary control, **perceived ideological polarization of the party system (WPIP)**, is introduced only in a robustness model.

---

## 5. Methodology

The analytical pipeline (fully reproducible in the accompanying R script) proceeds as follows:

1. **Preliminary checks.** Convergent validity of the WAPD components; a formal non-random attrition analysis comparing panel dropouts against those who remained (t-tests and chi-square tests); and a post-hoc statistical power analysis (Cohen's *f*² framework) given the modest size of the young subsample.

2. **Cross-sectional OLS regression**, estimated separately for young adults and the reference group, once for each wave:

   `AP_t = β₀ + β₁·SocialMedia_t + β₂·Ideology_t + β₃·Extremism_t + β₄·Age_t + β₅·Sex + β₆·Education + ε`

   Model diagnostics include VIF for multicollinearity, the Breusch-Pagan test for heteroscedasticity (with HC3 robust standard errors reported where warranted), and Shapiro-Wilk normality checks on residuals. Standardized versions of each model are also estimated to allow direct comparison of effect sizes across groups and waves.

3. **Cross-Lagged Panel Design (CLPD)**, regressing Wave-3 polarization on Wave-1 polarization, Wave-1 social media use, and Wave-1 controls. This is a lagged-dependent-variable design, not a fixed-effects panel model — it does not net out time-invariant unobserved heterogeneity, a limitation acknowledged explicitly (Hamaker et al., 2015). Where attrition is detected as non-random, an IPW-weighted version of the CLPD is estimated as a sensitivity check.

4. **Interaction models**, testing (a) whether the effect of social media use varies with ideological position/extremism, and (b) whether it varies between the young and older groups. Group-level equivalence is additionally tested with a formal **Chow test** comparing the full coefficient vectors of the young and reference-group models.

5. **Robustness analyses**: a model including perceived party-system polarization (WPIP) as an additional control; quadratic specifications to test for non-linear social-media effects; and the IPW-corrected CLPD described above.

6. A **descriptive multinomial logistic model** (age group as outcome) is used purely to characterize which attitudinal and demographic profiles distinguish age groups in the sample — it is explicitly non-causal and separate from the hypothesis-testing models above.

All analyses were conducted in R (`tidyverse`, `car`, `lmtest`, `sandwich`, `nnet`, `pwr`, `dunn.test`, `patchwork`), with α = .05 throughout and effect sizes reported alongside p-values using Cohen's (1988) conventions.

---

## 6. Results

**Affective polarization is real and substantial in this group (H1 — supported).** Mean WAPD among young adults is 4.65 (SD = 2.31) at Wave 1, rising to 5.27 at Wave 2 before settling at 4.91 at Wave 3 — all clearly above the neutral point of the scale (one-sample t-test against 0: t(178) = 27.00, p < .001). The Wave 1→2 increase is significant; the Wave 1→3 change is not, suggesting a transient spike rather than a sustained shift (Figure 1, Figure 2).

**Social media use is not a statistically robust predictor of affective polarization (H2 — not supported).** The bivariate correlation between social media use and AP is close to zero in both waves (r = .09, p = .223 at Wave 1; r = -.01, p = .932 at Wave 3). In the full OLS models, the coefficient is a marginal, non-significant trend at Wave 1 (β = 0.14, p = .079) and drops further at Wave 3 (β = 0.03, p = .732) (Figure 3). The CLPD, which relates Wave-1 social media use to Wave-3 polarization controlling for the Wave-1 baseline, likewise shows no effect (β = 0.07, p = .326), and this result is essentially unchanged after correcting for panel attrition via IPW (β = 0.07, p = .282) — indicating the null result is not an artefact of who dropped out of the panel.

**No moderation by ideology (H3 — not supported).** The interaction between social media use and ideological position, and separately between social media use and ideological extremism, is non-significant at both waves (all p > .18).

**Statistical power is the key qualifier for H2/H3.** With the available sample (N = 172 at Wave 1, N = 126 at Wave 3 for the young subgroup), post-hoc power to detect a *small* effect of social media use (f² = .02) is only .44 — well below the conventional .80 threshold; roughly 759 observations would be required. This means the null findings for H2 and H3 should be read as **inconclusive**, not as evidence that no effect exists.

**A conditional effect surfaces in the robustness check.** Adding perceived party-system polarization (WPIP) as a control shifts the Wave-1 social-media coefficient from a marginal β = 0.14 (p = .079) to a significant β = 0.18 (p = .024). This is consistent with the idea that social media use affects affective polarization indirectly, through its association with how polarized the party system is perceived to be, rather than as an independent, direct mechanism.

**Ideological extremism is the strongest and most consistent predictor (H4 — partially confirmed).** Distance from the centre of the left-right scale predicts AP with growing strength across waves: β = 0.47 (standardized β* = 0.74) at Wave 1, rising to β = 0.78 (β* = 1.24) at Wave 3 — the single largest standardized effect in either model. A one-way ANOVA confirms that individuals at the ideological extremes (left M = 5.06, right M = 5.19) show significantly higher AP than centrists (M = 4.15; F(2,176) = 3.78, p = .025), and a direct extremes-vs-centre comparison confirms the same pattern (t(169.55) = -2.73, p = .007) (Figure 4). Linear left-right position itself, by contrast, has no significant effect in either wave — it is distance from the centre that matters, not direction. Sex shows a significant effect at Wave 1 only, with young women reporting *lower* AP than young men (β = -0.86, p = .024; HC3-corrected p = .037) (Figure 5) — a direction that runs counter to most of the international literature, which typically finds the opposite. The sample's gender imbalance within the young group (213 women vs. 87 men) and possible wave-specific contextual effects are plausible partial explanations, but this result warrants dedicated follow-up rather than a confident interpretation.

**Young adults are not a distinctly different case (H5 — not supported).** Standardized social-media coefficients are actually somewhat *larger* in the older reference group at both waves (Wave 1: β* = 0.25, p = .006 for the reference group vs. β* = 0.34, p = .079 for young adults; Wave 3: β* = 0.17, p = .065 vs. β* = 0.07, p = .732) (Figures 6–8). None of the young-adult coefficients reach significance, but this reflects the reference group's much larger sample (N = 640 at Wave 1) rather than a genuinely different underlying relationship. The comparative coefficient plot (Figure 9) makes clear that the real point of divergence between groups is not social media use but ideological extremism, whose standardized effect is markedly larger among young adults by Wave 3 (β* = 1.24 vs. β* = 0.68 in the reference group).

---

## 7. Limitations

- **Secondary, project-defined data.** The analysis is constrained to the variables and wording available in the TriPol questionnaire.
- **Self-reported social media use.** The measure captures general frequency of use for informational purposes, not platform, content type, or actual exposure — it cannot distinguish between, say, encountering cross-cutting content and consuming ideologically homogeneous feeds.
- **No passive-metering data.** TriPol's *passive meter* component, which records real digital behaviour, was outside the scope of this analysis; a behavioural measure would likely be more diagnostic of the fragmentation/diversity mechanisms this literature debates.
- **Small young-adult subsample.** Valid cases per model range from roughly 126 to 179, substantially limiting statistical power for the specific hypotheses (H2, H3, H5) that concern this group.
- **Non-random panel attrition.** Respondents who dropped out between Wave 1 and Wave 3 had significantly lower Wave-1 AP than those who stayed (M = 3.97 vs. 5.07; p = .002), and differed in sex and education distribution. IPW correction suggests this does not materially distort the CLPD coefficients, but it may inflate the average polarization levels observed later in the panel.
- **Observational design.** None of the models support causal claims; associations, particularly between extremism and AP, could reflect unmeasured identity-alignment mechanisms.

---

## 8. Conclusions

Across every specification tested — cross-sectional, longitudinal, and interaction-based — social media use fails to emerge as a statistically robust, direct predictor of affective polarization among young Spanish adults. What does emerge, consistently and with growing strength between waves, is ideological extremism: how far a person sits from the political centre, independent of which side they're on. For this sample, the more relevant story about affective polarization among young adults isn't about their information diet on social platforms — it's about the intensity of their ideological commitments. That said, the study's own power analysis is an honest constraint: with the available young-adult sample, the design simply could not rule out a small social-media effect, so these results should be read as evidence that a substantial direct effect is unlikely, not as proof that no effect exists at all. Practically, this suggests that efforts to understand or address affective polarization among young voters may get more traction focusing on ideological identity formation than on social media exposure per se — while leaving open a real possibility that social media's effect, if any, operates indirectly through how polarized citizens perceive the political environment to be, rather than through raw frequency of use.

---

## Repository Note

All analyses in this project were conducted in **R**. This repository includes the complete, documented analysis script (covering data preparation, all regression and robustness models, and the figures referenced above), the generated figures, and supporting documentation describing variable construction and modelling decisions.

The original TriPol survey dataset is **not redistributed** in this repository, as it belongs to the TriPol research project and is subject to its own data-access terms. Researchers interested in replicating this analysis should contact the TriPol project team directly to request access to the Spanish panel data.
