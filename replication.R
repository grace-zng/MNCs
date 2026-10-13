rm(list=ls())

library(tidyverse)
library(stargazer)
library(plm)

cp <- read.csv("cp.csv")

cp |> filter(gvco > 1) |> select(country_iso3c, year, gvco)
cor(cp[21:27], use = "complete.obs")
cor(cp[21:27], use = "na.or.complete")

cp <- cp |> filter(gvco <= 1)

cor(check[21:27], use = "complete.obs")

# Full sample, OLS, FE

# Only GVC, GDP, Polity, EPI

lm1 <- plm(nti_env_chapter ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc, data = cp, index = c("country_iso3c", "year"), model = "within")
lm2 <- plm(env_level ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc, data = cp, index = c("country_iso3c", "year"), model = "within")
lm3 <- plm(env_coop ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc, data = cp, index = c("country_iso3c", "year"), model = "within")
lm4 <- plm(env_harm ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc, data = cp, index = c("country_iso3c", "year"), model = "within")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = c("Chapter",
                             "Level",
                             "Cooperation",
                             "Harmonization"),
          keep=c("l1.gvco", "l1.polity2", "l1.log_gdp", "l1.epi_cc"),
          order=c("l1.gvco","l1.polity2", "l1.log_gdp", "l1.epi_cc"),
          covariate.labels=c("GVC-related output",
                             "Democracy", "Ln(GDP)", "EPI"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/full_ols_no_controls.tex")

# Plus Green Party

lm1 <- plm(nti_env_chapter ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share, data = cp, index = c("country_iso3c", "year"), model = "within")
lm2 <- plm(env_level ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share, data = cp, index = c("country_iso3c", "year"), model = "within")
lm3 <- plm(env_coop ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share, data = cp, index = c("country_iso3c", "year"), model = "within")
lm4 <- plm(env_harm ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share, data = cp, index = c("country_iso3c", "year"), model = "within")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = c("Chapter",
                             "Level",
                             "Cooperation",
                             "Harmonization"),
          keep=c("l1.gvco", "l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share"),
          order=c("l1.gvco","l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share"),
          covariate.labels=c("GVC-related output",
                             "Democracy", "Ln(GDP)", "EPI",
                             "Green Party VS"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/full_ols_vs.tex")

# Plus Import Penetration

lm1 <- plm(nti_env_chapter ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm2 <- plm(env_level ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm3 <- plm(env_coop ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm4 <- plm(env_harm ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = c("Chapter",
                             "Level",
                             "Cooperation",
                             "Harmonization"),
          keep=c("l1.gvco", "l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.ipr"),
          order=c("l1.gvco","l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.ipr"),
          covariate.labels=c("GVC-related output",
                             "Democracy", "Ln(GDP)", "EPI",
                             "Import penetration"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/full_ols_ipr.tex")

# Full set of controls

lm1 <- plm(nti_env_chapter ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm2 <- plm(env_level ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm3 <- plm(env_coop ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm4 <- plm(env_harm ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = c("Chapter",
                             "Level",
                             "Cooperation",
                             "Harmonization"),
          keep=c("l1.gvco", "l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          order=c("l1.gvco","l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          covariate.labels=c("GVC-related output",
                             "Democracy", "Ln(GDP)", "EPI",
                             "Green Party VS", "Import penetration"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/full_ols_all.tex")

# Appendix

# Sectoral

names(cp)[21:27]

# Agr

lm1 <- plm(nti_env_chapter ~ l1.gvc_sect_agr + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm2 <- plm(env_level ~ l1.gvc_sect_agr + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm3 <- plm(env_coop ~ l1.gvc_sect_agr + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm4 <- plm(env_harm ~ l1.gvc_sect_agr + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = c("Chapter",
                             "Level",
                             "Cooperation",
                             "Harmonization"),
          keep=c("l1.gvc_sect_agr", "l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          order=c("l1.gvc_sect_agr","l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          covariate.labels=c("GVC-related output, Agriculture",
                             "Democracy", "Ln(GDP)", "EPI",
                             "Green Party VS", "Import penetration"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/full_ols_all_agr.tex")

# Con

lm1 <- plm(nti_env_chapter ~ l1.gvc_sect_con + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm2 <- plm(env_level ~ l1.gvc_sect_con + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm3 <- plm(env_coop ~ l1.gvc_sect_con + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm4 <- plm(env_harm ~ l1.gvc_sect_con + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = c("Chapter",
                             "Level",
                             "Cooperation",
                             "Harmonization"),
          keep=c("l1.gvc_sect_con", "l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          order=c("l1.gvc_sect_con","l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          covariate.labels=c("GVC-related output, Construction",
                             "Democracy", "Ln(GDP)", "EPI",
                             "Green Party VS", "Import penetration"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/full_ols_all_con.tex")

# Mfg

lm1 <- plm(nti_env_chapter ~ l1.gvc_sect_mfg + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm2 <- plm(env_level ~ l1.gvc_sect_mfg + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm3 <- plm(env_coop ~ l1.gvc_sect_mfg + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm4 <- plm(env_harm ~ l1.gvc_sect_mfg + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = c("Chapter",
                             "Level",
                             "Cooperation",
                             "Harmonization"),
          keep=c("l1.gvc_sect_mfg", "l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          order=c("l1.gvc_sect_mfg","l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          covariate.labels=c("GVC-related output, Manufacturing",
                             "Democracy", "Ln(GDP)", "EPI",
                             "Green Party VS", "Import penetration"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/full_ols_all_mfg.tex")

# Min

lm1 <- plm(nti_env_chapter ~ l1.gvc_sect_min + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm2 <- plm(env_level ~ l1.gvc_sect_min + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm3 <- plm(env_coop ~ l1.gvc_sect_min + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm4 <- plm(env_harm ~ l1.gvc_sect_min + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = c("Chapter",
                             "Level",
                             "Cooperation",
                             "Harmonization"),
          keep=c("l1.gvc_sect_min", "l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          order=c("l1.gvc_sect_min","l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          covariate.labels=c("GVC-related output, Mining",
                             "Democracy", "Ln(GDP)", "EPI",
                             "Green Party VS", "Import penetration"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/full_ols_all_min.tex")

# Srv

lm1 <- plm(nti_env_chapter ~ l1.gvc_sect_srv + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm2 <- plm(env_level ~ l1.gvc_sect_srv + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm3 <- plm(env_coop ~ l1.gvc_sect_srv + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm4 <- plm(env_harm ~ l1.gvc_sect_srv + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = c("Chapter",
                             "Level",
                             "Cooperation",
                             "Harmonization"),
          keep=c("l1.gvc_sect_srv", "l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          order=c("l1.gvc_sect_srv","l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          covariate.labels=c("GVC-related output, Services",
                             "Democracy", "Ln(GDP)", "EPI",
                             "Green Party VS", "Import penetration"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/full_ols_all_srv.tex")

# Utl

lm1 <- plm(nti_env_chapter ~ l1.gvc_sect_utl + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm2 <- plm(env_level ~ l1.gvc_sect_utl + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm3 <- plm(env_coop ~ l1.gvc_sect_utl + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
lm4 <- plm(env_harm ~ l1.gvc_sect_utl + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share + l1.ipr, data = cp, index = c("country_iso3c", "year"), model = "within")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = c("Chapter",
                             "Level",
                             "Cooperation",
                             "Harmonization"),
          keep=c("l1.gvc_sect_utl", "l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          order=c("l1.gvc_sect_utl","l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.vote_share", "l1.ipr"),
          covariate.labels=c("GVC-related output, Utilities",
                             "Democracy", "Ln(GDP)", "EPI",
                             "Green Party VS", "Import penetration"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/full_ols_all_utl.tex")



# Chapter, Logit, lagged with controls

lm1 <- glm(nti_env_chapter ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc, data = cp, family = "binomial")
lm2 <- glm(nti_env_chapter ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.ipr, data = cp, family = "binomial")
lm3 <- glm(nti_env_chapter ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.vote_share, data = cp, family = "binomial")
lm4 <- glm(nti_env_chapter ~ l1.gvco + l1.polity2 + l1.log_gdp + l1.epi_cc + l1.ipr + l1.vote_share, data = cp, family = "binomial")
stargazer(lm1, lm2, lm3, lm4, type = "text")
stargazer(lm1, lm2, lm3, lm4, type = "latex",
          title = "GVC Integration and Environmental Provisions, OLS Model",
          dep.var.labels = "Inclusion of an Environment Chapter",
          keep=c("l1.gvco", "l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.ipr", "l1.vote_share"),
          order=c("l1.gvco","l1.polity2", "l1.log_gdp", "l1.epi_cc", "l1.ipr", "l1.vote_share"),
          covariate.labels=c("GVC-related output",
                             "Democracy", "Ln(GDP)",
                             "EPI",
                             "Import Penetration",
                             "Green Party Vote Share"),
          digits = 3,
          omit.stat = c("ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "Tables/what_chapter_logit.tex")
