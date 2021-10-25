library(dplyr)

# Load data
filtered_data <- read.csv('Filtered_Data.csv')

# Add Preschool_info column to specify which preschool program each child participated in
filtered_data <- filtered_data %>%
  mutate(Preschool_info = case_when(HS ~ 'HS', Preschool ~ 'Preschool', No_Preschool ~ 'None'))

# List of income columns
Income_col_names = c('NetFamInc78', 'NetFamInc79', 'NetFamInc80', 'NetFamInc81', 'NetFamInc82', 
                     'NetFamInc83', 'NetFamInc84', 'NetFamInc85', 'NetFamInc86', 'NetFamInc87',
                     'NetFamInc88', 'NetFamInc89', 'NetFamInc90', 'NetFamInc91', 'NetFamInc92',
                     'NetFamInc93', 'NetFamInc95', 'NetFamInc97', 'NetFamInc99', 'NetFamInc101',
                     'NetFamInc103')
# Scale each year's income to account for inflation. The baseline is 2004.
Income_rescale = c(2.82, 2.54, 2.24, 2.03, 1.90, 1.85, 1.78, 1.71, 1.68, 1.62, 1.55, 1.48,
                   1.41, 1.35, 1.31, 1.27, 1.21, 1.15, 1.10, 1.04, 1.00)
for (i in 1:length(Income_col_names)) {
  name = Income_col_names[i]
  filtered_data[, name] = filtered_data[, name] * Income_rescale[i]
}

# Each household's permanent income is calculated as the average over the years
filtered_data <- filtered_data %>%
  rowwise() %>%
  mutate(permanent_income = mean(c(NetFamInc78, NetFamInc79, NetFamInc80, NetFamInc81, NetFamInc82, 
                                      NetFamInc83, NetFamInc84, NetFamInc85, NetFamInc86, NetFamInc87,
                                      NetFamInc88, NetFamInc89, NetFamInc90, NetFamInc91, NetFamInc92,
                                      NetFamInc93, NetFamInc95, NetFamInc97, NetFamInc99, NetFamInc101,
                                      NetFamInc103), na.rm = TRUE))

# Get the average permanent income within each preschool program
Income_statistics <- filtered_data %>%
  group_by(Preschool_info) %>%
  summarise(permanent_income_mean = mean(permanent_income, na.rm = TRUE), 
            permanent_income_sd = sd(permanent_income, na.rm = TRUE))
Income_statistics[dim(Income_statistics)[1] + 1, ] = list('All Programs', 
                                                   mean(filtered_data$permanent_income, na.rm = TRUE),
                                                   sd(filtered_data$permanent_income, na.rm = TRUE))

# Same calculation with an additional constraint that only the fixed effect (FE) households are
# included in the calculation
Income_FE_statistics <- filtered_data %>%
  filter(FE == 1) %>%
  group_by(Preschool_info) %>%
  summarise(permanent_income_mean_FE = mean(permanent_income, na.rm = TRUE), 
            permanent_income_sd_FE = sd(permanent_income, na.rm = TRUE))
Income_FE_statistics[dim(Income_FE_statistics)[1] + 1, ] = list('All Programs', 
                                                          mean((filtered_data %>% filter(FE==1))$permanent_income, na.rm = TRUE),
                                                          sd((filtered_data %>% filter(FE==1))$permanent_income, na.rm = TRUE))

# A binary indicator of whether the mother had less than 12 years of education (dropped out
# before college)
filtered_data <- filtered_data %>%
  rowwise() %>%
  mutate(mother_dropout = ifelse(max(across(starts_with('HighGrade_Moth')), na.rm = TRUE) < 12, 1, 0))

MotherDropOut <- filtered_data %>%
  group_by(Preschool_info) %>%
  summarise(Mother_dropout_mean = mean(mother_dropout, na.rm = TRUE),
            Mother_dropout_sd = sd(mother_dropout, na.rm = TRUE))
MotherDropOut[dim(MotherDropOut)[1] + 1, ] = list('All Programs', 
                                                  mean(filtered_data$mother_dropout, na.rm = TRUE),
                                                  sd(filtered_data$mother_dropout, na.rm = TRUE))

# Same calculation for FE group
MotherDropOut_FE <- filtered_data %>%
  filter(FE == 1) %>%
  group_by(Preschool_info) %>%
  summarise(Mother_dropout_FE = mean(mother_dropout, na.rm = TRUE),
            Mother_dropout_FE_sd = sd(mother_dropout, na.rm = TRUE))
MotherDropOut_FE[dim(MotherDropOut_FE)[1] + 1, ] = list('All Programs', 
                                                        mean((filtered_data %>% filter(FE==1))$mother_dropout, na.rm = TRUE),
                                                        sd((filtered_data %>% filter(FE==1))$mother_dropout, na.rm = TRUE))

# Indicator for mother having more than 12 years of education (attended some college)
filtered_data <- filtered_data %>%
  rowwise() %>%
  mutate(mother_college = ifelse(max(across(starts_with('HighGrade_Moth')), na.rm = TRUE) > 12, 1, 0))

MotherCollege <- filtered_data %>%
  group_by(Preschool_info) %>%
  summarise(Mother_college_mean = mean(mother_college, na.rm = TRUE),
            Mother_college_sd = sd(mother_college, na.rm = TRUE))
MotherCollege[dim(MotherCollege)[1] + 1, ] = list('All Programs', 
                                                  mean(filtered_data$mother_college, na.rm = TRUE),
                                                  sd(filtered_data$mother_college, na.rm = TRUE))

# Same calculation for FE group
MotherCollege_FE <- filtered_data %>%
  filter(FE == 1) %>%
  group_by(Preschool_info) %>%
  summarise(Mother_college_FE = mean(mother_college, na.rm = TRUE),
            Mother_college_FE_sd = sd(mother_college, na.rm = TRUE))
MotherCollege_FE[dim(MotherCollege_FE)[1] + 1, ] = list('All Programs', 
                                                        mean((filtered_data %>% filter(FE==1))$mother_college, na.rm = TRUE),
                                                        sd((filtered_data %>% filter(FE==1))$mother_college, na.rm = TRUE))

# Rescale the AFQT score based on the time it was taken
filtered_data <- filtered_data %>%
  mutate(AgeAFQT = case_when(Age_Mom79==14 ~ AFQT_Pct81_REV *(35.60881/28.79544),
         Age_Mom79==15 ~ AFQT_Pct81_REV *(35.60881/32.86273),
         Age_Mom79==16 ~ AFQT_Pct81_REV *(35.60881/32.86273),
         Age_Mom79==17 ~ AFQT_Pct81_REV *(35.60881/36.3544),
         Age_Mom79==18 ~ AFQT_Pct81_REV *(35.60881/33.45777),
         Age_Mom79==19 ~ AFQT_Pct81_REV *(35.60881/36.84),
         Age_Mom79==20 ~ AFQT_Pct81_REV *(35.60881/41.84536),
         Age_Mom79==21 ~ AFQT_Pct81_REV *(35.60881/40.95177),
         Age_Mom79==22 ~ AFQT_Pct81_REV *(35.60881/42.82069)))
filtered_data[, "AgeAFQT"] = scale(filtered_data[,"AgeAFQT"])

# Get the mean and sd of the AFQT scores
AFQT_statistics <- filtered_data %>%
  group_by(Preschool_info) %>%
  summarise(AFQT_mean = mean(AgeAFQT, na.rm = TRUE), 
            AFQT_sd = sd(AgeAFQT, na.rm = TRUE))
AFQT_statistics[dim(AFQT_statistics)[1] + 1, ] = list('All Programs', 
                                                  mean(filtered_data$AgeAFQT, na.rm = TRUE),
                                                  sd(filtered_data$AgeAFQT, na.rm = TRUE))

# Same calculation for FE group
AFQT_statistics_FE <- filtered_data %>%
  filter(FE == 1) %>%
  group_by(Preschool_info) %>%
  summarise(AFQT_FE = mean(AgeAFQT, na.rm = TRUE), 
            AFQT_FE_sd = sd(AgeAFQT, na.rm = TRUE))
AFQT_statistics_FE[dim(AFQT_statistics_FE)[1] + 1, ] = list('All Programs', 
                                                            mean((filtered_data %>% filter(FE==1))$AgeAFQT, na.rm = TRUE),
                                                            sd((filtered_data %>% filter(FE==1))$AgeAFQT, na.rm = TRUE))

# The education level of the grandmother
GrandMom_ED <- filtered_data %>%
  group_by(Preschool_info) %>%
  summarise(Grandmom_ED = mean(HighGrade_GMom79, na.rm = TRUE), 
            Grandmom_sd = sd(HighGrade_GMom79, na.rm = TRUE))
GrandMom_ED[dim(GrandMom_ED)[1] + 1, ] = list('All Programs', 
                                              mean(filtered_data$HighGrade_GMom79, na.rm = TRUE),
                                              sd(filtered_data$HighGrade_GMom79, na.rm = TRUE))

# Same calculation for the FE group
GrandMom_ED_FE <- filtered_data %>%
  filter(FE == 1) %>%
  group_by(Preschool_info) %>%
  summarise(Grandmom_ED_FE = mean(HighGrade_GMom79, na.rm = TRUE), 
            Grandmom_FE_sd = sd(HighGrade_GMom79, na.rm = TRUE))
GrandMom_ED_FE[dim(GrandMom_ED_FE)[1] + 1, ] = list('All Programs', 
                                                    mean((filtered_data %>% filter(FE==1))$HighGrade_GMom79, na.rm = TRUE),
                                                    sd((filtered_data %>% filter(FE==1))$HighGrade_GMom79, na.rm = TRUE))

# Join all the above statistic tables together
covariate_statistics <- left_join(Income_statistics, Income_FE_statistics)
covariate_statistics <- left_join(covariate_statistics, MotherDropOut)
covariate_statistics <- left_join(covariate_statistics, MotherDropOut_FE)
covariate_statistics <- left_join(covariate_statistics, MotherCollege)
covariate_statistics <- left_join(covariate_statistics, MotherCollege_FE)
covariate_statistics <- left_join(covariate_statistics, AFQT_statistics)
covariate_statistics <- left_join(covariate_statistics, AFQT_statistics_FE)
covariate_statistics <- left_join(covariate_statistics, GrandMom_ED)
covariate_statistics <- left_join(covariate_statistics, GrandMom_ED_FE)

# Flip the table for better viewing
flipped = data.frame(t(covariate_statistics[-1]))
colnames(flipped) = covariate_statistics$Preschool_info
write.csv(flipped, 'Table 1.csv')

