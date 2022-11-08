library(tidyverse)
library(here)
library(haven)

#https://nces.ed.gov/ccd/files.asp#FileNameId:11,VersionId:14,FileSchoolYearId:19,Page:1
data_04 <- here("Examples",
             "sc041bkn.dat") %>%
  read_fwf(col_positions = fwf_cols("NCESSCH" = c(1, 12),
                                    "SCHNAM04" = c(107, 156),
                                    "MSTATE04" = c(0227, 0228),
                                    "FRELCH04" = c(383, 386),
                                    "REDLCH04" = c(387, 390),
                                    "TOTFRL04" = c(391, 394),
                                    "MEMBER04" = c(1359,1362),
                                    "ASIAN04" = c(1379, 1382),
                                    "HISP04" = c(1395, 1398),
                                    "BLACK04" = c(1411, 1414),
                                    "WHITE04" = c(1427, 1430),
                                    "PUPTCH04" = c(1447, 1452))) %>%
  filter(MSTATE04 == "LA") %>%
  mutate(NCESSCH = as.character(NCESSCH)) %>%
  select(-MSTATE04) %>%
  filter(MEMBER04 > 0)


data_06 <- here("Examples",
                "sc061ckn.sas7bdat") %>%
  read_sas() %>%
  filter(MSTATE06 == "LA") %>%
  select(NCESSCH,
         SCHNAM06,
         FRELCH06,
         REDLCH06,
         TOTFRL06,
         MEMBER06,
         ASIAN06,
         HISP06,
         BLACK06,
         WHITE06,
         PUPTCH06) %>%
  filter(MEMBER06 > 0)

full_data <- full_join(data_04, data_06)

write_csv(full_data, here("Examples", "LA_school_data.csv"))

