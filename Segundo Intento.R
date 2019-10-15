hcc_data_complete_balanced_01 <- read_csv("hcc-dataset/hcc-data-complete-balanced.csv", 
                                       col_types = cols(AFP = col_number(), 
                                                        AHT = col_number(), 
                                                        ALP = col_number(), 
                                                        ALT = col_number(), 
                                                        AST = col_number(), 
                                                        Age = col_number(), 
                                                        Alcohol = col_number(), 
                                                        CRI = col_number(), 
                                                        Cirrhosis = col_number(), 
                                                        Class = col_number(), 
                                                        Creatinine = col_number(), 
                                                        Diabetes = col_number(), 
                                                        Dir_Bil = col_number(),
                                                        Endemic = col_number(), 
                                                        Ferritin = col_number(),
                                                        GGT = col_number(), 
                                                        Gender = col_number(),
                                                        Grams_day = col_number(), 
                                                        HBcAb = col_number(),
                                                        HBeAg = col_number(), 
                                                        HBsAg = col_number(),
                                                        HCVAb = col_number(), 
                                                        HIV = col_number(),
                                                        Hallmark = col_number(), 
                                                        Hemochro = col_number(),
                                                        INR = col_number(), 
                                                        Iron = col_number(),
                                                        Major_Dim = col_number(), 
                                                        Metastasis = col_number(),
                                                        NASH = col_number(), 
                                                        Nodule = col_number(),
                                                        Obesity = col_number(),
                                                        PHT = col_number(),
                                                        PVT = col_number(), 
                                                        Packs_year = col_number(),
                                                        Sat = col_number(), 
                                                        Smoking = col_number(),
                                                        Spleno = col_number(), 
                                                        Symptoms = col_number(),
                                                        TP = col_number(), 
                                                        Total_Bil = col_number(),
                                                        Varices = col_number()))

hcc_fil_num <- hcc_data_complete_balanced_01 %>% dplyr::select(Gender, Symptoms, HBsAg, HBeAg, HCVAb, Alcohol, HIV, Obesity, Diabetes, NASH, Smoking, Cirrhosis, PHT, PVT, Metastasis, ALT, AST, GGT, ALP, Creatinine, Dir_Bil, Total_Bil, Albumin, INR, Platelets)

SA_num <- hcc_fil_num %>% filter(Alcohol == 1, Symptoms == 1, GGT > 100, ALT > 100)

formula_02_num <- glm(GGT ~ I(ALT^2), data = SA_num)
summary(formula_02_num)
glance(formula_02_num)

SA_03_num <- hcc_fil_num %>% filter(Alcohol == 1, Symptoms == 1, GGT > 100, ALT > 100, AST > 100)

formula_03_num <- glm(GGT ~ I(ALT^2), data = SA_num)
summary(formula_03_num)
glance(formula_03_num)

