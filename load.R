# load the Pew dataset and save the required columns in fixed_pew.rdata
opt <- read.csv("data/2013 survey of aging and longevity.csv")

# delete unnecessary columns q6<delete all>, 
original_table <- data.frame(opt$caseid, opt$weight, opt$sample, opt$lang,
                             opt$cregion, opt$form, opt$relig, opt$rq5, opt$rq6, opt$rq10, opt$sex, opt$agerec,
                             opt$educ2, opt$racethn, opt$birth_hisp, opt$usborn, opt$marital,
                             opt$income, opt$q4a, opt$party, opt$employ, opt$q4b, opt$hh1rec, opt$hh3rec,
                             opt$fertrec, opt$q1, opt$q2, opt$q3,
                             opt$q4c, opt$q4d, opt$q4e,opt$q4f,
                             opt$q4g, opt$q4h, opt$q5, opt$q7a, opt$q7b, opt$q7c,
                             opt$q7e, opt$q8, opt$q9, opt$q10, opt$q14, opt$q18h,
                             opt$q21a, opt$q21b, opt$q21c, opt$q21e, opt$q22c, opt$q22d,
                             opt$q25, opt$q26, opt$q43, opt$q45, opt$q58a, opt$q58b, opt$q58c, opt$q58d,
                             opt$rq9a, opt$rq9bf13, opt$rq9cf24, opt$rq9df13, opt$rq9ef24, opt$ideo) 

save(original_table, file = "data/fixed_pew.rdata")

#------------------------------------------------------------------------------
  
# load the Gallup dataset and save the required columns in fixed_gallup.rdata
gallup <- read.csv("data/optimism_by_state.csv")

save(gallup, file = "data/fixed_gallup.rdata")
