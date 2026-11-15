

lm_melange_2gaus <- list()
lm_melange_3gaus <- list()
lm_melange_4gaus <- list()

lm_sep_2season <- list()
lm_sep_3season <- list()
lm_sep_4season <- list()
for (St in StsNames){
  print(St)
  lm_melange_2gaus[[St]] <- list(
    "Mu1" = lm(Mu1 ~ Year, data = Melange_2_gauss[[St]]),
    "Mu2" = lm(Mu2 ~ Year, data = Melange_2_gauss[[St]])
  )
  
  lm_sep_2season[[St]] <- list(
    "Mu1" = lm(Mu1 ~ Year, data = Melange_2_gauss[[St]]),
    "Mu2" = lm(Mu2 ~ Year, data = Melange_2_gauss[[St]])
  )
  
  lm_melange_3gaus[[St]] <- list(
    "Mu1" = lm(Mu1 ~ Year, data = Melange_3_gauss[[St]]),
    "Mu2" = lm(Mu2 ~ Year, data = Melange_3_gauss[[St]]),
    "Mu3" = lm(Mu3 ~ Year, data = Melange_3_gauss[[St]])
  )
  
  lm_sep_3season[[St]] <- list(
    "Mu1" = lm(Mu1 ~ Year, data = Melange_3_gauss[[St]]),
    "Mu2" = lm(Mu2 ~ Year, data = Melange_3_gauss[[St]]),
    "Mu3" = lm(Mu3 ~ Year, data = Melange_3_gauss[[St]])
  )
  
  lm_melange_4gaus[[St]] <- list(
    "Mu1" = lm(Mu1 ~ Year, data = Melange_4_gauss[[St]]),
    "Mu2" = lm(Mu2 ~ Year, data = Melange_4_gauss[[St]]),
    "Mu3" = lm(Mu3 ~ Year, data = Melange_4_gauss[[St]]),
    "Mu4" = lm(Mu4 ~ Year, data = Melange_4_gauss[[St]])
  )
  
  lm_sep_4season[[St]] <- list(
    "Mu1" = lm(Mu1 ~ Year, data = Melange_4_gauss[[St]]),
    "Mu2" = lm(Mu2 ~ Year, data = Melange_4_gauss[[St]]),
    "Mu3" = lm(Mu3 ~ Year, data = Melange_4_gauss[[St]]),
    "Mu4" = lm(Mu4 ~ Year, data = Melange_4_gauss[[St]])
  )
}


outputs_path <- "output/Part 1"
save(lm_melange_2gaus, file = paste(
  outputs_path, "/lm_melange_2gaus.RData", sep = ""
))
save(lm_melange_3gaus, file = paste(
  outputs_path, "/lm_melange_3gaus.RData", sep = ""
))
save(lm_melange_4gaus, file = paste(
  outputs_path, "/lm_melange_4gaus.RData", sep = ""
))

save(lm_sep_2season, file = paste(
  outputs_path, "/lm_sep_2season.RData", sep = ""
))
save(lm_sep_3season, file = paste(
  outputs_path, "/lm_sep_3season.RData", sep = ""
))
save(lm_sep_4season, file = paste(
  outputs_path, "/lm_sep_4season.RData", sep = ""
))