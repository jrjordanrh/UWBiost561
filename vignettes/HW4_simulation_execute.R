library(UWBiost561)

alpha_vec = c(.5)
data = compare_alpha(alpha_vec,1)


save(data, # save your results
     file = "~/UWBiost561/vignettes/HW4_simulation.RData")
