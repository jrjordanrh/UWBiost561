library(UWBiost561)

alpha_vec = c(.5,.6,.7,.8)
data = compare_alpha(alpha_vec,13)


save(data, # save your results
     file = "~/HW4_simulation.RData")
