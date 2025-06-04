See https://docs.google.com/document/d/1-ZTDPp39zbhsKbQ2FfBoCtiXKQj0jQSg75MAatYfiGI/edit?usp=sharing for more help

===========================================
===How to set up your UWBiost561 package===
===========================================
- Put all the `.R` files in the "files-to-put-into-R-folder" into your UWBiost561/R folder. These are all the `compute_maximal_partial_clique` implementations
- (Highly recommended): Run `devtools::check()` in your R console (when you're working on the R project for your `UWBiost561` package) to make sure everything works
- Run `remove.packages("UWBiost561")` in R. (This removes the R package to ensure you have a clean slate to do install.)
- Run `devtools::install()` in your R console (when you're working on the R project for your `UWBiost561` package) to install your new version of the `UWBiost561` with all the `compute_maximal_partial_clique` implementations

================================================
===How to check your UWBiost561 package works===
================================================
- Run the `hw4-demo_laptop.R` script. (This script assumes you have working `generate_partial_clique()` function
	- This script generates a random graph with `n=10` nodes
	- It then applies all 15 implementations of `compute_maximal_partial_clique` on that `adj_mat` using `alpha=0.95`, where there's a 30 second time limit
	- The results are put in `result_list`
	- It then stores all the cliques into `clique_list`
	- It then checks which results in `clique_list` are "valid partial cliques" via `compute_correct_density()`
	- Among all the implementations that are valid, it sees which one had the largest partial clique (by computing `size_vec_valid`)

=====================================================
===How to install your UWBiost561 package on Bayes===
=====================================================
- On your laptop:
	- Put the `hw4-demo_bayes_execute.R`, `hw4-demo_bayes_execute.slurm`, and `hw4-demo_bayes_plot.R` files into your `vignettes` folder in your `UWBiost561` package
	- Commit and push all these files from your laptop onto GitHub.com
- On Bayes
	- Log into Bayes
	- If you haven't yet, in your home directory, clone your `UWBiost561` package (via `git clone`). (You'll need to type in your GitHub username and the PAT)
	
	- Then, go into your `UWBiost561` package (NOT the `R` folder)
	- Type in `R` into the terminal to open up `R`
	- Inside R:
		- Type in `remove.packages("UWBiost561")`, followed by `devtools::install()` in R. (If the `remove.packages("UWBiost561")` line errors, it means you never had the UWBiost561 package installed on Bayes in the first place. This is okay. The goal is to make sure there's a clean slate on Bayes either way.)
		- After you've installed your `UWBiost561` package, you should be able to run `library(UWBiost561)` in R

=====================================================
===How to run the demo on Bayes===
=====================================================
- On Bayes
	- Back in the terminal (i.e., not in R), go into the `vignettes` folder and run `sbatch hw4-demo_bayes_execute.slurm`.
	- (You can check the status of your job via `squeue --me`)
	- Once the job is done, open up `R` again
	- Inside R:
		- You can copy-paste the entire script of `hw4-demo_bayes_plot.R` (opened on your local laptop) into the R terminal
		- This script will load the results and make a plot, which will be saved as `hw4-demo_bayes_plot.png` under the `vignettes` folder on Bayes
	- Back in the terminal (i.e., not in R), add the plot to be committed and push it onto GitHub. Specifically, in the vignettes folder, type in `git add hw4-demo_bayes_plot.png`, and then `git commit -m "Pushing plot"` and then `git push origin main`. (Then you'll need to type in your GitHub username and the PAT)
- Back on your laptop:
	- Pull your `UWBiost561` package from GitHub
	- When you open up your `vignettes` folder on your laptop, you should see your plot! :)
	