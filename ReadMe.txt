load.R
●	getting the tables into data frame
●	printing the two files, fixed_pew.rdata and fixed_gallup.rdata to use in clean.R

clean.R
●	sourcing the func.R
●	loading the data, weight scores
●	pew
	○	loading the data frame (output from load.R)
	○	changing the answers into numbers
	○	changing the doesn’t know and doesn’t apply to NAs
	○	calculating the optimism score and store in a column
	○	printing the cleaned data file, cleaned_pew.rdata (ready to use for analysis)
●	gallup
	○	loading the data frame (output from load.R)
	○	adding region column to be the same as the Pew dataset
	○	printing the cleaned data file, cleaned_gallup.rdata (ready to use for analysis)

func.R
●	calc_score(row): return the optimism score of the row
●	view_prop(x, col): return a vector that contains proportions for the people who answered x for six optimism score groups

do.R
●	packages used – animation, ggplot2
●	loading the cleaned pew and gallup data frame (output from clean.R)
●	saving graphs for explanatory variables vs. optimism score
●	among a lot of plots in this file, only 6 files used in the presentation and in the paper were saved
●	the 6 files - histogram.gif (in the same directory as do.R)
				- hist_pew.jpg (saved in the folder named graphs)
				- hist_gallup.jpg (saved in the folder named graphs)
				- gender_score.jpg (saved in the folder named graphs)
				- age_score.jpg (saved in the folder named graphs)
				- income_score.jpg (saved in the folder named graphs)

do_effects.R
●	libraries used - ggplot2, reshape2
●	sourcing func.R
●	loading the cleaned pew and gallup data frame (output from clean.R)
●	printing graphs for optimism score vs. response variables
●	since there are a lot of graphs printing, these are not saved
●	the response variable graphs can be looked at the interactive graphs

heat_map.R
●	packages used - wq, ggplot2, reshape2, grid
●	sourcing func.R
●	loading the cleaned pew and gallup data frame (output from clean.R)
●	saving heat maps on similar topics in graphs folder


HOW TO RUN:
First, the csv files have to be in data folder, which should be in the same directory as the R source code files. Also,, the resulted rdata files from each stage will also be saved in that data folder.

Secondly, the path also should have graphs folder to save the resulting graphs.

Just to look at loading the data: load.R

Just to look at how the data was cleaned: clean.R

To do the analysis on explanatory variables: do.R
	To look at a certain graph, only the code between the dashed lines could be run after sourcing and loading the data and packages

To do the analysis on response variables: do_effects.R
	Saving graph code were commented out, so to save the graph, uncomment the jpeg(...) and dev.off() line.
	To look at a certain graph, only the code between the dashed lines could be run after sourcing and loading the data and packages

To produce a heat map: heat_map.R
	Saving heat map in the graphs folder
	To look at a certain graph, only the code between the dashed lines could be run after sourcing and loading the data and packages
