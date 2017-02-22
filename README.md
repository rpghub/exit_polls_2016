General election data on voter turnout and preference by demographic groups, such as race or education level, is not publicly available. This project attempts to provide reasonable estimates of those values for each state for the 2016 and 2012 general elections. Results as well as more details on the model and data sources are presented in the [report](https://rpghub.github.io/exit_polls_2016/).
`summary results.csv` contains aggregated results. The individual model results are too large to post to github, but can be created locally by following the steps below.

## Running the Model

1. Clone this repository.
2. Install the required R packages and [Stan](http://mc-stan.org).
3. Replace the file path in the line `base_directory <- `  in the file `fit state model.R` with the location of the cloned repository. 
4. Run the file `fit state model.R`.

## Data Sources

The following data sources were used to fit the model

1. County level election results scraped from [townhall.com](https://townhall.com/election/). Part of the code to scrape the county level results was based on https://github.com/joshuakalla/county_election_results_2016.
2. American Community Survey data from [IPUMS](https://usa.ipums.org/usa/).
3. Exit poll data scraped from [CNN.com](http://www.cnn.com/election/results/exit-polls).
4. Total state vote estimates to benchmark to from [The United States Election Project](http://www.electproject.org/home/voter-turnout/voter-turnout-data).
