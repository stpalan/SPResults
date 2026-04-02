# SPResults

R package for systematically saving, documenting and outputting results for a research article. Make sure you follow these rules while coding:

  - First prepare the data, then run analyses on the data.
  - Never modify existing data that may already have been referenced in downstream analyses. So instead of, for example, removing outliers from a column, create a new column with outliers removed. This avoids that older analyses suddenly yield different results.
  - Always set a fixed seed before any data generation/analysis that requires random numbers. If random numbers are generated in different places throughout the script, set a seed every time. Ideally, write the seed value into a variable and always set the seed to the same variable and thus value.