load packages
```{r}
rm(list=ls())



#load relevant libraries
if(!require("pacman")) install.packages('pacman')
pacman::p_load(NetworkToolbox,
               # graph,
               ggthemes,
               knitr ,
               data.table,
               igraph,
               qgraph,
               tidyverse,
               dplyr,
               ggplot2,
               naniar,
               gdata,
               doParallel,
               wCorr,
               qgraph,
               MASS,
               Matrix,
               matrixcalc,
               randcorr,
               rWishart,
               ggpubr,
               ggdark )


```

set working directory

```{r}

### MANUAL INPUT: specify and set working directory ###
workdirec <-"C:/Users/20225262/OneDrive - TU Eindhoven/Documents/GitHub/ResIN" 
setwd(workdirec)
source("functions_behaviorspace_table_output_handling.R")

### MANUAL INPUT: Optionally specify filepath (i.e. where the behaviorspace csv is situated) ###
#NOTE: if csv files are placed in the workdirec, then leave filesPath unchanged
filesPath <- ""
### MANUAL INPUT: specify filenames ###
filesNames <- ("C:/Users/20225262/OneDrive - TU Eindhoven/Documents/Courses/ResIN/Resin dummy experiment ordinal false-table.csv")


```


Read data and rename columns
```{r}
# READ DATA ---------------------------------------------------------------

df <- loadData(filesPath, filesNames)


# REMOVE IRRELEVANT VARIABLES ---------------------------------------------------------------

df <- removeVariables(df)

# RENAME VARIABLES ---------------------------------------------------------------
printColumnNames(df)

### MANUAL INPUT: specify new (easy-to-work-with) variable names ###
new_variable_names <- list(
  "run_number",
  "k",
  "levels",
  "items", 
  "time_step",
  "adjacency_matrix"
)

#change variable names
variable_names <- names(df)
if (length(variable_names) == length(new_variable_names)) {
  clean_df <- changeColumnNames(df, new_variable_names)
} else {
  print (length(variable_names))
  print (length(new_variable_names))
  print("ERROR: the number of variable names you specified is not the same as the number of variables present within the dataframe; please check again")
}
```

Compute Modularity


```{r, echo=FALSE, warning=FALSE, message=FALSE}
#ComputeModularity <- function(p_clean_df, column_name, responses_stable?, stable_levels, stable_items, column_levels, column_items, resolution_parameter)

rp_list <- c(1)                   #resolution_parameter, add more to to list to compare modularity
df_mod <- clean_df
for (i in rp_list) {
df_mod <- ComputeModularity(df_mod, 6, "false", "false", 5, 7,3,4,i)
}
#kable(df_mod<- subset(df_mod, select = -c(adjacency_matrix)), format = "html", booktabs = TRUE)


```
Transform variables with right label
```{r}
#transform 'measurement' variable to numeric (as to avoid ggplot errors)
df_mod$md_1 <- as.numeric(df_mod$md_1)
#round 'measurement' variable to 4 decimals
df_mod$md_1 <- round(df_mod$md_1, 4)
#convert categorical variables to factors (as to avoid ggplot errors)
df_mod$run_number <- as.integer(df_mod$run_number)
df_mod$k <- as.factor(df_mod$k)
df_mod$items <- as.factor(df_mod$items)
df_mod$levels <- as.factor(df_mod$levels)


```


Make Graphs

```{r, echo=FALSE, warning=FALSE, message=FALSE, results='asis', fig.width=15, fig.height=15}
    ggplot(data = df_mod, aes(x = time_step,
                y = md_1)) +
    
    geom_smooth(aes(group=k, color=k),
                method="loess", formula = 'y ~ x', size=1.4, span = 0.2, se=F, fullrange=FALSE, level=0.95) +
    xlab("Timesteps") +
    ylab("Modularity") + 
    labs(title = "Evolution of Modularity (Leiden Algoritm, resolution-parameter == 1)",
       subtitle = "rb == 0, ordinal scale == FALSE!!, Attitude matrix == Whole-world, population size == 500, repetitions == 5 ")+
    scale_color_viridis_d()+
    theme_bw()+
    theme(strip.background = element_rect(fill="black"),
          strip.text = element_text(size=12, colour="white", face = "bold"), 
          plot.title = element_text( size = 18), 
          plot.subtitle = element_text( size = 15),
          axis.title = element_text( size = 15) ) +
      facet_grid( levels ~ items, labeller = label_both ) 



```



