load packages
```{r}
rm(list=ls())



#load relevant libraries
if(!require("pacman")) install.packages('pacman')
pacman::p_load(NetworkToolbox,
               # graph,
               ggthemes,
               knitr ,
               clustAnalytics,
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
filesNames <- ("C:/Users/20225262/OneDrive - TU Eindhoven/Documents/Courses/ResIN/Resin dummy experiment-table.csv")


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
  "ordinal_scale",
  "rb",
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


```{r, echo=FALSE, warning=FALSE, message=FALSE, results='df_mod'}
#ComputeModularity <- function((p_clean_df, column_name, levels_stable, items_stable, stable_levels, stable_items, column_levels, column_items, rep)
rp_list <- c(1)                   #resolution_parameter, add more to to list to compare modularity
df_mod <- clean_df
for (i in rp_list) {
df_mod <- ComputeModularity(df_mod, 6, "true", "true", 7, 5, 0, 0, i)
}
kable(df_mod<- subset(df_mod, select = -c(adjacency_matrix)), format = "html", booktabs = TRUE)

#transform 'measurement' variable to numeric (as to avoid ggplot errors)
df_mod$md_1 <- as.numeric(df_mod$md_1)
df_mod$clusters <- as.numeric(df_mod$clusters)
df_mod$cd <- as.numeric(df_mod$cd)
#round 'measurement' variable to 4 decimals
df_mod$md_1 <- round(df_mod$md_1, 4)
#convert categorical variables to factors (as to avoid ggplot errors)
df_mod$run_number <- as.integer(df_mod$run_number)
df_mod$k <- as.factor(df_mod$k)
df_mod$rb <- as.factor(df_mod$rb)
df_mod$ordinal_scale <- as.factor(df_mod$ordinal_scale)

```



Make Graphs:


Modularity:

```{r, echo=FALSE, warning=FALSE, message=FALSE, results='asis', fig.width=13, fig.height=7.5}
    
# sorted_mean <- filter(df_mod, time_step == max(df_mod$time_step)) 
                          
# sorted_mean <- sorted_mean[order(sorted_mean$run_number,decreasing = FALSE),]
# cluster_means <- .colMeans(sorted_mean$clusters, 5, length(sorted_mean$clusters) / 5)

ggplot(data = df_mod, aes(x = time_step,
                y = md_1)) +
    
    geom_smooth(aes(group=k, color=k),
                method="loess", formula = 'y ~ x', linewidth=1.3, span = 0.2, se=F, fullrange=FALSE, level=0.95) +
    xlab("Timesteps") +
    ylab("Modularity") + 
    labs(title = "Evolution of Modularity (Leiden Algoritm, resolution-parameter == 1)",
       subtitle = "Levels == 5, items == 7, Attitude matrix == Whole-world, population size == 500, repetitions == 5 ")+
    scale_color_viridis_d()+
    theme_bw()+
    theme(strip.background = element_rect(fill="black"),
          strip.text = element_text(size=12, colour="white", face = "bold"), 
          plot.title = element_text( size = 18), 
          plot.subtitle = element_text( size = 15),
          axis.title = element_text( size = 15) ) +
      facet_grid( ordinal_scale ~ rb, labeller = label_both ) 



```



