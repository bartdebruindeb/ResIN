# READ DATA ---------------------------------------------------------------
loadData <- function(p_files_path, p_files_names) {
  
  #read in datafiles using filesNames and filesPath variables
  for (i in 1:length(p_files_names)) {
    print(paste("read csv from:", p_files_path, p_files_names[i], sep=""))
    #bind data from dataframe into new dataframe
    if (exists('t_df') && is.data.frame(get('t_df'))) {
      temp_df <- read.csv(paste(p_files_path, p_files_names[i], sep=""), skip = 6, sep = ",",head=TRUE,stringsAsFactors = TRUE)
      temp_df$X.run.number. <- temp_df$X.run.number + max_run_number
      t_df <- rbind(t_df, temp_df)
    }  else {
      t_df <- read.csv(paste(p_files_path, p_files_names[i], sep=""), skip = 6, sep = ",",head=TRUE,stringsAsFactors = TRUE)
    }
    max_run_number <- max(t_df$X.run.number.)
  }
  return(t_df)
}

unpacklists <- function(t_df){
}

# REMOVE INVALID RUNS ---------------------------------------------------------------

cleanData <- function(p_df, p_infected_max_below_remove) {
  
  # remove all invalid runs, 1) for every run
  p_clean_df <- p_df
  r_removed <- 0
  print(paste("Removing runs that are invalid: infected <",p_infected_max_below_remove))
  for(i in 1:max(p_df$X.run.number.)) {
    # 2) get maximum number of infected and compare with infected_max_below_remove
    if (max(p_df[p_df$X.run.number.==i, ]$count.people.with..is.infected..) < p_infected_max_below_remove) {
      p_clean_df <- p_clean_df[p_clean_df$X.run.number!=i, ]
      print(paste(".. removed run", i, ", infected ", max(p_df[p_df$X.run.number.==i, ]$count.people.with..is.infected..), "<", p_infected_max_below_remove))
      r_removed <- r_removed + 1
    }
  }
  print(paste("Removed",r_removed,"runs"))
  return(p_clean_df)
}


# REMOVE IRRELEVANT VARIABLES ---------------------------------------------------------------

#Loop through dataframe and identify variables that do NOT vary (i.e. that are FIXED)
#Unfixed variables are either independent or dependent and therefore relevant to include in the analysis
removeVariables <- function(p_clean_df) {

  relevant_var_list <- list()
  index <- 1
  for (i in colnames(p_clean_df)){
    if (class(p_clean_df[[i]]) == "numeric" | class(p_clean_df[[i]]) == "integer") {
      if (min(p_clean_df[[i]]) != max(p_clean_df[[i]])) {
        relevant_var_list[[index]] <- i
        index <- index + 1
      }
    } else {
      if (nlevels(p_clean_df[[i]]) != 1) {
        relevant_var_list[[index]] <- i
        index <- index + 1
      }
    }
  }
  #Fixed variables are irrelevant and may therefore be dropped from the dataframe (i.e. select to keep only relevant variables)
  p_clean_df <- dplyr::select(p_clean_df, unlist(relevant_var_list))
  #remove redundant variables in order to free up working space
  rm(list = "relevant_var_list")
  
  return(p_clean_df);
}

# RENAME VARIABLES ---------------------------------------------------------------
printColumnNames <- function(p_clean_df) {
  
  variable_names <- names(p_clean_df)
  index <- 1
  for (i in variable_names) {
    # print(i)
    print(paste("Column", index, "is called:", i))
    index <- index + 1
  }
}

changeColumnNames <- function(p_clean_df, p_new_variable_names) {
  
  variable_names <- names(p_clean_df)
  #change variable names
  for (i in 1:length(variable_names)){
    colnames(p_clean_df)[i] = p_new_variable_names[[i]]
  }
  
  #remove redundant variables to free up working memory
  rm(list = "p_new_variable_names")
  
  return(p_clean_df)
}

# Compute modularity --------------------------------------------------------------
ComputeModularity <- function(p_clean_df, column_name, levels_stable, items_stable, stable_levels, stable_items, column_levels, column_items, rep) {
  for(i in 1:length(p_clean_df[[column_name]])) {
    if (levels_stable == "true" && items_stable == "true") {
      columns <- stable_levels * stable_items
    }
    else if (levels_stable == "true" && items_stable == "false"){
      columns <- stable_levels * p_clean_df[[i,column_items]]
    }
    else if (levels_stable == "false" && items_stable == "true"){
      columns <- p_clean_df[[i,column_levels]] * stable_items 
    }
    else {
      columns <- columns <- p_clean_df[[i,column_levels]] * p_clean_df[[i,column_items]]
    }
    aj_matrix <- pmax(matrix(scan(quiet = TRUE, text = toString(p_clean_df[[i,column_name]])), ncol = columns,byrow = TRUE),0)
    aj_graph <- graph_from_adjacency_matrix(aj_matrix, mode = "max", weighted = TRUE)
    cl <- cluster_leiden(aj_graph, objective_function = "modularity", resolution_parameter = rep, n_iterations = 3)
    clusters <- length(cl)
    cd <- conductance(aj_graph, membership(as_membership(cl)))
    md <- modularity(aj_graph, membership(as_membership(cl)))
    if (exists('md_df') && is.data.frame(get('md_df'))) {
      md_df <- rbind(md_df, md)
      clusters_df <- rbind(clusters_df, clusters)
      cd_df <- rbind(cd_df, cd)
    }
    else {
      md_df <- data.frame(md)
      clusters_df <- data.frame(clusters)
      cd_df <- data.frame(cd)
    }
  }
    p_clean_df <- cbind(p_clean_df, md_df)
    colnames(p_clean_df)[ncol(p_clean_df)] = paste("md", rep, sep = "_" )
    p_clean_df <- cbind(p_clean_df, clusters_df)
    colnames(p_clean_df)[ncol(p_clean_df)] = paste("clusters", sep = "_" )
 #   p_clean_df <- cbind(p_clean_df, cd_df)
#    colnames(p_clean_df)[ncol(p_clean_df)] = paste("cd", rep, sep = "_" )
  return (p_clean_df)
}
