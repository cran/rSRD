utilsCalculateRank <- function(df,nameCol) {
  df <- df %>% tibble::add_column( !!(paste(nameCol,"Rank",sep="_")):=rank(df[nameCol]), .after=nameCol )
  return(df)
}

#' @title utilsCalculateDistance
#' @name utilsCalculateDistance
#' @aliases utilsCalculateDistance
#' @export utilsCalculateDistance
#' @author Ali Tugay Sen
#' @description Calculates the Manhattan-distance between two rankings and inserts it into the DataFrame after the first column.
#' @param df A DataFrame.
#' @param nameCol The current Column of the iteration.
#' @param refCol The reference Column of the dataFrame.
#' @return Returns a new \code{df} that has a Distance Column based on the \code{nameCol}.
#' @examples
#' SRDInput <- data.frame(
#' A=c(32, 52, 44, 44, 47),
#' B=c(73, 75, 65, 76, 70),
#' C=c(60, 59, 57, 55, 60),
#' D=c(35, 24, 44, 83, 47),
#' E=c(41, 52, 46, 50, 65))
#' nameCol <- "A"
#' refCol <- "B"
#'  for(i in names(SRDInput)){ SRDInput <- rSRD:::utilsCalculateRank(SRDInput,i)}
#' rSRD::utilsCalculateDistance(SRDInput,nameCol,refCol)
utilsCalculateDistance <- function(df,nameCol,refCol){
  firstCol <-df[paste(nameCol,"Rank",sep="_")]
  secondCol <- df[paste(refCol,"Rank",sep="_")]
  absdiffCol <- abs(firstCol-secondCol)
  val <- as.numeric(unlist(absdiffCol))
  newColumnName <- paste(nameCol,"Distance",sep="_")
  df <- df %>% tibble::add_column(!!(newColumnName):=val, .after=paste(nameCol,"Rank",sep="_") )
  return(df)
}

#' @title utilsDetailedSRD
#' @name utilsDetailedSRD
#' @aliases utilsDetailedSRD
#' @export utilsDetailedSRD
#' @author Ali Tugay Sen
#' @description Detailed calculation of the SRD values including the computation of the ranking transformation. 
#' Unless there is a column specified with referenceCol the last column will always taken as the reference.
#' @param df A DataFrame.
#' @param referenceCol Optional. A string that contains a column of \code{df} which will be used as the reference column.
#' @param createRefCol Optional. Can be max, min, median, mean. Creates a new Column based on the existing \code{df} and attaches it to  \code{df} as the reference Column.
#' @return Returns a new DataFrame that shows the detailed SRD computation (ranking transformation and distance calculation). A newly added row contains the SRD values (displayed without normalization). 
#' @examples
#' SRDInput <- data.frame(
#' A=c(32, 52, 44, 44, 47),
#' B=c(73, 75, 65, 76, 70),
#' C=c(60, 59, 57, 55, 60),
#' D=c(35, 24, 44, 83, 47),
#' E=c(41, 52, 46, 50, 65))
#' rSRD::utilsDetailedSRD(SRDInput)
utilsDetailedSRD <- function (df,referenceCol,createRefCol=function(){}){
  # referenceCol is a existing Column in the dataframe that will be taken as the reference. Normally it will be always the last one
  origin_df_names <-names(df)
  
  if(missing(referenceCol) == FALSE){
    df <- df %>% relocate(all_of(referenceCol), .after= last_col())
    origin_df_names <- origin_df_names[origin_df_names != referenceCol]
    origin_df_names <- c(origin_df_names,referenceCol)
    
  }
  if (hasArg(createRefCol)) {
    df <- utilsCreateReference(df,createRefCol)
    origin_df_names <- c(origin_df_names, tail(names(df),n=1))
  }
  refCol <- tail(origin_df_names, n=1)
  
  for(i in origin_df_names){ df <- utilsCalculateRank(df,i)}
  for( i in  origin_df_names[1:length(origin_df_names)-1]){ df <- utilsCalculateDistance(df,i,refCol)}
  sumDF <- df  %>% janitor::adorn_totals(,,, "-", contains("_Distance")) %>% janitor::untabyl()
  return(sumDF)
}

#' @title utilsDetailedSRDNoChars
#' @name utilsDetailedSRDNoChars
#' @aliases utilsDetailedSRDNoChars
#' @export utilsDetailedSRDNoChars
#' @author Ali Tugay Sen
#' @description Detailed calculation of the SRD values including the computation of the ranking transformation. 
#' Unless there is a column specified with referenceCol the last column will always taken as the reference. 
#' In this variant unused variables will not be converted to chars.
#' @param df A DataFrame.
#' @param referenceCol Optional. A string that contains a column of \code{df} which will be used as the reference column.
#' @param createRefCol Optional. Can be max, min, median, mean. Creates a new Column based on the existing \code{df} and attaches it to  \code{df} as the reference Column.
#' @return Returns a new DataFrame that shows the detailed SRD computation (ranking transformation and distance calculation). A newly added row contains the SRD values (displayed without normalization). 
#' @examples
#' SRDInput <- data.frame(
#' A=c(32, 52, 44, 44, 47),
#' B=c(73, 75, 65, 76, 70),
#' C=c(60, 59, 57, 55, 60),
#' D=c(35, 24, 44, 83, 47),
#' E=c(41, 52, 46, 50, 65))
#' rSRD::utilsDetailedSRDNoChars(SRDInput)
utilsDetailedSRDNoChars <- function (df,referenceCol,createRefCol=function(){}){
  # referenceCol is a existing Column in the dataframe that will be taken as the reference. Normally it will be always the last one
  origin_df_names <-names(df)
  
  if(missing(referenceCol) == FALSE){
    df <- df %>% relocate(all_of(referenceCol), .after= last_col())
    origin_df_names <- origin_df_names[origin_df_names != referenceCol]
    origin_df_names <- c(origin_df_names,referenceCol)
    
  }
  if (hasArg(createRefCol)) {
    df <- utilsCreateReference(df,createRefCol)
    origin_df_names <- c(origin_df_names, tail(names(df),n=1))
  }
  refCol <- tail(origin_df_names, n=1)
  
  for(i in origin_df_names){ df <- utilsCalculateRank(df,i)}
  for( i in  origin_df_names[1:length(origin_df_names)-1]){ df <- utilsCalculateDistance(df,i,refCol)}
  sumDF <- df  %>% janitor::adorn_totals(,,, name = NA, fill=NA, contains("_Distance")) %>% janitor::untabyl() #removed as it converts the unused variables to char
  #return(sumDF) - changed to df
  #a<-c() # needed to store the sums
  #b<-c() # needed to store the normalized SRD values
  #for(i in seq(3, ncol(df)-2, 3)) { 
    #a[i] <- sum(df[,i])
    #b[i] <- a[i]/utilsMaxSRD(nrow(df))*100
  #}
  #sumDF<-rbind(df,a) # creates the final SRD table BUT all variables are numeric and has the normalized SRD values in its last row. Problem: empty cells are marked as NA (not sure if it is a real problem though)
  return(sumDF) # returns the df before the janitor function
}

#' @title utilsTieProbability
#' @name utilsTieProbability
#' @aliases utilsTieProbability
#' @export utilsTieProbability
#' @author Ali Tugay Sen, Linus Olsson \email{linusmeol@@gmail.com}
#' @description  Calculates the tie probability for a given vector. The tie probability is defined as the number of consecutive tied component-pairs \emph{in the sorted vector} divided by the size of the vector minus 1.
#' @param x A vector.
#' @return Returns the tie probability as a numeric value.
#' @examples
#' x <-c(1,2,4,4,5,5,6)
#' rSRD::utilsTieProbability(x)
utilsTieProbability <- function(x){
  sorted_x <- sort(x)
  ties <-0
  for(i in 2:length(sorted_x)){
    if(sorted_x[i] == sorted_x[i-1] ) {
      ties<-ties+1
    }
  } 
  return (ties/(length(sorted_x) - 1))
}


#' @title calculateCrossValidation
#' @name calculateCrossValidation
#' @aliases calculateCrossValidation
#' @export calculateCrossValidation
#' @author Balázs R. Sziklai \email{sziklai.balazs@@krtk.hu}, Linus Olsson \email{linusmeol@@gmail.com}, Jochen Staudacher \email{jochen.staudacher@@hs-kempten.de}
#' @description R interface to test whether the rankings induced by the columns come from the same 
#' distribution. If the number of folds and the test method are not specified, the default is 
#' the 8-fold Wilcoxon test combined with cross-validation. If the number of rows is less than 8, 
#' leave-one-out cross-validation is applied. Columns are ordered based on the SRD values of the 
#' different folds, then each consecutive column-pairs are tested. Test statistics for Alpaydin test 
#' follows F distribution with df1=2k, df2=k degrees of freedom. Dietterich test statistics follow 
#' t-distribution with k degrees of freedom (two-tailed). Wilcoxon test statistics is calculated 
#' as the absolute value of the difference of the sum of the positive ranks (W+) and sum of the 
#' negative ranks (W-). The distribution for this test statistics can be derived from the Wilcoxon 
#' signed rank distribution. For more information about the cross-validation process see 
#' Sziklai, Baranyi and Héberger (2021). 
#' @param data_matrix A DataFrame.
#' @param method A string specifying the method. The methods "Wilcoxon", "Alpaydin" and "Dietterich" are available.
#' @param number_of_folds The number of folds used in the cross validation. Ranges between 5 to 10.
#' @param precision The precision used for the the ranking matrix transformation.
#' @param output_to_file Boolean flag to enable file output.
#' @return A List containing
#' \itemize{
#' \item a new column order sorted by the median of the SRD values computed on the different folds
#' \item a vector of test statistics corresponding to each consecutive column pairs
#' \item a vector indicating the test statistics' statistical significance 
#' \item the SRD values of different folds and 
#' \item additional data needed for the plotCrossValidation function. 
#' } 
#' @references Sziklai, Balázs R., Máté Baranyi, and Károly Héberger (2021). 
#' "Testing Cross-Validation Variants in Ranking Environments",  
#' arXiv preprint arXiv:2105.11939 (2021).
#' @examples
#' df <- data.frame(
#' Sol_1=c(7, 6, 5, 4, 3, 2, 1),
#' Sol_2=c(1, 2, 3, 4, 5, 7, 6),
#' Sol_3=c(1, 2, 3, 4, 7, 5, 6),
#' Ref=c(1, 2, 3, 4, 5, 6, 7))
#' 
#' calculateCrossValidation(df, output_to_file = FALSE)
calculateCrossValidation <- function(data_matrix,  method = "Wilcoxon", number_of_folds = 8,  precision = 5,  output_to_file = TRUE){
  cv_result <- calculateCrossValidationAdapter(data_matrix, method, number_of_folds, precision, output_to_file)
  c_names_ordered <- colnames(data_matrix)
  
  c_order <- cv_result$new_column_order_based_on_folds
  
  c_names <- c()
  
  for (i in 1:(ncol(data_matrix)-1)) {
    c_names <- c(c_names, c_names_ordered[c_order[i]])
  }
  
  f_names <- sprintf("fold_%s",seq(1:nrow(cv_result$SRD_values_of_different_folds)))
  
  colnames(cv_result$SRD_values_of_different_folds) <- c_names
  rownames(cv_result$SRD_values_of_different_folds) <- f_names
  colnames(cv_result$boxplot_values) <- c_names
  rownames(cv_result$boxplot_values) <- c("min", "xx1", "q1", "median", "q3", "xx19", "max")
  
  # Determine statistical significance for output in R
  
  p_val_005 = c(0,0,0,0,0,0)
  p_val_010 = c(0,0,0,0,0,0)
  size_dm = dim(data_matrix)[1]
  
  if (method =="Wilcoxon"){
    p_val_005 = c(15,21,24,30,35,39)
    p_val_010 = c(15,17,22,26,29,35)
    k_fold = min(floor(size_dm), number_of_folds)
  } 
  
  if (method =="Alpaydin"){
    p_val_005 = c(4.735,4.000,3.529,3.202,2.960,2.774)
    p_val_010 = c(3.297,2.905,2.643,2.455,2.312,2.201)
    k_fold = min(floor(size_dm * (size_dm-1)/2), number_of_folds)
  } 
  
  if (method =="Dietterich"){
    p_val_005 = c(2.571,2.447,2.365,2.306,2.262,2.228)
    p_val_010 = c(2.015,1.943,1.895,1.860,1.833,1.812)
    k_fold = min(floor(size_dm * (size_dm-1)/2), number_of_folds)
  } 
  
  test_statistics = cv_result$test_statistics
  statSignifance = character(length(test_statistics))
  
  notAvailString = "n.a."
  less005String = "(p<0.05*)"
  less01String = "(p<0.1)"
  notSignifString = "n.s."
  
  
  
  for (i in (1:length(test_statistics)))
  {
    if (number_of_folds < 5 | number_of_folds>10)
    {
      statSignifance[i] = notAvailString
    }
    else if (abs(test_statistics[i]) >= p_val_005[k_fold - 4])
    {
      statSignifance[i] = less005String
    }
    else if (abs(test_statistics[i]) >= p_val_010[k_fold - 4])
    {
      statSignifance[i] = less01String
    }
    else
    {
      statSignifance[i] = notSignifString
    }
    
  }
  
  cv_output = list()
  cv_output$new_column_order_based_on_folds = cv_result$new_column_order_based_on_folds
  cv_output$test_statistics = cv_result$test_statistics
  cv_output$statistical_significance = statSignifance
  cv_output$SRD_values_of_different_folds = cv_result$SRD_values_of_different_folds
  cv_output$boxplot_values = cv_result$boxplot_values
  return(cv_output)
}