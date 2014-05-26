source("requirements.R")
source("functions.R")
source("data.R")

feesDataSummaryByUniversity <- function(df = data.list$raw, credits=9) {
  
  # Create a simplified data frame with computed columns for calculated 
  # fees by credit/block
  summary.category.df <- data.frame(
    Category = df$Category,
    Residency = df$Resident,
    AY = df$AY,
    Semester.Fees.UF = ifelse(df$Fee.Rate=="Semester",df$UF,df$UF*credits),
    Semester.Fees.FSU = ifelse(df$Fee.Rate=="Semester",df$FSU,df$FSU*credits),
    Semester.Fees.FAMU = ifelse(df$Fee.Rate=="Semester",df$FAMU,df$FAMU*credits),
    Semester.Fees.USF.T = ifelse(df$Fee.Rate=="Semester",df$USF.T,df$USF.T*credits)
  )

  summary.df.list <- list(
    Resident = subset(summary.category.df, Residency == "Both" | Residency == "Resident"),
    Non.Resident = subset(summary.category.df, Residency == "Both" | Residency == "Non-Resident")
  )
  
  # Create a summary data.frame by academic year
  summary.ay.df.list = NULL
  for(i in 1:length(summary.df.list)) {
    summary.ay.df.list[[i]] = ddply(summary.df.list[[i]], c("AY"), function(x) colSums(x[,-c(1,2,3)],na.rm=T))
  }
  names(summary.ay.df.list) <- names(summary.df.list)
     
  # Melt it for ggplot2
  summary.ay.df.list.melt = NULL
  for(i in 1:length(summary.ay.df.list)) {
    summary.ay.df.list.melt[[i]] = melt(summary.ay.df.list[[i]],
      id.vars=c("AY"),
      variable.name = "University",
      value.name = "Total.Fees"
    )
  }
  names(summary.ay.df.list.melt) <- names(summary.ay.df.list)
  
  for (i in 1:length(summary.ay.df.list.melt)) {
    levels(summary.ay.df.list.melt[[i]]["University"])[levels(summary.ay.df.list.melt[[i]]["University"])=="Semester.Fees.UF"] <- "UF"
    levels(summary.ay.df.list.melt[[i]]["University"])[levels(summary.ay.df.list.melt[[i]]["University"])=="Semester.Fees.FSU"] <- "FSU"
    levels(summary.ay.df.list.melt[[i]]["University"])[levels(summary.ay.df.list.melt[[i]]["University"])=="Semester.Fees.FAMU"] <- "FAMU"
    levels(summary.ay.df.list.melt[[i]]["University"])[levels(summary.ay.df.list.melt[[i]]["University"])=="Semester.Fees.USF.T"] <- "USF"
  }
  
  # Store everything we did in a list object
  data.list <- list(
    summaryByCategory = summary.category.df,
    summaryByResidency = summary.df.list,
    summaryByAY = summary.ay.df.list,
    summaryByAY.melt = summary.ay.df.list.melt
  )
  
  return(data.list)
}

feesDataSummaryByCategory <- function(df = data.list$raw, credits=9) {
  
  # Create a simplified data frame with computed columns for calculated 
  # fees by credit/block
  summary.category.df <- data.frame(
    Category = df$Category,
    Residency = df$Resident,
    AY = df$AY,
    Semester.Fees.UF = ifelse(df$Fee.Rate=="Semester",df$UF,df$UF*credits),
    Semester.Fees.FSU = ifelse(df$Fee.Rate=="Semester",df$FSU,df$FSU*credits),
    Semester.Fees.FAMU = ifelse(df$Fee.Rate=="Semester",df$FAMU,df$FAMU*credits),
    Semester.Fees.USF.T = ifelse(df$Fee.Rate=="Semester",df$USF.T,df$USF.T*credits)
  )
  
  
  summary.category.df$Semester.Fees.Mean = rowMeans(cbind(summary.category.df$Semester.Fees.UF,
                                                          summary.category.df$Semester.Fees.FSU,
                                                          summary.category.df$Semester.Fees.FAMU,
                                                          summary.category.df$Semester.Fees.USF.T))
  
  
  summary.df.list <- list(
    Resident = subset(summary.category.df, Residency == "Both" | Residency == "Resident"),
    Non.Resident = subset(summary.category.df, Residency == "Both" | Residency == "Non-Resident")
  )
  
#   # Create a summary data.frame by academic year
#   summary.ay.df.list = NULL
#   for(i in 1:length(summary.df.list)) {
#     summary.ay.df.list[[i]] = ddply(summary.df.list[[i]], c("AY","Category"), function(x) colSums(x[,-c(1,2,3)],na.rm=T))
#   }
#   names(summary.ay.df.list) <- names(summary.df.list)
  
  summary.ay.df.list = NULL
  for(i in 1:length(summary.df.list)) {
    summary.ay.df.list[[i]] = ddply(summary.df.list[[i]], c("AY","Category"), function(x) colSums(x[,-c(1,2,3)],na.rm=T))
  }
  names(summary.ay.df.list) <- names(summary.df.list)
  
  # Melt it for ggplot2
  summary.ay.df.list.melt = NULL
  for(i in 1:length(summary.ay.df.list)) {
    summary.ay.df.list.melt[[i]] = melt(summary.ay.df.list[[i]],
                                        id.vars=c("AY","Category"),
                                        measure.vars = c("Semester.Fees.Mean"),
                                        value.name = "Mean.Fees"
    )
  }
  names(summary.ay.df.list.melt) <- names(summary.ay.df.list)
  

  # Store everything we did in a list object
  data.list <- list(
    summaryByCategory = summary.category.df,
    summaryByResidency = summary.df.list,
    summaryByAY = summary.ay.df.list,
    summaryByAY.melt = summary.ay.df.list.melt
  )
  
  return(data.list)
}

plotFeesByUniversity <- function(credits=9, df=data.list$raw, residency="Resident") {

  # Get fees data
  feesDataSummary.df.list <- feesDataSummaryByUniversity(credits = credits, df=df)
  
  # Plot depending on residency/non-residency
  if(residency=="Resident") {
    ggplot(data=feesDataSummary.df.list$summaryByAY.melt$Resident, 
           aes(
             x=AY, 
             y=Total.Fees, 
             group=University,
             colour=University
           )
    ) + geom_line() + geom_point()
  } else if(residency=="Non-Resident") {
    ggplot(data=feesDataSummary.df.list$summaryByAY.melt$Non.Resident, 
           aes(
             x=AY, 
             y=Total.Fees, 
             group=University,
             colour=University
           )
    ) + geom_line() + geom_point()
  }
}

plotFeesByCategory <- function(credits=9, df=data.list$raw, residency="Resident") {
  
  # Get fees data
  feesDataSummary.df.list <- feesDataSummaryByCategory(credits = credits, df=df)
  
  # Plot depending on residency/non-residency
  if(residency=="Resident") {
    ggplot(data=feesDataSummary.df.list$summaryByAY.melt$Resident, 
           aes(
             x=AY, 
             y=Mean.Fees, 
             group=Category,
             colour=Category
           )
    ) + geom_line() + geom_point()
  } else if(residency=="Non-Resident") {
    ggplot(data=feesDataSummary.df.list$summaryByAY.melt$Non.Resident, 
           aes(
             x=AY, 
             y=Mean.Fees, 
             group=Category,
             colour=Category
           )
    ) + geom_line() + geom_point()
  }
}





