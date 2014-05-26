source("requirements.R")
source("functions.R")
source("data.R")

feesDataSummaryByCredit <- function(df = data.list$raw, credits=9) {
  
  # Create a simplified data frame with computed columns for calculated 
  # fees by credit/block
  summary.category.df <- data.frame(
    Category = df$Category,
    AY = df$AY,
    Semester.Fees.UF = ifelse(df$Fee.Rate=="Semester",df$UF,df$UF*credits),
    Semester.Fees.FSU = ifelse(df$Fee.Rate=="Semester",df$FSU,df$FSU*credits),
    Semester.Fees.FAMU = ifelse(df$Fee.Rate=="Semester",df$FAMU,df$FAMU*credits),
    Semester.Fees.USF.T = ifelse(df$Fee.Rate=="Semester",df$USF.T,df$USF.T*credits)
  )
  
  # Create a summary data.frame by academic year
  summary.ay.df <- ddply(summary.category.df,
        c("AY"),
        function(x) colSums(x[,-c(1,2)],na.rm=T)
  )
  
  # Melt it for ggplot2
  summary.ay.df.melt <- melt(
    summary.ay.df, 
    id.vars=c("AY"),
    variable.name = "University",
    value.name = "Total.Fees"
  )

  levels(summary.ay.df.melt$University)[levels(summary.ay.df.melt$University)=="Semester.Fees.UF"] <- "UF"
  levels(summary.ay.df.melt$University)[levels(summary.ay.df.melt$University)=="Semester.Fees.FSU"] <- "FSU"
  levels(summary.ay.df.melt$University)[levels(summary.ay.df.melt$University)=="Semester.Fees.FAMU"] <- "FAMU"
  levels(summary.ay.df.melt$University)[levels(summary.ay.df.melt$University)=="Semester.Fees.USF.T"] <- "USF"
  
  # Store everything we did in a list object
  data.list <- list(
    summaryByCategory = summary.category.df,
    summaryByAY = summary.ay.df,
    summaryByAY.melt = summary.ay.df.melt
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





