#install.packages("tidyverse")
library(tidyverse)

#-Working directory-
wdir<-"/Users/tania/source/itam/maestria/us_vote_counties/"
setwd(wdir)

#-Reading data-
county_facts <- read.table(paste(wdir,"data/county_facts.csv",sep=""),header=TRUE,sep=",")
cf_column_names <- read.table(paste(wdir,"data/county_facts_dictionary.csv",sep=""),header=TRUE,sep=",")
election_results <- read.table(paste(wdir,"data/2016_US_County_Level_Presidential_Results.csv",sep=""),header=TRUE,sep=",") 

#-Transform county facts data-#
remove <- c("PST040210","POP010210","HSG010214","HSD410213","INC910213",
            "RTN130207","LND110210","BZA110213","NES010213","SBO001207",
            "RTN130207","LND110210")
clean_cf <- county_facts[county_facts$fips%%1000!=0, !(names(county_facts) %in% remove)]
election_results$per_election_difference <- (election_results$per_gop - election_results$per_dem)*100
response_variables <- election_results[,c("combined_fips","per_election_difference")]
colnames(response_variables)[1] <- "fips"
full_data <- inner_join(clean_cf,response_variables,by="fips")
#normalized <- full_data %>% mutate_each_(funs(scale),
 #                                   vars=c("PST045214"))
divideby1k <- c("INC110213","AFN120207")
divideby100k <- c("HSG445213","MAN450207","WTN220207","RTN131207")
applylog <- c("POP060210", "VET605213","HSG495213","BPS030214","PST045214")
full_data[divideby1k] <- full_data[divideby1k]/1000
full_data[divideby100k] <- full_data[divideby100k]/100000
full_data[applylog] <- log(full_data[applylog])
#-Run Model-#
library(R2jags)
n<-nrow(full_data)
plot(full_data$BPS030214,full_data$per_election_difference)

#-define data and inits-#
data<-list("n"=n,"y"=full_data$per_election_difference,"x"=as.matrix(full_data[4:44]))
inits <- function(){list(beta=rep(0,42),tau=1)}

parameters<-c("beta","tau")

ej6a.sim<-jags(data,inits,parameters,model.file=paste(wdir,"src/elections.jags",sep=""),
               n.iter=50000,n.chains=1,n.burnin=5000,n.thin=1)
