#install.packages("tidyverse")
library(tidyverse)

#-Working directory-
wdir<-"/Users/tania/source/itam/maestria/us_vote_counties/"
setwd(wdir)

#-Reading data-
county_facts <- read.table(paste(wdir,"data/county_facts.csv",sep=""),
                           header=TRUE,sep=",")
cf_column_names <- read.table(paste(wdir,"data/county_facts_dictionary.csv",sep=""),
                              header=TRUE,sep=",")
election_results <- read.table(paste(wdir,"data/2016_US_County_Level_Presidential_Results.csv",sep=""),
                               header=TRUE,sep=",") 

#-Transform county facts data-#
remove <- c("PST040210","POP010210","HSG010214","HSD410213","INC910213",
            "RTN130207","LND110210","BZA110213","NES010213","SBO001207",
            "RTN130207","LND110210","BPS030214")
remove_from_var_selection <- c("RHI525214","HSG445213","BZA115213","SBO515207",
                               "SEX255214","LFE305213","SBO115207","BZA010213",
                               "AFN120207")
clean_cf <- county_facts[county_facts$fips%%1000!=0 & county_facts$state_abbreviation!="AK", 
                         !(names(county_facts) %in% c(remove,remove_from_var_selection))]
election_results$per_election_difference <- (election_results$per_gop - election_results$per_dem)*100
response_variables <- election_results[,c("combined_fips","per_election_difference")]
colnames(response_variables)[1] <- "fips"
full_data <- inner_join(clean_cf,response_variables,by="fips")
#normalized <- full_data %>% mutate_each_(funs(scale),
 #                                   vars=c("PST045214"))
divideby1k <- c("INC110213")
divideby100k <- c("MAN450207","WTN220207","RTN131207")
applylog <- c("POP060210", "VET605213","HSG495213","PST045214")
full_data[divideby1k] <- full_data[divideby1k]/1000
full_data[divideby100k] <- full_data[divideby100k]/100000
full_data[applylog] <- log(full_data[applylog])
indx <- apply(full_data, 2, function(x) any(is.na(x) | is.infinite(x)))
#-Run Model-#
library(R2jags)
n<-nrow(full_data)

#-define data and inits-#
data<-list("n"=n,"y"=full_data$per_election_difference,"x"=as.matrix(full_data[4:34]))
inits <- function(){list(beta=rep(0,32),tau=1)}
plot(full_data$AFN120207,full_data$per_election_difference)
parameters<-c("beta","tau")

ej6.sim<-jags(data,inits,parameters,
               model.file=paste(wdir,"src/elections.jags",sep=""),
               n.iter=5000,n.chains=1,n.burnin=500,n.thin=1)

out.sum<-ej6.sim$BUGSoutput$summary
print(out.sum)
tail(out.sum)
names(out.sum)
out.sum

#Usefull to get which variable belongs to each beta (remember to substract 1 from index)
#names(full_data[4:36])[24]

