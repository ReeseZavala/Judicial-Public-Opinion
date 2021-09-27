rm(list = ls())
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)


setwd("C:\\Users\\rzava\\Box Sync\\Spring 2021\\Comp Judicial\\Paper\\Data\\")


JustDat <- read.csv("IR_Justices.csv")

## Limiting to criminal cases
smallDat <- JustDat %>%
  filter(grepl("Criminal", LIssue)) 

smallDat <- smallDat %>%
  mutate(ProGov = ifelse((petitionerType1 == "Government" & winnerJustice == "Petitioner") |
                         (petitionerType1 != "Government" & winnerJustice == "Respondent") ,
                          1, 0)) %>%
  mutate(year = substr(dateFinalDecision, nchar(dateFinalDecision)-1, nchar(dateFinalDecision))) %>%
  group_by(Name) %>%
  mutate(N =  row_number()) %>%
  mutate(NaiveScore = round(cumsum(ProGov)/N, 3)) %>%
  mutate(DumbScore = round(sum(ProGov)/max(N), 3)) 
  

sumSimpDat <- smallDat %>%
  group_by(Name) %>%
  filter(row_number() == 1)

sumSimpDat <- as.data.frame(cbind(sumSimpDat$Name, sumSimpDat$DumbScore))
colnames(sumSimpDat) <- c("Name", "DumbScore")
Med <- median(as.numeric(sumSimpDat$DumbScore))
sd <- sd(as.numeric(sumSimpDat$DumbScore))

ggplot(data=sumSimpDat, aes(x=as.numeric(DumbScore), y=Name, group=Name, color=Name)) +
      geom_point() +
      # theme(axis.text.x =element_blank(),
      #   axis.ticks.x = element_blank()) +
      xlab("Score") + 
      geom_vline(xintercept = Med, color = "grey", linetype = 
                 "dotted") +
      theme(legend.position = "none")


length(which(abs(as.numeric(sumSimpDat$DumbScore) - Med) < sd^2))


# length(unique(smallDat$dateFinalDecision))
# 
# cbind(smallDat$petitionerType1[15], smallDat$winnerJustice[15], smallDat$ProGov[15])

###########################################################################################
##################################     ELECTIONS       ####################################

elec <- read.csv("IR_Elections.csv")

elec$election_year <- substr(elec$election_date, 1,4)
elec <- elec %>%
  filter(as.numeric(election_year) > "2000") %>%
  filter(seats != 0) %>%
  mutate(Right = ifelse(is.na(left_right), NA, ifelse(left_right > 5, 1,0))) %>%
  group_by(election_year) 

## There is a problem as 2019 had two elections, so the vote totals are off
## Need to split it into two years
## First 11 rows are April, last 9 are September

# get row numbers
rs <- which(elec$election_year == 2019)
for (i in 12:20){
  elec$election_year[rs[i]] <- 2019.5 
}


## Creating data frame for simple right-left votes
year <- unique(elec$election_date)
right <- NULL
for (k in year) {
  right <- c(right, sum(elec$vote_share[elec$election_date == k & elec$Right == 1], na.rm = T))
}

left <- NULL
for (k in year) {
  left <- c(left, sum(elec$vote_share[elec$election_date == k & elec$Right == 0], na.rm = T))
}



rlVote <- as.data.frame(cbind(right, left))
rownames(rlVote) <- year


plot(x = rownames(rlVote), y = rlVote$right, col = "red", type = "l", ylim = c(10,80), xlab = "Election Year", ylab = "Vote Share")
lines(x = rownames(rlVote), y = rlVote$left, col = "green", type = "l")
legend(x = 2016.65, y = 65, col = c("red", "green"), lty = 1, legend = c("Right", "Left"))


# ggplot(data = elec, aes(x = election_date, y = vote_share, group = party_name_english, color = party_name_english)) +
  # geom_line()


## Now to add DeltaVote stuff
lag <- dplyr::lag

deltaVote <- rlVote %>%
  mutate(election_date = as.Date(year, format = "%Y-%m-%d")) %>%
  mutate(year = unique(elec$election_year)) %>%
  mutate(lagR = lag(right, n = 1, order_by = year)) %>%
  mutate(lagL = lag(left, n = 1, order_by = year)) %>%
  mutate(deltaR = right - lagR) %>%
  mutate(deltaL = left - lagL) %>%
  mutate(Rmarg = right - left, Rmarg_lag = lag(Rmarg, order_by = year)) %>%
  mutate(delta_marg = Rmarg - Rmarg_lag)


## The new plan is to append smallDat with deltaRight info
#  Need to code delta for most proximate election result
#  First subset data down to interesting variables
justUs <-  c("outcomeJustice", "Name", "Title", "Seniority", "Religion", "caseId", "winnerJustice", "dateFinalDecision", "ProGov", "DumbScore") 

## Need to add Prox/Prev marg and deltaR/L vars
jComp <- smallDat %>%
  select(which(colnames(smallDat) %in% justUs)) %>%
  mutate(dateFinalDecision = as.Date(dateFinalDecision, format = "%m/%d/%y")) 

## This doesn't work
#  %>%
#  mutate(Prox_marg = deltaVote$delta_marg[which.min(abs( dateFinalDecision - deltaVote$election_date ))])
#  Gotta do for loop

prox_marg <- NULL
prev_marg <- NULL
prox_deltR <- NULL
prox_deltL <- NULL
prev_deltR <- NULL
prev_deltL <- NULL
for (i in 1:nrow(jComp)){
  ## Establish which election is closest using n
  n <- which.min(abs(jComp$dateFinalDecision[i] - deltaVote$election_date))
  ## Feed in closest result
  prox_marg <- c(prox_marg, deltaVote$delta_marg[n])
  prox_deltR <- c(prox_deltR, deltaVote$deltaR[n])
  prox_deltL <- c(prox_deltL, deltaVote$deltaL[n])
  ## If closest result is in the past, use it, otherwise use the result before
  ifelse(deltaVote$election_date[n] - jComp$dateFinalDecision[i] < 0,  prev_marg[i] <- deltaVote$delta_marg[n], 
                                                                       prev_marg[i] <- deltaVote$delta_marg[n-1])
  
  ifelse(deltaVote$election_date[n] - jComp$dateFinalDecision[i] < 0,  prev_deltR[i] <- deltaVote$deltaR[n], 
                                                                       prev_deltR[i] <- deltaVote$deltaR[n-1])
  
  ifelse(deltaVote$election_date[n] - jComp$dateFinalDecision[i] < 0,  prev_deltL[i] <- deltaVote$deltaL[n], 
                                                                       prev_deltL[i] <- deltaVote$deltaL[n-1])
}  

jComp$prox_marg <- prox_marg
jComp$prev_marg <- prev_marg
jComp$prox_deltR <- prox_deltR
jComp$prox_deltL <- prox_deltL
jComp$prev_deltR <- prev_deltR
jComp$prev_deltL <- prev_deltL

## Coding Pivotality

smallDat <- smallDat %>%
  mutate(Pivot_wide = ifelse(abs(as.numeric(DumbScore) - Med) < sd, 1, 0)) %>%
  mutate(Pivot_medi = ifelse(abs(as.numeric(DumbScore) - Med) < sd/2, 1, 0)) %>%
  mutate(Pivot_narr = ifelse(abs(as.numeric(DumbScore) - Med) < sd^2, 1, 0))

jComp$Pivot_wide <- smallDat$Pivot_wide
jComp$Pivot_medi <- smallDat$Pivot_medi 
jComp$Pivot_narr <- smallDat$Pivot_narr

write.csv(jComp, "JusticexElection_Israel.csv")

## Read in US Supreme Court Data
usDat <- read.csv("C:\\Users\\rzava\\Box Sync\\Third Year Paper\\Pivot\\sumData\\USSC.csv")



