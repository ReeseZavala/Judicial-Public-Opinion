rm(list = ls())
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)


setwd("C:\\Users\\rzava\\Box Sync\\Spring 2021\\Comp Judicial\\Paper\\Data\\")

maDat <- read.csv("JusticexElection_Israel.csv")
maDat <- maDat[2:ncol(maDat)]

## What the fuck do these results mean
#  Pivot
stargazer(lm(ProGov ~ as.factor(Religion) + Seniority + Pivot_wide + prev_marg + Pivot_wide*prev_marg, data = maDat))
#  Seniority
stargazer(lm(ProGov ~ as.factor(Religion) + Seniority + Pivot_wide + prev_marg + Pivot_wide*prev_marg + Seniority*prev_marg, data = maDat))


length(unique(maDat$caseId))





