#Spider tally

### Load necessary packages and read in data ###
library(tidyverse)
library(dplyr)
library(ggplot2)
vidsAnalyzed <- read.csv("C:\\Users\\brook\\Downloads\\SpiderGaitRelativeStridePhases_new.csv", header = T)

##### Subsequent code tallies and plots intact, first and second two limb autotomy TRIALS: see further down for tallies of STRIDES #####

### Sum total intact analyzed trials, plot 
vidsAnalyzed.intact <- subset(vidsAnalyzed, select = -c(spider.1,trialid.1,spider.2,trialid.2)) %>% 
  filter(GeneralTreat == "Intact") %>% 
    select(spider, trialid) %>% 
      group_by(spider) %>% 
        unique(incomparables = FALSE, fromLast = FALSE) %>%
          summarize(n=n()) %>% 
            group_by(spider) -> tidyVids.intact

totalAnalyzedIntact <- tidyVids.intact[, 1:2] %>% 
  separate(spider, into = c("spiderID", "date"), sep = '_') %>% 
  select(spiderID, n) %>% group_by(spiderID) %>% mutate(total = sum(n)) %>% filter(!duplicated(spiderID))

ggplot(data = totalAnalyzedIntact) + 
  geom_point(mapping = aes(x = spiderID, y = total)) 

### Sum total first autotomy two limb analyzed trials, plot
vidsAnalyzed.first <- subset(vidsAnalyzed, select = -c(spider.1,trialid.1,spider.2,trialid.2)) %>% 
  filter(GeneralTreat == "2Autotomy1") %>% 
  select(spider, trialid) %>% 
  group_by(spider) %>% 
  unique(incomparables = FALSE, fromLast = FALSE) %>%
  summarize(n=n()) %>% 
  group_by(spider) -> tidyVids.first

totalAnalyzedFirst <- tidyVids.first[, 1:2] %>% 
  separate(spider, into = c("spiderID", "date"), sep = '_') %>% 
  select(spiderID, n) %>% group_by(spiderID) %>% mutate(total = sum(n)) %>% filter(!duplicated(spiderID))


ggplot(data = totalAnalyzedFirst) + 
  geom_point(mapping = aes(x = spiderID, y = total)) 


### Sum total second autotomy two limb analyzed trials, plot
vidsAnalyzed.second <- subset(vidsAnalyzed, select = -c(spider.1,trialid.1,spider.2,trialid.2)) %>% 
  filter(GeneralTreat == "2Autotomy2") %>% 
  select(spider, trialid) %>% 
  group_by(spider) %>% 
  unique(incomparables = FALSE, fromLast = FALSE) %>%
  summarize(n=n()) %>% 
  group_by(spider) -> tidyVids.second

totalAnalyzedSecond <- tidyVids.second[2:7, 1:2] %>% 
  separate(spider, into = c("spiderID", "date"), sep = '_') %>% 
  select(spiderID, n) %>% group_by(spiderID) %>% mutate(total = sum(n)) %>% filter(!duplicated(spiderID)) 

# Problem with DP20_2017-10-20 trial 6 and I'm not sure why. Temporarily removing that line from results. 
ggplot(data = totalAnalyzedSecond) + 
  geom_point(mapping = aes(x = spiderID, y = total)) 

# Plot all three distributions on top of each other
colors <- c("Intact" = "red", "First Autotomy" = "blue", "Second Autotomy" = "orange")
ggplot(data = totalAnalyzedIntact) + 
  geom_point(mapping = aes(x = spiderID, y = total), color = "red") +
  geom_point(data = totalAnalyzedFirst, mapping = aes(x = spiderID, y = total), color = "blue") + 
  geom_point(data = totalAnalyzedSecond, mapping = aes(x = spiderID, y = total), color = "orange") + 
  labs(x = "Spider ID",
       y = "Total videos analyzed",
       color = "Legend") +
  scale_color_manual(values = colors)

### Plot total analyzed trials for each treatment in bar plot
totalIntactTrials <- sum(totalAnalyzedIntact$total)
totalFirstTrials <- sum(totalAnalyzedFirst$total)
totalSecondTrials <- sum(totalAnalyzedSecond$total)
allAnalyzedTrials = c(totalIntactTrials, totalFirstTrials, totalSecondTrials)
treatments = c("Intact", "First Autotomy", "Second Autotomy")
totals = as.data.frame(matrix(c(treatments, allAnalyzedTrials), nrow = 3, ncol = 2))

ggplot(data = totals, aes(x = treatments, y = allAnalyzedTrials)) + 
  geom_col(aes(fill = c("red", "blue", "green")), show.legend = FALSE)


##### Subsequent code tallies and plots intact, first and second two limb autotomy STRIDES #####

### Sum total intact analyzed strides, plot 
vidsAnalyzed.intact.str <- subset(vidsAnalyzed, select = -c(spider.1,trialid.1,spider.2,trialid.2)) %>% 
  filter(GeneralTreat == "Intact") %>%
  select(spider, trialid, startsample, endsample) %>%
  unite("spiderDateID", spider, trialid, sep = "_") %>% 
  group_by(spiderDateID) %>%
  mutate(strides = max(endsample) - min(startsample)) -> tidyVids.intact.str

totalAnalyzedIntactStr <- tidyVids.intact.str[, c(1,4)] %>% 
  unique(incomparables = FALSE, fromLast = FALSE) %>%
  separate(spiderDateID, into = c("spiderID", "date"), sep = '_') %>% 
  select(spiderID, strides) %>% 
  group_by(spiderID) %>% 
  mutate(totalStrides = sum(strides)) %>% 
  filter(!duplicated(spiderID)) %>% 
  select(spiderID, totalStrides)

ggplot(data = totalAnalyzedIntactStr) + 
  geom_point(mapping = aes(x = spiderID, y = totalStrides)) 


### Sum total first autotomy two limb analyzed strides, plot 
vidsAnalyzed.first.str <- subset(vidsAnalyzed, select = -c(spider.1,trialid.1,spider.2,trialid.2)) %>% 
  filter(GeneralTreat == "2Autotomy1") %>%
  select(spider, trialid, startsample, endsample) %>%
  unite("spiderDateID", spider, trialid, sep = "_") %>% 
  group_by(spiderDateID) %>%
  mutate(strides = max(endsample) - min(startsample)) -> tidyVids.first.str

totalAnalyzedFirstStr <- tidyVids.first.str[, c(1,4)] %>% 
  unique(incomparables = FALSE, fromLast = FALSE) %>%
  separate(spiderDateID, into = c("spiderID", "date"), sep = '_') %>% 
  select(spiderID, strides) %>% 
  group_by(spiderID) %>% 
  mutate(totalStrides = sum(strides)) %>% 
  filter(!duplicated(spiderID)) %>% 
  select(spiderID, totalStrides)

ggplot(data = totalAnalyzedFirstStr) + 
  geom_point(mapping = aes(x = spiderID, y = totalStrides)) 



### Sum total second autotomy two limb analyzed strides, plot 
vidsAnalyzed.second.str <- subset(vidsAnalyzed, select = -c(spider.1,trialid.1,spider.2,trialid.2)) %>% 
  filter(GeneralTreat == "2Autotomy2") %>%
  select(spider, trialid, startsample, endsample) %>%
  unite("spiderDateID", spider, trialid, sep = "_") %>% 
  group_by(spiderDateID) %>%
  mutate(strides = max(endsample) - min(startsample)) -> tidyVids.second.str

totalAnalyzedSecondStr <- tidyVids.second.str[, c(1,4)] %>% 
  unique(incomparables = FALSE, fromLast = FALSE) %>%
  separate(spiderDateID, into = c("spiderID", "date"), sep = '_') %>% 
  select(spiderID, strides) %>% 
  group_by(spiderID) %>% 
  mutate(totalStrides = sum(strides)) %>% 
  filter(!duplicated(spiderID)) %>% 
  select(spiderID, totalStrides)

ggplot(data = totalAnalyzedSecondStr) + 
  geom_point(mapping = aes(x = spiderID, y = totalStrides)) 


### Plot number of strides per spider by treatment
ggplot(data = totalAnalyzedSecondStr) + 
  geom_point(mapping = aes(x = spiderID, y = totalStrides), color = "purple") + 
  geom_point(data = totalAnalyzedFirstStr, mapping = aes(x = spiderID, y = totalStrides), color = "red") + 
  geom_point(data = totalAnalyzedIntactStr, mapping = aes(x = spiderID, y = totalStrides), color = "green")

autotomy2 <- mutate(totalAnalyzedSecondStr, Treatment = "Second Autotomy")  
autotomy1 <- mutate(totalAnalyzedFirstStr, Treatment = "First Autotomy")  
intact <- mutate(totalAnalyzedIntactStr, Treatment = "Intact")  

allStrides <- full_join(autotomy2, autotomy1) %>% full_join(intact)

ggplot(data = allStrides) + 
  geom_point(mapping = aes(x = spiderID, y = totalStrides, color = Treatment))
