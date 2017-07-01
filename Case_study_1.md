# Beers and Breweries
Sheri Loftin  
July 1, 2017  

#Introduction 
Using two data sets that contain information about different breweries and about a variety of beers throughout the United States, we can  compare beers and breweries by state. With the code created, answers to several questions are available. These questions are proposed in the Specifications section. The answers are available below the Specifications. All files and other necessary components used to find these answers are listed under Important.
  
##Specifications
* The first task is reading in the files and create the data.frame. Then we answer the question of how many breweries are present in each state.
* The second task is manipulating the data minorly to allow the two data.frames to be merged into one data.frame based on each brewery.
* The third task answers the question "How many values are missing from each column of the merged data.frame?"
* The merged data.frame is then used to create a simpler data.frame with only the values for State, ABV (alcohol by volume), and IBU (International Bitter Units). This simplified data set is used for some of the next tasks.
* The fourth task is determining the median values by state for ABV and IBU. Then plotting the values by state in a bar graph.
* The fifth task is determining which state has the most alcoholic beer and which has the most bitter beer.
* The sixth task provides a summary of our information about ABV for all states and breweries combined.
* The seventh task provides a graph comparing ABV and IBU for the entire data set. This is to see if there is a relationship between the two.
  
##Important
The files needed are beers.csv and breweries.csv.
Also used is the ggplot2 package for R.
These files are available in the github repository for this study.
And the ggplot2 package is available to everyone with R.
 
 
*This section reads in the data files and combines them for easier use* 


```r
head(beer_brew, 6)
```

```
##   Brew_ID        Name.x Beer_ID   ABV IBU
## 1       1  Get Together    2692 0.045  50
## 2       1 Maggie's Leap    2691 0.049  26
## 3       1    Wall's End    2690 0.048  19
## 4       1       Pumpion    2689 0.060  38
## 5       1    Stronghold    2688 0.060  25
## 6       1   Parapet ESB    2687 0.056  47
##                                 Style Ounces             Name.y
## 1                        American IPA     16 NorthGate Brewing 
## 2                  Milk / Sweet Stout     16 NorthGate Brewing 
## 3                   English Brown Ale     16 NorthGate Brewing 
## 4                         Pumpkin Ale     16 NorthGate Brewing 
## 5                     American Porter     16 NorthGate Brewing 
## 6 Extra Special / Strong Bitter (ESB)     16 NorthGate Brewing 
##          City State
## 1 Minneapolis    MN
## 2 Minneapolis    MN
## 3 Minneapolis    MN
## 4 Minneapolis    MN
## 5 Minneapolis    MN
## 6 Minneapolis    MN
```

```r
tail(beer_brew, 6)
```

```
##      Brew_ID                    Name.x Beer_ID   ABV IBU
## 2405     556             Pilsner Ukiah      98 0.055  NA
## 2406     557  Heinnieweisse Weissebier      52 0.049  NA
## 2407     557           Snapperhead IPA      51 0.068  NA
## 2408     557         Moo Thunder Stout      50 0.049  NA
## 2409     557         Porkslap Pale Ale      49 0.043  NA
## 2410     558 Urban Wilderness Pale Ale      30 0.049  NA
##                        Style Ounces                        Name.y
## 2405         German Pilsener     12         Ukiah Brewing Company
## 2406              Hefeweizen     12       Butternuts Beer and Ale
## 2407            American IPA     12       Butternuts Beer and Ale
## 2408      Milk / Sweet Stout     12       Butternuts Beer and Ale
## 2409 American Pale Ale (APA)     12       Butternuts Beer and Ale
## 2410        English Pale Ale     12 Sleeping Lady Brewing Company
##               City State
## 2405         Ukiah    CA
## 2406 Garrattsville    NY
## 2407 Garrattsville    NY
## 2408 Garrattsville    NY
## 2409 Garrattsville    NY
## 2410     Anchorage    AK
```
**Above you can see the first and last six entries in the data set.**


*Here we answer the question "How many breweries are in each state?"*

```r
# Breweries by state (#1)
Brews_by_state <- brew$State
Brews_by_state.freq <- table(Brews_by_state)
Brews_by_state.freq
```

```
## Brews_by_state
##  AK  AL  AR  AZ  CA  CO  CT  DC  DE  FL  GA  HI  IA  ID  IL  IN  KS  KY 
##   7   3   2  11  39  47   8   1   2  15   7   4   5   5  18  22   3   4 
##  LA  MA  MD  ME  MI  MN  MO  MS  MT  NC  ND  NE  NH  NJ  NM  NV  NY  OH 
##   5  23   7   9  32  12   9   2   9  19   1   5   3   3   4   2  16  15 
##  OK  OR  PA  RI  SC  SD  TN  TX  UT  VA  VT  WA  WI  WV  WY 
##   6  29  25   5   4   1   3  28   4  16  10  23  20   1   4
```

```r
barplot(Brews_by_state.freq)
```

![](Case_study_1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


**Above you can see the number of breweries in each state.**


*This section tells us how much data is missing from our files and what category of data is missing.*

```r
# Number of NA values in each column (#3)
colSums(is.na(beer_brew))
```

```
## Brew_ID  Name.x Beer_ID     ABV     IBU   Style  Ounces  Name.y    City 
##       0       0       0      62    1005       0       0       0       0 
##   State 
##       0
```
**There are missing values in only the columns for ABV (62 missing) and IBU (1005 missing).**


*The question answered here is "How do the alcoholic content and bitterness of the beers vary by state?"*

```r
# Simplified data with only State, ABV, and IBU; and remove NA values
state_alc <- data.frame(beer_brew$State, beer_brew$ABV, beer_brew$IBU)
state_alc_value <- na.omit(state_alc)

# Median Alcohol content and Bitterness rating by State (#4)
alc_by_state <- tapply(state_alc_value$beer_brew.ABV, state_alc_value$beer_brew.State, median)
med_bitterness_by_state <- tapply(state_alc_value$beer_brew.IBU, state_alc_value$beer_brew.State, median)
alc_by_state
```

```
##     AK     AL     AR     AZ     CA     CO     CT     DC     DE     FL 
## 0.0570 0.0600 0.0400 0.0575 0.0580 0.0650 0.0610 0.0590 0.0550 0.0620 
##     GA     HI     IA     ID     IL     IN     KS     KY     LA     MA 
## 0.0620 0.0520 0.0560 0.0580 0.0570 0.0570 0.0500 0.0575 0.0510 0.0540 
##     MD     ME     MI     MN     MO     MS     MT     NC     ND     NE 
## 0.0565 0.0670 0.0560 0.0555 0.0500 0.0580 0.0570 0.0610 0.0500 0.0560 
##     NH     NJ     NM     NV     NY     OH     OK     OR     PA     RI 
## 0.0465 0.0460 0.0610 0.0550 0.0595 0.0575 0.0630 0.0560 0.0570 0.0525 
##     SC     SD     TN     TX     UT     VA     VT     WA     WI     WV 
## 0.0500     NA 0.0550 0.0550 0.0400 0.0570 0.0550 0.0560 0.0510 0.0620 
##     WY 
## 0.0510
```

```r
med_bitterness_by_state
```

```
##   AK   AL   AR   AZ   CA   CO   CT   DC   DE   FL   GA   HI   IA   ID   IL 
## 46.0 43.0 39.0 20.5 42.0 40.0 29.0 47.5 52.0 55.0 55.0 22.5 26.0 39.0 30.0 
##   IN   KS   KY   LA   MA   MD   ME   MI   MN   MO   MS   MT   NC   ND   NE 
## 33.0 20.0 31.5 31.5 35.0 29.0 61.0 35.0 44.5 24.0 45.0 40.0 33.5 32.0 35.0 
##   NH   NJ   NM   NV   NY   OH   OK   OR   PA   RI   SC   SD   TN   TX   UT 
## 48.5 34.5 51.0 41.0 47.0 40.0 35.0 40.0 30.0 24.0 30.0   NA 37.0 33.0 34.0 
##   VA   VT   WA   WI   WV   WY 
## 42.0 30.0 38.0 19.0 57.5 21.0
```
**The values above are the lists of the median values for alcoholic content and bitterness of the beers by state.**



```r
test <- as.data.frame.table(alc_by_state)
test2 <- as.data.frame.table(med_bitterness_by_state)
med_by_state <- merge(test, test2, by="Var1")
colnames(med_by_state) <- c("State", "ABV", "IBU")
attach(med_by_state)
ggplot(data=med_by_state, aes(x=interaction(ABV,State), y=IBU, fill=State)) +
  geom_bar(stat="identity", position="dodge", colour="black")
```

```
## Warning: Removed 1 rows containing missing values (geom_bar).
```

![](Case_study_1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
detach(med_by_state)
```


**This graph shows how each state compares according to the median value of alcoholic content and bitterness.**


*"Which state has the most alcoholic beer?"*

```r
# Which state has most alcoholic beer (#5)
max_alc <- which.max(beer_brew$ABV)
beer_brew$State[max_alc]
```

```
## [1]  CO
## 51 Levels:  AK  AL  AR  AZ  CA  CO  CT  DC  DE  FL  GA  HI  IA  ID ...  WY
```
**Colorado has the beer with the highest alcoholic content.**


*"Which state has the most bitter beer?"*

```r
# which state has the most bitter beer (#5)
max_bitter <- which.max(beer_brew$IBU)
beer_brew$State[max_bitter]
```

```
## [1]  OR
## 51 Levels:  AK  AL  AR  AZ  CA  CO  CT  DC  DE  FL  GA  HI  IA  ID ...  WY
```
**Oregon has the beer that is the most bitter.**

*"What is the average, median, minimum, and maximum of the alcoholic content?"*

```r
#Summary of ABV (#6)
summary(beer_brew$ABV)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## 0.00100 0.05000 0.05600 0.05977 0.06700 0.12800      62
```

*"What is the relationship of alcoholic content and the bitterness of beer?"*

```r
# relationship between alcohol content and bitterness? (#7)
ggplot(state_alc_value, aes(x=state_alc_value$beer_brew.ABV, y=state_alc_value$beer_brew.IBU)) + geom_point()
```

![](Case_study_1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


**The graph above shows that there is a relationship between the alcoholic content of the beer and the bitterness. While there are exceptions, for the most part it looks as if the higher the alcoholic content, the more bitter the beer.**

##Summary
Using the data provided we have been able to show how states vary in their beer bitterness and alcoholic content, how many breweries there are in each state and how the bitterness of beer is affected by its alcoholic content.
