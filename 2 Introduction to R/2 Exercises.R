# Load packages for this section
library(abdwr3edata)
library(tidyverse)

# Get & set working directory
getwd()
setwd("C:/Users/peter/OneDrive/Documents/Baseball/R/ABDWR")


# 1. Top Base Stealers in the Hall of Fame

# a. In R, place the stolen base, caught stealing, and game counts in the vectors SB, CS, and G
SB <- c(1406, 938, 896, 808, 741, 738, 689, 580, 514, 509, 506, 504, 474)
CS <- c(335, 307, 178, 146, 173, 92, 162, 148, 141, 117, 136, 131, 114)
G <- c(3081, 2616, 3035, 2502, 2826, 2476, 2649, 2573, 2986, 2653, 2601, 2683, 2379)

# b. For all players, compute the number of stolen base attempts SB + CS and store in the vector SB.Attempt
SB.Attempt <- SB + CS

# c. For all players, compute the success rate Success.Rate = SB / SB.Attempt
Success.Rate <- SB / SB.Attempt

# d. Compute the number of stolen bases per game SB.Game = SB / Game
SB.Game <- SB / G

# e. Construct a scatterplot of the stolen bases per game against the success rates. Are there particular players with unusually high or low stolen base success rates? Which player had the greatest number of stolen bases per game?
plot(Success.Rate, SB.Game)

# Max Carey had the highest stolen base success rate at 0.889. Lou Brock had the lowest stolen base success rate at 0.753. Rickey Henderson had the highest number of stolen bases per game at 0.46, even though he sat around the median in success rate.


# 2. Character, Factor, and Logical Variables in R

# a. Use the c() function to collect these outcomes in a character vector outcomes
outcomes <- c("Single", "Out", "Out", "Single", "Out", "Double", "Out", "Walk", "Out", "Single")

# b. Use the table() function to construct a frequency table of outcomes
outcomes_table <- table(outcomes)

# c. In tabulating these results, suppose one prefers the results to be ordered from least-successful to most-successful. Use the following code to convert the character vector outcomes to a factor variable f.outcomes
f.outcomes <- factor(
  outcomes,
  levels = c("Out", "Walk", "Single", "Double")
)

# Use the table() function to tabulate the values in f.outcomes. How does the output differ from what you saw in part (b)?
f.outcomes_table <- table(f.outcomes)

outcomes_table
f.outcomes_table

# The table in part (b) displayed the tabulated character outcomes in alphabetical order where the factor variable table displayed the tabulated character outcomes in the order defined in the factor function

# Suppose you want to focus only on the walks in the plate appearances. Describe what is done in each of the following statements

outcomes == "Walk"
# This statement is creating a logical vector that will display which items in the outcomes vector equal the word "Walk"

sum(outcomes == "Walk")
# This statement is summing the items in the logical vector that are TRUE


# 3. Pitchers in the 350-Wins Club

# a. In R, place the wins and losses in the vectors W and L, respectively. Also, create a character vector Name containing the last names of these pitchers
W <- c(373, 354, 365, 417, 355, 373, 362, 363, 511)
L <- c(208, 184, 310, 279, 227, 188, 208, 245, 315)
Name <- c("Alexander", "Clemens", "Galvin", "Johnson", "Maddux", "Mathewson", "Nichols", "Spahn", "Young")

# b. Compute the winning percentage for all pitchers defined by 100 * W / (W + L) and put these winning percentages in the vector win_pCT
win_pCT <- 100 * W / (W + L)

# c. Create a data frame wins_350 containing the names, wins, losses, and winning percentages
wins_350 <- tibble(Name, W, L, win_pCT)

# d. By use of the arrange() function, sort the data frame wins_350 by winning percentage. Among these pitchers, who had the largest and smallest winning percentages?
wins_350 |>
  arrange(desc(win_pCT))

# Christy Mathewson had the highest winning percentage at 66.5 and Pud Galvin had the lowest winning percentage at 54.1


# 4. Pitchers in the 350-Wins Club, Continued

# a. In R, place the strikeout and walk totals from the 350 win pitchers in the vectors SO and BB, respectively. Also, create a character vector Name containing the last names of these pitchers
SO <- c(2198, 4672, 1807, 3509, 3371, 2507, 1881, 2583, 2803)
BB <- c(951, 1580, 745, 1363, 999, 848, 1272, 1434, 1217)

# b. Compute the strikeout-walk ration by SO / BB and put these ratios in the vector SO.BB.Ratio
SO.BB.Ratio <- SO / BB

# c. Create a data frame SO.BB containing the names, strikeouts, walks, and strikeout-walk ratios
SO.BB <- tibble(Name, SO, BB, SO.BB.Ratio)

# d. By use of the filter() function, find the pitchers who had a strikeout-walk ratio exceeding 2.8
SO.BB |>
  filter(SO.BB.Ratio > 2.8)

# e. By use of the arrange() function, sort the data frame by the number of walks. Did the pitcher with the largest number of walks have a high or low strikeout-walk ratio?
SO.BB |>
  arrange(desc(BB))

# Roger Clemens had the most walks but he still had a higher strikeout-walk ratio than most of the other pitchers in the table


# 5. Pitcher Strikeout/Walk Ratios

# a. Read the Lahman Pitching data into R
library(Lahman)

# b. The following script computes the cumulative strikeouts, cumulative walks, mid career year, and the total innings pitched (measured in terms of outs) for all pitchers in the data file. This new data frame is named career_pitching. Run this code and use the inner_join() function to merge the Pitching and career_pitching data frames
career_pitching <- Pitching |>
  group_by(playerID) |>
  summarize(
    SO = sum(SO, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    IPouts = sum(IPouts, na.rm = TRUE),
    midYear = median(yearID, na.rm = TRUE)
  )

Pitching <- inner_join(Pitching, career_pitching, by = "playerID")

# c. Use the filter() function to construct a new data frame career_10000 consisting of data for only those pitchers with at least 10,000 career IPouts
career_10000 <- career_pitching |>
  filter(IPouts >= 10000)

# d. For the pitchers with at least 10,000 career IPouts, construct a scatterplot of mid career year and ratio of strikeouts to walks. Comment on the general pattern in this scatterplot.
ggplot(career_10000, aes(x = midYear, y = SO / BB)) +
  geom_point() + geom_smooth()

# The scatterplot has a parabolic shape to it. The earlier pitchers had higher SO / BB ratios in the beginning of the game (mainly due to an outlier) but there was a dip around the 1920s. SO / BB rose after that dip and continues to go up as we get towards modern pitchers.