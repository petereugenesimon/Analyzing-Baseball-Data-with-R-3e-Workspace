# Get & set working directory
getwd()
setwd("C:/Users/peter/OneDrive/Documents/Baseball/R/ABDWR")

# Load packages and place hof_batting into hof data frame

library(tidyverse)
library(abdwr3edata)
hof <- hof_batting


# 3.2.1

# We want to create a new character variable Era giving the era for each player. First, we define a player’s mid career (variable MidCareer) as the average of his first and last seasons in baseball. We then use the mutate() and cut() functions to create the new factor variable Era—the arguments to the function are the numeric variable to be discretized, the vector of cut points, and the vector of labels for the categories of the factor variable.
hof <- hof |>
  mutate(
    MidCareer = (From + To) / 2,
    Era = cut(
      MidCareer,
      breaks = c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
      labels = c(
        "19th Century", "Dead Ball", "Lively Ball", "Integration",
        "Expansion", "Free Agency", "Long Ball"
      )
    )
  )

# A frequency table of the variable Era can be constructed using the summarize() function with the n() function. Below, we store that output in the data frame hof_eras
hof_eras <- hof |>
  group_by(Era) |>
  summarize(N = n())
hof_eras

# We construct a bar graph from those data using the geom_bar() function in ggplot2. The aes() function defines aesthetics. There are mappings between visual elements on the plot and variables in the data frame. Here we map the character vector Era to the x aesthetic, which defines horizontal positioning
ggplot(hof, aes(x = Era)) + geom_bar()


# 3.2.2

# In the ggplot2 package, the functions xlab() and ylab() add horizontal and vertical axis labels and the ggtitle() function adds a title
ggplot(hof, aes(Era)) +
  geom_bar() +
  xlab("Baseball Era") +
  ylab("Frequency") +
  ggtitle("Era of the Nonpitching Hall of Famers")


# 3.2.3

# For the data frame of era frequencies, we use the function geom_point() to construct a Cleveland-style dot plot
ggplot(hof_eras, aes(Era, N)) +
  geom_point(color = "red") +
  xlab("Baseball Era") +
  ylab("Frequency") +
  ggtitle("Era of the Nonpitching Hall of Famers") +
  coord_flip()


# 3.3

# We first type the R commands to produce the graph. Then we use the special ggsave() function where the argument is the name of the saved graphics file. Since the extension of the filename is png, the graph will be saved in PNG format
ggplot(hof, aes(Era)) +
  geom_bar() +
  xlab("Baseball Era") +
  ylab("Frequency") +
  ggtitle("Era of the Nonpitching Hall of Famers")
ggsave("C:/Users/peter/OneDrive/Documents/Baseball/R/ABDWR/bargraph.png")

# one can use the patchwork library to combine more than one ggplot into a single ggplot object
library(patchwork)
p1 <- ggplot(hof, aes(Era)) + geom_bar()
p2 <- ggplot(hof_eras, aes(Era, N)) + geom_point()
p1 + p2
ggsave("C:/Users/peter/OneDrive/Documents/Baseball/R/ABDWR/graphs.pdf")


# 3.4

# We construct a graph of the OPS values for the Hall of Fame inductees in ggplot2 by the geom_jitter() function. In the data frame hof, the OPS is mapped to the x aesthetic and the dummy variable y is set to a constant value. The theme elements are chosen to remove the tick marks, text, and title from the y-axis
ggplot(hof, aes(x = OPS, y = 1)) +
  geom_jitter(height = 0.2) +
  ylim(0, 2) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_fixed(ratio = 0.03)

# A histogram of the OPS values is constructed in the ggplot2 system by use of the function geom_histogram(). The only aesthetic mapping is to the variable OPS
ggplot(hof, aes(x = OPS)) +
  geom_histogram()

# One can select one’s own bins in geom_histogram() by use of the argument breaks. For example, if one wanted to choose the alternative bin endpoints 0.4, 0.5,..., 1.2, then one could construct the histogram by the following code. By use of the color and fill arguments, the lines of the bars are colored white and the bars are filled in orange
ggplot(hof, aes(OPS)) +
  geom_histogram(
    breaks = seq(0.4, 1.2, by = 0.1),
    color = "white", fill = "orange"
  )


# 3.5.1

# Construct a scatterplot using geom_point() where the variables MidCareer and OPS are respectively mapped to the x and y aesthetics. As it can be difficult to visually detect scatterplot patterns, it is helpful to add a smoothing curve by use of the geom_smooth() function to show the general association. This function by default implements the popular LOESS smoothing method
ggplot(hof, aes(MidCareer, OPS)) +
  geom_point() +
  geom_smooth()

# Add text labels to the plot using the geom_text_repel() function form the ggrepel package. Note that we use filter() to only send a small subset of the data to this function. Also the labels are colored red by use of the color = "red" argument to geom_text_repel()
library(ggrepel)
ggplot(hof, aes(MidCareer, OPS)) +
  geom_point() +
  geom_smooth() +
  geom_text_repel(
    data = filter(hof, OPS > 1.05 | OPS < 0.5),
    aes(MidCareer, OPS, label = Player), color = "red"
  )

# 3.5.2

# Use the geom_plot() function to construct a scatterplot of OBP and SLG
(p <- ggplot(hof, aes(OBP, SLG)) + geom_point())

# Use the xlab() and ylab() functions to replace OBP and SLG respectively with “On-Base Percentage” and “Slugging Percentage”
(p <- p +
    xlab("On Base Percentage") +
    ylab("Slugging Percentage"))

# To evaluate hitters in our graph on the basis of OPS, it would be helpful to draw constant values of OPS on the graph. If we represent OBP and SLG by x and y, suppose we wish to draw a line where OPS = 0.7 or where x + y = 0.7. Equivalently, we want to draw the function y = 0.7 - x on the graph; this is accomplished in the ggplot2 system by the geom_abline() function where the arguments to the function are given by slope = -1 and intercept = 0.7. Similarly, we apply the geom_abline() function three more times to draw lines on the graph where OPS takes on the values 0.8, 0.9, and 1.0
(p <- p +
    geom_abline(
      slope = -1,
      intercept = seq(0.7, 1, by = 0.1),
      color = "red"
    )
  )

# Each of the line labels is accomplished using the annotate() function—the three arguments are the x location and y location where the text is to be drawn, and label is the vector of strings of text to be displayed
p +
  annotate(
    "text", angle = -13,
    x = rep(0.31, 4),
    y = seq(0.4, 0.7, by = 0.1) + 0.02,
    label = paste("OPS = ", seq(0.7, 1, by = 0.1)),
    color = "red"
  )

# Rather than input these labels manually, we could create a data frame with the coordinates and labels, and then use the geom_text() function to add the labels to the plot
ops_labels <- tibble(
  OBP = rep(0.3, 4),
  SLG = seq(0.4, 0.7, by = 0.1) + 0.02,
  label = paste("OPS = ", (OBP + SLG) - 0.02),
  angle = -13
)
p +
  geom_text(
    data = ops_labels,
    hjust = "left",
    aes(label = label, angle = angle),
    color = "red"
  )


# 3.6

# Add a new variable hr_rate to the data frame hof
hof <- hof |>
  mutate(hr_rate = HR / AB)

# 3.6.1

# Construct parallel stripcharts of hr_rate by Era by using the geom_jitter() function; the x and y aesthetics are mapped to hr_rate and Era, respectively. We use the height = 0.1 argument to reduce the amount of the vertical jitter of the points
ggplot(hof, aes(hr_rate, Era)) +
  geom_jitter(height = 0.1)

# 3.6.2

# An alternative display for comparing distributions uses the geom_boxplot() function. Here the x and y aesthetics are mapped to Era and hr_rate, respectively. The function coord_flip() will flip the axes and display the boxplots horizontally. By use of the color and fill arguments, we display orange boxplots with brown edges
ggplot(hof, aes(Era, hr_rate)) +
  geom_boxplot(color = "brown", fill = "orange") +
  coord_flip()


# 3.7.1

# Begin by reading in the Lahman package
library(Lahman)

# In Major League Baseball, a player’s age for a season is defined to be his age on June 30. So we make a slight adjustment to a player’s birth year depending if his birthday falls in the first six months or not. The adjusted birth year is stored in the variable mlb_birthyear. (The if_else() function is useful for assignments based on a condition; if birthMonth >= 7 is TRUE, then birthyear <- birthYear + 1, otherwise birthyear <- birthyear.)
PlayerInfo <- People |>
  filter(
    playerID %in% c(
      "ruthba01", "aaronha01", "bondsba01", "rodrial01"
    )
  ) |>
  mutate(
    mlb_birthyear = if_else(
      birthMonth >= 7, birthYear + 1, birthYear
    ),
    Player = paste(nameFirst, nameLast)
  ) |>
  select(playerID, Player, mlb_birthyear)

# 3.7.2

# To get the batting and age data for Babe Ruth, we use the inner_join() function to match the rows of the batting data to those corresponding in the PlayerInfo data frame where playerID is equal. We create a new variable Age defined to be the season year minus the player’s birth year. (Recall that we made a slight modification to the birthyear variable so that one obtains a player’s correct age for a season.) Last, for each player, we use the cumsum() function on the grouped data to create a new variable cHR containing the cumulative count of home runs for each player each season
HR_data <- Batting |>
  inner_join(PlayerInfo, by = "playerID") |>
  mutate(Age = yearID - mlb_birthyear) |>
  select(Player, Age, HR) |>
  group_by(Player) |>
  mutate(cHR = cumsum(HR))


# 3.7.3

# Use the geom_line() function to graph the cumulative home run counts against age. By mapping the color aesthetic to the Player variable, distinct cumulative home run lines are drawn for each player. Note that different colors are used for the four players and a legend is automatically constructed that matches up the line type with the player’s name. The scale_color_manual function allows us to specify the set of colors to use in the plot. In this case, the vector crc_fc contains an ordered set of pre-defined colors
crc_fc <- c("#2905a1", "#e41a1c", "#4daf4a", "#984ea3")
ggplot(HR_data, aes(x = Age, y = cHR, color = Player)) +
  geom_line() +
  scale_color_manual(values = crc_fc)


# 3.8.1

# THE FOLLOWING CODE IS AN ALTERNATIVE METHOD TO PULLING RETROSHEET DATA WRITTEN BY ONE OF THE AUTHORS DUE TO THE DIFFICULTLY IN WORKING WITH THE CHADWICK TOOLS
# Begin by reading in the 1998 play-by-play data and storing it in the data frame retro1998
library(devtools)
source_gist("https://gist.github.com/bayesball/8892981",
            filename="parse.retrosheet2.pbp.R")

parse.retrosheet2.pbp(1998)

# I was getting an error using "col.names = FALSE" but don't get an error using "header = FALSE"
retro1998 <- read.csv("download.folder/unzipped/all1998.csv", header = FALSE)

fields <- read.csv("https://raw.githubusercontent.com/beanumber/baseball_R/master/data/fields.csv")
names(retro1998) <- fields[, "Header"]

# The variable bat_id gives the identification code for the player who is batting. We need to find the codes for these two players available in the Lahman People data frame. By use of the filter() function, we find the id code where nameFirst = "Sammy" and nameLast = "Sosa"
sosa_id <- People |>
  filter(nameFirst == "Sammy", nameLast == "Sosa") |>
  pull(retroID)
mac_id <- People |>
  filter(nameFirst == "Mark", nameLast == "McGwire") |>
  pull(retroID)

# We extract McGwire’s and Sosa’s plate appearance data from the play-by-play data frame retro1998
hr_race <- retro1998 |>
  filter(BAT_ID %in% c(sosa_id, mac_id))


# 3.8.2

# Using the str_sub() function, we select the 4th through 11th characters of this string variable and assign this date to the variable Date. (The ymd() function converts the date to the more readable “year-month-day” format, and forces R to recognize it as a Date.) Using the arrange() function, we sort the play-by-play data from the beginning to the end of the season. The variable event_cd contains the outcome of the batting play; a value event_cd of 23 indicates that a home run has been hit. We define a new variable HR to be either 1 or 0 depending if a home run occurred, and the new variable cumHR records the cumulative number of home runs hit in the season using the cumsum() function. The output of the function is a new data frame containing each date and the cumulative number of home runs to date for all plate appearances during the season
cum_hr <- function(data) {
  data |>
    mutate(Date = ymd(str_sub(GAME_ID, 4, 11))) |>
    arrange(Date) |>
    mutate(
      HR = if_else(EVENT_CD == 23, 1, 0),
      cumHR = cumsum(HR)
    ) |>
    select(Date, cumHR)
}

# We use the group_split() and map() functions to iterate cum_hr() twice, once on Sosa’s batting data and once on McGwire’s batting data, obtaining the new data frame hr_ytd
hr_grouped <- hr_race |>
  group_by(BAT_ID)

keys <- hr_grouped |>
  group_keys() |>
  pull(BAT_ID)

hr_ytd <- hr_grouped |>
  group_split() |>
  map(cum_hr) |>
  set_names(keys) |>
  bind_rows(.id = "BAT_ID") |>
  inner_join(People, by = c("BAT_ID" = "retroID"))


# 3.8.3

# The geom_line() function constructs a graph of the cumulative home run count against the date. By mapping nameLast to the color aesthetic, the lines corresponding to the two players are drawn using different colors. We use the geom_hline() function to add a horizontal line at the home run value of 62 and the annotate() function is applied to place the text string “62” above this plotted line
crcblue <- "#2905a1"
ggplot(hr_ytd, aes(Date, cumHR, color = nameLast)) +
  geom_line() +
  geom_hline(yintercept = 62, color = crcblue) +
  annotate(
    "text", ymd("1998-04-15"), 65,
    label = "62", color = crcblue
  ) +
  ylab("Home Runs in the Season")