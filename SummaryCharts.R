library(data.table)
library(ggplot2)

setwd("C:/work")

vets <- fread("VetsData.csv")


# Before I get into any analysis or summary visualizations, I want to do some EDA
# to see if I can learn anything about the data. First thing I'll do is great a 
# histogram and density plot for each factor variable. 
# Now I have all of my term data in one data table, I can begin to start converting
# the data types where needed. I'm going to convert all necessary variables to factors
# first, since they were imported as strings.
vets[, "Institution" := factor(Institution)]
vets[, "Ethnicity" := factor(Ethnicity)]
vets[, "Marital Status" := factor(`Marital Status`)]
vets[, "Gender" := factor(Gender)]
vets[, "Academic Level" := factor(`Academic Level`)]
vets[, "Campus" := factor(Campus)]
vets[, "Academic" := factor(`Academic Career`)]
vets[, "Primary Major" := factor(`Primary Major`)]
vets[, "Degree" := factor(Degree)]
vets[, `Admission Type` := factor(`Admission Type`)]
vets[, `Military Branch` := factor(`Military Branch`)]

# Set a key to remove duplicates
setkey(vets, `Student ID`) 
subset(unique(vets))

# Reassign to have data.table without repeats
vets <- vets[!duplicated(vets$`Student ID`)]

# ggplot base
p <- ggplot(vets)

# Ethnicity 
p + geom_bar(mapping = aes(x = reorder(Ethnicity, Ethnicity, function(x) -length(x))),
             fill = "#F5DEB3", col = "black") +
    labs(title = "Ethnicites of Students", x ="Ethnicity", y = "Count")        
    

# Gender
gen <- p + geom_bar(mapping = aes(x = reorder(Gender, Gender, function(x) -length(x))),
             fill = "#F5DEB3", col = "black") +
          labs(title = "Gender of Students", x ="Gender", y = "Count")
ggsave("gender.png", plot = gen)

# Academic Level
acad <- p + geom_bar(mapping = aes(x = reorder(`Academic Level`, `Academic Level`, function(x) -length(x))),
             fill = "#F5DEB3", col = "black") +
          labs(title = "Academic Level of Students", x = "Academic Level", y = "Count")
ggsave("academiclevel.png", plot = acad)

# Campus 
campus <- p + geom_bar(mapping = aes(x = reorder(Campus, Campus, function(x) -length(x))),
             fill = "#F5DEB3", col = "black") +
          labs(title = "Campus", x = "Campus", y = "Count")
ggsave("campus.png", plot = campus)

# Admission Type
p + geom_bar(mapping = aes(x = reorder(`Admission Type`, `Admission Type`, function(x) -length(x))),
             fill = "#F5DEB3", col = "black") +
  labs(title = "Admission Type of Students", x = "Admission Type", y = "Count")

# Military Branch
branch <- p + geom_bar(mapping = aes(x = reorder(`Military Branch`, `Military Branch`, function(x) -length(x))),
             fill = "#F5DEB3", col = "black") +
            labs(title = "Military Branch of Students", x = "Branch", y = "Count")

ggsave("branch.png", plot = branch)
# Benefit Chapter
ben.chap <- p + geom_bar(mapping = aes(x = reorder(`Benefit Chapter`, `Benefit Chapter`, function(x) -length(x))),
             fill = "#F5DEB3", col = "black") +
            labs(title = "Benefit Chapter of Students", x = "Chapter", y = "Count")

ggsave("benefitchapter.png", plot = ben.chap)
# 