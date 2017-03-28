library(data.table)
library(ggplot2)


setwd("C:/work")


vets <- fread("VetsData.csv")


# Change variables to appropriate type
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
vets[, `Current Term` := factor(`Current Term`)]
vets[, `Term Admitted` := factor(`Term Admitted`)]


# I need to develop a way to determine if a student has graduated. Additionally, 
# I need to determine which semester said student graduated in. It would also be 
# nice to develop some statisitcs around the student in question, e.g. how many semesters
# enrolled, how many years enrolled, how many credits taken, etc. This information
# will be stored in a data frame, the challenge is determining the the graduation. 
# First, I want to separate the undergrads, from graduate and other professional students
# This will make defining graduation rates a little easier. 
undergrads <- vets[`Academic Career` == "Undergraduate"]
table(undergrads$`Academic Level`)


# Since there is no graduation variable, I'm going to have to come up with a method
# for "guessing" if a student has graduated. I say "guessing", because there is still
# no way to be 100% a student graduated with data from the system. The method I will
# use is to create a list where each element is a student. Then I will see which students
# have taken more than 120 credits. Let's create the list. 
students <- vector("list", length = length(unique(undergrads[, `Student ID`])))

# I'm cycling through each of the unique student IDs and populating the students
# list by finding all the 
ids <- unique(undergrads[, `Student ID`])
for (i in 1:length(students)) {
  students[[i]] <- undergrads[`Student ID` == ids[i]]
}

# I will use this list later on, and it'll be good to have an object where
# each individual element is a student, but I'm going to get the information
# through different means now. Basically, what I'm going to do is sort the 
# Cummulative Progress Units Taken variable in a decreasing manner for each student. 
# First, I need to set a key on the Student ID variable
setkey(undergrads, 'Student ID')

# Now, I need to sort the Cumm Prog Units Taken in a decreasing order
undergrads <- undergrads[order(undergrads$`Cumulative Progress Units`, decreasing = TRUE)]

# Now I can use the unique() function and sort by the Student ID. What this will 
# do is grab the first row for each unique Student ID. The reason I sorted the
# Cum Progress Units Taken variable in a decreasing manner, is because the unique
# function will now grab the row for that corresponds to the greatest amount
# of progress units taken for each student. 
undergrads <- unique(undergrads, by = "Student ID")

# Now I have the last semester each student attended school. But we can see from
# a 5-num summary that there are some students who have more or less than 120 
# Cumm Progress Units Taken
summary(undergrads$`Cumulative Progress Units`)

# Now I want separate the data.table by people who have taken over 120 credits, 
# and those who have takne under. 
over.120.inc <- undergrads[`Cumulative Progress Units` >= 120]
under.120.exc <- undergrads[`Cumulative Progress Units` < 120]


