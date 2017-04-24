library(openxlsx)
library(data.table)
library(ggplot2)
library(RODBC)
library(RMySQL)

setwd("c:/Work/Veterans")

# setwd("C:/mystuff/datascience/projects/veterans")

# Get the names of each sheet of the Excel file
sheet.names <- getSheetNames("MasterList01_30_17.xlsx")

# Create a function that can be lapply'd over the sheet.names 
sheet.function <- function(x, sheet.names) {  
  as.data.table(read.xlsx(xlsxFile = x, sheet = sheet.names))
}

# lapply above function to sheet.names object. End result is a list 
# of all sheets in the MasterList workbook
all.data <- lapply(sheet.names, sheet.function, x = "MasterList01_30_17.xlsx")

files <- list.files()
files <- files[5:21]

all.data <- lapply(files, read.xlsx)
all.data <- lapply(all.data, as.data.table)

# Name each list element with the appropriate sheet name from the workbook
names(all.data) <- files

# Get all the column names from the first sheet. The column names are abbreviated, 
# so I want to change them to their extended forms for better understandability
col.abbr <- colnames(all.data$`Summer 2014`)

# Write this to an Excel file that will be sent to the person who can give me 
# extended versions
write.csv(col.abbr, file = "ColumnNames")

# Now I need to convert the columns to the appropriate type. To do this, I want 
# to make my list one large data table. In order for this to work, I need to add 
# a column which will be Current Term. The Current Term is the name of each list 
# element. By doing this, I won't lose the term information, and I can change the 
# variable types faster. To do this, 
for (i in 1:length(all.data)) {
  length.x <- nrow(all.data[[i]])
  term <- rep(sheet.names[i], times = length.x)
  all.data[[i]] <- cbind(all.data[[i]], term)
}
all.data[[10]]

# The Excel file has two types of sheets. One is an individual term, such as Summer 2014, 
# and the second is an aggregated sheet for each academic year, such as 2014-2015. 
# I want to separate these out because I don't want to include the aggregated sheets
# in my large data table, because I'd be repeating all the information. 
term.data <- all.data[-c(4, 8, 12)]
agg.data <- all.data[c(4, 8, 12)]

# Now that these are split, I can rbind each of the list elements into one data table
term.dt <- do.call("rbind", all.data)

# Before I start convert data types, I'm going to get rid of some of the columns
# I don't need. 
term.dt[, c("CU.VA.CLM.NUM", "CU.SMR.BNFT.AMT", "CU.ACD.YR.BNFT.AMT", "CU.SMR.EDBL.AMT",
            "CU.CRTFCTN.TERM", "ACAD.PLAN.CD") := NULL]

# Now I have all of my term data in one data table, I can begin to start converting
# the data types where needed. I'm going to convert all necessary variables to factors
# first. 
term.dt[, "INSTITUTION.CD" := factor(INSTITUTION.CD)]
class(term.dt$INSTITUTION.CD) # to ensure conversion worked 

term.dt[, "ETHNIC.GRP.CD" := factor(ETHNIC.GRP.CD)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "MAR.STAT.LD" := factor(MAR.STAT.LD)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "GENDER.CD" := factor(GENDER.CD)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "ACAD.LVL.LD" := factor(ACAD.LVL.LD)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "CAMPUS.CD" := factor(CAMPUS.CD)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "ACAD.CAREER" := factor(ACAD.CAREER)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "MAJOR1.LD" := factor(MAJOR1.LD)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "DEG.LD" := factor(DEG.LD)]
class(term.dt$ETHNIC.GRP.CD)


term.dt[, "ACAD.PLAN.LD" := factor(ACAD.PLAN.LD)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "CU.MILITARY.STS.CD" := factor(CU.MILITARY.STS.CD)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "CU.MILITARY.STS.LD" := factor(ETHNIC.GRP.CD)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "CU.MILITARY.BRANCH" := factor(CU.MILITARY.BRANCH)]
class(term.dt$ETHNIC.GRP.CD)

term.dt[, "ENRLD.TERM.SD" := factor(ENRLD.TERM.SD)]

term.dt[, "ADMIT_TYPE_LD" := factor(ADMIT_TYPE_LD)]

# Now comes the more tedious cleaning, which I won't do in any particular order
# First, the PR.ADMIT.TERM column. It is currently encoded to represent the various
# years, and the enrollment terms possible. Each code has form 21XY, where the 
# 21 refers to years 2000+, the X refers to the specific year, and the Y is the term. 
# The terms have encoding: 7 = fall, 1 = spring, 4 = summer. For example, the code
# 2157 refers to the year 2015, and the fall term. 
codes <- unique(term.dt$PR.ADMIT.TERM)
decode <- c("Summer 2014", "Fall 2011", "Summer 2013", "Summer 2011", "Summer 2012", "Fall 2012", 
            "Spring 2014", "Fall 2014", "Spring 2012", "Fall 2013", "Spring 2013", "Spring 2015", 
            "Summer 2015", "Fall 2015", "Spring 2016", "Summer 2016", "Fall 2016", "Spring 2017")


for (i in 1:length(decode)) {
  loc <- grep(codes[i], term.dt[, PR.ADMIT.TERM])
  set(term.dt, i = loc, j = "PR.ADMIT.TERM", value = decode[i])
}



# Now let's change the GENDER.CD to their exapnded form. 
# Males
loc <- grep("M", term.dt[, GENDER.CD])
set(term.dt, i = loc, j = "GENDER.CD", "Male")

# Females
loc <- grep("F", term.dt[, GENDER.CD])
set(term.dt, i = loc, j = "GENDER.CD", "Female")

# Unknown
loc <- grep("U", term.dt[, GENDER.CD])
set(term.dt, i = loc, j = "GENDER.CD", "Unknown")

# Reset as factor so levels are appropriate
term.dt[, GENDER.CD := factor(GENDER.CD)]

# Now let's change the BIRTH.DATE column to an actual date
term.dt[, BIRTH.DATE := as.Date(BIRTH.DATE, origin = "1899-12-30")]

# Now let's change the CAMPUS.CD to the long format. AMC = Anschutz Medical Campus
# and DC = Downtown Campus. I'm going to make DC Downtown 
loc <- grep("DC", term.dt[, CAMPUS.CD])
set(term.dt, i = loc, j = "CAMPUS.CD", "Downtown")

loc <- grep("AMC", term.dt[, CAMPUS.CD])
set(term.dt, i = loc, j = "CAMPUS.CD", "Anschutz Medical Campus")

# Now let's change the ACAD.CAREER
loc <- grep("DENT", term.dt[, ACAD.CAREER])
set(term.dt, i = loc, j = "ACAD.CAREER", "Dentistry")

loc <- grep("MED", term.dt[, ACAD.CAREER])
set(term.dt, i = loc, j = "ACAD.CAREER", "Medicine")

loc <- grep("PHAR", term.dt[, ACAD.CAREER])
set(term.dt, i = loc, j = "ACAD.CAREER", "Pharmacy")

loc <- grep("GRAD", term.dt[, ACAD.CAREER])
set(term.dt, i = loc, j = "ACAD.CAREER", "Graduate")

loc <- grep("UGRD", term.dt[, ACAD.CAREER])
set(term.dt, i = loc, j = "ACAD.CAREER", "Undergraduate")

loc <- grep("DPT", term.dt[, ACAD.CAREER])
set(term.dt, i = loc, j = "ACAD.CAREER", "Physical Therapy")


# Now let's change the CU.MILITARY.BRANCH column to its decoded form.
loc <- grep("AIR", term.dt[, CU.MILITARY.BRANCH])
set(term.dt, i = loc, j = "CU.MILITARY.BRANCH", value = "Air Force")

loc <- grep("ARM", term.dt[, CU.MILITARY.BRANCH])
set(term.dt, i = loc, j = "CU.MILITARY.BRANCH", value = "Army")

loc <- grep("MRN", term.dt[, CU.MILITARY.BRANCH])
set(term.dt, i = loc, j = "CU.MILITARY.BRANCH", value = "Marines")

loc <- grep("NAV", term.dt[, CU.MILITARY.BRANCH])
set(term.dt, i = loc, j = "CU.MILITARY.BRANCH", value = "Navy")

loc <- grep("CST", term.dt[, CU.MILITARY.BRANCH])
set(term.dt, i = loc, j = "CU.MILITARY.BRANCH", value = "Coast Guard")


term.dt[, CU.MILITARY.BRANCH := factor(CU.MILITARY.BRANCH)]
                                       
# Now let's get rid of unnecessary columns
term.dt[, ADMIT_TYPE := NULL]
term.dt[, ADMT_DT_MAX := NULL]
term.dt[, ENRLD.TERM.CD := NULL]
term.dt[, CU.MILITARY.STS.CD := NULL]
term.dt[, CU.MILITARY.STS.LD := NULL]
term.dt[, `Count.Distinct(PERSON.ID)1` := NULL]
term.dt[, TERM.SD := NULL]
term.dt[, ACAD.PROG.PRIMARY := NULL]
term.dt[, CU.ACD.YR.EDBL.AMT := NULL]

# Finally, let's change the column headers to a decoded version
heads <- c("Institution", "Student ID", "First Name", "Last Name", "Ethnicity",
           "Marital Status", "Gender", "Birth Date", "Term Admitted", "Admission Type", "Current Term",
           "Academic Level", "Campus", "Academic Career", "Academic Career CD", "Primary Academic Program", 
           "Primary Plan", "Primary Major", "Enrollment Status", "Progress Units Taken", 
           "Current Term GPA", "Cumulative Progress Units", "Cumulative GPA", "Degree",
           "Academic Plan", "Military Branch", "Began Service", "Ended Service", "
           Certification Status", "Aid Year", "Benefit Chapter", "Certification Type", 
           "Residency Status", "Tuition Residency", "Tuition Classification", "Admission Residency",
           "Email", "Home Phone", "Cell Phone", "Home Address", "Unit/Apt Number", 
           "Home City", "Home State", "Home Zipcode", "Mailing Address", "Mailing Unit/Apt Number",
           "Mailing Address Extended", "Mailing City", "Mailing State", "Mailing Zipcode")

names(term.dt) <- heads

unique(term.dt, by = c("Student ID", "Current Term"))

# That concludes the cleaning! Let's save this and we can work with this clean 
# data for our analysis/summary! 
write.csv(term.dt, file = "VetsData.csv")
