# ImportMaker.R

# This process requires a few files
# Export all current students from PowerSchool
# Export all course assignments from PowerSchool (cc table)
# Check to make sure the field names are the same as what is used below

source("functions.R") #load the functions

#-----------------------------------------#
#### Get the course-subject alignments ####
#-----------------------------------------#

# Sign in to google
# gs_auth() #this will launch a browser so you can sign into your account
CourseSubject = gs_url(x = "https://docs.google.com/spreadsheets/d/17QhVYZkjbx34M6wBvtHUYa_XrRUlRbOtuOsQ4P5l-nk/edit?usp=sharing", lookup = F, visibility = "private", verbose = F)
alignment = gs_read(ss = CourseSubject, verbose = F)
alignment$Course[alignment$Course == "6th Grade Attendance"][1] = "6th Grade Attendance "



#--------------------#
#### Read in data ####
#--------------------#
# Read the export of the cc(4) table from PowerSchool
# Make sure you have only exported the current year
# variables needed: 
#     [01]last_name	[01]first_name	[01]Student_Number	[01]grade_Level	[02]course_name	section_number
#     expression	[03]room	[05]lastfirst	Course_Number	DateEnrolled	DateLeft	TermID SectionID
cctable.raw = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "ccTableExport") 
cctable = cctable.raw
cctable = cctable[!(cctable$`[02]course_name` %in% c("Independent Study","Career Internship", "6th Grade Attendance ")),]
if("S_NY_CC_X.DualCrInd" %in% colnames(cctable)){
  cctable$S_NY_CC_X.DualCrInd = NULL
}
cctable = cctable[cctable$TermID > 0,]


# Check whethere there are any rows with missing info
cctable$AnyNA = apply(X = cctable, MARGIN = 1, FUN = anyNA)
if(any(cctable$AnyNA)){
  print(paste0("WARNING!  There are rows in the CC table that are missing info."))
  print(cctable[cctable$AnyNA,])
} else {
  print("Yay!  No missing info in the cc table.")
}

# unique(cctable$`[03]room`)
# x = cctable[cctable$AnyNA,]
# x = x[!duplicated(x$SectionID),]
# View(x)

# Pull in subject areas and check for missing subjects
cctable$subject = alignment$Subject[match(cctable$`[02]course_name`, alignment$Course)] #add subject variable
if(anyNA(cctable$subject)){
  MissingCourses = cctable$`[02]course_name`[is.na(cctable$subject)]
  MissingCourses = sort(unique(MissingCourses))
  print("WARNING!  The following courses are missing subject alignments:")
  print(MissingCourses)
} else {
  print("Yay!  No courses missing from the course alignment table.")
}


# Remove unnecessary courses
cctable = cctable[cctable$subject != "Delete",]
dropCourses = c("Exploring Algebra with Technology", "Study Hall", "6th Grade Math RTI", "6th Grade ELA RTI")
cctable = cctable[!(cctable$`[02]course_name` %in% dropCourses), ]

# Change the location of 6th grade PE
cctable$`[03]room`[cctable$`[02]course_name` == "6th Grade PE"] = "gym"

# Determine the class period
periodtable = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "PeriodLookup")
cctable$period = periodtable$Period[match(cctable$expression, periodtable$expression)]
cctable$ScantronPeriod = cctable$period  #usually, the scantron period is the same as the actual period
cctable$session = paste0(cctable$`[03]room`," -- ", cctable$period, " -- ",cctable$TermID)
cctable$CourseSection = paste0(cctable$`[02]course_name`, " -- ",cctable$section_number)

cctable[is.na(cctable$ScantronPeriod),]

# Remove spaces from course numbers
cctable$ScantronCourseNumber = gsub(pattern = " ", replacement = "", x = cctable$Course_Number)

# Check for courses missing from the course lookup
cctable.bad = cctable[!(cctable$Course_Number %in% CourseLookup$Course.Number),]
if(nrow(cctable.bad) > 0 ){
  print("WARNING!  There are courses missing from the CourseLookup tab in the Parameters and Settings.xlsx file.")
  print(cctable.bad[!duplicated(cctable.bad$Course_Number),])
} else {
  print("Yay!  No courses missing from the course lookup.")
}






#-------------------------------------#
#### Create Humanities Enrollments ####
#-------------------------------------#

HumanitiesSections = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "Humanities")

if(nrow(HumanitiesSections) > 0 ){
  for(i in 1:nrow(HumanitiesSections)){
    HumanitiesSections$ELAsession[i] = cctable$session[(cctable$`[02]course_name` == HumanitiesSections$ELAcourse[i] & 
                                                          cctable$section_number == HumanitiesSections$ELAsection[i])][1]
    HumanitiesSections$SStSession[i] = cctable$session[cctable$`[02]course_name` == HumanitiesSections$SScourse[i] & 
                                                         cctable$section_number == HumanitiesSections$SSsection[i] ][1]
    HumanitiesSections$room[i] = cctable$`[03]room`[cctable$`[02]course_name` == HumanitiesSections$SScourse[i] & 
                                                      cctable$section_number == HumanitiesSections$SSsection[i] ][1]
    HumanitiesSections$term[i] = cctable$TermID[cctable$`[02]course_name` == HumanitiesSections$SScourse[i] & 
                                                  cctable$section_number == HumanitiesSections$SSsection[i] ][1]
  }
  
  HumanitiesEnrollments1 = cctable[cctable$session %in% c(HumanitiesSections$ELAsession),]
  HumanitiesEnrollments1$`[02]course_name` = HumanitiesSections$`[02]course_name`[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
  HumanitiesEnrollments1$section_number = HumanitiesSections$section_number[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
  HumanitiesEnrollments1$period = HumanitiesSections$period[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
  HumanitiesEnrollments1$ScantronPeriod = HumanitiesSections$ScantronPeriod[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
  HumanitiesEnrollments1$`[03]room` = HumanitiesSections$room[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
  HumanitiesEnrollments1$TermID = HumanitiesSections$term[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
  HumanitiesEnrollments1$`[05]lastfirst` = HumanitiesSections$teacher[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
  
  
  
  HumanitiesEnrollments2 = cctable[cctable$session %in% c(HumanitiesSections$SStSession),]
  HumanitiesEnrollments2$`[02]course_name` = HumanitiesSections$`[02]course_name`[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
  HumanitiesEnrollments2$section_number = HumanitiesSections$section_number[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
  HumanitiesEnrollments2$period = HumanitiesSections$period[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
  HumanitiesEnrollments2$ScantronPeriod = HumanitiesSections$ScantronPeriod[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
  HumanitiesEnrollments2$`[03]room` = HumanitiesSections$room[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
  HumanitiesEnrollments2$TermID = HumanitiesSections$term[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
  HumanitiesEnrollments2$`[05]lastfirst` = HumanitiesSections$teacher[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
  
  HumanitiesEnrollments = rbind.data.frame(HumanitiesEnrollments1, HumanitiesEnrollments2)
  HumanitiesEnrollments$session = paste0(HumanitiesEnrollments$`[03]room`," -- ", HumanitiesEnrollments$period, " -- ",HumanitiesEnrollments$TermID)
  HumanitiesEnrollments$ID.session = paste0(HumanitiesEnrollments$`[01]Student_Number`, " -- ", HumanitiesEnrollments$session)
  HumanitiesEnrollments = HumanitiesEnrollments[!duplicated(HumanitiesEnrollments$ID.session), colnames(HumanitiesEnrollments) != "ID.session"]
  HumanitiesEnrollments$Course_Number = CourseLookup$Course.Number[match(HumanitiesEnrollments$`[02]course_name`, CourseLookup$Course.Name)]
  
  if(sum(is.na(HumanitiesEnrollments)) == 0){
    print("Yay!  Humanities Enrollments appear to have gone well.")
    cctable = rbind.data.frame(cctable, HumanitiesEnrollments)     #put humanities enrollments in cctable
  }  else {
    print("WARNING!  Humanities Enrollments did not work.  Rerun this section manually.")
  }
} else {
  print("No Humanities sections found.")
}


#--------------------------------#
#### Create table of sections ####
#--------------------------------#

sections = data.frame(session = unique(cctable$session), stringsAsFactors = F)
for (i in 1:nrow(sections)){
  sections$course[i] = paste0(unique(cctable$`[02]course_name`[cctable$session == sections$session[i]]), collapse = ", ")
  sections$teacher[i] = paste0(unique(cctable$`[05]lastfirst`[cctable$session == sections$session[i]]), collapse = ", ")
  sections$period[i] = paste0(unique(cctable$period[cctable$session == sections$session[i]]), collapse = ", ")
  sections$sectNumber[i] = paste0(unique(cctable$section_number[cctable$session == sections$session[i]]), collapse = ", ")
  sections$courseNumber[i] = paste0(unique(cctable$Course_Number[cctable$session == sections$session[i]]), collapse = ", ")
  sections$ScantronCourseNumber[i] = paste0(unique(cctable$ScantronCourseNumber[cctable$session == sections$session[i]]), collapse = ", ")
  sections$ScantronPeriod[i] = paste0(unique(cctable$ScantronPeriod[cctable$session == sections$session[i]]), collapse = ", ")
}
sections$CourseSection = paste0(sections$course, " -- ", sections$sectNumber)

# Add staff ID's
sections$Staff1 = ""
sections$Staff2 = ""
sections$Staff3 = ""
sections$Staff4 = ""
sections$Staff5 = ""
sections$Staff6 = ""
for (i in 1:nrow(sections)){
  staffSet = unique(cctable$`[05]lastfirst`[cctable$session == sections$session[i]])
  staffIDset = TeacherLookup$Primary.Staff[match(staffSet, TeacherLookup$TeacherName)]
  for(j in 1:(min(length(staffSet),6))){
    sections[i,paste0("Staff",j)] = staffSet[j]
  }
}


# Take sections that have a double name and give them a shorter name
ComboSections = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "CombinedSections")
ComboSections$v1 = paste0(ComboSections$CourseName1, ", ", ComboSections$CourseName2)
ComboSections$v2 = paste0(ComboSections$CourseName2, ", ", ComboSections$CourseName1)
for(i in 1:nrow(ComboSections)){
  sections$course[sections$course %in% ComboSections[i,c("v1","v2")]] = ComboSections$CombinedName[i]
}

sections$ScantronName = CourseLookup$Course.Name[match(sections$course, CourseLookup$PowerSchool.Name)]


# Find leftover sections
if(sum(is.na(sections)) > 0){
  leftoverEnrollments = cctable[cctable$session %in% sections$session[is.na(sections$ScantronName)],]
  leftoverSections = data.frame(CourseSection = unique(leftoverEnrollments$CourseSection), stringsAsFactors = F)
  leftoverSections$session = leftoverEnrollments$session[match(leftoverSections$CourseSection, leftoverEnrollments$CourseSection)]
  leftoverSections$course = leftoverEnrollments$`[02]course_name`[match(leftoverSections$CourseSection, leftoverEnrollments$CourseSection)]
  
  
  
  for (i in 1:nrow(leftoverSections)){
    leftoverSections$teacher[i] = paste0(unique(leftoverEnrollments$`[05]lastfirst`[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), 
                                         collapse = ", ")
    leftoverSections$period[i] = paste0(unique(leftoverEnrollments$period[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), 
                                        collapse = ", ")
    leftoverSections$sectNumber[i] = paste0(unique(leftoverEnrollments$section_number[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), 
                                            collapse = ", ")
    leftoverSections$courseNumber[i] = paste0(unique(leftoverEnrollments$Course_Number[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), 
                                              collapse = ", ")
    leftoverSections$ScantronCourseNumber[i] = paste0(unique(leftoverEnrollments$ScantronCourseNumber[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), 
                                                      collapse = ", ")
    leftoverSections$ScantronPeriod[i] = paste0(unique(leftoverEnrollments$ScantronPeriod[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), 
                                                collapse = ", ")
  }
  leftoverSections = leftoverSections[,c(2:9,1)]
  
  # Add staff ID's
  leftoverSections$Staff1 = ""
  leftoverSections$Staff2 = ""
  leftoverSections$Staff3 = ""
  leftoverSections$Staff4 = ""
  leftoverSections$Staff5 = ""
  leftoverSections$Staff6 = ""
  for (i in 1:nrow(leftoverSections)){
    staffSet = unique(leftoverEnrollments$`[05]lastfirst`[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]])
    staffIDset = TeacherLookup$Primary.Staff[match(staffSet, TeacherLookup$TeacherName)]
    for(j in 1:(min(length(staffSet),6))){
      leftoverSections[i,paste0("Staff",j)] = staffSet[j]
    }
  }
  
  leftoverSections$ScantronName = CourseLookup$Course.Name[match(leftoverSections$course, CourseLookup$PowerSchool.Name)]  
  
  sections = sections[!is.na(sections$ScantronName),]  
  sections = rbind.data.frame(sections, leftoverSections, stringsAsFactors = F)  
} else {
  print("Yay!  No leftover sections.")
}


sections$ScantronCourseNumber = CourseLookup$ScantronCourseNumber[match(sections$course, CourseLookup$PowerSchool.Name)]

if(sum(is.na(sections)) > 0){
  print("WARNING!  There are problems with the sections table.") 
} else {
  print("Yay!  No problems with the sections table.")
}


# Fix Bio R/B enrollments
BioRBsections = sections[sections$course == "Biology R/B",]
BioRBsections = BioRBsections[order(BioRBsections$teacher),]
rownames(BioRBsections) = NULL
BioRBsections$sectNumber = BioRBsections$ScantronPeriod
BioRBsections$sectNumber[duplicated(BioRBsections$sectNumber)] = paste0("1", BioRBsections$sectNumber[duplicated(BioRBsections$sectNumber)])
BioRBsections$courseNumber = "BioRB"
for(i in 1:nrow(cctable)){
  if(cctable$session[i] %in% BioRBsections$session){
    cctable$section_number[i] = BioRBsections$sectNumber[match(cctable$session[i], BioRBsections$session)]
    cctable$`[02]course_name`[i] = "Biology R/B"
    cctable$Course_Number[i] = "BioRB"
  }
}
sections = sections[sections$course != "Biology R/B",]
sections = rbind.data.frame(sections, BioRBsections, stringsAsFactors = F)

# Fix Alg1 R/I enrollments
Alg1RIsections = sections[sections$course == "Alg1 R/I",]
Alg1RIsections = Alg1RIsections[order(Alg1RIsections$teacher),]
rownames(Alg1RIsections) = NULL
Alg1RIsections$sectNumber = Alg1RIsections$ScantronPeriod
Alg1RIsections$sectNumber[duplicated(Alg1RIsections$sectNumber)] = paste0("1", Alg1RIsections$sectNumber[duplicated(Alg1RIsections$sectNumber)])
Alg1RIsections$courseNumber = "Alg1RI"
for(i in 1:nrow(cctable)){
  if(cctable$session[i] %in% Alg1RIsections$session){
    cctable$section_number[i] = Alg1RIsections$sectNumber[match(cctable$session[i], Alg1RIsections$session)]
    cctable$`[02]course_name`[i] = "Alg1 R/I"
    cctable$Course_Number[i] = "Alg1RI"
  }
}
sections = sections[sections$course != "Alg1 R/I",]
sections = rbind.data.frame(sections, Alg1RIsections, stringsAsFactors = F)


# Fix AlgA enrollments (change to coteacher)
# cctable$`[05]lastfirst`[cctable$`[05]lastfirst` == "Fedele, Marisa"] = "Fedele/Maude, ."
# sections$Staff1[sections$Staff1 == "Fedele, Marisa"] = "Fedele/Maude, ."
# sections$Staff2[sections$Staff1 == "Fedele/Maude, ."] = "Fedele, Marisa"
# sections$Staff3[sections$Staff1 == "Fedele/Maude, ."] = "Maude, Katricia"


if(nrow(HumanitiesSections) > 0) {
  # Fix Humanities sections (add individual teachers)
  sections$Staff2[sections$Staff1 == "Griffin/Krizar, ."] = "Griffin, Terrin"
  sections$Staff3[sections$Staff1 == "Griffin/Krizar, ."] = "Krizar, Elizabeth"
  sections$Staff2[sections$Staff1 == "Zalucki/Snyder, ."] = "Zalucki, Kristen"
  sections$Staff3[sections$Staff1 == "Zalucki/Snyder, ."] = "Snyder, Kara"
  sections$Staff2[sections$Staff1 == "Remington/Mackenzie, ."] = "Remington, Ted"
  sections$Staff3[sections$Staff1 == "Remington/Mackenzie, ."] = "Mackenzie, Peter"
}

# Fix Alg2/IT enrollments
Alg2ITsections = sections[sections$course == "Alg2/IT",]
Alg2ITsections = Alg2ITsections[order(Alg2ITsections$teacher),]
rownames(Alg2ITsections) = NULL
Alg2ITsections$sectNumber = Alg2ITsections$ScantronPeriod
Alg2ITsections$sectNumber[duplicated(Alg2ITsections$sectNumber)] = paste0("1", Alg2ITsections$sectNumber[duplicated(Alg2ITsections$sectNumber)])
Alg2ITsections$courseNumber = "Alg2IT"
for(i in 1:nrow(cctable)){
  if(cctable$session[i] %in% Alg2ITsections$session){
    cctable$section_number[i] = Alg2ITsections$sectNumber[match(cctable$session[i], Alg2ITsections$session)]
    cctable$`[02]course_name`[i] = "Alg2/IT"
    cctable$Course_Number[i] = "Alg2IT"
  }
}
sections = sections[sections$course != "Alg2/IT",]
sections = rbind.data.frame(sections, Alg2ITsections, stringsAsFactors = F)


# Fix PE 9/10 enrollments
# This is only required if the classes are separated as PE9 and PE10.
# PE910sections = sections[sections$course == "PE 9/10",]
# PE910sections$sectNumber = 1:nrow(PE910sections)
# PE910sections$courseNumber = "PE910"
# for(i in 1:nrow(cctable)){
#   if(cctable$session[i] %in% PE910sections$session){
#     cctable$section_number[i] = PE910sections$sectNumber[match(cctable$session[i], PE910sections$session)]
#     cctable$`[02]course_name`[i] = "PE 9/10"
#     cctable$Course_Number[i] = "PE 9/10"
#   }
# }
# sections = sections[sections$course != "PE 9/10",]
# sections = rbind.data.frame(sections, PE910sections, stringsAsFactors = F)



# Fix US R/Review enrollments
USsections = sections[sections$course == "US History R/Rev",]
USsections = USsections[order(USsections$teacher),]
rownames(USsections) = NULL
USsections$sectNumber = USsections$ScantronPeriod
USsections$sectNumber[duplicated(USsections$sectNumber)] = paste0("1", USsections$sectNumber[duplicated(USsections$sectNumber)])
USsections$courseNumber = "USRR"
for(i in 1:nrow(cctable)){
  if(cctable$session[i] %in% USsections$session){
    cctable$section_number[i] = USsections$sectNumber[match(cctable$session[i], USsections$session)]
    cctable$`[02]course_name`[i] = "US History R/Rev"
    cctable$Course_Number[i] = "USRR"
  }
}
sections = sections[sections$course != "US History R/Rev",]
sections = rbind.data.frame(sections, USsections, stringsAsFactors = F)






sort(unique(sections$course))


# Fix course names and numbers
cctable$ScantronCourseName = CourseLookup$Course.Name[match(cctable$`[02]course_name`, CourseLookup$PowerSchool.Name)]
cctable$ScantronCourseNumber = CourseLookup$ScantronCourseNumber[match(cctable$`[02]course_name`, CourseLookup$PowerSchool.Name)]

if(sum(is.na(cctable)) > 0){
  print("WARNING!  There are missing values in the cctable.")
} else {
  print("Yay!  No missing values in the cctable.")
}



# Load table of students from PowerSchool
# need an export of all sutdents with the following variables:
# student_number  grade_level   Last_Name   First_Name
students = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "currentstudents")

# check for missing staff
staff = unique(cctable$`[05]lastfirst`)
staff.missing = staff[!(staff %in% TeacherLookup$LastFirst)]
if(length(staff.missing) > 0){
  print("WARNING!  The following staff are missing from the TeacherLookup tab of the parameters and settings xlsx file.")
  print(staff.missing)
} else {
  print("Yay!  No staff are missing.")
}



#create the table of courses
courses = sections[!duplicated(sections$ScantronCourseNumber),]
courses$Course.Category = CourseLookup$Course.Category[match(courses$ScantronCourseNumber, CourseLookup$ScantronCourseNumber)]

if(sum(is.na(courses)) > 0){
  print("WARNING!  There are missing values in the courses table.")
} else {
  print("Yay!  No missing values in the courses table.")
}


#--------------------------------#
#### Produce the import files ####
#--------------------------------#

# The student file
in.student = read.csv("GTHimport/students.csv", stringsAsFactors = F)
Site.ID = in.student$Site.ID[1]
Site.ID = "40-4070-3155"
out.student = as.data.frame(matrix(data = NA_character_,
                                   nrow = nrow(students), 
                                   ncol = ncol(in.student)),
                            stringsAsFactors = F)
colnames(out.student) = colnames(in.student)
out.student$Site.ID = Site.ID
out.student$Student.ID = students$Student_number
out.student$First.Name = students$First_Name
out.student$Last.Name = students$Last_Name
out.student$Current.Grade = students$Grade_Level


write.csv(out.student, file = "GTHimport/students.csv", na = "", row.names = F)


# The enrollments file
in.enroll = read.csv("GTHimport/enrollments.csv", stringsAsFactors = F)
out.enroll = as.data.frame(matrix(data = NA_character_,
                                  nrow = nrow(cctable), 
                                  ncol = ncol(in.enroll)),
                           stringsAsFactors = F)
colnames(out.enroll) = colnames(in.enroll)
out.enroll$Site.ID = Site.ID
out.enroll$Student.ID = cctable$`[01]Student_Number`
out.enroll$Course.Number = cctable$ScantronCourseNumber
out.enroll$Section.Number = cctable$section_number

write.csv(out.enroll, file = "GTHimport/enrollments.csv", na = "", row.names = F)


# The Staff file
in.staff = read.csv("GTHimport/Staff.csv", stringsAsFactors = F)
out.staff = as.data.frame(matrix(data = NA_character_,
                                 nrow = nrow(TeacherLookup), 
                                 ncol = ncol(in.staff)),
                          stringsAsFactors = F)
colnames(out.staff) = colnames(in.staff)
out.staff$Site.ID = Site.ID
out.staff$Staff.ID = TeacherLookup$Staff.ID
out.staff$First.Name = TeacherLookup$First.Name
out.staff$Last.Name = TeacherLookup$TeacherName
out.staff$Email = TeacherLookup$email
out.staff$Password = "password1"
out.staff$Security.Position = TeacherLookup$Security.Position

write.csv(out.staff, file = "GTHimport/Staff.csv", na = "", row.names = F)


# The classes file
# this comes from the sections data.frame
in.classes = read.csv("GTHimport/Classes.csv", stringsAsFactors = F)
out.classes = as.data.frame(matrix(data = NA_character_,
                                   nrow = nrow(sections), 
                                   ncol = ncol(in.classes)),
                            stringsAsFactors = F)
colnames(out.classes) = colnames(in.classes)
out.classes$Site.ID = Site.ID
out.classes$Course.Number = sections$ScantronCourseNumber
out.classes$Section.Number = sections$sectNumber
out.classes$Lower.Grade = 6
out.classes$Upper.Grade = 12
out.classes$Period = sections$ScantronPeriod
out.classes$Primary.Staff.ID = TeacherLookup$Staff.ID[match(sections$Staff1, TeacherLookup$LastFirst)]
out.classes$Staff.ID..2. = TeacherLookup$Staff.ID[match(sections$Staff2, TeacherLookup$LastFirst)]
out.classes$Staff.ID..3. = TeacherLookup$Staff.ID[match(sections$Staff3, TeacherLookup$LastFirst)]

write.csv(out.classes, file = "GTHimport/Classes.csv", na = "", row.names = F)


# The courses file
in.courses = read.csv("GTHimport/Courses.csv", stringsAsFactors = F)
out.courses = as.data.frame(matrix(data = NA_character_,
                                   nrow = nrow(courses), 
                                   ncol = ncol(in.courses)),
                            stringsAsFactors = F)
colnames(out.courses) = colnames(in.courses)
out.courses$Course.Number = courses$ScantronCourseNumber
out.courses$Course.Name = courses$ScantronName
out.courses$Course.Category = courses$Course.Category
out.courses$SubjectAreas = "None"
out.courses$Lower.Grade = 6
out.courses$Upper.Grade = 12


write.csv(out.courses, file = "GTHimport/Courses.csv", na = "", row.names = F)

# Now go upload that shit!


#----------------------------------------------------------------------------#
#### This is some other code.  I think it looks for duplicate enrollments ####
#----------------------------------------------------------------------------#
out.enroll2 = out.enroll
out.enroll$code = paste0(out.enroll$Student.ID,out.enroll$Course.Number)
enrolldups = out.enroll[duplicated(out.enroll$code),]

cctable$code = paste0(cctable$`[01]Student_Number`, cctable$Course_Number)

cctable.dups = cctable[cctable$code %in% enrolldups$code,]

out.enroll2 = out.enroll2[!duplicated(out.enroll$code),]
str(out.enroll2)
write.csv(out.enroll2, file = "GTHimport/enrollments.csv", na = "", row.names = F)


out.enroll$code2 = paste0(out.enroll$Course.Number,"-",out.enroll$Section.Number)
out.classes$code2 = paste0(out.classes$Course.Number, "-", out.classes$Section.Number)


setdiff(unique(out.enroll$code2), unique(out.classes$code2))
setdiff(unique(out.classes$code2), unique(out.enroll$code2))



out.enroll[out.enroll$Student.ID == "161710468",]
