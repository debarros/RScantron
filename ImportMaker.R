# ImportMaker.R

# This process requires a few files
# Export all current students from PowerSchool
# Export all course assignments from PowerSchool

source("functions.R") #load the functions

# Get the course-subject alignments ####
# Sign in to google
#gs_auth() #this will launch a browser so you can sign into your account
CourseSubject = gs_url("https://docs.google.com/a/greentechhigh.org/spreadsheets/d/17QhVYZkjbx34M6wBvtHUYa_XrRUlRbOtuOsQ4P5l-nk/edit?usp=sharing")
alignment = gs_read(ss = CourseSubject)

# Read in data ####
# Read the export of the cc(4) table from PowerSchool
# Make sure you have only exported the current year
# variables needed: [01]last_name	[01]first_name	[01]Student_Number	[01]grade_Level	[02]course_name	section_number	expression	[03]room	[05]lastfirst	Course_Number	DateEnrolled	DateLeft	TermID
cctable.raw = read.csv("ccTableExport.csv", stringsAsFactors = FALSE) 
cctable = cctable.raw
cctable$subject = alignment$Subject[match(cctable$X.02.course_name, alignment$Course)] #add subject variable
cctable = cctable[cctable$subject != "Delete",]
dropCourses = c("Physical Education- 10th grade 1 credit","Physical Education- 9th grade 1 credit","Art II, Art I","Art I","Art II", "Exploring Algebra with Technology")
cctable = cctable[!(cctable$X.02.course_name %in% dropCourses), ]
cctable$period = substr(cctable$expression,1,1)
cctable$ScantronPeriod = cctable$period  #usually, the scantron period is the same as the actual period
cctable$session = paste0(cctable$X.03.room," -- ", cctable$period, " -- ",cctable$TermID)
cctable$CourseSection = paste0(cctable$X.02.course_name, " -- ",cctable$section_number)


#remove spaces from course numbers
cctable$ScantronCourseNumber = gsub(pattern = " ", replacement = "", x = cctable$Course_Number)
cctable[!(cctable$Course_Number %in% CourseLookup$Course.Number),]


# Create Humanities Enrollments ####
HumanitiesSections = data.frame(
  X.02.course_name = c("Global History & Lit 2 H","Global History & Lit 2 H","Global History & Lit 2 R", "Global History & Lit 2 R", "US History & Lit H", "US History & Lit R", "US History & Lit R", "Global History & Lit 2 R"),
  section_number = c(1, 2, 1, 2, 1, 1, 2, 1),
  period = c("7/8","2/3","4/6","6/7","7/8","2/3","5/6", "4/6"),
  ScantronPeriod = c(7, 2, 4, 6, 7, 2, 5, 4),
  SScourse = c("Global History II Honors- 1 credit", "Global History II Honors- 1 credit", "Global History II 1-credit", "Global History II 1-credit", "US History Honors-1 credit", "US History 1 credit", "US History 1 credit", "Global History II 1-credit"),
  SSsection = c(2, 3, 6, 2, 1, 1, 2, 6),
  ELAcourse = c("Literature 10 Honors- 1 credit", "Literature 10 Honors- 1 credit", "Literature 10- 1 credit", "Literature 10- 1 credit", "Literature 11 Honors", "Literature 11- 1 credit", "Literature 11- 1 credit", "Literature 9- 1 credit"),
  ELAsection = c(7, 2, 8, 1, 1, 1, 2, 1),
  teacher = c("D'Arcangelis/Griffin, .", "D'Arcangelis/Griffin, .", "D'Arcangelis/Griffin, .","Zalucki/Snyder, .","Remington/Mackenzie, .", "Remington/Mackenzie, .", "Remington/Mackenzie, .", "Zalucki/Snyder, ."),
  stringsAsFactors = F)


for(i in 1:nrow(HumanitiesSections)){
  HumanitiesSections$ELAsession[i] = cctable$session[(cctable$X.02.course_name == HumanitiesSections$ELAcourse[i] & cctable$section_number == HumanitiesSections$ELAsection[i])][1]
  HumanitiesSections$SStSession[i] = cctable$session[cctable$X.02.course_name == HumanitiesSections$SScourse[i] & cctable$section_number == HumanitiesSections$SSsection[i] ][1]
  HumanitiesSections$room[i] = cctable$X.03.room[cctable$X.02.course_name == HumanitiesSections$SScourse[i] & cctable$section_number == HumanitiesSections$SSsection[i] ][1]
  HumanitiesSections$term[i] = cctable$TermID[cctable$X.02.course_name == HumanitiesSections$SScourse[i] & cctable$section_number == HumanitiesSections$SSsection[i] ][1]
}

HumanitiesEnrollments1 = cctable[cctable$session %in% c(HumanitiesSections$ELAsession),]
HumanitiesEnrollments1$X.02.course_name = HumanitiesSections$X.02.course_name[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
HumanitiesEnrollments1$section_number = HumanitiesSections$section_number[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
HumanitiesEnrollments1$period = HumanitiesSections$period[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
HumanitiesEnrollments1$ScantronPeriod = HumanitiesSections$ScantronPeriod[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
HumanitiesEnrollments1$X.03.room = HumanitiesSections$room[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
HumanitiesEnrollments1$TermID = HumanitiesSections$term[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]
HumanitiesEnrollments1$X.05.lastfirst = HumanitiesSections$teacher[match(HumanitiesEnrollments1$session, HumanitiesSections$ELAsession)]

HumanitiesEnrollments2 = cctable[cctable$session %in% c(HumanitiesSections$SStSession),]
HumanitiesEnrollments2$X.02.course_name = HumanitiesSections$X.02.course_name[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
HumanitiesEnrollments2$section_number = HumanitiesSections$section_number[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
HumanitiesEnrollments2$period = HumanitiesSections$period[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
HumanitiesEnrollments2$ScantronPeriod = HumanitiesSections$ScantronPeriod[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
HumanitiesEnrollments2$X.03.room = HumanitiesSections$room[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
HumanitiesEnrollments2$TermID = HumanitiesSections$term[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]
HumanitiesEnrollments2$X.05.lastfirst = HumanitiesSections$teacher[match(HumanitiesEnrollments2$session, HumanitiesSections$SStSession)]

HumanitiesEnrollments = rbind.data.frame(HumanitiesEnrollments1, HumanitiesEnrollments2)
HumanitiesEnrollments$session = paste0(HumanitiesEnrollments$X.03.room," -- ", HumanitiesEnrollments$period, " -- ",HumanitiesEnrollments$TermID)
HumanitiesEnrollments$ID.session = paste0(HumanitiesEnrollments$X.01.Student_Number, " -- ", HumanitiesEnrollments$session)
HumanitiesEnrollments = HumanitiesEnrollments[!duplicated(HumanitiesEnrollments$ID.session), colnames(HumanitiesEnrollments) != "ID.session"]
HumanitiesEnrollments$Course_Number = CourseLookup$Course.Number[match(HumanitiesEnrollments$X.02.course_name, CourseLookup$Course.Name)]

#put humanities enrollments in cctable
cctable = rbind.data.frame(cctable, HumanitiesEnrollments)



# Create table of sections ####
sections = data.frame(session = unique(cctable$session), stringsAsFactors = F)
for (i in 1:nrow(sections)){
  sections$course[i] = paste0(unique(cctable$X.02.course_name[cctable$session == sections$session[i]]), collapse = ", ")
  sections$teacher[i] = paste0(unique(cctable$X.05.lastfirst[cctable$session == sections$session[i]]), collapse = ", ")
  sections$period[i] = paste0(unique(cctable$period[cctable$session == sections$session[i]]), collapse = ", ")
  sections$sectNumber[i] = paste0(unique(cctable$section_number[cctable$session == sections$session[i]]), collapse = ", ")
  sections$courseNumber[i] = paste0(unique(cctable$Course_Number[cctable$session == sections$session[i]]), collapse = ", ")
  sections$ScantronCourseNumber[i] = paste0(unique(cctable$ScantronCourseNumber[cctable$session == sections$session[i]]), collapse = ", ")
  sections$ScantronPeriod[i] = paste0(unique(cctable$ScantronPeriod[cctable$session == sections$session[i]]), collapse = ", ")
}
sections$CourseSection = paste0(sections$course, " -- ", sections$sectNumber)

#Add staff ID's
sections$Staff1 = ""
sections$Staff2 = ""
sections$Staff3 = ""
sections$Staff4 = ""
sections$Staff5 = ""
sections$Staff6 = ""
for (i in 1:nrow(sections)){
  staffSet = unique(cctable$X.05.lastfirst[cctable$session == sections$session[i]])
  staffIDset = TeacherLookup$Primary.Staff[match(staffSet, TeacherLookup$TeacherName)]
  for(j in 1:(min(length(staffSet),6))){
    sections[i,paste0("Staff",j)] = staffSet[j]
  }
}



sections$course[sections$course %in% c("Biology Part B, Biology 1 credit", "Biology 1 credit, Biology Part B")] = "Biology R/B"
sections$course[sections$course %in% c("Intermediate Algebra 10, Integrated Algebra I- 1 credit", "Integrated Algebra I- 1 credit, Intermediate Algebra 10")] = "Alg1 R/I"


sections$ScantronName = CourseLookup$Course.Name[match(sections$course, CourseLookup$PowerSchool.Name)]

#find leftover sections
sections[is.na(sections$ScantronName),]
leftoverEnrollments = cctable[cctable$session == sections$session[is.na(sections$ScantronName)],]
leftoverSections = data.frame(CourseSection = unique(leftoverEnrollments$CourseSection), stringsAsFactors = F)
leftoverSections$session = leftoverEnrollments$session[match(leftoverSections$CourseSection, leftoverEnrollments$CourseSection)]
leftoverSections$course = leftoverEnrollments$X.02.course_name[match(leftoverSections$CourseSection, leftoverEnrollments$CourseSection)]



for (i in 1:nrow(leftoverSections)){
  leftoverSections$teacher[i] = paste0(unique(leftoverEnrollments$X.05.lastfirst[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), collapse = ", ")
  leftoverSections$period[i] = paste0(unique(leftoverEnrollments$period[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), collapse = ", ")
  leftoverSections$sectNumber[i] = paste0(unique(leftoverEnrollments$section_number[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), collapse = ", ")
  leftoverSections$courseNumber[i] = paste0(unique(leftoverEnrollments$Course_Number[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), collapse = ", ")
  leftoverSections$ScantronCourseNumber[i] = paste0(unique(leftoverEnrollments$ScantronCourseNumber[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), collapse = ", ")
  leftoverSections$ScantronPeriod[i] = paste0(unique(leftoverEnrollments$ScantronPeriod[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]]), collapse = ", ")
}
leftoverSections = leftoverSections[,c(2:9,1)]

#Add staff ID's
leftoverSections$Staff1 = ""
leftoverSections$Staff2 = ""
leftoverSections$Staff3 = ""
leftoverSections$Staff4 = ""
leftoverSections$Staff5 = ""
leftoverSections$Staff6 = ""
for (i in 1:nrow(leftoverSections)){
  staffSet = unique(leftoverEnrollments$X.05.lastfirst[leftoverEnrollments$CourseSection == leftoverSections$CourseSection[i]])
  staffIDset = TeacherLookup$Primary.Staff[match(staffSet, TeacherLookup$TeacherName)]
  for(j in 1:(min(length(staffSet),6))){
    leftoverSections[i,paste0("Staff",j)] = staffSet[j]
  }
}
  
leftoverSections$ScantronName = CourseLookup$Course.Name[match(leftoverSections$course, CourseLookup$PowerSchool.Name)]  
  
sections = sections[!is.na(sections$ScantronName),]  
sections = rbind.data.frame(sections, leftoverSections, stringsAsFactors = F)


sections$ScantronCourseNumber = CourseLookup$ScantronCourseNumber[match(sections$course, CourseLookup$PowerSchool.Name)]


# Fix Bio R/B enrollments
BioRBsections = sections[sections$course == "Biology R/B",]
BioRBsections$sectNumber = 1:nrow(BioRBsections)
BioRBsections$courseNumber = "BioRB"
for(i in 1:nrow(cctable)){
  if(cctable$session[i] %in% BioRBsections$session){
    cctable$section_number[i] = BioRBsections$sectNumber[match(cctable$session[i], BioRBsections$session)]
    cctable$X.02.course_name[i] = "Biology R/B"
    cctable$Course_Number[i] = "BioRB"
  }
}
sections = sections[sections$course != "Biology R/B",]
sections = rbind.data.frame(sections, BioRBsections, stringsAsFactors = F)

# Fix Alg1 R/I enrollments
Alg1RIsections = sections[sections$course == "Alg1 R/I",]
Alg1RIsections$sectNumber = 1:nrow(Alg1RIsections)
Alg1RIsections$courseNumber = "Alg1RI"
for(i in 1:nrow(cctable)){
  if(cctable$session[i] %in% Alg1RIsections$session){
    cctable$section_number[i] = Alg1RIsections$sectNumber[match(cctable$session[i], Alg1RIsections$session)]
    cctable$X.02.course_name[i] = "Alg1 R/I"
    cctable$Course_Number[i] = "Alg1RI"
  }
}
sections = sections[sections$course != "Alg1 R/I",]
sections = rbind.data.frame(sections, Alg1RIsections, stringsAsFactors = F)


# Fix AlgA enrollments (change to coteacher)
cctable$X.05.lastfirst[cctable$X.05.lastfirst == "Shumway, Karissa"] = "Shumway/Santiago, ."
sections$Staff1[sections$Staff1 == "Shumway, Karissa"] = "Shumway/Santiago, ."
sections$Staff2[sections$Staff1 == "Shumway/Santiago, ."] = "Shumway, Karissa"
sections$Staff3[sections$Staff1 == "Shumway/Santiago, ."] = "Santiago, Noel"


# Fix Humanities sections (add individual teachers)
sections$Staff2[sections$Staff1 == "D'Arcangelis/Griffin, ."] = "Griffin, Terrin"
sections$Staff3[sections$Staff1 == "D'Arcangelis/Griffin, ."] = "D'Arcangelis, Esta"
sections$Staff2[sections$Staff1 == "Zalucki/Snyder, ."] = "Zalucki, Kristen"
sections$Staff3[sections$Staff1 == "Zalucki/Snyder, ."] = "Snyder, Kara"
sections$Staff2[sections$Staff1 == "Remington/Mackenzie, ."] = "Remington, Ted"
sections$Staff3[sections$Staff1 == "Remington/Mackenzie, ."] = "Mackenzie, Peter"



#Fix course names and numbers
cctable$ScantronCourseName = CourseLookup$Course.Name[match(cctable$X.02.course_name, CourseLookup$PowerSchool.Name)]
cctable$ScantronCourseNumber = CourseLookup$ScantronCourseNumber[match(cctable$X.02.course_name, CourseLookup$PowerSchool.Name)]




#load table of students from PowerSchool
#need an export of all sutdents with the following variables:
# student_number  grade_level   Last_Name   First_Name
students = read.csv("currentstudents.csv", stringsAsFactors = F)

#check for missing staff
staff = unique(cctable$X.05.lastfirst)
staff[!(staff %in% TeacherLookup$LastFirst)]



#create the table of courses
str(sections)
courses = sections[!duplicated(sections$ScantronCourseNumber),]
courses$Course.Category = CourseLookup$Course.Category[match(courses$ScantronCourseNumber, CourseLookup$ScantronCourseNumber)]


# Produce the import files ####

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
out.student$Student.ID = students$student_number
out.student$First.Name = students$First_Name
out.student$Last.Name = students$Last_Name
out.student$Current.Grade = students$grade_level

write.csv(out.student, file = "GTHimport/students.csv", na = "", row.names = F)


# The enrollments file
in.enroll = read.csv("GTHimport/enrollments.csv", stringsAsFactors = F)
out.enroll = as.data.frame(matrix(data = NA_character_,
                                   nrow = nrow(cctable), 
                                   ncol = ncol(in.enroll)),
                            stringsAsFactors = F)
colnames(out.enroll) = colnames(in.enroll)
out.enroll$Site.ID = Site.ID
out.enroll$Student.ID = cctable$X.01.Student_Number
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
out.classes$Lower.Grade = 9
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
out.courses$Lower.Grade = 9
out.courses$Upper.Grade = 12


write.csv(out.courses, file = "GTHimport/Courses.csv", na = "", row.names = F)






