# HumanitiesRosters.R

# Export all course assignments from PowerSchool (cc table)
source("functions.R") #load the functions

humCourseLists = list(
  "H2" = list(
    "SSt" = list("Courses" = unique(unlist(HumanitiesSections$SScourse[HumanitiesSections$Level == 2])),
                 "Students" = character(0)),
    "ELA" = list("Courses" = unique(unlist(HumanitiesSections$ELAcourse[HumanitiesSections$Level == 2])),
                 "Students" = character(0))),
  "H3" = list(
    "SSt" = list("Courses" = unique(unlist(HumanitiesSections$SScourse[HumanitiesSections$Level == 3])),  
                 "Students" = character(0)
    ),
    "ELA" = list("Courses" = unique(unlist(HumanitiesSections$ELAcourse[HumanitiesSections$Level == 3])),  
                 "Students" = character(0)))
) # /humCourseNames

for(i in 1:length(humCourseLists)){
  for(j in 1:length(humCourseLists[[i]])){
    humCourseLists[[i]][[j]]$Students = cctable$`[01]Student_Number`[cctable$`[02]course_name` %in% humCourseLists[[i]][[j]]$Courses]
  } # /subject loop
  humCourseLists[[i]]$Both = intersect(humCourseLists[[i]]$SSt$Students,humCourseLists[[i]]$ELA$Students)
  humCourseLists[[i]]$SStOnly = setdiff(humCourseLists[[i]]$SSt$Students, humCourseLists[[i]]$Both)
  humCourseLists[[i]]$ELAOnly = setdiff(humCourseLists[[i]]$ELA$Students, humCourseLists[[i]]$Both)
} # /course loop



HTable = data.frame(
  Level = c(
    rep(2, times = length(humCourseLists$H2$SStOnly) + length(humCourseLists$H2$ELAOnly)),
    rep(3, times = length(humCourseLists$H3$SStOnly) + length(humCourseLists$H3$ELAOnly))),
  StudentID = c(humCourseLists$H2$SStOnly,humCourseLists$H2$ELAOnly,humCourseLists$H3$SStOnly,humCourseLists$H3$ELAOnly),
  Course = c(
    rep("SSt", times = length(humCourseLists$H2$SStOnly)), 
    rep("ELA", times = length(humCourseLists$H2$ELAOnly)),
    rep("SSt", times = length(humCourseLists$H3$SStOnly)), 
    rep("ELA", times = length(humCourseLists$H3$ELAOnly))),
  stringsAsFactors = F)



writeData(wb = TAB.wb, sheet = "HumanitiesScoring", x = HTable)
saveWorkbook(wb = TAB.wb, file = TABpath, overwrite = T)
