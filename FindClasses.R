#FindClasses.R

FindClasses = function(ScantronHandle){
  
  #Set the URL's to be used
  url1 = "https://admin.achievementseries.com/classes/list.ssp"                #set the url for the class list page
  url2 = "https://admin.achievementseries.com/classes/list.csv?_list=Classes"  #set the url for the csv of classes
  
  #Pull the classes page in order to get the ClassID from the links
  x = getURI(url1, curl=ScantronHandle)                           #fetch the page with the list of classes
  page = htmlParse(x)                                             #convert the page to a parsed html object
  links = xpathSApply(page, "//a/@href")                          #this finds all of the links in the document
  ClassLinks = substr(links[grep("info.ssp\\?id=",links)],20,35)  #this pulls out the ClassID's from the links
  
  #Pull the csv with the rest of the class info
  x = getURI(url2, curl=ScantronHandle)                              #pull the csv of classes
  ClassFrame = read.csv(textConnection(x), stringsAsFactors = FALSE) #treat the page as a csv and make a data.frame
  ClassFrame$ClassID = ClassLinks                                    #add the ClassID's to the dataframe of classes
  
  #Find important locations in the character strings holding the class names, section numbers, and period numbers
  m = unlist(regexec(pattern = "Sec. " ,text = ClassFrame$Class))  #location of section number
  p = unlist(regexec(pattern = "," ,text = ClassFrame$Class))      #location of comma ending the section number
  r = unlist(regexec(pattern = "Block " ,text = ClassFrame$Class)) #location of period number
  n = nchar(ClassFrame$Class)                                      #location end of period number (last character)
  
  #Add columns to ClassFrame to hold the section, period, and class name
  ClassFrame$Section = substr(ClassFrame$Class, m+5, p-1)
  ClassFrame$Period = substr(ClassFrame$Class, r+6, n)
  ClassFrame$ClassName = substr(ClassFrame$Class, 1, unlist(m)-1)
  
  #Use the class and teacher lookup tables to add the abbreviated teacher name, short course name, and academic department
  ClassFrame$TeacherName = TeacherLookup$TeacherName[match(x = ClassFrame$Primary.Staff, table = TeacherLookup$Primary.Staff)]
  ClassFrame$Dept = CourseLookup$Dept[match(x = ClassFrame$ClassName, table = CourseLookup$ClassName)]
  ClassFrame$ShortName = CourseLookup$ShortName[match(x = ClassFrame$ClassName, table = CourseLookup$ClassName)]
  ClassFrame$Level = CourseLookup$Level[match(x = ClassFrame$ClassName, table = CourseLookup$ClassName)]
  
  
  return(ClassFrame)
} #end of FindClasses() function