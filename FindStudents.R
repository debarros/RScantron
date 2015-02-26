FindStudents = function (ScantronHandle){
  x = getURI('https://admin.achievementseries.com/students/list.ssp', curl=ScantronHandle)
  page = htmlParse(x)
  links = xpathSApply(page, "//a/@href")               # this finds all of the links in the document
  StudentLinks = substr(links[grep("info.ssp\\?id=",links)],29,44)    
  
  # The object "Location" holds the starting points of the sid codes, which are all of the same length
  Location = data.frame(integer(0))                    # initialize the location data.frame
  for (i in 1:length(StudentLinks)){                      # fill the Location data.frame with starting points of the sID codes
    Location[i,1] = as.integer(
      gregexpr(pattern = StudentLinks[i],x)[[1]][1])
  }  
  #Append to the Location data.frame the starting position of each student name
  Location = cbind(Location, Location + 17)
  colnames(Location) = c("sidStart","StudentNameStart")
  
  #find the ending point of each entry in the table
  ends = dcast(
    melt(
      str_locate_all(
        pattern = '</span></td>',  #this is the code that always follows table entries
        x)[[1]]), 
    formula = Var1 ~ Var2)[,2]  #the -1 moves from 1st char after the entry to the last char of the entry
  
  #find the beginning point of each entry in the table
  starts = dcast(
    melt(
      str_locate_all(
        pattern = '<span class="ss2">',  #this is the code that always follows test names
        x)[[1]]), 
    formula = Var1 ~ Var2)[,2]+17  #the 18 moves from 1st char of the pattern to the 1st char of the entry
  
  #initialize the bounds data.frame
  bounds = data.frame(starts, stringsAsFactors = FALSE)
  
  #add columns to hold the ends and the content
  blanks = rep(NA, times = nrow(bounds))
  bounds = cbind(bounds,ends = blanks,content = blanks, type = blanks, item = blanks)
  
  #match up the starts and ends, discarding duplicates
  for (i in 1:nrow(bounds)){
    bounds$ends[i] = min(ends[which(ends >= bounds$starts[i])])
    if(i>1){
      if (bounds$ends[i] == bounds$ends[i-1]){
        bounds[i-1,] = c(NA,NA,NA,NA,NA)
      }  
    }
  }
  
  #Find all the discarded duplicate rows and, if there are any, get rid of them.
  drop = which(is.na(bounds$starts))
  if (length(drop)>0){
    bounds = bounds[-drop,]
  }
  rownames(bounds) <- seq(length=nrow(bounds))
  
  #load the content of the entries
  for (i in 1:nrow(bounds)){
    bounds$content[i] = as.character(substr(x, bounds$starts[i], bounds$ends[i]))
  }
  
  # this will identify what type of row it is
  indices = seq.int(from = 1, to = nrow(bounds),by = 4)
  for (i in 0:3){
    bounds$type[indices + i] = i + 1
  }
  
  # this will identify which student it is
  bounds$item = floor((as.double(rownames(bounds))+3)/4)
  
  bounds$content = substr(bounds$content,2,nchar(bounds$content)-1)
  
  
  #this will convert the data.frame into wide format, with one row per student
  bounds = dcast(bounds, formula = item ~ type, value.var = "content")
  
  # this will replace unnecessary columns with the student name and sid
  bounds[,1] = as.character(substr(bounds[,2], 31,46))
  bounds[,4] = as.character(substr(bounds[,2], 49,nchar(bounds[,2])-4))
  
  #this will drop the link text column
  bounds = bounds[,c(1,3,4)]
  colnames(bounds) = c("sid","StNumber","StName")
  
  return(bounds)
  
}