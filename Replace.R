
Replace = function(x, list, values) { 
  # x : data
  # list : old values
  # values : new values
  xx = c()
  if(length(list) != length(values)) {
    if( length(values) == 1) {
      values = rep(values, length(list) )
  }else { 
    if(  length(values) > 1) stop("The length of list and values must be one or equal.") 
  }}
    
    if(any( duplicated(list)) ) warning("The values of list are not unique. The last values was replaced") 
  
  xx <- rep(NA, length(x))
  for(i in 1:length(list)) {
    xx[which(x == list[i])]  = values[i]
  }
  if((c(NA) %in% list)){
    na.i =  which(is.na(list))
    if(length(na.i)>1) warning("The NA duplicated in the list variable. value: ", values[na.i[length(na.i)]], " was replaced for NA" )
    xx[which(is.na(x))] <- values [na.i[length(na.i)]]
  }

  if(length(which(is.na(xx))) != 0)
    xx[which(is.na(xx))] = x [which(is.na(xx))] 
  
  if(is.character(xx)) warning("The type of output values are character, maybe you need to change numeric by 'as.numeric' function.")
  xx
}



#ff= Replace(x=c(1:10, NA,"k") ,list= c(2:6  ),values= 10000)
   
   
   
   
   
