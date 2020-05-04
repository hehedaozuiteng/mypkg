#' WeightCAL
#'
#' A tools to calculate the weight of each value in a dataset
#'
#'
#' @param dataframe the data to offer a set of date to find the weight
#'
#' @param remove a vectors of the name of the variable move away from the weight
#' calculate
#'
#' @param cate a vector of the name of variable should treat as vector but its
#' dataset is integer
#'
#' @param patten the method to calculate the each weight, when patten equals "a"
#' we add all wight togetherm, when patten equals "m", we mutiply them together.
#' when patten equal "l", we mutiple the logs of their weight
#'
#'
#' @return A vector of the weight
#'
#' @author
#' Henglin Huang
#'
#'
#' @export
#'
#'
weightCal = function(dataframe,remove,cate,patten = "a"){
  df = dataframe

  #remove unused varable
  df = df[ , -which(names(df) %in% remove)]

  #split the data into integer or factor
  fac = sapply(df,is.factor)
  factor.list = which(fac)
  number.list = which(!fac)

  #take the date from integer to factor
  factor.list = c(factor.list,number.list[cate])
  number.list = number.list[!number.list %in% number.list[cate]]




  df.length = length(df[,1])
  col.num = length(df[1,])
  weightrate = data.frame(matrix(NA, nrow = df.length, ncol = col.num ))


  for(i in 1:col.num ){
      if(i %in% factor.list){
        avc = sapply(df[,i],as.factor)
        num = summary(avc)##num is the number of each categare in this variable
        lvs = levels(avc)##name of each cate use for which(lvs== string)

        num = as.double(num)/df.length
        temp = rep(1,df.length)

        for (j in 1:df.length ) {
          temp[j] = num[which(lvs == df[j,i])]
        }
        weightrate[,i] = temp


      } else if(i %in% number.list){
        mean = mean(df[,i])
        variance =  var(df[,i])
        temp = rep(1,df.length)
        temp = dnorm(df[,i], mean, variance**0.5)
        weightrate[,i] = temp
      }


  }

  weightlist = rep(1,df.length)

  if(patten == "a"){
    for (i in 1:df.length) {
      weightlist[i]=abs(sum(weightrate[i,]))
    }
  }
  else if(patten == "m"){
    for (i in 1:df.length) {
      weightlist[i]=abs(prod(weightrate[i,]))
    }
  }
  else if(patten == "l"){
    for (i in 1:df.length) {
      weightlist[i]=abs(prod(log(weightrate[i,])))
    }
  }


  weightlist = 1/weightlist#inverse them
  weightlist = weightlist*df.length/sum(weightlist)#normalized them
  weightlist

}
