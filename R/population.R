#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' RDS Population Simulator
#' @description Generates RDS populations according to the user prefered variability. Do note the computer
#' capcities when using the function since it can affect the functionality of the methods used.
#' A population size of 10,000 can be produced with a RAM of 4GB without an issue. But if higher
#' population sizes are needed then higher RAM is needed. The produced population consists of the
#' degree size, a dichotonomous independent variable, a dichotonomous respond variable and the IDs of
#' the respondents in the network of each respond.
#'
#' @usage population (N = 1000, p.ties = 0.33, minVal = 0, maxVal = 300, zeros = 2, dis_type = "rexp",
#' skew = 0.05, pr = .33, pa = .33, atype_char = "NULL", atype_res = "NULL")
#' @param N Population size. Should be a positive integer
#' @param p.ties Number of ties to be in the population. Should be a positive integer
#' @param minVal The minimum degree size an individual in the population can have. Should be a positive
#' integer
#' @param maxVal The maximum degree size an individual in the population can have. Should be a positive
#' integer
#' @param zeros The maximum number of zeros the population can have
#' @param dis_type Specify the distribution of the degrees. If need a uniformly distributed degree
#' distribution dis_type should be "runif". If need a normally distributed degree distribution dis_type
#' should be "rnorm". If need a right skewed distribution dis_type should be "rexp" and the skew parameter
#' should be a positive decimal value. In need a negative skewed distribution then the skew parameter
#' should be a negative decimal value.
#' @param skew needed when the distribution is left or right skewed. When selecting the value for the
#' skewness it is advisable to first observe the range of values given from the rexp function and to see
#' whether the maximum value of the distribution is close to the defined maxVal.
#' @param pr Proportion of individuals that has '1' as the response in the response variable.
#' @param pa Proportion of individuals that has '1' as their character in the indipendent variable.
#' @param atype_char How the independent variable is associated to other external factors. "NULL" when
#' needs to be randomly distributed. If needs to be associated with the network size use "net". Can
#' associate with network size only. Must note these abbreviations when need to associate with these
#' variables. "network size = net"
#'
#' @param atype_res How the response variable is associated to other external factors."NULL" when needs
#' to be randomly distributed. If needs to be associated with both independent variable and network size
#' use "char * net". Can associate with network size and the independent variable. Must note these
#' abbreviations when need to associate with these variables. "network size = net","independent
#' dichotonoous variable = char"
#'
#' @inheritParams e1071::skewness
#'
#' @examples
#' population(N = 5000,p.ties = 0.6,minVal = 10,zeros = 0,pr = .5,pa = .1,atype_char = "net",atype_res = "net*char")
#' population(N = 10000,p.ties = 0.6,minVal = 10,zeros = 10,pr = .5,pa = .3,atype_char = "1/net",atype_res = "(1/net)*char")
#' population(N = 3000,p.ties = 0.6,minVal = 10,zeros = 0,pr = .5,pa = .4,atype_char = "2*net",atype_res = "net")
#' @export

population = function(N = 1000,
                      p.ties = 0.33,
                      minVal = 0,
                      maxVal = 300,
                      zeros = 2,
                      dis_type = "rexp",
                      skew = 0.05,
                      pr = .33,
                      pa = .33,
                      atype_char = "NULL",
                      atype_res = "NULL"){
  #atype_res = "net*char" # can use different combinations of net and char for random use NULL
  #atype_char ="net"
  ###############################################################################
  {
    qq =1; ff = 1
    while(ff!=0){

      #m will be the maximum network size of a respondent.
      #initiated by setting it to infinity so that the following loop gets executed
      m = Inf

      #This loop continues until the maximum network size is equal or less than (N-1) since (N-1) is the
      #maximum network size a respondent can have
      while(m>(N-1)){

        #If loop determines whether the distribution of the network sizes is uniform or not
        if(dis_type == "rnorm"){
          #If the skew = 0 then a random sample of size N is generated from a normal distibution
          #Since the network sizes cannot be fractions elements of the generated sample is rounded
          #to the nearest whole number
          q = round(rnorm(N, mean = mn/2, sd = mn/3))
          while(min(q) < minVal){
            q = q - min(q)
          }
        }else if(dis_type == "rexp"){
          #if it is not uniform the allocated skewness is checked to see what the skewness of
          #the distribution to be
          if(skew < 0){
            q = round(-rexp(N,abs(skew)))
            while(min(q) < minVal){
              q = q - min(q)
            }
          }else{
            q = round(rexp(N,abs(skew)))
          }
        }else{

          q = round(runif(N,min = minVal,max = maxVal))
        }
        m = max(q) #Maximum number of ties an individual can have
      }
      no.ties = round(p.ties*m*N/2) #randomly selecting the number of ties that the network should have
      adj = matrix(0, nrow = N, ncol = N); adj = as.data.frame(adj); row.names(adj) = 1:N; names(adj) = 1:N
      contact = data.frame(respondent = NULL, connection = NULL)
      p = data.frame(s = 1:N, r = q)#Generating the network sizes for a given number of respondents and ties

      while(sum(p$r) != 2*no.ties){
        if(sum(p$r)-2*no.ties<0){
          d = 2*no.ties - sum(p$r)
          e = p[p$r!=m,]
          if(dis_type == 'rnorm'){
            r = sample(e$s, d, replace = T)
          }else if(dis_type == 'rexp'){
            r = sample(e[order(e$r),]$s, d, replace = T)
          }else{
            r = sample(e$s, size = d, replace = T, prob = e$r)
          }
          s = as.data.frame(table(r))
          s = s[order(s$r),]
          p[p$s%in%s$r,]$r = p[p$s%in%s$r,]$r + s$Freq
          p$r[p$r>m] = m
          p$r[p$r<minVal] = minVal
        }else if(sum(p$r) - 2*no.ties>0){
          d = sum(p$r) - 2*no.ties
          e = p[p$r!=minVal,]

          if(dis_type == 'rexp'){
            if(skew != minVal){
              r = sample(e[order(e$r),]$s, d,replace = T)
            }else{
              r = sample(e$s, d, replace = T)
            }
          }else{
            r = sample(e$s, size = d, replace = T, prob = e$r)
          }
          s = as.data.frame(table(r))
          s = s[order(s$r),]
          p[p$s%in%s$r,'r'] = p[p$s%in%s$r,]$r - s$Freq
          p$r[p$r<minVal] = minVal
          p$r[p$r>m] = m

        }
      }

      if(minVal == 0){
        z = sum(p$r == 0)
        while(z > zeros){
          z = sum(p$r == 0)
          if(dis_type == "rexp"){
            d = as.data.frame(table(sample(p[p$r == 0,'s'], z, replace = T)))
            if(skew<e1071::skewness(p$r)){
              rem = as.data.frame(table(sample(p[p$r>mean(p$r) & p$r != 0,"s"], replace = T, size = z)))
            }else{
              rem = as.data.frame(table(sample(p[p$r<mean(p$r) & p$r != 0,"s"], replace = T, size = z)))
            }

          }else{
            ggg = p[p$r == 0,]$s[1:round(length(p[p$r == 0,'s'])/4)]
            if(length(ggg) == 1){
              d = as.data.frame(table(rep(ggg, z)))
            }else{
              d = as.data.frame(table(sample(ggg, size = z, replace = T)))
            }
            unif_ta = as.data.frame(table(p[p$r !=0 ,"r"]))
            rem = as.data.frame(table(sample(unif_ta$Var1, replace = T, size = z, prob = 1/unif_ta$Freq)))
          }
          p[p$s%in%d$Var1,]$r = d$Freq
          p[p$s%in%rem$Var1,]$r = p[p$s%in%rem$Var1,]$r - rem$Freq
          p$r[p$r<0] = 0
          p$r[p$r>m] = m
        }
      }

      hist(p$r)
      sum(p$r==0)

      #############
      frame = data.frame("s" = p$s,"netSize" = p$r, "network" = 0)#Initiating the dataframe with generated network sizes
      ties = data.frame(respondent1 = NULL, respondent2 = NULL)#Initiating a dataframe to get the ties

      frame1 = frame[(frame$netSize != 0),]
      n =sort(unique(frame1$netSize), decreasing = T)
      for(i in 1:length(n)){
        while(TRUE){
          y = frame1[frame1$netSize == n[i],] #Sub frame with network sizes n[i]
          if(nrow(y) == 0)
            break

          x = frame1[frame1$s != y$s[1],] #subframe excluding y$s[j]th row
          if(nrow(x) == 0)
            break

          r = x$netSize


          if(nrow(x)==1){
            u = x$s #if the x has only one row, respondent corresponding to theat row goes ibto the nrtwork of y[j]
          }else{
            f= as.numeric(unlist(strsplit(as.character(y[1,]$network), ","))) #getting the network as a vector to identify the number of vacants
            if(length(x$s)<(y$netSize[1]-length(f)+1)){
              u = NULL
            }else{
              u = sample(x$s, size = y$netSize[1]-length(f)+1, prob = 10*r) #generating respondents for the vacants
            }
          }

          if(!is.null(u)){
            ties = rbind(ties, data.frame(respondent1 = y$s[1], respondent2 = u))
            adj[as.integer(row.names(adj)) == y$s[1], u] = u
            adj[u,as.integer(row.names(adj)) == y$s[1]] = y$s[1]

            contact = rbind(contact, data.frame(respondent = y$s[1], connection = u))
            contact = rbind(contact, data.frame(respondent = u, connection = y$s[1]))
          }

          frame[frame$s == y$s[1],]$network = paste(c(frame[frame$s == y$s[1],]$network,u), collapse = ",")
          frame1 = frame1[frame1$s != y$s[1],]
          frame[frame$s%in%u,]$network = paste(frame[frame$s%in%u,"network"], rep(y$s[1],length(u)), sep=",")
          v = sapply(strsplit(as.character(frame[frame$s%in%u,]$network), ","),length)
          t = frame[frame$s%in%u,]
          y = t[t$netSize + 1 != v,]
          frame1 = rbind(frame1[!frame1$s%in%u,],y)
        }
        if(nrow(frame1) == 0)
          break
      }
      ff = sum((frame$netSize+1) != sapply(strsplit(as.character(frame$network), ","),length))
      qq = qq+1
    }
    qq
  }

  char_prob = frame$netSize #Change
  strchar = atype_char
  {
    ###################### Distributin characteristics #############################
    naa = round(pa*N) #Number of respondents belonging to group A in the population
    nbb = N - naa #Number of respondents belonging to group A in the population

    frame$character = 1 #Character 'b'
    a = sample(frame$s, naa, prob = eval(parse(text=atype_char))) #People with higher network sizes are more likely to get into group A
    frame[frame$s%in%a,]$character = 2 #Character 'a'

    ######################## Allocating response variable #############################
    frame$response = 0
    nr = pr*N
    net = scale(frame$netSize * 2) + 10
    char = scale(frame$character) + 10
  }

  srtres = atype_res
  res_prob = eval(parse(text=atype_res))
  {
    o = sample(frame$s, nr, prob = res_prob) #people with characteristic 'a' and with a higher network size are more likely to get the response
    frame[frame$s%in%o,]$response = 1
    contact = contact[contact$connection != 0,]
    sum(frame$response == 1 & frame$character == 1)
  }

  contact = contact[order(contact$respondent),]

  for(i in 1:nrow(frame)){
    kk = unlist(strsplit(frame$network[i],","))

    frame$network[i] = ifelse(length(kk[kk!=0]) == 0,"No Network", paste0(kk[kk!=0],collapse = ","))
  }

  out = list()
  out[["frame"]] = frame
  out[["Adjusent Matrix"]] = adj
  out[["Contact"]] = contact
  out[["Degree"]] = frame[,names(frame)%in%c("s","netSize")]

  return(out)

}
