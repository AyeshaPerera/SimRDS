#' Extracts sample from external populations
#' @description Extract samples from populations that the user inputs from an external source
#' without using a inbult function to simulate a population. Data should be in .csv format. Inputs are
#' recrited as that is in format of the output given in the inbuilt function 'population'
#'
#' @usage samplingEx (population, degree, net, seeds, coupon, waves = NULL, size = NULL, prob=NULL,
#' prob_var = NULL, prop_sam = NULL, prop_sam_var=NULL, showids = T, timeInterval = NULL)
#'
#' @param population .csv file containing the population details. First column should contain the id of the individual.
#' @param degree .csv file containing the network sizes. If it's not provided the degree size would the number of individuals
#' in the networks provided. If both the networks and the degrees are not provided, then it will randomly produce degrees for each resondent
#' @param net .csv file containging the network of each individual. Should contain the same format
#' of the output of the 'netsize table' given from the output of the inbuilt function 'population'. If not provided would
#' randomly assign individuals considering the degree.
#' @param seeds Number of seeds. Should be a positive integer
#' @param coupon Number of coupons. Should be a positive
#' integer
#' @param degree_column Column including degrees
#' @param waves Number of waves. Should be a positive integer
#' @param size Sample size. Should be a positive integer
#' @param prob Whether the seeds are selected probabilistically. If want to be selected randomly
#' then leave ot as 'NULL' If want to relate it to a variable add the variable name and the it
#' should be related. For example if the seeds selected should depend on the network size then 'network'
#' should be used. If it is inversly proportional then should enter as '1/network'.
#' @param prop_sam If the individuals from the network of an individual should be chosen randomly
#' use "NULL". If the selection should be done probabilistically state the variable and how it should
#' be related.
#' @param showids If TRUE displas the IDs beign recruited.
#' @param timeInterval Days between recruitment waves. zeroth wave is assumed to recruit at the present
#' day
#'
#' @inheritParams stats::rexp
#' @inheritParams RDS::as.rds.data.frame
#' @inheritParams RDS::get.seed.id
#'
#' @examples
#' samplingEx(population, degree, net, seeds = 10, coupon = 2, waves =8)
#' @export

samplingEx = function(population,
                      degree = NULL,
                      net = NULL,
                      seeds=10,
                      coupon=2,
                      degree_column = NULL,
                      response_column="HIV",
                      categorical_column = NULL,
                      waves = NULL,
                      size = 300,
                      prob=NULL,
                      prop_sam = NULL,
                      id_column = NULL,
                      showids =T,
                      timeInterval = NULL){

  if(is.null(waves)&&is.null(size)){
    stop("Must provide number of waves or sample size")
  }

  if(is.null(id_column)){
    population$id_column = 1:nrow(population)
    id_column = "id_column"
  }

  population = population[order(population$id_column),]

  if(is.null(degree)){
    if(is.null(degree_column)){
      if(!is.null(net)){
        uniq = unique(net[,1])
        numU = numeric()
        for(ii in 1:length(uniq)){
          numU = c(numU,sum(net[,1]%in%uniq[ii]))
        }
        uniq = c(uniq, population$id_column[which(!(population$id_column%in%uniq))])
        degree = data.frame(id = uniq, total = c(numU,rep(0,(length(uniq)-length(numU)))))
        degree = degree[order(degree$id),]
      }else{
        q = round(stats::rexp(nrow(population),0.05))
        q[q>=nrow(population)] = nrow(population)-1
        q[q==0] = sample(1:max(q), length(q[q==0]), replace = T, prob = max(q):1)
        if(is.null(id_column)){
          degree = data.frame(id = 1:nrow(population), total = q)
        }else{
          degree = data.frame(id = population[,id_column], total = q)
        }
      }
    }else{
      q1 = which(names(population)%in%degree_column)
      q1 = c(q1,which(names(population)%in%id_column))
      degree = population[,q1]
    }
  }else{
    degree = degree
  }
  names(degree) = c("id", "total")
  if(is.null(degree_column)){
    population[population[,names(population)%in%id_column]%in%degree$id,"degree_column"] = degree$total
    degree_column = "degree_column"
  }else{
    population[,"degree_column"] = population[,names(population)%in%degree_column]
  }

  N = nrow(population)
  adj = matrix(0, nrow = N, ncol = N); adj = as.data.frame(adj); row.names(adj) = 1:N; names(adj) = 1:N
  contact = data.frame(respondent = NULL, connection = NULL)

  if(is.null(net)){
    frame = data.frame("s" = degree$id,"netSize" = degree$total, "network" = 0)#Initiating the dataframe with generated network sizes
    ties = data.frame(respondent1 = NULL, respondent2 = NULL)#Initiating a dataframe to get the ties
    adj = matrix(0, nrow = N, ncol = N); adj = as.data.frame(adj); row.names(adj) = 1:N; names(adj) = 1:N
    contact = data.frame(respondent = NULL, connection = NULL)

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
    data = contact
  }else{
    data = net
  }
  row.names(data) = 1:nrow(data)

  pop = population
  N = nrow(pop)
  l=sample(degree$id,seeds,replace=F,prob=degree$total)
  if(is.null(categorical_column)){
    pop$categorical_column = 1
    categorical_column = "categorical_column"
  }
  t = data.frame(id = l, recruiter.id = "seed", network.size = pop[pop[,id_column]%in%l, degree_column], wave = 0, response = pop[pop[,id_column]%in%l, response_column], character = pop[pop[,id_column]%in%l, categorical_column])
  if(!is.null(timeInterval)){ et = rep(Sys.time(), length(l))}

  l0 = nrow(t)
  w = 1
  size1 = ifelse(is.null(size), l0, size)
  w1 = ifelse(is.null(waves), w, waves + 1)
  #t = data.frame(seed = rep(0,N))
  #t[row.names(t)%in%l,] = sort(l)

  lli = l
  while (w1 != w || l0 != size1) {
    qlq = numeric()
    for(i in 1:length(lli)){
      bond1 = data[data$respondent%in%lli[i],"connection"]
      bond = bond1[!bond1%in%(t$id)]

      if(length(bond)==0){
        next
      }else{
        c = ifelse(length(bond)<=coupon, length(bond),coupon)
        c1 = sample(x = 0:c, size = 1, prob = 1:(c+1))

        if(!is.null(prop_sam)){
          prop_sam = pop[pop$s%in%bond,"character"]
        }

        if(c1 == 0){
          next
        }else{
          while(T){
            a1 = sample(x = bond, replace = F, size = c1, prob = prop_sam)
            if(sum(t$id%in%a1)==0){
              break
            }
          }

          t = rbind(t, data.frame(id = a1, recruiter.id = as.character(lli[i]), network.size = pop[pop[,id_column]%in%a1, degree_column], wave = w, response = pop[pop[,id_column]%in%a1, response_column], character = pop[pop[,id_column]%in%a1, categorical_column]))
          qlq = c(qlq, a1)
        }
      }
      if(showids){ print(paste0(lli[i]))}
    }
    lli = qlq
    if(!is.null(timeInterval)){ et = c(et, rep(Sys.time()+60*60*24*timeInterval*w, length(qlq)))}
    w = w +1
    l0 = nrow(t)

    if(is.null(size)){
      size1 = l0
    }else{
      if(l0>size){t = t[1:size,]; l0 = nrow(t) }
      size1 = size
    }
    w1 = ifelse(is.null(waves), w, waves + 1)
    #if(!is.null(timeInterval)){ et = rep(Sys.time(), length(l))}
    if(length(qlq)==0){
      print(paste("Haven't attained the required waves or sample size, waves=",w," size=",l0))
      break()
    }

  }

  if(length(qlq)!=0){
    print(paste("waves=", w, "size=",nrow(t)))
  }
  if(!is.null(timeInterval)){t$time = et}
  if(!is.null(timeInterval)){
    samp11=RDS::as.rds.data.frame(t, id = "id", recruiter.id = "recruiter.id", network.size="network.size",population.size=N, time = "time")
  }else{
    samp11=RDS::as.rds.data.frame(t, id = "id", recruiter.id = "recruiter.id", network.size="network.size",population.size=N)
  }
  samp11$seed=RDS::get.seed.id(samp11)

  return(samp11)
}
#
# population = read.csv("C:\\Users\\Ayesha\\Downloads\\population_New.csv")
# net = read.csv("C:\\Users\\Ayesha\\Downloads\\data1.csv")
# degree = read.csv("C:\\Users\\Ayesha\\Downloads\\degree.csv")
