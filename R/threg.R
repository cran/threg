### The following is the definition of the threg function

"threg" <-
function (formula,data,hr,timevalue,scenario,graph,nolegend=0,nocolor=0) 
{
	### Read in all arguments
	cl <- match.call()


	indx <- match(c("formula", "data"),
		  names(cl), nomatch=0) 
	if (indx[1] ==0) stop("A formula argument is required")

	mf<- cl[c(1, indx)]
	f <- Formula(formula)
	f1<-formula(f,lhs=1)
	f1<-Formula(f1)
	mf[[1]] <- as.name("model.frame")

	mf$formula <- if(missing(data)) terms(f1)
		    else              terms(f1, data=data)  
	mf$formula <- f1
	mf <- eval(mf, parent.frame())


	if (nrow(mf) ==0) stop("No (non-missing) observations")
	Terms <- terms(mf)
	Y <- model.extract(mf, "response")
	if (!inherits(Y, "Surv")) stop("Response must be a survival object")
	type <- attr(Y, "type")
	if (type!='right')
	stop(paste("threg package current doesn't support \"", type,
			  "\" survival data", sep=''))

	f2<-formula(f1, lhs = 0)
	if (length(f2[[2]])!=3)
	stop(paste("Predictors for both lny0 and mu should be specified"))

	mf <- match.call(expand.dots = FALSE)
	f_lny0 <-formula(f1,lhs=0,rhs=1)
	f_mu <-formula(f1,lhs=0,rhs=2)
	m_time<-match(c("time"), names(mf), 0)

	m_failure<-match(c("failure"), names(mf), 0)
	m_hr<-match(c("hr"), names(mf), 0)
	m_timevalue<-match(c("timevalue"), names(mf), 0)

	m_scenario<-match(c("scenario"), names(mf), 0)
	m_graph<-match(c("graph"), names(mf), 0)

	#m_nolegend<-match(c("nolegend"), names(mf),0)


	str_hr_scenario<-NULL

	if(m_scenario!=0) {

	L_Scenario<-list()
						
	

	cur_scenario_string<-mf[[m_scenario]]
	  

	
		while(length(cur_scenario_string)>=2) {
	  
			if(length(cur_scenario_string)==3) {
			
		
				current_scenario_covariate_string<-cur_scenario_string[[3]]
				
								      			
		                if(length(current_scenario_covariate_string)==2) { 
		    
		                  current_scenario_covariate<-current_scenario_covariate_string[[1]]
		                  current_covariate_value<-current_scenario_covariate_string[[2]]
		      
				  cmd<-paste("L_Scenario$",current_scenario_covariate,"<-'",as.character(current_covariate_value), "'",sep = "")
			          eval(parse(text=cmd))		    
		      
		                  str_hr_scenario<-paste(str_hr_scenario,current_scenario_covariate," = ",as.character(current_covariate_value)," ",sep="")

		    
		                }  				
		                else {
                                  stop("wrong scenario specification")  
				
		                }
 

				cur_scenario_string<-cur_scenario_string[[2]]


			}
			else if(length(cur_scenario_string)==2) {
				current_scenario_covariate_string<-cur_scenario_string
		                current_scenario_covariate<-current_scenario_covariate_string[[1]]
		                current_covariate_value<-current_scenario_covariate_string[[2]]

				cmd<-paste("L_Scenario$",current_scenario_covariate,"<-'",as.character(current_covariate_value), "'",sep = "")
				eval(parse(text=cmd))		  
					
				str_hr_scenario<-paste(str_hr_scenario,current_scenario_covariate,"=",as.character(current_covariate_value),sep="")

				cur_scenario_string<-NULL
				current_scenario_covariate_string<-NULL
				current_scenario_covariate<-NULL
				current_covariate_value<-NULL
			
			}


		}
	
	}

	
	if(m_hr!=0) {
	
		hr_covariate<-mf[[m_hr]]
	  
		cmd<-paste("is.factor(data$",as.character(hr_covariate),")", sep = "")
		if(!eval(parse(text=cmd))) { 
		  stop("The variable for the hazard ratio calculation should be a factor variable")  
		}
		else {
			L_lny0_scvector<-list()
			L_mu_scvector<-list()		
			
			cmd<-paste("data$",hr_covariate, sep = "")

      
			hr_covariate_levels <- sort(unique(eval(parse(text=cmd))))
			

			for (i in 1:length(hr_covariate_levels)) { 
			  
			  
			  cmd<-paste("lny0_scvector_level_",hr_covariate_levels[i],"<-c()", sep = "")
			  eval(parse(text=cmd))			  
			  
			  
			  
			  cmd<-paste("mu_scvector_level_",hr_covariate_levels[i],"<-c()", sep = "")
			  eval(parse(text=cmd))					  
			}		
		}	
						
		if(m_timevalue!=0) {
			timevalue<-mf[[m_timevalue]]

			if(!is.numeric(timevalue)) {

				stop(paste("'timevalue' option must specify a numerical value!"))	     		
						  																	
			}		
		}   
		else {
		  stop("'timevalue' option is required to specify a time value at which hazard ratios are calculated!")  
		
		}
	        
                if (!is.null(str_hr_scenario)) {
	
		  str_hr_scenario<-paste(str_hr_scenario," at time = ",timevalue,sep="")
		}
                else {
                   str_hr_scenario<-paste("at time = ",timevalue,sep="")
		}
		
	}

	L_Dummies<-list()

	cmd_col_dim<-"col.dim<-c("

	i<-0
	cmd_lny0<-""
	if(f_lny0[[2]]!=0 & f_lny0[[2]]!=1) {

		i<-1

		cur_lny0_string<-f_lny0[[2]]

		while(length(cur_lny0_string)>=1) {


			if(length(cur_lny0_string)==3) {

		
				cur_covariate<-cur_lny0_string[[3]]
		        

				cur_lny0_string<-cur_lny0_string[[2]]


			}
			else if(length(cur_lny0_string)==1) {
				
				
				cur_covariate<-cur_lny0_string

				cur_lny0_string<-NULL
			
			}

			cmd<-paste("is.factor(data$",as.character(cur_covariate),")", sep = "")
			if(!eval(parse(text=cmd))) { 
   
        
			if(m_hr!=0) {
				
				cmd<-paste("L_Scenario$",cur_covariate, sep = "")				
				if (!is.null(eval(parse(text=cmd)))) {				
             
					cur_scenario_value<-as.numeric(eval(parse(text=cmd)))
					if(is.numeric(cur_scenario_value)) {

				     
						for (s in 1:length(hr_covariate_levels)) { 
									  
									  
						  
							  eval(parse(text=paste("cur_lny0_scvector<-c(lny0_scvector_level_",hr_covariate_levels[s],")",sep = "")))									  							  
							  cmd<-paste("lny0_scvector_level_",hr_covariate_levels[s],"<-c(cur_lny0_scvector,",cur_scenario_value,")",sep = "")
							  eval(parse(text=cmd))									  
							  
						}	  
									
					
								  																	
					}
					else  {		  
						stop(paste("scenario value for", cur_covariate, "must be a numerical value!"))
					}
				}  
				else  {
					stop(paste("scenario value for", cur_covariate, "is required!"))
				}    
					  
			}
     
        
			temp<-paste("par[",i,"]*data$",as.character(cur_covariate), sep = "")

			if (i==1) {
			  cmd_lny0<-paste(cmd_lny0,temp, sep = "")	

			  cmd_col_dim<-paste(cmd_col_dim,"'lny0: ",cur_covariate,"'", sep = "")
			}
			else  {
			  cmd_lny0<-paste(cmd_lny0,"+",temp, sep = "")

			  cmd_col_dim<-paste(cmd_col_dim,",'lny0: ",cur_covariate,"'", sep = "")  
			}



			i<-i+1

			}
			else {
              

				if(m_hr!=0) {
				 
				   
				   if (cur_covariate==hr_covariate) {
         
                   	             
						 for (s in 1:length(hr_covariate_levels)) { 							  
							  
							  temp_lny0_dummy_scvector<-rep(0,length(hr_covariate_levels))   
							  temp_lny0_dummy_scvector[s]<-1
							  
							  cur_lny0_dummy_scvector<-temp_lny0_dummy_scvector[2:length(hr_covariate_levels)]	
							  cur_lny0_dummy_scvector<-cur_lny0_dummy_scvector[length(cur_lny0_dummy_scvector):1]	
							  
				  
							  eval(parse(text=paste("cur_lny0_scvector<-c(lny0_scvector_level_",hr_covariate_levels[s],")",sep = "")))									  							  
							  cmd<-paste("lny0_scvector_level_",hr_covariate_levels[s],"<-c(cur_lny0_scvector,cur_lny0_dummy_scvector)",sep = "")
							  eval(parse(text=cmd))									  							  
						 }	  	             			     				     
				   }
				   else {
			     
						cmd<-paste("L_Scenario$",cur_covariate, sep = "")				
						if (!is.null(eval(parse(text=cmd)))) {									 


							cmd_temp<-paste("data$",cur_covariate, sep = "")
							idx <- sort(unique(eval(parse(text=cmd_temp))))	             


							if(any(idx==eval(parse(text=cmd)))){

								cur_lny0_dummy_scvector<-as.integer(idx==eval(parse(text=cmd)))[2:length(idx)]			
								cur_lny0_dummy_scvector<-cur_lny0_dummy_scvector[length(cur_lny0_dummy_scvector):1]	  							  

							}
							else{
								stop(paste("scenario value", eval(parse(text=cmd)), "for",cur_covariate,"does not exist!"))
							}

							for (s in 1:length(hr_covariate_levels)) { 							  

							  eval(parse(text=paste("cur_lny0_scvector<-c(lny0_scvector_level_",hr_covariate_levels[s],")",sep = "")))									  							  
							  cmd<-paste("lny0_scvector_level_",hr_covariate_levels[s],"<-c(cur_lny0_scvector,cur_lny0_dummy_scvector)",sep = "")
							  eval(parse(text=cmd))									  
							  
							}	  	             

						}  
						else  {
							stop(paste("scenario value for", cur_covariate, "is required!"))
						}    				     
			     
				   }
				  
				}
				

				cmd<-paste("L_Dummies$",cur_covariate, sep = "")				
				if (is.null(eval(parse(text=cmd)))) {


					cmd<-paste("data$",cur_covariate, sep = "")


					idx <- sort(unique(eval(parse(text=cmd))))


					cur_dum_num<-length(idx)  
					cur_dummies <- matrix(NA, nrow = nrow(data), ncol = cur_dum_num)


					for (d in 1:length(idx)) { 
					  cur_dummies[,d] <- as.integer(eval(parse(text=cmd)) == idx[d])
					}


					
					cmd<-paste("L_Dummies$",cur_covariate,"<-cur_dummies", sep = "")
					eval(parse(text=cmd))

					k<-cur_dum_num
					while(k>=2) {

						temp<-paste("par[",i,"]*L_Dummies$",as.character(cur_covariate),"[,",k,"]", sep = "")

						if (i==1) {
							cmd_lny0<-paste(cmd_lny0,temp, sep = "")
									 
							cmd_col_dim<-paste(cmd_col_dim,"'lny0: ",cur_covariate,"(",idx[k],")'", sep = "")
									      
						}
						else  {
							cmd_lny0<-paste(cmd_lny0,"+",temp, sep = "")

							cmd_col_dim<-paste(cmd_col_dim,",'lny0: ",cur_covariate,"(",idx[k],")'", sep = "")  
						}

				
						k<-k-1
						i<-i+1
					}


				}
				else{



						cmd<-paste("data$",cur_covariate, sep = "")


						idx <- sort(unique(eval(parse(text=cmd))))


						cur_dum_num<-length(idx)  

						k<-cur_dum_num

						while(k>=2) {

							temp<-paste("par[",i,"]*L_Dummies$",as.character(cur_covariate),"[,",k,"]", sep = "")

							if (i==1) {
							  cmd_lny0<-paste(cmd_lny0,temp, sep = "")
					         
							  cmd_col_dim<-paste(cmd_col_dim,"'lny0: ",cur_covariate,"(",idx[k],")'", sep = "")
							}
							else  {
							  cmd_lny0<-paste(cmd_lny0,"+",temp, sep = "")

							  cmd_col_dim<-paste(cmd_col_dim,",'lny0: ",cur_covariate,"(",idx[k],")'", sep = "")  
							}

					
							k<-k-1
							i<-i+1
						}
				}

			}
		  
		}

		if(m_hr!=0) {

		     
				for (s in 1:length(hr_covariate_levels)) { 
	  
				  eval(parse(text=paste("cur_lny0_scvector<-c(lny0_scvector_level_",hr_covariate_levels[s],")",sep = "")))		
				  cmd<-paste("lny0_scvector_level_",hr_covariate_levels[s],"<-c(cur_lny0_scvector,",1,")",sep = "")
				  eval(parse(text=cmd))									  
				  
				}	  

			  
		}


 
		cmd_lny0<-paste(cmd_lny0,"+par[",i,"]", sep = "")
		cmd_col_dim<-paste(cmd_col_dim,",'lny0: (Intercept)'", sep = "")

	} 
	else {
	
	
		if(m_hr!=0) {

		     
				for (s in 1:length(hr_covariate_levels)) { 
	  
				  eval(parse(text=paste("cur_lny0_scvector<-c(lny0_scvector_level_",hr_covariate_levels[s],")",sep = "")))		
				  cmd<-paste("lny0_scvector_level_",hr_covariate_levels[s],"<-c(cur_lny0_scvector,",1,")",sep = "")
				  eval(parse(text=cmd))									  
				  
				}	  

			  
		}
	
	
		i<-i+1 
		cmd_lny0<-paste(cmd_lny0,"par[",i,"]", sep = "")
		cmd_col_dim<-paste(cmd_col_dim,"'lny0: (Intercept)'", sep = "")
	}

	len_lny0<-i

	i<-i+1 

	j<-0

	cmd_mu<-""
	if(f_mu[[2]]!=0 & f_mu[[2]]!=1) {

		j<-1

                cur_mu_string<-f_mu[[2]]

		while(length(cur_mu_string)>=1) {


			if(length(cur_mu_string)==3) {

		
				cur_covariate<-cur_mu_string[[3]]
		        

				cur_mu_string<-cur_mu_string[[2]]


			}
			else if(length(cur_mu_string)==1) {
				
				
				cur_covariate<-cur_mu_string

				cur_mu_string<-NULL
			
			}


				 
           
			cmd<-paste("is.factor(data$",as.character(cur_covariate),")", sep = "")
			if(!eval(parse(text=cmd))) { 
        
				if(m_hr!=0) {
				
					cmd<-paste("L_Scenario$",cur_covariate, sep = "")				
					if (!is.null(eval(parse(text=cmd)))) {				

						cur_scenario_value<-as.numeric(eval(parse(text=cmd)))
						if(is.numeric(cur_scenario_value)) {

							for (s in 1:length(hr_covariate_levels)) { 

							  eval(parse(text=paste("cur_mu_scvector<-c(mu_scvector_level_",hr_covariate_levels[s],")",sep = "")))									  							  
							  cmd<-paste("mu_scvector_level_",hr_covariate_levels[s],"<-c(cur_mu_scvector,",cur_scenario_value,")",sep = "")
							  eval(parse(text=cmd))									  
							  
							}	  
						
						 }
						 else  {		  
							stop(paste("scenario value for", cur_covariate, "must be a numerical value!"))
						 }

					}  
					else  {
						 stop(paste("scenario value for", cur_covariate, "is required!"))
					}    
					  
				}
        
        
				temp<-paste("par[",i,"]*data$",as.character(cur_covariate), sep = "")

				if (j==1) {
				  cmd_mu<-paste(cmd_mu,temp, sep = "")	

				  cmd_col_dim<-paste(cmd_col_dim,",'  mu: ",cur_covariate,"'", sep = "")
				}
				else  {
				  cmd_mu<-paste(cmd_mu,"+",temp, sep = "")

				  cmd_col_dim<-paste(cmd_col_dim,",'  mu: ",cur_covariate,"'", sep = "")  
				}

				j<-j+1
				i<-i+1 

			}
			else {

				if(m_hr!=0) {
				 
				   
				   if (cur_covariate==hr_covariate) {
         
                   	             
						 for (s in 1:length(hr_covariate_levels)) { 							  
							  
							  temp_mu_dummy_scvector<-rep(0,length(hr_covariate_levels))   
							  temp_mu_dummy_scvector[s]<-1
							  
							  cur_mu_dummy_scvector<-temp_mu_dummy_scvector[2:length(hr_covariate_levels)]	
							  cur_mu_dummy_scvector<-cur_mu_dummy_scvector[length(cur_mu_dummy_scvector):1]	
							  
				  
							  eval(parse(text=paste("cur_mu_scvector<-c(mu_scvector_level_",hr_covariate_levels[s],")",sep = "")))									  							  
							  cmd<-paste("mu_scvector_level_",hr_covariate_levels[s],"<-c(cur_mu_scvector,cur_mu_dummy_scvector)",sep = "")
							  eval(parse(text=cmd))									  							  
						 }	  	             			     				     
				   }
				   else {
			     
						 cmd<-paste("L_Scenario$",cur_covariate, sep = "")				
						 if (!is.null(eval(parse(text=cmd)))) {									 
	             
	             
							cmd_temp<-paste("data$",cur_covariate, sep = "")
							idx <- sort(unique(eval(parse(text=cmd_temp))))	             
	             

							if(any(idx==eval(parse(text=cmd)))){

								cur_mu_dummy_scvector<-as.integer(idx==eval(parse(text=cmd)))[2:length(idx)]			
								cur_mu_dummy_scvector<-cur_mu_dummy_scvector[length(cur_mu_dummy_scvector):1]	  							  

							}
							else{
								stop(paste("scenario value", eval(parse(text=cmd)), "for",cur_covariate,"does not exist!"))
							}
	
							for (s in 1:length(hr_covariate_levels)) { 							  

							  eval(parse(text=paste("cur_mu_scvector<-c(mu_scvector_level_",hr_covariate_levels[s],")",sep = "")))									  							  
							  cmd<-paste("mu_scvector_level_",hr_covariate_levels[s],"<-c(cur_mu_scvector,cur_mu_dummy_scvector)",sep = "")
							  eval(parse(text=cmd))									  
							  
							}	  	             

						 }  
						 else  {
						    stop(paste("scenario value for", cur_covariate, "is required!"))
						 }    				     
			     
				   }
				  
				}

				cmd<-paste("L_Dummies$",cur_covariate, sep = "")				
				if (is.null(eval(parse(text=cmd)))) {


					cmd<-paste("data$",cur_covariate, sep = "")


					idx <- sort(unique(eval(parse(text=cmd))))


                                        cur_dum_num<-length(idx)  
					cur_dummies <- matrix(NA, nrow = nrow(data), ncol = cur_dum_num)


					for (d in 1:length(idx)) { 
					  cur_dummies[,d] <- as.integer(eval(parse(text=cmd)) == idx[d])
					}


					
					cmd<-paste("L_Dummies$",cur_covariate,"<-cur_dummies", sep = "")
					eval(parse(text=cmd))

					l<-cur_dum_num
					while(l>=2) {

						temp<-paste("par[",i,"]*L_Dummies$",as.character(cur_covariate),"[,",l,"]", sep = "")

						if (j==1) {
							cmd_mu<-paste(cmd_mu,temp, sep = "")
						 
							cmd_col_dim<-paste(cmd_col_dim,",'  mu: ",cur_covariate,"(",idx[l],")'", sep = "")
						}
						else  {
							cmd_mu<-paste(cmd_mu,"+",temp, sep = "")

							cmd_col_dim<-paste(cmd_col_dim,",'  mu: ",cur_covariate,"(",idx[l],")'", sep = "")  
						}

						l<-l-1
						j<-j+1
						i<-i+1
					}


				}

				else{


						cmd<-paste("data$",cur_covariate, sep = "")


						idx <- sort(unique(eval(parse(text=cmd))))


						cur_dum_num<-length(idx)  



						l<-cur_dum_num

						while(l>=2) {

							temp<-paste("par[",i,"]*L_Dummies$",as.character(cur_covariate),"[,",l,"]", sep = "")

							if (j==1) {
							  cmd_mu<-paste(cmd_mu,temp, sep = "")
					         
							  cmd_col_dim<-paste(cmd_col_dim,",'  mu: ",cur_covariate,"(",idx[l],")'", sep = "")
							}
							else  {
							  cmd_mu<-paste(cmd_mu,"+",temp, sep = "")

							  cmd_col_dim<-paste(cmd_col_dim,",'  mu: ",cur_covariate,"(",idx[l],")'", sep = "")  
							}

							l<-l-1
							j<-j+1
							i<-i+1
						}

				}



			}

		  
		}



		if(m_hr!=0) {

		     
				for (s in 1:length(hr_covariate_levels)) { 
	  
				  eval(parse(text=paste("cur_mu_scvector<-c(mu_scvector_level_",hr_covariate_levels[s],")",sep = "")))		
				  cmd<-paste("mu_scvector_level_",hr_covariate_levels[s],"<-c(cur_mu_scvector,",1,")",sep = "")
				  eval(parse(text=cmd))									  
				  
				}	  

			  
		}
 
		cmd_mu<-paste(cmd_mu,"+par[",i,"]", sep = "")
		cmd_col_dim<-paste(cmd_col_dim,",'  mu: (Intercept)'", sep = "")

	}
	else {
	
		if(m_hr!=0) {

		     
				for (s in 1:length(hr_covariate_levels)) { 
	  
				  eval(parse(text=paste("cur_mu_scvector<-c(mu_scvector_level_",hr_covariate_levels[s],")",sep = "")))		
				  cmd<-paste("mu_scvector_level_",hr_covariate_levels[s],"<-c(cur_mu_scvector,",1,")",sep = "")
				  eval(parse(text=cmd))									  
				  
				}	  

			  
		}
	
		j<-j+1 
		i<-i+1
		cmd_mu<-paste(cmd_mu,"par[",i,"]", sep = "")
		cmd_col_dim<-paste(cmd_col_dim,",'  mu: (Intercept)'", sep = "")
	}


	len_mu<-j
	
	len=i
	
	i<-i+1

	cmd_col_dim<-paste(cmd_col_dim,")", sep = "")

	eval(parse(text=cmd_col_dim))


	p<-rep(0,i-1)
	time<-Y[,1]


	failure<-Y[,2]


	cmd_d<-paste("(-(",cmd_mu,")/exp(",cmd_lny0,"))")
	cmd_v<-paste("(exp(-2*(",cmd_lny0,")))")


	logf<-NULL
	cmd_logf<-paste("logf<-function(par) {-sum(failure*(-.5*(log(2*pi*",cmd_v,"*(time^3))+(",cmd_d,"*time-1)^2/(",cmd_v,"*time))))- sum((1-failure)*log(pnorm((1-",cmd_d,"*time)/sqrt(",cmd_v,"*time))-exp(2*",cmd_d,"/",cmd_v,")*pnorm(-(1+",cmd_d,"*time)/sqrt(",cmd_v,"*time))))}")


	eval(parse(text=cmd_logf))
		
	est<-nlm(logf, p, hessian = TRUE)

	stderr=sqrt(diag(solve(est$hessian)))

	est
	stderr
	cbind(est$p, stderr)

	q<-rep(1, len)
	est.data<-round(matrix(est$estimate),7)
	stderr.data<-round(matrix(stderr),7)
	z.data<-round(est.data/stderr.data,3)
	p.data<-round(2*pmin(pnorm(z.data),q-pnorm(z.data)),4)

	table<-cbind(est.data,stderr.data,z.data,p.data)

	dimnames(table)<-list(col.dim,c("Coef. Est.", "Std. Err.", "z", "P>|z|"))
	L_Results<-list()
	L_Results$coef.est<-table[c(len_lny0:1,len:(len_lny0+1)),]
	L_Results$log.likelihood<-(-1)*est$minimum
	L_Results$AIC<-(-2)*L_Results$log.likelihood+2*len
	  
	if(m_hr!=0) {

		hazards<-NULL

		max_f_curve<-0
		min_f_curve<-0
		max_f_curve<-0
		min_f_curve<-0
		max_f_curve<-0
		min_f_curve<-0

		for (s in 1:length(hr_covariate_levels)) { 
  
              
			eval(parse(text=paste("lny0_",hr_covariate_levels[s],"<-lny0_scvector_level_",hr_covariate_levels[s],"%*%est.data[1:len_lny0]",sep = "")))	
			eval(parse(text=paste("mu_",hr_covariate_levels[s],"<-mu_scvector_level_",hr_covariate_levels[s],"%*%est.data[(len_lny0+1):(len_lny0+len_mu)]",sep = "")))
			
			eval(parse(text=paste("y0_",hr_covariate_levels[s],"<-exp(lny0_",hr_covariate_levels[s],")",sep = "")))	
			
			cmd<-paste("f_",hr_covariate_levels[s],"<-exp((log(y0_",hr_covariate_levels[s],")-.5*(log(2*pi*(timevalue^3))+(y0_",hr_covariate_levels[s],"+mu_",hr_covariate_levels[s],"*timevalue)^2/timevalue)))",sep = "")
			eval(parse(text=cmd))
			
			cmd<-paste("f_curve_",hr_covariate_levels[s],"<-exp((log(y0_",hr_covariate_levels[s],")-.5*(log(2*pi*(time^3))+(y0_",hr_covariate_levels[s],"+mu_",hr_covariate_levels[s],"*time)^2/time)))",sep = "")
			eval(parse(text=cmd))    
			cmd<-paste("max_f_curve<-max(f_curve_",hr_covariate_levels[s],",max_f_curve)",sep = "")    
			eval(parse(text=cmd)) 
			cmd<-paste("min_f_curve<-min(f_curve_",hr_covariate_levels[s],",min_f_curve)",sep = "")    
			eval(parse(text=cmd))                
			
			cmd<-paste("S_",hr_covariate_levels[s],"=exp(log(pnorm((mu_",hr_covariate_levels[s],"*timevalue+y0_",hr_covariate_levels[s],")/sqrt(timevalue))-exp(-2*y0_",hr_covariate_levels[s],"*mu_",hr_covariate_levels[s],")*pnorm((mu_",hr_covariate_levels[s],"*timevalue-y0_",hr_covariate_levels[s],")/sqrt(timevalue))))",sep = "")    
			eval(parse(text=cmd))    
			
			
			cmd<-paste("S_curve_",hr_covariate_levels[s],"=exp(log(pnorm((mu_",hr_covariate_levels[s],"*time+y0_",hr_covariate_levels[s],")/sqrt(time))-exp(-2*y0_",hr_covariate_levels[s],"*mu_",hr_covariate_levels[s],")*pnorm((mu_",hr_covariate_levels[s],"*time-y0_",hr_covariate_levels[s],")/sqrt(time))))",sep = "")    
			eval(parse(text=cmd))          
			
			cmd<-paste("h_",hr_covariate_levels[s],"=f_",hr_covariate_levels[s],"/S_",hr_covariate_levels[s],sep = "")  
			eval(parse(text=cmd))  
		       
			hazards<-c(hazards,eval(parse(text=paste("h_",hr_covariate_levels[s],sep = ""))))
			hazard_ratios<-hazards[2:length(hr_covariate_levels)]/hazards[1]
										  
			cmd<-paste("h_curve_",hr_covariate_levels[s],"=f_curve_",hr_covariate_levels[s],"/S_curve_",hr_covariate_levels[s],sep = "")  
			eval(parse(text=cmd))         
        			  
		}	  
		
		col.dim<-NULL
		for (s in 2:length(hr_covariate_levels)) { 

			col.dim<-c(col.dim,paste(hr_covariate,"(",hr_covariate_levels[s],")",sep=""))
		  
		}	 			
		table_hr<-cbind(hazard_ratios)
		dimnames(table_hr)<-list(col.dim,"Haz. Ratio")
		  
	}

	if(m_hr!=0) {  
	        L_Results$scenario<-str_hr_scenario  
                L_Results$hazard.ratio<-table_hr
      
        }
    
        if(m_graph!=0) {

		graph_type<-mf[[m_graph]]

		dt_graph<-time


	if(graph_type=="ds"){    
		for (s in 1:length(hr_covariate_levels)) { 

		 cmd<-paste("dt_graph<-cbind(dt_graph,f_curve_",hr_covariate_levels[s],")",sep = "")  

		 eval(parse(text=cmd))     
		}
		y_label="Estimated f(t)"
		legend_pos="topright"
	}
	else if(graph_type=="sv"){    
		for (s in 1:length(hr_covariate_levels)) { 

		 cmd<-paste("dt_graph<-cbind(dt_graph,S_curve_",hr_covariate_levels[s],")",sep = "")  

		 eval(parse(text=cmd))     
		}
		y_label="Estimated S(t)"

		legend_pos="topright"
	}
	else if(graph_type=="hz"){    
		for (s in 1:length(hr_covariate_levels)) { 

		 cmd<-paste("dt_graph<-cbind(dt_graph,h_curve_",hr_covariate_levels[s],")",sep = "")  

		 eval(parse(text=cmd))     
		}
		y_label="Estimated h(t)"
		legend_pos="topright"
		#legend_pos="topleft"
	}
	if(nocolor==0) {
		#matplot(dt_graph[order(time),1],dt_graph[order(time),2:(length(hr_covariate_levels)+1)],type = "l",col=1:(length(hr_covariate_levels)),lty=1:(length(hr_covariate_levels)),xlab="time",ylab=y_label)
		matplot(dt_graph[order(time),1],dt_graph[order(time),2:(length(hr_covariate_levels)+1)],type = "l",lty=1:(length(hr_covariate_levels)),xlab="time",ylab=y_label) 
	}
	else {
		matplot(dt_graph[order(time),1],dt_graph[order(time),2:(length(hr_covariate_levels)+1)],type = "l",col=1,lty=1:(length(hr_covariate_levels)),xlab="time",ylab=y_label) 
	}



	if(nolegend==0) {
		if(nocolor==0) {
		legend(legend_pos,paste(hr_covariate,"=",as.character(hr_covariate_levels)),col=1:(length(hr_covariate_levels)),lty=1:(length(hr_covariate_levels)),bty="n")
		}
		else {
		legend(legend_pos,paste(hr_covariate,"=",as.character(hr_covariate_levels)),col=1,lty=1:(length(hr_covariate_levels)),bty="n")
		}	   
	}

	}
	L_Results
		  


}


