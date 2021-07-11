expertprobs <-
function(fit, x, d = "best", ex = 1){
	
  if(d == "best"){
    d <- fit$best.fitting[ex, 1]
  }

	
		
	if(d == "normal"){
		px <- pnorm(x, fit$Normal[ex,1], fit$Normal[ex,2]) 		
	}
	
	if(d == "t"){
		px <- pt((x - fit$Student.t[ex,1])/fit$Student.t[ex,2], fit$Student.t[ex,3])	
	}
	
  if(d == "sn"){
    px <- sn::psn(x, fit$Skewed.normal[ex,1],fit$Skewed.normal[ex,2],fit$Skewed.normal[ex,3])
  }
  
  if(d == "st"){
    px <- sn::pst(x, fit$Skewed.t[ex,1],fit$Skewed.t[ex,2],fit$Skewed.t[ex,3],fit$Skewed.t[ex,4])
  }
  
  if(d == "sn_mix"){
    px <- sn::psn(x, fit$Mix.of.skewed.normals[ex,1],fit$Mix.of.skewed.normals[ex,2],
                  fit$Mix.of.skewed.normals[ex,3])*fit$Mix.of.skewed.normals[ex,7] +
      sn::psn(x, fit$Mix.of.skewed.normals[ex,4], fit$Mix.of.skewed.normals[ex,5],
              fit$Mix.of.skewed.normals[ex,6])*(1-fit$Mix.of.skewed.normals[ex,7])
  }
  
  if(d == "st_mix"){
    px <- sn::pst(x, fit$Mix.of.skewed.ts[ex,1],fit$Mix.of.skewed.ts[ex,2],
                  fit$Mix.of.skewed.ts[ex,3],fit$Mix.of.skewed.ts[ex,4])*fit$Mix.of.skewed.ts[ex,9] +
      sn::pst(x, fit$Mix.of.skewed.ts[ex,5], fit$Mix.of.skewed.ts[ex,6],
              fit$Mix.of.skewed.ts[ex,7],fit$Mix.of.skewed.ts[ex,8])*(1-fit$Mix.of.skewed.ts[ex,9])
  }
  
  if(d == "gamma"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		px <- pgamma(x - xl, fit$Gamma[ex,1], fit$Gamma[ex,2])  
	}
	
	if(d == "mirrorgamma"){
	  xu <- fit$limits[ex, 2]
	  px <- 1 - pgamma(xu - x, fit$mirrorgamma[ex,1], fit$mirrorgamma[ex,2])  
	}
	
	if(d == "lognormal"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		px <- plnorm(x - xl, fit$Log.normal[ex,1], fit$Log.normal[ex,2]) 
	}
	
	if(d == "mirrorlognormal"){
	  xu <- fit$limits[ex, 2]
	  px <- 1 - plnorm(xu - x, fit$mirrorlognormal[ex,1], fit$mirrorlognormal[ex,2]) 
	}	
	
	if(d == "logt"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		# Avoid NaN
		px <- pt( (log(abs(x - xl)) - fit$Log.Student.t[ex,1]) 
		          / fit$Log.Student.t[ex,2], fit$Log.Student.t[ex,3])
		px[x <= xl] <- 0 # Set to 0 for x < lower limit
	}
	
	if(d == "mirrorlogt"){
	  xu <- fit$limits[ex, 2]
	  # Avoid NaN
	  px <- 1 - pt( (log(abs(xu - x)) - fit$mirrorlogt[ex,1]) 
	            / fit$mirrorlogt[ex,2], fit$mirrorlogt[ex,3])
	  px[x >= xu] <- 1 # Set to 1 for x > upper limit
	}
		
	if(d == "beta"){
		xl <- fit$limits[ex,1]
		xu <- fit$limits[ex,2]
		if(xl == -Inf){xl <- 0}
		if(xu == Inf){xu <- 1}
		px <-  pbeta( (x - xl) / (xu - xl), fit$Beta[ex,1], fit$Beta[ex,2])
	}
	
	if(d == "hist"){
	  px <- approx(c(fit$limits[ex, 1],
	                 fit$vals[ex, ],
	                 fit$limits[ex, 2]),
	               c(0, fit$probs[ex, ], 1),
	               xout = x,
	               yleft = 0, 
	               yright = 1)$y
	}
  
px	
	
}
