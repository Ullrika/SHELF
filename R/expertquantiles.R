expertquantiles <-
function(fit, q, d = "best", ex = 1){
	
  if(d == "best"){
    d <- fit$best.fitting[ex, 1]
  }

	if(d == "normal"){
		qx <- qnorm(q, fit$Normal[ex,1], fit$Normal[ex,2]) 		
	}
	
	if(d == "t"){
		qx <- fit$Student.t[ex,1] + fit$Student.t[ex,2] * qt(q, fit$Student.t[ex,3])	
	}
	
  if(d == "sn"){
    qx <- sn::qsn(q, fit$Skewed.normal[ex,1],fit$Skewed.normal[ex,2],fit$Skewed.normal[ex,3])
  }
  
  if(d == "st"){
    qx <- sn::qst(q, fit$Skewed.t[ex,1],fit$Skewed.t[ex,2],fit$Skewed.t[ex,3],fit$Skewed.t[ex,4],method=4)
  }
  
  if(d == "sn_mix"){
    objective <- function(xx){
      min(((sn::psn(xx, fit$Mix.of.skewed.normals[ex,1],fit$Mix.of.skewed.normals[ex,2],
                  fit$Mix.of.skewed.normals[ex,3])*fit$Mix.of.skewed.normals[ex,7] +
      sn::psn(xx, fit$Mix.of.skewed.normals[ex,4], fit$Mix.of.skewed.normals[ex,5],
              fit$Mix.of.skewed.normals[ex,6])*(1-fit$Mix.of.skewed.normals[ex,7]))-q)^2)
      }
    qx <- optim(fit$Mix.of.skewed.normals[ex,1],objective,method="BFGS")$par
  }
  
  if(d == "st_mix"){
    objective <- function(xx){
      min(((sn::pst(xx, fit$Mix.of.skewed.ts[ex,1],fit$Mix.of.skewed.ts[ex,2],
                  fit$Mix.of.skewed.ts[ex,3],fit$Mix.of.skewed.ts[ex,4],method=4)*
          fit$Mix.of.skewed.ts[ex,9] +
      sn::pst(xx, fit$Mix.of.skewed.ts[ex,5], fit$Mix.of.skewed.ts[ex,6],
              fit$Mix.of.skewed.ts[ex,7],fit$Mix.of.skewed.ts[ex,8],method=4)*
        (1-fit$Mix.of.skewed.ts[ex,9]))-q)^2)
      }
      qx <- optim(fit$Mix.of.skewed.ts[ex,1],objective,method="BFGS")$par
    }
  
  if(d == "gamma"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		qx <- xl + qgamma(q, fit$Gamma[ex,1], fit$Gamma[ex,2])  
	}
	
	if(d == "mirrorgamma"){
	  xu <- fit$limits[ex, 2]
	  qx <- xu - qgamma(1 - q, fit$mirrorgamma[ex,1],
	                    fit$mirrorgamma[ex,2])  
	}
	
	if(d == "lognormal"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		qx <- xl + qlnorm(q, fit$Log.normal[ex,1], fit$Log.normal[ex,2]) 
	}
	
	if(d == "mirrorlognormal"){
	  xu <- fit$limits[ex, 2]
	  qx <- xu - qlnorm(1 - q, fit$mirrorlognormal[ex,1],
	                    fit$mirrorlognormal[ex,2]) 
	}	
	
	if(d == "logt"){
		xl <- fit$limits[ex,1]
		if(xl == -Inf){xl <- 0}
		qx <- xl + exp(fit$Log.Student.t[ex,1] + fit$Log.Student.t[ex,2] * qt( q , fit$Log.Student.t[ex,3])) 
	}
	
	if(d == "mirrorlogt"){
	  xu <- fit$limits[ex, 2]
	  qx <- xu - exp(fit$mirrorlogt[ex,1] + fit$mirrorlogt[ex,2] * qt(1- q , fit$mirrorlogt[ex,3])) 
	}
		
	if(d == "beta"){
		xl <- fit$limits[ex,1]
		xu <- fit$limits[ex,2]
		if(xl == -Inf){xl <- 0}
		if(xu == Inf){xu <- 1}
		qx <- xl + (xu - xl) * qbeta(q, fit$Beta[ex,1], fit$Beta[ex,2])
	}
	
	if(d == "hist"){
	  qx <- approx(c(0, fit$probs[ex, ], 1),
	               c(fit$limits[ex, 1],
	                 fit$vals[ex, ],
	                 fit$limits[ex, 2]),
	               xout = q)$y
	}
	
qx	
	
}
