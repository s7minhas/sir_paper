perfScore = function(act, pred){
	logarith = -log(dpois(act, lambda=pred))
	brier = -2*dpois(act,lambda=pred) + sapply(pred,
		function(act){ sum(dpois(1:1000,lambda=act)^2) })
	spherical= - dpois(act,pred) / sqrt(sapply(pred,
		function(act){ sum(dpois(1:1000,lambda=act)^2) }))
	resid = act-pred
	dawid=(resid)^2/pred + log(pred)
	rmse = sqrt(median( (resid)^2 )) # using median due to outliers

	return(list(
		logarith=mean(logarith[logarith!=Inf]),
		brier=mean(brier),
		spherical=mean(spherical[spherical!=-Inf]),
		dawidSebastiani=mean(dawid),rmse=rmse
		))
}