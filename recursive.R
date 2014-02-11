# recursive power function #
power = function(b, n){
	if(n < 0){
		stop('Your exponent must be a non-negative value!')
	} else if(n==0){
		1
	} else if(n==1){ 
		b * 1
	} else {
		b * (power(b, n - 1))
	}
}