# sorting algorithms

# create vector of 20 random numbers to be sorted
set.seed(111)
x = sample(seq(1:100), 20)
x

# insertion sort algorithm
insertSort = function(x){
for(j in 1:length(x)){
	key = x[j]
	i = j - 1
	while(i > 0 && x[i] > key){
		x[i + 1] = x[i]
		i = i - 1
	}
	x[i + 1] = key
}
return(x)
}
system.time(insertSort(x))
system.time(sort(x))

# merge sort algorithm
mergeSort = function(x, p, q, r){
	n_1 = q - p + 1
	n_2 = r - q
	L = seq(1, n_1 + 1)
	R = seq(1, n_2 + 1)
	for(i in 1:n_1){
		L[i] = x[p + i - 1]
	}
	for(j in 1:n_2){
		R[i] = x[q + j]
	}
	L[n_1 + 1] = Inf
	R[n_2 + 1] = Inf
	i = 1
	j = 1
	for(k in p:r){
		if(L[i] <= R[j])
			(x[k] = L[i]) & (i = i + 1)
		else
			(x[k] = R[j]) & (j = j + 1)
	}
return(x)
}
mergeSort(x,4,5,6)