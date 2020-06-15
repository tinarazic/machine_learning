set.seed(105); 
options(digits = 16)
A = matrix(runif(100), ncol=2);
A[2,2]
sum(A)

set.seed(265);
b = runif(100);
max(b)
b[2]
var(b)
