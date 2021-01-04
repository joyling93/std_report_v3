n=0
for (i in wday(d1+c(1:(d2-d1)))) {
        if(i%in%c(6,7)){
             n=n+1   
        }
}
n