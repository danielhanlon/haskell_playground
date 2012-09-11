cartProd (set:sets) = [x:xs | x<-set, xs<-cp]
                      where cp=cartProd sets
cartProd [] = [[]]
