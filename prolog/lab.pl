:- use_module(library(clpfd))
    
generate_fields(Rows,Ncols) :-  
  append(Rows, Vs), Vs ins 1..Ncols,
  maplist(all_distinct, Rows),
  maplist(label, Rows).      

