let s = [%arrow (fun a -> 
 let x = a + 1 in 
 let y = a + x in  
 let f = [%arrow (fun a -> a)] in
 let z = a + x + y in
 z)] ;; 

