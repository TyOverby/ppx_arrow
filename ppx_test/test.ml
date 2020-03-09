let k = [%arrow (fun a -> 
    let x = a + 1 in 
    let y = a + x in 
    y
)]

let s = [%arrow (fun a -> 
 let x = a + 1 in 
 let y = a + x in  
 let w = [%call f x] in
 let z = y + w in
 z
)]

