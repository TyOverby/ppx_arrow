(*let k = [%arrow (fun _ -> i)]*)
let k = [%arrow (fun i -> 
    let rec f = f in
    i)]
(*let s = [%arrow (fun () -> i)]*)
