let p2() = Array.init 3 (fun _ -> -1) 

let f1 (v1 : int [])  = 
    let v2 = p2()
    if (v1.[0]=1) && (v1.[1]=1) then
        v2.[0] <- v1.[2]
        v2.[1] <- v1.[3]
        v2.[2] <- v1.[4]
    else
        v2.[1] <- v1.[3]
        v2.[2] <- v1.[4]
    printfn "in %A" v1
    printfn "result %A" v2
    v2

let f2 (v1 : int []) = 
    let v2 = p2()
    if (v1.[0]=1) && (v1.[1]=1) then
        v2.[0] <- v1.[2]
    v2.[1] <- v1.[3]
    v2.[2] <- v1.[4]    
    printfn "in %A" v1
    printfn "result %A" v2
    v2

let p1 n = Array.init n (fun _ -> 0)

let rec bpMatch (x:int) = 
    match x with
    | 0 -> 1
    | _ -> 2 * bpMatch (x-1)

let sp x = bpMatch x - 1
let hasBit i np = (i &&& np)=np
let toBitVector n i = 
    let v = p1 n
    for j in [0 .. n-1] do
        let np = bpMatch j
        if hasBit i np then 
            v.[(n-1)-j] <- 1    
    v

let compareLogics v =
    not (f1 v = f2 v)

let comb n =
    seq {1 .. (sp n)}
    |> Seq.tryFind(fun i ->
        compareLogics (toBitVector n i))

//3 vectors logicVector, assignmentVector, outpuVector
// int not eaquel to value
//floor
//cast
// terminate rec
//no break, return replaced by none result ? , Seq.tryFind
//replace if with match ?
//garbage collector ?
//life time menagment

//optimize tail rec function, write rec with match to refactor to tail rec
//ref from c# project



