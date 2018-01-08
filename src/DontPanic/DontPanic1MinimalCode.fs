module DontPanic1MinimalCode
let x()=let t=(System.Console.In.ReadLine()).Split [|' '|]in((fun i->int(t.[i])),(fun i->t.[i]))
let (i,s)=x()
let t=(i 3,i 4)::List.init(i 7)(fun _->let(i,s)=x()in(i 0,i 1))
let l b (f,p,d)=if f<0||List.exists(fun x->x=f)b||List.exists(fun(ft,tp)->ft=f&&((d="RIGHT"&&tp>=p)||(d="LEFT"&&tp<=p)))t then printfn"WAIT";b else printfn"BLOCK";f::b
let g(_)=let(i,s)=x()in(i 0,i 1,s 2)
let a=Seq.fold l List.Empty (Seq.initInfinite g)