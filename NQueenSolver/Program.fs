module NQueen.Program
open System
open System.Collections.Generic
open System.Text

type ErrMsg = String

let flip f x y = f y x
let (=<<) = Result.bind
let (>>=) x = flip (=<<) x
let (<%>) = Result.map  // <$> in Haskell
let (<&>) x = flip (<%>) x

(* State of board
  0 1 2 3 4 5 6 7 -> y
0 _ _ _ _ _ _ _ _ 
1 _ _ _ _ _ _ _ _
2 _ _ _ _ _ _ _ _
3 _ _ _ _ _ _ _ _
4 _ _ _ _ _ _ _ _
5 _ _ _ _ _ _ _ _
6 _ _ _ _ _ _ _ _
7 _ _ _ _ _ _ _ _     
|
x

HasCol: y = 0, 1, 2, ..., 7 : 8(N) lines
 |
 |  (vertical direction)
 |
 
HasUp: x+y = 0, 1, 2, ..., 14 : 15(2N-1) lines
  /
 /   (diagonal direction, which is from lower left to upper right)
/

HasDown: x-y+N-1 = 0, 1, 2, ..., 14 : 15(2N-1) lines
\
 \   (diagonal direction, which is from upper left to lower right)
  \
*)
type Square = int * int // x, y

type Dat =
    {
        Row : int32 array    // Row[i] := col index of i-th row 
        HasCol : bool array  // HasCol[i] := i-th col has queen?
        HasUp : bool array   // HasDown[i] := i-th down diagonal line has queen?
        HasDown : bool array // HasUp[i] := i-th up diagonal line has queen?                
        Res : HashSet<string> // Answers: column position per row
    }

[<Literal>]
let Unset = -1
let Success = 0
let Failure = -1

let private _canPlace :
        Square->
        Dat->
        bool =
    fun (x,y) dat ->
        let N = Array.length dat.HasCol
        dat.HasCol[y] = false &&
        dat.HasUp[x+y] = false &&
        dat.HasDown[x-y+N-1] = false

let private _place :
        Square->
        Dat->
        Dat =
    fun (x,y) dat ->
        let N = Array.length dat.HasCol
        // let row = Array.copy dat.Row
        // let hasCol = Array.copy dat.HasCol
        // let hasUp = Array.copy dat.HasUp
        // let hasDown = Array.copy dat.HasDown
        
        // _place and _take have side effects to
        // avoid the copying cost of arrays.
        let row = dat.Row
        let hasCol = dat.HasCol
        let hasUp = dat.HasUp
        let hasDown = dat.HasDown
        row[x] <- y
        hasCol[y] <- true
        hasUp[x+y] <- true
        hasDown[x-y+N-1] <- true        
        {dat with
            Row = row
            HasCol = hasCol
            HasUp = hasUp
            HasDown = hasDown
        }

let private _take :
        Square->
        Dat->
        Dat =
    fun (x,y) dat ->
        let N = Array.length dat.HasCol
        let row = dat.Row
        let hasCol = dat.HasCol
        let hasUp = dat.HasUp
        let hasDown = dat.HasDown        
        row.[x] <- Unset
        hasCol.[y] <- false
        hasUp.[x+y] <- false
        hasDown.[x-y+N-1] <- false
        {dat with
            Row = row
            HasCol = hasCol
            HasUp = hasUp
            HasDown = hasDown
        }

let private _aryToStr :
        int array->
        string->
        string =
    fun ary sep ->
        Array.map
            (fun x -> string $"%x{(x+1)}")
            ary            
        |> String.concat sep 

let rec private _solve :
        int-> // row index
        Dat->
        Dat =
    fun x dat ->
        let N = Array.length dat.HasCol
                           
        for y in 0..N-1 do // col index
            if _canPlace (x,y) dat then
                _place (x,y) dat
                |> fun placedDat ->
                    match N <= x+1 with // end of the board?
                    | true -> // add the answer
                        placedDat.Res.Add(_aryToStr dat.Row " ") |> ignore
                        placedDat
                    | false -> // search the next row
                        _solve (x+1) placedDat
                |> fun judgedDat ->                    
                    _take (x,y) judgedDat |> ignore
        dat
        
let private _parseArgs : string array ->
        Result<int, ErrMsg> =
    fun args ->
        match args.Length with
        | 0 ->
            Ok 8
        | 1  ->
            match Int32.TryParse args[0] with
            | true, qnum when qnum > 0->
                Ok qnum
            | _ ->
                Error "Usage: NQueenSolver [qnum] (qnum > 0)"                        
        | _ ->
            Error "Usage: NQueenSolver [qnum]"                            
               
let private _main :
        string array ->
        Result<string,ErrMsg> =
    fun args ->
        args
        |> _parseArgs
        <&> fun N ->
                {
                    Row = Array.init N (fun _ -> Unset)
                    HasCol = Array.init N (fun _ -> false)
                    HasUp = Array.init (2*N-1) (fun _ -> false)
                    HasDown = Array.init (2*N-1) (fun _ -> false)
                    Res = HashSet<string>()
                }                
                |> _solve 0
            
        <&> fun dat ->
                let sb = StringBuilder()
                sb.AppendLine "Column position per row" |> ignore
                
                match dat.Res.Count with
                | 0 ->
                     sb.AppendLine "No solution" |> ignore
                | cnt ->
                    for r in dat.Res do
                        sb.AppendLine r |> ignore
                    sb.AppendLine $"{cnt} solutions" |> ignore
                
                sb.ToString()
            
[<EntryPoint>]
let main args =
    match _main args with
    | Ok s -> 
        Console.WriteLine s
        Success
    | Error eMsg ->
        Console.Error.WriteLine eMsg
        Failure
    