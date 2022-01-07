//System.Console.BackgroundColor <- System.ConsoleColor.White
System.Console.ForegroundColor <- System.ConsoleColor.Blue

type BoardDisplay (rows:int, cols:int) =
    //let board = Array.create rows (Array.create cols "a")
    let board : string[,] = Array2D.init (rows*2+1) (cols*2+1) (fun i j -> match (i,j) with
                                                                            |(i,j) when j % 2 = 0 && i % 2 = 0 ->  "+" 
                                                                            |(i,j) when j % 2 = 1 && (i=0 || i=rows*2) -> "-"
                                                                            |(i,j) when i % 2 = 1 && (j=0 || j=cols*2) -> "|"
                                                                            |_ -> "")
    member this.Set (r, c, cont) = board.[r*2-1,c*2-1] <- cont
    member this.SetBottomWall (row, col) = board.[row*2,col*2-1] <- "-"
    member this.SetRightWall (row, col) = board.[row*2-1,col*2] <- "|"
    member this.Show =  board |> Seq.cast<string> |> Seq.toArray
                        |> Array.iteri (fun j v -> (match j with
                                                       |j when j = 0 ->  printf "%2s" v
                                                       |j when (j+1) % (cols*2+1) = 0 -> printfn "%2s" v
                                                       //|j when j % (cols*2+1) = 0 -> printf "%s" v
                                                       |_ -> printf "%2s" v
                                                       ))

let test = BoardDisplay (5, 5)
test.Set (3, 4, " -")
test.Set (3, 1, "--")
test.SetBottomWall (1,1)
test.SetBottomWall (4,1)
test.SetBottomWall (3,2)
test.SetBottomWall (3,3)
test.SetBottomWall (3,4)
test.SetBottomWall (2,2)
test.SetBottomWall (2,3)
test.SetBottomWall (2,4)
test.SetRightWall (2,1)
//test.SetRightWall (3,1)
test.SetRightWall (4,1)
test.SetRightWall (3,4)
test.SetRightWall (3,3)

test.Show


type Direction = North | South | East | West

type Action =
    | Stop of Position
    | Continue of Direction * Position
    | Ignore

[<AbstractClass >]
type BoardElement () =
    abstract member RenderOn : BoardDisplay -> unit
    abstract member Interact : Robot -> Direction -> Action
    default __.Interact _ _ = Ignore
    abstract member GameOver : Robot list -> bool
    default __.GameOver l = if l |> List.exists (fun x -> x.Position = (x.R, x.C)) then true else false //Virker ikke
    


and Robot(row:int, col:int, name:string) =
    inherit BoardElement()
    member this.Position : int * int = (row, col)
    override this.Interact other dir: Action = match dir with
                                               |North -> if other.Position = (row+1,col) || other.Position =(row+2,col)
                                                            then (Stop of Position)
                                                            else Ignore
                                               |South -> if other.Position = (row-1,col) || other.Position =(row-2,col)
                                                            then (Stop of Position)
                                                            else Ignore
                                               |East -> if other.Position = (row,col-1) || other.Position =(row,col-2)
                                                            then (Stop of Position)
                                                            else Ignore
                                               |West -> if other.Position = (row,col+1) || other.Position =(row,col+2)
                                                            then (Stop of Position)
                                                            else Ignore
    override this.RenderOn display = display.Set (row, col, name)
    member val Name = name
    member robot.Step (dir: Direction) = if not (Stop of Position) then 
                                            match Direction with
                                            |North -> robot.Position <- (r-1,c)
                                            |South -> robot.Position <- (r+1,c)
                                            |East -> robot.Position <- (r,c+1)
                                            |West -> robot.Position <- (r,c-1)

and Goal (r0, c0) = // ved ikke hvad jeg laver
    let mutable r = r0
    let mutable c = c0
    member this.Position = (r, c)
    member this.R with get() = r and set(value) = r <- value
    member this.C with get() = c and set(value) = c <- value
    new() = Goal()   

//type BoardFrame (r, c) =
//    let mutable 

//display.Goal r c = (r, c)
// this.GameOver l r c = l |> List.exists (fun (x: Robot) -> x.Position = this.Goal)


