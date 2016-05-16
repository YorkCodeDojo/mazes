module LongestPath

open System  
open System.Windows.Forms  
open System.ComponentModel  
open System.Drawing  

// Directions we can move out of a cell
type direction = UP | DOWN | LEFT | RIGHT 

// We store the maze as an array of arrays of cells.  To make life easy we hold
// quite a lot of information in each cell.  This cell array is the only thing
// which is mutable in the program
type cell = { topWall : bool;
              bottomWall: bool;
              leftWall : bool;
              rightWall : bool;
              row : int;
              column : int;
              visited : bool;
              depth : int;
              onShortestPath : option<direction>}

let doMaze = 

    // The size of the maze
    let mazeWidth = 20
    let mazeHeight = 20

    // Size of each box
    let boxSize = 30

    // A nice border around the maze
    let gap = 100



    // Initialise all the cells in the array (row x column)
    let createCell row column =
        {cell.bottomWall=true; cell.leftWall=true; cell.rightWall=true; cell.topWall=true; cell.column=column; cell.row=row; cell.visited=false; cell.depth= -1; cell.onShortestPath=None}

    let (maze:cell[,]) = Array2D.init mazeHeight mazeWidth createCell

    // We have a single random number source to help with the seeding
    let r = new System.Random()

    /// Adds a border around the maze
    let addGap (p:Point) = new Point(p.X + gap, p.Y + gap)


    /// Draws the available walls for a given cell
    let drawCell (g:Graphics) column row largestDepth = 
        let cell = maze.[column, row]

        // Work out the colour to draw the inside of this cell depending on if it's on the shortest path or not
        let boxColour = 
            if cell.onShortestPath.IsSome then
                Color.White
            else
                Color.White
    
        // Start by drawing the cell as a single block of colour
        g.FillRectangle(new SolidBrush(boxColour), 
                        new Rectangle(addGap (new Point( ((column) * boxSize),  ((row - 1) * boxSize))), 
                                      new Size(boxSize , boxSize )))

        let bitMap = match cell.onShortestPath with
                     | Some(UP) -> "up.JPG"
                     | Some(DOWN) -> "down.JPG"
                     | Some(LEFT) -> "left.JPG"
                     | Some(RIGHT) -> "right.JPG"
                     | _ -> ""

        if bitMap <> "" then
            let bmpPicture : Bitmap = new Bitmap(bitMap)
            let bmpPictureSized = new Bitmap(bmpPicture, new Size(boxSize - 6,  boxSize - 6))
            g.DrawImage(bmpPictureSized,  (gap + (column * boxSize)), gap + ((row - 1) * boxSize))

        // Then draw in any walls
        if cell.leftWall then
            g.DrawLine(Pens.Black, 
                       addGap (new Point((column) * boxSize, row * boxSize)), 
                       addGap (new Point((column) * boxSize, (row - 1) * boxSize)) )

        if cell.rightWall then
            g.DrawLine(Pens.Black, 
                       addGap (new Point((column + 1) * boxSize, row * boxSize)), 
                       addGap (new Point((column + 1) * boxSize, (row - 1) * boxSize)) )

        if cell.topWall then
            g.DrawLine(Pens.Black, 
                       addGap (new Point((column) * boxSize, (row - 1) * boxSize)), 
                       addGap (new Point((column + 1) * boxSize, (row - 1) * boxSize)) )

        if cell.bottomWall then
            g.DrawLine(Pens.Black, 
                       addGap (new Point((column) * boxSize, (row) * boxSize)), 
                       addGap (new Point((column + 1) * boxSize, (row) * boxSize)) )




    /// Calculates the total number of cells in the maze,  we need to visit them all
    let totalNumberOfCells = mazeHeight * mazeWidth

    /// Finds the cell next to this one in the given direction.  If that would move us out of the maze then we return None
    let getAdjacentCell cell direction = 
        let newRow, newColumn = 
            match direction with
            | UP -> cell.row, cell.column - 1
            | DOWN -> cell.row, cell.column + 1
            | LEFT -> cell.row - 1, cell.column
            | RIGHT -> cell.row + 1, cell.column

        if newRow < 0 || newRow >= mazeHeight || newColumn < 0 || newColumn >= mazeWidth then
            None
        else
            Some(maze.[newRow, newColumn])

    /// Returns the cell with a given wall removed
    let removeWall cell direction = 
        match direction with
        | DOWN -> {cell with bottomWall = false}
        | LEFT -> {cell with leftWall = false}
        | RIGHT -> {cell with rightWall = false}
        | UP -> {cell with topWall = false}

    /// Returns the opposite direction
    let reverseDirection = function
        | DOWN -> UP
        | LEFT -> RIGHT
        | RIGHT -> LEFT
        | UP -> DOWN

    /// Keep trying to visit cells until we have reached them all
    let rec nextCell row column numberRemovedSoFar = 
        if numberRemovedSoFar < totalNumberOfCells then
            let cell = maze.[row, column]

            let direction = match r.Next(4) with
                            | 0 -> UP
                            | 1 -> DOWN
                            | 2 -> LEFT
                            | 3 -> RIGHT
                            | _ -> failwith "Unexpected random number"

            match getAdjacentCell cell direction with
            | None -> 
                // We are on the edge,  so we can't move in that direction. Try again
                nextCell row column numberRemovedSoFar

            | Some(adjCell) ->
                // Have we already been to the next cell?
                if adjCell.visited then
                    //We just move there
                    nextCell adjCell.row adjCell.column numberRemovedSoFar
                else
                    //We make a hole in the wall in order to move there
                    maze.[row, column] <- removeWall cell direction
                
                    let adjCell' = removeWall adjCell (reverseDirection direction)
                    maze.[adjCell.row, adjCell.column] <- {adjCell' with visited = true}

                    nextCell adjCell.row adjCell.column (numberRemovedSoFar + 1)

    // Gives a cell returns a list of cells which are immediate neighbours but there
    // isn't a wall between us,  and the depth algorithm hasn't already processed
    let getNeighbours restriction (cell:cell) =
        let canGoUp = match getAdjacentCell cell UP with
                      | Some(c) when restriction c && not cell.topWall -> Some(UP, c)
                      | _ -> None

        let canGoDown = match getAdjacentCell cell DOWN with
                        | Some(c) when restriction c && not cell.bottomWall -> Some(DOWN, c)
                        | _ -> None

        let canGoLeft = match getAdjacentCell cell LEFT with
                        | Some(c) when restriction c && not cell.leftWall -> Some(LEFT, c)
                        | _ -> None

        let canGoRight = match getAdjacentCell cell RIGHT with
                         | Some(c) when restriction c && not cell.rightWall -> Some(RIGHT, c)
                         | _ -> None

        [canGoUp; canGoDown; canGoLeft; canGoRight] |> List.choose id

    // Updates the list of cells with their new depth
    let setDepth (newDepth:int) (cells:List<cell>) =
        cells |> List.iter (fun cell -> maze.[cell.row, cell.column] <- {cell with depth = newDepth})

    // Resets all the depths in the maze to -1.
    let resetDepths maze = 
        maze |> Array2D.iter (fun cell -> maze.[cell.row, cell.column] <- {cell with depth = -1})

    // Walk around the maze to work out how many steps it takes to reach each
    // cell starting at the middle.  The function will eventually return the furthest
    // depth we found.
    let rec walk (currentDepth:int) (cells:List<cell>) =
        if not cells.IsEmpty then
            let nextLevelOfCells = cells |> List.collect (getNeighbours (fun c -> c.depth = -1)) |> List.map (fun (_,c) -> c)
            setDepth (currentDepth + 1) nextLevelOfCells
            walk (currentDepth + 1) nextLevelOfCells
        else
            currentDepth - 1

    let rec findShortestRoute cell = 
        if cell.depth > 0 then
            // Find the neighbour with the lowest depth
            let (nextDirection, nextCell) = cell |> (getNeighbours (fun _ -> true)) |> List.sortBy (fun (_,c) -> c.depth) |> List.head
            maze.[cell.row, cell.column] <- {maze.[cell.row, cell.column] with onShortestPath = Some(nextDirection)}
            findShortestRoute nextCell
        else
            maze.[cell.row, cell.column] <- {maze.[cell.row, cell.column] with onShortestPath = Some(UP)}


    // Searches the cell array to find a cell with the given depth.  Will error if
    // the cell does not exist.
    let findCellWithGivenDepth requiredDepth =
        maze |> Seq.cast<cell> |> Seq.find (fun x -> x.depth = requiredDepth)


    /// Create the maze using a random walk starting from a random location
    let startRow = r.Next(mazeHeight)
    let startColumn = r.Next(mazeWidth)
    nextCell startRow startColumn 0

    // Work out how far each cell is from the bottom left corner
    let cells = [maze.[mazeHeight - 1, 0]]
    setDepth 0 cells
    let largestDepth = walk 0 cells

    // Work out which cell was furthest away
    let furthestCell = findCellWithGivenDepth largestDepth

    // Work out how far all the cells are away from this furthest cell
    resetDepths maze
    let cells' = [maze.[furthestCell.row, furthestCell.column]]
    setDepth 0 cells'
    let largestDepth' = walk 0 cells'

    // Work out which cell is furthest away now
    let furthestCell' = findCellWithGivenDepth largestDepth'

    // The route between our two furthest cells should now be the longest path
    findShortestRoute maze.[furthestCell'.row, furthestCell'.column]

    // Draw it
    let frm = new Form(Text="Longest Path")  
    frm.Paint.Add(fun draw->  
                            let g = draw.Graphics
                            for row in [0..mazeHeight-1] do
                                for column in [0..mazeWidth-1] do
                                    drawCell g column row largestDepth
                        )

    Application.Run(frm)  