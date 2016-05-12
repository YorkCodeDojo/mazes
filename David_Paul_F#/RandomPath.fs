module RandomPath

open System  
open System.Windows.Forms  
open System.ComponentModel  
open System.Drawing  


type cell = { topWall : bool;
                bottomWall: bool;
                leftWall : bool;
                rightWall : bool;
                row : int;
                column : int;
                visited : bool}

type direction = UP | DOWN | LEFT | RIGHT 


let doMaze = 

    let mazeWidth = 20
    let mazeHeight = 20
    let boxSize = 30
    let gap = 100

    let createCell row column =
        {cell.bottomWall=true; cell.leftWall=true; cell.rightWall=true; cell.topWall=true; cell.column=column; cell.row=row; cell.visited=false}

    let (maze:cell[,]) = Array2D.init mazeHeight mazeWidth createCell

    let r = new System.Random()

    /// Adds a border around the maze
    let addGap (p:Point) = new Point(p.X + gap, p.Y + gap)

    /// Draws the available walls for a given cell
    let drawCell (g:Graphics) column row = 
        let cell = maze.[column, row]

        if cell.leftWall then
            g.DrawLine(Pens.Blue, 
                       addGap (new Point((column) * boxSize, row * boxSize)), 
                       addGap (new Point((column) * boxSize, (row - 1) * boxSize)) )

        if cell.rightWall then
            g.DrawLine(Pens.Green, 
                       addGap (new Point((column + 1) * boxSize, row * boxSize)), 
                       addGap (new Point((column + 1) * boxSize, (row - 1) * boxSize)) )

        if cell.topWall then
            g.DrawLine(Pens.Red, 
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


    // Pick a random location to start from
    let startRow = r.Next(mazeHeight)
    let startColumn = r.Next(mazeWidth)

    nextCell startRow startColumn 0


    let frm = new Form(Text="Random Walk")  
    frm.Paint.Add(fun draw->  
                            let g = draw.Graphics
                            for row in [0..mazeHeight-1] do
                                for column in [0..mazeWidth-1] do
                                    drawCell g column row
                        )

    Application.Run(frm)  