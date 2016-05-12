module BinaryMaze

open System  
open System.Windows.Forms  
open System.ComponentModel  
open System.Drawing  

let doMaze =

    let r = new System.Random()

    let mazeWidth = 174
    let mazeHeight = 69
    let boxSize = 5
    let gap = 10

    let createCell column row =
        if column = (mazeWidth - 1) then
            true
        elif row = 0 then
            false
        else
            r.Next() % 2 = 0

    let (maze:bool[,]) = Array2D.init mazeWidth mazeHeight createCell

    let addGap (p:Point) = new Point(p.X + gap, p.Y + gap)

    let drawCell (g:Graphics) column row = 
    
        let hasExitTop = maze.[column, row]

        if hasExitTop then
            //Right side
            g.DrawLine(Pens.Blue, 
                       addGap (new Point((column + 1) * boxSize, row * boxSize)), 
                       addGap (new Point((column + 1) * boxSize, (row - 1) * boxSize)) )
        else
            //Top side
            g.DrawLine(Pens.Blue, 
                       addGap (new Point((column) * boxSize, (row - 1) * boxSize)), 
                       addGap (new Point((column + 1) * boxSize, (row - 1) * boxSize)) )

    let frm = new Form(Text="Binary Maze")  
    frm.Paint.Add(fun draw->  
                            let g = draw.Graphics

                            //Left Edge
                            g.DrawLine(Pens.Blue, 
                                        addGap (new Point(0, (mazeHeight-1) * boxSize)), 
                                        addGap (new Point(0, -boxSize) ))

                            //Bottom Edge
                            g.DrawLine(Pens.Blue, 
                                        addGap (new Point(boxSize, (mazeHeight-1) * boxSize)), 
                                        addGap (new Point(boxSize * mazeWidth, (mazeHeight-1) * boxSize) ))

                            for row in [0..mazeHeight-1] do
                                for column in [0..mazeWidth-1] do
                                    drawCell g column row
                        )

    Application.Run(frm)  