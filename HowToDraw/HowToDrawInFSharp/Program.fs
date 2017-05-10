open System  
open System.Windows.Forms  
open System.ComponentModel  
open System.Drawing  


let ConvertToRadians angle = (Math.PI / 180.0) * angle;

let drawLine (g:Graphics) =
    let colour = Pens.Green;
    let startPoint = new Point(0, 0)
    let endPoint = new Point(1000, 1000)
    g.DrawLine(colour, startPoint, endPoint);

let drawCircleUsingLines (drawtriangleform:Form) (g:Graphics) =
    let h = drawtriangleform.Height / 2     // x coordinate of circle center
    let k = drawtriangleform.Width / 2      // y coordinate of circle center
    let step = 15                           // amount to add to theta each time (degrees)
    let r = 100

    let rec drawLine theta (lastPt:Point) =
        if theta <= 360 then
            let thetaR = ConvertToRadians((float)theta)
            let x = (float)h + (float)r * Math.Cos(thetaR)
            let y = (float)k + (float)r * Math.Sin(thetaR);
            let newPoint = new Point((int)x, (int)y);
            let newTheta = theta + step;

            if (lastPt.X <> 0) then
                g.DrawLine(Pens.Black, lastPt, newPoint);
            drawLine newTheta newPoint

    let theta = 0
    let last = new Point(0, 0)
    drawLine theta last

let drawtriangleform = new Form(Text="Drawing Example")  
drawtriangleform.Paint.Add(fun draw->  
                                let g = draw.Graphics
                                drawLine g
                                drawCircleUsingLines drawtriangleform g
                           )

Application.Run(drawtriangleform)  