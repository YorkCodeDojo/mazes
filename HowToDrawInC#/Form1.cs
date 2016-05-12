using System;
using System.Drawing;
using System.Windows.Forms;

namespace Circle
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }


        private void Form1_Paint(object sender, PaintEventArgs e)
        {
            var g = e.Graphics;
            g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.HighQuality;

            DrawStraightLine(g);

            DrawCircleUsingLines(g);

            var color = Pens.Orange;
            g.DrawEllipse(color, new Rectangle(50, 50, 100, 100));
            g.DrawRectangle(color, new Rectangle(80, 80, 30, 30));
            g.DrawString("Sample Text", SystemFonts.DefaultFont, Brushes.Black, new Point(195, 95));

            color = Pens.Blue;
            g.DrawArc(color, new Rectangle(300, 150, 80, 80), 90, 180);
        }

        /// <summary>
        /// Draws a single line on g
        /// </summary>
        /// <param name="g"></param>
        private static void DrawStraightLine(Graphics g)
        {
            var colour = Pens.Green;
            var startPoint = new Point(0, 0);
            var endPoint = new Point(1000, 1000);

            g.DrawLine(colour, startPoint, endPoint);
        }

        /// <summary>
        /// Draws a circle on g using just lines.
        /// </summary>
        /// <param name="g"></param>
        private void DrawCircleUsingLines(Graphics g)
        {
            var theta = 0;  // angle that will be increased each loop
            var h = this.Height / 2;      // x coordinate of circle center
            var k = this.Width / 2;      // y coordinate of circle center
            var step = 15;  // amount to add to theta each time (degrees)
            var r = 100;
            var last = new Point(0, 0);
            while (theta <= 360)
            {
                var x = h + r * Math.Cos(ConvertToRadians(theta));
                var y = k + r * Math.Sin(ConvertToRadians(theta));
                var newPoint = new Point((int)x, (int)y);
                theta += step;

                if (last.X != 0)
                    g.DrawLine(Pens.Black, last, newPoint);
                last = newPoint;
            }
        }

        /// <summary>
        /// Convert degrees to radians
        /// </summary>
        /// <param name="angle"></param>
        /// <returns></returns>
        private double ConvertToRadians(double angle)
        {
            return (Math.PI / 180) * angle;
        }
    }
}
