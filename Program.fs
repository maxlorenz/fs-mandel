open System.Windows.Forms
open System.Drawing
open System.Numerics

type Canvas = { Image: Bitmap; Graphics: Graphics }
type Dimensions = { Width: int; Height: int; }
type Plot = { XMin: float; XMax: float; YMin: float; YMax: float }

let dimensions = { Width = 320; Height = 240 }                 // Image size
let plot = { XMin = -2.0; XMax = 1.0; YMin = -1.0; YMax = 1.0 } // Image zoom
let steps = 10000

let convert dimensions plot x y = 
    (plot.XMin + (float x / float dimensions.Width) * (plot.XMax - plot.XMin),
     plot.YMin + (float y / float dimensions.Height) * (plot.YMax - plot.YMin))

let createCanvas dimensions =
    let image = new Bitmap (dimensions.Width, dimensions.Height)
    { Image = image; Graphics = Graphics.FromImage image }

let drawPixel canvas x y brush = canvas.Graphics.FillRectangle(brush, x, y, 1, 1)

let showImage canvas = 
    let picture = new PictureBox(Image = canvas.Image, Size = canvas.Image.Size)
    let form = new Form(AutoSize = true)
    form.Controls.Add picture
    Application.Run form

let rec mandelbrot steps c z =
    match steps with
    | 0                             -> 0        // break, no steps left
    | s when Complex.Abs(z) > 4.0   -> s        // draw, if |z| gets bigger than 4
    | _ -> mandelbrot (steps - 1) c (z * z + c) // mandelbrot step: z^2 + c

let canvas = createCanvas dimensions
let pixels = seq { for x in 0..dimensions.Width do for y in 0..dimensions.Height do yield x, y }

[<EntryPoint>]
let main argv = 
    for x, y in pixels do
        if y = 0 then printfn "%d of %d" x dimensions.Width

        mandelbrot steps (convert dimensions plot x y |> Complex) Complex.Zero
        |> fun c -> drawPixel canvas x y (new SolidBrush(Color.FromArgb(255, c/100, c/100, c/100)))

    showImage canvas; 0