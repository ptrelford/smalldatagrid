namespace SmallDataGrid

open System.Windows
open System.Windows.Controls

type App() as app =
    inherit Application()

    let toCell s (row:int) = 
        let text = TextBlock(Text=s+":"+row.ToString())       
        text.TextAlignment <- TextAlignment.Right
        text.Width <- 48.0
        text.Height <- 12.0           
        text :> FrameworkElement
    
    let grid = new DataGrid<int>()    
    do  for y = 0 to 10 do grid.Items.Add y
    do  for x = 0 to 5 do
            DataGridColumn(sprintf "%d" x, toCell (x.ToString()))             
            |> grid.Columns.Add
    do  app.Startup.Add(fun _ -> app.RootVisual <- grid)