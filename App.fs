namespace SmallDataGrid

open System.Windows
open System.Windows.Controls
open System.Windows.Data

type Row (name:string) =
    inherit ObservableObject()
    let mutable name = name
    member row.Name
        with get () = name
        and set value =
            name <- value
            row.NotifyPropertyChanged <@row.Name@>
    override row.ToString() = name

type App() as app =
    inherit Application()

    let toCell (row:Row) = 
        let binding = Binding("Name")
        let text = TextBlock()
        text.Text <- row.ToString()
        //text.DataContext <- row
        //text.SetBinding(TextBlock.TextProperty, binding) |> ignore
        text.TextAlignment <- TextAlignment.Right
        DataGridCell((fun () -> text.Text <- "Updated " + row.ToString()), Content=text)
    
    let grid = new DataGrid<Row>()

    do  for x = 0 to 300 do
            DataGridColumn<_>(sprintf "%d" x, toCell)
            |> grid.Columns.Add

    let rows = [1..20] |> List.map (fun x -> Row("Cell " + x.ToString()))
    do  rows |> Seq.iteri (fun i row -> DataGridRow(i, row) |> grid.Rows.Add)
    do  grid.Rows.RemoveAt(1)
    do  grid.Rows.Insert(9, DataGridRow("Hello", Row("New")))
    do  grid.CellAt(3,3).Update()
    
    (*
    let toDataTemplate s =
        let ns = "http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        sprintf "<DataTemplate xmlns='%s'>%s</DataTemplate>" ns s
        |> System.Windows.Markup.XamlReader.Load :?> DataTemplate
    
    //let grid = System.Windows.Controls.DataGrid() 
    let grid = DataGrid()
    
    do  for x = 0 to 200 do
            let column = DataGridTemplateColumn(x.ToString(), toDataTemplate "<TextBlock Text='Cell'/>")
            //column.CellTemplate <- toDataTemplate "<TextBlock Text='Cell'/>"
            column
            :> DataGridColumn
            |> grid.Columns.Add
    
    let rows = System.Collections.ObjectModel.ObservableCollection<_>()
    do  grid.LoadingRow
        |> Observable.subscribe
            (fun (e:DataGridRowEventArgs) -> 
                e.Row.Header <- "Row ") |> ignore
    do  grid.ItemsSource <- rows
    do  for y = 0 to 30 do
            Row(y.ToString()) |> rows.Add
    *)
    do  app.Startup.Add(fun _ -> app.RootVisual <- grid)
