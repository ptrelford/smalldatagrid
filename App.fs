﻿namespace SmallDataGrid

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
    (*
    let toCell s (row:_) = 
        let binding = Binding("Name")
        let text = TextBlock()
        text.DataContext <- row
        text.SetBinding(TextBlock.TextProperty, binding) |> ignore
        text.TextAlignment <- TextAlignment.Right
        text :> FrameworkElement
    
    let grid = new DataGrid<Row>()

    do  for x = 0 to 5 do
            DataGridColumn<_>(sprintf "%d" x, toCell (x.ToString()))
            |> grid.Columns.Add

    let rows = [1..10] |> List.map (fun x -> Row("Hello World " + x.ToString()))
    do  rows |> Seq.iteri (fun i row -> DataGridRow(i, row) |> grid.Rows.Add)
    *)

    let toDataTemplate s =
        let ns = "http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        sprintf "<DataTemplate xmlns='%s'>%s</DataTemplate>" ns s
        |> System.Windows.Markup.XamlReader.Load :?> DataTemplate
    
    let grid = DataGrid()
    
    do  for x = 0 to 50 do
            DataGridTemplateColumn(x.ToString(), toDataTemplate "<TextBlock Text='Cell'/>")
            :> DataGridColumn
            |> grid.Columns.Add
    
    let rows = System.Collections.ObjectModel.ObservableCollection<_>()
    do  grid.LoadingRow
        |> Observable.subscribe
            (fun (e:DataGridRowEventArgs) -> 
                e.Row.Header <- "Row " + e.Row.Item.ToString() ) |> ignore
    do  grid.ItemsSource <- rows
    do  for y = 0 to 40 do
            Row(y.ToString()) |> rows.Add
    
    do  app.Startup.Add(fun _ -> app.RootVisual <- grid)