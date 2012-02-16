namespace SmallDataGrid

open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.ComponentModel
open Microsoft.FSharp.Quotations.Patterns

type ObservableObject () =
    let propertyChanged = 
        Event<PropertyChangedEventHandler,PropertyChangedEventArgs>()
    let getPropertyName = function 
        | PropertyGet(_,pi,_) -> pi.Name
        | _ -> invalidOp "Expecting property getter expression"
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChanged.Publish
    member this.NotifyPropertyChanged propertyName = 
        propertyChanged.Trigger(this,PropertyChangedEventArgs(propertyName))
    member this.NotifyPropertyChanged quotation = 
        quotation |> getPropertyName |> this.NotifyPropertyChanged

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
    do  app.Startup.Add(fun _ -> app.RootVisual <- grid)