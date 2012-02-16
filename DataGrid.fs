namespace SmallDataGrid

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Collections.Specialized
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Shapes

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

[<AutoOpen>]
module GridLines =
    let createHorizontalLine () =
        Rectangle(Height=1.0,
                  Stroke=SolidColorBrush Colors.Gray,
                  VerticalAlignment=VerticalAlignment.Top)
    let createVerticalLine () =
        Rectangle(Width=1.0,
                  Stroke=SolidColorBrush Colors.Gray,
                  HorizontalAlignment=HorizontalAlignment.Left)

type internal ICollectionChanger<'T> =
    abstract member Insert : int * 'T -> unit
    abstract member RemoveAt : int -> unit
    abstract member SetAt : int * 'T -> unit
    abstract member Clear : unit -> unit

[<AutoOpen>]
module internal Relay =
    let relayChange (dest:ICollectionChanger<'T>) (change:NotifyCollectionChangedEventArgs) =
        match change.Action with
        | NotifyCollectionChangedAction.Add ->
            dest.Insert(change.NewStartingIndex,change.NewItems.[0] :?> 'T)
        | NotifyCollectionChangedAction.Remove -> 
            dest.RemoveAt(change.OldStartingIndex)
        | NotifyCollectionChangedAction.Replace -> 
            dest.SetAt(change.NewStartingIndex, change.NewItems.[0] :?> 'T)
        | NotifyCollectionChangedAction.Reset -> 
            dest.Clear()
        | _ -> invalidOp ""

    let relayChanges (source:INotifyCollectionChanged) (dest:ICollectionChanger<'T>) =
        source.CollectionChanged |> Observable.subscribe (relayChange dest)
       
type DataGridColumn<'TItem>(header:FrameworkElement, 
                            createCell:Func<'TItem,FrameworkElement>,
                            // TODO: updateCell:Action<'TItem>
                            definition:ColumnDefinition) =
    new (header:string,createCell:'TItem->FrameworkElement) =
        DataGridColumn(TextBlock(Text=header),createCell,ColumnDefinition(Width=GridLength.Auto))
    new (header,createCell:'TItem->FrameworkElement) =
        DataGridColumn(header,createCell,ColumnDefinition(Width=GridLength.Auto))
    new (header,createCell:'TItem->FrameworkElement,width:GridLength) =
        DataGridColumn(header,createCell,ColumnDefinition(Width=width))
    member column.Header = header
    member column.CreateCell row = createCell.Invoke row
    member column.Definition = definition

type DataGridRow<'TItem>(header:obj, item:'TItem, definition:RowDefinition) =
    inherit ObservableObject()
    let mutable header = header
    new (header,item) = 
        let defaultRowHeight = 22.0
        DataGridRow(header,item,RowDefinition(Height=GridLength(defaultRowHeight)))
    member this.Item = item
    member this.Header
        with get () = header
        and set value =
            header <- value
            this.NotifyPropertyChanged <@this.Header@>
    member this.Definition = definition

[<AutoOpen>]
module internal Changers =

    let createSplitter () =
        let splitter = GridSplitter(VerticalAlignment=VerticalAlignment.Stretch)
        splitter.HorizontalAlignment <- HorizontalAlignment.Right
        Grid.SetRow(splitter,0)
        Grid.SetRowSpan(splitter, 1)
        splitter.Width <- 3.0
        splitter

    let margin = Thickness(1.0,1.0,0.0,0.0)
    let setColumn(element,index) = Grid.SetColumn(element,index+1)
    let setRow(element,index) = Grid.SetRow(element,index+1)

    let createColumnsChanger 
            (grid:Grid) 
            (headers:IList<_>)
            (splitters:IList<_>)
            (vlines:IList<_>,hlines:IList<_>) 
            (rows:DataGridRow<_> seq)
            (rowCells:List<List<_>>) =

        let insertColumnDefinition(index,definition) =
            grid.ColumnDefinitions.Insert(index+1, definition)
        let removeColumnDefinition(index) =
            grid.ColumnDefinitions.RemoveAt(index+1)
        let clearColumnDefinitions() =
            let xs = grid.ColumnDefinitions 
            for index = xs.Count-1 downto 1 do xs.RemoveAt index
        { new ICollectionChanger<DataGridColumn<'TItem>> with
            member target.Insert(index,column) =
                insertColumnDefinition(index,column.Definition)

                let inline pushRight (elements:IList<_>) =
                    let columnCount = elements.Count
                    for x = columnCount-1 downto index do
                        setColumn(elements.[x], x+1)

                let inline insertElement element =
                    setColumn(element, index)
                    grid.Children.Add element

                pushRight vlines
                let vline = createVerticalLine()
                Grid.SetRowSpan(vline, 1 + Seq.length rows)
                insertElement vline
                vlines.Insert(index,vline)

                for hline in hlines do
                    Grid.SetColumnSpan(hline, 1 + vlines.Count)
                    
                pushRight headers
                let header = column.Header
                insertElement header
                headers.Insert(index,header)

                pushRight splitters
                let splitter = createSplitter()
                insertElement (splitter)
                splitters.Insert(index, splitter)

                rows |> Seq.iteri (fun y row -> 
                    let cells = rowCells.[y]
                    pushRight cells
                    let cell = column.CreateCell(row.Item) 
                    cell.Margin <- margin
                    insertElement cell
                    setRow(cell, y)
                    cells.Insert(index, cell)
                )

            member target.RemoveAt(index) =
                let inline pullLeft (elements:IList<_>) =
                    for x = elements.Count-1 downto index+1 do
                        setColumn(elements.[x], x-1)

                let inline removeElement element =
                    grid.Children.Remove element |> ignore

                removeElement vlines.[index]
                pullLeft vlines
                vlines.RemoveAt(index)

                for hline in hlines do
                    Grid.SetColumnSpan(hline, 1 + vlines.Count)

                removeElement headers.[index]
                pullLeft headers
                headers.RemoveAt(index)

                removeElement splitters.[index]
                pullLeft splitters
                splitters.RemoveAt(index)

                rows |> Seq.iteri (fun y row ->
                    let cells = rowCells.[y]
                    removeElement cells.[index]
                    pullLeft cells
                    rowCells.[y].RemoveAt(index)
                )
                removeColumnDefinition(index) 
            member target.SetAt(index,value) = 
                target.RemoveAt(index)
                target.Insert(index,value)
            member target.Clear() =
                let removeElements (elements:IList<_>) =
                    for element in elements do grid.Children.Remove element |> ignore
                    elements.Clear()
                removeElements headers
                removeElements vlines
                removeElements splitters
                for y = rowCells.Count - 1 downto 0 do
                    removeElements rowCells.[y]
                clearColumnDefinitions()
        }

    let createRowsChanger 
            (grid:Grid) 
            (rowHeaders:IList<FrameworkElement>)
            (vlines:IList<_>,hlines:IList<_>)
            (columns:DataGridColumn<'TItem> seq)
            (rowCells:List<List<_>>) =
        
        let insertRowDefinition(index,definition) =
            grid.RowDefinitions.Insert(index+1, definition)
        let removeRowDefinition(index) =
            grid.RowDefinitions.RemoveAt(index+1)
        let clearRowDefinitions() =
            let ys = grid.RowDefinitions
            for index = ys.Count-1 downto 1 do ys.RemoveAt index
        { new ICollectionChanger<DataGridRow<'TItem>> with
            member target.Insert(index,row) =
                insertRowDefinition(index,row.Definition)

                let rowCount = rowCells.Count
                let pushDown f =
                    for y = rowCount - 1 downto index do
                        let element = f y
                        setRow(element, y+1)

                columns |> Seq.iteri(fun x row -> 
                    pushDown (fun y -> rowCells.[y].[x]) 
                )
                
                pushDown (fun y -> hlines.[y])
                let hline = createHorizontalLine ()
                setRow(hline,index)  
                Grid.SetColumnSpan(hline, 1 + Seq.length columns)
                grid.Children.Add(hline)
                hlines.Insert(index,hline)

                for vline in vlines do
                    Grid.SetRowSpan(vline, 1 + hlines.Count)

                pushDown (fun y -> rowHeaders.[y])
                let header = 
                    match row.Header with
                    | :? FrameworkElement as fe -> fe
                    | null -> TextBlock() :> FrameworkElement
                    | x -> TextBlock(Text=row.Header.ToString()) :> FrameworkElement
                setRow(header,index)
                grid.Children.Add(header)
                rowHeaders.Insert(index, header)

                let cells = List<_>()
                columns |> Seq.iteri(fun x column ->
                    let cell = column.CreateCell(row.Item)
                    cell.Margin <- margin  
                    setColumn(cell, x)
                    setRow(cell, index)
                    cells.Insert(x, cell)
                    grid.Children.Add cell
                )
                rowCells.Insert(index, cells)

            member target.RemoveAt(index) =
                let inline pullUp f =
                    for y = rowCells.Count - 1 downto index+1 do
                        let element = f y
                        setRow(element, y-1)

                let inline removeElement element =
                    grid.Children.Remove element |> ignore

                removeElement hlines.[index]
                pullUp (fun y -> hlines.[y])
                hlines.RemoveAt(index)

                for vline in vlines do
                    Grid.SetRowSpan(vline, 1 + hlines.Count)

                removeElement rowHeaders.[index]
                pullUp (fun y -> rowHeaders.[y])
                rowHeaders.RemoveAt(index)

                columns |> Seq.iteri (fun x column ->
                    let cell = rowCells.[index].[x]
                    removeElement cell
                    pullUp (fun y -> rowCells.[y].[x])
                )
                rowCells.RemoveAt(index)

                removeRowDefinition(index)

            member target.SetAt(index,item) =
                target.RemoveAt(index)
                target.Insert(index,item)
            member target.Clear() =
                let removeElements (elements:IList<_>) =
                    for element in elements do grid.Children.Remove element |> ignore
                    elements.Clear()
                for y = rowCells.Count - 1 downto 0 do
                    removeElements rowCells.[y]
                rowCells.Clear()
                removeElements rowHeaders
                clearRowDefinitions()
        }

type DataGrid<'TItem> () =
    inherit UserControl()

    let mutable disposables = []
    let remember d = disposables <- d :: disposables
    
    let grid = Grid() 
    #if DEBUG
    //do  grid.ShowGridLines <- true
    #endif
    
    let rowHeaderColumn = ColumnDefinition(Width=GridLength())
    do  grid.ColumnDefinitions.Add rowHeaderColumn
    let columnHeaderRow = RowDefinition(Height=GridLength())
    do  grid.RowDefinitions.Add columnHeaderRow
    do  createSplitter () |> grid.Children.Add

    let columns = ObservableCollection<DataGridColumn<'TItem>>()
    let columnHeaders = List<FrameworkElement>()
    let columnSplitters = List<GridSplitter>()
    let columnLines = List<Rectangle>()
    let rows = ObservableCollection<DataGridRow<'TItem>>()
    let rowHeaders = List<FrameworkElement>()
    let rowLines = List<Rectangle>()
    let rowCells = List<List<FrameworkElement>>()

    let columnsChanger = createColumnsChanger grid columnHeaders columnSplitters (columnLines,rowLines) rows rowCells
    do  relayChanges columns columnsChanger |> remember
    
    let rowsChanger = createRowsChanger grid rowHeaders (columnLines,rowLines) columns rowCells
    do  relayChanges rows rowsChanger |> remember

    let scroll = ScrollViewer(Content=grid)
    do  scroll.HorizontalScrollBarVisibility <- ScrollBarVisibility.Auto
    do  scroll.VerticalScrollBarVisibility <- ScrollBarVisibility.Auto
    do base.Content <- scroll

    member this.Columns = columns
    member this.Rows = rows

    interface IDisposable with
        member this.Dispose() = for d in disposables do d.Dispose()

type DataGridColumn () =
    inherit DataGridColumn<obj>("Header", fun _ -> TextBlock(Text="Cell") :> FrameworkElement)

type DataGridRowEventArgs (row:DataGridRow<_>) =
    inherit EventArgs ()
    member this.Row = row

type DataGrid () as this =
    inherit UserControl()
    let dataGrid = new DataGrid<obj>()
    do  this.Content <- dataGrid
    let columns = ObservableCollection<DataGridColumn>()
    let mutable items : System.Collections.IEnumerable = null
    let columnsChanger =
        { new ICollectionChanger<_> with
            member this.Insert(index,item) =
                dataGrid.Columns.Insert(index,item)
            member this.RemoveAt(index) =
                dataGrid.Columns.RemoveAt(index)
            member this.SetAt(index, item) =
                dataGrid.Columns.[index] <- item
            member this.Clear() =
                dataGrid.Columns.Clear()
        }
    let loadingRow = Event<EventHandler<DataGridRowEventArgs>,DataGridRowEventArgs>()
    let insertRow index item =
        let row = DataGridRow<_>(null, item)
        loadingRow.Trigger(this,DataGridRowEventArgs(row))
        dataGrid.Rows.Insert(index,row)
    let rowsChanger =
        { new ICollectionChanger<_> with
            member this.Insert(index,item) =
                insertRow index item
            member this.RemoveAt(index) =
                dataGrid.Rows.RemoveAt(index)
            member this.SetAt(index, item) =
                let oldRow = dataGrid.Rows.[index]
                let newRow = DataGridRow<_>(oldRow.Header,item)
                dataGrid.Rows.[index] <- newRow
            member this.Clear() =
                dataGrid.Rows.Clear()
        }
    let columnSubscription = relayChanges columns columnsChanger
    let mutable rowSubscription = { new IDisposable with member x.Dispose() = () }
    member this.LoadingRow = loadingRow.Publish
    member this.Columns = columns
    member this.ItemsSource
        with get () = items
        and set value =
            rowSubscription.Dispose()
            items <- value
            items |> Seq.cast |> Seq.iteri insertRow
            match items with
            | :? INotifyCollectionChanged as items ->
                rowSubscription <- relayChanges items rowsChanger
            | _ -> ()