//     http://fsharp.org
//     'New Project' --> 'Online Templates'
//     http://go.microsoft.com/fwlink/?LinkID=234174 (F# Development Portal)
//     http://go.microsoft.com/fwlink/?LinkID=124614 (Code Gallery)
//     http://go.microsoft.com/fwlink/?LinkId=235173 (Math/Stats Programming)
//     http://go.microsoft.com/fwlink/?LinkId=235176 (Charting)

module Integers = 
    let sampleTableOfSquares = [ for i in 0 .. 99 -> (i, i*i) ]
module BasicFunctions = 
    let func2 (x:int) = 2*x*x - x/5 + 3
    let func3 x = 
        if x < 100.0 then 
            2.0*x*x - x/5.0 + 3.0
        else 
            2.0*x*x + x/5.0 - 37.0
    let result3 = func3 (6.5 + 4.5)
    printfn "The result of applying the 2nd sample function to (6.5 + 4.5) is %f" result3
module StringManipulation = 
    let string1 = "Hello"
    let string2  = "world"
    let string3 = @"c:\Program Files\"
    let string4 = """He said "hello world" after you did"""
    let helloWorld = string1 + " " + string2 
    printfn "%s" helloWorld
    /// A string formed by taking the first 7 characters of one of the result strings
    let substring = helloWorld.[0..6]
module Tuples = 
    let tuple1 = (1, 2, 3)
    let swapElems (a, b) = (b, a)
    printfn "The result of swapping (1, 2) is %A" (swapElems (1,2))
    let tuple2 = (1, "fred", 3.1415)
module Lists = 
    /// A list containing all the days of the year
    let daysList = 
        [ for month in 1 .. 12 do
              for day in 1 .. System.DateTime.DaysInMonth(2012, month) do 
                  yield System.DateTime(2012, month, day) ]
    /// A list containing the tuples which are the coordinates of the black squares on a chess board.
    let blackSquares = 
        [ for i in 0 .. 7 do
              for j in 0 .. 7 do 
                  if (i+j) % 2 = 1 then 
                      yield (i, j) ]
    let numberList = [ 1 .. 1000 ]  
    /// Computes the sum of the squares of the numbers divisible by 3.
    let sumOfSquares = 
        numberList
        |> List.filter (fun x -> x % 3 = 0)
        |> List.sumBy (fun x -> x * x)
module DefiningClasses = 
    /// The class's constructor takes two arguments: dx and dy, both of type 'float'. 
    type Vector2D(dx : float, dy : float) = 
        /// The length of the vector, computed when the object is constructed
        let length = sqrt (dx*dx + dy*dy)
        member this.DX = dx  
        member this.DY = dy
        member this.Length = length
        member this.Scale(k) = Vector2D(k * this.DX, k * this.DY)
    let vector1 = Vector2D(3.0, 4.0)
    let vector2 = vector1.Scale(10.0)
module DefiningGenericClasses = 
    type StateTracker<'T>(initialElement: 'T) = 
        let mutable states = [ initialElement ]
        member this.UpdateState newState = 
            states <- newState :: states  
        member this.History = states
        member this.Current = states.Head

    let tracker = StateTracker 10
    tracker.UpdateState 17
/// Type that implements IDisposable
type ReadFile() =
    let file = new System.IO.StreamReader("readme.txt")
    member this.ReadLine() = file.ReadLine()
    interface System.IDisposable with    
        member this.Dispose() = file.Close()
module Arrays = 
    let array1 = [| |]
    let array2 = [| "hello"; "world"; "and"; "hello"; "world"; "again" |]
    let array3 = [| 1 .. 1000 |]
    let array4 = [| for word in array2 do
                        if word.Contains("l") then 
                            yield word |]
    let evenNumbers = Array.init 1001 (fun n -> n * 2) 
    let evenNumbersSlice = evenNumbers.[0..500]
    for word in array4 do 
        printfn "word: %s" word
    array2.[1] <- "WORLD!"
    let sumOfLengthsOfWords = 
        array2
        |> Array.filter (fun x -> x.StartsWith "h")
        |> Array.sumBy (fun x -> x.Length)
module Sequences = 
    // Sequences are evaluated on-demand and are re-evaluated each time they are iterated. 
    // An F# sequence is an instance of a System.Collections.Generic.IEnumerable<'T>,
    // so Seq functions can be applied to Lists and Arrays as well.
    let seq1 = Seq.empty
    let seq2 = seq { yield "hello"; yield "world"; yield "and"; yield "hello"; yield "world"; yield "again" }
    let numbersSeq = seq { 1 .. 1000 }
    /// another array containing only the words "hello" and "world"
    let seq3 = 
        seq { for word in seq2 do
                  if word.Contains("l") then 
                      yield word }
    let rnd = System.Random()
    /// An infinite sequence which is a random walk
    //  Use yield! to return each element of a subsequence, similar to IEnumerable.SelectMany.
    let rec randomWalk x =
        seq { yield x 
              yield! randomWalk (x + rnd.NextDouble() - 0.5) }
    let first100ValuesOfRandomWalk = 
        randomWalk 5.0 
        |> Seq.truncate 100
        |> Seq.toList
module RecursiveFunctions  = 
    let rec factorial n = 
        if n = 0 then 1 else n * factorial (n-1)

    let rec factorialMatch n = 
        match n with
        | 0 -> 1
        | _ -> n * factorialMatch (n-1)

    /// Computes the greatest common factor of two integers. 
    //  Since all of the recursive calls are tail calls, the compiler will turn the function into a loop,
    //  which improves performance and reduces memory consumption.
    let rec greatestCommonFactor a b =                       
        if a = 0 then b
        elif a < b then greatestCommonFactor a (b - a)           
        else greatestCommonFactor (a - b) b
    /// Computes the sum of a list of integers using recursion.
    let rec sumList xs =
        match xs with
        | []    -> 0
        | y::ys -> y + sumList ys

    /// Make the function tail recursive, using a helper function with a result accumulator
    let rec private sumListTailRecHelper accumulator xs =
        match xs with
        | []    -> accumulator
        | y::ys -> sumListTailRecHelper (accumulator+y) ys

    let sumListTailRecursive xs = sumListTailRecHelper 0 xs
module RecordTypes = 
    type ContactCard = 
        { Name     : string;
          Phone    : string;
          Verified : bool }
    let contact1 = { Name = "Alf" ; Phone = "(206) 555-0157" ; Verified = false }
    let contact2 = { contact1 with Phone = "(206) 555-0112"; Verified = true }
    let showCard c = 
        c.Name + " Phone: " + c.Phone + (if not c.Verified then " (unverified)" else "")
module UnionTypes = 
    type Suit = 
        | Hearts 
        | Clubs 
        | Diamonds 
        | Spades
    type Rank = 
        | Value of int
        | Ace
        | King
        | Queen
        | Jack
        static member GetAllRanks() = 
            [ yield Ace
              for i in 2 .. 10 do yield Value i
              yield Jack
              yield Queen
              yield King ]
                                   
    type Card =  { Suit: Suit; Rank: Rank }
    let fullDeck = 
        [ for suit in [ Hearts; Diamonds; Clubs; Spades] do
              for rank in Rank.GetAllRanks() do 
                  yield { Suit=suit; Rank=rank } ]
    let showCard c = 
        let rankString = 
            match c.Rank with 
            | Ace -> "Ace"
            | King -> "King"
            | Queen -> "Queen"
            | Jack -> "Jack"
            | Value n -> string n
        let suitString = 
            match c.Suit with 
            | Clubs -> "clubs"
            | Diamonds -> "diamonds"
            | Spades -> "spades"
            | Hearts -> "hearts"
        rankString  + " of " + suitString
    let printAllCards() = 
        for card in fullDeck do 
            printfn "%s" (showCard card)

module OptionTypes = 
    /// Option values are any kind of value tagged with either 'Some' or 'None'.
    /// They are used extensively in F# code to represent the cases where many other
    /// languages would use null references.
    type Customer = { zipCode : decimal option }
    /// Abstract class that computes the shipping zone for the customer's zip code, 
    /// given implementations for the 'getState' and 'getShippingZone' abstract methods.
    [<AbstractClass>]
    type ShippingCalculator =
        abstract getState : decimal -> string option
        abstract getShippingZone : string -> int
        /// Return the shipping zone corresponding to the customer's ZIP code
        /// Customer may not yet have a ZIP code or the ZIP code may be invalid
        member this.customerShippingZone(customer : Customer) =
            customer.zipCode |> Option.bind this.getState |> Option.map this.getShippingZone
module PatternMatching = 
    type Person = {     
        First : string
        Last  : string
    }
    /// define a discriminated union of 3 different kinds of employees
    type Employee = 
        | Engineer  of Person
        | Manager   of Person * list<Employee>            // manager has list of reports
        | Executive of Person * list<Employee> * Employee // executive also has an assistant
    /// count everyone underneath the employee in the management hierarchy, including the employee
    let rec countReports(emp : Employee) = 
        1 + match emp with
            | Engineer(id) -> 
                0
            | Manager(id, reports) -> 
                reports |> List.sumBy countReports 
            | Executive(id, reports, assistant) ->
                (reports |> List.sumBy countReports) + countReports assistant
    /// find all managers/executives named "Dave" who do not have any reports
    let rec findDaveWithOpenPosition(emps : Employee list) =
        emps 
        |> List.filter(function 
                       | Manager({First = "Dave"}, []) -> true       // [] matches the empty list
                       | Executive({First = "Dave"}, [], _) -> true
                       | _ -> false)                                 // '_' is a wildcard pattern that matches anything
                                                                     // this handles the "or else" case
module UnitsOfMeasure = 
    // Code can be annotated with units of measure when using F# arithmetic over numeric types
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
    [<Measure>]
    type mile =
        /// Conversion factor mile to meter: meter is defined in SI.UnitNames
        static member asMeter = 1600.<meter/mile>
    let d  = 50.<mile>          // Distance expressed using imperial units
    let d2 = d * mile.asMeter   // Same distance expressed using metric system
    printfn "%A = %A" d d2
    // let error = d + d2       // Compile error: units of measure do not match
module ParallelArrayProgramming = 
    let oneBigArray = [| 0 .. 100000 |]
    // do some CPU intensive computation 
    let rec computeSomeFunction x = 
        if x <= 2 then 1 
        else computeSomeFunction (x - 1) + computeSomeFunction (x - 2)
    // Do a parallel map over a large input array
    let computeResults() = oneBigArray |> Array.Parallel.map (fun x -> computeSomeFunction (x % 20))
    printfn "Parallel computation results: %A" (computeResults())
module Events = 
    open System
    // create instance of Event object that consists of subscription point (event.Publish) and event trigger (event.Trigger)
    let simpleEvent = new Event<int>() 
    // add handler
    simpleEvent.Publish.Add(
        fun x -> printfn "this is handler was added with Publish.Add: %d" x)
    // trigger event
    simpleEvent.Trigger(5)
    // create instance of Event that follows standard .NET convention: (sender, EventArgs)
    let eventForDelegateType = new Event<EventHandler, EventArgs>()    
    // add handler
    eventForDelegateType.Publish.AddHandler(
        EventHandler(fun _ _ -> printfn "this is handler was added with Publish.AddHandler"))
    // trigger event (note that sender argument should be set)
    eventForDelegateType.Trigger(null, EventArgs.Empty)
module DatabaseAccess = 
    // The easiest way to access a SQL database from F# is to use F# type providers. 
    // Add references to System.Data, System.Data.Linq, and FSharp.Data.TypeProviders.dll.
    // You can use Server Explorer to build your ConnectionString. 
    (*
    #r "System.Data"
    #r "System.Data.Linq"
    #r "FSharp.Data.TypeProviders"
    open Microsoft.FSharp.Data.TypeProviders
    type SqlConnection = SqlDataConnection<ConnectionString = @"Data Source=.\sqlexpress;Initial Catalog=tempdb;Integrated Security=True">
    let db = SqlConnection.GetDataContext()
    let table = 
        query { for r in db.Table do
                select r }
    *)
    // You can also use SqlEntityConnection instead of SqlDataConnection, which accesses the database using Entity Framework.
    ()
module OData = 

    (*
    open System.Data.Services.Client
    open Microsoft.FSharp.Data.TypeProviders
    // Consume demographics population and income OData service from Azure Marketplace. 
    // For more information, see http://go.microsoft.com/fwlink/?LinkId=239712
    type Demographics = Microsoft.FSharp.Data.TypeProviders.ODataService<ServiceUri = "https://api.datamarket.azure.com/Esri/KeyUSDemographicsTrial/">
    let ctx = Demographics.GetDataContext()
    // Sign up for a Azure Marketplace account at https://datamarket.azure.com/account/info
    ctx.Credentials <- System.Net.NetworkCredential ("<your liveID>", "<your Azure Marketplace Key>")
    let cities = query {
        for c in ctx.demog1 do
        where (c.StateName = "Washington")
        } 
    for c in cities do
        printfn "%A - %A" c.GeographyId c.PerCapitaIncome2010.Value
    *)
    ()
#if COMPILED
module BoilerPlateForForm = 
    [<System.STAThread>]
    do ()
    do System.Windows.Forms.Application.Run()
#endif
