module NRKoukan

open System
open System.IO

type Rarity = N | R
    with
        member this.ToStr() =
            match this with
            | N -> "N"
            | R -> "R"

        static member Parse(str: string) =
            match str with
            | "N" -> N
            | "R" -> R
            | _ -> failwith "Unknown rarity"

type IdolType = Cu | Co | Pa
    with
        member this.ToStr() =
            match this with
            | Cu -> "Cu"
            | Co -> "Co"
            | Pa -> "Pa"

        static member Parse(str: string) =
            match str.ToUpper() with
            | "CU" | "CUTE" -> Cu
            | "CO" | "COOL" -> Co
            | "PA" | "PASSION" -> Pa
            | _ -> failwith "Unknown idol type"

type Card = {
    IdolName: string
    IdolType: IdolType
    Rarity: Rarity
}
    with
        member this.ToStr() =
            sprintf "%s %s %s" (this.IdolName) (this.IdolType.ToStr()) (this.Rarity.ToStr())

        static member Parse(str: string) =
            match str.Split(' ') with
            | [| idolName; idolType; rarity; |] ->
                {
                    IdolName = idolName
                    IdolType = IdolType.Parse(idolType)
                    Rarity = Rarity.Parse(rarity)
                }
            | _ -> failwithf "Unkonwn card format: %s" str

let printCards (cards: Set<Card>) =
    cards
    |> Set.iter (fun c -> printfn "%s" (c.ToStr()))

type Db = Db of Set<Card>
    with
        static member private FileName = "idols.txt"
        
        static member Read() =
            if not (File.Exists Db.FileName) then 
                (Db Set.empty)
            else
                let content = File.ReadAllLines Db.FileName
                content
                |> Array.map Card.Parse
                |> Set
                |> Db

        member this.Save() =
            let (Db data) = this
            data
            |> Set.map (fun c -> c.ToStr())
            |> String.concat Environment.NewLine
            |> fun c -> File.WriteAllText(Db.FileName, c)

        member this.Print() =
            let (Db data) = this
            printCards data



type Op =
    | Add of Card
    | SearchByName of string
    | SearchByIdolType of IdolType
    | SearchByRarity of Rarity

let parseOps (cmd: array<string>) =
    match cmd with
    | [| "add"; arg1; arg2; arg3 |] ->
        let cardLine = String.concat " " [| arg1; arg2; arg3 |]
        Add (Card.Parse cardLine)
    | [| "name"; arg1 |] ->
        SearchByName arg1
    | [| "type"; arg1 |] ->
        SearchByIdolType (IdolType.Parse arg1)
    | [| "rarity"; arg1 |] ->
        SearchByRarity (Rarity.Parse arg1)
    | _ ->
        failwith "Unkonwn op"

let add (card: Card) (Db data) =
    let newData = data |> Set.add card
    Db newData

let searchByName (name: string) (Db data) =
    data
    |> Set.filter (fun c -> c.IdolName.Contains(name))

let searchByIdolType (idolType: IdolType) (Db data) =
    data
    |> Set.filter (fun c -> c.IdolType = idolType)

let searchByRarity (rarity: Rarity) (Db data) =
    data
    |> Set.filter (fun c -> c.Rarity = rarity)

[<EntryPoint>]
let main argv =
    let db = Db.Read()
    match parseOps argv with
    | Add card ->
        let newDb = add card db
        newDb.Save()
        newDb.Print()
    | SearchByName name ->
        searchByName name db
        |> printCards
    | SearchByIdolType idolType ->
        searchByIdolType idolType db
        |> printCards
    | SearchByRarity rarity ->
        searchByRarity rarity db
        |> printCards

    0