module Db
open Shared
open FsharpMyExtension

let newDbPath = @"e:\Project\emojiViewer.db" // TODO

let database =
    let mapper = LiteDB.FSharp.FSharpBsonMapper()

    // mapper.RegisterType<System.Uri> (
    //     (fun (uri:System.Uri) ->
    //         printfn "ser"
    //         LiteDB.BsonValue uri.AbsoluteUri),
    //     (fun bson ->
    //         printfn "des"
    //         new System.Uri(bson.AsString)
    //         // |> Set.ofSeq
    //         // if y.IsArray then
    //         //     y.AsArray
    //         // else
    //         //     y.
    //     )
    // )
    let connStr = sprintf "Filename=%s;mode=Exclusive" newDbPath
    new LiteDB.LiteDatabase (connStr, mapper)
// database.Dispose()
// database.DropCollection("Emojis")
// database.DropCollection("Tags")
// database.DropCollection("Temp")
let dbEmojis = database.GetCollection<Emoji> "Emojis"
let dbTags = database.GetCollection<Tag> "Tags"

type LocalDb =
    {
        Emojis: Map<EmojiId, Emoji>
        Tags: Map<TagId, Tag>
    }
    static member Empty =
        { Emojis = Map.empty; Tags = Map.empty }

let removeTags (emoji:Emoji) localdbTags =
    let emojiId = emoji.Id
    emoji.Tags
    |> Set.fold (fun (st:Map<TagId,Tag>) tagId ->
        let f tag =
            let emojiIds = Set.remove emojiId tag.EmojiIds
            if Set.isEmpty emojiIds then
                dbTags.Delete(LiteDB.BsonValue tagId) |> ignore

                Map.remove tagId st
            else
                let tag =
                    { tag with
                        EmojiIds = emojiIds
                    }

                dbTags.Update(tag) |> ignore

                Map.add tagId tag st
        match Map.tryFind tagId st with
        | None ->
            let oldTag = dbTags.FindById (LiteDB.BsonValue tagId)
            if isNull (box oldTag) then
                st
            else
                f oldTag
        | Some tag -> f tag
    ) localdbTags


let remove (emojiId:EmojiId) (db:LocalDb) =
    match Map.tryFind emojiId db.Emojis with
    | Some emoji ->
        {
            Emojis =
                dbEmojis.Delete(LiteDB.BsonValue emojiId) |> ignore
                Map.remove emojiId db.Emojis
            Tags =
                removeTags emoji db.Tags
                // emoji.Tags
                // |> Set.fold (fun (st:Map<TagId,Tag>) tagId ->
                //     let tag = st.[tagId]
                //     let tag =
                //         { tag with
                //             EmojiIds = Set.remove emojiId tag.EmojiIds
                //         }

                //     tags.Update(tag) |> ignore

                //     Map.add tagId tag st
                // ) db.Tags
        }
    | None -> db

let updateTags (emoji:Emoji) localTags =
    emoji.Tags
    |> Set.fold
        (fun (st:Map<TagId,Tag>) tagId ->
            let f tag =
                let tag =
                    { tag with
                        EmojiIds =
                            Set.add emoji.Id tag.EmojiIds
                    }
                dbTags.Update tag |> ignore

                Map.add tagId tag st
            match Map.tryFind tagId st with
            | Some tag -> f tag
            | None ->
                let oldTag = dbTags.FindById (LiteDB.BsonValue tagId)
                if isNull (box oldTag) then
                    let tag =
                        {
                            Id = tagId
                            EmojiIds = Set.singleton emoji.Id
                        }
                    dbTags.Insert tag |> ignore

                    Map.add tagId tag st
                else
                    f oldTag
        )
        localTags

let insertOrUpdate (emoji:Emoji) (db:LocalDb) =
    let f (oldEmoji:Emoji) =
        {
            Emojis =
                dbEmojis.Update(emoji) |> ignore
                Map.add emoji.Id emoji db.Emojis
            Tags =
                let tags =
                    removeTags oldEmoji db.Tags

                updateTags emoji tags
        }
    match Map.tryFind emoji.Id db.Emojis with
    | Some oldEmoji -> f oldEmoji
    | None ->
        let oldEmoji = dbEmojis.FindById (LiteDB.BsonValue emoji.Id)
        if isNull (box oldEmoji) then
            {
                Emojis =
                    dbEmojis.Insert(emoji) |> ignore

                    Map.add emoji.Id emoji db.Emojis
                Tags = updateTags emoji db.Tags
            }
        else
            f oldEmoji

/// Returns None if emoji exists
let insert (emoji:Emoji) (db:LocalDb) =
    match Map.tryFind emoji.Id db.Emojis with
    | None ->
        let oldEmoji = dbEmojis.FindById (LiteDB.BsonValue emoji.Id)
        if isNull (box oldEmoji) then
            {
                Emojis =
                    dbEmojis.Insert(emoji) |> ignore

                    Map.add emoji.Id emoji db.Emojis
                Tags = updateTags emoji db.Tags
            }
            |> Some
        else
            None
    | Some oldEmoji -> None

let test () =
    let localDb =
        { Emojis = Map.empty; Tags = Map.empty }
        |> insertOrUpdate
            {
                Id = "emo1"
                Tags = Set ["cats"; "dogs"]
            }
        |> insertOrUpdate
            {
                Id = "emo2"
                Tags = Set ["cats"]
            }
        |> insertOrUpdate
            {
                Id = "emo3"
                Tags = Set ["dogs"]
            }
        |> insertOrUpdate
            {
                Id = "emo4"
                Tags = Set ["otters"]
            }
        |> insertOrUpdate
            {
                Id = "emo4"
                Tags = Set ["dogs"]
            }
    assert
        localDb.Emojis = (dbEmojis.FindAll() |> Seq.map (fun x -> x.Id, x) |> Map.ofSeq)
    assert
        localDb.Tags = (dbTags.FindAll() |> Seq.map (fun x -> x.Id, x) |> Map.ofSeq)

    let localDb = remove "emo4" localDb
    assert
        localDb.Emojis = (dbEmojis.FindAll() |> Seq.map (fun x -> x.Id, x) |> Map.ofSeq)
    assert
        localDb.Tags = (dbTags.FindAll() |> Seq.map (fun x -> x.Id, x) |> Map.ofSeq)

let findEmojisByTag (tagId:TagId) (localDb:LocalDb) =
    let tag = dbTags.FindById(LiteDB.BsonValue tagId)
    if isNull (box tag) then
        None
    else
        tag.EmojiIds
        |> Seq.mapFold
            (fun (localDb:LocalDb) emojiId ->
                match Map.tryFind emojiId localDb.Emojis with
                | Some emoji ->
                    emoji, localDb
                | None ->
                    let emoji =
                        dbEmojis.FindById(LiteDB.BsonValue emojiId)
                    let localDb =
                        { localDb with
                            Emojis = Map.add emojiId emoji localDb.Emojis
                        }
                    emoji, localDb
            )
            { localDb with
                Tags = Map.add tagId tag localDb.Tags
            }
        |> Some
// open LiteDB
// open LiteDB.FSharp
// open LiteDB.FSharp.Extensions

// "sdf".Contains "sdF"

// let query1 =
//     Query.createQueryFromExpr <@ fun (tag:Tag) -> tag.Id.Contains "sdf" @>
let getTagSuggestions (pattern:string) (localDb:LocalDb) =
    // https://github.com/Zaid-Ajaj/LiteDB.FSharp/blob/b63ddf8eda4363f93f1e018c26435f6058128676/LiteDB.FSharp/Query.fs
    let xs =
        dbTags.Find(
            // LiteDB.Query.Contains("_id", "cat") // https://github.com/mbdavid/LiteDB/blob/5ec64e5bd2453e506bd135ace1b1c0ab5717d3f5/LiteDB/Client/Mapper/Linq/TypeResolver/StringResolver.cs#L32

            LiteDB.Query.Where("_id", fun x -> x.AsString.Contains(pattern, System.StringComparison.CurrentCultureIgnoreCase)),
            limit = 10
        )
    let xs, tags =
        xs
        |> Seq.mapFold
            (fun st x -> x.Id, Map.add x.Id x st)
            localDb.Tags
    let localDb =
        { localDb with
            Tags = tags
        }
    Array.ofSeq xs, localDb

let getTagSuggestionsTest () =
    getTagSuggestions "cat" LocalDb.Empty
    let pattern = "cat"
    let x () =
        dbTags.Find(
            LiteDB.Query.Contains("_id", "CaT"), // https://github.com/mbdavid/LiteDB/blob/5ec64e5bd2453e506bd135ace1b1c0ab5717d3f5/LiteDB/Client/Mapper/Linq/TypeResolver/StringResolver.cs#L32

            // LiteDB.Query.Where("_id", fun x -> x.AsString.Contains("cat", System.StringComparison.CurrentCultureIgnoreCase)),
            limit = 10
        )
    dbTags.FindAll()
// LiteDB.BsonMapper.Global.RegisterType(
// // mapper.RegisterType(
//     (fun (x:string Set) -> Set.toList x |> LiteDB.BsonValue),
//     (fun y ->
//         y.AsArray
//         |> Seq.fold (fun st x -> Set.add x.AsString st)
//             Set.empty
//         // |> Set.ofSeq
//         // if y.IsArray then
//         //     y.AsArray
//         // else
//         //     y.
//     ))
// LiteDB.BsonMapper.Global.RegisterType(
// LiteDB.FSharp.FSharpBsonMapper.Global.RegisterType(
//     (fun (uri:System.Uri) -> LiteDB.BsonValue uri.AbsoluteUri),
//     (fun bson ->
//         new System.Uri(bson.AsString)
//         // |> Set.ofSeq
//         // if y.IsArray then
//         //     y.AsArray
//         // else
//         //     y.
//     ))
// LiteDB.FSharp.FSharpBsonMapper.Global.SerializeNullValues <- true
// LiteDB.FSharp.FSharpBsonMapper.RegisterInheritedConverterType()
// open Newtonsoft.Json
// /// Взято [отсюда](https://stackoverflow.com/a/29629215)
// type SetConverter() =
//     inherit JsonConverter<string Set>()
//     // inherit JsonConverter()
//     // override x.CanConvert(t) =
//     //     t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option>

//     override x.WriteJson((writer:JsonWriter), (value:string Set), (serializer:JsonSerializer)) =
//         // value
//         printfn "write"

//         serializer.Serialize(writer, Set.toArray value)
//         // let value =
//         //     if isNull value then null
//         //     else
//         //         let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
//         //         fields.[0]
//         // serializer.Serialize(writer, value)
//         // failwith ""

//     override x.ReadJson(reader:JsonReader, t:System.Type, existingValue, hasExistingValue, serializer) =
//         // JsonReader * System.Type * Set<string> * bool * JsonSerializer
//         printfn "ReadJson err"
//         failwith "ReadJson err"
//         // let innerType = t.GetGenericArguments().[0]
//         // let innerType =
//         //     if innerType.IsValueType then (typedefof<Nullable>).MakeGenericType([|innerType|])
//         //     else innerType
//         // let value = serializer.Deserialize(reader, innerType)
//         // let cases = FSharpType.GetUnionCases(t)
//         // if isNull value then FSharpValue.MakeUnion(cases.[0], [||])
//         // else FSharpValue.MakeUnion(cases.[1], [|value|])

// LiteDB.FSharp.FSharpBsonMapper.UseCustomJsonConverters [|
//     // new SetConverter()
// |]

type T =
    {
        Id: int
        Uri: System.Uri
    }
let temp = database.GetCollection<T> "Temp"
// temp.FindAll()
let test3 () =
    let x =
        {
            Id = 0
            Uri = System.Uri "https://github.com/mbdavid/LiteDB/wiki/Object-Mapping"
        }
    temp.Insert x
    |> ignore
// #if INTERACTIVE
// // #r @"C:\Users\User\.nuget\packages\litedb.fsharp\2.16.0\lib\netstandard2.0\LiteDB.FSharp.dll"
// #r @"e:\Project\LiteDB.FSharp\LiteDB.FSharp\bin\Release\netstandard2.0\LiteDB.FSharp.dll"
// #r @"C:\Users\User\.nuget\packages\litedb\4.1.4\lib\netstandard2.0\LiteDB.dll"

// #load @"..\Shared\Shared.fs"
// #load @"Db.fs"
// #endif
// open Db

let test2 () =
    dbEmojis.Insert
        {
            Id = "https://gretmn102.github.io/public/index.html#/GenerateWords?sdf&sdf=34y4"
            Tags =
                // System.Collections.Generic.HashSet<string>(["cats"; "dogs"])
                // h.Add 1
                // h.Add 2
                // h.Contains 4

                Set ["dogs";"cats"; ]
        }
    // "https://gretmn102.github.io/public/index.html#/GenerateWords"
    dbEmojis.FindById (LiteDB.BsonValue "https://gretmn102.github.io/public/index.html#/GenerateWords?sdf&sdf=34; df")
    dbEmojis.FindAll()
