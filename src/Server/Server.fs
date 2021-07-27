module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

type Cmd =
    | InsertEmoji of Emoji * AsyncReplyChannel<Result<unit, InsertEmojiError>>
    | UpdateEmoji of Emoji * AsyncReplyChannel<Result<unit, unit>>
    | GetEmojisByTag of TagId * AsyncReplyChannel<Emoji []>
    | GetTagSuggestions of pattern:string * AsyncReplyChannel<string []>

let p =
    let exec msg (st:Db.LocalDb) =
        match msg with
        | InsertEmoji (emoji, r) ->
            match Db.insert emoji st with
            | Some(st) ->
                r.Reply (Ok ())

                st
            | None ->
                r.Reply (Error EmojiAlreadyExist)

                st
        | UpdateEmoji (emoji, r) ->
            let db = Db.insertOrUpdate emoji st

            r.Reply (Ok ())

            db
        | GetEmojisByTag (tagId, r) ->
            match Db.findEmojisByTag tagId st with
            | Some(xs, st) ->
                r.Reply (Array.ofSeq xs)

                st
            | None ->
                r.Reply [||]

                st
        | GetTagSuggestions(pattern, r) ->
            let tagIds, st = Db.getTagSuggestions pattern st
            r.Reply tagIds

            st
    MailboxProcessor.Start(fun mail ->
        let rec loop (st:Db.LocalDb) =
            async {
                let! msg = mail.Receive()
                let st =
                    try
                        exec msg st
                    with e ->
                        printfn "%A" e
                        st
                return! loop st
            }
        loop Db.LocalDb.Empty
    )

let api =
    {
        getEmojisByTag = fun tagId ->
            async {
                return p.PostAndReply(fun r -> GetEmojisByTag(tagId, r))
            }
        insertEmoji = fun emoji ->
            async {
                return p.PostAndReply(fun r -> InsertEmoji(emoji, r))
            }
        updateEmoji = fun emoji ->
            async {
                return p.PostAndReply(fun r -> UpdateEmoji(emoji, r))
            }
        getTagSuggestions = fun pattern ->
            async {
                return p.PostAndReply(fun r -> GetTagSuggestions(pattern, r))
            }
    }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8086"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
