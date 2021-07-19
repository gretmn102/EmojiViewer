module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

type Cmd =
    | InsertEmoji of Emoji
    | GetEmojisByTag of TagId * AsyncReplyChannel<Emoji list>
    | GetTagSuggestions of pattern:string * AsyncReplyChannel<string []>

let p =
    MailboxProcessor.Start(fun mail ->
        let rec loop (st:Db.LocalDb) =
            async {
                let! msg = mail.Receive()
                let st =
                    match msg with
                    | InsertEmoji emoji ->
                        Db.insertOrUpdate emoji st
                    | GetEmojisByTag (tagId, r) ->
                        match Db.findEmojisByTag tagId st with
                        | Some(xs, st) ->
                            r.Reply (List.ofSeq xs)

                            st
                        | None -> st
                    | GetTagSuggestions(pattern, r) ->
                        let tagIds, st = Db.getTagSuggestions pattern st
                        r.Reply tagIds

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
                p.Post(InsertEmoji emoji)
                return ()
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
