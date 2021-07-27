namespace Shared


type EmojiId = string
type TagId = string

type Emoji =
    {
        Id: EmojiId
        // Url: string
        #if FABLE_COMPILER
        Tags: TagId list
        #else
        Tags: TagId Set
        #endif
    }

type Tag =
    {
        Id: TagId
        EmojiIds: EmojiId Set
    }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type InsertEmojiError =
    | EmojiAlreadyExist

type IApi =
    {
        insertEmoji : Emoji -> Async<Result<unit, InsertEmojiError>>
        updateEmoji : Emoji -> Async<Result<unit, unit>>
        getEmojisByTag : TagId -> Async<Emoji []>
        getTagSuggestions : string -> Async<string []>
    }