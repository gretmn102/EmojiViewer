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

type IApi =
    {
        insertEmoji : Emoji -> Async<unit>
        getEmojisByTag : TagId -> Async<Emoji list>
        getTagSuggestions : string -> Async<string []>
    }