module Index

open Elmish
open Fable.Remoting.Client
open Shared

open Fulma.Extensions
open Fulma.Extensions.Types

let api =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IApi>

module InputTags =
    let update msg state =
        InputTags.update api.getTagSuggestions msg state

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let spinner =
    div [ Class ("block " + Fa.Classes.Size.Fa3x) ] [
        Fa.i [ Fa.Solid.Spinner; Fa.Spin ] []
    ]

module EmojiUploadForm =
    type State =
        {
            Url: string
            Tags: InputTags.State
            SubmiteResult: unit Deferred
        }

    let init () =
        let state =
            {
                Url = ""
                Tags = InputTags.init()
                SubmiteResult = HasNotStartedYet
            }
        state

    type Msg =
        | InputTagsMsg of InputTags.Msg
        | UpdateUrl of string
        | Submit
        | SubmitResult of unit
    let update (msg: Msg) (state: State) =
        match msg with
        | UpdateUrl url ->
            let state =
                { state with
                    Url = url
                }
            state, Cmd.none

        | InputTagsMsg msg ->
            let inputTagsState, cmd =
                InputTags.update msg state.Tags
            let state =
                { state with
                    Tags = inputTagsState
                }
            let cmd = Cmd.map InputTagsMsg cmd
            state, cmd

        | Submit ->
            let state =
                { state with
                    SubmiteResult = InProgress
                }
            let emoji =
                {
                    Id = state.Url
                    Tags = state.Tags.InputTagsState.Tags
                }

            let cmd =
                Cmd.OfAsync.perform api.insertEmoji emoji SubmitResult
            state, cmd
        | SubmitResult res ->
            let state =
                { state with
                    SubmiteResult = Resolved res
                }
            state, Cmd.none

    open Fable.React
    open Fable.React.Props
    open Fulma
    open Fable.FontAwesome

    let view (state : State) (dispatch : Msg -> unit) =
        Box.box' [] [
            match state.SubmiteResult with
            | HasNotStartedYet | Resolved(_) ->
                Control.p [ Control.IsExpanded ] [
                    Input.text [
                      Input.Value state.Url
                      Input.Placeholder "Url"
                      Input.OnChange (fun x -> UpdateUrl x.Value |> dispatch) ]
                ]

                InputTags.view state.Tags (InputTagsMsg >> dispatch)

                Button.button [
                    let isDisabled = System.String.IsNullOrWhiteSpace state.Url
                    Button.Disabled isDisabled
                    Button.OnClick (fun _ ->
                        if not isDisabled then
                            dispatch Submit
                    )
                ] [
                    str "Submit"
                ]

            | InProgress -> spinner
        ]


type State =
    {
        EmojiUploadForm: EmojiUploadForm.State
        FindEmojisByTags: InputTags.State
        EmojisResult: Deferred<Emoji list>
    }

type Msg =
    | EmojiUploadFormMsg of EmojiUploadForm.Msg
    | InputTagsMsg of InputTags.Msg
    | Find
    | FindResult of Emoji list

let init(): State * Cmd<Msg> =
    let model =
        {
            EmojiUploadForm = EmojiUploadForm.init ()
            FindEmojisByTags = InputTags.init ()
            EmojisResult = HasNotStartedYet
        }
    // let cmd = Cmd.OfAsync.perform api.getTodos () GotTodos
    model, Cmd.none

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | EmojiUploadFormMsg msg ->
        let emojiUploadFormState, cmd =
            EmojiUploadForm.update msg state.EmojiUploadForm

        let state =
            { state with
                EmojiUploadForm = emojiUploadFormState
            }
        state, Cmd.map EmojiUploadFormMsg cmd
    | InputTagsMsg msg ->
        let inputTagsState, cmd =
            InputTags.update msg state.FindEmojisByTags
        let state =
            { state with
                FindEmojisByTags = inputTagsState
            }
        let cmd = Cmd.map InputTagsMsg cmd
        state, cmd

    | Find ->
        match state.FindEmojisByTags.InputTagsState.Tags with
        | tag::tags -> // TODO
            let cmd =
                Cmd.OfAsync.perform api.getEmojisByTag tag FindResult
            let state =
                { state with
                    EmojisResult = InProgress }
            state, cmd
        | [] ->
            state, Cmd.none
    | FindResult emojis ->
        let state =
            { state with
                EmojisResult = Resolved emojis }
        state, Cmd.none


open Fable.React
open Fable.React.Props
open Fulma

let navBrand =
    Navbar.Brand.div [] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
    ]

let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [] [
        EmojiUploadForm.view state.EmojiUploadForm (EmojiUploadFormMsg >> dispatch)

        Box.box' [] [
            str "Find by tags:"
            InputTags.view state.FindEmojisByTags (InputTagsMsg >> dispatch)

            Button.button [
                let isLoading =
                    state.EmojisResult = InProgress
                Button.IsLoading isLoading
                let isDisabled =
                    List.isEmpty state.FindEmojisByTags.InputTagsState.Tags
                    || isLoading
                Button.Disabled isDisabled
                Button.OnClick (fun _ ->
                    if not isDisabled then
                        dispatch Find
                )
            ] [
                str "Find"
            ]

            match state.EmojisResult with
            | HasNotStartedYet -> ()
            | Resolved emojis ->
                Content.content [] [
                    Content.Ol.ol [] [
                        for emoji in emojis do
                            li [] [ str (sprintf "%A" emoji) ]
                    ]
                ]
            | InProgress -> spinner
        ]
    ]

let view (model : State) (dispatch : Msg -> unit) =
    Hero.hero [
        // Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                // Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                // BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.head [] [
            Navbar.navbar [] [
                Container.container [] [ navBrand ]
            ]
        ]

        Hero.body [] [
            Container.container [] [
                Column.column [
                    Column.Width (Screen.All, Column.Is6)
                    Column.Offset (Screen.All, Column.Is3)
                ] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "SAFE" ]
                    containerBox model dispatch
                ]
            ]
        ]
    ]
