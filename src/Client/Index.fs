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
    open Fulma.Extensions.InputTags
    let init initTags =
        let state =
            {
                InputTagsState =
                    { View.State.Empty with
                        Tags = initTags }
                TagsSuggestions = HasNotStartedYet
            }
        state
    let update msg state =
        InputTags.update api.getTagSuggestions msg state

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Feliz.Router

let spinner =
    div [ Class ("block " + Fa.Classes.Size.Fa3x) ] [
        Fa.i [ Fa.Solid.Spinner; Fa.Spin ] []
    ]

module EmojiUploadForm =
    type State =
        {
            Url: string
            Tags: InputTags.State
            SubmitResult: Result<unit,InsertEmojiError> Deferred
            EmojiUploadFormVisible: bool
        }

    let init () =
        let state =
            {
                Url = ""
                Tags = InputTags.init []
                SubmitResult = HasNotStartedYet
                EmojiUploadFormVisible = false
            }
        state

    type Msg =
        | InputTagsMsg of InputTags.Msg
        | UpdateUrl of string
        | Submit
        | SubmitResult of Result<unit,InsertEmojiError>
        | FormVisible of bool

    let update (msg: Msg) (state: State) =
        match msg with
        | UpdateUrl url ->
            let state =
                { state with
                    Url = url
                    SubmitResult = HasNotStartedYet
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
                    SubmitResult = InProgress
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
                    SubmitResult = Resolved res
                }
            state, Cmd.none
        | FormVisible visible ->
            let state =
                { state with
                    EmojiUploadFormVisible = visible
                }
            state, Cmd.none

    open Fable.React
    open Fable.React.Props
    open Fulma
    open Fable.FontAwesome

    let view (state : State) (dispatch : Msg -> unit) =
        let close _ =
            dispatch (FormVisible false)

        Modal.modal [
            Modal.IsActive state.EmojiUploadFormVisible
        ] [
            Modal.background [
                Props [
                    OnClick close
                ]
            ] []
            Modal.Card.card [
            ] [
                Modal.Card.head [] [
                    Modal.Card.title [] [ str "Add emoji"]

                    Delete.delete [
                        Delete.OnClick close
                    ] []
                ]
                Modal.Card.body [
                ] [
                    match state.SubmitResult with
                    | HasNotStartedYet ->
                        Control.p [ Control.IsExpanded ] [
                            Input.text [
                              Input.Value state.Url
                              Input.Placeholder "Url"
                              Input.OnChange (fun x -> UpdateUrl x.Value |> dispatch) ]
                        ]

                        InputTags.view state.Tags (InputTagsMsg >> dispatch)

                    | Resolved err ->
                        Control.p [ Control.IsExpanded ] [
                            Input.text [
                              Input.Value state.Url
                              Input.Placeholder "Url"
                              Input.OnChange (fun x -> UpdateUrl x.Value |> dispatch) ]
                        ]

                        InputTags.view state.Tags (InputTagsMsg >> dispatch)
                    | InProgress -> spinner
                ]
                Modal.Card.foot [] [
                    match state.SubmitResult with
                    | HasNotStartedYet ->
                        Button.button [
                            let isDisabled =
                                System.String.IsNullOrWhiteSpace state.Url
                                || List.isEmpty state.Tags.InputTagsState.Tags
                            Button.Disabled isDisabled
                            Button.OnClick (fun _ ->
                                if not isDisabled then
                                    dispatch Submit
                            )
                        ] [
                            str "Submit"
                        ]

                        Button.button [
                            Button.OnClick close
                        ] [
                            str "Close"
                        ]
                    | Resolved err ->
                        Button.button [
                            Button.Color
                                (match err with
                                | Ok _ ->
                                    Color.IsSuccess
                                | Error _ ->
                                    Color.IsDanger)

                            let isDisabled = true
                            Button.Disabled isDisabled
                            Button.OnClick (fun _ ->
                                if not isDisabled then
                                    dispatch Submit
                            )
                        ] [
                            match err with
                            | Ok _ ->
                                str "Done"
                            | Error x ->
                                str (sprintf "%A" x)
                        ]

                        Button.button [
                            Button.OnClick close
                        ] [
                            str "Close"
                        ]
                    | InProgress -> spinner
                ]
            ]
        ]


[<Literal>]
let TagRoute = "tag"

module EmojiTagsEdit =
    type EmojiTagsEditState =
        | View
        | Edit of {| Tags: InputTags.State; Submit: Deferred<Result<unit, unit>> |}

    type EmojiState =
        {
            Emoji: Emoji
            EmojiTagsEditState: EmojiTagsEditState
        }
    let init emoji =
        {
            Emoji = emoji
            EmojiTagsEditState = View
        }
    type Msg =
        | ChangeToEditMode
        | Submit
        | SubmitResult of Result<Emoji, unit>
        | InputTagsMsg of InputTags.Msg

    let update (msg: Msg) (state: EmojiState) =
        match msg with
        | ChangeToEditMode ->
            let state =
                { state with
                    EmojiTagsEditState =
                        {|
                            Tags = InputTags.init state.Emoji.Tags
                            Submit = HasNotStartedYet |}
                        |> Edit
                }
            state, Cmd.none
        | Submit ->
            let emoji = state
            match emoji.EmojiTagsEditState with
            | Edit edit ->
                let em =
                    { emoji.Emoji with
                        Tags = edit.Tags.InputTagsState.Tags
                    }
                let cmd =
                    Cmd.OfAsync.perform
                        api.updateEmoji
                        em
                        (function
                         | Ok () -> SubmitResult (Ok em)
                         | Error e -> SubmitResult (Error e)
                        )
                let state =
                    { emoji with
                        EmojiTagsEditState =
                            {| edit with
                                Submit = InProgress
                            |}
                            |> Edit
                    }
                state, cmd
            | View ->
                state, Cmd.none
        | SubmitResult(res) ->
            match res with
            | Ok emoji ->
                let state =
                    {
                        Emoji = emoji
                        EmojiTagsEditState = View
                    }
                state, Cmd.none
            | Error err ->
                let emoji = state
                match emoji.EmojiTagsEditState with
                | Edit edit ->
                    let state =
                        { emoji with
                            EmojiTagsEditState =
                                Edit {| edit with Submit = Resolved (Error err) |}
                        }

                    state, Cmd.none
                | View ->
                    state, Cmd.none
        | InputTagsMsg( msg) ->
            let emoji = state
            match emoji.EmojiTagsEditState with
            | Edit edit ->
                let inputTagsState, cmd =
                    InputTags.update msg edit.Tags
                let state =
                    { emoji with
                        EmojiTagsEditState =
                            Edit {| edit with Tags = inputTagsState |}
                    }
                let cmd = Cmd.map (fun msg -> InputTagsMsg msg) cmd
                state, cmd
            | View ->
                state, Cmd.none

    let view (emoji:EmojiState) dispatch =
        let emoji' = emoji.Emoji
        match emoji.EmojiTagsEditState with
        | View ->
            Tag.list [] [
                for x in emoji'.Tags do
                    Tag.tag [
                    ] [
                        a [Href (Router.format [TagRoute; Router.encodeURIComponent x])] [str x]
                    ]

                Button.button [
                    Button.OnClick (fun _ ->
                        dispatch ChangeToEditMode
                    )
                ] [
                    Fa.i [ Fa.Solid.Edit ] []
                ]
            ]

        | Edit edit ->
            match edit.Submit with
            | HasNotStartedYet | Resolved _ ->
                div [] [
                    InputTags.view edit.Tags (fun msg ->
                        InputTagsMsg msg
                        |> dispatch)

                    Button.button [
                        let isDisabled =
                            match edit.Submit with
                            | Resolved(Error x) -> true
                            | _ -> false
                        if isDisabled then
                            Button.Color Color.IsDanger
                        Button.Disabled isDisabled
                        Button.OnClick (fun _ ->
                            if not isDisabled then
                                dispatch Submit
                        )
                    ] [
                        match edit.Submit with
                        | Resolved(Error errorMsg) ->
                            str (sprintf "%A" errorMsg)
                        | _ ->
                            Fa.i [ Fa.Solid.Check ] []
                    ]

                ]
            | InProgress -> spinner

type State =
    {
        EmojiUploadForm: EmojiUploadForm.State
        FindEmojisByTags: InputTags.State
        EmojisResult: Deferred<EmojiTagsEdit.EmojiState []>
    }

type Msg =
    | EmojiUploadFormMsg of EmojiUploadForm.Msg

    | InputTagsMsg of InputTags.Msg

    | Find
    | FindResult of Emoji []

    | ChangeUrl of string list
    | EmojiTagsEditMsg of int * EmojiTagsEdit.Msg

let parseUrl state segments =
    match segments with
    | [] ->
        state, Cmd.none
    | TagRoute::tag::_ ->
        let tag = Router.decodeURIComponent tag
        let cmd =
            Cmd.OfAsync.perform api.getEmojisByTag tag FindResult

        let state =
            { state with
                EmojisResult = InProgress
                FindEmojisByTags = InputTags.init [tag]
            }
        state, cmd
    | _ ->
        state, Cmd.none

let init(): State * Cmd<Msg> =
    let state =
        {
            EmojiUploadForm = EmojiUploadForm.init ()
            FindEmojisByTags = InputTags.init []
            EmojisResult = HasNotStartedYet
        }
    Router.currentUrl()
    |> parseUrl state

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
        | [tag] ->
            state, Feliz.Router.Cmd.navigate [|TagRoute; Router.encodeURIComponent tag|]
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
                EmojisResult =
                    emojis
                    |> Array.map EmojiTagsEdit.init
                    |> Resolved }
        state, Cmd.none

    | ChangeUrl segments ->
        parseUrl state segments

    | EmojiTagsEditMsg(idx, msg) ->
        match state.EmojisResult with
        | Resolved emojis ->
            let emojiState, cmd =
                EmojiTagsEdit.update msg emojis.[idx]
            let state =
                { state with
                    EmojisResult =
                        emojis.[idx] <- emojiState
                        Resolved emojis }
            let cmd = Cmd.map (fun msg -> EmojiTagsEditMsg(idx, msg)) cmd
            state, cmd
        | _ -> state, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let navBrand dispatch =
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
        Navbar.Item.a [
            Navbar.Item.Props [
                OnClick (fun _ ->
                    dispatch (EmojiUploadFormMsg (EmojiUploadForm.FormVisible true))
                )
            ]
        ] [
            str "Add emoji"
        ]
    ]

let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [] [
        EmojiUploadForm.view state.EmojiUploadForm (EmojiUploadFormMsg >> dispatch)

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
                    for idx, emojiState in Array.indexed emojis do
                        EmojiTagsEdit.view emojiState (fun msg ->
                            EmojiTagsEditMsg(idx, msg)
                            |> dispatch)
                        let emojiId = emojiState.Emoji.Id
                        img [ Src emojiId ]

                        match Browser.Navigator.navigator.clipboard with
                        | Some clipboard ->
                            Button.span [
                                Button.OnClick (fun _ ->
                                    clipboard.writeText emojiId
                                    |> ignore
                                )
                            ] [
                            Fa.span [ Fa.Solid.Clipboard
                                      Fa.FixedWidth
                                    ]
                                [ ]
                            ]
                        | None -> ()
                ]
            ]
        | InProgress -> spinner
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
                Container.container [] [ navBrand dispatch ]
            ]
        ]

        Feliz.React.router [
            router.onUrlChanged (ChangeUrl >> dispatch)
            router.children [
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
        ]
    ]
