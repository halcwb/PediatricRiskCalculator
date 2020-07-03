module App

open System

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Thoth.Json
open Feliz
open Feliz.UseElmish
open Feliz.MaterialUI
open Fable.MaterialUI.Icons
open Fable.Core.JsInterop
open Shared
open Utils


type State =
    {
        Page: Page
        ShowMenu : bool
    }
and Page = PIM | PRISM


type Msg = Started | MenuItemClick of string | ShowMenu of bool


let init () = { Page = PIM; ShowMenu = false }, Cmd.none


let menuItems =
    [
        "PIM 2 & 3 Score", PIM
        "PRISM IV Score", PRISM
    ]


let update msg state =
    match msg with
    | Started -> state, Cmd.none
    | MenuItemClick s ->
        match menuItems |> List.tryFind (fst >> ((=) s)) with
        | Some (_, p) ->
            { state with
                Page = p
                ShowMenu = false }
            , Cmd.none
        | _ -> state, Cmd.none
    | ShowMenu b ->
        { state with ShowMenu = b }, Cmd.none


let defaultTheme =
    Styles.createMuiTheme()
    |> Styles.responsiveFontSizes


let useStyles = Styles.makeStyles(fun styles theme ->
    {|
        appBar = styles.create [
            style.display.flex
            style.flexDirection.row
            style.padding (theme.spacing 1)
            style.zIndex (theme.zIndex.drawer + 1)
        ]

        page = styles.create [
            style.marginTop (theme.spacing 10)
            style.marginBottom (theme.spacing 5)
        ]

        title = styles.create [
            style.padding (theme.spacing 1)
        ]

        menuButton = styles.create [
                style.padding (theme.spacing 1)
        ]

    |}
)


let private comp =
    React.functionComponent("app", fun (props: {| state : State; dispatch : Msg -> unit |}) ->
        let state, dispatch = props.state, props.dispatch
        let classes = useStyles ()

        Mui.themeProvider [
            themeProvider.theme  defaultTheme
            themeProvider.children [
                Mui.container [
                    container.component' "main"
                    container.maxWidth.sm
                    prop.children [
                        menuItems
                        |> List.map fst
                        |> Components.MenuDrawer.render state.ShowMenu (MenuItemClick >> dispatch)

                        Mui.appBar [
                            appBar.classes.root classes.appBar
                            appBar.position.fixed'
                            appBar.children [
                                Mui.iconButton [
                                    prop.className classes.menuButton
                                    prop.onClick (fun _ -> state.ShowMenu |> not |> ShowMenu |> dispatch)

                                    iconButton.color.inherit'
                                    iconButton.children [
                                        menuIcon []
                                    ]
                                ]

                                Mui.typography [
                                    prop.className classes.title
                                    typography.variant.h6
                                    prop.text "Informedica Pediatric Risk Calculator"
                                ]
                            ]
                        ]

                        Mui.container [
                            prop.className classes.page
                            prop.children [
                                match state.Page with
                                | PIM   -> Pages.PIM.render ()
                                | PRISM -> Pages.PRISM.render ()
                            ]

                        ]
                    ]
                ]
            ]
        ]
    )


let render state dispatch = comp ({| state = state; dispatch = dispatch |})
