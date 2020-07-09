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


let title = "Informedica Pediatric Risk Calculator"


let menuItems =
    [
        "PIM 2 & 3 Score", PIM
        "PRISM IV Score", PRISM
    ]


let init () = { Page = PIM; ShowMenu = false }, Cmd.none


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

        page = styles.create [
            style.marginTop (theme.spacing 10)
            style.marginBottom (theme.spacing 5)
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
                    // set the container width depending on screen size
                    if Hooks.useMediaQuery defaultTheme.breakpoints.upLg then
                        container.maxWidth.md
                    else
                        container.maxWidth.sm

                    prop.children [
                        // create the title bar
                        menuItems
                        |> List.map fst
                        |> Components.MenuDrawer.render state.ShowMenu (MenuItemClick >> dispatch)

                        [
                            menuIcon [], (fun _ -> state.ShowMenu |> not |> ShowMenu |> dispatch)
                        ]
                        |> Components.TitleBar.render title

                        // select the page to show
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
