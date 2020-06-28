namespace Components


module ComboBox =
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


    let createMenuItems items =
        items
        |> List.mapi (fun i (s : string) ->
            Mui.menuItem
                [
                    prop.value i
                    menuItem.children [
                        Mui.typography [
                            typography.color.primary
                            typography.variant.body1
                            prop.text s
                        ]
                    ]
                ]
        )


    let private comp =
        React.functionComponent("combobox", fun (props: {| value: string; items: string list; label: string; dispatch : string -> unit |}) ->
            printfn "props: %A" props.value
            Mui.formControl [
                Mui.inputLabel props.label
                Mui.select [
                    match props.items |> List.tryFindIndex ((=) props.value) with
                    | Some i -> select.value i
                    | None   -> select.value 0

                    select.onChange (fun (e : int) ->
                        props.items.[e]
                        |> props.dispatch
                     )

                    props.items
                    |> createMenuItems
                    |> prop.children
                ]
            ]
        )


    let render props = comp props