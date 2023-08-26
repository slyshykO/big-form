module Main

open Elmish
open Elmish.React
open Fable.Form.Simple
open Fable.Form.Simple.Bulma
open Feliz

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type AsyncOperationStatus<'t> =
    | Started
    | Finished of 't


type Values =
    {
        Email : string
        Password : string
        RememberMe : bool
        extra0: string
        extra1: string
        extra2: string
        extra3: string
        extra4: string
        extra5: string
        extra6: string
        extra7: string
    }

type Model =
    Form.View.Model<Values>

type Msg =
    | Empty
    | FormChanged of Model
    | LogIn of string * string * bool * string* string* string* string* string* string* string* string

type State = 
    {
        empty: int
        model: Model 
    }

let init () = 

    let m = 
        {
            Email = ""
            Password = ""
            RememberMe = false
            extra0 = ""
            extra1 = ""
            extra2 = ""
            extra3 = ""
            extra4 = ""
            extra5 = ""
            extra6 = ""
            extra7 = ""
        } |> Form.View.idle

    { 
        empty = 0
        model = m
    }, Cmd.none

let update msg state =
    match msg with
    | Empty -> { state with empty = state.empty + 1 }, Cmd.none
    | FormChanged model ->
        let nextState = { state with model = model }
        nextState, Cmd.none

    | LogIn (_email, _password, _rememberMe, ex0, ex1, ex2, ex3, ex4, ex5, ex6, ex7) ->
        printfn "LogIn: %s %s %b %s %s %s %s %s %s %s %s" _email _password _rememberMe ex0 ex1 ex2 ex3 ex4 ex5 ex6 ex7
        let model = state.model
        let nextModel = { model with State = Form.View.Success "You have been logged in successfully" }
        { state with model = nextModel }, Cmd.none


let form : Form.Form<Values, Msg, _> =
    let emailField =
        Form.textField
            {
                Parser =
                    fun value ->
                        if value.Contains("@") then
                            Ok value
                        else
                            Error "The e-mail address must contain a '@' symbol"
                Value =
                    fun values -> values.Email
                Update =
                    fun newValue values ->
                        { values with Email = newValue }
                Error =
                    fun _ -> None
                Attributes =
                    {
                        Label = "Email"
                        Placeholder = "some@email.com"
                        HtmlAttributes = [
                            prop.autoComplete "email"
                        ]
                    }
            }

    let passwordField =
        Form.passwordField
            {
                Parser = Ok
                Value =
                    fun values -> values.Password
                Update =
                    fun newValue values ->
                        { values with Password = newValue }
                Error =
                    fun _ -> None
                Attributes =
                    {
                        Label = "Password"
                        Placeholder = "Your password"
                        HtmlAttributes = [
                            prop.autoComplete "current-password"
                        ]
                    }
            }

    let rememberMe =
        Form.checkboxField
            {
                Parser = Ok
                Value =
                    fun values -> values.RememberMe
                Update =
                    fun newValue values ->
                        { values with RememberMe = newValue }
                Error =
                    fun _ -> None
                Attributes =
                    {
                        Text = "Remember me"
                    }
            }

    let extra0Field =
        Form.textField
            {
                Parser =
                    fun value ->
                            Ok value
                Value =
                    fun values -> values.extra0
                Update =
                    fun newValue values ->
                        { values with extra0 = newValue }
                Error =
                    fun _ -> None
                Attributes =
                    {
                        Label = "extra0"
                        Placeholder = "extra0"
                        HtmlAttributes = [
                            prop.autoComplete "extra0"
                        ]
                    }
            }

    let extra1Field =
        Form.textField
            {
                Parser =
                    fun value ->
                            Ok value
                Value =
                    fun values -> values.extra1
                Update =
                    fun newValue values ->
                        { values with extra1 = newValue }
                Error =
                    fun _ -> None
                Attributes =
                    {
                        Label = "extra0"
                        Placeholder = "extra0"
                        HtmlAttributes = [
                            prop.autoComplete "extra0"
                        ]
                    }
            }

    let onSubmit =
        fun email password rememberMe ex0 ex1 ex2 ex3 ex4 ex5 ex6 ex7->
            LogIn (email, password, rememberMe, ex0, ex1, ex2, ex3, ex4, ex5, ex6, ex7)

    Form.succeed onSubmit
        |> Form.append emailField
        |> Form.append passwordField
        |> Form.append rememberMe
        |> Form.append extra0Field
        |> Form.append extra1Field
        |> Form.append extra0Field
        |> Form.append extra0Field
        |> Form.append extra0Field
        |> Form.append extra0Field
        |> Form.append extra0Field
        |> Form.append extra0Field

let private loginForm state dispatch =
    let model = state.model
    Form.View.asHtml
        {
            Dispatch = dispatch
            OnChange = FormChanged
            Action = Form.View.Action.SubmitOnly "Sign in"
            Validation = Form.View.ValidateOnSubmit
        }
        form
        model

let view state dispatch =
    Html.div [
        
        prop.children [
            loginForm state dispatch
        ]
    ]


Program.mkProgram init update view
|> Program.withReactBatched "feliz-app"
|> Program.run
