
let parseArgs (args: string []) =
    let rec parse (args: string [], parsed: _ list) =
        match List.ofArray args with
        | [] ->
            parsed
        | head :: tail ->
            match head with
            | "--token" when tail.Length >= 1 ->
                let tail' = tail.Tail
                let token = `Token tail.Head
                
                parse (Array.ofList tail', token :: parsed)
                
            | "--dry-run" ->
                let dryRun = `DryRun true
                parse (Array.ofList tail, dryRun :: parsed)
                
            | _ ->
                parse (Array.ofList tail, parsed)
            
    parse (args, [])
    
[<EntryPoint>]
let main (argv: string []) =
    let parsedArgs = parseArgs argv
    
    for arg in parsedArgs do
        match arg with
        | `Token token ->
            printfn "Token: %s" token
        | `DryRun isDryRun ->
            printfn "Dry run: %b" isDryRun
            
    let errors = [
        `Fuck "This";
        `Works "Really"
        `Well [42, 69]
    ]
    
    let errors' = errors @ [ `OneMore "Error" ]
    
    for error in errors' do
        match error with
        | `Fuck msg ->
            printfn "Error: %s" msg
        | `Works msg ->
            printfn "Error: %s" msg
        | `Well code ->
            printfn "Error: %A" code
        | _ ->
            printfn "Unknown error"
             

    
    
    0