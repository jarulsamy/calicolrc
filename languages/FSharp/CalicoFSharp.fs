module Test

open System.CodeDom.Compiler
open Microsoft.FSharp.Compiler.CodeDom
open System.Reflection
open System
open System.IO

let mutable x = 41

let test (code:string) =
  let compiler = new FSharpCodeProvider()
  let cp = new System.CodeDom.Compiler.CompilerParameters()
  for assembly in AppDomain.CurrentDomain.GetAssemblies() do
    if assembly.Location <> "" then  cp.ReferencedAssemblies.Add(assembly.Location) |>ignore 
    (*printfn "assembly: %s" (assembly.Location)*)
  done

  (*if references <> "" then cp.ReferencedAssemblies.Add(references) |> ignore *)
  cp.GenerateInMemory <- false
  let (++) v1 v2   = Path.Combine(v1, v2)  
  cp.OutputAssembly <-  __SOURCE_DIRECTORY__ ++ Path.GetRandomFileName() + ".exe"  
  (*cp.WaringLevel <- 4*)
  cp.GenerateExecutable <- true
  cp.TreatWarningsAsErrors <- false
  let cr = compiler.CompileAssemblyFromSource(cp, "module Test open Test;;" + code)
  for o in cr.Errors do
    eprintfn "%s" (o.ToString())

  cr.CompiledAssembly.EntryPoint.Invoke(null, null) |> ignore
  (cr.CompiledAssembly, cr.Output, cr.Errors)

printfn "%s" "starting the program"
test "let zack = 8888;; printfn \"%d\" x;;" |> ignore
x <- 42;
test "printfn \"%d\" (zack + 1);; printfn \"%d\" x;;" |> ignore
x <- 43;
test "printfn \"%d\" x;; printfn \"%d\" (x + 22);;" |> ignore
(*test "printfn \"%d\" 1;;"*)
