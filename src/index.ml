
let option_map v f = function
  | None -> v
  | Some x -> f x

let option_bind v f = function
  | None -> Some v
  | Some x -> f x

let option_default v = function
  | None -> v 
  | Some x -> x 

let string_option_of_json json = 
  let ty, s = Js_json.reifyType json in 
  match ty with
  | Js_json.String -> Some s
  | _ -> None 

open Express 

(* This is the exact same example from the Express documentation 
 * on how to create a Router *) 
module Birds = struct 

  let router = Router.make () 

  let () =
    Router.use router @@ Middleware.ofF (fun _ _ next -> 

      let date = 
        Js.Date.now () 
        |> Js.Date.fromFloat
        |> Js.Date.toString
      in 
      Js.log ("Time: " ^ date);
      next Js.undefined [@bs]
    ); 

    Router.get router "/" @@ Middleware.ofF (fun _ res _ -> 
      Response.sendString res "Birds Home Page"
    ); 

    Router.get router "/about" @@ Middleware.ofF (fun _ res _ -> 
      Response.sendString res "About birds"
    )
end 

let () = 
  let app = App.make () in 

  (* get/set settings *)

  App.set_setting app (`Etag (Js_json.boolean Js.true_)); 
  App.set_setting app (`Etag (Js_json.string "strong")); 
  App.set_setting app (`Case_sensitive_routing Js.true_); 
  App.set_setting app (`View "views"); 
  App.set_setting app (`Views [| "view1"; "view2"|]); 

  begin match App.get_setting app `Etag |> string_option_of_json with
    | Some "strong" -> () 
    | _ -> failwith "Invalid app setting"
  end; 

  (* enable *) 
  App.enable app `Etag; 
  
  begin 
    let ty, x = App.get_setting app `Etag |> Js_json.reifyType in 
    match ty with
    | Js_json.Boolean -> assert(x = Js.true_) 
    | _ -> failwith "Invalid app enabled setting"
  end; 

  (* enabled *)

  assert(App.enabled app `Etag);

  (* disable *)

  App.disable app `Etag;

  begin 
    let ty, x = App.get_setting app `Etag |> Js_json.reifyType in 
    match ty with
    | Js_json.Boolean -> assert(x = Js.false_) 
    | _ -> failwith "Invalid app enabled setting"
  end; 

  (** disabled *)

  assert(App.disabled app `Etag); 

  assert("" = App.path app);

  begin 
    let cb_invoked = ref false in 
    App.render app "non-existing.pug" (fun err _ -> 
      cb_invoked := true; 
      assert(not @@ Js_null.test err); 
        (* an error should have occured *)
    );
    assert(!cb_invoked);
  end; 
  
  begin 
    let cb_invoked = ref false in 
    App.render app "existing.pug" (fun err html -> 
      cb_invoked := true; 
      assert(Js_null.test err); 
      assert("<p>this is a test</p>" = html);
    );
    assert(!cb_invoked);
  end; 

  App.use app @@ Middleware.ofF(fun req _ next -> 
    print_endline @@ "baseUrl: " ^ (Request.baseUrl req);
    print_endline @@ 
      "isFresh: " ^ 
      (Request.fresh req |> string_of_bool);
    print_endline @@ 
      "isStale: " ^ 
      (Request.stale req |> string_of_bool);
    print_endline @@ "hostname: " ^ (Request.hostname req); 
    print_endline @@ "ip: " ^ (Request.ip req); 
    print_endline @@ 
      "ips: [" ^ (
      Request.ips req |> Array.to_list |> String.concat ", ") ^ 
      "]"; 
    print_endline @@ "method: " ^ (Request.method_ req); 
    Js.log (Request.ip req);
    print_endline @@ "originalUrl: " ^ (Request.originalUrl req);
    print_endline @@ "path: " ^ (Request.path req); 
    print_endline @@ "protocol: " ^ (Request.protocol req); 
    print_endline @@ 
      "subdomains: [" ^ (
      Request.subdomains req |> Array.to_list |> String.concat ", ") ^ 
      "]"; 

    print_string "accepts images/png: "; 
    Js.log @@ Request.accepts req [| "images/png" |]; 
    print_string "accept charsets: "; 
    Js.log @@ Request.acceptsCharsets req [| "utf-8" |]; 
    print_string "Accept Header: ";
    begin match Request.get req "Content-Type" with
    | Some x -> print_endline x 
    | _ -> assert(false)
    end;
    next Js.undefined  [@bs]
  );  

  (* Show how to add a Middleware made out of a Router class *)
  App.useOnPath app "/birds" (Birds.router |> Router.toMiddleware); 

  (* -- *)

  (* Show how to add a Middleware from a function *)
  App.useOnPath app "/" @@ Middleware.ofF (fun _ _ next -> 
    Js.log "Request received";
    next Js.undefined [@bs]
  ); 
  
  (* -- *)

  (* Show how to add multiple chained Middleware made of functions
   * for a given method and path *)

  let fromMiddlewares = [|

    Middleware.ofF (fun req _ next-> 
      let params = Request.params req in 
      let who = 
        Js_dict.get params "who"
        |> option_bind "No who param" string_option_of_json 
        |> option_default "Not a JSON string" 
      in 
      print_endline @@ "This is who: " ^ who; 
      next Js.undefined [@bs]
    );

    Middleware.ofF (fun _ res _ -> 
      Response.sendString res "Hello World"; 
    ); 

  |] in 
  
  App.getN app "/from/:who?" fromMiddlewares;
  
  App.get app "/skip" @@ Middleware.ofF (fun _ _ next -> 
    next Next.router [@bs]
  );

  
  (* -- *)

  App.get app "/ok" @@ Middleware.ofF (fun _ res _ -> 
    Response.sendStatus res 200
  ); 

  begin 
    let path = "/downloadSingle" in
    App.get app path @@ Middleware.ofF (fun _ res _ -> 
      Response.downloadSingle res "test.txt"
    );
  end; 
  
  begin 
    let path = "/downloadSingleWithError" in 
    App.get app path @@ Middleware.ofF  (fun _ res _ -> 
      Response.downloadSingleWithError res "test.txt" (fun _ -> 
        Js.log "An error happened"
      ) 
    )
  end;
  
  App.get app "/end" @@ Middleware.ofF (fun _ res _ -> 
    Response.end_ res ()
  );
  
  App.useOnPath app "/static" (
    let options = make_options () in 
    Static.etag options Js.true_; 
    Static.make "static-html" options |> Static.toMiddleware
  );  

  App.listen app 8000;

  ()
