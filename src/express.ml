type done_
(** abstract type which ensure middleware function must either
    call the [next] function or one of the [send] function on the 
    response object. 

    This should be a great argument for OCaml, the type system 
    prevents silly error which in this case would make the server hang *)

type app 

(** TODO : maybe this should be a more common module like bs-error *)
module Error = struct 

  type t 
  (** Error type *)

  external message : t -> string option = "" 
    [@@bs.send] [@@bs.return null_undefined_to_opt]
  
  external name : t -> string option = "" 
    [@@bs.send] [@@bs.return null_undefined_to_opt]

end 

module Request = struct 

  type t 

  type params = Js_json.t Js_dict.t

  external params : t -> params = "" [@@bs.get]
  (** [params request] return the JSON object filled with the 
      request parameters *)

  external asJSONObject : t -> Js_json.t Js_dict.t = "%identity"
  (** [asJSONObject request] casts a [request] to a JSON object. It is 
      common in Express application to use the Request object as a 
      placeholder to maintain state through the various middleware which 
      are executed. *) 

  external app : t -> app = "" [@@bs.get]
  (** [app request] returns the 'app' property.*)
  
  external baseUrl: t -> string = "" [@@bs.get]
  (** [baseUrl request] returns the 'baseUrl' property *)

  external body : t -> 'a Js.undefined = "" [@@bs.get] 
  (** [body request] returns the body. By default it is [undefined] and is 
      populated when you use body parsing middleware such as 
      body-parser or multer. *)

  external cookies : t -> Js_json.t Js_dict.t = "" [@@bs.get]
  (** When using cookie-parser middleware, this property is an object 
      that contains cookies sent by the request. If the request contains 
      no cookies, it defaults to {}.*) 

  external signedCookies : t -> Js_json.t Js_dict.t = "" [@@bs.get]
  (** When using cookie-parser middleware, this property contains 
      signed cookies sent by the request, unsigned and ready for use. *) 

  external fresh : t -> bool =  "" [@@bs.get]
  (** [fresh request] returns [true] whether the request is "fresh" *)
  
  external stale : t -> bool =  "" [@@bs.get]
  (** [stale request] returns [true] whether the request is "stale"*)

  external hostname : t -> string = "" [@@bs.get] 
  (** [hostname request] Contains the hostname derived from the Host 
      HTTP header.*)
  
  external ip : t -> string = "" [@@bs.get] 
  (** [ip request] Contains the remote IP address of the request.*) 
  
  external ips : t -> string array = "" [@@bs.get] 
  (** [ip request] Contains the remote IP address of the request.*) 
  
  external method_ : t -> string = "method" [@@bs.get] 
  (** [ip request] return a string corresponding to the HTTP 
      method of the request: GET, POST, PUT, and so on *)

  external originalUrl : t -> string = "" [@@bs.get]
  (** [originalUrl request] returns the original url. See
      https://expressjs.com/en/4x/api.html#req.originalUrl *)

  external path : t -> string = "" [@@bs.get] 
  (** [path request] returns the path part of the request URL.*)

  external protocol : t -> string = "" [@@bs.get] 
  (** [protocol request] returns the request protocol string: either http 
      or (for TLS requests) https. *)

  external query : t -> Js_json.t Js_dict.t = "" [@@bs.get] 
  (** [query request] returns an object containing a property for each 
      query string parameter in the route. If there is no query string, 
      it returns the empty object, {} *)

  (* missing route *) 

  external secure : t -> bool = "" [@@bs.get]
  (** [secure request] returns [true] if a TLS connection is established *)

  external subdomains : t -> string array = "" [@@bs.get] 
  (** [subdomains req] returns an array of subdomains in the domains 
      of the request *)

  external xhr : t -> bool = "" [@@bs.get]
  (** [xhr request] returns [true] if the request’s X-Requested-With 
      header field is "XMLHttpRequest", indicating that the request was 
      issued by a client library such as jQuery *)

  external acceptsRaw: t -> string array -> Js_json.t = "accepts" [@@bs.send]
  (** [acceptsRaw request types] checks if the specified content types 
      are acceptable, based on the request's Accept HTTP header field. 
      The method returns the best match, or if none of the specified 
      content types is acceptable, returns [false] *)

  let accepts : t -> string array -> string option = fun req types -> 
    let ret = acceptsRaw req types in 
    let ty, x = Js_json.reifyType ret in 
    match ty with 
    | Js_json.String -> Some x 
    | _ -> None
  (* this function while introducing a much better interface also adds 
   * dependency on Js_json module and increase code size *)

  external acceptsCharsetsRaw : 
    t -> string array -> Js_json.t = "acceptsCharsets" [@@bs.send]

  let acceptsCharsets : t -> string array -> string option = fun req types -> 
    let ret = acceptsCharsetsRaw req types in 
    let ty, x = Js_json.reifyType ret in 
    match ty with 
    | Js_json.String -> Some x 
    | _ -> None

  external get : t -> string -> string option = "" 
    [@@bs.send] [@@bs.return null_undefined_to_opt]
  (** [get return field] returns the specified HTTP request header 
      field (case-insensitive match) *)

  (*  external is : t -> string -> Js.boolean = "" [@@bs.send]*)
  (*  TODO : Differ from documentation !!! *)
  (** [isType request contentType] returns [true] if the incoming request's
      "Content-Type" HTTP header field matches the MIME type specified by 
      the type parameter. Returns [false] otherwise. *)
end


module Response = struct 
  type t 

  external end_ : 
    t -> 
    unit -> 
    done_ = "end" [@@bs.send]
  
  external sendJson: 
    t -> 
    Js_json.t -> 
    done_ = "send" [@@bs.send] 

  external sendString : 
    t -> 
    string -> 
    done_ = "send" [@@bs.send] 

  external sendStatus : 
    t -> 
    int -> 
    done_ = "" [@@bs.send]

  external downloadSingle : 
    t -> 
    string -> 
    done_ = "download" [@@bs.send] 

  external downloadSingleWithError: 
    t -> 
    string -> 
    (Error.t -> unit) -> 
    done_ = "download" [@@bs.send] 
end

module Next : sig 
  type content  

  type t = content Js.undefined -> done_ [@bs]

  val router : content Js.undefined  
  (** value to use as [next] callback argument to skip middleware 
      processing. *) 

  val error : Error.t -> content Js.undefined 
  (** [error e] returns the argument for [next] callback to be propagate 
      error [e] through the chain of middleware. *)

end = struct 

  type content 

  type t = content Js.undefined -> done_ [@bs]

  external castToContent : 'a -> content = "%identity" 
  (* Not exposed in .mli *)

  let router = 
    "router" |> castToContent |> Js.Undefined.return  

  let error (e:Error.t) = 
    e |> castToContent |> Js.Undefined.return 

end 

module Middleware = struct 

  type next = Next.t

  type t 
    (** Middleware abstract type which unified the various way an 
        Express middleware can be constructed:
        {ul
        {- from a {b function} using the [ofFunction]}  
        {- from a Router class}
        {- from an App class}
        {- from the third party middleware modules}   
        }
      
        For each of the class which implements the middleware interface 
        is JavaScript, one can use the "%identity" function to cast
        it to this type [t] *)

  type f = Request.t -> Response.t -> next -> done_

  external ofF : f -> t = "%identity" 
  (** Create a Middleware from a function *)

  type errorF = Error.t -> Request.t -> Response.t -> next -> done_ 

  external ofErrorF : errorF -> t = "%identity" 
  (** Create a Middleware from an error function *)

end 

(** Generate the common Middleware binding function for a given 
    type. This Functor is used for the Router and App classes. *)
module MakeBindFunctions(T: sig type t end) = struct 

  external use : T.t -> Middleware.t -> unit = "" [@@bs.send]
  
  external useOnPath : T.t -> string -> Middleware.t -> unit = "use" [@@bs.send]

  external get :  
    T.t -> 
    string -> 
    Middleware.t -> 
    unit = "" [@@bs.send] 

  external getN :  
    T.t -> 
    string -> 
    Middleware.t array -> 
    unit = "get" [@@bs.send] 

  external post :  
    T.t -> 
    string -> 
    Middleware.t -> 
    unit = "" [@@bs.send] 

  external postN :  
    T.t -> 
    string -> 
    Middleware.t array -> 
    unit = "post" [@@bs.send] 

  external put :  
    T.t -> 
    string -> 
    Middleware.t -> 
    unit = "" [@@bs.send] 

  external putN :  
    T.t -> 
    string -> 
    Middleware.t array -> 
    unit = "" [@@bs.send] 

  external delete :  
    T.t -> 
    string -> 
    Middleware.t -> 
    unit = "" [@@bs.send] 

  external deleteN :  
    T.t -> 
    string -> 
    Middleware.t array -> 
    unit = "delete" [@@bs.send] 

  external all :  
    T.t -> 
    string -> 
    Middleware.t -> 
    unit = "" [@@bs.send] 

  external allN :  
    T.t -> 
    string -> 
    Middleware.t array -> 
    unit = "all" [@@bs.send] 

end 

module App = struct 

  type t = app 

  include MakeBindFunctions(struct type nonrec t = t end)
  
  external make : unit -> t = "express" [@@bs.module] 
  (** [make ()] create an App class *)

  external toMiddleware : t -> Middleware.t = "%identity"
  (** [toMiddleware app] casts an App  to a Middleware type *) 

  external set_setting : t -> ([ 
    | `Case_sensitive_routing of Js.boolean [@bs.as "case sensitive routing"]
    | `Env of string [@bs.as "env"]
    | `Etag of Js_json.t [@bs.as "etag"] 
    | `Jsonp_callback_name of string [@bs.as "jsonp callback name"]
    | `Json_spaces_number of int [@bs.as "json spaces"] 
    | `Json_spaces_string of string [@bs.as "json spaces"]
    | `Query_parser of Js_json.t [@bs.as "query parser"] 
    | `String_routing of Js.boolean [@bs.as "strict routing"] 
    | `Subdomain_offset of int [@bs.as "subdomain offset"]
    | `Trust_proxy of Js_json.t [@bs.as "trust proxy"] 
    | `View of string [@bs.as "views"] 
    | `Views of string array [@bs.as "views"]
    | `View_engine of string [@bs.as "view engine"] 
    | `X_powered_by of Js.boolean [@bs.as "x-powered-by"]
  ] [@bs.string]) -> unit = "set" [@@bs.send] 
  (** [set_setting t name value] sets a setting with [name] and [value] *)
  
  external get_setting : t -> ([
    | `Case_sensitive_routing [@bs.as "case sensitive routing"]
    | `Env [@bs.as "env"]
    | `Etag [@bs.as "etag"] 
    | `Jsonp_callback_name [@bs.as "jsonp callback name"]
    | `Json_spaces_number [@bs.as "json spaces"] 
    | `Json_spaces_string [@bs.as "json spaces"]
    | `Query_parser [@bs.as "query parser"] 
    | `String_routing [@bs.as "strict routing"] 
    | `Subdomain_offset [@bs.as "subdomain offset"]
    | `Trust_proxy [@bs.as "trust proxy"] 
    | `View [@bs.as "views"] 
    | `Views [@bs.as "views"]
    | `View_engine [@bs.as "view engine"] 
    | `X_powered_by [@bs.as "x-powered-by"]
    ] [@bs.string]) -> Js_json.t = "get" [@@bs.send]
  (** [get_setting app setting] returns the setting value *) 

  external enable : t -> ([
    | `Case_sensitive_routing [@bs.as "case sensitive routing"]
    | `String_routing [@bs.as "strict routing"] 
    | `X_powered_by [@bs.as "x-powered-by"]
    | `Etag [@bs.as "etag"]
  ] [@bs.string]) -> unit = "" [@@bs.send] 
  (** [enable app `Case_sensitive_routing] enables the Case sensitive routing. 
    *)
  
  external enabled : t -> ([
    | `Case_sensitive_routing [@bs.as "case sensitive routing"]
    | `String_routing [@bs.as "strict routing"] 
    | `X_powered_by [@bs.as "x-powered-by"]
    | `Etag [@bs.as "etag"]
  ] [@bs.string]) -> bool = "" [@@bs.send] 
  (** [enabled app setting] returns [true] is [setting] is enabled, [false]
      otherwise. *)

  external disable : t -> ([
    | `Case_sensitive_routing [@bs.as "case sensitive routing"]
    | `String_routing [@bs.as "strict routing"] 
    | `X_powered_by [@bs.as "x-powered-by"]
    | `Etag [@bs.as "etag"]
  ] [@bs.string]) -> unit = "" [@@bs.send] 
  (** [disable app `Case_sensitive_routing] enables the Case sensitive routing. 
    *)
  external disabled : t -> ([
    | `Case_sensitive_routing [@bs.as "case sensitive routing"]
    | `String_routing [@bs.as "strict routing"] 
    | `X_powered_by [@bs.as "x-powered-by"]
    | `Etag [@bs.as "etag"]
  ] [@bs.string]) -> bool = "" [@@bs.send] 
  (** [disabled app setting] returns [true] if [setting] is disabled, [false]
      otherwise. *)

  external path : t -> string = "" [@@bs.send]
  (** [path app] returns the canonical path of the app 
      see https://expressjs.com/en/4x/api.html#app.path *)

  external render : 
    t -> string -> (Error.t Js.null -> string -> unit) -> unit = 
    "" [@@bs.send] 
  (** [render app view cb] renders the view as an HTML file. When rendering
      is complete [cb error html] is invoke. If no error happened [error] 
      will be undefined, otherwise it will hold an [Error.t] value. *)
  
  external listen : t -> int -> unit = "" [@@bs.send]
  (** [listen port] starts the web server and listen for new 
      connection on [port] *)

end 

module Router = struct 
  
  type t
  
  include MakeBindFunctions(struct type nonrec t = t end)
  
  external make : unit -> t = "Router" [@@bs.module "express"] 
  (** [make ()] create a new Router class *)

  external toMiddleware : t -> Middleware.t = "%identity" 
  (** [toMiddleware router] casts a Router to a Middleware type *)

end 

module Static = struct 

  type options 

  external dotfiles : options -> string -> unit = "" [@@bs.set]

  external etag : options -> Js.boolean -> unit = "" [@@bs.set]
  
  (* ... add all the other options *) 
  
  type t 
 
  external make : string -> options -> t = "static" [@@bs.module "express"] 
  (** [make directory] create a static middleware for [directory] *)

  external toMiddleware : t -> Middleware.t = "%identity"
  (** Cast a Static class to a Middleware *)

end 
  
let make_options : unit -> Static.options = fun () ->
  (Obj.magic (Js_obj.empty ()) : Static.options) 


(** Example of a Third Party Middleware binding.  
 *
 *  !!! This should really be in its own module !!!  *)

module BodyParserJSON = struct 

  type t (* JSON Middleware *)

  external make : unit -> t = "json" [@@bs.module "body-parser"]
  (** [make ()] create a new body parser middleware *)

  external toMiddleware : t -> Middleware.t = "%identity"
  (* One could skip the identity function and have the json function
   * returns directly a Middleware.t *) 

end 


