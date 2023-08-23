import gleam/json
import lustre/effect.{Effect}

pub type HttpError {
  Unauthorized
  NotFound
  InternalServerError(String)
  OtherError(Int, String)
}

pub type StringResult =
  Result(String, HttpError)

pub type HttpOrJsonError {
  H(HttpError)
  J(json.DecodeError)
}

pub type JsonResult(t) =
  Result(t, HttpOrJsonError)

external fn do_request(
  method: String,
  url: String,
  body: String,
  on_success: fn(String) -> Nil,
  on_error: fn(Int, String) -> Nil,
) -> Nil =
  "./ffi.mjs" "do_request"

fn do_get(url, on_success, on_error) {
  do_request("GET", url, "", on_success, on_error)
}

fn do_post(url, body, on_success, on_error) {
  do_request("POST", url, body, on_success, on_error)
}

/// ###  Usage
/// ```
/// import lustre_http as http
///
/// type Msg {
///   SomeInteraction
///   TextReceived(StringResult)
/// }
///
/// pub fn update(model, msg) {
///   case msg {
///     SomeInteraction -> #(model, http.get_as_text("the_text", TextReceived))
///     TextReceived(Ok(text)) -> #(apply(text, model), effect.none())
///     TextReceived(Error(e)) -> #(indicate_problem(e, model), effect.none())
///   }
/// }
/// ```
pub fn get_as_text(url: String, to_msg: fn(StringResult) -> msg) -> Effect(msg) {
  use dispatch <- effect.from

  do_get(
    url,
    fn(body) { dispatch(to_msg(Ok(body))) },
    fn(code, msg) {
      decode_error(code, msg)
      |> Error
      |> to_msg
      |> dispatch
    },
  )
}

fn decode_error(code, msg) {
  case code {
    401 -> Unauthorized
    404 -> NotFound
    500 -> InternalServerError(msg)
    _ -> OtherError(code, msg)
  }
}

/// Will automatically try to decode the JSON for you.
/// The error-type is a bit complicated. In a future version will probably
/// (a) force "text/json" in the headers (b) not decode for you.
pub fn get_as_json(
  url: String,
  to_msg: fn(JsonResult(String)) -> msg,
  decoder,
) -> Effect(msg) {
  use dispatch <- effect.from

  do_get(
    url,
    fn(body) {
      case json.decode(from: body, using: decoder) {
        Ok(json) -> dispatch(to_msg(Ok(json)))
        Error(json_error) -> dispatch(to_msg(Error(J(json_error))))
      }
    },
    fn(code, msg) {
      let http_error = case code {
        401 -> Unauthorized
        404 -> NotFound
        500 -> InternalServerError(msg)
        _ -> OtherError(code, msg)
      }
      dispatch(to_msg(Error(H(http_error))))
    },
  )
}

/// Future versions will force headers in both the request and the response
/// so you can post json and receive text, post text and receive json, etc.
pub fn post_text(url: String, body: String, to_msg: fn(StringResult) -> msg) {
  use dispatch <- effect.from

  do_post(
    url,
    body,
    fn(body) { dispatch(to_msg(Ok(body))) },
    fn(code, msg) {
      decode_error(code, msg)
      |> Error
      |> to_msg
      |> dispatch
    },
  )
}
