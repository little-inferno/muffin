package muffin.interop.http

import cats.effect.Sync
import cats.{~>, Applicative, MonadThrow}
import sttp.client3.*
import sttp.client3.given
import sttp.model.{Header, Method as SMethod, Uri}
import cats.syntax.all.given
import muffin.codec.{Decode, Encode}
import muffin.http.{Body, HttpClient, Method, MultipartElement}

import java.io.File

class SttpClient[F[_]: MonadThrow, To[_], From[_]](
  backend: SttpBackend[F, Any]
)(requestModify: SttpClient.Request => SttpClient.Request = identity)(using tk: To ~> Encode, fk: From ~> Decode)
    extends HttpClient[F, To, From] {
  def request[In: To, Out: From](
    url: String,
    method: Method,
    body: Body[In],
    headers: Map[String, String]
  ): F[Out] = {
    val req = basicRequest
      .method(
        method match {
          case Method.Get    => SMethod.GET
          case Method.Post   => SMethod.POST
          case Method.Put    => SMethod.PUT
          case Method.Delete => SMethod.DELETE
          case Method.Patch  => SMethod.PATCH
        },
        Uri.unsafeParse(url)
      )
      .headers(headers)
      .mapResponse(
        _.flatMap(fk(summon[From[Out]]).apply(_).left.map(_.getMessage))
      )

    (body match {
      case body: Body.RawJson =>
        backend.send(
          req
            .body(body.value, "UTF-8")
            .header("Content-Type", "application/json")
        )
      case body: Body.Json[?] =>
        backend.send(
          req
            .body(tk.apply(summon[To[In]]).apply(body.value), "UTF-8")
            .header("Content-Type", "application/json")
        )
      case Body.Multipart(parts) =>
        backend.send(
          req
            .multipartBody(parts.map {
              case MultipartElement.StringElement(name, value) =>
                multipart(name, value)
              case MultipartElement.FileElement(name, value) =>
                multipartFile(name, value)
            })
            .header("Content-Type", "multipart/form-data")
        )
      case Body.Empty => backend.send(req)
    })
      .map(_.body)
      .flatMap {
        case Left(value)  => MonadThrow[F].raiseError(new Exception(value))
        case Right(value) => value.pure[F]
      }
  }
}

object SttpClient {
  type Request = RequestT[Identity, Either[String, String], Any]

  def apply[I[_]: Sync, F[_]: MonadThrow, To[_], From[_]](
    backend: SttpBackend[F, Any]
  )(requestModify: Request => Request = identity)(using tk: To ~> Encode, fk: From ~> Decode): I[SttpClient[F, To, From]] =
    Sync[I].delay(new SttpClient[F, To, From](backend)(requestModify))
}
