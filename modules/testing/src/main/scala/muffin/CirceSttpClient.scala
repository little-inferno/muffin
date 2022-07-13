package muffin

import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio.Task
import zio.interop.catz.given
import muffin.interop.circe.codec
import muffin.interop.circe.codec.given
import muffin.api.{ApiClient, ClientConfig}
import muffin.codec.Encode
import muffin.http.HttpClient
import muffin.interop.http.SttpClient
import io.circe.{Json, Encoder, Decoder}

import java.time.ZoneId
import zio.*
import zio.interop.catz.given
import zio.interop.catz.implicits.given


//object CirceSttpClient {
//  type Api[F[_]] = ApiClient[F, Json, Encoder, Decoder]
//
//  def apply(cfg: ClientConfig): Task[Api[Task]] =
//    for {
//      backend <- HttpClientZioBackend()
//      client <- SttpClient[Task, Task, Encoder, Decoder](backend)
//      given ZoneId <- ZIO.succeed(ZoneId.systemDefault())
//    } yield new ApiClient[Task, Json, Encoder, Decoder](client, cfg)(codec)
//}
