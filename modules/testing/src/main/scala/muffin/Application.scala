package muffin

import cats.MonadThrow
import io.circe.{Json, JsonObject}
import io.circe.{Codec, Json, JsonObject}
import muffin.api.channels.ChannelMember
import muffin.api.emoji.*
import muffin.api.posts.*
import muffin.predef.*
import muffin.api.reactions.*

import java.time.ZoneId
//import muffin.CirceSttpClient
import muffin.api.dialogs.*
import sttp.client3.httpclient.zio.HttpClientZioBackend
import cats.syntax.all.given
import muffin.api.ClientConfig
import muffin.codec.RawDecode
import muffin.input.*
import muffin.interop.http.SttpClient
import zio.interop.catz.given

import java.io.File
import zio.interop.catz.implicits.given
import zio.*
//
//case class A(str: String) derives Codec.AsObject
//
//class HandlerA[F[_] : MonadThrow](client: CirceSttpClient.Api[F]) {
//  def kekA(action: CommandContext): F[AppResponse] =
//    client
//      .openDialog(
//        action.triggerId,
//        client.dialog("superdialog"),
//        Dialog(
//          "id",
//          "title",
//          "intoduction",
//          List(Element.Text("display", "name")),
//          Some("submit submit"),
//          true,
//          "State"
//        )
//      )
//      .map(_ => AppResponse.Ok())
//
//  def actionA(dialog: MessageAction[A]): F[AppResponse] =
//    client
//      .openDialog(
//        dialog.triggerId,
//        client.dialog("superdialog"),
//        Dialog(
//          "id",
//          "title",
//          "intoduction",
//          List(Element.Text("display", "name")),
//          Some("submit submit"),
//          true,
//          "State"
//        )
//      )
//      .map(_ => AppResponse.Ok())
//}
//
//class HandlerB[F[_] : MonadThrow](client: CirceSttpClient.Api[F]) {
//  def kekB(action: CommandContext): F[AppResponse] =
//    client
//      .openDialog(
//        action.triggerId,
//        client.dialog("superdialog"),
//        Dialog(
//          "id",
//          "title",
//          "intoduction",
//          List(Element.Text("display", "name")),
//          Some("submit submit"),
//          true,
//          "State"
//        )
//      )
//      .map(_ => AppResponse.Ok())
//
//  def actionB(dialog: MessageAction[A]): F[AppResponse] =
//    client
//      .openDialog(
//        dialog.triggerId,
//        client.dialog("superdialog"),
//        Dialog(
//          "id",
//          "title",
//          "intoduction",
//          List(Element.Text("display", "name")),
//          Some("submit submit"),
//          true,
//          "State"
//        )
//      )
//      .map(_ => AppResponse.Ok())
//}
//
//object Application extends ZIOAppDefault {
//
//  import muffin.interop.circe.codec.given
//
//  val token = "ayxxty8s1jy6mcsnsp9octpqqe"
//
//  val run =
//    for {
//      app: CirceSttpClient.Api[Task] <- CirceSttpClient(
//        ClientConfig(
//          "http://localhost:8065/api/v4",
//          token,
//          "name",
//          "http://host.docker.internal:8080"
//        )
//      )
//
//      router <- {
//        given HandlerA[Task] = new HandlerA[Task](app)
//
//        given HandlerB[Task] = new HandlerB[Task](app)
//
//        RouterBuilder[Task, Json]
//          .command[HandlerA[Task], "kekA"]
//          .action[HandlerA[Task], A, "actionA"]
//          .action[HandlerB[Task], A, "actionB"]
//          .unexpected((name, action) => ZIO.succeed(AppResponse.Ok()))
//          .build[Task]
//      }
//
//      //      _ <- DefaultServer(router).useForever
//    } yield ()
//
//}



object Application extends scala.App {

  import muffin.interop.circe.codec.given

  given ZoneId = ZoneId.systemDefault()

  val text = "[\n    {\n        \"channel_id\": \"6bwnogwiktyxmxhyobs9uj3n1y\",\n        \"user_id\": \"bbjjkoayk3fnzr1mewbazaippr\",\n        \"roles\": \"channel_user channel_admin\",\n        \"last_viewed_at\": 1657619978058,\n        \"msg_count\": 16,\n        \"mention_count\": 0,\n        \"mention_count_root\": 0,\n        \"msg_count_root\": 16,\n        \"notify_props\": {\n            \"desktop\": \"default\",\n            \"email\": \"default\",\n            \"ignore_channel_mentions\": \"default\",\n            \"mark_unread\": \"all\",\n            \"push\": \"default\"\n        },\n        \"last_update_at\": 1657619978058,\n        \"scheme_guest\": false,\n        \"scheme_user\": true,\n        \"scheme_admin\": true,\n        \"explicit_roles\": \"\"\n    },\n    {\n        \"channel_id\": \"6bwnogwiktyxmxhyobs9uj3n1y\",\n        \"user_id\": \"j87f1od3bpftz8pm14rc8xok6y\",\n        \"roles\": \"channel_user\",\n        \"last_viewed_at\": 0,\n        \"msg_count\": 0,\n        \"mention_count\": 0,\n        \"mention_count_root\": 0,\n        \"msg_count_root\": 0,\n        \"notify_props\": {\n            \"desktop\": \"default\",\n            \"email\": \"default\",\n            \"ignore_channel_mentions\": \"default\",\n            \"mark_unread\": \"all\",\n            \"push\": \"default\"\n        },\n        \"last_update_at\": 1654628299583,\n        \"scheme_guest\": false,\n        \"scheme_user\": true,\n        \"scheme_admin\": false,\n        \"explicit_roles\": \"\"\n    }\n]"

  print(io.circe.parser.parse(text).map(_.as[List[ChannelMember]]))



}
