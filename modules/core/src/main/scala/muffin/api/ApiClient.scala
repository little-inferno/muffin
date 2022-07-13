package muffin.api

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.all.given
import fs2.*
import muffin.api.ApiClient.params
import muffin.api.channels.*
import muffin.api.dialogs.{Dialog, Dialogs}
import muffin.api.emoji.*
import muffin.api.insights.*
import muffin.api.posts
import muffin.api.posts.*
import muffin.api.preferences.*
import muffin.api.reactions.*
import muffin.api.roles.*
import muffin.api.status.*
import muffin.api.users.*
import muffin.codec.*
import muffin.predef.*
import muffin.http.*

import java.time.*

class ApiClient[
  F[_] : Concurrent,
  R,
  To[_],
  From[_]
](http: HttpClient[F, To, From], cfg: ClientConfig)(codec: CodecSupport[R, To, From])(using ZoneId)
  extends Posts[F]
    with Dialogs[F]
    with Channels[F]
    with Emoji[F]
    with Reactions[F]
    with Users[F]
    with Preferences[F]
    with Status[F]
    with Insights[F]
    with Roles[F] {

  import codec.{given, *}
  import ApiClient.*

  def command(name: String): String =
    cfg.serviceUrl + s"/commands/$name"

  def dialog(name: String): String =
    cfg.serviceUrl + s"/dialogs/$name"

  def actions(name: String): String =
    cfg.serviceUrl + s"/actions/$name"

  private def botId: F[UserId] = userByUsername(cfg.botName).map(_.id)

  def postToDirect(
                    userId: UserId,
                    message: Option[String] = None,
                    props: Option[Props] = None
                  ): F[Post] =
    postToChat(userId :: Nil, message, props)

  def postToChat(
                  userIds: List[UserId],
                  message: Option[String] = None,
                  props: Option[Props] = None
                ): F[Post] =
    for {
      id <- botId
      info <- direct(id :: userIds)
      res <- postToChannel(info.id, message, props)
    } yield res


  def postToChannel(channelId: ChannelId, message: Option[String] = None, props: Option[Props] = None): F[Post] =
    http.request(
      cfg.baseUrl + "/posts",
      Method.Post,
      json
        .field("channel_id", channelId)
        .field("message", message)
        .field("props", props)
        .build,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def createEphemeralPost(userId: UserId, channelId: ChannelId, message: String): F[Post] =
    http.request(
      cfg.baseUrl + "/posts/ephemeral",
      Method.Post,
      json
        .field("user_id", userId)
        .field("channel_id", channelId)
        .field("message", message)
        .build,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def getPost(postId: MessageId): F[Post] =
    http
      .request[Unit, Post](
        cfg.baseUrl + s"posts/$postId",
        Method.Get,
        Body.Empty,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

  def deletePost(postId: MessageId): F[Unit] =
    http
      .request[Unit, Unit](
        cfg.baseUrl + s"posts/$postId",
        Method.Delete,
        Body.Empty,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

  def updatePost(postId: MessageId,
                 message: Option[String] = None,
                 props: Option[Props] = None): F[Post] = {
    http
      .request(
        cfg.baseUrl + s"/posts/$postId",
        Method.Put,
        json
          .field("id", postId)
          .field("message", message)
          .field("props", props)
          .build,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )
  }

  def patchPost(postId: MessageId,
                message: Option[String] = None,
                props: Option[Props] = None
               ): F[Post] =
    http
      .request(
        cfg.baseUrl + s"/posts/$postId/patch",
        Method.Put,
        json
          .field("message", message)
          .field("props", props)
          .build,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

  def getPostsByIds(messageId: List[MessageId]): F[List[Post]] =
    http
      .request(
        cfg.baseUrl + s"/posts/ids",
        Method.Put,
        Body.Json(messageId),
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )


  ////////////////////////////////////////////////
  def openDialog(triggerId: String, url: String, dialog: Dialog): F[Unit] =
    http
      .request(
        cfg.baseUrl + "/actions/dialogs/open",
        Method.Post,
        json
          .field("trigger_id", triggerId)
          .field("url", url)
          .field("dialog", dialog)
          .build,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

  def members(channelId: ChannelId): Stream[F, ChannelMember] = {
    def single(page: Int): F[List[ChannelMember]] =
      http.request[Unit, List[ChannelMember]](
        cfg.baseUrl + s"/channels/$channelId/members?page=$page",
        Method.Get,
        Body.Empty,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

    Stream
      .unfoldEval(0) { page =>
        single(page)
          .map(list => if (list.isEmpty) None else Some((list, page + 1)))
      }
      .flatMap(Stream.emits)
  }

  def direct(userIds: List[UserId]): F[ChannelInfo] =
    http.request(
      cfg.baseUrl + "/channels/direct",
      Method.Post,
      Body.Json(userIds),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def getChannelByName(teamId: TeamId, name: String): F[ChannelInfo] = {
    http.request[Unit, ChannelInfo](
      cfg.baseUrl + s"/teams/$teamId/channels/name/$name",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )
  }

  def createEmoji(req: CreateEmojiRequest): F[EmojiInfo] =
    http.request[Unit, EmojiInfo](
      cfg.baseUrl + s"/emoji",
      Method.Post,
      Body.Multipart(
        MultipartElement.FileElement("image", req.image) ::
          MultipartElement.StringElement(
            "emoji",
            json
              .field("creator_id", req.creatorId)
              .field("name", req.emojiName)
              .build.value
          ) :: Nil
      ),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def getEmojis(sorting: EmojiSorting = EmojiSorting.None): Stream[F, EmojiInfo] = {
    def single(page: Int) =
      http.request[Unit, List[EmojiInfo]](
        cfg.baseUrl + s"/emoji?page=$page&sort=$sorting",
        Method.Get,
        Body.Empty,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

    Stream
      .unfoldEval(0) { page =>
        single(page)
          .map(list => if (list.isEmpty) None else Some((list, page + 1)))
      }
      .flatMap(Stream.emits)
  }

  def getEmoji(emojiId: EmojiId): F[EmojiInfo] =
    http.request[Unit, EmojiInfo](
      cfg.baseUrl + s"/emoji/$emojiId",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def deleteEmoji(emojiId: EmojiId): F[EmojiInfo] =
    http.request[Unit, EmojiInfo](
      cfg.baseUrl + s"/emoji/name/$emojiId",
      Method.Delete,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def getEmojiByName(name: String): F[EmojiInfo] =
    http.request[Unit, EmojiInfo](
      cfg.baseUrl + s"/emoji/name/$name",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def searchEmoji(term: String, prefixOnly: Option[String]): F[List[EmojiInfo]] =
    http.request(
      cfg.baseUrl + s"/emoji/search",
      Method.Post,
      json
        .field("term", term)
        .field("prefix_only", prefixOnly)
        .build,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def autocompleteEmoji(
                         name: String
                       ): F[List[EmojiInfo]] =
    http.request[Unit, List[EmojiInfo]](
      cfg.baseUrl + s"/emoji/autocomplete?name=$name",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def createReaction(userId: UserId,
                     postId: MessageId,
                     emojiName: String): F[ReactionInfo] =
    http.request(
      cfg.baseUrl + "/reactions",
      Method.Post,
      json
        .field("user_id", userId)
        .field("post_id", postId)
        .field("emoji_name", emojiName)
        .build,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def getReactions(postId: MessageId): F[List[ReactionInfo]] =
    http.request[Unit, List[ReactionInfo]](
      cfg.baseUrl + s"/posts/$postId/reactions",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def removeReaction(userId: UserId,
                     postId: MessageId,
                     emojiName: String): F[Unit] =
    http
      .request[Unit, Unit](
        cfg.baseUrl + s"/users/$userId/posts/$postId/reactions/$emojiName",
        Method.Delete,
        Body.Empty,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

  def bulkReactions(messages: List[MessageId]): F[Map[String, List[ReactionInfo]]] =
    http.request(
      cfg.baseUrl + s"/posts/ids/reactions",
      Method.Post,
      Body.Json(messages),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def users(options: GetUserOptions): Stream[F, User] = {
    def single(page: Int): F[List[User]] =
      http.request[Unit, List[User]](
        cfg.baseUrl + s"/users${
          params(
            "page" -> page.toString.some,
            "in_team" -> options.inTeam(_.toString),
            "not_in_team" -> options.notInTeam(_.toString),
            "in_channel" -> options.inChannel(_.toString),
            "not_in_channel" -> options.notInChannel(_.toString),
            "active" -> options.active(_.toString),
          )
        }",
        Method.Get,
        Body.Empty,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

    Stream
      .unfoldEval(0) { page =>
        single(page)
          .map(list => if (list.isEmpty) None else Some((list, page + 1)))
      }
      .flatMap(Stream.emits)
  }


  def usersById(userIds: List[UserId]): F[List[User]] =
    http.request(
      cfg.baseUrl + s"/users/ids",
      Method.Post,
      Body.Json(userIds),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def usersByUsername(users: List[String]): F[List[User]] =
    http.request(
      cfg.baseUrl + s"/users/usernames",
      Method.Post,
      Body.Json(users),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def userByUsername(user: String): F[User] =
    http.request[Unit, User](
      cfg.baseUrl + s"/users/username/$user",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def user(userId: UserId): F[User] =
    http.request[Unit, User](
      cfg.baseUrl + s"/users/$userId",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )


  //  Preferences
  def getUserPreferences(userId: UserId): F[List[Preference]] =
    http.request[Unit, List[Preference]](
      cfg.baseUrl + s"/users/$userId/preferences",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def getUserPreferences(userId: UserId, category: String): F[List[Preference]] =
    http.request[Unit, List[Preference]](
      cfg.baseUrl + s"/users/$userId/preferences/${category}",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def getUserPreference(userId: UserId, category: String, name: String): F[Preference] =
    http.request[Unit, Preference](
      cfg.baseUrl + s"/users/$userId/preferences/${category}/name/${name}",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def saveUserPreference(userId: UserId, preferences: List[Preference]): F[Unit] =
    http.request(
      cfg.baseUrl + s"/users/$userId/preferences",
      Method.Put,
      Body.Json(preferences),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def deleteUserPreference(userId: UserId, preferences: List[Preference]): F[Unit] = {
    http.request(
      cfg.baseUrl + s"/users/$userId/preferences/delete",
      Method.Post,
      Body.Json(preferences),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )
  }
  //  Preferences


  // Status
  def getUserStatus(userId: UserId): F[UserStatus] = {
    http.request[Unit, UserStatus](
      cfg.baseUrl + s"/users/$userId/status",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )
  }

  def getUserStatuses(users: List[UserId]): F[List[UserStatus]] = {
    http.request(
      cfg.baseUrl + s"/users/status/ids",
      Method.Post,
      Body.Json(users),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )
  }

  def updateUserStatus(userId: UserId, statusUser: StatusUser): F[Unit] =
    http.request(
      cfg.baseUrl + s"/users/$userId/status/custom",
      Method.Put,
      Body.Json(UpdateUserStatusRequest(userId, statusUser)),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def updateCustomStatus(userId: UserId, customStatus: CustomStatus): F[Unit] = {
    http.request(
      cfg.baseUrl + s"/users/$userId/status/custom",
      Method.Put,
      Body.Json(customStatus),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )
  }

  def unsetCustomStatus(userId: UserId): F[Unit] = {
    http.request[Unit, Unit](
      cfg.baseUrl + s"/users/$userId/status/custom",
      Method.Delete,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )
  }
  // Status


  // Insights
  def getTopReactions(teamId: TeamId, timeRange: TimeRange): Stream[F, ReactionInsight] = {
    def single(page: Int) =
      http.request[Unit, ListWrapper[ReactionInsight]](
        cfg.baseUrl + s"/teams/$teamId/top/reactions?time_range=$timeRange&page=$page",
        Method.Get,
        Body.Empty,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

    Stream
      .unfoldEval(0) { page =>
        single(page).map(lw =>
          if (lw.hasNext) Some(lw.items -> (page + 1)) else None
        )
      }
      .flatMap(Stream.emits)
  }

  def getTopReactions(userId: UserId, timeRange: TimeRange, teamId: Option[TeamId]): Stream[F, ReactionInsight] = {
    def single(page: Int) =
      http.request[Unit, ListWrapper[ReactionInsight]](
        cfg.baseUrl + s"/users/$userId/top/reactions?time_range=$timeRange&page=$page${teamId.map(id => s"&team_id=$id")}",
        Method.Get,
        Body.Empty,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

    Stream
      .unfoldEval(0) { page =>
        single(page).map(lw =>
          if (lw.hasNext) Some(lw.items -> (page + 1)) else None
        )
      }
      .flatMap(Stream.emits)
  }

  def getTopChannels(teamId: TeamId, timeRange: TimeRange): Stream[F, ChannelInsight] = {
    def single(page: Int) =
      http.request[Unit, ListWrapper[ChannelInsight]](
        cfg.baseUrl + s"/teams/$teamId/top/channels?time_range=$timeRange&page=$page",
        Method.Get,
        Body.Empty,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

    Stream
      .unfoldEval(0) { page =>
        single(page).map(lw =>
          if (lw.hasNext) Some(lw.items -> (page + 1)) else None
        )
      }
      .flatMap(Stream.emits)
  }

  def getTopChannels(userId: UserId, timeRange: TimeRange, teamId: Option[TeamId]): Stream[F, ChannelInsight] = {
    def single(page: Int) =
      http.request[Unit, ListWrapper[ChannelInsight]](
        cfg.baseUrl + s"/users/$userId/top/channels?time_range=$timeRange&page=$page${teamId.map(id => s"&team_id=$id")}",
        Method.Get,
        Body.Empty,
        Map("Authorization" -> s"Bearer ${cfg.auth}")
      )

    Stream
      .unfoldEval(0) { page =>
        single(page).map(lw =>
          if (lw.hasNext) Some(lw.items -> (page + 1)) else None
        )
      }
      .flatMap(Stream.emits)
  }
  // Insights

  //  Roles
  def getAllRoles: F[List[RoleInfo]] =
    http.request[Unit, List[RoleInfo]](
      cfg.baseUrl + s"/roles",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def getRoleById(id: String): F[RoleInfo] =
    http.request[Unit, RoleInfo](
      cfg.baseUrl + s"/roles/$id",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def getRoleByName(name: String): F[RoleInfo] =
    http.request[Unit, RoleInfo](
      cfg.baseUrl + s"/roles/name/$name",
      Method.Get,
      Body.Empty,
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def updateRole(id: String, permissions: List[String]): F[RoleInfo] =
    http.request(
      cfg.baseUrl + s"/roles/$id/patch",
      Method.Patch,
      Body.Json(permissions),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )

  def getRoles(names: List[String]): F[List[RoleInfo]] =
    http.request(
      cfg.baseUrl + s"/roles/names",
      Method.Post,
      Body.Json(names),
      Map("Authorization" -> s"Bearer ${cfg.auth}")
    )
  //  Roles
}

object ApiClient {
  private def params(params: (String, Option[String])*): String = {
    params.toMap.map(p => s"${p._1}=${p._2}").mkString("?", "&", "")
  }
}
