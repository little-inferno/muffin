package muffin.interop.circe

import cats.arrow.FunctionK
import cats.~>
import muffin.codec.*
import io.circe.*
import io.circe.parser.*
import muffin.api.preferences.*
import muffin.api.status.*
import muffin.predef.*
import muffin.api.insights.*
import muffin.api.insights.*
import muffin.api.preferences.*
import muffin.api.roles.*
import muffin.api.status.*
import muffin.api.channels.*
import muffin.api.emoji.*
import cats.syntax.all.given
import Decoder.Result
import muffin.api.dialogs.*
import muffin.api.posts.*
import muffin.api.users.*
import muffin.api.reactions.*

import java.time.*
import syntax.given
import muffin.http.Body
import muffin.input.{
  AppResponse,
  CommandContext,
  DialogSubmissionValue,
  RawAction,
  ResponseType
}

object codec extends CodecSupport[Json, Encoder, Decoder] {
  given RawFrom[T: Decoder]: RawDecode[Json, T] = (from: Json) => from.as[T]

  given EncoderTo: FunctionK[Encoder, Encode] = new FunctionK[Encoder, Encode] {
    def apply[A](fa: Encoder[A]): Encode[A] = (obj: A) =>
      fa.apply(obj).dropNullValues.noSpaces
  }

  given DecoderFrom: FunctionK[Decoder, Decode] =
    new FunctionK[Decoder, Decode] {
      def apply[A](fa: Decoder[A]): Decode[A] = (str: String) =>
        parse(str).flatMap(_.as(fa))
    }

  given EncodeTo[A: Encoder]: Encode[A] =
    Encoder[A].apply(_).dropNullValues.noSpaces

  given DecodeFrom[A: Decoder]: Decode[A] = (str: String) =>
    parse(str).flatMap(_.as[A])

  given UnitTo: Encoder[Unit] = Encoder.encodeUnit

  given UnitFrom: Decoder[Unit] = _ => Right(())

  given RTo: Encoder[Json] = Encoder.encodeJson

  given StringTo: Encoder[String] = Encoder.encodeString.apply(_)

  given ListTo[A: Encoder]: Encoder[List[A]] = Encoder.encodeList[A]

  given ListFrom[A: Decoder]: Decoder[List[A]] = Decoder.decodeList[A]

  given MapFrom[A: Decoder]: Decoder[Map[String, A]] =
    Decoder.decodeMap[String, A]

  given LoginTo: Encoder[Login] = Encoder.encodeString.contramap(_.toString)

  given UserIdTo: Encoder[UserId] = Encoder.encodeString.contramap(_.toString)

  given GroupIdTo: Encoder[GroupId] = Encoder.encodeString.contramap(_.toString)

  given TeamIdTo: Encoder[TeamId] = Encoder.encodeString.contramap(_.toString)

  given ChannelIdTo: Encoder[ChannelId] =
    Encoder.encodeString.contramap(_.toString)

  given MessageIdTo: Encoder[MessageId] =
    Encoder.encodeString.contramap(_.toString)

  given EmojiIdTo: Encoder[EmojiId] = Encoder.encodeString.contramap(_.toString)

  given LoginFrom: Decoder[Login] = Decoder.decodeString.map(Login(_))

  given UserIdFrom: Decoder[UserId] = Decoder.decodeString.map(UserId(_))

  given GroupIdFrom: Decoder[GroupId] = Decoder.decodeString.map(GroupId(_))

  given TeamIdFrom: Decoder[TeamId] = Decoder.decodeString.map(TeamId(_))

  given ChannelIdFrom: Decoder[ChannelId] =
    Decoder.decodeString.map(ChannelId(_))

  given MessageIdFrom: Decoder[MessageId] =
    Decoder.decodeString.map(MessageId(_))

  given EmojiIdFrom: Decoder[EmojiId] = Decoder.decodeString.map(EmojiId(_))

  given PreferenceEncode: Encoder[Preference] = p =>
    Json.obj(
      "user_id" -> Json.fromString(p.userId.toString),
      "category" -> Json.fromString(p.category),
      "name" -> Json.fromString(p.name),
      "value" -> Json.fromString(p.value)
    )

  given PreferenceDecode: Decoder[Preference] = (c: HCursor) =>
    for {
      userId <- c.downField("user_id").as[UserId]
      category <- c.downField("category").as[String]
      name <- c.downField("name").as[String]
      value <- c.downField("value").as[String]
    } yield Preference(userId, category, name, value)

  given StatusUserEncode: Encoder[StatusUser] = {
    case StatusUser.Online  => Json.fromString("online")
    case StatusUser.Offline => Json.fromString("offline")
    case StatusUser.Away    => Json.fromString("away")
    case StatusUser.Dnd(_)  => Json.fromString("dnd")
  }

  given StatusUserDecode(using
    time: Option[LocalDateTime]
  ): Decoder[StatusUser] = (c: HCursor) =>
    c.as[String].map(_ -> time).flatMap {
      case ("online", _)       => Right(StatusUser.Online)
      case ("offline", _)      => Right(StatusUser.Offline)
      case ("away", _)         => Right(StatusUser.Away)
      case ("dnd", Some(time)) => Right(StatusUser.Dnd(time))
      case status =>
        Left(DecodingFailure(s"Invalid custom status $status", c.history))
    }

  given UserStatusEncode(using zone: ZoneId): Encoder[UserStatus] = us =>
    Json.obj(
      List(
        ("user_id" -> Json.fromString(us.userId.toString)).some,
        ("status" -> Encoder[StatusUser].apply(us.status)).some,
        ("last_activity_at" -> Json
          .fromLong(us.lastActivityAt.atZone(zone).toEpochSecond)).some,
        us.activeChannel.map(a =>
          "active_channel" -> Json.fromString(a.toString)
        ),
        us.status match {
          case StatusUser.Dnd(time) =>
            ("dnd_end_time" -> Json.fromLong(
              time.atZone(zone).toEpochSecond
            )).some
          case _ => None
        }
      ).flatten*
    )

  given UserStatusDecode(using zone: ZoneId): Decoder[UserStatus] =
    (c: HCursor) =>
      for {
        userId <- c.downField("user_id").as[UserId]
        manual <- c.downField("manual").as[Boolean]
        lastActivityAt <- c
          .downField("last_activity_at")
          .as[Long]
          .map(Instant.ofEpochSecond)
          .map(LocalDateTime.ofInstant(_, zone))
        activeChannel <- c.downField("active_channel").as[Option[ChannelId]]
        given Option[LocalDateTime] <- c
          .downField("dnd_end_time")
          .as[Option[Long]]
          .map(
            _.map(Instant.ofEpochSecond).map(LocalDateTime.ofInstant(_, zone))
          )
        status <- c.downField("status").as[StatusUser]
      } yield UserStatus(userId, status, manual, lastActivityAt, activeChannel)

  given CustomStatusDurationEncode: Encoder[CustomStatusDuration] = {
    case CustomStatusDuration.ThirtyMinutes => Json.fromString("thirty_minutes")
    case CustomStatusDuration.OneHour       => Json.fromString("one_hour")
    case CustomStatusDuration.FourHours     => Json.fromString("four_hours")
    case CustomStatusDuration.Today         => Json.fromString("today")
    case CustomStatusDuration.ThisWeek      => Json.fromString("this_week")
    case CustomStatusDuration.DateAndTime(_) => Json.fromString("date_and_time")
  }

  given CustomStatusDurationDecode(using
    time: Option[LocalDateTime]
  ): Decoder[CustomStatusDuration] = (c: HCursor) =>
    c.as[String].map(_ -> time).flatMap {
      case ("thirty_minutes", _) => Right(CustomStatusDuration.ThirtyMinutes)
      case ("one_hour", _)       => Right(CustomStatusDuration.OneHour)
      case ("four_hours", _)     => Right(CustomStatusDuration.FourHours)
      case ("today", _)          => Right(CustomStatusDuration.Today)
      case ("this_week", _)      => Right(CustomStatusDuration.ThisWeek)
      case ("date_and_time", Some(time)) =>
        Right(CustomStatusDuration.DateAndTime(time))
      case status =>
        Left(DecodingFailure(s"Invalid custom status $status", c.history))
    }

  given CustomStatusEncode(using zone: ZoneId): Encoder[CustomStatus] = cs =>
    Json.obj(
      List(
        ("emoji" -> Json.fromString(cs.emoji)).some,
        ("text" -> Json.fromString(cs.text.getOrElse(""))).some,
        cs.duration.map(d =>
          "duration" -> Encoder[CustomStatusDuration].apply(d)
        ),
        cs.duration match {
          case Some(CustomStatusDuration.DateAndTime(time)) =>
            ("expires_at" -> Json.fromLong(
              time.atZone(zone).toEpochSecond
            )).some
          case _ => None
        }
      ).flatten*
    )

  given CustomStatusDecode(using zone: ZoneId): Decoder[CustomStatus] =
    (c: HCursor) =>
      for {
        emoji <- c.downField("emoji").as[String]
        text <- c
          .downField("text")
          .as[String]
          .map(s =>
            if (s.isBlank)
              None
            else
              s.some
          )
        given Option[LocalDateTime] <- c
          .downField("expires_at")
          .as[Option[Long]]
          .map(
            _.map(Instant.ofEpochSecond).map(LocalDateTime.ofInstant(_, zone))
          )
        duration <- c.downField("duration").as[Option[CustomStatusDuration]]
      } yield CustomStatus(emoji, text, duration)

  given UpdateUserStatusRequestEncode(using
    zone: ZoneId
  ): Encoder[UpdateUserStatusRequest] = uusr =>
    Json.obj(
      List(
        ("user_id" -> Json.fromString(uusr.userId.toString)).some,
        ("status" -> Encoder[StatusUser].apply(uusr.status)).some,
        uusr.status match
          case StatusUser.Dnd(time) =>
            ("dnd_end_time" -> Json.fromLong(
              time.atZone(zone).toEpochSecond
            )).some
          case _ => None
      ).flatten*
    )

  given ReactionInsightDecode: Decoder[ReactionInsight] = (c: HCursor) =>
    for {
      emojiName <- c.downField("emoji_name").as[String]
      count <- c.downField("count").as[Long]
    } yield ReactionInsight(emojiName, count)

  given ChannelInsightDecode: Decoder[ChannelInsight] = (c: HCursor) =>
    for {
      id <- c.downField("id").as[ChannelId]
      channelType <- c.downField("type").as[String]
      name: String <- c.downField("name").as[String]
      teamId <- c.downField("team_id").as[TeamId]
      messageCount <- c.downField("message_count").as[Long]
    } yield ChannelInsight(id, channelType, name, teamId, messageCount)

  given ListWrapperDecode[T: Decoder]: Decoder[ListWrapper[T]] = (c: HCursor) =>
    for {
      hasNext <- c.downField("has_next").as[Boolean]
      items <- c.downField("items").as[List[T]]
    } yield ListWrapper(hasNext, items)

  given RoleInfoDecode: Decoder[RoleInfo] = (c: HCursor) =>
    for {
      id <- c.downField("id").as[String]
      name <- c.downField("name").as[String]
      displayName <- c.downField("display_name").as[String]
      description <- c.downField("description").as[String]
      permissions <- c.downField("permissions").as[List[String]]
      schemeManaged <- c.downField("scheme_managed").as[Boolean]
    } yield RoleInfo(
      id,
      name,
      displayName,
      description,
      permissions,
      schemeManaged
    )

  given ChannelInfoFrom(using zone: ZoneId): Decoder[ChannelInfo] =
    (c: HCursor) =>
      for {
        id <- c.downField("id").as[ChannelId]
        createAt <- c
          .downField("create_at")
          .as[Long]
          .map(Instant.ofEpochSecond)
          .map(LocalDateTime.ofInstant(_, zone))
        updateAt <- c
          .downField("update_at")
          .as[Long]
          .map(Instant.ofEpochSecond)
          .map(LocalDateTime.ofInstant(_, zone))
        deleteAt = c
          .downField("delete_at")
          .as[Option[Long]]
          .toOption
          .flatten
          .map(Instant.ofEpochSecond)
          .map(LocalDateTime.ofInstant(_, zone))
        teamId <- c.downField("team_id").as[TeamId]
        channelType <- c.downField("type").as[String]
        displayName <- c.downField("display_name").as[String]
        name <- c.downField("name").as[String]
        header <- c.downField("header").as[String]
        purpose <- c.downField("purpose").as[String]
        lastPostAt = c
          .downField("last_post_at")
          .as[Long]
          .toOption
          .map(Instant.ofEpochSecond)
          .map(LocalDateTime.ofInstant(_, zone))
        totalMsgCount <- c.downField("total_msg_count").as[Int]
        creatorId <- c.downField("creator_id").as[UserId]
      } yield ChannelInfo(
        id,
        createAt,
        updateAt,
        deleteAt,
        teamId,
        channelType,
        displayName,
        name,
        header,
        purpose,
        lastPostAt,
        totalMsgCount,
        creatorId
      )

  given ChannelMemberFrom(using zone: ZoneId): Decoder[ChannelMember] =
    (c: HCursor) =>
      for {
        channelId <- c.downField("channel_id").as[ChannelId]
        userId <- c.downField("user_id").as[UserId]
        roles <- c.downField("roles").as[String]
        lastViewedAt <- c
          .downField("last_viewed_at")
          .as[Long]
          .map(Instant.ofEpochSecond)
          .map(LocalDateTime.ofInstant(_, zone))
        msgCount = c.downField("msg_count").as[Int].toOption
        mentionCount = c.downField("mention_count").as[Int].toOption
        notifyProps <- c.downField("notify_props").as[NotifyProps]
        lastUpdateAt = c
          .downField("last_viewed_at")
          .as[Option[Long]]
          .toOption
          .flatten
          .map(Instant.ofEpochSecond)
          .map(LocalDateTime.ofInstant(_, zone))
        teamDisplayName = c.downField("team_display_name").as[String].toOption
        teamName = c.downField("team_name").as[String].toOption
        teamUpdateAt = c.downField("team_update_at").as[String].toOption
      } yield ChannelMember(
        channelId,
        userId,
        roles,
        lastViewedAt,
        msgCount,
        mentionCount,
        notifyProps,
        lastUpdateAt,
        teamDisplayName,
        teamName,
        teamUpdateAt
      )

  given DialogTo: Encoder[muffin.api.dialogs.Dialog] = dialog =>
    Json.obj(
      List(
        ("callback_id" -> Json.fromString(dialog.callbackId)).some,
        ("title" -> Json.fromString(dialog.title)).some,
        ("introduction_text" -> Json.fromString(dialog.introductionText)).some,
        dialog.submitLabel.map("submit_label" -> Json.fromString(_)),
        ("notify_on_cancel" -> Json.fromBoolean(dialog.notifyOnCancel)).some,
        ("state" -> Json.fromString(dialog.state)).some,
        ("elements" -> Encoder.encodeList[Element].apply(dialog.elements)).some
      ).flatten*
    )

  given DialogElementTo: Encoder[Element] = {
    case value: Element.Text =>
      Json.obj(
        List(
          ("display_name" -> Json.fromString(value.displayName)).some,
          ("name" -> Json.fromString(value.name)).some,
          ("subtype" -> Encoder[TextSubtype].apply(value.subtype)).some,
          ("optional" -> Json.fromBoolean(value.optional)).some,
          value.minLength.map(l => ("min_length" -> Json.fromInt(l))),
          value.maxLength.map(l => ("max_length" -> Json.fromInt(l))),
          value.helpText.map(l => ("help_text" -> Json.fromString(l))),
          value.default.map(l => ("default" -> Json.fromString(l))),
          ("type" -> Json.fromString("text")).some
        ).flatten*
      )
    case value: Element.Textarea =>
      Json.obj(
        List(
          ("display_name" -> Json.fromString(value.displayName)).some,
          ("name" -> Json.fromString(value.name)).some,
          ("subtype" -> Encoder[TextSubtype].apply(value.subtype)).some,
          ("optional" -> Json.fromBoolean(value.optional)).some,
          value.minLength.map(l => ("min_length" -> Json.fromInt(l))),
          value.maxLength.map(l => ("max_length" -> Json.fromInt(l))),
          value.helpText.map(l => ("help_text" -> Json.fromString(l))),
          value.default.map(l => ("default" -> Json.fromString(l))),
          ("type" -> Json.fromString("textarea")).some
        ).flatten*
      )
    case value: Element.Select =>
      Json.obj(
        List(
          ("display_name" -> Json.fromString(value.displayName)).some,
          ("name" -> Json.fromString(value.name)).some,
          value.dataSource.map(l =>
            ("data_source" -> Encoder[DataSource].apply(l))
          ),
          ("options" -> Encoder
            .encodeList[SelectOption]
            .apply(value.options)).some,
          ("optional" -> Json.fromBoolean(value.optional)).some,
          value.helpText.map(l => ("help_text" -> Json.fromString(l))),
          value.default.map(l => ("default" -> Json.fromString(l))),
          value.placeholder.map(l => ("placeholder" -> Json.fromString(l))),
          ("type" -> Json.fromString("select")).some
        ).flatten*
      )
    case value: Element.Checkbox =>
      Json.obj(
        List(
          ("display_name" -> Json.fromString(value.displayName)).some,
          ("name" -> Json.fromString(value.name)).some,
          ("optional" -> Json.fromBoolean(value.optional)).some,
          value.helpText.map(l => ("help_text" -> Json.fromString(l))),
          value.default.map(l => ("default" -> Json.fromBoolean(l))),
          value.placeholder.map(l => ("placeholder" -> Json.fromString(l))),
          ("type" -> Json.fromString("bool")).some
        ).flatten*
      )
    case value: Element.Radio =>
      Json.obj(
        List(
          ("display_name" -> Json.fromString(value.displayName)).some,
          ("name" -> Json.fromString(value.name)).some,
          ("options" -> Encoder
            .encodeList[SelectOption]
            .apply(value.options)).some,
          value.helpText.map(l => ("help_text" -> Json.fromString(l))),
          value.default.map(l => ("default" -> Json.fromBoolean(l))),
          ("type" -> Json.fromString("radio")).some
        ).flatten*
      )
  }

  given DataSourceTo: Encoder[DataSource] = {
    case DataSource.Users    => Json.fromString("users")
    case DataSource.Channels => Json.fromString("channels")
  }

  given SelectOptionTo: Encoder[SelectOption] = select =>
    Json.obj(
      "text" -> Json.fromString(select.text),
      "value" -> Json.fromString(select.value)
    )

  given TextSubtypeTo: Encoder[TextSubtype] = {
    case TextSubtype.Text     => Encoder.encodeString("text")
    case TextSubtype.Email    => Encoder.encodeString("email")
    case TextSubtype.Number   => Encoder.encodeString("number")
    case TextSubtype.Password => Encoder.encodeString("password")
    case TextSubtype.Tel      => Encoder.encodeString("tel")
    case TextSubtype.Url      => Encoder.encodeString("url")
  }

  given ReactionInfoFrom(using zone: ZoneId): Decoder[ReactionInfo] =
    (c: HCursor) =>
      for {
        userId <- c.downField("user_id").as[UserId]
        postId <- c.downField("post_id").as[MessageId]
        emojiName <- c.downField("emoji_name").as[String]
        createAt <- c
          .downField("create_at")
          .as[Long]
          .map(Instant.ofEpochSecond)
          .map(LocalDateTime.ofInstant(_, zone))
      } yield ReactionInfo(userId, postId, emojiName, createAt)

  given EmojiInfoFrom(using zone: ZoneId): Decoder[EmojiInfo] = (c: HCursor) =>
    for {
      id <- c.downField("id").as[EmojiId]
      creatorId <- c.downField("creator_id").as[UserId]
      name <- c.downField("name").as[String]
      createAt <- c
        .downField("create_at")
        .as[Long]
        .map(Instant.ofEpochSecond)
        .map(LocalDateTime.ofInstant(_, zone))
      updateAt <- c
        .downField("update_at")
        .as[Long]
        .map(Instant.ofEpochSecond)
        .map(LocalDateTime.ofInstant(_, zone))
      deleteAt = c
        .downField("delete_at")
        .as[Long]
        .toOption
        .map(Instant.ofEpochSecond)
        .map(LocalDateTime.ofInstant(_, zone))
    } yield EmojiInfo(id, creatorId, name, createAt, updateAt, deleteAt)

  given NotifyOptionFrom: Decoder[NotifyOption] =
    _.as[String].map(a => NotifyOption.valueOf(a.capitalize))

  given NotifyPropsFrom: Decoder[NotifyProps] = (c: HCursor) =>
    for {
      email <- c.downField("email").as[NotifyOption]
      push <- c.downField("push").as[NotifyOption]
      desktop <- c.downField("desktop").as[NotifyOption]
      markUnread <- c.downField("mark_unread").as[UnreadOption]
    } yield NotifyProps(email, push, desktop, markUnread)

  given UnreadOptionFrom: Decoder[UnreadOption] =
    _.as[String].map(a => UnreadOption.valueOf(a.capitalize))

  given AppResponseTo: Encoder[muffin.input.AppResponse] = {
    case AppResponse.Ok() => Json.obj()
    case AppResponse.Message(text, responseType, attachments) =>
      Json.obj(
        "text" -> Json.fromString(text),
        "response_type" -> Encoder[ResponseType].apply(responseType),
        "attachments" -> Encoder.encodeList[Attachment].apply(attachments)
      )
  }

  given ResponseTypeTo: Encoder[ResponseType] = {
    case ResponseType.Ephemeral => Encoder.encodeString("ephemeral")
    case ResponseType.InChannel => Encoder.encodeString("in_channel")
  }

  given DialogContextFrom: Decoder[muffin.input.DialogContext] = (c: HCursor) =>
    for {
      callbackId <- c.downField("callback_id").as[String]
      state <- c.downField("state").as[String]
      userId <- c.downField("user_id").as[UserId]
      channelId <- c.downField("channel_id").as[ChannelId]
      teamId <- c.downField("team_id").as[TeamId]
      submission <- c
        .downField("submission")
        .as[Map[String, Option[DialogSubmissionValue]]]
      cancelled <- c.downField("cancelled").as[Boolean]
    } yield muffin.input.DialogContext(
      callbackId,
      state,
      userId,
      channelId,
      teamId,
      submission,
      cancelled
    )

  given DialogSubmissionValueFrom: Decoder[DialogSubmissionValue] =
    (c: HCursor) =>
      c.value.asNumber
        .flatMap(_.toLong.map(DialogSubmissionValue.Num(_)))
        .orElse(c.value.asBoolean.map(DialogSubmissionValue.Bool(_)))
        .orElse(c.value.asString.map(DialogSubmissionValue.Str(_))) match {
        case Some(value) => Right(value)
        case None =>
          Left(DecodingFailure("Invalid dialog submission", c.history))
      }

  given RawActionFrom: Decoder[muffin.input.RawAction[Json]] = (c: HCursor) =>
    for {
      userId <- c.downField("user_id").as[UserId]
      userName <- c.downField("user_name").as[Login]
      channelId <- c.downField("channel_id").as[ChannelId]
      channelName <- c.downField("channel_name").as[String]
      teamId <- c.downField("team_id").as[TeamId]
      teamDomain <- c.downField("team_domain").as[String]
      postId <- c.downField("post_id").as[MessageId]
      triggerId <- c.downField("trigger_id").as[String]
      dataSource <- c.downField("data_source").as[String]
      typ <- c.downField("type").as[String]
      context <- c.downField("context").as[Json]
    } yield RawAction(
      userId,
      userName,
      channelId,
      channelName,
      teamId,
      teamDomain,
      postId,
      triggerId,
      dataSource,
      typ,
      context
    )

  given UserFrom(using zone: ZoneId): Decoder[User] = (c: HCursor) =>
    for {
      id <- c.downField("id").as[UserId]
      username <- c.downField("username").as[String]
      first_name = c.downField("first_name").as[String].toOption
      last_name = c.downField("last_name").as[String].toOption
      nickname = c.downField("nickname").as[String].toOption
      email = c.downField("email").as[String].toOption
      roles = c.downField("roles").as[String].toOption
      locale = c.downField("locale").as[String].toOption
    } yield User(
      id,
      username,
      first_name,
      last_name,
      nickname,
      email,
      roles,
      locale
    )

  given ActionTo: Encoder[Action] = {
    case value: Action.Button =>
      Json.obj(
        List(
          ("id" -> Json.fromString(value.id)).some,
          ("name" -> Json.fromString(value.name)).some,
          ("integration" -> Encoder[Integration].apply(value.integration)).some,
          value.style.map(s => "style" -> Encoder[Style].apply(s)),
          ("type" -> Json.fromString("button")).some
        ).flatten*
      )
    case value: Action.Select =>
      Json.obj(
        List(
          ("id" -> Json.fromString(value.id)).some,
          ("name" -> Json.fromString(value.name)).some,
          ("integration" -> Encoder[Integration].apply(value.integration)).some,
          ("options" -> Encoder
            .encodeList[SelectOption]
            .apply(value.options)).some,
          value.dataSource.map(s =>
            "data_source" -> Encoder[DataSource].apply(s)
          ),
          ("type" -> Json.fromString("select")).some
        ).flatten*
      )
  }

  given ActionFrom: Decoder[Action] = (c: HCursor) => ???

  given IntegrationTo: Encoder[Integration] = i =>
    Json.obj(
      "url" -> Json.fromString(i.url),
      "context" -> parse(i.context).toOption
        .getOrElse(Json.fromString(i.context))
    )

  given StyleTo: Encoder[Style] = (s: Style) =>
    Json.fromString(s.toString.toLowerCase)

  given StyleFrom: Decoder[Style] = (c: HCursor) =>
    c.as[String].map(s => Style.valueOf(s.capitalize))

  given PropsTo: Encoder[Props] = p =>
    Json.obj(
      "attachments" -> Encoder.encodeList[Attachment].apply(p.attachments)
    )

  given AttachmentTo: Encoder[Attachment] = a =>
    Json.obj(
      List(
        a.fallback.map(i => "fallback" -> Json.fromString(i)),
        a.color.map(i => "color" -> Json.fromString(i)),
        a.pretext.map(i => "pretext" -> Json.fromString(i)),
        a.text.map(i => "text" -> Json.fromString(i)),
        a.authorName.map(i => "author_name" -> Json.fromString(i)),
        a.authorLink.map(i => "author_link" -> Json.fromString(i)),
        a.authorIcon.map(i => "author_icon" -> Json.fromString(i)),
        a.title.map(i => "title" -> Json.fromString(i)),
        a.titleLink.map(i => "title_link" -> Json.fromString(i)),
        ("fields" -> Encoder.encodeList[AttachmentField].apply(a.fields)).some,
        a.imageUrl.map(i => "image_url" -> Json.fromString(i)),
        a.thumbUrl.map(i => "thumb_url" -> Json.fromString(i)),
        a.footer.map(i => "footer" -> Json.fromString(i)),
        a.footerIcon.map(i => "footer_icon" -> Json.fromString(i)),
        ("actions" -> Encoder.encodeList[Action].apply(a.actions)).some
      ).flatten*
    )

  given AttachmentFieldTo: Encoder[AttachmentField] = a =>
    Json.obj(
      "title" -> Json.fromString(a.title),
      "value" -> Json.fromString(a.value),
      "short" -> Json.fromBoolean(a.short)
    )

  given PostFrom: Decoder[Post] = (c: HCursor) =>
    for {
      id <- c.downField("id").as[MessageId]
    } yield Post(id)

  def json: JsonRequestBuilder[Encoder] = new CirceJsonBuilder(JsonObject.empty)

  private class CirceJsonBuilder(state: JsonObject)
      extends JsonRequestBuilder[Encoder] {
    def field[T: Encoder](
      fieldName: String,
      fieldValue: T
    ): JsonRequestBuilder[Encoder] =
      CirceJsonBuilder(state.add(fieldName, fieldValue.asJson))

    def field[T: Encoder](
      fieldName: String,
      fieldValue: Option[T]
    ): JsonRequestBuilder[Encoder] =
      fieldValue match
        case Some(value) => CirceJsonBuilder(state.add(fieldName, value.asJson))
        case None        => this

    def build: Body.RawJson = Body.RawJson(Json.fromJsonObject(state).dropNullValues.noSpaces)
  }
}
