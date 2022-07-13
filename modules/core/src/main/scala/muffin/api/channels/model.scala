package muffin.api.channels

import muffin.predef.*

import java.time.LocalDateTime

enum NotifyOption:
  case All
  case Mention
  case None
  case Default

enum UnreadOption:
  case All
  case Mention

case class NotifyProps(
  email: NotifyOption,
  push: NotifyOption,
  desktop: NotifyOption,
  markUnread: UnreadOption
)

case class ChannelMember(
  channelId: ChannelId,
  userId: UserId,
  roles: String,
  lastViewedAt: LocalDateTime,
  msgCount: Option[Int],
  mentionCount: Option[Int],
  notifyProps: NotifyProps,
  lastUpdateAt: Option[LocalDateTime],
  teamDisplayName: Option[String],
  teamName: Option[String],
  teamUpdateAt: Option[String]
)

case class ChannelInfo(
  id: ChannelId,
  createAt: LocalDateTime,
  updateAt: LocalDateTime,
  deleteAt: Option[LocalDateTime],
  teamId: TeamId,
  channelType: String,
  displayName: String,
  name: String,
  header: String,
  purpose: String,
  lastPostAt: Option[LocalDateTime],
  totalMsgCount: Int,
  creatorId: UserId
)
