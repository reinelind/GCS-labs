import java.net.URLEncoder

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.util.ByteString

import info.mukel.telegrambot4s.Implicits._
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.api.declarative._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._
object Bot extends TelegramBot
  with Polling
  with Commands
  with ChatActions
  with InlineQueries {
  def token = "528618622:AAERcLVWG5eA29h1WEwR-Fr4I1bGHHrwESA"

  def ttsUrl(text: String): String =
    s"http://translate.google.com/translate_tts?client=tw-ob&tl=ru-us&q=${URLEncoder.encode(text, "UTF-8")}"

  onCommand('speak, 'talk, 'say) {
    implicit msg =>
      withArgs {
        args =>
          val text = args.mkString("")
          for {
            response <- Http().singleRequest(HttpRequest(uri = Uri(ttsUrl(text))))
            if response.status.isSuccess()
            bytes <- Unmarshal(response).to[ByteString]
          } {
            uploadingAudio
            val voiceMp3 = InputFile("voice.mp3", bytes)
            request(SendVoice(msg.source, voiceMp3))
          }
      }
  }
  def nonEmptyQuery (iq : InlineQuery) : Boolean = iq.query.nonEmpty

  whenOrElse (onInlineQuery, nonEmptyQuery) {
    implicit iq =>
      answerInlineQuery (Seq(
        InlineQueryResultVoice ("inline: " + iq.query, ttsUrl(iq.query), iq.query),
        InlineQueryResultArticle ("command: " + iq.query, iq.query,
          inputMessageContent = InputTextMessageContent ("/speak" + iq.query),
          description = "/speak "+ iq.query )))
  } {
    answerInlineQuery(Seq())(_)
  }
}

