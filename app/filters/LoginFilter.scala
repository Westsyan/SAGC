package filters

import javax.inject.Inject

import akka.stream.Materializer
import controllers.routes
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class LoginFilter @Inject()(implicit val mat: Materializer, ec: ExecutionContext) extends Filter {

  override def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {

    if (rh.session.get("phone").isEmpty && rh.path.contains("/admin") && !rh.path.contains("/assets/") &&
      !rh.path.contains("/login")) {
      if (rh.path.contains("chinese")) {
        Future.successful(Results.Redirect(routes.ChineseController.loginBefore()).flashing("info" -> "请先登录!"))

      } else {
        Future.successful(Results.Redirect(routes.AdminController.loginBefore()).flashing("info" -> "Please login in first!"))
      }
    } else {
      f(rh)
    }
  }
}
