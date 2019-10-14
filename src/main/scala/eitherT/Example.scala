package eitherT

object example {

  import eitherT.{Either, Right, Left, EitherT, Monad}
  import eitherT.Monad.MonadSyntax
  import scala.concurrent.{Future, ExecutionContext}

  case class Admin(id: Long)
  class AdminAuthorizer {
    import AdminAuthorizer.AdminAuthorizerErr
    def authorize(token: String): Future[Either[AdminAuthorizerErr, Admin]] = ???
  }
  object AdminAuthorizer {
    enum AdminAuthorizerErr {
      case TokenInvalid, TokenExpired
    }
  }
  
  case class User(userId: Long)
  class UserRepository {
    import UserRepository.UserRepositoryErr
    def findBy(userId: Long): Future[Either[UserRepositoryErr, User]] = ???
  }
  object UserRepository {
    enum UserRepositoryErr {
      case UserNotFound, UserDisabled
    }
  }
  
  class HogeMoneyService {
    import HogeMoneyService.HogeMoneyServiceErr
    def enable(admin: Admin, user: User): Future[Either[HogeMoneyServiceErr, Unit]] = ???
  }
  object HogeMoneyService {
    enum HogeMoneyServiceErr {
      case HogeRequirementsErr, HogeMoneyInsufficientBalanceErr, UnknownErr
    }
  }

  class Controller(
    adminAuthorizer: AdminAuthorizer,
    userRepository: UserRepository,
    hogeMoneyService: HogeMoneyService
  ) {

    def enable(token: String, userId: Long): Future[String] = {

      implicit val ec = ExecutionContext.global

      import AdminAuthorizer.AdminAuthorizerErr
      import UserRepository.UserRepositoryErr
      import HogeMoneyService.HogeMoneyServiceErr
      type Errs = AdminAuthorizerErr | UserRepositoryErr | HogeMoneyServiceErr

      val et: EitherT[Future, Errs, Unit] = for {
        admin <- EitherT(adminAuthorizer.authorize(token))
        user <- EitherT(userRepository.findBy(userId))
        enabled <- EitherT(hogeMoneyService.enable(admin, user))
      } yield enabled

      val result: Future[String] = et.value.map {
        case Left(AdminAuthorizerErr.TokenInvalid) => "401 Unauthorized"
        case Left(AdminAuthorizerErr.TokenExpired) => "401 Unauthorized"
        case Left(UserRepositoryErr.UserNotFound) => "404 NotFound: User not found"
        case Left(UserRepositoryErr.UserDisabled) => "403 Forbidden: User temporary disabled"
        case Left(HogeMoneyServiceErr.HogeRequirementsErr) => "403 Forbidden: User does not meet requirements"
        case Left(HogeMoneyServiceErr.HogeMoneyInsufficientBalanceErr) => "403 Forbidden: Unsufficient user balance"
        case Left(HogeMoneyServiceErr.UnknownErr) => "500 Internal Server Error: HogeMoneyServiceErr.UnknownErr"
        case Right(_) => "200 OK"
      }

      result.recover { case e: Throwable =>
        e.printStackTrace()
        "500 Internal Server Error: Unknown"
      }

    }

  }


}
