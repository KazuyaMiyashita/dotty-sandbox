package eitherT

import org.junit.Test
import org.junit.Assert._

import eitherT.{Either, Right, Left, EitherT, Monad}
import eitherT.Monad.MonadSyntax
import eitherT.example._
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

import AdminAuthorizer.AdminAuthorizerErr
import UserRepository.UserRepositoryErr
import HogeMoneyService.HogeMoneyServiceErr

class ExampleTest {

  val fakeController = {
    val adminAuthorizer = new AdminAuthorizer {
      override def authorize(token: String): Future[Either[AdminAuthorizerErr, Admin]] = Future {
        if (token == "faketoken1") Right(Admin(1))
        else Left(AdminAuthorizerErr.TokenInvalid)
      }
    }
    val userRepository = new UserRepository {
      override def findBy(userId: Long): Future[Either[UserRepositoryErr, User]] = Future {
        if (userId == 42L) Right(User(42))
        else Left(UserRepositoryErr.UserNotFound)
      }
    }
    val hogeMoneyService = new HogeMoneyService {
      override def enable(admin: Admin, user: User): Future[Either[HogeMoneyServiceErr, Unit]] = Future {
        if (admin == Admin(1) && user == User(42)) Right(())
        else Left(HogeMoneyServiceErr.UnknownErr)
      }
    }

    Controller(adminAuthorizer, userRepository, hogeMoneyService)
  }

  @Test def t1(): Unit = {
    fakeController.enable("faketoken1", 42).foreach { result =>
      assertEquals(result, "200 OK")
    }
  }

  @Test def t2(): Unit = {
    fakeController.enable("faketoken1", 4200).foreach { result =>
      assertEquals(result, "404 NotFound: User not found")
    }
  }


}
