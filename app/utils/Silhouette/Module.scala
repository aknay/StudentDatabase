package utils.Silhouette

/**
  * Created by aknay on 1/2/17.
  */

import com.google.inject.name.Named
import com.google.inject.{AbstractModule, Provides}
import com.mohiva.play.silhouette.api.actions.{SecuredErrorHandler, UnsecuredErrorHandler}
import com.mohiva.play.silhouette.api.crypto._
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services._
import com.mohiva.play.silhouette.api.util._
import com.mohiva.play.silhouette.api.{Environment, EventBus, Silhouette, SilhouetteProvider}
import com.mohiva.play.silhouette.crypto.{JcaCrypter, JcaCrypterSettings, JcaSigner, JcaSignerSettings}
import com.mohiva.play.silhouette.impl.authenticators._
import com.mohiva.play.silhouette.impl.providers._
import com.mohiva.play.silhouette.impl.util._
import com.mohiva.play.silhouette.password.BCryptPasswordHasher
import com.mohiva.play.silhouette.persistence.daos.DelegableAuthInfoDAO
import com.mohiva.play.silhouette.persistence.repositories.DelegableAuthInfoRepository
import dao.UserDao
import models.{MailTokenMasterUser, MailTokenUser, User}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.codingwell.scalaguice.ScalaModule
import play.api.Configuration
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.CookieHeaderEncoding

/**
  * The Guice module which wires all Silhouette dependencies.
  */
class Module extends AbstractModule with ScalaModule {

  /**
    * Configures the module.
    */
  def configure() {
    bind[Silhouette[MyEnv]].to[SilhouetteProvider[MyEnv]]
    bind[SecuredErrorHandler].to[ErrorHandler]
    bind[UnsecuredErrorHandler].to[ErrorHandler]
    bind[IdentityService[User]].to[UserService]
    bind[MailTokenService[MailTokenUser]].to[MailTokenUserService]
    bind[MailTokenService[MailTokenMasterUser]].to[MailTokenMasterUserService]
    bind[IDGenerator].toInstance(new SecureRandomIDGenerator())
    bind[PasswordHasher].toInstance(new BCryptPasswordHasher())
    bind[FingerprintGenerator].toInstance(new DefaultFingerprintGenerator(false))
    bind[EventBus].toInstance(EventBus())
    bind[Clock].toInstance(Clock())

  }

  @Provides
  def providePasswordInfoDao(userDao: UserDao): DelegableAuthInfoDAO[PasswordInfo] = {
    new PasswordInfoDao(userDao)
  }

  /**
    * Provides the Silhouette environment.
    *
    * @param userService          The user service implementation.
    * @param authenticatorService The authentication service implementation.
    * @param eventBus             The event bus instance.
    * @return The Silhouette environment.
    */
  @Provides
  def provideEnvironment(
                          userService: UserService,
                          authenticatorService: AuthenticatorService[CookieAuthenticator],
                          eventBus: EventBus
                        ): Environment[MyEnv] = {
    Environment[MyEnv](
      userService,
      authenticatorService,
      Seq(),
      eventBus
    )
  }

  /**
    * Provides the signer for the authenticator.
    *
    * @param configuration The Play configuration.
    * @return The signer for the authenticator.
    */
  @Provides
  @Named("authenticator-signer")
  def provideAuthenticatorSigner(configuration: Configuration): Signer = {
    val config = configuration.underlying.as[JcaSignerSettings]("silhouette.authenticator.signer")

    new JcaSigner(config)
  }

  /**
    * Provides the crypter for the authenticator.
    *
    * @param configuration The Play configuration.
    * @return The crypter for the authenticator.
    */
  @Provides
  @Named("authenticator-crypter")
  def provideAuthenticatorCrypter(configuration: Configuration): Crypter = {
    val config = configuration.underlying.as[JcaCrypterSettings]("silhouette.authenticator.crypter")

    new JcaCrypter(config)
  }


  /**
    * Provides the auth info repository.
    *
    * @param passwordInfoDAO The implementation of the delegable password auth info DAO.
    * @return The auth info repository instance.
    */
  @Provides
  def provideAuthInfoRepository(passwordInfoDAO: DelegableAuthInfoDAO[PasswordInfo]): AuthInfoRepository = {
    new DelegableAuthInfoRepository(passwordInfoDAO)
  }

  /**
    * Provides the authenticator service.
    *
    * @param signer               The cookie signer implementation.
    * @param crypter              The crypter implementation.
    * @param fingerprintGenerator The fingerprint generator implementation.
    * @param idGenerator          The ID generator implementation.
    * @param configuration        The Play configuration.
    * @param clock                The clock instance.
    * @return The authenticator service.
    */

  @Provides
  def provideAuthenticatorService(
                                   @Named("authenticator-signer") signer: Signer,
                                   @Named("authenticator-crypter") crypter: Crypter,
                                   cookieHeaderEncoding: CookieHeaderEncoding,
                                   fingerprintGenerator: FingerprintGenerator,
                                   idGenerator: IDGenerator,
                                   configuration: Configuration,
                                   clock: Clock): AuthenticatorService[CookieAuthenticator] = {

    val config = configuration.underlying.as[CookieAuthenticatorSettings]("silhouette.authenticator")
    val authenticatorEncoder = new CrypterAuthenticatorEncoder(crypter)

    new CookieAuthenticatorService(config, None, signer, cookieHeaderEncoding, authenticatorEncoder, fingerprintGenerator, idGenerator, clock)
  }


  /**
    * Provides the password hasher registry.
    *
    * @param passwordHasher The default password hasher implementation.
    * @return The password hasher registry.
    */
  @Provides
  def providePasswordHasherRegistry(passwordHasher: PasswordHasher): PasswordHasherRegistry = {
    new PasswordHasherRegistry(passwordHasher)
  }

  /**
    * Provides the credentials provider.
    *
    * @param authInfoRepository     The auth info repository implementation.
    * @param passwordHasherRegistry The password hasher registry.
    * @return The credentials provider.
    */
  @Provides
  def provideCredentialsProvider(
                                  authInfoRepository: AuthInfoRepository,
                                  passwordHasherRegistry: PasswordHasherRegistry
                                ): CredentialsProvider = {
    new CredentialsProvider(authInfoRepository, passwordHasherRegistry)
  }
}