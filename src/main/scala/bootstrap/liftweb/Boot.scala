package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._

import sitemap._
import Loc._
import mapper._
import net.liftmodules.JQueryModule
import js.jquery.JQueryArtifacts
import code.model._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Loggable{
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
            "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User)

    // where to search snippet
    LiftRules.addToPackages("code")

    // Build SiteMap
    def sitemap = SiteMap(
      Menu.i("Home") / "index" >> User.AddUserMenusAfter, // the simple way to declare a menu

      Menu.i("FineUploader") / "fineuploader", // the simple way to declare a menu

      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
        "Static Content")))

    def sitemapMutators = User.sitemapMutator

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMapFunc(() => sitemapMutators(sitemap))

    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery = JQueryModule.JQuery172
    JQueryModule.init()

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    FileUpload.init()
    FineUpload.init()
    
    // Set max file upload to 200mb 
    LiftRules.maxMimeSize = 200 * 1024 * 1024

    LiftRules.maxMimeFileSize = 200 * 1024 * 1024

    logger.info("Uploaded files will be written to temporary location:%s".format(System.getProperty("java.io.tmpdir")));

    /**
     * If you hit the size limit, an exception will be thrown from the underlying file upload library.
     * Be aware that the container (Jetty, Tomcat) or any web server (Apache, Nginx) may also have limits on file upload sizes.
     * You will recognise this situation by an error such as java.lang.IllegalStateException: Form too large705784>200000.
     * Check with documentation for the container for changing these limits.
     */
    //LiftRules.exceptionHandler.prepend {
    //  case (_, _, x: FileUploadIOException) =>
    //    ResponseWithReason(BadResponse(), "Unable to process file. Too large?")
    //}

    //Make sure we don't put stuff in memory for uploads
    LiftRules.handleMimeFile = OnDiskFileParamHolder.apply
    

    LiftRules.progressListener = {
      val opl = LiftRules.progressListener
      val ret: (Long, Long, Int) => Unit =
        (a, b, c) => {
          // println("progress listener "+a+" plus "+b+" "+c)
          // Thread.sleep(100) -- demonstrate slow uploads
          opl(a, b, c)
        }
      ret
    }

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)
  }
}

import rest._
import json._
import JsonDSL._

object FileUpload extends RestHelper with Logger {
  serve {
    case "upload" :: "thing" :: Nil Post req => {
      val uploads = req.uploadedFiles
      debug("Uploaded files: " + uploads)
      val ojv: List[JObject] =
        uploads.map(fph =>
          ("name" -> fph.fileName) ~
            ("sizef" -> fph.length) ~
            ("delete_url" -> "/delete/thing") ~
            ("delete_type" -> "DELETE"))

      // run callbacks
      //      S.session.map(_.runParams(req))
      // This is a tad bit of a hack, but we need to return text/plain, not JSON
      val jr = JsonResponse(ojv).toResponse.asInstanceOf[InMemoryResponse]
      InMemoryResponse(jr.data, ("Content-Length", jr.data.length.toString) ::
        ("Content-Type", "text/plain") :: S.getResponseHeaders(Nil),
        S.responseCookies, 200)
    }

    case "delete" :: "thing" :: Nil Delete req => {
      debug("TODO: got a delete request, handle it!")
      OkResponse()
    }

  }

  def init() = {
    //rewrite so the rest-callback will be a param instead to be fired with LiftSession.runParams
    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath("upload" :: "thing" :: callback :: Nil, "", true, _), _, _) =>
        RewriteResponse("upload" :: "thing" :: Nil, Map("callback" -> "_"))
    }

    LiftRules.dispatch.append(this)
  }
}

object FineUpload extends RestHelper with Loggable {
  serve {
    
    case "upload" :: "fine" :: Nil Post AllowedMimeTypes(req)  => {
      
      val uploads = req.uploadedFiles
      
      for (file <- req.uploadedFiles) {
        logger.debug("Received: " + file.fileName)
      }
      
      val resp:JValue = uploads.map(fph =>
          ("success" -> true) ~
            ( "name" -> fph.fileName) ~
            ("size" -> fph.length) ~
            ("newUuid" -> S.param("uid").openOr("missing") )
            ).headOption.getOrElse( ("success" -> false))
           
            
            
      val ojv: List[JObject] =
        uploads.map(fph =>
          ("success" -> true) ~
            ( "name" -> fph.fileName) ~
            ("size" -> fph.length) ~
            ("newUuid" -> S.param("uid").openOr("missing") )
            )

      // run callbacks
      //      S.session.map(_.runParams(req))
      // This is a tad bit of a hack, but we need to return text/plain, not JSON
      val jr = JsonResponse(resp).toResponse.asInstanceOf[InMemoryResponse]
      InMemoryResponse(jr.data, ("Content-Length", jr.data.length.toString) ::
        ("Content-Type", "text/plain") :: S.getResponseHeaders(Nil),
        S.responseCookies, 200)
    }

  }

  def init() = {
    
    //rewrite so the rest-callback will be a param instead to be fired with LiftSession.runParams
    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath("upload" :: "fine" :: callback :: Nil, "", true, _), _, _) =>
        RewriteResponse("upload" :: "fine" :: Nil, Map("callback" -> "_"))
    }

    LiftRules.dispatch.append(this)
  }
}




object AllowedMimeTypes extends Loggable {
  def unapply(req: Req): Option[Req] = {
    logger.info("req.uploadedFiles.map{_.mimeType) is %s".format(req.uploadedFiles.map{_.mimeType}))
    req.uploadedFiles.flatMap{_.mimeType match {
      case "image/bmp"            => Some(req)
      case "image/x-windows-bmp"  => Some(req)
      case "image/vnd.dwg"        => Some(req)
      case "image/gif"            => Some(req)
      case "image/x-icon"         => Some(req)
      case "image/jpeg"           => Some(req)
      case "image/pict"           => Some(req)
      case "image/png"            => Some(req)
      case "image/x-quicktime"    => Some(req)
      case "image/tiff"           => Some(req)
      case "image/x-tiff"         => Some(req)
      case _                      => None
    }}.headOption
  }
 
}