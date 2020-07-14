package de.bwhc.user.auth.fs.repo



import java.io.File


trait Config
{
  def dataDir: File
}


object Config
{

  def getInstance: Config = {
    DefaultConfig
  }

  private object DefaultConfig extends Config
  {
    val dataDir =
      Option(System.getProperty("bwhc.user.data.dir"))
        .map(new File(_))
        .get
  }

}

