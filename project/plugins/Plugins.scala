import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val lwjglPlugin = "calico" % "sbt-lwjgl-plugin" % "1.0"
}
