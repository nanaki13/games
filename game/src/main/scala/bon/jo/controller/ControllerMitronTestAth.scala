package bon.jo.controller

import java.io._

import bon.jo.conf.Conf
import bon.jo.model.Model._
import bon.jo.model.Shape.ComposedShape
import bon.jo.model.Shapes.ShapeParamMultiple
import bon.jo.model._
import bon.jo.view.View

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

trait ControllerMitronTestAth extends Controller[MitronAthParam]