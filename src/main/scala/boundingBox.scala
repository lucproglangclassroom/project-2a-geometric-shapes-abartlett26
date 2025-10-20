package edu.luc.cs.laufer.cs371.shapes

// TODO: implement this behavior

import Shape.*

object boundingBox:
  def apply(s: Shape): Location = s match
    case Rectangle(width, height) =>
      Location(0, 0, Rectangle(width, height))
      
    case Ellipse(radiusX, radiusY) =>
      // Ellipse parameters are radii, so bounding box is twice the size
      val width = radiusX * 2
      val height = radiusY * 2
      Location(-radiusX, -radiusY, Rectangle(width, height))
      
    case Location(x, y, shape) =>
      val innerBox = boundingBox(shape)
      Location(x + innerBox.x, y + innerBox.y, innerBox.shape)
      
    case Group(shapes @ _*) =>
      if shapes.isEmpty then
        Location(0, 0, Rectangle(0, 0))
      else
        val boxes = shapes.map(boundingBox.apply)
        val minX = boxes.map(box => box.x).min
        val minY = boxes.map(box => box.y).min
        val maxX = boxes.map(box => box.x + getWidth(box.shape)).max
        val maxY = boxes.map(box => box.y + getHeight(box.shape)).max
        Location(minX, minY, Rectangle(maxX - minX, maxY - minY))

  // Helper methods to extract width and height from shape
  private def getWidth(shape: Shape): Int = shape match
    case Rectangle(width, _) => width
    case Ellipse(radiusX, _) => radiusX * 2  // Ellipse width is twice the radius
    case _ => 0
  
  private def getHeight(shape: Shape): Int = shape match
    case Rectangle(_, height) => height
    case Ellipse(_, radiusY) => radiusY * 2  // Ellipse height is twice the radius
    case _ => 0

end boundingBox