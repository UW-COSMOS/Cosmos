from pydantic import BaseModel, Field
from pydantic.schema import Optional

class AnnotationBounds(BaseModel):
    page_num: int
    postprocess_cls: str
    bounding_box: list[int]


    def area(self):
        """ Get the area of a bounding box"""
        (x0, y0, x1, y1) = self.bounding_box
        return (x1 - x0) * (y1 - y0)

    def overlap(self, other: 'AnnotationBounds'):
        """ Get the intersecting area of two rectangles """
        # via https://stackoverflow.com/a/27162334

        (my_x0, my_y0, my_x1, my_y1) = self.bounding_box
        (their_x0, their_y0, their_x1, their_y1) = other.bounding_box

        dx = min(my_x1, their_x1) - max(my_x0, their_x0)
        dy = min(my_y1, their_y1) - max(my_y0, their_y0)

        return dx * dy if dx > 0 and dy > 0 else 0
