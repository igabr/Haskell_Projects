{-
  File      :  CollisionDetection.hs
  Name      :  Ibrahim Gabr, 10/02/17 
  Contains types for representing 2D objects and collision functions for them.
-}

type Point = (Double, Double)

type LineSegment = (Point, Point)

type BoundingBox = (LineSegment, LineSegment, LineSegment, LineSegment)

type Circle = (Point, Double)

{-This function returns true if two BoundingBoxes collide.-}
rectsIntersect :: BoundingBox -> BoundingBox -> Bool
rectsIntersect (left_edge_1, bottom_edge_1, right_edge1, top_edge_1) (left_edge_2, bottom_edge_2, right_edge_2, top_edge_2) =
    if not (((fst $ fst right_edge1) < (fst $ fst left_edge_2)) ||
        ((fst $ fst left_edge_1) > (fst $ fst right_edge_2)) ||
        ((snd $ snd bottom_edge_1 ) > (snd $ snd top_edge_2)) ||
        ((snd $ snd top_edge_1 ) < (snd $ snd bottom_edge_2)))
        then True
        else False

{-This is a helper function that calculates the distance between two points.-}
distanceBetweenPoints :: Point -> Point -> Double
distanceBetweenPoints (px1, py1) (px2, py2) = sqrt((px2-px1)**2 + (py2-py1)**2)

{-This functions returns true if two Circles collide.-}
circlesIntersect :: Circle -> Circle -> Bool
circlesIntersect ((px1, py1), r1) ((px2,py2), r2) =
    let circleDistance = distanceBetweenPoints (px1, py1) (px2, py2)
        totalRadius = r1 + r2
        in if ((circleDistance == totalRadius) || (circleDistance < totalRadius))
            then True
            else False

{-This is a helper function that determines if line intersects a BoundingBox-}
finiteLineCheck :: LineSegment -> LineSegment -> Bool
finiteLineCheck ((x1, y1), (x2, y2)) ((ex1, ey1), (ex2, ey2)) =
    let ua = (((x2 - x1) * (ey1 - y1)) - ((y2-y1) * (ex1 - x1)))/ (((y2-y1) * (ex2-ex1)) - ((x2-x1) * (ey2-ey1)))
        ub = (((ex2-ex1) * (ey1-y1)) - ((ey2-ey1) * (ex1 - x1)))/ (((y2-y1) * (ex2-ex1)) - ((x2-x1) * (ey2-ey1)))
        in if (
            ua < 0 ||
            ua > 1 ||
            ub < 0 ||
            ub > 1
            )
            then False
            else True

{-This function returns true if a LineSegment intersects with a BoundingBox.-}
rectLineIntersect :: LineSegment -> BoundingBox -> Bool
rectLineIntersect ((x1, y1), (x2, y2)) (left_edge, bottom_edge, right_edge, top_edge) =
    if(
        finiteLineCheck ((x1, y1), (x2, y2)) left_edge == False &&
        finiteLineCheck ((x1, y1), (x2, y2)) right_edge == False &&
        finiteLineCheck ((x1, y1), (x2, y2)) bottom_edge == False &&
        finiteLineCheck ((x1, y1), (x2, y2)) right_edge == False &&
        finiteLineCheck ((x1, y1), (x2, y2)) top_edge == False
        )
        then False
        else True

{-This function returns true if a LineSegment intersects with a Circle.-}
circleLineIntersect:: LineSegment -> Circle -> Bool
circleLineIntersect ((x1,y1), (x2,y2)) ((px1, py1), r) =
    let distx1_from_circle = (x1 - px1, y1-py1)
        distx2_from_circle = (x2-px1, y2-py1)
        diff_tuple = (fst distx2_from_circle - fst distx1_from_circle, snd distx2_from_circle - snd distx1_from_circle)
        a = (fst diff_tuple)**2 + (snd diff_tuple)**2
        b = 2 * ((fst diff_tuple * fst distx1_from_circle) + (snd diff_tuple * snd distx1_from_circle))
        c = (fst distx1_from_circle)**2 + (snd distx1_from_circle)**2 - r**2
        discriminant = b**2 - (4*a*c)
        in if (discriminant == 0) || (discriminant > 0)
            then True
            else False
