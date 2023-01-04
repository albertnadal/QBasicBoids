DECLARE SUB drawObstacle (obstacle AS ANY)
DECLARE SUB drawObstacles ()
DECLARE SUB initializeObstacles ()
DECLARE SUB setVectorMagnitude (a AS ANY, m AS SINGLE)
DECLARE SUB calculateVectorSubstraction (a AS ANY, b AS ANY)
DECLARE SUB calculateVectorDivision (a AS ANY, value AS SINGLE)
DECLARE SUB calculateVectorNormalization (inputVector AS ANY, normalizedVector AS ANY)
DECLARE SUB calculateNormalizedVector (inputVector AS ANY, normalizedVector AS ANY)
DECLARE FUNCTION getVectorMagnitude! (a AS ANY)
DECLARE FUNCTION getDistanceBetweenPoints! (a AS ANY, b AS ANY)
DECLARE SUB updateBoidPosition (boid AS ANY)
DECLARE FUNCTION min! (a AS SINGLE, b AS SINGLE)
DECLARE SUB computeBoidForces (boid AS ANY)
DECLARE SUB updateBoidNeighbors (boid AS ANY)
DECLARE SUB updateBoid (boid AS ANY)
DECLARE SUB updateBoids ()
DECLARE SUB drawBoid (boid AS ANY)
DECLARE SUB drawBoids ()
DECLARE SUB delay (seconds#)
DECLARE SUB initializeBoids ()

DEFINT A-Z
RANDOMIZE TIMER

CONST SCREENWIDTH = 319
CONST SCREENHEIGHT = 199
CONST NUMOBSTACLES = 3
CONST OBSTACLERADIUS = 10!
CONST OBSTACLEACTIONRADIUS = 38!
CONST NUMBOIDS = 20
CONST NEIGHBORRADIUS = 26!
CONST MAXSPEED = 3!
CONST CROWDRADIUS = 6!

TYPE TVector
 x AS SINGLE
 y AS SINGLE
END TYPE

TYPE TBoid
 id AS INTEGER
 position AS TVector
 prevPosition AS TVector
 prevPrevPosition AS TVector
 movement AS TVector
 totalNeighbours AS INTEGER
END TYPE

TYPE TObstacle
 position AS TVector
END TYPE

DIM SHARED neighbours(NUMBOIDS, NUMBOIDS) AS INTEGER
DIM SHARED boids(NUMBOIDS) AS TBoid
DIM SHARED obstacles(NUMOBSTACLES) AS TObstacle

SCREEN 1

CALL initializeBoids
CALL initializeObstacles

FOR i = 0 TO 250
  CALL updateBoids
  CALL drawBoids
  CALL drawObstacles
  'CALL delay(.03)
NEXT i

END

SUB calculateVectorDivision (a AS TVector, value AS SINGLE)
  a.x = a.x / value
  a.y = a.y / value
END SUB

SUB calculateVectorNormalization (inputVector AS TVector, normalizedVector AS TVector)
  magnitude! = getVectorMagnitude(inputVector)
  IF magnitude! > 0! THEN
    normalizedVector.x = inputVector.x / magnitude!
    normalizedVector.y = inputVector.y / magnitude!
  ELSE
    normalizedVector.x = inputVector.x
    normalizedVector.y = inputVector.y
  END IF
END SUB

SUB calculateVectorSubstraction (a AS TVector, b AS TVector)
  a.x = a.x - b.x
  a.y = a.y - b.y
END SUB

SUB computeBoidForces (boid AS TBoid)
  DIM neighbor AS TBoid
  DIM obstacle AS TObstacle
  DIM normalizedVector AS TVector
  DIM distanceVector AS TVector

  ' Calculate the alignment between the boid and its neighbors
  FOR idx = 0 TO boid.totalNeighbours - 1
    neighborId = neighbours(boid.id, idx)
    neighbor = boids(neighborId)
    distance! = getDistanceBetweenPoints(boid.position, neighbor.position)
    IF ((distance! > 0) AND (distance! < NEIGHBORRADIUS)) THEN
      CALL calculateVectorNormalization(neighbor.movement, normalizedVector)
      CALL calculateVectorDivision(normalizedVector, distance!)
      boid.movement.x = boid.movement.x + normalizedVector.x
      boid.movement.y = boid.movement.y + normalizedVector.y
    END IF
  NEXT idx

  ' Calculate the cohesion force
  DIM cohesionVector AS TVector
  cohesionVector.x = 0
  cohesionVector.y = 0
  cohesionCount! = 0!
  FOR idx = 0 TO boid.totalNeighbours - 1
    neighborId = neighbours(boid.id, idx)
    neighbor = boids(neighborId)
    distance! = getDistanceBetweenPoints(boid.position, neighbor.position)
    IF ((distance! > 0) AND (distance! < NEIGHBORRADIUS)) THEN
      cohesionVector.x = cohesionVector.x + neighbor.position.x
      cohesionVector.y = cohesionVector.y + neighbor.position.y
      cohesionCount! = cohesionCount! + 1!
    END IF
  NEXT idx

  IF cohesionCount! > 0! THEN
    CALL calculateVectorDivision(cohesionVector, cohesionCount!)
    CALL calculateVectorSubstraction(cohesionVector, boid.position)
    CALL setVectorMagnitude(cohesionVector, .05)
    boid.movement.x = boid.movement.x + cohesionVector.x
    boid.movement.y = boid.movement.y + cohesionVector.y
  END IF

  ' Calculate the separation force
  FOR idx = 0 TO boid.totalNeighbours - 1
    neighborId = neighbours(boid.id, idx)
    neighbor = boids(neighborId)
    distance! = getDistanceBetweenPoints(boid.position, neighbor.position)
    IF ((distance! > 0!) AND (distance! < CROWDRADIUS)) THEN
      distanceVector.x = boid.position.x
      distanceVector.y = boid.position.y
      CALL calculateVectorSubstraction(distanceVector, neighbor.position)
      CALL calculateVectorNormalization(distanceVector, normalizedVector)
      CALL calculateVectorDivision(normalizedVector, distance!)
      boid.movement.x = boid.movement.x + normalizedVector.x
      boid.movement.y = boid.movement.y + normalizedVector.y
    END IF
  NEXT idx

  ' Calculate force to avoid obstacles
  FOR id = 0 TO NUMOBSTACLES - 1
    obstacle = obstacles(id)
    distance! = getDistanceBetweenPoints(boid.position, obstacle.position)
    IF ((distance! > 0!) AND (distance! < OBSTACLEACTIONRADIUS)) THEN
      distanceVector.x = boid.position.x
      distanceVector.y = boid.position.y
      CALL calculateVectorSubstraction(distanceVector, obstacle.position)
      CALL calculateVectorNormalization(distanceVector, normalizedVector)
      CALL calculateVectorDivision(normalizedVector, distance!)
      boid.movement.x = boid.movement.x + 8! * normalizedVector.x
      boid.movement.y = boid.movement.y + 8! * normalizedVector.y
    END IF
  NEXT id

  ' Create noise movement
  boid.movement.x = boid.movement.x + (.35 * ((RND(1) * 2!) - 1!))
  boid.movement.y = boid.movement.y + (.35 * ((RND(1) * 2!) - 1!))

  ' Limit the movement
  magnitude! = getVectorMagnitude(boid.movement)
  IF magnitude! > MAXSPEED THEN
    ratio! = MAXSPEED / magnitude!
    boid.movement.x = boid.movement.x * ratio!
    boid.movement.y = boid.movement.y * ratio!
  END IF
END SUB

SUB delay (seconds#)
  start# = TIMER
  DO
  LOOP UNTIL (TIMER - start#) >= seconds#
END SUB

SUB drawBoid (boid AS TBoid)
  LINE (boid.prevPrevPosition.x, boid.prevPrevPosition.y)-(boid.prevPosition.x, boid.prevPosition.y), 0
  IF (ABS(boid.prevPosition.x - boid.position.x) < (SCREENWIDTH - MAXSPEED - 1)) AND (ABS(boid.prevPosition.y - boid.position.y) < (SCREENHEIGHT - MAXSPEED - 1)) THEN
    LINE (boid.prevPosition.x, boid.prevPosition.y)-(boid.position.x, boid.position.y), 1
  END IF
END SUB

SUB drawBoids
  FOR id = 0 TO NUMBOIDS - 1
    CALL drawBoid(boids(id))
  NEXT id
END SUB

SUB drawObstacle (obstacle AS TObstacle)
  CIRCLE (obstacle.position.x, obstacle.position.y), OBSTACLERADIUS, 2
  'CIRCLE (obstacle.position.x, obstacle.position.y), OBSTACLEACTIONRADIUS, 3
  'PSET (obstacle.position.x, obstacle.position.y), 3
END SUB

SUB drawObstacles
  FOR id = 0 TO NUMOBSTACLES - 1
    CALL drawObstacle(obstacles(id))
  NEXT id
END SUB

FUNCTION getDistanceBetweenPoints! (a AS TVector, b AS TVector)
  getDistanceBetweenPoints = SQR((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)
END FUNCTION

FUNCTION getVectorMagnitude! (a AS TVector)
  getVectorMagnitude = SQR((a.x ^ 2) + (a.y ^ 2))
END FUNCTION

SUB initializeBoids
  FOR id = 0 TO NUMBOIDS - 1
    boids(id).id = id
    boids(id).position.x = INT(RND * (SCREENWIDTH + 1))
    boids(id).position.y = INT(RND * (SCREENHEIGHT + 1))
    boids(id).prevPosition.x = boids(id).position.x
    boids(id).prevPosition.y = boids(id).position.y
    boids(id).prevPrevPosition.x = boids(id).position.x
    boids(id).prevPrevPosition.y = boids(id).position.y
    boids(id).movement.x = .5 * ((RND(1) * 2!) - 1!)
    boids(id).movement.y = .5 * ((RND(1) * 2!) - 1!)
    boids(id).totalNeighbours = 0
  NEXT id
END SUB

SUB initializeObstacles
  FOR id = 0 TO NUMOBSTACLES - 1
    obstacles(id).position.x = INT(RND * (SCREENWIDTH - (OBSTACLERADIUS * 2!) + 1!) + OBSTACLERADIUS)
    obstacles(id).position.y = INT(RND * (SCREENHEIGHT - (OBSTACLERADIUS * 2!) + 1!) + OBSTACLERADIUS)
  NEXT id
END SUB

FUNCTION min! (a AS SINGLE, b AS SINGLE)
  IF a < b THEN
    min = a
  ELSE
    min = b
  END IF
END FUNCTION

SUB setVectorMagnitude (a AS TVector, m AS SINGLE)
  magnitude! = getVectorMagnitude(a)
  IF magnitude! > 0! THEN
    ratio! = m / magnitude!
    a.x = a.x * ratio!
    a.y = a.y * ratio!
  END IF
END SUB

SUB updateBoid (boid AS TBoid)
  CALL updateBoidNeighbors(boid)
  CALL computeBoidForces(boid)
  CALL updateBoidPosition(boid)
END SUB

SUB updateBoidNeighbors (boid AS TBoid)
  numNeighbors% = 0
  FOR id = 0 TO NUMBOIDS - 1
    IF id <> boid.id THEN
      IF (ABS(boids(id).position.x - boid.position.x) < NEIGHBORRADIUS) AND (ABS(boids(id).position.y - boid.position.y) < NEIGHBORRADIUS) THEN
	neighbours(boid.id, numNeighbors%) = id
	numNeighbors% = numNeighbors% + 1
      END IF
    END IF
  NEXT id
  boid.totalNeighbours = numNeighbors%
END SUB

SUB updateBoidPosition (boid AS TBoid)
  boid.prevPrevPosition.x = boid.prevPosition.x
  boid.prevPrevPosition.y = boid.prevPosition.y
  boid.prevPosition.x = boid.position.x
  boid.prevPosition.y = boid.position.y
  boid.position.x = (boid.position.x + boid.movement.x + SCREENWIDTH) MOD SCREENWIDTH
  boid.position.y = (boid.position.y + boid.movement.y + SCREENHEIGHT) MOD SCREENHEIGHT
END SUB

SUB updateBoids
  FOR id = 0 TO NUMBOIDS - 1
    CALL updateBoid(boids(id))
  NEXT id
END SUB

