module Complex

%default total

test : String
test = "Hello from Idris2!"

export
record LateralN where
  constructor LN
  magnitud: Double
  angulo: Double

cartesiano : LateralN -> Pair Double Double
cartesiano a = MkPair (a.magnitud * cos a.angulo) (a.magnitud * sin a.angulo)

Polar : Pair Double Double -> LateralN
Polar (MkPair a b) = LN (a*a + b*b) (atan (b/a))


public export
Num LateralN where
  (+) x y =
    let (realx, imx) : (Double, Double) = cartesiano x
        (realy, imy)  := cartesiano y
        suma := ( realx + imy , realx + imy )
      in
    Polar suma
  (*) varx y = LN (varx.magnitud  * y.magnitud) ( varx.angulo * varx.angulo )
  fromInteger var = LN (cast var) 0


public export
Neg LateralN where
  negate x = LN x.magnitud x.angulo * -1
  (-)  x y =
    let (magx, angx) : Pair Double Double = (cartesiano x)
        (magy, angy)  := cartesiano y
        resta := ( magx - magy , angx - angy )
      in
    Polar resta


public export 
Fractional LateralN where 
    (/) x y = LN (x.magnitud / y.magnitud) (x.angulo - y.angulo)
    recip  x = LN (recip x.magnitud) x.angulo


public export
Abso : LateralN -> LateralN
Abso x = LN x.magnitud 0

public export 
Abs LateralN where
  abs = Abso


complexLog : Double -> Double -> (Double, Double)
complexLog r theta =
  let log_r = log r
      log_theta = if (theta > pi) then (theta - 2 * pi)
                  else if (theta <= -pi) then (theta + 2 * pi)
                  else theta
  in (log_r, log_theta)

log : LateralN -> LateralN
log x = 
  let (uno,dos) = complexLog x.magnitud x.angulo
      in 
  LN uno dos

exp : LateralN -> LateralN
exp x =
  let r = x.magnitud
      theta = x.angulo
      exp_r = exp r
      cos_theta = cos theta
      sin_theta = sin theta
      re = exp_r * cos_theta
      im = exp_r * sin_theta
  in Polar (re, im)


complexPow : Double -> Double -> Double -> Double -> (Double, Double)
complexPow r1 theta1 r2 theta2 =
  let r' = pow r1 r2
      theta' = r2 * theta1 + theta2
  in (r', theta')


pow : LateralN -> LateralN -> LateralN
pow x y = LN (pow x.magnitud y.magnitud) (
  y.magnitud * x.angulo + x.angulo)