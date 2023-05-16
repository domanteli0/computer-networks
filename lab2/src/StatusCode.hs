module StatusCode(
    StatusCode(..)
  , fromIntStr
  , show
  , isSc100
  , isSc101
  , isSc102
  , isSc103
  , isSc200
  , isSc201
  , isSc202
  , isSc203
  , isSc204
  , isSc205
  , isSc206
  , isSc207
  , isSc208
  , isSc226
  , isSc300
  , isSc301
  , isSc302
  , isSc303
  , isSc304
  , isSc305
  , isSc306
  , isSc307
  , isSc308
  , isSc400
  , isSc401
  , isSc402
  , isSc403
  , isSc404
  , isSc405
  , isSc406
  , isSc407
  , isSc408
  , isSc409
  , isSc410
  , isSc411
  , isSc412
  , isSc413
  , isSc414
  , isSc415
  , isSc416
  , isSc417
  , isSc418
  , isSc420
  , isSc421
  , isSc422
  , isSc423
  , isSc424
  , isSc425
  , isSc426
  , isSc427
  , isSc428
  , isSc429
  , isSc431
  , isSc451
  , isSc500
  , isSc501
  , isSc502
  , isSc503
  , isSc504
  , isSc505
  , isSc506
  , isSc507
  , isSc508
  , isSc509
  , isSc510
  , isSc511
  , isSc3xx
  , isScOther
  , isSc
  , toInt
) where

data StatusCode =
    Sc100 String
  | Sc101 String
  | Sc102 String
  | Sc103 String

  | Sc200 String
  | Sc201 String
  | Sc202 String
  | Sc203 String
  | Sc204 String
  | Sc205 String
  | Sc206 String
  | Sc207 String -- WebDAV
  | Sc208 String -- WebDAV
  | Sc226 String -- HTTP delta encoding

  | Sc300 String
  | Sc301 String
  | Sc302 String
  | Sc303 String
  | Sc304 String
  | Sc305 String -- deprecated
  | Sc306 String -- no longer used
  | Sc307 String
  | Sc308 String

  | Sc400 String
  | Sc401 String
  | Sc402 String
  | Sc403 String
  | Sc404 String
  | Sc405 String
  | Sc406 String
  | Sc407 String
  | Sc408 String
  | Sc409 String
  | Sc410 String
  | Sc411 String
  | Sc412 String
  | Sc413 String
  | Sc414 String
  | Sc415 String
  | Sc416 String
  | Sc417 String
  | Sc418 String
  -- 419 is not specified
  | Sc420 String
  | Sc421 String
  | Sc422 String
  | Sc423 String
  | Sc424 String
  | Sc425 String
  | Sc426 String
  | Sc427 String
  | Sc428 String
  | Sc429 String
  | Sc431 String
  | Sc451 String

  | Sc500 String
  | Sc501 String
  | Sc502 String
  | Sc503 String
  | Sc504 String
  | Sc505 String
  | Sc506 String
  | Sc507 String
  | Sc508 String
  | Sc509 String
  | Sc510 String
  | Sc511 String

  | ScOther Int String

instance Show StatusCode where
  show (Sc100 str) = "100 " ++ str
  show (Sc101 str) = "101 " ++ str
  show (Sc102 str) = "102 " ++ str
  show (Sc103 str) = "103 " ++ str

  show (Sc200 str) = "200 " ++ str
  show (Sc201 str) = "201 " ++ str
  show (Sc202 str) = "202 " ++ str
  show (Sc203 str) = "203 " ++ str
  show (Sc204 str) = "204 " ++ str
  show (Sc205 str) = "205 " ++ str
  show (Sc206 str) = "206 " ++ str
  show (Sc207 str) = "207 " ++ str
  show (Sc208 str) = "208 " ++ str
  show (Sc226 str) = "226 " ++ str

  show (Sc300 str) = "300 " ++ str
  show (Sc301 str) = "301 " ++ str
  show (Sc302 str) = "302 " ++ str
  show (Sc303 str) = "303 " ++ str
  show (Sc304 str) = "304 " ++ str
  show (Sc305 str) = "305 " ++ str
  show (Sc306 str) = "306 " ++ str
  show (Sc307 str) = "307 " ++ str
  show (Sc308 str) = "308 " ++ str

  show (Sc400 str) = "400 " ++ str
  show (Sc401 str) = "401 " ++ str
  show (Sc402 str) = "402 " ++ str
  show (Sc403 str) = "403 " ++ str
  show (Sc404 str) = "404 " ++ str
  show (Sc405 str) = "405 " ++ str
  show (Sc406 str) = "406 " ++ str
  show (Sc407 str) = "407 " ++ str
  show (Sc408 str) = "408 " ++ str
  show (Sc409 str) = "409 " ++ str
  show (Sc410 str) = "410 " ++ str
  show (Sc411 str) = "411 " ++ str
  show (Sc412 str) = "412 " ++ str
  show (Sc413 str) = "413 " ++ str
  show (Sc414 str) = "414 " ++ str
  show (Sc415 str) = "415 " ++ str
  show (Sc416 str) = "416 " ++ str
  show (Sc417 str) = "417 " ++ str
  show (Sc418 str) = "418 " ++ str
  show (Sc420 str) = "420 " ++ str
  show (Sc421 str) = "421 " ++ str
  show (Sc422 str) = "422 " ++ str
  show (Sc423 str) = "423 " ++ str
  show (Sc424 str) = "424 " ++ str
  show (Sc425 str) = "425 " ++ str
  show (Sc426 str) = "426 " ++ str
  show (Sc427 str) = "427 " ++ str
  show (Sc428 str) = "428 " ++ str
  show (Sc429 str) = "429 " ++ str
  show (Sc431 str) = "431 " ++ str
  show (Sc451 str) = "451 " ++ str

  show (Sc500 str) = "500 " ++ str
  show (Sc501 str) = "501 " ++ str
  show (Sc502 str) = "502 " ++ str
  show (Sc503 str) = "503 " ++ str
  show (Sc504 str) = "504 " ++ str
  show (Sc505 str) = "505 " ++ str
  show (Sc506 str) = "506 " ++ str
  show (Sc507 str) = "507 " ++ str
  show (Sc508 str) = "508 " ++ str
  show (Sc509 str) = "509 " ++ str
  show (Sc510 str) = "510 " ++ str
  show (Sc511 str) = "511 " ++ str

  show (ScOther code str) = show code ++ str

fromIntStr :: Int -> String -> StatusCode
fromIntStr 100 = Sc100
fromIntStr 101 = Sc101
fromIntStr 102 = Sc102
fromIntStr 103 = Sc103

fromIntStr 200 = Sc200
fromIntStr 201 = Sc201
fromIntStr 202 = Sc202
fromIntStr 203 = Sc203
fromIntStr 204 = Sc204
fromIntStr 205 = Sc205
fromIntStr 206 = Sc206
fromIntStr 207 = Sc207
fromIntStr 208 = Sc208
fromIntStr 226 = Sc226

fromIntStr 300 = Sc300
fromIntStr 301 = Sc301
fromIntStr 302 = Sc302
fromIntStr 303 = Sc303
fromIntStr 304 = Sc304
fromIntStr 305 = Sc305
fromIntStr 306 = Sc306
fromIntStr 307 = Sc307
fromIntStr 308 = Sc308

fromIntStr 400 = Sc400
fromIntStr 401 = Sc401
fromIntStr 402 = Sc402
fromIntStr 403 = Sc403
fromIntStr 404 = Sc404
fromIntStr 405 = Sc405
fromIntStr 406 = Sc406
fromIntStr 407 = Sc407
fromIntStr 408 = Sc408
fromIntStr 409 = Sc409
fromIntStr 410 = Sc410
fromIntStr 411 = Sc411
fromIntStr 412 = Sc412
fromIntStr 413 = Sc413
fromIntStr 414 = Sc414
fromIntStr 415 = Sc415
fromIntStr 416 = Sc416
fromIntStr 417 = Sc417
fromIntStr 418 = Sc418
fromIntStr 420 = Sc420
fromIntStr 421 = Sc421
fromIntStr 422 = Sc422
fromIntStr 423 = Sc423
fromIntStr 424 = Sc424
fromIntStr 425 = Sc425
fromIntStr 426 = Sc426
fromIntStr 427 = Sc427
fromIntStr 428 = Sc428
fromIntStr 429 = Sc429
fromIntStr 431 = Sc431
fromIntStr 451 = Sc451

fromIntStr 500 = Sc500
fromIntStr 501 = Sc501
fromIntStr 502 = Sc502
fromIntStr 503 = Sc503
fromIntStr 504 = Sc504
fromIntStr 505 = Sc505
fromIntStr 506 = Sc506
fromIntStr 507 = Sc507
fromIntStr 508 = Sc508
fromIntStr 509 = Sc509
fromIntStr 510 = Sc510
fromIntStr 511 = Sc511

fromIntStr int = ScOther int


isSc100 :: StatusCode -> Bool
isSc100 (Sc100 _) = True
isSc100 _ = False

isSc101 :: StatusCode -> Bool
isSc101 (Sc101 _) = True
isSc101 _ = False

isSc102 :: StatusCode -> Bool
isSc102 (Sc102 _) = True
isSc102 _ = False

isSc103 :: StatusCode -> Bool
isSc103 (Sc103 _) = True
isSc103 _ = False

isSc200 :: StatusCode -> Bool
isSc200 (Sc200 _) = True
isSc200 _ = False

isSc201 :: StatusCode -> Bool
isSc201 (Sc201 _) = True
isSc201 _ = False

isSc202 :: StatusCode -> Bool
isSc202 (Sc202 _) = True
isSc202 _ = False

isSc203 :: StatusCode -> Bool
isSc203 (Sc203 _) = True
isSc203 _ = False

isSc204 :: StatusCode -> Bool
isSc204 (Sc204 _) = True
isSc204 _ = False

isSc205 :: StatusCode -> Bool
isSc205 (Sc205 _) = True
isSc205 _ = False

isSc206 :: StatusCode -> Bool
isSc206 (Sc206 _) = True
isSc206 _ = False

isSc207 :: StatusCode -> Bool
isSc207 (Sc207 _) = True
isSc207 _ = False

isSc208 :: StatusCode -> Bool
isSc208 (Sc208 _) = True
isSc208 _ = False

isSc226 :: StatusCode -> Bool
isSc226 (Sc226 _) = True
isSc226 _ = False

isSc300 :: StatusCode -> Bool
isSc300 (Sc300 _) = True
isSc300 _ = False

isSc301 :: StatusCode -> Bool
isSc301 (Sc301 _) = True
isSc301 _ = False

isSc302 :: StatusCode -> Bool
isSc302 (Sc302 _) = True
isSc302 _ = False

isSc303 :: StatusCode -> Bool
isSc303 (Sc303 _) = True
isSc303 _ = False

isSc304 :: StatusCode -> Bool
isSc304 (Sc304 _) = True
isSc304 _ = False

isSc305 :: StatusCode -> Bool
isSc305 (Sc305 _) = True
isSc305 _ = False

isSc306 :: StatusCode -> Bool
isSc306 (Sc306 _) = True
isSc306 _ = False

isSc307 :: StatusCode -> Bool
isSc307 (Sc307 _) = True
isSc307 _ = False

isSc308 :: StatusCode -> Bool
isSc308 (Sc308 _) = True
isSc308 _ = False

isSc400 :: StatusCode -> Bool
isSc400 (Sc400 _) = True
isSc400 _ = False

isSc401 :: StatusCode -> Bool
isSc401 (Sc401 _) = True
isSc401 _ = False

isSc402 :: StatusCode -> Bool
isSc402 (Sc402 _) = True
isSc402 _ = False

isSc403 :: StatusCode -> Bool
isSc403 (Sc403 _) = True
isSc403 _ = False

isSc404 :: StatusCode -> Bool
isSc404 (Sc404 _) = True
isSc404 _ = False

isSc405 :: StatusCode -> Bool
isSc405 (Sc405 _) = True
isSc405 _ = False

isSc406 :: StatusCode -> Bool
isSc406 (Sc406 _) = True
isSc406 _ = False

isSc407 :: StatusCode -> Bool
isSc407 (Sc407 _) = True
isSc407 _ = False

isSc408 :: StatusCode -> Bool
isSc408 (Sc408 _) = True
isSc408 _ = False

isSc409 :: StatusCode -> Bool
isSc409 (Sc409 _) = True
isSc409 _ = False

isSc410 :: StatusCode -> Bool
isSc410 (Sc410 _) = True
isSc410 _ = False

isSc411 :: StatusCode -> Bool
isSc411 (Sc411 _) = True
isSc411 _ = False

isSc412 :: StatusCode -> Bool
isSc412 (Sc412 _) = True
isSc412 _ = False

isSc413 :: StatusCode -> Bool
isSc413 (Sc413 _) = True
isSc413 _ = False

isSc414 :: StatusCode -> Bool
isSc414 (Sc414 _) = True
isSc414 _ = False

isSc415 :: StatusCode -> Bool
isSc415 (Sc415 _) = True
isSc415 _ = False

isSc416 :: StatusCode -> Bool
isSc416 (Sc416 _) = True
isSc416 _ = False

isSc417 :: StatusCode -> Bool
isSc417 (Sc417 _) = True
isSc417 _ = False

isSc418 :: StatusCode -> Bool
isSc418 (Sc418 _) = True
isSc418 _ = False

isSc420 :: StatusCode -> Bool
isSc420 (Sc420 _) = True
isSc420 _ = False

isSc421 :: StatusCode -> Bool
isSc421 (Sc421 _) = True
isSc421 _ = False

isSc422 :: StatusCode -> Bool
isSc422 (Sc422 _) = True
isSc422 _ = False

isSc423 :: StatusCode -> Bool
isSc423 (Sc423 _) = True
isSc423 _ = False

isSc424 :: StatusCode -> Bool
isSc424 (Sc424 _) = True
isSc424 _ = False

isSc425 :: StatusCode -> Bool
isSc425 (Sc425 _) = True
isSc425 _ = False

isSc426 :: StatusCode -> Bool
isSc426 (Sc426 _) = True
isSc426 _ = False

isSc427 :: StatusCode -> Bool
isSc427 (Sc427 _) = True
isSc427 _ = False

isSc428 :: StatusCode -> Bool
isSc428 (Sc428 _) = True
isSc428 _ = False

isSc429 :: StatusCode -> Bool
isSc429 (Sc429 _) = True
isSc429 _ = False

isSc431 :: StatusCode -> Bool
isSc431 (Sc431 _) = True
isSc431 _ = False

isSc451 :: StatusCode -> Bool
isSc451 (Sc451 _) = True
isSc451 _ = False

isSc500 :: StatusCode -> Bool
isSc500 (Sc500 _) = True
isSc500 _ = False

isSc501 :: StatusCode -> Bool
isSc501 (Sc501 _) = True
isSc501 _ = False

isSc502 :: StatusCode -> Bool
isSc502 (Sc502 _) = True
isSc502 _ = False

isSc503 :: StatusCode -> Bool
isSc503 (Sc503 _) = True
isSc503 _ = False

isSc504 :: StatusCode -> Bool
isSc504 (Sc504 _) = True
isSc504 _ = False

isSc505 :: StatusCode -> Bool
isSc505 (Sc505 _) = True
isSc505 _ = False

isSc506 :: StatusCode -> Bool
isSc506 (Sc506 _) = True
isSc506 _ = False

isSc507 :: StatusCode -> Bool
isSc507 (Sc507 _) = True
isSc507 _ = False

isSc508 :: StatusCode -> Bool
isSc508 (Sc508 _) = True
isSc508 _ = False

isSc509 :: StatusCode -> Bool
isSc509 (Sc509 _) = True
isSc509 _ = False

isSc510 :: StatusCode -> Bool
isSc510 (Sc510 _) = True
isSc510 _ = False

isSc511 :: StatusCode -> Bool
isSc511 (Sc511 _) = True
isSc511 _ = False

isScOther :: StatusCode -> Bool
isScOther (ScOther _ _) = True
isScOther _ = False

mkIsScxxx range stat = any ( uncurry isSc ) $ zip (repeat stat) range

isSc3xx :: StatusCode -> Bool
isSc3xx = mkIsScxxx [300..308]

isSc :: StatusCode -> Int -> Bool
isSc (Sc100 _) 100 = True
isSc (Sc101 _) 101 = True
isSc (Sc102 _) 102 = True
isSc (Sc103 _) 103 = True
isSc (Sc200 _) 200 = True
isSc (Sc201 _) 201 = True
isSc (Sc202 _) 202 = True
isSc (Sc203 _) 203 = True
isSc (Sc204 _) 204 = True
isSc (Sc205 _) 205 = True
isSc (Sc206 _) 206 = True
isSc (Sc207 _) 207 = True
isSc (Sc208 _) 208 = True
isSc (Sc226 _) 226 = True
isSc (Sc300 _) 300 = True
isSc (Sc301 _) 301 = True
isSc (Sc302 _) 302 = True
isSc (Sc303 _) 303 = True
isSc (Sc304 _) 304 = True
isSc (Sc305 _) 305 = True
isSc (Sc306 _) 306 = True
isSc (Sc307 _) 307 = True
isSc (Sc308 _) 308 = True
isSc (Sc400 _) 400 = True
isSc (Sc401 _) 401 = True
isSc (Sc402 _) 402 = True
isSc (Sc403 _) 403 = True
isSc (Sc404 _) 404 = True
isSc (Sc405 _) 405 = True
isSc (Sc406 _) 406 = True
isSc (Sc407 _) 407 = True
isSc (Sc408 _) 408 = True
isSc (Sc409 _) 409 = True
isSc (Sc410 _) 410 = True
isSc (Sc411 _) 411 = True
isSc (Sc412 _) 412 = True
isSc (Sc413 _) 413 = True
isSc (Sc414 _) 414 = True
isSc (Sc415 _) 415 = True
isSc (Sc416 _) 416 = True
isSc (Sc417 _) 417 = True
isSc (Sc418 _) 418 = True
isSc (Sc420 _) 420 = True
isSc (Sc421 _) 421 = True
isSc (Sc422 _) 422 = True
isSc (Sc423 _) 423 = True
isSc (Sc424 _) 424 = True
isSc (Sc425 _) 425 = True
isSc (Sc426 _) 426 = True
isSc (Sc427 _) 427 = True
isSc (Sc428 _) 428 = True
isSc (Sc429 _) 429 = True
isSc (Sc431 _) 431 = True
isSc (Sc451 _) 451 = True
isSc (Sc500 _) 500 = True
isSc (Sc501 _) 501 = True
isSc (Sc502 _) 502 = True
isSc (Sc503 _) 503 = True
isSc (Sc504 _) 504 = True
isSc (Sc505 _) 505 = True
isSc (Sc506 _) 506 = True
isSc (Sc507 _) 507 = True
isSc (Sc508 _) 508 = True
isSc (Sc509 _) 509 = True
isSc (Sc510 _) 510 = True
isSc (Sc511 _) 511 = True
isSc (ScOther num _) num' = num == num'
isSc _ _ = False

toInt :: StatusCode -> Int
toInt (Sc100 _) = 100
toInt (Sc101 _) = 101
toInt (Sc102 _) = 102
toInt (Sc103 _) = 103
toInt (Sc200 _) = 200
toInt (Sc201 _) = 201
toInt (Sc202 _) = 202
toInt (Sc203 _) = 203
toInt (Sc204 _) = 204
toInt (Sc205 _) = 205
toInt (Sc206 _) = 206
toInt (Sc207 _) = 207
toInt (Sc208 _) = 208
toInt (Sc226 _) = 226
toInt (Sc300 _) = 300
toInt (Sc301 _) = 301
toInt (Sc302 _) = 302
toInt (Sc303 _) = 303
toInt (Sc304 _) = 304
toInt (Sc305 _) = 305
toInt (Sc306 _) = 306
toInt (Sc307 _) = 307
toInt (Sc308 _) = 308
toInt (Sc400 _) = 400
toInt (Sc401 _) = 401
toInt (Sc402 _) = 402
toInt (Sc403 _) = 403
toInt (Sc404 _) = 404
toInt (Sc405 _) = 405
toInt (Sc406 _) = 406
toInt (Sc407 _) = 407
toInt (Sc408 _) = 408
toInt (Sc409 _) = 409
toInt (Sc410 _) = 410
toInt (Sc411 _) = 411
toInt (Sc412 _) = 412
toInt (Sc413 _) = 413
toInt (Sc414 _) = 414
toInt (Sc415 _) = 415
toInt (Sc416 _) = 416
toInt (Sc417 _) = 417
toInt (Sc418 _) = 418
toInt (Sc420 _) = 420
toInt (Sc421 _) = 421
toInt (Sc422 _) = 422
toInt (Sc423 _) = 423
toInt (Sc424 _) = 424
toInt (Sc425 _) = 425
toInt (Sc426 _) = 426
toInt (Sc427 _) = 427
toInt (Sc428 _) = 428
toInt (Sc429 _) = 429
toInt (Sc431 _) = 431
toInt (Sc451 _) = 451
toInt (Sc500 _) = 500
toInt (Sc501 _) = 501
toInt (Sc502 _) = 502
toInt (Sc503 _) = 503
toInt (Sc504 _) = 504
toInt (Sc505 _) = 505
toInt (Sc506 _) = 506
toInt (Sc507 _) = 507
toInt (Sc508 _) = 508
toInt (Sc509 _) = 509
toInt (Sc510 _) = 510
toInt (Sc511 _) = 511
toInt (ScOther num _) = num