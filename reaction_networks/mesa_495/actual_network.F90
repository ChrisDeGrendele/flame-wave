module actual_network

  use network_properties
  use physical_constants, only: ERG_PER_MeV
  use amrex_fort_module, only: rt => amrex_real
  use partition_function_table

  implicit none

  public

  character (len=32), parameter :: network_name = "pynucastro"

  real(rt), parameter :: avo = 6.0221417930e23_rt
  real(rt), parameter :: c_light = 2.99792458e10_rt
  real(rt), parameter :: enuc_conv2 = -avo*c_light*c_light

  real(rt), parameter :: ev2erg  = 1.60217648740e-12_rt
  real(rt), parameter :: mev2erg = ev2erg * 1.0e6_rt
  real(rt), parameter :: mev2gr  = mev2erg / c_light**2

  real(rt), parameter :: mass_neutron  = 1.67492721184e-24_rt
  real(rt), parameter :: mass_proton   = 1.67262163783e-24_rt
  real(rt), parameter :: mass_electron = 9.10938215450e-28_rt

  integer, parameter :: nrates = 5689


  ! For each rate, we need: rate, drate/dT, screening, dscreening/dT
  integer, parameter :: num_rate_groups = 4

  ! Number of reaclib rates
  integer, parameter :: nrat_reaclib = 5685
  integer, parameter :: number_reaclib_sets = 6425

  ! Number of tabular rates
  integer, parameter :: nrat_tabular = 4

  ! Binding Energies Per Nucleon (MeV)
  real(rt) :: ebind_per_nucleon(nspec)

  ! bion: Binding Energies (ergs)

  ! Nuclides
  integer, parameter :: jn   = 1
  integer, parameter :: jp   = 2
  integer, parameter :: jd   = 3
  integer, parameter :: jt   = 4
  integer, parameter :: jhe3   = 5
  integer, parameter :: jhe4   = 6
  integer, parameter :: jli6   = 7
  integer, parameter :: jli7   = 8
  integer, parameter :: jbe7   = 9
  integer, parameter :: jbe9   = 10
  integer, parameter :: jb8   = 11
  integer, parameter :: jb10   = 12
  integer, parameter :: jb11   = 13
  integer, parameter :: jc11   = 14
  integer, parameter :: jc12   = 15
  integer, parameter :: jc13   = 16
  integer, parameter :: jc14   = 17
  integer, parameter :: jn12   = 18
  integer, parameter :: jn13   = 19
  integer, parameter :: jn14   = 20
  integer, parameter :: jn15   = 21
  integer, parameter :: jo14   = 22
  integer, parameter :: jo15   = 23
  integer, parameter :: jo16   = 24
  integer, parameter :: jo17   = 25
  integer, parameter :: jo18   = 26
  integer, parameter :: jo19   = 27
  integer, parameter :: jo20   = 28
  integer, parameter :: jf17   = 29
  integer, parameter :: jf18   = 30
  integer, parameter :: jf19   = 31
  integer, parameter :: jf20   = 32
  integer, parameter :: jf21   = 33
  integer, parameter :: jne17   = 34
  integer, parameter :: jne18   = 35
  integer, parameter :: jne19   = 36
  integer, parameter :: jne20   = 37
  integer, parameter :: jne21   = 38
  integer, parameter :: jne22   = 39
  integer, parameter :: jne23   = 40
  integer, parameter :: jne24   = 41
  integer, parameter :: jna19   = 42
  integer, parameter :: jna20   = 43
  integer, parameter :: jna21   = 44
  integer, parameter :: jna22   = 45
  integer, parameter :: jna23   = 46
  integer, parameter :: jna24   = 47
  integer, parameter :: jna25   = 48
  integer, parameter :: jna26   = 49
  integer, parameter :: jna27   = 50
  integer, parameter :: jmg20   = 51
  integer, parameter :: jmg21   = 52
  integer, parameter :: jmg22   = 53
  integer, parameter :: jmg23   = 54
  integer, parameter :: jmg24   = 55
  integer, parameter :: jmg25   = 56
  integer, parameter :: jmg26   = 57
  integer, parameter :: jmg27   = 58
  integer, parameter :: jmg28   = 59
  integer, parameter :: jmg29   = 60
  integer, parameter :: jal22   = 61
  integer, parameter :: jal23   = 62
  integer, parameter :: jal24   = 63
  integer, parameter :: jal25   = 64
  integer, parameter :: jal26   = 65
  integer, parameter :: jal27   = 66
  integer, parameter :: jal28   = 67
  integer, parameter :: jal29   = 68
  integer, parameter :: jal30   = 69
  integer, parameter :: jal31   = 70
  integer, parameter :: jsi23   = 71
  integer, parameter :: jsi24   = 72
  integer, parameter :: jsi25   = 73
  integer, parameter :: jsi26   = 74
  integer, parameter :: jsi27   = 75
  integer, parameter :: jsi28   = 76
  integer, parameter :: jsi29   = 77
  integer, parameter :: jsi30   = 78
  integer, parameter :: jsi31   = 79
  integer, parameter :: jsi32   = 80
  integer, parameter :: jsi33   = 81
  integer, parameter :: jsi34   = 82
  integer, parameter :: jp27   = 83
  integer, parameter :: jp28   = 84
  integer, parameter :: jp29   = 85
  integer, parameter :: jp30   = 86
  integer, parameter :: jp31   = 87
  integer, parameter :: jp32   = 88
  integer, parameter :: jp33   = 89
  integer, parameter :: jp34   = 90
  integer, parameter :: jp35   = 91
  integer, parameter :: jp36   = 92
  integer, parameter :: jp37   = 93
  integer, parameter :: jp38   = 94
  integer, parameter :: js28   = 95
  integer, parameter :: js29   = 96
  integer, parameter :: js30   = 97
  integer, parameter :: js31   = 98
  integer, parameter :: js32   = 99
  integer, parameter :: js33   = 100
  integer, parameter :: js34   = 101
  integer, parameter :: js35   = 102
  integer, parameter :: js36   = 103
  integer, parameter :: js37   = 104
  integer, parameter :: js38   = 105
  integer, parameter :: js39   = 106
  integer, parameter :: js40   = 107
  integer, parameter :: js41   = 108
  integer, parameter :: js42   = 109
  integer, parameter :: jcl31   = 110
  integer, parameter :: jcl32   = 111
  integer, parameter :: jcl33   = 112
  integer, parameter :: jcl34   = 113
  integer, parameter :: jcl35   = 114
  integer, parameter :: jcl36   = 115
  integer, parameter :: jcl37   = 116
  integer, parameter :: jcl38   = 117
  integer, parameter :: jcl39   = 118
  integer, parameter :: jcl40   = 119
  integer, parameter :: jcl41   = 120
  integer, parameter :: jcl42   = 121
  integer, parameter :: jcl43   = 122
  integer, parameter :: jcl44   = 123
  integer, parameter :: jcl45   = 124
  integer, parameter :: jar32   = 125
  integer, parameter :: jar33   = 126
  integer, parameter :: jar34   = 127
  integer, parameter :: jar35   = 128
  integer, parameter :: jar36   = 129
  integer, parameter :: jar37   = 130
  integer, parameter :: jar38   = 131
  integer, parameter :: jar39   = 132
  integer, parameter :: jar40   = 133
  integer, parameter :: jar41   = 134
  integer, parameter :: jar42   = 135
  integer, parameter :: jar43   = 136
  integer, parameter :: jar44   = 137
  integer, parameter :: jar45   = 138
  integer, parameter :: jar46   = 139
  integer, parameter :: jk35   = 140
  integer, parameter :: jk36   = 141
  integer, parameter :: jk37   = 142
  integer, parameter :: jk38   = 143
  integer, parameter :: jk39   = 144
  integer, parameter :: jk40   = 145
  integer, parameter :: jk41   = 146
  integer, parameter :: jk42   = 147
  integer, parameter :: jk43   = 148
  integer, parameter :: jk44   = 149
  integer, parameter :: jk45   = 150
  integer, parameter :: jk46   = 151
  integer, parameter :: jk47   = 152
  integer, parameter :: jk48   = 153
  integer, parameter :: jk49   = 154
  integer, parameter :: jca36   = 155
  integer, parameter :: jca37   = 156
  integer, parameter :: jca38   = 157
  integer, parameter :: jca39   = 158
  integer, parameter :: jca40   = 159
  integer, parameter :: jca41   = 160
  integer, parameter :: jca42   = 161
  integer, parameter :: jca43   = 162
  integer, parameter :: jca44   = 163
  integer, parameter :: jca45   = 164
  integer, parameter :: jca46   = 165
  integer, parameter :: jca47   = 166
  integer, parameter :: jca48   = 167
  integer, parameter :: jca49   = 168
  integer, parameter :: jsc40   = 169
  integer, parameter :: jsc41   = 170
  integer, parameter :: jsc42   = 171
  integer, parameter :: jsc43   = 172
  integer, parameter :: jsc44   = 173
  integer, parameter :: jsc45   = 174
  integer, parameter :: jsc46   = 175
  integer, parameter :: jsc47   = 176
  integer, parameter :: jsc48   = 177
  integer, parameter :: jsc49   = 178
  integer, parameter :: jsc50   = 179
  integer, parameter :: jsc51   = 180
  integer, parameter :: jti41   = 181
  integer, parameter :: jti42   = 182
  integer, parameter :: jti43   = 183
  integer, parameter :: jti44   = 184
  integer, parameter :: jti45   = 185
  integer, parameter :: jti46   = 186
  integer, parameter :: jti47   = 187
  integer, parameter :: jti48   = 188
  integer, parameter :: jti49   = 189
  integer, parameter :: jti50   = 190
  integer, parameter :: jti51   = 191
  integer, parameter :: jti52   = 192
  integer, parameter :: jti53   = 193
  integer, parameter :: jv43   = 194
  integer, parameter :: jv44   = 195
  integer, parameter :: jv45   = 196
  integer, parameter :: jv46   = 197
  integer, parameter :: jv47   = 198
  integer, parameter :: jv48   = 199
  integer, parameter :: jv49   = 200
  integer, parameter :: jv50   = 201
  integer, parameter :: jv51   = 202
  integer, parameter :: jv52   = 203
  integer, parameter :: jv53   = 204
  integer, parameter :: jv54   = 205
  integer, parameter :: jv55   = 206
  integer, parameter :: jcr44   = 207
  integer, parameter :: jcr45   = 208
  integer, parameter :: jcr46   = 209
  integer, parameter :: jcr47   = 210
  integer, parameter :: jcr48   = 211
  integer, parameter :: jcr49   = 212
  integer, parameter :: jcr50   = 213
  integer, parameter :: jcr51   = 214
  integer, parameter :: jcr52   = 215
  integer, parameter :: jcr53   = 216
  integer, parameter :: jcr54   = 217
  integer, parameter :: jcr55   = 218
  integer, parameter :: jcr56   = 219
  integer, parameter :: jcr57   = 220
  integer, parameter :: jcr58   = 221
  integer, parameter :: jmn46   = 222
  integer, parameter :: jmn47   = 223
  integer, parameter :: jmn48   = 224
  integer, parameter :: jmn49   = 225
  integer, parameter :: jmn50   = 226
  integer, parameter :: jmn51   = 227
  integer, parameter :: jmn52   = 228
  integer, parameter :: jmn53   = 229
  integer, parameter :: jmn54   = 230
  integer, parameter :: jmn55   = 231
  integer, parameter :: jmn56   = 232
  integer, parameter :: jmn57   = 233
  integer, parameter :: jmn58   = 234
  integer, parameter :: jmn59   = 235
  integer, parameter :: jmn60   = 236
  integer, parameter :: jmn61   = 237
  integer, parameter :: jfe47   = 238
  integer, parameter :: jfe48   = 239
  integer, parameter :: jfe49   = 240
  integer, parameter :: jfe50   = 241
  integer, parameter :: jfe51   = 242
  integer, parameter :: jfe52   = 243
  integer, parameter :: jfe53   = 244
  integer, parameter :: jfe54   = 245
  integer, parameter :: jfe55   = 246
  integer, parameter :: jfe56   = 247
  integer, parameter :: jfe57   = 248
  integer, parameter :: jfe58   = 249
  integer, parameter :: jfe59   = 250
  integer, parameter :: jfe60   = 251
  integer, parameter :: jfe61   = 252
  integer, parameter :: jfe62   = 253
  integer, parameter :: jfe63   = 254
  integer, parameter :: jfe64   = 255
  integer, parameter :: jfe65   = 256
  integer, parameter :: jfe66   = 257
  integer, parameter :: jco50   = 258
  integer, parameter :: jco51   = 259
  integer, parameter :: jco52   = 260
  integer, parameter :: jco53   = 261
  integer, parameter :: jco54   = 262
  integer, parameter :: jco55   = 263
  integer, parameter :: jco56   = 264
  integer, parameter :: jco57   = 265
  integer, parameter :: jco58   = 266
  integer, parameter :: jco59   = 267
  integer, parameter :: jco60   = 268
  integer, parameter :: jco61   = 269
  integer, parameter :: jco62   = 270
  integer, parameter :: jco63   = 271
  integer, parameter :: jco64   = 272
  integer, parameter :: jco65   = 273
  integer, parameter :: jco66   = 274
  integer, parameter :: jco67   = 275
  integer, parameter :: jni51   = 276
  integer, parameter :: jni52   = 277
  integer, parameter :: jni53   = 278
  integer, parameter :: jni54   = 279
  integer, parameter :: jni55   = 280
  integer, parameter :: jni56   = 281
  integer, parameter :: jni57   = 282
  integer, parameter :: jni58   = 283
  integer, parameter :: jni59   = 284
  integer, parameter :: jni60   = 285
  integer, parameter :: jni61   = 286
  integer, parameter :: jni62   = 287
  integer, parameter :: jni63   = 288
  integer, parameter :: jni64   = 289
  integer, parameter :: jni65   = 290
  integer, parameter :: jni66   = 291
  integer, parameter :: jni67   = 292
  integer, parameter :: jni68   = 293
  integer, parameter :: jcu55   = 294
  integer, parameter :: jcu56   = 295
  integer, parameter :: jcu57   = 296
  integer, parameter :: jcu58   = 297
  integer, parameter :: jcu59   = 298
  integer, parameter :: jcu60   = 299
  integer, parameter :: jcu61   = 300
  integer, parameter :: jcu62   = 301
  integer, parameter :: jcu63   = 302
  integer, parameter :: jcu64   = 303
  integer, parameter :: jcu65   = 304
  integer, parameter :: jcu66   = 305
  integer, parameter :: jcu67   = 306
  integer, parameter :: jcu68   = 307
  integer, parameter :: jcu69   = 308
  integer, parameter :: jzn57   = 309
  integer, parameter :: jzn58   = 310
  integer, parameter :: jzn59   = 311
  integer, parameter :: jzn60   = 312
  integer, parameter :: jzn61   = 313
  integer, parameter :: jzn62   = 314
  integer, parameter :: jzn63   = 315
  integer, parameter :: jzn64   = 316
  integer, parameter :: jzn65   = 317
  integer, parameter :: jzn66   = 318
  integer, parameter :: jzn67   = 319
  integer, parameter :: jzn68   = 320
  integer, parameter :: jzn69   = 321
  integer, parameter :: jzn70   = 322
  integer, parameter :: jzn71   = 323
  integer, parameter :: jzn72   = 324
  integer, parameter :: jga59   = 325
  integer, parameter :: jga60   = 326
  integer, parameter :: jga61   = 327
  integer, parameter :: jga62   = 328
  integer, parameter :: jga63   = 329
  integer, parameter :: jga64   = 330
  integer, parameter :: jga65   = 331
  integer, parameter :: jga66   = 332
  integer, parameter :: jga67   = 333
  integer, parameter :: jga68   = 334
  integer, parameter :: jga69   = 335
  integer, parameter :: jga70   = 336
  integer, parameter :: jga71   = 337
  integer, parameter :: jga72   = 338
  integer, parameter :: jga73   = 339
  integer, parameter :: jga74   = 340
  integer, parameter :: jga75   = 341
  integer, parameter :: jge62   = 342
  integer, parameter :: jge63   = 343
  integer, parameter :: jge64   = 344
  integer, parameter :: jge65   = 345
  integer, parameter :: jge66   = 346
  integer, parameter :: jge67   = 347
  integer, parameter :: jge68   = 348
  integer, parameter :: jge69   = 349
  integer, parameter :: jge70   = 350
  integer, parameter :: jge71   = 351
  integer, parameter :: jge72   = 352
  integer, parameter :: jge73   = 353
  integer, parameter :: jge74   = 354
  integer, parameter :: jge75   = 355
  integer, parameter :: jge76   = 356
  integer, parameter :: jge77   = 357
  integer, parameter :: jge78   = 358
  integer, parameter :: jas65   = 359
  integer, parameter :: jas66   = 360
  integer, parameter :: jas67   = 361
  integer, parameter :: jas68   = 362
  integer, parameter :: jas69   = 363
  integer, parameter :: jas70   = 364
  integer, parameter :: jas71   = 365
  integer, parameter :: jas72   = 366
  integer, parameter :: jas73   = 367
  integer, parameter :: jas74   = 368
  integer, parameter :: jas75   = 369
  integer, parameter :: jas76   = 370
  integer, parameter :: jas77   = 371
  integer, parameter :: jas78   = 372
  integer, parameter :: jas79   = 373
  integer, parameter :: jse67   = 374
  integer, parameter :: jse68   = 375
  integer, parameter :: jse69   = 376
  integer, parameter :: jse70   = 377
  integer, parameter :: jse71   = 378
  integer, parameter :: jse72   = 379
  integer, parameter :: jse73   = 380
  integer, parameter :: jse74   = 381
  integer, parameter :: jse75   = 382
  integer, parameter :: jse76   = 383
  integer, parameter :: jse77   = 384
  integer, parameter :: jse78   = 385
  integer, parameter :: jse79   = 386
  integer, parameter :: jse80   = 387
  integer, parameter :: jse81   = 388
  integer, parameter :: jse82   = 389
  integer, parameter :: jse83   = 390
  integer, parameter :: jbr68   = 391
  integer, parameter :: jbr69   = 392
  integer, parameter :: jbr70   = 393
  integer, parameter :: jbr71   = 394
  integer, parameter :: jbr72   = 395
  integer, parameter :: jbr73   = 396
  integer, parameter :: jbr74   = 397
  integer, parameter :: jbr75   = 398
  integer, parameter :: jbr76   = 399
  integer, parameter :: jbr77   = 400
  integer, parameter :: jbr78   = 401
  integer, parameter :: jbr79   = 402
  integer, parameter :: jbr80   = 403
  integer, parameter :: jbr81   = 404
  integer, parameter :: jbr82   = 405
  integer, parameter :: jbr83   = 406
  integer, parameter :: jkr69   = 407
  integer, parameter :: jkr70   = 408
  integer, parameter :: jkr71   = 409
  integer, parameter :: jkr72   = 410
  integer, parameter :: jkr73   = 411
  integer, parameter :: jkr74   = 412
  integer, parameter :: jkr75   = 413
  integer, parameter :: jkr76   = 414
  integer, parameter :: jkr77   = 415
  integer, parameter :: jkr78   = 416
  integer, parameter :: jkr79   = 417
  integer, parameter :: jkr80   = 418
  integer, parameter :: jkr81   = 419
  integer, parameter :: jkr82   = 420
  integer, parameter :: jkr83   = 421
  integer, parameter :: jkr84   = 422
  integer, parameter :: jkr85   = 423
  integer, parameter :: jkr86   = 424
  integer, parameter :: jkr87   = 425
  integer, parameter :: jrb73   = 426
  integer, parameter :: jrb74   = 427
  integer, parameter :: jrb75   = 428
  integer, parameter :: jrb76   = 429
  integer, parameter :: jrb77   = 430
  integer, parameter :: jrb78   = 431
  integer, parameter :: jrb79   = 432
  integer, parameter :: jrb80   = 433
  integer, parameter :: jrb81   = 434
  integer, parameter :: jrb82   = 435
  integer, parameter :: jrb83   = 436
  integer, parameter :: jrb84   = 437
  integer, parameter :: jrb85   = 438
  integer, parameter :: jsr74   = 439
  integer, parameter :: jsr75   = 440
  integer, parameter :: jsr76   = 441
  integer, parameter :: jsr77   = 442
  integer, parameter :: jsr78   = 443
  integer, parameter :: jsr79   = 444
  integer, parameter :: jsr80   = 445
  integer, parameter :: jsr81   = 446
  integer, parameter :: jsr82   = 447
  integer, parameter :: jsr83   = 448
  integer, parameter :: jsr84   = 449
  integer, parameter :: jy75   = 450
  integer, parameter :: jy76   = 451
  integer, parameter :: jy77   = 452
  integer, parameter :: jy78   = 453
  integer, parameter :: jy79   = 454
  integer, parameter :: jy80   = 455
  integer, parameter :: jy81   = 456
  integer, parameter :: jy82   = 457
  integer, parameter :: jy83   = 458
  integer, parameter :: jy84   = 459
  integer, parameter :: jy85   = 460
  integer, parameter :: jy86   = 461
  integer, parameter :: jy87   = 462
  integer, parameter :: jzr78   = 463
  integer, parameter :: jzr79   = 464
  integer, parameter :: jzr80   = 465
  integer, parameter :: jzr81   = 466
  integer, parameter :: jzr82   = 467
  integer, parameter :: jzr83   = 468
  integer, parameter :: jzr84   = 469
  integer, parameter :: jzr85   = 470
  integer, parameter :: jzr86   = 471
  integer, parameter :: jzr87   = 472
  integer, parameter :: jzr88   = 473
  integer, parameter :: jzr89   = 474
  integer, parameter :: jzr90   = 475
  integer, parameter :: jnb82   = 476
  integer, parameter :: jnb83   = 477
  integer, parameter :: jnb84   = 478
  integer, parameter :: jnb85   = 479
  integer, parameter :: jnb86   = 480
  integer, parameter :: jnb87   = 481
  integer, parameter :: jnb88   = 482
  integer, parameter :: jnb89   = 483
  integer, parameter :: jnb90   = 484
  integer, parameter :: jmo83   = 485
  integer, parameter :: jmo84   = 486
  integer, parameter :: jmo85   = 487
  integer, parameter :: jmo86   = 488
  integer, parameter :: jmo87   = 489
  integer, parameter :: jmo88   = 490
  integer, parameter :: jmo89   = 491
  integer, parameter :: jmo90   = 492
  integer, parameter :: jtc89   = 493
  integer, parameter :: jtc90   = 494
  integer, parameter :: jtc91   = 495

  ! Reactions
  integer, parameter :: k_n__p__weak__wc12   = 1
  integer, parameter :: k_t__he3__weak__wc12   = 2
  integer, parameter :: k_he3__t__weak__electron_capture   = 3
  integer, parameter :: k_be7__li7__weak__electron_capture   = 4
  integer, parameter :: k_c11__b11__weak__wc12   = 5
  integer, parameter :: k_c14__n14__weak__wc12   = 6
  integer, parameter :: k_n12__c12__weak__wc12   = 7
  integer, parameter :: k_n13__c13__weak__wc12   = 8
  integer, parameter :: k_o14__n14__weak__wc12   = 9
  integer, parameter :: k_o15__n15__weak__wc12   = 10
  integer, parameter :: k_o19__f19__weak__wc12   = 11
  integer, parameter :: k_f17__o17__weak__wc12   = 12
  integer, parameter :: k_f18__o18__weak__wc12   = 13
  integer, parameter :: k_f21__ne21__weak__wc12   = 14
  integer, parameter :: k_ne17__f17__weak__wc12   = 15
  integer, parameter :: k_ne18__f18__weak__wc12   = 16
  integer, parameter :: k_ne19__f19__weak__wc12   = 17
  integer, parameter :: k_ne23__na23__weak__wc12   = 18
  integer, parameter :: k_ne24__na24__weak__wc12   = 19
  integer, parameter :: k_na19__ne19__weak__bqa_pos_   = 20
  integer, parameter :: k_na20__ne20__weak__wc12   = 21
  integer, parameter :: k_na21__ne21__weak__wc12   = 22
  integer, parameter :: k_na22__ne22__weak__wc12   = 23
  integer, parameter :: k_na24__mg24__weak__wc12   = 24
  integer, parameter :: k_na25__mg25__weak__wc12   = 25
  integer, parameter :: k_na26__mg26__weak__wc12   = 26
  integer, parameter :: k_na27__mg27__weak__wc12   = 27
  integer, parameter :: k_mg20__na20__weak__wc12   = 28
  integer, parameter :: k_mg21__na21__weak__wc12   = 29
  integer, parameter :: k_mg22__na22__weak__wc12   = 30
  integer, parameter :: k_mg23__na23__weak__wc12   = 31
  integer, parameter :: k_mg27__al27__weak__wc12   = 32
  integer, parameter :: k_mg28__al28__weak__wc12   = 33
  integer, parameter :: k_mg29__al29__weak__wc12   = 34
  integer, parameter :: k_al22__mg22__weak__wc12   = 35
  integer, parameter :: k_al23__mg23__weak__wc12   = 36
  integer, parameter :: k_al24__mg24__weak__wc12   = 37
  integer, parameter :: k_al25__mg25__weak__wc12   = 38
  integer, parameter :: k_al26__mg26__weak__wc12   = 39
  integer, parameter :: k_al28__si28__weak__wc12   = 40
  integer, parameter :: k_al29__si29__weak__wc12   = 41
  integer, parameter :: k_al30__si30__weak__wc12   = 42
  integer, parameter :: k_al31__si31__weak__wc12   = 43
  integer, parameter :: k_si23__al23__weak__wc12   = 44
  integer, parameter :: k_si24__al24__weak__wc12   = 45
  integer, parameter :: k_si25__al25__weak__wc12   = 46
  integer, parameter :: k_si26__al26__weak__wc12   = 47
  integer, parameter :: k_si27__al27__weak__wc12   = 48
  integer, parameter :: k_si31__p31__weak__wc12   = 49
  integer, parameter :: k_si32__p32__weak__wc12   = 50
  integer, parameter :: k_si33__p33__weak__wc12   = 51
  integer, parameter :: k_si34__p34__weak__wc12   = 52
  integer, parameter :: k_p27__si27__weak__wc12   = 53
  integer, parameter :: k_p28__si28__weak__wc12   = 54
  integer, parameter :: k_p29__si29__weak__wc12   = 55
  integer, parameter :: k_p30__si30__weak__wc12   = 56
  integer, parameter :: k_p32__s32__weak__wc12   = 57
  integer, parameter :: k_p33__s33__weak__wc12   = 58
  integer, parameter :: k_p34__s34__weak__wc12   = 59
  integer, parameter :: k_p35__s35__weak__wc12   = 60
  integer, parameter :: k_p36__s36__weak__wc12   = 61
  integer, parameter :: k_p37__s37__weak__wc12   = 62
  integer, parameter :: k_p38__s38__weak__wc12   = 63
  integer, parameter :: k_s28__p28__weak__wc12   = 64
  integer, parameter :: k_s29__p29__weak__wc12   = 65
  integer, parameter :: k_s30__p30__weak__wc12   = 66
  integer, parameter :: k_s31__p31__weak__wc12   = 67
  integer, parameter :: k_s35__cl35__weak__wc12   = 68
  integer, parameter :: k_s37__cl37__weak__wc12   = 69
  integer, parameter :: k_s38__cl38__weak__wc12   = 70
  integer, parameter :: k_s39__cl39__weak__wc12   = 71
  integer, parameter :: k_s40__cl40__weak__wc12   = 72
  integer, parameter :: k_s41__cl41__weak__wc12   = 73
  integer, parameter :: k_s42__cl42__weak__wc12   = 74
  integer, parameter :: k_cl31__s31__weak__wc12   = 75
  integer, parameter :: k_cl32__s32__weak__wc12   = 76
  integer, parameter :: k_cl33__s33__weak__wc12   = 77
  integer, parameter :: k_cl34__s34__weak__wc12   = 78
  integer, parameter :: k_cl36__ar36__weak__wc12   = 79
  integer, parameter :: k_cl36__s36__weak__wc12   = 80
  integer, parameter :: k_cl38__ar38__weak__wc12   = 81
  integer, parameter :: k_cl39__ar39__weak__wc12   = 82
  integer, parameter :: k_cl40__ar40__weak__wc12   = 83
  integer, parameter :: k_cl41__ar41__weak__wc12   = 84
  integer, parameter :: k_cl42__ar42__weak__wc12   = 85
  integer, parameter :: k_cl43__ar43__weak__wc12   = 86
  integer, parameter :: k_cl44__ar44__weak__wc12   = 87
  integer, parameter :: k_cl45__ar45__weak__wc12   = 88
  integer, parameter :: k_ar32__cl32__weak__wc12   = 89
  integer, parameter :: k_ar33__cl33__weak__wc12   = 90
  integer, parameter :: k_ar34__cl34__weak__wc12   = 91
  integer, parameter :: k_ar35__cl35__weak__wc12   = 92
  integer, parameter :: k_ar37__cl37__weak__wc12   = 93
  integer, parameter :: k_ar39__k39__weak__wc12   = 94
  integer, parameter :: k_ar41__k41__weak__wc12   = 95
  integer, parameter :: k_ar42__k42__weak__wc12   = 96
  integer, parameter :: k_ar43__k43__weak__wc12   = 97
  integer, parameter :: k_ar44__k44__weak__wc12   = 98
  integer, parameter :: k_ar45__k45__weak__wc12   = 99
  integer, parameter :: k_ar46__k46__weak__wc12   = 100
  integer, parameter :: k_k35__ar35__weak__wc12   = 101
  integer, parameter :: k_k36__ar36__weak__wc12   = 102
  integer, parameter :: k_k37__ar37__weak__wc12   = 103
  integer, parameter :: k_k38__ar38__weak__wc12   = 104
  integer, parameter :: k_k40__ca40__weak__wc12   = 105
  integer, parameter :: k_k40__ar40__weak__wc12   = 106
  integer, parameter :: k_k42__ca42__weak__wc12   = 107
  integer, parameter :: k_k43__ca43__weak__wc12   = 108
  integer, parameter :: k_k44__ca44__weak__wc12   = 109
  integer, parameter :: k_k45__ca45__weak__wc12   = 110
  integer, parameter :: k_k46__ca46__weak__wc12   = 111
  integer, parameter :: k_k47__ca47__weak__wc12   = 112
  integer, parameter :: k_k48__ca48__weak__wc12   = 113
  integer, parameter :: k_k49__ca49__weak__wc12   = 114
  integer, parameter :: k_ca36__k36__weak__wc12   = 115
  integer, parameter :: k_ca37__k37__weak__wc12   = 116
  integer, parameter :: k_ca38__k38__weak__wc12   = 117
  integer, parameter :: k_ca39__k39__weak__wc12   = 118
  integer, parameter :: k_ca41__k41__weak__wc12   = 119
  integer, parameter :: k_ca45__sc45__weak__wc12   = 120
  integer, parameter :: k_ca47__sc47__weak__wc12   = 121
  integer, parameter :: k_ca48__sc48__weak__wc07   = 122
  integer, parameter :: k_ca49__sc49__weak__wc12   = 123
  integer, parameter :: k_sc40__ca40__weak__wc12   = 124
  integer, parameter :: k_sc41__ca41__weak__wc12   = 125
  integer, parameter :: k_sc42__ca42__weak__wc12   = 126
  integer, parameter :: k_sc43__ca43__weak__wc12   = 127
  integer, parameter :: k_sc44__ca44__weak__wc12   = 128
  integer, parameter :: k_sc46__ca46__weak__bex_pos_   = 129
  integer, parameter :: k_sc46__ti46__weak__wc12   = 130
  integer, parameter :: k_sc47__ti47__weak__wc12   = 131
  integer, parameter :: k_sc48__ti48__weak__wc12   = 132
  integer, parameter :: k_sc49__ti49__weak__wc12   = 133
  integer, parameter :: k_sc50__ti50__weak__wc12   = 134
  integer, parameter :: k_sc51__ti51__weak__wc12   = 135
  integer, parameter :: k_ti41__sc41__weak__wc12   = 136
  integer, parameter :: k_ti42__sc42__weak__wc12   = 137
  integer, parameter :: k_ti43__sc43__weak__wc12   = 138
  integer, parameter :: k_ti44__sc44__weak__wc12   = 139
  integer, parameter :: k_ti45__sc45__weak__wc12   = 140
  integer, parameter :: k_ti51__v51__weak__wc12   = 141
  integer, parameter :: k_ti52__v52__weak__wc12   = 142
  integer, parameter :: k_ti53__v53__weak__wc12   = 143
  integer, parameter :: k_v43__ti43__weak__wc12   = 144
  integer, parameter :: k_v44__ti44__weak__wc12   = 145
  integer, parameter :: k_v45__ti45__weak__wc12   = 146
  integer, parameter :: k_v46__ti46__weak__wc12   = 147
  integer, parameter :: k_v47__ti47__weak__wc12   = 148
  integer, parameter :: k_v48__ti48__weak__wc12   = 149
  integer, parameter :: k_v49__ti49__weak__wc12   = 150
  integer, parameter :: k_v50__ti50__weak__wc12   = 151
  integer, parameter :: k_v50__cr50__weak__wc12   = 152
  integer, parameter :: k_v52__cr52__weak__wc12   = 153
  integer, parameter :: k_v53__cr53__weak__wc12   = 154
  integer, parameter :: k_v54__cr54__weak__wc12   = 155
  integer, parameter :: k_v55__cr55__weak__wc12   = 156
  integer, parameter :: k_cr44__v44__weak__wc12   = 157
  integer, parameter :: k_cr45__v45__weak__wc12   = 158
  integer, parameter :: k_cr46__v46__weak__wc12   = 159
  integer, parameter :: k_cr47__v47__weak__wc12   = 160
  integer, parameter :: k_cr48__v48__weak__wc12   = 161
  integer, parameter :: k_cr49__v49__weak__wc12   = 162
  integer, parameter :: k_cr51__v51__weak__wc12   = 163
  integer, parameter :: k_cr55__mn55__weak__wc12   = 164
  integer, parameter :: k_cr56__mn56__weak__wc12   = 165
  integer, parameter :: k_cr57__mn57__weak__wc12   = 166
  integer, parameter :: k_cr58__mn58__weak__wc12   = 167
  integer, parameter :: k_mn46__cr46__weak__wc12   = 168
  integer, parameter :: k_mn47__cr47__weak__wc12   = 169
  integer, parameter :: k_mn48__cr48__weak__wc12   = 170
  integer, parameter :: k_mn49__cr49__weak__wc12   = 171
  integer, parameter :: k_mn50__cr50__weak__wc12   = 172
  integer, parameter :: k_mn51__cr51__weak__wc12   = 173
  integer, parameter :: k_mn52__cr52__weak__wc12   = 174
  integer, parameter :: k_mn53__cr53__weak__wc12   = 175
  integer, parameter :: k_mn54__cr54__weak__wc12   = 176
  integer, parameter :: k_mn54__fe54__weak__wc12   = 177
  integer, parameter :: k_mn56__fe56__weak__wc12   = 178
  integer, parameter :: k_mn57__fe57__weak__wc12   = 179
  integer, parameter :: k_mn58__fe58__weak__wc12   = 180
  integer, parameter :: k_mn59__fe59__weak__wc12   = 181
  integer, parameter :: k_mn60__fe60__weak__wc12   = 182
  integer, parameter :: k_mn61__fe61__weak__wc12   = 183
  integer, parameter :: k_fe47__mn47__weak__wc12   = 184
  integer, parameter :: k_fe48__mn48__weak__wc12   = 185
  integer, parameter :: k_fe49__mn49__weak__wc12   = 186
  integer, parameter :: k_fe50__mn50__weak__wc12   = 187
  integer, parameter :: k_fe51__mn51__weak__wc12   = 188
  integer, parameter :: k_fe52__mn52__weak__wc12   = 189
  integer, parameter :: k_fe53__mn53__weak__wc12   = 190
  integer, parameter :: k_fe55__mn55__weak__wc12   = 191
  integer, parameter :: k_fe59__co59__weak__wc12   = 192
  integer, parameter :: k_fe60__co60__weak__wc12   = 193
  integer, parameter :: k_fe61__co61__weak__wc12   = 194
  integer, parameter :: k_fe62__co62__weak__wc12   = 195
  integer, parameter :: k_fe63__co63__weak__wc12   = 196
  integer, parameter :: k_fe64__co64__weak__wc12   = 197
  integer, parameter :: k_fe65__co65__weak__wc12   = 198
  integer, parameter :: k_fe66__co66__weak__wc12   = 199
  integer, parameter :: k_co50__fe50__weak__wc12   = 200
  integer, parameter :: k_co51__fe51__weak__wc12   = 201
  integer, parameter :: k_co52__fe52__weak__wc12   = 202
  integer, parameter :: k_co53__fe53__weak__wc12   = 203
  integer, parameter :: k_co54__fe54__weak__wc12   = 204
  integer, parameter :: k_co55__fe55__weak__wc12   = 205
  integer, parameter :: k_co56__fe56__weak__wc12   = 206
  integer, parameter :: k_co57__fe57__weak__wc12   = 207
  integer, parameter :: k_co58__fe58__weak__wc12   = 208
  integer, parameter :: k_co58__ni58__weak__mo03   = 209
  integer, parameter :: k_co60__ni60__weak__wc12   = 210
  integer, parameter :: k_co61__ni61__weak__wc12   = 211
  integer, parameter :: k_co62__ni62__weak__wc12   = 212
  integer, parameter :: k_co63__ni63__weak__wc12   = 213
  integer, parameter :: k_co64__ni64__weak__wc12   = 214
  integer, parameter :: k_co65__ni65__weak__wc12   = 215
  integer, parameter :: k_co66__ni66__weak__wc12   = 216
  integer, parameter :: k_co67__ni67__weak__wc12   = 217
  integer, parameter :: k_ni51__co51__weak__btyk   = 218
  integer, parameter :: k_ni52__co52__weak__wc12   = 219
  integer, parameter :: k_ni53__co53__weak__wc12   = 220
  integer, parameter :: k_ni54__co54__weak__wc12   = 221
  integer, parameter :: k_ni55__co55__weak__wc12   = 222
  integer, parameter :: k_ni56__co56__weak__wc12   = 223
  integer, parameter :: k_ni57__co57__weak__wc12   = 224
  integer, parameter :: k_ni59__co59__weak__wc12   = 225
  integer, parameter :: k_ni63__cu63__weak__wc12   = 226
  integer, parameter :: k_ni65__cu65__weak__wc12   = 227
  integer, parameter :: k_ni66__cu66__weak__wc12   = 228
  integer, parameter :: k_ni67__cu67__weak__wc12   = 229
  integer, parameter :: k_ni68__cu68__weak__wc12   = 230
  integer, parameter :: k_cu55__ni55__weak__wc12   = 231
  integer, parameter :: k_cu56__ni56__weak__wc12   = 232
  integer, parameter :: k_cu57__ni57__weak__wc12   = 233
  integer, parameter :: k_cu58__ni58__weak__wc12   = 234
  integer, parameter :: k_cu59__ni59__weak__wc12   = 235
  integer, parameter :: k_cu60__ni60__weak__wc12   = 236
  integer, parameter :: k_cu61__ni61__weak__wc12   = 237
  integer, parameter :: k_cu62__ni62__weak__wc12   = 238
  integer, parameter :: k_cu64__ni64__weak__wc12   = 239
  integer, parameter :: k_cu64__zn64__weak__wc12   = 240
  integer, parameter :: k_cu66__zn66__weak__wc12   = 241
  integer, parameter :: k_cu67__zn67__weak__wc12   = 242
  integer, parameter :: k_cu68__zn68__weak__wc12   = 243
  integer, parameter :: k_cu69__zn69__weak__wc12   = 244
  integer, parameter :: k_zn57__cu57__weak__wc12   = 245
  integer, parameter :: k_zn58__cu58__weak__wc12   = 246
  integer, parameter :: k_zn59__cu59__weak__wc12   = 247
  integer, parameter :: k_zn60__cu60__weak__wc12   = 248
  integer, parameter :: k_zn61__cu61__weak__wc12   = 249
  integer, parameter :: k_zn62__cu62__weak__wc12   = 250
  integer, parameter :: k_zn63__cu63__weak__wc12   = 251
  integer, parameter :: k_zn65__cu65__weak__wc12   = 252
  integer, parameter :: k_zn69__ga69__weak__wc12   = 253
  integer, parameter :: k_zn71__ga71__weak__wc12   = 254
  integer, parameter :: k_zn72__ga72__weak__wc12   = 255
  integer, parameter :: k_ga59__zn59__weak__bqa_pos_   = 256
  integer, parameter :: k_ga60__zn60__weak__wc12   = 257
  integer, parameter :: k_ga61__zn61__weak__wc12   = 258
  integer, parameter :: k_ga62__zn62__weak__wc12   = 259
  integer, parameter :: k_ga63__zn63__weak__wc12   = 260
  integer, parameter :: k_ga64__zn64__weak__wc12   = 261
  integer, parameter :: k_ga65__zn65__weak__wc12   = 262
  integer, parameter :: k_ga66__zn66__weak__wc12   = 263
  integer, parameter :: k_ga67__zn67__weak__wc12   = 264
  integer, parameter :: k_ga68__zn68__weak__wc12   = 265
  integer, parameter :: k_ga70__ge70__weak__wc12   = 266
  integer, parameter :: k_ga70__zn70__weak__wc12   = 267
  integer, parameter :: k_ga72__ge72__weak__wc12   = 268
  integer, parameter :: k_ga73__ge73__weak__wc12   = 269
  integer, parameter :: k_ga74__ge74__weak__wc12   = 270
  integer, parameter :: k_ga75__ge75__weak__wc12   = 271
  integer, parameter :: k_ge62__ga62__weak__wc12   = 272
  integer, parameter :: k_ge63__ga63__weak__wc12   = 273
  integer, parameter :: k_ge64__ga64__weak__wc12   = 274
  integer, parameter :: k_ge65__ga65__weak__wc12   = 275
  integer, parameter :: k_ge66__ga66__weak__wc12   = 276
  integer, parameter :: k_ge67__ga67__weak__wc12   = 277
  integer, parameter :: k_ge68__ga68__weak__wc12   = 278
  integer, parameter :: k_ge69__ga69__weak__wc12   = 279
  integer, parameter :: k_ge71__ga71__weak__wc12   = 280
  integer, parameter :: k_ge75__as75__weak__wc12   = 281
  integer, parameter :: k_ge77__as77__weak__wc12   = 282
  integer, parameter :: k_ge78__as78__weak__wc12   = 283
  integer, parameter :: k_as65__ge65__weak__wc12   = 284
  integer, parameter :: k_as66__ge66__weak__wc12   = 285
  integer, parameter :: k_as67__ge67__weak__wc12   = 286
  integer, parameter :: k_as68__ge68__weak__wc12   = 287
  integer, parameter :: k_as69__ge69__weak__wc12   = 288
  integer, parameter :: k_as70__ge70__weak__wc12   = 289
  integer, parameter :: k_as71__ge71__weak__wc12   = 290
  integer, parameter :: k_as72__ge72__weak__wc12   = 291
  integer, parameter :: k_as73__ge73__weak__wc12   = 292
  integer, parameter :: k_as74__ge74__weak__wc12   = 293
  integer, parameter :: k_as74__se74__weak__wc12   = 294
  integer, parameter :: k_as76__ge76__weak__bex_pos_   = 295
  integer, parameter :: k_as76__se76__weak__wc12   = 296
  integer, parameter :: k_as77__se77__weak__wc12   = 297
  integer, parameter :: k_as78__se78__weak__wc12   = 298
  integer, parameter :: k_as79__se79__weak__wc12   = 299
  integer, parameter :: k_se67__as67__weak__wc12   = 300
  integer, parameter :: k_se68__as68__weak__wc12   = 301
  integer, parameter :: k_se69__as69__weak__wc12   = 302
  integer, parameter :: k_se70__as70__weak__wc12   = 303
  integer, parameter :: k_se71__as71__weak__wc12   = 304
  integer, parameter :: k_se72__as72__weak__wc12   = 305
  integer, parameter :: k_se73__as73__weak__wc12   = 306
  integer, parameter :: k_se75__as75__weak__wc12   = 307
  integer, parameter :: k_se79__br79__weak__wc12   = 308
  integer, parameter :: k_se81__br81__weak__wc12   = 309
  integer, parameter :: k_se83__br83__weak__wc12   = 310
  integer, parameter :: k_br68__se68__weak__bqa_pos_   = 311
  integer, parameter :: k_br69__se69__weak__bqa_pos_   = 312
  integer, parameter :: k_br70__se70__weak__wc12   = 313
  integer, parameter :: k_br71__se71__weak__wc12   = 314
  integer, parameter :: k_br72__se72__weak__wc12   = 315
  integer, parameter :: k_br73__se73__weak__wc12   = 316
  integer, parameter :: k_br74__se74__weak__wc12   = 317
  integer, parameter :: k_br75__se75__weak__wc12   = 318
  integer, parameter :: k_br76__se76__weak__wc12   = 319
  integer, parameter :: k_br77__se77__weak__wc12   = 320
  integer, parameter :: k_br78__se78__weak__wc12   = 321
  integer, parameter :: k_br78__kr78__weak__wc12   = 322
  integer, parameter :: k_br80__kr80__weak__wc12   = 323
  integer, parameter :: k_br80__se80__weak__wc12   = 324
  integer, parameter :: k_br82__se82__weak__bex_pos_   = 325
  integer, parameter :: k_br82__kr82__weak__wc12   = 326
  integer, parameter :: k_br83__kr83__weak__wc12   = 327
  integer, parameter :: k_kr69__br69__weak__wc12   = 328
  integer, parameter :: k_kr70__br70__weak__wc12   = 329
  integer, parameter :: k_kr71__br71__weak__wc12   = 330
  integer, parameter :: k_kr72__br72__weak__wc12   = 331
  integer, parameter :: k_kr73__br73__weak__wc12   = 332
  integer, parameter :: k_kr74__br74__weak__wc12   = 333
  integer, parameter :: k_kr75__br75__weak__wc12   = 334
  integer, parameter :: k_kr76__br76__weak__wc12   = 335
  integer, parameter :: k_kr77__br77__weak__wc12   = 336
  integer, parameter :: k_kr79__br79__weak__wc12   = 337
  integer, parameter :: k_kr81__br81__weak__wc12   = 338
  integer, parameter :: k_kr85__rb85__weak__wc12   = 339
  integer, parameter :: k_rb73__kr73__weak__bqa_pos_   = 340
  integer, parameter :: k_rb74__kr74__weak__wc12   = 341
  integer, parameter :: k_rb75__kr75__weak__wc12   = 342
  integer, parameter :: k_rb76__kr76__weak__wc12   = 343
  integer, parameter :: k_rb77__kr77__weak__wc12   = 344
  integer, parameter :: k_rb78__kr78__weak__wc12   = 345
  integer, parameter :: k_rb79__kr79__weak__wc12   = 346
  integer, parameter :: k_rb80__kr80__weak__wc12   = 347
  integer, parameter :: k_rb81__kr81__weak__wc12   = 348
  integer, parameter :: k_rb82__kr82__weak__wc12   = 349
  integer, parameter :: k_rb83__kr83__weak__wc12   = 350
  integer, parameter :: k_rb84__kr84__weak__wc12   = 351
  integer, parameter :: k_rb84__sr84__weak__wc12   = 352
  integer, parameter :: k_sr74__rb74__weak__wc12   = 353
  integer, parameter :: k_sr75__rb75__weak__wc12   = 354
  integer, parameter :: k_sr76__rb76__weak__wc12   = 355
  integer, parameter :: k_sr77__rb77__weak__wc12   = 356
  integer, parameter :: k_sr78__rb78__weak__wc12   = 357
  integer, parameter :: k_sr79__rb79__weak__wc12   = 358
  integer, parameter :: k_sr80__rb80__weak__wc12   = 359
  integer, parameter :: k_sr81__rb81__weak__wc12   = 360
  integer, parameter :: k_sr82__rb82__weak__wc12   = 361
  integer, parameter :: k_sr83__rb83__weak__wc12   = 362
  integer, parameter :: k_y75__sr75__weak__bqa_pos_   = 363
  integer, parameter :: k_y76__sr76__weak__bqa_pos_   = 364
  integer, parameter :: k_y77__sr77__weak__wc12   = 365
  integer, parameter :: k_y78__sr78__weak__wc12   = 366
  integer, parameter :: k_y79__sr79__weak__wc12   = 367
  integer, parameter :: k_y80__sr80__weak__wc12   = 368
  integer, parameter :: k_y81__sr81__weak__wc12   = 369
  integer, parameter :: k_y82__sr82__weak__wc12   = 370
  integer, parameter :: k_y83__sr83__weak__wc12   = 371
  integer, parameter :: k_y84__sr84__weak__wc12   = 372
  integer, parameter :: k_zr78__y78__weak__wc12   = 373
  integer, parameter :: k_zr79__y79__weak__bex_pos_   = 374
  integer, parameter :: k_zr80__y80__weak__wc12   = 375
  integer, parameter :: k_zr81__y81__weak__wc12   = 376
  integer, parameter :: k_zr82__y82__weak__wc12   = 377
  integer, parameter :: k_zr83__y83__weak__wc12   = 378
  integer, parameter :: k_zr84__y84__weak__wc12   = 379
  integer, parameter :: k_zr85__y85__weak__wc12   = 380
  integer, parameter :: k_zr86__y86__weak__wc12   = 381
  integer, parameter :: k_zr87__y87__weak__wc12   = 382
  integer, parameter :: k_nb82__zr82__weak__wc12   = 383
  integer, parameter :: k_nb83__zr83__weak__wc12   = 384
  integer, parameter :: k_nb84__zr84__weak__wc12   = 385
  integer, parameter :: k_nb85__zr85__weak__wc12   = 386
  integer, parameter :: k_nb86__zr86__weak__wc12   = 387
  integer, parameter :: k_nb87__zr87__weak__wc12   = 388
  integer, parameter :: k_nb88__zr88__weak__wc12   = 389
  integer, parameter :: k_nb89__zr89__weak__wc12   = 390
  integer, parameter :: k_nb90__zr90__weak__wc12   = 391
  integer, parameter :: k_mo83__nb83__weak__wc12   = 392
  integer, parameter :: k_mo84__nb84__weak__wc12   = 393
  integer, parameter :: k_mo85__nb85__weak__wc12   = 394
  integer, parameter :: k_mo86__nb86__weak__wc12   = 395
  integer, parameter :: k_mo87__nb87__weak__wc12   = 396
  integer, parameter :: k_mo88__nb88__weak__wc12   = 397
  integer, parameter :: k_mo89__nb89__weak__wc12   = 398
  integer, parameter :: k_mo90__nb90__weak__wc12   = 399
  integer, parameter :: k_tc89__mo89__weak__wc12   = 400
  integer, parameter :: k_tc90__mo90__weak__bqa_pos_   = 401
  integer, parameter :: k_d__n_p   = 402
  integer, parameter :: k_t__n_d   = 403
  integer, parameter :: k_he3__p_d   = 404
  integer, parameter :: k_he4__n_he3   = 405
  integer, parameter :: k_he4__p_t   = 406
  integer, parameter :: k_he4__d_d   = 407
  integer, parameter :: k_li6__he4_d   = 408
  integer, parameter :: k_li7__n_li6   = 409
  integer, parameter :: k_li7__he4_t   = 410
  integer, parameter :: k_be7__p_li6   = 411
  integer, parameter :: k_be7__he4_he3   = 412
  integer, parameter :: k_b8__p_be7   = 413
  integer, parameter :: k_b8__he4_he4__weak__wc12   = 414
  integer, parameter :: k_b10__p_be9   = 415
  integer, parameter :: k_b10__he4_li6   = 416
  integer, parameter :: k_b11__n_b10   = 417
  integer, parameter :: k_b11__he4_li7   = 418
  integer, parameter :: k_c11__p_b10   = 419
  integer, parameter :: k_c11__he4_be7   = 420
  integer, parameter :: k_c12__n_c11   = 421
  integer, parameter :: k_c12__p_b11   = 422
  integer, parameter :: k_c13__n_c12   = 423
  integer, parameter :: k_c14__n_c13   = 424
  integer, parameter :: k_n12__p_c11   = 425
  integer, parameter :: k_n13__p_c12   = 426
  integer, parameter :: k_n14__n_n13   = 427
  integer, parameter :: k_n14__p_c13   = 428
  integer, parameter :: k_n15__n_n14   = 429
  integer, parameter :: k_n15__p_c14   = 430
  integer, parameter :: k_o14__p_n13   = 431
  integer, parameter :: k_o15__n_o14   = 432
  integer, parameter :: k_o15__p_n14   = 433
  integer, parameter :: k_o16__n_o15   = 434
  integer, parameter :: k_o16__p_n15   = 435
  integer, parameter :: k_o16__he4_c12   = 436
  integer, parameter :: k_o17__n_o16   = 437
  integer, parameter :: k_o18__n_o17   = 438
  integer, parameter :: k_o18__he4_c14   = 439
  integer, parameter :: k_o19__n_o18   = 440
  integer, parameter :: k_f17__p_o16   = 441
  integer, parameter :: k_f18__n_f17   = 442
  integer, parameter :: k_f18__p_o17   = 443
  integer, parameter :: k_f18__he4_n14   = 444
  integer, parameter :: k_f19__n_f18   = 445
  integer, parameter :: k_f19__p_o18   = 446
  integer, parameter :: k_f19__he4_n15   = 447
  integer, parameter :: k_f20__n_f19   = 448
  integer, parameter :: k_f20__p_o19   = 449
  integer, parameter :: k_f21__n_f20   = 450
  integer, parameter :: k_ne17__p_o16__weak__wc12   = 451
  integer, parameter :: k_ne18__n_ne17   = 452
  integer, parameter :: k_ne18__p_f17   = 453
  integer, parameter :: k_ne18__he4_o14   = 454
  integer, parameter :: k_ne19__n_ne18   = 455
  integer, parameter :: k_ne19__p_f18   = 456
  integer, parameter :: k_ne19__he4_o15   = 457
  integer, parameter :: k_ne20__n_ne19   = 458
  integer, parameter :: k_ne20__p_f19   = 459
  integer, parameter :: k_ne20__he4_o16   = 460
  integer, parameter :: k_ne21__n_ne20   = 461
  integer, parameter :: k_ne21__p_f20   = 462
  integer, parameter :: k_ne21__he4_o17   = 463
  integer, parameter :: k_ne22__n_ne21   = 464
  integer, parameter :: k_ne22__p_f21   = 465
  integer, parameter :: k_ne22__he4_o18   = 466
  integer, parameter :: k_ne23__n_ne22   = 467
  integer, parameter :: k_ne23__he4_o19   = 468
  integer, parameter :: k_ne24__n_ne23   = 469
  integer, parameter :: k_na19__p_ne18   = 470
  integer, parameter :: k_na20__n_na19   = 471
  integer, parameter :: k_na20__p_ne19   = 472
  integer, parameter :: k_na20__he4_o16__weak__wc12   = 473
  integer, parameter :: k_na21__n_na20   = 474
  integer, parameter :: k_na21__p_ne20   = 475
  integer, parameter :: k_na21__he4_f17   = 476
  integer, parameter :: k_na22__n_na21   = 477
  integer, parameter :: k_na22__p_ne21   = 478
  integer, parameter :: k_na22__he4_f18   = 479
  integer, parameter :: k_na23__n_na22   = 480
  integer, parameter :: k_na23__p_ne22   = 481
  integer, parameter :: k_na23__he4_f19   = 482
  integer, parameter :: k_na24__n_na23   = 483
  integer, parameter :: k_na24__p_ne23   = 484
  integer, parameter :: k_na24__he4_f20   = 485
  integer, parameter :: k_na25__n_na24   = 486
  integer, parameter :: k_na25__p_ne24   = 487
  integer, parameter :: k_na25__he4_f21   = 488
  integer, parameter :: k_na26__n_na25   = 489
  integer, parameter :: k_na27__n_na26   = 490
  integer, parameter :: k_na27__n_mg26__weak__wc12   = 491
  integer, parameter :: k_mg20__p_na19   = 492
  integer, parameter :: k_mg20__p_ne19__weak__wc12   = 493
  integer, parameter :: k_mg21__n_mg20   = 494
  integer, parameter :: k_mg21__p_ne20__weak__wc12   = 495
  integer, parameter :: k_mg21__p_na20   = 496
  integer, parameter :: k_mg21__he4_ne17   = 497
  integer, parameter :: k_mg21__he4_f17__weak__wc12   = 498
  integer, parameter :: k_mg22__n_mg21   = 499
  integer, parameter :: k_mg22__p_na21   = 500
  integer, parameter :: k_mg22__he4_ne18   = 501
  integer, parameter :: k_mg23__n_mg22   = 502
  integer, parameter :: k_mg23__p_na22   = 503
  integer, parameter :: k_mg23__he4_ne19   = 504
  integer, parameter :: k_mg24__n_mg23   = 505
  integer, parameter :: k_mg24__p_na23   = 506
  integer, parameter :: k_mg24__he4_ne20   = 507
  integer, parameter :: k_mg25__n_mg24   = 508
  integer, parameter :: k_mg25__p_na24   = 509
  integer, parameter :: k_mg25__he4_ne21   = 510
  integer, parameter :: k_mg26__n_mg25   = 511
  integer, parameter :: k_mg26__p_na25   = 512
  integer, parameter :: k_mg26__he4_ne22   = 513
  integer, parameter :: k_mg27__n_mg26   = 514
  integer, parameter :: k_mg27__p_na26   = 515
  integer, parameter :: k_mg27__he4_ne23   = 516
  integer, parameter :: k_mg28__n_mg27   = 517
  integer, parameter :: k_mg28__p_na27   = 518
  integer, parameter :: k_mg28__he4_ne24   = 519
  integer, parameter :: k_mg29__n_mg28   = 520
  integer, parameter :: k_al22__p_mg21   = 521
  integer, parameter :: k_al22__p_na21__weak__wc12   = 522
  integer, parameter :: k_al22__he4_ne18__weak__wc12   = 523
  integer, parameter :: k_al23__n_al22   = 524
  integer, parameter :: k_al23__p_mg22   = 525
  integer, parameter :: k_al23__p_na22__weak__wc12   = 526
  integer, parameter :: k_al23__he4_na19   = 527
  integer, parameter :: k_al24__n_al23   = 528
  integer, parameter :: k_al24__p_mg23   = 529
  integer, parameter :: k_al24__p_na23__weak__wc12   = 530
  integer, parameter :: k_al24__he4_na20   = 531
  integer, parameter :: k_al24__he4_ne20__weak__wc12   = 532
  integer, parameter :: k_al25__n_al24   = 533
  integer, parameter :: k_al25__p_mg24   = 534
  integer, parameter :: k_al25__he4_na21   = 535
  integer, parameter :: k_al26__n_al25   = 536
  integer, parameter :: k_al26__p_mg25   = 537
  integer, parameter :: k_al26__he4_na22   = 538
  integer, parameter :: k_al27__n_al26   = 539
  integer, parameter :: k_al27__p_mg26   = 540
  integer, parameter :: k_al27__he4_na23   = 541
  integer, parameter :: k_al28__n_al27   = 542
  integer, parameter :: k_al28__p_mg27   = 543
  integer, parameter :: k_al28__he4_na24   = 544
  integer, parameter :: k_al29__n_al28   = 545
  integer, parameter :: k_al29__p_mg28   = 546
  integer, parameter :: k_al29__he4_na25   = 547
  integer, parameter :: k_al30__n_al29   = 548
  integer, parameter :: k_al30__p_mg29   = 549
  integer, parameter :: k_al30__he4_na26   = 550
  integer, parameter :: k_al31__n_al30   = 551
  integer, parameter :: k_al31__he4_na27   = 552
  integer, parameter :: k_si23__p_al22   = 553
  integer, parameter :: k_si23__p_mg22__weak__wc12   = 554
  integer, parameter :: k_si24__n_si23   = 555
  integer, parameter :: k_si24__p_al23   = 556
  integer, parameter :: k_si24__p_mg23__weak__wc12   = 557
  integer, parameter :: k_si24__he4_mg20   = 558
  integer, parameter :: k_si25__n_si24   = 559
  integer, parameter :: k_si25__p_al24   = 560
  integer, parameter :: k_si25__p_mg24__weak__wc12   = 561
  integer, parameter :: k_si25__he4_mg21   = 562
  integer, parameter :: k_si26__n_si25   = 563
  integer, parameter :: k_si26__p_al25   = 564
  integer, parameter :: k_si26__he4_mg22   = 565
  integer, parameter :: k_si27__n_si26   = 566
  integer, parameter :: k_si27__p_al26   = 567
  integer, parameter :: k_si27__he4_mg23   = 568
  integer, parameter :: k_si28__n_si27   = 569
  integer, parameter :: k_si28__p_al27   = 570
  integer, parameter :: k_si28__he4_mg24   = 571
  integer, parameter :: k_si29__n_si28   = 572
  integer, parameter :: k_si29__p_al28   = 573
  integer, parameter :: k_si29__he4_mg25   = 574
  integer, parameter :: k_si30__n_si29   = 575
  integer, parameter :: k_si30__p_al29   = 576
  integer, parameter :: k_si30__he4_mg26   = 577
  integer, parameter :: k_si31__n_si30   = 578
  integer, parameter :: k_si31__p_al30   = 579
  integer, parameter :: k_si31__he4_mg27   = 580
  integer, parameter :: k_si32__n_si31   = 581
  integer, parameter :: k_si32__p_al31   = 582
  integer, parameter :: k_si32__he4_mg28   = 583
  integer, parameter :: k_si33__n_si32   = 584
  integer, parameter :: k_si33__he4_mg29   = 585
  integer, parameter :: k_si34__n_si33   = 586
  integer, parameter :: k_p27__p_si26   = 587
  integer, parameter :: k_p27__p_al26__weak__wc12   = 588
  integer, parameter :: k_p27__he4_al23   = 589
  integer, parameter :: k_p28__n_p27   = 590
  integer, parameter :: k_p28__p_si27   = 591
  integer, parameter :: k_p28__p_al27__weak__wc12   = 592
  integer, parameter :: k_p28__he4_al24   = 593
  integer, parameter :: k_p28__he4_mg24__weak__wc12   = 594
  integer, parameter :: k_p29__n_p28   = 595
  integer, parameter :: k_p29__p_si28   = 596
  integer, parameter :: k_p29__he4_al25   = 597
  integer, parameter :: k_p30__n_p29   = 598
  integer, parameter :: k_p30__p_si29   = 599
  integer, parameter :: k_p30__he4_al26   = 600
  integer, parameter :: k_p31__n_p30   = 601
  integer, parameter :: k_p31__p_si30   = 602
  integer, parameter :: k_p31__he4_al27   = 603
  integer, parameter :: k_p32__n_p31   = 604
  integer, parameter :: k_p32__p_si31   = 605
  integer, parameter :: k_p32__he4_al28   = 606
  integer, parameter :: k_p33__n_p32   = 607
  integer, parameter :: k_p33__p_si32   = 608
  integer, parameter :: k_p33__he4_al29   = 609
  integer, parameter :: k_p34__n_p33   = 610
  integer, parameter :: k_p34__p_si33   = 611
  integer, parameter :: k_p34__he4_al30   = 612
  integer, parameter :: k_p35__n_p34   = 613
  integer, parameter :: k_p35__p_si34   = 614
  integer, parameter :: k_p35__he4_al31   = 615
  integer, parameter :: k_p36__n_p35   = 616
  integer, parameter :: k_p37__n_s36__weak__bkmo   = 617
  integer, parameter :: k_p37__n_p36   = 618
  integer, parameter :: k_p38__n_s37__weak__wc12   = 619
  integer, parameter :: k_p38__n_p37   = 620
  integer, parameter :: k_s28__p_p27   = 621
  integer, parameter :: k_s28__p_si27__weak__wc12   = 622
  integer, parameter :: k_s28__he4_si24   = 623
  integer, parameter :: k_s29__n_s28   = 624
  integer, parameter :: k_s29__p_p28   = 625
  integer, parameter :: k_s29__p_si28__weak__wc12   = 626
  integer, parameter :: k_s29__he4_si25   = 627
  integer, parameter :: k_s30__n_s29   = 628
  integer, parameter :: k_s30__p_p29   = 629
  integer, parameter :: k_s30__he4_si26   = 630
  integer, parameter :: k_s31__n_s30   = 631
  integer, parameter :: k_s31__p_p30   = 632
  integer, parameter :: k_s31__he4_si27   = 633
  integer, parameter :: k_s32__n_s31   = 634
  integer, parameter :: k_s32__p_p31   = 635
  integer, parameter :: k_s32__he4_si28   = 636
  integer, parameter :: k_s33__n_s32   = 637
  integer, parameter :: k_s33__p_p32   = 638
  integer, parameter :: k_s33__he4_si29   = 639
  integer, parameter :: k_s34__n_s33   = 640
  integer, parameter :: k_s34__p_p33   = 641
  integer, parameter :: k_s34__he4_si30   = 642
  integer, parameter :: k_s35__n_s34   = 643
  integer, parameter :: k_s35__p_p34   = 644
  integer, parameter :: k_s35__he4_si31   = 645
  integer, parameter :: k_s36__n_s35   = 646
  integer, parameter :: k_s36__p_p35   = 647
  integer, parameter :: k_s36__he4_si32   = 648
  integer, parameter :: k_s37__n_s36   = 649
  integer, parameter :: k_s37__p_p36   = 650
  integer, parameter :: k_s37__he4_si33   = 651
  integer, parameter :: k_s38__n_s37   = 652
  integer, parameter :: k_s38__p_p37   = 653
  integer, parameter :: k_s38__he4_si34   = 654
  integer, parameter :: k_s39__n_s38   = 655
  integer, parameter :: k_s39__p_p38   = 656
  integer, parameter :: k_s40__n_s39   = 657
  integer, parameter :: k_s41__n_cl40__weak__bkmo   = 658
  integer, parameter :: k_s41__n_s40   = 659
  integer, parameter :: k_s42__n_cl41__weak__bkmo   = 660
  integer, parameter :: k_s42__n_s41   = 661
  integer, parameter :: k_cl31__p_s30   = 662
  integer, parameter :: k_cl31__p_p30__weak__wc12   = 663
  integer, parameter :: k_cl31__he4_p27   = 664
  integer, parameter :: k_cl32__n_cl31   = 665
  integer, parameter :: k_cl32__p_s31   = 666
  integer, parameter :: k_cl32__p_p31__weak__wc12   = 667
  integer, parameter :: k_cl32__he4_p28   = 668
  integer, parameter :: k_cl32__he4_si28__weak__wc12   = 669
  integer, parameter :: k_cl33__n_cl32   = 670
  integer, parameter :: k_cl33__p_s32   = 671
  integer, parameter :: k_cl33__he4_p29   = 672
  integer, parameter :: k_cl34__n_cl33   = 673
  integer, parameter :: k_cl34__p_s33   = 674
  integer, parameter :: k_cl34__he4_p30   = 675
  integer, parameter :: k_cl35__n_cl34   = 676
  integer, parameter :: k_cl35__p_s34   = 677
  integer, parameter :: k_cl35__he4_p31   = 678
  integer, parameter :: k_cl36__n_cl35   = 679
  integer, parameter :: k_cl36__p_s35   = 680
  integer, parameter :: k_cl36__he4_p32   = 681
  integer, parameter :: k_cl37__n_cl36   = 682
  integer, parameter :: k_cl37__p_s36   = 683
  integer, parameter :: k_cl37__he4_p33   = 684
  integer, parameter :: k_cl38__n_cl37   = 685
  integer, parameter :: k_cl38__p_s37   = 686
  integer, parameter :: k_cl38__he4_p34   = 687
  integer, parameter :: k_cl39__n_cl38   = 688
  integer, parameter :: k_cl39__p_s38   = 689
  integer, parameter :: k_cl39__he4_p35   = 690
  integer, parameter :: k_cl40__n_cl39   = 691
  integer, parameter :: k_cl40__p_s39   = 692
  integer, parameter :: k_cl40__he4_p36   = 693
  integer, parameter :: k_cl41__n_cl40   = 694
  integer, parameter :: k_cl41__p_s40   = 695
  integer, parameter :: k_cl41__he4_p37   = 696
  integer, parameter :: k_cl42__n_cl41   = 697
  integer, parameter :: k_cl42__p_s41   = 698
  integer, parameter :: k_cl42__he4_p38   = 699
  integer, parameter :: k_cl43__n_cl42   = 700
  integer, parameter :: k_cl43__n_ar42__weak__mo03   = 701
  integer, parameter :: k_cl43__p_s42   = 702
  integer, parameter :: k_cl44__n_ar43__weak__wc12   = 703
  integer, parameter :: k_cl44__n_cl43   = 704
  integer, parameter :: k_cl45__n_cl44   = 705
  integer, parameter :: k_cl45__n_ar44__weak__wc12   = 706
  integer, parameter :: k_ar32__p_cl31   = 707
  integer, parameter :: k_ar32__p_s31__weak__wc12   = 708
  integer, parameter :: k_ar32__he4_s28   = 709
  integer, parameter :: k_ar33__n_ar32   = 710
  integer, parameter :: k_ar33__p_cl32   = 711
  integer, parameter :: k_ar33__p_s32__weak__wc12   = 712
  integer, parameter :: k_ar33__he4_s29   = 713
  integer, parameter :: k_ar34__n_ar33   = 714
  integer, parameter :: k_ar34__p_cl33   = 715
  integer, parameter :: k_ar34__he4_s30   = 716
  integer, parameter :: k_ar35__n_ar34   = 717
  integer, parameter :: k_ar35__p_cl34   = 718
  integer, parameter :: k_ar35__he4_s31   = 719
  integer, parameter :: k_ar36__n_ar35   = 720
  integer, parameter :: k_ar36__p_cl35   = 721
  integer, parameter :: k_ar36__he4_s32   = 722
  integer, parameter :: k_ar37__n_ar36   = 723
  integer, parameter :: k_ar37__p_cl36   = 724
  integer, parameter :: k_ar37__he4_s33   = 725
  integer, parameter :: k_ar38__n_ar37   = 726
  integer, parameter :: k_ar38__p_cl37   = 727
  integer, parameter :: k_ar38__he4_s34   = 728
  integer, parameter :: k_ar39__n_ar38   = 729
  integer, parameter :: k_ar39__p_cl38   = 730
  integer, parameter :: k_ar39__he4_s35   = 731
  integer, parameter :: k_ar40__n_ar39   = 732
  integer, parameter :: k_ar40__p_cl39   = 733
  integer, parameter :: k_ar40__he4_s36   = 734
  integer, parameter :: k_ar41__n_ar40   = 735
  integer, parameter :: k_ar41__p_cl40   = 736
  integer, parameter :: k_ar41__he4_s37   = 737
  integer, parameter :: k_ar42__n_ar41   = 738
  integer, parameter :: k_ar42__p_cl41   = 739
  integer, parameter :: k_ar42__he4_s38   = 740
  integer, parameter :: k_ar43__n_ar42   = 741
  integer, parameter :: k_ar43__p_cl42   = 742
  integer, parameter :: k_ar43__he4_s39   = 743
  integer, parameter :: k_ar44__n_ar43   = 744
  integer, parameter :: k_ar44__p_cl43   = 745
  integer, parameter :: k_ar44__he4_s40   = 746
  integer, parameter :: k_ar45__n_ar44   = 747
  integer, parameter :: k_ar45__p_cl44   = 748
  integer, parameter :: k_ar45__he4_s41   = 749
  integer, parameter :: k_ar46__n_ar45   = 750
  integer, parameter :: k_ar46__p_cl45   = 751
  integer, parameter :: k_ar46__he4_s42   = 752
  integer, parameter :: k_k35__p_ar34   = 753
  integer, parameter :: k_k35__p_cl34__weak__wc12   = 754
  integer, parameter :: k_k35__he4_cl31   = 755
  integer, parameter :: k_k36__n_k35   = 756
  integer, parameter :: k_k36__p_ar35   = 757
  integer, parameter :: k_k36__p_cl35__weak__wc12   = 758
  integer, parameter :: k_k36__he4_cl32   = 759
  integer, parameter :: k_k36__he4_s32__weak__wc12   = 760
  integer, parameter :: k_k37__n_k36   = 761
  integer, parameter :: k_k37__p_ar36   = 762
  integer, parameter :: k_k37__he4_cl33   = 763
  integer, parameter :: k_k38__n_k37   = 764
  integer, parameter :: k_k38__p_ar37   = 765
  integer, parameter :: k_k38__he4_cl34   = 766
  integer, parameter :: k_k39__n_k38   = 767
  integer, parameter :: k_k39__p_ar38   = 768
  integer, parameter :: k_k39__he4_cl35   = 769
  integer, parameter :: k_k40__n_k39   = 770
  integer, parameter :: k_k40__p_ar39   = 771
  integer, parameter :: k_k40__he4_cl36   = 772
  integer, parameter :: k_k41__n_k40   = 773
  integer, parameter :: k_k41__p_ar40   = 774
  integer, parameter :: k_k41__he4_cl37   = 775
  integer, parameter :: k_k42__n_k41   = 776
  integer, parameter :: k_k42__p_ar41   = 777
  integer, parameter :: k_k42__he4_cl38   = 778
  integer, parameter :: k_k43__n_k42   = 779
  integer, parameter :: k_k43__p_ar42   = 780
  integer, parameter :: k_k43__he4_cl39   = 781
  integer, parameter :: k_k44__n_k43   = 782
  integer, parameter :: k_k44__p_ar43   = 783
  integer, parameter :: k_k44__he4_cl40   = 784
  integer, parameter :: k_k45__n_k44   = 785
  integer, parameter :: k_k45__p_ar44   = 786
  integer, parameter :: k_k45__he4_cl41   = 787
  integer, parameter :: k_k46__n_k45   = 788
  integer, parameter :: k_k46__p_ar45   = 789
  integer, parameter :: k_k46__he4_cl42   = 790
  integer, parameter :: k_k47__n_k46   = 791
  integer, parameter :: k_k47__p_ar46   = 792
  integer, parameter :: k_k47__he4_cl43   = 793
  integer, parameter :: k_k48__n_k47   = 794
  integer, parameter :: k_k48__n_ca47__weak__wc12   = 795
  integer, parameter :: k_k48__he4_cl44   = 796
  integer, parameter :: k_k49__n_k48   = 797
  integer, parameter :: k_k49__n_ca48__weak__wc12   = 798
  integer, parameter :: k_k49__he4_cl45   = 799
  integer, parameter :: k_ca36__p_k35   = 800
  integer, parameter :: k_ca36__p_ar35__weak__wc12   = 801
  integer, parameter :: k_ca36__he4_ar32   = 802
  integer, parameter :: k_ca37__n_ca36   = 803
  integer, parameter :: k_ca37__p_ar36__weak__wc12   = 804
  integer, parameter :: k_ca37__p_k36   = 805
  integer, parameter :: k_ca37__he4_ar33   = 806
  integer, parameter :: k_ca38__n_ca37   = 807
  integer, parameter :: k_ca38__p_k37   = 808
  integer, parameter :: k_ca38__he4_ar34   = 809
  integer, parameter :: k_ca39__n_ca38   = 810
  integer, parameter :: k_ca39__p_k38   = 811
  integer, parameter :: k_ca39__he4_ar35   = 812
  integer, parameter :: k_ca40__n_ca39   = 813
  integer, parameter :: k_ca40__p_k39   = 814
  integer, parameter :: k_ca40__he4_ar36   = 815
  integer, parameter :: k_ca41__n_ca40   = 816
  integer, parameter :: k_ca41__p_k40   = 817
  integer, parameter :: k_ca41__he4_ar37   = 818
  integer, parameter :: k_ca42__n_ca41   = 819
  integer, parameter :: k_ca42__p_k41   = 820
  integer, parameter :: k_ca42__he4_ar38   = 821
  integer, parameter :: k_ca43__n_ca42   = 822
  integer, parameter :: k_ca43__p_k42   = 823
  integer, parameter :: k_ca43__he4_ar39   = 824
  integer, parameter :: k_ca44__n_ca43   = 825
  integer, parameter :: k_ca44__p_k43   = 826
  integer, parameter :: k_ca44__he4_ar40   = 827
  integer, parameter :: k_ca45__n_ca44   = 828
  integer, parameter :: k_ca45__p_k44   = 829
  integer, parameter :: k_ca45__he4_ar41   = 830
  integer, parameter :: k_ca46__n_ca45   = 831
  integer, parameter :: k_ca46__p_k45   = 832
  integer, parameter :: k_ca46__he4_ar42   = 833
  integer, parameter :: k_ca47__n_ca46   = 834
  integer, parameter :: k_ca47__p_k46   = 835
  integer, parameter :: k_ca47__he4_ar43   = 836
  integer, parameter :: k_ca48__n_ca47   = 837
  integer, parameter :: k_ca48__p_k47   = 838
  integer, parameter :: k_ca48__he4_ar44   = 839
  integer, parameter :: k_ca49__n_ca48   = 840
  integer, parameter :: k_ca49__p_k48   = 841
  integer, parameter :: k_ca49__he4_ar45   = 842
  integer, parameter :: k_sc40__p_ca39   = 843
  integer, parameter :: k_sc40__p_k39__weak__wc12   = 844
  integer, parameter :: k_sc40__he4_k36   = 845
  integer, parameter :: k_sc40__he4_ar36__weak__wc12   = 846
  integer, parameter :: k_sc41__n_sc40   = 847
  integer, parameter :: k_sc41__p_ca40   = 848
  integer, parameter :: k_sc41__he4_k37   = 849
  integer, parameter :: k_sc42__n_sc41   = 850
  integer, parameter :: k_sc42__p_ca41   = 851
  integer, parameter :: k_sc42__he4_k38   = 852
  integer, parameter :: k_sc43__n_sc42   = 853
  integer, parameter :: k_sc43__p_ca42   = 854
  integer, parameter :: k_sc43__he4_k39   = 855
  integer, parameter :: k_sc44__n_sc43   = 856
  integer, parameter :: k_sc44__p_ca43   = 857
  integer, parameter :: k_sc44__he4_k40   = 858
  integer, parameter :: k_sc45__n_sc44   = 859
  integer, parameter :: k_sc45__p_ca44   = 860
  integer, parameter :: k_sc45__he4_k41   = 861
  integer, parameter :: k_sc46__n_sc45   = 862
  integer, parameter :: k_sc46__p_ca45   = 863
  integer, parameter :: k_sc46__he4_k42   = 864
  integer, parameter :: k_sc47__n_sc46   = 865
  integer, parameter :: k_sc47__p_ca46   = 866
  integer, parameter :: k_sc47__he4_k43   = 867
  integer, parameter :: k_sc48__n_sc47   = 868
  integer, parameter :: k_sc48__p_ca47   = 869
  integer, parameter :: k_sc48__he4_k44   = 870
  integer, parameter :: k_sc49__n_sc48   = 871
  integer, parameter :: k_sc49__p_ca48   = 872
  integer, parameter :: k_sc49__he4_k45   = 873
  integer, parameter :: k_sc50__n_sc49   = 874
  integer, parameter :: k_sc50__p_ca49   = 875
  integer, parameter :: k_sc50__he4_k46   = 876
  integer, parameter :: k_sc51__n_sc50   = 877
  integer, parameter :: k_sc51__he4_k47   = 878
  integer, parameter :: k_ti41__p_sc40   = 879
  integer, parameter :: k_ti41__p_ca40__weak__wc12   = 880
  integer, parameter :: k_ti41__he4_ca37   = 881
  integer, parameter :: k_ti42__n_ti41   = 882
  integer, parameter :: k_ti42__p_sc41   = 883
  integer, parameter :: k_ti42__he4_ca38   = 884
  integer, parameter :: k_ti43__n_ti42   = 885
  integer, parameter :: k_ti43__p_sc42   = 886
  integer, parameter :: k_ti43__he4_ca39   = 887
  integer, parameter :: k_ti44__n_ti43   = 888
  integer, parameter :: k_ti44__p_sc43   = 889
  integer, parameter :: k_ti44__he4_ca40   = 890
  integer, parameter :: k_ti45__n_ti44   = 891
  integer, parameter :: k_ti45__p_sc44   = 892
  integer, parameter :: k_ti45__he4_ca41   = 893
  integer, parameter :: k_ti46__n_ti45   = 894
  integer, parameter :: k_ti46__p_sc45   = 895
  integer, parameter :: k_ti46__he4_ca42   = 896
  integer, parameter :: k_ti47__n_ti46   = 897
  integer, parameter :: k_ti47__p_sc46   = 898
  integer, parameter :: k_ti47__he4_ca43   = 899
  integer, parameter :: k_ti48__n_ti47   = 900
  integer, parameter :: k_ti48__p_sc47   = 901
  integer, parameter :: k_ti48__he4_ca44   = 902
  integer, parameter :: k_ti49__n_ti48   = 903
  integer, parameter :: k_ti49__p_sc48   = 904
  integer, parameter :: k_ti49__he4_ca45   = 905
  integer, parameter :: k_ti50__n_ti49   = 906
  integer, parameter :: k_ti50__p_sc49   = 907
  integer, parameter :: k_ti50__he4_ca46   = 908
  integer, parameter :: k_ti51__n_ti50   = 909
  integer, parameter :: k_ti51__p_sc50   = 910
  integer, parameter :: k_ti51__he4_ca47   = 911
  integer, parameter :: k_ti52__n_ti51   = 912
  integer, parameter :: k_ti52__p_sc51   = 913
  integer, parameter :: k_ti52__he4_ca48   = 914
  integer, parameter :: k_ti53__n_ti52   = 915
  integer, parameter :: k_ti53__he4_ca49   = 916
  integer, parameter :: k_v43__p_ti42   = 917
  integer, parameter :: k_v44__n_v43   = 918
  integer, parameter :: k_v44__p_ti43   = 919
  integer, parameter :: k_v44__he4_sc40   = 920
  integer, parameter :: k_v45__n_v44   = 921
  integer, parameter :: k_v45__p_ti44   = 922
  integer, parameter :: k_v45__he4_sc41   = 923
  integer, parameter :: k_v46__n_v45   = 924
  integer, parameter :: k_v46__p_ti45   = 925
  integer, parameter :: k_v46__he4_sc42   = 926
  integer, parameter :: k_v47__n_v46   = 927
  integer, parameter :: k_v47__p_ti46   = 928
  integer, parameter :: k_v47__he4_sc43   = 929
  integer, parameter :: k_v48__n_v47   = 930
  integer, parameter :: k_v48__p_ti47   = 931
  integer, parameter :: k_v48__he4_sc44   = 932
  integer, parameter :: k_v49__n_v48   = 933
  integer, parameter :: k_v49__p_ti48   = 934
  integer, parameter :: k_v49__he4_sc45   = 935
  integer, parameter :: k_v50__n_v49   = 936
  integer, parameter :: k_v50__p_ti49   = 937
  integer, parameter :: k_v50__he4_sc46   = 938
  integer, parameter :: k_v51__n_v50   = 939
  integer, parameter :: k_v51__p_ti50   = 940
  integer, parameter :: k_v51__he4_sc47   = 941
  integer, parameter :: k_v52__n_v51   = 942
  integer, parameter :: k_v52__p_ti51   = 943
  integer, parameter :: k_v52__he4_sc48   = 944
  integer, parameter :: k_v53__n_v52   = 945
  integer, parameter :: k_v53__p_ti52   = 946
  integer, parameter :: k_v53__he4_sc49   = 947
  integer, parameter :: k_v54__n_v53   = 948
  integer, parameter :: k_v54__p_ti53   = 949
  integer, parameter :: k_v54__he4_sc50   = 950
  integer, parameter :: k_v55__n_v54   = 951
  integer, parameter :: k_v55__he4_sc51   = 952
  integer, parameter :: k_cr44__p_v43   = 953
  integer, parameter :: k_cr44__p_ti43__weak__wc12   = 954
  integer, parameter :: k_cr45__n_cr44   = 955
  integer, parameter :: k_cr45__p_v44   = 956
  integer, parameter :: k_cr45__p_ti44__weak__wc12   = 957
  integer, parameter :: k_cr45__he4_ti41   = 958
  integer, parameter :: k_cr46__n_cr45   = 959
  integer, parameter :: k_cr46__p_v45   = 960
  integer, parameter :: k_cr46__he4_ti42   = 961
  integer, parameter :: k_cr47__n_cr46   = 962
  integer, parameter :: k_cr47__p_v46   = 963
  integer, parameter :: k_cr47__he4_ti43   = 964
  integer, parameter :: k_cr48__n_cr47   = 965
  integer, parameter :: k_cr48__p_v47   = 966
  integer, parameter :: k_cr48__he4_ti44   = 967
  integer, parameter :: k_cr49__n_cr48   = 968
  integer, parameter :: k_cr49__p_v48   = 969
  integer, parameter :: k_cr49__he4_ti45   = 970
  integer, parameter :: k_cr50__n_cr49   = 971
  integer, parameter :: k_cr50__p_v49   = 972
  integer, parameter :: k_cr50__he4_ti46   = 973
  integer, parameter :: k_cr51__n_cr50   = 974
  integer, parameter :: k_cr51__p_v50   = 975
  integer, parameter :: k_cr51__he4_ti47   = 976
  integer, parameter :: k_cr52__n_cr51   = 977
  integer, parameter :: k_cr52__p_v51   = 978
  integer, parameter :: k_cr52__he4_ti48   = 979
  integer, parameter :: k_cr53__n_cr52   = 980
  integer, parameter :: k_cr53__p_v52   = 981
  integer, parameter :: k_cr53__he4_ti49   = 982
  integer, parameter :: k_cr54__n_cr53   = 983
  integer, parameter :: k_cr54__p_v53   = 984
  integer, parameter :: k_cr54__he4_ti50   = 985
  integer, parameter :: k_cr55__n_cr54   = 986
  integer, parameter :: k_cr55__p_v54   = 987
  integer, parameter :: k_cr55__he4_ti51   = 988
  integer, parameter :: k_cr56__n_cr55   = 989
  integer, parameter :: k_cr56__p_v55   = 990
  integer, parameter :: k_cr56__he4_ti52   = 991
  integer, parameter :: k_cr57__n_cr56   = 992
  integer, parameter :: k_cr57__he4_ti53   = 993
  integer, parameter :: k_cr58__n_cr57   = 994
  integer, parameter :: k_mn46__p_cr45   = 995
  integer, parameter :: k_mn46__p_v45__weak__wc12   = 996
  integer, parameter :: k_mn47__n_mn46   = 997
  integer, parameter :: k_mn47__p_cr46   = 998
  integer, parameter :: k_mn47__p_v46__weak__wc12   = 999
  integer, parameter :: k_mn47__he4_v43   = 1000
  integer, parameter :: k_mn48__n_mn47   = 1001
  integer, parameter :: k_mn48__p_cr47   = 1002
  integer, parameter :: k_mn48__p_v47__weak__wc12   = 1003
  integer, parameter :: k_mn48__he4_v44   = 1004
  integer, parameter :: k_mn48__he4_ti44__weak__wc12   = 1005
  integer, parameter :: k_mn49__n_mn48   = 1006
  integer, parameter :: k_mn49__p_cr48   = 1007
  integer, parameter :: k_mn49__he4_v45   = 1008
  integer, parameter :: k_mn50__n_mn49   = 1009
  integer, parameter :: k_mn50__p_cr49   = 1010
  integer, parameter :: k_mn50__he4_v46   = 1011
  integer, parameter :: k_mn51__n_mn50   = 1012
  integer, parameter :: k_mn51__p_cr50   = 1013
  integer, parameter :: k_mn51__he4_v47   = 1014
  integer, parameter :: k_mn52__n_mn51   = 1015
  integer, parameter :: k_mn52__p_cr51   = 1016
  integer, parameter :: k_mn52__he4_v48   = 1017
  integer, parameter :: k_mn53__n_mn52   = 1018
  integer, parameter :: k_mn53__p_cr52   = 1019
  integer, parameter :: k_mn53__he4_v49   = 1020
  integer, parameter :: k_mn54__n_mn53   = 1021
  integer, parameter :: k_mn54__p_cr53   = 1022
  integer, parameter :: k_mn54__he4_v50   = 1023
  integer, parameter :: k_mn55__n_mn54   = 1024
  integer, parameter :: k_mn55__p_cr54   = 1025
  integer, parameter :: k_mn55__he4_v51   = 1026
  integer, parameter :: k_mn56__n_mn55   = 1027
  integer, parameter :: k_mn56__p_cr55   = 1028
  integer, parameter :: k_mn56__he4_v52   = 1029
  integer, parameter :: k_mn57__n_mn56   = 1030
  integer, parameter :: k_mn57__p_cr56   = 1031
  integer, parameter :: k_mn57__he4_v53   = 1032
  integer, parameter :: k_mn58__n_mn57   = 1033
  integer, parameter :: k_mn58__p_cr57   = 1034
  integer, parameter :: k_mn58__he4_v54   = 1035
  integer, parameter :: k_mn59__n_mn58   = 1036
  integer, parameter :: k_mn59__p_cr58   = 1037
  integer, parameter :: k_mn59__he4_v55   = 1038
  integer, parameter :: k_mn60__n_mn59   = 1039
  integer, parameter :: k_mn61__n_mn60   = 1040
  integer, parameter :: k_mn61__n_fe60__weak__mo03   = 1041
  integer, parameter :: k_fe47__p_mn46   = 1042
  integer, parameter :: k_fe47__p_cr46__weak__wc12   = 1043
  integer, parameter :: k_fe48__n_fe47   = 1044
  integer, parameter :: k_fe48__p_mn47   = 1045
  integer, parameter :: k_fe48__p_cr47__weak__wc12   = 1046
  integer, parameter :: k_fe48__he4_cr44   = 1047
  integer, parameter :: k_fe49__n_fe48   = 1048
  integer, parameter :: k_fe49__p_mn48   = 1049
  integer, parameter :: k_fe49__p_cr48__weak__wc12   = 1050
  integer, parameter :: k_fe49__he4_cr45   = 1051
  integer, parameter :: k_fe50__n_fe49   = 1052
  integer, parameter :: k_fe50__p_mn49   = 1053
  integer, parameter :: k_fe50__he4_cr46   = 1054
  integer, parameter :: k_fe51__n_fe50   = 1055
  integer, parameter :: k_fe51__p_mn50   = 1056
  integer, parameter :: k_fe51__he4_cr47   = 1057
  integer, parameter :: k_fe52__n_fe51   = 1058
  integer, parameter :: k_fe52__p_mn51   = 1059
  integer, parameter :: k_fe52__he4_cr48   = 1060
  integer, parameter :: k_fe53__n_fe52   = 1061
  integer, parameter :: k_fe53__p_mn52   = 1062
  integer, parameter :: k_fe53__he4_cr49   = 1063
  integer, parameter :: k_fe54__n_fe53   = 1064
  integer, parameter :: k_fe54__p_mn53   = 1065
  integer, parameter :: k_fe54__he4_cr50   = 1066
  integer, parameter :: k_fe55__n_fe54   = 1067
  integer, parameter :: k_fe55__p_mn54   = 1068
  integer, parameter :: k_fe55__he4_cr51   = 1069
  integer, parameter :: k_fe56__n_fe55   = 1070
  integer, parameter :: k_fe56__p_mn55   = 1071
  integer, parameter :: k_fe56__he4_cr52   = 1072
  integer, parameter :: k_fe57__n_fe56   = 1073
  integer, parameter :: k_fe57__p_mn56   = 1074
  integer, parameter :: k_fe57__he4_cr53   = 1075
  integer, parameter :: k_fe58__n_fe57   = 1076
  integer, parameter :: k_fe58__p_mn57   = 1077
  integer, parameter :: k_fe58__he4_cr54   = 1078
  integer, parameter :: k_fe59__n_fe58   = 1079
  integer, parameter :: k_fe59__p_mn58   = 1080
  integer, parameter :: k_fe59__he4_cr55   = 1081
  integer, parameter :: k_fe60__n_fe59   = 1082
  integer, parameter :: k_fe60__p_mn59   = 1083
  integer, parameter :: k_fe60__he4_cr56   = 1084
  integer, parameter :: k_fe61__n_fe60   = 1085
  integer, parameter :: k_fe61__p_mn60   = 1086
  integer, parameter :: k_fe61__he4_cr57   = 1087
  integer, parameter :: k_fe62__n_fe61   = 1088
  integer, parameter :: k_fe62__p_mn61   = 1089
  integer, parameter :: k_fe62__he4_cr58   = 1090
  integer, parameter :: k_fe63__n_fe62   = 1091
  integer, parameter :: k_fe64__n_fe63   = 1092
  integer, parameter :: k_fe65__n_fe64   = 1093
  integer, parameter :: k_fe66__n_fe65   = 1094
  integer, parameter :: k_fe66__n_co65__weak__mo03   = 1095
  integer, parameter :: k_co50__p_fe49   = 1096
  integer, parameter :: k_co50__p_mn49__weak__wc12   = 1097
  integer, parameter :: k_co50__he4_mn46   = 1098
  integer, parameter :: k_co51__n_co50   = 1099
  integer, parameter :: k_co51__p_fe50   = 1100
  integer, parameter :: k_co51__he4_mn47   = 1101
  integer, parameter :: k_co52__n_co51   = 1102
  integer, parameter :: k_co52__p_fe51   = 1103
  integer, parameter :: k_co52__he4_mn48   = 1104
  integer, parameter :: k_co53__n_co52   = 1105
  integer, parameter :: k_co53__p_fe52   = 1106
  integer, parameter :: k_co53__he4_mn49   = 1107
  integer, parameter :: k_co54__n_co53   = 1108
  integer, parameter :: k_co54__p_fe53   = 1109
  integer, parameter :: k_co54__he4_mn50   = 1110
  integer, parameter :: k_co55__n_co54   = 1111
  integer, parameter :: k_co55__p_fe54   = 1112
  integer, parameter :: k_co55__he4_mn51   = 1113
  integer, parameter :: k_co56__n_co55   = 1114
  integer, parameter :: k_co56__p_fe55   = 1115
  integer, parameter :: k_co56__he4_mn52   = 1116
  integer, parameter :: k_co57__n_co56   = 1117
  integer, parameter :: k_co57__p_fe56   = 1118
  integer, parameter :: k_co57__he4_mn53   = 1119
  integer, parameter :: k_co58__n_co57   = 1120
  integer, parameter :: k_co58__p_fe57   = 1121
  integer, parameter :: k_co58__he4_mn54   = 1122
  integer, parameter :: k_co59__n_co58   = 1123
  integer, parameter :: k_co59__p_fe58   = 1124
  integer, parameter :: k_co59__he4_mn55   = 1125
  integer, parameter :: k_co60__n_co59   = 1126
  integer, parameter :: k_co60__p_fe59   = 1127
  integer, parameter :: k_co60__he4_mn56   = 1128
  integer, parameter :: k_co61__n_co60   = 1129
  integer, parameter :: k_co61__p_fe60   = 1130
  integer, parameter :: k_co61__he4_mn57   = 1131
  integer, parameter :: k_co62__n_co61   = 1132
  integer, parameter :: k_co62__p_fe61   = 1133
  integer, parameter :: k_co62__he4_mn58   = 1134
  integer, parameter :: k_co63__n_co62   = 1135
  integer, parameter :: k_co63__p_fe62   = 1136
  integer, parameter :: k_co63__he4_mn59   = 1137
  integer, parameter :: k_co64__n_co63   = 1138
  integer, parameter :: k_co64__p_fe63   = 1139
  integer, parameter :: k_co64__he4_mn60   = 1140
  integer, parameter :: k_co65__n_co64   = 1141
  integer, parameter :: k_co65__p_fe64   = 1142
  integer, parameter :: k_co65__he4_mn61   = 1143
  integer, parameter :: k_co66__n_co65   = 1144
  integer, parameter :: k_co66__p_fe65   = 1145
  integer, parameter :: k_co67__n_ni66__weak__bkmo   = 1146
  integer, parameter :: k_co67__n_co66   = 1147
  integer, parameter :: k_co67__p_fe66   = 1148
  integer, parameter :: k_ni51__p_co50   = 1149
  integer, parameter :: k_ni51__p_fe50__weak__wc12   = 1150
  integer, parameter :: k_ni51__he4_fe47   = 1151
  integer, parameter :: k_ni52__n_ni51   = 1152
  integer, parameter :: k_ni52__p_co51   = 1153
  integer, parameter :: k_ni52__p_fe51__weak__wc12   = 1154
  integer, parameter :: k_ni52__he4_fe48   = 1155
  integer, parameter :: k_ni53__n_ni52   = 1156
  integer, parameter :: k_ni53__p_co52   = 1157
  integer, parameter :: k_ni53__p_fe52__weak__wc12   = 1158
  integer, parameter :: k_ni53__he4_fe49   = 1159
  integer, parameter :: k_ni54__n_ni53   = 1160
  integer, parameter :: k_ni54__p_co53   = 1161
  integer, parameter :: k_ni54__he4_fe50   = 1162
  integer, parameter :: k_ni55__n_ni54   = 1163
  integer, parameter :: k_ni55__p_co54   = 1164
  integer, parameter :: k_ni55__he4_fe51   = 1165
  integer, parameter :: k_ni56__n_ni55   = 1166
  integer, parameter :: k_ni56__p_co55   = 1167
  integer, parameter :: k_ni56__he4_fe52   = 1168
  integer, parameter :: k_ni57__n_ni56   = 1169
  integer, parameter :: k_ni57__p_co56   = 1170
  integer, parameter :: k_ni57__he4_fe53   = 1171
  integer, parameter :: k_ni58__n_ni57   = 1172
  integer, parameter :: k_ni58__p_co57   = 1173
  integer, parameter :: k_ni58__he4_fe54   = 1174
  integer, parameter :: k_ni59__n_ni58   = 1175
  integer, parameter :: k_ni59__p_co58   = 1176
  integer, parameter :: k_ni59__he4_fe55   = 1177
  integer, parameter :: k_ni60__n_ni59   = 1178
  integer, parameter :: k_ni60__p_co59   = 1179
  integer, parameter :: k_ni60__he4_fe56   = 1180
  integer, parameter :: k_ni61__n_ni60   = 1181
  integer, parameter :: k_ni61__p_co60   = 1182
  integer, parameter :: k_ni61__he4_fe57   = 1183
  integer, parameter :: k_ni62__n_ni61   = 1184
  integer, parameter :: k_ni62__p_co61   = 1185
  integer, parameter :: k_ni62__he4_fe58   = 1186
  integer, parameter :: k_ni63__n_ni62   = 1187
  integer, parameter :: k_ni63__p_co62   = 1188
  integer, parameter :: k_ni63__he4_fe59   = 1189
  integer, parameter :: k_ni64__n_ni63   = 1190
  integer, parameter :: k_ni64__p_co63   = 1191
  integer, parameter :: k_ni64__he4_fe60   = 1192
  integer, parameter :: k_ni65__n_ni64   = 1193
  integer, parameter :: k_ni65__p_co64   = 1194
  integer, parameter :: k_ni65__he4_fe61   = 1195
  integer, parameter :: k_ni66__n_ni65   = 1196
  integer, parameter :: k_ni66__p_co65   = 1197
  integer, parameter :: k_ni66__he4_fe62   = 1198
  integer, parameter :: k_ni67__n_ni66   = 1199
  integer, parameter :: k_ni67__p_co66   = 1200
  integer, parameter :: k_ni67__he4_fe63   = 1201
  integer, parameter :: k_ni68__n_ni67   = 1202
  integer, parameter :: k_ni68__p_co67   = 1203
  integer, parameter :: k_ni68__he4_fe64   = 1204
  integer, parameter :: k_cu55__p_ni54   = 1205
  integer, parameter :: k_cu55__p_co54__weak__wc12   = 1206
  integer, parameter :: k_cu55__he4_co51   = 1207
  integer, parameter :: k_cu56__n_cu55   = 1208
  integer, parameter :: k_cu56__p_ni55   = 1209
  integer, parameter :: k_cu56__p_co55__weak__wc12   = 1210
  integer, parameter :: k_cu56__he4_co52   = 1211
  integer, parameter :: k_cu57__n_cu56   = 1212
  integer, parameter :: k_cu57__p_ni56   = 1213
  integer, parameter :: k_cu57__he4_co53   = 1214
  integer, parameter :: k_cu58__n_cu57   = 1215
  integer, parameter :: k_cu58__p_ni57   = 1216
  integer, parameter :: k_cu58__he4_co54   = 1217
  integer, parameter :: k_cu59__n_cu58   = 1218
  integer, parameter :: k_cu59__p_ni58   = 1219
  integer, parameter :: k_cu59__he4_co55   = 1220
  integer, parameter :: k_cu60__n_cu59   = 1221
  integer, parameter :: k_cu60__p_ni59   = 1222
  integer, parameter :: k_cu60__he4_co56   = 1223
  integer, parameter :: k_cu61__n_cu60   = 1224
  integer, parameter :: k_cu61__p_ni60   = 1225
  integer, parameter :: k_cu61__he4_co57   = 1226
  integer, parameter :: k_cu62__n_cu61   = 1227
  integer, parameter :: k_cu62__p_ni61   = 1228
  integer, parameter :: k_cu62__he4_co58   = 1229
  integer, parameter :: k_cu63__n_cu62   = 1230
  integer, parameter :: k_cu63__p_ni62   = 1231
  integer, parameter :: k_cu63__he4_co59   = 1232
  integer, parameter :: k_cu64__n_cu63   = 1233
  integer, parameter :: k_cu64__p_ni63   = 1234
  integer, parameter :: k_cu64__he4_co60   = 1235
  integer, parameter :: k_cu65__n_cu64   = 1236
  integer, parameter :: k_cu65__p_ni64   = 1237
  integer, parameter :: k_cu65__he4_co61   = 1238
  integer, parameter :: k_cu66__n_cu65   = 1239
  integer, parameter :: k_cu66__p_ni65   = 1240
  integer, parameter :: k_cu66__he4_co62   = 1241
  integer, parameter :: k_cu67__n_cu66   = 1242
  integer, parameter :: k_cu67__p_ni66   = 1243
  integer, parameter :: k_cu67__he4_co63   = 1244
  integer, parameter :: k_cu68__n_cu67   = 1245
  integer, parameter :: k_cu68__p_ni67   = 1246
  integer, parameter :: k_cu68__he4_co64   = 1247
  integer, parameter :: k_cu69__n_cu68   = 1248
  integer, parameter :: k_cu69__p_ni68   = 1249
  integer, parameter :: k_cu69__he4_co65   = 1250
  integer, parameter :: k_zn57__p_cu56   = 1251
  integer, parameter :: k_zn57__p_ni56__weak__wc12   = 1252
  integer, parameter :: k_zn57__he4_ni53   = 1253
  integer, parameter :: k_zn58__n_zn57   = 1254
  integer, parameter :: k_zn58__p_cu57   = 1255
  integer, parameter :: k_zn58__p_ni57__weak__wc12   = 1256
  integer, parameter :: k_zn58__he4_ni54   = 1257
  integer, parameter :: k_zn59__n_zn58   = 1258
  integer, parameter :: k_zn59__p_cu58   = 1259
  integer, parameter :: k_zn59__p_ni58__weak__wc12   = 1260
  integer, parameter :: k_zn59__he4_ni55   = 1261
  integer, parameter :: k_zn60__n_zn59   = 1262
  integer, parameter :: k_zn60__p_cu59   = 1263
  integer, parameter :: k_zn60__he4_ni56   = 1264
  integer, parameter :: k_zn61__n_zn60   = 1265
  integer, parameter :: k_zn61__p_cu60   = 1266
  integer, parameter :: k_zn61__he4_ni57   = 1267
  integer, parameter :: k_zn62__n_zn61   = 1268
  integer, parameter :: k_zn62__p_cu61   = 1269
  integer, parameter :: k_zn62__he4_ni58   = 1270
  integer, parameter :: k_zn63__n_zn62   = 1271
  integer, parameter :: k_zn63__p_cu62   = 1272
  integer, parameter :: k_zn63__he4_ni59   = 1273
  integer, parameter :: k_zn64__n_zn63   = 1274
  integer, parameter :: k_zn64__p_cu63   = 1275
  integer, parameter :: k_zn64__he4_ni60   = 1276
  integer, parameter :: k_zn65__n_zn64   = 1277
  integer, parameter :: k_zn65__p_cu64   = 1278
  integer, parameter :: k_zn65__he4_ni61   = 1279
  integer, parameter :: k_zn66__n_zn65   = 1280
  integer, parameter :: k_zn66__p_cu65   = 1281
  integer, parameter :: k_zn66__he4_ni62   = 1282
  integer, parameter :: k_zn67__n_zn66   = 1283
  integer, parameter :: k_zn67__p_cu66   = 1284
  integer, parameter :: k_zn67__he4_ni63   = 1285
  integer, parameter :: k_zn68__n_zn67   = 1286
  integer, parameter :: k_zn68__p_cu67   = 1287
  integer, parameter :: k_zn68__he4_ni64   = 1288
  integer, parameter :: k_zn69__n_zn68   = 1289
  integer, parameter :: k_zn69__p_cu68   = 1290
  integer, parameter :: k_zn69__he4_ni65   = 1291
  integer, parameter :: k_zn70__n_zn69   = 1292
  integer, parameter :: k_zn70__p_cu69   = 1293
  integer, parameter :: k_zn70__he4_ni66   = 1294
  integer, parameter :: k_zn71__n_zn70   = 1295
  integer, parameter :: k_zn71__he4_ni67   = 1296
  integer, parameter :: k_zn72__n_zn71   = 1297
  integer, parameter :: k_zn72__he4_ni68   = 1298
  integer, parameter :: k_ga59__p_zn58   = 1299
  integer, parameter :: k_ga59__he4_cu55   = 1300
  integer, parameter :: k_ga60__n_ga59   = 1301
  integer, parameter :: k_ga60__p_zn59   = 1302
  integer, parameter :: k_ga60__p_cu59__weak__wc12   = 1303
  integer, parameter :: k_ga60__he4_cu56   = 1304
  integer, parameter :: k_ga60__he4_ni56__weak__wc12   = 1305
  integer, parameter :: k_ga61__n_ga60   = 1306
  integer, parameter :: k_ga61__p_zn60   = 1307
  integer, parameter :: k_ga61__p_cu60__weak__wc12   = 1308
  integer, parameter :: k_ga61__he4_cu57   = 1309
  integer, parameter :: k_ga62__n_ga61   = 1310
  integer, parameter :: k_ga62__p_zn61   = 1311
  integer, parameter :: k_ga62__he4_cu58   = 1312
  integer, parameter :: k_ga63__n_ga62   = 1313
  integer, parameter :: k_ga63__p_zn62   = 1314
  integer, parameter :: k_ga63__he4_cu59   = 1315
  integer, parameter :: k_ga64__n_ga63   = 1316
  integer, parameter :: k_ga64__p_zn63   = 1317
  integer, parameter :: k_ga64__he4_cu60   = 1318
  integer, parameter :: k_ga65__n_ga64   = 1319
  integer, parameter :: k_ga65__p_zn64   = 1320
  integer, parameter :: k_ga65__he4_cu61   = 1321
  integer, parameter :: k_ga66__n_ga65   = 1322
  integer, parameter :: k_ga66__p_zn65   = 1323
  integer, parameter :: k_ga66__he4_cu62   = 1324
  integer, parameter :: k_ga67__n_ga66   = 1325
  integer, parameter :: k_ga67__p_zn66   = 1326
  integer, parameter :: k_ga67__he4_cu63   = 1327
  integer, parameter :: k_ga68__n_ga67   = 1328
  integer, parameter :: k_ga68__p_zn67   = 1329
  integer, parameter :: k_ga68__he4_cu64   = 1330
  integer, parameter :: k_ga69__n_ga68   = 1331
  integer, parameter :: k_ga69__p_zn68   = 1332
  integer, parameter :: k_ga69__he4_cu65   = 1333
  integer, parameter :: k_ga70__n_ga69   = 1334
  integer, parameter :: k_ga70__p_zn69   = 1335
  integer, parameter :: k_ga70__he4_cu66   = 1336
  integer, parameter :: k_ga71__n_ga70   = 1337
  integer, parameter :: k_ga71__p_zn70   = 1338
  integer, parameter :: k_ga71__he4_cu67   = 1339
  integer, parameter :: k_ga72__n_ga71   = 1340
  integer, parameter :: k_ga72__p_zn71   = 1341
  integer, parameter :: k_ga72__he4_cu68   = 1342
  integer, parameter :: k_ga73__n_ga72   = 1343
  integer, parameter :: k_ga73__p_zn72   = 1344
  integer, parameter :: k_ga73__he4_cu69   = 1345
  integer, parameter :: k_ga74__n_ga73   = 1346
  integer, parameter :: k_ga75__n_ga74   = 1347
  integer, parameter :: k_ge62__p_ga61   = 1348
  integer, parameter :: k_ge62__he4_zn58   = 1349
  integer, parameter :: k_ge63__n_ge62   = 1350
  integer, parameter :: k_ge63__p_ga62   = 1351
  integer, parameter :: k_ge63__he4_zn59   = 1352
  integer, parameter :: k_ge64__n_ge63   = 1353
  integer, parameter :: k_ge64__p_ga63   = 1354
  integer, parameter :: k_ge64__he4_zn60   = 1355
  integer, parameter :: k_ge65__n_ge64   = 1356
  integer, parameter :: k_ge65__p_ga64   = 1357
  integer, parameter :: k_ge65__p_zn64__weak__wc12   = 1358
  integer, parameter :: k_ge65__he4_zn61   = 1359
  integer, parameter :: k_ge66__n_ge65   = 1360
  integer, parameter :: k_ge66__p_ga65   = 1361
  integer, parameter :: k_ge66__he4_zn62   = 1362
  integer, parameter :: k_ge67__n_ge66   = 1363
  integer, parameter :: k_ge67__p_ga66   = 1364
  integer, parameter :: k_ge67__he4_zn63   = 1365
  integer, parameter :: k_ge68__n_ge67   = 1366
  integer, parameter :: k_ge68__p_ga67   = 1367
  integer, parameter :: k_ge68__he4_zn64   = 1368
  integer, parameter :: k_ge69__n_ge68   = 1369
  integer, parameter :: k_ge69__p_ga68   = 1370
  integer, parameter :: k_ge69__he4_zn65   = 1371
  integer, parameter :: k_ge70__n_ge69   = 1372
  integer, parameter :: k_ge70__p_ga69   = 1373
  integer, parameter :: k_ge70__he4_zn66   = 1374
  integer, parameter :: k_ge71__n_ge70   = 1375
  integer, parameter :: k_ge71__p_ga70   = 1376
  integer, parameter :: k_ge71__he4_zn67   = 1377
  integer, parameter :: k_ge72__n_ge71   = 1378
  integer, parameter :: k_ge72__p_ga71   = 1379
  integer, parameter :: k_ge72__he4_zn68   = 1380
  integer, parameter :: k_ge73__n_ge72   = 1381
  integer, parameter :: k_ge73__p_ga72   = 1382
  integer, parameter :: k_ge73__he4_zn69   = 1383
  integer, parameter :: k_ge74__n_ge73   = 1384
  integer, parameter :: k_ge74__p_ga73   = 1385
  integer, parameter :: k_ge74__he4_zn70   = 1386
  integer, parameter :: k_ge75__n_ge74   = 1387
  integer, parameter :: k_ge75__p_ga74   = 1388
  integer, parameter :: k_ge75__he4_zn71   = 1389
  integer, parameter :: k_ge76__n_ge75   = 1390
  integer, parameter :: k_ge76__p_ga75   = 1391
  integer, parameter :: k_ge76__he4_zn72   = 1392
  integer, parameter :: k_ge77__n_ge76   = 1393
  integer, parameter :: k_ge78__n_ge77   = 1394
  integer, parameter :: k_as65__p_ge64   = 1395
  integer, parameter :: k_as65__he4_ga61   = 1396
  integer, parameter :: k_as66__n_as65   = 1397
  integer, parameter :: k_as66__p_ge65   = 1398
  integer, parameter :: k_as66__he4_ga62   = 1399
  integer, parameter :: k_as67__n_as66   = 1400
  integer, parameter :: k_as67__p_ge66   = 1401
  integer, parameter :: k_as67__he4_ga63   = 1402
  integer, parameter :: k_as68__n_as67   = 1403
  integer, parameter :: k_as68__p_ge67   = 1404
  integer, parameter :: k_as68__he4_ga64   = 1405
  integer, parameter :: k_as69__n_as68   = 1406
  integer, parameter :: k_as69__p_ge68   = 1407
  integer, parameter :: k_as69__he4_ga65   = 1408
  integer, parameter :: k_as70__n_as69   = 1409
  integer, parameter :: k_as70__p_ge69   = 1410
  integer, parameter :: k_as70__he4_ga66   = 1411
  integer, parameter :: k_as71__n_as70   = 1412
  integer, parameter :: k_as71__p_ge70   = 1413
  integer, parameter :: k_as71__he4_ga67   = 1414
  integer, parameter :: k_as72__n_as71   = 1415
  integer, parameter :: k_as72__p_ge71   = 1416
  integer, parameter :: k_as72__he4_ga68   = 1417
  integer, parameter :: k_as73__n_as72   = 1418
  integer, parameter :: k_as73__p_ge72   = 1419
  integer, parameter :: k_as73__he4_ga69   = 1420
  integer, parameter :: k_as74__n_as73   = 1421
  integer, parameter :: k_as74__p_ge73   = 1422
  integer, parameter :: k_as74__he4_ga70   = 1423
  integer, parameter :: k_as75__n_as74   = 1424
  integer, parameter :: k_as75__p_ge74   = 1425
  integer, parameter :: k_as75__he4_ga71   = 1426
  integer, parameter :: k_as76__n_as75   = 1427
  integer, parameter :: k_as76__p_ge75   = 1428
  integer, parameter :: k_as76__he4_ga72   = 1429
  integer, parameter :: k_as77__n_as76   = 1430
  integer, parameter :: k_as77__p_ge76   = 1431
  integer, parameter :: k_as77__he4_ga73   = 1432
  integer, parameter :: k_as78__n_as77   = 1433
  integer, parameter :: k_as78__p_ge77   = 1434
  integer, parameter :: k_as78__he4_ga74   = 1435
  integer, parameter :: k_as79__n_as78   = 1436
  integer, parameter :: k_as79__p_ge78   = 1437
  integer, parameter :: k_as79__he4_ga75   = 1438
  integer, parameter :: k_se67__p_as66   = 1439
  integer, parameter :: k_se67__p_ge66__weak__wc12   = 1440
  integer, parameter :: k_se67__he4_ge63   = 1441
  integer, parameter :: k_se68__n_se67   = 1442
  integer, parameter :: k_se68__p_as67   = 1443
  integer, parameter :: k_se68__he4_ge64   = 1444
  integer, parameter :: k_se69__n_se68   = 1445
  integer, parameter :: k_se69__p_as68   = 1446
  integer, parameter :: k_se69__p_ge68__weak__wc12   = 1447
  integer, parameter :: k_se69__he4_ge65   = 1448
  integer, parameter :: k_se70__n_se69   = 1449
  integer, parameter :: k_se70__p_as69   = 1450
  integer, parameter :: k_se70__he4_ge66   = 1451
  integer, parameter :: k_se71__n_se70   = 1452
  integer, parameter :: k_se71__p_as70   = 1453
  integer, parameter :: k_se71__he4_ge67   = 1454
  integer, parameter :: k_se72__n_se71   = 1455
  integer, parameter :: k_se72__p_as71   = 1456
  integer, parameter :: k_se72__he4_ge68   = 1457
  integer, parameter :: k_se73__n_se72   = 1458
  integer, parameter :: k_se73__p_as72   = 1459
  integer, parameter :: k_se73__he4_ge69   = 1460
  integer, parameter :: k_se74__n_se73   = 1461
  integer, parameter :: k_se74__p_as73   = 1462
  integer, parameter :: k_se74__he4_ge70   = 1463
  integer, parameter :: k_se75__n_se74   = 1464
  integer, parameter :: k_se75__p_as74   = 1465
  integer, parameter :: k_se75__he4_ge71   = 1466
  integer, parameter :: k_se76__n_se75   = 1467
  integer, parameter :: k_se76__p_as75   = 1468
  integer, parameter :: k_se76__he4_ge72   = 1469
  integer, parameter :: k_se77__n_se76   = 1470
  integer, parameter :: k_se77__p_as76   = 1471
  integer, parameter :: k_se77__he4_ge73   = 1472
  integer, parameter :: k_se78__n_se77   = 1473
  integer, parameter :: k_se78__p_as77   = 1474
  integer, parameter :: k_se78__he4_ge74   = 1475
  integer, parameter :: k_se79__n_se78   = 1476
  integer, parameter :: k_se79__p_as78   = 1477
  integer, parameter :: k_se79__he4_ge75   = 1478
  integer, parameter :: k_se80__n_se79   = 1479
  integer, parameter :: k_se80__p_as79   = 1480
  integer, parameter :: k_se80__he4_ge76   = 1481
  integer, parameter :: k_se81__n_se80   = 1482
  integer, parameter :: k_se81__he4_ge77   = 1483
  integer, parameter :: k_se82__n_se81   = 1484
  integer, parameter :: k_se82__he4_ge78   = 1485
  integer, parameter :: k_se83__n_se82   = 1486
  integer, parameter :: k_br68__p_se67   = 1487
  integer, parameter :: k_br69__n_br68   = 1488
  integer, parameter :: k_br69__p_se68   = 1489
  integer, parameter :: k_br69__he4_as65   = 1490
  integer, parameter :: k_br70__n_br69   = 1491
  integer, parameter :: k_br70__p_se69   = 1492
  integer, parameter :: k_br70__he4_as66   = 1493
  integer, parameter :: k_br71__n_br70   = 1494
  integer, parameter :: k_br71__p_se70   = 1495
  integer, parameter :: k_br71__he4_as67   = 1496
  integer, parameter :: k_br72__n_br71   = 1497
  integer, parameter :: k_br72__p_se71   = 1498
  integer, parameter :: k_br72__he4_as68   = 1499
  integer, parameter :: k_br73__n_br72   = 1500
  integer, parameter :: k_br73__p_se72   = 1501
  integer, parameter :: k_br73__he4_as69   = 1502
  integer, parameter :: k_br74__n_br73   = 1503
  integer, parameter :: k_br74__p_se73   = 1504
  integer, parameter :: k_br74__he4_as70   = 1505
  integer, parameter :: k_br75__n_br74   = 1506
  integer, parameter :: k_br75__p_se74   = 1507
  integer, parameter :: k_br75__he4_as71   = 1508
  integer, parameter :: k_br76__n_br75   = 1509
  integer, parameter :: k_br76__p_se75   = 1510
  integer, parameter :: k_br76__he4_as72   = 1511
  integer, parameter :: k_br77__n_br76   = 1512
  integer, parameter :: k_br77__p_se76   = 1513
  integer, parameter :: k_br77__he4_as73   = 1514
  integer, parameter :: k_br78__n_br77   = 1515
  integer, parameter :: k_br78__p_se77   = 1516
  integer, parameter :: k_br78__he4_as74   = 1517
  integer, parameter :: k_br79__n_br78   = 1518
  integer, parameter :: k_br79__p_se78   = 1519
  integer, parameter :: k_br79__he4_as75   = 1520
  integer, parameter :: k_br80__n_br79   = 1521
  integer, parameter :: k_br80__p_se79   = 1522
  integer, parameter :: k_br80__he4_as76   = 1523
  integer, parameter :: k_br81__n_br80   = 1524
  integer, parameter :: k_br81__p_se80   = 1525
  integer, parameter :: k_br81__he4_as77   = 1526
  integer, parameter :: k_br82__n_br81   = 1527
  integer, parameter :: k_br82__p_se81   = 1528
  integer, parameter :: k_br82__he4_as78   = 1529
  integer, parameter :: k_br83__n_br82   = 1530
  integer, parameter :: k_br83__p_se82   = 1531
  integer, parameter :: k_br83__he4_as79   = 1532
  integer, parameter :: k_kr69__p_br68   = 1533
  integer, parameter :: k_kr70__n_kr69   = 1534
  integer, parameter :: k_kr70__p_br69   = 1535
  integer, parameter :: k_kr70__p_se69__weak__wc12   = 1536
  integer, parameter :: k_kr71__n_kr70   = 1537
  integer, parameter :: k_kr71__p_br70   = 1538
  integer, parameter :: k_kr71__p_se70__weak__wc12   = 1539
  integer, parameter :: k_kr71__he4_se67   = 1540
  integer, parameter :: k_kr72__n_kr71   = 1541
  integer, parameter :: k_kr72__p_br71   = 1542
  integer, parameter :: k_kr72__p_se71__weak__wc12   = 1543
  integer, parameter :: k_kr72__he4_se68   = 1544
  integer, parameter :: k_kr73__n_kr72   = 1545
  integer, parameter :: k_kr73__p_br72   = 1546
  integer, parameter :: k_kr73__p_se72__weak__wc12   = 1547
  integer, parameter :: k_kr73__he4_se69   = 1548
  integer, parameter :: k_kr74__n_kr73   = 1549
  integer, parameter :: k_kr74__p_br73   = 1550
  integer, parameter :: k_kr74__he4_se70   = 1551
  integer, parameter :: k_kr75__n_kr74   = 1552
  integer, parameter :: k_kr75__p_br74   = 1553
  integer, parameter :: k_kr75__he4_se71   = 1554
  integer, parameter :: k_kr76__n_kr75   = 1555
  integer, parameter :: k_kr76__p_br75   = 1556
  integer, parameter :: k_kr76__he4_se72   = 1557
  integer, parameter :: k_kr77__n_kr76   = 1558
  integer, parameter :: k_kr77__p_br76   = 1559
  integer, parameter :: k_kr77__he4_se73   = 1560
  integer, parameter :: k_kr78__n_kr77   = 1561
  integer, parameter :: k_kr78__p_br77   = 1562
  integer, parameter :: k_kr78__he4_se74   = 1563
  integer, parameter :: k_kr79__n_kr78   = 1564
  integer, parameter :: k_kr79__p_br78   = 1565
  integer, parameter :: k_kr79__he4_se75   = 1566
  integer, parameter :: k_kr80__n_kr79   = 1567
  integer, parameter :: k_kr80__p_br79   = 1568
  integer, parameter :: k_kr80__he4_se76   = 1569
  integer, parameter :: k_kr81__n_kr80   = 1570
  integer, parameter :: k_kr81__p_br80   = 1571
  integer, parameter :: k_kr81__he4_se77   = 1572
  integer, parameter :: k_kr82__n_kr81   = 1573
  integer, parameter :: k_kr82__p_br81   = 1574
  integer, parameter :: k_kr82__he4_se78   = 1575
  integer, parameter :: k_kr83__n_kr82   = 1576
  integer, parameter :: k_kr83__p_br82   = 1577
  integer, parameter :: k_kr83__he4_se79   = 1578
  integer, parameter :: k_kr84__n_kr83   = 1579
  integer, parameter :: k_kr84__p_br83   = 1580
  integer, parameter :: k_kr84__he4_se80   = 1581
  integer, parameter :: k_kr85__n_kr84   = 1582
  integer, parameter :: k_kr85__he4_se81   = 1583
  integer, parameter :: k_kr86__n_kr85   = 1584
  integer, parameter :: k_kr86__he4_se82   = 1585
  integer, parameter :: k_kr87__n_kr86   = 1586
  integer, parameter :: k_kr87__he4_se83   = 1587
  integer, parameter :: k_rb73__p_kr72   = 1588
  integer, parameter :: k_rb73__he4_br69   = 1589
  integer, parameter :: k_rb74__n_rb73   = 1590
  integer, parameter :: k_rb74__p_kr73   = 1591
  integer, parameter :: k_rb74__he4_br70   = 1592
  integer, parameter :: k_rb75__n_rb74   = 1593
  integer, parameter :: k_rb75__p_kr74   = 1594
  integer, parameter :: k_rb75__he4_br71   = 1595
  integer, parameter :: k_rb76__n_rb75   = 1596
  integer, parameter :: k_rb76__p_kr75   = 1597
  integer, parameter :: k_rb76__he4_br72   = 1598
  integer, parameter :: k_rb76__he4_se72__weak__wc12   = 1599
  integer, parameter :: k_rb77__n_rb76   = 1600
  integer, parameter :: k_rb77__p_kr76   = 1601
  integer, parameter :: k_rb77__he4_br73   = 1602
  integer, parameter :: k_rb78__n_rb77   = 1603
  integer, parameter :: k_rb78__p_kr77   = 1604
  integer, parameter :: k_rb78__he4_br74   = 1605
  integer, parameter :: k_rb79__n_rb78   = 1606
  integer, parameter :: k_rb79__p_kr78   = 1607
  integer, parameter :: k_rb79__he4_br75   = 1608
  integer, parameter :: k_rb80__n_rb79   = 1609
  integer, parameter :: k_rb80__p_kr79   = 1610
  integer, parameter :: k_rb80__he4_br76   = 1611
  integer, parameter :: k_rb81__n_rb80   = 1612
  integer, parameter :: k_rb81__p_kr80   = 1613
  integer, parameter :: k_rb81__he4_br77   = 1614
  integer, parameter :: k_rb82__n_rb81   = 1615
  integer, parameter :: k_rb82__p_kr81   = 1616
  integer, parameter :: k_rb82__he4_br78   = 1617
  integer, parameter :: k_rb83__n_rb82   = 1618
  integer, parameter :: k_rb83__p_kr82   = 1619
  integer, parameter :: k_rb83__he4_br79   = 1620
  integer, parameter :: k_rb84__n_rb83   = 1621
  integer, parameter :: k_rb84__p_kr83   = 1622
  integer, parameter :: k_rb84__he4_br80   = 1623
  integer, parameter :: k_rb85__n_rb84   = 1624
  integer, parameter :: k_rb85__p_kr84   = 1625
  integer, parameter :: k_rb85__he4_br81   = 1626
  integer, parameter :: k_sr74__p_rb73   = 1627
  integer, parameter :: k_sr74__he4_kr70   = 1628
  integer, parameter :: k_sr75__n_sr74   = 1629
  integer, parameter :: k_sr75__p_rb74   = 1630
  integer, parameter :: k_sr75__p_kr74__weak__wc12   = 1631
  integer, parameter :: k_sr75__he4_kr71   = 1632
  integer, parameter :: k_sr76__n_sr75   = 1633
  integer, parameter :: k_sr76__p_rb75   = 1634
  integer, parameter :: k_sr76__p_kr75__weak__wc12   = 1635
  integer, parameter :: k_sr76__he4_kr72   = 1636
  integer, parameter :: k_sr77__n_sr76   = 1637
  integer, parameter :: k_sr77__p_rb76   = 1638
  integer, parameter :: k_sr77__p_kr76__weak__wc12   = 1639
  integer, parameter :: k_sr77__he4_kr73   = 1640
  integer, parameter :: k_sr78__n_sr77   = 1641
  integer, parameter :: k_sr78__p_rb77   = 1642
  integer, parameter :: k_sr78__he4_kr74   = 1643
  integer, parameter :: k_sr79__n_sr78   = 1644
  integer, parameter :: k_sr79__p_rb78   = 1645
  integer, parameter :: k_sr79__he4_kr75   = 1646
  integer, parameter :: k_sr80__n_sr79   = 1647
  integer, parameter :: k_sr80__p_rb79   = 1648
  integer, parameter :: k_sr80__he4_kr76   = 1649
  integer, parameter :: k_sr81__n_sr80   = 1650
  integer, parameter :: k_sr81__p_rb80   = 1651
  integer, parameter :: k_sr81__he4_kr77   = 1652
  integer, parameter :: k_sr82__n_sr81   = 1653
  integer, parameter :: k_sr82__p_rb81   = 1654
  integer, parameter :: k_sr82__he4_kr78   = 1655
  integer, parameter :: k_sr83__n_sr82   = 1656
  integer, parameter :: k_sr83__p_rb82   = 1657
  integer, parameter :: k_sr83__he4_kr79   = 1658
  integer, parameter :: k_sr84__n_sr83   = 1659
  integer, parameter :: k_sr84__p_rb83   = 1660
  integer, parameter :: k_sr84__he4_kr80   = 1661
  integer, parameter :: k_y75__p_sr74   = 1662
  integer, parameter :: k_y76__n_y75   = 1663
  integer, parameter :: k_y76__p_sr75   = 1664
  integer, parameter :: k_y77__n_y76   = 1665
  integer, parameter :: k_y77__p_sr76   = 1666
  integer, parameter :: k_y77__he4_rb73   = 1667
  integer, parameter :: k_y78__n_y77   = 1668
  integer, parameter :: k_y78__p_sr77   = 1669
  integer, parameter :: k_y78__he4_rb74   = 1670
  integer, parameter :: k_y79__n_y78   = 1671
  integer, parameter :: k_y79__p_sr78   = 1672
  integer, parameter :: k_y79__he4_rb75   = 1673
  integer, parameter :: k_y80__n_y79   = 1674
  integer, parameter :: k_y80__p_sr79   = 1675
  integer, parameter :: k_y80__he4_rb76   = 1676
  integer, parameter :: k_y81__n_y80   = 1677
  integer, parameter :: k_y81__p_sr80   = 1678
  integer, parameter :: k_y81__he4_rb77   = 1679
  integer, parameter :: k_y82__n_y81   = 1680
  integer, parameter :: k_y82__p_sr81   = 1681
  integer, parameter :: k_y82__he4_rb78   = 1682
  integer, parameter :: k_y83__n_y82   = 1683
  integer, parameter :: k_y83__p_sr82   = 1684
  integer, parameter :: k_y83__he4_rb79   = 1685
  integer, parameter :: k_y84__n_y83   = 1686
  integer, parameter :: k_y84__p_sr83   = 1687
  integer, parameter :: k_y84__he4_rb80   = 1688
  integer, parameter :: k_y85__n_y84   = 1689
  integer, parameter :: k_y85__p_sr84   = 1690
  integer, parameter :: k_y85__he4_rb81   = 1691
  integer, parameter :: k_y86__n_y85   = 1692
  integer, parameter :: k_y86__he4_rb82   = 1693
  integer, parameter :: k_y87__n_y86   = 1694
  integer, parameter :: k_y87__he4_rb83   = 1695
  integer, parameter :: k_zr78__p_y77   = 1696
  integer, parameter :: k_zr78__he4_sr74   = 1697
  integer, parameter :: k_zr79__n_zr78   = 1698
  integer, parameter :: k_zr79__p_y78   = 1699
  integer, parameter :: k_zr79__he4_sr75   = 1700
  integer, parameter :: k_zr80__n_zr79   = 1701
  integer, parameter :: k_zr80__p_y79   = 1702
  integer, parameter :: k_zr80__he4_sr76   = 1703
  integer, parameter :: k_zr81__n_zr80   = 1704
  integer, parameter :: k_zr81__p_y80   = 1705
  integer, parameter :: k_zr81__p_sr80__weak__wc12   = 1706
  integer, parameter :: k_zr81__he4_sr77   = 1707
  integer, parameter :: k_zr82__n_zr81   = 1708
  integer, parameter :: k_zr82__p_y81   = 1709
  integer, parameter :: k_zr82__he4_sr78   = 1710
  integer, parameter :: k_zr83__n_zr82   = 1711
  integer, parameter :: k_zr83__p_y82   = 1712
  integer, parameter :: k_zr83__he4_sr79   = 1713
  integer, parameter :: k_zr84__n_zr83   = 1714
  integer, parameter :: k_zr84__p_y83   = 1715
  integer, parameter :: k_zr84__he4_sr80   = 1716
  integer, parameter :: k_zr85__n_zr84   = 1717
  integer, parameter :: k_zr85__p_y84   = 1718
  integer, parameter :: k_zr85__he4_sr81   = 1719
  integer, parameter :: k_zr86__n_zr85   = 1720
  integer, parameter :: k_zr86__p_y85   = 1721
  integer, parameter :: k_zr86__he4_sr82   = 1722
  integer, parameter :: k_zr87__n_zr86   = 1723
  integer, parameter :: k_zr87__p_y86   = 1724
  integer, parameter :: k_zr87__he4_sr83   = 1725
  integer, parameter :: k_zr88__n_zr87   = 1726
  integer, parameter :: k_zr88__p_y87   = 1727
  integer, parameter :: k_zr88__he4_sr84   = 1728
  integer, parameter :: k_zr89__n_zr88   = 1729
  integer, parameter :: k_zr90__n_zr89   = 1730
  integer, parameter :: k_nb82__p_zr81   = 1731
  integer, parameter :: k_nb82__he4_y78   = 1732
  integer, parameter :: k_nb83__n_nb82   = 1733
  integer, parameter :: k_nb83__p_zr82   = 1734
  integer, parameter :: k_nb83__he4_y79   = 1735
  integer, parameter :: k_nb84__n_nb83   = 1736
  integer, parameter :: k_nb84__p_zr83   = 1737
  integer, parameter :: k_nb84__he4_y80   = 1738
  integer, parameter :: k_nb85__n_nb84   = 1739
  integer, parameter :: k_nb85__p_zr84   = 1740
  integer, parameter :: k_nb85__he4_y81   = 1741
  integer, parameter :: k_nb86__n_nb85   = 1742
  integer, parameter :: k_nb86__p_zr85   = 1743
  integer, parameter :: k_nb86__he4_y82   = 1744
  integer, parameter :: k_nb87__n_nb86   = 1745
  integer, parameter :: k_nb87__p_zr86   = 1746
  integer, parameter :: k_nb87__he4_y83   = 1747
  integer, parameter :: k_nb88__n_nb87   = 1748
  integer, parameter :: k_nb88__p_zr87   = 1749
  integer, parameter :: k_nb88__he4_y84   = 1750
  integer, parameter :: k_nb89__n_nb88   = 1751
  integer, parameter :: k_nb89__p_zr88   = 1752
  integer, parameter :: k_nb89__he4_y85   = 1753
  integer, parameter :: k_nb90__n_nb89   = 1754
  integer, parameter :: k_nb90__p_zr89   = 1755
  integer, parameter :: k_nb90__he4_y86   = 1756
  integer, parameter :: k_mo83__p_nb82   = 1757
  integer, parameter :: k_mo83__he4_zr79   = 1758
  integer, parameter :: k_mo84__n_mo83   = 1759
  integer, parameter :: k_mo84__p_nb83   = 1760
  integer, parameter :: k_mo84__he4_zr80   = 1761
  integer, parameter :: k_mo85__n_mo84   = 1762
  integer, parameter :: k_mo85__p_nb84   = 1763
  integer, parameter :: k_mo85__p_zr84__weak__wc12   = 1764
  integer, parameter :: k_mo85__he4_zr81   = 1765
  integer, parameter :: k_mo86__n_mo85   = 1766
  integer, parameter :: k_mo86__p_nb85   = 1767
  integer, parameter :: k_mo86__he4_zr82   = 1768
  integer, parameter :: k_mo87__n_mo86   = 1769
  integer, parameter :: k_mo87__p_nb86   = 1770
  integer, parameter :: k_mo87__p_zr86__weak__wc12   = 1771
  integer, parameter :: k_mo87__he4_zr83   = 1772
  integer, parameter :: k_mo88__n_mo87   = 1773
  integer, parameter :: k_mo88__p_nb87   = 1774
  integer, parameter :: k_mo88__he4_zr84   = 1775
  integer, parameter :: k_mo89__n_mo88   = 1776
  integer, parameter :: k_mo89__p_nb88   = 1777
  integer, parameter :: k_mo89__he4_zr85   = 1778
  integer, parameter :: k_mo90__n_mo89   = 1779
  integer, parameter :: k_mo90__p_nb89   = 1780
  integer, parameter :: k_mo90__he4_zr86   = 1781
  integer, parameter :: k_tc89__p_mo88   = 1782
  integer, parameter :: k_tc89__he4_nb85   = 1783
  integer, parameter :: k_tc90__n_tc89   = 1784
  integer, parameter :: k_tc90__p_mo89   = 1785
  integer, parameter :: k_tc90__he4_nb86   = 1786
  integer, parameter :: k_tc91__n_tc90   = 1787
  integer, parameter :: k_tc91__p_mo90   = 1788
  integer, parameter :: k_tc91__he4_nb87   = 1789
  integer, parameter :: k_li6__n_p_he4   = 1790
  integer, parameter :: k_be9__n_he4_he4   = 1791
  integer, parameter :: k_c12__he4_he4_he4   = 1792
  integer, parameter :: k_al22__p_p_ne20__weak__wc12   = 1793
  integer, parameter :: k_si23__p_p_na21__weak__wc12   = 1794
  integer, parameter :: k_n_p__d   = 1795
  integer, parameter :: k_p_p__d__weak__bet_pos_   = 1796
  integer, parameter :: k_p_p__d__weak__electron_capture   = 1797
  integer, parameter :: k_n_d__t   = 1798
  integer, parameter :: k_p_d__he3   = 1799
  integer, parameter :: k_d_d__he4   = 1800
  integer, parameter :: k_he4_d__li6   = 1801
  integer, parameter :: k_p_t__he4   = 1802
  integer, parameter :: k_he4_t__li7   = 1803
  integer, parameter :: k_n_he3__he4   = 1804
  integer, parameter :: k_p_he3__he4__weak__bet_pos_   = 1805
  integer, parameter :: k_he4_he3__be7   = 1806
  integer, parameter :: k_n_li6__li7   = 1807
  integer, parameter :: k_p_li6__be7   = 1808
  integer, parameter :: k_he4_li6__b10   = 1809
  integer, parameter :: k_he4_li7__b11   = 1810
  integer, parameter :: k_p_be7__b8   = 1811
  integer, parameter :: k_he4_be7__c11   = 1812
  integer, parameter :: k_p_be9__b10   = 1813
  integer, parameter :: k_n_b10__b11   = 1814
  integer, parameter :: k_p_b10__c11   = 1815
  integer, parameter :: k_p_b11__c12   = 1816
  integer, parameter :: k_n_c11__c12   = 1817
  integer, parameter :: k_p_c11__n12   = 1818
  integer, parameter :: k_n_c12__c13   = 1819
  integer, parameter :: k_p_c12__n13   = 1820
  integer, parameter :: k_he4_c12__o16   = 1821
  integer, parameter :: k_n_c13__c14   = 1822
  integer, parameter :: k_p_c13__n14   = 1823
  integer, parameter :: k_p_c14__n15   = 1824
  integer, parameter :: k_he4_c14__o18   = 1825
  integer, parameter :: k_n_n13__n14   = 1826
  integer, parameter :: k_p_n13__o14   = 1827
  integer, parameter :: k_n_n14__n15   = 1828
  integer, parameter :: k_p_n14__o15   = 1829
  integer, parameter :: k_he4_n14__f18   = 1830
  integer, parameter :: k_p_n15__o16   = 1831
  integer, parameter :: k_he4_n15__f19   = 1832
  integer, parameter :: k_n_o14__o15   = 1833
  integer, parameter :: k_he4_o14__ne18   = 1834
  integer, parameter :: k_n_o15__o16   = 1835
  integer, parameter :: k_he4_o15__ne19   = 1836
  integer, parameter :: k_n_o16__o17   = 1837
  integer, parameter :: k_p_o16__f17   = 1838
  integer, parameter :: k_he4_o16__ne20   = 1839
  integer, parameter :: k_n_o17__o18   = 1840
  integer, parameter :: k_p_o17__f18   = 1841
  integer, parameter :: k_he4_o17__ne21   = 1842
  integer, parameter :: k_n_o18__o19   = 1843
  integer, parameter :: k_p_o18__f19   = 1844
  integer, parameter :: k_he4_o18__ne22   = 1845
  integer, parameter :: k_p_o19__f20   = 1846
  integer, parameter :: k_he4_o19__ne23   = 1847
  integer, parameter :: k_n_f17__f18   = 1848
  integer, parameter :: k_p_f17__ne18   = 1849
  integer, parameter :: k_he4_f17__na21   = 1850
  integer, parameter :: k_n_f18__f19   = 1851
  integer, parameter :: k_p_f18__ne19   = 1852
  integer, parameter :: k_he4_f18__na22   = 1853
  integer, parameter :: k_n_f19__f20   = 1854
  integer, parameter :: k_p_f19__ne20   = 1855
  integer, parameter :: k_he4_f19__na23   = 1856
  integer, parameter :: k_n_f20__f21   = 1857
  integer, parameter :: k_p_f20__ne21   = 1858
  integer, parameter :: k_he4_f20__na24   = 1859
  integer, parameter :: k_p_f21__ne22   = 1860
  integer, parameter :: k_he4_f21__na25   = 1861
  integer, parameter :: k_n_ne17__ne18   = 1862
  integer, parameter :: k_he4_ne17__mg21   = 1863
  integer, parameter :: k_n_ne18__ne19   = 1864
  integer, parameter :: k_p_ne18__na19   = 1865
  integer, parameter :: k_he4_ne18__mg22   = 1866
  integer, parameter :: k_n_ne19__ne20   = 1867
  integer, parameter :: k_p_ne19__na20   = 1868
  integer, parameter :: k_he4_ne19__mg23   = 1869
  integer, parameter :: k_n_ne20__ne21   = 1870
  integer, parameter :: k_p_ne20__na21   = 1871
  integer, parameter :: k_he4_ne20__mg24   = 1872
  integer, parameter :: k_n_ne21__ne22   = 1873
  integer, parameter :: k_p_ne21__na22   = 1874
  integer, parameter :: k_he4_ne21__mg25   = 1875
  integer, parameter :: k_n_ne22__ne23   = 1876
  integer, parameter :: k_p_ne22__na23   = 1877
  integer, parameter :: k_he4_ne22__mg26   = 1878
  integer, parameter :: k_n_ne23__ne24   = 1879
  integer, parameter :: k_p_ne23__na24   = 1880
  integer, parameter :: k_he4_ne23__mg27   = 1881
  integer, parameter :: k_p_ne24__na25   = 1882
  integer, parameter :: k_he4_ne24__mg28   = 1883
  integer, parameter :: k_n_na19__na20   = 1884
  integer, parameter :: k_p_na19__mg20   = 1885
  integer, parameter :: k_he4_na19__al23   = 1886
  integer, parameter :: k_n_na20__na21   = 1887
  integer, parameter :: k_p_na20__mg21   = 1888
  integer, parameter :: k_he4_na20__al24   = 1889
  integer, parameter :: k_n_na21__na22   = 1890
  integer, parameter :: k_p_na21__mg22   = 1891
  integer, parameter :: k_he4_na21__al25   = 1892
  integer, parameter :: k_n_na22__na23   = 1893
  integer, parameter :: k_p_na22__mg23   = 1894
  integer, parameter :: k_he4_na22__al26   = 1895
  integer, parameter :: k_n_na23__na24   = 1896
  integer, parameter :: k_p_na23__mg24   = 1897
  integer, parameter :: k_he4_na23__al27   = 1898
  integer, parameter :: k_n_na24__na25   = 1899
  integer, parameter :: k_p_na24__mg25   = 1900
  integer, parameter :: k_he4_na24__al28   = 1901
  integer, parameter :: k_n_na25__na26   = 1902
  integer, parameter :: k_p_na25__mg26   = 1903
  integer, parameter :: k_he4_na25__al29   = 1904
  integer, parameter :: k_n_na26__na27   = 1905
  integer, parameter :: k_p_na26__mg27   = 1906
  integer, parameter :: k_he4_na26__al30   = 1907
  integer, parameter :: k_p_na27__mg28   = 1908
  integer, parameter :: k_he4_na27__al31   = 1909
  integer, parameter :: k_n_mg20__mg21   = 1910
  integer, parameter :: k_he4_mg20__si24   = 1911
  integer, parameter :: k_n_mg21__mg22   = 1912
  integer, parameter :: k_p_mg21__al22   = 1913
  integer, parameter :: k_he4_mg21__si25   = 1914
  integer, parameter :: k_n_mg22__mg23   = 1915
  integer, parameter :: k_p_mg22__al23   = 1916
  integer, parameter :: k_he4_mg22__si26   = 1917
  integer, parameter :: k_n_mg23__mg24   = 1918
  integer, parameter :: k_p_mg23__al24   = 1919
  integer, parameter :: k_he4_mg23__si27   = 1920
  integer, parameter :: k_n_mg24__mg25   = 1921
  integer, parameter :: k_p_mg24__al25   = 1922
  integer, parameter :: k_he4_mg24__si28   = 1923
  integer, parameter :: k_n_mg25__mg26   = 1924
  integer, parameter :: k_p_mg25__al26   = 1925
  integer, parameter :: k_he4_mg25__si29   = 1926
  integer, parameter :: k_n_mg26__mg27   = 1927
  integer, parameter :: k_p_mg26__al27   = 1928
  integer, parameter :: k_he4_mg26__si30   = 1929
  integer, parameter :: k_n_mg27__mg28   = 1930
  integer, parameter :: k_p_mg27__al28   = 1931
  integer, parameter :: k_he4_mg27__si31   = 1932
  integer, parameter :: k_n_mg28__mg29   = 1933
  integer, parameter :: k_p_mg28__al29   = 1934
  integer, parameter :: k_he4_mg28__si32   = 1935
  integer, parameter :: k_p_mg29__al30   = 1936
  integer, parameter :: k_he4_mg29__si33   = 1937
  integer, parameter :: k_n_al22__al23   = 1938
  integer, parameter :: k_p_al22__si23   = 1939
  integer, parameter :: k_n_al23__al24   = 1940
  integer, parameter :: k_p_al23__si24   = 1941
  integer, parameter :: k_he4_al23__p27   = 1942
  integer, parameter :: k_n_al24__al25   = 1943
  integer, parameter :: k_p_al24__si25   = 1944
  integer, parameter :: k_he4_al24__p28   = 1945
  integer, parameter :: k_n_al25__al26   = 1946
  integer, parameter :: k_p_al25__si26   = 1947
  integer, parameter :: k_he4_al25__p29   = 1948
  integer, parameter :: k_n_al26__al27   = 1949
  integer, parameter :: k_p_al26__si27   = 1950
  integer, parameter :: k_he4_al26__p30   = 1951
  integer, parameter :: k_n_al27__al28   = 1952
  integer, parameter :: k_p_al27__si28   = 1953
  integer, parameter :: k_he4_al27__p31   = 1954
  integer, parameter :: k_n_al28__al29   = 1955
  integer, parameter :: k_p_al28__si29   = 1956
  integer, parameter :: k_he4_al28__p32   = 1957
  integer, parameter :: k_n_al29__al30   = 1958
  integer, parameter :: k_p_al29__si30   = 1959
  integer, parameter :: k_he4_al29__p33   = 1960
  integer, parameter :: k_n_al30__al31   = 1961
  integer, parameter :: k_p_al30__si31   = 1962
  integer, parameter :: k_he4_al30__p34   = 1963
  integer, parameter :: k_p_al31__si32   = 1964
  integer, parameter :: k_he4_al31__p35   = 1965
  integer, parameter :: k_n_si23__si24   = 1966
  integer, parameter :: k_n_si24__si25   = 1967
  integer, parameter :: k_he4_si24__s28   = 1968
  integer, parameter :: k_n_si25__si26   = 1969
  integer, parameter :: k_he4_si25__s29   = 1970
  integer, parameter :: k_n_si26__si27   = 1971
  integer, parameter :: k_p_si26__p27   = 1972
  integer, parameter :: k_he4_si26__s30   = 1973
  integer, parameter :: k_n_si27__si28   = 1974
  integer, parameter :: k_p_si27__p28   = 1975
  integer, parameter :: k_he4_si27__s31   = 1976
  integer, parameter :: k_n_si28__si29   = 1977
  integer, parameter :: k_p_si28__p29   = 1978
  integer, parameter :: k_he4_si28__s32   = 1979
  integer, parameter :: k_n_si29__si30   = 1980
  integer, parameter :: k_p_si29__p30   = 1981
  integer, parameter :: k_he4_si29__s33   = 1982
  integer, parameter :: k_n_si30__si31   = 1983
  integer, parameter :: k_p_si30__p31   = 1984
  integer, parameter :: k_he4_si30__s34   = 1985
  integer, parameter :: k_n_si31__si32   = 1986
  integer, parameter :: k_p_si31__p32   = 1987
  integer, parameter :: k_he4_si31__s35   = 1988
  integer, parameter :: k_n_si32__si33   = 1989
  integer, parameter :: k_p_si32__p33   = 1990
  integer, parameter :: k_he4_si32__s36   = 1991
  integer, parameter :: k_n_si33__si34   = 1992
  integer, parameter :: k_p_si33__p34   = 1993
  integer, parameter :: k_he4_si33__s37   = 1994
  integer, parameter :: k_p_si34__p35   = 1995
  integer, parameter :: k_he4_si34__s38   = 1996
  integer, parameter :: k_n_p27__p28   = 1997
  integer, parameter :: k_p_p27__s28   = 1998
  integer, parameter :: k_he4_p27__cl31   = 1999
  integer, parameter :: k_n_p28__p29   = 2000
  integer, parameter :: k_p_p28__s29   = 2001
  integer, parameter :: k_he4_p28__cl32   = 2002
  integer, parameter :: k_n_p29__p30   = 2003
  integer, parameter :: k_p_p29__s30   = 2004
  integer, parameter :: k_he4_p29__cl33   = 2005
  integer, parameter :: k_n_p30__p31   = 2006
  integer, parameter :: k_p_p30__s31   = 2007
  integer, parameter :: k_he4_p30__cl34   = 2008
  integer, parameter :: k_n_p31__p32   = 2009
  integer, parameter :: k_p_p31__s32   = 2010
  integer, parameter :: k_he4_p31__cl35   = 2011
  integer, parameter :: k_n_p32__p33   = 2012
  integer, parameter :: k_p_p32__s33   = 2013
  integer, parameter :: k_he4_p32__cl36   = 2014
  integer, parameter :: k_n_p33__p34   = 2015
  integer, parameter :: k_p_p33__s34   = 2016
  integer, parameter :: k_he4_p33__cl37   = 2017
  integer, parameter :: k_n_p34__p35   = 2018
  integer, parameter :: k_p_p34__s35   = 2019
  integer, parameter :: k_he4_p34__cl38   = 2020
  integer, parameter :: k_n_p35__p36   = 2021
  integer, parameter :: k_p_p35__s36   = 2022
  integer, parameter :: k_he4_p35__cl39   = 2023
  integer, parameter :: k_n_p36__p37   = 2024
  integer, parameter :: k_p_p36__s37   = 2025
  integer, parameter :: k_he4_p36__cl40   = 2026
  integer, parameter :: k_n_p37__p38   = 2027
  integer, parameter :: k_p_p37__s38   = 2028
  integer, parameter :: k_he4_p37__cl41   = 2029
  integer, parameter :: k_p_p38__s39   = 2030
  integer, parameter :: k_he4_p38__cl42   = 2031
  integer, parameter :: k_n_s28__s29   = 2032
  integer, parameter :: k_he4_s28__ar32   = 2033
  integer, parameter :: k_n_s29__s30   = 2034
  integer, parameter :: k_he4_s29__ar33   = 2035
  integer, parameter :: k_n_s30__s31   = 2036
  integer, parameter :: k_p_s30__cl31   = 2037
  integer, parameter :: k_he4_s30__ar34   = 2038
  integer, parameter :: k_n_s31__s32   = 2039
  integer, parameter :: k_p_s31__cl32   = 2040
  integer, parameter :: k_he4_s31__ar35   = 2041
  integer, parameter :: k_n_s32__s33   = 2042
  integer, parameter :: k_p_s32__cl33   = 2043
  integer, parameter :: k_he4_s32__ar36   = 2044
  integer, parameter :: k_n_s33__s34   = 2045
  integer, parameter :: k_p_s33__cl34   = 2046
  integer, parameter :: k_he4_s33__ar37   = 2047
  integer, parameter :: k_n_s34__s35   = 2048
  integer, parameter :: k_p_s34__cl35   = 2049
  integer, parameter :: k_he4_s34__ar38   = 2050
  integer, parameter :: k_n_s35__s36   = 2051
  integer, parameter :: k_p_s35__cl36   = 2052
  integer, parameter :: k_he4_s35__ar39   = 2053
  integer, parameter :: k_n_s36__s37   = 2054
  integer, parameter :: k_p_s36__cl37   = 2055
  integer, parameter :: k_he4_s36__ar40   = 2056
  integer, parameter :: k_n_s37__s38   = 2057
  integer, parameter :: k_p_s37__cl38   = 2058
  integer, parameter :: k_he4_s37__ar41   = 2059
  integer, parameter :: k_n_s38__s39   = 2060
  integer, parameter :: k_p_s38__cl39   = 2061
  integer, parameter :: k_he4_s38__ar42   = 2062
  integer, parameter :: k_n_s39__s40   = 2063
  integer, parameter :: k_p_s39__cl40   = 2064
  integer, parameter :: k_he4_s39__ar43   = 2065
  integer, parameter :: k_n_s40__s41   = 2066
  integer, parameter :: k_p_s40__cl41   = 2067
  integer, parameter :: k_he4_s40__ar44   = 2068
  integer, parameter :: k_n_s41__s42   = 2069
  integer, parameter :: k_p_s41__cl42   = 2070
  integer, parameter :: k_he4_s41__ar45   = 2071
  integer, parameter :: k_p_s42__cl43   = 2072
  integer, parameter :: k_he4_s42__ar46   = 2073
  integer, parameter :: k_n_cl31__cl32   = 2074
  integer, parameter :: k_p_cl31__ar32   = 2075
  integer, parameter :: k_he4_cl31__k35   = 2076
  integer, parameter :: k_n_cl32__cl33   = 2077
  integer, parameter :: k_p_cl32__ar33   = 2078
  integer, parameter :: k_he4_cl32__k36   = 2079
  integer, parameter :: k_n_cl33__cl34   = 2080
  integer, parameter :: k_p_cl33__ar34   = 2081
  integer, parameter :: k_he4_cl33__k37   = 2082
  integer, parameter :: k_n_cl34__cl35   = 2083
  integer, parameter :: k_p_cl34__ar35   = 2084
  integer, parameter :: k_he4_cl34__k38   = 2085
  integer, parameter :: k_n_cl35__cl36   = 2086
  integer, parameter :: k_p_cl35__ar36   = 2087
  integer, parameter :: k_he4_cl35__k39   = 2088
  integer, parameter :: k_n_cl36__cl37   = 2089
  integer, parameter :: k_p_cl36__ar37   = 2090
  integer, parameter :: k_he4_cl36__k40   = 2091
  integer, parameter :: k_n_cl37__cl38   = 2092
  integer, parameter :: k_p_cl37__ar38   = 2093
  integer, parameter :: k_he4_cl37__k41   = 2094
  integer, parameter :: k_n_cl38__cl39   = 2095
  integer, parameter :: k_p_cl38__ar39   = 2096
  integer, parameter :: k_he4_cl38__k42   = 2097
  integer, parameter :: k_n_cl39__cl40   = 2098
  integer, parameter :: k_p_cl39__ar40   = 2099
  integer, parameter :: k_he4_cl39__k43   = 2100
  integer, parameter :: k_n_cl40__cl41   = 2101
  integer, parameter :: k_p_cl40__ar41   = 2102
  integer, parameter :: k_he4_cl40__k44   = 2103
  integer, parameter :: k_n_cl41__cl42   = 2104
  integer, parameter :: k_p_cl41__ar42   = 2105
  integer, parameter :: k_he4_cl41__k45   = 2106
  integer, parameter :: k_n_cl42__cl43   = 2107
  integer, parameter :: k_p_cl42__ar43   = 2108
  integer, parameter :: k_he4_cl42__k46   = 2109
  integer, parameter :: k_n_cl43__cl44   = 2110
  integer, parameter :: k_p_cl43__ar44   = 2111
  integer, parameter :: k_he4_cl43__k47   = 2112
  integer, parameter :: k_n_cl44__cl45   = 2113
  integer, parameter :: k_p_cl44__ar45   = 2114
  integer, parameter :: k_he4_cl44__k48   = 2115
  integer, parameter :: k_p_cl45__ar46   = 2116
  integer, parameter :: k_he4_cl45__k49   = 2117
  integer, parameter :: k_n_ar32__ar33   = 2118
  integer, parameter :: k_he4_ar32__ca36   = 2119
  integer, parameter :: k_n_ar33__ar34   = 2120
  integer, parameter :: k_he4_ar33__ca37   = 2121
  integer, parameter :: k_n_ar34__ar35   = 2122
  integer, parameter :: k_p_ar34__k35   = 2123
  integer, parameter :: k_he4_ar34__ca38   = 2124
  integer, parameter :: k_n_ar35__ar36   = 2125
  integer, parameter :: k_p_ar35__k36   = 2126
  integer, parameter :: k_he4_ar35__ca39   = 2127
  integer, parameter :: k_n_ar36__ar37   = 2128
  integer, parameter :: k_p_ar36__k37   = 2129
  integer, parameter :: k_he4_ar36__ca40   = 2130
  integer, parameter :: k_n_ar37__ar38   = 2131
  integer, parameter :: k_p_ar37__k38   = 2132
  integer, parameter :: k_he4_ar37__ca41   = 2133
  integer, parameter :: k_n_ar38__ar39   = 2134
  integer, parameter :: k_p_ar38__k39   = 2135
  integer, parameter :: k_he4_ar38__ca42   = 2136
  integer, parameter :: k_n_ar39__ar40   = 2137
  integer, parameter :: k_p_ar39__k40   = 2138
  integer, parameter :: k_he4_ar39__ca43   = 2139
  integer, parameter :: k_n_ar40__ar41   = 2140
  integer, parameter :: k_p_ar40__k41   = 2141
  integer, parameter :: k_he4_ar40__ca44   = 2142
  integer, parameter :: k_n_ar41__ar42   = 2143
  integer, parameter :: k_p_ar41__k42   = 2144
  integer, parameter :: k_he4_ar41__ca45   = 2145
  integer, parameter :: k_n_ar42__ar43   = 2146
  integer, parameter :: k_p_ar42__k43   = 2147
  integer, parameter :: k_he4_ar42__ca46   = 2148
  integer, parameter :: k_n_ar43__ar44   = 2149
  integer, parameter :: k_p_ar43__k44   = 2150
  integer, parameter :: k_he4_ar43__ca47   = 2151
  integer, parameter :: k_n_ar44__ar45   = 2152
  integer, parameter :: k_p_ar44__k45   = 2153
  integer, parameter :: k_he4_ar44__ca48   = 2154
  integer, parameter :: k_n_ar45__ar46   = 2155
  integer, parameter :: k_p_ar45__k46   = 2156
  integer, parameter :: k_he4_ar45__ca49   = 2157
  integer, parameter :: k_p_ar46__k47   = 2158
  integer, parameter :: k_n_k35__k36   = 2159
  integer, parameter :: k_p_k35__ca36   = 2160
  integer, parameter :: k_n_k36__k37   = 2161
  integer, parameter :: k_p_k36__ca37   = 2162
  integer, parameter :: k_he4_k36__sc40   = 2163
  integer, parameter :: k_n_k37__k38   = 2164
  integer, parameter :: k_p_k37__ca38   = 2165
  integer, parameter :: k_he4_k37__sc41   = 2166
  integer, parameter :: k_n_k38__k39   = 2167
  integer, parameter :: k_p_k38__ca39   = 2168
  integer, parameter :: k_he4_k38__sc42   = 2169
  integer, parameter :: k_n_k39__k40   = 2170
  integer, parameter :: k_p_k39__ca40   = 2171
  integer, parameter :: k_he4_k39__sc43   = 2172
  integer, parameter :: k_n_k40__k41   = 2173
  integer, parameter :: k_p_k40__ca41   = 2174
  integer, parameter :: k_he4_k40__sc44   = 2175
  integer, parameter :: k_n_k41__k42   = 2176
  integer, parameter :: k_p_k41__ca42   = 2177
  integer, parameter :: k_he4_k41__sc45   = 2178
  integer, parameter :: k_n_k42__k43   = 2179
  integer, parameter :: k_p_k42__ca43   = 2180
  integer, parameter :: k_he4_k42__sc46   = 2181
  integer, parameter :: k_n_k43__k44   = 2182
  integer, parameter :: k_p_k43__ca44   = 2183
  integer, parameter :: k_he4_k43__sc47   = 2184
  integer, parameter :: k_n_k44__k45   = 2185
  integer, parameter :: k_p_k44__ca45   = 2186
  integer, parameter :: k_he4_k44__sc48   = 2187
  integer, parameter :: k_n_k45__k46   = 2188
  integer, parameter :: k_p_k45__ca46   = 2189
  integer, parameter :: k_he4_k45__sc49   = 2190
  integer, parameter :: k_n_k46__k47   = 2191
  integer, parameter :: k_p_k46__ca47   = 2192
  integer, parameter :: k_he4_k46__sc50   = 2193
  integer, parameter :: k_n_k47__k48   = 2194
  integer, parameter :: k_p_k47__ca48   = 2195
  integer, parameter :: k_he4_k47__sc51   = 2196
  integer, parameter :: k_n_k48__k49   = 2197
  integer, parameter :: k_p_k48__ca49   = 2198
  integer, parameter :: k_n_ca36__ca37   = 2199
  integer, parameter :: k_n_ca37__ca38   = 2200
  integer, parameter :: k_he4_ca37__ti41   = 2201
  integer, parameter :: k_n_ca38__ca39   = 2202
  integer, parameter :: k_he4_ca38__ti42   = 2203
  integer, parameter :: k_n_ca39__ca40   = 2204
  integer, parameter :: k_p_ca39__sc40   = 2205
  integer, parameter :: k_he4_ca39__ti43   = 2206
  integer, parameter :: k_n_ca40__ca41   = 2207
  integer, parameter :: k_p_ca40__sc41   = 2208
  integer, parameter :: k_he4_ca40__ti44   = 2209
  integer, parameter :: k_n_ca41__ca42   = 2210
  integer, parameter :: k_p_ca41__sc42   = 2211
  integer, parameter :: k_he4_ca41__ti45   = 2212
  integer, parameter :: k_n_ca42__ca43   = 2213
  integer, parameter :: k_p_ca42__sc43   = 2214
  integer, parameter :: k_he4_ca42__ti46   = 2215
  integer, parameter :: k_n_ca43__ca44   = 2216
  integer, parameter :: k_p_ca43__sc44   = 2217
  integer, parameter :: k_he4_ca43__ti47   = 2218
  integer, parameter :: k_n_ca44__ca45   = 2219
  integer, parameter :: k_p_ca44__sc45   = 2220
  integer, parameter :: k_he4_ca44__ti48   = 2221
  integer, parameter :: k_n_ca45__ca46   = 2222
  integer, parameter :: k_p_ca45__sc46   = 2223
  integer, parameter :: k_he4_ca45__ti49   = 2224
  integer, parameter :: k_n_ca46__ca47   = 2225
  integer, parameter :: k_p_ca46__sc47   = 2226
  integer, parameter :: k_he4_ca46__ti50   = 2227
  integer, parameter :: k_n_ca47__ca48   = 2228
  integer, parameter :: k_p_ca47__sc48   = 2229
  integer, parameter :: k_he4_ca47__ti51   = 2230
  integer, parameter :: k_n_ca48__ca49   = 2231
  integer, parameter :: k_p_ca48__sc49   = 2232
  integer, parameter :: k_he4_ca48__ti52   = 2233
  integer, parameter :: k_p_ca49__sc50   = 2234
  integer, parameter :: k_he4_ca49__ti53   = 2235
  integer, parameter :: k_n_sc40__sc41   = 2236
  integer, parameter :: k_p_sc40__ti41   = 2237
  integer, parameter :: k_he4_sc40__v44   = 2238
  integer, parameter :: k_n_sc41__sc42   = 2239
  integer, parameter :: k_p_sc41__ti42   = 2240
  integer, parameter :: k_he4_sc41__v45   = 2241
  integer, parameter :: k_n_sc42__sc43   = 2242
  integer, parameter :: k_p_sc42__ti43   = 2243
  integer, parameter :: k_he4_sc42__v46   = 2244
  integer, parameter :: k_n_sc43__sc44   = 2245
  integer, parameter :: k_p_sc43__ti44   = 2246
  integer, parameter :: k_he4_sc43__v47   = 2247
  integer, parameter :: k_n_sc44__sc45   = 2248
  integer, parameter :: k_p_sc44__ti45   = 2249
  integer, parameter :: k_he4_sc44__v48   = 2250
  integer, parameter :: k_n_sc45__sc46   = 2251
  integer, parameter :: k_p_sc45__ti46   = 2252
  integer, parameter :: k_he4_sc45__v49   = 2253
  integer, parameter :: k_n_sc46__sc47   = 2254
  integer, parameter :: k_p_sc46__ti47   = 2255
  integer, parameter :: k_he4_sc46__v50   = 2256
  integer, parameter :: k_n_sc47__sc48   = 2257
  integer, parameter :: k_p_sc47__ti48   = 2258
  integer, parameter :: k_he4_sc47__v51   = 2259
  integer, parameter :: k_n_sc48__sc49   = 2260
  integer, parameter :: k_p_sc48__ti49   = 2261
  integer, parameter :: k_he4_sc48__v52   = 2262
  integer, parameter :: k_n_sc49__sc50   = 2263
  integer, parameter :: k_p_sc49__ti50   = 2264
  integer, parameter :: k_he4_sc49__v53   = 2265
  integer, parameter :: k_n_sc50__sc51   = 2266
  integer, parameter :: k_p_sc50__ti51   = 2267
  integer, parameter :: k_he4_sc50__v54   = 2268
  integer, parameter :: k_p_sc51__ti52   = 2269
  integer, parameter :: k_he4_sc51__v55   = 2270
  integer, parameter :: k_n_ti41__ti42   = 2271
  integer, parameter :: k_he4_ti41__cr45   = 2272
  integer, parameter :: k_n_ti42__ti43   = 2273
  integer, parameter :: k_p_ti42__v43   = 2274
  integer, parameter :: k_he4_ti42__cr46   = 2275
  integer, parameter :: k_n_ti43__ti44   = 2276
  integer, parameter :: k_p_ti43__v44   = 2277
  integer, parameter :: k_he4_ti43__cr47   = 2278
  integer, parameter :: k_n_ti44__ti45   = 2279
  integer, parameter :: k_p_ti44__v45   = 2280
  integer, parameter :: k_he4_ti44__cr48   = 2281
  integer, parameter :: k_n_ti45__ti46   = 2282
  integer, parameter :: k_p_ti45__v46   = 2283
  integer, parameter :: k_he4_ti45__cr49   = 2284
  integer, parameter :: k_n_ti46__ti47   = 2285
  integer, parameter :: k_p_ti46__v47   = 2286
  integer, parameter :: k_he4_ti46__cr50   = 2287
  integer, parameter :: k_n_ti47__ti48   = 2288
  integer, parameter :: k_p_ti47__v48   = 2289
  integer, parameter :: k_he4_ti47__cr51   = 2290
  integer, parameter :: k_n_ti48__ti49   = 2291
  integer, parameter :: k_p_ti48__v49   = 2292
  integer, parameter :: k_he4_ti48__cr52   = 2293
  integer, parameter :: k_n_ti49__ti50   = 2294
  integer, parameter :: k_p_ti49__v50   = 2295
  integer, parameter :: k_he4_ti49__cr53   = 2296
  integer, parameter :: k_n_ti50__ti51   = 2297
  integer, parameter :: k_p_ti50__v51   = 2298
  integer, parameter :: k_he4_ti50__cr54   = 2299
  integer, parameter :: k_n_ti51__ti52   = 2300
  integer, parameter :: k_p_ti51__v52   = 2301
  integer, parameter :: k_he4_ti51__cr55   = 2302
  integer, parameter :: k_n_ti52__ti53   = 2303
  integer, parameter :: k_p_ti52__v53   = 2304
  integer, parameter :: k_he4_ti52__cr56   = 2305
  integer, parameter :: k_p_ti53__v54   = 2306
  integer, parameter :: k_he4_ti53__cr57   = 2307
  integer, parameter :: k_n_v43__v44   = 2308
  integer, parameter :: k_p_v43__cr44   = 2309
  integer, parameter :: k_he4_v43__mn47   = 2310
  integer, parameter :: k_n_v44__v45   = 2311
  integer, parameter :: k_p_v44__cr45   = 2312
  integer, parameter :: k_he4_v44__mn48   = 2313
  integer, parameter :: k_n_v45__v46   = 2314
  integer, parameter :: k_p_v45__cr46   = 2315
  integer, parameter :: k_he4_v45__mn49   = 2316
  integer, parameter :: k_n_v46__v47   = 2317
  integer, parameter :: k_p_v46__cr47   = 2318
  integer, parameter :: k_he4_v46__mn50   = 2319
  integer, parameter :: k_n_v47__v48   = 2320
  integer, parameter :: k_p_v47__cr48   = 2321
  integer, parameter :: k_he4_v47__mn51   = 2322
  integer, parameter :: k_n_v48__v49   = 2323
  integer, parameter :: k_p_v48__cr49   = 2324
  integer, parameter :: k_he4_v48__mn52   = 2325
  integer, parameter :: k_n_v49__v50   = 2326
  integer, parameter :: k_p_v49__cr50   = 2327
  integer, parameter :: k_he4_v49__mn53   = 2328
  integer, parameter :: k_n_v50__v51   = 2329
  integer, parameter :: k_p_v50__cr51   = 2330
  integer, parameter :: k_he4_v50__mn54   = 2331
  integer, parameter :: k_n_v51__v52   = 2332
  integer, parameter :: k_p_v51__cr52   = 2333
  integer, parameter :: k_he4_v51__mn55   = 2334
  integer, parameter :: k_n_v52__v53   = 2335
  integer, parameter :: k_p_v52__cr53   = 2336
  integer, parameter :: k_he4_v52__mn56   = 2337
  integer, parameter :: k_n_v53__v54   = 2338
  integer, parameter :: k_p_v53__cr54   = 2339
  integer, parameter :: k_he4_v53__mn57   = 2340
  integer, parameter :: k_n_v54__v55   = 2341
  integer, parameter :: k_p_v54__cr55   = 2342
  integer, parameter :: k_he4_v54__mn58   = 2343
  integer, parameter :: k_p_v55__cr56   = 2344
  integer, parameter :: k_he4_v55__mn59   = 2345
  integer, parameter :: k_n_cr44__cr45   = 2346
  integer, parameter :: k_he4_cr44__fe48   = 2347
  integer, parameter :: k_n_cr45__cr46   = 2348
  integer, parameter :: k_p_cr45__mn46   = 2349
  integer, parameter :: k_he4_cr45__fe49   = 2350
  integer, parameter :: k_n_cr46__cr47   = 2351
  integer, parameter :: k_p_cr46__mn47   = 2352
  integer, parameter :: k_he4_cr46__fe50   = 2353
  integer, parameter :: k_n_cr47__cr48   = 2354
  integer, parameter :: k_p_cr47__mn48   = 2355
  integer, parameter :: k_he4_cr47__fe51   = 2356
  integer, parameter :: k_n_cr48__cr49   = 2357
  integer, parameter :: k_p_cr48__mn49   = 2358
  integer, parameter :: k_he4_cr48__fe52   = 2359
  integer, parameter :: k_n_cr49__cr50   = 2360
  integer, parameter :: k_p_cr49__mn50   = 2361
  integer, parameter :: k_he4_cr49__fe53   = 2362
  integer, parameter :: k_n_cr50__cr51   = 2363
  integer, parameter :: k_p_cr50__mn51   = 2364
  integer, parameter :: k_he4_cr50__fe54   = 2365
  integer, parameter :: k_n_cr51__cr52   = 2366
  integer, parameter :: k_p_cr51__mn52   = 2367
  integer, parameter :: k_he4_cr51__fe55   = 2368
  integer, parameter :: k_n_cr52__cr53   = 2369
  integer, parameter :: k_p_cr52__mn53   = 2370
  integer, parameter :: k_he4_cr52__fe56   = 2371
  integer, parameter :: k_n_cr53__cr54   = 2372
  integer, parameter :: k_p_cr53__mn54   = 2373
  integer, parameter :: k_he4_cr53__fe57   = 2374
  integer, parameter :: k_n_cr54__cr55   = 2375
  integer, parameter :: k_p_cr54__mn55   = 2376
  integer, parameter :: k_he4_cr54__fe58   = 2377
  integer, parameter :: k_n_cr55__cr56   = 2378
  integer, parameter :: k_p_cr55__mn56   = 2379
  integer, parameter :: k_he4_cr55__fe59   = 2380
  integer, parameter :: k_n_cr56__cr57   = 2381
  integer, parameter :: k_p_cr56__mn57   = 2382
  integer, parameter :: k_he4_cr56__fe60   = 2383
  integer, parameter :: k_n_cr57__cr58   = 2384
  integer, parameter :: k_p_cr57__mn58   = 2385
  integer, parameter :: k_he4_cr57__fe61   = 2386
  integer, parameter :: k_p_cr58__mn59   = 2387
  integer, parameter :: k_he4_cr58__fe62   = 2388
  integer, parameter :: k_n_mn46__mn47   = 2389
  integer, parameter :: k_p_mn46__fe47   = 2390
  integer, parameter :: k_he4_mn46__co50   = 2391
  integer, parameter :: k_n_mn47__mn48   = 2392
  integer, parameter :: k_p_mn47__fe48   = 2393
  integer, parameter :: k_he4_mn47__co51   = 2394
  integer, parameter :: k_n_mn48__mn49   = 2395
  integer, parameter :: k_p_mn48__fe49   = 2396
  integer, parameter :: k_he4_mn48__co52   = 2397
  integer, parameter :: k_n_mn49__mn50   = 2398
  integer, parameter :: k_p_mn49__fe50   = 2399
  integer, parameter :: k_he4_mn49__co53   = 2400
  integer, parameter :: k_n_mn50__mn51   = 2401
  integer, parameter :: k_p_mn50__fe51   = 2402
  integer, parameter :: k_he4_mn50__co54   = 2403
  integer, parameter :: k_n_mn51__mn52   = 2404
  integer, parameter :: k_p_mn51__fe52   = 2405
  integer, parameter :: k_he4_mn51__co55   = 2406
  integer, parameter :: k_n_mn52__mn53   = 2407
  integer, parameter :: k_p_mn52__fe53   = 2408
  integer, parameter :: k_he4_mn52__co56   = 2409
  integer, parameter :: k_n_mn53__mn54   = 2410
  integer, parameter :: k_p_mn53__fe54   = 2411
  integer, parameter :: k_he4_mn53__co57   = 2412
  integer, parameter :: k_n_mn54__mn55   = 2413
  integer, parameter :: k_p_mn54__fe55   = 2414
  integer, parameter :: k_he4_mn54__co58   = 2415
  integer, parameter :: k_n_mn55__mn56   = 2416
  integer, parameter :: k_p_mn55__fe56   = 2417
  integer, parameter :: k_he4_mn55__co59   = 2418
  integer, parameter :: k_n_mn56__mn57   = 2419
  integer, parameter :: k_p_mn56__fe57   = 2420
  integer, parameter :: k_he4_mn56__co60   = 2421
  integer, parameter :: k_n_mn57__mn58   = 2422
  integer, parameter :: k_p_mn57__fe58   = 2423
  integer, parameter :: k_he4_mn57__co61   = 2424
  integer, parameter :: k_n_mn58__mn59   = 2425
  integer, parameter :: k_p_mn58__fe59   = 2426
  integer, parameter :: k_he4_mn58__co62   = 2427
  integer, parameter :: k_n_mn59__mn60   = 2428
  integer, parameter :: k_p_mn59__fe60   = 2429
  integer, parameter :: k_he4_mn59__co63   = 2430
  integer, parameter :: k_n_mn60__mn61   = 2431
  integer, parameter :: k_p_mn60__fe61   = 2432
  integer, parameter :: k_he4_mn60__co64   = 2433
  integer, parameter :: k_p_mn61__fe62   = 2434
  integer, parameter :: k_he4_mn61__co65   = 2435
  integer, parameter :: k_n_fe47__fe48   = 2436
  integer, parameter :: k_he4_fe47__ni51   = 2437
  integer, parameter :: k_n_fe48__fe49   = 2438
  integer, parameter :: k_he4_fe48__ni52   = 2439
  integer, parameter :: k_n_fe49__fe50   = 2440
  integer, parameter :: k_p_fe49__co50   = 2441
  integer, parameter :: k_he4_fe49__ni53   = 2442
  integer, parameter :: k_n_fe50__fe51   = 2443
  integer, parameter :: k_p_fe50__co51   = 2444
  integer, parameter :: k_he4_fe50__ni54   = 2445
  integer, parameter :: k_n_fe51__fe52   = 2446
  integer, parameter :: k_p_fe51__co52   = 2447
  integer, parameter :: k_he4_fe51__ni55   = 2448
  integer, parameter :: k_n_fe52__fe53   = 2449
  integer, parameter :: k_p_fe52__co53   = 2450
  integer, parameter :: k_he4_fe52__ni56   = 2451
  integer, parameter :: k_n_fe53__fe54   = 2452
  integer, parameter :: k_p_fe53__co54   = 2453
  integer, parameter :: k_he4_fe53__ni57   = 2454
  integer, parameter :: k_n_fe54__fe55   = 2455
  integer, parameter :: k_p_fe54__co55   = 2456
  integer, parameter :: k_he4_fe54__ni58   = 2457
  integer, parameter :: k_n_fe55__fe56   = 2458
  integer, parameter :: k_p_fe55__co56   = 2459
  integer, parameter :: k_he4_fe55__ni59   = 2460
  integer, parameter :: k_n_fe56__fe57   = 2461
  integer, parameter :: k_p_fe56__co57   = 2462
  integer, parameter :: k_he4_fe56__ni60   = 2463
  integer, parameter :: k_n_fe57__fe58   = 2464
  integer, parameter :: k_p_fe57__co58   = 2465
  integer, parameter :: k_he4_fe57__ni61   = 2466
  integer, parameter :: k_n_fe58__fe59   = 2467
  integer, parameter :: k_p_fe58__co59   = 2468
  integer, parameter :: k_he4_fe58__ni62   = 2469
  integer, parameter :: k_n_fe59__fe60   = 2470
  integer, parameter :: k_p_fe59__co60   = 2471
  integer, parameter :: k_he4_fe59__ni63   = 2472
  integer, parameter :: k_n_fe60__fe61   = 2473
  integer, parameter :: k_p_fe60__co61   = 2474
  integer, parameter :: k_he4_fe60__ni64   = 2475
  integer, parameter :: k_n_fe61__fe62   = 2476
  integer, parameter :: k_p_fe61__co62   = 2477
  integer, parameter :: k_he4_fe61__ni65   = 2478
  integer, parameter :: k_n_fe62__fe63   = 2479
  integer, parameter :: k_p_fe62__co63   = 2480
  integer, parameter :: k_he4_fe62__ni66   = 2481
  integer, parameter :: k_n_fe63__fe64   = 2482
  integer, parameter :: k_p_fe63__co64   = 2483
  integer, parameter :: k_he4_fe63__ni67   = 2484
  integer, parameter :: k_n_fe64__fe65   = 2485
  integer, parameter :: k_p_fe64__co65   = 2486
  integer, parameter :: k_he4_fe64__ni68   = 2487
  integer, parameter :: k_n_fe65__fe66   = 2488
  integer, parameter :: k_p_fe65__co66   = 2489
  integer, parameter :: k_p_fe66__co67   = 2490
  integer, parameter :: k_n_co50__co51   = 2491
  integer, parameter :: k_p_co50__ni51   = 2492
  integer, parameter :: k_n_co51__co52   = 2493
  integer, parameter :: k_p_co51__ni52   = 2494
  integer, parameter :: k_he4_co51__cu55   = 2495
  integer, parameter :: k_n_co52__co53   = 2496
  integer, parameter :: k_p_co52__ni53   = 2497
  integer, parameter :: k_he4_co52__cu56   = 2498
  integer, parameter :: k_n_co53__co54   = 2499
  integer, parameter :: k_p_co53__ni54   = 2500
  integer, parameter :: k_he4_co53__cu57   = 2501
  integer, parameter :: k_n_co54__co55   = 2502
  integer, parameter :: k_p_co54__ni55   = 2503
  integer, parameter :: k_he4_co54__cu58   = 2504
  integer, parameter :: k_n_co55__co56   = 2505
  integer, parameter :: k_p_co55__ni56   = 2506
  integer, parameter :: k_he4_co55__cu59   = 2507
  integer, parameter :: k_n_co56__co57   = 2508
  integer, parameter :: k_p_co56__ni57   = 2509
  integer, parameter :: k_he4_co56__cu60   = 2510
  integer, parameter :: k_n_co57__co58   = 2511
  integer, parameter :: k_p_co57__ni58   = 2512
  integer, parameter :: k_he4_co57__cu61   = 2513
  integer, parameter :: k_n_co58__co59   = 2514
  integer, parameter :: k_p_co58__ni59   = 2515
  integer, parameter :: k_he4_co58__cu62   = 2516
  integer, parameter :: k_n_co59__co60   = 2517
  integer, parameter :: k_p_co59__ni60   = 2518
  integer, parameter :: k_he4_co59__cu63   = 2519
  integer, parameter :: k_n_co60__co61   = 2520
  integer, parameter :: k_p_co60__ni61   = 2521
  integer, parameter :: k_he4_co60__cu64   = 2522
  integer, parameter :: k_n_co61__co62   = 2523
  integer, parameter :: k_p_co61__ni62   = 2524
  integer, parameter :: k_he4_co61__cu65   = 2525
  integer, parameter :: k_n_co62__co63   = 2526
  integer, parameter :: k_p_co62__ni63   = 2527
  integer, parameter :: k_he4_co62__cu66   = 2528
  integer, parameter :: k_n_co63__co64   = 2529
  integer, parameter :: k_p_co63__ni64   = 2530
  integer, parameter :: k_he4_co63__cu67   = 2531
  integer, parameter :: k_n_co64__co65   = 2532
  integer, parameter :: k_p_co64__ni65   = 2533
  integer, parameter :: k_he4_co64__cu68   = 2534
  integer, parameter :: k_n_co65__co66   = 2535
  integer, parameter :: k_p_co65__ni66   = 2536
  integer, parameter :: k_he4_co65__cu69   = 2537
  integer, parameter :: k_n_co66__co67   = 2538
  integer, parameter :: k_p_co66__ni67   = 2539
  integer, parameter :: k_p_co67__ni68   = 2540
  integer, parameter :: k_n_ni51__ni52   = 2541
  integer, parameter :: k_n_ni52__ni53   = 2542
  integer, parameter :: k_n_ni53__ni54   = 2543
  integer, parameter :: k_he4_ni53__zn57   = 2544
  integer, parameter :: k_n_ni54__ni55   = 2545
  integer, parameter :: k_p_ni54__cu55   = 2546
  integer, parameter :: k_he4_ni54__zn58   = 2547
  integer, parameter :: k_n_ni55__ni56   = 2548
  integer, parameter :: k_p_ni55__cu56   = 2549
  integer, parameter :: k_he4_ni55__zn59   = 2550
  integer, parameter :: k_n_ni56__ni57   = 2551
  integer, parameter :: k_p_ni56__cu57   = 2552
  integer, parameter :: k_he4_ni56__zn60   = 2553
  integer, parameter :: k_n_ni57__ni58   = 2554
  integer, parameter :: k_p_ni57__cu58   = 2555
  integer, parameter :: k_he4_ni57__zn61   = 2556
  integer, parameter :: k_n_ni58__ni59   = 2557
  integer, parameter :: k_p_ni58__cu59   = 2558
  integer, parameter :: k_he4_ni58__zn62   = 2559
  integer, parameter :: k_n_ni59__ni60   = 2560
  integer, parameter :: k_p_ni59__cu60   = 2561
  integer, parameter :: k_he4_ni59__zn63   = 2562
  integer, parameter :: k_n_ni60__ni61   = 2563
  integer, parameter :: k_p_ni60__cu61   = 2564
  integer, parameter :: k_he4_ni60__zn64   = 2565
  integer, parameter :: k_n_ni61__ni62   = 2566
  integer, parameter :: k_p_ni61__cu62   = 2567
  integer, parameter :: k_he4_ni61__zn65   = 2568
  integer, parameter :: k_n_ni62__ni63   = 2569
  integer, parameter :: k_p_ni62__cu63   = 2570
  integer, parameter :: k_he4_ni62__zn66   = 2571
  integer, parameter :: k_n_ni63__ni64   = 2572
  integer, parameter :: k_p_ni63__cu64   = 2573
  integer, parameter :: k_he4_ni63__zn67   = 2574
  integer, parameter :: k_n_ni64__ni65   = 2575
  integer, parameter :: k_p_ni64__cu65   = 2576
  integer, parameter :: k_he4_ni64__zn68   = 2577
  integer, parameter :: k_n_ni65__ni66   = 2578
  integer, parameter :: k_p_ni65__cu66   = 2579
  integer, parameter :: k_he4_ni65__zn69   = 2580
  integer, parameter :: k_n_ni66__ni67   = 2581
  integer, parameter :: k_p_ni66__cu67   = 2582
  integer, parameter :: k_he4_ni66__zn70   = 2583
  integer, parameter :: k_n_ni67__ni68   = 2584
  integer, parameter :: k_p_ni67__cu68   = 2585
  integer, parameter :: k_he4_ni67__zn71   = 2586
  integer, parameter :: k_p_ni68__cu69   = 2587
  integer, parameter :: k_he4_ni68__zn72   = 2588
  integer, parameter :: k_n_cu55__cu56   = 2589
  integer, parameter :: k_he4_cu55__ga59   = 2590
  integer, parameter :: k_n_cu56__cu57   = 2591
  integer, parameter :: k_p_cu56__zn57   = 2592
  integer, parameter :: k_he4_cu56__ga60   = 2593
  integer, parameter :: k_n_cu57__cu58   = 2594
  integer, parameter :: k_p_cu57__zn58   = 2595
  integer, parameter :: k_he4_cu57__ga61   = 2596
  integer, parameter :: k_n_cu58__cu59   = 2597
  integer, parameter :: k_p_cu58__zn59   = 2598
  integer, parameter :: k_he4_cu58__ga62   = 2599
  integer, parameter :: k_n_cu59__cu60   = 2600
  integer, parameter :: k_p_cu59__zn60   = 2601
  integer, parameter :: k_he4_cu59__ga63   = 2602
  integer, parameter :: k_n_cu60__cu61   = 2603
  integer, parameter :: k_p_cu60__zn61   = 2604
  integer, parameter :: k_he4_cu60__ga64   = 2605
  integer, parameter :: k_n_cu61__cu62   = 2606
  integer, parameter :: k_p_cu61__zn62   = 2607
  integer, parameter :: k_he4_cu61__ga65   = 2608
  integer, parameter :: k_n_cu62__cu63   = 2609
  integer, parameter :: k_p_cu62__zn63   = 2610
  integer, parameter :: k_he4_cu62__ga66   = 2611
  integer, parameter :: k_n_cu63__cu64   = 2612
  integer, parameter :: k_p_cu63__zn64   = 2613
  integer, parameter :: k_he4_cu63__ga67   = 2614
  integer, parameter :: k_n_cu64__cu65   = 2615
  integer, parameter :: k_p_cu64__zn65   = 2616
  integer, parameter :: k_he4_cu64__ga68   = 2617
  integer, parameter :: k_n_cu65__cu66   = 2618
  integer, parameter :: k_p_cu65__zn66   = 2619
  integer, parameter :: k_he4_cu65__ga69   = 2620
  integer, parameter :: k_n_cu66__cu67   = 2621
  integer, parameter :: k_p_cu66__zn67   = 2622
  integer, parameter :: k_he4_cu66__ga70   = 2623
  integer, parameter :: k_n_cu67__cu68   = 2624
  integer, parameter :: k_p_cu67__zn68   = 2625
  integer, parameter :: k_he4_cu67__ga71   = 2626
  integer, parameter :: k_n_cu68__cu69   = 2627
  integer, parameter :: k_p_cu68__zn69   = 2628
  integer, parameter :: k_he4_cu68__ga72   = 2629
  integer, parameter :: k_p_cu69__zn70   = 2630
  integer, parameter :: k_he4_cu69__ga73   = 2631
  integer, parameter :: k_n_zn57__zn58   = 2632
  integer, parameter :: k_n_zn58__zn59   = 2633
  integer, parameter :: k_p_zn58__ga59   = 2634
  integer, parameter :: k_he4_zn58__ge62   = 2635
  integer, parameter :: k_n_zn59__zn60   = 2636
  integer, parameter :: k_p_zn59__ga60   = 2637
  integer, parameter :: k_he4_zn59__ge63   = 2638
  integer, parameter :: k_n_zn60__zn61   = 2639
  integer, parameter :: k_p_zn60__ga61   = 2640
  integer, parameter :: k_he4_zn60__ge64   = 2641
  integer, parameter :: k_n_zn61__zn62   = 2642
  integer, parameter :: k_p_zn61__ga62   = 2643
  integer, parameter :: k_he4_zn61__ge65   = 2644
  integer, parameter :: k_n_zn62__zn63   = 2645
  integer, parameter :: k_p_zn62__ga63   = 2646
  integer, parameter :: k_he4_zn62__ge66   = 2647
  integer, parameter :: k_n_zn63__zn64   = 2648
  integer, parameter :: k_p_zn63__ga64   = 2649
  integer, parameter :: k_he4_zn63__ge67   = 2650
  integer, parameter :: k_n_zn64__zn65   = 2651
  integer, parameter :: k_p_zn64__ga65   = 2652
  integer, parameter :: k_he4_zn64__ge68   = 2653
  integer, parameter :: k_n_zn65__zn66   = 2654
  integer, parameter :: k_p_zn65__ga66   = 2655
  integer, parameter :: k_he4_zn65__ge69   = 2656
  integer, parameter :: k_n_zn66__zn67   = 2657
  integer, parameter :: k_p_zn66__ga67   = 2658
  integer, parameter :: k_he4_zn66__ge70   = 2659
  integer, parameter :: k_n_zn67__zn68   = 2660
  integer, parameter :: k_p_zn67__ga68   = 2661
  integer, parameter :: k_he4_zn67__ge71   = 2662
  integer, parameter :: k_n_zn68__zn69   = 2663
  integer, parameter :: k_p_zn68__ga69   = 2664
  integer, parameter :: k_he4_zn68__ge72   = 2665
  integer, parameter :: k_n_zn69__zn70   = 2666
  integer, parameter :: k_p_zn69__ga70   = 2667
  integer, parameter :: k_he4_zn69__ge73   = 2668
  integer, parameter :: k_n_zn70__zn71   = 2669
  integer, parameter :: k_p_zn70__ga71   = 2670
  integer, parameter :: k_he4_zn70__ge74   = 2671
  integer, parameter :: k_n_zn71__zn72   = 2672
  integer, parameter :: k_p_zn71__ga72   = 2673
  integer, parameter :: k_he4_zn71__ge75   = 2674
  integer, parameter :: k_p_zn72__ga73   = 2675
  integer, parameter :: k_he4_zn72__ge76   = 2676
  integer, parameter :: k_n_ga59__ga60   = 2677
  integer, parameter :: k_n_ga60__ga61   = 2678
  integer, parameter :: k_n_ga61__ga62   = 2679
  integer, parameter :: k_p_ga61__ge62   = 2680
  integer, parameter :: k_he4_ga61__as65   = 2681
  integer, parameter :: k_n_ga62__ga63   = 2682
  integer, parameter :: k_p_ga62__ge63   = 2683
  integer, parameter :: k_he4_ga62__as66   = 2684
  integer, parameter :: k_n_ga63__ga64   = 2685
  integer, parameter :: k_p_ga63__ge64   = 2686
  integer, parameter :: k_he4_ga63__as67   = 2687
  integer, parameter :: k_n_ga64__ga65   = 2688
  integer, parameter :: k_p_ga64__ge65   = 2689
  integer, parameter :: k_he4_ga64__as68   = 2690
  integer, parameter :: k_n_ga65__ga66   = 2691
  integer, parameter :: k_p_ga65__ge66   = 2692
  integer, parameter :: k_he4_ga65__as69   = 2693
  integer, parameter :: k_n_ga66__ga67   = 2694
  integer, parameter :: k_p_ga66__ge67   = 2695
  integer, parameter :: k_he4_ga66__as70   = 2696
  integer, parameter :: k_n_ga67__ga68   = 2697
  integer, parameter :: k_p_ga67__ge68   = 2698
  integer, parameter :: k_he4_ga67__as71   = 2699
  integer, parameter :: k_n_ga68__ga69   = 2700
  integer, parameter :: k_p_ga68__ge69   = 2701
  integer, parameter :: k_he4_ga68__as72   = 2702
  integer, parameter :: k_n_ga69__ga70   = 2703
  integer, parameter :: k_p_ga69__ge70   = 2704
  integer, parameter :: k_he4_ga69__as73   = 2705
  integer, parameter :: k_n_ga70__ga71   = 2706
  integer, parameter :: k_p_ga70__ge71   = 2707
  integer, parameter :: k_he4_ga70__as74   = 2708
  integer, parameter :: k_n_ga71__ga72   = 2709
  integer, parameter :: k_p_ga71__ge72   = 2710
  integer, parameter :: k_he4_ga71__as75   = 2711
  integer, parameter :: k_n_ga72__ga73   = 2712
  integer, parameter :: k_p_ga72__ge73   = 2713
  integer, parameter :: k_he4_ga72__as76   = 2714
  integer, parameter :: k_n_ga73__ga74   = 2715
  integer, parameter :: k_p_ga73__ge74   = 2716
  integer, parameter :: k_he4_ga73__as77   = 2717
  integer, parameter :: k_n_ga74__ga75   = 2718
  integer, parameter :: k_p_ga74__ge75   = 2719
  integer, parameter :: k_he4_ga74__as78   = 2720
  integer, parameter :: k_p_ga75__ge76   = 2721
  integer, parameter :: k_he4_ga75__as79   = 2722
  integer, parameter :: k_n_ge62__ge63   = 2723
  integer, parameter :: k_n_ge63__ge64   = 2724
  integer, parameter :: k_he4_ge63__se67   = 2725
  integer, parameter :: k_n_ge64__ge65   = 2726
  integer, parameter :: k_p_ge64__as65   = 2727
  integer, parameter :: k_he4_ge64__se68   = 2728
  integer, parameter :: k_n_ge65__ge66   = 2729
  integer, parameter :: k_p_ge65__as66   = 2730
  integer, parameter :: k_he4_ge65__se69   = 2731
  integer, parameter :: k_n_ge66__ge67   = 2732
  integer, parameter :: k_p_ge66__as67   = 2733
  integer, parameter :: k_he4_ge66__se70   = 2734
  integer, parameter :: k_n_ge67__ge68   = 2735
  integer, parameter :: k_p_ge67__as68   = 2736
  integer, parameter :: k_he4_ge67__se71   = 2737
  integer, parameter :: k_n_ge68__ge69   = 2738
  integer, parameter :: k_p_ge68__as69   = 2739
  integer, parameter :: k_he4_ge68__se72   = 2740
  integer, parameter :: k_n_ge69__ge70   = 2741
  integer, parameter :: k_p_ge69__as70   = 2742
  integer, parameter :: k_he4_ge69__se73   = 2743
  integer, parameter :: k_n_ge70__ge71   = 2744
  integer, parameter :: k_p_ge70__as71   = 2745
  integer, parameter :: k_he4_ge70__se74   = 2746
  integer, parameter :: k_n_ge71__ge72   = 2747
  integer, parameter :: k_p_ge71__as72   = 2748
  integer, parameter :: k_he4_ge71__se75   = 2749
  integer, parameter :: k_n_ge72__ge73   = 2750
  integer, parameter :: k_p_ge72__as73   = 2751
  integer, parameter :: k_he4_ge72__se76   = 2752
  integer, parameter :: k_n_ge73__ge74   = 2753
  integer, parameter :: k_p_ge73__as74   = 2754
  integer, parameter :: k_he4_ge73__se77   = 2755
  integer, parameter :: k_n_ge74__ge75   = 2756
  integer, parameter :: k_p_ge74__as75   = 2757
  integer, parameter :: k_he4_ge74__se78   = 2758
  integer, parameter :: k_n_ge75__ge76   = 2759
  integer, parameter :: k_p_ge75__as76   = 2760
  integer, parameter :: k_he4_ge75__se79   = 2761
  integer, parameter :: k_n_ge76__ge77   = 2762
  integer, parameter :: k_p_ge76__as77   = 2763
  integer, parameter :: k_he4_ge76__se80   = 2764
  integer, parameter :: k_n_ge77__ge78   = 2765
  integer, parameter :: k_p_ge77__as78   = 2766
  integer, parameter :: k_he4_ge77__se81   = 2767
  integer, parameter :: k_p_ge78__as79   = 2768
  integer, parameter :: k_he4_ge78__se82   = 2769
  integer, parameter :: k_n_as65__as66   = 2770
  integer, parameter :: k_he4_as65__br69   = 2771
  integer, parameter :: k_n_as66__as67   = 2772
  integer, parameter :: k_p_as66__se67   = 2773
  integer, parameter :: k_he4_as66__br70   = 2774
  integer, parameter :: k_n_as67__as68   = 2775
  integer, parameter :: k_p_as67__se68   = 2776
  integer, parameter :: k_he4_as67__br71   = 2777
  integer, parameter :: k_n_as68__as69   = 2778
  integer, parameter :: k_p_as68__se69   = 2779
  integer, parameter :: k_he4_as68__br72   = 2780
  integer, parameter :: k_n_as69__as70   = 2781
  integer, parameter :: k_p_as69__se70   = 2782
  integer, parameter :: k_he4_as69__br73   = 2783
  integer, parameter :: k_n_as70__as71   = 2784
  integer, parameter :: k_p_as70__se71   = 2785
  integer, parameter :: k_he4_as70__br74   = 2786
  integer, parameter :: k_n_as71__as72   = 2787
  integer, parameter :: k_p_as71__se72   = 2788
  integer, parameter :: k_he4_as71__br75   = 2789
  integer, parameter :: k_n_as72__as73   = 2790
  integer, parameter :: k_p_as72__se73   = 2791
  integer, parameter :: k_he4_as72__br76   = 2792
  integer, parameter :: k_n_as73__as74   = 2793
  integer, parameter :: k_p_as73__se74   = 2794
  integer, parameter :: k_he4_as73__br77   = 2795
  integer, parameter :: k_n_as74__as75   = 2796
  integer, parameter :: k_p_as74__se75   = 2797
  integer, parameter :: k_he4_as74__br78   = 2798
  integer, parameter :: k_n_as75__as76   = 2799
  integer, parameter :: k_p_as75__se76   = 2800
  integer, parameter :: k_he4_as75__br79   = 2801
  integer, parameter :: k_n_as76__as77   = 2802
  integer, parameter :: k_p_as76__se77   = 2803
  integer, parameter :: k_he4_as76__br80   = 2804
  integer, parameter :: k_n_as77__as78   = 2805
  integer, parameter :: k_p_as77__se78   = 2806
  integer, parameter :: k_he4_as77__br81   = 2807
  integer, parameter :: k_n_as78__as79   = 2808
  integer, parameter :: k_p_as78__se79   = 2809
  integer, parameter :: k_he4_as78__br82   = 2810
  integer, parameter :: k_p_as79__se80   = 2811
  integer, parameter :: k_he4_as79__br83   = 2812
  integer, parameter :: k_n_se67__se68   = 2813
  integer, parameter :: k_p_se67__br68   = 2814
  integer, parameter :: k_he4_se67__kr71   = 2815
  integer, parameter :: k_n_se68__se69   = 2816
  integer, parameter :: k_p_se68__br69   = 2817
  integer, parameter :: k_he4_se68__kr72   = 2818
  integer, parameter :: k_n_se69__se70   = 2819
  integer, parameter :: k_p_se69__br70   = 2820
  integer, parameter :: k_he4_se69__kr73   = 2821
  integer, parameter :: k_n_se70__se71   = 2822
  integer, parameter :: k_p_se70__br71   = 2823
  integer, parameter :: k_he4_se70__kr74   = 2824
  integer, parameter :: k_n_se71__se72   = 2825
  integer, parameter :: k_p_se71__br72   = 2826
  integer, parameter :: k_he4_se71__kr75   = 2827
  integer, parameter :: k_n_se72__se73   = 2828
  integer, parameter :: k_p_se72__br73   = 2829
  integer, parameter :: k_he4_se72__kr76   = 2830
  integer, parameter :: k_n_se73__se74   = 2831
  integer, parameter :: k_p_se73__br74   = 2832
  integer, parameter :: k_he4_se73__kr77   = 2833
  integer, parameter :: k_n_se74__se75   = 2834
  integer, parameter :: k_p_se74__br75   = 2835
  integer, parameter :: k_he4_se74__kr78   = 2836
  integer, parameter :: k_n_se75__se76   = 2837
  integer, parameter :: k_p_se75__br76   = 2838
  integer, parameter :: k_he4_se75__kr79   = 2839
  integer, parameter :: k_n_se76__se77   = 2840
  integer, parameter :: k_p_se76__br77   = 2841
  integer, parameter :: k_he4_se76__kr80   = 2842
  integer, parameter :: k_n_se77__se78   = 2843
  integer, parameter :: k_p_se77__br78   = 2844
  integer, parameter :: k_he4_se77__kr81   = 2845
  integer, parameter :: k_n_se78__se79   = 2846
  integer, parameter :: k_p_se78__br79   = 2847
  integer, parameter :: k_he4_se78__kr82   = 2848
  integer, parameter :: k_n_se79__se80   = 2849
  integer, parameter :: k_p_se79__br80   = 2850
  integer, parameter :: k_he4_se79__kr83   = 2851
  integer, parameter :: k_n_se80__se81   = 2852
  integer, parameter :: k_p_se80__br81   = 2853
  integer, parameter :: k_he4_se80__kr84   = 2854
  integer, parameter :: k_n_se81__se82   = 2855
  integer, parameter :: k_p_se81__br82   = 2856
  integer, parameter :: k_he4_se81__kr85   = 2857
  integer, parameter :: k_n_se82__se83   = 2858
  integer, parameter :: k_p_se82__br83   = 2859
  integer, parameter :: k_he4_se82__kr86   = 2860
  integer, parameter :: k_he4_se83__kr87   = 2861
  integer, parameter :: k_n_br68__br69   = 2862
  integer, parameter :: k_p_br68__kr69   = 2863
  integer, parameter :: k_n_br69__br70   = 2864
  integer, parameter :: k_p_br69__kr70   = 2865
  integer, parameter :: k_he4_br69__rb73   = 2866
  integer, parameter :: k_n_br70__br71   = 2867
  integer, parameter :: k_p_br70__kr71   = 2868
  integer, parameter :: k_he4_br70__rb74   = 2869
  integer, parameter :: k_n_br71__br72   = 2870
  integer, parameter :: k_p_br71__kr72   = 2871
  integer, parameter :: k_he4_br71__rb75   = 2872
  integer, parameter :: k_n_br72__br73   = 2873
  integer, parameter :: k_p_br72__kr73   = 2874
  integer, parameter :: k_he4_br72__rb76   = 2875
  integer, parameter :: k_n_br73__br74   = 2876
  integer, parameter :: k_p_br73__kr74   = 2877
  integer, parameter :: k_he4_br73__rb77   = 2878
  integer, parameter :: k_n_br74__br75   = 2879
  integer, parameter :: k_p_br74__kr75   = 2880
  integer, parameter :: k_he4_br74__rb78   = 2881
  integer, parameter :: k_n_br75__br76   = 2882
  integer, parameter :: k_p_br75__kr76   = 2883
  integer, parameter :: k_he4_br75__rb79   = 2884
  integer, parameter :: k_n_br76__br77   = 2885
  integer, parameter :: k_p_br76__kr77   = 2886
  integer, parameter :: k_he4_br76__rb80   = 2887
  integer, parameter :: k_n_br77__br78   = 2888
  integer, parameter :: k_p_br77__kr78   = 2889
  integer, parameter :: k_he4_br77__rb81   = 2890
  integer, parameter :: k_n_br78__br79   = 2891
  integer, parameter :: k_p_br78__kr79   = 2892
  integer, parameter :: k_he4_br78__rb82   = 2893
  integer, parameter :: k_n_br79__br80   = 2894
  integer, parameter :: k_p_br79__kr80   = 2895
  integer, parameter :: k_he4_br79__rb83   = 2896
  integer, parameter :: k_n_br80__br81   = 2897
  integer, parameter :: k_p_br80__kr81   = 2898
  integer, parameter :: k_he4_br80__rb84   = 2899
  integer, parameter :: k_n_br81__br82   = 2900
  integer, parameter :: k_p_br81__kr82   = 2901
  integer, parameter :: k_he4_br81__rb85   = 2902
  integer, parameter :: k_n_br82__br83   = 2903
  integer, parameter :: k_p_br82__kr83   = 2904
  integer, parameter :: k_p_br83__kr84   = 2905
  integer, parameter :: k_n_kr69__kr70   = 2906
  integer, parameter :: k_n_kr70__kr71   = 2907
  integer, parameter :: k_he4_kr70__sr74   = 2908
  integer, parameter :: k_n_kr71__kr72   = 2909
  integer, parameter :: k_he4_kr71__sr75   = 2910
  integer, parameter :: k_n_kr72__kr73   = 2911
  integer, parameter :: k_p_kr72__rb73   = 2912
  integer, parameter :: k_he4_kr72__sr76   = 2913
  integer, parameter :: k_n_kr73__kr74   = 2914
  integer, parameter :: k_p_kr73__rb74   = 2915
  integer, parameter :: k_he4_kr73__sr77   = 2916
  integer, parameter :: k_n_kr74__kr75   = 2917
  integer, parameter :: k_p_kr74__rb75   = 2918
  integer, parameter :: k_he4_kr74__sr78   = 2919
  integer, parameter :: k_n_kr75__kr76   = 2920
  integer, parameter :: k_p_kr75__rb76   = 2921
  integer, parameter :: k_he4_kr75__sr79   = 2922
  integer, parameter :: k_n_kr76__kr77   = 2923
  integer, parameter :: k_p_kr76__rb77   = 2924
  integer, parameter :: k_he4_kr76__sr80   = 2925
  integer, parameter :: k_n_kr77__kr78   = 2926
  integer, parameter :: k_p_kr77__rb78   = 2927
  integer, parameter :: k_he4_kr77__sr81   = 2928
  integer, parameter :: k_n_kr78__kr79   = 2929
  integer, parameter :: k_p_kr78__rb79   = 2930
  integer, parameter :: k_he4_kr78__sr82   = 2931
  integer, parameter :: k_n_kr79__kr80   = 2932
  integer, parameter :: k_p_kr79__rb80   = 2933
  integer, parameter :: k_he4_kr79__sr83   = 2934
  integer, parameter :: k_n_kr80__kr81   = 2935
  integer, parameter :: k_p_kr80__rb81   = 2936
  integer, parameter :: k_he4_kr80__sr84   = 2937
  integer, parameter :: k_n_kr81__kr82   = 2938
  integer, parameter :: k_p_kr81__rb82   = 2939
  integer, parameter :: k_n_kr82__kr83   = 2940
  integer, parameter :: k_p_kr82__rb83   = 2941
  integer, parameter :: k_n_kr83__kr84   = 2942
  integer, parameter :: k_p_kr83__rb84   = 2943
  integer, parameter :: k_n_kr84__kr85   = 2944
  integer, parameter :: k_p_kr84__rb85   = 2945
  integer, parameter :: k_n_kr85__kr86   = 2946
  integer, parameter :: k_n_kr86__kr87   = 2947
  integer, parameter :: k_n_rb73__rb74   = 2948
  integer, parameter :: k_p_rb73__sr74   = 2949
  integer, parameter :: k_he4_rb73__y77   = 2950
  integer, parameter :: k_n_rb74__rb75   = 2951
  integer, parameter :: k_p_rb74__sr75   = 2952
  integer, parameter :: k_he4_rb74__y78   = 2953
  integer, parameter :: k_n_rb75__rb76   = 2954
  integer, parameter :: k_p_rb75__sr76   = 2955
  integer, parameter :: k_he4_rb75__y79   = 2956
  integer, parameter :: k_n_rb76__rb77   = 2957
  integer, parameter :: k_p_rb76__sr77   = 2958
  integer, parameter :: k_he4_rb76__y80   = 2959
  integer, parameter :: k_n_rb77__rb78   = 2960
  integer, parameter :: k_p_rb77__sr78   = 2961
  integer, parameter :: k_he4_rb77__y81   = 2962
  integer, parameter :: k_n_rb78__rb79   = 2963
  integer, parameter :: k_p_rb78__sr79   = 2964
  integer, parameter :: k_he4_rb78__y82   = 2965
  integer, parameter :: k_n_rb79__rb80   = 2966
  integer, parameter :: k_p_rb79__sr80   = 2967
  integer, parameter :: k_he4_rb79__y83   = 2968
  integer, parameter :: k_n_rb80__rb81   = 2969
  integer, parameter :: k_p_rb80__sr81   = 2970
  integer, parameter :: k_he4_rb80__y84   = 2971
  integer, parameter :: k_n_rb81__rb82   = 2972
  integer, parameter :: k_p_rb81__sr82   = 2973
  integer, parameter :: k_he4_rb81__y85   = 2974
  integer, parameter :: k_n_rb82__rb83   = 2975
  integer, parameter :: k_p_rb82__sr83   = 2976
  integer, parameter :: k_he4_rb82__y86   = 2977
  integer, parameter :: k_n_rb83__rb84   = 2978
  integer, parameter :: k_p_rb83__sr84   = 2979
  integer, parameter :: k_he4_rb83__y87   = 2980
  integer, parameter :: k_n_rb84__rb85   = 2981
  integer, parameter :: k_n_sr74__sr75   = 2982
  integer, parameter :: k_p_sr74__y75   = 2983
  integer, parameter :: k_he4_sr74__zr78   = 2984
  integer, parameter :: k_n_sr75__sr76   = 2985
  integer, parameter :: k_p_sr75__y76   = 2986
  integer, parameter :: k_he4_sr75__zr79   = 2987
  integer, parameter :: k_n_sr76__sr77   = 2988
  integer, parameter :: k_p_sr76__y77   = 2989
  integer, parameter :: k_he4_sr76__zr80   = 2990
  integer, parameter :: k_n_sr77__sr78   = 2991
  integer, parameter :: k_p_sr77__y78   = 2992
  integer, parameter :: k_he4_sr77__zr81   = 2993
  integer, parameter :: k_n_sr78__sr79   = 2994
  integer, parameter :: k_p_sr78__y79   = 2995
  integer, parameter :: k_he4_sr78__zr82   = 2996
  integer, parameter :: k_n_sr79__sr80   = 2997
  integer, parameter :: k_p_sr79__y80   = 2998
  integer, parameter :: k_he4_sr79__zr83   = 2999
  integer, parameter :: k_n_sr80__sr81   = 3000
  integer, parameter :: k_p_sr80__y81   = 3001
  integer, parameter :: k_he4_sr80__zr84   = 3002
  integer, parameter :: k_n_sr81__sr82   = 3003
  integer, parameter :: k_p_sr81__y82   = 3004
  integer, parameter :: k_he4_sr81__zr85   = 3005
  integer, parameter :: k_n_sr82__sr83   = 3006
  integer, parameter :: k_p_sr82__y83   = 3007
  integer, parameter :: k_he4_sr82__zr86   = 3008
  integer, parameter :: k_n_sr83__sr84   = 3009
  integer, parameter :: k_p_sr83__y84   = 3010
  integer, parameter :: k_he4_sr83__zr87   = 3011
  integer, parameter :: k_p_sr84__y85   = 3012
  integer, parameter :: k_he4_sr84__zr88   = 3013
  integer, parameter :: k_n_y75__y76   = 3014
  integer, parameter :: k_n_y76__y77   = 3015
  integer, parameter :: k_n_y77__y78   = 3016
  integer, parameter :: k_p_y77__zr78   = 3017
  integer, parameter :: k_n_y78__y79   = 3018
  integer, parameter :: k_p_y78__zr79   = 3019
  integer, parameter :: k_he4_y78__nb82   = 3020
  integer, parameter :: k_n_y79__y80   = 3021
  integer, parameter :: k_p_y79__zr80   = 3022
  integer, parameter :: k_he4_y79__nb83   = 3023
  integer, parameter :: k_n_y80__y81   = 3024
  integer, parameter :: k_p_y80__zr81   = 3025
  integer, parameter :: k_he4_y80__nb84   = 3026
  integer, parameter :: k_n_y81__y82   = 3027
  integer, parameter :: k_p_y81__zr82   = 3028
  integer, parameter :: k_he4_y81__nb85   = 3029
  integer, parameter :: k_n_y82__y83   = 3030
  integer, parameter :: k_p_y82__zr83   = 3031
  integer, parameter :: k_he4_y82__nb86   = 3032
  integer, parameter :: k_n_y83__y84   = 3033
  integer, parameter :: k_p_y83__zr84   = 3034
  integer, parameter :: k_he4_y83__nb87   = 3035
  integer, parameter :: k_n_y84__y85   = 3036
  integer, parameter :: k_p_y84__zr85   = 3037
  integer, parameter :: k_he4_y84__nb88   = 3038
  integer, parameter :: k_n_y85__y86   = 3039
  integer, parameter :: k_p_y85__zr86   = 3040
  integer, parameter :: k_he4_y85__nb89   = 3041
  integer, parameter :: k_n_y86__y87   = 3042
  integer, parameter :: k_p_y86__zr87   = 3043
  integer, parameter :: k_he4_y86__nb90   = 3044
  integer, parameter :: k_p_y87__zr88   = 3045
  integer, parameter :: k_n_zr78__zr79   = 3046
  integer, parameter :: k_n_zr79__zr80   = 3047
  integer, parameter :: k_he4_zr79__mo83   = 3048
  integer, parameter :: k_n_zr80__zr81   = 3049
  integer, parameter :: k_he4_zr80__mo84   = 3050
  integer, parameter :: k_n_zr81__zr82   = 3051
  integer, parameter :: k_p_zr81__nb82   = 3052
  integer, parameter :: k_he4_zr81__mo85   = 3053
  integer, parameter :: k_n_zr82__zr83   = 3054
  integer, parameter :: k_p_zr82__nb83   = 3055
  integer, parameter :: k_he4_zr82__mo86   = 3056
  integer, parameter :: k_n_zr83__zr84   = 3057
  integer, parameter :: k_p_zr83__nb84   = 3058
  integer, parameter :: k_he4_zr83__mo87   = 3059
  integer, parameter :: k_n_zr84__zr85   = 3060
  integer, parameter :: k_p_zr84__nb85   = 3061
  integer, parameter :: k_he4_zr84__mo88   = 3062
  integer, parameter :: k_n_zr85__zr86   = 3063
  integer, parameter :: k_p_zr85__nb86   = 3064
  integer, parameter :: k_he4_zr85__mo89   = 3065
  integer, parameter :: k_n_zr86__zr87   = 3066
  integer, parameter :: k_p_zr86__nb87   = 3067
  integer, parameter :: k_he4_zr86__mo90   = 3068
  integer, parameter :: k_n_zr87__zr88   = 3069
  integer, parameter :: k_p_zr87__nb88   = 3070
  integer, parameter :: k_n_zr88__zr89   = 3071
  integer, parameter :: k_p_zr88__nb89   = 3072
  integer, parameter :: k_n_zr89__zr90   = 3073
  integer, parameter :: k_p_zr89__nb90   = 3074
  integer, parameter :: k_n_nb82__nb83   = 3075
  integer, parameter :: k_p_nb82__mo83   = 3076
  integer, parameter :: k_n_nb83__nb84   = 3077
  integer, parameter :: k_p_nb83__mo84   = 3078
  integer, parameter :: k_n_nb84__nb85   = 3079
  integer, parameter :: k_p_nb84__mo85   = 3080
  integer, parameter :: k_n_nb85__nb86   = 3081
  integer, parameter :: k_p_nb85__mo86   = 3082
  integer, parameter :: k_he4_nb85__tc89   = 3083
  integer, parameter :: k_n_nb86__nb87   = 3084
  integer, parameter :: k_p_nb86__mo87   = 3085
  integer, parameter :: k_he4_nb86__tc90   = 3086
  integer, parameter :: k_n_nb87__nb88   = 3087
  integer, parameter :: k_p_nb87__mo88   = 3088
  integer, parameter :: k_he4_nb87__tc91   = 3089
  integer, parameter :: k_n_nb88__nb89   = 3090
  integer, parameter :: k_p_nb88__mo89   = 3091
  integer, parameter :: k_n_nb89__nb90   = 3092
  integer, parameter :: k_p_nb89__mo90   = 3093
  integer, parameter :: k_n_mo83__mo84   = 3094
  integer, parameter :: k_n_mo84__mo85   = 3095
  integer, parameter :: k_n_mo85__mo86   = 3096
  integer, parameter :: k_n_mo86__mo87   = 3097
  integer, parameter :: k_n_mo87__mo88   = 3098
  integer, parameter :: k_n_mo88__mo89   = 3099
  integer, parameter :: k_p_mo88__tc89   = 3100
  integer, parameter :: k_n_mo89__mo90   = 3101
  integer, parameter :: k_p_mo89__tc90   = 3102
  integer, parameter :: k_p_mo90__tc91   = 3103
  integer, parameter :: k_n_tc89__tc90   = 3104
  integer, parameter :: k_n_tc90__tc91   = 3105
  integer, parameter :: k_d_d__n_he3   = 3106
  integer, parameter :: k_d_d__p_t   = 3107
  integer, parameter :: k_p_t__n_he3   = 3108
  integer, parameter :: k_p_t__d_d   = 3109
  integer, parameter :: k_d_t__n_he4   = 3110
  integer, parameter :: k_he4_t__n_li6   = 3111
  integer, parameter :: k_n_he3__p_t   = 3112
  integer, parameter :: k_n_he3__d_d   = 3113
  integer, parameter :: k_d_he3__p_he4   = 3114
  integer, parameter :: k_t_he3__d_he4   = 3115
  integer, parameter :: k_he4_he3__p_li6   = 3116
  integer, parameter :: k_n_he4__d_t   = 3117
  integer, parameter :: k_p_he4__d_he3   = 3118
  integer, parameter :: k_d_he4__t_he3   = 3119
  integer, parameter :: k_he4_he4__n_be7   = 3120
  integer, parameter :: k_he4_he4__p_li7   = 3121
  integer, parameter :: k_n_li6__he4_t   = 3122
  integer, parameter :: k_p_li6__he4_he3   = 3123
  integer, parameter :: k_d_li6__n_be7   = 3124
  integer, parameter :: k_d_li6__p_li7   = 3125
  integer, parameter :: k_he4_li6__p_be9   = 3126
  integer, parameter :: k_p_li7__n_be7   = 3127
  integer, parameter :: k_p_li7__d_li6   = 3128
  integer, parameter :: k_p_li7__he4_he4   = 3129
  integer, parameter :: k_t_li7__n_be9   = 3130
  integer, parameter :: k_he4_li7__n_b10   = 3131
  integer, parameter :: k_n_be7__p_li7   = 3132
  integer, parameter :: k_n_be7__d_li6   = 3133
  integer, parameter :: k_n_be7__he4_he4   = 3134
  integer, parameter :: k_he4_be7__p_b10   = 3135
  integer, parameter :: k_n_be9__t_li7   = 3136
  integer, parameter :: k_p_be9__he4_li6   = 3137
  integer, parameter :: k_t_be9__n_b11   = 3138
  integer, parameter :: k_he4_be9__n_c12   = 3139
  integer, parameter :: k_he4_b8__p_c11   = 3140
  integer, parameter :: k_n_b10__he4_li7   = 3141
  integer, parameter :: k_p_b10__he4_be7   = 3142
  integer, parameter :: k_he4_b10__n_n13   = 3143
  integer, parameter :: k_he4_b10__p_c13   = 3144
  integer, parameter :: k_n_b11__t_be9   = 3145
  integer, parameter :: k_p_b11__n_c11   = 3146
  integer, parameter :: k_he4_b11__n_n14   = 3147
  integer, parameter :: k_he4_b11__p_c14   = 3148
  integer, parameter :: k_n_c11__p_b11   = 3149
  integer, parameter :: k_p_c11__he4_b8   = 3150
  integer, parameter :: k_he4_c11__n_o14   = 3151
  integer, parameter :: k_he4_c11__p_n14   = 3152
  integer, parameter :: k_n_c12__he4_be9   = 3153
  integer, parameter :: k_he4_c12__n_o15   = 3154
  integer, parameter :: k_he4_c12__p_n15   = 3155
  integer, parameter :: k_c12_c12__n_mg23   = 3156
  integer, parameter :: k_c12_c12__p_na23   = 3157
  integer, parameter :: k_c12_c12__he4_ne20   = 3158
  integer, parameter :: k_p_c13__n_n13   = 3159
  integer, parameter :: k_p_c13__he4_b10   = 3160
  integer, parameter :: k_d_c13__n_n14   = 3161
  integer, parameter :: k_he4_c13__n_o16   = 3162
  integer, parameter :: k_p_c14__n_n14   = 3163
  integer, parameter :: k_p_c14__he4_b11   = 3164
  integer, parameter :: k_d_c14__n_n15   = 3165
  integer, parameter :: k_he4_c14__n_o17   = 3166
  integer, parameter :: k_he4_n12__p_o15   = 3167
  integer, parameter :: k_n_n13__p_c13   = 3168
  integer, parameter :: k_n_n13__he4_b10   = 3169
  integer, parameter :: k_he4_n13__p_o16   = 3170
  integer, parameter :: k_n_n14__p_c14   = 3171
  integer, parameter :: k_n_n14__d_c13   = 3172
  integer, parameter :: k_n_n14__he4_b11   = 3173
  integer, parameter :: k_p_n14__n_o14   = 3174
  integer, parameter :: k_p_n14__he4_c11   = 3175
  integer, parameter :: k_he4_n14__n_f17   = 3176
  integer, parameter :: k_he4_n14__p_o17   = 3177
  integer, parameter :: k_n_n15__d_c14   = 3178
  integer, parameter :: k_p_n15__n_o15   = 3179
  integer, parameter :: k_p_n15__he4_c12   = 3180
  integer, parameter :: k_he4_n15__n_f18   = 3181
  integer, parameter :: k_he4_n15__p_o18   = 3182
  integer, parameter :: k_n_o14__p_n14   = 3183
  integer, parameter :: k_n_o14__he4_c11   = 3184
  integer, parameter :: k_he4_o14__n_ne17   = 3185
  integer, parameter :: k_he4_o14__p_f17   = 3186
  integer, parameter :: k_n_o15__p_n15   = 3187
  integer, parameter :: k_n_o15__he4_c12   = 3188
  integer, parameter :: k_p_o15__he4_n12   = 3189
  integer, parameter :: k_he4_o15__n_ne18   = 3190
  integer, parameter :: k_he4_o15__p_f18   = 3191
  integer, parameter :: k_n_o16__he4_c13   = 3192
  integer, parameter :: k_p_o16__he4_n13   = 3193
  integer, parameter :: k_he4_o16__n_ne19   = 3194
  integer, parameter :: k_he4_o16__p_f19   = 3195
  integer, parameter :: k_c12_o16__n_si27   = 3196
  integer, parameter :: k_c12_o16__p_al27   = 3197
  integer, parameter :: k_c12_o16__he4_mg24   = 3198
  integer, parameter :: k_o16_o16__n_s31   = 3199
  integer, parameter :: k_o16_o16__p_p31   = 3200
  integer, parameter :: k_o16_o16__he4_si28   = 3201
  integer, parameter :: k_n_o17__he4_c14   = 3202
  integer, parameter :: k_p_o17__n_f17   = 3203
  integer, parameter :: k_p_o17__he4_n14   = 3204
  integer, parameter :: k_he4_o17__n_ne20   = 3205
  integer, parameter :: k_he4_o17__p_f20   = 3206
  integer, parameter :: k_p_o18__n_f18   = 3207
  integer, parameter :: k_p_o18__he4_n15   = 3208
  integer, parameter :: k_he4_o18__n_ne21   = 3209
  integer, parameter :: k_he4_o18__p_f21   = 3210
  integer, parameter :: k_p_o19__n_f19   = 3211
  integer, parameter :: k_he4_o19__n_ne22   = 3212
  integer, parameter :: k_n_f17__p_o17   = 3213
  integer, parameter :: k_n_f17__he4_n14   = 3214
  integer, parameter :: k_p_f17__n_ne17   = 3215
  integer, parameter :: k_p_f17__he4_o14   = 3216
  integer, parameter :: k_he4_f17__n_na20   = 3217
  integer, parameter :: k_he4_f17__p_ne20   = 3218
  integer, parameter :: k_n_f18__p_o18   = 3219
  integer, parameter :: k_n_f18__he4_n15   = 3220
  integer, parameter :: k_p_f18__n_ne18   = 3221
  integer, parameter :: k_p_f18__he4_o15   = 3222
  integer, parameter :: k_he4_f18__n_na21   = 3223
  integer, parameter :: k_he4_f18__p_ne21   = 3224
  integer, parameter :: k_n_f19__p_o19   = 3225
  integer, parameter :: k_p_f19__n_ne19   = 3226
  integer, parameter :: k_p_f19__he4_o16   = 3227
  integer, parameter :: k_he4_f19__n_na22   = 3228
  integer, parameter :: k_he4_f19__p_ne22   = 3229
  integer, parameter :: k_p_f20__n_ne20   = 3230
  integer, parameter :: k_p_f20__he4_o17   = 3231
  integer, parameter :: k_he4_f20__n_na23   = 3232
  integer, parameter :: k_he4_f20__p_ne23   = 3233
  integer, parameter :: k_p_f21__n_ne21   = 3234
  integer, parameter :: k_p_f21__he4_o18   = 3235
  integer, parameter :: k_he4_f21__n_na24   = 3236
  integer, parameter :: k_he4_f21__p_ne24   = 3237
  integer, parameter :: k_n_ne17__p_f17   = 3238
  integer, parameter :: k_n_ne17__he4_o14   = 3239
  integer, parameter :: k_he4_ne17__n_mg20   = 3240
  integer, parameter :: k_he4_ne17__p_na20   = 3241
  integer, parameter :: k_n_ne18__p_f18   = 3242
  integer, parameter :: k_n_ne18__he4_o15   = 3243
  integer, parameter :: k_he4_ne18__n_mg21   = 3244
  integer, parameter :: k_he4_ne18__p_na21   = 3245
  integer, parameter :: k_n_ne19__p_f19   = 3246
  integer, parameter :: k_n_ne19__he4_o16   = 3247
  integer, parameter :: k_p_ne19__n_na19   = 3248
  integer, parameter :: k_he4_ne19__n_mg22   = 3249
  integer, parameter :: k_he4_ne19__p_na22   = 3250
  integer, parameter :: k_n_ne20__p_f20   = 3251
  integer, parameter :: k_n_ne20__he4_o17   = 3252
  integer, parameter :: k_p_ne20__n_na20   = 3253
  integer, parameter :: k_p_ne20__he4_f17   = 3254
  integer, parameter :: k_he4_ne20__n_mg23   = 3255
  integer, parameter :: k_he4_ne20__p_na23   = 3256
  integer, parameter :: k_he4_ne20__c12_c12   = 3257
  integer, parameter :: k_c12_ne20__n_s31   = 3258
  integer, parameter :: k_c12_ne20__p_p31   = 3259
  integer, parameter :: k_c12_ne20__he4_si28   = 3260
  integer, parameter :: k_n_ne21__p_f21   = 3261
  integer, parameter :: k_n_ne21__he4_o18   = 3262
  integer, parameter :: k_p_ne21__n_na21   = 3263
  integer, parameter :: k_p_ne21__he4_f18   = 3264
  integer, parameter :: k_he4_ne21__n_mg24   = 3265
  integer, parameter :: k_he4_ne21__p_na24   = 3266
  integer, parameter :: k_n_ne22__he4_o19   = 3267
  integer, parameter :: k_p_ne22__n_na22   = 3268
  integer, parameter :: k_p_ne22__he4_f19   = 3269
  integer, parameter :: k_he4_ne22__n_mg25   = 3270
  integer, parameter :: k_he4_ne22__p_na25   = 3271
  integer, parameter :: k_p_ne23__n_na23   = 3272
  integer, parameter :: k_p_ne23__he4_f20   = 3273
  integer, parameter :: k_he4_ne23__n_mg26   = 3274
  integer, parameter :: k_he4_ne23__p_na26   = 3275
  integer, parameter :: k_p_ne24__n_na24   = 3276
  integer, parameter :: k_p_ne24__he4_f21   = 3277
  integer, parameter :: k_he4_ne24__n_mg27   = 3278
  integer, parameter :: k_he4_ne24__p_na27   = 3279
  integer, parameter :: k_n_na19__p_ne19   = 3280
  integer, parameter :: k_he4_na19__n_al22   = 3281
  integer, parameter :: k_he4_na19__p_mg22   = 3282
  integer, parameter :: k_n_na20__p_ne20   = 3283
  integer, parameter :: k_n_na20__he4_f17   = 3284
  integer, parameter :: k_p_na20__n_mg20   = 3285
  integer, parameter :: k_p_na20__he4_ne17   = 3286
  integer, parameter :: k_he4_na20__n_al23   = 3287
  integer, parameter :: k_he4_na20__p_mg23   = 3288
  integer, parameter :: k_n_na21__p_ne21   = 3289
  integer, parameter :: k_n_na21__he4_f18   = 3290
  integer, parameter :: k_p_na21__n_mg21   = 3291
  integer, parameter :: k_p_na21__he4_ne18   = 3292
  integer, parameter :: k_he4_na21__n_al24   = 3293
  integer, parameter :: k_he4_na21__p_mg24   = 3294
  integer, parameter :: k_n_na22__p_ne22   = 3295
  integer, parameter :: k_n_na22__he4_f19   = 3296
  integer, parameter :: k_p_na22__n_mg22   = 3297
  integer, parameter :: k_p_na22__he4_ne19   = 3298
  integer, parameter :: k_he4_na22__n_al25   = 3299
  integer, parameter :: k_he4_na22__p_mg25   = 3300
  integer, parameter :: k_n_na23__p_ne23   = 3301
  integer, parameter :: k_n_na23__he4_f20   = 3302
  integer, parameter :: k_p_na23__n_mg23   = 3303
  integer, parameter :: k_p_na23__he4_ne20   = 3304
  integer, parameter :: k_p_na23__c12_c12   = 3305
  integer, parameter :: k_he4_na23__n_al26   = 3306
  integer, parameter :: k_he4_na23__p_mg26   = 3307
  integer, parameter :: k_n_na24__p_ne24   = 3308
  integer, parameter :: k_n_na24__he4_f21   = 3309
  integer, parameter :: k_p_na24__n_mg24   = 3310
  integer, parameter :: k_p_na24__he4_ne21   = 3311
  integer, parameter :: k_he4_na24__n_al27   = 3312
  integer, parameter :: k_he4_na24__p_mg27   = 3313
  integer, parameter :: k_p_na25__n_mg25   = 3314
  integer, parameter :: k_p_na25__he4_ne22   = 3315
  integer, parameter :: k_he4_na25__n_al28   = 3316
  integer, parameter :: k_he4_na25__p_mg28   = 3317
  integer, parameter :: k_p_na26__n_mg26   = 3318
  integer, parameter :: k_p_na26__he4_ne23   = 3319
  integer, parameter :: k_he4_na26__n_al29   = 3320
  integer, parameter :: k_he4_na26__p_mg29   = 3321
  integer, parameter :: k_p_na27__n_mg27   = 3322
  integer, parameter :: k_p_na27__he4_ne24   = 3323
  integer, parameter :: k_he4_na27__n_al30   = 3324
  integer, parameter :: k_n_mg20__p_na20   = 3325
  integer, parameter :: k_n_mg20__he4_ne17   = 3326
  integer, parameter :: k_he4_mg20__n_si23   = 3327
  integer, parameter :: k_he4_mg20__p_al23   = 3328
  integer, parameter :: k_n_mg21__p_na21   = 3329
  integer, parameter :: k_n_mg21__he4_ne18   = 3330
  integer, parameter :: k_he4_mg21__n_si24   = 3331
  integer, parameter :: k_he4_mg21__p_al24   = 3332
  integer, parameter :: k_n_mg22__p_na22   = 3333
  integer, parameter :: k_n_mg22__he4_ne19   = 3334
  integer, parameter :: k_p_mg22__n_al22   = 3335
  integer, parameter :: k_p_mg22__he4_na19   = 3336
  integer, parameter :: k_he4_mg22__n_si25   = 3337
  integer, parameter :: k_he4_mg22__p_al25   = 3338
  integer, parameter :: k_n_mg23__p_na23   = 3339
  integer, parameter :: k_n_mg23__he4_ne20   = 3340
  integer, parameter :: k_n_mg23__c12_c12   = 3341
  integer, parameter :: k_p_mg23__n_al23   = 3342
  integer, parameter :: k_p_mg23__he4_na20   = 3343
  integer, parameter :: k_he4_mg23__n_si26   = 3344
  integer, parameter :: k_he4_mg23__p_al26   = 3345
  integer, parameter :: k_n_mg24__p_na24   = 3346
  integer, parameter :: k_n_mg24__he4_ne21   = 3347
  integer, parameter :: k_p_mg24__n_al24   = 3348
  integer, parameter :: k_p_mg24__he4_na21   = 3349
  integer, parameter :: k_he4_mg24__n_si27   = 3350
  integer, parameter :: k_he4_mg24__p_al27   = 3351
  integer, parameter :: k_he4_mg24__c12_o16   = 3352
  integer, parameter :: k_n_mg25__p_na25   = 3353
  integer, parameter :: k_n_mg25__he4_ne22   = 3354
  integer, parameter :: k_p_mg25__n_al25   = 3355
  integer, parameter :: k_p_mg25__he4_na22   = 3356
  integer, parameter :: k_he4_mg25__n_si28   = 3357
  integer, parameter :: k_he4_mg25__p_al28   = 3358
  integer, parameter :: k_n_mg26__p_na26   = 3359
  integer, parameter :: k_n_mg26__he4_ne23   = 3360
  integer, parameter :: k_p_mg26__n_al26   = 3361
  integer, parameter :: k_p_mg26__he4_na23   = 3362
  integer, parameter :: k_he4_mg26__n_si29   = 3363
  integer, parameter :: k_he4_mg26__p_al29   = 3364
  integer, parameter :: k_n_mg27__p_na27   = 3365
  integer, parameter :: k_n_mg27__he4_ne24   = 3366
  integer, parameter :: k_p_mg27__n_al27   = 3367
  integer, parameter :: k_p_mg27__he4_na24   = 3368
  integer, parameter :: k_he4_mg27__n_si30   = 3369
  integer, parameter :: k_he4_mg27__p_al30   = 3370
  integer, parameter :: k_p_mg28__n_al28   = 3371
  integer, parameter :: k_p_mg28__he4_na25   = 3372
  integer, parameter :: k_he4_mg28__n_si31   = 3373
  integer, parameter :: k_he4_mg28__p_al31   = 3374
  integer, parameter :: k_p_mg29__n_al29   = 3375
  integer, parameter :: k_p_mg29__he4_na26   = 3376
  integer, parameter :: k_he4_mg29__n_si32   = 3377
  integer, parameter :: k_n_al22__p_mg22   = 3378
  integer, parameter :: k_n_al22__he4_na19   = 3379
  integer, parameter :: k_he4_al22__p_si25   = 3380
  integer, parameter :: k_n_al23__p_mg23   = 3381
  integer, parameter :: k_n_al23__he4_na20   = 3382
  integer, parameter :: k_p_al23__n_si23   = 3383
  integer, parameter :: k_p_al23__he4_mg20   = 3384
  integer, parameter :: k_he4_al23__p_si26   = 3385
  integer, parameter :: k_n_al24__p_mg24   = 3386
  integer, parameter :: k_n_al24__he4_na21   = 3387
  integer, parameter :: k_p_al24__n_si24   = 3388
  integer, parameter :: k_p_al24__he4_mg21   = 3389
  integer, parameter :: k_he4_al24__n_p27   = 3390
  integer, parameter :: k_he4_al24__p_si27   = 3391
  integer, parameter :: k_n_al25__p_mg25   = 3392
  integer, parameter :: k_n_al25__he4_na22   = 3393
  integer, parameter :: k_p_al25__n_si25   = 3394
  integer, parameter :: k_p_al25__he4_mg22   = 3395
  integer, parameter :: k_he4_al25__n_p28   = 3396
  integer, parameter :: k_he4_al25__p_si28   = 3397
  integer, parameter :: k_n_al26__p_mg26   = 3398
  integer, parameter :: k_n_al26__he4_na23   = 3399
  integer, parameter :: k_p_al26__n_si26   = 3400
  integer, parameter :: k_p_al26__he4_mg23   = 3401
  integer, parameter :: k_he4_al26__n_p29   = 3402
  integer, parameter :: k_he4_al26__p_si29   = 3403
  integer, parameter :: k_n_al27__p_mg27   = 3404
  integer, parameter :: k_n_al27__he4_na24   = 3405
  integer, parameter :: k_p_al27__n_si27   = 3406
  integer, parameter :: k_p_al27__he4_mg24   = 3407
  integer, parameter :: k_p_al27__c12_o16   = 3408
  integer, parameter :: k_he4_al27__n_p30   = 3409
  integer, parameter :: k_he4_al27__p_si30   = 3410
  integer, parameter :: k_n_al28__p_mg28   = 3411
  integer, parameter :: k_n_al28__he4_na25   = 3412
  integer, parameter :: k_p_al28__n_si28   = 3413
  integer, parameter :: k_p_al28__he4_mg25   = 3414
  integer, parameter :: k_he4_al28__n_p31   = 3415
  integer, parameter :: k_he4_al28__p_si31   = 3416
  integer, parameter :: k_n_al29__p_mg29   = 3417
  integer, parameter :: k_n_al29__he4_na26   = 3418
  integer, parameter :: k_p_al29__n_si29   = 3419
  integer, parameter :: k_p_al29__he4_mg26   = 3420
  integer, parameter :: k_he4_al29__n_p32   = 3421
  integer, parameter :: k_he4_al29__p_si32   = 3422
  integer, parameter :: k_n_al30__he4_na27   = 3423
  integer, parameter :: k_p_al30__n_si30   = 3424
  integer, parameter :: k_p_al30__he4_mg27   = 3425
  integer, parameter :: k_he4_al30__n_p33   = 3426
  integer, parameter :: k_he4_al30__p_si33   = 3427
  integer, parameter :: k_p_al31__n_si31   = 3428
  integer, parameter :: k_p_al31__he4_mg28   = 3429
  integer, parameter :: k_he4_al31__n_p34   = 3430
  integer, parameter :: k_he4_al31__p_si34   = 3431
  integer, parameter :: k_n_si23__p_al23   = 3432
  integer, parameter :: k_n_si23__he4_mg20   = 3433
  integer, parameter :: k_n_si24__p_al24   = 3434
  integer, parameter :: k_n_si24__he4_mg21   = 3435
  integer, parameter :: k_he4_si24__p_p27   = 3436
  integer, parameter :: k_n_si25__p_al25   = 3437
  integer, parameter :: k_n_si25__he4_mg22   = 3438
  integer, parameter :: k_p_si25__he4_al22   = 3439
  integer, parameter :: k_he4_si25__n_s28   = 3440
  integer, parameter :: k_he4_si25__p_p28   = 3441
  integer, parameter :: k_n_si26__p_al26   = 3442
  integer, parameter :: k_n_si26__he4_mg23   = 3443
  integer, parameter :: k_p_si26__he4_al23   = 3444
  integer, parameter :: k_he4_si26__n_s29   = 3445
  integer, parameter :: k_he4_si26__p_p29   = 3446
  integer, parameter :: k_n_si27__p_al27   = 3447
  integer, parameter :: k_n_si27__he4_mg24   = 3448
  integer, parameter :: k_n_si27__c12_o16   = 3449
  integer, parameter :: k_p_si27__n_p27   = 3450
  integer, parameter :: k_p_si27__he4_al24   = 3451
  integer, parameter :: k_he4_si27__n_s30   = 3452
  integer, parameter :: k_he4_si27__p_p30   = 3453
  integer, parameter :: k_n_si28__p_al28   = 3454
  integer, parameter :: k_n_si28__he4_mg25   = 3455
  integer, parameter :: k_p_si28__n_p28   = 3456
  integer, parameter :: k_p_si28__he4_al25   = 3457
  integer, parameter :: k_he4_si28__n_s31   = 3458
  integer, parameter :: k_he4_si28__p_p31   = 3459
  integer, parameter :: k_he4_si28__c12_ne20   = 3460
  integer, parameter :: k_he4_si28__o16_o16   = 3461
  integer, parameter :: k_n_si29__p_al29   = 3462
  integer, parameter :: k_n_si29__he4_mg26   = 3463
  integer, parameter :: k_p_si29__n_p29   = 3464
  integer, parameter :: k_p_si29__he4_al26   = 3465
  integer, parameter :: k_he4_si29__n_s32   = 3466
  integer, parameter :: k_he4_si29__p_p32   = 3467
  integer, parameter :: k_n_si30__p_al30   = 3468
  integer, parameter :: k_n_si30__he4_mg27   = 3469
  integer, parameter :: k_p_si30__n_p30   = 3470
  integer, parameter :: k_p_si30__he4_al27   = 3471
  integer, parameter :: k_he4_si30__n_s33   = 3472
  integer, parameter :: k_he4_si30__p_p33   = 3473
  integer, parameter :: k_n_si31__p_al31   = 3474
  integer, parameter :: k_n_si31__he4_mg28   = 3475
  integer, parameter :: k_p_si31__n_p31   = 3476
  integer, parameter :: k_p_si31__he4_al28   = 3477
  integer, parameter :: k_he4_si31__n_s34   = 3478
  integer, parameter :: k_he4_si31__p_p34   = 3479
  integer, parameter :: k_n_si32__he4_mg29   = 3480
  integer, parameter :: k_p_si32__n_p32   = 3481
  integer, parameter :: k_p_si32__he4_al29   = 3482
  integer, parameter :: k_he4_si32__n_s35   = 3483
  integer, parameter :: k_he4_si32__p_p35   = 3484
  integer, parameter :: k_p_si33__n_p33   = 3485
  integer, parameter :: k_p_si33__he4_al30   = 3486
  integer, parameter :: k_he4_si33__n_s36   = 3487
  integer, parameter :: k_he4_si33__p_p36   = 3488
  integer, parameter :: k_p_si34__n_p34   = 3489
  integer, parameter :: k_p_si34__he4_al31   = 3490
  integer, parameter :: k_he4_si34__n_s37   = 3491
  integer, parameter :: k_he4_si34__p_p37   = 3492
  integer, parameter :: k_n_p27__p_si27   = 3493
  integer, parameter :: k_n_p27__he4_al24   = 3494
  integer, parameter :: k_p_p27__he4_si24   = 3495
  integer, parameter :: k_he4_p27__p_s30   = 3496
  integer, parameter :: k_n_p28__p_si28   = 3497
  integer, parameter :: k_n_p28__he4_al25   = 3498
  integer, parameter :: k_p_p28__n_s28   = 3499
  integer, parameter :: k_p_p28__he4_si25   = 3500
  integer, parameter :: k_he4_p28__n_cl31   = 3501
  integer, parameter :: k_he4_p28__p_s31   = 3502
  integer, parameter :: k_n_p29__p_si29   = 3503
  integer, parameter :: k_n_p29__he4_al26   = 3504
  integer, parameter :: k_p_p29__n_s29   = 3505
  integer, parameter :: k_p_p29__he4_si26   = 3506
  integer, parameter :: k_he4_p29__n_cl32   = 3507
  integer, parameter :: k_he4_p29__p_s32   = 3508
  integer, parameter :: k_n_p30__p_si30   = 3509
  integer, parameter :: k_n_p30__he4_al27   = 3510
  integer, parameter :: k_p_p30__n_s30   = 3511
  integer, parameter :: k_p_p30__he4_si27   = 3512
  integer, parameter :: k_he4_p30__n_cl33   = 3513
  integer, parameter :: k_he4_p30__p_s33   = 3514
  integer, parameter :: k_n_p31__p_si31   = 3515
  integer, parameter :: k_n_p31__he4_al28   = 3516
  integer, parameter :: k_p_p31__n_s31   = 3517
  integer, parameter :: k_p_p31__he4_si28   = 3518
  integer, parameter :: k_p_p31__c12_ne20   = 3519
  integer, parameter :: k_p_p31__o16_o16   = 3520
  integer, parameter :: k_he4_p31__n_cl34   = 3521
  integer, parameter :: k_he4_p31__p_s34   = 3522
  integer, parameter :: k_n_p32__p_si32   = 3523
  integer, parameter :: k_n_p32__he4_al29   = 3524
  integer, parameter :: k_p_p32__n_s32   = 3525
  integer, parameter :: k_p_p32__he4_si29   = 3526
  integer, parameter :: k_he4_p32__n_cl35   = 3527
  integer, parameter :: k_he4_p32__p_s35   = 3528
  integer, parameter :: k_n_p33__p_si33   = 3529
  integer, parameter :: k_n_p33__he4_al30   = 3530
  integer, parameter :: k_p_p33__n_s33   = 3531
  integer, parameter :: k_p_p33__he4_si30   = 3532
  integer, parameter :: k_he4_p33__n_cl36   = 3533
  integer, parameter :: k_he4_p33__p_s36   = 3534
  integer, parameter :: k_n_p34__p_si34   = 3535
  integer, parameter :: k_n_p34__he4_al31   = 3536
  integer, parameter :: k_p_p34__n_s34   = 3537
  integer, parameter :: k_p_p34__he4_si31   = 3538
  integer, parameter :: k_he4_p34__n_cl37   = 3539
  integer, parameter :: k_he4_p34__p_s37   = 3540
  integer, parameter :: k_p_p35__n_s35   = 3541
  integer, parameter :: k_p_p35__he4_si32   = 3542
  integer, parameter :: k_he4_p35__n_cl38   = 3543
  integer, parameter :: k_he4_p35__p_s38   = 3544
  integer, parameter :: k_p_p36__n_s36   = 3545
  integer, parameter :: k_p_p36__he4_si33   = 3546
  integer, parameter :: k_he4_p36__n_cl39   = 3547
  integer, parameter :: k_he4_p36__p_s39   = 3548
  integer, parameter :: k_p_p37__n_s37   = 3549
  integer, parameter :: k_p_p37__he4_si34   = 3550
  integer, parameter :: k_he4_p37__n_cl40   = 3551
  integer, parameter :: k_he4_p37__p_s40   = 3552
  integer, parameter :: k_p_p38__n_s38   = 3553
  integer, parameter :: k_he4_p38__n_cl41   = 3554
  integer, parameter :: k_he4_p38__p_s41   = 3555
  integer, parameter :: k_n_s28__p_p28   = 3556
  integer, parameter :: k_n_s28__he4_si25   = 3557
  integer, parameter :: k_he4_s28__p_cl31   = 3558
  integer, parameter :: k_n_s29__p_p29   = 3559
  integer, parameter :: k_n_s29__he4_si26   = 3560
  integer, parameter :: k_he4_s29__n_ar32   = 3561
  integer, parameter :: k_he4_s29__p_cl32   = 3562
  integer, parameter :: k_n_s30__p_p30   = 3563
  integer, parameter :: k_n_s30__he4_si27   = 3564
  integer, parameter :: k_p_s30__he4_p27   = 3565
  integer, parameter :: k_he4_s30__n_ar33   = 3566
  integer, parameter :: k_he4_s30__p_cl33   = 3567
  integer, parameter :: k_n_s31__p_p31   = 3568
  integer, parameter :: k_n_s31__he4_si28   = 3569
  integer, parameter :: k_n_s31__c12_ne20   = 3570
  integer, parameter :: k_n_s31__o16_o16   = 3571
  integer, parameter :: k_p_s31__n_cl31   = 3572
  integer, parameter :: k_p_s31__he4_p28   = 3573
  integer, parameter :: k_he4_s31__n_ar34   = 3574
  integer, parameter :: k_he4_s31__p_cl34   = 3575
  integer, parameter :: k_n_s32__p_p32   = 3576
  integer, parameter :: k_n_s32__he4_si29   = 3577
  integer, parameter :: k_p_s32__n_cl32   = 3578
  integer, parameter :: k_p_s32__he4_p29   = 3579
  integer, parameter :: k_he4_s32__n_ar35   = 3580
  integer, parameter :: k_he4_s32__p_cl35   = 3581
  integer, parameter :: k_n_s33__p_p33   = 3582
  integer, parameter :: k_n_s33__he4_si30   = 3583
  integer, parameter :: k_p_s33__n_cl33   = 3584
  integer, parameter :: k_p_s33__he4_p30   = 3585
  integer, parameter :: k_he4_s33__n_ar36   = 3586
  integer, parameter :: k_he4_s33__p_cl36   = 3587
  integer, parameter :: k_n_s34__p_p34   = 3588
  integer, parameter :: k_n_s34__he4_si31   = 3589
  integer, parameter :: k_p_s34__n_cl34   = 3590
  integer, parameter :: k_p_s34__he4_p31   = 3591
  integer, parameter :: k_he4_s34__n_ar37   = 3592
  integer, parameter :: k_he4_s34__p_cl37   = 3593
  integer, parameter :: k_n_s35__p_p35   = 3594
  integer, parameter :: k_n_s35__he4_si32   = 3595
  integer, parameter :: k_p_s35__n_cl35   = 3596
  integer, parameter :: k_p_s35__he4_p32   = 3597
  integer, parameter :: k_he4_s35__n_ar38   = 3598
  integer, parameter :: k_he4_s35__p_cl38   = 3599
  integer, parameter :: k_n_s36__p_p36   = 3600
  integer, parameter :: k_n_s36__he4_si33   = 3601
  integer, parameter :: k_p_s36__n_cl36   = 3602
  integer, parameter :: k_p_s36__he4_p33   = 3603
  integer, parameter :: k_he4_s36__n_ar39   = 3604
  integer, parameter :: k_he4_s36__p_cl39   = 3605
  integer, parameter :: k_n_s37__p_p37   = 3606
  integer, parameter :: k_n_s37__he4_si34   = 3607
  integer, parameter :: k_p_s37__n_cl37   = 3608
  integer, parameter :: k_p_s37__he4_p34   = 3609
  integer, parameter :: k_he4_s37__n_ar40   = 3610
  integer, parameter :: k_he4_s37__p_cl40   = 3611
  integer, parameter :: k_n_s38__p_p38   = 3612
  integer, parameter :: k_p_s38__n_cl38   = 3613
  integer, parameter :: k_p_s38__he4_p35   = 3614
  integer, parameter :: k_he4_s38__n_ar41   = 3615
  integer, parameter :: k_he4_s38__p_cl41   = 3616
  integer, parameter :: k_p_s39__n_cl39   = 3617
  integer, parameter :: k_p_s39__he4_p36   = 3618
  integer, parameter :: k_he4_s39__n_ar42   = 3619
  integer, parameter :: k_he4_s39__p_cl42   = 3620
  integer, parameter :: k_p_s40__n_cl40   = 3621
  integer, parameter :: k_p_s40__he4_p37   = 3622
  integer, parameter :: k_he4_s40__n_ar43   = 3623
  integer, parameter :: k_he4_s40__p_cl43   = 3624
  integer, parameter :: k_p_s41__n_cl41   = 3625
  integer, parameter :: k_p_s41__he4_p38   = 3626
  integer, parameter :: k_he4_s41__n_ar44   = 3627
  integer, parameter :: k_he4_s41__p_cl44   = 3628
  integer, parameter :: k_p_s42__n_cl42   = 3629
  integer, parameter :: k_he4_s42__n_ar45   = 3630
  integer, parameter :: k_he4_s42__p_cl45   = 3631
  integer, parameter :: k_n_cl31__p_s31   = 3632
  integer, parameter :: k_n_cl31__he4_p28   = 3633
  integer, parameter :: k_p_cl31__he4_s28   = 3634
  integer, parameter :: k_he4_cl31__p_ar34   = 3635
  integer, parameter :: k_n_cl32__p_s32   = 3636
  integer, parameter :: k_n_cl32__he4_p29   = 3637
  integer, parameter :: k_p_cl32__n_ar32   = 3638
  integer, parameter :: k_p_cl32__he4_s29   = 3639
  integer, parameter :: k_he4_cl32__n_k35   = 3640
  integer, parameter :: k_he4_cl32__p_ar35   = 3641
  integer, parameter :: k_n_cl33__p_s33   = 3642
  integer, parameter :: k_n_cl33__he4_p30   = 3643
  integer, parameter :: k_p_cl33__n_ar33   = 3644
  integer, parameter :: k_p_cl33__he4_s30   = 3645
  integer, parameter :: k_he4_cl33__n_k36   = 3646
  integer, parameter :: k_he4_cl33__p_ar36   = 3647
  integer, parameter :: k_n_cl34__p_s34   = 3648
  integer, parameter :: k_n_cl34__he4_p31   = 3649
  integer, parameter :: k_p_cl34__n_ar34   = 3650
  integer, parameter :: k_p_cl34__he4_s31   = 3651
  integer, parameter :: k_he4_cl34__n_k37   = 3652
  integer, parameter :: k_he4_cl34__p_ar37   = 3653
  integer, parameter :: k_n_cl35__p_s35   = 3654
  integer, parameter :: k_n_cl35__he4_p32   = 3655
  integer, parameter :: k_p_cl35__n_ar35   = 3656
  integer, parameter :: k_p_cl35__he4_s32   = 3657
  integer, parameter :: k_he4_cl35__n_k38   = 3658
  integer, parameter :: k_he4_cl35__p_ar38   = 3659
  integer, parameter :: k_n_cl36__p_s36   = 3660
  integer, parameter :: k_n_cl36__he4_p33   = 3661
  integer, parameter :: k_p_cl36__n_ar36   = 3662
  integer, parameter :: k_p_cl36__he4_s33   = 3663
  integer, parameter :: k_he4_cl36__n_k39   = 3664
  integer, parameter :: k_he4_cl36__p_ar39   = 3665
  integer, parameter :: k_n_cl37__p_s37   = 3666
  integer, parameter :: k_n_cl37__he4_p34   = 3667
  integer, parameter :: k_p_cl37__n_ar37   = 3668
  integer, parameter :: k_p_cl37__he4_s34   = 3669
  integer, parameter :: k_he4_cl37__n_k40   = 3670
  integer, parameter :: k_he4_cl37__p_ar40   = 3671
  integer, parameter :: k_n_cl38__p_s38   = 3672
  integer, parameter :: k_n_cl38__he4_p35   = 3673
  integer, parameter :: k_p_cl38__n_ar38   = 3674
  integer, parameter :: k_p_cl38__he4_s35   = 3675
  integer, parameter :: k_he4_cl38__n_k41   = 3676
  integer, parameter :: k_he4_cl38__p_ar41   = 3677
  integer, parameter :: k_n_cl39__p_s39   = 3678
  integer, parameter :: k_n_cl39__he4_p36   = 3679
  integer, parameter :: k_p_cl39__n_ar39   = 3680
  integer, parameter :: k_p_cl39__he4_s36   = 3681
  integer, parameter :: k_he4_cl39__n_k42   = 3682
  integer, parameter :: k_he4_cl39__p_ar42   = 3683
  integer, parameter :: k_n_cl40__p_s40   = 3684
  integer, parameter :: k_n_cl40__he4_p37   = 3685
  integer, parameter :: k_p_cl40__n_ar40   = 3686
  integer, parameter :: k_p_cl40__he4_s37   = 3687
  integer, parameter :: k_he4_cl40__n_k43   = 3688
  integer, parameter :: k_he4_cl40__p_ar43   = 3689
  integer, parameter :: k_n_cl41__p_s41   = 3690
  integer, parameter :: k_n_cl41__he4_p38   = 3691
  integer, parameter :: k_p_cl41__n_ar41   = 3692
  integer, parameter :: k_p_cl41__he4_s38   = 3693
  integer, parameter :: k_he4_cl41__n_k44   = 3694
  integer, parameter :: k_he4_cl41__p_ar44   = 3695
  integer, parameter :: k_n_cl42__p_s42   = 3696
  integer, parameter :: k_p_cl42__n_ar42   = 3697
  integer, parameter :: k_p_cl42__he4_s39   = 3698
  integer, parameter :: k_he4_cl42__n_k45   = 3699
  integer, parameter :: k_he4_cl42__p_ar45   = 3700
  integer, parameter :: k_p_cl43__n_ar43   = 3701
  integer, parameter :: k_p_cl43__he4_s40   = 3702
  integer, parameter :: k_he4_cl43__n_k46   = 3703
  integer, parameter :: k_he4_cl43__p_ar46   = 3704
  integer, parameter :: k_p_cl44__n_ar44   = 3705
  integer, parameter :: k_p_cl44__he4_s41   = 3706
  integer, parameter :: k_he4_cl44__n_k47   = 3707
  integer, parameter :: k_p_cl45__n_ar45   = 3708
  integer, parameter :: k_p_cl45__he4_s42   = 3709
  integer, parameter :: k_he4_cl45__n_k48   = 3710
  integer, parameter :: k_n_ar32__p_cl32   = 3711
  integer, parameter :: k_n_ar32__he4_s29   = 3712
  integer, parameter :: k_he4_ar32__p_k35   = 3713
  integer, parameter :: k_n_ar33__p_cl33   = 3714
  integer, parameter :: k_n_ar33__he4_s30   = 3715
  integer, parameter :: k_he4_ar33__n_ca36   = 3716
  integer, parameter :: k_he4_ar33__p_k36   = 3717
  integer, parameter :: k_n_ar34__p_cl34   = 3718
  integer, parameter :: k_n_ar34__he4_s31   = 3719
  integer, parameter :: k_p_ar34__he4_cl31   = 3720
  integer, parameter :: k_he4_ar34__n_ca37   = 3721
  integer, parameter :: k_he4_ar34__p_k37   = 3722
  integer, parameter :: k_n_ar35__p_cl35   = 3723
  integer, parameter :: k_n_ar35__he4_s32   = 3724
  integer, parameter :: k_p_ar35__n_k35   = 3725
  integer, parameter :: k_p_ar35__he4_cl32   = 3726
  integer, parameter :: k_he4_ar35__n_ca38   = 3727
  integer, parameter :: k_he4_ar35__p_k38   = 3728
  integer, parameter :: k_n_ar36__p_cl36   = 3729
  integer, parameter :: k_n_ar36__he4_s33   = 3730
  integer, parameter :: k_p_ar36__n_k36   = 3731
  integer, parameter :: k_p_ar36__he4_cl33   = 3732
  integer, parameter :: k_he4_ar36__n_ca39   = 3733
  integer, parameter :: k_he4_ar36__p_k39   = 3734
  integer, parameter :: k_n_ar37__p_cl37   = 3735
  integer, parameter :: k_n_ar37__he4_s34   = 3736
  integer, parameter :: k_p_ar37__n_k37   = 3737
  integer, parameter :: k_p_ar37__he4_cl34   = 3738
  integer, parameter :: k_he4_ar37__n_ca40   = 3739
  integer, parameter :: k_he4_ar37__p_k40   = 3740
  integer, parameter :: k_n_ar38__p_cl38   = 3741
  integer, parameter :: k_n_ar38__he4_s35   = 3742
  integer, parameter :: k_p_ar38__n_k38   = 3743
  integer, parameter :: k_p_ar38__he4_cl35   = 3744
  integer, parameter :: k_he4_ar38__n_ca41   = 3745
  integer, parameter :: k_he4_ar38__p_k41   = 3746
  integer, parameter :: k_n_ar39__p_cl39   = 3747
  integer, parameter :: k_n_ar39__he4_s36   = 3748
  integer, parameter :: k_p_ar39__n_k39   = 3749
  integer, parameter :: k_p_ar39__he4_cl36   = 3750
  integer, parameter :: k_he4_ar39__n_ca42   = 3751
  integer, parameter :: k_he4_ar39__p_k42   = 3752
  integer, parameter :: k_n_ar40__p_cl40   = 3753
  integer, parameter :: k_n_ar40__he4_s37   = 3754
  integer, parameter :: k_p_ar40__n_k40   = 3755
  integer, parameter :: k_p_ar40__he4_cl37   = 3756
  integer, parameter :: k_he4_ar40__n_ca43   = 3757
  integer, parameter :: k_he4_ar40__p_k43   = 3758
  integer, parameter :: k_n_ar41__p_cl41   = 3759
  integer, parameter :: k_n_ar41__he4_s38   = 3760
  integer, parameter :: k_p_ar41__n_k41   = 3761
  integer, parameter :: k_p_ar41__he4_cl38   = 3762
  integer, parameter :: k_he4_ar41__n_ca44   = 3763
  integer, parameter :: k_he4_ar41__p_k44   = 3764
  integer, parameter :: k_n_ar42__p_cl42   = 3765
  integer, parameter :: k_n_ar42__he4_s39   = 3766
  integer, parameter :: k_p_ar42__n_k42   = 3767
  integer, parameter :: k_p_ar42__he4_cl39   = 3768
  integer, parameter :: k_he4_ar42__n_ca45   = 3769
  integer, parameter :: k_he4_ar42__p_k45   = 3770
  integer, parameter :: k_n_ar43__p_cl43   = 3771
  integer, parameter :: k_n_ar43__he4_s40   = 3772
  integer, parameter :: k_p_ar43__n_k43   = 3773
  integer, parameter :: k_p_ar43__he4_cl40   = 3774
  integer, parameter :: k_he4_ar43__n_ca46   = 3775
  integer, parameter :: k_he4_ar43__p_k46   = 3776
  integer, parameter :: k_n_ar44__p_cl44   = 3777
  integer, parameter :: k_n_ar44__he4_s41   = 3778
  integer, parameter :: k_p_ar44__n_k44   = 3779
  integer, parameter :: k_p_ar44__he4_cl41   = 3780
  integer, parameter :: k_he4_ar44__n_ca47   = 3781
  integer, parameter :: k_he4_ar44__p_k47   = 3782
  integer, parameter :: k_n_ar45__p_cl45   = 3783
  integer, parameter :: k_n_ar45__he4_s42   = 3784
  integer, parameter :: k_p_ar45__n_k45   = 3785
  integer, parameter :: k_p_ar45__he4_cl42   = 3786
  integer, parameter :: k_he4_ar45__n_ca48   = 3787
  integer, parameter :: k_he4_ar45__p_k48   = 3788
  integer, parameter :: k_p_ar46__n_k46   = 3789
  integer, parameter :: k_p_ar46__he4_cl43   = 3790
  integer, parameter :: k_he4_ar46__n_ca49   = 3791
  integer, parameter :: k_he4_ar46__p_k49   = 3792
  integer, parameter :: k_n_k35__p_ar35   = 3793
  integer, parameter :: k_n_k35__he4_cl32   = 3794
  integer, parameter :: k_p_k35__he4_ar32   = 3795
  integer, parameter :: k_he4_k35__p_ca38   = 3796
  integer, parameter :: k_n_k36__p_ar36   = 3797
  integer, parameter :: k_n_k36__he4_cl33   = 3798
  integer, parameter :: k_p_k36__n_ca36   = 3799
  integer, parameter :: k_p_k36__he4_ar33   = 3800
  integer, parameter :: k_he4_k36__p_ca39   = 3801
  integer, parameter :: k_n_k37__p_ar37   = 3802
  integer, parameter :: k_n_k37__he4_cl34   = 3803
  integer, parameter :: k_p_k37__n_ca37   = 3804
  integer, parameter :: k_p_k37__he4_ar34   = 3805
  integer, parameter :: k_he4_k37__n_sc40   = 3806
  integer, parameter :: k_he4_k37__p_ca40   = 3807
  integer, parameter :: k_n_k38__p_ar38   = 3808
  integer, parameter :: k_n_k38__he4_cl35   = 3809
  integer, parameter :: k_p_k38__n_ca38   = 3810
  integer, parameter :: k_p_k38__he4_ar35   = 3811
  integer, parameter :: k_he4_k38__n_sc41   = 3812
  integer, parameter :: k_he4_k38__p_ca41   = 3813
  integer, parameter :: k_n_k39__p_ar39   = 3814
  integer, parameter :: k_n_k39__he4_cl36   = 3815
  integer, parameter :: k_p_k39__n_ca39   = 3816
  integer, parameter :: k_p_k39__he4_ar36   = 3817
  integer, parameter :: k_he4_k39__n_sc42   = 3818
  integer, parameter :: k_he4_k39__p_ca42   = 3819
  integer, parameter :: k_n_k40__p_ar40   = 3820
  integer, parameter :: k_n_k40__he4_cl37   = 3821
  integer, parameter :: k_p_k40__n_ca40   = 3822
  integer, parameter :: k_p_k40__he4_ar37   = 3823
  integer, parameter :: k_he4_k40__n_sc43   = 3824
  integer, parameter :: k_he4_k40__p_ca43   = 3825
  integer, parameter :: k_n_k41__p_ar41   = 3826
  integer, parameter :: k_n_k41__he4_cl38   = 3827
  integer, parameter :: k_p_k41__n_ca41   = 3828
  integer, parameter :: k_p_k41__he4_ar38   = 3829
  integer, parameter :: k_he4_k41__n_sc44   = 3830
  integer, parameter :: k_he4_k41__p_ca44   = 3831
  integer, parameter :: k_n_k42__p_ar42   = 3832
  integer, parameter :: k_n_k42__he4_cl39   = 3833
  integer, parameter :: k_p_k42__n_ca42   = 3834
  integer, parameter :: k_p_k42__he4_ar39   = 3835
  integer, parameter :: k_he4_k42__n_sc45   = 3836
  integer, parameter :: k_he4_k42__p_ca45   = 3837
  integer, parameter :: k_n_k43__p_ar43   = 3838
  integer, parameter :: k_n_k43__he4_cl40   = 3839
  integer, parameter :: k_p_k43__n_ca43   = 3840
  integer, parameter :: k_p_k43__he4_ar40   = 3841
  integer, parameter :: k_he4_k43__n_sc46   = 3842
  integer, parameter :: k_he4_k43__p_ca46   = 3843
  integer, parameter :: k_n_k44__p_ar44   = 3844
  integer, parameter :: k_n_k44__he4_cl41   = 3845
  integer, parameter :: k_p_k44__n_ca44   = 3846
  integer, parameter :: k_p_k44__he4_ar41   = 3847
  integer, parameter :: k_he4_k44__n_sc47   = 3848
  integer, parameter :: k_he4_k44__p_ca47   = 3849
  integer, parameter :: k_n_k45__p_ar45   = 3850
  integer, parameter :: k_n_k45__he4_cl42   = 3851
  integer, parameter :: k_p_k45__n_ca45   = 3852
  integer, parameter :: k_p_k45__he4_ar42   = 3853
  integer, parameter :: k_he4_k45__n_sc48   = 3854
  integer, parameter :: k_he4_k45__p_ca48   = 3855
  integer, parameter :: k_n_k46__p_ar46   = 3856
  integer, parameter :: k_n_k46__he4_cl43   = 3857
  integer, parameter :: k_p_k46__n_ca46   = 3858
  integer, parameter :: k_p_k46__he4_ar43   = 3859
  integer, parameter :: k_he4_k46__n_sc49   = 3860
  integer, parameter :: k_he4_k46__p_ca49   = 3861
  integer, parameter :: k_n_k47__he4_cl44   = 3862
  integer, parameter :: k_p_k47__n_ca47   = 3863
  integer, parameter :: k_p_k47__he4_ar44   = 3864
  integer, parameter :: k_he4_k47__n_sc50   = 3865
  integer, parameter :: k_n_k48__he4_cl45   = 3866
  integer, parameter :: k_p_k48__n_ca48   = 3867
  integer, parameter :: k_p_k48__he4_ar45   = 3868
  integer, parameter :: k_he4_k48__n_sc51   = 3869
  integer, parameter :: k_p_k49__n_ca49   = 3870
  integer, parameter :: k_p_k49__he4_ar46   = 3871
  integer, parameter :: k_n_ca36__p_k36   = 3872
  integer, parameter :: k_n_ca36__he4_ar33   = 3873
  integer, parameter :: k_n_ca37__p_k37   = 3874
  integer, parameter :: k_n_ca37__he4_ar34   = 3875
  integer, parameter :: k_he4_ca37__p_sc40   = 3876
  integer, parameter :: k_n_ca38__p_k38   = 3877
  integer, parameter :: k_n_ca38__he4_ar35   = 3878
  integer, parameter :: k_p_ca38__he4_k35   = 3879
  integer, parameter :: k_he4_ca38__n_ti41   = 3880
  integer, parameter :: k_he4_ca38__p_sc41   = 3881
  integer, parameter :: k_n_ca39__p_k39   = 3882
  integer, parameter :: k_n_ca39__he4_ar36   = 3883
  integer, parameter :: k_p_ca39__he4_k36   = 3884
  integer, parameter :: k_he4_ca39__n_ti42   = 3885
  integer, parameter :: k_he4_ca39__p_sc42   = 3886
  integer, parameter :: k_n_ca40__p_k40   = 3887
  integer, parameter :: k_n_ca40__he4_ar37   = 3888
  integer, parameter :: k_p_ca40__n_sc40   = 3889
  integer, parameter :: k_p_ca40__he4_k37   = 3890
  integer, parameter :: k_he4_ca40__n_ti43   = 3891
  integer, parameter :: k_he4_ca40__p_sc43   = 3892
  integer, parameter :: k_n_ca41__p_k41   = 3893
  integer, parameter :: k_n_ca41__he4_ar38   = 3894
  integer, parameter :: k_p_ca41__n_sc41   = 3895
  integer, parameter :: k_p_ca41__he4_k38   = 3896
  integer, parameter :: k_he4_ca41__n_ti44   = 3897
  integer, parameter :: k_he4_ca41__p_sc44   = 3898
  integer, parameter :: k_n_ca42__p_k42   = 3899
  integer, parameter :: k_n_ca42__he4_ar39   = 3900
  integer, parameter :: k_p_ca42__n_sc42   = 3901
  integer, parameter :: k_p_ca42__he4_k39   = 3902
  integer, parameter :: k_he4_ca42__n_ti45   = 3903
  integer, parameter :: k_he4_ca42__p_sc45   = 3904
  integer, parameter :: k_n_ca43__p_k43   = 3905
  integer, parameter :: k_n_ca43__he4_ar40   = 3906
  integer, parameter :: k_p_ca43__n_sc43   = 3907
  integer, parameter :: k_p_ca43__he4_k40   = 3908
  integer, parameter :: k_he4_ca43__n_ti46   = 3909
  integer, parameter :: k_he4_ca43__p_sc46   = 3910
  integer, parameter :: k_n_ca44__p_k44   = 3911
  integer, parameter :: k_n_ca44__he4_ar41   = 3912
  integer, parameter :: k_p_ca44__n_sc44   = 3913
  integer, parameter :: k_p_ca44__he4_k41   = 3914
  integer, parameter :: k_he4_ca44__n_ti47   = 3915
  integer, parameter :: k_he4_ca44__p_sc47   = 3916
  integer, parameter :: k_n_ca45__p_k45   = 3917
  integer, parameter :: k_n_ca45__he4_ar42   = 3918
  integer, parameter :: k_p_ca45__n_sc45   = 3919
  integer, parameter :: k_p_ca45__he4_k42   = 3920
  integer, parameter :: k_he4_ca45__n_ti48   = 3921
  integer, parameter :: k_he4_ca45__p_sc48   = 3922
  integer, parameter :: k_n_ca46__p_k46   = 3923
  integer, parameter :: k_n_ca46__he4_ar43   = 3924
  integer, parameter :: k_p_ca46__n_sc46   = 3925
  integer, parameter :: k_p_ca46__he4_k43   = 3926
  integer, parameter :: k_he4_ca46__n_ti49   = 3927
  integer, parameter :: k_he4_ca46__p_sc49   = 3928
  integer, parameter :: k_n_ca47__p_k47   = 3929
  integer, parameter :: k_n_ca47__he4_ar44   = 3930
  integer, parameter :: k_p_ca47__n_sc47   = 3931
  integer, parameter :: k_p_ca47__he4_k44   = 3932
  integer, parameter :: k_he4_ca47__n_ti50   = 3933
  integer, parameter :: k_he4_ca47__p_sc50   = 3934
  integer, parameter :: k_n_ca48__p_k48   = 3935
  integer, parameter :: k_n_ca48__he4_ar45   = 3936
  integer, parameter :: k_p_ca48__n_sc48   = 3937
  integer, parameter :: k_p_ca48__he4_k45   = 3938
  integer, parameter :: k_he4_ca48__n_ti51   = 3939
  integer, parameter :: k_he4_ca48__p_sc51   = 3940
  integer, parameter :: k_n_ca49__p_k49   = 3941
  integer, parameter :: k_n_ca49__he4_ar46   = 3942
  integer, parameter :: k_p_ca49__n_sc49   = 3943
  integer, parameter :: k_p_ca49__he4_k46   = 3944
  integer, parameter :: k_he4_ca49__n_ti52   = 3945
  integer, parameter :: k_n_sc40__p_ca40   = 3946
  integer, parameter :: k_n_sc40__he4_k37   = 3947
  integer, parameter :: k_p_sc40__he4_ca37   = 3948
  integer, parameter :: k_he4_sc40__n_v43   = 3949
  integer, parameter :: k_he4_sc40__p_ti43   = 3950
  integer, parameter :: k_n_sc41__p_ca41   = 3951
  integer, parameter :: k_n_sc41__he4_k38   = 3952
  integer, parameter :: k_p_sc41__n_ti41   = 3953
  integer, parameter :: k_p_sc41__he4_ca38   = 3954
  integer, parameter :: k_he4_sc41__n_v44   = 3955
  integer, parameter :: k_he4_sc41__p_ti44   = 3956
  integer, parameter :: k_n_sc42__p_ca42   = 3957
  integer, parameter :: k_n_sc42__he4_k39   = 3958
  integer, parameter :: k_p_sc42__n_ti42   = 3959
  integer, parameter :: k_p_sc42__he4_ca39   = 3960
  integer, parameter :: k_he4_sc42__n_v45   = 3961
  integer, parameter :: k_he4_sc42__p_ti45   = 3962
  integer, parameter :: k_n_sc43__p_ca43   = 3963
  integer, parameter :: k_n_sc43__he4_k40   = 3964
  integer, parameter :: k_p_sc43__n_ti43   = 3965
  integer, parameter :: k_p_sc43__he4_ca40   = 3966
  integer, parameter :: k_he4_sc43__n_v46   = 3967
  integer, parameter :: k_he4_sc43__p_ti46   = 3968
  integer, parameter :: k_n_sc44__p_ca44   = 3969
  integer, parameter :: k_n_sc44__he4_k41   = 3970
  integer, parameter :: k_p_sc44__n_ti44   = 3971
  integer, parameter :: k_p_sc44__he4_ca41   = 3972
  integer, parameter :: k_he4_sc44__n_v47   = 3973
  integer, parameter :: k_he4_sc44__p_ti47   = 3974
  integer, parameter :: k_n_sc45__p_ca45   = 3975
  integer, parameter :: k_n_sc45__he4_k42   = 3976
  integer, parameter :: k_p_sc45__n_ti45   = 3977
  integer, parameter :: k_p_sc45__he4_ca42   = 3978
  integer, parameter :: k_he4_sc45__n_v48   = 3979
  integer, parameter :: k_he4_sc45__p_ti48   = 3980
  integer, parameter :: k_n_sc46__p_ca46   = 3981
  integer, parameter :: k_n_sc46__he4_k43   = 3982
  integer, parameter :: k_p_sc46__n_ti46   = 3983
  integer, parameter :: k_p_sc46__he4_ca43   = 3984
  integer, parameter :: k_he4_sc46__n_v49   = 3985
  integer, parameter :: k_he4_sc46__p_ti49   = 3986
  integer, parameter :: k_n_sc47__p_ca47   = 3987
  integer, parameter :: k_n_sc47__he4_k44   = 3988
  integer, parameter :: k_p_sc47__n_ti47   = 3989
  integer, parameter :: k_p_sc47__he4_ca44   = 3990
  integer, parameter :: k_he4_sc47__n_v50   = 3991
  integer, parameter :: k_he4_sc47__p_ti50   = 3992
  integer, parameter :: k_n_sc48__p_ca48   = 3993
  integer, parameter :: k_n_sc48__he4_k45   = 3994
  integer, parameter :: k_p_sc48__n_ti48   = 3995
  integer, parameter :: k_p_sc48__he4_ca45   = 3996
  integer, parameter :: k_he4_sc48__n_v51   = 3997
  integer, parameter :: k_he4_sc48__p_ti51   = 3998
  integer, parameter :: k_n_sc49__p_ca49   = 3999
  integer, parameter :: k_n_sc49__he4_k46   = 4000
  integer, parameter :: k_p_sc49__n_ti49   = 4001
  integer, parameter :: k_p_sc49__he4_ca46   = 4002
  integer, parameter :: k_he4_sc49__n_v52   = 4003
  integer, parameter :: k_he4_sc49__p_ti52   = 4004
  integer, parameter :: k_n_sc50__he4_k47   = 4005
  integer, parameter :: k_p_sc50__n_ti50   = 4006
  integer, parameter :: k_p_sc50__he4_ca47   = 4007
  integer, parameter :: k_he4_sc50__n_v53   = 4008
  integer, parameter :: k_he4_sc50__p_ti53   = 4009
  integer, parameter :: k_n_sc51__he4_k48   = 4010
  integer, parameter :: k_p_sc51__n_ti51   = 4011
  integer, parameter :: k_p_sc51__he4_ca48   = 4012
  integer, parameter :: k_he4_sc51__n_v54   = 4013
  integer, parameter :: k_n_ti41__p_sc41   = 4014
  integer, parameter :: k_n_ti41__he4_ca38   = 4015
  integer, parameter :: k_he4_ti41__n_cr44   = 4016
  integer, parameter :: k_he4_ti41__p_v44   = 4017
  integer, parameter :: k_n_ti42__p_sc42   = 4018
  integer, parameter :: k_n_ti42__he4_ca39   = 4019
  integer, parameter :: k_he4_ti42__n_cr45   = 4020
  integer, parameter :: k_he4_ti42__p_v45   = 4021
  integer, parameter :: k_n_ti43__p_sc43   = 4022
  integer, parameter :: k_n_ti43__he4_ca40   = 4023
  integer, parameter :: k_p_ti43__n_v43   = 4024
  integer, parameter :: k_p_ti43__he4_sc40   = 4025
  integer, parameter :: k_he4_ti43__n_cr46   = 4026
  integer, parameter :: k_he4_ti43__p_v46   = 4027
  integer, parameter :: k_n_ti44__p_sc44   = 4028
  integer, parameter :: k_n_ti44__he4_ca41   = 4029
  integer, parameter :: k_p_ti44__n_v44   = 4030
  integer, parameter :: k_p_ti44__he4_sc41   = 4031
  integer, parameter :: k_he4_ti44__n_cr47   = 4032
  integer, parameter :: k_he4_ti44__p_v47   = 4033
  integer, parameter :: k_n_ti45__p_sc45   = 4034
  integer, parameter :: k_n_ti45__he4_ca42   = 4035
  integer, parameter :: k_p_ti45__n_v45   = 4036
  integer, parameter :: k_p_ti45__he4_sc42   = 4037
  integer, parameter :: k_he4_ti45__n_cr48   = 4038
  integer, parameter :: k_he4_ti45__p_v48   = 4039
  integer, parameter :: k_n_ti46__p_sc46   = 4040
  integer, parameter :: k_n_ti46__he4_ca43   = 4041
  integer, parameter :: k_p_ti46__n_v46   = 4042
  integer, parameter :: k_p_ti46__he4_sc43   = 4043
  integer, parameter :: k_he4_ti46__n_cr49   = 4044
  integer, parameter :: k_he4_ti46__p_v49   = 4045
  integer, parameter :: k_n_ti47__p_sc47   = 4046
  integer, parameter :: k_n_ti47__he4_ca44   = 4047
  integer, parameter :: k_p_ti47__n_v47   = 4048
  integer, parameter :: k_p_ti47__he4_sc44   = 4049
  integer, parameter :: k_he4_ti47__n_cr50   = 4050
  integer, parameter :: k_he4_ti47__p_v50   = 4051
  integer, parameter :: k_n_ti48__p_sc48   = 4052
  integer, parameter :: k_n_ti48__he4_ca45   = 4053
  integer, parameter :: k_p_ti48__n_v48   = 4054
  integer, parameter :: k_p_ti48__he4_sc45   = 4055
  integer, parameter :: k_he4_ti48__n_cr51   = 4056
  integer, parameter :: k_he4_ti48__p_v51   = 4057
  integer, parameter :: k_n_ti49__p_sc49   = 4058
  integer, parameter :: k_n_ti49__he4_ca46   = 4059
  integer, parameter :: k_p_ti49__n_v49   = 4060
  integer, parameter :: k_p_ti49__he4_sc46   = 4061
  integer, parameter :: k_he4_ti49__n_cr52   = 4062
  integer, parameter :: k_he4_ti49__p_v52   = 4063
  integer, parameter :: k_n_ti50__p_sc50   = 4064
  integer, parameter :: k_n_ti50__he4_ca47   = 4065
  integer, parameter :: k_p_ti50__n_v50   = 4066
  integer, parameter :: k_p_ti50__he4_sc47   = 4067
  integer, parameter :: k_he4_ti50__n_cr53   = 4068
  integer, parameter :: k_he4_ti50__p_v53   = 4069
  integer, parameter :: k_n_ti51__p_sc51   = 4070
  integer, parameter :: k_n_ti51__he4_ca48   = 4071
  integer, parameter :: k_p_ti51__n_v51   = 4072
  integer, parameter :: k_p_ti51__he4_sc48   = 4073
  integer, parameter :: k_he4_ti51__n_cr54   = 4074
  integer, parameter :: k_he4_ti51__p_v54   = 4075
  integer, parameter :: k_n_ti52__he4_ca49   = 4076
  integer, parameter :: k_p_ti52__n_v52   = 4077
  integer, parameter :: k_p_ti52__he4_sc49   = 4078
  integer, parameter :: k_he4_ti52__n_cr55   = 4079
  integer, parameter :: k_he4_ti52__p_v55   = 4080
  integer, parameter :: k_p_ti53__n_v53   = 4081
  integer, parameter :: k_p_ti53__he4_sc50   = 4082
  integer, parameter :: k_he4_ti53__n_cr56   = 4083
  integer, parameter :: k_n_v43__p_ti43   = 4084
  integer, parameter :: k_n_v43__he4_sc40   = 4085
  integer, parameter :: k_he4_v43__n_mn46   = 4086
  integer, parameter :: k_he4_v43__p_cr46   = 4087
  integer, parameter :: k_n_v44__p_ti44   = 4088
  integer, parameter :: k_n_v44__he4_sc41   = 4089
  integer, parameter :: k_p_v44__n_cr44   = 4090
  integer, parameter :: k_p_v44__he4_ti41   = 4091
  integer, parameter :: k_he4_v44__n_mn47   = 4092
  integer, parameter :: k_he4_v44__p_cr47   = 4093
  integer, parameter :: k_n_v45__p_ti45   = 4094
  integer, parameter :: k_n_v45__he4_sc42   = 4095
  integer, parameter :: k_p_v45__n_cr45   = 4096
  integer, parameter :: k_p_v45__he4_ti42   = 4097
  integer, parameter :: k_he4_v45__n_mn48   = 4098
  integer, parameter :: k_he4_v45__p_cr48   = 4099
  integer, parameter :: k_n_v46__p_ti46   = 4100
  integer, parameter :: k_n_v46__he4_sc43   = 4101
  integer, parameter :: k_p_v46__n_cr46   = 4102
  integer, parameter :: k_p_v46__he4_ti43   = 4103
  integer, parameter :: k_he4_v46__n_mn49   = 4104
  integer, parameter :: k_he4_v46__p_cr49   = 4105
  integer, parameter :: k_n_v47__p_ti47   = 4106
  integer, parameter :: k_n_v47__he4_sc44   = 4107
  integer, parameter :: k_p_v47__n_cr47   = 4108
  integer, parameter :: k_p_v47__he4_ti44   = 4109
  integer, parameter :: k_he4_v47__n_mn50   = 4110
  integer, parameter :: k_he4_v47__p_cr50   = 4111
  integer, parameter :: k_n_v48__p_ti48   = 4112
  integer, parameter :: k_n_v48__he4_sc45   = 4113
  integer, parameter :: k_p_v48__n_cr48   = 4114
  integer, parameter :: k_p_v48__he4_ti45   = 4115
  integer, parameter :: k_he4_v48__n_mn51   = 4116
  integer, parameter :: k_he4_v48__p_cr51   = 4117
  integer, parameter :: k_n_v49__p_ti49   = 4118
  integer, parameter :: k_n_v49__he4_sc46   = 4119
  integer, parameter :: k_p_v49__n_cr49   = 4120
  integer, parameter :: k_p_v49__he4_ti46   = 4121
  integer, parameter :: k_he4_v49__n_mn52   = 4122
  integer, parameter :: k_he4_v49__p_cr52   = 4123
  integer, parameter :: k_n_v50__p_ti50   = 4124
  integer, parameter :: k_n_v50__he4_sc47   = 4125
  integer, parameter :: k_p_v50__n_cr50   = 4126
  integer, parameter :: k_p_v50__he4_ti47   = 4127
  integer, parameter :: k_he4_v50__n_mn53   = 4128
  integer, parameter :: k_he4_v50__p_cr53   = 4129
  integer, parameter :: k_n_v51__p_ti51   = 4130
  integer, parameter :: k_n_v51__he4_sc48   = 4131
  integer, parameter :: k_p_v51__n_cr51   = 4132
  integer, parameter :: k_p_v51__he4_ti48   = 4133
  integer, parameter :: k_he4_v51__n_mn54   = 4134
  integer, parameter :: k_he4_v51__p_cr54   = 4135
  integer, parameter :: k_n_v52__p_ti52   = 4136
  integer, parameter :: k_n_v52__he4_sc49   = 4137
  integer, parameter :: k_p_v52__n_cr52   = 4138
  integer, parameter :: k_p_v52__he4_ti49   = 4139
  integer, parameter :: k_he4_v52__n_mn55   = 4140
  integer, parameter :: k_he4_v52__p_cr55   = 4141
  integer, parameter :: k_n_v53__p_ti53   = 4142
  integer, parameter :: k_n_v53__he4_sc50   = 4143
  integer, parameter :: k_p_v53__n_cr53   = 4144
  integer, parameter :: k_p_v53__he4_ti50   = 4145
  integer, parameter :: k_he4_v53__n_mn56   = 4146
  integer, parameter :: k_he4_v53__p_cr56   = 4147
  integer, parameter :: k_n_v54__he4_sc51   = 4148
  integer, parameter :: k_p_v54__n_cr54   = 4149
  integer, parameter :: k_p_v54__he4_ti51   = 4150
  integer, parameter :: k_he4_v54__n_mn57   = 4151
  integer, parameter :: k_he4_v54__p_cr57   = 4152
  integer, parameter :: k_p_v55__n_cr55   = 4153
  integer, parameter :: k_p_v55__he4_ti52   = 4154
  integer, parameter :: k_he4_v55__n_mn58   = 4155
  integer, parameter :: k_he4_v55__p_cr58   = 4156
  integer, parameter :: k_n_cr44__p_v44   = 4157
  integer, parameter :: k_n_cr44__he4_ti41   = 4158
  integer, parameter :: k_he4_cr44__n_fe47   = 4159
  integer, parameter :: k_he4_cr44__p_mn47   = 4160
  integer, parameter :: k_n_cr45__p_v45   = 4161
  integer, parameter :: k_n_cr45__he4_ti42   = 4162
  integer, parameter :: k_he4_cr45__n_fe48   = 4163
  integer, parameter :: k_he4_cr45__p_mn48   = 4164
  integer, parameter :: k_n_cr46__p_v46   = 4165
  integer, parameter :: k_n_cr46__he4_ti43   = 4166
  integer, parameter :: k_p_cr46__n_mn46   = 4167
  integer, parameter :: k_p_cr46__he4_v43   = 4168
  integer, parameter :: k_he4_cr46__n_fe49   = 4169
  integer, parameter :: k_he4_cr46__p_mn49   = 4170
  integer, parameter :: k_n_cr47__p_v47   = 4171
  integer, parameter :: k_n_cr47__he4_ti44   = 4172
  integer, parameter :: k_p_cr47__n_mn47   = 4173
  integer, parameter :: k_p_cr47__he4_v44   = 4174
  integer, parameter :: k_he4_cr47__n_fe50   = 4175
  integer, parameter :: k_he4_cr47__p_mn50   = 4176
  integer, parameter :: k_n_cr48__p_v48   = 4177
  integer, parameter :: k_n_cr48__he4_ti45   = 4178
  integer, parameter :: k_p_cr48__n_mn48   = 4179
  integer, parameter :: k_p_cr48__he4_v45   = 4180
  integer, parameter :: k_he4_cr48__n_fe51   = 4181
  integer, parameter :: k_he4_cr48__p_mn51   = 4182
  integer, parameter :: k_n_cr49__p_v49   = 4183
  integer, parameter :: k_n_cr49__he4_ti46   = 4184
  integer, parameter :: k_p_cr49__n_mn49   = 4185
  integer, parameter :: k_p_cr49__he4_v46   = 4186
  integer, parameter :: k_he4_cr49__n_fe52   = 4187
  integer, parameter :: k_he4_cr49__p_mn52   = 4188
  integer, parameter :: k_n_cr50__p_v50   = 4189
  integer, parameter :: k_n_cr50__he4_ti47   = 4190
  integer, parameter :: k_p_cr50__n_mn50   = 4191
  integer, parameter :: k_p_cr50__he4_v47   = 4192
  integer, parameter :: k_he4_cr50__n_fe53   = 4193
  integer, parameter :: k_he4_cr50__p_mn53   = 4194
  integer, parameter :: k_n_cr51__p_v51   = 4195
  integer, parameter :: k_n_cr51__he4_ti48   = 4196
  integer, parameter :: k_p_cr51__n_mn51   = 4197
  integer, parameter :: k_p_cr51__he4_v48   = 4198
  integer, parameter :: k_he4_cr51__n_fe54   = 4199
  integer, parameter :: k_he4_cr51__p_mn54   = 4200
  integer, parameter :: k_n_cr52__p_v52   = 4201
  integer, parameter :: k_n_cr52__he4_ti49   = 4202
  integer, parameter :: k_p_cr52__n_mn52   = 4203
  integer, parameter :: k_p_cr52__he4_v49   = 4204
  integer, parameter :: k_he4_cr52__n_fe55   = 4205
  integer, parameter :: k_he4_cr52__p_mn55   = 4206
  integer, parameter :: k_n_cr53__p_v53   = 4207
  integer, parameter :: k_n_cr53__he4_ti50   = 4208
  integer, parameter :: k_p_cr53__n_mn53   = 4209
  integer, parameter :: k_p_cr53__he4_v50   = 4210
  integer, parameter :: k_he4_cr53__n_fe56   = 4211
  integer, parameter :: k_he4_cr53__p_mn56   = 4212
  integer, parameter :: k_n_cr54__p_v54   = 4213
  integer, parameter :: k_n_cr54__he4_ti51   = 4214
  integer, parameter :: k_p_cr54__n_mn54   = 4215
  integer, parameter :: k_p_cr54__he4_v51   = 4216
  integer, parameter :: k_he4_cr54__n_fe57   = 4217
  integer, parameter :: k_he4_cr54__p_mn57   = 4218
  integer, parameter :: k_n_cr55__p_v55   = 4219
  integer, parameter :: k_n_cr55__he4_ti52   = 4220
  integer, parameter :: k_p_cr55__n_mn55   = 4221
  integer, parameter :: k_p_cr55__he4_v52   = 4222
  integer, parameter :: k_he4_cr55__n_fe58   = 4223
  integer, parameter :: k_he4_cr55__p_mn58   = 4224
  integer, parameter :: k_n_cr56__he4_ti53   = 4225
  integer, parameter :: k_p_cr56__n_mn56   = 4226
  integer, parameter :: k_p_cr56__he4_v53   = 4227
  integer, parameter :: k_he4_cr56__n_fe59   = 4228
  integer, parameter :: k_he4_cr56__p_mn59   = 4229
  integer, parameter :: k_p_cr57__n_mn57   = 4230
  integer, parameter :: k_p_cr57__he4_v54   = 4231
  integer, parameter :: k_he4_cr57__n_fe60   = 4232
  integer, parameter :: k_he4_cr57__p_mn60   = 4233
  integer, parameter :: k_p_cr58__n_mn58   = 4234
  integer, parameter :: k_p_cr58__he4_v55   = 4235
  integer, parameter :: k_he4_cr58__n_fe61   = 4236
  integer, parameter :: k_he4_cr58__p_mn61   = 4237
  integer, parameter :: k_n_mn46__p_cr46   = 4238
  integer, parameter :: k_n_mn46__he4_v43   = 4239
  integer, parameter :: k_he4_mn46__p_fe49   = 4240
  integer, parameter :: k_n_mn47__p_cr47   = 4241
  integer, parameter :: k_n_mn47__he4_v44   = 4242
  integer, parameter :: k_p_mn47__n_fe47   = 4243
  integer, parameter :: k_p_mn47__he4_cr44   = 4244
  integer, parameter :: k_he4_mn47__n_co50   = 4245
  integer, parameter :: k_he4_mn47__p_fe50   = 4246
  integer, parameter :: k_n_mn48__p_cr48   = 4247
  integer, parameter :: k_n_mn48__he4_v45   = 4248
  integer, parameter :: k_p_mn48__n_fe48   = 4249
  integer, parameter :: k_p_mn48__he4_cr45   = 4250
  integer, parameter :: k_he4_mn48__n_co51   = 4251
  integer, parameter :: k_he4_mn48__p_fe51   = 4252
  integer, parameter :: k_n_mn49__p_cr49   = 4253
  integer, parameter :: k_n_mn49__he4_v46   = 4254
  integer, parameter :: k_p_mn49__n_fe49   = 4255
  integer, parameter :: k_p_mn49__he4_cr46   = 4256
  integer, parameter :: k_he4_mn49__n_co52   = 4257
  integer, parameter :: k_he4_mn49__p_fe52   = 4258
  integer, parameter :: k_n_mn50__p_cr50   = 4259
  integer, parameter :: k_n_mn50__he4_v47   = 4260
  integer, parameter :: k_p_mn50__n_fe50   = 4261
  integer, parameter :: k_p_mn50__he4_cr47   = 4262
  integer, parameter :: k_he4_mn50__n_co53   = 4263
  integer, parameter :: k_he4_mn50__p_fe53   = 4264
  integer, parameter :: k_n_mn51__p_cr51   = 4265
  integer, parameter :: k_n_mn51__he4_v48   = 4266
  integer, parameter :: k_p_mn51__n_fe51   = 4267
  integer, parameter :: k_p_mn51__he4_cr48   = 4268
  integer, parameter :: k_he4_mn51__n_co54   = 4269
  integer, parameter :: k_he4_mn51__p_fe54   = 4270
  integer, parameter :: k_n_mn52__p_cr52   = 4271
  integer, parameter :: k_n_mn52__he4_v49   = 4272
  integer, parameter :: k_p_mn52__n_fe52   = 4273
  integer, parameter :: k_p_mn52__he4_cr49   = 4274
  integer, parameter :: k_he4_mn52__n_co55   = 4275
  integer, parameter :: k_he4_mn52__p_fe55   = 4276
  integer, parameter :: k_n_mn53__p_cr53   = 4277
  integer, parameter :: k_n_mn53__he4_v50   = 4278
  integer, parameter :: k_p_mn53__n_fe53   = 4279
  integer, parameter :: k_p_mn53__he4_cr50   = 4280
  integer, parameter :: k_he4_mn53__n_co56   = 4281
  integer, parameter :: k_he4_mn53__p_fe56   = 4282
  integer, parameter :: k_n_mn54__p_cr54   = 4283
  integer, parameter :: k_n_mn54__he4_v51   = 4284
  integer, parameter :: k_p_mn54__n_fe54   = 4285
  integer, parameter :: k_p_mn54__he4_cr51   = 4286
  integer, parameter :: k_he4_mn54__n_co57   = 4287
  integer, parameter :: k_he4_mn54__p_fe57   = 4288
  integer, parameter :: k_n_mn55__p_cr55   = 4289
  integer, parameter :: k_n_mn55__he4_v52   = 4290
  integer, parameter :: k_p_mn55__n_fe55   = 4291
  integer, parameter :: k_p_mn55__he4_cr52   = 4292
  integer, parameter :: k_he4_mn55__n_co58   = 4293
  integer, parameter :: k_he4_mn55__p_fe58   = 4294
  integer, parameter :: k_n_mn56__p_cr56   = 4295
  integer, parameter :: k_n_mn56__he4_v53   = 4296
  integer, parameter :: k_p_mn56__n_fe56   = 4297
  integer, parameter :: k_p_mn56__he4_cr53   = 4298
  integer, parameter :: k_he4_mn56__n_co59   = 4299
  integer, parameter :: k_he4_mn56__p_fe59   = 4300
  integer, parameter :: k_n_mn57__p_cr57   = 4301
  integer, parameter :: k_n_mn57__he4_v54   = 4302
  integer, parameter :: k_p_mn57__n_fe57   = 4303
  integer, parameter :: k_p_mn57__he4_cr54   = 4304
  integer, parameter :: k_he4_mn57__n_co60   = 4305
  integer, parameter :: k_he4_mn57__p_fe60   = 4306
  integer, parameter :: k_n_mn58__p_cr58   = 4307
  integer, parameter :: k_n_mn58__he4_v55   = 4308
  integer, parameter :: k_p_mn58__n_fe58   = 4309
  integer, parameter :: k_p_mn58__he4_cr55   = 4310
  integer, parameter :: k_he4_mn58__n_co61   = 4311
  integer, parameter :: k_he4_mn58__p_fe61   = 4312
  integer, parameter :: k_p_mn59__n_fe59   = 4313
  integer, parameter :: k_p_mn59__he4_cr56   = 4314
  integer, parameter :: k_he4_mn59__n_co62   = 4315
  integer, parameter :: k_he4_mn59__p_fe62   = 4316
  integer, parameter :: k_p_mn60__n_fe60   = 4317
  integer, parameter :: k_p_mn60__he4_cr57   = 4318
  integer, parameter :: k_he4_mn60__n_co63   = 4319
  integer, parameter :: k_he4_mn60__p_fe63   = 4320
  integer, parameter :: k_p_mn61__n_fe61   = 4321
  integer, parameter :: k_p_mn61__he4_cr58   = 4322
  integer, parameter :: k_he4_mn61__n_co64   = 4323
  integer, parameter :: k_he4_mn61__p_fe64   = 4324
  integer, parameter :: k_n_fe47__p_mn47   = 4325
  integer, parameter :: k_n_fe47__he4_cr44   = 4326
  integer, parameter :: k_he4_fe47__p_co50   = 4327
  integer, parameter :: k_n_fe48__p_mn48   = 4328
  integer, parameter :: k_n_fe48__he4_cr45   = 4329
  integer, parameter :: k_he4_fe48__n_ni51   = 4330
  integer, parameter :: k_he4_fe48__p_co51   = 4331
  integer, parameter :: k_n_fe49__p_mn49   = 4332
  integer, parameter :: k_n_fe49__he4_cr46   = 4333
  integer, parameter :: k_p_fe49__he4_mn46   = 4334
  integer, parameter :: k_he4_fe49__n_ni52   = 4335
  integer, parameter :: k_he4_fe49__p_co52   = 4336
  integer, parameter :: k_n_fe50__p_mn50   = 4337
  integer, parameter :: k_n_fe50__he4_cr47   = 4338
  integer, parameter :: k_p_fe50__n_co50   = 4339
  integer, parameter :: k_p_fe50__he4_mn47   = 4340
  integer, parameter :: k_he4_fe50__n_ni53   = 4341
  integer, parameter :: k_he4_fe50__p_co53   = 4342
  integer, parameter :: k_n_fe51__p_mn51   = 4343
  integer, parameter :: k_n_fe51__he4_cr48   = 4344
  integer, parameter :: k_p_fe51__n_co51   = 4345
  integer, parameter :: k_p_fe51__he4_mn48   = 4346
  integer, parameter :: k_he4_fe51__n_ni54   = 4347
  integer, parameter :: k_he4_fe51__p_co54   = 4348
  integer, parameter :: k_n_fe52__p_mn52   = 4349
  integer, parameter :: k_n_fe52__he4_cr49   = 4350
  integer, parameter :: k_p_fe52__n_co52   = 4351
  integer, parameter :: k_p_fe52__he4_mn49   = 4352
  integer, parameter :: k_he4_fe52__n_ni55   = 4353
  integer, parameter :: k_he4_fe52__p_co55   = 4354
  integer, parameter :: k_n_fe53__p_mn53   = 4355
  integer, parameter :: k_n_fe53__he4_cr50   = 4356
  integer, parameter :: k_p_fe53__n_co53   = 4357
  integer, parameter :: k_p_fe53__he4_mn50   = 4358
  integer, parameter :: k_he4_fe53__n_ni56   = 4359
  integer, parameter :: k_he4_fe53__p_co56   = 4360
  integer, parameter :: k_n_fe54__p_mn54   = 4361
  integer, parameter :: k_n_fe54__he4_cr51   = 4362
  integer, parameter :: k_p_fe54__n_co54   = 4363
  integer, parameter :: k_p_fe54__he4_mn51   = 4364
  integer, parameter :: k_he4_fe54__n_ni57   = 4365
  integer, parameter :: k_he4_fe54__p_co57   = 4366
  integer, parameter :: k_n_fe55__p_mn55   = 4367
  integer, parameter :: k_n_fe55__he4_cr52   = 4368
  integer, parameter :: k_p_fe55__n_co55   = 4369
  integer, parameter :: k_p_fe55__he4_mn52   = 4370
  integer, parameter :: k_he4_fe55__n_ni58   = 4371
  integer, parameter :: k_he4_fe55__p_co58   = 4372
  integer, parameter :: k_n_fe56__p_mn56   = 4373
  integer, parameter :: k_n_fe56__he4_cr53   = 4374
  integer, parameter :: k_p_fe56__n_co56   = 4375
  integer, parameter :: k_p_fe56__he4_mn53   = 4376
  integer, parameter :: k_he4_fe56__n_ni59   = 4377
  integer, parameter :: k_he4_fe56__p_co59   = 4378
  integer, parameter :: k_n_fe57__p_mn57   = 4379
  integer, parameter :: k_n_fe57__he4_cr54   = 4380
  integer, parameter :: k_p_fe57__n_co57   = 4381
  integer, parameter :: k_p_fe57__he4_mn54   = 4382
  integer, parameter :: k_he4_fe57__n_ni60   = 4383
  integer, parameter :: k_he4_fe57__p_co60   = 4384
  integer, parameter :: k_n_fe58__p_mn58   = 4385
  integer, parameter :: k_n_fe58__he4_cr55   = 4386
  integer, parameter :: k_p_fe58__n_co58   = 4387
  integer, parameter :: k_p_fe58__he4_mn55   = 4388
  integer, parameter :: k_he4_fe58__n_ni61   = 4389
  integer, parameter :: k_he4_fe58__p_co61   = 4390
  integer, parameter :: k_n_fe59__p_mn59   = 4391
  integer, parameter :: k_n_fe59__he4_cr56   = 4392
  integer, parameter :: k_p_fe59__n_co59   = 4393
  integer, parameter :: k_p_fe59__he4_mn56   = 4394
  integer, parameter :: k_he4_fe59__n_ni62   = 4395
  integer, parameter :: k_he4_fe59__p_co62   = 4396
  integer, parameter :: k_n_fe60__p_mn60   = 4397
  integer, parameter :: k_n_fe60__he4_cr57   = 4398
  integer, parameter :: k_p_fe60__n_co60   = 4399
  integer, parameter :: k_p_fe60__he4_mn57   = 4400
  integer, parameter :: k_he4_fe60__n_ni63   = 4401
  integer, parameter :: k_he4_fe60__p_co63   = 4402
  integer, parameter :: k_n_fe61__p_mn61   = 4403
  integer, parameter :: k_n_fe61__he4_cr58   = 4404
  integer, parameter :: k_p_fe61__n_co61   = 4405
  integer, parameter :: k_p_fe61__he4_mn58   = 4406
  integer, parameter :: k_he4_fe61__n_ni64   = 4407
  integer, parameter :: k_he4_fe61__p_co64   = 4408
  integer, parameter :: k_p_fe62__n_co62   = 4409
  integer, parameter :: k_p_fe62__he4_mn59   = 4410
  integer, parameter :: k_he4_fe62__n_ni65   = 4411
  integer, parameter :: k_he4_fe62__p_co65   = 4412
  integer, parameter :: k_p_fe63__n_co63   = 4413
  integer, parameter :: k_p_fe63__he4_mn60   = 4414
  integer, parameter :: k_he4_fe63__n_ni66   = 4415
  integer, parameter :: k_he4_fe63__p_co66   = 4416
  integer, parameter :: k_p_fe64__n_co64   = 4417
  integer, parameter :: k_p_fe64__he4_mn61   = 4418
  integer, parameter :: k_he4_fe64__n_ni67   = 4419
  integer, parameter :: k_he4_fe64__p_co67   = 4420
  integer, parameter :: k_p_fe65__n_co65   = 4421
  integer, parameter :: k_he4_fe65__n_ni68   = 4422
  integer, parameter :: k_p_fe66__n_co66   = 4423
  integer, parameter :: k_n_co50__p_fe50   = 4424
  integer, parameter :: k_n_co50__he4_mn47   = 4425
  integer, parameter :: k_p_co50__he4_fe47   = 4426
  integer, parameter :: k_he4_co50__p_ni53   = 4427
  integer, parameter :: k_n_co51__p_fe51   = 4428
  integer, parameter :: k_n_co51__he4_mn48   = 4429
  integer, parameter :: k_p_co51__n_ni51   = 4430
  integer, parameter :: k_p_co51__he4_fe48   = 4431
  integer, parameter :: k_he4_co51__p_ni54   = 4432
  integer, parameter :: k_n_co52__p_fe52   = 4433
  integer, parameter :: k_n_co52__he4_mn49   = 4434
  integer, parameter :: k_p_co52__n_ni52   = 4435
  integer, parameter :: k_p_co52__he4_fe49   = 4436
  integer, parameter :: k_he4_co52__n_cu55   = 4437
  integer, parameter :: k_he4_co52__p_ni55   = 4438
  integer, parameter :: k_n_co53__p_fe53   = 4439
  integer, parameter :: k_n_co53__he4_mn50   = 4440
  integer, parameter :: k_p_co53__n_ni53   = 4441
  integer, parameter :: k_p_co53__he4_fe50   = 4442
  integer, parameter :: k_he4_co53__n_cu56   = 4443
  integer, parameter :: k_he4_co53__p_ni56   = 4444
  integer, parameter :: k_n_co54__p_fe54   = 4445
  integer, parameter :: k_n_co54__he4_mn51   = 4446
  integer, parameter :: k_p_co54__n_ni54   = 4447
  integer, parameter :: k_p_co54__he4_fe51   = 4448
  integer, parameter :: k_he4_co54__n_cu57   = 4449
  integer, parameter :: k_he4_co54__p_ni57   = 4450
  integer, parameter :: k_n_co55__p_fe55   = 4451
  integer, parameter :: k_n_co55__he4_mn52   = 4452
  integer, parameter :: k_p_co55__n_ni55   = 4453
  integer, parameter :: k_p_co55__he4_fe52   = 4454
  integer, parameter :: k_he4_co55__n_cu58   = 4455
  integer, parameter :: k_he4_co55__p_ni58   = 4456
  integer, parameter :: k_n_co56__p_fe56   = 4457
  integer, parameter :: k_n_co56__he4_mn53   = 4458
  integer, parameter :: k_p_co56__n_ni56   = 4459
  integer, parameter :: k_p_co56__he4_fe53   = 4460
  integer, parameter :: k_he4_co56__n_cu59   = 4461
  integer, parameter :: k_he4_co56__p_ni59   = 4462
  integer, parameter :: k_n_co57__p_fe57   = 4463
  integer, parameter :: k_n_co57__he4_mn54   = 4464
  integer, parameter :: k_p_co57__n_ni57   = 4465
  integer, parameter :: k_p_co57__he4_fe54   = 4466
  integer, parameter :: k_he4_co57__n_cu60   = 4467
  integer, parameter :: k_he4_co57__p_ni60   = 4468
  integer, parameter :: k_n_co58__p_fe58   = 4469
  integer, parameter :: k_n_co58__he4_mn55   = 4470
  integer, parameter :: k_p_co58__n_ni58   = 4471
  integer, parameter :: k_p_co58__he4_fe55   = 4472
  integer, parameter :: k_he4_co58__n_cu61   = 4473
  integer, parameter :: k_he4_co58__p_ni61   = 4474
  integer, parameter :: k_n_co59__p_fe59   = 4475
  integer, parameter :: k_n_co59__he4_mn56   = 4476
  integer, parameter :: k_p_co59__n_ni59   = 4477
  integer, parameter :: k_p_co59__he4_fe56   = 4478
  integer, parameter :: k_he4_co59__n_cu62   = 4479
  integer, parameter :: k_he4_co59__p_ni62   = 4480
  integer, parameter :: k_n_co60__p_fe60   = 4481
  integer, parameter :: k_n_co60__he4_mn57   = 4482
  integer, parameter :: k_p_co60__n_ni60   = 4483
  integer, parameter :: k_p_co60__he4_fe57   = 4484
  integer, parameter :: k_he4_co60__n_cu63   = 4485
  integer, parameter :: k_he4_co60__p_ni63   = 4486
  integer, parameter :: k_n_co61__p_fe61   = 4487
  integer, parameter :: k_n_co61__he4_mn58   = 4488
  integer, parameter :: k_p_co61__n_ni61   = 4489
  integer, parameter :: k_p_co61__he4_fe58   = 4490
  integer, parameter :: k_he4_co61__n_cu64   = 4491
  integer, parameter :: k_he4_co61__p_ni64   = 4492
  integer, parameter :: k_n_co62__p_fe62   = 4493
  integer, parameter :: k_n_co62__he4_mn59   = 4494
  integer, parameter :: k_p_co62__n_ni62   = 4495
  integer, parameter :: k_p_co62__he4_fe59   = 4496
  integer, parameter :: k_he4_co62__n_cu65   = 4497
  integer, parameter :: k_he4_co62__p_ni65   = 4498
  integer, parameter :: k_n_co63__p_fe63   = 4499
  integer, parameter :: k_n_co63__he4_mn60   = 4500
  integer, parameter :: k_p_co63__n_ni63   = 4501
  integer, parameter :: k_p_co63__he4_fe60   = 4502
  integer, parameter :: k_he4_co63__n_cu66   = 4503
  integer, parameter :: k_he4_co63__p_ni66   = 4504
  integer, parameter :: k_n_co64__p_fe64   = 4505
  integer, parameter :: k_n_co64__he4_mn61   = 4506
  integer, parameter :: k_p_co64__n_ni64   = 4507
  integer, parameter :: k_p_co64__he4_fe61   = 4508
  integer, parameter :: k_he4_co64__n_cu67   = 4509
  integer, parameter :: k_he4_co64__p_ni67   = 4510
  integer, parameter :: k_n_co65__p_fe65   = 4511
  integer, parameter :: k_p_co65__n_ni65   = 4512
  integer, parameter :: k_p_co65__he4_fe62   = 4513
  integer, parameter :: k_he4_co65__n_cu68   = 4514
  integer, parameter :: k_he4_co65__p_ni68   = 4515
  integer, parameter :: k_n_co66__p_fe66   = 4516
  integer, parameter :: k_p_co66__n_ni66   = 4517
  integer, parameter :: k_p_co66__he4_fe63   = 4518
  integer, parameter :: k_he4_co66__n_cu69   = 4519
  integer, parameter :: k_p_co67__n_ni67   = 4520
  integer, parameter :: k_p_co67__he4_fe64   = 4521
  integer, parameter :: k_n_ni51__p_co51   = 4522
  integer, parameter :: k_n_ni51__he4_fe48   = 4523
  integer, parameter :: k_n_ni52__p_co52   = 4524
  integer, parameter :: k_n_ni52__he4_fe49   = 4525
  integer, parameter :: k_he4_ni52__p_cu55   = 4526
  integer, parameter :: k_n_ni53__p_co53   = 4527
  integer, parameter :: k_n_ni53__he4_fe50   = 4528
  integer, parameter :: k_p_ni53__he4_co50   = 4529
  integer, parameter :: k_he4_ni53__p_cu56   = 4530
  integer, parameter :: k_n_ni54__p_co54   = 4531
  integer, parameter :: k_n_ni54__he4_fe51   = 4532
  integer, parameter :: k_p_ni54__he4_co51   = 4533
  integer, parameter :: k_he4_ni54__n_zn57   = 4534
  integer, parameter :: k_he4_ni54__p_cu57   = 4535
  integer, parameter :: k_n_ni55__p_co55   = 4536
  integer, parameter :: k_n_ni55__he4_fe52   = 4537
  integer, parameter :: k_p_ni55__n_cu55   = 4538
  integer, parameter :: k_p_ni55__he4_co52   = 4539
  integer, parameter :: k_he4_ni55__n_zn58   = 4540
  integer, parameter :: k_he4_ni55__p_cu58   = 4541
  integer, parameter :: k_n_ni56__p_co56   = 4542
  integer, parameter :: k_n_ni56__he4_fe53   = 4543
  integer, parameter :: k_p_ni56__n_cu56   = 4544
  integer, parameter :: k_p_ni56__he4_co53   = 4545
  integer, parameter :: k_he4_ni56__n_zn59   = 4546
  integer, parameter :: k_he4_ni56__p_cu59   = 4547
  integer, parameter :: k_n_ni57__p_co57   = 4548
  integer, parameter :: k_n_ni57__he4_fe54   = 4549
  integer, parameter :: k_p_ni57__n_cu57   = 4550
  integer, parameter :: k_p_ni57__he4_co54   = 4551
  integer, parameter :: k_he4_ni57__n_zn60   = 4552
  integer, parameter :: k_he4_ni57__p_cu60   = 4553
  integer, parameter :: k_n_ni58__p_co58   = 4554
  integer, parameter :: k_n_ni58__he4_fe55   = 4555
  integer, parameter :: k_p_ni58__n_cu58   = 4556
  integer, parameter :: k_p_ni58__he4_co55   = 4557
  integer, parameter :: k_he4_ni58__n_zn61   = 4558
  integer, parameter :: k_he4_ni58__p_cu61   = 4559
  integer, parameter :: k_n_ni59__p_co59   = 4560
  integer, parameter :: k_n_ni59__he4_fe56   = 4561
  integer, parameter :: k_p_ni59__n_cu59   = 4562
  integer, parameter :: k_p_ni59__he4_co56   = 4563
  integer, parameter :: k_he4_ni59__n_zn62   = 4564
  integer, parameter :: k_he4_ni59__p_cu62   = 4565
  integer, parameter :: k_n_ni60__p_co60   = 4566
  integer, parameter :: k_n_ni60__he4_fe57   = 4567
  integer, parameter :: k_p_ni60__n_cu60   = 4568
  integer, parameter :: k_p_ni60__he4_co57   = 4569
  integer, parameter :: k_he4_ni60__n_zn63   = 4570
  integer, parameter :: k_he4_ni60__p_cu63   = 4571
  integer, parameter :: k_n_ni61__p_co61   = 4572
  integer, parameter :: k_n_ni61__he4_fe58   = 4573
  integer, parameter :: k_p_ni61__n_cu61   = 4574
  integer, parameter :: k_p_ni61__he4_co58   = 4575
  integer, parameter :: k_he4_ni61__n_zn64   = 4576
  integer, parameter :: k_he4_ni61__p_cu64   = 4577
  integer, parameter :: k_n_ni62__p_co62   = 4578
  integer, parameter :: k_n_ni62__he4_fe59   = 4579
  integer, parameter :: k_p_ni62__n_cu62   = 4580
  integer, parameter :: k_p_ni62__he4_co59   = 4581
  integer, parameter :: k_he4_ni62__n_zn65   = 4582
  integer, parameter :: k_he4_ni62__p_cu65   = 4583
  integer, parameter :: k_n_ni63__p_co63   = 4584
  integer, parameter :: k_n_ni63__he4_fe60   = 4585
  integer, parameter :: k_p_ni63__n_cu63   = 4586
  integer, parameter :: k_p_ni63__he4_co60   = 4587
  integer, parameter :: k_he4_ni63__n_zn66   = 4588
  integer, parameter :: k_he4_ni63__p_cu66   = 4589
  integer, parameter :: k_n_ni64__p_co64   = 4590
  integer, parameter :: k_n_ni64__he4_fe61   = 4591
  integer, parameter :: k_p_ni64__n_cu64   = 4592
  integer, parameter :: k_p_ni64__he4_co61   = 4593
  integer, parameter :: k_he4_ni64__n_zn67   = 4594
  integer, parameter :: k_he4_ni64__p_cu67   = 4595
  integer, parameter :: k_n_ni65__p_co65   = 4596
  integer, parameter :: k_n_ni65__he4_fe62   = 4597
  integer, parameter :: k_p_ni65__n_cu65   = 4598
  integer, parameter :: k_p_ni65__he4_co62   = 4599
  integer, parameter :: k_he4_ni65__n_zn68   = 4600
  integer, parameter :: k_he4_ni65__p_cu68   = 4601
  integer, parameter :: k_n_ni66__p_co66   = 4602
  integer, parameter :: k_n_ni66__he4_fe63   = 4603
  integer, parameter :: k_p_ni66__n_cu66   = 4604
  integer, parameter :: k_p_ni66__he4_co63   = 4605
  integer, parameter :: k_he4_ni66__n_zn69   = 4606
  integer, parameter :: k_he4_ni66__p_cu69   = 4607
  integer, parameter :: k_n_ni67__p_co67   = 4608
  integer, parameter :: k_n_ni67__he4_fe64   = 4609
  integer, parameter :: k_p_ni67__n_cu67   = 4610
  integer, parameter :: k_p_ni67__he4_co64   = 4611
  integer, parameter :: k_he4_ni67__n_zn70   = 4612
  integer, parameter :: k_n_ni68__he4_fe65   = 4613
  integer, parameter :: k_p_ni68__n_cu68   = 4614
  integer, parameter :: k_p_ni68__he4_co65   = 4615
  integer, parameter :: k_he4_ni68__n_zn71   = 4616
  integer, parameter :: k_n_cu55__p_ni55   = 4617
  integer, parameter :: k_n_cu55__he4_co52   = 4618
  integer, parameter :: k_p_cu55__he4_ni52   = 4619
  integer, parameter :: k_he4_cu55__p_zn58   = 4620
  integer, parameter :: k_n_cu56__p_ni56   = 4621
  integer, parameter :: k_n_cu56__he4_co53   = 4622
  integer, parameter :: k_p_cu56__he4_ni53   = 4623
  integer, parameter :: k_he4_cu56__n_ga59   = 4624
  integer, parameter :: k_he4_cu56__p_zn59   = 4625
  integer, parameter :: k_n_cu57__p_ni57   = 4626
  integer, parameter :: k_n_cu57__he4_co54   = 4627
  integer, parameter :: k_p_cu57__n_zn57   = 4628
  integer, parameter :: k_p_cu57__he4_ni54   = 4629
  integer, parameter :: k_he4_cu57__n_ga60   = 4630
  integer, parameter :: k_he4_cu57__p_zn60   = 4631
  integer, parameter :: k_n_cu58__p_ni58   = 4632
  integer, parameter :: k_n_cu58__he4_co55   = 4633
  integer, parameter :: k_p_cu58__n_zn58   = 4634
  integer, parameter :: k_p_cu58__he4_ni55   = 4635
  integer, parameter :: k_he4_cu58__n_ga61   = 4636
  integer, parameter :: k_he4_cu58__p_zn61   = 4637
  integer, parameter :: k_n_cu59__p_ni59   = 4638
  integer, parameter :: k_n_cu59__he4_co56   = 4639
  integer, parameter :: k_p_cu59__n_zn59   = 4640
  integer, parameter :: k_p_cu59__he4_ni56   = 4641
  integer, parameter :: k_he4_cu59__n_ga62   = 4642
  integer, parameter :: k_he4_cu59__p_zn62   = 4643
  integer, parameter :: k_n_cu60__p_ni60   = 4644
  integer, parameter :: k_n_cu60__he4_co57   = 4645
  integer, parameter :: k_p_cu60__n_zn60   = 4646
  integer, parameter :: k_p_cu60__he4_ni57   = 4647
  integer, parameter :: k_he4_cu60__n_ga63   = 4648
  integer, parameter :: k_he4_cu60__p_zn63   = 4649
  integer, parameter :: k_n_cu61__p_ni61   = 4650
  integer, parameter :: k_n_cu61__he4_co58   = 4651
  integer, parameter :: k_p_cu61__n_zn61   = 4652
  integer, parameter :: k_p_cu61__he4_ni58   = 4653
  integer, parameter :: k_he4_cu61__n_ga64   = 4654
  integer, parameter :: k_he4_cu61__p_zn64   = 4655
  integer, parameter :: k_n_cu62__p_ni62   = 4656
  integer, parameter :: k_n_cu62__he4_co59   = 4657
  integer, parameter :: k_p_cu62__n_zn62   = 4658
  integer, parameter :: k_p_cu62__he4_ni59   = 4659
  integer, parameter :: k_he4_cu62__n_ga65   = 4660
  integer, parameter :: k_he4_cu62__p_zn65   = 4661
  integer, parameter :: k_n_cu63__p_ni63   = 4662
  integer, parameter :: k_n_cu63__he4_co60   = 4663
  integer, parameter :: k_p_cu63__n_zn63   = 4664
  integer, parameter :: k_p_cu63__he4_ni60   = 4665
  integer, parameter :: k_he4_cu63__n_ga66   = 4666
  integer, parameter :: k_he4_cu63__p_zn66   = 4667
  integer, parameter :: k_n_cu64__p_ni64   = 4668
  integer, parameter :: k_n_cu64__he4_co61   = 4669
  integer, parameter :: k_p_cu64__n_zn64   = 4670
  integer, parameter :: k_p_cu64__he4_ni61   = 4671
  integer, parameter :: k_he4_cu64__n_ga67   = 4672
  integer, parameter :: k_he4_cu64__p_zn67   = 4673
  integer, parameter :: k_n_cu65__p_ni65   = 4674
  integer, parameter :: k_n_cu65__he4_co62   = 4675
  integer, parameter :: k_p_cu65__n_zn65   = 4676
  integer, parameter :: k_p_cu65__he4_ni62   = 4677
  integer, parameter :: k_he4_cu65__n_ga68   = 4678
  integer, parameter :: k_he4_cu65__p_zn68   = 4679
  integer, parameter :: k_n_cu66__p_ni66   = 4680
  integer, parameter :: k_n_cu66__he4_co63   = 4681
  integer, parameter :: k_p_cu66__n_zn66   = 4682
  integer, parameter :: k_p_cu66__he4_ni63   = 4683
  integer, parameter :: k_he4_cu66__n_ga69   = 4684
  integer, parameter :: k_he4_cu66__p_zn69   = 4685
  integer, parameter :: k_n_cu67__p_ni67   = 4686
  integer, parameter :: k_n_cu67__he4_co64   = 4687
  integer, parameter :: k_p_cu67__n_zn67   = 4688
  integer, parameter :: k_p_cu67__he4_ni64   = 4689
  integer, parameter :: k_he4_cu67__n_ga70   = 4690
  integer, parameter :: k_he4_cu67__p_zn70   = 4691
  integer, parameter :: k_n_cu68__p_ni68   = 4692
  integer, parameter :: k_n_cu68__he4_co65   = 4693
  integer, parameter :: k_p_cu68__n_zn68   = 4694
  integer, parameter :: k_p_cu68__he4_ni65   = 4695
  integer, parameter :: k_he4_cu68__n_ga71   = 4696
  integer, parameter :: k_he4_cu68__p_zn71   = 4697
  integer, parameter :: k_n_cu69__he4_co66   = 4698
  integer, parameter :: k_p_cu69__n_zn69   = 4699
  integer, parameter :: k_p_cu69__he4_ni66   = 4700
  integer, parameter :: k_he4_cu69__n_ga72   = 4701
  integer, parameter :: k_he4_cu69__p_zn72   = 4702
  integer, parameter :: k_n_zn57__p_cu57   = 4703
  integer, parameter :: k_n_zn57__he4_ni54   = 4704
  integer, parameter :: k_he4_zn57__p_ga60   = 4705
  integer, parameter :: k_n_zn58__p_cu58   = 4706
  integer, parameter :: k_n_zn58__he4_ni55   = 4707
  integer, parameter :: k_p_zn58__he4_cu55   = 4708
  integer, parameter :: k_he4_zn58__p_ga61   = 4709
  integer, parameter :: k_n_zn59__p_cu59   = 4710
  integer, parameter :: k_n_zn59__he4_ni56   = 4711
  integer, parameter :: k_p_zn59__n_ga59   = 4712
  integer, parameter :: k_p_zn59__he4_cu56   = 4713
  integer, parameter :: k_he4_zn59__n_ge62   = 4714
  integer, parameter :: k_he4_zn59__p_ga62   = 4715
  integer, parameter :: k_n_zn60__p_cu60   = 4716
  integer, parameter :: k_n_zn60__he4_ni57   = 4717
  integer, parameter :: k_p_zn60__n_ga60   = 4718
  integer, parameter :: k_p_zn60__he4_cu57   = 4719
  integer, parameter :: k_he4_zn60__n_ge63   = 4720
  integer, parameter :: k_he4_zn60__p_ga63   = 4721
  integer, parameter :: k_n_zn61__p_cu61   = 4722
  integer, parameter :: k_n_zn61__he4_ni58   = 4723
  integer, parameter :: k_p_zn61__n_ga61   = 4724
  integer, parameter :: k_p_zn61__he4_cu58   = 4725
  integer, parameter :: k_he4_zn61__n_ge64   = 4726
  integer, parameter :: k_he4_zn61__p_ga64   = 4727
  integer, parameter :: k_n_zn62__p_cu62   = 4728
  integer, parameter :: k_n_zn62__he4_ni59   = 4729
  integer, parameter :: k_p_zn62__n_ga62   = 4730
  integer, parameter :: k_p_zn62__he4_cu59   = 4731
  integer, parameter :: k_he4_zn62__n_ge65   = 4732
  integer, parameter :: k_he4_zn62__p_ga65   = 4733
  integer, parameter :: k_n_zn63__p_cu63   = 4734
  integer, parameter :: k_n_zn63__he4_ni60   = 4735
  integer, parameter :: k_p_zn63__n_ga63   = 4736
  integer, parameter :: k_p_zn63__he4_cu60   = 4737
  integer, parameter :: k_he4_zn63__n_ge66   = 4738
  integer, parameter :: k_he4_zn63__p_ga66   = 4739
  integer, parameter :: k_n_zn64__p_cu64   = 4740
  integer, parameter :: k_n_zn64__he4_ni61   = 4741
  integer, parameter :: k_p_zn64__n_ga64   = 4742
  integer, parameter :: k_p_zn64__he4_cu61   = 4743
  integer, parameter :: k_he4_zn64__n_ge67   = 4744
  integer, parameter :: k_he4_zn64__p_ga67   = 4745
  integer, parameter :: k_n_zn65__p_cu65   = 4746
  integer, parameter :: k_n_zn65__he4_ni62   = 4747
  integer, parameter :: k_p_zn65__n_ga65   = 4748
  integer, parameter :: k_p_zn65__he4_cu62   = 4749
  integer, parameter :: k_he4_zn65__n_ge68   = 4750
  integer, parameter :: k_he4_zn65__p_ga68   = 4751
  integer, parameter :: k_n_zn66__p_cu66   = 4752
  integer, parameter :: k_n_zn66__he4_ni63   = 4753
  integer, parameter :: k_p_zn66__n_ga66   = 4754
  integer, parameter :: k_p_zn66__he4_cu63   = 4755
  integer, parameter :: k_he4_zn66__n_ge69   = 4756
  integer, parameter :: k_he4_zn66__p_ga69   = 4757
  integer, parameter :: k_n_zn67__p_cu67   = 4758
  integer, parameter :: k_n_zn67__he4_ni64   = 4759
  integer, parameter :: k_p_zn67__n_ga67   = 4760
  integer, parameter :: k_p_zn67__he4_cu64   = 4761
  integer, parameter :: k_he4_zn67__n_ge70   = 4762
  integer, parameter :: k_he4_zn67__p_ga70   = 4763
  integer, parameter :: k_n_zn68__p_cu68   = 4764
  integer, parameter :: k_n_zn68__he4_ni65   = 4765
  integer, parameter :: k_p_zn68__n_ga68   = 4766
  integer, parameter :: k_p_zn68__he4_cu65   = 4767
  integer, parameter :: k_he4_zn68__n_ge71   = 4768
  integer, parameter :: k_he4_zn68__p_ga71   = 4769
  integer, parameter :: k_n_zn69__p_cu69   = 4770
  integer, parameter :: k_n_zn69__he4_ni66   = 4771
  integer, parameter :: k_p_zn69__n_ga69   = 4772
  integer, parameter :: k_p_zn69__he4_cu66   = 4773
  integer, parameter :: k_he4_zn69__n_ge72   = 4774
  integer, parameter :: k_he4_zn69__p_ga72   = 4775
  integer, parameter :: k_n_zn70__he4_ni67   = 4776
  integer, parameter :: k_p_zn70__n_ga70   = 4777
  integer, parameter :: k_p_zn70__he4_cu67   = 4778
  integer, parameter :: k_he4_zn70__n_ge73   = 4779
  integer, parameter :: k_he4_zn70__p_ga73   = 4780
  integer, parameter :: k_n_zn71__he4_ni68   = 4781
  integer, parameter :: k_p_zn71__n_ga71   = 4782
  integer, parameter :: k_p_zn71__he4_cu68   = 4783
  integer, parameter :: k_he4_zn71__n_ge74   = 4784
  integer, parameter :: k_he4_zn71__p_ga74   = 4785
  integer, parameter :: k_p_zn72__n_ga72   = 4786
  integer, parameter :: k_p_zn72__he4_cu69   = 4787
  integer, parameter :: k_he4_zn72__n_ge75   = 4788
  integer, parameter :: k_he4_zn72__p_ga75   = 4789
  integer, parameter :: k_n_ga59__p_zn59   = 4790
  integer, parameter :: k_n_ga59__he4_cu56   = 4791
  integer, parameter :: k_he4_ga59__p_ge62   = 4792
  integer, parameter :: k_n_ga60__p_zn60   = 4793
  integer, parameter :: k_n_ga60__he4_cu57   = 4794
  integer, parameter :: k_p_ga60__he4_zn57   = 4795
  integer, parameter :: k_he4_ga60__p_ge63   = 4796
  integer, parameter :: k_n_ga61__p_zn61   = 4797
  integer, parameter :: k_n_ga61__he4_cu58   = 4798
  integer, parameter :: k_p_ga61__he4_zn58   = 4799
  integer, parameter :: k_he4_ga61__p_ge64   = 4800
  integer, parameter :: k_n_ga62__p_zn62   = 4801
  integer, parameter :: k_n_ga62__he4_cu59   = 4802
  integer, parameter :: k_p_ga62__n_ge62   = 4803
  integer, parameter :: k_p_ga62__he4_zn59   = 4804
  integer, parameter :: k_he4_ga62__n_as65   = 4805
  integer, parameter :: k_he4_ga62__p_ge65   = 4806
  integer, parameter :: k_n_ga63__p_zn63   = 4807
  integer, parameter :: k_n_ga63__he4_cu60   = 4808
  integer, parameter :: k_p_ga63__n_ge63   = 4809
  integer, parameter :: k_p_ga63__he4_zn60   = 4810
  integer, parameter :: k_he4_ga63__n_as66   = 4811
  integer, parameter :: k_he4_ga63__p_ge66   = 4812
  integer, parameter :: k_n_ga64__p_zn64   = 4813
  integer, parameter :: k_n_ga64__he4_cu61   = 4814
  integer, parameter :: k_p_ga64__n_ge64   = 4815
  integer, parameter :: k_p_ga64__he4_zn61   = 4816
  integer, parameter :: k_he4_ga64__n_as67   = 4817
  integer, parameter :: k_he4_ga64__p_ge67   = 4818
  integer, parameter :: k_n_ga65__p_zn65   = 4819
  integer, parameter :: k_n_ga65__he4_cu62   = 4820
  integer, parameter :: k_p_ga65__n_ge65   = 4821
  integer, parameter :: k_p_ga65__he4_zn62   = 4822
  integer, parameter :: k_he4_ga65__n_as68   = 4823
  integer, parameter :: k_he4_ga65__p_ge68   = 4824
  integer, parameter :: k_n_ga66__p_zn66   = 4825
  integer, parameter :: k_n_ga66__he4_cu63   = 4826
  integer, parameter :: k_p_ga66__n_ge66   = 4827
  integer, parameter :: k_p_ga66__he4_zn63   = 4828
  integer, parameter :: k_he4_ga66__n_as69   = 4829
  integer, parameter :: k_he4_ga66__p_ge69   = 4830
  integer, parameter :: k_n_ga67__p_zn67   = 4831
  integer, parameter :: k_n_ga67__he4_cu64   = 4832
  integer, parameter :: k_p_ga67__n_ge67   = 4833
  integer, parameter :: k_p_ga67__he4_zn64   = 4834
  integer, parameter :: k_he4_ga67__n_as70   = 4835
  integer, parameter :: k_he4_ga67__p_ge70   = 4836
  integer, parameter :: k_n_ga68__p_zn68   = 4837
  integer, parameter :: k_n_ga68__he4_cu65   = 4838
  integer, parameter :: k_p_ga68__n_ge68   = 4839
  integer, parameter :: k_p_ga68__he4_zn65   = 4840
  integer, parameter :: k_he4_ga68__n_as71   = 4841
  integer, parameter :: k_he4_ga68__p_ge71   = 4842
  integer, parameter :: k_n_ga69__p_zn69   = 4843
  integer, parameter :: k_n_ga69__he4_cu66   = 4844
  integer, parameter :: k_p_ga69__n_ge69   = 4845
  integer, parameter :: k_p_ga69__he4_zn66   = 4846
  integer, parameter :: k_he4_ga69__n_as72   = 4847
  integer, parameter :: k_he4_ga69__p_ge72   = 4848
  integer, parameter :: k_n_ga70__p_zn70   = 4849
  integer, parameter :: k_n_ga70__he4_cu67   = 4850
  integer, parameter :: k_p_ga70__n_ge70   = 4851
  integer, parameter :: k_p_ga70__he4_zn67   = 4852
  integer, parameter :: k_he4_ga70__n_as73   = 4853
  integer, parameter :: k_he4_ga70__p_ge73   = 4854
  integer, parameter :: k_n_ga71__p_zn71   = 4855
  integer, parameter :: k_n_ga71__he4_cu68   = 4856
  integer, parameter :: k_p_ga71__n_ge71   = 4857
  integer, parameter :: k_p_ga71__he4_zn68   = 4858
  integer, parameter :: k_he4_ga71__n_as74   = 4859
  integer, parameter :: k_he4_ga71__p_ge74   = 4860
  integer, parameter :: k_n_ga72__p_zn72   = 4861
  integer, parameter :: k_n_ga72__he4_cu69   = 4862
  integer, parameter :: k_p_ga72__n_ge72   = 4863
  integer, parameter :: k_p_ga72__he4_zn69   = 4864
  integer, parameter :: k_he4_ga72__n_as75   = 4865
  integer, parameter :: k_he4_ga72__p_ge75   = 4866
  integer, parameter :: k_p_ga73__n_ge73   = 4867
  integer, parameter :: k_p_ga73__he4_zn70   = 4868
  integer, parameter :: k_he4_ga73__n_as76   = 4869
  integer, parameter :: k_he4_ga73__p_ge76   = 4870
  integer, parameter :: k_p_ga74__n_ge74   = 4871
  integer, parameter :: k_p_ga74__he4_zn71   = 4872
  integer, parameter :: k_he4_ga74__n_as77   = 4873
  integer, parameter :: k_he4_ga74__p_ge77   = 4874
  integer, parameter :: k_p_ga75__n_ge75   = 4875
  integer, parameter :: k_p_ga75__he4_zn72   = 4876
  integer, parameter :: k_he4_ga75__n_as78   = 4877
  integer, parameter :: k_he4_ga75__p_ge78   = 4878
  integer, parameter :: k_n_ge62__p_ga62   = 4879
  integer, parameter :: k_n_ge62__he4_zn59   = 4880
  integer, parameter :: k_p_ge62__he4_ga59   = 4881
  integer, parameter :: k_he4_ge62__p_as65   = 4882
  integer, parameter :: k_n_ge63__p_ga63   = 4883
  integer, parameter :: k_n_ge63__he4_zn60   = 4884
  integer, parameter :: k_p_ge63__he4_ga60   = 4885
  integer, parameter :: k_he4_ge63__p_as66   = 4886
  integer, parameter :: k_n_ge64__p_ga64   = 4887
  integer, parameter :: k_n_ge64__he4_zn61   = 4888
  integer, parameter :: k_p_ge64__he4_ga61   = 4889
  integer, parameter :: k_he4_ge64__n_se67   = 4890
  integer, parameter :: k_he4_ge64__p_as67   = 4891
  integer, parameter :: k_n_ge65__p_ga65   = 4892
  integer, parameter :: k_n_ge65__he4_zn62   = 4893
  integer, parameter :: k_p_ge65__n_as65   = 4894
  integer, parameter :: k_p_ge65__he4_ga62   = 4895
  integer, parameter :: k_he4_ge65__n_se68   = 4896
  integer, parameter :: k_he4_ge65__p_as68   = 4897
  integer, parameter :: k_n_ge66__p_ga66   = 4898
  integer, parameter :: k_n_ge66__he4_zn63   = 4899
  integer, parameter :: k_p_ge66__n_as66   = 4900
  integer, parameter :: k_p_ge66__he4_ga63   = 4901
  integer, parameter :: k_he4_ge66__n_se69   = 4902
  integer, parameter :: k_he4_ge66__p_as69   = 4903
  integer, parameter :: k_n_ge67__p_ga67   = 4904
  integer, parameter :: k_n_ge67__he4_zn64   = 4905
  integer, parameter :: k_p_ge67__n_as67   = 4906
  integer, parameter :: k_p_ge67__he4_ga64   = 4907
  integer, parameter :: k_he4_ge67__n_se70   = 4908
  integer, parameter :: k_he4_ge67__p_as70   = 4909
  integer, parameter :: k_n_ge68__p_ga68   = 4910
  integer, parameter :: k_n_ge68__he4_zn65   = 4911
  integer, parameter :: k_p_ge68__n_as68   = 4912
  integer, parameter :: k_p_ge68__he4_ga65   = 4913
  integer, parameter :: k_he4_ge68__n_se71   = 4914
  integer, parameter :: k_he4_ge68__p_as71   = 4915
  integer, parameter :: k_n_ge69__p_ga69   = 4916
  integer, parameter :: k_n_ge69__he4_zn66   = 4917
  integer, parameter :: k_p_ge69__n_as69   = 4918
  integer, parameter :: k_p_ge69__he4_ga66   = 4919
  integer, parameter :: k_he4_ge69__n_se72   = 4920
  integer, parameter :: k_he4_ge69__p_as72   = 4921
  integer, parameter :: k_n_ge70__p_ga70   = 4922
  integer, parameter :: k_n_ge70__he4_zn67   = 4923
  integer, parameter :: k_p_ge70__n_as70   = 4924
  integer, parameter :: k_p_ge70__he4_ga67   = 4925
  integer, parameter :: k_he4_ge70__n_se73   = 4926
  integer, parameter :: k_he4_ge70__p_as73   = 4927
  integer, parameter :: k_n_ge71__p_ga71   = 4928
  integer, parameter :: k_n_ge71__he4_zn68   = 4929
  integer, parameter :: k_p_ge71__n_as71   = 4930
  integer, parameter :: k_p_ge71__he4_ga68   = 4931
  integer, parameter :: k_he4_ge71__n_se74   = 4932
  integer, parameter :: k_he4_ge71__p_as74   = 4933
  integer, parameter :: k_n_ge72__p_ga72   = 4934
  integer, parameter :: k_n_ge72__he4_zn69   = 4935
  integer, parameter :: k_p_ge72__n_as72   = 4936
  integer, parameter :: k_p_ge72__he4_ga69   = 4937
  integer, parameter :: k_he4_ge72__n_se75   = 4938
  integer, parameter :: k_he4_ge72__p_as75   = 4939
  integer, parameter :: k_n_ge73__p_ga73   = 4940
  integer, parameter :: k_n_ge73__he4_zn70   = 4941
  integer, parameter :: k_p_ge73__n_as73   = 4942
  integer, parameter :: k_p_ge73__he4_ga70   = 4943
  integer, parameter :: k_he4_ge73__n_se76   = 4944
  integer, parameter :: k_he4_ge73__p_as76   = 4945
  integer, parameter :: k_n_ge74__p_ga74   = 4946
  integer, parameter :: k_n_ge74__he4_zn71   = 4947
  integer, parameter :: k_p_ge74__n_as74   = 4948
  integer, parameter :: k_p_ge74__he4_ga71   = 4949
  integer, parameter :: k_he4_ge74__n_se77   = 4950
  integer, parameter :: k_he4_ge74__p_as77   = 4951
  integer, parameter :: k_n_ge75__p_ga75   = 4952
  integer, parameter :: k_n_ge75__he4_zn72   = 4953
  integer, parameter :: k_p_ge75__n_as75   = 4954
  integer, parameter :: k_p_ge75__he4_ga72   = 4955
  integer, parameter :: k_he4_ge75__n_se78   = 4956
  integer, parameter :: k_he4_ge75__p_as78   = 4957
  integer, parameter :: k_p_ge76__n_as76   = 4958
  integer, parameter :: k_p_ge76__he4_ga73   = 4959
  integer, parameter :: k_he4_ge76__n_se79   = 4960
  integer, parameter :: k_he4_ge76__p_as79   = 4961
  integer, parameter :: k_p_ge77__n_as77   = 4962
  integer, parameter :: k_p_ge77__he4_ga74   = 4963
  integer, parameter :: k_he4_ge77__n_se80   = 4964
  integer, parameter :: k_p_ge78__n_as78   = 4965
  integer, parameter :: k_p_ge78__he4_ga75   = 4966
  integer, parameter :: k_he4_ge78__n_se81   = 4967
  integer, parameter :: k_n_as65__p_ge65   = 4968
  integer, parameter :: k_n_as65__he4_ga62   = 4969
  integer, parameter :: k_p_as65__he4_ge62   = 4970
  integer, parameter :: k_he4_as65__n_br68   = 4971
  integer, parameter :: k_he4_as65__p_se68   = 4972
  integer, parameter :: k_n_as66__p_ge66   = 4973
  integer, parameter :: k_n_as66__he4_ga63   = 4974
  integer, parameter :: k_p_as66__he4_ge63   = 4975
  integer, parameter :: k_he4_as66__n_br69   = 4976
  integer, parameter :: k_he4_as66__p_se69   = 4977
  integer, parameter :: k_n_as67__p_ge67   = 4978
  integer, parameter :: k_n_as67__he4_ga64   = 4979
  integer, parameter :: k_p_as67__n_se67   = 4980
  integer, parameter :: k_p_as67__he4_ge64   = 4981
  integer, parameter :: k_he4_as67__n_br70   = 4982
  integer, parameter :: k_he4_as67__p_se70   = 4983
  integer, parameter :: k_n_as68__p_ge68   = 4984
  integer, parameter :: k_n_as68__he4_ga65   = 4985
  integer, parameter :: k_p_as68__n_se68   = 4986
  integer, parameter :: k_p_as68__he4_ge65   = 4987
  integer, parameter :: k_he4_as68__n_br71   = 4988
  integer, parameter :: k_he4_as68__p_se71   = 4989
  integer, parameter :: k_n_as69__p_ge69   = 4990
  integer, parameter :: k_n_as69__he4_ga66   = 4991
  integer, parameter :: k_p_as69__n_se69   = 4992
  integer, parameter :: k_p_as69__he4_ge66   = 4993
  integer, parameter :: k_he4_as69__n_br72   = 4994
  integer, parameter :: k_he4_as69__p_se72   = 4995
  integer, parameter :: k_n_as70__p_ge70   = 4996
  integer, parameter :: k_n_as70__he4_ga67   = 4997
  integer, parameter :: k_p_as70__n_se70   = 4998
  integer, parameter :: k_p_as70__he4_ge67   = 4999
  integer, parameter :: k_he4_as70__n_br73   = 5000
  integer, parameter :: k_he4_as70__p_se73   = 5001
  integer, parameter :: k_n_as71__p_ge71   = 5002
  integer, parameter :: k_n_as71__he4_ga68   = 5003
  integer, parameter :: k_p_as71__n_se71   = 5004
  integer, parameter :: k_p_as71__he4_ge68   = 5005
  integer, parameter :: k_he4_as71__n_br74   = 5006
  integer, parameter :: k_he4_as71__p_se74   = 5007
  integer, parameter :: k_n_as72__p_ge72   = 5008
  integer, parameter :: k_n_as72__he4_ga69   = 5009
  integer, parameter :: k_p_as72__n_se72   = 5010
  integer, parameter :: k_p_as72__he4_ge69   = 5011
  integer, parameter :: k_he4_as72__n_br75   = 5012
  integer, parameter :: k_he4_as72__p_se75   = 5013
  integer, parameter :: k_n_as73__p_ge73   = 5014
  integer, parameter :: k_n_as73__he4_ga70   = 5015
  integer, parameter :: k_p_as73__n_se73   = 5016
  integer, parameter :: k_p_as73__he4_ge70   = 5017
  integer, parameter :: k_he4_as73__n_br76   = 5018
  integer, parameter :: k_he4_as73__p_se76   = 5019
  integer, parameter :: k_n_as74__p_ge74   = 5020
  integer, parameter :: k_n_as74__he4_ga71   = 5021
  integer, parameter :: k_p_as74__n_se74   = 5022
  integer, parameter :: k_p_as74__he4_ge71   = 5023
  integer, parameter :: k_he4_as74__n_br77   = 5024
  integer, parameter :: k_he4_as74__p_se77   = 5025
  integer, parameter :: k_n_as75__p_ge75   = 5026
  integer, parameter :: k_n_as75__he4_ga72   = 5027
  integer, parameter :: k_p_as75__n_se75   = 5028
  integer, parameter :: k_p_as75__he4_ge72   = 5029
  integer, parameter :: k_he4_as75__n_br78   = 5030
  integer, parameter :: k_he4_as75__p_se78   = 5031
  integer, parameter :: k_n_as76__p_ge76   = 5032
  integer, parameter :: k_n_as76__he4_ga73   = 5033
  integer, parameter :: k_p_as76__n_se76   = 5034
  integer, parameter :: k_p_as76__he4_ge73   = 5035
  integer, parameter :: k_he4_as76__n_br79   = 5036
  integer, parameter :: k_he4_as76__p_se79   = 5037
  integer, parameter :: k_n_as77__p_ge77   = 5038
  integer, parameter :: k_n_as77__he4_ga74   = 5039
  integer, parameter :: k_p_as77__n_se77   = 5040
  integer, parameter :: k_p_as77__he4_ge74   = 5041
  integer, parameter :: k_he4_as77__n_br80   = 5042
  integer, parameter :: k_he4_as77__p_se80   = 5043
  integer, parameter :: k_n_as78__p_ge78   = 5044
  integer, parameter :: k_n_as78__he4_ga75   = 5045
  integer, parameter :: k_p_as78__n_se78   = 5046
  integer, parameter :: k_p_as78__he4_ge75   = 5047
  integer, parameter :: k_he4_as78__n_br81   = 5048
  integer, parameter :: k_he4_as78__p_se81   = 5049
  integer, parameter :: k_p_as79__n_se79   = 5050
  integer, parameter :: k_p_as79__he4_ge76   = 5051
  integer, parameter :: k_he4_as79__n_br82   = 5052
  integer, parameter :: k_he4_as79__p_se82   = 5053
  integer, parameter :: k_n_se67__p_as67   = 5054
  integer, parameter :: k_n_se67__he4_ge64   = 5055
  integer, parameter :: k_he4_se67__n_kr70   = 5056
  integer, parameter :: k_he4_se67__p_br70   = 5057
  integer, parameter :: k_n_se68__p_as68   = 5058
  integer, parameter :: k_n_se68__he4_ge65   = 5059
  integer, parameter :: k_p_se68__n_br68   = 5060
  integer, parameter :: k_p_se68__he4_as65   = 5061
  integer, parameter :: k_he4_se68__n_kr71   = 5062
  integer, parameter :: k_he4_se68__p_br71   = 5063
  integer, parameter :: k_n_se69__p_as69   = 5064
  integer, parameter :: k_n_se69__he4_ge66   = 5065
  integer, parameter :: k_p_se69__n_br69   = 5066
  integer, parameter :: k_p_se69__he4_as66   = 5067
  integer, parameter :: k_he4_se69__n_kr72   = 5068
  integer, parameter :: k_he4_se69__p_br72   = 5069
  integer, parameter :: k_n_se70__p_as70   = 5070
  integer, parameter :: k_n_se70__he4_ge67   = 5071
  integer, parameter :: k_p_se70__n_br70   = 5072
  integer, parameter :: k_p_se70__he4_as67   = 5073
  integer, parameter :: k_he4_se70__n_kr73   = 5074
  integer, parameter :: k_he4_se70__p_br73   = 5075
  integer, parameter :: k_n_se71__p_as71   = 5076
  integer, parameter :: k_n_se71__he4_ge68   = 5077
  integer, parameter :: k_p_se71__n_br71   = 5078
  integer, parameter :: k_p_se71__he4_as68   = 5079
  integer, parameter :: k_he4_se71__n_kr74   = 5080
  integer, parameter :: k_he4_se71__p_br74   = 5081
  integer, parameter :: k_n_se72__p_as72   = 5082
  integer, parameter :: k_n_se72__he4_ge69   = 5083
  integer, parameter :: k_p_se72__n_br72   = 5084
  integer, parameter :: k_p_se72__he4_as69   = 5085
  integer, parameter :: k_he4_se72__n_kr75   = 5086
  integer, parameter :: k_he4_se72__p_br75   = 5087
  integer, parameter :: k_n_se73__p_as73   = 5088
  integer, parameter :: k_n_se73__he4_ge70   = 5089
  integer, parameter :: k_p_se73__n_br73   = 5090
  integer, parameter :: k_p_se73__he4_as70   = 5091
  integer, parameter :: k_he4_se73__n_kr76   = 5092
  integer, parameter :: k_he4_se73__p_br76   = 5093
  integer, parameter :: k_n_se74__p_as74   = 5094
  integer, parameter :: k_n_se74__he4_ge71   = 5095
  integer, parameter :: k_p_se74__n_br74   = 5096
  integer, parameter :: k_p_se74__he4_as71   = 5097
  integer, parameter :: k_he4_se74__n_kr77   = 5098
  integer, parameter :: k_he4_se74__p_br77   = 5099
  integer, parameter :: k_n_se75__p_as75   = 5100
  integer, parameter :: k_n_se75__he4_ge72   = 5101
  integer, parameter :: k_p_se75__n_br75   = 5102
  integer, parameter :: k_p_se75__he4_as72   = 5103
  integer, parameter :: k_he4_se75__n_kr78   = 5104
  integer, parameter :: k_he4_se75__p_br78   = 5105
  integer, parameter :: k_n_se76__p_as76   = 5106
  integer, parameter :: k_n_se76__he4_ge73   = 5107
  integer, parameter :: k_p_se76__n_br76   = 5108
  integer, parameter :: k_p_se76__he4_as73   = 5109
  integer, parameter :: k_he4_se76__n_kr79   = 5110
  integer, parameter :: k_he4_se76__p_br79   = 5111
  integer, parameter :: k_n_se77__p_as77   = 5112
  integer, parameter :: k_n_se77__he4_ge74   = 5113
  integer, parameter :: k_p_se77__n_br77   = 5114
  integer, parameter :: k_p_se77__he4_as74   = 5115
  integer, parameter :: k_he4_se77__n_kr80   = 5116
  integer, parameter :: k_he4_se77__p_br80   = 5117
  integer, parameter :: k_n_se78__p_as78   = 5118
  integer, parameter :: k_n_se78__he4_ge75   = 5119
  integer, parameter :: k_p_se78__n_br78   = 5120
  integer, parameter :: k_p_se78__he4_as75   = 5121
  integer, parameter :: k_he4_se78__n_kr81   = 5122
  integer, parameter :: k_he4_se78__p_br81   = 5123
  integer, parameter :: k_n_se79__p_as79   = 5124
  integer, parameter :: k_n_se79__he4_ge76   = 5125
  integer, parameter :: k_p_se79__n_br79   = 5126
  integer, parameter :: k_p_se79__he4_as76   = 5127
  integer, parameter :: k_he4_se79__n_kr82   = 5128
  integer, parameter :: k_he4_se79__p_br82   = 5129
  integer, parameter :: k_n_se80__he4_ge77   = 5130
  integer, parameter :: k_p_se80__n_br80   = 5131
  integer, parameter :: k_p_se80__he4_as77   = 5132
  integer, parameter :: k_he4_se80__n_kr83   = 5133
  integer, parameter :: k_he4_se80__p_br83   = 5134
  integer, parameter :: k_n_se81__he4_ge78   = 5135
  integer, parameter :: k_p_se81__n_br81   = 5136
  integer, parameter :: k_p_se81__he4_as78   = 5137
  integer, parameter :: k_he4_se81__n_kr84   = 5138
  integer, parameter :: k_p_se82__n_br82   = 5139
  integer, parameter :: k_p_se82__he4_as79   = 5140
  integer, parameter :: k_he4_se82__n_kr85   = 5141
  integer, parameter :: k_p_se83__n_br83   = 5142
  integer, parameter :: k_he4_se83__n_kr86   = 5143
  integer, parameter :: k_n_br68__p_se68   = 5144
  integer, parameter :: k_n_br68__he4_as65   = 5145
  integer, parameter :: k_he4_br68__p_kr71   = 5146
  integer, parameter :: k_n_br69__p_se69   = 5147
  integer, parameter :: k_n_br69__he4_as66   = 5148
  integer, parameter :: k_p_br69__n_kr69   = 5149
  integer, parameter :: k_he4_br69__p_kr72   = 5150
  integer, parameter :: k_n_br70__p_se70   = 5151
  integer, parameter :: k_n_br70__he4_as67   = 5152
  integer, parameter :: k_p_br70__n_kr70   = 5153
  integer, parameter :: k_p_br70__he4_se67   = 5154
  integer, parameter :: k_he4_br70__n_rb73   = 5155
  integer, parameter :: k_he4_br70__p_kr73   = 5156
  integer, parameter :: k_n_br71__p_se71   = 5157
  integer, parameter :: k_n_br71__he4_as68   = 5158
  integer, parameter :: k_p_br71__n_kr71   = 5159
  integer, parameter :: k_p_br71__he4_se68   = 5160
  integer, parameter :: k_he4_br71__n_rb74   = 5161
  integer, parameter :: k_he4_br71__p_kr74   = 5162
  integer, parameter :: k_n_br72__p_se72   = 5163
  integer, parameter :: k_n_br72__he4_as69   = 5164
  integer, parameter :: k_p_br72__n_kr72   = 5165
  integer, parameter :: k_p_br72__he4_se69   = 5166
  integer, parameter :: k_he4_br72__n_rb75   = 5167
  integer, parameter :: k_he4_br72__p_kr75   = 5168
  integer, parameter :: k_n_br73__p_se73   = 5169
  integer, parameter :: k_n_br73__he4_as70   = 5170
  integer, parameter :: k_p_br73__n_kr73   = 5171
  integer, parameter :: k_p_br73__he4_se70   = 5172
  integer, parameter :: k_he4_br73__n_rb76   = 5173
  integer, parameter :: k_he4_br73__p_kr76   = 5174
  integer, parameter :: k_n_br74__p_se74   = 5175
  integer, parameter :: k_n_br74__he4_as71   = 5176
  integer, parameter :: k_p_br74__n_kr74   = 5177
  integer, parameter :: k_p_br74__he4_se71   = 5178
  integer, parameter :: k_he4_br74__n_rb77   = 5179
  integer, parameter :: k_he4_br74__p_kr77   = 5180
  integer, parameter :: k_n_br75__p_se75   = 5181
  integer, parameter :: k_n_br75__he4_as72   = 5182
  integer, parameter :: k_p_br75__n_kr75   = 5183
  integer, parameter :: k_p_br75__he4_se72   = 5184
  integer, parameter :: k_he4_br75__n_rb78   = 5185
  integer, parameter :: k_he4_br75__p_kr78   = 5186
  integer, parameter :: k_n_br76__p_se76   = 5187
  integer, parameter :: k_n_br76__he4_as73   = 5188
  integer, parameter :: k_p_br76__n_kr76   = 5189
  integer, parameter :: k_p_br76__he4_se73   = 5190
  integer, parameter :: k_he4_br76__n_rb79   = 5191
  integer, parameter :: k_he4_br76__p_kr79   = 5192
  integer, parameter :: k_n_br77__p_se77   = 5193
  integer, parameter :: k_n_br77__he4_as74   = 5194
  integer, parameter :: k_p_br77__n_kr77   = 5195
  integer, parameter :: k_p_br77__he4_se74   = 5196
  integer, parameter :: k_he4_br77__n_rb80   = 5197
  integer, parameter :: k_he4_br77__p_kr80   = 5198
  integer, parameter :: k_n_br78__p_se78   = 5199
  integer, parameter :: k_n_br78__he4_as75   = 5200
  integer, parameter :: k_p_br78__n_kr78   = 5201
  integer, parameter :: k_p_br78__he4_se75   = 5202
  integer, parameter :: k_he4_br78__n_rb81   = 5203
  integer, parameter :: k_he4_br78__p_kr81   = 5204
  integer, parameter :: k_n_br79__p_se79   = 5205
  integer, parameter :: k_n_br79__he4_as76   = 5206
  integer, parameter :: k_p_br79__n_kr79   = 5207
  integer, parameter :: k_p_br79__he4_se76   = 5208
  integer, parameter :: k_he4_br79__n_rb82   = 5209
  integer, parameter :: k_he4_br79__p_kr82   = 5210
  integer, parameter :: k_n_br80__p_se80   = 5211
  integer, parameter :: k_n_br80__he4_as77   = 5212
  integer, parameter :: k_p_br80__n_kr80   = 5213
  integer, parameter :: k_p_br80__he4_se77   = 5214
  integer, parameter :: k_he4_br80__n_rb83   = 5215
  integer, parameter :: k_he4_br80__p_kr83   = 5216
  integer, parameter :: k_n_br81__p_se81   = 5217
  integer, parameter :: k_n_br81__he4_as78   = 5218
  integer, parameter :: k_p_br81__n_kr81   = 5219
  integer, parameter :: k_p_br81__he4_se78   = 5220
  integer, parameter :: k_he4_br81__n_rb84   = 5221
  integer, parameter :: k_he4_br81__p_kr84   = 5222
  integer, parameter :: k_n_br82__p_se82   = 5223
  integer, parameter :: k_n_br82__he4_as79   = 5224
  integer, parameter :: k_p_br82__n_kr82   = 5225
  integer, parameter :: k_p_br82__he4_se79   = 5226
  integer, parameter :: k_he4_br82__n_rb85   = 5227
  integer, parameter :: k_he4_br82__p_kr85   = 5228
  integer, parameter :: k_n_br83__p_se83   = 5229
  integer, parameter :: k_p_br83__n_kr83   = 5230
  integer, parameter :: k_p_br83__he4_se80   = 5231
  integer, parameter :: k_he4_br83__p_kr86   = 5232
  integer, parameter :: k_n_kr69__p_br69   = 5233
  integer, parameter :: k_n_kr70__p_br70   = 5234
  integer, parameter :: k_n_kr70__he4_se67   = 5235
  integer, parameter :: k_he4_kr70__p_rb73   = 5236
  integer, parameter :: k_n_kr71__p_br71   = 5237
  integer, parameter :: k_n_kr71__he4_se68   = 5238
  integer, parameter :: k_p_kr71__he4_br68   = 5239
  integer, parameter :: k_he4_kr71__n_sr74   = 5240
  integer, parameter :: k_he4_kr71__p_rb74   = 5241
  integer, parameter :: k_n_kr72__p_br72   = 5242
  integer, parameter :: k_n_kr72__he4_se69   = 5243
  integer, parameter :: k_p_kr72__he4_br69   = 5244
  integer, parameter :: k_he4_kr72__n_sr75   = 5245
  integer, parameter :: k_he4_kr72__p_rb75   = 5246
  integer, parameter :: k_n_kr73__p_br73   = 5247
  integer, parameter :: k_n_kr73__he4_se70   = 5248
  integer, parameter :: k_p_kr73__n_rb73   = 5249
  integer, parameter :: k_p_kr73__he4_br70   = 5250
  integer, parameter :: k_he4_kr73__n_sr76   = 5251
  integer, parameter :: k_he4_kr73__p_rb76   = 5252
  integer, parameter :: k_n_kr74__p_br74   = 5253
  integer, parameter :: k_n_kr74__he4_se71   = 5254
  integer, parameter :: k_p_kr74__n_rb74   = 5255
  integer, parameter :: k_p_kr74__he4_br71   = 5256
  integer, parameter :: k_he4_kr74__n_sr77   = 5257
  integer, parameter :: k_he4_kr74__p_rb77   = 5258
  integer, parameter :: k_n_kr75__p_br75   = 5259
  integer, parameter :: k_n_kr75__he4_se72   = 5260
  integer, parameter :: k_p_kr75__n_rb75   = 5261
  integer, parameter :: k_p_kr75__he4_br72   = 5262
  integer, parameter :: k_he4_kr75__n_sr78   = 5263
  integer, parameter :: k_he4_kr75__p_rb78   = 5264
  integer, parameter :: k_n_kr76__p_br76   = 5265
  integer, parameter :: k_n_kr76__he4_se73   = 5266
  integer, parameter :: k_p_kr76__n_rb76   = 5267
  integer, parameter :: k_p_kr76__he4_br73   = 5268
  integer, parameter :: k_he4_kr76__n_sr79   = 5269
  integer, parameter :: k_he4_kr76__p_rb79   = 5270
  integer, parameter :: k_n_kr77__p_br77   = 5271
  integer, parameter :: k_n_kr77__he4_se74   = 5272
  integer, parameter :: k_p_kr77__n_rb77   = 5273
  integer, parameter :: k_p_kr77__he4_br74   = 5274
  integer, parameter :: k_he4_kr77__n_sr80   = 5275
  integer, parameter :: k_he4_kr77__p_rb80   = 5276
  integer, parameter :: k_n_kr78__p_br78   = 5277
  integer, parameter :: k_n_kr78__he4_se75   = 5278
  integer, parameter :: k_p_kr78__n_rb78   = 5279
  integer, parameter :: k_p_kr78__he4_br75   = 5280
  integer, parameter :: k_he4_kr78__n_sr81   = 5281
  integer, parameter :: k_he4_kr78__p_rb81   = 5282
  integer, parameter :: k_n_kr79__p_br79   = 5283
  integer, parameter :: k_n_kr79__he4_se76   = 5284
  integer, parameter :: k_p_kr79__n_rb79   = 5285
  integer, parameter :: k_p_kr79__he4_br76   = 5286
  integer, parameter :: k_he4_kr79__n_sr82   = 5287
  integer, parameter :: k_he4_kr79__p_rb82   = 5288
  integer, parameter :: k_n_kr80__p_br80   = 5289
  integer, parameter :: k_n_kr80__he4_se77   = 5290
  integer, parameter :: k_p_kr80__n_rb80   = 5291
  integer, parameter :: k_p_kr80__he4_br77   = 5292
  integer, parameter :: k_he4_kr80__n_sr83   = 5293
  integer, parameter :: k_he4_kr80__p_rb83   = 5294
  integer, parameter :: k_n_kr81__p_br81   = 5295
  integer, parameter :: k_n_kr81__he4_se78   = 5296
  integer, parameter :: k_p_kr81__n_rb81   = 5297
  integer, parameter :: k_p_kr81__he4_br78   = 5298
  integer, parameter :: k_he4_kr81__n_sr84   = 5299
  integer, parameter :: k_he4_kr81__p_rb84   = 5300
  integer, parameter :: k_n_kr82__p_br82   = 5301
  integer, parameter :: k_n_kr82__he4_se79   = 5302
  integer, parameter :: k_p_kr82__n_rb82   = 5303
  integer, parameter :: k_p_kr82__he4_br79   = 5304
  integer, parameter :: k_he4_kr82__p_rb85   = 5305
  integer, parameter :: k_n_kr83__p_br83   = 5306
  integer, parameter :: k_n_kr83__he4_se80   = 5307
  integer, parameter :: k_p_kr83__n_rb83   = 5308
  integer, parameter :: k_p_kr83__he4_br80   = 5309
  integer, parameter :: k_n_kr84__he4_se81   = 5310
  integer, parameter :: k_p_kr84__n_rb84   = 5311
  integer, parameter :: k_p_kr84__he4_br81   = 5312
  integer, parameter :: k_n_kr85__he4_se82   = 5313
  integer, parameter :: k_p_kr85__n_rb85   = 5314
  integer, parameter :: k_p_kr85__he4_br82   = 5315
  integer, parameter :: k_n_kr86__he4_se83   = 5316
  integer, parameter :: k_p_kr86__he4_br83   = 5317
  integer, parameter :: k_n_rb73__p_kr73   = 5318
  integer, parameter :: k_n_rb73__he4_br70   = 5319
  integer, parameter :: k_p_rb73__he4_kr70   = 5320
  integer, parameter :: k_he4_rb73__n_y76   = 5321
  integer, parameter :: k_he4_rb73__p_sr76   = 5322
  integer, parameter :: k_n_rb74__p_kr74   = 5323
  integer, parameter :: k_n_rb74__he4_br71   = 5324
  integer, parameter :: k_p_rb74__n_sr74   = 5325
  integer, parameter :: k_p_rb74__he4_kr71   = 5326
  integer, parameter :: k_he4_rb74__n_y77   = 5327
  integer, parameter :: k_he4_rb74__p_sr77   = 5328
  integer, parameter :: k_n_rb75__p_kr75   = 5329
  integer, parameter :: k_n_rb75__he4_br72   = 5330
  integer, parameter :: k_p_rb75__n_sr75   = 5331
  integer, parameter :: k_p_rb75__he4_kr72   = 5332
  integer, parameter :: k_he4_rb75__n_y78   = 5333
  integer, parameter :: k_he4_rb75__p_sr78   = 5334
  integer, parameter :: k_n_rb76__p_kr76   = 5335
  integer, parameter :: k_n_rb76__he4_br73   = 5336
  integer, parameter :: k_p_rb76__n_sr76   = 5337
  integer, parameter :: k_p_rb76__he4_kr73   = 5338
  integer, parameter :: k_he4_rb76__n_y79   = 5339
  integer, parameter :: k_he4_rb76__p_sr79   = 5340
  integer, parameter :: k_n_rb77__p_kr77   = 5341
  integer, parameter :: k_n_rb77__he4_br74   = 5342
  integer, parameter :: k_p_rb77__n_sr77   = 5343
  integer, parameter :: k_p_rb77__he4_kr74   = 5344
  integer, parameter :: k_he4_rb77__n_y80   = 5345
  integer, parameter :: k_he4_rb77__p_sr80   = 5346
  integer, parameter :: k_n_rb78__p_kr78   = 5347
  integer, parameter :: k_n_rb78__he4_br75   = 5348
  integer, parameter :: k_p_rb78__n_sr78   = 5349
  integer, parameter :: k_p_rb78__he4_kr75   = 5350
  integer, parameter :: k_he4_rb78__n_y81   = 5351
  integer, parameter :: k_he4_rb78__p_sr81   = 5352
  integer, parameter :: k_n_rb79__p_kr79   = 5353
  integer, parameter :: k_n_rb79__he4_br76   = 5354
  integer, parameter :: k_p_rb79__n_sr79   = 5355
  integer, parameter :: k_p_rb79__he4_kr76   = 5356
  integer, parameter :: k_he4_rb79__n_y82   = 5357
  integer, parameter :: k_he4_rb79__p_sr82   = 5358
  integer, parameter :: k_n_rb80__p_kr80   = 5359
  integer, parameter :: k_n_rb80__he4_br77   = 5360
  integer, parameter :: k_p_rb80__n_sr80   = 5361
  integer, parameter :: k_p_rb80__he4_kr77   = 5362
  integer, parameter :: k_he4_rb80__n_y83   = 5363
  integer, parameter :: k_he4_rb80__p_sr83   = 5364
  integer, parameter :: k_n_rb81__p_kr81   = 5365
  integer, parameter :: k_n_rb81__he4_br78   = 5366
  integer, parameter :: k_p_rb81__n_sr81   = 5367
  integer, parameter :: k_p_rb81__he4_kr78   = 5368
  integer, parameter :: k_he4_rb81__n_y84   = 5369
  integer, parameter :: k_he4_rb81__p_sr84   = 5370
  integer, parameter :: k_n_rb82__p_kr82   = 5371
  integer, parameter :: k_n_rb82__he4_br79   = 5372
  integer, parameter :: k_p_rb82__n_sr82   = 5373
  integer, parameter :: k_p_rb82__he4_kr79   = 5374
  integer, parameter :: k_he4_rb82__n_y85   = 5375
  integer, parameter :: k_n_rb83__p_kr83   = 5376
  integer, parameter :: k_n_rb83__he4_br80   = 5377
  integer, parameter :: k_p_rb83__n_sr83   = 5378
  integer, parameter :: k_p_rb83__he4_kr80   = 5379
  integer, parameter :: k_he4_rb83__n_y86   = 5380
  integer, parameter :: k_n_rb84__p_kr84   = 5381
  integer, parameter :: k_n_rb84__he4_br81   = 5382
  integer, parameter :: k_p_rb84__n_sr84   = 5383
  integer, parameter :: k_p_rb84__he4_kr81   = 5384
  integer, parameter :: k_he4_rb84__n_y87   = 5385
  integer, parameter :: k_n_rb85__p_kr85   = 5386
  integer, parameter :: k_n_rb85__he4_br82   = 5387
  integer, parameter :: k_p_rb85__he4_kr82   = 5388
  integer, parameter :: k_n_sr74__p_rb74   = 5389
  integer, parameter :: k_n_sr74__he4_kr71   = 5390
  integer, parameter :: k_he4_sr74__p_y77   = 5391
  integer, parameter :: k_n_sr75__p_rb75   = 5392
  integer, parameter :: k_n_sr75__he4_kr72   = 5393
  integer, parameter :: k_p_sr75__n_y75   = 5394
  integer, parameter :: k_he4_sr75__n_zr78   = 5395
  integer, parameter :: k_he4_sr75__p_y78   = 5396
  integer, parameter :: k_n_sr76__p_rb76   = 5397
  integer, parameter :: k_n_sr76__he4_kr73   = 5398
  integer, parameter :: k_p_sr76__n_y76   = 5399
  integer, parameter :: k_p_sr76__he4_rb73   = 5400
  integer, parameter :: k_he4_sr76__n_zr79   = 5401
  integer, parameter :: k_he4_sr76__p_y79   = 5402
  integer, parameter :: k_n_sr77__p_rb77   = 5403
  integer, parameter :: k_n_sr77__he4_kr74   = 5404
  integer, parameter :: k_p_sr77__n_y77   = 5405
  integer, parameter :: k_p_sr77__he4_rb74   = 5406
  integer, parameter :: k_he4_sr77__n_zr80   = 5407
  integer, parameter :: k_he4_sr77__p_y80   = 5408
  integer, parameter :: k_n_sr78__p_rb78   = 5409
  integer, parameter :: k_n_sr78__he4_kr75   = 5410
  integer, parameter :: k_p_sr78__n_y78   = 5411
  integer, parameter :: k_p_sr78__he4_rb75   = 5412
  integer, parameter :: k_he4_sr78__n_zr81   = 5413
  integer, parameter :: k_he4_sr78__p_y81   = 5414
  integer, parameter :: k_n_sr79__p_rb79   = 5415
  integer, parameter :: k_n_sr79__he4_kr76   = 5416
  integer, parameter :: k_p_sr79__n_y79   = 5417
  integer, parameter :: k_p_sr79__he4_rb76   = 5418
  integer, parameter :: k_he4_sr79__n_zr82   = 5419
  integer, parameter :: k_he4_sr79__p_y82   = 5420
  integer, parameter :: k_n_sr80__p_rb80   = 5421
  integer, parameter :: k_n_sr80__he4_kr77   = 5422
  integer, parameter :: k_p_sr80__n_y80   = 5423
  integer, parameter :: k_p_sr80__he4_rb77   = 5424
  integer, parameter :: k_he4_sr80__n_zr83   = 5425
  integer, parameter :: k_he4_sr80__p_y83   = 5426
  integer, parameter :: k_n_sr81__p_rb81   = 5427
  integer, parameter :: k_n_sr81__he4_kr78   = 5428
  integer, parameter :: k_p_sr81__n_y81   = 5429
  integer, parameter :: k_p_sr81__he4_rb78   = 5430
  integer, parameter :: k_he4_sr81__n_zr84   = 5431
  integer, parameter :: k_he4_sr81__p_y84   = 5432
  integer, parameter :: k_n_sr82__p_rb82   = 5433
  integer, parameter :: k_n_sr82__he4_kr79   = 5434
  integer, parameter :: k_p_sr82__n_y82   = 5435
  integer, parameter :: k_p_sr82__he4_rb79   = 5436
  integer, parameter :: k_he4_sr82__n_zr85   = 5437
  integer, parameter :: k_he4_sr82__p_y85   = 5438
  integer, parameter :: k_n_sr83__p_rb83   = 5439
  integer, parameter :: k_n_sr83__he4_kr80   = 5440
  integer, parameter :: k_p_sr83__n_y83   = 5441
  integer, parameter :: k_p_sr83__he4_rb80   = 5442
  integer, parameter :: k_he4_sr83__n_zr86   = 5443
  integer, parameter :: k_he4_sr83__p_y86   = 5444
  integer, parameter :: k_n_sr84__p_rb84   = 5445
  integer, parameter :: k_n_sr84__he4_kr81   = 5446
  integer, parameter :: k_p_sr84__n_y84   = 5447
  integer, parameter :: k_p_sr84__he4_rb81   = 5448
  integer, parameter :: k_he4_sr84__n_zr87   = 5449
  integer, parameter :: k_he4_sr84__p_y87   = 5450
  integer, parameter :: k_n_y75__p_sr75   = 5451
  integer, parameter :: k_he4_y75__p_zr78   = 5452
  integer, parameter :: k_n_y76__p_sr76   = 5453
  integer, parameter :: k_n_y76__he4_rb73   = 5454
  integer, parameter :: k_he4_y76__p_zr79   = 5455
  integer, parameter :: k_n_y77__p_sr77   = 5456
  integer, parameter :: k_n_y77__he4_rb74   = 5457
  integer, parameter :: k_p_y77__he4_sr74   = 5458
  integer, parameter :: k_he4_y77__p_zr80   = 5459
  integer, parameter :: k_n_y78__p_sr78   = 5460
  integer, parameter :: k_n_y78__he4_rb75   = 5461
  integer, parameter :: k_p_y78__n_zr78   = 5462
  integer, parameter :: k_p_y78__he4_sr75   = 5463
  integer, parameter :: k_he4_y78__p_zr81   = 5464
  integer, parameter :: k_n_y79__p_sr79   = 5465
  integer, parameter :: k_n_y79__he4_rb76   = 5466
  integer, parameter :: k_p_y79__n_zr79   = 5467
  integer, parameter :: k_p_y79__he4_sr76   = 5468
  integer, parameter :: k_he4_y79__n_nb82   = 5469
  integer, parameter :: k_he4_y79__p_zr82   = 5470
  integer, parameter :: k_n_y80__p_sr80   = 5471
  integer, parameter :: k_n_y80__he4_rb77   = 5472
  integer, parameter :: k_p_y80__n_zr80   = 5473
  integer, parameter :: k_p_y80__he4_sr77   = 5474
  integer, parameter :: k_he4_y80__n_nb83   = 5475
  integer, parameter :: k_he4_y80__p_zr83   = 5476
  integer, parameter :: k_n_y81__p_sr81   = 5477
  integer, parameter :: k_n_y81__he4_rb78   = 5478
  integer, parameter :: k_p_y81__n_zr81   = 5479
  integer, parameter :: k_p_y81__he4_sr78   = 5480
  integer, parameter :: k_he4_y81__n_nb84   = 5481
  integer, parameter :: k_he4_y81__p_zr84   = 5482
  integer, parameter :: k_n_y82__p_sr82   = 5483
  integer, parameter :: k_n_y82__he4_rb79   = 5484
  integer, parameter :: k_p_y82__n_zr82   = 5485
  integer, parameter :: k_p_y82__he4_sr79   = 5486
  integer, parameter :: k_he4_y82__n_nb85   = 5487
  integer, parameter :: k_he4_y82__p_zr85   = 5488
  integer, parameter :: k_n_y83__p_sr83   = 5489
  integer, parameter :: k_n_y83__he4_rb80   = 5490
  integer, parameter :: k_p_y83__n_zr83   = 5491
  integer, parameter :: k_p_y83__he4_sr80   = 5492
  integer, parameter :: k_he4_y83__n_nb86   = 5493
  integer, parameter :: k_he4_y83__p_zr86   = 5494
  integer, parameter :: k_n_y84__p_sr84   = 5495
  integer, parameter :: k_n_y84__he4_rb81   = 5496
  integer, parameter :: k_p_y84__n_zr84   = 5497
  integer, parameter :: k_p_y84__he4_sr81   = 5498
  integer, parameter :: k_he4_y84__n_nb87   = 5499
  integer, parameter :: k_he4_y84__p_zr87   = 5500
  integer, parameter :: k_n_y85__he4_rb82   = 5501
  integer, parameter :: k_p_y85__n_zr85   = 5502
  integer, parameter :: k_p_y85__he4_sr82   = 5503
  integer, parameter :: k_he4_y85__n_nb88   = 5504
  integer, parameter :: k_he4_y85__p_zr88   = 5505
  integer, parameter :: k_n_y86__he4_rb83   = 5506
  integer, parameter :: k_p_y86__n_zr86   = 5507
  integer, parameter :: k_p_y86__he4_sr83   = 5508
  integer, parameter :: k_he4_y86__n_nb89   = 5509
  integer, parameter :: k_he4_y86__p_zr89   = 5510
  integer, parameter :: k_n_y87__he4_rb84   = 5511
  integer, parameter :: k_p_y87__n_zr87   = 5512
  integer, parameter :: k_p_y87__he4_sr84   = 5513
  integer, parameter :: k_he4_y87__n_nb90   = 5514
  integer, parameter :: k_he4_y87__p_zr90   = 5515
  integer, parameter :: k_n_zr78__p_y78   = 5516
  integer, parameter :: k_n_zr78__he4_sr75   = 5517
  integer, parameter :: k_p_zr78__he4_y75   = 5518
  integer, parameter :: k_n_zr79__p_y79   = 5519
  integer, parameter :: k_n_zr79__he4_sr76   = 5520
  integer, parameter :: k_p_zr79__he4_y76   = 5521
  integer, parameter :: k_he4_zr79__p_nb82   = 5522
  integer, parameter :: k_n_zr80__p_y80   = 5523
  integer, parameter :: k_n_zr80__he4_sr77   = 5524
  integer, parameter :: k_p_zr80__he4_y77   = 5525
  integer, parameter :: k_he4_zr80__n_mo83   = 5526
  integer, parameter :: k_he4_zr80__p_nb83   = 5527
  integer, parameter :: k_n_zr81__p_y81   = 5528
  integer, parameter :: k_n_zr81__he4_sr78   = 5529
  integer, parameter :: k_p_zr81__he4_y78   = 5530
  integer, parameter :: k_he4_zr81__n_mo84   = 5531
  integer, parameter :: k_he4_zr81__p_nb84   = 5532
  integer, parameter :: k_n_zr82__p_y82   = 5533
  integer, parameter :: k_n_zr82__he4_sr79   = 5534
  integer, parameter :: k_p_zr82__n_nb82   = 5535
  integer, parameter :: k_p_zr82__he4_y79   = 5536
  integer, parameter :: k_he4_zr82__n_mo85   = 5537
  integer, parameter :: k_he4_zr82__p_nb85   = 5538
  integer, parameter :: k_n_zr83__p_y83   = 5539
  integer, parameter :: k_n_zr83__he4_sr80   = 5540
  integer, parameter :: k_p_zr83__n_nb83   = 5541
  integer, parameter :: k_p_zr83__he4_y80   = 5542
  integer, parameter :: k_he4_zr83__n_mo86   = 5543
  integer, parameter :: k_he4_zr83__p_nb86   = 5544
  integer, parameter :: k_n_zr84__p_y84   = 5545
  integer, parameter :: k_n_zr84__he4_sr81   = 5546
  integer, parameter :: k_p_zr84__n_nb84   = 5547
  integer, parameter :: k_p_zr84__he4_y81   = 5548
  integer, parameter :: k_he4_zr84__n_mo87   = 5549
  integer, parameter :: k_he4_zr84__p_nb87   = 5550
  integer, parameter :: k_n_zr85__p_y85   = 5551
  integer, parameter :: k_n_zr85__he4_sr82   = 5552
  integer, parameter :: k_p_zr85__n_nb85   = 5553
  integer, parameter :: k_p_zr85__he4_y82   = 5554
  integer, parameter :: k_he4_zr85__n_mo88   = 5555
  integer, parameter :: k_he4_zr85__p_nb88   = 5556
  integer, parameter :: k_n_zr86__p_y86   = 5557
  integer, parameter :: k_n_zr86__he4_sr83   = 5558
  integer, parameter :: k_p_zr86__n_nb86   = 5559
  integer, parameter :: k_p_zr86__he4_y83   = 5560
  integer, parameter :: k_he4_zr86__n_mo89   = 5561
  integer, parameter :: k_he4_zr86__p_nb89   = 5562
  integer, parameter :: k_n_zr87__p_y87   = 5563
  integer, parameter :: k_n_zr87__he4_sr84   = 5564
  integer, parameter :: k_p_zr87__n_nb87   = 5565
  integer, parameter :: k_p_zr87__he4_y84   = 5566
  integer, parameter :: k_he4_zr87__n_mo90   = 5567
  integer, parameter :: k_he4_zr87__p_nb90   = 5568
  integer, parameter :: k_p_zr88__n_nb88   = 5569
  integer, parameter :: k_p_zr88__he4_y85   = 5570
  integer, parameter :: k_p_zr89__n_nb89   = 5571
  integer, parameter :: k_p_zr89__he4_y86   = 5572
  integer, parameter :: k_p_zr90__n_nb90   = 5573
  integer, parameter :: k_p_zr90__he4_y87   = 5574
  integer, parameter :: k_n_nb82__p_zr82   = 5575
  integer, parameter :: k_n_nb82__he4_y79   = 5576
  integer, parameter :: k_p_nb82__he4_zr79   = 5577
  integer, parameter :: k_he4_nb82__p_mo85   = 5578
  integer, parameter :: k_n_nb83__p_zr83   = 5579
  integer, parameter :: k_n_nb83__he4_y80   = 5580
  integer, parameter :: k_p_nb83__n_mo83   = 5581
  integer, parameter :: k_p_nb83__he4_zr80   = 5582
  integer, parameter :: k_he4_nb83__p_mo86   = 5583
  integer, parameter :: k_n_nb84__p_zr84   = 5584
  integer, parameter :: k_n_nb84__he4_y81   = 5585
  integer, parameter :: k_p_nb84__n_mo84   = 5586
  integer, parameter :: k_p_nb84__he4_zr81   = 5587
  integer, parameter :: k_he4_nb84__p_mo87   = 5588
  integer, parameter :: k_n_nb85__p_zr85   = 5589
  integer, parameter :: k_n_nb85__he4_y82   = 5590
  integer, parameter :: k_p_nb85__n_mo85   = 5591
  integer, parameter :: k_p_nb85__he4_zr82   = 5592
  integer, parameter :: k_he4_nb85__p_mo88   = 5593
  integer, parameter :: k_n_nb86__p_zr86   = 5594
  integer, parameter :: k_n_nb86__he4_y83   = 5595
  integer, parameter :: k_p_nb86__n_mo86   = 5596
  integer, parameter :: k_p_nb86__he4_zr83   = 5597
  integer, parameter :: k_he4_nb86__n_tc89   = 5598
  integer, parameter :: k_he4_nb86__p_mo89   = 5599
  integer, parameter :: k_n_nb87__p_zr87   = 5600
  integer, parameter :: k_n_nb87__he4_y84   = 5601
  integer, parameter :: k_p_nb87__n_mo87   = 5602
  integer, parameter :: k_p_nb87__he4_zr84   = 5603
  integer, parameter :: k_he4_nb87__n_tc90   = 5604
  integer, parameter :: k_he4_nb87__p_mo90   = 5605
  integer, parameter :: k_n_nb88__p_zr88   = 5606
  integer, parameter :: k_n_nb88__he4_y85   = 5607
  integer, parameter :: k_p_nb88__n_mo88   = 5608
  integer, parameter :: k_p_nb88__he4_zr85   = 5609
  integer, parameter :: k_he4_nb88__n_tc91   = 5610
  integer, parameter :: k_n_nb89__p_zr89   = 5611
  integer, parameter :: k_n_nb89__he4_y86   = 5612
  integer, parameter :: k_p_nb89__n_mo89   = 5613
  integer, parameter :: k_p_nb89__he4_zr86   = 5614
  integer, parameter :: k_n_nb90__p_zr90   = 5615
  integer, parameter :: k_n_nb90__he4_y87   = 5616
  integer, parameter :: k_p_nb90__n_mo90   = 5617
  integer, parameter :: k_p_nb90__he4_zr87   = 5618
  integer, parameter :: k_n_mo83__p_nb83   = 5619
  integer, parameter :: k_n_mo83__he4_zr80   = 5620
  integer, parameter :: k_n_mo84__p_nb84   = 5621
  integer, parameter :: k_n_mo84__he4_zr81   = 5622
  integer, parameter :: k_n_mo85__p_nb85   = 5623
  integer, parameter :: k_n_mo85__he4_zr82   = 5624
  integer, parameter :: k_p_mo85__he4_nb82   = 5625
  integer, parameter :: k_n_mo86__p_nb86   = 5626
  integer, parameter :: k_n_mo86__he4_zr83   = 5627
  integer, parameter :: k_p_mo86__he4_nb83   = 5628
  integer, parameter :: k_he4_mo86__p_tc89   = 5629
  integer, parameter :: k_n_mo87__p_nb87   = 5630
  integer, parameter :: k_n_mo87__he4_zr84   = 5631
  integer, parameter :: k_p_mo87__he4_nb84   = 5632
  integer, parameter :: k_he4_mo87__p_tc90   = 5633
  integer, parameter :: k_n_mo88__p_nb88   = 5634
  integer, parameter :: k_n_mo88__he4_zr85   = 5635
  integer, parameter :: k_p_mo88__he4_nb85   = 5636
  integer, parameter :: k_he4_mo88__p_tc91   = 5637
  integer, parameter :: k_n_mo89__p_nb89   = 5638
  integer, parameter :: k_n_mo89__he4_zr86   = 5639
  integer, parameter :: k_p_mo89__n_tc89   = 5640
  integer, parameter :: k_p_mo89__he4_nb86   = 5641
  integer, parameter :: k_n_mo90__p_nb90   = 5642
  integer, parameter :: k_n_mo90__he4_zr87   = 5643
  integer, parameter :: k_p_mo90__n_tc90   = 5644
  integer, parameter :: k_p_mo90__he4_nb87   = 5645
  integer, parameter :: k_n_tc89__p_mo89   = 5646
  integer, parameter :: k_n_tc89__he4_nb86   = 5647
  integer, parameter :: k_p_tc89__he4_mo86   = 5648
  integer, parameter :: k_n_tc90__p_mo90   = 5649
  integer, parameter :: k_n_tc90__he4_nb87   = 5650
  integer, parameter :: k_p_tc90__he4_mo87   = 5651
  integer, parameter :: k_n_tc91__he4_nb88   = 5652
  integer, parameter :: k_p_tc91__he4_mo88   = 5653
  integer, parameter :: k_p_d__n_p_p   = 5654
  integer, parameter :: k_t_t__n_n_he4   = 5655
  integer, parameter :: k_t_he3__n_p_he4   = 5656
  integer, parameter :: k_he3_he3__p_p_he4   = 5657
  integer, parameter :: k_d_li7__n_he4_he4   = 5658
  integer, parameter :: k_d_be7__p_he4_he4   = 5659
  integer, parameter :: k_p_be9__d_he4_he4   = 5660
  integer, parameter :: k_n_b8__p_he4_he4   = 5661
  integer, parameter :: k_p_b11__he4_he4_he4   = 5662
  integer, parameter :: k_n_c11__he4_he4_he4   = 5663
  integer, parameter :: k_he4_ca36__p_p_ca38   = 5664
  integer, parameter :: k_t_li7__n_n_he4_he4   = 5665
  integer, parameter :: k_he3_li7__n_p_he4_he4   = 5666
  integer, parameter :: k_t_be7__n_p_he4_he4   = 5667
  integer, parameter :: k_he3_be7__p_p_he4_he4   = 5668
  integer, parameter :: k_p_be9__n_p_he4_he4   = 5669
  integer, parameter :: k_n_p_he4__li6   = 5670
  integer, parameter :: k_n_he4_he4__be9   = 5671
  integer, parameter :: k_he4_he4_he4__c12   = 5672
  integer, parameter :: k_n_p_p__p   = 5673
  integer, parameter :: k_n_n_he4__t   = 5674
  integer, parameter :: k_n_p_he4__t   = 5675
  integer, parameter :: k_p_p_he4__he3   = 5676
  integer, parameter :: k_n_he4_he4__p   = 5677
  integer, parameter :: k_n_he4_he4__d   = 5678
  integer, parameter :: k_p_he4_he4__n   = 5679
  integer, parameter :: k_p_he4_he4__d   = 5680
  integer, parameter :: k_d_he4_he4__p   = 5681
  integer, parameter :: k_he4_he4_he4__n   = 5682
  integer, parameter :: k_he4_he4_he4__p   = 5683
  integer, parameter :: k_p_p_o15__he4   = 5684
  integer, parameter :: k_p_p_ca38__he4   = 5685
  integer, parameter :: k_f20__o20   = 5686
  integer, parameter :: k_ne20__f20   = 5687
  integer, parameter :: k_o20__f20   = 5688
  integer, parameter :: k_f20__ne20   = 5689

  real(rt), allocatable, save :: bion(:), mion(:)

#ifdef AMREX_USE_CUDA
  attributes(managed) :: bion, mion
#endif

  !$acc declare create(bion, mion)

#ifdef REACT_SPARSE_JACOBIAN
  ! Shape of Jacobian in Compressed Sparse Row format
  integer, parameter   :: NETWORK_SPARSE_JAC_NNZ = 10179
  integer, allocatable :: csr_jac_col_index(:), csr_jac_row_count(:)

#ifdef AMREX_USE_CUDA
  attributes(managed) :: csr_jac_col_index, csr_jac_row_count
#endif
#endif

contains

  subroutine actual_network_init()

    implicit none

    integer :: i

    call network_properties_init()

    ! Allocate ion info arrays
    allocate(bion(nspec))
    allocate(mion(nspec))

    ebind_per_nucleon(jn)   = 0.00000000000000e+00_rt
    ebind_per_nucleon(jp)   = 0.00000000000000e+00_rt
    ebind_per_nucleon(jd)   = 1.11228300000000e+00_rt
    ebind_per_nucleon(jt)   = 2.82726500000000e+00_rt
    ebind_per_nucleon(jhe3)   = 2.57268000000000e+00_rt
    ebind_per_nucleon(jhe4)   = 7.07391500000000e+00_rt
    ebind_per_nucleon(jli6)   = 5.33233100000000e+00_rt
    ebind_per_nucleon(jli7)   = 5.60643900000000e+00_rt
    ebind_per_nucleon(jbe7)   = 5.37154800000000e+00_rt
    ebind_per_nucleon(jbe9)   = 6.46266800000000e+00_rt
    ebind_per_nucleon(jb8)   = 4.71715500000000e+00_rt
    ebind_per_nucleon(jb10)   = 6.47508300000000e+00_rt
    ebind_per_nucleon(jb11)   = 6.92773200000000e+00_rt
    ebind_per_nucleon(jc11)   = 6.67645600000000e+00_rt
    ebind_per_nucleon(jc12)   = 7.68014400000000e+00_rt
    ebind_per_nucleon(jc13)   = 7.46984900000000e+00_rt
    ebind_per_nucleon(jc14)   = 7.52031900000000e+00_rt
    ebind_per_nucleon(jn12)   = 6.17010900000000e+00_rt
    ebind_per_nucleon(jn13)   = 7.23886300000000e+00_rt
    ebind_per_nucleon(jn14)   = 7.47561400000000e+00_rt
    ebind_per_nucleon(jn15)   = 7.69946000000000e+00_rt
    ebind_per_nucleon(jo14)   = 7.05227800000000e+00_rt
    ebind_per_nucleon(jo15)   = 7.46369200000000e+00_rt
    ebind_per_nucleon(jo16)   = 7.97620600000000e+00_rt
    ebind_per_nucleon(jo17)   = 7.75072800000000e+00_rt
    ebind_per_nucleon(jo18)   = 7.76709700000000e+00_rt
    ebind_per_nucleon(jo19)   = 7.56649500000000e+00_rt
    ebind_per_nucleon(jo20)   = 7.56857000000000e+00_rt
    ebind_per_nucleon(jf17)   = 7.54232800000000e+00_rt
    ebind_per_nucleon(jf18)   = 7.63163800000000e+00_rt
    ebind_per_nucleon(jf19)   = 7.77901800000000e+00_rt
    ebind_per_nucleon(jf20)   = 7.72013400000000e+00_rt
    ebind_per_nucleon(jf21)   = 7.73829300000000e+00_rt
    ebind_per_nucleon(jne17)   = 6.64049900000000e+00_rt
    ebind_per_nucleon(jne18)   = 7.34125700000000e+00_rt
    ebind_per_nucleon(jne19)   = 7.56734300000000e+00_rt
    ebind_per_nucleon(jne20)   = 8.03224000000000e+00_rt
    ebind_per_nucleon(jne21)   = 7.97171300000000e+00_rt
    ebind_per_nucleon(jne22)   = 8.08046500000000e+00_rt
    ebind_per_nucleon(jne23)   = 7.95525600000000e+00_rt
    ebind_per_nucleon(jne24)   = 7.99332500000000e+00_rt
    ebind_per_nucleon(jna19)   = 6.93788500000000e+00_rt
    ebind_per_nucleon(jna20)   = 7.29849600000000e+00_rt
    ebind_per_nucleon(jna21)   = 7.76554700000000e+00_rt
    ebind_per_nucleon(jna22)   = 7.91566700000000e+00_rt
    ebind_per_nucleon(jna23)   = 8.11149300000000e+00_rt
    ebind_per_nucleon(jna24)   = 8.06348800000000e+00_rt
    ebind_per_nucleon(jna25)   = 8.10139700000000e+00_rt
    ebind_per_nucleon(jna26)   = 8.00420100000000e+00_rt
    ebind_per_nucleon(jna27)   = 7.95694600000000e+00_rt
    ebind_per_nucleon(jmg20)   = 6.72802500000000e+00_rt
    ebind_per_nucleon(jmg21)   = 7.10503100000000e+00_rt
    ebind_per_nucleon(jmg22)   = 7.66276100000000e+00_rt
    ebind_per_nucleon(jmg23)   = 7.90111500000000e+00_rt
    ebind_per_nucleon(jmg24)   = 8.26070900000000e+00_rt
    ebind_per_nucleon(jmg25)   = 8.22350200000000e+00_rt
    ebind_per_nucleon(jmg26)   = 8.33387000000000e+00_rt
    ebind_per_nucleon(jmg27)   = 8.26385200000000e+00_rt
    ebind_per_nucleon(jmg28)   = 8.27241300000000e+00_rt
    ebind_per_nucleon(jmg29)   = 8.11320200000000e+00_rt
    ebind_per_nucleon(jal22)   = 6.78200000000000e+00_rt
    ebind_per_nucleon(jal23)   = 7.33572700000000e+00_rt
    ebind_per_nucleon(jal24)   = 7.64958200000000e+00_rt
    ebind_per_nucleon(jal25)   = 8.02113600000000e+00_rt
    ebind_per_nucleon(jal26)   = 8.14976500000000e+00_rt
    ebind_per_nucleon(jal27)   = 8.33155300000000e+00_rt
    ebind_per_nucleon(jal28)   = 8.30989400000000e+00_rt
    ebind_per_nucleon(jal29)   = 8.34846400000000e+00_rt
    ebind_per_nucleon(jal30)   = 8.26112800000000e+00_rt
    ebind_per_nucleon(jal31)   = 8.22551700000000e+00_rt
    ebind_per_nucleon(jsi23)   = 6.56500000000000e+00_rt
    ebind_per_nucleon(jsi24)   = 7.16723200000000e+00_rt
    ebind_per_nucleon(jsi25)   = 7.48011000000000e+00_rt
    ebind_per_nucleon(jsi26)   = 7.92470800000000e+00_rt
    ebind_per_nucleon(jsi27)   = 8.12434100000000e+00_rt
    ebind_per_nucleon(jsi28)   = 8.44774400000000e+00_rt
    ebind_per_nucleon(jsi29)   = 8.44863500000000e+00_rt
    ebind_per_nucleon(jsi30)   = 8.52065400000000e+00_rt
    ebind_per_nucleon(jsi31)   = 8.45829100000000e+00_rt
    ebind_per_nucleon(jsi32)   = 8.48146800000000e+00_rt
    ebind_per_nucleon(jsi33)   = 8.36105900000000e+00_rt
    ebind_per_nucleon(jsi34)   = 8.33614100000000e+00_rt
    ebind_per_nucleon(jp27)   = 7.66343800000000e+00_rt
    ebind_per_nucleon(jp28)   = 7.90747900000000e+00_rt
    ebind_per_nucleon(jp29)   = 8.25123600000000e+00_rt
    ebind_per_nucleon(jp30)   = 8.35350600000000e+00_rt
    ebind_per_nucleon(jp31)   = 8.48116700000000e+00_rt
    ebind_per_nucleon(jp32)   = 8.46412000000000e+00_rt
    ebind_per_nucleon(jp33)   = 8.51380600000000e+00_rt
    ebind_per_nucleon(jp34)   = 8.44818500000000e+00_rt
    ebind_per_nucleon(jp35)   = 8.44624900000000e+00_rt
    ebind_per_nucleon(jp36)   = 8.30786800000000e+00_rt
    ebind_per_nucleon(jp37)   = 8.26755500000000e+00_rt
    ebind_per_nucleon(jp38)   = 8.14727400000000e+00_rt
    ebind_per_nucleon(js28)   = 7.47879000000000e+00_rt
    ebind_per_nucleon(js29)   = 7.74852000000000e+00_rt
    ebind_per_nucleon(js30)   = 8.12270700000000e+00_rt
    ebind_per_nucleon(js31)   = 8.28180000000000e+00_rt
    ebind_per_nucleon(js32)   = 8.49312900000000e+00_rt
    ebind_per_nucleon(js33)   = 8.49763000000000e+00_rt
    ebind_per_nucleon(js34)   = 8.58349800000000e+00_rt
    ebind_per_nucleon(js35)   = 8.53785000000000e+00_rt
    ebind_per_nucleon(js36)   = 8.57538900000000e+00_rt
    ebind_per_nucleon(js37)   = 8.45993500000000e+00_rt
    ebind_per_nucleon(js38)   = 8.44878200000000e+00_rt
    ebind_per_nucleon(js39)   = 8.34426900000000e+00_rt
    ebind_per_nucleon(js40)   = 8.32932500000000e+00_rt
    ebind_per_nucleon(js41)   = 8.22963500000000e+00_rt
    ebind_per_nucleon(js42)   = 8.19322700000000e+00_rt
    ebind_per_nucleon(jcl31)   = 7.86920900000000e+00_rt
    ebind_per_nucleon(jcl32)   = 8.07240400000000e+00_rt
    ebind_per_nucleon(jcl33)   = 8.30475500000000e+00_rt
    ebind_per_nucleon(jcl34)   = 8.39897000000000e+00_rt
    ebind_per_nucleon(jcl35)   = 8.52027800000000e+00_rt
    ebind_per_nucleon(jcl36)   = 8.52193100000000e+00_rt
    ebind_per_nucleon(jcl37)   = 8.57028100000000e+00_rt
    ebind_per_nucleon(jcl38)   = 8.50548100000000e+00_rt
    ebind_per_nucleon(jcl39)   = 8.49440200000000e+00_rt
    ebind_per_nucleon(jcl40)   = 8.42776500000000e+00_rt
    ebind_per_nucleon(jcl41)   = 8.41295900000000e+00_rt
    ebind_per_nucleon(jcl42)   = 8.34588600000000e+00_rt
    ebind_per_nucleon(jcl43)   = 8.32386600000000e+00_rt
    ebind_per_nucleon(jcl44)   = 8.23233200000000e+00_rt
    ebind_per_nucleon(jcl45)   = 8.18159800000000e+00_rt
    ebind_per_nucleon(jar32)   = 7.70000800000000e+00_rt
    ebind_per_nucleon(jar33)   = 7.92895500000000e+00_rt
    ebind_per_nucleon(jar34)   = 8.19767200000000e+00_rt
    ebind_per_nucleon(jar35)   = 8.32746100000000e+00_rt
    ebind_per_nucleon(jar36)   = 8.51990900000000e+00_rt
    ebind_per_nucleon(jar37)   = 8.52713900000000e+00_rt
    ebind_per_nucleon(jar38)   = 8.61428000000000e+00_rt
    ebind_per_nucleon(jar39)   = 8.56259800000000e+00_rt
    ebind_per_nucleon(jar40)   = 8.59525900000000e+00_rt
    ebind_per_nucleon(jar41)   = 8.53437200000000e+00_rt
    ebind_per_nucleon(jar42)   = 8.55561300000000e+00_rt
    ebind_per_nucleon(jar43)   = 8.48823700000000e+00_rt
    ebind_per_nucleon(jar44)   = 8.49384000000000e+00_rt
    ebind_per_nucleon(jar45)   = 8.41995200000000e+00_rt
    ebind_per_nucleon(jar46)   = 8.41241900000000e+00_rt
    ebind_per_nucleon(jk35)   = 7.96584000000000e+00_rt
    ebind_per_nucleon(jk36)   = 8.14221900000000e+00_rt
    ebind_per_nucleon(jk37)   = 8.33984700000000e+00_rt
    ebind_per_nucleon(jk38)   = 8.43805800000000e+00_rt
    ebind_per_nucleon(jk39)   = 8.55702500000000e+00_rt
    ebind_per_nucleon(jk40)   = 8.53809000000000e+00_rt
    ebind_per_nucleon(jk41)   = 8.57607200000000e+00_rt
    ebind_per_nucleon(jk42)   = 8.55125600000000e+00_rt
    ebind_per_nucleon(jk43)   = 8.57622000000000e+00_rt
    ebind_per_nucleon(jk44)   = 8.54670100000000e+00_rt
    ebind_per_nucleon(jk45)   = 8.55467400000000e+00_rt
    ebind_per_nucleon(jk46)   = 8.51804200000000e+00_rt
    ebind_per_nucleon(jk47)   = 8.51487900000000e+00_rt
    ebind_per_nucleon(jk48)   = 8.43423200000000e+00_rt
    ebind_per_nucleon(jk49)   = 8.37227400000000e+00_rt
    ebind_per_nucleon(jca36)   = 7.81587900000000e+00_rt
    ebind_per_nucleon(jca37)   = 8.00345600000000e+00_rt
    ebind_per_nucleon(jca38)   = 8.24004300000000e+00_rt
    ebind_per_nucleon(jca39)   = 8.36967000000000e+00_rt
    ebind_per_nucleon(jca40)   = 8.55130300000000e+00_rt
    ebind_per_nucleon(jca41)   = 8.54670600000000e+00_rt
    ebind_per_nucleon(jca42)   = 8.61656300000000e+00_rt
    ebind_per_nucleon(jca43)   = 8.60066300000000e+00_rt
    ebind_per_nucleon(jca44)   = 8.65817500000000e+00_rt
    ebind_per_nucleon(jca45)   = 8.63054500000000e+00_rt
    ebind_per_nucleon(jca46)   = 8.66897900000000e+00_rt
    ebind_per_nucleon(jca47)   = 8.63934900000000e+00_rt
    ebind_per_nucleon(jca48)   = 8.66668600000000e+00_rt
    ebind_per_nucleon(jca49)   = 8.59484400000000e+00_rt
    ebind_per_nucleon(jsc40)   = 8.17366900000000e+00_rt
    ebind_per_nucleon(jsc41)   = 8.36919800000000e+00_rt
    ebind_per_nucleon(jsc42)   = 8.44493300000000e+00_rt
    ebind_per_nucleon(jsc43)   = 8.53082500000000e+00_rt
    ebind_per_nucleon(jsc44)   = 8.55737900000000e+00_rt
    ebind_per_nucleon(jsc45)   = 8.61893100000000e+00_rt
    ebind_per_nucleon(jsc46)   = 8.62201200000000e+00_rt
    ebind_per_nucleon(jsc47)   = 8.66509000000000e+00_rt
    ebind_per_nucleon(jsc48)   = 8.65620400000000e+00_rt
    ebind_per_nucleon(jsc49)   = 8.68625600000000e+00_rt
    ebind_per_nucleon(jsc50)   = 8.63367900000000e+00_rt
    ebind_per_nucleon(jsc51)   = 8.59679600000000e+00_rt
    ebind_per_nucleon(jti41)   = 8.03438800000000e+00_rt
    ebind_per_nucleon(jti42)   = 8.25924700000000e+00_rt
    ebind_per_nucleon(jti43)   = 8.35293200000000e+00_rt
    ebind_per_nucleon(jti44)   = 8.53352000000000e+00_rt
    ebind_per_nucleon(jti45)   = 8.55572200000000e+00_rt
    ebind_per_nucleon(jti46)   = 8.65645100000000e+00_rt
    ebind_per_nucleon(jti47)   = 8.66122700000000e+00_rt
    ebind_per_nucleon(jti48)   = 8.72300600000000e+00_rt
    ebind_per_nucleon(jti49)   = 8.71115700000000e+00_rt
    ebind_per_nucleon(jti50)   = 8.75571800000000e+00_rt
    ebind_per_nucleon(jti51)   = 8.70898800000000e+00_rt
    ebind_per_nucleon(jti52)   = 8.69166700000000e+00_rt
    ebind_per_nucleon(jti53)   = 8.63017400000000e+00_rt
    ebind_per_nucleon(jv43)   = 8.06951200000000e+00_rt
    ebind_per_nucleon(jv44)   = 8.21046300000000e+00_rt
    ebind_per_nucleon(jv45)   = 8.38002900000000e+00_rt
    ebind_per_nucleon(jv46)   = 8.48613000000000e+00_rt
    ebind_per_nucleon(jv47)   = 8.58222500000000e+00_rt
    ebind_per_nucleon(jv48)   = 8.62306100000000e+00_rt
    ebind_per_nucleon(jv49)   = 8.68290800000000e+00_rt
    ebind_per_nucleon(jv50)   = 8.69591800000000e+00_rt
    ebind_per_nucleon(jv51)   = 8.74209900000000e+00_rt
    ebind_per_nucleon(jv52)   = 8.71458200000000e+00_rt
    ebind_per_nucleon(jv53)   = 8.71013000000000e+00_rt
    ebind_per_nucleon(jv54)   = 8.66204300000000e+00_rt
    ebind_per_nucleon(jv55)   = 8.63769200000000e+00_rt
    ebind_per_nucleon(jcr44)   = 7.94800000000000e+00_rt
    ebind_per_nucleon(jcr45)   = 8.08772800000000e+00_rt
    ebind_per_nucleon(jcr46)   = 8.30382300000000e+00_rt
    ebind_per_nucleon(jcr47)   = 8.40719500000000e+00_rt
    ebind_per_nucleon(jcr48)   = 8.57226900000000e+00_rt
    ebind_per_nucleon(jcr49)   = 8.61329100000000e+00_rt
    ebind_per_nucleon(jcr50)   = 8.70103200000000e+00_rt
    ebind_per_nucleon(jcr51)   = 8.71200500000000e+00_rt
    ebind_per_nucleon(jcr52)   = 8.77598900000000e+00_rt
    ebind_per_nucleon(jcr53)   = 8.76019800000000e+00_rt
    ebind_per_nucleon(jcr54)   = 8.77795500000000e+00_rt
    ebind_per_nucleon(jcr55)   = 8.73192400000000e+00_rt
    ebind_per_nucleon(jcr56)   = 8.72325800000000e+00_rt
    ebind_per_nucleon(jcr57)   = 8.66339400000000e+00_rt
    ebind_per_nucleon(jcr58)   = 8.64399800000000e+00_rt
    ebind_per_nucleon(jmn46)   = 7.91900000000000e+00_rt
    ebind_per_nucleon(jmn47)   = 8.13531100000000e+00_rt
    ebind_per_nucleon(jmn48)   = 8.27418500000000e+00_rt
    ebind_per_nucleon(jmn49)   = 8.43992900000000e+00_rt
    ebind_per_nucleon(jmn50)   = 8.53269600000000e+00_rt
    ebind_per_nucleon(jmn51)   = 8.63377200000000e+00_rt
    ebind_per_nucleon(jmn52)   = 8.67032900000000e+00_rt
    ebind_per_nucleon(jmn53)   = 8.73417500000000e+00_rt
    ebind_per_nucleon(jmn54)   = 8.73796500000000e+00_rt
    ebind_per_nucleon(jmn55)   = 8.76502200000000e+00_rt
    ebind_per_nucleon(jmn56)   = 8.73833300000000e+00_rt
    ebind_per_nucleon(jmn57)   = 8.73671300000000e+00_rt
    ebind_per_nucleon(jmn58)   = 8.69664300000000e+00_rt
    ebind_per_nucleon(jmn59)   = 8.68092100000000e+00_rt
    ebind_per_nucleon(jmn60)   = 8.62813800000000e+00_rt
    ebind_per_nucleon(jmn61)   = 8.59891500000000e+00_rt
    ebind_per_nucleon(jfe47)   = 7.78500000000000e+00_rt
    ebind_per_nucleon(jfe48)   = 8.02300000000000e+00_rt
    ebind_per_nucleon(jfe49)   = 8.16131100000000e+00_rt
    ebind_per_nucleon(jfe50)   = 8.35402600000000e+00_rt
    ebind_per_nucleon(jfe51)   = 8.46075900000000e+00_rt
    ebind_per_nucleon(jfe52)   = 8.60957400000000e+00_rt
    ebind_per_nucleon(jfe53)   = 8.64879900000000e+00_rt
    ebind_per_nucleon(jfe54)   = 8.73638200000000e+00_rt
    ebind_per_nucleon(jfe55)   = 8.74659500000000e+00_rt
    ebind_per_nucleon(jfe56)   = 8.79035400000000e+00_rt
    ebind_per_nucleon(jfe57)   = 8.77027900000000e+00_rt
    ebind_per_nucleon(jfe58)   = 8.79225000000000e+00_rt
    ebind_per_nucleon(jfe59)   = 8.75477100000000e+00_rt
    ebind_per_nucleon(jfe60)   = 8.75585100000000e+00_rt
    ebind_per_nucleon(jfe61)   = 8.70376800000000e+00_rt
    ebind_per_nucleon(jfe62)   = 8.69288200000000e+00_rt
    ebind_per_nucleon(jfe63)   = 8.63154900000000e+00_rt
    ebind_per_nucleon(jfe64)   = 8.61238800000000e+00_rt
    ebind_per_nucleon(jfe65)   = 8.54634600000000e+00_rt
    ebind_per_nucleon(jfe66)   = 8.52172400000000e+00_rt
    ebind_per_nucleon(jco50)   = 8.00100000000000e+00_rt
    ebind_per_nucleon(jco51)   = 8.19325400000000e+00_rt
    ebind_per_nucleon(jco52)   = 8.32588600000000e+00_rt
    ebind_per_nucleon(jco53)   = 8.47765800000000e+00_rt
    ebind_per_nucleon(jco54)   = 8.56921700000000e+00_rt
    ebind_per_nucleon(jco55)   = 8.66961800000000e+00_rt
    ebind_per_nucleon(jco56)   = 8.69483600000000e+00_rt
    ebind_per_nucleon(jco57)   = 8.74188200000000e+00_rt
    ebind_per_nucleon(jco58)   = 8.73896900000000e+00_rt
    ebind_per_nucleon(jco59)   = 8.76803500000000e+00_rt
    ebind_per_nucleon(jco60)   = 8.74676600000000e+00_rt
    ebind_per_nucleon(jco61)   = 8.75614800000000e+00_rt
    ebind_per_nucleon(jco62)   = 8.72133200000000e+00_rt
    ebind_per_nucleon(jco63)   = 8.71779500000000e+00_rt
    ebind_per_nucleon(jco64)   = 8.67552000000000e+00_rt
    ebind_per_nucleon(jco65)   = 8.65688400000000e+00_rt
    ebind_per_nucleon(jco66)   = 8.60594100000000e+00_rt
    ebind_per_nucleon(jco67)   = 8.58174100000000e+00_rt
    ebind_per_nucleon(jni51)   = 7.87500000000000e+00_rt
    ebind_per_nucleon(jni52)   = 8.07900000000000e+00_rt
    ebind_per_nucleon(jni53)   = 8.21707400000000e+00_rt
    ebind_per_nucleon(jni54)   = 8.39303200000000e+00_rt
    ebind_per_nucleon(jni55)   = 8.49732000000000e+00_rt
    ebind_per_nucleon(jni56)   = 8.64277900000000e+00_rt
    ebind_per_nucleon(jni57)   = 8.67093300000000e+00_rt
    ebind_per_nucleon(jni58)   = 8.73205900000000e+00_rt
    ebind_per_nucleon(jni59)   = 8.73658800000000e+00_rt
    ebind_per_nucleon(jni60)   = 8.78077400000000e+00_rt
    ebind_per_nucleon(jni61)   = 8.76502500000000e+00_rt
    ebind_per_nucleon(jni62)   = 8.79455300000000e+00_rt
    ebind_per_nucleon(jni63)   = 8.76349300000000e+00_rt
    ebind_per_nucleon(jni64)   = 8.77746100000000e+00_rt
    ebind_per_nucleon(jni65)   = 8.73624000000000e+00_rt
    ebind_per_nucleon(jni66)   = 8.73950800000000e+00_rt
    ebind_per_nucleon(jni67)   = 8.69575000000000e+00_rt
    ebind_per_nucleon(jni68)   = 8.68246600000000e+00_rt
    ebind_per_nucleon(jcu55)   = 8.23399600000000e+00_rt
    ebind_per_nucleon(jcu56)   = 8.35622700000000e+00_rt
    ebind_per_nucleon(jcu57)   = 8.50326200000000e+00_rt
    ebind_per_nucleon(jcu58)   = 8.57096700000000e+00_rt
    ebind_per_nucleon(jcu59)   = 8.64200000000000e+00_rt
    ebind_per_nucleon(jcu60)   = 8.66560200000000e+00_rt
    ebind_per_nucleon(jcu61)   = 8.71551400000000e+00_rt
    ebind_per_nucleon(jcu62)   = 8.71808100000000e+00_rt
    ebind_per_nucleon(jcu63)   = 8.75213800000000e+00_rt
    ebind_per_nucleon(jcu64)   = 8.73907500000000e+00_rt
    ebind_per_nucleon(jcu65)   = 8.75709600000000e+00_rt
    ebind_per_nucleon(jcu66)   = 8.73147200000000e+00_rt
    ebind_per_nucleon(jcu67)   = 8.73745800000000e+00_rt
    ebind_per_nucleon(jcu68)   = 8.70189000000000e+00_rt
    ebind_per_nucleon(jcu69)   = 8.69520400000000e+00_rt
    ebind_per_nucleon(jzn57)   = 8.23100000000000e+00_rt
    ebind_per_nucleon(jzn58)   = 8.39594400000000e+00_rt
    ebind_per_nucleon(jzn59)   = 8.47377700000000e+00_rt
    ebind_per_nucleon(jzn60)   = 8.58305000000000e+00_rt
    ebind_per_nucleon(jzn61)   = 8.61030900000000e+00_rt
    ebind_per_nucleon(jzn62)   = 8.67934300000000e+00_rt
    ebind_per_nucleon(jzn63)   = 8.68628500000000e+00_rt
    ebind_per_nucleon(jzn64)   = 8.73590500000000e+00_rt
    ebind_per_nucleon(jzn65)   = 8.72426500000000e+00_rt
    ebind_per_nucleon(jzn66)   = 8.75963200000000e+00_rt
    ebind_per_nucleon(jzn67)   = 8.73415200000000e+00_rt
    ebind_per_nucleon(jzn68)   = 8.75568000000000e+00_rt
    ebind_per_nucleon(jzn69)   = 8.72272900000000e+00_rt
    ebind_per_nucleon(jzn70)   = 8.72980800000000e+00_rt
    ebind_per_nucleon(jzn71)   = 8.68904100000000e+00_rt
    ebind_per_nucleon(jzn72)   = 8.69180500000000e+00_rt
    ebind_per_nucleon(jga59)   = 8.23200000000000e+00_rt
    ebind_per_nucleon(jga60)   = 8.32700000000000e+00_rt
    ebind_per_nucleon(jga61)   = 8.44643100000000e+00_rt
    ebind_per_nucleon(jga62)   = 8.51864200000000e+00_rt
    ebind_per_nucleon(jga63)   = 8.58392600000000e+00_rt
    ebind_per_nucleon(jga64)   = 8.61163100000000e+00_rt
    ebind_per_nucleon(jga65)   = 8.66216000000000e+00_rt
    ebind_per_nucleon(jga66)   = 8.66936100000000e+00_rt
    ebind_per_nucleon(jga67)   = 8.70753100000000e+00_rt
    ebind_per_nucleon(jga68)   = 8.70121800000000e+00_rt
    ebind_per_nucleon(jga69)   = 8.72457900000000e+00_rt
    ebind_per_nucleon(jga70)   = 8.70928000000000e+00_rt
    ebind_per_nucleon(jga71)   = 8.71760400000000e+00_rt
    ebind_per_nucleon(jga72)   = 8.68708900000000e+00_rt
    ebind_per_nucleon(jga73)   = 8.69387300000000e+00_rt
    ebind_per_nucleon(jga74)   = 8.66316700000000e+00_rt
    ebind_per_nucleon(jga75)   = 8.66080800000000e+00_rt
    ebind_per_nucleon(jge62)   = 8.34100000000000e+00_rt
    ebind_per_nucleon(jge63)   = 8.41871600000000e+00_rt
    ebind_per_nucleon(jge64)   = 8.52882300000000e+00_rt
    ebind_per_nucleon(jge65)   = 8.55505800000000e+00_rt
    ebind_per_nucleon(jge66)   = 8.62543700000000e+00_rt
    ebind_per_nucleon(jge67)   = 8.63285700000000e+00_rt
    ebind_per_nucleon(jge68)   = 8.68813600000000e+00_rt
    ebind_per_nucleon(jge69)   = 8.68096300000000e+00_rt
    ebind_per_nucleon(jge70)   = 8.72170000000000e+00_rt
    ebind_per_nucleon(jge71)   = 8.70330900000000e+00_rt
    ebind_per_nucleon(jge72)   = 8.73174500000000e+00_rt
    ebind_per_nucleon(jge73)   = 8.70504900000000e+00_rt
    ebind_per_nucleon(jge74)   = 8.72520000000000e+00_rt
    ebind_per_nucleon(jge75)   = 8.69560900000000e+00_rt
    ebind_per_nucleon(jge76)   = 8.70523600000000e+00_rt
    ebind_per_nucleon(jge77)   = 8.67102800000000e+00_rt
    ebind_per_nucleon(jge78)   = 8.67166300000000e+00_rt
    ebind_per_nucleon(jas65)   = 8.39623400000000e+00_rt
    ebind_per_nucleon(jas66)   = 8.46840300000000e+00_rt
    ebind_per_nucleon(jas67)   = 8.53056800000000e+00_rt
    ebind_per_nucleon(jas68)   = 8.55774500000000e+00_rt
    ebind_per_nucleon(jas69)   = 8.61182100000000e+00_rt
    ebind_per_nucleon(jas70)   = 8.62166600000000e+00_rt
    ebind_per_nucleon(jas71)   = 8.66393200000000e+00_rt
    ebind_per_nucleon(jas72)   = 8.66037800000000e+00_rt
    ebind_per_nucleon(jas73)   = 8.68960900000000e+00_rt
    ebind_per_nucleon(jas74)   = 8.68000100000000e+00_rt
    ebind_per_nucleon(jas75)   = 8.70087400000000e+00_rt
    ebind_per_nucleon(jas76)   = 8.68281600000000e+00_rt
    ebind_per_nucleon(jas77)   = 8.69597800000000e+00_rt
    ebind_per_nucleon(jas78)   = 8.67387500000000e+00_rt
    ebind_per_nucleon(jas79)   = 8.67661600000000e+00_rt
    ebind_per_nucleon(jse67)   = 8.36953400000000e+00_rt
    ebind_per_nucleon(jse68)   = 8.47704700000000e+00_rt
    ebind_per_nucleon(jse69)   = 8.50370700000000e+00_rt
    ebind_per_nucleon(jse70)   = 8.57603300000000e+00_rt
    ebind_per_nucleon(jse71)   = 8.58606000000000e+00_rt
    ebind_per_nucleon(jse72)   = 8.64448900000000e+00_rt
    ebind_per_nucleon(jse73)   = 8.64155800000000e+00_rt
    ebind_per_nucleon(jse74)   = 8.68771500000000e+00_rt
    ebind_per_nucleon(jse75)   = 8.67891300000000e+00_rt
    ebind_per_nucleon(jse76)   = 8.71147700000000e+00_rt
    ebind_per_nucleon(jse77)   = 8.69469000000000e+00_rt
    ebind_per_nucleon(jse78)   = 8.71780600000000e+00_rt
    ebind_per_nucleon(jse79)   = 8.69559200000000e+00_rt
    ebind_per_nucleon(jse80)   = 8.71081300000000e+00_rt
    ebind_per_nucleon(jse81)   = 8.68599900000000e+00_rt
    ebind_per_nucleon(jse82)   = 8.69319600000000e+00_rt
    ebind_per_nucleon(jse83)   = 8.65855500000000e+00_rt
    ebind_per_nucleon(jbr68)   = 8.23900000000000e+00_rt
    ebind_per_nucleon(jbr69)   = 8.34490200000000e+00_rt
    ebind_per_nucleon(jbr70)   = 8.41479600000000e+00_rt
    ebind_per_nucleon(jbr71)   = 8.48146200000000e+00_rt
    ebind_per_nucleon(jbr72)   = 8.51131200000000e+00_rt
    ebind_per_nucleon(jbr73)   = 8.56810300000000e+00_rt
    ebind_per_nucleon(jbr74)   = 8.58356100000000e+00_rt
    ebind_per_nucleon(jbr75)   = 8.62764900000000e+00_rt
    ebind_per_nucleon(jbr76)   = 8.63588200000000e+00_rt
    ebind_per_nucleon(jbr77)   = 8.66680600000000e+00_rt
    ebind_per_nucleon(jbr78)   = 8.66195900000000e+00_rt
    ebind_per_nucleon(jbr79)   = 8.68759400000000e+00_rt
    ebind_per_nucleon(jbr80)   = 8.67765300000000e+00_rt
    ebind_per_nucleon(jbr81)   = 8.69594600000000e+00_rt
    ebind_per_nucleon(jbr82)   = 8.68249400000000e+00_rt
    ebind_per_nucleon(jbr83)   = 8.69338400000000e+00_rt
    ebind_per_nucleon(jkr69)   = 8.13300000000000e+00_rt
    ebind_per_nucleon(jkr70)   = 8.25600000000000e+00_rt
    ebind_per_nucleon(jkr71)   = 8.32713000000000e+00_rt
    ebind_per_nucleon(jkr72)   = 8.42931900000000e+00_rt
    ebind_per_nucleon(jkr73)   = 8.46018400000000e+00_rt
    ebind_per_nucleon(jkr74)   = 8.53303800000000e+00_rt
    ebind_per_nucleon(jkr75)   = 8.55343900000000e+00_rt
    ebind_per_nucleon(jkr76)   = 8.60880700000000e+00_rt
    ebind_per_nucleon(jkr77)   = 8.61683600000000e+00_rt
    ebind_per_nucleon(jkr78)   = 8.66123800000000e+00_rt
    ebind_per_nucleon(jkr79)   = 8.65711200000000e+00_rt
    ebind_per_nucleon(jkr80)   = 8.69292800000000e+00_rt
    ebind_per_nucleon(jkr81)   = 8.68282000000000e+00_rt
    ebind_per_nucleon(jkr82)   = 8.71067500000000e+00_rt
    ebind_per_nucleon(jkr83)   = 8.69572900000000e+00_rt
    ebind_per_nucleon(jkr84)   = 8.71744600000000e+00_rt
    ebind_per_nucleon(jkr85)   = 8.69856200000000e+00_rt
    ebind_per_nucleon(jkr86)   = 8.71202900000000e+00_rt
    ebind_per_nucleon(jkr87)   = 8.67528300000000e+00_rt
    ebind_per_nucleon(jrb73)   = 8.30600000000000e+00_rt
    ebind_per_nucleon(jrb74)   = 8.38171200000000e+00_rt
    ebind_per_nucleon(jrb75)   = 8.44827500000000e+00_rt
    ebind_per_nucleon(jrb76)   = 8.48621500000000e+00_rt
    ebind_per_nucleon(jrb77)   = 8.53733900000000e+00_rt
    ebind_per_nucleon(jrb78)   = 8.55835000000000e+00_rt
    ebind_per_nucleon(jrb79)   = 8.60114200000000e+00_rt
    ebind_per_nucleon(jrb80)   = 8.61167500000000e+00_rt
    ebind_per_nucleon(jrb81)   = 8.64551300000000e+00_rt
    ebind_per_nucleon(jrb82)   = 8.64742700000000e+00_rt
    ebind_per_nucleon(jrb83)   = 8.67521800000000e+00_rt
    ebind_per_nucleon(jrb84)   = 8.67622400000000e+00_rt
    ebind_per_nucleon(jrb85)   = 8.69744100000000e+00_rt
    ebind_per_nucleon(jsr74)   = 8.22100000000000e+00_rt
    ebind_per_nucleon(jsr75)   = 8.29651100000000e+00_rt
    ebind_per_nucleon(jsr76)   = 8.39392900000000e+00_rt
    ebind_per_nucleon(jsr77)   = 8.43591800000000e+00_rt
    ebind_per_nucleon(jsr78)   = 8.50009600000000e+00_rt
    ebind_per_nucleon(jsr79)   = 8.52382000000000e+00_rt
    ebind_per_nucleon(jsr80)   = 8.57859600000000e+00_rt
    ebind_per_nucleon(jsr81)   = 8.58735400000000e+00_rt
    ebind_per_nucleon(jsr82)   = 8.63571800000000e+00_rt
    ebind_per_nucleon(jsr83)   = 8.63840700000000e+00_rt
    ebind_per_nucleon(jsr84)   = 8.67751200000000e+00_rt
    ebind_per_nucleon(jy75)   = 8.08900000000000e+00_rt
    ebind_per_nucleon(jy76)   = 8.17600000000000e+00_rt
    ebind_per_nucleon(jy77)   = 8.27800000000000e+00_rt
    ebind_per_nucleon(jy78)   = 8.34900000000000e+00_rt
    ebind_per_nucleon(jy79)   = 8.41696700000000e+00_rt
    ebind_per_nucleon(jy80)   = 8.45427500000000e+00_rt
    ebind_per_nucleon(jy81)   = 8.50590200000000e+00_rt
    ebind_per_nucleon(jy82)   = 8.52927500000000e+00_rt
    ebind_per_nucleon(jy83)   = 8.57365600000000e+00_rt
    ebind_per_nucleon(jy84)   = 8.58778000000000e+00_rt
    ebind_per_nucleon(jy85)   = 8.62814800000000e+00_rt
    ebind_per_nucleon(jy86)   = 8.63842800000000e+00_rt
    ebind_per_nucleon(jy87)   = 8.67484400000000e+00_rt
    ebind_per_nucleon(jzr78)   = 8.19400000000000e+00_rt
    ebind_per_nucleon(jzr79)   = 8.26700000000000e+00_rt
    ebind_per_nucleon(jzr80)   = 8.36000000000000e+00_rt
    ebind_per_nucleon(jzr81)   = 8.39435800000000e+00_rt
    ebind_per_nucleon(jzr82)   = 8.46567600000000e+00_rt
    ebind_per_nucleon(jzr83)   = 8.48839900000000e+00_rt
    ebind_per_nucleon(jzr84)   = 8.54902900000000e+00_rt
    ebind_per_nucleon(jzr85)   = 8.56403900000000e+00_rt
    ebind_per_nucleon(jzr86)   = 8.61405100000000e+00_rt
    ebind_per_nucleon(jzr87)   = 8.62365400000000e+00_rt
    ebind_per_nucleon(jzr88)   = 8.66603300000000e+00_rt
    ebind_per_nucleon(jzr89)   = 8.67335900000000e+00_rt
    ebind_per_nucleon(jzr90)   = 8.70996900000000e+00_rt
    ebind_per_nucleon(jnb82)   = 8.31500000000000e+00_rt
    ebind_per_nucleon(jnb83)   = 8.37830400000000e+00_rt
    ebind_per_nucleon(jnb84)   = 8.41825200000000e+00_rt
    ebind_per_nucleon(jnb85)   = 8.47371100000000e+00_rt
    ebind_per_nucleon(jnb86)   = 8.50222200000000e+00_rt
    ebind_per_nucleon(jnb87)   = 8.55175700000000e+00_rt
    ebind_per_nucleon(jnb88)   = 8.57242400000000e+00_rt
    ebind_per_nucleon(jnb89)   = 8.61681200000000e+00_rt
    ebind_per_nucleon(jnb90)   = 8.63337600000000e+00_rt
    ebind_per_nucleon(jmo83)   = 8.23400000000000e+00_rt
    ebind_per_nucleon(jmo84)   = 8.32500000000000e+00_rt
    ebind_per_nucleon(jmo85)   = 8.36133100000000e+00_rt
    ebind_per_nucleon(jmo86)   = 8.43470900000000e+00_rt
    ebind_per_nucleon(jmo87)   = 8.46242400000000e+00_rt
    ebind_per_nucleon(jmo88)   = 8.52390800000000e+00_rt
    ebind_per_nucleon(jmo89)   = 8.54498400000000e+00_rt
    ebind_per_nucleon(jmo90)   = 8.59702800000000e+00_rt
    ebind_per_nucleon(jtc89)   = 8.45057500000000e+00_rt
    ebind_per_nucleon(jtc90)   = 8.48335900000000e+00_rt
    ebind_per_nucleon(jtc91)   = 8.53665500000000e+00_rt

    do i = 1, nspec
       bion(i) = ebind_per_nucleon(i) * aion(i) * ERG_PER_MeV
    end do

    ! Set the mass
    mion(:) = nion(:) * mass_neutron + zion(:) * (mass_proton + mass_electron) &
         - bion(:)/(c_light**2)

    call read_partition_file()

    !$acc update device(bion, mion)

#ifdef REACT_SPARSE_JACOBIAN
    ! Set CSR format metadata for Jacobian
    allocate(csr_jac_col_index(NETWORK_SPARSE_JAC_NNZ))
    allocate(csr_jac_row_count(nspec_evolve + 3)) ! neq + 1

    csr_jac_col_index = [ &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      10, &
      11, &
      12, &
      13, &
      14, &
      15, &
      16, &
      17, &
      19, &
      20, &
      21, &
      22, &
      23, &
      24, &
      25, &
      26, &
      27, &
      29, &
      30, &
      31, &
      32, &
      33, &
      34, &
      35, &
      36, &
      37, &
      38, &
      39, &
      40, &
      41, &
      42, &
      43, &
      44, &
      45, &
      46, &
      47, &
      48, &
      49, &
      50, &
      51, &
      52, &
      53, &
      54, &
      55, &
      56, &
      57, &
      58, &
      59, &
      60, &
      61, &
      62, &
      63, &
      64, &
      65, &
      66, &
      67, &
      68, &
      69, &
      70, &
      71, &
      72, &
      73, &
      74, &
      75, &
      76, &
      77, &
      78, &
      79, &
      80, &
      81, &
      82, &
      83, &
      84, &
      85, &
      86, &
      87, &
      88, &
      89, &
      90, &
      91, &
      92, &
      93, &
      94, &
      95, &
      96, &
      97, &
      98, &
      99, &
      100, &
      101, &
      102, &
      103, &
      104, &
      105, &
      106, &
      107, &
      108, &
      109, &
      110, &
      111, &
      112, &
      113, &
      114, &
      115, &
      116, &
      117, &
      118, &
      119, &
      120, &
      121, &
      122, &
      123, &
      124, &
      125, &
      126, &
      127, &
      128, &
      129, &
      130, &
      131, &
      132, &
      133, &
      134, &
      135, &
      136, &
      137, &
      138, &
      139, &
      140, &
      141, &
      142, &
      143, &
      144, &
      145, &
      146, &
      147, &
      148, &
      149, &
      150, &
      151, &
      152, &
      153, &
      154, &
      155, &
      156, &
      157, &
      158, &
      159, &
      160, &
      161, &
      162, &
      163, &
      164, &
      165, &
      166, &
      167, &
      168, &
      169, &
      170, &
      171, &
      172, &
      173, &
      174, &
      175, &
      176, &
      177, &
      178, &
      179, &
      180, &
      181, &
      182, &
      183, &
      184, &
      185, &
      186, &
      187, &
      188, &
      189, &
      190, &
      191, &
      192, &
      193, &
      194, &
      195, &
      196, &
      197, &
      198, &
      199, &
      200, &
      201, &
      202, &
      203, &
      204, &
      205, &
      206, &
      207, &
      208, &
      209, &
      210, &
      211, &
      212, &
      213, &
      214, &
      215, &
      216, &
      217, &
      218, &
      219, &
      220, &
      221, &
      222, &
      223, &
      224, &
      225, &
      226, &
      227, &
      228, &
      229, &
      230, &
      231, &
      232, &
      233, &
      234, &
      235, &
      236, &
      237, &
      238, &
      239, &
      240, &
      241, &
      242, &
      243, &
      244, &
      245, &
      246, &
      247, &
      248, &
      249, &
      250, &
      251, &
      252, &
      253, &
      254, &
      255, &
      256, &
      257, &
      258, &
      259, &
      260, &
      261, &
      262, &
      263, &
      264, &
      265, &
      266, &
      267, &
      268, &
      269, &
      270, &
      271, &
      272, &
      273, &
      274, &
      275, &
      276, &
      277, &
      278, &
      279, &
      280, &
      281, &
      282, &
      283, &
      284, &
      285, &
      286, &
      287, &
      288, &
      289, &
      290, &
      291, &
      292, &
      293, &
      294, &
      295, &
      296, &
      297, &
      298, &
      299, &
      300, &
      301, &
      302, &
      303, &
      304, &
      305, &
      306, &
      307, &
      308, &
      309, &
      310, &
      311, &
      312, &
      313, &
      314, &
      315, &
      316, &
      317, &
      318, &
      319, &
      320, &
      321, &
      322, &
      323, &
      324, &
      325, &
      326, &
      327, &
      328, &
      329, &
      330, &
      331, &
      332, &
      333, &
      334, &
      335, &
      336, &
      337, &
      338, &
      339, &
      340, &
      341, &
      342, &
      343, &
      344, &
      345, &
      346, &
      347, &
      348, &
      349, &
      350, &
      351, &
      352, &
      353, &
      354, &
      355, &
      356, &
      357, &
      358, &
      359, &
      360, &
      361, &
      362, &
      363, &
      364, &
      365, &
      366, &
      367, &
      368, &
      369, &
      370, &
      371, &
      372, &
      373, &
      374, &
      375, &
      376, &
      377, &
      378, &
      379, &
      380, &
      381, &
      382, &
      383, &
      384, &
      385, &
      386, &
      387, &
      388, &
      389, &
      390, &
      391, &
      392, &
      393, &
      394, &
      395, &
      396, &
      397, &
      398, &
      399, &
      400, &
      401, &
      402, &
      403, &
      404, &
      405, &
      406, &
      407, &
      408, &
      409, &
      410, &
      411, &
      412, &
      413, &
      414, &
      415, &
      416, &
      417, &
      418, &
      419, &
      420, &
      421, &
      422, &
      423, &
      424, &
      425, &
      426, &
      427, &
      428, &
      429, &
      430, &
      431, &
      432, &
      433, &
      434, &
      435, &
      436, &
      437, &
      438, &
      439, &
      440, &
      441, &
      442, &
      443, &
      444, &
      445, &
      446, &
      447, &
      448, &
      449, &
      450, &
      451, &
      452, &
      453, &
      454, &
      455, &
      456, &
      457, &
      458, &
      459, &
      460, &
      461, &
      462, &
      463, &
      464, &
      465, &
      466, &
      467, &
      468, &
      469, &
      470, &
      471, &
      472, &
      473, &
      474, &
      475, &
      476, &
      477, &
      478, &
      479, &
      480, &
      481, &
      482, &
      483, &
      484, &
      485, &
      486, &
      487, &
      488, &
      489, &
      490, &
      491, &
      492, &
      493, &
      494, &
      495, &
      496, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      10, &
      11, &
      12, &
      13, &
      14, &
      15, &
      16, &
      17, &
      18, &
      19, &
      20, &
      21, &
      22, &
      23, &
      24, &
      25, &
      26, &
      27, &
      29, &
      30, &
      31, &
      32, &
      33, &
      34, &
      35, &
      36, &
      37, &
      38, &
      39, &
      40, &
      41, &
      42, &
      43, &
      44, &
      45, &
      46, &
      47, &
      48, &
      49, &
      50, &
      51, &
      52, &
      53, &
      54, &
      55, &
      56, &
      57, &
      58, &
      59, &
      60, &
      61, &
      62, &
      63, &
      64, &
      65, &
      66, &
      67, &
      68, &
      69, &
      70, &
      71, &
      72, &
      73, &
      74, &
      75, &
      76, &
      77, &
      78, &
      79, &
      80, &
      81, &
      82, &
      83, &
      84, &
      85, &
      86, &
      87, &
      88, &
      89, &
      90, &
      91, &
      92, &
      93, &
      94, &
      95, &
      96, &
      97, &
      98, &
      99, &
      100, &
      101, &
      102, &
      103, &
      104, &
      105, &
      106, &
      107, &
      108, &
      109, &
      110, &
      111, &
      112, &
      113, &
      114, &
      115, &
      116, &
      117, &
      118, &
      119, &
      120, &
      121, &
      122, &
      123, &
      124, &
      125, &
      126, &
      127, &
      128, &
      129, &
      130, &
      131, &
      132, &
      133, &
      134, &
      135, &
      136, &
      137, &
      138, &
      139, &
      140, &
      141, &
      142, &
      143, &
      144, &
      145, &
      146, &
      147, &
      148, &
      149, &
      150, &
      151, &
      152, &
      153, &
      154, &
      155, &
      156, &
      157, &
      158, &
      159, &
      160, &
      161, &
      162, &
      163, &
      164, &
      165, &
      166, &
      167, &
      168, &
      169, &
      170, &
      171, &
      172, &
      173, &
      174, &
      175, &
      176, &
      177, &
      178, &
      179, &
      180, &
      181, &
      182, &
      183, &
      184, &
      185, &
      186, &
      187, &
      188, &
      189, &
      190, &
      191, &
      192, &
      193, &
      194, &
      195, &
      196, &
      197, &
      198, &
      199, &
      200, &
      201, &
      202, &
      203, &
      204, &
      205, &
      206, &
      207, &
      208, &
      209, &
      210, &
      211, &
      212, &
      213, &
      214, &
      215, &
      216, &
      217, &
      218, &
      219, &
      220, &
      221, &
      222, &
      223, &
      224, &
      225, &
      226, &
      227, &
      228, &
      229, &
      230, &
      231, &
      232, &
      233, &
      234, &
      235, &
      236, &
      237, &
      238, &
      239, &
      240, &
      241, &
      242, &
      243, &
      244, &
      245, &
      246, &
      247, &
      248, &
      249, &
      250, &
      251, &
      252, &
      253, &
      254, &
      255, &
      256, &
      257, &
      258, &
      259, &
      260, &
      261, &
      262, &
      263, &
      264, &
      265, &
      266, &
      267, &
      268, &
      269, &
      270, &
      271, &
      272, &
      273, &
      274, &
      275, &
      276, &
      277, &
      278, &
      279, &
      280, &
      281, &
      282, &
      283, &
      284, &
      285, &
      286, &
      287, &
      288, &
      289, &
      290, &
      291, &
      292, &
      293, &
      294, &
      295, &
      296, &
      297, &
      298, &
      299, &
      300, &
      301, &
      302, &
      303, &
      304, &
      305, &
      306, &
      307, &
      308, &
      309, &
      310, &
      311, &
      312, &
      313, &
      314, &
      315, &
      316, &
      317, &
      318, &
      319, &
      320, &
      321, &
      322, &
      323, &
      324, &
      325, &
      326, &
      327, &
      328, &
      329, &
      330, &
      331, &
      332, &
      333, &
      334, &
      335, &
      336, &
      337, &
      338, &
      339, &
      340, &
      341, &
      342, &
      343, &
      344, &
      345, &
      346, &
      347, &
      348, &
      349, &
      350, &
      351, &
      352, &
      353, &
      354, &
      355, &
      356, &
      357, &
      358, &
      359, &
      360, &
      361, &
      362, &
      363, &
      364, &
      365, &
      366, &
      367, &
      368, &
      369, &
      370, &
      371, &
      372, &
      373, &
      374, &
      375, &
      376, &
      377, &
      378, &
      379, &
      380, &
      381, &
      382, &
      383, &
      384, &
      385, &
      386, &
      387, &
      388, &
      389, &
      390, &
      391, &
      392, &
      393, &
      394, &
      395, &
      396, &
      397, &
      398, &
      399, &
      400, &
      401, &
      402, &
      403, &
      404, &
      405, &
      406, &
      407, &
      408, &
      409, &
      410, &
      411, &
      412, &
      413, &
      414, &
      415, &
      416, &
      417, &
      418, &
      419, &
      420, &
      421, &
      422, &
      423, &
      424, &
      426, &
      427, &
      428, &
      429, &
      430, &
      431, &
      432, &
      433, &
      434, &
      435, &
      436, &
      437, &
      438, &
      439, &
      440, &
      441, &
      442, &
      443, &
      444, &
      445, &
      446, &
      447, &
      448, &
      449, &
      450, &
      451, &
      452, &
      453, &
      454, &
      455, &
      456, &
      457, &
      458, &
      459, &
      460, &
      461, &
      462, &
      463, &
      464, &
      465, &
      466, &
      467, &
      468, &
      469, &
      470, &
      471, &
      472, &
      473, &
      474, &
      475, &
      476, &
      477, &
      478, &
      479, &
      480, &
      481, &
      482, &
      483, &
      484, &
      485, &
      486, &
      487, &
      488, &
      489, &
      490, &
      491, &
      492, &
      493, &
      494, &
      495, &
      496, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      10, &
      16, &
      17, &
      20, &
      21, &
      496, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      10, &
      13, &
      496, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      496, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      10, &
      11, &
      12, &
      13, &
      14, &
      15, &
      16, &
      17, &
      18, &
      19, &
      20, &
      21, &
      22, &
      23, &
      24, &
      25, &
      26, &
      27, &
      29, &
      30, &
      31, &
      32, &
      33, &
      34, &
      35, &
      36, &
      37, &
      38, &
      39, &
      40, &
      41, &
      42, &
      43, &
      44, &
      45, &
      46, &
      47, &
      48, &
      49, &
      50, &
      51, &
      52, &
      53, &
      54, &
      55, &
      56, &
      57, &
      58, &
      59, &
      60, &
      61, &
      62, &
      63, &
      64, &
      65, &
      66, &
      67, &
      68, &
      69, &
      70, &
      71, &
      72, &
      73, &
      74, &
      75, &
      76, &
      77, &
      78, &
      79, &
      80, &
      81, &
      82, &
      83, &
      84, &
      85, &
      86, &
      87, &
      88, &
      89, &
      90, &
      91, &
      92, &
      93, &
      94, &
      95, &
      96, &
      97, &
      98, &
      99, &
      100, &
      101, &
      102, &
      103, &
      104, &
      105, &
      106, &
      107, &
      108, &
      109, &
      110, &
      111, &
      112, &
      113, &
      114, &
      115, &
      116, &
      117, &
      118, &
      119, &
      120, &
      121, &
      122, &
      123, &
      124, &
      125, &
      126, &
      127, &
      128, &
      129, &
      130, &
      131, &
      132, &
      133, &
      134, &
      135, &
      136, &
      137, &
      138, &
      139, &
      140, &
      141, &
      142, &
      143, &
      144, &
      145, &
      146, &
      147, &
      148, &
      149, &
      150, &
      151, &
      152, &
      153, &
      154, &
      155, &
      156, &
      157, &
      158, &
      159, &
      160, &
      161, &
      162, &
      163, &
      164, &
      165, &
      166, &
      167, &
      168, &
      169, &
      170, &
      171, &
      172, &
      173, &
      174, &
      175, &
      176, &
      177, &
      178, &
      179, &
      180, &
      181, &
      182, &
      183, &
      184, &
      185, &
      186, &
      187, &
      188, &
      189, &
      190, &
      191, &
      192, &
      193, &
      194, &
      195, &
      196, &
      197, &
      198, &
      199, &
      200, &
      201, &
      202, &
      203, &
      204, &
      205, &
      206, &
      207, &
      208, &
      209, &
      210, &
      211, &
      212, &
      213, &
      214, &
      215, &
      216, &
      217, &
      218, &
      219, &
      220, &
      221, &
      222, &
      223, &
      224, &
      225, &
      226, &
      227, &
      228, &
      229, &
      230, &
      231, &
      232, &
      233, &
      234, &
      235, &
      236, &
      237, &
      238, &
      239, &
      240, &
      241, &
      242, &
      243, &
      244, &
      245, &
      246, &
      247, &
      248, &
      249, &
      250, &
      251, &
      252, &
      253, &
      254, &
      255, &
      256, &
      258, &
      259, &
      260, &
      261, &
      262, &
      263, &
      264, &
      265, &
      266, &
      267, &
      268, &
      269, &
      270, &
      271, &
      272, &
      273, &
      274, &
      275, &
      276, &
      277, &
      278, &
      279, &
      280, &
      281, &
      282, &
      283, &
      284, &
      285, &
      286, &
      287, &
      288, &
      289, &
      290, &
      291, &
      292, &
      293, &
      294, &
      295, &
      296, &
      297, &
      298, &
      299, &
      300, &
      301, &
      302, &
      303, &
      304, &
      305, &
      306, &
      307, &
      308, &
      309, &
      310, &
      311, &
      312, &
      313, &
      314, &
      315, &
      316, &
      317, &
      318, &
      319, &
      320, &
      321, &
      322, &
      323, &
      324, &
      325, &
      326, &
      327, &
      328, &
      329, &
      330, &
      331, &
      332, &
      333, &
      334, &
      335, &
      336, &
      337, &
      338, &
      339, &
      340, &
      341, &
      342, &
      343, &
      344, &
      345, &
      346, &
      347, &
      348, &
      349, &
      350, &
      351, &
      352, &
      353, &
      354, &
      355, &
      356, &
      357, &
      358, &
      359, &
      360, &
      361, &
      362, &
      363, &
      364, &
      365, &
      366, &
      367, &
      368, &
      369, &
      370, &
      371, &
      372, &
      373, &
      374, &
      375, &
      376, &
      377, &
      378, &
      379, &
      380, &
      381, &
      382, &
      383, &
      384, &
      385, &
      386, &
      387, &
      388, &
      389, &
      390, &
      391, &
      392, &
      393, &
      394, &
      395, &
      396, &
      397, &
      398, &
      399, &
      400, &
      401, &
      402, &
      403, &
      404, &
      405, &
      406, &
      408, &
      409, &
      410, &
      411, &
      412, &
      413, &
      414, &
      415, &
      416, &
      417, &
      418, &
      419, &
      420, &
      421, &
      422, &
      423, &
      424, &
      425, &
      426, &
      427, &
      428, &
      429, &
      430, &
      431, &
      432, &
      433, &
      434, &
      435, &
      436, &
      437, &
      438, &
      439, &
      440, &
      441, &
      442, &
      443, &
      444, &
      445, &
      446, &
      447, &
      448, &
      449, &
      450, &
      451, &
      452, &
      453, &
      454, &
      455, &
      456, &
      457, &
      458, &
      459, &
      460, &
      461, &
      462, &
      463, &
      464, &
      465, &
      466, &
      467, &
      468, &
      469, &
      470, &
      471, &
      472, &
      473, &
      474, &
      475, &
      476, &
      477, &
      478, &
      479, &
      480, &
      481, &
      482, &
      483, &
      484, &
      485, &
      486, &
      487, &
      488, &
      489, &
      490, &
      491, &
      492, &
      493, &
      494, &
      495, &
      496, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      10, &
      12, &
      496, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      10, &
      12, &
      13, &
      496, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      11, &
      12, &
      14, &
      496, &
      1, &
      2, &
      4, &
      6, &
      7, &
      8, &
      10, &
      12, &
      13, &
      15, &
      496, &
      1, &
      2, &
      6, &
      9, &
      11, &
      14, &
      496, &
      1, &
      2, &
      6, &
      7, &
      8, &
      9, &
      10, &
      12, &
      13, &
      14, &
      16, &
      19, &
      496, &
      1, &
      2, &
      4, &
      6, &
      8, &
      10, &
      12, &
      13, &
      14, &
      15, &
      17, &
      20, &
      496, &
      1, &
      2, &
      6, &
      9, &
      11, &
      12, &
      13, &
      14, &
      15, &
      18, &
      20, &
      22, &
      496, &
      1, &
      2, &
      6, &
      10, &
      13, &
      14, &
      15, &
      16, &
      18, &
      19, &
      21, &
      23, &
      24, &
      37, &
      46, &
      54, &
      55, &
      66, &
      75, &
      76, &
      87, &
      98, &
      496, &
      1, &
      2, &
      3, &
      6, &
      12, &
      15, &
      16, &
      17, &
      19, &
      20, &
      24, &
      496, &
      1, &
      2, &
      3, &
      6, &
      13, &
      16, &
      17, &
      20, &
      21, &
      25, &
      26, &
      496, &
      2, &
      6, &
      14, &
      18, &
      23, &
      496, &
      1, &
      2, &
      6, &
      12, &
      15, &
      16, &
      19, &
      20, &
      22, &
      24, &
      496, &
      1, &
      2, &
      3, &
      6, &
      13, &
      14, &
      16, &
      17, &
      19, &
      20, &
      21, &
      22, &
      23, &
      25, &
      29, &
      30, &
      496, &
      1, &
      2, &
      3, &
      6, &
      15, &
      17, &
      20, &
      21, &
      23, &
      24, &
      26, &
      30, &
      31, &
      496, &
      1, &
      2, &
      6, &
      14, &
      19, &
      20, &
      22, &
      23, &
      29, &
      34, &
      35, &
      496, &
      1, &
      2, &
      6, &
      15, &
      18, &
      20, &
      21, &
      22, &
      23, &
      24, &
      30, &
      35, &
      36, &
      496, &
      1, &
      2, &
      6, &
      15, &
      16, &
      19, &
      21, &
      23, &
      24, &
      25, &
      29, &
      31, &
      34, &
      36, &
      37, &
      43, &
      55, &
      66, &
      75, &
      76, &
      87, &
      98, &
      496, &
      1, &
      2, &
      6, &
      17, &
      20, &
      24, &
      25, &
      26, &
      29, &
      30, &
      32, &
      37, &
      38, &
      496, &
      1, &
      2, &
      6, &
      17, &
      21, &
      25, &
      26, &
      27, &
      30, &
      31, &
      33, &
      38, &
      39, &
      496, &
      1, &
      2, &
      6, &
      26, &
      27, &
      31, &
      32, &
      39, &
      40, &
      496, &
      28, &
      32, &
      496, &
      1, &
      2, &
      6, &
      20, &
      22, &
      24, &
      25, &
      29, &
      30, &
      34, &
      35, &
      37, &
      43, &
      44, &
      52, &
      496, &
      1, &
      2, &
      6, &
      20, &
      21, &
      23, &
      25, &
      26, &
      29, &
      30, &
      31, &
      35, &
      36, &
      38, &
      44, &
      45, &
      496, &
      1, &
      2, &
      6, &
      21, &
      24, &
      26, &
      27, &
      30, &
      31, &
      32, &
      36, &
      37, &
      39, &
      45, &
      46, &
      496, &
      1, &
      2, &
      6, &
      25, &
      27, &
      28, &
      31, &
      32, &
      33, &
      37, &
      38, &
      40, &
      46, &
      47, &
      496, &
      1, &
      2, &
      6, &
      26, &
      32, &
      33, &
      38, &
      39, &
      41, &
      47, &
      48, &
      496, &
      1, &
      2, &
      6, &
      22, &
      29, &
      34, &
      35, &
      43, &
      51, &
      52, &
      496, &
      1, &
      2, &
      6, &
      22, &
      23, &
      29, &
      30, &
      34, &
      35, &
      36, &
      42, &
      44, &
      52, &
      53, &
      61, &
      496, &
      1, &
      2, &
      6, &
      23, &
      24, &
      30, &
      31, &
      35, &
      36, &
      37, &
      42, &
      43, &
      45, &
      51, &
      53, &
      54, &
      496, &
      1, &
      2, &
      6, &
      15, &
      24, &
      25, &
      29, &
      31, &
      32, &
      36, &
      37, &
      38, &
      43, &
      44, &
      46, &
      52, &
      54, &
      55, &
      61, &
      63, &
      76, &
      87, &
      98, &
      496, &
      1, &
      2, &
      6, &
      25, &
      26, &
      30, &
      32, &
      33, &
      37, &
      38, &
      39, &
      44, &
      45, &
      47, &
      55, &
      56, &
      496, &
      1, &
      2, &
      6, &
      26, &
      27, &
      31, &
      33, &
      38, &
      39, &
      40, &
      45, &
      46, &
      48, &
      56, &
      57, &
      496, &
      1, &
      2, &
      6, &
      27, &
      32, &
      39, &
      40, &
      41, &
      46, &
      47, &
      49, &
      57, &
      58, &
      496, &
      1, &
      2, &
      6, &
      33, &
      40, &
      41, &
      47, &
      48, &
      50, &
      58, &
      59, &
      496, &
      1, &
      2, &
      6, &
      35, &
      36, &
      42, &
      43, &
      51, &
      53, &
      61, &
      62, &
      496, &
      1, &
      2, &
      6, &
      29, &
      34, &
      36, &
      37, &
      42, &
      43, &
      44, &
      51, &
      52, &
      54, &
      62, &
      63, &
      496, &
      1, &
      2, &
      6, &
      29, &
      30, &
      35, &
      37, &
      38, &
      43, &
      44, &
      45, &
      52, &
      53, &
      55, &
      61, &
      63, &
      64, &
      71, &
      496, &
      1, &
      2, &
      6, &
      30, &
      31, &
      36, &
      38, &
      39, &
      44, &
      45, &
      46, &
      53, &
      54, &
      56, &
      62, &
      64, &
      65, &
      496, &
      1, &
      2, &
      6, &
      15, &
      31, &
      32, &
      37, &
      39, &
      40, &
      45, &
      46, &
      47, &
      54, &
      55, &
      57, &
      63, &
      65, &
      66, &
      496, &
      1, &
      2, &
      6, &
      32, &
      33, &
      38, &
      40, &
      41, &
      46, &
      47, &
      48, &
      55, &
      56, &
      58, &
      66, &
      67, &
      496, &
      1, &
      2, &
      6, &
      33, &
      39, &
      41, &
      47, &
      48, &
      49, &
      56, &
      57, &
      59, &
      67, &
      68, &
      496, &
      1, &
      2, &
      6, &
      40, &
      48, &
      49, &
      50, &
      57, &
      58, &
      60, &
      68, &
      69, &
      496, &
      1, &
      2, &
      6, &
      41, &
      49, &
      50, &
      58, &
      59, &
      69, &
      70, &
      496, &
      1, &
      2, &
      6, &
      34, &
      42, &
      43, &
      51, &
      52, &
      62, &
      71, &
      72, &
      496, &
      1, &
      2, &
      6, &
      34, &
      35, &
      43, &
      44, &
      51, &
      52, &
      53, &
      61, &
      63, &
      72, &
      73, &
      496, &
      1, &
      2, &
      6, &
      35, &
      36, &
      42, &
      44, &
      45, &
      52, &
      53, &
      54, &
      61, &
      62, &
      64, &
      71, &
      73, &
      74, &
      496, &
      1, &
      2, &
      6, &
      15, &
      36, &
      37, &
      43, &
      45, &
      46, &
      53, &
      54, &
      55, &
      62, &
      63, &
      65, &
      72, &
      74, &
      75, &
      496, &
      1, &
      2, &
      6, &
      15, &
      24, &
      37, &
      38, &
      44, &
      46, &
      47, &
      54, &
      55, &
      56, &
      63, &
      64, &
      66, &
      73, &
      75, &
      76, &
      84, &
      496, &
      1, &
      2, &
      6, &
      38, &
      39, &
      45, &
      47, &
      48, &
      55, &
      56, &
      57, &
      64, &
      65, &
      67, &
      76, &
      77, &
      496, &
      1, &
      2, &
      6, &
      39, &
      40, &
      46, &
      48, &
      49, &
      50, &
      56, &
      57, &
      58, &
      65, &
      66, &
      68, &
      77, &
      78, &
      496, &
      1, &
      2, &
      6, &
      40, &
      41, &
      47, &
      49, &
      50, &
      57, &
      58, &
      59, &
      66, &
      67, &
      69, &
      78, &
      79, &
      496, &
      1, &
      2, &
      6, &
      41, &
      48, &
      50, &
      58, &
      59, &
      60, &
      67, &
      68, &
      70, &
      79, &
      80, &
      496, &
      1, &
      2, &
      6, &
      49, &
      59, &
      60, &
      68, &
      69, &
      80, &
      81, &
      496, &
      1, &
      2, &
      6, &
      42, &
      52, &
      53, &
      61, &
      62, &
      71, &
      73, &
      496, &
      1, &
      2, &
      6, &
      42, &
      43, &
      51, &
      53, &
      54, &
      61, &
      62, &
      63, &
      71, &
      72, &
      74, &
      83, &
      496, &
      1, &
      2, &
      6, &
      43, &
      44, &
      52, &
      54, &
      55, &
      62, &
      63, &
      64, &
      72, &
      73, &
      75, &
      83, &
      84, &
      496, &
      1, &
      2, &
      6, &
      44, &
      45, &
      53, &
      55, &
      56, &
      63, &
      64, &
      65, &
      73, &
      74, &
      76, &
      84, &
      85, &
      496, &
      1, &
      2, &
      6, &
      45, &
      46, &
      54, &
      56, &
      57, &
      64, &
      65, &
      66, &
      74, &
      75, &
      77, &
      83, &
      85, &
      86, &
      496, &
      1, &
      2, &
      6, &
      15, &
      24, &
      46, &
      47, &
      55, &
      57, &
      58, &
      65, &
      66, &
      67, &
      75, &
      76, &
      78, &
      84, &
      86, &
      87, &
      496, &
      1, &
      2, &
      6, &
      47, &
      48, &
      56, &
      58, &
      59, &
      66, &
      67, &
      68, &
      76, &
      77, &
      79, &
      87, &
      88, &
      496, &
      1, &
      2, &
      6, &
      48, &
      49, &
      57, &
      59, &
      60, &
      67, &
      68, &
      69, &
      77, &
      78, &
      80, &
      88, &
      89, &
      496, &
      1, &
      2, &
      6, &
      49, &
      50, &
      58, &
      60, &
      68, &
      69, &
      70, &
      78, &
      79, &
      81, &
      89, &
      90, &
      496, &
      1, &
      2, &
      6, &
      50, &
      59, &
      69, &
      70, &
      79, &
      80, &
      82, &
      90, &
      91, &
      496, &
      1, &
      2, &
      6, &
      51, &
      61, &
      62, &
      71, &
      72, &
      496, &
      1, &
      2, &
      6, &
      51, &
      52, &
      62, &
      63, &
      71, &
      72, &
      73, &
      83, &
      95, &
      496, &
      1, &
      2, &
      6, &
      52, &
      53, &
      61, &
      63, &
      64, &
      72, &
      73, &
      74, &
      84, &
      95, &
      96, &
      496, &
      1, &
      2, &
      6, &
      53, &
      54, &
      62, &
      64, &
      65, &
      73, &
      74, &
      75, &
      83, &
      85, &
      96, &
      97, &
      496, &
      1, &
      2, &
      6, &
      15, &
      24, &
      54, &
      55, &
      63, &
      65, &
      66, &
      74, &
      75, &
      76, &
      83, &
      84, &
      86, &
      95, &
      97, &
      98, &
      496, &
      1, &
      2, &
      6, &
      15, &
      24, &
      37, &
      55, &
      56, &
      64, &
      66, &
      67, &
      75, &
      76, &
      77, &
      84, &
      85, &
      87, &
      96, &
      98, &
      99, &
      111, &
      496, &
      1, &
      2, &
      6, &
      56, &
      57, &
      65, &
      67, &
      68, &
      76, &
      77, &
      78, &
      85, &
      86, &
      88, &
      99, &
      100, &
      496, &
      1, &
      2, &
      6, &
      57, &
      58, &
      66, &
      68, &
      69, &
      77, &
      78, &
      79, &
      86, &
      87, &
      89, &
      100, &
      101, &
      496, &
      1, &
      2, &
      6, &
      58, &
      59, &
      67, &
      69, &
      70, &
      78, &
      79, &
      80, &
      87, &
      88, &
      90, &
      101, &
      102, &
      496, &
      1, &
      2, &
      6, &
      59, &
      60, &
      68, &
      70, &
      79, &
      80, &
      81, &
      88, &
      89, &
      91, &
      102, &
      103, &
      496, &
      1, &
      2, &
      6, &
      60, &
      69, &
      80, &
      81, &
      82, &
      89, &
      90, &
      92, &
      103, &
      104, &
      496, &
      1, &
      2, &
      6, &
      70, &
      81, &
      82, &
      90, &
      91, &
      93, &
      104, &
      105, &
      496, &
      1, &
      2, &
      6, &
      62, &
      63, &
      72, &
      74, &
      75, &
      83, &
      84, &
      95, &
      97, &
      110, &
      496, &
      1, &
      2, &
      6, &
      63, &
      64, &
      73, &
      75, &
      76, &
      83, &
      84, &
      85, &
      95, &
      96, &
      98, &
      110, &
      111, &
      496, &
      1, &
      2, &
      6, &
      64, &
      65, &
      74, &
      76, &
      77, &
      84, &
      85, &
      86, &
      96, &
      97, &
      99, &
      111, &
      112, &
      496, &
      1, &
      2, &
      6, &
      65, &
      66, &
      75, &
      77, &
      78, &
      85, &
      86, &
      87, &
      97, &
      98, &
      100, &
      110, &
      112, &
      113, &
      496, &
      1, &
      2, &
      6, &
      15, &
      24, &
      37, &
      66, &
      67, &
      76, &
      78, &
      79, &
      86, &
      87, &
      88, &
      98, &
      99, &
      101, &
      111, &
      113, &
      114, &
      496, &
      1, &
      2, &
      6, &
      67, &
      68, &
      77, &
      79, &
      80, &
      87, &
      88, &
      89, &
      99, &
      100, &
      102, &
      114, &
      115, &
      496, &
      1, &
      2, &
      6, &
      68, &
      69, &
      78, &
      80, &
      81, &
      88, &
      89, &
      90, &
      100, &
      101, &
      103, &
      115, &
      116, &
      496, &
      1, &
      2, &
      6, &
      69, &
      70, &
      79, &
      81, &
      82, &
      89, &
      90, &
      91, &
      101, &
      102, &
      104, &
      116, &
      117, &
      496, &
      1, &
      2, &
      6, &
      70, &
      80, &
      82, &
      90, &
      91, &
      92, &
      102, &
      103, &
      105, &
      117, &
      118, &
      496, &
      1, &
      2, &
      6, &
      81, &
      91, &
      92, &
      93, &
      103, &
      104, &
      106, &
      118, &
      119, &
      496, &
      1, &
      2, &
      6, &
      82, &
      92, &
      93, &
      94, &
      104, &
      105, &
      107, &
      119, &
      120, &
      496, &
      1, &
      2, &
      6, &
      93, &
      94, &
      105, &
      106, &
      108, &
      120, &
      121, &
      496, &
      1, &
      2, &
      6, &
      72, &
      73, &
      83, &
      84, &
      95, &
      96, &
      110, &
      125, &
      496, &
      1, &
      2, &
      6, &
      73, &
      74, &
      84, &
      85, &
      95, &
      96, &
      97, &
      111, &
      125, &
      126, &
      496, &
      1, &
      2, &
      6, &
      74, &
      75, &
      83, &
      85, &
      86, &
      96, &
      97, &
      98, &
      110, &
      112, &
      126, &
      127, &
      496, &
      1, &
      2, &
      6, &
      15, &
      24, &
      37, &
      75, &
      76, &
      84, &
      86, &
      87, &
      97, &
      98, &
      99, &
      110, &
      111, &
      113, &
      125, &
      127, &
      128, &
      496, &
      1, &
      2, &
      6, &
      76, &
      77, &
      85, &
      87, &
      88, &
      98, &
      99, &
      100, &
      111, &
      112, &
      114, &
      126, &
      128, &
      129, &
      141, &
      496, &
      1, &
      2, &
      6, &
      77, &
      78, &
      86, &
      88, &
      89, &
      99, &
      100, &
      101, &
      112, &
      113, &
      115, &
      129, &
      130, &
      496, &
      1, &
      2, &
      6, &
      78, &
      79, &
      87, &
      89, &
      90, &
      100, &
      101, &
      102, &
      113, &
      114, &
      116, &
      130, &
      131, &
      496, &
      1, &
      2, &
      6, &
      79, &
      80, &
      88, &
      90, &
      91, &
      101, &
      102, &
      103, &
      114, &
      115, &
      117, &
      131, &
      132, &
      496, &
      1, &
      2, &
      6, &
      80, &
      81, &
      89, &
      91, &
      92, &
      93, &
      102, &
      103, &
      104, &
      115, &
      116, &
      118, &
      132, &
      133, &
      496, &
      1, &
      2, &
      6, &
      81, &
      82, &
      90, &
      92, &
      93, &
      94, &
      103, &
      104, &
      105, &
      116, &
      117, &
      119, &
      133, &
      134, &
      496, &
      1, &
      2, &
      6, &
      82, &
      91, &
      93, &
      94, &
      104, &
      105, &
      106, &
      117, &
      118, &
      120, &
      134, &
      135, &
      496, &
      1, &
      2, &
      6, &
      92, &
      94, &
      105, &
      106, &
      107, &
      118, &
      119, &
      121, &
      135, &
      136, &
      496, &
      1, &
      2, &
      6, &
      93, &
      106, &
      107, &
      108, &
      119, &
      120, &
      122, &
      136, &
      137, &
      496, &
      1, &
      2, &
      6, &
      94, &
      107, &
      108, &
      109, &
      120, &
      121, &
      123, &
      137, &
      138, &
      496, &
      1, &
      2, &
      6, &
      108, &
      109, &
      121, &
      122, &
      124, &
      138, &
      139, &
      496, &
      1, &
      2, &
      6, &
      83, &
      84, &
      95, &
      97, &
      98, &
      110, &
      111, &
      125, &
      127, &
      140, &
      496, &
      1, &
      2, &
      6, &
      84, &
      85, &
      96, &
      98, &
      99, &
      110, &
      111, &
      112, &
      125, &
      126, &
      128, &
      140, &
      141, &
      496, &
      1, &
      2, &
      6, &
      85, &
      86, &
      97, &
      99, &
      100, &
      111, &
      112, &
      113, &
      126, &
      127, &
      129, &
      141, &
      142, &
      496, &
      1, &
      2, &
      6, &
      86, &
      87, &
      98, &
      100, &
      101, &
      112, &
      113, &
      114, &
      127, &
      128, &
      130, &
      140, &
      142, &
      143, &
      496, &
      1, &
      2, &
      6, &
      87, &
      88, &
      99, &
      101, &
      102, &
      113, &
      114, &
      115, &
      128, &
      129, &
      131, &
      141, &
      143, &
      144, &
      496, &
      1, &
      2, &
      6, &
      88, &
      89, &
      100, &
      102, &
      103, &
      114, &
      115, &
      116, &
      129, &
      130, &
      132, &
      144, &
      145, &
      496, &
      1, &
      2, &
      6, &
      89, &
      90, &
      101, &
      103, &
      104, &
      115, &
      116, &
      117, &
      130, &
      131, &
      133, &
      145, &
      146, &
      496, &
      1, &
      2, &
      6, &
      90, &
      91, &
      102, &
      104, &
      105, &
      116, &
      117, &
      118, &
      131, &
      132, &
      134, &
      146, &
      147, &
      496, &
      1, &
      2, &
      6, &
      91, &
      92, &
      103, &
      105, &
      106, &
      117, &
      118, &
      119, &
      132, &
      133, &
      135, &
      147, &
      148, &
      496, &
      1, &
      2, &
      6, &
      92, &
      93, &
      104, &
      106, &
      107, &
      108, &
      118, &
      119, &
      120, &
      133, &
      134, &
      136, &
      148, &
      149, &
      496, &
      1, &
      2, &
      6, &
      93, &
      94, &
      105, &
      107, &
      108, &
      109, &
      119, &
      120, &
      121, &
      134, &
      135, &
      137, &
      149, &
      150, &
      496, &
      1, &
      2, &
      6, &
      94, &
      106, &
      108, &
      109, &
      120, &
      121, &
      122, &
      135, &
      136, &
      138, &
      150, &
      151, &
      496, &
      1, &
      2, &
      6, &
      107, &
      109, &
      121, &
      122, &
      123, &
      136, &
      137, &
      139, &
      151, &
      152, &
      496, &
      1, &
      2, &
      6, &
      108, &
      122, &
      123, &
      124, &
      137, &
      138, &
      152, &
      153, &
      496, &
      1, &
      2, &
      6, &
      109, &
      123, &
      124, &
      138, &
      139, &
      153, &
      154, &
      496, &
      1, &
      2, &
      6, &
      95, &
      96, &
      110, &
      111, &
      125, &
      126, &
      140, &
      155, &
      496, &
      1, &
      2, &
      6, &
      96, &
      97, &
      111, &
      112, &
      125, &
      126, &
      127, &
      141, &
      155, &
      156, &
      496, &
      1, &
      2, &
      6, &
      97, &
      98, &
      110, &
      112, &
      113, &
      126, &
      127, &
      128, &
      140, &
      142, &
      156, &
      157, &
      496, &
      1, &
      2, &
      6, &
      98, &
      99, &
      111, &
      113, &
      114, &
      127, &
      128, &
      129, &
      140, &
      141, &
      143, &
      155, &
      157, &
      158, &
      496, &
      1, &
      2, &
      6, &
      99, &
      100, &
      112, &
      114, &
      115, &
      128, &
      129, &
      130, &
      141, &
      142, &
      144, &
      156, &
      158, &
      159, &
      169, &
      496, &
      1, &
      2, &
      6, &
      100, &
      101, &
      113, &
      115, &
      116, &
      129, &
      130, &
      131, &
      142, &
      143, &
      145, &
      159, &
      160, &
      496, &
      1, &
      2, &
      6, &
      101, &
      102, &
      114, &
      116, &
      117, &
      130, &
      131, &
      132, &
      143, &
      144, &
      146, &
      160, &
      161, &
      496, &
      1, &
      2, &
      6, &
      102, &
      103, &
      115, &
      117, &
      118, &
      131, &
      132, &
      133, &
      144, &
      145, &
      147, &
      161, &
      162, &
      496, &
      1, &
      2, &
      6, &
      103, &
      104, &
      116, &
      118, &
      119, &
      132, &
      133, &
      134, &
      145, &
      146, &
      148, &
      162, &
      163, &
      496, &
      1, &
      2, &
      6, &
      104, &
      105, &
      117, &
      119, &
      120, &
      133, &
      134, &
      135, &
      146, &
      147, &
      149, &
      163, &
      164, &
      496, &
      1, &
      2, &
      6, &
      105, &
      106, &
      118, &
      120, &
      121, &
      122, &
      134, &
      135, &
      136, &
      147, &
      148, &
      150, &
      164, &
      165, &
      496, &
      1, &
      2, &
      6, &
      106, &
      107, &
      119, &
      121, &
      122, &
      123, &
      135, &
      136, &
      137, &
      148, &
      149, &
      151, &
      165, &
      166, &
      496, &
      1, &
      2, &
      6, &
      107, &
      108, &
      120, &
      122, &
      123, &
      124, &
      136, &
      137, &
      138, &
      149, &
      150, &
      152, &
      166, &
      167, &
      496, &
      1, &
      2, &
      6, &
      108, &
      109, &
      121, &
      123, &
      124, &
      137, &
      138, &
      139, &
      150, &
      151, &
      153, &
      167, &
      168, &
      496, &
      1, &
      2, &
      6, &
      109, &
      122, &
      124, &
      138, &
      139, &
      151, &
      152, &
      154, &
      168, &
      496, &
      1, &
      2, &
      6, &
      110, &
      111, &
      125, &
      127, &
      128, &
      140, &
      141, &
      155, &
      157, &
      496, &
      1, &
      2, &
      6, &
      111, &
      112, &
      126, &
      128, &
      129, &
      140, &
      141, &
      142, &
      155, &
      156, &
      158, &
      169, &
      496, &
      1, &
      2, &
      6, &
      112, &
      113, &
      127, &
      129, &
      130, &
      141, &
      142, &
      143, &
      156, &
      157, &
      159, &
      169, &
      170, &
      496, &
      1, &
      2, &
      6, &
      113, &
      114, &
      128, &
      130, &
      131, &
      142, &
      143, &
      144, &
      157, &
      158, &
      160, &
      170, &
      171, &
      496, &
      1, &
      2, &
      6, &
      114, &
      115, &
      129, &
      131, &
      132, &
      143, &
      144, &
      145, &
      158, &
      159, &
      161, &
      169, &
      171, &
      172, &
      496, &
      1, &
      2, &
      6, &
      115, &
      116, &
      130, &
      132, &
      133, &
      144, &
      145, &
      146, &
      159, &
      160, &
      162, &
      172, &
      173, &
      496, &
      1, &
      2, &
      6, &
      116, &
      117, &
      131, &
      133, &
      134, &
      145, &
      146, &
      147, &
      160, &
      161, &
      163, &
      173, &
      174, &
      496, &
      1, &
      2, &
      6, &
      117, &
      118, &
      132, &
      134, &
      135, &
      146, &
      147, &
      148, &
      161, &
      162, &
      164, &
      174, &
      175, &
      496, &
      1, &
      2, &
      6, &
      118, &
      119, &
      133, &
      135, &
      136, &
      147, &
      148, &
      149, &
      162, &
      163, &
      165, &
      175, &
      176, &
      496, &
      1, &
      2, &
      6, &
      119, &
      120, &
      134, &
      136, &
      137, &
      148, &
      149, &
      150, &
      163, &
      164, &
      166, &
      176, &
      177, &
      496, &
      1, &
      2, &
      6, &
      120, &
      121, &
      135, &
      137, &
      138, &
      149, &
      150, &
      151, &
      164, &
      165, &
      167, &
      177, &
      178, &
      496, &
      1, &
      2, &
      6, &
      121, &
      122, &
      136, &
      138, &
      139, &
      150, &
      151, &
      152, &
      165, &
      166, &
      168, &
      178, &
      179, &
      496, &
      1, &
      2, &
      6, &
      122, &
      123, &
      137, &
      139, &
      151, &
      152, &
      153, &
      166, &
      167, &
      179, &
      180, &
      496, &
      1, &
      2, &
      6, &
      123, &
      124, &
      138, &
      152, &
      153, &
      154, &
      167, &
      168, &
      180, &
      496, &
      1, &
      2, &
      6, &
      124, &
      139, &
      153, &
      154, &
      168, &
      496, &
      1, &
      2, &
      6, &
      125, &
      126, &
      140, &
      141, &
      155, &
      156, &
      496, &
      1, &
      2, &
      6, &
      126, &
      127, &
      141, &
      142, &
      155, &
      156, &
      157, &
      169, &
      181, &
      496, &
      1, &
      2, &
      6, &
      127, &
      128, &
      140, &
      142, &
      143, &
      155, &
      156, &
      157, &
      158, &
      170, &
      181, &
      182, &
      496, &
      1, &
      2, &
      6, &
      128, &
      129, &
      141, &
      143, &
      144, &
      157, &
      158, &
      159, &
      169, &
      171, &
      182, &
      183, &
      496, &
      1, &
      2, &
      6, &
      129, &
      130, &
      142, &
      144, &
      145, &
      158, &
      159, &
      160, &
      169, &
      170, &
      172, &
      181, &
      183, &
      184, &
      496, &
      1, &
      2, &
      6, &
      130, &
      131, &
      143, &
      145, &
      146, &
      159, &
      160, &
      161, &
      170, &
      171, &
      173, &
      184, &
      185, &
      496, &
      1, &
      2, &
      6, &
      131, &
      132, &
      144, &
      146, &
      147, &
      160, &
      161, &
      162, &
      171, &
      172, &
      174, &
      185, &
      186, &
      496, &
      1, &
      2, &
      6, &
      132, &
      133, &
      145, &
      147, &
      148, &
      161, &
      162, &
      163, &
      172, &
      173, &
      175, &
      186, &
      187, &
      496, &
      1, &
      2, &
      6, &
      133, &
      134, &
      146, &
      148, &
      149, &
      162, &
      163, &
      164, &
      173, &
      174, &
      176, &
      187, &
      188, &
      496, &
      1, &
      2, &
      6, &
      134, &
      135, &
      147, &
      149, &
      150, &
      163, &
      164, &
      165, &
      174, &
      175, &
      177, &
      188, &
      189, &
      496, &
      1, &
      2, &
      6, &
      135, &
      136, &
      148, &
      150, &
      151, &
      164, &
      165, &
      166, &
      175, &
      176, &
      178, &
      189, &
      190, &
      496, &
      1, &
      2, &
      6, &
      136, &
      137, &
      149, &
      151, &
      152, &
      153, &
      165, &
      166, &
      167, &
      176, &
      177, &
      179, &
      190, &
      191, &
      496, &
      1, &
      2, &
      6, &
      137, &
      138, &
      150, &
      152, &
      153, &
      154, &
      166, &
      167, &
      168, &
      177, &
      178, &
      180, &
      191, &
      192, &
      496, &
      1, &
      2, &
      6, &
      138, &
      139, &
      151, &
      153, &
      154, &
      167, &
      168, &
      178, &
      179, &
      192, &
      193, &
      496, &
      1, &
      2, &
      6, &
      141, &
      142, &
      156, &
      158, &
      159, &
      169, &
      170, &
      181, &
      183, &
      194, &
      195, &
      496, &
      1, &
      2, &
      6, &
      142, &
      143, &
      157, &
      159, &
      160, &
      169, &
      170, &
      171, &
      181, &
      182, &
      184, &
      195, &
      196, &
      496, &
      1, &
      2, &
      6, &
      143, &
      144, &
      158, &
      160, &
      161, &
      170, &
      171, &
      172, &
      182, &
      183, &
      185, &
      196, &
      197, &
      496, &
      1, &
      2, &
      6, &
      144, &
      145, &
      159, &
      161, &
      162, &
      171, &
      172, &
      173, &
      183, &
      184, &
      186, &
      197, &
      198, &
      496, &
      1, &
      2, &
      6, &
      145, &
      146, &
      160, &
      162, &
      163, &
      172, &
      173, &
      174, &
      184, &
      185, &
      187, &
      198, &
      199, &
      496, &
      1, &
      2, &
      6, &
      146, &
      147, &
      161, &
      163, &
      164, &
      173, &
      174, &
      175, &
      185, &
      186, &
      188, &
      199, &
      200, &
      496, &
      1, &
      2, &
      6, &
      147, &
      148, &
      162, &
      164, &
      165, &
      174, &
      175, &
      176, &
      186, &
      187, &
      189, &
      200, &
      201, &
      496, &
      1, &
      2, &
      6, &
      148, &
      149, &
      163, &
      165, &
      166, &
      175, &
      176, &
      177, &
      187, &
      188, &
      190, &
      201, &
      202, &
      496, &
      1, &
      2, &
      6, &
      149, &
      150, &
      164, &
      166, &
      167, &
      176, &
      177, &
      178, &
      188, &
      189, &
      191, &
      202, &
      203, &
      496, &
      1, &
      2, &
      6, &
      150, &
      151, &
      165, &
      167, &
      168, &
      177, &
      178, &
      179, &
      189, &
      190, &
      192, &
      203, &
      204, &
      496, &
      1, &
      2, &
      6, &
      151, &
      152, &
      166, &
      168, &
      178, &
      179, &
      180, &
      190, &
      191, &
      193, &
      204, &
      205, &
      496, &
      1, &
      2, &
      6, &
      152, &
      153, &
      167, &
      179, &
      180, &
      191, &
      192, &
      205, &
      206, &
      496, &
      1, &
      2, &
      6, &
      156, &
      157, &
      169, &
      170, &
      181, &
      182, &
      195, &
      207, &
      208, &
      496, &
      1, &
      2, &
      6, &
      157, &
      158, &
      170, &
      171, &
      181, &
      182, &
      183, &
      194, &
      196, &
      208, &
      209, &
      496, &
      1, &
      2, &
      6, &
      158, &
      159, &
      169, &
      171, &
      172, &
      182, &
      183, &
      184, &
      194, &
      195, &
      197, &
      207, &
      209, &
      210, &
      496, &
      1, &
      2, &
      6, &
      159, &
      160, &
      170, &
      172, &
      173, &
      183, &
      184, &
      185, &
      195, &
      196, &
      198, &
      208, &
      210, &
      211, &
      224, &
      496, &
      1, &
      2, &
      6, &
      160, &
      161, &
      171, &
      173, &
      174, &
      184, &
      185, &
      186, &
      196, &
      197, &
      199, &
      211, &
      212, &
      496, &
      1, &
      2, &
      6, &
      161, &
      162, &
      172, &
      174, &
      175, &
      185, &
      186, &
      187, &
      197, &
      198, &
      200, &
      212, &
      213, &
      496, &
      1, &
      2, &
      6, &
      162, &
      163, &
      173, &
      175, &
      176, &
      186, &
      187, &
      188, &
      198, &
      199, &
      201, &
      213, &
      214, &
      496, &
      1, &
      2, &
      6, &
      163, &
      164, &
      174, &
      176, &
      177, &
      187, &
      188, &
      189, &
      199, &
      200, &
      202, &
      214, &
      215, &
      496, &
      1, &
      2, &
      6, &
      164, &
      165, &
      175, &
      177, &
      178, &
      188, &
      189, &
      190, &
      200, &
      201, &
      203, &
      215, &
      216, &
      496, &
      1, &
      2, &
      6, &
      165, &
      166, &
      176, &
      178, &
      179, &
      189, &
      190, &
      191, &
      201, &
      202, &
      204, &
      216, &
      217, &
      496, &
      1, &
      2, &
      6, &
      166, &
      167, &
      177, &
      179, &
      180, &
      190, &
      191, &
      192, &
      202, &
      203, &
      205, &
      217, &
      218, &
      496, &
      1, &
      2, &
      6, &
      167, &
      168, &
      178, &
      180, &
      191, &
      192, &
      193, &
      203, &
      204, &
      206, &
      218, &
      219, &
      496, &
      1, &
      2, &
      6, &
      168, &
      179, &
      192, &
      193, &
      204, &
      205, &
      219, &
      220, &
      496, &
      1, &
      2, &
      6, &
      169, &
      182, &
      183, &
      194, &
      195, &
      207, &
      209, &
      222, &
      223, &
      496, &
      1, &
      2, &
      6, &
      169, &
      170, &
      181, &
      183, &
      184, &
      194, &
      195, &
      196, &
      207, &
      208, &
      210, &
      223, &
      224, &
      496, &
      1, &
      2, &
      6, &
      170, &
      171, &
      182, &
      184, &
      185, &
      195, &
      196, &
      197, &
      208, &
      209, &
      211, &
      222, &
      224, &
      225, &
      496, &
      1, &
      2, &
      6, &
      171, &
      172, &
      183, &
      185, &
      186, &
      196, &
      197, &
      198, &
      209, &
      210, &
      212, &
      223, &
      225, &
      226, &
      496, &
      1, &
      2, &
      6, &
      172, &
      173, &
      184, &
      186, &
      187, &
      197, &
      198, &
      199, &
      210, &
      211, &
      213, &
      224, &
      226, &
      227, &
      496, &
      1, &
      2, &
      6, &
      173, &
      174, &
      185, &
      187, &
      188, &
      198, &
      199, &
      200, &
      211, &
      212, &
      214, &
      227, &
      228, &
      496, &
      1, &
      2, &
      6, &
      174, &
      175, &
      186, &
      188, &
      189, &
      199, &
      200, &
      201, &
      212, &
      213, &
      215, &
      228, &
      229, &
      496, &
      1, &
      2, &
      6, &
      175, &
      176, &
      187, &
      189, &
      190, &
      200, &
      201, &
      202, &
      213, &
      214, &
      216, &
      229, &
      230, &
      496, &
      1, &
      2, &
      6, &
      176, &
      177, &
      188, &
      190, &
      191, &
      201, &
      202, &
      203, &
      214, &
      215, &
      217, &
      230, &
      231, &
      496, &
      1, &
      2, &
      6, &
      177, &
      178, &
      189, &
      191, &
      192, &
      202, &
      203, &
      204, &
      215, &
      216, &
      218, &
      231, &
      232, &
      496, &
      1, &
      2, &
      6, &
      178, &
      179, &
      190, &
      192, &
      193, &
      203, &
      204, &
      205, &
      216, &
      217, &
      219, &
      232, &
      233, &
      496, &
      1, &
      2, &
      6, &
      179, &
      180, &
      191, &
      193, &
      204, &
      205, &
      206, &
      217, &
      218, &
      220, &
      233, &
      234, &
      496, &
      1, &
      2, &
      6, &
      180, &
      192, &
      205, &
      206, &
      218, &
      219, &
      221, &
      234, &
      235, &
      496, &
      1, &
      2, &
      6, &
      181, &
      194, &
      195, &
      207, &
      208, &
      223, &
      238, &
      239, &
      496, &
      1, &
      2, &
      6, &
      181, &
      182, &
      195, &
      196, &
      207, &
      208, &
      209, &
      222, &
      224, &
      239, &
      240, &
      496, &
      1, &
      2, &
      6, &
      182, &
      183, &
      194, &
      196, &
      197, &
      208, &
      209, &
      210, &
      222, &
      223, &
      225, &
      238, &
      240, &
      241, &
      496, &
      1, &
      2, &
      6, &
      183, &
      184, &
      195, &
      197, &
      198, &
      209, &
      210, &
      211, &
      223, &
      224, &
      226, &
      239, &
      241, &
      242, &
      496, &
      1, &
      2, &
      6, &
      184, &
      185, &
      196, &
      198, &
      199, &
      210, &
      211, &
      212, &
      224, &
      225, &
      227, &
      240, &
      242, &
      243, &
      496, &
      1, &
      2, &
      6, &
      185, &
      186, &
      197, &
      199, &
      200, &
      211, &
      212, &
      213, &
      225, &
      226, &
      228, &
      243, &
      244, &
      496, &
      1, &
      2, &
      6, &
      186, &
      187, &
      198, &
      200, &
      201, &
      212, &
      213, &
      214, &
      226, &
      227, &
      229, &
      244, &
      245, &
      496, &
      1, &
      2, &
      6, &
      187, &
      188, &
      199, &
      201, &
      202, &
      213, &
      214, &
      215, &
      227, &
      228, &
      230, &
      245, &
      246, &
      496, &
      1, &
      2, &
      6, &
      188, &
      189, &
      200, &
      202, &
      203, &
      214, &
      215, &
      216, &
      228, &
      229, &
      231, &
      246, &
      247, &
      496, &
      1, &
      2, &
      6, &
      189, &
      190, &
      201, &
      203, &
      204, &
      215, &
      216, &
      217, &
      229, &
      230, &
      232, &
      247, &
      248, &
      496, &
      1, &
      2, &
      6, &
      190, &
      191, &
      202, &
      204, &
      205, &
      216, &
      217, &
      218, &
      230, &
      231, &
      233, &
      248, &
      249, &
      496, &
      1, &
      2, &
      6, &
      191, &
      192, &
      203, &
      205, &
      206, &
      217, &
      218, &
      219, &
      231, &
      232, &
      234, &
      249, &
      250, &
      496, &
      1, &
      2, &
      6, &
      192, &
      193, &
      204, &
      206, &
      218, &
      219, &
      220, &
      232, &
      233, &
      235, &
      250, &
      251, &
      496, &
      1, &
      2, &
      6, &
      193, &
      205, &
      219, &
      220, &
      221, &
      233, &
      234, &
      236, &
      251, &
      252, &
      496, &
      1, &
      2, &
      6, &
      206, &
      220, &
      221, &
      234, &
      235, &
      237, &
      252, &
      253, &
      496, &
      1, &
      2, &
      6, &
      194, &
      208, &
      209, &
      222, &
      223, &
      238, &
      240, &
      258, &
      496, &
      1, &
      2, &
      6, &
      194, &
      195, &
      207, &
      209, &
      210, &
      222, &
      223, &
      224, &
      238, &
      239, &
      241, &
      258, &
      259, &
      496, &
      1, &
      2, &
      6, &
      195, &
      196, &
      208, &
      210, &
      211, &
      223, &
      224, &
      225, &
      239, &
      240, &
      242, &
      259, &
      260, &
      496, &
      1, &
      2, &
      6, &
      196, &
      197, &
      209, &
      211, &
      212, &
      224, &
      225, &
      226, &
      240, &
      241, &
      243, &
      258, &
      260, &
      261, &
      496, &
      1, &
      2, &
      6, &
      197, &
      198, &
      210, &
      212, &
      213, &
      225, &
      226, &
      227, &
      241, &
      242, &
      244, &
      261, &
      262, &
      496, &
      1, &
      2, &
      6, &
      198, &
      199, &
      211, &
      213, &
      214, &
      226, &
      227, &
      228, &
      242, &
      243, &
      245, &
      262, &
      263, &
      496, &
      1, &
      2, &
      6, &
      199, &
      200, &
      212, &
      214, &
      215, &
      227, &
      228, &
      229, &
      243, &
      244, &
      246, &
      263, &
      264, &
      496, &
      1, &
      2, &
      6, &
      200, &
      201, &
      213, &
      215, &
      216, &
      228, &
      229, &
      230, &
      244, &
      245, &
      247, &
      264, &
      265, &
      496, &
      1, &
      2, &
      6, &
      201, &
      202, &
      214, &
      216, &
      217, &
      229, &
      230, &
      231, &
      245, &
      246, &
      248, &
      265, &
      266, &
      496, &
      1, &
      2, &
      6, &
      202, &
      203, &
      215, &
      217, &
      218, &
      230, &
      231, &
      232, &
      246, &
      247, &
      249, &
      266, &
      267, &
      496, &
      1, &
      2, &
      6, &
      203, &
      204, &
      216, &
      218, &
      219, &
      231, &
      232, &
      233, &
      247, &
      248, &
      250, &
      267, &
      268, &
      496, &
      1, &
      2, &
      6, &
      204, &
      205, &
      217, &
      219, &
      220, &
      232, &
      233, &
      234, &
      248, &
      249, &
      251, &
      268, &
      269, &
      496, &
      1, &
      2, &
      6, &
      205, &
      206, &
      218, &
      220, &
      221, &
      233, &
      234, &
      235, &
      249, &
      250, &
      252, &
      269, &
      270, &
      496, &
      1, &
      2, &
      6, &
      206, &
      219, &
      221, &
      234, &
      235, &
      236, &
      250, &
      251, &
      253, &
      270, &
      271, &
      496, &
      1, &
      2, &
      6, &
      220, &
      235, &
      236, &
      237, &
      251, &
      252, &
      254, &
      271, &
      272, &
      496, &
      1, &
      2, &
      6, &
      221, &
      236, &
      237, &
      252, &
      253, &
      255, &
      272, &
      273, &
      496, &
      1, &
      2, &
      6, &
      207, &
      222, &
      223, &
      238, &
      239, &
      258, &
      276, &
      496, &
      1, &
      2, &
      6, &
      207, &
      208, &
      223, &
      224, &
      238, &
      239, &
      240, &
      259, &
      276, &
      277, &
      496, &
      1, &
      2, &
      6, &
      208, &
      209, &
      222, &
      224, &
      225, &
      239, &
      240, &
      241, &
      258, &
      260, &
      277, &
      278, &
      496, &
      1, &
      2, &
      6, &
      209, &
      210, &
      223, &
      225, &
      226, &
      240, &
      241, &
      242, &
      258, &
      259, &
      261, &
      276, &
      278, &
      279, &
      496, &
      1, &
      2, &
      6, &
      210, &
      211, &
      224, &
      226, &
      227, &
      241, &
      242, &
      243, &
      259, &
      260, &
      262, &
      277, &
      279, &
      280, &
      496, &
      1, &
      2, &
      6, &
      211, &
      212, &
      225, &
      227, &
      228, &
      242, &
      243, &
      244, &
      260, &
      261, &
      263, &
      278, &
      280, &
      281, &
      496, &
      1, &
      2, &
      6, &
      212, &
      213, &
      226, &
      228, &
      229, &
      243, &
      244, &
      245, &
      261, &
      262, &
      264, &
      281, &
      282, &
      496, &
      1, &
      2, &
      6, &
      213, &
      214, &
      227, &
      229, &
      230, &
      244, &
      245, &
      246, &
      262, &
      263, &
      265, &
      282, &
      283, &
      496, &
      1, &
      2, &
      6, &
      214, &
      215, &
      228, &
      230, &
      231, &
      245, &
      246, &
      247, &
      263, &
      264, &
      266, &
      283, &
      284, &
      496, &
      1, &
      2, &
      6, &
      215, &
      216, &
      229, &
      231, &
      232, &
      246, &
      247, &
      248, &
      264, &
      265, &
      267, &
      284, &
      285, &
      496, &
      1, &
      2, &
      6, &
      216, &
      217, &
      230, &
      232, &
      233, &
      247, &
      248, &
      249, &
      265, &
      266, &
      268, &
      285, &
      286, &
      496, &
      1, &
      2, &
      6, &
      217, &
      218, &
      231, &
      233, &
      234, &
      248, &
      249, &
      250, &
      266, &
      267, &
      269, &
      286, &
      287, &
      496, &
      1, &
      2, &
      6, &
      218, &
      219, &
      232, &
      234, &
      235, &
      249, &
      250, &
      251, &
      267, &
      268, &
      270, &
      287, &
      288, &
      496, &
      1, &
      2, &
      6, &
      219, &
      220, &
      233, &
      235, &
      236, &
      237, &
      250, &
      251, &
      252, &
      268, &
      269, &
      271, &
      288, &
      289, &
      496, &
      1, &
      2, &
      6, &
      220, &
      221, &
      234, &
      236, &
      237, &
      251, &
      252, &
      253, &
      269, &
      270, &
      272, &
      289, &
      290, &
      496, &
      1, &
      2, &
      6, &
      221, &
      235, &
      237, &
      252, &
      253, &
      254, &
      270, &
      271, &
      273, &
      290, &
      291, &
      496, &
      1, &
      2, &
      6, &
      236, &
      253, &
      254, &
      255, &
      271, &
      272, &
      274, &
      291, &
      292, &
      496, &
      1, &
      2, &
      6, &
      237, &
      254, &
      255, &
      256, &
      272, &
      273, &
      275, &
      292, &
      293, &
      496, &
      1, &
      2, &
      6, &
      255, &
      256, &
      257, &
      273, &
      274, &
      293, &
      496, &
      1, &
      2, &
      256, &
      257, &
      274, &
      275, &
      496, &
      1, &
      2, &
      6, &
      222, &
      223, &
      238, &
      240, &
      241, &
      258, &
      259, &
      276, &
      278, &
      496, &
      1, &
      2, &
      6, &
      223, &
      224, &
      239, &
      241, &
      242, &
      258, &
      259, &
      260, &
      276, &
      277, &
      279, &
      294, &
      496, &
      1, &
      2, &
      6, &
      224, &
      225, &
      240, &
      242, &
      243, &
      259, &
      260, &
      261, &
      277, &
      278, &
      280, &
      294, &
      295, &
      496, &
      1, &
      2, &
      6, &
      225, &
      226, &
      241, &
      243, &
      244, &
      260, &
      261, &
      262, &
      278, &
      279, &
      281, &
      295, &
      296, &
      496, &
      1, &
      2, &
      6, &
      226, &
      227, &
      242, &
      244, &
      245, &
      261, &
      262, &
      263, &
      279, &
      280, &
      282, &
      294, &
      296, &
      297, &
      496, &
      1, &
      2, &
      6, &
      227, &
      228, &
      243, &
      245, &
      246, &
      262, &
      263, &
      264, &
      280, &
      281, &
      283, &
      295, &
      297, &
      298, &
      496, &
      1, &
      2, &
      6, &
      228, &
      229, &
      244, &
      246, &
      247, &
      263, &
      264, &
      265, &
      281, &
      282, &
      284, &
      298, &
      299, &
      496, &
      1, &
      2, &
      6, &
      229, &
      230, &
      245, &
      247, &
      248, &
      264, &
      265, &
      266, &
      282, &
      283, &
      285, &
      299, &
      300, &
      496, &
      1, &
      2, &
      6, &
      230, &
      231, &
      246, &
      248, &
      249, &
      265, &
      266, &
      267, &
      283, &
      284, &
      286, &
      300, &
      301, &
      496, &
      1, &
      2, &
      6, &
      231, &
      232, &
      247, &
      249, &
      250, &
      266, &
      267, &
      268, &
      284, &
      285, &
      287, &
      301, &
      302, &
      496, &
      1, &
      2, &
      6, &
      232, &
      233, &
      248, &
      250, &
      251, &
      267, &
      268, &
      269, &
      285, &
      286, &
      288, &
      302, &
      303, &
      496, &
      1, &
      2, &
      6, &
      233, &
      234, &
      249, &
      251, &
      252, &
      268, &
      269, &
      270, &
      286, &
      287, &
      289, &
      303, &
      304, &
      496, &
      1, &
      2, &
      6, &
      234, &
      235, &
      250, &
      252, &
      253, &
      269, &
      270, &
      271, &
      287, &
      288, &
      290, &
      304, &
      305, &
      496, &
      1, &
      2, &
      6, &
      235, &
      236, &
      251, &
      253, &
      254, &
      270, &
      271, &
      272, &
      288, &
      289, &
      291, &
      305, &
      306, &
      496, &
      1, &
      2, &
      6, &
      236, &
      237, &
      252, &
      254, &
      255, &
      271, &
      272, &
      273, &
      289, &
      290, &
      292, &
      306, &
      307, &
      496, &
      1, &
      2, &
      6, &
      237, &
      253, &
      255, &
      256, &
      257, &
      272, &
      273, &
      274, &
      290, &
      291, &
      293, &
      307, &
      308, &
      496, &
      1, &
      2, &
      6, &
      254, &
      256, &
      257, &
      273, &
      274, &
      275, &
      291, &
      292, &
      308, &
      496, &
      1, &
      2, &
      6, &
      255, &
      257, &
      274, &
      275, &
      292, &
      293, &
      496, &
      1, &
      2, &
      6, &
      238, &
      239, &
      258, &
      259, &
      276, &
      277, &
      496, &
      1, &
      2, &
      6, &
      239, &
      240, &
      259, &
      260, &
      276, &
      277, &
      278, &
      294, &
      496, &
      1, &
      2, &
      6, &
      240, &
      241, &
      258, &
      260, &
      261, &
      277, &
      278, &
      279, &
      295, &
      309, &
      496, &
      1, &
      2, &
      6, &
      241, &
      242, &
      259, &
      261, &
      262, &
      278, &
      279, &
      280, &
      294, &
      296, &
      309, &
      310, &
      496, &
      1, &
      2, &
      6, &
      242, &
      243, &
      260, &
      262, &
      263, &
      279, &
      280, &
      281, &
      294, &
      295, &
      297, &
      310, &
      311, &
      496, &
      1, &
      2, &
      6, &
      243, &
      244, &
      261, &
      263, &
      264, &
      280, &
      281, &
      282, &
      295, &
      296, &
      298, &
      309, &
      311, &
      312, &
      326, &
      496, &
      1, &
      2, &
      6, &
      244, &
      245, &
      262, &
      264, &
      265, &
      281, &
      282, &
      283, &
      296, &
      297, &
      299, &
      310, &
      312, &
      313, &
      496, &
      1, &
      2, &
      6, &
      245, &
      246, &
      263, &
      265, &
      266, &
      282, &
      283, &
      284, &
      297, &
      298, &
      300, &
      311, &
      313, &
      314, &
      496, &
      1, &
      2, &
      6, &
      246, &
      247, &
      264, &
      266, &
      267, &
      283, &
      284, &
      285, &
      298, &
      299, &
      301, &
      314, &
      315, &
      496, &
      1, &
      2, &
      6, &
      247, &
      248, &
      265, &
      267, &
      268, &
      284, &
      285, &
      286, &
      299, &
      300, &
      302, &
      315, &
      316, &
      496, &
      1, &
      2, &
      6, &
      248, &
      249, &
      266, &
      268, &
      269, &
      285, &
      286, &
      287, &
      300, &
      301, &
      303, &
      316, &
      317, &
      496, &
      1, &
      2, &
      6, &
      249, &
      250, &
      267, &
      269, &
      270, &
      286, &
      287, &
      288, &
      301, &
      302, &
      304, &
      317, &
      318, &
      496, &
      1, &
      2, &
      6, &
      250, &
      251, &
      268, &
      270, &
      271, &
      287, &
      288, &
      289, &
      302, &
      303, &
      305, &
      318, &
      319, &
      496, &
      1, &
      2, &
      6, &
      251, &
      252, &
      269, &
      271, &
      272, &
      288, &
      289, &
      290, &
      303, &
      304, &
      306, &
      319, &
      320, &
      496, &
      1, &
      2, &
      6, &
      252, &
      253, &
      270, &
      272, &
      273, &
      289, &
      290, &
      291, &
      304, &
      305, &
      307, &
      320, &
      321, &
      496, &
      1, &
      2, &
      6, &
      253, &
      254, &
      271, &
      273, &
      274, &
      275, &
      290, &
      291, &
      292, &
      305, &
      306, &
      308, &
      321, &
      322, &
      496, &
      1, &
      2, &
      6, &
      254, &
      255, &
      272, &
      274, &
      275, &
      291, &
      292, &
      293, &
      306, &
      307, &
      322, &
      323, &
      496, &
      1, &
      2, &
      6, &
      255, &
      256, &
      273, &
      275, &
      292, &
      293, &
      307, &
      308, &
      323, &
      324, &
      496, &
      1, &
      2, &
      6, &
      259, &
      260, &
      277, &
      279, &
      280, &
      294, &
      295, &
      310, &
      325, &
      496, &
      1, &
      2, &
      6, &
      260, &
      261, &
      278, &
      280, &
      281, &
      294, &
      295, &
      296, &
      309, &
      311, &
      325, &
      326, &
      496, &
      1, &
      2, &
      6, &
      261, &
      262, &
      279, &
      281, &
      282, &
      295, &
      296, &
      297, &
      309, &
      310, &
      312, &
      326, &
      327, &
      496, &
      1, &
      2, &
      6, &
      262, &
      263, &
      280, &
      282, &
      283, &
      296, &
      297, &
      298, &
      310, &
      311, &
      313, &
      327, &
      328, &
      496, &
      1, &
      2, &
      6, &
      263, &
      264, &
      281, &
      283, &
      284, &
      297, &
      298, &
      299, &
      311, &
      312, &
      314, &
      326, &
      328, &
      329, &
      496, &
      1, &
      2, &
      6, &
      264, &
      265, &
      282, &
      284, &
      285, &
      298, &
      299, &
      300, &
      312, &
      313, &
      315, &
      327, &
      329, &
      330, &
      496, &
      1, &
      2, &
      6, &
      265, &
      266, &
      283, &
      285, &
      286, &
      299, &
      300, &
      301, &
      313, &
      314, &
      316, &
      330, &
      331, &
      496, &
      1, &
      2, &
      6, &
      266, &
      267, &
      284, &
      286, &
      287, &
      300, &
      301, &
      302, &
      314, &
      315, &
      317, &
      331, &
      332, &
      496, &
      1, &
      2, &
      6, &
      267, &
      268, &
      285, &
      287, &
      288, &
      301, &
      302, &
      303, &
      315, &
      316, &
      318, &
      332, &
      333, &
      496, &
      1, &
      2, &
      6, &
      268, &
      269, &
      286, &
      288, &
      289, &
      302, &
      303, &
      304, &
      316, &
      317, &
      319, &
      333, &
      334, &
      496, &
      1, &
      2, &
      6, &
      269, &
      270, &
      287, &
      289, &
      290, &
      303, &
      304, &
      305, &
      317, &
      318, &
      320, &
      334, &
      335, &
      496, &
      1, &
      2, &
      6, &
      270, &
      271, &
      288, &
      290, &
      291, &
      304, &
      305, &
      306, &
      318, &
      319, &
      321, &
      335, &
      336, &
      496, &
      1, &
      2, &
      6, &
      271, &
      272, &
      289, &
      291, &
      292, &
      305, &
      306, &
      307, &
      319, &
      320, &
      322, &
      336, &
      337, &
      496, &
      1, &
      2, &
      6, &
      272, &
      273, &
      290, &
      292, &
      293, &
      306, &
      307, &
      308, &
      320, &
      321, &
      323, &
      337, &
      338, &
      496, &
      1, &
      2, &
      6, &
      273, &
      274, &
      291, &
      293, &
      307, &
      308, &
      321, &
      322, &
      324, &
      338, &
      339, &
      496, &
      1, &
      2, &
      6, &
      278, &
      279, &
      295, &
      296, &
      309, &
      310, &
      326, &
      496, &
      1, &
      2, &
      6, &
      279, &
      280, &
      294, &
      296, &
      297, &
      309, &
      310, &
      311, &
      325, &
      327, &
      342, &
      496, &
      1, &
      2, &
      6, &
      280, &
      281, &
      295, &
      297, &
      298, &
      310, &
      311, &
      312, &
      325, &
      326, &
      328, &
      342, &
      343, &
      496, &
      1, &
      2, &
      6, &
      281, &
      282, &
      296, &
      298, &
      299, &
      311, &
      312, &
      313, &
      326, &
      327, &
      329, &
      343, &
      344, &
      496, &
      1, &
      2, &
      6, &
      282, &
      283, &
      297, &
      299, &
      300, &
      312, &
      313, &
      314, &
      327, &
      328, &
      330, &
      344, &
      345, &
      496, &
      1, &
      2, &
      6, &
      283, &
      284, &
      298, &
      300, &
      301, &
      313, &
      314, &
      315, &
      328, &
      329, &
      331, &
      345, &
      346, &
      496, &
      1, &
      2, &
      6, &
      284, &
      285, &
      299, &
      301, &
      302, &
      314, &
      315, &
      316, &
      329, &
      330, &
      332, &
      346, &
      347, &
      496, &
      1, &
      2, &
      6, &
      285, &
      286, &
      300, &
      302, &
      303, &
      315, &
      316, &
      317, &
      330, &
      331, &
      333, &
      345, &
      347, &
      348, &
      496, &
      1, &
      2, &
      6, &
      286, &
      287, &
      301, &
      303, &
      304, &
      316, &
      317, &
      318, &
      331, &
      332, &
      334, &
      348, &
      349, &
      496, &
      1, &
      2, &
      6, &
      287, &
      288, &
      302, &
      304, &
      305, &
      317, &
      318, &
      319, &
      332, &
      333, &
      335, &
      349, &
      350, &
      496, &
      1, &
      2, &
      6, &
      288, &
      289, &
      303, &
      305, &
      306, &
      318, &
      319, &
      320, &
      333, &
      334, &
      336, &
      350, &
      351, &
      496, &
      1, &
      2, &
      6, &
      289, &
      290, &
      304, &
      306, &
      307, &
      319, &
      320, &
      321, &
      334, &
      335, &
      337, &
      351, &
      352, &
      496, &
      1, &
      2, &
      6, &
      290, &
      291, &
      305, &
      307, &
      308, &
      320, &
      321, &
      322, &
      335, &
      336, &
      338, &
      352, &
      353, &
      496, &
      1, &
      2, &
      6, &
      291, &
      292, &
      306, &
      308, &
      321, &
      322, &
      323, &
      336, &
      337, &
      339, &
      353, &
      354, &
      496, &
      1, &
      2, &
      6, &
      292, &
      293, &
      307, &
      322, &
      323, &
      324, &
      337, &
      338, &
      340, &
      354, &
      355, &
      496, &
      1, &
      2, &
      6, &
      293, &
      308, &
      323, &
      324, &
      338, &
      339, &
      341, &
      355, &
      356, &
      496, &
      1, &
      2, &
      6, &
      294, &
      295, &
      310, &
      311, &
      325, &
      326, &
      342, &
      496, &
      1, &
      2, &
      6, &
      295, &
      296, &
      309, &
      311, &
      312, &
      325, &
      326, &
      327, &
      343, &
      496, &
      1, &
      2, &
      6, &
      296, &
      297, &
      310, &
      312, &
      313, &
      326, &
      327, &
      328, &
      342, &
      344, &
      359, &
      496, &
      1, &
      2, &
      6, &
      297, &
      298, &
      311, &
      313, &
      314, &
      327, &
      328, &
      329, &
      342, &
      343, &
      345, &
      359, &
      360, &
      496, &
      1, &
      2, &
      6, &
      298, &
      299, &
      312, &
      314, &
      315, &
      328, &
      329, &
      330, &
      343, &
      344, &
      346, &
      360, &
      361, &
      496, &
      1, &
      2, &
      6, &
      299, &
      300, &
      313, &
      315, &
      316, &
      329, &
      330, &
      331, &
      344, &
      345, &
      347, &
      361, &
      362, &
      496, &
      1, &
      2, &
      6, &
      300, &
      301, &
      314, &
      316, &
      317, &
      330, &
      331, &
      332, &
      345, &
      346, &
      348, &
      362, &
      363, &
      496, &
      1, &
      2, &
      6, &
      301, &
      302, &
      315, &
      317, &
      318, &
      331, &
      332, &
      333, &
      346, &
      347, &
      349, &
      363, &
      364, &
      496, &
      1, &
      2, &
      6, &
      302, &
      303, &
      316, &
      318, &
      319, &
      332, &
      333, &
      334, &
      347, &
      348, &
      350, &
      364, &
      365, &
      496, &
      1, &
      2, &
      6, &
      303, &
      304, &
      317, &
      319, &
      320, &
      333, &
      334, &
      335, &
      348, &
      349, &
      351, &
      365, &
      366, &
      496, &
      1, &
      2, &
      6, &
      304, &
      305, &
      318, &
      320, &
      321, &
      334, &
      335, &
      336, &
      349, &
      350, &
      352, &
      366, &
      367, &
      496, &
      1, &
      2, &
      6, &
      305, &
      306, &
      319, &
      321, &
      322, &
      335, &
      336, &
      337, &
      350, &
      351, &
      353, &
      367, &
      368, &
      496, &
      1, &
      2, &
      6, &
      306, &
      307, &
      320, &
      322, &
      323, &
      336, &
      337, &
      338, &
      351, &
      352, &
      354, &
      368, &
      369, &
      496, &
      1, &
      2, &
      6, &
      307, &
      308, &
      321, &
      323, &
      324, &
      337, &
      338, &
      339, &
      352, &
      353, &
      355, &
      369, &
      370, &
      496, &
      1, &
      2, &
      6, &
      308, &
      322, &
      324, &
      338, &
      339, &
      340, &
      353, &
      354, &
      356, &
      370, &
      371, &
      496, &
      1, &
      2, &
      6, &
      323, &
      339, &
      340, &
      341, &
      354, &
      355, &
      357, &
      371, &
      372, &
      496, &
      1, &
      2, &
      6, &
      324, &
      340, &
      341, &
      355, &
      356, &
      358, &
      372, &
      373, &
      496, &
      1, &
      2, &
      6, &
      310, &
      311, &
      325, &
      327, &
      328, &
      342, &
      343, &
      359, &
      496, &
      1, &
      2, &
      6, &
      311, &
      312, &
      326, &
      328, &
      329, &
      342, &
      343, &
      344, &
      360, &
      374, &
      496, &
      1, &
      2, &
      6, &
      312, &
      313, &
      327, &
      329, &
      330, &
      343, &
      344, &
      345, &
      359, &
      361, &
      374, &
      375, &
      496, &
      1, &
      2, &
      6, &
      313, &
      314, &
      328, &
      330, &
      331, &
      344, &
      345, &
      346, &
      359, &
      360, &
      362, &
      375, &
      376, &
      496, &
      1, &
      2, &
      6, &
      314, &
      315, &
      329, &
      331, &
      332, &
      345, &
      346, &
      347, &
      360, &
      361, &
      363, &
      374, &
      376, &
      377, &
      496, &
      1, &
      2, &
      6, &
      315, &
      316, &
      330, &
      332, &
      333, &
      346, &
      347, &
      348, &
      361, &
      362, &
      364, &
      377, &
      378, &
      496, &
      1, &
      2, &
      6, &
      316, &
      317, &
      331, &
      333, &
      334, &
      347, &
      348, &
      349, &
      362, &
      363, &
      365, &
      376, &
      378, &
      379, &
      496, &
      1, &
      2, &
      6, &
      317, &
      318, &
      332, &
      334, &
      335, &
      348, &
      349, &
      350, &
      363, &
      364, &
      366, &
      379, &
      380, &
      496, &
      1, &
      2, &
      6, &
      318, &
      319, &
      333, &
      335, &
      336, &
      349, &
      350, &
      351, &
      364, &
      365, &
      367, &
      380, &
      381, &
      496, &
      1, &
      2, &
      6, &
      319, &
      320, &
      334, &
      336, &
      337, &
      350, &
      351, &
      352, &
      365, &
      366, &
      368, &
      381, &
      382, &
      496, &
      1, &
      2, &
      6, &
      320, &
      321, &
      335, &
      337, &
      338, &
      351, &
      352, &
      353, &
      366, &
      367, &
      369, &
      382, &
      383, &
      496, &
      1, &
      2, &
      6, &
      321, &
      322, &
      336, &
      338, &
      339, &
      352, &
      353, &
      354, &
      367, &
      368, &
      370, &
      383, &
      384, &
      496, &
      1, &
      2, &
      6, &
      322, &
      323, &
      337, &
      339, &
      340, &
      353, &
      354, &
      355, &
      368, &
      369, &
      371, &
      384, &
      385, &
      496, &
      1, &
      2, &
      6, &
      323, &
      324, &
      338, &
      340, &
      341, &
      354, &
      355, &
      356, &
      369, &
      370, &
      372, &
      385, &
      386, &
      496, &
      1, &
      2, &
      6, &
      324, &
      339, &
      341, &
      355, &
      356, &
      357, &
      370, &
      371, &
      373, &
      386, &
      387, &
      496, &
      1, &
      2, &
      6, &
      340, &
      356, &
      357, &
      358, &
      371, &
      372, &
      387, &
      388, &
      496, &
      1, &
      2, &
      6, &
      341, &
      357, &
      358, &
      372, &
      373, &
      388, &
      389, &
      496, &
      1, &
      2, &
      6, &
      327, &
      328, &
      342, &
      344, &
      345, &
      359, &
      360, &
      375, &
      391, &
      392, &
      496, &
      1, &
      2, &
      6, &
      328, &
      329, &
      343, &
      345, &
      346, &
      359, &
      360, &
      361, &
      374, &
      376, &
      392, &
      393, &
      496, &
      1, &
      2, &
      6, &
      329, &
      330, &
      344, &
      346, &
      347, &
      360, &
      361, &
      362, &
      374, &
      375, &
      377, &
      393, &
      394, &
      496, &
      1, &
      2, &
      6, &
      330, &
      331, &
      345, &
      347, &
      348, &
      361, &
      362, &
      363, &
      375, &
      376, &
      378, &
      394, &
      395, &
      496, &
      1, &
      2, &
      6, &
      331, &
      332, &
      346, &
      348, &
      349, &
      362, &
      363, &
      364, &
      376, &
      377, &
      379, &
      395, &
      396, &
      496, &
      1, &
      2, &
      6, &
      332, &
      333, &
      347, &
      349, &
      350, &
      363, &
      364, &
      365, &
      377, &
      378, &
      380, &
      396, &
      397, &
      496, &
      1, &
      2, &
      6, &
      333, &
      334, &
      348, &
      350, &
      351, &
      364, &
      365, &
      366, &
      378, &
      379, &
      381, &
      397, &
      398, &
      496, &
      1, &
      2, &
      6, &
      334, &
      335, &
      349, &
      351, &
      352, &
      365, &
      366, &
      367, &
      379, &
      380, &
      382, &
      398, &
      399, &
      496, &
      1, &
      2, &
      6, &
      335, &
      336, &
      350, &
      352, &
      353, &
      366, &
      367, &
      368, &
      380, &
      381, &
      383, &
      399, &
      400, &
      496, &
      1, &
      2, &
      6, &
      336, &
      337, &
      351, &
      353, &
      354, &
      367, &
      368, &
      369, &
      381, &
      382, &
      384, &
      400, &
      401, &
      496, &
      1, &
      2, &
      6, &
      337, &
      338, &
      352, &
      354, &
      355, &
      368, &
      369, &
      370, &
      382, &
      383, &
      385, &
      401, &
      402, &
      496, &
      1, &
      2, &
      6, &
      338, &
      339, &
      353, &
      355, &
      356, &
      369, &
      370, &
      371, &
      383, &
      384, &
      386, &
      402, &
      403, &
      496, &
      1, &
      2, &
      6, &
      339, &
      340, &
      354, &
      356, &
      357, &
      370, &
      371, &
      372, &
      384, &
      385, &
      387, &
      403, &
      404, &
      496, &
      1, &
      2, &
      6, &
      340, &
      341, &
      355, &
      357, &
      358, &
      371, &
      372, &
      373, &
      385, &
      386, &
      388, &
      404, &
      405, &
      496, &
      1, &
      2, &
      6, &
      341, &
      356, &
      358, &
      372, &
      373, &
      386, &
      387, &
      389, &
      405, &
      406, &
      496, &
      1, &
      2, &
      6, &
      343, &
      344, &
      360, &
      361, &
      374, &
      375, &
      391, &
      393, &
      408, &
      409, &
      496, &
      1, &
      2, &
      6, &
      344, &
      345, &
      359, &
      361, &
      362, &
      374, &
      375, &
      376, &
      391, &
      392, &
      394, &
      409, &
      410, &
      496, &
      1, &
      2, &
      6, &
      345, &
      346, &
      360, &
      362, &
      363, &
      375, &
      376, &
      377, &
      392, &
      393, &
      395, &
      408, &
      410, &
      411, &
      496, &
      1, &
      2, &
      6, &
      346, &
      347, &
      361, &
      363, &
      364, &
      376, &
      377, &
      378, &
      393, &
      394, &
      396, &
      409, &
      411, &
      412, &
      496, &
      1, &
      2, &
      6, &
      347, &
      348, &
      362, &
      364, &
      365, &
      377, &
      378, &
      379, &
      394, &
      395, &
      397, &
      410, &
      412, &
      413, &
      496, &
      1, &
      2, &
      6, &
      348, &
      349, &
      363, &
      365, &
      366, &
      378, &
      379, &
      380, &
      395, &
      396, &
      398, &
      411, &
      413, &
      414, &
      429, &
      496, &
      1, &
      2, &
      6, &
      349, &
      350, &
      364, &
      366, &
      367, &
      379, &
      380, &
      381, &
      396, &
      397, &
      399, &
      414, &
      415, &
      496, &
      1, &
      2, &
      6, &
      350, &
      351, &
      365, &
      367, &
      368, &
      380, &
      381, &
      382, &
      397, &
      398, &
      400, &
      415, &
      416, &
      496, &
      1, &
      2, &
      6, &
      351, &
      352, &
      366, &
      368, &
      369, &
      381, &
      382, &
      383, &
      398, &
      399, &
      401, &
      416, &
      417, &
      496, &
      1, &
      2, &
      6, &
      352, &
      353, &
      367, &
      369, &
      370, &
      382, &
      383, &
      384, &
      399, &
      400, &
      402, &
      417, &
      418, &
      496, &
      1, &
      2, &
      6, &
      353, &
      354, &
      368, &
      370, &
      371, &
      383, &
      384, &
      385, &
      400, &
      401, &
      403, &
      418, &
      419, &
      496, &
      1, &
      2, &
      6, &
      354, &
      355, &
      369, &
      371, &
      372, &
      384, &
      385, &
      386, &
      401, &
      402, &
      404, &
      419, &
      420, &
      496, &
      1, &
      2, &
      6, &
      355, &
      356, &
      370, &
      372, &
      373, &
      385, &
      386, &
      387, &
      402, &
      403, &
      405, &
      420, &
      421, &
      496, &
      1, &
      2, &
      6, &
      356, &
      357, &
      371, &
      373, &
      386, &
      387, &
      388, &
      403, &
      404, &
      406, &
      421, &
      422, &
      496, &
      1, &
      2, &
      6, &
      357, &
      358, &
      372, &
      387, &
      388, &
      389, &
      404, &
      405, &
      422, &
      423, &
      496, &
      1, &
      2, &
      6, &
      358, &
      373, &
      388, &
      389, &
      390, &
      405, &
      406, &
      423, &
      424, &
      496, &
      1, &
      2, &
      6, &
      389, &
      390, &
      406, &
      424, &
      425, &
      496, &
      1, &
      2, &
      6, &
      359, &
      374, &
      375, &
      391, &
      392, &
      407, &
      409, &
      496, &
      1, &
      2, &
      6, &
      359, &
      360, &
      375, &
      376, &
      391, &
      392, &
      393, &
      407, &
      408, &
      410, &
      426, &
      496, &
      1, &
      2, &
      6, &
      360, &
      361, &
      374, &
      376, &
      377, &
      392, &
      393, &
      394, &
      408, &
      409, &
      411, &
      426, &
      427, &
      496, &
      1, &
      2, &
      6, &
      361, &
      362, &
      375, &
      377, &
      378, &
      393, &
      394, &
      395, &
      409, &
      410, &
      412, &
      427, &
      428, &
      496, &
      1, &
      2, &
      6, &
      362, &
      363, &
      376, &
      378, &
      379, &
      394, &
      395, &
      396, &
      410, &
      411, &
      413, &
      428, &
      429, &
      496, &
      1, &
      2, &
      6, &
      363, &
      364, &
      377, &
      379, &
      380, &
      395, &
      396, &
      397, &
      411, &
      412, &
      414, &
      429, &
      430, &
      496, &
      1, &
      2, &
      6, &
      364, &
      365, &
      378, &
      380, &
      381, &
      396, &
      397, &
      398, &
      412, &
      413, &
      415, &
      430, &
      431, &
      496, &
      1, &
      2, &
      6, &
      365, &
      366, &
      379, &
      381, &
      382, &
      397, &
      398, &
      399, &
      413, &
      414, &
      416, &
      431, &
      432, &
      496, &
      1, &
      2, &
      6, &
      366, &
      367, &
      380, &
      382, &
      383, &
      398, &
      399, &
      400, &
      414, &
      415, &
      417, &
      432, &
      433, &
      496, &
      1, &
      2, &
      6, &
      367, &
      368, &
      381, &
      383, &
      384, &
      399, &
      400, &
      401, &
      415, &
      416, &
      418, &
      433, &
      434, &
      496, &
      1, &
      2, &
      6, &
      368, &
      369, &
      382, &
      384, &
      385, &
      400, &
      401, &
      402, &
      416, &
      417, &
      419, &
      434, &
      435, &
      496, &
      1, &
      2, &
      6, &
      369, &
      370, &
      383, &
      385, &
      386, &
      401, &
      402, &
      403, &
      417, &
      418, &
      420, &
      435, &
      436, &
      496, &
      1, &
      2, &
      6, &
      370, &
      371, &
      384, &
      386, &
      387, &
      402, &
      403, &
      404, &
      418, &
      419, &
      421, &
      436, &
      437, &
      496, &
      1, &
      2, &
      6, &
      371, &
      372, &
      385, &
      387, &
      388, &
      403, &
      404, &
      405, &
      419, &
      420, &
      422, &
      437, &
      438, &
      496, &
      1, &
      2, &
      6, &
      372, &
      373, &
      386, &
      388, &
      389, &
      404, &
      405, &
      406, &
      420, &
      421, &
      423, &
      438, &
      496, &
      1, &
      2, &
      6, &
      373, &
      387, &
      389, &
      390, &
      405, &
      406, &
      421, &
      422, &
      424, &
      496, &
      1, &
      2, &
      391, &
      392, &
      407, &
      408, &
      496, &
      1, &
      2, &
      6, &
      374, &
      392, &
      393, &
      407, &
      408, &
      409, &
      426, &
      439, &
      496, &
      1, &
      2, &
      6, &
      374, &
      375, &
      391, &
      393, &
      394, &
      408, &
      409, &
      410, &
      427, &
      439, &
      440, &
      496, &
      1, &
      2, &
      6, &
      375, &
      376, &
      392, &
      394, &
      395, &
      409, &
      410, &
      411, &
      426, &
      428, &
      440, &
      441, &
      496, &
      1, &
      2, &
      6, &
      376, &
      377, &
      393, &
      395, &
      396, &
      410, &
      411, &
      412, &
      426, &
      427, &
      429, &
      441, &
      442, &
      496, &
      1, &
      2, &
      6, &
      377, &
      378, &
      394, &
      396, &
      397, &
      411, &
      412, &
      413, &
      427, &
      428, &
      430, &
      440, &
      442, &
      443, &
      496, &
      1, &
      2, &
      6, &
      378, &
      379, &
      395, &
      397, &
      398, &
      412, &
      413, &
      414, &
      428, &
      429, &
      431, &
      441, &
      443, &
      444, &
      496, &
      1, &
      2, &
      6, &
      379, &
      380, &
      396, &
      398, &
      399, &
      413, &
      414, &
      415, &
      429, &
      430, &
      432, &
      442, &
      444, &
      445, &
      496, &
      1, &
      2, &
      6, &
      380, &
      381, &
      397, &
      399, &
      400, &
      414, &
      415, &
      416, &
      430, &
      431, &
      433, &
      445, &
      446, &
      496, &
      1, &
      2, &
      6, &
      381, &
      382, &
      398, &
      400, &
      401, &
      415, &
      416, &
      417, &
      431, &
      432, &
      434, &
      446, &
      447, &
      496, &
      1, &
      2, &
      6, &
      382, &
      383, &
      399, &
      401, &
      402, &
      416, &
      417, &
      418, &
      432, &
      433, &
      435, &
      447, &
      448, &
      496, &
      1, &
      2, &
      6, &
      383, &
      384, &
      400, &
      402, &
      403, &
      417, &
      418, &
      419, &
      433, &
      434, &
      436, &
      448, &
      449, &
      496, &
      1, &
      2, &
      6, &
      384, &
      385, &
      401, &
      403, &
      404, &
      418, &
      419, &
      420, &
      434, &
      435, &
      437, &
      449, &
      496, &
      1, &
      2, &
      6, &
      385, &
      386, &
      402, &
      404, &
      405, &
      419, &
      420, &
      421, &
      435, &
      436, &
      438, &
      496, &
      1, &
      2, &
      6, &
      386, &
      387, &
      403, &
      405, &
      406, &
      420, &
      421, &
      422, &
      436, &
      437, &
      496, &
      1, &
      2, &
      6, &
      387, &
      388, &
      404, &
      406, &
      421, &
      422, &
      423, &
      437, &
      438, &
      496, &
      1, &
      2, &
      6, &
      388, &
      389, &
      405, &
      422, &
      423, &
      424, &
      438, &
      496, &
      1, &
      2, &
      6, &
      389, &
      390, &
      406, &
      423, &
      424, &
      425, &
      496, &
      1, &
      6, &
      390, &
      424, &
      425, &
      496, &
      1, &
      2, &
      6, &
      392, &
      393, &
      408, &
      410, &
      411, &
      426, &
      427, &
      439, &
      441, &
      451, &
      452, &
      496, &
      1, &
      2, &
      6, &
      393, &
      394, &
      409, &
      411, &
      412, &
      426, &
      427, &
      428, &
      439, &
      440, &
      442, &
      452, &
      453, &
      496, &
      1, &
      2, &
      6, &
      394, &
      395, &
      410, &
      412, &
      413, &
      427, &
      428, &
      429, &
      440, &
      441, &
      443, &
      453, &
      454, &
      496, &
      1, &
      2, &
      6, &
      395, &
      396, &
      411, &
      413, &
      414, &
      428, &
      429, &
      430, &
      441, &
      442, &
      444, &
      454, &
      455, &
      496, &
      1, &
      2, &
      6, &
      396, &
      397, &
      412, &
      414, &
      415, &
      429, &
      430, &
      431, &
      442, &
      443, &
      445, &
      455, &
      456, &
      496, &
      1, &
      2, &
      6, &
      397, &
      398, &
      413, &
      415, &
      416, &
      430, &
      431, &
      432, &
      443, &
      444, &
      446, &
      456, &
      457, &
      496, &
      1, &
      2, &
      6, &
      398, &
      399, &
      414, &
      416, &
      417, &
      431, &
      432, &
      433, &
      444, &
      445, &
      447, &
      457, &
      458, &
      496, &
      1, &
      2, &
      6, &
      399, &
      400, &
      415, &
      417, &
      418, &
      432, &
      433, &
      434, &
      445, &
      446, &
      448, &
      458, &
      459, &
      496, &
      1, &
      2, &
      6, &
      400, &
      401, &
      416, &
      418, &
      419, &
      433, &
      434, &
      435, &
      446, &
      447, &
      449, &
      459, &
      460, &
      496, &
      1, &
      2, &
      6, &
      401, &
      402, &
      417, &
      419, &
      420, &
      434, &
      435, &
      436, &
      447, &
      448, &
      460, &
      461, &
      496, &
      1, &
      2, &
      6, &
      402, &
      403, &
      418, &
      420, &
      421, &
      435, &
      436, &
      437, &
      448, &
      449, &
      461, &
      462, &
      496, &
      1, &
      2, &
      6, &
      403, &
      404, &
      419, &
      421, &
      422, &
      436, &
      437, &
      438, &
      449, &
      462, &
      496, &
      1, &
      2, &
      6, &
      404, &
      405, &
      420, &
      422, &
      423, &
      437, &
      438, &
      496, &
      1, &
      2, &
      6, &
      408, &
      409, &
      426, &
      427, &
      439, &
      440, &
      450, &
      452, &
      463, &
      496, &
      1, &
      2, &
      6, &
      409, &
      410, &
      427, &
      428, &
      439, &
      440, &
      441, &
      450, &
      451, &
      453, &
      463, &
      464, &
      496, &
      1, &
      2, &
      6, &
      410, &
      411, &
      426, &
      428, &
      429, &
      440, &
      441, &
      442, &
      451, &
      452, &
      454, &
      464, &
      465, &
      496, &
      1, &
      2, &
      6, &
      411, &
      412, &
      427, &
      429, &
      430, &
      441, &
      442, &
      443, &
      452, &
      453, &
      455, &
      465, &
      466, &
      496, &
      1, &
      2, &
      6, &
      412, &
      413, &
      428, &
      430, &
      431, &
      442, &
      443, &
      444, &
      453, &
      454, &
      456, &
      466, &
      467, &
      496, &
      1, &
      2, &
      6, &
      413, &
      414, &
      429, &
      431, &
      432, &
      443, &
      444, &
      445, &
      454, &
      455, &
      457, &
      467, &
      468, &
      496, &
      1, &
      2, &
      6, &
      414, &
      415, &
      430, &
      432, &
      433, &
      444, &
      445, &
      446, &
      455, &
      456, &
      458, &
      466, &
      468, &
      469, &
      496, &
      1, &
      2, &
      6, &
      415, &
      416, &
      431, &
      433, &
      434, &
      445, &
      446, &
      447, &
      456, &
      457, &
      459, &
      469, &
      470, &
      496, &
      1, &
      2, &
      6, &
      416, &
      417, &
      432, &
      434, &
      435, &
      446, &
      447, &
      448, &
      457, &
      458, &
      460, &
      470, &
      471, &
      496, &
      1, &
      2, &
      6, &
      417, &
      418, &
      433, &
      435, &
      436, &
      447, &
      448, &
      449, &
      458, &
      459, &
      461, &
      471, &
      472, &
      496, &
      1, &
      2, &
      6, &
      418, &
      419, &
      434, &
      436, &
      437, &
      448, &
      449, &
      459, &
      460, &
      462, &
      472, &
      473, &
      496, &
      1, &
      2, &
      6, &
      439, &
      440, &
      450, &
      451, &
      463, &
      496, &
      1, &
      2, &
      6, &
      426, &
      440, &
      441, &
      450, &
      451, &
      452, &
      464, &
      496, &
      1, &
      2, &
      6, &
      426, &
      427, &
      439, &
      441, &
      442, &
      451, &
      452, &
      453, &
      463, &
      465, &
      496, &
      1, &
      2, &
      6, &
      427, &
      428, &
      440, &
      442, &
      443, &
      452, &
      453, &
      454, &
      463, &
      464, &
      466, &
      476, &
      496, &
      1, &
      2, &
      6, &
      428, &
      429, &
      441, &
      443, &
      444, &
      453, &
      454, &
      455, &
      464, &
      465, &
      467, &
      476, &
      477, &
      496, &
      1, &
      2, &
      6, &
      429, &
      430, &
      442, &
      444, &
      445, &
      454, &
      455, &
      456, &
      465, &
      466, &
      468, &
      477, &
      478, &
      496, &
      1, &
      2, &
      6, &
      430, &
      431, &
      443, &
      445, &
      446, &
      455, &
      456, &
      457, &
      466, &
      467, &
      469, &
      478, &
      479, &
      496, &
      1, &
      2, &
      6, &
      431, &
      432, &
      444, &
      446, &
      447, &
      456, &
      457, &
      458, &
      467, &
      468, &
      470, &
      479, &
      480, &
      496, &
      1, &
      2, &
      6, &
      432, &
      433, &
      445, &
      447, &
      448, &
      457, &
      458, &
      459, &
      468, &
      469, &
      471, &
      480, &
      481, &
      496, &
      1, &
      2, &
      6, &
      433, &
      434, &
      446, &
      448, &
      449, &
      458, &
      459, &
      460, &
      469, &
      470, &
      472, &
      481, &
      482, &
      496, &
      1, &
      2, &
      6, &
      434, &
      435, &
      447, &
      449, &
      459, &
      460, &
      461, &
      470, &
      471, &
      473, &
      482, &
      483, &
      496, &
      1, &
      2, &
      6, &
      435, &
      436, &
      448, &
      460, &
      461, &
      462, &
      471, &
      472, &
      474, &
      483, &
      484, &
      496, &
      1, &
      2, &
      6, &
      436, &
      437, &
      449, &
      461, &
      462, &
      472, &
      473, &
      475, &
      484, &
      496, &
      1, &
      2, &
      6, &
      439, &
      440, &
      450, &
      452, &
      453, &
      463, &
      464, &
      496, &
      1, &
      2, &
      6, &
      440, &
      441, &
      451, &
      453, &
      454, &
      463, &
      464, &
      465, &
      476, &
      485, &
      496, &
      1, &
      2, &
      6, &
      441, &
      442, &
      452, &
      454, &
      455, &
      464, &
      465, &
      466, &
      477, &
      485, &
      486, &
      496, &
      1, &
      2, &
      6, &
      442, &
      443, &
      453, &
      455, &
      456, &
      465, &
      466, &
      467, &
      476, &
      478, &
      486, &
      487, &
      496, &
      1, &
      2, &
      6, &
      443, &
      444, &
      454, &
      456, &
      457, &
      466, &
      467, &
      468, &
      476, &
      477, &
      479, &
      487, &
      488, &
      496, &
      1, &
      2, &
      6, &
      444, &
      445, &
      455, &
      457, &
      458, &
      467, &
      468, &
      469, &
      477, &
      478, &
      480, &
      488, &
      489, &
      496, &
      1, &
      2, &
      6, &
      445, &
      446, &
      456, &
      458, &
      459, &
      468, &
      469, &
      470, &
      478, &
      479, &
      481, &
      487, &
      489, &
      490, &
      496, &
      1, &
      2, &
      6, &
      446, &
      447, &
      457, &
      459, &
      460, &
      469, &
      470, &
      471, &
      479, &
      480, &
      482, &
      490, &
      491, &
      496, &
      1, &
      2, &
      6, &
      447, &
      448, &
      458, &
      460, &
      461, &
      470, &
      471, &
      472, &
      480, &
      481, &
      483, &
      489, &
      491, &
      492, &
      496, &
      1, &
      2, &
      6, &
      448, &
      449, &
      459, &
      461, &
      462, &
      471, &
      472, &
      473, &
      481, &
      482, &
      484, &
      492, &
      496, &
      1, &
      2, &
      6, &
      449, &
      460, &
      462, &
      472, &
      473, &
      474, &
      482, &
      483, &
      496, &
      1, &
      2, &
      6, &
      461, &
      473, &
      474, &
      475, &
      483, &
      484, &
      496, &
      1, &
      2, &
      6, &
      462, &
      474, &
      475, &
      484, &
      496, &
      1, &
      2, &
      6, &
      453, &
      454, &
      464, &
      466, &
      467, &
      476, &
      477, &
      485, &
      487, &
      496, &
      1, &
      2, &
      6, &
      454, &
      455, &
      465, &
      467, &
      468, &
      476, &
      477, &
      478, &
      485, &
      486, &
      488, &
      496, &
      1, &
      2, &
      6, &
      455, &
      456, &
      466, &
      468, &
      469, &
      477, &
      478, &
      479, &
      486, &
      487, &
      489, &
      496, &
      1, &
      2, &
      6, &
      456, &
      457, &
      467, &
      469, &
      470, &
      478, &
      479, &
      480, &
      487, &
      488, &
      490, &
      493, &
      496, &
      1, &
      2, &
      6, &
      457, &
      458, &
      468, &
      470, &
      471, &
      479, &
      480, &
      481, &
      488, &
      489, &
      491, &
      493, &
      494, &
      496, &
      1, &
      2, &
      6, &
      458, &
      459, &
      469, &
      471, &
      472, &
      480, &
      481, &
      482, &
      489, &
      490, &
      492, &
      494, &
      495, &
      496, &
      1, &
      2, &
      6, &
      459, &
      460, &
      470, &
      472, &
      473, &
      481, &
      482, &
      483, &
      490, &
      491, &
      495, &
      496, &
      1, &
      2, &
      6, &
      460, &
      461, &
      471, &
      473, &
      474, &
      482, &
      483, &
      484, &
      491, &
      492, &
      496, &
      1, &
      2, &
      6, &
      461, &
      462, &
      472, &
      474, &
      475, &
      483, &
      484, &
      492, &
      496, &
      1, &
      2, &
      6, &
      464, &
      465, &
      476, &
      477, &
      485, &
      486, &
      496, &
      1, &
      2, &
      6, &
      465, &
      466, &
      477, &
      478, &
      485, &
      486, &
      487, &
      496, &
      1, &
      2, &
      6, &
      466, &
      467, &
      476, &
      478, &
      479, &
      486, &
      487, &
      488, &
      496, &
      1, &
      2, &
      6, &
      467, &
      468, &
      477, &
      479, &
      480, &
      487, &
      488, &
      489, &
      493, &
      496, &
      1, &
      2, &
      6, &
      468, &
      469, &
      478, &
      480, &
      481, &
      488, &
      489, &
      490, &
      494, &
      496, &
      1, &
      2, &
      6, &
      469, &
      470, &
      479, &
      481, &
      482, &
      489, &
      490, &
      491, &
      493, &
      495, &
      496, &
      1, &
      2, &
      6, &
      470, &
      471, &
      480, &
      482, &
      483, &
      490, &
      491, &
      492, &
      493, &
      494, &
      496, &
      1, &
      2, &
      6, &
      471, &
      472, &
      481, &
      483, &
      484, &
      491, &
      492, &
      494, &
      495, &
      496, &
      1, &
      2, &
      6, &
      479, &
      480, &
      488, &
      490, &
      491, &
      493, &
      494, &
      496, &
      1, &
      2, &
      6, &
      480, &
      481, &
      489, &
      491, &
      492, &
      493, &
      494, &
      495, &
      496, &
      1, &
      2, &
      6, &
      481, &
      482, &
      490, &
      492, &
      494, &
      495, &
      496, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      10, &
      11, &
      12, &
      13, &
      14, &
      15, &
      16, &
      17, &
      18, &
      19, &
      20, &
      21, &
      22, &
      23, &
      24, &
      25, &
      26, &
      27, &
      28, &
      29, &
      30, &
      31, &
      32, &
      33, &
      34, &
      35, &
      36, &
      37, &
      38, &
      39, &
      40, &
      41, &
      42, &
      43, &
      44, &
      45, &
      46, &
      47, &
      48, &
      49, &
      50, &
      51, &
      52, &
      53, &
      54, &
      55, &
      56, &
      57, &
      58, &
      59, &
      60, &
      61, &
      62, &
      63, &
      64, &
      65, &
      66, &
      67, &
      68, &
      69, &
      70, &
      71, &
      72, &
      73, &
      74, &
      75, &
      76, &
      77, &
      78, &
      79, &
      80, &
      81, &
      82, &
      83, &
      84, &
      85, &
      86, &
      87, &
      88, &
      89, &
      90, &
      91, &
      92, &
      93, &
      94, &
      95, &
      96, &
      97, &
      98, &
      99, &
      100, &
      101, &
      102, &
      103, &
      104, &
      105, &
      106, &
      107, &
      108, &
      109, &
      110, &
      111, &
      112, &
      113, &
      114, &
      115, &
      116, &
      117, &
      118, &
      119, &
      120, &
      121, &
      122, &
      123, &
      124, &
      125, &
      126, &
      127, &
      128, &
      129, &
      130, &
      131, &
      132, &
      133, &
      134, &
      135, &
      136, &
      137, &
      138, &
      139, &
      140, &
      141, &
      142, &
      143, &
      144, &
      145, &
      146, &
      147, &
      148, &
      149, &
      150, &
      151, &
      152, &
      153, &
      154, &
      155, &
      156, &
      157, &
      158, &
      159, &
      160, &
      161, &
      162, &
      163, &
      164, &
      165, &
      166, &
      167, &
      168, &
      169, &
      170, &
      171, &
      172, &
      173, &
      174, &
      175, &
      176, &
      177, &
      178, &
      179, &
      180, &
      181, &
      182, &
      183, &
      184, &
      185, &
      186, &
      187, &
      188, &
      189, &
      190, &
      191, &
      192, &
      193, &
      194, &
      195, &
      196, &
      197, &
      198, &
      199, &
      200, &
      201, &
      202, &
      203, &
      204, &
      205, &
      206, &
      207, &
      208, &
      209, &
      210, &
      211, &
      212, &
      213, &
      214, &
      215, &
      216, &
      217, &
      218, &
      219, &
      220, &
      221, &
      222, &
      223, &
      224, &
      225, &
      226, &
      227, &
      228, &
      229, &
      230, &
      231, &
      232, &
      233, &
      234, &
      235, &
      236, &
      237, &
      238, &
      239, &
      240, &
      241, &
      242, &
      243, &
      244, &
      245, &
      246, &
      247, &
      248, &
      249, &
      250, &
      251, &
      252, &
      253, &
      254, &
      255, &
      256, &
      257, &
      258, &
      259, &
      260, &
      261, &
      262, &
      263, &
      264, &
      265, &
      266, &
      267, &
      268, &
      269, &
      270, &
      271, &
      272, &
      273, &
      274, &
      275, &
      276, &
      277, &
      278, &
      279, &
      280, &
      281, &
      282, &
      283, &
      284, &
      285, &
      286, &
      287, &
      288, &
      289, &
      290, &
      291, &
      292, &
      293, &
      294, &
      295, &
      296, &
      297, &
      298, &
      299, &
      300, &
      301, &
      302, &
      303, &
      304, &
      305, &
      306, &
      307, &
      308, &
      309, &
      310, &
      311, &
      312, &
      313, &
      314, &
      315, &
      316, &
      317, &
      318, &
      319, &
      320, &
      321, &
      322, &
      323, &
      324, &
      325, &
      326, &
      327, &
      328, &
      329, &
      330, &
      331, &
      332, &
      333, &
      334, &
      335, &
      336, &
      337, &
      338, &
      339, &
      340, &
      341, &
      342, &
      343, &
      344, &
      345, &
      346, &
      347, &
      348, &
      349, &
      350, &
      351, &
      352, &
      353, &
      354, &
      355, &
      356, &
      357, &
      358, &
      359, &
      360, &
      361, &
      362, &
      363, &
      364, &
      365, &
      366, &
      367, &
      368, &
      369, &
      370, &
      371, &
      372, &
      373, &
      374, &
      375, &
      376, &
      377, &
      378, &
      379, &
      380, &
      381, &
      382, &
      383, &
      384, &
      385, &
      386, &
      387, &
      388, &
      389, &
      390, &
      391, &
      392, &
      393, &
      394, &
      395, &
      396, &
      397, &
      398, &
      399, &
      400, &
      401, &
      402, &
      403, &
      404, &
      405, &
      406, &
      407, &
      408, &
      409, &
      410, &
      411, &
      412, &
      413, &
      414, &
      415, &
      416, &
      417, &
      418, &
      419, &
      420, &
      421, &
      422, &
      423, &
      424, &
      425, &
      426, &
      427, &
      428, &
      429, &
      430, &
      431, &
      432, &
      433, &
      434, &
      435, &
      436, &
      437, &
      438, &
      439, &
      440, &
      441, &
      442, &
      443, &
      444, &
      445, &
      446, &
      447, &
      448, &
      449, &
      450, &
      451, &
      452, &
      453, &
      454, &
      455, &
      456, &
      457, &
      458, &
      459, &
      460, &
      461, &
      462, &
      463, &
      464, &
      465, &
      466, &
      467, &
      468, &
      469, &
      470, &
      471, &
      472, &
      473, &
      474, &
      475, &
      476, &
      477, &
      478, &
      479, &
      480, &
      481, &
      482, &
      483, &
      484, &
      485, &
      486, &
      487, &
      488, &
      489, &
      490, &
      491, &
      492, &
      493, &
      494, &
      495, &
      496, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      10, &
      11, &
      12, &
      13, &
      14, &
      15, &
      16, &
      17, &
      18, &
      19, &
      20, &
      21, &
      22, &
      23, &
      24, &
      25, &
      26, &
      27, &
      28, &
      29, &
      30, &
      31, &
      32, &
      33, &
      34, &
      35, &
      36, &
      37, &
      38, &
      39, &
      40, &
      41, &
      42, &
      43, &
      44, &
      45, &
      46, &
      47, &
      48, &
      49, &
      50, &
      51, &
      52, &
      53, &
      54, &
      55, &
      56, &
      57, &
      58, &
      59, &
      60, &
      61, &
      62, &
      63, &
      64, &
      65, &
      66, &
      67, &
      68, &
      69, &
      70, &
      71, &
      72, &
      73, &
      74, &
      75, &
      76, &
      77, &
      78, &
      79, &
      80, &
      81, &
      82, &
      83, &
      84, &
      85, &
      86, &
      87, &
      88, &
      89, &
      90, &
      91, &
      92, &
      93, &
      94, &
      95, &
      96, &
      97, &
      98, &
      99, &
      100, &
      101, &
      102, &
      103, &
      104, &
      105, &
      106, &
      107, &
      108, &
      109, &
      110, &
      111, &
      112, &
      113, &
      114, &
      115, &
      116, &
      117, &
      118, &
      119, &
      120, &
      121, &
      122, &
      123, &
      124, &
      125, &
      126, &
      127, &
      128, &
      129, &
      130, &
      131, &
      132, &
      133, &
      134, &
      135, &
      136, &
      137, &
      138, &
      139, &
      140, &
      141, &
      142, &
      143, &
      144, &
      145, &
      146, &
      147, &
      148, &
      149, &
      150, &
      151, &
      152, &
      153, &
      154, &
      155, &
      156, &
      157, &
      158, &
      159, &
      160, &
      161, &
      162, &
      163, &
      164, &
      165, &
      166, &
      167, &
      168, &
      169, &
      170, &
      171, &
      172, &
      173, &
      174, &
      175, &
      176, &
      177, &
      178, &
      179, &
      180, &
      181, &
      182, &
      183, &
      184, &
      185, &
      186, &
      187, &
      188, &
      189, &
      190, &
      191, &
      192, &
      193, &
      194, &
      195, &
      196, &
      197, &
      198, &
      199, &
      200, &
      201, &
      202, &
      203, &
      204, &
      205, &
      206, &
      207, &
      208, &
      209, &
      210, &
      211, &
      212, &
      213, &
      214, &
      215, &
      216, &
      217, &
      218, &
      219, &
      220, &
      221, &
      222, &
      223, &
      224, &
      225, &
      226, &
      227, &
      228, &
      229, &
      230, &
      231, &
      232, &
      233, &
      234, &
      235, &
      236, &
      237, &
      238, &
      239, &
      240, &
      241, &
      242, &
      243, &
      244, &
      245, &
      246, &
      247, &
      248, &
      249, &
      250, &
      251, &
      252, &
      253, &
      254, &
      255, &
      256, &
      257, &
      258, &
      259, &
      260, &
      261, &
      262, &
      263, &
      264, &
      265, &
      266, &
      267, &
      268, &
      269, &
      270, &
      271, &
      272, &
      273, &
      274, &
      275, &
      276, &
      277, &
      278, &
      279, &
      280, &
      281, &
      282, &
      283, &
      284, &
      285, &
      286, &
      287, &
      288, &
      289, &
      290, &
      291, &
      292, &
      293, &
      294, &
      295, &
      296, &
      297, &
      298, &
      299, &
      300, &
      301, &
      302, &
      303, &
      304, &
      305, &
      306, &
      307, &
      308, &
      309, &
      310, &
      311, &
      312, &
      313, &
      314, &
      315, &
      316, &
      317, &
      318, &
      319, &
      320, &
      321, &
      322, &
      323, &
      324, &
      325, &
      326, &
      327, &
      328, &
      329, &
      330, &
      331, &
      332, &
      333, &
      334, &
      335, &
      336, &
      337, &
      338, &
      339, &
      340, &
      341, &
      342, &
      343, &
      344, &
      345, &
      346, &
      347, &
      348, &
      349, &
      350, &
      351, &
      352, &
      353, &
      354, &
      355, &
      356, &
      357, &
      358, &
      359, &
      360, &
      361, &
      362, &
      363, &
      364, &
      365, &
      366, &
      367, &
      368, &
      369, &
      370, &
      371, &
      372, &
      373, &
      374, &
      375, &
      376, &
      377, &
      378, &
      379, &
      380, &
      381, &
      382, &
      383, &
      384, &
      385, &
      386, &
      387, &
      388, &
      389, &
      390, &
      391, &
      392, &
      393, &
      394, &
      395, &
      396, &
      397, &
      398, &
      399, &
      400, &
      401, &
      402, &
      403, &
      404, &
      405, &
      406, &
      407, &
      408, &
      409, &
      410, &
      411, &
      412, &
      413, &
      414, &
      415, &
      416, &
      417, &
      418, &
      419, &
      420, &
      421, &
      422, &
      423, &
      424, &
      425, &
      426, &
      427, &
      428, &
      429, &
      430, &
      431, &
      432, &
      433, &
      434, &
      435, &
      436, &
      437, &
      438, &
      439, &
      440, &
      441, &
      442, &
      443, &
      444, &
      445, &
      446, &
      447, &
      448, &
      449, &
      450, &
      451, &
      452, &
      453, &
      454, &
      455, &
      456, &
      457, &
      458, &
      459, &
      460, &
      461, &
      462, &
      463, &
      464, &
      465, &
      466, &
      467, &
      468, &
      469, &
      470, &
      471, &
      472, &
      473, &
      474, &
      475, &
      476, &
      477, &
      478, &
      479, &
      480, &
      481, &
      482, &
      483, &
      484, &
      485, &
      486, &
      487, &
      488, &
      489, &
      490, &
      491, &
      492, &
      493, &
      494, &
      495, &
      496, &
      497  ]

    csr_jac_row_count = [ &
      1, &
      495, &
      989, &
      1004, &
      1016, &
      1026, &
      1519, &
      1531, &
      1544, &
      1557, &
      1568, &
      1575, &
      1588, &
      1601, &
      1614, &
      1637, &
      1649, &
      1661, &
      1667, &
      1678, &
      1695, &
      1709, &
      1721, &
      1735, &
      1758, &
      1772, &
      1786, &
      1796, &
      1799, &
      1815, &
      1832, &
      1848, &
      1863, &
      1875, &
      1886, &
      1902, &
      1919, &
      1943, &
      1960, &
      1976, &
      1990, &
      2002, &
      2014, &
      2030, &
      2049, &
      2067, &
      2086, &
      2103, &
      2118, &
      2131, &
      2142, &
      2154, &
      2169, &
      2187, &
      2206, &
      2227, &
      2244, &
      2262, &
      2279, &
      2294, &
      2305, &
      2316, &
      2332, &
      2349, &
      2366, &
      2384, &
      2404, &
      2421, &
      2438, &
      2454, &
      2467, &
      2476, &
      2489, &
      2504, &
      2520, &
      2540, &
      2562, &
      2579, &
      2596, &
      2613, &
      2629, &
      2643, &
      2655, &
      2669, &
      2686, &
      2703, &
      2721, &
      2742, &
      2759, &
      2776, &
      2793, &
      2808, &
      2821, &
      2834, &
      2845, &
      2857, &
      2871, &
      2887, &
      2908, &
      2927, &
      2944, &
      2961, &
      2978, &
      2996, &
      3014, &
      3030, &
      3044, &
      3057, &
      3070, &
      3081, &
      3095, &
      3112, &
      3129, &
      3147, &
      3165, &
      3182, &
      3199, &
      3216, &
      3233, &
      3251, &
      3269, &
      3285, &
      3299, &
      3311, &
      3322, &
      3334, &
      3348, &
      3364, &
      3382, &
      3401, &
      3418, &
      3435, &
      3452, &
      3469, &
      3486, &
      3504, &
      3522, &
      3540, &
      3557, &
      3570, &
      3583, &
      3599, &
      3616, &
      3633, &
      3651, &
      3668, &
      3685, &
      3702, &
      3719, &
      3736, &
      3753, &
      3770, &
      3785, &
      3798, &
      3807, &
      3817, &
      3830, &
      3846, &
      3862, &
      3880, &
      3897, &
      3914, &
      3931, &
      3948, &
      3965, &
      3982, &
      4000, &
      4018, &
      4033, &
      4048, &
      4065, &
      4082, &
      4099, &
      4116, &
      4133, &
      4150, &
      4167, &
      4184, &
      4201, &
      4217, &
      4230, &
      4243, &
      4258, &
      4276, &
      4295, &
      4312, &
      4329, &
      4346, &
      4363, &
      4380, &
      4397, &
      4414, &
      4430, &
      4442, &
      4455, &
      4472, &
      4490, &
      4508, &
      4526, &
      4543, &
      4560, &
      4577, &
      4594, &
      4611, &
      4628, &
      4644, &
      4657, &
      4669, &
      4684, &
      4702, &
      4720, &
      4738, &
      4755, &
      4772, &
      4789, &
      4806, &
      4823, &
      4840, &
      4857, &
      4873, &
      4887, &
      4899, &
      4911, &
      4928, &
      4945, &
      4963, &
      4980, &
      4997, &
      5014, &
      5031, &
      5048, &
      5065, &
      5082, &
      5099, &
      5116, &
      5131, &
      5144, &
      5156, &
      5167, &
      5181, &
      5197, &
      5215, &
      5233, &
      5251, &
      5268, &
      5285, &
      5302, &
      5319, &
      5336, &
      5353, &
      5370, &
      5388, &
      5405, &
      5420, &
      5433, &
      5446, &
      5456, &
      5463, &
      5476, &
      5492, &
      5509, &
      5526, &
      5544, &
      5562, &
      5579, &
      5596, &
      5613, &
      5630, &
      5647, &
      5664, &
      5681, &
      5698, &
      5715, &
      5732, &
      5745, &
      5755, &
      5765, &
      5777, &
      5791, &
      5807, &
      5824, &
      5843, &
      5861, &
      5879, &
      5896, &
      5913, &
      5930, &
      5947, &
      5964, &
      5981, &
      5998, &
      6016, &
      6032, &
      6046, &
      6059, &
      6075, &
      6092, &
      6109, &
      6127, &
      6145, &
      6162, &
      6179, &
      6196, &
      6213, &
      6230, &
      6247, &
      6264, &
      6281, &
      6296, &
      6307, &
      6322, &
      6339, &
      6356, &
      6373, &
      6390, &
      6407, &
      6425, &
      6442, &
      6459, &
      6476, &
      6493, &
      6510, &
      6526, &
      6541, &
      6554, &
      6565, &
      6578, &
      6593, &
      6610, &
      6627, &
      6644, &
      6661, &
      6678, &
      6695, &
      6712, &
      6729, &
      6746, &
      6763, &
      6780, &
      6795, &
      6808, &
      6820, &
      6832, &
      6846, &
      6862, &
      6879, &
      6897, &
      6914, &
      6932, &
      6949, &
      6966, &
      6983, &
      7000, &
      7017, &
      7034, &
      7051, &
      7066, &
      7078, &
      7089, &
      7103, &
      7119, &
      7136, &
      7153, &
      7170, &
      7187, &
      7204, &
      7221, &
      7238, &
      7255, &
      7272, &
      7289, &
      7306, &
      7323, &
      7337, &
      7351, &
      7368, &
      7386, &
      7404, &
      7422, &
      7441, &
      7458, &
      7475, &
      7492, &
      7509, &
      7526, &
      7543, &
      7560, &
      7576, &
      7590, &
      7603, &
      7612, &
      7623, &
      7638, &
      7655, &
      7672, &
      7689, &
      7706, &
      7723, &
      7740, &
      7757, &
      7774, &
      7791, &
      7808, &
      7825, &
      7842, &
      7858, &
      7871, &
      7878, &
      7890, &
      7905, &
      7921, &
      7938, &
      7956, &
      7974, &
      7992, &
      8009, &
      8026, &
      8043, &
      8060, &
      8076, &
      8091, &
      8105, &
      8118, &
      8129, &
      8139, &
      8145, &
      8160, &
      8177, &
      8194, &
      8211, &
      8228, &
      8245, &
      8262, &
      8279, &
      8296, &
      8312, &
      8328, &
      8342, &
      8353, &
      8366, &
      8382, &
      8399, &
      8416, &
      8433, &
      8450, &
      8468, &
      8485, &
      8502, &
      8519, &
      8535, &
      8544, &
      8555, &
      8569, &
      8585, &
      8602, &
      8619, &
      8636, &
      8653, &
      8670, &
      8687, &
      8703, &
      8718, &
      8731, &
      8742, &
      8756, &
      8771, &
      8787, &
      8804, &
      8821, &
      8839, &
      8856, &
      8874, &
      8890, &
      8902, &
      8912, &
      8920, &
      8933, &
      8948, &
      8963, &
      8979, &
      8996, &
      9013, &
      9028, &
      9042, &
      9054, &
      9064, &
      9075, &
      9087, &
      9100, &
      9113, &
      9127, &
      9141, &
      9154, &
      9165, &
      9177, &
      9187, &
      9683, &
      10180  ]
#endif

  end subroutine actual_network_init


  subroutine actual_network_finalize()
    ! Deallocate storage arrays

    if (allocated(bion)) then
       deallocate(bion)
    endif

    if (allocated(mion)) then
       deallocate(mion)
    endif

#ifdef REACT_SPARSE_JACOBIAN
    if (allocated(csr_jac_col_index)) then
       deallocate(csr_jac_col_index)
    endif

    if (allocated(csr_jac_row_count)) then
       deallocate(csr_jac_row_count)
    endif
#endif

  end subroutine actual_network_finalize


  subroutine ener_gener_rate(dydt, enuc)
    ! Computes the instantaneous energy generation rate

    !$acc routine seq

    implicit none

    real(rt) :: dydt(nspec), enuc

    !$gpu

    ! This is basically e = m c**2

    enuc = sum(dydt(:) * mion(:)) * enuc_conv2

  end subroutine ener_gener_rate

end module actual_network
