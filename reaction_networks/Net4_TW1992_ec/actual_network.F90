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

  integer, parameter :: nrates = 707


  ! For each rate, we need: rate, drate/dT, screening, dscreening/dT
  integer, parameter :: num_rate_groups = 4

  ! Number of reaclib rates
  integer, parameter :: nrat_reaclib = 703
  integer, parameter :: number_reaclib_sets = 943

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
  integer, parameter :: jc12   = 7
  integer, parameter :: jn14   = 8
  integer, parameter :: jn15   = 9
  integer, parameter :: jo16   = 10
  integer, parameter :: jo17   = 11
  integer, parameter :: jo18   = 12
  integer, parameter :: jo20   = 13
  integer, parameter :: jf19   = 14
  integer, parameter :: jf20   = 15
  integer, parameter :: jne19   = 16
  integer, parameter :: jne20   = 17
  integer, parameter :: jne21   = 18
  integer, parameter :: jna22   = 19
  integer, parameter :: jna23   = 20
  integer, parameter :: jna24   = 21
  integer, parameter :: jmg23   = 22
  integer, parameter :: jmg24   = 23
  integer, parameter :: jmg25   = 24
  integer, parameter :: jmg26   = 25
  integer, parameter :: jal26   = 26
  integer, parameter :: jal27   = 27
  integer, parameter :: jal28   = 28
  integer, parameter :: jsi27   = 29
  integer, parameter :: jsi28   = 30
  integer, parameter :: jsi29   = 31
  integer, parameter :: jsi30   = 32
  integer, parameter :: jp30   = 33
  integer, parameter :: jp31   = 34
  integer, parameter :: jp32   = 35
  integer, parameter :: js31   = 36
  integer, parameter :: js32   = 37
  integer, parameter :: js33   = 38
  integer, parameter :: js34   = 39
  integer, parameter :: jcl35   = 40
  integer, parameter :: jcl36   = 41
  integer, parameter :: jcl37   = 42
  integer, parameter :: jar36   = 43
  integer, parameter :: jar37   = 44
  integer, parameter :: jar38   = 45
  integer, parameter :: jk39   = 46
  integer, parameter :: jk40   = 47
  integer, parameter :: jk41   = 48
  integer, parameter :: jca40   = 49
  integer, parameter :: jca41   = 50
  integer, parameter :: jca42   = 51
  integer, parameter :: jca43   = 52
  integer, parameter :: jca44   = 53
  integer, parameter :: jsc43   = 54
  integer, parameter :: jsc44   = 55
  integer, parameter :: jsc45   = 56
  integer, parameter :: jti44   = 57
  integer, parameter :: jti45   = 58
  integer, parameter :: jti46   = 59
  integer, parameter :: jti47   = 60
  integer, parameter :: jti48   = 61
  integer, parameter :: jv47   = 62
  integer, parameter :: jv48   = 63
  integer, parameter :: jv49   = 64
  integer, parameter :: jcr48   = 65
  integer, parameter :: jcr49   = 66
  integer, parameter :: jcr50   = 67
  integer, parameter :: jcr51   = 68
  integer, parameter :: jcr52   = 69
  integer, parameter :: jmn51   = 70
  integer, parameter :: jmn52   = 71
  integer, parameter :: jmn53   = 72
  integer, parameter :: jmn54   = 73
  integer, parameter :: jfe52   = 74
  integer, parameter :: jfe53   = 75
  integer, parameter :: jfe54   = 76
  integer, parameter :: jfe55   = 77
  integer, parameter :: jfe56   = 78
  integer, parameter :: jco55   = 79
  integer, parameter :: jco56   = 80
  integer, parameter :: jco57   = 81
  integer, parameter :: jni56   = 82
  integer, parameter :: jni57   = 83
  integer, parameter :: jni58   = 84

  ! Reactions
  integer, parameter :: k_n__p__weak__wc12   = 1
  integer, parameter :: k_t__he3__weak__wc12   = 2
  integer, parameter :: k_he3__t__weak__electron_capture   = 3
  integer, parameter :: k_ne19__f19__weak__wc12   = 4
  integer, parameter :: k_na24__mg24__weak__wc12   = 5
  integer, parameter :: k_mg23__na23__weak__wc12   = 6
  integer, parameter :: k_al26__mg26__weak__wc12   = 7
  integer, parameter :: k_al28__si28__weak__wc12   = 8
  integer, parameter :: k_si27__al27__weak__wc12   = 9
  integer, parameter :: k_p30__si30__weak__wc12   = 10
  integer, parameter :: k_p32__s32__weak__wc12   = 11
  integer, parameter :: k_s31__p31__weak__wc12   = 12
  integer, parameter :: k_cl36__ar36__weak__wc12   = 13
  integer, parameter :: k_ar37__cl37__weak__wc12   = 14
  integer, parameter :: k_k40__ca40__weak__wc12   = 15
  integer, parameter :: k_ca41__k41__weak__wc12   = 16
  integer, parameter :: k_sc43__ca43__weak__wc12   = 17
  integer, parameter :: k_sc44__ca44__weak__wc12   = 18
  integer, parameter :: k_ti44__sc44__weak__wc12   = 19
  integer, parameter :: k_ti45__sc45__weak__wc12   = 20
  integer, parameter :: k_v47__ti47__weak__wc12   = 21
  integer, parameter :: k_v48__ti48__weak__wc12   = 22
  integer, parameter :: k_cr48__v48__weak__wc12   = 23
  integer, parameter :: k_cr49__v49__weak__wc12   = 24
  integer, parameter :: k_mn51__cr51__weak__wc12   = 25
  integer, parameter :: k_mn52__cr52__weak__wc12   = 26
  integer, parameter :: k_mn54__fe54__weak__wc12   = 27
  integer, parameter :: k_fe52__mn52__weak__wc12   = 28
  integer, parameter :: k_fe53__mn53__weak__wc12   = 29
  integer, parameter :: k_co55__fe55__weak__wc12   = 30
  integer, parameter :: k_co56__fe56__weak__wc12   = 31
  integer, parameter :: k_ni56__co56__weak__wc12   = 32
  integer, parameter :: k_ni57__co57__weak__wc12   = 33
  integer, parameter :: k_d__n_p   = 34
  integer, parameter :: k_t__n_d   = 35
  integer, parameter :: k_he3__p_d   = 36
  integer, parameter :: k_he4__n_he3   = 37
  integer, parameter :: k_he4__p_t   = 38
  integer, parameter :: k_he4__d_d   = 39
  integer, parameter :: k_n15__n_n14   = 40
  integer, parameter :: k_o16__p_n15   = 41
  integer, parameter :: k_o16__he4_c12   = 42
  integer, parameter :: k_o17__n_o16   = 43
  integer, parameter :: k_o18__n_o17   = 44
  integer, parameter :: k_f19__p_o18   = 45
  integer, parameter :: k_f19__he4_n15   = 46
  integer, parameter :: k_f20__n_f19   = 47
  integer, parameter :: k_ne20__n_ne19   = 48
  integer, parameter :: k_ne20__p_f19   = 49
  integer, parameter :: k_ne20__he4_o16   = 50
  integer, parameter :: k_ne21__n_ne20   = 51
  integer, parameter :: k_ne21__p_f20   = 52
  integer, parameter :: k_ne21__he4_o17   = 53
  integer, parameter :: k_na22__p_ne21   = 54
  integer, parameter :: k_na23__n_na22   = 55
  integer, parameter :: k_na23__he4_f19   = 56
  integer, parameter :: k_na24__n_na23   = 57
  integer, parameter :: k_na24__he4_f20   = 58
  integer, parameter :: k_mg23__p_na22   = 59
  integer, parameter :: k_mg23__he4_ne19   = 60
  integer, parameter :: k_mg24__n_mg23   = 61
  integer, parameter :: k_mg24__p_na23   = 62
  integer, parameter :: k_mg24__he4_ne20   = 63
  integer, parameter :: k_mg25__n_mg24   = 64
  integer, parameter :: k_mg25__p_na24   = 65
  integer, parameter :: k_mg25__he4_ne21   = 66
  integer, parameter :: k_mg26__n_mg25   = 67
  integer, parameter :: k_al26__p_mg25   = 68
  integer, parameter :: k_al26__he4_na22   = 69
  integer, parameter :: k_al27__n_al26   = 70
  integer, parameter :: k_al27__p_mg26   = 71
  integer, parameter :: k_al27__he4_na23   = 72
  integer, parameter :: k_al28__n_al27   = 73
  integer, parameter :: k_al28__he4_na24   = 74
  integer, parameter :: k_si27__p_al26   = 75
  integer, parameter :: k_si27__he4_mg23   = 76
  integer, parameter :: k_si28__n_si27   = 77
  integer, parameter :: k_si28__p_al27   = 78
  integer, parameter :: k_si28__he4_mg24   = 79
  integer, parameter :: k_si29__n_si28   = 80
  integer, parameter :: k_si29__p_al28   = 81
  integer, parameter :: k_si29__he4_mg25   = 82
  integer, parameter :: k_si30__n_si29   = 83
  integer, parameter :: k_si30__he4_mg26   = 84
  integer, parameter :: k_p30__p_si29   = 85
  integer, parameter :: k_p30__he4_al26   = 86
  integer, parameter :: k_p31__n_p30   = 87
  integer, parameter :: k_p31__p_si30   = 88
  integer, parameter :: k_p31__he4_al27   = 89
  integer, parameter :: k_p32__n_p31   = 90
  integer, parameter :: k_p32__he4_al28   = 91
  integer, parameter :: k_s31__p_p30   = 92
  integer, parameter :: k_s31__he4_si27   = 93
  integer, parameter :: k_s32__n_s31   = 94
  integer, parameter :: k_s32__p_p31   = 95
  integer, parameter :: k_s32__he4_si28   = 96
  integer, parameter :: k_s33__n_s32   = 97
  integer, parameter :: k_s33__p_p32   = 98
  integer, parameter :: k_s33__he4_si29   = 99
  integer, parameter :: k_s34__n_s33   = 100
  integer, parameter :: k_s34__he4_si30   = 101
  integer, parameter :: k_cl35__p_s34   = 102
  integer, parameter :: k_cl35__he4_p31   = 103
  integer, parameter :: k_cl36__n_cl35   = 104
  integer, parameter :: k_cl36__he4_p32   = 105
  integer, parameter :: k_cl37__n_cl36   = 106
  integer, parameter :: k_ar36__p_cl35   = 107
  integer, parameter :: k_ar36__he4_s32   = 108
  integer, parameter :: k_ar37__n_ar36   = 109
  integer, parameter :: k_ar37__p_cl36   = 110
  integer, parameter :: k_ar37__he4_s33   = 111
  integer, parameter :: k_ar38__n_ar37   = 112
  integer, parameter :: k_ar38__p_cl37   = 113
  integer, parameter :: k_ar38__he4_s34   = 114
  integer, parameter :: k_k39__p_ar38   = 115
  integer, parameter :: k_k39__he4_cl35   = 116
  integer, parameter :: k_k40__n_k39   = 117
  integer, parameter :: k_k40__he4_cl36   = 118
  integer, parameter :: k_k41__n_k40   = 119
  integer, parameter :: k_k41__he4_cl37   = 120
  integer, parameter :: k_ca40__p_k39   = 121
  integer, parameter :: k_ca40__he4_ar36   = 122
  integer, parameter :: k_ca41__n_ca40   = 123
  integer, parameter :: k_ca41__p_k40   = 124
  integer, parameter :: k_ca41__he4_ar37   = 125
  integer, parameter :: k_ca42__n_ca41   = 126
  integer, parameter :: k_ca42__p_k41   = 127
  integer, parameter :: k_ca42__he4_ar38   = 128
  integer, parameter :: k_ca43__n_ca42   = 129
  integer, parameter :: k_ca44__n_ca43   = 130
  integer, parameter :: k_sc43__p_ca42   = 131
  integer, parameter :: k_sc43__he4_k39   = 132
  integer, parameter :: k_sc44__n_sc43   = 133
  integer, parameter :: k_sc44__p_ca43   = 134
  integer, parameter :: k_sc44__he4_k40   = 135
  integer, parameter :: k_sc45__n_sc44   = 136
  integer, parameter :: k_sc45__p_ca44   = 137
  integer, parameter :: k_sc45__he4_k41   = 138
  integer, parameter :: k_ti44__p_sc43   = 139
  integer, parameter :: k_ti44__he4_ca40   = 140
  integer, parameter :: k_ti45__n_ti44   = 141
  integer, parameter :: k_ti45__p_sc44   = 142
  integer, parameter :: k_ti45__he4_ca41   = 143
  integer, parameter :: k_ti46__n_ti45   = 144
  integer, parameter :: k_ti46__p_sc45   = 145
  integer, parameter :: k_ti46__he4_ca42   = 146
  integer, parameter :: k_ti47__n_ti46   = 147
  integer, parameter :: k_ti47__he4_ca43   = 148
  integer, parameter :: k_ti48__n_ti47   = 149
  integer, parameter :: k_ti48__he4_ca44   = 150
  integer, parameter :: k_v47__p_ti46   = 151
  integer, parameter :: k_v47__he4_sc43   = 152
  integer, parameter :: k_v48__n_v47   = 153
  integer, parameter :: k_v48__p_ti47   = 154
  integer, parameter :: k_v48__he4_sc44   = 155
  integer, parameter :: k_v49__n_v48   = 156
  integer, parameter :: k_v49__p_ti48   = 157
  integer, parameter :: k_v49__he4_sc45   = 158
  integer, parameter :: k_cr48__p_v47   = 159
  integer, parameter :: k_cr48__he4_ti44   = 160
  integer, parameter :: k_cr49__n_cr48   = 161
  integer, parameter :: k_cr49__p_v48   = 162
  integer, parameter :: k_cr49__he4_ti45   = 163
  integer, parameter :: k_cr50__n_cr49   = 164
  integer, parameter :: k_cr50__p_v49   = 165
  integer, parameter :: k_cr50__he4_ti46   = 166
  integer, parameter :: k_cr51__n_cr50   = 167
  integer, parameter :: k_cr51__he4_ti47   = 168
  integer, parameter :: k_cr52__n_cr51   = 169
  integer, parameter :: k_cr52__he4_ti48   = 170
  integer, parameter :: k_mn51__p_cr50   = 171
  integer, parameter :: k_mn51__he4_v47   = 172
  integer, parameter :: k_mn52__n_mn51   = 173
  integer, parameter :: k_mn52__p_cr51   = 174
  integer, parameter :: k_mn52__he4_v48   = 175
  integer, parameter :: k_mn53__n_mn52   = 176
  integer, parameter :: k_mn53__p_cr52   = 177
  integer, parameter :: k_mn53__he4_v49   = 178
  integer, parameter :: k_mn54__n_mn53   = 179
  integer, parameter :: k_fe52__p_mn51   = 180
  integer, parameter :: k_fe52__he4_cr48   = 181
  integer, parameter :: k_fe53__n_fe52   = 182
  integer, parameter :: k_fe53__p_mn52   = 183
  integer, parameter :: k_fe53__he4_cr49   = 184
  integer, parameter :: k_fe54__n_fe53   = 185
  integer, parameter :: k_fe54__p_mn53   = 186
  integer, parameter :: k_fe54__he4_cr50   = 187
  integer, parameter :: k_fe55__n_fe54   = 188
  integer, parameter :: k_fe55__p_mn54   = 189
  integer, parameter :: k_fe55__he4_cr51   = 190
  integer, parameter :: k_fe56__n_fe55   = 191
  integer, parameter :: k_fe56__he4_cr52   = 192
  integer, parameter :: k_co55__p_fe54   = 193
  integer, parameter :: k_co55__he4_mn51   = 194
  integer, parameter :: k_co56__n_co55   = 195
  integer, parameter :: k_co56__p_fe55   = 196
  integer, parameter :: k_co56__he4_mn52   = 197
  integer, parameter :: k_co57__n_co56   = 198
  integer, parameter :: k_co57__p_fe56   = 199
  integer, parameter :: k_co57__he4_mn53   = 200
  integer, parameter :: k_ni56__p_co55   = 201
  integer, parameter :: k_ni56__he4_fe52   = 202
  integer, parameter :: k_ni57__n_ni56   = 203
  integer, parameter :: k_ni57__p_co56   = 204
  integer, parameter :: k_ni57__he4_fe53   = 205
  integer, parameter :: k_ni58__n_ni57   = 206
  integer, parameter :: k_ni58__p_co57   = 207
  integer, parameter :: k_ni58__he4_fe54   = 208
  integer, parameter :: k_c12__he4_he4_he4   = 209
  integer, parameter :: k_n_p__d   = 210
  integer, parameter :: k_p_p__d__weak__bet_pos_   = 211
  integer, parameter :: k_p_p__d__weak__electron_capture   = 212
  integer, parameter :: k_n_d__t   = 213
  integer, parameter :: k_p_d__he3   = 214
  integer, parameter :: k_d_d__he4   = 215
  integer, parameter :: k_p_t__he4   = 216
  integer, parameter :: k_n_he3__he4   = 217
  integer, parameter :: k_p_he3__he4__weak__bet_pos_   = 218
  integer, parameter :: k_he4_c12__o16   = 219
  integer, parameter :: k_n_n14__n15   = 220
  integer, parameter :: k_p_n15__o16   = 221
  integer, parameter :: k_he4_n15__f19   = 222
  integer, parameter :: k_n_o16__o17   = 223
  integer, parameter :: k_he4_o16__ne20   = 224
  integer, parameter :: k_n_o17__o18   = 225
  integer, parameter :: k_he4_o17__ne21   = 226
  integer, parameter :: k_p_o18__f19   = 227
  integer, parameter :: k_n_f19__f20   = 228
  integer, parameter :: k_p_f19__ne20   = 229
  integer, parameter :: k_he4_f19__na23   = 230
  integer, parameter :: k_p_f20__ne21   = 231
  integer, parameter :: k_he4_f20__na24   = 232
  integer, parameter :: k_n_ne19__ne20   = 233
  integer, parameter :: k_he4_ne19__mg23   = 234
  integer, parameter :: k_n_ne20__ne21   = 235
  integer, parameter :: k_he4_ne20__mg24   = 236
  integer, parameter :: k_p_ne21__na22   = 237
  integer, parameter :: k_he4_ne21__mg25   = 238
  integer, parameter :: k_n_na22__na23   = 239
  integer, parameter :: k_p_na22__mg23   = 240
  integer, parameter :: k_he4_na22__al26   = 241
  integer, parameter :: k_n_na23__na24   = 242
  integer, parameter :: k_p_na23__mg24   = 243
  integer, parameter :: k_he4_na23__al27   = 244
  integer, parameter :: k_p_na24__mg25   = 245
  integer, parameter :: k_he4_na24__al28   = 246
  integer, parameter :: k_n_mg23__mg24   = 247
  integer, parameter :: k_he4_mg23__si27   = 248
  integer, parameter :: k_n_mg24__mg25   = 249
  integer, parameter :: k_he4_mg24__si28   = 250
  integer, parameter :: k_n_mg25__mg26   = 251
  integer, parameter :: k_p_mg25__al26   = 252
  integer, parameter :: k_he4_mg25__si29   = 253
  integer, parameter :: k_p_mg26__al27   = 254
  integer, parameter :: k_he4_mg26__si30   = 255
  integer, parameter :: k_n_al26__al27   = 256
  integer, parameter :: k_p_al26__si27   = 257
  integer, parameter :: k_he4_al26__p30   = 258
  integer, parameter :: k_n_al27__al28   = 259
  integer, parameter :: k_p_al27__si28   = 260
  integer, parameter :: k_he4_al27__p31   = 261
  integer, parameter :: k_p_al28__si29   = 262
  integer, parameter :: k_he4_al28__p32   = 263
  integer, parameter :: k_n_si27__si28   = 264
  integer, parameter :: k_he4_si27__s31   = 265
  integer, parameter :: k_n_si28__si29   = 266
  integer, parameter :: k_he4_si28__s32   = 267
  integer, parameter :: k_n_si29__si30   = 268
  integer, parameter :: k_p_si29__p30   = 269
  integer, parameter :: k_he4_si29__s33   = 270
  integer, parameter :: k_p_si30__p31   = 271
  integer, parameter :: k_he4_si30__s34   = 272
  integer, parameter :: k_n_p30__p31   = 273
  integer, parameter :: k_p_p30__s31   = 274
  integer, parameter :: k_n_p31__p32   = 275
  integer, parameter :: k_p_p31__s32   = 276
  integer, parameter :: k_he4_p31__cl35   = 277
  integer, parameter :: k_p_p32__s33   = 278
  integer, parameter :: k_he4_p32__cl36   = 279
  integer, parameter :: k_n_s31__s32   = 280
  integer, parameter :: k_n_s32__s33   = 281
  integer, parameter :: k_he4_s32__ar36   = 282
  integer, parameter :: k_n_s33__s34   = 283
  integer, parameter :: k_he4_s33__ar37   = 284
  integer, parameter :: k_p_s34__cl35   = 285
  integer, parameter :: k_he4_s34__ar38   = 286
  integer, parameter :: k_n_cl35__cl36   = 287
  integer, parameter :: k_p_cl35__ar36   = 288
  integer, parameter :: k_he4_cl35__k39   = 289
  integer, parameter :: k_n_cl36__cl37   = 290
  integer, parameter :: k_p_cl36__ar37   = 291
  integer, parameter :: k_he4_cl36__k40   = 292
  integer, parameter :: k_p_cl37__ar38   = 293
  integer, parameter :: k_he4_cl37__k41   = 294
  integer, parameter :: k_n_ar36__ar37   = 295
  integer, parameter :: k_he4_ar36__ca40   = 296
  integer, parameter :: k_n_ar37__ar38   = 297
  integer, parameter :: k_he4_ar37__ca41   = 298
  integer, parameter :: k_p_ar38__k39   = 299
  integer, parameter :: k_he4_ar38__ca42   = 300
  integer, parameter :: k_n_k39__k40   = 301
  integer, parameter :: k_p_k39__ca40   = 302
  integer, parameter :: k_he4_k39__sc43   = 303
  integer, parameter :: k_n_k40__k41   = 304
  integer, parameter :: k_p_k40__ca41   = 305
  integer, parameter :: k_he4_k40__sc44   = 306
  integer, parameter :: k_p_k41__ca42   = 307
  integer, parameter :: k_he4_k41__sc45   = 308
  integer, parameter :: k_n_ca40__ca41   = 309
  integer, parameter :: k_he4_ca40__ti44   = 310
  integer, parameter :: k_n_ca41__ca42   = 311
  integer, parameter :: k_he4_ca41__ti45   = 312
  integer, parameter :: k_n_ca42__ca43   = 313
  integer, parameter :: k_p_ca42__sc43   = 314
  integer, parameter :: k_he4_ca42__ti46   = 315
  integer, parameter :: k_n_ca43__ca44   = 316
  integer, parameter :: k_p_ca43__sc44   = 317
  integer, parameter :: k_he4_ca43__ti47   = 318
  integer, parameter :: k_p_ca44__sc45   = 319
  integer, parameter :: k_he4_ca44__ti48   = 320
  integer, parameter :: k_n_sc43__sc44   = 321
  integer, parameter :: k_p_sc43__ti44   = 322
  integer, parameter :: k_he4_sc43__v47   = 323
  integer, parameter :: k_n_sc44__sc45   = 324
  integer, parameter :: k_p_sc44__ti45   = 325
  integer, parameter :: k_he4_sc44__v48   = 326
  integer, parameter :: k_p_sc45__ti46   = 327
  integer, parameter :: k_he4_sc45__v49   = 328
  integer, parameter :: k_n_ti44__ti45   = 329
  integer, parameter :: k_he4_ti44__cr48   = 330
  integer, parameter :: k_n_ti45__ti46   = 331
  integer, parameter :: k_he4_ti45__cr49   = 332
  integer, parameter :: k_n_ti46__ti47   = 333
  integer, parameter :: k_p_ti46__v47   = 334
  integer, parameter :: k_he4_ti46__cr50   = 335
  integer, parameter :: k_n_ti47__ti48   = 336
  integer, parameter :: k_p_ti47__v48   = 337
  integer, parameter :: k_he4_ti47__cr51   = 338
  integer, parameter :: k_p_ti48__v49   = 339
  integer, parameter :: k_he4_ti48__cr52   = 340
  integer, parameter :: k_n_v47__v48   = 341
  integer, parameter :: k_p_v47__cr48   = 342
  integer, parameter :: k_he4_v47__mn51   = 343
  integer, parameter :: k_n_v48__v49   = 344
  integer, parameter :: k_p_v48__cr49   = 345
  integer, parameter :: k_he4_v48__mn52   = 346
  integer, parameter :: k_p_v49__cr50   = 347
  integer, parameter :: k_he4_v49__mn53   = 348
  integer, parameter :: k_n_cr48__cr49   = 349
  integer, parameter :: k_he4_cr48__fe52   = 350
  integer, parameter :: k_n_cr49__cr50   = 351
  integer, parameter :: k_he4_cr49__fe53   = 352
  integer, parameter :: k_n_cr50__cr51   = 353
  integer, parameter :: k_p_cr50__mn51   = 354
  integer, parameter :: k_he4_cr50__fe54   = 355
  integer, parameter :: k_n_cr51__cr52   = 356
  integer, parameter :: k_p_cr51__mn52   = 357
  integer, parameter :: k_he4_cr51__fe55   = 358
  integer, parameter :: k_p_cr52__mn53   = 359
  integer, parameter :: k_he4_cr52__fe56   = 360
  integer, parameter :: k_n_mn51__mn52   = 361
  integer, parameter :: k_p_mn51__fe52   = 362
  integer, parameter :: k_he4_mn51__co55   = 363
  integer, parameter :: k_n_mn52__mn53   = 364
  integer, parameter :: k_p_mn52__fe53   = 365
  integer, parameter :: k_he4_mn52__co56   = 366
  integer, parameter :: k_n_mn53__mn54   = 367
  integer, parameter :: k_p_mn53__fe54   = 368
  integer, parameter :: k_he4_mn53__co57   = 369
  integer, parameter :: k_p_mn54__fe55   = 370
  integer, parameter :: k_n_fe52__fe53   = 371
  integer, parameter :: k_he4_fe52__ni56   = 372
  integer, parameter :: k_n_fe53__fe54   = 373
  integer, parameter :: k_he4_fe53__ni57   = 374
  integer, parameter :: k_n_fe54__fe55   = 375
  integer, parameter :: k_p_fe54__co55   = 376
  integer, parameter :: k_he4_fe54__ni58   = 377
  integer, parameter :: k_n_fe55__fe56   = 378
  integer, parameter :: k_p_fe55__co56   = 379
  integer, parameter :: k_p_fe56__co57   = 380
  integer, parameter :: k_n_co55__co56   = 381
  integer, parameter :: k_p_co55__ni56   = 382
  integer, parameter :: k_n_co56__co57   = 383
  integer, parameter :: k_p_co56__ni57   = 384
  integer, parameter :: k_p_co57__ni58   = 385
  integer, parameter :: k_n_ni56__ni57   = 386
  integer, parameter :: k_n_ni57__ni58   = 387
  integer, parameter :: k_d_d__n_he3   = 388
  integer, parameter :: k_d_d__p_t   = 389
  integer, parameter :: k_p_t__n_he3   = 390
  integer, parameter :: k_p_t__d_d   = 391
  integer, parameter :: k_d_t__n_he4   = 392
  integer, parameter :: k_n_he3__p_t   = 393
  integer, parameter :: k_n_he3__d_d   = 394
  integer, parameter :: k_d_he3__p_he4   = 395
  integer, parameter :: k_t_he3__d_he4   = 396
  integer, parameter :: k_n_he4__d_t   = 397
  integer, parameter :: k_p_he4__d_he3   = 398
  integer, parameter :: k_d_he4__t_he3   = 399
  integer, parameter :: k_he4_c12__p_n15   = 400
  integer, parameter :: k_c12_c12__n_mg23   = 401
  integer, parameter :: k_c12_c12__p_na23   = 402
  integer, parameter :: k_c12_c12__he4_ne20   = 403
  integer, parameter :: k_he4_n14__p_o17   = 404
  integer, parameter :: k_p_n15__he4_c12   = 405
  integer, parameter :: k_he4_n15__p_o18   = 406
  integer, parameter :: k_he4_o16__n_ne19   = 407
  integer, parameter :: k_he4_o16__p_f19   = 408
  integer, parameter :: k_c12_o16__n_si27   = 409
  integer, parameter :: k_c12_o16__p_al27   = 410
  integer, parameter :: k_c12_o16__he4_mg24   = 411
  integer, parameter :: k_o16_o16__n_s31   = 412
  integer, parameter :: k_o16_o16__p_p31   = 413
  integer, parameter :: k_o16_o16__he4_si28   = 414
  integer, parameter :: k_p_o17__he4_n14   = 415
  integer, parameter :: k_he4_o17__n_ne20   = 416
  integer, parameter :: k_he4_o17__p_f20   = 417
  integer, parameter :: k_p_o18__he4_n15   = 418
  integer, parameter :: k_he4_o18__n_ne21   = 419
  integer, parameter :: k_p_f19__n_ne19   = 420
  integer, parameter :: k_p_f19__he4_o16   = 421
  integer, parameter :: k_he4_f19__n_na22   = 422
  integer, parameter :: k_p_f20__n_ne20   = 423
  integer, parameter :: k_p_f20__he4_o17   = 424
  integer, parameter :: k_he4_f20__n_na23   = 425
  integer, parameter :: k_n_ne19__p_f19   = 426
  integer, parameter :: k_n_ne19__he4_o16   = 427
  integer, parameter :: k_he4_ne19__p_na22   = 428
  integer, parameter :: k_n_ne20__p_f20   = 429
  integer, parameter :: k_n_ne20__he4_o17   = 430
  integer, parameter :: k_he4_ne20__n_mg23   = 431
  integer, parameter :: k_he4_ne20__p_na23   = 432
  integer, parameter :: k_he4_ne20__c12_c12   = 433
  integer, parameter :: k_c12_ne20__n_s31   = 434
  integer, parameter :: k_c12_ne20__p_p31   = 435
  integer, parameter :: k_c12_ne20__he4_si28   = 436
  integer, parameter :: k_n_ne21__he4_o18   = 437
  integer, parameter :: k_he4_ne21__n_mg24   = 438
  integer, parameter :: k_he4_ne21__p_na24   = 439
  integer, parameter :: k_n_na22__he4_f19   = 440
  integer, parameter :: k_p_na22__he4_ne19   = 441
  integer, parameter :: k_he4_na22__p_mg25   = 442
  integer, parameter :: k_n_na23__he4_f20   = 443
  integer, parameter :: k_p_na23__n_mg23   = 444
  integer, parameter :: k_p_na23__he4_ne20   = 445
  integer, parameter :: k_p_na23__c12_c12   = 446
  integer, parameter :: k_he4_na23__n_al26   = 447
  integer, parameter :: k_he4_na23__p_mg26   = 448
  integer, parameter :: k_p_na24__n_mg24   = 449
  integer, parameter :: k_p_na24__he4_ne21   = 450
  integer, parameter :: k_he4_na24__n_al27   = 451
  integer, parameter :: k_n_mg23__p_na23   = 452
  integer, parameter :: k_n_mg23__he4_ne20   = 453
  integer, parameter :: k_n_mg23__c12_c12   = 454
  integer, parameter :: k_he4_mg23__p_al26   = 455
  integer, parameter :: k_n_mg24__p_na24   = 456
  integer, parameter :: k_n_mg24__he4_ne21   = 457
  integer, parameter :: k_he4_mg24__n_si27   = 458
  integer, parameter :: k_he4_mg24__p_al27   = 459
  integer, parameter :: k_he4_mg24__c12_o16   = 460
  integer, parameter :: k_p_mg25__he4_na22   = 461
  integer, parameter :: k_he4_mg25__n_si28   = 462
  integer, parameter :: k_he4_mg25__p_al28   = 463
  integer, parameter :: k_p_mg26__n_al26   = 464
  integer, parameter :: k_p_mg26__he4_na23   = 465
  integer, parameter :: k_he4_mg26__n_si29   = 466
  integer, parameter :: k_n_al26__p_mg26   = 467
  integer, parameter :: k_n_al26__he4_na23   = 468
  integer, parameter :: k_p_al26__he4_mg23   = 469
  integer, parameter :: k_he4_al26__p_si29   = 470
  integer, parameter :: k_n_al27__he4_na24   = 471
  integer, parameter :: k_p_al27__n_si27   = 472
  integer, parameter :: k_p_al27__he4_mg24   = 473
  integer, parameter :: k_p_al27__c12_o16   = 474
  integer, parameter :: k_he4_al27__n_p30   = 475
  integer, parameter :: k_he4_al27__p_si30   = 476
  integer, parameter :: k_p_al28__n_si28   = 477
  integer, parameter :: k_p_al28__he4_mg25   = 478
  integer, parameter :: k_he4_al28__n_p31   = 479
  integer, parameter :: k_n_si27__p_al27   = 480
  integer, parameter :: k_n_si27__he4_mg24   = 481
  integer, parameter :: k_n_si27__c12_o16   = 482
  integer, parameter :: k_he4_si27__p_p30   = 483
  integer, parameter :: k_n_si28__p_al28   = 484
  integer, parameter :: k_n_si28__he4_mg25   = 485
  integer, parameter :: k_he4_si28__n_s31   = 486
  integer, parameter :: k_he4_si28__p_p31   = 487
  integer, parameter :: k_he4_si28__c12_ne20   = 488
  integer, parameter :: k_he4_si28__o16_o16   = 489
  integer, parameter :: k_n_si29__he4_mg26   = 490
  integer, parameter :: k_p_si29__he4_al26   = 491
  integer, parameter :: k_he4_si29__n_s32   = 492
  integer, parameter :: k_he4_si29__p_p32   = 493
  integer, parameter :: k_p_si30__n_p30   = 494
  integer, parameter :: k_p_si30__he4_al27   = 495
  integer, parameter :: k_he4_si30__n_s33   = 496
  integer, parameter :: k_n_p30__p_si30   = 497
  integer, parameter :: k_n_p30__he4_al27   = 498
  integer, parameter :: k_p_p30__he4_si27   = 499
  integer, parameter :: k_he4_p30__p_s33   = 500
  integer, parameter :: k_n_p31__he4_al28   = 501
  integer, parameter :: k_p_p31__n_s31   = 502
  integer, parameter :: k_p_p31__he4_si28   = 503
  integer, parameter :: k_p_p31__c12_ne20   = 504
  integer, parameter :: k_p_p31__o16_o16   = 505
  integer, parameter :: k_he4_p31__p_s34   = 506
  integer, parameter :: k_p_p32__n_s32   = 507
  integer, parameter :: k_p_p32__he4_si29   = 508
  integer, parameter :: k_he4_p32__n_cl35   = 509
  integer, parameter :: k_n_s31__p_p31   = 510
  integer, parameter :: k_n_s31__he4_si28   = 511
  integer, parameter :: k_n_s31__c12_ne20   = 512
  integer, parameter :: k_n_s31__o16_o16   = 513
  integer, parameter :: k_n_s32__p_p32   = 514
  integer, parameter :: k_n_s32__he4_si29   = 515
  integer, parameter :: k_he4_s32__p_cl35   = 516
  integer, parameter :: k_n_s33__he4_si30   = 517
  integer, parameter :: k_p_s33__he4_p30   = 518
  integer, parameter :: k_he4_s33__n_ar36   = 519
  integer, parameter :: k_he4_s33__p_cl36   = 520
  integer, parameter :: k_p_s34__he4_p31   = 521
  integer, parameter :: k_he4_s34__n_ar37   = 522
  integer, parameter :: k_he4_s34__p_cl37   = 523
  integer, parameter :: k_n_cl35__he4_p32   = 524
  integer, parameter :: k_p_cl35__he4_s32   = 525
  integer, parameter :: k_he4_cl35__p_ar38   = 526
  integer, parameter :: k_p_cl36__n_ar36   = 527
  integer, parameter :: k_p_cl36__he4_s33   = 528
  integer, parameter :: k_he4_cl36__n_k39   = 529
  integer, parameter :: k_p_cl37__n_ar37   = 530
  integer, parameter :: k_p_cl37__he4_s34   = 531
  integer, parameter :: k_he4_cl37__n_k40   = 532
  integer, parameter :: k_n_ar36__p_cl36   = 533
  integer, parameter :: k_n_ar36__he4_s33   = 534
  integer, parameter :: k_he4_ar36__p_k39   = 535
  integer, parameter :: k_n_ar37__p_cl37   = 536
  integer, parameter :: k_n_ar37__he4_s34   = 537
  integer, parameter :: k_he4_ar37__n_ca40   = 538
  integer, parameter :: k_he4_ar37__p_k40   = 539
  integer, parameter :: k_p_ar38__he4_cl35   = 540
  integer, parameter :: k_he4_ar38__n_ca41   = 541
  integer, parameter :: k_he4_ar38__p_k41   = 542
  integer, parameter :: k_n_k39__he4_cl36   = 543
  integer, parameter :: k_p_k39__he4_ar36   = 544
  integer, parameter :: k_he4_k39__p_ca42   = 545
  integer, parameter :: k_n_k40__he4_cl37   = 546
  integer, parameter :: k_p_k40__n_ca40   = 547
  integer, parameter :: k_p_k40__he4_ar37   = 548
  integer, parameter :: k_he4_k40__n_sc43   = 549
  integer, parameter :: k_he4_k40__p_ca43   = 550
  integer, parameter :: k_p_k41__n_ca41   = 551
  integer, parameter :: k_p_k41__he4_ar38   = 552
  integer, parameter :: k_he4_k41__n_sc44   = 553
  integer, parameter :: k_he4_k41__p_ca44   = 554
  integer, parameter :: k_n_ca40__p_k40   = 555
  integer, parameter :: k_n_ca40__he4_ar37   = 556
  integer, parameter :: k_he4_ca40__p_sc43   = 557
  integer, parameter :: k_n_ca41__p_k41   = 558
  integer, parameter :: k_n_ca41__he4_ar38   = 559
  integer, parameter :: k_he4_ca41__n_ti44   = 560
  integer, parameter :: k_he4_ca41__p_sc44   = 561
  integer, parameter :: k_p_ca42__he4_k39   = 562
  integer, parameter :: k_he4_ca42__n_ti45   = 563
  integer, parameter :: k_he4_ca42__p_sc45   = 564
  integer, parameter :: k_p_ca43__n_sc43   = 565
  integer, parameter :: k_p_ca43__he4_k40   = 566
  integer, parameter :: k_he4_ca43__n_ti46   = 567
  integer, parameter :: k_p_ca44__n_sc44   = 568
  integer, parameter :: k_p_ca44__he4_k41   = 569
  integer, parameter :: k_he4_ca44__n_ti47   = 570
  integer, parameter :: k_n_sc43__p_ca43   = 571
  integer, parameter :: k_n_sc43__he4_k40   = 572
  integer, parameter :: k_p_sc43__he4_ca40   = 573
  integer, parameter :: k_he4_sc43__p_ti46   = 574
  integer, parameter :: k_n_sc44__p_ca44   = 575
  integer, parameter :: k_n_sc44__he4_k41   = 576
  integer, parameter :: k_p_sc44__n_ti44   = 577
  integer, parameter :: k_p_sc44__he4_ca41   = 578
  integer, parameter :: k_he4_sc44__n_v47   = 579
  integer, parameter :: k_he4_sc44__p_ti47   = 580
  integer, parameter :: k_p_sc45__n_ti45   = 581
  integer, parameter :: k_p_sc45__he4_ca42   = 582
  integer, parameter :: k_he4_sc45__n_v48   = 583
  integer, parameter :: k_he4_sc45__p_ti48   = 584
  integer, parameter :: k_n_ti44__p_sc44   = 585
  integer, parameter :: k_n_ti44__he4_ca41   = 586
  integer, parameter :: k_he4_ti44__p_v47   = 587
  integer, parameter :: k_n_ti45__p_sc45   = 588
  integer, parameter :: k_n_ti45__he4_ca42   = 589
  integer, parameter :: k_he4_ti45__n_cr48   = 590
  integer, parameter :: k_he4_ti45__p_v48   = 591
  integer, parameter :: k_n_ti46__he4_ca43   = 592
  integer, parameter :: k_p_ti46__he4_sc43   = 593
  integer, parameter :: k_he4_ti46__n_cr49   = 594
  integer, parameter :: k_he4_ti46__p_v49   = 595
  integer, parameter :: k_n_ti47__he4_ca44   = 596
  integer, parameter :: k_p_ti47__n_v47   = 597
  integer, parameter :: k_p_ti47__he4_sc44   = 598
  integer, parameter :: k_he4_ti47__n_cr50   = 599
  integer, parameter :: k_p_ti48__n_v48   = 600
  integer, parameter :: k_p_ti48__he4_sc45   = 601
  integer, parameter :: k_he4_ti48__n_cr51   = 602
  integer, parameter :: k_n_v47__p_ti47   = 603
  integer, parameter :: k_n_v47__he4_sc44   = 604
  integer, parameter :: k_p_v47__he4_ti44   = 605
  integer, parameter :: k_he4_v47__p_cr50   = 606
  integer, parameter :: k_n_v48__p_ti48   = 607
  integer, parameter :: k_n_v48__he4_sc45   = 608
  integer, parameter :: k_p_v48__n_cr48   = 609
  integer, parameter :: k_p_v48__he4_ti45   = 610
  integer, parameter :: k_he4_v48__n_mn51   = 611
  integer, parameter :: k_he4_v48__p_cr51   = 612
  integer, parameter :: k_p_v49__n_cr49   = 613
  integer, parameter :: k_p_v49__he4_ti46   = 614
  integer, parameter :: k_he4_v49__n_mn52   = 615
  integer, parameter :: k_he4_v49__p_cr52   = 616
  integer, parameter :: k_n_cr48__p_v48   = 617
  integer, parameter :: k_n_cr48__he4_ti45   = 618
  integer, parameter :: k_he4_cr48__p_mn51   = 619
  integer, parameter :: k_n_cr49__p_v49   = 620
  integer, parameter :: k_n_cr49__he4_ti46   = 621
  integer, parameter :: k_he4_cr49__n_fe52   = 622
  integer, parameter :: k_he4_cr49__p_mn52   = 623
  integer, parameter :: k_n_cr50__he4_ti47   = 624
  integer, parameter :: k_p_cr50__he4_v47   = 625
  integer, parameter :: k_he4_cr50__n_fe53   = 626
  integer, parameter :: k_he4_cr50__p_mn53   = 627
  integer, parameter :: k_n_cr51__he4_ti48   = 628
  integer, parameter :: k_p_cr51__n_mn51   = 629
  integer, parameter :: k_p_cr51__he4_v48   = 630
  integer, parameter :: k_he4_cr51__n_fe54   = 631
  integer, parameter :: k_he4_cr51__p_mn54   = 632
  integer, parameter :: k_p_cr52__n_mn52   = 633
  integer, parameter :: k_p_cr52__he4_v49   = 634
  integer, parameter :: k_he4_cr52__n_fe55   = 635
  integer, parameter :: k_n_mn51__p_cr51   = 636
  integer, parameter :: k_n_mn51__he4_v48   = 637
  integer, parameter :: k_p_mn51__he4_cr48   = 638
  integer, parameter :: k_he4_mn51__p_fe54   = 639
  integer, parameter :: k_n_mn52__p_cr52   = 640
  integer, parameter :: k_n_mn52__he4_v49   = 641
  integer, parameter :: k_p_mn52__n_fe52   = 642
  integer, parameter :: k_p_mn52__he4_cr49   = 643
  integer, parameter :: k_he4_mn52__n_co55   = 644
  integer, parameter :: k_he4_mn52__p_fe55   = 645
  integer, parameter :: k_p_mn53__n_fe53   = 646
  integer, parameter :: k_p_mn53__he4_cr50   = 647
  integer, parameter :: k_he4_mn53__n_co56   = 648
  integer, parameter :: k_he4_mn53__p_fe56   = 649
  integer, parameter :: k_p_mn54__n_fe54   = 650
  integer, parameter :: k_p_mn54__he4_cr51   = 651
  integer, parameter :: k_he4_mn54__n_co57   = 652
  integer, parameter :: k_n_fe52__p_mn52   = 653
  integer, parameter :: k_n_fe52__he4_cr49   = 654
  integer, parameter :: k_he4_fe52__p_co55   = 655
  integer, parameter :: k_n_fe53__p_mn53   = 656
  integer, parameter :: k_n_fe53__he4_cr50   = 657
  integer, parameter :: k_he4_fe53__n_ni56   = 658
  integer, parameter :: k_he4_fe53__p_co56   = 659
  integer, parameter :: k_n_fe54__p_mn54   = 660
  integer, parameter :: k_n_fe54__he4_cr51   = 661
  integer, parameter :: k_p_fe54__he4_mn51   = 662
  integer, parameter :: k_he4_fe54__n_ni57   = 663
  integer, parameter :: k_he4_fe54__p_co57   = 664
  integer, parameter :: k_n_fe55__he4_cr52   = 665
  integer, parameter :: k_p_fe55__n_co55   = 666
  integer, parameter :: k_p_fe55__he4_mn52   = 667
  integer, parameter :: k_he4_fe55__n_ni58   = 668
  integer, parameter :: k_p_fe56__n_co56   = 669
  integer, parameter :: k_p_fe56__he4_mn53   = 670
  integer, parameter :: k_n_co55__p_fe55   = 671
  integer, parameter :: k_n_co55__he4_mn52   = 672
  integer, parameter :: k_p_co55__he4_fe52   = 673
  integer, parameter :: k_he4_co55__p_ni58   = 674
  integer, parameter :: k_n_co56__p_fe56   = 675
  integer, parameter :: k_n_co56__he4_mn53   = 676
  integer, parameter :: k_p_co56__n_ni56   = 677
  integer, parameter :: k_p_co56__he4_fe53   = 678
  integer, parameter :: k_n_co57__he4_mn54   = 679
  integer, parameter :: k_p_co57__n_ni57   = 680
  integer, parameter :: k_p_co57__he4_fe54   = 681
  integer, parameter :: k_n_ni56__p_co56   = 682
  integer, parameter :: k_n_ni56__he4_fe53   = 683
  integer, parameter :: k_n_ni57__p_co57   = 684
  integer, parameter :: k_n_ni57__he4_fe54   = 685
  integer, parameter :: k_n_ni58__he4_fe55   = 686
  integer, parameter :: k_p_ni58__he4_co55   = 687
  integer, parameter :: k_p_d__n_p_p   = 688
  integer, parameter :: k_t_t__n_n_he4   = 689
  integer, parameter :: k_t_he3__n_p_he4   = 690
  integer, parameter :: k_he3_he3__p_p_he4   = 691
  integer, parameter :: k_he4_he4_he4__c12   = 692
  integer, parameter :: k_n_p_p__p   = 693
  integer, parameter :: k_n_n_he4__t   = 694
  integer, parameter :: k_n_p_he4__t   = 695
  integer, parameter :: k_p_p_he4__he3   = 696
  integer, parameter :: k_n_he4_he4__p   = 697
  integer, parameter :: k_n_he4_he4__d   = 698
  integer, parameter :: k_p_he4_he4__n   = 699
  integer, parameter :: k_p_he4_he4__d   = 700
  integer, parameter :: k_d_he4_he4__p   = 701
  integer, parameter :: k_he4_he4_he4__n   = 702
  integer, parameter :: k_he4_he4_he4__p   = 703
  integer, parameter :: k_f20__o20   = 704
  integer, parameter :: k_ne20__f20   = 705
  integer, parameter :: k_o20__f20   = 706
  integer, parameter :: k_f20__ne20   = 707

  real(rt), allocatable, save :: bion(:), mion(:)

#ifdef AMREX_USE_CUDA
  attributes(managed) :: bion, mion
#endif

  !$acc declare create(bion, mion)

#ifdef REACT_SPARSE_JACOBIAN
  ! Shape of Jacobian in Compressed Sparse Row format
  integer, parameter   :: NETWORK_SPARSE_JAC_NNZ = 1471
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
    ebind_per_nucleon(jc12)   = 7.68014400000000e+00_rt
    ebind_per_nucleon(jn14)   = 7.47561400000000e+00_rt
    ebind_per_nucleon(jn15)   = 7.69946000000000e+00_rt
    ebind_per_nucleon(jo16)   = 7.97620600000000e+00_rt
    ebind_per_nucleon(jo17)   = 7.75072800000000e+00_rt
    ebind_per_nucleon(jo18)   = 7.76709700000000e+00_rt
    ebind_per_nucleon(jo20)   = 7.56857000000000e+00_rt
    ebind_per_nucleon(jf19)   = 7.77901800000000e+00_rt
    ebind_per_nucleon(jf20)   = 7.72013400000000e+00_rt
    ebind_per_nucleon(jne19)   = 7.56734300000000e+00_rt
    ebind_per_nucleon(jne20)   = 8.03224000000000e+00_rt
    ebind_per_nucleon(jne21)   = 7.97171300000000e+00_rt
    ebind_per_nucleon(jna22)   = 7.91566700000000e+00_rt
    ebind_per_nucleon(jna23)   = 8.11149300000000e+00_rt
    ebind_per_nucleon(jna24)   = 8.06348800000000e+00_rt
    ebind_per_nucleon(jmg23)   = 7.90111500000000e+00_rt
    ebind_per_nucleon(jmg24)   = 8.26070900000000e+00_rt
    ebind_per_nucleon(jmg25)   = 8.22350200000000e+00_rt
    ebind_per_nucleon(jmg26)   = 8.33387000000000e+00_rt
    ebind_per_nucleon(jal26)   = 8.14976500000000e+00_rt
    ebind_per_nucleon(jal27)   = 8.33155300000000e+00_rt
    ebind_per_nucleon(jal28)   = 8.30989400000000e+00_rt
    ebind_per_nucleon(jsi27)   = 8.12434100000000e+00_rt
    ebind_per_nucleon(jsi28)   = 8.44774400000000e+00_rt
    ebind_per_nucleon(jsi29)   = 8.44863500000000e+00_rt
    ebind_per_nucleon(jsi30)   = 8.52065400000000e+00_rt
    ebind_per_nucleon(jp30)   = 8.35350600000000e+00_rt
    ebind_per_nucleon(jp31)   = 8.48116700000000e+00_rt
    ebind_per_nucleon(jp32)   = 8.46412000000000e+00_rt
    ebind_per_nucleon(js31)   = 8.28180000000000e+00_rt
    ebind_per_nucleon(js32)   = 8.49312900000000e+00_rt
    ebind_per_nucleon(js33)   = 8.49763000000000e+00_rt
    ebind_per_nucleon(js34)   = 8.58349800000000e+00_rt
    ebind_per_nucleon(jcl35)   = 8.52027800000000e+00_rt
    ebind_per_nucleon(jcl36)   = 8.52193100000000e+00_rt
    ebind_per_nucleon(jcl37)   = 8.57028100000000e+00_rt
    ebind_per_nucleon(jar36)   = 8.51990900000000e+00_rt
    ebind_per_nucleon(jar37)   = 8.52713900000000e+00_rt
    ebind_per_nucleon(jar38)   = 8.61428000000000e+00_rt
    ebind_per_nucleon(jk39)   = 8.55702500000000e+00_rt
    ebind_per_nucleon(jk40)   = 8.53809000000000e+00_rt
    ebind_per_nucleon(jk41)   = 8.57607200000000e+00_rt
    ebind_per_nucleon(jca40)   = 8.55130300000000e+00_rt
    ebind_per_nucleon(jca41)   = 8.54670600000000e+00_rt
    ebind_per_nucleon(jca42)   = 8.61656300000000e+00_rt
    ebind_per_nucleon(jca43)   = 8.60066300000000e+00_rt
    ebind_per_nucleon(jca44)   = 8.65817500000000e+00_rt
    ebind_per_nucleon(jsc43)   = 8.53082500000000e+00_rt
    ebind_per_nucleon(jsc44)   = 8.55737900000000e+00_rt
    ebind_per_nucleon(jsc45)   = 8.61893100000000e+00_rt
    ebind_per_nucleon(jti44)   = 8.53352000000000e+00_rt
    ebind_per_nucleon(jti45)   = 8.55572200000000e+00_rt
    ebind_per_nucleon(jti46)   = 8.65645100000000e+00_rt
    ebind_per_nucleon(jti47)   = 8.66122700000000e+00_rt
    ebind_per_nucleon(jti48)   = 8.72300600000000e+00_rt
    ebind_per_nucleon(jv47)   = 8.58222500000000e+00_rt
    ebind_per_nucleon(jv48)   = 8.62306100000000e+00_rt
    ebind_per_nucleon(jv49)   = 8.68290800000000e+00_rt
    ebind_per_nucleon(jcr48)   = 8.57226900000000e+00_rt
    ebind_per_nucleon(jcr49)   = 8.61329100000000e+00_rt
    ebind_per_nucleon(jcr50)   = 8.70103200000000e+00_rt
    ebind_per_nucleon(jcr51)   = 8.71200500000000e+00_rt
    ebind_per_nucleon(jcr52)   = 8.77598900000000e+00_rt
    ebind_per_nucleon(jmn51)   = 8.63377200000000e+00_rt
    ebind_per_nucleon(jmn52)   = 8.67032900000000e+00_rt
    ebind_per_nucleon(jmn53)   = 8.73417500000000e+00_rt
    ebind_per_nucleon(jmn54)   = 8.73796500000000e+00_rt
    ebind_per_nucleon(jfe52)   = 8.60957400000000e+00_rt
    ebind_per_nucleon(jfe53)   = 8.64879900000000e+00_rt
    ebind_per_nucleon(jfe54)   = 8.73638200000000e+00_rt
    ebind_per_nucleon(jfe55)   = 8.74659500000000e+00_rt
    ebind_per_nucleon(jfe56)   = 8.79035400000000e+00_rt
    ebind_per_nucleon(jco55)   = 8.66961800000000e+00_rt
    ebind_per_nucleon(jco56)   = 8.69483600000000e+00_rt
    ebind_per_nucleon(jco57)   = 8.74188200000000e+00_rt
    ebind_per_nucleon(jni56)   = 8.64277900000000e+00_rt
    ebind_per_nucleon(jni57)   = 8.67093300000000e+00_rt
    ebind_per_nucleon(jni58)   = 8.73205900000000e+00_rt

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
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      85, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      85, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      85, &
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
      1, &
      2, &
      6, &
      7, &
      9, &
      10, &
      17, &
      20, &
      22, &
      23, &
      27, &
      29, &
      30, &
      34, &
      36, &
      85, &
      1, &
      2, &
      6, &
      8, &
      9, &
      11, &
      85, &
      1, &
      2, &
      6, &
      7, &
      8, &
      9, &
      10, &
      12, &
      14, &
      85, &
      1, &
      2, &
      6, &
      7, &
      9, &
      10, &
      11, &
      14, &
      16, &
      17, &
      23, &
      27, &
      29, &
      30, &
      34, &
      36, &
      85, &
      1, &
      2, &
      6, &
      8, &
      10, &
      11, &
      12, &
      15, &
      17, &
      18, &
      85, &
      1, &
      2, &
      6, &
      9, &
      11, &
      12, &
      14, &
      18, &
      85, &
      13, &
      15, &
      85, &
      1, &
      2, &
      6, &
      9, &
      10, &
      12, &
      14, &
      15, &
      16, &
      17, &
      19, &
      20, &
      85, &
      1, &
      2, &
      6, &
      11, &
      13, &
      14, &
      15, &
      17, &
      18, &
      20, &
      21, &
      85, &
      1, &
      2, &
      6, &
      10, &
      14, &
      16, &
      17, &
      19, &
      22, &
      85, &
      1, &
      2, &
      6, &
      7, &
      10, &
      11, &
      14, &
      15, &
      16, &
      17, &
      18, &
      20, &
      22, &
      23, &
      30, &
      34, &
      36, &
      85, &
      1, &
      2, &
      6, &
      11, &
      12, &
      15, &
      17, &
      18, &
      19, &
      21, &
      23, &
      24, &
      85, &
      1, &
      2, &
      6, &
      14, &
      16, &
      18, &
      19, &
      20, &
      22, &
      24, &
      26, &
      85, &
      1, &
      2, &
      6, &
      7, &
      14, &
      15, &
      17, &
      19, &
      20, &
      21, &
      22, &
      23, &
      25, &
      26, &
      27, &
      85, &
      1, &
      2, &
      6, &
      15, &
      18, &
      20, &
      21, &
      23, &
      24, &
      27, &
      28, &
      85, &
      1, &
      2, &
      6, &
      7, &
      16, &
      17, &
      19, &
      20, &
      22, &
      23, &
      26, &
      29, &
      85, &
      1, &
      2, &
      6, &
      7, &
      10, &
      17, &
      18, &
      20, &
      21, &
      22, &
      23, &
      24, &
      27, &
      29, &
      30, &
      85, &
      1, &
      2, &
      6, &
      18, &
      19, &
      21, &
      23, &
      24, &
      25, &
      26, &
      28, &
      30, &
      31, &
      85, &
      1, &
      2, &
      6, &
      20, &
      24, &
      25, &
      26, &
      27, &
      31, &
      32, &
      85, &
      1, &
      2, &
      6, &
      19, &
      20, &
      22, &
      24, &
      25, &
      26, &
      27, &
      29, &
      31, &
      33, &
      85, &
      1, &
      2, &
      6, &
      7, &
      10, &
      20, &
      21, &
      23, &
      25, &
      26, &
      27, &
      28, &
      29, &
      30, &
      32, &
      33, &
      34, &
      85, &
      1, &
      2, &
      6, &
      21, &
      24, &
      27, &
      28, &
      30, &
      31, &
      34, &
      35, &
      85, &
      1, &
      2, &
      6, &
      7, &
      10, &
      22, &
      23, &
      26, &
      27, &
      29, &
      30, &
      33, &
      36, &
      85, &
      1, &
      2, &
      6, &
      7, &
      10, &
      17, &
      23, &
      24, &
      27, &
      28, &
      29, &
      30, &
      31, &
      34, &
      36, &
      37, &
      85, &
      1, &
      2, &
      6, &
      24, &
      25, &
      26, &
      28, &
      30, &
      31, &
      32, &
      33, &
      35, &
      37, &
      38, &
      85, &
      1, &
      2, &
      6, &
      25, &
      27, &
      31, &
      32, &
      33, &
      34, &
      38, &
      39, &
      85, &
      1, &
      2, &
      6, &
      26, &
      27, &
      29, &
      31, &
      32, &
      33, &
      34, &
      36, &
      38, &
      85, &
      1, &
      2, &
      6, &
      7, &
      10, &
      17, &
      27, &
      28, &
      30, &
      32, &
      33, &
      34, &
      35, &
      36, &
      37, &
      39, &
      40, &
      85, &
      1, &
      2, &
      6, &
      28, &
      31, &
      34, &
      35, &
      37, &
      38, &
      40, &
      41, &
      85, &
      1, &
      2, &
      6, &
      7, &
      10, &
      17, &
      29, &
      30, &
      33, &
      34, &
      36, &
      37, &
      85, &
      1, &
      2, &
      6, &
      30, &
      31, &
      34, &
      35, &
      36, &
      37, &
      38, &
      40, &
      43, &
      85, &
      1, &
      2, &
      6, &
      31, &
      32, &
      33, &
      35, &
      37, &
      38, &
      39, &
      41, &
      43, &
      44, &
      85, &
      1, &
      2, &
      6, &
      32, &
      34, &
      38, &
      39, &
      40, &
      42, &
      44, &
      45, &
      85, &
      1, &
      2, &
      6, &
      34, &
      35, &
      37, &
      39, &
      40, &
      41, &
      43, &
      45, &
      46, &
      85, &
      1, &
      2, &
      6, &
      35, &
      38, &
      40, &
      41, &
      42, &
      43, &
      44, &
      46, &
      47, &
      85, &
      1, &
      2, &
      6, &
      39, &
      41, &
      42, &
      44, &
      45, &
      47, &
      48, &
      85, &
      1, &
      2, &
      6, &
      37, &
      38, &
      40, &
      41, &
      43, &
      44, &
      46, &
      49, &
      85, &
      1, &
      2, &
      6, &
      38, &
      39, &
      41, &
      42, &
      43, &
      44, &
      45, &
      47, &
      49, &
      50, &
      85, &
      1, &
      2, &
      6, &
      39, &
      40, &
      42, &
      44, &
      45, &
      46, &
      48, &
      50, &
      51, &
      85, &
      1, &
      2, &
      6, &
      40, &
      41, &
      43, &
      45, &
      46, &
      47, &
      49, &
      51, &
      54, &
      85, &
      1, &
      2, &
      6, &
      41, &
      42, &
      44, &
      46, &
      47, &
      48, &
      49, &
      50, &
      52, &
      54, &
      55, &
      85, &
      1, &
      2, &
      6, &
      42, &
      45, &
      47, &
      48, &
      50, &
      51, &
      53, &
      55, &
      56, &
      85, &
      1, &
      2, &
      6, &
      43, &
      44, &
      46, &
      47, &
      49, &
      50, &
      54, &
      57, &
      85, &
      1, &
      2, &
      6, &
      44, &
      45, &
      47, &
      48, &
      49, &
      50, &
      51, &
      55, &
      57, &
      58, &
      85, &
      1, &
      2, &
      6, &
      45, &
      46, &
      48, &
      50, &
      51, &
      52, &
      54, &
      56, &
      58, &
      59, &
      85, &
      1, &
      2, &
      6, &
      47, &
      51, &
      52, &
      53, &
      54, &
      55, &
      59, &
      60, &
      85, &
      1, &
      2, &
      6, &
      48, &
      52, &
      53, &
      55, &
      56, &
      60, &
      61, &
      85, &
      1, &
      2, &
      6, &
      46, &
      47, &
      49, &
      51, &
      52, &
      54, &
      55, &
      57, &
      59, &
      62, &
      85, &
      1, &
      2, &
      6, &
      47, &
      48, &
      50, &
      52, &
      53, &
      54, &
      55, &
      56, &
      57, &
      58, &
      60, &
      62, &
      63, &
      85, &
      1, &
      2, &
      6, &
      48, &
      51, &
      53, &
      55, &
      56, &
      58, &
      59, &
      61, &
      63, &
      64, &
      85, &
      1, &
      2, &
      6, &
      49, &
      50, &
      54, &
      55, &
      57, &
      58, &
      62, &
      65, &
      85, &
      1, &
      2, &
      6, &
      50, &
      51, &
      55, &
      56, &
      57, &
      58, &
      59, &
      63, &
      65, &
      66, &
      85, &
      1, &
      2, &
      6, &
      51, &
      52, &
      54, &
      56, &
      58, &
      59, &
      60, &
      62, &
      64, &
      66, &
      67, &
      85, &
      1, &
      2, &
      6, &
      52, &
      53, &
      55, &
      59, &
      60, &
      61, &
      62, &
      63, &
      67, &
      68, &
      85, &
      1, &
      2, &
      6, &
      53, &
      56, &
      60, &
      61, &
      63, &
      64, &
      68, &
      69, &
      85, &
      1, &
      2, &
      6, &
      54, &
      55, &
      57, &
      59, &
      60, &
      62, &
      63, &
      65, &
      67, &
      70, &
      85, &
      1, &
      2, &
      6, &
      55, &
      56, &
      58, &
      60, &
      61, &
      62, &
      63, &
      64, &
      65, &
      66, &
      68, &
      70, &
      71, &
      85, &
      1, &
      2, &
      6, &
      56, &
      59, &
      61, &
      63, &
      64, &
      66, &
      67, &
      69, &
      71, &
      72, &
      85, &
      1, &
      2, &
      6, &
      57, &
      58, &
      62, &
      63, &
      65, &
      66, &
      70, &
      74, &
      85, &
      1, &
      2, &
      6, &
      58, &
      59, &
      63, &
      64, &
      65, &
      66, &
      67, &
      71, &
      74, &
      75, &
      85, &
      1, &
      2, &
      6, &
      59, &
      60, &
      62, &
      64, &
      66, &
      67, &
      68, &
      70, &
      72, &
      75, &
      76, &
      85, &
      1, &
      2, &
      6, &
      60, &
      61, &
      63, &
      67, &
      68, &
      69, &
      70, &
      71, &
      73, &
      76, &
      77, &
      85, &
      1, &
      2, &
      6, &
      61, &
      64, &
      68, &
      69, &
      71, &
      72, &
      77, &
      78, &
      85, &
      1, &
      2, &
      6, &
      62, &
      63, &
      65, &
      67, &
      68, &
      70, &
      71, &
      74, &
      76, &
      79, &
      85, &
      1, &
      2, &
      6, &
      63, &
      64, &
      66, &
      68, &
      69, &
      70, &
      71, &
      72, &
      74, &
      75, &
      77, &
      79, &
      80, &
      85, &
      1, &
      2, &
      6, &
      64, &
      67, &
      69, &
      71, &
      72, &
      73, &
      75, &
      76, &
      78, &
      80, &
      81, &
      85, &
      1, &
      2, &
      6, &
      68, &
      72, &
      73, &
      76, &
      77, &
      81, &
      85, &
      1, &
      2, &
      6, &
      65, &
      66, &
      70, &
      71, &
      74, &
      75, &
      79, &
      82, &
      85, &
      1, &
      2, &
      6, &
      66, &
      67, &
      71, &
      72, &
      74, &
      75, &
      76, &
      80, &
      82, &
      83, &
      85, &
      1, &
      2, &
      6, &
      67, &
      68, &
      70, &
      72, &
      73, &
      75, &
      76, &
      77, &
      79, &
      81, &
      83, &
      84, &
      85, &
      1, &
      2, &
      6, &
      68, &
      69, &
      71, &
      73, &
      76, &
      77, &
      78, &
      79, &
      80, &
      84, &
      85, &
      1, &
      2, &
      6, &
      69, &
      72, &
      77, &
      78, &
      80, &
      81, &
      85, &
      1, &
      2, &
      6, &
      70, &
      71, &
      74, &
      76, &
      77, &
      79, &
      80, &
      82, &
      84, &
      85, &
      1, &
      2, &
      6, &
      71, &
      72, &
      75, &
      77, &
      78, &
      79, &
      80, &
      81, &
      82, &
      83, &
      85, &
      1, &
      2, &
      6, &
      72, &
      73, &
      76, &
      78, &
      80, &
      81, &
      83, &
      84, &
      85, &
      1, &
      2, &
      6, &
      74, &
      75, &
      79, &
      80, &
      82, &
      83, &
      85, &
      1, &
      2, &
      6, &
      75, &
      76, &
      80, &
      81, &
      82, &
      83, &
      84, &
      85, &
      1, &
      2, &
      6, &
      76, &
      77, &
      79, &
      81, &
      83, &
      84, &
      85, &
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
      86  ]

    csr_jac_row_count = [ &
      1, &
      85, &
      169, &
      176, &
      183, &
      190, &
      274, &
      290, &
      297, &
      307, &
      324, &
      335, &
      344, &
      347, &
      360, &
      372, &
      382, &
      400, &
      413, &
      425, &
      441, &
      453, &
      466, &
      482, &
      496, &
      507, &
      521, &
      539, &
      551, &
      565, &
      582, &
      597, &
      609, &
      622, &
      640, &
      652, &
      665, &
      678, &
      692, &
      704, &
      717, &
      730, &
      741, &
      753, &
      767, &
      780, &
      793, &
      808, &
      821, &
      833, &
      847, &
      861, &
      873, &
      884, &
      898, &
      915, &
      929, &
      941, &
      955, &
      970, &
      984, &
      996, &
      1010, &
      1027, &
      1041, &
      1053, &
      1067, &
      1082, &
      1097, &
      1109, &
      1123, &
      1140, &
      1155, &
      1165, &
      1177, &
      1191, &
      1207, &
      1221, &
      1231, &
      1244, &
      1258, &
      1270, &
      1280, &
      1291, &
      1301, &
      1386, &
      1472  ]
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
