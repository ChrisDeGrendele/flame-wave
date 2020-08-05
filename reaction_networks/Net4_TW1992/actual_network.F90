module actual_network

  use network_properties
  use physical_constants, only: ERG_PER_MeV
  use amrex_fort_module, only: rt => amrex_real

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

  integer, parameter :: nrates = 367


  ! For each rate, we need: rate, drate/dT, screening, dscreening/dT
  integer, parameter :: num_rate_groups = 4

  ! Number of reaclib rates
  integer, parameter :: nrat_reaclib = 367
  integer, parameter :: number_reaclib_sets = 483

  ! Number of tabular rates
  integer, parameter :: nrat_tabular = 0

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
  integer, parameter :: jf19   = 13
  integer, parameter :: jf20   = 14
  integer, parameter :: jne19   = 15
  integer, parameter :: jne20   = 16
  integer, parameter :: jne21   = 17
  integer, parameter :: jna22   = 18
  integer, parameter :: jna23   = 19
  integer, parameter :: jna24   = 20
  integer, parameter :: jmg23   = 21
  integer, parameter :: jmg24   = 22
  integer, parameter :: jmg25   = 23
  integer, parameter :: jmg26   = 24
  integer, parameter :: jal26   = 25
  integer, parameter :: jal27   = 26
  integer, parameter :: jal28   = 27
  integer, parameter :: jsi27   = 28
  integer, parameter :: jsi28   = 29
  integer, parameter :: jsi29   = 30
  integer, parameter :: jsi30   = 31
  integer, parameter :: jp30   = 32
  integer, parameter :: jp31   = 33
  integer, parameter :: jp32   = 34
  integer, parameter :: js31   = 35
  integer, parameter :: js32   = 36
  integer, parameter :: js33   = 37
  integer, parameter :: js34   = 38
  integer, parameter :: jcl35   = 39
  integer, parameter :: jcl36   = 40
  integer, parameter :: jcl37   = 41
  integer, parameter :: jar36   = 42
  integer, parameter :: jar37   = 43
  integer, parameter :: jar38   = 44
  integer, parameter :: jk39   = 45
  integer, parameter :: jk40   = 46
  integer, parameter :: jk41   = 47
  integer, parameter :: jca40   = 48
  integer, parameter :: jca41   = 49
  integer, parameter :: jca42   = 50
  integer, parameter :: jca43   = 51
  integer, parameter :: jca44   = 52
  integer, parameter :: jsc43   = 53
  integer, parameter :: jsc44   = 54
  integer, parameter :: jsc45   = 55
  integer, parameter :: jti44   = 56
  integer, parameter :: jti45   = 57
  integer, parameter :: jti46   = 58
  integer, parameter :: jti47   = 59
  integer, parameter :: jti48   = 60
  integer, parameter :: jv47   = 61
  integer, parameter :: jv48   = 62
  integer, parameter :: jv49   = 63
  integer, parameter :: jcr48   = 64
  integer, parameter :: jcr49   = 65
  integer, parameter :: jcr50   = 66
  integer, parameter :: jcr51   = 67
  integer, parameter :: jcr52   = 68
  integer, parameter :: jmn51   = 69
  integer, parameter :: jmn52   = 70
  integer, parameter :: jmn53   = 71
  integer, parameter :: jmn54   = 72
  integer, parameter :: jfe52   = 73
  integer, parameter :: jfe53   = 74
  integer, parameter :: jfe54   = 75
  integer, parameter :: jfe55   = 76
  integer, parameter :: jfe56   = 77
  integer, parameter :: jco55   = 78
  integer, parameter :: jco56   = 79
  integer, parameter :: jco57   = 80
  integer, parameter :: jni56   = 81
  integer, parameter :: jni57   = 82
  integer, parameter :: jni58   = 83

  ! Reactions
  integer, parameter :: k_n__p__weak__wc12   = 1
  integer, parameter :: k_t__he3__weak__wc12   = 2
  integer, parameter :: k_he3__t__weak__electron_capture   = 3
  integer, parameter :: k_f20__ne20__weak__wc12   = 4
  integer, parameter :: k_ne19__f19__weak__wc12   = 5
  integer, parameter :: k_na24__mg24__weak__wc12   = 6
  integer, parameter :: k_mg23__na23__weak__wc12   = 7
  integer, parameter :: k_al26__mg26__weak__wc12   = 8
  integer, parameter :: k_al28__si28__weak__wc12   = 9
  integer, parameter :: k_si27__al27__weak__wc12   = 10
  integer, parameter :: k_p30__si30__weak__wc12   = 11
  integer, parameter :: k_p32__s32__weak__wc12   = 12
  integer, parameter :: k_s31__p31__weak__wc12   = 13
  integer, parameter :: k_cl36__ar36__weak__wc12   = 14
  integer, parameter :: k_ar37__cl37__weak__wc12   = 15
  integer, parameter :: k_k40__ca40__weak__wc12   = 16
  integer, parameter :: k_ca41__k41__weak__wc12   = 17
  integer, parameter :: k_sc43__ca43__weak__wc12   = 18
  integer, parameter :: k_sc44__ca44__weak__wc12   = 19
  integer, parameter :: k_ti44__sc44__weak__wc12   = 20
  integer, parameter :: k_ti45__sc45__weak__wc12   = 21
  integer, parameter :: k_v47__ti47__weak__wc12   = 22
  integer, parameter :: k_v48__ti48__weak__wc12   = 23
  integer, parameter :: k_cr48__v48__weak__wc12   = 24
  integer, parameter :: k_cr49__v49__weak__wc12   = 25
  integer, parameter :: k_mn51__cr51__weak__wc12   = 26
  integer, parameter :: k_mn52__cr52__weak__wc12   = 27
  integer, parameter :: k_mn54__fe54__weak__wc12   = 28
  integer, parameter :: k_fe52__mn52__weak__wc12   = 29
  integer, parameter :: k_fe53__mn53__weak__wc12   = 30
  integer, parameter :: k_co55__fe55__weak__wc12   = 31
  integer, parameter :: k_co56__fe56__weak__wc12   = 32
  integer, parameter :: k_ni56__co56__weak__wc12   = 33
  integer, parameter :: k_ni57__co57__weak__wc12   = 34
  integer, parameter :: k_n_p__d   = 35
  integer, parameter :: k_p_p__d__weak__bet_pos_   = 36
  integer, parameter :: k_p_p__d__weak__electron_capture   = 37
  integer, parameter :: k_n_d__t   = 38
  integer, parameter :: k_p_d__he3   = 39
  integer, parameter :: k_d_d__he4   = 40
  integer, parameter :: k_p_t__he4   = 41
  integer, parameter :: k_n_he3__he4   = 42
  integer, parameter :: k_p_he3__he4__weak__bet_pos_   = 43
  integer, parameter :: k_he4_c12__o16   = 44
  integer, parameter :: k_n_n14__n15   = 45
  integer, parameter :: k_p_n15__o16   = 46
  integer, parameter :: k_he4_n15__f19   = 47
  integer, parameter :: k_n_o16__o17   = 48
  integer, parameter :: k_he4_o16__ne20   = 49
  integer, parameter :: k_n_o17__o18   = 50
  integer, parameter :: k_he4_o17__ne21   = 51
  integer, parameter :: k_p_o18__f19   = 52
  integer, parameter :: k_n_f19__f20   = 53
  integer, parameter :: k_p_f19__ne20   = 54
  integer, parameter :: k_he4_f19__na23   = 55
  integer, parameter :: k_p_f20__ne21   = 56
  integer, parameter :: k_he4_f20__na24   = 57
  integer, parameter :: k_n_ne19__ne20   = 58
  integer, parameter :: k_he4_ne19__mg23   = 59
  integer, parameter :: k_n_ne20__ne21   = 60
  integer, parameter :: k_he4_ne20__mg24   = 61
  integer, parameter :: k_p_ne21__na22   = 62
  integer, parameter :: k_he4_ne21__mg25   = 63
  integer, parameter :: k_n_na22__na23   = 64
  integer, parameter :: k_p_na22__mg23   = 65
  integer, parameter :: k_he4_na22__al26   = 66
  integer, parameter :: k_n_na23__na24   = 67
  integer, parameter :: k_p_na23__mg24   = 68
  integer, parameter :: k_he4_na23__al27   = 69
  integer, parameter :: k_p_na24__mg25   = 70
  integer, parameter :: k_he4_na24__al28   = 71
  integer, parameter :: k_n_mg23__mg24   = 72
  integer, parameter :: k_he4_mg23__si27   = 73
  integer, parameter :: k_n_mg24__mg25   = 74
  integer, parameter :: k_he4_mg24__si28   = 75
  integer, parameter :: k_n_mg25__mg26   = 76
  integer, parameter :: k_p_mg25__al26   = 77
  integer, parameter :: k_he4_mg25__si29   = 78
  integer, parameter :: k_p_mg26__al27   = 79
  integer, parameter :: k_he4_mg26__si30   = 80
  integer, parameter :: k_n_al26__al27   = 81
  integer, parameter :: k_p_al26__si27   = 82
  integer, parameter :: k_he4_al26__p30   = 83
  integer, parameter :: k_n_al27__al28   = 84
  integer, parameter :: k_p_al27__si28   = 85
  integer, parameter :: k_he4_al27__p31   = 86
  integer, parameter :: k_p_al28__si29   = 87
  integer, parameter :: k_he4_al28__p32   = 88
  integer, parameter :: k_n_si27__si28   = 89
  integer, parameter :: k_he4_si27__s31   = 90
  integer, parameter :: k_n_si28__si29   = 91
  integer, parameter :: k_he4_si28__s32   = 92
  integer, parameter :: k_n_si29__si30   = 93
  integer, parameter :: k_p_si29__p30   = 94
  integer, parameter :: k_he4_si29__s33   = 95
  integer, parameter :: k_p_si30__p31   = 96
  integer, parameter :: k_he4_si30__s34   = 97
  integer, parameter :: k_n_p30__p31   = 98
  integer, parameter :: k_p_p30__s31   = 99
  integer, parameter :: k_n_p31__p32   = 100
  integer, parameter :: k_p_p31__s32   = 101
  integer, parameter :: k_he4_p31__cl35   = 102
  integer, parameter :: k_p_p32__s33   = 103
  integer, parameter :: k_he4_p32__cl36   = 104
  integer, parameter :: k_n_s31__s32   = 105
  integer, parameter :: k_n_s32__s33   = 106
  integer, parameter :: k_he4_s32__ar36   = 107
  integer, parameter :: k_n_s33__s34   = 108
  integer, parameter :: k_he4_s33__ar37   = 109
  integer, parameter :: k_p_s34__cl35   = 110
  integer, parameter :: k_he4_s34__ar38   = 111
  integer, parameter :: k_n_cl35__cl36   = 112
  integer, parameter :: k_p_cl35__ar36   = 113
  integer, parameter :: k_he4_cl35__k39   = 114
  integer, parameter :: k_n_cl36__cl37   = 115
  integer, parameter :: k_p_cl36__ar37   = 116
  integer, parameter :: k_he4_cl36__k40   = 117
  integer, parameter :: k_p_cl37__ar38   = 118
  integer, parameter :: k_he4_cl37__k41   = 119
  integer, parameter :: k_n_ar36__ar37   = 120
  integer, parameter :: k_he4_ar36__ca40   = 121
  integer, parameter :: k_n_ar37__ar38   = 122
  integer, parameter :: k_he4_ar37__ca41   = 123
  integer, parameter :: k_p_ar38__k39   = 124
  integer, parameter :: k_he4_ar38__ca42   = 125
  integer, parameter :: k_n_k39__k40   = 126
  integer, parameter :: k_p_k39__ca40   = 127
  integer, parameter :: k_he4_k39__sc43   = 128
  integer, parameter :: k_n_k40__k41   = 129
  integer, parameter :: k_p_k40__ca41   = 130
  integer, parameter :: k_he4_k40__sc44   = 131
  integer, parameter :: k_p_k41__ca42   = 132
  integer, parameter :: k_he4_k41__sc45   = 133
  integer, parameter :: k_n_ca40__ca41   = 134
  integer, parameter :: k_he4_ca40__ti44   = 135
  integer, parameter :: k_n_ca41__ca42   = 136
  integer, parameter :: k_he4_ca41__ti45   = 137
  integer, parameter :: k_n_ca42__ca43   = 138
  integer, parameter :: k_p_ca42__sc43   = 139
  integer, parameter :: k_he4_ca42__ti46   = 140
  integer, parameter :: k_n_ca43__ca44   = 141
  integer, parameter :: k_p_ca43__sc44   = 142
  integer, parameter :: k_he4_ca43__ti47   = 143
  integer, parameter :: k_p_ca44__sc45   = 144
  integer, parameter :: k_he4_ca44__ti48   = 145
  integer, parameter :: k_n_sc43__sc44   = 146
  integer, parameter :: k_p_sc43__ti44   = 147
  integer, parameter :: k_he4_sc43__v47   = 148
  integer, parameter :: k_n_sc44__sc45   = 149
  integer, parameter :: k_p_sc44__ti45   = 150
  integer, parameter :: k_he4_sc44__v48   = 151
  integer, parameter :: k_p_sc45__ti46   = 152
  integer, parameter :: k_he4_sc45__v49   = 153
  integer, parameter :: k_n_ti44__ti45   = 154
  integer, parameter :: k_he4_ti44__cr48   = 155
  integer, parameter :: k_n_ti45__ti46   = 156
  integer, parameter :: k_he4_ti45__cr49   = 157
  integer, parameter :: k_n_ti46__ti47   = 158
  integer, parameter :: k_p_ti46__v47   = 159
  integer, parameter :: k_he4_ti46__cr50   = 160
  integer, parameter :: k_n_ti47__ti48   = 161
  integer, parameter :: k_p_ti47__v48   = 162
  integer, parameter :: k_he4_ti47__cr51   = 163
  integer, parameter :: k_p_ti48__v49   = 164
  integer, parameter :: k_he4_ti48__cr52   = 165
  integer, parameter :: k_n_v47__v48   = 166
  integer, parameter :: k_p_v47__cr48   = 167
  integer, parameter :: k_he4_v47__mn51   = 168
  integer, parameter :: k_n_v48__v49   = 169
  integer, parameter :: k_p_v48__cr49   = 170
  integer, parameter :: k_he4_v48__mn52   = 171
  integer, parameter :: k_p_v49__cr50   = 172
  integer, parameter :: k_he4_v49__mn53   = 173
  integer, parameter :: k_n_cr48__cr49   = 174
  integer, parameter :: k_he4_cr48__fe52   = 175
  integer, parameter :: k_n_cr49__cr50   = 176
  integer, parameter :: k_he4_cr49__fe53   = 177
  integer, parameter :: k_n_cr50__cr51   = 178
  integer, parameter :: k_p_cr50__mn51   = 179
  integer, parameter :: k_he4_cr50__fe54   = 180
  integer, parameter :: k_n_cr51__cr52   = 181
  integer, parameter :: k_p_cr51__mn52   = 182
  integer, parameter :: k_he4_cr51__fe55   = 183
  integer, parameter :: k_p_cr52__mn53   = 184
  integer, parameter :: k_he4_cr52__fe56   = 185
  integer, parameter :: k_n_mn51__mn52   = 186
  integer, parameter :: k_p_mn51__fe52   = 187
  integer, parameter :: k_he4_mn51__co55   = 188
  integer, parameter :: k_n_mn52__mn53   = 189
  integer, parameter :: k_p_mn52__fe53   = 190
  integer, parameter :: k_he4_mn52__co56   = 191
  integer, parameter :: k_n_mn53__mn54   = 192
  integer, parameter :: k_p_mn53__fe54   = 193
  integer, parameter :: k_he4_mn53__co57   = 194
  integer, parameter :: k_p_mn54__fe55   = 195
  integer, parameter :: k_n_fe52__fe53   = 196
  integer, parameter :: k_he4_fe52__ni56   = 197
  integer, parameter :: k_n_fe53__fe54   = 198
  integer, parameter :: k_he4_fe53__ni57   = 199
  integer, parameter :: k_n_fe54__fe55   = 200
  integer, parameter :: k_p_fe54__co55   = 201
  integer, parameter :: k_he4_fe54__ni58   = 202
  integer, parameter :: k_n_fe55__fe56   = 203
  integer, parameter :: k_p_fe55__co56   = 204
  integer, parameter :: k_p_fe56__co57   = 205
  integer, parameter :: k_n_co55__co56   = 206
  integer, parameter :: k_p_co55__ni56   = 207
  integer, parameter :: k_n_co56__co57   = 208
  integer, parameter :: k_p_co56__ni57   = 209
  integer, parameter :: k_p_co57__ni58   = 210
  integer, parameter :: k_n_ni56__ni57   = 211
  integer, parameter :: k_n_ni57__ni58   = 212
  integer, parameter :: k_d_d__n_he3   = 213
  integer, parameter :: k_d_d__p_t   = 214
  integer, parameter :: k_d_t__n_he4   = 215
  integer, parameter :: k_n_he3__p_t   = 216
  integer, parameter :: k_d_he3__p_he4   = 217
  integer, parameter :: k_t_he3__d_he4   = 218
  integer, parameter :: k_c12_c12__p_na23   = 219
  integer, parameter :: k_c12_c12__he4_ne20   = 220
  integer, parameter :: k_p_n15__he4_c12   = 221
  integer, parameter :: k_c12_o16__p_al27   = 222
  integer, parameter :: k_c12_o16__he4_mg24   = 223
  integer, parameter :: k_o16_o16__n_s31   = 224
  integer, parameter :: k_o16_o16__p_p31   = 225
  integer, parameter :: k_o16_o16__he4_si28   = 226
  integer, parameter :: k_p_o17__he4_n14   = 227
  integer, parameter :: k_he4_o17__n_ne20   = 228
  integer, parameter :: k_p_o18__he4_n15   = 229
  integer, parameter :: k_he4_o18__n_ne21   = 230
  integer, parameter :: k_p_f19__n_ne19   = 231
  integer, parameter :: k_p_f19__he4_o16   = 232
  integer, parameter :: k_p_f20__n_ne20   = 233
  integer, parameter :: k_p_f20__he4_o17   = 234
  integer, parameter :: k_he4_f20__n_na23   = 235
  integer, parameter :: k_n_ne19__he4_o16   = 236
  integer, parameter :: k_he4_ne19__p_na22   = 237
  integer, parameter :: k_c12_ne20__n_s31   = 238
  integer, parameter :: k_c12_ne20__p_p31   = 239
  integer, parameter :: k_c12_ne20__he4_si28   = 240
  integer, parameter :: k_he4_ne21__n_mg24   = 241
  integer, parameter :: k_n_na22__he4_f19   = 242
  integer, parameter :: k_he4_na22__p_mg25   = 243
  integer, parameter :: k_p_na23__n_mg23   = 244
  integer, parameter :: k_p_na23__he4_ne20   = 245
  integer, parameter :: k_he4_na23__p_mg26   = 246
  integer, parameter :: k_p_na24__n_mg24   = 247
  integer, parameter :: k_p_na24__he4_ne21   = 248
  integer, parameter :: k_he4_na24__n_al27   = 249
  integer, parameter :: k_n_mg23__he4_ne20   = 250
  integer, parameter :: k_n_mg23__c12_c12   = 251
  integer, parameter :: k_he4_mg23__p_al26   = 252
  integer, parameter :: k_he4_mg25__n_si28   = 253
  integer, parameter :: k_he4_mg26__n_si29   = 254
  integer, parameter :: k_n_al26__p_mg26   = 255
  integer, parameter :: k_n_al26__he4_na23   = 256
  integer, parameter :: k_he4_al26__p_si29   = 257
  integer, parameter :: k_p_al27__he4_mg24   = 258
  integer, parameter :: k_he4_al27__n_p30   = 259
  integer, parameter :: k_he4_al27__p_si30   = 260
  integer, parameter :: k_p_al28__n_si28   = 261
  integer, parameter :: k_p_al28__he4_mg25   = 262
  integer, parameter :: k_he4_al28__n_p31   = 263
  integer, parameter :: k_n_si27__p_al27   = 264
  integer, parameter :: k_n_si27__he4_mg24   = 265
  integer, parameter :: k_n_si27__c12_o16   = 266
  integer, parameter :: k_he4_si27__p_p30   = 267
  integer, parameter :: k_n_p30__p_si30   = 268
  integer, parameter :: k_he4_p30__p_s33   = 269
  integer, parameter :: k_p_p31__he4_si28   = 270
  integer, parameter :: k_he4_p31__p_s34   = 271
  integer, parameter :: k_p_p32__n_s32   = 272
  integer, parameter :: k_p_p32__he4_si29   = 273
  integer, parameter :: k_n_s31__p_p31   = 274
  integer, parameter :: k_n_s31__he4_si28   = 275
  integer, parameter :: k_n_s32__he4_si29   = 276
  integer, parameter :: k_n_s33__he4_si30   = 277
  integer, parameter :: k_he4_s34__n_ar37   = 278
  integer, parameter :: k_he4_s34__p_cl37   = 279
  integer, parameter :: k_n_cl35__he4_p32   = 280
  integer, parameter :: k_p_cl35__he4_s32   = 281
  integer, parameter :: k_he4_cl35__p_ar38   = 282
  integer, parameter :: k_p_cl36__he4_s33   = 283
  integer, parameter :: k_n_ar36__p_cl36   = 284
  integer, parameter :: k_n_ar36__he4_s33   = 285
  integer, parameter :: k_n_ar37__p_cl37   = 286
  integer, parameter :: k_n_k39__he4_cl36   = 287
  integer, parameter :: k_p_k39__he4_ar36   = 288
  integer, parameter :: k_n_k40__he4_cl37   = 289
  integer, parameter :: k_p_k40__n_ca40   = 290
  integer, parameter :: k_p_k40__he4_ar37   = 291
  integer, parameter :: k_he4_k40__p_ca43   = 292
  integer, parameter :: k_p_k41__n_ca41   = 293
  integer, parameter :: k_p_k41__he4_ar38   = 294
  integer, parameter :: k_he4_k41__n_sc44   = 295
  integer, parameter :: k_he4_k41__p_ca44   = 296
  integer, parameter :: k_n_ca40__he4_ar37   = 297
  integer, parameter :: k_n_ca41__he4_ar38   = 298
  integer, parameter :: k_p_ca42__he4_k39   = 299
  integer, parameter :: k_he4_ca43__n_ti46   = 300
  integer, parameter :: k_n_sc43__p_ca43   = 301
  integer, parameter :: k_n_sc43__he4_k40   = 302
  integer, parameter :: k_p_sc43__he4_ca40   = 303
  integer, parameter :: k_he4_sc43__p_ti46   = 304
  integer, parameter :: k_n_sc44__p_ca44   = 305
  integer, parameter :: k_p_sc44__he4_ca41   = 306
  integer, parameter :: k_he4_sc44__p_ti47   = 307
  integer, parameter :: k_p_sc45__he4_ca42   = 308
  integer, parameter :: k_he4_sc45__p_ti48   = 309
  integer, parameter :: k_n_ti44__p_sc44   = 310
  integer, parameter :: k_n_ti44__he4_ca41   = 311
  integer, parameter :: k_he4_ti44__p_v47   = 312
  integer, parameter :: k_n_ti45__p_sc45   = 313
  integer, parameter :: k_n_ti45__he4_ca42   = 314
  integer, parameter :: k_he4_ti45__p_v48   = 315
  integer, parameter :: k_n_ti47__he4_ca44   = 316
  integer, parameter :: k_n_v47__p_ti47   = 317
  integer, parameter :: k_n_v47__he4_sc44   = 318
  integer, parameter :: k_he4_v47__p_cr50   = 319
  integer, parameter :: k_n_v48__p_ti48   = 320
  integer, parameter :: k_n_v48__he4_sc45   = 321
  integer, parameter :: k_he4_v48__p_cr51   = 322
  integer, parameter :: k_p_v49__he4_ti46   = 323
  integer, parameter :: k_he4_v49__p_cr52   = 324
  integer, parameter :: k_n_cr48__p_v48   = 325
  integer, parameter :: k_n_cr48__he4_ti45   = 326
  integer, parameter :: k_he4_cr48__p_mn51   = 327
  integer, parameter :: k_n_cr49__p_v49   = 328
  integer, parameter :: k_n_cr49__he4_ti46   = 329
  integer, parameter :: k_he4_cr49__p_mn52   = 330
  integer, parameter :: k_n_cr50__he4_ti47   = 331
  integer, parameter :: k_n_cr51__he4_ti48   = 332
  integer, parameter :: k_n_mn51__p_cr51   = 333
  integer, parameter :: k_n_mn51__he4_v48   = 334
  integer, parameter :: k_he4_mn51__p_fe54   = 335
  integer, parameter :: k_n_mn52__p_cr52   = 336
  integer, parameter :: k_n_mn52__he4_v49   = 337
  integer, parameter :: k_he4_mn52__p_fe55   = 338
  integer, parameter :: k_p_mn53__he4_cr50   = 339
  integer, parameter :: k_he4_mn53__p_fe56   = 340
  integer, parameter :: k_p_mn54__he4_cr51   = 341
  integer, parameter :: k_n_fe52__p_mn52   = 342
  integer, parameter :: k_n_fe52__he4_cr49   = 343
  integer, parameter :: k_he4_fe52__p_co55   = 344
  integer, parameter :: k_n_fe53__p_mn53   = 345
  integer, parameter :: k_n_fe53__he4_cr50   = 346
  integer, parameter :: k_he4_fe53__p_co56   = 347
  integer, parameter :: k_n_fe54__p_mn54   = 348
  integer, parameter :: k_n_fe54__he4_cr51   = 349
  integer, parameter :: k_n_fe55__he4_cr52   = 350
  integer, parameter :: k_n_co55__p_fe55   = 351
  integer, parameter :: k_n_co55__he4_mn52   = 352
  integer, parameter :: k_he4_co55__p_ni58   = 353
  integer, parameter :: k_n_co56__p_fe56   = 354
  integer, parameter :: k_n_co56__he4_mn53   = 355
  integer, parameter :: k_n_co57__he4_mn54   = 356
  integer, parameter :: k_p_co57__he4_fe54   = 357
  integer, parameter :: k_n_ni56__p_co56   = 358
  integer, parameter :: k_n_ni56__he4_fe53   = 359
  integer, parameter :: k_n_ni57__p_co57   = 360
  integer, parameter :: k_n_ni57__he4_fe54   = 361
  integer, parameter :: k_n_ni58__he4_fe55   = 362
  integer, parameter :: k_t_t__n_n_he4   = 363
  integer, parameter :: k_t_he3__n_p_he4   = 364
  integer, parameter :: k_he3_he3__p_p_he4   = 365
  integer, parameter :: k_he4_he4_he4__c12   = 366
  integer, parameter :: k_n_p_p__p   = 367

  real(rt), allocatable, save :: bion(:), mion(:)

#ifdef AMREX_USE_CUDA
  attributes(managed) :: bion, mion
#endif

  !$acc declare create(bion, mion)

#ifdef REACT_SPARSE_JACOBIAN
  ! Shape of Jacobian in Compressed Sparse Row format
  integer, parameter   :: NETWORK_SPARSE_JAC_NNZ = 1127
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
      32, &
      33, &
      34, &
      35, &
      36, &
      37, &
      38, &
      39, &
      40, &
      42, &
      43, &
      45, &
      46, &
      47, &
      48, &
      49, &
      50, &
      51, &
      53, &
      54, &
      56, &
      57, &
      58, &
      59, &
      61, &
      62, &
      64, &
      65, &
      66, &
      67, &
      69, &
      70, &
      71, &
      73, &
      74, &
      75, &
      76, &
      78, &
      79, &
      80, &
      81, &
      82, &
      83, &
      84, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
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
      23, &
      24, &
      25, &
      26, &
      27, &
      28, &
      30, &
      31, &
      32, &
      33, &
      34, &
      35, &
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
      84, &
      1, &
      2, &
      3, &
      4, &
      5, &
      84, &
      1, &
      2, &
      3, &
      4, &
      5, &
      84, &
      1, &
      2, &
      3, &
      4, &
      5, &
      84, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
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
      78, &
      79, &
      80, &
      81, &
      82, &
      83, &
      84, &
      1, &
      2, &
      6, &
      7, &
      9, &
      10, &
      16, &
      21, &
      28, &
      84, &
      1, &
      2, &
      8, &
      11, &
      84, &
      1, &
      2, &
      6, &
      8, &
      9, &
      12, &
      84, &
      1, &
      2, &
      6, &
      7, &
      9, &
      10, &
      13, &
      15, &
      28, &
      84, &
      1, &
      2, &
      6, &
      10, &
      11, &
      14, &
      84, &
      1, &
      2, &
      6, &
      11, &
      12, &
      84, &
      1, &
      2, &
      6, &
      9, &
      12, &
      13, &
      15, &
      18, &
      84, &
      1, &
      2, &
      6, &
      13, &
      14, &
      84, &
      1, &
      2, &
      6, &
      13, &
      15, &
      84, &
      1, &
      2, &
      6, &
      7, &
      10, &
      11, &
      13, &
      14, &
      15, &
      16, &
      19, &
      21, &
      84, &
      1, &
      2, &
      6, &
      11, &
      12, &
      14, &
      16, &
      17, &
      20, &
      84, &
      1, &
      2, &
      6, &
      15, &
      17, &
      18, &
      84, &
      1, &
      2, &
      6, &
      7, &
      13, &
      14, &
      18, &
      19, &
      21, &
      25, &
      84, &
      1, &
      2, &
      6, &
      14, &
      19, &
      20, &
      84, &
      1, &
      2, &
      6, &
      15, &
      18, &
      19, &
      21, &
      84, &
      1, &
      2, &
      6, &
      7, &
      10, &
      16, &
      17, &
      19, &
      20, &
      21, &
      22, &
      26, &
      28, &
      84, &
      1, &
      2, &
      6, &
      17, &
      18, &
      20, &
      22, &
      23, &
      27, &
      84, &
      1, &
      2, &
      6, &
      19, &
      23, &
      24, &
      25, &
      84, &
      1, &
      2, &
      6, &
      18, &
      21, &
      23, &
      25, &
      84, &
      1, &
      2, &
      6, &
      7, &
      10, &
      19, &
      20, &
      24, &
      25, &
      26, &
      28, &
      84, &
      1, &
      2, &
      6, &
      20, &
      26, &
      27, &
      84, &
      1, &
      2, &
      6, &
      21, &
      25, &
      28, &
      84, &
      1, &
      2, &
      6, &
      7, &
      10, &
      16, &
      22, &
      23, &
      26, &
      27, &
      28, &
      29, &
      33, &
      35, &
      84, &
      1, &
      2, &
      6, &
      23, &
      24, &
      25, &
      27, &
      29, &
      30, &
      34, &
      36, &
      84, &
      1, &
      2, &
      6, &
      24, &
      26, &
      30, &
      31, &
      32, &
      37, &
      84, &
      1, &
      2, &
      6, &
      25, &
      26, &
      28, &
      30, &
      32, &
      84, &
      1, &
      2, &
      6, &
      7, &
      10, &
      16, &
      26, &
      27, &
      31, &
      32, &
      33, &
      35, &
      84, &
      1, &
      2, &
      6, &
      27, &
      33, &
      34, &
      39, &
      84, &
      1, &
      2, &
      6, &
      7, &
      10, &
      16, &
      28, &
      32, &
      35, &
      84, &
      1, &
      2, &
      6, &
      29, &
      33, &
      34, &
      35, &
      36, &
      39, &
      84, &
      1, &
      2, &
      6, &
      30, &
      32, &
      34, &
      36, &
      37, &
      40, &
      42, &
      84, &
      1, &
      2, &
      6, &
      31, &
      33, &
      37, &
      38, &
      84, &
      1, &
      2, &
      6, &
      33, &
      38, &
      39, &
      84, &
      1, &
      2, &
      6, &
      34, &
      39, &
      40, &
      42, &
      45, &
      84, &
      1, &
      2, &
      6, &
      38, &
      40, &
      41, &
      43, &
      46, &
      84, &
      1, &
      2, &
      6, &
      36, &
      39, &
      40, &
      42, &
      45, &
      84, &
      1, &
      2, &
      6, &
      37, &
      38, &
      40, &
      42, &
      43, &
      46, &
      48, &
      84, &
      1, &
      2, &
      6, &
      38, &
      39, &
      41, &
      43, &
      44, &
      47, &
      49, &
      84, &
      1, &
      2, &
      6, &
      39, &
      44, &
      45, &
      50, &
      84, &
      1, &
      2, &
      6, &
      40, &
      45, &
      46, &
      53, &
      84, &
      1, &
      2, &
      6, &
      41, &
      46, &
      47, &
      49, &
      84, &
      1, &
      2, &
      6, &
      42, &
      45, &
      46, &
      48, &
      53, &
      84, &
      1, &
      2, &
      6, &
      43, &
      46, &
      47, &
      48, &
      49, &
      54, &
      56, &
      84, &
      1, &
      2, &
      6, &
      44, &
      47, &
      49, &
      50, &
      55, &
      57, &
      84, &
      1, &
      2, &
      6, &
      46, &
      50, &
      51, &
      53, &
      84, &
      1, &
      2, &
      6, &
      47, &
      51, &
      52, &
      54, &
      59, &
      84, &
      1, &
      2, &
      6, &
      45, &
      50, &
      53, &
      84, &
      1, &
      2, &
      6, &
      46, &
      47, &
      51, &
      53, &
      54, &
      56, &
      61, &
      84, &
      1, &
      2, &
      6, &
      47, &
      52, &
      54, &
      55, &
      57, &
      62, &
      84, &
      1, &
      2, &
      6, &
      48, &
      53, &
      56, &
      84, &
      1, &
      2, &
      6, &
      49, &
      54, &
      56, &
      57, &
      64, &
      84, &
      1, &
      2, &
      6, &
      50, &
      51, &
      53, &
      55, &
      57, &
      58, &
      63, &
      65, &
      84, &
      1, &
      2, &
      6, &
      51, &
      54, &
      58, &
      59, &
      61, &
      66, &
      84, &
      1, &
      2, &
      6, &
      52, &
      55, &
      59, &
      60, &
      62, &
      67, &
      84, &
      1, &
      2, &
      6, &
      53, &
      56, &
      58, &
      61, &
      84, &
      1, &
      2, &
      6, &
      54, &
      57, &
      59, &
      61, &
      62, &
      64, &
      69, &
      84, &
      1, &
      2, &
      6, &
      55, &
      60, &
      62, &
      63, &
      65, &
      70, &
      84, &
      1, &
      2, &
      6, &
      56, &
      61, &
      64, &
      84, &
      1, &
      2, &
      6, &
      57, &
      62, &
      64, &
      65, &
      73, &
      84, &
      1, &
      2, &
      6, &
      58, &
      61, &
      63, &
      65, &
      66, &
      71, &
      74, &
      84, &
      1, &
      2, &
      6, &
      59, &
      62, &
      66, &
      67, &
      69, &
      72, &
      75, &
      84, &
      1, &
      2, &
      6, &
      60, &
      63, &
      67, &
      68, &
      70, &
      76, &
      84, &
      1, &
      2, &
      6, &
      61, &
      64, &
      66, &
      69, &
      84, &
      1, &
      2, &
      6, &
      62, &
      65, &
      67, &
      69, &
      70, &
      73, &
      78, &
      84, &
      1, &
      2, &
      6, &
      63, &
      68, &
      70, &
      71, &
      74, &
      79, &
      84, &
      1, &
      2, &
      71, &
      72, &
      75, &
      80, &
      84, &
      1, &
      2, &
      6, &
      64, &
      69, &
      73, &
      84, &
      1, &
      2, &
      6, &
      65, &
      70, &
      73, &
      74, &
      81, &
      84, &
      1, &
      2, &
      6, &
      66, &
      69, &
      71, &
      72, &
      74, &
      75, &
      80, &
      82, &
      84, &
      1, &
      2, &
      6, &
      67, &
      70, &
      72, &
      75, &
      76, &
      78, &
      83, &
      84, &
      1, &
      2, &
      6, &
      68, &
      71, &
      76, &
      77, &
      79, &
      84, &
      1, &
      2, &
      6, &
      69, &
      73, &
      75, &
      78, &
      84, &
      1, &
      2, &
      6, &
      70, &
      74, &
      76, &
      78, &
      79, &
      81, &
      84, &
      1, &
      2, &
      6, &
      71, &
      77, &
      79, &
      80, &
      82, &
      84, &
      1, &
      2, &
      6, &
      73, &
      78, &
      81, &
      84, &
      1, &
      2, &
      6, &
      74, &
      79, &
      81, &
      82, &
      84, &
      1, &
      2, &
      6, &
      75, &
      78, &
      80, &
      82, &
      83, &
      84, &
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
      85  ]

    csr_jac_row_count = [ &
      1, &
      74, &
      150, &
      156, &
      162, &
      168, &
      250, &
      260, &
      265, &
      272, &
      282, &
      289, &
      295, &
      304, &
      310, &
      316, &
      329, &
      339, &
      346, &
      357, &
      364, &
      372, &
      386, &
      396, &
      404, &
      412, &
      424, &
      431, &
      438, &
      453, &
      465, &
      475, &
      484, &
      497, &
      505, &
      515, &
      525, &
      536, &
      544, &
      551, &
      560, &
      569, &
      578, &
      589, &
      600, &
      608, &
      616, &
      624, &
      633, &
      644, &
      654, &
      662, &
      671, &
      678, &
      689, &
      699, &
      706, &
      715, &
      727, &
      737, &
      747, &
      755, &
      766, &
      776, &
      783, &
      792, &
      803, &
      814, &
      824, &
      832, &
      843, &
      853, &
      860, &
      867, &
      876, &
      888, &
      899, &
      908, &
      916, &
      926, &
      935, &
      942, &
      950, &
      959, &
      1043, &
      1128  ]
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
