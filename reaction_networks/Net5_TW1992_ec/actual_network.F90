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

  integer, parameter :: nrates = 1285


  ! For each rate, we need: rate, drate/dT, screening, dscreening/dT
  integer, parameter :: num_rate_groups = 4

  ! Number of reaclib rates
  integer, parameter :: nrat_reaclib = 1281
  integer, parameter :: number_reaclib_sets = 1643

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
  integer, parameter :: jc13   = 8
  integer, parameter :: jc14   = 9
  integer, parameter :: jn13   = 10
  integer, parameter :: jn14   = 11
  integer, parameter :: jn15   = 12
  integer, parameter :: jo16   = 13
  integer, parameter :: jo17   = 14
  integer, parameter :: jo18   = 15
  integer, parameter :: jo19   = 16
  integer, parameter :: jo20   = 17
  integer, parameter :: jf18   = 18
  integer, parameter :: jf19   = 19
  integer, parameter :: jf20   = 20
  integer, parameter :: jf21   = 21
  integer, parameter :: jne19   = 22
  integer, parameter :: jne20   = 23
  integer, parameter :: jne21   = 24
  integer, parameter :: jne22   = 25
  integer, parameter :: jne23   = 26
  integer, parameter :: jne24   = 27
  integer, parameter :: jna20   = 28
  integer, parameter :: jna21   = 29
  integer, parameter :: jna22   = 30
  integer, parameter :: jna23   = 31
  integer, parameter :: jna24   = 32
  integer, parameter :: jmg22   = 33
  integer, parameter :: jmg23   = 34
  integer, parameter :: jmg24   = 35
  integer, parameter :: jmg25   = 36
  integer, parameter :: jmg26   = 37
  integer, parameter :: jmg27   = 38
  integer, parameter :: jal25   = 39
  integer, parameter :: jal26   = 40
  integer, parameter :: jal27   = 41
  integer, parameter :: jal28   = 42
  integer, parameter :: jal29   = 43
  integer, parameter :: jsi27   = 44
  integer, parameter :: jsi28   = 45
  integer, parameter :: jsi29   = 46
  integer, parameter :: jsi30   = 47
  integer, parameter :: jsi31   = 48
  integer, parameter :: jsi32   = 49
  integer, parameter :: jp29   = 50
  integer, parameter :: jp30   = 51
  integer, parameter :: jp31   = 52
  integer, parameter :: jp32   = 53
  integer, parameter :: jp33   = 54
  integer, parameter :: js30   = 55
  integer, parameter :: js31   = 56
  integer, parameter :: js32   = 57
  integer, parameter :: js33   = 58
  integer, parameter :: js34   = 59
  integer, parameter :: js35   = 60
  integer, parameter :: jcl33   = 61
  integer, parameter :: jcl34   = 62
  integer, parameter :: jcl35   = 63
  integer, parameter :: jcl36   = 64
  integer, parameter :: jcl37   = 65
  integer, parameter :: jar35   = 66
  integer, parameter :: jar36   = 67
  integer, parameter :: jar37   = 68
  integer, parameter :: jar38   = 69
  integer, parameter :: jar39   = 70
  integer, parameter :: jk37   = 71
  integer, parameter :: jk38   = 72
  integer, parameter :: jk39   = 73
  integer, parameter :: jk40   = 74
  integer, parameter :: jk41   = 75
  integer, parameter :: jk42   = 76
  integer, parameter :: jca39   = 77
  integer, parameter :: jca40   = 78
  integer, parameter :: jca41   = 79
  integer, parameter :: jca42   = 80
  integer, parameter :: jca43   = 81
  integer, parameter :: jca44   = 82
  integer, parameter :: jsc42   = 83
  integer, parameter :: jsc43   = 84
  integer, parameter :: jsc44   = 85
  integer, parameter :: jsc45   = 86
  integer, parameter :: jsc46   = 87
  integer, parameter :: jti43   = 88
  integer, parameter :: jti44   = 89
  integer, parameter :: jti45   = 90
  integer, parameter :: jti46   = 91
  integer, parameter :: jti47   = 92
  integer, parameter :: jti48   = 93
  integer, parameter :: jti49   = 94
  integer, parameter :: jv46   = 95
  integer, parameter :: jv47   = 96
  integer, parameter :: jv48   = 97
  integer, parameter :: jv49   = 98
  integer, parameter :: jv50   = 99
  integer, parameter :: jv51   = 100
  integer, parameter :: jcr47   = 101
  integer, parameter :: jcr48   = 102
  integer, parameter :: jcr49   = 103
  integer, parameter :: jcr50   = 104
  integer, parameter :: jcr51   = 105
  integer, parameter :: jcr52   = 106
  integer, parameter :: jmn49   = 107
  integer, parameter :: jmn50   = 108
  integer, parameter :: jmn51   = 109
  integer, parameter :: jmn52   = 110
  integer, parameter :: jmn53   = 111
  integer, parameter :: jmn54   = 112
  integer, parameter :: jmn55   = 113
  integer, parameter :: jfe51   = 114
  integer, parameter :: jfe52   = 115
  integer, parameter :: jfe53   = 116
  integer, parameter :: jfe54   = 117
  integer, parameter :: jfe55   = 118
  integer, parameter :: jfe56   = 119
  integer, parameter :: jco53   = 120
  integer, parameter :: jco54   = 121
  integer, parameter :: jco55   = 122
  integer, parameter :: jco56   = 123
  integer, parameter :: jco57   = 124
  integer, parameter :: jco58   = 125
  integer, parameter :: jni54   = 126
  integer, parameter :: jni55   = 127
  integer, parameter :: jni56   = 128
  integer, parameter :: jni57   = 129
  integer, parameter :: jni58   = 130
  integer, parameter :: jni59   = 131
  integer, parameter :: jni60   = 132

  ! Reactions
  integer, parameter :: k_n__p__weak__wc12   = 1
  integer, parameter :: k_t__he3__weak__wc12   = 2
  integer, parameter :: k_he3__t__weak__electron_capture   = 3
  integer, parameter :: k_c14__n14__weak__wc12   = 4
  integer, parameter :: k_n13__c13__weak__wc12   = 5
  integer, parameter :: k_o19__f19__weak__wc12   = 6
  integer, parameter :: k_f18__o18__weak__wc12   = 7
  integer, parameter :: k_f21__ne21__weak__wc12   = 8
  integer, parameter :: k_ne19__f19__weak__wc12   = 9
  integer, parameter :: k_ne23__na23__weak__wc12   = 10
  integer, parameter :: k_ne24__na24__weak__wc12   = 11
  integer, parameter :: k_na20__ne20__weak__wc12   = 12
  integer, parameter :: k_na21__ne21__weak__wc12   = 13
  integer, parameter :: k_na22__ne22__weak__wc12   = 14
  integer, parameter :: k_na24__mg24__weak__wc12   = 15
  integer, parameter :: k_mg22__na22__weak__wc12   = 16
  integer, parameter :: k_mg23__na23__weak__wc12   = 17
  integer, parameter :: k_mg27__al27__weak__wc12   = 18
  integer, parameter :: k_al25__mg25__weak__wc12   = 19
  integer, parameter :: k_al26__mg26__weak__wc12   = 20
  integer, parameter :: k_al28__si28__weak__wc12   = 21
  integer, parameter :: k_al29__si29__weak__wc12   = 22
  integer, parameter :: k_si27__al27__weak__wc12   = 23
  integer, parameter :: k_si31__p31__weak__wc12   = 24
  integer, parameter :: k_si32__p32__weak__wc12   = 25
  integer, parameter :: k_p29__si29__weak__wc12   = 26
  integer, parameter :: k_p30__si30__weak__wc12   = 27
  integer, parameter :: k_p32__s32__weak__wc12   = 28
  integer, parameter :: k_p33__s33__weak__wc12   = 29
  integer, parameter :: k_s30__p30__weak__wc12   = 30
  integer, parameter :: k_s31__p31__weak__wc12   = 31
  integer, parameter :: k_s35__cl35__weak__wc12   = 32
  integer, parameter :: k_cl33__s33__weak__wc12   = 33
  integer, parameter :: k_cl34__s34__weak__wc12   = 34
  integer, parameter :: k_cl36__ar36__weak__wc12   = 35
  integer, parameter :: k_ar35__cl35__weak__wc12   = 36
  integer, parameter :: k_ar37__cl37__weak__wc12   = 37
  integer, parameter :: k_ar39__k39__weak__wc12   = 38
  integer, parameter :: k_k37__ar37__weak__wc12   = 39
  integer, parameter :: k_k38__ar38__weak__wc12   = 40
  integer, parameter :: k_k40__ca40__weak__wc12   = 41
  integer, parameter :: k_k42__ca42__weak__wc12   = 42
  integer, parameter :: k_ca39__k39__weak__wc12   = 43
  integer, parameter :: k_ca41__k41__weak__wc12   = 44
  integer, parameter :: k_sc42__ca42__weak__wc12   = 45
  integer, parameter :: k_sc43__ca43__weak__wc12   = 46
  integer, parameter :: k_sc44__ca44__weak__wc12   = 47
  integer, parameter :: k_sc46__ti46__weak__wc12   = 48
  integer, parameter :: k_ti43__sc43__weak__wc12   = 49
  integer, parameter :: k_ti44__sc44__weak__wc12   = 50
  integer, parameter :: k_ti45__sc45__weak__wc12   = 51
  integer, parameter :: k_v46__ti46__weak__wc12   = 52
  integer, parameter :: k_v47__ti47__weak__wc12   = 53
  integer, parameter :: k_v48__ti48__weak__wc12   = 54
  integer, parameter :: k_v49__ti49__weak__wc12   = 55
  integer, parameter :: k_v50__cr50__weak__wc12   = 56
  integer, parameter :: k_cr47__v47__weak__wc12   = 57
  integer, parameter :: k_cr48__v48__weak__wc12   = 58
  integer, parameter :: k_cr49__v49__weak__wc12   = 59
  integer, parameter :: k_cr51__v51__weak__wc12   = 60
  integer, parameter :: k_mn49__cr49__weak__wc12   = 61
  integer, parameter :: k_mn50__cr50__weak__wc12   = 62
  integer, parameter :: k_mn51__cr51__weak__wc12   = 63
  integer, parameter :: k_mn52__cr52__weak__wc12   = 64
  integer, parameter :: k_mn54__fe54__weak__wc12   = 65
  integer, parameter :: k_fe51__mn51__weak__wc12   = 66
  integer, parameter :: k_fe52__mn52__weak__wc12   = 67
  integer, parameter :: k_fe53__mn53__weak__wc12   = 68
  integer, parameter :: k_fe55__mn55__weak__wc12   = 69
  integer, parameter :: k_co53__fe53__weak__wc12   = 70
  integer, parameter :: k_co54__fe54__weak__wc12   = 71
  integer, parameter :: k_co55__fe55__weak__wc12   = 72
  integer, parameter :: k_co56__fe56__weak__wc12   = 73
  integer, parameter :: k_co58__ni58__weak__mo03   = 74
  integer, parameter :: k_ni54__co54__weak__wc12   = 75
  integer, parameter :: k_ni55__co55__weak__wc12   = 76
  integer, parameter :: k_ni56__co56__weak__wc12   = 77
  integer, parameter :: k_ni57__co57__weak__wc12   = 78
  integer, parameter :: k_d__n_p   = 79
  integer, parameter :: k_t__n_d   = 80
  integer, parameter :: k_he3__p_d   = 81
  integer, parameter :: k_he4__n_he3   = 82
  integer, parameter :: k_he4__p_t   = 83
  integer, parameter :: k_he4__d_d   = 84
  integer, parameter :: k_c13__n_c12   = 85
  integer, parameter :: k_c14__n_c13   = 86
  integer, parameter :: k_n13__p_c12   = 87
  integer, parameter :: k_n14__n_n13   = 88
  integer, parameter :: k_n14__p_c13   = 89
  integer, parameter :: k_n15__n_n14   = 90
  integer, parameter :: k_n15__p_c14   = 91
  integer, parameter :: k_o16__p_n15   = 92
  integer, parameter :: k_o16__he4_c12   = 93
  integer, parameter :: k_o17__n_o16   = 94
  integer, parameter :: k_o18__n_o17   = 95
  integer, parameter :: k_o18__he4_c14   = 96
  integer, parameter :: k_o19__n_o18   = 97
  integer, parameter :: k_f18__p_o17   = 98
  integer, parameter :: k_f18__he4_n14   = 99
  integer, parameter :: k_f19__n_f18   = 100
  integer, parameter :: k_f19__p_o18   = 101
  integer, parameter :: k_f19__he4_n15   = 102
  integer, parameter :: k_f20__n_f19   = 103
  integer, parameter :: k_f20__p_o19   = 104
  integer, parameter :: k_f21__n_f20   = 105
  integer, parameter :: k_ne19__p_f18   = 106
  integer, parameter :: k_ne20__n_ne19   = 107
  integer, parameter :: k_ne20__p_f19   = 108
  integer, parameter :: k_ne20__he4_o16   = 109
  integer, parameter :: k_ne21__n_ne20   = 110
  integer, parameter :: k_ne21__p_f20   = 111
  integer, parameter :: k_ne21__he4_o17   = 112
  integer, parameter :: k_ne22__n_ne21   = 113
  integer, parameter :: k_ne22__p_f21   = 114
  integer, parameter :: k_ne22__he4_o18   = 115
  integer, parameter :: k_ne23__n_ne22   = 116
  integer, parameter :: k_ne23__he4_o19   = 117
  integer, parameter :: k_ne24__n_ne23   = 118
  integer, parameter :: k_na20__p_ne19   = 119
  integer, parameter :: k_na20__he4_o16__weak__wc12   = 120
  integer, parameter :: k_na21__n_na20   = 121
  integer, parameter :: k_na21__p_ne20   = 122
  integer, parameter :: k_na22__n_na21   = 123
  integer, parameter :: k_na22__p_ne21   = 124
  integer, parameter :: k_na22__he4_f18   = 125
  integer, parameter :: k_na23__n_na22   = 126
  integer, parameter :: k_na23__p_ne22   = 127
  integer, parameter :: k_na23__he4_f19   = 128
  integer, parameter :: k_na24__n_na23   = 129
  integer, parameter :: k_na24__p_ne23   = 130
  integer, parameter :: k_na24__he4_f20   = 131
  integer, parameter :: k_mg22__p_na21   = 132
  integer, parameter :: k_mg23__n_mg22   = 133
  integer, parameter :: k_mg23__p_na22   = 134
  integer, parameter :: k_mg23__he4_ne19   = 135
  integer, parameter :: k_mg24__n_mg23   = 136
  integer, parameter :: k_mg24__p_na23   = 137
  integer, parameter :: k_mg24__he4_ne20   = 138
  integer, parameter :: k_mg25__n_mg24   = 139
  integer, parameter :: k_mg25__p_na24   = 140
  integer, parameter :: k_mg25__he4_ne21   = 141
  integer, parameter :: k_mg26__n_mg25   = 142
  integer, parameter :: k_mg26__he4_ne22   = 143
  integer, parameter :: k_mg27__n_mg26   = 144
  integer, parameter :: k_mg27__he4_ne23   = 145
  integer, parameter :: k_al25__p_mg24   = 146
  integer, parameter :: k_al25__he4_na21   = 147
  integer, parameter :: k_al26__n_al25   = 148
  integer, parameter :: k_al26__p_mg25   = 149
  integer, parameter :: k_al26__he4_na22   = 150
  integer, parameter :: k_al27__n_al26   = 151
  integer, parameter :: k_al27__p_mg26   = 152
  integer, parameter :: k_al27__he4_na23   = 153
  integer, parameter :: k_al28__n_al27   = 154
  integer, parameter :: k_al28__p_mg27   = 155
  integer, parameter :: k_al28__he4_na24   = 156
  integer, parameter :: k_al29__n_al28   = 157
  integer, parameter :: k_si27__p_al26   = 158
  integer, parameter :: k_si27__he4_mg23   = 159
  integer, parameter :: k_si28__n_si27   = 160
  integer, parameter :: k_si28__p_al27   = 161
  integer, parameter :: k_si28__he4_mg24   = 162
  integer, parameter :: k_si29__n_si28   = 163
  integer, parameter :: k_si29__p_al28   = 164
  integer, parameter :: k_si29__he4_mg25   = 165
  integer, parameter :: k_si30__n_si29   = 166
  integer, parameter :: k_si30__p_al29   = 167
  integer, parameter :: k_si30__he4_mg26   = 168
  integer, parameter :: k_si31__n_si30   = 169
  integer, parameter :: k_si31__he4_mg27   = 170
  integer, parameter :: k_si32__n_si31   = 171
  integer, parameter :: k_p29__p_si28   = 172
  integer, parameter :: k_p29__he4_al25   = 173
  integer, parameter :: k_p30__n_p29   = 174
  integer, parameter :: k_p30__p_si29   = 175
  integer, parameter :: k_p30__he4_al26   = 176
  integer, parameter :: k_p31__n_p30   = 177
  integer, parameter :: k_p31__p_si30   = 178
  integer, parameter :: k_p31__he4_al27   = 179
  integer, parameter :: k_p32__n_p31   = 180
  integer, parameter :: k_p32__p_si31   = 181
  integer, parameter :: k_p32__he4_al28   = 182
  integer, parameter :: k_p33__n_p32   = 183
  integer, parameter :: k_p33__p_si32   = 184
  integer, parameter :: k_p33__he4_al29   = 185
  integer, parameter :: k_s30__p_p29   = 186
  integer, parameter :: k_s31__n_s30   = 187
  integer, parameter :: k_s31__p_p30   = 188
  integer, parameter :: k_s31__he4_si27   = 189
  integer, parameter :: k_s32__n_s31   = 190
  integer, parameter :: k_s32__p_p31   = 191
  integer, parameter :: k_s32__he4_si28   = 192
  integer, parameter :: k_s33__n_s32   = 193
  integer, parameter :: k_s33__p_p32   = 194
  integer, parameter :: k_s33__he4_si29   = 195
  integer, parameter :: k_s34__n_s33   = 196
  integer, parameter :: k_s34__p_p33   = 197
  integer, parameter :: k_s34__he4_si30   = 198
  integer, parameter :: k_s35__n_s34   = 199
  integer, parameter :: k_s35__he4_si31   = 200
  integer, parameter :: k_cl33__p_s32   = 201
  integer, parameter :: k_cl33__he4_p29   = 202
  integer, parameter :: k_cl34__n_cl33   = 203
  integer, parameter :: k_cl34__p_s33   = 204
  integer, parameter :: k_cl34__he4_p30   = 205
  integer, parameter :: k_cl35__n_cl34   = 206
  integer, parameter :: k_cl35__p_s34   = 207
  integer, parameter :: k_cl35__he4_p31   = 208
  integer, parameter :: k_cl36__n_cl35   = 209
  integer, parameter :: k_cl36__p_s35   = 210
  integer, parameter :: k_cl36__he4_p32   = 211
  integer, parameter :: k_cl37__n_cl36   = 212
  integer, parameter :: k_cl37__he4_p33   = 213
  integer, parameter :: k_ar35__p_cl34   = 214
  integer, parameter :: k_ar35__he4_s31   = 215
  integer, parameter :: k_ar36__n_ar35   = 216
  integer, parameter :: k_ar36__p_cl35   = 217
  integer, parameter :: k_ar36__he4_s32   = 218
  integer, parameter :: k_ar37__n_ar36   = 219
  integer, parameter :: k_ar37__p_cl36   = 220
  integer, parameter :: k_ar37__he4_s33   = 221
  integer, parameter :: k_ar38__n_ar37   = 222
  integer, parameter :: k_ar38__p_cl37   = 223
  integer, parameter :: k_ar38__he4_s34   = 224
  integer, parameter :: k_ar39__n_ar38   = 225
  integer, parameter :: k_ar39__he4_s35   = 226
  integer, parameter :: k_k37__p_ar36   = 227
  integer, parameter :: k_k37__he4_cl33   = 228
  integer, parameter :: k_k38__n_k37   = 229
  integer, parameter :: k_k38__p_ar37   = 230
  integer, parameter :: k_k38__he4_cl34   = 231
  integer, parameter :: k_k39__n_k38   = 232
  integer, parameter :: k_k39__p_ar38   = 233
  integer, parameter :: k_k39__he4_cl35   = 234
  integer, parameter :: k_k40__n_k39   = 235
  integer, parameter :: k_k40__p_ar39   = 236
  integer, parameter :: k_k40__he4_cl36   = 237
  integer, parameter :: k_k41__n_k40   = 238
  integer, parameter :: k_k41__he4_cl37   = 239
  integer, parameter :: k_k42__n_k41   = 240
  integer, parameter :: k_ca39__p_k38   = 241
  integer, parameter :: k_ca39__he4_ar35   = 242
  integer, parameter :: k_ca40__n_ca39   = 243
  integer, parameter :: k_ca40__p_k39   = 244
  integer, parameter :: k_ca40__he4_ar36   = 245
  integer, parameter :: k_ca41__n_ca40   = 246
  integer, parameter :: k_ca41__p_k40   = 247
  integer, parameter :: k_ca41__he4_ar37   = 248
  integer, parameter :: k_ca42__n_ca41   = 249
  integer, parameter :: k_ca42__p_k41   = 250
  integer, parameter :: k_ca42__he4_ar38   = 251
  integer, parameter :: k_ca43__n_ca42   = 252
  integer, parameter :: k_ca43__p_k42   = 253
  integer, parameter :: k_ca43__he4_ar39   = 254
  integer, parameter :: k_ca44__n_ca43   = 255
  integer, parameter :: k_sc42__p_ca41   = 256
  integer, parameter :: k_sc42__he4_k38   = 257
  integer, parameter :: k_sc43__n_sc42   = 258
  integer, parameter :: k_sc43__p_ca42   = 259
  integer, parameter :: k_sc43__he4_k39   = 260
  integer, parameter :: k_sc44__n_sc43   = 261
  integer, parameter :: k_sc44__p_ca43   = 262
  integer, parameter :: k_sc44__he4_k40   = 263
  integer, parameter :: k_sc45__n_sc44   = 264
  integer, parameter :: k_sc45__p_ca44   = 265
  integer, parameter :: k_sc45__he4_k41   = 266
  integer, parameter :: k_sc46__n_sc45   = 267
  integer, parameter :: k_sc46__he4_k42   = 268
  integer, parameter :: k_ti43__p_sc42   = 269
  integer, parameter :: k_ti43__he4_ca39   = 270
  integer, parameter :: k_ti44__n_ti43   = 271
  integer, parameter :: k_ti44__p_sc43   = 272
  integer, parameter :: k_ti44__he4_ca40   = 273
  integer, parameter :: k_ti45__n_ti44   = 274
  integer, parameter :: k_ti45__p_sc44   = 275
  integer, parameter :: k_ti45__he4_ca41   = 276
  integer, parameter :: k_ti46__n_ti45   = 277
  integer, parameter :: k_ti46__p_sc45   = 278
  integer, parameter :: k_ti46__he4_ca42   = 279
  integer, parameter :: k_ti47__n_ti46   = 280
  integer, parameter :: k_ti47__p_sc46   = 281
  integer, parameter :: k_ti47__he4_ca43   = 282
  integer, parameter :: k_ti48__n_ti47   = 283
  integer, parameter :: k_ti48__he4_ca44   = 284
  integer, parameter :: k_ti49__n_ti48   = 285
  integer, parameter :: k_v46__p_ti45   = 286
  integer, parameter :: k_v46__he4_sc42   = 287
  integer, parameter :: k_v47__n_v46   = 288
  integer, parameter :: k_v47__p_ti46   = 289
  integer, parameter :: k_v47__he4_sc43   = 290
  integer, parameter :: k_v48__n_v47   = 291
  integer, parameter :: k_v48__p_ti47   = 292
  integer, parameter :: k_v48__he4_sc44   = 293
  integer, parameter :: k_v49__n_v48   = 294
  integer, parameter :: k_v49__p_ti48   = 295
  integer, parameter :: k_v49__he4_sc45   = 296
  integer, parameter :: k_v50__n_v49   = 297
  integer, parameter :: k_v50__p_ti49   = 298
  integer, parameter :: k_v50__he4_sc46   = 299
  integer, parameter :: k_v51__n_v50   = 300
  integer, parameter :: k_cr47__p_v46   = 301
  integer, parameter :: k_cr47__he4_ti43   = 302
  integer, parameter :: k_cr48__n_cr47   = 303
  integer, parameter :: k_cr48__p_v47   = 304
  integer, parameter :: k_cr48__he4_ti44   = 305
  integer, parameter :: k_cr49__n_cr48   = 306
  integer, parameter :: k_cr49__p_v48   = 307
  integer, parameter :: k_cr49__he4_ti45   = 308
  integer, parameter :: k_cr50__n_cr49   = 309
  integer, parameter :: k_cr50__p_v49   = 310
  integer, parameter :: k_cr50__he4_ti46   = 311
  integer, parameter :: k_cr51__n_cr50   = 312
  integer, parameter :: k_cr51__p_v50   = 313
  integer, parameter :: k_cr51__he4_ti47   = 314
  integer, parameter :: k_cr52__n_cr51   = 315
  integer, parameter :: k_cr52__p_v51   = 316
  integer, parameter :: k_cr52__he4_ti48   = 317
  integer, parameter :: k_mn49__p_cr48   = 318
  integer, parameter :: k_mn50__n_mn49   = 319
  integer, parameter :: k_mn50__p_cr49   = 320
  integer, parameter :: k_mn50__he4_v46   = 321
  integer, parameter :: k_mn51__n_mn50   = 322
  integer, parameter :: k_mn51__p_cr50   = 323
  integer, parameter :: k_mn51__he4_v47   = 324
  integer, parameter :: k_mn52__n_mn51   = 325
  integer, parameter :: k_mn52__p_cr51   = 326
  integer, parameter :: k_mn52__he4_v48   = 327
  integer, parameter :: k_mn53__n_mn52   = 328
  integer, parameter :: k_mn53__p_cr52   = 329
  integer, parameter :: k_mn53__he4_v49   = 330
  integer, parameter :: k_mn54__n_mn53   = 331
  integer, parameter :: k_mn54__he4_v50   = 332
  integer, parameter :: k_mn55__n_mn54   = 333
  integer, parameter :: k_mn55__he4_v51   = 334
  integer, parameter :: k_fe51__p_mn50   = 335
  integer, parameter :: k_fe51__he4_cr47   = 336
  integer, parameter :: k_fe52__n_fe51   = 337
  integer, parameter :: k_fe52__p_mn51   = 338
  integer, parameter :: k_fe52__he4_cr48   = 339
  integer, parameter :: k_fe53__n_fe52   = 340
  integer, parameter :: k_fe53__p_mn52   = 341
  integer, parameter :: k_fe53__he4_cr49   = 342
  integer, parameter :: k_fe54__n_fe53   = 343
  integer, parameter :: k_fe54__p_mn53   = 344
  integer, parameter :: k_fe54__he4_cr50   = 345
  integer, parameter :: k_fe55__n_fe54   = 346
  integer, parameter :: k_fe55__p_mn54   = 347
  integer, parameter :: k_fe55__he4_cr51   = 348
  integer, parameter :: k_fe56__n_fe55   = 349
  integer, parameter :: k_fe56__p_mn55   = 350
  integer, parameter :: k_fe56__he4_cr52   = 351
  integer, parameter :: k_co53__p_fe52   = 352
  integer, parameter :: k_co53__he4_mn49   = 353
  integer, parameter :: k_co54__n_co53   = 354
  integer, parameter :: k_co54__p_fe53   = 355
  integer, parameter :: k_co54__he4_mn50   = 356
  integer, parameter :: k_co55__n_co54   = 357
  integer, parameter :: k_co55__p_fe54   = 358
  integer, parameter :: k_co55__he4_mn51   = 359
  integer, parameter :: k_co56__n_co55   = 360
  integer, parameter :: k_co56__p_fe55   = 361
  integer, parameter :: k_co56__he4_mn52   = 362
  integer, parameter :: k_co57__n_co56   = 363
  integer, parameter :: k_co57__p_fe56   = 364
  integer, parameter :: k_co57__he4_mn53   = 365
  integer, parameter :: k_co58__n_co57   = 366
  integer, parameter :: k_co58__he4_mn54   = 367
  integer, parameter :: k_ni54__p_co53   = 368
  integer, parameter :: k_ni55__n_ni54   = 369
  integer, parameter :: k_ni55__p_co54   = 370
  integer, parameter :: k_ni55__he4_fe51   = 371
  integer, parameter :: k_ni56__n_ni55   = 372
  integer, parameter :: k_ni56__p_co55   = 373
  integer, parameter :: k_ni56__he4_fe52   = 374
  integer, parameter :: k_ni57__n_ni56   = 375
  integer, parameter :: k_ni57__p_co56   = 376
  integer, parameter :: k_ni57__he4_fe53   = 377
  integer, parameter :: k_ni58__n_ni57   = 378
  integer, parameter :: k_ni58__p_co57   = 379
  integer, parameter :: k_ni58__he4_fe54   = 380
  integer, parameter :: k_ni59__n_ni58   = 381
  integer, parameter :: k_ni59__p_co58   = 382
  integer, parameter :: k_ni59__he4_fe55   = 383
  integer, parameter :: k_ni60__n_ni59   = 384
  integer, parameter :: k_ni60__he4_fe56   = 385
  integer, parameter :: k_c12__he4_he4_he4   = 386
  integer, parameter :: k_n_p__d   = 387
  integer, parameter :: k_p_p__d__weak__bet_pos_   = 388
  integer, parameter :: k_p_p__d__weak__electron_capture   = 389
  integer, parameter :: k_n_d__t   = 390
  integer, parameter :: k_p_d__he3   = 391
  integer, parameter :: k_d_d__he4   = 392
  integer, parameter :: k_p_t__he4   = 393
  integer, parameter :: k_n_he3__he4   = 394
  integer, parameter :: k_p_he3__he4__weak__bet_pos_   = 395
  integer, parameter :: k_n_c12__c13   = 396
  integer, parameter :: k_p_c12__n13   = 397
  integer, parameter :: k_he4_c12__o16   = 398
  integer, parameter :: k_n_c13__c14   = 399
  integer, parameter :: k_p_c13__n14   = 400
  integer, parameter :: k_p_c14__n15   = 401
  integer, parameter :: k_he4_c14__o18   = 402
  integer, parameter :: k_n_n13__n14   = 403
  integer, parameter :: k_n_n14__n15   = 404
  integer, parameter :: k_he4_n14__f18   = 405
  integer, parameter :: k_p_n15__o16   = 406
  integer, parameter :: k_he4_n15__f19   = 407
  integer, parameter :: k_n_o16__o17   = 408
  integer, parameter :: k_he4_o16__ne20   = 409
  integer, parameter :: k_n_o17__o18   = 410
  integer, parameter :: k_p_o17__f18   = 411
  integer, parameter :: k_he4_o17__ne21   = 412
  integer, parameter :: k_n_o18__o19   = 413
  integer, parameter :: k_p_o18__f19   = 414
  integer, parameter :: k_he4_o18__ne22   = 415
  integer, parameter :: k_p_o19__f20   = 416
  integer, parameter :: k_he4_o19__ne23   = 417
  integer, parameter :: k_n_f18__f19   = 418
  integer, parameter :: k_p_f18__ne19   = 419
  integer, parameter :: k_he4_f18__na22   = 420
  integer, parameter :: k_n_f19__f20   = 421
  integer, parameter :: k_p_f19__ne20   = 422
  integer, parameter :: k_he4_f19__na23   = 423
  integer, parameter :: k_n_f20__f21   = 424
  integer, parameter :: k_p_f20__ne21   = 425
  integer, parameter :: k_he4_f20__na24   = 426
  integer, parameter :: k_p_f21__ne22   = 427
  integer, parameter :: k_n_ne19__ne20   = 428
  integer, parameter :: k_p_ne19__na20   = 429
  integer, parameter :: k_he4_ne19__mg23   = 430
  integer, parameter :: k_n_ne20__ne21   = 431
  integer, parameter :: k_p_ne20__na21   = 432
  integer, parameter :: k_he4_ne20__mg24   = 433
  integer, parameter :: k_n_ne21__ne22   = 434
  integer, parameter :: k_p_ne21__na22   = 435
  integer, parameter :: k_he4_ne21__mg25   = 436
  integer, parameter :: k_n_ne22__ne23   = 437
  integer, parameter :: k_p_ne22__na23   = 438
  integer, parameter :: k_he4_ne22__mg26   = 439
  integer, parameter :: k_n_ne23__ne24   = 440
  integer, parameter :: k_p_ne23__na24   = 441
  integer, parameter :: k_he4_ne23__mg27   = 442
  integer, parameter :: k_n_na20__na21   = 443
  integer, parameter :: k_n_na21__na22   = 444
  integer, parameter :: k_p_na21__mg22   = 445
  integer, parameter :: k_he4_na21__al25   = 446
  integer, parameter :: k_n_na22__na23   = 447
  integer, parameter :: k_p_na22__mg23   = 448
  integer, parameter :: k_he4_na22__al26   = 449
  integer, parameter :: k_n_na23__na24   = 450
  integer, parameter :: k_p_na23__mg24   = 451
  integer, parameter :: k_he4_na23__al27   = 452
  integer, parameter :: k_p_na24__mg25   = 453
  integer, parameter :: k_he4_na24__al28   = 454
  integer, parameter :: k_n_mg22__mg23   = 455
  integer, parameter :: k_n_mg23__mg24   = 456
  integer, parameter :: k_he4_mg23__si27   = 457
  integer, parameter :: k_n_mg24__mg25   = 458
  integer, parameter :: k_p_mg24__al25   = 459
  integer, parameter :: k_he4_mg24__si28   = 460
  integer, parameter :: k_n_mg25__mg26   = 461
  integer, parameter :: k_p_mg25__al26   = 462
  integer, parameter :: k_he4_mg25__si29   = 463
  integer, parameter :: k_n_mg26__mg27   = 464
  integer, parameter :: k_p_mg26__al27   = 465
  integer, parameter :: k_he4_mg26__si30   = 466
  integer, parameter :: k_p_mg27__al28   = 467
  integer, parameter :: k_he4_mg27__si31   = 468
  integer, parameter :: k_n_al25__al26   = 469
  integer, parameter :: k_he4_al25__p29   = 470
  integer, parameter :: k_n_al26__al27   = 471
  integer, parameter :: k_p_al26__si27   = 472
  integer, parameter :: k_he4_al26__p30   = 473
  integer, parameter :: k_n_al27__al28   = 474
  integer, parameter :: k_p_al27__si28   = 475
  integer, parameter :: k_he4_al27__p31   = 476
  integer, parameter :: k_n_al28__al29   = 477
  integer, parameter :: k_p_al28__si29   = 478
  integer, parameter :: k_he4_al28__p32   = 479
  integer, parameter :: k_p_al29__si30   = 480
  integer, parameter :: k_he4_al29__p33   = 481
  integer, parameter :: k_n_si27__si28   = 482
  integer, parameter :: k_he4_si27__s31   = 483
  integer, parameter :: k_n_si28__si29   = 484
  integer, parameter :: k_p_si28__p29   = 485
  integer, parameter :: k_he4_si28__s32   = 486
  integer, parameter :: k_n_si29__si30   = 487
  integer, parameter :: k_p_si29__p30   = 488
  integer, parameter :: k_he4_si29__s33   = 489
  integer, parameter :: k_n_si30__si31   = 490
  integer, parameter :: k_p_si30__p31   = 491
  integer, parameter :: k_he4_si30__s34   = 492
  integer, parameter :: k_n_si31__si32   = 493
  integer, parameter :: k_p_si31__p32   = 494
  integer, parameter :: k_he4_si31__s35   = 495
  integer, parameter :: k_p_si32__p33   = 496
  integer, parameter :: k_n_p29__p30   = 497
  integer, parameter :: k_p_p29__s30   = 498
  integer, parameter :: k_he4_p29__cl33   = 499
  integer, parameter :: k_n_p30__p31   = 500
  integer, parameter :: k_p_p30__s31   = 501
  integer, parameter :: k_he4_p30__cl34   = 502
  integer, parameter :: k_n_p31__p32   = 503
  integer, parameter :: k_p_p31__s32   = 504
  integer, parameter :: k_he4_p31__cl35   = 505
  integer, parameter :: k_n_p32__p33   = 506
  integer, parameter :: k_p_p32__s33   = 507
  integer, parameter :: k_he4_p32__cl36   = 508
  integer, parameter :: k_p_p33__s34   = 509
  integer, parameter :: k_he4_p33__cl37   = 510
  integer, parameter :: k_n_s30__s31   = 511
  integer, parameter :: k_n_s31__s32   = 512
  integer, parameter :: k_he4_s31__ar35   = 513
  integer, parameter :: k_n_s32__s33   = 514
  integer, parameter :: k_p_s32__cl33   = 515
  integer, parameter :: k_he4_s32__ar36   = 516
  integer, parameter :: k_n_s33__s34   = 517
  integer, parameter :: k_p_s33__cl34   = 518
  integer, parameter :: k_he4_s33__ar37   = 519
  integer, parameter :: k_n_s34__s35   = 520
  integer, parameter :: k_p_s34__cl35   = 521
  integer, parameter :: k_he4_s34__ar38   = 522
  integer, parameter :: k_p_s35__cl36   = 523
  integer, parameter :: k_he4_s35__ar39   = 524
  integer, parameter :: k_n_cl33__cl34   = 525
  integer, parameter :: k_he4_cl33__k37   = 526
  integer, parameter :: k_n_cl34__cl35   = 527
  integer, parameter :: k_p_cl34__ar35   = 528
  integer, parameter :: k_he4_cl34__k38   = 529
  integer, parameter :: k_n_cl35__cl36   = 530
  integer, parameter :: k_p_cl35__ar36   = 531
  integer, parameter :: k_he4_cl35__k39   = 532
  integer, parameter :: k_n_cl36__cl37   = 533
  integer, parameter :: k_p_cl36__ar37   = 534
  integer, parameter :: k_he4_cl36__k40   = 535
  integer, parameter :: k_p_cl37__ar38   = 536
  integer, parameter :: k_he4_cl37__k41   = 537
  integer, parameter :: k_n_ar35__ar36   = 538
  integer, parameter :: k_he4_ar35__ca39   = 539
  integer, parameter :: k_n_ar36__ar37   = 540
  integer, parameter :: k_p_ar36__k37   = 541
  integer, parameter :: k_he4_ar36__ca40   = 542
  integer, parameter :: k_n_ar37__ar38   = 543
  integer, parameter :: k_p_ar37__k38   = 544
  integer, parameter :: k_he4_ar37__ca41   = 545
  integer, parameter :: k_n_ar38__ar39   = 546
  integer, parameter :: k_p_ar38__k39   = 547
  integer, parameter :: k_he4_ar38__ca42   = 548
  integer, parameter :: k_p_ar39__k40   = 549
  integer, parameter :: k_he4_ar39__ca43   = 550
  integer, parameter :: k_n_k37__k38   = 551
  integer, parameter :: k_n_k38__k39   = 552
  integer, parameter :: k_p_k38__ca39   = 553
  integer, parameter :: k_he4_k38__sc42   = 554
  integer, parameter :: k_n_k39__k40   = 555
  integer, parameter :: k_p_k39__ca40   = 556
  integer, parameter :: k_he4_k39__sc43   = 557
  integer, parameter :: k_n_k40__k41   = 558
  integer, parameter :: k_p_k40__ca41   = 559
  integer, parameter :: k_he4_k40__sc44   = 560
  integer, parameter :: k_n_k41__k42   = 561
  integer, parameter :: k_p_k41__ca42   = 562
  integer, parameter :: k_he4_k41__sc45   = 563
  integer, parameter :: k_p_k42__ca43   = 564
  integer, parameter :: k_he4_k42__sc46   = 565
  integer, parameter :: k_n_ca39__ca40   = 566
  integer, parameter :: k_he4_ca39__ti43   = 567
  integer, parameter :: k_n_ca40__ca41   = 568
  integer, parameter :: k_he4_ca40__ti44   = 569
  integer, parameter :: k_n_ca41__ca42   = 570
  integer, parameter :: k_p_ca41__sc42   = 571
  integer, parameter :: k_he4_ca41__ti45   = 572
  integer, parameter :: k_n_ca42__ca43   = 573
  integer, parameter :: k_p_ca42__sc43   = 574
  integer, parameter :: k_he4_ca42__ti46   = 575
  integer, parameter :: k_n_ca43__ca44   = 576
  integer, parameter :: k_p_ca43__sc44   = 577
  integer, parameter :: k_he4_ca43__ti47   = 578
  integer, parameter :: k_p_ca44__sc45   = 579
  integer, parameter :: k_he4_ca44__ti48   = 580
  integer, parameter :: k_n_sc42__sc43   = 581
  integer, parameter :: k_p_sc42__ti43   = 582
  integer, parameter :: k_he4_sc42__v46   = 583
  integer, parameter :: k_n_sc43__sc44   = 584
  integer, parameter :: k_p_sc43__ti44   = 585
  integer, parameter :: k_he4_sc43__v47   = 586
  integer, parameter :: k_n_sc44__sc45   = 587
  integer, parameter :: k_p_sc44__ti45   = 588
  integer, parameter :: k_he4_sc44__v48   = 589
  integer, parameter :: k_n_sc45__sc46   = 590
  integer, parameter :: k_p_sc45__ti46   = 591
  integer, parameter :: k_he4_sc45__v49   = 592
  integer, parameter :: k_p_sc46__ti47   = 593
  integer, parameter :: k_he4_sc46__v50   = 594
  integer, parameter :: k_n_ti43__ti44   = 595
  integer, parameter :: k_he4_ti43__cr47   = 596
  integer, parameter :: k_n_ti44__ti45   = 597
  integer, parameter :: k_he4_ti44__cr48   = 598
  integer, parameter :: k_n_ti45__ti46   = 599
  integer, parameter :: k_p_ti45__v46   = 600
  integer, parameter :: k_he4_ti45__cr49   = 601
  integer, parameter :: k_n_ti46__ti47   = 602
  integer, parameter :: k_p_ti46__v47   = 603
  integer, parameter :: k_he4_ti46__cr50   = 604
  integer, parameter :: k_n_ti47__ti48   = 605
  integer, parameter :: k_p_ti47__v48   = 606
  integer, parameter :: k_he4_ti47__cr51   = 607
  integer, parameter :: k_n_ti48__ti49   = 608
  integer, parameter :: k_p_ti48__v49   = 609
  integer, parameter :: k_he4_ti48__cr52   = 610
  integer, parameter :: k_p_ti49__v50   = 611
  integer, parameter :: k_n_v46__v47   = 612
  integer, parameter :: k_p_v46__cr47   = 613
  integer, parameter :: k_he4_v46__mn50   = 614
  integer, parameter :: k_n_v47__v48   = 615
  integer, parameter :: k_p_v47__cr48   = 616
  integer, parameter :: k_he4_v47__mn51   = 617
  integer, parameter :: k_n_v48__v49   = 618
  integer, parameter :: k_p_v48__cr49   = 619
  integer, parameter :: k_he4_v48__mn52   = 620
  integer, parameter :: k_n_v49__v50   = 621
  integer, parameter :: k_p_v49__cr50   = 622
  integer, parameter :: k_he4_v49__mn53   = 623
  integer, parameter :: k_n_v50__v51   = 624
  integer, parameter :: k_p_v50__cr51   = 625
  integer, parameter :: k_he4_v50__mn54   = 626
  integer, parameter :: k_p_v51__cr52   = 627
  integer, parameter :: k_he4_v51__mn55   = 628
  integer, parameter :: k_n_cr47__cr48   = 629
  integer, parameter :: k_he4_cr47__fe51   = 630
  integer, parameter :: k_n_cr48__cr49   = 631
  integer, parameter :: k_p_cr48__mn49   = 632
  integer, parameter :: k_he4_cr48__fe52   = 633
  integer, parameter :: k_n_cr49__cr50   = 634
  integer, parameter :: k_p_cr49__mn50   = 635
  integer, parameter :: k_he4_cr49__fe53   = 636
  integer, parameter :: k_n_cr50__cr51   = 637
  integer, parameter :: k_p_cr50__mn51   = 638
  integer, parameter :: k_he4_cr50__fe54   = 639
  integer, parameter :: k_n_cr51__cr52   = 640
  integer, parameter :: k_p_cr51__mn52   = 641
  integer, parameter :: k_he4_cr51__fe55   = 642
  integer, parameter :: k_p_cr52__mn53   = 643
  integer, parameter :: k_he4_cr52__fe56   = 644
  integer, parameter :: k_n_mn49__mn50   = 645
  integer, parameter :: k_he4_mn49__co53   = 646
  integer, parameter :: k_n_mn50__mn51   = 647
  integer, parameter :: k_p_mn50__fe51   = 648
  integer, parameter :: k_he4_mn50__co54   = 649
  integer, parameter :: k_n_mn51__mn52   = 650
  integer, parameter :: k_p_mn51__fe52   = 651
  integer, parameter :: k_he4_mn51__co55   = 652
  integer, parameter :: k_n_mn52__mn53   = 653
  integer, parameter :: k_p_mn52__fe53   = 654
  integer, parameter :: k_he4_mn52__co56   = 655
  integer, parameter :: k_n_mn53__mn54   = 656
  integer, parameter :: k_p_mn53__fe54   = 657
  integer, parameter :: k_he4_mn53__co57   = 658
  integer, parameter :: k_n_mn54__mn55   = 659
  integer, parameter :: k_p_mn54__fe55   = 660
  integer, parameter :: k_he4_mn54__co58   = 661
  integer, parameter :: k_p_mn55__fe56   = 662
  integer, parameter :: k_n_fe51__fe52   = 663
  integer, parameter :: k_he4_fe51__ni55   = 664
  integer, parameter :: k_n_fe52__fe53   = 665
  integer, parameter :: k_p_fe52__co53   = 666
  integer, parameter :: k_he4_fe52__ni56   = 667
  integer, parameter :: k_n_fe53__fe54   = 668
  integer, parameter :: k_p_fe53__co54   = 669
  integer, parameter :: k_he4_fe53__ni57   = 670
  integer, parameter :: k_n_fe54__fe55   = 671
  integer, parameter :: k_p_fe54__co55   = 672
  integer, parameter :: k_he4_fe54__ni58   = 673
  integer, parameter :: k_n_fe55__fe56   = 674
  integer, parameter :: k_p_fe55__co56   = 675
  integer, parameter :: k_he4_fe55__ni59   = 676
  integer, parameter :: k_p_fe56__co57   = 677
  integer, parameter :: k_he4_fe56__ni60   = 678
  integer, parameter :: k_n_co53__co54   = 679
  integer, parameter :: k_p_co53__ni54   = 680
  integer, parameter :: k_n_co54__co55   = 681
  integer, parameter :: k_p_co54__ni55   = 682
  integer, parameter :: k_n_co55__co56   = 683
  integer, parameter :: k_p_co55__ni56   = 684
  integer, parameter :: k_n_co56__co57   = 685
  integer, parameter :: k_p_co56__ni57   = 686
  integer, parameter :: k_n_co57__co58   = 687
  integer, parameter :: k_p_co57__ni58   = 688
  integer, parameter :: k_p_co58__ni59   = 689
  integer, parameter :: k_n_ni54__ni55   = 690
  integer, parameter :: k_n_ni55__ni56   = 691
  integer, parameter :: k_n_ni56__ni57   = 692
  integer, parameter :: k_n_ni57__ni58   = 693
  integer, parameter :: k_n_ni58__ni59   = 694
  integer, parameter :: k_n_ni59__ni60   = 695
  integer, parameter :: k_d_d__n_he3   = 696
  integer, parameter :: k_d_d__p_t   = 697
  integer, parameter :: k_p_t__n_he3   = 698
  integer, parameter :: k_p_t__d_d   = 699
  integer, parameter :: k_d_t__n_he4   = 700
  integer, parameter :: k_n_he3__p_t   = 701
  integer, parameter :: k_n_he3__d_d   = 702
  integer, parameter :: k_d_he3__p_he4   = 703
  integer, parameter :: k_t_he3__d_he4   = 704
  integer, parameter :: k_n_he4__d_t   = 705
  integer, parameter :: k_p_he4__d_he3   = 706
  integer, parameter :: k_d_he4__t_he3   = 707
  integer, parameter :: k_he4_c12__p_n15   = 708
  integer, parameter :: k_c12_c12__n_mg23   = 709
  integer, parameter :: k_c12_c12__p_na23   = 710
  integer, parameter :: k_c12_c12__he4_ne20   = 711
  integer, parameter :: k_p_c13__n_n13   = 712
  integer, parameter :: k_d_c13__n_n14   = 713
  integer, parameter :: k_he4_c13__n_o16   = 714
  integer, parameter :: k_p_c14__n_n14   = 715
  integer, parameter :: k_d_c14__n_n15   = 716
  integer, parameter :: k_he4_c14__n_o17   = 717
  integer, parameter :: k_n_n13__p_c13   = 718
  integer, parameter :: k_he4_n13__p_o16   = 719
  integer, parameter :: k_n_n14__p_c14   = 720
  integer, parameter :: k_n_n14__d_c13   = 721
  integer, parameter :: k_he4_n14__p_o17   = 722
  integer, parameter :: k_n_n15__d_c14   = 723
  integer, parameter :: k_p_n15__he4_c12   = 724
  integer, parameter :: k_he4_n15__n_f18   = 725
  integer, parameter :: k_he4_n15__p_o18   = 726
  integer, parameter :: k_n_o16__he4_c13   = 727
  integer, parameter :: k_p_o16__he4_n13   = 728
  integer, parameter :: k_he4_o16__n_ne19   = 729
  integer, parameter :: k_he4_o16__p_f19   = 730
  integer, parameter :: k_c12_o16__n_si27   = 731
  integer, parameter :: k_c12_o16__p_al27   = 732
  integer, parameter :: k_c12_o16__he4_mg24   = 733
  integer, parameter :: k_o16_o16__n_s31   = 734
  integer, parameter :: k_o16_o16__p_p31   = 735
  integer, parameter :: k_o16_o16__he4_si28   = 736
  integer, parameter :: k_n_o17__he4_c14   = 737
  integer, parameter :: k_p_o17__he4_n14   = 738
  integer, parameter :: k_he4_o17__n_ne20   = 739
  integer, parameter :: k_he4_o17__p_f20   = 740
  integer, parameter :: k_p_o18__n_f18   = 741
  integer, parameter :: k_p_o18__he4_n15   = 742
  integer, parameter :: k_he4_o18__n_ne21   = 743
  integer, parameter :: k_he4_o18__p_f21   = 744
  integer, parameter :: k_p_o19__n_f19   = 745
  integer, parameter :: k_he4_o19__n_ne22   = 746
  integer, parameter :: k_n_f18__p_o18   = 747
  integer, parameter :: k_n_f18__he4_n15   = 748
  integer, parameter :: k_he4_f18__n_na21   = 749
  integer, parameter :: k_he4_f18__p_ne21   = 750
  integer, parameter :: k_n_f19__p_o19   = 751
  integer, parameter :: k_p_f19__n_ne19   = 752
  integer, parameter :: k_p_f19__he4_o16   = 753
  integer, parameter :: k_he4_f19__n_na22   = 754
  integer, parameter :: k_he4_f19__p_ne22   = 755
  integer, parameter :: k_p_f20__n_ne20   = 756
  integer, parameter :: k_p_f20__he4_o17   = 757
  integer, parameter :: k_he4_f20__n_na23   = 758
  integer, parameter :: k_he4_f20__p_ne23   = 759
  integer, parameter :: k_p_f21__n_ne21   = 760
  integer, parameter :: k_p_f21__he4_o18   = 761
  integer, parameter :: k_he4_f21__n_na24   = 762
  integer, parameter :: k_he4_f21__p_ne24   = 763
  integer, parameter :: k_n_ne19__p_f19   = 764
  integer, parameter :: k_n_ne19__he4_o16   = 765
  integer, parameter :: k_he4_ne19__n_mg22   = 766
  integer, parameter :: k_he4_ne19__p_na22   = 767
  integer, parameter :: k_n_ne20__p_f20   = 768
  integer, parameter :: k_n_ne20__he4_o17   = 769
  integer, parameter :: k_p_ne20__n_na20   = 770
  integer, parameter :: k_he4_ne20__n_mg23   = 771
  integer, parameter :: k_he4_ne20__p_na23   = 772
  integer, parameter :: k_he4_ne20__c12_c12   = 773
  integer, parameter :: k_c12_ne20__n_s31   = 774
  integer, parameter :: k_c12_ne20__p_p31   = 775
  integer, parameter :: k_c12_ne20__he4_si28   = 776
  integer, parameter :: k_n_ne21__p_f21   = 777
  integer, parameter :: k_n_ne21__he4_o18   = 778
  integer, parameter :: k_p_ne21__n_na21   = 779
  integer, parameter :: k_p_ne21__he4_f18   = 780
  integer, parameter :: k_he4_ne21__n_mg24   = 781
  integer, parameter :: k_he4_ne21__p_na24   = 782
  integer, parameter :: k_n_ne22__he4_o19   = 783
  integer, parameter :: k_p_ne22__n_na22   = 784
  integer, parameter :: k_p_ne22__he4_f19   = 785
  integer, parameter :: k_he4_ne22__n_mg25   = 786
  integer, parameter :: k_p_ne23__n_na23   = 787
  integer, parameter :: k_p_ne23__he4_f20   = 788
  integer, parameter :: k_he4_ne23__n_mg26   = 789
  integer, parameter :: k_p_ne24__n_na24   = 790
  integer, parameter :: k_p_ne24__he4_f21   = 791
  integer, parameter :: k_he4_ne24__n_mg27   = 792
  integer, parameter :: k_n_na20__p_ne20   = 793
  integer, parameter :: k_he4_na20__p_mg23   = 794
  integer, parameter :: k_n_na21__p_ne21   = 795
  integer, parameter :: k_n_na21__he4_f18   = 796
  integer, parameter :: k_he4_na21__p_mg24   = 797
  integer, parameter :: k_n_na22__p_ne22   = 798
  integer, parameter :: k_n_na22__he4_f19   = 799
  integer, parameter :: k_p_na22__n_mg22   = 800
  integer, parameter :: k_p_na22__he4_ne19   = 801
  integer, parameter :: k_he4_na22__n_al25   = 802
  integer, parameter :: k_he4_na22__p_mg25   = 803
  integer, parameter :: k_n_na23__p_ne23   = 804
  integer, parameter :: k_n_na23__he4_f20   = 805
  integer, parameter :: k_p_na23__n_mg23   = 806
  integer, parameter :: k_p_na23__he4_ne20   = 807
  integer, parameter :: k_p_na23__c12_c12   = 808
  integer, parameter :: k_he4_na23__n_al26   = 809
  integer, parameter :: k_he4_na23__p_mg26   = 810
  integer, parameter :: k_n_na24__p_ne24   = 811
  integer, parameter :: k_n_na24__he4_f21   = 812
  integer, parameter :: k_p_na24__n_mg24   = 813
  integer, parameter :: k_p_na24__he4_ne21   = 814
  integer, parameter :: k_he4_na24__n_al27   = 815
  integer, parameter :: k_he4_na24__p_mg27   = 816
  integer, parameter :: k_n_mg22__p_na22   = 817
  integer, parameter :: k_n_mg22__he4_ne19   = 818
  integer, parameter :: k_he4_mg22__p_al25   = 819
  integer, parameter :: k_n_mg23__p_na23   = 820
  integer, parameter :: k_n_mg23__he4_ne20   = 821
  integer, parameter :: k_n_mg23__c12_c12   = 822
  integer, parameter :: k_p_mg23__he4_na20   = 823
  integer, parameter :: k_he4_mg23__p_al26   = 824
  integer, parameter :: k_n_mg24__p_na24   = 825
  integer, parameter :: k_n_mg24__he4_ne21   = 826
  integer, parameter :: k_p_mg24__he4_na21   = 827
  integer, parameter :: k_he4_mg24__n_si27   = 828
  integer, parameter :: k_he4_mg24__p_al27   = 829
  integer, parameter :: k_he4_mg24__c12_o16   = 830
  integer, parameter :: k_n_mg25__he4_ne22   = 831
  integer, parameter :: k_p_mg25__n_al25   = 832
  integer, parameter :: k_p_mg25__he4_na22   = 833
  integer, parameter :: k_he4_mg25__n_si28   = 834
  integer, parameter :: k_he4_mg25__p_al28   = 835
  integer, parameter :: k_n_mg26__he4_ne23   = 836
  integer, parameter :: k_p_mg26__n_al26   = 837
  integer, parameter :: k_p_mg26__he4_na23   = 838
  integer, parameter :: k_he4_mg26__n_si29   = 839
  integer, parameter :: k_he4_mg26__p_al29   = 840
  integer, parameter :: k_n_mg27__he4_ne24   = 841
  integer, parameter :: k_p_mg27__n_al27   = 842
  integer, parameter :: k_p_mg27__he4_na24   = 843
  integer, parameter :: k_he4_mg27__n_si30   = 844
  integer, parameter :: k_n_al25__p_mg25   = 845
  integer, parameter :: k_n_al25__he4_na22   = 846
  integer, parameter :: k_p_al25__he4_mg22   = 847
  integer, parameter :: k_he4_al25__p_si28   = 848
  integer, parameter :: k_n_al26__p_mg26   = 849
  integer, parameter :: k_n_al26__he4_na23   = 850
  integer, parameter :: k_p_al26__he4_mg23   = 851
  integer, parameter :: k_he4_al26__n_p29   = 852
  integer, parameter :: k_he4_al26__p_si29   = 853
  integer, parameter :: k_n_al27__p_mg27   = 854
  integer, parameter :: k_n_al27__he4_na24   = 855
  integer, parameter :: k_p_al27__n_si27   = 856
  integer, parameter :: k_p_al27__he4_mg24   = 857
  integer, parameter :: k_p_al27__c12_o16   = 858
  integer, parameter :: k_he4_al27__n_p30   = 859
  integer, parameter :: k_he4_al27__p_si30   = 860
  integer, parameter :: k_p_al28__n_si28   = 861
  integer, parameter :: k_p_al28__he4_mg25   = 862
  integer, parameter :: k_he4_al28__n_p31   = 863
  integer, parameter :: k_he4_al28__p_si31   = 864
  integer, parameter :: k_p_al29__n_si29   = 865
  integer, parameter :: k_p_al29__he4_mg26   = 866
  integer, parameter :: k_he4_al29__n_p32   = 867
  integer, parameter :: k_he4_al29__p_si32   = 868
  integer, parameter :: k_n_si27__p_al27   = 869
  integer, parameter :: k_n_si27__he4_mg24   = 870
  integer, parameter :: k_n_si27__c12_o16   = 871
  integer, parameter :: k_he4_si27__n_s30   = 872
  integer, parameter :: k_he4_si27__p_p30   = 873
  integer, parameter :: k_n_si28__p_al28   = 874
  integer, parameter :: k_n_si28__he4_mg25   = 875
  integer, parameter :: k_p_si28__he4_al25   = 876
  integer, parameter :: k_he4_si28__n_s31   = 877
  integer, parameter :: k_he4_si28__p_p31   = 878
  integer, parameter :: k_he4_si28__c12_ne20   = 879
  integer, parameter :: k_he4_si28__o16_o16   = 880
  integer, parameter :: k_n_si29__p_al29   = 881
  integer, parameter :: k_n_si29__he4_mg26   = 882
  integer, parameter :: k_p_si29__n_p29   = 883
  integer, parameter :: k_p_si29__he4_al26   = 884
  integer, parameter :: k_he4_si29__n_s32   = 885
  integer, parameter :: k_he4_si29__p_p32   = 886
  integer, parameter :: k_n_si30__he4_mg27   = 887
  integer, parameter :: k_p_si30__n_p30   = 888
  integer, parameter :: k_p_si30__he4_al27   = 889
  integer, parameter :: k_he4_si30__n_s33   = 890
  integer, parameter :: k_he4_si30__p_p33   = 891
  integer, parameter :: k_p_si31__n_p31   = 892
  integer, parameter :: k_p_si31__he4_al28   = 893
  integer, parameter :: k_he4_si31__n_s34   = 894
  integer, parameter :: k_p_si32__n_p32   = 895
  integer, parameter :: k_p_si32__he4_al29   = 896
  integer, parameter :: k_he4_si32__n_s35   = 897
  integer, parameter :: k_n_p29__p_si29   = 898
  integer, parameter :: k_n_p29__he4_al26   = 899
  integer, parameter :: k_he4_p29__p_s32   = 900
  integer, parameter :: k_n_p30__p_si30   = 901
  integer, parameter :: k_n_p30__he4_al27   = 902
  integer, parameter :: k_p_p30__n_s30   = 903
  integer, parameter :: k_p_p30__he4_si27   = 904
  integer, parameter :: k_he4_p30__n_cl33   = 905
  integer, parameter :: k_he4_p30__p_s33   = 906
  integer, parameter :: k_n_p31__p_si31   = 907
  integer, parameter :: k_n_p31__he4_al28   = 908
  integer, parameter :: k_p_p31__n_s31   = 909
  integer, parameter :: k_p_p31__he4_si28   = 910
  integer, parameter :: k_p_p31__c12_ne20   = 911
  integer, parameter :: k_p_p31__o16_o16   = 912
  integer, parameter :: k_he4_p31__n_cl34   = 913
  integer, parameter :: k_he4_p31__p_s34   = 914
  integer, parameter :: k_n_p32__p_si32   = 915
  integer, parameter :: k_n_p32__he4_al29   = 916
  integer, parameter :: k_p_p32__n_s32   = 917
  integer, parameter :: k_p_p32__he4_si29   = 918
  integer, parameter :: k_he4_p32__n_cl35   = 919
  integer, parameter :: k_he4_p32__p_s35   = 920
  integer, parameter :: k_p_p33__n_s33   = 921
  integer, parameter :: k_p_p33__he4_si30   = 922
  integer, parameter :: k_he4_p33__n_cl36   = 923
  integer, parameter :: k_n_s30__p_p30   = 924
  integer, parameter :: k_n_s30__he4_si27   = 925
  integer, parameter :: k_he4_s30__p_cl33   = 926
  integer, parameter :: k_n_s31__p_p31   = 927
  integer, parameter :: k_n_s31__he4_si28   = 928
  integer, parameter :: k_n_s31__c12_ne20   = 929
  integer, parameter :: k_n_s31__o16_o16   = 930
  integer, parameter :: k_he4_s31__p_cl34   = 931
  integer, parameter :: k_n_s32__p_p32   = 932
  integer, parameter :: k_n_s32__he4_si29   = 933
  integer, parameter :: k_p_s32__he4_p29   = 934
  integer, parameter :: k_he4_s32__n_ar35   = 935
  integer, parameter :: k_he4_s32__p_cl35   = 936
  integer, parameter :: k_n_s33__p_p33   = 937
  integer, parameter :: k_n_s33__he4_si30   = 938
  integer, parameter :: k_p_s33__n_cl33   = 939
  integer, parameter :: k_p_s33__he4_p30   = 940
  integer, parameter :: k_he4_s33__n_ar36   = 941
  integer, parameter :: k_he4_s33__p_cl36   = 942
  integer, parameter :: k_n_s34__he4_si31   = 943
  integer, parameter :: k_p_s34__n_cl34   = 944
  integer, parameter :: k_p_s34__he4_p31   = 945
  integer, parameter :: k_he4_s34__n_ar37   = 946
  integer, parameter :: k_he4_s34__p_cl37   = 947
  integer, parameter :: k_n_s35__he4_si32   = 948
  integer, parameter :: k_p_s35__n_cl35   = 949
  integer, parameter :: k_p_s35__he4_p32   = 950
  integer, parameter :: k_he4_s35__n_ar38   = 951
  integer, parameter :: k_n_cl33__p_s33   = 952
  integer, parameter :: k_n_cl33__he4_p30   = 953
  integer, parameter :: k_p_cl33__he4_s30   = 954
  integer, parameter :: k_he4_cl33__p_ar36   = 955
  integer, parameter :: k_n_cl34__p_s34   = 956
  integer, parameter :: k_n_cl34__he4_p31   = 957
  integer, parameter :: k_p_cl34__he4_s31   = 958
  integer, parameter :: k_he4_cl34__n_k37   = 959
  integer, parameter :: k_he4_cl34__p_ar37   = 960
  integer, parameter :: k_n_cl35__p_s35   = 961
  integer, parameter :: k_n_cl35__he4_p32   = 962
  integer, parameter :: k_p_cl35__n_ar35   = 963
  integer, parameter :: k_p_cl35__he4_s32   = 964
  integer, parameter :: k_he4_cl35__n_k38   = 965
  integer, parameter :: k_he4_cl35__p_ar38   = 966
  integer, parameter :: k_n_cl36__he4_p33   = 967
  integer, parameter :: k_p_cl36__n_ar36   = 968
  integer, parameter :: k_p_cl36__he4_s33   = 969
  integer, parameter :: k_he4_cl36__n_k39   = 970
  integer, parameter :: k_he4_cl36__p_ar39   = 971
  integer, parameter :: k_p_cl37__n_ar37   = 972
  integer, parameter :: k_p_cl37__he4_s34   = 973
  integer, parameter :: k_he4_cl37__n_k40   = 974
  integer, parameter :: k_n_ar35__p_cl35   = 975
  integer, parameter :: k_n_ar35__he4_s32   = 976
  integer, parameter :: k_he4_ar35__p_k38   = 977
  integer, parameter :: k_n_ar36__p_cl36   = 978
  integer, parameter :: k_n_ar36__he4_s33   = 979
  integer, parameter :: k_p_ar36__he4_cl33   = 980
  integer, parameter :: k_he4_ar36__n_ca39   = 981
  integer, parameter :: k_he4_ar36__p_k39   = 982
  integer, parameter :: k_n_ar37__p_cl37   = 983
  integer, parameter :: k_n_ar37__he4_s34   = 984
  integer, parameter :: k_p_ar37__n_k37   = 985
  integer, parameter :: k_p_ar37__he4_cl34   = 986
  integer, parameter :: k_he4_ar37__n_ca40   = 987
  integer, parameter :: k_he4_ar37__p_k40   = 988
  integer, parameter :: k_n_ar38__he4_s35   = 989
  integer, parameter :: k_p_ar38__n_k38   = 990
  integer, parameter :: k_p_ar38__he4_cl35   = 991
  integer, parameter :: k_he4_ar38__n_ca41   = 992
  integer, parameter :: k_he4_ar38__p_k41   = 993
  integer, parameter :: k_p_ar39__n_k39   = 994
  integer, parameter :: k_p_ar39__he4_cl36   = 995
  integer, parameter :: k_he4_ar39__n_ca42   = 996
  integer, parameter :: k_he4_ar39__p_k42   = 997
  integer, parameter :: k_n_k37__p_ar37   = 998
  integer, parameter :: k_n_k37__he4_cl34   = 999
  integer, parameter :: k_he4_k37__p_ca40   = 1000
  integer, parameter :: k_n_k38__p_ar38   = 1001
  integer, parameter :: k_n_k38__he4_cl35   = 1002
  integer, parameter :: k_p_k38__he4_ar35   = 1003
  integer, parameter :: k_he4_k38__p_ca41   = 1004
  integer, parameter :: k_n_k39__p_ar39   = 1005
  integer, parameter :: k_n_k39__he4_cl36   = 1006
  integer, parameter :: k_p_k39__n_ca39   = 1007
  integer, parameter :: k_p_k39__he4_ar36   = 1008
  integer, parameter :: k_he4_k39__n_sc42   = 1009
  integer, parameter :: k_he4_k39__p_ca42   = 1010
  integer, parameter :: k_n_k40__he4_cl37   = 1011
  integer, parameter :: k_p_k40__n_ca40   = 1012
  integer, parameter :: k_p_k40__he4_ar37   = 1013
  integer, parameter :: k_he4_k40__n_sc43   = 1014
  integer, parameter :: k_he4_k40__p_ca43   = 1015
  integer, parameter :: k_p_k41__n_ca41   = 1016
  integer, parameter :: k_p_k41__he4_ar38   = 1017
  integer, parameter :: k_he4_k41__n_sc44   = 1018
  integer, parameter :: k_he4_k41__p_ca44   = 1019
  integer, parameter :: k_p_k42__n_ca42   = 1020
  integer, parameter :: k_p_k42__he4_ar39   = 1021
  integer, parameter :: k_he4_k42__n_sc45   = 1022
  integer, parameter :: k_n_ca39__p_k39   = 1023
  integer, parameter :: k_n_ca39__he4_ar36   = 1024
  integer, parameter :: k_he4_ca39__p_sc42   = 1025
  integer, parameter :: k_n_ca40__p_k40   = 1026
  integer, parameter :: k_n_ca40__he4_ar37   = 1027
  integer, parameter :: k_p_ca40__he4_k37   = 1028
  integer, parameter :: k_he4_ca40__n_ti43   = 1029
  integer, parameter :: k_he4_ca40__p_sc43   = 1030
  integer, parameter :: k_n_ca41__p_k41   = 1031
  integer, parameter :: k_n_ca41__he4_ar38   = 1032
  integer, parameter :: k_p_ca41__he4_k38   = 1033
  integer, parameter :: k_he4_ca41__n_ti44   = 1034
  integer, parameter :: k_he4_ca41__p_sc44   = 1035
  integer, parameter :: k_n_ca42__p_k42   = 1036
  integer, parameter :: k_n_ca42__he4_ar39   = 1037
  integer, parameter :: k_p_ca42__n_sc42   = 1038
  integer, parameter :: k_p_ca42__he4_k39   = 1039
  integer, parameter :: k_he4_ca42__n_ti45   = 1040
  integer, parameter :: k_he4_ca42__p_sc45   = 1041
  integer, parameter :: k_p_ca43__n_sc43   = 1042
  integer, parameter :: k_p_ca43__he4_k40   = 1043
  integer, parameter :: k_he4_ca43__n_ti46   = 1044
  integer, parameter :: k_he4_ca43__p_sc46   = 1045
  integer, parameter :: k_p_ca44__n_sc44   = 1046
  integer, parameter :: k_p_ca44__he4_k41   = 1047
  integer, parameter :: k_he4_ca44__n_ti47   = 1048
  integer, parameter :: k_n_sc42__p_ca42   = 1049
  integer, parameter :: k_n_sc42__he4_k39   = 1050
  integer, parameter :: k_p_sc42__he4_ca39   = 1051
  integer, parameter :: k_he4_sc42__p_ti45   = 1052
  integer, parameter :: k_n_sc43__p_ca43   = 1053
  integer, parameter :: k_n_sc43__he4_k40   = 1054
  integer, parameter :: k_p_sc43__n_ti43   = 1055
  integer, parameter :: k_p_sc43__he4_ca40   = 1056
  integer, parameter :: k_he4_sc43__n_v46   = 1057
  integer, parameter :: k_he4_sc43__p_ti46   = 1058
  integer, parameter :: k_n_sc44__p_ca44   = 1059
  integer, parameter :: k_n_sc44__he4_k41   = 1060
  integer, parameter :: k_p_sc44__n_ti44   = 1061
  integer, parameter :: k_p_sc44__he4_ca41   = 1062
  integer, parameter :: k_he4_sc44__n_v47   = 1063
  integer, parameter :: k_he4_sc44__p_ti47   = 1064
  integer, parameter :: k_n_sc45__he4_k42   = 1065
  integer, parameter :: k_p_sc45__n_ti45   = 1066
  integer, parameter :: k_p_sc45__he4_ca42   = 1067
  integer, parameter :: k_he4_sc45__n_v48   = 1068
  integer, parameter :: k_he4_sc45__p_ti48   = 1069
  integer, parameter :: k_p_sc46__n_ti46   = 1070
  integer, parameter :: k_p_sc46__he4_ca43   = 1071
  integer, parameter :: k_he4_sc46__n_v49   = 1072
  integer, parameter :: k_he4_sc46__p_ti49   = 1073
  integer, parameter :: k_n_ti43__p_sc43   = 1074
  integer, parameter :: k_n_ti43__he4_ca40   = 1075
  integer, parameter :: k_he4_ti43__p_v46   = 1076
  integer, parameter :: k_n_ti44__p_sc44   = 1077
  integer, parameter :: k_n_ti44__he4_ca41   = 1078
  integer, parameter :: k_he4_ti44__n_cr47   = 1079
  integer, parameter :: k_he4_ti44__p_v47   = 1080
  integer, parameter :: k_n_ti45__p_sc45   = 1081
  integer, parameter :: k_n_ti45__he4_ca42   = 1082
  integer, parameter :: k_p_ti45__he4_sc42   = 1083
  integer, parameter :: k_he4_ti45__n_cr48   = 1084
  integer, parameter :: k_he4_ti45__p_v48   = 1085
  integer, parameter :: k_n_ti46__p_sc46   = 1086
  integer, parameter :: k_n_ti46__he4_ca43   = 1087
  integer, parameter :: k_p_ti46__n_v46   = 1088
  integer, parameter :: k_p_ti46__he4_sc43   = 1089
  integer, parameter :: k_he4_ti46__n_cr49   = 1090
  integer, parameter :: k_he4_ti46__p_v49   = 1091
  integer, parameter :: k_n_ti47__he4_ca44   = 1092
  integer, parameter :: k_p_ti47__n_v47   = 1093
  integer, parameter :: k_p_ti47__he4_sc44   = 1094
  integer, parameter :: k_he4_ti47__n_cr50   = 1095
  integer, parameter :: k_he4_ti47__p_v50   = 1096
  integer, parameter :: k_p_ti48__n_v48   = 1097
  integer, parameter :: k_p_ti48__he4_sc45   = 1098
  integer, parameter :: k_he4_ti48__n_cr51   = 1099
  integer, parameter :: k_he4_ti48__p_v51   = 1100
  integer, parameter :: k_p_ti49__n_v49   = 1101
  integer, parameter :: k_p_ti49__he4_sc46   = 1102
  integer, parameter :: k_he4_ti49__n_cr52   = 1103
  integer, parameter :: k_n_v46__p_ti46   = 1104
  integer, parameter :: k_n_v46__he4_sc43   = 1105
  integer, parameter :: k_p_v46__he4_ti43   = 1106
  integer, parameter :: k_he4_v46__n_mn49   = 1107
  integer, parameter :: k_he4_v46__p_cr49   = 1108
  integer, parameter :: k_n_v47__p_ti47   = 1109
  integer, parameter :: k_n_v47__he4_sc44   = 1110
  integer, parameter :: k_p_v47__n_cr47   = 1111
  integer, parameter :: k_p_v47__he4_ti44   = 1112
  integer, parameter :: k_he4_v47__n_mn50   = 1113
  integer, parameter :: k_he4_v47__p_cr50   = 1114
  integer, parameter :: k_n_v48__p_ti48   = 1115
  integer, parameter :: k_n_v48__he4_sc45   = 1116
  integer, parameter :: k_p_v48__n_cr48   = 1117
  integer, parameter :: k_p_v48__he4_ti45   = 1118
  integer, parameter :: k_he4_v48__n_mn51   = 1119
  integer, parameter :: k_he4_v48__p_cr51   = 1120
  integer, parameter :: k_n_v49__p_ti49   = 1121
  integer, parameter :: k_n_v49__he4_sc46   = 1122
  integer, parameter :: k_p_v49__n_cr49   = 1123
  integer, parameter :: k_p_v49__he4_ti46   = 1124
  integer, parameter :: k_he4_v49__n_mn52   = 1125
  integer, parameter :: k_he4_v49__p_cr52   = 1126
  integer, parameter :: k_p_v50__n_cr50   = 1127
  integer, parameter :: k_p_v50__he4_ti47   = 1128
  integer, parameter :: k_he4_v50__n_mn53   = 1129
  integer, parameter :: k_p_v51__n_cr51   = 1130
  integer, parameter :: k_p_v51__he4_ti48   = 1131
  integer, parameter :: k_he4_v51__n_mn54   = 1132
  integer, parameter :: k_n_cr47__p_v47   = 1133
  integer, parameter :: k_n_cr47__he4_ti44   = 1134
  integer, parameter :: k_he4_cr47__p_mn50   = 1135
  integer, parameter :: k_n_cr48__p_v48   = 1136
  integer, parameter :: k_n_cr48__he4_ti45   = 1137
  integer, parameter :: k_he4_cr48__n_fe51   = 1138
  integer, parameter :: k_he4_cr48__p_mn51   = 1139
  integer, parameter :: k_n_cr49__p_v49   = 1140
  integer, parameter :: k_n_cr49__he4_ti46   = 1141
  integer, parameter :: k_p_cr49__n_mn49   = 1142
  integer, parameter :: k_p_cr49__he4_v46   = 1143
  integer, parameter :: k_he4_cr49__n_fe52   = 1144
  integer, parameter :: k_he4_cr49__p_mn52   = 1145
  integer, parameter :: k_n_cr50__p_v50   = 1146
  integer, parameter :: k_n_cr50__he4_ti47   = 1147
  integer, parameter :: k_p_cr50__n_mn50   = 1148
  integer, parameter :: k_p_cr50__he4_v47   = 1149
  integer, parameter :: k_he4_cr50__n_fe53   = 1150
  integer, parameter :: k_he4_cr50__p_mn53   = 1151
  integer, parameter :: k_n_cr51__p_v51   = 1152
  integer, parameter :: k_n_cr51__he4_ti48   = 1153
  integer, parameter :: k_p_cr51__n_mn51   = 1154
  integer, parameter :: k_p_cr51__he4_v48   = 1155
  integer, parameter :: k_he4_cr51__n_fe54   = 1156
  integer, parameter :: k_he4_cr51__p_mn54   = 1157
  integer, parameter :: k_n_cr52__he4_ti49   = 1158
  integer, parameter :: k_p_cr52__n_mn52   = 1159
  integer, parameter :: k_p_cr52__he4_v49   = 1160
  integer, parameter :: k_he4_cr52__n_fe55   = 1161
  integer, parameter :: k_he4_cr52__p_mn55   = 1162
  integer, parameter :: k_n_mn49__p_cr49   = 1163
  integer, parameter :: k_n_mn49__he4_v46   = 1164
  integer, parameter :: k_he4_mn49__p_fe52   = 1165
  integer, parameter :: k_n_mn50__p_cr50   = 1166
  integer, parameter :: k_n_mn50__he4_v47   = 1167
  integer, parameter :: k_p_mn50__he4_cr47   = 1168
  integer, parameter :: k_he4_mn50__n_co53   = 1169
  integer, parameter :: k_he4_mn50__p_fe53   = 1170
  integer, parameter :: k_n_mn51__p_cr51   = 1171
  integer, parameter :: k_n_mn51__he4_v48   = 1172
  integer, parameter :: k_p_mn51__n_fe51   = 1173
  integer, parameter :: k_p_mn51__he4_cr48   = 1174
  integer, parameter :: k_he4_mn51__n_co54   = 1175
  integer, parameter :: k_he4_mn51__p_fe54   = 1176
  integer, parameter :: k_n_mn52__p_cr52   = 1177
  integer, parameter :: k_n_mn52__he4_v49   = 1178
  integer, parameter :: k_p_mn52__n_fe52   = 1179
  integer, parameter :: k_p_mn52__he4_cr49   = 1180
  integer, parameter :: k_he4_mn52__n_co55   = 1181
  integer, parameter :: k_he4_mn52__p_fe55   = 1182
  integer, parameter :: k_n_mn53__he4_v50   = 1183
  integer, parameter :: k_p_mn53__n_fe53   = 1184
  integer, parameter :: k_p_mn53__he4_cr50   = 1185
  integer, parameter :: k_he4_mn53__n_co56   = 1186
  integer, parameter :: k_he4_mn53__p_fe56   = 1187
  integer, parameter :: k_n_mn54__he4_v51   = 1188
  integer, parameter :: k_p_mn54__n_fe54   = 1189
  integer, parameter :: k_p_mn54__he4_cr51   = 1190
  integer, parameter :: k_he4_mn54__n_co57   = 1191
  integer, parameter :: k_p_mn55__n_fe55   = 1192
  integer, parameter :: k_p_mn55__he4_cr52   = 1193
  integer, parameter :: k_he4_mn55__n_co58   = 1194
  integer, parameter :: k_n_fe51__p_mn51   = 1195
  integer, parameter :: k_n_fe51__he4_cr48   = 1196
  integer, parameter :: k_he4_fe51__n_ni54   = 1197
  integer, parameter :: k_he4_fe51__p_co54   = 1198
  integer, parameter :: k_n_fe52__p_mn52   = 1199
  integer, parameter :: k_n_fe52__he4_cr49   = 1200
  integer, parameter :: k_p_fe52__he4_mn49   = 1201
  integer, parameter :: k_he4_fe52__n_ni55   = 1202
  integer, parameter :: k_he4_fe52__p_co55   = 1203
  integer, parameter :: k_n_fe53__p_mn53   = 1204
  integer, parameter :: k_n_fe53__he4_cr50   = 1205
  integer, parameter :: k_p_fe53__n_co53   = 1206
  integer, parameter :: k_p_fe53__he4_mn50   = 1207
  integer, parameter :: k_he4_fe53__n_ni56   = 1208
  integer, parameter :: k_he4_fe53__p_co56   = 1209
  integer, parameter :: k_n_fe54__p_mn54   = 1210
  integer, parameter :: k_n_fe54__he4_cr51   = 1211
  integer, parameter :: k_p_fe54__n_co54   = 1212
  integer, parameter :: k_p_fe54__he4_mn51   = 1213
  integer, parameter :: k_he4_fe54__n_ni57   = 1214
  integer, parameter :: k_he4_fe54__p_co57   = 1215
  integer, parameter :: k_n_fe55__p_mn55   = 1216
  integer, parameter :: k_n_fe55__he4_cr52   = 1217
  integer, parameter :: k_p_fe55__n_co55   = 1218
  integer, parameter :: k_p_fe55__he4_mn52   = 1219
  integer, parameter :: k_he4_fe55__n_ni58   = 1220
  integer, parameter :: k_he4_fe55__p_co58   = 1221
  integer, parameter :: k_p_fe56__n_co56   = 1222
  integer, parameter :: k_p_fe56__he4_mn53   = 1223
  integer, parameter :: k_he4_fe56__n_ni59   = 1224
  integer, parameter :: k_n_co53__p_fe53   = 1225
  integer, parameter :: k_n_co53__he4_mn50   = 1226
  integer, parameter :: k_he4_co53__p_ni56   = 1227
  integer, parameter :: k_n_co54__p_fe54   = 1228
  integer, parameter :: k_n_co54__he4_mn51   = 1229
  integer, parameter :: k_p_co54__n_ni54   = 1230
  integer, parameter :: k_p_co54__he4_fe51   = 1231
  integer, parameter :: k_he4_co54__p_ni57   = 1232
  integer, parameter :: k_n_co55__p_fe55   = 1233
  integer, parameter :: k_n_co55__he4_mn52   = 1234
  integer, parameter :: k_p_co55__n_ni55   = 1235
  integer, parameter :: k_p_co55__he4_fe52   = 1236
  integer, parameter :: k_he4_co55__p_ni58   = 1237
  integer, parameter :: k_n_co56__p_fe56   = 1238
  integer, parameter :: k_n_co56__he4_mn53   = 1239
  integer, parameter :: k_p_co56__n_ni56   = 1240
  integer, parameter :: k_p_co56__he4_fe53   = 1241
  integer, parameter :: k_he4_co56__p_ni59   = 1242
  integer, parameter :: k_n_co57__he4_mn54   = 1243
  integer, parameter :: k_p_co57__n_ni57   = 1244
  integer, parameter :: k_p_co57__he4_fe54   = 1245
  integer, parameter :: k_he4_co57__p_ni60   = 1246
  integer, parameter :: k_n_co58__he4_mn55   = 1247
  integer, parameter :: k_p_co58__n_ni58   = 1248
  integer, parameter :: k_p_co58__he4_fe55   = 1249
  integer, parameter :: k_n_ni54__p_co54   = 1250
  integer, parameter :: k_n_ni54__he4_fe51   = 1251
  integer, parameter :: k_n_ni55__p_co55   = 1252
  integer, parameter :: k_n_ni55__he4_fe52   = 1253
  integer, parameter :: k_n_ni56__p_co56   = 1254
  integer, parameter :: k_n_ni56__he4_fe53   = 1255
  integer, parameter :: k_p_ni56__he4_co53   = 1256
  integer, parameter :: k_n_ni57__p_co57   = 1257
  integer, parameter :: k_n_ni57__he4_fe54   = 1258
  integer, parameter :: k_p_ni57__he4_co54   = 1259
  integer, parameter :: k_n_ni58__p_co58   = 1260
  integer, parameter :: k_n_ni58__he4_fe55   = 1261
  integer, parameter :: k_p_ni58__he4_co55   = 1262
  integer, parameter :: k_n_ni59__he4_fe56   = 1263
  integer, parameter :: k_p_ni59__he4_co56   = 1264
  integer, parameter :: k_p_ni60__he4_co57   = 1265
  integer, parameter :: k_p_d__n_p_p   = 1266
  integer, parameter :: k_t_t__n_n_he4   = 1267
  integer, parameter :: k_t_he3__n_p_he4   = 1268
  integer, parameter :: k_he3_he3__p_p_he4   = 1269
  integer, parameter :: k_he4_he4_he4__c12   = 1270
  integer, parameter :: k_n_p_p__p   = 1271
  integer, parameter :: k_n_n_he4__t   = 1272
  integer, parameter :: k_n_p_he4__t   = 1273
  integer, parameter :: k_p_p_he4__he3   = 1274
  integer, parameter :: k_n_he4_he4__p   = 1275
  integer, parameter :: k_n_he4_he4__d   = 1276
  integer, parameter :: k_p_he4_he4__n   = 1277
  integer, parameter :: k_p_he4_he4__d   = 1278
  integer, parameter :: k_d_he4_he4__p   = 1279
  integer, parameter :: k_he4_he4_he4__n   = 1280
  integer, parameter :: k_he4_he4_he4__p   = 1281
  integer, parameter :: k_f20__o20   = 1282
  integer, parameter :: k_ne20__f20   = 1283
  integer, parameter :: k_o20__f20   = 1284
  integer, parameter :: k_f20__ne20   = 1285

  real(rt), allocatable, save :: bion(:), mion(:)

#ifdef AMREX_USE_CUDA
  attributes(managed) :: bion, mion
#endif

  !$acc declare create(bion, mion)

#ifdef REACT_SPARSE_JACOBIAN
  ! Shape of Jacobian in Compressed Sparse Row format
  integer, parameter   :: NETWORK_SPARSE_JAC_NNZ = 2488
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
    ebind_per_nucleon(jc13)   = 7.46984900000000e+00_rt
    ebind_per_nucleon(jc14)   = 7.52031900000000e+00_rt
    ebind_per_nucleon(jn13)   = 7.23886300000000e+00_rt
    ebind_per_nucleon(jn14)   = 7.47561400000000e+00_rt
    ebind_per_nucleon(jn15)   = 7.69946000000000e+00_rt
    ebind_per_nucleon(jo16)   = 7.97620600000000e+00_rt
    ebind_per_nucleon(jo17)   = 7.75072800000000e+00_rt
    ebind_per_nucleon(jo18)   = 7.76709700000000e+00_rt
    ebind_per_nucleon(jo19)   = 7.56649500000000e+00_rt
    ebind_per_nucleon(jo20)   = 7.56857000000000e+00_rt
    ebind_per_nucleon(jf18)   = 7.63163800000000e+00_rt
    ebind_per_nucleon(jf19)   = 7.77901800000000e+00_rt
    ebind_per_nucleon(jf20)   = 7.72013400000000e+00_rt
    ebind_per_nucleon(jf21)   = 7.73829300000000e+00_rt
    ebind_per_nucleon(jne19)   = 7.56734300000000e+00_rt
    ebind_per_nucleon(jne20)   = 8.03224000000000e+00_rt
    ebind_per_nucleon(jne21)   = 7.97171300000000e+00_rt
    ebind_per_nucleon(jne22)   = 8.08046500000000e+00_rt
    ebind_per_nucleon(jne23)   = 7.95525600000000e+00_rt
    ebind_per_nucleon(jne24)   = 7.99332500000000e+00_rt
    ebind_per_nucleon(jna20)   = 7.29849600000000e+00_rt
    ebind_per_nucleon(jna21)   = 7.76554700000000e+00_rt
    ebind_per_nucleon(jna22)   = 7.91566700000000e+00_rt
    ebind_per_nucleon(jna23)   = 8.11149300000000e+00_rt
    ebind_per_nucleon(jna24)   = 8.06348800000000e+00_rt
    ebind_per_nucleon(jmg22)   = 7.66276100000000e+00_rt
    ebind_per_nucleon(jmg23)   = 7.90111500000000e+00_rt
    ebind_per_nucleon(jmg24)   = 8.26070900000000e+00_rt
    ebind_per_nucleon(jmg25)   = 8.22350200000000e+00_rt
    ebind_per_nucleon(jmg26)   = 8.33387000000000e+00_rt
    ebind_per_nucleon(jmg27)   = 8.26385200000000e+00_rt
    ebind_per_nucleon(jal25)   = 8.02113600000000e+00_rt
    ebind_per_nucleon(jal26)   = 8.14976500000000e+00_rt
    ebind_per_nucleon(jal27)   = 8.33155300000000e+00_rt
    ebind_per_nucleon(jal28)   = 8.30989400000000e+00_rt
    ebind_per_nucleon(jal29)   = 8.34846400000000e+00_rt
    ebind_per_nucleon(jsi27)   = 8.12434100000000e+00_rt
    ebind_per_nucleon(jsi28)   = 8.44774400000000e+00_rt
    ebind_per_nucleon(jsi29)   = 8.44863500000000e+00_rt
    ebind_per_nucleon(jsi30)   = 8.52065400000000e+00_rt
    ebind_per_nucleon(jsi31)   = 8.45829100000000e+00_rt
    ebind_per_nucleon(jsi32)   = 8.48146800000000e+00_rt
    ebind_per_nucleon(jp29)   = 8.25123600000000e+00_rt
    ebind_per_nucleon(jp30)   = 8.35350600000000e+00_rt
    ebind_per_nucleon(jp31)   = 8.48116700000000e+00_rt
    ebind_per_nucleon(jp32)   = 8.46412000000000e+00_rt
    ebind_per_nucleon(jp33)   = 8.51380600000000e+00_rt
    ebind_per_nucleon(js30)   = 8.12270700000000e+00_rt
    ebind_per_nucleon(js31)   = 8.28180000000000e+00_rt
    ebind_per_nucleon(js32)   = 8.49312900000000e+00_rt
    ebind_per_nucleon(js33)   = 8.49763000000000e+00_rt
    ebind_per_nucleon(js34)   = 8.58349800000000e+00_rt
    ebind_per_nucleon(js35)   = 8.53785000000000e+00_rt
    ebind_per_nucleon(jcl33)   = 8.30475500000000e+00_rt
    ebind_per_nucleon(jcl34)   = 8.39897000000000e+00_rt
    ebind_per_nucleon(jcl35)   = 8.52027800000000e+00_rt
    ebind_per_nucleon(jcl36)   = 8.52193100000000e+00_rt
    ebind_per_nucleon(jcl37)   = 8.57028100000000e+00_rt
    ebind_per_nucleon(jar35)   = 8.32746100000000e+00_rt
    ebind_per_nucleon(jar36)   = 8.51990900000000e+00_rt
    ebind_per_nucleon(jar37)   = 8.52713900000000e+00_rt
    ebind_per_nucleon(jar38)   = 8.61428000000000e+00_rt
    ebind_per_nucleon(jar39)   = 8.56259800000000e+00_rt
    ebind_per_nucleon(jk37)   = 8.33984700000000e+00_rt
    ebind_per_nucleon(jk38)   = 8.43805800000000e+00_rt
    ebind_per_nucleon(jk39)   = 8.55702500000000e+00_rt
    ebind_per_nucleon(jk40)   = 8.53809000000000e+00_rt
    ebind_per_nucleon(jk41)   = 8.57607200000000e+00_rt
    ebind_per_nucleon(jk42)   = 8.55125600000000e+00_rt
    ebind_per_nucleon(jca39)   = 8.36967000000000e+00_rt
    ebind_per_nucleon(jca40)   = 8.55130300000000e+00_rt
    ebind_per_nucleon(jca41)   = 8.54670600000000e+00_rt
    ebind_per_nucleon(jca42)   = 8.61656300000000e+00_rt
    ebind_per_nucleon(jca43)   = 8.60066300000000e+00_rt
    ebind_per_nucleon(jca44)   = 8.65817500000000e+00_rt
    ebind_per_nucleon(jsc42)   = 8.44493300000000e+00_rt
    ebind_per_nucleon(jsc43)   = 8.53082500000000e+00_rt
    ebind_per_nucleon(jsc44)   = 8.55737900000000e+00_rt
    ebind_per_nucleon(jsc45)   = 8.61893100000000e+00_rt
    ebind_per_nucleon(jsc46)   = 8.62201200000000e+00_rt
    ebind_per_nucleon(jti43)   = 8.35293200000000e+00_rt
    ebind_per_nucleon(jti44)   = 8.53352000000000e+00_rt
    ebind_per_nucleon(jti45)   = 8.55572200000000e+00_rt
    ebind_per_nucleon(jti46)   = 8.65645100000000e+00_rt
    ebind_per_nucleon(jti47)   = 8.66122700000000e+00_rt
    ebind_per_nucleon(jti48)   = 8.72300600000000e+00_rt
    ebind_per_nucleon(jti49)   = 8.71115700000000e+00_rt
    ebind_per_nucleon(jv46)   = 8.48613000000000e+00_rt
    ebind_per_nucleon(jv47)   = 8.58222500000000e+00_rt
    ebind_per_nucleon(jv48)   = 8.62306100000000e+00_rt
    ebind_per_nucleon(jv49)   = 8.68290800000000e+00_rt
    ebind_per_nucleon(jv50)   = 8.69591800000000e+00_rt
    ebind_per_nucleon(jv51)   = 8.74209900000000e+00_rt
    ebind_per_nucleon(jcr47)   = 8.40719500000000e+00_rt
    ebind_per_nucleon(jcr48)   = 8.57226900000000e+00_rt
    ebind_per_nucleon(jcr49)   = 8.61329100000000e+00_rt
    ebind_per_nucleon(jcr50)   = 8.70103200000000e+00_rt
    ebind_per_nucleon(jcr51)   = 8.71200500000000e+00_rt
    ebind_per_nucleon(jcr52)   = 8.77598900000000e+00_rt
    ebind_per_nucleon(jmn49)   = 8.43992900000000e+00_rt
    ebind_per_nucleon(jmn50)   = 8.53269600000000e+00_rt
    ebind_per_nucleon(jmn51)   = 8.63377200000000e+00_rt
    ebind_per_nucleon(jmn52)   = 8.67032900000000e+00_rt
    ebind_per_nucleon(jmn53)   = 8.73417500000000e+00_rt
    ebind_per_nucleon(jmn54)   = 8.73796500000000e+00_rt
    ebind_per_nucleon(jmn55)   = 8.76502200000000e+00_rt
    ebind_per_nucleon(jfe51)   = 8.46075900000000e+00_rt
    ebind_per_nucleon(jfe52)   = 8.60957400000000e+00_rt
    ebind_per_nucleon(jfe53)   = 8.64879900000000e+00_rt
    ebind_per_nucleon(jfe54)   = 8.73638200000000e+00_rt
    ebind_per_nucleon(jfe55)   = 8.74659500000000e+00_rt
    ebind_per_nucleon(jfe56)   = 8.79035400000000e+00_rt
    ebind_per_nucleon(jco53)   = 8.47765800000000e+00_rt
    ebind_per_nucleon(jco54)   = 8.56921700000000e+00_rt
    ebind_per_nucleon(jco55)   = 8.66961800000000e+00_rt
    ebind_per_nucleon(jco56)   = 8.69483600000000e+00_rt
    ebind_per_nucleon(jco57)   = 8.74188200000000e+00_rt
    ebind_per_nucleon(jco58)   = 8.73896900000000e+00_rt
    ebind_per_nucleon(jni54)   = 8.39303200000000e+00_rt
    ebind_per_nucleon(jni55)   = 8.49732000000000e+00_rt
    ebind_per_nucleon(jni56)   = 8.64277900000000e+00_rt
    ebind_per_nucleon(jni57)   = 8.67093300000000e+00_rt
    ebind_per_nucleon(jni58)   = 8.73205900000000e+00_rt
    ebind_per_nucleon(jni59)   = 8.73658800000000e+00_rt
    ebind_per_nucleon(jni60)   = 8.78077400000000e+00_rt

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
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      8, &
      9, &
      11, &
      12, &
      133, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      133, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      133, &
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
      1, &
      2, &
      6, &
      7, &
      8, &
      10, &
      12, &
      13, &
      23, &
      31, &
      34, &
      35, &
      41, &
      44, &
      45, &
      52, &
      56, &
      133, &
      1, &
      2, &
      3, &
      6, &
      7, &
      8, &
      9, &
      10, &
      11, &
      13, &
      133, &
      1, &
      2, &
      3, &
      6, &
      8, &
      9, &
      11, &
      12, &
      14, &
      15, &
      133, &
      1, &
      2, &
      6, &
      7, &
      8, &
      10, &
      11, &
      13, &
      133, &
      1, &
      2, &
      3, &
      6, &
      8, &
      9, &
      10, &
      11, &
      12, &
      14, &
      18, &
      133, &
      1, &
      2, &
      3, &
      6, &
      7, &
      9, &
      11, &
      12, &
      13, &
      15, &
      18, &
      19, &
      133, &
      1, &
      2, &
      6, &
      7, &
      8, &
      10, &
      12, &
      13, &
      14, &
      19, &
      22, &
      23, &
      28, &
      35, &
      41, &
      44, &
      45, &
      52, &
      56, &
      133, &
      1, &
      2, &
      6, &
      9, &
      11, &
      13, &
      14, &
      15, &
      18, &
      20, &
      23, &
      24, &
      133, &
      1, &
      2, &
      6, &
      9, &
      12, &
      14, &
      15, &
      16, &
      18, &
      19, &
      21, &
      24, &
      25, &
      133, &
      1, &
      2, &
      6, &
      15, &
      16, &
      19, &
      20, &
      25, &
      26, &
      133, &
      17, &
      20, &
      133, &
      1, &
      2, &
      6, &
      11, &
      12, &
      14, &
      15, &
      18, &
      19, &
      22, &
      24, &
      29, &
      30, &
      133, &
      1, &
      2, &
      6, &
      12, &
      13, &
      15, &
      16, &
      18, &
      19, &
      20, &
      22, &
      23, &
      25, &
      30, &
      31, &
      133, &
      1, &
      2, &
      6, &
      14, &
      16, &
      17, &
      19, &
      20, &
      21, &
      23, &
      24, &
      26, &
      31, &
      32, &
      133, &
      1, &
      2, &
      6, &
      15, &
      20, &
      21, &
      24, &
      25, &
      27, &
      32, &
      133, &
      1, &
      2, &
      6, &
      13, &
      18, &
      19, &
      22, &
      23, &
      28, &
      30, &
      33, &
      34, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      14, &
      19, &
      20, &
      22, &
      23, &
      24, &
      28, &
      29, &
      31, &
      34, &
      35, &
      45, &
      52, &
      56, &
      133, &
      1, &
      2, &
      6, &
      14, &
      15, &
      18, &
      20, &
      21, &
      23, &
      24, &
      25, &
      29, &
      30, &
      32, &
      35, &
      36, &
      133, &
      1, &
      2, &
      6, &
      15, &
      16, &
      19, &
      21, &
      24, &
      25, &
      26, &
      30, &
      31, &
      36, &
      37, &
      133, &
      1, &
      2, &
      6, &
      16, &
      20, &
      25, &
      26, &
      27, &
      31, &
      32, &
      37, &
      38, &
      133, &
      1, &
      2, &
      6, &
      21, &
      26, &
      27, &
      32, &
      38, &
      133, &
      1, &
      2, &
      6, &
      22, &
      23, &
      28, &
      29, &
      34, &
      133, &
      1, &
      2, &
      6, &
      18, &
      23, &
      24, &
      28, &
      29, &
      30, &
      33, &
      35, &
      39, &
      133, &
      1, &
      2, &
      6, &
      18, &
      19, &
      22, &
      24, &
      25, &
      29, &
      30, &
      31, &
      33, &
      34, &
      36, &
      39, &
      40, &
      133, &
      1, &
      2, &
      6, &
      7, &
      19, &
      20, &
      23, &
      25, &
      26, &
      30, &
      31, &
      32, &
      34, &
      35, &
      37, &
      40, &
      41, &
      133, &
      1, &
      2, &
      6, &
      20, &
      21, &
      24, &
      26, &
      27, &
      31, &
      32, &
      35, &
      36, &
      38, &
      41, &
      42, &
      133, &
      1, &
      2, &
      6, &
      22, &
      29, &
      30, &
      33, &
      34, &
      39, &
      133, &
      1, &
      2, &
      6, &
      7, &
      22, &
      23, &
      28, &
      30, &
      31, &
      33, &
      34, &
      35, &
      40, &
      44, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      23, &
      24, &
      29, &
      31, &
      32, &
      34, &
      35, &
      36, &
      39, &
      41, &
      44, &
      45, &
      133, &
      1, &
      2, &
      6, &
      24, &
      25, &
      30, &
      32, &
      35, &
      36, &
      37, &
      39, &
      40, &
      42, &
      45, &
      46, &
      133, &
      1, &
      2, &
      6, &
      25, &
      26, &
      31, &
      36, &
      37, &
      38, &
      40, &
      41, &
      43, &
      46, &
      47, &
      133, &
      1, &
      2, &
      6, &
      26, &
      27, &
      32, &
      37, &
      38, &
      41, &
      42, &
      47, &
      48, &
      133, &
      1, &
      2, &
      6, &
      29, &
      30, &
      33, &
      35, &
      36, &
      39, &
      40, &
      45, &
      50, &
      133, &
      1, &
      2, &
      6, &
      30, &
      31, &
      34, &
      36, &
      37, &
      39, &
      40, &
      41, &
      44, &
      46, &
      50, &
      51, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      31, &
      32, &
      35, &
      37, &
      38, &
      40, &
      41, &
      42, &
      44, &
      45, &
      47, &
      51, &
      52, &
      133, &
      1, &
      2, &
      6, &
      32, &
      36, &
      38, &
      41, &
      42, &
      43, &
      45, &
      46, &
      48, &
      52, &
      53, &
      133, &
      1, &
      2, &
      6, &
      37, &
      42, &
      43, &
      46, &
      47, &
      49, &
      53, &
      54, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      34, &
      35, &
      40, &
      41, &
      44, &
      45, &
      51, &
      55, &
      56, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      23, &
      35, &
      36, &
      39, &
      41, &
      42, &
      44, &
      45, &
      46, &
      50, &
      52, &
      56, &
      57, &
      133, &
      1, &
      2, &
      6, &
      36, &
      37, &
      40, &
      42, &
      43, &
      45, &
      46, &
      47, &
      50, &
      51, &
      53, &
      57, &
      58, &
      133, &
      1, &
      2, &
      6, &
      37, &
      38, &
      41, &
      43, &
      46, &
      47, &
      48, &
      51, &
      52, &
      54, &
      58, &
      59, &
      133, &
      1, &
      2, &
      6, &
      38, &
      42, &
      47, &
      48, &
      49, &
      52, &
      53, &
      59, &
      60, &
      133, &
      1, &
      2, &
      6, &
      43, &
      48, &
      49, &
      53, &
      54, &
      60, &
      133, &
      1, &
      2, &
      6, &
      39, &
      40, &
      45, &
      46, &
      50, &
      51, &
      55, &
      57, &
      61, &
      133, &
      1, &
      2, &
      6, &
      40, &
      41, &
      44, &
      46, &
      47, &
      50, &
      51, &
      52, &
      55, &
      56, &
      58, &
      61, &
      62, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      23, &
      41, &
      42, &
      45, &
      47, &
      48, &
      51, &
      52, &
      53, &
      56, &
      57, &
      59, &
      62, &
      63, &
      133, &
      1, &
      2, &
      6, &
      42, &
      43, &
      46, &
      48, &
      49, &
      52, &
      53, &
      54, &
      57, &
      58, &
      60, &
      63, &
      64, &
      133, &
      1, &
      2, &
      6, &
      43, &
      47, &
      49, &
      53, &
      54, &
      58, &
      59, &
      64, &
      65, &
      133, &
      1, &
      2, &
      6, &
      44, &
      50, &
      51, &
      55, &
      56, &
      61, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      23, &
      44, &
      45, &
      51, &
      52, &
      55, &
      56, &
      57, &
      62, &
      66, &
      133, &
      1, &
      2, &
      6, &
      45, &
      46, &
      50, &
      52, &
      53, &
      56, &
      57, &
      58, &
      61, &
      63, &
      66, &
      67, &
      133, &
      1, &
      2, &
      6, &
      46, &
      47, &
      51, &
      53, &
      54, &
      57, &
      58, &
      59, &
      61, &
      62, &
      64, &
      67, &
      68, &
      133, &
      1, &
      2, &
      6, &
      47, &
      48, &
      52, &
      54, &
      58, &
      59, &
      60, &
      62, &
      63, &
      65, &
      68, &
      69, &
      133, &
      1, &
      2, &
      6, &
      48, &
      49, &
      53, &
      59, &
      60, &
      63, &
      64, &
      69, &
      70, &
      133, &
      1, &
      2, &
      6, &
      50, &
      51, &
      55, &
      57, &
      58, &
      61, &
      62, &
      67, &
      71, &
      133, &
      1, &
      2, &
      6, &
      51, &
      52, &
      56, &
      58, &
      59, &
      61, &
      62, &
      63, &
      66, &
      68, &
      71, &
      72, &
      133, &
      1, &
      2, &
      6, &
      52, &
      53, &
      57, &
      59, &
      60, &
      62, &
      63, &
      64, &
      66, &
      67, &
      69, &
      72, &
      73, &
      133, &
      1, &
      2, &
      6, &
      53, &
      54, &
      58, &
      60, &
      63, &
      64, &
      65, &
      67, &
      68, &
      70, &
      73, &
      74, &
      133, &
      1, &
      2, &
      6, &
      54, &
      59, &
      64, &
      65, &
      68, &
      69, &
      74, &
      75, &
      133, &
      1, &
      2, &
      6, &
      56, &
      57, &
      62, &
      63, &
      66, &
      67, &
      72, &
      77, &
      133, &
      1, &
      2, &
      6, &
      57, &
      58, &
      61, &
      63, &
      64, &
      66, &
      67, &
      68, &
      71, &
      73, &
      77, &
      78, &
      133, &
      1, &
      2, &
      6, &
      58, &
      59, &
      62, &
      64, &
      65, &
      67, &
      68, &
      69, &
      71, &
      72, &
      74, &
      78, &
      79, &
      133, &
      1, &
      2, &
      6, &
      59, &
      60, &
      63, &
      65, &
      68, &
      69, &
      70, &
      72, &
      73, &
      75, &
      79, &
      80, &
      133, &
      1, &
      2, &
      6, &
      60, &
      64, &
      69, &
      70, &
      73, &
      74, &
      76, &
      80, &
      81, &
      133, &
      1, &
      2, &
      6, &
      61, &
      62, &
      67, &
      68, &
      71, &
      72, &
      78, &
      133, &
      1, &
      2, &
      6, &
      62, &
      63, &
      66, &
      68, &
      69, &
      71, &
      72, &
      73, &
      77, &
      79, &
      83, &
      133, &
      1, &
      2, &
      6, &
      63, &
      64, &
      67, &
      69, &
      70, &
      72, &
      73, &
      74, &
      77, &
      78, &
      80, &
      83, &
      84, &
      133, &
      1, &
      2, &
      6, &
      64, &
      65, &
      68, &
      70, &
      73, &
      74, &
      75, &
      78, &
      79, &
      81, &
      84, &
      85, &
      133, &
      1, &
      2, &
      6, &
      65, &
      69, &
      74, &
      75, &
      76, &
      79, &
      80, &
      82, &
      85, &
      86, &
      133, &
      1, &
      2, &
      6, &
      70, &
      75, &
      76, &
      80, &
      81, &
      86, &
      87, &
      133, &
      1, &
      2, &
      6, &
      66, &
      67, &
      72, &
      73, &
      77, &
      78, &
      83, &
      88, &
      133, &
      1, &
      2, &
      6, &
      67, &
      68, &
      71, &
      73, &
      74, &
      77, &
      78, &
      79, &
      84, &
      88, &
      89, &
      133, &
      1, &
      2, &
      6, &
      68, &
      69, &
      72, &
      74, &
      75, &
      78, &
      79, &
      80, &
      83, &
      85, &
      89, &
      90, &
      133, &
      1, &
      2, &
      6, &
      69, &
      70, &
      73, &
      75, &
      76, &
      79, &
      80, &
      81, &
      83, &
      84, &
      86, &
      90, &
      91, &
      133, &
      1, &
      2, &
      6, &
      70, &
      74, &
      76, &
      80, &
      81, &
      82, &
      84, &
      85, &
      87, &
      91, &
      92, &
      133, &
      1, &
      2, &
      6, &
      75, &
      81, &
      82, &
      85, &
      86, &
      92, &
      93, &
      133, &
      1, &
      2, &
      6, &
      72, &
      73, &
      77, &
      79, &
      80, &
      83, &
      84, &
      88, &
      90, &
      95, &
      133, &
      1, &
      2, &
      6, &
      73, &
      74, &
      78, &
      80, &
      81, &
      83, &
      84, &
      85, &
      88, &
      89, &
      91, &
      95, &
      96, &
      133, &
      1, &
      2, &
      6, &
      74, &
      75, &
      79, &
      81, &
      82, &
      84, &
      85, &
      86, &
      89, &
      90, &
      92, &
      96, &
      97, &
      133, &
      1, &
      2, &
      6, &
      75, &
      76, &
      80, &
      82, &
      85, &
      86, &
      87, &
      90, &
      91, &
      93, &
      97, &
      98, &
      133, &
      1, &
      2, &
      6, &
      76, &
      81, &
      86, &
      87, &
      91, &
      92, &
      94, &
      98, &
      99, &
      133, &
      1, &
      2, &
      6, &
      77, &
      78, &
      83, &
      84, &
      88, &
      89, &
      95, &
      101, &
      133, &
      1, &
      2, &
      6, &
      78, &
      79, &
      84, &
      85, &
      88, &
      89, &
      90, &
      96, &
      101, &
      102, &
      133, &
      1, &
      2, &
      6, &
      79, &
      80, &
      83, &
      85, &
      86, &
      89, &
      90, &
      91, &
      95, &
      97, &
      102, &
      103, &
      133, &
      1, &
      2, &
      6, &
      80, &
      81, &
      84, &
      86, &
      87, &
      90, &
      91, &
      92, &
      95, &
      96, &
      98, &
      103, &
      104, &
      133, &
      1, &
      2, &
      6, &
      81, &
      82, &
      85, &
      87, &
      91, &
      92, &
      93, &
      96, &
      97, &
      99, &
      104, &
      105, &
      133, &
      1, &
      2, &
      6, &
      82, &
      86, &
      92, &
      93, &
      94, &
      97, &
      98, &
      100, &
      105, &
      106, &
      133, &
      1, &
      2, &
      6, &
      87, &
      93, &
      94, &
      98, &
      99, &
      106, &
      133, &
      1, &
      2, &
      6, &
      83, &
      84, &
      88, &
      90, &
      91, &
      95, &
      96, &
      101, &
      103, &
      107, &
      108, &
      133, &
      1, &
      2, &
      6, &
      84, &
      85, &
      89, &
      91, &
      92, &
      95, &
      96, &
      97, &
      101, &
      102, &
      104, &
      108, &
      109, &
      133, &
      1, &
      2, &
      6, &
      85, &
      86, &
      90, &
      92, &
      93, &
      96, &
      97, &
      98, &
      102, &
      103, &
      105, &
      109, &
      110, &
      133, &
      1, &
      2, &
      6, &
      86, &
      87, &
      91, &
      93, &
      94, &
      97, &
      98, &
      99, &
      103, &
      104, &
      106, &
      110, &
      111, &
      133, &
      1, &
      2, &
      6, &
      87, &
      92, &
      94, &
      98, &
      99, &
      100, &
      104, &
      105, &
      111, &
      112, &
      133, &
      1, &
      2, &
      6, &
      93, &
      99, &
      100, &
      105, &
      106, &
      112, &
      113, &
      133, &
      1, &
      2, &
      6, &
      88, &
      89, &
      95, &
      96, &
      101, &
      102, &
      108, &
      114, &
      133, &
      1, &
      2, &
      6, &
      89, &
      90, &
      96, &
      97, &
      101, &
      102, &
      103, &
      107, &
      109, &
      114, &
      115, &
      133, &
      1, &
      2, &
      6, &
      90, &
      91, &
      95, &
      97, &
      98, &
      102, &
      103, &
      104, &
      107, &
      108, &
      110, &
      115, &
      116, &
      133, &
      1, &
      2, &
      6, &
      91, &
      92, &
      96, &
      98, &
      99, &
      103, &
      104, &
      105, &
      108, &
      109, &
      111, &
      116, &
      117, &
      133, &
      1, &
      2, &
      6, &
      92, &
      93, &
      97, &
      99, &
      100, &
      104, &
      105, &
      106, &
      109, &
      110, &
      112, &
      117, &
      118, &
      133, &
      1, &
      2, &
      6, &
      93, &
      94, &
      98, &
      100, &
      105, &
      106, &
      110, &
      111, &
      113, &
      118, &
      119, &
      133, &
      1, &
      2, &
      6, &
      95, &
      102, &
      103, &
      107, &
      108, &
      115, &
      120, &
      133, &
      1, &
      2, &
      6, &
      95, &
      96, &
      101, &
      103, &
      104, &
      107, &
      108, &
      109, &
      114, &
      116, &
      120, &
      121, &
      133, &
      1, &
      2, &
      6, &
      96, &
      97, &
      102, &
      104, &
      105, &
      108, &
      109, &
      110, &
      114, &
      115, &
      117, &
      121, &
      122, &
      133, &
      1, &
      2, &
      6, &
      97, &
      98, &
      103, &
      105, &
      106, &
      109, &
      110, &
      111, &
      115, &
      116, &
      118, &
      122, &
      123, &
      133, &
      1, &
      2, &
      6, &
      98, &
      99, &
      104, &
      106, &
      110, &
      111, &
      112, &
      116, &
      117, &
      119, &
      123, &
      124, &
      133, &
      1, &
      2, &
      6, &
      99, &
      100, &
      105, &
      111, &
      112, &
      113, &
      117, &
      118, &
      124, &
      125, &
      133, &
      1, &
      2, &
      6, &
      100, &
      106, &
      112, &
      113, &
      118, &
      119, &
      125, &
      133, &
      1, &
      2, &
      6, &
      101, &
      102, &
      108, &
      109, &
      114, &
      115, &
      121, &
      126, &
      127, &
      133, &
      1, &
      2, &
      6, &
      102, &
      103, &
      107, &
      109, &
      110, &
      114, &
      115, &
      116, &
      120, &
      122, &
      127, &
      128, &
      133, &
      1, &
      2, &
      6, &
      103, &
      104, &
      108, &
      110, &
      111, &
      115, &
      116, &
      117, &
      120, &
      121, &
      123, &
      128, &
      129, &
      133, &
      1, &
      2, &
      6, &
      104, &
      105, &
      109, &
      111, &
      112, &
      116, &
      117, &
      118, &
      121, &
      122, &
      124, &
      129, &
      130, &
      133, &
      1, &
      2, &
      6, &
      105, &
      106, &
      110, &
      112, &
      113, &
      117, &
      118, &
      119, &
      122, &
      123, &
      125, &
      130, &
      131, &
      133, &
      1, &
      2, &
      6, &
      106, &
      111, &
      113, &
      118, &
      119, &
      123, &
      124, &
      131, &
      132, &
      133, &
      1, &
      2, &
      6, &
      107, &
      108, &
      115, &
      116, &
      120, &
      121, &
      126, &
      128, &
      133, &
      1, &
      2, &
      6, &
      108, &
      109, &
      114, &
      116, &
      117, &
      120, &
      121, &
      122, &
      126, &
      127, &
      129, &
      133, &
      1, &
      2, &
      6, &
      109, &
      110, &
      115, &
      117, &
      118, &
      121, &
      122, &
      123, &
      127, &
      128, &
      130, &
      133, &
      1, &
      2, &
      6, &
      110, &
      111, &
      116, &
      118, &
      119, &
      122, &
      123, &
      124, &
      128, &
      129, &
      131, &
      133, &
      1, &
      2, &
      6, &
      111, &
      112, &
      117, &
      119, &
      123, &
      124, &
      125, &
      129, &
      130, &
      132, &
      133, &
      1, &
      2, &
      6, &
      112, &
      113, &
      118, &
      124, &
      125, &
      130, &
      131, &
      133, &
      1, &
      2, &
      6, &
      114, &
      120, &
      121, &
      126, &
      127, &
      133, &
      1, &
      2, &
      6, &
      114, &
      115, &
      121, &
      122, &
      126, &
      127, &
      128, &
      133, &
      1, &
      2, &
      6, &
      115, &
      116, &
      120, &
      122, &
      123, &
      127, &
      128, &
      129, &
      133, &
      1, &
      2, &
      6, &
      116, &
      117, &
      121, &
      123, &
      124, &
      128, &
      129, &
      130, &
      133, &
      1, &
      2, &
      6, &
      117, &
      118, &
      122, &
      124, &
      125, &
      129, &
      130, &
      131, &
      133, &
      1, &
      2, &
      6, &
      118, &
      119, &
      123, &
      125, &
      130, &
      131, &
      132, &
      133, &
      1, &
      2, &
      6, &
      119, &
      124, &
      131, &
      132, &
      133, &
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
      134  ]

    csr_jac_row_count = [ &
      1, &
      133, &
      265, &
      276, &
      283, &
      290, &
      422, &
      440, &
      451, &
      462, &
      471, &
      483, &
      496, &
      516, &
      529, &
      543, &
      553, &
      556, &
      570, &
      586, &
      601, &
      612, &
      625, &
      645, &
      662, &
      677, &
      690, &
      699, &
      708, &
      721, &
      738, &
      756, &
      772, &
      782, &
      797, &
      815, &
      831, &
      846, &
      859, &
      872, &
      888, &
      907, &
      922, &
      934, &
      949, &
      968, &
      985, &
      1001, &
      1014, &
      1024, &
      1037, &
      1054, &
      1074, &
      1091, &
      1104, &
      1114, &
      1130, &
      1146, &
      1163, &
      1179, &
      1192, &
      1205, &
      1221, &
      1238, &
      1254, &
      1266, &
      1278, &
      1294, &
      1311, &
      1327, &
      1340, &
      1351, &
      1366, &
      1383, &
      1399, &
      1413, &
      1424, &
      1436, &
      1451, &
      1467, &
      1484, &
      1499, &
      1510, &
      1524, &
      1541, &
      1558, &
      1574, &
      1587, &
      1599, &
      1613, &
      1629, &
      1646, &
      1662, &
      1676, &
      1686, &
      1701, &
      1718, &
      1735, &
      1752, &
      1766, &
      1777, &
      1789, &
      1804, &
      1821, &
      1838, &
      1855, &
      1870, &
      1881, &
      1897, &
      1914, &
      1931, &
      1947, &
      1961, &
      1972, &
      1985, &
      2001, &
      2018, &
      2035, &
      2052, &
      2065, &
      2077, &
      2092, &
      2107, &
      2122, &
      2136, &
      2147, &
      2156, &
      2167, &
      2179, &
      2191, &
      2203, &
      2214, &
      2222, &
      2355, &
      2489  ]
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
