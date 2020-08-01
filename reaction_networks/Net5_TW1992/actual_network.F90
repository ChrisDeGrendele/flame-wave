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

  integer, parameter :: nrates = 676


  ! For each rate, we need: rate, drate/dT, screening, dscreening/dT
  integer, parameter :: num_rate_groups = 4

  ! Number of reaclib rates
  integer, parameter :: nrat_reaclib = 676
  integer, parameter :: number_reaclib_sets = 833

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
  integer, parameter :: jc13   = 8
  integer, parameter :: jc14   = 9
  integer, parameter :: jn13   = 10
  integer, parameter :: jn14   = 11
  integer, parameter :: jn15   = 12
  integer, parameter :: jo16   = 13
  integer, parameter :: jo17   = 14
  integer, parameter :: jo18   = 15
  integer, parameter :: jo19   = 16
  integer, parameter :: jf18   = 17
  integer, parameter :: jf19   = 18
  integer, parameter :: jf20   = 19
  integer, parameter :: jf21   = 20
  integer, parameter :: jne19   = 21
  integer, parameter :: jne20   = 22
  integer, parameter :: jne21   = 23
  integer, parameter :: jne22   = 24
  integer, parameter :: jne23   = 25
  integer, parameter :: jne24   = 26
  integer, parameter :: jna20   = 27
  integer, parameter :: jna21   = 28
  integer, parameter :: jna22   = 29
  integer, parameter :: jna23   = 30
  integer, parameter :: jna24   = 31
  integer, parameter :: jmg22   = 32
  integer, parameter :: jmg23   = 33
  integer, parameter :: jmg24   = 34
  integer, parameter :: jmg25   = 35
  integer, parameter :: jmg26   = 36
  integer, parameter :: jmg27   = 37
  integer, parameter :: jal25   = 38
  integer, parameter :: jal26   = 39
  integer, parameter :: jal27   = 40
  integer, parameter :: jal28   = 41
  integer, parameter :: jal29   = 42
  integer, parameter :: jsi27   = 43
  integer, parameter :: jsi28   = 44
  integer, parameter :: jsi29   = 45
  integer, parameter :: jsi30   = 46
  integer, parameter :: jsi31   = 47
  integer, parameter :: jsi32   = 48
  integer, parameter :: jp29   = 49
  integer, parameter :: jp30   = 50
  integer, parameter :: jp31   = 51
  integer, parameter :: jp32   = 52
  integer, parameter :: jp33   = 53
  integer, parameter :: js30   = 54
  integer, parameter :: js31   = 55
  integer, parameter :: js32   = 56
  integer, parameter :: js33   = 57
  integer, parameter :: js34   = 58
  integer, parameter :: js35   = 59
  integer, parameter :: jcl33   = 60
  integer, parameter :: jcl34   = 61
  integer, parameter :: jcl35   = 62
  integer, parameter :: jcl36   = 63
  integer, parameter :: jcl37   = 64
  integer, parameter :: jar35   = 65
  integer, parameter :: jar36   = 66
  integer, parameter :: jar37   = 67
  integer, parameter :: jar38   = 68
  integer, parameter :: jar39   = 69
  integer, parameter :: jk37   = 70
  integer, parameter :: jk38   = 71
  integer, parameter :: jk39   = 72
  integer, parameter :: jk40   = 73
  integer, parameter :: jk41   = 74
  integer, parameter :: jk42   = 75
  integer, parameter :: jca39   = 76
  integer, parameter :: jca40   = 77
  integer, parameter :: jca41   = 78
  integer, parameter :: jca42   = 79
  integer, parameter :: jca43   = 80
  integer, parameter :: jca44   = 81
  integer, parameter :: jsc42   = 82
  integer, parameter :: jsc43   = 83
  integer, parameter :: jsc44   = 84
  integer, parameter :: jsc45   = 85
  integer, parameter :: jsc46   = 86
  integer, parameter :: jti43   = 87
  integer, parameter :: jti44   = 88
  integer, parameter :: jti45   = 89
  integer, parameter :: jti46   = 90
  integer, parameter :: jti47   = 91
  integer, parameter :: jti48   = 92
  integer, parameter :: jti49   = 93
  integer, parameter :: jv46   = 94
  integer, parameter :: jv47   = 95
  integer, parameter :: jv48   = 96
  integer, parameter :: jv49   = 97
  integer, parameter :: jv50   = 98
  integer, parameter :: jv51   = 99
  integer, parameter :: jcr47   = 100
  integer, parameter :: jcr48   = 101
  integer, parameter :: jcr49   = 102
  integer, parameter :: jcr50   = 103
  integer, parameter :: jcr51   = 104
  integer, parameter :: jcr52   = 105
  integer, parameter :: jmn49   = 106
  integer, parameter :: jmn50   = 107
  integer, parameter :: jmn51   = 108
  integer, parameter :: jmn52   = 109
  integer, parameter :: jmn53   = 110
  integer, parameter :: jmn54   = 111
  integer, parameter :: jmn55   = 112
  integer, parameter :: jfe51   = 113
  integer, parameter :: jfe52   = 114
  integer, parameter :: jfe53   = 115
  integer, parameter :: jfe54   = 116
  integer, parameter :: jfe55   = 117
  integer, parameter :: jfe56   = 118
  integer, parameter :: jco53   = 119
  integer, parameter :: jco54   = 120
  integer, parameter :: jco55   = 121
  integer, parameter :: jco56   = 122
  integer, parameter :: jco57   = 123
  integer, parameter :: jco58   = 124
  integer, parameter :: jni54   = 125
  integer, parameter :: jni55   = 126
  integer, parameter :: jni56   = 127
  integer, parameter :: jni57   = 128
  integer, parameter :: jni58   = 129
  integer, parameter :: jni59   = 130
  integer, parameter :: jni60   = 131

  ! Reactions
  integer, parameter :: k_n__p__weak__wc12   = 1
  integer, parameter :: k_t__he3__weak__wc12   = 2
  integer, parameter :: k_he3__t__weak__electron_capture   = 3
  integer, parameter :: k_c14__n14__weak__wc12   = 4
  integer, parameter :: k_n13__c13__weak__wc12   = 5
  integer, parameter :: k_o19__f19__weak__wc12   = 6
  integer, parameter :: k_f18__o18__weak__wc12   = 7
  integer, parameter :: k_f20__ne20__weak__wc12   = 8
  integer, parameter :: k_f21__ne21__weak__wc12   = 9
  integer, parameter :: k_ne19__f19__weak__wc12   = 10
  integer, parameter :: k_ne23__na23__weak__wc12   = 11
  integer, parameter :: k_ne24__na24__weak__wc12   = 12
  integer, parameter :: k_na20__ne20__weak__wc12   = 13
  integer, parameter :: k_na21__ne21__weak__wc12   = 14
  integer, parameter :: k_na22__ne22__weak__wc12   = 15
  integer, parameter :: k_na24__mg24__weak__wc12   = 16
  integer, parameter :: k_mg22__na22__weak__wc12   = 17
  integer, parameter :: k_mg23__na23__weak__wc12   = 18
  integer, parameter :: k_mg27__al27__weak__wc12   = 19
  integer, parameter :: k_al25__mg25__weak__wc12   = 20
  integer, parameter :: k_al26__mg26__weak__wc12   = 21
  integer, parameter :: k_al28__si28__weak__wc12   = 22
  integer, parameter :: k_al29__si29__weak__wc12   = 23
  integer, parameter :: k_si27__al27__weak__wc12   = 24
  integer, parameter :: k_si31__p31__weak__wc12   = 25
  integer, parameter :: k_si32__p32__weak__wc12   = 26
  integer, parameter :: k_p29__si29__weak__wc12   = 27
  integer, parameter :: k_p30__si30__weak__wc12   = 28
  integer, parameter :: k_p32__s32__weak__wc12   = 29
  integer, parameter :: k_p33__s33__weak__wc12   = 30
  integer, parameter :: k_s30__p30__weak__wc12   = 31
  integer, parameter :: k_s31__p31__weak__wc12   = 32
  integer, parameter :: k_s35__cl35__weak__wc12   = 33
  integer, parameter :: k_cl33__s33__weak__wc12   = 34
  integer, parameter :: k_cl34__s34__weak__wc12   = 35
  integer, parameter :: k_cl36__ar36__weak__wc12   = 36
  integer, parameter :: k_ar35__cl35__weak__wc12   = 37
  integer, parameter :: k_ar37__cl37__weak__wc12   = 38
  integer, parameter :: k_ar39__k39__weak__wc12   = 39
  integer, parameter :: k_k37__ar37__weak__wc12   = 40
  integer, parameter :: k_k38__ar38__weak__wc12   = 41
  integer, parameter :: k_k40__ca40__weak__wc12   = 42
  integer, parameter :: k_k42__ca42__weak__wc12   = 43
  integer, parameter :: k_ca39__k39__weak__wc12   = 44
  integer, parameter :: k_ca41__k41__weak__wc12   = 45
  integer, parameter :: k_sc42__ca42__weak__wc12   = 46
  integer, parameter :: k_sc43__ca43__weak__wc12   = 47
  integer, parameter :: k_sc44__ca44__weak__wc12   = 48
  integer, parameter :: k_sc46__ti46__weak__wc12   = 49
  integer, parameter :: k_ti43__sc43__weak__wc12   = 50
  integer, parameter :: k_ti44__sc44__weak__wc12   = 51
  integer, parameter :: k_ti45__sc45__weak__wc12   = 52
  integer, parameter :: k_v46__ti46__weak__wc12   = 53
  integer, parameter :: k_v47__ti47__weak__wc12   = 54
  integer, parameter :: k_v48__ti48__weak__wc12   = 55
  integer, parameter :: k_v49__ti49__weak__wc12   = 56
  integer, parameter :: k_cr47__v47__weak__wc12   = 57
  integer, parameter :: k_cr48__v48__weak__wc12   = 58
  integer, parameter :: k_cr49__v49__weak__wc12   = 59
  integer, parameter :: k_cr51__v51__weak__wc12   = 60
  integer, parameter :: k_mn49__cr49__weak__wc12   = 61
  integer, parameter :: k_mn50__cr50__weak__wc12   = 62
  integer, parameter :: k_mn51__cr51__weak__wc12   = 63
  integer, parameter :: k_mn52__cr52__weak__wc12   = 64
  integer, parameter :: k_fe51__mn51__weak__wc12   = 65
  integer, parameter :: k_fe52__mn52__weak__wc12   = 66
  integer, parameter :: k_fe53__mn53__weak__wc12   = 67
  integer, parameter :: k_fe55__mn55__weak__wc12   = 68
  integer, parameter :: k_co53__fe53__weak__wc12   = 69
  integer, parameter :: k_co54__fe54__weak__wc12   = 70
  integer, parameter :: k_co55__fe55__weak__wc12   = 71
  integer, parameter :: k_co56__fe56__weak__wc12   = 72
  integer, parameter :: k_ni54__co54__weak__wc12   = 73
  integer, parameter :: k_ni55__co55__weak__wc12   = 74
  integer, parameter :: k_ni56__co56__weak__wc12   = 75
  integer, parameter :: k_ni57__co57__weak__wc12   = 76
  integer, parameter :: k_na20__he4_o16__weak__wc12   = 77
  integer, parameter :: k_n_p__d   = 78
  integer, parameter :: k_p_p__d__weak__bet_pos_   = 79
  integer, parameter :: k_p_p__d__weak__electron_capture   = 80
  integer, parameter :: k_n_d__t   = 81
  integer, parameter :: k_p_d__he3   = 82
  integer, parameter :: k_d_d__he4   = 83
  integer, parameter :: k_p_t__he4   = 84
  integer, parameter :: k_n_he3__he4   = 85
  integer, parameter :: k_p_he3__he4__weak__bet_pos_   = 86
  integer, parameter :: k_n_c12__c13   = 87
  integer, parameter :: k_p_c12__n13   = 88
  integer, parameter :: k_he4_c12__o16   = 89
  integer, parameter :: k_n_c13__c14   = 90
  integer, parameter :: k_p_c13__n14   = 91
  integer, parameter :: k_p_c14__n15   = 92
  integer, parameter :: k_he4_c14__o18   = 93
  integer, parameter :: k_n_n13__n14   = 94
  integer, parameter :: k_n_n14__n15   = 95
  integer, parameter :: k_he4_n14__f18   = 96
  integer, parameter :: k_p_n15__o16   = 97
  integer, parameter :: k_he4_n15__f19   = 98
  integer, parameter :: k_n_o16__o17   = 99
  integer, parameter :: k_he4_o16__ne20   = 100
  integer, parameter :: k_n_o17__o18   = 101
  integer, parameter :: k_p_o17__f18   = 102
  integer, parameter :: k_he4_o17__ne21   = 103
  integer, parameter :: k_n_o18__o19   = 104
  integer, parameter :: k_p_o18__f19   = 105
  integer, parameter :: k_he4_o18__ne22   = 106
  integer, parameter :: k_p_o19__f20   = 107
  integer, parameter :: k_he4_o19__ne23   = 108
  integer, parameter :: k_n_f18__f19   = 109
  integer, parameter :: k_p_f18__ne19   = 110
  integer, parameter :: k_he4_f18__na22   = 111
  integer, parameter :: k_n_f19__f20   = 112
  integer, parameter :: k_p_f19__ne20   = 113
  integer, parameter :: k_he4_f19__na23   = 114
  integer, parameter :: k_n_f20__f21   = 115
  integer, parameter :: k_p_f20__ne21   = 116
  integer, parameter :: k_he4_f20__na24   = 117
  integer, parameter :: k_p_f21__ne22   = 118
  integer, parameter :: k_n_ne19__ne20   = 119
  integer, parameter :: k_p_ne19__na20   = 120
  integer, parameter :: k_he4_ne19__mg23   = 121
  integer, parameter :: k_n_ne20__ne21   = 122
  integer, parameter :: k_p_ne20__na21   = 123
  integer, parameter :: k_he4_ne20__mg24   = 124
  integer, parameter :: k_n_ne21__ne22   = 125
  integer, parameter :: k_p_ne21__na22   = 126
  integer, parameter :: k_he4_ne21__mg25   = 127
  integer, parameter :: k_n_ne22__ne23   = 128
  integer, parameter :: k_p_ne22__na23   = 129
  integer, parameter :: k_he4_ne22__mg26   = 130
  integer, parameter :: k_n_ne23__ne24   = 131
  integer, parameter :: k_p_ne23__na24   = 132
  integer, parameter :: k_he4_ne23__mg27   = 133
  integer, parameter :: k_n_na20__na21   = 134
  integer, parameter :: k_n_na21__na22   = 135
  integer, parameter :: k_p_na21__mg22   = 136
  integer, parameter :: k_he4_na21__al25   = 137
  integer, parameter :: k_n_na22__na23   = 138
  integer, parameter :: k_p_na22__mg23   = 139
  integer, parameter :: k_he4_na22__al26   = 140
  integer, parameter :: k_n_na23__na24   = 141
  integer, parameter :: k_p_na23__mg24   = 142
  integer, parameter :: k_he4_na23__al27   = 143
  integer, parameter :: k_p_na24__mg25   = 144
  integer, parameter :: k_he4_na24__al28   = 145
  integer, parameter :: k_n_mg22__mg23   = 146
  integer, parameter :: k_n_mg23__mg24   = 147
  integer, parameter :: k_he4_mg23__si27   = 148
  integer, parameter :: k_n_mg24__mg25   = 149
  integer, parameter :: k_p_mg24__al25   = 150
  integer, parameter :: k_he4_mg24__si28   = 151
  integer, parameter :: k_n_mg25__mg26   = 152
  integer, parameter :: k_p_mg25__al26   = 153
  integer, parameter :: k_he4_mg25__si29   = 154
  integer, parameter :: k_n_mg26__mg27   = 155
  integer, parameter :: k_p_mg26__al27   = 156
  integer, parameter :: k_he4_mg26__si30   = 157
  integer, parameter :: k_p_mg27__al28   = 158
  integer, parameter :: k_he4_mg27__si31   = 159
  integer, parameter :: k_n_al25__al26   = 160
  integer, parameter :: k_he4_al25__p29   = 161
  integer, parameter :: k_n_al26__al27   = 162
  integer, parameter :: k_p_al26__si27   = 163
  integer, parameter :: k_he4_al26__p30   = 164
  integer, parameter :: k_n_al27__al28   = 165
  integer, parameter :: k_p_al27__si28   = 166
  integer, parameter :: k_he4_al27__p31   = 167
  integer, parameter :: k_n_al28__al29   = 168
  integer, parameter :: k_p_al28__si29   = 169
  integer, parameter :: k_he4_al28__p32   = 170
  integer, parameter :: k_p_al29__si30   = 171
  integer, parameter :: k_he4_al29__p33   = 172
  integer, parameter :: k_n_si27__si28   = 173
  integer, parameter :: k_he4_si27__s31   = 174
  integer, parameter :: k_n_si28__si29   = 175
  integer, parameter :: k_p_si28__p29   = 176
  integer, parameter :: k_he4_si28__s32   = 177
  integer, parameter :: k_n_si29__si30   = 178
  integer, parameter :: k_p_si29__p30   = 179
  integer, parameter :: k_he4_si29__s33   = 180
  integer, parameter :: k_n_si30__si31   = 181
  integer, parameter :: k_p_si30__p31   = 182
  integer, parameter :: k_he4_si30__s34   = 183
  integer, parameter :: k_n_si31__si32   = 184
  integer, parameter :: k_p_si31__p32   = 185
  integer, parameter :: k_he4_si31__s35   = 186
  integer, parameter :: k_p_si32__p33   = 187
  integer, parameter :: k_n_p29__p30   = 188
  integer, parameter :: k_p_p29__s30   = 189
  integer, parameter :: k_he4_p29__cl33   = 190
  integer, parameter :: k_n_p30__p31   = 191
  integer, parameter :: k_p_p30__s31   = 192
  integer, parameter :: k_he4_p30__cl34   = 193
  integer, parameter :: k_n_p31__p32   = 194
  integer, parameter :: k_p_p31__s32   = 195
  integer, parameter :: k_he4_p31__cl35   = 196
  integer, parameter :: k_n_p32__p33   = 197
  integer, parameter :: k_p_p32__s33   = 198
  integer, parameter :: k_he4_p32__cl36   = 199
  integer, parameter :: k_p_p33__s34   = 200
  integer, parameter :: k_he4_p33__cl37   = 201
  integer, parameter :: k_n_s30__s31   = 202
  integer, parameter :: k_n_s31__s32   = 203
  integer, parameter :: k_he4_s31__ar35   = 204
  integer, parameter :: k_n_s32__s33   = 205
  integer, parameter :: k_p_s32__cl33   = 206
  integer, parameter :: k_he4_s32__ar36   = 207
  integer, parameter :: k_n_s33__s34   = 208
  integer, parameter :: k_p_s33__cl34   = 209
  integer, parameter :: k_he4_s33__ar37   = 210
  integer, parameter :: k_n_s34__s35   = 211
  integer, parameter :: k_p_s34__cl35   = 212
  integer, parameter :: k_he4_s34__ar38   = 213
  integer, parameter :: k_p_s35__cl36   = 214
  integer, parameter :: k_he4_s35__ar39   = 215
  integer, parameter :: k_n_cl33__cl34   = 216
  integer, parameter :: k_he4_cl33__k37   = 217
  integer, parameter :: k_n_cl34__cl35   = 218
  integer, parameter :: k_p_cl34__ar35   = 219
  integer, parameter :: k_he4_cl34__k38   = 220
  integer, parameter :: k_n_cl35__cl36   = 221
  integer, parameter :: k_p_cl35__ar36   = 222
  integer, parameter :: k_he4_cl35__k39   = 223
  integer, parameter :: k_n_cl36__cl37   = 224
  integer, parameter :: k_p_cl36__ar37   = 225
  integer, parameter :: k_he4_cl36__k40   = 226
  integer, parameter :: k_p_cl37__ar38   = 227
  integer, parameter :: k_he4_cl37__k41   = 228
  integer, parameter :: k_n_ar35__ar36   = 229
  integer, parameter :: k_he4_ar35__ca39   = 230
  integer, parameter :: k_n_ar36__ar37   = 231
  integer, parameter :: k_p_ar36__k37   = 232
  integer, parameter :: k_he4_ar36__ca40   = 233
  integer, parameter :: k_n_ar37__ar38   = 234
  integer, parameter :: k_p_ar37__k38   = 235
  integer, parameter :: k_he4_ar37__ca41   = 236
  integer, parameter :: k_n_ar38__ar39   = 237
  integer, parameter :: k_p_ar38__k39   = 238
  integer, parameter :: k_he4_ar38__ca42   = 239
  integer, parameter :: k_p_ar39__k40   = 240
  integer, parameter :: k_he4_ar39__ca43   = 241
  integer, parameter :: k_n_k37__k38   = 242
  integer, parameter :: k_n_k38__k39   = 243
  integer, parameter :: k_p_k38__ca39   = 244
  integer, parameter :: k_he4_k38__sc42   = 245
  integer, parameter :: k_n_k39__k40   = 246
  integer, parameter :: k_p_k39__ca40   = 247
  integer, parameter :: k_he4_k39__sc43   = 248
  integer, parameter :: k_n_k40__k41   = 249
  integer, parameter :: k_p_k40__ca41   = 250
  integer, parameter :: k_he4_k40__sc44   = 251
  integer, parameter :: k_n_k41__k42   = 252
  integer, parameter :: k_p_k41__ca42   = 253
  integer, parameter :: k_he4_k41__sc45   = 254
  integer, parameter :: k_p_k42__ca43   = 255
  integer, parameter :: k_he4_k42__sc46   = 256
  integer, parameter :: k_n_ca39__ca40   = 257
  integer, parameter :: k_he4_ca39__ti43   = 258
  integer, parameter :: k_n_ca40__ca41   = 259
  integer, parameter :: k_he4_ca40__ti44   = 260
  integer, parameter :: k_n_ca41__ca42   = 261
  integer, parameter :: k_p_ca41__sc42   = 262
  integer, parameter :: k_he4_ca41__ti45   = 263
  integer, parameter :: k_n_ca42__ca43   = 264
  integer, parameter :: k_p_ca42__sc43   = 265
  integer, parameter :: k_he4_ca42__ti46   = 266
  integer, parameter :: k_n_ca43__ca44   = 267
  integer, parameter :: k_p_ca43__sc44   = 268
  integer, parameter :: k_he4_ca43__ti47   = 269
  integer, parameter :: k_p_ca44__sc45   = 270
  integer, parameter :: k_he4_ca44__ti48   = 271
  integer, parameter :: k_n_sc42__sc43   = 272
  integer, parameter :: k_p_sc42__ti43   = 273
  integer, parameter :: k_he4_sc42__v46   = 274
  integer, parameter :: k_n_sc43__sc44   = 275
  integer, parameter :: k_p_sc43__ti44   = 276
  integer, parameter :: k_he4_sc43__v47   = 277
  integer, parameter :: k_n_sc44__sc45   = 278
  integer, parameter :: k_p_sc44__ti45   = 279
  integer, parameter :: k_he4_sc44__v48   = 280
  integer, parameter :: k_n_sc45__sc46   = 281
  integer, parameter :: k_p_sc45__ti46   = 282
  integer, parameter :: k_he4_sc45__v49   = 283
  integer, parameter :: k_p_sc46__ti47   = 284
  integer, parameter :: k_he4_sc46__v50   = 285
  integer, parameter :: k_n_ti43__ti44   = 286
  integer, parameter :: k_he4_ti43__cr47   = 287
  integer, parameter :: k_n_ti44__ti45   = 288
  integer, parameter :: k_he4_ti44__cr48   = 289
  integer, parameter :: k_n_ti45__ti46   = 290
  integer, parameter :: k_p_ti45__v46   = 291
  integer, parameter :: k_he4_ti45__cr49   = 292
  integer, parameter :: k_n_ti46__ti47   = 293
  integer, parameter :: k_p_ti46__v47   = 294
  integer, parameter :: k_he4_ti46__cr50   = 295
  integer, parameter :: k_n_ti47__ti48   = 296
  integer, parameter :: k_p_ti47__v48   = 297
  integer, parameter :: k_he4_ti47__cr51   = 298
  integer, parameter :: k_n_ti48__ti49   = 299
  integer, parameter :: k_p_ti48__v49   = 300
  integer, parameter :: k_he4_ti48__cr52   = 301
  integer, parameter :: k_p_ti49__v50   = 302
  integer, parameter :: k_n_v46__v47   = 303
  integer, parameter :: k_p_v46__cr47   = 304
  integer, parameter :: k_he4_v46__mn50   = 305
  integer, parameter :: k_n_v47__v48   = 306
  integer, parameter :: k_p_v47__cr48   = 307
  integer, parameter :: k_he4_v47__mn51   = 308
  integer, parameter :: k_n_v48__v49   = 309
  integer, parameter :: k_p_v48__cr49   = 310
  integer, parameter :: k_he4_v48__mn52   = 311
  integer, parameter :: k_n_v49__v50   = 312
  integer, parameter :: k_p_v49__cr50   = 313
  integer, parameter :: k_he4_v49__mn53   = 314
  integer, parameter :: k_n_v50__v51   = 315
  integer, parameter :: k_p_v50__cr51   = 316
  integer, parameter :: k_he4_v50__mn54   = 317
  integer, parameter :: k_p_v51__cr52   = 318
  integer, parameter :: k_he4_v51__mn55   = 319
  integer, parameter :: k_n_cr47__cr48   = 320
  integer, parameter :: k_he4_cr47__fe51   = 321
  integer, parameter :: k_n_cr48__cr49   = 322
  integer, parameter :: k_p_cr48__mn49   = 323
  integer, parameter :: k_he4_cr48__fe52   = 324
  integer, parameter :: k_n_cr49__cr50   = 325
  integer, parameter :: k_p_cr49__mn50   = 326
  integer, parameter :: k_he4_cr49__fe53   = 327
  integer, parameter :: k_n_cr50__cr51   = 328
  integer, parameter :: k_p_cr50__mn51   = 329
  integer, parameter :: k_he4_cr50__fe54   = 330
  integer, parameter :: k_n_cr51__cr52   = 331
  integer, parameter :: k_p_cr51__mn52   = 332
  integer, parameter :: k_he4_cr51__fe55   = 333
  integer, parameter :: k_p_cr52__mn53   = 334
  integer, parameter :: k_he4_cr52__fe56   = 335
  integer, parameter :: k_n_mn49__mn50   = 336
  integer, parameter :: k_he4_mn49__co53   = 337
  integer, parameter :: k_n_mn50__mn51   = 338
  integer, parameter :: k_p_mn50__fe51   = 339
  integer, parameter :: k_he4_mn50__co54   = 340
  integer, parameter :: k_n_mn51__mn52   = 341
  integer, parameter :: k_p_mn51__fe52   = 342
  integer, parameter :: k_he4_mn51__co55   = 343
  integer, parameter :: k_n_mn52__mn53   = 344
  integer, parameter :: k_p_mn52__fe53   = 345
  integer, parameter :: k_he4_mn52__co56   = 346
  integer, parameter :: k_n_mn53__mn54   = 347
  integer, parameter :: k_p_mn53__fe54   = 348
  integer, parameter :: k_he4_mn53__co57   = 349
  integer, parameter :: k_n_mn54__mn55   = 350
  integer, parameter :: k_p_mn54__fe55   = 351
  integer, parameter :: k_he4_mn54__co58   = 352
  integer, parameter :: k_p_mn55__fe56   = 353
  integer, parameter :: k_n_fe51__fe52   = 354
  integer, parameter :: k_he4_fe51__ni55   = 355
  integer, parameter :: k_n_fe52__fe53   = 356
  integer, parameter :: k_p_fe52__co53   = 357
  integer, parameter :: k_he4_fe52__ni56   = 358
  integer, parameter :: k_n_fe53__fe54   = 359
  integer, parameter :: k_p_fe53__co54   = 360
  integer, parameter :: k_he4_fe53__ni57   = 361
  integer, parameter :: k_n_fe54__fe55   = 362
  integer, parameter :: k_p_fe54__co55   = 363
  integer, parameter :: k_he4_fe54__ni58   = 364
  integer, parameter :: k_n_fe55__fe56   = 365
  integer, parameter :: k_p_fe55__co56   = 366
  integer, parameter :: k_he4_fe55__ni59   = 367
  integer, parameter :: k_p_fe56__co57   = 368
  integer, parameter :: k_he4_fe56__ni60   = 369
  integer, parameter :: k_n_co53__co54   = 370
  integer, parameter :: k_p_co53__ni54   = 371
  integer, parameter :: k_n_co54__co55   = 372
  integer, parameter :: k_p_co54__ni55   = 373
  integer, parameter :: k_n_co55__co56   = 374
  integer, parameter :: k_p_co55__ni56   = 375
  integer, parameter :: k_n_co56__co57   = 376
  integer, parameter :: k_p_co56__ni57   = 377
  integer, parameter :: k_n_co57__co58   = 378
  integer, parameter :: k_p_co57__ni58   = 379
  integer, parameter :: k_p_co58__ni59   = 380
  integer, parameter :: k_n_ni54__ni55   = 381
  integer, parameter :: k_n_ni55__ni56   = 382
  integer, parameter :: k_n_ni56__ni57   = 383
  integer, parameter :: k_n_ni57__ni58   = 384
  integer, parameter :: k_n_ni58__ni59   = 385
  integer, parameter :: k_n_ni59__ni60   = 386
  integer, parameter :: k_d_d__n_he3   = 387
  integer, parameter :: k_d_d__p_t   = 388
  integer, parameter :: k_d_t__n_he4   = 389
  integer, parameter :: k_n_he3__p_t   = 390
  integer, parameter :: k_d_he3__p_he4   = 391
  integer, parameter :: k_t_he3__d_he4   = 392
  integer, parameter :: k_c12_c12__p_na23   = 393
  integer, parameter :: k_c12_c12__he4_ne20   = 394
  integer, parameter :: k_p_c13__n_n13   = 395
  integer, parameter :: k_d_c13__n_n14   = 396
  integer, parameter :: k_he4_c13__n_o16   = 397
  integer, parameter :: k_d_c14__n_n15   = 398
  integer, parameter :: k_he4_n13__p_o16   = 399
  integer, parameter :: k_n_n14__p_c14   = 400
  integer, parameter :: k_p_n15__he4_c12   = 401
  integer, parameter :: k_c12_o16__p_al27   = 402
  integer, parameter :: k_c12_o16__he4_mg24   = 403
  integer, parameter :: k_o16_o16__n_s31   = 404
  integer, parameter :: k_o16_o16__p_p31   = 405
  integer, parameter :: k_o16_o16__he4_si28   = 406
  integer, parameter :: k_n_o17__he4_c14   = 407
  integer, parameter :: k_p_o17__he4_n14   = 408
  integer, parameter :: k_he4_o17__n_ne20   = 409
  integer, parameter :: k_p_o18__he4_n15   = 410
  integer, parameter :: k_he4_o18__n_ne21   = 411
  integer, parameter :: k_p_o19__n_f19   = 412
  integer, parameter :: k_he4_o19__n_ne22   = 413
  integer, parameter :: k_n_f18__p_o18   = 414
  integer, parameter :: k_n_f18__he4_n15   = 415
  integer, parameter :: k_he4_f18__p_ne21   = 416
  integer, parameter :: k_p_f19__n_ne19   = 417
  integer, parameter :: k_p_f19__he4_o16   = 418
  integer, parameter :: k_he4_f19__p_ne22   = 419
  integer, parameter :: k_p_f20__n_ne20   = 420
  integer, parameter :: k_p_f20__he4_o17   = 421
  integer, parameter :: k_he4_f20__n_na23   = 422
  integer, parameter :: k_he4_f20__p_ne23   = 423
  integer, parameter :: k_p_f21__n_ne21   = 424
  integer, parameter :: k_p_f21__he4_o18   = 425
  integer, parameter :: k_he4_f21__n_na24   = 426
  integer, parameter :: k_he4_f21__p_ne24   = 427
  integer, parameter :: k_n_ne19__he4_o16   = 428
  integer, parameter :: k_he4_ne19__p_na22   = 429
  integer, parameter :: k_c12_ne20__n_s31   = 430
  integer, parameter :: k_c12_ne20__p_p31   = 431
  integer, parameter :: k_c12_ne20__he4_si28   = 432
  integer, parameter :: k_he4_ne21__n_mg24   = 433
  integer, parameter :: k_he4_ne22__n_mg25   = 434
  integer, parameter :: k_p_ne23__n_na23   = 435
  integer, parameter :: k_he4_ne23__n_mg26   = 436
  integer, parameter :: k_p_ne24__n_na24   = 437
  integer, parameter :: k_he4_ne24__n_mg27   = 438
  integer, parameter :: k_n_na20__p_ne20   = 439
  integer, parameter :: k_he4_na20__p_mg23   = 440
  integer, parameter :: k_n_na21__p_ne21   = 441
  integer, parameter :: k_n_na21__he4_f18   = 442
  integer, parameter :: k_n_na22__p_ne22   = 443
  integer, parameter :: k_n_na22__he4_f19   = 444
  integer, parameter :: k_he4_na22__p_mg25   = 445
  integer, parameter :: k_p_na23__n_mg23   = 446
  integer, parameter :: k_p_na23__he4_ne20   = 447
  integer, parameter :: k_he4_na23__p_mg26   = 448
  integer, parameter :: k_p_na24__n_mg24   = 449
  integer, parameter :: k_p_na24__he4_ne21   = 450
  integer, parameter :: k_he4_na24__n_al27   = 451
  integer, parameter :: k_he4_na24__p_mg27   = 452
  integer, parameter :: k_n_mg22__p_na22   = 453
  integer, parameter :: k_n_mg22__he4_ne19   = 454
  integer, parameter :: k_he4_mg22__p_al25   = 455
  integer, parameter :: k_n_mg23__he4_ne20   = 456
  integer, parameter :: k_n_mg23__c12_c12   = 457
  integer, parameter :: k_he4_mg23__p_al26   = 458
  integer, parameter :: k_p_mg24__he4_na21   = 459
  integer, parameter :: k_he4_mg25__n_si28   = 460
  integer, parameter :: k_he4_mg26__n_si29   = 461
  integer, parameter :: k_p_mg27__n_al27   = 462
  integer, parameter :: k_he4_mg27__n_si30   = 463
  integer, parameter :: k_n_al25__p_mg25   = 464
  integer, parameter :: k_n_al25__he4_na22   = 465
  integer, parameter :: k_he4_al25__p_si28   = 466
  integer, parameter :: k_n_al26__p_mg26   = 467
  integer, parameter :: k_n_al26__he4_na23   = 468
  integer, parameter :: k_he4_al26__p_si29   = 469
  integer, parameter :: k_p_al27__he4_mg24   = 470
  integer, parameter :: k_he4_al27__n_p30   = 471
  integer, parameter :: k_he4_al27__p_si30   = 472
  integer, parameter :: k_p_al28__n_si28   = 473
  integer, parameter :: k_p_al28__he4_mg25   = 474
  integer, parameter :: k_he4_al28__n_p31   = 475
  integer, parameter :: k_he4_al28__p_si31   = 476
  integer, parameter :: k_p_al29__n_si29   = 477
  integer, parameter :: k_p_al29__he4_mg26   = 478
  integer, parameter :: k_he4_al29__n_p32   = 479
  integer, parameter :: k_he4_al29__p_si32   = 480
  integer, parameter :: k_n_si27__p_al27   = 481
  integer, parameter :: k_n_si27__he4_mg24   = 482
  integer, parameter :: k_n_si27__c12_o16   = 483
  integer, parameter :: k_he4_si27__p_p30   = 484
  integer, parameter :: k_p_si31__n_p31   = 485
  integer, parameter :: k_he4_si31__n_s34   = 486
  integer, parameter :: k_n_p29__p_si29   = 487
  integer, parameter :: k_n_p29__he4_al26   = 488
  integer, parameter :: k_he4_p29__p_s32   = 489
  integer, parameter :: k_n_p30__p_si30   = 490
  integer, parameter :: k_he4_p30__p_s33   = 491
  integer, parameter :: k_p_p31__he4_si28   = 492
  integer, parameter :: k_he4_p31__p_s34   = 493
  integer, parameter :: k_n_p32__p_si32   = 494
  integer, parameter :: k_p_p32__n_s32   = 495
  integer, parameter :: k_p_p32__he4_si29   = 496
  integer, parameter :: k_p_p33__he4_si30   = 497
  integer, parameter :: k_n_s30__p_p30   = 498
  integer, parameter :: k_n_s30__he4_si27   = 499
  integer, parameter :: k_he4_s30__p_cl33   = 500
  integer, parameter :: k_n_s31__p_p31   = 501
  integer, parameter :: k_n_s31__he4_si28   = 502
  integer, parameter :: k_he4_s31__p_cl34   = 503
  integer, parameter :: k_n_s32__he4_si29   = 504
  integer, parameter :: k_n_s33__p_p33   = 505
  integer, parameter :: k_n_s33__he4_si30   = 506
  integer, parameter :: k_he4_s34__n_ar37   = 507
  integer, parameter :: k_he4_s34__p_cl37   = 508
  integer, parameter :: k_n_s35__he4_si32   = 509
  integer, parameter :: k_p_s35__he4_p32   = 510
  integer, parameter :: k_he4_s35__n_ar38   = 511
  integer, parameter :: k_n_cl33__p_s33   = 512
  integer, parameter :: k_n_cl33__he4_p30   = 513
  integer, parameter :: k_he4_cl33__p_ar36   = 514
  integer, parameter :: k_n_cl34__p_s34   = 515
  integer, parameter :: k_n_cl34__he4_p31   = 516
  integer, parameter :: k_he4_cl34__p_ar37   = 517
  integer, parameter :: k_n_cl35__p_s35   = 518
  integer, parameter :: k_n_cl35__he4_p32   = 519
  integer, parameter :: k_p_cl35__he4_s32   = 520
  integer, parameter :: k_he4_cl35__p_ar38   = 521
  integer, parameter :: k_n_cl36__he4_p33   = 522
  integer, parameter :: k_p_cl36__he4_s33   = 523
  integer, parameter :: k_n_ar35__p_cl35   = 524
  integer, parameter :: k_n_ar35__he4_s32   = 525
  integer, parameter :: k_he4_ar35__p_k38   = 526
  integer, parameter :: k_n_ar36__p_cl36   = 527
  integer, parameter :: k_n_ar36__he4_s33   = 528
  integer, parameter :: k_n_ar37__p_cl37   = 529
  integer, parameter :: k_p_ar39__he4_cl36   = 530
  integer, parameter :: k_n_k37__p_ar37   = 531
  integer, parameter :: k_n_k37__he4_cl34   = 532
  integer, parameter :: k_he4_k37__p_ca40   = 533
  integer, parameter :: k_n_k38__p_ar38   = 534
  integer, parameter :: k_n_k38__he4_cl35   = 535
  integer, parameter :: k_he4_k38__p_ca41   = 536
  integer, parameter :: k_n_k39__p_ar39   = 537
  integer, parameter :: k_n_k39__he4_cl36   = 538
  integer, parameter :: k_p_k39__he4_ar36   = 539
  integer, parameter :: k_n_k40__he4_cl37   = 540
  integer, parameter :: k_p_k40__n_ca40   = 541
  integer, parameter :: k_p_k40__he4_ar37   = 542
  integer, parameter :: k_he4_k40__p_ca43   = 543
  integer, parameter :: k_p_k41__n_ca41   = 544
  integer, parameter :: k_p_k41__he4_ar38   = 545
  integer, parameter :: k_he4_k41__n_sc44   = 546
  integer, parameter :: k_he4_k41__p_ca44   = 547
  integer, parameter :: k_p_k42__n_ca42   = 548
  integer, parameter :: k_p_k42__he4_ar39   = 549
  integer, parameter :: k_he4_k42__n_sc45   = 550
  integer, parameter :: k_n_ca39__p_k39   = 551
  integer, parameter :: k_n_ca39__he4_ar36   = 552
  integer, parameter :: k_n_ca40__he4_ar37   = 553
  integer, parameter :: k_n_ca41__he4_ar38   = 554
  integer, parameter :: k_n_ca42__he4_ar39   = 555
  integer, parameter :: k_p_ca42__he4_k39   = 556
  integer, parameter :: k_he4_ca43__n_ti46   = 557
  integer, parameter :: k_n_sc42__p_ca42   = 558
  integer, parameter :: k_n_sc42__he4_k39   = 559
  integer, parameter :: k_p_sc42__he4_ca39   = 560
  integer, parameter :: k_he4_sc42__p_ti45   = 561
  integer, parameter :: k_n_sc43__p_ca43   = 562
  integer, parameter :: k_n_sc43__he4_k40   = 563
  integer, parameter :: k_p_sc43__he4_ca40   = 564
  integer, parameter :: k_he4_sc43__p_ti46   = 565
  integer, parameter :: k_n_sc44__p_ca44   = 566
  integer, parameter :: k_p_sc44__he4_ca41   = 567
  integer, parameter :: k_he4_sc44__p_ti47   = 568
  integer, parameter :: k_p_sc45__he4_ca42   = 569
  integer, parameter :: k_he4_sc45__p_ti48   = 570
  integer, parameter :: k_p_sc46__n_ti46   = 571
  integer, parameter :: k_p_sc46__he4_ca43   = 572
  integer, parameter :: k_he4_sc46__n_v49   = 573
  integer, parameter :: k_he4_sc46__p_ti49   = 574
  integer, parameter :: k_n_ti43__p_sc43   = 575
  integer, parameter :: k_n_ti43__he4_ca40   = 576
  integer, parameter :: k_he4_ti43__p_v46   = 577
  integer, parameter :: k_n_ti44__p_sc44   = 578
  integer, parameter :: k_n_ti44__he4_ca41   = 579
  integer, parameter :: k_he4_ti44__p_v47   = 580
  integer, parameter :: k_n_ti45__p_sc45   = 581
  integer, parameter :: k_n_ti45__he4_ca42   = 582
  integer, parameter :: k_he4_ti45__p_v48   = 583
  integer, parameter :: k_n_ti47__he4_ca44   = 584
  integer, parameter :: k_he4_ti49__n_cr52   = 585
  integer, parameter :: k_n_v46__p_ti46   = 586
  integer, parameter :: k_n_v46__he4_sc43   = 587
  integer, parameter :: k_he4_v46__p_cr49   = 588
  integer, parameter :: k_n_v47__p_ti47   = 589
  integer, parameter :: k_n_v47__he4_sc44   = 590
  integer, parameter :: k_he4_v47__p_cr50   = 591
  integer, parameter :: k_n_v48__p_ti48   = 592
  integer, parameter :: k_n_v48__he4_sc45   = 593
  integer, parameter :: k_he4_v48__p_cr51   = 594
  integer, parameter :: k_n_v49__p_ti49   = 595
  integer, parameter :: k_p_v49__he4_ti46   = 596
  integer, parameter :: k_he4_v49__p_cr52   = 597
  integer, parameter :: k_p_v50__n_cr50   = 598
  integer, parameter :: k_p_v50__he4_ti47   = 599
  integer, parameter :: k_p_v51__he4_ti48   = 600
  integer, parameter :: k_n_cr47__p_v47   = 601
  integer, parameter :: k_n_cr47__he4_ti44   = 602
  integer, parameter :: k_he4_cr47__p_mn50   = 603
  integer, parameter :: k_n_cr48__p_v48   = 604
  integer, parameter :: k_n_cr48__he4_ti45   = 605
  integer, parameter :: k_he4_cr48__p_mn51   = 606
  integer, parameter :: k_n_cr49__p_v49   = 607
  integer, parameter :: k_n_cr49__he4_ti46   = 608
  integer, parameter :: k_he4_cr49__p_mn52   = 609
  integer, parameter :: k_n_cr50__he4_ti47   = 610
  integer, parameter :: k_n_cr51__p_v51   = 611
  integer, parameter :: k_n_cr51__he4_ti48   = 612
  integer, parameter :: k_n_mn49__p_cr49   = 613
  integer, parameter :: k_n_mn49__he4_v46   = 614
  integer, parameter :: k_he4_mn49__p_fe52   = 615
  integer, parameter :: k_n_mn50__p_cr50   = 616
  integer, parameter :: k_n_mn50__he4_v47   = 617
  integer, parameter :: k_he4_mn50__p_fe53   = 618
  integer, parameter :: k_n_mn51__p_cr51   = 619
  integer, parameter :: k_n_mn51__he4_v48   = 620
  integer, parameter :: k_he4_mn51__p_fe54   = 621
  integer, parameter :: k_n_mn52__p_cr52   = 622
  integer, parameter :: k_n_mn52__he4_v49   = 623
  integer, parameter :: k_he4_mn52__p_fe55   = 624
  integer, parameter :: k_n_mn53__he4_v50   = 625
  integer, parameter :: k_p_mn53__he4_cr50   = 626
  integer, parameter :: k_he4_mn53__p_fe56   = 627
  integer, parameter :: k_n_mn54__he4_v51   = 628
  integer, parameter :: k_p_mn54__he4_cr51   = 629
  integer, parameter :: k_p_mn55__he4_cr52   = 630
  integer, parameter :: k_n_fe51__p_mn51   = 631
  integer, parameter :: k_n_fe51__he4_cr48   = 632
  integer, parameter :: k_he4_fe51__p_co54   = 633
  integer, parameter :: k_n_fe52__p_mn52   = 634
  integer, parameter :: k_n_fe52__he4_cr49   = 635
  integer, parameter :: k_he4_fe52__p_co55   = 636
  integer, parameter :: k_n_fe53__p_mn53   = 637
  integer, parameter :: k_n_fe53__he4_cr50   = 638
  integer, parameter :: k_he4_fe53__p_co56   = 639
  integer, parameter :: k_n_fe54__p_mn54   = 640
  integer, parameter :: k_n_fe54__he4_cr51   = 641
  integer, parameter :: k_n_fe55__p_mn55   = 642
  integer, parameter :: k_n_fe55__he4_cr52   = 643
  integer, parameter :: k_n_co53__p_fe53   = 644
  integer, parameter :: k_n_co53__he4_mn50   = 645
  integer, parameter :: k_he4_co53__p_ni56   = 646
  integer, parameter :: k_n_co54__p_fe54   = 647
  integer, parameter :: k_n_co54__he4_mn51   = 648
  integer, parameter :: k_he4_co54__p_ni57   = 649
  integer, parameter :: k_n_co55__p_fe55   = 650
  integer, parameter :: k_n_co55__he4_mn52   = 651
  integer, parameter :: k_he4_co55__p_ni58   = 652
  integer, parameter :: k_n_co56__p_fe56   = 653
  integer, parameter :: k_n_co56__he4_mn53   = 654
  integer, parameter :: k_he4_co56__p_ni59   = 655
  integer, parameter :: k_n_co57__he4_mn54   = 656
  integer, parameter :: k_p_co57__he4_fe54   = 657
  integer, parameter :: k_he4_co57__p_ni60   = 658
  integer, parameter :: k_n_co58__he4_mn55   = 659
  integer, parameter :: k_p_co58__he4_fe55   = 660
  integer, parameter :: k_n_ni54__p_co54   = 661
  integer, parameter :: k_n_ni54__he4_fe51   = 662
  integer, parameter :: k_n_ni55__p_co55   = 663
  integer, parameter :: k_n_ni55__he4_fe52   = 664
  integer, parameter :: k_n_ni56__p_co56   = 665
  integer, parameter :: k_n_ni56__he4_fe53   = 666
  integer, parameter :: k_n_ni57__p_co57   = 667
  integer, parameter :: k_n_ni57__he4_fe54   = 668
  integer, parameter :: k_n_ni58__p_co58   = 669
  integer, parameter :: k_n_ni58__he4_fe55   = 670
  integer, parameter :: k_n_ni59__he4_fe56   = 671
  integer, parameter :: k_t_t__n_n_he4   = 672
  integer, parameter :: k_t_he3__n_p_he4   = 673
  integer, parameter :: k_he3_he3__p_p_he4   = 674
  integer, parameter :: k_he4_he4_he4__c12   = 675
  integer, parameter :: k_n_p_p__p   = 676

  real(rt), allocatable, save :: bion(:), mion(:)

#ifdef AMREX_USE_CUDA
  attributes(managed) :: bion, mion
#endif

  !$acc declare create(bion, mion)

#ifdef REACT_SPARSE_JACOBIAN
  ! Shape of Jacobian in Compressed Sparse Row format
  integer, parameter   :: NETWORK_SPARSE_JAC_NNZ = 1888
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
      49, &
      50, &
      51, &
      52, &
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
      65, &
      66, &
      67, &
      68, &
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
      100, &
      101, &
      102, &
      103, &
      104, &
      106, &
      107, &
      108, &
      109, &
      110, &
      111, &
      113, &
      114, &
      115, &
      116, &
      117, &
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
      132, &
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
      132, &
      1, &
      2, &
      3, &
      4, &
      5, &
      8, &
      9, &
      132, &
      1, &
      2, &
      3, &
      4, &
      5, &
      132, &
      1, &
      2, &
      3, &
      4, &
      5, &
      132, &
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
      132, &
      1, &
      2, &
      6, &
      7, &
      12, &
      13, &
      22, &
      33, &
      43, &
      132, &
      1, &
      2, &
      3, &
      6, &
      7, &
      8, &
      10, &
      132, &
      1, &
      2, &
      3, &
      6, &
      8, &
      9, &
      11, &
      14, &
      132, &
      1, &
      2, &
      6, &
      7, &
      8, &
      10, &
      132, &
      1, &
      2, &
      3, &
      6, &
      8, &
      9, &
      10, &
      11, &
      14, &
      132, &
      1, &
      2, &
      3, &
      6, &
      9, &
      11, &
      12, &
      15, &
      17, &
      132, &
      1, &
      2, &
      6, &
      7, &
      8, &
      10, &
      12, &
      13, &
      18, &
      21, &
      27, &
      43, &
      132, &
      1, &
      2, &
      6, &
      13, &
      14, &
      19, &
      132, &
      1, &
      2, &
      6, &
      9, &
      14, &
      15, &
      17, &
      20, &
      132, &
      1, &
      2, &
      6, &
      15, &
      16, &
      132, &
      1, &
      2, &
      6, &
      11, &
      14, &
      17, &
      28, &
      132, &
      1, &
      2, &
      6, &
      12, &
      15, &
      16, &
      17, &
      18, &
      21, &
      29, &
      132, &
      1, &
      2, &
      6, &
      16, &
      18, &
      19, &
      132, &
      1, &
      2, &
      6, &
      19, &
      20, &
      132, &
      1, &
      2, &
      6, &
      17, &
      18, &
      21, &
      32, &
      132, &
      1, &
      2, &
      6, &
      7, &
      13, &
      14, &
      18, &
      19, &
      21, &
      22, &
      27, &
      30, &
      33, &
      132, &
      1, &
      2, &
      6, &
      14, &
      15, &
      17, &
      19, &
      20, &
      22, &
      23, &
      28, &
      31, &
      132, &
      1, &
      2, &
      6, &
      15, &
      16, &
      18, &
      20, &
      23, &
      24, &
      29, &
      132, &
      1, &
      2, &
      6, &
      16, &
      19, &
      24, &
      25, &
      132, &
      1, &
      2, &
      6, &
      20, &
      25, &
      26, &
      132, &
      1, &
      2, &
      6, &
      21, &
      27, &
      132, &
      1, &
      2, &
      6, &
      22, &
      27, &
      28, &
      34, &
      132, &
      1, &
      2, &
      6, &
      17, &
      21, &
      23, &
      28, &
      29, &
      32, &
      38, &
      132, &
      1, &
      2, &
      6, &
      7, &
      18, &
      19, &
      24, &
      25, &
      29, &
      30, &
      33, &
      39, &
      132, &
      1, &
      2, &
      6, &
      19, &
      20, &
      25, &
      26, &
      30, &
      31, &
      132, &
      1, &
      2, &
      6, &
      28, &
      32, &
      132, &
      1, &
      2, &
      6, &
      21, &
      27, &
      29, &
      30, &
      32, &
      33, &
      132, &
      1, &
      2, &
      6, &
      7, &
      13, &
      22, &
      23, &
      30, &
      31, &
      33, &
      34, &
      40, &
      43, &
      132, &
      1, &
      2, &
      6, &
      23, &
      24, &
      29, &
      31, &
      34, &
      35, &
      38, &
      41, &
      132, &
      1, &
      2, &
      6, &
      24, &
      25, &
      30, &
      35, &
      36, &
      39, &
      42, &
      132, &
      1, &
      2, &
      6, &
      25, &
      26, &
      31, &
      36, &
      37, &
      132, &
      1, &
      2, &
      6, &
      28, &
      32, &
      34, &
      38, &
      132, &
      1, &
      2, &
      6, &
      29, &
      33, &
      35, &
      38, &
      39, &
      49, &
      132, &
      1, &
      2, &
      6, &
      7, &
      13, &
      30, &
      31, &
      36, &
      37, &
      39, &
      40, &
      43, &
      132, &
      1, &
      2, &
      6, &
      31, &
      37, &
      40, &
      41, &
      132, &
      1, &
      2, &
      6, &
      41, &
      42, &
      132, &
      1, &
      2, &
      6, &
      33, &
      39, &
      43, &
      54, &
      132, &
      1, &
      2, &
      6, &
      7, &
      13, &
      22, &
      34, &
      35, &
      38, &
      40, &
      41, &
      43, &
      44, &
      51, &
      55, &
      132, &
      1, &
      2, &
      6, &
      35, &
      36, &
      39, &
      41, &
      42, &
      44, &
      45, &
      49, &
      52, &
      56, &
      132, &
      1, &
      2, &
      6, &
      36, &
      37, &
      40, &
      42, &
      45, &
      46, &
      50, &
      53, &
      57, &
      132, &
      1, &
      2, &
      6, &
      37, &
      41, &
      46, &
      47, &
      132, &
      1, &
      2, &
      6, &
      42, &
      47, &
      48, &
      52, &
      59, &
      132, &
      1, &
      2, &
      6, &
      38, &
      44, &
      49, &
      132, &
      1, &
      2, &
      6, &
      39, &
      40, &
      43, &
      45, &
      49, &
      50, &
      54, &
      60, &
      132, &
      1, &
      2, &
      6, &
      7, &
      13, &
      22, &
      40, &
      41, &
      46, &
      47, &
      50, &
      51, &
      55, &
      61, &
      132, &
      1, &
      2, &
      6, &
      41, &
      42, &
      47, &
      48, &
      51, &
      52, &
      59, &
      62, &
      132, &
      1, &
      2, &
      6, &
      42, &
      48, &
      52, &
      53, &
      57, &
      63, &
      132, &
      1, &
      2, &
      6, &
      49, &
      54, &
      132, &
      1, &
      2, &
      6, &
      7, &
      13, &
      22, &
      43, &
      50, &
      54, &
      55, &
      132, &
      1, &
      2, &
      6, &
      44, &
      49, &
      51, &
      52, &
      55, &
      56, &
      62, &
      65, &
      132, &
      1, &
      2, &
      6, &
      45, &
      50, &
      52, &
      53, &
      56, &
      57, &
      60, &
      63, &
      66, &
      132, &
      1, &
      2, &
      6, &
      46, &
      47, &
      51, &
      53, &
      57, &
      58, &
      61, &
      132, &
      1, &
      2, &
      6, &
      47, &
      58, &
      59, &
      62, &
      132, &
      1, &
      2, &
      6, &
      49, &
      54, &
      56, &
      60, &
      132, &
      1, &
      2, &
      6, &
      50, &
      55, &
      57, &
      60, &
      61, &
      70, &
      132, &
      1, &
      2, &
      6, &
      51, &
      58, &
      59, &
      61, &
      62, &
      65, &
      71, &
      132, &
      1, &
      2, &
      6, &
      52, &
      59, &
      62, &
      63, &
      66, &
      69, &
      72, &
      132, &
      1, &
      2, &
      6, &
      53, &
      58, &
      63, &
      64, &
      67, &
      73, &
      132, &
      1, &
      2, &
      6, &
      55, &
      61, &
      65, &
      132, &
      1, &
      2, &
      6, &
      56, &
      60, &
      62, &
      63, &
      65, &
      66, &
      72, &
      76, &
      132, &
      1, &
      2, &
      6, &
      57, &
      58, &
      61, &
      63, &
      66, &
      67, &
      70, &
      73, &
      77, &
      132, &
      1, &
      2, &
      6, &
      58, &
      59, &
      62, &
      64, &
      67, &
      68, &
      71, &
      74, &
      78, &
      132, &
      1, &
      2, &
      6, &
      59, &
      68, &
      69, &
      72, &
      75, &
      79, &
      132, &
      1, &
      2, &
      6, &
      60, &
      66, &
      70, &
      132, &
      1, &
      2, &
      6, &
      61, &
      65, &
      67, &
      70, &
      71, &
      132, &
      1, &
      2, &
      6, &
      62, &
      68, &
      69, &
      71, &
      72, &
      76, &
      79, &
      82, &
      132, &
      1, &
      2, &
      6, &
      63, &
      69, &
      72, &
      73, &
      83, &
      132, &
      1, &
      2, &
      6, &
      64, &
      73, &
      74, &
      78, &
      132, &
      1, &
      2, &
      6, &
      74, &
      75, &
      132, &
      1, &
      2, &
      6, &
      65, &
      71, &
      76, &
      82, &
      132, &
      1, &
      2, &
      6, &
      66, &
      70, &
      72, &
      73, &
      76, &
      77, &
      83, &
      87, &
      132, &
      1, &
      2, &
      6, &
      67, &
      71, &
      73, &
      74, &
      77, &
      78, &
      84, &
      88, &
      132, &
      1, &
      2, &
      6, &
      68, &
      74, &
      75, &
      78, &
      79, &
      82, &
      85, &
      89, &
      132, &
      1, &
      2, &
      6, &
      69, &
      73, &
      75, &
      79, &
      80, &
      83, &
      86, &
      132, &
      1, &
      2, &
      6, &
      74, &
      80, &
      81, &
      84, &
      91, &
      132, &
      1, &
      2, &
      6, &
      71, &
      78, &
      82, &
      132, &
      1, &
      2, &
      6, &
      72, &
      79, &
      82, &
      83, &
      87, &
      94, &
      132, &
      1, &
      2, &
      6, &
      73, &
      74, &
      80, &
      83, &
      84, &
      88, &
      95, &
      132, &
      1, &
      2, &
      6, &
      74, &
      75, &
      81, &
      84, &
      85, &
      89, &
      96, &
      132, &
      1, &
      2, &
      6, &
      75, &
      85, &
      86, &
      132, &
      1, &
      2, &
      6, &
      76, &
      82, &
      87, &
      132, &
      1, &
      2, &
      6, &
      77, &
      83, &
      87, &
      88, &
      100, &
      132, &
      1, &
      2, &
      6, &
      78, &
      82, &
      84, &
      88, &
      89, &
      101, &
      132, &
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
      94, &
      97, &
      102, &
      132, &
      1, &
      2, &
      6, &
      80, &
      84, &
      86, &
      90, &
      91, &
      95, &
      98, &
      103, &
      132, &
      1, &
      2, &
      6, &
      81, &
      85, &
      91, &
      92, &
      96, &
      99, &
      104, &
      132, &
      1, &
      2, &
      6, &
      86, &
      92, &
      93, &
      97, &
      132, &
      1, &
      2, &
      6, &
      82, &
      87, &
      89, &
      94, &
      106, &
      132, &
      1, &
      2, &
      6, &
      83, &
      88, &
      90, &
      94, &
      95, &
      100, &
      107, &
      132, &
      1, &
      2, &
      6, &
      84, &
      89, &
      91, &
      95, &
      96, &
      101, &
      108, &
      132, &
      1, &
      2, &
      6, &
      85, &
      86, &
      92, &
      96, &
      97, &
      102, &
      109, &
      132, &
      1, &
      2, &
      6, &
      86, &
      93, &
      97, &
      98, &
      110, &
      132, &
      1, &
      2, &
      6, &
      98, &
      99, &
      104, &
      111, &
      132, &
      1, &
      2, &
      6, &
      87, &
      94, &
      100, &
      132, &
      1, &
      2, &
      6, &
      88, &
      95, &
      100, &
      101, &
      113, &
      132, &
      1, &
      2, &
      6, &
      89, &
      94, &
      96, &
      101, &
      102, &
      106, &
      114, &
      132, &
      1, &
      2, &
      6, &
      90, &
      95, &
      97, &
      98, &
      102, &
      103, &
      107, &
      110, &
      115, &
      132, &
      1, &
      2, &
      6, &
      91, &
      96, &
      98, &
      103, &
      104, &
      108, &
      111, &
      116, &
      132, &
      1, &
      2, &
      6, &
      92, &
      93, &
      97, &
      99, &
      104, &
      105, &
      109, &
      112, &
      117, &
      132, &
      1, &
      2, &
      6, &
      101, &
      106, &
      132, &
      1, &
      2, &
      6, &
      94, &
      100, &
      102, &
      106, &
      107, &
      119, &
      132, &
      1, &
      2, &
      6, &
      95, &
      101, &
      103, &
      107, &
      108, &
      113, &
      120, &
      132, &
      1, &
      2, &
      6, &
      96, &
      102, &
      104, &
      108, &
      109, &
      114, &
      121, &
      132, &
      1, &
      2, &
      6, &
      97, &
      105, &
      109, &
      110, &
      115, &
      122, &
      132, &
      1, &
      2, &
      6, &
      98, &
      110, &
      111, &
      116, &
      123, &
      132, &
      1, &
      2, &
      6, &
      99, &
      111, &
      112, &
      117, &
      124, &
      132, &
      1, &
      2, &
      6, &
      100, &
      107, &
      113, &
      125, &
      132, &
      1, &
      2, &
      6, &
      101, &
      106, &
      108, &
      113, &
      114, &
      126, &
      132, &
      1, &
      2, &
      6, &
      102, &
      107, &
      109, &
      114, &
      115, &
      119, &
      127, &
      132, &
      1, &
      2, &
      6, &
      103, &
      108, &
      110, &
      115, &
      116, &
      120, &
      123, &
      128, &
      132, &
      1, &
      2, &
      6, &
      104, &
      109, &
      111, &
      116, &
      117, &
      121, &
      124, &
      129, &
      132, &
      1, &
      2, &
      6, &
      105, &
      110, &
      112, &
      117, &
      118, &
      122, &
      130, &
      132, &
      1, &
      2, &
      6, &
      106, &
      114, &
      119, &
      132, &
      1, &
      2, &
      6, &
      107, &
      113, &
      115, &
      119, &
      120, &
      125, &
      132, &
      1, &
      2, &
      6, &
      108, &
      114, &
      116, &
      120, &
      121, &
      126, &
      132, &
      1, &
      2, &
      6, &
      109, &
      115, &
      117, &
      121, &
      122, &
      127, &
      132, &
      1, &
      2, &
      6, &
      110, &
      118, &
      122, &
      123, &
      128, &
      132, &
      1, &
      2, &
      6, &
      111, &
      123, &
      124, &
      129, &
      132, &
      1, &
      2, &
      119, &
      125, &
      132, &
      1, &
      2, &
      6, &
      113, &
      120, &
      125, &
      126, &
      132, &
      1, &
      2, &
      6, &
      114, &
      119, &
      121, &
      126, &
      127, &
      132, &
      1, &
      2, &
      6, &
      115, &
      120, &
      122, &
      127, &
      128, &
      132, &
      1, &
      2, &
      6, &
      116, &
      121, &
      123, &
      128, &
      129, &
      132, &
      1, &
      2, &
      6, &
      117, &
      122, &
      124, &
      129, &
      130, &
      132, &
      1, &
      6, &
      118, &
      123, &
      130, &
      131, &
      132, &
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
      133  ]

    csr_jac_row_count = [ &
      1, &
      122, &
      251, &
      259, &
      265, &
      271, &
      401, &
      411, &
      419, &
      428, &
      435, &
      445, &
      455, &
      468, &
      475, &
      484, &
      490, &
      498, &
      509, &
      516, &
      522, &
      530, &
      544, &
      557, &
      568, &
      576, &
      583, &
      589, &
      597, &
      608, &
      621, &
      631, &
      637, &
      647, &
      661, &
      673, &
      684, &
      693, &
      701, &
      711, &
      724, &
      732, &
      738, &
      746, &
      762, &
      776, &
      789, &
      797, &
      806, &
      813, &
      825, &
      840, &
      852, &
      862, &
      868, &
      879, &
      891, &
      904, &
      915, &
      923, &
      931, &
      941, &
      952, &
      963, &
      973, &
      980, &
      992, &
      1005, &
      1018, &
      1028, &
      1035, &
      1044, &
      1056, &
      1065, &
      1073, &
      1079, &
      1087, &
      1099, &
      1111, &
      1123, &
      1134, &
      1143, &
      1150, &
      1160, &
      1171, &
      1182, &
      1189, &
      1196, &
      1205, &
      1215, &
      1229, &
      1241, &
      1252, &
      1260, &
      1269, &
      1280, &
      1291, &
      1302, &
      1311, &
      1319, &
      1326, &
      1335, &
      1346, &
      1359, &
      1371, &
      1384, &
      1390, &
      1400, &
      1411, &
      1422, &
      1432, &
      1441, &
      1450, &
      1458, &
      1468, &
      1479, &
      1491, &
      1503, &
      1514, &
      1521, &
      1531, &
      1541, &
      1551, &
      1560, &
      1568, &
      1573, &
      1581, &
      1590, &
      1599, &
      1608, &
      1617, &
      1624, &
      1756, &
      1889  ]
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
