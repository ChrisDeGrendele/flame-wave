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

  integer, parameter :: nrates = 682


  ! For each rate, we need: rate, drate/dT, screening, dscreening/dT
  integer, parameter :: num_rate_groups = 4

  ! Number of reaclib rates
  integer, parameter :: nrat_reaclib = 678
  integer, parameter :: number_reaclib_sets = 855

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
  integer, parameter :: k_na20__he4_o16__weak__wc12   = 79
  integer, parameter :: k_n_p__d   = 80
  integer, parameter :: k_p_p__d__weak__bet_pos_   = 81
  integer, parameter :: k_p_p__d__weak__electron_capture   = 82
  integer, parameter :: k_n_d__t   = 83
  integer, parameter :: k_p_d__he3   = 84
  integer, parameter :: k_d_d__he4   = 85
  integer, parameter :: k_p_t__he4   = 86
  integer, parameter :: k_n_he3__he4   = 87
  integer, parameter :: k_p_he3__he4__weak__bet_pos_   = 88
  integer, parameter :: k_n_c12__c13   = 89
  integer, parameter :: k_p_c12__n13   = 90
  integer, parameter :: k_he4_c12__o16   = 91
  integer, parameter :: k_n_c13__c14   = 92
  integer, parameter :: k_p_c13__n14   = 93
  integer, parameter :: k_p_c14__n15   = 94
  integer, parameter :: k_he4_c14__o18   = 95
  integer, parameter :: k_n_n13__n14   = 96
  integer, parameter :: k_n_n14__n15   = 97
  integer, parameter :: k_he4_n14__f18   = 98
  integer, parameter :: k_p_n15__o16   = 99
  integer, parameter :: k_he4_n15__f19   = 100
  integer, parameter :: k_n_o16__o17   = 101
  integer, parameter :: k_he4_o16__ne20   = 102
  integer, parameter :: k_n_o17__o18   = 103
  integer, parameter :: k_p_o17__f18   = 104
  integer, parameter :: k_he4_o17__ne21   = 105
  integer, parameter :: k_n_o18__o19   = 106
  integer, parameter :: k_p_o18__f19   = 107
  integer, parameter :: k_he4_o18__ne22   = 108
  integer, parameter :: k_p_o19__f20   = 109
  integer, parameter :: k_he4_o19__ne23   = 110
  integer, parameter :: k_n_f18__f19   = 111
  integer, parameter :: k_p_f18__ne19   = 112
  integer, parameter :: k_he4_f18__na22   = 113
  integer, parameter :: k_n_f19__f20   = 114
  integer, parameter :: k_p_f19__ne20   = 115
  integer, parameter :: k_he4_f19__na23   = 116
  integer, parameter :: k_n_f20__f21   = 117
  integer, parameter :: k_p_f20__ne21   = 118
  integer, parameter :: k_he4_f20__na24   = 119
  integer, parameter :: k_p_f21__ne22   = 120
  integer, parameter :: k_n_ne19__ne20   = 121
  integer, parameter :: k_p_ne19__na20   = 122
  integer, parameter :: k_he4_ne19__mg23   = 123
  integer, parameter :: k_n_ne20__ne21   = 124
  integer, parameter :: k_p_ne20__na21   = 125
  integer, parameter :: k_he4_ne20__mg24   = 126
  integer, parameter :: k_n_ne21__ne22   = 127
  integer, parameter :: k_p_ne21__na22   = 128
  integer, parameter :: k_he4_ne21__mg25   = 129
  integer, parameter :: k_n_ne22__ne23   = 130
  integer, parameter :: k_p_ne22__na23   = 131
  integer, parameter :: k_he4_ne22__mg26   = 132
  integer, parameter :: k_n_ne23__ne24   = 133
  integer, parameter :: k_p_ne23__na24   = 134
  integer, parameter :: k_he4_ne23__mg27   = 135
  integer, parameter :: k_n_na20__na21   = 136
  integer, parameter :: k_n_na21__na22   = 137
  integer, parameter :: k_p_na21__mg22   = 138
  integer, parameter :: k_he4_na21__al25   = 139
  integer, parameter :: k_n_na22__na23   = 140
  integer, parameter :: k_p_na22__mg23   = 141
  integer, parameter :: k_he4_na22__al26   = 142
  integer, parameter :: k_n_na23__na24   = 143
  integer, parameter :: k_p_na23__mg24   = 144
  integer, parameter :: k_he4_na23__al27   = 145
  integer, parameter :: k_p_na24__mg25   = 146
  integer, parameter :: k_he4_na24__al28   = 147
  integer, parameter :: k_n_mg22__mg23   = 148
  integer, parameter :: k_n_mg23__mg24   = 149
  integer, parameter :: k_he4_mg23__si27   = 150
  integer, parameter :: k_n_mg24__mg25   = 151
  integer, parameter :: k_p_mg24__al25   = 152
  integer, parameter :: k_he4_mg24__si28   = 153
  integer, parameter :: k_n_mg25__mg26   = 154
  integer, parameter :: k_p_mg25__al26   = 155
  integer, parameter :: k_he4_mg25__si29   = 156
  integer, parameter :: k_n_mg26__mg27   = 157
  integer, parameter :: k_p_mg26__al27   = 158
  integer, parameter :: k_he4_mg26__si30   = 159
  integer, parameter :: k_p_mg27__al28   = 160
  integer, parameter :: k_he4_mg27__si31   = 161
  integer, parameter :: k_n_al25__al26   = 162
  integer, parameter :: k_he4_al25__p29   = 163
  integer, parameter :: k_n_al26__al27   = 164
  integer, parameter :: k_p_al26__si27   = 165
  integer, parameter :: k_he4_al26__p30   = 166
  integer, parameter :: k_n_al27__al28   = 167
  integer, parameter :: k_p_al27__si28   = 168
  integer, parameter :: k_he4_al27__p31   = 169
  integer, parameter :: k_n_al28__al29   = 170
  integer, parameter :: k_p_al28__si29   = 171
  integer, parameter :: k_he4_al28__p32   = 172
  integer, parameter :: k_p_al29__si30   = 173
  integer, parameter :: k_he4_al29__p33   = 174
  integer, parameter :: k_n_si27__si28   = 175
  integer, parameter :: k_he4_si27__s31   = 176
  integer, parameter :: k_n_si28__si29   = 177
  integer, parameter :: k_p_si28__p29   = 178
  integer, parameter :: k_he4_si28__s32   = 179
  integer, parameter :: k_n_si29__si30   = 180
  integer, parameter :: k_p_si29__p30   = 181
  integer, parameter :: k_he4_si29__s33   = 182
  integer, parameter :: k_n_si30__si31   = 183
  integer, parameter :: k_p_si30__p31   = 184
  integer, parameter :: k_he4_si30__s34   = 185
  integer, parameter :: k_n_si31__si32   = 186
  integer, parameter :: k_p_si31__p32   = 187
  integer, parameter :: k_he4_si31__s35   = 188
  integer, parameter :: k_p_si32__p33   = 189
  integer, parameter :: k_n_p29__p30   = 190
  integer, parameter :: k_p_p29__s30   = 191
  integer, parameter :: k_he4_p29__cl33   = 192
  integer, parameter :: k_n_p30__p31   = 193
  integer, parameter :: k_p_p30__s31   = 194
  integer, parameter :: k_he4_p30__cl34   = 195
  integer, parameter :: k_n_p31__p32   = 196
  integer, parameter :: k_p_p31__s32   = 197
  integer, parameter :: k_he4_p31__cl35   = 198
  integer, parameter :: k_n_p32__p33   = 199
  integer, parameter :: k_p_p32__s33   = 200
  integer, parameter :: k_he4_p32__cl36   = 201
  integer, parameter :: k_p_p33__s34   = 202
  integer, parameter :: k_he4_p33__cl37   = 203
  integer, parameter :: k_n_s30__s31   = 204
  integer, parameter :: k_n_s31__s32   = 205
  integer, parameter :: k_he4_s31__ar35   = 206
  integer, parameter :: k_n_s32__s33   = 207
  integer, parameter :: k_p_s32__cl33   = 208
  integer, parameter :: k_he4_s32__ar36   = 209
  integer, parameter :: k_n_s33__s34   = 210
  integer, parameter :: k_p_s33__cl34   = 211
  integer, parameter :: k_he4_s33__ar37   = 212
  integer, parameter :: k_n_s34__s35   = 213
  integer, parameter :: k_p_s34__cl35   = 214
  integer, parameter :: k_he4_s34__ar38   = 215
  integer, parameter :: k_p_s35__cl36   = 216
  integer, parameter :: k_he4_s35__ar39   = 217
  integer, parameter :: k_n_cl33__cl34   = 218
  integer, parameter :: k_he4_cl33__k37   = 219
  integer, parameter :: k_n_cl34__cl35   = 220
  integer, parameter :: k_p_cl34__ar35   = 221
  integer, parameter :: k_he4_cl34__k38   = 222
  integer, parameter :: k_n_cl35__cl36   = 223
  integer, parameter :: k_p_cl35__ar36   = 224
  integer, parameter :: k_he4_cl35__k39   = 225
  integer, parameter :: k_n_cl36__cl37   = 226
  integer, parameter :: k_p_cl36__ar37   = 227
  integer, parameter :: k_he4_cl36__k40   = 228
  integer, parameter :: k_p_cl37__ar38   = 229
  integer, parameter :: k_he4_cl37__k41   = 230
  integer, parameter :: k_n_ar35__ar36   = 231
  integer, parameter :: k_he4_ar35__ca39   = 232
  integer, parameter :: k_n_ar36__ar37   = 233
  integer, parameter :: k_p_ar36__k37   = 234
  integer, parameter :: k_he4_ar36__ca40   = 235
  integer, parameter :: k_n_ar37__ar38   = 236
  integer, parameter :: k_p_ar37__k38   = 237
  integer, parameter :: k_he4_ar37__ca41   = 238
  integer, parameter :: k_n_ar38__ar39   = 239
  integer, parameter :: k_p_ar38__k39   = 240
  integer, parameter :: k_he4_ar38__ca42   = 241
  integer, parameter :: k_p_ar39__k40   = 242
  integer, parameter :: k_he4_ar39__ca43   = 243
  integer, parameter :: k_n_k37__k38   = 244
  integer, parameter :: k_n_k38__k39   = 245
  integer, parameter :: k_p_k38__ca39   = 246
  integer, parameter :: k_he4_k38__sc42   = 247
  integer, parameter :: k_n_k39__k40   = 248
  integer, parameter :: k_p_k39__ca40   = 249
  integer, parameter :: k_he4_k39__sc43   = 250
  integer, parameter :: k_n_k40__k41   = 251
  integer, parameter :: k_p_k40__ca41   = 252
  integer, parameter :: k_he4_k40__sc44   = 253
  integer, parameter :: k_n_k41__k42   = 254
  integer, parameter :: k_p_k41__ca42   = 255
  integer, parameter :: k_he4_k41__sc45   = 256
  integer, parameter :: k_p_k42__ca43   = 257
  integer, parameter :: k_he4_k42__sc46   = 258
  integer, parameter :: k_n_ca39__ca40   = 259
  integer, parameter :: k_he4_ca39__ti43   = 260
  integer, parameter :: k_n_ca40__ca41   = 261
  integer, parameter :: k_he4_ca40__ti44   = 262
  integer, parameter :: k_n_ca41__ca42   = 263
  integer, parameter :: k_p_ca41__sc42   = 264
  integer, parameter :: k_he4_ca41__ti45   = 265
  integer, parameter :: k_n_ca42__ca43   = 266
  integer, parameter :: k_p_ca42__sc43   = 267
  integer, parameter :: k_he4_ca42__ti46   = 268
  integer, parameter :: k_n_ca43__ca44   = 269
  integer, parameter :: k_p_ca43__sc44   = 270
  integer, parameter :: k_he4_ca43__ti47   = 271
  integer, parameter :: k_p_ca44__sc45   = 272
  integer, parameter :: k_he4_ca44__ti48   = 273
  integer, parameter :: k_n_sc42__sc43   = 274
  integer, parameter :: k_p_sc42__ti43   = 275
  integer, parameter :: k_he4_sc42__v46   = 276
  integer, parameter :: k_n_sc43__sc44   = 277
  integer, parameter :: k_p_sc43__ti44   = 278
  integer, parameter :: k_he4_sc43__v47   = 279
  integer, parameter :: k_n_sc44__sc45   = 280
  integer, parameter :: k_p_sc44__ti45   = 281
  integer, parameter :: k_he4_sc44__v48   = 282
  integer, parameter :: k_n_sc45__sc46   = 283
  integer, parameter :: k_p_sc45__ti46   = 284
  integer, parameter :: k_he4_sc45__v49   = 285
  integer, parameter :: k_p_sc46__ti47   = 286
  integer, parameter :: k_he4_sc46__v50   = 287
  integer, parameter :: k_n_ti43__ti44   = 288
  integer, parameter :: k_he4_ti43__cr47   = 289
  integer, parameter :: k_n_ti44__ti45   = 290
  integer, parameter :: k_he4_ti44__cr48   = 291
  integer, parameter :: k_n_ti45__ti46   = 292
  integer, parameter :: k_p_ti45__v46   = 293
  integer, parameter :: k_he4_ti45__cr49   = 294
  integer, parameter :: k_n_ti46__ti47   = 295
  integer, parameter :: k_p_ti46__v47   = 296
  integer, parameter :: k_he4_ti46__cr50   = 297
  integer, parameter :: k_n_ti47__ti48   = 298
  integer, parameter :: k_p_ti47__v48   = 299
  integer, parameter :: k_he4_ti47__cr51   = 300
  integer, parameter :: k_n_ti48__ti49   = 301
  integer, parameter :: k_p_ti48__v49   = 302
  integer, parameter :: k_he4_ti48__cr52   = 303
  integer, parameter :: k_p_ti49__v50   = 304
  integer, parameter :: k_n_v46__v47   = 305
  integer, parameter :: k_p_v46__cr47   = 306
  integer, parameter :: k_he4_v46__mn50   = 307
  integer, parameter :: k_n_v47__v48   = 308
  integer, parameter :: k_p_v47__cr48   = 309
  integer, parameter :: k_he4_v47__mn51   = 310
  integer, parameter :: k_n_v48__v49   = 311
  integer, parameter :: k_p_v48__cr49   = 312
  integer, parameter :: k_he4_v48__mn52   = 313
  integer, parameter :: k_n_v49__v50   = 314
  integer, parameter :: k_p_v49__cr50   = 315
  integer, parameter :: k_he4_v49__mn53   = 316
  integer, parameter :: k_n_v50__v51   = 317
  integer, parameter :: k_p_v50__cr51   = 318
  integer, parameter :: k_he4_v50__mn54   = 319
  integer, parameter :: k_p_v51__cr52   = 320
  integer, parameter :: k_he4_v51__mn55   = 321
  integer, parameter :: k_n_cr47__cr48   = 322
  integer, parameter :: k_he4_cr47__fe51   = 323
  integer, parameter :: k_n_cr48__cr49   = 324
  integer, parameter :: k_p_cr48__mn49   = 325
  integer, parameter :: k_he4_cr48__fe52   = 326
  integer, parameter :: k_n_cr49__cr50   = 327
  integer, parameter :: k_p_cr49__mn50   = 328
  integer, parameter :: k_he4_cr49__fe53   = 329
  integer, parameter :: k_n_cr50__cr51   = 330
  integer, parameter :: k_p_cr50__mn51   = 331
  integer, parameter :: k_he4_cr50__fe54   = 332
  integer, parameter :: k_n_cr51__cr52   = 333
  integer, parameter :: k_p_cr51__mn52   = 334
  integer, parameter :: k_he4_cr51__fe55   = 335
  integer, parameter :: k_p_cr52__mn53   = 336
  integer, parameter :: k_he4_cr52__fe56   = 337
  integer, parameter :: k_n_mn49__mn50   = 338
  integer, parameter :: k_he4_mn49__co53   = 339
  integer, parameter :: k_n_mn50__mn51   = 340
  integer, parameter :: k_p_mn50__fe51   = 341
  integer, parameter :: k_he4_mn50__co54   = 342
  integer, parameter :: k_n_mn51__mn52   = 343
  integer, parameter :: k_p_mn51__fe52   = 344
  integer, parameter :: k_he4_mn51__co55   = 345
  integer, parameter :: k_n_mn52__mn53   = 346
  integer, parameter :: k_p_mn52__fe53   = 347
  integer, parameter :: k_he4_mn52__co56   = 348
  integer, parameter :: k_n_mn53__mn54   = 349
  integer, parameter :: k_p_mn53__fe54   = 350
  integer, parameter :: k_he4_mn53__co57   = 351
  integer, parameter :: k_n_mn54__mn55   = 352
  integer, parameter :: k_p_mn54__fe55   = 353
  integer, parameter :: k_he4_mn54__co58   = 354
  integer, parameter :: k_p_mn55__fe56   = 355
  integer, parameter :: k_n_fe51__fe52   = 356
  integer, parameter :: k_he4_fe51__ni55   = 357
  integer, parameter :: k_n_fe52__fe53   = 358
  integer, parameter :: k_p_fe52__co53   = 359
  integer, parameter :: k_he4_fe52__ni56   = 360
  integer, parameter :: k_n_fe53__fe54   = 361
  integer, parameter :: k_p_fe53__co54   = 362
  integer, parameter :: k_he4_fe53__ni57   = 363
  integer, parameter :: k_n_fe54__fe55   = 364
  integer, parameter :: k_p_fe54__co55   = 365
  integer, parameter :: k_he4_fe54__ni58   = 366
  integer, parameter :: k_n_fe55__fe56   = 367
  integer, parameter :: k_p_fe55__co56   = 368
  integer, parameter :: k_he4_fe55__ni59   = 369
  integer, parameter :: k_p_fe56__co57   = 370
  integer, parameter :: k_he4_fe56__ni60   = 371
  integer, parameter :: k_n_co53__co54   = 372
  integer, parameter :: k_p_co53__ni54   = 373
  integer, parameter :: k_n_co54__co55   = 374
  integer, parameter :: k_p_co54__ni55   = 375
  integer, parameter :: k_n_co55__co56   = 376
  integer, parameter :: k_p_co55__ni56   = 377
  integer, parameter :: k_n_co56__co57   = 378
  integer, parameter :: k_p_co56__ni57   = 379
  integer, parameter :: k_n_co57__co58   = 380
  integer, parameter :: k_p_co57__ni58   = 381
  integer, parameter :: k_p_co58__ni59   = 382
  integer, parameter :: k_n_ni54__ni55   = 383
  integer, parameter :: k_n_ni55__ni56   = 384
  integer, parameter :: k_n_ni56__ni57   = 385
  integer, parameter :: k_n_ni57__ni58   = 386
  integer, parameter :: k_n_ni58__ni59   = 387
  integer, parameter :: k_n_ni59__ni60   = 388
  integer, parameter :: k_d_d__n_he3   = 389
  integer, parameter :: k_d_d__p_t   = 390
  integer, parameter :: k_d_t__n_he4   = 391
  integer, parameter :: k_n_he3__p_t   = 392
  integer, parameter :: k_d_he3__p_he4   = 393
  integer, parameter :: k_t_he3__d_he4   = 394
  integer, parameter :: k_c12_c12__p_na23   = 395
  integer, parameter :: k_c12_c12__he4_ne20   = 396
  integer, parameter :: k_p_c13__n_n13   = 397
  integer, parameter :: k_d_c13__n_n14   = 398
  integer, parameter :: k_he4_c13__n_o16   = 399
  integer, parameter :: k_d_c14__n_n15   = 400
  integer, parameter :: k_he4_n13__p_o16   = 401
  integer, parameter :: k_n_n14__p_c14   = 402
  integer, parameter :: k_p_n15__he4_c12   = 403
  integer, parameter :: k_c12_o16__p_al27   = 404
  integer, parameter :: k_c12_o16__he4_mg24   = 405
  integer, parameter :: k_o16_o16__n_s31   = 406
  integer, parameter :: k_o16_o16__p_p31   = 407
  integer, parameter :: k_o16_o16__he4_si28   = 408
  integer, parameter :: k_n_o17__he4_c14   = 409
  integer, parameter :: k_p_o17__he4_n14   = 410
  integer, parameter :: k_he4_o17__n_ne20   = 411
  integer, parameter :: k_p_o18__he4_n15   = 412
  integer, parameter :: k_he4_o18__n_ne21   = 413
  integer, parameter :: k_p_o19__n_f19   = 414
  integer, parameter :: k_he4_o19__n_ne22   = 415
  integer, parameter :: k_n_f18__p_o18   = 416
  integer, parameter :: k_n_f18__he4_n15   = 417
  integer, parameter :: k_he4_f18__p_ne21   = 418
  integer, parameter :: k_p_f19__n_ne19   = 419
  integer, parameter :: k_p_f19__he4_o16   = 420
  integer, parameter :: k_he4_f19__p_ne22   = 421
  integer, parameter :: k_p_f20__n_ne20   = 422
  integer, parameter :: k_p_f20__he4_o17   = 423
  integer, parameter :: k_he4_f20__n_na23   = 424
  integer, parameter :: k_he4_f20__p_ne23   = 425
  integer, parameter :: k_p_f21__n_ne21   = 426
  integer, parameter :: k_p_f21__he4_o18   = 427
  integer, parameter :: k_he4_f21__n_na24   = 428
  integer, parameter :: k_he4_f21__p_ne24   = 429
  integer, parameter :: k_n_ne19__he4_o16   = 430
  integer, parameter :: k_he4_ne19__p_na22   = 431
  integer, parameter :: k_c12_ne20__n_s31   = 432
  integer, parameter :: k_c12_ne20__p_p31   = 433
  integer, parameter :: k_c12_ne20__he4_si28   = 434
  integer, parameter :: k_he4_ne21__n_mg24   = 435
  integer, parameter :: k_he4_ne22__n_mg25   = 436
  integer, parameter :: k_p_ne23__n_na23   = 437
  integer, parameter :: k_he4_ne23__n_mg26   = 438
  integer, parameter :: k_p_ne24__n_na24   = 439
  integer, parameter :: k_he4_ne24__n_mg27   = 440
  integer, parameter :: k_n_na20__p_ne20   = 441
  integer, parameter :: k_he4_na20__p_mg23   = 442
  integer, parameter :: k_n_na21__p_ne21   = 443
  integer, parameter :: k_n_na21__he4_f18   = 444
  integer, parameter :: k_n_na22__p_ne22   = 445
  integer, parameter :: k_n_na22__he4_f19   = 446
  integer, parameter :: k_he4_na22__p_mg25   = 447
  integer, parameter :: k_p_na23__n_mg23   = 448
  integer, parameter :: k_p_na23__he4_ne20   = 449
  integer, parameter :: k_he4_na23__p_mg26   = 450
  integer, parameter :: k_p_na24__n_mg24   = 451
  integer, parameter :: k_p_na24__he4_ne21   = 452
  integer, parameter :: k_he4_na24__n_al27   = 453
  integer, parameter :: k_he4_na24__p_mg27   = 454
  integer, parameter :: k_n_mg22__p_na22   = 455
  integer, parameter :: k_n_mg22__he4_ne19   = 456
  integer, parameter :: k_he4_mg22__p_al25   = 457
  integer, parameter :: k_n_mg23__he4_ne20   = 458
  integer, parameter :: k_n_mg23__c12_c12   = 459
  integer, parameter :: k_he4_mg23__p_al26   = 460
  integer, parameter :: k_p_mg24__he4_na21   = 461
  integer, parameter :: k_he4_mg25__n_si28   = 462
  integer, parameter :: k_he4_mg26__n_si29   = 463
  integer, parameter :: k_p_mg27__n_al27   = 464
  integer, parameter :: k_he4_mg27__n_si30   = 465
  integer, parameter :: k_n_al25__p_mg25   = 466
  integer, parameter :: k_n_al25__he4_na22   = 467
  integer, parameter :: k_he4_al25__p_si28   = 468
  integer, parameter :: k_n_al26__p_mg26   = 469
  integer, parameter :: k_n_al26__he4_na23   = 470
  integer, parameter :: k_he4_al26__p_si29   = 471
  integer, parameter :: k_p_al27__he4_mg24   = 472
  integer, parameter :: k_he4_al27__n_p30   = 473
  integer, parameter :: k_he4_al27__p_si30   = 474
  integer, parameter :: k_p_al28__n_si28   = 475
  integer, parameter :: k_p_al28__he4_mg25   = 476
  integer, parameter :: k_he4_al28__n_p31   = 477
  integer, parameter :: k_he4_al28__p_si31   = 478
  integer, parameter :: k_p_al29__n_si29   = 479
  integer, parameter :: k_p_al29__he4_mg26   = 480
  integer, parameter :: k_he4_al29__n_p32   = 481
  integer, parameter :: k_he4_al29__p_si32   = 482
  integer, parameter :: k_n_si27__p_al27   = 483
  integer, parameter :: k_n_si27__he4_mg24   = 484
  integer, parameter :: k_n_si27__c12_o16   = 485
  integer, parameter :: k_he4_si27__p_p30   = 486
  integer, parameter :: k_p_si31__n_p31   = 487
  integer, parameter :: k_he4_si31__n_s34   = 488
  integer, parameter :: k_n_p29__p_si29   = 489
  integer, parameter :: k_n_p29__he4_al26   = 490
  integer, parameter :: k_he4_p29__p_s32   = 491
  integer, parameter :: k_n_p30__p_si30   = 492
  integer, parameter :: k_he4_p30__p_s33   = 493
  integer, parameter :: k_p_p31__he4_si28   = 494
  integer, parameter :: k_he4_p31__p_s34   = 495
  integer, parameter :: k_n_p32__p_si32   = 496
  integer, parameter :: k_p_p32__n_s32   = 497
  integer, parameter :: k_p_p32__he4_si29   = 498
  integer, parameter :: k_p_p33__he4_si30   = 499
  integer, parameter :: k_n_s30__p_p30   = 500
  integer, parameter :: k_n_s30__he4_si27   = 501
  integer, parameter :: k_he4_s30__p_cl33   = 502
  integer, parameter :: k_n_s31__p_p31   = 503
  integer, parameter :: k_n_s31__he4_si28   = 504
  integer, parameter :: k_he4_s31__p_cl34   = 505
  integer, parameter :: k_n_s32__he4_si29   = 506
  integer, parameter :: k_n_s33__p_p33   = 507
  integer, parameter :: k_n_s33__he4_si30   = 508
  integer, parameter :: k_he4_s34__n_ar37   = 509
  integer, parameter :: k_he4_s34__p_cl37   = 510
  integer, parameter :: k_n_s35__he4_si32   = 511
  integer, parameter :: k_p_s35__he4_p32   = 512
  integer, parameter :: k_he4_s35__n_ar38   = 513
  integer, parameter :: k_n_cl33__p_s33   = 514
  integer, parameter :: k_n_cl33__he4_p30   = 515
  integer, parameter :: k_he4_cl33__p_ar36   = 516
  integer, parameter :: k_n_cl34__p_s34   = 517
  integer, parameter :: k_n_cl34__he4_p31   = 518
  integer, parameter :: k_he4_cl34__p_ar37   = 519
  integer, parameter :: k_n_cl35__p_s35   = 520
  integer, parameter :: k_n_cl35__he4_p32   = 521
  integer, parameter :: k_p_cl35__he4_s32   = 522
  integer, parameter :: k_he4_cl35__p_ar38   = 523
  integer, parameter :: k_n_cl36__he4_p33   = 524
  integer, parameter :: k_p_cl36__he4_s33   = 525
  integer, parameter :: k_n_ar35__p_cl35   = 526
  integer, parameter :: k_n_ar35__he4_s32   = 527
  integer, parameter :: k_he4_ar35__p_k38   = 528
  integer, parameter :: k_n_ar36__p_cl36   = 529
  integer, parameter :: k_n_ar36__he4_s33   = 530
  integer, parameter :: k_n_ar37__p_cl37   = 531
  integer, parameter :: k_p_ar39__he4_cl36   = 532
  integer, parameter :: k_n_k37__p_ar37   = 533
  integer, parameter :: k_n_k37__he4_cl34   = 534
  integer, parameter :: k_he4_k37__p_ca40   = 535
  integer, parameter :: k_n_k38__p_ar38   = 536
  integer, parameter :: k_n_k38__he4_cl35   = 537
  integer, parameter :: k_he4_k38__p_ca41   = 538
  integer, parameter :: k_n_k39__p_ar39   = 539
  integer, parameter :: k_n_k39__he4_cl36   = 540
  integer, parameter :: k_p_k39__he4_ar36   = 541
  integer, parameter :: k_n_k40__he4_cl37   = 542
  integer, parameter :: k_p_k40__n_ca40   = 543
  integer, parameter :: k_p_k40__he4_ar37   = 544
  integer, parameter :: k_he4_k40__p_ca43   = 545
  integer, parameter :: k_p_k41__n_ca41   = 546
  integer, parameter :: k_p_k41__he4_ar38   = 547
  integer, parameter :: k_he4_k41__n_sc44   = 548
  integer, parameter :: k_he4_k41__p_ca44   = 549
  integer, parameter :: k_p_k42__n_ca42   = 550
  integer, parameter :: k_p_k42__he4_ar39   = 551
  integer, parameter :: k_he4_k42__n_sc45   = 552
  integer, parameter :: k_n_ca39__p_k39   = 553
  integer, parameter :: k_n_ca39__he4_ar36   = 554
  integer, parameter :: k_n_ca40__he4_ar37   = 555
  integer, parameter :: k_n_ca41__he4_ar38   = 556
  integer, parameter :: k_n_ca42__he4_ar39   = 557
  integer, parameter :: k_p_ca42__he4_k39   = 558
  integer, parameter :: k_he4_ca43__n_ti46   = 559
  integer, parameter :: k_n_sc42__p_ca42   = 560
  integer, parameter :: k_n_sc42__he4_k39   = 561
  integer, parameter :: k_p_sc42__he4_ca39   = 562
  integer, parameter :: k_he4_sc42__p_ti45   = 563
  integer, parameter :: k_n_sc43__p_ca43   = 564
  integer, parameter :: k_n_sc43__he4_k40   = 565
  integer, parameter :: k_p_sc43__he4_ca40   = 566
  integer, parameter :: k_he4_sc43__p_ti46   = 567
  integer, parameter :: k_n_sc44__p_ca44   = 568
  integer, parameter :: k_p_sc44__he4_ca41   = 569
  integer, parameter :: k_he4_sc44__p_ti47   = 570
  integer, parameter :: k_p_sc45__he4_ca42   = 571
  integer, parameter :: k_he4_sc45__p_ti48   = 572
  integer, parameter :: k_p_sc46__n_ti46   = 573
  integer, parameter :: k_p_sc46__he4_ca43   = 574
  integer, parameter :: k_he4_sc46__n_v49   = 575
  integer, parameter :: k_he4_sc46__p_ti49   = 576
  integer, parameter :: k_n_ti43__p_sc43   = 577
  integer, parameter :: k_n_ti43__he4_ca40   = 578
  integer, parameter :: k_he4_ti43__p_v46   = 579
  integer, parameter :: k_n_ti44__p_sc44   = 580
  integer, parameter :: k_n_ti44__he4_ca41   = 581
  integer, parameter :: k_he4_ti44__p_v47   = 582
  integer, parameter :: k_n_ti45__p_sc45   = 583
  integer, parameter :: k_n_ti45__he4_ca42   = 584
  integer, parameter :: k_he4_ti45__p_v48   = 585
  integer, parameter :: k_n_ti47__he4_ca44   = 586
  integer, parameter :: k_he4_ti49__n_cr52   = 587
  integer, parameter :: k_n_v46__p_ti46   = 588
  integer, parameter :: k_n_v46__he4_sc43   = 589
  integer, parameter :: k_he4_v46__p_cr49   = 590
  integer, parameter :: k_n_v47__p_ti47   = 591
  integer, parameter :: k_n_v47__he4_sc44   = 592
  integer, parameter :: k_he4_v47__p_cr50   = 593
  integer, parameter :: k_n_v48__p_ti48   = 594
  integer, parameter :: k_n_v48__he4_sc45   = 595
  integer, parameter :: k_he4_v48__p_cr51   = 596
  integer, parameter :: k_n_v49__p_ti49   = 597
  integer, parameter :: k_p_v49__he4_ti46   = 598
  integer, parameter :: k_he4_v49__p_cr52   = 599
  integer, parameter :: k_p_v50__n_cr50   = 600
  integer, parameter :: k_p_v50__he4_ti47   = 601
  integer, parameter :: k_p_v51__he4_ti48   = 602
  integer, parameter :: k_n_cr47__p_v47   = 603
  integer, parameter :: k_n_cr47__he4_ti44   = 604
  integer, parameter :: k_he4_cr47__p_mn50   = 605
  integer, parameter :: k_n_cr48__p_v48   = 606
  integer, parameter :: k_n_cr48__he4_ti45   = 607
  integer, parameter :: k_he4_cr48__p_mn51   = 608
  integer, parameter :: k_n_cr49__p_v49   = 609
  integer, parameter :: k_n_cr49__he4_ti46   = 610
  integer, parameter :: k_he4_cr49__p_mn52   = 611
  integer, parameter :: k_n_cr50__he4_ti47   = 612
  integer, parameter :: k_n_cr51__p_v51   = 613
  integer, parameter :: k_n_cr51__he4_ti48   = 614
  integer, parameter :: k_n_mn49__p_cr49   = 615
  integer, parameter :: k_n_mn49__he4_v46   = 616
  integer, parameter :: k_he4_mn49__p_fe52   = 617
  integer, parameter :: k_n_mn50__p_cr50   = 618
  integer, parameter :: k_n_mn50__he4_v47   = 619
  integer, parameter :: k_he4_mn50__p_fe53   = 620
  integer, parameter :: k_n_mn51__p_cr51   = 621
  integer, parameter :: k_n_mn51__he4_v48   = 622
  integer, parameter :: k_he4_mn51__p_fe54   = 623
  integer, parameter :: k_n_mn52__p_cr52   = 624
  integer, parameter :: k_n_mn52__he4_v49   = 625
  integer, parameter :: k_he4_mn52__p_fe55   = 626
  integer, parameter :: k_n_mn53__he4_v50   = 627
  integer, parameter :: k_p_mn53__he4_cr50   = 628
  integer, parameter :: k_he4_mn53__p_fe56   = 629
  integer, parameter :: k_n_mn54__he4_v51   = 630
  integer, parameter :: k_p_mn54__he4_cr51   = 631
  integer, parameter :: k_p_mn55__he4_cr52   = 632
  integer, parameter :: k_n_fe51__p_mn51   = 633
  integer, parameter :: k_n_fe51__he4_cr48   = 634
  integer, parameter :: k_he4_fe51__p_co54   = 635
  integer, parameter :: k_n_fe52__p_mn52   = 636
  integer, parameter :: k_n_fe52__he4_cr49   = 637
  integer, parameter :: k_he4_fe52__p_co55   = 638
  integer, parameter :: k_n_fe53__p_mn53   = 639
  integer, parameter :: k_n_fe53__he4_cr50   = 640
  integer, parameter :: k_he4_fe53__p_co56   = 641
  integer, parameter :: k_n_fe54__p_mn54   = 642
  integer, parameter :: k_n_fe54__he4_cr51   = 643
  integer, parameter :: k_n_fe55__p_mn55   = 644
  integer, parameter :: k_n_fe55__he4_cr52   = 645
  integer, parameter :: k_n_co53__p_fe53   = 646
  integer, parameter :: k_n_co53__he4_mn50   = 647
  integer, parameter :: k_he4_co53__p_ni56   = 648
  integer, parameter :: k_n_co54__p_fe54   = 649
  integer, parameter :: k_n_co54__he4_mn51   = 650
  integer, parameter :: k_he4_co54__p_ni57   = 651
  integer, parameter :: k_n_co55__p_fe55   = 652
  integer, parameter :: k_n_co55__he4_mn52   = 653
  integer, parameter :: k_he4_co55__p_ni58   = 654
  integer, parameter :: k_n_co56__p_fe56   = 655
  integer, parameter :: k_n_co56__he4_mn53   = 656
  integer, parameter :: k_he4_co56__p_ni59   = 657
  integer, parameter :: k_n_co57__he4_mn54   = 658
  integer, parameter :: k_p_co57__he4_fe54   = 659
  integer, parameter :: k_he4_co57__p_ni60   = 660
  integer, parameter :: k_n_co58__he4_mn55   = 661
  integer, parameter :: k_p_co58__he4_fe55   = 662
  integer, parameter :: k_n_ni54__p_co54   = 663
  integer, parameter :: k_n_ni54__he4_fe51   = 664
  integer, parameter :: k_n_ni55__p_co55   = 665
  integer, parameter :: k_n_ni55__he4_fe52   = 666
  integer, parameter :: k_n_ni56__p_co56   = 667
  integer, parameter :: k_n_ni56__he4_fe53   = 668
  integer, parameter :: k_n_ni57__p_co57   = 669
  integer, parameter :: k_n_ni57__he4_fe54   = 670
  integer, parameter :: k_n_ni58__p_co58   = 671
  integer, parameter :: k_n_ni58__he4_fe55   = 672
  integer, parameter :: k_n_ni59__he4_fe56   = 673
  integer, parameter :: k_t_t__n_n_he4   = 674
  integer, parameter :: k_t_he3__n_p_he4   = 675
  integer, parameter :: k_he3_he3__p_p_he4   = 676
  integer, parameter :: k_he4_he4_he4__c12   = 677
  integer, parameter :: k_n_p_p__p   = 678
  integer, parameter :: k_f20__o20   = 679
  integer, parameter :: k_ne20__f20   = 680
  integer, parameter :: k_o20__f20   = 681
  integer, parameter :: k_f20__ne20   = 682

  real(rt), allocatable, save :: bion(:), mion(:)

#ifdef AMREX_USE_CUDA
  attributes(managed) :: bion, mion
#endif

  !$acc declare create(bion, mion)

#ifdef REACT_SPARSE_JACOBIAN
  ! Shape of Jacobian in Compressed Sparse Row format
  integer, parameter   :: NETWORK_SPARSE_JAC_NNZ = 1897
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
      50, &
      51, &
      52, &
      53, &
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
      66, &
      67, &
      68, &
      69, &
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
      101, &
      102, &
      103, &
      104, &
      105, &
      107, &
      108, &
      109, &
      110, &
      111, &
      112, &
      114, &
      115, &
      116, &
      117, &
      118, &
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
      133, &
      1, &
      2, &
      3, &
      4, &
      5, &
      8, &
      9, &
      133, &
      1, &
      2, &
      3, &
      4, &
      5, &
      133, &
      1, &
      2, &
      3, &
      4, &
      5, &
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
      133, &
      1, &
      2, &
      6, &
      7, &
      12, &
      13, &
      23, &
      34, &
      44, &
      133, &
      1, &
      2, &
      3, &
      6, &
      7, &
      8, &
      10, &
      133, &
      1, &
      2, &
      3, &
      6, &
      8, &
      9, &
      11, &
      14, &
      133, &
      1, &
      2, &
      6, &
      7, &
      8, &
      10, &
      133, &
      1, &
      2, &
      3, &
      6, &
      8, &
      9, &
      10, &
      11, &
      14, &
      133, &
      1, &
      2, &
      3, &
      6, &
      9, &
      11, &
      12, &
      15, &
      18, &
      133, &
      1, &
      2, &
      6, &
      7, &
      8, &
      10, &
      12, &
      13, &
      19, &
      22, &
      28, &
      44, &
      133, &
      1, &
      2, &
      6, &
      13, &
      14, &
      20, &
      133, &
      1, &
      2, &
      6, &
      9, &
      14, &
      15, &
      18, &
      21, &
      133, &
      1, &
      2, &
      6, &
      15, &
      16, &
      133, &
      17, &
      20, &
      133, &
      1, &
      2, &
      6, &
      11, &
      14, &
      18, &
      29, &
      133, &
      1, &
      2, &
      6, &
      12, &
      15, &
      16, &
      18, &
      19, &
      22, &
      30, &
      133, &
      1, &
      2, &
      6, &
      16, &
      17, &
      19, &
      20, &
      23, &
      133, &
      1, &
      2, &
      6, &
      20, &
      21, &
      133, &
      1, &
      2, &
      6, &
      18, &
      19, &
      22, &
      33, &
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
      28, &
      31, &
      34, &
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
      29, &
      32, &
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
      30, &
      133, &
      1, &
      2, &
      6, &
      16, &
      20, &
      25, &
      26, &
      133, &
      1, &
      2, &
      6, &
      21, &
      26, &
      27, &
      133, &
      1, &
      2, &
      6, &
      22, &
      28, &
      133, &
      1, &
      2, &
      6, &
      23, &
      28, &
      29, &
      35, &
      133, &
      1, &
      2, &
      6, &
      18, &
      22, &
      24, &
      29, &
      30, &
      33, &
      39, &
      133, &
      1, &
      2, &
      6, &
      7, &
      19, &
      20, &
      25, &
      26, &
      30, &
      31, &
      34, &
      40, &
      133, &
      1, &
      2, &
      6, &
      20, &
      21, &
      26, &
      27, &
      31, &
      32, &
      133, &
      1, &
      2, &
      6, &
      29, &
      33, &
      133, &
      1, &
      2, &
      6, &
      22, &
      28, &
      30, &
      31, &
      33, &
      34, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      23, &
      24, &
      31, &
      32, &
      34, &
      35, &
      41, &
      44, &
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
      39, &
      42, &
      133, &
      1, &
      2, &
      6, &
      25, &
      26, &
      31, &
      36, &
      37, &
      40, &
      43, &
      133, &
      1, &
      2, &
      6, &
      26, &
      27, &
      32, &
      37, &
      38, &
      133, &
      1, &
      2, &
      6, &
      29, &
      33, &
      35, &
      39, &
      133, &
      1, &
      2, &
      6, &
      30, &
      34, &
      36, &
      39, &
      40, &
      50, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      31, &
      32, &
      37, &
      38, &
      40, &
      41, &
      44, &
      133, &
      1, &
      2, &
      6, &
      32, &
      38, &
      41, &
      42, &
      133, &
      1, &
      2, &
      6, &
      42, &
      43, &
      133, &
      1, &
      2, &
      6, &
      34, &
      40, &
      44, &
      55, &
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
      52, &
      56, &
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
      50, &
      53, &
      57, &
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
      51, &
      54, &
      58, &
      133, &
      1, &
      2, &
      6, &
      38, &
      42, &
      47, &
      48, &
      133, &
      1, &
      2, &
      6, &
      43, &
      48, &
      49, &
      53, &
      60, &
      133, &
      1, &
      2, &
      6, &
      39, &
      45, &
      50, &
      133, &
      1, &
      2, &
      6, &
      40, &
      41, &
      44, &
      46, &
      50, &
      51, &
      55, &
      61, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      23, &
      41, &
      42, &
      47, &
      48, &
      51, &
      52, &
      56, &
      62, &
      133, &
      1, &
      2, &
      6, &
      42, &
      43, &
      48, &
      49, &
      52, &
      53, &
      60, &
      63, &
      133, &
      1, &
      2, &
      6, &
      43, &
      49, &
      53, &
      54, &
      58, &
      64, &
      133, &
      1, &
      2, &
      6, &
      50, &
      55, &
      133, &
      1, &
      2, &
      6, &
      7, &
      13, &
      23, &
      44, &
      51, &
      55, &
      56, &
      133, &
      1, &
      2, &
      6, &
      45, &
      50, &
      52, &
      53, &
      56, &
      57, &
      63, &
      66, &
      133, &
      1, &
      2, &
      6, &
      46, &
      51, &
      53, &
      54, &
      57, &
      58, &
      61, &
      64, &
      67, &
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
      62, &
      133, &
      1, &
      2, &
      6, &
      48, &
      59, &
      60, &
      63, &
      133, &
      1, &
      2, &
      6, &
      50, &
      55, &
      57, &
      61, &
      133, &
      1, &
      2, &
      6, &
      51, &
      56, &
      58, &
      61, &
      62, &
      71, &
      133, &
      1, &
      2, &
      6, &
      52, &
      59, &
      60, &
      62, &
      63, &
      66, &
      72, &
      133, &
      1, &
      2, &
      6, &
      53, &
      60, &
      63, &
      64, &
      67, &
      70, &
      73, &
      133, &
      1, &
      2, &
      6, &
      54, &
      59, &
      64, &
      65, &
      68, &
      74, &
      133, &
      1, &
      2, &
      6, &
      56, &
      62, &
      66, &
      133, &
      1, &
      2, &
      6, &
      57, &
      61, &
      63, &
      64, &
      66, &
      67, &
      73, &
      77, &
      133, &
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
      72, &
      75, &
      79, &
      133, &
      1, &
      2, &
      6, &
      60, &
      69, &
      70, &
      73, &
      76, &
      80, &
      133, &
      1, &
      2, &
      6, &
      61, &
      67, &
      71, &
      133, &
      1, &
      2, &
      6, &
      62, &
      66, &
      68, &
      71, &
      72, &
      133, &
      1, &
      2, &
      6, &
      63, &
      69, &
      70, &
      72, &
      73, &
      77, &
      80, &
      83, &
      133, &
      1, &
      2, &
      6, &
      64, &
      70, &
      73, &
      74, &
      84, &
      133, &
      1, &
      2, &
      6, &
      65, &
      74, &
      75, &
      79, &
      133, &
      1, &
      2, &
      6, &
      75, &
      76, &
      133, &
      1, &
      2, &
      6, &
      66, &
      72, &
      77, &
      83, &
      133, &
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
      133, &
      1, &
      2, &
      6, &
      68, &
      72, &
      74, &
      75, &
      78, &
      79, &
      85, &
      89, &
      133, &
      1, &
      2, &
      6, &
      69, &
      75, &
      76, &
      79, &
      80, &
      83, &
      86, &
      90, &
      133, &
      1, &
      2, &
      6, &
      70, &
      74, &
      76, &
      80, &
      81, &
      84, &
      87, &
      133, &
      1, &
      2, &
      6, &
      75, &
      81, &
      82, &
      85, &
      92, &
      133, &
      1, &
      2, &
      6, &
      72, &
      79, &
      83, &
      133, &
      1, &
      2, &
      6, &
      73, &
      80, &
      83, &
      84, &
      88, &
      95, &
      133, &
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
      133, &
      1, &
      2, &
      6, &
      75, &
      76, &
      82, &
      85, &
      86, &
      90, &
      97, &
      133, &
      1, &
      2, &
      6, &
      76, &
      86, &
      87, &
      133, &
      1, &
      2, &
      6, &
      77, &
      83, &
      88, &
      133, &
      1, &
      2, &
      6, &
      78, &
      84, &
      88, &
      89, &
      101, &
      133, &
      1, &
      2, &
      6, &
      79, &
      83, &
      85, &
      89, &
      90, &
      102, &
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
      95, &
      98, &
      103, &
      133, &
      1, &
      2, &
      6, &
      81, &
      85, &
      87, &
      91, &
      92, &
      96, &
      99, &
      104, &
      133, &
      1, &
      2, &
      6, &
      82, &
      86, &
      92, &
      93, &
      97, &
      100, &
      105, &
      133, &
      1, &
      2, &
      6, &
      87, &
      93, &
      94, &
      98, &
      133, &
      1, &
      2, &
      6, &
      83, &
      88, &
      90, &
      95, &
      107, &
      133, &
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
      133, &
      1, &
      2, &
      6, &
      85, &
      90, &
      92, &
      96, &
      97, &
      102, &
      109, &
      133, &
      1, &
      2, &
      6, &
      86, &
      87, &
      93, &
      97, &
      98, &
      103, &
      110, &
      133, &
      1, &
      2, &
      6, &
      87, &
      94, &
      98, &
      99, &
      111, &
      133, &
      1, &
      2, &
      6, &
      99, &
      100, &
      105, &
      112, &
      133, &
      1, &
      2, &
      6, &
      88, &
      95, &
      101, &
      133, &
      1, &
      2, &
      6, &
      89, &
      96, &
      101, &
      102, &
      114, &
      133, &
      1, &
      2, &
      6, &
      90, &
      95, &
      97, &
      102, &
      103, &
      107, &
      115, &
      133, &
      1, &
      2, &
      6, &
      91, &
      96, &
      98, &
      99, &
      103, &
      104, &
      108, &
      111, &
      116, &
      133, &
      1, &
      2, &
      6, &
      92, &
      97, &
      99, &
      104, &
      105, &
      109, &
      112, &
      117, &
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
      113, &
      118, &
      133, &
      1, &
      2, &
      6, &
      102, &
      107, &
      133, &
      1, &
      2, &
      6, &
      95, &
      101, &
      103, &
      107, &
      108, &
      120, &
      133, &
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
      133, &
      1, &
      2, &
      6, &
      97, &
      103, &
      105, &
      109, &
      110, &
      115, &
      122, &
      133, &
      1, &
      2, &
      6, &
      98, &
      106, &
      110, &
      111, &
      116, &
      123, &
      133, &
      1, &
      2, &
      6, &
      99, &
      111, &
      112, &
      117, &
      124, &
      133, &
      1, &
      2, &
      6, &
      100, &
      112, &
      113, &
      118, &
      125, &
      133, &
      1, &
      2, &
      6, &
      101, &
      108, &
      114, &
      126, &
      133, &
      1, &
      2, &
      6, &
      102, &
      107, &
      109, &
      114, &
      115, &
      127, &
      133, &
      1, &
      2, &
      6, &
      103, &
      108, &
      110, &
      115, &
      116, &
      120, &
      128, &
      133, &
      1, &
      2, &
      6, &
      104, &
      109, &
      111, &
      112, &
      116, &
      117, &
      121, &
      124, &
      129, &
      133, &
      1, &
      2, &
      6, &
      105, &
      110, &
      112, &
      117, &
      118, &
      122, &
      125, &
      130, &
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
      131, &
      133, &
      1, &
      2, &
      6, &
      107, &
      115, &
      120, &
      133, &
      1, &
      2, &
      6, &
      108, &
      114, &
      116, &
      120, &
      121, &
      126, &
      133, &
      1, &
      2, &
      6, &
      109, &
      115, &
      117, &
      121, &
      122, &
      127, &
      133, &
      1, &
      2, &
      6, &
      110, &
      116, &
      118, &
      122, &
      123, &
      128, &
      133, &
      1, &
      2, &
      6, &
      111, &
      119, &
      123, &
      124, &
      129, &
      133, &
      1, &
      2, &
      6, &
      112, &
      124, &
      125, &
      130, &
      133, &
      1, &
      2, &
      120, &
      126, &
      133, &
      1, &
      2, &
      6, &
      114, &
      121, &
      126, &
      127, &
      133, &
      1, &
      2, &
      6, &
      115, &
      120, &
      122, &
      127, &
      128, &
      133, &
      1, &
      2, &
      6, &
      116, &
      121, &
      123, &
      128, &
      129, &
      133, &
      1, &
      2, &
      6, &
      117, &
      122, &
      124, &
      125, &
      129, &
      130, &
      133, &
      1, &
      2, &
      6, &
      118, &
      123, &
      125, &
      130, &
      131, &
      133, &
      1, &
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
      493, &
      501, &
      512, &
      521, &
      527, &
      535, &
      549, &
      562, &
      573, &
      581, &
      588, &
      594, &
      602, &
      613, &
      626, &
      636, &
      642, &
      652, &
      666, &
      678, &
      689, &
      698, &
      706, &
      716, &
      729, &
      737, &
      743, &
      751, &
      767, &
      781, &
      794, &
      802, &
      811, &
      818, &
      830, &
      845, &
      857, &
      867, &
      873, &
      884, &
      896, &
      909, &
      920, &
      928, &
      936, &
      946, &
      957, &
      968, &
      978, &
      985, &
      997, &
      1010, &
      1023, &
      1033, &
      1040, &
      1049, &
      1061, &
      1070, &
      1078, &
      1084, &
      1092, &
      1104, &
      1116, &
      1128, &
      1139, &
      1148, &
      1155, &
      1165, &
      1176, &
      1187, &
      1194, &
      1201, &
      1210, &
      1220, &
      1234, &
      1246, &
      1257, &
      1265, &
      1274, &
      1285, &
      1296, &
      1307, &
      1316, &
      1324, &
      1331, &
      1340, &
      1351, &
      1364, &
      1376, &
      1389, &
      1395, &
      1405, &
      1416, &
      1427, &
      1437, &
      1446, &
      1455, &
      1463, &
      1473, &
      1484, &
      1497, &
      1509, &
      1520, &
      1527, &
      1537, &
      1547, &
      1557, &
      1566, &
      1574, &
      1579, &
      1587, &
      1596, &
      1605, &
      1615, &
      1624, &
      1631, &
      1764, &
      1898  ]
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
