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

  integer, parameter :: nrates = 2055


  ! For each rate, we need: rate, drate/dT, screening, dscreening/dT
  integer, parameter :: num_rate_groups = 4

  ! Number of reaclib rates
  integer, parameter :: nrat_reaclib = 2051
  integer, parameter :: number_reaclib_sets = 2453

  ! Number of tabular rates
  integer, parameter :: nrat_tabular = 4

  ! Binding Energies Per Nucleon (MeV)
  real(rt) :: ebind_per_nucleon(nspec)

  ! bion: Binding Energies (ergs)

  ! Nuclides
  integer, parameter :: jn   = 1
  integer, parameter :: jp   = 2
  integer, parameter :: jd   = 3
  integer, parameter :: jhe3   = 4
  integer, parameter :: jhe4   = 5
  integer, parameter :: jli6   = 6
  integer, parameter :: jli7   = 7
  integer, parameter :: jbe7   = 8
  integer, parameter :: jbe9   = 9
  integer, parameter :: jbe10   = 10
  integer, parameter :: jb8   = 11
  integer, parameter :: jb10   = 12
  integer, parameter :: jb11   = 13
  integer, parameter :: jc12   = 14
  integer, parameter :: jc13   = 15
  integer, parameter :: jn13   = 16
  integer, parameter :: jn14   = 17
  integer, parameter :: jn15   = 18
  integer, parameter :: jn16   = 19
  integer, parameter :: jo15   = 20
  integer, parameter :: jo16   = 21
  integer, parameter :: jo17   = 22
  integer, parameter :: jo18   = 23
  integer, parameter :: jo19   = 24
  integer, parameter :: jo20   = 25
  integer, parameter :: jf17   = 26
  integer, parameter :: jf18   = 27
  integer, parameter :: jf19   = 28
  integer, parameter :: jf20   = 29
  integer, parameter :: jne19   = 30
  integer, parameter :: jne20   = 31
  integer, parameter :: jne21   = 32
  integer, parameter :: jne22   = 33
  integer, parameter :: jne23   = 34
  integer, parameter :: jna21   = 35
  integer, parameter :: jna22   = 36
  integer, parameter :: jna23   = 37
  integer, parameter :: jna24   = 38
  integer, parameter :: jmg23   = 39
  integer, parameter :: jmg24   = 40
  integer, parameter :: jmg25   = 41
  integer, parameter :: jmg26   = 42
  integer, parameter :: jmg27   = 43
  integer, parameter :: jal25   = 44
  integer, parameter :: jal26   = 45
  integer, parameter :: jal27   = 46
  integer, parameter :: jal28   = 47
  integer, parameter :: jsi27   = 48
  integer, parameter :: jsi28   = 49
  integer, parameter :: jsi29   = 50
  integer, parameter :: jsi30   = 51
  integer, parameter :: jsi31   = 52
  integer, parameter :: jsi32   = 53
  integer, parameter :: jsi33   = 54
  integer, parameter :: jp30   = 55
  integer, parameter :: jp31   = 56
  integer, parameter :: jp32   = 57
  integer, parameter :: jp33   = 58
  integer, parameter :: jp34   = 59
  integer, parameter :: js31   = 60
  integer, parameter :: js32   = 61
  integer, parameter :: js33   = 62
  integer, parameter :: js34   = 63
  integer, parameter :: js35   = 64
  integer, parameter :: js36   = 65
  integer, parameter :: js37   = 66
  integer, parameter :: jcl35   = 67
  integer, parameter :: jcl36   = 68
  integer, parameter :: jcl37   = 69
  integer, parameter :: jcl38   = 70
  integer, parameter :: jar35   = 71
  integer, parameter :: jar36   = 72
  integer, parameter :: jar37   = 73
  integer, parameter :: jar38   = 74
  integer, parameter :: jar39   = 75
  integer, parameter :: jar40   = 76
  integer, parameter :: jar41   = 77
  integer, parameter :: jk39   = 78
  integer, parameter :: jk40   = 79
  integer, parameter :: jk41   = 80
  integer, parameter :: jk42   = 81
  integer, parameter :: jk43   = 82
  integer, parameter :: jk44   = 83
  integer, parameter :: jca39   = 84
  integer, parameter :: jca40   = 85
  integer, parameter :: jca41   = 86
  integer, parameter :: jca42   = 87
  integer, parameter :: jca43   = 88
  integer, parameter :: jca44   = 89
  integer, parameter :: jca45   = 90
  integer, parameter :: jca46   = 91
  integer, parameter :: jca47   = 92
  integer, parameter :: jca48   = 93
  integer, parameter :: jca49   = 94
  integer, parameter :: jsc43   = 95
  integer, parameter :: jsc44   = 96
  integer, parameter :: jsc45   = 97
  integer, parameter :: jsc46   = 98
  integer, parameter :: jsc47   = 99
  integer, parameter :: jsc48   = 100
  integer, parameter :: jsc49   = 101
  integer, parameter :: jsc50   = 102
  integer, parameter :: jsc51   = 103
  integer, parameter :: jti43   = 104
  integer, parameter :: jti44   = 105
  integer, parameter :: jti45   = 106
  integer, parameter :: jti46   = 107
  integer, parameter :: jti47   = 108
  integer, parameter :: jti48   = 109
  integer, parameter :: jti49   = 110
  integer, parameter :: jti50   = 111
  integer, parameter :: jti51   = 112
  integer, parameter :: jti52   = 113
  integer, parameter :: jti53   = 114
  integer, parameter :: jti54   = 115
  integer, parameter :: jv47   = 116
  integer, parameter :: jv48   = 117
  integer, parameter :: jv49   = 118
  integer, parameter :: jv50   = 119
  integer, parameter :: jv51   = 120
  integer, parameter :: jv52   = 121
  integer, parameter :: jv53   = 122
  integer, parameter :: jv54   = 123
  integer, parameter :: jv55   = 124
  integer, parameter :: jv56   = 125
  integer, parameter :: jcr47   = 126
  integer, parameter :: jcr48   = 127
  integer, parameter :: jcr49   = 128
  integer, parameter :: jcr50   = 129
  integer, parameter :: jcr51   = 130
  integer, parameter :: jcr52   = 131
  integer, parameter :: jcr53   = 132
  integer, parameter :: jcr54   = 133
  integer, parameter :: jcr55   = 134
  integer, parameter :: jcr56   = 135
  integer, parameter :: jcr57   = 136
  integer, parameter :: jcr58   = 137
  integer, parameter :: jmn51   = 138
  integer, parameter :: jmn52   = 139
  integer, parameter :: jmn53   = 140
  integer, parameter :: jmn54   = 141
  integer, parameter :: jmn55   = 142
  integer, parameter :: jmn56   = 143
  integer, parameter :: jmn57   = 144
  integer, parameter :: jmn58   = 145
  integer, parameter :: jmn59   = 146
  integer, parameter :: jfe51   = 147
  integer, parameter :: jfe52   = 148
  integer, parameter :: jfe53   = 149
  integer, parameter :: jfe54   = 150
  integer, parameter :: jfe55   = 151
  integer, parameter :: jfe56   = 152
  integer, parameter :: jfe57   = 153
  integer, parameter :: jfe58   = 154
  integer, parameter :: jfe59   = 155
  integer, parameter :: jfe60   = 156
  integer, parameter :: jfe61   = 157
  integer, parameter :: jfe62   = 158
  integer, parameter :: jfe63   = 159
  integer, parameter :: jfe64   = 160
  integer, parameter :: jfe65   = 161
  integer, parameter :: jfe66   = 162
  integer, parameter :: jco55   = 163
  integer, parameter :: jco56   = 164
  integer, parameter :: jco57   = 165
  integer, parameter :: jco58   = 166
  integer, parameter :: jco59   = 167
  integer, parameter :: jco60   = 168
  integer, parameter :: jco61   = 169
  integer, parameter :: jco62   = 170
  integer, parameter :: jco63   = 171
  integer, parameter :: jco64   = 172
  integer, parameter :: jco65   = 173
  integer, parameter :: jco66   = 174
  integer, parameter :: jco67   = 175
  integer, parameter :: jni55   = 176
  integer, parameter :: jni56   = 177
  integer, parameter :: jni57   = 178
  integer, parameter :: jni58   = 179
  integer, parameter :: jni59   = 180
  integer, parameter :: jni60   = 181
  integer, parameter :: jni61   = 182
  integer, parameter :: jni62   = 183
  integer, parameter :: jni63   = 184
  integer, parameter :: jni64   = 185
  integer, parameter :: jni65   = 186
  integer, parameter :: jni66   = 187
  integer, parameter :: jni67   = 188
  integer, parameter :: jni68   = 189
  integer, parameter :: jcu59   = 190
  integer, parameter :: jcu60   = 191
  integer, parameter :: jcu61   = 192
  integer, parameter :: jcu62   = 193
  integer, parameter :: jcu63   = 194
  integer, parameter :: jcu64   = 195
  integer, parameter :: jcu65   = 196
  integer, parameter :: jcu66   = 197
  integer, parameter :: jzn59   = 198
  integer, parameter :: jzn60   = 199
  integer, parameter :: jzn61   = 200
  integer, parameter :: jzn62   = 201
  integer, parameter :: jzn63   = 202
  integer, parameter :: jzn64   = 203
  integer, parameter :: jzn65   = 204
  integer, parameter :: jzn66   = 205

  ! Reactions
  integer, parameter :: k_n__p__weak__wc12   = 1
  integer, parameter :: k_be7__li7__weak__electron_capture   = 2
  integer, parameter :: k_be10__b10__weak__wc12   = 3
  integer, parameter :: k_n13__c13__weak__wc12   = 4
  integer, parameter :: k_n16__o16__weak__wc12   = 5
  integer, parameter :: k_o15__n15__weak__wc12   = 6
  integer, parameter :: k_o19__f19__weak__wc12   = 7
  integer, parameter :: k_f17__o17__weak__wc12   = 8
  integer, parameter :: k_f18__o18__weak__wc12   = 9
  integer, parameter :: k_ne19__f19__weak__wc12   = 10
  integer, parameter :: k_ne23__na23__weak__wc12   = 11
  integer, parameter :: k_na21__ne21__weak__wc12   = 12
  integer, parameter :: k_na22__ne22__weak__wc12   = 13
  integer, parameter :: k_na24__mg24__weak__wc12   = 14
  integer, parameter :: k_mg23__na23__weak__wc12   = 15
  integer, parameter :: k_mg27__al27__weak__wc12   = 16
  integer, parameter :: k_al25__mg25__weak__wc12   = 17
  integer, parameter :: k_al26__mg26__weak__wc12   = 18
  integer, parameter :: k_al28__si28__weak__wc12   = 19
  integer, parameter :: k_si27__al27__weak__wc12   = 20
  integer, parameter :: k_si31__p31__weak__wc12   = 21
  integer, parameter :: k_si32__p32__weak__wc12   = 22
  integer, parameter :: k_si33__p33__weak__wc12   = 23
  integer, parameter :: k_p30__si30__weak__wc12   = 24
  integer, parameter :: k_p32__s32__weak__wc12   = 25
  integer, parameter :: k_p33__s33__weak__wc12   = 26
  integer, parameter :: k_p34__s34__weak__wc12   = 27
  integer, parameter :: k_s31__p31__weak__wc12   = 28
  integer, parameter :: k_s35__cl35__weak__wc12   = 29
  integer, parameter :: k_s37__cl37__weak__wc12   = 30
  integer, parameter :: k_cl36__ar36__weak__wc12   = 31
  integer, parameter :: k_cl36__s36__weak__wc12   = 32
  integer, parameter :: k_cl38__ar38__weak__wc12   = 33
  integer, parameter :: k_ar35__cl35__weak__wc12   = 34
  integer, parameter :: k_ar37__cl37__weak__wc12   = 35
  integer, parameter :: k_ar39__k39__weak__wc12   = 36
  integer, parameter :: k_ar41__k41__weak__wc12   = 37
  integer, parameter :: k_k40__ca40__weak__wc12   = 38
  integer, parameter :: k_k40__ar40__weak__wc12   = 39
  integer, parameter :: k_k42__ca42__weak__wc12   = 40
  integer, parameter :: k_k43__ca43__weak__wc12   = 41
  integer, parameter :: k_k44__ca44__weak__wc12   = 42
  integer, parameter :: k_ca39__k39__weak__wc12   = 43
  integer, parameter :: k_ca41__k41__weak__wc12   = 44
  integer, parameter :: k_ca45__sc45__weak__wc12   = 45
  integer, parameter :: k_ca47__sc47__weak__wc12   = 46
  integer, parameter :: k_ca48__sc48__weak__wc07   = 47
  integer, parameter :: k_ca49__sc49__weak__wc12   = 48
  integer, parameter :: k_sc43__ca43__weak__wc12   = 49
  integer, parameter :: k_sc44__ca44__weak__wc12   = 50
  integer, parameter :: k_sc46__ca46__weak__bex_pos_   = 51
  integer, parameter :: k_sc46__ti46__weak__wc12   = 52
  integer, parameter :: k_sc47__ti47__weak__wc12   = 53
  integer, parameter :: k_sc48__ti48__weak__wc12   = 54
  integer, parameter :: k_sc49__ti49__weak__wc12   = 55
  integer, parameter :: k_sc50__ti50__weak__wc12   = 56
  integer, parameter :: k_sc51__ti51__weak__wc12   = 57
  integer, parameter :: k_ti43__sc43__weak__wc12   = 58
  integer, parameter :: k_ti44__sc44__weak__wc12   = 59
  integer, parameter :: k_ti45__sc45__weak__wc12   = 60
  integer, parameter :: k_ti51__v51__weak__wc12   = 61
  integer, parameter :: k_ti52__v52__weak__wc12   = 62
  integer, parameter :: k_ti53__v53__weak__wc12   = 63
  integer, parameter :: k_ti54__v54__weak__wc12   = 64
  integer, parameter :: k_v47__ti47__weak__wc12   = 65
  integer, parameter :: k_v48__ti48__weak__wc12   = 66
  integer, parameter :: k_v49__ti49__weak__wc12   = 67
  integer, parameter :: k_v50__ti50__weak__wc12   = 68
  integer, parameter :: k_v50__cr50__weak__wc12   = 69
  integer, parameter :: k_v52__cr52__weak__wc12   = 70
  integer, parameter :: k_v53__cr53__weak__wc12   = 71
  integer, parameter :: k_v54__cr54__weak__wc12   = 72
  integer, parameter :: k_v55__cr55__weak__wc12   = 73
  integer, parameter :: k_v56__cr56__weak__wc12   = 74
  integer, parameter :: k_cr47__v47__weak__wc12   = 75
  integer, parameter :: k_cr48__v48__weak__wc12   = 76
  integer, parameter :: k_cr49__v49__weak__wc12   = 77
  integer, parameter :: k_cr51__v51__weak__wc12   = 78
  integer, parameter :: k_cr55__mn55__weak__wc12   = 79
  integer, parameter :: k_cr56__mn56__weak__wc12   = 80
  integer, parameter :: k_cr57__mn57__weak__wc12   = 81
  integer, parameter :: k_cr58__mn58__weak__wc12   = 82
  integer, parameter :: k_mn51__cr51__weak__wc12   = 83
  integer, parameter :: k_mn52__cr52__weak__wc12   = 84
  integer, parameter :: k_mn53__cr53__weak__wc12   = 85
  integer, parameter :: k_mn54__cr54__weak__wc12   = 86
  integer, parameter :: k_mn54__fe54__weak__wc12   = 87
  integer, parameter :: k_mn56__fe56__weak__wc12   = 88
  integer, parameter :: k_mn57__fe57__weak__wc12   = 89
  integer, parameter :: k_mn58__fe58__weak__wc12   = 90
  integer, parameter :: k_mn59__fe59__weak__wc12   = 91
  integer, parameter :: k_fe51__mn51__weak__wc12   = 92
  integer, parameter :: k_fe52__mn52__weak__wc12   = 93
  integer, parameter :: k_fe53__mn53__weak__wc12   = 94
  integer, parameter :: k_fe55__mn55__weak__wc12   = 95
  integer, parameter :: k_fe59__co59__weak__wc12   = 96
  integer, parameter :: k_fe60__co60__weak__wc12   = 97
  integer, parameter :: k_fe61__co61__weak__wc12   = 98
  integer, parameter :: k_fe62__co62__weak__wc12   = 99
  integer, parameter :: k_fe63__co63__weak__wc12   = 100
  integer, parameter :: k_fe64__co64__weak__wc12   = 101
  integer, parameter :: k_fe65__co65__weak__wc12   = 102
  integer, parameter :: k_fe66__co66__weak__wc12   = 103
  integer, parameter :: k_co55__fe55__weak__wc12   = 104
  integer, parameter :: k_co56__fe56__weak__wc12   = 105
  integer, parameter :: k_co57__fe57__weak__wc12   = 106
  integer, parameter :: k_co58__fe58__weak__wc12   = 107
  integer, parameter :: k_co58__ni58__weak__mo03   = 108
  integer, parameter :: k_co60__ni60__weak__wc12   = 109
  integer, parameter :: k_co61__ni61__weak__wc12   = 110
  integer, parameter :: k_co62__ni62__weak__wc12   = 111
  integer, parameter :: k_co63__ni63__weak__wc12   = 112
  integer, parameter :: k_co64__ni64__weak__wc12   = 113
  integer, parameter :: k_co65__ni65__weak__wc12   = 114
  integer, parameter :: k_co66__ni66__weak__wc12   = 115
  integer, parameter :: k_co67__ni67__weak__wc12   = 116
  integer, parameter :: k_ni55__co55__weak__wc12   = 117
  integer, parameter :: k_ni56__co56__weak__wc12   = 118
  integer, parameter :: k_ni57__co57__weak__wc12   = 119
  integer, parameter :: k_ni59__co59__weak__wc12   = 120
  integer, parameter :: k_ni63__cu63__weak__wc12   = 121
  integer, parameter :: k_ni65__cu65__weak__wc12   = 122
  integer, parameter :: k_ni66__cu66__weak__wc12   = 123
  integer, parameter :: k_cu59__ni59__weak__wc12   = 124
  integer, parameter :: k_cu60__ni60__weak__wc12   = 125
  integer, parameter :: k_cu61__ni61__weak__wc12   = 126
  integer, parameter :: k_cu62__ni62__weak__wc12   = 127
  integer, parameter :: k_cu64__ni64__weak__wc12   = 128
  integer, parameter :: k_cu64__zn64__weak__wc12   = 129
  integer, parameter :: k_cu66__zn66__weak__wc12   = 130
  integer, parameter :: k_zn59__cu59__weak__wc12   = 131
  integer, parameter :: k_zn60__cu60__weak__wc12   = 132
  integer, parameter :: k_zn61__cu61__weak__wc12   = 133
  integer, parameter :: k_zn62__cu62__weak__wc12   = 134
  integer, parameter :: k_zn63__cu63__weak__wc12   = 135
  integer, parameter :: k_zn65__cu65__weak__wc12   = 136
  integer, parameter :: k_d__n_p   = 137
  integer, parameter :: k_he3__p_d   = 138
  integer, parameter :: k_he4__n_he3   = 139
  integer, parameter :: k_he4__d_d   = 140
  integer, parameter :: k_li6__he4_d   = 141
  integer, parameter :: k_li7__n_li6   = 142
  integer, parameter :: k_be7__p_li6   = 143
  integer, parameter :: k_be7__he4_he3   = 144
  integer, parameter :: k_be10__n_be9   = 145
  integer, parameter :: k_b8__p_be7   = 146
  integer, parameter :: k_b8__he4_he4__weak__wc12   = 147
  integer, parameter :: k_b10__p_be9   = 148
  integer, parameter :: k_b10__he4_li6   = 149
  integer, parameter :: k_b11__n_b10   = 150
  integer, parameter :: k_b11__p_be10   = 151
  integer, parameter :: k_b11__he4_li7   = 152
  integer, parameter :: k_c12__p_b11   = 153
  integer, parameter :: k_c13__n_c12   = 154
  integer, parameter :: k_n13__p_c12   = 155
  integer, parameter :: k_n14__n_n13   = 156
  integer, parameter :: k_n14__p_c13   = 157
  integer, parameter :: k_n15__n_n14   = 158
  integer, parameter :: k_n16__n_n15   = 159
  integer, parameter :: k_n16__he4_c12__weak__wc12   = 160
  integer, parameter :: k_o15__p_n14   = 161
  integer, parameter :: k_o16__n_o15   = 162
  integer, parameter :: k_o16__p_n15   = 163
  integer, parameter :: k_o16__he4_c12   = 164
  integer, parameter :: k_o17__n_o16   = 165
  integer, parameter :: k_o18__n_o17   = 166
  integer, parameter :: k_o19__n_o18   = 167
  integer, parameter :: k_f17__p_o16   = 168
  integer, parameter :: k_f18__n_f17   = 169
  integer, parameter :: k_f18__p_o17   = 170
  integer, parameter :: k_f18__he4_n14   = 171
  integer, parameter :: k_f19__n_f18   = 172
  integer, parameter :: k_f19__p_o18   = 173
  integer, parameter :: k_f19__he4_n15   = 174
  integer, parameter :: k_f20__n_f19   = 175
  integer, parameter :: k_f20__p_o19   = 176
  integer, parameter :: k_ne19__p_f18   = 177
  integer, parameter :: k_ne19__he4_o15   = 178
  integer, parameter :: k_ne20__n_ne19   = 179
  integer, parameter :: k_ne20__p_f19   = 180
  integer, parameter :: k_ne20__he4_o16   = 181
  integer, parameter :: k_ne21__n_ne20   = 182
  integer, parameter :: k_ne21__p_f20   = 183
  integer, parameter :: k_ne21__he4_o17   = 184
  integer, parameter :: k_ne22__n_ne21   = 185
  integer, parameter :: k_ne22__he4_o18   = 186
  integer, parameter :: k_ne23__n_ne22   = 187
  integer, parameter :: k_ne23__he4_o19   = 188
  integer, parameter :: k_na21__p_ne20   = 189
  integer, parameter :: k_na21__he4_f17   = 190
  integer, parameter :: k_na22__n_na21   = 191
  integer, parameter :: k_na22__p_ne21   = 192
  integer, parameter :: k_na22__he4_f18   = 193
  integer, parameter :: k_na23__n_na22   = 194
  integer, parameter :: k_na23__p_ne22   = 195
  integer, parameter :: k_na23__he4_f19   = 196
  integer, parameter :: k_na24__n_na23   = 197
  integer, parameter :: k_na24__p_ne23   = 198
  integer, parameter :: k_na24__he4_f20   = 199
  integer, parameter :: k_mg23__p_na22   = 200
  integer, parameter :: k_mg23__he4_ne19   = 201
  integer, parameter :: k_mg24__n_mg23   = 202
  integer, parameter :: k_mg24__p_na23   = 203
  integer, parameter :: k_mg24__he4_ne20   = 204
  integer, parameter :: k_mg25__n_mg24   = 205
  integer, parameter :: k_mg25__p_na24   = 206
  integer, parameter :: k_mg25__he4_ne21   = 207
  integer, parameter :: k_mg26__n_mg25   = 208
  integer, parameter :: k_mg26__he4_ne22   = 209
  integer, parameter :: k_mg27__n_mg26   = 210
  integer, parameter :: k_mg27__he4_ne23   = 211
  integer, parameter :: k_al25__p_mg24   = 212
  integer, parameter :: k_al25__he4_na21   = 213
  integer, parameter :: k_al26__n_al25   = 214
  integer, parameter :: k_al26__p_mg25   = 215
  integer, parameter :: k_al26__he4_na22   = 216
  integer, parameter :: k_al27__n_al26   = 217
  integer, parameter :: k_al27__p_mg26   = 218
  integer, parameter :: k_al27__he4_na23   = 219
  integer, parameter :: k_al28__n_al27   = 220
  integer, parameter :: k_al28__p_mg27   = 221
  integer, parameter :: k_al28__he4_na24   = 222
  integer, parameter :: k_si27__p_al26   = 223
  integer, parameter :: k_si27__he4_mg23   = 224
  integer, parameter :: k_si28__n_si27   = 225
  integer, parameter :: k_si28__p_al27   = 226
  integer, parameter :: k_si28__he4_mg24   = 227
  integer, parameter :: k_si29__n_si28   = 228
  integer, parameter :: k_si29__p_al28   = 229
  integer, parameter :: k_si29__he4_mg25   = 230
  integer, parameter :: k_si30__n_si29   = 231
  integer, parameter :: k_si30__he4_mg26   = 232
  integer, parameter :: k_si31__n_si30   = 233
  integer, parameter :: k_si31__he4_mg27   = 234
  integer, parameter :: k_si32__n_si31   = 235
  integer, parameter :: k_si33__n_si32   = 236
  integer, parameter :: k_p30__p_si29   = 237
  integer, parameter :: k_p30__he4_al26   = 238
  integer, parameter :: k_p31__n_p30   = 239
  integer, parameter :: k_p31__p_si30   = 240
  integer, parameter :: k_p31__he4_al27   = 241
  integer, parameter :: k_p32__n_p31   = 242
  integer, parameter :: k_p32__p_si31   = 243
  integer, parameter :: k_p32__he4_al28   = 244
  integer, parameter :: k_p33__n_p32   = 245
  integer, parameter :: k_p33__p_si32   = 246
  integer, parameter :: k_p34__n_p33   = 247
  integer, parameter :: k_p34__p_si33   = 248
  integer, parameter :: k_s31__p_p30   = 249
  integer, parameter :: k_s31__he4_si27   = 250
  integer, parameter :: k_s32__n_s31   = 251
  integer, parameter :: k_s32__p_p31   = 252
  integer, parameter :: k_s32__he4_si28   = 253
  integer, parameter :: k_s33__n_s32   = 254
  integer, parameter :: k_s33__p_p32   = 255
  integer, parameter :: k_s33__he4_si29   = 256
  integer, parameter :: k_s34__n_s33   = 257
  integer, parameter :: k_s34__p_p33   = 258
  integer, parameter :: k_s34__he4_si30   = 259
  integer, parameter :: k_s35__n_s34   = 260
  integer, parameter :: k_s35__p_p34   = 261
  integer, parameter :: k_s35__he4_si31   = 262
  integer, parameter :: k_s36__n_s35   = 263
  integer, parameter :: k_s36__he4_si32   = 264
  integer, parameter :: k_s37__n_s36   = 265
  integer, parameter :: k_s37__he4_si33   = 266
  integer, parameter :: k_cl35__p_s34   = 267
  integer, parameter :: k_cl35__he4_p31   = 268
  integer, parameter :: k_cl36__n_cl35   = 269
  integer, parameter :: k_cl36__p_s35   = 270
  integer, parameter :: k_cl36__he4_p32   = 271
  integer, parameter :: k_cl37__n_cl36   = 272
  integer, parameter :: k_cl37__p_s36   = 273
  integer, parameter :: k_cl37__he4_p33   = 274
  integer, parameter :: k_cl38__n_cl37   = 275
  integer, parameter :: k_cl38__p_s37   = 276
  integer, parameter :: k_cl38__he4_p34   = 277
  integer, parameter :: k_ar35__he4_s31   = 278
  integer, parameter :: k_ar36__n_ar35   = 279
  integer, parameter :: k_ar36__p_cl35   = 280
  integer, parameter :: k_ar36__he4_s32   = 281
  integer, parameter :: k_ar37__n_ar36   = 282
  integer, parameter :: k_ar37__p_cl36   = 283
  integer, parameter :: k_ar37__he4_s33   = 284
  integer, parameter :: k_ar38__n_ar37   = 285
  integer, parameter :: k_ar38__p_cl37   = 286
  integer, parameter :: k_ar38__he4_s34   = 287
  integer, parameter :: k_ar39__n_ar38   = 288
  integer, parameter :: k_ar39__p_cl38   = 289
  integer, parameter :: k_ar39__he4_s35   = 290
  integer, parameter :: k_ar40__n_ar39   = 291
  integer, parameter :: k_ar40__he4_s36   = 292
  integer, parameter :: k_ar41__n_ar40   = 293
  integer, parameter :: k_ar41__he4_s37   = 294
  integer, parameter :: k_k39__p_ar38   = 295
  integer, parameter :: k_k39__he4_cl35   = 296
  integer, parameter :: k_k40__n_k39   = 297
  integer, parameter :: k_k40__p_ar39   = 298
  integer, parameter :: k_k40__he4_cl36   = 299
  integer, parameter :: k_k41__n_k40   = 300
  integer, parameter :: k_k41__p_ar40   = 301
  integer, parameter :: k_k41__he4_cl37   = 302
  integer, parameter :: k_k42__n_k41   = 303
  integer, parameter :: k_k42__p_ar41   = 304
  integer, parameter :: k_k42__he4_cl38   = 305
  integer, parameter :: k_k43__n_k42   = 306
  integer, parameter :: k_k44__n_k43   = 307
  integer, parameter :: k_ca39__he4_ar35   = 308
  integer, parameter :: k_ca40__n_ca39   = 309
  integer, parameter :: k_ca40__p_k39   = 310
  integer, parameter :: k_ca40__he4_ar36   = 311
  integer, parameter :: k_ca41__n_ca40   = 312
  integer, parameter :: k_ca41__p_k40   = 313
  integer, parameter :: k_ca41__he4_ar37   = 314
  integer, parameter :: k_ca42__n_ca41   = 315
  integer, parameter :: k_ca42__p_k41   = 316
  integer, parameter :: k_ca42__he4_ar38   = 317
  integer, parameter :: k_ca43__n_ca42   = 318
  integer, parameter :: k_ca43__p_k42   = 319
  integer, parameter :: k_ca43__he4_ar39   = 320
  integer, parameter :: k_ca44__n_ca43   = 321
  integer, parameter :: k_ca44__p_k43   = 322
  integer, parameter :: k_ca44__he4_ar40   = 323
  integer, parameter :: k_ca45__n_ca44   = 324
  integer, parameter :: k_ca45__p_k44   = 325
  integer, parameter :: k_ca45__he4_ar41   = 326
  integer, parameter :: k_ca46__n_ca45   = 327
  integer, parameter :: k_ca47__n_ca46   = 328
  integer, parameter :: k_ca48__n_ca47   = 329
  integer, parameter :: k_ca49__n_ca48   = 330
  integer, parameter :: k_sc43__p_ca42   = 331
  integer, parameter :: k_sc43__he4_k39   = 332
  integer, parameter :: k_sc44__n_sc43   = 333
  integer, parameter :: k_sc44__p_ca43   = 334
  integer, parameter :: k_sc44__he4_k40   = 335
  integer, parameter :: k_sc45__n_sc44   = 336
  integer, parameter :: k_sc45__p_ca44   = 337
  integer, parameter :: k_sc45__he4_k41   = 338
  integer, parameter :: k_sc46__n_sc45   = 339
  integer, parameter :: k_sc46__p_ca45   = 340
  integer, parameter :: k_sc46__he4_k42   = 341
  integer, parameter :: k_sc47__n_sc46   = 342
  integer, parameter :: k_sc47__p_ca46   = 343
  integer, parameter :: k_sc47__he4_k43   = 344
  integer, parameter :: k_sc48__n_sc47   = 345
  integer, parameter :: k_sc48__p_ca47   = 346
  integer, parameter :: k_sc48__he4_k44   = 347
  integer, parameter :: k_sc49__n_sc48   = 348
  integer, parameter :: k_sc49__p_ca48   = 349
  integer, parameter :: k_sc50__n_sc49   = 350
  integer, parameter :: k_sc50__p_ca49   = 351
  integer, parameter :: k_sc51__n_sc50   = 352
  integer, parameter :: k_ti43__he4_ca39   = 353
  integer, parameter :: k_ti44__n_ti43   = 354
  integer, parameter :: k_ti44__p_sc43   = 355
  integer, parameter :: k_ti44__he4_ca40   = 356
  integer, parameter :: k_ti45__n_ti44   = 357
  integer, parameter :: k_ti45__p_sc44   = 358
  integer, parameter :: k_ti45__he4_ca41   = 359
  integer, parameter :: k_ti46__n_ti45   = 360
  integer, parameter :: k_ti46__p_sc45   = 361
  integer, parameter :: k_ti46__he4_ca42   = 362
  integer, parameter :: k_ti47__n_ti46   = 363
  integer, parameter :: k_ti47__p_sc46   = 364
  integer, parameter :: k_ti47__he4_ca43   = 365
  integer, parameter :: k_ti48__n_ti47   = 366
  integer, parameter :: k_ti48__p_sc47   = 367
  integer, parameter :: k_ti48__he4_ca44   = 368
  integer, parameter :: k_ti49__n_ti48   = 369
  integer, parameter :: k_ti49__p_sc48   = 370
  integer, parameter :: k_ti49__he4_ca45   = 371
  integer, parameter :: k_ti50__n_ti49   = 372
  integer, parameter :: k_ti50__p_sc49   = 373
  integer, parameter :: k_ti50__he4_ca46   = 374
  integer, parameter :: k_ti51__n_ti50   = 375
  integer, parameter :: k_ti51__p_sc50   = 376
  integer, parameter :: k_ti51__he4_ca47   = 377
  integer, parameter :: k_ti52__n_ti51   = 378
  integer, parameter :: k_ti52__p_sc51   = 379
  integer, parameter :: k_ti52__he4_ca48   = 380
  integer, parameter :: k_ti53__n_ti52   = 381
  integer, parameter :: k_ti53__he4_ca49   = 382
  integer, parameter :: k_ti54__n_ti53   = 383
  integer, parameter :: k_v47__p_ti46   = 384
  integer, parameter :: k_v47__he4_sc43   = 385
  integer, parameter :: k_v48__n_v47   = 386
  integer, parameter :: k_v48__p_ti47   = 387
  integer, parameter :: k_v48__he4_sc44   = 388
  integer, parameter :: k_v49__n_v48   = 389
  integer, parameter :: k_v49__p_ti48   = 390
  integer, parameter :: k_v49__he4_sc45   = 391
  integer, parameter :: k_v50__n_v49   = 392
  integer, parameter :: k_v50__p_ti49   = 393
  integer, parameter :: k_v50__he4_sc46   = 394
  integer, parameter :: k_v51__n_v50   = 395
  integer, parameter :: k_v51__p_ti50   = 396
  integer, parameter :: k_v51__he4_sc47   = 397
  integer, parameter :: k_v52__n_v51   = 398
  integer, parameter :: k_v52__p_ti51   = 399
  integer, parameter :: k_v52__he4_sc48   = 400
  integer, parameter :: k_v53__n_v52   = 401
  integer, parameter :: k_v53__p_ti52   = 402
  integer, parameter :: k_v53__he4_sc49   = 403
  integer, parameter :: k_v54__n_v53   = 404
  integer, parameter :: k_v54__p_ti53   = 405
  integer, parameter :: k_v54__he4_sc50   = 406
  integer, parameter :: k_v55__n_v54   = 407
  integer, parameter :: k_v55__p_ti54   = 408
  integer, parameter :: k_v55__he4_sc51   = 409
  integer, parameter :: k_v56__n_v55   = 410
  integer, parameter :: k_v56__n_cr55__weak__wc07   = 411
  integer, parameter :: k_cr47__he4_ti43   = 412
  integer, parameter :: k_cr48__n_cr47   = 413
  integer, parameter :: k_cr48__p_v47   = 414
  integer, parameter :: k_cr48__he4_ti44   = 415
  integer, parameter :: k_cr49__n_cr48   = 416
  integer, parameter :: k_cr49__p_v48   = 417
  integer, parameter :: k_cr49__he4_ti45   = 418
  integer, parameter :: k_cr50__n_cr49   = 419
  integer, parameter :: k_cr50__p_v49   = 420
  integer, parameter :: k_cr50__he4_ti46   = 421
  integer, parameter :: k_cr51__n_cr50   = 422
  integer, parameter :: k_cr51__p_v50   = 423
  integer, parameter :: k_cr51__he4_ti47   = 424
  integer, parameter :: k_cr52__n_cr51   = 425
  integer, parameter :: k_cr52__p_v51   = 426
  integer, parameter :: k_cr52__he4_ti48   = 427
  integer, parameter :: k_cr53__n_cr52   = 428
  integer, parameter :: k_cr53__p_v52   = 429
  integer, parameter :: k_cr53__he4_ti49   = 430
  integer, parameter :: k_cr54__n_cr53   = 431
  integer, parameter :: k_cr54__p_v53   = 432
  integer, parameter :: k_cr54__he4_ti50   = 433
  integer, parameter :: k_cr55__n_cr54   = 434
  integer, parameter :: k_cr55__p_v54   = 435
  integer, parameter :: k_cr55__he4_ti51   = 436
  integer, parameter :: k_cr56__n_cr55   = 437
  integer, parameter :: k_cr56__p_v55   = 438
  integer, parameter :: k_cr56__he4_ti52   = 439
  integer, parameter :: k_cr57__n_cr56   = 440
  integer, parameter :: k_cr57__p_v56   = 441
  integer, parameter :: k_cr57__he4_ti53   = 442
  integer, parameter :: k_cr58__n_cr57   = 443
  integer, parameter :: k_cr58__he4_ti54   = 444
  integer, parameter :: k_mn51__p_cr50   = 445
  integer, parameter :: k_mn51__he4_v47   = 446
  integer, parameter :: k_mn52__n_mn51   = 447
  integer, parameter :: k_mn52__p_cr51   = 448
  integer, parameter :: k_mn52__he4_v48   = 449
  integer, parameter :: k_mn53__n_mn52   = 450
  integer, parameter :: k_mn53__p_cr52   = 451
  integer, parameter :: k_mn53__he4_v49   = 452
  integer, parameter :: k_mn54__n_mn53   = 453
  integer, parameter :: k_mn54__p_cr53   = 454
  integer, parameter :: k_mn54__he4_v50   = 455
  integer, parameter :: k_mn55__n_mn54   = 456
  integer, parameter :: k_mn55__p_cr54   = 457
  integer, parameter :: k_mn55__he4_v51   = 458
  integer, parameter :: k_mn56__n_mn55   = 459
  integer, parameter :: k_mn56__p_cr55   = 460
  integer, parameter :: k_mn56__he4_v52   = 461
  integer, parameter :: k_mn57__n_mn56   = 462
  integer, parameter :: k_mn57__p_cr56   = 463
  integer, parameter :: k_mn57__he4_v53   = 464
  integer, parameter :: k_mn58__n_mn57   = 465
  integer, parameter :: k_mn58__p_cr57   = 466
  integer, parameter :: k_mn58__he4_v54   = 467
  integer, parameter :: k_mn59__n_mn58   = 468
  integer, parameter :: k_mn59__p_cr58   = 469
  integer, parameter :: k_mn59__he4_v55   = 470
  integer, parameter :: k_fe51__he4_cr47   = 471
  integer, parameter :: k_fe52__n_fe51   = 472
  integer, parameter :: k_fe52__p_mn51   = 473
  integer, parameter :: k_fe52__he4_cr48   = 474
  integer, parameter :: k_fe53__n_fe52   = 475
  integer, parameter :: k_fe53__p_mn52   = 476
  integer, parameter :: k_fe53__he4_cr49   = 477
  integer, parameter :: k_fe54__n_fe53   = 478
  integer, parameter :: k_fe54__p_mn53   = 479
  integer, parameter :: k_fe54__he4_cr50   = 480
  integer, parameter :: k_fe55__n_fe54   = 481
  integer, parameter :: k_fe55__p_mn54   = 482
  integer, parameter :: k_fe55__he4_cr51   = 483
  integer, parameter :: k_fe56__n_fe55   = 484
  integer, parameter :: k_fe56__p_mn55   = 485
  integer, parameter :: k_fe56__he4_cr52   = 486
  integer, parameter :: k_fe57__n_fe56   = 487
  integer, parameter :: k_fe57__p_mn56   = 488
  integer, parameter :: k_fe57__he4_cr53   = 489
  integer, parameter :: k_fe58__n_fe57   = 490
  integer, parameter :: k_fe58__p_mn57   = 491
  integer, parameter :: k_fe58__he4_cr54   = 492
  integer, parameter :: k_fe59__n_fe58   = 493
  integer, parameter :: k_fe59__p_mn58   = 494
  integer, parameter :: k_fe59__he4_cr55   = 495
  integer, parameter :: k_fe60__n_fe59   = 496
  integer, parameter :: k_fe60__p_mn59   = 497
  integer, parameter :: k_fe60__he4_cr56   = 498
  integer, parameter :: k_fe61__n_fe60   = 499
  integer, parameter :: k_fe61__he4_cr57   = 500
  integer, parameter :: k_fe62__n_fe61   = 501
  integer, parameter :: k_fe62__he4_cr58   = 502
  integer, parameter :: k_fe63__n_fe62   = 503
  integer, parameter :: k_fe64__n_fe63   = 504
  integer, parameter :: k_fe65__n_fe64   = 505
  integer, parameter :: k_fe66__n_fe65   = 506
  integer, parameter :: k_fe66__n_co65__weak__mo03   = 507
  integer, parameter :: k_co55__p_fe54   = 508
  integer, parameter :: k_co55__he4_mn51   = 509
  integer, parameter :: k_co56__n_co55   = 510
  integer, parameter :: k_co56__p_fe55   = 511
  integer, parameter :: k_co56__he4_mn52   = 512
  integer, parameter :: k_co57__n_co56   = 513
  integer, parameter :: k_co57__p_fe56   = 514
  integer, parameter :: k_co57__he4_mn53   = 515
  integer, parameter :: k_co58__n_co57   = 516
  integer, parameter :: k_co58__p_fe57   = 517
  integer, parameter :: k_co58__he4_mn54   = 518
  integer, parameter :: k_co59__n_co58   = 519
  integer, parameter :: k_co59__p_fe58   = 520
  integer, parameter :: k_co59__he4_mn55   = 521
  integer, parameter :: k_co60__n_co59   = 522
  integer, parameter :: k_co60__p_fe59   = 523
  integer, parameter :: k_co60__he4_mn56   = 524
  integer, parameter :: k_co61__n_co60   = 525
  integer, parameter :: k_co61__p_fe60   = 526
  integer, parameter :: k_co61__he4_mn57   = 527
  integer, parameter :: k_co62__n_co61   = 528
  integer, parameter :: k_co62__p_fe61   = 529
  integer, parameter :: k_co62__he4_mn58   = 530
  integer, parameter :: k_co63__n_co62   = 531
  integer, parameter :: k_co63__p_fe62   = 532
  integer, parameter :: k_co63__he4_mn59   = 533
  integer, parameter :: k_co64__n_co63   = 534
  integer, parameter :: k_co64__p_fe63   = 535
  integer, parameter :: k_co65__n_co64   = 536
  integer, parameter :: k_co65__p_fe64   = 537
  integer, parameter :: k_co66__n_co65   = 538
  integer, parameter :: k_co66__p_fe65   = 539
  integer, parameter :: k_co67__n_ni66__weak__bkmo   = 540
  integer, parameter :: k_co67__n_co66   = 541
  integer, parameter :: k_co67__p_fe66   = 542
  integer, parameter :: k_ni55__he4_fe51   = 543
  integer, parameter :: k_ni56__n_ni55   = 544
  integer, parameter :: k_ni56__p_co55   = 545
  integer, parameter :: k_ni56__he4_fe52   = 546
  integer, parameter :: k_ni57__n_ni56   = 547
  integer, parameter :: k_ni57__p_co56   = 548
  integer, parameter :: k_ni57__he4_fe53   = 549
  integer, parameter :: k_ni58__n_ni57   = 550
  integer, parameter :: k_ni58__p_co57   = 551
  integer, parameter :: k_ni58__he4_fe54   = 552
  integer, parameter :: k_ni59__n_ni58   = 553
  integer, parameter :: k_ni59__p_co58   = 554
  integer, parameter :: k_ni59__he4_fe55   = 555
  integer, parameter :: k_ni60__n_ni59   = 556
  integer, parameter :: k_ni60__p_co59   = 557
  integer, parameter :: k_ni60__he4_fe56   = 558
  integer, parameter :: k_ni61__n_ni60   = 559
  integer, parameter :: k_ni61__p_co60   = 560
  integer, parameter :: k_ni61__he4_fe57   = 561
  integer, parameter :: k_ni62__n_ni61   = 562
  integer, parameter :: k_ni62__p_co61   = 563
  integer, parameter :: k_ni62__he4_fe58   = 564
  integer, parameter :: k_ni63__n_ni62   = 565
  integer, parameter :: k_ni63__p_co62   = 566
  integer, parameter :: k_ni63__he4_fe59   = 567
  integer, parameter :: k_ni64__n_ni63   = 568
  integer, parameter :: k_ni64__p_co63   = 569
  integer, parameter :: k_ni64__he4_fe60   = 570
  integer, parameter :: k_ni65__n_ni64   = 571
  integer, parameter :: k_ni65__p_co64   = 572
  integer, parameter :: k_ni65__he4_fe61   = 573
  integer, parameter :: k_ni66__n_ni65   = 574
  integer, parameter :: k_ni66__p_co65   = 575
  integer, parameter :: k_ni66__he4_fe62   = 576
  integer, parameter :: k_ni67__n_ni66   = 577
  integer, parameter :: k_ni67__p_co66   = 578
  integer, parameter :: k_ni67__he4_fe63   = 579
  integer, parameter :: k_ni68__n_ni67   = 580
  integer, parameter :: k_ni68__p_co67   = 581
  integer, parameter :: k_ni68__he4_fe64   = 582
  integer, parameter :: k_cu59__p_ni58   = 583
  integer, parameter :: k_cu59__he4_co55   = 584
  integer, parameter :: k_cu60__n_cu59   = 585
  integer, parameter :: k_cu60__p_ni59   = 586
  integer, parameter :: k_cu60__he4_co56   = 587
  integer, parameter :: k_cu61__n_cu60   = 588
  integer, parameter :: k_cu61__p_ni60   = 589
  integer, parameter :: k_cu61__he4_co57   = 590
  integer, parameter :: k_cu62__n_cu61   = 591
  integer, parameter :: k_cu62__p_ni61   = 592
  integer, parameter :: k_cu62__he4_co58   = 593
  integer, parameter :: k_cu63__n_cu62   = 594
  integer, parameter :: k_cu63__p_ni62   = 595
  integer, parameter :: k_cu63__he4_co59   = 596
  integer, parameter :: k_cu64__n_cu63   = 597
  integer, parameter :: k_cu64__p_ni63   = 598
  integer, parameter :: k_cu64__he4_co60   = 599
  integer, parameter :: k_cu65__n_cu64   = 600
  integer, parameter :: k_cu65__p_ni64   = 601
  integer, parameter :: k_cu65__he4_co61   = 602
  integer, parameter :: k_cu66__n_cu65   = 603
  integer, parameter :: k_cu66__p_ni65   = 604
  integer, parameter :: k_cu66__he4_co62   = 605
  integer, parameter :: k_zn59__p_ni58__weak__wc12   = 606
  integer, parameter :: k_zn59__he4_ni55   = 607
  integer, parameter :: k_zn60__n_zn59   = 608
  integer, parameter :: k_zn60__p_cu59   = 609
  integer, parameter :: k_zn60__he4_ni56   = 610
  integer, parameter :: k_zn61__n_zn60   = 611
  integer, parameter :: k_zn61__p_cu60   = 612
  integer, parameter :: k_zn61__he4_ni57   = 613
  integer, parameter :: k_zn62__n_zn61   = 614
  integer, parameter :: k_zn62__p_cu61   = 615
  integer, parameter :: k_zn62__he4_ni58   = 616
  integer, parameter :: k_zn63__n_zn62   = 617
  integer, parameter :: k_zn63__p_cu62   = 618
  integer, parameter :: k_zn63__he4_ni59   = 619
  integer, parameter :: k_zn64__n_zn63   = 620
  integer, parameter :: k_zn64__p_cu63   = 621
  integer, parameter :: k_zn64__he4_ni60   = 622
  integer, parameter :: k_zn65__n_zn64   = 623
  integer, parameter :: k_zn65__p_cu64   = 624
  integer, parameter :: k_zn65__he4_ni61   = 625
  integer, parameter :: k_zn66__n_zn65   = 626
  integer, parameter :: k_zn66__p_cu65   = 627
  integer, parameter :: k_zn66__he4_ni62   = 628
  integer, parameter :: k_li6__n_p_he4   = 629
  integer, parameter :: k_be9__n_he4_he4   = 630
  integer, parameter :: k_c12__he4_he4_he4   = 631
  integer, parameter :: k_n_p__d   = 632
  integer, parameter :: k_p_p__d__weak__bet_pos_   = 633
  integer, parameter :: k_p_p__d__weak__electron_capture   = 634
  integer, parameter :: k_p_d__he3   = 635
  integer, parameter :: k_d_d__he4   = 636
  integer, parameter :: k_he4_d__li6   = 637
  integer, parameter :: k_n_he3__he4   = 638
  integer, parameter :: k_p_he3__he4__weak__bet_pos_   = 639
  integer, parameter :: k_he4_he3__be7   = 640
  integer, parameter :: k_n_li6__li7   = 641
  integer, parameter :: k_p_li6__be7   = 642
  integer, parameter :: k_he4_li6__b10   = 643
  integer, parameter :: k_he4_li7__b11   = 644
  integer, parameter :: k_p_be7__b8   = 645
  integer, parameter :: k_n_be9__be10   = 646
  integer, parameter :: k_p_be9__b10   = 647
  integer, parameter :: k_p_be10__b11   = 648
  integer, parameter :: k_n_b10__b11   = 649
  integer, parameter :: k_p_b11__c12   = 650
  integer, parameter :: k_n_c12__c13   = 651
  integer, parameter :: k_p_c12__n13   = 652
  integer, parameter :: k_he4_c12__o16   = 653
  integer, parameter :: k_p_c13__n14   = 654
  integer, parameter :: k_n_n13__n14   = 655
  integer, parameter :: k_n_n14__n15   = 656
  integer, parameter :: k_p_n14__o15   = 657
  integer, parameter :: k_he4_n14__f18   = 658
  integer, parameter :: k_n_n15__n16   = 659
  integer, parameter :: k_p_n15__o16   = 660
  integer, parameter :: k_he4_n15__f19   = 661
  integer, parameter :: k_n_o15__o16   = 662
  integer, parameter :: k_he4_o15__ne19   = 663
  integer, parameter :: k_n_o16__o17   = 664
  integer, parameter :: k_p_o16__f17   = 665
  integer, parameter :: k_he4_o16__ne20   = 666
  integer, parameter :: k_n_o17__o18   = 667
  integer, parameter :: k_p_o17__f18   = 668
  integer, parameter :: k_he4_o17__ne21   = 669
  integer, parameter :: k_n_o18__o19   = 670
  integer, parameter :: k_p_o18__f19   = 671
  integer, parameter :: k_he4_o18__ne22   = 672
  integer, parameter :: k_p_o19__f20   = 673
  integer, parameter :: k_he4_o19__ne23   = 674
  integer, parameter :: k_n_f17__f18   = 675
  integer, parameter :: k_he4_f17__na21   = 676
  integer, parameter :: k_n_f18__f19   = 677
  integer, parameter :: k_p_f18__ne19   = 678
  integer, parameter :: k_he4_f18__na22   = 679
  integer, parameter :: k_n_f19__f20   = 680
  integer, parameter :: k_p_f19__ne20   = 681
  integer, parameter :: k_he4_f19__na23   = 682
  integer, parameter :: k_p_f20__ne21   = 683
  integer, parameter :: k_he4_f20__na24   = 684
  integer, parameter :: k_n_ne19__ne20   = 685
  integer, parameter :: k_he4_ne19__mg23   = 686
  integer, parameter :: k_n_ne20__ne21   = 687
  integer, parameter :: k_p_ne20__na21   = 688
  integer, parameter :: k_he4_ne20__mg24   = 689
  integer, parameter :: k_n_ne21__ne22   = 690
  integer, parameter :: k_p_ne21__na22   = 691
  integer, parameter :: k_he4_ne21__mg25   = 692
  integer, parameter :: k_n_ne22__ne23   = 693
  integer, parameter :: k_p_ne22__na23   = 694
  integer, parameter :: k_he4_ne22__mg26   = 695
  integer, parameter :: k_p_ne23__na24   = 696
  integer, parameter :: k_he4_ne23__mg27   = 697
  integer, parameter :: k_n_na21__na22   = 698
  integer, parameter :: k_he4_na21__al25   = 699
  integer, parameter :: k_n_na22__na23   = 700
  integer, parameter :: k_p_na22__mg23   = 701
  integer, parameter :: k_he4_na22__al26   = 702
  integer, parameter :: k_n_na23__na24   = 703
  integer, parameter :: k_p_na23__mg24   = 704
  integer, parameter :: k_he4_na23__al27   = 705
  integer, parameter :: k_p_na24__mg25   = 706
  integer, parameter :: k_he4_na24__al28   = 707
  integer, parameter :: k_n_mg23__mg24   = 708
  integer, parameter :: k_he4_mg23__si27   = 709
  integer, parameter :: k_n_mg24__mg25   = 710
  integer, parameter :: k_p_mg24__al25   = 711
  integer, parameter :: k_he4_mg24__si28   = 712
  integer, parameter :: k_n_mg25__mg26   = 713
  integer, parameter :: k_p_mg25__al26   = 714
  integer, parameter :: k_he4_mg25__si29   = 715
  integer, parameter :: k_n_mg26__mg27   = 716
  integer, parameter :: k_p_mg26__al27   = 717
  integer, parameter :: k_he4_mg26__si30   = 718
  integer, parameter :: k_p_mg27__al28   = 719
  integer, parameter :: k_he4_mg27__si31   = 720
  integer, parameter :: k_n_al25__al26   = 721
  integer, parameter :: k_n_al26__al27   = 722
  integer, parameter :: k_p_al26__si27   = 723
  integer, parameter :: k_he4_al26__p30   = 724
  integer, parameter :: k_n_al27__al28   = 725
  integer, parameter :: k_p_al27__si28   = 726
  integer, parameter :: k_he4_al27__p31   = 727
  integer, parameter :: k_p_al28__si29   = 728
  integer, parameter :: k_he4_al28__p32   = 729
  integer, parameter :: k_n_si27__si28   = 730
  integer, parameter :: k_he4_si27__s31   = 731
  integer, parameter :: k_n_si28__si29   = 732
  integer, parameter :: k_he4_si28__s32   = 733
  integer, parameter :: k_n_si29__si30   = 734
  integer, parameter :: k_p_si29__p30   = 735
  integer, parameter :: k_he4_si29__s33   = 736
  integer, parameter :: k_n_si30__si31   = 737
  integer, parameter :: k_p_si30__p31   = 738
  integer, parameter :: k_he4_si30__s34   = 739
  integer, parameter :: k_n_si31__si32   = 740
  integer, parameter :: k_p_si31__p32   = 741
  integer, parameter :: k_he4_si31__s35   = 742
  integer, parameter :: k_n_si32__si33   = 743
  integer, parameter :: k_p_si32__p33   = 744
  integer, parameter :: k_he4_si32__s36   = 745
  integer, parameter :: k_p_si33__p34   = 746
  integer, parameter :: k_he4_si33__s37   = 747
  integer, parameter :: k_n_p30__p31   = 748
  integer, parameter :: k_p_p30__s31   = 749
  integer, parameter :: k_n_p31__p32   = 750
  integer, parameter :: k_p_p31__s32   = 751
  integer, parameter :: k_he4_p31__cl35   = 752
  integer, parameter :: k_n_p32__p33   = 753
  integer, parameter :: k_p_p32__s33   = 754
  integer, parameter :: k_he4_p32__cl36   = 755
  integer, parameter :: k_n_p33__p34   = 756
  integer, parameter :: k_p_p33__s34   = 757
  integer, parameter :: k_he4_p33__cl37   = 758
  integer, parameter :: k_p_p34__s35   = 759
  integer, parameter :: k_he4_p34__cl38   = 760
  integer, parameter :: k_n_s31__s32   = 761
  integer, parameter :: k_he4_s31__ar35   = 762
  integer, parameter :: k_n_s32__s33   = 763
  integer, parameter :: k_he4_s32__ar36   = 764
  integer, parameter :: k_n_s33__s34   = 765
  integer, parameter :: k_he4_s33__ar37   = 766
  integer, parameter :: k_n_s34__s35   = 767
  integer, parameter :: k_p_s34__cl35   = 768
  integer, parameter :: k_he4_s34__ar38   = 769
  integer, parameter :: k_n_s35__s36   = 770
  integer, parameter :: k_p_s35__cl36   = 771
  integer, parameter :: k_he4_s35__ar39   = 772
  integer, parameter :: k_n_s36__s37   = 773
  integer, parameter :: k_p_s36__cl37   = 774
  integer, parameter :: k_he4_s36__ar40   = 775
  integer, parameter :: k_p_s37__cl38   = 776
  integer, parameter :: k_he4_s37__ar41   = 777
  integer, parameter :: k_n_cl35__cl36   = 778
  integer, parameter :: k_p_cl35__ar36   = 779
  integer, parameter :: k_he4_cl35__k39   = 780
  integer, parameter :: k_n_cl36__cl37   = 781
  integer, parameter :: k_p_cl36__ar37   = 782
  integer, parameter :: k_he4_cl36__k40   = 783
  integer, parameter :: k_n_cl37__cl38   = 784
  integer, parameter :: k_p_cl37__ar38   = 785
  integer, parameter :: k_he4_cl37__k41   = 786
  integer, parameter :: k_p_cl38__ar39   = 787
  integer, parameter :: k_he4_cl38__k42   = 788
  integer, parameter :: k_n_ar35__ar36   = 789
  integer, parameter :: k_he4_ar35__ca39   = 790
  integer, parameter :: k_n_ar36__ar37   = 791
  integer, parameter :: k_he4_ar36__ca40   = 792
  integer, parameter :: k_n_ar37__ar38   = 793
  integer, parameter :: k_he4_ar37__ca41   = 794
  integer, parameter :: k_n_ar38__ar39   = 795
  integer, parameter :: k_p_ar38__k39   = 796
  integer, parameter :: k_he4_ar38__ca42   = 797
  integer, parameter :: k_n_ar39__ar40   = 798
  integer, parameter :: k_p_ar39__k40   = 799
  integer, parameter :: k_he4_ar39__ca43   = 800
  integer, parameter :: k_n_ar40__ar41   = 801
  integer, parameter :: k_p_ar40__k41   = 802
  integer, parameter :: k_he4_ar40__ca44   = 803
  integer, parameter :: k_p_ar41__k42   = 804
  integer, parameter :: k_he4_ar41__ca45   = 805
  integer, parameter :: k_n_k39__k40   = 806
  integer, parameter :: k_p_k39__ca40   = 807
  integer, parameter :: k_he4_k39__sc43   = 808
  integer, parameter :: k_n_k40__k41   = 809
  integer, parameter :: k_p_k40__ca41   = 810
  integer, parameter :: k_he4_k40__sc44   = 811
  integer, parameter :: k_n_k41__k42   = 812
  integer, parameter :: k_p_k41__ca42   = 813
  integer, parameter :: k_he4_k41__sc45   = 814
  integer, parameter :: k_n_k42__k43   = 815
  integer, parameter :: k_p_k42__ca43   = 816
  integer, parameter :: k_he4_k42__sc46   = 817
  integer, parameter :: k_n_k43__k44   = 818
  integer, parameter :: k_p_k43__ca44   = 819
  integer, parameter :: k_he4_k43__sc47   = 820
  integer, parameter :: k_p_k44__ca45   = 821
  integer, parameter :: k_he4_k44__sc48   = 822
  integer, parameter :: k_n_ca39__ca40   = 823
  integer, parameter :: k_he4_ca39__ti43   = 824
  integer, parameter :: k_n_ca40__ca41   = 825
  integer, parameter :: k_he4_ca40__ti44   = 826
  integer, parameter :: k_n_ca41__ca42   = 827
  integer, parameter :: k_he4_ca41__ti45   = 828
  integer, parameter :: k_n_ca42__ca43   = 829
  integer, parameter :: k_p_ca42__sc43   = 830
  integer, parameter :: k_he4_ca42__ti46   = 831
  integer, parameter :: k_n_ca43__ca44   = 832
  integer, parameter :: k_p_ca43__sc44   = 833
  integer, parameter :: k_he4_ca43__ti47   = 834
  integer, parameter :: k_n_ca44__ca45   = 835
  integer, parameter :: k_p_ca44__sc45   = 836
  integer, parameter :: k_he4_ca44__ti48   = 837
  integer, parameter :: k_n_ca45__ca46   = 838
  integer, parameter :: k_p_ca45__sc46   = 839
  integer, parameter :: k_he4_ca45__ti49   = 840
  integer, parameter :: k_n_ca46__ca47   = 841
  integer, parameter :: k_p_ca46__sc47   = 842
  integer, parameter :: k_he4_ca46__ti50   = 843
  integer, parameter :: k_n_ca47__ca48   = 844
  integer, parameter :: k_p_ca47__sc48   = 845
  integer, parameter :: k_he4_ca47__ti51   = 846
  integer, parameter :: k_n_ca48__ca49   = 847
  integer, parameter :: k_p_ca48__sc49   = 848
  integer, parameter :: k_he4_ca48__ti52   = 849
  integer, parameter :: k_p_ca49__sc50   = 850
  integer, parameter :: k_he4_ca49__ti53   = 851
  integer, parameter :: k_n_sc43__sc44   = 852
  integer, parameter :: k_p_sc43__ti44   = 853
  integer, parameter :: k_he4_sc43__v47   = 854
  integer, parameter :: k_n_sc44__sc45   = 855
  integer, parameter :: k_p_sc44__ti45   = 856
  integer, parameter :: k_he4_sc44__v48   = 857
  integer, parameter :: k_n_sc45__sc46   = 858
  integer, parameter :: k_p_sc45__ti46   = 859
  integer, parameter :: k_he4_sc45__v49   = 860
  integer, parameter :: k_n_sc46__sc47   = 861
  integer, parameter :: k_p_sc46__ti47   = 862
  integer, parameter :: k_he4_sc46__v50   = 863
  integer, parameter :: k_n_sc47__sc48   = 864
  integer, parameter :: k_p_sc47__ti48   = 865
  integer, parameter :: k_he4_sc47__v51   = 866
  integer, parameter :: k_n_sc48__sc49   = 867
  integer, parameter :: k_p_sc48__ti49   = 868
  integer, parameter :: k_he4_sc48__v52   = 869
  integer, parameter :: k_n_sc49__sc50   = 870
  integer, parameter :: k_p_sc49__ti50   = 871
  integer, parameter :: k_he4_sc49__v53   = 872
  integer, parameter :: k_n_sc50__sc51   = 873
  integer, parameter :: k_p_sc50__ti51   = 874
  integer, parameter :: k_he4_sc50__v54   = 875
  integer, parameter :: k_p_sc51__ti52   = 876
  integer, parameter :: k_he4_sc51__v55   = 877
  integer, parameter :: k_n_ti43__ti44   = 878
  integer, parameter :: k_he4_ti43__cr47   = 879
  integer, parameter :: k_n_ti44__ti45   = 880
  integer, parameter :: k_he4_ti44__cr48   = 881
  integer, parameter :: k_n_ti45__ti46   = 882
  integer, parameter :: k_he4_ti45__cr49   = 883
  integer, parameter :: k_n_ti46__ti47   = 884
  integer, parameter :: k_p_ti46__v47   = 885
  integer, parameter :: k_he4_ti46__cr50   = 886
  integer, parameter :: k_n_ti47__ti48   = 887
  integer, parameter :: k_p_ti47__v48   = 888
  integer, parameter :: k_he4_ti47__cr51   = 889
  integer, parameter :: k_n_ti48__ti49   = 890
  integer, parameter :: k_p_ti48__v49   = 891
  integer, parameter :: k_he4_ti48__cr52   = 892
  integer, parameter :: k_n_ti49__ti50   = 893
  integer, parameter :: k_p_ti49__v50   = 894
  integer, parameter :: k_he4_ti49__cr53   = 895
  integer, parameter :: k_n_ti50__ti51   = 896
  integer, parameter :: k_p_ti50__v51   = 897
  integer, parameter :: k_he4_ti50__cr54   = 898
  integer, parameter :: k_n_ti51__ti52   = 899
  integer, parameter :: k_p_ti51__v52   = 900
  integer, parameter :: k_he4_ti51__cr55   = 901
  integer, parameter :: k_n_ti52__ti53   = 902
  integer, parameter :: k_p_ti52__v53   = 903
  integer, parameter :: k_he4_ti52__cr56   = 904
  integer, parameter :: k_n_ti53__ti54   = 905
  integer, parameter :: k_p_ti53__v54   = 906
  integer, parameter :: k_he4_ti53__cr57   = 907
  integer, parameter :: k_p_ti54__v55   = 908
  integer, parameter :: k_he4_ti54__cr58   = 909
  integer, parameter :: k_n_v47__v48   = 910
  integer, parameter :: k_p_v47__cr48   = 911
  integer, parameter :: k_he4_v47__mn51   = 912
  integer, parameter :: k_n_v48__v49   = 913
  integer, parameter :: k_p_v48__cr49   = 914
  integer, parameter :: k_he4_v48__mn52   = 915
  integer, parameter :: k_n_v49__v50   = 916
  integer, parameter :: k_p_v49__cr50   = 917
  integer, parameter :: k_he4_v49__mn53   = 918
  integer, parameter :: k_n_v50__v51   = 919
  integer, parameter :: k_p_v50__cr51   = 920
  integer, parameter :: k_he4_v50__mn54   = 921
  integer, parameter :: k_n_v51__v52   = 922
  integer, parameter :: k_p_v51__cr52   = 923
  integer, parameter :: k_he4_v51__mn55   = 924
  integer, parameter :: k_n_v52__v53   = 925
  integer, parameter :: k_p_v52__cr53   = 926
  integer, parameter :: k_he4_v52__mn56   = 927
  integer, parameter :: k_n_v53__v54   = 928
  integer, parameter :: k_p_v53__cr54   = 929
  integer, parameter :: k_he4_v53__mn57   = 930
  integer, parameter :: k_n_v54__v55   = 931
  integer, parameter :: k_p_v54__cr55   = 932
  integer, parameter :: k_he4_v54__mn58   = 933
  integer, parameter :: k_n_v55__v56   = 934
  integer, parameter :: k_p_v55__cr56   = 935
  integer, parameter :: k_he4_v55__mn59   = 936
  integer, parameter :: k_p_v56__cr57   = 937
  integer, parameter :: k_n_cr47__cr48   = 938
  integer, parameter :: k_he4_cr47__fe51   = 939
  integer, parameter :: k_n_cr48__cr49   = 940
  integer, parameter :: k_he4_cr48__fe52   = 941
  integer, parameter :: k_n_cr49__cr50   = 942
  integer, parameter :: k_he4_cr49__fe53   = 943
  integer, parameter :: k_n_cr50__cr51   = 944
  integer, parameter :: k_p_cr50__mn51   = 945
  integer, parameter :: k_he4_cr50__fe54   = 946
  integer, parameter :: k_n_cr51__cr52   = 947
  integer, parameter :: k_p_cr51__mn52   = 948
  integer, parameter :: k_he4_cr51__fe55   = 949
  integer, parameter :: k_n_cr52__cr53   = 950
  integer, parameter :: k_p_cr52__mn53   = 951
  integer, parameter :: k_he4_cr52__fe56   = 952
  integer, parameter :: k_n_cr53__cr54   = 953
  integer, parameter :: k_p_cr53__mn54   = 954
  integer, parameter :: k_he4_cr53__fe57   = 955
  integer, parameter :: k_n_cr54__cr55   = 956
  integer, parameter :: k_p_cr54__mn55   = 957
  integer, parameter :: k_he4_cr54__fe58   = 958
  integer, parameter :: k_n_cr55__cr56   = 959
  integer, parameter :: k_p_cr55__mn56   = 960
  integer, parameter :: k_he4_cr55__fe59   = 961
  integer, parameter :: k_n_cr56__cr57   = 962
  integer, parameter :: k_p_cr56__mn57   = 963
  integer, parameter :: k_he4_cr56__fe60   = 964
  integer, parameter :: k_n_cr57__cr58   = 965
  integer, parameter :: k_p_cr57__mn58   = 966
  integer, parameter :: k_he4_cr57__fe61   = 967
  integer, parameter :: k_p_cr58__mn59   = 968
  integer, parameter :: k_he4_cr58__fe62   = 969
  integer, parameter :: k_n_mn51__mn52   = 970
  integer, parameter :: k_p_mn51__fe52   = 971
  integer, parameter :: k_he4_mn51__co55   = 972
  integer, parameter :: k_n_mn52__mn53   = 973
  integer, parameter :: k_p_mn52__fe53   = 974
  integer, parameter :: k_he4_mn52__co56   = 975
  integer, parameter :: k_n_mn53__mn54   = 976
  integer, parameter :: k_p_mn53__fe54   = 977
  integer, parameter :: k_he4_mn53__co57   = 978
  integer, parameter :: k_n_mn54__mn55   = 979
  integer, parameter :: k_p_mn54__fe55   = 980
  integer, parameter :: k_he4_mn54__co58   = 981
  integer, parameter :: k_n_mn55__mn56   = 982
  integer, parameter :: k_p_mn55__fe56   = 983
  integer, parameter :: k_he4_mn55__co59   = 984
  integer, parameter :: k_n_mn56__mn57   = 985
  integer, parameter :: k_p_mn56__fe57   = 986
  integer, parameter :: k_he4_mn56__co60   = 987
  integer, parameter :: k_n_mn57__mn58   = 988
  integer, parameter :: k_p_mn57__fe58   = 989
  integer, parameter :: k_he4_mn57__co61   = 990
  integer, parameter :: k_n_mn58__mn59   = 991
  integer, parameter :: k_p_mn58__fe59   = 992
  integer, parameter :: k_he4_mn58__co62   = 993
  integer, parameter :: k_p_mn59__fe60   = 994
  integer, parameter :: k_he4_mn59__co63   = 995
  integer, parameter :: k_n_fe51__fe52   = 996
  integer, parameter :: k_he4_fe51__ni55   = 997
  integer, parameter :: k_n_fe52__fe53   = 998
  integer, parameter :: k_he4_fe52__ni56   = 999
  integer, parameter :: k_n_fe53__fe54   = 1000
  integer, parameter :: k_he4_fe53__ni57   = 1001
  integer, parameter :: k_n_fe54__fe55   = 1002
  integer, parameter :: k_p_fe54__co55   = 1003
  integer, parameter :: k_he4_fe54__ni58   = 1004
  integer, parameter :: k_n_fe55__fe56   = 1005
  integer, parameter :: k_p_fe55__co56   = 1006
  integer, parameter :: k_he4_fe55__ni59   = 1007
  integer, parameter :: k_n_fe56__fe57   = 1008
  integer, parameter :: k_p_fe56__co57   = 1009
  integer, parameter :: k_he4_fe56__ni60   = 1010
  integer, parameter :: k_n_fe57__fe58   = 1011
  integer, parameter :: k_p_fe57__co58   = 1012
  integer, parameter :: k_he4_fe57__ni61   = 1013
  integer, parameter :: k_n_fe58__fe59   = 1014
  integer, parameter :: k_p_fe58__co59   = 1015
  integer, parameter :: k_he4_fe58__ni62   = 1016
  integer, parameter :: k_n_fe59__fe60   = 1017
  integer, parameter :: k_p_fe59__co60   = 1018
  integer, parameter :: k_he4_fe59__ni63   = 1019
  integer, parameter :: k_n_fe60__fe61   = 1020
  integer, parameter :: k_p_fe60__co61   = 1021
  integer, parameter :: k_he4_fe60__ni64   = 1022
  integer, parameter :: k_n_fe61__fe62   = 1023
  integer, parameter :: k_p_fe61__co62   = 1024
  integer, parameter :: k_he4_fe61__ni65   = 1025
  integer, parameter :: k_n_fe62__fe63   = 1026
  integer, parameter :: k_p_fe62__co63   = 1027
  integer, parameter :: k_he4_fe62__ni66   = 1028
  integer, parameter :: k_n_fe63__fe64   = 1029
  integer, parameter :: k_p_fe63__co64   = 1030
  integer, parameter :: k_he4_fe63__ni67   = 1031
  integer, parameter :: k_n_fe64__fe65   = 1032
  integer, parameter :: k_p_fe64__co65   = 1033
  integer, parameter :: k_he4_fe64__ni68   = 1034
  integer, parameter :: k_n_fe65__fe66   = 1035
  integer, parameter :: k_p_fe65__co66   = 1036
  integer, parameter :: k_p_fe66__co67   = 1037
  integer, parameter :: k_n_co55__co56   = 1038
  integer, parameter :: k_p_co55__ni56   = 1039
  integer, parameter :: k_he4_co55__cu59   = 1040
  integer, parameter :: k_n_co56__co57   = 1041
  integer, parameter :: k_p_co56__ni57   = 1042
  integer, parameter :: k_he4_co56__cu60   = 1043
  integer, parameter :: k_n_co57__co58   = 1044
  integer, parameter :: k_p_co57__ni58   = 1045
  integer, parameter :: k_he4_co57__cu61   = 1046
  integer, parameter :: k_n_co58__co59   = 1047
  integer, parameter :: k_p_co58__ni59   = 1048
  integer, parameter :: k_he4_co58__cu62   = 1049
  integer, parameter :: k_n_co59__co60   = 1050
  integer, parameter :: k_p_co59__ni60   = 1051
  integer, parameter :: k_he4_co59__cu63   = 1052
  integer, parameter :: k_n_co60__co61   = 1053
  integer, parameter :: k_p_co60__ni61   = 1054
  integer, parameter :: k_he4_co60__cu64   = 1055
  integer, parameter :: k_n_co61__co62   = 1056
  integer, parameter :: k_p_co61__ni62   = 1057
  integer, parameter :: k_he4_co61__cu65   = 1058
  integer, parameter :: k_n_co62__co63   = 1059
  integer, parameter :: k_p_co62__ni63   = 1060
  integer, parameter :: k_he4_co62__cu66   = 1061
  integer, parameter :: k_n_co63__co64   = 1062
  integer, parameter :: k_p_co63__ni64   = 1063
  integer, parameter :: k_n_co64__co65   = 1064
  integer, parameter :: k_p_co64__ni65   = 1065
  integer, parameter :: k_n_co65__co66   = 1066
  integer, parameter :: k_p_co65__ni66   = 1067
  integer, parameter :: k_n_co66__co67   = 1068
  integer, parameter :: k_p_co66__ni67   = 1069
  integer, parameter :: k_p_co67__ni68   = 1070
  integer, parameter :: k_n_ni55__ni56   = 1071
  integer, parameter :: k_he4_ni55__zn59   = 1072
  integer, parameter :: k_n_ni56__ni57   = 1073
  integer, parameter :: k_he4_ni56__zn60   = 1074
  integer, parameter :: k_n_ni57__ni58   = 1075
  integer, parameter :: k_he4_ni57__zn61   = 1076
  integer, parameter :: k_n_ni58__ni59   = 1077
  integer, parameter :: k_p_ni58__cu59   = 1078
  integer, parameter :: k_he4_ni58__zn62   = 1079
  integer, parameter :: k_n_ni59__ni60   = 1080
  integer, parameter :: k_p_ni59__cu60   = 1081
  integer, parameter :: k_he4_ni59__zn63   = 1082
  integer, parameter :: k_n_ni60__ni61   = 1083
  integer, parameter :: k_p_ni60__cu61   = 1084
  integer, parameter :: k_he4_ni60__zn64   = 1085
  integer, parameter :: k_n_ni61__ni62   = 1086
  integer, parameter :: k_p_ni61__cu62   = 1087
  integer, parameter :: k_he4_ni61__zn65   = 1088
  integer, parameter :: k_n_ni62__ni63   = 1089
  integer, parameter :: k_p_ni62__cu63   = 1090
  integer, parameter :: k_he4_ni62__zn66   = 1091
  integer, parameter :: k_n_ni63__ni64   = 1092
  integer, parameter :: k_p_ni63__cu64   = 1093
  integer, parameter :: k_n_ni64__ni65   = 1094
  integer, parameter :: k_p_ni64__cu65   = 1095
  integer, parameter :: k_n_ni65__ni66   = 1096
  integer, parameter :: k_p_ni65__cu66   = 1097
  integer, parameter :: k_n_ni66__ni67   = 1098
  integer, parameter :: k_n_ni67__ni68   = 1099
  integer, parameter :: k_n_cu59__cu60   = 1100
  integer, parameter :: k_p_cu59__zn60   = 1101
  integer, parameter :: k_n_cu60__cu61   = 1102
  integer, parameter :: k_p_cu60__zn61   = 1103
  integer, parameter :: k_n_cu61__cu62   = 1104
  integer, parameter :: k_p_cu61__zn62   = 1105
  integer, parameter :: k_n_cu62__cu63   = 1106
  integer, parameter :: k_p_cu62__zn63   = 1107
  integer, parameter :: k_n_cu63__cu64   = 1108
  integer, parameter :: k_p_cu63__zn64   = 1109
  integer, parameter :: k_n_cu64__cu65   = 1110
  integer, parameter :: k_p_cu64__zn65   = 1111
  integer, parameter :: k_n_cu65__cu66   = 1112
  integer, parameter :: k_p_cu65__zn66   = 1113
  integer, parameter :: k_n_zn59__zn60   = 1114
  integer, parameter :: k_n_zn60__zn61   = 1115
  integer, parameter :: k_n_zn61__zn62   = 1116
  integer, parameter :: k_n_zn62__zn63   = 1117
  integer, parameter :: k_n_zn63__zn64   = 1118
  integer, parameter :: k_n_zn64__zn65   = 1119
  integer, parameter :: k_n_zn65__zn66   = 1120
  integer, parameter :: k_d_d__n_he3   = 1121
  integer, parameter :: k_n_he3__d_d   = 1122
  integer, parameter :: k_d_he3__p_he4   = 1123
  integer, parameter :: k_he4_he3__p_li6   = 1124
  integer, parameter :: k_p_he4__d_he3   = 1125
  integer, parameter :: k_he4_he4__n_be7   = 1126
  integer, parameter :: k_he4_he4__p_li7   = 1127
  integer, parameter :: k_p_li6__he4_he3   = 1128
  integer, parameter :: k_d_li6__n_be7   = 1129
  integer, parameter :: k_d_li6__p_li7   = 1130
  integer, parameter :: k_he4_li6__p_be9   = 1131
  integer, parameter :: k_p_li7__n_be7   = 1132
  integer, parameter :: k_p_li7__d_li6   = 1133
  integer, parameter :: k_p_li7__he4_he4   = 1134
  integer, parameter :: k_he4_li7__n_b10   = 1135
  integer, parameter :: k_he4_li7__p_be10   = 1136
  integer, parameter :: k_n_be7__p_li7   = 1137
  integer, parameter :: k_n_be7__d_li6   = 1138
  integer, parameter :: k_n_be7__he4_he4   = 1139
  integer, parameter :: k_he4_be7__p_b10   = 1140
  integer, parameter :: k_p_be9__he4_li6   = 1141
  integer, parameter :: k_he4_be9__n_c12   = 1142
  integer, parameter :: k_p_be10__he4_li7   = 1143
  integer, parameter :: k_he4_be10__n_c13   = 1144
  integer, parameter :: k_n_b10__he4_li7   = 1145
  integer, parameter :: k_p_b10__he4_be7   = 1146
  integer, parameter :: k_he4_b10__n_n13   = 1147
  integer, parameter :: k_he4_b10__p_c13   = 1148
  integer, parameter :: k_he4_b11__n_n14   = 1149
  integer, parameter :: k_n_c12__he4_be9   = 1150
  integer, parameter :: k_he4_c12__n_o15   = 1151
  integer, parameter :: k_he4_c12__p_n15   = 1152
  integer, parameter :: k_c12_c12__n_mg23   = 1153
  integer, parameter :: k_c12_c12__p_na23   = 1154
  integer, parameter :: k_c12_c12__he4_ne20   = 1155
  integer, parameter :: k_n_c13__he4_be10   = 1156
  integer, parameter :: k_p_c13__n_n13   = 1157
  integer, parameter :: k_p_c13__he4_b10   = 1158
  integer, parameter :: k_d_c13__n_n14   = 1159
  integer, parameter :: k_he4_c13__n_o16   = 1160
  integer, parameter :: k_he4_c13__p_n16   = 1161
  integer, parameter :: k_n_n13__p_c13   = 1162
  integer, parameter :: k_n_n13__he4_b10   = 1163
  integer, parameter :: k_he4_n13__p_o16   = 1164
  integer, parameter :: k_n_n14__d_c13   = 1165
  integer, parameter :: k_n_n14__he4_b11   = 1166
  integer, parameter :: k_he4_n14__n_f17   = 1167
  integer, parameter :: k_he4_n14__p_o17   = 1168
  integer, parameter :: k_p_n15__n_o15   = 1169
  integer, parameter :: k_p_n15__he4_c12   = 1170
  integer, parameter :: k_he4_n15__n_f18   = 1171
  integer, parameter :: k_he4_n15__p_o18   = 1172
  integer, parameter :: k_p_n16__n_o16   = 1173
  integer, parameter :: k_p_n16__he4_c13   = 1174
  integer, parameter :: k_he4_n16__n_f19   = 1175
  integer, parameter :: k_he4_n16__p_o19   = 1176
  integer, parameter :: k_n_o15__p_n15   = 1177
  integer, parameter :: k_n_o15__he4_c12   = 1178
  integer, parameter :: k_he4_o15__p_f18   = 1179
  integer, parameter :: k_n_o16__p_n16   = 1180
  integer, parameter :: k_n_o16__he4_c13   = 1181
  integer, parameter :: k_p_o16__he4_n13   = 1182
  integer, parameter :: k_he4_o16__n_ne19   = 1183
  integer, parameter :: k_he4_o16__p_f19   = 1184
  integer, parameter :: k_c12_o16__n_si27   = 1185
  integer, parameter :: k_c12_o16__p_al27   = 1186
  integer, parameter :: k_c12_o16__he4_mg24   = 1187
  integer, parameter :: k_o16_o16__n_s31   = 1188
  integer, parameter :: k_o16_o16__p_p31   = 1189
  integer, parameter :: k_o16_o16__he4_si28   = 1190
  integer, parameter :: k_p_o17__n_f17   = 1191
  integer, parameter :: k_p_o17__he4_n14   = 1192
  integer, parameter :: k_he4_o17__n_ne20   = 1193
  integer, parameter :: k_he4_o17__p_f20   = 1194
  integer, parameter :: k_p_o18__n_f18   = 1195
  integer, parameter :: k_p_o18__he4_n15   = 1196
  integer, parameter :: k_he4_o18__n_ne21   = 1197
  integer, parameter :: k_p_o19__n_f19   = 1198
  integer, parameter :: k_p_o19__he4_n16   = 1199
  integer, parameter :: k_he4_o19__n_ne22   = 1200
  integer, parameter :: k_n_f17__p_o17   = 1201
  integer, parameter :: k_n_f17__he4_n14   = 1202
  integer, parameter :: k_he4_f17__p_ne20   = 1203
  integer, parameter :: k_n_f18__p_o18   = 1204
  integer, parameter :: k_n_f18__he4_n15   = 1205
  integer, parameter :: k_p_f18__he4_o15   = 1206
  integer, parameter :: k_he4_f18__n_na21   = 1207
  integer, parameter :: k_he4_f18__p_ne21   = 1208
  integer, parameter :: k_n_f19__p_o19   = 1209
  integer, parameter :: k_n_f19__he4_n16   = 1210
  integer, parameter :: k_p_f19__n_ne19   = 1211
  integer, parameter :: k_p_f19__he4_o16   = 1212
  integer, parameter :: k_he4_f19__n_na22   = 1213
  integer, parameter :: k_he4_f19__p_ne22   = 1214
  integer, parameter :: k_p_f20__n_ne20   = 1215
  integer, parameter :: k_p_f20__he4_o17   = 1216
  integer, parameter :: k_he4_f20__n_na23   = 1217
  integer, parameter :: k_he4_f20__p_ne23   = 1218
  integer, parameter :: k_n_ne19__p_f19   = 1219
  integer, parameter :: k_n_ne19__he4_o16   = 1220
  integer, parameter :: k_he4_ne19__p_na22   = 1221
  integer, parameter :: k_n_ne20__p_f20   = 1222
  integer, parameter :: k_n_ne20__he4_o17   = 1223
  integer, parameter :: k_p_ne20__he4_f17   = 1224
  integer, parameter :: k_he4_ne20__n_mg23   = 1225
  integer, parameter :: k_he4_ne20__p_na23   = 1226
  integer, parameter :: k_he4_ne20__c12_c12   = 1227
  integer, parameter :: k_c12_ne20__n_s31   = 1228
  integer, parameter :: k_c12_ne20__p_p31   = 1229
  integer, parameter :: k_c12_ne20__he4_si28   = 1230
  integer, parameter :: k_n_ne21__he4_o18   = 1231
  integer, parameter :: k_p_ne21__n_na21   = 1232
  integer, parameter :: k_p_ne21__he4_f18   = 1233
  integer, parameter :: k_he4_ne21__n_mg24   = 1234
  integer, parameter :: k_he4_ne21__p_na24   = 1235
  integer, parameter :: k_n_ne22__he4_o19   = 1236
  integer, parameter :: k_p_ne22__n_na22   = 1237
  integer, parameter :: k_p_ne22__he4_f19   = 1238
  integer, parameter :: k_he4_ne22__n_mg25   = 1239
  integer, parameter :: k_p_ne23__n_na23   = 1240
  integer, parameter :: k_p_ne23__he4_f20   = 1241
  integer, parameter :: k_he4_ne23__n_mg26   = 1242
  integer, parameter :: k_n_na21__p_ne21   = 1243
  integer, parameter :: k_n_na21__he4_f18   = 1244
  integer, parameter :: k_he4_na21__p_mg24   = 1245
  integer, parameter :: k_n_na22__p_ne22   = 1246
  integer, parameter :: k_n_na22__he4_f19   = 1247
  integer, parameter :: k_p_na22__he4_ne19   = 1248
  integer, parameter :: k_he4_na22__n_al25   = 1249
  integer, parameter :: k_he4_na22__p_mg25   = 1250
  integer, parameter :: k_n_na23__p_ne23   = 1251
  integer, parameter :: k_n_na23__he4_f20   = 1252
  integer, parameter :: k_p_na23__n_mg23   = 1253
  integer, parameter :: k_p_na23__he4_ne20   = 1254
  integer, parameter :: k_p_na23__c12_c12   = 1255
  integer, parameter :: k_he4_na23__n_al26   = 1256
  integer, parameter :: k_he4_na23__p_mg26   = 1257
  integer, parameter :: k_p_na24__n_mg24   = 1258
  integer, parameter :: k_p_na24__he4_ne21   = 1259
  integer, parameter :: k_he4_na24__n_al27   = 1260
  integer, parameter :: k_he4_na24__p_mg27   = 1261
  integer, parameter :: k_n_mg23__p_na23   = 1262
  integer, parameter :: k_n_mg23__he4_ne20   = 1263
  integer, parameter :: k_n_mg23__c12_c12   = 1264
  integer, parameter :: k_he4_mg23__p_al26   = 1265
  integer, parameter :: k_n_mg24__p_na24   = 1266
  integer, parameter :: k_n_mg24__he4_ne21   = 1267
  integer, parameter :: k_p_mg24__he4_na21   = 1268
  integer, parameter :: k_he4_mg24__n_si27   = 1269
  integer, parameter :: k_he4_mg24__p_al27   = 1270
  integer, parameter :: k_he4_mg24__c12_o16   = 1271
  integer, parameter :: k_n_mg25__he4_ne22   = 1272
  integer, parameter :: k_p_mg25__n_al25   = 1273
  integer, parameter :: k_p_mg25__he4_na22   = 1274
  integer, parameter :: k_he4_mg25__n_si28   = 1275
  integer, parameter :: k_he4_mg25__p_al28   = 1276
  integer, parameter :: k_n_mg26__he4_ne23   = 1277
  integer, parameter :: k_p_mg26__n_al26   = 1278
  integer, parameter :: k_p_mg26__he4_na23   = 1279
  integer, parameter :: k_he4_mg26__n_si29   = 1280
  integer, parameter :: k_p_mg27__n_al27   = 1281
  integer, parameter :: k_p_mg27__he4_na24   = 1282
  integer, parameter :: k_he4_mg27__n_si30   = 1283
  integer, parameter :: k_n_al25__p_mg25   = 1284
  integer, parameter :: k_n_al25__he4_na22   = 1285
  integer, parameter :: k_he4_al25__p_si28   = 1286
  integer, parameter :: k_n_al26__p_mg26   = 1287
  integer, parameter :: k_n_al26__he4_na23   = 1288
  integer, parameter :: k_p_al26__he4_mg23   = 1289
  integer, parameter :: k_he4_al26__p_si29   = 1290
  integer, parameter :: k_n_al27__p_mg27   = 1291
  integer, parameter :: k_n_al27__he4_na24   = 1292
  integer, parameter :: k_p_al27__n_si27   = 1293
  integer, parameter :: k_p_al27__he4_mg24   = 1294
  integer, parameter :: k_p_al27__c12_o16   = 1295
  integer, parameter :: k_he4_al27__n_p30   = 1296
  integer, parameter :: k_he4_al27__p_si30   = 1297
  integer, parameter :: k_p_al28__n_si28   = 1298
  integer, parameter :: k_p_al28__he4_mg25   = 1299
  integer, parameter :: k_he4_al28__n_p31   = 1300
  integer, parameter :: k_he4_al28__p_si31   = 1301
  integer, parameter :: k_n_si27__p_al27   = 1302
  integer, parameter :: k_n_si27__he4_mg24   = 1303
  integer, parameter :: k_n_si27__c12_o16   = 1304
  integer, parameter :: k_he4_si27__p_p30   = 1305
  integer, parameter :: k_n_si28__p_al28   = 1306
  integer, parameter :: k_n_si28__he4_mg25   = 1307
  integer, parameter :: k_p_si28__he4_al25   = 1308
  integer, parameter :: k_he4_si28__n_s31   = 1309
  integer, parameter :: k_he4_si28__p_p31   = 1310
  integer, parameter :: k_he4_si28__c12_ne20   = 1311
  integer, parameter :: k_he4_si28__o16_o16   = 1312
  integer, parameter :: k_n_si29__he4_mg26   = 1313
  integer, parameter :: k_p_si29__he4_al26   = 1314
  integer, parameter :: k_he4_si29__n_s32   = 1315
  integer, parameter :: k_he4_si29__p_p32   = 1316
  integer, parameter :: k_n_si30__he4_mg27   = 1317
  integer, parameter :: k_p_si30__n_p30   = 1318
  integer, parameter :: k_p_si30__he4_al27   = 1319
  integer, parameter :: k_he4_si30__n_s33   = 1320
  integer, parameter :: k_he4_si30__p_p33   = 1321
  integer, parameter :: k_p_si31__n_p31   = 1322
  integer, parameter :: k_p_si31__he4_al28   = 1323
  integer, parameter :: k_he4_si31__n_s34   = 1324
  integer, parameter :: k_he4_si31__p_p34   = 1325
  integer, parameter :: k_p_si32__n_p32   = 1326
  integer, parameter :: k_he4_si32__n_s35   = 1327
  integer, parameter :: k_p_si33__n_p33   = 1328
  integer, parameter :: k_he4_si33__n_s36   = 1329
  integer, parameter :: k_n_p30__p_si30   = 1330
  integer, parameter :: k_n_p30__he4_al27   = 1331
  integer, parameter :: k_p_p30__he4_si27   = 1332
  integer, parameter :: k_he4_p30__p_s33   = 1333
  integer, parameter :: k_n_p31__p_si31   = 1334
  integer, parameter :: k_n_p31__he4_al28   = 1335
  integer, parameter :: k_p_p31__n_s31   = 1336
  integer, parameter :: k_p_p31__he4_si28   = 1337
  integer, parameter :: k_p_p31__c12_ne20   = 1338
  integer, parameter :: k_p_p31__o16_o16   = 1339
  integer, parameter :: k_he4_p31__p_s34   = 1340
  integer, parameter :: k_n_p32__p_si32   = 1341
  integer, parameter :: k_p_p32__n_s32   = 1342
  integer, parameter :: k_p_p32__he4_si29   = 1343
  integer, parameter :: k_he4_p32__n_cl35   = 1344
  integer, parameter :: k_he4_p32__p_s35   = 1345
  integer, parameter :: k_n_p33__p_si33   = 1346
  integer, parameter :: k_p_p33__n_s33   = 1347
  integer, parameter :: k_p_p33__he4_si30   = 1348
  integer, parameter :: k_he4_p33__n_cl36   = 1349
  integer, parameter :: k_he4_p33__p_s36   = 1350
  integer, parameter :: k_p_p34__n_s34   = 1351
  integer, parameter :: k_p_p34__he4_si31   = 1352
  integer, parameter :: k_he4_p34__n_cl37   = 1353
  integer, parameter :: k_he4_p34__p_s37   = 1354
  integer, parameter :: k_n_s31__p_p31   = 1355
  integer, parameter :: k_n_s31__he4_si28   = 1356
  integer, parameter :: k_n_s31__c12_ne20   = 1357
  integer, parameter :: k_n_s31__o16_o16   = 1358
  integer, parameter :: k_n_s32__p_p32   = 1359
  integer, parameter :: k_n_s32__he4_si29   = 1360
  integer, parameter :: k_he4_s32__n_ar35   = 1361
  integer, parameter :: k_he4_s32__p_cl35   = 1362
  integer, parameter :: k_n_s33__p_p33   = 1363
  integer, parameter :: k_n_s33__he4_si30   = 1364
  integer, parameter :: k_p_s33__he4_p30   = 1365
  integer, parameter :: k_he4_s33__n_ar36   = 1366
  integer, parameter :: k_he4_s33__p_cl36   = 1367
  integer, parameter :: k_n_s34__p_p34   = 1368
  integer, parameter :: k_n_s34__he4_si31   = 1369
  integer, parameter :: k_p_s34__he4_p31   = 1370
  integer, parameter :: k_he4_s34__n_ar37   = 1371
  integer, parameter :: k_he4_s34__p_cl37   = 1372
  integer, parameter :: k_n_s35__he4_si32   = 1373
  integer, parameter :: k_p_s35__n_cl35   = 1374
  integer, parameter :: k_p_s35__he4_p32   = 1375
  integer, parameter :: k_he4_s35__n_ar38   = 1376
  integer, parameter :: k_he4_s35__p_cl38   = 1377
  integer, parameter :: k_n_s36__he4_si33   = 1378
  integer, parameter :: k_p_s36__n_cl36   = 1379
  integer, parameter :: k_p_s36__he4_p33   = 1380
  integer, parameter :: k_he4_s36__n_ar39   = 1381
  integer, parameter :: k_p_s37__n_cl37   = 1382
  integer, parameter :: k_p_s37__he4_p34   = 1383
  integer, parameter :: k_he4_s37__n_ar40   = 1384
  integer, parameter :: k_n_cl35__p_s35   = 1385
  integer, parameter :: k_n_cl35__he4_p32   = 1386
  integer, parameter :: k_p_cl35__n_ar35   = 1387
  integer, parameter :: k_p_cl35__he4_s32   = 1388
  integer, parameter :: k_he4_cl35__p_ar38   = 1389
  integer, parameter :: k_n_cl36__p_s36   = 1390
  integer, parameter :: k_n_cl36__he4_p33   = 1391
  integer, parameter :: k_p_cl36__n_ar36   = 1392
  integer, parameter :: k_p_cl36__he4_s33   = 1393
  integer, parameter :: k_he4_cl36__n_k39   = 1394
  integer, parameter :: k_he4_cl36__p_ar39   = 1395
  integer, parameter :: k_n_cl37__p_s37   = 1396
  integer, parameter :: k_n_cl37__he4_p34   = 1397
  integer, parameter :: k_p_cl37__n_ar37   = 1398
  integer, parameter :: k_p_cl37__he4_s34   = 1399
  integer, parameter :: k_he4_cl37__n_k40   = 1400
  integer, parameter :: k_he4_cl37__p_ar40   = 1401
  integer, parameter :: k_p_cl38__n_ar38   = 1402
  integer, parameter :: k_p_cl38__he4_s35   = 1403
  integer, parameter :: k_he4_cl38__n_k41   = 1404
  integer, parameter :: k_he4_cl38__p_ar41   = 1405
  integer, parameter :: k_n_ar35__p_cl35   = 1406
  integer, parameter :: k_n_ar35__he4_s32   = 1407
  integer, parameter :: k_n_ar36__p_cl36   = 1408
  integer, parameter :: k_n_ar36__he4_s33   = 1409
  integer, parameter :: k_he4_ar36__n_ca39   = 1410
  integer, parameter :: k_he4_ar36__p_k39   = 1411
  integer, parameter :: k_n_ar37__p_cl37   = 1412
  integer, parameter :: k_n_ar37__he4_s34   = 1413
  integer, parameter :: k_he4_ar37__n_ca40   = 1414
  integer, parameter :: k_he4_ar37__p_k40   = 1415
  integer, parameter :: k_n_ar38__p_cl38   = 1416
  integer, parameter :: k_n_ar38__he4_s35   = 1417
  integer, parameter :: k_p_ar38__he4_cl35   = 1418
  integer, parameter :: k_he4_ar38__n_ca41   = 1419
  integer, parameter :: k_he4_ar38__p_k41   = 1420
  integer, parameter :: k_n_ar39__he4_s36   = 1421
  integer, parameter :: k_p_ar39__n_k39   = 1422
  integer, parameter :: k_p_ar39__he4_cl36   = 1423
  integer, parameter :: k_he4_ar39__n_ca42   = 1424
  integer, parameter :: k_he4_ar39__p_k42   = 1425
  integer, parameter :: k_n_ar40__he4_s37   = 1426
  integer, parameter :: k_p_ar40__n_k40   = 1427
  integer, parameter :: k_p_ar40__he4_cl37   = 1428
  integer, parameter :: k_he4_ar40__n_ca43   = 1429
  integer, parameter :: k_he4_ar40__p_k43   = 1430
  integer, parameter :: k_p_ar41__n_k41   = 1431
  integer, parameter :: k_p_ar41__he4_cl38   = 1432
  integer, parameter :: k_he4_ar41__n_ca44   = 1433
  integer, parameter :: k_he4_ar41__p_k44   = 1434
  integer, parameter :: k_n_k39__p_ar39   = 1435
  integer, parameter :: k_n_k39__he4_cl36   = 1436
  integer, parameter :: k_p_k39__n_ca39   = 1437
  integer, parameter :: k_p_k39__he4_ar36   = 1438
  integer, parameter :: k_he4_k39__p_ca42   = 1439
  integer, parameter :: k_n_k40__p_ar40   = 1440
  integer, parameter :: k_n_k40__he4_cl37   = 1441
  integer, parameter :: k_p_k40__n_ca40   = 1442
  integer, parameter :: k_p_k40__he4_ar37   = 1443
  integer, parameter :: k_he4_k40__n_sc43   = 1444
  integer, parameter :: k_he4_k40__p_ca43   = 1445
  integer, parameter :: k_n_k41__p_ar41   = 1446
  integer, parameter :: k_n_k41__he4_cl38   = 1447
  integer, parameter :: k_p_k41__n_ca41   = 1448
  integer, parameter :: k_p_k41__he4_ar38   = 1449
  integer, parameter :: k_he4_k41__n_sc44   = 1450
  integer, parameter :: k_he4_k41__p_ca44   = 1451
  integer, parameter :: k_p_k42__n_ca42   = 1452
  integer, parameter :: k_p_k42__he4_ar39   = 1453
  integer, parameter :: k_he4_k42__n_sc45   = 1454
  integer, parameter :: k_he4_k42__p_ca45   = 1455
  integer, parameter :: k_p_k43__n_ca43   = 1456
  integer, parameter :: k_p_k43__he4_ar40   = 1457
  integer, parameter :: k_he4_k43__n_sc46   = 1458
  integer, parameter :: k_he4_k43__p_ca46   = 1459
  integer, parameter :: k_p_k44__n_ca44   = 1460
  integer, parameter :: k_p_k44__he4_ar41   = 1461
  integer, parameter :: k_he4_k44__n_sc47   = 1462
  integer, parameter :: k_he4_k44__p_ca47   = 1463
  integer, parameter :: k_n_ca39__p_k39   = 1464
  integer, parameter :: k_n_ca39__he4_ar36   = 1465
  integer, parameter :: k_n_ca40__p_k40   = 1466
  integer, parameter :: k_n_ca40__he4_ar37   = 1467
  integer, parameter :: k_he4_ca40__n_ti43   = 1468
  integer, parameter :: k_he4_ca40__p_sc43   = 1469
  integer, parameter :: k_n_ca41__p_k41   = 1470
  integer, parameter :: k_n_ca41__he4_ar38   = 1471
  integer, parameter :: k_he4_ca41__n_ti44   = 1472
  integer, parameter :: k_he4_ca41__p_sc44   = 1473
  integer, parameter :: k_n_ca42__p_k42   = 1474
  integer, parameter :: k_n_ca42__he4_ar39   = 1475
  integer, parameter :: k_p_ca42__he4_k39   = 1476
  integer, parameter :: k_he4_ca42__n_ti45   = 1477
  integer, parameter :: k_he4_ca42__p_sc45   = 1478
  integer, parameter :: k_n_ca43__p_k43   = 1479
  integer, parameter :: k_n_ca43__he4_ar40   = 1480
  integer, parameter :: k_p_ca43__n_sc43   = 1481
  integer, parameter :: k_p_ca43__he4_k40   = 1482
  integer, parameter :: k_he4_ca43__n_ti46   = 1483
  integer, parameter :: k_he4_ca43__p_sc46   = 1484
  integer, parameter :: k_n_ca44__p_k44   = 1485
  integer, parameter :: k_n_ca44__he4_ar41   = 1486
  integer, parameter :: k_p_ca44__n_sc44   = 1487
  integer, parameter :: k_p_ca44__he4_k41   = 1488
  integer, parameter :: k_he4_ca44__n_ti47   = 1489
  integer, parameter :: k_he4_ca44__p_sc47   = 1490
  integer, parameter :: k_p_ca45__n_sc45   = 1491
  integer, parameter :: k_p_ca45__he4_k42   = 1492
  integer, parameter :: k_he4_ca45__n_ti48   = 1493
  integer, parameter :: k_he4_ca45__p_sc48   = 1494
  integer, parameter :: k_p_ca46__n_sc46   = 1495
  integer, parameter :: k_p_ca46__he4_k43   = 1496
  integer, parameter :: k_he4_ca46__n_ti49   = 1497
  integer, parameter :: k_he4_ca46__p_sc49   = 1498
  integer, parameter :: k_p_ca47__n_sc47   = 1499
  integer, parameter :: k_p_ca47__he4_k44   = 1500
  integer, parameter :: k_he4_ca47__n_ti50   = 1501
  integer, parameter :: k_he4_ca47__p_sc50   = 1502
  integer, parameter :: k_p_ca48__n_sc48   = 1503
  integer, parameter :: k_he4_ca48__n_ti51   = 1504
  integer, parameter :: k_he4_ca48__p_sc51   = 1505
  integer, parameter :: k_p_ca49__n_sc49   = 1506
  integer, parameter :: k_he4_ca49__n_ti52   = 1507
  integer, parameter :: k_n_sc43__p_ca43   = 1508
  integer, parameter :: k_n_sc43__he4_k40   = 1509
  integer, parameter :: k_p_sc43__n_ti43   = 1510
  integer, parameter :: k_p_sc43__he4_ca40   = 1511
  integer, parameter :: k_he4_sc43__p_ti46   = 1512
  integer, parameter :: k_n_sc44__p_ca44   = 1513
  integer, parameter :: k_n_sc44__he4_k41   = 1514
  integer, parameter :: k_p_sc44__n_ti44   = 1515
  integer, parameter :: k_p_sc44__he4_ca41   = 1516
  integer, parameter :: k_he4_sc44__n_v47   = 1517
  integer, parameter :: k_he4_sc44__p_ti47   = 1518
  integer, parameter :: k_n_sc45__p_ca45   = 1519
  integer, parameter :: k_n_sc45__he4_k42   = 1520
  integer, parameter :: k_p_sc45__n_ti45   = 1521
  integer, parameter :: k_p_sc45__he4_ca42   = 1522
  integer, parameter :: k_he4_sc45__n_v48   = 1523
  integer, parameter :: k_he4_sc45__p_ti48   = 1524
  integer, parameter :: k_n_sc46__p_ca46   = 1525
  integer, parameter :: k_n_sc46__he4_k43   = 1526
  integer, parameter :: k_p_sc46__n_ti46   = 1527
  integer, parameter :: k_p_sc46__he4_ca43   = 1528
  integer, parameter :: k_he4_sc46__n_v49   = 1529
  integer, parameter :: k_he4_sc46__p_ti49   = 1530
  integer, parameter :: k_n_sc47__p_ca47   = 1531
  integer, parameter :: k_n_sc47__he4_k44   = 1532
  integer, parameter :: k_p_sc47__n_ti47   = 1533
  integer, parameter :: k_p_sc47__he4_ca44   = 1534
  integer, parameter :: k_he4_sc47__n_v50   = 1535
  integer, parameter :: k_he4_sc47__p_ti50   = 1536
  integer, parameter :: k_n_sc48__p_ca48   = 1537
  integer, parameter :: k_p_sc48__n_ti48   = 1538
  integer, parameter :: k_p_sc48__he4_ca45   = 1539
  integer, parameter :: k_he4_sc48__n_v51   = 1540
  integer, parameter :: k_he4_sc48__p_ti51   = 1541
  integer, parameter :: k_n_sc49__p_ca49   = 1542
  integer, parameter :: k_p_sc49__n_ti49   = 1543
  integer, parameter :: k_p_sc49__he4_ca46   = 1544
  integer, parameter :: k_he4_sc49__n_v52   = 1545
  integer, parameter :: k_he4_sc49__p_ti52   = 1546
  integer, parameter :: k_p_sc50__n_ti50   = 1547
  integer, parameter :: k_p_sc50__he4_ca47   = 1548
  integer, parameter :: k_he4_sc50__n_v53   = 1549
  integer, parameter :: k_he4_sc50__p_ti53   = 1550
  integer, parameter :: k_p_sc51__n_ti51   = 1551
  integer, parameter :: k_p_sc51__he4_ca48   = 1552
  integer, parameter :: k_he4_sc51__n_v54   = 1553
  integer, parameter :: k_he4_sc51__p_ti54   = 1554
  integer, parameter :: k_n_ti43__p_sc43   = 1555
  integer, parameter :: k_n_ti43__he4_ca40   = 1556
  integer, parameter :: k_n_ti44__p_sc44   = 1557
  integer, parameter :: k_n_ti44__he4_ca41   = 1558
  integer, parameter :: k_he4_ti44__n_cr47   = 1559
  integer, parameter :: k_he4_ti44__p_v47   = 1560
  integer, parameter :: k_n_ti45__p_sc45   = 1561
  integer, parameter :: k_n_ti45__he4_ca42   = 1562
  integer, parameter :: k_he4_ti45__n_cr48   = 1563
  integer, parameter :: k_he4_ti45__p_v48   = 1564
  integer, parameter :: k_n_ti46__p_sc46   = 1565
  integer, parameter :: k_n_ti46__he4_ca43   = 1566
  integer, parameter :: k_p_ti46__he4_sc43   = 1567
  integer, parameter :: k_he4_ti46__n_cr49   = 1568
  integer, parameter :: k_he4_ti46__p_v49   = 1569
  integer, parameter :: k_n_ti47__p_sc47   = 1570
  integer, parameter :: k_n_ti47__he4_ca44   = 1571
  integer, parameter :: k_p_ti47__n_v47   = 1572
  integer, parameter :: k_p_ti47__he4_sc44   = 1573
  integer, parameter :: k_he4_ti47__n_cr50   = 1574
  integer, parameter :: k_he4_ti47__p_v50   = 1575
  integer, parameter :: k_n_ti48__p_sc48   = 1576
  integer, parameter :: k_n_ti48__he4_ca45   = 1577
  integer, parameter :: k_p_ti48__n_v48   = 1578
  integer, parameter :: k_p_ti48__he4_sc45   = 1579
  integer, parameter :: k_he4_ti48__n_cr51   = 1580
  integer, parameter :: k_he4_ti48__p_v51   = 1581
  integer, parameter :: k_n_ti49__p_sc49   = 1582
  integer, parameter :: k_n_ti49__he4_ca46   = 1583
  integer, parameter :: k_p_ti49__n_v49   = 1584
  integer, parameter :: k_p_ti49__he4_sc46   = 1585
  integer, parameter :: k_he4_ti49__n_cr52   = 1586
  integer, parameter :: k_he4_ti49__p_v52   = 1587
  integer, parameter :: k_n_ti50__p_sc50   = 1588
  integer, parameter :: k_n_ti50__he4_ca47   = 1589
  integer, parameter :: k_p_ti50__n_v50   = 1590
  integer, parameter :: k_p_ti50__he4_sc47   = 1591
  integer, parameter :: k_he4_ti50__n_cr53   = 1592
  integer, parameter :: k_he4_ti50__p_v53   = 1593
  integer, parameter :: k_n_ti51__p_sc51   = 1594
  integer, parameter :: k_n_ti51__he4_ca48   = 1595
  integer, parameter :: k_p_ti51__n_v51   = 1596
  integer, parameter :: k_p_ti51__he4_sc48   = 1597
  integer, parameter :: k_he4_ti51__n_cr54   = 1598
  integer, parameter :: k_he4_ti51__p_v54   = 1599
  integer, parameter :: k_n_ti52__he4_ca49   = 1600
  integer, parameter :: k_p_ti52__n_v52   = 1601
  integer, parameter :: k_p_ti52__he4_sc49   = 1602
  integer, parameter :: k_he4_ti52__n_cr55   = 1603
  integer, parameter :: k_he4_ti52__p_v55   = 1604
  integer, parameter :: k_p_ti53__n_v53   = 1605
  integer, parameter :: k_p_ti53__he4_sc50   = 1606
  integer, parameter :: k_he4_ti53__n_cr56   = 1607
  integer, parameter :: k_he4_ti53__p_v56   = 1608
  integer, parameter :: k_p_ti54__n_v54   = 1609
  integer, parameter :: k_p_ti54__he4_sc51   = 1610
  integer, parameter :: k_he4_ti54__n_cr57   = 1611
  integer, parameter :: k_n_v47__p_ti47   = 1612
  integer, parameter :: k_n_v47__he4_sc44   = 1613
  integer, parameter :: k_p_v47__n_cr47   = 1614
  integer, parameter :: k_p_v47__he4_ti44   = 1615
  integer, parameter :: k_he4_v47__p_cr50   = 1616
  integer, parameter :: k_n_v48__p_ti48   = 1617
  integer, parameter :: k_n_v48__he4_sc45   = 1618
  integer, parameter :: k_p_v48__n_cr48   = 1619
  integer, parameter :: k_p_v48__he4_ti45   = 1620
  integer, parameter :: k_he4_v48__n_mn51   = 1621
  integer, parameter :: k_he4_v48__p_cr51   = 1622
  integer, parameter :: k_n_v49__p_ti49   = 1623
  integer, parameter :: k_n_v49__he4_sc46   = 1624
  integer, parameter :: k_p_v49__n_cr49   = 1625
  integer, parameter :: k_p_v49__he4_ti46   = 1626
  integer, parameter :: k_he4_v49__n_mn52   = 1627
  integer, parameter :: k_he4_v49__p_cr52   = 1628
  integer, parameter :: k_n_v50__p_ti50   = 1629
  integer, parameter :: k_n_v50__he4_sc47   = 1630
  integer, parameter :: k_p_v50__n_cr50   = 1631
  integer, parameter :: k_p_v50__he4_ti47   = 1632
  integer, parameter :: k_he4_v50__n_mn53   = 1633
  integer, parameter :: k_he4_v50__p_cr53   = 1634
  integer, parameter :: k_n_v51__p_ti51   = 1635
  integer, parameter :: k_n_v51__he4_sc48   = 1636
  integer, parameter :: k_p_v51__n_cr51   = 1637
  integer, parameter :: k_p_v51__he4_ti48   = 1638
  integer, parameter :: k_he4_v51__n_mn54   = 1639
  integer, parameter :: k_he4_v51__p_cr54   = 1640
  integer, parameter :: k_n_v52__p_ti52   = 1641
  integer, parameter :: k_n_v52__he4_sc49   = 1642
  integer, parameter :: k_p_v52__n_cr52   = 1643
  integer, parameter :: k_p_v52__he4_ti49   = 1644
  integer, parameter :: k_he4_v52__n_mn55   = 1645
  integer, parameter :: k_he4_v52__p_cr55   = 1646
  integer, parameter :: k_n_v53__p_ti53   = 1647
  integer, parameter :: k_n_v53__he4_sc50   = 1648
  integer, parameter :: k_p_v53__n_cr53   = 1649
  integer, parameter :: k_p_v53__he4_ti50   = 1650
  integer, parameter :: k_he4_v53__n_mn56   = 1651
  integer, parameter :: k_he4_v53__p_cr56   = 1652
  integer, parameter :: k_n_v54__p_ti54   = 1653
  integer, parameter :: k_n_v54__he4_sc51   = 1654
  integer, parameter :: k_p_v54__n_cr54   = 1655
  integer, parameter :: k_p_v54__he4_ti51   = 1656
  integer, parameter :: k_he4_v54__n_mn57   = 1657
  integer, parameter :: k_he4_v54__p_cr57   = 1658
  integer, parameter :: k_p_v55__n_cr55   = 1659
  integer, parameter :: k_p_v55__he4_ti52   = 1660
  integer, parameter :: k_he4_v55__n_mn58   = 1661
  integer, parameter :: k_he4_v55__p_cr58   = 1662
  integer, parameter :: k_p_v56__n_cr56   = 1663
  integer, parameter :: k_p_v56__he4_ti53   = 1664
  integer, parameter :: k_he4_v56__n_mn59   = 1665
  integer, parameter :: k_n_cr47__p_v47   = 1666
  integer, parameter :: k_n_cr47__he4_ti44   = 1667
  integer, parameter :: k_n_cr48__p_v48   = 1668
  integer, parameter :: k_n_cr48__he4_ti45   = 1669
  integer, parameter :: k_he4_cr48__n_fe51   = 1670
  integer, parameter :: k_he4_cr48__p_mn51   = 1671
  integer, parameter :: k_n_cr49__p_v49   = 1672
  integer, parameter :: k_n_cr49__he4_ti46   = 1673
  integer, parameter :: k_he4_cr49__n_fe52   = 1674
  integer, parameter :: k_he4_cr49__p_mn52   = 1675
  integer, parameter :: k_n_cr50__p_v50   = 1676
  integer, parameter :: k_n_cr50__he4_ti47   = 1677
  integer, parameter :: k_p_cr50__he4_v47   = 1678
  integer, parameter :: k_he4_cr50__n_fe53   = 1679
  integer, parameter :: k_he4_cr50__p_mn53   = 1680
  integer, parameter :: k_n_cr51__p_v51   = 1681
  integer, parameter :: k_n_cr51__he4_ti48   = 1682
  integer, parameter :: k_p_cr51__n_mn51   = 1683
  integer, parameter :: k_p_cr51__he4_v48   = 1684
  integer, parameter :: k_he4_cr51__n_fe54   = 1685
  integer, parameter :: k_he4_cr51__p_mn54   = 1686
  integer, parameter :: k_n_cr52__p_v52   = 1687
  integer, parameter :: k_n_cr52__he4_ti49   = 1688
  integer, parameter :: k_p_cr52__n_mn52   = 1689
  integer, parameter :: k_p_cr52__he4_v49   = 1690
  integer, parameter :: k_he4_cr52__n_fe55   = 1691
  integer, parameter :: k_he4_cr52__p_mn55   = 1692
  integer, parameter :: k_n_cr53__p_v53   = 1693
  integer, parameter :: k_n_cr53__he4_ti50   = 1694
  integer, parameter :: k_p_cr53__n_mn53   = 1695
  integer, parameter :: k_p_cr53__he4_v50   = 1696
  integer, parameter :: k_he4_cr53__n_fe56   = 1697
  integer, parameter :: k_he4_cr53__p_mn56   = 1698
  integer, parameter :: k_n_cr54__p_v54   = 1699
  integer, parameter :: k_n_cr54__he4_ti51   = 1700
  integer, parameter :: k_p_cr54__n_mn54   = 1701
  integer, parameter :: k_p_cr54__he4_v51   = 1702
  integer, parameter :: k_he4_cr54__n_fe57   = 1703
  integer, parameter :: k_he4_cr54__p_mn57   = 1704
  integer, parameter :: k_n_cr55__p_v55   = 1705
  integer, parameter :: k_n_cr55__he4_ti52   = 1706
  integer, parameter :: k_p_cr55__n_mn55   = 1707
  integer, parameter :: k_p_cr55__he4_v52   = 1708
  integer, parameter :: k_he4_cr55__n_fe58   = 1709
  integer, parameter :: k_he4_cr55__p_mn58   = 1710
  integer, parameter :: k_n_cr56__p_v56   = 1711
  integer, parameter :: k_n_cr56__he4_ti53   = 1712
  integer, parameter :: k_p_cr56__n_mn56   = 1713
  integer, parameter :: k_p_cr56__he4_v53   = 1714
  integer, parameter :: k_he4_cr56__n_fe59   = 1715
  integer, parameter :: k_he4_cr56__p_mn59   = 1716
  integer, parameter :: k_n_cr57__he4_ti54   = 1717
  integer, parameter :: k_p_cr57__n_mn57   = 1718
  integer, parameter :: k_p_cr57__he4_v54   = 1719
  integer, parameter :: k_he4_cr57__n_fe60   = 1720
  integer, parameter :: k_p_cr58__n_mn58   = 1721
  integer, parameter :: k_p_cr58__he4_v55   = 1722
  integer, parameter :: k_he4_cr58__n_fe61   = 1723
  integer, parameter :: k_n_mn51__p_cr51   = 1724
  integer, parameter :: k_n_mn51__he4_v48   = 1725
  integer, parameter :: k_p_mn51__n_fe51   = 1726
  integer, parameter :: k_p_mn51__he4_cr48   = 1727
  integer, parameter :: k_he4_mn51__p_fe54   = 1728
  integer, parameter :: k_n_mn52__p_cr52   = 1729
  integer, parameter :: k_n_mn52__he4_v49   = 1730
  integer, parameter :: k_p_mn52__n_fe52   = 1731
  integer, parameter :: k_p_mn52__he4_cr49   = 1732
  integer, parameter :: k_he4_mn52__n_co55   = 1733
  integer, parameter :: k_he4_mn52__p_fe55   = 1734
  integer, parameter :: k_n_mn53__p_cr53   = 1735
  integer, parameter :: k_n_mn53__he4_v50   = 1736
  integer, parameter :: k_p_mn53__n_fe53   = 1737
  integer, parameter :: k_p_mn53__he4_cr50   = 1738
  integer, parameter :: k_he4_mn53__n_co56   = 1739
  integer, parameter :: k_he4_mn53__p_fe56   = 1740
  integer, parameter :: k_n_mn54__p_cr54   = 1741
  integer, parameter :: k_n_mn54__he4_v51   = 1742
  integer, parameter :: k_p_mn54__n_fe54   = 1743
  integer, parameter :: k_p_mn54__he4_cr51   = 1744
  integer, parameter :: k_he4_mn54__n_co57   = 1745
  integer, parameter :: k_he4_mn54__p_fe57   = 1746
  integer, parameter :: k_n_mn55__p_cr55   = 1747
  integer, parameter :: k_n_mn55__he4_v52   = 1748
  integer, parameter :: k_p_mn55__n_fe55   = 1749
  integer, parameter :: k_p_mn55__he4_cr52   = 1750
  integer, parameter :: k_he4_mn55__n_co58   = 1751
  integer, parameter :: k_he4_mn55__p_fe58   = 1752
  integer, parameter :: k_n_mn56__p_cr56   = 1753
  integer, parameter :: k_n_mn56__he4_v53   = 1754
  integer, parameter :: k_p_mn56__n_fe56   = 1755
  integer, parameter :: k_p_mn56__he4_cr53   = 1756
  integer, parameter :: k_he4_mn56__n_co59   = 1757
  integer, parameter :: k_he4_mn56__p_fe59   = 1758
  integer, parameter :: k_n_mn57__p_cr57   = 1759
  integer, parameter :: k_n_mn57__he4_v54   = 1760
  integer, parameter :: k_p_mn57__n_fe57   = 1761
  integer, parameter :: k_p_mn57__he4_cr54   = 1762
  integer, parameter :: k_he4_mn57__n_co60   = 1763
  integer, parameter :: k_he4_mn57__p_fe60   = 1764
  integer, parameter :: k_n_mn58__p_cr58   = 1765
  integer, parameter :: k_n_mn58__he4_v55   = 1766
  integer, parameter :: k_p_mn58__n_fe58   = 1767
  integer, parameter :: k_p_mn58__he4_cr55   = 1768
  integer, parameter :: k_he4_mn58__n_co61   = 1769
  integer, parameter :: k_he4_mn58__p_fe61   = 1770
  integer, parameter :: k_n_mn59__he4_v56   = 1771
  integer, parameter :: k_p_mn59__n_fe59   = 1772
  integer, parameter :: k_p_mn59__he4_cr56   = 1773
  integer, parameter :: k_he4_mn59__n_co62   = 1774
  integer, parameter :: k_he4_mn59__p_fe62   = 1775
  integer, parameter :: k_n_fe51__p_mn51   = 1776
  integer, parameter :: k_n_fe51__he4_cr48   = 1777
  integer, parameter :: k_n_fe52__p_mn52   = 1778
  integer, parameter :: k_n_fe52__he4_cr49   = 1779
  integer, parameter :: k_he4_fe52__n_ni55   = 1780
  integer, parameter :: k_he4_fe52__p_co55   = 1781
  integer, parameter :: k_n_fe53__p_mn53   = 1782
  integer, parameter :: k_n_fe53__he4_cr50   = 1783
  integer, parameter :: k_he4_fe53__n_ni56   = 1784
  integer, parameter :: k_he4_fe53__p_co56   = 1785
  integer, parameter :: k_n_fe54__p_mn54   = 1786
  integer, parameter :: k_n_fe54__he4_cr51   = 1787
  integer, parameter :: k_p_fe54__he4_mn51   = 1788
  integer, parameter :: k_he4_fe54__n_ni57   = 1789
  integer, parameter :: k_he4_fe54__p_co57   = 1790
  integer, parameter :: k_n_fe55__p_mn55   = 1791
  integer, parameter :: k_n_fe55__he4_cr52   = 1792
  integer, parameter :: k_p_fe55__n_co55   = 1793
  integer, parameter :: k_p_fe55__he4_mn52   = 1794
  integer, parameter :: k_he4_fe55__n_ni58   = 1795
  integer, parameter :: k_he4_fe55__p_co58   = 1796
  integer, parameter :: k_n_fe56__p_mn56   = 1797
  integer, parameter :: k_n_fe56__he4_cr53   = 1798
  integer, parameter :: k_p_fe56__n_co56   = 1799
  integer, parameter :: k_p_fe56__he4_mn53   = 1800
  integer, parameter :: k_he4_fe56__n_ni59   = 1801
  integer, parameter :: k_he4_fe56__p_co59   = 1802
  integer, parameter :: k_n_fe57__p_mn57   = 1803
  integer, parameter :: k_n_fe57__he4_cr54   = 1804
  integer, parameter :: k_p_fe57__n_co57   = 1805
  integer, parameter :: k_p_fe57__he4_mn54   = 1806
  integer, parameter :: k_he4_fe57__n_ni60   = 1807
  integer, parameter :: k_he4_fe57__p_co60   = 1808
  integer, parameter :: k_n_fe58__p_mn58   = 1809
  integer, parameter :: k_n_fe58__he4_cr55   = 1810
  integer, parameter :: k_p_fe58__n_co58   = 1811
  integer, parameter :: k_p_fe58__he4_mn55   = 1812
  integer, parameter :: k_he4_fe58__n_ni61   = 1813
  integer, parameter :: k_he4_fe58__p_co61   = 1814
  integer, parameter :: k_n_fe59__p_mn59   = 1815
  integer, parameter :: k_n_fe59__he4_cr56   = 1816
  integer, parameter :: k_p_fe59__n_co59   = 1817
  integer, parameter :: k_p_fe59__he4_mn56   = 1818
  integer, parameter :: k_he4_fe59__n_ni62   = 1819
  integer, parameter :: k_he4_fe59__p_co62   = 1820
  integer, parameter :: k_n_fe60__he4_cr57   = 1821
  integer, parameter :: k_p_fe60__n_co60   = 1822
  integer, parameter :: k_p_fe60__he4_mn57   = 1823
  integer, parameter :: k_he4_fe60__n_ni63   = 1824
  integer, parameter :: k_he4_fe60__p_co63   = 1825
  integer, parameter :: k_n_fe61__he4_cr58   = 1826
  integer, parameter :: k_p_fe61__n_co61   = 1827
  integer, parameter :: k_p_fe61__he4_mn58   = 1828
  integer, parameter :: k_he4_fe61__n_ni64   = 1829
  integer, parameter :: k_he4_fe61__p_co64   = 1830
  integer, parameter :: k_p_fe62__n_co62   = 1831
  integer, parameter :: k_p_fe62__he4_mn59   = 1832
  integer, parameter :: k_he4_fe62__n_ni65   = 1833
  integer, parameter :: k_he4_fe62__p_co65   = 1834
  integer, parameter :: k_p_fe63__n_co63   = 1835
  integer, parameter :: k_he4_fe63__n_ni66   = 1836
  integer, parameter :: k_he4_fe63__p_co66   = 1837
  integer, parameter :: k_p_fe64__n_co64   = 1838
  integer, parameter :: k_he4_fe64__n_ni67   = 1839
  integer, parameter :: k_he4_fe64__p_co67   = 1840
  integer, parameter :: k_p_fe65__n_co65   = 1841
  integer, parameter :: k_he4_fe65__n_ni68   = 1842
  integer, parameter :: k_p_fe66__n_co66   = 1843
  integer, parameter :: k_n_co55__p_fe55   = 1844
  integer, parameter :: k_n_co55__he4_mn52   = 1845
  integer, parameter :: k_p_co55__n_ni55   = 1846
  integer, parameter :: k_p_co55__he4_fe52   = 1847
  integer, parameter :: k_he4_co55__p_ni58   = 1848
  integer, parameter :: k_n_co56__p_fe56   = 1849
  integer, parameter :: k_n_co56__he4_mn53   = 1850
  integer, parameter :: k_p_co56__n_ni56   = 1851
  integer, parameter :: k_p_co56__he4_fe53   = 1852
  integer, parameter :: k_he4_co56__n_cu59   = 1853
  integer, parameter :: k_he4_co56__p_ni59   = 1854
  integer, parameter :: k_n_co57__p_fe57   = 1855
  integer, parameter :: k_n_co57__he4_mn54   = 1856
  integer, parameter :: k_p_co57__n_ni57   = 1857
  integer, parameter :: k_p_co57__he4_fe54   = 1858
  integer, parameter :: k_he4_co57__n_cu60   = 1859
  integer, parameter :: k_he4_co57__p_ni60   = 1860
  integer, parameter :: k_n_co58__p_fe58   = 1861
  integer, parameter :: k_n_co58__he4_mn55   = 1862
  integer, parameter :: k_p_co58__n_ni58   = 1863
  integer, parameter :: k_p_co58__he4_fe55   = 1864
  integer, parameter :: k_he4_co58__n_cu61   = 1865
  integer, parameter :: k_he4_co58__p_ni61   = 1866
  integer, parameter :: k_n_co59__p_fe59   = 1867
  integer, parameter :: k_n_co59__he4_mn56   = 1868
  integer, parameter :: k_p_co59__n_ni59   = 1869
  integer, parameter :: k_p_co59__he4_fe56   = 1870
  integer, parameter :: k_he4_co59__n_cu62   = 1871
  integer, parameter :: k_he4_co59__p_ni62   = 1872
  integer, parameter :: k_n_co60__p_fe60   = 1873
  integer, parameter :: k_n_co60__he4_mn57   = 1874
  integer, parameter :: k_p_co60__n_ni60   = 1875
  integer, parameter :: k_p_co60__he4_fe57   = 1876
  integer, parameter :: k_he4_co60__n_cu63   = 1877
  integer, parameter :: k_he4_co60__p_ni63   = 1878
  integer, parameter :: k_n_co61__p_fe61   = 1879
  integer, parameter :: k_n_co61__he4_mn58   = 1880
  integer, parameter :: k_p_co61__n_ni61   = 1881
  integer, parameter :: k_p_co61__he4_fe58   = 1882
  integer, parameter :: k_he4_co61__n_cu64   = 1883
  integer, parameter :: k_he4_co61__p_ni64   = 1884
  integer, parameter :: k_n_co62__p_fe62   = 1885
  integer, parameter :: k_n_co62__he4_mn59   = 1886
  integer, parameter :: k_p_co62__n_ni62   = 1887
  integer, parameter :: k_p_co62__he4_fe59   = 1888
  integer, parameter :: k_he4_co62__n_cu65   = 1889
  integer, parameter :: k_he4_co62__p_ni65   = 1890
  integer, parameter :: k_n_co63__p_fe63   = 1891
  integer, parameter :: k_p_co63__n_ni63   = 1892
  integer, parameter :: k_p_co63__he4_fe60   = 1893
  integer, parameter :: k_he4_co63__n_cu66   = 1894
  integer, parameter :: k_he4_co63__p_ni66   = 1895
  integer, parameter :: k_n_co64__p_fe64   = 1896
  integer, parameter :: k_p_co64__n_ni64   = 1897
  integer, parameter :: k_p_co64__he4_fe61   = 1898
  integer, parameter :: k_he4_co64__p_ni67   = 1899
  integer, parameter :: k_n_co65__p_fe65   = 1900
  integer, parameter :: k_p_co65__n_ni65   = 1901
  integer, parameter :: k_p_co65__he4_fe62   = 1902
  integer, parameter :: k_he4_co65__p_ni68   = 1903
  integer, parameter :: k_n_co66__p_fe66   = 1904
  integer, parameter :: k_p_co66__n_ni66   = 1905
  integer, parameter :: k_p_co66__he4_fe63   = 1906
  integer, parameter :: k_p_co67__n_ni67   = 1907
  integer, parameter :: k_p_co67__he4_fe64   = 1908
  integer, parameter :: k_n_ni55__p_co55   = 1909
  integer, parameter :: k_n_ni55__he4_fe52   = 1910
  integer, parameter :: k_n_ni56__p_co56   = 1911
  integer, parameter :: k_n_ni56__he4_fe53   = 1912
  integer, parameter :: k_he4_ni56__n_zn59   = 1913
  integer, parameter :: k_he4_ni56__p_cu59   = 1914
  integer, parameter :: k_n_ni57__p_co57   = 1915
  integer, parameter :: k_n_ni57__he4_fe54   = 1916
  integer, parameter :: k_he4_ni57__n_zn60   = 1917
  integer, parameter :: k_he4_ni57__p_cu60   = 1918
  integer, parameter :: k_n_ni58__p_co58   = 1919
  integer, parameter :: k_n_ni58__he4_fe55   = 1920
  integer, parameter :: k_p_ni58__he4_co55   = 1921
  integer, parameter :: k_he4_ni58__n_zn61   = 1922
  integer, parameter :: k_he4_ni58__p_cu61   = 1923
  integer, parameter :: k_n_ni59__p_co59   = 1924
  integer, parameter :: k_n_ni59__he4_fe56   = 1925
  integer, parameter :: k_p_ni59__n_cu59   = 1926
  integer, parameter :: k_p_ni59__he4_co56   = 1927
  integer, parameter :: k_he4_ni59__n_zn62   = 1928
  integer, parameter :: k_he4_ni59__p_cu62   = 1929
  integer, parameter :: k_n_ni60__p_co60   = 1930
  integer, parameter :: k_n_ni60__he4_fe57   = 1931
  integer, parameter :: k_p_ni60__n_cu60   = 1932
  integer, parameter :: k_p_ni60__he4_co57   = 1933
  integer, parameter :: k_he4_ni60__n_zn63   = 1934
  integer, parameter :: k_he4_ni60__p_cu63   = 1935
  integer, parameter :: k_n_ni61__p_co61   = 1936
  integer, parameter :: k_n_ni61__he4_fe58   = 1937
  integer, parameter :: k_p_ni61__n_cu61   = 1938
  integer, parameter :: k_p_ni61__he4_co58   = 1939
  integer, parameter :: k_he4_ni61__n_zn64   = 1940
  integer, parameter :: k_he4_ni61__p_cu64   = 1941
  integer, parameter :: k_n_ni62__p_co62   = 1942
  integer, parameter :: k_n_ni62__he4_fe59   = 1943
  integer, parameter :: k_p_ni62__n_cu62   = 1944
  integer, parameter :: k_p_ni62__he4_co59   = 1945
  integer, parameter :: k_he4_ni62__n_zn65   = 1946
  integer, parameter :: k_he4_ni62__p_cu65   = 1947
  integer, parameter :: k_n_ni63__p_co63   = 1948
  integer, parameter :: k_n_ni63__he4_fe60   = 1949
  integer, parameter :: k_p_ni63__n_cu63   = 1950
  integer, parameter :: k_p_ni63__he4_co60   = 1951
  integer, parameter :: k_he4_ni63__n_zn66   = 1952
  integer, parameter :: k_he4_ni63__p_cu66   = 1953
  integer, parameter :: k_n_ni64__p_co64   = 1954
  integer, parameter :: k_n_ni64__he4_fe61   = 1955
  integer, parameter :: k_p_ni64__n_cu64   = 1956
  integer, parameter :: k_p_ni64__he4_co61   = 1957
  integer, parameter :: k_n_ni65__p_co65   = 1958
  integer, parameter :: k_n_ni65__he4_fe62   = 1959
  integer, parameter :: k_p_ni65__n_cu65   = 1960
  integer, parameter :: k_p_ni65__he4_co62   = 1961
  integer, parameter :: k_n_ni66__p_co66   = 1962
  integer, parameter :: k_n_ni66__he4_fe63   = 1963
  integer, parameter :: k_p_ni66__n_cu66   = 1964
  integer, parameter :: k_p_ni66__he4_co63   = 1965
  integer, parameter :: k_n_ni67__p_co67   = 1966
  integer, parameter :: k_n_ni67__he4_fe64   = 1967
  integer, parameter :: k_p_ni67__he4_co64   = 1968
  integer, parameter :: k_n_ni68__he4_fe65   = 1969
  integer, parameter :: k_p_ni68__he4_co65   = 1970
  integer, parameter :: k_n_cu59__p_ni59   = 1971
  integer, parameter :: k_n_cu59__he4_co56   = 1972
  integer, parameter :: k_p_cu59__n_zn59   = 1973
  integer, parameter :: k_p_cu59__he4_ni56   = 1974
  integer, parameter :: k_he4_cu59__p_zn62   = 1975
  integer, parameter :: k_n_cu60__p_ni60   = 1976
  integer, parameter :: k_n_cu60__he4_co57   = 1977
  integer, parameter :: k_p_cu60__n_zn60   = 1978
  integer, parameter :: k_p_cu60__he4_ni57   = 1979
  integer, parameter :: k_he4_cu60__p_zn63   = 1980
  integer, parameter :: k_n_cu61__p_ni61   = 1981
  integer, parameter :: k_n_cu61__he4_co58   = 1982
  integer, parameter :: k_p_cu61__n_zn61   = 1983
  integer, parameter :: k_p_cu61__he4_ni58   = 1984
  integer, parameter :: k_he4_cu61__p_zn64   = 1985
  integer, parameter :: k_n_cu62__p_ni62   = 1986
  integer, parameter :: k_n_cu62__he4_co59   = 1987
  integer, parameter :: k_p_cu62__n_zn62   = 1988
  integer, parameter :: k_p_cu62__he4_ni59   = 1989
  integer, parameter :: k_he4_cu62__p_zn65   = 1990
  integer, parameter :: k_n_cu63__p_ni63   = 1991
  integer, parameter :: k_n_cu63__he4_co60   = 1992
  integer, parameter :: k_p_cu63__n_zn63   = 1993
  integer, parameter :: k_p_cu63__he4_ni60   = 1994
  integer, parameter :: k_he4_cu63__p_zn66   = 1995
  integer, parameter :: k_n_cu64__p_ni64   = 1996
  integer, parameter :: k_n_cu64__he4_co61   = 1997
  integer, parameter :: k_p_cu64__n_zn64   = 1998
  integer, parameter :: k_p_cu64__he4_ni61   = 1999
  integer, parameter :: k_n_cu65__p_ni65   = 2000
  integer, parameter :: k_n_cu65__he4_co62   = 2001
  integer, parameter :: k_p_cu65__n_zn65   = 2002
  integer, parameter :: k_p_cu65__he4_ni62   = 2003
  integer, parameter :: k_n_cu66__p_ni66   = 2004
  integer, parameter :: k_n_cu66__he4_co63   = 2005
  integer, parameter :: k_p_cu66__n_zn66   = 2006
  integer, parameter :: k_p_cu66__he4_ni63   = 2007
  integer, parameter :: k_n_zn59__p_cu59   = 2008
  integer, parameter :: k_n_zn59__he4_ni56   = 2009
  integer, parameter :: k_n_zn60__p_cu60   = 2010
  integer, parameter :: k_n_zn60__he4_ni57   = 2011
  integer, parameter :: k_n_zn61__p_cu61   = 2012
  integer, parameter :: k_n_zn61__he4_ni58   = 2013
  integer, parameter :: k_n_zn62__p_cu62   = 2014
  integer, parameter :: k_n_zn62__he4_ni59   = 2015
  integer, parameter :: k_p_zn62__he4_cu59   = 2016
  integer, parameter :: k_n_zn63__p_cu63   = 2017
  integer, parameter :: k_n_zn63__he4_ni60   = 2018
  integer, parameter :: k_p_zn63__he4_cu60   = 2019
  integer, parameter :: k_n_zn64__p_cu64   = 2020
  integer, parameter :: k_n_zn64__he4_ni61   = 2021
  integer, parameter :: k_p_zn64__he4_cu61   = 2022
  integer, parameter :: k_n_zn65__p_cu65   = 2023
  integer, parameter :: k_n_zn65__he4_ni62   = 2024
  integer, parameter :: k_p_zn65__he4_cu62   = 2025
  integer, parameter :: k_n_zn66__p_cu66   = 2026
  integer, parameter :: k_n_zn66__he4_ni63   = 2027
  integer, parameter :: k_p_zn66__he4_cu63   = 2028
  integer, parameter :: k_p_d__n_p_p   = 2029
  integer, parameter :: k_he3_he3__p_p_he4   = 2030
  integer, parameter :: k_d_li7__n_he4_he4   = 2031
  integer, parameter :: k_d_be7__p_he4_he4   = 2032
  integer, parameter :: k_p_be9__d_he4_he4   = 2033
  integer, parameter :: k_n_b8__p_he4_he4   = 2034
  integer, parameter :: k_p_b11__he4_he4_he4   = 2035
  integer, parameter :: k_he3_li7__n_p_he4_he4   = 2036
  integer, parameter :: k_he3_be7__p_p_he4_he4   = 2037
  integer, parameter :: k_p_be9__n_p_he4_he4   = 2038
  integer, parameter :: k_n_p_he4__li6   = 2039
  integer, parameter :: k_n_he4_he4__be9   = 2040
  integer, parameter :: k_he4_he4_he4__c12   = 2041
  integer, parameter :: k_n_p_p__p   = 2042
  integer, parameter :: k_p_p_he4__he3   = 2043
  integer, parameter :: k_n_he4_he4__p   = 2044
  integer, parameter :: k_n_he4_he4__d   = 2045
  integer, parameter :: k_p_he4_he4__n   = 2046
  integer, parameter :: k_p_he4_he4__d   = 2047
  integer, parameter :: k_d_he4_he4__p   = 2048
  integer, parameter :: k_he4_he4_he4__n   = 2049
  integer, parameter :: k_he4_he4_he4__p   = 2050
  integer, parameter :: k_p_p_o15__he4   = 2051
  integer, parameter :: k_f20__o20   = 2052
  integer, parameter :: k_ne20__f20   = 2053
  integer, parameter :: k_o20__f20   = 2054
  integer, parameter :: k_f20__ne20   = 2055

  real(rt), allocatable, save :: bion(:), mion(:)

#ifdef AMREX_USE_CUDA
  attributes(managed) :: bion, mion
#endif

  !$acc declare create(bion, mion)

#ifdef REACT_SPARSE_JACOBIAN
  ! Shape of Jacobian in Compressed Sparse Row format
  integer, parameter   :: NETWORK_SPARSE_JAC_NNZ = 3923
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
    ebind_per_nucleon(jhe3)   = 2.57268000000000e+00_rt
    ebind_per_nucleon(jhe4)   = 7.07391500000000e+00_rt
    ebind_per_nucleon(jli6)   = 5.33233100000000e+00_rt
    ebind_per_nucleon(jli7)   = 5.60643900000000e+00_rt
    ebind_per_nucleon(jbe7)   = 5.37154800000000e+00_rt
    ebind_per_nucleon(jbe9)   = 6.46266800000000e+00_rt
    ebind_per_nucleon(jbe10)   = 6.49763000000000e+00_rt
    ebind_per_nucleon(jb8)   = 4.71715500000000e+00_rt
    ebind_per_nucleon(jb10)   = 6.47508300000000e+00_rt
    ebind_per_nucleon(jb11)   = 6.92773200000000e+00_rt
    ebind_per_nucleon(jc12)   = 7.68014400000000e+00_rt
    ebind_per_nucleon(jc13)   = 7.46984900000000e+00_rt
    ebind_per_nucleon(jn13)   = 7.23886300000000e+00_rt
    ebind_per_nucleon(jn14)   = 7.47561400000000e+00_rt
    ebind_per_nucleon(jn15)   = 7.69946000000000e+00_rt
    ebind_per_nucleon(jn16)   = 7.37379600000000e+00_rt
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
    ebind_per_nucleon(jne19)   = 7.56734300000000e+00_rt
    ebind_per_nucleon(jne20)   = 8.03224000000000e+00_rt
    ebind_per_nucleon(jne21)   = 7.97171300000000e+00_rt
    ebind_per_nucleon(jne22)   = 8.08046500000000e+00_rt
    ebind_per_nucleon(jne23)   = 7.95525600000000e+00_rt
    ebind_per_nucleon(jna21)   = 7.76554700000000e+00_rt
    ebind_per_nucleon(jna22)   = 7.91566700000000e+00_rt
    ebind_per_nucleon(jna23)   = 8.11149300000000e+00_rt
    ebind_per_nucleon(jna24)   = 8.06348800000000e+00_rt
    ebind_per_nucleon(jmg23)   = 7.90111500000000e+00_rt
    ebind_per_nucleon(jmg24)   = 8.26070900000000e+00_rt
    ebind_per_nucleon(jmg25)   = 8.22350200000000e+00_rt
    ebind_per_nucleon(jmg26)   = 8.33387000000000e+00_rt
    ebind_per_nucleon(jmg27)   = 8.26385200000000e+00_rt
    ebind_per_nucleon(jal25)   = 8.02113600000000e+00_rt
    ebind_per_nucleon(jal26)   = 8.14976500000000e+00_rt
    ebind_per_nucleon(jal27)   = 8.33155300000000e+00_rt
    ebind_per_nucleon(jal28)   = 8.30989400000000e+00_rt
    ebind_per_nucleon(jsi27)   = 8.12434100000000e+00_rt
    ebind_per_nucleon(jsi28)   = 8.44774400000000e+00_rt
    ebind_per_nucleon(jsi29)   = 8.44863500000000e+00_rt
    ebind_per_nucleon(jsi30)   = 8.52065400000000e+00_rt
    ebind_per_nucleon(jsi31)   = 8.45829100000000e+00_rt
    ebind_per_nucleon(jsi32)   = 8.48146800000000e+00_rt
    ebind_per_nucleon(jsi33)   = 8.36105900000000e+00_rt
    ebind_per_nucleon(jp30)   = 8.35350600000000e+00_rt
    ebind_per_nucleon(jp31)   = 8.48116700000000e+00_rt
    ebind_per_nucleon(jp32)   = 8.46412000000000e+00_rt
    ebind_per_nucleon(jp33)   = 8.51380600000000e+00_rt
    ebind_per_nucleon(jp34)   = 8.44818500000000e+00_rt
    ebind_per_nucleon(js31)   = 8.28180000000000e+00_rt
    ebind_per_nucleon(js32)   = 8.49312900000000e+00_rt
    ebind_per_nucleon(js33)   = 8.49763000000000e+00_rt
    ebind_per_nucleon(js34)   = 8.58349800000000e+00_rt
    ebind_per_nucleon(js35)   = 8.53785000000000e+00_rt
    ebind_per_nucleon(js36)   = 8.57538900000000e+00_rt
    ebind_per_nucleon(js37)   = 8.45993500000000e+00_rt
    ebind_per_nucleon(jcl35)   = 8.52027800000000e+00_rt
    ebind_per_nucleon(jcl36)   = 8.52193100000000e+00_rt
    ebind_per_nucleon(jcl37)   = 8.57028100000000e+00_rt
    ebind_per_nucleon(jcl38)   = 8.50548100000000e+00_rt
    ebind_per_nucleon(jar35)   = 8.32746100000000e+00_rt
    ebind_per_nucleon(jar36)   = 8.51990900000000e+00_rt
    ebind_per_nucleon(jar37)   = 8.52713900000000e+00_rt
    ebind_per_nucleon(jar38)   = 8.61428000000000e+00_rt
    ebind_per_nucleon(jar39)   = 8.56259800000000e+00_rt
    ebind_per_nucleon(jar40)   = 8.59525900000000e+00_rt
    ebind_per_nucleon(jar41)   = 8.53437200000000e+00_rt
    ebind_per_nucleon(jk39)   = 8.55702500000000e+00_rt
    ebind_per_nucleon(jk40)   = 8.53809000000000e+00_rt
    ebind_per_nucleon(jk41)   = 8.57607200000000e+00_rt
    ebind_per_nucleon(jk42)   = 8.55125600000000e+00_rt
    ebind_per_nucleon(jk43)   = 8.57622000000000e+00_rt
    ebind_per_nucleon(jk44)   = 8.54670100000000e+00_rt
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
    ebind_per_nucleon(jsc43)   = 8.53082500000000e+00_rt
    ebind_per_nucleon(jsc44)   = 8.55737900000000e+00_rt
    ebind_per_nucleon(jsc45)   = 8.61893100000000e+00_rt
    ebind_per_nucleon(jsc46)   = 8.62201200000000e+00_rt
    ebind_per_nucleon(jsc47)   = 8.66509000000000e+00_rt
    ebind_per_nucleon(jsc48)   = 8.65620400000000e+00_rt
    ebind_per_nucleon(jsc49)   = 8.68625600000000e+00_rt
    ebind_per_nucleon(jsc50)   = 8.63367900000000e+00_rt
    ebind_per_nucleon(jsc51)   = 8.59679600000000e+00_rt
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
    ebind_per_nucleon(jti54)   = 8.59743500000000e+00_rt
    ebind_per_nucleon(jv47)   = 8.58222500000000e+00_rt
    ebind_per_nucleon(jv48)   = 8.62306100000000e+00_rt
    ebind_per_nucleon(jv49)   = 8.68290800000000e+00_rt
    ebind_per_nucleon(jv50)   = 8.69591800000000e+00_rt
    ebind_per_nucleon(jv51)   = 8.74209900000000e+00_rt
    ebind_per_nucleon(jv52)   = 8.71458200000000e+00_rt
    ebind_per_nucleon(jv53)   = 8.71013000000000e+00_rt
    ebind_per_nucleon(jv54)   = 8.66204300000000e+00_rt
    ebind_per_nucleon(jv55)   = 8.63769200000000e+00_rt
    ebind_per_nucleon(jv56)   = 8.57419100000000e+00_rt
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
    ebind_per_nucleon(jmn51)   = 8.63377200000000e+00_rt
    ebind_per_nucleon(jmn52)   = 8.67032900000000e+00_rt
    ebind_per_nucleon(jmn53)   = 8.73417500000000e+00_rt
    ebind_per_nucleon(jmn54)   = 8.73796500000000e+00_rt
    ebind_per_nucleon(jmn55)   = 8.76502200000000e+00_rt
    ebind_per_nucleon(jmn56)   = 8.73833300000000e+00_rt
    ebind_per_nucleon(jmn57)   = 8.73671300000000e+00_rt
    ebind_per_nucleon(jmn58)   = 8.69664300000000e+00_rt
    ebind_per_nucleon(jmn59)   = 8.68092100000000e+00_rt
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
    ebind_per_nucleon(jcu59)   = 8.64200000000000e+00_rt
    ebind_per_nucleon(jcu60)   = 8.66560200000000e+00_rt
    ebind_per_nucleon(jcu61)   = 8.71551400000000e+00_rt
    ebind_per_nucleon(jcu62)   = 8.71808100000000e+00_rt
    ebind_per_nucleon(jcu63)   = 8.75213800000000e+00_rt
    ebind_per_nucleon(jcu64)   = 8.73907500000000e+00_rt
    ebind_per_nucleon(jcu65)   = 8.75709600000000e+00_rt
    ebind_per_nucleon(jcu66)   = 8.73147200000000e+00_rt
    ebind_per_nucleon(jzn59)   = 8.47377700000000e+00_rt
    ebind_per_nucleon(jzn60)   = 8.58305000000000e+00_rt
    ebind_per_nucleon(jzn61)   = 8.61030900000000e+00_rt
    ebind_per_nucleon(jzn62)   = 8.67934300000000e+00_rt
    ebind_per_nucleon(jzn63)   = 8.68628500000000e+00_rt
    ebind_per_nucleon(jzn64)   = 8.73590500000000e+00_rt
    ebind_per_nucleon(jzn65)   = 8.72426500000000e+00_rt
    ebind_per_nucleon(jzn66)   = 8.75963200000000e+00_rt

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
      18, &
      19, &
      20, &
      21, &
      22, &
      23, &
      24, &
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
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      15, &
      17, &
      206, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      206, &
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
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      9, &
      12, &
      206, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      10, &
      12, &
      13, &
      206, &
      1, &
      2, &
      3, &
      4, &
      5, &
      6, &
      7, &
      8, &
      11, &
      12, &
      206, &
      1, &
      2, &
      5, &
      6, &
      9, &
      10, &
      12, &
      14, &
      206, &
      1, &
      2, &
      5, &
      7, &
      9, &
      10, &
      13, &
      15, &
      206, &
      1, &
      2, &
      8, &
      11, &
      206, &
      1, &
      2, &
      5, &
      6, &
      7, &
      8, &
      9, &
      10, &
      12, &
      13, &
      15, &
      16, &
      206, &
      1, &
      2, &
      5, &
      7, &
      10, &
      12, &
      13, &
      14, &
      17, &
      206, &
      1, &
      2, &
      5, &
      9, &
      13, &
      14, &
      15, &
      16, &
      18, &
      19, &
      20, &
      21, &
      31, &
      37, &
      39, &
      40, &
      46, &
      48, &
      49, &
      56, &
      60, &
      206, &
      1, &
      2, &
      3, &
      5, &
      10, &
      12, &
      14, &
      15, &
      16, &
      17, &
      19, &
      21, &
      206, &
      1, &
      2, &
      5, &
      12, &
      14, &
      15, &
      16, &
      17, &
      21, &
      206, &
      1, &
      2, &
      3, &
      5, &
      13, &
      15, &
      16, &
      17, &
      18, &
      20, &
      22, &
      26, &
      27, &
      206, &
      1, &
      2, &
      5, &
      14, &
      17, &
      18, &
      19, &
      20, &
      21, &
      23, &
      27, &
      28, &
      206, &
      1, &
      2, &
      5, &
      15, &
      18, &
      19, &
      21, &
      24, &
      28, &
      206, &
      1, &
      2, &
      5, &
      14, &
      17, &
      18, &
      20, &
      21, &
      27, &
      30, &
      206, &
      1, &
      2, &
      5, &
      14, &
      15, &
      16, &
      18, &
      19, &
      20, &
      21, &
      22, &
      26, &
      28, &
      30, &
      31, &
      40, &
      46, &
      48, &
      49, &
      56, &
      60, &
      206, &
      1, &
      2, &
      5, &
      17, &
      21, &
      22, &
      23, &
      26, &
      27, &
      29, &
      31, &
      32, &
      206, &
      1, &
      2, &
      5, &
      18, &
      22, &
      23, &
      24, &
      27, &
      28, &
      32, &
      33, &
      206, &
      1, &
      2, &
      5, &
      19, &
      23, &
      24, &
      28, &
      29, &
      33, &
      34, &
      206, &
      25, &
      29, &
      206, &
      1, &
      2, &
      5, &
      17, &
      21, &
      22, &
      26, &
      27, &
      31, &
      35, &
      206, &
      1, &
      2, &
      5, &
      17, &
      18, &
      20, &
      22, &
      23, &
      26, &
      27, &
      28, &
      30, &
      32, &
      35, &
      36, &
      206, &
      1, &
      2, &
      5, &
      18, &
      19, &
      21, &
      23, &
      24, &
      27, &
      28, &
      29, &
      30, &
      31, &
      33, &
      36, &
      37, &
      206, &
      1, &
      2, &
      5, &
      22, &
      24, &
      25, &
      28, &
      29, &
      31, &
      32, &
      34, &
      37, &
      38, &
      206, &
      1, &
      2, &
      5, &
      20, &
      21, &
      27, &
      28, &
      30, &
      31, &
      36, &
      39, &
      206, &
      1, &
      2, &
      5, &
      14, &
      21, &
      22, &
      26, &
      28, &
      29, &
      30, &
      31, &
      32, &
      35, &
      37, &
      39, &
      40, &
      49, &
      56, &
      60, &
      206, &
      1, &
      2, &
      5, &
      22, &
      23, &
      27, &
      29, &
      31, &
      32, &
      33, &
      35, &
      36, &
      38, &
      40, &
      41, &
      206, &
      1, &
      2, &
      5, &
      23, &
      24, &
      28, &
      32, &
      33, &
      34, &
      36, &
      37, &
      41, &
      42, &
      206, &
      1, &
      2, &
      5, &
      24, &
      29, &
      33, &
      34, &
      37, &
      38, &
      42, &
      43, &
      206, &
      1, &
      2, &
      5, &
      26, &
      27, &
      31, &
      32, &
      35, &
      36, &
      40, &
      44, &
      206, &
      1, &
      2, &
      5, &
      27, &
      28, &
      30, &
      32, &
      33, &
      35, &
      36, &
      37, &
      39, &
      41, &
      44, &
      45, &
      206, &
      1, &
      2, &
      5, &
      14, &
      28, &
      29, &
      31, &
      33, &
      34, &
      36, &
      37, &
      38, &
      39, &
      40, &
      42, &
      45, &
      46, &
      206, &
      1, &
      2, &
      5, &
      29, &
      32, &
      34, &
      37, &
      38, &
      40, &
      41, &
      43, &
      46, &
      47, &
      206, &
      1, &
      2, &
      5, &
      14, &
      30, &
      31, &
      36, &
      37, &
      39, &
      40, &
      45, &
      48, &
      206, &
      1, &
      2, &
      5, &
      14, &
      21, &
      31, &
      32, &
      35, &
      37, &
      38, &
      39, &
      40, &
      41, &
      44, &
      46, &
      48, &
      49, &
      206, &
      1, &
      2, &
      5, &
      32, &
      33, &
      36, &
      38, &
      40, &
      41, &
      42, &
      44, &
      45, &
      47, &
      49, &
      50, &
      206, &
      1, &
      2, &
      5, &
      33, &
      34, &
      37, &
      41, &
      42, &
      43, &
      45, &
      46, &
      50, &
      51, &
      206, &
      1, &
      2, &
      5, &
      34, &
      38, &
      42, &
      43, &
      46, &
      47, &
      51, &
      52, &
      206, &
      1, &
      2, &
      5, &
      35, &
      36, &
      40, &
      41, &
      44, &
      45, &
      49, &
      206, &
      1, &
      2, &
      5, &
      36, &
      37, &
      39, &
      41, &
      42, &
      44, &
      45, &
      46, &
      48, &
      50, &
      55, &
      206, &
      1, &
      2, &
      5, &
      14, &
      21, &
      37, &
      38, &
      40, &
      42, &
      43, &
      45, &
      46, &
      47, &
      48, &
      49, &
      51, &
      55, &
      56, &
      206, &
      1, &
      2, &
      5, &
      38, &
      41, &
      43, &
      46, &
      47, &
      49, &
      50, &
      52, &
      56, &
      57, &
      206, &
      1, &
      2, &
      5, &
      14, &
      21, &
      39, &
      40, &
      45, &
      46, &
      48, &
      49, &
      55, &
      60, &
      206, &
      1, &
      2, &
      5, &
      14, &
      21, &
      31, &
      40, &
      41, &
      44, &
      46, &
      47, &
      48, &
      49, &
      50, &
      56, &
      60, &
      61, &
      206, &
      1, &
      2, &
      5, &
      41, &
      42, &
      45, &
      47, &
      49, &
      50, &
      51, &
      55, &
      57, &
      61, &
      62, &
      206, &
      1, &
      2, &
      5, &
      42, &
      43, &
      46, &
      50, &
      51, &
      52, &
      55, &
      56, &
      58, &
      62, &
      63, &
      206, &
      1, &
      2, &
      5, &
      43, &
      47, &
      51, &
      52, &
      53, &
      56, &
      57, &
      59, &
      63, &
      64, &
      206, &
      1, &
      2, &
      5, &
      52, &
      53, &
      54, &
      57, &
      58, &
      64, &
      65, &
      206, &
      1, &
      2, &
      5, &
      53, &
      54, &
      58, &
      59, &
      65, &
      66, &
      206, &
      1, &
      2, &
      5, &
      45, &
      46, &
      48, &
      50, &
      51, &
      55, &
      56, &
      60, &
      62, &
      206, &
      1, &
      2, &
      5, &
      14, &
      21, &
      31, &
      46, &
      47, &
      49, &
      51, &
      52, &
      55, &
      56, &
      57, &
      60, &
      61, &
      63, &
      67, &
      206, &
      1, &
      2, &
      5, &
      47, &
      50, &
      52, &
      53, &
      56, &
      57, &
      58, &
      61, &
      62, &
      64, &
      67, &
      68, &
      206, &
      1, &
      2, &
      5, &
      51, &
      53, &
      54, &
      57, &
      58, &
      59, &
      62, &
      63, &
      65, &
      68, &
      69, &
      206, &
      1, &
      2, &
      5, &
      52, &
      54, &
      58, &
      59, &
      63, &
      64, &
      66, &
      69, &
      70, &
      206, &
      1, &
      2, &
      5, &
      14, &
      21, &
      31, &
      48, &
      49, &
      55, &
      56, &
      60, &
      61, &
      71, &
      206, &
      1, &
      2, &
      5, &
      49, &
      50, &
      56, &
      57, &
      60, &
      61, &
      62, &
      67, &
      71, &
      72, &
      206, &
      1, &
      2, &
      5, &
      50, &
      51, &
      55, &
      57, &
      58, &
      61, &
      62, &
      63, &
      68, &
      72, &
      73, &
      206, &
      1, &
      2, &
      5, &
      51, &
      52, &
      56, &
      58, &
      59, &
      62, &
      63, &
      64, &
      67, &
      69, &
      73, &
      74, &
      206, &
      1, &
      2, &
      5, &
      52, &
      53, &
      57, &
      59, &
      63, &
      64, &
      65, &
      67, &
      68, &
      70, &
      74, &
      75, &
      206, &
      1, &
      2, &
      5, &
      53, &
      54, &
      58, &
      64, &
      65, &
      66, &
      68, &
      69, &
      75, &
      76, &
      206, &
      1, &
      2, &
      5, &
      54, &
      59, &
      65, &
      66, &
      69, &
      70, &
      76, &
      77, &
      206, &
      1, &
      2, &
      5, &
      56, &
      57, &
      61, &
      63, &
      64, &
      67, &
      68, &
      71, &
      72, &
      74, &
      78, &
      206, &
      1, &
      2, &
      5, &
      57, &
      58, &
      62, &
      64, &
      65, &
      67, &
      68, &
      69, &
      72, &
      73, &
      75, &
      78, &
      79, &
      206, &
      1, &
      2, &
      5, &
      58, &
      59, &
      63, &
      65, &
      66, &
      68, &
      69, &
      70, &
      73, &
      74, &
      76, &
      79, &
      80, &
      206, &
      1, &
      2, &
      5, &
      59, &
      64, &
      66, &
      69, &
      70, &
      74, &
      75, &
      77, &
      80, &
      81, &
      206, &
      1, &
      2, &
      5, &
      60, &
      61, &
      67, &
      71, &
      72, &
      84, &
      206, &
      1, &
      2, &
      5, &
      61, &
      62, &
      67, &
      68, &
      71, &
      72, &
      73, &
      78, &
      84, &
      85, &
      206, &
      1, &
      2, &
      5, &
      62, &
      63, &
      68, &
      69, &
      72, &
      73, &
      74, &
      79, &
      85, &
      86, &
      206, &
      1, &
      2, &
      5, &
      63, &
      64, &
      67, &
      69, &
      70, &
      73, &
      74, &
      75, &
      78, &
      80, &
      86, &
      87, &
      206, &
      1, &
      2, &
      5, &
      64, &
      65, &
      68, &
      70, &
      74, &
      75, &
      76, &
      78, &
      79, &
      81, &
      87, &
      88, &
      206, &
      1, &
      2, &
      5, &
      65, &
      66, &
      69, &
      75, &
      76, &
      77, &
      79, &
      80, &
      82, &
      88, &
      89, &
      206, &
      1, &
      2, &
      5, &
      66, &
      70, &
      76, &
      77, &
      80, &
      81, &
      83, &
      89, &
      90, &
      206, &
      1, &
      2, &
      5, &
      67, &
      68, &
      72, &
      74, &
      75, &
      78, &
      79, &
      84, &
      85, &
      87, &
      95, &
      206, &
      1, &
      2, &
      5, &
      68, &
      69, &
      73, &
      75, &
      76, &
      78, &
      79, &
      80, &
      85, &
      86, &
      88, &
      95, &
      96, &
      206, &
      1, &
      2, &
      5, &
      69, &
      70, &
      74, &
      76, &
      77, &
      79, &
      80, &
      81, &
      86, &
      87, &
      89, &
      96, &
      97, &
      206, &
      1, &
      2, &
      5, &
      70, &
      75, &
      77, &
      80, &
      81, &
      82, &
      87, &
      88, &
      90, &
      97, &
      98, &
      206, &
      1, &
      2, &
      5, &
      76, &
      81, &
      82, &
      83, &
      88, &
      89, &
      91, &
      98, &
      99, &
      206, &
      1, &
      2, &
      5, &
      77, &
      82, &
      83, &
      89, &
      90, &
      92, &
      99, &
      100, &
      206, &
      1, &
      2, &
      5, &
      71, &
      72, &
      78, &
      84, &
      85, &
      104, &
      206, &
      1, &
      2, &
      5, &
      72, &
      73, &
      78, &
      79, &
      84, &
      85, &
      86, &
      95, &
      104, &
      105, &
      206, &
      1, &
      2, &
      5, &
      73, &
      74, &
      79, &
      80, &
      85, &
      86, &
      87, &
      96, &
      105, &
      106, &
      206, &
      1, &
      2, &
      5, &
      74, &
      75, &
      78, &
      80, &
      81, &
      86, &
      87, &
      88, &
      95, &
      97, &
      106, &
      107, &
      206, &
      1, &
      2, &
      5, &
      75, &
      76, &
      79, &
      81, &
      82, &
      87, &
      88, &
      89, &
      95, &
      96, &
      98, &
      107, &
      108, &
      206, &
      1, &
      2, &
      5, &
      76, &
      77, &
      80, &
      82, &
      83, &
      88, &
      89, &
      90, &
      96, &
      97, &
      99, &
      108, &
      109, &
      206, &
      1, &
      2, &
      5, &
      77, &
      81, &
      83, &
      89, &
      90, &
      91, &
      97, &
      98, &
      100, &
      109, &
      110, &
      206, &
      1, &
      2, &
      5, &
      82, &
      90, &
      91, &
      92, &
      98, &
      99, &
      101, &
      110, &
      111, &
      206, &
      1, &
      2, &
      5, &
      83, &
      91, &
      92, &
      93, &
      99, &
      100, &
      102, &
      111, &
      112, &
      206, &
      1, &
      2, &
      5, &
      92, &
      93, &
      94, &
      100, &
      101, &
      103, &
      112, &
      113, &
      206, &
      1, &
      2, &
      5, &
      93, &
      94, &
      101, &
      102, &
      113, &
      114, &
      206, &
      1, &
      2, &
      5, &
      78, &
      79, &
      85, &
      87, &
      88, &
      95, &
      96, &
      104, &
      105, &
      107, &
      116, &
      206, &
      1, &
      2, &
      5, &
      79, &
      80, &
      86, &
      88, &
      89, &
      95, &
      96, &
      97, &
      105, &
      106, &
      108, &
      116, &
      117, &
      206, &
      1, &
      2, &
      5, &
      80, &
      81, &
      87, &
      89, &
      90, &
      96, &
      97, &
      98, &
      106, &
      107, &
      109, &
      117, &
      118, &
      206, &
      1, &
      2, &
      5, &
      81, &
      82, &
      88, &
      90, &
      91, &
      97, &
      98, &
      99, &
      107, &
      108, &
      110, &
      118, &
      119, &
      206, &
      1, &
      2, &
      5, &
      82, &
      83, &
      89, &
      91, &
      92, &
      98, &
      99, &
      100, &
      108, &
      109, &
      111, &
      119, &
      120, &
      206, &
      1, &
      2, &
      5, &
      83, &
      90, &
      92, &
      93, &
      99, &
      100, &
      101, &
      109, &
      110, &
      112, &
      120, &
      121, &
      206, &
      1, &
      2, &
      5, &
      91, &
      93, &
      94, &
      100, &
      101, &
      102, &
      110, &
      111, &
      113, &
      121, &
      122, &
      206, &
      1, &
      2, &
      5, &
      92, &
      94, &
      101, &
      102, &
      103, &
      111, &
      112, &
      114, &
      122, &
      123, &
      206, &
      1, &
      2, &
      5, &
      93, &
      102, &
      103, &
      112, &
      113, &
      115, &
      123, &
      124, &
      206, &
      1, &
      2, &
      5, &
      84, &
      85, &
      95, &
      104, &
      105, &
      126, &
      206, &
      1, &
      2, &
      5, &
      85, &
      86, &
      95, &
      96, &
      104, &
      105, &
      106, &
      116, &
      126, &
      127, &
      206, &
      1, &
      2, &
      5, &
      86, &
      87, &
      96, &
      97, &
      105, &
      106, &
      107, &
      117, &
      127, &
      128, &
      206, &
      1, &
      2, &
      5, &
      87, &
      88, &
      95, &
      97, &
      98, &
      106, &
      107, &
      108, &
      116, &
      118, &
      128, &
      129, &
      206, &
      1, &
      2, &
      5, &
      88, &
      89, &
      96, &
      98, &
      99, &
      107, &
      108, &
      109, &
      116, &
      117, &
      119, &
      129, &
      130, &
      206, &
      1, &
      2, &
      5, &
      89, &
      90, &
      97, &
      99, &
      100, &
      108, &
      109, &
      110, &
      117, &
      118, &
      120, &
      130, &
      131, &
      206, &
      1, &
      2, &
      5, &
      90, &
      91, &
      98, &
      100, &
      101, &
      109, &
      110, &
      111, &
      118, &
      119, &
      121, &
      131, &
      132, &
      206, &
      1, &
      2, &
      5, &
      91, &
      92, &
      99, &
      101, &
      102, &
      110, &
      111, &
      112, &
      119, &
      120, &
      122, &
      132, &
      133, &
      206, &
      1, &
      2, &
      5, &
      92, &
      93, &
      100, &
      102, &
      103, &
      111, &
      112, &
      113, &
      120, &
      121, &
      123, &
      133, &
      134, &
      206, &
      1, &
      2, &
      5, &
      93, &
      94, &
      101, &
      103, &
      112, &
      113, &
      114, &
      121, &
      122, &
      124, &
      134, &
      135, &
      206, &
      1, &
      2, &
      5, &
      94, &
      102, &
      113, &
      114, &
      115, &
      122, &
      123, &
      125, &
      135, &
      136, &
      206, &
      1, &
      2, &
      5, &
      103, &
      114, &
      115, &
      123, &
      124, &
      136, &
      137, &
      206, &
      1, &
      2, &
      5, &
      95, &
      96, &
      105, &
      107, &
      108, &
      116, &
      117, &
      126, &
      127, &
      129, &
      138, &
      206, &
      1, &
      2, &
      5, &
      96, &
      97, &
      106, &
      108, &
      109, &
      116, &
      117, &
      118, &
      127, &
      128, &
      130, &
      138, &
      139, &
      206, &
      1, &
      2, &
      5, &
      97, &
      98, &
      107, &
      109, &
      110, &
      117, &
      118, &
      119, &
      128, &
      129, &
      131, &
      139, &
      140, &
      206, &
      1, &
      2, &
      5, &
      98, &
      99, &
      108, &
      110, &
      111, &
      118, &
      119, &
      120, &
      129, &
      130, &
      132, &
      140, &
      141, &
      206, &
      1, &
      2, &
      5, &
      99, &
      100, &
      109, &
      111, &
      112, &
      119, &
      120, &
      121, &
      130, &
      131, &
      133, &
      141, &
      142, &
      206, &
      1, &
      2, &
      5, &
      100, &
      101, &
      110, &
      112, &
      113, &
      120, &
      121, &
      122, &
      131, &
      132, &
      134, &
      142, &
      143, &
      206, &
      1, &
      2, &
      5, &
      101, &
      102, &
      111, &
      113, &
      114, &
      121, &
      122, &
      123, &
      132, &
      133, &
      135, &
      143, &
      144, &
      206, &
      1, &
      2, &
      5, &
      102, &
      103, &
      112, &
      114, &
      115, &
      122, &
      123, &
      124, &
      133, &
      134, &
      136, &
      144, &
      145, &
      206, &
      1, &
      2, &
      5, &
      103, &
      113, &
      115, &
      123, &
      124, &
      125, &
      134, &
      135, &
      137, &
      145, &
      146, &
      206, &
      1, &
      2, &
      5, &
      114, &
      124, &
      125, &
      135, &
      136, &
      146, &
      206, &
      1, &
      2, &
      5, &
      104, &
      105, &
      116, &
      126, &
      127, &
      147, &
      206, &
      1, &
      2, &
      5, &
      105, &
      106, &
      116, &
      117, &
      126, &
      127, &
      128, &
      138, &
      147, &
      148, &
      206, &
      1, &
      2, &
      5, &
      106, &
      107, &
      117, &
      118, &
      127, &
      128, &
      129, &
      139, &
      148, &
      149, &
      206, &
      1, &
      2, &
      5, &
      107, &
      108, &
      116, &
      118, &
      119, &
      128, &
      129, &
      130, &
      138, &
      140, &
      149, &
      150, &
      206, &
      1, &
      2, &
      5, &
      108, &
      109, &
      117, &
      119, &
      120, &
      129, &
      130, &
      131, &
      138, &
      139, &
      141, &
      150, &
      151, &
      206, &
      1, &
      2, &
      5, &
      109, &
      110, &
      118, &
      120, &
      121, &
      130, &
      131, &
      132, &
      139, &
      140, &
      142, &
      151, &
      152, &
      206, &
      1, &
      2, &
      5, &
      110, &
      111, &
      119, &
      121, &
      122, &
      131, &
      132, &
      133, &
      140, &
      141, &
      143, &
      152, &
      153, &
      206, &
      1, &
      2, &
      5, &
      111, &
      112, &
      120, &
      122, &
      123, &
      132, &
      133, &
      134, &
      141, &
      142, &
      144, &
      153, &
      154, &
      206, &
      1, &
      2, &
      5, &
      112, &
      113, &
      121, &
      123, &
      124, &
      125, &
      133, &
      134, &
      135, &
      142, &
      143, &
      145, &
      154, &
      155, &
      206, &
      1, &
      2, &
      5, &
      113, &
      114, &
      122, &
      124, &
      125, &
      134, &
      135, &
      136, &
      143, &
      144, &
      146, &
      155, &
      156, &
      206, &
      1, &
      2, &
      5, &
      114, &
      115, &
      123, &
      125, &
      135, &
      136, &
      137, &
      144, &
      145, &
      156, &
      157, &
      206, &
      1, &
      2, &
      5, &
      115, &
      124, &
      136, &
      137, &
      145, &
      146, &
      157, &
      158, &
      206, &
      1, &
      2, &
      5, &
      116, &
      117, &
      127, &
      129, &
      130, &
      138, &
      139, &
      147, &
      148, &
      150, &
      163, &
      206, &
      1, &
      2, &
      5, &
      117, &
      118, &
      128, &
      130, &
      131, &
      138, &
      139, &
      140, &
      148, &
      149, &
      151, &
      163, &
      164, &
      206, &
      1, &
      2, &
      5, &
      118, &
      119, &
      129, &
      131, &
      132, &
      139, &
      140, &
      141, &
      149, &
      150, &
      152, &
      164, &
      165, &
      206, &
      1, &
      2, &
      5, &
      119, &
      120, &
      130, &
      132, &
      133, &
      140, &
      141, &
      142, &
      150, &
      151, &
      153, &
      165, &
      166, &
      206, &
      1, &
      2, &
      5, &
      120, &
      121, &
      131, &
      133, &
      134, &
      141, &
      142, &
      143, &
      151, &
      152, &
      154, &
      166, &
      167, &
      206, &
      1, &
      2, &
      5, &
      121, &
      122, &
      132, &
      134, &
      135, &
      142, &
      143, &
      144, &
      152, &
      153, &
      155, &
      167, &
      168, &
      206, &
      1, &
      2, &
      5, &
      122, &
      123, &
      133, &
      135, &
      136, &
      143, &
      144, &
      145, &
      153, &
      154, &
      156, &
      168, &
      169, &
      206, &
      1, &
      2, &
      5, &
      123, &
      124, &
      134, &
      136, &
      137, &
      144, &
      145, &
      146, &
      154, &
      155, &
      157, &
      169, &
      170, &
      206, &
      1, &
      2, &
      5, &
      124, &
      125, &
      135, &
      137, &
      145, &
      146, &
      155, &
      156, &
      158, &
      170, &
      171, &
      206, &
      1, &
      2, &
      5, &
      126, &
      127, &
      138, &
      147, &
      148, &
      176, &
      206, &
      1, &
      2, &
      5, &
      127, &
      128, &
      138, &
      139, &
      147, &
      148, &
      149, &
      163, &
      176, &
      177, &
      206, &
      1, &
      2, &
      5, &
      128, &
      129, &
      139, &
      140, &
      148, &
      149, &
      150, &
      164, &
      177, &
      178, &
      206, &
      1, &
      2, &
      5, &
      129, &
      130, &
      138, &
      140, &
      141, &
      149, &
      150, &
      151, &
      163, &
      165, &
      178, &
      179, &
      206, &
      1, &
      2, &
      5, &
      130, &
      131, &
      139, &
      141, &
      142, &
      150, &
      151, &
      152, &
      163, &
      164, &
      166, &
      179, &
      180, &
      206, &
      1, &
      2, &
      5, &
      131, &
      132, &
      140, &
      142, &
      143, &
      151, &
      152, &
      153, &
      164, &
      165, &
      167, &
      180, &
      181, &
      206, &
      1, &
      2, &
      5, &
      132, &
      133, &
      141, &
      143, &
      144, &
      152, &
      153, &
      154, &
      165, &
      166, &
      168, &
      181, &
      182, &
      206, &
      1, &
      2, &
      5, &
      133, &
      134, &
      142, &
      144, &
      145, &
      153, &
      154, &
      155, &
      166, &
      167, &
      169, &
      182, &
      183, &
      206, &
      1, &
      2, &
      5, &
      134, &
      135, &
      143, &
      145, &
      146, &
      154, &
      155, &
      156, &
      167, &
      168, &
      170, &
      183, &
      184, &
      206, &
      1, &
      2, &
      5, &
      135, &
      136, &
      144, &
      146, &
      155, &
      156, &
      157, &
      168, &
      169, &
      171, &
      184, &
      185, &
      206, &
      1, &
      2, &
      5, &
      136, &
      137, &
      145, &
      156, &
      157, &
      158, &
      169, &
      170, &
      172, &
      185, &
      186, &
      206, &
      1, &
      2, &
      5, &
      137, &
      146, &
      157, &
      158, &
      159, &
      170, &
      171, &
      173, &
      186, &
      187, &
      206, &
      1, &
      2, &
      5, &
      158, &
      159, &
      160, &
      171, &
      172, &
      174, &
      187, &
      188, &
      206, &
      1, &
      2, &
      5, &
      159, &
      160, &
      161, &
      172, &
      173, &
      175, &
      188, &
      189, &
      206, &
      1, &
      2, &
      5, &
      160, &
      161, &
      162, &
      173, &
      174, &
      189, &
      206, &
      1, &
      2, &
      161, &
      162, &
      174, &
      175, &
      206, &
      1, &
      2, &
      5, &
      138, &
      139, &
      148, &
      150, &
      151, &
      163, &
      164, &
      176, &
      177, &
      179, &
      190, &
      206, &
      1, &
      2, &
      5, &
      139, &
      140, &
      149, &
      151, &
      152, &
      163, &
      164, &
      165, &
      177, &
      178, &
      180, &
      190, &
      191, &
      206, &
      1, &
      2, &
      5, &
      140, &
      141, &
      150, &
      152, &
      153, &
      164, &
      165, &
      166, &
      178, &
      179, &
      181, &
      191, &
      192, &
      206, &
      1, &
      2, &
      5, &
      141, &
      142, &
      151, &
      153, &
      154, &
      165, &
      166, &
      167, &
      179, &
      180, &
      182, &
      192, &
      193, &
      206, &
      1, &
      2, &
      5, &
      142, &
      143, &
      152, &
      154, &
      155, &
      166, &
      167, &
      168, &
      180, &
      181, &
      183, &
      193, &
      194, &
      206, &
      1, &
      2, &
      5, &
      143, &
      144, &
      153, &
      155, &
      156, &
      167, &
      168, &
      169, &
      181, &
      182, &
      184, &
      194, &
      195, &
      206, &
      1, &
      2, &
      5, &
      144, &
      145, &
      154, &
      156, &
      157, &
      168, &
      169, &
      170, &
      182, &
      183, &
      185, &
      195, &
      196, &
      206, &
      1, &
      2, &
      5, &
      145, &
      146, &
      155, &
      157, &
      158, &
      169, &
      170, &
      171, &
      183, &
      184, &
      186, &
      196, &
      197, &
      206, &
      1, &
      2, &
      5, &
      146, &
      156, &
      158, &
      159, &
      170, &
      171, &
      172, &
      184, &
      185, &
      187, &
      197, &
      206, &
      1, &
      2, &
      5, &
      157, &
      159, &
      160, &
      171, &
      172, &
      173, &
      185, &
      186, &
      188, &
      206, &
      1, &
      2, &
      5, &
      158, &
      160, &
      161, &
      162, &
      172, &
      173, &
      174, &
      186, &
      187, &
      189, &
      206, &
      1, &
      2, &
      5, &
      159, &
      161, &
      162, &
      173, &
      174, &
      175, &
      187, &
      188, &
      206, &
      1, &
      2, &
      5, &
      160, &
      162, &
      174, &
      175, &
      188, &
      189, &
      206, &
      1, &
      2, &
      5, &
      147, &
      148, &
      163, &
      176, &
      177, &
      198, &
      206, &
      1, &
      2, &
      5, &
      148, &
      149, &
      163, &
      164, &
      176, &
      177, &
      178, &
      190, &
      198, &
      199, &
      206, &
      1, &
      2, &
      5, &
      149, &
      150, &
      164, &
      165, &
      177, &
      178, &
      179, &
      191, &
      199, &
      200, &
      206, &
      1, &
      2, &
      5, &
      150, &
      151, &
      163, &
      165, &
      166, &
      178, &
      179, &
      180, &
      190, &
      192, &
      198, &
      200, &
      201, &
      206, &
      1, &
      2, &
      5, &
      151, &
      152, &
      164, &
      166, &
      167, &
      179, &
      180, &
      181, &
      190, &
      191, &
      193, &
      201, &
      202, &
      206, &
      1, &
      2, &
      5, &
      152, &
      153, &
      165, &
      167, &
      168, &
      180, &
      181, &
      182, &
      191, &
      192, &
      194, &
      202, &
      203, &
      206, &
      1, &
      2, &
      5, &
      153, &
      154, &
      166, &
      168, &
      169, &
      181, &
      182, &
      183, &
      192, &
      193, &
      195, &
      203, &
      204, &
      206, &
      1, &
      2, &
      5, &
      154, &
      155, &
      167, &
      169, &
      170, &
      182, &
      183, &
      184, &
      193, &
      194, &
      196, &
      204, &
      205, &
      206, &
      1, &
      2, &
      5, &
      155, &
      156, &
      168, &
      170, &
      171, &
      183, &
      184, &
      185, &
      194, &
      195, &
      197, &
      205, &
      206, &
      1, &
      2, &
      5, &
      156, &
      157, &
      169, &
      171, &
      172, &
      184, &
      185, &
      186, &
      195, &
      196, &
      206, &
      1, &
      2, &
      5, &
      157, &
      158, &
      170, &
      172, &
      173, &
      185, &
      186, &
      187, &
      196, &
      197, &
      206, &
      1, &
      2, &
      5, &
      158, &
      159, &
      171, &
      173, &
      174, &
      175, &
      186, &
      187, &
      188, &
      197, &
      206, &
      1, &
      2, &
      5, &
      159, &
      160, &
      172, &
      174, &
      175, &
      187, &
      188, &
      189, &
      206, &
      1, &
      2, &
      5, &
      160, &
      161, &
      173, &
      175, &
      188, &
      189, &
      206, &
      1, &
      2, &
      5, &
      163, &
      164, &
      177, &
      179, &
      180, &
      190, &
      191, &
      198, &
      199, &
      201, &
      206, &
      1, &
      2, &
      5, &
      164, &
      165, &
      178, &
      180, &
      181, &
      190, &
      191, &
      192, &
      199, &
      200, &
      202, &
      206, &
      1, &
      2, &
      5, &
      165, &
      166, &
      179, &
      181, &
      182, &
      191, &
      192, &
      193, &
      200, &
      201, &
      203, &
      206, &
      1, &
      2, &
      5, &
      166, &
      167, &
      180, &
      182, &
      183, &
      192, &
      193, &
      194, &
      201, &
      202, &
      204, &
      206, &
      1, &
      2, &
      5, &
      167, &
      168, &
      181, &
      183, &
      184, &
      193, &
      194, &
      195, &
      202, &
      203, &
      205, &
      206, &
      1, &
      2, &
      5, &
      168, &
      169, &
      182, &
      184, &
      185, &
      194, &
      195, &
      196, &
      203, &
      204, &
      206, &
      1, &
      2, &
      5, &
      169, &
      170, &
      183, &
      185, &
      186, &
      195, &
      196, &
      197, &
      204, &
      205, &
      206, &
      1, &
      2, &
      5, &
      170, &
      171, &
      184, &
      186, &
      187, &
      196, &
      197, &
      205, &
      206, &
      1, &
      2, &
      5, &
      176, &
      177, &
      190, &
      198, &
      199, &
      206, &
      1, &
      2, &
      5, &
      177, &
      178, &
      190, &
      191, &
      198, &
      199, &
      200, &
      206, &
      1, &
      2, &
      5, &
      178, &
      179, &
      191, &
      192, &
      199, &
      200, &
      201, &
      206, &
      1, &
      2, &
      5, &
      179, &
      180, &
      190, &
      192, &
      193, &
      200, &
      201, &
      202, &
      206, &
      1, &
      2, &
      5, &
      180, &
      181, &
      191, &
      193, &
      194, &
      201, &
      202, &
      203, &
      206, &
      1, &
      2, &
      5, &
      181, &
      182, &
      192, &
      194, &
      195, &
      202, &
      203, &
      204, &
      206, &
      1, &
      2, &
      5, &
      182, &
      183, &
      193, &
      195, &
      196, &
      203, &
      204, &
      205, &
      206, &
      1, &
      2, &
      5, &
      183, &
      184, &
      194, &
      196, &
      197, &
      204, &
      205, &
      206, &
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
      207  ]

    csr_jac_row_count = [ &
      1, &
      206, &
      411, &
      423, &
      432, &
      636, &
      647, &
      659, &
      670, &
      679, &
      688, &
      693, &
      706, &
      716, &
      738, &
      751, &
      761, &
      775, &
      788, &
      798, &
      809, &
      831, &
      844, &
      856, &
      867, &
      870, &
      881, &
      897, &
      914, &
      928, &
      940, &
      960, &
      976, &
      990, &
      1002, &
      1014, &
      1030, &
      1048, &
      1062, &
      1075, &
      1093, &
      1109, &
      1123, &
      1135, &
      1146, &
      1161, &
      1180, &
      1194, &
      1208, &
      1226, &
      1241, &
      1256, &
      1270, &
      1281, &
      1291, &
      1304, &
      1323, &
      1339, &
      1354, &
      1367, &
      1381, &
      1395, &
      1410, &
      1426, &
      1442, &
      1456, &
      1468, &
      1483, &
      1500, &
      1517, &
      1531, &
      1541, &
      1555, &
      1569, &
      1585, &
      1601, &
      1616, &
      1629, &
      1644, &
      1661, &
      1678, &
      1693, &
      1706, &
      1718, &
      1728, &
      1742, &
      1756, &
      1772, &
      1789, &
      1806, &
      1821, &
      1834, &
      1847, &
      1859, &
      1869, &
      1884, &
      1901, &
      1918, &
      1935, &
      1952, &
      1968, &
      1983, &
      1997, &
      2009, &
      2019, &
      2033, &
      2047, &
      2063, &
      2080, &
      2097, &
      2114, &
      2131, &
      2148, &
      2164, &
      2178, &
      2189, &
      2204, &
      2221, &
      2238, &
      2255, &
      2272, &
      2289, &
      2306, &
      2323, &
      2338, &
      2348, &
      2358, &
      2372, &
      2386, &
      2402, &
      2419, &
      2436, &
      2453, &
      2470, &
      2488, &
      2505, &
      2520, &
      2532, &
      2547, &
      2564, &
      2581, &
      2598, &
      2615, &
      2632, &
      2649, &
      2666, &
      2681, &
      2691, &
      2705, &
      2719, &
      2735, &
      2752, &
      2769, &
      2786, &
      2803, &
      2820, &
      2836, &
      2851, &
      2865, &
      2877, &
      2889, &
      2899, &
      2906, &
      2921, &
      2938, &
      2955, &
      2972, &
      2989, &
      3006, &
      3023, &
      3040, &
      3055, &
      3068, &
      3082, &
      3094, &
      3104, &
      3114, &
      3128, &
      3142, &
      3159, &
      3176, &
      3193, &
      3210, &
      3227, &
      3243, &
      3257, &
      3271, &
      3285, &
      3297, &
      3307, &
      3321, &
      3336, &
      3351, &
      3366, &
      3381, &
      3395, &
      3409, &
      3421, &
      3430, &
      3441, &
      3452, &
      3464, &
      3476, &
      3488, &
      3500, &
      3511, &
      3717, &
      3924  ]
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
