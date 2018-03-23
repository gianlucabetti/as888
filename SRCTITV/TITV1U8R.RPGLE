000100020916      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020916     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700000724     D*------------
000800990920     D dscmz         e ds                  inz
000900990910     D psds           sds
001000990910     D  procname         *PROC
001100941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001200941229     D* 10+3
001300941229     D                 DS
001400000313     D  WI15                   1     10
001500000313     D  WD15                  11     13
001600000313     D  WASC15                 1     13
001700941229     D*  6+1
001800941229     D                 DS
001900000313     D  WI09                   1      6
002000000313     D  WD09                   7      7
002100000313     D  WASC09                 1      7
002200941229     D*  2+3
002300941229     D                 DS
002400000313     D  WI07                   1      2
002500000313     D  WD07                   3      5
002600000313     D  WASC07                 1      5
002700000313     D*
002800990920     D tivlrds       e ds                  extname(tivlr00f)
002900041025     D tisi95ds      e ds
003000000313     D*---------------------------------------------------------
003100000313     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
003200000313     D*---------------------------------------------------------
003300000313     D                 ds
003400000313     D vhbatb                  1      1
003500000313     D vhbccm                  2      8
003600000313     D vhblnp                  9     11
003700000313     D vhbaas                 12     15
003800000313     D vhbmgs                 16     19
003900000313     D vhbnrs                 20     21
004000000313     D vhbnsp                 22     28
004100000313     D vhbcbo                 29     30
004200000313     D vhblna                 31     33
004300000313     D vhbrsd                 34     68
004400000313     D vhbrd2                 69    103
004500000313     D vhbind                104    138
004600000313     D vhbcad                139    147
004700000313     D vhblod                148    182
004800000313     D vhbprd                183    184
004900000313     D vhbnzd                185    187
005000000313     D vhbgc1                188    189
005100000313     D vhbgc2                190    191
005200000313     D vhbctr                192    194
005300000313     D vhbtsp                195    195
005400000313     D vhbias                196    208
005500000313     D vhbvas                209    211
005600000313     D vhbnas                212    226
005700000313     D vhbncl                227    231
005800000313     D vhbpkb                232    238
005900000313     D vhbvlb                239    243
006000000313     D vhbqft                244    256
006100000313     D vhbcas                257    269
006200000313     D vhbtic                270    271
006300000313     D vhbvca                272    274
006400000313     D vhbgca                275    276
006500000313     D vhbrmn                277    291
006600000313     D vhbrma                292    306
006700000313     D vhbncd                307    313
006800000313     D vhbnca                314    320
006900000313     D vhbxco                321    321
007000000313     D vhbnot                322    356
007100000313     D vhbnt2                357    391
007200000313     D vhbznc                392    393
007300000313     D vhbctm                394    395
007400000313     D vhbffd                396    396
007500000313     D vhbdcr                397    404
007600000313     D vhbtcr                405    405
007700000313     D vhbhcr                406    409
007800000313     D vhbcts                410    411
007900000313     D vhbftm                412    412
008000000313     D vhbvmd                413    425
008100000313     D vhbvad                426    428
008200000313     D vhbgma                429    430
008300000313     D vhbgga                431    432
008400000313     D vhbgva                433    434
008500000313     D vhbtc1                435    435
008600000313     D vhbtc2                436    436
008700000313     D vhbscl                437    437
008800000313     D vhbant                438    446
008900000313     D vhbrmo                447    471
009000000313     D vhbcmo                472    480
009100000313     D vhbnmo                481    483
009200000313     D  X_rec                  1    483    inz
009300000313     D*
009400990910     D esito           s              1
009500000724     D prmlit          s             10
009600000710     D prmfir          s             10
009700990921     D wrkesito        s                   like(esito)
009800000613     D rrnum           s              6  0 INZ(*zeros)
009900000313     D*
010000010606     D*------------------
010100010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
010200010606     D*------------------
010300010606     D WLBDA8          DS                  INZ
010400010606     D  G08DAT                 1      8  0
010500010606     D  G08INV                 9     16  0
010600010606     D  G08ERR                17     17
010700010606     D  G08TGI                18     22  0
010800010606     D*
010900040407     D TxtInOut        S           2048
011000040407     D ElencoChar      S            256
011100040407     D TipoElenco      S              1
011200040407     D CharSost        S              1
011300040407     D UpperCase       S              1
011400040407     D ChkNull         S              1
011500040407     D CharNull        S              1
011600040407     D EsitoBon        S              1
011700010606
011800010606
011900010606
012000010606
012100000913     C                   reset                   rrnum
012200990921     C                   reset                   esito
012300990921     C                   reset                   wrkesito
012400010601     C*
012500010601     C                   exsr      opeini
012600010601     C                   exsr      rwvab
012700041025     C*
012800041025     C* Esegue lancio TISI95R solo x chiusura
012900041025     C                   CLEAR                   TISI95DS
013000041025     C                   EVAL      I95TLA = 'C'
013100041025     C                   CALL      'TISI95R'
013200041025     C                   PARM                    TISI95DS
013300010601     C*
013400010601     C                   seton                                        lr
013500010601
013600010601
013700010601
013800010601
013900010601     C*--------------------------------------------------------
014000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
014100010601     C*--------------------------------------------------------
014200010601     C     PREELA        BEGSR
014300010601     C*
014400010601     C* SE OCCORRE SPEDIRE IN FILIALE
014500010601     C                   if        invfil <> *zeros and
014600010601     C                             flgGiro = '0'
014700010601     C*
014800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
014900010601     C                   eval      flgGiro = '1'
015000010601     C*
015100010601     C                   endif
015200010601     C*
015300010601     C                   ENDSR
015400010601     C***
015500010601
015600010601
015700010601
015800010601
015900010601
016000010601
016100010601     C*--------------------------------------------------------
016200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016300010601     C*--------------------------------------------------------
016400010601     C     ENDELA        BEGSR
016500010601     C*
016600010601     C                   ENDSR
016700010601     C***
016800000613
016900010601
017000010601
017100010601
017200010601
017300910830     C*--------------------------------------------------------
017400020916     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
017500910830     C*--------------------------------------------------------
017600941229     C     RWVAB         BEGSR
017700990910     C*
017800990914     C                   if        not %open(tivin00r)
017900990908     C                   open      tivin00r
018000990914     C                   endif
018100020916     C                   if        not %open(fivabwwr)
018200020916     C                   open      fivabwwr
018300990914     C                   endif
018400990910     C*
018500990910     C                   clear                   §CTROK
018600990910     C                   clear                   §CTRMO
018700990910     C                   clear                   §CTRNO
018800990910     C*
018900921023     C                   DO        *HIVAL
019000990913     C*
019100990915     C                   READ      tivin00r                               70
019200010618     C*
019300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019400010618     C                   if        vinflg = '1'
019500010618     C                   eval      flgOk = '1'
019600010618     C                   endif
019700010618     C*
019800051017     C                   if        vindta  > *blanks
019900000613     C                   add       1             rrnum
020000990913     C*
020100010601     C                   if        *in70 = *off and
020200990913     C                             (vinflg = *blanks
020300010711     C                              or vinflg = '0'
020400010711     C                              or vinflg = '2')
020500990913     C*
020600020916     C                   clear                   FIVAB000
020700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020800010711     C                   if        vinflg = *blanks or vinflg = '0'
020900010711     C                   clear                   vinmsg
021000010711     C                   endif
021100010711     C*
021200000313     C                   eval      x_rec  = vindta
021300991022     C*
021400991022     C                   testb     x'3F'         vhbatb                   02
021500991022     C                   if        not *in02
021600991022     C*
021700971021     C                   SETOFF                                       303132
021800971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
021900971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
022000971023     C*    OPPURE TUTTI BLANKS
022100010601     C*
022200010601     C                   TESTN                   VHBLNP               30
022300010601     C  N30              TESTN                   VHBLNP                 30
022400010601     C  N30              TESTN                   VHBLNP                   30
022500010601     C                   MOVE      VHBLNP        W001A             1
022600010601     C     *IN30         IFEQ      *OFF
022700010601     C     W001A         ORLT      '0'
022800010601     C                   SETON                                        31
022900010601     C                   eval      vinmsg = %trimr(vinmsg)
023000010601     C                             + ' ' + 'VHBLNP'
023100010601     C                   ELSE
023200010601     C                   MOVE      VHBLNP        VABLNP
023300010601     C     VABLNP        IFLE      *ZEROS
023400010601     C                   SETON                                        31
023500010601     C                   eval      vinmsg = %trimr(vinmsg)
023600010601     C                             + ' ' + 'VHBLNP'
023700010601     C                   ENDIF
023800010601     C                   ENDIF
023900010601     C*
024000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024100010601     C                   eval      depfil = VABLNP
024200010601     C                   exsr      repfil
024300010601     C                   if        depfil = invfil
024400021014     C                   if        vlrpoi = 999
024500021010     C                   MOVE(P)   invfil        VABFGS
024600021014     C                   else
024700021014     C                   MOVE(P)   vlrpoi        VABFGS
024800021014     C                   endif
024900010601     C*
025000010601     C                   exsr      PREELA
025100010601     C*
025200971023     C                   TESTN                   VHBCCM               30
025300971023     C  N30              TESTN                   VHBCCM                 30
025400971023     C  N30              TESTN                   VHBCCM                   30
025500971021     C                   MOVE      VHBCCM        W001A             1
025600971021     C     *IN30         IFEQ      *OFF
025700971021     C     W001A         ORLT      '0'
025800971021     C                   SETON                                        31
025900010604     C                   eval      vinmsg = %trimr(vinmsg)
026000010604     C                             + ' ' + 'VHBCCM'
026100971021     C                   ELSE
026200971021     C                   MOVE      VHBCCM        VABCCM
026300971024     C     VABCCM        IFLE      *ZEROS
026400971024     C                   SETON                                        31
026500010604     C                   eval      vinmsg = %trimr(vinmsg)
026600010604     C                             + ' ' + 'VHBCCM'
026700971024     C                   ENDIF
026800971021     C                   ENDIF
026900971021     C*
027000971023     C                   TESTN                   VHBAAS               30
027100971023     C  N30              TESTN                   VHBAAS                 30
027200971023     C  N30              TESTN                   VHBAAS                   30
027300971021     C                   MOVE      VHBAAS        W001A             1
027400971021     C     *IN30         IFEQ      *OFF
027500971021     C     W001A         ORLT      '0'
027600971023     C     W001A         ANDNE     *BLANK
027700971021     C                   SETON                                        32
027800010606     C                   MOVEL     datcor        VABAAS
027900010604     C                   eval      vinmsg = %trimr(vinmsg)
028000010604     C                             + ' ' + 'VHBAAS'
028100971021     C                   ELSE
028200971021     C                   MOVE      VHBAAS        VABAAS
028300971021     C                   ENDIF
028400971021     C*
028500971023     C                   TESTN                   VHBMGS               30
028600971023     C  N30              TESTN                   VHBMGS                 30
028700971023     C  N30              TESTN                   VHBMGS                   30
028800971021     C                   MOVE      VHBMGS        W001A             1
028900971021     C     *IN30         IFEQ      *OFF
029000971021     C     W001A         ORLT      '0'
029100971023     C     W001A         ANDNE     *BLANK
029200971021     C                   SETON                                        32
029300010601     C                   MOVE      '0101'        VABMGS
029400010604     C                   eval      vinmsg = %trimr(vinmsg)
029500010604     C                             + ' ' + 'VHBMGS'
029600971021     C                   ELSE
029700971021     C                   MOVE      VHBMGS        VABMGS
029800971021     C                   ENDIF
029900971021     C*
030000971023     C                   TESTN                   VHBNRS               30
030100971023     C  N30              TESTN                   VHBNRS                 30
030200971023     C  N30              TESTN                   VHBNRS                   30
030300971021     C                   MOVE      VHBNRS        W001A             1
030400971021     C     *IN30         IFEQ      *OFF
030500971021     C     W001A         ORLT      '0'
030600971023     C     W001A         ANDNE     *BLANK
030700971021     C                   SETON                                        32
030800971021     C                   MOVE      *ZERO         VABNRS
030900010604     C                   eval      vinmsg = %trimr(vinmsg)
031000010604     C                             + ' ' + 'VHBNRS'
031100971021     C                   ELSE
031200971021     C                   MOVE      VHBNRS        VABNRS
031300971021     C                   ENDIF
031400971021     C*
031500971023     C                   TESTN                   VHBNSP               30
031600971023     C  N30              TESTN                   VHBNSP                 30
031700971023     C  N30              TESTN                   VHBNSP                   30
031800971021     C                   MOVE      VHBNSP        W001A             1
031900971021     C     *IN30         IFEQ      *OFF
032000971021     C     W001A         ORLT      '0'
032100971023     C     W001A         ANDNE     *BLANK
032200971021     C                   SETON                                        32
032300971021     C                   MOVE      *ZERO         VABNSP
032400010604     C                   eval      vinmsg = %trimr(vinmsg)
032500010604     C                             + ' ' + 'VHBNSP'
032600971021     C                   ELSE
032700971021     C                   MOVE      VHBNSP        VABNSP
032800971021     C                   ENDIF
032900971021     C*
033000971023     C                   TESTN                   VHBLNA               30
033100971023     C  N30              TESTN                   VHBLNA                 30
033200971023     C  N30              TESTN                   VHBLNA                   30
033300971021     C                   MOVE      VHBLNA        W001A             1
033400971021     C     *IN30         IFEQ      *OFF
033500971021     C     W001A         ORLT      '0'
033600971023     C     W001A         ANDNE     *BLANK
033700971021     C                   SETON                                        32
033800971021     C                   MOVE      *ZERO         VABLNA
033900010604     C                   eval      vinmsg = %trimr(vinmsg)
034000010604     C                             + ' ' + 'VHBLNA'
034100971021     C                   ELSE
034200971021     C                   MOVE      VHBLNA        VABLNA
034300971021     C                   ENDIF
034400971021     C*
034500971023     C                   TESTN                   VHBCTR               30
034600971023     C  N30              TESTN                   VHBCTR                 30
034700971023     C  N30              TESTN                   VHBCTR                   30
034800971021     C                   MOVE      VHBCTR        W001A             1
034900971021     C     *IN30         IFEQ      *OFF
035000971021     C     W001A         ORLT      '0'
035100971023     C     W001A         ANDNE     *BLANK
035200971021     C                   SETON                                        32
035300971021     C                   MOVE      *ZERO         VABCTR
035400010604     C                   eval      vinmsg = %trimr(vinmsg)
035500010604     C                             + ' ' + 'VHBCTR'
035600971021     C                   ELSE
035700971021     C                   MOVE      VHBCTR        VABCTR
035800971021     C                   ENDIF
035900941229     C*
036000971021     C                   MOVE      *ZERO         VABIAS
036100941229     C                   MOVE      VHBIAS        WASC15
036200971023     C                   TESTN                   WI15                 30
036300971023     C  N30              TESTN                   WI15                   30
036400971023     C  N30              TESTN                   WI15                     30
036500971021     C                   MOVE      WI15          W001A             1
036600971021     C     *IN30         IFEQ      *OFF
036700971021     C     W001A         ORLT      '0'
036800971023     C     W001A         ANDNE     *BLANK
036900971021     C                   SETON                                        32
037000971021     C                   MOVE      *ZERO         VABIAS
037100010604     C                   eval      vinmsg = %trimr(vinmsg)
037200010604     C                             + ' ' + 'VHBIAS'
037300971021     C                   ELSE
037400971021     C                   MOVEL     WI15          VABIAS
037500971023     C                   TESTN                   WD15                 30
037600971023     C  N30              TESTN                   WD15                   30
037700971023     C  N30              TESTN                   WD15                     30
037800971021     C                   MOVE      WD15          W001A             1
037900971021     C     *IN30         IFEQ      *OFF
038000971021     C     W001A         ORLT      '0'
038100971023     C     W001A         ANDNE     *BLANK
038200971021     C                   SETON                                        32
038300971021     C                   MOVE      *ZERO         VABIAS
038400010604     C                   eval      vinmsg = %trimr(vinmsg)
038500010604     C                             + ' ' + 'VHBIAS'
038600971021     C                   ELSE
038700971021     C                   MOVE      WD15          VABIAS
038800971021     C                   ENDIF
038900050118     C                   ENDIF
039000941229     C*
039100971023     C                   TESTN                   VHBNCL               30
039200971023     C  N30              TESTN                   VHBNCL                 30
039300971023     C  N30              TESTN                   VHBNCL                   30
039400971021     C                   MOVE      VHBNCL        W001A             1
039500971021     C     *IN30         IFEQ      *OFF
039600971021     C     W001A         ORLT      '0'
039700971021     C                   SETON                                        32
039800971021     C                   MOVE      *ZERO         VABNCL
039900010604     C                   eval      vinmsg = %trimr(vinmsg)
040000010604     C                             + ' ' + 'VHBNCL'
040100971021     C                   ELSE
040200971021     C                   MOVE      VHBNCL        VABNCL
040300971021     C                   ENDIF
040400941229     C*
040500971021     C                   MOVE      *ZERO         VABPKB
040600971021     C                   MOVE      VHBPKB        WASC09
040700050503     C                   IF        WI09 = *blanks
040800050503     C                   EVAL      WI09 = *zeros
040900050503     C                   ENDIF
041000971023     C                   TESTN                   WI09                 30
041100971023     C  N30              TESTN                   WI09                   30
041200971023     C  N30              TESTN                   WI09                     30
041300971021     C                   MOVE      WI09          W001A             1
041400971021     C     *IN30         IFEQ      *OFF
041500971021     C     W001A         ORLT      '0'
041600971021     C                   SETON                                        32
041700971021     C                   MOVE      *ZERO         VABPKB
041800010604     C                   eval      vinmsg = %trimr(vinmsg)
041900010604     C                             + ' ' + 'VHBPKB'
042000971021     C                   ELSE
042100971021     C                   MOVEL     WI09          VABPKB
042200971023     C                   TESTN                   WD09                 30
042300971023     C  N30              TESTN                   WD09                     30
042400971021     C                   MOVE      WD09          W001A             1
042500971021     C     *IN30         IFEQ      *OFF
042600971021     C     W001A         ORLT      '0'
042700971023     C     W001A         ANDNE     *BLANK
042800971021     C                   SETON                                        32
042900971021     C                   MOVE      *ZERO         VABPKB
043000010604     C                   eval      vinmsg = %trimr(vinmsg)
043100010604     C                             + ' ' + 'VHBPKB'
043200971021     C                   ELSE
043300971021     C                   MOVE      WD09          VABPKB
043400971021     C                   ENDIF
043500050118     C                   ENDIF
043600971021     C*
043700041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
043800041001     C                   IF        *IN51 = *OFF
043900971021     C                   MOVE      *ZERO         VABVLB
044000971021     C                   MOVE      VHBVLB        WASC07
044100050503     C                   IF        WI07 = *blanks
044200050503     C                   EVAL      WI07 = *zeros
044300050503     C                   ENDIF
044400971023     C                   TESTN                   WI07                 30
044500971023     C  N30              TESTN                   WI07                   30
044600971023     C  N30              TESTN                   WI07                     30
044700971021     C                   MOVE      WI07          W001A             1
044800971021     C     *IN30         IFEQ      *OFF
044900971021     C     W001A         ORLT      '0'
045000971023     C     W001A         ANDNE     *BLANK
045100971021     C                   SETON                                        32
045200971021     C                   MOVE      *ZERO         VABVLB
045300010604     C                   eval      vinmsg = %trimr(vinmsg)
045400010604     C                             + ' ' + 'VHBVLB'
045500971021     C                   ELSE
045600971021     C                   MOVEL     WI07          VABVLB
045700971023     C                   TESTN                   WD07                 30
045800971023     C  N30              TESTN                   WD07                   30
045900971023     C  N30              TESTN                   WD07                     30
046000971021     C                   MOVE      WD07          W001A             1
046100971021     C     *IN30         IFEQ      *OFF
046200971021     C     W001A         ORLT      '0'
046300971023     C     W001A         ANDNE     *BLANK
046400971021     C                   SETON                                        32
046500971021     C                   MOVE      *ZERO         VABVLB
046600010604     C                   eval      vinmsg = %trimr(vinmsg)
046700010604     C                             + ' ' + 'VHBVLB'
046800971021     C                   ELSE
046900971021     C                   MOVE      WD07          VABVLB
047000971021     C                   ENDIF
047100050118     C                   ENDIF
047200041001     C                   ELSE
047300041001     C* ...altrimenti forzo azzeramento del volume
047400041001     C                   Z-ADD     *zeros        VABVLB
047500041001     C                   ENDIF
047600941229     C*
047700971021     C                   MOVE      *ZERO         VABQFT
047800971021     C                   MOVE      VHBQFT        WASC15
047900971023     C                   TESTN                   WI15                 30
048000971023     C  N30              TESTN                   WI15                   30
048100971023     C  N30              TESTN                   WI15                     30
048200971021     C                   MOVE      WI15          W001A             1
048300971021     C     *IN30         IFEQ      *OFF
048400971021     C     W001A         ORLT      '0'
048500971023     C     W001A         ANDNE     *BLANK
048600971021     C                   SETON                                        32
048700971021     C                   MOVE      *ZERO         VABQFT
048800010604     C                   eval      vinmsg = %trimr(vinmsg)
048900010604     C                             + ' ' + 'VHBQFT'
049000971021     C                   ELSE
049100971021     C                   MOVEL     WI15          VABQFT
049200971023     C                   TESTN                   WD15                 30
049300971023     C  N30              TESTN                   WD15                   30
049400971023     C  N30              TESTN                   WD15                     30
049500971021     C                   MOVE      WD15          W001A             1
049600971021     C     *IN30         IFEQ      *OFF
049700971021     C     W001A         ORLT      '0'
049800971023     C     W001A         ANDNE     *BLANK
049900971021     C                   SETON                                        32
050000971021     C                   MOVE      *ZERO         VABQFT
050100010604     C                   eval      vinmsg = %trimr(vinmsg)
050200010604     C                             + ' ' + 'VHBQFT'
050300971021     C                   ELSE
050400971021     C                   MOVE      WD15          VABQFT
050500971021     C                   ENDIF
050600050118     C                   ENDIF
050700941229     C*
050800971021     C                   MOVE      *ZERO         VABCAS
050900971021     C                   MOVE      VHBCAS        WASC15
051000971023     C                   TESTN                   WI15                 30
051100971023     C  N30              TESTN                   WI15                   30
051200971023     C  N30              TESTN                   WI15                     30
051300971021     C                   MOVE      WI15          W001A             1
051400971021     C     *IN30         IFEQ      *OFF
051500971021     C     W001A         ORLT      '0'
051600971023     C     W001A         ANDNE     *BLANK
051700971021     C                   SETON                                        32
051800971021     C                   MOVE      *ZERO         VABCAS
051900010604     C                   eval      vinmsg = %trimr(vinmsg)
052000010604     C                             + ' ' + 'VHBCAS'
052100971021     C                   ELSE
052200971021     C                   MOVEL     WI15          VABCAS
052300971023     C                   TESTN                   WD15                 30
052400971023     C  N30              TESTN                   WD15                   30
052500971023     C  N30              TESTN                   WD15                     30
052600971021     C                   MOVE      WD15          W001A             1
052700971021     C     *IN30         IFEQ      *OFF
052800971021     C     W001A         ORLT      '0'
052900971023     C     W001A         ANDNE     *BLANK
053000971021     C                   SETON                                        32
053100971021     C                   MOVE      *ZERO         VABCAS
053200010604     C                   eval      vinmsg = %trimr(vinmsg)
053300010604     C                             + ' ' + 'VHBCAS'
053400971021     C                   ELSE
053500971021     C                   MOVE      WD15          VABCAS
053600971021     C                   ENDIF
053700040827     C                   ENDIF
053800971021     C*
053900971023     C                   TESTN                   VHBRMN               30
054000971023     C  N30              TESTN                   VHBRMN                 30
054100971023     C  N30              TESTN                   VHBRMN                   30
054200971021     C                   MOVE      VHBRMN        W001A             1
054300971021     C     *IN30         IFEQ      *OFF
054400971021     C     W001A         ORLT      '0'
054500971021     C                   SETON                                        32
054600971021     C                   MOVE      *ZERO         VABRMN
054700010604     C                   eval      vinmsg = %trimr(vinmsg)
054800010604     C                             + ' ' + 'VHBRMN'
054900971021     C                   ELSE
055000971021     C                   MOVE      VHBRMN        VABRMN
055100971021     C                   ENDIF
055200971021     C*
055300971023     C                   TESTN                   VHBNCD               30
055400971023     C  N30              TESTN                   VHBNCD                 30
055500971023     C  N30              TESTN                   VHBNCD                   30
055600971021     C                   MOVE      VHBNCD        W001A             1
055700971021     C     *IN30         IFEQ      *OFF
055800971021     C     W001A         ORLT      '0'
055900971023     C     W001A         ANDNE     *BLANK
056000971021     C                   SETON                                        32
056100971021     C                   MOVE      *ZERO         VABNCD
056200010604     C                   eval      vinmsg = %trimr(vinmsg)
056300010604     C                             + ' ' + 'VHBNCD'
056400971021     C                   ELSE
056500971021     C                   MOVE      VHBNCD        VABNCD
056600971021     C                   ENDIF
056700971021     C*
056800971023     C                   TESTN                   VHBNCA               30
056900971023     C  N30              TESTN                   VHBNCA                 30
057000971023     C  N30              TESTN                   VHBNCA                   30
057100971021     C                   MOVE      VHBNCA        W001A             1
057200971021     C     *IN30         IFEQ      *OFF
057300971021     C     W001A         ORLT      '0'
057400971023     C     W001A         ANDNE     *BLANK
057500971021     C                   SETON                                        32
057600971021     C                   MOVE      *ZERO         VABNCA
057700010604     C                   eval      vinmsg = %trimr(vinmsg)
057800010604     C                             + ' ' + 'VHBNCA'
057900971021     C                   ELSE
058000971021     C                   MOVE      VHBNCA        VABNCA
058100971021     C                   ENDIF
058200971021     C*
058300971023     C                   TESTN                   VHBZNC               30
058400971023     C  N30              TESTN                   VHBZNC                 30
058500971023     C  N30              TESTN                   VHBZNC                   30
058600971021     C                   MOVE      VHBZNC        W001A             1
058700971021     C     *IN30         IFEQ      *OFF
058800971021     C     W001A         ORLT      '0'
058900971023     C     W001A         ANDNE     *BLANK
059000971021     C                   SETON                                        32
059100971021     C                   MOVE      *ZERO         VABZNC
059200010604     C                   eval      vinmsg = %trimr(vinmsg)
059300010604     C                             + ' ' + 'VHBZNC'
059400971021     C                   ELSE
059500971021     C                   MOVE      VHBZNC        VABZNC
059600971021     C                   ENDIF
059700971021     C*
059800971023     C                   TESTN                   VHBDCR               30
059900971023     C  N30              TESTN                   VHBDCR                 30
060000971023     C  N30              TESTN                   VHBDCR                   30
060100971021     C                   MOVE      VHBDCR        W001A             1
060200971021     C     *IN30         IFEQ      *OFF
060300971021     C     W001A         ORLT      '0'
060400971023     C     W001A         ANDNE     *BLANK
060500971021     C                   SETON                                        32
060600971021     C                   MOVE      *ZERO         VABDCR
060700010604     C                   eval      vinmsg = %trimr(vinmsg)
060800010604     C                             + ' ' + 'VHBDCR'
060900971021     C                   ELSE
061000971021     C                   MOVE      VHBDCR        VABDCR
061100971021     C                   ENDIF
061200971021     C*
061300971023     C                   TESTN                   VHBHCR               30
061400971023     C  N30              TESTN                   VHBHCR                 30
061500971023     C  N30              TESTN                   VHBHCR                   30
061600971021     C                   MOVE      VHBHCR        W001A             1
061700971021     C     *IN30         IFEQ      *OFF
061800971021     C     W001A         ORLT      '0'
061900971023     C     W001A         ANDNE     *BLANK
062000971021     C                   SETON                                        32
062100971021     C                   MOVE      *ZERO         VABHCR
062200010604     C                   eval      vinmsg = %trimr(vinmsg)
062300010604     C                             + ' ' + 'VHBHCR'
062400971021     C                   ELSE
062500971021     C                   MOVE      VHBHCR        VABHCR
062600971021     C                   ENDIF
062700941229     C*
062800971021     C                   MOVE      *ZERO         VABVMD
062900971021     C                   MOVE      VHBVMD        WASC15
063000971023     C                   TESTN                   WI15                 30
063100971023     C  N30              TESTN                   WI15                   30
063200971023     C  N30              TESTN                   WI15                     30
063300971021     C                   MOVE      WI15          W001A             1
063400971021     C     *IN30         IFEQ      *OFF
063500971021     C     W001A         ORLT      '0'
063600971023     C     W001A         ANDNE     *BLANK
063700971021     C                   SETON                                        32
063800971021     C                   MOVE      *ZERO         VABVMD
063900010604     C                   eval      vinmsg = %trimr(vinmsg)
064000010604     C                             + ' ' + 'VHBVMD'
064100971021     C                   ELSE
064200971021     C                   MOVEL     WI15          VABVMD
064300971023     C                   TESTN                   WD15                 30
064400971023     C  N30              TESTN                   WD15                   30
064500971023     C  N30              TESTN                   WD15                     30
064600971021     C                   MOVE      WD15          W001A             1
064700971021     C     *IN30         IFEQ      *OFF
064800971021     C     W001A         ORLT      '0'
064900971023     C     W001A         ANDNE     *BLANK
065000971021     C                   SETON                                        32
065100971021     C                   MOVE      *ZERO         VABVMD
065200010604     C                   eval      vinmsg = %trimr(vinmsg)
065300010604     C                             + ' ' + 'VHBVMD'
065400971021     C                   ELSE
065500971021     C                   MOVE      WD15          VABVMD
065600971021     C                   ENDIF
065700050118     C                   ENDIF
065800971021     C*
065900971023     C                   TESTN                   VHBANT               30
066000971023     C  N30              TESTN                   VHBANT                 30
066100971023     C  N30              TESTN                   VHBANT                   30
066200971021     C                   MOVE      VHBANT        W001A             1
066300971021     C     *IN30         IFEQ      *OFF
066400971021     C     W001A         ORLT      '0'
066500971023     C     W001A         ANDNE     *BLANK
066600971021     C                   SETON                                        32
066700971021     C                   MOVE      *ZERO         VABANT
066800010604     C                   eval      vinmsg = %trimr(vinmsg)
066900010604     C                             + ' ' + 'VHBANT'
067000971021     C                   ELSE
067100971021     C                   MOVE      VHBANT        VABANT
067200971021     C                   ENDIF
067300971021     C*
067400971021     C* CAMPI ALFABETICI:
067500971021     C                   MOVE      VHBATB        VABATB
067600971021     C                   MOVE      VHBCBO        VABCBO
067700971021     C                   MOVE      VHBRSD        VABRSD
067800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
067900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
068000020117     C* ==
068100971021     C                   MOVE      VHBRD2        VABRD2
068200971021     C                   MOVE      VHBIND        VABIND
068300971021     C                   MOVE      VHBCAD        VABCAD
068400971021     C                   MOVE      VHBLOD        VABLOD
068500971021     C                   MOVE      VHBPRD        VABPRD
068600971021     C                   MOVE      VHBNZD        VABNZD
068700971021     C                   MOVE      VHBGC1        VABGC1
068800971021     C                   MOVE      VHBGC2        VABGC2
068900971021     C                   MOVE      VHBTSP        VABTSP
069000971021     C                   MOVE      VHBVAS        VABVAS
069100971021     C                   MOVE      VHBNAS        VABNAS
069200971021     C                   MOVE      VHBTIC        VABTIC
069300971021     C                   MOVE      VHBVCA        VABVCA
069400971021     C                   MOVE      VHBGCA        VABGCA
069500971021     C                   MOVE      VHBRMA        VABRMA
069600971021     C                   MOVE      VHBXCO        VABXCO
069700971021     C                   MOVE      VHBNOT        VABNOT
069800971021     C                   MOVE      VHBNT2        VABNT2
069900971021     C                   MOVE      VHBCTM        VABCTM
070000971021     C                   MOVE      VHBFFD        VABFFD
070100971021     C                   MOVE      VHBTCR        VABTCR
070200971021     C                   MOVE      VHBCTS        VABCTS
070300971021     C                   MOVE      VHBFTM        VABFTM
070400971021     C                   MOVE      VHBVAD        VABVAD
070500971021     C                   MOVE      VHBGMA        VABGMA
070600971021     C                   MOVE      VHBGGA        VABGGA
070700971021     C                   MOVE      VHBGVA        VABGVA
070800971021     C                   MOVE      VHBTC1        VABTC1
070900971021     C                   MOVE      VHBTC2        VABTC2
071000971021     C                   MOVE      VHBSCL        VABSCL
071100971021     C                   MOVE      VHBRMO        VABRMO
071200971021     C                   MOVE      VHBCMO        VABCMO
071300971021     C                   MOVE      VHBNMO        VABNMO
071400971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
071500060329     C* NELLA NAZIONE MITTENTE ORIGINALE
071600060331     C                   IF        %trim(VHBNMO) < 'A'
071700060329     C                   MOVE      *BLANKS       VABNMO
071800060329     C                   ENDIF
071900050418     C*
072000050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
072100050418     C                   IF        %trim(VABNZD) = 'I'   OR
072200050418     C                             %trim(VABNZD) = 'IT'  OR
072300050418     C                             %trim(VABNZD) = 'ITA'
072400050418     C                   EVAL      VABNZD = *blanks
072500050418     C                   ENDIF
072600041025     C*
072700041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
072800041025     C                   IF        *IN52 = *ON
072900041025     C* Reperisco la provincia dal CAP e dalla località
073000041025     C                   IF        VABCAD <> *blanks AND
073100050907     C                             VABPRD  = *blanks AND
073200050907     C                             VABNZD  = *blanks
073300041025     C                   CLEAR                   TISI95DS
073400041025     C                   EVAL      I95TCN = '3'
073500041025     C                   Z-ADD     datcor        I95DAT
073600041025     C                   EVAL      I95CAP = VABCAD
073700041025     C                   EVAL      I95LOC = VABLOD
073800041025     C                   CALL      'TISI95R'
073900041025     C                   PARM                    TISI95DS
074000041025     C                   EVAL      VABPRD = O95PRV
074100041025     C                   ENDIF
074200041025     C                   ENDIF
074300040630     C*
074400040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
074500040630     C                   exsr      exeBon
074600100211     C*
074700100211     C* Forzatura su campo indirizzo => max 1 blank consecutivo
074800100211     C                   dow       %scan('  ':vabind) > 0 AND
074900100211     C                             %scan('  ':vabind) < %len(%trim(vabind))
075000100211     C                   eval      vabind = %replace(' ':vabind:
075100100211     C                                      %scan('  ':vabind):2)
075200100211     C                   enddo
075300930409     C*
075400010604     C  N31              ADD       1             §CTROK            7 0
075500010604     C   32              ADD       1             §CTRMO            7 0
075600010604     C   31              ADD       1             §CTRNO            7 0
075700020916     C  N31              WRITE     FIVAB000
075800990910     C*
075900010601     C                   if        *in31 = *off and
076000010601     C                             *in32 = *off
076100990910     C                   eval      vinflg = '1'
076200990910     C                   else
076300990910     C                   eval      vinflg = '2'
076400990910     C                   endif
076500000905     C                   endif
076600000905     C                   endif
076700010601     C*
076800010601     C                   endif
076900000905     C*
077000000905     C                   else
077100000905     C                   eval      vinflg = '1'
077200000905     C                   endif
077300000905     C*
077400000905     C  N70              update    tivin000
077500000905     C*
077600991022     C  N70              ENDdo
077700010601     C
077800010601     C*
077900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
078000010601     C                   if        cntNonEl = *zeros or
078100010601     C                             flgMulti = '0'
078200990910     C* Se non ci sono record con errori ...
078300010601     C                   if        §ctrno = 0 and
078400010601     C                             §ctrmo = 0 and
078500010601     C                             flgStato <> '2'
078600990910     C* ... restituisco esito OK.
078700990921     C                   eval      wrkesito = '0'
078800990910     C                   else
078900010601     C                   if        §ctrok > 0
079000000710     C                   eval      wrkesito = '1'
079100000710     C                   else
079200010615     C                   if        flgOk = '0'
079300010615     C                   eval      wrkesito = '2'
079400010615     C                   else
079500010615     C                   eval      wrkesito = '6'
079600010615     C                   endif
079700000710     C                   endif
079800010615     C                   endif
079900010601     C                   else
080000010601     C                   eval      wrkesito = '9'
080100010601     C                   endif
080200990910     C*
080300990914     C                   if        %open(tivin00r)
080400990908     C                   close     tivin00r
080500990914     C                   endif
080600020916     C                   if        %open(fivabwwr)
080700020916     C                   close     fivabwwr
080800990914     C                   endif
080900010601     C*
081000010601     C                   if        vlrpoi <> 999
081100010601     C                   eval      invfil = vlrpoi
081200010601     C                   endif
081300990910     C*
081400990920     C                   if        §ctrok > 0
081500010601     C                             and invfil > *zeros
081600990920     C                   exsr      invio
081700990920     C                   endif
081800010612     C*
081900010612     C                   if        flgGiro = '1'
082000010612     C                   exsr      endela
082100010612     C                   endif
082200990920     C*
082300910830     C                   ENDSR
082400000613     C***
082500990920
082600010601
082700010601
082800010601
082900010601
083000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
083100010601     C     repfil        BEGSR
083200010601     C*
083300010601     C                   if        invfil = *zeros and
083400010601     C                             depfil > *zeros and
083500010613     C                             (vinflg = *blanks or
083600010613     C                              vinflg = *zeros)
083700010601     C
083800010601     C                   eval      invfil = depfil
083900010601     C                   endif
084000010601     C*
084100010601     C                   if        depfil <> invfil and
084200010601     C                             invfil > *zeros
084300010601     C                   eval      flgMulti = '1'
084400010601     C                   if        vinflg = *blanks
084500010601     C                   add       1             cntNonEl
084600010601     C                   endif
084700010601     C                   endif
084800010601     C*
084900010601     C                   if        vinflg = '2'
085000010601     C                   eval      flgStato = '2'
085100010601     C                   endif
085200010601     C*
085300010601     C                   ENDSR
085400010601     C***
085500010601
085600010601
085700010601
085800010601
085900010601
086000010601
086100010601
086200990920      /TITLE Invio dei dati al punto operativo.
086300000613     C     invio         BEGSR
086400990920     C*
086500990920     C                   reset                   dscmz
086600010601     C                   move      invfil        cmzdst
086700990920     C                   eval      cmzfld = vlrfou
086800990920     C                   eval      cmzmbd = vlrhdl
086900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
087000000710     C                   if        prmfir = *blanks
087100020916     C                   eval      cmzfla = 'FIVAB00F'
087200020916     C                   eval      cmzmba = 'FIVAB00F'
087300000710     C                   else
087400000710     C                   eval      cmzfla = prmfir
087500000710     C                   eval      cmzmba = prmfir
087600000710     C                   endif
087700990920     C                   eval      cmznrr = *zeros
087800990920     C                   move      §ctrok        cmznrr
087900021003     C                   eval      cmzlba = vlrfl1
088000990920     C                   call(e)   'TIS711C'
088100990920     C                   parm                    dscmz
088200990921     C                   parm      *blanks       esito
088300990920     C                   if        %error
088400990920     C                             or cmzerr = '1'
088500990921     C                             or esito  = '1'
088600000710     C                   eval      wrkesito = '3'
088700990920     C                   endif
088800990920     C*
088900000613     C                   ENDSR
089000010601
089100010601
089200010601
089300010601
089400010601      /TITLE Invio dei dati al punto operativo.
089500010601     C     opeini        BEGSR
089600010601     C*
089700010601     C* Inizializzo flag e contatori operativi
089800010601     C                   movel     '0'           flgGiro           1
089900010601     C                   movel     '0'           flgMulti          1
090000010601     C                   movel     '1'           flgStato          1
090100010615     C                   movel     '0'           flgOk             1
090200010601     C                   z-add     *zeros        cntNonEl         10 0
090300010601     C                   z-add     *zeros        depfil            3 0
090400010601     C                   z-add     *zeros        invfil            3 0
090500030407     C*
090600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
090700030407     C                   if        %subst(vlrppt:1:1) = 'S'
090800030407     C                   seton                                        50
090900030407     C                   else
091000030407     C                   setoff                                       50
091100030407     C                   endif
091200041001     C*
091300041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
091400041001     C                   if        %subst(vlrppt:2:1) = 'S'
091500041001     C                   seton                                        51
091600041001     C                   else
091700041001     C                   setoff                                       51
091800041001     C                   endif
091900041025     C*
092000041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
092100041025     C* (naturalmente solo se nn già presente)
092200041025     C                   if        %subst(vlrppt:3:1) = 'S'
092300041025     C                   seton                                        52
092400041025     C                   else
092500041025     C                   setoff                                       52
092600041025     C                   endif
092700010601     C*
092800010601     C                   ENDSR
092900010601     C***
093000040630
093100040630
093200040630
093300040630
093400040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
093500040630     C     exeBon        BEGSR
093600040630     C*
093700040630     C* Imposto i parametri x il *pgm d "bonifica"
093800040630     C                   EVAL      ElencoChar = *blanks
093900040630     C                   EVAL      TipoElenco = *blanks
094000040630     C                   EVAL      CharSost   = *blanks
094100040630     C                   EVAL      UpperCase  = '1'
094200040630     C                   EVAL      ChkNull    = '1'
094300040630     C                   EVAL      CharNull   = *blanks
094400040630     C*
094500040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
094600040630     C                   EVAL      TxtInOut   = VABRSD
094700040630     C                   EVAL      EsitoBon   = *blanks
094800040630     C*
094900040630     C                   CALL      'XCHKCHAR'
095000040630     C                   PARM                    TxtInOut
095100040630     C                   PARM                    ElencoChar
095200040630     C                   PARM                    TipoElenco
095300040630     C                   PARM                    CharSost
095400040630     C                   PARM                    UpperCase
095500040630     C                   PARM                    ChkNull
095600040630     C                   PARM                    CharNull
095700040630     C                   PARM                    EsitoBon
095800040630     C*
095900040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
096000040630     C                   IF        EsitoBon = '1'
096100040630     C                   EVAL      VABRSD = TxtInOut
096200040630     C                   ENDIF
096300040630     C*
096400040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
096500040630     C                   EVAL      TxtInOut   = VABIND
096600040630     C                   EVAL      EsitoBon   = *blanks
096700040630     C*
096800040630     C                   CALL      'XCHKCHAR'
096900040630     C                   PARM                    TxtInOut
097000040630     C                   PARM                    ElencoChar
097100040630     C                   PARM                    TipoElenco
097200040630     C                   PARM                    CharSost
097300040630     C                   PARM                    UpperCase
097400040630     C                   PARM                    ChkNull
097500040630     C                   PARM                    CharNull
097600040630     C                   PARM                    EsitoBon
097700040630     C*
097800040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
097900040630     C                   IF        EsitoBon = '1'
098000040630     C                   EVAL      VABIND = TxtInOut
098100040630     C                   ENDIF
098200040630     C*
098300040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
098400040630     C                   EVAL      TxtInOut   = VABLOD
098500040630     C                   EVAL      EsitoBon   = *blanks
098600040630     C*
098700040630     C                   CALL      'XCHKCHAR'
098800040630     C                   PARM                    TxtInOut
098900040630     C                   PARM                    ElencoChar
099000040630     C                   PARM                    TipoElenco
099100040630     C                   PARM                    CharSost
099200040630     C                   PARM                    UpperCase
099300040630     C                   PARM                    ChkNull
099400040630     C                   PARM                    CharNull
099500040630     C                   PARM                    EsitoBon
099600040630     C*
099700040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
099800040630     C                   IF        EsitoBon = '1'
099900040630     C                   EVAL      VABLOD = TxtInOut
100000040630     C                   ENDIF
100100040630     C*
100200040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
100300040630     C                   EVAL      TxtInOut   = VABNOT
100400040630     C                   EVAL      EsitoBon   = *blanks
100500040630     C                   EVAL      UpperCase  = *blanks
100600040630     C*
100700040630     C                   CALL      'XCHKCHAR'
100800040630     C                   PARM                    TxtInOut
100900040630     C                   PARM                    ElencoChar
101000040630     C                   PARM                    TipoElenco
101100040630     C                   PARM                    CharSost
101200040630     C                   PARM                    UpperCase
101300040630     C                   PARM                    ChkNull
101400040630     C                   PARM                    CharNull
101500040630     C                   PARM                    EsitoBon
101600040630     C*
101700040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
101800040630     C                   IF        EsitoBon = '1'
101900040630     C                   EVAL      VABNOT = TxtInOut
102000040630     C                   ENDIF
102100040630     C*
102200040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
102300040630     C                   EVAL      TxtInOut   = VABNT2
102400040630     C                   EVAL      EsitoBon   = *blanks
102500040630     C                   EVAL      UpperCase  = *blanks
102600040630     C*
102700040630     C                   CALL      'XCHKCHAR'
102800040630     C                   PARM                    TxtInOut
102900040630     C                   PARM                    ElencoChar
103000040630     C                   PARM                    TipoElenco
103100040630     C                   PARM                    CharSost
103200040630     C                   PARM                    UpperCase
103300040630     C                   PARM                    ChkNull
103400040630     C                   PARM                    CharNull
103500040630     C                   PARM                    EsitoBon
103600040630     C*
103700040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
103800040630     C                   IF        EsitoBon = '1'
103900040630     C                   EVAL      VABNT2 = TxtInOut
104000040630     C                   ENDIF
104100040630     C*
104200040630     C                   ENDSR
104300040630
104400010601
104500010601
104600990910
104700000613     C     *inzsr        BEGSR
104800990910     C*
104900990910     C     *entry        plist
105000990920     C                   parm                    tivlrds
105100990921     C                   parm      wrkesito      esito
105200000724     C                   parm                    prmlit
105300000710     C                   parm                    prmfir
105400010606     C*
105500010606     C* CALCOLA LA DATA CORRENTE
105600010606     C                   time                    wn14             14 0
105700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
105800010606     C                   z-add     wn8           g08dat
105900010606     C                   z-add     *zeros        g08inv
106000010606     C                   movel     '0'           g08err
106100010606     C                   call      'XSRDA8'
106200010606     C                   parm                    wlbda8
106300010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
106400000613     C*
106500000613     C                   ENDSR
106600000613     C***
