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
010000090511
010100090511
010200010606     D*------------------
010300010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
010400010606     D*------------------
010500010606     D WLBDA8          DS                  INZ
010600010606     D  G08DAT                 1      8  0
010700010606     D  G08INV                 9     16  0
010800010606     D  G08ERR                17     17
010900010606     D  G08TGI                18     22  0
011000010606     D*
011100040407     D TxtInOut        S           2048
011200040407     D ElencoChar      S            256
011300040407     D TipoElenco      S              1
011400040407     D CharSost        S              1
011500040407     D UpperCase       S              1
011600040407     D ChkNull         S              1
011700040407     D CharNull        S              1
011800040407     D EsitoBon        S              1
011900010606
012000010606
012100010606
012200010606
012300000913     C                   reset                   rrnum
012400990921     C                   reset                   esito
012500990921     C                   reset                   wrkesito
012600090511     C*
012700010601     C*
012800010601     C                   exsr      opeini
012900010601     C                   exsr      rwvab
013000041025     C*
013100041025     C* Esegue lancio TISI95R solo x chiusura
013200041025     C                   CLEAR                   TISI95DS
013300041025     C                   EVAL      I95TLA = 'C'
013400041025     C                   CALL      'TISI95R'
013500041025     C                   PARM                    TISI95DS
013600010601     C*
013700010601     C                   seton                                        lr
013800010601
013900010601
014000010601
014100010601
014200010601     C*--------------------------------------------------------
014300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
014400010601     C*--------------------------------------------------------
014500010601     C     PREELA        BEGSR
014600010601     C*
014700010601     C* SE OCCORRE SPEDIRE IN FILIALE
014800010601     C                   if        invfil <> *zeros and
014900010601     C                             flgGiro = '0'
015000010601     C*
015100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
015200010601     C                   eval      flgGiro = '1'
015300010601     C*
015400010601     C                   endif
015500010601     C*
015600010601     C                   ENDSR
015700010601     C***
015800010601
015900010601
016000010601
016100010601
016200010601
016300010601
016400010601     C*--------------------------------------------------------
016500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016600010601     C*--------------------------------------------------------
016700010601     C     ENDELA        BEGSR
016800010601     C*
016900010601     C                   ENDSR
017000010601     C***
017100000613
017200010601
017300010601
017400010601
017500010601
017600910830     C*--------------------------------------------------------
017700020916     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
017800910830     C*--------------------------------------------------------
017900941229     C     RWVAB         BEGSR
018000990910     C*
018100990914     C                   if        not %open(tivin00r)
018200990908     C                   open      tivin00r
018300990914     C                   endif
018400020916     C                   if        not %open(fivabwwr)
018500020916     C                   open      fivabwwr
018600990914     C                   endif
018700990910     C*
018800990910     C                   clear                   §CTROK
018900990910     C                   clear                   §CTRMO
019000990910     C                   clear                   §CTRNO
019100990910     C*
019200921023     C                   DO        *HIVAL
019300990913     C*
019400990915     C                   READ      tivin00r                               70
019500010618     C*
019600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019700010618     C                   if        vinflg = '1'
019800010618     C                   eval      flgOk = '1'
019900010618     C                   endif
020000010618     C*
020100051017     C                   if        vindta  > *blanks
020200000613     C                   add       1             rrnum
020300990913     C*
020400010601     C                   if        *in70 = *off and
020500990913     C                             (vinflg = *blanks
020600010711     C                              or vinflg = '0'
020700010711     C                              or vinflg = '2')
020800990913     C*
020900020916     C                   clear                   FIVAB000
021000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021100010711     C                   if        vinflg = *blanks or vinflg = '0'
021200010711     C                   clear                   vinmsg
021300010711     C                   endif
021400010711     C*
021500000313     C                   eval      x_rec  = vindta
021600991022     C*
021700991022     C                   testb     x'3F'         vhbatb                   02
021800991022     C                   if        not *in02
021900991022     C*
022000971021     C                   SETOFF                                       303132
022100971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
022200971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
022300971023     C*    OPPURE TUTTI BLANKS
022400010601     C*
022500010601     C                   TESTN                   VHBLNP               30
022600010601     C  N30              TESTN                   VHBLNP                 30
022700010601     C  N30              TESTN                   VHBLNP                   30
022800010601     C                   MOVE      VHBLNP        W001A             1
022900010601     C     *IN30         IFEQ      *OFF
023000010601     C     W001A         ORLT      '0'
023100010601     C                   SETON                                        31
023200010601     C                   eval      vinmsg = %trimr(vinmsg)
023300010601     C                             + ' ' + 'VHBLNP'
023400010601     C                   ELSE
023500010601     C                   MOVE      VHBLNP        VABLNP
023600010601     C     VABLNP        IFLE      *ZEROS
023700010601     C                   SETON                                        31
023800010601     C                   eval      vinmsg = %trimr(vinmsg)
023900010601     C                             + ' ' + 'VHBLNP'
024000010601     C                   ENDIF
024100010601     C                   ENDIF
024200010601     C*
024300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024400010601     C                   eval      depfil = VABLNP
024500010601     C                   exsr      repfil
024600010601     C                   if        depfil = invfil
024700021014     C                   if        vlrpoi = 999
024800021010     C                   MOVE(P)   invfil        VABFGS
024900021014     C                   else
025000021014     C                   MOVE(P)   vlrpoi        VABFGS
025100021014     C                   endif
025200010601     C*
025300010601     C                   exsr      PREELA
025400010601     C*
025500971023     C                   TESTN                   VHBCCM               30
025600971023     C  N30              TESTN                   VHBCCM                 30
025700971023     C  N30              TESTN                   VHBCCM                   30
025800971021     C                   MOVE      VHBCCM        W001A             1
025900971021     C     *IN30         IFEQ      *OFF
026000971021     C     W001A         ORLT      '0'
026100971021     C                   SETON                                        31
026200010604     C                   eval      vinmsg = %trimr(vinmsg)
026300010604     C                             + ' ' + 'VHBCCM'
026400971021     C                   ELSE
026500971021     C                   MOVE      VHBCCM        VABCCM
026600971024     C     VABCCM        IFLE      *ZEROS
026700971024     C                   SETON                                        31
026800010604     C                   eval      vinmsg = %trimr(vinmsg)
026900010604     C                             + ' ' + 'VHBCCM'
027000971024     C                   ENDIF
027100971021     C                   ENDIF
027200971021     C*
027300971023     C                   TESTN                   VHBAAS               30
027400971023     C  N30              TESTN                   VHBAAS                 30
027500971023     C  N30              TESTN                   VHBAAS                   30
027600971021     C                   MOVE      VHBAAS        W001A             1
027700971021     C     *IN30         IFEQ      *OFF
027800971021     C     W001A         ORLT      '0'
027900971023     C     W001A         ANDNE     *BLANK
028000971021     C                   SETON                                        32
028100010606     C                   MOVEL     datcor        VABAAS
028200010604     C                   eval      vinmsg = %trimr(vinmsg)
028300010604     C                             + ' ' + 'VHBAAS'
028400971021     C                   ELSE
028500971021     C                   MOVE      VHBAAS        VABAAS
028600971021     C                   ENDIF
028700971021     C*
028800971023     C                   TESTN                   VHBMGS               30
028900971023     C  N30              TESTN                   VHBMGS                 30
029000971023     C  N30              TESTN                   VHBMGS                   30
029100971021     C                   MOVE      VHBMGS        W001A             1
029200971021     C     *IN30         IFEQ      *OFF
029300971021     C     W001A         ORLT      '0'
029400971023     C     W001A         ANDNE     *BLANK
029500971021     C                   SETON                                        32
029600010601     C                   MOVE      '0101'        VABMGS
029700010604     C                   eval      vinmsg = %trimr(vinmsg)
029800010604     C                             + ' ' + 'VHBMGS'
029900971021     C                   ELSE
030000971021     C                   MOVE      VHBMGS        VABMGS
030100971021     C                   ENDIF
030200971021     C*
030300971023     C                   TESTN                   VHBNRS               30
030400971023     C  N30              TESTN                   VHBNRS                 30
030500971023     C  N30              TESTN                   VHBNRS                   30
030600971021     C                   MOVE      VHBNRS        W001A             1
030700971021     C     *IN30         IFEQ      *OFF
030800971021     C     W001A         ORLT      '0'
030900971023     C     W001A         ANDNE     *BLANK
031000971021     C                   SETON                                        32
031100971021     C                   MOVE      *ZERO         VABNRS
031200010604     C                   eval      vinmsg = %trimr(vinmsg)
031300010604     C                             + ' ' + 'VHBNRS'
031400971021     C                   ELSE
031500971021     C                   MOVE      VHBNRS        VABNRS
031600971021     C                   ENDIF
031700971021     C*
031800971023     C                   TESTN                   VHBNSP               30
031900971023     C  N30              TESTN                   VHBNSP                 30
032000971023     C  N30              TESTN                   VHBNSP                   30
032100971021     C                   MOVE      VHBNSP        W001A             1
032200971021     C     *IN30         IFEQ      *OFF
032300971021     C     W001A         ORLT      '0'
032400971023     C     W001A         ANDNE     *BLANK
032500971021     C                   SETON                                        32
032600971021     C                   MOVE      *ZERO         VABNSP
032700010604     C                   eval      vinmsg = %trimr(vinmsg)
032800010604     C                             + ' ' + 'VHBNSP'
032900971021     C                   ELSE
033000971021     C                   MOVE      VHBNSP        VABNSP
033100971021     C                   ENDIF
033200971021     C*
033300971023     C                   TESTN                   VHBLNA               30
033400971023     C  N30              TESTN                   VHBLNA                 30
033500971023     C  N30              TESTN                   VHBLNA                   30
033600971021     C                   MOVE      VHBLNA        W001A             1
033700971021     C     *IN30         IFEQ      *OFF
033800971021     C     W001A         ORLT      '0'
033900971023     C     W001A         ANDNE     *BLANK
034000971021     C                   SETON                                        32
034100971021     C                   MOVE      *ZERO         VABLNA
034200010604     C                   eval      vinmsg = %trimr(vinmsg)
034300010604     C                             + ' ' + 'VHBLNA'
034400971021     C                   ELSE
034500971021     C                   MOVE      VHBLNA        VABLNA
034600971021     C                   ENDIF
034700971021     C*
034800971023     C                   TESTN                   VHBCTR               30
034900971023     C  N30              TESTN                   VHBCTR                 30
035000971023     C  N30              TESTN                   VHBCTR                   30
035100971021     C                   MOVE      VHBCTR        W001A             1
035200971021     C     *IN30         IFEQ      *OFF
035300971021     C     W001A         ORLT      '0'
035400971023     C     W001A         ANDNE     *BLANK
035500971021     C                   SETON                                        32
035600971021     C                   MOVE      *ZERO         VABCTR
035700010604     C                   eval      vinmsg = %trimr(vinmsg)
035800010604     C                             + ' ' + 'VHBCTR'
035900971021     C                   ELSE
036000971021     C                   MOVE      VHBCTR        VABCTR
036100971021     C                   ENDIF
036200941229     C*
036300971021     C                   MOVE      *ZERO         VABIAS
036400941229     C                   MOVE      VHBIAS        WASC15
036500971023     C                   TESTN                   WI15                 30
036600971023     C  N30              TESTN                   WI15                   30
036700971023     C  N30              TESTN                   WI15                     30
036800971021     C                   MOVE      WI15          W001A             1
036900971021     C     *IN30         IFEQ      *OFF
037000971021     C     W001A         ORLT      '0'
037100971023     C     W001A         ANDNE     *BLANK
037200971021     C                   SETON                                        32
037300971021     C                   MOVE      *ZERO         VABIAS
037400010604     C                   eval      vinmsg = %trimr(vinmsg)
037500010604     C                             + ' ' + 'VHBIAS'
037600971021     C                   ELSE
037700971021     C                   MOVEL     WI15          VABIAS
037800971023     C                   TESTN                   WD15                 30
037900971023     C  N30              TESTN                   WD15                   30
038000971023     C  N30              TESTN                   WD15                     30
038100971021     C                   MOVE      WD15          W001A             1
038200971021     C     *IN30         IFEQ      *OFF
038300971021     C     W001A         ORLT      '0'
038400971023     C     W001A         ANDNE     *BLANK
038500971021     C                   SETON                                        32
038600971021     C                   MOVE      *ZERO         VABIAS
038700010604     C                   eval      vinmsg = %trimr(vinmsg)
038800010604     C                             + ' ' + 'VHBIAS'
038900971021     C                   ELSE
039000971021     C                   MOVE      WD15          VABIAS
039100971021     C                   ENDIF
039200050118     C                   ENDIF
039300941229     C*
039400971023     C                   TESTN                   VHBNCL               30
039500971023     C  N30              TESTN                   VHBNCL                 30
039600971023     C  N30              TESTN                   VHBNCL                   30
039700971021     C                   MOVE      VHBNCL        W001A             1
039800971021     C     *IN30         IFEQ      *OFF
039900971021     C     W001A         ORLT      '0'
040000971021     C                   SETON                                        32
040100971021     C                   MOVE      *ZERO         VABNCL
040200010604     C                   eval      vinmsg = %trimr(vinmsg)
040300010604     C                             + ' ' + 'VHBNCL'
040400971021     C                   ELSE
040500971021     C                   MOVE      VHBNCL        VABNCL
040600971021     C                   ENDIF
040700941229     C*
040800971021     C                   MOVE      *ZERO         VABPKB
040900971021     C                   MOVE      VHBPKB        WASC09
041000050503     C                   IF        WI09 = *blanks
041100050503     C                   EVAL      WI09 = *zeros
041200050503     C                   ENDIF
041300971023     C                   TESTN                   WI09                 30
041400971023     C  N30              TESTN                   WI09                   30
041500971023     C  N30              TESTN                   WI09                     30
041600971021     C                   MOVE      WI09          W001A             1
041700971021     C     *IN30         IFEQ      *OFF
041800971021     C     W001A         ORLT      '0'
041900971021     C                   SETON                                        32
042000971021     C                   MOVE      *ZERO         VABPKB
042100010604     C                   eval      vinmsg = %trimr(vinmsg)
042200010604     C                             + ' ' + 'VHBPKB'
042300971021     C                   ELSE
042400971021     C                   MOVEL     WI09          VABPKB
042500971023     C                   TESTN                   WD09                 30
042600971023     C  N30              TESTN                   WD09                     30
042700971021     C                   MOVE      WD09          W001A             1
042800971021     C     *IN30         IFEQ      *OFF
042900971021     C     W001A         ORLT      '0'
043000971023     C     W001A         ANDNE     *BLANK
043100971021     C                   SETON                                        32
043200971021     C                   MOVE      *ZERO         VABPKB
043300010604     C                   eval      vinmsg = %trimr(vinmsg)
043400010604     C                             + ' ' + 'VHBPKB'
043500971021     C                   ELSE
043600971021     C                   MOVE      WD09          VABPKB
043700971021     C                   ENDIF
043800050118     C                   ENDIF
043900971021     C*
044000041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
044100041001     C                   IF        *IN51 = *OFF
044200971021     C                   MOVE      *ZERO         VABVLB
044300971021     C                   MOVE      VHBVLB        WASC07
044400050503     C                   IF        WI07 = *blanks
044500050503     C                   EVAL      WI07 = *zeros
044600050503     C                   ENDIF
044700971023     C                   TESTN                   WI07                 30
044800971023     C  N30              TESTN                   WI07                   30
044900971023     C  N30              TESTN                   WI07                     30
045000971021     C                   MOVE      WI07          W001A             1
045100971021     C     *IN30         IFEQ      *OFF
045200971021     C     W001A         ORLT      '0'
045300971023     C     W001A         ANDNE     *BLANK
045400971021     C                   SETON                                        32
045500971021     C                   MOVE      *ZERO         VABVLB
045600010604     C                   eval      vinmsg = %trimr(vinmsg)
045700010604     C                             + ' ' + 'VHBVLB'
045800971021     C                   ELSE
045900971021     C                   MOVEL     WI07          VABVLB
046000971023     C                   TESTN                   WD07                 30
046100971023     C  N30              TESTN                   WD07                   30
046200971023     C  N30              TESTN                   WD07                     30
046300971021     C                   MOVE      WD07          W001A             1
046400971021     C     *IN30         IFEQ      *OFF
046500971021     C     W001A         ORLT      '0'
046600971023     C     W001A         ANDNE     *BLANK
046700971021     C                   SETON                                        32
046800971021     C                   MOVE      *ZERO         VABVLB
046900010604     C                   eval      vinmsg = %trimr(vinmsg)
047000010604     C                             + ' ' + 'VHBVLB'
047100971021     C                   ELSE
047200971021     C                   MOVE      WD07          VABVLB
047300971021     C                   ENDIF
047400050118     C                   ENDIF
047500041001     C                   ELSE
047600041001     C* ...altrimenti forzo azzeramento del volume
047700041001     C                   Z-ADD     *zeros        VABVLB
047800041001     C                   ENDIF
047900941229     C*
048000971021     C                   MOVE      *ZERO         VABQFT
048100971021     C                   MOVE      VHBQFT        WASC15
048200971023     C                   TESTN                   WI15                 30
048300971023     C  N30              TESTN                   WI15                   30
048400971023     C  N30              TESTN                   WI15                     30
048500971021     C                   MOVE      WI15          W001A             1
048600971021     C     *IN30         IFEQ      *OFF
048700971021     C     W001A         ORLT      '0'
048800971023     C     W001A         ANDNE     *BLANK
048900971021     C                   SETON                                        32
049000971021     C                   MOVE      *ZERO         VABQFT
049100010604     C                   eval      vinmsg = %trimr(vinmsg)
049200010604     C                             + ' ' + 'VHBQFT'
049300971021     C                   ELSE
049400971021     C                   MOVEL     WI15          VABQFT
049500971023     C                   TESTN                   WD15                 30
049600971023     C  N30              TESTN                   WD15                   30
049700971023     C  N30              TESTN                   WD15                     30
049800971021     C                   MOVE      WD15          W001A             1
049900971021     C     *IN30         IFEQ      *OFF
050000971021     C     W001A         ORLT      '0'
050100971023     C     W001A         ANDNE     *BLANK
050200971021     C                   SETON                                        32
050300971021     C                   MOVE      *ZERO         VABQFT
050400010604     C                   eval      vinmsg = %trimr(vinmsg)
050500010604     C                             + ' ' + 'VHBQFT'
050600971021     C                   ELSE
050700971021     C                   MOVE      WD15          VABQFT
050800971021     C                   ENDIF
050900050118     C                   ENDIF
051000941229     C*
051100971021     C                   MOVE      *ZERO         VABCAS
051200971021     C                   MOVE      VHBCAS        WASC15
051300971023     C                   TESTN                   WI15                 30
051400971023     C  N30              TESTN                   WI15                   30
051500971023     C  N30              TESTN                   WI15                     30
051600971021     C                   MOVE      WI15          W001A             1
051700971021     C     *IN30         IFEQ      *OFF
051800971021     C     W001A         ORLT      '0'
051900971023     C     W001A         ANDNE     *BLANK
052000971021     C                   SETON                                        32
052100971021     C                   MOVE      *ZERO         VABCAS
052200010604     C                   eval      vinmsg = %trimr(vinmsg)
052300010604     C                             + ' ' + 'VHBCAS'
052400971021     C                   ELSE
052500971021     C                   MOVEL     WI15          VABCAS
052600971023     C                   TESTN                   WD15                 30
052700971023     C  N30              TESTN                   WD15                   30
052800971023     C  N30              TESTN                   WD15                     30
052900971021     C                   MOVE      WD15          W001A             1
053000971021     C     *IN30         IFEQ      *OFF
053100971021     C     W001A         ORLT      '0'
053200971023     C     W001A         ANDNE     *BLANK
053300971021     C                   SETON                                        32
053400971021     C                   MOVE      *ZERO         VABCAS
053500010604     C                   eval      vinmsg = %trimr(vinmsg)
053600010604     C                             + ' ' + 'VHBCAS'
053700971021     C                   ELSE
053800971021     C                   MOVE      WD15          VABCAS
053900971021     C                   ENDIF
054000040827     C                   ENDIF
054100971021     C*
054200971023     C                   TESTN                   VHBRMN               30
054300971023     C  N30              TESTN                   VHBRMN                 30
054400971023     C  N30              TESTN                   VHBRMN                   30
054500971021     C                   MOVE      VHBRMN        W001A             1
054600971021     C     *IN30         IFEQ      *OFF
054700971021     C     W001A         ORLT      '0'
054800971021     C                   SETON                                        32
054900971021     C                   MOVE      *ZERO         VABRMN
055000010604     C                   eval      vinmsg = %trimr(vinmsg)
055100010604     C                             + ' ' + 'VHBRMN'
055200971021     C                   ELSE
055300971021     C                   MOVE      VHBRMN        VABRMN
055400971021     C                   ENDIF
055500971021     C*
055600971023     C                   TESTN                   VHBNCD               30
055700971023     C  N30              TESTN                   VHBNCD                 30
055800971023     C  N30              TESTN                   VHBNCD                   30
055900971021     C                   MOVE      VHBNCD        W001A             1
056000971021     C     *IN30         IFEQ      *OFF
056100971021     C     W001A         ORLT      '0'
056200971023     C     W001A         ANDNE     *BLANK
056300971021     C                   SETON                                        32
056400971021     C                   MOVE      *ZERO         VABNCD
056500010604     C                   eval      vinmsg = %trimr(vinmsg)
056600010604     C                             + ' ' + 'VHBNCD'
056700971021     C                   ELSE
056800971021     C                   MOVE      VHBNCD        VABNCD
056900971021     C                   ENDIF
057000971021     C*
057100971023     C                   TESTN                   VHBNCA               30
057200971023     C  N30              TESTN                   VHBNCA                 30
057300971023     C  N30              TESTN                   VHBNCA                   30
057400971021     C                   MOVE      VHBNCA        W001A             1
057500971021     C     *IN30         IFEQ      *OFF
057600971021     C     W001A         ORLT      '0'
057700971023     C     W001A         ANDNE     *BLANK
057800971021     C                   SETON                                        32
057900971021     C                   MOVE      *ZERO         VABNCA
058000010604     C                   eval      vinmsg = %trimr(vinmsg)
058100010604     C                             + ' ' + 'VHBNCA'
058200971021     C                   ELSE
058300971021     C                   MOVE      VHBNCA        VABNCA
058400971021     C                   ENDIF
058500971021     C*
058600971023     C                   TESTN                   VHBZNC               30
058700971023     C  N30              TESTN                   VHBZNC                 30
058800971023     C  N30              TESTN                   VHBZNC                   30
058900971021     C                   MOVE      VHBZNC        W001A             1
059000971021     C     *IN30         IFEQ      *OFF
059100971021     C     W001A         ORLT      '0'
059200971023     C     W001A         ANDNE     *BLANK
059300971021     C                   SETON                                        32
059400971021     C                   MOVE      *ZERO         VABZNC
059500010604     C                   eval      vinmsg = %trimr(vinmsg)
059600010604     C                             + ' ' + 'VHBZNC'
059700971021     C                   ELSE
059800971021     C                   MOVE      VHBZNC        VABZNC
059900971021     C                   ENDIF
060000971021     C*
060100971023     C                   TESTN                   VHBDCR               30
060200971023     C  N30              TESTN                   VHBDCR                 30
060300971023     C  N30              TESTN                   VHBDCR                   30
060400971021     C                   MOVE      VHBDCR        W001A             1
060500971021     C     *IN30         IFEQ      *OFF
060600971021     C     W001A         ORLT      '0'
060700971023     C     W001A         ANDNE     *BLANK
060800971021     C                   SETON                                        32
060900971021     C                   MOVE      *ZERO         VABDCR
061000010604     C                   eval      vinmsg = %trimr(vinmsg)
061100010604     C                             + ' ' + 'VHBDCR'
061200971021     C                   ELSE
061300971021     C                   MOVE      VHBDCR        VABDCR
061400971021     C                   ENDIF
061500971021     C*
061600971023     C                   TESTN                   VHBHCR               30
061700971023     C  N30              TESTN                   VHBHCR                 30
061800971023     C  N30              TESTN                   VHBHCR                   30
061900971021     C                   MOVE      VHBHCR        W001A             1
062000971021     C     *IN30         IFEQ      *OFF
062100971021     C     W001A         ORLT      '0'
062200971023     C     W001A         ANDNE     *BLANK
062300971021     C                   SETON                                        32
062400971021     C                   MOVE      *ZERO         VABHCR
062500010604     C                   eval      vinmsg = %trimr(vinmsg)
062600010604     C                             + ' ' + 'VHBHCR'
062700971021     C                   ELSE
062800971021     C                   MOVE      VHBHCR        VABHCR
062900971021     C                   ENDIF
063000941229     C*
063100971021     C                   MOVE      *ZERO         VABVMD
063200971021     C                   MOVE      VHBVMD        WASC15
063300971023     C                   TESTN                   WI15                 30
063400971023     C  N30              TESTN                   WI15                   30
063500971023     C  N30              TESTN                   WI15                     30
063600971021     C                   MOVE      WI15          W001A             1
063700971021     C     *IN30         IFEQ      *OFF
063800971021     C     W001A         ORLT      '0'
063900971023     C     W001A         ANDNE     *BLANK
064000971021     C                   SETON                                        32
064100971021     C                   MOVE      *ZERO         VABVMD
064200010604     C                   eval      vinmsg = %trimr(vinmsg)
064300010604     C                             + ' ' + 'VHBVMD'
064400971021     C                   ELSE
064500971021     C                   MOVEL     WI15          VABVMD
064600971023     C                   TESTN                   WD15                 30
064700971023     C  N30              TESTN                   WD15                   30
064800971023     C  N30              TESTN                   WD15                     30
064900971021     C                   MOVE      WD15          W001A             1
065000971021     C     *IN30         IFEQ      *OFF
065100971021     C     W001A         ORLT      '0'
065200971023     C     W001A         ANDNE     *BLANK
065300971021     C                   SETON                                        32
065400971021     C                   MOVE      *ZERO         VABVMD
065500010604     C                   eval      vinmsg = %trimr(vinmsg)
065600010604     C                             + ' ' + 'VHBVMD'
065700971021     C                   ELSE
065800971021     C                   MOVE      WD15          VABVMD
065900971021     C                   ENDIF
066000050118     C                   ENDIF
066100971021     C*
066200971023     C                   TESTN                   VHBANT               30
066300971023     C  N30              TESTN                   VHBANT                 30
066400971023     C  N30              TESTN                   VHBANT                   30
066500971021     C                   MOVE      VHBANT        W001A             1
066600971021     C     *IN30         IFEQ      *OFF
066700971021     C     W001A         ORLT      '0'
066800971023     C     W001A         ANDNE     *BLANK
066900971021     C                   SETON                                        32
067000971021     C                   MOVE      *ZERO         VABANT
067100010604     C                   eval      vinmsg = %trimr(vinmsg)
067200010604     C                             + ' ' + 'VHBANT'
067300971021     C                   ELSE
067400971021     C                   MOVE      VHBANT        VABANT
067500971021     C                   ENDIF
067600971021     C*
067700971021     C* CAMPI ALFABETICI:
067800971021     C                   MOVE      VHBATB        VABATB
067900971021     C                   MOVE      VHBCBO        VABCBO
068000971021     C                   MOVE      VHBRSD        VABRSD
068100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
068200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
068300020117     C* ==
068400971021     C                   MOVE      VHBRD2        VABRD2
068500971021     C                   MOVE      VHBIND        VABIND
068600971021     C                   MOVE      VHBCAD        VABCAD
068700971021     C                   MOVE      VHBLOD        VABLOD
068800971021     C                   MOVE      VHBPRD        VABPRD
068900971021     C                   MOVE      VHBNZD        VABNZD
069000971021     C                   MOVE      VHBGC1        VABGC1
069100971021     C                   MOVE      VHBGC2        VABGC2
069200971021     C                   MOVE      VHBTSP        VABTSP
069300971021     C                   MOVE      VHBVAS        VABVAS
069400971021     C                   MOVE      VHBNAS        VABNAS
069500971021     C                   MOVE      VHBTIC        VABTIC
069600971021     C                   MOVE      VHBVCA        VABVCA
069700971021     C                   MOVE      VHBGCA        VABGCA
069800971021     C                   MOVE      VHBRMA        VABRMA
069900971021     C                   MOVE      VHBXCO        VABXCO
070000971021     C                   MOVE      VHBNOT        VABNOT
070100971021     C                   MOVE      VHBNT2        VABNT2
070200971021     C                   MOVE      VHBCTM        VABCTM
070300971021     C                   MOVE      VHBFFD        VABFFD
070400971021     C                   MOVE      VHBTCR        VABTCR
070500971021     C                   MOVE      VHBCTS        VABCTS
070600971021     C                   MOVE      VHBFTM        VABFTM
070700971021     C                   MOVE      VHBVAD        VABVAD
070800971021     C                   MOVE      VHBGMA        VABGMA
070900971021     C                   MOVE      VHBGGA        VABGGA
071000971021     C                   MOVE      VHBGVA        VABGVA
071100971021     C                   MOVE      VHBTC1        VABTC1
071200971021     C                   MOVE      VHBTC2        VABTC2
071300971021     C                   MOVE      VHBSCL        VABSCL
071400971021     C                   MOVE      VHBRMO        VABRMO
071500971021     C                   MOVE      VHBCMO        VABCMO
071600971021     C                   MOVE      VHBNMO        VABNMO
071700971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
071800060329     C* NELLA NAZIONE MITTENTE ORIGINALE
071900060331     C                   IF        %trim(VHBNMO) < 'A'
072000060329     C                   MOVE      *BLANKS       VABNMO
072100060329     C                   ENDIF
072200050418     C*
072300050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
072400050418     C                   IF        %trim(VABNZD) = 'I'   OR
072500050418     C                             %trim(VABNZD) = 'IT'  OR
072600050418     C                             %trim(VABNZD) = 'ITA'
072700050418     C                   EVAL      VABNZD = *blanks
072800050418     C                   ENDIF
072900041025     C*
073000041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
073100041025     C                   IF        *IN52 = *ON
073200041025     C* Reperisco la provincia dal CAP e dalla località
073300041025     C                   IF        VABCAD <> *blanks AND
073400050907     C                             VABPRD  = *blanks AND
073500050907     C                             VABNZD  = *blanks
073600041025     C                   CLEAR                   TISI95DS
073700041025     C                   EVAL      I95TCN = '3'
073800041025     C                   Z-ADD     datcor        I95DAT
073900041025     C                   EVAL      I95CAP = VABCAD
074000041025     C                   EVAL      I95LOC = VABLOD
074100041025     C                   CALL      'TISI95R'
074200041025     C                   PARM                    TISI95DS
074300041025     C                   EVAL      VABPRD = O95PRV
074400041025     C                   ENDIF
074500041025     C                   ENDIF
074600090216     C*
074700090216     C* Forzatura x aggiungere il secolo al riferimenti mittente numerico passato
074800090216     C* dal cliente (x T&T via link diretto XML)
074900090216     C                   MOVEL     *blanks       wRMN_Alfa15      15
075000090216     C                   MOVE(P)   VABRMN        wRMN_Alfa06       6
075100090216     C                   MOVE(P)   VABAAS        wAAS_Alfa         2
075200090216     C                   EVALR     wRMN_Alfa15=wAAS_Alfa+wRMN_Alfa06
075300090216     C                   MOVE      wRMN_Alfa15   VABRMN
075400040630     C*
075500040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
075600040630     C                   exsr      exeBon
075700930409     C*
075800010604     C  N31              ADD       1             §CTROK            7 0
075900010604     C   32              ADD       1             §CTRMO            7 0
076000010604     C   31              ADD       1             §CTRNO            7 0
076100020916     C  N31              WRITE     FIVAB000
076200990910     C*
076300010601     C                   if        *in31 = *off and
076400010601     C                             *in32 = *off
076500990910     C                   eval      vinflg = '1'
076600990910     C                   else
076700990910     C                   eval      vinflg = '2'
076800990910     C                   endif
076900000905     C                   endif
077000000905     C                   endif
077100010601     C*
077200010601     C                   endif
077300000905     C*
077400000905     C                   else
077500000905     C                   eval      vinflg = '1'
077600000905     C                   endif
077700000905     C*
077800000905     C  N70              update    tivin000
077900000905     C*
078000991022     C  N70              ENDdo
078100010601     C
078200010601     C*
078300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
078400010601     C                   if        cntNonEl = *zeros or
078500010601     C                             flgMulti = '0'
078600990910     C* Se non ci sono record con errori ...
078700010601     C                   if        §ctrno = 0 and
078800010601     C                             §ctrmo = 0 and
078900010601     C                             flgStato <> '2'
079000990910     C* ... restituisco esito OK.
079100990921     C                   eval      wrkesito = '0'
079200990910     C                   else
079300010601     C                   if        §ctrok > 0
079400000710     C                   eval      wrkesito = '1'
079500000710     C                   else
079600010615     C                   if        flgOk = '0'
079700010615     C                   eval      wrkesito = '2'
079800010615     C                   else
079900010615     C                   eval      wrkesito = '6'
080000010615     C                   endif
080100000710     C                   endif
080200010615     C                   endif
080300010601     C                   else
080400010601     C                   eval      wrkesito = '9'
080500010601     C                   endif
080600990910     C*
080700990914     C                   if        %open(tivin00r)
080800990908     C                   close     tivin00r
080900990914     C                   endif
081000020916     C                   if        %open(fivabwwr)
081100020916     C                   close     fivabwwr
081200990914     C                   endif
081300010601     C*
081400010601     C                   if        vlrpoi <> 999
081500010601     C                   eval      invfil = vlrpoi
081600010601     C                   endif
081700990910     C*
081800990920     C                   if        §ctrok > 0
081900010601     C                             and invfil > *zeros
082000990920     C                   exsr      invio
082100990920     C                   endif
082200010612     C*
082300010612     C                   if        flgGiro = '1'
082400010612     C                   exsr      endela
082500010612     C                   endif
082600990920     C*
082700910830     C                   ENDSR
082800000613     C***
082900990920
083000010601
083100010601
083200010601
083300010601
083400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
083500010601     C     repfil        BEGSR
083600010601     C*
083700010601     C                   if        invfil = *zeros and
083800010601     C                             depfil > *zeros and
083900010613     C                             (vinflg = *blanks or
084000010613     C                              vinflg = *zeros)
084100010601     C
084200010601     C                   eval      invfil = depfil
084300010601     C                   endif
084400010601     C*
084500010601     C                   if        depfil <> invfil and
084600010601     C                             invfil > *zeros
084700010601     C                   eval      flgMulti = '1'
084800010601     C                   if        vinflg = *blanks
084900010601     C                   add       1             cntNonEl
085000010601     C                   endif
085100010601     C                   endif
085200010601     C*
085300010601     C                   if        vinflg = '2'
085400010601     C                   eval      flgStato = '2'
085500010601     C                   endif
085600010601     C*
085700010601     C                   ENDSR
085800010601     C***
085900010601
086000010601
086100010601
086200010601
086300010601
086400010601
086500010601
086600990920      /TITLE Invio dei dati al punto operativo.
086700000613     C     invio         BEGSR
086800990920     C*
086900990920     C                   reset                   dscmz
087000010601     C                   move      invfil        cmzdst
087100990920     C                   eval      cmzfld = vlrfou
087200990920     C                   eval      cmzmbd = vlrhdl
087300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
087400000710     C                   if        prmfir = *blanks
087500020916     C                   eval      cmzfla = 'FIVAB00F'
087600020916     C                   eval      cmzmba = 'FIVAB00F'
087700000710     C                   else
087800000710     C                   eval      cmzfla = prmfir
087900000710     C                   eval      cmzmba = prmfir
088000000710     C                   endif
088100990920     C                   eval      cmznrr = *zeros
088200990920     C                   move      §ctrok        cmznrr
088300021003     C                   eval      cmzlba = vlrfl1
088400990920     C                   call(e)   'TIS711C'
088500990920     C                   parm                    dscmz
088600990921     C                   parm      *blanks       esito
088700990920     C                   if        %error
088800990920     C                             or cmzerr = '1'
088900990921     C                             or esito  = '1'
089000000710     C                   eval      wrkesito = '3'
089100990920     C                   endif
089200990920     C*
089300000613     C                   ENDSR
089400010601
089500010601
089600010601
089700010601
089800010601      /TITLE Invio dei dati al punto operativo.
089900010601     C     opeini        BEGSR
090000010601     C*
090100010601     C* Inizializzo flag e contatori operativi
090200010601     C                   movel     '0'           flgGiro           1
090300010601     C                   movel     '0'           flgMulti          1
090400010601     C                   movel     '1'           flgStato          1
090500010615     C                   movel     '0'           flgOk             1
090600010601     C                   z-add     *zeros        cntNonEl         10 0
090700010601     C                   z-add     *zeros        depfil            3 0
090800010601     C                   z-add     *zeros        invfil            3 0
090900030407     C*
091000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
091100030407     C                   if        %subst(vlrppt:1:1) = 'S'
091200030407     C                   seton                                        50
091300030407     C                   else
091400030407     C                   setoff                                       50
091500030407     C                   endif
091600041001     C*
091700041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
091800041001     C                   if        %subst(vlrppt:2:1) = 'S'
091900041001     C                   seton                                        51
092000041001     C                   else
092100041001     C                   setoff                                       51
092200041001     C                   endif
092300041025     C*
092400041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
092500041025     C* (naturalmente solo se nn già presente)
092600041025     C                   if        %subst(vlrppt:3:1) = 'S'
092700041025     C                   seton                                        52
092800041025     C                   else
092900041025     C                   setoff                                       52
093000041025     C                   endif
093100010601     C*
093200010601     C                   ENDSR
093300010601     C***
093400040630
093500040630
093600040630
093700040630
093800040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
093900040630     C     exeBon        BEGSR
094000040630     C*
094100040630     C* Imposto i parametri x il *pgm d "bonifica"
094200040630     C                   EVAL      ElencoChar = *blanks
094300040630     C                   EVAL      TipoElenco = *blanks
094400040630     C                   EVAL      CharSost   = *blanks
094500040630     C                   EVAL      UpperCase  = '1'
094600040630     C                   EVAL      ChkNull    = '1'
094700040630     C                   EVAL      CharNull   = *blanks
094800040630     C*
094900040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
095000040630     C                   EVAL      TxtInOut   = VABRSD
095100040630     C                   EVAL      EsitoBon   = *blanks
095200040630     C*
095300040630     C                   CALL      'XCHKCHAR'
095400040630     C                   PARM                    TxtInOut
095500040630     C                   PARM                    ElencoChar
095600040630     C                   PARM                    TipoElenco
095700040630     C                   PARM                    CharSost
095800040630     C                   PARM                    UpperCase
095900040630     C                   PARM                    ChkNull
096000040630     C                   PARM                    CharNull
096100040630     C                   PARM                    EsitoBon
096200040630     C*
096300040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
096400040630     C                   IF        EsitoBon = '1'
096500040630     C                   EVAL      VABRSD = TxtInOut
096600040630     C                   ENDIF
096700040630     C*
096800040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
096900040630     C                   EVAL      TxtInOut   = VABIND
097000040630     C                   EVAL      EsitoBon   = *blanks
097100040630     C*
097200040630     C                   CALL      'XCHKCHAR'
097300040630     C                   PARM                    TxtInOut
097400040630     C                   PARM                    ElencoChar
097500040630     C                   PARM                    TipoElenco
097600040630     C                   PARM                    CharSost
097700040630     C                   PARM                    UpperCase
097800040630     C                   PARM                    ChkNull
097900040630     C                   PARM                    CharNull
098000040630     C                   PARM                    EsitoBon
098100040630     C*
098200040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
098300040630     C                   IF        EsitoBon = '1'
098400040630     C                   EVAL      VABIND = TxtInOut
098500040630     C                   ENDIF
098600040630     C*
098700040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
098800040630     C                   EVAL      TxtInOut   = VABLOD
098900040630     C                   EVAL      EsitoBon   = *blanks
099000040630     C*
099100040630     C                   CALL      'XCHKCHAR'
099200040630     C                   PARM                    TxtInOut
099300040630     C                   PARM                    ElencoChar
099400040630     C                   PARM                    TipoElenco
099500040630     C                   PARM                    CharSost
099600040630     C                   PARM                    UpperCase
099700040630     C                   PARM                    ChkNull
099800040630     C                   PARM                    CharNull
099900040630     C                   PARM                    EsitoBon
100000040630     C*
100100040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100200040630     C                   IF        EsitoBon = '1'
100300040630     C                   EVAL      VABLOD = TxtInOut
100400040630     C                   ENDIF
100500040630     C*
100600040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
100700040630     C                   EVAL      TxtInOut   = VABNOT
100800040630     C                   EVAL      EsitoBon   = *blanks
100900040630     C                   EVAL      UpperCase  = *blanks
101000040630     C*
101100040630     C                   CALL      'XCHKCHAR'
101200040630     C                   PARM                    TxtInOut
101300040630     C                   PARM                    ElencoChar
101400040630     C                   PARM                    TipoElenco
101500040630     C                   PARM                    CharSost
101600040630     C                   PARM                    UpperCase
101700040630     C                   PARM                    ChkNull
101800040630     C                   PARM                    CharNull
101900040630     C                   PARM                    EsitoBon
102000040630     C*
102100040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
102200040630     C                   IF        EsitoBon = '1'
102300040630     C                   EVAL      VABNOT = TxtInOut
102400040630     C                   ENDIF
102500040630     C*
102600040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
102700040630     C                   EVAL      TxtInOut   = VABNT2
102800040630     C                   EVAL      EsitoBon   = *blanks
102900040630     C                   EVAL      UpperCase  = *blanks
103000040630     C*
103100040630     C                   CALL      'XCHKCHAR'
103200040630     C                   PARM                    TxtInOut
103300040630     C                   PARM                    ElencoChar
103400040630     C                   PARM                    TipoElenco
103500040630     C                   PARM                    CharSost
103600040630     C                   PARM                    UpperCase
103700040630     C                   PARM                    ChkNull
103800040630     C                   PARM                    CharNull
103900040630     C                   PARM                    EsitoBon
104000040630     C*
104100040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
104200040630     C                   IF        EsitoBon = '1'
104300040630     C                   EVAL      VABNT2 = TxtInOut
104400040630     C                   ENDIF
104500040630     C*
104600040630     C                   ENDSR
104700090511
104800090511
104900090511
105000010601
105100010601
105200990910
105300000613     C     *inzsr        BEGSR
105400990910     C*
105500990910     C     *entry        plist
105600990920     C                   parm                    tivlrds
105700990921     C                   parm      wrkesito      esito
105800000724     C                   parm                    prmlit
105900000710     C                   parm                    prmfir
106000010606     C*
106100010606     C* CALCOLA LA DATA CORRENTE
106200010606     C                   time                    wn14             14 0
106300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
106400010606     C                   z-add     wn8           g08dat
106500010606     C                   z-add     *zeros        g08inv
106600010606     C                   movel     '0'           g08err
106700010606     C                   call      'XSRDA8'
106800010606     C                   parm                    wlbda8
106900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
107000000613     C*
107100000613     C                   ENDSR
107200000613     C***
