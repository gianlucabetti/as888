000100090108      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500090108     FEDIVABwr  O    E             DISK    usropn
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
004500080611     D vhbtelx                89    103
004600000313     D vhbind                104    138
004700000313     D vhbcad                139    147
004800000313     D vhblod                148    182
004900000313     D vhbprd                183    184
005000000313     D vhbnzd                185    187
005100000313     D vhbgc1                188    189
005200000313     D vhbgc2                190    191
005300000313     D vhbctr                192    194
005400000313     D vhbtsp                195    195
005500000313     D vhbias                196    208
005600000313     D vhbvas                209    211
005700000313     D vhbnas                212    226
005800000313     D vhbncl                227    231
005900000313     D vhbpkb                232    238
006000000313     D vhbvlb                239    243
006100000313     D vhbqft                244    256
006200000313     D vhbcas                257    269
006300000313     D vhbtic                270    271
006400000313     D vhbvca                272    274
006500000313     D vhbgca                275    276
006600000313     D vhbrmn                277    291
006700000313     D vhbrma                292    306
006800000313     D vhbncd                307    313
006900000313     D vhbnca                314    320
007000000313     D vhbxco                321    321
007100000313     D vhbnot                322    356
007200000313     D vhbnt2                357    391
007300000313     D vhbznc                392    393
007400000313     D vhbctm                394    395
007500000313     D vhbffd                396    396
007600000313     D vhbdcr                397    404
007700000313     D vhbtcr                405    405
007800000313     D vhbhcr                406    409
007900000313     D vhbcts                410    411
008000000313     D vhbftm                412    412
008100000313     D vhbvmd                413    425
008200000313     D vhbvad                426    428
008300000313     D vhbgma                429    430
008400000313     D vhbgga                431    432
008500000313     D vhbgva                433    434
008600000313     D vhbtc1                435    435
008700000313     D vhbtc2                436    436
008800000313     D vhbscl                437    437
008900000313     D vhbant                438    446
009000000313     D vhbrmo                447    471
009100000313     D vhbcmo                472    480
009200000313     D vhbnmo                481    483
009300000313     D  X_rec                  1    483    inz
009400000313     D*
009500990910     D esito           s              1
009600000724     D prmlit          s             10
009700000710     D prmfir          s             10
009800990921     D wrkesito        s                   like(esito)
009900000613     D rrnum           s              6  0 INZ(*zeros)
010000000313     D*
010100010606     D*------------------
010200010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
010300010606     D*------------------
010400010606     D WLBDA8          DS                  INZ
010500010606     D  G08DAT                 1      8  0
010600010606     D  G08INV                 9     16  0
010700010606     D  G08ERR                17     17
010800010606     D  G08TGI                18     22  0
010900010606     D*
011000040407     D TxtInOut        S           2048
011100040407     D ElencoChar      S            256
011200040407     D TipoElenco      S              1
011300040407     D CharSost        S              1
011400040407     D UpperCase       S              1
011500040407     D ChkNull         S              1
011600040407     D CharNull        S              1
011700040407     D EsitoBon        S              1
011800010606
011900010606
012000010606
012100010606
012200000913     C                   reset                   rrnum
012300990921     C                   reset                   esito
012400990921     C                   reset                   wrkesito
012500010601     C*
012600010601     C                   exsr      opeini
012700010601     C                   exsr      rwvab
012800041025     C*
012900041025     C* Esegue lancio TISI95R solo x chiusura
013000041025     C                   CLEAR                   TISI95DS
013100041025     C                   EVAL      I95TLA = 'C'
013200041025     C                   CALL      'TISI95R'
013300041025     C                   PARM                    TISI95DS
013400010601     C*
013500010601     C                   seton                                        lr
013600010601
013700010601
013800010601
013900010601
014000010601     C*--------------------------------------------------------
014100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
014200010601     C*--------------------------------------------------------
014300010601     C     PREELA        BEGSR
014400010601     C*
014500010601     C* SE OCCORRE SPEDIRE IN FILIALE
014600010601     C                   if        invfil <> *zeros and
014700010601     C                             flgGiro = '0'
014800010601     C*
014900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
015000010601     C                   eval      flgGiro = '1'
015100010601     C*
015200010601     C                   endif
015300010601     C*
015400010601     C                   ENDSR
015500010601     C***
015600010601
015700010601
015800010601
015900010601
016000010601
016100010601
016200010601     C*--------------------------------------------------------
016300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016400010601     C*--------------------------------------------------------
016500010601     C     ENDELA        BEGSR
016600010601     C*
016700010601     C                   ENDSR
016800010601     C***
016900000613
017000010601
017100010601
017200010601
017300010601
017400910830     C*--------------------------------------------------------
017500090108     C* RWVAB   LEGGE tivin00r E SCRIVE EDIVABWF              *
017600910830     C*--------------------------------------------------------
017700941229     C     RWVAB         BEGSR
017800990910     C*
017900990914     C                   if        not %open(tivin00r)
018000990908     C                   open      tivin00r
018100990914     C                   endif
018200090108     C                   if        not %open(edivabwr)
018300090108     C                   open      edivabwr
018400990914     C                   endif
018500990910     C*
018600990910     C                   clear                   §CTROK
018700990910     C                   clear                   §CTRMO
018800990910     C                   clear                   §CTRNO
018900990910     C*
019000921023     C                   DO        *HIVAL
019100990913     C*
019200990915     C                   READ      tivin00r                               70
019300010618     C*
019400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019500010618     C                   if        vinflg = '1'
019600010618     C                   eval      flgOk = '1'
019700010618     C                   endif
019800010618     C*
019900000905     C                   if        vindta > *blanks
020000000613     C                   add       1             rrnum
020100990913     C*
020200010601     C                   if        *in70 = *off and
020300990913     C                             (vinflg = *blanks
020400010711     C                              or vinflg = '0'
020500010711     C                              or vinflg = '2')
020600990913     C*
020700090108     C                   clear                   EDIVAB00
020800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020900010711     C                   if        vinflg = *blanks or vinflg = '0'
021000010711     C                   clear                   vinmsg
021100010711     C                   endif
021200010711     C*
021300000313     C                   eval      x_rec  = vindta
021400991022     C*
021500991022     C                   testb     x'3F'         vhbatb                   02
021600991022     C                   if        not *in02
021700991022     C*
021800971021     C                   SETOFF                                       303132
021900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
022000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
022100971023     C*    OPPURE TUTTI BLANKS
022200010601     C*
022300010601     C                   TESTN                   VHBLNP               30
022400010601     C  N30              TESTN                   VHBLNP                 30
022500010601     C  N30              TESTN                   VHBLNP                   30
022600010601     C                   MOVE      VHBLNP        W001A             1
022700010601     C     *IN30         IFEQ      *OFF
022800010601     C     W001A         ORLT      '0'
022900010601     C                   SETON                                        31
023000010601     C                   eval      vinmsg = %trimr(vinmsg)
023100010601     C                             + ' ' + 'VHBLNP'
023200010601     C                   ELSE
023300010601     C                   MOVE      VHBLNP        VABLNP
023400010601     C     VABLNP        IFLE      *ZEROS
023500010601     C                   SETON                                        31
023600010601     C                   eval      vinmsg = %trimr(vinmsg)
023700010601     C                             + ' ' + 'VHBLNP'
023800010601     C                   ENDIF
023900010601     C                   ENDIF
024000010601     C*
024100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024200010601     C                   eval      depfil = VABLNP
024300010601     C                   exsr      repfil
024400010601     C                   if        depfil = invfil
024500021014     C                   if        vlrpoi = 999
024600021010     C                   MOVE(P)   invfil        VABFGS
024700021014     C                   else
024800021014     C                   MOVE(P)   vlrpoi        VABFGS
024900021014     C                   endif
025000010601     C*
025100010601     C                   exsr      PREELA
025200010601     C*
025300971023     C                   TESTN                   VHBCCM               30
025400971023     C  N30              TESTN                   VHBCCM                 30
025500971023     C  N30              TESTN                   VHBCCM                   30
025600971021     C                   MOVE      VHBCCM        W001A             1
025700971021     C     *IN30         IFEQ      *OFF
025800971021     C     W001A         ORLT      '0'
025900971021     C                   SETON                                        31
026000010604     C                   eval      vinmsg = %trimr(vinmsg)
026100010604     C                             + ' ' + 'VHBCCM'
026200971021     C                   ELSE
026300971021     C                   MOVE      VHBCCM        VABCCM
026400971024     C     VABCCM        IFLE      *ZEROS
026500971024     C                   SETON                                        31
026600010604     C                   eval      vinmsg = %trimr(vinmsg)
026700010604     C                             + ' ' + 'VHBCCM'
026800971024     C                   ENDIF
026900971021     C                   ENDIF
027000971021     C*
027100971023     C                   TESTN                   VHBAAS               30
027200971023     C  N30              TESTN                   VHBAAS                 30
027300971023     C  N30              TESTN                   VHBAAS                   30
027400971021     C                   MOVE      VHBAAS        W001A             1
027500971021     C     *IN30         IFEQ      *OFF
027600971021     C     W001A         ORLT      '0'
027700971023     C     W001A         ANDNE     *BLANK
027800971021     C                   SETON                                        32
027900010606     C                   MOVEL     datcor        VABAAS
028000010604     C                   eval      vinmsg = %trimr(vinmsg)
028100010604     C                             + ' ' + 'VHBAAS'
028200971021     C                   ELSE
028300971021     C                   MOVE      VHBAAS        VABAAS
028400971021     C                   ENDIF
028500971021     C*
028600971023     C                   TESTN                   VHBMGS               30
028700971023     C  N30              TESTN                   VHBMGS                 30
028800971023     C  N30              TESTN                   VHBMGS                   30
028900971021     C                   MOVE      VHBMGS        W001A             1
029000971021     C     *IN30         IFEQ      *OFF
029100971021     C     W001A         ORLT      '0'
029200971023     C     W001A         ANDNE     *BLANK
029300971021     C                   SETON                                        32
029400010601     C                   MOVE      '0101'        VABMGS
029500010604     C                   eval      vinmsg = %trimr(vinmsg)
029600010604     C                             + ' ' + 'VHBMGS'
029700971021     C                   ELSE
029800971021     C                   MOVE      VHBMGS        VABMGS
029900971021     C                   ENDIF
030000971021     C*
030100971023     C                   TESTN                   VHBNRS               30
030200971023     C  N30              TESTN                   VHBNRS                 30
030300971023     C  N30              TESTN                   VHBNRS                   30
030400971021     C                   MOVE      VHBNRS        W001A             1
030500971021     C     *IN30         IFEQ      *OFF
030600971021     C     W001A         ORLT      '0'
030700971023     C     W001A         ANDNE     *BLANK
030800971021     C                   SETON                                        32
030900971021     C                   MOVE      *ZERO         VABNRS
031000010604     C                   eval      vinmsg = %trimr(vinmsg)
031100010604     C                             + ' ' + 'VHBNRS'
031200971021     C                   ELSE
031300971021     C                   MOVE      VHBNRS        VABNRS
031400971021     C                   ENDIF
031500971021     C*
031600971023     C                   TESTN                   VHBNSP               30
031700971023     C  N30              TESTN                   VHBNSP                 30
031800971023     C  N30              TESTN                   VHBNSP                   30
031900971021     C                   MOVE      VHBNSP        W001A             1
032000971021     C     *IN30         IFEQ      *OFF
032100971021     C     W001A         ORLT      '0'
032200971023     C     W001A         ANDNE     *BLANK
032300971021     C                   SETON                                        32
032400971021     C                   MOVE      *ZERO         VABNSP
032500010604     C                   eval      vinmsg = %trimr(vinmsg)
032600010604     C                             + ' ' + 'VHBNSP'
032700971021     C                   ELSE
032800971021     C                   MOVE      VHBNSP        VABNSP
032900971021     C                   ENDIF
033000971021     C*
033100971023     C                   TESTN                   VHBLNA               30
033200971023     C  N30              TESTN                   VHBLNA                 30
033300971023     C  N30              TESTN                   VHBLNA                   30
033400971021     C                   MOVE      VHBLNA        W001A             1
033500971021     C     *IN30         IFEQ      *OFF
033600971021     C     W001A         ORLT      '0'
033700971023     C     W001A         ANDNE     *BLANK
033800971021     C                   SETON                                        32
033900971021     C                   MOVE      *ZERO         VABLNA
034000010604     C                   eval      vinmsg = %trimr(vinmsg)
034100010604     C                             + ' ' + 'VHBLNA'
034200971021     C                   ELSE
034300971021     C                   MOVE      VHBLNA        VABLNA
034400971021     C                   ENDIF
034500971021     C*
034600971023     C                   TESTN                   VHBCTR               30
034700971023     C  N30              TESTN                   VHBCTR                 30
034800971023     C  N30              TESTN                   VHBCTR                   30
034900971021     C                   MOVE      VHBCTR        W001A             1
035000971021     C     *IN30         IFEQ      *OFF
035100971021     C     W001A         ORLT      '0'
035200971023     C     W001A         ANDNE     *BLANK
035300971021     C                   SETON                                        32
035400971021     C                   MOVE      *ZERO         VABCTR
035500010604     C                   eval      vinmsg = %trimr(vinmsg)
035600010604     C                             + ' ' + 'VHBCTR'
035700971021     C                   ELSE
035800971021     C                   MOVE      VHBCTR        VABCTR
035900971021     C                   ENDIF
036000941229     C*
036100971021     C                   MOVE      *ZERO         VABIAS
036200941229     C                   MOVE      VHBIAS        WASC15
036300971023     C                   TESTN                   WI15                 30
036400971023     C  N30              TESTN                   WI15                   30
036500971023     C  N30              TESTN                   WI15                     30
036600971021     C                   MOVE      WI15          W001A             1
036700971021     C     *IN30         IFEQ      *OFF
036800971021     C     W001A         ORLT      '0'
036900971023     C     W001A         ANDNE     *BLANK
037000971021     C                   SETON                                        32
037100971021     C                   MOVE      *ZERO         VABIAS
037200010604     C                   eval      vinmsg = %trimr(vinmsg)
037300010604     C                             + ' ' + 'VHBIAS'
037400971021     C                   ELSE
037500971021     C                   MOVEL     WI15          VABIAS
037600971023     C                   TESTN                   WD15                 30
037700971023     C  N30              TESTN                   WD15                   30
037800971023     C  N30              TESTN                   WD15                     30
037900971021     C                   MOVE      WD15          W001A             1
038000971021     C     *IN30         IFEQ      *OFF
038100971021     C     W001A         ORLT      '0'
038200971023     C     W001A         ANDNE     *BLANK
038300971021     C                   SETON                                        32
038400971021     C                   MOVE      *ZERO         VABIAS
038500010604     C                   eval      vinmsg = %trimr(vinmsg)
038600010604     C                             + ' ' + 'VHBIAS'
038700971021     C                   ELSE
038800971021     C                   MOVE      WD15          VABIAS
038900971021     C                   ENDIF
039000050118     C                   ENDIF
039100941229     C*
039200971023     C                   TESTN                   VHBNCL               30
039300971023     C  N30              TESTN                   VHBNCL                 30
039400971023     C  N30              TESTN                   VHBNCL                   30
039500971021     C                   MOVE      VHBNCL        W001A             1
039600971021     C     *IN30         IFEQ      *OFF
039700971021     C     W001A         ORLT      '0'
039800971021     C                   SETON                                        32
039900971021     C                   MOVE      *ZERO         VABNCL
040000010604     C                   eval      vinmsg = %trimr(vinmsg)
040100010604     C                             + ' ' + 'VHBNCL'
040200971021     C                   ELSE
040300971021     C                   MOVE      VHBNCL        VABNCL
040400971021     C                   ENDIF
040500941229     C*
040600971021     C                   MOVE      *ZERO         VABPKB
040700971021     C                   MOVE      VHBPKB        WASC09
040800050503     C                   IF        WI09 = *blanks
040900050503     C                   EVAL      WI09 = *zeros
041000050503     C                   ENDIF
041100971023     C                   TESTN                   WI09                 30
041200971023     C  N30              TESTN                   WI09                   30
041300971023     C  N30              TESTN                   WI09                     30
041400971021     C                   MOVE      WI09          W001A             1
041500971021     C     *IN30         IFEQ      *OFF
041600971021     C     W001A         ORLT      '0'
041700971021     C                   SETON                                        32
041800971021     C                   MOVE      *ZERO         VABPKB
041900010604     C                   eval      vinmsg = %trimr(vinmsg)
042000010604     C                             + ' ' + 'VHBPKB'
042100971021     C                   ELSE
042200971021     C                   MOVEL     WI09          VABPKB
042300971023     C                   TESTN                   WD09                 30
042400971023     C  N30              TESTN                   WD09                     30
042500971021     C                   MOVE      WD09          W001A             1
042600971021     C     *IN30         IFEQ      *OFF
042700971021     C     W001A         ORLT      '0'
042800971023     C     W001A         ANDNE     *BLANK
042900971021     C                   SETON                                        32
043000971021     C                   MOVE      *ZERO         VABPKB
043100010604     C                   eval      vinmsg = %trimr(vinmsg)
043200010604     C                             + ' ' + 'VHBPKB'
043300971021     C                   ELSE
043400971021     C                   MOVE      WD09          VABPKB
043500971021     C                   ENDIF
043600050118     C                   ENDIF
043700971021     C*
043800041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
043900041001     C                   IF        *IN51 = *OFF
044000971021     C                   MOVE      *ZERO         VABVLB
044100971021     C                   MOVE      VHBVLB        WASC07
044200050503     C                   IF        WI07 = *blanks
044300050503     C                   EVAL      WI07 = *zeros
044400050503     C                   ENDIF
044500971023     C                   TESTN                   WI07                 30
044600971023     C  N30              TESTN                   WI07                   30
044700971023     C  N30              TESTN                   WI07                     30
044800971021     C                   MOVE      WI07          W001A             1
044900971021     C     *IN30         IFEQ      *OFF
045000971021     C     W001A         ORLT      '0'
045100971023     C     W001A         ANDNE     *BLANK
045200971021     C                   SETON                                        32
045300971021     C                   MOVE      *ZERO         VABVLB
045400010604     C                   eval      vinmsg = %trimr(vinmsg)
045500010604     C                             + ' ' + 'VHBVLB'
045600971021     C                   ELSE
045700971021     C                   MOVEL     WI07          VABVLB
045800971023     C                   TESTN                   WD07                 30
045900971023     C  N30              TESTN                   WD07                   30
046000971023     C  N30              TESTN                   WD07                     30
046100971021     C                   MOVE      WD07          W001A             1
046200971021     C     *IN30         IFEQ      *OFF
046300971021     C     W001A         ORLT      '0'
046400971023     C     W001A         ANDNE     *BLANK
046500971021     C                   SETON                                        32
046600971021     C                   MOVE      *ZERO         VABVLB
046700010604     C                   eval      vinmsg = %trimr(vinmsg)
046800010604     C                             + ' ' + 'VHBVLB'
046900971021     C                   ELSE
047000971021     C                   MOVE      WD07          VABVLB
047100971021     C                   ENDIF
047200050118     C                   ENDIF
047300041001     C                   ELSE
047400041001     C* ...altrimenti forzo azzeramento del volume
047500041001     C                   Z-ADD     *zeros        VABVLB
047600041001     C                   ENDIF
047700941229     C*
047800971021     C                   MOVE      *ZERO         VABQFT
047900971021     C                   MOVE      VHBQFT        WASC15
048000971023     C                   TESTN                   WI15                 30
048100971023     C  N30              TESTN                   WI15                   30
048200971023     C  N30              TESTN                   WI15                     30
048300971021     C                   MOVE      WI15          W001A             1
048400971021     C     *IN30         IFEQ      *OFF
048500971021     C     W001A         ORLT      '0'
048600971023     C     W001A         ANDNE     *BLANK
048700971021     C                   SETON                                        32
048800971021     C                   MOVE      *ZERO         VABQFT
048900010604     C                   eval      vinmsg = %trimr(vinmsg)
049000010604     C                             + ' ' + 'VHBQFT'
049100971021     C                   ELSE
049200971021     C                   MOVEL     WI15          VABQFT
049300971023     C                   TESTN                   WD15                 30
049400971023     C  N30              TESTN                   WD15                   30
049500971023     C  N30              TESTN                   WD15                     30
049600971021     C                   MOVE      WD15          W001A             1
049700971021     C     *IN30         IFEQ      *OFF
049800971021     C     W001A         ORLT      '0'
049900971023     C     W001A         ANDNE     *BLANK
050000971021     C                   SETON                                        32
050100971021     C                   MOVE      *ZERO         VABQFT
050200010604     C                   eval      vinmsg = %trimr(vinmsg)
050300010604     C                             + ' ' + 'VHBQFT'
050400971021     C                   ELSE
050500971021     C                   MOVE      WD15          VABQFT
050600971021     C                   ENDIF
050700050118     C                   ENDIF
050800941229     C*
050900971021     C                   MOVE      *ZERO         VABCAS
051000971021     C                   MOVE      VHBCAS        WASC15
051100971023     C                   TESTN                   WI15                 30
051200971023     C  N30              TESTN                   WI15                   30
051300971023     C  N30              TESTN                   WI15                     30
051400971021     C                   MOVE      WI15          W001A             1
051500971021     C     *IN30         IFEQ      *OFF
051600971021     C     W001A         ORLT      '0'
051700971023     C     W001A         ANDNE     *BLANK
051800971021     C                   SETON                                        32
051900971021     C                   MOVE      *ZERO         VABCAS
052000010604     C                   eval      vinmsg = %trimr(vinmsg)
052100010604     C                             + ' ' + 'VHBCAS'
052200971021     C                   ELSE
052300971021     C                   MOVEL     WI15          VABCAS
052400971023     C                   TESTN                   WD15                 30
052500971023     C  N30              TESTN                   WD15                   30
052600971023     C  N30              TESTN                   WD15                     30
052700971021     C                   MOVE      WD15          W001A             1
052800971021     C     *IN30         IFEQ      *OFF
052900971021     C     W001A         ORLT      '0'
053000971023     C     W001A         ANDNE     *BLANK
053100971021     C                   SETON                                        32
053200971021     C                   MOVE      *ZERO         VABCAS
053300010604     C                   eval      vinmsg = %trimr(vinmsg)
053400010604     C                             + ' ' + 'VHBCAS'
053500971021     C                   ELSE
053600971021     C                   MOVE      WD15          VABCAS
053700971021     C                   ENDIF
053800040827     C                   ENDIF
053900971021     C*
054000971023     C                   TESTN                   VHBRMN               30
054100971023     C  N30              TESTN                   VHBRMN                 30
054200971023     C  N30              TESTN                   VHBRMN                   30
054300971021     C                   MOVE      VHBRMN        W001A             1
054400971021     C     *IN30         IFEQ      *OFF
054500971021     C     W001A         ORLT      '0'
054600971021     C                   SETON                                        32
054700971021     C                   MOVE      *ZERO         VABRMN
054800010604     C                   eval      vinmsg = %trimr(vinmsg)
054900010604     C                             + ' ' + 'VHBRMN'
055000971021     C                   ELSE
055100971021     C                   MOVE      VHBRMN        VABRMN
055200971021     C                   ENDIF
055300971021     C*
055400971023     C                   TESTN                   VHBNCD               30
055500971023     C  N30              TESTN                   VHBNCD                 30
055600971023     C  N30              TESTN                   VHBNCD                   30
055700971021     C                   MOVE      VHBNCD        W001A             1
055800971021     C     *IN30         IFEQ      *OFF
055900971021     C     W001A         ORLT      '0'
056000971023     C     W001A         ANDNE     *BLANK
056100971021     C                   SETON                                        32
056200971021     C                   MOVE      *ZERO         VABNCD
056300010604     C                   eval      vinmsg = %trimr(vinmsg)
056400010604     C                             + ' ' + 'VHBNCD'
056500971021     C                   ELSE
056600971021     C                   MOVE      VHBNCD        VABNCD
056700971021     C                   ENDIF
056800971021     C*
056900971023     C                   TESTN                   VHBNCA               30
057000971023     C  N30              TESTN                   VHBNCA                 30
057100971023     C  N30              TESTN                   VHBNCA                   30
057200971021     C                   MOVE      VHBNCA        W001A             1
057300971021     C     *IN30         IFEQ      *OFF
057400971021     C     W001A         ORLT      '0'
057500971023     C     W001A         ANDNE     *BLANK
057600971021     C                   SETON                                        32
057700971021     C                   MOVE      *ZERO         VABNCA
057800010604     C                   eval      vinmsg = %trimr(vinmsg)
057900010604     C                             + ' ' + 'VHBNCA'
058000971021     C                   ELSE
058100971021     C                   MOVE      VHBNCA        VABNCA
058200971021     C                   ENDIF
058300971021     C*
058400971023     C                   TESTN                   VHBZNC               30
058500971023     C  N30              TESTN                   VHBZNC                 30
058600971023     C  N30              TESTN                   VHBZNC                   30
058700971021     C                   MOVE      VHBZNC        W001A             1
058800971021     C     *IN30         IFEQ      *OFF
058900971021     C     W001A         ORLT      '0'
059000971023     C     W001A         ANDNE     *BLANK
059100971021     C                   SETON                                        32
059200971021     C                   MOVE      *ZERO         VABZNC
059300010604     C                   eval      vinmsg = %trimr(vinmsg)
059400010604     C                             + ' ' + 'VHBZNC'
059500971021     C                   ELSE
059600971021     C                   MOVE      VHBZNC        VABZNC
059700971021     C                   ENDIF
059800971021     C*
059900971023     C                   TESTN                   VHBDCR               30
060000971023     C  N30              TESTN                   VHBDCR                 30
060100971023     C  N30              TESTN                   VHBDCR                   30
060200971021     C                   MOVE      VHBDCR        W001A             1
060300971021     C     *IN30         IFEQ      *OFF
060400971021     C     W001A         ORLT      '0'
060500971023     C     W001A         ANDNE     *BLANK
060600971021     C                   SETON                                        32
060700971021     C                   MOVE      *ZERO         VABDCR
060800010604     C                   eval      vinmsg = %trimr(vinmsg)
060900010604     C                             + ' ' + 'VHBDCR'
061000971021     C                   ELSE
061100971021     C                   MOVE      VHBDCR        VABDCR
061200971021     C                   ENDIF
061300971021     C*
061400971023     C                   TESTN                   VHBHCR               30
061500971023     C  N30              TESTN                   VHBHCR                 30
061600971023     C  N30              TESTN                   VHBHCR                   30
061700971021     C                   MOVE      VHBHCR        W001A             1
061800971021     C     *IN30         IFEQ      *OFF
061900971021     C     W001A         ORLT      '0'
062000971023     C     W001A         ANDNE     *BLANK
062100971021     C                   SETON                                        32
062200971021     C                   MOVE      *ZERO         VABHCR
062300010604     C                   eval      vinmsg = %trimr(vinmsg)
062400010604     C                             + ' ' + 'VHBHCR'
062500971021     C                   ELSE
062600971021     C                   MOVE      VHBHCR        VABHCR
062700971021     C                   ENDIF
062800941229     C*
062900971021     C                   MOVE      *ZERO         VABVMD
063000971021     C                   MOVE      VHBVMD        WASC15
063100971023     C                   TESTN                   WI15                 30
063200971023     C  N30              TESTN                   WI15                   30
063300971023     C  N30              TESTN                   WI15                     30
063400971021     C                   MOVE      WI15          W001A             1
063500971021     C     *IN30         IFEQ      *OFF
063600971021     C     W001A         ORLT      '0'
063700971023     C     W001A         ANDNE     *BLANK
063800971021     C                   SETON                                        32
063900971021     C                   MOVE      *ZERO         VABVMD
064000010604     C                   eval      vinmsg = %trimr(vinmsg)
064100010604     C                             + ' ' + 'VHBVMD'
064200971021     C                   ELSE
064300971021     C                   MOVEL     WI15          VABVMD
064400971023     C                   TESTN                   WD15                 30
064500971023     C  N30              TESTN                   WD15                   30
064600971023     C  N30              TESTN                   WD15                     30
064700971021     C                   MOVE      WD15          W001A             1
064800971021     C     *IN30         IFEQ      *OFF
064900971021     C     W001A         ORLT      '0'
065000971023     C     W001A         ANDNE     *BLANK
065100971021     C                   SETON                                        32
065200971021     C                   MOVE      *ZERO         VABVMD
065300010604     C                   eval      vinmsg = %trimr(vinmsg)
065400010604     C                             + ' ' + 'VHBVMD'
065500971021     C                   ELSE
065600971021     C                   MOVE      WD15          VABVMD
065700971021     C                   ENDIF
065800050118     C                   ENDIF
065900971021     C*
066000971023     C                   TESTN                   VHBANT               30
066100971023     C  N30              TESTN                   VHBANT                 30
066200971023     C  N30              TESTN                   VHBANT                   30
066300971021     C                   MOVE      VHBANT        W001A             1
066400971021     C     *IN30         IFEQ      *OFF
066500971021     C     W001A         ORLT      '0'
066600971023     C     W001A         ANDNE     *BLANK
066700971021     C                   SETON                                        32
066800971021     C                   MOVE      *ZERO         VABANT
066900010604     C                   eval      vinmsg = %trimr(vinmsg)
067000010604     C                             + ' ' + 'VHBANT'
067100971021     C                   ELSE
067200971021     C                   MOVE      VHBANT        VABANT
067300971021     C                   ENDIF
067400971021     C*
067500971021     C* CAMPI ALFABETICI:
067600971021     C                   MOVE      VHBATB        VABATB
067700971021     C                   MOVE      VHBCBO        VABCBO
067800971021     C                   MOVE      VHBRSD        VABRSD
067900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
068000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
068100020117     C* ==
068200971021     C                   MOVE      VHBRD2        VABRD2
068300971021     C                   MOVE      VHBIND        VABIND
068400971021     C                   MOVE      VHBCAD        VABCAD
068500971021     C                   MOVE      VHBLOD        VABLOD
068600971021     C                   MOVE      VHBPRD        VABPRD
068700971021     C                   MOVE      VHBNZD        VABNZD
068800971021     C                   MOVE      VHBGC1        VABGC1
068900971021     C                   MOVE      VHBGC2        VABGC2
069000971021     C                   MOVE      VHBTSP        VABTSP
069100971021     C                   MOVE      VHBVAS        VABVAS
069200971021     C                   MOVE      VHBNAS        VABNAS
069300971021     C                   MOVE      VHBTIC        VABTIC
069400971021     C                   MOVE      VHBVCA        VABVCA
069500971021     C                   MOVE      VHBGCA        VABGCA
069600971021     C                   MOVE      VHBRMA        VABRMA
069700971021     C                   MOVE      VHBXCO        VABXCO
069800971021     C                   MOVE      VHBNOT        VABNOT
069900971021     C                   MOVE      VHBNT2        VABNT2
070000971021     C                   MOVE      VHBCTM        VABCTM
070100971021     C                   MOVE      VHBFFD        VABFFD
070200971021     C                   MOVE      VHBTCR        VABTCR
070300971021     C                   MOVE      VHBCTS        VABCTS
070400971021     C                   MOVE      VHBFTM        VABFTM
070500971021     C                   MOVE      VHBVAD        VABVAD
070600971021     C                   MOVE      VHBGMA        VABGMA
070700971021     C                   MOVE      VHBGGA        VABGGA
070800971021     C                   MOVE      VHBGVA        VABGVA
070900971021     C                   MOVE      VHBTC1        VABTC1
071000971021     C                   MOVE      VHBTC2        VABTC2
071100971021     C                   MOVE      VHBSCL        VABSCL
071200971021     C                   MOVE      VHBRMO        VABRMO
071300971021     C                   MOVE      VHBCMO        VABCMO
071400971021     C                   MOVE      VHBNMO        VABNMO
071500971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
071600971021     C* NELLA NAZIONE ORIGINALE
071700971021     C                   MOVE      VHBNMO        WA1D              1
071800971021     C                   MOVEL     VHBNMO        WA1S              1
071900971021     C     WA1D          IFNE      *BLANK
072000971021     C     WA1S          ANDEQ     *BLANK
072100971021     C                   MOVE      *BLANK        VABNMO
072200971021     C                   END
072300050418     C*
072400050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
072500050418     C                   IF        %trim(VABNZD) = 'I'   OR
072600050418     C                             %trim(VABNZD) = 'IT'  OR
072700050418     C                             %trim(VABNZD) = 'ITA'
072800050418     C                   EVAL      VABNZD = *blanks
072900050418     C                   ENDIF
073000041025     C*
073100041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
073200041025     C                   IF        *IN52 = *ON
073300041025     C* Reperisco la provincia dal CAP e dalla località
073400041025     C                   IF        VABCAD <> *blanks AND
073500041025     C                             VABPRD  = *blanks
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
074600050905     C*
074700050905     C* EFfettuo considerazioni x forzatura codici clienti mittenti pilotati dal cliente
074800080110     C                   IF        VABSCL = '1' AND
074900120709     C*                            VABCCM = 0431004
075000120709     C                             VABCCM = 0435520
075100120709     C*                  EVAL      VABCCM = 0433068
075200120709     C                   EVAL      VABCCM = 0435557
075300050905     C                   ENDIF
075400050905     C                   IF        VABSCL = '2' AND
075500120709     C*                            VABCCM = 0430093
075600120709     C                             VABCCM = 0435556
075700120709     C*                  EVAL      VABCCM = 0433069
075800120709     C                   EVAL      VABCCM = 0435558
075900050905     C                   ENDIF
076000080611     C*
076100080611     C* Effettuo considerazioni x evidenziazione telefono destinatario
076200080611     C                   MOVEL     *BLANKS       wNOTE            70
076300080611     C                   EVAL      wNOTE  = %trim(VABNOT) + ' ' +
076400080611     C                                      %trim(VABNT2)
076500080611     C                   EVAL      wNOTE  = %trim(wNOTE)  + ' ' +
076600080611     C                                      'TEL.' + %trim(VHBTELX)
076700080611     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
076800080611     C                   EVAL      VABNT2 = %subst(wNOTE:36)
076900040630     C*
077000040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
077100040630     C                   exsr      exeBon
077200090108     C*
077300090108     C* VALORIZZO CAMPI RELATIVI AL "CMR"
077400090108     C                   eval      VABCMR = %char(datcor) + %char(oracor)
077500090108     C                   eval      VABDCM = DATCOR
077600090108     C                   eval      VABDTS = DATCOR
077700090108     C                   eval      VABHMS = ORACOR
077800090108     C                   eval      VABCNT = 1
077900930409     C*
078000010604     C  N31              ADD       1             §CTROK            7 0
078100010604     C   32              ADD       1             §CTRMO            7 0
078200010604     C   31              ADD       1             §CTRNO            7 0
078300090108     C  N31              WRITE     EDIVAB00
078400990910     C*
078500010601     C                   if        *in31 = *off and
078600010601     C                             *in32 = *off
078700990910     C                   eval      vinflg = '1'
078800990910     C                   else
078900990910     C                   eval      vinflg = '2'
079000990910     C                   endif
079100000905     C                   endif
079200000905     C                   endif
079300010601     C*
079400010601     C                   endif
079500000905     C*
079600000905     C                   else
079700000905     C                   eval      vinflg = '1'
079800000905     C                   endif
079900000905     C*
080000000905     C  N70              update    tivin000
080100000905     C*
080200991022     C  N70              ENDdo
080300010601     C
080400010601     C*
080500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
080600010601     C                   if        cntNonEl = *zeros or
080700010601     C                             flgMulti = '0'
080800990910     C* Se non ci sono record con errori ...
080900010601     C                   if        §ctrno = 0 and
081000010601     C                             §ctrmo = 0 and
081100010601     C                             flgStato <> '2'
081200990910     C* ... restituisco esito OK.
081300990921     C                   eval      wrkesito = '0'
081400990910     C                   else
081500010601     C                   if        §ctrok > 0
081600000710     C                   eval      wrkesito = '1'
081700000710     C                   else
081800010615     C                   if        flgOk = '0'
081900010615     C                   eval      wrkesito = '2'
082000010615     C                   else
082100010615     C                   eval      wrkesito = '6'
082200010615     C                   endif
082300000710     C                   endif
082400010615     C                   endif
082500010601     C                   else
082600010601     C                   eval      wrkesito = '9'
082700010601     C                   endif
082800990910     C*
082900990914     C                   if        %open(tivin00r)
083000990908     C                   close     tivin00r
083100990914     C                   endif
083200090108     C                   if        %open(edivabwr)
083300090108     C                   close     edivabwr
083400990914     C                   endif
083500010601     C*
083600010601     C                   if        vlrpoi <> 999
083700010601     C                   eval      invfil = vlrpoi
083800010601     C                   endif
083900990910     C*
084000990920     C                   if        §ctrok > 0
084100010601     C                             and invfil > *zeros
084200990920     C                   exsr      invio
084300990920     C                   endif
084400010612     C*
084500010612     C                   if        flgGiro = '1'
084600010612     C                   exsr      endela
084700010612     C                   endif
084800990920     C*
084900910830     C                   ENDSR
085000000613     C***
085100990920
085200010601
085300010601
085400010601
085500010601
085600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
085700010601     C     repfil        BEGSR
085800010601     C*
085900010601     C                   if        invfil = *zeros and
086000010601     C                             depfil > *zeros and
086100010613     C                             (vinflg = *blanks or
086200010613     C                              vinflg = *zeros)
086300010601     C
086400010601     C                   eval      invfil = depfil
086500010601     C                   endif
086600010601     C*
086700010601     C                   if        depfil <> invfil and
086800010601     C                             invfil > *zeros
086900010601     C                   eval      flgMulti = '1'
087000010601     C                   if        vinflg = *blanks
087100010601     C                   add       1             cntNonEl
087200010601     C                   endif
087300010601     C                   endif
087400010601     C*
087500010601     C                   if        vinflg = '2'
087600010601     C                   eval      flgStato = '2'
087700010601     C                   endif
087800010601     C*
087900010601     C                   ENDSR
088000010601     C***
088100010601
088200010601
088300010601
088400010601
088500010601
088600010601
088700010601
088800990920      /TITLE Invio dei dati al punto operativo.
088900000613     C     invio         BEGSR
089000990920     C*
089100990920     C                   reset                   dscmz
089200010601     C                   move      invfil        cmzdst
089300990920     C                   eval      cmzfld = vlrfou
089400990920     C                   eval      cmzmbd = vlrhdl
089500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
089600000710     C                   if        prmfir = *blanks
089700090108     C                   eval      cmzfla = 'EDIVAB0F'
089800090108     C                   eval      cmzmba = 'EDIVAB0F'
089900000710     C                   else
090000000710     C                   eval      cmzfla = prmfir
090100000710     C                   eval      cmzmba = prmfir
090200000710     C                   endif
090300990920     C                   eval      cmznrr = *zeros
090400990920     C                   move      §ctrok        cmznrr
090500021003     C                   eval      cmzlba = vlrfl1
090600990920     C                   call(e)   'TIS711C'
090700990920     C                   parm                    dscmz
090800990921     C                   parm      *blanks       esito
090900990920     C                   if        %error
091000990920     C                             or cmzerr = '1'
091100990921     C                             or esito  = '1'
091200000710     C                   eval      wrkesito = '3'
091300990920     C                   endif
091400990920     C*
091500000613     C                   ENDSR
091600010601
091700010601
091800010601
091900010601
092000010601      /TITLE Invio dei dati al punto operativo.
092100010601     C     opeini        BEGSR
092200010601     C*
092300010601     C* Inizializzo flag e contatori operativi
092400010601     C                   movel     '0'           flgGiro           1
092500010601     C                   movel     '0'           flgMulti          1
092600010601     C                   movel     '1'           flgStato          1
092700010615     C                   movel     '0'           flgOk             1
092800010601     C                   z-add     *zeros        cntNonEl         10 0
092900010601     C                   z-add     *zeros        depfil            3 0
093000010601     C                   z-add     *zeros        invfil            3 0
093100030407     C*
093200030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
093300030407     C                   if        %subst(vlrppt:1:1) = 'S'
093400030407     C                   seton                                        50
093500030407     C                   else
093600030407     C                   setoff                                       50
093700030407     C                   endif
093800041001     C*
093900041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
094000041001     C                   if        %subst(vlrppt:2:1) = 'S'
094100041001     C                   seton                                        51
094200041001     C                   else
094300041001     C                   setoff                                       51
094400041001     C                   endif
094500041025     C*
094600041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
094700041025     C* (naturalmente solo se nn già presente)
094800041025     C                   if        %subst(vlrppt:3:1) = 'S'
094900041025     C                   seton                                        52
095000041025     C                   else
095100041025     C                   setoff                                       52
095200041025     C                   endif
095300010601     C*
095400010601     C                   ENDSR
095500010601     C***
095600040630
095700040630
095800040630
095900040630
096000040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
096100040630     C     exeBon        BEGSR
096200040630     C*
096300040630     C* Imposto i parametri x il *pgm d "bonifica"
096400040630     C                   EVAL      ElencoChar = *blanks
096500040630     C                   EVAL      TipoElenco = *blanks
096600040630     C                   EVAL      CharSost   = *blanks
096700040630     C                   EVAL      UpperCase  = '1'
096800040630     C                   EVAL      ChkNull    = '1'
096900040630     C                   EVAL      CharNull   = *blanks
097000040630     C*
097100040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
097200040630     C                   EVAL      TxtInOut   = VABRSD
097300040630     C                   EVAL      EsitoBon   = *blanks
097400040630     C*
097500040630     C                   CALL      'XCHKCHAR'
097600040630     C                   PARM                    TxtInOut
097700040630     C                   PARM                    ElencoChar
097800040630     C                   PARM                    TipoElenco
097900040630     C                   PARM                    CharSost
098000040630     C                   PARM                    UpperCase
098100040630     C                   PARM                    ChkNull
098200040630     C                   PARM                    CharNull
098300040630     C                   PARM                    EsitoBon
098400040630     C*
098500040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
098600040630     C                   IF        EsitoBon = '1'
098700040630     C                   EVAL      VABRSD = TxtInOut
098800040630     C                   ENDIF
098900040630     C*
099000040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
099100040630     C                   EVAL      TxtInOut   = VABIND
099200040630     C                   EVAL      EsitoBon   = *blanks
099300040630     C*
099400040630     C                   CALL      'XCHKCHAR'
099500040630     C                   PARM                    TxtInOut
099600040630     C                   PARM                    ElencoChar
099700040630     C                   PARM                    TipoElenco
099800040630     C                   PARM                    CharSost
099900040630     C                   PARM                    UpperCase
100000040630     C                   PARM                    ChkNull
100100040630     C                   PARM                    CharNull
100200040630     C                   PARM                    EsitoBon
100300040630     C*
100400040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100500040630     C                   IF        EsitoBon = '1'
100600040630     C                   EVAL      VABIND = TxtInOut
100700040630     C                   ENDIF
100800040630     C*
100900040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
101000040630     C                   EVAL      TxtInOut   = VABLOD
101100040630     C                   EVAL      EsitoBon   = *blanks
101200040630     C*
101300040630     C                   CALL      'XCHKCHAR'
101400040630     C                   PARM                    TxtInOut
101500040630     C                   PARM                    ElencoChar
101600040630     C                   PARM                    TipoElenco
101700040630     C                   PARM                    CharSost
101800040630     C                   PARM                    UpperCase
101900040630     C                   PARM                    ChkNull
102000040630     C                   PARM                    CharNull
102100040630     C                   PARM                    EsitoBon
102200040630     C*
102300040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
102400040630     C                   IF        EsitoBon = '1'
102500040630     C                   EVAL      VABLOD = TxtInOut
102600040630     C                   ENDIF
102700040630     C*
102800040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
102900040630     C                   EVAL      TxtInOut   = VABNOT
103000040630     C                   EVAL      EsitoBon   = *blanks
103100040630     C                   EVAL      UpperCase  = *blanks
103200040630     C*
103300040630     C                   CALL      'XCHKCHAR'
103400040630     C                   PARM                    TxtInOut
103500040630     C                   PARM                    ElencoChar
103600040630     C                   PARM                    TipoElenco
103700040630     C                   PARM                    CharSost
103800040630     C                   PARM                    UpperCase
103900040630     C                   PARM                    ChkNull
104000040630     C                   PARM                    CharNull
104100040630     C                   PARM                    EsitoBon
104200040630     C*
104300040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
104400040630     C                   IF        EsitoBon = '1'
104500040630     C                   EVAL      VABNOT = TxtInOut
104600040630     C                   ENDIF
104700040630     C*
104800040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
104900040630     C                   EVAL      TxtInOut   = VABNT2
105000040630     C                   EVAL      EsitoBon   = *blanks
105100040630     C                   EVAL      UpperCase  = *blanks
105200040630     C*
105300040630     C                   CALL      'XCHKCHAR'
105400040630     C                   PARM                    TxtInOut
105500040630     C                   PARM                    ElencoChar
105600040630     C                   PARM                    TipoElenco
105700040630     C                   PARM                    CharSost
105800040630     C                   PARM                    UpperCase
105900040630     C                   PARM                    ChkNull
106000040630     C                   PARM                    CharNull
106100040630     C                   PARM                    EsitoBon
106200040630     C*
106300040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
106400040630     C                   IF        EsitoBon = '1'
106500040630     C                   EVAL      VABNT2 = TxtInOut
106600040630     C                   ENDIF
106700040630     C*
106800040630     C                   ENDSR
106900040630
107000010601
107100010601
107200990910
107300000613     C     *inzsr        BEGSR
107400990910     C*
107500990910     C     *entry        plist
107600990920     C                   parm                    tivlrds
107700990921     C                   parm      wrkesito      esito
107800000724     C                   parm                    prmlit
107900000710     C                   parm                    prmfir
108000010606     C*
108100010606     C* CALCOLA LA DATA CORRENTE
108200010606     C                   time                    wn14             14 0
108300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
108400090108     C                   movel     wn14          oracor            6 0          *ORA
108500010606     C                   z-add     wn8           g08dat
108600010606     C                   z-add     *zeros        g08inv
108700010606     C                   movel     '0'           g08err
108800010606     C                   call      'XSRDA8'
108900010606     C                   parm                    wlbda8
109000010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
109100000613     C*
109200000613     C                   ENDSR
109300000613     C***
