000100060112      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500060112     FEDIVABwr  O    E             DISK    usropn
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
004100070326     D vhbldv                 18     28
004200000313     D vhbcbo                 29     30
004300000313     D vhblna                 31     33
004400000313     D vhbrsd                 34     68
004500000313     D vhbrd2                 69    103
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
017500060112     C* RWVAB   LEGGE tivin00r E SCRIVE EDIVABWF              *
017600910830     C*--------------------------------------------------------
017700941229     C     RWVAB         BEGSR
017800990910     C*
017900990914     C                   if        not %open(tivin00r)
018000990908     C                   open      tivin00r
018100990914     C                   endif
018200060112     C                   if        not %open(edivabwr)
018300060112     C                   open      edivabwr
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
019900051017     C                   if        vindta  > *blanks
020000000613     C                   add       1             rrnum
020100990913     C*
020200010601     C                   if        *in70 = *off and
020300990913     C                             (vinflg = *blanks
020400010711     C                              or vinflg = '0'
020500010711     C                              or vinflg = '2')
020600990913     C*
020700060112     C                   clear                   EDIVAB00
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
028600070215     C*                  TESTN                   VHBMGS               30
028700070215     C* N30              TESTN                   VHBMGS                 30
028800070215     C* N30              TESTN                   VHBMGS                   30
028900070215     C*                  MOVE      VHBMGS        W001A             1
029000070215     C*    *IN30         IFEQ      *OFF
029100070215     C*    W001A         ORLT      '0'
029200070215     C*    W001A         ANDNE     *BLANK
029300070215     C*                  SETON                                        32
029400070215     C*                  MOVE      '0101'        VABMGS
029500070215     C*                  eval      vinmsg = %trimr(vinmsg)
029600070215     C*                            + ' ' + 'VHBMGS'
029700070215     C*                  ELSE
029800070215     C*                  MOVE      VHBMGS        VABMGS
029900070215     C*                  ENDIF
030000971021     C*
030100070215     C*                  TESTN                   VHBNRS               30
030200070215     C* N30              TESTN                   VHBNRS                 30
030300070215     C* N30              TESTN                   VHBNRS                   30
030400070215     C*                  MOVE      VHBNRS        W001A             1
030500070215     C*    *IN30         IFEQ      *OFF
030600070215     C*    W001A         ORLT      '0'
030700070215     C*    W001A         ANDNE     *BLANK
030800070215     C*                  SETON                                        32
030900070215     C*                  MOVE      *ZERO         VABNRS
031000070215     C*                  eval      vinmsg = %trimr(vinmsg)
031100070215     C*                            + ' ' + 'VHBNRS'
031200070215     C*                  ELSE
031300070215     C*                  MOVE      VHBNRS        VABNRS
031400070215     C*                  ENDIF
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
040600051130     C                   EVAL      PiStr=VHBPKB
040700051130     C                   EXSR      CHKNUM
040800051130     C                   IF        PiNum=*on
040900051130     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 decim.
041000051130     C                   Z-ADD(H)  PiVal         VABPKB
041100051130     C                   ELSE
041200971021     C                   SETON                                        32
041300051130     C                   Z-ADD     *ZERO         VABPKB
041400010604     C                   eval      vinmsg = %trimr(vinmsg)
041500010604     C                             + ' ' + 'VHBPKB'
041600050118     C                   ENDIF
041700971021     C*
041800041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
041900041001     C                   IF        *IN51 = *OFF
042000971021     C                   MOVE      *ZERO         VABVLB
042100971021     C                   MOVE      VHBVLB        WASC07
042200050503     C                   IF        WI07 = *blanks
042300050503     C                   EVAL      WI07 = *zeros
042400050503     C                   ENDIF
042500971023     C                   TESTN                   WI07                 30
042600971023     C  N30              TESTN                   WI07                   30
042700971023     C  N30              TESTN                   WI07                     30
042800971021     C                   MOVE      WI07          W001A             1
042900971021     C     *IN30         IFEQ      *OFF
043000971021     C     W001A         ORLT      '0'
043100971023     C     W001A         ANDNE     *BLANK
043200971021     C                   SETON                                        32
043300971021     C                   MOVE      *ZERO         VABVLB
043400010604     C                   eval      vinmsg = %trimr(vinmsg)
043500010604     C                             + ' ' + 'VHBVLB'
043600971021     C                   ELSE
043700971021     C                   MOVEL     WI07          VABVLB
043800971023     C                   TESTN                   WD07                 30
043900971023     C  N30              TESTN                   WD07                   30
044000971023     C  N30              TESTN                   WD07                     30
044100971021     C                   MOVE      WD07          W001A             1
044200971021     C     *IN30         IFEQ      *OFF
044300971021     C     W001A         ORLT      '0'
044400971023     C     W001A         ANDNE     *BLANK
044500971021     C                   SETON                                        32
044600971021     C                   MOVE      *ZERO         VABVLB
044700010604     C                   eval      vinmsg = %trimr(vinmsg)
044800010604     C                             + ' ' + 'VHBVLB'
044900971021     C                   ELSE
045000971021     C                   MOVE      WD07          VABVLB
045100971021     C                   ENDIF
045200050118     C                   ENDIF
045300041001     C                   ELSE
045400041001     C* ...altrimenti forzo azzeramento del volume
045500041001     C                   Z-ADD     *zeros        VABVLB
045600041001     C                   ENDIF
045700941229     C*
045800971021     C                   MOVE      *ZERO         VABQFT
045900971021     C                   MOVE      VHBQFT        WASC15
046000971023     C                   TESTN                   WI15                 30
046100971023     C  N30              TESTN                   WI15                   30
046200971023     C  N30              TESTN                   WI15                     30
046300971021     C                   MOVE      WI15          W001A             1
046400971021     C     *IN30         IFEQ      *OFF
046500971021     C     W001A         ORLT      '0'
046600971023     C     W001A         ANDNE     *BLANK
046700971021     C                   SETON                                        32
046800971021     C                   MOVE      *ZERO         VABQFT
046900010604     C                   eval      vinmsg = %trimr(vinmsg)
047000010604     C                             + ' ' + 'VHBQFT'
047100971021     C                   ELSE
047200971021     C                   MOVEL     WI15          VABQFT
047300971023     C                   TESTN                   WD15                 30
047400971023     C  N30              TESTN                   WD15                   30
047500971023     C  N30              TESTN                   WD15                     30
047600971021     C                   MOVE      WD15          W001A             1
047700971021     C     *IN30         IFEQ      *OFF
047800971021     C     W001A         ORLT      '0'
047900971023     C     W001A         ANDNE     *BLANK
048000971021     C                   SETON                                        32
048100971021     C                   MOVE      *ZERO         VABQFT
048200010604     C                   eval      vinmsg = %trimr(vinmsg)
048300010604     C                             + ' ' + 'VHBQFT'
048400971021     C                   ELSE
048500971021     C                   MOVE      WD15          VABQFT
048600971021     C                   ENDIF
048700050118     C                   ENDIF
048800941229     C*
048900971021     C                   MOVE      *ZERO         VABCAS
049000971021     C                   MOVE      VHBCAS        WASC15
049100971023     C                   TESTN                   WI15                 30
049200971023     C  N30              TESTN                   WI15                   30
049300971023     C  N30              TESTN                   WI15                     30
049400971021     C                   MOVE      WI15          W001A             1
049500971021     C     *IN30         IFEQ      *OFF
049600971021     C     W001A         ORLT      '0'
049700971023     C     W001A         ANDNE     *BLANK
049800971021     C                   SETON                                        32
049900971021     C                   MOVE      *ZERO         VABCAS
050000010604     C                   eval      vinmsg = %trimr(vinmsg)
050100010604     C                             + ' ' + 'VHBCAS'
050200971021     C                   ELSE
050300971021     C                   MOVEL     WI15          VABCAS
050400971023     C                   TESTN                   WD15                 30
050500971023     C  N30              TESTN                   WD15                   30
050600971023     C  N30              TESTN                   WD15                     30
050700971021     C                   MOVE      WD15          W001A             1
050800971021     C     *IN30         IFEQ      *OFF
050900971021     C     W001A         ORLT      '0'
051000971023     C     W001A         ANDNE     *BLANK
051100971021     C                   SETON                                        32
051200971021     C                   MOVE      *ZERO         VABCAS
051300010604     C                   eval      vinmsg = %trimr(vinmsg)
051400010604     C                             + ' ' + 'VHBCAS'
051500971021     C                   ELSE
051600971021     C                   MOVE      WD15          VABCAS
051700971021     C                   ENDIF
051800040827     C                   ENDIF
051900971021     C*
052000971023     C                   TESTN                   VHBRMN               30
052100971023     C  N30              TESTN                   VHBRMN                 30
052200971023     C  N30              TESTN                   VHBRMN                   30
052300971021     C                   MOVE      VHBRMN        W001A             1
052400971021     C     *IN30         IFEQ      *OFF
052500971021     C     W001A         ORLT      '0'
052600971021     C                   SETON                                        32
052700971021     C                   MOVE      *ZERO         VABRMN
052800010604     C                   eval      vinmsg = %trimr(vinmsg)
052900010604     C                             + ' ' + 'VHBRMN'
053000971021     C                   ELSE
053100971021     C                   MOVE      VHBRMN        VABRMN
053200971021     C                   ENDIF
053300971021     C*
053400971023     C                   TESTN                   VHBNCD               30
053500971023     C  N30              TESTN                   VHBNCD                 30
053600971023     C  N30              TESTN                   VHBNCD                   30
053700971021     C                   MOVE      VHBNCD        W001A             1
053800971021     C     *IN30         IFEQ      *OFF
053900971021     C     W001A         ORLT      '0'
054000971023     C     W001A         ANDNE     *BLANK
054100971021     C                   SETON                                        32
054200971021     C                   MOVE      *ZERO         VABNCD
054300010604     C                   eval      vinmsg = %trimr(vinmsg)
054400010604     C                             + ' ' + 'VHBNCD'
054500971021     C                   ELSE
054600971021     C                   MOVE      VHBNCD        VABNCD
054700971021     C                   ENDIF
054800971021     C*
054900971023     C                   TESTN                   VHBNCA               30
055000971023     C  N30              TESTN                   VHBNCA                 30
055100971023     C  N30              TESTN                   VHBNCA                   30
055200971021     C                   MOVE      VHBNCA        W001A             1
055300971021     C     *IN30         IFEQ      *OFF
055400971021     C     W001A         ORLT      '0'
055500971023     C     W001A         ANDNE     *BLANK
055600971021     C                   SETON                                        32
055700971021     C                   MOVE      *ZERO         VABNCA
055800010604     C                   eval      vinmsg = %trimr(vinmsg)
055900010604     C                             + ' ' + 'VHBNCA'
056000971021     C                   ELSE
056100971021     C                   MOVE      VHBNCA        VABNCA
056200971021     C                   ENDIF
056300971021     C*
056400971023     C                   TESTN                   VHBZNC               30
056500971023     C  N30              TESTN                   VHBZNC                 30
056600971023     C  N30              TESTN                   VHBZNC                   30
056700971021     C                   MOVE      VHBZNC        W001A             1
056800971021     C     *IN30         IFEQ      *OFF
056900971021     C     W001A         ORLT      '0'
057000971023     C     W001A         ANDNE     *BLANK
057100971021     C                   SETON                                        32
057200971021     C                   MOVE      *ZERO         VABZNC
057300010604     C                   eval      vinmsg = %trimr(vinmsg)
057400010604     C                             + ' ' + 'VHBZNC'
057500971021     C                   ELSE
057600971021     C                   MOVE      VHBZNC        VABZNC
057700971021     C                   ENDIF
057800971021     C*
057900971023     C                   TESTN                   VHBDCR               30
058000971023     C  N30              TESTN                   VHBDCR                 30
058100971023     C  N30              TESTN                   VHBDCR                   30
058200971021     C                   MOVE      VHBDCR        W001A             1
058300971021     C     *IN30         IFEQ      *OFF
058400971021     C     W001A         ORLT      '0'
058500971023     C     W001A         ANDNE     *BLANK
058600971021     C                   SETON                                        32
058700971021     C                   MOVE      *ZERO         VABDCR
058800010604     C                   eval      vinmsg = %trimr(vinmsg)
058900010604     C                             + ' ' + 'VHBDCR'
059000971021     C                   ELSE
059100971021     C                   MOVE      VHBDCR        VABDCR
059200971021     C                   ENDIF
059300971021     C*
059400971023     C                   TESTN                   VHBHCR               30
059500971023     C  N30              TESTN                   VHBHCR                 30
059600971023     C  N30              TESTN                   VHBHCR                   30
059700971021     C                   MOVE      VHBHCR        W001A             1
059800971021     C     *IN30         IFEQ      *OFF
059900971021     C     W001A         ORLT      '0'
060000971023     C     W001A         ANDNE     *BLANK
060100971021     C                   SETON                                        32
060200971021     C                   MOVE      *ZERO         VABHCR
060300010604     C                   eval      vinmsg = %trimr(vinmsg)
060400010604     C                             + ' ' + 'VHBHCR'
060500971021     C                   ELSE
060600971021     C                   MOVE      VHBHCR        VABHCR
060700971021     C                   ENDIF
060800941229     C*
060900971021     C                   MOVE      *ZERO         VABVMD
061000971021     C                   MOVE      VHBVMD        WASC15
061100971023     C                   TESTN                   WI15                 30
061200971023     C  N30              TESTN                   WI15                   30
061300971023     C  N30              TESTN                   WI15                     30
061400971021     C                   MOVE      WI15          W001A             1
061500971021     C     *IN30         IFEQ      *OFF
061600971021     C     W001A         ORLT      '0'
061700971023     C     W001A         ANDNE     *BLANK
061800971021     C                   SETON                                        32
061900971021     C                   MOVE      *ZERO         VABVMD
062000010604     C                   eval      vinmsg = %trimr(vinmsg)
062100010604     C                             + ' ' + 'VHBVMD'
062200971021     C                   ELSE
062300971021     C                   MOVEL     WI15          VABVMD
062400971023     C                   TESTN                   WD15                 30
062500971023     C  N30              TESTN                   WD15                   30
062600971023     C  N30              TESTN                   WD15                     30
062700971021     C                   MOVE      WD15          W001A             1
062800971021     C     *IN30         IFEQ      *OFF
062900971021     C     W001A         ORLT      '0'
063000971023     C     W001A         ANDNE     *BLANK
063100971021     C                   SETON                                        32
063200971021     C                   MOVE      *ZERO         VABVMD
063300010604     C                   eval      vinmsg = %trimr(vinmsg)
063400010604     C                             + ' ' + 'VHBVMD'
063500971021     C                   ELSE
063600971021     C                   MOVE      WD15          VABVMD
063700971021     C                   ENDIF
063800050118     C                   ENDIF
063900971021     C*
064000971023     C                   TESTN                   VHBANT               30
064100971023     C  N30              TESTN                   VHBANT                 30
064200971023     C  N30              TESTN                   VHBANT                   30
064300971021     C                   MOVE      VHBANT        W001A             1
064400971021     C     *IN30         IFEQ      *OFF
064500971021     C     W001A         ORLT      '0'
064600971023     C     W001A         ANDNE     *BLANK
064700971021     C                   SETON                                        32
064800971021     C                   MOVE      *ZERO         VABANT
064900010604     C                   eval      vinmsg = %trimr(vinmsg)
065000010604     C                             + ' ' + 'VHBANT'
065100971021     C                   ELSE
065200971021     C                   MOVE      VHBANT        VABANT
065300971021     C                   ENDIF
065400971021     C*
065500971021     C* CAMPI ALFABETICI:
065600971021     C                   MOVE      VHBATB        VABATB
065700971021     C                   MOVE      VHBCBO        VABCBO
065800971021     C                   MOVE      VHBRSD        VABRSD
065900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
066000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
066100020117     C* ==
066200971021     C                   MOVE      VHBRD2        VABRD2
066300971021     C                   MOVE      VHBIND        VABIND
066400971021     C                   MOVE      VHBCAD        VABCAD
066500971021     C                   MOVE      VHBLOD        VABLOD
066600971021     C                   MOVE      VHBPRD        VABPRD
066700971021     C                   MOVE      VHBNZD        VABNZD
066800971021     C                   MOVE      VHBGC1        VABGC1
066900971021     C                   MOVE      VHBGC2        VABGC2
067000971021     C                   MOVE      VHBTSP        VABTSP
067100971021     C                   MOVE      VHBVAS        VABVAS
067200971021     C                   MOVE      VHBNAS        VABNAS
067300971021     C                   MOVE      VHBTIC        VABTIC
067400971021     C                   MOVE      VHBVCA        VABVCA
067500971021     C                   MOVE      VHBGCA        VABGCA
067600971021     C                   MOVE      VHBRMA        VABRMA
067700971021     C                   MOVE      VHBXCO        VABXCO
067800971021     C                   MOVE      VHBNOT        VABNOT
067900971021     C                   MOVE      VHBNT2        VABNT2
068000971021     C                   MOVE      VHBCTM        VABCTM
068100070419     C*
068200070419     C                   IF        VABCTM = *blanks
068300070419     C                   EVAL      VABCTM = '7T'
068400070419     C                   ENDIF
068500070419     C*
068600971021     C                   MOVE      VHBFFD        VABFFD
068700971021     C                   MOVE      VHBTCR        VABTCR
068800971021     C                   MOVE      VHBCTS        VABCTS
068900971021     C                   MOVE      VHBFTM        VABFTM
069000971021     C                   MOVE      VHBVAD        VABVAD
069100971021     C                   MOVE      VHBGMA        VABGMA
069200971021     C                   MOVE      VHBGGA        VABGGA
069300971021     C                   MOVE      VHBGVA        VABGVA
069400971021     C                   MOVE      VHBTC1        VABTC1
069500971021     C                   MOVE      VHBTC2        VABTC2
069600971021     C                   MOVE      VHBSCL        VABSCL
069700971021     C                   MOVE      VHBRMO        VABRMO
069800971021     C                   MOVE      VHBCMO        VABCMO
069900971021     C                   MOVE      VHBNMO        VABNMO
070000971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
070100971021     C* NELLA NAZIONE ORIGINALE
070200971021     C                   MOVE      VHBNMO        WA1D              1
070300971021     C                   MOVEL     VHBNMO        WA1S              1
070400971021     C     WA1D          IFNE      *BLANK
070500971021     C     WA1S          ANDEQ     *BLANK
070600971021     C                   MOVE      *BLANK        VABNMO
070700971021     C                   END
070800050418     C*
070900050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
071000050418     C                   IF        %trim(VABNZD) = 'I'   OR
071100050418     C                             %trim(VABNZD) = 'IT'  OR
071200050418     C                             %trim(VABNZD) = 'ITA'
071300050418     C                   EVAL      VABNZD = *blanks
071400050418     C                   ENDIF
071500070214     C*
071600070214     C* Se richiesto nei parametri determinato codice trattamento merce gestisco
071700070214     C                   if        %subst(vlrppt:4:2) <> *blanks
071800070214     C                   eval      vabctm = %subst(vlrppt:4:2)
071900070214     C                   endif
072000041025     C*
072100041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
072200041025     C                   IF        *IN52 = *ON
072300041025     C* Reperisco la provincia dal CAP e dalla località
072400041025     C                   IF        VABCAD <> *blanks AND
072500050907     C                             VABPRD  = *blanks AND
072600050907     C                             VABNZD  = *blanks
072700041025     C                   CLEAR                   TISI95DS
072800041025     C                   EVAL      I95TCN = '3'
072900041025     C                   Z-ADD     datcor        I95DAT
073000041025     C                   EVAL      I95CAP = VABCAD
073100041025     C                   EVAL      I95LOC = VABLOD
073200041025     C                   CALL      'TISI95R'
073300041025     C                   PARM                    TISI95DS
073400041025     C                   EVAL      VABPRD = O95PRV
073500041025     C                   ENDIF
073600041025     C                   ENDIF
073700040630     C*
073800040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
073900040630     C                   exsr      exeBon
074000060112     C*
074100060112     C* Valorizzo i campi relativi al CMR
074200070417     C                   EVAL      VABCMR = %trim(vhbldv)
074300060112     C                   EVAL      VABDCM = datcor
074400060112     C                   EVAL      VABDTS = datcor
074500060112     C                   EVAL      VABHMS = oracor
074600060112     C                   EVAL      VABCNT = 1
074700930409     C*
074800010604     C  N31              ADD       1             §CTROK            7 0
074900010604     C   32              ADD       1             §CTRMO            7 0
075000010604     C   31              ADD       1             §CTRNO            7 0
075100060112     C  N31              WRITE     EDIVAB00
075200990910     C*
075300010601     C                   if        *in31 = *off and
075400010601     C                             *in32 = *off
075500990910     C                   eval      vinflg = '1'
075600990910     C                   else
075700990910     C                   eval      vinflg = '2'
075800990910     C                   endif
075900000905     C                   endif
076000000905     C                   endif
076100010601     C*
076200010601     C                   endif
076300000905     C*
076400000905     C                   else
076500000905     C                   eval      vinflg = '1'
076600000905     C                   endif
076700000905     C*
076800000905     C  N70              update    tivin000
076900000905     C*
077000991022     C  N70              ENDdo
077100010601     C
077200010601     C*
077300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
077400010601     C                   if        cntNonEl = *zeros or
077500010601     C                             flgMulti = '0'
077600990910     C* Se non ci sono record con errori ...
077700010601     C                   if        §ctrno = 0 and
077800010601     C                             §ctrmo = 0 and
077900010601     C                             flgStato <> '2'
078000990910     C* ... restituisco esito OK.
078100990921     C                   eval      wrkesito = '0'
078200990910     C                   else
078300010601     C                   if        §ctrok > 0
078400000710     C                   eval      wrkesito = '1'
078500000710     C                   else
078600010615     C                   if        flgOk = '0'
078700010615     C                   eval      wrkesito = '2'
078800010615     C                   else
078900010615     C                   eval      wrkesito = '6'
079000010615     C                   endif
079100000710     C                   endif
079200010615     C                   endif
079300010601     C                   else
079400010601     C                   eval      wrkesito = '9'
079500010601     C                   endif
079600990910     C*
079700990914     C                   if        %open(tivin00r)
079800990908     C                   close     tivin00r
079900990914     C                   endif
080000060112     C                   if        %open(edivabwr)
080100060112     C                   close     edivabwr
080200990914     C                   endif
080300010601     C*
080400010601     C                   if        vlrpoi <> 999
080500010601     C                   eval      invfil = vlrpoi
080600010601     C                   endif
080700990910     C*
080800990920     C                   if        §ctrok > 0
080900010601     C                             and invfil > *zeros
081000990920     C                   exsr      invio
081100990920     C                   endif
081200010612     C*
081300010612     C                   if        flgGiro = '1'
081400010612     C                   exsr      endela
081500010612     C                   endif
081600990920     C*
081700910830     C                   ENDSR
081800000613     C***
081900051130
082000051130
082100051130     C*----------------------------------------------------*
082200051130     C*  CONTROLLO NUMERICITA' CAMPI
082300051130     C*----------------------------------------------------*
082400051130     C     CHKNUM        BEGSR
082500051130     C*
082600051130     C                   call(e)   'ISNUMERIC'
082700051130     C                   PARM                    PiStr            30
082800051130     C                   PARM      ','           PiDecChr          1
082900051130     C                   PARM      *ZEROS        PiVal            30 9
083000051130     C                   PARM      '0'           PiInt             1
083100051130     C                   PARM      '0'           PiNum             1
083200051130     C                   IF        %error
083300051130     C                   EVAL      PiInt=*off
083400051130     C                   ENDIF
083500051130     C*
083600051130     C                   ENDSR
083700051130     C***
083800990920
083900010601
084000010601
084100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
084200010601     C     repfil        BEGSR
084300010601     C*
084400010601     C                   if        invfil = *zeros and
084500010601     C                             depfil > *zeros and
084600010613     C                             (vinflg = *blanks or
084700010613     C                              vinflg = *zeros)
084800010601     C
084900010601     C                   eval      invfil = depfil
085000010601     C                   endif
085100010601     C*
085200010601     C                   if        depfil <> invfil and
085300010601     C                             invfil > *zeros
085400010601     C                   eval      flgMulti = '1'
085500010601     C                   if        vinflg = *blanks
085600010601     C                   add       1             cntNonEl
085700010601     C                   endif
085800010601     C                   endif
085900010601     C*
086000010601     C                   if        vinflg = '2'
086100010601     C                   eval      flgStato = '2'
086200010601     C                   endif
086300010601     C*
086400010601     C                   ENDSR
086500010601     C***
086600010601
086700010601
086800010601
086900010601
087000010601
087100010601
087200010601
087300990920      /TITLE Invio dei dati al punto operativo.
087400000613     C     invio         BEGSR
087500990920     C*
087600990920     C                   reset                   dscmz
087700010601     C                   move      invfil        cmzdst
087800990920     C                   eval      cmzfld = vlrfou
087900990920     C                   eval      cmzmbd = vlrhdl
088000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
088100000710     C                   if        prmfir = *blanks
088200060112     C                   eval      cmzfla = 'EDIVAB0F'
088300060112     C                   eval      cmzmba = 'EDIVAB0F'
088400000710     C                   else
088500000710     C                   eval      cmzfla = prmfir
088600000710     C                   eval      cmzmba = prmfir
088700000710     C                   endif
088800990920     C                   eval      cmznrr = *zeros
088900990920     C                   move      §ctrok        cmznrr
089000021003     C                   eval      cmzlba = vlrfl1
089100990920     C                   call(e)   'TIS711C'
089200990920     C                   parm                    dscmz
089300990921     C                   parm      *blanks       esito
089400990920     C                   if        %error
089500990920     C                             or cmzerr = '1'
089600990921     C                             or esito  = '1'
089700000710     C                   eval      wrkesito = '3'
089800990920     C                   endif
089900990920     C*
090000000613     C                   ENDSR
090100010601
090200010601
090300010601
090400010601
090500010601      /TITLE Invio dei dati al punto operativo.
090600010601     C     opeini        BEGSR
090700010601     C*
090800010601     C* Inizializzo flag e contatori operativi
090900010601     C                   movel     '0'           flgGiro           1
091000010601     C                   movel     '0'           flgMulti          1
091100010601     C                   movel     '1'           flgStato          1
091200010615     C                   movel     '0'           flgOk             1
091300010601     C                   z-add     *zeros        cntNonEl         10 0
091400010601     C                   z-add     *zeros        depfil            3 0
091500010601     C                   z-add     *zeros        invfil            3 0
091600030407     C*
091700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
091800030407     C                   if        %subst(vlrppt:1:1) = 'S'
091900030407     C                   seton                                        50
092000030407     C                   else
092100030407     C                   setoff                                       50
092200030407     C                   endif
092300041001     C*
092400041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
092500041001     C                   if        %subst(vlrppt:2:1) = 'S'
092600041001     C                   seton                                        51
092700041001     C                   else
092800041001     C                   setoff                                       51
092900041001     C                   endif
093000041025     C*
093100041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
093200041025     C* (naturalmente solo se nn già presente)
093300041025     C                   if        %subst(vlrppt:3:1) = 'S'
093400041025     C                   seton                                        52
093500041025     C                   else
093600041025     C                   setoff                                       52
093700041025     C                   endif
093800010601     C*
093900010601     C                   ENDSR
094000010601     C***
094100040630
094200040630
094300040630
094400040630
094500040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
094600040630     C     exeBon        BEGSR
094700040630     C*
094800040630     C* Imposto i parametri x il *pgm d "bonifica"
094900040630     C                   EVAL      ElencoChar = *blanks
095000040630     C                   EVAL      TipoElenco = *blanks
095100040630     C                   EVAL      CharSost   = *blanks
095200040630     C                   EVAL      UpperCase  = '1'
095300040630     C                   EVAL      ChkNull    = '1'
095400040630     C                   EVAL      CharNull   = *blanks
095500040630     C*
095600040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
095700040630     C                   EVAL      TxtInOut   = VABRSD
095800040630     C                   EVAL      EsitoBon   = *blanks
095900040630     C*
096000040630     C                   CALL      'XCHKCHAR'
096100040630     C                   PARM                    TxtInOut
096200040630     C                   PARM                    ElencoChar
096300040630     C                   PARM                    TipoElenco
096400040630     C                   PARM                    CharSost
096500040630     C                   PARM                    UpperCase
096600040630     C                   PARM                    ChkNull
096700040630     C                   PARM                    CharNull
096800040630     C                   PARM                    EsitoBon
096900040630     C*
097000040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
097100040630     C                   IF        EsitoBon = '1'
097200040630     C                   EVAL      VABRSD = TxtInOut
097300040630     C                   ENDIF
097400040630     C*
097500040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
097600040630     C                   EVAL      TxtInOut   = VABIND
097700040630     C                   EVAL      EsitoBon   = *blanks
097800040630     C*
097900040630     C                   CALL      'XCHKCHAR'
098000040630     C                   PARM                    TxtInOut
098100040630     C                   PARM                    ElencoChar
098200040630     C                   PARM                    TipoElenco
098300040630     C                   PARM                    CharSost
098400040630     C                   PARM                    UpperCase
098500040630     C                   PARM                    ChkNull
098600040630     C                   PARM                    CharNull
098700040630     C                   PARM                    EsitoBon
098800040630     C*
098900040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
099000040630     C                   IF        EsitoBon = '1'
099100040630     C                   EVAL      VABIND = TxtInOut
099200040630     C                   ENDIF
099300040630     C*
099400040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
099500040630     C                   EVAL      TxtInOut   = VABLOD
099600040630     C                   EVAL      EsitoBon   = *blanks
099700040630     C*
099800040630     C                   CALL      'XCHKCHAR'
099900040630     C                   PARM                    TxtInOut
100000040630     C                   PARM                    ElencoChar
100100040630     C                   PARM                    TipoElenco
100200040630     C                   PARM                    CharSost
100300040630     C                   PARM                    UpperCase
100400040630     C                   PARM                    ChkNull
100500040630     C                   PARM                    CharNull
100600040630     C                   PARM                    EsitoBon
100700040630     C*
100800040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100900040630     C                   IF        EsitoBon = '1'
101000040630     C                   EVAL      VABLOD = TxtInOut
101100040630     C                   ENDIF
101200040630     C*
101300040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
101400040630     C                   EVAL      TxtInOut   = VABNOT
101500040630     C                   EVAL      EsitoBon   = *blanks
101600040630     C                   EVAL      UpperCase  = *blanks
101700040630     C*
101800040630     C                   CALL      'XCHKCHAR'
101900040630     C                   PARM                    TxtInOut
102000040630     C                   PARM                    ElencoChar
102100040630     C                   PARM                    TipoElenco
102200040630     C                   PARM                    CharSost
102300040630     C                   PARM                    UpperCase
102400040630     C                   PARM                    ChkNull
102500040630     C                   PARM                    CharNull
102600040630     C                   PARM                    EsitoBon
102700040630     C*
102800040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
102900040630     C                   IF        EsitoBon = '1'
103000040630     C                   EVAL      VABNOT = TxtInOut
103100040630     C                   ENDIF
103200040630     C*
103300040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
103400040630     C                   EVAL      TxtInOut   = VABNT2
103500040630     C                   EVAL      EsitoBon   = *blanks
103600040630     C                   EVAL      UpperCase  = *blanks
103700040630     C*
103800040630     C                   CALL      'XCHKCHAR'
103900040630     C                   PARM                    TxtInOut
104000040630     C                   PARM                    ElencoChar
104100040630     C                   PARM                    TipoElenco
104200040630     C                   PARM                    CharSost
104300040630     C                   PARM                    UpperCase
104400040630     C                   PARM                    ChkNull
104500040630     C                   PARM                    CharNull
104600040630     C                   PARM                    EsitoBon
104700040630     C*
104800040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
104900040630     C                   IF        EsitoBon = '1'
105000040630     C                   EVAL      VABNT2 = TxtInOut
105100040630     C                   ENDIF
105200040630     C*
105300040630     C                   ENDSR
105400040630
105500010601
105600010601
105700990910
105800000613     C     *inzsr        BEGSR
105900990910     C*
106000990910     C     *entry        plist
106100990920     C                   parm                    tivlrds
106200990921     C                   parm      wrkesito      esito
106300000724     C                   parm                    prmlit
106400000710     C                   parm                    prmfir
106500010606     C*
106600010606     C* CALCOLA LA DATA CORRENTE
106700010606     C                   time                    wn14             14 0
106800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
106900060112     C                   movel     wn14          oracor            6 0          *ORA  (8) IN HH/MM/S
107000010606     C                   z-add     wn8           g08dat
107100010606     C                   z-add     *zeros        g08inv
107200010606     C                   movel     '0'           g08err
107300010606     C                   call      'XSRDA8'
107400010606     C                   parm                    wlbda8
107500010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
107600000613     C*
107700000613     C                   ENDSR
107800000613     C***
