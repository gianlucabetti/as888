000100020913      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020913     FFIVABwwr  O    E             DISK    usropn
000600990908
000700000512     D*------------
000800990920     D dscmz         e ds                  inz
000900990910     D psds           sds
001000990910     D  procname         *PROC
001100941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001200941229     D* 10+3
001300941229     D                 DS
001400941229     D  WS15                   1      1
001500941229     D  WI15                   2     11
001600941229     D  WV15                  12     12
001700941229     D  WD15                  13     15
001800941229     D  WASC15                 1     15
001900941229     D*  6+1
002000941229     D                 DS
002100941229     D  WS09                   1      1
002200941229     D  WI09                   2      7
002300941229     D  WV09                   8      8
002400941229     D  WD09                   9      9
002500941229     D  WASC09                 1      9
002600941229     D*  2+3
002700941229     D                 DS
002800941229     D  WS07                   1      1
002900941229     D  WI07                   2      3
003000941229     D  WV07                   4      4
003100941229     D  WD07                   5      7
003200941229     D  WASC07                 1      7
003300990920     D tivlrds       e ds                  extname(tivlr00f)
003400041025     D tisi95ds      e ds
003500990910     D esito           s              1
003600000724     D prmlit          s             10
003700000710     D prmfir          s             10
003800990921     D wrkesito        s                   like(esito)
003900000613     D rrnum           s              6  0 INZ(*zeros)
004000060410     D wVHBCAS         s                   LIKE(VHBCAS) INZ(*blanks)
004100010606     D*------------------
004200010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300010606     D*------------------
004400010606     D WLBDA8          DS                  INZ
004500010606     D  G08DAT                 1      8  0
004600010606     D  G08INV                 9     16  0
004700010606     D  G08ERR                17     17
004800010606     D  G08TGI                18     22  0
004900010606     D*
005000040407     D TxtInOut        S           2048
005100040407     D ElencoChar      S            256
005200040407     D TipoElenco      S              1
005300040407     D CharSost        S              1
005400040407     D UpperCase       S              1
005500040407     D ChkNull         S              1
005600040407     D CharNull        S              1
005700040407     D EsitoBon        S              1
005800091214
005900091214
006000091214     D*------------------
006100091214     D* DS FIVAB00F
006200091214     D*------------------
006300091214     D fnvabaar        ds                  INZ
006400100128     D   VHBATB                1      1
006500100128     D   VHBCCM                3      9
006600100128     D   VHBLNP               11     13
006700100128     D   VHBAAS               15     18
006800100128     D   VHBMGS               20     23
006900100128     D   VHBNRS               25     26
007000100128     D   VHBNSP               28     34
007100100128     D   VHBCBO               35     36
007200100128     D   VHBLNA               39     41
007300100128     D   VHBRSD               43     77
007400100128     D   VHBRD2               79    113
007500100128     D   VHBIND              115    149
007600100128     D   VHBCAD              151    160
007700100128     D   VHBLOD              161    195
007800100128     D   VHBPRD              197    198
007900100128     D   VHBNZD              200    202
008000100128     D   VHBGC1              204    205
008100100128     D   VHBGC2              207    208
008200100128     D   VHBCTR              210    212
008300100128     D   VHBTSP              214    214
008400100128     D   VHBIAS              216    229
008500100128     D   VHBVAS              231    233
008600100128     D   VHBNAS              235    250
008700100128     D   VHBNCL              251    255
008800100128     D   VHBPKB              257    264
008900100128     D   VHBVLB              266    271
009000100128     D   VHBQFT              273    286
009100100128     D   VHBCAS              288    301
009200100128     D   VHBTIC              303    304
009300100128     D   VHBVCA              306    308
009400100128     D   VHBGCA              310    311
009500100128     D   VHBRMN              313    327
009600100128     D   VHBRMA              329    343
009700100128     D   VHBNCD              345    351
009800100128     D   VHBNCA              353    359
009900100128     D   VHBXCO              361    361
010000100128     D   VHBNOT              363    397
010100100128     D   VHBNT2              399    433
010200100128     D   VHBZNC              435    436
010300100128     D   VHBCTM              438    439
010400100128     D   VHBFFD              441    441
010500100128     D   VHBDCR              443    450
010600100128     D   VHBTCR              452    452
010700100128     D   VHBHCR              454    457
010800100128     D   VHBCTS              459    460
010900100128     D   VHBFTM              462    462
011000100128     D   VHBVMD              464    477
011100100128     D   VHBVAD              479    481
011200100128     D   VHBGMA              483    484
011300100128     D   VHBGGA              486    487
011400100128     D   VHBGVA              489    490
011500100128     D   VHBTC1              492    492
011600100128     D   VHBTC2              494    494
011700100128     D   VHBSCL              496    496
011800100128     D   VHBANT              498    506
011900100128     D   VHBRMO              508    532
012000100128     D   VHBCMO              534    542
012100100128     D   VHBNMO              544    546
012200100128
012300100128
012400100128
012500000913     C                   reset                   rrnum
012600990921     C                   reset                   esito
012700990921     C                   reset                   wrkesito
012800010530     C*
012900010530     C                   exsr      opeini
013000010530     C                   exsr      rwvab
013100041025     C*
013200041025     C* Esegue lancio TISI95R solo x chiusura
013300041025     C                   CLEAR                   TISI95DS
013400041025     C                   EVAL      I95TLA = 'C'
013500041025     C                   CALL      'TISI95R'
013600041025     C                   PARM                    TISI95DS
013700010530     C*
013800010530     C                   seton                                        lr
013900010530
014000010530
014100010530
014200010530
014300010530     C*--------------------------------------------------------
014400010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
014500010530     C*--------------------------------------------------------
014600010530     C     PREELA        BEGSR
014700000724     C*
014800000724     C* SE OCCORRE SPEDIRE IN FILIALE
014900010530     C                   if        invfil <> *zeros and
015000010530     C                             flgGiro = '0'
015100010530     C*
015200010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
015300010530     C                   eval      flgGiro = '1'
015400010530     C*
015500010530     C                   endif
015600010530     C*
015700010530     C                   ENDSR
015800010530     C***
015900010530
016000010530
016100010530
016200010530
016300010530
016400010530
016500010530     C*--------------------------------------------------------
016600010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016700010530     C*--------------------------------------------------------
016800010530     C     ENDELA        BEGSR
016900010530     C*
017000010530     C                   ENDSR
017100010530     C***
017200990908
017300010530
017400010530
017500010530
017600910830     C*--------------------------------------------------------
017700020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
017800910830     C*--------------------------------------------------------
017900941229     C     RWVAB         BEGSR
018000990910     C*
018100990914     C                   if        not %open(tivin00r)
018200990908     C                   open      tivin00r
018300990914     C                   endif
018400020913     C                   if        not %open(fivabwwr)
018500020913     C                   open      fivabwwr
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
020000010705     C*
020100010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
020200010705     C                   if        %subst(vindta:1:1) = '='
020300010705     C                   eval      %subst(vindta:1:1) = *blanks
020400010705     C                   endif
020500010705     C*
020600051017     C                   if        vindta  > *blanks
020700000613     C                   add       1             rrnum
020800010601     C* ...elimino eventuali segni
020900010601     C     '+':' '       xlate     vindta        vindta
021000990913     C*
021100010601     C                   if        *in70 = *off and
021200990913     C                             (vinflg = *blanks
021300010711     C                              or vinflg = '0'
021400010711     C                              or vinflg = '2')
021500990913     C*
021600020913     C                   clear                   FIVAB000
021700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021800010711     C                   if        vinflg = *blanks or vinflg = '0'
021900010711     C                   clear                   vinmsg
022000010711     C                   endif
022100010711     C*
022200990913     C                   eval      fnvabaar = vindta
022300991022     C*
022400991022     C                   testb     x'3F'         vhbatb                   02
022500991022     C                   if        not *in02
022600991022     C*
022700971021     C                   SETOFF                                       303132
022800971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
022900971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
023000971023     C*    OPPURE TUTTI BLANKS
023100010530     C*
023200010530     C                   TESTN                   VHBLNP               30
023300010530     C  N30              TESTN                   VHBLNP                 30
023400010530     C  N30              TESTN                   VHBLNP                   30
023500010530     C                   MOVE      VHBLNP        W001A             1
023600010530     C     *IN30         IFEQ      *OFF
023700010530     C     W001A         ORLT      '0'
023800010530     C                   SETON                                        31
023900010530     C                   eval      vinmsg = %trimr(vinmsg)
024000010530     C                             + ' ' + 'VHBLNP'
024100010530     C                   ELSE
024200010530     C                   MOVE      VHBLNP        VABLNP
024300010530     C     VABLNP        IFLE      *ZEROS
024400010530     C                   SETON                                        31
024500010530     C                   eval      vinmsg = %trimr(vinmsg)
024600010530     C                             + ' ' + 'VHBLNP'
024700010530     C                   ENDIF
024800010530     C                   ENDIF
024900010530     C*
025000010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025100010530     C                   eval      depfil = VABLNP
025200010530     C                   exsr      repfil
025300010601     C                   if        depfil = invfil
025400021014     C                   if        vlrpoi = 999
025500021010     C                   MOVE(P)   invfil        VABFGS
025600021014     C                   else
025700021014     C                   MOVE(P)   vlrpoi        VABFGS
025800021014     C                   endif
025900010530     C*
026000010530     C                   exsr      PREELA
026100010530     C*
026200971023     C                   TESTN                   VHBCCM               30
026300971023     C  N30              TESTN                   VHBCCM                 30
026400971023     C  N30              TESTN                   VHBCCM                   30
026500971021     C                   MOVE      VHBCCM        W001A             1
026600971021     C     *IN30         IFEQ      *OFF
026700971021     C     W001A         ORLT      '0'
026800971021     C                   SETON                                        31
026900010604     C                   eval      vinmsg = %trimr(vinmsg)
027000010604     C                             + ' ' + 'VHBCCM'
027100971021     C                   ELSE
027200971021     C                   MOVE      VHBCCM        VABCCM
027300971024     C     VABCCM        IFLE      *ZEROS
027400971024     C                   SETON                                        31
027500010604     C                   eval      vinmsg = %trimr(vinmsg)
027600010604     C                             + ' ' + 'VHBCCM'
027700971024     C                   ENDIF
027800971021     C                   ENDIF
027900971021     C*
028000971023     C                   TESTN                   VHBAAS               30
028100971023     C  N30              TESTN                   VHBAAS                 30
028200971023     C  N30              TESTN                   VHBAAS                   30
028300971021     C                   MOVE      VHBAAS        W001A             1
028400971021     C     *IN30         IFEQ      *OFF
028500971021     C     W001A         ORLT      '0'
028600971023     C     W001A         ANDNE     *BLANK
028700971021     C                   SETON                                        32
028800010606     C                   MOVEL     datcor        VABAAS
028900010604     C                   eval      vinmsg = %trimr(vinmsg)
029000010604     C                             + ' ' + 'VHBAAS'
029100971021     C                   ELSE
029200971021     C                   MOVE      VHBAAS        VABAAS
029300971021     C                   ENDIF
029400971021     C*
029500971023     C                   TESTN                   VHBMGS               30
029600971023     C  N30              TESTN                   VHBMGS                 30
029700971023     C  N30              TESTN                   VHBMGS                   30
029800971021     C                   MOVE      VHBMGS        W001A             1
029900971021     C     *IN30         IFEQ      *OFF
030000971021     C     W001A         ORLT      '0'
030100971023     C     W001A         ANDNE     *BLANK
030200971021     C                   SETON                                        32
030300010601     C                   MOVE      '0101'        VABMGS
030400010604     C                   eval      vinmsg = %trimr(vinmsg)
030500010604     C                             + ' ' + 'VHBMGS'
030600971021     C                   ELSE
030700971021     C                   MOVE      VHBMGS        VABMGS
030800971021     C                   ENDIF
030900971021     C*
031000971023     C                   TESTN                   VHBNRS               30
031100971023     C  N30              TESTN                   VHBNRS                 30
031200971023     C  N30              TESTN                   VHBNRS                   30
031300971021     C                   MOVE      VHBNRS        W001A             1
031400971021     C     *IN30         IFEQ      *OFF
031500971021     C     W001A         ORLT      '0'
031600971023     C     W001A         ANDNE     *BLANK
031700971021     C                   SETON                                        32
031800971021     C                   MOVE      *ZERO         VABNRS
031900010604     C                   eval      vinmsg = %trimr(vinmsg)
032000010604     C                             + ' ' + 'VHBNRS'
032100971021     C                   ELSE
032200971021     C                   MOVE      VHBNRS        VABNRS
032300971021     C                   ENDIF
032400971021     C*
032500971023     C                   TESTN                   VHBNSP               30
032600971023     C  N30              TESTN                   VHBNSP                 30
032700971023     C  N30              TESTN                   VHBNSP                   30
032800971021     C                   MOVE      VHBNSP        W001A             1
032900971021     C     *IN30         IFEQ      *OFF
033000971021     C     W001A         ORLT      '0'
033100971023     C     W001A         ANDNE     *BLANK
033200971021     C                   SETON                                        32
033300971021     C                   MOVE      *ZERO         VABNSP
033400010604     C                   eval      vinmsg = %trimr(vinmsg)
033500010604     C                             + ' ' + 'VHBNSP'
033600971021     C                   ELSE
033700971021     C                   MOVE      VHBNSP        VABNSP
033800971021     C                   ENDIF
033900971021     C*
034000971023     C                   TESTN                   VHBLNA               30
034100971023     C  N30              TESTN                   VHBLNA                 30
034200971023     C  N30              TESTN                   VHBLNA                   30
034300971021     C                   MOVE      VHBLNA        W001A             1
034400971021     C     *IN30         IFEQ      *OFF
034500971021     C     W001A         ORLT      '0'
034600971023     C     W001A         ANDNE     *BLANK
034700971021     C                   SETON                                        32
034800971021     C                   MOVE      *ZERO         VABLNA
034900010604     C                   eval      vinmsg = %trimr(vinmsg)
035000010604     C                             + ' ' + 'VHBLNA'
035100971021     C                   ELSE
035200971021     C                   MOVE      VHBLNA        VABLNA
035300971021     C                   ENDIF
035400971021     C*
035500971023     C                   TESTN                   VHBCTR               30
035600971023     C  N30              TESTN                   VHBCTR                 30
035700971023     C  N30              TESTN                   VHBCTR                   30
035800971021     C                   MOVE      VHBCTR        W001A             1
035900971021     C     *IN30         IFEQ      *OFF
036000971021     C     W001A         ORLT      '0'
036100971023     C     W001A         ANDNE     *BLANK
036200971021     C                   SETON                                        32
036300971021     C                   MOVE      *ZERO         VABCTR
036400010604     C                   eval      vinmsg = %trimr(vinmsg)
036500010604     C                             + ' ' + 'VHBCTR'
036600971021     C                   ELSE
036700971021     C                   MOVE      VHBCTR        VABCTR
036800971021     C                   ENDIF
036900941229     C*
037000971021     C                   MOVE      *ZERO         VABIAS
037100941229     C                   MOVE      VHBIAS        WASC15
037200971023     C                   TESTN                   WI15                 30
037300971023     C  N30              TESTN                   WI15                   30
037400971023     C  N30              TESTN                   WI15                     30
037500971021     C                   MOVE      WI15          W001A             1
037600971021     C     *IN30         IFEQ      *OFF
037700971021     C     W001A         ORLT      '0'
037800971023     C     W001A         ANDNE     *BLANK
037900971021     C                   SETON                                        32
038000971021     C                   MOVE      *ZERO         VABIAS
038100010604     C                   eval      vinmsg = %trimr(vinmsg)
038200010604     C                             + ' ' + 'VHBIAS'
038300971021     C                   ELSE
038400971021     C                   MOVEL     WI15          VABIAS
038500971023     C                   TESTN                   WD15                 30
038600971023     C  N30              TESTN                   WD15                   30
038700971023     C  N30              TESTN                   WD15                     30
038800971021     C                   MOVE      WD15          W001A             1
038900971021     C     *IN30         IFEQ      *OFF
039000971021     C     W001A         ORLT      '0'
039100971023     C     W001A         ANDNE     *BLANK
039200971021     C                   SETON                                        32
039300971021     C                   MOVE      *ZERO         VABIAS
039400010604     C                   eval      vinmsg = %trimr(vinmsg)
039500010604     C                             + ' ' + 'VHBIAS'
039600971021     C                   ELSE
039700971021     C                   MOVE      WD15          VABIAS
039800971021     C                   ENDIF
039900050118     C                   ENDIF
040000941229     C*
040100971023     C                   TESTN                   VHBNCL               30
040200971023     C  N30              TESTN                   VHBNCL                 30
040300971023     C  N30              TESTN                   VHBNCL                   30
040400971021     C                   MOVE      VHBNCL        W001A             1
040500971021     C     *IN30         IFEQ      *OFF
040600971021     C     W001A         ORLT      '0'
040700971021     C                   SETON                                        32
040800971021     C                   MOVE      *ZERO         VABNCL
040900010604     C                   eval      vinmsg = %trimr(vinmsg)
041000010604     C                             + ' ' + 'VHBNCL'
041100971021     C                   ELSE
041200971021     C                   MOVE      VHBNCL        VABNCL
041300971021     C                   ENDIF
041400941229     C*
041500971021     C                   MOVE      *ZERO         VABPKB
041600971021     C                   MOVE      VHBPKB        WASC09
041700010719     C                   IF        WI09 = *blanks
041800010719     C                   EVAL      WI09 = *zeros
041900010719     C                   ENDIF
042000971023     C                   TESTN                   WI09                 30
042100971023     C  N30              TESTN                   WI09                   30
042200971023     C  N30              TESTN                   WI09                     30
042300971021     C                   MOVE      WI09          W001A             1
042400971021     C     *IN30         IFEQ      *OFF
042500971021     C     W001A         ORLT      '0'
042600971021     C                   SETON                                        32
042700971021     C                   MOVE      *ZERO         VABPKB
042800010604     C                   eval      vinmsg = %trimr(vinmsg)
042900010604     C                             + ' ' + 'VHBPKB'
043000971021     C                   ELSE
043100971021     C                   MOVEL     WI09          VABPKB
043200010719     C                   IF        WD09 = *blanks
043300010719     C                   EVAL      WD09 = *zeros
043400010719     C                   ENDIF
043500971023     C                   TESTN                   WD09                 30
043600971023     C  N30              TESTN                   WD09                     30
043700971021     C                   MOVE      WD09          W001A             1
043800971021     C     *IN30         IFEQ      *OFF
043900971021     C     W001A         ORLT      '0'
044000971023     C     W001A         ANDNE     *BLANK
044100971021     C                   SETON                                        32
044200971021     C                   MOVE      *ZERO         VABPKB
044300010604     C                   eval      vinmsg = %trimr(vinmsg)
044400010604     C                             + ' ' + 'VHBPKB'
044500971021     C                   ELSE
044600971021     C                   MOVE      WD09          VABPKB
044700971021     C                   ENDIF
044800050118     C                   ENDIF
044900971021     C*
045000041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
045100041001     C                   IF        *IN51 = *OFF
045200971021     C                   MOVE      *ZERO         VABVLB
045300971021     C                   MOVE      VHBVLB        WASC07
045400010719     C                   IF        WI07 = *blanks
045500010719     C                   EVAL      WI07 = *zeros
045600010719     C                   ENDIF
045700971023     C                   TESTN                   WI07                 30
045800971023     C  N30              TESTN                   WI07                   30
045900971023     C  N30              TESTN                   WI07                     30
046000971021     C                   MOVE      WI07          W001A             1
046100971021     C     *IN30         IFEQ      *OFF
046200971021     C     W001A         ORLT      '0'
046300971023     C     W001A         ANDNE     *BLANK
046400971021     C                   SETON                                        32
046500971021     C                   MOVE      *ZERO         VABVLB
046600010604     C                   eval      vinmsg = %trimr(vinmsg)
046700010604     C                             + ' ' + 'VHBVLB'
046800971021     C                   ELSE
046900971021     C                   MOVEL     WI07          VABVLB
047000010719     C                   IF        WD07 = *blanks
047100010719     C                   EVAL      WD07 = *zeros
047200010719     C                   ENDIF
047300971023     C                   TESTN                   WD07                 30
047400971023     C  N30              TESTN                   WD07                   30
047500971023     C  N30              TESTN                   WD07                     30
047600971021     C                   MOVE      WD07          W001A             1
047700971021     C     *IN30         IFEQ      *OFF
047800971021     C     W001A         ORLT      '0'
047900971023     C     W001A         ANDNE     *BLANK
048000971021     C                   SETON                                        32
048100971021     C                   MOVE      *ZERO         VABVLB
048200010604     C                   eval      vinmsg = %trimr(vinmsg)
048300010604     C                             + ' ' + 'VHBVLB'
048400971021     C                   ELSE
048500971021     C                   MOVE      WD07          VABVLB
048600971021     C                   ENDIF
048700050118     C                   ENDIF
048800041001     C                   ELSE
048900041001     C* ...altrimenti forzo azzeramento del volume
049000041001     C                   Z-ADD     *zeros        VABVLB
049100041001     C                   ENDIF
049200941229     C*
049300971021     C                   MOVE      *ZERO         VABQFT
049400971021     C                   MOVE      VHBQFT        WASC15
049500010719     C                   IF        WI15 = *blanks
049600010719     C                   EVAL      WI15 = *zeros
049700010719     C                   ENDIF
049800971023     C                   TESTN                   WI15                 30
049900971023     C  N30              TESTN                   WI15                   30
050000971023     C  N30              TESTN                   WI15                     30
050100971021     C                   MOVE      WI15          W001A             1
050200971021     C     *IN30         IFEQ      *OFF
050300971021     C     W001A         ORLT      '0'
050400971023     C     W001A         ANDNE     *BLANK
050500971021     C                   SETON                                        32
050600971021     C                   MOVE      *ZERO         VABQFT
050700010604     C                   eval      vinmsg = %trimr(vinmsg)
050800010604     C                             + ' ' + 'VHBQFT'
050900971021     C                   ELSE
051000971021     C                   MOVEL     WI15          VABQFT
051100010719     C                   IF        WD15 = *blanks
051200010719     C                   EVAL      WD15 = *zeros
051300010719     C                   ENDIF
051400971023     C                   TESTN                   WD15                 30
051500971023     C  N30              TESTN                   WD15                   30
051600971023     C  N30              TESTN                   WD15                     30
051700971021     C                   MOVE      WD15          W001A             1
051800971021     C     *IN30         IFEQ      *OFF
051900971021     C     W001A         ORLT      '0'
052000971023     C     W001A         ANDNE     *BLANK
052100971021     C                   SETON                                        32
052200971021     C                   MOVE      *ZERO         VABQFT
052300010604     C                   eval      vinmsg = %trimr(vinmsg)
052400010604     C                             + ' ' + 'VHBQFT'
052500971021     C                   ELSE
052600971021     C                   MOVE      WD15          VABQFT
052700971021     C                   ENDIF
052800050118     C                   ENDIF
052900941229     C*
053000971021     C                   MOVE      *ZERO         VABCAS
053100060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
053200060410     C                   EVAL      wVHBCAS = VHBCAS
053300060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
053400060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
053500060410     C                   ENDIF
053600060410     C*
053700060410     C                   MOVEL(P)  wVHBCAS       PiStr
053800060410     C                   EXSR      CHKNUM
053900060410     C                   IF        PiNum=*on
054000060410     C                   Z-ADD     PiVal         VABCAS
054100060410     C                   ELSE
054200060410     C                   SETON                                        32
054300060410     C                   Z-ADD     *zeros        VABCAS
054400060410     C                   EVAL      vinmsg = %trimr(vinmsg)
054500060410     C                             + ' ' + 'VHBCAS'
054600060410     C                   ENDIF
054700971021     C*
054800971023     C                   TESTN                   VHBRMN               30
054900971023     C  N30              TESTN                   VHBRMN                 30
055000971023     C  N30              TESTN                   VHBRMN                   30
055100971021     C                   MOVE      VHBRMN        W001A             1
055200971021     C     *IN30         IFEQ      *OFF
055300971021     C     W001A         ORLT      '0'
055400971021     C                   SETON                                        32
055500971021     C                   MOVE      *ZERO         VABRMN
055600010604     C                   eval      vinmsg = %trimr(vinmsg)
055700010604     C                             + ' ' + 'VHBRMN'
055800971021     C                   ELSE
055900971021     C                   MOVE      VHBRMN        VABRMN
056000971021     C                   ENDIF
056100971021     C*
056200971023     C                   TESTN                   VHBNCD               30
056300971023     C  N30              TESTN                   VHBNCD                 30
056400971023     C  N30              TESTN                   VHBNCD                   30
056500971021     C                   MOVE      VHBNCD        W001A             1
056600971021     C     *IN30         IFEQ      *OFF
056700971021     C     W001A         ORLT      '0'
056800971023     C     W001A         ANDNE     *BLANK
056900971021     C                   SETON                                        32
057000971021     C                   MOVE      *ZERO         VABNCD
057100010604     C                   eval      vinmsg = %trimr(vinmsg)
057200010604     C                             + ' ' + 'VHBNCD'
057300971021     C                   ELSE
057400971021     C                   MOVE      VHBNCD        VABNCD
057500971021     C                   ENDIF
057600971021     C*
057700971023     C                   TESTN                   VHBNCA               30
057800971023     C  N30              TESTN                   VHBNCA                 30
057900971023     C  N30              TESTN                   VHBNCA                   30
058000971021     C                   MOVE      VHBNCA        W001A             1
058100971021     C     *IN30         IFEQ      *OFF
058200971021     C     W001A         ORLT      '0'
058300971023     C     W001A         ANDNE     *BLANK
058400971021     C                   SETON                                        32
058500971021     C                   MOVE      *ZERO         VABNCA
058600010604     C                   eval      vinmsg = %trimr(vinmsg)
058700010604     C                             + ' ' + 'VHBNCA'
058800971021     C                   ELSE
058900971021     C                   MOVE      VHBNCA        VABNCA
059000971021     C                   ENDIF
059100971021     C*
059200971023     C                   TESTN                   VHBZNC               30
059300971023     C  N30              TESTN                   VHBZNC                 30
059400971023     C  N30              TESTN                   VHBZNC                   30
059500971021     C                   MOVE      VHBZNC        W001A             1
059600971021     C     *IN30         IFEQ      *OFF
059700971021     C     W001A         ORLT      '0'
059800971023     C     W001A         ANDNE     *BLANK
059900971021     C                   SETON                                        32
060000971021     C                   MOVE      *ZERO         VABZNC
060100010604     C                   eval      vinmsg = %trimr(vinmsg)
060200010604     C                             + ' ' + 'VHBZNC'
060300971021     C                   ELSE
060400971021     C                   MOVE      VHBZNC        VABZNC
060500971021     C                   ENDIF
060600971021     C*
060700971023     C                   TESTN                   VHBDCR               30
060800971023     C  N30              TESTN                   VHBDCR                 30
060900971023     C  N30              TESTN                   VHBDCR                   30
061000971021     C                   MOVE      VHBDCR        W001A             1
061100971021     C     *IN30         IFEQ      *OFF
061200971021     C     W001A         ORLT      '0'
061300971023     C     W001A         ANDNE     *BLANK
061400971021     C                   SETON                                        32
061500971021     C                   MOVE      *ZERO         VABDCR
061600010604     C                   eval      vinmsg = %trimr(vinmsg)
061700010604     C                             + ' ' + 'VHBDCR'
061800971021     C                   ELSE
061900971021     C                   MOVE      VHBDCR        VABDCR
062000971021     C                   ENDIF
062100971021     C*
062200971023     C                   TESTN                   VHBHCR               30
062300971023     C  N30              TESTN                   VHBHCR                 30
062400971023     C  N30              TESTN                   VHBHCR                   30
062500971021     C                   MOVE      VHBHCR        W001A             1
062600971021     C     *IN30         IFEQ      *OFF
062700971021     C     W001A         ORLT      '0'
062800971023     C     W001A         ANDNE     *BLANK
062900971021     C                   SETON                                        32
063000971021     C                   MOVE      *ZERO         VABHCR
063100010604     C                   eval      vinmsg = %trimr(vinmsg)
063200010604     C                             + ' ' + 'VHBHCR'
063300971021     C                   ELSE
063400971021     C                   MOVE      VHBHCR        VABHCR
063500971021     C                   ENDIF
063600941229     C*
063700971021     C                   MOVE      *ZERO         VABVMD
063800971021     C                   MOVE      VHBVMD        WASC15
063900010719     C                   IF        WI15 = *blanks
064000010719     C                   EVAL      WI15 = *zeros
064100010719     C                   ENDIF
064200971023     C                   TESTN                   WI15                 30
064300971023     C  N30              TESTN                   WI15                   30
064400971023     C  N30              TESTN                   WI15                     30
064500971021     C                   MOVE      WI15          W001A             1
064600971021     C     *IN30         IFEQ      *OFF
064700971021     C     W001A         ORLT      '0'
064800971023     C     W001A         ANDNE     *BLANK
064900971021     C                   SETON                                        32
065000971021     C                   MOVE      *ZERO         VABVMD
065100010604     C                   eval      vinmsg = %trimr(vinmsg)
065200010604     C                             + ' ' + 'VHBVMD'
065300971021     C                   ELSE
065400971021     C                   MOVEL     WI15          VABVMD
065500010719     C                   IF        WD15 = *blanks
065600010719     C                   EVAL      WD15 = *zeros
065700010719     C                   ENDIF
065800971023     C                   TESTN                   WD15                 30
065900971023     C  N30              TESTN                   WD15                   30
066000971023     C  N30              TESTN                   WD15                     30
066100971021     C                   MOVE      WD15          W001A             1
066200971021     C     *IN30         IFEQ      *OFF
066300971021     C     W001A         ORLT      '0'
066400971023     C     W001A         ANDNE     *BLANK
066500971021     C                   SETON                                        32
066600971021     C                   MOVE      *ZERO         VABVMD
066700010604     C                   eval      vinmsg = %trimr(vinmsg)
066800010604     C                             + ' ' + 'VHBVMD'
066900971021     C                   ELSE
067000971021     C                   MOVE      WD15          VABVMD
067100971021     C                   ENDIF
067200050118     C                   ENDIF
067300971021     C*
067400971023     C                   TESTN                   VHBANT               30
067500971023     C  N30              TESTN                   VHBANT                 30
067600971023     C  N30              TESTN                   VHBANT                   30
067700971021     C                   MOVE      VHBANT        W001A             1
067800971021     C     *IN30         IFEQ      *OFF
067900971021     C     W001A         ORLT      '0'
068000971023     C     W001A         ANDNE     *BLANK
068100971021     C                   SETON                                        32
068200971021     C                   MOVE      *ZERO         VABANT
068300010604     C                   eval      vinmsg = %trimr(vinmsg)
068400010604     C                             + ' ' + 'VHBANT'
068500971021     C                   ELSE
068600971021     C                   MOVE      VHBANT        VABANT
068700971021     C                   ENDIF
068800971021     C*
068900971021     C* CAMPI ALFABETICI:
069000971021     C                   MOVE      VHBATB        VABATB
069100971021     C                   MOVE      VHBCBO        VABCBO
069200971021     C                   MOVE      VHBRSD        VABRSD
069300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
069400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
069500020117     C* ==
069600100128     C                   MOVEL     VHBRD2        VABRD2
069700100128     C                   MOVEL     VHBIND        VABIND
069800100128     C                   MOVEL     VHBCAD        VABCAD
069900100128     C                   MOVEL     VHBLOD        VABLOD
070000100128     C                   MOVEL     VHBPRD        VABPRD
070100100128     C                   MOVEL     VHBNZD        VABNZD
070200100128     C                   MOVEL     VHBGC1        VABGC1
070300100128     C                   MOVEL     VHBGC2        VABGC2
070400100128     C                   MOVEL     VHBTSP        VABTSP
070500100128     C                   MOVEL     VHBVAS        VABVAS
070600100128     C                   MOVEL     VHBNAS        VABNAS
070700100128     C                   MOVEL     VHBTIC        VABTIC
070800100128     C                   MOVEL     VHBVCA        VABVCA
070900100128     C                   MOVEL     VHBGCA        VABGCA
071000100128     C                   MOVEL     VHBRMA        VABRMA
071100100128     C                   MOVEL     VHBXCO        VABXCO
071200100128     C                   MOVEL     VHBNOT        VABNOT
071300100128     C                   MOVEL     VHBNT2        VABNT2
071400100128     C                   MOVEL     VHBCTM        VABCTM
071500100128     C                   MOVEL     VHBFFD        VABFFD
071600100128     C                   MOVEL     VHBTCR        VABTCR
071700100128     C                   MOVEL     VHBCTS        VABCTS
071800100128     C                   MOVEL     VHBFTM        VABFTM
071900100128     C                   MOVEL     VHBVAD        VABVAD
072000100128     C                   MOVEL     VHBGMA        VABGMA
072100100128     C                   MOVEL     VHBGGA        VABGGA
072200100128     C                   MOVEL     VHBGVA        VABGVA
072300100128     C                   MOVEL     VHBTC1        VABTC1
072400100128     C                   MOVEL     VHBTC2        VABTC2
072500100128     C                   MOVEL     VHBSCL        VABSCL
072600100128     C                   MOVEL     VHBRMO        VABRMO
072700100128     C                   MOVEL     VHBCMO        VABCMO
072800100128     C                   MOVEL     VHBNMO        VABNMO
072900971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
073000060329     C* NELLA NAZIONE MITTENTE ORIGINALE
073100060331     C                   IF        %trim(VHBNMO) < 'A'
073200060329     C                   MOVE      *BLANKS       VABNMO
073300060329     C                   ENDIF
073400050418     C*
073500050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
073600050418     C                   IF        %trim(VABNZD) = 'I'   OR
073700050418     C                             %trim(VABNZD) = 'IT'  OR
073800050418     C                             %trim(VABNZD) = 'ITA'
073900050418     C                   EVAL      VABNZD = *blanks
074000050418     C                   ENDIF
074100041025     C*
074200041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
074300041025     C                   IF        *IN52 = *ON
074400041025     C* Reperisco la provincia dal CAP e dalla località
074500041025     C                   IF        VABCAD <> *blanks AND
074600050907     C                             VABPRD  = *blanks AND
074700050907     C                             VABNZD  = *blanks
074800041025     C                   CLEAR                   TISI95DS
074900041025     C                   EVAL      I95TCN = '3'
075000041025     C                   Z-ADD     datcor        I95DAT
075100041025     C                   EVAL      I95CAP = VABCAD
075200041025     C                   EVAL      I95LOC = VABLOD
075300041025     C                   CALL      'TISI95R'
075400041025     C                   PARM                    TISI95DS
075500041025     C                   EVAL      VABPRD = O95PRV
075600041025     C                   ENDIF
075700041025     C                   ENDIF
075800040630     C*
075900040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
076000040630     C                   exsr      exeBon
076100930409     C*
076200010604     C  N31              ADD       1             §CTROK            7 0
076300010604     C   32              ADD       1             §CTRMO            7 0
076400010604     C   31              ADD       1             §CTRNO            7 0
076500020913     C  N31              WRITE     FIVAB000
076600990910     C*
076700010601     C                   if        *in31 = *off and
076800010601     C                             *in32 = *off
076900990910     C                   eval      vinflg = '1'
077000990910     C                   else
077100990910     C                   eval      vinflg = '2'
077200990910     C                   endif
077300991022     C                   endif
077400990910     C                   endif
077500010601     C*
077600010601     C                   endif
077700000905     C*
077800000905     C                   else
077900000905     C                   eval      vinflg = '1'
078000000905     C                   endif
078100000905     C*
078200000905     C  N70              update    tivin000
078300991022     C*
078400991022     C  N70              ENDdo
078500010530     C
078600010530     C*
078700010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
078800010530     C                   if        cntNonEl = *zeros or
078900010530     C                             flgMulti = '0'
079000990910     C* Se non ci sono record con errori ...
079100010530     C                   if        §ctrno = 0 and
079200010601     C                             §ctrmo = 0 and
079300010530     C                             flgStato <> '2'
079400990910     C* ... restituisco esito OK.
079500990921     C                   eval      wrkesito = '0'
079600990910     C                   else
079700010601     C                   if        §ctrok > 0
079800990921     C                   eval      wrkesito = '1'
079900000710     C                   else
080000010615     C                   if        flgOk = '0'
080100010615     C                   eval      wrkesito = '2'
080200010615     C                   else
080300010615     C                   eval      wrkesito = '6'
080400010615     C                   endif
080500990910     C                   endif
080600000710     C                   endif
080700010530     C                   else
080800010530     C                   eval      wrkesito = '9'
080900010530     C                   endif
081000990910     C*
081100990914     C                   if        %open(tivin00r)
081200990908     C                   close     tivin00r
081300990914     C                   endif
081400020913     C                   if        %open(fivabwwr)
081500020913     C                   close     fivabwwr
081600990914     C                   endif
081700010530     C*
081800010530     C                   if        vlrpoi <> 999
081900010530     C                   eval      invfil = vlrpoi
082000010530     C                   endif
082100990910     C*
082200990920     C                   if        §ctrok > 0
082300010530     C                             and invfil > *zeros
082400000613     C                   exsr      invio
082500990920     C                   endif
082600010612     C*
082700010612     C                   if        flgGiro = '1'
082800010612     C                   exsr      endela
082900010612     C                   endif
083000990920     C*
083100910830     C                   ENDSR
083200000613     C***
083300990920
083400010530
083500010530
083600010530
083700010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
083800010530     C     repfil        BEGSR
083900010530     C*
084000010530     C                   if        invfil = *zeros and
084100010530     C                             depfil > *zeros and
084200010613     C                             (vinflg = *blanks or
084300010613     C                              vinflg = *zeros)
084400010530     C
084500010530     C                   eval      invfil = depfil
084600010530     C                   endif
084700010530     C*
084800010530     C                   if        depfil <> invfil and
084900010530     C                             invfil > *zeros
085000010530     C                   eval      flgMulti = '1'
085100010530     C                   if        vinflg = *blanks
085200010530     C                   add       1             cntNonEl
085300010530     C                   endif
085400010530     C                   endif
085500010530     C*
085600010530     C                   if        vinflg = '2'
085700010530     C                   eval      flgStato = '2'
085800010530     C                   endif
085900010530     C*
086000010530     C                   ENDSR
086100010530     C***
086200010530
086300010530
086400010530
086500010530
086600990920      /TITLE Invio dei dati al punto operativo.
086700000613     C     invio         BEGSR
086800990920     C*
086900990920     C                   reset                   dscmz
087000010530     C                   move      invfil        cmzdst
087100990920     C                   eval      cmzfld = vlrfou
087200990920     C                   eval      cmzmbd = vlrhdl
087300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
087400000710     C                   if        prmfir = *blanks
087500020913     C                   eval      cmzfla = 'FIVAB00F'
087600020913     C                   eval      cmzmba = 'FIVAB00F'
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
089400000613     C***
089500010530
089600010530
089700010530
089800010530
089900010530      /TITLE Invio dei dati al punto operativo.
090000010530     C     opeini        BEGSR
090100010530     C*
090200010530     C* Inizializzo flag e contatori operativi
090300010530     C                   movel     '0'           flgGiro           1
090400010530     C                   movel     '0'           flgMulti          1
090500010530     C                   movel     '1'           flgStato          1
090600010615     C                   movel     '0'           flgOk             1
090700010530     C                   z-add     *zeros        cntNonEl         10 0
090800010530     C                   z-add     *zeros        depfil            3 0
090900010530     C                   z-add     *zeros        invfil            3 0
091000030407     C*
091100030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
091200030407     C                   if        %subst(vlrppt:1:1) = 'S'
091300030407     C                   seton                                        50
091400030407     C                   else
091500030407     C                   setoff                                       50
091600030407     C                   endif
091700041001     C*
091800041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
091900041001     C                   if        %subst(vlrppt:2:1) = 'S'
092000041001     C                   seton                                        51
092100041001     C                   else
092200041001     C                   setoff                                       51
092300041001     C                   endif
092400041025     C*
092500041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
092600041025     C* (naturalmente solo se nn già presente)
092700041025     C                   if        %subst(vlrppt:3:1) = 'S'
092800041025     C                   seton                                        52
092900041025     C                   else
093000041025     C                   setoff                                       52
093100041025     C                   endif
093200010530     C*
093300010530     C                   ENDSR
093400010530     C***
093500040407
093600040407
093700040407
093800040407
093900040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
094000040407     C     exeBon        BEGSR
094100040407     C*
094200040407     C* Imposto i parametri x il *pgm d "bonifica"
094300040407     C                   EVAL      ElencoChar = *blanks
094400040407     C                   EVAL      TipoElenco = *blanks
094500040407     C                   EVAL      CharSost   = *blanks
094600040629     C                   EVAL      UpperCase  = '1'
094700040407     C                   EVAL      ChkNull    = '1'
094800040407     C                   EVAL      CharNull   = *blanks
094900040629     C*
095000040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
095100040629     C                   EVAL      TxtInOut   = VABRSD
095200040629     C                   EVAL      EsitoBon   = *blanks
095300040407     C*
095400040407     C                   CALL      'XCHKCHAR'
095500040407     C                   PARM                    TxtInOut
095600040407     C                   PARM                    ElencoChar
095700040407     C                   PARM                    TipoElenco
095800040407     C                   PARM                    CharSost
095900040407     C                   PARM                    UpperCase
096000040407     C                   PARM                    ChkNull
096100040407     C                   PARM                    CharNull
096200040407     C                   PARM                    EsitoBon
096300040407     C*
096400040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
096500040407     C                   IF        EsitoBon = '1'
096600040629     C                   EVAL      VABRSD = TxtInOut
096700040407     C                   ENDIF
096800040629     C*
096900040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
097000040629     C                   EVAL      TxtInOut   = VABIND
097100040629     C                   EVAL      EsitoBon   = *blanks
097200040629     C*
097300040629     C                   CALL      'XCHKCHAR'
097400040629     C                   PARM                    TxtInOut
097500040629     C                   PARM                    ElencoChar
097600040629     C                   PARM                    TipoElenco
097700040629     C                   PARM                    CharSost
097800040629     C                   PARM                    UpperCase
097900040629     C                   PARM                    ChkNull
098000040629     C                   PARM                    CharNull
098100040629     C                   PARM                    EsitoBon
098200040629     C*
098300040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
098400040629     C                   IF        EsitoBon = '1'
098500040629     C                   EVAL      VABIND = TxtInOut
098600040629     C                   ENDIF
098700040629     C*
098800040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
098900040629     C                   EVAL      TxtInOut   = VABLOD
099000040629     C                   EVAL      EsitoBon   = *blanks
099100040629     C*
099200040629     C                   CALL      'XCHKCHAR'
099300040629     C                   PARM                    TxtInOut
099400040629     C                   PARM                    ElencoChar
099500040629     C                   PARM                    TipoElenco
099600040629     C                   PARM                    CharSost
099700040629     C                   PARM                    UpperCase
099800040629     C                   PARM                    ChkNull
099900040629     C                   PARM                    CharNull
100000040629     C                   PARM                    EsitoBon
100100040629     C*
100200040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100300040629     C                   IF        EsitoBon = '1'
100400040629     C                   EVAL      VABLOD = TxtInOut
100500040629     C                   ENDIF
100600040629     C*
100700040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
100800040629     C                   EVAL      TxtInOut   = VABNOT
100900040629     C                   EVAL      EsitoBon   = *blanks
101000040630     C                   EVAL      UpperCase  = *blanks
101100040629     C*
101200040629     C                   CALL      'XCHKCHAR'
101300040629     C                   PARM                    TxtInOut
101400040629     C                   PARM                    ElencoChar
101500040629     C                   PARM                    TipoElenco
101600040629     C                   PARM                    CharSost
101700040629     C                   PARM                    UpperCase
101800040629     C                   PARM                    ChkNull
101900040629     C                   PARM                    CharNull
102000040629     C                   PARM                    EsitoBon
102100040629     C*
102200040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
102300040629     C                   IF        EsitoBon = '1'
102400040629     C                   EVAL      VABNOT = TxtInOut
102500040629     C                   ENDIF
102600040629     C*
102700040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
102800040629     C                   EVAL      TxtInOut   = VABNT2
102900040629     C                   EVAL      EsitoBon   = *blanks
103000040630     C                   EVAL      UpperCase  = *blanks
103100040629     C*
103200040629     C                   CALL      'XCHKCHAR'
103300040629     C                   PARM                    TxtInOut
103400040629     C                   PARM                    ElencoChar
103500040629     C                   PARM                    TipoElenco
103600040629     C                   PARM                    CharSost
103700040629     C                   PARM                    UpperCase
103800040629     C                   PARM                    ChkNull
103900040629     C                   PARM                    CharNull
104000040629     C                   PARM                    EsitoBon
104100040629     C*
104200040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
104300040629     C                   IF        EsitoBon = '1'
104400040629     C                   EVAL      VABNT2 = TxtInOut
104500040629     C                   ENDIF
104600040407     C*
104700040407     C                   ENDSR
104800010530
104900060410
105000060410     C*----------------------------------------------------*
105100060410     C*  CONTROLLO NUMERICITA' CAMPI
105200060410     C*----------------------------------------------------*
105300060410     C     CHKNUM        BEGSR
105400060410     C*
105500060410     C                   IF        PiDecChr = *blanks
105600060410     C                   EVAL      PiDecChr = ','
105700060410     C                   ENDIF
105800060410     C*
105900060410     C                   CALL(e)   'ISNUMERIC'
106000060410     C                   PARM                    PiStr            30
106100060410     C                   PARM                    PiDecChr          1
106200060410     C                   PARM      *ZEROS        PiVal            30 9
106300060410     C                   PARM      '0'           PiInt             1
106400060410     C                   PARM      '0'           PiNum             1
106500060410     C                   IF        %error
106600060410     C                   EVAL      PiNum=*off
106700060410     C                   ENDIF
106800060410     C*
106900060410     C                   ENDSR
107000060410     C***
107100060410
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
108400010606     C                   z-add     wn8           g08dat
108500010606     C                   z-add     *zeros        g08inv
108600010606     C                   movel     '0'           g08err
108700010606     C                   call      'XSRDA8'
108800010606     C                   parm                    wlbda8
108900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
109000000613     C*
109100000613     C                   ENDSR
109200000613     C***
