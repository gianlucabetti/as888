000100020913      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
003500990910     D fnvabaar      e ds                  inz
003600990910     D esito           s              1
003700000724     D prmlit          s             10
003800000710     D prmfir          s             10
003900990921     D wrkesito        s                   like(esito)
004000000613     D rrnum           s              6  0 INZ(*zeros)
004100060410     D wVHBCAS         s                   LIKE(VHBCAS) INZ(*blanks)
004200010606     D*------------------
004300010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004400010606     D*------------------
004500010606     D WLBDA8          DS                  INZ
004600010606     D  G08DAT                 1      8  0
004700010606     D  G08INV                 9     16  0
004800010606     D  G08ERR                17     17
004900010606     D  G08TGI                18     22  0
005000010606     D*
005100040407     D TxtInOut        S           2048
005200040407     D ElencoChar      S            256
005300040407     D TipoElenco      S              1
005400040407     D CharSost        S              1
005500040407     D UpperCase       S              1
005600040407     D ChkNull         S              1
005700040407     D CharNull        S              1
005800040407     D EsitoBon        S              1
005900990908
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300010530     C*
006400010530     C                   exsr      opeini
006500010530     C                   exsr      rwvab
006600041025     C*
006700041025     C* Esegue lancio TISI95R solo x chiusura
006800041025     C                   CLEAR                   TISI95DS
006900041025     C                   EVAL      I95TLA = 'C'
007000041025     C                   CALL      'TISI95R'
007100041025     C                   PARM                    TISI95DS
007200010530     C*
007300010530     C                   seton                                        lr
007400010530
007500010530
007600010530
007700010530
007800010530     C*--------------------------------------------------------
007900010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008000010530     C*--------------------------------------------------------
008100010530     C     PREELA        BEGSR
008200000724     C*
008300000724     C* SE OCCORRE SPEDIRE IN FILIALE
008400010530     C                   if        invfil <> *zeros and
008500010530     C                             flgGiro = '0'
008600010530     C*
008700010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008800010530     C                   eval      flgGiro = '1'
008900010530     C*
009000010530     C                   endif
009100010530     C*
009200010530     C                   ENDSR
009300010530     C***
009400010530
009500010530
009600010530
009700010530
009800010530
009900010530
010000010530     C*--------------------------------------------------------
010100010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010200010530     C*--------------------------------------------------------
010300010530     C     ENDELA        BEGSR
010400010530     C*
010500010530     C*
010600010530     C                   ENDSR
010700010530     C***
010800990908
010900010530
011000010530
011100010530
011200910830     C*--------------------------------------------------------
011300020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
011400910830     C*--------------------------------------------------------
011500941229     C     RWVAB         BEGSR
011600990910     C*
011700990914     C                   if        not %open(tivin00r)
011800990908     C                   open      tivin00r
011900990914     C                   endif
012000020913     C                   if        not %open(fivabwwr)
012100020913     C                   open      fivabwwr
012200990914     C                   endif
012300990910     C*
012400990910     C                   clear                   §CTROK
012500990910     C                   clear                   §CTRMO
012600990910     C                   clear                   §CTRNO
012700990910     C*
012800921023     C                   DO        *HIVAL
012900990913     C*
013000990915     C                   READ      tivin00r                               70
013100010618     C*
013200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013300010618     C                   if        vinflg = '1'
013400010618     C                   eval      flgOk = '1'
013500010618     C                   endif
013600010705     C*
013700010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
013800010705     C                   if        %subst(vindta:1:1) = '='
013900010705     C                   eval      %subst(vindta:1:1) = *blanks
014000010705     C                   endif
014100010705     C*
014200051017     C                   if        vindta  > *blanks
014300000613     C                   add       1             rrnum
014400010601     C* ...elimino eventuali segni
014500010601     C     '+':' '       xlate     vindta        vindta
014600990913     C*
014700010601     C                   if        *in70 = *off and
014800990913     C                             (vinflg = *blanks
014900010711     C                              or vinflg = '0'
015000010711     C                              or vinflg = '2')
015100990913     C*
015200020913     C                   clear                   FIVAB000
015300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015400010711     C                   if        vinflg = *blanks or vinflg = '0'
015500010711     C                   clear                   vinmsg
015600010711     C                   endif
015700010711     C*
015800990913     C                   eval      fnvabaar = vindta
015900991022     C*
016000991022     C                   testb     x'3F'         vhbatb                   02
016100991022     C                   if        not *in02
016200991022     C*
016300971021     C                   SETOFF                                       303132
016400971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
016500971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
016600971023     C*    OPPURE TUTTI BLANKS
016700010530     C*
016800010530     C                   TESTN                   VHBLNP               30
016900010530     C  N30              TESTN                   VHBLNP                 30
017000010530     C  N30              TESTN                   VHBLNP                   30
017100010530     C                   MOVE      VHBLNP        W001A             1
017200010530     C     *IN30         IFEQ      *OFF
017300010530     C     W001A         ORLT      '0'
017400010530     C                   SETON                                        31
017500010530     C                   eval      vinmsg = %trimr(vinmsg)
017600010530     C                             + ' ' + 'VHBLNP'
017700010530     C                   ELSE
017800010530     C                   MOVE      VHBLNP        VABLNP
017900010530     C     VABLNP        IFLE      *ZEROS
018000010530     C                   SETON                                        31
018100010530     C                   eval      vinmsg = %trimr(vinmsg)
018200010530     C                             + ' ' + 'VHBLNP'
018300010530     C                   ENDIF
018400010530     C                   ENDIF
018500010530     C*
018600010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018700010530     C                   eval      depfil = VABLNP
018800010530     C                   exsr      repfil
018900010601     C                   if        depfil = invfil
019000021014     C                   if        vlrpoi = 999
019100021010     C                   MOVE(P)   invfil        VABFGS
019200021014     C                   else
019300021014     C                   MOVE(P)   vlrpoi        VABFGS
019400021014     C                   endif
019500010530     C*
019600010530     C                   exsr      PREELA
019700010530     C*
019800971023     C                   TESTN                   VHBCCM               30
019900971023     C  N30              TESTN                   VHBCCM                 30
020000971023     C  N30              TESTN                   VHBCCM                   30
020100971021     C                   MOVE      VHBCCM        W001A             1
020200971021     C     *IN30         IFEQ      *OFF
020300971021     C     W001A         ORLT      '0'
020400971021     C                   SETON                                        31
020500010604     C                   eval      vinmsg = %trimr(vinmsg)
020600010604     C                             + ' ' + 'VHBCCM'
020700971021     C                   ELSE
020800971021     C                   MOVE      VHBCCM        VABCCM
020900971024     C     VABCCM        IFLE      *ZEROS
021000971024     C                   SETON                                        31
021100010604     C                   eval      vinmsg = %trimr(vinmsg)
021200010604     C                             + ' ' + 'VHBCCM'
021300971024     C                   ENDIF
021400971021     C                   ENDIF
021500971021     C*
021600971023     C                   TESTN                   VHBAAS               30
021700971023     C  N30              TESTN                   VHBAAS                 30
021800971023     C  N30              TESTN                   VHBAAS                   30
021900971021     C                   MOVE      VHBAAS        W001A             1
022000971021     C     *IN30         IFEQ      *OFF
022100971021     C     W001A         ORLT      '0'
022200971023     C     W001A         ANDNE     *BLANK
022300971021     C                   SETON                                        32
022400010606     C                   MOVEL     datcor        VABAAS
022500010604     C                   eval      vinmsg = %trimr(vinmsg)
022600010604     C                             + ' ' + 'VHBAAS'
022700971021     C                   ELSE
022800971021     C                   MOVE      VHBAAS        VABAAS
022900971021     C                   ENDIF
023000971021     C*
023100971023     C                   TESTN                   VHBMGS               30
023200971023     C  N30              TESTN                   VHBMGS                 30
023300971023     C  N30              TESTN                   VHBMGS                   30
023400971021     C                   MOVE      VHBMGS        W001A             1
023500971021     C     *IN30         IFEQ      *OFF
023600971021     C     W001A         ORLT      '0'
023700971023     C     W001A         ANDNE     *BLANK
023800971021     C                   SETON                                        32
023900010601     C                   MOVE      '0101'        VABMGS
024000010604     C                   eval      vinmsg = %trimr(vinmsg)
024100010604     C                             + ' ' + 'VHBMGS'
024200971021     C                   ELSE
024300971021     C                   MOVE      VHBMGS        VABMGS
024400971021     C                   ENDIF
024500971021     C*
024600971023     C                   TESTN                   VHBNRS               30
024700971023     C  N30              TESTN                   VHBNRS                 30
024800971023     C  N30              TESTN                   VHBNRS                   30
024900971021     C                   MOVE      VHBNRS        W001A             1
025000971021     C     *IN30         IFEQ      *OFF
025100971021     C     W001A         ORLT      '0'
025200971023     C     W001A         ANDNE     *BLANK
025300971021     C                   SETON                                        32
025400971021     C                   MOVE      *ZERO         VABNRS
025500010604     C                   eval      vinmsg = %trimr(vinmsg)
025600010604     C                             + ' ' + 'VHBNRS'
025700971021     C                   ELSE
025800971021     C                   MOVE      VHBNRS        VABNRS
025900971021     C                   ENDIF
026000971021     C*
026100971023     C                   TESTN                   VHBNSP               30
026200971023     C  N30              TESTN                   VHBNSP                 30
026300971023     C  N30              TESTN                   VHBNSP                   30
026400971021     C                   MOVE      VHBNSP        W001A             1
026500971021     C     *IN30         IFEQ      *OFF
026600971021     C     W001A         ORLT      '0'
026700971023     C     W001A         ANDNE     *BLANK
026800971021     C                   SETON                                        32
026900971021     C                   MOVE      *ZERO         VABNSP
027000010604     C                   eval      vinmsg = %trimr(vinmsg)
027100010604     C                             + ' ' + 'VHBNSP'
027200971021     C                   ELSE
027300971021     C                   MOVE      VHBNSP        VABNSP
027400971021     C                   ENDIF
027500971021     C*
027600971023     C                   TESTN                   VHBLNA               30
027700971023     C  N30              TESTN                   VHBLNA                 30
027800971023     C  N30              TESTN                   VHBLNA                   30
027900971021     C                   MOVE      VHBLNA        W001A             1
028000971021     C     *IN30         IFEQ      *OFF
028100971021     C     W001A         ORLT      '0'
028200971023     C     W001A         ANDNE     *BLANK
028300971021     C                   SETON                                        32
028400971021     C                   MOVE      *ZERO         VABLNA
028500010604     C                   eval      vinmsg = %trimr(vinmsg)
028600010604     C                             + ' ' + 'VHBLNA'
028700971021     C                   ELSE
028800971021     C                   MOVE      VHBLNA        VABLNA
028900971021     C                   ENDIF
029000971021     C*
029100971023     C                   TESTN                   VHBCTR               30
029200971023     C  N30              TESTN                   VHBCTR                 30
029300971023     C  N30              TESTN                   VHBCTR                   30
029400971021     C                   MOVE      VHBCTR        W001A             1
029500971021     C     *IN30         IFEQ      *OFF
029600971021     C     W001A         ORLT      '0'
029700971023     C     W001A         ANDNE     *BLANK
029800971021     C                   SETON                                        32
029900971021     C                   MOVE      *ZERO         VABCTR
030000010604     C                   eval      vinmsg = %trimr(vinmsg)
030100010604     C                             + ' ' + 'VHBCTR'
030200971021     C                   ELSE
030300971021     C                   MOVE      VHBCTR        VABCTR
030400971021     C                   ENDIF
030500941229     C*
030600971021     C                   MOVE      *ZERO         VABIAS
030700941229     C                   MOVE      VHBIAS        WASC15
030800971023     C                   TESTN                   WI15                 30
030900971023     C  N30              TESTN                   WI15                   30
031000971023     C  N30              TESTN                   WI15                     30
031100971021     C                   MOVE      WI15          W001A             1
031200971021     C     *IN30         IFEQ      *OFF
031300971021     C     W001A         ORLT      '0'
031400971023     C     W001A         ANDNE     *BLANK
031500971021     C                   SETON                                        32
031600971021     C                   MOVE      *ZERO         VABIAS
031700010604     C                   eval      vinmsg = %trimr(vinmsg)
031800010604     C                             + ' ' + 'VHBIAS'
031900971021     C                   ELSE
032000971021     C                   MOVEL     WI15          VABIAS
032100971023     C                   TESTN                   WD15                 30
032200971023     C  N30              TESTN                   WD15                   30
032300971023     C  N30              TESTN                   WD15                     30
032400971021     C                   MOVE      WD15          W001A             1
032500971021     C     *IN30         IFEQ      *OFF
032600971021     C     W001A         ORLT      '0'
032700971023     C     W001A         ANDNE     *BLANK
032800971021     C                   SETON                                        32
032900971021     C                   MOVE      *ZERO         VABIAS
033000010604     C                   eval      vinmsg = %trimr(vinmsg)
033100010604     C                             + ' ' + 'VHBIAS'
033200971021     C                   ELSE
033300971021     C                   MOVE      WD15          VABIAS
033400971021     C                   ENDIF
033500050118     C                   ENDIF
033600941229     C*
033700971023     C                   TESTN                   VHBNCL               30
033800971023     C  N30              TESTN                   VHBNCL                 30
033900971023     C  N30              TESTN                   VHBNCL                   30
034000971021     C                   MOVE      VHBNCL        W001A             1
034100971021     C     *IN30         IFEQ      *OFF
034200971021     C     W001A         ORLT      '0'
034300971021     C                   SETON                                        32
034400971021     C                   MOVE      *ZERO         VABNCL
034500010604     C                   eval      vinmsg = %trimr(vinmsg)
034600010604     C                             + ' ' + 'VHBNCL'
034700971021     C                   ELSE
034800971021     C                   MOVE      VHBNCL        VABNCL
034900971021     C                   ENDIF
035000941229     C*
035100971021     C                   MOVE      *ZERO         VABPKB
035200971021     C                   MOVE      VHBPKB        WASC09
035300010719     C                   IF        WI09 = *blanks
035400010719     C                   EVAL      WI09 = *zeros
035500010719     C                   ENDIF
035600971023     C                   TESTN                   WI09                 30
035700971023     C  N30              TESTN                   WI09                   30
035800971023     C  N30              TESTN                   WI09                     30
035900971021     C                   MOVE      WI09          W001A             1
036000971021     C     *IN30         IFEQ      *OFF
036100971021     C     W001A         ORLT      '0'
036200971021     C                   SETON                                        32
036300971021     C                   MOVE      *ZERO         VABPKB
036400010604     C                   eval      vinmsg = %trimr(vinmsg)
036500010604     C                             + ' ' + 'VHBPKB'
036600971021     C                   ELSE
036700971021     C                   MOVEL     WI09          VABPKB
036800010719     C                   IF        WD09 = *blanks
036900010719     C                   EVAL      WD09 = *zeros
037000010719     C                   ENDIF
037100971023     C                   TESTN                   WD09                 30
037200971023     C  N30              TESTN                   WD09                     30
037300971021     C                   MOVE      WD09          W001A             1
037400971021     C     *IN30         IFEQ      *OFF
037500971021     C     W001A         ORLT      '0'
037600971023     C     W001A         ANDNE     *BLANK
037700971021     C                   SETON                                        32
037800971021     C                   MOVE      *ZERO         VABPKB
037900010604     C                   eval      vinmsg = %trimr(vinmsg)
038000010604     C                             + ' ' + 'VHBPKB'
038100971021     C                   ELSE
038200971021     C                   MOVE      WD09          VABPKB
038300971021     C                   ENDIF
038400050118     C                   ENDIF
038500971021     C*
038600041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
038700041001     C                   IF        *IN51 = *OFF
038800971021     C                   MOVE      *ZERO         VABVLB
038900971021     C                   MOVE      VHBVLB        WASC07
039000010719     C                   IF        WI07 = *blanks
039100010719     C                   EVAL      WI07 = *zeros
039200010719     C                   ENDIF
039300971023     C                   TESTN                   WI07                 30
039400971023     C  N30              TESTN                   WI07                   30
039500971023     C  N30              TESTN                   WI07                     30
039600971021     C                   MOVE      WI07          W001A             1
039700971021     C     *IN30         IFEQ      *OFF
039800971021     C     W001A         ORLT      '0'
039900971023     C     W001A         ANDNE     *BLANK
040000971021     C                   SETON                                        32
040100971021     C                   MOVE      *ZERO         VABVLB
040200010604     C                   eval      vinmsg = %trimr(vinmsg)
040300010604     C                             + ' ' + 'VHBVLB'
040400971021     C                   ELSE
040500971021     C                   MOVEL     WI07          VABVLB
040600010719     C                   IF        WD07 = *blanks
040700010719     C                   EVAL      WD07 = *zeros
040800010719     C                   ENDIF
040900971023     C                   TESTN                   WD07                 30
041000971023     C  N30              TESTN                   WD07                   30
041100971023     C  N30              TESTN                   WD07                     30
041200971021     C                   MOVE      WD07          W001A             1
041300971021     C     *IN30         IFEQ      *OFF
041400971021     C     W001A         ORLT      '0'
041500971023     C     W001A         ANDNE     *BLANK
041600971021     C                   SETON                                        32
041700971021     C                   MOVE      *ZERO         VABVLB
041800010604     C                   eval      vinmsg = %trimr(vinmsg)
041900010604     C                             + ' ' + 'VHBVLB'
042000971021     C                   ELSE
042100971021     C                   MOVE      WD07          VABVLB
042200971021     C                   ENDIF
042300050118     C                   ENDIF
042400041001     C                   ELSE
042500041001     C* ...altrimenti forzo azzeramento del volume
042600041001     C                   Z-ADD     *zeros        VABVLB
042700041001     C                   ENDIF
042800941229     C*
042900971021     C                   MOVE      *ZERO         VABQFT
043000971021     C                   MOVE      VHBQFT        WASC15
043100010719     C                   IF        WI15 = *blanks
043200010719     C                   EVAL      WI15 = *zeros
043300010719     C                   ENDIF
043400971023     C                   TESTN                   WI15                 30
043500971023     C  N30              TESTN                   WI15                   30
043600971023     C  N30              TESTN                   WI15                     30
043700971021     C                   MOVE      WI15          W001A             1
043800971021     C     *IN30         IFEQ      *OFF
043900971021     C     W001A         ORLT      '0'
044000971023     C     W001A         ANDNE     *BLANK
044100971021     C                   SETON                                        32
044200971021     C                   MOVE      *ZERO         VABQFT
044300010604     C                   eval      vinmsg = %trimr(vinmsg)
044400010604     C                             + ' ' + 'VHBQFT'
044500971021     C                   ELSE
044600971021     C                   MOVEL     WI15          VABQFT
044700010719     C                   IF        WD15 = *blanks
044800010719     C                   EVAL      WD15 = *zeros
044900010719     C                   ENDIF
045000971023     C                   TESTN                   WD15                 30
045100971023     C  N30              TESTN                   WD15                   30
045200971023     C  N30              TESTN                   WD15                     30
045300971021     C                   MOVE      WD15          W001A             1
045400971021     C     *IN30         IFEQ      *OFF
045500971021     C     W001A         ORLT      '0'
045600971023     C     W001A         ANDNE     *BLANK
045700971021     C                   SETON                                        32
045800971021     C                   MOVE      *ZERO         VABQFT
045900010604     C                   eval      vinmsg = %trimr(vinmsg)
046000010604     C                             + ' ' + 'VHBQFT'
046100971021     C                   ELSE
046200971021     C                   MOVE      WD15          VABQFT
046300971021     C                   ENDIF
046400050118     C                   ENDIF
046500941229     C*
046600971021     C                   MOVE      *ZERO         VABCAS
046700060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
046800060410     C                   EVAL      wVHBCAS = VHBCAS
046900060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
047000060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
047100060410     C                   ENDIF
047200060410     C*
047300060410     C                   MOVEL(P)  wVHBCAS       PiStr
047400060410     C                   EXSR      CHKNUM
047500060410     C                   IF        PiNum=*on
047600060410     C                   Z-ADD     PiVal         VABCAS
047700060410     C                   ELSE
047800060410     C                   SETON                                        32
047900060410     C                   Z-ADD     *zeros        VABCAS
048000060410     C                   EVAL      vinmsg = %trimr(vinmsg)
048100060410     C                             + ' ' + 'VHBCAS'
048200060410     C                   ENDIF
048300971021     C*
048400971023     C                   TESTN                   VHBRMN               30
048500971023     C  N30              TESTN                   VHBRMN                 30
048600971023     C  N30              TESTN                   VHBRMN                   30
048700971021     C                   MOVE      VHBRMN        W001A             1
048800971021     C     *IN30         IFEQ      *OFF
048900971021     C     W001A         ORLT      '0'
049000971021     C                   SETON                                        32
049100971021     C                   MOVE      *ZERO         VABRMN
049200010604     C                   eval      vinmsg = %trimr(vinmsg)
049300010604     C                             + ' ' + 'VHBRMN'
049400971021     C                   ELSE
049500971021     C                   MOVE      VHBRMN        VABRMN
049600971021     C                   ENDIF
049700971021     C*
049800971023     C                   TESTN                   VHBNCD               30
049900971023     C  N30              TESTN                   VHBNCD                 30
050000971023     C  N30              TESTN                   VHBNCD                   30
050100971021     C                   MOVE      VHBNCD        W001A             1
050200971021     C     *IN30         IFEQ      *OFF
050300971021     C     W001A         ORLT      '0'
050400971023     C     W001A         ANDNE     *BLANK
050500971021     C                   SETON                                        32
050600971021     C                   MOVE      *ZERO         VABNCD
050700010604     C                   eval      vinmsg = %trimr(vinmsg)
050800010604     C                             + ' ' + 'VHBNCD'
050900971021     C                   ELSE
051000971021     C                   MOVE      VHBNCD        VABNCD
051100971021     C                   ENDIF
051200971021     C*
051300971023     C                   TESTN                   VHBNCA               30
051400971023     C  N30              TESTN                   VHBNCA                 30
051500971023     C  N30              TESTN                   VHBNCA                   30
051600971021     C                   MOVE      VHBNCA        W001A             1
051700971021     C     *IN30         IFEQ      *OFF
051800971021     C     W001A         ORLT      '0'
051900971023     C     W001A         ANDNE     *BLANK
052000971021     C                   SETON                                        32
052100971021     C                   MOVE      *ZERO         VABNCA
052200010604     C                   eval      vinmsg = %trimr(vinmsg)
052300010604     C                             + ' ' + 'VHBNCA'
052400971021     C                   ELSE
052500971021     C                   MOVE      VHBNCA        VABNCA
052600971021     C                   ENDIF
052700971021     C*
052800971023     C                   TESTN                   VHBZNC               30
052900971023     C  N30              TESTN                   VHBZNC                 30
053000971023     C  N30              TESTN                   VHBZNC                   30
053100971021     C                   MOVE      VHBZNC        W001A             1
053200971021     C     *IN30         IFEQ      *OFF
053300971021     C     W001A         ORLT      '0'
053400971023     C     W001A         ANDNE     *BLANK
053500971021     C                   SETON                                        32
053600971021     C                   MOVE      *ZERO         VABZNC
053700010604     C                   eval      vinmsg = %trimr(vinmsg)
053800010604     C                             + ' ' + 'VHBZNC'
053900971021     C                   ELSE
054000971021     C                   MOVE      VHBZNC        VABZNC
054100971021     C                   ENDIF
054200971021     C*
054300971023     C                   TESTN                   VHBDCR               30
054400971023     C  N30              TESTN                   VHBDCR                 30
054500971023     C  N30              TESTN                   VHBDCR                   30
054600971021     C                   MOVE      VHBDCR        W001A             1
054700971021     C     *IN30         IFEQ      *OFF
054800971021     C     W001A         ORLT      '0'
054900971023     C     W001A         ANDNE     *BLANK
055000971021     C                   SETON                                        32
055100971021     C                   MOVE      *ZERO         VABDCR
055200010604     C                   eval      vinmsg = %trimr(vinmsg)
055300010604     C                             + ' ' + 'VHBDCR'
055400971021     C                   ELSE
055500971021     C                   MOVE      VHBDCR        VABDCR
055600971021     C                   ENDIF
055700971021     C*
055800971023     C                   TESTN                   VHBHCR               30
055900971023     C  N30              TESTN                   VHBHCR                 30
056000971023     C  N30              TESTN                   VHBHCR                   30
056100971021     C                   MOVE      VHBHCR        W001A             1
056200971021     C     *IN30         IFEQ      *OFF
056300971021     C     W001A         ORLT      '0'
056400971023     C     W001A         ANDNE     *BLANK
056500971021     C                   SETON                                        32
056600971021     C                   MOVE      *ZERO         VABHCR
056700010604     C                   eval      vinmsg = %trimr(vinmsg)
056800010604     C                             + ' ' + 'VHBHCR'
056900971021     C                   ELSE
057000971021     C                   MOVE      VHBHCR        VABHCR
057100971021     C                   ENDIF
057200941229     C*
057300971021     C                   MOVE      *ZERO         VABVMD
057400971021     C                   MOVE      VHBVMD        WASC15
057500010719     C                   IF        WI15 = *blanks
057600010719     C                   EVAL      WI15 = *zeros
057700010719     C                   ENDIF
057800971023     C                   TESTN                   WI15                 30
057900971023     C  N30              TESTN                   WI15                   30
058000971023     C  N30              TESTN                   WI15                     30
058100971021     C                   MOVE      WI15          W001A             1
058200971021     C     *IN30         IFEQ      *OFF
058300971021     C     W001A         ORLT      '0'
058400971023     C     W001A         ANDNE     *BLANK
058500971021     C                   SETON                                        32
058600971021     C                   MOVE      *ZERO         VABVMD
058700010604     C                   eval      vinmsg = %trimr(vinmsg)
058800010604     C                             + ' ' + 'VHBVMD'
058900971021     C                   ELSE
059000971021     C                   MOVEL     WI15          VABVMD
059100010719     C                   IF        WD15 = *blanks
059200010719     C                   EVAL      WD15 = *zeros
059300010719     C                   ENDIF
059400971023     C                   TESTN                   WD15                 30
059500971023     C  N30              TESTN                   WD15                   30
059600971023     C  N30              TESTN                   WD15                     30
059700971021     C                   MOVE      WD15          W001A             1
059800971021     C     *IN30         IFEQ      *OFF
059900971021     C     W001A         ORLT      '0'
060000971023     C     W001A         ANDNE     *BLANK
060100971021     C                   SETON                                        32
060200971021     C                   MOVE      *ZERO         VABVMD
060300010604     C                   eval      vinmsg = %trimr(vinmsg)
060400010604     C                             + ' ' + 'VHBVMD'
060500971021     C                   ELSE
060600971021     C                   MOVE      WD15          VABVMD
060700971021     C                   ENDIF
060800050118     C                   ENDIF
060900971021     C*
061000971023     C                   TESTN                   VHBANT               30
061100971023     C  N30              TESTN                   VHBANT                 30
061200971023     C  N30              TESTN                   VHBANT                   30
061300971021     C                   MOVE      VHBANT        W001A             1
061400971021     C     *IN30         IFEQ      *OFF
061500971021     C     W001A         ORLT      '0'
061600971023     C     W001A         ANDNE     *BLANK
061700971021     C                   SETON                                        32
061800971021     C                   MOVE      *ZERO         VABANT
061900010604     C                   eval      vinmsg = %trimr(vinmsg)
062000010604     C                             + ' ' + 'VHBANT'
062100971021     C                   ELSE
062200971021     C                   MOVE      VHBANT        VABANT
062300971021     C                   ENDIF
062400971021     C*
062500971021     C* CAMPI ALFABETICI:
062600971021     C                   MOVE      VHBATB        VABATB
062700971021     C                   MOVE      VHBCBO        VABCBO
062800971021     C                   MOVE      VHBRSD        VABRSD
062900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
063000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
063100020117     C* ==
063200971021     C                   MOVE      VHBRD2        VABRD2
063300971021     C                   MOVE      VHBIND        VABIND
063400971021     C                   MOVE      VHBCAD        VABCAD
063500971021     C                   MOVE      VHBLOD        VABLOD
063600971021     C                   MOVE      VHBPRD        VABPRD
063700971021     C                   MOVE      VHBNZD        VABNZD
063800971021     C                   MOVE      VHBGC1        VABGC1
063900971021     C                   MOVE      VHBGC2        VABGC2
064000971021     C                   MOVE      VHBTSP        VABTSP
064100971021     C                   MOVE      VHBVAS        VABVAS
064200971021     C                   MOVE      VHBNAS        VABNAS
064300971021     C                   MOVE      VHBTIC        VABTIC
064400971021     C                   MOVE      VHBVCA        VABVCA
064500971021     C                   MOVE      VHBGCA        VABGCA
064600971021     C                   MOVE      VHBRMA        VABRMA
064700971021     C                   MOVE      VHBXCO        VABXCO
064800971021     C                   MOVE      VHBNOT        VABNOT
064900971021     C                   MOVE      VHBNT2        VABNT2
065000971021     C                   MOVE      VHBCTM        VABCTM
065100971021     C                   MOVE      VHBFFD        VABFFD
065200971021     C                   MOVE      VHBTCR        VABTCR
065300971021     C                   MOVE      VHBCTS        VABCTS
065400971021     C                   MOVE      VHBFTM        VABFTM
065500971021     C                   MOVE      VHBVAD        VABVAD
065600971021     C                   MOVE      VHBGMA        VABGMA
065700971021     C                   MOVE      VHBGGA        VABGGA
065800971021     C                   MOVE      VHBGVA        VABGVA
065900971021     C                   MOVE      VHBTC1        VABTC1
066000971021     C                   MOVE      VHBTC2        VABTC2
066100971021     C                   MOVE      VHBSCL        VABSCL
066200971021     C                   MOVE      VHBRMO        VABRMO
066300971021     C                   MOVE      VHBCMO        VABCMO
066400971021     C                   MOVE      VHBNMO        VABNMO
066500971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
066600060329     C* NELLA NAZIONE MITTENTE ORIGINALE
066700060331     C                   IF        %trim(VHBNMO) < 'A'
066800060329     C                   MOVE      *BLANKS       VABNMO
066900060329     C                   ENDIF
067000050418     C*
067100050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
067200050418     C                   IF        %trim(VABNZD) = 'I'   OR
067300050418     C                             %trim(VABNZD) = 'IT'  OR
067400050418     C                             %trim(VABNZD) = 'ITA'
067500050418     C                   EVAL      VABNZD = *blanks
067600050418     C                   ENDIF
067700041025     C*
067800041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
067900041025     C                   IF        *IN52 = *ON
068000041025     C* Reperisco la provincia dal CAP e dalla località
068100041025     C                   IF        VABCAD <> *blanks AND
068200050907     C                             VABPRD  = *blanks AND
068300050907     C                             VABNZD  = *blanks
068400041025     C                   CLEAR                   TISI95DS
068500041025     C                   EVAL      I95TCN = '3'
068600041025     C                   Z-ADD     datcor        I95DAT
068700041025     C                   EVAL      I95CAP = VABCAD
068800041025     C                   EVAL      I95LOC = VABLOD
068900041025     C                   CALL      'TISI95R'
069000041025     C                   PARM                    TISI95DS
069100041025     C                   EVAL      VABPRD = O95PRV
069200041025     C                   ENDIF
069300041025     C                   ENDIF
069400040630     C*
069500040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
069600040630     C                   exsr      exeBon
069700100419     C*
069800100419     C* Forzatura su campi DIVISE
069900100419     C                   IF        VABVAS = *blanks
070000100419     C                   EVAL      VABVAS = 'EUR'
070100100419     C                   ENDIF
070200100419     C                   IF        VABVCA = *blanks
070300100419     C                   EVAL      VABVCA = 'EUR'
070400100419     C                   ENDIF
070500100419     C                   IF        VABVAD = *blanks
070600100419     C                   EVAL      VABVAD = 'EUR'
070700100419     C                   ENDIF
070800111102     C*
070900111102     C* Forzature da parametri d anagrafica
071000111102     C                   if        %subst(vlrppt:4:2) <> *blanks
071100111102     C                   eval      VABTIC = %subst(vlrppt:4:2)
071200111102     C                   endif
071300131003     C*
071400131003     C* Forzature ad hoc per il cliente
071500131003     C                   if        VABCTR = 550
071600131003     C                             and VABCAS > 0
071700131003     C                   eval      VABTIC = 'TM'
071800131003     C                   endif
071900930409     C*
072000010604     C  N31              ADD       1             §CTROK            7 0
072100010604     C   32              ADD       1             §CTRMO            7 0
072200010604     C   31              ADD       1             §CTRNO            7 0
072300020913     C  N31              WRITE     FIVAB000
072400990910     C*
072500010601     C                   if        *in31 = *off and
072600010601     C                             *in32 = *off
072700990910     C                   eval      vinflg = '1'
072800990910     C                   else
072900990910     C                   eval      vinflg = '2'
073000990910     C                   endif
073100991022     C                   endif
073200990910     C                   endif
073300010601     C*
073400010601     C                   endif
073500000905     C*
073600000905     C                   else
073700000905     C                   eval      vinflg = '1'
073800000905     C                   endif
073900000905     C*
074000000905     C  N70              update    tivin000
074100991022     C*
074200991022     C  N70              ENDdo
074300010530     C
074400010530     C*
074500010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
074600010530     C                   if        cntNonEl = *zeros or
074700010530     C                             flgMulti = '0'
074800990910     C* Se non ci sono record con errori ...
074900010530     C                   if        §ctrno = 0 and
075000010601     C                             §ctrmo = 0 and
075100010530     C                             flgStato <> '2'
075200990910     C* ... restituisco esito OK.
075300990921     C                   eval      wrkesito = '0'
075400990910     C                   else
075500010601     C                   if        §ctrok > 0
075600990921     C                   eval      wrkesito = '1'
075700000710     C                   else
075800010615     C                   if        flgOk = '0'
075900010615     C                   eval      wrkesito = '2'
076000010615     C                   else
076100010615     C                   eval      wrkesito = '6'
076200010615     C                   endif
076300990910     C                   endif
076400000710     C                   endif
076500010530     C                   else
076600010530     C                   eval      wrkesito = '9'
076700010530     C                   endif
076800990910     C*
076900990914     C                   if        %open(tivin00r)
077000990908     C                   close     tivin00r
077100990914     C                   endif
077200020913     C                   if        %open(fivabwwr)
077300020913     C                   close     fivabwwr
077400990914     C                   endif
077500010530     C*
077600010530     C                   if        vlrpoi <> 999
077700010530     C                   eval      invfil = vlrpoi
077800010530     C                   endif
077900990910     C*
078000990920     C                   if        §ctrok > 0
078100010530     C                             and invfil > *zeros
078200000613     C                   exsr      invio
078300990920     C                   endif
078400010612     C*
078500010612     C                   if        flgGiro = '1'
078600010612     C                   exsr      endela
078700010612     C                   endif
078800990920     C*
078900910830     C                   ENDSR
079000000613     C***
079100990920
079200010530
079300010530
079400010530
079500010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
079600010530     C     repfil        BEGSR
079700010530     C*
079800010530     C                   if        invfil = *zeros and
079900010530     C                             depfil > *zeros and
080000010613     C                             (vinflg = *blanks or
080100010613     C                              vinflg = *zeros)
080200010530     C
080300010530     C                   eval      invfil = depfil
080400010530     C                   endif
080500010530     C*
080600010530     C                   if        depfil <> invfil and
080700010530     C                             invfil > *zeros
080800010530     C                   eval      flgMulti = '1'
080900010530     C                   if        vinflg = *blanks
081000010530     C                   add       1             cntNonEl
081100010530     C                   endif
081200010530     C                   endif
081300010530     C*
081400010530     C                   if        vinflg = '2'
081500010530     C                   eval      flgStato = '2'
081600010530     C                   endif
081700010530     C*
081800010530     C                   ENDSR
081900010530     C***
082000010530
082100010530
082200010530
082300010530
082400990920      /TITLE Invio dei dati al punto operativo.
082500000613     C     invio         BEGSR
082600990920     C*
082700990920     C                   reset                   dscmz
082800010530     C                   move      invfil        cmzdst
082900990920     C                   eval      cmzfld = vlrfou
083000990920     C                   eval      cmzmbd = vlrhdl
083100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
083200000710     C                   if        prmfir = *blanks
083300020913     C                   eval      cmzfla = 'FIVAB00F'
083400020913     C                   eval      cmzmba = 'FIVAB00F'
083500000710     C                   else
083600000710     C                   eval      cmzfla = prmfir
083700000710     C                   eval      cmzmba = prmfir
083800000710     C                   endif
083900990920     C                   eval      cmznrr = *zeros
084000990920     C                   move      §ctrok        cmznrr
084100021003     C                   eval      cmzlba = vlrfl1
084200990920     C                   call(e)   'TIS711C'
084300990920     C                   parm                    dscmz
084400990921     C                   parm      *blanks       esito
084500990920     C                   if        %error
084600990920     C                             or cmzerr = '1'
084700990921     C                             or esito  = '1'
084800000710     C                   eval      wrkesito = '3'
084900990920     C                   endif
085000990920     C*
085100000613     C                   ENDSR
085200000613     C***
085300010530
085400010530
085500010530
085600010530
085700010530      /TITLE Invio dei dati al punto operativo.
085800010530     C     opeini        BEGSR
085900010530     C*
086000010530     C* Inizializzo flag e contatori operativi
086100010530     C                   movel     '0'           flgGiro           1
086200010530     C                   movel     '0'           flgMulti          1
086300010530     C                   movel     '1'           flgStato          1
086400010615     C                   movel     '0'           flgOk             1
086500010530     C                   z-add     *zeros        cntNonEl         10 0
086600010530     C                   z-add     *zeros        depfil            3 0
086700010530     C                   z-add     *zeros        invfil            3 0
086800030407     C*
086900030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
087000030407     C                   if        %subst(vlrppt:1:1) = 'S'
087100030407     C                   seton                                        50
087200030407     C                   else
087300030407     C                   setoff                                       50
087400030407     C                   endif
087500041001     C*
087600041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
087700041001     C                   if        %subst(vlrppt:2:1) = 'S'
087800041001     C                   seton                                        51
087900041001     C                   else
088000041001     C                   setoff                                       51
088100041001     C                   endif
088200041025     C*
088300041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
088400041025     C* (naturalmente solo se nn già presente)
088500041025     C                   if        %subst(vlrppt:3:1) = 'S'
088600041025     C                   seton                                        52
088700041025     C                   else
088800041025     C                   setoff                                       52
088900041025     C                   endif
089000010530     C*
089100010530     C                   ENDSR
089200010530     C***
089300040407
089400040407
089500040407
089600040407
089700040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
089800040407     C     exeBon        BEGSR
089900040407     C*
090000040407     C* Imposto i parametri x il *pgm d "bonifica"
090100040407     C                   EVAL      ElencoChar = *blanks
090200040407     C                   EVAL      TipoElenco = *blanks
090300040407     C                   EVAL      CharSost   = *blanks
090400040629     C                   EVAL      UpperCase  = '1'
090500040407     C                   EVAL      ChkNull    = '1'
090600040407     C                   EVAL      CharNull   = *blanks
090700040629     C*
090800040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
090900040629     C                   EVAL      TxtInOut   = VABRSD
091000040629     C                   EVAL      EsitoBon   = *blanks
091100040407     C*
091200040407     C                   CALL      'XCHKCHAR'
091300040407     C                   PARM                    TxtInOut
091400040407     C                   PARM                    ElencoChar
091500040407     C                   PARM                    TipoElenco
091600040407     C                   PARM                    CharSost
091700040407     C                   PARM                    UpperCase
091800040407     C                   PARM                    ChkNull
091900040407     C                   PARM                    CharNull
092000040407     C                   PARM                    EsitoBon
092100040407     C*
092200040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
092300040407     C                   IF        EsitoBon = '1'
092400040629     C                   EVAL      VABRSD = TxtInOut
092500040407     C                   ENDIF
092600040629     C*
092700040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
092800040629     C                   EVAL      TxtInOut   = VABIND
092900040629     C                   EVAL      EsitoBon   = *blanks
093000040629     C*
093100040629     C                   CALL      'XCHKCHAR'
093200040629     C                   PARM                    TxtInOut
093300040629     C                   PARM                    ElencoChar
093400040629     C                   PARM                    TipoElenco
093500040629     C                   PARM                    CharSost
093600040629     C                   PARM                    UpperCase
093700040629     C                   PARM                    ChkNull
093800040629     C                   PARM                    CharNull
093900040629     C                   PARM                    EsitoBon
094000040629     C*
094100040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
094200040629     C                   IF        EsitoBon = '1'
094300040629     C                   EVAL      VABIND = TxtInOut
094400040629     C                   ENDIF
094500040629     C*
094600040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
094700040629     C                   EVAL      TxtInOut   = VABLOD
094800040629     C                   EVAL      EsitoBon   = *blanks
094900040629     C*
095000040629     C                   CALL      'XCHKCHAR'
095100040629     C                   PARM                    TxtInOut
095200040629     C                   PARM                    ElencoChar
095300040629     C                   PARM                    TipoElenco
095400040629     C                   PARM                    CharSost
095500040629     C                   PARM                    UpperCase
095600040629     C                   PARM                    ChkNull
095700040629     C                   PARM                    CharNull
095800040629     C                   PARM                    EsitoBon
095900040629     C*
096000040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
096100040629     C                   IF        EsitoBon = '1'
096200040629     C                   EVAL      VABLOD = TxtInOut
096300040629     C                   ENDIF
096400040629     C*
096500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
096600040629     C                   EVAL      TxtInOut   = VABNOT
096700040629     C                   EVAL      EsitoBon   = *blanks
096800040630     C                   EVAL      UpperCase  = *blanks
096900040629     C*
097000040629     C                   CALL      'XCHKCHAR'
097100040629     C                   PARM                    TxtInOut
097200040629     C                   PARM                    ElencoChar
097300040629     C                   PARM                    TipoElenco
097400040629     C                   PARM                    CharSost
097500040629     C                   PARM                    UpperCase
097600040629     C                   PARM                    ChkNull
097700040629     C                   PARM                    CharNull
097800040629     C                   PARM                    EsitoBon
097900040629     C*
098000040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
098100040629     C                   IF        EsitoBon = '1'
098200040629     C                   EVAL      VABNOT = TxtInOut
098300040629     C                   ENDIF
098400040629     C*
098500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
098600040629     C                   EVAL      TxtInOut   = VABNT2
098700040629     C                   EVAL      EsitoBon   = *blanks
098800040630     C                   EVAL      UpperCase  = *blanks
098900040629     C*
099000040629     C                   CALL      'XCHKCHAR'
099100040629     C                   PARM                    TxtInOut
099200040629     C                   PARM                    ElencoChar
099300040629     C                   PARM                    TipoElenco
099400040629     C                   PARM                    CharSost
099500040629     C                   PARM                    UpperCase
099600040629     C                   PARM                    ChkNull
099700040629     C                   PARM                    CharNull
099800040629     C                   PARM                    EsitoBon
099900040629     C*
100000040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100100040629     C                   IF        EsitoBon = '1'
100200040629     C                   EVAL      VABNT2 = TxtInOut
100300040629     C                   ENDIF
100400040407     C*
100500040407     C                   ENDSR
100600010530
100700060410
100800060410     C*----------------------------------------------------*
100900060410     C*  CONTROLLO NUMERICITA' CAMPI
101000060410     C*----------------------------------------------------*
101100060410     C     CHKNUM        BEGSR
101200060410     C*
101300060410     C                   IF        PiDecChr = *blanks
101400060410     C                   EVAL      PiDecChr = ','
101500060410     C                   ENDIF
101600060410     C*
101700060410     C                   CALL(e)   'ISNUMERIC'
101800060410     C                   PARM                    PiStr            30
101900060410     C                   PARM                    PiDecChr          1
102000060410     C                   PARM      *ZEROS        PiVal            30 9
102100060410     C                   PARM      '0'           PiInt             1
102200060410     C                   PARM      '0'           PiNum             1
102300060410     C                   IF        %error
102400060410     C                   EVAL      PiNum=*off
102500060410     C                   ENDIF
102600060410     C*
102700060410     C                   ENDSR
102800060410     C***
102900060410
103000990910
103100000613     C     *inzsr        BEGSR
103200990910     C*
103300990910     C     *entry        plist
103400990920     C                   parm                    tivlrds
103500990921     C                   parm      wrkesito      esito
103600000724     C                   parm                    prmlit
103700000710     C                   parm                    prmfir
103800010606     C*
103900010606     C* CALCOLA LA DATA CORRENTE
104000010606     C                   time                    wn14             14 0
104100010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
104200010606     C                   z-add     wn8           g08dat
104300010606     C                   z-add     *zeros        g08inv
104400010606     C                   movel     '0'           g08err
104500010606     C                   call      'XSRDA8'
104600010606     C                   parm                    wlbda8
104700010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
104800000613     C*
104900000613     C                   ENDSR
