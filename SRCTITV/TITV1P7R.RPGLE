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
010500010530     C                   ENDSR
010600010530     C***
010700990908
010800010530
010900010530
011000010530
011100910830     C*--------------------------------------------------------
011200020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
011300910830     C*--------------------------------------------------------
011400941229     C     RWVAB         BEGSR
011500990910     C*
011600990914     C                   if        not %open(tivin00r)
011700990908     C                   open      tivin00r
011800990914     C                   endif
011900020913     C                   if        not %open(fivabwwr)
012000020913     C                   open      fivabwwr
012100990914     C                   endif
012200990910     C*
012300990910     C                   clear                   §CTROK
012400990910     C                   clear                   §CTRMO
012500990910     C                   clear                   §CTRNO
012600990910     C*
012700921023     C                   DO        *HIVAL
012800990913     C*
012900990915     C                   READ      tivin00r                               70
013000010618     C*
013100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013200010618     C                   if        vinflg = '1'
013300010618     C                   eval      flgOk = '1'
013400010618     C                   endif
013500010705     C*
013600010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
013700010705     C                   if        %subst(vindta:1:1) = '='
013800010705     C                   eval      %subst(vindta:1:1) = *blanks
013900010705     C                   endif
014000010705     C*
014100051017     C                   if        vindta  > *blanks
014200000613     C                   add       1             rrnum
014300010601     C* ...elimino eventuali segni
014400010601     C     '+':' '       xlate     vindta        vindta
014500990913     C*
014600010601     C                   if        *in70 = *off and
014700990913     C                             (vinflg = *blanks
014800010711     C                              or vinflg = '0'
014900010711     C                              or vinflg = '2')
015000990913     C*
015100020913     C                   clear                   FIVAB000
015200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015300010711     C                   if        vinflg = *blanks or vinflg = '0'
015400010711     C                   clear                   vinmsg
015500010711     C                   endif
015600010711     C*
015700990913     C                   eval      fnvabaar = vindta
015800991022     C*
015900991022     C                   testb     x'3F'         vhbatb                   02
016000991022     C                   if        not *in02
016100991022     C*
016200971021     C                   SETOFF                                       303132
016300971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
016400971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
016500971023     C*    OPPURE TUTTI BLANKS
016600010530     C*
016700010530     C                   TESTN                   VHBLNP               30
016800010530     C  N30              TESTN                   VHBLNP                 30
016900010530     C  N30              TESTN                   VHBLNP                   30
017000010530     C                   MOVE      VHBLNP        W001A             1
017100010530     C     *IN30         IFEQ      *OFF
017200010530     C     W001A         ORLT      '0'
017300010530     C                   SETON                                        31
017400010530     C                   eval      vinmsg = %trimr(vinmsg)
017500010530     C                             + ' ' + 'VHBLNP'
017600010530     C                   ELSE
017700010530     C                   MOVE      VHBLNP        VABLNP
017800010530     C     VABLNP        IFLE      *ZEROS
017900010530     C                   SETON                                        31
018000010530     C                   eval      vinmsg = %trimr(vinmsg)
018100010530     C                             + ' ' + 'VHBLNP'
018200010530     C                   ENDIF
018300010530     C                   ENDIF
018400010530     C*
018500010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018600010530     C                   eval      depfil = VABLNP
018700010530     C                   exsr      repfil
018800010601     C                   if        depfil = invfil
018900021014     C                   if        vlrpoi = 999
019000021010     C                   MOVE(P)   invfil        VABFGS
019100021014     C                   else
019200021014     C                   MOVE(P)   vlrpoi        VABFGS
019300021014     C                   endif
019400010530     C*
019500010530     C                   exsr      PREELA
019600010530     C*
019700971023     C                   TESTN                   VHBCCM               30
019800971023     C  N30              TESTN                   VHBCCM                 30
019900971023     C  N30              TESTN                   VHBCCM                   30
020000971021     C                   MOVE      VHBCCM        W001A             1
020100971021     C     *IN30         IFEQ      *OFF
020200971021     C     W001A         ORLT      '0'
020300971021     C                   SETON                                        31
020400010604     C                   eval      vinmsg = %trimr(vinmsg)
020500010604     C                             + ' ' + 'VHBCCM'
020600971021     C                   ELSE
020700971021     C                   MOVE      VHBCCM        VABCCM
020800971024     C     VABCCM        IFLE      *ZEROS
020900971024     C                   SETON                                        31
021000010604     C                   eval      vinmsg = %trimr(vinmsg)
021100010604     C                             + ' ' + 'VHBCCM'
021200971024     C                   ENDIF
021300971021     C                   ENDIF
021400971021     C*
021500971023     C                   TESTN                   VHBAAS               30
021600971023     C  N30              TESTN                   VHBAAS                 30
021700971023     C  N30              TESTN                   VHBAAS                   30
021800971021     C                   MOVE      VHBAAS        W001A             1
021900971021     C     *IN30         IFEQ      *OFF
022000971021     C     W001A         ORLT      '0'
022100971023     C     W001A         ANDNE     *BLANK
022200971021     C                   SETON                                        32
022300010606     C                   MOVEL     datcor        VABAAS
022400010604     C                   eval      vinmsg = %trimr(vinmsg)
022500010604     C                             + ' ' + 'VHBAAS'
022600971021     C                   ELSE
022700971021     C                   MOVE      VHBAAS        VABAAS
022800971021     C                   ENDIF
022900971021     C*
023000971023     C                   TESTN                   VHBMGS               30
023100971023     C  N30              TESTN                   VHBMGS                 30
023200971023     C  N30              TESTN                   VHBMGS                   30
023300971021     C                   MOVE      VHBMGS        W001A             1
023400971021     C     *IN30         IFEQ      *OFF
023500971021     C     W001A         ORLT      '0'
023600971023     C     W001A         ANDNE     *BLANK
023700971021     C                   SETON                                        32
023800010601     C                   MOVE      '0101'        VABMGS
023900010604     C                   eval      vinmsg = %trimr(vinmsg)
024000010604     C                             + ' ' + 'VHBMGS'
024100971021     C                   ELSE
024200971021     C                   MOVE      VHBMGS        VABMGS
024300971021     C                   ENDIF
024400971021     C*
024500971023     C                   TESTN                   VHBNRS               30
024600971023     C  N30              TESTN                   VHBNRS                 30
024700971023     C  N30              TESTN                   VHBNRS                   30
024800971021     C                   MOVE      VHBNRS        W001A             1
024900971021     C     *IN30         IFEQ      *OFF
025000971021     C     W001A         ORLT      '0'
025100971023     C     W001A         ANDNE     *BLANK
025200971021     C                   SETON                                        32
025300971021     C                   MOVE      *ZERO         VABNRS
025400010604     C                   eval      vinmsg = %trimr(vinmsg)
025500010604     C                             + ' ' + 'VHBNRS'
025600971021     C                   ELSE
025700971021     C                   MOVE      VHBNRS        VABNRS
025800971021     C                   ENDIF
025900971021     C*
026000971023     C                   TESTN                   VHBNSP               30
026100971023     C  N30              TESTN                   VHBNSP                 30
026200971023     C  N30              TESTN                   VHBNSP                   30
026300971021     C                   MOVE      VHBNSP        W001A             1
026400971021     C     *IN30         IFEQ      *OFF
026500971021     C     W001A         ORLT      '0'
026600971023     C     W001A         ANDNE     *BLANK
026700971021     C                   SETON                                        32
026800971021     C                   MOVE      *ZERO         VABNSP
026900010604     C                   eval      vinmsg = %trimr(vinmsg)
027000010604     C                             + ' ' + 'VHBNSP'
027100971021     C                   ELSE
027200971021     C                   MOVE      VHBNSP        VABNSP
027300971021     C                   ENDIF
027400971021     C*
027500971023     C                   TESTN                   VHBLNA               30
027600971023     C  N30              TESTN                   VHBLNA                 30
027700971023     C  N30              TESTN                   VHBLNA                   30
027800971021     C                   MOVE      VHBLNA        W001A             1
027900971021     C     *IN30         IFEQ      *OFF
028000971021     C     W001A         ORLT      '0'
028100971023     C     W001A         ANDNE     *BLANK
028200971021     C                   SETON                                        32
028300971021     C                   MOVE      *ZERO         VABLNA
028400010604     C                   eval      vinmsg = %trimr(vinmsg)
028500010604     C                             + ' ' + 'VHBLNA'
028600971021     C                   ELSE
028700971021     C                   MOVE      VHBLNA        VABLNA
028800971021     C                   ENDIF
028900971021     C*
029000971023     C                   TESTN                   VHBCTR               30
029100971023     C  N30              TESTN                   VHBCTR                 30
029200971023     C  N30              TESTN                   VHBCTR                   30
029300971021     C                   MOVE      VHBCTR        W001A             1
029400971021     C     *IN30         IFEQ      *OFF
029500971021     C     W001A         ORLT      '0'
029600971023     C     W001A         ANDNE     *BLANK
029700971021     C                   SETON                                        32
029800971021     C                   MOVE      *ZERO         VABCTR
029900010604     C                   eval      vinmsg = %trimr(vinmsg)
030000010604     C                             + ' ' + 'VHBCTR'
030100971021     C                   ELSE
030200971021     C                   MOVE      VHBCTR        VABCTR
030300971021     C                   ENDIF
030400941229     C*
030500971021     C                   MOVE      *ZERO         VABIAS
030600941229     C                   MOVE      VHBIAS        WASC15
030700971023     C                   TESTN                   WI15                 30
030800971023     C  N30              TESTN                   WI15                   30
030900971023     C  N30              TESTN                   WI15                     30
031000971021     C                   MOVE      WI15          W001A             1
031100971021     C     *IN30         IFEQ      *OFF
031200971021     C     W001A         ORLT      '0'
031300971023     C     W001A         ANDNE     *BLANK
031400971021     C                   SETON                                        32
031500971021     C                   MOVE      *ZERO         VABIAS
031600010604     C                   eval      vinmsg = %trimr(vinmsg)
031700010604     C                             + ' ' + 'VHBIAS'
031800971021     C                   ELSE
031900971021     C                   MOVEL     WI15          VABIAS
032000971023     C                   TESTN                   WD15                 30
032100971023     C  N30              TESTN                   WD15                   30
032200971023     C  N30              TESTN                   WD15                     30
032300971021     C                   MOVE      WD15          W001A             1
032400971021     C     *IN30         IFEQ      *OFF
032500971021     C     W001A         ORLT      '0'
032600971023     C     W001A         ANDNE     *BLANK
032700971021     C                   SETON                                        32
032800971021     C                   MOVE      *ZERO         VABIAS
032900010604     C                   eval      vinmsg = %trimr(vinmsg)
033000010604     C                             + ' ' + 'VHBIAS'
033100971021     C                   ELSE
033200971021     C                   MOVE      WD15          VABIAS
033300971021     C                   ENDIF
033400050118     C                   ENDIF
033500941229     C*
033600971023     C                   TESTN                   VHBNCL               30
033700971023     C  N30              TESTN                   VHBNCL                 30
033800971023     C  N30              TESTN                   VHBNCL                   30
033900971021     C                   MOVE      VHBNCL        W001A             1
034000971021     C     *IN30         IFEQ      *OFF
034100971021     C     W001A         ORLT      '0'
034200971021     C                   SETON                                        32
034300971021     C                   MOVE      *ZERO         VABNCL
034400010604     C                   eval      vinmsg = %trimr(vinmsg)
034500010604     C                             + ' ' + 'VHBNCL'
034600971021     C                   ELSE
034700971021     C                   MOVE      VHBNCL        VABNCL
034800971021     C                   ENDIF
034900941229     C*
035000971021     C                   MOVE      *ZERO         VABPKB
035100971021     C                   MOVE      VHBPKB        WASC09
035200010719     C                   IF        WI09 = *blanks
035300010719     C                   EVAL      WI09 = *zeros
035400010719     C                   ENDIF
035500971023     C                   TESTN                   WI09                 30
035600971023     C  N30              TESTN                   WI09                   30
035700971023     C  N30              TESTN                   WI09                     30
035800971021     C                   MOVE      WI09          W001A             1
035900971021     C     *IN30         IFEQ      *OFF
036000971021     C     W001A         ORLT      '0'
036100971021     C                   SETON                                        32
036200971021     C                   MOVE      *ZERO         VABPKB
036300010604     C                   eval      vinmsg = %trimr(vinmsg)
036400010604     C                             + ' ' + 'VHBPKB'
036500971021     C                   ELSE
036600971021     C                   MOVEL     WI09          VABPKB
036700010719     C                   IF        WD09 = *blanks
036800010719     C                   EVAL      WD09 = *zeros
036900010719     C                   ENDIF
037000971023     C                   TESTN                   WD09                 30
037100971023     C  N30              TESTN                   WD09                     30
037200971021     C                   MOVE      WD09          W001A             1
037300971021     C     *IN30         IFEQ      *OFF
037400971021     C     W001A         ORLT      '0'
037500971023     C     W001A         ANDNE     *BLANK
037600971021     C                   SETON                                        32
037700971021     C                   MOVE      *ZERO         VABPKB
037800010604     C                   eval      vinmsg = %trimr(vinmsg)
037900010604     C                             + ' ' + 'VHBPKB'
038000971021     C                   ELSE
038100971021     C                   MOVE      WD09          VABPKB
038200971021     C                   ENDIF
038300050118     C                   ENDIF
038400971021     C*
038500041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
038600041001     C                   IF        *IN51 = *OFF
038700971021     C                   MOVE      *ZERO         VABVLB
038800971021     C                   MOVE      VHBVLB        WASC07
038900010719     C                   IF        WI07 = *blanks
039000010719     C                   EVAL      WI07 = *zeros
039100010719     C                   ENDIF
039200971023     C                   TESTN                   WI07                 30
039300971023     C  N30              TESTN                   WI07                   30
039400971023     C  N30              TESTN                   WI07                     30
039500971021     C                   MOVE      WI07          W001A             1
039600971021     C     *IN30         IFEQ      *OFF
039700971021     C     W001A         ORLT      '0'
039800971023     C     W001A         ANDNE     *BLANK
039900971021     C                   SETON                                        32
040000971021     C                   MOVE      *ZERO         VABVLB
040100010604     C                   eval      vinmsg = %trimr(vinmsg)
040200010604     C                             + ' ' + 'VHBVLB'
040300971021     C                   ELSE
040400971021     C                   MOVEL     WI07          VABVLB
040500010719     C                   IF        WD07 = *blanks
040600010719     C                   EVAL      WD07 = *zeros
040700010719     C                   ENDIF
040800971023     C                   TESTN                   WD07                 30
040900971023     C  N30              TESTN                   WD07                   30
041000971023     C  N30              TESTN                   WD07                     30
041100971021     C                   MOVE      WD07          W001A             1
041200971021     C     *IN30         IFEQ      *OFF
041300971021     C     W001A         ORLT      '0'
041400971023     C     W001A         ANDNE     *BLANK
041500971021     C                   SETON                                        32
041600971021     C                   MOVE      *ZERO         VABVLB
041700010604     C                   eval      vinmsg = %trimr(vinmsg)
041800010604     C                             + ' ' + 'VHBVLB'
041900971021     C                   ELSE
042000971021     C                   MOVE      WD07          VABVLB
042100971021     C                   ENDIF
042200050118     C                   ENDIF
042300041001     C                   ELSE
042400041001     C* ...altrimenti forzo azzeramento del volume
042500041001     C                   Z-ADD     *zeros        VABVLB
042600041001     C                   ENDIF
042700941229     C*
042800971021     C                   MOVE      *ZERO         VABQFT
042900971021     C                   MOVE      VHBQFT        WASC15
043000010719     C                   IF        WI15 = *blanks
043100010719     C                   EVAL      WI15 = *zeros
043200010719     C                   ENDIF
043300971023     C                   TESTN                   WI15                 30
043400971023     C  N30              TESTN                   WI15                   30
043500971023     C  N30              TESTN                   WI15                     30
043600971021     C                   MOVE      WI15          W001A             1
043700971021     C     *IN30         IFEQ      *OFF
043800971021     C     W001A         ORLT      '0'
043900971023     C     W001A         ANDNE     *BLANK
044000971021     C                   SETON                                        32
044100971021     C                   MOVE      *ZERO         VABQFT
044200010604     C                   eval      vinmsg = %trimr(vinmsg)
044300010604     C                             + ' ' + 'VHBQFT'
044400971021     C                   ELSE
044500971021     C                   MOVEL     WI15          VABQFT
044600010719     C                   IF        WD15 = *blanks
044700010719     C                   EVAL      WD15 = *zeros
044800010719     C                   ENDIF
044900971023     C                   TESTN                   WD15                 30
045000971023     C  N30              TESTN                   WD15                   30
045100971023     C  N30              TESTN                   WD15                     30
045200971021     C                   MOVE      WD15          W001A             1
045300971021     C     *IN30         IFEQ      *OFF
045400971021     C     W001A         ORLT      '0'
045500971023     C     W001A         ANDNE     *BLANK
045600971021     C                   SETON                                        32
045700971021     C                   MOVE      *ZERO         VABQFT
045800010604     C                   eval      vinmsg = %trimr(vinmsg)
045900010604     C                             + ' ' + 'VHBQFT'
046000971021     C                   ELSE
046100971021     C                   MOVE      WD15          VABQFT
046200971021     C                   ENDIF
046300050118     C                   ENDIF
046400941229     C*
046500971021     C                   MOVE      *ZERO         VABCAS
046600060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
046700060410     C                   EVAL      wVHBCAS = VHBCAS
046800060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
046900060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
047000060410     C                   ENDIF
047100060410     C*
047200060410     C                   MOVEL(P)  wVHBCAS       PiStr
047300060410     C                   EXSR      CHKNUM
047400060410     C                   IF        PiNum=*on
047500060410     C                   Z-ADD     PiVal         VABCAS
047600060410     C                   ELSE
047700060410     C                   SETON                                        32
047800060410     C                   Z-ADD     *zeros        VABCAS
047900060410     C                   EVAL      vinmsg = %trimr(vinmsg)
048000060410     C                             + ' ' + 'VHBCAS'
048100060410     C                   ENDIF
048200971021     C*
048300971023     C                   TESTN                   VHBRMN               30
048400971023     C  N30              TESTN                   VHBRMN                 30
048500971023     C  N30              TESTN                   VHBRMN                   30
048600971021     C                   MOVE      VHBRMN        W001A             1
048700971021     C     *IN30         IFEQ      *OFF
048800971021     C     W001A         ORLT      '0'
048900971021     C                   SETON                                        32
049000971021     C                   MOVE      *ZERO         VABRMN
049100010604     C                   eval      vinmsg = %trimr(vinmsg)
049200010604     C                             + ' ' + 'VHBRMN'
049300971021     C                   ELSE
049400971021     C                   MOVE      VHBRMN        VABRMN
049500971021     C                   ENDIF
049600971021     C*
049700971023     C                   TESTN                   VHBNCD               30
049800971023     C  N30              TESTN                   VHBNCD                 30
049900971023     C  N30              TESTN                   VHBNCD                   30
050000971021     C                   MOVE      VHBNCD        W001A             1
050100971021     C     *IN30         IFEQ      *OFF
050200971021     C     W001A         ORLT      '0'
050300971023     C     W001A         ANDNE     *BLANK
050400971021     C                   SETON                                        32
050500971021     C                   MOVE      *ZERO         VABNCD
050600010604     C                   eval      vinmsg = %trimr(vinmsg)
050700010604     C                             + ' ' + 'VHBNCD'
050800971021     C                   ELSE
050900971021     C                   MOVE      VHBNCD        VABNCD
051000971021     C                   ENDIF
051100971021     C*
051200971023     C                   TESTN                   VHBNCA               30
051300971023     C  N30              TESTN                   VHBNCA                 30
051400971023     C  N30              TESTN                   VHBNCA                   30
051500971021     C                   MOVE      VHBNCA        W001A             1
051600971021     C     *IN30         IFEQ      *OFF
051700971021     C     W001A         ORLT      '0'
051800971023     C     W001A         ANDNE     *BLANK
051900971021     C                   SETON                                        32
052000971021     C                   MOVE      *ZERO         VABNCA
052100010604     C                   eval      vinmsg = %trimr(vinmsg)
052200010604     C                             + ' ' + 'VHBNCA'
052300971021     C                   ELSE
052400971021     C                   MOVE      VHBNCA        VABNCA
052500971021     C                   ENDIF
052600971021     C*
052700971023     C                   TESTN                   VHBZNC               30
052800971023     C  N30              TESTN                   VHBZNC                 30
052900971023     C  N30              TESTN                   VHBZNC                   30
053000971021     C                   MOVE      VHBZNC        W001A             1
053100971021     C     *IN30         IFEQ      *OFF
053200971021     C     W001A         ORLT      '0'
053300971023     C     W001A         ANDNE     *BLANK
053400971021     C                   SETON                                        32
053500971021     C                   MOVE      *ZERO         VABZNC
053600010604     C                   eval      vinmsg = %trimr(vinmsg)
053700010604     C                             + ' ' + 'VHBZNC'
053800971021     C                   ELSE
053900971021     C                   MOVE      VHBZNC        VABZNC
054000971021     C                   ENDIF
054100971021     C*
054200971023     C                   TESTN                   VHBDCR               30
054300971023     C  N30              TESTN                   VHBDCR                 30
054400971023     C  N30              TESTN                   VHBDCR                   30
054500971021     C                   MOVE      VHBDCR        W001A             1
054600971021     C     *IN30         IFEQ      *OFF
054700971021     C     W001A         ORLT      '0'
054800971023     C     W001A         ANDNE     *BLANK
054900971021     C                   SETON                                        32
055000971021     C                   MOVE      *ZERO         VABDCR
055100010604     C                   eval      vinmsg = %trimr(vinmsg)
055200010604     C                             + ' ' + 'VHBDCR'
055300971021     C                   ELSE
055400971021     C                   MOVE      VHBDCR        VABDCR
055500971021     C                   ENDIF
055600971021     C*
055700971023     C                   TESTN                   VHBHCR               30
055800971023     C  N30              TESTN                   VHBHCR                 30
055900971023     C  N30              TESTN                   VHBHCR                   30
056000971021     C                   MOVE      VHBHCR        W001A             1
056100971021     C     *IN30         IFEQ      *OFF
056200971021     C     W001A         ORLT      '0'
056300971023     C     W001A         ANDNE     *BLANK
056400971021     C                   SETON                                        32
056500971021     C                   MOVE      *ZERO         VABHCR
056600010604     C                   eval      vinmsg = %trimr(vinmsg)
056700010604     C                             + ' ' + 'VHBHCR'
056800971021     C                   ELSE
056900971021     C                   MOVE      VHBHCR        VABHCR
057000971021     C                   ENDIF
057100941229     C*
057200971021     C                   MOVE      *ZERO         VABVMD
057300971021     C                   MOVE      VHBVMD        WASC15
057400010719     C                   IF        WI15 = *blanks
057500010719     C                   EVAL      WI15 = *zeros
057600010719     C                   ENDIF
057700971023     C                   TESTN                   WI15                 30
057800971023     C  N30              TESTN                   WI15                   30
057900971023     C  N30              TESTN                   WI15                     30
058000971021     C                   MOVE      WI15          W001A             1
058100971021     C     *IN30         IFEQ      *OFF
058200971021     C     W001A         ORLT      '0'
058300971023     C     W001A         ANDNE     *BLANK
058400971021     C                   SETON                                        32
058500971021     C                   MOVE      *ZERO         VABVMD
058600010604     C                   eval      vinmsg = %trimr(vinmsg)
058700010604     C                             + ' ' + 'VHBVMD'
058800971021     C                   ELSE
058900971021     C                   MOVEL     WI15          VABVMD
059000010719     C                   IF        WD15 = *blanks
059100010719     C                   EVAL      WD15 = *zeros
059200010719     C                   ENDIF
059300971023     C                   TESTN                   WD15                 30
059400971023     C  N30              TESTN                   WD15                   30
059500971023     C  N30              TESTN                   WD15                     30
059600971021     C                   MOVE      WD15          W001A             1
059700971021     C     *IN30         IFEQ      *OFF
059800971021     C     W001A         ORLT      '0'
059900971023     C     W001A         ANDNE     *BLANK
060000971021     C                   SETON                                        32
060100971021     C                   MOVE      *ZERO         VABVMD
060200010604     C                   eval      vinmsg = %trimr(vinmsg)
060300010604     C                             + ' ' + 'VHBVMD'
060400971021     C                   ELSE
060500971021     C                   MOVE      WD15          VABVMD
060600971021     C                   ENDIF
060700050118     C                   ENDIF
060800971021     C*
060900971023     C                   TESTN                   VHBANT               30
061000971023     C  N30              TESTN                   VHBANT                 30
061100971023     C  N30              TESTN                   VHBANT                   30
061200971021     C                   MOVE      VHBANT        W001A             1
061300971021     C     *IN30         IFEQ      *OFF
061400971021     C     W001A         ORLT      '0'
061500971023     C     W001A         ANDNE     *BLANK
061600971021     C                   SETON                                        32
061700971021     C                   MOVE      *ZERO         VABANT
061800010604     C                   eval      vinmsg = %trimr(vinmsg)
061900010604     C                             + ' ' + 'VHBANT'
062000971021     C                   ELSE
062100971021     C                   MOVE      VHBANT        VABANT
062200971021     C                   ENDIF
062300971021     C*
062400971021     C* CAMPI ALFABETICI:
062500971021     C                   MOVE      VHBATB        VABATB
062600971021     C                   MOVE      VHBCBO        VABCBO
062700971021     C                   MOVE      VHBRSD        VABRSD
062800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
062900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
063000020117     C* ==
063100971021     C                   MOVE      VHBRD2        VABRD2
063200971021     C                   MOVE      VHBIND        VABIND
063300971021     C                   MOVE      VHBCAD        VABCAD
063400971021     C                   MOVE      VHBLOD        VABLOD
063500971021     C                   MOVE      VHBPRD        VABPRD
063600971021     C                   MOVE      VHBNZD        VABNZD
063700971021     C                   MOVE      VHBGC1        VABGC1
063800971021     C                   MOVE      VHBGC2        VABGC2
063900971021     C                   MOVE      VHBTSP        VABTSP
064000971021     C                   MOVE      VHBVAS        VABVAS
064100971021     C                   MOVE      VHBNAS        VABNAS
064200971021     C                   MOVE      VHBTIC        VABTIC
064300971021     C                   MOVE      VHBVCA        VABVCA
064400971021     C                   MOVE      VHBGCA        VABGCA
064500971021     C                   MOVE      VHBRMA        VABRMA
064600971021     C                   MOVE      VHBXCO        VABXCO
064700971021     C                   MOVE      VHBNOT        VABNOT
064800971021     C                   MOVE      VHBNT2        VABNT2
064900971021     C                   MOVE      VHBCTM        VABCTM
065000971021     C                   MOVE      VHBFFD        VABFFD
065100971021     C                   MOVE      VHBTCR        VABTCR
065200971021     C                   MOVE      VHBCTS        VABCTS
065300971021     C                   MOVE      VHBFTM        VABFTM
065400971021     C                   MOVE      VHBVAD        VABVAD
065500971021     C                   MOVE      VHBGMA        VABGMA
065600971021     C                   MOVE      VHBGGA        VABGGA
065700971021     C                   MOVE      VHBGVA        VABGVA
065800971021     C                   MOVE      VHBTC1        VABTC1
065900971021     C                   MOVE      VHBTC2        VABTC2
066000971021     C                   MOVE      VHBSCL        VABSCL
066100971021     C                   MOVE      VHBRMO        VABRMO
066200971021     C                   MOVE      VHBCMO        VABCMO
066300971021     C                   MOVE      VHBNMO        VABNMO
066400971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
066500060329     C* NELLA NAZIONE MITTENTE ORIGINALE
066600060331     C                   IF        %trim(VHBNMO) < 'A'
066700060329     C                   MOVE      *BLANKS       VABNMO
066800060329     C                   ENDIF
066900090519     C*
067000090519     C* PERSONALIZZAZIONE: RMA = RMN
067100090519     C                   MOVE(P)   VABRMN        VABRMA
067200050418     C*
067300050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
067400050418     C                   IF        %trim(VABNZD) = 'I'   OR
067500050418     C                             %trim(VABNZD) = 'IT'  OR
067600050418     C                             %trim(VABNZD) = 'ITA'
067700050418     C                   EVAL      VABNZD = *blanks
067800050418     C                   ENDIF
067900041025     C*
068000041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
068100041025     C                   IF        *IN52 = *ON
068200041025     C* Reperisco la provincia dal CAP e dalla località
068300041025     C                   IF        VABCAD <> *blanks AND
068400050907     C                             VABPRD  = *blanks AND
068500050907     C                             VABNZD  = *blanks
068600041025     C                   CLEAR                   TISI95DS
068700041025     C                   EVAL      I95TCN = '3'
068800041025     C                   Z-ADD     datcor        I95DAT
068900041025     C                   EVAL      I95CAP = VABCAD
069000041025     C                   EVAL      I95LOC = VABLOD
069100041025     C                   CALL      'TISI95R'
069200041025     C                   PARM                    TISI95DS
069300041025     C                   EVAL      VABPRD = O95PRV
069400041025     C                   ENDIF
069500041025     C                   ENDIF
069600040630     C*
069700040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
069800040630     C                   exsr      exeBon
069900930409     C*
070000010604     C  N31              ADD       1             §CTROK            7 0
070100010604     C   32              ADD       1             §CTRMO            7 0
070200010604     C   31              ADD       1             §CTRNO            7 0
070300020913     C  N31              WRITE     FIVAB000
070400990910     C*
070500010601     C                   if        *in31 = *off and
070600010601     C                             *in32 = *off
070700990910     C                   eval      vinflg = '1'
070800990910     C                   else
070900990910     C                   eval      vinflg = '2'
071000990910     C                   endif
071100991022     C                   endif
071200990910     C                   endif
071300010601     C*
071400010601     C                   endif
071500000905     C*
071600000905     C                   else
071700000905     C                   eval      vinflg = '1'
071800000905     C                   endif
071900000905     C*
072000000905     C  N70              update    tivin000
072100991022     C*
072200991022     C  N70              ENDdo
072300010530     C
072400010530     C*
072500010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
072600010530     C                   if        cntNonEl = *zeros or
072700010530     C                             flgMulti = '0'
072800990910     C* Se non ci sono record con errori ...
072900010530     C                   if        §ctrno = 0 and
073000010601     C                             §ctrmo = 0 and
073100010530     C                             flgStato <> '2'
073200990910     C* ... restituisco esito OK.
073300990921     C                   eval      wrkesito = '0'
073400990910     C                   else
073500010601     C                   if        §ctrok > 0
073600990921     C                   eval      wrkesito = '1'
073700000710     C                   else
073800010615     C                   if        flgOk = '0'
073900010615     C                   eval      wrkesito = '2'
074000010615     C                   else
074100010615     C                   eval      wrkesito = '6'
074200010615     C                   endif
074300990910     C                   endif
074400000710     C                   endif
074500010530     C                   else
074600010530     C                   eval      wrkesito = '9'
074700010530     C                   endif
074800990910     C*
074900990914     C                   if        %open(tivin00r)
075000990908     C                   close     tivin00r
075100990914     C                   endif
075200020913     C                   if        %open(fivabwwr)
075300020913     C                   close     fivabwwr
075400990914     C                   endif
075500010530     C*
075600010530     C                   if        vlrpoi <> 999
075700010530     C                   eval      invfil = vlrpoi
075800010530     C                   endif
075900990910     C*
076000990920     C                   if        §ctrok > 0
076100010530     C                             and invfil > *zeros
076200000613     C                   exsr      invio
076300990920     C                   endif
076400010612     C*
076500010612     C                   if        flgGiro = '1'
076600010612     C                   exsr      endela
076700010612     C                   endif
076800990920     C*
076900910830     C                   ENDSR
077000000613     C***
077100990920
077200010530
077300010530
077400010530
077500010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
077600010530     C     repfil        BEGSR
077700010530     C*
077800010530     C                   if        invfil = *zeros and
077900010530     C                             depfil > *zeros and
078000010613     C                             (vinflg = *blanks or
078100010613     C                              vinflg = *zeros)
078200010530     C
078300010530     C                   eval      invfil = depfil
078400010530     C                   endif
078500010530     C*
078600010530     C                   if        depfil <> invfil and
078700010530     C                             invfil > *zeros
078800010530     C                   eval      flgMulti = '1'
078900010530     C                   if        vinflg = *blanks
079000010530     C                   add       1             cntNonEl
079100010530     C                   endif
079200010530     C                   endif
079300010530     C*
079400010530     C                   if        vinflg = '2'
079500010530     C                   eval      flgStato = '2'
079600010530     C                   endif
079700010530     C*
079800010530     C                   ENDSR
079900010530     C***
080000010530
080100010530
080200010530
080300010530
080400990920      /TITLE Invio dei dati al punto operativo.
080500000613     C     invio         BEGSR
080600990920     C*
080700990920     C                   reset                   dscmz
080800010530     C                   move      invfil        cmzdst
080900990920     C                   eval      cmzfld = vlrfou
081000990920     C                   eval      cmzmbd = vlrhdl
081100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
081200000710     C                   if        prmfir = *blanks
081300020913     C                   eval      cmzfla = 'FIVAB00F'
081400020913     C                   eval      cmzmba = 'FIVAB00F'
081500000710     C                   else
081600000710     C                   eval      cmzfla = prmfir
081700000710     C                   eval      cmzmba = prmfir
081800000710     C                   endif
081900990920     C                   eval      cmznrr = *zeros
082000990920     C                   move      §ctrok        cmznrr
082100021003     C                   eval      cmzlba = vlrfl1
082200990920     C                   call(e)   'TIS711C'
082300990920     C                   parm                    dscmz
082400990921     C                   parm      *blanks       esito
082500990920     C                   if        %error
082600990920     C                             or cmzerr = '1'
082700990921     C                             or esito  = '1'
082800000710     C                   eval      wrkesito = '3'
082900990920     C                   endif
083000990920     C*
083100000613     C                   ENDSR
083200000613     C***
083300010530
083400010530
083500010530
083600010530
083700010530      /TITLE Invio dei dati al punto operativo.
083800010530     C     opeini        BEGSR
083900010530     C*
084000010530     C* Inizializzo flag e contatori operativi
084100010530     C                   movel     '0'           flgGiro           1
084200010530     C                   movel     '0'           flgMulti          1
084300010530     C                   movel     '1'           flgStato          1
084400010615     C                   movel     '0'           flgOk             1
084500010530     C                   z-add     *zeros        cntNonEl         10 0
084600010530     C                   z-add     *zeros        depfil            3 0
084700010530     C                   z-add     *zeros        invfil            3 0
084800030407     C*
084900030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
085000030407     C                   if        %subst(vlrppt:1:1) = 'S'
085100030407     C                   seton                                        50
085200030407     C                   else
085300030407     C                   setoff                                       50
085400030407     C                   endif
085500041001     C*
085600041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
085700041001     C                   if        %subst(vlrppt:2:1) = 'S'
085800041001     C                   seton                                        51
085900041001     C                   else
086000041001     C                   setoff                                       51
086100041001     C                   endif
086200041025     C*
086300041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
086400041025     C* (naturalmente solo se nn già presente)
086500041025     C                   if        %subst(vlrppt:3:1) = 'S'
086600041025     C                   seton                                        52
086700041025     C                   else
086800041025     C                   setoff                                       52
086900041025     C                   endif
087000010530     C*
087100010530     C                   ENDSR
087200010530     C***
087300040407
087400040407
087500040407
087600040407
087700040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
087800040407     C     exeBon        BEGSR
087900040407     C*
088000040407     C* Imposto i parametri x il *pgm d "bonifica"
088100040407     C                   EVAL      ElencoChar = *blanks
088200040407     C                   EVAL      TipoElenco = *blanks
088300040407     C                   EVAL      CharSost   = *blanks
088400040629     C                   EVAL      UpperCase  = '1'
088500040407     C                   EVAL      ChkNull    = '1'
088600040407     C                   EVAL      CharNull   = *blanks
088700040629     C*
088800040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
088900040629     C                   EVAL      TxtInOut   = VABRSD
089000040629     C                   EVAL      EsitoBon   = *blanks
089100040407     C*
089200040407     C                   CALL      'XCHKCHAR'
089300040407     C                   PARM                    TxtInOut
089400040407     C                   PARM                    ElencoChar
089500040407     C                   PARM                    TipoElenco
089600040407     C                   PARM                    CharSost
089700040407     C                   PARM                    UpperCase
089800040407     C                   PARM                    ChkNull
089900040407     C                   PARM                    CharNull
090000040407     C                   PARM                    EsitoBon
090100040407     C*
090200040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
090300040407     C                   IF        EsitoBon = '1'
090400040629     C                   EVAL      VABRSD = TxtInOut
090500040407     C                   ENDIF
090600040629     C*
090700040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
090800040629     C                   EVAL      TxtInOut   = VABIND
090900040629     C                   EVAL      EsitoBon   = *blanks
091000040629     C*
091100040629     C                   CALL      'XCHKCHAR'
091200040629     C                   PARM                    TxtInOut
091300040629     C                   PARM                    ElencoChar
091400040629     C                   PARM                    TipoElenco
091500040629     C                   PARM                    CharSost
091600040629     C                   PARM                    UpperCase
091700040629     C                   PARM                    ChkNull
091800040629     C                   PARM                    CharNull
091900040629     C                   PARM                    EsitoBon
092000040629     C*
092100040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
092200040629     C                   IF        EsitoBon = '1'
092300040629     C                   EVAL      VABIND = TxtInOut
092400040629     C                   ENDIF
092500040629     C*
092600040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
092700040629     C                   EVAL      TxtInOut   = VABLOD
092800040629     C                   EVAL      EsitoBon   = *blanks
092900040629     C*
093000040629     C                   CALL      'XCHKCHAR'
093100040629     C                   PARM                    TxtInOut
093200040629     C                   PARM                    ElencoChar
093300040629     C                   PARM                    TipoElenco
093400040629     C                   PARM                    CharSost
093500040629     C                   PARM                    UpperCase
093600040629     C                   PARM                    ChkNull
093700040629     C                   PARM                    CharNull
093800040629     C                   PARM                    EsitoBon
093900040629     C*
094000040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
094100040629     C                   IF        EsitoBon = '1'
094200040629     C                   EVAL      VABLOD = TxtInOut
094300040629     C                   ENDIF
094400040629     C*
094500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
094600040629     C                   EVAL      TxtInOut   = VABNOT
094700040629     C                   EVAL      EsitoBon   = *blanks
094800040630     C                   EVAL      UpperCase  = *blanks
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
096200040629     C                   EVAL      VABNOT = TxtInOut
096300040629     C                   ENDIF
096400040629     C*
096500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
096600040629     C                   EVAL      TxtInOut   = VABNT2
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
098200040629     C                   EVAL      VABNT2 = TxtInOut
098300040629     C                   ENDIF
098400040407     C*
098500040407     C                   ENDSR
098600010530
098700060410
098800060410     C*----------------------------------------------------*
098900060410     C*  CONTROLLO NUMERICITA' CAMPI
099000060410     C*----------------------------------------------------*
099100060410     C     CHKNUM        BEGSR
099200060410     C*
099300060410     C                   IF        PiDecChr = *blanks
099400060410     C                   EVAL      PiDecChr = ','
099500060410     C                   ENDIF
099600060410     C*
099700060410     C                   CALL(e)   'ISNUMERIC'
099800060410     C                   PARM                    PiStr            30
099900060410     C                   PARM                    PiDecChr          1
100000060410     C                   PARM      *ZEROS        PiVal            30 9
100100060410     C                   PARM      '0'           PiInt             1
100200060410     C                   PARM      '0'           PiNum             1
100300060410     C                   IF        %error
100400060410     C                   EVAL      PiNum=*off
100500060410     C                   ENDIF
100600060410     C*
100700060410     C                   ENDSR
100800060410     C***
100900060410
101000990910
101100000613     C     *inzsr        BEGSR
101200990910     C*
101300990910     C     *entry        plist
101400990920     C                   parm                    tivlrds
101500990921     C                   parm      wrkesito      esito
101600000724     C                   parm                    prmlit
101700000710     C                   parm                    prmfir
101800010606     C*
101900010606     C* CALCOLA LA DATA CORRENTE
102000010606     C                   time                    wn14             14 0
102100010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
102200010606     C                   z-add     wn8           g08dat
102300010606     C                   z-add     *zeros        g08inv
102400010606     C                   movel     '0'           g08err
102500010606     C                   call      'XSRDA8'
102600010606     C                   parm                    wlbda8
102700010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
102800000613     C*
102900000613     C                   ENDSR
103000000613     C***
