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
066900050418     C*
067000050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
067100050418     C                   IF        %trim(VABNZD) = 'I'   OR
067200050418     C                             %trim(VABNZD) = 'IT'  OR
067300050418     C                             %trim(VABNZD) = 'ITA'
067400050418     C                   EVAL      VABNZD = *blanks
067500050418     C                   ENDIF
067600041025     C*
067700041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
067800041025     C                   IF        *IN52 = *ON
067900041025     C* Reperisco la provincia dal CAP e dalla località
068000041025     C                   IF        VABCAD <> *blanks AND
068100050907     C                             VABPRD  = *blanks AND
068200050907     C                             VABNZD  = *blanks
068300041025     C                   CLEAR                   TISI95DS
068400041025     C                   EVAL      I95TCN = '3'
068500041025     C                   Z-ADD     datcor        I95DAT
068600041025     C                   EVAL      I95CAP = VABCAD
068700041025     C                   EVAL      I95LOC = VABLOD
068800041025     C                   CALL      'TISI95R'
068900041025     C                   PARM                    TISI95DS
069000041025     C                   EVAL      VABPRD = O95PRV
069100041025     C                   ENDIF
069200041025     C                   ENDIF
069300040630     C*
069400040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
069500040630     C                   exsr      exeBon
069600100419     C*
069700100419     C* Forzatura su campi DIVISE
069800100419     C                   IF        VABVAS = *blanks
069900100419     C                   EVAL      VABVAS = 'EUR'
070000100419     C                   ENDIF
070100100419     C                   IF        VABVCA = *blanks
070200100419     C                   EVAL      VABVCA = 'EUR'
070300100419     C                   ENDIF
070400100419     C                   IF        VABVAD = *blanks
070500100419     C                   EVAL      VABVAD = 'EUR'
070600100419     C                   ENDIF
070700110429     C*
070800110429     C* Forzature x diversa gestione traffici ILLY
070900110429     C*
071000110429     C* CCM
071100110429     C                   MOVEL     VABNSP        wCLI              1
071200110429     C                   IF        wCLI = '7'
071300110429     C                   EVAL      VABCCM = 1162074
071400110429     C                   ENDIF
071500110429     C* TIC
071600110502     C                   IF        VABCCM = 1162074 AND
071700110502     C                             (VABCBO = '4' OR VABCBO = '6') AND
071800110502     C                             VABTIC = *blanks
071900110429     C                   EVAL      VABTIC = 'SC'
072000110429     C                   ENDIF
072100111125     C                   IF        VABCCM = 1161162 AND
072200111125     C                             VABCAS >= 500 AND VABCAS <= 1000 AND
072300111125     C                             VABTIC = *blanks
072400111125     C                   EVAL      VABTIC = 'SC'
072500111125     C                   ENDIF
072600930409     C*
072700010604     C  N31              ADD       1             §CTROK            7 0
072800010604     C   32              ADD       1             §CTRMO            7 0
072900010604     C   31              ADD       1             §CTRNO            7 0
073000020913     C  N31              WRITE     FIVAB000
073100990910     C*
073200010601     C                   if        *in31 = *off and
073300010601     C                             *in32 = *off
073400990910     C                   eval      vinflg = '1'
073500990910     C                   else
073600990910     C                   eval      vinflg = '2'
073700990910     C                   endif
073800991022     C                   endif
073900990910     C                   endif
074000010601     C*
074100010601     C                   endif
074200000905     C*
074300000905     C                   else
074400000905     C                   eval      vinflg = '1'
074500000905     C                   endif
074600000905     C*
074700000905     C  N70              update    tivin000
074800991022     C*
074900991022     C  N70              ENDdo
075000010530     C
075100010530     C*
075200010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
075300010530     C                   if        cntNonEl = *zeros or
075400010530     C                             flgMulti = '0'
075500990910     C* Se non ci sono record con errori ...
075600010530     C                   if        §ctrno = 0 and
075700010601     C                             §ctrmo = 0 and
075800010530     C                             flgStato <> '2'
075900990910     C* ... restituisco esito OK.
076000990921     C                   eval      wrkesito = '0'
076100990910     C                   else
076200010601     C                   if        §ctrok > 0
076300990921     C                   eval      wrkesito = '1'
076400000710     C                   else
076500010615     C                   if        flgOk = '0'
076600010615     C                   eval      wrkesito = '2'
076700010615     C                   else
076800010615     C                   eval      wrkesito = '6'
076900010615     C                   endif
077000990910     C                   endif
077100000710     C                   endif
077200010530     C                   else
077300010530     C                   eval      wrkesito = '9'
077400010530     C                   endif
077500990910     C*
077600990914     C                   if        %open(tivin00r)
077700990908     C                   close     tivin00r
077800990914     C                   endif
077900020913     C                   if        %open(fivabwwr)
078000020913     C                   close     fivabwwr
078100990914     C                   endif
078200010530     C*
078300010530     C                   if        vlrpoi <> 999
078400010530     C                   eval      invfil = vlrpoi
078500010530     C                   endif
078600990910     C*
078700990920     C                   if        §ctrok > 0
078800010530     C                             and invfil > *zeros
078900000613     C                   exsr      invio
079000990920     C                   endif
079100010612     C*
079200010612     C                   if        flgGiro = '1'
079300010612     C                   exsr      endela
079400010612     C                   endif
079500990920     C*
079600910830     C                   ENDSR
079700000613     C***
079800990920
079900010530
080000010530
080100010530
080200010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
080300010530     C     repfil        BEGSR
080400010530     C*
080500010530     C                   if        invfil = *zeros and
080600010530     C                             depfil > *zeros and
080700010613     C                             (vinflg = *blanks or
080800010613     C                              vinflg = *zeros)
080900010530     C
081000010530     C                   eval      invfil = depfil
081100010530     C                   endif
081200010530     C*
081300010530     C                   if        depfil <> invfil and
081400010530     C                             invfil > *zeros
081500010530     C                   eval      flgMulti = '1'
081600010530     C                   if        vinflg = *blanks
081700010530     C                   add       1             cntNonEl
081800010530     C                   endif
081900010530     C                   endif
082000010530     C*
082100010530     C                   if        vinflg = '2'
082200010530     C                   eval      flgStato = '2'
082300010530     C                   endif
082400010530     C*
082500010530     C                   ENDSR
082600010530     C***
082700010530
082800010530
082900010530
083000010530
083100990920      /TITLE Invio dei dati al punto operativo.
083200000613     C     invio         BEGSR
083300990920     C*
083400990920     C                   reset                   dscmz
083500010530     C                   move      invfil        cmzdst
083600990920     C                   eval      cmzfld = vlrfou
083700990920     C                   eval      cmzmbd = vlrhdl
083800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
083900000710     C                   if        prmfir = *blanks
084000020913     C                   eval      cmzfla = 'FIVAB00F'
084100020913     C                   eval      cmzmba = 'FIVAB00F'
084200000710     C                   else
084300000710     C                   eval      cmzfla = prmfir
084400000710     C                   eval      cmzmba = prmfir
084500000710     C                   endif
084600990920     C                   eval      cmznrr = *zeros
084700990920     C                   move      §ctrok        cmznrr
084800021003     C                   eval      cmzlba = vlrfl1
084900990920     C                   call(e)   'TIS711C'
085000990920     C                   parm                    dscmz
085100990921     C                   parm      *blanks       esito
085200990920     C                   if        %error
085300990920     C                             or cmzerr = '1'
085400990921     C                             or esito  = '1'
085500000710     C                   eval      wrkesito = '3'
085600990920     C                   endif
085700990920     C*
085800000613     C                   ENDSR
085900000613     C***
086000010530
086100010530
086200010530
086300010530
086400010530      /TITLE Invio dei dati al punto operativo.
086500010530     C     opeini        BEGSR
086600010530     C*
086700010530     C* Inizializzo flag e contatori operativi
086800010530     C                   movel     '0'           flgGiro           1
086900010530     C                   movel     '0'           flgMulti          1
087000010530     C                   movel     '1'           flgStato          1
087100010615     C                   movel     '0'           flgOk             1
087200010530     C                   z-add     *zeros        cntNonEl         10 0
087300010530     C                   z-add     *zeros        depfil            3 0
087400010530     C                   z-add     *zeros        invfil            3 0
087500030407     C*
087600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
087700030407     C                   if        %subst(vlrppt:1:1) = 'S'
087800030407     C                   seton                                        50
087900030407     C                   else
088000030407     C                   setoff                                       50
088100030407     C                   endif
088200041001     C*
088300041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
088400041001     C                   if        %subst(vlrppt:2:1) = 'S'
088500041001     C                   seton                                        51
088600041001     C                   else
088700041001     C                   setoff                                       51
088800041001     C                   endif
088900041025     C*
089000041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
089100041025     C* (naturalmente solo se nn già presente)
089200041025     C                   if        %subst(vlrppt:3:1) = 'S'
089300041025     C                   seton                                        52
089400041025     C                   else
089500041025     C                   setoff                                       52
089600041025     C                   endif
089700010530     C*
089800010530     C                   ENDSR
089900010530     C***
090000040407
090100040407
090200040407
090300040407
090400040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
090500040407     C     exeBon        BEGSR
090600040407     C*
090700040407     C* Imposto i parametri x il *pgm d "bonifica"
090800040407     C                   EVAL      ElencoChar = *blanks
090900040407     C                   EVAL      TipoElenco = *blanks
091000040407     C                   EVAL      CharSost   = *blanks
091100040629     C                   EVAL      UpperCase  = '1'
091200040407     C                   EVAL      ChkNull    = '1'
091300040407     C                   EVAL      CharNull   = *blanks
091400040629     C*
091500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
091600040629     C                   EVAL      TxtInOut   = VABRSD
091700040629     C                   EVAL      EsitoBon   = *blanks
091800040407     C*
091900040407     C                   CALL      'XCHKCHAR'
092000040407     C                   PARM                    TxtInOut
092100040407     C                   PARM                    ElencoChar
092200040407     C                   PARM                    TipoElenco
092300040407     C                   PARM                    CharSost
092400040407     C                   PARM                    UpperCase
092500040407     C                   PARM                    ChkNull
092600040407     C                   PARM                    CharNull
092700040407     C                   PARM                    EsitoBon
092800040407     C*
092900040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
093000040407     C                   IF        EsitoBon = '1'
093100040629     C                   EVAL      VABRSD = TxtInOut
093200040407     C                   ENDIF
093300040629     C*
093400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
093500040629     C                   EVAL      TxtInOut   = VABIND
093600040629     C                   EVAL      EsitoBon   = *blanks
093700040629     C*
093800040629     C                   CALL      'XCHKCHAR'
093900040629     C                   PARM                    TxtInOut
094000040629     C                   PARM                    ElencoChar
094100040629     C                   PARM                    TipoElenco
094200040629     C                   PARM                    CharSost
094300040629     C                   PARM                    UpperCase
094400040629     C                   PARM                    ChkNull
094500040629     C                   PARM                    CharNull
094600040629     C                   PARM                    EsitoBon
094700040629     C*
094800040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
094900040629     C                   IF        EsitoBon = '1'
095000040629     C                   EVAL      VABIND = TxtInOut
095100040629     C                   ENDIF
095200040629     C*
095300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
095400040629     C                   EVAL      TxtInOut   = VABLOD
095500040629     C                   EVAL      EsitoBon   = *blanks
095600040629     C*
095700040629     C                   CALL      'XCHKCHAR'
095800040629     C                   PARM                    TxtInOut
095900040629     C                   PARM                    ElencoChar
096000040629     C                   PARM                    TipoElenco
096100040629     C                   PARM                    CharSost
096200040629     C                   PARM                    UpperCase
096300040629     C                   PARM                    ChkNull
096400040629     C                   PARM                    CharNull
096500040629     C                   PARM                    EsitoBon
096600040629     C*
096700040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
096800040629     C                   IF        EsitoBon = '1'
096900040629     C                   EVAL      VABLOD = TxtInOut
097000040629     C                   ENDIF
097100040629     C*
097200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
097300040629     C                   EVAL      TxtInOut   = VABNOT
097400040629     C                   EVAL      EsitoBon   = *blanks
097500040630     C                   EVAL      UpperCase  = *blanks
097600040629     C*
097700040629     C                   CALL      'XCHKCHAR'
097800040629     C                   PARM                    TxtInOut
097900040629     C                   PARM                    ElencoChar
098000040629     C                   PARM                    TipoElenco
098100040629     C                   PARM                    CharSost
098200040629     C                   PARM                    UpperCase
098300040629     C                   PARM                    ChkNull
098400040629     C                   PARM                    CharNull
098500040629     C                   PARM                    EsitoBon
098600040629     C*
098700040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
098800040629     C                   IF        EsitoBon = '1'
098900040629     C                   EVAL      VABNOT = TxtInOut
099000040629     C                   ENDIF
099100040629     C*
099200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
099300040629     C                   EVAL      TxtInOut   = VABNT2
099400040629     C                   EVAL      EsitoBon   = *blanks
099500040630     C                   EVAL      UpperCase  = *blanks
099600040629     C*
099700040629     C                   CALL      'XCHKCHAR'
099800040629     C                   PARM                    TxtInOut
099900040629     C                   PARM                    ElencoChar
100000040629     C                   PARM                    TipoElenco
100100040629     C                   PARM                    CharSost
100200040629     C                   PARM                    UpperCase
100300040629     C                   PARM                    ChkNull
100400040629     C                   PARM                    CharNull
100500040629     C                   PARM                    EsitoBon
100600040629     C*
100700040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100800040629     C                   IF        EsitoBon = '1'
100900040629     C                   EVAL      VABNT2 = TxtInOut
101000040629     C                   ENDIF
101100040407     C*
101200040407     C                   ENDSR
101300010530
101400060410
101500060410     C*----------------------------------------------------*
101600060410     C*  CONTROLLO NUMERICITA' CAMPI
101700060410     C*----------------------------------------------------*
101800060410     C     CHKNUM        BEGSR
101900060410     C*
102000060410     C                   IF        PiDecChr = *blanks
102100060410     C                   EVAL      PiDecChr = ','
102200060410     C                   ENDIF
102300060410     C*
102400060410     C                   CALL(e)   'ISNUMERIC'
102500060410     C                   PARM                    PiStr            30
102600060410     C                   PARM                    PiDecChr          1
102700060410     C                   PARM      *ZEROS        PiVal            30 9
102800060410     C                   PARM      '0'           PiInt             1
102900060410     C                   PARM      '0'           PiNum             1
103000060410     C                   IF        %error
103100060410     C                   EVAL      PiNum=*off
103200060410     C                   ENDIF
103300060410     C*
103400060410     C                   ENDSR
103500060410     C***
103600060410
103700990910
103800000613     C     *inzsr        BEGSR
103900990910     C*
104000990910     C     *entry        plist
104100990920     C                   parm                    tivlrds
104200990921     C                   parm      wrkesito      esito
104300000724     C                   parm                    prmlit
104400000710     C                   parm                    prmfir
104500010606     C*
104600010606     C* CALCOLA LA DATA CORRENTE
104700010606     C                   time                    wn14             14 0
104800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
104900010606     C                   z-add     wn8           g08dat
105000010606     C                   z-add     *zeros        g08inv
105100010606     C                   movel     '0'           g08err
105200010606     C                   call      'XSRDA8'
105300010606     C                   parm                    wlbda8
105400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
105500000613     C*
105600000613     C                   ENDSR
105700000613     C***
