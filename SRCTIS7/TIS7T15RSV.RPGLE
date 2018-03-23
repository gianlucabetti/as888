000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200130115     H dftactgrp(*no) actgrp(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500030616     FEDIVABWR  O    E             DISK    usropn
000600000512     D*------------
000700990920     D dscmz         e ds                  inz
000800990910     D psds           sds
000900990910     D  procname         *PROC
001000941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001100941229     D* 10+3
001200941229     D                 DS
001300941229     D  WS15                   1      1
001400941229     D  WI15                   2     11
001500941229     D  WV15                  12     12
001600941229     D  WD15                  13     15
001700941229     D  WASC15                 1     15
001800941229     D*  6+1
001900941229     D                 DS
002000941229     D  WS09                   1      1
002100941229     D  WI09                   2      7
002200941229     D  WV09                   8      8
002300941229     D  WD09                   9      9
002400941229     D  WASC09                 1      9
002500941229     D*  2+3
002600941229     D                 DS
002700941229     D  WS07                   1      1
002800941229     D  WI07                   2      3
002900941229     D  WV07                   4      4
003000941229     D  WD07                   5      7
003100941229     D  WASC07                 1      7
003200990920     D tivlrds       e ds                  extname(tivlr00f)
003300990910     D fnvabaar      e ds                  inz
003400990910     D esito           s              1
003500000724     D prmlit          s             10
003600000710     D prmfir          s             10
003700990921     D wrkesito        s                   like(esito)
003800000613     D rrnum           s              6  0 INZ(*zeros)
003900030616     D wNomeFile       s             30    INZ(*blanks)
004000010606     D*
004100990908
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500010530     C*
004600010530     C                   exsr      opeini
004700010530     C                   exsr      rwvab
004800010530     C*
004900010530     C                   seton                                        lr
005000010530
005100010530
005200010530
005300010530
005400010530     C*--------------------------------------------------------
005500010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005600010530     C*--------------------------------------------------------
005700010530     C     PREELA        BEGSR
005800000724     C*
005900000724     C* SE OCCORRE SPEDIRE IN FILIALE
006000010530     C                   if        invfil <> *zeros and
006100010530     C                             flgGiro = '0'
006200010530     C*
006300010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006400010530     C                   eval      flgGiro = '1'
006500010530     C*
006600010530     C                   endif
006700010530     C*
006800010530     C                   ENDSR
006900010530     C***
007000010530
007100010530
007200010530
007300010530
007400010530
007500010530
007600010530     C*--------------------------------------------------------
007700010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007800010530     C*--------------------------------------------------------
007900010530     C     ENDELA        BEGSR
008000010530     C*
008100010530     C                   ENDSR
008200010530     C***
008300990908
008400010530
008500010530
008600010530
008700910830     C*--------------------------------------------------------
008800030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
008900910830     C*--------------------------------------------------------
009000941229     C     RWVAB         BEGSR
009100990910     C*
009200990914     C                   if        not %open(tivin00r)
009300990908     C                   open      tivin00r
009400990914     C                   endif
009500030616     C                   if        not %open(edivabwr)
009600030616     C                   open      edivabwr
009700990914     C                   endif
009800990910     C*
009900990910     C                   clear                   §CTROK
010000990910     C                   clear                   §CTRMO
010100990910     C                   clear                   §CTRNO
010200990910     C*
010300921023     C                   DO        *HIVAL
010400990913     C*
010500990915     C                   READ      tivin00r                               70
010600010618     C*
010700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010800010618     C                   if        vinflg = '1'
010900010618     C                   eval      flgOk = '1'
011000010618     C                   endif
011100010705     C*
011200010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
011300010705     C                   if        %subst(vindta:1:1) = '='
011400010705     C                   eval      %subst(vindta:1:1) = *blanks
011500010705     C                   endif
011600010705     C*
011700000905     C                   if        vindta > *blanks
011800000613     C                   add       1             rrnum
011900010601     C* ...elimino eventuali segni
012000010601     C     '+':' '       xlate     vindta        vindta
012100990913     C*
012200010601     C                   if        *in70 = *off and
012300990913     C                             (vinflg = *blanks
012400010711     C                              or vinflg = '0'
012500010711     C                              or vinflg = '2')
012600990913     C*
012700030616     C                   clear                   EDIVAB00
012800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012900010711     C                   if        vinflg = *blanks or vinflg = '0'
013000010711     C                   clear                   vinmsg
013100010711     C                   endif
013200010711     C*
013300990913     C                   eval      fnvabaar = vindta
013400991022     C*
013500991022     C                   testb     x'3F'         vhbatb                   02
013600991022     C                   if        not *in02
013700991022     C*
013800971021     C                   SETOFF                                       303132
013900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
014000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
014100971023     C*    OPPURE TUTTI BLANKS
014200010530     C*
014300010530     C                   TESTN                   VHBLNP               30
014400010530     C  N30              TESTN                   VHBLNP                 30
014500010530     C  N30              TESTN                   VHBLNP                   30
014600010530     C                   MOVE      VHBLNP        W001A             1
014700010530     C     *IN30         IFEQ      *OFF
014800010530     C     W001A         ORLT      '0'
014900010530     C                   SETON                                        31
015000010530     C                   eval      vinmsg = %trimr(vinmsg)
015100010530     C                             + ' ' + 'VHBLNP'
015200010530     C                   ELSE
015300010530     C                   MOVE      VHBLNP        VABLNP
015400010530     C     VABLNP        IFLE      *ZEROS
015500010530     C                   SETON                                        31
015600010530     C                   eval      vinmsg = %trimr(vinmsg)
015700010530     C                             + ' ' + 'VHBLNP'
015800010530     C                   ENDIF
015900010530     C                   ENDIF
016000010530     C*
016100010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
016200010530     C                   eval      depfil = VABLNP
016300010530     C                   exsr      repfil
016400010601     C                   if        depfil = invfil
016500021014     C                   if        vlrpoi = 999
016600021010     C                   MOVE(P)   invfil        VABFGS
016700021014     C                   else
016800021014     C                   MOVE(P)   vlrpoi        VABFGS
016900021014     C                   endif
017000010530     C*
017100010530     C                   exsr      PREELA
017200010530     C*
017300971023     C                   TESTN                   VHBCCM               30
017400971023     C  N30              TESTN                   VHBCCM                 30
017500971023     C  N30              TESTN                   VHBCCM                   30
017600971021     C                   MOVE      VHBCCM        W001A             1
017700971021     C     *IN30         IFEQ      *OFF
017800971021     C     W001A         ORLT      '0'
017900971021     C                   SETON                                        31
018000010604     C                   eval      vinmsg = %trimr(vinmsg)
018100010604     C                             + ' ' + 'VHBCCM'
018200971021     C                   ELSE
018300971021     C                   MOVE      VHBCCM        VABCCM
018400971024     C     VABCCM        IFLE      *ZEROS
018500971024     C                   SETON                                        31
018600010604     C                   eval      vinmsg = %trimr(vinmsg)
018700010604     C                             + ' ' + 'VHBCCM'
018800971024     C                   ENDIF
018900971021     C                   ENDIF
019000971021     C*
019100971023     C                   TESTN                   VHBAAS               30
019200971023     C  N30              TESTN                   VHBAAS                 30
019300971023     C  N30              TESTN                   VHBAAS                   30
019400971021     C                   MOVE      VHBAAS        W001A             1
019500971021     C     *IN30         IFEQ      *OFF
019600971021     C     W001A         ORLT      '0'
019700971023     C     W001A         ANDNE     *BLANK
019800971021     C                   SETON                                        32
019900010606     C                   MOVEL     datcor        VABAAS
020000010604     C                   eval      vinmsg = %trimr(vinmsg)
020100010604     C                             + ' ' + 'VHBAAS'
020200971021     C                   ELSE
020300971021     C                   MOVE      VHBAAS        VABAAS
020400971021     C                   ENDIF
020500971021     C*
020600971023     C                   TESTN                   VHBMGS               30
020700971023     C  N30              TESTN                   VHBMGS                 30
020800971023     C  N30              TESTN                   VHBMGS                   30
020900971021     C                   MOVE      VHBMGS        W001A             1
021000971021     C     *IN30         IFEQ      *OFF
021100971021     C     W001A         ORLT      '0'
021200971023     C     W001A         ANDNE     *BLANK
021300971021     C                   SETON                                        32
021400010601     C                   MOVE      '0101'        VABMGS
021500010604     C                   eval      vinmsg = %trimr(vinmsg)
021600010604     C                             + ' ' + 'VHBMGS'
021700971021     C                   ELSE
021800971021     C                   MOVE      VHBMGS        VABMGS
021900971021     C                   ENDIF
022000971021     C*
022100971023     C                   TESTN                   VHBNRS               30
022200971023     C  N30              TESTN                   VHBNRS                 30
022300971023     C  N30              TESTN                   VHBNRS                   30
022400971021     C                   MOVE      VHBNRS        W001A             1
022500971021     C     *IN30         IFEQ      *OFF
022600971021     C     W001A         ORLT      '0'
022700971023     C     W001A         ANDNE     *BLANK
022800971021     C                   SETON                                        32
022900971021     C                   MOVE      *ZERO         VABNRS
023000010604     C                   eval      vinmsg = %trimr(vinmsg)
023100010604     C                             + ' ' + 'VHBNRS'
023200971021     C                   ELSE
023300971021     C                   MOVE      VHBNRS        VABNRS
023400971021     C                   ENDIF
023500971021     C*
023600971023     C                   TESTN                   VHBNSP               30
023700971023     C  N30              TESTN                   VHBNSP                 30
023800971023     C  N30              TESTN                   VHBNSP                   30
023900971021     C                   MOVE      VHBNSP        W001A             1
024000971021     C     *IN30         IFEQ      *OFF
024100971021     C     W001A         ORLT      '0'
024200971023     C     W001A         ANDNE     *BLANK
024300971021     C                   SETON                                        32
024400971021     C                   MOVE      *ZERO         VABNSP
024500010604     C                   eval      vinmsg = %trimr(vinmsg)
024600010604     C                             + ' ' + 'VHBNSP'
024700971021     C                   ELSE
024800971021     C                   MOVE      VHBNSP        VABNSP
024900971021     C                   ENDIF
025000971021     C*
025100971023     C                   TESTN                   VHBLNA               30
025200971023     C  N30              TESTN                   VHBLNA                 30
025300971023     C  N30              TESTN                   VHBLNA                   30
025400971021     C                   MOVE      VHBLNA        W001A             1
025500971021     C     *IN30         IFEQ      *OFF
025600971021     C     W001A         ORLT      '0'
025700971023     C     W001A         ANDNE     *BLANK
025800971021     C                   SETON                                        32
025900971021     C                   MOVE      *ZERO         VABLNA
026000010604     C                   eval      vinmsg = %trimr(vinmsg)
026100010604     C                             + ' ' + 'VHBLNA'
026200971021     C                   ELSE
026300971021     C                   MOVE      VHBLNA        VABLNA
026400971021     C                   ENDIF
026500971021     C*
026600971023     C                   TESTN                   VHBCTR               30
026700971023     C  N30              TESTN                   VHBCTR                 30
026800971023     C  N30              TESTN                   VHBCTR                   30
026900971021     C                   MOVE      VHBCTR        W001A             1
027000971021     C     *IN30         IFEQ      *OFF
027100971021     C     W001A         ORLT      '0'
027200971023     C     W001A         ANDNE     *BLANK
027300971021     C                   SETON                                        32
027400971021     C                   MOVE      *ZERO         VABCTR
027500010604     C                   eval      vinmsg = %trimr(vinmsg)
027600010604     C                             + ' ' + 'VHBCTR'
027700971021     C                   ELSE
027800971021     C                   MOVE      VHBCTR        VABCTR
027900971021     C                   ENDIF
028000941229     C*
028100971021     C                   MOVE      *ZERO         VABIAS
028200941229     C                   MOVE      VHBIAS        WASC15
028300971023     C                   TESTN                   WI15                 30
028400971023     C  N30              TESTN                   WI15                   30
028500971023     C  N30              TESTN                   WI15                     30
028600971021     C                   MOVE      WI15          W001A             1
028700971021     C     *IN30         IFEQ      *OFF
028800971021     C     W001A         ORLT      '0'
028900971023     C     W001A         ANDNE     *BLANK
029000971021     C                   SETON                                        32
029100971021     C                   MOVE      *ZERO         VABIAS
029200010604     C                   eval      vinmsg = %trimr(vinmsg)
029300010604     C                             + ' ' + 'VHBIAS'
029400971021     C                   ELSE
029500971021     C                   MOVEL     WI15          VABIAS
029600971021     C                   ENDIF
029700971023     C                   TESTN                   WD15                 30
029800971023     C  N30              TESTN                   WD15                   30
029900971023     C  N30              TESTN                   WD15                     30
030000971021     C                   MOVE      WD15          W001A             1
030100971021     C     *IN30         IFEQ      *OFF
030200971021     C     W001A         ORLT      '0'
030300971023     C     W001A         ANDNE     *BLANK
030400971021     C                   SETON                                        32
030500971021     C                   MOVE      *ZERO         VABIAS
030600010604     C                   eval      vinmsg = %trimr(vinmsg)
030700010604     C                             + ' ' + 'VHBIAS'
030800971021     C                   ELSE
030900971021     C                   MOVE      WD15          VABIAS
031000971021     C                   ENDIF
031100941229     C*
031200971023     C                   TESTN                   VHBNCL               30
031300971023     C  N30              TESTN                   VHBNCL                 30
031400971023     C  N30              TESTN                   VHBNCL                   30
031500971021     C                   MOVE      VHBNCL        W001A             1
031600971021     C     *IN30         IFEQ      *OFF
031700971021     C     W001A         ORLT      '0'
031800971021     C                   SETON                                        32
031900971021     C                   MOVE      *ZERO         VABNCL
032000010604     C                   eval      vinmsg = %trimr(vinmsg)
032100010604     C                             + ' ' + 'VHBNCL'
032200971021     C                   ELSE
032300971021     C                   MOVE      VHBNCL        VABNCL
032400971021     C                   ENDIF
032500941229     C*
032600971021     C                   MOVE      *ZERO         VABPKB
032700971021     C                   MOVE      VHBPKB        WASC09
032800010719     C                   IF        WI09 = *blanks
032900010719     C                   EVAL      WI09 = *zeros
033000010719     C                   ENDIF
033100971023     C                   TESTN                   WI09                 30
033200971023     C  N30              TESTN                   WI09                   30
033300971023     C  N30              TESTN                   WI09                     30
033400971021     C                   MOVE      WI09          W001A             1
033500971021     C     *IN30         IFEQ      *OFF
033600971021     C     W001A         ORLT      '0'
033700971021     C                   SETON                                        32
033800971021     C                   MOVE      *ZERO         VABPKB
033900010604     C                   eval      vinmsg = %trimr(vinmsg)
034000010604     C                             + ' ' + 'VHBPKB'
034100971021     C                   ELSE
034200971021     C                   MOVEL     WI09          VABPKB
034300971021     C                   ENDIF
034400010719     C                   IF        WD09 = *blanks
034500010719     C                   EVAL      WD09 = *zeros
034600010719     C                   ENDIF
034700971023     C                   TESTN                   WD09                 30
034800971023     C  N30              TESTN                   WD09                     30
034900971021     C                   MOVE      WD09          W001A             1
035000971021     C     *IN30         IFEQ      *OFF
035100971021     C     W001A         ORLT      '0'
035200971023     C     W001A         ANDNE     *BLANK
035300971021     C                   SETON                                        32
035400971021     C                   MOVE      *ZERO         VABPKB
035500010604     C                   eval      vinmsg = %trimr(vinmsg)
035600010604     C                             + ' ' + 'VHBPKB'
035700971021     C                   ELSE
035800971021     C                   MOVE      WD09          VABPKB
035900971021     C                   ENDIF
036000971021     C*
036100971021     C                   MOVE      *ZERO         VABVLB
036200971021     C                   MOVE      VHBVLB        WASC07
036300010719     C                   IF        WI07 = *blanks
036400010719     C                   EVAL      WI07 = *zeros
036500010719     C                   ENDIF
036600971023     C                   TESTN                   WI07                 30
036700971023     C  N30              TESTN                   WI07                   30
036800971023     C  N30              TESTN                   WI07                     30
036900971021     C                   MOVE      WI07          W001A             1
037000971021     C     *IN30         IFEQ      *OFF
037100971021     C     W001A         ORLT      '0'
037200971023     C     W001A         ANDNE     *BLANK
037300971021     C                   SETON                                        32
037400971021     C                   MOVE      *ZERO         VABVLB
037500010604     C                   eval      vinmsg = %trimr(vinmsg)
037600010604     C                             + ' ' + 'VHBVLB'
037700971021     C                   ELSE
037800971021     C                   MOVEL     WI07          VABVLB
037900971021     C                   ENDIF
038000010719     C                   IF        WD07 = *blanks
038100010719     C                   EVAL      WD07 = *zeros
038200010719     C                   ENDIF
038300971023     C                   TESTN                   WD07                 30
038400971023     C  N30              TESTN                   WD07                   30
038500971023     C  N30              TESTN                   WD07                     30
038600971021     C                   MOVE      WD07          W001A             1
038700971021     C     *IN30         IFEQ      *OFF
038800971021     C     W001A         ORLT      '0'
038900971023     C     W001A         ANDNE     *BLANK
039000971021     C                   SETON                                        32
039100971021     C                   MOVE      *ZERO         VABVLB
039200010604     C                   eval      vinmsg = %trimr(vinmsg)
039300010604     C                             + ' ' + 'VHBVLB'
039400971021     C                   ELSE
039500971021     C                   MOVE      WD07          VABVLB
039600971021     C                   ENDIF
039700941229     C*
039800971021     C                   MOVE      *ZERO         VABQFT
039900971021     C                   MOVE      VHBQFT        WASC15
040000010719     C                   IF        WI15 = *blanks
040100010719     C                   EVAL      WI15 = *zeros
040200010719     C                   ENDIF
040300971023     C                   TESTN                   WI15                 30
040400971023     C  N30              TESTN                   WI15                   30
040500971023     C  N30              TESTN                   WI15                     30
040600971021     C                   MOVE      WI15          W001A             1
040700971021     C     *IN30         IFEQ      *OFF
040800971021     C     W001A         ORLT      '0'
040900971023     C     W001A         ANDNE     *BLANK
041000971021     C                   SETON                                        32
041100971021     C                   MOVE      *ZERO         VABQFT
041200010604     C                   eval      vinmsg = %trimr(vinmsg)
041300010604     C                             + ' ' + 'VHBQFT'
041400971021     C                   ELSE
041500971021     C                   MOVEL     WI15          VABQFT
041600971021     C                   ENDIF
041700010719     C                   IF        WD15 = *blanks
041800010719     C                   EVAL      WD15 = *zeros
041900010719     C                   ENDIF
042000971023     C                   TESTN                   WD15                 30
042100971023     C  N30              TESTN                   WD15                   30
042200971023     C  N30              TESTN                   WD15                     30
042300971021     C                   MOVE      WD15          W001A             1
042400971021     C     *IN30         IFEQ      *OFF
042500971021     C     W001A         ORLT      '0'
042600971023     C     W001A         ANDNE     *BLANK
042700971021     C                   SETON                                        32
042800971021     C                   MOVE      *ZERO         VABQFT
042900010604     C                   eval      vinmsg = %trimr(vinmsg)
043000010604     C                             + ' ' + 'VHBQFT'
043100971021     C                   ELSE
043200971021     C                   MOVE      WD15          VABQFT
043300971021     C                   ENDIF
043400941229     C*
043500971021     C                   MOVE      *ZERO         VABCAS
043600971021     C                   MOVE      VHBCAS        WASC15
043700010719     C                   IF        WI15 = *blanks
043800010719     C                   EVAL      WI15 = *zeros
043900010719     C                   ENDIF
044000971023     C                   TESTN                   WI15                 30
044100971023     C  N30              TESTN                   WI15                   30
044200971023     C  N30              TESTN                   WI15                     30
044300971021     C                   MOVE      WI15          W001A             1
044400971021     C     *IN30         IFEQ      *OFF
044500971021     C     W001A         ORLT      '0'
044600971023     C     W001A         ANDNE     *BLANK
044700971021     C                   SETON                                        32
044800971021     C                   MOVE      *ZERO         VABCAS
044900010604     C                   eval      vinmsg = %trimr(vinmsg)
045000010604     C                             + ' ' + 'VHBCAS'
045100971021     C                   ELSE
045200971021     C                   MOVEL     WI15          VABCAS
045300971021     C                   ENDIF
045400010719     C                   IF        WD15 = *blanks
045500010719     C                   EVAL      WD15 = *zeros
045600010719     C                   ENDIF
045700971023     C                   TESTN                   WD15                 30
045800971023     C  N30              TESTN                   WD15                   30
045900971023     C  N30              TESTN                   WD15                     30
046000971021     C                   MOVE      WD15          W001A             1
046100971021     C     *IN30         IFEQ      *OFF
046200971021     C     W001A         ORLT      '0'
046300971023     C     W001A         ANDNE     *BLANK
046400971021     C                   SETON                                        32
046500971021     C                   MOVE      *ZERO         VABCAS
046600010604     C                   eval      vinmsg = %trimr(vinmsg)
046700010604     C                             + ' ' + 'VHBCAS'
046800971021     C                   ELSE
046900971021     C                   MOVE      WD15          VABCAS
047000971021     C                   ENDIF
047100971021     C*
047200971023     C                   TESTN                   VHBRMN               30
047300971023     C  N30              TESTN                   VHBRMN                 30
047400971023     C  N30              TESTN                   VHBRMN                   30
047500971021     C                   MOVE      VHBRMN        W001A             1
047600971021     C     *IN30         IFEQ      *OFF
047700971021     C     W001A         ORLT      '0'
047800971021     C                   SETON                                        32
047900971021     C                   MOVE      *ZERO         VABRMN
048000010604     C                   eval      vinmsg = %trimr(vinmsg)
048100010604     C                             + ' ' + 'VHBRMN'
048200971021     C                   ELSE
048300971021     C                   MOVE      VHBRMN        VABRMN
048400971021     C                   ENDIF
048500971021     C*
048600971023     C                   TESTN                   VHBNCD               30
048700971023     C  N30              TESTN                   VHBNCD                 30
048800971023     C  N30              TESTN                   VHBNCD                   30
048900971021     C                   MOVE      VHBNCD        W001A             1
049000971021     C     *IN30         IFEQ      *OFF
049100971021     C     W001A         ORLT      '0'
049200971023     C     W001A         ANDNE     *BLANK
049300971021     C                   SETON                                        32
049400971021     C                   MOVE      *ZERO         VABNCD
049500010604     C                   eval      vinmsg = %trimr(vinmsg)
049600010604     C                             + ' ' + 'VHBNCD'
049700971021     C                   ELSE
049800971021     C                   MOVE      VHBNCD        VABNCD
049900971021     C                   ENDIF
050000971021     C*
050100971023     C                   TESTN                   VHBNCA               30
050200971023     C  N30              TESTN                   VHBNCA                 30
050300971023     C  N30              TESTN                   VHBNCA                   30
050400971021     C                   MOVE      VHBNCA        W001A             1
050500971021     C     *IN30         IFEQ      *OFF
050600971021     C     W001A         ORLT      '0'
050700971023     C     W001A         ANDNE     *BLANK
050800971021     C                   SETON                                        32
050900971021     C                   MOVE      *ZERO         VABNCA
051000010604     C                   eval      vinmsg = %trimr(vinmsg)
051100010604     C                             + ' ' + 'VHBNCA'
051200971021     C                   ELSE
051300971021     C                   MOVE      VHBNCA        VABNCA
051400971021     C                   ENDIF
051500971021     C*
051600971023     C                   TESTN                   VHBZNC               30
051700971023     C  N30              TESTN                   VHBZNC                 30
051800971023     C  N30              TESTN                   VHBZNC                   30
051900971021     C                   MOVE      VHBZNC        W001A             1
052000971021     C     *IN30         IFEQ      *OFF
052100971021     C     W001A         ORLT      '0'
052200971023     C     W001A         ANDNE     *BLANK
052300971021     C                   SETON                                        32
052400971021     C                   MOVE      *ZERO         VABZNC
052500010604     C                   eval      vinmsg = %trimr(vinmsg)
052600010604     C                             + ' ' + 'VHBZNC'
052700971021     C                   ELSE
052800971021     C                   MOVE      VHBZNC        VABZNC
052900971021     C                   ENDIF
053000971021     C*
053100971023     C                   TESTN                   VHBDCR               30
053200971023     C  N30              TESTN                   VHBDCR                 30
053300971023     C  N30              TESTN                   VHBDCR                   30
053400971021     C                   MOVE      VHBDCR        W001A             1
053500971021     C     *IN30         IFEQ      *OFF
053600971021     C     W001A         ORLT      '0'
053700971023     C     W001A         ANDNE     *BLANK
053800971021     C                   SETON                                        32
053900971021     C                   MOVE      *ZERO         VABDCR
054000010604     C                   eval      vinmsg = %trimr(vinmsg)
054100010604     C                             + ' ' + 'VHBDCR'
054200971021     C                   ELSE
054300971021     C                   MOVE      VHBDCR        VABDCR
054400971021     C                   ENDIF
054500971021     C*
054600971023     C                   TESTN                   VHBHCR               30
054700971023     C  N30              TESTN                   VHBHCR                 30
054800971023     C  N30              TESTN                   VHBHCR                   30
054900971021     C                   MOVE      VHBHCR        W001A             1
055000971021     C     *IN30         IFEQ      *OFF
055100971021     C     W001A         ORLT      '0'
055200971023     C     W001A         ANDNE     *BLANK
055300971021     C                   SETON                                        32
055400971021     C                   MOVE      *ZERO         VABHCR
055500010604     C                   eval      vinmsg = %trimr(vinmsg)
055600010604     C                             + ' ' + 'VHBHCR'
055700971021     C                   ELSE
055800971021     C                   MOVE      VHBHCR        VABHCR
055900971021     C                   ENDIF
056000941229     C*
056100971021     C                   MOVE      *ZERO         VABVMD
056200971021     C                   MOVE      VHBVMD        WASC15
056300010719     C                   IF        WI15 = *blanks
056400010719     C                   EVAL      WI15 = *zeros
056500010719     C                   ENDIF
056600971023     C                   TESTN                   WI15                 30
056700971023     C  N30              TESTN                   WI15                   30
056800971023     C  N30              TESTN                   WI15                     30
056900971021     C                   MOVE      WI15          W001A             1
057000971021     C     *IN30         IFEQ      *OFF
057100971021     C     W001A         ORLT      '0'
057200971023     C     W001A         ANDNE     *BLANK
057300971021     C                   SETON                                        32
057400971021     C                   MOVE      *ZERO         VABVMD
057500010604     C                   eval      vinmsg = %trimr(vinmsg)
057600010604     C                             + ' ' + 'VHBVMD'
057700971021     C                   ELSE
057800971021     C                   MOVEL     WI15          VABVMD
057900971021     C                   ENDIF
058000010719     C                   IF        WD15 = *blanks
058100010719     C                   EVAL      WD15 = *zeros
058200010719     C                   ENDIF
058300971023     C                   TESTN                   WD15                 30
058400971023     C  N30              TESTN                   WD15                   30
058500971023     C  N30              TESTN                   WD15                     30
058600971021     C                   MOVE      WD15          W001A             1
058700971021     C     *IN30         IFEQ      *OFF
058800971021     C     W001A         ORLT      '0'
058900971023     C     W001A         ANDNE     *BLANK
059000971021     C                   SETON                                        32
059100971021     C                   MOVE      *ZERO         VABVMD
059200010604     C                   eval      vinmsg = %trimr(vinmsg)
059300010604     C                             + ' ' + 'VHBVMD'
059400971021     C                   ELSE
059500971021     C                   MOVE      WD15          VABVMD
059600971021     C                   ENDIF
059700971021     C*
059800971023     C                   TESTN                   VHBANT               30
059900971023     C  N30              TESTN                   VHBANT                 30
060000971023     C  N30              TESTN                   VHBANT                   30
060100971021     C                   MOVE      VHBANT        W001A             1
060200971021     C     *IN30         IFEQ      *OFF
060300971021     C     W001A         ORLT      '0'
060400971023     C     W001A         ANDNE     *BLANK
060500971021     C                   SETON                                        32
060600971021     C                   MOVE      *ZERO         VABANT
060700010604     C                   eval      vinmsg = %trimr(vinmsg)
060800010604     C                             + ' ' + 'VHBANT'
060900971021     C                   ELSE
061000971021     C                   MOVE      VHBANT        VABANT
061100971021     C                   ENDIF
061200971021     C*
061300971021     C* CAMPI ALFABETICI:
061400971021     C                   MOVE      VHBATB        VABATB
061500971021     C                   MOVE      VHBCBO        VABCBO
061600971021     C                   MOVE      VHBRSD        VABRSD
061700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
061800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
061900020117     C* ==
062000971021     C                   MOVE      VHBRD2        VABRD2
062100971021     C                   MOVE      VHBIND        VABIND
062200971021     C                   MOVE      VHBCAD        VABCAD
062300971021     C                   MOVE      VHBLOD        VABLOD
062400971021     C                   MOVE      VHBPRD        VABPRD
062500971021     C                   MOVE      VHBNZD        VABNZD
062600971021     C                   MOVE      VHBGC1        VABGC1
062700971021     C                   MOVE      VHBGC2        VABGC2
062800971021     C                   MOVE      VHBTSP        VABTSP
062900971021     C                   MOVE      VHBVAS        VABVAS
063000971021     C                   MOVE      VHBNAS        VABNAS
063100971021     C                   MOVE      VHBTIC        VABTIC
063200971021     C                   MOVE      VHBVCA        VABVCA
063300971021     C                   MOVE      VHBGCA        VABGCA
063400971021     C                   MOVE      VHBRMA        VABRMA
063500971021     C                   MOVE      VHBXCO        VABXCO
063600971021     C                   MOVE      VHBNOT        VABNOT
063700971021     C                   MOVE      VHBNT2        VABNT2
063800971021     C                   MOVE      VHBCTM        VABCTM
063900971021     C                   MOVE      VHBFFD        VABFFD
064000971021     C                   MOVE      VHBTCR        VABTCR
064100971021     C                   MOVE      VHBCTS        VABCTS
064200971021     C                   MOVE      VHBFTM        VABFTM
064300971021     C                   MOVE      VHBVAD        VABVAD
064400971021     C                   MOVE      VHBGMA        VABGMA
064500971021     C                   MOVE      VHBGGA        VABGGA
064600971021     C                   MOVE      VHBGVA        VABGVA
064700971021     C                   MOVE      VHBTC1        VABTC1
064800971021     C                   MOVE      VHBTC2        VABTC2
064900971021     C                   MOVE      VHBSCL        VABSCL
065000971021     C                   MOVE      VHBRMO        VABRMO
065100971021     C                   MOVE      VHBCMO        VABCMO
065200971021     C                   MOVE      VHBNMO        VABNMO
065300971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
065400971021     C* NELLA NAZIONE ORIGINALE
065500971021     C                   MOVE      VHBNMO        WA1D              1
065600971021     C                   MOVEL     VHBNMO        WA1S              1
065700971021     C     WA1D          IFNE      *BLANK
065800971021     C     WA1S          ANDEQ     *BLANK
065900971021     C                   MOVE      *BLANK        VABNMO
066000030616     C                   ENDIF
066100030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
066200030616     C                   EVAL      VABCMR = %subst(wNomeFile:4:5)
066300030616     C                   EVAL      VABDCM = DATCOR
066400030616     C                   EVAL      VABDTS = DATCOR
066500030616     C                   EVAL      VABCNT = 1
066600930409     C*
066700010604     C  N31              ADD       1             §CTROK            7 0
066800010604     C   32              ADD       1             §CTRMO            7 0
066900010604     C   31              ADD       1             §CTRNO            7 0
067000030616     C  N31              WRITE     EDIVAB00
067100990910     C*
067200010601     C                   if        *in31 = *off and
067300010601     C                             *in32 = *off
067400990910     C                   eval      vinflg = '1'
067500990910     C                   else
067600990910     C                   eval      vinflg = '2'
067700990910     C                   endif
067800991022     C                   endif
067900990910     C                   endif
068000010601     C*
068100010601     C                   endif
068200000905     C*
068300000905     C                   else
068400000905     C                   eval      vinflg = '1'
068500000905     C                   endif
068600000905     C*
068700000905     C  N70              update    tivin000
068800991022     C*
068900991022     C  N70              ENDdo
069000010530     C
069100010530     C*
069200010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
069300010530     C                   if        cntNonEl = *zeros or
069400010530     C                             flgMulti = '0'
069500990910     C* Se non ci sono record con errori ...
069600010530     C                   if        §ctrno = 0 and
069700010601     C                             §ctrmo = 0 and
069800010530     C                             flgStato <> '2'
069900990910     C* ... restituisco esito OK.
070000990921     C                   eval      wrkesito = '0'
070100990910     C                   else
070200010601     C                   if        §ctrok > 0
070300990921     C                   eval      wrkesito = '1'
070400000710     C                   else
070500010615     C                   if        flgOk = '0'
070600010615     C                   eval      wrkesito = '2'
070700010615     C                   else
070800010615     C                   eval      wrkesito = '6'
070900010615     C                   endif
071000990910     C                   endif
071100000710     C                   endif
071200010530     C                   else
071300010530     C                   eval      wrkesito = '9'
071400010530     C                   endif
071500990910     C*
071600990914     C                   if        %open(tivin00r)
071700990908     C                   close     tivin00r
071800990914     C                   endif
071900030616     C                   if        %open(edivabwr)
072000030616     C                   close     edivabwr
072100990914     C                   endif
072200010530     C*
072300010530     C                   if        vlrpoi <> 999
072400010530     C                   eval      invfil = vlrpoi
072500010530     C                   endif
072600990910     C*
072700990920     C                   if        §ctrok > 0
072800010530     C                             and invfil > *zeros
072900000613     C                   exsr      invio
073000990920     C                   endif
073100010612     C*
073200010612     C                   if        flgGiro = '1'
073300010612     C                   exsr      endela
073400010612     C                   endif
073500990920     C*
073600910830     C                   ENDSR
073700000613     C***
073800990920
073900010530
074000010530
074100010530
074200010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
074300010530     C     repfil        BEGSR
074400010530     C*
074500010530     C                   if        invfil = *zeros and
074600010530     C                             depfil > *zeros and
074700010613     C                             (vinflg = *blanks or
074800010613     C                              vinflg = *zeros)
074900010530     C
075000010530     C                   eval      invfil = depfil
075100010530     C                   endif
075200010530     C*
075300010530     C                   if        depfil <> invfil and
075400010530     C                             invfil > *zeros
075500010530     C                   eval      flgMulti = '1'
075600010530     C                   if        vinflg = *blanks
075700010530     C                   add       1             cntNonEl
075800010530     C                   endif
075900010530     C                   endif
076000010530     C*
076100010530     C                   if        vinflg = '2'
076200010530     C                   eval      flgStato = '2'
076300010530     C                   endif
076400010530     C*
076500010530     C                   ENDSR
076600010530     C***
076700010530
076800010530
076900010530
077000010530
077100990920      /TITLE Invio dei dati al punto operativo.
077200000613     C     invio         BEGSR
077300990920     C*
077400990920     C                   reset                   dscmz
077500030618     C                   move      invfil        cmzdst
077600030618     C***                move      '789'         cmzdst
077700990920     C                   eval      cmzfld = vlrfou
077800990920     C                   eval      cmzmbd = vlrhdl
077900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
078000000710     C                   if        prmfir = *blanks
078100030616     C                   eval      cmzfla = 'EDIVAB0F'
078200030616     C                   eval      cmzmba = 'EDIVAB0F'
078300000710     C                   else
078400000710     C                   eval      cmzfla = prmfir
078500000710     C                   eval      cmzmba = prmfir
078600000710     C                   endif
078700990920     C                   eval      cmznrr = *zeros
078800990920     C                   move      §ctrok        cmznrr
078900021003     C                   eval      cmzlba = vlrfl1
079000990920     C                   call(e)   'TIS711C'
079100990920     C                   parm                    dscmz
079200990921     C                   parm      *blanks       esito
079300990920     C                   if        %error
079400990920     C                             or cmzerr = '1'
079500990921     C                             or esito  = '1'
079600000710     C                   eval      wrkesito = '3'
079700990920     C                   endif
079800990920     C*
079900000613     C                   ENDSR
080000000613     C***
080100010530
080200010530
080300010530
080400010530
080500010530      /TITLE Invio dei dati al punto operativo.
080600010530     C     opeini        BEGSR
080700010530     C*
080800010530     C* Inizializzo flag e contatori operativi
080900010530     C                   movel     '0'           flgGiro           1
081000010530     C                   movel     '0'           flgMulti          1
081100010530     C                   movel     '1'           flgStato          1
081200010615     C                   movel     '0'           flgOk             1
081300010530     C                   z-add     *zeros        cntNonEl         10 0
081400010530     C                   z-add     *zeros        depfil            3 0
081500010530     C                   z-add     *zeros        invfil            3 0
081600030407     C*
081700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
081800030407     C                   if        %subst(vlrppt:1:1) = 'S'
081900030407     C                   seton                                        50
082000030407     C                   else
082100030407     C                   setoff                                       50
082200030407     C                   endif
082300010530     C*
082400010530     C                   ENDSR
082500010530     C***
082600010530
082700010530
082800010530
082900990910
083000000613     C     *inzsr        BEGSR
083100990910     C*
083200990910     C     *entry        plist
083300990920     C                   parm                    tivlrds
083400990921     C                   parm      wrkesito      esito
083500000724     C                   parm                    prmlit
083600000710     C                   parm                    prmfir
083700030616     C*
083800030616     C* SE RICHIESTO NEI PARAMETRI DI TRADUZIONE
083900030616     C* REPERISCO SUBITO IL NOME DEL FILE "UPLOADATO" (SE C'E') E REPERISCO LA FILIALE A CUI INVIARE
084000030618     C                   MOVE(P)   vlrMSG        wNomeFile
084100030616     C                   IF        %subst(vlrPPT:2:1) = 'S'
084200100319     C                   SELECT
084300100319     C                   WHEN      %subst(wNomeFile:7:2) = '60' OR
084400091207     C                             %subst(wNomeFile:7:2) = '61' OR
084500091210     C                             %subst(wNomeFile:7:2) = '62' OR
084600100319     C***                          %subst(wNomeFile:7:2) = '63' OR
084700081016     C                             %subst(wNomeFile:7:2) = '64' OR
084800100319     C***                          %subst(wNomeFile:7:2) = '65' OR
084900091203     C                             %subst(wNomeFile:7:2) = '66' OR
085000100319     C***                          %subst(wNomeFile:7:2) = '67' OR
085100081016     C                             %subst(wNomeFile:7:2) = '68' OR
085200100319     C                             %subst(wNomeFile:7:2) = '69'
085300100319     C***                          %subst(wNomeFile:7:2) = '70'
085400081016     C                   EVAL      vlrpoi = 999
085500100319     C                   WHEN      %subst(wNomeFile:7:2) = '99'
085600030623     C                   EVAL      vlrpoi = 048
085700100319     C                   WHEN      %subst(wNomeFile:7:2) = '13' OR
085800070207     C                             %subst(wNomeFile:7:2) = '25' OR
085900100301     C                             %subst(wNomeFile:7:2) = '55' OR
086000100301     C***                          %subst(wNomeFile:7:2) = '10' OR
086100100301     C***                          %subst(wNomeFile:7:2) = '20' OR
086200100301     C***                          %subst(wNomeFile:7:2) = '30' OR
086300100301     C***                          %subst(wNomeFile:7:2) = '40' OR
086400100301     C***                          %subst(wNomeFile:7:2) = '45' OR
086500100301     C***                          %subst(wNomeFile:7:2) = '80'
086600100301     C                             %subst(wNomeFile:7:2) = '01' OR
086700100301     C                             %subst(wNomeFile:7:2) = '02' OR
086800100301     C                             %subst(wNomeFile:7:2) = '24' OR              * sped. "garantite"
086900100319     C                             %subst(wNomeFile:7:2) = '03' OR
087000100319     C                             %subst(wNomeFile:7:2) = '63' OR
087100100319     C                             %subst(wNomeFile:7:2) = '65' OR
087200100319     C                             %subst(wNomeFile:7:2) = '67' OR
087300100319     C                             %subst(wNomeFile:7:2) = '70'
087400100319     C                   EVAL      vlrpoi = 277
087500100319     C***                WHEN      %subst(wNomeFile:7:2) = '01' OR
087600091109     C***                          %subst(wNomeFile:7:2) = '02' OR
087700091109     C***                          %subst(wNomeFile:7:2) = '24' OR              * sped. "garantite"
087800100319     C***                WHEN      %subst(wNomeFile:7:2) = '03' OR
087900100319     C                   WHEN      %subst(wNomeFile:7:2) = '04' OR
088000030623     C                             %subst(wNomeFile:7:2) = '05' OR
088100030623     C                             %subst(wNomeFile:7:2) = '06' OR
088200030623     C                             %subst(wNomeFile:7:2) = '07' OR
088300030623     C                             %subst(wNomeFile:7:2) = '08' OR
088400091109     C                             %subst(wNomeFile:7:2) = '09'
088500030623     C                   EVAL      vlrpoi = 006
088600100319     C                   OTHER
088700030623     C                   EVAL      vlrpoi = 043
088800100319     C                   ENDSL
088900030616     C                   ENDIF
089000010606     C*
089100010606     C* CALCOLA LA DATA CORRENTE
089200100319     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
089300100319     C                   eval      datcor = %dec(%date() : *ISO)
089400000613     C*
089500000613     C                   ENDSR
089600000613     C***
