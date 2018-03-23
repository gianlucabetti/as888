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
013200130221     C*
013300130221     C* Forzatura x correzzione errori Wurth
013400130221     C                   if        %subst(vindta:251:1) = ','
013500130221     C                   eval      vindta = %subst(vindta:1:243)  +
013600130221     C                                      %subst(vindta:245:31) +
013700130221     C                                      %subst(vindta:277)
013800130221     C                   endif
013900130221     C*
014000130221     C                   if        %subst(vindta:283:1) = ','
014100130221     C                   eval      vindta = %subst(vindta:1:274)  + '00' +
014200130221     C                                      %subst(vindta:275)
014300130221     C                   endif
014400010711     C*
014500990913     C                   eval      fnvabaar = vindta
014600991022     C*
014700991022     C                   testb     x'3F'         vhbatb                   02
014800991022     C                   if        not *in02
014900991022     C*
015000971021     C                   SETOFF                                       303132
015100971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
015200971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
015300971023     C*    OPPURE TUTTI BLANKS
015400010530     C*
015500010530     C                   TESTN                   VHBLNP               30
015600010530     C  N30              TESTN                   VHBLNP                 30
015700010530     C  N30              TESTN                   VHBLNP                   30
015800010530     C                   MOVE      VHBLNP        W001A             1
015900010530     C     *IN30         IFEQ      *OFF
016000010530     C     W001A         ORLT      '0'
016100010530     C                   SETON                                        31
016200010530     C                   eval      vinmsg = %trimr(vinmsg)
016300010530     C                             + ' ' + 'VHBLNP'
016400010530     C                   ELSE
016500010530     C                   MOVE      VHBLNP        VABLNP
016600010530     C     VABLNP        IFLE      *ZEROS
016700010530     C                   SETON                                        31
016800010530     C                   eval      vinmsg = %trimr(vinmsg)
016900010530     C                             + ' ' + 'VHBLNP'
017000010530     C                   ENDIF
017100010530     C                   ENDIF
017200010530     C*
017300010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017400010530     C                   eval      depfil = VABLNP
017500010530     C                   exsr      repfil
017600010601     C                   if        depfil = invfil
017700021014     C                   if        vlrpoi = 999
017800021010     C                   MOVE(P)   invfil        VABFGS
017900021014     C                   else
018000021014     C                   MOVE(P)   vlrpoi        VABFGS
018100021014     C                   endif
018200010530     C*
018300010530     C                   exsr      PREELA
018400010530     C*
018500971023     C                   TESTN                   VHBCCM               30
018600971023     C  N30              TESTN                   VHBCCM                 30
018700971023     C  N30              TESTN                   VHBCCM                   30
018800971021     C                   MOVE      VHBCCM        W001A             1
018900971021     C     *IN30         IFEQ      *OFF
019000971021     C     W001A         ORLT      '0'
019100971021     C                   SETON                                        31
019200010604     C                   eval      vinmsg = %trimr(vinmsg)
019300010604     C                             + ' ' + 'VHBCCM'
019400971021     C                   ELSE
019500971021     C                   MOVE      VHBCCM        VABCCM
019600971024     C     VABCCM        IFLE      *ZEROS
019700971024     C                   SETON                                        31
019800010604     C                   eval      vinmsg = %trimr(vinmsg)
019900010604     C                             + ' ' + 'VHBCCM'
020000971024     C                   ENDIF
020100971021     C                   ENDIF
020200971021     C*
020300971023     C                   TESTN                   VHBAAS               30
020400971023     C  N30              TESTN                   VHBAAS                 30
020500971023     C  N30              TESTN                   VHBAAS                   30
020600971021     C                   MOVE      VHBAAS        W001A             1
020700971021     C     *IN30         IFEQ      *OFF
020800971021     C     W001A         ORLT      '0'
020900971023     C     W001A         ANDNE     *BLANK
021000971021     C                   SETON                                        32
021100010606     C                   MOVEL     datcor        VABAAS
021200010604     C                   eval      vinmsg = %trimr(vinmsg)
021300010604     C                             + ' ' + 'VHBAAS'
021400971021     C                   ELSE
021500971021     C                   MOVE      VHBAAS        VABAAS
021600971021     C                   ENDIF
021700971021     C*
021800971023     C                   TESTN                   VHBMGS               30
021900971023     C  N30              TESTN                   VHBMGS                 30
022000971023     C  N30              TESTN                   VHBMGS                   30
022100971021     C                   MOVE      VHBMGS        W001A             1
022200971021     C     *IN30         IFEQ      *OFF
022300971021     C     W001A         ORLT      '0'
022400971023     C     W001A         ANDNE     *BLANK
022500971021     C                   SETON                                        32
022600010601     C                   MOVE      '0101'        VABMGS
022700010604     C                   eval      vinmsg = %trimr(vinmsg)
022800010604     C                             + ' ' + 'VHBMGS'
022900971021     C                   ELSE
023000971021     C                   MOVE      VHBMGS        VABMGS
023100971021     C                   ENDIF
023200971021     C*
023300971023     C                   TESTN                   VHBNRS               30
023400971023     C  N30              TESTN                   VHBNRS                 30
023500971023     C  N30              TESTN                   VHBNRS                   30
023600971021     C                   MOVE      VHBNRS        W001A             1
023700971021     C     *IN30         IFEQ      *OFF
023800971021     C     W001A         ORLT      '0'
023900971023     C     W001A         ANDNE     *BLANK
024000971021     C                   SETON                                        32
024100971021     C                   MOVE      *ZERO         VABNRS
024200010604     C                   eval      vinmsg = %trimr(vinmsg)
024300010604     C                             + ' ' + 'VHBNRS'
024400971021     C                   ELSE
024500971021     C                   MOVE      VHBNRS        VABNRS
024600971021     C                   ENDIF
024700971021     C*
024800971023     C                   TESTN                   VHBNSP               30
024900971023     C  N30              TESTN                   VHBNSP                 30
025000971023     C  N30              TESTN                   VHBNSP                   30
025100971021     C                   MOVE      VHBNSP        W001A             1
025200971021     C     *IN30         IFEQ      *OFF
025300971021     C     W001A         ORLT      '0'
025400971023     C     W001A         ANDNE     *BLANK
025500971021     C                   SETON                                        32
025600971021     C                   MOVE      *ZERO         VABNSP
025700010604     C                   eval      vinmsg = %trimr(vinmsg)
025800010604     C                             + ' ' + 'VHBNSP'
025900971021     C                   ELSE
026000971021     C                   MOVE      VHBNSP        VABNSP
026100971021     C                   ENDIF
026200971021     C*
026300971023     C                   TESTN                   VHBLNA               30
026400971023     C  N30              TESTN                   VHBLNA                 30
026500971023     C  N30              TESTN                   VHBLNA                   30
026600971021     C                   MOVE      VHBLNA        W001A             1
026700971021     C     *IN30         IFEQ      *OFF
026800971021     C     W001A         ORLT      '0'
026900971023     C     W001A         ANDNE     *BLANK
027000971021     C                   SETON                                        32
027100971021     C                   MOVE      *ZERO         VABLNA
027200010604     C                   eval      vinmsg = %trimr(vinmsg)
027300010604     C                             + ' ' + 'VHBLNA'
027400971021     C                   ELSE
027500971021     C                   MOVE      VHBLNA        VABLNA
027600971021     C                   ENDIF
027700971021     C*
027800971023     C                   TESTN                   VHBCTR               30
027900971023     C  N30              TESTN                   VHBCTR                 30
028000971023     C  N30              TESTN                   VHBCTR                   30
028100971021     C                   MOVE      VHBCTR        W001A             1
028200971021     C     *IN30         IFEQ      *OFF
028300971021     C     W001A         ORLT      '0'
028400971023     C     W001A         ANDNE     *BLANK
028500971021     C                   SETON                                        32
028600971021     C                   MOVE      *ZERO         VABCTR
028700010604     C                   eval      vinmsg = %trimr(vinmsg)
028800010604     C                             + ' ' + 'VHBCTR'
028900971021     C                   ELSE
029000971021     C                   MOVE      VHBCTR        VABCTR
029100971021     C                   ENDIF
029200941229     C*
029300971021     C                   MOVE      *ZERO         VABIAS
029400941229     C                   MOVE      VHBIAS        WASC15
029500971023     C                   TESTN                   WI15                 30
029600971023     C  N30              TESTN                   WI15                   30
029700971023     C  N30              TESTN                   WI15                     30
029800971021     C                   MOVE      WI15          W001A             1
029900971021     C     *IN30         IFEQ      *OFF
030000971021     C     W001A         ORLT      '0'
030100971023     C     W001A         ANDNE     *BLANK
030200971021     C                   SETON                                        32
030300971021     C                   MOVE      *ZERO         VABIAS
030400010604     C                   eval      vinmsg = %trimr(vinmsg)
030500010604     C                             + ' ' + 'VHBIAS'
030600971021     C                   ELSE
030700971021     C                   MOVEL     WI15          VABIAS
030800971021     C                   ENDIF
030900971023     C                   TESTN                   WD15                 30
031000971023     C  N30              TESTN                   WD15                   30
031100971023     C  N30              TESTN                   WD15                     30
031200971021     C                   MOVE      WD15          W001A             1
031300971021     C     *IN30         IFEQ      *OFF
031400971021     C     W001A         ORLT      '0'
031500971023     C     W001A         ANDNE     *BLANK
031600971021     C                   SETON                                        32
031700971021     C                   MOVE      *ZERO         VABIAS
031800010604     C                   eval      vinmsg = %trimr(vinmsg)
031900010604     C                             + ' ' + 'VHBIAS'
032000971021     C                   ELSE
032100971021     C                   MOVE      WD15          VABIAS
032200971021     C                   ENDIF
032300941229     C*
032400971023     C                   TESTN                   VHBNCL               30
032500971023     C  N30              TESTN                   VHBNCL                 30
032600971023     C  N30              TESTN                   VHBNCL                   30
032700971021     C                   MOVE      VHBNCL        W001A             1
032800971021     C     *IN30         IFEQ      *OFF
032900971021     C     W001A         ORLT      '0'
033000971021     C                   SETON                                        32
033100971021     C                   MOVE      *ZERO         VABNCL
033200010604     C                   eval      vinmsg = %trimr(vinmsg)
033300010604     C                             + ' ' + 'VHBNCL'
033400971021     C                   ELSE
033500971021     C                   MOVE      VHBNCL        VABNCL
033600971021     C                   ENDIF
033700941229     C*
033800971021     C                   MOVE      *ZERO         VABPKB
033900971021     C                   MOVE      VHBPKB        WASC09
034000010719     C                   IF        WI09 = *blanks
034100010719     C                   EVAL      WI09 = *zeros
034200010719     C                   ENDIF
034300971023     C                   TESTN                   WI09                 30
034400971023     C  N30              TESTN                   WI09                   30
034500971023     C  N30              TESTN                   WI09                     30
034600971021     C                   MOVE      WI09          W001A             1
034700971021     C     *IN30         IFEQ      *OFF
034800971021     C     W001A         ORLT      '0'
034900971021     C                   SETON                                        32
035000971021     C                   MOVE      *ZERO         VABPKB
035100010604     C                   eval      vinmsg = %trimr(vinmsg)
035200010604     C                             + ' ' + 'VHBPKB'
035300971021     C                   ELSE
035400971021     C                   MOVEL     WI09          VABPKB
035500971021     C                   ENDIF
035600010719     C                   IF        WD09 = *blanks
035700010719     C                   EVAL      WD09 = *zeros
035800010719     C                   ENDIF
035900971023     C                   TESTN                   WD09                 30
036000971023     C  N30              TESTN                   WD09                     30
036100971021     C                   MOVE      WD09          W001A             1
036200971021     C     *IN30         IFEQ      *OFF
036300971021     C     W001A         ORLT      '0'
036400971023     C     W001A         ANDNE     *BLANK
036500971021     C                   SETON                                        32
036600971021     C                   MOVE      *ZERO         VABPKB
036700010604     C                   eval      vinmsg = %trimr(vinmsg)
036800010604     C                             + ' ' + 'VHBPKB'
036900971021     C                   ELSE
037000971021     C                   MOVE      WD09          VABPKB
037100971021     C                   ENDIF
037200971021     C*
037300971021     C                   MOVE      *ZERO         VABVLB
037400971021     C                   MOVE      VHBVLB        WASC07
037500010719     C                   IF        WI07 = *blanks
037600010719     C                   EVAL      WI07 = *zeros
037700010719     C                   ENDIF
037800971023     C                   TESTN                   WI07                 30
037900971023     C  N30              TESTN                   WI07                   30
038000971023     C  N30              TESTN                   WI07                     30
038100971021     C                   MOVE      WI07          W001A             1
038200971021     C     *IN30         IFEQ      *OFF
038300971021     C     W001A         ORLT      '0'
038400971023     C     W001A         ANDNE     *BLANK
038500971021     C                   SETON                                        32
038600971021     C                   MOVE      *ZERO         VABVLB
038700010604     C                   eval      vinmsg = %trimr(vinmsg)
038800010604     C                             + ' ' + 'VHBVLB'
038900971021     C                   ELSE
039000971021     C                   MOVEL     WI07          VABVLB
039100971021     C                   ENDIF
039200010719     C                   IF        WD07 = *blanks
039300010719     C                   EVAL      WD07 = *zeros
039400010719     C                   ENDIF
039500971023     C                   TESTN                   WD07                 30
039600971023     C  N30              TESTN                   WD07                   30
039700971023     C  N30              TESTN                   WD07                     30
039800971021     C                   MOVE      WD07          W001A             1
039900971021     C     *IN30         IFEQ      *OFF
040000971021     C     W001A         ORLT      '0'
040100971023     C     W001A         ANDNE     *BLANK
040200971021     C                   SETON                                        32
040300971021     C                   MOVE      *ZERO         VABVLB
040400010604     C                   eval      vinmsg = %trimr(vinmsg)
040500010604     C                             + ' ' + 'VHBVLB'
040600971021     C                   ELSE
040700971021     C                   MOVE      WD07          VABVLB
040800971021     C                   ENDIF
040900941229     C*
041000971021     C                   MOVE      *ZERO         VABQFT
041100971021     C                   MOVE      VHBQFT        WASC15
041200010719     C                   IF        WI15 = *blanks
041300010719     C                   EVAL      WI15 = *zeros
041400010719     C                   ENDIF
041500971023     C                   TESTN                   WI15                 30
041600971023     C  N30              TESTN                   WI15                   30
041700971023     C  N30              TESTN                   WI15                     30
041800971021     C                   MOVE      WI15          W001A             1
041900971021     C     *IN30         IFEQ      *OFF
042000971021     C     W001A         ORLT      '0'
042100971023     C     W001A         ANDNE     *BLANK
042200971021     C                   SETON                                        32
042300971021     C                   MOVE      *ZERO         VABQFT
042400010604     C                   eval      vinmsg = %trimr(vinmsg)
042500010604     C                             + ' ' + 'VHBQFT'
042600971021     C                   ELSE
042700971021     C                   MOVEL     WI15          VABQFT
042800971021     C                   ENDIF
042900010719     C                   IF        WD15 = *blanks
043000010719     C                   EVAL      WD15 = *zeros
043100010719     C                   ENDIF
043200971023     C                   TESTN                   WD15                 30
043300971023     C  N30              TESTN                   WD15                   30
043400971023     C  N30              TESTN                   WD15                     30
043500971021     C                   MOVE      WD15          W001A             1
043600971021     C     *IN30         IFEQ      *OFF
043700971021     C     W001A         ORLT      '0'
043800971023     C     W001A         ANDNE     *BLANK
043900971021     C                   SETON                                        32
044000971021     C                   MOVE      *ZERO         VABQFT
044100010604     C                   eval      vinmsg = %trimr(vinmsg)
044200010604     C                             + ' ' + 'VHBQFT'
044300971021     C                   ELSE
044400971021     C                   MOVE      WD15          VABQFT
044500971021     C                   ENDIF
044600941229     C*
044700971021     C                   MOVE      *ZERO         VABCAS
044800971021     C                   MOVE      VHBCAS        WASC15
044900010719     C                   IF        WI15 = *blanks
045000010719     C                   EVAL      WI15 = *zeros
045100010719     C                   ENDIF
045200971023     C                   TESTN                   WI15                 30
045300971023     C  N30              TESTN                   WI15                   30
045400971023     C  N30              TESTN                   WI15                     30
045500971021     C                   MOVE      WI15          W001A             1
045600971021     C     *IN30         IFEQ      *OFF
045700971021     C     W001A         ORLT      '0'
045800971023     C     W001A         ANDNE     *BLANK
045900971021     C                   SETON                                        32
046000971021     C                   MOVE      *ZERO         VABCAS
046100010604     C                   eval      vinmsg = %trimr(vinmsg)
046200010604     C                             + ' ' + 'VHBCAS'
046300971021     C                   ELSE
046400971021     C                   MOVEL     WI15          VABCAS
046500971021     C                   ENDIF
046600010719     C                   IF        WD15 = *blanks
046700010719     C                   EVAL      WD15 = *zeros
046800010719     C                   ENDIF
046900971023     C                   TESTN                   WD15                 30
047000971023     C  N30              TESTN                   WD15                   30
047100971023     C  N30              TESTN                   WD15                     30
047200971021     C                   MOVE      WD15          W001A             1
047300971021     C     *IN30         IFEQ      *OFF
047400971021     C     W001A         ORLT      '0'
047500971023     C     W001A         ANDNE     *BLANK
047600971021     C                   SETON                                        32
047700971021     C                   MOVE      *ZERO         VABCAS
047800010604     C                   eval      vinmsg = %trimr(vinmsg)
047900010604     C                             + ' ' + 'VHBCAS'
048000971021     C                   ELSE
048100971021     C                   MOVE      WD15          VABCAS
048200971021     C                   ENDIF
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
059100971021     C                   ENDIF
059200010719     C                   IF        WD15 = *blanks
059300010719     C                   EVAL      WD15 = *zeros
059400010719     C                   ENDIF
059500971023     C                   TESTN                   WD15                 30
059600971023     C  N30              TESTN                   WD15                   30
059700971023     C  N30              TESTN                   WD15                     30
059800971021     C                   MOVE      WD15          W001A             1
059900971021     C     *IN30         IFEQ      *OFF
060000971021     C     W001A         ORLT      '0'
060100971023     C     W001A         ANDNE     *BLANK
060200971021     C                   SETON                                        32
060300971021     C                   MOVE      *ZERO         VABVMD
060400010604     C                   eval      vinmsg = %trimr(vinmsg)
060500010604     C                             + ' ' + 'VHBVMD'
060600971021     C                   ELSE
060700971021     C                   MOVE      WD15          VABVMD
060800971021     C                   ENDIF
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
066600971021     C* NELLA NAZIONE ORIGINALE
066700971021     C                   MOVE      VHBNMO        WA1D              1
066800971021     C                   MOVEL     VHBNMO        WA1S              1
066900971021     C     WA1D          IFNE      *BLANK
067000971021     C     WA1S          ANDEQ     *BLANK
067100971021     C                   MOVE      *BLANK        VABNMO
067200030616     C                   ENDIF
067300030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
067400110131     C                   EVAL      VABCMR = 'WURTH - ' + %subst(wNomeFile:4:5)
067500030616     C                   EVAL      VABDCM = DATCOR
067600030616     C                   EVAL      VABDTS = DATCOR
067700030616     C                   EVAL      VABCNT = 1
067800130221     C*
067900130221 xxx C* Forzatura TEMPORANEA x nuovo servizio Wurth "abbinamento bolle"
068000130221     C                   IF        VABXCO = 'W'
068100130221     C                   EVAL      VABXCO = *blanks
068200130221     C                   ENDIF
068300930409     C*
068400010604     C  N31              ADD       1             §CTROK            7 0
068500010604     C   32              ADD       1             §CTRMO            7 0
068600010604     C   31              ADD       1             §CTRNO            7 0
068700030616     C  N31              WRITE     EDIVAB00
068800990910     C*
068900010601     C                   if        *in31 = *off and
069000010601     C                             *in32 = *off
069100990910     C                   eval      vinflg = '1'
069200990910     C                   else
069300990910     C                   eval      vinflg = '2'
069400990910     C                   endif
069500991022     C                   endif
069600990910     C                   endif
069700010601     C*
069800010601     C                   endif
069900000905     C*
070000000905     C                   else
070100000905     C                   eval      vinflg = '1'
070200000905     C                   endif
070300000905     C*
070400000905     C  N70              update    tivin000
070500991022     C*
070600991022     C  N70              ENDdo
070700010530     C
070800010530     C*
070900010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
071000010530     C                   if        cntNonEl = *zeros or
071100010530     C                             flgMulti = '0'
071200990910     C* Se non ci sono record con errori ...
071300010530     C                   if        §ctrno = 0 and
071400010601     C                             §ctrmo = 0 and
071500010530     C                             flgStato <> '2'
071600990910     C* ... restituisco esito OK.
071700990921     C                   eval      wrkesito = '0'
071800990910     C                   else
071900010601     C                   if        §ctrok > 0
072000990921     C                   eval      wrkesito = '1'
072100000710     C                   else
072200010615     C                   if        flgOk = '0'
072300010615     C                   eval      wrkesito = '2'
072400010615     C                   else
072500010615     C                   eval      wrkesito = '6'
072600010615     C                   endif
072700990910     C                   endif
072800000710     C                   endif
072900010530     C                   else
073000010530     C                   eval      wrkesito = '9'
073100010530     C                   endif
073200990910     C*
073300990914     C                   if        %open(tivin00r)
073400990908     C                   close     tivin00r
073500990914     C                   endif
073600030616     C                   if        %open(edivabwr)
073700030616     C                   close     edivabwr
073800990914     C                   endif
073900010530     C*
074000010530     C                   if        vlrpoi <> 999
074100010530     C                   eval      invfil = vlrpoi
074200010530     C                   endif
074300990910     C*
074400990920     C                   if        §ctrok > 0
074500010530     C                             and invfil > *zeros
074600000613     C                   exsr      invio
074700990920     C                   endif
074800010612     C*
074900010612     C                   if        flgGiro = '1'
075000010612     C                   exsr      endela
075100010612     C                   endif
075200990920     C*
075300910830     C                   ENDSR
075400000613     C***
075500990920
075600010530
075700010530
075800010530
075900010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
076000010530     C     repfil        BEGSR
076100010530     C*
076200010530     C                   if        invfil = *zeros and
076300010530     C                             depfil > *zeros and
076400010613     C                             (vinflg = *blanks or
076500010613     C                              vinflg = *zeros)
076600010530     C
076700010530     C                   eval      invfil = depfil
076800010530     C                   endif
076900010530     C*
077000010530     C                   if        depfil <> invfil and
077100010530     C                             invfil > *zeros
077200010530     C                   eval      flgMulti = '1'
077300010530     C                   if        vinflg = *blanks
077400010530     C                   add       1             cntNonEl
077500010530     C                   endif
077600010530     C                   endif
077700010530     C*
077800010530     C                   if        vinflg = '2'
077900010530     C                   eval      flgStato = '2'
078000010530     C                   endif
078100010530     C*
078200010530     C                   ENDSR
078300010530     C***
078400010530
078500010530
078600010530
078700010530
078800990920      /TITLE Invio dei dati al punto operativo.
078900000613     C     invio         BEGSR
079000990920     C*
079100990920     C                   reset                   dscmz
079200030618     C                   move      invfil        cmzdst
079300030618     C***                move      '789'         cmzdst
079400990920     C                   eval      cmzfld = vlrfou
079500990920     C                   eval      cmzmbd = vlrhdl
079600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
079700000710     C                   if        prmfir = *blanks
079800030616     C                   eval      cmzfla = 'EDIVAB0F'
079900030616     C                   eval      cmzmba = 'EDIVAB0F'
080000000710     C                   else
080100000710     C                   eval      cmzfla = prmfir
080200000710     C                   eval      cmzmba = prmfir
080300000710     C                   endif
080400990920     C                   eval      cmznrr = *zeros
080500990920     C                   move      §ctrok        cmznrr
080600021003     C                   eval      cmzlba = vlrfl1
080700990920     C                   call(e)   'TIS711C'
080800990920     C                   parm                    dscmz
080900990921     C                   parm      *blanks       esito
081000990920     C                   if        %error
081100990920     C                             or cmzerr = '1'
081200990921     C                             or esito  = '1'
081300000710     C                   eval      wrkesito = '3'
081400990920     C                   endif
081500990920     C*
081600000613     C                   ENDSR
081700000613     C***
081800010530
081900010530
082000010530
082100010530
082200010530      /TITLE Invio dei dati al punto operativo.
082300010530     C     opeini        BEGSR
082400010530     C*
082500010530     C* Inizializzo flag e contatori operativi
082600010530     C                   movel     '0'           flgGiro           1
082700010530     C                   movel     '0'           flgMulti          1
082800010530     C                   movel     '1'           flgStato          1
082900010615     C                   movel     '0'           flgOk             1
083000010530     C                   z-add     *zeros        cntNonEl         10 0
083100010530     C                   z-add     *zeros        depfil            3 0
083200010530     C                   z-add     *zeros        invfil            3 0
083300030407     C*
083400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
083500030407     C                   if        %subst(vlrppt:1:1) = 'S'
083600030407     C                   seton                                        50
083700030407     C                   else
083800030407     C                   setoff                                       50
083900030407     C                   endif
084000010530     C*
084100010530     C                   ENDSR
084200010530     C***
084300010530
084400010530
084500010530
084600990910
084700000613     C     *inzsr        BEGSR
084800990910     C*
084900990910     C     *entry        plist
085000990920     C                   parm                    tivlrds
085100990921     C                   parm      wrkesito      esito
085200000724     C                   parm                    prmlit
085300000710     C                   parm                    prmfir
085400030616     C*
085500030616     C* SE RICHIESTO NEI PARAMETRI DI TRADUZIONE
085600030616     C* REPERISCO SUBITO IL NOME DEL FILE "UPLOADATO" (SE C'E') E REPERISCO LA FILIALE A CUI INVIARE
085700030618     C                   MOVE(P)   vlrMSG        wNomeFile
085800030616     C                   IF        %subst(vlrPPT:2:1) = 'S'
085900100319     C                   SELECT
086000100319     C                   WHEN      %subst(wNomeFile:7:2) = '60' OR
086100091207     C                             %subst(wNomeFile:7:2) = '61' OR
086200091210     C                             %subst(wNomeFile:7:2) = '62' OR
086300101012     C                             %subst(wNomeFile:7:2) = '63' OR
086400101012     C                             %subst(wNomeFile:7:2) = '64' OR
086500101012     C                             %subst(wNomeFile:7:2) = '65' OR
086600101012     C                             %subst(wNomeFile:7:2) = '66' OR
086700101012     C                             %subst(wNomeFile:7:2) = '67' OR
086800081016     C                             %subst(wNomeFile:7:2) = '68' OR
086900100319     C                             %subst(wNomeFile:7:2) = '69'
087000081016     C                   EVAL      vlrpoi = 999
087100100319     C                   WHEN      %subst(wNomeFile:7:2) = '99'
087200030623     C                   EVAL      vlrpoi = 048
087300101012     C                   WHEN      %subst(wNomeFile:7:2) = '01' OR
087400100301     C                             %subst(wNomeFile:7:2) = '02' OR
087500100319     C                             %subst(wNomeFile:7:2) = '03' OR
087600101012     C                             %subst(wNomeFile:7:2) = '04' OR
087700101012     C                             %subst(wNomeFile:7:2) = '13' OR
087800101012     C                             %subst(wNomeFile:7:2) = '24' OR
087900101012     C                             %subst(wNomeFile:7:2) = '25' OR
088000101012     C                             %subst(wNomeFile:7:2) = '55'
088100100319     C                   EVAL      vlrpoi = 277
088200130220     C                   WHEN      %subst(wNomeFile:7:2) = '95' OR
088300130220     C                             %subst(wNomeFile:7:2) = '96'
088400130220     C                   EVAL      vlrpoi = 043
088500100319     C                   OTHER
088600030623     C                   EVAL      vlrpoi = 043
088700100319     C                   ENDSL
088800030616     C                   ENDIF
088900010606     C*
089000010606     C* CALCOLA LA DATA CORRENTE
089100100319     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
089200100319     C                   eval      datcor = %dec(%date() : *ISO)
089300000613     C*
089400000613     C                   ENDSR
089500000613     C***
