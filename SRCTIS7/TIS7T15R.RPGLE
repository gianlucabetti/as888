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
016700150903     C* FORZATURA PER SPOSTAMENTO WURTH A ROVERETO
016800150903     C                   if        VABFGS = 043
016900150903     C                   eval      VABFGS = 172
017000150903     C                   endif
017100150903     C*
017200021014     C                   else
017300021014     C                   MOVE(P)   vlrpoi        VABFGS
017400021014     C                   endif
017500010530     C*
017600010530     C                   exsr      PREELA
017700010530     C*
017800971023     C                   TESTN                   VHBCCM               30
017900971023     C  N30              TESTN                   VHBCCM                 30
018000971023     C  N30              TESTN                   VHBCCM                   30
018100971021     C                   MOVE      VHBCCM        W001A             1
018200971021     C     *IN30         IFEQ      *OFF
018300971021     C     W001A         ORLT      '0'
018400971021     C                   SETON                                        31
018500010604     C                   eval      vinmsg = %trimr(vinmsg)
018600010604     C                             + ' ' + 'VHBCCM'
018700971021     C                   ELSE
018800971021     C                   MOVE      VHBCCM        VABCCM
018900971024     C     VABCCM        IFLE      *ZEROS
019000971024     C                   SETON                                        31
019100010604     C                   eval      vinmsg = %trimr(vinmsg)
019200010604     C                             + ' ' + 'VHBCCM'
019300971024     C                   ENDIF
019400971021     C                   ENDIF
019500971021     C*
019600971023     C                   TESTN                   VHBAAS               30
019700971023     C  N30              TESTN                   VHBAAS                 30
019800971023     C  N30              TESTN                   VHBAAS                   30
019900971021     C                   MOVE      VHBAAS        W001A             1
020000971021     C     *IN30         IFEQ      *OFF
020100971021     C     W001A         ORLT      '0'
020200971023     C     W001A         ANDNE     *BLANK
020300971021     C                   SETON                                        32
020400010606     C                   MOVEL     datcor        VABAAS
020500010604     C                   eval      vinmsg = %trimr(vinmsg)
020600010604     C                             + ' ' + 'VHBAAS'
020700971021     C                   ELSE
020800971021     C                   MOVE      VHBAAS        VABAAS
020900971021     C                   ENDIF
021000971021     C*
021100971023     C                   TESTN                   VHBMGS               30
021200971023     C  N30              TESTN                   VHBMGS                 30
021300971023     C  N30              TESTN                   VHBMGS                   30
021400971021     C                   MOVE      VHBMGS        W001A             1
021500971021     C     *IN30         IFEQ      *OFF
021600971021     C     W001A         ORLT      '0'
021700971023     C     W001A         ANDNE     *BLANK
021800971021     C                   SETON                                        32
021900010601     C                   MOVE      '0101'        VABMGS
022000010604     C                   eval      vinmsg = %trimr(vinmsg)
022100010604     C                             + ' ' + 'VHBMGS'
022200971021     C                   ELSE
022300971021     C                   MOVE      VHBMGS        VABMGS
022400971021     C                   ENDIF
022500971021     C*
022600971023     C                   TESTN                   VHBNRS               30
022700971023     C  N30              TESTN                   VHBNRS                 30
022800971023     C  N30              TESTN                   VHBNRS                   30
022900971021     C                   MOVE      VHBNRS        W001A             1
023000971021     C     *IN30         IFEQ      *OFF
023100971021     C     W001A         ORLT      '0'
023200971023     C     W001A         ANDNE     *BLANK
023300971021     C                   SETON                                        32
023400971021     C                   MOVE      *ZERO         VABNRS
023500010604     C                   eval      vinmsg = %trimr(vinmsg)
023600010604     C                             + ' ' + 'VHBNRS'
023700971021     C                   ELSE
023800971021     C                   MOVE      VHBNRS        VABNRS
023900971021     C                   ENDIF
024000971021     C*
024100971023     C                   TESTN                   VHBNSP               30
024200971023     C  N30              TESTN                   VHBNSP                 30
024300971023     C  N30              TESTN                   VHBNSP                   30
024400971021     C                   MOVE      VHBNSP        W001A             1
024500971021     C     *IN30         IFEQ      *OFF
024600971021     C     W001A         ORLT      '0'
024700971023     C     W001A         ANDNE     *BLANK
024800971021     C                   SETON                                        32
024900971021     C                   MOVE      *ZERO         VABNSP
025000010604     C                   eval      vinmsg = %trimr(vinmsg)
025100010604     C                             + ' ' + 'VHBNSP'
025200971021     C                   ELSE
025300971021     C                   MOVE      VHBNSP        VABNSP
025400971021     C                   ENDIF
025500971021     C*
025600971023     C                   TESTN                   VHBLNA               30
025700971023     C  N30              TESTN                   VHBLNA                 30
025800971023     C  N30              TESTN                   VHBLNA                   30
025900971021     C                   MOVE      VHBLNA        W001A             1
026000971021     C     *IN30         IFEQ      *OFF
026100971021     C     W001A         ORLT      '0'
026200971023     C     W001A         ANDNE     *BLANK
026300971021     C                   SETON                                        32
026400971021     C                   MOVE      *ZERO         VABLNA
026500010604     C                   eval      vinmsg = %trimr(vinmsg)
026600010604     C                             + ' ' + 'VHBLNA'
026700971021     C                   ELSE
026800971021     C                   MOVE      VHBLNA        VABLNA
026900971021     C                   ENDIF
027000971021     C*
027100971023     C                   TESTN                   VHBCTR               30
027200971023     C  N30              TESTN                   VHBCTR                 30
027300971023     C  N30              TESTN                   VHBCTR                   30
027400971021     C                   MOVE      VHBCTR        W001A             1
027500971021     C     *IN30         IFEQ      *OFF
027600971021     C     W001A         ORLT      '0'
027700971023     C     W001A         ANDNE     *BLANK
027800971021     C                   SETON                                        32
027900971021     C                   MOVE      *ZERO         VABCTR
028000010604     C                   eval      vinmsg = %trimr(vinmsg)
028100010604     C                             + ' ' + 'VHBCTR'
028200971021     C                   ELSE
028300971021     C                   MOVE      VHBCTR        VABCTR
028400971021     C                   ENDIF
028500941229     C*
028600971021     C                   MOVE      *ZERO         VABIAS
028700941229     C                   MOVE      VHBIAS        WASC15
028800971023     C                   TESTN                   WI15                 30
028900971023     C  N30              TESTN                   WI15                   30
029000971023     C  N30              TESTN                   WI15                     30
029100971021     C                   MOVE      WI15          W001A             1
029200971021     C     *IN30         IFEQ      *OFF
029300971021     C     W001A         ORLT      '0'
029400971023     C     W001A         ANDNE     *BLANK
029500971021     C                   SETON                                        32
029600971021     C                   MOVE      *ZERO         VABIAS
029700010604     C                   eval      vinmsg = %trimr(vinmsg)
029800010604     C                             + ' ' + 'VHBIAS'
029900971021     C                   ELSE
030000971021     C                   MOVEL     WI15          VABIAS
030100971021     C                   ENDIF
030200971023     C                   TESTN                   WD15                 30
030300971023     C  N30              TESTN                   WD15                   30
030400971023     C  N30              TESTN                   WD15                     30
030500971021     C                   MOVE      WD15          W001A             1
030600971021     C     *IN30         IFEQ      *OFF
030700971021     C     W001A         ORLT      '0'
030800971023     C     W001A         ANDNE     *BLANK
030900971021     C                   SETON                                        32
031000971021     C                   MOVE      *ZERO         VABIAS
031100010604     C                   eval      vinmsg = %trimr(vinmsg)
031200010604     C                             + ' ' + 'VHBIAS'
031300971021     C                   ELSE
031400971021     C                   MOVE      WD15          VABIAS
031500971021     C                   ENDIF
031600941229     C*
031700971023     C                   TESTN                   VHBNCL               30
031800971023     C  N30              TESTN                   VHBNCL                 30
031900971023     C  N30              TESTN                   VHBNCL                   30
032000971021     C                   MOVE      VHBNCL        W001A             1
032100971021     C     *IN30         IFEQ      *OFF
032200971021     C     W001A         ORLT      '0'
032300971021     C                   SETON                                        32
032400971021     C                   MOVE      *ZERO         VABNCL
032500010604     C                   eval      vinmsg = %trimr(vinmsg)
032600010604     C                             + ' ' + 'VHBNCL'
032700971021     C                   ELSE
032800971021     C                   MOVE      VHBNCL        VABNCL
032900971021     C                   ENDIF
033000941229     C*
033100971021     C                   MOVE      *ZERO         VABPKB
033200971021     C                   MOVE      VHBPKB        WASC09
033300010719     C                   IF        WI09 = *blanks
033400010719     C                   EVAL      WI09 = *zeros
033500010719     C                   ENDIF
033600971023     C                   TESTN                   WI09                 30
033700971023     C  N30              TESTN                   WI09                   30
033800971023     C  N30              TESTN                   WI09                     30
033900971021     C                   MOVE      WI09          W001A             1
034000971021     C     *IN30         IFEQ      *OFF
034100971021     C     W001A         ORLT      '0'
034200971021     C                   SETON                                        32
034300971021     C                   MOVE      *ZERO         VABPKB
034400010604     C                   eval      vinmsg = %trimr(vinmsg)
034500010604     C                             + ' ' + 'VHBPKB'
034600971021     C                   ELSE
034700971021     C                   MOVEL     WI09          VABPKB
034800971021     C                   ENDIF
034900010719     C                   IF        WD09 = *blanks
035000010719     C                   EVAL      WD09 = *zeros
035100010719     C                   ENDIF
035200971023     C                   TESTN                   WD09                 30
035300971023     C  N30              TESTN                   WD09                     30
035400971021     C                   MOVE      WD09          W001A             1
035500971021     C     *IN30         IFEQ      *OFF
035600971021     C     W001A         ORLT      '0'
035700971023     C     W001A         ANDNE     *BLANK
035800971021     C                   SETON                                        32
035900971021     C                   MOVE      *ZERO         VABPKB
036000010604     C                   eval      vinmsg = %trimr(vinmsg)
036100010604     C                             + ' ' + 'VHBPKB'
036200971021     C                   ELSE
036300971021     C                   MOVE      WD09          VABPKB
036400971021     C                   ENDIF
036500971021     C*
036600971021     C                   MOVE      *ZERO         VABVLB
036700971021     C                   MOVE      VHBVLB        WASC07
036800010719     C                   IF        WI07 = *blanks
036900010719     C                   EVAL      WI07 = *zeros
037000010719     C                   ENDIF
037100971023     C                   TESTN                   WI07                 30
037200971023     C  N30              TESTN                   WI07                   30
037300971023     C  N30              TESTN                   WI07                     30
037400971021     C                   MOVE      WI07          W001A             1
037500971021     C     *IN30         IFEQ      *OFF
037600971021     C     W001A         ORLT      '0'
037700971023     C     W001A         ANDNE     *BLANK
037800971021     C                   SETON                                        32
037900971021     C                   MOVE      *ZERO         VABVLB
038000010604     C                   eval      vinmsg = %trimr(vinmsg)
038100010604     C                             + ' ' + 'VHBVLB'
038200971021     C                   ELSE
038300971021     C                   MOVEL     WI07          VABVLB
038400971021     C                   ENDIF
038500010719     C                   IF        WD07 = *blanks
038600010719     C                   EVAL      WD07 = *zeros
038700010719     C                   ENDIF
038800971023     C                   TESTN                   WD07                 30
038900971023     C  N30              TESTN                   WD07                   30
039000971023     C  N30              TESTN                   WD07                     30
039100971021     C                   MOVE      WD07          W001A             1
039200971021     C     *IN30         IFEQ      *OFF
039300971021     C     W001A         ORLT      '0'
039400971023     C     W001A         ANDNE     *BLANK
039500971021     C                   SETON                                        32
039600971021     C                   MOVE      *ZERO         VABVLB
039700010604     C                   eval      vinmsg = %trimr(vinmsg)
039800010604     C                             + ' ' + 'VHBVLB'
039900971021     C                   ELSE
040000971021     C                   MOVE      WD07          VABVLB
040100971021     C                   ENDIF
040200941229     C*
040300971021     C                   MOVE      *ZERO         VABQFT
040400971021     C                   MOVE      VHBQFT        WASC15
040500010719     C                   IF        WI15 = *blanks
040600010719     C                   EVAL      WI15 = *zeros
040700010719     C                   ENDIF
040800971023     C                   TESTN                   WI15                 30
040900971023     C  N30              TESTN                   WI15                   30
041000971023     C  N30              TESTN                   WI15                     30
041100971021     C                   MOVE      WI15          W001A             1
041200971021     C     *IN30         IFEQ      *OFF
041300971021     C     W001A         ORLT      '0'
041400971023     C     W001A         ANDNE     *BLANK
041500971021     C                   SETON                                        32
041600971021     C                   MOVE      *ZERO         VABQFT
041700010604     C                   eval      vinmsg = %trimr(vinmsg)
041800010604     C                             + ' ' + 'VHBQFT'
041900971021     C                   ELSE
042000971021     C                   MOVEL     WI15          VABQFT
042100971021     C                   ENDIF
042200010719     C                   IF        WD15 = *blanks
042300010719     C                   EVAL      WD15 = *zeros
042400010719     C                   ENDIF
042500971023     C                   TESTN                   WD15                 30
042600971023     C  N30              TESTN                   WD15                   30
042700971023     C  N30              TESTN                   WD15                     30
042800971021     C                   MOVE      WD15          W001A             1
042900971021     C     *IN30         IFEQ      *OFF
043000971021     C     W001A         ORLT      '0'
043100971023     C     W001A         ANDNE     *BLANK
043200971021     C                   SETON                                        32
043300971021     C                   MOVE      *ZERO         VABQFT
043400010604     C                   eval      vinmsg = %trimr(vinmsg)
043500010604     C                             + ' ' + 'VHBQFT'
043600971021     C                   ELSE
043700971021     C                   MOVE      WD15          VABQFT
043800971021     C                   ENDIF
043900941229     C*
044000971021     C                   MOVE      *ZERO         VABCAS
044100971021     C                   MOVE      VHBCAS        WASC15
044200010719     C                   IF        WI15 = *blanks
044300010719     C                   EVAL      WI15 = *zeros
044400010719     C                   ENDIF
044500971023     C                   TESTN                   WI15                 30
044600971023     C  N30              TESTN                   WI15                   30
044700971023     C  N30              TESTN                   WI15                     30
044800971021     C                   MOVE      WI15          W001A             1
044900971021     C     *IN30         IFEQ      *OFF
045000971021     C     W001A         ORLT      '0'
045100971023     C     W001A         ANDNE     *BLANK
045200971021     C                   SETON                                        32
045300971021     C                   MOVE      *ZERO         VABCAS
045400010604     C                   eval      vinmsg = %trimr(vinmsg)
045500010604     C                             + ' ' + 'VHBCAS'
045600971021     C                   ELSE
045700971021     C                   MOVEL     WI15          VABCAS
045800971021     C                   ENDIF
045900010719     C                   IF        WD15 = *blanks
046000010719     C                   EVAL      WD15 = *zeros
046100010719     C                   ENDIF
046200971023     C                   TESTN                   WD15                 30
046300971023     C  N30              TESTN                   WD15                   30
046400971023     C  N30              TESTN                   WD15                     30
046500971021     C                   MOVE      WD15          W001A             1
046600971021     C     *IN30         IFEQ      *OFF
046700971021     C     W001A         ORLT      '0'
046800971023     C     W001A         ANDNE     *BLANK
046900971021     C                   SETON                                        32
047000971021     C                   MOVE      *ZERO         VABCAS
047100010604     C                   eval      vinmsg = %trimr(vinmsg)
047200010604     C                             + ' ' + 'VHBCAS'
047300971021     C                   ELSE
047400971021     C                   MOVE      WD15          VABCAS
047500971021     C                   ENDIF
047600971021     C*
047700971023     C                   TESTN                   VHBRMN               30
047800971023     C  N30              TESTN                   VHBRMN                 30
047900971023     C  N30              TESTN                   VHBRMN                   30
048000971021     C                   MOVE      VHBRMN        W001A             1
048100971021     C     *IN30         IFEQ      *OFF
048200971021     C     W001A         ORLT      '0'
048300971021     C                   SETON                                        32
048400971021     C                   MOVE      *ZERO         VABRMN
048500010604     C                   eval      vinmsg = %trimr(vinmsg)
048600010604     C                             + ' ' + 'VHBRMN'
048700971021     C                   ELSE
048800971021     C                   MOVE      VHBRMN        VABRMN
048900971021     C                   ENDIF
049000971021     C*
049100971023     C                   TESTN                   VHBNCD               30
049200971023     C  N30              TESTN                   VHBNCD                 30
049300971023     C  N30              TESTN                   VHBNCD                   30
049400971021     C                   MOVE      VHBNCD        W001A             1
049500971021     C     *IN30         IFEQ      *OFF
049600971021     C     W001A         ORLT      '0'
049700971023     C     W001A         ANDNE     *BLANK
049800971021     C                   SETON                                        32
049900971021     C                   MOVE      *ZERO         VABNCD
050000010604     C                   eval      vinmsg = %trimr(vinmsg)
050100010604     C                             + ' ' + 'VHBNCD'
050200971021     C                   ELSE
050300971021     C                   MOVE      VHBNCD        VABNCD
050400971021     C                   ENDIF
050500971021     C*
050600971023     C                   TESTN                   VHBNCA               30
050700971023     C  N30              TESTN                   VHBNCA                 30
050800971023     C  N30              TESTN                   VHBNCA                   30
050900971021     C                   MOVE      VHBNCA        W001A             1
051000971021     C     *IN30         IFEQ      *OFF
051100971021     C     W001A         ORLT      '0'
051200971023     C     W001A         ANDNE     *BLANK
051300971021     C                   SETON                                        32
051400971021     C                   MOVE      *ZERO         VABNCA
051500010604     C                   eval      vinmsg = %trimr(vinmsg)
051600010604     C                             + ' ' + 'VHBNCA'
051700971021     C                   ELSE
051800971021     C                   MOVE      VHBNCA        VABNCA
051900971021     C                   ENDIF
052000971021     C*
052100971023     C                   TESTN                   VHBZNC               30
052200971023     C  N30              TESTN                   VHBZNC                 30
052300971023     C  N30              TESTN                   VHBZNC                   30
052400971021     C                   MOVE      VHBZNC        W001A             1
052500971021     C     *IN30         IFEQ      *OFF
052600971021     C     W001A         ORLT      '0'
052700971023     C     W001A         ANDNE     *BLANK
052800971021     C                   SETON                                        32
052900971021     C                   MOVE      *ZERO         VABZNC
053000010604     C                   eval      vinmsg = %trimr(vinmsg)
053100010604     C                             + ' ' + 'VHBZNC'
053200971021     C                   ELSE
053300971021     C                   MOVE      VHBZNC        VABZNC
053400971021     C                   ENDIF
053500971021     C*
053600971023     C                   TESTN                   VHBDCR               30
053700971023     C  N30              TESTN                   VHBDCR                 30
053800971023     C  N30              TESTN                   VHBDCR                   30
053900971021     C                   MOVE      VHBDCR        W001A             1
054000971021     C     *IN30         IFEQ      *OFF
054100971021     C     W001A         ORLT      '0'
054200971023     C     W001A         ANDNE     *BLANK
054300971021     C                   SETON                                        32
054400971021     C                   MOVE      *ZERO         VABDCR
054500010604     C                   eval      vinmsg = %trimr(vinmsg)
054600010604     C                             + ' ' + 'VHBDCR'
054700971021     C                   ELSE
054800971021     C                   MOVE      VHBDCR        VABDCR
054900971021     C                   ENDIF
055000971021     C*
055100971023     C                   TESTN                   VHBHCR               30
055200971023     C  N30              TESTN                   VHBHCR                 30
055300971023     C  N30              TESTN                   VHBHCR                   30
055400971021     C                   MOVE      VHBHCR        W001A             1
055500971021     C     *IN30         IFEQ      *OFF
055600971021     C     W001A         ORLT      '0'
055700971023     C     W001A         ANDNE     *BLANK
055800971021     C                   SETON                                        32
055900971021     C                   MOVE      *ZERO         VABHCR
056000010604     C                   eval      vinmsg = %trimr(vinmsg)
056100010604     C                             + ' ' + 'VHBHCR'
056200971021     C                   ELSE
056300971021     C                   MOVE      VHBHCR        VABHCR
056400971021     C                   ENDIF
056500941229     C*
056600971021     C                   MOVE      *ZERO         VABVMD
056700971021     C                   MOVE      VHBVMD        WASC15
056800010719     C                   IF        WI15 = *blanks
056900010719     C                   EVAL      WI15 = *zeros
057000010719     C                   ENDIF
057100971023     C                   TESTN                   WI15                 30
057200971023     C  N30              TESTN                   WI15                   30
057300971023     C  N30              TESTN                   WI15                     30
057400971021     C                   MOVE      WI15          W001A             1
057500971021     C     *IN30         IFEQ      *OFF
057600971021     C     W001A         ORLT      '0'
057700971023     C     W001A         ANDNE     *BLANK
057800971021     C                   SETON                                        32
057900971021     C                   MOVE      *ZERO         VABVMD
058000010604     C                   eval      vinmsg = %trimr(vinmsg)
058100010604     C                             + ' ' + 'VHBVMD'
058200971021     C                   ELSE
058300971021     C                   MOVEL     WI15          VABVMD
058400971021     C                   ENDIF
058500010719     C                   IF        WD15 = *blanks
058600010719     C                   EVAL      WD15 = *zeros
058700010719     C                   ENDIF
058800971023     C                   TESTN                   WD15                 30
058900971023     C  N30              TESTN                   WD15                   30
059000971023     C  N30              TESTN                   WD15                     30
059100971021     C                   MOVE      WD15          W001A             1
059200971021     C     *IN30         IFEQ      *OFF
059300971021     C     W001A         ORLT      '0'
059400971023     C     W001A         ANDNE     *BLANK
059500971021     C                   SETON                                        32
059600971021     C                   MOVE      *ZERO         VABVMD
059700010604     C                   eval      vinmsg = %trimr(vinmsg)
059800010604     C                             + ' ' + 'VHBVMD'
059900971021     C                   ELSE
060000971021     C                   MOVE      WD15          VABVMD
060100971021     C                   ENDIF
060200971021     C*
060300971023     C                   TESTN                   VHBANT               30
060400971023     C  N30              TESTN                   VHBANT                 30
060500971023     C  N30              TESTN                   VHBANT                   30
060600971021     C                   MOVE      VHBANT        W001A             1
060700971021     C     *IN30         IFEQ      *OFF
060800971021     C     W001A         ORLT      '0'
060900971023     C     W001A         ANDNE     *BLANK
061000971021     C                   SETON                                        32
061100971021     C                   MOVE      *ZERO         VABANT
061200010604     C                   eval      vinmsg = %trimr(vinmsg)
061300010604     C                             + ' ' + 'VHBANT'
061400971021     C                   ELSE
061500971021     C                   MOVE      VHBANT        VABANT
061600971021     C                   ENDIF
061700971021     C*
061800971021     C* CAMPI ALFABETICI:
061900971021     C                   MOVE      VHBATB        VABATB
062000971021     C                   MOVE      VHBCBO        VABCBO
062100971021     C                   MOVE      VHBRSD        VABRSD
062200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
062300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
062400020117     C* ==
062500971021     C                   MOVE      VHBRD2        VABRD2
062600971021     C                   MOVE      VHBIND        VABIND
062700971021     C                   MOVE      VHBCAD        VABCAD
062800971021     C                   MOVE      VHBLOD        VABLOD
062900971021     C                   MOVE      VHBPRD        VABPRD
063000971021     C                   MOVE      VHBNZD        VABNZD
063100971021     C                   MOVE      VHBGC1        VABGC1
063200971021     C                   MOVE      VHBGC2        VABGC2
063300971021     C                   MOVE      VHBTSP        VABTSP
063400971021     C                   MOVE      VHBVAS        VABVAS
063500971021     C                   MOVE      VHBNAS        VABNAS
063600971021     C                   MOVE      VHBTIC        VABTIC
063700971021     C                   MOVE      VHBVCA        VABVCA
063800971021     C                   MOVE      VHBGCA        VABGCA
063900971021     C                   MOVE      VHBRMA        VABRMA
064000971021     C                   MOVE      VHBXCO        VABXCO
064100971021     C                   MOVE      VHBNOT        VABNOT
064200971021     C                   MOVE      VHBNT2        VABNT2
064300971021     C                   MOVE      VHBCTM        VABCTM
064400971021     C                   MOVE      VHBFFD        VABFFD
064500971021     C                   MOVE      VHBTCR        VABTCR
064600971021     C                   MOVE      VHBCTS        VABCTS
064700971021     C                   MOVE      VHBFTM        VABFTM
064800971021     C                   MOVE      VHBVAD        VABVAD
064900971021     C                   MOVE      VHBGMA        VABGMA
065000971021     C                   MOVE      VHBGGA        VABGGA
065100971021     C                   MOVE      VHBGVA        VABGVA
065200971021     C                   MOVE      VHBTC1        VABTC1
065300971021     C                   MOVE      VHBTC2        VABTC2
065400971021     C                   MOVE      VHBSCL        VABSCL
065500971021     C                   MOVE      VHBRMO        VABRMO
065600971021     C                   MOVE      VHBCMO        VABCMO
065700971021     C                   MOVE      VHBNMO        VABNMO
065800971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
065900971021     C* NELLA NAZIONE ORIGINALE
066000971021     C                   MOVE      VHBNMO        WA1D              1
066100971021     C                   MOVEL     VHBNMO        WA1S              1
066200971021     C     WA1D          IFNE      *BLANK
066300971021     C     WA1S          ANDEQ     *BLANK
066400971021     C                   MOVE      *BLANK        VABNMO
066500030616     C                   ENDIF
066600030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
066700110131     C                   EVAL      VABCMR = 'WURTH - ' + %subst(wNomeFile:4:5)
066800030616     C                   EVAL      VABDCM = DATCOR
066900030616     C                   EVAL      VABDTS = DATCOR
067000030616     C                   EVAL      VABCNT = 1
067100130221     C*
067200130221 xxx C* Forzatura TEMPORANEA x nuovo servizio Wurth "abbinamento bolle"
067300130226     C***                IF        VABXCO = 'W'
067400130226     C***                EVAL      VABXCO = *blanks
067500130226     C***                ENDIF
067600930409     C*
067700010604     C  N31              ADD       1             §CTROK            7 0
067800010604     C   32              ADD       1             §CTRMO            7 0
067900010604     C   31              ADD       1             §CTRNO            7 0
068000030616     C  N31              WRITE     EDIVAB00
068100990910     C*
068200010601     C                   if        *in31 = *off and
068300010601     C                             *in32 = *off
068400990910     C                   eval      vinflg = '1'
068500990910     C                   else
068600990910     C                   eval      vinflg = '2'
068700990910     C                   endif
068800991022     C                   endif
068900990910     C                   endif
069000010601     C*
069100010601     C                   endif
069200000905     C*
069300000905     C                   else
069400000905     C                   eval      vinflg = '1'
069500000905     C                   endif
069600000905     C*
069700000905     C  N70              update    tivin000
069800991022     C*
069900991022     C  N70              ENDdo
070000010530     C
070100010530     C*
070200010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
070300010530     C                   if        cntNonEl = *zeros or
070400010530     C                             flgMulti = '0'
070500990910     C* Se non ci sono record con errori ...
070600010530     C                   if        §ctrno = 0 and
070700010601     C                             §ctrmo = 0 and
070800010530     C                             flgStato <> '2'
070900990910     C* ... restituisco esito OK.
071000990921     C                   eval      wrkesito = '0'
071100990910     C                   else
071200010601     C                   if        §ctrok > 0
071300990921     C                   eval      wrkesito = '1'
071400000710     C                   else
071500010615     C                   if        flgOk = '0'
071600010615     C                   eval      wrkesito = '2'
071700010615     C                   else
071800010615     C                   eval      wrkesito = '6'
071900010615     C                   endif
072000990910     C                   endif
072100000710     C                   endif
072200010530     C                   else
072300010530     C                   eval      wrkesito = '9'
072400010530     C                   endif
072500990910     C*
072600990914     C                   if        %open(tivin00r)
072700990908     C                   close     tivin00r
072800990914     C                   endif
072900030616     C                   if        %open(edivabwr)
073000030616     C                   close     edivabwr
073100990914     C                   endif
073200010530     C*
073300010530     C                   if        vlrpoi <> 999
073400010530     C                   eval      invfil = vlrpoi
073500010530     C                   endif
073600990910     C*
073700990920     C                   if        §ctrok > 0
073800010530     C                             and invfil > *zeros
073900000613     C                   exsr      invio
074000990920     C                   endif
074100010612     C*
074200010612     C                   if        flgGiro = '1'
074300010612     C                   exsr      endela
074400010612     C                   endif
074500990920     C*
074600910830     C                   ENDSR
074700000613     C***
074800990920
074900010530
075000010530
075100010530
075200010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
075300010530     C     repfil        BEGSR
075400010530     C*
075500010530     C                   if        invfil = *zeros and
075600010530     C                             depfil > *zeros and
075700010613     C                             (vinflg = *blanks or
075800010613     C                              vinflg = *zeros)
075900010530     C
076000010530     C                   eval      invfil = depfil
076100010530     C                   endif
076200010530     C*
076300010530     C                   if        depfil <> invfil and
076400010530     C                             invfil > *zeros
076500010530     C                   eval      flgMulti = '1'
076600010530     C                   if        vinflg = *blanks
076700010530     C                   add       1             cntNonEl
076800010530     C                   endif
076900010530     C                   endif
077000010530     C*
077100010530     C                   if        vinflg = '2'
077200010530     C                   eval      flgStato = '2'
077300010530     C                   endif
077400010530     C*
077500010530     C                   ENDSR
077600010530     C***
077700010530
077800010530
077900010530
078000010530
078100990920      /TITLE Invio dei dati al punto operativo.
078200000613     C     invio         BEGSR
078300990920     C*
078400990920     C                   reset                   dscmz
078500030618     C                   move      invfil        cmzdst
078600030618     C***                move      '789'         cmzdst
078700990920     C                   eval      cmzfld = vlrfou
078800990920     C                   eval      cmzmbd = vlrhdl
078900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
079000000710     C                   if        prmfir = *blanks
079100030616     C                   eval      cmzfla = 'EDIVAB0F'
079200030616     C                   eval      cmzmba = 'EDIVAB0F'
079300000710     C                   else
079400000710     C                   eval      cmzfla = prmfir
079500000710     C                   eval      cmzmba = prmfir
079600000710     C                   endif
079700990920     C                   eval      cmznrr = *zeros
079800990920     C                   move      §ctrok        cmznrr
079900021003     C                   eval      cmzlba = vlrfl1
080000990920     C                   call(e)   'TIS711C'
080100990920     C                   parm                    dscmz
080200990921     C                   parm      *blanks       esito
080300990920     C                   if        %error
080400990920     C                             or cmzerr = '1'
080500990921     C                             or esito  = '1'
080600000710     C                   eval      wrkesito = '3'
080700990920     C                   endif
080800990920     C*
080900000613     C                   ENDSR
081000000613     C***
081100010530
081200010530
081300010530
081400010530
081500010530      /TITLE Invio dei dati al punto operativo.
081600010530     C     opeini        BEGSR
081700010530     C*
081800010530     C* Inizializzo flag e contatori operativi
081900010530     C                   movel     '0'           flgGiro           1
082000010530     C                   movel     '0'           flgMulti          1
082100010530     C                   movel     '1'           flgStato          1
082200010615     C                   movel     '0'           flgOk             1
082300010530     C                   z-add     *zeros        cntNonEl         10 0
082400010530     C                   z-add     *zeros        depfil            3 0
082500010530     C                   z-add     *zeros        invfil            3 0
082600030407     C*
082700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
082800030407     C                   if        %subst(vlrppt:1:1) = 'S'
082900030407     C                   seton                                        50
083000030407     C                   else
083100030407     C                   setoff                                       50
083200030407     C                   endif
083300010530     C*
083400010530     C                   ENDSR
083500010530     C***
083600010530
083700010530
083800010530
083900990910
084000000613     C     *inzsr        BEGSR
084100990910     C*
084200990910     C     *entry        plist
084300990920     C                   parm                    tivlrds
084400990921     C                   parm      wrkesito      esito
084500000724     C                   parm                    prmlit
084600000710     C                   parm                    prmfir
084700030616     C*
084800030616     C* SE RICHIESTO NEI PARAMETRI DI TRADUZIONE
084900030616     C* REPERISCO SUBITO IL NOME DEL FILE "UPLOADATO" (SE C'E') E REPERISCO LA FILIALE A CUI INVIARE
085000030618     C                   MOVE(P)   vlrMSG        wNomeFile
085100030616     C                   IF        %subst(vlrPPT:2:1) = 'S'
085200100319     C                   SELECT
085300100319     C                   WHEN      %subst(wNomeFile:7:2) = '60' OR
085400091207     C                             %subst(wNomeFile:7:2) = '61' OR
085500091210     C                             %subst(wNomeFile:7:2) = '62' OR
085600101012     C                             %subst(wNomeFile:7:2) = '63' OR
085700101012     C                             %subst(wNomeFile:7:2) = '64' OR
085800101012     C                             %subst(wNomeFile:7:2) = '65' OR
085900101012     C                             %subst(wNomeFile:7:2) = '66' OR
086000101012     C                             %subst(wNomeFile:7:2) = '67' OR
086100081016     C                             %subst(wNomeFile:7:2) = '68' OR
086200100319     C                             %subst(wNomeFile:7:2) = '69'
086300150903     C                   EVAL      vlrpoi = 999
086400100319     C                   WHEN      %subst(wNomeFile:7:2) = '99'
086500030623     C                   EVAL      vlrpoi = 048
086600101012     C                   WHEN      %subst(wNomeFile:7:2) = '01' OR
086700100301     C                             %subst(wNomeFile:7:2) = '02' OR
086800100319     C                             %subst(wNomeFile:7:2) = '03' OR
086900101012     C                             %subst(wNomeFile:7:2) = '04' OR
087000101012     C                             %subst(wNomeFile:7:2) = '13' OR
087100101012     C                             %subst(wNomeFile:7:2) = '24' OR
087200101012     C                             %subst(wNomeFile:7:2) = '25' OR
087300101012     C                             %subst(wNomeFile:7:2) = '55'
087400100319     C                   EVAL      vlrpoi = 277
087500130220     C                   WHEN      %subst(wNomeFile:7:2) = '95' OR
087600130220     C                             %subst(wNomeFile:7:2) = '96'
087700150831     C                   EVAL      vlrpoi = 172
087800100319     C                   OTHER
087900150831     C                   EVAL      vlrpoi = 172
088000100319     C                   ENDSL
088100030616     C                   ENDIF
088200010606     C*
088300010606     C* CALCOLA LA DATA CORRENTE
088400100319     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
088500100319     C                   eval      datcor = %dec(%date() : *ISO)
088600000613     C*
088700000613     C                   ENDSR
088800000613     C***
