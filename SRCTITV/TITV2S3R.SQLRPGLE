000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200130114     H dftactgrp(*no) actgrp(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500030616     FEDIVABWR  O    E             DISK    usropn
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
004100030616     D wNomeFile       s             30    INZ(*blanks)
004200030710     D wCMR            s             35    INZ(*blanks)
004300030710     D wPosDaA         s              2    INZ(*blanks)
004400030710     D wPosDa          s              2  0 INZ(*zeros)
004500030710     D wLungA          s              2    INZ(*blanks)
004600030710     D wLung           s              2  0 INZ(*zeros)
004700050914     D wDataOraAlfa    s             14    INZ(*blanks)
004800071018     D*-------------------
004900071018     D* COSTANTI
005000071018     D*-------------------
005100071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005200071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005300990908
005400140529
005500140529
005600140529     C*
005700140529     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005800140529     C
005900140529     C/EXEC SQL
006000140529     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006100140529     C/END-EXEC
006200140529     C
006300140529     C*
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700010530     C*
006800010530     C                   exsr      opeini
006900010530     C                   exsr      rwvab
007000041025     C*
007100041025     C* Esegue lancio TISI95R solo x chiusura
007200041025     C                   CLEAR                   TISI95DS
007300041025     C                   EVAL      I95TLA = 'C'
007400041025     C                   CALL      'TISI95R'
007500041025     C                   PARM                    TISI95DS
007600010530     C*
007700010530     C                   seton                                        lr
007800010530
007900010530
008000010530
008100010530
008200010530     C*--------------------------------------------------------
008300010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008400010530     C*--------------------------------------------------------
008500010530     C     PREELA        BEGSR
008600000724     C*
008700000724     C* SE OCCORRE SPEDIRE IN FILIALE
008800010530     C                   if        invfil <> *zeros and
008900010530     C                             flgGiro = '0'
009000010530     C*
009100010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009200010530     C                   eval      flgGiro = '1'
009300010530     C*
009400010530     C                   endif
009500010530     C*
009600010530     C                   ENDSR
009700010530     C***
009800010530
009900010530
010000010530
010100010530
010200010530
010300010530
010400010530     C*--------------------------------------------------------
010500010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010600010530     C*--------------------------------------------------------
010700010530     C     ENDELA        BEGSR
010800010530     C*
010900010530     C                   ENDSR
011000010530     C***
011100990908
011200010530
011300010530
011400010530
011500910830     C*--------------------------------------------------------
011600030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
011700910830     C*--------------------------------------------------------
011800941229     C     RWVAB         BEGSR
011900030710     C*
012000030710     C                   clear                   §CTROK
012100030710     C                   clear                   §CTRMO
012200030710     C                   clear                   §CTRNO
012300990910     C*
012400030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
012500030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
012600030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
012700030710     C                   if        *in60 = *on
012800990914     C                   if        not %open(tivin00r)
012900990908     C                   open      tivin00r
013000990914     C                   endif
013100030616     C                   if        not %open(edivabwr)
013200030616     C                   open      edivabwr
013300990914     C                   endif
013400990910     C*
013500921023     C                   DO        *HIVAL
013600990913     C*
013700990915     C                   READ      tivin00r                               70
013800071018     C*
013900071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
014000071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
014100010618     C*
014200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014300010618     C                   if        vinflg = '1'
014400010618     C                   eval      flgOk = '1'
014500010618     C                   endif
014600010705     C*
014700010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
014800010705     C                   if        %subst(vindta:1:1) = '='
014900010705     C                   eval      %subst(vindta:1:1) = *blanks
015000010705     C                   endif
015100010705     C*
015200000905     C                   if        vindta > *blanks
015300000613     C                   add       1             rrnum
015400010601     C* ...elimino eventuali segni
015500010601     C     '+':' '       xlate     vindta        vindta
015600990913     C*
015700010601     C                   if        *in70 = *off and
015800990913     C                             (vinflg = *blanks
015900010711     C                              or vinflg = '0'
016000010711     C                              or vinflg = '2')
016100990913     C*
016200030616     C                   clear                   EDIVAB00
016300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016400010711     C                   if        vinflg = *blanks or vinflg = '0'
016500010711     C                   clear                   vinmsg
016600010711     C                   endif
016700010711     C*
016800990913     C                   eval      fnvabaar = vindta
016900991022     C*
017000991022     C                   testb     x'3F'         vhbatb                   02
017100991022     C                   if        not *in02
017200991022     C*
017300971021     C                   SETOFF                                       303132
017400971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
017500971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
017600971023     C*    OPPURE TUTTI BLANKS
017700010530     C*
017800010530     C                   TESTN                   VHBLNP               30
017900010530     C  N30              TESTN                   VHBLNP                 30
018000010530     C  N30              TESTN                   VHBLNP                   30
018100010530     C                   MOVE      VHBLNP        W001A             1
018200010530     C     *IN30         IFEQ      *OFF
018300010530     C     W001A         ORLT      '0'
018400010530     C                   SETON                                        31
018500010530     C                   eval      vinmsg = %trimr(vinmsg)
018600010530     C                             + ' ' + 'VHBLNP'
018700010530     C                   ELSE
018800010530     C                   MOVE      VHBLNP        VABLNP
018900010530     C     VABLNP        IFLE      *ZEROS
019000010530     C                   SETON                                        31
019100010530     C                   eval      vinmsg = %trimr(vinmsg)
019200010530     C                             + ' ' + 'VHBLNP'
019300010530     C                   ENDIF
019400010530     C                   ENDIF
019500010530     C*
019600010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019700010530     C                   eval      depfil = VABLNP
019800010530     C                   exsr      repfil
019900010601     C                   if        depfil = invfil
020000021014     C                   if        vlrpoi = 999
020100021010     C                   MOVE(P)   invfil        VABFGS
020200021014     C                   else
020300021014     C                   MOVE(P)   vlrpoi        VABFGS
020400021014     C                   endif
020500010530     C*
020600010530     C                   exsr      PREELA
020700010530     C*
020800971023     C                   TESTN                   VHBCCM               30
020900971023     C  N30              TESTN                   VHBCCM                 30
021000971023     C  N30              TESTN                   VHBCCM                   30
021100971021     C                   MOVE      VHBCCM        W001A             1
021200971021     C     *IN30         IFEQ      *OFF
021300971021     C     W001A         ORLT      '0'
021400971021     C                   SETON                                        31
021500010604     C                   eval      vinmsg = %trimr(vinmsg)
021600010604     C                             + ' ' + 'VHBCCM'
021700971021     C                   ELSE
021800971021     C                   MOVE      VHBCCM        VABCCM
021900971024     C     VABCCM        IFLE      *ZEROS
022000971024     C                   SETON                                        31
022100010604     C                   eval      vinmsg = %trimr(vinmsg)
022200010604     C                             + ' ' + 'VHBCCM'
022300971024     C                   ENDIF
022400971021     C                   ENDIF
022500971021     C*
022600971023     C                   TESTN                   VHBAAS               30
022700971023     C  N30              TESTN                   VHBAAS                 30
022800971023     C  N30              TESTN                   VHBAAS                   30
022900971021     C                   MOVE      VHBAAS        W001A             1
023000971021     C     *IN30         IFEQ      *OFF
023100971021     C     W001A         ORLT      '0'
023200971023     C     W001A         ANDNE     *BLANK
023300971021     C                   SETON                                        32
023400010606     C                   MOVEL     datcor        VABAAS
023500010604     C                   eval      vinmsg = %trimr(vinmsg)
023600010604     C                             + ' ' + 'VHBAAS'
023700971021     C                   ELSE
023800971021     C                   MOVE      VHBAAS        VABAAS
023900971021     C                   ENDIF
024000971021     C*
024100971023     C                   TESTN                   VHBMGS               30
024200971023     C  N30              TESTN                   VHBMGS                 30
024300971023     C  N30              TESTN                   VHBMGS                   30
024400971021     C                   MOVE      VHBMGS        W001A             1
024500971021     C     *IN30         IFEQ      *OFF
024600971021     C     W001A         ORLT      '0'
024700971023     C     W001A         ANDNE     *BLANK
024800971021     C                   SETON                                        32
024900010601     C                   MOVE      '0101'        VABMGS
025000010604     C                   eval      vinmsg = %trimr(vinmsg)
025100010604     C                             + ' ' + 'VHBMGS'
025200971021     C                   ELSE
025300971021     C                   MOVE      VHBMGS        VABMGS
025400971021     C                   ENDIF
025500971021     C*
025600971023     C                   TESTN                   VHBNRS               30
025700971023     C  N30              TESTN                   VHBNRS                 30
025800971023     C  N30              TESTN                   VHBNRS                   30
025900971021     C                   MOVE      VHBNRS        W001A             1
026000971021     C     *IN30         IFEQ      *OFF
026100971021     C     W001A         ORLT      '0'
026200971023     C     W001A         ANDNE     *BLANK
026300971021     C                   SETON                                        32
026400971021     C                   MOVE      *ZERO         VABNRS
026500010604     C                   eval      vinmsg = %trimr(vinmsg)
026600010604     C                             + ' ' + 'VHBNRS'
026700971021     C                   ELSE
026800971021     C                   MOVE      VHBNRS        VABNRS
026900971021     C                   ENDIF
027000971021     C*
027100971023     C                   TESTN                   VHBNSP               30
027200971023     C  N30              TESTN                   VHBNSP                 30
027300971023     C  N30              TESTN                   VHBNSP                   30
027400971021     C                   MOVE      VHBNSP        W001A             1
027500971021     C     *IN30         IFEQ      *OFF
027600971021     C     W001A         ORLT      '0'
027700971023     C     W001A         ANDNE     *BLANK
027800971021     C                   SETON                                        32
027900971021     C                   MOVE      *ZERO         VABNSP
028000010604     C                   eval      vinmsg = %trimr(vinmsg)
028100010604     C                             + ' ' + 'VHBNSP'
028200971021     C                   ELSE
028300971021     C                   MOVE      VHBNSP        VABNSP
028400971021     C                   ENDIF
028500971021     C*
028600971023     C                   TESTN                   VHBLNA               30
028700971023     C  N30              TESTN                   VHBLNA                 30
028800971023     C  N30              TESTN                   VHBLNA                   30
028900971021     C                   MOVE      VHBLNA        W001A             1
029000971021     C     *IN30         IFEQ      *OFF
029100971021     C     W001A         ORLT      '0'
029200971023     C     W001A         ANDNE     *BLANK
029300971021     C                   SETON                                        32
029400971021     C                   MOVE      *ZERO         VABLNA
029500010604     C                   eval      vinmsg = %trimr(vinmsg)
029600010604     C                             + ' ' + 'VHBLNA'
029700971021     C                   ELSE
029800971021     C                   MOVE      VHBLNA        VABLNA
029900971021     C                   ENDIF
030000971021     C*
030100971023     C                   TESTN                   VHBCTR               30
030200971023     C  N30              TESTN                   VHBCTR                 30
030300971023     C  N30              TESTN                   VHBCTR                   30
030400971021     C                   MOVE      VHBCTR        W001A             1
030500971021     C     *IN30         IFEQ      *OFF
030600971021     C     W001A         ORLT      '0'
030700971023     C     W001A         ANDNE     *BLANK
030800971021     C                   SETON                                        32
030900971021     C                   MOVE      *ZERO         VABCTR
031000010604     C                   eval      vinmsg = %trimr(vinmsg)
031100010604     C                             + ' ' + 'VHBCTR'
031200971021     C                   ELSE
031300971021     C                   MOVE      VHBCTR        VABCTR
031400971021     C                   ENDIF
031500941229     C*
031600971021     C                   MOVE      *ZERO         VABIAS
031700941229     C                   MOVE      VHBIAS        WASC15
031800971023     C                   TESTN                   WI15                 30
031900971023     C  N30              TESTN                   WI15                   30
032000971023     C  N30              TESTN                   WI15                     30
032100971021     C                   MOVE      WI15          W001A             1
032200971021     C     *IN30         IFEQ      *OFF
032300971021     C     W001A         ORLT      '0'
032400971023     C     W001A         ANDNE     *BLANK
032500971021     C                   SETON                                        32
032600971021     C                   MOVE      *ZERO         VABIAS
032700010604     C                   eval      vinmsg = %trimr(vinmsg)
032800010604     C                             + ' ' + 'VHBIAS'
032900971021     C                   ELSE
033000971021     C                   MOVEL     WI15          VABIAS
033100971023     C                   TESTN                   WD15                 30
033200971023     C  N30              TESTN                   WD15                   30
033300971023     C  N30              TESTN                   WD15                     30
033400971021     C                   MOVE      WD15          W001A             1
033500971021     C     *IN30         IFEQ      *OFF
033600971021     C     W001A         ORLT      '0'
033700971023     C     W001A         ANDNE     *BLANK
033800971021     C                   SETON                                        32
033900971021     C                   MOVE      *ZERO         VABIAS
034000010604     C                   eval      vinmsg = %trimr(vinmsg)
034100010604     C                             + ' ' + 'VHBIAS'
034200971021     C                   ELSE
034300971021     C                   MOVE      WD15          VABIAS
034400971021     C                   ENDIF
034500050118     C                   ENDIF
034600941229     C*
034700971023     C                   TESTN                   VHBNCL               30
034800971023     C  N30              TESTN                   VHBNCL                 30
034900971023     C  N30              TESTN                   VHBNCL                   30
035000971021     C                   MOVE      VHBNCL        W001A             1
035100971021     C     *IN30         IFEQ      *OFF
035200971021     C     W001A         ORLT      '0'
035300971021     C                   SETON                                        32
035400971021     C                   MOVE      *ZERO         VABNCL
035500010604     C                   eval      vinmsg = %trimr(vinmsg)
035600010604     C                             + ' ' + 'VHBNCL'
035700971021     C                   ELSE
035800971021     C                   MOVE      VHBNCL        VABNCL
035900971021     C                   ENDIF
036000941229     C*
036100971021     C                   MOVE      *ZERO         VABPKB
036200971021     C                   MOVE      VHBPKB        WASC09
036300010719     C                   IF        WI09 = *blanks
036400010719     C                   EVAL      WI09 = *zeros
036500010719     C                   ENDIF
036600971023     C                   TESTN                   WI09                 30
036700971023     C  N30              TESTN                   WI09                   30
036800971023     C  N30              TESTN                   WI09                     30
036900971021     C                   MOVE      WI09          W001A             1
037000971021     C     *IN30         IFEQ      *OFF
037100971021     C     W001A         ORLT      '0'
037200971021     C                   SETON                                        32
037300971021     C                   MOVE      *ZERO         VABPKB
037400010604     C                   eval      vinmsg = %trimr(vinmsg)
037500010604     C                             + ' ' + 'VHBPKB'
037600971021     C                   ELSE
037700971021     C                   MOVEL     WI09          VABPKB
037800010719     C                   IF        WD09 = *blanks
037900010719     C                   EVAL      WD09 = *zeros
038000010719     C                   ENDIF
038100971023     C                   TESTN                   WD09                 30
038200971023     C  N30              TESTN                   WD09                     30
038300971021     C                   MOVE      WD09          W001A             1
038400971021     C     *IN30         IFEQ      *OFF
038500971021     C     W001A         ORLT      '0'
038600971023     C     W001A         ANDNE     *BLANK
038700971021     C                   SETON                                        32
038800971021     C                   MOVE      *ZERO         VABPKB
038900010604     C                   eval      vinmsg = %trimr(vinmsg)
039000010604     C                             + ' ' + 'VHBPKB'
039100971021     C                   ELSE
039200971021     C                   MOVE      WD09          VABPKB
039300971021     C                   ENDIF
039400050118     C                   ENDIF
039500971021     C*
039600041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
039700041001     C                   IF        *IN51 = *OFF
039800971021     C                   MOVE      *ZERO         VABVLB
039900971021     C                   MOVE      VHBVLB        WASC07
040000010719     C                   IF        WI07 = *blanks
040100010719     C                   EVAL      WI07 = *zeros
040200010719     C                   ENDIF
040300971023     C                   TESTN                   WI07                 30
040400971023     C  N30              TESTN                   WI07                   30
040500971023     C  N30              TESTN                   WI07                     30
040600971021     C                   MOVE      WI07          W001A             1
040700971021     C     *IN30         IFEQ      *OFF
040800971021     C     W001A         ORLT      '0'
040900971023     C     W001A         ANDNE     *BLANK
041000971021     C                   SETON                                        32
041100971021     C                   MOVE      *ZERO         VABVLB
041200010604     C                   eval      vinmsg = %trimr(vinmsg)
041300010604     C                             + ' ' + 'VHBVLB'
041400971021     C                   ELSE
041500971021     C                   MOVEL     WI07          VABVLB
041600010719     C                   IF        WD07 = *blanks
041700010719     C                   EVAL      WD07 = *zeros
041800010719     C                   ENDIF
041900971023     C                   TESTN                   WD07                 30
042000971023     C  N30              TESTN                   WD07                   30
042100971023     C  N30              TESTN                   WD07                     30
042200971021     C                   MOVE      WD07          W001A             1
042300971021     C     *IN30         IFEQ      *OFF
042400971021     C     W001A         ORLT      '0'
042500971023     C     W001A         ANDNE     *BLANK
042600971021     C                   SETON                                        32
042700971021     C                   MOVE      *ZERO         VABVLB
042800010604     C                   eval      vinmsg = %trimr(vinmsg)
042900010604     C                             + ' ' + 'VHBVLB'
043000971021     C                   ELSE
043100971021     C                   MOVE      WD07          VABVLB
043200971021     C                   ENDIF
043300050118     C                   ENDIF
043400041001     C                   ELSE
043500041001     C* ...altrimenti forzo azzeramento del volume
043600041001     C                   Z-ADD     *zeros        VABVLB
043700041001     C                   ENDIF
043800941229     C*
043900971021     C                   MOVE      *ZERO         VABQFT
044000971021     C                   MOVE      VHBQFT        WASC15
044100010719     C                   IF        WI15 = *blanks
044200010719     C                   EVAL      WI15 = *zeros
044300010719     C                   ENDIF
044400971023     C                   TESTN                   WI15                 30
044500971023     C  N30              TESTN                   WI15                   30
044600971023     C  N30              TESTN                   WI15                     30
044700971021     C                   MOVE      WI15          W001A             1
044800971021     C     *IN30         IFEQ      *OFF
044900971021     C     W001A         ORLT      '0'
045000971023     C     W001A         ANDNE     *BLANK
045100971021     C                   SETON                                        32
045200971021     C                   MOVE      *ZERO         VABQFT
045300010604     C                   eval      vinmsg = %trimr(vinmsg)
045400010604     C                             + ' ' + 'VHBQFT'
045500971021     C                   ELSE
045600971021     C                   MOVEL     WI15          VABQFT
045700010719     C                   IF        WD15 = *blanks
045800010719     C                   EVAL      WD15 = *zeros
045900010719     C                   ENDIF
046000971023     C                   TESTN                   WD15                 30
046100971023     C  N30              TESTN                   WD15                   30
046200971023     C  N30              TESTN                   WD15                     30
046300971021     C                   MOVE      WD15          W001A             1
046400971021     C     *IN30         IFEQ      *OFF
046500971021     C     W001A         ORLT      '0'
046600971023     C     W001A         ANDNE     *BLANK
046700971021     C                   SETON                                        32
046800971021     C                   MOVE      *ZERO         VABQFT
046900010604     C                   eval      vinmsg = %trimr(vinmsg)
047000010604     C                             + ' ' + 'VHBQFT'
047100971021     C                   ELSE
047200971021     C                   MOVE      WD15          VABQFT
047300971021     C                   ENDIF
047400050118     C                   ENDIF
047500941229     C*
047600971021     C                   MOVE      *ZERO         VABCAS
047700971021     C                   MOVE      VHBCAS        WASC15
047800010719     C                   IF        WI15 = *blanks
047900010719     C                   EVAL      WI15 = *zeros
048000010719     C                   ENDIF
048100971023     C                   TESTN                   WI15                 30
048200971023     C  N30              TESTN                   WI15                   30
048300971023     C  N30              TESTN                   WI15                     30
048400971021     C                   MOVE      WI15          W001A             1
048500971021     C     *IN30         IFEQ      *OFF
048600971021     C     W001A         ORLT      '0'
048700971023     C     W001A         ANDNE     *BLANK
048800971021     C                   SETON                                        32
048900971021     C                   MOVE      *ZERO         VABCAS
049000010604     C                   eval      vinmsg = %trimr(vinmsg)
049100010604     C                             + ' ' + 'VHBCAS'
049200971021     C                   ELSE
049300971021     C                   MOVEL     WI15          VABCAS
049400010719     C                   IF        WD15 = *blanks
049500010719     C                   EVAL      WD15 = *zeros
049600010719     C                   ENDIF
049700971023     C                   TESTN                   WD15                 30
049800971023     C  N30              TESTN                   WD15                   30
049900971023     C  N30              TESTN                   WD15                     30
050000971021     C                   MOVE      WD15          W001A             1
050100971021     C     *IN30         IFEQ      *OFF
050200971021     C     W001A         ORLT      '0'
050300971023     C     W001A         ANDNE     *BLANK
050400971021     C                   SETON                                        32
050500971021     C                   MOVE      *ZERO         VABCAS
050600010604     C                   eval      vinmsg = %trimr(vinmsg)
050700010604     C                             + ' ' + 'VHBCAS'
050800971021     C                   ELSE
050900971021     C                   MOVE      WD15          VABCAS
051000971021     C                   ENDIF
051100050118     C                   ENDIF
051200971021     C*
051300971023     C                   TESTN                   VHBRMN               30
051400971023     C  N30              TESTN                   VHBRMN                 30
051500971023     C  N30              TESTN                   VHBRMN                   30
051600971021     C                   MOVE      VHBRMN        W001A             1
051700971021     C     *IN30         IFEQ      *OFF
051800971021     C     W001A         ORLT      '0'
051900971021     C                   SETON                                        32
052000971021     C                   MOVE      *ZERO         VABRMN
052100010604     C                   eval      vinmsg = %trimr(vinmsg)
052200010604     C                             + ' ' + 'VHBRMN'
052300971021     C                   ELSE
052400971021     C                   MOVE      VHBRMN        VABRMN
052500971021     C                   ENDIF
052600971021     C*
052700971023     C                   TESTN                   VHBNCD               30
052800971023     C  N30              TESTN                   VHBNCD                 30
052900971023     C  N30              TESTN                   VHBNCD                   30
053000971021     C                   MOVE      VHBNCD        W001A             1
053100971021     C     *IN30         IFEQ      *OFF
053200971021     C     W001A         ORLT      '0'
053300971023     C     W001A         ANDNE     *BLANK
053400971021     C                   SETON                                        32
053500971021     C                   MOVE      *ZERO         VABNCD
053600010604     C                   eval      vinmsg = %trimr(vinmsg)
053700010604     C                             + ' ' + 'VHBNCD'
053800971021     C                   ELSE
053900971021     C                   MOVE      VHBNCD        VABNCD
054000971021     C                   ENDIF
054100971021     C*
054200971023     C                   TESTN                   VHBNCA               30
054300971023     C  N30              TESTN                   VHBNCA                 30
054400971023     C  N30              TESTN                   VHBNCA                   30
054500971021     C                   MOVE      VHBNCA        W001A             1
054600971021     C     *IN30         IFEQ      *OFF
054700971021     C     W001A         ORLT      '0'
054800971023     C     W001A         ANDNE     *BLANK
054900971021     C                   SETON                                        32
055000971021     C                   MOVE      *ZERO         VABNCA
055100010604     C                   eval      vinmsg = %trimr(vinmsg)
055200010604     C                             + ' ' + 'VHBNCA'
055300971021     C                   ELSE
055400971021     C                   MOVE      VHBNCA        VABNCA
055500971021     C                   ENDIF
055600971021     C*
055700971023     C                   TESTN                   VHBZNC               30
055800971023     C  N30              TESTN                   VHBZNC                 30
055900971023     C  N30              TESTN                   VHBZNC                   30
056000971021     C                   MOVE      VHBZNC        W001A             1
056100971021     C     *IN30         IFEQ      *OFF
056200971021     C     W001A         ORLT      '0'
056300971023     C     W001A         ANDNE     *BLANK
056400971021     C                   SETON                                        32
056500971021     C                   MOVE      *ZERO         VABZNC
056600010604     C                   eval      vinmsg = %trimr(vinmsg)
056700010604     C                             + ' ' + 'VHBZNC'
056800971021     C                   ELSE
056900971021     C                   MOVE      VHBZNC        VABZNC
057000971021     C                   ENDIF
057100971021     C*
057200971023     C                   TESTN                   VHBDCR               30
057300971023     C  N30              TESTN                   VHBDCR                 30
057400971023     C  N30              TESTN                   VHBDCR                   30
057500971021     C                   MOVE      VHBDCR        W001A             1
057600971021     C     *IN30         IFEQ      *OFF
057700971021     C     W001A         ORLT      '0'
057800971023     C     W001A         ANDNE     *BLANK
057900971021     C                   SETON                                        32
058000971021     C                   MOVE      *ZERO         VABDCR
058100010604     C                   eval      vinmsg = %trimr(vinmsg)
058200010604     C                             + ' ' + 'VHBDCR'
058300971021     C                   ELSE
058400971021     C                   MOVE      VHBDCR        VABDCR
058500971021     C                   ENDIF
058600971021     C*
058700971023     C                   TESTN                   VHBHCR               30
058800971023     C  N30              TESTN                   VHBHCR                 30
058900971023     C  N30              TESTN                   VHBHCR                   30
059000971021     C                   MOVE      VHBHCR        W001A             1
059100971021     C     *IN30         IFEQ      *OFF
059200971021     C     W001A         ORLT      '0'
059300971023     C     W001A         ANDNE     *BLANK
059400971021     C                   SETON                                        32
059500971021     C                   MOVE      *ZERO         VABHCR
059600010604     C                   eval      vinmsg = %trimr(vinmsg)
059700010604     C                             + ' ' + 'VHBHCR'
059800971021     C                   ELSE
059900971021     C                   MOVE      VHBHCR        VABHCR
060000971021     C                   ENDIF
060100941229     C*
060200971021     C                   MOVE      *ZERO         VABVMD
060300971021     C                   MOVE      VHBVMD        WASC15
060400010719     C                   IF        WI15 = *blanks
060500010719     C                   EVAL      WI15 = *zeros
060600010719     C                   ENDIF
060700971023     C                   TESTN                   WI15                 30
060800971023     C  N30              TESTN                   WI15                   30
060900971023     C  N30              TESTN                   WI15                     30
061000971021     C                   MOVE      WI15          W001A             1
061100971021     C     *IN30         IFEQ      *OFF
061200971021     C     W001A         ORLT      '0'
061300971023     C     W001A         ANDNE     *BLANK
061400971021     C                   SETON                                        32
061500971021     C                   MOVE      *ZERO         VABVMD
061600010604     C                   eval      vinmsg = %trimr(vinmsg)
061700010604     C                             + ' ' + 'VHBVMD'
061800971021     C                   ELSE
061900971021     C                   MOVEL     WI15          VABVMD
062000010719     C                   IF        WD15 = *blanks
062100010719     C                   EVAL      WD15 = *zeros
062200010719     C                   ENDIF
062300971023     C                   TESTN                   WD15                 30
062400971023     C  N30              TESTN                   WD15                   30
062500971023     C  N30              TESTN                   WD15                     30
062600971021     C                   MOVE      WD15          W001A             1
062700971021     C     *IN30         IFEQ      *OFF
062800971021     C     W001A         ORLT      '0'
062900971023     C     W001A         ANDNE     *BLANK
063000971021     C                   SETON                                        32
063100971021     C                   MOVE      *ZERO         VABVMD
063200010604     C                   eval      vinmsg = %trimr(vinmsg)
063300010604     C                             + ' ' + 'VHBVMD'
063400971021     C                   ELSE
063500971021     C                   MOVE      WD15          VABVMD
063600971021     C                   ENDIF
063700050118     C                   ENDIF
063800971021     C*
063900971023     C                   TESTN                   VHBANT               30
064000971023     C  N30              TESTN                   VHBANT                 30
064100971023     C  N30              TESTN                   VHBANT                   30
064200971021     C                   MOVE      VHBANT        W001A             1
064300971021     C     *IN30         IFEQ      *OFF
064400971021     C     W001A         ORLT      '0'
064500971023     C     W001A         ANDNE     *BLANK
064600971021     C                   SETON                                        32
064700971021     C                   MOVE      *ZERO         VABANT
064800010604     C                   eval      vinmsg = %trimr(vinmsg)
064900010604     C                             + ' ' + 'VHBANT'
065000971021     C                   ELSE
065100971021     C                   MOVE      VHBANT        VABANT
065200971021     C                   ENDIF
065300971021     C*
065400971021     C* CAMPI ALFABETICI:
065500971021     C                   MOVE      VHBATB        VABATB
065600971021     C                   MOVE      VHBCBO        VABCBO
065700971021     C                   MOVE      VHBRSD        VABRSD
065800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
065900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
066000020117     C* ==
066100971021     C                   MOVE      VHBRD2        VABRD2
066200971021     C                   MOVE      VHBIND        VABIND
066300971021     C                   MOVE      VHBCAD        VABCAD
066400971021     C                   MOVE      VHBLOD        VABLOD
066500971021     C                   MOVE      VHBPRD        VABPRD
066600971021     C                   MOVE      VHBNZD        VABNZD
066700971021     C                   MOVE      VHBGC1        VABGC1
066800971021     C                   MOVE      VHBGC2        VABGC2
066900971021     C                   MOVE      VHBTSP        VABTSP
067000971021     C                   MOVE      VHBVAS        VABVAS
067100971021     C                   MOVE      VHBNAS        VABNAS
067200971021     C                   MOVE      VHBTIC        VABTIC
067300971021     C                   MOVE      VHBVCA        VABVCA
067400971021     C                   MOVE      VHBGCA        VABGCA
067500971021     C                   MOVE      VHBRMA        VABRMA
067600971021     C                   MOVE      VHBXCO        VABXCO
067700971021     C                   MOVE      VHBNOT        VABNOT
067800971021     C                   MOVE      VHBNT2        VABNT2
067900971021     C                   MOVE      VHBCTM        VABCTM
068000971021     C                   MOVE      VHBFFD        VABFFD
068100971021     C                   MOVE      VHBTCR        VABTCR
068200971021     C                   MOVE      VHBCTS        VABCTS
068300971021     C                   MOVE      VHBFTM        VABFTM
068400971021     C                   MOVE      VHBVAD        VABVAD
068500971021     C                   MOVE      VHBGMA        VABGMA
068600971021     C                   MOVE      VHBGGA        VABGGA
068700971021     C                   MOVE      VHBGVA        VABGVA
068800971021     C                   MOVE      VHBTC1        VABTC1
068900971021     C                   MOVE      VHBTC2        VABTC2
069000971021     C                   MOVE      VHBSCL        VABSCL
069100971021     C                   MOVE      VHBRMO        VABRMO
069200971021     C                   MOVE      VHBCMO        VABCMO
069300971021     C                   MOVE      VHBNMO        VABNMO
069400971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
069500971021     C* NELLA NAZIONE ORIGINALE
069600971021     C                   MOVE      VHBNMO        WA1D              1
069700971021     C                   MOVEL     VHBNMO        WA1S              1
069800971021     C     WA1D          IFNE      *BLANK
069900971021     C     WA1S          ANDEQ     *BLANK
070000971021     C                   MOVE      *BLANK        VABNMO
070100030616     C                   ENDIF
070200030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
070300030710     C                   EVAL      VABCMR = wCMR
070400030616     C                   EVAL      VABDCM = DATCOR
070500030616     C                   EVAL      VABDTS = DATCOR
070600040402     C                   EVAL      VABHMS = ORACOR
070700030616     C                   EVAL      VABCNT = 1
070800130328     C*
070900130328     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
071000130328     C                   IF        %trim(VABNZD) = 'I'   OR
071100130328     C                             %trim(VABNZD) = 'IT'  OR
071200130328     C                             %trim(VABNZD) = 'ITA'
071300130328     C                   EVAL      VABNZD = *blanks
071400130328     C                   ENDIF
071500041025     C*
071600041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
071700041025     C                   IF        *IN52 = *ON
071800041025     C* Reperisco la provincia dal CAP e dalla località
071900041025     C                   IF        VABCAD <> *blanks AND
072000050907     C                             VABPRD  = *blanks AND
072100050907     C                             VABNZD  = *blanks
072200041025     C                   CLEAR                   TISI95DS
072300041025     C                   EVAL      I95TCN = '3'
072400041025     C                   Z-ADD     datcor        I95DAT
072500041025     C                   EVAL      I95CAP = VABCAD
072600041025     C                   EVAL      I95LOC = VABLOD
072700041025     C                   CALL      'TISI95R'
072800041025     C                   PARM                    TISI95DS
072900041025     C                   EVAL      VABPRD = O95PRV
073000041025     C                   ENDIF
073100041025     C                   ENDIF
073200100419     C*
073300100419     C* Forzatura su campi DIVISE
073400100419     C                   IF        VABVAS = *blanks
073500100419     C                   EVAL      VABVAS = 'EUR'
073600100419     C                   ENDIF
073700100419     C                   IF        VABVCA = *blanks
073800100419     C                   EVAL      VABVCA = 'EUR'
073900100419     C                   ENDIF
074000100419     C                   IF        VABVAD = *blanks
074100100419     C                   EVAL      VABVAD = 'EUR'
074200100419     C                   ENDIF
074300140519     C*
074400140519     C* --- * PERSONALIZZAZIONE PRENATAL * --- *
074500140519     C                   MOVEL     *blanks       wVABRSD          35
074600140529     C                   MOVEL     *blanks       wCOD              4
074700140529     C                   EVAL      wCOD = %subst(VABRSD:1:4)
074800140519     C                   EVAL      VABRSD = %trim(VABRSD)
074900140519     C*
075000140519     C/EXEC SQL
075100140519     C+ declare C1 cursor for
075200140519     C+ select vabrsd
075300140529     C+ from gaitragru/edivabwr
075400140519     C+ where vabccm = :VABCCM and
075500140519     C+ substr(vabrsd, 1, 4) = substr(:VABRSD, 1, 4)
075600140519     C+ for read only
075700140519     C/END-EXEC
075800140519     C
075900140519     C/EXEC SQL
076000140519     C+ open C1
076100140519     C/END-EXEC
076200140519     C
076300140519     C/EXEC SQL
076400140519     C+ Fetch C1 into :wVABRSD
076500140519     C/END-EXEC
076600140519     C*
076700140519     C                   if        sqlcod = 100
076800140519     C                   else
076900140519     C                   EVAL      VABRSD = %trim(wVABRSD)
077000140519     C                   endif
077100140519     C*
077200140519     C/EXEC SQL
077300140519     C+ close C1
077400140519     C/END-EXEC
077500140529     C*
077600140529     C/EXEC SQL
077700140529     C+ declare C2 cursor for
077800140529     C+ select vabrsd
077900140529     C+ from filtragru/edivab0f
078000140529     C+ where vabccm = :VABCCM and
078100140529     C+ substr(vabrsd, 1, 4) = substr(:VABRSD, 1, 4)
078200140529     C+ for read only
078300140529     C/END-EXEC
078400140529     C
078500140529     C/EXEC SQL
078600140529     C+ open C2
078700140529     C/END-EXEC
078800140529     C
078900140529     C/EXEC SQL
079000140529     C+ Fetch C2 into :wVABRSD
079100140529     C/END-EXEC
079200140529     C*
079300140529     C                   if        sqlcod = 100
079400140529     C                   else
079500140529     C                   EVAL      VABRSD = %trim(wVABRSD)
079600140529     C                   endif
079700140529     C*
079800140529     C/EXEC SQL
079900140529     C+ close C2
080000140529     C/END-EXEC
080100140519     C*
080200140519     C* -------------------------------------- *
080300930409     C*
080400010604     C  N31              ADD       1             §CTROK            7 0
080500010604     C   32              ADD       1             §CTRMO            7 0
080600010604     C   31              ADD       1             §CTRNO            7 0
080700030616     C  N31              WRITE     EDIVAB00
080800990910     C*
080900010601     C                   if        *in31 = *off and
081000010601     C                             *in32 = *off
081100990910     C                   eval      vinflg = '1'
081200990910     C                   else
081300990910     C                   eval      vinflg = '2'
081400990910     C                   endif
081500991022     C                   endif
081600990910     C                   endif
081700010601     C*
081800010601     C                   endif
081900000905     C*
082000000905     C                   else
082100000905     C                   eval      vinflg = '1'
082200000905     C                   endif
082300000905     C*
082400000905     C  N70              update    tivin000
082500991022     C*
082600991022     C  N70              ENDdo
082700010530     C
082800010530     C*
082900010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
083000010530     C                   if        cntNonEl = *zeros or
083100010530     C                             flgMulti = '0'
083200990910     C* Se non ci sono record con errori ...
083300010530     C                   if        §ctrno = 0 and
083400010601     C                             §ctrmo = 0 and
083500010530     C                             flgStato <> '2'
083600990910     C* ... restituisco esito OK.
083700990921     C                   eval      wrkesito = '0'
083800990910     C                   else
083900010601     C                   if        §ctrok > 0
084000990921     C                   eval      wrkesito = '1'
084100000710     C                   else
084200010615     C                   if        flgOk = '0'
084300010615     C                   eval      wrkesito = '2'
084400010615     C                   else
084500010615     C                   eval      wrkesito = '6'
084600010615     C                   endif
084700990910     C                   endif
084800000710     C                   endif
084900010530     C                   else
085000010530     C                   eval      wrkesito = '9'
085100010530     C                   endif
085200030710     C*
085300030710 X  1C                   else
085400030710     C                   eval      wrkesito = '8'
085500030710     C                   eval      flgGiro = '1'
085600030710 E  1C                   endif
085700030710     C*
085800990914     C                   if        %open(tivin00r)
085900990908     C                   close     tivin00r
086000990914     C                   endif
086100030616     C                   if        %open(edivabwr)
086200030616     C                   close     edivabwr
086300990914     C                   endif
086400010530     C*
086500010530     C                   if        vlrpoi <> 999
086600010530     C                   eval      invfil = vlrpoi
086700010530     C                   endif
086800990910     C*
086900990920     C                   if        §ctrok > 0
087000010530     C                             and invfil > *zeros
087100000613     C                   exsr      invio
087200990920     C                   endif
087300010612     C*
087400010612     C                   if        flgGiro = '1'
087500010612     C                   exsr      endela
087600010612     C                   endif
087700990920     C*
087800910830     C                   ENDSR
087900000613     C***
088000990920
088100010530
088200010530
088300010530
088400010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
088500010530     C     repfil        BEGSR
088600010530     C*
088700010530     C                   if        invfil = *zeros and
088800010530     C                             depfil > *zeros and
088900010613     C                             (vinflg = *blanks or
089000010613     C                              vinflg = *zeros)
089100010530     C
089200010530     C                   eval      invfil = depfil
089300010530     C                   endif
089400010530     C*
089500010530     C                   if        depfil <> invfil and
089600010530     C                             invfil > *zeros
089700010530     C                   eval      flgMulti = '1'
089800010530     C                   if        vinflg = *blanks
089900010530     C                   add       1             cntNonEl
090000010530     C                   endif
090100010530     C                   endif
090200010530     C*
090300010530     C                   if        vinflg = '2'
090400010530     C                   eval      flgStato = '2'
090500010530     C                   endif
090600010530     C*
090700010530     C                   ENDSR
090800010530     C***
090900010530
091000010530
091100010530
091200010530
091300990920      /TITLE Invio dei dati al punto operativo.
091400000613     C     invio         BEGSR
091500990920     C*
091600990920     C                   reset                   dscmz
091700030618     C                   move      invfil        cmzdst
091800030618     C***                move      '789'         cmzdst
091900990920     C                   eval      cmzfld = vlrfou
092000990920     C                   eval      cmzmbd = vlrhdl
092100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
092200000710     C                   if        prmfir = *blanks
092300030616     C                   eval      cmzfla = 'EDIVAB0F'
092400030616     C                   eval      cmzmba = 'EDIVAB0F'
092500000710     C                   else
092600000710     C                   eval      cmzfla = prmfir
092700000710     C                   eval      cmzmba = prmfir
092800000710     C                   endif
092900990920     C                   eval      cmznrr = *zeros
093000990920     C                   move      §ctrok        cmznrr
093100021003     C                   eval      cmzlba = vlrfl1
093200990920     C                   call(e)   'TIS711C'
093300990920     C                   parm                    dscmz
093400990921     C                   parm      *blanks       esito
093500990920     C                   if        %error
093600990920     C                             or cmzerr = '1'
093700990921     C                             or esito  = '1'
093800000710     C                   eval      wrkesito = '3'
093900990920     C                   endif
094000990920     C*
094100000613     C                   ENDSR
094200000613     C***
094300010530
094400010530
094500010530
094600010530
094700010530      /TITLE Invio dei dati al punto operativo.
094800010530     C     opeini        BEGSR
094900010530     C*
095000010530     C* Inizializzo flag e contatori operativi
095100010530     C                   movel     '0'           flgGiro           1
095200010530     C                   movel     '0'           flgMulti          1
095300010530     C                   movel     '1'           flgStato          1
095400010615     C                   movel     '0'           flgOk             1
095500010530     C                   z-add     *zeros        cntNonEl         10 0
095600010530     C                   z-add     *zeros        depfil            3 0
095700010530     C                   z-add     *zeros        invfil            3 0
095800010530     C*
095900010530     C                   ENDSR
096000010530     C***
096100010530
096200010530
096300010530
096400990910
096500000613     C     *inzsr        BEGSR
096600990910     C*
096700990910     C     *entry        plist
096800990920     C                   parm                    tivlrds
096900990921     C                   parm      wrkesito      esito
097000000724     C                   parm                    prmlit
097100000710     C                   parm                    prmfir
097200030616     C*
097300030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
097400030710     C                   move(p)   vlrMSG        wNomeFile
097500030710     C*
097600030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
097700030710     C* del codice CMR dal nome del file importato.
097800030710     C                   if        wNomeFile <> *blanks AND
097900030710     C                             vlrppt<>*blanks
098000041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
098100030710     C                   if        %subst(vlrppt:1:1) = 'S'
098200030710     C                   seton                                        50
098300030710     C                   else
098400030710     C                   setoff                                       50
098500030710     C                   endif
098600041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
098700041001     C                   if        %subst(vlrppt:6:1) = 'S'
098800041001     C                   seton                                        51
098900041001     C                   else
099000041001     C                   setoff                                       51
099100041001     C                   endif
099200041025     C*
099300041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
099400041025     C* (naturalmente solo se nn già presente)
099500041025     C                   if        %subst(vlrppt:7:1) = 'S'
099600041025     C                   seton                                        52
099700041025     C                   else
099800041025     C                   setoff                                       52
099900041025     C                   endif
100000041001     C*
100100030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
100200030710     C                   move(p)   wPosDaA       wPosDa
100300030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
100400030710     C                   move(p)   wLungA        wLung
100500030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
100600030710     C                   seton                                        60
100700030710     C                   else
100800050914     C                   if        vlrisv = 'UL'
100900050914     C                   movel     datcor        wDataOraAlfa
101000050914     C                   move      oracor        wDataOraAlfa
101100050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
101200050914     C                   seton                                        60
101300050914     C                   else
101400050914     C                   setoff                                       60
101500050914     C                   endif
101600030710     C                   endif
101700010606     C*
101800010606     C* CALCOLA LA DATA CORRENTE
101900140521     C                   time                    wn14             14 0
102000140521     C                   movel     wn14          oracor            6 0          *ORA
102100140521     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
102200140521     C                   eval      datcor = %dec(%date() : *ISO)
102300000613     C*
102400000613     C                   ENDSR
102500000613     C***
