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
003600130318     D edivabds      e ds                  extname(edivab0f)
003700990910     D esito           s              1
003800000724     D prmlit          s             10
003900000710     D prmfir          s             10
004000990921     D wrkesito        s                   like(esito)
004100000613     D rrnum           s              6  0 INZ(*zeros)
004200030616     D wNomeFile       s             30    INZ(*blanks)
004300030710     D wCMR            s             35    INZ(*blanks)
004400030710     D wPosDaA         s              2    INZ(*blanks)
004500030710     D wPosDa          s              2  0 INZ(*zeros)
004600030710     D wLungA          s              2    INZ(*blanks)
004700030710     D wLung           s              2  0 INZ(*zeros)
004800050914     D wDataOraAlfa    s             14    INZ(*blanks)
004900071018     D*-------------------
005000071018     D* COSTANTI
005100071018     D*-------------------
005200071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005300071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005400990908
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800010530     C*
005900010530     C                   exsr      opeini
006000010530     C                   exsr      rwvab
006100041025     C*
006200041025     C* Esegue lancio TISI95R solo x chiusura
006300041025     C                   CLEAR                   TISI95DS
006400041025     C                   EVAL      I95TLA = 'C'
006500041025     C                   CALL      'TISI95R'
006600041025     C                   PARM                    TISI95DS
006700010530     C*
006800010530     C                   seton                                        lr
006900010530
007000010530
007100010530
007200010530
007300010530     C*--------------------------------------------------------
007400010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007500010530     C*--------------------------------------------------------
007600010530     C     PREELA        BEGSR
007700000724     C*
007800000724     C* SE OCCORRE SPEDIRE IN FILIALE
007900010530     C                   if        invfil <> *zeros and
008000010530     C                             flgGiro = '0'
008100010530     C*
008200010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008300010530     C                   eval      flgGiro = '1'
008400010530     C*
008500010530     C                   endif
008600010530     C*
008700010530     C                   ENDSR
008800010530     C***
008900010530
009000010530
009100010530
009200010530
009300010530
009400010530
009500010530     C*--------------------------------------------------------
009600010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009700010530     C*--------------------------------------------------------
009800010530     C     ENDELA        BEGSR
009900010530     C*
010000010530     C                   ENDSR
010100010530     C***
010200990908
010300010530
010400010530
010500010530
010600910830     C*--------------------------------------------------------
010700030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
010800910830     C*--------------------------------------------------------
010900941229     C     RWVAB         BEGSR
011000030710     C*
011100030710     C                   clear                   §CTROK
011200030710     C                   clear                   §CTRMO
011300030710     C                   clear                   §CTRNO
011400990910     C*
011500030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
011600030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
011700030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
011800030710     C                   if        *in60 = *on
011900990914     C                   if        not %open(tivin00r)
012000990908     C                   open      tivin00r
012100990914     C                   endif
012200030616     C                   if        not %open(edivabwr)
012300030616     C                   open      edivabwr
012400990914     C                   endif
012500130318     C*
012600130318     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
012700130318     C                   if        *in53
012800130318     C                   call      'TIS7P2SER'
012900130318     C                   parm      'O'           pIn_Opz           1
013000130318     C                   parm                    tivlrds
013100130318     C                   parm                    EDIVABDS
013200130318     C                   parm      *blanks       pIn_CdIdAz        2
013300130318     C                   parm      *blanks       pIn_MaskPDF      50
013400130318     C                   parm      *blanks       pOut_Esito        1
013500130318     C                   endif
013600990910     C*
013700921023     C                   DO        *HIVAL
013800990913     C*
013900990915     C                   READ      tivin00r                               70
014000071018     C*
014100071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
014200071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
014300010618     C*
014400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014500010618     C                   if        vinflg = '1'
014600010618     C                   eval      flgOk = '1'
014700010618     C                   endif
014800010705     C*
014900010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
015000010705     C                   if        %subst(vindta:1:1) = '='
015100010705     C                   eval      %subst(vindta:1:1) = *blanks
015200010705     C                   endif
015300010705     C*
015400000905     C                   if        vindta > *blanks
015500000613     C                   add       1             rrnum
015600010601     C* ...elimino eventuali segni
015700010601     C     '+':' '       xlate     vindta        vindta
015800990913     C*
015900010601     C                   if        *in70 = *off and
016000990913     C                             (vinflg = *blanks
016100010711     C                              or vinflg = '0'
016200010711     C                              or vinflg = '2')
016300990913     C*
016400030616     C                   clear                   EDIVAB00
016500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016600010711     C                   if        vinflg = *blanks or vinflg = '0'
016700010711     C                   clear                   vinmsg
016800010711     C                   endif
016900010711     C*
017000990913     C                   eval      fnvabaar = vindta
017100991022     C*
017200991022     C                   testb     x'3F'         vhbatb                   02
017300991022     C                   if        not *in02
017400991022     C*
017500971021     C                   SETOFF                                       303132
017600971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
017700971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
017800971023     C*    OPPURE TUTTI BLANKS
017900010530     C*
018000010530     C                   TESTN                   VHBLNP               30
018100010530     C  N30              TESTN                   VHBLNP                 30
018200010530     C  N30              TESTN                   VHBLNP                   30
018300010530     C                   MOVE      VHBLNP        W001A             1
018400010530     C     *IN30         IFEQ      *OFF
018500010530     C     W001A         ORLT      '0'
018600010530     C                   SETON                                        31
018700010530     C                   eval      vinmsg = %trimr(vinmsg)
018800010530     C                             + ' ' + 'VHBLNP'
018900010530     C                   ELSE
019000010530     C                   MOVE      VHBLNP        VABLNP
019100010530     C     VABLNP        IFLE      *ZEROS
019200010530     C                   SETON                                        31
019300010530     C                   eval      vinmsg = %trimr(vinmsg)
019400010530     C                             + ' ' + 'VHBLNP'
019500010530     C                   ENDIF
019600010530     C                   ENDIF
019700010530     C*
019800010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019900010530     C                   eval      depfil = VABLNP
020000010530     C                   exsr      repfil
020100010601     C                   if        depfil = invfil
020200021014     C                   if        vlrpoi = 999
020300021010     C                   MOVE(P)   invfil        VABFGS
020400021014     C                   else
020500021014     C                   MOVE(P)   vlrpoi        VABFGS
020600021014     C                   endif
020700010530     C*
020800010530     C                   exsr      PREELA
020900010530     C*
021000971023     C                   TESTN                   VHBCCM               30
021100971023     C  N30              TESTN                   VHBCCM                 30
021200971023     C  N30              TESTN                   VHBCCM                   30
021300971021     C                   MOVE      VHBCCM        W001A             1
021400971021     C     *IN30         IFEQ      *OFF
021500971021     C     W001A         ORLT      '0'
021600971021     C                   SETON                                        31
021700010604     C                   eval      vinmsg = %trimr(vinmsg)
021800010604     C                             + ' ' + 'VHBCCM'
021900971021     C                   ELSE
022000971021     C                   MOVE      VHBCCM        VABCCM
022100971024     C     VABCCM        IFLE      *ZEROS
022200971024     C                   SETON                                        31
022300010604     C                   eval      vinmsg = %trimr(vinmsg)
022400010604     C                             + ' ' + 'VHBCCM'
022500971024     C                   ENDIF
022600971021     C                   ENDIF
022700971021     C*
022800971023     C                   TESTN                   VHBAAS               30
022900971023     C  N30              TESTN                   VHBAAS                 30
023000971023     C  N30              TESTN                   VHBAAS                   30
023100971021     C                   MOVE      VHBAAS        W001A             1
023200971021     C     *IN30         IFEQ      *OFF
023300971021     C     W001A         ORLT      '0'
023400971023     C     W001A         ANDNE     *BLANK
023500971021     C                   SETON                                        32
023600010606     C                   MOVEL     datcor        VABAAS
023700010604     C                   eval      vinmsg = %trimr(vinmsg)
023800010604     C                             + ' ' + 'VHBAAS'
023900971021     C                   ELSE
024000971021     C                   MOVE      VHBAAS        VABAAS
024100971021     C                   ENDIF
024200971021     C*
024300971023     C                   TESTN                   VHBMGS               30
024400971023     C  N30              TESTN                   VHBMGS                 30
024500971023     C  N30              TESTN                   VHBMGS                   30
024600971021     C                   MOVE      VHBMGS        W001A             1
024700971021     C     *IN30         IFEQ      *OFF
024800971021     C     W001A         ORLT      '0'
024900971023     C     W001A         ANDNE     *BLANK
025000971021     C                   SETON                                        32
025100010601     C                   MOVE      '0101'        VABMGS
025200010604     C                   eval      vinmsg = %trimr(vinmsg)
025300010604     C                             + ' ' + 'VHBMGS'
025400971021     C                   ELSE
025500971021     C                   MOVE      VHBMGS        VABMGS
025600971021     C                   ENDIF
025700971021     C*
025800971023     C                   TESTN                   VHBNRS               30
025900971023     C  N30              TESTN                   VHBNRS                 30
026000971023     C  N30              TESTN                   VHBNRS                   30
026100971021     C                   MOVE      VHBNRS        W001A             1
026200971021     C     *IN30         IFEQ      *OFF
026300971021     C     W001A         ORLT      '0'
026400971023     C     W001A         ANDNE     *BLANK
026500971021     C                   SETON                                        32
026600971021     C                   MOVE      *ZERO         VABNRS
026700010604     C                   eval      vinmsg = %trimr(vinmsg)
026800010604     C                             + ' ' + 'VHBNRS'
026900971021     C                   ELSE
027000971021     C                   MOVE      VHBNRS        VABNRS
027100971021     C                   ENDIF
027200971021     C*
027300971023     C                   TESTN                   VHBNSP               30
027400971023     C  N30              TESTN                   VHBNSP                 30
027500971023     C  N30              TESTN                   VHBNSP                   30
027600971021     C                   MOVE      VHBNSP        W001A             1
027700971021     C     *IN30         IFEQ      *OFF
027800971021     C     W001A         ORLT      '0'
027900971023     C     W001A         ANDNE     *BLANK
028000971021     C                   SETON                                        32
028100971021     C                   MOVE      *ZERO         VABNSP
028200010604     C                   eval      vinmsg = %trimr(vinmsg)
028300010604     C                             + ' ' + 'VHBNSP'
028400971021     C                   ELSE
028500971021     C                   MOVE      VHBNSP        VABNSP
028600971021     C                   ENDIF
028700971021     C*
028800971023     C                   TESTN                   VHBLNA               30
028900971023     C  N30              TESTN                   VHBLNA                 30
029000971023     C  N30              TESTN                   VHBLNA                   30
029100971021     C                   MOVE      VHBLNA        W001A             1
029200971021     C     *IN30         IFEQ      *OFF
029300971021     C     W001A         ORLT      '0'
029400971023     C     W001A         ANDNE     *BLANK
029500971021     C                   SETON                                        32
029600971021     C                   MOVE      *ZERO         VABLNA
029700010604     C                   eval      vinmsg = %trimr(vinmsg)
029800010604     C                             + ' ' + 'VHBLNA'
029900971021     C                   ELSE
030000971021     C                   MOVE      VHBLNA        VABLNA
030100971021     C                   ENDIF
030200971021     C*
030300971023     C                   TESTN                   VHBCTR               30
030400971023     C  N30              TESTN                   VHBCTR                 30
030500971023     C  N30              TESTN                   VHBCTR                   30
030600971021     C                   MOVE      VHBCTR        W001A             1
030700971021     C     *IN30         IFEQ      *OFF
030800971021     C     W001A         ORLT      '0'
030900971023     C     W001A         ANDNE     *BLANK
031000971021     C                   SETON                                        32
031100971021     C                   MOVE      *ZERO         VABCTR
031200010604     C                   eval      vinmsg = %trimr(vinmsg)
031300010604     C                             + ' ' + 'VHBCTR'
031400971021     C                   ELSE
031500971021     C                   MOVE      VHBCTR        VABCTR
031600971021     C                   ENDIF
031700941229     C*
031800971021     C                   MOVE      *ZERO         VABIAS
031900941229     C                   MOVE      VHBIAS        WASC15
032000971023     C                   TESTN                   WI15                 30
032100971023     C  N30              TESTN                   WI15                   30
032200971023     C  N30              TESTN                   WI15                     30
032300971021     C                   MOVE      WI15          W001A             1
032400971021     C     *IN30         IFEQ      *OFF
032500971021     C     W001A         ORLT      '0'
032600971023     C     W001A         ANDNE     *BLANK
032700971021     C                   SETON                                        32
032800971021     C                   MOVE      *ZERO         VABIAS
032900010604     C                   eval      vinmsg = %trimr(vinmsg)
033000010604     C                             + ' ' + 'VHBIAS'
033100971021     C                   ELSE
033200971021     C                   MOVEL     WI15          VABIAS
033300971023     C                   TESTN                   WD15                 30
033400971023     C  N30              TESTN                   WD15                   30
033500971023     C  N30              TESTN                   WD15                     30
033600971021     C                   MOVE      WD15          W001A             1
033700971021     C     *IN30         IFEQ      *OFF
033800971021     C     W001A         ORLT      '0'
033900971023     C     W001A         ANDNE     *BLANK
034000971021     C                   SETON                                        32
034100971021     C                   MOVE      *ZERO         VABIAS
034200010604     C                   eval      vinmsg = %trimr(vinmsg)
034300010604     C                             + ' ' + 'VHBIAS'
034400971021     C                   ELSE
034500971021     C                   MOVE      WD15          VABIAS
034600971021     C                   ENDIF
034700050118     C                   ENDIF
034800941229     C*
034900971023     C                   TESTN                   VHBNCL               30
035000971023     C  N30              TESTN                   VHBNCL                 30
035100971023     C  N30              TESTN                   VHBNCL                   30
035200971021     C                   MOVE      VHBNCL        W001A             1
035300971021     C     *IN30         IFEQ      *OFF
035400971021     C     W001A         ORLT      '0'
035500971021     C                   SETON                                        32
035600971021     C                   MOVE      *ZERO         VABNCL
035700010604     C                   eval      vinmsg = %trimr(vinmsg)
035800010604     C                             + ' ' + 'VHBNCL'
035900971021     C                   ELSE
036000971021     C                   MOVE      VHBNCL        VABNCL
036100971021     C                   ENDIF
036200941229     C*
036300971021     C                   MOVE      *ZERO         VABPKB
036400971021     C                   MOVE      VHBPKB        WASC09
036500010719     C                   IF        WI09 = *blanks
036600010719     C                   EVAL      WI09 = *zeros
036700010719     C                   ENDIF
036800971023     C                   TESTN                   WI09                 30
036900971023     C  N30              TESTN                   WI09                   30
037000971023     C  N30              TESTN                   WI09                     30
037100971021     C                   MOVE      WI09          W001A             1
037200971021     C     *IN30         IFEQ      *OFF
037300971021     C     W001A         ORLT      '0'
037400971021     C                   SETON                                        32
037500971021     C                   MOVE      *ZERO         VABPKB
037600010604     C                   eval      vinmsg = %trimr(vinmsg)
037700010604     C                             + ' ' + 'VHBPKB'
037800971021     C                   ELSE
037900971021     C                   MOVEL     WI09          VABPKB
038000010719     C                   IF        WD09 = *blanks
038100010719     C                   EVAL      WD09 = *zeros
038200010719     C                   ENDIF
038300971023     C                   TESTN                   WD09                 30
038400971023     C  N30              TESTN                   WD09                     30
038500971021     C                   MOVE      WD09          W001A             1
038600971021     C     *IN30         IFEQ      *OFF
038700971021     C     W001A         ORLT      '0'
038800971023     C     W001A         ANDNE     *BLANK
038900971021     C                   SETON                                        32
039000971021     C                   MOVE      *ZERO         VABPKB
039100010604     C                   eval      vinmsg = %trimr(vinmsg)
039200010604     C                             + ' ' + 'VHBPKB'
039300971021     C                   ELSE
039400971021     C                   MOVE      WD09          VABPKB
039500971021     C                   ENDIF
039600050118     C                   ENDIF
039700971021     C*
039800041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
039900041001     C                   IF        *IN51 = *OFF
040000971021     C                   MOVE      *ZERO         VABVLB
040100971021     C                   MOVE      VHBVLB        WASC07
040200010719     C                   IF        WI07 = *blanks
040300010719     C                   EVAL      WI07 = *zeros
040400010719     C                   ENDIF
040500971023     C                   TESTN                   WI07                 30
040600971023     C  N30              TESTN                   WI07                   30
040700971023     C  N30              TESTN                   WI07                     30
040800971021     C                   MOVE      WI07          W001A             1
040900971021     C     *IN30         IFEQ      *OFF
041000971021     C     W001A         ORLT      '0'
041100971023     C     W001A         ANDNE     *BLANK
041200971021     C                   SETON                                        32
041300971021     C                   MOVE      *ZERO         VABVLB
041400010604     C                   eval      vinmsg = %trimr(vinmsg)
041500010604     C                             + ' ' + 'VHBVLB'
041600971021     C                   ELSE
041700971021     C                   MOVEL     WI07          VABVLB
041800010719     C                   IF        WD07 = *blanks
041900010719     C                   EVAL      WD07 = *zeros
042000010719     C                   ENDIF
042100971023     C                   TESTN                   WD07                 30
042200971023     C  N30              TESTN                   WD07                   30
042300971023     C  N30              TESTN                   WD07                     30
042400971021     C                   MOVE      WD07          W001A             1
042500971021     C     *IN30         IFEQ      *OFF
042600971021     C     W001A         ORLT      '0'
042700971023     C     W001A         ANDNE     *BLANK
042800971021     C                   SETON                                        32
042900971021     C                   MOVE      *ZERO         VABVLB
043000010604     C                   eval      vinmsg = %trimr(vinmsg)
043100010604     C                             + ' ' + 'VHBVLB'
043200971021     C                   ELSE
043300971021     C                   MOVE      WD07          VABVLB
043400971021     C                   ENDIF
043500050118     C                   ENDIF
043600041001     C                   ELSE
043700041001     C* ...altrimenti forzo azzeramento del volume
043800041001     C                   Z-ADD     *zeros        VABVLB
043900041001     C                   ENDIF
044000941229     C*
044100971021     C                   MOVE      *ZERO         VABQFT
044200971021     C                   MOVE      VHBQFT        WASC15
044300010719     C                   IF        WI15 = *blanks
044400010719     C                   EVAL      WI15 = *zeros
044500010719     C                   ENDIF
044600971023     C                   TESTN                   WI15                 30
044700971023     C  N30              TESTN                   WI15                   30
044800971023     C  N30              TESTN                   WI15                     30
044900971021     C                   MOVE      WI15          W001A             1
045000971021     C     *IN30         IFEQ      *OFF
045100971021     C     W001A         ORLT      '0'
045200971023     C     W001A         ANDNE     *BLANK
045300971021     C                   SETON                                        32
045400971021     C                   MOVE      *ZERO         VABQFT
045500010604     C                   eval      vinmsg = %trimr(vinmsg)
045600010604     C                             + ' ' + 'VHBQFT'
045700971021     C                   ELSE
045800971021     C                   MOVEL     WI15          VABQFT
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
047000971021     C                   MOVE      *ZERO         VABQFT
047100010604     C                   eval      vinmsg = %trimr(vinmsg)
047200010604     C                             + ' ' + 'VHBQFT'
047300971021     C                   ELSE
047400971021     C                   MOVE      WD15          VABQFT
047500971021     C                   ENDIF
047600050118     C                   ENDIF
047700941229     C*
047800971021     C                   MOVE      *ZERO         VABCAS
047900971021     C                   MOVE      VHBCAS        WASC15
048000010719     C                   IF        WI15 = *blanks
048100010719     C                   EVAL      WI15 = *zeros
048200010719     C                   ENDIF
048300971023     C                   TESTN                   WI15                 30
048400971023     C  N30              TESTN                   WI15                   30
048500971023     C  N30              TESTN                   WI15                     30
048600971021     C                   MOVE      WI15          W001A             1
048700971021     C     *IN30         IFEQ      *OFF
048800971021     C     W001A         ORLT      '0'
048900971023     C     W001A         ANDNE     *BLANK
049000971021     C                   SETON                                        32
049100971021     C                   MOVE      *ZERO         VABCAS
049200010604     C                   eval      vinmsg = %trimr(vinmsg)
049300010604     C                             + ' ' + 'VHBCAS'
049400971021     C                   ELSE
049500971021     C                   MOVEL     WI15          VABCAS
049600010719     C                   IF        WD15 = *blanks
049700010719     C                   EVAL      WD15 = *zeros
049800010719     C                   ENDIF
049900971023     C                   TESTN                   WD15                 30
050000971023     C  N30              TESTN                   WD15                   30
050100971023     C  N30              TESTN                   WD15                     30
050200971021     C                   MOVE      WD15          W001A             1
050300971021     C     *IN30         IFEQ      *OFF
050400971021     C     W001A         ORLT      '0'
050500971023     C     W001A         ANDNE     *BLANK
050600971021     C                   SETON                                        32
050700971021     C                   MOVE      *ZERO         VABCAS
050800010604     C                   eval      vinmsg = %trimr(vinmsg)
050900010604     C                             + ' ' + 'VHBCAS'
051000971021     C                   ELSE
051100971021     C                   MOVE      WD15          VABCAS
051200971021     C                   ENDIF
051300050118     C                   ENDIF
051400971021     C*
051500971023     C                   TESTN                   VHBRMN               30
051600971023     C  N30              TESTN                   VHBRMN                 30
051700971023     C  N30              TESTN                   VHBRMN                   30
051800971021     C                   MOVE      VHBRMN        W001A             1
051900971021     C     *IN30         IFEQ      *OFF
052000971021     C     W001A         ORLT      '0'
052100971021     C                   SETON                                        32
052200971021     C                   MOVE      *ZERO         VABRMN
052300010604     C                   eval      vinmsg = %trimr(vinmsg)
052400010604     C                             + ' ' + 'VHBRMN'
052500971021     C                   ELSE
052600971021     C                   MOVE      VHBRMN        VABRMN
052700971021     C                   ENDIF
052800971021     C*
052900971023     C                   TESTN                   VHBNCD               30
053000971023     C  N30              TESTN                   VHBNCD                 30
053100971023     C  N30              TESTN                   VHBNCD                   30
053200971021     C                   MOVE      VHBNCD        W001A             1
053300971021     C     *IN30         IFEQ      *OFF
053400971021     C     W001A         ORLT      '0'
053500971023     C     W001A         ANDNE     *BLANK
053600971021     C                   SETON                                        32
053700971021     C                   MOVE      *ZERO         VABNCD
053800010604     C                   eval      vinmsg = %trimr(vinmsg)
053900010604     C                             + ' ' + 'VHBNCD'
054000971021     C                   ELSE
054100971021     C                   MOVE      VHBNCD        VABNCD
054200971021     C                   ENDIF
054300971021     C*
054400971023     C                   TESTN                   VHBNCA               30
054500971023     C  N30              TESTN                   VHBNCA                 30
054600971023     C  N30              TESTN                   VHBNCA                   30
054700971021     C                   MOVE      VHBNCA        W001A             1
054800971021     C     *IN30         IFEQ      *OFF
054900971021     C     W001A         ORLT      '0'
055000971023     C     W001A         ANDNE     *BLANK
055100971021     C                   SETON                                        32
055200971021     C                   MOVE      *ZERO         VABNCA
055300010604     C                   eval      vinmsg = %trimr(vinmsg)
055400010604     C                             + ' ' + 'VHBNCA'
055500971021     C                   ELSE
055600971021     C                   MOVE      VHBNCA        VABNCA
055700971021     C                   ENDIF
055800971021     C*
055900971023     C                   TESTN                   VHBZNC               30
056000971023     C  N30              TESTN                   VHBZNC                 30
056100971023     C  N30              TESTN                   VHBZNC                   30
056200971021     C                   MOVE      VHBZNC        W001A             1
056300971021     C     *IN30         IFEQ      *OFF
056400971021     C     W001A         ORLT      '0'
056500971023     C     W001A         ANDNE     *BLANK
056600971021     C                   SETON                                        32
056700971021     C                   MOVE      *ZERO         VABZNC
056800010604     C                   eval      vinmsg = %trimr(vinmsg)
056900010604     C                             + ' ' + 'VHBZNC'
057000971021     C                   ELSE
057100971021     C                   MOVE      VHBZNC        VABZNC
057200971021     C                   ENDIF
057300971021     C*
057400971023     C                   TESTN                   VHBDCR               30
057500971023     C  N30              TESTN                   VHBDCR                 30
057600971023     C  N30              TESTN                   VHBDCR                   30
057700971021     C                   MOVE      VHBDCR        W001A             1
057800971021     C     *IN30         IFEQ      *OFF
057900971021     C     W001A         ORLT      '0'
058000971023     C     W001A         ANDNE     *BLANK
058100971021     C                   SETON                                        32
058200971021     C                   MOVE      *ZERO         VABDCR
058300010604     C                   eval      vinmsg = %trimr(vinmsg)
058400010604     C                             + ' ' + 'VHBDCR'
058500971021     C                   ELSE
058600971021     C                   MOVE      VHBDCR        VABDCR
058700971021     C                   ENDIF
058800971021     C*
058900971023     C                   TESTN                   VHBHCR               30
059000971023     C  N30              TESTN                   VHBHCR                 30
059100971023     C  N30              TESTN                   VHBHCR                   30
059200971021     C                   MOVE      VHBHCR        W001A             1
059300971021     C     *IN30         IFEQ      *OFF
059400971021     C     W001A         ORLT      '0'
059500971023     C     W001A         ANDNE     *BLANK
059600971021     C                   SETON                                        32
059700971021     C                   MOVE      *ZERO         VABHCR
059800010604     C                   eval      vinmsg = %trimr(vinmsg)
059900010604     C                             + ' ' + 'VHBHCR'
060000971021     C                   ELSE
060100971021     C                   MOVE      VHBHCR        VABHCR
060200971021     C                   ENDIF
060300941229     C*
060400971021     C                   MOVE      *ZERO         VABVMD
060500971021     C                   MOVE      VHBVMD        WASC15
060600010719     C                   IF        WI15 = *blanks
060700010719     C                   EVAL      WI15 = *zeros
060800010719     C                   ENDIF
060900971023     C                   TESTN                   WI15                 30
061000971023     C  N30              TESTN                   WI15                   30
061100971023     C  N30              TESTN                   WI15                     30
061200971021     C                   MOVE      WI15          W001A             1
061300971021     C     *IN30         IFEQ      *OFF
061400971021     C     W001A         ORLT      '0'
061500971023     C     W001A         ANDNE     *BLANK
061600971021     C                   SETON                                        32
061700971021     C                   MOVE      *ZERO         VABVMD
061800010604     C                   eval      vinmsg = %trimr(vinmsg)
061900010604     C                             + ' ' + 'VHBVMD'
062000971021     C                   ELSE
062100971021     C                   MOVEL     WI15          VABVMD
062200010719     C                   IF        WD15 = *blanks
062300010719     C                   EVAL      WD15 = *zeros
062400010719     C                   ENDIF
062500971023     C                   TESTN                   WD15                 30
062600971023     C  N30              TESTN                   WD15                   30
062700971023     C  N30              TESTN                   WD15                     30
062800971021     C                   MOVE      WD15          W001A             1
062900971021     C     *IN30         IFEQ      *OFF
063000971021     C     W001A         ORLT      '0'
063100971023     C     W001A         ANDNE     *BLANK
063200971021     C                   SETON                                        32
063300971021     C                   MOVE      *ZERO         VABVMD
063400010604     C                   eval      vinmsg = %trimr(vinmsg)
063500010604     C                             + ' ' + 'VHBVMD'
063600971021     C                   ELSE
063700971021     C                   MOVE      WD15          VABVMD
063800971021     C                   ENDIF
063900050118     C                   ENDIF
064000971021     C*
064100971023     C                   TESTN                   VHBANT               30
064200971023     C  N30              TESTN                   VHBANT                 30
064300971023     C  N30              TESTN                   VHBANT                   30
064400971021     C                   MOVE      VHBANT        W001A             1
064500971021     C     *IN30         IFEQ      *OFF
064600971021     C     W001A         ORLT      '0'
064700971023     C     W001A         ANDNE     *BLANK
064800971021     C                   SETON                                        32
064900971021     C                   MOVE      *ZERO         VABANT
065000010604     C                   eval      vinmsg = %trimr(vinmsg)
065100010604     C                             + ' ' + 'VHBANT'
065200971021     C                   ELSE
065300971021     C                   MOVE      VHBANT        VABANT
065400971021     C                   ENDIF
065500971021     C*
065600971021     C* CAMPI ALFABETICI:
065700971021     C                   MOVE      VHBATB        VABATB
065800971021     C                   MOVE      VHBCBO        VABCBO
065900971021     C                   MOVE      VHBRSD        VABRSD
066000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
066100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
066200020117     C* ==
066300971021     C                   MOVE      VHBRD2        VABRD2
066400971021     C                   MOVE      VHBIND        VABIND
066500971021     C                   MOVE      VHBCAD        VABCAD
066600971021     C                   MOVE      VHBLOD        VABLOD
066700971021     C                   MOVE      VHBPRD        VABPRD
066800971021     C                   MOVE      VHBNZD        VABNZD
066900971021     C                   MOVE      VHBGC1        VABGC1
067000971021     C                   MOVE      VHBGC2        VABGC2
067100971021     C                   MOVE      VHBTSP        VABTSP
067200971021     C                   MOVE      VHBVAS        VABVAS
067300971021     C                   MOVE      VHBNAS        VABNAS
067400971021     C                   MOVE      VHBTIC        VABTIC
067500971021     C                   MOVE      VHBVCA        VABVCA
067600971021     C                   MOVE      VHBGCA        VABGCA
067700971021     C                   MOVE      VHBRMA        VABRMA
067800971021     C                   MOVE      VHBXCO        VABXCO
067900971021     C                   MOVE      VHBNOT        VABNOT
068000971021     C                   MOVE      VHBNT2        VABNT2
068100971021     C                   MOVE      VHBCTM        VABCTM
068200971021     C                   MOVE      VHBFFD        VABFFD
068300971021     C                   MOVE      VHBTCR        VABTCR
068400971021     C                   MOVE      VHBCTS        VABCTS
068500971021     C                   MOVE      VHBFTM        VABFTM
068600971021     C                   MOVE      VHBVAD        VABVAD
068700971021     C                   MOVE      VHBGMA        VABGMA
068800971021     C                   MOVE      VHBGGA        VABGGA
068900971021     C                   MOVE      VHBGVA        VABGVA
069000971021     C                   MOVE      VHBTC1        VABTC1
069100971021     C                   MOVE      VHBTC2        VABTC2
069200971021     C                   MOVE      VHBSCL        VABSCL
069300971021     C                   MOVE      VHBRMO        VABRMO
069400971021     C                   MOVE      VHBCMO        VABCMO
069500971021     C                   MOVE      VHBNMO        VABNMO
069600971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
069700971021     C* NELLA NAZIONE ORIGINALE
069800971021     C                   MOVE      VHBNMO        WA1D              1
069900971021     C                   MOVEL     VHBNMO        WA1S              1
070000971021     C     WA1D          IFNE      *BLANK
070100971021     C     WA1S          ANDEQ     *BLANK
070200971021     C                   MOVE      *BLANK        VABNMO
070300030616     C                   ENDIF
070400030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
070500030710     C                   EVAL      VABCMR = wCMR
070600030616     C                   EVAL      VABDCM = DATCOR
070700030616     C                   EVAL      VABDTS = DATCOR
070800040402     C                   EVAL      VABHMS = ORACOR
070900030616     C                   EVAL      VABCNT = 1
071000041025     C*
071100041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
071200041025     C                   IF        *IN52 = *ON
071300041025     C* Reperisco la provincia dal CAP e dalla località
071400041025     C                   IF        VABCAD <> *blanks AND
071500050907     C                             VABPRD  = *blanks AND
071600050907     C                             VABNZD  = *blanks
071700041025     C                   CLEAR                   TISI95DS
071800041025     C                   EVAL      I95TCN = '3'
071900041025     C                   Z-ADD     datcor        I95DAT
072000041025     C                   EVAL      I95CAP = VABCAD
072100041025     C                   EVAL      I95LOC = VABLOD
072200041025     C                   CALL      'TISI95R'
072300041025     C                   PARM                    TISI95DS
072400041025     C                   EVAL      VABPRD = O95PRV
072500041025     C                   ENDIF
072600041025     C                   ENDIF
072700100419     C*
072800100419     C* Forzatura su campi DIVISE
072900100419     C                   IF        VABVAS = *blanks
073000100419     C                   EVAL      VABVAS = 'EUR'
073100100419     C                   ENDIF
073200100419     C                   IF        VABVCA = *blanks
073300100419     C                   EVAL      VABVCA = 'EUR'
073400100419     C                   ENDIF
073500100419     C                   IF        VABVAD = *blanks
073600100419     C                   EVAL      VABVAD = 'EUR'
073700100419     C                   ENDIF
073800130401     C*
073900130401     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
074000130401     C                   if        *in53
074100130401     C                   if        not *in31
074200130401     C*
074300130401     C                   eval      pIn_MaskPDF = 'BL_&0RMN*_&RMA*.pdf'
074400130401     C                   call      'TIS7P2SER'
074500130401     C                   parm      'W'           pIn_Opz           1
074600130401     C                   parm                    tivlrds
074700130401     C                   parm                    EDIVABDS
074800130401     C                   parm      '10'          pIn_CdIdAz        2
074900130401     C                   parm                    pIn_MaskPDF      50
075000130401     C                   parm      *blanks       pOut_Esito        1
075100130401     C*
075200130401     C* Forzature:
075300130401     C*
075400130401     C* CTM
075500130401     C                   if        wCTM <> *blanks
075600130401     C                   eval      VABCTM = wCTM
075700130401     C                   endif
075800130401     C*
075900130401     C                   endif
076000130401     C                   endif
076100930409     C*
076200010604     C  N31              ADD       1             §CTROK            7 0
076300010604     C   32              ADD       1             §CTRMO            7 0
076400010604     C   31              ADD       1             §CTRNO            7 0
076500030616     C  N31              WRITE     EDIVAB00
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
081000030710     C*
081100030710 X  1C                   else
081200030710     C                   eval      wrkesito = '8'
081300030710     C                   eval      flgGiro = '1'
081400030710 E  1C                   endif
081500030710     C*
081600990914     C                   if        %open(tivin00r)
081700990908     C                   close     tivin00r
081800990914     C                   endif
081900030616     C                   if        %open(edivabwr)
082000030616     C                   close     edivabwr
082100990914     C                   endif
082200010530     C*
082300010530     C                   if        vlrpoi <> 999
082400010530     C                   eval      invfil = vlrpoi
082500010530     C                   endif
082600990910     C*
082700990920     C                   if        §ctrok > 0
082800010530     C                             and invfil > *zeros
082900000613     C                   exsr      invio
083000990920     C                   endif
083100170731     C*
083200170731     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
083300170731     C                   if        *in53
083400170731     C                   call      'TIS7P2SER'
083500170731     C                   parm      'C'           pIn_Opz           1
083600170731     C                   parm                    tivlrds
083700170731     C                   parm                    EDIVABDS
083800170731     C                   parm      *blanks       pIn_CdIdAz        2
083900170731     C                   parm      *blanks       pIn_MaskPDF      50
084000170731     C                   parm      *blanks       pOut_Esito        1
084100170731     C                   endif
084200010612     C*
084300010612     C                   if        flgGiro = '1'
084400010612     C                   exsr      endela
084500010612     C                   endif
084600990920     C*
084700910830     C                   ENDSR
084800000613     C***
084900990920
085000010530
085100010530
085200010530
085300010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
085400010530     C     repfil        BEGSR
085500010530     C*
085600010530     C                   if        invfil = *zeros and
085700010530     C                             depfil > *zeros and
085800010613     C                             (vinflg = *blanks or
085900010613     C                              vinflg = *zeros)
086000010530     C
086100010530     C                   eval      invfil = depfil
086200010530     C                   endif
086300010530     C*
086400010530     C                   if        depfil <> invfil and
086500010530     C                             invfil > *zeros
086600010530     C                   eval      flgMulti = '1'
086700010530     C                   if        vinflg = *blanks
086800010530     C                   add       1             cntNonEl
086900010530     C                   endif
087000010530     C                   endif
087100010530     C*
087200010530     C                   if        vinflg = '2'
087300010530     C                   eval      flgStato = '2'
087400010530     C                   endif
087500010530     C*
087600010530     C                   ENDSR
087700010530     C***
087800010530
087900010530
088000010530
088100010530
088200990920      /TITLE Invio dei dati al punto operativo.
088300000613     C     invio         BEGSR
088400990920     C*
088500990920     C                   reset                   dscmz
088600030618     C                   move      invfil        cmzdst
088700030618     C***                move      '789'         cmzdst
088800990920     C                   eval      cmzfld = vlrfou
088900990920     C                   eval      cmzmbd = vlrhdl
089000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
089100000710     C                   if        prmfir = *blanks
089200030616     C                   eval      cmzfla = 'EDIVAB0F'
089300030616     C                   eval      cmzmba = 'EDIVAB0F'
089400000710     C                   else
089500000710     C                   eval      cmzfla = prmfir
089600000710     C                   eval      cmzmba = prmfir
089700000710     C                   endif
089800990920     C                   eval      cmznrr = *zeros
089900990920     C                   move      §ctrok        cmznrr
090000021003     C                   eval      cmzlba = vlrfl1
090100990920     C                   call(e)   'TIS711C'
090200990920     C                   parm                    dscmz
090300990921     C                   parm      *blanks       esito
090400990920     C                   if        %error
090500990920     C                             or cmzerr = '1'
090600990921     C                             or esito  = '1'
090700000710     C                   eval      wrkesito = '3'
090800990920     C                   endif
090900990920     C*
091000000613     C                   ENDSR
091100000613     C***
091200010530
091300010530
091400010530
091500010530
091600010530      /TITLE Invio dei dati al punto operativo.
091700010530     C     opeini        BEGSR
091800010530     C*
091900010530     C* Inizializzo flag e contatori operativi
092000010530     C                   movel     '0'           flgGiro           1
092100010530     C                   movel     '0'           flgMulti          1
092200010530     C                   movel     '1'           flgStato          1
092300010615     C                   movel     '0'           flgOk             1
092400010530     C                   z-add     *zeros        cntNonEl         10 0
092500010530     C                   z-add     *zeros        depfil            3 0
092600010530     C                   z-add     *zeros        invfil            3 0
092700010530     C*
092800010530     C                   ENDSR
092900010530     C***
093000010530
093100010530
093200010530
093300990910
093400000613     C     *inzsr        BEGSR
093500990910     C*
093600990910     C     *entry        plist
093700990920     C                   parm                    tivlrds
093800990921     C                   parm      wrkesito      esito
093900000724     C                   parm                    prmlit
094000000710     C                   parm                    prmfir
094100130318     C*
094200130318     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
094300130318     C                   if        %subst(vlrppt:1:1) = 'S'
094400130318     C                   seton                                        50
094500130318     C                   else
094600130318     C                   setoff                                       50
094700130318     C                   endif
094800130318     C*
094900130318     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
095000130318     C                   if        %subst(vlrppt:6:1) = 'S'
095100130318     C                   seton                                        51
095200130318     C                   else
095300130318     C                   setoff                                       51
095400130318     C                   endif
095500130318     C*
095600130318     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
095700130318     C* (naturalmente solo se nn già presente)
095800130318     C                   if        %subst(vlrppt:7:1) = 'S'
095900130318     C                   seton                                        52
096000130318     C                   else
096100130318     C                   setoff                                       52
096200130318     C                   endif
096300130318     C*
096400130318     C* Verifico se sui parametri del traduttore è richiesto la scrittura forzata del
096500130318     C* tipo record 'P' del VAT (packing list PDF)
096600130318     C                   if        %subst(vlrppt:8:1) = 'S'
096700130318     C                   seton                                        53
096800130318     C                   else
096900130318     C                   setoff                                       53
097000130318     C                   endif
097100130322     C*
097200130322     C* Verifico se sui parametri del traduttore è richiesto un particolare codice trattamento merce
097300130322     C                   if        %subst(vlrppt:9:2) <> *blanks
097400130322     C                   movel     *blanks       wCTM              2
097500130322     C                   eval      wCTM = %subst(vlrppt:9:2)
097600130322     C                   endif
097700030616     C*
097800030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
097900030710     C                   move(p)   vlrMSG        wNomeFile
098000030710     C*
098100030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
098200030710     C* del codice CMR dal nome del file importato.
098300030710     C                   if        wNomeFile <> *blanks AND
098400030710     C                             vlrppt<>*blanks
098500041001     C*
098600030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
098700030710     C                   move(p)   wPosDaA       wPosDa
098800030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
098900030710     C                   move(p)   wLungA        wLung
099000030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
099100030710     C                   seton                                        60
099200030710     C                   else
099300050914     C                   if        vlrisv = 'UL'
099400050914     C                   movel     datcor        wDataOraAlfa
099500050914     C                   move      oracor        wDataOraAlfa
099600050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
099700050914     C                   seton                                        60
099800050914     C                   else
099900050914     C                   setoff                                       60
100000050914     C                   endif
100100030710     C                   endif
100200010606     C*
100300010606     C* CALCOLA LA DATA CORRENTE
100400130318     C                   time                    wn14             14 0
100500130318     C                   movel     wn14          oracor            6 0          *ORA
100600130318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
100700130318     C                   eval      datcor = %dec(%date() : *ISO)
100800000613     C*
100900000613     C                   ENDSR
101000000613     C***
