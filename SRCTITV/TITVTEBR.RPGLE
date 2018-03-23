000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
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
004800010606     D*------------------
004900010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000010606     D*------------------
005100010606     D WLBDA8          DS                  INZ
005200010606     D  G08DAT                 1      8  0
005300010606     D  G08INV                 9     16  0
005400010606     D  G08ERR                17     17
005500010606     D  G08TGI                18     22  0
005600010606     D*
005700071018     D*-------------------
005800071018     D* COSTANTI
005900071018     D*-------------------
006000071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006100071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006200990908
006300000913     C                   reset                   rrnum
006400990921     C                   reset                   esito
006500990921     C                   reset                   wrkesito
006600010530     C*
006700010530     C                   exsr      opeini
006800010530     C                   exsr      rwvab
006900041025     C*
007000041025     C* Esegue lancio TISI95R solo x chiusura
007100041025     C                   CLEAR                   TISI95DS
007200041025     C                   EVAL      I95TLA = 'C'
007300041025     C                   CALL      'TISI95R'
007400041025     C                   PARM                    TISI95DS
007500010530     C*
007600010530     C                   seton                                        lr
007700010530
007800010530
007900010530
008000010530
008100010530     C*--------------------------------------------------------
008200010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008300010530     C*--------------------------------------------------------
008400010530     C     PREELA        BEGSR
008500000724     C*
008600000724     C* SE OCCORRE SPEDIRE IN FILIALE
008700010530     C                   if        invfil <> *zeros and
008800010530     C                             flgGiro = '0'
008900010530     C*
009000010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009100010530     C                   eval      flgGiro = '1'
009200010530     C*
009300010530     C                   endif
009400010530     C*
009500010530     C                   ENDSR
009600010530     C***
009700010530
009800010530
009900010530
010000010530
010100010530
010200010530
010300010530     C*--------------------------------------------------------
010400010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010500010530     C*--------------------------------------------------------
010600010530     C     ENDELA        BEGSR
010700010530     C*
010800010530     C                   ENDSR
010900010530     C***
011000990908
011100010530
011200010530
011300010530
011400910830     C*--------------------------------------------------------
011500030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
011600910830     C*--------------------------------------------------------
011700941229     C     RWVAB         BEGSR
011800030710     C*
011900030710     C                   clear                   §CTROK
012000030710     C                   clear                   §CTRMO
012100030710     C                   clear                   §CTRNO
012200990910     C*
012300030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
012400030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
012500030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
012600030710     C                   if        *in60 = *on
012700990914     C                   if        not %open(tivin00r)
012800990908     C                   open      tivin00r
012900990914     C                   endif
013000030616     C                   if        not %open(edivabwr)
013100030616     C                   open      edivabwr
013200990914     C                   endif
013300990910     C*
013400921023     C                   DO        *HIVAL
013500990913     C*
013600990915     C                   READ      tivin00r                               70
013700071018     C*
013800071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
013900071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
014000010618     C*
014100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014200010618     C                   if        vinflg = '1'
014300010618     C                   eval      flgOk = '1'
014400010618     C                   endif
014500010705     C*
014600010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
014700010705     C                   if        %subst(vindta:1:1) = '='
014800010705     C                   eval      %subst(vindta:1:1) = *blanks
014900010705     C                   endif
015000010705     C*
015100000905     C                   if        vindta > *blanks
015200000613     C                   add       1             rrnum
015300010601     C* ...elimino eventuali segni
015400010601     C     '+':' '       xlate     vindta        vindta
015500990913     C*
015600010601     C                   if        *in70 = *off and
015700990913     C                             (vinflg = *blanks
015800010711     C                              or vinflg = '0'
015900010711     C                              or vinflg = '2')
016000990913     C*
016100030616     C                   clear                   EDIVAB00
016200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016300010711     C                   if        vinflg = *blanks or vinflg = '0'
016400010711     C                   clear                   vinmsg
016500010711     C                   endif
016600010711     C*
016700990913     C                   eval      fnvabaar = vindta
016800991022     C*
016900991022     C                   testb     x'3F'         vhbatb                   02
017000991022     C                   if        not *in02
017100991022     C*
017200971021     C                   SETOFF                                       303132
017300971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
017400971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
017500971023     C*    OPPURE TUTTI BLANKS
017600010530     C*
017700010530     C                   TESTN                   VHBLNP               30
017800010530     C  N30              TESTN                   VHBLNP                 30
017900010530     C  N30              TESTN                   VHBLNP                   30
018000010530     C                   MOVE      VHBLNP        W001A             1
018100010530     C     *IN30         IFEQ      *OFF
018200010530     C     W001A         ORLT      '0'
018300010530     C                   SETON                                        31
018400010530     C                   eval      vinmsg = %trimr(vinmsg)
018500010530     C                             + ' ' + 'VHBLNP'
018600010530     C                   ELSE
018700010530     C                   MOVE      VHBLNP        VABLNP
018800010530     C     VABLNP        IFLE      *ZEROS
018900010530     C                   SETON                                        31
019000010530     C                   eval      vinmsg = %trimr(vinmsg)
019100010530     C                             + ' ' + 'VHBLNP'
019200010530     C                   ENDIF
019300010530     C                   ENDIF
019400010530     C*
019500010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019600010530     C                   eval      depfil = VABLNP
019700010530     C                   exsr      repfil
019800010601     C                   if        depfil = invfil
019900021014     C                   if        vlrpoi = 999
020000021010     C                   MOVE(P)   invfil        VABFGS
020100021014     C                   else
020200021014     C                   MOVE(P)   vlrpoi        VABFGS
020300021014     C                   endif
020400010530     C*
020500010530     C                   exsr      PREELA
020600010530     C*
020700971023     C                   TESTN                   VHBCCM               30
020800971023     C  N30              TESTN                   VHBCCM                 30
020900971023     C  N30              TESTN                   VHBCCM                   30
021000971021     C                   MOVE      VHBCCM        W001A             1
021100971021     C     *IN30         IFEQ      *OFF
021200971021     C     W001A         ORLT      '0'
021300971021     C                   SETON                                        31
021400010604     C                   eval      vinmsg = %trimr(vinmsg)
021500010604     C                             + ' ' + 'VHBCCM'
021600971021     C                   ELSE
021700971021     C                   MOVE      VHBCCM        VABCCM
021800971024     C     VABCCM        IFLE      *ZEROS
021900971024     C                   SETON                                        31
022000010604     C                   eval      vinmsg = %trimr(vinmsg)
022100010604     C                             + ' ' + 'VHBCCM'
022200971024     C                   ENDIF
022300971021     C                   ENDIF
022400971021     C*
022500971023     C                   TESTN                   VHBAAS               30
022600971023     C  N30              TESTN                   VHBAAS                 30
022700971023     C  N30              TESTN                   VHBAAS                   30
022800971021     C                   MOVE      VHBAAS        W001A             1
022900971021     C     *IN30         IFEQ      *OFF
023000971021     C     W001A         ORLT      '0'
023100971023     C     W001A         ANDNE     *BLANK
023200971021     C                   SETON                                        32
023300010606     C                   MOVEL     datcor        VABAAS
023400010604     C                   eval      vinmsg = %trimr(vinmsg)
023500010604     C                             + ' ' + 'VHBAAS'
023600971021     C                   ELSE
023700971021     C                   MOVE      VHBAAS        VABAAS
023800971021     C                   ENDIF
023900971021     C*
024000971023     C                   TESTN                   VHBMGS               30
024100971023     C  N30              TESTN                   VHBMGS                 30
024200971023     C  N30              TESTN                   VHBMGS                   30
024300971021     C                   MOVE      VHBMGS        W001A             1
024400971021     C     *IN30         IFEQ      *OFF
024500971021     C     W001A         ORLT      '0'
024600971023     C     W001A         ANDNE     *BLANK
024700971021     C                   SETON                                        32
024800010601     C                   MOVE      '0101'        VABMGS
024900010604     C                   eval      vinmsg = %trimr(vinmsg)
025000010604     C                             + ' ' + 'VHBMGS'
025100971021     C                   ELSE
025200971021     C                   MOVE      VHBMGS        VABMGS
025300971021     C                   ENDIF
025400971021     C*
025500971023     C                   TESTN                   VHBNRS               30
025600971023     C  N30              TESTN                   VHBNRS                 30
025700971023     C  N30              TESTN                   VHBNRS                   30
025800971021     C                   MOVE      VHBNRS        W001A             1
025900971021     C     *IN30         IFEQ      *OFF
026000971021     C     W001A         ORLT      '0'
026100971023     C     W001A         ANDNE     *BLANK
026200971021     C                   SETON                                        32
026300971021     C                   MOVE      *ZERO         VABNRS
026400010604     C                   eval      vinmsg = %trimr(vinmsg)
026500010604     C                             + ' ' + 'VHBNRS'
026600971021     C                   ELSE
026700971021     C                   MOVE      VHBNRS        VABNRS
026800971021     C                   ENDIF
026900971021     C*
027000971023     C                   TESTN                   VHBNSP               30
027100971023     C  N30              TESTN                   VHBNSP                 30
027200971023     C  N30              TESTN                   VHBNSP                   30
027300971021     C                   MOVE      VHBNSP        W001A             1
027400971021     C     *IN30         IFEQ      *OFF
027500971021     C     W001A         ORLT      '0'
027600971023     C     W001A         ANDNE     *BLANK
027700971021     C                   SETON                                        32
027800971021     C                   MOVE      *ZERO         VABNSP
027900010604     C                   eval      vinmsg = %trimr(vinmsg)
028000010604     C                             + ' ' + 'VHBNSP'
028100971021     C                   ELSE
028200971021     C                   MOVE      VHBNSP        VABNSP
028300971021     C                   ENDIF
028400971021     C*
028500971023     C                   TESTN                   VHBLNA               30
028600971023     C  N30              TESTN                   VHBLNA                 30
028700971023     C  N30              TESTN                   VHBLNA                   30
028800971021     C                   MOVE      VHBLNA        W001A             1
028900971021     C     *IN30         IFEQ      *OFF
029000971021     C     W001A         ORLT      '0'
029100971023     C     W001A         ANDNE     *BLANK
029200971021     C                   SETON                                        32
029300971021     C                   MOVE      *ZERO         VABLNA
029400010604     C                   eval      vinmsg = %trimr(vinmsg)
029500010604     C                             + ' ' + 'VHBLNA'
029600971021     C                   ELSE
029700971021     C                   MOVE      VHBLNA        VABLNA
029800971021     C                   ENDIF
029900971021     C*
030000971023     C                   TESTN                   VHBCTR               30
030100971023     C  N30              TESTN                   VHBCTR                 30
030200971023     C  N30              TESTN                   VHBCTR                   30
030300971021     C                   MOVE      VHBCTR        W001A             1
030400971021     C     *IN30         IFEQ      *OFF
030500971021     C     W001A         ORLT      '0'
030600971023     C     W001A         ANDNE     *BLANK
030700971021     C                   SETON                                        32
030800971021     C                   MOVE      *ZERO         VABCTR
030900010604     C                   eval      vinmsg = %trimr(vinmsg)
031000010604     C                             + ' ' + 'VHBCTR'
031100971021     C                   ELSE
031200971021     C                   MOVE      VHBCTR        VABCTR
031300971021     C                   ENDIF
031400941229     C*
031500971021     C                   MOVE      *ZERO         VABIAS
031600941229     C                   MOVE      VHBIAS        WASC15
031700971023     C                   TESTN                   WI15                 30
031800971023     C  N30              TESTN                   WI15                   30
031900971023     C  N30              TESTN                   WI15                     30
032000971021     C                   MOVE      WI15          W001A             1
032100971021     C     *IN30         IFEQ      *OFF
032200971021     C     W001A         ORLT      '0'
032300971023     C     W001A         ANDNE     *BLANK
032400971021     C                   SETON                                        32
032500971021     C                   MOVE      *ZERO         VABIAS
032600010604     C                   eval      vinmsg = %trimr(vinmsg)
032700010604     C                             + ' ' + 'VHBIAS'
032800971021     C                   ELSE
032900971021     C                   MOVEL     WI15          VABIAS
033000971023     C                   TESTN                   WD15                 30
033100971023     C  N30              TESTN                   WD15                   30
033200971023     C  N30              TESTN                   WD15                     30
033300971021     C                   MOVE      WD15          W001A             1
033400971021     C     *IN30         IFEQ      *OFF
033500971021     C     W001A         ORLT      '0'
033600971023     C     W001A         ANDNE     *BLANK
033700971021     C                   SETON                                        32
033800971021     C                   MOVE      *ZERO         VABIAS
033900010604     C                   eval      vinmsg = %trimr(vinmsg)
034000010604     C                             + ' ' + 'VHBIAS'
034100971021     C                   ELSE
034200971021     C                   MOVE      WD15          VABIAS
034300971021     C                   ENDIF
034400050118     C                   ENDIF
034500941229     C*
034600971023     C                   TESTN                   VHBNCL               30
034700971023     C  N30              TESTN                   VHBNCL                 30
034800971023     C  N30              TESTN                   VHBNCL                   30
034900971021     C                   MOVE      VHBNCL        W001A             1
035000971021     C     *IN30         IFEQ      *OFF
035100971021     C     W001A         ORLT      '0'
035200971021     C                   SETON                                        32
035300971021     C                   MOVE      *ZERO         VABNCL
035400010604     C                   eval      vinmsg = %trimr(vinmsg)
035500010604     C                             + ' ' + 'VHBNCL'
035600971021     C                   ELSE
035700971021     C                   MOVE      VHBNCL        VABNCL
035800971021     C                   ENDIF
035900941229     C*
036000971021     C                   MOVE      *ZERO         VABPKB
036100971021     C                   MOVE      VHBPKB        WASC09
036200010719     C                   IF        WI09 = *blanks
036300010719     C                   EVAL      WI09 = *zeros
036400010719     C                   ENDIF
036500971023     C                   TESTN                   WI09                 30
036600971023     C  N30              TESTN                   WI09                   30
036700971023     C  N30              TESTN                   WI09                     30
036800971021     C                   MOVE      WI09          W001A             1
036900971021     C     *IN30         IFEQ      *OFF
037000971021     C     W001A         ORLT      '0'
037100971021     C                   SETON                                        32
037200971021     C                   MOVE      *ZERO         VABPKB
037300010604     C                   eval      vinmsg = %trimr(vinmsg)
037400010604     C                             + ' ' + 'VHBPKB'
037500971021     C                   ELSE
037600971021     C                   MOVEL     WI09          VABPKB
037700010719     C                   IF        WD09 = *blanks
037800010719     C                   EVAL      WD09 = *zeros
037900010719     C                   ENDIF
038000971023     C                   TESTN                   WD09                 30
038100971023     C  N30              TESTN                   WD09                     30
038200971021     C                   MOVE      WD09          W001A             1
038300971021     C     *IN30         IFEQ      *OFF
038400971021     C     W001A         ORLT      '0'
038500971023     C     W001A         ANDNE     *BLANK
038600971021     C                   SETON                                        32
038700971021     C                   MOVE      *ZERO         VABPKB
038800010604     C                   eval      vinmsg = %trimr(vinmsg)
038900010604     C                             + ' ' + 'VHBPKB'
039000971021     C                   ELSE
039100971021     C                   MOVE      WD09          VABPKB
039200971021     C                   ENDIF
039300050118     C                   ENDIF
039400971021     C*
039500041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
039600041001     C                   IF        *IN51 = *OFF
039700971021     C                   MOVE      *ZERO         VABVLB
039800971021     C                   MOVE      VHBVLB        WASC07
039900010719     C                   IF        WI07 = *blanks
040000010719     C                   EVAL      WI07 = *zeros
040100010719     C                   ENDIF
040200971023     C                   TESTN                   WI07                 30
040300971023     C  N30              TESTN                   WI07                   30
040400971023     C  N30              TESTN                   WI07                     30
040500971021     C                   MOVE      WI07          W001A             1
040600971021     C     *IN30         IFEQ      *OFF
040700971021     C     W001A         ORLT      '0'
040800971023     C     W001A         ANDNE     *BLANK
040900971021     C                   SETON                                        32
041000971021     C                   MOVE      *ZERO         VABVLB
041100010604     C                   eval      vinmsg = %trimr(vinmsg)
041200010604     C                             + ' ' + 'VHBVLB'
041300971021     C                   ELSE
041400971021     C                   MOVEL     WI07          VABVLB
041500010719     C                   IF        WD07 = *blanks
041600010719     C                   EVAL      WD07 = *zeros
041700010719     C                   ENDIF
041800971023     C                   TESTN                   WD07                 30
041900971023     C  N30              TESTN                   WD07                   30
042000971023     C  N30              TESTN                   WD07                     30
042100971021     C                   MOVE      WD07          W001A             1
042200971021     C     *IN30         IFEQ      *OFF
042300971021     C     W001A         ORLT      '0'
042400971023     C     W001A         ANDNE     *BLANK
042500971021     C                   SETON                                        32
042600971021     C                   MOVE      *ZERO         VABVLB
042700010604     C                   eval      vinmsg = %trimr(vinmsg)
042800010604     C                             + ' ' + 'VHBVLB'
042900971021     C                   ELSE
043000971021     C                   MOVE      WD07          VABVLB
043100971021     C                   ENDIF
043200050118     C                   ENDIF
043300041001     C                   ELSE
043400041001     C* ...altrimenti forzo azzeramento del volume
043500041001     C                   Z-ADD     *zeros        VABVLB
043600041001     C                   ENDIF
043700941229     C*
043800971021     C                   MOVE      *ZERO         VABQFT
043900971021     C                   MOVE      VHBQFT        WASC15
044000010719     C                   IF        WI15 = *blanks
044100010719     C                   EVAL      WI15 = *zeros
044200010719     C                   ENDIF
044300971023     C                   TESTN                   WI15                 30
044400971023     C  N30              TESTN                   WI15                   30
044500971023     C  N30              TESTN                   WI15                     30
044600971021     C                   MOVE      WI15          W001A             1
044700971021     C     *IN30         IFEQ      *OFF
044800971021     C     W001A         ORLT      '0'
044900971023     C     W001A         ANDNE     *BLANK
045000971021     C                   SETON                                        32
045100971021     C                   MOVE      *ZERO         VABQFT
045200010604     C                   eval      vinmsg = %trimr(vinmsg)
045300010604     C                             + ' ' + 'VHBQFT'
045400971021     C                   ELSE
045500971021     C                   MOVEL     WI15          VABQFT
045600010719     C                   IF        WD15 = *blanks
045700010719     C                   EVAL      WD15 = *zeros
045800010719     C                   ENDIF
045900971023     C                   TESTN                   WD15                 30
046000971023     C  N30              TESTN                   WD15                   30
046100971023     C  N30              TESTN                   WD15                     30
046200971021     C                   MOVE      WD15          W001A             1
046300971021     C     *IN30         IFEQ      *OFF
046400971021     C     W001A         ORLT      '0'
046500971023     C     W001A         ANDNE     *BLANK
046600971021     C                   SETON                                        32
046700971021     C                   MOVE      *ZERO         VABQFT
046800010604     C                   eval      vinmsg = %trimr(vinmsg)
046900010604     C                             + ' ' + 'VHBQFT'
047000971021     C                   ELSE
047100971021     C                   MOVE      WD15          VABQFT
047200971021     C                   ENDIF
047300050118     C                   ENDIF
047400941229     C*
047500971021     C                   MOVE      *ZERO         VABCAS
047600971021     C                   MOVE      VHBCAS        WASC15
047700010719     C                   IF        WI15 = *blanks
047800010719     C                   EVAL      WI15 = *zeros
047900010719     C                   ENDIF
048000971023     C                   TESTN                   WI15                 30
048100971023     C  N30              TESTN                   WI15                   30
048200971023     C  N30              TESTN                   WI15                     30
048300971021     C                   MOVE      WI15          W001A             1
048400971021     C     *IN30         IFEQ      *OFF
048500971021     C     W001A         ORLT      '0'
048600971023     C     W001A         ANDNE     *BLANK
048700971021     C                   SETON                                        32
048800971021     C                   MOVE      *ZERO         VABCAS
048900010604     C                   eval      vinmsg = %trimr(vinmsg)
049000010604     C                             + ' ' + 'VHBCAS'
049100971021     C                   ELSE
049200971021     C                   MOVEL     WI15          VABCAS
049300010719     C                   IF        WD15 = *blanks
049400010719     C                   EVAL      WD15 = *zeros
049500010719     C                   ENDIF
049600971023     C                   TESTN                   WD15                 30
049700971023     C  N30              TESTN                   WD15                   30
049800971023     C  N30              TESTN                   WD15                     30
049900971021     C                   MOVE      WD15          W001A             1
050000971021     C     *IN30         IFEQ      *OFF
050100971021     C     W001A         ORLT      '0'
050200971023     C     W001A         ANDNE     *BLANK
050300971021     C                   SETON                                        32
050400971021     C                   MOVE      *ZERO         VABCAS
050500010604     C                   eval      vinmsg = %trimr(vinmsg)
050600010604     C                             + ' ' + 'VHBCAS'
050700971021     C                   ELSE
050800971021     C                   MOVE      WD15          VABCAS
050900971021     C                   ENDIF
051000050118     C                   ENDIF
051100971021     C*
051200971023     C                   TESTN                   VHBRMN               30
051300971023     C  N30              TESTN                   VHBRMN                 30
051400971023     C  N30              TESTN                   VHBRMN                   30
051500971021     C                   MOVE      VHBRMN        W001A             1
051600971021     C     *IN30         IFEQ      *OFF
051700971021     C     W001A         ORLT      '0'
051800971021     C                   SETON                                        32
051900971021     C                   MOVE      *ZERO         VABRMN
052000010604     C                   eval      vinmsg = %trimr(vinmsg)
052100010604     C                             + ' ' + 'VHBRMN'
052200971021     C                   ELSE
052300971021     C                   MOVE      VHBRMN        VABRMN
052400971021     C                   ENDIF
052500971021     C*
052600971023     C                   TESTN                   VHBNCD               30
052700971023     C  N30              TESTN                   VHBNCD                 30
052800971023     C  N30              TESTN                   VHBNCD                   30
052900971021     C                   MOVE      VHBNCD        W001A             1
053000971021     C     *IN30         IFEQ      *OFF
053100971021     C     W001A         ORLT      '0'
053200971023     C     W001A         ANDNE     *BLANK
053300971021     C                   SETON                                        32
053400971021     C                   MOVE      *ZERO         VABNCD
053500010604     C                   eval      vinmsg = %trimr(vinmsg)
053600010604     C                             + ' ' + 'VHBNCD'
053700971021     C                   ELSE
053800971021     C                   MOVE      VHBNCD        VABNCD
053900971021     C                   ENDIF
054000971021     C*
054100971023     C                   TESTN                   VHBNCA               30
054200971023     C  N30              TESTN                   VHBNCA                 30
054300971023     C  N30              TESTN                   VHBNCA                   30
054400971021     C                   MOVE      VHBNCA        W001A             1
054500971021     C     *IN30         IFEQ      *OFF
054600971021     C     W001A         ORLT      '0'
054700971023     C     W001A         ANDNE     *BLANK
054800971021     C                   SETON                                        32
054900971021     C                   MOVE      *ZERO         VABNCA
055000010604     C                   eval      vinmsg = %trimr(vinmsg)
055100010604     C                             + ' ' + 'VHBNCA'
055200971021     C                   ELSE
055300971021     C                   MOVE      VHBNCA        VABNCA
055400971021     C                   ENDIF
055500971021     C*
055600971023     C                   TESTN                   VHBZNC               30
055700971023     C  N30              TESTN                   VHBZNC                 30
055800971023     C  N30              TESTN                   VHBZNC                   30
055900971021     C                   MOVE      VHBZNC        W001A             1
056000971021     C     *IN30         IFEQ      *OFF
056100971021     C     W001A         ORLT      '0'
056200971023     C     W001A         ANDNE     *BLANK
056300971021     C                   SETON                                        32
056400971021     C                   MOVE      *ZERO         VABZNC
056500010604     C                   eval      vinmsg = %trimr(vinmsg)
056600010604     C                             + ' ' + 'VHBZNC'
056700971021     C                   ELSE
056800971021     C                   MOVE      VHBZNC        VABZNC
056900971021     C                   ENDIF
057000971021     C*
057100971023     C                   TESTN                   VHBDCR               30
057200971023     C  N30              TESTN                   VHBDCR                 30
057300971023     C  N30              TESTN                   VHBDCR                   30
057400971021     C                   MOVE      VHBDCR        W001A             1
057500971021     C     *IN30         IFEQ      *OFF
057600971021     C     W001A         ORLT      '0'
057700971023     C     W001A         ANDNE     *BLANK
057800971021     C                   SETON                                        32
057900971021     C                   MOVE      *ZERO         VABDCR
058000010604     C                   eval      vinmsg = %trimr(vinmsg)
058100010604     C                             + ' ' + 'VHBDCR'
058200971021     C                   ELSE
058300971021     C                   MOVE      VHBDCR        VABDCR
058400971021     C                   ENDIF
058500971021     C*
058600971023     C                   TESTN                   VHBHCR               30
058700971023     C  N30              TESTN                   VHBHCR                 30
058800971023     C  N30              TESTN                   VHBHCR                   30
058900971021     C                   MOVE      VHBHCR        W001A             1
059000971021     C     *IN30         IFEQ      *OFF
059100971021     C     W001A         ORLT      '0'
059200971023     C     W001A         ANDNE     *BLANK
059300971021     C                   SETON                                        32
059400971021     C                   MOVE      *ZERO         VABHCR
059500010604     C                   eval      vinmsg = %trimr(vinmsg)
059600010604     C                             + ' ' + 'VHBHCR'
059700971021     C                   ELSE
059800971021     C                   MOVE      VHBHCR        VABHCR
059900971021     C                   ENDIF
060000941229     C*
060100971021     C                   MOVE      *ZERO         VABVMD
060200971021     C                   MOVE      VHBVMD        WASC15
060300010719     C                   IF        WI15 = *blanks
060400010719     C                   EVAL      WI15 = *zeros
060500010719     C                   ENDIF
060600971023     C                   TESTN                   WI15                 30
060700971023     C  N30              TESTN                   WI15                   30
060800971023     C  N30              TESTN                   WI15                     30
060900971021     C                   MOVE      WI15          W001A             1
061000971021     C     *IN30         IFEQ      *OFF
061100971021     C     W001A         ORLT      '0'
061200971023     C     W001A         ANDNE     *BLANK
061300971021     C                   SETON                                        32
061400971021     C                   MOVE      *ZERO         VABVMD
061500010604     C                   eval      vinmsg = %trimr(vinmsg)
061600010604     C                             + ' ' + 'VHBVMD'
061700971021     C                   ELSE
061800971021     C                   MOVEL     WI15          VABVMD
061900010719     C                   IF        WD15 = *blanks
062000010719     C                   EVAL      WD15 = *zeros
062100010719     C                   ENDIF
062200971023     C                   TESTN                   WD15                 30
062300971023     C  N30              TESTN                   WD15                   30
062400971023     C  N30              TESTN                   WD15                     30
062500971021     C                   MOVE      WD15          W001A             1
062600971021     C     *IN30         IFEQ      *OFF
062700971021     C     W001A         ORLT      '0'
062800971023     C     W001A         ANDNE     *BLANK
062900971021     C                   SETON                                        32
063000971021     C                   MOVE      *ZERO         VABVMD
063100010604     C                   eval      vinmsg = %trimr(vinmsg)
063200010604     C                             + ' ' + 'VHBVMD'
063300971021     C                   ELSE
063400971021     C                   MOVE      WD15          VABVMD
063500971021     C                   ENDIF
063600050118     C                   ENDIF
063700971021     C*
063800971023     C                   TESTN                   VHBANT               30
063900971023     C  N30              TESTN                   VHBANT                 30
064000971023     C  N30              TESTN                   VHBANT                   30
064100971021     C                   MOVE      VHBANT        W001A             1
064200971021     C     *IN30         IFEQ      *OFF
064300971021     C     W001A         ORLT      '0'
064400971023     C     W001A         ANDNE     *BLANK
064500971021     C                   SETON                                        32
064600971021     C                   MOVE      *ZERO         VABANT
064700010604     C                   eval      vinmsg = %trimr(vinmsg)
064800010604     C                             + ' ' + 'VHBANT'
064900971021     C                   ELSE
065000971021     C                   MOVE      VHBANT        VABANT
065100971021     C                   ENDIF
065200971021     C*
065300971021     C* CAMPI ALFABETICI:
065400971021     C                   MOVE      VHBATB        VABATB
065500971021     C                   MOVE      VHBCBO        VABCBO
065600971021     C                   MOVE      VHBRSD        VABRSD
065700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
065800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
065900020117     C* ==
066000971021     C                   MOVE      VHBRD2        VABRD2
066100971021     C                   MOVE      VHBIND        VABIND
066200971021     C                   MOVE      VHBCAD        VABCAD
066300971021     C                   MOVE      VHBLOD        VABLOD
066400971021     C                   MOVE      VHBPRD        VABPRD
066500971021     C                   MOVE      VHBNZD        VABNZD
066600971021     C                   MOVE      VHBGC1        VABGC1
066700971021     C                   MOVE      VHBGC2        VABGC2
066800971021     C                   MOVE      VHBTSP        VABTSP
066900971021     C                   MOVE      VHBVAS        VABVAS
067000971021     C                   MOVE      VHBNAS        VABNAS
067100971021     C                   MOVE      VHBTIC        VABTIC
067200971021     C                   MOVE      VHBVCA        VABVCA
067300971021     C                   MOVE      VHBGCA        VABGCA
067400971021     C                   MOVE      VHBRMA        VABRMA
067500971021     C                   MOVE      VHBXCO        VABXCO
067600971021     C                   MOVE      VHBNOT        VABNOT
067700971021     C                   MOVE      VHBNT2        VABNT2
067800971021     C                   MOVE      VHBCTM        VABCTM
067900080303     C**
068000100708     C***                IF        VABCTM = '7Q' AND VABCCM = 0053594
068100100708     C***                EVAL      VABCTM = '7P'
068200100708     C***                ENDIF
068300080303     C**
068400971021     C                   MOVE      VHBFFD        VABFFD
068500971021     C                   MOVE      VHBTCR        VABTCR
068600971021     C                   MOVE      VHBCTS        VABCTS
068700971021     C                   MOVE      VHBFTM        VABFTM
068800971021     C                   MOVE      VHBVAD        VABVAD
068900971021     C                   MOVE      VHBGMA        VABGMA
069000971021     C                   MOVE      VHBGGA        VABGGA
069100971021     C                   MOVE      VHBGVA        VABGVA
069200971021     C                   MOVE      VHBTC1        VABTC1
069300971021     C                   MOVE      VHBTC2        VABTC2
069400971021     C                   MOVE      VHBSCL        VABSCL
069500971021     C                   MOVE      VHBRMO        VABRMO
069600971021     C                   MOVE      VHBCMO        VABCMO
069700971021     C                   MOVE      VHBNMO        VABNMO
069800971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
069900971021     C* NELLA NAZIONE ORIGINALE
070000971021     C                   MOVE      VHBNMO        WA1D              1
070100971021     C                   MOVEL     VHBNMO        WA1S              1
070200971021     C     WA1D          IFNE      *BLANK
070300971021     C     WA1S          ANDEQ     *BLANK
070400971021     C                   MOVE      *BLANK        VABNMO
070500030616     C                   ENDIF
070600030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
070700030710     C                   EVAL      VABCMR = wCMR
070800030616     C                   EVAL      VABDCM = DATCOR
070900030616     C                   EVAL      VABDTS = DATCOR
071000040402     C                   EVAL      VABHMS = ORACOR
071100030616     C                   EVAL      VABCNT = 1
071200041025     C*
071300041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
071400041025     C                   IF        *IN52 = *ON
071500041025     C* Reperisco la provincia dal CAP e dalla località
071600041025     C                   IF        VABCAD <> *blanks AND
071700050907     C                             VABPRD  = *blanks AND
071800050907     C                             VABNZD  = *blanks
071900041025     C                   CLEAR                   TISI95DS
072000041025     C                   EVAL      I95TCN = '3'
072100041025     C                   Z-ADD     datcor        I95DAT
072200041025     C                   EVAL      I95CAP = VABCAD
072300041025     C                   EVAL      I95LOC = VABLOD
072400041025     C                   CALL      'TISI95R'
072500041025     C                   PARM                    TISI95DS
072600041025     C                   EVAL      VABPRD = O95PRV
072700041025     C                   ENDIF
072800041025     C                   ENDIF
072900930409     C*
073000010604     C  N31              ADD       1             §CTROK            7 0
073100010604     C   32              ADD       1             §CTRMO            7 0
073200010604     C   31              ADD       1             §CTRNO            7 0
073300030616     C  N31              WRITE     EDIVAB00
073400990910     C*
073500010601     C                   if        *in31 = *off and
073600010601     C                             *in32 = *off
073700990910     C                   eval      vinflg = '1'
073800990910     C                   else
073900990910     C                   eval      vinflg = '2'
074000990910     C                   endif
074100991022     C                   endif
074200990910     C                   endif
074300010601     C*
074400010601     C                   endif
074500000905     C*
074600000905     C                   else
074700000905     C                   eval      vinflg = '1'
074800000905     C                   endif
074900000905     C*
075000000905     C  N70              update    tivin000
075100991022     C*
075200991022     C  N70              ENDdo
075300010530     C
075400010530     C*
075500010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
075600010530     C                   if        cntNonEl = *zeros or
075700010530     C                             flgMulti = '0'
075800990910     C* Se non ci sono record con errori ...
075900010530     C                   if        §ctrno = 0 and
076000010601     C                             §ctrmo = 0 and
076100010530     C                             flgStato <> '2'
076200990910     C* ... restituisco esito OK.
076300990921     C                   eval      wrkesito = '0'
076400990910     C                   else
076500010601     C                   if        §ctrok > 0
076600990921     C                   eval      wrkesito = '1'
076700000710     C                   else
076800010615     C                   if        flgOk = '0'
076900010615     C                   eval      wrkesito = '2'
077000010615     C                   else
077100010615     C                   eval      wrkesito = '6'
077200010615     C                   endif
077300990910     C                   endif
077400000710     C                   endif
077500010530     C                   else
077600010530     C                   eval      wrkesito = '9'
077700010530     C                   endif
077800030710     C*
077900030710 X  1C                   else
078000030710     C                   eval      wrkesito = '8'
078100030710     C                   eval      flgGiro = '1'
078200030710 E  1C                   endif
078300030710     C*
078400990914     C                   if        %open(tivin00r)
078500990908     C                   close     tivin00r
078600990914     C                   endif
078700030616     C                   if        %open(edivabwr)
078800030616     C                   close     edivabwr
078900990914     C                   endif
079000010530     C*
079100010530     C                   if        vlrpoi <> 999
079200010530     C                   eval      invfil = vlrpoi
079300010530     C                   endif
079400990910     C*
079500990920     C                   if        §ctrok > 0
079600010530     C                             and invfil > *zeros
079700000613     C                   exsr      invio
079800990920     C                   endif
079900010612     C*
080000010612     C                   if        flgGiro = '1'
080100010612     C                   exsr      endela
080200010612     C                   endif
080300990920     C*
080400910830     C                   ENDSR
080500000613     C***
080600990920
080700010530
080800010530
080900010530
081000010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
081100010530     C     repfil        BEGSR
081200010530     C*
081300010530     C                   if        invfil = *zeros and
081400010530     C                             depfil > *zeros and
081500010613     C                             (vinflg = *blanks or
081600010613     C                              vinflg = *zeros)
081700010530     C
081800010530     C                   eval      invfil = depfil
081900010530     C                   endif
082000010530     C*
082100010530     C                   if        depfil <> invfil and
082200010530     C                             invfil > *zeros
082300010530     C                   eval      flgMulti = '1'
082400010530     C                   if        vinflg = *blanks
082500010530     C                   add       1             cntNonEl
082600010530     C                   endif
082700010530     C                   endif
082800010530     C*
082900010530     C                   if        vinflg = '2'
083000010530     C                   eval      flgStato = '2'
083100010530     C                   endif
083200010530     C*
083300010530     C                   ENDSR
083400010530     C***
083500010530
083600010530
083700010530
083800010530
083900990920      /TITLE Invio dei dati al punto operativo.
084000000613     C     invio         BEGSR
084100990920     C*
084200990920     C                   reset                   dscmz
084300030618     C                   move      invfil        cmzdst
084400030618     C***                move      '789'         cmzdst
084500990920     C                   eval      cmzfld = vlrfou
084600990920     C                   eval      cmzmbd = vlrhdl
084700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
084800000710     C                   if        prmfir = *blanks
084900030616     C                   eval      cmzfla = 'EDIVAB0F'
085000030616     C                   eval      cmzmba = 'EDIVAB0F'
085100000710     C                   else
085200000710     C                   eval      cmzfla = prmfir
085300000710     C                   eval      cmzmba = prmfir
085400000710     C                   endif
085500990920     C                   eval      cmznrr = *zeros
085600990920     C                   move      §ctrok        cmznrr
085700021003     C                   eval      cmzlba = vlrfl1
085800990920     C                   call(e)   'TIS711C'
085900990920     C                   parm                    dscmz
086000990921     C                   parm      *blanks       esito
086100990920     C                   if        %error
086200990920     C                             or cmzerr = '1'
086300990921     C                             or esito  = '1'
086400000710     C                   eval      wrkesito = '3'
086500990920     C                   endif
086600990920     C*
086700000613     C                   ENDSR
086800000613     C***
086900010530
087000010530
087100010530
087200010530
087300010530      /TITLE Invio dei dati al punto operativo.
087400010530     C     opeini        BEGSR
087500010530     C*
087600010530     C* Inizializzo flag e contatori operativi
087700010530     C                   movel     '0'           flgGiro           1
087800010530     C                   movel     '0'           flgMulti          1
087900010530     C                   movel     '1'           flgStato          1
088000010615     C                   movel     '0'           flgOk             1
088100010530     C                   z-add     *zeros        cntNonEl         10 0
088200010530     C                   z-add     *zeros        depfil            3 0
088300010530     C                   z-add     *zeros        invfil            3 0
088400010530     C*
088500010530     C                   ENDSR
088600010530     C***
088700010530
088800010530
088900010530
089000990910
089100000613     C     *inzsr        BEGSR
089200990910     C*
089300990910     C     *entry        plist
089400990920     C                   parm                    tivlrds
089500990921     C                   parm      wrkesito      esito
089600000724     C                   parm                    prmlit
089700000710     C                   parm                    prmfir
089800030616     C*
089900030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
090000030710     C                   move(p)   vlrMSG        wNomeFile
090100030710     C*
090200030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
090300030710     C* del codice CMR dal nome del file importato.
090400030710     C                   if        wNomeFile <> *blanks AND
090500030710     C                             vlrppt<>*blanks
090600041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
090700030710     C                   if        %subst(vlrppt:1:1) = 'S'
090800030710     C                   seton                                        50
090900030710     C                   else
091000030710     C                   setoff                                       50
091100030710     C                   endif
091200041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
091300041001     C                   if        %subst(vlrppt:6:1) = 'S'
091400041001     C                   seton                                        51
091500041001     C                   else
091600041001     C                   setoff                                       51
091700041001     C                   endif
091800041025     C*
091900041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
092000041025     C* (naturalmente solo se nn già presente)
092100041025     C                   if        %subst(vlrppt:7:1) = 'S'
092200041025     C                   seton                                        52
092300041025     C                   else
092400041025     C                   setoff                                       52
092500041025     C                   endif
092600041001     C*
092700030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
092800030710     C                   move(p)   wPosDaA       wPosDa
092900030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
093000030710     C                   move(p)   wLungA        wLung
093100030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
093200030710     C                   seton                                        60
093300030710     C                   else
093400050914     C                   if        vlrisv = 'UL'
093500050914     C                   movel     datcor        wDataOraAlfa
093600050914     C                   move      oracor        wDataOraAlfa
093700050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
093800050914     C                   seton                                        60
093900050914     C                   else
094000050914     C                   setoff                                       60
094100050914     C                   endif
094200030710     C                   endif
094300010606     C*
094400010606     C* CALCOLA LA DATA CORRENTE
094500010606     C                   time                    wn14             14 0
094600010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
094700040402     C                   movel     wn14          oracor            6 0          *ORA
094800010606     C                   z-add     wn8           g08dat
094900010606     C                   z-add     *zeros        g08inv
095000010606     C                   movel     '0'           g08err
095100010606     C                   call      'XSRDA8'
095200010606     C                   parm                    wlbda8
095300010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
095400000613     C*
095500000613     C                   ENDSR
095600000613     C***
