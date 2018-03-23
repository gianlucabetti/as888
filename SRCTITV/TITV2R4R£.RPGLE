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
003600140113     D edivabds      e ds                  extname(edivab0f)
003700140114     D edivadds      e ds                  extname(edivad0f)
003800990910     D esito           s              1
003900000724     D prmlit          s             10
004000000710     D prmfir          s             10
004100990921     D wrkesito        s                   like(esito)
004200000613     D rrnum           s              6  0 INZ(*zeros)
004300030616     D wNomeFile       s             30    INZ(*blanks)
004400030710     D wCMR            s             35    INZ(*blanks)
004500030710     D wPosDaA         s              2    INZ(*blanks)
004600030710     D wPosDa          s              2  0 INZ(*zeros)
004700030710     D wLungA          s              2    INZ(*blanks)
004800030710     D wLung           s              2  0 INZ(*zeros)
004900050914     D wDataOraAlfa    s             14    INZ(*blanks)
005000010606     D*------------------
005100010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005200010606     D*------------------
005300010606     D WLBDA8          DS                  INZ
005400010606     D  G08DAT                 1      8  0
005500010606     D  G08INV                 9     16  0
005600010606     D  G08ERR                17     17
005700010606     D  G08TGI                18     22  0
005800010606     D*
005900071018     D*-------------------
006000071018     D* COSTANTI
006100071018     D*-------------------
006200071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006300071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006400990908
006500000913     C                   reset                   rrnum
006600990921     C                   reset                   esito
006700990921     C                   reset                   wrkesito
006800010530     C*
006900010530     C                   exsr      opeini
007000010530     C                   exsr      rwvab
007100041025     C*
007200041025     C* Esegue lancio TISI95R solo x chiusura
007300041025     C                   CLEAR                   TISI95DS
007400041025     C                   EVAL      I95TLA = 'C'
007500041025     C                   CALL      'TISI95R'
007600041025     C                   PARM                    TISI95DS
007700010530     C*
007800010530     C                   seton                                        lr
007900010530
008000010530
008100010530
008200010530
008300010530     C*--------------------------------------------------------
008400010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008500010530     C*--------------------------------------------------------
008600010530     C     PREELA        BEGSR
008700000724     C*
008800000724     C* SE OCCORRE SPEDIRE IN FILIALE
008900010530     C                   if        invfil <> *zeros and
009000010530     C                             flgGiro = '0'
009100010530     C*
009200010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009300010530     C                   eval      flgGiro = '1'
009400010530     C*
009500010530     C                   endif
009600010530     C*
009700010530     C                   ENDSR
009800010530     C***
009900010530
010000010530
010100010530
010200010530
010300010530
010400010530
010500010530     C*--------------------------------------------------------
010600010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010700010530     C*--------------------------------------------------------
010800010530     C     ENDELA        BEGSR
010900010530     C*
011000010530     C                   ENDSR
011100010530     C***
011200990908
011300010530
011400010530
011500010530
011600910830     C*--------------------------------------------------------
011700030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
011800910830     C*--------------------------------------------------------
011900941229     C     RWVAB         BEGSR
012000030710     C*
012100030710     C                   clear                   §CTROK
012200030710     C                   clear                   §CTRMO
012300030710     C                   clear                   §CTRNO
012400990910     C*
012500030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
012600030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
012700030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
012800030710     C                   if        *in60 = *on
012900990914     C                   if        not %open(tivin00r)
013000990908     C                   open      tivin00r
013100990914     C                   endif
013200030616     C                   if        not %open(edivabwr)
013300030616     C                   open      edivabwr
013400990914     C                   endif
013500140114     C*
013600140114     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
013700140114     C                   if        *in53
013800140114     C*
013900140114     C* Eseguo operazioni iniziali per scrittura file EDIVAD
014000140114     C                   call      'TIS7WB2ER'
014100140114     C                   parm      'O'           pIn_Opz           1
014200140114     C                   parm                    tivlrds
014300140114     C                   parm                    EDIVADDS
014400140114     C                   parm      *blanks       pOut_Esito        1
014500140114     C*
014600140113     C                   call      'TIS7P2SER'
014700140113     C                   parm      'O'           pIn_Opz           1
014800140113     C                   parm                    tivlrds
014900140113     C                   parm                    EDIVABDS
015000140113     C                   parm      *blanks       pIn_CdIdAz        2
015100140113     C                   parm      *blanks       pIn_MaskPDF      50
015200140113     C                   parm      *blanks       pOut_Esito        1
015300140113     C                   endif
015400990910     C*
015500921023     C                   DO        *HIVAL
015600990913     C*
015700990915     C                   READ      tivin00r                               70
015800071018     C*
015900071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016000071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016100010618     C*
016200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016300010618     C                   if        vinflg = '1'
016400010618     C                   eval      flgOk = '1'
016500010618     C                   endif
016600010705     C*
016700010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
016800010705     C                   if        %subst(vindta:1:1) = '='
016900010705     C                   eval      %subst(vindta:1:1) = *blanks
017000010705     C                   endif
017100010705     C*
017200000905     C                   if        vindta > *blanks
017300000613     C                   add       1             rrnum
017400010601     C* ...elimino eventuali segni
017500010601     C     '+':' '       xlate     vindta        vindta
017600990913     C*
017700010601     C                   if        *in70 = *off and
017800990913     C                             (vinflg = *blanks
017900010711     C                              or vinflg = '0'
018000010711     C                              or vinflg = '2')
018100990913     C*
018200030616     C                   clear                   EDIVAB00
018300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018400010711     C                   if        vinflg = *blanks or vinflg = '0'
018500010711     C                   clear                   vinmsg
018600010711     C                   endif
018700010711     C*
018800990913     C                   eval      fnvabaar = vindta
018900991022     C*
019000991022     C                   testb     x'3F'         vhbatb                   02
019100991022     C                   if        not *in02
019200991022     C*
019300971021     C                   SETOFF                                       303132
019400971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
019500971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
019600971023     C*    OPPURE TUTTI BLANKS
019700010530     C*
019800010530     C                   TESTN                   VHBLNP               30
019900010530     C  N30              TESTN                   VHBLNP                 30
020000010530     C  N30              TESTN                   VHBLNP                   30
020100010530     C                   MOVE      VHBLNP        W001A             1
020200010530     C     *IN30         IFEQ      *OFF
020300010530     C     W001A         ORLT      '0'
020400010530     C                   SETON                                        31
020500010530     C                   eval      vinmsg = %trimr(vinmsg)
020600010530     C                             + ' ' + 'VHBLNP'
020700010530     C                   ELSE
020800010530     C                   MOVE      VHBLNP        VABLNP
020900010530     C     VABLNP        IFLE      *ZEROS
021000010530     C                   SETON                                        31
021100010530     C                   eval      vinmsg = %trimr(vinmsg)
021200010530     C                             + ' ' + 'VHBLNP'
021300010530     C                   ENDIF
021400010530     C                   ENDIF
021500010530     C*
021600010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021700010530     C                   eval      depfil = VABLNP
021800010530     C                   exsr      repfil
021900010601     C                   if        depfil = invfil
022000021014     C                   if        vlrpoi = 999
022100021010     C                   MOVE(P)   invfil        VABFGS
022200021014     C                   else
022300021014     C                   MOVE(P)   vlrpoi        VABFGS
022400021014     C                   endif
022500010530     C*
022600010530     C                   exsr      PREELA
022700010530     C*
022800971023     C                   TESTN                   VHBCCM               30
022900971023     C  N30              TESTN                   VHBCCM                 30
023000971023     C  N30              TESTN                   VHBCCM                   30
023100971021     C                   MOVE      VHBCCM        W001A             1
023200971021     C     *IN30         IFEQ      *OFF
023300971021     C     W001A         ORLT      '0'
023400971021     C                   SETON                                        31
023500010604     C                   eval      vinmsg = %trimr(vinmsg)
023600010604     C                             + ' ' + 'VHBCCM'
023700971021     C                   ELSE
023800971021     C                   MOVE      VHBCCM        VABCCM
023900971024     C     VABCCM        IFLE      *ZEROS
024000971024     C                   SETON                                        31
024100010604     C                   eval      vinmsg = %trimr(vinmsg)
024200010604     C                             + ' ' + 'VHBCCM'
024300971024     C                   ENDIF
024400971021     C                   ENDIF
024500971021     C*
024600971023     C                   TESTN                   VHBAAS               30
024700971023     C  N30              TESTN                   VHBAAS                 30
024800971023     C  N30              TESTN                   VHBAAS                   30
024900971021     C                   MOVE      VHBAAS        W001A             1
025000971021     C     *IN30         IFEQ      *OFF
025100971021     C     W001A         ORLT      '0'
025200971023     C     W001A         ANDNE     *BLANK
025300971021     C                   SETON                                        32
025400010606     C                   MOVEL     datcor        VABAAS
025500010604     C                   eval      vinmsg = %trimr(vinmsg)
025600010604     C                             + ' ' + 'VHBAAS'
025700971021     C                   ELSE
025800971021     C                   MOVE      VHBAAS        VABAAS
025900971021     C                   ENDIF
026000971021     C*
026100971023     C                   TESTN                   VHBMGS               30
026200971023     C  N30              TESTN                   VHBMGS                 30
026300971023     C  N30              TESTN                   VHBMGS                   30
026400971021     C                   MOVE      VHBMGS        W001A             1
026500971021     C     *IN30         IFEQ      *OFF
026600971021     C     W001A         ORLT      '0'
026700971023     C     W001A         ANDNE     *BLANK
026800971021     C                   SETON                                        32
026900010601     C                   MOVE      '0101'        VABMGS
027000010604     C                   eval      vinmsg = %trimr(vinmsg)
027100010604     C                             + ' ' + 'VHBMGS'
027200971021     C                   ELSE
027300971021     C                   MOVE      VHBMGS        VABMGS
027400971021     C                   ENDIF
027500971021     C*
027600971023     C                   TESTN                   VHBNRS               30
027700971023     C  N30              TESTN                   VHBNRS                 30
027800971023     C  N30              TESTN                   VHBNRS                   30
027900971021     C                   MOVE      VHBNRS        W001A             1
028000971021     C     *IN30         IFEQ      *OFF
028100971021     C     W001A         ORLT      '0'
028200971023     C     W001A         ANDNE     *BLANK
028300971021     C                   SETON                                        32
028400971021     C                   MOVE      *ZERO         VABNRS
028500010604     C                   eval      vinmsg = %trimr(vinmsg)
028600010604     C                             + ' ' + 'VHBNRS'
028700971021     C                   ELSE
028800971021     C                   MOVE      VHBNRS        VABNRS
028900971021     C                   ENDIF
029000971021     C*
029100971023     C                   TESTN                   VHBNSP               30
029200971023     C  N30              TESTN                   VHBNSP                 30
029300971023     C  N30              TESTN                   VHBNSP                   30
029400971021     C                   MOVE      VHBNSP        W001A             1
029500971021     C     *IN30         IFEQ      *OFF
029600971021     C     W001A         ORLT      '0'
029700971023     C     W001A         ANDNE     *BLANK
029800971021     C                   SETON                                        32
029900971021     C                   MOVE      *ZERO         VABNSP
030000010604     C                   eval      vinmsg = %trimr(vinmsg)
030100010604     C                             + ' ' + 'VHBNSP'
030200971021     C                   ELSE
030300971021     C                   MOVE      VHBNSP        VABNSP
030400971021     C                   ENDIF
030500971021     C*
030600971023     C                   TESTN                   VHBLNA               30
030700971023     C  N30              TESTN                   VHBLNA                 30
030800971023     C  N30              TESTN                   VHBLNA                   30
030900971021     C                   MOVE      VHBLNA        W001A             1
031000971021     C     *IN30         IFEQ      *OFF
031100971021     C     W001A         ORLT      '0'
031200971023     C     W001A         ANDNE     *BLANK
031300971021     C                   SETON                                        32
031400971021     C                   MOVE      *ZERO         VABLNA
031500010604     C                   eval      vinmsg = %trimr(vinmsg)
031600010604     C                             + ' ' + 'VHBLNA'
031700971021     C                   ELSE
031800971021     C                   MOVE      VHBLNA        VABLNA
031900971021     C                   ENDIF
032000971021     C*
032100971023     C                   TESTN                   VHBCTR               30
032200971023     C  N30              TESTN                   VHBCTR                 30
032300971023     C  N30              TESTN                   VHBCTR                   30
032400971021     C                   MOVE      VHBCTR        W001A             1
032500971021     C     *IN30         IFEQ      *OFF
032600971021     C     W001A         ORLT      '0'
032700971023     C     W001A         ANDNE     *BLANK
032800971021     C                   SETON                                        32
032900971021     C                   MOVE      *ZERO         VABCTR
033000010604     C                   eval      vinmsg = %trimr(vinmsg)
033100010604     C                             + ' ' + 'VHBCTR'
033200971021     C                   ELSE
033300971021     C                   MOVE      VHBCTR        VABCTR
033400971021     C                   ENDIF
033500941229     C*
033600971021     C                   MOVE      *ZERO         VABIAS
033700941229     C                   MOVE      VHBIAS        WASC15
033800971023     C                   TESTN                   WI15                 30
033900971023     C  N30              TESTN                   WI15                   30
034000971023     C  N30              TESTN                   WI15                     30
034100971021     C                   MOVE      WI15          W001A             1
034200971021     C     *IN30         IFEQ      *OFF
034300971021     C     W001A         ORLT      '0'
034400971023     C     W001A         ANDNE     *BLANK
034500971021     C                   SETON                                        32
034600971021     C                   MOVE      *ZERO         VABIAS
034700010604     C                   eval      vinmsg = %trimr(vinmsg)
034800010604     C                             + ' ' + 'VHBIAS'
034900971021     C                   ELSE
035000971021     C                   MOVEL     WI15          VABIAS
035100971023     C                   TESTN                   WD15                 30
035200971023     C  N30              TESTN                   WD15                   30
035300971023     C  N30              TESTN                   WD15                     30
035400971021     C                   MOVE      WD15          W001A             1
035500971021     C     *IN30         IFEQ      *OFF
035600971021     C     W001A         ORLT      '0'
035700971023     C     W001A         ANDNE     *BLANK
035800971021     C                   SETON                                        32
035900971021     C                   MOVE      *ZERO         VABIAS
036000010604     C                   eval      vinmsg = %trimr(vinmsg)
036100010604     C                             + ' ' + 'VHBIAS'
036200971021     C                   ELSE
036300971021     C                   MOVE      WD15          VABIAS
036400971021     C                   ENDIF
036500050118     C                   ENDIF
036600941229     C*
036700971023     C                   TESTN                   VHBNCL               30
036800971023     C  N30              TESTN                   VHBNCL                 30
036900971023     C  N30              TESTN                   VHBNCL                   30
037000971021     C                   MOVE      VHBNCL        W001A             1
037100971021     C     *IN30         IFEQ      *OFF
037200971021     C     W001A         ORLT      '0'
037300971021     C                   SETON                                        32
037400971021     C                   MOVE      *ZERO         VABNCL
037500010604     C                   eval      vinmsg = %trimr(vinmsg)
037600010604     C                             + ' ' + 'VHBNCL'
037700971021     C                   ELSE
037800971021     C                   MOVE      VHBNCL        VABNCL
037900971021     C                   ENDIF
038000941229     C*
038100971021     C                   MOVE      *ZERO         VABPKB
038200971021     C                   MOVE      VHBPKB        WASC09
038300010719     C                   IF        WI09 = *blanks
038400010719     C                   EVAL      WI09 = *zeros
038500010719     C                   ENDIF
038600971023     C                   TESTN                   WI09                 30
038700971023     C  N30              TESTN                   WI09                   30
038800971023     C  N30              TESTN                   WI09                     30
038900971021     C                   MOVE      WI09          W001A             1
039000971021     C     *IN30         IFEQ      *OFF
039100971021     C     W001A         ORLT      '0'
039200971021     C                   SETON                                        32
039300971021     C                   MOVE      *ZERO         VABPKB
039400010604     C                   eval      vinmsg = %trimr(vinmsg)
039500010604     C                             + ' ' + 'VHBPKB'
039600971021     C                   ELSE
039700971021     C                   MOVEL     WI09          VABPKB
039800010719     C                   IF        WD09 = *blanks
039900010719     C                   EVAL      WD09 = *zeros
040000010719     C                   ENDIF
040100971023     C                   TESTN                   WD09                 30
040200971023     C  N30              TESTN                   WD09                     30
040300971021     C                   MOVE      WD09          W001A             1
040400971021     C     *IN30         IFEQ      *OFF
040500971021     C     W001A         ORLT      '0'
040600971023     C     W001A         ANDNE     *BLANK
040700971021     C                   SETON                                        32
040800971021     C                   MOVE      *ZERO         VABPKB
040900010604     C                   eval      vinmsg = %trimr(vinmsg)
041000010604     C                             + ' ' + 'VHBPKB'
041100971021     C                   ELSE
041200971021     C                   MOVE      WD09          VABPKB
041300971021     C                   ENDIF
041400050118     C                   ENDIF
041500971021     C*
041600041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
041700041001     C                   IF        *IN51 = *OFF
041800971021     C                   MOVE      *ZERO         VABVLB
041900971021     C                   MOVE      VHBVLB        WASC07
042000010719     C                   IF        WI07 = *blanks
042100010719     C                   EVAL      WI07 = *zeros
042200010719     C                   ENDIF
042300971023     C                   TESTN                   WI07                 30
042400971023     C  N30              TESTN                   WI07                   30
042500971023     C  N30              TESTN                   WI07                     30
042600971021     C                   MOVE      WI07          W001A             1
042700971021     C     *IN30         IFEQ      *OFF
042800971021     C     W001A         ORLT      '0'
042900971023     C     W001A         ANDNE     *BLANK
043000971021     C                   SETON                                        32
043100971021     C                   MOVE      *ZERO         VABVLB
043200010604     C                   eval      vinmsg = %trimr(vinmsg)
043300010604     C                             + ' ' + 'VHBVLB'
043400971021     C                   ELSE
043500971021     C                   MOVEL     WI07          VABVLB
043600010719     C                   IF        WD07 = *blanks
043700010719     C                   EVAL      WD07 = *zeros
043800010719     C                   ENDIF
043900971023     C                   TESTN                   WD07                 30
044000971023     C  N30              TESTN                   WD07                   30
044100971023     C  N30              TESTN                   WD07                     30
044200971021     C                   MOVE      WD07          W001A             1
044300971021     C     *IN30         IFEQ      *OFF
044400971021     C     W001A         ORLT      '0'
044500971023     C     W001A         ANDNE     *BLANK
044600971021     C                   SETON                                        32
044700971021     C                   MOVE      *ZERO         VABVLB
044800010604     C                   eval      vinmsg = %trimr(vinmsg)
044900010604     C                             + ' ' + 'VHBVLB'
045000971021     C                   ELSE
045100971021     C                   MOVE      WD07          VABVLB
045200971021     C                   ENDIF
045300050118     C                   ENDIF
045400041001     C                   ELSE
045500041001     C* ...altrimenti forzo azzeramento del volume
045600041001     C                   Z-ADD     *zeros        VABVLB
045700041001     C                   ENDIF
045800941229     C*
045900971021     C                   MOVE      *ZERO         VABQFT
046000971021     C                   MOVE      VHBQFT        WASC15
046100010719     C                   IF        WI15 = *blanks
046200010719     C                   EVAL      WI15 = *zeros
046300010719     C                   ENDIF
046400971023     C                   TESTN                   WI15                 30
046500971023     C  N30              TESTN                   WI15                   30
046600971023     C  N30              TESTN                   WI15                     30
046700971021     C                   MOVE      WI15          W001A             1
046800971021     C     *IN30         IFEQ      *OFF
046900971021     C     W001A         ORLT      '0'
047000971023     C     W001A         ANDNE     *BLANK
047100971021     C                   SETON                                        32
047200971021     C                   MOVE      *ZERO         VABQFT
047300010604     C                   eval      vinmsg = %trimr(vinmsg)
047400010604     C                             + ' ' + 'VHBQFT'
047500971021     C                   ELSE
047600971021     C                   MOVEL     WI15          VABQFT
047700010719     C                   IF        WD15 = *blanks
047800010719     C                   EVAL      WD15 = *zeros
047900010719     C                   ENDIF
048000971023     C                   TESTN                   WD15                 30
048100971023     C  N30              TESTN                   WD15                   30
048200971023     C  N30              TESTN                   WD15                     30
048300971021     C                   MOVE      WD15          W001A             1
048400971021     C     *IN30         IFEQ      *OFF
048500971021     C     W001A         ORLT      '0'
048600971023     C     W001A         ANDNE     *BLANK
048700971021     C                   SETON                                        32
048800971021     C                   MOVE      *ZERO         VABQFT
048900010604     C                   eval      vinmsg = %trimr(vinmsg)
049000010604     C                             + ' ' + 'VHBQFT'
049100971021     C                   ELSE
049200971021     C                   MOVE      WD15          VABQFT
049300971021     C                   ENDIF
049400050118     C                   ENDIF
049500941229     C*
049600971021     C                   MOVE      *ZERO         VABCAS
049700971021     C                   MOVE      VHBCAS        WASC15
049800010719     C                   IF        WI15 = *blanks
049900010719     C                   EVAL      WI15 = *zeros
050000010719     C                   ENDIF
050100971023     C                   TESTN                   WI15                 30
050200971023     C  N30              TESTN                   WI15                   30
050300971023     C  N30              TESTN                   WI15                     30
050400971021     C                   MOVE      WI15          W001A             1
050500971021     C     *IN30         IFEQ      *OFF
050600971021     C     W001A         ORLT      '0'
050700971023     C     W001A         ANDNE     *BLANK
050800971021     C                   SETON                                        32
050900971021     C                   MOVE      *ZERO         VABCAS
051000010604     C                   eval      vinmsg = %trimr(vinmsg)
051100010604     C                             + ' ' + 'VHBCAS'
051200971021     C                   ELSE
051300971021     C                   MOVEL     WI15          VABCAS
051400010719     C                   IF        WD15 = *blanks
051500010719     C                   EVAL      WD15 = *zeros
051600010719     C                   ENDIF
051700971023     C                   TESTN                   WD15                 30
051800971023     C  N30              TESTN                   WD15                   30
051900971023     C  N30              TESTN                   WD15                     30
052000971021     C                   MOVE      WD15          W001A             1
052100971021     C     *IN30         IFEQ      *OFF
052200971021     C     W001A         ORLT      '0'
052300971023     C     W001A         ANDNE     *BLANK
052400971021     C                   SETON                                        32
052500971021     C                   MOVE      *ZERO         VABCAS
052600010604     C                   eval      vinmsg = %trimr(vinmsg)
052700010604     C                             + ' ' + 'VHBCAS'
052800971021     C                   ELSE
052900971021     C                   MOVE      WD15          VABCAS
053000971021     C                   ENDIF
053100050118     C                   ENDIF
053200971021     C*
053300971023     C                   TESTN                   VHBRMN               30
053400971023     C  N30              TESTN                   VHBRMN                 30
053500971023     C  N30              TESTN                   VHBRMN                   30
053600971021     C                   MOVE      VHBRMN        W001A             1
053700971021     C     *IN30         IFEQ      *OFF
053800971021     C     W001A         ORLT      '0'
053900971021     C                   SETON                                        32
054000971021     C                   MOVE      *ZERO         VABRMN
054100010604     C                   eval      vinmsg = %trimr(vinmsg)
054200010604     C                             + ' ' + 'VHBRMN'
054300971021     C                   ELSE
054400971021     C                   MOVE      VHBRMN        VABRMN
054500971021     C                   ENDIF
054600971021     C*
054700971023     C                   TESTN                   VHBNCD               30
054800971023     C  N30              TESTN                   VHBNCD                 30
054900971023     C  N30              TESTN                   VHBNCD                   30
055000971021     C                   MOVE      VHBNCD        W001A             1
055100971021     C     *IN30         IFEQ      *OFF
055200971021     C     W001A         ORLT      '0'
055300971023     C     W001A         ANDNE     *BLANK
055400971021     C                   SETON                                        32
055500971021     C                   MOVE      *ZERO         VABNCD
055600010604     C                   eval      vinmsg = %trimr(vinmsg)
055700010604     C                             + ' ' + 'VHBNCD'
055800971021     C                   ELSE
055900971021     C                   MOVE      VHBNCD        VABNCD
056000971021     C                   ENDIF
056100971021     C*
056200971023     C                   TESTN                   VHBNCA               30
056300971023     C  N30              TESTN                   VHBNCA                 30
056400971023     C  N30              TESTN                   VHBNCA                   30
056500971021     C                   MOVE      VHBNCA        W001A             1
056600971021     C     *IN30         IFEQ      *OFF
056700971021     C     W001A         ORLT      '0'
056800971023     C     W001A         ANDNE     *BLANK
056900971021     C                   SETON                                        32
057000971021     C                   MOVE      *ZERO         VABNCA
057100010604     C                   eval      vinmsg = %trimr(vinmsg)
057200010604     C                             + ' ' + 'VHBNCA'
057300971021     C                   ELSE
057400971021     C                   MOVE      VHBNCA        VABNCA
057500971021     C                   ENDIF
057600971021     C*
057700971023     C                   TESTN                   VHBZNC               30
057800971023     C  N30              TESTN                   VHBZNC                 30
057900971023     C  N30              TESTN                   VHBZNC                   30
058000971021     C                   MOVE      VHBZNC        W001A             1
058100971021     C     *IN30         IFEQ      *OFF
058200971021     C     W001A         ORLT      '0'
058300971023     C     W001A         ANDNE     *BLANK
058400971021     C                   SETON                                        32
058500971021     C                   MOVE      *ZERO         VABZNC
058600010604     C                   eval      vinmsg = %trimr(vinmsg)
058700010604     C                             + ' ' + 'VHBZNC'
058800971021     C                   ELSE
058900971021     C                   MOVE      VHBZNC        VABZNC
059000971021     C                   ENDIF
059100971021     C*
059200971023     C                   TESTN                   VHBDCR               30
059300971023     C  N30              TESTN                   VHBDCR                 30
059400971023     C  N30              TESTN                   VHBDCR                   30
059500971021     C                   MOVE      VHBDCR        W001A             1
059600971021     C     *IN30         IFEQ      *OFF
059700971021     C     W001A         ORLT      '0'
059800971023     C     W001A         ANDNE     *BLANK
059900971021     C                   SETON                                        32
060000971021     C                   MOVE      *ZERO         VABDCR
060100010604     C                   eval      vinmsg = %trimr(vinmsg)
060200010604     C                             + ' ' + 'VHBDCR'
060300971021     C                   ELSE
060400971021     C                   MOVE      VHBDCR        VABDCR
060500971021     C                   ENDIF
060600971021     C*
060700971023     C                   TESTN                   VHBHCR               30
060800971023     C  N30              TESTN                   VHBHCR                 30
060900971023     C  N30              TESTN                   VHBHCR                   30
061000971021     C                   MOVE      VHBHCR        W001A             1
061100971021     C     *IN30         IFEQ      *OFF
061200971021     C     W001A         ORLT      '0'
061300971023     C     W001A         ANDNE     *BLANK
061400971021     C                   SETON                                        32
061500971021     C                   MOVE      *ZERO         VABHCR
061600010604     C                   eval      vinmsg = %trimr(vinmsg)
061700010604     C                             + ' ' + 'VHBHCR'
061800971021     C                   ELSE
061900971021     C                   MOVE      VHBHCR        VABHCR
062000971021     C                   ENDIF
062100941229     C*
062200971021     C                   MOVE      *ZERO         VABVMD
062300971021     C                   MOVE      VHBVMD        WASC15
062400010719     C                   IF        WI15 = *blanks
062500010719     C                   EVAL      WI15 = *zeros
062600010719     C                   ENDIF
062700971023     C                   TESTN                   WI15                 30
062800971023     C  N30              TESTN                   WI15                   30
062900971023     C  N30              TESTN                   WI15                     30
063000971021     C                   MOVE      WI15          W001A             1
063100971021     C     *IN30         IFEQ      *OFF
063200971021     C     W001A         ORLT      '0'
063300971023     C     W001A         ANDNE     *BLANK
063400971021     C                   SETON                                        32
063500971021     C                   MOVE      *ZERO         VABVMD
063600010604     C                   eval      vinmsg = %trimr(vinmsg)
063700010604     C                             + ' ' + 'VHBVMD'
063800971021     C                   ELSE
063900971021     C                   MOVEL     WI15          VABVMD
064000010719     C                   IF        WD15 = *blanks
064100010719     C                   EVAL      WD15 = *zeros
064200010719     C                   ENDIF
064300971023     C                   TESTN                   WD15                 30
064400971023     C  N30              TESTN                   WD15                   30
064500971023     C  N30              TESTN                   WD15                     30
064600971021     C                   MOVE      WD15          W001A             1
064700971021     C     *IN30         IFEQ      *OFF
064800971021     C     W001A         ORLT      '0'
064900971023     C     W001A         ANDNE     *BLANK
065000971021     C                   SETON                                        32
065100971021     C                   MOVE      *ZERO         VABVMD
065200010604     C                   eval      vinmsg = %trimr(vinmsg)
065300010604     C                             + ' ' + 'VHBVMD'
065400971021     C                   ELSE
065500971021     C                   MOVE      WD15          VABVMD
065600971021     C                   ENDIF
065700050118     C                   ENDIF
065800971021     C*
065900971023     C                   TESTN                   VHBANT               30
066000971023     C  N30              TESTN                   VHBANT                 30
066100971023     C  N30              TESTN                   VHBANT                   30
066200971021     C                   MOVE      VHBANT        W001A             1
066300971021     C     *IN30         IFEQ      *OFF
066400971021     C     W001A         ORLT      '0'
066500971023     C     W001A         ANDNE     *BLANK
066600971021     C                   SETON                                        32
066700971021     C                   MOVE      *ZERO         VABANT
066800010604     C                   eval      vinmsg = %trimr(vinmsg)
066900010604     C                             + ' ' + 'VHBANT'
067000971021     C                   ELSE
067100971021     C                   MOVE      VHBANT        VABANT
067200971021     C                   ENDIF
067300971021     C*
067400971021     C* CAMPI ALFABETICI:
067500971021     C                   MOVE      VHBATB        VABATB
067600971021     C                   MOVE      VHBCBO        VABCBO
067700971021     C                   MOVE      VHBRSD        VABRSD
067800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
067900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
068000020117     C* ==
068100971021     C                   MOVE      VHBRD2        VABRD2
068200971021     C                   MOVE      VHBIND        VABIND
068300971021     C                   MOVE      VHBCAD        VABCAD
068400971021     C                   MOVE      VHBLOD        VABLOD
068500971021     C                   MOVE      VHBPRD        VABPRD
068600971021     C                   MOVE      VHBNZD        VABNZD
068700971021     C                   MOVE      VHBGC1        VABGC1
068800971021     C                   MOVE      VHBGC2        VABGC2
068900971021     C                   MOVE      VHBTSP        VABTSP
069000971021     C                   MOVE      VHBVAS        VABVAS
069100971021     C                   MOVE      VHBNAS        VABNAS
069200971021     C                   MOVE      VHBTIC        VABTIC
069300971021     C                   MOVE      VHBVCA        VABVCA
069400971021     C                   MOVE      VHBGCA        VABGCA
069500971021     C                   MOVE      VHBRMA        VABRMA
069600971021     C                   MOVE      VHBXCO        VABXCO
069700971021     C                   MOVE      VHBNOT        VABNOT
069800971021     C                   MOVE      VHBNT2        VABNT2
069900971021     C                   MOVE      VHBCTM        VABCTM
070000971021     C                   MOVE      VHBFFD        VABFFD
070100971021     C                   MOVE      VHBTCR        VABTCR
070200971021     C                   MOVE      VHBCTS        VABCTS
070300971021     C                   MOVE      VHBFTM        VABFTM
070400971021     C                   MOVE      VHBVAD        VABVAD
070500971021     C                   MOVE      VHBGMA        VABGMA
070600971021     C                   MOVE      VHBGGA        VABGGA
070700971021     C                   MOVE      VHBGVA        VABGVA
070800971021     C                   MOVE      VHBTC1        VABTC1
070900971021     C                   MOVE      VHBTC2        VABTC2
071000971021     C                   MOVE      VHBSCL        VABSCL
071100971021     C                   MOVE      VHBRMO        VABRMO
071200971021     C                   MOVE      VHBCMO        VABCMO
071300971021     C                   MOVE      VHBNMO        VABNMO
071400971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
071500971021     C* NELLA NAZIONE ORIGINALE
071600971021     C                   MOVE      VHBNMO        WA1D              1
071700971021     C                   MOVEL     VHBNMO        WA1S              1
071800971021     C     WA1D          IFNE      *BLANK
071900971021     C     WA1S          ANDEQ     *BLANK
072000971021     C                   MOVE      *BLANK        VABNMO
072100030616     C                   ENDIF
072200030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
072300030710     C                   EVAL      VABCMR = wCMR
072400030616     C                   EVAL      VABDCM = DATCOR
072500030616     C                   EVAL      VABDTS = DATCOR
072600040402     C                   EVAL      VABHMS = ORACOR
072700030616     C                   EVAL      VABCNT = 1
072800130328     C*
072900130328     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
073000130328     C                   IF        %trim(VABNZD) = 'I'   OR
073100130328     C                             %trim(VABNZD) = 'IT'  OR
073200130328     C                             %trim(VABNZD) = 'ITA'
073300130328     C                   EVAL      VABNZD = *blanks
073400130328     C                   ENDIF
073500041025     C*
073600041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
073700041025     C                   IF        *IN52 = *ON
073800041025     C* Reperisco la provincia dal CAP e dalla località
073900041025     C                   IF        VABCAD <> *blanks AND
074000050907     C                             VABPRD  = *blanks AND
074100050907     C                             VABNZD  = *blanks
074200041025     C                   CLEAR                   TISI95DS
074300041025     C                   EVAL      I95TCN = '3'
074400041025     C                   Z-ADD     datcor        I95DAT
074500041025     C                   EVAL      I95CAP = VABCAD
074600041025     C                   EVAL      I95LOC = VABLOD
074700041025     C                   CALL      'TISI95R'
074800041025     C                   PARM                    TISI95DS
074900041025     C                   EVAL      VABPRD = O95PRV
075000041025     C                   ENDIF
075100041025     C                   ENDIF
075200100419     C*
075300100419     C* Forzatura su campi DIVISE
075400100419     C                   IF        VABVAS = *blanks
075500100419     C                   EVAL      VABVAS = 'EUR'
075600100419     C                   ENDIF
075700100419     C                   IF        VABVCA = *blanks
075800100419     C                   EVAL      VABVCA = 'EUR'
075900100419     C                   ENDIF
076000100419     C                   IF        VABVAD = *blanks
076100100419     C                   EVAL      VABVAD = 'EUR'
076200100419     C                   ENDIF
076300140114     C*
076400140114     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
076500140114     C                   if        *in53
076600140114     C*
076700140114     C* Eseguo scrittura file EDIVAD
076800140114     C                   eval      VABCTM = '2D'
076900140114     C                   exsr      valVAD
077000140114     C                   call      'TIS7WB2ER'
077100140114     C                   parm      'W'           pIn_Opz           1
077200140114     C                   parm                    tivlrds
077300140114     C                   parm                    EDIVADDS
077400140114     C                   parm      *blanks       pOut_Esito        1
077500140114     C*
077600140113     C                   if        not *in31
077700140113     C*
077800140113     C* Se per spedizione richiesta gestione packing-list PDF
077900140114     C                   if        (%len(%trim(%editc(vabRMN:'4'))) >= 7 AND
078000140114     C                              %subst(%trim(%editc(vabRMN:'4')):1:7)
078100140114     C                              <> '9999999')    OR
078200140114     C                             (%len(%trim(%editc(vabRMN:'4'))) < 7)
078300140113     C*
078400140121     C***                eval      pIn_MaskPDF = 'FT&0RMN*.&AAS4*.pdf'
078500140121     C                   eval      pIn_MaskPDF = '&0RMN*.pdf'
078600140113     C                   call      'TIS7P2SER'
078700140113     C                   parm      'W'           pIn_Opz           1
078800140113     C                   parm                    tivlrds
078900140113     C                   parm                    EDIVABDS
079000140113     C                   parm      '10'          pIn_CdIdAz        2
079100140113     C                   parm                    pIn_MaskPDF      50
079200140113     C                   parm      *blanks       pOut_Esito        1
079300140113     C*
079400140113     C* Forzature:
079500140113     C*
079600140113     C* CTM
079700140113     C                   if        wCTM <> *blanks
079800140113     C                   eval      VABCTM = wCTM
079900140113     C                   endif
080000140113     C*
080100140113     C                   endif
080200140113     C*
080300140113     C                   endif
080400140113     C                   endif
080500140113     C*
080600930409     C*
080700010604     C  N31              ADD       1             §CTROK            7 0
080800010604     C   32              ADD       1             §CTRMO            7 0
080900010604     C   31              ADD       1             §CTRNO            7 0
081000030616     C  N31              WRITE     EDIVAB00
081100990910     C*
081200010601     C                   if        *in31 = *off and
081300010601     C                             *in32 = *off
081400990910     C                   eval      vinflg = '1'
081500990910     C                   else
081600990910     C                   eval      vinflg = '2'
081700990910     C                   endif
081800991022     C                   endif
081900990910     C                   endif
082000010601     C*
082100010601     C                   endif
082200000905     C*
082300000905     C                   else
082400000905     C                   eval      vinflg = '1'
082500000905     C                   endif
082600000905     C*
082700000905     C  N70              update    tivin000
082800991022     C*
082900991022     C  N70              ENDdo
083000010530     C
083100140114     C*
083200140114     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
083300140114     C                   if        *in53
083400140114     C*
083500140114     C* Eseguo operazioni finali dopo scrittura file EDIVAD
083600140114     C                   call      'TIS7WB2ER'
083700140114     C                   parm      'C'           pIn_Opz           1
083800140114     C                   parm                    tivlrds
083900140114     C                   parm                    EDIVADDS
084000140114     C                   parm      *blanks       pOut_Esito        1
084100140114     C*
084200140113     C                   call      'TIS7P2SER'
084300140113     C                   parm      'C'           pIn_Opz           1
084400140113     C                   parm                    tivlrds
084500140113     C                   parm                    EDIVABDS
084600140113     C                   parm      *blanks       pIn_CdIdAz        2
084700140113     C                   parm      *blanks       pIn_MaskPDF      50
084800140113     C                   parm      *blanks       pOut_Esito        1
084900140113     C                   endif
085000010530     C*
085100010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
085200010530     C                   if        cntNonEl = *zeros or
085300010530     C                             flgMulti = '0'
085400990910     C* Se non ci sono record con errori ...
085500010530     C                   if        §ctrno = 0 and
085600010601     C                             §ctrmo = 0 and
085700010530     C                             flgStato <> '2'
085800990910     C* ... restituisco esito OK.
085900990921     C                   eval      wrkesito = '0'
086000990910     C                   else
086100010601     C                   if        §ctrok > 0
086200990921     C                   eval      wrkesito = '1'
086300000710     C                   else
086400010615     C                   if        flgOk = '0'
086500010615     C                   eval      wrkesito = '2'
086600010615     C                   else
086700010615     C                   eval      wrkesito = '6'
086800010615     C                   endif
086900990910     C                   endif
087000000710     C                   endif
087100010530     C                   else
087200010530     C                   eval      wrkesito = '9'
087300010530     C                   endif
087400030710     C*
087500030710 X  1C                   else
087600030710     C                   eval      wrkesito = '8'
087700030710     C                   eval      flgGiro = '1'
087800030710 E  1C                   endif
087900030710     C*
088000990914     C                   if        %open(tivin00r)
088100990908     C                   close     tivin00r
088200990914     C                   endif
088300030616     C                   if        %open(edivabwr)
088400030616     C                   close     edivabwr
088500990914     C                   endif
088600010530     C*
088700010530     C                   if        vlrpoi <> 999
088800010530     C                   eval      invfil = vlrpoi
088900010530     C                   endif
089000990910     C*
089100990920     C                   if        §ctrok > 0
089200010530     C                             and invfil > *zeros
089300000613     C                   exsr      invio
089400990920     C                   endif
089500010612     C*
089600010612     C                   if        flgGiro = '1'
089700010612     C                   exsr      endela
089800010612     C                   endif
089900990920     C*
090000910830     C                   ENDSR
090100000613     C***
090200140114
090300140114
090400140114
090500140114     C     valVAD        BEGSR
090600140114     C*
090700140114     C                   clear                   edivadds
090800140114     C                   eval      VADATB = VABATB
090900140114     C                   eval      VADFGS = VABFGS
091000140114     C                   eval      VADCCM = VABCCM
091100140114     C                   eval      VADLNP = VABLNP
091200140114     C                   eval      VADAAS = VABAAS
091300140114     C                   eval      VADNRS = VABNRS
091400140114     C                   eval      VADNSP = VABNSP
091500140114     C                   eval      VADCDP = VABRMA
091600140114     C                   eval      VADNCD = VABNCD
091700140114     C                   eval      VADNCA = VABNCA
091800140114     C                   eval      VADNCL = VABNCL
091900140114     C                   eval      VADCMR = VABCMR
092000140114     C                   eval      VADCNT = VABCNT
092100140114     C*
092200140114     C                   ENDSR
092300140114     C***
092400010530
092500010530
092600010530
092700010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
092800010530     C     repfil        BEGSR
092900010530     C*
093000010530     C                   if        invfil = *zeros and
093100010530     C                             depfil > *zeros and
093200010613     C                             (vinflg = *blanks or
093300010613     C                              vinflg = *zeros)
093400010530     C
093500010530     C                   eval      invfil = depfil
093600010530     C                   endif
093700010530     C*
093800010530     C                   if        depfil <> invfil and
093900010530     C                             invfil > *zeros
094000010530     C                   eval      flgMulti = '1'
094100010530     C                   if        vinflg = *blanks
094200010530     C                   add       1             cntNonEl
094300010530     C                   endif
094400010530     C                   endif
094500010530     C*
094600010530     C                   if        vinflg = '2'
094700010530     C                   eval      flgStato = '2'
094800010530     C                   endif
094900010530     C*
095000010530     C                   ENDSR
095100010530     C***
095200010530
095300010530
095400010530
095500010530
095600990920      /TITLE Invio dei dati al punto operativo.
095700000613     C     invio         BEGSR
095800990920     C*
095900990920     C                   reset                   dscmz
096000030618     C                   move      invfil        cmzdst
096100030618     C***                move      '789'         cmzdst
096200990920     C                   eval      cmzfld = vlrfou
096300990920     C                   eval      cmzmbd = vlrhdl
096400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
096500000710     C                   if        prmfir = *blanks
096600030616     C                   eval      cmzfla = 'EDIVAB0F'
096700030616     C                   eval      cmzmba = 'EDIVAB0F'
096800000710     C                   else
096900000710     C                   eval      cmzfla = prmfir
097000000710     C                   eval      cmzmba = prmfir
097100000710     C                   endif
097200990920     C                   eval      cmznrr = *zeros
097300990920     C                   move      §ctrok        cmznrr
097400021003     C                   eval      cmzlba = vlrfl1
097500990920     C                   call(e)   'TIS711C'
097600990920     C                   parm                    dscmz
097700990921     C                   parm      *blanks       esito
097800990920     C                   if        %error
097900990920     C                             or cmzerr = '1'
098000990921     C                             or esito  = '1'
098100000710     C                   eval      wrkesito = '3'
098200990920     C                   endif
098300990920     C*
098400000613     C                   ENDSR
098500000613     C***
098600010530
098700010530
098800010530
098900010530
099000010530      /TITLE Invio dei dati al punto operativo.
099100010530     C     opeini        BEGSR
099200010530     C*
099300010530     C* Inizializzo flag e contatori operativi
099400010530     C                   movel     '0'           flgGiro           1
099500010530     C                   movel     '0'           flgMulti          1
099600010530     C                   movel     '1'           flgStato          1
099700010615     C                   movel     '0'           flgOk             1
099800010530     C                   z-add     *zeros        cntNonEl         10 0
099900010530     C                   z-add     *zeros        depfil            3 0
100000010530     C                   z-add     *zeros        invfil            3 0
100100010530     C*
100200010530     C                   ENDSR
100300010530     C***
100400010530
100500010530
100600010530
100700990910
100800000613     C     *inzsr        BEGSR
100900990910     C*
101000990910     C     *entry        plist
101100990920     C                   parm                    tivlrds
101200990921     C                   parm      wrkesito      esito
101300000724     C                   parm                    prmlit
101400000710     C                   parm                    prmfir
101500140113     C*
101600140113     C* Verifico se sui parametri del traduttore è richiesto la scrittura forzata del
101700140113     C* tipo record 'P' del VAT (packing list PDF)
101800140113     C                   if        %subst(vlrppt:8:1) = 'S'
101900140113     C                   seton                                        53
102000140113     C                   else
102100140113     C                   setoff                                       53
102200140113     C                   endif
102300140113     C*
102400140113     C* Verifico se sui parametri del traduttore è richiesto un particolare codice trattamento merce
102500140113     C                   if        %subst(vlrppt:9:2) <> *blanks
102600140113     C                   movel     *blanks       wCTM              2
102700140113     C                   eval      wCTM = %subst(vlrppt:9:2)
102800140113     C                   endif
102900030616     C*
103000030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
103100030710     C                   move(p)   vlrMSG        wNomeFile
103200030710     C*
103300030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
103400030710     C* del codice CMR dal nome del file importato.
103500030710     C                   if        wNomeFile <> *blanks AND
103600030710     C                             vlrppt<>*blanks
103700041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
103800030710     C                   if        %subst(vlrppt:1:1) = 'S'
103900030710     C                   seton                                        50
104000030710     C                   else
104100030710     C                   setoff                                       50
104200030710     C                   endif
104300041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
104400041001     C                   if        %subst(vlrppt:6:1) = 'S'
104500041001     C                   seton                                        51
104600041001     C                   else
104700041001     C                   setoff                                       51
104800041001     C                   endif
104900041025     C*
105000041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
105100041025     C* (naturalmente solo se nn già presente)
105200041025     C                   if        %subst(vlrppt:7:1) = 'S'
105300041025     C                   seton                                        52
105400041025     C                   else
105500041025     C                   setoff                                       52
105600041025     C                   endif
105700041001     C*
105800030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
105900030710     C                   move(p)   wPosDaA       wPosDa
106000030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
106100030710     C                   move(p)   wLungA        wLung
106200030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
106300030710     C                   seton                                        60
106400030710     C                   else
106500050914     C                   if        vlrisv = 'UL'
106600050914     C                   movel     datcor        wDataOraAlfa
106700050914     C                   move      oracor        wDataOraAlfa
106800050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
106900050914     C                   seton                                        60
107000050914     C                   else
107100050914     C                   setoff                                       60
107200050914     C                   endif
107300030710     C                   endif
107400010606     C*
107500010606     C* CALCOLA LA DATA CORRENTE
107600010606     C                   time                    wn14             14 0
107700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
107800040402     C                   movel     wn14          oracor            6 0          *ORA
107900010606     C                   z-add     wn8           g08dat
108000010606     C                   z-add     *zeros        g08inv
108100010606     C                   movel     '0'           g08err
108200010606     C                   call      'XSRDA8'
108300010606     C                   parm                    wlbda8
108400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
108500000613     C*
108600000613     C                   ENDSR
108700000613     C***
