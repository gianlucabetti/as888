000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200130115     H dftactgrp(*no) actgrp(*caller)
000300990908
000400040421     Ftntbe01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030616     FEDIVABWR  O    E             DISK    usropn
000700990908
000800000512     D*------------
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
003300040421     D kpjba         e ds                  inz
003400040421     D fivabds       e ds                  extname(fivab00f)
003500990920     D tivlrds       e ds                  extname(tivlr00f)
003600041025     D tisi95ds      e ds
003700990910     D fnvabaar      e ds                  inz
003800040421     D dscmz         e ds                  inz
003900040421     D dosr          e ds                  inz
004000080502     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
004100080502     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
004200080502     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
004300080502     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
004400080502     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
004500080502     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
004600080502     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
004700080502     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
004800080502     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
004900080502     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
005000080502     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
005100990910     D esito           s              1
005200000724     D prmlit          s             10
005300000710     D prmfir          s             10
005400990921     D wrkesito        s                   like(esito)
005500000613     D rrnum           s              6  0 INZ(*zeros)
005600030616     D wNomeFile       s             30    INZ(*blanks)
005700030710     D wCMR            s             35    INZ(*blanks)
005800030710     D wPosDaA         s              2    INZ(*blanks)
005900030710     D wPosDa          s              2  0 INZ(*zeros)
006000030710     D wLungA          s              2    INZ(*blanks)
006100030710     D wLung           s              2  0 INZ(*zeros)
006200040421     D wCTM            s                   LIKE(vabCTM)
006300040421     D wNSR            s              2    INZ
006400040421     D wCRC            s             10    INZ
006500040421     D wKSC            s              7    INZ
006600040421     D wCTR            s              3    INZ
006700040421     D wfivabds        s                   LIKE(fivabds)
006800050914     D wDataOraAlfa    s             14    INZ(*blanks)
006900010606     D*------------------
007000010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
007100010606     D*------------------
007200010606     D WLBDA8          DS                  INZ
007300010606     D  G08DAT                 1      8  0
007400010606     D  G08INV                 9     16  0
007500010606     D  G08ERR                17     17
007600010606     D  G08TGI                18     22  0
007700080502     D*------------------
007800080502     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
007900080502     D*------------------
008000080502     D SkCliOSR        s             10    DIM(900)
008100080502     D*
008200080502     D DsCliOSR        ds
008300080502     D  wLnpOSR                       3
008400080502     D  wCliOSR                       7
008500080502     D*
008600080502     D SkLnpNrsOSR     s              5    DIM(900)
008700071018     D*-------------------
008800071018     D* COSTANTI
008900071018     D*-------------------
009000071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200990908
009300000913     C                   reset                   rrnum
009400990921     C                   reset                   esito
009500990921     C                   reset                   wrkesito
009600010530     C*
009700010530     C                   exsr      opeini
009800010530     C                   exsr      rwvab
009900041025     C*
010000041025     C* Esegue lancio TISI95R solo x chiusura
010100041025     C                   CLEAR                   TISI95DS
010200041025     C                   EVAL      I95TLA = 'C'
010300041025     C                   CALL      'TISI95R'
010400041025     C                   PARM                    TISI95DS
010500010530     C*
010600010530     C                   seton                                        lr
010700010530
010800010530
010900010530
011000010530
011100010530     C*--------------------------------------------------------
011200010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011300010530     C*--------------------------------------------------------
011400010530     C     PREELA        BEGSR
011500000724     C*
011600000724     C* SE OCCORRE SPEDIRE IN FILIALE
011700010530     C                   if        invfil <> *zeros and
011800010530     C                             flgGiro = '0'
011900010530     C*
012000010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012100010530     C                   eval      flgGiro = '1'
012200010530     C*
012300010530     C                   endif
012400010530     C*
012500010530     C                   ENDSR
012600010530     C***
012700010530
012800010530
012900010530
013000010530
013100010530
013200010530
013300010530     C*--------------------------------------------------------
013400010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013500010530     C*--------------------------------------------------------
013600010530     C     ENDELA        BEGSR
013700040421     C*
013800040421     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
013900040923     C   54              call(e)   'TITV84R'
014000040421     C                   parm      'C'           InTipl
014100040421     C                   parm                    tivlrds
014200040421     C                   parm                    wfivabds
014300010530     C*
014400010530     C                   ENDSR
014500000613     C***
014600990908
014700010530
014800010530
014900010530
015000910830     C*--------------------------------------------------------
015100030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
015200910830     C*--------------------------------------------------------
015300941229     C     RWVAB         BEGSR
015400030710     C*
015500030710     C                   clear                   §CTROK
015600030710     C                   clear                   §CTRMO
015700030710     C                   clear                   §CTRNO
015800990910     C*
015900030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
016000030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
016100030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
016200030710     C                   if        *in60 = *on
016300990914     C                   if        not %open(tivin00r)
016400990908     C                   open      tivin00r
016500990914     C                   endif
016600030616     C                   if        not %open(edivabwr)
016700030616     C                   open      edivabwr
016800990914     C                   endif
016900990910     C*
017000921023     C                   DO        *HIVAL
017100990913     C*
017200990915     C                   READ      tivin00r                               70
017300071018     C*
017400071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
017500071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
017600010618     C*
017700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017800010618     C                   if        vinflg = '1'
017900010618     C                   eval      flgOk = '1'
018000010618     C                   endif
018100010705     C*
018200010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
018300010705     C                   if        %subst(vindta:1:1) = '='
018400010705     C                   eval      %subst(vindta:1:1) = *blanks
018500010705     C                   endif
018600010705     C*
018700000905     C                   if        vindta > *blanks
018800000613     C                   add       1             rrnum
018900010601     C* ...elimino eventuali segni
019000010601     C     '+':' '       xlate     vindta        vindta
019100990913     C*
019200010601     C                   if        *in70 = *off and
019300990913     C                             (vinflg = *blanks
019400010711     C                              or vinflg = '0'
019500010711     C                              or vinflg = '2')
019600990913     C*
019700030616     C                   clear                   EDIVAB00
019800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019900010711     C                   if        vinflg = *blanks or vinflg = '0'
020000010711     C                   clear                   vinmsg
020100010711     C                   endif
020200010711     C*
020300990913     C                   eval      fnvabaar = vindta
020400991022     C*
020500991022     C                   testb     x'3F'         vhbatb                   02
020600991022     C                   if        not *in02
020700991022     C*
020800971021     C                   SETOFF                                       303132
020900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
021000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
021100971023     C*    OPPURE TUTTI BLANKS
021200010530     C*
021300010530     C                   TESTN                   VHBLNP               30
021400010530     C  N30              TESTN                   VHBLNP                 30
021500010530     C  N30              TESTN                   VHBLNP                   30
021600010530     C                   MOVE      VHBLNP        W001A             1
021700010530     C     *IN30         IFEQ      *OFF
021800010530     C     W001A         ORLT      '0'
021900010530     C                   SETON                                        31
022000010530     C                   eval      vinmsg = %trimr(vinmsg)
022100010530     C                             + ' ' + 'VHBLNP'
022200010530     C                   ELSE
022300010530     C                   MOVE      VHBLNP        VABLNP
022400010530     C     VABLNP        IFLE      *ZEROS
022500010530     C                   SETON                                        31
022600010530     C                   eval      vinmsg = %trimr(vinmsg)
022700010530     C                             + ' ' + 'VHBLNP'
022800010530     C                   ENDIF
022900010530     C                   ENDIF
023000010530     C*
023100010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023200010530     C                   eval      depfil = VABLNP
023300010530     C                   exsr      repfil
023400010601     C                   if        depfil = invfil
023500021014     C                   if        vlrpoi = 999
023600021010     C                   MOVE(P)   invfil        VABFGS
023700021014     C                   else
023800021014     C                   MOVE(P)   vlrpoi        VABFGS
023900021014     C                   endif
024000010530     C*
024100010530     C                   exsr      PREELA
024200010530     C*
024300971023     C                   TESTN                   VHBCCM               30
024400971023     C  N30              TESTN                   VHBCCM                 30
024500971023     C  N30              TESTN                   VHBCCM                   30
024600971021     C                   MOVE      VHBCCM        W001A             1
024700971021     C     *IN30         IFEQ      *OFF
024800971021     C     W001A         ORLT      '0'
024900971021     C                   SETON                                        31
025000010604     C                   eval      vinmsg = %trimr(vinmsg)
025100010604     C                             + ' ' + 'VHBCCM'
025200971021     C                   ELSE
025300971021     C                   MOVE      VHBCCM        VABCCM
025400971024     C     VABCCM        IFLE      *ZEROS
025500971024     C                   SETON                                        31
025600010604     C                   eval      vinmsg = %trimr(vinmsg)
025700010604     C                             + ' ' + 'VHBCCM'
025800971024     C                   ENDIF
025900971021     C                   ENDIF
026000971021     C*
026100971023     C                   TESTN                   VHBAAS               30
026200971023     C  N30              TESTN                   VHBAAS                 30
026300971023     C  N30              TESTN                   VHBAAS                   30
026400971021     C                   MOVE      VHBAAS        W001A             1
026500971021     C     *IN30         IFEQ      *OFF
026600971021     C     W001A         ORLT      '0'
026700971023     C     W001A         ANDNE     *BLANK
026800971021     C                   SETON                                        32
026900010606     C                   MOVEL     datcor        VABAAS
027000010604     C                   eval      vinmsg = %trimr(vinmsg)
027100010604     C                             + ' ' + 'VHBAAS'
027200971021     C                   ELSE
027300971021     C                   MOVE      VHBAAS        VABAAS
027400971021     C                   ENDIF
027500971021     C*
027600971023     C                   TESTN                   VHBMGS               30
027700971023     C  N30              TESTN                   VHBMGS                 30
027800971023     C  N30              TESTN                   VHBMGS                   30
027900971021     C                   MOVE      VHBMGS        W001A             1
028000971021     C     *IN30         IFEQ      *OFF
028100971021     C     W001A         ORLT      '0'
028200971023     C     W001A         ANDNE     *BLANK
028300971021     C                   SETON                                        32
028400010601     C                   MOVE      '0101'        VABMGS
028500010604     C                   eval      vinmsg = %trimr(vinmsg)
028600010604     C                             + ' ' + 'VHBMGS'
028700971021     C                   ELSE
028800971021     C                   MOVE      VHBMGS        VABMGS
028900971021     C                   ENDIF
029000971021     C*
029100971023     C                   TESTN                   VHBNRS               30
029200971023     C  N30              TESTN                   VHBNRS                 30
029300971023     C  N30              TESTN                   VHBNRS                   30
029400971021     C                   MOVE      VHBNRS        W001A             1
029500971021     C     *IN30         IFEQ      *OFF
029600971021     C     W001A         ORLT      '0'
029700971023     C     W001A         ANDNE     *BLANK
029800971021     C                   SETON                                        32
029900971021     C                   MOVE      *ZERO         VABNRS
030000010604     C                   eval      vinmsg = %trimr(vinmsg)
030100010604     C                             + ' ' + 'VHBNRS'
030200971021     C                   ELSE
030300971021     C                   MOVE      VHBNRS        VABNRS
030400971021     C                   ENDIF
030500971021     C*
030600971023     C                   TESTN                   VHBNSP               30
030700971023     C  N30              TESTN                   VHBNSP                 30
030800971023     C  N30              TESTN                   VHBNSP                   30
030900971021     C                   MOVE      VHBNSP        W001A             1
031000971021     C     *IN30         IFEQ      *OFF
031100971021     C     W001A         ORLT      '0'
031200971023     C     W001A         ANDNE     *BLANK
031300971021     C                   SETON                                        32
031400971021     C                   MOVE      *ZERO         VABNSP
031500010604     C                   eval      vinmsg = %trimr(vinmsg)
031600010604     C                             + ' ' + 'VHBNSP'
031700971021     C                   ELSE
031800971021     C                   MOVE      VHBNSP        VABNSP
031900971021     C                   ENDIF
032000971021     C*
032100971023     C                   TESTN                   VHBLNA               30
032200971023     C  N30              TESTN                   VHBLNA                 30
032300971023     C  N30              TESTN                   VHBLNA                   30
032400971021     C                   MOVE      VHBLNA        W001A             1
032500971021     C     *IN30         IFEQ      *OFF
032600971021     C     W001A         ORLT      '0'
032700971023     C     W001A         ANDNE     *BLANK
032800971021     C                   SETON                                        32
032900971021     C                   MOVE      *ZERO         VABLNA
033000010604     C                   eval      vinmsg = %trimr(vinmsg)
033100010604     C                             + ' ' + 'VHBLNA'
033200971021     C                   ELSE
033300971021     C                   MOVE      VHBLNA        VABLNA
033400971021     C                   ENDIF
033500971021     C*
033600971023     C                   TESTN                   VHBCTR               30
033700971023     C  N30              TESTN                   VHBCTR                 30
033800971023     C  N30              TESTN                   VHBCTR                   30
033900971021     C                   MOVE      VHBCTR        W001A             1
034000971021     C     *IN30         IFEQ      *OFF
034100971021     C     W001A         ORLT      '0'
034200971023     C     W001A         ANDNE     *BLANK
034300971021     C                   SETON                                        32
034400971021     C                   MOVE      *ZERO         VABCTR
034500010604     C                   eval      vinmsg = %trimr(vinmsg)
034600010604     C                             + ' ' + 'VHBCTR'
034700971021     C                   ELSE
034800971021     C                   MOVE      VHBCTR        VABCTR
034900971021     C                   ENDIF
035000941229     C*
035100971021     C                   MOVE      *ZERO         VABIAS
035200941229     C                   MOVE      VHBIAS        WASC15
035300971023     C                   TESTN                   WI15                 30
035400971023     C  N30              TESTN                   WI15                   30
035500971023     C  N30              TESTN                   WI15                     30
035600971021     C                   MOVE      WI15          W001A             1
035700971021     C     *IN30         IFEQ      *OFF
035800971021     C     W001A         ORLT      '0'
035900971023     C     W001A         ANDNE     *BLANK
036000971021     C                   SETON                                        32
036100971021     C                   MOVE      *ZERO         VABIAS
036200010604     C                   eval      vinmsg = %trimr(vinmsg)
036300010604     C                             + ' ' + 'VHBIAS'
036400971021     C                   ELSE
036500971021     C                   MOVEL     WI15          VABIAS
036600971023     C                   TESTN                   WD15                 30
036700971023     C  N30              TESTN                   WD15                   30
036800971023     C  N30              TESTN                   WD15                     30
036900971021     C                   MOVE      WD15          W001A             1
037000971021     C     *IN30         IFEQ      *OFF
037100971021     C     W001A         ORLT      '0'
037200971023     C     W001A         ANDNE     *BLANK
037300971021     C                   SETON                                        32
037400971021     C                   MOVE      *ZERO         VABIAS
037500010604     C                   eval      vinmsg = %trimr(vinmsg)
037600010604     C                             + ' ' + 'VHBIAS'
037700971021     C                   ELSE
037800971021     C                   MOVE      WD15          VABIAS
037900971021     C                   ENDIF
038000050118     C                   ENDIF
038100941229     C*
038200971023     C                   TESTN                   VHBNCL               30
038300971023     C  N30              TESTN                   VHBNCL                 30
038400971023     C  N30              TESTN                   VHBNCL                   30
038500971021     C                   MOVE      VHBNCL        W001A             1
038600971021     C     *IN30         IFEQ      *OFF
038700971021     C     W001A         ORLT      '0'
038800971021     C                   SETON                                        32
038900971021     C                   MOVE      *ZERO         VABNCL
039000010604     C                   eval      vinmsg = %trimr(vinmsg)
039100010604     C                             + ' ' + 'VHBNCL'
039200971021     C                   ELSE
039300971021     C                   MOVE      VHBNCL        VABNCL
039400971021     C                   ENDIF
039500941229     C*
039600971021     C                   MOVE      *ZERO         VABPKB
039700971021     C                   MOVE      VHBPKB        WASC09
039800010719     C                   IF        WI09 = *blanks
039900010719     C                   EVAL      WI09 = *zeros
040000010719     C                   ENDIF
040100971023     C                   TESTN                   WI09                 30
040200971023     C  N30              TESTN                   WI09                   30
040300971023     C  N30              TESTN                   WI09                     30
040400971021     C                   MOVE      WI09          W001A             1
040500971021     C     *IN30         IFEQ      *OFF
040600971021     C     W001A         ORLT      '0'
040700971021     C                   SETON                                        32
040800971021     C                   MOVE      *ZERO         VABPKB
040900010604     C                   eval      vinmsg = %trimr(vinmsg)
041000010604     C                             + ' ' + 'VHBPKB'
041100971021     C                   ELSE
041200971021     C                   MOVEL     WI09          VABPKB
041300010719     C                   IF        WD09 = *blanks
041400010719     C                   EVAL      WD09 = *zeros
041500010719     C                   ENDIF
041600971023     C                   TESTN                   WD09                 30
041700971023     C  N30              TESTN                   WD09                     30
041800971021     C                   MOVE      WD09          W001A             1
041900971021     C     *IN30         IFEQ      *OFF
042000971021     C     W001A         ORLT      '0'
042100971023     C     W001A         ANDNE     *BLANK
042200971021     C                   SETON                                        32
042300971021     C                   MOVE      *ZERO         VABPKB
042400010604     C                   eval      vinmsg = %trimr(vinmsg)
042500010604     C                             + ' ' + 'VHBPKB'
042600971021     C                   ELSE
042700971021     C                   MOVE      WD09          VABPKB
042800971021     C                   ENDIF
042900050118     C                   ENDIF
043000971021     C*
043100041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
043200041001     C                   IF        *IN51 = *OFF
043300971021     C                   MOVE      *ZERO         VABVLB
043400971021     C                   MOVE      VHBVLB        WASC07
043500010719     C                   IF        WI07 = *blanks
043600010719     C                   EVAL      WI07 = *zeros
043700010719     C                   ENDIF
043800971023     C                   TESTN                   WI07                 30
043900971023     C  N30              TESTN                   WI07                   30
044000971023     C  N30              TESTN                   WI07                     30
044100971021     C                   MOVE      WI07          W001A             1
044200971021     C     *IN30         IFEQ      *OFF
044300971021     C     W001A         ORLT      '0'
044400971023     C     W001A         ANDNE     *BLANK
044500971021     C                   SETON                                        32
044600971021     C                   MOVE      *ZERO         VABVLB
044700010604     C                   eval      vinmsg = %trimr(vinmsg)
044800010604     C                             + ' ' + 'VHBVLB'
044900971021     C                   ELSE
045000971021     C                   MOVEL     WI07          VABVLB
045100010719     C                   IF        WD07 = *blanks
045200010719     C                   EVAL      WD07 = *zeros
045300010719     C                   ENDIF
045400971023     C                   TESTN                   WD07                 30
045500971023     C  N30              TESTN                   WD07                   30
045600971023     C  N30              TESTN                   WD07                     30
045700971021     C                   MOVE      WD07          W001A             1
045800971021     C     *IN30         IFEQ      *OFF
045900971021     C     W001A         ORLT      '0'
046000971023     C     W001A         ANDNE     *BLANK
046100971021     C                   SETON                                        32
046200971021     C                   MOVE      *ZERO         VABVLB
046300010604     C                   eval      vinmsg = %trimr(vinmsg)
046400010604     C                             + ' ' + 'VHBVLB'
046500971021     C                   ELSE
046600971021     C                   MOVE      WD07          VABVLB
046700971021     C                   ENDIF
046800050118     C                   ENDIF
046900041001     C                   ELSE
047000041001     C* ...altrimenti forzo azzeramento del volume
047100041001     C                   Z-ADD     *zeros        VABVLB
047200041001     C                   ENDIF
047300941229     C*
047400971021     C                   MOVE      *ZERO         VABQFT
047500971021     C                   MOVE      VHBQFT        WASC15
047600010719     C                   IF        WI15 = *blanks
047700010719     C                   EVAL      WI15 = *zeros
047800010719     C                   ENDIF
047900971023     C                   TESTN                   WI15                 30
048000971023     C  N30              TESTN                   WI15                   30
048100971023     C  N30              TESTN                   WI15                     30
048200971021     C                   MOVE      WI15          W001A             1
048300971021     C     *IN30         IFEQ      *OFF
048400971021     C     W001A         ORLT      '0'
048500971023     C     W001A         ANDNE     *BLANK
048600971021     C                   SETON                                        32
048700971021     C                   MOVE      *ZERO         VABQFT
048800010604     C                   eval      vinmsg = %trimr(vinmsg)
048900010604     C                             + ' ' + 'VHBQFT'
049000971021     C                   ELSE
049100971021     C                   MOVEL     WI15          VABQFT
049200010719     C                   IF        WD15 = *blanks
049300010719     C                   EVAL      WD15 = *zeros
049400010719     C                   ENDIF
049500971023     C                   TESTN                   WD15                 30
049600971023     C  N30              TESTN                   WD15                   30
049700971023     C  N30              TESTN                   WD15                     30
049800971021     C                   MOVE      WD15          W001A             1
049900971021     C     *IN30         IFEQ      *OFF
050000971021     C     W001A         ORLT      '0'
050100971023     C     W001A         ANDNE     *BLANK
050200971021     C                   SETON                                        32
050300971021     C                   MOVE      *ZERO         VABQFT
050400010604     C                   eval      vinmsg = %trimr(vinmsg)
050500010604     C                             + ' ' + 'VHBQFT'
050600971021     C                   ELSE
050700971021     C                   MOVE      WD15          VABQFT
050800971021     C                   ENDIF
050900050118     C                   ENDIF
051000941229     C*
051100971021     C                   MOVE      *ZERO         VABCAS
051200971021     C                   MOVE      VHBCAS        WASC15
051300010719     C                   IF        WI15 = *blanks
051400010719     C                   EVAL      WI15 = *zeros
051500010719     C                   ENDIF
051600971023     C                   TESTN                   WI15                 30
051700971023     C  N30              TESTN                   WI15                   30
051800971023     C  N30              TESTN                   WI15                     30
051900971021     C                   MOVE      WI15          W001A             1
052000971021     C     *IN30         IFEQ      *OFF
052100971021     C     W001A         ORLT      '0'
052200971023     C     W001A         ANDNE     *BLANK
052300971021     C                   SETON                                        32
052400971021     C                   MOVE      *ZERO         VABCAS
052500010604     C                   eval      vinmsg = %trimr(vinmsg)
052600010604     C                             + ' ' + 'VHBCAS'
052700971021     C                   ELSE
052800971021     C                   MOVEL     WI15          VABCAS
052900010719     C                   IF        WD15 = *blanks
053000010719     C                   EVAL      WD15 = *zeros
053100010719     C                   ENDIF
053200971023     C                   TESTN                   WD15                 30
053300971023     C  N30              TESTN                   WD15                   30
053400971023     C  N30              TESTN                   WD15                     30
053500971021     C                   MOVE      WD15          W001A             1
053600971021     C     *IN30         IFEQ      *OFF
053700971021     C     W001A         ORLT      '0'
053800971023     C     W001A         ANDNE     *BLANK
053900971021     C                   SETON                                        32
054000971021     C                   MOVE      *ZERO         VABCAS
054100010604     C                   eval      vinmsg = %trimr(vinmsg)
054200010604     C                             + ' ' + 'VHBCAS'
054300971021     C                   ELSE
054400971021     C                   MOVE      WD15          VABCAS
054500971021     C                   ENDIF
054600050118     C                   ENDIF
054700971021     C*
054800971023     C                   TESTN                   VHBRMN               30
054900971023     C  N30              TESTN                   VHBRMN                 30
055000971023     C  N30              TESTN                   VHBRMN                   30
055100971021     C                   MOVE      VHBRMN        W001A             1
055200971021     C     *IN30         IFEQ      *OFF
055300971021     C     W001A         ORLT      '0'
055400971021     C                   SETON                                        32
055500971021     C                   MOVE      *ZERO         VABRMN
055600010604     C                   eval      vinmsg = %trimr(vinmsg)
055700010604     C                             + ' ' + 'VHBRMN'
055800971021     C                   ELSE
055900971021     C                   MOVE      VHBRMN        VABRMN
056000971021     C                   ENDIF
056100971021     C*
056200971023     C                   TESTN                   VHBNCD               30
056300971023     C  N30              TESTN                   VHBNCD                 30
056400971023     C  N30              TESTN                   VHBNCD                   30
056500971021     C                   MOVE      VHBNCD        W001A             1
056600971021     C     *IN30         IFEQ      *OFF
056700971021     C     W001A         ORLT      '0'
056800971023     C     W001A         ANDNE     *BLANK
056900971021     C                   SETON                                        32
057000971021     C                   MOVE      *ZERO         VABNCD
057100010604     C                   eval      vinmsg = %trimr(vinmsg)
057200010604     C                             + ' ' + 'VHBNCD'
057300971021     C                   ELSE
057400971021     C                   MOVE      VHBNCD        VABNCD
057500971021     C                   ENDIF
057600971021     C*
057700971023     C                   TESTN                   VHBNCA               30
057800971023     C  N30              TESTN                   VHBNCA                 30
057900971023     C  N30              TESTN                   VHBNCA                   30
058000971021     C                   MOVE      VHBNCA        W001A             1
058100971021     C     *IN30         IFEQ      *OFF
058200971021     C     W001A         ORLT      '0'
058300971023     C     W001A         ANDNE     *BLANK
058400971021     C                   SETON                                        32
058500971021     C                   MOVE      *ZERO         VABNCA
058600010604     C                   eval      vinmsg = %trimr(vinmsg)
058700010604     C                             + ' ' + 'VHBNCA'
058800971021     C                   ELSE
058900971021     C                   MOVE      VHBNCA        VABNCA
059000971021     C                   ENDIF
059100971021     C*
059200971023     C                   TESTN                   VHBZNC               30
059300971023     C  N30              TESTN                   VHBZNC                 30
059400971023     C  N30              TESTN                   VHBZNC                   30
059500971021     C                   MOVE      VHBZNC        W001A             1
059600971021     C     *IN30         IFEQ      *OFF
059700971021     C     W001A         ORLT      '0'
059800971023     C     W001A         ANDNE     *BLANK
059900971021     C                   SETON                                        32
060000971021     C                   MOVE      *ZERO         VABZNC
060100010604     C                   eval      vinmsg = %trimr(vinmsg)
060200010604     C                             + ' ' + 'VHBZNC'
060300971021     C                   ELSE
060400971021     C                   MOVE      VHBZNC        VABZNC
060500971021     C                   ENDIF
060600971021     C*
060700971023     C                   TESTN                   VHBDCR               30
060800971023     C  N30              TESTN                   VHBDCR                 30
060900971023     C  N30              TESTN                   VHBDCR                   30
061000971021     C                   MOVE      VHBDCR        W001A             1
061100971021     C     *IN30         IFEQ      *OFF
061200971021     C     W001A         ORLT      '0'
061300971023     C     W001A         ANDNE     *BLANK
061400971021     C                   SETON                                        32
061500971021     C                   MOVE      *ZERO         VABDCR
061600010604     C                   eval      vinmsg = %trimr(vinmsg)
061700010604     C                             + ' ' + 'VHBDCR'
061800971021     C                   ELSE
061900971021     C                   MOVE      VHBDCR        VABDCR
062000971021     C                   ENDIF
062100971021     C*
062200971023     C                   TESTN                   VHBHCR               30
062300971023     C  N30              TESTN                   VHBHCR                 30
062400971023     C  N30              TESTN                   VHBHCR                   30
062500971021     C                   MOVE      VHBHCR        W001A             1
062600971021     C     *IN30         IFEQ      *OFF
062700971021     C     W001A         ORLT      '0'
062800971023     C     W001A         ANDNE     *BLANK
062900971021     C                   SETON                                        32
063000971021     C                   MOVE      *ZERO         VABHCR
063100010604     C                   eval      vinmsg = %trimr(vinmsg)
063200010604     C                             + ' ' + 'VHBHCR'
063300971021     C                   ELSE
063400971021     C                   MOVE      VHBHCR        VABHCR
063500971021     C                   ENDIF
063600941229     C*
063700971021     C                   MOVE      *ZERO         VABVMD
063800971021     C                   MOVE      VHBVMD        WASC15
063900010719     C                   IF        WI15 = *blanks
064000010719     C                   EVAL      WI15 = *zeros
064100010719     C                   ENDIF
064200971023     C                   TESTN                   WI15                 30
064300971023     C  N30              TESTN                   WI15                   30
064400971023     C  N30              TESTN                   WI15                     30
064500971021     C                   MOVE      WI15          W001A             1
064600971021     C     *IN30         IFEQ      *OFF
064700971021     C     W001A         ORLT      '0'
064800971023     C     W001A         ANDNE     *BLANK
064900971021     C                   SETON                                        32
065000971021     C                   MOVE      *ZERO         VABVMD
065100010604     C                   eval      vinmsg = %trimr(vinmsg)
065200010604     C                             + ' ' + 'VHBVMD'
065300971021     C                   ELSE
065400971021     C                   MOVEL     WI15          VABVMD
065500010719     C                   IF        WD15 = *blanks
065600010719     C                   EVAL      WD15 = *zeros
065700010719     C                   ENDIF
065800971023     C                   TESTN                   WD15                 30
065900971023     C  N30              TESTN                   WD15                   30
066000971023     C  N30              TESTN                   WD15                     30
066100971021     C                   MOVE      WD15          W001A             1
066200971021     C     *IN30         IFEQ      *OFF
066300971021     C     W001A         ORLT      '0'
066400971023     C     W001A         ANDNE     *BLANK
066500971021     C                   SETON                                        32
066600971021     C                   MOVE      *ZERO         VABVMD
066700010604     C                   eval      vinmsg = %trimr(vinmsg)
066800010604     C                             + ' ' + 'VHBVMD'
066900971021     C                   ELSE
067000971021     C                   MOVE      WD15          VABVMD
067100971021     C                   ENDIF
067200050118     C                   ENDIF
067300971021     C*
067400971023     C                   TESTN                   VHBANT               30
067500971023     C  N30              TESTN                   VHBANT                 30
067600971023     C  N30              TESTN                   VHBANT                   30
067700971021     C                   MOVE      VHBANT        W001A             1
067800971021     C     *IN30         IFEQ      *OFF
067900971021     C     W001A         ORLT      '0'
068000971023     C     W001A         ANDNE     *BLANK
068100971021     C                   SETON                                        32
068200971021     C                   MOVE      *ZERO         VABANT
068300010604     C                   eval      vinmsg = %trimr(vinmsg)
068400010604     C                             + ' ' + 'VHBANT'
068500971021     C                   ELSE
068600971021     C                   MOVE      VHBANT        VABANT
068700971021     C                   ENDIF
068800971021     C*
068900971021     C* CAMPI ALFABETICI:
069000971021     C                   MOVE      VHBATB        VABATB
069100971021     C                   MOVE      VHBCBO        VABCBO
069200971021     C                   MOVE      VHBRSD        VABRSD
069300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
069400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
069500020117     C* ==
069600971021     C                   MOVE      VHBRD2        VABRD2
069700971021     C                   MOVE      VHBIND        VABIND
069800971021     C                   MOVE      VHBCAD        VABCAD
069900971021     C                   MOVE      VHBLOD        VABLOD
070000971021     C                   MOVE      VHBPRD        VABPRD
070100971021     C                   MOVE      VHBNZD        VABNZD
070200971021     C                   MOVE      VHBGC1        VABGC1
070300971021     C                   MOVE      VHBGC2        VABGC2
070400971021     C                   MOVE      VHBTSP        VABTSP
070500971021     C                   MOVE      VHBVAS        VABVAS
070600971021     C                   MOVE      VHBNAS        VABNAS
070700971021     C                   MOVE      VHBTIC        VABTIC
070800971021     C                   MOVE      VHBVCA        VABVCA
070900971021     C                   MOVE      VHBGCA        VABGCA
071000971021     C                   MOVE      VHBRMA        VABRMA
071100971021     C                   MOVE      VHBXCO        VABXCO
071200971021     C                   MOVE      VHBNOT        VABNOT
071300971021     C                   MOVE      VHBNT2        VABNT2
071400971021     C                   MOVE      VHBCTM        VABCTM
071500971021     C                   MOVE      VHBFFD        VABFFD
071600971021     C                   MOVE      VHBTCR        VABTCR
071700971021     C                   MOVE      VHBCTS        VABCTS
071800971021     C                   MOVE      VHBFTM        VABFTM
071900971021     C                   MOVE      VHBVAD        VABVAD
072000971021     C                   MOVE      VHBGMA        VABGMA
072100971021     C                   MOVE      VHBGGA        VABGGA
072200971021     C                   MOVE      VHBGVA        VABGVA
072300971021     C                   MOVE      VHBTC1        VABTC1
072400971021     C                   MOVE      VHBTC2        VABTC2
072500971021     C                   MOVE      VHBSCL        VABSCL
072600971021     C                   MOVE      VHBRMO        VABRMO
072700971021     C                   MOVE      VHBCMO        VABCMO
072800971021     C                   MOVE      VHBNMO        VABNMO
072900971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
073000971021     C* NELLA NAZIONE ORIGINALE
073100971021     C                   MOVE      VHBNMO        WA1D              1
073200971021     C                   MOVEL     VHBNMO        WA1S              1
073300971021     C     WA1D          IFNE      *BLANK
073400971021     C     WA1S          ANDEQ     *BLANK
073500971021     C                   MOVE      *BLANK        VABNMO
073600030616     C                   ENDIF
073700030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
073800030710     C                   EVAL      VABCMR = wCMR
073900030616     C                   EVAL      VABDCM = DATCOR
074000030616     C                   EVAL      VABDTS = DATCOR
074100040402     C                   EVAL      VABHMS = ORACOR
074200030616     C                   EVAL      VABCNT = 1
074300040421     C*
074400040421     C* Se il codice trattamento merce nn viene passato dal cliente lo valorizzo con il valore
074500040421     C* d default passato come parametro dall'anagrafica clienti/tipo file
074600040421     C                   IF        VABCTM = *blanks
074700040421     C                   EVAL      VABCTM = wCTM
074800040421     C                   ENDIF
074900040421     C*
075000040421     C* Se presente particolarità consegna 'RC' e se il codice
075100040421     C* cliente mittente è valorizzato e numero serie ORM nn passata
075200040421     C* nei parametri verifico se il cliente è abilitato in tabella 'OSR'
075300040421     C                   IF        VABGMA = 'RC'
075400040421     C                   IF        VABCCM > *ZEROS
075500040421     C                   IF        wNSR = *BLANKS
075600040421     C                   Z-ADD     1             jOSR
075700080502     C                   move(p)   VABLNP        wLnpOSR
075800080502     C                   move(p)   VABCCM        wCliOSR
075900080502     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
076000040421     C                   IF        %found
076100040421     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
076200040423     C                   EVAL      %subst(vlrppt:4:2) = wNSR
076300040421     C                   ELSE
076400040421     C                   SETON                                        31
076500040421     C                   EVAL      vinmsg = %trimr(vinmsg)
076600040421     C                             + ' ' + 'ORMNRS(tabellaOSR)'
076700040421     C                   ENDIF
076800040421     C                   ENDIF
076900040421     C                   ENDIF
077000040421     C                   ENDIF
077100040421     C*
077200040421     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
077300040421     C                   if        *IN31 = *OFF AND
077400040421     C                             vabGMA = 'RC'
077500040923     C*
077600040923     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
077700040923     C                   seton                                        54
077800040421     C*
077900040421     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
078000040421     C                   eval      wfivabds = fivabds
078100040920     C                   call      'TITV84R'
078200040421     C                   parm      *blanks       InTipl            1
078300040421     C                   parm                    tivlrds
078400040421     C                   parm                    wfivabds
078500040421     C*
078600040421     C                   endif
078700041025     C*
078800041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
078900041025     C                   IF        *IN52 = *ON
079000041025     C* Reperisco la provincia dal CAP e dalla località
079100041025     C                   IF        VABCAD <> *blanks AND
079200050907     C                             VABPRD  = *blanks AND
079300050907     C                             VABNZD  = *blanks
079400041025     C                   CLEAR                   TISI95DS
079500041025     C                   EVAL      I95TCN = '3'
079600041025     C                   Z-ADD     datcor        I95DAT
079700041025     C                   EVAL      I95CAP = VABCAD
079800041025     C                   EVAL      I95LOC = VABLOD
079900041025     C                   CALL      'TISI95R'
080000041025     C                   PARM                    TISI95DS
080100041025     C                   EVAL      VABPRD = O95PRV
080200041025     C                   ENDIF
080300041025     C                   ENDIF
080400100419     C*
080500100419     C* Forzatura su campi DIVISE
080600100419     C                   IF        VABVAS = *blanks
080700100419     C                   EVAL      VABVAS = 'EUR'
080800100419     C                   ENDIF
080900100419     C                   IF        VABVCA = *blanks
081000100419     C                   EVAL      VABVCA = 'EUR'
081100100419     C                   ENDIF
081200100419     C                   IF        VABVAD = *blanks
081300100419     C                   EVAL      VABVAD = 'EUR'
081400100419     C                   ENDIF
081500930409     C*
081600010604     C  N31              ADD       1             §CTROK            7 0
081700010604     C   32              ADD       1             §CTRMO            7 0
081800010604     C   31              ADD       1             §CTRNO            7 0
081900030616     C  N31              WRITE     EDIVAB00
082000990910     C*
082100010601     C                   if        *in31 = *off and
082200010601     C                             *in32 = *off
082300990910     C                   eval      vinflg = '1'
082400990910     C                   else
082500990910     C                   eval      vinflg = '2'
082600990910     C                   endif
082700991022     C                   endif
082800990910     C                   endif
082900010601     C*
083000010601     C                   endif
083100000905     C*
083200000905     C                   else
083300000905     C                   eval      vinflg = '1'
083400000905     C                   endif
083500000905     C*
083600000905     C  N70              update    tivin000
083700991022     C*
083800991022     C  N70              ENDdo
083900010530     C
084000010530     C*
084100010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
084200010530     C                   if        cntNonEl = *zeros or
084300010530     C                             flgMulti = '0'
084400990910     C* Se non ci sono record con errori ...
084500010530     C                   if        §ctrno = 0 and
084600010601     C                             §ctrmo = 0 and
084700010530     C                             flgStato <> '2'
084800990910     C* ... restituisco esito OK.
084900990921     C                   eval      wrkesito = '0'
085000990910     C                   else
085100010601     C                   if        §ctrok > 0
085200990921     C                   eval      wrkesito = '1'
085300000710     C                   else
085400010615     C                   if        flgOk = '0'
085500010615     C                   eval      wrkesito = '2'
085600010615     C                   else
085700010615     C                   eval      wrkesito = '6'
085800010615     C                   endif
085900990910     C                   endif
086000000710     C                   endif
086100010530     C                   else
086200010530     C                   eval      wrkesito = '9'
086300010530     C                   endif
086400030710     C*
086500030710 X  1C                   else
086600030710     C                   eval      wrkesito = '8'
086700030710     C                   eval      flgGiro = '1'
086800030710 E  1C                   endif
086900030710     C*
087000990914     C                   if        %open(tivin00r)
087100990908     C                   close     tivin00r
087200990914     C                   endif
087300030616     C                   if        %open(edivabwr)
087400030616     C                   close     edivabwr
087500990914     C                   endif
087600010530     C*
087700010530     C                   if        vlrpoi <> 999
087800010530     C                   eval      invfil = vlrpoi
087900010530     C                   endif
088000990910     C*
088100990920     C                   if        §ctrok > 0
088200010530     C                             and invfil > *zeros
088300000613     C                   exsr      invio
088400990920     C                   endif
088500010612     C*
088600010612     C                   if        flgGiro = '1'
088700010612     C                   exsr      endela
088800010612     C                   endif
088900990920     C*
089000910830     C                   ENDSR
089100000613     C***
089200990920
089300010530
089400010530
089500010530
089600010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
089700010530     C     repfil        BEGSR
089800010530     C*
089900010530     C                   if        invfil = *zeros and
090000010530     C                             depfil > *zeros and
090100010613     C                             (vinflg = *blanks or
090200010613     C                              vinflg = *zeros)
090300010530     C
090400010530     C                   eval      invfil = depfil
090500010530     C                   endif
090600010530     C*
090700010530     C                   if        depfil <> invfil and
090800010530     C                             invfil > *zeros
090900010530     C                   eval      flgMulti = '1'
091000010530     C                   if        vinflg = *blanks
091100010530     C                   add       1             cntNonEl
091200010530     C                   endif
091300010530     C                   endif
091400010530     C*
091500010530     C                   if        vinflg = '2'
091600010530     C                   eval      flgStato = '2'
091700010530     C                   endif
091800010530     C*
091900010530     C                   ENDSR
092000010530     C***
092100010530
092200010530
092300010530
092400010530
092500990920      /TITLE Invio dei dati al punto operativo.
092600000613     C     invio         BEGSR
092700990920     C*
092800990920     C                   reset                   dscmz
092900030618     C                   move      invfil        cmzdst
093000030618     C***                move      '789'         cmzdst
093100990920     C                   eval      cmzfld = vlrfou
093200990920     C                   eval      cmzmbd = vlrhdl
093300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
093400000710     C                   if        prmfir = *blanks
093500030616     C                   eval      cmzfla = 'EDIVAB0F'
093600030616     C                   eval      cmzmba = 'EDIVAB0F'
093700000710     C                   else
093800000710     C                   eval      cmzfla = prmfir
093900000710     C                   eval      cmzmba = prmfir
094000000710     C                   endif
094100990920     C                   eval      cmznrr = *zeros
094200990920     C                   move      §ctrok        cmznrr
094300021003     C                   eval      cmzlba = vlrfl1
094400990920     C                   call(e)   'TIS711C'
094500990920     C                   parm                    dscmz
094600990921     C                   parm      *blanks       esito
094700990920     C                   if        %error
094800990920     C                             or cmzerr = '1'
094900990921     C                             or esito  = '1'
095000000710     C                   eval      wrkesito = '3'
095100990920     C                   endif
095200990920     C*
095300000613     C                   ENDSR
095400000613     C***
095500010530
095600010530
095700010530
095800010530      /TITLE Invio dei dati al punto operativo.
095900010530     C     opeini        BEGSR
096000010530     C*
096100010530     C* Inizializzo flag e contatori operativi
096200010530     C                   movel     '0'           flgGiro           1
096300010530     C                   movel     '0'           flgMulti          1
096400010530     C                   movel     '1'           flgStato          1
096500010615     C                   movel     '0'           flgOk             1
096600010530     C                   z-add     *zeros        cntNonEl         10 0
096700010530     C                   z-add     *zeros        depfil            3 0
096800010530     C                   z-add     *zeros        invfil            3 0
096900040423     C*
097000040423     C* Verifico se impostato il parametro relativo al codice trattamento merce (VABCTM)
097100040423     C                   if        %subst(vlrppt:2:2) <> *blanks
097200040423     C                   eval      wCTM = %subst(vlrppt:2:2)
097300040423     C                   endif
097400040423     C*
097500040423     C* Verifico se impostato il parametro relativo al numero serie orm (ORMNSR)
097600040423     C                   if        %subst(vlrppt:4:2) <> *blanks
097700040423     C                   eval      wNSR = %subst(vlrppt:4:2)
097800040423     C                   endif
097900040423     C*
098000040423     C* Verifico se impostato il parametro relativo al codice consegna (ORMCRC)
098100040423     C                   if        %subst(vlrppt:6:10) <> *blanks
098200040423     C                   eval      wCRC = %subst(vlrppt:6:10)
098300040423     C                   endif
098400040423     C*
098500040423     C* Verifico se impostato il parametro relativo al codice pagante (ORMKSC)
098600040423     C                   if        %subst(vlrppt:16:7) <> *blanks
098700040423     C                   eval      wKSC = %subst(vlrppt:16:7)
098800040423     C                   endif
098900040423     C*
099000040423     C* Verifico se impostato il parametro relativo al codice pagante (ORMCTR)
099100040423     C                   if        %subst(vlrppt:23:3) <> *blanks
099200040423     C                   eval      wCTR = %subst(vlrppt:23:3)
099300040423     C                   endif
099400010530     C*
099500010530     C                   ENDSR
099600010530     C***
099700040421
099800040421
099900040421
100000080502     C*--------------------------------------------------------
100100080502     C* CARTAB - CARICAMENTO TABELLE                          *
100200080502     C*--------------------------------------------------------
100300080502     C     CARTAB        BEGSR
100400080502     C*
100500080502     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
100600080502     C                   z-add     *zeros        jOSR              3 0
100700080502     C     'OSR'         setll     tntbe01l
100800080502     C                   if        %equal(tntbe01l)
100900080502     C     'OSR'         reade     tntbe01l
101000080502     C                   dow       not %eof(tntbe01l)
101100080502     C                   eval      DOSR = tbeUNI
101200080502     c*
101300080502     c* Verifica e forzatura x nuovi campi aggiunti
101400080502     c                   if        aD§OSRcl6 = *blanks
101500080502     c                   eval      D§OSRcl6 = *zeros
101600080502     c                   endif
101700080502     c                   if        aD§OSRcl7 = *blanks
101800080502     c                   eval      D§OSRcl7 = *zeros
101900080502     c                   endif
102000080502     c                   if        aD§OSRcl8 = *blanks
102100080502     c                   eval      D§OSRcl8 = *zeros
102200080502     c                   endif
102300080502     c                   if        aD§OSRcl9 = *blanks
102400080502     c                   eval      D§OSRcl9 = *zeros
102500080502     c                   endif
102600080502     c                   if        aD§OSRcl10 = *blanks
102700080502     c                   eval      D§OSRcl10 = *zeros
102800080502     c                   endif
102900080502     c                   if        aD§OSRcl11 = *blanks
103000080502     c                   eval      D§OSRcl11 = *zeros
103100080502     c                   endif
103200080502     c                   if        aD§OSRcl12 = *blanks
103300080502     c                   eval      D§OSRcl12 = *zeros
103400080502     c                   endif
103500080502     c                   if        aD§OSRcl13 = *blanks
103600080502     c                   eval      D§OSRcl13 = *zeros
103700080502     c                   endif
103800080502     c                   if        aD§OSRcl14 = *blanks
103900080502     c                   eval      D§OSRcl14 = *zeros
104000080502     c                   endif
104100080502     c                   if        aD§OSRcl15 = *blanks
104200080502     c                   eval      D§OSRcl15 = *zeros
104300080502     c                   endif
104400080502     c                   if        aD§OSRcl16 = *blanks
104500080502     c                   eval      D§OSRcl16 = *zeros
104600080502     c                   endif
104700080502     c*
104800080502     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
104900080502     C                   if        D§OSRCLI <> *zeros
105000080502     C                   add       1             jOSR
105100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
105200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
105300080502     C                   movel     D§OSRCLI      wCliOSR
105400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
105500080502     C                   endif
105600080502     C*
105700080502     C                   if        D§OSRCL2 <> *zeros
105800080502     C                   add       1             jOSR
105900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
106000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
106100080502     C                   movel     D§OSRCL2      wCliOSR
106200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
106300080502     C                   endif
106400080502     C*
106500080502     C                   if        D§OSRCL3 <> *zeros
106600080502     C                   add       1             jOSR
106700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
106800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
106900080502     C                   movel     D§OSRCL3      wCliOSR
107000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
107100080502     C                   endif
107200080502     C*
107300080502     C                   if        D§OSRCL4 <> *zeros
107400080502     C                   add       1             jOSR
107500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
107600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
107700080502     C                   movel     D§OSRCL4      wCliOSR
107800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
107900080502     C                   endif
108000080502     C*
108100080502     C                   if        D§OSRCL5 <> *zeros
108200080502     C                   add       1             jOSR
108300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
108400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
108500080502     C                   movel     D§OSRCL5      wCliOSR
108600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
108700080502     C                   endif
108800080502     C*
108900080502     C                   if        D§OSRCL6 <> *zeros
109000080502     C                   add       1             jOSR
109100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
109200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
109300080502     C                   movel     D§OSRCL6      wCliOSR
109400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
109500080502     C                   endif
109600080502     C*
109700080502     C                   if        D§OSRCL7 <> *zeros
109800080502     C                   add       1             jOSR
109900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
110000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
110100080502     C                   movel     D§OSRCL7      wCliOSR
110200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
110300080502     C                   endif
110400080502     C*
110500080502     C                   if        D§OSRCL8 <> *zeros
110600080502     C                   add       1             jOSR
110700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
110800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
110900080502     C                   movel     D§OSRCL8      wCliOSR
111000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
111100080502     C                   endif
111200080502     C*
111300080502     C                   if        D§OSRCL9 <> *zeros
111400080502     C                   add       1             jOSR
111500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
111600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
111700080502     C                   movel     D§OSRCL9      wCliOSR
111800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
111900080502     C                   endif
112000080502     C*
112100080502     C                   if        D§OSRCL10 <> *zeros
112200080502     C                   add       1             jOSR
112300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
112400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
112500080502     C                   movel     D§OSRCL10     wCliOSR
112600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
112700080502     C                   endif
112800080502     C*
112900080502     C                   if        D§OSRCL11 <> *zeros
113000080502     C                   add       1             jOSR
113100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
113200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
113300080502     C                   movel     D§OSRCL11     wCliOSR
113400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
113500080502     C                   endif
113600080502     C*
113700080502     C                   if        D§OSRCL12 <> *zeros
113800080502     C                   add       1             jOSR
113900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
114000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
114100080502     C                   movel     D§OSRCL12     wCliOSR
114200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
114300080502     C                   endif
114400080502     C*
114500080502     C                   if        D§OSRCL13 <> *zeros
114600080502     C                   add       1             jOSR
114700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
114800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
114900080502     C                   movel     D§OSRCL13     wCliOSR
115000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
115100080502     C                   endif
115200080502     C*
115300080502     C                   if        D§OSRCL14 <> *zeros
115400080502     C                   add       1             jOSR
115500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
115600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
115700080502     C                   movel     D§OSRCL14     wCliOSR
115800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
115900080502     C                   endif
116000080502     C*
116100080502     C                   if        D§OSRCL15 <> *zeros
116200080502     C                   add       1             jOSR
116300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
116400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
116500080502     C                   movel     D§OSRCL15     wCliOSR
116600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
116700080502     C                   endif
116800080502     C*
116900080502     C                   if        D§OSRCL16 <> *zeros
117000080502     C                   add       1             jOSR
117100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
117200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
117300080502     C                   movel     D§OSRCL16     wCliOSR
117400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
117500080502     C                   endif
117600080502     C*
117700080502     C     'OSR'         reade     tntbe01l
117800080502     C                   enddo
117900080502     C                   endif
118000080502     C*
118100080502     C                   ENDSR
118200080502     C***
118300010530
118400010530
118500010530
118600990910
118700000613     C     *inzsr        BEGSR
118800990910     C*
118900990910     C     *entry        plist
119000990920     C                   parm                    tivlrds
119100990921     C                   parm      wrkesito      esito
119200000724     C                   parm                    prmlit
119300000710     C                   parm                    prmfir
119400040421     C*
119500040421     C* CARICO LE TABELLE OCCORRENTI
119600040421     C                   exsr      CARTAB
119700030616     C*
119800030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
119900030710     C                   move(p)   vlrMSG        wNomeFile
120000030710     C*
120100030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
120200030710     C* del codice CMR dal nome del file importato.
120300030710     C                   if        wNomeFile <> *blanks AND
120400030710     C                             vlrppt<>*blanks
120500041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
120600041001     C                   if        %subst(vlrppt:1:1) = 'S'
120700030710     C                   seton                                        50
120800030710     C                   else
120900030710     C                   setoff                                       50
121000030710     C                   endif
121100041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
121200041001     C                   if        %subst(vlrppt:30:1) = 'S'
121300041001     C                   seton                                        51
121400041001     C                   else
121500041001     C                   setoff                                       51
121600041001     C                   endif
121700041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
121800041025     C* (naturalmente solo se nn già presente)
121900041025     C                   if        %subst(vlrppt:31:1) = 'S'
122000041025     C                   seton                                        52
122100041025     C                   else
122200041025     C                   setoff                                       52
122300041025     C                   endif
122400041001     C*
122500040423     C                   eval      wPosDaA = %subst(vlrppt:26:2)
122600030710     C                   move(p)   wPosDaA       wPosDa
122700040423     C                   eval      wLungA  = %subst(vlrppt:28:2)
122800030710     C                   move(p)   wLungA        wLung
122900030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
123000030710     C                   seton                                        60
123100030710     C                   else
123200050914     C                   if        vlrisv = 'UL'
123300050914     C                   movel     datcor        wDataOraAlfa
123400050914     C                   move      oracor        wDataOraAlfa
123500050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
123600050914     C                   seton                                        60
123700050914     C                   else
123800050914     C                   setoff                                       60
123900050914     C                   endif
124000030710     C                   endif
124100010606     C*
124200010606     C* CALCOLA LA DATA CORRENTE
124300010606     C                   time                    wn14             14 0
124400010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
124500040402     C                   movel     wn14          oracor            6 0          *ORA
124600010606     C                   z-add     wn8           g08dat
124700010606     C                   z-add     *zeros        g08inv
124800010606     C                   movel     '0'           g08err
124900010606     C                   call      'XSRDA8'
125000010606     C                   parm                    wlbda8
125100010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
125200000613     C*
125300000613     C                   ENDSR
