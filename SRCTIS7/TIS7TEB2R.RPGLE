000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200130115     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500030616     FEDIVABWR  O    E             DISK    usropn
000600990908
000700000512     D*------------
000800990920     D dscmz         e ds                  inz
000900990910     D psds           sds
001000990910     D  procname         *PROC
001100050118     D*
001200050118     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001300050118     D* 10+3
001400050118     D                 DS
001500050118     D  WI15                   1     10
001600050118     D  WD15                  11     13
001700050118     D  WASC15                 1     13
001800050118     D*  6+1
001900050118     D                 DS
002000050118     D  WI09                   1      6
002100050118     D  WD09                   7      7
002200050118     D  WASC09                 1      7
002300050118     D*  2+3
002400050118     D                 DS
002500050118     D  WI07                   1      2
002600050118     D  WD07                   3      5
002700050118     D  WASC07                 1      5
002800990920     D tivlrds       e ds                  extname(tivlr00f)
002900041025     D tisi95ds      e ds
003000990910     D esito           s              1
003100000724     D prmlit          s             10
003200000710     D prmfir          s             10
003300990921     D wrkesito        s                   like(esito)
003400000613     D rrnum           s              6  0 INZ(*zeros)
003500030616     D wNomeFile       s             30    INZ(*blanks)
003600030710     D wCMR            s             35    INZ(*blanks)
003700030710     D wPosDaA         s              2    INZ(*blanks)
003800030710     D wPosDa          s              2  0 INZ(*zeros)
003900030710     D wLungA          s              2    INZ(*blanks)
004000030710     D wLung           s              2  0 INZ(*zeros)
004100050914     D wDataOraAlfa    s             14    INZ(*blanks)
004200010606     D*------------------
004300010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004400010606     D*------------------
004500010606     D WLBDA8          DS                  INZ
004600010606     D  G08DAT                 1      8  0
004700010606     D  G08INV                 9     16  0
004800010606     D  G08ERR                17     17
004900010606     D  G08TGI                18     22  0
005000050118     D*---------------------------------------------------------
005100050118     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
005200050118     D*---------------------------------------------------------
005300050118     D                 ds
005400101209     D vhbatb                         1
005500101209     D vhbfgs                         3
005600101209     D vhbccm                         7
005700101209     D vhblnp                         3
005800101209     D vhbaas                         4
005900101209     D vhbmgs                         4
006000101209     D vhbnrs                         2
006100101209     D vhbnsp                         7
006200101209     D vhbcbo                         2
006300101209     D vhblna                         3
006400101209     D vhbrsd                        35
006500101209     D vhbrd2                        35
006600101209     D vhbind                        35
006700101209     D vhbcad                         9
006800101209     D vhblod                        35
006900101209     D vhbprd                         2
007000101209     D vhbnzd                         3
007100101209     D vhbgc1                         2
007200101209     D vhbgc2                         2
007300101209     D vhbctr                         3
007400101209     D vhbtsp                         1
007500101209     D vhbias                        13
007600101209     D vhbvas                         3
007700101209     D vhbnas                        15
007800101209     D vhbncl                         5
007900101209     D vhbpkb                         7
008000101209     D vhbvlb                         5
008100101209     D vhbqft                        13
008200101209     D vhbcas                        13
008300101209     D vhbtic                         2
008400101209     D vhbvca                         3
008500101209     D vhbgca                         2
008600101209     D vhbrmn                        15
008700101209     D vhbrma                        15
008800101209     D vhbncd                         7
008900101209     D vhbnca                         7
009000101209     D vhbxco                         1
009100101209     D vhbnot                        35
009200101209     D vhbnt2                        35
009300101209     D vhbznc                         2
009400101209     D vhbctm                         2
009500101209     D vhbffd                         1
009600101209     D vhbdcr                         8
009700101209     D vhbtcr                         1
009800101209     D vhbhcr                         4
009900101209     D vhbcts                         2
010000101209     D vhbftm                         1
010100101209     D vhbvmd                        13
010200101209     D vhbvad                         3
010300101209     D vhbgma                         2
010400101209     D vhbgga                         2
010500101209     D vhbgva                         2
010600101209     D vhbtc1                         1
010700101209     D vhbtc2                         1
010800101209     D vhbscl                         1
010900101209     D vhbant                         9
011000101209     D vhbrmo                        25
011100101209     D vhbcmo                         9
011200101209     D vhbnmo                         3
011300101209     D  X_rec                  1    486    inz
011400010606     D*
011500071018     D*-------------------
011600071018     D* COSTANTI
011700071018     D*-------------------
011800071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
011900071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
012000990908
012100000913     C                   reset                   rrnum
012200990921     C                   reset                   esito
012300990921     C                   reset                   wrkesito
012400010530     C*
012500010530     C                   exsr      opeini
012600010530     C                   exsr      rwvab
012700041025     C*
012800041025     C* Esegue lancio TISI95R solo x chiusura
012900041025     C                   CLEAR                   TISI95DS
013000041025     C                   EVAL      I95TLA = 'C'
013100041025     C                   CALL      'TISI95R'
013200041025     C                   PARM                    TISI95DS
013300010530     C*
013400010530     C                   seton                                        lr
013500010530
013600010530
013700010530
013800010530
013900010530     C*--------------------------------------------------------
014000010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
014100010530     C*--------------------------------------------------------
014200010530     C     PREELA        BEGSR
014300000724     C*
014400000724     C* SE OCCORRE SPEDIRE IN FILIALE
014500010530     C                   if        invfil <> *zeros and
014600010530     C                             flgGiro = '0'
014700010530     C*
014800010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
014900010530     C                   eval      flgGiro = '1'
015000010530     C*
015100010530     C                   endif
015200010530     C*
015300010530     C                   ENDSR
015400010530     C***
015500010530
015600010530
015700010530
015800010530
015900010530
016000010530
016100010530     C*--------------------------------------------------------
016200010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016300010530     C*--------------------------------------------------------
016400010530     C     ENDELA        BEGSR
016500010530     C*
016600010530     C                   ENDSR
016700010530     C***
016800990908
016900010530
017000010530
017100010530
017200910830     C*--------------------------------------------------------
017300030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
017400910830     C*--------------------------------------------------------
017500941229     C     RWVAB         BEGSR
017600030710     C*
017700030710     C                   clear                   §CTROK
017800030710     C                   clear                   §CTRMO
017900030710     C                   clear                   §CTRNO
018000990910     C*
018100030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
018200030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
018300030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
018400030710     C                   if        *in60 = *on
018500990914     C                   if        not %open(tivin00r)
018600990908     C                   open      tivin00r
018700990914     C                   endif
018800030616     C                   if        not %open(edivabwr)
018900030616     C                   open      edivabwr
019000990914     C                   endif
019100990910     C*
019200921023     C                   DO        *HIVAL
019300990913     C*
019400990915     C                   READ      tivin00r                               70
019500071018     C*
019600071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
019700071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
019800010618     C*
019900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020000010618     C                   if        vinflg = '1'
020100010618     C                   eval      flgOk = '1'
020200010618     C                   endif
020300010705     C*
020400010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
020500010705     C                   if        %subst(vindta:1:1) = '='
020600010705     C                   eval      %subst(vindta:1:1) = *blanks
020700010705     C                   endif
020800010705     C*
020900000905     C                   if        vindta > *blanks
021000000613     C                   add       1             rrnum
021100010601     C* ...elimino eventuali segni
021200010601     C     '+':' '       xlate     vindta        vindta
021300990913     C*
021400010601     C                   if        *in70 = *off and
021500990913     C                             (vinflg = *blanks
021600010711     C                              or vinflg = '0'
021700010711     C                              or vinflg = '2')
021800990913     C*
021900030616     C                   clear                   EDIVAB00
022000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022100010711     C                   if        vinflg = *blanks or vinflg = '0'
022200010711     C                   clear                   vinmsg
022300010711     C                   endif
022400010711     C*
022500050118     C                   eval      x_rec  = vindta
022600991022     C*
022700991022     C                   testb     x'3F'         vhbatb                   02
022800991022     C                   if        not *in02
022900991022     C*
023000971021     C                   SETOFF                                       303132
023100971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
023200971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
023300971023     C*    OPPURE TUTTI BLANKS
023400101209     C*
023500101209     C                   TESTN                   VHBFGS               30
023600101209     C  N30              TESTN                   VHBFGS                 30
023700101209     C  N30              TESTN                   VHBFGS                   30
023800101209     C                   MOVE      VHBFGS        W001A             1
023900101209     C     *IN30         IFEQ      *OFF
024000101209     C     W001A         ORLT      '0'
024100101209     C                   SETON                                        31
024200101209     C                   eval      vinmsg = %trimr(vinmsg)
024300101209     C                             + ' ' + 'VHBFGS'
024400101209     C                   ELSE
024500101209     C                   MOVE      VHBFGS        VABFGS
024600101209     C     VABFGS        IFLE      *ZEROS
024700101209     C                   SETON                                        31
024800101209     C                   eval      vinmsg = %trimr(vinmsg)
024900101209     C                             + ' ' + 'VHBFGS'
025000101209     C                   ENDIF
025100101209     C                   ENDIF
025200010530     C*
025300010530     C                   TESTN                   VHBLNP               30
025400010530     C  N30              TESTN                   VHBLNP                 30
025500010530     C  N30              TESTN                   VHBLNP                   30
025600010530     C                   MOVE      VHBLNP        W001A             1
025700010530     C     *IN30         IFEQ      *OFF
025800010530     C     W001A         ORLT      '0'
025900010530     C                   SETON                                        31
026000010530     C                   eval      vinmsg = %trimr(vinmsg)
026100010530     C                             + ' ' + 'VHBLNP'
026200010530     C                   ELSE
026300010530     C                   MOVE      VHBLNP        VABLNP
026400010530     C     VABLNP        IFLE      *ZEROS
026500010530     C                   SETON                                        31
026600010530     C                   eval      vinmsg = %trimr(vinmsg)
026700010530     C                             + ' ' + 'VHBLNP'
026800010530     C                   ENDIF
026900010530     C                   ENDIF
027000010530     C*
027100010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027200101209     C                   eval      depfil = VABFGS
027300010530     C                   exsr      repfil
027400010601     C                   if        depfil = invfil
027500021014     C                   if        vlrpoi = 999
027600101209     C                   MOVE(P)   invfil        VABFGS
027700021014     C                   else
027800021014     C                   MOVE(P)   vlrpoi        VABFGS
027900021014     C                   endif
028000010530     C*
028100010530     C                   exsr      PREELA
028200010530     C*
028300971023     C                   TESTN                   VHBCCM               30
028400971023     C  N30              TESTN                   VHBCCM                 30
028500971023     C  N30              TESTN                   VHBCCM                   30
028600971021     C                   MOVE      VHBCCM        W001A             1
028700971021     C     *IN30         IFEQ      *OFF
028800971021     C     W001A         ORLT      '0'
028900971021     C                   SETON                                        31
029000010604     C                   eval      vinmsg = %trimr(vinmsg)
029100010604     C                             + ' ' + 'VHBCCM'
029200971021     C                   ELSE
029300971021     C                   MOVE      VHBCCM        VABCCM
029400971024     C     VABCCM        IFLE      *ZEROS
029500971024     C                   SETON                                        31
029600010604     C                   eval      vinmsg = %trimr(vinmsg)
029700010604     C                             + ' ' + 'VHBCCM'
029800971024     C                   ENDIF
029900971021     C                   ENDIF
030000971021     C*
030100971023     C                   TESTN                   VHBAAS               30
030200971023     C  N30              TESTN                   VHBAAS                 30
030300971023     C  N30              TESTN                   VHBAAS                   30
030400971021     C                   MOVE      VHBAAS        W001A             1
030500971021     C     *IN30         IFEQ      *OFF
030600971021     C     W001A         ORLT      '0'
030700971023     C     W001A         ANDNE     *BLANK
030800971021     C                   SETON                                        32
030900010606     C                   MOVEL     datcor        VABAAS
031000010604     C                   eval      vinmsg = %trimr(vinmsg)
031100010604     C                             + ' ' + 'VHBAAS'
031200971021     C                   ELSE
031300971021     C                   MOVE      VHBAAS        VABAAS
031400971021     C                   ENDIF
031500971021     C*
031600971023     C                   TESTN                   VHBMGS               30
031700971023     C  N30              TESTN                   VHBMGS                 30
031800971023     C  N30              TESTN                   VHBMGS                   30
031900971021     C                   MOVE      VHBMGS        W001A             1
032000971021     C     *IN30         IFEQ      *OFF
032100971021     C     W001A         ORLT      '0'
032200971023     C     W001A         ANDNE     *BLANK
032300971021     C                   SETON                                        32
032400010601     C                   MOVE      '0101'        VABMGS
032500010604     C                   eval      vinmsg = %trimr(vinmsg)
032600010604     C                             + ' ' + 'VHBMGS'
032700971021     C                   ELSE
032800971021     C                   MOVE      VHBMGS        VABMGS
032900971021     C                   ENDIF
033000971021     C*
033100971023     C                   TESTN                   VHBNRS               30
033200971023     C  N30              TESTN                   VHBNRS                 30
033300971023     C  N30              TESTN                   VHBNRS                   30
033400971021     C                   MOVE      VHBNRS        W001A             1
033500971021     C     *IN30         IFEQ      *OFF
033600971021     C     W001A         ORLT      '0'
033700971023     C     W001A         ANDNE     *BLANK
033800971021     C                   SETON                                        32
033900971021     C                   MOVE      *ZERO         VABNRS
034000010604     C                   eval      vinmsg = %trimr(vinmsg)
034100010604     C                             + ' ' + 'VHBNRS'
034200971021     C                   ELSE
034300971021     C                   MOVE      VHBNRS        VABNRS
034400971021     C                   ENDIF
034500971021     C*
034600971023     C                   TESTN                   VHBNSP               30
034700971023     C  N30              TESTN                   VHBNSP                 30
034800971023     C  N30              TESTN                   VHBNSP                   30
034900971021     C                   MOVE      VHBNSP        W001A             1
035000971021     C     *IN30         IFEQ      *OFF
035100971021     C     W001A         ORLT      '0'
035200971023     C     W001A         ANDNE     *BLANK
035300971021     C                   SETON                                        32
035400971021     C                   MOVE      *ZERO         VABNSP
035500010604     C                   eval      vinmsg = %trimr(vinmsg)
035600010604     C                             + ' ' + 'VHBNSP'
035700971021     C                   ELSE
035800971021     C                   MOVE      VHBNSP        VABNSP
035900971021     C                   ENDIF
036000971021     C*
036100971023     C                   TESTN                   VHBLNA               30
036200971023     C  N30              TESTN                   VHBLNA                 30
036300971023     C  N30              TESTN                   VHBLNA                   30
036400971021     C                   MOVE      VHBLNA        W001A             1
036500971021     C     *IN30         IFEQ      *OFF
036600971021     C     W001A         ORLT      '0'
036700971023     C     W001A         ANDNE     *BLANK
036800971021     C                   SETON                                        32
036900971021     C                   MOVE      *ZERO         VABLNA
037000010604     C                   eval      vinmsg = %trimr(vinmsg)
037100010604     C                             + ' ' + 'VHBLNA'
037200971021     C                   ELSE
037300971021     C                   MOVE      VHBLNA        VABLNA
037400971021     C                   ENDIF
037500971021     C*
037600971023     C                   TESTN                   VHBCTR               30
037700971023     C  N30              TESTN                   VHBCTR                 30
037800971023     C  N30              TESTN                   VHBCTR                   30
037900971021     C                   MOVE      VHBCTR        W001A             1
038000971021     C     *IN30         IFEQ      *OFF
038100971021     C     W001A         ORLT      '0'
038200971023     C     W001A         ANDNE     *BLANK
038300971021     C                   SETON                                        32
038400971021     C                   MOVE      *ZERO         VABCTR
038500010604     C                   eval      vinmsg = %trimr(vinmsg)
038600010604     C                             + ' ' + 'VHBCTR'
038700971021     C                   ELSE
038800971021     C                   MOVE      VHBCTR        VABCTR
038900971021     C                   ENDIF
039000941229     C*
039100971021     C                   MOVE      *ZERO         VABIAS
039200941229     C                   MOVE      VHBIAS        WASC15
039300971023     C                   TESTN                   WI15                 30
039400971023     C  N30              TESTN                   WI15                   30
039500971023     C  N30              TESTN                   WI15                     30
039600971021     C                   MOVE      WI15          W001A             1
039700971021     C     *IN30         IFEQ      *OFF
039800971021     C     W001A         ORLT      '0'
039900971023     C     W001A         ANDNE     *BLANK
040000971021     C                   SETON                                        32
040100971021     C                   MOVE      *ZERO         VABIAS
040200010604     C                   eval      vinmsg = %trimr(vinmsg)
040300010604     C                             + ' ' + 'VHBIAS'
040400971021     C                   ELSE
040500971021     C                   MOVEL     WI15          VABIAS
040600971023     C                   TESTN                   WD15                 30
040700971023     C  N30              TESTN                   WD15                   30
040800971023     C  N30              TESTN                   WD15                     30
040900971021     C                   MOVE      WD15          W001A             1
041000971021     C     *IN30         IFEQ      *OFF
041100971021     C     W001A         ORLT      '0'
041200971023     C     W001A         ANDNE     *BLANK
041300971021     C                   SETON                                        32
041400971021     C                   MOVE      *ZERO         VABIAS
041500010604     C                   eval      vinmsg = %trimr(vinmsg)
041600010604     C                             + ' ' + 'VHBIAS'
041700971021     C                   ELSE
041800971021     C                   MOVE      WD15          VABIAS
041900971021     C                   ENDIF
042000050118     C                   ENDIF
042100941229     C*
042200971023     C                   TESTN                   VHBNCL               30
042300971023     C  N30              TESTN                   VHBNCL                 30
042400971023     C  N30              TESTN                   VHBNCL                   30
042500971021     C                   MOVE      VHBNCL        W001A             1
042600971021     C     *IN30         IFEQ      *OFF
042700971021     C     W001A         ORLT      '0'
042800971021     C                   SETON                                        32
042900971021     C                   MOVE      *ZERO         VABNCL
043000010604     C                   eval      vinmsg = %trimr(vinmsg)
043100010604     C                             + ' ' + 'VHBNCL'
043200971021     C                   ELSE
043300971021     C                   MOVE      VHBNCL        VABNCL
043400971021     C                   ENDIF
043500941229     C*
043600971021     C                   MOVE      *ZERO         VABPKB
043700971021     C                   MOVE      VHBPKB        WASC09
043800010719     C                   IF        WI09 = *blanks
043900010719     C                   EVAL      WI09 = *zeros
044000010719     C                   ENDIF
044100971023     C                   TESTN                   WI09                 30
044200971023     C  N30              TESTN                   WI09                   30
044300971023     C  N30              TESTN                   WI09                     30
044400971021     C                   MOVE      WI09          W001A             1
044500971021     C     *IN30         IFEQ      *OFF
044600971021     C     W001A         ORLT      '0'
044700971021     C                   SETON                                        32
044800971021     C                   MOVE      *ZERO         VABPKB
044900010604     C                   eval      vinmsg = %trimr(vinmsg)
045000010604     C                             + ' ' + 'VHBPKB'
045100971021     C                   ELSE
045200971021     C                   MOVEL     WI09          VABPKB
045300010719     C                   IF        WD09 = *blanks
045400010719     C                   EVAL      WD09 = *zeros
045500010719     C                   ENDIF
045600971023     C                   TESTN                   WD09                 30
045700971023     C  N30              TESTN                   WD09                     30
045800971021     C                   MOVE      WD09          W001A             1
045900971021     C     *IN30         IFEQ      *OFF
046000971021     C     W001A         ORLT      '0'
046100971023     C     W001A         ANDNE     *BLANK
046200971021     C                   SETON                                        32
046300971021     C                   MOVE      *ZERO         VABPKB
046400010604     C                   eval      vinmsg = %trimr(vinmsg)
046500010604     C                             + ' ' + 'VHBPKB'
046600971021     C                   ELSE
046700971021     C                   MOVE      WD09          VABPKB
046800971021     C                   ENDIF
046900050118     C                   ENDIF
047000971021     C*
047100041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
047200041001     C                   IF        *IN51 = *OFF
047300971021     C                   MOVE      *ZERO         VABVLB
047400971021     C                   MOVE      VHBVLB        WASC07
047500010719     C                   IF        WI07 = *blanks
047600010719     C                   EVAL      WI07 = *zeros
047700010719     C                   ENDIF
047800971023     C                   TESTN                   WI07                 30
047900971023     C  N30              TESTN                   WI07                   30
048000971023     C  N30              TESTN                   WI07                     30
048100971021     C                   MOVE      WI07          W001A             1
048200971021     C     *IN30         IFEQ      *OFF
048300971021     C     W001A         ORLT      '0'
048400971023     C     W001A         ANDNE     *BLANK
048500971021     C                   SETON                                        32
048600971021     C                   MOVE      *ZERO         VABVLB
048700010604     C                   eval      vinmsg = %trimr(vinmsg)
048800010604     C                             + ' ' + 'VHBVLB'
048900971021     C                   ELSE
049000971021     C                   MOVEL     WI07          VABVLB
049100010719     C                   IF        WD07 = *blanks
049200010719     C                   EVAL      WD07 = *zeros
049300010719     C                   ENDIF
049400971023     C                   TESTN                   WD07                 30
049500971023     C  N30              TESTN                   WD07                   30
049600971023     C  N30              TESTN                   WD07                     30
049700971021     C                   MOVE      WD07          W001A             1
049800971021     C     *IN30         IFEQ      *OFF
049900971021     C     W001A         ORLT      '0'
050000971023     C     W001A         ANDNE     *BLANK
050100971021     C                   SETON                                        32
050200971021     C                   MOVE      *ZERO         VABVLB
050300010604     C                   eval      vinmsg = %trimr(vinmsg)
050400010604     C                             + ' ' + 'VHBVLB'
050500971021     C                   ELSE
050600971021     C                   MOVE      WD07          VABVLB
050700971021     C                   ENDIF
050800050118     C                   ENDIF
050900041001     C                   ELSE
051000041001     C* ...altrimenti forzo azzeramento del volume
051100041001     C                   Z-ADD     *zeros        VABVLB
051200041001     C                   ENDIF
051300941229     C*
051400971021     C                   MOVE      *ZERO         VABQFT
051500971021     C                   MOVE      VHBQFT        WASC15
051600010719     C                   IF        WI15 = *blanks
051700010719     C                   EVAL      WI15 = *zeros
051800010719     C                   ENDIF
051900971023     C                   TESTN                   WI15                 30
052000971023     C  N30              TESTN                   WI15                   30
052100971023     C  N30              TESTN                   WI15                     30
052200971021     C                   MOVE      WI15          W001A             1
052300971021     C     *IN30         IFEQ      *OFF
052400971021     C     W001A         ORLT      '0'
052500971023     C     W001A         ANDNE     *BLANK
052600971021     C                   SETON                                        32
052700971021     C                   MOVE      *ZERO         VABQFT
052800010604     C                   eval      vinmsg = %trimr(vinmsg)
052900010604     C                             + ' ' + 'VHBQFT'
053000971021     C                   ELSE
053100971021     C                   MOVEL     WI15          VABQFT
053200010719     C                   IF        WD15 = *blanks
053300010719     C                   EVAL      WD15 = *zeros
053400010719     C                   ENDIF
053500971023     C                   TESTN                   WD15                 30
053600971023     C  N30              TESTN                   WD15                   30
053700971023     C  N30              TESTN                   WD15                     30
053800971021     C                   MOVE      WD15          W001A             1
053900971021     C     *IN30         IFEQ      *OFF
054000971021     C     W001A         ORLT      '0'
054100971023     C     W001A         ANDNE     *BLANK
054200971021     C                   SETON                                        32
054300971021     C                   MOVE      *ZERO         VABQFT
054400010604     C                   eval      vinmsg = %trimr(vinmsg)
054500010604     C                             + ' ' + 'VHBQFT'
054600971021     C                   ELSE
054700971021     C                   MOVE      WD15          VABQFT
054800971021     C                   ENDIF
054900050118     C                   ENDIF
055000941229     C*
055100971021     C                   MOVE      *ZERO         VABCAS
055200971021     C                   MOVE      VHBCAS        WASC15
055300010719     C                   IF        WI15 = *blanks
055400010719     C                   EVAL      WI15 = *zeros
055500010719     C                   ENDIF
055600971023     C                   TESTN                   WI15                 30
055700971023     C  N30              TESTN                   WI15                   30
055800971023     C  N30              TESTN                   WI15                     30
055900971021     C                   MOVE      WI15          W001A             1
056000971021     C     *IN30         IFEQ      *OFF
056100971021     C     W001A         ORLT      '0'
056200971023     C     W001A         ANDNE     *BLANK
056300971021     C                   SETON                                        32
056400971021     C                   MOVE      *ZERO         VABCAS
056500010604     C                   eval      vinmsg = %trimr(vinmsg)
056600010604     C                             + ' ' + 'VHBCAS'
056700971021     C                   ELSE
056800971021     C                   MOVEL     WI15          VABCAS
056900010719     C                   IF        WD15 = *blanks
057000010719     C                   EVAL      WD15 = *zeros
057100010719     C                   ENDIF
057200971023     C                   TESTN                   WD15                 30
057300971023     C  N30              TESTN                   WD15                   30
057400971023     C  N30              TESTN                   WD15                     30
057500971021     C                   MOVE      WD15          W001A             1
057600971021     C     *IN30         IFEQ      *OFF
057700971021     C     W001A         ORLT      '0'
057800971023     C     W001A         ANDNE     *BLANK
057900971021     C                   SETON                                        32
058000971021     C                   MOVE      *ZERO         VABCAS
058100010604     C                   eval      vinmsg = %trimr(vinmsg)
058200010604     C                             + ' ' + 'VHBCAS'
058300971021     C                   ELSE
058400971021     C                   MOVE      WD15          VABCAS
058500971021     C                   ENDIF
058600050118     C                   ENDIF
058700971021     C*
058800971023     C                   TESTN                   VHBRMN               30
058900971023     C  N30              TESTN                   VHBRMN                 30
059000971023     C  N30              TESTN                   VHBRMN                   30
059100971021     C                   MOVE      VHBRMN        W001A             1
059200971021     C     *IN30         IFEQ      *OFF
059300971021     C     W001A         ORLT      '0'
059400971021     C                   SETON                                        32
059500971021     C                   MOVE      *ZERO         VABRMN
059600010604     C                   eval      vinmsg = %trimr(vinmsg)
059700010604     C                             + ' ' + 'VHBRMN'
059800971021     C                   ELSE
059900971021     C                   MOVE      VHBRMN        VABRMN
060000971021     C                   ENDIF
060100971021     C*
060200971023     C                   TESTN                   VHBNCD               30
060300971023     C  N30              TESTN                   VHBNCD                 30
060400971023     C  N30              TESTN                   VHBNCD                   30
060500971021     C                   MOVE      VHBNCD        W001A             1
060600971021     C     *IN30         IFEQ      *OFF
060700971021     C     W001A         ORLT      '0'
060800971023     C     W001A         ANDNE     *BLANK
060900971021     C                   SETON                                        32
061000971021     C                   MOVE      *ZERO         VABNCD
061100010604     C                   eval      vinmsg = %trimr(vinmsg)
061200010604     C                             + ' ' + 'VHBNCD'
061300971021     C                   ELSE
061400971021     C                   MOVE      VHBNCD        VABNCD
061500971021     C                   ENDIF
061600971021     C*
061700971023     C                   TESTN                   VHBNCA               30
061800971023     C  N30              TESTN                   VHBNCA                 30
061900971023     C  N30              TESTN                   VHBNCA                   30
062000971021     C                   MOVE      VHBNCA        W001A             1
062100971021     C     *IN30         IFEQ      *OFF
062200971021     C     W001A         ORLT      '0'
062300971023     C     W001A         ANDNE     *BLANK
062400971021     C                   SETON                                        32
062500971021     C                   MOVE      *ZERO         VABNCA
062600010604     C                   eval      vinmsg = %trimr(vinmsg)
062700010604     C                             + ' ' + 'VHBNCA'
062800971021     C                   ELSE
062900971021     C                   MOVE      VHBNCA        VABNCA
063000971021     C                   ENDIF
063100971021     C*
063200971023     C                   TESTN                   VHBZNC               30
063300971023     C  N30              TESTN                   VHBZNC                 30
063400971023     C  N30              TESTN                   VHBZNC                   30
063500971021     C                   MOVE      VHBZNC        W001A             1
063600971021     C     *IN30         IFEQ      *OFF
063700971021     C     W001A         ORLT      '0'
063800971023     C     W001A         ANDNE     *BLANK
063900971021     C                   SETON                                        32
064000971021     C                   MOVE      *ZERO         VABZNC
064100010604     C                   eval      vinmsg = %trimr(vinmsg)
064200010604     C                             + ' ' + 'VHBZNC'
064300971021     C                   ELSE
064400971021     C                   MOVE      VHBZNC        VABZNC
064500971021     C                   ENDIF
064600971021     C*
064700971023     C                   TESTN                   VHBDCR               30
064800971023     C  N30              TESTN                   VHBDCR                 30
064900971023     C  N30              TESTN                   VHBDCR                   30
065000971021     C                   MOVE      VHBDCR        W001A             1
065100971021     C     *IN30         IFEQ      *OFF
065200971021     C     W001A         ORLT      '0'
065300971023     C     W001A         ANDNE     *BLANK
065400971021     C                   SETON                                        32
065500971021     C                   MOVE      *ZERO         VABDCR
065600010604     C                   eval      vinmsg = %trimr(vinmsg)
065700010604     C                             + ' ' + 'VHBDCR'
065800971021     C                   ELSE
065900971021     C                   MOVE      VHBDCR        VABDCR
066000971021     C                   ENDIF
066100971021     C*
066200971023     C                   TESTN                   VHBHCR               30
066300971023     C  N30              TESTN                   VHBHCR                 30
066400971023     C  N30              TESTN                   VHBHCR                   30
066500971021     C                   MOVE      VHBHCR        W001A             1
066600971021     C     *IN30         IFEQ      *OFF
066700971021     C     W001A         ORLT      '0'
066800971023     C     W001A         ANDNE     *BLANK
066900971021     C                   SETON                                        32
067000971021     C                   MOVE      *ZERO         VABHCR
067100010604     C                   eval      vinmsg = %trimr(vinmsg)
067200010604     C                             + ' ' + 'VHBHCR'
067300971021     C                   ELSE
067400971021     C                   MOVE      VHBHCR        VABHCR
067500971021     C                   ENDIF
067600941229     C*
067700971021     C                   MOVE      *ZERO         VABVMD
067800971021     C                   MOVE      VHBVMD        WASC15
067900010719     C                   IF        WI15 = *blanks
068000010719     C                   EVAL      WI15 = *zeros
068100010719     C                   ENDIF
068200971023     C                   TESTN                   WI15                 30
068300971023     C  N30              TESTN                   WI15                   30
068400971023     C  N30              TESTN                   WI15                     30
068500971021     C                   MOVE      WI15          W001A             1
068600971021     C     *IN30         IFEQ      *OFF
068700971021     C     W001A         ORLT      '0'
068800971023     C     W001A         ANDNE     *BLANK
068900971021     C                   SETON                                        32
069000971021     C                   MOVE      *ZERO         VABVMD
069100010604     C                   eval      vinmsg = %trimr(vinmsg)
069200010604     C                             + ' ' + 'VHBVMD'
069300971021     C                   ELSE
069400971021     C                   MOVEL     WI15          VABVMD
069500010719     C                   IF        WD15 = *blanks
069600010719     C                   EVAL      WD15 = *zeros
069700010719     C                   ENDIF
069800971023     C                   TESTN                   WD15                 30
069900971023     C  N30              TESTN                   WD15                   30
070000971023     C  N30              TESTN                   WD15                     30
070100971021     C                   MOVE      WD15          W001A             1
070200971021     C     *IN30         IFEQ      *OFF
070300971021     C     W001A         ORLT      '0'
070400971023     C     W001A         ANDNE     *BLANK
070500971021     C                   SETON                                        32
070600971021     C                   MOVE      *ZERO         VABVMD
070700010604     C                   eval      vinmsg = %trimr(vinmsg)
070800010604     C                             + ' ' + 'VHBVMD'
070900971021     C                   ELSE
071000971021     C                   MOVE      WD15          VABVMD
071100971021     C                   ENDIF
071200050118     C                   ENDIF
071300971021     C*
071400971023     C                   TESTN                   VHBANT               30
071500971023     C  N30              TESTN                   VHBANT                 30
071600971023     C  N30              TESTN                   VHBANT                   30
071700971021     C                   MOVE      VHBANT        W001A             1
071800971021     C     *IN30         IFEQ      *OFF
071900971021     C     W001A         ORLT      '0'
072000971023     C     W001A         ANDNE     *BLANK
072100971021     C                   SETON                                        32
072200971021     C                   MOVE      *ZERO         VABANT
072300010604     C                   eval      vinmsg = %trimr(vinmsg)
072400010604     C                             + ' ' + 'VHBANT'
072500971021     C                   ELSE
072600971021     C                   MOVE      VHBANT        VABANT
072700971021     C                   ENDIF
072800971021     C*
072900971021     C* CAMPI ALFABETICI:
073000971021     C                   MOVE      VHBATB        VABATB
073100971021     C                   MOVE      VHBCBO        VABCBO
073200971021     C                   MOVE      VHBRSD        VABRSD
073300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
073400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
073500020117     C* ==
073600971021     C                   MOVE      VHBRD2        VABRD2
073700971021     C                   MOVE      VHBIND        VABIND
073800971021     C                   MOVE      VHBCAD        VABCAD
073900971021     C                   MOVE      VHBLOD        VABLOD
074000971021     C                   MOVE      VHBPRD        VABPRD
074100971021     C                   MOVE      VHBNZD        VABNZD
074200971021     C                   MOVE      VHBGC1        VABGC1
074300971021     C                   MOVE      VHBGC2        VABGC2
074400971021     C                   MOVE      VHBTSP        VABTSP
074500971021     C                   MOVE      VHBVAS        VABVAS
074600971021     C                   MOVE      VHBNAS        VABNAS
074700971021     C                   MOVE      VHBTIC        VABTIC
074800971021     C                   MOVE      VHBVCA        VABVCA
074900971021     C                   MOVE      VHBGCA        VABGCA
075000971021     C                   MOVE      VHBRMA        VABRMA
075100971021     C                   MOVE      VHBXCO        VABXCO
075200971021     C                   MOVE      VHBNOT        VABNOT
075300971021     C                   MOVE      VHBNT2        VABNT2
075400971021     C                   MOVE      VHBCTM        VABCTM
075500971021     C                   MOVE      VHBFFD        VABFFD
075600971021     C                   MOVE      VHBTCR        VABTCR
075700971021     C                   MOVE      VHBCTS        VABCTS
075800971021     C                   MOVE      VHBFTM        VABFTM
075900971021     C                   MOVE      VHBVAD        VABVAD
076000971021     C                   MOVE      VHBGMA        VABGMA
076100971021     C                   MOVE      VHBGGA        VABGGA
076200971021     C                   MOVE      VHBGVA        VABGVA
076300971021     C                   MOVE      VHBTC1        VABTC1
076400971021     C                   MOVE      VHBTC2        VABTC2
076500971021     C                   MOVE      VHBSCL        VABSCL
076600971021     C                   MOVE      VHBRMO        VABRMO
076700971021     C                   MOVE      VHBCMO        VABCMO
076800971021     C                   MOVE      VHBNMO        VABNMO
076900971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
077000971021     C* NELLA NAZIONE ORIGINALE
077100971021     C                   MOVE      VHBNMO        WA1D              1
077200971021     C                   MOVEL     VHBNMO        WA1S              1
077300971021     C     WA1D          IFNE      *BLANK
077400971021     C     WA1S          ANDEQ     *BLANK
077500971021     C                   MOVE      *BLANK        VABNMO
077600030616     C                   ENDIF
077700030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
077800030710     C                   EVAL      VABCMR = wCMR
077900030616     C                   EVAL      VABDCM = DATCOR
078000030616     C                   EVAL      VABDTS = DATCOR
078100040402     C                   EVAL      VABHMS = ORACOR
078200030616     C                   EVAL      VABCNT = 1
078300041025     C*
078400041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
078500041025     C                   IF        *IN52 = *ON
078600041025     C* Reperisco la provincia dal CAP e dalla località
078700041025     C                   IF        VABCAD <> *blanks AND
078800050907     C                             VABPRD  = *blanks AND
078900050907     C                             VABNZD  = *blanks
079000041025     C                   CLEAR                   TISI95DS
079100041025     C                   EVAL      I95TCN = '3'
079200041025     C                   Z-ADD     datcor        I95DAT
079300041025     C                   EVAL      I95CAP = VABCAD
079400041025     C                   EVAL      I95LOC = VABLOD
079500041025     C                   CALL      'TISI95R'
079600041025     C                   PARM                    TISI95DS
079700041025     C                   EVAL      VABPRD = O95PRV
079800041025     C                   ENDIF
079900041025     C                   ENDIF
080000100419     C*
080100100419     C* Forzatura su campi DIVISE
080200100419     C                   IF        VABVAS = *blanks
080300100419     C                   EVAL      VABVAS = 'EUR'
080400100419     C                   ENDIF
080500100419     C                   IF        VABVCA = *blanks
080600100419     C                   EVAL      VABVCA = 'EUR'
080700100419     C                   ENDIF
080800100419     C                   IF        VABVAD = *blanks
080900100419     C                   EVAL      VABVAD = 'EUR'
081000100419     C                   ENDIF
081100930409     C*
081200010604     C  N31              ADD       1             §CTROK            7 0
081300010604     C   32              ADD       1             §CTRMO            7 0
081400010604     C   31              ADD       1             §CTRNO            7 0
081500030616     C  N31              WRITE     EDIVAB00
081600990910     C*
081700010601     C                   if        *in31 = *off and
081800010601     C                             *in32 = *off
081900990910     C                   eval      vinflg = '1'
082000990910     C                   else
082100990910     C                   eval      vinflg = '2'
082200990910     C                   endif
082300991022     C                   endif
082400990910     C                   endif
082500010601     C*
082600010601     C                   endif
082700000905     C*
082800000905     C                   else
082900000905     C                   eval      vinflg = '1'
083000000905     C                   endif
083100000905     C*
083200000905     C  N70              update    tivin000
083300991022     C*
083400991022     C  N70              ENDdo
083500010530     C
083600010530     C*
083700010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
083800010530     C                   if        cntNonEl = *zeros or
083900010530     C                             flgMulti = '0'
084000990910     C* Se non ci sono record con errori ...
084100010530     C                   if        §ctrno = 0 and
084200010601     C                             §ctrmo = 0 and
084300010530     C                             flgStato <> '2'
084400990910     C* ... restituisco esito OK.
084500990921     C                   eval      wrkesito = '0'
084600990910     C                   else
084700010601     C                   if        §ctrok > 0
084800990921     C                   eval      wrkesito = '1'
084900000710     C                   else
085000010615     C                   if        flgOk = '0'
085100010615     C                   eval      wrkesito = '2'
085200010615     C                   else
085300010615     C                   eval      wrkesito = '6'
085400010615     C                   endif
085500990910     C                   endif
085600000710     C                   endif
085700010530     C                   else
085800010530     C                   eval      wrkesito = '9'
085900010530     C                   endif
086000030710     C*
086100030710 X  1C                   else
086200030710     C                   eval      wrkesito = '8'
086300030710     C                   eval      flgGiro = '1'
086400030710 E  1C                   endif
086500030710     C*
086600990914     C                   if        %open(tivin00r)
086700990908     C                   close     tivin00r
086800990914     C                   endif
086900030616     C                   if        %open(edivabwr)
087000030616     C                   close     edivabwr
087100990914     C                   endif
087200010530     C*
087300010530     C                   if        vlrpoi <> 999
087400010530     C                   eval      invfil = vlrpoi
087500010530     C                   endif
087600990910     C*
087700990920     C                   if        §ctrok > 0
087800010530     C                             and invfil > *zeros
087900000613     C                   exsr      invio
088000990920     C                   endif
088100010612     C*
088200010612     C                   if        flgGiro = '1'
088300010612     C                   exsr      endela
088400010612     C                   endif
088500990920     C*
088600910830     C                   ENDSR
088700000613     C***
088800990920
088900010530
089000010530
089100010530
089200010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
089300010530     C     repfil        BEGSR
089400010530     C*
089500010530     C                   if        invfil = *zeros and
089600010530     C                             depfil > *zeros and
089700010613     C                             (vinflg = *blanks or
089800010613     C                              vinflg = *zeros)
089900010530     C
090000010530     C                   eval      invfil = depfil
090100010530     C                   endif
090200010530     C*
090300010530     C                   if        depfil <> invfil and
090400010530     C                             invfil > *zeros
090500010530     C                   eval      flgMulti = '1'
090600010530     C                   if        vinflg = *blanks
090700010530     C                   add       1             cntNonEl
090800010530     C                   endif
090900010530     C                   endif
091000010530     C*
091100010530     C                   if        vinflg = '2'
091200010530     C                   eval      flgStato = '2'
091300010530     C                   endif
091400010530     C*
091500010530     C                   ENDSR
091600010530     C***
091700010530
091800010530
091900010530
092000010530
092100990920      /TITLE Invio dei dati al punto operativo.
092200000613     C     invio         BEGSR
092300990920     C*
092400990920     C                   reset                   dscmz
092500030618     C                   move      invfil        cmzdst
092600030618     C***                move      '789'         cmzdst
092700990920     C                   eval      cmzfld = vlrfou
092800990920     C                   eval      cmzmbd = vlrhdl
092900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
093000000710     C                   if        prmfir = *blanks
093100030616     C                   eval      cmzfla = 'EDIVAB0F'
093200030616     C                   eval      cmzmba = 'EDIVAB0F'
093300000710     C                   else
093400000710     C                   eval      cmzfla = prmfir
093500000710     C                   eval      cmzmba = prmfir
093600000710     C                   endif
093700990920     C                   eval      cmznrr = *zeros
093800990920     C                   move      §ctrok        cmznrr
093900021003     C                   eval      cmzlba = vlrfl1
094000990920     C                   call(e)   'TIS711C'
094100990920     C                   parm                    dscmz
094200990921     C                   parm      *blanks       esito
094300990920     C                   if        %error
094400990920     C                             or cmzerr = '1'
094500990921     C                             or esito  = '1'
094600000710     C                   eval      wrkesito = '3'
094700990920     C                   endif
094800990920     C*
094900000613     C                   ENDSR
095000000613     C***
095100010530
095200010530
095300010530
095400010530
095500010530      /TITLE Invio dei dati al punto operativo.
095600010530     C     opeini        BEGSR
095700010530     C*
095800010530     C* Inizializzo flag e contatori operativi
095900010530     C                   movel     '0'           flgGiro           1
096000010530     C                   movel     '0'           flgMulti          1
096100010530     C                   movel     '1'           flgStato          1
096200010615     C                   movel     '0'           flgOk             1
096300010530     C                   z-add     *zeros        cntNonEl         10 0
096400010530     C                   z-add     *zeros        depfil            3 0
096500010530     C                   z-add     *zeros        invfil            3 0
096600010530     C*
096700010530     C                   ENDSR
096800010530     C***
096900010530
097000010530
097100010530
097200990910
097300000613     C     *inzsr        BEGSR
097400990910     C*
097500990910     C     *entry        plist
097600990920     C                   parm                    tivlrds
097700990921     C                   parm      wrkesito      esito
097800000724     C                   parm                    prmlit
097900000710     C                   parm                    prmfir
098000030616     C*
098100030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
098200030710     C                   move(p)   vlrMSG        wNomeFile
098300030710     C*
098400030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
098500030710     C* del codice CMR dal nome del file importato.
098600030710     C                   if        wNomeFile <> *blanks AND
098700030710     C                             vlrppt<>*blanks
098800041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
098900030710     C                   if        %subst(vlrppt:1:1) = 'S'
099000030710     C                   seton                                        50
099100030710     C                   else
099200030710     C                   setoff                                       50
099300030710     C                   endif
099400041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
099500041001     C                   if        %subst(vlrppt:6:1) = 'S'
099600041001     C                   seton                                        51
099700041001     C                   else
099800041001     C                   setoff                                       51
099900041001     C                   endif
100000041025     C*
100100041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
100200041025     C* (naturalmente solo se nn già presente)
100300041025     C                   if        %subst(vlrppt:7:1) = 'S'
100400041025     C                   seton                                        52
100500041025     C                   else
100600041025     C                   setoff                                       52
100700041025     C                   endif
100800041001     C*
100900030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
101000030710     C                   move(p)   wPosDaA       wPosDa
101100030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
101200030710     C                   move(p)   wLungA        wLung
101300030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
101400030710     C                   seton                                        60
101500030710     C                   else
101600050914     C                   if        vlrisv = 'UL'
101700050914     C                   movel     datcor        wDataOraAlfa
101800050914     C                   move      oracor        wDataOraAlfa
101900050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
102000050914     C                   seton                                        60
102100050914     C                   else
102200030710     C                   setoff                                       60
102300050914     C                   endif
102400030710     C                   endif
102500010606     C*
102600010606     C* CALCOLA LA DATA CORRENTE
102700010606     C                   time                    wn14             14 0
102800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
102900040402     C                   movel     wn14          oracor            6 0          *ORA
103000010606     C                   z-add     wn8           g08dat
103100010606     C                   z-add     *zeros        g08inv
103200010606     C                   movel     '0'           g08err
103300010606     C                   call      'XSRDA8'
103400010606     C                   parm                    wlbda8
103500010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
103600000613     C*
103700000613     C                   ENDSR
103800000613     C***
