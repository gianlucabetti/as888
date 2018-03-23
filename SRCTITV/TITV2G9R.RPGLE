000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500110705     Fcnaco00f  if   e           k disk
000600030616     FEDIVABWR  O    E             DISK    usropn
000700990908
000800000512     D*------------
000900990920     D dscmz         e ds                  inz
001000990910     D psds           sds
001100990910     D  procname         *PROC
001200050118     D*
001300050118     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001400050118     D* 10+3
001500050118     D                 DS
001600050118     D  WI15                   1     10
001700050118     D  WD15                  11     13
001800050118     D  WASC15                 1     13
001900050118     D*  6+1
002000050118     D                 DS
002100050118     D  WI09                   1      6
002200050118     D  WD09                   7      7
002300050118     D  WASC09                 1      7
002400050118     D*  2+3
002500050118     D                 DS
002600050118     D  WI07                   1      2
002700050118     D  WD07                   3      5
002800050118     D  WASC07                 1      5
002900990920     D tivlrds       e ds                  extname(tivlr00f)
003000041025     D tisi95ds      e ds
003100990910     D esito           s              1
003200000724     D prmlit          s             10
003300000710     D prmfir          s             10
003400990921     D wrkesito        s                   like(esito)
003500990915     D wrkdata         s               d
003600990915     D wrkora          s               t
003700000613     D rrnum           s              6  0 INZ(*zeros)
003800030616     D wNomeFile       s             30    INZ(*blanks)
003900030710     D wCMR            s             35    INZ(*blanks)
004000030710     D wPosDaA         s              2    INZ(*blanks)
004100030710     D wPosDa          s              2  0 INZ(*zeros)
004200030710     D wLungA          s              2    INZ(*blanks)
004300030710     D wLung           s              2  0 INZ(*zeros)
004400050914     D wDataOraAlfa    s             14    INZ(*blanks)
004500010606     D*------------------
004600010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700010606     D*------------------
004800010606     D WLBDA8          DS                  INZ
004900010606     D  G08DAT                 1      8  0
005000010606     D  G08INV                 9     16  0
005100010606     D  G08ERR                17     17
005200010606     D  G08TGI                18     22  0
005300050118     D*---------------------------------------------------------
005400050118     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
005500050118     D*---------------------------------------------------------
005600050118     D                 ds
005700101209     D vhbatb                         1
005800101209     D vhbfgs                         3
005900101209     D vhbccm                         7
006000101209     D vhblnp                         3
006100101209     D vhbaas                         4
006200101209     D vhbmgs                         4
006300101209     D vhbnrs                         2
006400101209     D vhbnsp                         7
006500101209     D vhbcbo                         2
006600101209     D vhblna                         3
006700101209     D vhbrsd                        35
006800101209     D vhbrd2                        35
006900101209     D vhbind                        35
007000101209     D vhbcad                         9
007100101209     D vhblod                        35
007200101209     D vhbprd                         2
007300101209     D vhbnzd                         3
007400101209     D vhbgc1                         2
007500101209     D vhbgc2                         2
007600101209     D vhbctr                         3
007700101209     D vhbtsp                         1
007800101209     D vhbias                        13
007900101209     D vhbvas                         3
008000101209     D vhbnas                        15
008100101209     D vhbncl                         5
008200101209     D vhbpkb                         7
008300101209     D vhbvlb                         5
008400101209     D vhbqft                        13
008500101209     D vhbcas                        13
008600101209     D vhbtic                         2
008700101209     D vhbvca                         3
008800101209     D vhbgca                         2
008900101209     D vhbrmn                        15
009000101209     D vhbrma                        15
009100101209     D vhbncd                         7
009200101209     D vhbnca                         7
009300101209     D vhbxco                         1
009400101209     D vhbnot                        35
009500101209     D vhbnt2                        35
009600101209     D vhbznc                         2
009700101209     D vhbctm                         2
009800101209     D vhbffd                         1
009900101209     D vhbdcr                         8
010000101209     D vhbtcr                         1
010100101209     D vhbhcr                         4
010200101209     D vhbcts                         2
010300101209     D vhbftm                         1
010400101209     D vhbvmd                        13
010500101209     D vhbvad                         3
010600101209     D vhbgma                         2
010700101209     D vhbgga                         2
010800101209     D vhbgva                         2
010900101209     D vhbtc1                         1
011000101209     D vhbtc2                         1
011100101209     D vhbscl                         1
011200101209     D vhbant                         9
011300101209     D vhbrmo                        25
011400101209     D vhbcmo                         9
011500101209     D vhbnmo                         3
011600101209     D  X_rec                  1    486    inz
011700010606     D*
011800071018     D*-------------------
011900071018     D* COSTANTI
012000071018     D*-------------------
012100071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
012200071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
012300990908
012400000913     C                   reset                   rrnum
012500990921     C                   reset                   esito
012600990921     C                   reset                   wrkesito
012700010530     C*
012800010530     C                   exsr      opeini
012900010530     C                   exsr      rwvab
013000041025     C*
013100041025     C* Esegue lancio TISI95R solo x chiusura
013200041025     C                   CLEAR                   TISI95DS
013300041025     C                   EVAL      I95TLA = 'C'
013400041025     C                   CALL      'TISI95R'
013500041025     C                   PARM                    TISI95DS
013600010530     C*
013700010530     C                   seton                                        lr
013800010530
013900010530
014000010530
014100010530
014200010530     C*--------------------------------------------------------
014300010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
014400010530     C*--------------------------------------------------------
014500010530     C     PREELA        BEGSR
014600000724     C*
014700000724     C* SE OCCORRE SPEDIRE IN FILIALE
014800010530     C                   if        invfil <> *zeros and
014900010530     C                             flgGiro = '0'
015000010530     C*
015100010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
015200010530     C                   eval      flgGiro = '1'
015300010530     C*
015400010530     C                   endif
015500010530     C*
015600010530     C                   ENDSR
015700010530     C***
015800010530
015900010530
016000010530
016100010530
016200010530
016300010530
016400010530     C*--------------------------------------------------------
016500010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016600010530     C*--------------------------------------------------------
016700010530     C     ENDELA        BEGSR
016800010530     C*
016900010530     C*
017000010530     C                   ENDSR
017100010530     C***
017200990908
017300010530
017400010530
017500010530
017600910830     C*--------------------------------------------------------
017700030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
017800910830     C*--------------------------------------------------------
017900941229     C     RWVAB         BEGSR
018000030710     C*
018100030710     C                   clear                   §CTROK
018200030710     C                   clear                   §CTRMO
018300030710     C                   clear                   §CTRNO
018400990910     C*
018500030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
018600030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
018700030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
018800030710     C                   if        *in60 = *on
018900990914     C                   if        not %open(tivin00r)
019000990908     C                   open      tivin00r
019100990914     C                   endif
019200030616     C                   if        not %open(edivabwr)
019300030616     C                   open      edivabwr
019400990914     C                   endif
019500990910     C*
019600921023     C                   DO        *HIVAL
019700990913     C*
019800990915     C                   READ      tivin00r                               70
019900071018     C*
020000071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
020100071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
020200010618     C*
020300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020400010618     C                   if        vinflg = '1'
020500010618     C                   eval      flgOk = '1'
020600010618     C                   endif
020700010705     C*
020800010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
020900010705     C                   if        %subst(vindta:1:1) = '='
021000010705     C                   eval      %subst(vindta:1:1) = *blanks
021100010705     C                   endif
021200010705     C*
021300000905     C                   if        vindta > *blanks
021400000613     C                   add       1             rrnum
021500010601     C* ...elimino eventuali segni
021600010601     C     '+':' '       xlate     vindta        vindta
021700990913     C*
021800010601     C                   if        *in70 = *off and
021900990913     C                             (vinflg = *blanks
022000010711     C                              or vinflg = '0'
022100010711     C                              or vinflg = '2')
022200990913     C*
022300030616     C                   clear                   EDIVAB00
022400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022500010711     C                   if        vinflg = *blanks or vinflg = '0'
022600010711     C                   clear                   vinmsg
022700010711     C                   endif
022800010711     C*
022900050118     C                   eval      x_rec  = vindta
023000991022     C*
023100991022     C                   testb     x'3F'         vhbatb                   02
023200991022     C                   if        not *in02
023300991022     C*
023400971021     C                   SETOFF                                       303132
023500971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
023600971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
023700971023     C*    OPPURE TUTTI BLANKS
023800101209     C*
023900101209     C                   TESTN                   VHBFGS               30
024000101209     C  N30              TESTN                   VHBFGS                 30
024100101209     C  N30              TESTN                   VHBFGS                   30
024200101209     C                   MOVE      VHBFGS        W001A             1
024300101209     C     *IN30         IFEQ      *OFF
024400101209     C     W001A         ORLT      '0'
024500101209     C                   SETON                                        31
024600101209     C                   eval      vinmsg = %trimr(vinmsg)
024700101209     C                             + ' ' + 'VHBFGS'
024800101209     C                   ELSE
024900101209     C                   MOVE      VHBFGS        VABFGS
025000101209     C     VABFGS        IFLE      *ZEROS
025100101209     C                   SETON                                        31
025200101209     C                   eval      vinmsg = %trimr(vinmsg)
025300101209     C                             + ' ' + 'VHBFGS'
025400101209     C                   ENDIF
025500101209     C                   ENDIF
025600010530     C*
025700010530     C                   TESTN                   VHBLNP               30
025800010530     C  N30              TESTN                   VHBLNP                 30
025900010530     C  N30              TESTN                   VHBLNP                   30
026000010530     C                   MOVE      VHBLNP        W001A             1
026100010530     C     *IN30         IFEQ      *OFF
026200010530     C     W001A         ORLT      '0'
026300010530     C                   SETON                                        31
026400010530     C                   eval      vinmsg = %trimr(vinmsg)
026500010530     C                             + ' ' + 'VHBLNP'
026600010530     C                   ELSE
026700010530     C                   MOVE      VHBLNP        VABLNP
026800010530     C     VABLNP        IFLE      *ZEROS
026900010530     C                   SETON                                        31
027000010530     C                   eval      vinmsg = %trimr(vinmsg)
027100010530     C                             + ' ' + 'VHBLNP'
027200010530     C                   ENDIF
027300010530     C                   ENDIF
027400010530     C*
027500010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027600101209     C                   eval      depfil = VABFGS
027700010530     C                   exsr      repfil
027800010601     C                   if        depfil = invfil
027900021014     C                   if        vlrpoi = 999
028000101209     C                   MOVE(P)   invfil        VABFGS
028100021014     C                   else
028200021014     C                   MOVE(P)   vlrpoi        VABFGS
028300021014     C                   endif
028400010530     C*
028500010530     C                   exsr      PREELA
028600010530     C*
028700971023     C                   TESTN                   VHBCCM               30
028800971023     C  N30              TESTN                   VHBCCM                 30
028900971023     C  N30              TESTN                   VHBCCM                   30
029000971021     C                   MOVE      VHBCCM        W001A             1
029100971021     C     *IN30         IFEQ      *OFF
029200971021     C     W001A         ORLT      '0'
029300971021     C                   SETON                                        31
029400010604     C                   eval      vinmsg = %trimr(vinmsg)
029500010604     C                             + ' ' + 'VHBCCM'
029600971021     C                   ELSE
029700971021     C                   MOVE      VHBCCM        VABCCM
029800971024     C     VABCCM        IFLE      *ZEROS
029900971024     C                   SETON                                        31
030000010604     C                   eval      vinmsg = %trimr(vinmsg)
030100010604     C                             + ' ' + 'VHBCCM'
030200971024     C                   ENDIF
030300971021     C                   ENDIF
030400971021     C*
030500971023     C                   TESTN                   VHBAAS               30
030600971023     C  N30              TESTN                   VHBAAS                 30
030700971023     C  N30              TESTN                   VHBAAS                   30
030800971021     C                   MOVE      VHBAAS        W001A             1
030900971021     C     *IN30         IFEQ      *OFF
031000971021     C     W001A         ORLT      '0'
031100971023     C     W001A         ANDNE     *BLANK
031200971021     C                   SETON                                        32
031300010606     C                   MOVEL     datcor        VABAAS
031400010604     C                   eval      vinmsg = %trimr(vinmsg)
031500010604     C                             + ' ' + 'VHBAAS'
031600971021     C                   ELSE
031700971021     C                   MOVE      VHBAAS        VABAAS
031800971021     C                   ENDIF
031900971021     C*
032000971023     C                   TESTN                   VHBMGS               30
032100971023     C  N30              TESTN                   VHBMGS                 30
032200971023     C  N30              TESTN                   VHBMGS                   30
032300971021     C                   MOVE      VHBMGS        W001A             1
032400971021     C     *IN30         IFEQ      *OFF
032500971021     C     W001A         ORLT      '0'
032600971023     C     W001A         ANDNE     *BLANK
032700971021     C                   SETON                                        32
032800010601     C                   MOVE      '0101'        VABMGS
032900010604     C                   eval      vinmsg = %trimr(vinmsg)
033000010604     C                             + ' ' + 'VHBMGS'
033100971021     C                   ELSE
033200971021     C                   MOVE      VHBMGS        VABMGS
033300971021     C                   ENDIF
033400971021     C*
033500971023     C                   TESTN                   VHBNRS               30
033600971023     C  N30              TESTN                   VHBNRS                 30
033700971023     C  N30              TESTN                   VHBNRS                   30
033800971021     C                   MOVE      VHBNRS        W001A             1
033900971021     C     *IN30         IFEQ      *OFF
034000971021     C     W001A         ORLT      '0'
034100971023     C     W001A         ANDNE     *BLANK
034200971021     C                   SETON                                        32
034300971021     C                   MOVE      *ZERO         VABNRS
034400010604     C                   eval      vinmsg = %trimr(vinmsg)
034500010604     C                             + ' ' + 'VHBNRS'
034600971021     C                   ELSE
034700971021     C                   MOVE      VHBNRS        VABNRS
034800971021     C                   ENDIF
034900971021     C*
035000971023     C                   TESTN                   VHBNSP               30
035100971023     C  N30              TESTN                   VHBNSP                 30
035200971023     C  N30              TESTN                   VHBNSP                   30
035300971021     C                   MOVE      VHBNSP        W001A             1
035400971021     C     *IN30         IFEQ      *OFF
035500971021     C     W001A         ORLT      '0'
035600971023     C     W001A         ANDNE     *BLANK
035700971021     C                   SETON                                        32
035800971021     C                   MOVE      *ZERO         VABNSP
035900010604     C                   eval      vinmsg = %trimr(vinmsg)
036000010604     C                             + ' ' + 'VHBNSP'
036100971021     C                   ELSE
036200971021     C                   MOVE      VHBNSP        VABNSP
036300971021     C                   ENDIF
036400971021     C*
036500971023     C                   TESTN                   VHBLNA               30
036600971023     C  N30              TESTN                   VHBLNA                 30
036700971023     C  N30              TESTN                   VHBLNA                   30
036800971021     C                   MOVE      VHBLNA        W001A             1
036900971021     C     *IN30         IFEQ      *OFF
037000971021     C     W001A         ORLT      '0'
037100971023     C     W001A         ANDNE     *BLANK
037200971021     C                   SETON                                        32
037300971021     C                   MOVE      *ZERO         VABLNA
037400010604     C                   eval      vinmsg = %trimr(vinmsg)
037500010604     C                             + ' ' + 'VHBLNA'
037600971021     C                   ELSE
037700971021     C                   MOVE      VHBLNA        VABLNA
037800971021     C                   ENDIF
037900971021     C*
038000971023     C                   TESTN                   VHBCTR               30
038100971023     C  N30              TESTN                   VHBCTR                 30
038200971023     C  N30              TESTN                   VHBCTR                   30
038300971021     C                   MOVE      VHBCTR        W001A             1
038400971021     C     *IN30         IFEQ      *OFF
038500971021     C     W001A         ORLT      '0'
038600971023     C     W001A         ANDNE     *BLANK
038700971021     C                   SETON                                        32
038800971021     C                   MOVE      *ZERO         VABCTR
038900010604     C                   eval      vinmsg = %trimr(vinmsg)
039000010604     C                             + ' ' + 'VHBCTR'
039100971021     C                   ELSE
039200971021     C                   MOVE      VHBCTR        VABCTR
039300971021     C                   ENDIF
039400941229     C*
039500971021     C                   MOVE      *ZERO         VABIAS
039600941229     C                   MOVE      VHBIAS        WASC15
039700971023     C                   TESTN                   WI15                 30
039800971023     C  N30              TESTN                   WI15                   30
039900971023     C  N30              TESTN                   WI15                     30
040000971021     C                   MOVE      WI15          W001A             1
040100971021     C     *IN30         IFEQ      *OFF
040200971021     C     W001A         ORLT      '0'
040300971023     C     W001A         ANDNE     *BLANK
040400971021     C                   SETON                                        32
040500971021     C                   MOVE      *ZERO         VABIAS
040600010604     C                   eval      vinmsg = %trimr(vinmsg)
040700010604     C                             + ' ' + 'VHBIAS'
040800971021     C                   ELSE
040900971021     C                   MOVEL     WI15          VABIAS
041000971023     C                   TESTN                   WD15                 30
041100971023     C  N30              TESTN                   WD15                   30
041200971023     C  N30              TESTN                   WD15                     30
041300971021     C                   MOVE      WD15          W001A             1
041400971021     C     *IN30         IFEQ      *OFF
041500971021     C     W001A         ORLT      '0'
041600971023     C     W001A         ANDNE     *BLANK
041700971021     C                   SETON                                        32
041800971021     C                   MOVE      *ZERO         VABIAS
041900010604     C                   eval      vinmsg = %trimr(vinmsg)
042000010604     C                             + ' ' + 'VHBIAS'
042100971021     C                   ELSE
042200971021     C                   MOVE      WD15          VABIAS
042300971021     C                   ENDIF
042400050118     C                   ENDIF
042500941229     C*
042600971023     C                   TESTN                   VHBNCL               30
042700971023     C  N30              TESTN                   VHBNCL                 30
042800971023     C  N30              TESTN                   VHBNCL                   30
042900971021     C                   MOVE      VHBNCL        W001A             1
043000971021     C     *IN30         IFEQ      *OFF
043100971021     C     W001A         ORLT      '0'
043200971021     C                   SETON                                        32
043300971021     C                   MOVE      *ZERO         VABNCL
043400010604     C                   eval      vinmsg = %trimr(vinmsg)
043500010604     C                             + ' ' + 'VHBNCL'
043600971021     C                   ELSE
043700971021     C                   MOVE      VHBNCL        VABNCL
043800971021     C                   ENDIF
043900941229     C*
044000971021     C                   MOVE      *ZERO         VABPKB
044100971021     C                   MOVE      VHBPKB        WASC09
044200010719     C                   IF        WI09 = *blanks
044300010719     C                   EVAL      WI09 = *zeros
044400010719     C                   ENDIF
044500971023     C                   TESTN                   WI09                 30
044600971023     C  N30              TESTN                   WI09                   30
044700971023     C  N30              TESTN                   WI09                     30
044800971021     C                   MOVE      WI09          W001A             1
044900971021     C     *IN30         IFEQ      *OFF
045000971021     C     W001A         ORLT      '0'
045100971021     C                   SETON                                        32
045200971021     C                   MOVE      *ZERO         VABPKB
045300010604     C                   eval      vinmsg = %trimr(vinmsg)
045400010604     C                             + ' ' + 'VHBPKB'
045500971021     C                   ELSE
045600971021     C                   MOVEL     WI09          VABPKB
045700010719     C                   IF        WD09 = *blanks
045800010719     C                   EVAL      WD09 = *zeros
045900010719     C                   ENDIF
046000971023     C                   TESTN                   WD09                 30
046100971023     C  N30              TESTN                   WD09                     30
046200971021     C                   MOVE      WD09          W001A             1
046300971021     C     *IN30         IFEQ      *OFF
046400971021     C     W001A         ORLT      '0'
046500971023     C     W001A         ANDNE     *BLANK
046600971021     C                   SETON                                        32
046700971021     C                   MOVE      *ZERO         VABPKB
046800010604     C                   eval      vinmsg = %trimr(vinmsg)
046900010604     C                             + ' ' + 'VHBPKB'
047000971021     C                   ELSE
047100971021     C                   MOVE      WD09          VABPKB
047200971021     C                   ENDIF
047300050118     C                   ENDIF
047400971021     C*
047500041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
047600041001     C                   IF        *IN51 = *OFF
047700971021     C                   MOVE      *ZERO         VABVLB
047800971021     C                   MOVE      VHBVLB        WASC07
047900010719     C                   IF        WI07 = *blanks
048000010719     C                   EVAL      WI07 = *zeros
048100010719     C                   ENDIF
048200971023     C                   TESTN                   WI07                 30
048300971023     C  N30              TESTN                   WI07                   30
048400971023     C  N30              TESTN                   WI07                     30
048500971021     C                   MOVE      WI07          W001A             1
048600971021     C     *IN30         IFEQ      *OFF
048700971021     C     W001A         ORLT      '0'
048800971023     C     W001A         ANDNE     *BLANK
048900971021     C                   SETON                                        32
049000971021     C                   MOVE      *ZERO         VABVLB
049100010604     C                   eval      vinmsg = %trimr(vinmsg)
049200010604     C                             + ' ' + 'VHBVLB'
049300971021     C                   ELSE
049400971021     C                   MOVEL     WI07          VABVLB
049500010719     C                   IF        WD07 = *blanks
049600010719     C                   EVAL      WD07 = *zeros
049700010719     C                   ENDIF
049800971023     C                   TESTN                   WD07                 30
049900971023     C  N30              TESTN                   WD07                   30
050000971023     C  N30              TESTN                   WD07                     30
050100971021     C                   MOVE      WD07          W001A             1
050200971021     C     *IN30         IFEQ      *OFF
050300971021     C     W001A         ORLT      '0'
050400971023     C     W001A         ANDNE     *BLANK
050500971021     C                   SETON                                        32
050600971021     C                   MOVE      *ZERO         VABVLB
050700010604     C                   eval      vinmsg = %trimr(vinmsg)
050800010604     C                             + ' ' + 'VHBVLB'
050900971021     C                   ELSE
051000971021     C                   MOVE      WD07          VABVLB
051100971021     C                   ENDIF
051200050118     C                   ENDIF
051300041001     C                   ELSE
051400041001     C* ...altrimenti forzo azzeramento del volume
051500041001     C                   Z-ADD     *zeros        VABVLB
051600041001     C                   ENDIF
051700941229     C*
051800971021     C                   MOVE      *ZERO         VABQFT
051900971021     C                   MOVE      VHBQFT        WASC15
052000010719     C                   IF        WI15 = *blanks
052100010719     C                   EVAL      WI15 = *zeros
052200010719     C                   ENDIF
052300971023     C                   TESTN                   WI15                 30
052400971023     C  N30              TESTN                   WI15                   30
052500971023     C  N30              TESTN                   WI15                     30
052600971021     C                   MOVE      WI15          W001A             1
052700971021     C     *IN30         IFEQ      *OFF
052800971021     C     W001A         ORLT      '0'
052900971023     C     W001A         ANDNE     *BLANK
053000971021     C                   SETON                                        32
053100971021     C                   MOVE      *ZERO         VABQFT
053200010604     C                   eval      vinmsg = %trimr(vinmsg)
053300010604     C                             + ' ' + 'VHBQFT'
053400971021     C                   ELSE
053500971021     C                   MOVEL     WI15          VABQFT
053600010719     C                   IF        WD15 = *blanks
053700010719     C                   EVAL      WD15 = *zeros
053800010719     C                   ENDIF
053900971023     C                   TESTN                   WD15                 30
054000971023     C  N30              TESTN                   WD15                   30
054100971023     C  N30              TESTN                   WD15                     30
054200971021     C                   MOVE      WD15          W001A             1
054300971021     C     *IN30         IFEQ      *OFF
054400971021     C     W001A         ORLT      '0'
054500971023     C     W001A         ANDNE     *BLANK
054600971021     C                   SETON                                        32
054700971021     C                   MOVE      *ZERO         VABQFT
054800010604     C                   eval      vinmsg = %trimr(vinmsg)
054900010604     C                             + ' ' + 'VHBQFT'
055000971021     C                   ELSE
055100971021     C                   MOVE      WD15          VABQFT
055200971021     C                   ENDIF
055300050118     C                   ENDIF
055400941229     C*
055500971021     C                   MOVE      *ZERO         VABCAS
055600971021     C                   MOVE      VHBCAS        WASC15
055700010719     C                   IF        WI15 = *blanks
055800010719     C                   EVAL      WI15 = *zeros
055900010719     C                   ENDIF
056000971023     C                   TESTN                   WI15                 30
056100971023     C  N30              TESTN                   WI15                   30
056200971023     C  N30              TESTN                   WI15                     30
056300971021     C                   MOVE      WI15          W001A             1
056400971021     C     *IN30         IFEQ      *OFF
056500971021     C     W001A         ORLT      '0'
056600971023     C     W001A         ANDNE     *BLANK
056700971021     C                   SETON                                        32
056800971021     C                   MOVE      *ZERO         VABCAS
056900010604     C                   eval      vinmsg = %trimr(vinmsg)
057000010604     C                             + ' ' + 'VHBCAS'
057100971021     C                   ELSE
057200971021     C                   MOVEL     WI15          VABCAS
057300010719     C                   IF        WD15 = *blanks
057400010719     C                   EVAL      WD15 = *zeros
057500010719     C                   ENDIF
057600971023     C                   TESTN                   WD15                 30
057700971023     C  N30              TESTN                   WD15                   30
057800971023     C  N30              TESTN                   WD15                     30
057900971021     C                   MOVE      WD15          W001A             1
058000971021     C     *IN30         IFEQ      *OFF
058100971021     C     W001A         ORLT      '0'
058200971023     C     W001A         ANDNE     *BLANK
058300971021     C                   SETON                                        32
058400971021     C                   MOVE      *ZERO         VABCAS
058500010604     C                   eval      vinmsg = %trimr(vinmsg)
058600010604     C                             + ' ' + 'VHBCAS'
058700971021     C                   ELSE
058800971021     C                   MOVE      WD15          VABCAS
058900971021     C                   ENDIF
059000050118     C                   ENDIF
059100971021     C*
059200971023     C                   TESTN                   VHBRMN               30
059300971023     C  N30              TESTN                   VHBRMN                 30
059400971023     C  N30              TESTN                   VHBRMN                   30
059500971021     C                   MOVE      VHBRMN        W001A             1
059600971021     C     *IN30         IFEQ      *OFF
059700971021     C     W001A         ORLT      '0'
059800971021     C                   SETON                                        32
059900971021     C                   MOVE      *ZERO         VABRMN
060000010604     C                   eval      vinmsg = %trimr(vinmsg)
060100010604     C                             + ' ' + 'VHBRMN'
060200971021     C                   ELSE
060300971021     C                   MOVE      VHBRMN        VABRMN
060400971021     C                   ENDIF
060500971021     C*
060600971023     C                   TESTN                   VHBNCD               30
060700971023     C  N30              TESTN                   VHBNCD                 30
060800971023     C  N30              TESTN                   VHBNCD                   30
060900971021     C                   MOVE      VHBNCD        W001A             1
061000971021     C     *IN30         IFEQ      *OFF
061100971021     C     W001A         ORLT      '0'
061200971023     C     W001A         ANDNE     *BLANK
061300971021     C                   SETON                                        32
061400971021     C                   MOVE      *ZERO         VABNCD
061500010604     C                   eval      vinmsg = %trimr(vinmsg)
061600010604     C                             + ' ' + 'VHBNCD'
061700971021     C                   ELSE
061800971021     C                   MOVE      VHBNCD        VABNCD
061900971021     C                   ENDIF
062000971021     C*
062100971023     C                   TESTN                   VHBNCA               30
062200971023     C  N30              TESTN                   VHBNCA                 30
062300971023     C  N30              TESTN                   VHBNCA                   30
062400971021     C                   MOVE      VHBNCA        W001A             1
062500971021     C     *IN30         IFEQ      *OFF
062600971021     C     W001A         ORLT      '0'
062700971023     C     W001A         ANDNE     *BLANK
062800971021     C                   SETON                                        32
062900971021     C                   MOVE      *ZERO         VABNCA
063000010604     C                   eval      vinmsg = %trimr(vinmsg)
063100010604     C                             + ' ' + 'VHBNCA'
063200971021     C                   ELSE
063300971021     C                   MOVE      VHBNCA        VABNCA
063400971021     C                   ENDIF
063500971021     C*
063600971023     C                   TESTN                   VHBZNC               30
063700971023     C  N30              TESTN                   VHBZNC                 30
063800971023     C  N30              TESTN                   VHBZNC                   30
063900971021     C                   MOVE      VHBZNC        W001A             1
064000971021     C     *IN30         IFEQ      *OFF
064100971021     C     W001A         ORLT      '0'
064200971023     C     W001A         ANDNE     *BLANK
064300971021     C                   SETON                                        32
064400971021     C                   MOVE      *ZERO         VABZNC
064500010604     C                   eval      vinmsg = %trimr(vinmsg)
064600010604     C                             + ' ' + 'VHBZNC'
064700971021     C                   ELSE
064800971021     C                   MOVE      VHBZNC        VABZNC
064900971021     C                   ENDIF
065000971021     C*
065100971023     C                   TESTN                   VHBDCR               30
065200971023     C  N30              TESTN                   VHBDCR                 30
065300971023     C  N30              TESTN                   VHBDCR                   30
065400971021     C                   MOVE      VHBDCR        W001A             1
065500971021     C     *IN30         IFEQ      *OFF
065600971021     C     W001A         ORLT      '0'
065700971023     C     W001A         ANDNE     *BLANK
065800971021     C                   SETON                                        32
065900971021     C                   MOVE      *ZERO         VABDCR
066000010604     C                   eval      vinmsg = %trimr(vinmsg)
066100010604     C                             + ' ' + 'VHBDCR'
066200971021     C                   ELSE
066300971021     C                   MOVE      VHBDCR        VABDCR
066400971021     C                   ENDIF
066500971021     C*
066600971023     C                   TESTN                   VHBHCR               30
066700971023     C  N30              TESTN                   VHBHCR                 30
066800971023     C  N30              TESTN                   VHBHCR                   30
066900971021     C                   MOVE      VHBHCR        W001A             1
067000971021     C     *IN30         IFEQ      *OFF
067100971021     C     W001A         ORLT      '0'
067200971023     C     W001A         ANDNE     *BLANK
067300971021     C                   SETON                                        32
067400971021     C                   MOVE      *ZERO         VABHCR
067500010604     C                   eval      vinmsg = %trimr(vinmsg)
067600010604     C                             + ' ' + 'VHBHCR'
067700971021     C                   ELSE
067800971021     C                   MOVE      VHBHCR        VABHCR
067900971021     C                   ENDIF
068000941229     C*
068100971021     C                   MOVE      *ZERO         VABVMD
068200971021     C                   MOVE      VHBVMD        WASC15
068300010719     C                   IF        WI15 = *blanks
068400010719     C                   EVAL      WI15 = *zeros
068500010719     C                   ENDIF
068600971023     C                   TESTN                   WI15                 30
068700971023     C  N30              TESTN                   WI15                   30
068800971023     C  N30              TESTN                   WI15                     30
068900971021     C                   MOVE      WI15          W001A             1
069000971021     C     *IN30         IFEQ      *OFF
069100971021     C     W001A         ORLT      '0'
069200971023     C     W001A         ANDNE     *BLANK
069300971021     C                   SETON                                        32
069400971021     C                   MOVE      *ZERO         VABVMD
069500010604     C                   eval      vinmsg = %trimr(vinmsg)
069600010604     C                             + ' ' + 'VHBVMD'
069700971021     C                   ELSE
069800971021     C                   MOVEL     WI15          VABVMD
069900010719     C                   IF        WD15 = *blanks
070000010719     C                   EVAL      WD15 = *zeros
070100010719     C                   ENDIF
070200971023     C                   TESTN                   WD15                 30
070300971023     C  N30              TESTN                   WD15                   30
070400971023     C  N30              TESTN                   WD15                     30
070500971021     C                   MOVE      WD15          W001A             1
070600971021     C     *IN30         IFEQ      *OFF
070700971021     C     W001A         ORLT      '0'
070800971023     C     W001A         ANDNE     *BLANK
070900971021     C                   SETON                                        32
071000971021     C                   MOVE      *ZERO         VABVMD
071100010604     C                   eval      vinmsg = %trimr(vinmsg)
071200010604     C                             + ' ' + 'VHBVMD'
071300971021     C                   ELSE
071400971021     C                   MOVE      WD15          VABVMD
071500971021     C                   ENDIF
071600050118     C                   ENDIF
071700971021     C*
071800971023     C                   TESTN                   VHBANT               30
071900971023     C  N30              TESTN                   VHBANT                 30
072000971023     C  N30              TESTN                   VHBANT                   30
072100971021     C                   MOVE      VHBANT        W001A             1
072200971021     C     *IN30         IFEQ      *OFF
072300971021     C     W001A         ORLT      '0'
072400971023     C     W001A         ANDNE     *BLANK
072500971021     C                   SETON                                        32
072600971021     C                   MOVE      *ZERO         VABANT
072700010604     C                   eval      vinmsg = %trimr(vinmsg)
072800010604     C                             + ' ' + 'VHBANT'
072900971021     C                   ELSE
073000971021     C                   MOVE      VHBANT        VABANT
073100971021     C                   ENDIF
073200971021     C*
073300971021     C* CAMPI ALFABETICI:
073400971021     C                   MOVE      VHBATB        VABATB
073500971021     C                   MOVE      VHBCBO        VABCBO
073600971021     C                   MOVE      VHBRSD        VABRSD
073700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
073800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
073900020117     C* ==
074000971021     C                   MOVE      VHBRD2        VABRD2
074100971021     C                   MOVE      VHBIND        VABIND
074200971021     C                   MOVE      VHBCAD        VABCAD
074300971021     C                   MOVE      VHBLOD        VABLOD
074400971021     C                   MOVE      VHBPRD        VABPRD
074500971021     C                   MOVE      VHBNZD        VABNZD
074600971021     C                   MOVE      VHBGC1        VABGC1
074700971021     C                   MOVE      VHBGC2        VABGC2
074800971021     C                   MOVE      VHBTSP        VABTSP
074900971021     C                   MOVE      VHBVAS        VABVAS
075000971021     C                   MOVE      VHBNAS        VABNAS
075100971021     C                   MOVE      VHBTIC        VABTIC
075200971021     C                   MOVE      VHBVCA        VABVCA
075300971021     C                   MOVE      VHBGCA        VABGCA
075400971021     C                   MOVE      VHBRMA        VABRMA
075500971021     C                   MOVE      VHBXCO        VABXCO
075600971021     C                   MOVE      VHBNOT        VABNOT
075700971021     C                   MOVE      VHBNT2        VABNT2
075800971021     C                   MOVE      VHBCTM        VABCTM
075900971021     C                   MOVE      VHBFFD        VABFFD
076000971021     C                   MOVE      VHBTCR        VABTCR
076100971021     C                   MOVE      VHBCTS        VABCTS
076200971021     C                   MOVE      VHBFTM        VABFTM
076300971021     C                   MOVE      VHBVAD        VABVAD
076400971021     C                   MOVE      VHBGMA        VABGMA
076500971021     C                   MOVE      VHBGGA        VABGGA
076600971021     C                   MOVE      VHBGVA        VABGVA
076700971021     C                   MOVE      VHBTC1        VABTC1
076800971021     C                   MOVE      VHBTC2        VABTC2
076900971021     C                   MOVE      VHBSCL        VABSCL
077000971021     C                   MOVE      VHBRMO        VABRMO
077100971021     C                   MOVE      VHBCMO        VABCMO
077200971021     C                   MOVE      VHBNMO        VABNMO
077300971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
077400971021     C* NELLA NAZIONE ORIGINALE
077500971021     C                   MOVE      VHBNMO        WA1D              1
077600971021     C                   MOVEL     VHBNMO        WA1S              1
077700971021     C     WA1D          IFNE      *BLANK
077800971021     C     WA1S          ANDEQ     *BLANK
077900971021     C                   MOVE      *BLANK        VABNMO
078000030616     C                   ENDIF
078100030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
078200030710     C                   EVAL      VABCMR = wCMR
078300030616     C                   EVAL      VABDCM = DATCOR
078400030616     C                   EVAL      VABDTS = DATCOR
078500040402     C                   EVAL      VABHMS = ORACOR
078600030616     C                   EVAL      VABCNT = 1
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
081500110705     C*
081600110705     C* Verifica ID progressivo (VABRMN) tutto OK
081700110705     C                   IF        VABRMN = *zeros
081800110705     C                   SETON                                        31
081900110705     C                   Z-ADD     151           acokcc
082000110705     C                   Z-ADD     VABCCM        acoksc
082100110705     C     KEYaco00_C    CHAIN     cnaco00f                           99
082200110705     C                   EVAL      VABDSP = VABAAS*10000+VABMGS
082300110705     C                   EVAL      BORDERO = wCMR
082400110705     C*
082500110705     C                   CALL      'TIS7EMLR6'
082600110705     C                   PARM                    VABRSD
082700110705     C                   PARM                    VABLOD
082800110705     C                   PARM                    VABDSP            8 0
082900110705     C                   PARM                    VABCCM
083000110705     C                   PARM                    ACORAG
083100110705     C                   PARM                    BORDERO          35
083200110705     C                   ENDIF
083300930409     C*
083400010604     C  N31              ADD       1             §CTROK            7 0
083500010604     C   32              ADD       1             §CTRMO            7 0
083600010604     C   31              ADD       1             §CTRNO            7 0
083700030616     C  N31              WRITE     EDIVAB00
083800990910     C*
083900010601     C                   if        *in31 = *off and
084000010601     C                             *in32 = *off
084100990910     C                   eval      vinflg = '1'
084200990910     C                   else
084300990910     C                   eval      vinflg = '2'
084400990910     C                   endif
084500991022     C                   endif
084600990910     C                   endif
084700010601     C*
084800010601     C                   endif
084900000905     C*
085000000905     C                   else
085100000905     C                   eval      vinflg = '1'
085200000905     C                   endif
085300000905     C*
085400000905     C  N70              update    tivin000
085500991022     C*
085600991022     C  N70              ENDdo
085700010530     C
085800010530     C*
085900010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
086000010530     C                   if        cntNonEl = *zeros or
086100010530     C                             flgMulti = '0'
086200990910     C* Se non ci sono record con errori ...
086300010530     C                   if        §ctrno = 0 and
086400010601     C                             §ctrmo = 0 and
086500010530     C                             flgStato <> '2'
086600990910     C* ... restituisco esito OK.
086700990921     C                   eval      wrkesito = '0'
086800990910     C                   else
086900010601     C                   if        §ctrok > 0
087000990921     C                   eval      wrkesito = '1'
087100000710     C                   else
087200010615     C                   if        flgOk = '0'
087300010615     C                   eval      wrkesito = '2'
087400010615     C                   else
087500010615     C                   eval      wrkesito = '6'
087600010615     C                   endif
087700990910     C                   endif
087800000710     C                   endif
087900010530     C                   else
088000010530     C                   eval      wrkesito = '9'
088100010530     C                   endif
088200030710     C*
088300030710 X  1C                   else
088400030710     C                   eval      wrkesito = '8'
088500030710     C                   eval      flgGiro = '1'
088600030710 E  1C                   endif
088700030710     C*
088800990914     C                   if        %open(tivin00r)
088900990908     C                   close     tivin00r
089000990914     C                   endif
089100030616     C                   if        %open(edivabwr)
089200030616     C                   close     edivabwr
089300990914     C                   endif
089400010530     C*
089500010530     C                   if        vlrpoi <> 999
089600010530     C                   eval      invfil = vlrpoi
089700010530     C                   endif
089800990910     C*
089900990920     C                   if        §ctrok > 0
090000010530     C                             and invfil > *zeros
090100000613     C                   exsr      invio
090200990920     C                   endif
090300010612     C*
090400010612     C                   if        flgGiro = '1'
090500010612     C                   exsr      endela
090600010612     C                   endif
090700990920     C*
090800910830     C                   ENDSR
090900000613     C***
091000990920
091100010530
091200010530
091300010530
091400010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
091500010530     C     repfil        BEGSR
091600010530     C*
091700010530     C                   if        invfil = *zeros and
091800010530     C                             depfil > *zeros and
091900010613     C                             (vinflg = *blanks or
092000010613     C                              vinflg = *zeros)
092100010530     C
092200010530     C                   eval      invfil = depfil
092300010530     C                   endif
092400010530     C*
092500010530     C                   if        depfil <> invfil and
092600010530     C                             invfil > *zeros
092700010530     C                   eval      flgMulti = '1'
092800010530     C                   if        vinflg = *blanks
092900010530     C                   add       1             cntNonEl
093000010530     C                   endif
093100010530     C                   endif
093200010530     C*
093300010530     C                   if        vinflg = '2'
093400010530     C                   eval      flgStato = '2'
093500010530     C                   endif
093600010530     C*
093700010530     C                   ENDSR
093800010530     C***
093900010530
094000010530
094100010530
094200010530
094300990920      /TITLE Invio dei dati al punto operativo.
094400000613     C     invio         BEGSR
094500990920     C*
094600990920     C                   reset                   dscmz
094700030618     C                   move      invfil        cmzdst
094800030618     C***                move      '789'         cmzdst
094900990920     C                   eval      cmzfld = vlrfou
095000990920     C                   eval      cmzmbd = vlrhdl
095100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
095200000710     C                   if        prmfir = *blanks
095300030616     C                   eval      cmzfla = 'EDIVAB0F'
095400030616     C                   eval      cmzmba = 'EDIVAB0F'
095500000710     C                   else
095600000710     C                   eval      cmzfla = prmfir
095700000710     C                   eval      cmzmba = prmfir
095800000710     C                   endif
095900990920     C                   eval      cmznrr = *zeros
096000990920     C                   move      §ctrok        cmznrr
096100021003     C                   eval      cmzlba = vlrfl1
096200990920     C                   call(e)   'TIS711C'
096300990920     C                   parm                    dscmz
096400990921     C                   parm      *blanks       esito
096500990920     C                   if        %error
096600990920     C                             or cmzerr = '1'
096700990921     C                             or esito  = '1'
096800000710     C                   eval      wrkesito = '3'
096900990920     C                   endif
097000990920     C*
097100000613     C                   ENDSR
097200000613     C***
097300010530
097400010530
097500010530
097600010530
097700010530      /TITLE Invio dei dati al punto operativo.
097800010530     C     opeini        BEGSR
097900010530     C*
098000010530     C* Inizializzo flag e contatori operativi
098100010530     C                   movel     '0'           flgGiro           1
098200010530     C                   movel     '0'           flgMulti          1
098300010530     C                   movel     '1'           flgStato          1
098400010615     C                   movel     '0'           flgOk             1
098500010530     C                   z-add     *zeros        cntNonEl         10 0
098600010530     C                   z-add     *zeros        depfil            3 0
098700010530     C                   z-add     *zeros        invfil            3 0
098800010530     C*
098900010530     C                   ENDSR
099000010530     C***
099100010530
099200010530
099300010530
099400990910
099500000613     C     *inzsr        BEGSR
099600990910     C*
099700990910     C     *entry        plist
099800990920     C                   parm                    tivlrds
099900990921     C                   parm      wrkesito      esito
100000000724     C                   parm                    prmlit
100100000710     C                   parm                    prmfir
100200110705     C*
100300110705     C* LETTURA cnaco00f
100400110705     C     KEYaco00_C    KLIST
100500110705     C                   KFLD                    acokut
100600110705     C                   KFLD                    acokcc
100700110705     C                   KFLD                    acoksc
100800110705     C                   z-add     1             acokut
100900030616     C*
101000030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
101100030710     C                   move(p)   vlrMSG        wNomeFile
101200030710     C*
101300030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
101400030710     C* del codice CMR dal nome del file importato.
101500030710     C                   if        wNomeFile <> *blanks AND
101600030710     C                             vlrppt<>*blanks
101700041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
101800030710     C                   if        %subst(vlrppt:1:1) = 'S'
101900030710     C                   seton                                        50
102000030710     C                   else
102100030710     C                   setoff                                       50
102200030710     C                   endif
102300041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
102400041001     C                   if        %subst(vlrppt:6:1) = 'S'
102500041001     C                   seton                                        51
102600041001     C                   else
102700041001     C                   setoff                                       51
102800041001     C                   endif
102900041025     C*
103000041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
103100041025     C* (naturalmente solo se nn già presente)
103200041025     C                   if        %subst(vlrppt:7:1) = 'S'
103300041025     C                   seton                                        52
103400041025     C                   else
103500041025     C                   setoff                                       52
103600041025     C                   endif
103700041001     C*
103800030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
103900030710     C                   move(p)   wPosDaA       wPosDa
104000030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
104100030710     C                   move(p)   wLungA        wLung
104200030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
104300030710     C                   seton                                        60
104400030710     C                   else
104500050914     C                   if        vlrisv = 'UL'
104600050914     C                   movel     datcor        wDataOraAlfa
104700050914     C                   move      oracor        wDataOraAlfa
104800050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
104900050914     C                   seton                                        60
105000050914     C                   else
105100030710     C                   setoff                                       60
105200050914     C                   endif
105300030710     C                   endif
105400010606     C*
105500010606     C* CALCOLA LA DATA CORRENTE
105600010606     C                   time                    wn14             14 0
105700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
105800040402     C                   movel     wn14          oracor            6 0          *ORA
105900010606     C                   z-add     wn8           g08dat
106000010606     C                   z-add     *zeros        g08inv
106100010606     C                   movel     '0'           g08err
106200010606     C                   call      'XSRDA8'
106300010606     C                   parm                    wlbda8
106400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
106500000613     C*
106600000613     C                   ENDSR
106700000613     C***
