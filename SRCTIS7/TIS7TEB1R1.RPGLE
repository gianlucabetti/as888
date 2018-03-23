000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200130114     H dftactgrp(*no) actgrp(*caller)
000300990908
000400040421     Ftntbe01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030616     FEDIVABWR  O    E             DISK    usropn
000700990908
000800000512     D*------------
000900990910     D psds           sds
001000990910     D  procname         *PROC
001100050713     D*
001200050713     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001300050713     D* 10+3
001400050713     D                 DS
001500050713     D  WI15                   1     10
001600050713     D  WD15                  11     13
001700050713     D  WASC15                 1     13
001800050713     D*  6+1
001900050713     D                 DS
002000050713     D  WI09                   1      6
002100050713     D  WD09                   7      7
002200050713     D  WASC09                 1      7
002300050713     D*  2+3
002400050713     D                 DS
002500050713     D  WI07                   1      2
002600050713     D  WD07                   3      5
002700050713     D  WASC07                 1      5
002800040421     D kpjba         e ds                  inz
002900050713     D fivabds       e ds                  extname(fivab00f)
003000990920     D tivlrds       e ds                  extname(tivlr00f)
003100041025     D tisi95ds      e ds
003200040421     D dscmz         e ds                  inz
003300040421     D dosr          e ds                  inz
003400080502     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
003500080502     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
003600080502     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
003700080502     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
003800080502     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
003900080502     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
004000080502     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
004100080502     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
004200080502     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
004300080502     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
004400080502     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
004500990910     D esito           s              1
004600000724     D prmlit          s             10
004700000710     D prmfir          s             10
004800990921     D wrkesito        s                   like(esito)
004900000613     D rrnum           s              6  0 INZ(*zeros)
005000030616     D wNomeFile       s             30    INZ(*blanks)
005100030710     D wCMR            s             35    INZ(*blanks)
005200030710     D wPosDaA         s              2    INZ(*blanks)
005300030710     D wPosDa          s              2  0 INZ(*zeros)
005400030710     D wLungA          s              2    INZ(*blanks)
005500030710     D wLung           s              2  0 INZ(*zeros)
005600040421     D wCTM            s                   LIKE(vabCTM)
005700040421     D wNSR            s              2    INZ
005800040421     D wCRC            s             10    INZ
005900040421     D wKSC            s              7    INZ
006000040421     D wCTR            s              3    INZ
006100050713     D wfivabds        s                   LIKE(fivabds)
006200050914     D wDataOraAlfa    s             14    INZ(*blanks)
006300010606     D*------------------
006400010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
006500010606     D*------------------
006600010606     D WLBDA8          DS                  INZ
006700010606     D  G08DAT                 1      8  0
006800010606     D  G08INV                 9     16  0
006900010606     D  G08ERR                17     17
007000010606     D  G08TGI                18     22  0
007100080502     D*------------------
007200080502     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
007300080502     D*------------------
007400080502     D SkCliOSR        s             10    DIM(900)
007500080502     D*
007600080502     D DsCliOSR        ds
007700080502     D  wLnpOSR                       3
007800080502     D  wCliOSR                       7
007900080502     D*
008000080502     D SkLnpNrsOSR     s              5    DIM(900)
008100050713     D*---------------------------------------------------------
008200050713     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
008300050713     D*---------------------------------------------------------
008400050713     D                 ds
008500050713     D vhbatb                  1      1
008600050713     D vhbccm                  2      8
008700050713     D vhblnp                  9     11
008800050713     D vhbaas                 12     15
008900050713     D vhbmgs                 16     19
009000050713     D vhbnrs                 20     21
009100050713     D vhbnsp                 22     28
009200050713     D vhbcbo                 29     30
009300050713     D vhblna                 31     33
009400050713     D vhbrsd                 34     68
009500050713     D vhbrd2                 69    103
009600050713     D vhbind                104    138
009700050713     D vhbcad                139    147
009800050713     D vhblod                148    182
009900050713     D vhbprd                183    184
010000050713     D vhbnzd                185    187
010100050713     D vhbgc1                188    189
010200050713     D vhbgc2                190    191
010300050713     D vhbctr                192    194
010400050713     D vhbtsp                195    195
010500050713     D vhbias                196    208
010600050713     D vhbvas                209    211
010700050713     D vhbnas                212    226
010800050713     D vhbncl                227    231
010900050713     D vhbpkb                232    238
011000050713     D vhbvlb                239    243
011100050713     D vhbqft                244    256
011200050713     D vhbcas                257    269
011300050713     D vhbtic                270    271
011400050713     D vhbvca                272    274
011500050713     D vhbgca                275    276
011600050713     D vhbrmn                277    291
011700050713     D vhbrma                292    306
011800050713     D vhbncd                307    313
011900050713     D vhbnca                314    320
012000050713     D vhbxco                321    321
012100050713     D vhbnot                322    356
012200050713     D vhbnt2                357    391
012300050713     D vhbznc                392    393
012400050713     D vhbctm                394    395
012500050713     D vhbffd                396    396
012600050713     D vhbdcr                397    404
012700050713     D vhbtcr                405    405
012800050713     D vhbhcr                406    409
012900050713     D vhbcts                410    411
013000050713     D vhbftm                412    412
013100050713     D vhbvmd                413    425
013200050713     D vhbvad                426    428
013300050713     D vhbgma                429    430
013400050713     D vhbgga                431    432
013500050713     D vhbgva                433    434
013600050713     D vhbtc1                435    435
013700050713     D vhbtc2                436    436
013800050713     D vhbscl                437    437
013900050713     D vhbant                438    446
014000050713     D vhbrmo                447    471
014100050713     D vhbcmo                472    480
014200050713     D vhbnmo                481    483
014300050713     D  X_rec                  1    483    inz
014400050713     D*
014500071018     D*-------------------
014600071018     D* COSTANTI
014700071018     D*-------------------
014800071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
014900071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
015000990908
015100000913     C                   reset                   rrnum
015200990921     C                   reset                   esito
015300990921     C                   reset                   wrkesito
015400010530     C*
015500010530     C                   exsr      opeini
015600010530     C                   exsr      rwvab
015700041025     C*
015800041025     C* Esegue lancio TISI95R solo x chiusura
015900041025     C                   CLEAR                   TISI95DS
016000041025     C                   EVAL      I95TLA = 'C'
016100041025     C                   CALL      'TISI95R'
016200041025     C                   PARM                    TISI95DS
016300010530     C*
016400010530     C                   seton                                        lr
016500010530
016600010530
016700010530
016800010530
016900010530     C*--------------------------------------------------------
017000010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
017100010530     C*--------------------------------------------------------
017200010530     C     PREELA        BEGSR
017300000724     C*
017400000724     C* SE OCCORRE SPEDIRE IN FILIALE
017500010530     C                   if        invfil <> *zeros and
017600010530     C                             flgGiro = '0'
017700010530     C*
017800010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
017900010530     C                   eval      flgGiro = '1'
018000010530     C*
018100010530     C                   endif
018200010530     C*
018300010530     C                   ENDSR
018400010530     C***
018500010530
018600010530
018700010530
018800010530
018900010530
019000010530
019100010530     C*--------------------------------------------------------
019200010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
019300010530     C*--------------------------------------------------------
019400010530     C     ENDELA        BEGSR
019500040421     C*
019600040421     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
019700040923     C   54              call(e)   'TITV84R'
019800040421     C                   parm      'C'           InTipl
019900040421     C                   parm                    tivlrds
020000040421     C                   parm                    wfivabds
020100010530     C*
020200010530     C                   ENDSR
020300010530     C***
020400990908
020500010530
020600010530
020700010530
020800910830     C*--------------------------------------------------------
020900030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
021000910830     C*--------------------------------------------------------
021100941229     C     RWVAB         BEGSR
021200030710     C*
021300030710     C                   clear                   §CTROK
021400030710     C                   clear                   §CTRMO
021500030710     C                   clear                   §CTRNO
021600990910     C*
021700030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
021800030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
021900030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
022000030710     C                   if        *in60 = *on
022100990914     C                   if        not %open(tivin00r)
022200990908     C                   open      tivin00r
022300990914     C                   endif
022400030616     C                   if        not %open(edivabwr)
022500030616     C                   open      edivabwr
022600990914     C                   endif
022700990910     C*
022800921023     C                   DO        *HIVAL
022900990913     C*
023000990915     C                   READ      tivin00r                               70
023100071018     C*
023200071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
023300071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
023400010618     C*
023500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023600010618     C                   if        vinflg = '1'
023700010618     C                   eval      flgOk = '1'
023800010618     C                   endif
023900010705     C*
024000010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
024100010705     C                   if        %subst(vindta:1:1) = '='
024200010705     C                   eval      %subst(vindta:1:1) = *blanks
024300010705     C                   endif
024400010705     C*
024500000905     C                   if        vindta > *blanks
024600000613     C                   add       1             rrnum
024700010601     C* ...elimino eventuali segni
024800010601     C     '+':' '       xlate     vindta        vindta
024900990913     C*
025000010601     C                   if        *in70 = *off and
025100990913     C                             (vinflg = *blanks
025200010711     C                              or vinflg = '0'
025300010711     C                              or vinflg = '2')
025400990913     C*
025500030616     C                   clear                   EDIVAB00
025600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025700010711     C                   if        vinflg = *blanks or vinflg = '0'
025800010711     C                   clear                   vinmsg
025900010711     C                   endif
026000010711     C*
026100050713     C                   eval      x_rec  = vindta
026200991022     C*
026300991022     C                   testb     x'3F'         vhbatb                   02
026400991022     C                   if        not *in02
026500991022     C*
026600971021     C                   SETOFF                                       303132
026700050713     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
026800050713     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
026900050713     C*    OPPURE TUTTI BLANKS
027000050713     C*
027100050713     C                   TESTN                   VHBLNP               30
027200050713     C  N30              TESTN                   VHBLNP                 30
027300050713     C  N30              TESTN                   VHBLNP                   30
027400050713     C                   MOVE      VHBLNP        W001A             1
027500050713     C     *IN30         IFEQ      *OFF
027600050713     C     W001A         ORLT      '0'
027700050713     C                   SETON                                        31
027800050713     C                   eval      vinmsg = %trimr(vinmsg)
027900050713     C                             + ' ' + 'VHBLNP'
028000050713     C                   ELSE
028100050713     C                   MOVE      VHBLNP        VABLNP
028200050713     C     VABLNP        IFLE      *ZEROS
028300050713     C                   SETON                                        31
028400050713     C                   eval      vinmsg = %trimr(vinmsg)
028500050713     C                             + ' ' + 'VHBLNP'
028600050713     C                   ENDIF
028700050713     C                   ENDIF
028800050713     C*
028900050713     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029000050713     C                   eval      depfil = VABLNP
029100050713     C                   exsr      repfil
029200050713     C                   if        depfil = invfil
029300050713     C                   if        vlrpoi = 999
029400050713     C                   MOVE(P)   invfil        VABFGS
029500050713     C                   else
029600050713     C                   MOVE(P)   vlrpoi        VABFGS
029700050713     C                   endif
029800050713     C*
029900050713     C                   exsr      PREELA
030000050713     C*
030100050713     C                   TESTN                   VHBCCM               30
030200050713     C  N30              TESTN                   VHBCCM                 30
030300050713     C  N30              TESTN                   VHBCCM                   30
030400050713     C                   MOVE      VHBCCM        W001A             1
030500050713     C     *IN30         IFEQ      *OFF
030600050713     C     W001A         ORLT      '0'
030700050713     C                   SETON                                        31
030800050713     C                   eval      vinmsg = %trimr(vinmsg)
030900050713     C                             + ' ' + 'VHBCCM'
031000050713     C                   ELSE
031100050713     C                   MOVE      VHBCCM        VABCCM
031200050713     C     VABCCM        IFLE      *ZEROS
031300050713     C                   SETON                                        31
031400050713     C                   eval      vinmsg = %trimr(vinmsg)
031500050713     C                             + ' ' + 'VHBCCM'
031600050713     C                   ENDIF
031700050713     C                   ENDIF
031800050713     C*
031900050713     C                   TESTN                   VHBAAS               30
032000050713     C  N30              TESTN                   VHBAAS                 30
032100050713     C  N30              TESTN                   VHBAAS                   30
032200050713     C                   MOVE      VHBAAS        W001A             1
032300050713     C     *IN30         IFEQ      *OFF
032400050713     C     W001A         ORLT      '0'
032500050713     C     W001A         ANDNE     *BLANK
032600050713     C                   SETON                                        32
032700050713     C                   MOVEL     datcor        VABAAS
032800050713     C                   eval      vinmsg = %trimr(vinmsg)
032900050713     C                             + ' ' + 'VHBAAS'
033000050713     C                   ELSE
033100050713     C                   MOVE      VHBAAS        VABAAS
033200050713     C                   ENDIF
033300050713     C*
033400050713     C                   TESTN                   VHBMGS               30
033500050713     C  N30              TESTN                   VHBMGS                 30
033600050713     C  N30              TESTN                   VHBMGS                   30
033700050713     C                   MOVE      VHBMGS        W001A             1
033800050713     C     *IN30         IFEQ      *OFF
033900050713     C     W001A         ORLT      '0'
034000050713     C     W001A         ANDNE     *BLANK
034100050713     C                   SETON                                        32
034200050713     C                   MOVE      '0101'        VABMGS
034300050713     C                   eval      vinmsg = %trimr(vinmsg)
034400050713     C                             + ' ' + 'VHBMGS'
034500050713     C                   ELSE
034600050713     C                   MOVE      VHBMGS        VABMGS
034700050713     C                   ENDIF
034800050713     C*
034900050713     C                   TESTN                   VHBNRS               30
035000050713     C  N30              TESTN                   VHBNRS                 30
035100050713     C  N30              TESTN                   VHBNRS                   30
035200050713     C                   MOVE      VHBNRS        W001A             1
035300050713     C     *IN30         IFEQ      *OFF
035400050713     C     W001A         ORLT      '0'
035500050713     C     W001A         ANDNE     *BLANK
035600050713     C                   SETON                                        32
035700050713     C                   MOVE      *ZERO         VABNRS
035800050713     C                   eval      vinmsg = %trimr(vinmsg)
035900050713     C                             + ' ' + 'VHBNRS'
036000050713     C                   ELSE
036100050713     C                   MOVE      VHBNRS        VABNRS
036200050713     C                   ENDIF
036300050713     C*
036400050713     C                   TESTN                   VHBNSP               30
036500050713     C  N30              TESTN                   VHBNSP                 30
036600050713     C  N30              TESTN                   VHBNSP                   30
036700050713     C                   MOVE      VHBNSP        W001A             1
036800050713     C     *IN30         IFEQ      *OFF
036900050713     C     W001A         ORLT      '0'
037000050713     C     W001A         ANDNE     *BLANK
037100050713     C                   SETON                                        32
037200050713     C                   MOVE      *ZERO         VABNSP
037300050713     C                   eval      vinmsg = %trimr(vinmsg)
037400050713     C                             + ' ' + 'VHBNSP'
037500050713     C                   ELSE
037600050713     C                   MOVE      VHBNSP        VABNSP
037700050713     C                   ENDIF
037800050713     C*
037900050713     C                   TESTN                   VHBLNA               30
038000050713     C  N30              TESTN                   VHBLNA                 30
038100050713     C  N30              TESTN                   VHBLNA                   30
038200050713     C                   MOVE      VHBLNA        W001A             1
038300050713     C     *IN30         IFEQ      *OFF
038400050713     C     W001A         ORLT      '0'
038500050713     C     W001A         ANDNE     *BLANK
038600050713     C                   SETON                                        32
038700050713     C                   MOVE      *ZERO         VABLNA
038800050713     C                   eval      vinmsg = %trimr(vinmsg)
038900050713     C                             + ' ' + 'VHBLNA'
039000050713     C                   ELSE
039100050713     C                   MOVE      VHBLNA        VABLNA
039200050713     C                   ENDIF
039300050713     C*
039400050713     C                   TESTN                   VHBCTR               30
039500050713     C  N30              TESTN                   VHBCTR                 30
039600050713     C  N30              TESTN                   VHBCTR                   30
039700050713     C                   MOVE      VHBCTR        W001A             1
039800050713     C     *IN30         IFEQ      *OFF
039900050713     C     W001A         ORLT      '0'
040000050713     C     W001A         ANDNE     *BLANK
040100050713     C                   SETON                                        32
040200050713     C                   MOVE      *ZERO         VABCTR
040300050713     C                   eval      vinmsg = %trimr(vinmsg)
040400050713     C                             + ' ' + 'VHBCTR'
040500050713     C                   ELSE
040600050713     C                   MOVE      VHBCTR        VABCTR
040700050713     C                   ENDIF
040800050713     C*
040900050713     C                   MOVE      *ZERO         VABIAS
041000050713     C                   MOVE      VHBIAS        WASC15
041100050713     C                   TESTN                   WI15                 30
041200050713     C  N30              TESTN                   WI15                   30
041300050713     C  N30              TESTN                   WI15                     30
041400050713     C                   MOVE      WI15          W001A             1
041500050713     C     *IN30         IFEQ      *OFF
041600050713     C     W001A         ORLT      '0'
041700050713     C     W001A         ANDNE     *BLANK
041800050713     C                   SETON                                        32
041900050713     C                   MOVE      *ZERO         VABIAS
042000050713     C                   eval      vinmsg = %trimr(vinmsg)
042100050713     C                             + ' ' + 'VHBIAS'
042200050713     C                   ELSE
042300050713     C                   MOVEL     WI15          VABIAS
042400050713     C                   TESTN                   WD15                 30
042500050713     C  N30              TESTN                   WD15                   30
042600050713     C  N30              TESTN                   WD15                     30
042700050713     C                   MOVE      WD15          W001A             1
042800050713     C     *IN30         IFEQ      *OFF
042900050713     C     W001A         ORLT      '0'
043000050713     C     W001A         ANDNE     *BLANK
043100050713     C                   SETON                                        32
043200050713     C                   MOVE      *ZERO         VABIAS
043300050713     C                   eval      vinmsg = %trimr(vinmsg)
043400050713     C                             + ' ' + 'VHBIAS'
043500050713     C                   ELSE
043600050713     C                   MOVE      WD15          VABIAS
043700050713     C                   ENDIF
043800050713     C                   ENDIF
043900050713     C*
044000050713     C                   TESTN                   VHBNCL               30
044100050713     C  N30              TESTN                   VHBNCL                 30
044200050713     C  N30              TESTN                   VHBNCL                   30
044300050713     C                   MOVE      VHBNCL        W001A             1
044400050713     C     *IN30         IFEQ      *OFF
044500050713     C     W001A         ORLT      '0'
044600050713     C                   SETON                                        32
044700050713     C                   MOVE      *ZERO         VABNCL
044800050713     C                   eval      vinmsg = %trimr(vinmsg)
044900050713     C                             + ' ' + 'VHBNCL'
045000050713     C                   ELSE
045100050713     C                   MOVE      VHBNCL        VABNCL
045200050713     C                   ENDIF
045300050713     C*
045400050713     C                   MOVE      *ZERO         VABPKB
045500050713     C                   MOVE      VHBPKB        WASC09
045600050713     C                   IF        WI09 = *blanks
045700050713     C                   EVAL      WI09 = *zeros
045800050713     C                   ENDIF
045900050713     C                   TESTN                   WI09                 30
046000050713     C  N30              TESTN                   WI09                   30
046100050713     C  N30              TESTN                   WI09                     30
046200050713     C                   MOVE      WI09          W001A             1
046300050713     C     *IN30         IFEQ      *OFF
046400050713     C     W001A         ORLT      '0'
046500050713     C                   SETON                                        32
046600050713     C                   MOVE      *ZERO         VABPKB
046700050713     C                   eval      vinmsg = %trimr(vinmsg)
046800050713     C                             + ' ' + 'VHBPKB'
046900050713     C                   ELSE
047000050713     C                   MOVEL     WI09          VABPKB
047100050713     C                   IF        WD09 = *blanks
047200050713     C                   EVAL      WD09 = *zeros
047300050713     C                   ENDIF
047400050713     C                   TESTN                   WD09                 30
047500050713     C  N30              TESTN                   WD09                     30
047600050713     C                   MOVE      WD09          W001A             1
047700050713     C     *IN30         IFEQ      *OFF
047800050713     C     W001A         ORLT      '0'
047900050713     C     W001A         ANDNE     *BLANK
048000050713     C                   SETON                                        32
048100050713     C                   MOVE      *ZERO         VABPKB
048200050713     C                   eval      vinmsg = %trimr(vinmsg)
048300050713     C                             + ' ' + 'VHBPKB'
048400050713     C                   ELSE
048500050713     C                   MOVE      WD09          VABPKB
048600050713     C                   ENDIF
048700050713     C                   ENDIF
048800050713     C*
048900050713     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
049000050713     C                   IF        *IN51 = *OFF
049100050713     C                   MOVE      *ZERO         VABVLB
049200050713     C                   MOVE      VHBVLB        WASC07
049300050713     C                   IF        WI07 = *blanks
049400050713     C                   EVAL      WI07 = *zeros
049500050713     C                   ENDIF
049600050713     C                   TESTN                   WI07                 30
049700050713     C  N30              TESTN                   WI07                   30
049800050713     C  N30              TESTN                   WI07                     30
049900050713     C                   MOVE      WI07          W001A             1
050000050713     C     *IN30         IFEQ      *OFF
050100050713     C     W001A         ORLT      '0'
050200050713     C     W001A         ANDNE     *BLANK
050300050713     C                   SETON                                        32
050400050713     C                   MOVE      *ZERO         VABVLB
050500050713     C                   eval      vinmsg = %trimr(vinmsg)
050600050713     C                             + ' ' + 'VHBVLB'
050700050713     C                   ELSE
050800050713     C                   MOVEL     WI07          VABVLB
050900050713     C                   IF        WD07 = *blanks
051000050713     C                   EVAL      WD07 = *zeros
051100050713     C                   ENDIF
051200050713     C                   TESTN                   WD07                 30
051300050713     C  N30              TESTN                   WD07                   30
051400050713     C  N30              TESTN                   WD07                     30
051500050713     C                   MOVE      WD07          W001A             1
051600050713     C     *IN30         IFEQ      *OFF
051700050713     C     W001A         ORLT      '0'
051800050713     C     W001A         ANDNE     *BLANK
051900050713     C                   SETON                                        32
052000050713     C                   MOVE      *ZERO         VABVLB
052100050713     C                   eval      vinmsg = %trimr(vinmsg)
052200050713     C                             + ' ' + 'VHBVLB'
052300050713     C                   ELSE
052400050713     C                   MOVE      WD07          VABVLB
052500050713     C                   ENDIF
052600050713     C                   ENDIF
052700050713     C                   ELSE
052800050713     C* ...altrimenti forzo azzeramento del volume
052900050713     C                   Z-ADD     *zeros        VABVLB
053000050713     C                   ENDIF
053100050713     C*
053200050713     C                   MOVE      *ZERO         VABQFT
053300050713     C                   MOVE      VHBQFT        WASC15
053400050713     C                   IF        WI15 = *blanks
053500050713     C                   EVAL      WI15 = *zeros
053600050713     C                   ENDIF
053700050713     C                   TESTN                   WI15                 30
053800050713     C  N30              TESTN                   WI15                   30
053900050713     C  N30              TESTN                   WI15                     30
054000050713     C                   MOVE      WI15          W001A             1
054100050713     C     *IN30         IFEQ      *OFF
054200050713     C     W001A         ORLT      '0'
054300050713     C     W001A         ANDNE     *BLANK
054400050713     C                   SETON                                        32
054500050713     C                   MOVE      *ZERO         VABQFT
054600050713     C                   eval      vinmsg = %trimr(vinmsg)
054700050713     C                             + ' ' + 'VHBQFT'
054800050713     C                   ELSE
054900050713     C                   MOVEL     WI15          VABQFT
055000050713     C                   IF        WD15 = *blanks
055100050713     C                   EVAL      WD15 = *zeros
055200050713     C                   ENDIF
055300050713     C                   TESTN                   WD15                 30
055400050713     C  N30              TESTN                   WD15                   30
055500050713     C  N30              TESTN                   WD15                     30
055600050713     C                   MOVE      WD15          W001A             1
055700050713     C     *IN30         IFEQ      *OFF
055800050713     C     W001A         ORLT      '0'
055900050713     C     W001A         ANDNE     *BLANK
056000050713     C                   SETON                                        32
056100050713     C                   MOVE      *ZERO         VABQFT
056200050713     C                   eval      vinmsg = %trimr(vinmsg)
056300050713     C                             + ' ' + 'VHBQFT'
056400050713     C                   ELSE
056500050713     C                   MOVE      WD15          VABQFT
056600050713     C                   ENDIF
056700050713     C                   ENDIF
056800050713     C*
056900050713     C                   MOVE      *ZERO         VABCAS
057000050713     C                   MOVE      VHBCAS        WASC15
057100050713     C                   IF        WI15 = *blanks
057200050713     C                   EVAL      WI15 = *zeros
057300050713     C                   ENDIF
057400050713     C                   TESTN                   WI15                 30
057500050713     C  N30              TESTN                   WI15                   30
057600050713     C  N30              TESTN                   WI15                     30
057700050713     C                   MOVE      WI15          W001A             1
057800050713     C     *IN30         IFEQ      *OFF
057900050713     C     W001A         ORLT      '0'
058000050713     C     W001A         ANDNE     *BLANK
058100050713     C                   SETON                                        32
058200050713     C                   MOVE      *ZERO         VABCAS
058300050713     C                   eval      vinmsg = %trimr(vinmsg)
058400050713     C                             + ' ' + 'VHBCAS'
058500050713     C                   ELSE
058600050713     C                   MOVEL     WI15          VABCAS
058700050713     C                   IF        WD15 = *blanks
058800050713     C                   EVAL      WD15 = *zeros
058900050713     C                   ENDIF
059000050713     C                   TESTN                   WD15                 30
059100050713     C  N30              TESTN                   WD15                   30
059200050713     C  N30              TESTN                   WD15                     30
059300050713     C                   MOVE      WD15          W001A             1
059400050713     C     *IN30         IFEQ      *OFF
059500050713     C     W001A         ORLT      '0'
059600050713     C     W001A         ANDNE     *BLANK
059700050713     C                   SETON                                        32
059800050713     C                   MOVE      *ZERO         VABCAS
059900050713     C                   eval      vinmsg = %trimr(vinmsg)
060000050713     C                             + ' ' + 'VHBCAS'
060100050713     C                   ELSE
060200050713     C                   MOVE      WD15          VABCAS
060300050713     C                   ENDIF
060400050713     C                   ENDIF
060500050713     C*
060600050713     C                   TESTN                   VHBRMN               30
060700050713     C  N30              TESTN                   VHBRMN                 30
060800050713     C  N30              TESTN                   VHBRMN                   30
060900050713     C                   MOVE      VHBRMN        W001A             1
061000050713     C     *IN30         IFEQ      *OFF
061100050713     C     W001A         ORLT      '0'
061200050713     C                   SETON                                        32
061300050713     C                   MOVE      *ZERO         VABRMN
061400050713     C                   eval      vinmsg = %trimr(vinmsg)
061500050713     C                             + ' ' + 'VHBRMN'
061600050713     C                   ELSE
061700050713     C                   MOVE      VHBRMN        VABRMN
061800050713     C                   ENDIF
061900050713     C*
062000050713     C                   TESTN                   VHBNCD               30
062100050713     C  N30              TESTN                   VHBNCD                 30
062200050713     C  N30              TESTN                   VHBNCD                   30
062300050713     C                   MOVE      VHBNCD        W001A             1
062400050713     C     *IN30         IFEQ      *OFF
062500050713     C     W001A         ORLT      '0'
062600050713     C     W001A         ANDNE     *BLANK
062700050713     C                   SETON                                        32
062800050713     C                   MOVE      *ZERO         VABNCD
062900050713     C                   eval      vinmsg = %trimr(vinmsg)
063000050713     C                             + ' ' + 'VHBNCD'
063100050713     C                   ELSE
063200050713     C                   MOVE      VHBNCD        VABNCD
063300050713     C                   ENDIF
063400050713     C*
063500050713     C                   TESTN                   VHBNCA               30
063600050713     C  N30              TESTN                   VHBNCA                 30
063700050713     C  N30              TESTN                   VHBNCA                   30
063800050713     C                   MOVE      VHBNCA        W001A             1
063900050713     C     *IN30         IFEQ      *OFF
064000050713     C     W001A         ORLT      '0'
064100050713     C     W001A         ANDNE     *BLANK
064200050713     C                   SETON                                        32
064300050713     C                   MOVE      *ZERO         VABNCA
064400050713     C                   eval      vinmsg = %trimr(vinmsg)
064500050713     C                             + ' ' + 'VHBNCA'
064600050713     C                   ELSE
064700050713     C                   MOVE      VHBNCA        VABNCA
064800050713     C                   ENDIF
064900050713     C*
065000050713     C                   TESTN                   VHBZNC               30
065100050713     C  N30              TESTN                   VHBZNC                 30
065200050713     C  N30              TESTN                   VHBZNC                   30
065300050713     C                   MOVE      VHBZNC        W001A             1
065400050713     C     *IN30         IFEQ      *OFF
065500050713     C     W001A         ORLT      '0'
065600050713     C     W001A         ANDNE     *BLANK
065700050713     C                   SETON                                        32
065800050713     C                   MOVE      *ZERO         VABZNC
065900050713     C                   eval      vinmsg = %trimr(vinmsg)
066000050713     C                             + ' ' + 'VHBZNC'
066100050713     C                   ELSE
066200050713     C                   MOVE      VHBZNC        VABZNC
066300050713     C                   ENDIF
066400050713     C*
066500050713     C                   TESTN                   VHBDCR               30
066600050713     C  N30              TESTN                   VHBDCR                 30
066700050713     C  N30              TESTN                   VHBDCR                   30
066800050713     C                   MOVE      VHBDCR        W001A             1
066900050713     C     *IN30         IFEQ      *OFF
067000050713     C     W001A         ORLT      '0'
067100050713     C     W001A         ANDNE     *BLANK
067200050713     C                   SETON                                        32
067300050713     C                   MOVE      *ZERO         VABDCR
067400050713     C                   eval      vinmsg = %trimr(vinmsg)
067500050713     C                             + ' ' + 'VHBDCR'
067600050713     C                   ELSE
067700050713     C                   MOVE      VHBDCR        VABDCR
067800050713     C                   ENDIF
067900050713     C*
068000050713     C                   TESTN                   VHBHCR               30
068100050713     C  N30              TESTN                   VHBHCR                 30
068200050713     C  N30              TESTN                   VHBHCR                   30
068300050713     C                   MOVE      VHBHCR        W001A             1
068400050713     C     *IN30         IFEQ      *OFF
068500050713     C     W001A         ORLT      '0'
068600050713     C     W001A         ANDNE     *BLANK
068700050713     C                   SETON                                        32
068800050713     C                   MOVE      *ZERO         VABHCR
068900050713     C                   eval      vinmsg = %trimr(vinmsg)
069000050713     C                             + ' ' + 'VHBHCR'
069100050713     C                   ELSE
069200050713     C                   MOVE      VHBHCR        VABHCR
069300050713     C                   ENDIF
069400050713     C*
069500050713     C                   MOVE      *ZERO         VABVMD
069600050713     C                   MOVE      VHBVMD        WASC15
069700050713     C                   IF        WI15 = *blanks
069800050713     C                   EVAL      WI15 = *zeros
069900050713     C                   ENDIF
070000050713     C                   TESTN                   WI15                 30
070100050713     C  N30              TESTN                   WI15                   30
070200050713     C  N30              TESTN                   WI15                     30
070300050713     C                   MOVE      WI15          W001A             1
070400050713     C     *IN30         IFEQ      *OFF
070500050713     C     W001A         ORLT      '0'
070600050713     C     W001A         ANDNE     *BLANK
070700050713     C                   SETON                                        32
070800050713     C                   MOVE      *ZERO         VABVMD
070900050713     C                   eval      vinmsg = %trimr(vinmsg)
071000050713     C                             + ' ' + 'VHBVMD'
071100050713     C                   ELSE
071200050713     C                   MOVEL     WI15          VABVMD
071300050713     C                   IF        WD15 = *blanks
071400050713     C                   EVAL      WD15 = *zeros
071500050713     C                   ENDIF
071600050713     C                   TESTN                   WD15                 30
071700050713     C  N30              TESTN                   WD15                   30
071800050713     C  N30              TESTN                   WD15                     30
071900050713     C                   MOVE      WD15          W001A             1
072000050713     C     *IN30         IFEQ      *OFF
072100050713     C     W001A         ORLT      '0'
072200050713     C     W001A         ANDNE     *BLANK
072300050713     C                   SETON                                        32
072400050713     C                   MOVE      *ZERO         VABVMD
072500050713     C                   eval      vinmsg = %trimr(vinmsg)
072600050713     C                             + ' ' + 'VHBVMD'
072700050713     C                   ELSE
072800050713     C                   MOVE      WD15          VABVMD
072900050713     C                   ENDIF
073000050713     C                   ENDIF
073100050713     C*
073200050713     C                   TESTN                   VHBANT               30
073300050713     C  N30              TESTN                   VHBANT                 30
073400050713     C  N30              TESTN                   VHBANT                   30
073500050713     C                   MOVE      VHBANT        W001A             1
073600050713     C     *IN30         IFEQ      *OFF
073700050713     C     W001A         ORLT      '0'
073800050713     C     W001A         ANDNE     *BLANK
073900050713     C                   SETON                                        32
074000050713     C                   MOVE      *ZERO         VABANT
074100050713     C                   eval      vinmsg = %trimr(vinmsg)
074200050713     C                             + ' ' + 'VHBANT'
074300050713     C                   ELSE
074400050713     C                   MOVE      VHBANT        VABANT
074500050713     C                   ENDIF
074600050713     C*
074700050713     C* CAMPI ALFABETICI:
074800050713     C                   MOVE      VHBATB        VABATB
074900050713     C                   MOVE      VHBCBO        VABCBO
075000050713     C                   MOVE      VHBRSD        VABRSD
075100050713     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
075200050713     C     '@':'A'       XLATE     VABRSD        VABRSD
075300050713     C* ==
075400050713     C                   MOVE      VHBRD2        VABRD2
075500050713     C                   MOVE      VHBIND        VABIND
075600050713     C                   MOVE      VHBCAD        VABCAD
075700050713     C                   MOVE      VHBLOD        VABLOD
075800050713     C                   MOVE      VHBPRD        VABPRD
075900050713     C                   MOVE      VHBNZD        VABNZD
076000050713     C                   MOVE      VHBGC1        VABGC1
076100050713     C                   MOVE      VHBGC2        VABGC2
076200050713     C                   MOVE      VHBTSP        VABTSP
076300050713     C                   MOVE      VHBVAS        VABVAS
076400050713     C                   MOVE      VHBNAS        VABNAS
076500050713     C                   MOVE      VHBTIC        VABTIC
076600050713     C                   MOVE      VHBVCA        VABVCA
076700050713     C                   MOVE      VHBGCA        VABGCA
076800050713     C                   MOVE      VHBRMA        VABRMA
076900050713     C                   MOVE      VHBXCO        VABXCO
077000050713     C                   MOVE      VHBNOT        VABNOT
077100050713     C                   MOVE      VHBNT2        VABNT2
077200050713     C                   MOVE      VHBCTM        VABCTM
077300050713     C                   MOVE      VHBFFD        VABFFD
077400050713     C                   MOVE      VHBTCR        VABTCR
077500050713     C                   MOVE      VHBCTS        VABCTS
077600050713     C                   MOVE      VHBFTM        VABFTM
077700050713     C                   MOVE      VHBVAD        VABVAD
077800050713     C                   MOVE      VHBGMA        VABGMA
077900050713     C                   MOVE      VHBGGA        VABGGA
078000050713     C                   MOVE      VHBGVA        VABGVA
078100050713     C                   MOVE      VHBTC1        VABTC1
078200050713     C                   MOVE      VHBTC2        VABTC2
078300050713     C                   MOVE      VHBSCL        VABSCL
078400050713     C                   MOVE      VHBRMO        VABRMO
078500050713     C                   MOVE      VHBCMO        VABCMO
078600050713     C                   MOVE      VHBNMO        VABNMO
078700050713     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
078800050713     C* NELLA NAZIONE ORIGINALE
078900050713     C                   MOVE      VHBNMO        WA1D              1
079000050713     C                   MOVEL     VHBNMO        WA1S              1
079100050713     C     WA1D          IFNE      *BLANK
079200050713     C     WA1S          ANDEQ     *BLANK
079300050713     C                   MOVE      *BLANK        VABNMO
079400050713     C                   ENDIF
079500030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
079600030710     C                   EVAL      VABCMR = wCMR
079700030616     C                   EVAL      VABDCM = DATCOR
079800030616     C                   EVAL      VABDTS = DATCOR
079900040402     C                   EVAL      VABHMS = ORACOR
080000030616     C                   EVAL      VABCNT = 1
080100040421     C*
080200040421     C* Se il codice trattamento merce nn viene passato dal cliente lo valorizzo con il valore
080300040421     C* d default passato come parametro dall'anagrafica clienti/tipo file
080400040421     C                   IF        VABCTM = *blanks
080500040421     C                   EVAL      VABCTM = wCTM
080600040421     C                   ENDIF
080700040421     C*
080800040421     C* Se presente particolarità consegna 'RC' e se il codice
080900040421     C* cliente mittente è valorizzato e numero serie ORM nn passata
081000040421     C* nei parametri verifico se il cliente è abilitato in tabella 'OSR'
081100040421     C                   IF        VABGMA = 'RC'
081200040421     C                   IF        VABCCM > *ZEROS
081300040421     C                   IF        wNSR = *BLANKS
081400040421     C                   Z-ADD     1             jOSR
081500080502     C                   move(p)   VABLNP        wLnpOSR
081600080502     C                   move(p)   VABCCM        wCliOSR
081700080502     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
081800040421     C                   IF        %found
081900040421     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
082000040423     C                   EVAL      %subst(vlrppt:4:2) = wNSR
082100040421     C                   ELSE
082200040421     C                   SETON                                        31
082300040421     C                   EVAL      vinmsg = %trimr(vinmsg)
082400040421     C                             + ' ' + 'ORMNRS(tabellaOSR)'
082500040421     C                   ENDIF
082600040421     C                   ENDIF
082700040421     C                   ENDIF
082800040421     C                   ENDIF
082900040421     C*
083000040421     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
083100040421     C                   if        *IN31 = *OFF AND
083200040421     C                             vabGMA = 'RC'
083300040923     C*
083400040923     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
083500040923     C                   seton                                        54
083600040421     C*
083700040421     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
083800040421     C                   eval      wfivabds = fivabds
083900040920     C                   call      'TITV84R'
084000040421     C                   parm      *blanks       InTipl            1
084100040421     C                   parm                    tivlrds
084200040421     C                   parm                    wfivabds
084300040421     C*
084400040421     C                   endif
084500171003     C*
084600171003     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
084700171003     C                   IF        %trim(VABNZD) = 'I'   OR
084800171003     C                             %trim(VABNZD) = 'IT'  OR
084900171003     C                             %trim(VABNZD) = 'ITA'
085000171003     C                   EVAL      VABNZD = *blanks
085100171003     C                   ENDIF
085200041025     C*
085300041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
085400041025     C                   IF        *IN52 = *ON
085500041025     C* Reperisco la provincia dal CAP e dalla località
085600041025     C                   IF        VABCAD <> *blanks AND
085700050907     C                             VABPRD  = *blanks AND
085800050907     C                             VABNZD  = *blanks
085900041025     C                   CLEAR                   TISI95DS
086000041025     C                   EVAL      I95TCN = '3'
086100041025     C                   Z-ADD     datcor        I95DAT
086200041025     C                   EVAL      I95CAP = VABCAD
086300041025     C                   EVAL      I95LOC = VABLOD
086400041025     C                   CALL      'TISI95R'
086500041025     C                   PARM                    TISI95DS
086600041025     C                   EVAL      VABPRD = O95PRV
086700041025     C                   ENDIF
086800041025     C                   ENDIF
086900100419     C*
087000100419     C* Forzatura su campi DIVISE
087100100419     C                   IF        VABVAS = *blanks
087200100419     C                   EVAL      VABVAS = 'EUR'
087300100419     C                   ENDIF
087400100419     C                   IF        VABVCA = *blanks
087500100419     C                   EVAL      VABVCA = 'EUR'
087600100419     C                   ENDIF
087700100419     C                   IF        VABVAD = *blanks
087800100419     C                   EVAL      VABVAD = 'EUR'
087900100419     C                   ENDIF
088000930409     C*
088100010604     C  N31              ADD       1             §CTROK            7 0
088200010604     C   32              ADD       1             §CTRMO            7 0
088300010604     C   31              ADD       1             §CTRNO            7 0
088400030616     C  N31              WRITE     EDIVAB00
088500990910     C*
088600010601     C                   if        *in31 = *off and
088700010601     C                             *in32 = *off
088800990910     C                   eval      vinflg = '1'
088900990910     C                   else
089000990910     C                   eval      vinflg = '2'
089100990910     C                   endif
089200991022     C                   endif
089300990910     C                   endif
089400010601     C*
089500010601     C                   endif
089600000905     C*
089700000905     C                   else
089800000905     C                   eval      vinflg = '1'
089900000905     C                   endif
090000000905     C*
090100000905     C  N70              update    tivin000
090200991022     C*
090300991022     C  N70              ENDdo
090400010530     C
090500010530     C*
090600010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
090700010530     C                   if        cntNonEl = *zeros or
090800010530     C                             flgMulti = '0'
090900990910     C* Se non ci sono record con errori ...
091000010530     C                   if        §ctrno = 0 and
091100010601     C                             §ctrmo = 0 and
091200010530     C                             flgStato <> '2'
091300990910     C* ... restituisco esito OK.
091400990921     C                   eval      wrkesito = '0'
091500990910     C                   else
091600010601     C                   if        §ctrok > 0
091700990921     C                   eval      wrkesito = '1'
091800000710     C                   else
091900010615     C                   if        flgOk = '0'
092000010615     C                   eval      wrkesito = '2'
092100010615     C                   else
092200010615     C                   eval      wrkesito = '6'
092300010615     C                   endif
092400990910     C                   endif
092500000710     C                   endif
092600010530     C                   else
092700010530     C                   eval      wrkesito = '9'
092800010530     C                   endif
092900030710     C*
093000030710 X  1C                   else
093100030710     C                   eval      wrkesito = '8'
093200030710     C                   eval      flgGiro = '1'
093300030710 E  1C                   endif
093400030710     C*
093500990914     C                   if        %open(tivin00r)
093600990908     C                   close     tivin00r
093700990914     C                   endif
093800030616     C                   if        %open(edivabwr)
093900030616     C                   close     edivabwr
094000990914     C                   endif
094100010530     C*
094200010530     C                   if        vlrpoi <> 999
094300010530     C                   eval      invfil = vlrpoi
094400010530     C                   endif
094500990910     C*
094600990920     C                   if        §ctrok > 0
094700010530     C                             and invfil > *zeros
094800000613     C                   exsr      invio
094900990920     C                   endif
095000010612     C*
095100010612     C                   if        flgGiro = '1'
095200010612     C                   exsr      endela
095300010612     C                   endif
095400990920     C*
095500910830     C                   ENDSR
095600000613     C***
095700990920
095800010530
095900010530
096000010530
096100010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
096200010530     C     repfil        BEGSR
096300010530     C*
096400010530     C                   if        invfil = *zeros and
096500010530     C                             depfil > *zeros and
096600010613     C                             (vinflg = *blanks or
096700010613     C                              vinflg = *zeros)
096800010530     C
096900010530     C                   eval      invfil = depfil
097000010530     C                   endif
097100010530     C*
097200010530     C                   if        depfil <> invfil and
097300010530     C                             invfil > *zeros
097400010530     C                   eval      flgMulti = '1'
097500010530     C                   if        vinflg = *blanks
097600010530     C                   add       1             cntNonEl
097700010530     C                   endif
097800010530     C                   endif
097900010530     C*
098000010530     C                   if        vinflg = '2'
098100010530     C                   eval      flgStato = '2'
098200010530     C                   endif
098300010530     C*
098400010530     C                   ENDSR
098500010530     C***
098600010530
098700010530
098800010530
098900010530
099000990920      /TITLE Invio dei dati al punto operativo.
099100000613     C     invio         BEGSR
099200990920     C*
099300990920     C                   reset                   dscmz
099400030618     C                   move      invfil        cmzdst
099500030618     C***                move      '789'         cmzdst
099600990920     C                   eval      cmzfld = vlrfou
099700990920     C                   eval      cmzmbd = vlrhdl
099800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
099900000710     C                   if        prmfir = *blanks
100000030616     C                   eval      cmzfla = 'EDIVAB0F'
100100030616     C                   eval      cmzmba = 'EDIVAB0F'
100200000710     C                   else
100300000710     C                   eval      cmzfla = prmfir
100400000710     C                   eval      cmzmba = prmfir
100500000710     C                   endif
100600990920     C                   eval      cmznrr = *zeros
100700990920     C                   move      §ctrok        cmznrr
100800021003     C                   eval      cmzlba = vlrfl1
100900990920     C                   call(e)   'TIS711C'
101000990920     C                   parm                    dscmz
101100990921     C                   parm      *blanks       esito
101200990920     C                   if        %error
101300990920     C                             or cmzerr = '1'
101400990921     C                             or esito  = '1'
101500000710     C                   eval      wrkesito = '3'
101600990920     C                   endif
101700990920     C*
101800000613     C                   ENDSR
101900000613     C***
102000010530
102100010530
102200010530
102300010530      /TITLE Invio dei dati al punto operativo.
102400010530     C     opeini        BEGSR
102500010530     C*
102600010530     C* Inizializzo flag e contatori operativi
102700010530     C                   movel     '0'           flgGiro           1
102800010530     C                   movel     '0'           flgMulti          1
102900010530     C                   movel     '1'           flgStato          1
103000010615     C                   movel     '0'           flgOk             1
103100010530     C                   z-add     *zeros        cntNonEl         10 0
103200010530     C                   z-add     *zeros        depfil            3 0
103300010530     C                   z-add     *zeros        invfil            3 0
103400040423     C*
103500040423     C* Verifico se impostato il parametro relativo al codice trattamento merce (VABCTM)
103600040423     C                   if        %subst(vlrppt:2:2) <> *blanks
103700040423     C                   eval      wCTM = %subst(vlrppt:2:2)
103800040423     C                   endif
103900040423     C*
104000040423     C* Verifico se impostato il parametro relativo al numero serie orm (ORMNSR)
104100040423     C                   if        %subst(vlrppt:4:2) <> *blanks
104200040423     C                   eval      wNSR = %subst(vlrppt:4:2)
104300040423     C                   endif
104400040423     C*
104500040423     C* Verifico se impostato il parametro relativo al codice consegna (ORMCRC)
104600040423     C                   if        %subst(vlrppt:6:10) <> *blanks
104700040423     C                   eval      wCRC = %subst(vlrppt:6:10)
104800040423     C                   endif
104900040423     C*
105000040423     C* Verifico se impostato il parametro relativo al codice pagante (ORMKSC)
105100040423     C                   if        %subst(vlrppt:16:7) <> *blanks
105200040423     C                   eval      wKSC = %subst(vlrppt:16:7)
105300040423     C                   endif
105400040423     C*
105500040423     C* Verifico se impostato il parametro relativo al codice pagante (ORMCTR)
105600040423     C                   if        %subst(vlrppt:23:3) <> *blanks
105700040423     C                   eval      wCTR = %subst(vlrppt:23:3)
105800040423     C                   endif
105900010530     C*
106000010530     C                   ENDSR
106100010530     C***
106200040421
106300080502
106400040421
106500080502     C*--------------------------------------------------------
106600080502     C* CARTAB - CARICAMENTO TABELLE                          *
106700080502     C*--------------------------------------------------------
106800080502     C     CARTAB        BEGSR
106900080502     C*
107000080502     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
107100080502     C                   z-add     *zeros        jOSR              3 0
107200080502     C     'OSR'         setll     tntbe01l
107300080502     C                   if        %equal(tntbe01l)
107400080502     C     'OSR'         reade     tntbe01l
107500080502     C                   dow       not %eof(tntbe01l)
107600080502     C                   eval      DOSR = tbeUNI
107700080502     c*
107800080502     c* Verifica e forzatura x nuovi campi aggiunti
107900080502     c                   if        aD§OSRcl6 = *blanks
108000080502     c                   eval      D§OSRcl6 = *zeros
108100080502     c                   endif
108200080502     c                   if        aD§OSRcl7 = *blanks
108300080502     c                   eval      D§OSRcl7 = *zeros
108400080502     c                   endif
108500080502     c                   if        aD§OSRcl8 = *blanks
108600080502     c                   eval      D§OSRcl8 = *zeros
108700080502     c                   endif
108800080502     c                   if        aD§OSRcl9 = *blanks
108900080502     c                   eval      D§OSRcl9 = *zeros
109000080502     c                   endif
109100080502     c                   if        aD§OSRcl10 = *blanks
109200080502     c                   eval      D§OSRcl10 = *zeros
109300080502     c                   endif
109400080502     c                   if        aD§OSRcl11 = *blanks
109500080502     c                   eval      D§OSRcl11 = *zeros
109600080502     c                   endif
109700080502     c                   if        aD§OSRcl12 = *blanks
109800080502     c                   eval      D§OSRcl12 = *zeros
109900080502     c                   endif
110000080502     c                   if        aD§OSRcl13 = *blanks
110100080502     c                   eval      D§OSRcl13 = *zeros
110200080502     c                   endif
110300080502     c                   if        aD§OSRcl14 = *blanks
110400080502     c                   eval      D§OSRcl14 = *zeros
110500080502     c                   endif
110600080502     c                   if        aD§OSRcl15 = *blanks
110700080502     c                   eval      D§OSRcl15 = *zeros
110800080502     c                   endif
110900080502     c                   if        aD§OSRcl16 = *blanks
111000080502     c                   eval      D§OSRcl16 = *zeros
111100080502     c                   endif
111200080502     c*
111300080502     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
111400080502     C                   if        D§OSRCLI <> *zeros
111500080502     C                   add       1             jOSR
111600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
111700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
111800080502     C                   movel     D§OSRCLI      wCliOSR
111900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
112000080502     C                   endif
112100080502     C*
112200080502     C                   if        D§OSRCL2 <> *zeros
112300080502     C                   add       1             jOSR
112400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
112500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
112600080502     C                   movel     D§OSRCL2      wCliOSR
112700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
112800080502     C                   endif
112900080502     C*
113000080502     C                   if        D§OSRCL3 <> *zeros
113100080502     C                   add       1             jOSR
113200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
113300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
113400080502     C                   movel     D§OSRCL3      wCliOSR
113500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
113600080502     C                   endif
113700080502     C*
113800080502     C                   if        D§OSRCL4 <> *zeros
113900080502     C                   add       1             jOSR
114000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
114100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
114200080502     C                   movel     D§OSRCL4      wCliOSR
114300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
114400080502     C                   endif
114500080502     C*
114600080502     C                   if        D§OSRCL5 <> *zeros
114700080502     C                   add       1             jOSR
114800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
114900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
115000080502     C                   movel     D§OSRCL5      wCliOSR
115100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
115200080502     C                   endif
115300080502     C*
115400080502     C                   if        D§OSRCL6 <> *zeros
115500080502     C                   add       1             jOSR
115600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
115700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
115800080502     C                   movel     D§OSRCL6      wCliOSR
115900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
116000080502     C                   endif
116100080502     C*
116200080502     C                   if        D§OSRCL7 <> *zeros
116300080502     C                   add       1             jOSR
116400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
116500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
116600080502     C                   movel     D§OSRCL7      wCliOSR
116700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
116800080502     C                   endif
116900080502     C*
117000080502     C                   if        D§OSRCL8 <> *zeros
117100080502     C                   add       1             jOSR
117200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
117300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
117400080502     C                   movel     D§OSRCL8      wCliOSR
117500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
117600080502     C                   endif
117700080502     C*
117800080502     C                   if        D§OSRCL9 <> *zeros
117900080502     C                   add       1             jOSR
118000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
118100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
118200080502     C                   movel     D§OSRCL9      wCliOSR
118300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
118400080502     C                   endif
118500080502     C*
118600080502     C                   if        D§OSRCL10 <> *zeros
118700080502     C                   add       1             jOSR
118800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
118900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
119000080502     C                   movel     D§OSRCL10     wCliOSR
119100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
119200080502     C                   endif
119300080502     C*
119400080502     C                   if        D§OSRCL11 <> *zeros
119500080502     C                   add       1             jOSR
119600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
119700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
119800080502     C                   movel     D§OSRCL11     wCliOSR
119900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
120000080502     C                   endif
120100080502     C*
120200080502     C                   if        D§OSRCL12 <> *zeros
120300080502     C                   add       1             jOSR
120400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
120500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
120600080502     C                   movel     D§OSRCL12     wCliOSR
120700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
120800080502     C                   endif
120900080502     C*
121000080502     C                   if        D§OSRCL13 <> *zeros
121100080502     C                   add       1             jOSR
121200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
121300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
121400080502     C                   movel     D§OSRCL13     wCliOSR
121500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
121600080502     C                   endif
121700080502     C*
121800080502     C                   if        D§OSRCL14 <> *zeros
121900080502     C                   add       1             jOSR
122000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
122100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
122200080502     C                   movel     D§OSRCL14     wCliOSR
122300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
122400080502     C                   endif
122500080502     C*
122600080502     C                   if        D§OSRCL15 <> *zeros
122700080502     C                   add       1             jOSR
122800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
122900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
123000080502     C                   movel     D§OSRCL15     wCliOSR
123100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
123200080502     C                   endif
123300080502     C*
123400080502     C                   if        D§OSRCL16 <> *zeros
123500080502     C                   add       1             jOSR
123600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
123700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
123800080502     C                   movel     D§OSRCL16     wCliOSR
123900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
124000080502     C                   endif
124100080502     C*
124200080502     C     'OSR'         reade     tntbe01l
124300080502     C                   enddo
124400080502     C                   endif
124500080502     C*
124600080502     C                   ENDSR
124700080502     C***
124800040421
124900010530
125000990910
125100000613     C     *inzsr        BEGSR
125200990910     C*
125300990910     C     *entry        plist
125400990920     C                   parm                    tivlrds
125500990921     C                   parm      wrkesito      esito
125600000724     C                   parm                    prmlit
125700000710     C                   parm                    prmfir
125800040421     C*
125900040421     C* CARICO LE TABELLE OCCORRENTI
126000040421     C                   exsr      CARTAB
126100030616     C*
126200030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
126300030710     C                   move(p)   vlrMSG        wNomeFile
126400030710     C*
126500030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
126600030710     C* del codice CMR dal nome del file importato.
126700030710     C                   if        wNomeFile <> *blanks AND
126800030710     C                             vlrppt<>*blanks
126900041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
127000041001     C                   if        %subst(vlrppt:1:1) = 'S'
127100030710     C                   seton                                        50
127200030710     C                   else
127300030710     C                   setoff                                       50
127400030710     C                   endif
127500041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
127600041001     C                   if        %subst(vlrppt:30:1) = 'S'
127700041001     C                   seton                                        51
127800041001     C                   else
127900041001     C                   setoff                                       51
128000041001     C                   endif
128100041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
128200041025     C* (naturalmente solo se nn già presente)
128300041025     C                   if        %subst(vlrppt:31:1) = 'S'
128400041025     C                   seton                                        52
128500041025     C                   else
128600041025     C                   setoff                                       52
128700041025     C                   endif
128800041001     C*
128900040423     C                   eval      wPosDaA = %subst(vlrppt:26:2)
129000030710     C                   move(p)   wPosDaA       wPosDa
129100040423     C                   eval      wLungA  = %subst(vlrppt:28:2)
129200030710     C                   move(p)   wLungA        wLung
129300030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
129400030710     C                   seton                                        60
129500030710     C                   else
129600050914     C                   if        vlrisv = 'UL'
129700050914     C                   movel     datcor        wDataOraAlfa
129800050914     C                   move      oracor        wDataOraAlfa
129900050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
130000050914     C                   seton                                        60
130100050914     C                   else
130200050914     C                   setoff                                       60
130300050914     C                   endif
130400030710     C                   endif
130500010606     C*
130600010606     C* CALCOLA LA DATA CORRENTE
130700010606     C                   time                    wn14             14 0
130800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
130900040402     C                   movel     wn14          oracor            6 0          *ORA
131000010606     C                   z-add     wn8           g08dat
131100010606     C                   z-add     *zeros        g08inv
131200010606     C                   movel     '0'           g08err
131300010606     C                   call      'XSRDA8'
131400010606     C                   parm                    wlbda8
131500010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
131600000613     C*
131700000613     C                   ENDSR
