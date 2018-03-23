000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200130116     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
005400050118     D vhbatb                  1      1
005500050118     D vhbccm                  2      8
005600050118     D vhblnp                  9     11
005700050118     D vhbaas                 12     15
005800050118     D vhbmgs                 16     19
005900050118     D vhbnrs                 20     21
006000050118     D vhbnsp                 22     28
006100050118     D vhbcbo                 29     30
006200050118     D vhblna                 31     33
006300050118     D vhbrsd                 34     68
006400050118     D vhbrd2                 69    103
006500050118     D vhbind                104    138
006600050118     D vhbcad                139    147
006700050118     D vhblod                148    182
006800050118     D vhbprd                183    184
006900050118     D vhbnzd                185    187
007000050118     D vhbgc1                188    189
007100050118     D vhbgc2                190    191
007200050118     D vhbctr                192    194
007300050118     D vhbtsp                195    195
007400050118     D vhbias                196    208
007500050118     D vhbvas                209    211
007600050118     D vhbnas                212    226
007700050118     D vhbncl                227    231
007800050118     D vhbpkb                232    238
007900050118     D vhbvlb                239    243
008000050118     D vhbqft                244    256
008100050118     D vhbcas                257    269
008200050118     D vhbtic                270    271
008300050118     D vhbvca                272    274
008400050118     D vhbgca                275    276
008500050118     D vhbrmn                277    291
008600050118     D vhbrma                292    306
008700050118     D vhbncd                307    313
008800050118     D vhbnca                314    320
008900050118     D vhbxco                321    321
009000050118     D vhbnot                322    356
009100050118     D vhbnt2                357    391
009200050118     D vhbznc                392    393
009300050118     D vhbctm                394    395
009400050118     D vhbffd                396    396
009500050118     D vhbdcr                397    404
009600050118     D vhbtcr                405    405
009700050118     D vhbhcr                406    409
009800050118     D vhbcts                410    411
009900050118     D vhbftm                412    412
010000050118     D vhbvmd                413    425
010100050118     D vhbvad                426    428
010200050118     D vhbgma                429    430
010300050118     D vhbgga                431    432
010400050118     D vhbgva                433    434
010500050118     D vhbtc1                435    435
010600050118     D vhbtc2                436    436
010700050118     D vhbscl                437    437
010800050118     D vhbant                438    446
010900050118     D vhbrmo                447    471
011000050118     D vhbcmo                472    480
011100050118     D vhbnmo                481    483
011200050118     D  X_rec                  1    483    inz
011300010606     D*
011400071018     D*-------------------
011500071018     D* COSTANTI
011600071018     D*-------------------
011700071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
011800071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
011900990908
012000000913     C                   reset                   rrnum
012100990921     C                   reset                   esito
012200990921     C                   reset                   wrkesito
012300010530     C*
012400010530     C                   exsr      opeini
012500010530     C                   exsr      rwvab
012600041025     C*
012700041025     C* Esegue lancio TISI95R solo x chiusura
012800041025     C                   CLEAR                   TISI95DS
012900041025     C                   EVAL      I95TLA = 'C'
013000041025     C                   CALL      'TISI95R'
013100041025     C                   PARM                    TISI95DS
013200010530     C*
013300010530     C                   seton                                        lr
013400010530
013500010530
013600010530
013700010530
013800010530     C*--------------------------------------------------------
013900010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
014000010530     C*--------------------------------------------------------
014100010530     C     PREELA        BEGSR
014200000724     C*
014300000724     C* SE OCCORRE SPEDIRE IN FILIALE
014400010530     C                   if        invfil <> *zeros and
014500010530     C                             flgGiro = '0'
014600010530     C*
014700010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
014800010530     C                   eval      flgGiro = '1'
014900010530     C*
015000010530     C                   endif
015100010530     C*
015200010530     C                   ENDSR
015300010530     C***
015400010530
015500010530
015600010530
015700010530
015800010530
015900010530
016000010530     C*--------------------------------------------------------
016100010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016200010530     C*--------------------------------------------------------
016300010530     C     ENDELA        BEGSR
016400010530     C*
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
023400010530     C*
023500010530     C                   TESTN                   VHBLNP               30
023600010530     C  N30              TESTN                   VHBLNP                 30
023700010530     C  N30              TESTN                   VHBLNP                   30
023800010530     C                   MOVE      VHBLNP        W001A             1
023900010530     C     *IN30         IFEQ      *OFF
024000010530     C     W001A         ORLT      '0'
024100010530     C                   SETON                                        31
024200010530     C                   eval      vinmsg = %trimr(vinmsg)
024300010530     C                             + ' ' + 'VHBLNP'
024400010530     C                   ELSE
024500010530     C                   MOVE      VHBLNP        VABLNP
024600010530     C     VABLNP        IFLE      *ZEROS
024700010530     C                   SETON                                        31
024800010530     C                   eval      vinmsg = %trimr(vinmsg)
024900010530     C                             + ' ' + 'VHBLNP'
025000010530     C                   ENDIF
025100010530     C                   ENDIF
025200010530     C*
025300010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025400010530     C                   eval      depfil = VABLNP
025500010530     C                   exsr      repfil
025600010601     C                   if        depfil = invfil
025700021014     C                   if        vlrpoi = 999
025800021010     C                   MOVE(P)   invfil        VABFGS
025900021014     C                   else
026000021014     C                   MOVE(P)   vlrpoi        VABFGS
026100021014     C                   endif
026200010530     C*
026300010530     C                   exsr      PREELA
026400010530     C*
026500971023     C                   TESTN                   VHBCCM               30
026600971023     C  N30              TESTN                   VHBCCM                 30
026700971023     C  N30              TESTN                   VHBCCM                   30
026800971021     C                   MOVE      VHBCCM        W001A             1
026900971021     C     *IN30         IFEQ      *OFF
027000971021     C     W001A         ORLT      '0'
027100971021     C                   SETON                                        31
027200010604     C                   eval      vinmsg = %trimr(vinmsg)
027300010604     C                             + ' ' + 'VHBCCM'
027400971021     C                   ELSE
027500971021     C                   MOVE      VHBCCM        VABCCM
027600971024     C     VABCCM        IFLE      *ZEROS
027700971024     C                   SETON                                        31
027800010604     C                   eval      vinmsg = %trimr(vinmsg)
027900010604     C                             + ' ' + 'VHBCCM'
028000971024     C                   ENDIF
028100971021     C                   ENDIF
028200971021     C*
028300971023     C                   TESTN                   VHBAAS               30
028400971023     C  N30              TESTN                   VHBAAS                 30
028500971023     C  N30              TESTN                   VHBAAS                   30
028600971021     C                   MOVE      VHBAAS        W001A             1
028700971021     C     *IN30         IFEQ      *OFF
028800971021     C     W001A         ORLT      '0'
028900971023     C     W001A         ANDNE     *BLANK
029000971021     C                   SETON                                        32
029100010606     C                   MOVEL     datcor        VABAAS
029200010604     C                   eval      vinmsg = %trimr(vinmsg)
029300010604     C                             + ' ' + 'VHBAAS'
029400971021     C                   ELSE
029500971021     C                   MOVE      VHBAAS        VABAAS
029600971021     C                   ENDIF
029700971021     C*
029800971023     C                   TESTN                   VHBMGS               30
029900971023     C  N30              TESTN                   VHBMGS                 30
030000971023     C  N30              TESTN                   VHBMGS                   30
030100971021     C                   MOVE      VHBMGS        W001A             1
030200971021     C     *IN30         IFEQ      *OFF
030300971021     C     W001A         ORLT      '0'
030400971023     C     W001A         ANDNE     *BLANK
030500971021     C                   SETON                                        32
030600010601     C                   MOVE      '0101'        VABMGS
030700010604     C                   eval      vinmsg = %trimr(vinmsg)
030800010604     C                             + ' ' + 'VHBMGS'
030900971021     C                   ELSE
031000971021     C                   MOVE      VHBMGS        VABMGS
031100971021     C                   ENDIF
031200971021     C*
031300971023     C                   TESTN                   VHBNRS               30
031400971023     C  N30              TESTN                   VHBNRS                 30
031500971023     C  N30              TESTN                   VHBNRS                   30
031600971021     C                   MOVE      VHBNRS        W001A             1
031700971021     C     *IN30         IFEQ      *OFF
031800971021     C     W001A         ORLT      '0'
031900971023     C     W001A         ANDNE     *BLANK
032000971021     C                   SETON                                        32
032100971021     C                   MOVE      *ZERO         VABNRS
032200010604     C                   eval      vinmsg = %trimr(vinmsg)
032300010604     C                             + ' ' + 'VHBNRS'
032400971021     C                   ELSE
032500971021     C                   MOVE      VHBNRS        VABNRS
032600971021     C                   ENDIF
032700971021     C*
032800971023     C                   TESTN                   VHBNSP               30
032900971023     C  N30              TESTN                   VHBNSP                 30
033000971023     C  N30              TESTN                   VHBNSP                   30
033100971021     C                   MOVE      VHBNSP        W001A             1
033200971021     C     *IN30         IFEQ      *OFF
033300971021     C     W001A         ORLT      '0'
033400971023     C     W001A         ANDNE     *BLANK
033500971021     C                   SETON                                        32
033600971021     C                   MOVE      *ZERO         VABNSP
033700010604     C                   eval      vinmsg = %trimr(vinmsg)
033800010604     C                             + ' ' + 'VHBNSP'
033900971021     C                   ELSE
034000971021     C                   MOVE      VHBNSP        VABNSP
034100971021     C                   ENDIF
034200971021     C*
034300971023     C                   TESTN                   VHBLNA               30
034400971023     C  N30              TESTN                   VHBLNA                 30
034500971023     C  N30              TESTN                   VHBLNA                   30
034600971021     C                   MOVE      VHBLNA        W001A             1
034700971021     C     *IN30         IFEQ      *OFF
034800971021     C     W001A         ORLT      '0'
034900971023     C     W001A         ANDNE     *BLANK
035000971021     C                   SETON                                        32
035100971021     C                   MOVE      *ZERO         VABLNA
035200010604     C                   eval      vinmsg = %trimr(vinmsg)
035300010604     C                             + ' ' + 'VHBLNA'
035400971021     C                   ELSE
035500971021     C                   MOVE      VHBLNA        VABLNA
035600971021     C                   ENDIF
035700971021     C*
035800971023     C                   TESTN                   VHBCTR               30
035900971023     C  N30              TESTN                   VHBCTR                 30
036000971023     C  N30              TESTN                   VHBCTR                   30
036100971021     C                   MOVE      VHBCTR        W001A             1
036200971021     C     *IN30         IFEQ      *OFF
036300971021     C     W001A         ORLT      '0'
036400971023     C     W001A         ANDNE     *BLANK
036500971021     C                   SETON                                        32
036600971021     C                   MOVE      *ZERO         VABCTR
036700010604     C                   eval      vinmsg = %trimr(vinmsg)
036800010604     C                             + ' ' + 'VHBCTR'
036900971021     C                   ELSE
037000971021     C                   MOVE      VHBCTR        VABCTR
037100971021     C                   ENDIF
037200941229     C*
037300971021     C                   MOVE      *ZERO         VABIAS
037400941229     C                   MOVE      VHBIAS        WASC15
037500971023     C                   TESTN                   WI15                 30
037600971023     C  N30              TESTN                   WI15                   30
037700971023     C  N30              TESTN                   WI15                     30
037800971021     C                   MOVE      WI15          W001A             1
037900971021     C     *IN30         IFEQ      *OFF
038000971021     C     W001A         ORLT      '0'
038100971023     C     W001A         ANDNE     *BLANK
038200971021     C                   SETON                                        32
038300971021     C                   MOVE      *ZERO         VABIAS
038400010604     C                   eval      vinmsg = %trimr(vinmsg)
038500010604     C                             + ' ' + 'VHBIAS'
038600971021     C                   ELSE
038700971021     C                   MOVEL     WI15          VABIAS
038800971023     C                   TESTN                   WD15                 30
038900971023     C  N30              TESTN                   WD15                   30
039000971023     C  N30              TESTN                   WD15                     30
039100971021     C                   MOVE      WD15          W001A             1
039200971021     C     *IN30         IFEQ      *OFF
039300971021     C     W001A         ORLT      '0'
039400971023     C     W001A         ANDNE     *BLANK
039500971021     C                   SETON                                        32
039600971021     C                   MOVE      *ZERO         VABIAS
039700010604     C                   eval      vinmsg = %trimr(vinmsg)
039800010604     C                             + ' ' + 'VHBIAS'
039900971021     C                   ELSE
040000971021     C                   MOVE      WD15          VABIAS
040100971021     C                   ENDIF
040200050118     C                   ENDIF
040300941229     C*
040400971023     C                   TESTN                   VHBNCL               30
040500971023     C  N30              TESTN                   VHBNCL                 30
040600971023     C  N30              TESTN                   VHBNCL                   30
040700971021     C                   MOVE      VHBNCL        W001A             1
040800971021     C     *IN30         IFEQ      *OFF
040900971021     C     W001A         ORLT      '0'
041000971021     C                   SETON                                        32
041100971021     C                   MOVE      *ZERO         VABNCL
041200010604     C                   eval      vinmsg = %trimr(vinmsg)
041300010604     C                             + ' ' + 'VHBNCL'
041400971021     C                   ELSE
041500971021     C                   MOVE      VHBNCL        VABNCL
041600971021     C                   ENDIF
041700941229     C*
041800971021     C                   MOVE      *ZERO         VABPKB
041900971021     C                   MOVE      VHBPKB        WASC09
042000010719     C                   IF        WI09 = *blanks
042100010719     C                   EVAL      WI09 = *zeros
042200010719     C                   ENDIF
042300971023     C                   TESTN                   WI09                 30
042400971023     C  N30              TESTN                   WI09                   30
042500971023     C  N30              TESTN                   WI09                     30
042600971021     C                   MOVE      WI09          W001A             1
042700971021     C     *IN30         IFEQ      *OFF
042800971021     C     W001A         ORLT      '0'
042900971021     C                   SETON                                        32
043000971021     C                   MOVE      *ZERO         VABPKB
043100010604     C                   eval      vinmsg = %trimr(vinmsg)
043200010604     C                             + ' ' + 'VHBPKB'
043300971021     C                   ELSE
043400971021     C                   MOVEL     WI09          VABPKB
043500010719     C                   IF        WD09 = *blanks
043600010719     C                   EVAL      WD09 = *zeros
043700010719     C                   ENDIF
043800971023     C                   TESTN                   WD09                 30
043900971023     C  N30              TESTN                   WD09                     30
044000971021     C                   MOVE      WD09          W001A             1
044100971021     C     *IN30         IFEQ      *OFF
044200971021     C     W001A         ORLT      '0'
044300971023     C     W001A         ANDNE     *BLANK
044400971021     C                   SETON                                        32
044500971021     C                   MOVE      *ZERO         VABPKB
044600010604     C                   eval      vinmsg = %trimr(vinmsg)
044700010604     C                             + ' ' + 'VHBPKB'
044800971021     C                   ELSE
044900971021     C                   MOVE      WD09          VABPKB
045000971021     C                   ENDIF
045100050118     C                   ENDIF
045200971021     C*
045300041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
045400041001     C                   IF        *IN51 = *OFF
045500971021     C                   MOVE      *ZERO         VABVLB
045600971021     C                   MOVE      VHBVLB        WASC07
045700010719     C                   IF        WI07 = *blanks
045800010719     C                   EVAL      WI07 = *zeros
045900010719     C                   ENDIF
046000971023     C                   TESTN                   WI07                 30
046100971023     C  N30              TESTN                   WI07                   30
046200971023     C  N30              TESTN                   WI07                     30
046300971021     C                   MOVE      WI07          W001A             1
046400971021     C     *IN30         IFEQ      *OFF
046500971021     C     W001A         ORLT      '0'
046600971023     C     W001A         ANDNE     *BLANK
046700971021     C                   SETON                                        32
046800971021     C                   MOVE      *ZERO         VABVLB
046900010604     C                   eval      vinmsg = %trimr(vinmsg)
047000010604     C                             + ' ' + 'VHBVLB'
047100971021     C                   ELSE
047200971021     C                   MOVEL     WI07          VABVLB
047300010719     C                   IF        WD07 = *blanks
047400010719     C                   EVAL      WD07 = *zeros
047500010719     C                   ENDIF
047600971023     C                   TESTN                   WD07                 30
047700971023     C  N30              TESTN                   WD07                   30
047800971023     C  N30              TESTN                   WD07                     30
047900971021     C                   MOVE      WD07          W001A             1
048000971021     C     *IN30         IFEQ      *OFF
048100971021     C     W001A         ORLT      '0'
048200971023     C     W001A         ANDNE     *BLANK
048300971021     C                   SETON                                        32
048400971021     C                   MOVE      *ZERO         VABVLB
048500010604     C                   eval      vinmsg = %trimr(vinmsg)
048600010604     C                             + ' ' + 'VHBVLB'
048700971021     C                   ELSE
048800971021     C                   MOVE      WD07          VABVLB
048900971021     C                   ENDIF
049000050118     C                   ENDIF
049100041001     C                   ELSE
049200041001     C* ...altrimenti forzo azzeramento del volume
049300041001     C                   Z-ADD     *zeros        VABVLB
049400041001     C                   ENDIF
049500941229     C*
049600971021     C                   MOVE      *ZERO         VABQFT
049700971021     C                   MOVE      VHBQFT        WASC15
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
050900971021     C                   MOVE      *ZERO         VABQFT
051000010604     C                   eval      vinmsg = %trimr(vinmsg)
051100010604     C                             + ' ' + 'VHBQFT'
051200971021     C                   ELSE
051300971021     C                   MOVEL     WI15          VABQFT
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
052500971021     C                   MOVE      *ZERO         VABQFT
052600010604     C                   eval      vinmsg = %trimr(vinmsg)
052700010604     C                             + ' ' + 'VHBQFT'
052800971021     C                   ELSE
052900971021     C                   MOVE      WD15          VABQFT
053000971021     C                   ENDIF
053100050118     C                   ENDIF
053200941229     C*
053300971021     C                   MOVE      *ZERO         VABCAS
053400971021     C                   MOVE      VHBCAS        WASC15
053500010719     C                   IF        WI15 = *blanks
053600010719     C                   EVAL      WI15 = *zeros
053700010719     C                   ENDIF
053800971023     C                   TESTN                   WI15                 30
053900971023     C  N30              TESTN                   WI15                   30
054000971023     C  N30              TESTN                   WI15                     30
054100971021     C                   MOVE      WI15          W001A             1
054200971021     C     *IN30         IFEQ      *OFF
054300971021     C     W001A         ORLT      '0'
054400971023     C     W001A         ANDNE     *BLANK
054500971021     C                   SETON                                        32
054600971021     C                   MOVE      *ZERO         VABCAS
054700010604     C                   eval      vinmsg = %trimr(vinmsg)
054800010604     C                             + ' ' + 'VHBCAS'
054900971021     C                   ELSE
055000971021     C                   MOVEL     WI15          VABCAS
055100010719     C                   IF        WD15 = *blanks
055200010719     C                   EVAL      WD15 = *zeros
055300010719     C                   ENDIF
055400971023     C                   TESTN                   WD15                 30
055500971023     C  N30              TESTN                   WD15                   30
055600971023     C  N30              TESTN                   WD15                     30
055700971021     C                   MOVE      WD15          W001A             1
055800971021     C     *IN30         IFEQ      *OFF
055900971021     C     W001A         ORLT      '0'
056000971023     C     W001A         ANDNE     *BLANK
056100971021     C                   SETON                                        32
056200971021     C                   MOVE      *ZERO         VABCAS
056300010604     C                   eval      vinmsg = %trimr(vinmsg)
056400010604     C                             + ' ' + 'VHBCAS'
056500971021     C                   ELSE
056600971021     C                   MOVE      WD15          VABCAS
056700971021     C                   ENDIF
056800050118     C                   ENDIF
056900971021     C*
057000971023     C                   TESTN                   VHBRMN               30
057100971023     C  N30              TESTN                   VHBRMN                 30
057200971023     C  N30              TESTN                   VHBRMN                   30
057300971021     C                   MOVE      VHBRMN        W001A             1
057400971021     C     *IN30         IFEQ      *OFF
057500971021     C     W001A         ORLT      '0'
057600971021     C                   SETON                                        32
057700971021     C                   MOVE      *ZERO         VABRMN
057800010604     C                   eval      vinmsg = %trimr(vinmsg)
057900010604     C                             + ' ' + 'VHBRMN'
058000971021     C                   ELSE
058100971021     C                   MOVE      VHBRMN        VABRMN
058200971021     C                   ENDIF
058300971021     C*
058400971023     C                   TESTN                   VHBNCD               30
058500971023     C  N30              TESTN                   VHBNCD                 30
058600971023     C  N30              TESTN                   VHBNCD                   30
058700971021     C                   MOVE      VHBNCD        W001A             1
058800971021     C     *IN30         IFEQ      *OFF
058900971021     C     W001A         ORLT      '0'
059000971023     C     W001A         ANDNE     *BLANK
059100971021     C                   SETON                                        32
059200971021     C                   MOVE      *ZERO         VABNCD
059300010604     C                   eval      vinmsg = %trimr(vinmsg)
059400010604     C                             + ' ' + 'VHBNCD'
059500971021     C                   ELSE
059600971021     C                   MOVE      VHBNCD        VABNCD
059700971021     C                   ENDIF
059800971021     C*
059900971023     C                   TESTN                   VHBNCA               30
060000971023     C  N30              TESTN                   VHBNCA                 30
060100971023     C  N30              TESTN                   VHBNCA                   30
060200971021     C                   MOVE      VHBNCA        W001A             1
060300971021     C     *IN30         IFEQ      *OFF
060400971021     C     W001A         ORLT      '0'
060500971023     C     W001A         ANDNE     *BLANK
060600971021     C                   SETON                                        32
060700971021     C                   MOVE      *ZERO         VABNCA
060800010604     C                   eval      vinmsg = %trimr(vinmsg)
060900010604     C                             + ' ' + 'VHBNCA'
061000971021     C                   ELSE
061100971021     C                   MOVE      VHBNCA        VABNCA
061200971021     C                   ENDIF
061300971021     C*
061400971023     C                   TESTN                   VHBZNC               30
061500971023     C  N30              TESTN                   VHBZNC                 30
061600971023     C  N30              TESTN                   VHBZNC                   30
061700971021     C                   MOVE      VHBZNC        W001A             1
061800971021     C     *IN30         IFEQ      *OFF
061900971021     C     W001A         ORLT      '0'
062000971023     C     W001A         ANDNE     *BLANK
062100971021     C                   SETON                                        32
062200971021     C                   MOVE      *ZERO         VABZNC
062300010604     C                   eval      vinmsg = %trimr(vinmsg)
062400010604     C                             + ' ' + 'VHBZNC'
062500971021     C                   ELSE
062600971021     C                   MOVE      VHBZNC        VABZNC
062700971021     C                   ENDIF
062800971021     C*
062900971023     C                   TESTN                   VHBDCR               30
063000971023     C  N30              TESTN                   VHBDCR                 30
063100971023     C  N30              TESTN                   VHBDCR                   30
063200971021     C                   MOVE      VHBDCR        W001A             1
063300971021     C     *IN30         IFEQ      *OFF
063400971021     C     W001A         ORLT      '0'
063500971023     C     W001A         ANDNE     *BLANK
063600971021     C                   SETON                                        32
063700971021     C                   MOVE      *ZERO         VABDCR
063800010604     C                   eval      vinmsg = %trimr(vinmsg)
063900010604     C                             + ' ' + 'VHBDCR'
064000971021     C                   ELSE
064100971021     C                   MOVE      VHBDCR        VABDCR
064200971021     C                   ENDIF
064300971021     C*
064400971023     C                   TESTN                   VHBHCR               30
064500971023     C  N30              TESTN                   VHBHCR                 30
064600971023     C  N30              TESTN                   VHBHCR                   30
064700971021     C                   MOVE      VHBHCR        W001A             1
064800971021     C     *IN30         IFEQ      *OFF
064900971021     C     W001A         ORLT      '0'
065000971023     C     W001A         ANDNE     *BLANK
065100971021     C                   SETON                                        32
065200971021     C                   MOVE      *ZERO         VABHCR
065300010604     C                   eval      vinmsg = %trimr(vinmsg)
065400010604     C                             + ' ' + 'VHBHCR'
065500971021     C                   ELSE
065600971021     C                   MOVE      VHBHCR        VABHCR
065700971021     C                   ENDIF
065800941229     C*
065900971021     C                   MOVE      *ZERO         VABVMD
066000971021     C                   MOVE      VHBVMD        WASC15
066100010719     C                   IF        WI15 = *blanks
066200010719     C                   EVAL      WI15 = *zeros
066300010719     C                   ENDIF
066400971023     C                   TESTN                   WI15                 30
066500971023     C  N30              TESTN                   WI15                   30
066600971023     C  N30              TESTN                   WI15                     30
066700971021     C                   MOVE      WI15          W001A             1
066800971021     C     *IN30         IFEQ      *OFF
066900971021     C     W001A         ORLT      '0'
067000971023     C     W001A         ANDNE     *BLANK
067100971021     C                   SETON                                        32
067200971021     C                   MOVE      *ZERO         VABVMD
067300010604     C                   eval      vinmsg = %trimr(vinmsg)
067400010604     C                             + ' ' + 'VHBVMD'
067500971021     C                   ELSE
067600971021     C                   MOVEL     WI15          VABVMD
067700010719     C                   IF        WD15 = *blanks
067800010719     C                   EVAL      WD15 = *zeros
067900010719     C                   ENDIF
068000971023     C                   TESTN                   WD15                 30
068100971023     C  N30              TESTN                   WD15                   30
068200971023     C  N30              TESTN                   WD15                     30
068300971021     C                   MOVE      WD15          W001A             1
068400971021     C     *IN30         IFEQ      *OFF
068500971021     C     W001A         ORLT      '0'
068600971023     C     W001A         ANDNE     *BLANK
068700971021     C                   SETON                                        32
068800971021     C                   MOVE      *ZERO         VABVMD
068900010604     C                   eval      vinmsg = %trimr(vinmsg)
069000010604     C                             + ' ' + 'VHBVMD'
069100971021     C                   ELSE
069200971021     C                   MOVE      WD15          VABVMD
069300971021     C                   ENDIF
069400050118     C                   ENDIF
069500971021     C*
069600971023     C                   TESTN                   VHBANT               30
069700971023     C  N30              TESTN                   VHBANT                 30
069800971023     C  N30              TESTN                   VHBANT                   30
069900971021     C                   MOVE      VHBANT        W001A             1
070000971021     C     *IN30         IFEQ      *OFF
070100971021     C     W001A         ORLT      '0'
070200971023     C     W001A         ANDNE     *BLANK
070300971021     C                   SETON                                        32
070400971021     C                   MOVE      *ZERO         VABANT
070500010604     C                   eval      vinmsg = %trimr(vinmsg)
070600010604     C                             + ' ' + 'VHBANT'
070700971021     C                   ELSE
070800971021     C                   MOVE      VHBANT        VABANT
070900971021     C                   ENDIF
071000971021     C*
071100971021     C* CAMPI ALFABETICI:
071200971021     C                   MOVE      VHBATB        VABATB
071300971021     C                   MOVE      VHBCBO        VABCBO
071400971021     C                   MOVE      VHBRSD        VABRSD
071500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
071600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
071700020117     C* ==
071800971021     C                   MOVE      VHBRD2        VABRD2
071900971021     C                   MOVE      VHBIND        VABIND
072000971021     C                   MOVE      VHBCAD        VABCAD
072100971021     C                   MOVE      VHBLOD        VABLOD
072200971021     C                   MOVE      VHBPRD        VABPRD
072300971021     C                   MOVE      VHBNZD        VABNZD
072400971021     C                   MOVE      VHBGC1        VABGC1
072500971021     C                   MOVE      VHBGC2        VABGC2
072600971021     C                   MOVE      VHBTSP        VABTSP
072700971021     C                   MOVE      VHBVAS        VABVAS
072800971021     C                   MOVE      VHBNAS        VABNAS
072900971021     C                   MOVE      VHBTIC        VABTIC
073000971021     C                   MOVE      VHBVCA        VABVCA
073100971021     C                   MOVE      VHBGCA        VABGCA
073200971021     C                   MOVE      VHBRMA        VABRMA
073300971021     C                   MOVE      VHBXCO        VABXCO
073400971021     C                   MOVE      VHBNOT        VABNOT
073500971021     C                   MOVE      VHBNT2        VABNT2
073600971021     C                   MOVE      VHBCTM        VABCTM
073700971021     C                   MOVE      VHBFFD        VABFFD
073800971021     C                   MOVE      VHBTCR        VABTCR
073900971021     C                   MOVE      VHBCTS        VABCTS
074000971021     C                   MOVE      VHBFTM        VABFTM
074100971021     C                   MOVE      VHBVAD        VABVAD
074200971021     C                   MOVE      VHBGMA        VABGMA
074300971021     C                   MOVE      VHBGGA        VABGGA
074400971021     C                   MOVE      VHBGVA        VABGVA
074500971021     C                   MOVE      VHBTC1        VABTC1
074600971021     C                   MOVE      VHBTC2        VABTC2
074700971021     C                   MOVE      VHBSCL        VABSCL
074800971021     C                   MOVE      VHBRMO        VABRMO
074900971021     C                   MOVE      VHBCMO        VABCMO
075000971021     C                   MOVE      VHBNMO        VABNMO
075100971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
075200971021     C* NELLA NAZIONE ORIGINALE
075300971021     C                   MOVE      VHBNMO        WA1D              1
075400971021     C                   MOVEL     VHBNMO        WA1S              1
075500971021     C     WA1D          IFNE      *BLANK
075600971021     C     WA1S          ANDEQ     *BLANK
075700971021     C                   MOVE      *BLANK        VABNMO
075800030616     C                   ENDIF
075900030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
076000030710     C                   EVAL      VABCMR = wCMR
076100030616     C                   EVAL      VABDCM = DATCOR
076200030616     C                   EVAL      VABDTS = DATCOR
076300040402     C                   EVAL      VABHMS = ORACOR
076400030616     C                   EVAL      VABCNT = 1
076500041025     C*
076600041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
076700041025     C                   IF        *IN52 = *ON
076800041025     C* Reperisco la provincia dal CAP e dalla località
076900041025     C                   IF        VABCAD <> *blanks AND
077000050907     C                             VABPRD  = *blanks AND
077100050907     C                             VABNZD  = *blanks
077200041025     C                   CLEAR                   TISI95DS
077300041025     C                   EVAL      I95TCN = '3'
077400041025     C                   Z-ADD     datcor        I95DAT
077500041025     C                   EVAL      I95CAP = VABCAD
077600041025     C                   EVAL      I95LOC = VABLOD
077700041025     C                   CALL      'TISI95R'
077800041025     C                   PARM                    TISI95DS
077900041025     C                   EVAL      VABPRD = O95PRV
078000041025     C                   ENDIF
078100041025     C                   ENDIF
078200080605     C*
078300080605     C* Forzatura x personalizzazione CEVA LOGISTIC - TIM
078400120223     C                   if        *in54 = *on AND VABCCM <> 0635734 AND
078500121203     C                                             VABCCM <> 0635526 AND
078600130308     C                                             VABCCM <> 0636217 AND
078700130308     C                                             VABCCM <> 0636324 AND
078800130308     C                                             VABCCM <> 0636325 AND
078900130308     C                                             VABCCM <> 0636326 AND
079000130308     C                                             VABCCM <> 0636327 AND
079100130308     C                                             VABCCM <> 0636328 AND
079200130308     C                                             VABCCM <> 0636348
079300080606     C                   eval      VABNOT = %subst(%editc(VABRMN:'X'):
079400080609     C                                      %len(VABRMN)-10+1)
079500080605     C                   eval      VABNT2 = *blanks
079600100519     C                   endif
079700930409     C*
079800010604     C  N31              ADD       1             §CTROK            7 0
079900010604     C   32              ADD       1             §CTRMO            7 0
080000010604     C   31              ADD       1             §CTRNO            7 0
080100030616     C  N31              WRITE     EDIVAB00
080200990910     C*
080300010601     C                   if        *in31 = *off and
080400010601     C                             *in32 = *off
080500990910     C                   eval      vinflg = '1'
080600990910     C                   else
080700990910     C                   eval      vinflg = '2'
080800990910     C                   endif
080900991022     C                   endif
081000990910     C                   endif
081100010601     C*
081200010601     C                   endif
081300000905     C*
081400000905     C                   else
081500000905     C                   eval      vinflg = '1'
081600000905     C                   endif
081700000905     C*
081800000905     C  N70              update    tivin000
081900991022     C*
082000991022     C  N70              ENDdo
082100010530     C
082200010530     C*
082300010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
082400010530     C                   if        cntNonEl = *zeros or
082500010530     C                             flgMulti = '0'
082600990910     C* Se non ci sono record con errori ...
082700010530     C                   if        §ctrno = 0 and
082800010601     C                             §ctrmo = 0 and
082900010530     C                             flgStato <> '2'
083000990910     C* ... restituisco esito OK.
083100990921     C                   eval      wrkesito = '0'
083200990910     C                   else
083300010601     C                   if        §ctrok > 0
083400990921     C                   eval      wrkesito = '1'
083500000710     C                   else
083600010615     C                   if        flgOk = '0'
083700010615     C                   eval      wrkesito = '2'
083800010615     C                   else
083900010615     C                   eval      wrkesito = '6'
084000010615     C                   endif
084100990910     C                   endif
084200000710     C                   endif
084300010530     C                   else
084400010530     C                   eval      wrkesito = '9'
084500010530     C                   endif
084600030710     C*
084700030710 X  1C                   else
084800030710     C                   eval      wrkesito = '8'
084900030710     C                   eval      flgGiro = '1'
085000030710 E  1C                   endif
085100030710     C*
085200990914     C                   if        %open(tivin00r)
085300990908     C                   close     tivin00r
085400990914     C                   endif
085500030616     C                   if        %open(edivabwr)
085600030616     C                   close     edivabwr
085700990914     C                   endif
085800010530     C*
085900010530     C                   if        vlrpoi <> 999
086000010530     C                   eval      invfil = vlrpoi
086100010530     C                   endif
086200990910     C*
086300990920     C                   if        §ctrok > 0
086400010530     C                             and invfil > *zeros
086500000613     C                   exsr      invio
086600990920     C                   endif
086700010612     C*
086800010612     C                   if        flgGiro = '1'
086900010612     C                   exsr      endela
087000010612     C                   endif
087100990920     C*
087200910830     C                   ENDSR
087300000613     C***
087400990920
087500010530
087600010530
087700010530
087800010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
087900010530     C     repfil        BEGSR
088000010530     C*
088100010530     C                   if        invfil = *zeros and
088200010530     C                             depfil > *zeros and
088300010613     C                             (vinflg = *blanks or
088400010613     C                              vinflg = *zeros)
088500010530     C
088600010530     C                   eval      invfil = depfil
088700010530     C                   endif
088800010530     C*
088900010530     C                   if        depfil <> invfil and
089000010530     C                             invfil > *zeros
089100010530     C                   eval      flgMulti = '1'
089200010530     C                   if        vinflg = *blanks
089300010530     C                   add       1             cntNonEl
089400010530     C                   endif
089500010530     C                   endif
089600010530     C*
089700010530     C                   if        vinflg = '2'
089800010530     C                   eval      flgStato = '2'
089900010530     C                   endif
090000010530     C*
090100010530     C                   ENDSR
090200010530     C***
090300010530
090400010530
090500010530
090600010530
090700990920      /TITLE Invio dei dati al punto operativo.
090800000613     C     invio         BEGSR
090900990920     C*
091000990920     C                   reset                   dscmz
091100030618     C                   move      invfil        cmzdst
091200030618     C***                move      '789'         cmzdst
091300990920     C                   eval      cmzfld = vlrfou
091400990920     C                   eval      cmzmbd = vlrhdl
091500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
091600000710     C                   if        prmfir = *blanks
091700030616     C                   eval      cmzfla = 'EDIVAB0F'
091800030616     C                   eval      cmzmba = 'EDIVAB0F'
091900000710     C                   else
092000000710     C                   eval      cmzfla = prmfir
092100000710     C                   eval      cmzmba = prmfir
092200000710     C                   endif
092300990920     C                   eval      cmznrr = *zeros
092400990920     C                   move      §ctrok        cmznrr
092500021003     C                   eval      cmzlba = vlrfl1
092600990920     C                   call(e)   'TIS711C'
092700990920     C                   parm                    dscmz
092800990921     C                   parm      *blanks       esito
092900990920     C                   if        %error
093000990920     C                             or cmzerr = '1'
093100990921     C                             or esito  = '1'
093200000710     C                   eval      wrkesito = '3'
093300990920     C                   endif
093400990920     C*
093500000613     C                   ENDSR
093600000613     C***
093700010530
093800010530
093900010530
094000010530
094100010530      /TITLE Invio dei dati al punto operativo.
094200010530     C     opeini        BEGSR
094300010530     C*
094400010530     C* Inizializzo flag e contatori operativi
094500010530     C                   movel     '0'           flgGiro           1
094600010530     C                   movel     '0'           flgMulti          1
094700010530     C                   movel     '1'           flgStato          1
094800010615     C                   movel     '0'           flgOk             1
094900010530     C                   z-add     *zeros        cntNonEl         10 0
095000010530     C                   z-add     *zeros        depfil            3 0
095100010530     C                   z-add     *zeros        invfil            3 0
095200010530     C*
095300010530     C                   ENDSR
095400010530     C***
095500010530
095600010530
095700010530
095800990910
095900000613     C     *inzsr        BEGSR
096000990910     C*
096100990910     C     *entry        plist
096200990920     C                   parm                    tivlrds
096300990921     C                   parm      wrkesito      esito
096400000724     C                   parm                    prmlit
096500000710     C                   parm                    prmfir
096600030616     C*
096700030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
096800030710     C                   move(p)   vlrMSG        wNomeFile
096900030710     C*
097000030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
097100030710     C* del codice CMR dal nome del file importato.
097200030710     C                   if        wNomeFile <> *blanks AND
097300030710     C                             vlrppt<>*blanks
097400041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
097500030710     C                   if        %subst(vlrppt:1:1) = 'S'
097600030710     C                   seton                                        50
097700030710     C                   else
097800030710     C                   setoff                                       50
097900030710     C                   endif
098000041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
098100041001     C                   if        %subst(vlrppt:6:1) = 'S'
098200041001     C                   seton                                        51
098300041001     C                   else
098400041001     C                   setoff                                       51
098500041001     C                   endif
098600041025     C*
098700100519     C* Verifico se sui parametri del traduttore è richiesto il rerimento provincia
098800041025     C* (naturalmente solo se nn già presente)
098900041025     C                   if        %subst(vlrppt:7:1) = 'S'
099000041025     C                   seton                                        52
099100041025     C                   else
099200041025     C                   setoff                                       52
099300041025     C                   endif
099400100519     C*
099500100519     C* Verifico se sui parametri del traduttore è richiesto il trattamento "particolare"
099600100519     C* delle note
099700100519     C                   if        %subst(vlrppt:8:1) = 'S'
099800100519     C                   seton                                        54
099900100519     C                   else
100000100519     C                   setoff                                       54
100100100519     C                   endif
100200041001     C*
100300030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
100400030710     C                   move(p)   wPosDaA       wPosDa
100500030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
100600030710     C                   move(p)   wLungA        wLung
100700030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
100800030710     C                   seton                                        60
100900030710     C                   else
101000050914     C                   if        vlrisv = 'UL'
101100050914     C                   movel     datcor        wDataOraAlfa
101200050914     C                   move      oracor        wDataOraAlfa
101300050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
101400050914     C                   seton                                        60
101500050914     C                   else
101600030710     C                   setoff                                       60
101700050914     C                   endif
101800030710     C                   endif
101900010606     C*
102000010606     C* CALCOLA LA DATA CORRENTE
102100010606     C                   time                    wn14             14 0
102200010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
102300040402     C                   movel     wn14          oracor            6 0          *ORA
102400010606     C                   z-add     wn8           g08dat
102500010606     C                   z-add     *zeros        g08inv
102600010606     C                   movel     '0'           g08err
102700010606     C                   call      'XSRDA8'
102800010606     C                   parm                    wlbda8
102900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
103000000613     C*
103100000613     C                   ENDSR
103200000613     C***
