000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209
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
016500010530     C                   ENDSR
016600010530     C***
016700010530
016800010530
016900910830     C*--------------------------------------------------------
017000030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
017100910830     C*--------------------------------------------------------
017200941229     C     RWVAB         BEGSR
017300030710     C*
017400030710     C                   clear                   §CTROK
017500030710     C                   clear                   §CTRMO
017600030710     C                   clear                   §CTRNO
017700990910     C*
017800030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
017900030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
018000030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
018100030710     C                   if        *in60 = *on
018200990914     C                   if        not %open(tivin00r)
018300990908     C                   open      tivin00r
018400990914     C                   endif
018500030616     C                   if        not %open(edivabwr)
018600030616     C                   open      edivabwr
018700990914     C                   endif
018800990910     C*
018900921023     C                   DO        *HIVAL
019000990913     C*
019100990915     C                   READ      tivin00r                               70
019200071018     C*
019300071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
019400071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
019500010618     C*
019600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019700010618     C                   if        vinflg = '1'
019800010618     C                   eval      flgOk = '1'
019900010618     C                   endif
020000010705     C*
020100010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
020200010705     C                   if        %subst(vindta:1:1) = '='
020300010705     C                   eval      %subst(vindta:1:1) = *blanks
020400010705     C                   endif
020500010705     C*
020600000905     C                   if        vindta > *blanks
020700000613     C                   add       1             rrnum
020800010601     C* ...elimino eventuali segni
020900010601     C     '+':' '       xlate     vindta        vindta
021000990913     C*
021100010601     C                   if        *in70 = *off and
021200990913     C                             (vinflg = *blanks
021300010711     C                              or vinflg = '0'
021400010711     C                              or vinflg = '2')
021500990913     C*
021600030616     C                   clear                   EDIVAB00
021700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021800010711     C                   if        vinflg = *blanks or vinflg = '0'
021900010711     C                   clear                   vinmsg
022000010711     C                   endif
022100010711     C*
022200050118     C                   eval      x_rec  = vindta
022300991022     C*
022400991022     C                   testb     x'3F'         vhbatb                   02
022500991022     C                   if        not *in02
022600991022     C*
022700971021     C                   SETOFF                                       303132
022800971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
022900971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
023000971023     C*    OPPURE TUTTI BLANKS
023100010530     C*
023200010530     C                   TESTN                   VHBLNP               30
023300010530     C  N30              TESTN                   VHBLNP                 30
023400010530     C  N30              TESTN                   VHBLNP                   30
023500010530     C                   MOVE      VHBLNP        W001A             1
023600010530     C     *IN30         IFEQ      *OFF
023700010530     C     W001A         ORLT      '0'
023800010530     C                   SETON                                        31
023900010530     C                   eval      vinmsg = %trimr(vinmsg)
024000010530     C                             + ' ' + 'VHBLNP'
024100010530     C                   ELSE
024200010530     C                   MOVE      VHBLNP        VABLNP
024300010530     C     VABLNP        IFLE      *ZEROS
024400010530     C                   SETON                                        31
024500010530     C                   eval      vinmsg = %trimr(vinmsg)
024600010530     C                             + ' ' + 'VHBLNP'
024700010530     C                   ENDIF
024800010530     C                   ENDIF
024900010530     C*
025000010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025100010530     C                   eval      depfil = VABLNP
025200010530     C                   exsr      repfil
025300010601     C                   if        depfil = invfil
025400021014     C                   if        vlrpoi = 999
025500021010     C                   MOVE(P)   invfil        VABFGS
025600021014     C                   else
025700021014     C                   MOVE(P)   vlrpoi        VABFGS
025800021014     C                   endif
025900010530     C*
026000010530     C                   exsr      PREELA
026100010530     C*
026200971023     C                   TESTN                   VHBCCM               30
026300971023     C  N30              TESTN                   VHBCCM                 30
026400971023     C  N30              TESTN                   VHBCCM                   30
026500971021     C                   MOVE      VHBCCM        W001A             1
026600971021     C     *IN30         IFEQ      *OFF
026700971021     C     W001A         ORLT      '0'
026800971021     C                   SETON                                        31
026900010604     C                   eval      vinmsg = %trimr(vinmsg)
027000010604     C                             + ' ' + 'VHBCCM'
027100971021     C                   ELSE
027200971021     C                   MOVE      VHBCCM        VABCCM
027300971024     C     VABCCM        IFLE      *ZEROS
027400971024     C                   SETON                                        31
027500010604     C                   eval      vinmsg = %trimr(vinmsg)
027600010604     C                             + ' ' + 'VHBCCM'
027700971024     C                   ENDIF
027800971021     C                   ENDIF
027900971021     C*
028000971023     C                   TESTN                   VHBAAS               30
028100971023     C  N30              TESTN                   VHBAAS                 30
028200971023     C  N30              TESTN                   VHBAAS                   30
028300971021     C                   MOVE      VHBAAS        W001A             1
028400971021     C     *IN30         IFEQ      *OFF
028500971021     C     W001A         ORLT      '0'
028600971023     C     W001A         ANDNE     *BLANK
028700971021     C                   SETON                                        32
028800010606     C                   MOVEL     datcor        VABAAS
028900010604     C                   eval      vinmsg = %trimr(vinmsg)
029000010604     C                             + ' ' + 'VHBAAS'
029100971021     C                   ELSE
029200971021     C                   MOVE      VHBAAS        VABAAS
029300971021     C                   ENDIF
029400971021     C*
029500971023     C                   TESTN                   VHBMGS               30
029600971023     C  N30              TESTN                   VHBMGS                 30
029700971023     C  N30              TESTN                   VHBMGS                   30
029800971021     C                   MOVE      VHBMGS        W001A             1
029900971021     C     *IN30         IFEQ      *OFF
030000971021     C     W001A         ORLT      '0'
030100971023     C     W001A         ANDNE     *BLANK
030200971021     C                   SETON                                        32
030300010601     C                   MOVE      '0101'        VABMGS
030400010604     C                   eval      vinmsg = %trimr(vinmsg)
030500010604     C                             + ' ' + 'VHBMGS'
030600971021     C                   ELSE
030700971021     C                   MOVE      VHBMGS        VABMGS
030800971021     C                   ENDIF
030900971021     C*
031000971023     C                   TESTN                   VHBNRS               30
031100971023     C  N30              TESTN                   VHBNRS                 30
031200971023     C  N30              TESTN                   VHBNRS                   30
031300971021     C                   MOVE      VHBNRS        W001A             1
031400971021     C     *IN30         IFEQ      *OFF
031500971021     C     W001A         ORLT      '0'
031600971023     C     W001A         ANDNE     *BLANK
031700971021     C                   SETON                                        32
031800971021     C                   MOVE      *ZERO         VABNRS
031900010604     C                   eval      vinmsg = %trimr(vinmsg)
032000010604     C                             + ' ' + 'VHBNRS'
032100971021     C                   ELSE
032200971021     C                   MOVE      VHBNRS        VABNRS
032300971021     C                   ENDIF
032400971021     C*
032500971023     C                   TESTN                   VHBNSP               30
032600971023     C  N30              TESTN                   VHBNSP                 30
032700971023     C  N30              TESTN                   VHBNSP                   30
032800971021     C                   MOVE      VHBNSP        W001A             1
032900971021     C     *IN30         IFEQ      *OFF
033000971021     C     W001A         ORLT      '0'
033100971023     C     W001A         ANDNE     *BLANK
033200971021     C                   SETON                                        32
033300971021     C                   MOVE      *ZERO         VABNSP
033400010604     C                   eval      vinmsg = %trimr(vinmsg)
033500010604     C                             + ' ' + 'VHBNSP'
033600971021     C                   ELSE
033700971021     C                   MOVE      VHBNSP        VABNSP
033800971021     C                   ENDIF
033900971021     C*
034000971023     C                   TESTN                   VHBLNA               30
034100971023     C  N30              TESTN                   VHBLNA                 30
034200971023     C  N30              TESTN                   VHBLNA                   30
034300971021     C                   MOVE      VHBLNA        W001A             1
034400971021     C     *IN30         IFEQ      *OFF
034500971021     C     W001A         ORLT      '0'
034600971023     C     W001A         ANDNE     *BLANK
034700971021     C                   SETON                                        32
034800971021     C                   MOVE      *ZERO         VABLNA
034900010604     C                   eval      vinmsg = %trimr(vinmsg)
035000010604     C                             + ' ' + 'VHBLNA'
035100971021     C                   ELSE
035200971021     C                   MOVE      VHBLNA        VABLNA
035300971021     C                   ENDIF
035400971021     C*
035500971023     C                   TESTN                   VHBCTR               30
035600971023     C  N30              TESTN                   VHBCTR                 30
035700971023     C  N30              TESTN                   VHBCTR                   30
035800971021     C                   MOVE      VHBCTR        W001A             1
035900971021     C     *IN30         IFEQ      *OFF
036000971021     C     W001A         ORLT      '0'
036100971023     C     W001A         ANDNE     *BLANK
036200971021     C                   SETON                                        32
036300971021     C                   MOVE      *ZERO         VABCTR
036400010604     C                   eval      vinmsg = %trimr(vinmsg)
036500010604     C                             + ' ' + 'VHBCTR'
036600971021     C                   ELSE
036700971021     C                   MOVE      VHBCTR        VABCTR
036800971021     C                   ENDIF
036900941229     C*
037000971021     C                   MOVE      *ZERO         VABIAS
037100941229     C                   MOVE      VHBIAS        WASC15
037200971023     C                   TESTN                   WI15                 30
037300971023     C  N30              TESTN                   WI15                   30
037400971023     C  N30              TESTN                   WI15                     30
037500971021     C                   MOVE      WI15          W001A             1
037600971021     C     *IN30         IFEQ      *OFF
037700971021     C     W001A         ORLT      '0'
037800971023     C     W001A         ANDNE     *BLANK
037900971021     C                   SETON                                        32
038000971021     C                   MOVE      *ZERO         VABIAS
038100010604     C                   eval      vinmsg = %trimr(vinmsg)
038200010604     C                             + ' ' + 'VHBIAS'
038300971021     C                   ELSE
038400971021     C                   MOVEL     WI15          VABIAS
038500971023     C                   TESTN                   WD15                 30
038600971023     C  N30              TESTN                   WD15                   30
038700971023     C  N30              TESTN                   WD15                     30
038800971021     C                   MOVE      WD15          W001A             1
038900971021     C     *IN30         IFEQ      *OFF
039000971021     C     W001A         ORLT      '0'
039100971023     C     W001A         ANDNE     *BLANK
039200971021     C                   SETON                                        32
039300971021     C                   MOVE      *ZERO         VABIAS
039400010604     C                   eval      vinmsg = %trimr(vinmsg)
039500010604     C                             + ' ' + 'VHBIAS'
039600971021     C                   ELSE
039700971021     C                   MOVE      WD15          VABIAS
039800971021     C                   ENDIF
039900050118     C                   ENDIF
040000941229     C*
040100971023     C                   TESTN                   VHBNCL               30
040200971023     C  N30              TESTN                   VHBNCL                 30
040300971023     C  N30              TESTN                   VHBNCL                   30
040400971021     C                   MOVE      VHBNCL        W001A             1
040500971021     C     *IN30         IFEQ      *OFF
040600971021     C     W001A         ORLT      '0'
040700971021     C                   SETON                                        32
040800971021     C                   MOVE      *ZERO         VABNCL
040900010604     C                   eval      vinmsg = %trimr(vinmsg)
041000010604     C                             + ' ' + 'VHBNCL'
041100971021     C                   ELSE
041200971021     C                   MOVE      VHBNCL        VABNCL
041300971021     C                   ENDIF
041400941229     C*
041500971021     C                   MOVE      *ZERO         VABPKB
041600971021     C                   MOVE      VHBPKB        WASC09
041700010719     C                   IF        WI09 = *blanks
041800010719     C                   EVAL      WI09 = *zeros
041900010719     C                   ENDIF
042000971023     C                   TESTN                   WI09                 30
042100971023     C  N30              TESTN                   WI09                   30
042200971023     C  N30              TESTN                   WI09                     30
042300971021     C                   MOVE      WI09          W001A             1
042400971021     C     *IN30         IFEQ      *OFF
042500971021     C     W001A         ORLT      '0'
042600971021     C                   SETON                                        32
042700971021     C                   MOVE      *ZERO         VABPKB
042800010604     C                   eval      vinmsg = %trimr(vinmsg)
042900010604     C                             + ' ' + 'VHBPKB'
043000971021     C                   ELSE
043100971021     C                   MOVEL     WI09          VABPKB
043200010719     C                   IF        WD09 = *blanks
043300010719     C                   EVAL      WD09 = *zeros
043400010719     C                   ENDIF
043500971023     C                   TESTN                   WD09                 30
043600971023     C  N30              TESTN                   WD09                     30
043700971021     C                   MOVE      WD09          W001A             1
043800971021     C     *IN30         IFEQ      *OFF
043900971021     C     W001A         ORLT      '0'
044000971023     C     W001A         ANDNE     *BLANK
044100971021     C                   SETON                                        32
044200971021     C                   MOVE      *ZERO         VABPKB
044300010604     C                   eval      vinmsg = %trimr(vinmsg)
044400010604     C                             + ' ' + 'VHBPKB'
044500971021     C                   ELSE
044600971021     C                   MOVE      WD09          VABPKB
044700971021     C                   ENDIF
044800050118     C                   ENDIF
044900971021     C*
045000041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
045100041001     C                   IF        *IN51 = *OFF
045200971021     C                   MOVE      *ZERO         VABVLB
045300971021     C                   MOVE      VHBVLB        WASC07
045400010719     C                   IF        WI07 = *blanks
045500010719     C                   EVAL      WI07 = *zeros
045600010719     C                   ENDIF
045700971023     C                   TESTN                   WI07                 30
045800971023     C  N30              TESTN                   WI07                   30
045900971023     C  N30              TESTN                   WI07                     30
046000971021     C                   MOVE      WI07          W001A             1
046100971021     C     *IN30         IFEQ      *OFF
046200971021     C     W001A         ORLT      '0'
046300971023     C     W001A         ANDNE     *BLANK
046400971021     C                   SETON                                        32
046500971021     C                   MOVE      *ZERO         VABVLB
046600010604     C                   eval      vinmsg = %trimr(vinmsg)
046700010604     C                             + ' ' + 'VHBVLB'
046800971021     C                   ELSE
046900971021     C                   MOVEL     WI07          VABVLB
047000010719     C                   IF        WD07 = *blanks
047100010719     C                   EVAL      WD07 = *zeros
047200010719     C                   ENDIF
047300971023     C                   TESTN                   WD07                 30
047400971023     C  N30              TESTN                   WD07                   30
047500971023     C  N30              TESTN                   WD07                     30
047600971021     C                   MOVE      WD07          W001A             1
047700971021     C     *IN30         IFEQ      *OFF
047800971021     C     W001A         ORLT      '0'
047900971023     C     W001A         ANDNE     *BLANK
048000971021     C                   SETON                                        32
048100971021     C                   MOVE      *ZERO         VABVLB
048200010604     C                   eval      vinmsg = %trimr(vinmsg)
048300010604     C                             + ' ' + 'VHBVLB'
048400971021     C                   ELSE
048500971021     C                   MOVE      WD07          VABVLB
048600971021     C                   ENDIF
048700050118     C                   ENDIF
048800041001     C                   ELSE
048900041001     C* ...altrimenti forzo azzeramento del volume
049000041001     C                   Z-ADD     *zeros        VABVLB
049100041001     C                   ENDIF
049200941229     C*
049300971021     C                   MOVE      *ZERO         VABQFT
049400971021     C                   MOVE      VHBQFT        WASC15
049500010719     C                   IF        WI15 = *blanks
049600010719     C                   EVAL      WI15 = *zeros
049700010719     C                   ENDIF
049800971023     C                   TESTN                   WI15                 30
049900971023     C  N30              TESTN                   WI15                   30
050000971023     C  N30              TESTN                   WI15                     30
050100971021     C                   MOVE      WI15          W001A             1
050200971021     C     *IN30         IFEQ      *OFF
050300971021     C     W001A         ORLT      '0'
050400971023     C     W001A         ANDNE     *BLANK
050500971021     C                   SETON                                        32
050600971021     C                   MOVE      *ZERO         VABQFT
050700010604     C                   eval      vinmsg = %trimr(vinmsg)
050800010604     C                             + ' ' + 'VHBQFT'
050900971021     C                   ELSE
051000971021     C                   MOVEL     WI15          VABQFT
051100010719     C                   IF        WD15 = *blanks
051200010719     C                   EVAL      WD15 = *zeros
051300010719     C                   ENDIF
051400971023     C                   TESTN                   WD15                 30
051500971023     C  N30              TESTN                   WD15                   30
051600971023     C  N30              TESTN                   WD15                     30
051700971021     C                   MOVE      WD15          W001A             1
051800971021     C     *IN30         IFEQ      *OFF
051900971021     C     W001A         ORLT      '0'
052000971023     C     W001A         ANDNE     *BLANK
052100971021     C                   SETON                                        32
052200971021     C                   MOVE      *ZERO         VABQFT
052300010604     C                   eval      vinmsg = %trimr(vinmsg)
052400010604     C                             + ' ' + 'VHBQFT'
052500971021     C                   ELSE
052600971021     C                   MOVE      WD15          VABQFT
052700971021     C                   ENDIF
052800050118     C                   ENDIF
052900941229     C*
053000971021     C                   MOVE      *ZERO         VABCAS
053100971021     C                   MOVE      VHBCAS        WASC15
053200010719     C                   IF        WI15 = *blanks
053300010719     C                   EVAL      WI15 = *zeros
053400010719     C                   ENDIF
053500971023     C                   TESTN                   WI15                 30
053600971023     C  N30              TESTN                   WI15                   30
053700971023     C  N30              TESTN                   WI15                     30
053800971021     C                   MOVE      WI15          W001A             1
053900971021     C     *IN30         IFEQ      *OFF
054000971021     C     W001A         ORLT      '0'
054100971023     C     W001A         ANDNE     *BLANK
054200971021     C                   SETON                                        32
054300971021     C                   MOVE      *ZERO         VABCAS
054400010604     C                   eval      vinmsg = %trimr(vinmsg)
054500010604     C                             + ' ' + 'VHBCAS'
054600971021     C                   ELSE
054700971021     C                   MOVEL     WI15          VABCAS
054800010719     C                   IF        WD15 = *blanks
054900010719     C                   EVAL      WD15 = *zeros
055000010719     C                   ENDIF
055100971023     C                   TESTN                   WD15                 30
055200971023     C  N30              TESTN                   WD15                   30
055300971023     C  N30              TESTN                   WD15                     30
055400971021     C                   MOVE      WD15          W001A             1
055500971021     C     *IN30         IFEQ      *OFF
055600971021     C     W001A         ORLT      '0'
055700971023     C     W001A         ANDNE     *BLANK
055800971021     C                   SETON                                        32
055900971021     C                   MOVE      *ZERO         VABCAS
056000010604     C                   eval      vinmsg = %trimr(vinmsg)
056100010604     C                             + ' ' + 'VHBCAS'
056200971021     C                   ELSE
056300971021     C                   MOVE      WD15          VABCAS
056400971021     C                   ENDIF
056500050118     C                   ENDIF
056600971021     C*
056700971023     C                   TESTN                   VHBRMN               30
056800971023     C  N30              TESTN                   VHBRMN                 30
056900971023     C  N30              TESTN                   VHBRMN                   30
057000971021     C                   MOVE      VHBRMN        W001A             1
057100971021     C     *IN30         IFEQ      *OFF
057200971021     C     W001A         ORLT      '0'
057300971021     C                   SETON                                        32
057400971021     C                   MOVE      *ZERO         VABRMN
057500010604     C                   eval      vinmsg = %trimr(vinmsg)
057600010604     C                             + ' ' + 'VHBRMN'
057700971021     C                   ELSE
057800971021     C                   MOVE      VHBRMN        VABRMN
057900971021     C                   ENDIF
058000971021     C*
058100971023     C                   TESTN                   VHBNCD               30
058200971023     C  N30              TESTN                   VHBNCD                 30
058300971023     C  N30              TESTN                   VHBNCD                   30
058400971021     C                   MOVE      VHBNCD        W001A             1
058500971021     C     *IN30         IFEQ      *OFF
058600971021     C     W001A         ORLT      '0'
058700971023     C     W001A         ANDNE     *BLANK
058800971021     C                   SETON                                        32
058900971021     C                   MOVE      *ZERO         VABNCD
059000010604     C                   eval      vinmsg = %trimr(vinmsg)
059100010604     C                             + ' ' + 'VHBNCD'
059200971021     C                   ELSE
059300971021     C                   MOVE      VHBNCD        VABNCD
059400971021     C                   ENDIF
059500971021     C*
059600971023     C                   TESTN                   VHBNCA               30
059700971023     C  N30              TESTN                   VHBNCA                 30
059800971023     C  N30              TESTN                   VHBNCA                   30
059900971021     C                   MOVE      VHBNCA        W001A             1
060000971021     C     *IN30         IFEQ      *OFF
060100971021     C     W001A         ORLT      '0'
060200971023     C     W001A         ANDNE     *BLANK
060300971021     C                   SETON                                        32
060400971021     C                   MOVE      *ZERO         VABNCA
060500010604     C                   eval      vinmsg = %trimr(vinmsg)
060600010604     C                             + ' ' + 'VHBNCA'
060700971021     C                   ELSE
060800971021     C                   MOVE      VHBNCA        VABNCA
060900971021     C                   ENDIF
061000971021     C*
061100971023     C                   TESTN                   VHBZNC               30
061200971023     C  N30              TESTN                   VHBZNC                 30
061300971023     C  N30              TESTN                   VHBZNC                   30
061400971021     C                   MOVE      VHBZNC        W001A             1
061500971021     C     *IN30         IFEQ      *OFF
061600971021     C     W001A         ORLT      '0'
061700971023     C     W001A         ANDNE     *BLANK
061800971021     C                   SETON                                        32
061900971021     C                   MOVE      *ZERO         VABZNC
062000010604     C                   eval      vinmsg = %trimr(vinmsg)
062100010604     C                             + ' ' + 'VHBZNC'
062200971021     C                   ELSE
062300971021     C                   MOVE      VHBZNC        VABZNC
062400971021     C                   ENDIF
062500971021     C*
062600971023     C                   TESTN                   VHBDCR               30
062700971023     C  N30              TESTN                   VHBDCR                 30
062800971023     C  N30              TESTN                   VHBDCR                   30
062900971021     C                   MOVE      VHBDCR        W001A             1
063000971021     C     *IN30         IFEQ      *OFF
063100971021     C     W001A         ORLT      '0'
063200971023     C     W001A         ANDNE     *BLANK
063300971021     C                   SETON                                        32
063400971021     C                   MOVE      *ZERO         VABDCR
063500010604     C                   eval      vinmsg = %trimr(vinmsg)
063600010604     C                             + ' ' + 'VHBDCR'
063700971021     C                   ELSE
063800971021     C                   MOVE      VHBDCR        VABDCR
063900971021     C                   ENDIF
064000971021     C*
064100971023     C                   TESTN                   VHBHCR               30
064200971023     C  N30              TESTN                   VHBHCR                 30
064300971023     C  N30              TESTN                   VHBHCR                   30
064400971021     C                   MOVE      VHBHCR        W001A             1
064500971021     C     *IN30         IFEQ      *OFF
064600971021     C     W001A         ORLT      '0'
064700971023     C     W001A         ANDNE     *BLANK
064800971021     C                   SETON                                        32
064900971021     C                   MOVE      *ZERO         VABHCR
065000010604     C                   eval      vinmsg = %trimr(vinmsg)
065100010604     C                             + ' ' + 'VHBHCR'
065200971021     C                   ELSE
065300971021     C                   MOVE      VHBHCR        VABHCR
065400971021     C                   ENDIF
065500941229     C*
065600971021     C                   MOVE      *ZERO         VABVMD
065700971021     C                   MOVE      VHBVMD        WASC15
065800010719     C                   IF        WI15 = *blanks
065900010719     C                   EVAL      WI15 = *zeros
066000010719     C                   ENDIF
066100971023     C                   TESTN                   WI15                 30
066200971023     C  N30              TESTN                   WI15                   30
066300971023     C  N30              TESTN                   WI15                     30
066400971021     C                   MOVE      WI15          W001A             1
066500971021     C     *IN30         IFEQ      *OFF
066600971021     C     W001A         ORLT      '0'
066700971023     C     W001A         ANDNE     *BLANK
066800971021     C                   SETON                                        32
066900971021     C                   MOVE      *ZERO         VABVMD
067000010604     C                   eval      vinmsg = %trimr(vinmsg)
067100010604     C                             + ' ' + 'VHBVMD'
067200971021     C                   ELSE
067300971021     C                   MOVEL     WI15          VABVMD
067400010719     C                   IF        WD15 = *blanks
067500010719     C                   EVAL      WD15 = *zeros
067600010719     C                   ENDIF
067700971023     C                   TESTN                   WD15                 30
067800971023     C  N30              TESTN                   WD15                   30
067900971023     C  N30              TESTN                   WD15                     30
068000971021     C                   MOVE      WD15          W001A             1
068100971021     C     *IN30         IFEQ      *OFF
068200971021     C     W001A         ORLT      '0'
068300971023     C     W001A         ANDNE     *BLANK
068400971021     C                   SETON                                        32
068500971021     C                   MOVE      *ZERO         VABVMD
068600010604     C                   eval      vinmsg = %trimr(vinmsg)
068700010604     C                             + ' ' + 'VHBVMD'
068800971021     C                   ELSE
068900971021     C                   MOVE      WD15          VABVMD
069000971021     C                   ENDIF
069100050118     C                   ENDIF
069200971021     C*
069300971023     C                   TESTN                   VHBANT               30
069400971023     C  N30              TESTN                   VHBANT                 30
069500971023     C  N30              TESTN                   VHBANT                   30
069600971021     C                   MOVE      VHBANT        W001A             1
069700971021     C     *IN30         IFEQ      *OFF
069800971021     C     W001A         ORLT      '0'
069900971023     C     W001A         ANDNE     *BLANK
070000971021     C                   SETON                                        32
070100971021     C                   MOVE      *ZERO         VABANT
070200010604     C                   eval      vinmsg = %trimr(vinmsg)
070300010604     C                             + ' ' + 'VHBANT'
070400971021     C                   ELSE
070500971021     C                   MOVE      VHBANT        VABANT
070600971021     C                   ENDIF
070700971021     C*
070800971021     C* CAMPI ALFABETICI:
070900971021     C                   MOVE      VHBATB        VABATB
071000971021     C                   MOVE      VHBCBO        VABCBO
071100971021     C                   MOVE      VHBRSD        VABRSD
071200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
071300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
071400020117     C* ==
071500971021     C                   MOVE      VHBRD2        VABRD2
071600971021     C                   MOVE      VHBIND        VABIND
071700971021     C                   MOVE      VHBCAD        VABCAD
071800971021     C                   MOVE      VHBLOD        VABLOD
071900971021     C                   MOVE      VHBPRD        VABPRD
072000971021     C                   MOVE      VHBNZD        VABNZD
072100971021     C                   MOVE      VHBGC1        VABGC1
072200971021     C                   MOVE      VHBGC2        VABGC2
072300971021     C                   MOVE      VHBTSP        VABTSP
072400971021     C                   MOVE      VHBVAS        VABVAS
072500971021     C                   MOVE      VHBNAS        VABNAS
072600971021     C                   MOVE      VHBTIC        VABTIC
072700971021     C                   MOVE      VHBVCA        VABVCA
072800971021     C                   MOVE      VHBGCA        VABGCA
072900971021     C                   MOVE      VHBRMA        VABRMA
073000971021     C                   MOVE      VHBXCO        VABXCO
073100971021     C                   MOVE      VHBNOT        VABNOT
073200971021     C                   MOVE      VHBNT2        VABNT2
073300971021     C                   MOVE      VHBCTM        VABCTM
073400971021     C                   MOVE      VHBFFD        VABFFD
073500971021     C                   MOVE      VHBTCR        VABTCR
073600971021     C                   MOVE      VHBCTS        VABCTS
073700971021     C                   MOVE      VHBFTM        VABFTM
073800971021     C                   MOVE      VHBVAD        VABVAD
073900971021     C                   MOVE      VHBGMA        VABGMA
074000971021     C                   MOVE      VHBGGA        VABGGA
074100971021     C                   MOVE      VHBGVA        VABGVA
074200971021     C                   MOVE      VHBTC1        VABTC1
074300971021     C                   MOVE      VHBTC2        VABTC2
074400971021     C                   MOVE      VHBSCL        VABSCL
074500971021     C                   MOVE      VHBRMO        VABRMO
074600971021     C                   MOVE      VHBCMO        VABCMO
074700971021     C                   MOVE      VHBNMO        VABNMO
074800971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
074900971021     C* NELLA NAZIONE ORIGINALE
075000971021     C                   MOVE      VHBNMO        WA1D              1
075100971021     C                   MOVEL     VHBNMO        WA1S              1
075200971021     C     WA1D          IFNE      *BLANK
075300971021     C     WA1S          ANDEQ     *BLANK
075400971021     C                   MOVE      *BLANK        VABNMO
075500030616     C                   ENDIF
075600030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
075700030710     C                   EVAL      VABCMR = wCMR
075800030616     C                   EVAL      VABDCM = DATCOR
075900030616     C                   EVAL      VABDTS = DATCOR
076000040402     C                   EVAL      VABHMS = ORACOR
076100030616     C                   EVAL      VABCNT = 1
076200041025     C*
076300041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
076400041025     C                   IF        *IN52 = *ON
076500041025     C* Reperisco la provincia dal CAP e dalla località
076600041025     C                   IF        VABCAD <> *blanks AND
076700050907     C                             VABPRD  = *blanks AND
076800050907     C                             VABNZD  = *blanks
076900041025     C                   CLEAR                   TISI95DS
077000041025     C                   EVAL      I95TCN = '3'
077100041025     C                   Z-ADD     datcor        I95DAT
077200041025     C                   EVAL      I95CAP = VABCAD
077300041025     C                   EVAL      I95LOC = VABLOD
077400041025     C                   CALL      'TISI95R'
077500041025     C                   PARM                    TISI95DS
077600041025     C                   EVAL      VABPRD = O95PRV
077700041025     C                   ENDIF
077800041025     C                   ENDIF
077900100419     C*
078000100419     C* Forzatura su campi DIVISE
078100100419     C                   IF        VABVAS = *blanks
078200100419     C                   EVAL      VABVAS = 'EUR'
078300100419     C                   ENDIF
078400100419     C                   IF        VABVCA = *blanks
078500100419     C                   EVAL      VABVCA = 'EUR'
078600100419     C                   ENDIF
078700100419     C                   IF        VABVAD = *blanks
078800100419     C                   EVAL      VABVAD = 'EUR'
078900100419     C                   ENDIF
079000111220
079100111220      * FORZATURE:
079200111220      * tipo incasso
079300111220      * se c/assegno > 500 tipo incasso = 'SC'
079400111220     c                   IF        VABcas > 500
079500111220     c                   eval      VABtic = 'SC'
079600111220     c                   ENDIF
079700111220
079800111220
079900111220
080000930409     C*
080100010604     C  N31              ADD       1             §CTROK            7 0
080200010604     C   32              ADD       1             §CTRMO            7 0
080300010604     C   31              ADD       1             §CTRNO            7 0
080400030616     C  N31              WRITE     EDIVAB00
080500990910     C*
080600010601     C                   if        *in31 = *off and
080700010601     C                             *in32 = *off
080800990910     C                   eval      vinflg = '1'
080900990910     C                   else
081000990910     C                   eval      vinflg = '2'
081100990910     C                   endif
081200991022     C                   endif
081300990910     C                   endif
081400010601     C*
081500010601     C                   endif
081600000905     C*
081700000905     C                   else
081800000905     C                   eval      vinflg = '1'
081900000905     C                   endif
082000000905     C*
082100000905     C  N70              update    tivin000
082200991022     C*
082300991022     C  N70              ENDdo
082400010530     C
082500010530     C*
082600010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
082700010530     C                   if        cntNonEl = *zeros or
082800010530     C                             flgMulti = '0'
082900990910     C* Se non ci sono record con errori ...
083000010530     C                   if        §ctrno = 0 and
083100010601     C                             §ctrmo = 0 and
083200010530     C                             flgStato <> '2'
083300990910     C* ... restituisco esito OK.
083400990921     C                   eval      wrkesito = '0'
083500990910     C                   else
083600010601     C                   if        §ctrok > 0
083700990921     C                   eval      wrkesito = '1'
083800000710     C                   else
083900010615     C                   if        flgOk = '0'
084000010615     C                   eval      wrkesito = '2'
084100010615     C                   else
084200010615     C                   eval      wrkesito = '6'
084300010615     C                   endif
084400990910     C                   endif
084500000710     C                   endif
084600010530     C                   else
084700010530     C                   eval      wrkesito = '9'
084800010530     C                   endif
084900030710     C*
085000030710 X  1C                   else
085100030710     C                   eval      wrkesito = '8'
085200030710     C                   eval      flgGiro = '1'
085300030710 E  1C                   endif
085400030710     C*
085500990914     C                   if        %open(tivin00r)
085600990908     C                   close     tivin00r
085700990914     C                   endif
085800030616     C                   if        %open(edivabwr)
085900030616     C                   close     edivabwr
086000990914     C                   endif
086100010530     C*
086200010530     C                   if        vlrpoi <> 999
086300010530     C                   eval      invfil = vlrpoi
086400010530     C                   endif
086500990910     C*
086600990920     C                   if        §ctrok > 0
086700010530     C                             and invfil > *zeros
086800000613     C                   exsr      invio
086900990920     C                   endif
087000010612     C*
087100010612     C                   if        flgGiro = '1'
087200010612     C                   exsr      endela
087300010612     C                   endif
087400990920     C*
087500910830     C                   ENDSR
087600000613     C***
087700990920
087800010530
087900010530
088000010530
088100010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
088200010530     C     repfil        BEGSR
088300010530     C*
088400010530     C                   if        invfil = *zeros and
088500010530     C                             depfil > *zeros and
088600010613     C                             (vinflg = *blanks or
088700010613     C                              vinflg = *zeros)
088800010530     C
088900010530     C                   eval      invfil = depfil
089000010530     C                   endif
089100010530     C*
089200010530     C                   if        depfil <> invfil and
089300010530     C                             invfil > *zeros
089400010530     C                   eval      flgMulti = '1'
089500010530     C                   if        vinflg = *blanks
089600010530     C                   add       1             cntNonEl
089700010530     C                   endif
089800010530     C                   endif
089900010530     C*
090000010530     C                   if        vinflg = '2'
090100010530     C                   eval      flgStato = '2'
090200010530     C                   endif
090300010530     C*
090400010530     C                   ENDSR
090500010530     C***
090600010530
090700010530
090800010530
090900010530
091000990920      /TITLE Invio dei dati al punto operativo.
091100000613     C     invio         BEGSR
091200990920     C*
091300990920     C                   reset                   dscmz
091400030618     C                   move      invfil        cmzdst
091500030618     C***                move      '789'         cmzdst
091600990920     C                   eval      cmzfld = vlrfou
091700990920     C                   eval      cmzmbd = vlrhdl
091800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
091900000710     C                   if        prmfir = *blanks
092000030616     C                   eval      cmzfla = 'EDIVAB0F'
092100030616     C                   eval      cmzmba = 'EDIVAB0F'
092200000710     C                   else
092300000710     C                   eval      cmzfla = prmfir
092400000710     C                   eval      cmzmba = prmfir
092500000710     C                   endif
092600990920     C                   eval      cmznrr = *zeros
092700990920     C                   move      §ctrok        cmznrr
092800021003     C                   eval      cmzlba = vlrfl1
092900990920     C                   call(e)   'TIS711C'
093000990920     C                   parm                    dscmz
093100990921     C                   parm      *blanks       esito
093200990920     C                   if        %error
093300990920     C                             or cmzerr = '1'
093400990921     C                             or esito  = '1'
093500000710     C                   eval      wrkesito = '3'
093600990920     C                   endif
093700990920     C*
093800000613     C                   ENDSR
093900000613     C***
094000010530
094100010530
094200010530
094300010530
094400010530      /TITLE Invio dei dati al punto operativo.
094500010530     C     opeini        BEGSR
094600010530     C*
094700010530     C* Inizializzo flag e contatori operativi
094800010530     C                   movel     '0'           flgGiro           1
094900010530     C                   movel     '0'           flgMulti          1
095000010530     C                   movel     '1'           flgStato          1
095100010615     C                   movel     '0'           flgOk             1
095200010530     C                   z-add     *zeros        cntNonEl         10 0
095300010530     C                   z-add     *zeros        depfil            3 0
095400010530     C                   z-add     *zeros        invfil            3 0
095500010530     C*
095600010530     C                   ENDSR
095700010530     C***
095800010530
095900010530
096000010530
096100990910
096200000613     C     *inzsr        BEGSR
096300990910     C*
096400990910     C     *entry        plist
096500990920     C                   parm                    tivlrds
096600990921     C                   parm      wrkesito      esito
096700000724     C                   parm                    prmlit
096800000710     C                   parm                    prmfir
096900030616     C*
097000030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
097100030710     C                   move(p)   vlrMSG        wNomeFile
097200030710     C*
097300030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
097400030710     C* del codice CMR dal nome del file importato.
097500030710     C                   if        wNomeFile <> *blanks AND
097600030710     C                             vlrppt<>*blanks
097700041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
097800030710     C                   if        %subst(vlrppt:1:1) = 'S'
097900030710     C                   seton                                        50
098000030710     C                   else
098100030710     C                   setoff                                       50
098200030710     C                   endif
098300041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
098400041001     C                   if        %subst(vlrppt:6:1) = 'S'
098500041001     C                   seton                                        51
098600041001     C                   else
098700041001     C                   setoff                                       51
098800041001     C                   endif
098900041025     C*
099000041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
099100041025     C* (naturalmente solo se nn già presente)
099200041025     C                   if        %subst(vlrppt:7:1) = 'S'
099300041025     C                   seton                                        52
099400041025     C                   else
099500041025     C                   setoff                                       52
099600041025     C                   endif
099700041001     C*
099800030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
099900030710     C                   move(p)   wPosDaA       wPosDa
100000030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
100100030710     C                   move(p)   wLungA        wLung
100200030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
100300030710     C                   seton                                        60
100400030710     C                   else
100500050914     C                   if        vlrisv = 'UL'
100600050914     C                   movel     datcor        wDataOraAlfa
100700050914     C                   move      oracor        wDataOraAlfa
100800050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
100900050914     C                   seton                                        60
101000050914     C                   else
101100030710     C                   setoff                                       60
101200050914     C                   endif
101300030710     C                   endif
101400010606     C*
101500010606     C* CALCOLA LA DATA CORRENTE
101600010606     C                   time                    wn14             14 0
101700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
101800040402     C                   movel     wn14          oracor            6 0          *ORA
101900010606     C                   z-add     wn8           g08dat
102000010606     C                   z-add     *zeros        g08inv
102100010606     C                   movel     '0'           g08err
102200010606     C                   call      'XSRDA8'
102300010606     C                   parm                    wlbda8
102400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
102500000613     C*
102600000613     C                   ENDSR
