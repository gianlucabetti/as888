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
016700990908
016800010530
016900010530
017000010530
017100910830     C*--------------------------------------------------------
017200030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
017300910830     C*--------------------------------------------------------
017400941229     C     RWVAB         BEGSR
017500030710     C*
017600030710     C                   clear                   §CTROK
017700030710     C                   clear                   §CTRMO
017800030710     C                   clear                   §CTRNO
017900990910     C*
018000030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
018100030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
018200030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
018300030710     C                   if        *in60 = *on
018400990914     C                   if        not %open(tivin00r)
018500990908     C                   open      tivin00r
018600990914     C                   endif
018700030616     C                   if        not %open(edivabwr)
018800030616     C                   open      edivabwr
018900990914     C                   endif
019000990910     C*
019100921023     C                   DO        *HIVAL
019200990913     C*
019300990915     C                   READ      tivin00r                               70
019400071018     C*
019500071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
019600071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
019700010618     C*
019800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019900010618     C                   if        vinflg = '1'
020000010618     C                   eval      flgOk = '1'
020100010618     C                   endif
020200010705     C*
020300010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
020400010705     C                   if        %subst(vindta:1:1) = '='
020500010705     C                   eval      %subst(vindta:1:1) = *blanks
020600010705     C                   endif
020700010705     C*
020800000905     C                   if        vindta > *blanks
020900000613     C                   add       1             rrnum
021000010601     C* ...elimino eventuali segni
021100010601     C     '+':' '       xlate     vindta        vindta
021200990913     C*
021300010601     C                   if        *in70 = *off and
021400990913     C                             (vinflg = *blanks
021500010711     C                              or vinflg = '0'
021600010711     C                              or vinflg = '2')
021700990913     C*
021800030616     C                   clear                   EDIVAB00
021900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022000010711     C                   if        vinflg = *blanks or vinflg = '0'
022100010711     C                   clear                   vinmsg
022200010711     C                   endif
022300010711     C*
022400050118     C                   eval      x_rec  = vindta
022500991022     C*
022600991022     C                   testb     x'3F'         vhbatb                   02
022700991022     C                   if        not *in02
022800991022     C*
022900971021     C                   SETOFF                                       303132
023000971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
023100971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
023200971023     C*    OPPURE TUTTI BLANKS
023300010530     C*
023400010530     C                   TESTN                   VHBLNP               30
023500010530     C  N30              TESTN                   VHBLNP                 30
023600010530     C  N30              TESTN                   VHBLNP                   30
023700010530     C                   MOVE      VHBLNP        W001A             1
023800010530     C     *IN30         IFEQ      *OFF
023900010530     C     W001A         ORLT      '0'
024000010530     C                   SETON                                        31
024100010530     C                   eval      vinmsg = %trimr(vinmsg)
024200010530     C                             + ' ' + 'VHBLNP'
024300010530     C                   ELSE
024400010530     C                   MOVE      VHBLNP        VABLNP
024500010530     C     VABLNP        IFLE      *ZEROS
024600010530     C                   SETON                                        31
024700010530     C                   eval      vinmsg = %trimr(vinmsg)
024800010530     C                             + ' ' + 'VHBLNP'
024900010530     C                   ENDIF
025000010530     C                   ENDIF
025100010530     C*
025200010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025300010530     C                   eval      depfil = VABLNP
025400010530     C                   exsr      repfil
025500010601     C                   if        depfil = invfil
025600021014     C                   if        vlrpoi = 999
025700021010     C                   MOVE(P)   invfil        VABFGS
025800021014     C                   else
025900021014     C                   MOVE(P)   vlrpoi        VABFGS
026000021014     C                   endif
026100010530     C*
026200010530     C                   exsr      PREELA
026300010530     C*
026400971023     C                   TESTN                   VHBCCM               30
026500971023     C  N30              TESTN                   VHBCCM                 30
026600971023     C  N30              TESTN                   VHBCCM                   30
026700971021     C                   MOVE      VHBCCM        W001A             1
026800971021     C     *IN30         IFEQ      *OFF
026900971021     C     W001A         ORLT      '0'
027000971021     C                   SETON                                        31
027100010604     C                   eval      vinmsg = %trimr(vinmsg)
027200010604     C                             + ' ' + 'VHBCCM'
027300971021     C                   ELSE
027400971021     C                   MOVE      VHBCCM        VABCCM
027500971024     C     VABCCM        IFLE      *ZEROS
027600971024     C                   SETON                                        31
027700010604     C                   eval      vinmsg = %trimr(vinmsg)
027800010604     C                             + ' ' + 'VHBCCM'
027900971024     C                   ENDIF
028000971021     C                   ENDIF
028100971021     C*
028200971023     C                   TESTN                   VHBAAS               30
028300971023     C  N30              TESTN                   VHBAAS                 30
028400971023     C  N30              TESTN                   VHBAAS                   30
028500971021     C                   MOVE      VHBAAS        W001A             1
028600971021     C     *IN30         IFEQ      *OFF
028700971021     C     W001A         ORLT      '0'
028800971023     C     W001A         ANDNE     *BLANK
028900971021     C                   SETON                                        32
029000010606     C                   MOVEL     datcor        VABAAS
029100010604     C                   eval      vinmsg = %trimr(vinmsg)
029200010604     C                             + ' ' + 'VHBAAS'
029300971021     C                   ELSE
029400971021     C                   MOVE      VHBAAS        VABAAS
029500971021     C                   ENDIF
029600971021     C*
029700971023     C                   TESTN                   VHBMGS               30
029800971023     C  N30              TESTN                   VHBMGS                 30
029900971023     C  N30              TESTN                   VHBMGS                   30
030000971021     C                   MOVE      VHBMGS        W001A             1
030100971021     C     *IN30         IFEQ      *OFF
030200971021     C     W001A         ORLT      '0'
030300971023     C     W001A         ANDNE     *BLANK
030400971021     C                   SETON                                        32
030500010601     C                   MOVE      '0101'        VABMGS
030600010604     C                   eval      vinmsg = %trimr(vinmsg)
030700010604     C                             + ' ' + 'VHBMGS'
030800971021     C                   ELSE
030900971021     C                   MOVE      VHBMGS        VABMGS
031000971021     C                   ENDIF
031100971021     C*
031200971023     C                   TESTN                   VHBNRS               30
031300971023     C  N30              TESTN                   VHBNRS                 30
031400971023     C  N30              TESTN                   VHBNRS                   30
031500971021     C                   MOVE      VHBNRS        W001A             1
031600971021     C     *IN30         IFEQ      *OFF
031700971021     C     W001A         ORLT      '0'
031800971023     C     W001A         ANDNE     *BLANK
031900971021     C                   SETON                                        32
032000971021     C                   MOVE      *ZERO         VABNRS
032100010604     C                   eval      vinmsg = %trimr(vinmsg)
032200010604     C                             + ' ' + 'VHBNRS'
032300971021     C                   ELSE
032400971021     C                   MOVE      VHBNRS        VABNRS
032500971021     C                   ENDIF
032600971021     C*
032700971023     C                   TESTN                   VHBNSP               30
032800971023     C  N30              TESTN                   VHBNSP                 30
032900971023     C  N30              TESTN                   VHBNSP                   30
033000971021     C                   MOVE      VHBNSP        W001A             1
033100971021     C     *IN30         IFEQ      *OFF
033200971021     C     W001A         ORLT      '0'
033300971023     C     W001A         ANDNE     *BLANK
033400971021     C                   SETON                                        32
033500971021     C                   MOVE      *ZERO         VABNSP
033600010604     C                   eval      vinmsg = %trimr(vinmsg)
033700010604     C                             + ' ' + 'VHBNSP'
033800971021     C                   ELSE
033900971021     C                   MOVE      VHBNSP        VABNSP
034000971021     C                   ENDIF
034100971021     C*
034200971023     C                   TESTN                   VHBLNA               30
034300971023     C  N30              TESTN                   VHBLNA                 30
034400971023     C  N30              TESTN                   VHBLNA                   30
034500971021     C                   MOVE      VHBLNA        W001A             1
034600971021     C     *IN30         IFEQ      *OFF
034700971021     C     W001A         ORLT      '0'
034800971023     C     W001A         ANDNE     *BLANK
034900971021     C                   SETON                                        32
035000971021     C                   MOVE      *ZERO         VABLNA
035100010604     C                   eval      vinmsg = %trimr(vinmsg)
035200010604     C                             + ' ' + 'VHBLNA'
035300971021     C                   ELSE
035400971021     C                   MOVE      VHBLNA        VABLNA
035500971021     C                   ENDIF
035600971021     C*
035700971023     C                   TESTN                   VHBCTR               30
035800971023     C  N30              TESTN                   VHBCTR                 30
035900971023     C  N30              TESTN                   VHBCTR                   30
036000971021     C                   MOVE      VHBCTR        W001A             1
036100971021     C     *IN30         IFEQ      *OFF
036200971021     C     W001A         ORLT      '0'
036300971023     C     W001A         ANDNE     *BLANK
036400971021     C                   SETON                                        32
036500971021     C                   MOVE      *ZERO         VABCTR
036600010604     C                   eval      vinmsg = %trimr(vinmsg)
036700010604     C                             + ' ' + 'VHBCTR'
036800971021     C                   ELSE
036900971021     C                   MOVE      VHBCTR        VABCTR
037000971021     C                   ENDIF
037100941229     C*
037200971021     C                   MOVE      *ZERO         VABIAS
037300941229     C                   MOVE      VHBIAS        WASC15
037400971023     C                   TESTN                   WI15                 30
037500971023     C  N30              TESTN                   WI15                   30
037600971023     C  N30              TESTN                   WI15                     30
037700971021     C                   MOVE      WI15          W001A             1
037800971021     C     *IN30         IFEQ      *OFF
037900971021     C     W001A         ORLT      '0'
038000971023     C     W001A         ANDNE     *BLANK
038100971021     C                   SETON                                        32
038200971021     C                   MOVE      *ZERO         VABIAS
038300010604     C                   eval      vinmsg = %trimr(vinmsg)
038400010604     C                             + ' ' + 'VHBIAS'
038500971021     C                   ELSE
038600971021     C                   MOVEL     WI15          VABIAS
038700971023     C                   TESTN                   WD15                 30
038800971023     C  N30              TESTN                   WD15                   30
038900971023     C  N30              TESTN                   WD15                     30
039000971021     C                   MOVE      WD15          W001A             1
039100971021     C     *IN30         IFEQ      *OFF
039200971021     C     W001A         ORLT      '0'
039300971023     C     W001A         ANDNE     *BLANK
039400971021     C                   SETON                                        32
039500971021     C                   MOVE      *ZERO         VABIAS
039600010604     C                   eval      vinmsg = %trimr(vinmsg)
039700010604     C                             + ' ' + 'VHBIAS'
039800971021     C                   ELSE
039900971021     C                   MOVE      WD15          VABIAS
040000971021     C                   ENDIF
040100050118     C                   ENDIF
040200941229     C*
040300971023     C                   TESTN                   VHBNCL               30
040400971023     C  N30              TESTN                   VHBNCL                 30
040500971023     C  N30              TESTN                   VHBNCL                   30
040600971021     C                   MOVE      VHBNCL        W001A             1
040700971021     C     *IN30         IFEQ      *OFF
040800971021     C     W001A         ORLT      '0'
040900971021     C                   SETON                                        32
041000971021     C                   MOVE      *ZERO         VABNCL
041100010604     C                   eval      vinmsg = %trimr(vinmsg)
041200010604     C                             + ' ' + 'VHBNCL'
041300971021     C                   ELSE
041400971021     C                   MOVE      VHBNCL        VABNCL
041500971021     C                   ENDIF
041600941229     C*
041700971021     C                   MOVE      *ZERO         VABPKB
041800971021     C                   MOVE      VHBPKB        WASC09
041900010719     C                   IF        WI09 = *blanks
042000010719     C                   EVAL      WI09 = *zeros
042100010719     C                   ENDIF
042200971023     C                   TESTN                   WI09                 30
042300971023     C  N30              TESTN                   WI09                   30
042400971023     C  N30              TESTN                   WI09                     30
042500971021     C                   MOVE      WI09          W001A             1
042600971021     C     *IN30         IFEQ      *OFF
042700971021     C     W001A         ORLT      '0'
042800971021     C                   SETON                                        32
042900971021     C                   MOVE      *ZERO         VABPKB
043000010604     C                   eval      vinmsg = %trimr(vinmsg)
043100010604     C                             + ' ' + 'VHBPKB'
043200971021     C                   ELSE
043300971021     C                   MOVEL     WI09          VABPKB
043400010719     C                   IF        WD09 = *blanks
043500010719     C                   EVAL      WD09 = *zeros
043600010719     C                   ENDIF
043700971023     C                   TESTN                   WD09                 30
043800971023     C  N30              TESTN                   WD09                     30
043900971021     C                   MOVE      WD09          W001A             1
044000971021     C     *IN30         IFEQ      *OFF
044100971021     C     W001A         ORLT      '0'
044200971023     C     W001A         ANDNE     *BLANK
044300971021     C                   SETON                                        32
044400971021     C                   MOVE      *ZERO         VABPKB
044500010604     C                   eval      vinmsg = %trimr(vinmsg)
044600010604     C                             + ' ' + 'VHBPKB'
044700971021     C                   ELSE
044800971021     C                   MOVE      WD09          VABPKB
044900971021     C                   ENDIF
045000050118     C                   ENDIF
045100971021     C*
045200041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
045300041001     C                   IF        *IN51 = *OFF
045400971021     C                   MOVE      *ZERO         VABVLB
045500971021     C                   MOVE      VHBVLB        WASC07
045600010719     C                   IF        WI07 = *blanks
045700010719     C                   EVAL      WI07 = *zeros
045800010719     C                   ENDIF
045900971023     C                   TESTN                   WI07                 30
046000971023     C  N30              TESTN                   WI07                   30
046100971023     C  N30              TESTN                   WI07                     30
046200971021     C                   MOVE      WI07          W001A             1
046300971021     C     *IN30         IFEQ      *OFF
046400971021     C     W001A         ORLT      '0'
046500971023     C     W001A         ANDNE     *BLANK
046600971021     C                   SETON                                        32
046700971021     C                   MOVE      *ZERO         VABVLB
046800010604     C                   eval      vinmsg = %trimr(vinmsg)
046900010604     C                             + ' ' + 'VHBVLB'
047000971021     C                   ELSE
047100971021     C                   MOVEL     WI07          VABVLB
047200010719     C                   IF        WD07 = *blanks
047300010719     C                   EVAL      WD07 = *zeros
047400010719     C                   ENDIF
047500971023     C                   TESTN                   WD07                 30
047600971023     C  N30              TESTN                   WD07                   30
047700971023     C  N30              TESTN                   WD07                     30
047800971021     C                   MOVE      WD07          W001A             1
047900971021     C     *IN30         IFEQ      *OFF
048000971021     C     W001A         ORLT      '0'
048100971023     C     W001A         ANDNE     *BLANK
048200971021     C                   SETON                                        32
048300971021     C                   MOVE      *ZERO         VABVLB
048400010604     C                   eval      vinmsg = %trimr(vinmsg)
048500010604     C                             + ' ' + 'VHBVLB'
048600971021     C                   ELSE
048700971021     C                   MOVE      WD07          VABVLB
048800971021     C                   ENDIF
048900050118     C                   ENDIF
049000041001     C                   ELSE
049100041001     C* ...altrimenti forzo azzeramento del volume
049200041001     C                   Z-ADD     *zeros        VABVLB
049300041001     C                   ENDIF
049400941229     C*
049500971021     C                   MOVE      *ZERO         VABQFT
049600971021     C                   MOVE      VHBQFT        WASC15
049700010719     C                   IF        WI15 = *blanks
049800010719     C                   EVAL      WI15 = *zeros
049900010719     C                   ENDIF
050000971023     C                   TESTN                   WI15                 30
050100971023     C  N30              TESTN                   WI15                   30
050200971023     C  N30              TESTN                   WI15                     30
050300971021     C                   MOVE      WI15          W001A             1
050400971021     C     *IN30         IFEQ      *OFF
050500971021     C     W001A         ORLT      '0'
050600971023     C     W001A         ANDNE     *BLANK
050700971021     C                   SETON                                        32
050800971021     C                   MOVE      *ZERO         VABQFT
050900010604     C                   eval      vinmsg = %trimr(vinmsg)
051000010604     C                             + ' ' + 'VHBQFT'
051100971021     C                   ELSE
051200971021     C                   MOVEL     WI15          VABQFT
051300010719     C                   IF        WD15 = *blanks
051400010719     C                   EVAL      WD15 = *zeros
051500010719     C                   ENDIF
051600971023     C                   TESTN                   WD15                 30
051700971023     C  N30              TESTN                   WD15                   30
051800971023     C  N30              TESTN                   WD15                     30
051900971021     C                   MOVE      WD15          W001A             1
052000971021     C     *IN30         IFEQ      *OFF
052100971021     C     W001A         ORLT      '0'
052200971023     C     W001A         ANDNE     *BLANK
052300971021     C                   SETON                                        32
052400971021     C                   MOVE      *ZERO         VABQFT
052500010604     C                   eval      vinmsg = %trimr(vinmsg)
052600010604     C                             + ' ' + 'VHBQFT'
052700971021     C                   ELSE
052800971021     C                   MOVE      WD15          VABQFT
052900971021     C                   ENDIF
053000050118     C                   ENDIF
053100941229     C*
053200971021     C                   MOVE      *ZERO         VABCAS
053300971021     C                   MOVE      VHBCAS        WASC15
053400010719     C                   IF        WI15 = *blanks
053500010719     C                   EVAL      WI15 = *zeros
053600010719     C                   ENDIF
053700971023     C                   TESTN                   WI15                 30
053800971023     C  N30              TESTN                   WI15                   30
053900971023     C  N30              TESTN                   WI15                     30
054000971021     C                   MOVE      WI15          W001A             1
054100971021     C     *IN30         IFEQ      *OFF
054200971021     C     W001A         ORLT      '0'
054300971023     C     W001A         ANDNE     *BLANK
054400971021     C                   SETON                                        32
054500971021     C                   MOVE      *ZERO         VABCAS
054600010604     C                   eval      vinmsg = %trimr(vinmsg)
054700010604     C                             + ' ' + 'VHBCAS'
054800971021     C                   ELSE
054900971021     C                   MOVEL     WI15          VABCAS
055000010719     C                   IF        WD15 = *blanks
055100010719     C                   EVAL      WD15 = *zeros
055200010719     C                   ENDIF
055300971023     C                   TESTN                   WD15                 30
055400971023     C  N30              TESTN                   WD15                   30
055500971023     C  N30              TESTN                   WD15                     30
055600971021     C                   MOVE      WD15          W001A             1
055700971021     C     *IN30         IFEQ      *OFF
055800971021     C     W001A         ORLT      '0'
055900971023     C     W001A         ANDNE     *BLANK
056000971021     C                   SETON                                        32
056100971021     C                   MOVE      *ZERO         VABCAS
056200010604     C                   eval      vinmsg = %trimr(vinmsg)
056300010604     C                             + ' ' + 'VHBCAS'
056400971021     C                   ELSE
056500971021     C                   MOVE      WD15          VABCAS
056600971021     C                   ENDIF
056700050118     C                   ENDIF
056800971021     C*
056900971023     C                   TESTN                   VHBRMN               30
057000971023     C  N30              TESTN                   VHBRMN                 30
057100971023     C  N30              TESTN                   VHBRMN                   30
057200971021     C                   MOVE      VHBRMN        W001A             1
057300971021     C     *IN30         IFEQ      *OFF
057400971021     C     W001A         ORLT      '0'
057500971021     C                   SETON                                        32
057600971021     C                   MOVE      *ZERO         VABRMN
057700010604     C                   eval      vinmsg = %trimr(vinmsg)
057800010604     C                             + ' ' + 'VHBRMN'
057900971021     C                   ELSE
058000971021     C                   MOVE      VHBRMN        VABRMN
058100971021     C                   ENDIF
058200971021     C*
058300971023     C                   TESTN                   VHBNCD               30
058400971023     C  N30              TESTN                   VHBNCD                 30
058500971023     C  N30              TESTN                   VHBNCD                   30
058600971021     C                   MOVE      VHBNCD        W001A             1
058700971021     C     *IN30         IFEQ      *OFF
058800971021     C     W001A         ORLT      '0'
058900971023     C     W001A         ANDNE     *BLANK
059000971021     C                   SETON                                        32
059100971021     C                   MOVE      *ZERO         VABNCD
059200010604     C                   eval      vinmsg = %trimr(vinmsg)
059300010604     C                             + ' ' + 'VHBNCD'
059400971021     C                   ELSE
059500971021     C                   MOVE      VHBNCD        VABNCD
059600971021     C                   ENDIF
059700971021     C*
059800971023     C                   TESTN                   VHBNCA               30
059900971023     C  N30              TESTN                   VHBNCA                 30
060000971023     C  N30              TESTN                   VHBNCA                   30
060100971021     C                   MOVE      VHBNCA        W001A             1
060200971021     C     *IN30         IFEQ      *OFF
060300971021     C     W001A         ORLT      '0'
060400971023     C     W001A         ANDNE     *BLANK
060500971021     C                   SETON                                        32
060600971021     C                   MOVE      *ZERO         VABNCA
060700010604     C                   eval      vinmsg = %trimr(vinmsg)
060800010604     C                             + ' ' + 'VHBNCA'
060900971021     C                   ELSE
061000971021     C                   MOVE      VHBNCA        VABNCA
061100971021     C                   ENDIF
061200971021     C*
061300971023     C                   TESTN                   VHBZNC               30
061400971023     C  N30              TESTN                   VHBZNC                 30
061500971023     C  N30              TESTN                   VHBZNC                   30
061600971021     C                   MOVE      VHBZNC        W001A             1
061700971021     C     *IN30         IFEQ      *OFF
061800971021     C     W001A         ORLT      '0'
061900971023     C     W001A         ANDNE     *BLANK
062000971021     C                   SETON                                        32
062100971021     C                   MOVE      *ZERO         VABZNC
062200010604     C                   eval      vinmsg = %trimr(vinmsg)
062300010604     C                             + ' ' + 'VHBZNC'
062400971021     C                   ELSE
062500971021     C                   MOVE      VHBZNC        VABZNC
062600971021     C                   ENDIF
062700971021     C*
062800971023     C                   TESTN                   VHBDCR               30
062900971023     C  N30              TESTN                   VHBDCR                 30
063000971023     C  N30              TESTN                   VHBDCR                   30
063100971021     C                   MOVE      VHBDCR        W001A             1
063200971021     C     *IN30         IFEQ      *OFF
063300971021     C     W001A         ORLT      '0'
063400971023     C     W001A         ANDNE     *BLANK
063500971021     C                   SETON                                        32
063600971021     C                   MOVE      *ZERO         VABDCR
063700010604     C                   eval      vinmsg = %trimr(vinmsg)
063800010604     C                             + ' ' + 'VHBDCR'
063900971021     C                   ELSE
064000971021     C                   MOVE      VHBDCR        VABDCR
064100971021     C                   ENDIF
064200971021     C*
064300971023     C                   TESTN                   VHBHCR               30
064400971023     C  N30              TESTN                   VHBHCR                 30
064500971023     C  N30              TESTN                   VHBHCR                   30
064600971021     C                   MOVE      VHBHCR        W001A             1
064700971021     C     *IN30         IFEQ      *OFF
064800971021     C     W001A         ORLT      '0'
064900971023     C     W001A         ANDNE     *BLANK
065000971021     C                   SETON                                        32
065100971021     C                   MOVE      *ZERO         VABHCR
065200010604     C                   eval      vinmsg = %trimr(vinmsg)
065300010604     C                             + ' ' + 'VHBHCR'
065400971021     C                   ELSE
065500971021     C                   MOVE      VHBHCR        VABHCR
065600971021     C                   ENDIF
065700941229     C*
065800971021     C                   MOVE      *ZERO         VABVMD
065900971021     C                   MOVE      VHBVMD        WASC15
066000010719     C                   IF        WI15 = *blanks
066100010719     C                   EVAL      WI15 = *zeros
066200010719     C                   ENDIF
066300971023     C                   TESTN                   WI15                 30
066400971023     C  N30              TESTN                   WI15                   30
066500971023     C  N30              TESTN                   WI15                     30
066600971021     C                   MOVE      WI15          W001A             1
066700971021     C     *IN30         IFEQ      *OFF
066800971021     C     W001A         ORLT      '0'
066900971023     C     W001A         ANDNE     *BLANK
067000971021     C                   SETON                                        32
067100971021     C                   MOVE      *ZERO         VABVMD
067200010604     C                   eval      vinmsg = %trimr(vinmsg)
067300010604     C                             + ' ' + 'VHBVMD'
067400971021     C                   ELSE
067500971021     C                   MOVEL     WI15          VABVMD
067600010719     C                   IF        WD15 = *blanks
067700010719     C                   EVAL      WD15 = *zeros
067800010719     C                   ENDIF
067900971023     C                   TESTN                   WD15                 30
068000971023     C  N30              TESTN                   WD15                   30
068100971023     C  N30              TESTN                   WD15                     30
068200971021     C                   MOVE      WD15          W001A             1
068300971021     C     *IN30         IFEQ      *OFF
068400971021     C     W001A         ORLT      '0'
068500971023     C     W001A         ANDNE     *BLANK
068600971021     C                   SETON                                        32
068700971021     C                   MOVE      *ZERO         VABVMD
068800010604     C                   eval      vinmsg = %trimr(vinmsg)
068900010604     C                             + ' ' + 'VHBVMD'
069000971021     C                   ELSE
069100971021     C                   MOVE      WD15          VABVMD
069200971021     C                   ENDIF
069300050118     C                   ENDIF
069400971021     C*
069500971023     C                   TESTN                   VHBANT               30
069600971023     C  N30              TESTN                   VHBANT                 30
069700971023     C  N30              TESTN                   VHBANT                   30
069800971021     C                   MOVE      VHBANT        W001A             1
069900971021     C     *IN30         IFEQ      *OFF
070000971021     C     W001A         ORLT      '0'
070100971023     C     W001A         ANDNE     *BLANK
070200971021     C                   SETON                                        32
070300971021     C                   MOVE      *ZERO         VABANT
070400010604     C                   eval      vinmsg = %trimr(vinmsg)
070500010604     C                             + ' ' + 'VHBANT'
070600971021     C                   ELSE
070700971021     C                   MOVE      VHBANT        VABANT
070800971021     C                   ENDIF
070900971021     C*
071000971021     C* CAMPI ALFABETICI:
071100971021     C                   MOVE      VHBATB        VABATB
071200971021     C                   MOVE      VHBCBO        VABCBO
071300971021     C                   MOVE      VHBRSD        VABRSD
071400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
071500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
071600020117     C* ==
071700971021     C                   MOVE      VHBRD2        VABRD2
071800971021     C                   MOVE      VHBIND        VABIND
071900971021     C                   MOVE      VHBCAD        VABCAD
072000971021     C                   MOVE      VHBLOD        VABLOD
072100971021     C                   MOVE      VHBPRD        VABPRD
072200971021     C                   MOVE      VHBNZD        VABNZD
072300971021     C                   MOVE      VHBGC1        VABGC1
072400971021     C                   MOVE      VHBGC2        VABGC2
072500971021     C                   MOVE      VHBTSP        VABTSP
072600971021     C                   MOVE      VHBVAS        VABVAS
072700971021     C                   MOVE      VHBNAS        VABNAS
072800971021     C                   MOVE      VHBTIC        VABTIC
072900971021     C                   MOVE      VHBVCA        VABVCA
073000971021     C                   MOVE      VHBGCA        VABGCA
073100971021     C                   MOVE      VHBRMA        VABRMA
073200971021     C                   MOVE      VHBXCO        VABXCO
073300971021     C                   MOVE      VHBNOT        VABNOT
073400971021     C                   MOVE      VHBNT2        VABNT2
073500971021     C                   MOVE      VHBCTM        VABCTM
073600971021     C                   MOVE      VHBFFD        VABFFD
073700971021     C                   MOVE      VHBTCR        VABTCR
073800971021     C                   MOVE      VHBCTS        VABCTS
073900971021     C                   MOVE      VHBFTM        VABFTM
074000971021     C                   MOVE      VHBVAD        VABVAD
074100971021     C                   MOVE      VHBGMA        VABGMA
074200971021     C                   MOVE      VHBGGA        VABGGA
074300971021     C                   MOVE      VHBGVA        VABGVA
074400971021     C                   MOVE      VHBTC1        VABTC1
074500971021     C                   MOVE      VHBTC2        VABTC2
074600971021     C                   MOVE      VHBSCL        VABSCL
074700971021     C                   MOVE      VHBRMO        VABRMO
074800971021     C                   MOVE      VHBCMO        VABCMO
074900971021     C                   MOVE      VHBNMO        VABNMO
075000971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
075100971021     C* NELLA NAZIONE ORIGINALE
075200971021     C                   MOVE      VHBNMO        WA1D              1
075300971021     C                   MOVEL     VHBNMO        WA1S              1
075400971021     C     WA1D          IFNE      *BLANK
075500971021     C     WA1S          ANDEQ     *BLANK
075600971021     C                   MOVE      *BLANK        VABNMO
075700030616     C                   ENDIF
075800030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
075900030710     C                   EVAL      VABCMR = wCMR
076000030616     C                   EVAL      VABDCM = DATCOR
076100030616     C                   EVAL      VABDTS = DATCOR
076200040402     C                   EVAL      VABHMS = ORACOR
076300030616     C                   EVAL      VABCNT = 1
076400041025     C*
076500041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
076600041025     C                   IF        *IN52 = *ON
076700041025     C* Reperisco la provincia dal CAP e dalla località
076800041025     C                   IF        VABCAD <> *blanks AND
076900050907     C                             VABPRD  = *blanks AND
077000050907     C                             VABNZD  = *blanks
077100041025     C                   CLEAR                   TISI95DS
077200041025     C                   EVAL      I95TCN = '3'
077300041025     C                   Z-ADD     datcor        I95DAT
077400041025     C                   EVAL      I95CAP = VABCAD
077500041025     C                   EVAL      I95LOC = VABLOD
077600041025     C                   CALL      'TISI95R'
077700041025     C                   PARM                    TISI95DS
077800041025     C                   EVAL      VABPRD = O95PRV
077900041025     C                   ENDIF
078000041025     C                   ENDIF
078100080605     C*
078200080605     C* Forzatura x personalizzazione CEVA LOGISTIC - TIM
078300100519     C                   if        *in54 = *on
078400080606     C                   eval      VABNOT = %subst(%editc(VABRMN:'X'):
078500080609     C                                      %len(VABRMN)-10+1)
078600080605     C                   eval      VABNT2 = *blanks
078700100519     C                   endif
078800100622     C*
078900100622     C* Forzatura x Electrolux => tipo incasso contrassegnio sempre 'OM' +
079000100622     C*                           mittente originale sempre 'ELECTROLUX'
079100100622     C                   if        VABCBO = '4' OR
079200100622     C                             VABCBO = '6'
079300100622     C                   eval      VABTIC = 'OM'
079400100622     C                   endif
079500100622     C                   eval      VABRMO = 'ELECTROLUX'
079600930409     C*
079700010604     C  N31              ADD       1             §CTROK            7 0
079800010604     C   32              ADD       1             §CTRMO            7 0
079900010604     C   31              ADD       1             §CTRNO            7 0
080000030616     C  N31              WRITE     EDIVAB00
080100990910     C*
080200010601     C                   if        *in31 = *off and
080300010601     C                             *in32 = *off
080400990910     C                   eval      vinflg = '1'
080500990910     C                   else
080600990910     C                   eval      vinflg = '2'
080700990910     C                   endif
080800991022     C                   endif
080900990910     C                   endif
081000010601     C*
081100010601     C                   endif
081200000905     C*
081300000905     C                   else
081400000905     C                   eval      vinflg = '1'
081500000905     C                   endif
081600000905     C*
081700000905     C  N70              update    tivin000
081800991022     C*
081900991022     C  N70              ENDdo
082000010530     C
082100010530     C*
082200010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
082300010530     C                   if        cntNonEl = *zeros or
082400010530     C                             flgMulti = '0'
082500990910     C* Se non ci sono record con errori ...
082600010530     C                   if        §ctrno = 0 and
082700010601     C                             §ctrmo = 0 and
082800010530     C                             flgStato <> '2'
082900990910     C* ... restituisco esito OK.
083000990921     C                   eval      wrkesito = '0'
083100990910     C                   else
083200010601     C                   if        §ctrok > 0
083300990921     C                   eval      wrkesito = '1'
083400000710     C                   else
083500010615     C                   if        flgOk = '0'
083600010615     C                   eval      wrkesito = '2'
083700010615     C                   else
083800010615     C                   eval      wrkesito = '6'
083900010615     C                   endif
084000990910     C                   endif
084100000710     C                   endif
084200010530     C                   else
084300010530     C                   eval      wrkesito = '9'
084400010530     C                   endif
084500030710     C*
084600030710 X  1C                   else
084700030710     C                   eval      wrkesito = '8'
084800030710     C                   eval      flgGiro = '1'
084900030710 E  1C                   endif
085000030710     C*
085100990914     C                   if        %open(tivin00r)
085200990908     C                   close     tivin00r
085300990914     C                   endif
085400030616     C                   if        %open(edivabwr)
085500030616     C                   close     edivabwr
085600990914     C                   endif
085700010530     C*
085800010530     C                   if        vlrpoi <> 999
085900010530     C                   eval      invfil = vlrpoi
086000010530     C                   endif
086100990910     C*
086200990920     C                   if        §ctrok > 0
086300010530     C                             and invfil > *zeros
086400000613     C                   exsr      invio
086500990920     C                   endif
086600010612     C*
086700010612     C                   if        flgGiro = '1'
086800010612     C                   exsr      endela
086900010612     C                   endif
087000990920     C*
087100910830     C                   ENDSR
087200000613     C***
087300990920
087400010530
087500010530
087600010530
087700010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
087800010530     C     repfil        BEGSR
087900010530     C*
088000010530     C                   if        invfil = *zeros and
088100010530     C                             depfil > *zeros and
088200010613     C                             (vinflg = *blanks or
088300010613     C                              vinflg = *zeros)
088400010530     C
088500010530     C                   eval      invfil = depfil
088600010530     C                   endif
088700010530     C*
088800010530     C                   if        depfil <> invfil and
088900010530     C                             invfil > *zeros
089000010530     C                   eval      flgMulti = '1'
089100010530     C                   if        vinflg = *blanks
089200010530     C                   add       1             cntNonEl
089300010530     C                   endif
089400010530     C                   endif
089500010530     C*
089600010530     C                   if        vinflg = '2'
089700010530     C                   eval      flgStato = '2'
089800010530     C                   endif
089900010530     C*
090000010530     C                   ENDSR
090100010530     C***
090200010530
090300010530
090400010530
090500010530
090600990920      /TITLE Invio dei dati al punto operativo.
090700000613     C     invio         BEGSR
090800990920     C*
090900990920     C                   reset                   dscmz
091000030618     C                   move      invfil        cmzdst
091100030618     C***                move      '789'         cmzdst
091200990920     C                   eval      cmzfld = vlrfou
091300990920     C                   eval      cmzmbd = vlrhdl
091400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
091500000710     C                   if        prmfir = *blanks
091600030616     C                   eval      cmzfla = 'EDIVAB0F'
091700030616     C                   eval      cmzmba = 'EDIVAB0F'
091800000710     C                   else
091900000710     C                   eval      cmzfla = prmfir
092000000710     C                   eval      cmzmba = prmfir
092100000710     C                   endif
092200990920     C                   eval      cmznrr = *zeros
092300990920     C                   move      §ctrok        cmznrr
092400021003     C                   eval      cmzlba = vlrfl1
092500990920     C                   call(e)   'TIS711C'
092600990920     C                   parm                    dscmz
092700990921     C                   parm      *blanks       esito
092800990920     C                   if        %error
092900990920     C                             or cmzerr = '1'
093000990921     C                             or esito  = '1'
093100000710     C                   eval      wrkesito = '3'
093200990920     C                   endif
093300990920     C*
093400000613     C                   ENDSR
093500000613     C***
093600010530
093700010530
093800010530
093900010530
094000010530      /TITLE Invio dei dati al punto operativo.
094100010530     C     opeini        BEGSR
094200010530     C*
094300010530     C* Inizializzo flag e contatori operativi
094400010530     C                   movel     '0'           flgGiro           1
094500010530     C                   movel     '0'           flgMulti          1
094600010530     C                   movel     '1'           flgStato          1
094700010615     C                   movel     '0'           flgOk             1
094800010530     C                   z-add     *zeros        cntNonEl         10 0
094900010530     C                   z-add     *zeros        depfil            3 0
095000010530     C                   z-add     *zeros        invfil            3 0
095100010530     C*
095200010530     C                   ENDSR
095300010530     C***
095400010530
095500010530
095600010530
095700990910
095800000613     C     *inzsr        BEGSR
095900990910     C*
096000990910     C     *entry        plist
096100990920     C                   parm                    tivlrds
096200990921     C                   parm      wrkesito      esito
096300000724     C                   parm                    prmlit
096400000710     C                   parm                    prmfir
096500030616     C*
096600030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
096700030710     C                   move(p)   vlrMSG        wNomeFile
096800030710     C*
096900030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
097000030710     C* del codice CMR dal nome del file importato.
097100030710     C                   if        wNomeFile <> *blanks AND
097200030710     C                             vlrppt<>*blanks
097300041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
097400030710     C                   if        %subst(vlrppt:1:1) = 'S'
097500030710     C                   seton                                        50
097600030710     C                   else
097700030710     C                   setoff                                       50
097800030710     C                   endif
097900041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
098000041001     C                   if        %subst(vlrppt:6:1) = 'S'
098100041001     C                   seton                                        51
098200041001     C                   else
098300041001     C                   setoff                                       51
098400041001     C                   endif
098500041025     C*
098600100519     C* Verifico se sui parametri del traduttore è richiesto il rerimento provincia
098700041025     C* (naturalmente solo se nn già presente)
098800041025     C                   if        %subst(vlrppt:7:1) = 'S'
098900041025     C                   seton                                        52
099000041025     C                   else
099100041025     C                   setoff                                       52
099200041025     C                   endif
099300100519     C*
099400100519     C* Verifico se sui parametri del traduttore è richiesto il trattamento "particolare"
099500100519     C* delle note
099600100519     C                   if        %subst(vlrppt:8:1) = 'S'
099700100519     C                   seton                                        54
099800100519     C                   else
099900100519     C                   setoff                                       54
100000100519     C                   endif
100100041001     C*
100200030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
100300030710     C                   move(p)   wPosDaA       wPosDa
100400030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
100500030710     C                   move(p)   wLungA        wLung
100600030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
100700030710     C                   seton                                        60
100800030710     C                   else
100900050914     C                   if        vlrisv = 'UL'
101000050914     C                   movel     datcor        wDataOraAlfa
101100050914     C                   move      oracor        wDataOraAlfa
101200050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
101300050914     C                   seton                                        60
101400050914     C                   else
101500030710     C                   setoff                                       60
101600050914     C                   endif
101700030710     C                   endif
101800010606     C*
101900010606     C* CALCOLA LA DATA CORRENTE
102000010606     C                   time                    wn14             14 0
102100010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
102200040402     C                   movel     wn14          oracor            6 0          *ORA
102300010606     C                   z-add     wn8           g08dat
102400010606     C                   z-add     *zeros        g08inv
102500010606     C                   movel     '0'           g08err
102600010606     C                   call      'XSRDA8'
102700010606     C                   parm                    wlbda8
102800010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
102900000613     C*
103000000613     C                   ENDSR
103100000613     C***
