000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200180309      *
000300180309      * PARTICOLARITA':
000400180309      *
000500180309      * 09/03/18
000600180309      * Questo pgm è il duplicata di TIS7TEB1R (che è uno standard super usato) perché:
000700180309      * dal 15/2/18 causa una modifica del cliente alle anagrafiche, riceviamo spesso
000800180309      * l'indirizzo non nel campo VABIND ma nell' estensione Ragione Sociale VABRD2.
000900180312      * Per cui il POC chiede che nel caso l'indirizzo sia '-', valorizzarlo con RD2 e svuotare
001000180309      * quest'ultima.
001100180309      * Il tutto va ben documentato perché, quando il cliente correggerà il suo sbaglio rimetteremo
001200180309      * il traduttore standard.
001300180309      *                          a
001400121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001500990908
001600990910     Ftivin00r  uF   E             DISK    usropn
001700030616     FEDIVABWR  O    E             DISK    usropn
001800990908
001900000512     D*------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300050118     D*
002400050118     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002500050118     D* 10+3
002600050118     D                 DS
002700050118     D  WI15                   1     10
002800050118     D  WD15                  11     13
002900050118     D  WASC15                 1     13
003000050118     D*  6+1
003100050118     D                 DS
003200050118     D  WI09                   1      6
003300050118     D  WD09                   7      7
003400050118     D  WASC09                 1      7
003500050118     D*  2+3
003600050118     D                 DS
003700050118     D  WI07                   1      2
003800050118     D  WD07                   3      5
003900050118     D  WASC07                 1      5
004000990920     D tivlrds       e ds                  extname(tivlr00f)
004100041025     D tisi95ds      e ds
004200990910     D esito           s              1
004300000724     D prmlit          s             10
004400000710     D prmfir          s             10
004500990921     D wrkesito        s                   like(esito)
004600000613     D rrnum           s              6  0 INZ(*zeros)
004700030616     D wNomeFile       s             30    INZ(*blanks)
004800030710     D wCMR            s             35    INZ(*blanks)
004900030710     D wPosDaA         s              2    INZ(*blanks)
005000030710     D wPosDa          s              2  0 INZ(*zeros)
005100030710     D wLungA          s              2    INZ(*blanks)
005200030710     D wLung           s              2  0 INZ(*zeros)
005300050914     D wDataOraAlfa    s             14    INZ(*blanks)
005400010606     D*------------------
005500010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005600010606     D*------------------
005700010606     D WLBDA8          DS                  INZ
005800010606     D  G08DAT                 1      8  0
005900010606     D  G08INV                 9     16  0
006000010606     D  G08ERR                17     17
006100010606     D  G08TGI                18     22  0
006200050118     D*---------------------------------------------------------
006300050118     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
006400050118     D*---------------------------------------------------------
006500050118     D                 ds
006600050118     D vhbatb                  1      1
006700050118     D vhbccm                  2      8
006800050118     D vhblnp                  9     11
006900050118     D vhbaas                 12     15
007000050118     D vhbmgs                 16     19
007100050118     D vhbnrs                 20     21
007200050118     D vhbnsp                 22     28
007300050118     D vhbcbo                 29     30
007400050118     D vhblna                 31     33
007500050118     D vhbrsd                 34     68
007600050118     D vhbrd2                 69    103
007700050118     D vhbind                104    138
007800050118     D vhbcad                139    147
007900050118     D vhblod                148    182
008000050118     D vhbprd                183    184
008100050118     D vhbnzd                185    187
008200050118     D vhbgc1                188    189
008300050118     D vhbgc2                190    191
008400050118     D vhbctr                192    194
008500050118     D vhbtsp                195    195
008600050118     D vhbias                196    208
008700050118     D vhbvas                209    211
008800050118     D vhbnas                212    226
008900050118     D vhbncl                227    231
009000050118     D vhbpkb                232    238
009100050118     D vhbvlb                239    243
009200050118     D vhbqft                244    256
009300050118     D vhbcas                257    269
009400050118     D vhbtic                270    271
009500050118     D vhbvca                272    274
009600050118     D vhbgca                275    276
009700050118     D vhbrmn                277    291
009800050118     D vhbrma                292    306
009900050118     D vhbncd                307    313
010000050118     D vhbnca                314    320
010100050118     D vhbxco                321    321
010200050118     D vhbnot                322    356
010300050118     D vhbnt2                357    391
010400050118     D vhbznc                392    393
010500050118     D vhbctm                394    395
010600050118     D vhbffd                396    396
010700050118     D vhbdcr                397    404
010800050118     D vhbtcr                405    405
010900050118     D vhbhcr                406    409
011000050118     D vhbcts                410    411
011100050118     D vhbftm                412    412
011200050118     D vhbvmd                413    425
011300050118     D vhbvad                426    428
011400050118     D vhbgma                429    430
011500050118     D vhbgga                431    432
011600050118     D vhbgva                433    434
011700050118     D vhbtc1                435    435
011800050118     D vhbtc2                436    436
011900050118     D vhbscl                437    437
012000050118     D vhbant                438    446
012100050118     D vhbrmo                447    471
012200050118     D vhbcmo                472    480
012300050118     D vhbnmo                481    483
012400050118     D  X_rec                  1    483    inz
012500010606     D*
012600071018     D*-------------------
012700071018     D* COSTANTI
012800071018     D*-------------------
012900071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
013000071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
013100990908
013200000913     C                   reset                   rrnum
013300990921     C                   reset                   esito
013400990921     C                   reset                   wrkesito
013500010530     C*
013600010530     C                   exsr      opeini
013700010530     C                   exsr      rwvab
013800041025     C*
013900041025     C* Esegue lancio TISI95R solo x chiusura
014000041025     C                   CLEAR                   TISI95DS
014100041025     C                   EVAL      I95TLA = 'C'
014200041025     C                   CALL      'TISI95R'
014300041025     C                   PARM                    TISI95DS
014400010530     C*
014500010530     C                   seton                                        lr
014600010530
014700010530
014800010530
014900010530
015000010530     C*--------------------------------------------------------
015100010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
015200010530     C*--------------------------------------------------------
015300010530     C     PREELA        BEGSR
015400000724     C*
015500000724     C* SE OCCORRE SPEDIRE IN FILIALE
015600010530     C                   if        invfil <> *zeros and
015700010530     C                             flgGiro = '0'
015800010530     C*
015900010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
016000010530     C                   eval      flgGiro = '1'
016100010530     C*
016200010530     C                   endif
016300010530     C*
016400010530     C                   ENDSR
016500010530     C***
016600010530
016700010530
016800010530
016900010530
017000010530
017100010530
017200010530     C*--------------------------------------------------------
017300010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017400010530     C*--------------------------------------------------------
017500010530     C     ENDELA        BEGSR
017600010530     C*
017700010530     C                   ENDSR
017800010530     C***
017900990908
018000010530
018100010530
018200010530
018300910830     C*--------------------------------------------------------
018400030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
018500910830     C*--------------------------------------------------------
018600941229     C     RWVAB         BEGSR
018700030710     C*
018800030710     C                   clear                   §CTROK
018900030710     C                   clear                   §CTRMO
019000030710     C                   clear                   §CTRNO
019100990910     C*
019200030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
019300030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
019400030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
019500030710     C                   if        *in60 = *on
019600990914     C                   if        not %open(tivin00r)
019700990908     C                   open      tivin00r
019800990914     C                   endif
019900030616     C                   if        not %open(edivabwr)
020000030616     C                   open      edivabwr
020100990914     C                   endif
020200990910     C*
020300921023     C                   DO        *HIVAL
020400990913     C*
020500990915     C                   READ      tivin00r                               70
020600071018     C*
020700071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
020800071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
020900010618     C*
021000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021100010618     C                   if        vinflg = '1'
021200010618     C                   eval      flgOk = '1'
021300010618     C                   endif
021400010705     C*
021500010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
021600010705     C                   if        %subst(vindta:1:1) = '='
021700010705     C                   eval      %subst(vindta:1:1) = *blanks
021800010705     C                   endif
021900010705     C*
022000000905     C                   if        vindta > *blanks
022100000613     C                   add       1             rrnum
022200010601     C* ...elimino eventuali segni
022300010601     C     '+':' '       xlate     vindta        vindta
022400990913     C*
022500010601     C                   if        *in70 = *off and
022600990913     C                             (vinflg = *blanks
022700010711     C                              or vinflg = '0'
022800010711     C                              or vinflg = '2')
022900990913     C*
023000030616     C                   clear                   EDIVAB00
023100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023200010711     C                   if        vinflg = *blanks or vinflg = '0'
023300010711     C                   clear                   vinmsg
023400010711     C                   endif
023500010711     C*
023600050118     C                   eval      x_rec  = vindta
023700991022     C*
023800991022     C                   testb     x'3F'         vhbatb                   02
023900991022     C                   if        not *in02
024000991022     C*
024100971021     C                   SETOFF                                       303132
024200971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
024300971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
024400971023     C*    OPPURE TUTTI BLANKS
024500010530     C*
024600010530     C                   TESTN                   VHBLNP               30
024700010530     C  N30              TESTN                   VHBLNP                 30
024800010530     C  N30              TESTN                   VHBLNP                   30
024900010530     C                   MOVE      VHBLNP        W001A             1
025000010530     C     *IN30         IFEQ      *OFF
025100010530     C     W001A         ORLT      '0'
025200010530     C                   SETON                                        31
025300010530     C                   eval      vinmsg = %trimr(vinmsg)
025400010530     C                             + ' ' + 'VHBLNP'
025500010530     C                   ELSE
025600010530     C                   MOVE      VHBLNP        VABLNP
025700010530     C     VABLNP        IFLE      *ZEROS
025800010530     C                   SETON                                        31
025900010530     C                   eval      vinmsg = %trimr(vinmsg)
026000010530     C                             + ' ' + 'VHBLNP'
026100010530     C                   ENDIF
026200010530     C                   ENDIF
026300010530     C*
026400010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026500010530     C                   eval      depfil = VABLNP
026600010530     C                   exsr      repfil
026700010601     C                   if        depfil = invfil
026800021014     C                   if        vlrpoi = 999
026900021010     C                   MOVE(P)   invfil        VABFGS
027000021014     C                   else
027100021014     C                   MOVE(P)   vlrpoi        VABFGS
027200021014     C                   endif
027300010530     C*
027400010530     C                   exsr      PREELA
027500010530     C*
027600971023     C                   TESTN                   VHBCCM               30
027700971023     C  N30              TESTN                   VHBCCM                 30
027800971023     C  N30              TESTN                   VHBCCM                   30
027900971021     C                   MOVE      VHBCCM        W001A             1
028000971021     C     *IN30         IFEQ      *OFF
028100971021     C     W001A         ORLT      '0'
028200971021     C                   SETON                                        31
028300010604     C                   eval      vinmsg = %trimr(vinmsg)
028400010604     C                             + ' ' + 'VHBCCM'
028500971021     C                   ELSE
028600971021     C                   MOVE      VHBCCM        VABCCM
028700971024     C     VABCCM        IFLE      *ZEROS
028800971024     C                   SETON                                        31
028900010604     C                   eval      vinmsg = %trimr(vinmsg)
029000010604     C                             + ' ' + 'VHBCCM'
029100971024     C                   ENDIF
029200971021     C                   ENDIF
029300971021     C*
029400971023     C                   TESTN                   VHBAAS               30
029500971023     C  N30              TESTN                   VHBAAS                 30
029600971023     C  N30              TESTN                   VHBAAS                   30
029700971021     C                   MOVE      VHBAAS        W001A             1
029800971021     C     *IN30         IFEQ      *OFF
029900971021     C     W001A         ORLT      '0'
030000971023     C     W001A         ANDNE     *BLANK
030100971021     C                   SETON                                        32
030200010606     C                   MOVEL     datcor        VABAAS
030300010604     C                   eval      vinmsg = %trimr(vinmsg)
030400010604     C                             + ' ' + 'VHBAAS'
030500971021     C                   ELSE
030600971021     C                   MOVE      VHBAAS        VABAAS
030700971021     C                   ENDIF
030800971021     C*
030900971023     C                   TESTN                   VHBMGS               30
031000971023     C  N30              TESTN                   VHBMGS                 30
031100971023     C  N30              TESTN                   VHBMGS                   30
031200971021     C                   MOVE      VHBMGS        W001A             1
031300971021     C     *IN30         IFEQ      *OFF
031400971021     C     W001A         ORLT      '0'
031500971023     C     W001A         ANDNE     *BLANK
031600971021     C                   SETON                                        32
031700010601     C                   MOVE      '0101'        VABMGS
031800010604     C                   eval      vinmsg = %trimr(vinmsg)
031900010604     C                             + ' ' + 'VHBMGS'
032000971021     C                   ELSE
032100971021     C                   MOVE      VHBMGS        VABMGS
032200971021     C                   ENDIF
032300971021     C*
032400971023     C                   TESTN                   VHBNRS               30
032500971023     C  N30              TESTN                   VHBNRS                 30
032600971023     C  N30              TESTN                   VHBNRS                   30
032700971021     C                   MOVE      VHBNRS        W001A             1
032800971021     C     *IN30         IFEQ      *OFF
032900971021     C     W001A         ORLT      '0'
033000971023     C     W001A         ANDNE     *BLANK
033100971021     C                   SETON                                        32
033200971021     C                   MOVE      *ZERO         VABNRS
033300010604     C                   eval      vinmsg = %trimr(vinmsg)
033400010604     C                             + ' ' + 'VHBNRS'
033500971021     C                   ELSE
033600971021     C                   MOVE      VHBNRS        VABNRS
033700971021     C                   ENDIF
033800971021     C*
033900971023     C                   TESTN                   VHBNSP               30
034000971023     C  N30              TESTN                   VHBNSP                 30
034100971023     C  N30              TESTN                   VHBNSP                   30
034200971021     C                   MOVE      VHBNSP        W001A             1
034300971021     C     *IN30         IFEQ      *OFF
034400971021     C     W001A         ORLT      '0'
034500971023     C     W001A         ANDNE     *BLANK
034600971021     C                   SETON                                        32
034700971021     C                   MOVE      *ZERO         VABNSP
034800010604     C                   eval      vinmsg = %trimr(vinmsg)
034900010604     C                             + ' ' + 'VHBNSP'
035000971021     C                   ELSE
035100971021     C                   MOVE      VHBNSP        VABNSP
035200971021     C                   ENDIF
035300971021     C*
035400971023     C                   TESTN                   VHBLNA               30
035500971023     C  N30              TESTN                   VHBLNA                 30
035600971023     C  N30              TESTN                   VHBLNA                   30
035700971021     C                   MOVE      VHBLNA        W001A             1
035800971021     C     *IN30         IFEQ      *OFF
035900971021     C     W001A         ORLT      '0'
036000971023     C     W001A         ANDNE     *BLANK
036100971021     C                   SETON                                        32
036200971021     C                   MOVE      *ZERO         VABLNA
036300010604     C                   eval      vinmsg = %trimr(vinmsg)
036400010604     C                             + ' ' + 'VHBLNA'
036500971021     C                   ELSE
036600971021     C                   MOVE      VHBLNA        VABLNA
036700971021     C                   ENDIF
036800971021     C*
036900971023     C                   TESTN                   VHBCTR               30
037000971023     C  N30              TESTN                   VHBCTR                 30
037100971023     C  N30              TESTN                   VHBCTR                   30
037200971021     C                   MOVE      VHBCTR        W001A             1
037300971021     C     *IN30         IFEQ      *OFF
037400971021     C     W001A         ORLT      '0'
037500971023     C     W001A         ANDNE     *BLANK
037600971021     C                   SETON                                        32
037700971021     C                   MOVE      *ZERO         VABCTR
037800010604     C                   eval      vinmsg = %trimr(vinmsg)
037900010604     C                             + ' ' + 'VHBCTR'
038000971021     C                   ELSE
038100971021     C                   MOVE      VHBCTR        VABCTR
038200971021     C                   ENDIF
038300941229     C*
038400971021     C                   MOVE      *ZERO         VABIAS
038500941229     C                   MOVE      VHBIAS        WASC15
038600971023     C                   TESTN                   WI15                 30
038700971023     C  N30              TESTN                   WI15                   30
038800971023     C  N30              TESTN                   WI15                     30
038900971021     C                   MOVE      WI15          W001A             1
039000971021     C     *IN30         IFEQ      *OFF
039100971021     C     W001A         ORLT      '0'
039200971023     C     W001A         ANDNE     *BLANK
039300971021     C                   SETON                                        32
039400971021     C                   MOVE      *ZERO         VABIAS
039500010604     C                   eval      vinmsg = %trimr(vinmsg)
039600010604     C                             + ' ' + 'VHBIAS'
039700971021     C                   ELSE
039800971021     C                   MOVEL     WI15          VABIAS
039900971023     C                   TESTN                   WD15                 30
040000971023     C  N30              TESTN                   WD15                   30
040100971023     C  N30              TESTN                   WD15                     30
040200971021     C                   MOVE      WD15          W001A             1
040300971021     C     *IN30         IFEQ      *OFF
040400971021     C     W001A         ORLT      '0'
040500971023     C     W001A         ANDNE     *BLANK
040600971021     C                   SETON                                        32
040700971021     C                   MOVE      *ZERO         VABIAS
040800010604     C                   eval      vinmsg = %trimr(vinmsg)
040900010604     C                             + ' ' + 'VHBIAS'
041000971021     C                   ELSE
041100971021     C                   MOVE      WD15          VABIAS
041200971021     C                   ENDIF
041300050118     C                   ENDIF
041400941229     C*
041500971023     C                   TESTN                   VHBNCL               30
041600971023     C  N30              TESTN                   VHBNCL                 30
041700971023     C  N30              TESTN                   VHBNCL                   30
041800971021     C                   MOVE      VHBNCL        W001A             1
041900971021     C     *IN30         IFEQ      *OFF
042000971021     C     W001A         ORLT      '0'
042100971021     C                   SETON                                        32
042200971021     C                   MOVE      *ZERO         VABNCL
042300010604     C                   eval      vinmsg = %trimr(vinmsg)
042400010604     C                             + ' ' + 'VHBNCL'
042500971021     C                   ELSE
042600971021     C                   MOVE      VHBNCL        VABNCL
042700971021     C                   ENDIF
042800941229     C*
042900971021     C                   MOVE      *ZERO         VABPKB
043000971021     C                   MOVE      VHBPKB        WASC09
043100010719     C                   IF        WI09 = *blanks
043200010719     C                   EVAL      WI09 = *zeros
043300010719     C                   ENDIF
043400971023     C                   TESTN                   WI09                 30
043500971023     C  N30              TESTN                   WI09                   30
043600971023     C  N30              TESTN                   WI09                     30
043700971021     C                   MOVE      WI09          W001A             1
043800971021     C     *IN30         IFEQ      *OFF
043900971021     C     W001A         ORLT      '0'
044000971021     C                   SETON                                        32
044100971021     C                   MOVE      *ZERO         VABPKB
044200010604     C                   eval      vinmsg = %trimr(vinmsg)
044300010604     C                             + ' ' + 'VHBPKB'
044400971021     C                   ELSE
044500971021     C                   MOVEL     WI09          VABPKB
044600010719     C                   IF        WD09 = *blanks
044700010719     C                   EVAL      WD09 = *zeros
044800010719     C                   ENDIF
044900971023     C                   TESTN                   WD09                 30
045000971023     C  N30              TESTN                   WD09                     30
045100971021     C                   MOVE      WD09          W001A             1
045200971021     C     *IN30         IFEQ      *OFF
045300971021     C     W001A         ORLT      '0'
045400971023     C     W001A         ANDNE     *BLANK
045500971021     C                   SETON                                        32
045600971021     C                   MOVE      *ZERO         VABPKB
045700010604     C                   eval      vinmsg = %trimr(vinmsg)
045800010604     C                             + ' ' + 'VHBPKB'
045900971021     C                   ELSE
046000971021     C                   MOVE      WD09          VABPKB
046100971021     C                   ENDIF
046200050118     C                   ENDIF
046300971021     C*
046400041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
046500041001     C                   IF        *IN51 = *OFF
046600971021     C                   MOVE      *ZERO         VABVLB
046700971021     C                   MOVE      VHBVLB        WASC07
046800010719     C                   IF        WI07 = *blanks
046900010719     C                   EVAL      WI07 = *zeros
047000010719     C                   ENDIF
047100971023     C                   TESTN                   WI07                 30
047200971023     C  N30              TESTN                   WI07                   30
047300971023     C  N30              TESTN                   WI07                     30
047400971021     C                   MOVE      WI07          W001A             1
047500971021     C     *IN30         IFEQ      *OFF
047600971021     C     W001A         ORLT      '0'
047700971023     C     W001A         ANDNE     *BLANK
047800971021     C                   SETON                                        32
047900971021     C                   MOVE      *ZERO         VABVLB
048000010604     C                   eval      vinmsg = %trimr(vinmsg)
048100010604     C                             + ' ' + 'VHBVLB'
048200971021     C                   ELSE
048300971021     C                   MOVEL     WI07          VABVLB
048400010719     C                   IF        WD07 = *blanks
048500010719     C                   EVAL      WD07 = *zeros
048600010719     C                   ENDIF
048700971023     C                   TESTN                   WD07                 30
048800971023     C  N30              TESTN                   WD07                   30
048900971023     C  N30              TESTN                   WD07                     30
049000971021     C                   MOVE      WD07          W001A             1
049100971021     C     *IN30         IFEQ      *OFF
049200971021     C     W001A         ORLT      '0'
049300971023     C     W001A         ANDNE     *BLANK
049400971021     C                   SETON                                        32
049500971021     C                   MOVE      *ZERO         VABVLB
049600010604     C                   eval      vinmsg = %trimr(vinmsg)
049700010604     C                             + ' ' + 'VHBVLB'
049800971021     C                   ELSE
049900971021     C                   MOVE      WD07          VABVLB
050000971021     C                   ENDIF
050100050118     C                   ENDIF
050200041001     C                   ELSE
050300041001     C* ...altrimenti forzo azzeramento del volume
050400041001     C                   Z-ADD     *zeros        VABVLB
050500041001     C                   ENDIF
050600941229     C*
050700971021     C                   MOVE      *ZERO         VABQFT
050800971021     C                   MOVE      VHBQFT        WASC15
050900010719     C                   IF        WI15 = *blanks
051000010719     C                   EVAL      WI15 = *zeros
051100010719     C                   ENDIF
051200971023     C                   TESTN                   WI15                 30
051300971023     C  N30              TESTN                   WI15                   30
051400971023     C  N30              TESTN                   WI15                     30
051500971021     C                   MOVE      WI15          W001A             1
051600971021     C     *IN30         IFEQ      *OFF
051700971021     C     W001A         ORLT      '0'
051800971023     C     W001A         ANDNE     *BLANK
051900971021     C                   SETON                                        32
052000971021     C                   MOVE      *ZERO         VABQFT
052100010604     C                   eval      vinmsg = %trimr(vinmsg)
052200010604     C                             + ' ' + 'VHBQFT'
052300971021     C                   ELSE
052400971021     C                   MOVEL     WI15          VABQFT
052500010719     C                   IF        WD15 = *blanks
052600010719     C                   EVAL      WD15 = *zeros
052700010719     C                   ENDIF
052800971023     C                   TESTN                   WD15                 30
052900971023     C  N30              TESTN                   WD15                   30
053000971023     C  N30              TESTN                   WD15                     30
053100971021     C                   MOVE      WD15          W001A             1
053200971021     C     *IN30         IFEQ      *OFF
053300971021     C     W001A         ORLT      '0'
053400971023     C     W001A         ANDNE     *BLANK
053500971021     C                   SETON                                        32
053600971021     C                   MOVE      *ZERO         VABQFT
053700010604     C                   eval      vinmsg = %trimr(vinmsg)
053800010604     C                             + ' ' + 'VHBQFT'
053900971021     C                   ELSE
054000971021     C                   MOVE      WD15          VABQFT
054100971021     C                   ENDIF
054200050118     C                   ENDIF
054300941229     C*
054400971021     C                   MOVE      *ZERO         VABCAS
054500971021     C                   MOVE      VHBCAS        WASC15
054600010719     C                   IF        WI15 = *blanks
054700010719     C                   EVAL      WI15 = *zeros
054800010719     C                   ENDIF
054900971023     C                   TESTN                   WI15                 30
055000971023     C  N30              TESTN                   WI15                   30
055100971023     C  N30              TESTN                   WI15                     30
055200971021     C                   MOVE      WI15          W001A             1
055300971021     C     *IN30         IFEQ      *OFF
055400971021     C     W001A         ORLT      '0'
055500971023     C     W001A         ANDNE     *BLANK
055600971021     C                   SETON                                        32
055700971021     C                   MOVE      *ZERO         VABCAS
055800010604     C                   eval      vinmsg = %trimr(vinmsg)
055900010604     C                             + ' ' + 'VHBCAS'
056000971021     C                   ELSE
056100971021     C                   MOVEL     WI15          VABCAS
056200010719     C                   IF        WD15 = *blanks
056300010719     C                   EVAL      WD15 = *zeros
056400010719     C                   ENDIF
056500971023     C                   TESTN                   WD15                 30
056600971023     C  N30              TESTN                   WD15                   30
056700971023     C  N30              TESTN                   WD15                     30
056800971021     C                   MOVE      WD15          W001A             1
056900971021     C     *IN30         IFEQ      *OFF
057000971021     C     W001A         ORLT      '0'
057100971023     C     W001A         ANDNE     *BLANK
057200971021     C                   SETON                                        32
057300971021     C                   MOVE      *ZERO         VABCAS
057400010604     C                   eval      vinmsg = %trimr(vinmsg)
057500010604     C                             + ' ' + 'VHBCAS'
057600971021     C                   ELSE
057700971021     C                   MOVE      WD15          VABCAS
057800971021     C                   ENDIF
057900050118     C                   ENDIF
058000971021     C*
058100971023     C                   TESTN                   VHBRMN               30
058200971023     C  N30              TESTN                   VHBRMN                 30
058300971023     C  N30              TESTN                   VHBRMN                   30
058400971021     C                   MOVE      VHBRMN        W001A             1
058500971021     C     *IN30         IFEQ      *OFF
058600971021     C     W001A         ORLT      '0'
058700971021     C                   SETON                                        32
058800971021     C                   MOVE      *ZERO         VABRMN
058900010604     C                   eval      vinmsg = %trimr(vinmsg)
059000010604     C                             + ' ' + 'VHBRMN'
059100971021     C                   ELSE
059200971021     C                   MOVE      VHBRMN        VABRMN
059300971021     C                   ENDIF
059400971021     C*
059500971023     C                   TESTN                   VHBNCD               30
059600971023     C  N30              TESTN                   VHBNCD                 30
059700971023     C  N30              TESTN                   VHBNCD                   30
059800971021     C                   MOVE      VHBNCD        W001A             1
059900971021     C     *IN30         IFEQ      *OFF
060000971021     C     W001A         ORLT      '0'
060100971023     C     W001A         ANDNE     *BLANK
060200971021     C                   SETON                                        32
060300971021     C                   MOVE      *ZERO         VABNCD
060400010604     C                   eval      vinmsg = %trimr(vinmsg)
060500010604     C                             + ' ' + 'VHBNCD'
060600971021     C                   ELSE
060700971021     C                   MOVE      VHBNCD        VABNCD
060800971021     C                   ENDIF
060900971021     C*
061000971023     C                   TESTN                   VHBNCA               30
061100971023     C  N30              TESTN                   VHBNCA                 30
061200971023     C  N30              TESTN                   VHBNCA                   30
061300971021     C                   MOVE      VHBNCA        W001A             1
061400971021     C     *IN30         IFEQ      *OFF
061500971021     C     W001A         ORLT      '0'
061600971023     C     W001A         ANDNE     *BLANK
061700971021     C                   SETON                                        32
061800971021     C                   MOVE      *ZERO         VABNCA
061900010604     C                   eval      vinmsg = %trimr(vinmsg)
062000010604     C                             + ' ' + 'VHBNCA'
062100971021     C                   ELSE
062200971021     C                   MOVE      VHBNCA        VABNCA
062300971021     C                   ENDIF
062400971021     C*
062500971023     C                   TESTN                   VHBZNC               30
062600971023     C  N30              TESTN                   VHBZNC                 30
062700971023     C  N30              TESTN                   VHBZNC                   30
062800971021     C                   MOVE      VHBZNC        W001A             1
062900971021     C     *IN30         IFEQ      *OFF
063000971021     C     W001A         ORLT      '0'
063100971023     C     W001A         ANDNE     *BLANK
063200971021     C                   SETON                                        32
063300971021     C                   MOVE      *ZERO         VABZNC
063400010604     C                   eval      vinmsg = %trimr(vinmsg)
063500010604     C                             + ' ' + 'VHBZNC'
063600971021     C                   ELSE
063700971021     C                   MOVE      VHBZNC        VABZNC
063800971021     C                   ENDIF
063900971021     C*
064000971023     C                   TESTN                   VHBDCR               30
064100971023     C  N30              TESTN                   VHBDCR                 30
064200971023     C  N30              TESTN                   VHBDCR                   30
064300971021     C                   MOVE      VHBDCR        W001A             1
064400971021     C     *IN30         IFEQ      *OFF
064500971021     C     W001A         ORLT      '0'
064600971023     C     W001A         ANDNE     *BLANK
064700971021     C                   SETON                                        32
064800971021     C                   MOVE      *ZERO         VABDCR
064900010604     C                   eval      vinmsg = %trimr(vinmsg)
065000010604     C                             + ' ' + 'VHBDCR'
065100971021     C                   ELSE
065200971021     C                   MOVE      VHBDCR        VABDCR
065300971021     C                   ENDIF
065400971021     C*
065500971023     C                   TESTN                   VHBHCR               30
065600971023     C  N30              TESTN                   VHBHCR                 30
065700971023     C  N30              TESTN                   VHBHCR                   30
065800971021     C                   MOVE      VHBHCR        W001A             1
065900971021     C     *IN30         IFEQ      *OFF
066000971021     C     W001A         ORLT      '0'
066100971023     C     W001A         ANDNE     *BLANK
066200971021     C                   SETON                                        32
066300971021     C                   MOVE      *ZERO         VABHCR
066400010604     C                   eval      vinmsg = %trimr(vinmsg)
066500010604     C                             + ' ' + 'VHBHCR'
066600971021     C                   ELSE
066700971021     C                   MOVE      VHBHCR        VABHCR
066800971021     C                   ENDIF
066900941229     C*
067000971021     C                   MOVE      *ZERO         VABVMD
067100971021     C                   MOVE      VHBVMD        WASC15
067200010719     C                   IF        WI15 = *blanks
067300010719     C                   EVAL      WI15 = *zeros
067400010719     C                   ENDIF
067500971023     C                   TESTN                   WI15                 30
067600971023     C  N30              TESTN                   WI15                   30
067700971023     C  N30              TESTN                   WI15                     30
067800971021     C                   MOVE      WI15          W001A             1
067900971021     C     *IN30         IFEQ      *OFF
068000971021     C     W001A         ORLT      '0'
068100971023     C     W001A         ANDNE     *BLANK
068200971021     C                   SETON                                        32
068300971021     C                   MOVE      *ZERO         VABVMD
068400010604     C                   eval      vinmsg = %trimr(vinmsg)
068500010604     C                             + ' ' + 'VHBVMD'
068600971021     C                   ELSE
068700971021     C                   MOVEL     WI15          VABVMD
068800010719     C                   IF        WD15 = *blanks
068900010719     C                   EVAL      WD15 = *zeros
069000010719     C                   ENDIF
069100971023     C                   TESTN                   WD15                 30
069200971023     C  N30              TESTN                   WD15                   30
069300971023     C  N30              TESTN                   WD15                     30
069400971021     C                   MOVE      WD15          W001A             1
069500971021     C     *IN30         IFEQ      *OFF
069600971021     C     W001A         ORLT      '0'
069700971023     C     W001A         ANDNE     *BLANK
069800971021     C                   SETON                                        32
069900971021     C                   MOVE      *ZERO         VABVMD
070000010604     C                   eval      vinmsg = %trimr(vinmsg)
070100010604     C                             + ' ' + 'VHBVMD'
070200971021     C                   ELSE
070300971021     C                   MOVE      WD15          VABVMD
070400971021     C                   ENDIF
070500050118     C                   ENDIF
070600971021     C*
070700971023     C                   TESTN                   VHBANT               30
070800971023     C  N30              TESTN                   VHBANT                 30
070900971023     C  N30              TESTN                   VHBANT                   30
071000971021     C                   MOVE      VHBANT        W001A             1
071100971021     C     *IN30         IFEQ      *OFF
071200971021     C     W001A         ORLT      '0'
071300971023     C     W001A         ANDNE     *BLANK
071400971021     C                   SETON                                        32
071500971021     C                   MOVE      *ZERO         VABANT
071600010604     C                   eval      vinmsg = %trimr(vinmsg)
071700010604     C                             + ' ' + 'VHBANT'
071800971021     C                   ELSE
071900971021     C                   MOVE      VHBANT        VABANT
072000971021     C                   ENDIF
072100971021     C*
072200971021     C* CAMPI ALFABETICI:
072300971021     C                   MOVE      VHBATB        VABATB
072400971021     C                   MOVE      VHBCBO        VABCBO
072500971021     C                   MOVE      VHBRSD        VABRSD
072600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
072700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
072800020117     C* ==
072900971021     C                   MOVE      VHBRD2        VABRD2
073000971021     C                   MOVE      VHBIND        VABIND
073100180309     C*
073200180309      * 09/03/18
073300180309      * Questo pgm è il duplicata di TIS7TEB1R (che è uno standard super usato) perché:
073400180309      * dal 15/2/18 causa una modifica del cliente alle anagrafiche, riceviamo spesso
073500180309      * l'indirizzo non nel campo VABIND ma nell' estensione Ragione Sociale VABRD2.
073600180312      * Per cui il POC chiede che nel caso l'indirizzo sia '-', valorizzarlo con RD2 e svuotare
073700180309      * quest'ultima.
073800180309      * Il tutto va ben documentato perché, quando il cliente correggerà il suo sbaglio rimetteremo
073900180309      * il traduttore standard.
074000180312     C                   IF        VABIND = '-'    and
074100180309     C                             VABRD2<> *blank
074200180309     C                   EVAL      VABIND = VABRD2
074300180309     C                   EVAL      VABRD2 = *blank
074400180309     C                   ENDIF
074500180309     C*
074600971021     C                   MOVE      VHBCAD        VABCAD
074700971021     C                   MOVE      VHBLOD        VABLOD
074800971021     C                   MOVE      VHBPRD        VABPRD
074900971021     C                   MOVE      VHBNZD        VABNZD
075000971021     C                   MOVE      VHBGC1        VABGC1
075100971021     C                   MOVE      VHBGC2        VABGC2
075200971021     C                   MOVE      VHBTSP        VABTSP
075300971021     C                   MOVE      VHBVAS        VABVAS
075400971021     C                   MOVE      VHBNAS        VABNAS
075500971021     C                   MOVE      VHBTIC        VABTIC
075600971021     C                   MOVE      VHBVCA        VABVCA
075700971021     C                   MOVE      VHBGCA        VABGCA
075800971021     C                   MOVE      VHBRMA        VABRMA
075900971021     C                   MOVE      VHBXCO        VABXCO
076000971021     C                   MOVE      VHBNOT        VABNOT
076100971021     C                   MOVE      VHBNT2        VABNT2
076200971021     C                   MOVE      VHBCTM        VABCTM
076300971021     C                   MOVE      VHBFFD        VABFFD
076400971021     C                   MOVE      VHBTCR        VABTCR
076500971021     C                   MOVE      VHBCTS        VABCTS
076600971021     C                   MOVE      VHBFTM        VABFTM
076700971021     C                   MOVE      VHBVAD        VABVAD
076800971021     C                   MOVE      VHBGMA        VABGMA
076900971021     C                   MOVE      VHBGGA        VABGGA
077000971021     C                   MOVE      VHBGVA        VABGVA
077100971021     C                   MOVE      VHBTC1        VABTC1
077200971021     C                   MOVE      VHBTC2        VABTC2
077300971021     C                   MOVE      VHBSCL        VABSCL
077400971021     C                   MOVE      VHBRMO        VABRMO
077500971021     C                   MOVE      VHBCMO        VABCMO
077600971021     C                   MOVE      VHBNMO        VABNMO
077700971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
077800971021     C* NELLA NAZIONE ORIGINALE
077900971021     C                   MOVE      VHBNMO        WA1D              1
078000971021     C                   MOVEL     VHBNMO        WA1S              1
078100971021     C     WA1D          IFNE      *BLANK
078200971021     C     WA1S          ANDEQ     *BLANK
078300971021     C                   MOVE      *BLANK        VABNMO
078400030616     C                   ENDIF
078500030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
078600030710     C                   EVAL      VABCMR = wCMR
078700030616     C                   EVAL      VABDCM = DATCOR
078800030616     C                   EVAL      VABDTS = DATCOR
078900040402     C                   EVAL      VABHMS = ORACOR
079000030616     C                   EVAL      VABCNT = 1
079100171003     C*
079200171003     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
079300171003     C                   IF        %trim(VABNZD) = 'I'   OR
079400171003     C                             %trim(VABNZD) = 'IT'  OR
079500171003     C                             %trim(VABNZD) = 'ITA'
079600171003     C                   EVAL      VABNZD = *blanks
079700171003     C                   ENDIF
079800041025     C*
079900041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
080000041025     C                   IF        *IN52 = *ON
080100041025     C* Reperisco la provincia dal CAP e dalla località
080200041025     C                   IF        VABCAD <> *blanks AND
080300050907     C                             VABPRD  = *blanks AND
080400050907     C                             VABNZD  = *blanks
080500041025     C                   CLEAR                   TISI95DS
080600041025     C                   EVAL      I95TCN = '3'
080700041025     C                   Z-ADD     datcor        I95DAT
080800041025     C                   EVAL      I95CAP = VABCAD
080900041025     C                   EVAL      I95LOC = VABLOD
081000041025     C                   CALL      'TISI95R'
081100041025     C                   PARM                    TISI95DS
081200041025     C                   EVAL      VABPRD = O95PRV
081300041025     C                   ENDIF
081400041025     C                   ENDIF
081500100419     C*
081600100419     C* Forzatura su campi DIVISE
081700100419     C                   IF        VABVAS = *blanks
081800100419     C                   EVAL      VABVAS = 'EUR'
081900100419     C                   ENDIF
082000100419     C                   IF        VABVCA = *blanks
082100100419     C                   EVAL      VABVCA = 'EUR'
082200100419     C                   ENDIF
082300100419     C                   IF        VABVAD = *blanks
082400100419     C                   EVAL      VABVAD = 'EUR'
082500100419     C                   ENDIF
082600930409     C*
082700010604     C  N31              ADD       1             §CTROK            7 0
082800010604     C   32              ADD       1             §CTRMO            7 0
082900010604     C   31              ADD       1             §CTRNO            7 0
083000030616     C  N31              WRITE     EDIVAB00
083100990910     C*
083200010601     C                   if        *in31 = *off and
083300010601     C                             *in32 = *off
083400990910     C                   eval      vinflg = '1'
083500990910     C                   else
083600990910     C                   eval      vinflg = '2'
083700990910     C                   endif
083800991022     C                   endif
083900990910     C                   endif
084000010601     C*
084100010601     C                   endif
084200000905     C*
084300000905     C                   else
084400000905     C                   eval      vinflg = '1'
084500000905     C                   endif
084600000905     C*
084700000905     C  N70              update    tivin000
084800991022     C*
084900991022     C  N70              ENDdo
085000010530     C
085100010530     C*
085200010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
085300010530     C                   if        cntNonEl = *zeros or
085400010530     C                             flgMulti = '0'
085500990910     C* Se non ci sono record con errori ...
085600010530     C                   if        §ctrno = 0 and
085700010601     C                             §ctrmo = 0 and
085800010530     C                             flgStato <> '2'
085900990910     C* ... restituisco esito OK.
086000990921     C                   eval      wrkesito = '0'
086100990910     C                   else
086200010601     C                   if        §ctrok > 0
086300990921     C                   eval      wrkesito = '1'
086400000710     C                   else
086500010615     C                   if        flgOk = '0'
086600010615     C                   eval      wrkesito = '2'
086700010615     C                   else
086800010615     C                   eval      wrkesito = '6'
086900010615     C                   endif
087000990910     C                   endif
087100000710     C                   endif
087200010530     C                   else
087300010530     C                   eval      wrkesito = '9'
087400010530     C                   endif
087500030710     C*
087600030710 X  1C                   else
087700030710     C                   eval      wrkesito = '8'
087800030710     C                   eval      flgGiro = '1'
087900030710 E  1C                   endif
088000030710     C*
088100990914     C                   if        %open(tivin00r)
088200990908     C                   close     tivin00r
088300990914     C                   endif
088400030616     C                   if        %open(edivabwr)
088500030616     C                   close     edivabwr
088600990914     C                   endif
088700010530     C*
088800010530     C                   if        vlrpoi <> 999
088900010530     C                   eval      invfil = vlrpoi
089000010530     C                   endif
089100990910     C*
089200990920     C                   if        §ctrok > 0
089300010530     C                             and invfil > *zeros
089400000613     C                   exsr      invio
089500990920     C                   endif
089600010612     C*
089700010612     C                   if        flgGiro = '1'
089800010612     C                   exsr      endela
089900010612     C                   endif
090000990920     C*
090100910830     C                   ENDSR
090200000613     C***
090300990920
090400010530
090500010530
090600010530
090700010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
090800010530     C     repfil        BEGSR
090900010530     C*
091000010530     C                   if        invfil = *zeros and
091100010530     C                             depfil > *zeros and
091200010613     C                             (vinflg = *blanks or
091300010613     C                              vinflg = *zeros)
091400010530     C
091500010530     C                   eval      invfil = depfil
091600010530     C                   endif
091700010530     C*
091800010530     C                   if        depfil <> invfil and
091900010530     C                             invfil > *zeros
092000010530     C                   eval      flgMulti = '1'
092100010530     C                   if        vinflg = *blanks
092200010530     C                   add       1             cntNonEl
092300010530     C                   endif
092400010530     C                   endif
092500010530     C*
092600010530     C                   if        vinflg = '2'
092700010530     C                   eval      flgStato = '2'
092800010530     C                   endif
092900010530     C*
093000010530     C                   ENDSR
093100010530     C***
093200010530
093300010530
093400010530
093500010530
093600990920      /TITLE Invio dei dati al punto operativo.
093700000613     C     invio         BEGSR
093800990920     C*
093900990920     C                   reset                   dscmz
094000030618     C                   move      invfil        cmzdst
094100030618     C***                move      '789'         cmzdst
094200990920     C                   eval      cmzfld = vlrfou
094300990920     C                   eval      cmzmbd = vlrhdl
094400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
094500000710     C                   if        prmfir = *blanks
094600030616     C                   eval      cmzfla = 'EDIVAB0F'
094700030616     C                   eval      cmzmba = 'EDIVAB0F'
094800000710     C                   else
094900000710     C                   eval      cmzfla = prmfir
095000000710     C                   eval      cmzmba = prmfir
095100000710     C                   endif
095200990920     C                   eval      cmznrr = *zeros
095300990920     C                   move      §ctrok        cmznrr
095400021003     C                   eval      cmzlba = vlrfl1
095500990920     C                   call(e)   'TIS711C'
095600990920     C                   parm                    dscmz
095700990921     C                   parm      *blanks       esito
095800990920     C                   if        %error
095900990920     C                             or cmzerr = '1'
096000990921     C                             or esito  = '1'
096100000710     C                   eval      wrkesito = '3'
096200990920     C                   endif
096300990920     C*
096400000613     C                   ENDSR
096500000613     C***
096600010530
096700010530
096800010530
096900010530
097000010530      /TITLE Invio dei dati al punto operativo.
097100010530     C     opeini        BEGSR
097200010530     C*
097300010530     C* Inizializzo flag e contatori operativi
097400010530     C                   movel     '0'           flgGiro           1
097500010530     C                   movel     '0'           flgMulti          1
097600010530     C                   movel     '1'           flgStato          1
097700010615     C                   movel     '0'           flgOk             1
097800010530     C                   z-add     *zeros        cntNonEl         10 0
097900010530     C                   z-add     *zeros        depfil            3 0
098000010530     C                   z-add     *zeros        invfil            3 0
098100010530     C*
098200010530     C                   ENDSR
098300010530     C***
098400010530
098500010530
098600010530
098700990910
098800000613     C     *inzsr        BEGSR
098900990910     C*
099000990910     C     *entry        plist
099100990920     C                   parm                    tivlrds
099200990921     C                   parm      wrkesito      esito
099300000724     C                   parm                    prmlit
099400000710     C                   parm                    prmfir
099500030616     C*
099600030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
099700030710     C                   move(p)   vlrMSG        wNomeFile
099800030710     C*
099900030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
100000030710     C* del codice CMR dal nome del file importato.
100100030710     C                   if        wNomeFile <> *blanks AND
100200030710     C                             vlrppt<>*blanks
100300041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
100400030710     C                   if        %subst(vlrppt:1:1) = 'S'
100500030710     C                   seton                                        50
100600030710     C                   else
100700030710     C                   setoff                                       50
100800030710     C                   endif
100900041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
101000041001     C                   if        %subst(vlrppt:6:1) = 'S'
101100041001     C                   seton                                        51
101200041001     C                   else
101300041001     C                   setoff                                       51
101400041001     C                   endif
101500041025     C*
101600041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
101700041025     C* (naturalmente solo se nn già presente)
101800041025     C                   if        %subst(vlrppt:7:1) = 'S'
101900041025     C                   seton                                        52
102000041025     C                   else
102100041025     C                   setoff                                       52
102200041025     C                   endif
102300041001     C*
102400030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
102500030710     C                   move(p)   wPosDaA       wPosDa
102600030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
102700030710     C                   move(p)   wLungA        wLung
102800030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
102900030710     C                   seton                                        60
103000030710     C                   else
103100050914     C                   if        vlrisv = 'UL'
103200050914     C                   movel     datcor        wDataOraAlfa
103300050914     C                   move      oracor        wDataOraAlfa
103400050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
103500050914     C                   seton                                        60
103600050914     C                   else
103700030710     C                   setoff                                       60
103800050914     C                   endif
103900030710     C                   endif
104000010606     C*
104100010606     C* CALCOLA LA DATA CORRENTE
104200010606     C                   time                    wn14             14 0
104300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
104400040402     C                   movel     wn14          oracor            6 0          *ORA
104500010606     C                   z-add     wn8           g08dat
104600010606     C                   z-add     *zeros        g08inv
104700010606     C                   movel     '0'           g08err
104800010606     C                   call      'XSRDA8'
104900010606     C                   parm                    wlbda8
105000010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
105100000613     C*
105200000613     C                   ENDSR
