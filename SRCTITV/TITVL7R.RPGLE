000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500170807     FEDIVABWR  O    E             DISK
000600050613     FEDIVADWR  O    E             DISK    usropn
000700050618     FEDIVATWR  O    E             DISK    usropn
000800990908
000900000512     D*------------
001000990920     D dscmz         e ds                  inz
001100990910     D psds           sds
001200990910     D  procname         *PROC
001300050118     D*
001400050118     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001500050118     D* 10+3
001600050118     D                 DS
001700050118     D  WI15                   1     10
001800050118     D  WD15                  11     13
001900050118     D  WASC15                 1     13
002000050118     D*  6+1
002100050118     D                 DS
002200050118     D  WI09                   1      6
002300050118     D  WD09                   7      7
002400050118     D  WASC09                 1      7
002500050118     D*  2+3
002600050118     D                 DS
002700050118     D  WI07                   1      2
002800050118     D  WD07                   3      5
002900050118     D  WASC07                 1      5
003000050618     D*
003100050618     D fnvataar        ds                  inz
003200050618     D  a_vatatb                      1
003300050618     D  a_vatccm                      7
003400050618     D  a_vatlnp                      3
003500050618     D  a_vataas                      4
003600050618     D  a_vatnrs                      2
003700050618     D  a_vatnsp                      7
003800050618     D  a_vattrc                      1
003900050618     D  a_vatnot                     35
004000990920     D tivlrds       e ds                  extname(tivlr00f)
004100041025     D tisi95ds      e ds
004200990910     D esito           s              1
004300000724     D prmlit          s             10
004400000710     D prmfir          s             10
004500990921     D wrkesito        s                   like(esito)
004600000613     D rrnum           s              6  0 INZ(*zeros)
004700050613     D parccm          s              8    INZ(*blanks)
004800050613     D parmbr          s             10    INZ(*blanks)
004900050613     D paropz          s              1    INZ(*blanks)
005000050613     D chkcall         s              1    INZ(*blanks)
005100030616     D wNomeFile       s             30    INZ(*blanks)
005200030710     D wCMR            s             35    INZ(*blanks)
005300030710     D wPosDaA         s              2    INZ(*blanks)
005400030710     D wPosDa          s              2  0 INZ(*zeros)
005500030710     D wLungA          s              2    INZ(*blanks)
005600030710     D wLung           s              2  0 INZ(*zeros)
005700010606     D*------------------
005800010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005900010606     D*------------------
006000010606     D WLBDA8          DS                  INZ
006100010606     D  G08DAT                 1      8  0
006200010606     D  G08INV                 9     16  0
006300010606     D  G08ERR                17     17
006400010606     D  G08TGI                18     22  0
006500050118     D*---------------------------------------------------------
006600050118     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
006700050118     D*---------------------------------------------------------
006800050118     D                 ds
006900050118     D vhbatb                  1      1
007000050118     D vhbccm                  2      8
007100050118     D vhblnp                  9     11
007200050118     D vhbaas                 12     15
007300050118     D vhbmgs                 16     19
007400050118     D vhbnrs                 20     21
007500050118     D vhbnsp                 22     28
007600050118     D vhbcbo                 29     30
007700050118     D vhblna                 31     33
007800050118     D vhbrsd                 34     68
007900050118     D vhbrd2                 69    103
008000050118     D vhbind                104    138
008100050118     D vhbcad                139    147
008200050118     D vhblod                148    182
008300050118     D vhbprd                183    184
008400050118     D vhbnzd                185    187
008500050118     D vhbgc1                188    189
008600050118     D vhbgc2                190    191
008700050118     D vhbctr                192    194
008800050118     D vhbtsp                195    195
008900050118     D vhbias                196    208
009000050118     D vhbvas                209    211
009100050118     D vhbnas                212    226
009200050118     D vhbncl                227    231
009300050118     D vhbpkb                232    238
009400050118     D vhbvlb                239    243
009500050118     D vhbqft                244    256
009600050118     D vhbcas                257    269
009700050118     D vhbtic                270    271
009800050118     D vhbvca                272    274
009900050118     D vhbgca                275    276
010000050118     D vhbrmn                277    291
010100050118     D vhbrma                292    306
010200050118     D vhbncd                307    313
010300050118     D vhbnca                314    320
010400050118     D vhbxco                321    321
010500050118     D vhbnot                322    356
010600050118     D vhbnt2                357    391
010700050118     D vhbznc                392    393
010800050118     D vhbctm                394    395
010900050118     D vhbffd                396    396
011000050118     D vhbdcr                397    404
011100050118     D vhbtcr                405    405
011200050118     D vhbhcr                406    409
011300050118     D vhbcts                410    411
011400050118     D vhbftm                412    412
011500050118     D vhbvmd                413    425
011600050118     D vhbvad                426    428
011700050118     D vhbgma                429    430
011800050118     D vhbgga                431    432
011900050118     D vhbgva                433    434
012000050118     D vhbtc1                435    435
012100050118     D vhbtc2                436    436
012200050118     D vhbscl                437    437
012300050118     D vhbant                438    446
012400050118     D vhbrmo                447    471
012500050118     D vhbcmo                472    480
012600050118     D vhbnmo                481    483
012700050118     D  X_rec                  1    483    inz
012800010606     D*
012900990908
013000000913     C                   reset                   rrnum
013100990921     C                   reset                   esito
013200990921     C                   reset                   wrkesito
013300010530     C*
013400010530     C                   exsr      opeini
013500010530     C                   exsr      rwvab
013600081125     C*
013700081125     C                   exsr      endela
013800041025     C*
013900041025     C* Esegue lancio TISI95R solo x chiusura
014000041025     C                   CLEAR                   TISI95DS
014100041025     C                   EVAL      I95TLA = 'C'
014200041025     C                   CALL      'TISI95R'
014300041025     C                   PARM                    TISI95DS
014400050613     C*
014500050613     C* Effettuo la chiamata al CLLE preposto
014600050618     C                   call(e)   'TITVEVDTC'
014700050613     C                   parm                    parccm
014800050613     C                   parm                    parmbr
014900050613     C                   parm      '2'           paropz
015000010530     C*
015100010530     C                   seton                                        lr
015200010530
015300010530
015400010530
015500010530
015600010530     C*--------------------------------------------------------
015700010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
015800010530     C*--------------------------------------------------------
015900010530     C     PREELA        BEGSR
016000000724     C*
016100000724     C* SE OCCORRE SPEDIRE IN FILIALE
016200010530     C                   if        invfil <> *zeros and
016300010530     C                             flgGiro = '0'
016400010530     C*
016500010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
016600010530     C                   eval      flgGiro = '1'
016700010530     C*
016800010530     C                   endif
016900010530     C*
017000010530     C                   ENDSR
017100010530     C***
017200010530
017300010530
017400010530
017500010530
017600010530
017700010530
017800010530     C*--------------------------------------------------------
017900010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
018000010530     C*--------------------------------------------------------
018100010530     C     ENDELA        BEGSR
018200080930     C*
018300080930     C                   if        %open(tivin00r)
018400080930     C                   close     tivin00r
018500080930     C                   endif
018600170807     C***                if        %open(edivabwr)
018700170807     C***                close     edivabwr
018800170807     C***                endif
018900080930     C                   if        %open(edivadwr)
019000080930     C                   close     edivadwr
019100080930     C                   endif
019200080930     C                   if        %open(edivatwr)
019300080930     C                   close     edivatwr
019400080930     C                   endif
019500010530     C*
019600010530     C                   ENDSR
019700010530     C***
019800990908
019900010530
020000010530
020100010530
020200910830     C*--------------------------------------------------------
020300030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
020400910830     C*--------------------------------------------------------
020500941229     C     RWVAB         BEGSR
020600030710     C*
020700030710     C                   clear                   §CTROK
020800030710     C                   clear                   §CTRMO
020900030710     C                   clear                   §CTRNO
021000990910     C*
021100030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
021200030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
021300030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
021400030710     C                   if        *in60 = *on
021500990914     C                   if        not %open(tivin00r)
021600990908     C                   open      tivin00r
021700990914     C                   endif
021800170807     C***                if        not %open(edivabwr)
021900170807     C***                open      edivabwr
022000170807     C***                endif
022100050613     C*
022200050618     C* Eseguo operazioni di aggiunta nuovo membro in EDIVADWR/EDIVATWR
022300050613     C                   exsr      prevad
022400050613     C*
022500050613     C                   if        chkcall = '0'
022600050613     C*
022700050613     C                   if        not %open(edivadwr)
022800050613     C                   open      edivadwr
022900050613     C                   endif
023000050618     C                   if        not %open(edivatwr)
023100050618     C                   open      edivatwr
023200050618     C                   endif
023300050613     C*
023400050613     C* Inizializzo il flag d wrk che pilota lo scarico dei buffer testate
023500050613     C                   movel     '0'           wGiroVAB          1
023600990910     C*
023700921023     C                   DO        *HIVAL
023800990913     C*
023900990915     C                   READ      tivin00r                               70
024000010618     C*
024100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024200010618     C                   if        vinflg = '1'
024300010618     C                   eval      flgOk = '1'
024400010618     C                   endif
024500010705     C*
024600010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
024700010705     C                   if        %subst(vindta:1:1) = '='
024800010705     C                   eval      %subst(vindta:1:1) = *blanks
024900010705     C                   endif
025000010705     C*
025100000905     C                   if        vindta > *blanks
025200000613     C                   add       1             rrnum
025300010601     C* ...elimino eventuali segni
025400010601     C     '+':' '       xlate     vindta        vindta
025500990913     C*
025600010601     C                   if        *in70 = *off and
025700990913     C                             (vinflg = *blanks
025800010711     C                              or vinflg = '0'
025900010711     C                              or vinflg = '2')
026000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026100010711     C                   if        vinflg = *blanks or vinflg = '0'
026200010711     C                   clear                   vinmsg
026300010711     C                   endif
026400050708     C* Inizializzo gli indicatori d errori traduzione su tutti i file d output (VAB/VAD/VAT)
026500050708     C                   SETOFF                                       3132      * VAB
026600050708     C                   SETOFF                                       3334      * VAD
026700050708     C                   SETOFF                                       3536      * VAT
026800050613     C*
026900050613     C* In questo caso il flag d annullamento (ATB) identifica il tipo file:
027000050613     C* 1 = VAB
027100050613     C* 2 = VAD
027200050618     C* 3 = VAT
027300050613     C                   if        %subst(vindta:1:1) = '1'
027400050613     C*
027500050613     C* Se trattasi NON del primo record d tipo '1' => devo scaricare la testata precedente
027600050613     C                   if        wGiroVAB = '1'
027700050613     C* SCARICO IL BUFFER DEL RECORD D TESTATA
027800050613     C  N31              ADD       1             §CTROK            7 0
027900050613     C   32              ADD       1             §CTRMO            7 0
028000050613     C   31              ADD       1             §CTRNO            7 0
028100050613     C  N31              WRITE     EDIVAB00
028200050613     C                   else
028300050613     C                   movel     '1'           wGiroVAB
028400050613     C                   endif
028500050613     C*
028600050613     C                   clear                   EDIVAB00
028700050613     C                   eval      x_rec  = vindta
028800991022     C*
028900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
029000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
029100971023     C*    OPPURE TUTTI BLANKS
029200010530     C*
029300010530     C                   TESTN                   VHBLNP               30
029400010530     C  N30              TESTN                   VHBLNP                 30
029500010530     C  N30              TESTN                   VHBLNP                   30
029600010530     C                   MOVE      VHBLNP        W001A             1
029700010530     C     *IN30         IFEQ      *OFF
029800010530     C     W001A         ORLT      '0'
029900010530     C                   SETON                                        31
030000010530     C                   eval      vinmsg = %trimr(vinmsg)
030100010530     C                             + ' ' + 'VHBLNP'
030200010530     C                   ELSE
030300010530     C                   MOVE      VHBLNP        VABLNP
030400010530     C     VABLNP        IFLE      *ZEROS
030500010530     C                   SETON                                        31
030600010530     C                   eval      vinmsg = %trimr(vinmsg)
030700010530     C                             + ' ' + 'VHBLNP'
030800010530     C                   ENDIF
030900010530     C                   ENDIF
031000010530     C*
031100010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
031200010530     C                   eval      depfil = VABLNP
031300010530     C                   exsr      repfil
031400010601     C                   if        depfil = invfil
031500021014     C                   if        vlrpoi = 999
031600021010     C                   MOVE(P)   invfil        VABFGS
031700021014     C                   else
031800021014     C                   MOVE(P)   vlrpoi        VABFGS
031900021014     C                   endif
032000050613     C                   endif
032100010530     C*
032200010530     C                   exsr      PREELA
032300010530     C*
032400971023     C                   TESTN                   VHBCCM               30
032500971023     C  N30              TESTN                   VHBCCM                 30
032600971023     C  N30              TESTN                   VHBCCM                   30
032700971021     C                   MOVE      VHBCCM        W001A             1
032800971021     C     *IN30         IFEQ      *OFF
032900971021     C     W001A         ORLT      '0'
033000971021     C                   SETON                                        31
033100010604     C                   eval      vinmsg = %trimr(vinmsg)
033200010604     C                             + ' ' + 'VHBCCM'
033300971021     C                   ELSE
033400971021     C                   MOVE      VHBCCM        VABCCM
033500971024     C     VABCCM        IFLE      *ZEROS
033600971024     C                   SETON                                        31
033700010604     C                   eval      vinmsg = %trimr(vinmsg)
033800010604     C                             + ' ' + 'VHBCCM'
033900971024     C                   ENDIF
034000971021     C                   ENDIF
034100971021     C*
034200971023     C                   TESTN                   VHBAAS               30
034300971023     C  N30              TESTN                   VHBAAS                 30
034400971023     C  N30              TESTN                   VHBAAS                   30
034500971021     C                   MOVE      VHBAAS        W001A             1
034600971021     C     *IN30         IFEQ      *OFF
034700971021     C     W001A         ORLT      '0'
034800971023     C     W001A         ANDNE     *BLANK
034900971021     C                   SETON                                        32
035000010606     C                   MOVEL     datcor        VABAAS
035100010604     C                   eval      vinmsg = %trimr(vinmsg)
035200010604     C                             + ' ' + 'VHBAAS'
035300971021     C                   ELSE
035400971021     C                   MOVE      VHBAAS        VABAAS
035500971021     C                   ENDIF
035600971021     C*
035700971023     C                   TESTN                   VHBMGS               30
035800971023     C  N30              TESTN                   VHBMGS                 30
035900971023     C  N30              TESTN                   VHBMGS                   30
036000971021     C                   MOVE      VHBMGS        W001A             1
036100971021     C     *IN30         IFEQ      *OFF
036200971021     C     W001A         ORLT      '0'
036300971023     C     W001A         ANDNE     *BLANK
036400971021     C                   SETON                                        32
036500010601     C                   MOVE      '0101'        VABMGS
036600010604     C                   eval      vinmsg = %trimr(vinmsg)
036700010604     C                             + ' ' + 'VHBMGS'
036800971021     C                   ELSE
036900971021     C                   MOVE      VHBMGS        VABMGS
037000971021     C                   ENDIF
037100971021     C*
037200971023     C                   TESTN                   VHBNRS               30
037300971023     C  N30              TESTN                   VHBNRS                 30
037400971023     C  N30              TESTN                   VHBNRS                   30
037500971021     C                   MOVE      VHBNRS        W001A             1
037600971021     C     *IN30         IFEQ      *OFF
037700971021     C     W001A         ORLT      '0'
037800971023     C     W001A         ANDNE     *BLANK
037900971021     C                   SETON                                        32
038000971021     C                   MOVE      *ZERO         VABNRS
038100010604     C                   eval      vinmsg = %trimr(vinmsg)
038200010604     C                             + ' ' + 'VHBNRS'
038300971021     C                   ELSE
038400971021     C                   MOVE      VHBNRS        VABNRS
038500971021     C                   ENDIF
038600971021     C*
038700971023     C                   TESTN                   VHBNSP               30
038800971023     C  N30              TESTN                   VHBNSP                 30
038900971023     C  N30              TESTN                   VHBNSP                   30
039000971021     C                   MOVE      VHBNSP        W001A             1
039100971021     C     *IN30         IFEQ      *OFF
039200971021     C     W001A         ORLT      '0'
039300971023     C     W001A         ANDNE     *BLANK
039400971021     C                   SETON                                        32
039500971021     C                   MOVE      *ZERO         VABNSP
039600010604     C                   eval      vinmsg = %trimr(vinmsg)
039700010604     C                             + ' ' + 'VHBNSP'
039800971021     C                   ELSE
039900971021     C                   MOVE      VHBNSP        VABNSP
040000971021     C                   ENDIF
040100971021     C*
040200971023     C                   TESTN                   VHBLNA               30
040300971023     C  N30              TESTN                   VHBLNA                 30
040400971023     C  N30              TESTN                   VHBLNA                   30
040500971021     C                   MOVE      VHBLNA        W001A             1
040600971021     C     *IN30         IFEQ      *OFF
040700971021     C     W001A         ORLT      '0'
040800971023     C     W001A         ANDNE     *BLANK
040900971021     C                   SETON                                        32
041000971021     C                   MOVE      *ZERO         VABLNA
041100010604     C                   eval      vinmsg = %trimr(vinmsg)
041200010604     C                             + ' ' + 'VHBLNA'
041300971021     C                   ELSE
041400971021     C                   MOVE      VHBLNA        VABLNA
041500971021     C                   ENDIF
041600971021     C*
041700971023     C                   TESTN                   VHBCTR               30
041800971023     C  N30              TESTN                   VHBCTR                 30
041900971023     C  N30              TESTN                   VHBCTR                   30
042000971021     C                   MOVE      VHBCTR        W001A             1
042100971021     C     *IN30         IFEQ      *OFF
042200971021     C     W001A         ORLT      '0'
042300971023     C     W001A         ANDNE     *BLANK
042400971021     C                   SETON                                        32
042500971021     C                   MOVE      *ZERO         VABCTR
042600010604     C                   eval      vinmsg = %trimr(vinmsg)
042700010604     C                             + ' ' + 'VHBCTR'
042800971021     C                   ELSE
042900971021     C                   MOVE      VHBCTR        VABCTR
043000971021     C                   ENDIF
043100941229     C*
043200971021     C                   MOVE      *ZERO         VABIAS
043300941229     C                   MOVE      VHBIAS        WASC15
043400971023     C                   TESTN                   WI15                 30
043500971023     C  N30              TESTN                   WI15                   30
043600971023     C  N30              TESTN                   WI15                     30
043700971021     C                   MOVE      WI15          W001A             1
043800971021     C     *IN30         IFEQ      *OFF
043900971021     C     W001A         ORLT      '0'
044000971023     C     W001A         ANDNE     *BLANK
044100971021     C                   SETON                                        32
044200971021     C                   MOVE      *ZERO         VABIAS
044300010604     C                   eval      vinmsg = %trimr(vinmsg)
044400010604     C                             + ' ' + 'VHBIAS'
044500971021     C                   ELSE
044600971021     C                   MOVEL     WI15          VABIAS
044700971023     C                   TESTN                   WD15                 30
044800971023     C  N30              TESTN                   WD15                   30
044900971023     C  N30              TESTN                   WD15                     30
045000971021     C                   MOVE      WD15          W001A             1
045100971021     C     *IN30         IFEQ      *OFF
045200971021     C     W001A         ORLT      '0'
045300971023     C     W001A         ANDNE     *BLANK
045400971021     C                   SETON                                        32
045500971021     C                   MOVE      *ZERO         VABIAS
045600010604     C                   eval      vinmsg = %trimr(vinmsg)
045700010604     C                             + ' ' + 'VHBIAS'
045800971021     C                   ELSE
045900971021     C                   MOVE      WD15          VABIAS
046000971021     C                   ENDIF
046100050118     C                   ENDIF
046200941229     C*
046300971023     C                   TESTN                   VHBNCL               30
046400971023     C  N30              TESTN                   VHBNCL                 30
046500971023     C  N30              TESTN                   VHBNCL                   30
046600971021     C                   MOVE      VHBNCL        W001A             1
046700971021     C     *IN30         IFEQ      *OFF
046800971021     C     W001A         ORLT      '0'
046900971021     C                   SETON                                        32
047000971021     C                   MOVE      *ZERO         VABNCL
047100010604     C                   eval      vinmsg = %trimr(vinmsg)
047200010604     C                             + ' ' + 'VHBNCL'
047300971021     C                   ELSE
047400971021     C                   MOVE      VHBNCL        VABNCL
047500971021     C                   ENDIF
047600941229     C*
047700971021     C                   MOVE      *ZERO         VABPKB
047800971021     C                   MOVE      VHBPKB        WASC09
047900010719     C                   IF        WI09 = *blanks
048000010719     C                   EVAL      WI09 = *zeros
048100010719     C                   ENDIF
048200971023     C                   TESTN                   WI09                 30
048300971023     C  N30              TESTN                   WI09                   30
048400971023     C  N30              TESTN                   WI09                     30
048500971021     C                   MOVE      WI09          W001A             1
048600971021     C     *IN30         IFEQ      *OFF
048700971021     C     W001A         ORLT      '0'
048800971021     C                   SETON                                        32
048900971021     C                   MOVE      *ZERO         VABPKB
049000010604     C                   eval      vinmsg = %trimr(vinmsg)
049100010604     C                             + ' ' + 'VHBPKB'
049200971021     C                   ELSE
049300971021     C                   MOVEL     WI09          VABPKB
049400010719     C                   IF        WD09 = *blanks
049500010719     C                   EVAL      WD09 = *zeros
049600010719     C                   ENDIF
049700971023     C                   TESTN                   WD09                 30
049800971023     C  N30              TESTN                   WD09                     30
049900971021     C                   MOVE      WD09          W001A             1
050000971021     C     *IN30         IFEQ      *OFF
050100971021     C     W001A         ORLT      '0'
050200971023     C     W001A         ANDNE     *BLANK
050300971021     C                   SETON                                        32
050400971021     C                   MOVE      *ZERO         VABPKB
050500010604     C                   eval      vinmsg = %trimr(vinmsg)
050600010604     C                             + ' ' + 'VHBPKB'
050700971021     C                   ELSE
050800971021     C                   MOVE      WD09          VABPKB
050900971021     C                   ENDIF
051000050118     C                   ENDIF
051100971021     C*
051200041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
051300041001     C                   IF        *IN51 = *OFF
051400971021     C                   MOVE      *ZERO         VABVLB
051500971021     C                   MOVE      VHBVLB        WASC07
051600010719     C                   IF        WI07 = *blanks
051700010719     C                   EVAL      WI07 = *zeros
051800010719     C                   ENDIF
051900971023     C                   TESTN                   WI07                 30
052000971023     C  N30              TESTN                   WI07                   30
052100971023     C  N30              TESTN                   WI07                     30
052200971021     C                   MOVE      WI07          W001A             1
052300971021     C     *IN30         IFEQ      *OFF
052400971021     C     W001A         ORLT      '0'
052500971023     C     W001A         ANDNE     *BLANK
052600971021     C                   SETON                                        32
052700971021     C                   MOVE      *ZERO         VABVLB
052800010604     C                   eval      vinmsg = %trimr(vinmsg)
052900010604     C                             + ' ' + 'VHBVLB'
053000971021     C                   ELSE
053100971021     C                   MOVEL     WI07          VABVLB
053200010719     C                   IF        WD07 = *blanks
053300010719     C                   EVAL      WD07 = *zeros
053400010719     C                   ENDIF
053500971023     C                   TESTN                   WD07                 30
053600971023     C  N30              TESTN                   WD07                   30
053700971023     C  N30              TESTN                   WD07                     30
053800971021     C                   MOVE      WD07          W001A             1
053900971021     C     *IN30         IFEQ      *OFF
054000971021     C     W001A         ORLT      '0'
054100971023     C     W001A         ANDNE     *BLANK
054200971021     C                   SETON                                        32
054300971021     C                   MOVE      *ZERO         VABVLB
054400010604     C                   eval      vinmsg = %trimr(vinmsg)
054500010604     C                             + ' ' + 'VHBVLB'
054600971021     C                   ELSE
054700971021     C                   MOVE      WD07          VABVLB
054800971021     C                   ENDIF
054900050118     C                   ENDIF
055000041001     C                   ELSE
055100041001     C* ...altrimenti forzo azzeramento del volume
055200041001     C                   Z-ADD     *zeros        VABVLB
055300041001     C                   ENDIF
055400941229     C*
055500971021     C                   MOVE      *ZERO         VABQFT
055600971021     C                   MOVE      VHBQFT        WASC15
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
056800971021     C                   MOVE      *ZERO         VABQFT
056900010604     C                   eval      vinmsg = %trimr(vinmsg)
057000010604     C                             + ' ' + 'VHBQFT'
057100971021     C                   ELSE
057200971021     C                   MOVEL     WI15          VABQFT
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
058400971021     C                   MOVE      *ZERO         VABQFT
058500010604     C                   eval      vinmsg = %trimr(vinmsg)
058600010604     C                             + ' ' + 'VHBQFT'
058700971021     C                   ELSE
058800971021     C                   MOVE      WD15          VABQFT
058900971021     C                   ENDIF
059000050118     C                   ENDIF
059100941229     C*
059200971021     C                   MOVE      *ZERO         VABCAS
059300971021     C                   MOVE      VHBCAS        WASC15
059400010719     C                   IF        WI15 = *blanks
059500010719     C                   EVAL      WI15 = *zeros
059600010719     C                   ENDIF
059700971023     C                   TESTN                   WI15                 30
059800971023     C  N30              TESTN                   WI15                   30
059900971023     C  N30              TESTN                   WI15                     30
060000971021     C                   MOVE      WI15          W001A             1
060100971021     C     *IN30         IFEQ      *OFF
060200971021     C     W001A         ORLT      '0'
060300971023     C     W001A         ANDNE     *BLANK
060400971021     C                   SETON                                        32
060500971021     C                   MOVE      *ZERO         VABCAS
060600010604     C                   eval      vinmsg = %trimr(vinmsg)
060700010604     C                             + ' ' + 'VHBCAS'
060800971021     C                   ELSE
060900971021     C                   MOVEL     WI15          VABCAS
061000010719     C                   IF        WD15 = *blanks
061100010719     C                   EVAL      WD15 = *zeros
061200010719     C                   ENDIF
061300971023     C                   TESTN                   WD15                 30
061400971023     C  N30              TESTN                   WD15                   30
061500971023     C  N30              TESTN                   WD15                     30
061600971021     C                   MOVE      WD15          W001A             1
061700971021     C     *IN30         IFEQ      *OFF
061800971021     C     W001A         ORLT      '0'
061900971023     C     W001A         ANDNE     *BLANK
062000971021     C                   SETON                                        32
062100971021     C                   MOVE      *ZERO         VABCAS
062200010604     C                   eval      vinmsg = %trimr(vinmsg)
062300010604     C                             + ' ' + 'VHBCAS'
062400971021     C                   ELSE
062500971021     C                   MOVE      WD15          VABCAS
062600971021     C                   ENDIF
062700050118     C                   ENDIF
062800971021     C*
062900971023     C                   TESTN                   VHBRMN               30
063000971023     C  N30              TESTN                   VHBRMN                 30
063100971023     C  N30              TESTN                   VHBRMN                   30
063200971021     C                   MOVE      VHBRMN        W001A             1
063300971021     C     *IN30         IFEQ      *OFF
063400971021     C     W001A         ORLT      '0'
063500971021     C                   SETON                                        32
063600971021     C                   MOVE      *ZERO         VABRMN
063700010604     C                   eval      vinmsg = %trimr(vinmsg)
063800010604     C                             + ' ' + 'VHBRMN'
063900971021     C                   ELSE
064000971021     C                   MOVE      VHBRMN        VABRMN
064100971021     C                   ENDIF
064200971021     C*
064300971023     C                   TESTN                   VHBNCD               30
064400971023     C  N30              TESTN                   VHBNCD                 30
064500971023     C  N30              TESTN                   VHBNCD                   30
064600971021     C                   MOVE      VHBNCD        W001A             1
064700971021     C     *IN30         IFEQ      *OFF
064800971021     C     W001A         ORLT      '0'
064900971023     C     W001A         ANDNE     *BLANK
065000971021     C                   SETON                                        32
065100971021     C                   MOVE      *ZERO         VABNCD
065200010604     C                   eval      vinmsg = %trimr(vinmsg)
065300010604     C                             + ' ' + 'VHBNCD'
065400971021     C                   ELSE
065500971021     C                   MOVE      VHBNCD        VABNCD
065600971021     C                   ENDIF
065700971021     C*
065800971023     C                   TESTN                   VHBNCA               30
065900971023     C  N30              TESTN                   VHBNCA                 30
066000971023     C  N30              TESTN                   VHBNCA                   30
066100971021     C                   MOVE      VHBNCA        W001A             1
066200971021     C     *IN30         IFEQ      *OFF
066300971021     C     W001A         ORLT      '0'
066400971023     C     W001A         ANDNE     *BLANK
066500971021     C                   SETON                                        32
066600971021     C                   MOVE      *ZERO         VABNCA
066700010604     C                   eval      vinmsg = %trimr(vinmsg)
066800010604     C                             + ' ' + 'VHBNCA'
066900971021     C                   ELSE
067000971021     C                   MOVE      VHBNCA        VABNCA
067100971021     C                   ENDIF
067200971021     C*
067300971023     C                   TESTN                   VHBZNC               30
067400971023     C  N30              TESTN                   VHBZNC                 30
067500971023     C  N30              TESTN                   VHBZNC                   30
067600971021     C                   MOVE      VHBZNC        W001A             1
067700971021     C     *IN30         IFEQ      *OFF
067800971021     C     W001A         ORLT      '0'
067900971023     C     W001A         ANDNE     *BLANK
068000971021     C                   SETON                                        32
068100971021     C                   MOVE      *ZERO         VABZNC
068200010604     C                   eval      vinmsg = %trimr(vinmsg)
068300010604     C                             + ' ' + 'VHBZNC'
068400971021     C                   ELSE
068500971021     C                   MOVE      VHBZNC        VABZNC
068600971021     C                   ENDIF
068700971021     C*
068800971023     C                   TESTN                   VHBDCR               30
068900971023     C  N30              TESTN                   VHBDCR                 30
069000971023     C  N30              TESTN                   VHBDCR                   30
069100971021     C                   MOVE      VHBDCR        W001A             1
069200971021     C     *IN30         IFEQ      *OFF
069300971021     C     W001A         ORLT      '0'
069400971023     C     W001A         ANDNE     *BLANK
069500971021     C                   SETON                                        32
069600971021     C                   MOVE      *ZERO         VABDCR
069700010604     C                   eval      vinmsg = %trimr(vinmsg)
069800010604     C                             + ' ' + 'VHBDCR'
069900971021     C                   ELSE
070000971021     C                   MOVE      VHBDCR        VABDCR
070100971021     C                   ENDIF
070200971021     C*
070300971023     C                   TESTN                   VHBHCR               30
070400971023     C  N30              TESTN                   VHBHCR                 30
070500971023     C  N30              TESTN                   VHBHCR                   30
070600971021     C                   MOVE      VHBHCR        W001A             1
070700971021     C     *IN30         IFEQ      *OFF
070800971021     C     W001A         ORLT      '0'
070900971023     C     W001A         ANDNE     *BLANK
071000971021     C                   SETON                                        32
071100971021     C                   MOVE      *ZERO         VABHCR
071200010604     C                   eval      vinmsg = %trimr(vinmsg)
071300010604     C                             + ' ' + 'VHBHCR'
071400971021     C                   ELSE
071500971021     C                   MOVE      VHBHCR        VABHCR
071600971021     C                   ENDIF
071700941229     C*
071800971021     C                   MOVE      *ZERO         VABVMD
071900971021     C                   MOVE      VHBVMD        WASC15
072000010719     C                   IF        WI15 = *blanks
072100010719     C                   EVAL      WI15 = *zeros
072200010719     C                   ENDIF
072300971023     C                   TESTN                   WI15                 30
072400971023     C  N30              TESTN                   WI15                   30
072500971023     C  N30              TESTN                   WI15                     30
072600971021     C                   MOVE      WI15          W001A             1
072700971021     C     *IN30         IFEQ      *OFF
072800971021     C     W001A         ORLT      '0'
072900971023     C     W001A         ANDNE     *BLANK
073000971021     C                   SETON                                        32
073100971021     C                   MOVE      *ZERO         VABVMD
073200010604     C                   eval      vinmsg = %trimr(vinmsg)
073300010604     C                             + ' ' + 'VHBVMD'
073400971021     C                   ELSE
073500971021     C                   MOVEL     WI15          VABVMD
073600010719     C                   IF        WD15 = *blanks
073700010719     C                   EVAL      WD15 = *zeros
073800010719     C                   ENDIF
073900971023     C                   TESTN                   WD15                 30
074000971023     C  N30              TESTN                   WD15                   30
074100971023     C  N30              TESTN                   WD15                     30
074200971021     C                   MOVE      WD15          W001A             1
074300971021     C     *IN30         IFEQ      *OFF
074400971021     C     W001A         ORLT      '0'
074500971023     C     W001A         ANDNE     *BLANK
074600971021     C                   SETON                                        32
074700971021     C                   MOVE      *ZERO         VABVMD
074800010604     C                   eval      vinmsg = %trimr(vinmsg)
074900010604     C                             + ' ' + 'VHBVMD'
075000971021     C                   ELSE
075100971021     C                   MOVE      WD15          VABVMD
075200971021     C                   ENDIF
075300050118     C                   ENDIF
075400971021     C*
075500971023     C                   TESTN                   VHBANT               30
075600971023     C  N30              TESTN                   VHBANT                 30
075700971023     C  N30              TESTN                   VHBANT                   30
075800971021     C                   MOVE      VHBANT        W001A             1
075900971021     C     *IN30         IFEQ      *OFF
076000971021     C     W001A         ORLT      '0'
076100971023     C     W001A         ANDNE     *BLANK
076200971021     C                   SETON                                        32
076300971021     C                   MOVE      *ZERO         VABANT
076400010604     C                   eval      vinmsg = %trimr(vinmsg)
076500010604     C                             + ' ' + 'VHBANT'
076600971021     C                   ELSE
076700971021     C                   MOVE      VHBANT        VABANT
076800971021     C                   ENDIF
076900971021     C*
077000971021     C* CAMPI ALFABETICI:
077100971021     C                   MOVE      VHBCBO        VABCBO
077200971021     C                   MOVE      VHBRSD        VABRSD
077300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
077400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
077500020117     C* ==
077600971021     C                   MOVE      VHBRD2        VABRD2
077700971021     C                   MOVE      VHBIND        VABIND
077800971021     C                   MOVE      VHBCAD        VABCAD
077900971021     C                   MOVE      VHBLOD        VABLOD
078000971021     C                   MOVE      VHBPRD        VABPRD
078100971021     C                   MOVE      VHBNZD        VABNZD
078200971021     C                   MOVE      VHBGC1        VABGC1
078300971021     C                   MOVE      VHBGC2        VABGC2
078400971021     C                   MOVE      VHBTSP        VABTSP
078500971021     C                   MOVE      VHBVAS        VABVAS
078600971021     C                   MOVE      VHBNAS        VABNAS
078700971021     C                   MOVE      VHBTIC        VABTIC
078800971021     C                   MOVE      VHBVCA        VABVCA
078900971021     C                   MOVE      VHBGCA        VABGCA
079000971021     C                   MOVE      VHBRMA        VABRMA
079100971021     C                   MOVE      VHBXCO        VABXCO
079200971021     C                   MOVE      VHBNOT        VABNOT
079300971021     C                   MOVE      VHBNT2        VABNT2
079400971021     C                   MOVE      VHBCTM        VABCTM
079500971021     C                   MOVE      VHBFFD        VABFFD
079600971021     C                   MOVE      VHBTCR        VABTCR
079700971021     C                   MOVE      VHBCTS        VABCTS
079800971021     C                   MOVE      VHBFTM        VABFTM
079900971021     C                   MOVE      VHBVAD        VABVAD
080000971021     C                   MOVE      VHBGMA        VABGMA
080100971021     C                   MOVE      VHBGGA        VABGGA
080200971021     C                   MOVE      VHBGVA        VABGVA
080300971021     C                   MOVE      VHBTC1        VABTC1
080400971021     C                   MOVE      VHBTC2        VABTC2
080500971021     C                   MOVE      VHBSCL        VABSCL
080600971021     C                   MOVE      VHBRMO        VABRMO
080700971021     C                   MOVE      VHBCMO        VABCMO
080800971021     C                   MOVE      VHBNMO        VABNMO
080900971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
081000971021     C* NELLA NAZIONE ORIGINALE
081100971021     C                   MOVE      VHBNMO        WA1D              1
081200971021     C                   MOVEL     VHBNMO        WA1S              1
081300971021     C     WA1D          IFNE      *BLANK
081400971021     C     WA1S          ANDEQ     *BLANK
081500971021     C                   MOVE      *BLANK        VABNMO
081600030616     C                   ENDIF
081700030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
081800030710     C                   EVAL      VABCMR = wCMR
081900030616     C                   EVAL      VABDCM = DATCOR
082000030616     C                   EVAL      VABDTS = DATCOR
082100040402     C                   EVAL      VABHMS = ORACOR
082200030616     C                   EVAL      VABCNT = 1
082300041025     C*
082400041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
082500041025     C                   IF        *IN52 = *ON
082600041025     C* Reperisco la provincia dal CAP e dalla località
082700041025     C                   IF        VABCAD <> *blanks AND
082800041025     C                             VABPRD  = *blanks
082900041025     C                   CLEAR                   TISI95DS
083000041025     C                   EVAL      I95TCN = '3'
083100041025     C                   Z-ADD     datcor        I95DAT
083200041025     C                   EVAL      I95CAP = VABCAD
083300041025     C                   EVAL      I95LOC = VABLOD
083400041025     C                   CALL      'TISI95R'
083500041025     C                   PARM                    TISI95DS
083600041025     C                   EVAL      VABPRD = O95PRV
083700041025     C                   ENDIF
083800041025     C                   ENDIF
083900050613     C*
084000050613     C                   else
084100050618     C*
084200050618     C* *** VAD ***
084300050618     C                   if        %subst(vindta:1:1) = '2'
084400050613     C*
084500050613     C                   clear                   EDIVAD00
084600050613     C*
084700050613     C* Reperimento campi ALFA
084800050613     C                   EVAL      VADCDP=%trim(%subst(vindta:25:15))
084900050613     C*
085000050613     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
085100050613     C                   EXSR      CHKNUM
085200050613     C                   IF        PiInt=*on
085300050613     C                   Z-ADD     PiVal         VADCCM
085400050613     C                   ELSE
085500050613     C                   SETON                                        33
085600050613     C                   EVAL      vinmsg = %trimr(vinmsg)
085700050613     C                             + ' ' + 'VADCCM'
085800050613     C                   ENDIF
085900050613     C*
086000050613     C                   EVAL      PiStr=%trim(%subst(vindta:9:3))
086100050613     C                   EXSR      CHKNUM
086200050613     C                   IF        PiInt=*on
086300050613     C                   Z-ADD     PiVal         VADLNP
086400050613     C                   ELSE
086500050613     C                   SETON                                        33
086600050613     C                   EVAL      vinmsg = %trimr(vinmsg)
086700050613     C                             + ' ' + 'VADLNP'
086800050613     C                   ENDIF
086900050613     C*
087000050613     C                   IF        %subst(vindta:12:4) <> *blanks
087100050613     C                   EVAL      PiStr=%trim(%subst(vindta:12:4))
087200050613     C                   EXSR      CHKNUM
087300050613     C                   IF        PiInt=*on
087400050613     C                   Z-ADD     PiVal         VADAAS
087500050613     C                   ELSE
087600050613     C                   SETON                                        33
087700050613     C                   EVAL      vinmsg = %trimr(vinmsg)
087800050613     C                             + ' ' + 'VADAAS'
087900050613     C                   ENDIF
088000050613     C                   ELSE
088100050613     C                   MOVEL     datcor        VADAAS
088200050613     C                   ENDIF
088300050613     C*
088400050613     C                   EVAL      PiStr=%trim(%subst(vindta:16:2))
088500050613     C                   EXSR      CHKNUM
088600050613     C                   IF        PiInt=*on
088700050613     C                   Z-ADD     PiVal         VADNRS
088800050613     C                   ELSE
088900050613     C                   SETON                                        33
089000050613     C                   Z-ADD     *zeros        VADNRS
089100050613     C                   EVAL      vinmsg = %trimr(vinmsg)
089200050613     C                             + ' ' + 'VADNRS'
089300050613     C                   ENDIF
089400050613     C*
089500050613     C                   EVAL      PiStr=%trim(%subst(vindta:18:7))
089600050613     C                   EXSR      CHKNUM
089700050613     C                   IF        PiInt=*on
089800050613     C                   Z-ADD     PiVal         VADNSP
089900050613     C                   ELSE
090000050613     C                   SETON                                        33
090100050613     C                   Z-ADD     *zeros        VADNSP
090200050613     C                   EVAL      vinmsg = %trimr(vinmsg)
090300050613     C                             + ' ' + 'VADNSP'
090400050613     C                   ENDIF
090500050613     C*
090600050613     C                   EVAL      PiStr=%trim(%subst(vindta:40:5))
090700050613     C                   EXSR      CHKNUM
090800050613     C                   IF        PiInt=*on
090900050613     C                   Z-ADD     PiVal         VADNCL
091000050613     C                   ELSE
091100050613     C                   SETON                                        34
091200050613     C                   Z-ADD     *zeros        VADNCL
091300050613     C                   EVAL      vinmsg = %trimr(vinmsg)
091400050613     C                             + ' ' + 'VADNCL'
091500050613     C                   ENDIF
091600050613     C*
091700050613     C                   EVAL      PiStr=%trim(%subst(vindta:45:7))
091800050613     C                   EXSR      CHKNUM
091900050613     C                   IF        PiInt=*on
092000050613     C                   Z-ADD     PiVal         VADNCD
092100050613     C                   ELSE
092200050613     C                   SETON                                        34
092300050613     C                   Z-ADD     *zeros        VADNCD
092400050613     C                   EVAL      vinmsg = %trimr(vinmsg)
092500050613     C                             + ' ' + 'VADNCD'
092600050613     C                   ENDIF
092700050613     C*
092800050613     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
092900050613     C                   EXSR      CHKNUM
093000050613     C                   IF        PiInt=*on
093100050613     C                   Z-ADD     PiVal         VADNCA
093200050613     C                   ELSE
093300050613     C                   SETON                                        34
093400050613     C                   Z-ADD     *zeros        VADNCA
093500050613     C                   EVAL      vinmsg = %trimr(vinmsg)
093600050613     C                             + ' ' + 'VADNCA'
093700050613     C                   ENDIF
093800050618     C*
093900050618     C* Effettuo considerazioni x elaborazioni "multi-filiale"
094000050618     C                   eval      depfil = VADLNP
094100050618     C                   exsr      repfil
094200050618     C                   if        depfil = invfil
094300050618     C                   if        vlrpoi = 999
094400050618     C                   MOVE(P)   invfil        VADFGS
094500050618     C                   else
094600050618     C                   MOVE(P)   vlrpoi        VADFGS
094700050618     C                   endif
094800050618     C                   endif
094900050618     C*
095000050613     C* VALORIZZO CAMPI RELATIVI AL "CMR"
095100050613     C                   EVAL      VADCMR = wCMR
095200050613     C                   EVAL      VADCNT = 1
095300050613     C*
095400050613     C* SCARICO IL BUFFER DEL RECORD D DETTAGLIO
095500050613     C  N33              ADD       1             §CTROK            7 0
095600050613     C   34              ADD       1             §CTRMO            7 0
095700050613     C   33              ADD       1             §CTRNO            7 0
095800050618     C  N33              WRITE     EDIVAD00
095900050613     C*
096000050613     C                   endif
096100050618     C*
096200050618     C* *** VAT ***
096300050618     C                   if        %subst(vindta:1:1) = '3'
096400050618     C*
096500050618     C                   clear                   EDIVAT00
096600050618     C*
096700050618     C                   eval      fnvataar = vindta
096800050618     C*
096900050618     C                   exsr      impvat
097000050618     C*
097100050618     C* Effettuo considerazioni x elaborazioni "multi-filiale"
097200050618     C                   eval      depfil = VATLNP
097300050618     C                   exsr      repfil
097400050618     C                   if        depfil = invfil
097500050618     C                   if        vlrpoi = 999
097600050618     C                   MOVE(p)   invfil        VATFGS
097700050618     C                   else
097800050618     C                   MOVE(p)   vlrpoi        VATFGS
097900050618     C                   endif
098000050618     C                   endif
098100050618     C*
098200050618     C* VALORIZZO CAMPI RELATIVI AL "CMR"
098300050618     C                   EVAL      VATCMR = wCMR
098400050618     C                   EVAL      VATCNT = 1
098500050618     C*
098600050618     C* SCARICO IL BUFFER DEL RECORD D DETTAGLIO
098700050618     C  N35              ADD       1             §CTROK            7 0
098800050618     C   36              ADD       1             §CTRMO            7 0
098900050618     C   35              ADD       1             §CTRNO            7 0
099000050618     C  N35              WRITE     EDIVAT00
099100050618     C*
099200050618     C                   endif
099300050708     C                   endif
099400990910     C*
099500010601     C                   if        *in31 = *off and
099600050613     C                             *in32 = *off and
099700050613     C                             *in33 = *off and
099800050618     C                             *in34 = *off and
099900050618     C                             *in35 = *off and
100000050618     C                             *in36 = *off
100100990910     C                   eval      vinflg = '1'
100200990910     C                   else
100300990910     C                   eval      vinflg = '2'
100400990910     C                   endif
100500010601     C*
100600010601     C                   endif
100700000905     C*
100800000905     C                   else
100900000905     C                   eval      vinflg = '1'
101000000905     C                   endif
101100000905     C*
101200000905     C  N70              update    tivin000
101300991022     C*
101400991022     C  N70              ENDdo
101500050613     C*
101600050613     C* Scarico l'ultima testata rimasta ancora in "canna"
101700050613     C  N31              ADD       1             §CTROK            7 0
101800050613     C   32              ADD       1             §CTRMO            7 0
101900050613     C   31              ADD       1             §CTRNO            7 0
102000050613     C  N31              WRITE     EDIVAB00
102100010530     C*
102200010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
102300010530     C                   if        cntNonEl = *zeros or
102400010530     C                             flgMulti = '0'
102500990910     C* Se non ci sono record con errori ...
102600010530     C                   if        §ctrno = 0 and
102700010601     C                             §ctrmo = 0 and
102800010530     C                             flgStato <> '2'
102900990910     C* ... restituisco esito OK.
103000990921     C                   eval      wrkesito = '0'
103100990910     C                   else
103200010601     C                   if        §ctrok > 0
103300990921     C                   eval      wrkesito = '1'
103400000710     C                   else
103500010615     C                   if        flgOk = '0'
103600010615     C                   eval      wrkesito = '2'
103700010615     C                   else
103800010615     C                   eval      wrkesito = '6'
103900010615     C                   endif
104000990910     C                   endif
104100000710     C                   endif
104200010530     C                   else
104300010530     C                   eval      wrkesito = '9'
104400010530     C                   endif
104500050613     C*
104600050613     C                   else
104700050613     C                   eval      wrkesito = '2'
104800050613     C                   endif
104900030710     C*
105000030710 X  1C                   else
105100030710     C                   eval      wrkesito = '8'
105200030710     C                   eval      flgGiro = '1'
105300030710 E  1C                   endif
105400010530     C*
105500010530     C                   if        vlrpoi <> 999
105600010530     C                   eval      invfil = vlrpoi
105700010530     C                   endif
105800990910     C*
105900990920     C                   if        §ctrok > 0
106000010530     C                             and invfil > *zeros
106100000613     C                   exsr      invio
106200990920     C                   endif
106300010612     C*
106400010612     C                   if        flgGiro = '1'
106500081126     C***                exsr      endela
106600010612     C                   endif
106700990920     C*
106800910830     C                   ENDSR
106900000613     C***
107000050618
107100050618
107200050618
107300050618     C*----------------------------------------------------*
107400050618     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
107500050618     C*----------------------------------------------------*
107600050618     C     IMPVAT        BEGSR
107700050618     C*
107800050618     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
107900050618     C                   TESTN                   a_vatccm             30
108000050618     C  N30              TESTN                   a_vatccm               30
108100050618     C  N30              TESTN                   a_vatccm                 30
108200050618     C                   MOVE      a_vatccm      W001A             1
108300050618     C     *IN30         IFEQ      *OFF
108400050618     C     W001A         ORLT      '0'
108500050618     C                   SETON                                        35
108600050618     C                   Z-ADD     *zeros        VATCCM
108700050618     C                   EVAL      vinmsg = 'VATCCM'
108800050618     C                   ELSE
108900050618     C                   MOVE      a_vatccm      VATCCM
109000050618     C     VATCCM        IFLE      *ZEROS
109100050618     C                   SETON                                        35
109200050618     C                   Z-ADD     *zeros        VATCCM
109300050618     C                   EVAL      vinmsg = 'VATCCM'
109400050618     C                   ENDIF
109500050618     C                   ENDIF
109600050618     C*
109700050618     C                   TESTN                   a_vatlnp             30
109800050618     C  N30              TESTN                   a_vatlnp               30
109900050618     C  N30              TESTN                   a_vatlnp                 30
110000050618     C                   MOVE      a_vatlnp      W001A             1
110100050618     C     *IN30         IFEQ      *OFF
110200050618     C     W001A         ORLT      '0'
110300050618     C                   SETON                                        35
110400050618     C                   Z-ADD     *zeros        VATLNP
110500050618     C                   EVAL      vinmsg = %trimr(vinmsg)
110600050618     C                             + ' ' + 'VATLNP'
110700050618     C                   ELSE
110800050618     C                   MOVE      a_vatlnp      VATLNP
110900050618     C     VATLNP        IFLE      *ZEROS
111000050618     C                   SETON                                        35
111100050618     C                   Z-ADD     *zeros        VATLNP
111200050618     C                   EVAL      vinmsg = %trimr(vinmsg)
111300050618     C                             + ' ' + 'VATLNP'
111400050618     C                   ENDIF
111500050618     C                   ENDIF
111600050618     C*
111700050618     C                   TESTN                   a_vataas             30
111800050618     C  N30              TESTN                   a_vataas               30
111900050618     C  N30              TESTN                   a_vataas                 30
112000050618     C                   MOVE      a_vataas      W001A             1
112100050618     C     *IN30         IFEQ      *OFF
112200050618     C     W001A         ORLT      '0'
112300050618     C                   SETON                                        35
112400050618     C                   MOVEL(P)  datcor        VATAAS
112500050618     C                   EVAL      vinmsg = %trimr(vinmsg)
112600050618     C                             + ' ' + 'VATAAS'
112700050618     C                   ELSE
112800050618     C                   MOVE      a_vataas      VATAAS
112900050618     C                   ENDIF
113000050618     C*
113100050618     C                   TESTN                   a_vatnrs             30
113200050618     C  N30              TESTN                   a_vatnrs               30
113300050618     C  N30              TESTN                   a_vatnrs                 30
113400050618     C                   MOVE      a_vatnrs      W001A             1
113500050618     C     *IN30         IFEQ      *OFF
113600050618     C     W001A         ORLT      '0'
113700050618     C     W001A         ANDNE     *BLANK
113800050618     C                   SETON                                        35
113900050618     C                   Z-ADD     *zeros        VATNRS
114000050618     C                   EVAL      vinmsg = %trimr(vinmsg)
114100050618     C                             + ' ' + 'VATNRS'
114200050618     C                   ELSE
114300050618     C                   MOVE      a_vatnrs      VATNRS
114400050618     C                   ENDIF
114500050618     C*
114600050618     C                   TESTN                   a_vatnsp             30
114700050618     C  N30              TESTN                   a_vatnsp               30
114800050618     C  N30              TESTN                   a_vatnsp                 30
114900050618     C                   MOVE      a_vatnsp      W001A             1
115000050618     C     *IN30         IFEQ      *OFF
115100050618     C     W001A         ORLT      '0'
115200050618     C                   SETON                                        35
115300050618     C                   Z-ADD     *zeros        VATNSP
115400050618     C                   EVAL      vinmsg = %trimr(vinmsg)
115500050618     C                             + ' ' + 'VATNSP'
115600050618     C                   ELSE
115700050618     C                   MOVE      a_vatnsp      VATNSP
115800050618     C                   ENDIF
115900050618     C*
116000050618     C*
116100050618     C* ALFABETICI
116200050618     C                   MOVE      a_vattrc      VATTRC
116300050618     C                   EVAL      VATNOT = %trim(a_vatnot)
116400050618     C*
116500050618     C                   ENDSR
116600050618     C*----------------------------------------------------*
116700050613
116800050613
116900050613
117000050613     C*----------------------------------------------------*
117100050618     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVADWR/EDIVATWR
117200050613     C*----------------------------------------------------*
117300050613     C     PREVAD        BEGSR
117400050613     C*
117500050618     C* Compongo il nome del membro da dare al EDIVADWR/EDIVATWR
117600050613     C                   eval      parmbr = vlrhdl
117700050613     C                   movel     'M'           parmbr
117800050613     C                   eval      parccm = %subst(vlrKSC:2:7)
117900050613     C                   eval      paropz = '1'
118000050613     C* Effettuo la chiamata al CLLE preposto
118100050618     C                   call(e)   'TITVEVDTC'
118200050613     C                   parm                    parccm
118300050613     C                   parm                    parmbr
118400050613     C                   parm                    paropz
118500050613     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
118600050613     C                   if        %error
118700050613     C                   movel     '1'           chkcall
118800050613     C                   else
118900050613     C                   movel     '0'           chkcall
119000050613     C                   endif
119100050613     C*
119200050613     C                   ENDSR
119300990920
119400050613
119500050613
119600050613     C*----------------------------------------------------*
119700050613     C*  CONTROLLO NUMERICITA' CAMPI
119800050613     C*----------------------------------------------------*
119900050613     C     CHKNUM        BEGSR
120000050613     C*
120100050613     C                   call(e)   'ISNUMERIC'
120200050613     C                   PARM                    PiStr            30
120300050613     C                   PARM      '.'           PiDecChr          1
120400050613     C                   PARM      *ZEROS        PiVal            30 9
120500050613     C                   PARM      '0'           PiInt             1
120600050613     C                   PARM      '0'           PiNum             1
120700050613     C                   IF        %error
120800050613     C                   EVAL      PiInt=*off
120900050613     C                   ENDIF
121000050613     C*
121100050613     C                   ENDSR
121200050613     C***
121300010530
121400010530
121500050613
121600010530
121700010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
121800010530     C     repfil        BEGSR
121900010530     C*
122000010530     C                   if        invfil = *zeros and
122100010530     C                             depfil > *zeros and
122200010613     C                             (vinflg = *blanks or
122300010613     C                              vinflg = *zeros)
122400010530     C
122500010530     C                   eval      invfil = depfil
122600010530     C                   endif
122700010530     C*
122800010530     C                   if        depfil <> invfil and
122900010530     C                             invfil > *zeros
123000010530     C                   eval      flgMulti = '1'
123100010530     C                   if        vinflg = *blanks
123200010530     C                   add       1             cntNonEl
123300010530     C                   endif
123400010530     C                   endif
123500010530     C*
123600010530     C                   if        vinflg = '2'
123700010530     C                   eval      flgStato = '2'
123800010530     C                   endif
123900010530     C*
124000010530     C                   ENDSR
124100010530     C***
124200010530
124300010530
124400010530
124500010530
124600990920      /TITLE Invio dei dati al punto operativo.
124700000613     C     invio         BEGSR
124800050613     C*
124900050613     C* 1° invio EDIVAD
125000050613     C                   reset                   dscmz
125100170515     C                   move      invfil        cmzdst
125200050613     C                   eval      cmzfld = 'EDIVADWR'
125300050613     C                   eval      cmzmbd = parmbr
125400050613     C                   eval      %subst(cmzmbd:1:1) = 'M'
125500050613     C***                if        prmfir = *blanks
125600050613     C                   eval      cmzfla = 'EDIVAD0F'
125700050613     C                   eval      cmzmba = 'EDIVAD0F'
125800050613     C***                else
125900050613     C***                eval      cmzfla = prmfir
126000050613     C***                eval      cmzmba = prmfir
126100050613     C***                endif
126200050613     C                   eval      cmznrr = *zeros
126300050613     C                   move      §ctrok        cmznrr
126400050613     C                   eval      cmzlba = vlrfl1
126500050613     C                   call(e)   'TIS711C'
126600050613     C                   parm                    dscmz
126700050613     C                   parm      *blanks       esito
126800050613     C                   if        %error
126900050613     C                             or cmzerr = '1'
127000050613     C                             or esito  = '1'
127100050613     C                   eval      wrkesito = '3'
127200050613     C                   else
127300050618     C*
127400050618     C* 2° invio EDIVAT
127500050618     C                   reset                   dscmz
127600170515     C                   move      invfil        cmzdst
127700050618     C                   eval      cmzfld = 'EDIVATWR'
127800050618     C                   eval      cmzmbd = parmbr
127900050618     C                   eval      %subst(cmzmbd:1:1) = 'M'
128000050618     C***                if        prmfir = *blanks
128100050618     C                   eval      cmzfla = 'EDIVAT0F'
128200050618     C                   eval      cmzmba = 'EDIVAT0F'
128300050618     C***                else
128400050618     C***                eval      cmzfla = prmfir
128500050618     C***                eval      cmzmba = prmfir
128600050618     C***                endif
128700050618     C                   eval      cmznrr = *zeros
128800050618     C                   move      §ctrok        cmznrr
128900050618     C                   eval      cmzlba = vlrfl1
129000050618     C                   call(e)   'TIS711C'
129100050618     C                   parm                    dscmz
129200050618     C                   parm      *blanks       esito
129300050618     C                   if        %error
129400050618     C                             or cmzerr = '1'
129500050618     C                             or esito  = '1'
129600050618     C                   eval      wrkesito = '3'
129700050618     C                   else
129800050613     C*
129900050618     C* 3° invio EDIVAB
130000050613     C                   reset                   dscmz
130100170515     C                   move      invfil        cmzdst
130200050613     C                   eval      cmzfld = vlrfou
130300050613     C                   eval      cmzmbd = vlrhdl
130400050613     C                   eval      %subst(cmzmbd:1:1) = 'M'
130500050613     C***                if        prmfir = *blanks
130600050613     C                   eval      cmzfla = 'EDIVAB0F'
130700050613     C                   eval      cmzmba = 'EDIVAB0F'
130800050613     C***                else
130900050613     C***                eval      cmzfla = prmfir
131000050613     C***                eval      cmzmba = prmfir
131100050613     C***                endif
131200050613     C                   eval      cmznrr = *zeros
131300050613     C                   move      §ctrok        cmznrr
131400050613     C                   eval      cmzlba = vlrfl1
131500050613     C                   call(e)   'TIS711C'
131600050613     C                   parm                    dscmz
131700050613     C                   parm      *blanks       esito
131800050613     C                   if        %error
131900050613     C                             or cmzerr = '1'
132000050613     C                             or esito  = '1'
132100050613     C                   eval      wrkesito = '3'
132200050613     C                   endif
132300050613     C                   endif
132400050618     C                   endif
132500990920     C*
132600000613     C                   ENDSR
132700000613     C***
132800010530
132900010530
133000010530
133100010530
133200010530      /TITLE Invio dei dati al punto operativo.
133300010530     C     opeini        BEGSR
133400010530     C*
133500010530     C* Inizializzo flag e contatori operativi
133600010530     C                   movel     '0'           flgGiro           1
133700010530     C                   movel     '0'           flgMulti          1
133800010530     C                   movel     '1'           flgStato          1
133900010615     C                   movel     '0'           flgOk             1
134000010530     C                   z-add     *zeros        cntNonEl         10 0
134100010530     C                   z-add     *zeros        depfil            3 0
134200010530     C                   z-add     *zeros        invfil            3 0
134300010530     C*
134400010530     C                   ENDSR
134500010530     C***
134600080930
134700080930
134800080930
134900170804     C***  *pssr         BEGSR
135000080930     C*
135100170804     C***                if        %open(tivin00r)
135200170804     C***                close     tivin00r
135300170804     C***                endif
135400170804     C***                if        %open(edivabwr)
135500170804     C***                close     edivabwr
135600170804     C***                endif
135700170804     C***                if        %open(edivadwr)
135800170804     C***                close     edivadwr
135900170804     C***                endif
136000170804     C***                if        %open(edivatwr)
136100170804     C***                close     edivatwr
136200170804     C***                endif
136300080930     C*
136400080930     C* Effettuo la chiamata al CLLE preposto
136500170804     C***                call(e)   'TITVEVDTC'
136600170804     C***                parm                    parccm
136700170804     C***                parm                    parmbr
136800170804     C***                parm      '2'           paropz
136900080930     C*
137000170804     C***                eval      wrkesito = '2'
137100080930     C*
137200170804     C***                seton                                        LR
137300080930     C*
137400170804     C***                ENDSR     '*CANCL'
137500080930     C***
137600080930
137700010530
137800990910
137900000613     C     *inzsr        BEGSR
138000990910     C*
138100990910     C     *entry        plist
138200990920     C                   parm                    tivlrds
138300990921     C                   parm      wrkesito      esito
138400000724     C                   parm                    prmlit
138500000710     C                   parm                    prmfir
138600030616     C*
138700030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
138800030710     C                   move(p)   vlrMSG        wNomeFile
138900030710     C*
139000030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
139100030710     C* del codice CMR dal nome del file importato.
139200030710     C                   if        wNomeFile <> *blanks AND
139300030710     C                             vlrppt<>*blanks
139400041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
139500030710     C                   if        %subst(vlrppt:1:1) = 'S'
139600030710     C                   seton                                        50
139700030710     C                   else
139800030710     C                   setoff                                       50
139900030710     C                   endif
140000041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
140100041001     C                   if        %subst(vlrppt:6:1) = 'S'
140200041001     C                   seton                                        51
140300041001     C                   else
140400041001     C                   setoff                                       51
140500041001     C                   endif
140600041025     C*
140700041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
140800041025     C* (naturalmente solo se nn già presente)
140900041025     C                   if        %subst(vlrppt:7:1) = 'S'
141000041025     C                   seton                                        52
141100041025     C                   else
141200041025     C                   setoff                                       52
141300041025     C                   endif
141400041001     C*
141500030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
141600030710     C                   move(p)   wPosDaA       wPosDa
141700030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
141800030710     C                   move(p)   wLungA        wLung
141900030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
142000030710     C                   seton                                        60
142100030710     C                   else
142200030710     C                   setoff                                       60
142300030710     C                   endif
142400010606     C*
142500010606     C* CALCOLA LA DATA CORRENTE
142600010606     C                   time                    wn14             14 0
142700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
142800040402     C                   movel     wn14          oracor            6 0          *ORA
142900010606     C                   z-add     wn8           g08dat
143000010606     C                   z-add     *zeros        g08inv
143100010606     C                   movel     '0'           g08err
143200010606     C                   call      'XSRDA8'
143300010606     C                   parm                    wlbda8
143400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
143500000613     C*
143600000613     C                   ENDSR
143700000613     C***
