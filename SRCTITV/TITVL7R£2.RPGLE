000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030616     FEDIVABWR  O    E             DISK    usropn
000700050613     FEDIVADWR  O    E             DISK    usropn
000800050618     FEDIVATWR  O    E             DISK    usropn
000900050613     Ftitvl7p   O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100080930     F                                     infsr(*pssr)
001200050613     Ftitvl7ps  O    f  198        PRINTER usropn
001300000621     F                                     oflind(*inob)
001400080930     F                                     infsr(*pssr)
001500990908
001600000512     D*------------
001700000512     D* COMANDI
001800000512     D*------------
001900011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000000512     D*------------
002100990920     D dscmz         e ds                  inz
002200990910     D psds           sds
002300990910     D  procname         *PROC
002400050118     D*
002500050118     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002600050118     D* 10+3
002700050118     D                 DS
002800050118     D  WI15                   1     10
002900050118     D  WD15                  11     13
003000050118     D  WASC15                 1     13
003100050118     D*  6+1
003200050118     D                 DS
003300050118     D  WI09                   1      6
003400050118     D  WD09                   7      7
003500050118     D  WASC09                 1      7
003600050118     D*  2+3
003700050118     D                 DS
003800050118     D  WI07                   1      2
003900050118     D  WD07                   3      5
004000050118     D  WASC07                 1      5
004100050618     D*
004200050618     D fnvataar        ds                  inz
004300050618     D  a_vatatb                      1
004400050618     D  a_vatccm                      7
004500050618     D  a_vatlnp                      3
004600050618     D  a_vataas                      4
004700050618     D  a_vatnrs                      2
004800050618     D  a_vatnsp                      7
004900050618     D  a_vattrc                      1
005000050618     D  a_vatnot                     35
005100990920     D tivlrds       e ds                  extname(tivlr00f)
005200041025     D tisi95ds      e ds
005300990910     D esito           s              1
005400000724     D prmlit          s             10
005500000710     D prmfir          s             10
005600990921     D wrkesito        s                   like(esito)
005700990915     D wrkdata         s               d
005800990915     D wrkora          s               t
005900000613     D rrnum           s              6  0 INZ(*zeros)
006000000621     D recko           s            150    INZ(*blanks)
006100050613     D parccm          s              8    INZ(*blanks)
006200050613     D parmbr          s             10    INZ(*blanks)
006300050613     D paropz          s              1    INZ(*blanks)
006400050613     D chkcall         s              1    INZ(*blanks)
006500011123     D depcmd          s            150    INZ(*blanks)
006600030616     D wNomeFile       s             30    INZ(*blanks)
006700030710     D wCMR            s             35    INZ(*blanks)
006800030710     D wPosDaA         s              2    INZ(*blanks)
006900030710     D wPosDa          s              2  0 INZ(*zeros)
007000030710     D wLungA          s              2    INZ(*blanks)
007100030710     D wLung           s              2  0 INZ(*zeros)
007200010606     D*------------------
007300010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
007400010606     D*------------------
007500010606     D WLBDA8          DS                  INZ
007600010606     D  G08DAT                 1      8  0
007700010606     D  G08INV                 9     16  0
007800010606     D  G08ERR                17     17
007900010606     D  G08TGI                18     22  0
008000050118     D*---------------------------------------------------------
008100050118     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
008200050118     D*---------------------------------------------------------
008300050118     D                 ds
008400050118     D vhbatb                  1      1
008500050118     D vhbccm                  2      8
008600050118     D vhblnp                  9     11
008700050118     D vhbaas                 12     15
008800050118     D vhbmgs                 16     19
008900050118     D vhbnrs                 20     21
009000050118     D vhbnsp                 22     28
009100050118     D vhbcbo                 29     30
009200050118     D vhblna                 31     33
009300050118     D vhbrsd                 34     68
009400050118     D vhbrd2                 69    103
009500050118     D vhbind                104    138
009600050118     D vhbcad                139    147
009700050118     D vhblod                148    182
009800050118     D vhbprd                183    184
009900050118     D vhbnzd                185    187
010000050118     D vhbgc1                188    189
010100050118     D vhbgc2                190    191
010200050118     D vhbctr                192    194
010300050118     D vhbtsp                195    195
010400050118     D vhbias                196    208
010500050118     D vhbvas                209    211
010600050118     D vhbnas                212    226
010700050118     D vhbncl                227    231
010800050118     D vhbpkb                232    238
010900050118     D vhbvlb                239    243
011000050118     D vhbqft                244    256
011100050118     D vhbcas                257    269
011200050118     D vhbtic                270    271
011300050118     D vhbvca                272    274
011400050118     D vhbgca                275    276
011500050118     D vhbrmn                277    291
011600050118     D vhbrma                292    306
011700050118     D vhbncd                307    313
011800050118     D vhbnca                314    320
011900050118     D vhbxco                321    321
012000050118     D vhbnot                322    356
012100050118     D vhbnt2                357    391
012200050118     D vhbznc                392    393
012300050118     D vhbctm                394    395
012400050118     D vhbffd                396    396
012500050118     D vhbdcr                397    404
012600050118     D vhbtcr                405    405
012700050118     D vhbhcr                406    409
012800050118     D vhbcts                410    411
012900050118     D vhbftm                412    412
013000050118     D vhbvmd                413    425
013100050118     D vhbvad                426    428
013200050118     D vhbgma                429    430
013300050118     D vhbgga                431    432
013400050118     D vhbgva                433    434
013500050118     D vhbtc1                435    435
013600050118     D vhbtc2                436    436
013700050118     D vhbscl                437    437
013800050118     D vhbant                438    446
013900050118     D vhbrmo                447    471
014000050118     D vhbcmo                472    480
014100050118     D vhbnmo                481    483
014200050118     D  X_rec                  1    483    inz
014300010606     D*
014400990908
014500990915     C                   time                    wrkdata
014600990915     C                   time                    wrkora
014700000913     C                   reset                   rrnum
014800990921     C                   reset                   esito
014900990921     C                   reset                   wrkesito
015000010530     C*
015100010530     C                   exsr      opeini
015200010530     C                   exsr      rwvab
015300081125     C*
015400081125     C                   exsr      endela
015500041025     C*
015600041025     C* Esegue lancio TISI95R solo x chiusura
015700041025     C                   CLEAR                   TISI95DS
015800041025     C                   EVAL      I95TLA = 'C'
015900041025     C                   CALL      'TISI95R'
016000041025     C                   PARM                    TISI95DS
016100050613     C*
016200050613     C* Effettuo la chiamata al CLLE preposto
016300050618     C                   call(e)   'TITVEVDTC'
016400050613     C                   parm                    parccm
016500050613     C                   parm                    parmbr
016600050613     C                   parm      '2'           paropz
016700010530     C*
016800010530     C                   seton                                        lr
016900010530
017000010530
017100010530
017200010530
017300010530     C*--------------------------------------------------------
017400010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
017500010530     C*--------------------------------------------------------
017600010530     C     PREELA        BEGSR
017700000724     C*
017800000724     C* SE OCCORRE SPEDIRE IN FILIALE
017900010530     C                   if        invfil <> *zeros and
018000010530     C                             flgGiro = '0'
018100010530     C*
018200010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
018300010530     C                   eval      flgGiro = '1'
018400000724     C*
018500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
018600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
018700030623     C     VABFGS        chain     azorg01l
018800000724     C                   if        %found
018900000616     C                   movel(p)  CMD(1)        depcmd
019000020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
019100000616     C*
019200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
019300011123     C                   Z-ADD     150           LENGH            15 5
019400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019500000616     C                   PARM                    depcmd
019600000616     C                   PARM                    LENGH
019700000724     C*
019800000724     C                   endif
019900000616     C*
020000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
020100000616     C                   movel(p)  CMD(2)        depcmd
020200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
020300000616     C*
020400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
020500011123     C                   Z-ADD     150           LENGH            15 5
020600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020700000616     C                   PARM                    depcmd
020800000616     C                   PARM                    LENGH
020900000616     C*
021000050613     C                   if        not %open(titvl7ps)
021100050613     C                   open      titvl7ps
021200030407     C   50              except    testdett
021300000613     C                   endif
021400010530     C*
021500010530     C                   endif
021600010530     C*
021700010530     C                   ENDSR
021800010530     C***
021900010530
022000010530
022100010530
022200010530
022300010530
022400010530
022500010530     C*--------------------------------------------------------
022600010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
022700010530     C*--------------------------------------------------------
022800010530     C     ENDELA        BEGSR
022900010530     C*
023000010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
023100000613     C*
023200050613     C                   if        %open(titvl7ps)
023300030407     C   50              except    findett
023400050613     C                   close     titvl7ps
023500000613     C                   endif
023600080930     C*
023700080930     C                   if        %open(tivin00r)
023800080930     C                   close     tivin00r
023900080930     C                   endif
024000080930     C                   if        %open(edivabwr)
024100080930     C                   close     edivabwr
024200080930     C                   endif
024300080930     C                   if        %open(edivadwr)
024400080930     C                   close     edivadwr
024500080930     C                   endif
024600080930     C                   if        %open(edivatwr)
024700080930     C                   close     edivatwr
024800080930     C                   endif
024900000616     C*
025000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
025100011123     C                   Z-ADD     150           LENGH            15 5
025200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
025300000616     C                   PARM                    CMD(3)
025400000616     C                   PARM                    LENGH
025500010530     C*
025600010530     C                   ENDSR
025700010530     C***
025800000613
025900010530
026000010530
026100000613
026200000613     C*--------------------------------------------------------
026300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
026400000613     C*--------------------------------------------------------
026500000621     C     STPR          BEGSR
026600000613     C*
026700050613     C                   if        not %open(titvl7p)
026800050613     C                   open      titvl7p
026900990915     C                   endif
027000990915     C*
027100030407     C   50              except    riepilogo
027200990915     C*
027300050613     C                   if        %open(titvl7p)
027400050613     C                   close     titvl7p
027500990914     C                   endif
027600990910     C*
027700000613     C                   ENDSR
027800000613     C***
027900990908
028000010530
028100010530
028200010530
028300910830     C*--------------------------------------------------------
028400030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
028500910830     C*--------------------------------------------------------
028600941229     C     RWVAB         BEGSR
028700030710     C*
028800030710     C                   clear                   §CTROK
028900030710     C                   clear                   §CTRMO
029000030710     C                   clear                   §CTRNO
029100990910     C*
029200030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
029300030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
029400030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
029500030710     C                   if        *in60 = *on
029600990914     C                   if        not %open(tivin00r)
029700990908     C                   open      tivin00r
029800990914     C                   endif
029900030616     C                   if        not %open(edivabwr)
030000030616     C                   open      edivabwr
030100990914     C                   endif
030200050613     C*
030300050618     C* Eseguo operazioni di aggiunta nuovo membro in EDIVADWR/EDIVATWR
030400050613     C                   exsr      prevad
030500050613     C*
030600050613     C                   if        chkcall = '0'
030700050613     C*
030800050613     C                   if        not %open(edivadwr)
030900050613     C                   open      edivadwr
031000050613     C                   endif
031100050618     C                   if        not %open(edivatwr)
031200050618     C                   open      edivatwr
031300050618     C                   endif
031400050613     C*
031500050613     C* Inizializzo il flag d wrk che pilota lo scarico dei buffer testate
031600050613     C                   movel     '0'           wGiroVAB          1
031700990910     C*
031800921023     C                   DO        *HIVAL
031900990913     C*
032000990915     C                   READ      tivin00r                               70
032100010618     C*
032200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
032300010618     C                   if        vinflg = '1'
032400010618     C                   eval      flgOk = '1'
032500010618     C                   endif
032600010705     C*
032700010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
032800010705     C                   if        %subst(vindta:1:1) = '='
032900010705     C                   eval      %subst(vindta:1:1) = *blanks
033000010705     C                   endif
033100010705     C*
033200000905     C                   if        vindta > *blanks
033300000613     C                   add       1             rrnum
033400010601     C* ...elimino eventuali segni
033500010601     C     '+':' '       xlate     vindta        vindta
033600990913     C*
033700010601     C                   if        *in70 = *off and
033800990913     C                             (vinflg = *blanks
033900010711     C                              or vinflg = '0'
034000010711     C                              or vinflg = '2')
034100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
034200010711     C                   if        vinflg = *blanks or vinflg = '0'
034300010711     C                   clear                   vinmsg
034400010711     C                   endif
034500050708     C* Inizializzo gli indicatori d errori traduzione su tutti i file d output (VAB/VAD/VAT)
034600050708     C                   SETOFF                                       3132      * VAB
034700050708     C                   SETOFF                                       3334      * VAD
034800050708     C                   SETOFF                                       3536      * VAT
034900050613     C*
035000050613     C* In questo caso il flag d annullamento (ATB) identifica il tipo file:
035100050613     C* 1 = VAB
035200050613     C* 2 = VAD
035300050618     C* 3 = VAT
035400050613     C                   if        %subst(vindta:1:1) = '1'
035500050613     C*
035600050613     C* Se trattasi NON del primo record d tipo '1' => devo scaricare la testata precedente
035700050613     C                   if        wGiroVAB = '1'
035800050613     C* SCARICO IL BUFFER DEL RECORD D TESTATA
035900050613     C  N31              ADD       1             §CTROK            7 0
036000050613     C   32              ADD       1             §CTRMO            7 0
036100050613     C   31              ADD       1             §CTRNO            7 0
036200050613     C  N31              WRITE     EDIVAB00
036300050613     C                   else
036400050613     C                   movel     '1'           wGiroVAB
036500050613     C                   endif
036600050613     C*
036700050613     C                   clear                   EDIVAB00
036800050613     C                   eval      x_rec  = vindta
036900991022     C*
037000971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
037100971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
037200971023     C*    OPPURE TUTTI BLANKS
037300010530     C*
037400010530     C                   TESTN                   VHBLNP               30
037500010530     C  N30              TESTN                   VHBLNP                 30
037600010530     C  N30              TESTN                   VHBLNP                   30
037700010530     C                   MOVE      VHBLNP        W001A             1
037800010530     C     *IN30         IFEQ      *OFF
037900010530     C     W001A         ORLT      '0'
038000010530     C                   SETON                                        31
038100010530     C                   eval      vinmsg = %trimr(vinmsg)
038200010530     C                             + ' ' + 'VHBLNP'
038300010530     C                   ELSE
038400010530     C                   MOVE      VHBLNP        VABLNP
038500010530     C     VABLNP        IFLE      *ZEROS
038600010530     C                   SETON                                        31
038700010530     C                   eval      vinmsg = %trimr(vinmsg)
038800010530     C                             + ' ' + 'VHBLNP'
038900010530     C                   ENDIF
039000010530     C                   ENDIF
039100010530     C*
039200010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
039300010530     C                   eval      depfil = VABLNP
039400010530     C                   exsr      repfil
039500010601     C                   if        depfil = invfil
039600021014     C                   if        vlrpoi = 999
039700021010     C                   MOVE(P)   invfil        VABFGS
039800021014     C                   else
039900021014     C                   MOVE(P)   vlrpoi        VABFGS
040000021014     C                   endif
040100050613     C                   endif
040200010530     C*
040300010530     C                   exsr      PREELA
040400010530     C*
040500971023     C                   TESTN                   VHBCCM               30
040600971023     C  N30              TESTN                   VHBCCM                 30
040700971023     C  N30              TESTN                   VHBCCM                   30
040800971021     C                   MOVE      VHBCCM        W001A             1
040900971021     C     *IN30         IFEQ      *OFF
041000971021     C     W001A         ORLT      '0'
041100971021     C                   SETON                                        31
041200010604     C                   eval      vinmsg = %trimr(vinmsg)
041300010604     C                             + ' ' + 'VHBCCM'
041400971021     C                   ELSE
041500971021     C                   MOVE      VHBCCM        VABCCM
041600971024     C     VABCCM        IFLE      *ZEROS
041700971024     C                   SETON                                        31
041800010604     C                   eval      vinmsg = %trimr(vinmsg)
041900010604     C                             + ' ' + 'VHBCCM'
042000971024     C                   ENDIF
042100971021     C                   ENDIF
042200971021     C*
042300971023     C                   TESTN                   VHBAAS               30
042400971023     C  N30              TESTN                   VHBAAS                 30
042500971023     C  N30              TESTN                   VHBAAS                   30
042600971021     C                   MOVE      VHBAAS        W001A             1
042700971021     C     *IN30         IFEQ      *OFF
042800971021     C     W001A         ORLT      '0'
042900971023     C     W001A         ANDNE     *BLANK
043000971021     C                   SETON                                        32
043100010606     C                   MOVEL     datcor        VABAAS
043200010604     C                   eval      vinmsg = %trimr(vinmsg)
043300010604     C                             + ' ' + 'VHBAAS'
043400971021     C                   ELSE
043500971021     C                   MOVE      VHBAAS        VABAAS
043600971021     C                   ENDIF
043700971021     C*
043800971023     C                   TESTN                   VHBMGS               30
043900971023     C  N30              TESTN                   VHBMGS                 30
044000971023     C  N30              TESTN                   VHBMGS                   30
044100971021     C                   MOVE      VHBMGS        W001A             1
044200971021     C     *IN30         IFEQ      *OFF
044300971021     C     W001A         ORLT      '0'
044400971023     C     W001A         ANDNE     *BLANK
044500971021     C                   SETON                                        32
044600010601     C                   MOVE      '0101'        VABMGS
044700010604     C                   eval      vinmsg = %trimr(vinmsg)
044800010604     C                             + ' ' + 'VHBMGS'
044900971021     C                   ELSE
045000971021     C                   MOVE      VHBMGS        VABMGS
045100971021     C                   ENDIF
045200971021     C*
045300971023     C                   TESTN                   VHBNRS               30
045400971023     C  N30              TESTN                   VHBNRS                 30
045500971023     C  N30              TESTN                   VHBNRS                   30
045600971021     C                   MOVE      VHBNRS        W001A             1
045700971021     C     *IN30         IFEQ      *OFF
045800971021     C     W001A         ORLT      '0'
045900971023     C     W001A         ANDNE     *BLANK
046000971021     C                   SETON                                        32
046100971021     C                   MOVE      *ZERO         VABNRS
046200010604     C                   eval      vinmsg = %trimr(vinmsg)
046300010604     C                             + ' ' + 'VHBNRS'
046400971021     C                   ELSE
046500971021     C                   MOVE      VHBNRS        VABNRS
046600971021     C                   ENDIF
046700971021     C*
046800971023     C                   TESTN                   VHBNSP               30
046900971023     C  N30              TESTN                   VHBNSP                 30
047000971023     C  N30              TESTN                   VHBNSP                   30
047100971021     C                   MOVE      VHBNSP        W001A             1
047200971021     C     *IN30         IFEQ      *OFF
047300971021     C     W001A         ORLT      '0'
047400971023     C     W001A         ANDNE     *BLANK
047500971021     C                   SETON                                        32
047600971021     C                   MOVE      *ZERO         VABNSP
047700010604     C                   eval      vinmsg = %trimr(vinmsg)
047800010604     C                             + ' ' + 'VHBNSP'
047900971021     C                   ELSE
048000971021     C                   MOVE      VHBNSP        VABNSP
048100971021     C                   ENDIF
048200971021     C*
048300971023     C                   TESTN                   VHBLNA               30
048400971023     C  N30              TESTN                   VHBLNA                 30
048500971023     C  N30              TESTN                   VHBLNA                   30
048600971021     C                   MOVE      VHBLNA        W001A             1
048700971021     C     *IN30         IFEQ      *OFF
048800971021     C     W001A         ORLT      '0'
048900971023     C     W001A         ANDNE     *BLANK
049000971021     C                   SETON                                        32
049100971021     C                   MOVE      *ZERO         VABLNA
049200010604     C                   eval      vinmsg = %trimr(vinmsg)
049300010604     C                             + ' ' + 'VHBLNA'
049400971021     C                   ELSE
049500971021     C                   MOVE      VHBLNA        VABLNA
049600971021     C                   ENDIF
049700971021     C*
049800971023     C                   TESTN                   VHBCTR               30
049900971023     C  N30              TESTN                   VHBCTR                 30
050000971023     C  N30              TESTN                   VHBCTR                   30
050100971021     C                   MOVE      VHBCTR        W001A             1
050200971021     C     *IN30         IFEQ      *OFF
050300971021     C     W001A         ORLT      '0'
050400971023     C     W001A         ANDNE     *BLANK
050500971021     C                   SETON                                        32
050600971021     C                   MOVE      *ZERO         VABCTR
050700010604     C                   eval      vinmsg = %trimr(vinmsg)
050800010604     C                             + ' ' + 'VHBCTR'
050900971021     C                   ELSE
051000971021     C                   MOVE      VHBCTR        VABCTR
051100971021     C                   ENDIF
051200941229     C*
051300971021     C                   MOVE      *ZERO         VABIAS
051400941229     C                   MOVE      VHBIAS        WASC15
051500971023     C                   TESTN                   WI15                 30
051600971023     C  N30              TESTN                   WI15                   30
051700971023     C  N30              TESTN                   WI15                     30
051800971021     C                   MOVE      WI15          W001A             1
051900971021     C     *IN30         IFEQ      *OFF
052000971021     C     W001A         ORLT      '0'
052100971023     C     W001A         ANDNE     *BLANK
052200971021     C                   SETON                                        32
052300971021     C                   MOVE      *ZERO         VABIAS
052400010604     C                   eval      vinmsg = %trimr(vinmsg)
052500010604     C                             + ' ' + 'VHBIAS'
052600971021     C                   ELSE
052700971021     C                   MOVEL     WI15          VABIAS
052800971023     C                   TESTN                   WD15                 30
052900971023     C  N30              TESTN                   WD15                   30
053000971023     C  N30              TESTN                   WD15                     30
053100971021     C                   MOVE      WD15          W001A             1
053200971021     C     *IN30         IFEQ      *OFF
053300971021     C     W001A         ORLT      '0'
053400971023     C     W001A         ANDNE     *BLANK
053500971021     C                   SETON                                        32
053600971021     C                   MOVE      *ZERO         VABIAS
053700010604     C                   eval      vinmsg = %trimr(vinmsg)
053800010604     C                             + ' ' + 'VHBIAS'
053900971021     C                   ELSE
054000971021     C                   MOVE      WD15          VABIAS
054100971021     C                   ENDIF
054200050118     C                   ENDIF
054300941229     C*
054400971023     C                   TESTN                   VHBNCL               30
054500971023     C  N30              TESTN                   VHBNCL                 30
054600971023     C  N30              TESTN                   VHBNCL                   30
054700971021     C                   MOVE      VHBNCL        W001A             1
054800971021     C     *IN30         IFEQ      *OFF
054900971021     C     W001A         ORLT      '0'
055000971021     C                   SETON                                        32
055100971021     C                   MOVE      *ZERO         VABNCL
055200010604     C                   eval      vinmsg = %trimr(vinmsg)
055300010604     C                             + ' ' + 'VHBNCL'
055400971021     C                   ELSE
055500971021     C                   MOVE      VHBNCL        VABNCL
055600971021     C                   ENDIF
055700941229     C*
055800971021     C                   MOVE      *ZERO         VABPKB
055900971021     C                   MOVE      VHBPKB        WASC09
056000010719     C                   IF        WI09 = *blanks
056100010719     C                   EVAL      WI09 = *zeros
056200010719     C                   ENDIF
056300971023     C                   TESTN                   WI09                 30
056400971023     C  N30              TESTN                   WI09                   30
056500971023     C  N30              TESTN                   WI09                     30
056600971021     C                   MOVE      WI09          W001A             1
056700971021     C     *IN30         IFEQ      *OFF
056800971021     C     W001A         ORLT      '0'
056900971021     C                   SETON                                        32
057000971021     C                   MOVE      *ZERO         VABPKB
057100010604     C                   eval      vinmsg = %trimr(vinmsg)
057200010604     C                             + ' ' + 'VHBPKB'
057300971021     C                   ELSE
057400971021     C                   MOVEL     WI09          VABPKB
057500010719     C                   IF        WD09 = *blanks
057600010719     C                   EVAL      WD09 = *zeros
057700010719     C                   ENDIF
057800971023     C                   TESTN                   WD09                 30
057900971023     C  N30              TESTN                   WD09                     30
058000971021     C                   MOVE      WD09          W001A             1
058100971021     C     *IN30         IFEQ      *OFF
058200971021     C     W001A         ORLT      '0'
058300971023     C     W001A         ANDNE     *BLANK
058400971021     C                   SETON                                        32
058500971021     C                   MOVE      *ZERO         VABPKB
058600010604     C                   eval      vinmsg = %trimr(vinmsg)
058700010604     C                             + ' ' + 'VHBPKB'
058800971021     C                   ELSE
058900971021     C                   MOVE      WD09          VABPKB
059000971021     C                   ENDIF
059100050118     C                   ENDIF
059200971021     C*
059300041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
059400041001     C                   IF        *IN51 = *OFF
059500971021     C                   MOVE      *ZERO         VABVLB
059600971021     C                   MOVE      VHBVLB        WASC07
059700010719     C                   IF        WI07 = *blanks
059800010719     C                   EVAL      WI07 = *zeros
059900010719     C                   ENDIF
060000971023     C                   TESTN                   WI07                 30
060100971023     C  N30              TESTN                   WI07                   30
060200971023     C  N30              TESTN                   WI07                     30
060300971021     C                   MOVE      WI07          W001A             1
060400971021     C     *IN30         IFEQ      *OFF
060500971021     C     W001A         ORLT      '0'
060600971023     C     W001A         ANDNE     *BLANK
060700971021     C                   SETON                                        32
060800971021     C                   MOVE      *ZERO         VABVLB
060900010604     C                   eval      vinmsg = %trimr(vinmsg)
061000010604     C                             + ' ' + 'VHBVLB'
061100971021     C                   ELSE
061200971021     C                   MOVEL     WI07          VABVLB
061300010719     C                   IF        WD07 = *blanks
061400010719     C                   EVAL      WD07 = *zeros
061500010719     C                   ENDIF
061600971023     C                   TESTN                   WD07                 30
061700971023     C  N30              TESTN                   WD07                   30
061800971023     C  N30              TESTN                   WD07                     30
061900971021     C                   MOVE      WD07          W001A             1
062000971021     C     *IN30         IFEQ      *OFF
062100971021     C     W001A         ORLT      '0'
062200971023     C     W001A         ANDNE     *BLANK
062300971021     C                   SETON                                        32
062400971021     C                   MOVE      *ZERO         VABVLB
062500010604     C                   eval      vinmsg = %trimr(vinmsg)
062600010604     C                             + ' ' + 'VHBVLB'
062700971021     C                   ELSE
062800971021     C                   MOVE      WD07          VABVLB
062900971021     C                   ENDIF
063000050118     C                   ENDIF
063100041001     C                   ELSE
063200041001     C* ...altrimenti forzo azzeramento del volume
063300041001     C                   Z-ADD     *zeros        VABVLB
063400041001     C                   ENDIF
063500941229     C*
063600971021     C                   MOVE      *ZERO         VABQFT
063700971021     C                   MOVE      VHBQFT        WASC15
063800010719     C                   IF        WI15 = *blanks
063900010719     C                   EVAL      WI15 = *zeros
064000010719     C                   ENDIF
064100971023     C                   TESTN                   WI15                 30
064200971023     C  N30              TESTN                   WI15                   30
064300971023     C  N30              TESTN                   WI15                     30
064400971021     C                   MOVE      WI15          W001A             1
064500971021     C     *IN30         IFEQ      *OFF
064600971021     C     W001A         ORLT      '0'
064700971023     C     W001A         ANDNE     *BLANK
064800971021     C                   SETON                                        32
064900971021     C                   MOVE      *ZERO         VABQFT
065000010604     C                   eval      vinmsg = %trimr(vinmsg)
065100010604     C                             + ' ' + 'VHBQFT'
065200971021     C                   ELSE
065300971021     C                   MOVEL     WI15          VABQFT
065400010719     C                   IF        WD15 = *blanks
065500010719     C                   EVAL      WD15 = *zeros
065600010719     C                   ENDIF
065700971023     C                   TESTN                   WD15                 30
065800971023     C  N30              TESTN                   WD15                   30
065900971023     C  N30              TESTN                   WD15                     30
066000971021     C                   MOVE      WD15          W001A             1
066100971021     C     *IN30         IFEQ      *OFF
066200971021     C     W001A         ORLT      '0'
066300971023     C     W001A         ANDNE     *BLANK
066400971021     C                   SETON                                        32
066500971021     C                   MOVE      *ZERO         VABQFT
066600010604     C                   eval      vinmsg = %trimr(vinmsg)
066700010604     C                             + ' ' + 'VHBQFT'
066800971021     C                   ELSE
066900971021     C                   MOVE      WD15          VABQFT
067000971021     C                   ENDIF
067100050118     C                   ENDIF
067200941229     C*
067300971021     C                   MOVE      *ZERO         VABCAS
067400971021     C                   MOVE      VHBCAS        WASC15
067500010719     C                   IF        WI15 = *blanks
067600010719     C                   EVAL      WI15 = *zeros
067700010719     C                   ENDIF
067800971023     C                   TESTN                   WI15                 30
067900971023     C  N30              TESTN                   WI15                   30
068000971023     C  N30              TESTN                   WI15                     30
068100971021     C                   MOVE      WI15          W001A             1
068200971021     C     *IN30         IFEQ      *OFF
068300971021     C     W001A         ORLT      '0'
068400971023     C     W001A         ANDNE     *BLANK
068500971021     C                   SETON                                        32
068600971021     C                   MOVE      *ZERO         VABCAS
068700010604     C                   eval      vinmsg = %trimr(vinmsg)
068800010604     C                             + ' ' + 'VHBCAS'
068900971021     C                   ELSE
069000971021     C                   MOVEL     WI15          VABCAS
069100010719     C                   IF        WD15 = *blanks
069200010719     C                   EVAL      WD15 = *zeros
069300010719     C                   ENDIF
069400971023     C                   TESTN                   WD15                 30
069500971023     C  N30              TESTN                   WD15                   30
069600971023     C  N30              TESTN                   WD15                     30
069700971021     C                   MOVE      WD15          W001A             1
069800971021     C     *IN30         IFEQ      *OFF
069900971021     C     W001A         ORLT      '0'
070000971023     C     W001A         ANDNE     *BLANK
070100971021     C                   SETON                                        32
070200971021     C                   MOVE      *ZERO         VABCAS
070300010604     C                   eval      vinmsg = %trimr(vinmsg)
070400010604     C                             + ' ' + 'VHBCAS'
070500971021     C                   ELSE
070600971021     C                   MOVE      WD15          VABCAS
070700971021     C                   ENDIF
070800050118     C                   ENDIF
070900971021     C*
071000971023     C                   TESTN                   VHBRMN               30
071100971023     C  N30              TESTN                   VHBRMN                 30
071200971023     C  N30              TESTN                   VHBRMN                   30
071300971021     C                   MOVE      VHBRMN        W001A             1
071400971021     C     *IN30         IFEQ      *OFF
071500971021     C     W001A         ORLT      '0'
071600971021     C                   SETON                                        32
071700971021     C                   MOVE      *ZERO         VABRMN
071800010604     C                   eval      vinmsg = %trimr(vinmsg)
071900010604     C                             + ' ' + 'VHBRMN'
072000971021     C                   ELSE
072100971021     C                   MOVE      VHBRMN        VABRMN
072200971021     C                   ENDIF
072300971021     C*
072400971023     C                   TESTN                   VHBNCD               30
072500971023     C  N30              TESTN                   VHBNCD                 30
072600971023     C  N30              TESTN                   VHBNCD                   30
072700971021     C                   MOVE      VHBNCD        W001A             1
072800971021     C     *IN30         IFEQ      *OFF
072900971021     C     W001A         ORLT      '0'
073000971023     C     W001A         ANDNE     *BLANK
073100971021     C                   SETON                                        32
073200971021     C                   MOVE      *ZERO         VABNCD
073300010604     C                   eval      vinmsg = %trimr(vinmsg)
073400010604     C                             + ' ' + 'VHBNCD'
073500971021     C                   ELSE
073600971021     C                   MOVE      VHBNCD        VABNCD
073700971021     C                   ENDIF
073800971021     C*
073900971023     C                   TESTN                   VHBNCA               30
074000971023     C  N30              TESTN                   VHBNCA                 30
074100971023     C  N30              TESTN                   VHBNCA                   30
074200971021     C                   MOVE      VHBNCA        W001A             1
074300971021     C     *IN30         IFEQ      *OFF
074400971021     C     W001A         ORLT      '0'
074500971023     C     W001A         ANDNE     *BLANK
074600971021     C                   SETON                                        32
074700971021     C                   MOVE      *ZERO         VABNCA
074800010604     C                   eval      vinmsg = %trimr(vinmsg)
074900010604     C                             + ' ' + 'VHBNCA'
075000971021     C                   ELSE
075100971021     C                   MOVE      VHBNCA        VABNCA
075200971021     C                   ENDIF
075300971021     C*
075400971023     C                   TESTN                   VHBZNC               30
075500971023     C  N30              TESTN                   VHBZNC                 30
075600971023     C  N30              TESTN                   VHBZNC                   30
075700971021     C                   MOVE      VHBZNC        W001A             1
075800971021     C     *IN30         IFEQ      *OFF
075900971021     C     W001A         ORLT      '0'
076000971023     C     W001A         ANDNE     *BLANK
076100971021     C                   SETON                                        32
076200971021     C                   MOVE      *ZERO         VABZNC
076300010604     C                   eval      vinmsg = %trimr(vinmsg)
076400010604     C                             + ' ' + 'VHBZNC'
076500971021     C                   ELSE
076600971021     C                   MOVE      VHBZNC        VABZNC
076700971021     C                   ENDIF
076800971021     C*
076900971023     C                   TESTN                   VHBDCR               30
077000971023     C  N30              TESTN                   VHBDCR                 30
077100971023     C  N30              TESTN                   VHBDCR                   30
077200971021     C                   MOVE      VHBDCR        W001A             1
077300971021     C     *IN30         IFEQ      *OFF
077400971021     C     W001A         ORLT      '0'
077500971023     C     W001A         ANDNE     *BLANK
077600971021     C                   SETON                                        32
077700971021     C                   MOVE      *ZERO         VABDCR
077800010604     C                   eval      vinmsg = %trimr(vinmsg)
077900010604     C                             + ' ' + 'VHBDCR'
078000971021     C                   ELSE
078100971021     C                   MOVE      VHBDCR        VABDCR
078200971021     C                   ENDIF
078300971021     C*
078400971023     C                   TESTN                   VHBHCR               30
078500971023     C  N30              TESTN                   VHBHCR                 30
078600971023     C  N30              TESTN                   VHBHCR                   30
078700971021     C                   MOVE      VHBHCR        W001A             1
078800971021     C     *IN30         IFEQ      *OFF
078900971021     C     W001A         ORLT      '0'
079000971023     C     W001A         ANDNE     *BLANK
079100971021     C                   SETON                                        32
079200971021     C                   MOVE      *ZERO         VABHCR
079300010604     C                   eval      vinmsg = %trimr(vinmsg)
079400010604     C                             + ' ' + 'VHBHCR'
079500971021     C                   ELSE
079600971021     C                   MOVE      VHBHCR        VABHCR
079700971021     C                   ENDIF
079800941229     C*
079900971021     C                   MOVE      *ZERO         VABVMD
080000971021     C                   MOVE      VHBVMD        WASC15
080100010719     C                   IF        WI15 = *blanks
080200010719     C                   EVAL      WI15 = *zeros
080300010719     C                   ENDIF
080400971023     C                   TESTN                   WI15                 30
080500971023     C  N30              TESTN                   WI15                   30
080600971023     C  N30              TESTN                   WI15                     30
080700971021     C                   MOVE      WI15          W001A             1
080800971021     C     *IN30         IFEQ      *OFF
080900971021     C     W001A         ORLT      '0'
081000971023     C     W001A         ANDNE     *BLANK
081100971021     C                   SETON                                        32
081200971021     C                   MOVE      *ZERO         VABVMD
081300010604     C                   eval      vinmsg = %trimr(vinmsg)
081400010604     C                             + ' ' + 'VHBVMD'
081500971021     C                   ELSE
081600971021     C                   MOVEL     WI15          VABVMD
081700010719     C                   IF        WD15 = *blanks
081800010719     C                   EVAL      WD15 = *zeros
081900010719     C                   ENDIF
082000971023     C                   TESTN                   WD15                 30
082100971023     C  N30              TESTN                   WD15                   30
082200971023     C  N30              TESTN                   WD15                     30
082300971021     C                   MOVE      WD15          W001A             1
082400971021     C     *IN30         IFEQ      *OFF
082500971021     C     W001A         ORLT      '0'
082600971023     C     W001A         ANDNE     *BLANK
082700971021     C                   SETON                                        32
082800971021     C                   MOVE      *ZERO         VABVMD
082900010604     C                   eval      vinmsg = %trimr(vinmsg)
083000010604     C                             + ' ' + 'VHBVMD'
083100971021     C                   ELSE
083200971021     C                   MOVE      WD15          VABVMD
083300971021     C                   ENDIF
083400050118     C                   ENDIF
083500971021     C*
083600971023     C                   TESTN                   VHBANT               30
083700971023     C  N30              TESTN                   VHBANT                 30
083800971023     C  N30              TESTN                   VHBANT                   30
083900971021     C                   MOVE      VHBANT        W001A             1
084000971021     C     *IN30         IFEQ      *OFF
084100971021     C     W001A         ORLT      '0'
084200971023     C     W001A         ANDNE     *BLANK
084300971021     C                   SETON                                        32
084400971021     C                   MOVE      *ZERO         VABANT
084500010604     C                   eval      vinmsg = %trimr(vinmsg)
084600010604     C                             + ' ' + 'VHBANT'
084700971021     C                   ELSE
084800971021     C                   MOVE      VHBANT        VABANT
084900971021     C                   ENDIF
085000971021     C*
085100971021     C* CAMPI ALFABETICI:
085200971021     C                   MOVE      VHBCBO        VABCBO
085300971021     C                   MOVE      VHBRSD        VABRSD
085400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
085500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
085600020117     C* ==
085700971021     C                   MOVE      VHBRD2        VABRD2
085800971021     C                   MOVE      VHBIND        VABIND
085900971021     C                   MOVE      VHBCAD        VABCAD
086000971021     C                   MOVE      VHBLOD        VABLOD
086100971021     C                   MOVE      VHBPRD        VABPRD
086200971021     C                   MOVE      VHBNZD        VABNZD
086300971021     C                   MOVE      VHBGC1        VABGC1
086400971021     C                   MOVE      VHBGC2        VABGC2
086500971021     C                   MOVE      VHBTSP        VABTSP
086600971021     C                   MOVE      VHBVAS        VABVAS
086700971021     C                   MOVE      VHBNAS        VABNAS
086800971021     C                   MOVE      VHBTIC        VABTIC
086900971021     C                   MOVE      VHBVCA        VABVCA
087000971021     C                   MOVE      VHBGCA        VABGCA
087100971021     C                   MOVE      VHBRMA        VABRMA
087200971021     C                   MOVE      VHBXCO        VABXCO
087300971021     C                   MOVE      VHBNOT        VABNOT
087400971021     C                   MOVE      VHBNT2        VABNT2
087500971021     C                   MOVE      VHBCTM        VABCTM
087600971021     C                   MOVE      VHBFFD        VABFFD
087700971021     C                   MOVE      VHBTCR        VABTCR
087800971021     C                   MOVE      VHBCTS        VABCTS
087900971021     C                   MOVE      VHBFTM        VABFTM
088000971021     C                   MOVE      VHBVAD        VABVAD
088100971021     C                   MOVE      VHBGMA        VABGMA
088200971021     C                   MOVE      VHBGGA        VABGGA
088300971021     C                   MOVE      VHBGVA        VABGVA
088400971021     C                   MOVE      VHBTC1        VABTC1
088500971021     C                   MOVE      VHBTC2        VABTC2
088600971021     C                   MOVE      VHBSCL        VABSCL
088700971021     C                   MOVE      VHBRMO        VABRMO
088800971021     C                   MOVE      VHBCMO        VABCMO
088900971021     C                   MOVE      VHBNMO        VABNMO
089000971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
089100971021     C* NELLA NAZIONE ORIGINALE
089200971021     C                   MOVE      VHBNMO        WA1D              1
089300971021     C                   MOVEL     VHBNMO        WA1S              1
089400971021     C     WA1D          IFNE      *BLANK
089500971021     C     WA1S          ANDEQ     *BLANK
089600971021     C                   MOVE      *BLANK        VABNMO
089700030616     C                   ENDIF
089800030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
089900030710     C                   EVAL      VABCMR = wCMR
090000030616     C                   EVAL      VABDCM = DATCOR
090100030616     C                   EVAL      VABDTS = DATCOR
090200040402     C                   EVAL      VABHMS = ORACOR
090300030616     C                   EVAL      VABCNT = 1
090400041025     C*
090500041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
090600041025     C                   IF        *IN52 = *ON
090700041025     C* Reperisco la provincia dal CAP e dalla località
090800041025     C                   IF        VABCAD <> *blanks AND
090900041025     C                             VABPRD  = *blanks
091000041025     C                   CLEAR                   TISI95DS
091100041025     C                   EVAL      I95TCN = '3'
091200041025     C                   Z-ADD     datcor        I95DAT
091300041025     C                   EVAL      I95CAP = VABCAD
091400041025     C                   EVAL      I95LOC = VABLOD
091500041025     C                   CALL      'TISI95R'
091600041025     C                   PARM                    TISI95DS
091700041025     C                   EVAL      VABPRD = O95PRV
091800041025     C                   ENDIF
091900041025     C                   ENDIF
092000050613     C*
092100050613     C                   else
092200050618     C*
092300050618     C* *** VAD ***
092400050618     C                   if        %subst(vindta:1:1) = '2'
092500050613     C*
092600050613     C                   clear                   EDIVAD00
092700050613     C*
092800050613     C* Reperimento campi ALFA
092900050613     C                   EVAL      VADCDP=%trim(%subst(vindta:25:15))
093000050613     C*
093100050613     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
093200050613     C                   EXSR      CHKNUM
093300050613     C                   IF        PiInt=*on
093400050613     C                   Z-ADD     PiVal         VADCCM
093500050613     C                   ELSE
093600050613     C                   SETON                                        33
093700050613     C                   EVAL      vinmsg = %trimr(vinmsg)
093800050613     C                             + ' ' + 'VADCCM'
093900050613     C                   ENDIF
094000050613     C*
094100050613     C                   EVAL      PiStr=%trim(%subst(vindta:9:3))
094200050613     C                   EXSR      CHKNUM
094300050613     C                   IF        PiInt=*on
094400050613     C                   Z-ADD     PiVal         VADLNP
094500050613     C                   ELSE
094600050613     C                   SETON                                        33
094700050613     C                   EVAL      vinmsg = %trimr(vinmsg)
094800050613     C                             + ' ' + 'VADLNP'
094900050613     C                   ENDIF
095000050613     C*
095100050613     C                   IF        %subst(vindta:12:4) <> *blanks
095200050613     C                   EVAL      PiStr=%trim(%subst(vindta:12:4))
095300050613     C                   EXSR      CHKNUM
095400050613     C                   IF        PiInt=*on
095500050613     C                   Z-ADD     PiVal         VADAAS
095600050613     C                   ELSE
095700050613     C                   SETON                                        33
095800050613     C                   EVAL      vinmsg = %trimr(vinmsg)
095900050613     C                             + ' ' + 'VADAAS'
096000050613     C                   ENDIF
096100050613     C                   ELSE
096200050613     C                   MOVEL     datcor        VADAAS
096300050613     C                   ENDIF
096400050613     C*
096500050613     C                   EVAL      PiStr=%trim(%subst(vindta:16:2))
096600050613     C                   EXSR      CHKNUM
096700050613     C                   IF        PiInt=*on
096800050613     C                   Z-ADD     PiVal         VADNRS
096900050613     C                   ELSE
097000050613     C                   SETON                                        33
097100050613     C                   Z-ADD     *zeros        VADNRS
097200050613     C                   EVAL      vinmsg = %trimr(vinmsg)
097300050613     C                             + ' ' + 'VADNRS'
097400050613     C                   ENDIF
097500050613     C*
097600050613     C                   EVAL      PiStr=%trim(%subst(vindta:18:7))
097700050613     C                   EXSR      CHKNUM
097800050613     C                   IF        PiInt=*on
097900050613     C                   Z-ADD     PiVal         VADNSP
098000050613     C                   ELSE
098100050613     C                   SETON                                        33
098200050613     C                   Z-ADD     *zeros        VADNSP
098300050613     C                   EVAL      vinmsg = %trimr(vinmsg)
098400050613     C                             + ' ' + 'VADNSP'
098500050613     C                   ENDIF
098600050613     C*
098700050613     C                   EVAL      PiStr=%trim(%subst(vindta:40:5))
098800050613     C                   EXSR      CHKNUM
098900050613     C                   IF        PiInt=*on
099000050613     C                   Z-ADD     PiVal         VADNCL
099100050613     C                   ELSE
099200050613     C                   SETON                                        34
099300050613     C                   Z-ADD     *zeros        VADNCL
099400050613     C                   EVAL      vinmsg = %trimr(vinmsg)
099500050613     C                             + ' ' + 'VADNCL'
099600050613     C                   ENDIF
099700050613     C*
099800050613     C                   EVAL      PiStr=%trim(%subst(vindta:45:7))
099900050613     C                   EXSR      CHKNUM
100000050613     C                   IF        PiInt=*on
100100050613     C                   Z-ADD     PiVal         VADNCD
100200050613     C                   ELSE
100300050613     C                   SETON                                        34
100400050613     C                   Z-ADD     *zeros        VADNCD
100500050613     C                   EVAL      vinmsg = %trimr(vinmsg)
100600050613     C                             + ' ' + 'VADNCD'
100700050613     C                   ENDIF
100800050613     C*
100900050613     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
101000050613     C                   EXSR      CHKNUM
101100050613     C                   IF        PiInt=*on
101200050613     C                   Z-ADD     PiVal         VADNCA
101300050613     C                   ELSE
101400050613     C                   SETON                                        34
101500050613     C                   Z-ADD     *zeros        VADNCA
101600050613     C                   EVAL      vinmsg = %trimr(vinmsg)
101700050613     C                             + ' ' + 'VADNCA'
101800050613     C                   ENDIF
101900050618     C*
102000050618     C* Effettuo considerazioni x elaborazioni "multi-filiale"
102100050618     C                   eval      depfil = VADLNP
102200050618     C                   exsr      repfil
102300050618     C                   if        depfil = invfil
102400050618     C                   if        vlrpoi = 999
102500050618     C                   MOVE(P)   invfil        VADFGS
102600050618     C                   else
102700050618     C                   MOVE(P)   vlrpoi        VADFGS
102800050618     C                   endif
102900050618     C                   endif
103000050618     C*
103100050613     C* VALORIZZO CAMPI RELATIVI AL "CMR"
103200050613     C                   EVAL      VADCMR = wCMR
103300050613     C                   EVAL      VADCNT = 1
103400050613     C*
103500050613     C* SCARICO IL BUFFER DEL RECORD D DETTAGLIO
103600050613     C  N33              ADD       1             §CTROK            7 0
103700050613     C   34              ADD       1             §CTRMO            7 0
103800050613     C   33              ADD       1             §CTRNO            7 0
103900050618     C  N33              WRITE     EDIVAD00
104000050613     C*
104100050613     C                   endif
104200050618     C*
104300050618     C* *** VAT ***
104400050618     C                   if        %subst(vindta:1:1) = '3'
104500050618     C*
104600050618     C                   clear                   EDIVAT00
104700050618     C*
104800050618     C                   eval      fnvataar = vindta
104900050618     C*
105000050618     C                   exsr      impvat
105100050618     C*
105200050618     C* Effettuo considerazioni x elaborazioni "multi-filiale"
105300050618     C                   eval      depfil = VATLNP
105400050618     C                   exsr      repfil
105500050618     C                   if        depfil = invfil
105600050618     C                   if        vlrpoi = 999
105700050618     C                   MOVE(p)   invfil        VATFGS
105800050618     C                   else
105900050618     C                   MOVE(p)   vlrpoi        VATFGS
106000050618     C                   endif
106100050618     C                   endif
106200050618     C*
106300050618     C* VALORIZZO CAMPI RELATIVI AL "CMR"
106400050618     C                   EVAL      VATCMR = wCMR
106500050618     C                   EVAL      VATCNT = 1
106600050618     C*
106700050618     C* SCARICO IL BUFFER DEL RECORD D DETTAGLIO
106800050618     C  N35              ADD       1             §CTROK            7 0
106900050618     C   36              ADD       1             §CTRMO            7 0
107000050618     C   35              ADD       1             §CTRNO            7 0
107100050618     C  N35              WRITE     EDIVAT00
107200050618     C*
107300050618     C                   endif
107400050708     C                   endif
107500990910     C*
107600010601     C                   if        *in31 = *off and
107700050613     C                             *in32 = *off and
107800050613     C                             *in33 = *off and
107900050618     C                             *in34 = *off and
108000050618     C                             *in35 = *off and
108100050618     C                             *in36 = *off
108200990910     C                   eval      vinflg = '1'
108300990910     C                   else
108400000621     C                   eval      recko = vindta
108500050613     C                   if        %open(titvl7ps)
108600030407     C   50              except    rigadett
108700010612     C                   endif
108800990910     C                   eval      vinflg = '2'
108900990910     C                   endif
109000010601     C*
109100010601     C                   endif
109200000905     C*
109300000905     C                   else
109400000905     C                   eval      vinflg = '1'
109500000905     C                   endif
109600000905     C*
109700000905     C  N70              update    tivin000
109800991022     C*
109900991022     C  N70              ENDdo
110000050613     C*
110100050613     C* Scarico l'ultima testata rimasta ancora in "canna"
110200050613     C  N31              ADD       1             §CTROK            7 0
110300050613     C   32              ADD       1             §CTRMO            7 0
110400050613     C   31              ADD       1             §CTRNO            7 0
110500050613     C  N31              WRITE     EDIVAB00
110600010530     C*
110700010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
110800010530     C                   if        cntNonEl = *zeros or
110900010530     C                             flgMulti = '0'
111000990910     C* Se non ci sono record con errori ...
111100010530     C                   if        §ctrno = 0 and
111200010601     C                             §ctrmo = 0 and
111300010530     C                             flgStato <> '2'
111400990910     C* ... restituisco esito OK.
111500990921     C                   eval      wrkesito = '0'
111600990910     C                   else
111700010601     C                   if        §ctrok > 0
111800990921     C                   eval      wrkesito = '1'
111900000710     C                   else
112000010615     C                   if        flgOk = '0'
112100010615     C                   eval      wrkesito = '2'
112200010615     C                   else
112300010615     C                   eval      wrkesito = '6'
112400010615     C                   endif
112500990910     C                   endif
112600000710     C                   endif
112700010530     C                   else
112800010530     C                   eval      wrkesito = '9'
112900010530     C                   endif
113000050613     C*
113100050613     C                   else
113200050613     C                   eval      wrkesito = '2'
113300050613     C                   endif
113400030710     C*
113500030710 X  1C                   else
113600030710     C                   eval      wrkesito = '8'
113700030710     C                   eval      flgGiro = '1'
113800030710 E  1C                   endif
113900010530     C*
114000010530     C                   if        vlrpoi <> 999
114100010530     C                   eval      invfil = vlrpoi
114200010530     C                   endif
114300990910     C*
114400990920     C                   if        §ctrok > 0
114500010530     C                             and invfil > *zeros
114600000613     C                   exsr      invio
114700990920     C                   endif
114800010612     C*
114900010612     C                   if        flgGiro = '1'
115000081126     C***                exsr      endela
115100010612     C                   endif
115200990920     C*
115300910830     C                   ENDSR
115400000613     C***
115500050618
115600050618
115700050618
115800050618     C*----------------------------------------------------*
115900050618     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
116000050618     C*----------------------------------------------------*
116100050618     C     IMPVAT        BEGSR
116200050618     C*
116300050618     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
116400050618     C                   TESTN                   a_vatccm             30
116500050618     C  N30              TESTN                   a_vatccm               30
116600050618     C  N30              TESTN                   a_vatccm                 30
116700050618     C                   MOVE      a_vatccm      W001A             1
116800050618     C     *IN30         IFEQ      *OFF
116900050618     C     W001A         ORLT      '0'
117000050618     C                   SETON                                        35
117100050618     C                   Z-ADD     *zeros        VATCCM
117200050618     C                   EVAL      vinmsg = 'VATCCM'
117300050618     C                   ELSE
117400050618     C                   MOVE      a_vatccm      VATCCM
117500050618     C     VATCCM        IFLE      *ZEROS
117600050618     C                   SETON                                        35
117700050618     C                   Z-ADD     *zeros        VATCCM
117800050618     C                   EVAL      vinmsg = 'VATCCM'
117900050618     C                   ENDIF
118000050618     C                   ENDIF
118100050618     C*
118200050618     C                   TESTN                   a_vatlnp             30
118300050618     C  N30              TESTN                   a_vatlnp               30
118400050618     C  N30              TESTN                   a_vatlnp                 30
118500050618     C                   MOVE      a_vatlnp      W001A             1
118600050618     C     *IN30         IFEQ      *OFF
118700050618     C     W001A         ORLT      '0'
118800050618     C                   SETON                                        35
118900050618     C                   Z-ADD     *zeros        VATLNP
119000050618     C                   EVAL      vinmsg = %trimr(vinmsg)
119100050618     C                             + ' ' + 'VATLNP'
119200050618     C                   ELSE
119300050618     C                   MOVE      a_vatlnp      VATLNP
119400050618     C     VATLNP        IFLE      *ZEROS
119500050618     C                   SETON                                        35
119600050618     C                   Z-ADD     *zeros        VATLNP
119700050618     C                   EVAL      vinmsg = %trimr(vinmsg)
119800050618     C                             + ' ' + 'VATLNP'
119900050618     C                   ENDIF
120000050618     C                   ENDIF
120100050618     C*
120200050618     C                   TESTN                   a_vataas             30
120300050618     C  N30              TESTN                   a_vataas               30
120400050618     C  N30              TESTN                   a_vataas                 30
120500050618     C                   MOVE      a_vataas      W001A             1
120600050618     C     *IN30         IFEQ      *OFF
120700050618     C     W001A         ORLT      '0'
120800050618     C                   SETON                                        35
120900050618     C                   MOVEL(P)  datcor        VATAAS
121000050618     C                   EVAL      vinmsg = %trimr(vinmsg)
121100050618     C                             + ' ' + 'VATAAS'
121200050618     C                   ELSE
121300050618     C                   MOVE      a_vataas      VATAAS
121400050618     C                   ENDIF
121500050618     C*
121600050618     C                   TESTN                   a_vatnrs             30
121700050618     C  N30              TESTN                   a_vatnrs               30
121800050618     C  N30              TESTN                   a_vatnrs                 30
121900050618     C                   MOVE      a_vatnrs      W001A             1
122000050618     C     *IN30         IFEQ      *OFF
122100050618     C     W001A         ORLT      '0'
122200050618     C     W001A         ANDNE     *BLANK
122300050618     C                   SETON                                        35
122400050618     C                   Z-ADD     *zeros        VATNRS
122500050618     C                   EVAL      vinmsg = %trimr(vinmsg)
122600050618     C                             + ' ' + 'VATNRS'
122700050618     C                   ELSE
122800050618     C                   MOVE      a_vatnrs      VATNRS
122900050618     C                   ENDIF
123000050618     C*
123100050618     C                   TESTN                   a_vatnsp             30
123200050618     C  N30              TESTN                   a_vatnsp               30
123300050618     C  N30              TESTN                   a_vatnsp                 30
123400050618     C                   MOVE      a_vatnsp      W001A             1
123500050618     C     *IN30         IFEQ      *OFF
123600050618     C     W001A         ORLT      '0'
123700050618     C                   SETON                                        35
123800050618     C                   Z-ADD     *zeros        VATNSP
123900050618     C                   EVAL      vinmsg = %trimr(vinmsg)
124000050618     C                             + ' ' + 'VATNSP'
124100050618     C                   ELSE
124200050618     C                   MOVE      a_vatnsp      VATNSP
124300050618     C                   ENDIF
124400050618     C*
124500050618     C*
124600050618     C* ALFABETICI
124700050618     C                   MOVE      a_vattrc      VATTRC
124800050618     C                   EVAL      VATNOT = %trim(a_vatnot)
124900050618     C*
125000050618     C                   ENDSR
125100050618     C*----------------------------------------------------*
125200050613
125300050613
125400050613
125500050613     C*----------------------------------------------------*
125600050618     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVADWR/EDIVATWR
125700050613     C*----------------------------------------------------*
125800050613     C     PREVAD        BEGSR
125900050613     C*
126000050618     C* Compongo il nome del membro da dare al EDIVADWR/EDIVATWR
126100050613     C                   eval      parmbr = vlrhdl
126200050613     C                   movel     'M'           parmbr
126300050613     C                   eval      parccm = %subst(vlrKSC:2:7)
126400050613     C                   eval      paropz = '1'
126500050613     C* Effettuo la chiamata al CLLE preposto
126600050618     C                   call(e)   'TITVEVDTC'
126700050613     C                   parm                    parccm
126800050613     C                   parm                    parmbr
126900050613     C                   parm                    paropz
127000050613     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
127100050613     C                   if        %error
127200050613     C                   movel     '1'           chkcall
127300050613     C                   else
127400050613     C                   movel     '0'           chkcall
127500050613     C                   endif
127600050613     C*
127700050613     C                   ENDSR
127800990920
127900050613
128000050613
128100050613     C*----------------------------------------------------*
128200050613     C*  CONTROLLO NUMERICITA' CAMPI
128300050613     C*----------------------------------------------------*
128400050613     C     CHKNUM        BEGSR
128500050613     C*
128600050613     C                   call(e)   'ISNUMERIC'
128700050613     C                   PARM                    PiStr            30
128800050613     C                   PARM      '.'           PiDecChr          1
128900050613     C                   PARM      *ZEROS        PiVal            30 9
129000050613     C                   PARM      '0'           PiInt             1
129100050613     C                   PARM      '0'           PiNum             1
129200050613     C                   IF        %error
129300050613     C                   EVAL      PiInt=*off
129400050613     C                   ENDIF
129500050613     C*
129600050613     C                   ENDSR
129700050613     C***
129800010530
129900010530
130000050613
130100010530
130200010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
130300010530     C     repfil        BEGSR
130400010530     C*
130500010530     C                   if        invfil = *zeros and
130600010530     C                             depfil > *zeros and
130700010613     C                             (vinflg = *blanks or
130800010613     C                              vinflg = *zeros)
130900010530     C
131000010530     C                   eval      invfil = depfil
131100010530     C                   endif
131200010530     C*
131300010530     C                   if        depfil <> invfil and
131400010530     C                             invfil > *zeros
131500010530     C                   eval      flgMulti = '1'
131600010530     C                   if        vinflg = *blanks
131700010530     C                   add       1             cntNonEl
131800010530     C                   endif
131900010530     C                   endif
132000010530     C*
132100010530     C                   if        vinflg = '2'
132200010530     C                   eval      flgStato = '2'
132300010530     C                   endif
132400010530     C*
132500010530     C                   ENDSR
132600010530     C***
132700010530
132800010530
132900010530
133000010530
133100990920      /TITLE Invio dei dati al punto operativo.
133200000613     C     invio         BEGSR
133300050613     C*
133400050613     C* 1° invio EDIVAD
133500050613     C                   reset                   dscmz
133600050613     C                   move      vlrpoi        cmzdst
133700050613     C                   eval      cmzfld = 'EDIVADWR'
133800050613     C                   eval      cmzmbd = parmbr
133900050613     C                   eval      %subst(cmzmbd:1:1) = 'M'
134000050613     C***                if        prmfir = *blanks
134100050613     C                   eval      cmzfla = 'EDIVAD0F'
134200050613     C                   eval      cmzmba = 'EDIVAD0F'
134300050613     C***                else
134400050613     C***                eval      cmzfla = prmfir
134500050613     C***                eval      cmzmba = prmfir
134600050613     C***                endif
134700050613     C                   eval      cmznrr = *zeros
134800050613     C                   move      §ctrok        cmznrr
134900050613     C                   eval      cmzlba = vlrfl1
135000050613     C                   call(e)   'TIS711C'
135100050613     C                   parm                    dscmz
135200050613     C                   parm      *blanks       esito
135300050613     C                   if        %error
135400050613     C                             or cmzerr = '1'
135500050613     C                             or esito  = '1'
135600050613     C                   eval      wrkesito = '3'
135700050613     C                   else
135800050618     C*
135900050618     C* 2° invio EDIVAT
136000050618     C                   reset                   dscmz
136100050618     C                   move      vlrpoi        cmzdst
136200050618     C                   eval      cmzfld = 'EDIVATWR'
136300050618     C                   eval      cmzmbd = parmbr
136400050618     C                   eval      %subst(cmzmbd:1:1) = 'M'
136500050618     C***                if        prmfir = *blanks
136600050618     C                   eval      cmzfla = 'EDIVAT0F'
136700050618     C                   eval      cmzmba = 'EDIVAT0F'
136800050618     C***                else
136900050618     C***                eval      cmzfla = prmfir
137000050618     C***                eval      cmzmba = prmfir
137100050618     C***                endif
137200050618     C                   eval      cmznrr = *zeros
137300050618     C                   move      §ctrok        cmznrr
137400050618     C                   eval      cmzlba = vlrfl1
137500050618     C                   call(e)   'TIS711C'
137600050618     C                   parm                    dscmz
137700050618     C                   parm      *blanks       esito
137800050618     C                   if        %error
137900050618     C                             or cmzerr = '1'
138000050618     C                             or esito  = '1'
138100050618     C                   eval      wrkesito = '3'
138200050618     C                   else
138300050613     C*
138400050618     C* 3° invio EDIVAB
138500050613     C                   reset                   dscmz
138600050613     C                   move      vlrpoi        cmzdst
138700050613     C                   eval      cmzfld = vlrfou
138800050613     C                   eval      cmzmbd = vlrhdl
138900050613     C                   eval      %subst(cmzmbd:1:1) = 'M'
139000050613     C***                if        prmfir = *blanks
139100050613     C                   eval      cmzfla = 'EDIVAB0F'
139200050613     C                   eval      cmzmba = 'EDIVAB0F'
139300050613     C***                else
139400050613     C***                eval      cmzfla = prmfir
139500050613     C***                eval      cmzmba = prmfir
139600050613     C***                endif
139700050613     C                   eval      cmznrr = *zeros
139800050613     C                   move      §ctrok        cmznrr
139900050613     C                   eval      cmzlba = vlrfl1
140000050613     C                   call(e)   'TIS711C'
140100050613     C                   parm                    dscmz
140200050613     C                   parm      *blanks       esito
140300050613     C                   if        %error
140400050613     C                             or cmzerr = '1'
140500050613     C                             or esito  = '1'
140600050613     C                   eval      wrkesito = '3'
140700050613     C                   endif
140800050613     C                   endif
140900050618     C                   endif
141000990920     C*
141100000613     C                   ENDSR
141200000613     C***
141300010530
141400010530
141500010530
141600010530
141700010530      /TITLE Invio dei dati al punto operativo.
141800010530     C     opeini        BEGSR
141900010530     C*
142000010530     C* Inizializzo flag e contatori operativi
142100010530     C                   movel     '0'           flgGiro           1
142200010530     C                   movel     '0'           flgMulti          1
142300010530     C                   movel     '1'           flgStato          1
142400010615     C                   movel     '0'           flgOk             1
142500010530     C                   z-add     *zeros        cntNonEl         10 0
142600010530     C                   z-add     *zeros        depfil            3 0
142700010530     C                   z-add     *zeros        invfil            3 0
142800010530     C*
142900010530     C                   ENDSR
143000010530     C***
143100080930
143200080930
143300080930
143400080930     C     *pssr         BEGSR
143500080930     C*
143600080930     C                   if        %open(tivin00r)
143700080930     C                   close     tivin00r
143800080930     C                   endif
143900080930     C                   if        %open(edivabwr)
144000080930     C                   close     edivabwr
144100080930     C                   endif
144200080930     C                   if        %open(edivadwr)
144300080930     C                   close     edivadwr
144400080930     C                   endif
144500080930     C                   if        %open(edivatwr)
144600080930     C                   close     edivatwr
144700080930     C                   endif
144800080930     C*
144900080930     C* Effettuo la chiamata al CLLE preposto
145000080930     C                   call(e)   'TITVEVDTC'
145100080930     C                   parm                    parccm
145200080930     C                   parm                    parmbr
145300080930     C                   parm      '2'           paropz
145400080930     C*
145500080930     C                   eval      wrkesito = '2'
145600080930     C*
145700080930     C                   seton                                        LR
145800080930     C*
145900080930     C                   ENDSR     '*CANCL'
146000080930     C***
146100080930
146200010530
146300990910
146400000613     C     *inzsr        BEGSR
146500990910     C*
146600990910     C     *entry        plist
146700990920     C                   parm                    tivlrds
146800990921     C                   parm      wrkesito      esito
146900000724     C                   parm                    prmlit
147000000710     C                   parm                    prmfir
147100030616     C*
147200030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
147300030710     C                   move(p)   vlrMSG        wNomeFile
147400030710     C*
147500030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
147600030710     C* del codice CMR dal nome del file importato.
147700030710     C                   if        wNomeFile <> *blanks AND
147800030710     C                             vlrppt<>*blanks
147900041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
148000030710     C                   if        %subst(vlrppt:1:1) = 'S'
148100030710     C                   seton                                        50
148200030710     C                   else
148300030710     C                   setoff                                       50
148400030710     C                   endif
148500041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
148600041001     C                   if        %subst(vlrppt:6:1) = 'S'
148700041001     C                   seton                                        51
148800041001     C                   else
148900041001     C                   setoff                                       51
149000041001     C                   endif
149100041025     C*
149200041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
149300041025     C* (naturalmente solo se nn già presente)
149400041025     C                   if        %subst(vlrppt:7:1) = 'S'
149500041025     C                   seton                                        52
149600041025     C                   else
149700041025     C                   setoff                                       52
149800041025     C                   endif
149900041001     C*
150000030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
150100030710     C                   move(p)   wPosDaA       wPosDa
150200030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
150300030710     C                   move(p)   wLungA        wLung
150400030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
150500030710     C                   seton                                        60
150600030710     C                   else
150700030710     C                   setoff                                       60
150800030710     C                   endif
150900010606     C*
151000010606     C* CALCOLA LA DATA CORRENTE
151100010606     C                   time                    wn14             14 0
151200010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
151300040402     C                   movel     wn14          oracor            6 0          *ORA
151400010606     C                   z-add     wn8           g08dat
151500010606     C                   z-add     *zeros        g08inv
151600010606     C                   movel     '0'           g08err
151700010606     C                   call      'XSRDA8'
151800010606     C                   parm                    wlbda8
151900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
152000000613     C*
152100000613     C                   ENDSR
152200000613     C***
152300990908
152400050613     Otitvl7p   E            riepilogo         2
152500990915     O                                              'Upload via Internet'
152600990915     O                                           +1 'Traduzione TIVIN00R -'
152700030616     O                                           +1 'EDIVABWR'
152800990915     O                                           +1 'Testate bolle'
152900990915     O          E            riepilogo   2
153000990915     O                       wrkdata
153100990915     O                       wrkora              +1
153200990915     O                       procname            +1
153300990915     O          E            riepilogo   2
153400990915     O                                              'Cliente..................:'
153500990915     O                       VABCCM        z     +1
153600030618     O          E            riepilogo   2
153700030618     O                                              'Numero CMR...............:'
153800030618     O                       VABCMR              +1
153900990915     O          E            riepilogo   2
154000990920     O                                              'Riferimento Strategi.....:'
154100990920     O                       vlrhdl              +1
154200990915     O          E            riepilogo   2
154300990915     O                                              'Giusti...................:'
154400971022     O                       §CTROK        2   +  1
154500990915     O          E            riepilogo   2
154600990915     O                                              'Sbagliati e corretti.....:'
154700971022     O                       §CTRMO        2   +  1
154800990915     O          E            riepilogo   2
154900990915     O                                              'Sbagliati e scartati.....:'
155000971022     O                       §CTRNO        2   +  1
155100000613
155200050613     Otitvl7ps  E            testdett          2
155300000613     O                                              'Upload via Internet'
155400000613     O                                           +1 'Traduzione TIVIN00R -'
155500030616     O                                           +1 'EDIVABWR'
155600000613     O                                           +1 'Testate bolle'
155700000616     O          E            testdett    3
155800000613     O                                           +2 'N° rec'
155900000613     O                                           +3 'Anteprima contenuto'
156000000616     O          E            rigadett    2
156100000613     O                       rrnum               +2
156200000621     O                       recko               +3
156300000616     O          E            findett     2
156400000613     O                       wrkdata
156500000613     O                       wrkora              +1
156600000613     O                       procname            +1
156700000616     O          E            findett     2
156800000613     O                                              'Cliente..................:'
156900000613     O                       VABCCM        z     +1
157000030618     O          E            findett     2
157100030618     O                                              'Numero CMR...............:'
157200030618     O                       VABCMR              +1
157300000616     O          E            findett     2
157400000613     O                                              'Riferimento Strategi.....:'
157500000613     O                       vlrhdl              +1
157600000616     O          E            findett     2
157700000613     O                                              'Giusti...................:'
157800000613     O                       §CTROK        2   +  1
157900000616     O          E            findett     2
158000000613     O                                              'Sbagliati e corretti.....:'
158100000613     O                       §CTRMO        2   +  1
158200000616     O          E            findett     2
158300000613     O                                              'Sbagliati e scartati.....:'
158400000613     O                       §CTRNO        2   +  1
158500000512** CMD - COMANDI CL
158600050613OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITVL7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
158700050613OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITVL7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
158800050613DLTOVR FILE(TITVL7P TITVL7PS) LVL(*)
158900000512
159000000512
