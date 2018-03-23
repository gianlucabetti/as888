000100050303      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente RS Components - Cod. 0051406)
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020913     FFIVABwwr  O    E             DISK    usropn
000700050209     FFIVATwwr  O    E             DISK    usropn
000800050303     Ftitvi5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000050303     Ftitvi5ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200990908
001300000512     D*------------
001400000512     D* COMANDI
001500000512     D*------------
001600011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000512     D*------------
001800990920     D dscmz         e ds                  inz
001900990910     D psds           sds
002000990910     D  procname         *PROC
002100060308     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002200060308     D* 10+3
002300060308     D                 DS
002400060308     D  WI15                   1     10
002500060308     D  WD15                  11     13
002600060308     D  WASC15                 1     13
002700060308     D*  6+1
002800060308     D                 DS
002900060308     D  WI09                   1      6
003000060308     D  WD09                   7      7
003100060308     D  WASC09                 1      7
003200060308     D*  2+3
003300060308     D                 DS
003400060308     D  WI07                   1      2
003500060308     D  WD07                   3      5
003600060308     D  WASC07                 1      5
003700990920     D tivlrds       e ds                  extname(tivlr00f)
003800990910     D esito           s              1
003900000724     D prmlit          s             10
004000000710     D prmfir          s             10
004100990921     D wrkesito        s                   like(esito)
004200990915     D wrkdata         s               d
004300990915     D wrkora          s               t
004400000613     D rrnum           s              6  0 INZ(*zeros)
004500000621     D recko           s            150    INZ(*blanks)
004600011123     D depcmd          s            150    INZ(*blanks)
004700050209     D parccm          s              8    INZ(*blanks)
004800050209     D parmbr          s             10    INZ(*blanks)
004900050209     D paropz          s              1    INZ(*blanks)
005000050209     D chkcall         s              1    INZ(*blanks)
005100010606     D*------------------
005200010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005300010606     D*------------------
005400010606     D WLBDA8          DS                  INZ
005500010606     D  G08DAT                 1      8  0
005600010606     D  G08INV                 9     16  0
005700010606     D  G08ERR                17     17
005800010606     D  G08TGI                18     22  0
005900050209     D*------------------
006000050209     D* DS REPERIMENTO NUMERATORE
006100050209     D*------------------
006200050303     D trul33ds      e ds                  inz
006300050209     D*------------------
006400050209     D* DS ARCHITETTURA
006500050209     D*------------------
006600050209     D kpjba         e ds                  inz
006700050209     D*------------------
006800060308     D*---------------------------------------------------------
006900060308     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
007000060308     D*---------------------------------------------------------
007100060308     D                 ds
007200060308     D vhbatb                  1      1
007300060308     D vhbccm                  2      8
007400060308     D vhblnp                  9     11
007500060308     D vhbaas                 12     15
007600060308     D vhbmgs                 16     19
007700060308     D vhbnrs                 20     21
007800060308     D vhbnsp                 22     28
007900060308     D vhbcbo                 29     30
008000060308     D vhblna                 31     33
008100060308     D vhbrsd                 34     68
008200060308     D vhbrd2                 69    103
008300060308     D vhbind                104    138
008400060308     D vhbcad                139    147
008500060308     D vhblod                148    182
008600060308     D vhbprd                183    184
008700060308     D vhbnzd                185    187
008800060308     D vhbgc1                188    189
008900060308     D vhbgc2                190    191
009000060308     D vhbctr                192    194
009100060308     D vhbtsp                195    195
009200060308     D vhbias                196    208
009300060308     D vhbvas                209    211
009400060308     D vhbnas                212    226
009500060308     D vhbncl                227    231
009600060308     D vhbpkb                232    238
009700060308     D vhbvlb                239    243
009800060308     D vhbqft                244    256
009900060308     D vhbcas                257    269
010000060308     D vhbtic                270    271
010100060308     D vhbvca                272    274
010200060308     D vhbgca                275    276
010300060308     D vhbrmn                277    291
010400060308     D vhbrma                292    306
010500060308     D vhbncd                307    313
010600060308     D vhbnca                314    320
010700060308     D vhbxco                321    321
010800060308     D vhbnot                322    356
010900060308     D vhbnt2                357    391
011000060308     D vhbznc                392    393
011100060308     D vhbctm                394    395
011200060308     D vhbffd                396    396
011300060308     D vhbdcr                397    404
011400060308     D vhbtcr                405    405
011500060308     D vhbhcr                406    409
011600060308     D vhbcts                410    411
011700060308     D vhbftm                412    412
011800060308     D vhbvmd                413    425
011900060308     D vhbvad                426    428
012000060308     D vhbgma                429    430
012100060308     D vhbgga                431    432
012200060308     D vhbgva                433    434
012300060308     D vhbtc1                435    435
012400060308     D vhbtc2                436    436
012500060308     D vhbscl                437    437
012600060308     D vhbant                438    446
012700060308     D vhbrmo                447    471
012800060308     D vhbcmo                472    480
012900060308     D vhbnmo                481    483
013000060308     D  X_rec                  1    483    inz
013100990908
013200050209
013300050209
013400050209     C* /MAIN
013500990915     C                   time                    wrkdata
013600990915     C                   time                    wrkora
013700000913     C                   reset                   rrnum
013800990921     C                   reset                   esito
013900990921     C                   reset                   wrkesito
014000010530     C*
014100010530     C                   exsr      opeini
014200010530     C                   exsr      rwvab
014300050209     C*
014400050209     C* Effettuo la chiamata al CLLE preposto
014500050209     C                   call(e)   'TITVVTC'
014600050209     C                   parm                    parccm
014700050209     C                   parm                    parmbr
014800050209     C                   parm      '2'           paropz
014900010530     C*
015000010530     C                   seton                                        lr
015100010530
015200010530
015300010530
015400010530
015500010530     C*--------------------------------------------------------
015600010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
015700010530     C*--------------------------------------------------------
015800010530     C     PREELA        BEGSR
015900000724     C*
016000000724     C* SE OCCORRE SPEDIRE IN FILIALE
016100010530     C                   if        invfil <> *zeros and
016200010530     C                             flgGiro = '0'
016300010530     C*
016400010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
016500010530     C                   eval      flgGiro = '1'
016600000724     C*
016700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
016800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016900010530     C     invfil        chain     azorg01l
017000000724     C                   if        %found
017100000616     C                   movel(p)  CMD(1)        depcmd
017200020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
017300000616     C*
017400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
017500011123     C                   Z-ADD     150           LENGH            15 5
017600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017700000616     C                   PARM                    depcmd
017800000616     C                   PARM                    LENGH
017900000724     C*
018000000724     C                   endif
018100000616     C*
018200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
018300000616     C                   movel(p)  CMD(2)        depcmd
018400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
018500000616     C*
018600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
018700011123     C                   Z-ADD     150           LENGH            15 5
018800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018900000616     C                   PARM                    depcmd
019000000616     C                   PARM                    LENGH
019100000616     C*
019200050303     C                   if        not %open(titvi5ps)
019300050303     C                   open      titvi5ps
019400030407     C   50              except    testdett
019500000613     C                   endif
019600010530     C*
019700010530     C                   endif
019800010530     C*
019900010530     C                   ENDSR
020000010530     C***
020100010530
020200010530
020300010530
020400010530
020500010530
020600010530
020700010530     C*--------------------------------------------------------
020800010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
020900010530     C*--------------------------------------------------------
021000010530     C     ENDELA        BEGSR
021100010530     C*
021200010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
021300000613     C*
021400050303     C                   if        %open(titvi5ps)
021500030407     C   50              except    findett
021600050303     C                   close     titvi5ps
021700000613     C                   endif
021800000616     C*
021900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
022000011123     C                   Z-ADD     150           LENGH            15 5
022100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
022200000616     C                   PARM                    CMD(3)
022300000616     C                   PARM                    LENGH
022400010530     C*
022500010530     C                   ENDSR
022600010530     C***
022700000613
022800010530
022900010530
023000000613
023100000613     C*--------------------------------------------------------
023200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
023300000613     C*--------------------------------------------------------
023400000621     C     STPR          BEGSR
023500000613     C*
023600050303     C                   if        not %open(titvi5p)
023700050303     C                   open      titvi5p
023800990915     C                   endif
023900990915     C*
024000030407     C   50              except    riepilogo
024100990915     C*
024200050303     C                   if        %open(titvi5p)
024300050303     C                   close     titvi5p
024400990914     C                   endif
024500990910     C*
024600000613     C                   ENDSR
024700000613     C***
024800990908
024900010530
025000010530
025100010530
025200910830     C*--------------------------------------------------------
025300020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
025400910830     C*--------------------------------------------------------
025500941229     C     RWVAB         BEGSR
025600990910     C*
025700990914     C                   if        not %open(tivin00r)
025800990908     C                   open      tivin00r
025900990914     C                   endif
026000020913     C                   if        not %open(fivabwwr)
026100020913     C                   open      fivabwwr
026200990914     C                   endif
026300050209     C*
026400050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
026500050209     C                   exsr      prevat
026600050209     C*
026700050209     C                   if        chkcall = '0'
026800050209     C*
026900050209     C                   if        not %open(fivatwwr)
027000050209     C                   open      fivatwwr
027100050209     C                   endif
027200050209     C*
027300990910     C                   clear                   §CTROK
027400990910     C                   clear                   §CTRMO
027500990910     C                   clear                   §CTRNO
027600990910     C*
027700921023     C                   DO        *HIVAL
027800990913     C*
027900990915     C                   READ      tivin00r                               70
028000010618     C*
028100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
028200010618     C                   if        vinflg = '1'
028300010618     C                   eval      flgOk = '1'
028400010618     C                   endif
028500010705     C*
028600010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
028700010705     C                   if        %subst(vindta:1:1) = '='
028800010705     C                   eval      %subst(vindta:1:1) = *blanks
028900010705     C                   endif
029000010705     C*
029100000905     C                   if        vindta > *blanks
029200000613     C                   add       1             rrnum
029300010601     C* ...elimino eventuali segni
029400010601     C     '+':' '       xlate     vindta        vindta
029500990913     C*
029600010601     C                   if        *in70 = *off and
029700990913     C                             (vinflg = *blanks
029800010711     C                              or vinflg = '0'
029900010711     C                              or vinflg = '2')
030000990913     C*
030100020913     C                   clear                   FIVAB000
030200050209     C                   clear                   FIVAT000
030300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
030400010711     C                   if        vinflg = *blanks or vinflg = '0'
030500010711     C                   clear                   vinmsg
030600010711     C                   endif
030700010711     C*
030800060308     C                   eval      x_rec  = vindta
030900991022     C*
031000991022     C                   testb     x'3F'         vhbatb                   02
031100991022     C                   if        not *in02
031200991022     C*
031300971021     C                   SETOFF                                       303132
031400971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
031500971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
031600971023     C*    OPPURE TUTTI BLANKS
031700010530     C*
031800010530     C                   TESTN                   VHBLNP               30
031900010530     C  N30              TESTN                   VHBLNP                 30
032000010530     C  N30              TESTN                   VHBLNP                   30
032100010530     C                   MOVE      VHBLNP        W001A             1
032200010530     C     *IN30         IFEQ      *OFF
032300010530     C     W001A         ORLT      '0'
032400010530     C                   SETON                                        31
032500010530     C                   eval      vinmsg = %trimr(vinmsg)
032600010530     C                             + ' ' + 'VHBLNP'
032700010530     C                   ELSE
032800010530     C                   MOVE      VHBLNP        VABLNP
032900010530     C     VABLNP        IFLE      *ZEROS
033000010530     C                   SETON                                        31
033100010530     C                   eval      vinmsg = %trimr(vinmsg)
033200010530     C                             + ' ' + 'VHBLNP'
033300010530     C                   ENDIF
033400010530     C                   ENDIF
033500010530     C*
033600010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
033700010530     C                   eval      depfil = VABLNP
033800010530     C                   exsr      repfil
033900010601     C                   if        depfil = invfil
034000021014     C                   if        vlrpoi = 999
034100021010     C                   MOVE(P)   invfil        VABFGS
034200021014     C                   else
034300021014     C                   MOVE(P)   vlrpoi        VABFGS
034400021014     C                   endif
034500010530     C*
034600010530     C                   exsr      PREELA
034700010530     C*
034800971023     C                   TESTN                   VHBCCM               30
034900971023     C  N30              TESTN                   VHBCCM                 30
035000971023     C  N30              TESTN                   VHBCCM                   30
035100971021     C                   MOVE      VHBCCM        W001A             1
035200971021     C     *IN30         IFEQ      *OFF
035300971021     C     W001A         ORLT      '0'
035400971021     C                   SETON                                        31
035500010604     C                   eval      vinmsg = %trimr(vinmsg)
035600010604     C                             + ' ' + 'VHBCCM'
035700971021     C                   ELSE
035800971021     C                   MOVE      VHBCCM        VABCCM
035900971024     C     VABCCM        IFLE      *ZEROS
036000971024     C                   SETON                                        31
036100010604     C                   eval      vinmsg = %trimr(vinmsg)
036200010604     C                             + ' ' + 'VHBCCM'
036300971024     C                   ENDIF
036400971021     C                   ENDIF
036500971021     C*
036600971023     C                   TESTN                   VHBAAS               30
036700971023     C  N30              TESTN                   VHBAAS                 30
036800971023     C  N30              TESTN                   VHBAAS                   30
036900971021     C                   MOVE      VHBAAS        W001A             1
037000971021     C     *IN30         IFEQ      *OFF
037100971021     C     W001A         ORLT      '0'
037200971023     C     W001A         ANDNE     *BLANK
037300971021     C                   SETON                                        32
037400010606     C                   MOVEL     datcor        VABAAS
037500010604     C                   eval      vinmsg = %trimr(vinmsg)
037600010604     C                             + ' ' + 'VHBAAS'
037700971021     C                   ELSE
037800971021     C                   MOVE      VHBAAS        VABAAS
037900971021     C                   ENDIF
038000971021     C*
038100971023     C                   TESTN                   VHBMGS               30
038200971023     C  N30              TESTN                   VHBMGS                 30
038300971023     C  N30              TESTN                   VHBMGS                   30
038400971021     C                   MOVE      VHBMGS        W001A             1
038500971021     C     *IN30         IFEQ      *OFF
038600971021     C     W001A         ORLT      '0'
038700971023     C     W001A         ANDNE     *BLANK
038800971021     C                   SETON                                        32
038900010601     C                   MOVE      '0101'        VABMGS
039000010604     C                   eval      vinmsg = %trimr(vinmsg)
039100010604     C                             + ' ' + 'VHBMGS'
039200971021     C                   ELSE
039300971021     C                   MOVE      VHBMGS        VABMGS
039400971021     C                   ENDIF
039500971021     C*
039600971023     C                   TESTN                   VHBNRS               30
039700971023     C  N30              TESTN                   VHBNRS                 30
039800971023     C  N30              TESTN                   VHBNRS                   30
039900971021     C                   MOVE      VHBNRS        W001A             1
040000971021     C     *IN30         IFEQ      *OFF
040100971021     C     W001A         ORLT      '0'
040200971023     C     W001A         ANDNE     *BLANK
040300971021     C                   SETON                                        32
040400971021     C                   MOVE      *ZERO         VABNRS
040500010604     C                   eval      vinmsg = %trimr(vinmsg)
040600010604     C                             + ' ' + 'VHBNRS'
040700971021     C                   ELSE
040800971021     C                   MOVE      VHBNRS        VABNRS
040900971021     C                   ENDIF
041000971021     C*
041100971023     C                   TESTN                   VHBNSP               30
041200971023     C  N30              TESTN                   VHBNSP                 30
041300971023     C  N30              TESTN                   VHBNSP                   30
041400971021     C                   MOVE      VHBNSP        W001A             1
041500971021     C     *IN30         IFEQ      *OFF
041600971021     C     W001A         ORLT      '0'
041700971023     C     W001A         ANDNE     *BLANK
041800971021     C                   SETON                                        32
041900971021     C                   MOVE      *ZERO         VABNSP
042000050303     C                   MOVE      *ZERO         VATNSP
042100010604     C                   eval      vinmsg = %trimr(vinmsg)
042200010604     C                             + ' ' + 'VHBNSP'
042300971021     C                   ELSE
042400971021     C                   MOVE      VHBNSP        VABNSP
042500050303     C                   MOVE      VHBNSP        VATNSP
042600971021     C                   ENDIF
042700971021     C*
042800971023     C                   TESTN                   VHBLNA               30
042900971023     C  N30              TESTN                   VHBLNA                 30
043000971023     C  N30              TESTN                   VHBLNA                   30
043100971021     C                   MOVE      VHBLNA        W001A             1
043200971021     C     *IN30         IFEQ      *OFF
043300971021     C     W001A         ORLT      '0'
043400971023     C     W001A         ANDNE     *BLANK
043500971021     C                   SETON                                        32
043600971021     C                   MOVE      *ZERO         VABLNA
043700010604     C                   eval      vinmsg = %trimr(vinmsg)
043800010604     C                             + ' ' + 'VHBLNA'
043900971021     C                   ELSE
044000971021     C                   MOVE      VHBLNA        VABLNA
044100971021     C                   ENDIF
044200971021     C*
044300971023     C                   TESTN                   VHBCTR               30
044400971023     C  N30              TESTN                   VHBCTR                 30
044500971023     C  N30              TESTN                   VHBCTR                   30
044600971021     C                   MOVE      VHBCTR        W001A             1
044700971021     C     *IN30         IFEQ      *OFF
044800971021     C     W001A         ORLT      '0'
044900971023     C     W001A         ANDNE     *BLANK
045000971021     C                   SETON                                        32
045100971021     C                   MOVE      *ZERO         VABCTR
045200010604     C                   eval      vinmsg = %trimr(vinmsg)
045300010604     C                             + ' ' + 'VHBCTR'
045400971021     C                   ELSE
045500971021     C                   MOVE      VHBCTR        VABCTR
045600971021     C                   ENDIF
045700941229     C*
045800971021     C                   MOVE      *ZERO         VABIAS
045900941229     C                   MOVE      VHBIAS        WASC15
046000971023     C                   TESTN                   WI15                 30
046100971023     C  N30              TESTN                   WI15                   30
046200971023     C  N30              TESTN                   WI15                     30
046300971021     C                   MOVE      WI15          W001A             1
046400971021     C     *IN30         IFEQ      *OFF
046500971021     C     W001A         ORLT      '0'
046600971023     C     W001A         ANDNE     *BLANK
046700971021     C                   SETON                                        32
046800971021     C                   MOVE      *ZERO         VABIAS
046900010604     C                   eval      vinmsg = %trimr(vinmsg)
047000010604     C                             + ' ' + 'VHBIAS'
047100971021     C                   ELSE
047200971021     C                   MOVEL     WI15          VABIAS
047300971023     C                   TESTN                   WD15                 30
047400971023     C  N30              TESTN                   WD15                   30
047500971023     C  N30              TESTN                   WD15                     30
047600971021     C                   MOVE      WD15          W001A             1
047700971021     C     *IN30         IFEQ      *OFF
047800971021     C     W001A         ORLT      '0'
047900971023     C     W001A         ANDNE     *BLANK
048000971021     C                   SETON                                        32
048100971021     C                   MOVE      *ZERO         VABIAS
048200010604     C                   eval      vinmsg = %trimr(vinmsg)
048300010604     C                             + ' ' + 'VHBIAS'
048400971021     C                   ELSE
048500971021     C                   MOVE      WD15          VABIAS
048600971021     C                   ENDIF
048700050118     C                   ENDIF
048800941229     C*
048900971023     C                   TESTN                   VHBNCL               30
049000971023     C  N30              TESTN                   VHBNCL                 30
049100971023     C  N30              TESTN                   VHBNCL                   30
049200971021     C                   MOVE      VHBNCL        W001A             1
049300971021     C     *IN30         IFEQ      *OFF
049400971021     C     W001A         ORLT      '0'
049500971021     C                   SETON                                        32
049600971021     C                   MOVE      *ZERO         VABNCL
049700010604     C                   eval      vinmsg = %trimr(vinmsg)
049800010604     C                             + ' ' + 'VHBNCL'
049900971021     C                   ELSE
050000971021     C                   MOVE      VHBNCL        VABNCL
050100971021     C                   ENDIF
050200941229     C*
050300971021     C                   MOVE      *ZERO         VABPKB
050400971021     C                   MOVE      VHBPKB        WASC09
050500010719     C                   IF        WI09 = *blanks
050600010719     C                   EVAL      WI09 = *zeros
050700010719     C                   ENDIF
050800971023     C                   TESTN                   WI09                 30
050900971023     C  N30              TESTN                   WI09                   30
051000971023     C  N30              TESTN                   WI09                     30
051100971021     C                   MOVE      WI09          W001A             1
051200971021     C     *IN30         IFEQ      *OFF
051300971021     C     W001A         ORLT      '0'
051400971021     C                   SETON                                        32
051500971021     C                   MOVE      *ZERO         VABPKB
051600010604     C                   eval      vinmsg = %trimr(vinmsg)
051700010604     C                             + ' ' + 'VHBPKB'
051800971021     C                   ELSE
051900971021     C                   MOVEL     WI09          VABPKB
052000010719     C                   IF        WD09 = *blanks
052100010719     C                   EVAL      WD09 = *zeros
052200010719     C                   ENDIF
052300971023     C                   TESTN                   WD09                 30
052400971023     C  N30              TESTN                   WD09                     30
052500971021     C                   MOVE      WD09          W001A             1
052600971021     C     *IN30         IFEQ      *OFF
052700971021     C     W001A         ORLT      '0'
052800971023     C     W001A         ANDNE     *BLANK
052900971021     C                   SETON                                        32
053000971021     C                   MOVE      *ZERO         VABPKB
053100010604     C                   eval      vinmsg = %trimr(vinmsg)
053200010604     C                             + ' ' + 'VHBPKB'
053300971021     C                   ELSE
053400971021     C                   MOVE      WD09          VABPKB
053500971021     C                   ENDIF
053600050118     C                   ENDIF
053700971021     C*
053800041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
053900041001     C                   IF        *IN51 = *OFF
054000971021     C                   MOVE      *ZERO         VABVLB
054100971021     C                   MOVE      VHBVLB        WASC07
054200010719     C                   IF        WI07 = *blanks
054300010719     C                   EVAL      WI07 = *zeros
054400010719     C                   ENDIF
054500971023     C                   TESTN                   WI07                 30
054600971023     C  N30              TESTN                   WI07                   30
054700971023     C  N30              TESTN                   WI07                     30
054800971021     C                   MOVE      WI07          W001A             1
054900971021     C     *IN30         IFEQ      *OFF
055000971021     C     W001A         ORLT      '0'
055100971023     C     W001A         ANDNE     *BLANK
055200971021     C                   SETON                                        32
055300971021     C                   MOVE      *ZERO         VABVLB
055400010604     C                   eval      vinmsg = %trimr(vinmsg)
055500010604     C                             + ' ' + 'VHBVLB'
055600971021     C                   ELSE
055700971021     C                   MOVEL     WI07          VABVLB
055800010719     C                   IF        WD07 = *blanks
055900010719     C                   EVAL      WD07 = *zeros
056000010719     C                   ENDIF
056100971023     C                   TESTN                   WD07                 30
056200971023     C  N30              TESTN                   WD07                   30
056300971023     C  N30              TESTN                   WD07                     30
056400971021     C                   MOVE      WD07          W001A             1
056500971021     C     *IN30         IFEQ      *OFF
056600971021     C     W001A         ORLT      '0'
056700971023     C     W001A         ANDNE     *BLANK
056800971021     C                   SETON                                        32
056900971021     C                   MOVE      *ZERO         VABVLB
057000010604     C                   eval      vinmsg = %trimr(vinmsg)
057100010604     C                             + ' ' + 'VHBVLB'
057200971021     C                   ELSE
057300971021     C                   MOVE      WD07          VABVLB
057400971021     C                   ENDIF
057500050118     C                   ENDIF
057600041001     C                   ELSE
057700041001     C* ...altrimenti forzo azzeramento del volume
057800041001     C                   Z-ADD     *zeros        VABVLB
057900041001     C                   ENDIF
058000941229     C*
058100971021     C                   MOVE      *ZERO         VABQFT
058200971021     C                   MOVE      VHBQFT        WASC15
058300010719     C                   IF        WI15 = *blanks
058400010719     C                   EVAL      WI15 = *zeros
058500010719     C                   ENDIF
058600971023     C                   TESTN                   WI15                 30
058700971023     C  N30              TESTN                   WI15                   30
058800971023     C  N30              TESTN                   WI15                     30
058900971021     C                   MOVE      WI15          W001A             1
059000971021     C     *IN30         IFEQ      *OFF
059100971021     C     W001A         ORLT      '0'
059200971023     C     W001A         ANDNE     *BLANK
059300971021     C                   SETON                                        32
059400971021     C                   MOVE      *ZERO         VABQFT
059500010604     C                   eval      vinmsg = %trimr(vinmsg)
059600010604     C                             + ' ' + 'VHBQFT'
059700971021     C                   ELSE
059800971021     C                   MOVEL     WI15          VABQFT
059900010719     C                   IF        WD15 = *blanks
060000010719     C                   EVAL      WD15 = *zeros
060100010719     C                   ENDIF
060200971023     C                   TESTN                   WD15                 30
060300971023     C  N30              TESTN                   WD15                   30
060400971023     C  N30              TESTN                   WD15                     30
060500971021     C                   MOVE      WD15          W001A             1
060600971021     C     *IN30         IFEQ      *OFF
060700971021     C     W001A         ORLT      '0'
060800971023     C     W001A         ANDNE     *BLANK
060900971021     C                   SETON                                        32
061000971021     C                   MOVE      *ZERO         VABQFT
061100010604     C                   eval      vinmsg = %trimr(vinmsg)
061200010604     C                             + ' ' + 'VHBQFT'
061300971021     C                   ELSE
061400971021     C                   MOVE      WD15          VABQFT
061500971021     C                   ENDIF
061600050118     C                   ENDIF
061700941229     C*
061800971021     C                   MOVE      *ZERO         VABCAS
061900971021     C                   MOVE      VHBCAS        WASC15
062000010719     C                   IF        WI15 = *blanks
062100010719     C                   EVAL      WI15 = *zeros
062200010719     C                   ENDIF
062300971023     C                   TESTN                   WI15                 30
062400971023     C  N30              TESTN                   WI15                   30
062500971023     C  N30              TESTN                   WI15                     30
062600971021     C                   MOVE      WI15          W001A             1
062700971021     C     *IN30         IFEQ      *OFF
062800971021     C     W001A         ORLT      '0'
062900971023     C     W001A         ANDNE     *BLANK
063000971021     C                   SETON                                        32
063100971021     C                   MOVE      *ZERO         VABCAS
063200010604     C                   eval      vinmsg = %trimr(vinmsg)
063300010604     C                             + ' ' + 'VHBCAS'
063400971021     C                   ELSE
063500971021     C                   MOVEL     WI15          VABCAS
063600010719     C                   IF        WD15 = *blanks
063700010719     C                   EVAL      WD15 = *zeros
063800010719     C                   ENDIF
063900971023     C                   TESTN                   WD15                 30
064000971023     C  N30              TESTN                   WD15                   30
064100971023     C  N30              TESTN                   WD15                     30
064200971021     C                   MOVE      WD15          W001A             1
064300971021     C     *IN30         IFEQ      *OFF
064400971021     C     W001A         ORLT      '0'
064500971023     C     W001A         ANDNE     *BLANK
064600971021     C                   SETON                                        32
064700971021     C                   MOVE      *ZERO         VABCAS
064800010604     C                   eval      vinmsg = %trimr(vinmsg)
064900010604     C                             + ' ' + 'VHBCAS'
065000971021     C                   ELSE
065100971021     C                   MOVE      WD15          VABCAS
065200971021     C                   ENDIF
065300040827     C                   ENDIF
065400971021     C*
065500971023     C                   TESTN                   VHBRMN               30
065600971023     C  N30              TESTN                   VHBRMN                 30
065700971023     C  N30              TESTN                   VHBRMN                   30
065800971021     C                   MOVE      VHBRMN        W001A             1
065900971021     C     *IN30         IFEQ      *OFF
066000971021     C     W001A         ORLT      '0'
066100971021     C                   SETON                                        32
066200971021     C                   MOVE      *ZERO         VABRMN
066300010604     C                   eval      vinmsg = %trimr(vinmsg)
066400010604     C                             + ' ' + 'VHBRMN'
066500971021     C                   ELSE
066600971021     C                   MOVE      VHBRMN        VABRMN
066700971021     C                   ENDIF
066800971021     C*
066900971023     C                   TESTN                   VHBNCD               30
067000971023     C  N30              TESTN                   VHBNCD                 30
067100971023     C  N30              TESTN                   VHBNCD                   30
067200971021     C                   MOVE      VHBNCD        W001A             1
067300971021     C     *IN30         IFEQ      *OFF
067400971021     C     W001A         ORLT      '0'
067500971023     C     W001A         ANDNE     *BLANK
067600971021     C                   SETON                                        32
067700971021     C                   MOVE      *ZERO         VABNCD
067800010604     C                   eval      vinmsg = %trimr(vinmsg)
067900010604     C                             + ' ' + 'VHBNCD'
068000971021     C                   ELSE
068100971021     C                   MOVE      VHBNCD        VABNCD
068200971021     C                   ENDIF
068300971021     C*
068400971023     C                   TESTN                   VHBNCA               30
068500971023     C  N30              TESTN                   VHBNCA                 30
068600971023     C  N30              TESTN                   VHBNCA                   30
068700971021     C                   MOVE      VHBNCA        W001A             1
068800971021     C     *IN30         IFEQ      *OFF
068900971021     C     W001A         ORLT      '0'
069000971023     C     W001A         ANDNE     *BLANK
069100971021     C                   SETON                                        32
069200971021     C                   MOVE      *ZERO         VABNCA
069300010604     C                   eval      vinmsg = %trimr(vinmsg)
069400010604     C                             + ' ' + 'VHBNCA'
069500971021     C                   ELSE
069600971021     C                   MOVE      VHBNCA        VABNCA
069700971021     C                   ENDIF
069800971021     C*
069900971023     C                   TESTN                   VHBZNC               30
070000971023     C  N30              TESTN                   VHBZNC                 30
070100971023     C  N30              TESTN                   VHBZNC                   30
070200971021     C                   MOVE      VHBZNC        W001A             1
070300971021     C     *IN30         IFEQ      *OFF
070400971021     C     W001A         ORLT      '0'
070500971023     C     W001A         ANDNE     *BLANK
070600971021     C                   SETON                                        32
070700971021     C                   MOVE      *ZERO         VABZNC
070800010604     C                   eval      vinmsg = %trimr(vinmsg)
070900010604     C                             + ' ' + 'VHBZNC'
071000971021     C                   ELSE
071100971021     C                   MOVE      VHBZNC        VABZNC
071200971021     C                   ENDIF
071300971021     C*
071400971023     C                   TESTN                   VHBDCR               30
071500971023     C  N30              TESTN                   VHBDCR                 30
071600971023     C  N30              TESTN                   VHBDCR                   30
071700971021     C                   MOVE      VHBDCR        W001A             1
071800971021     C     *IN30         IFEQ      *OFF
071900971021     C     W001A         ORLT      '0'
072000971023     C     W001A         ANDNE     *BLANK
072100971021     C                   SETON                                        32
072200971021     C                   MOVE      *ZERO         VABDCR
072300010604     C                   eval      vinmsg = %trimr(vinmsg)
072400010604     C                             + ' ' + 'VHBDCR'
072500971021     C                   ELSE
072600971021     C                   MOVE      VHBDCR        VABDCR
072700971021     C                   ENDIF
072800971021     C*
072900971023     C                   TESTN                   VHBHCR               30
073000971023     C  N30              TESTN                   VHBHCR                 30
073100971023     C  N30              TESTN                   VHBHCR                   30
073200971021     C                   MOVE      VHBHCR        W001A             1
073300971021     C     *IN30         IFEQ      *OFF
073400971021     C     W001A         ORLT      '0'
073500971023     C     W001A         ANDNE     *BLANK
073600971021     C                   SETON                                        32
073700971021     C                   MOVE      *ZERO         VABHCR
073800010604     C                   eval      vinmsg = %trimr(vinmsg)
073900010604     C                             + ' ' + 'VHBHCR'
074000971021     C                   ELSE
074100971021     C                   MOVE      VHBHCR        VABHCR
074200971021     C                   ENDIF
074300941229     C*
074400971021     C                   MOVE      *ZERO         VABVMD
074500971021     C                   MOVE      VHBVMD        WASC15
074600010719     C                   IF        WI15 = *blanks
074700010719     C                   EVAL      WI15 = *zeros
074800010719     C                   ENDIF
074900971023     C                   TESTN                   WI15                 30
075000971023     C  N30              TESTN                   WI15                   30
075100971023     C  N30              TESTN                   WI15                     30
075200971021     C                   MOVE      WI15          W001A             1
075300971021     C     *IN30         IFEQ      *OFF
075400971021     C     W001A         ORLT      '0'
075500971023     C     W001A         ANDNE     *BLANK
075600971021     C                   SETON                                        32
075700971021     C                   MOVE      *ZERO         VABVMD
075800010604     C                   eval      vinmsg = %trimr(vinmsg)
075900010604     C                             + ' ' + 'VHBVMD'
076000971021     C                   ELSE
076100971021     C                   MOVEL     WI15          VABVMD
076200010719     C                   IF        WD15 = *blanks
076300010719     C                   EVAL      WD15 = *zeros
076400010719     C                   ENDIF
076500971023     C                   TESTN                   WD15                 30
076600971023     C  N30              TESTN                   WD15                   30
076700971023     C  N30              TESTN                   WD15                     30
076800971021     C                   MOVE      WD15          W001A             1
076900971021     C     *IN30         IFEQ      *OFF
077000971021     C     W001A         ORLT      '0'
077100971023     C     W001A         ANDNE     *BLANK
077200971021     C                   SETON                                        32
077300971021     C                   MOVE      *ZERO         VABVMD
077400010604     C                   eval      vinmsg = %trimr(vinmsg)
077500010604     C                             + ' ' + 'VHBVMD'
077600971021     C                   ELSE
077700971021     C                   MOVE      WD15          VABVMD
077800971021     C                   ENDIF
077900050118     C                   ENDIF
078000971021     C*
078100971023     C                   TESTN                   VHBANT               30
078200971023     C  N30              TESTN                   VHBANT                 30
078300971023     C  N30              TESTN                   VHBANT                   30
078400971021     C                   MOVE      VHBANT        W001A             1
078500971021     C     *IN30         IFEQ      *OFF
078600971021     C     W001A         ORLT      '0'
078700971023     C     W001A         ANDNE     *BLANK
078800971021     C                   SETON                                        32
078900971021     C                   MOVE      *ZERO         VABANT
079000010604     C                   eval      vinmsg = %trimr(vinmsg)
079100010604     C                             + ' ' + 'VHBANT'
079200971021     C                   ELSE
079300971021     C                   MOVE      VHBANT        VABANT
079400971021     C                   ENDIF
079500971021     C*
079600971021     C* CAMPI ALFABETICI:
079700971021     C                   MOVE      VHBATB        VABATB
079800971021     C                   MOVE      VHBCBO        VABCBO
079900971021     C                   MOVE      VHBRSD        VABRSD
080000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
080100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
080200020117     C* ==
080300971021     C                   MOVE      VHBRD2        VABRD2
080400971021     C                   MOVE      VHBIND        VABIND
080500971021     C                   MOVE      VHBCAD        VABCAD
080600971021     C                   MOVE      VHBLOD        VABLOD
080700971021     C                   MOVE      VHBPRD        VABPRD
080800971021     C                   MOVE      VHBNZD        VABNZD
080900971021     C                   MOVE      VHBGC1        VABGC1
081000971021     C                   MOVE      VHBGC2        VABGC2
081100971021     C                   MOVE      VHBTSP        VABTSP
081200971021     C                   MOVE      VHBVAS        VABVAS
081300971021     C                   MOVE      VHBNAS        VABNAS
081400971021     C                   MOVE      VHBTIC        VABTIC
081500971021     C                   MOVE      VHBVCA        VABVCA
081600971021     C                   MOVE      VHBGCA        VABGCA
081700971021     C                   MOVE      VHBRMA        VABRMA
081800971021     C                   MOVE      VHBXCO        VABXCO
081900971021     C                   MOVE      VHBNOT        VABNOT
082000971021     C                   MOVE      VHBNT2        VABNT2
082100971021     C                   MOVE      VHBCTM        VABCTM
082200971021     C                   MOVE      VHBFFD        VABFFD
082300971021     C                   MOVE      VHBTCR        VABTCR
082400971021     C                   MOVE      VHBCTS        VABCTS
082500971021     C                   MOVE      VHBFTM        VABFTM
082600971021     C                   MOVE      VHBVAD        VABVAD
082700971021     C                   MOVE      VHBGMA        VABGMA
082800971021     C                   MOVE      VHBGGA        VABGGA
082900971021     C                   MOVE      VHBGVA        VABGVA
083000971021     C                   MOVE      VHBTC1        VABTC1
083100971021     C                   MOVE      VHBTC2        VABTC2
083200971021     C                   MOVE      VHBSCL        VABSCL
083300971021     C                   MOVE      VHBRMO        VABRMO
083400971021     C                   MOVE      VHBCMO        VABCMO
083500971021     C                   MOVE      VHBNMO        VABNMO
083600971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
083700971021     C* NELLA NAZIONE ORIGINALE
083800971021     C                   MOVE      VHBNMO        WA1D              1
083900971021     C                   MOVEL     VHBNMO        WA1S              1
084000971021     C     WA1D          IFNE      *BLANK
084100971021     C     WA1S          ANDEQ     *BLANK
084200971021     C                   MOVE      *BLANK        VABNMO
084300971021     C                   END
084400050209     C*
084500050303     C* NSP => Se nn valorizzata numero spedizione => stacco numeratore da AZNUM
084600050303     C                   if        VABNSP = *zeros
084700050209     C                   clear                   TRUL33DS
084800050209     C                   eval      I33OPE = *zeros
084900050209     C                   eval      I33CNU = 302
085000050209     C                   eval      I33NUM = 1
085100050209     C                   movel     TRUL33DS      KPJBU
085200050209     C                   call      'TRUL33R'
085300050209     C                   parm                    KPJBA
085400050209     C                   movel     KPJBU         TRUL33DS
085500050209     C                   if        O33ERR = *zeros
085600050209     C                   z-add     O33NRF        VABNSP
085700050209     C                   z-add     O33NRF        VATNSP
085800050209     C                   else
085900050209     C                   SETON                                        31
086000050209     C                   EVAL      vinmsg = %trimr(vinmsg)
086100050209     C                             + ' ' + 'VABNSP VATNSP'
086200050209     C                   endif
086300050303     C                   endif
086400050209     C*
086500050209     C* Forzo il codice trattamento merce a 7Q
086600050209     C                   EVAL      VABCTM = '7Q'
086700930409     C*
086800010604     C  N31              ADD       1             §CTROK            7 0
086900010604     C   32              ADD       1             §CTRMO            7 0
087000010604     C   31              ADD       1             §CTRNO            7 0
087100020913     C  N31              WRITE     FIVAB000
087200050209     C  N31              EXSR      WRIVAT
087300990910     C*
087400010601     C                   if        *in31 = *off and
087500010601     C                             *in32 = *off
087600990910     C                   eval      vinflg = '1'
087700990910     C                   else
087800000621     C                   eval      recko = vindta
087900050303     C                   if        %open(titvi5ps)
088000030407     C   50              except    rigadett
088100010612     C                   endif
088200990910     C                   eval      vinflg = '2'
088300990910     C                   endif
088400991022     C                   endif
088500990910     C                   endif
088600010601     C*
088700010601     C                   endif
088800000905     C*
088900000905     C                   else
089000000905     C                   eval      vinflg = '1'
089100000905     C                   endif
089200000905     C*
089300000905     C  N70              update    tivin000
089400991022     C*
089500991022     C  N70              ENDdo
089600010530     C
089700010530     C*
089800010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
089900010530     C                   if        cntNonEl = *zeros or
090000010530     C                             flgMulti = '0'
090100990910     C* Se non ci sono record con errori ...
090200010530     C                   if        §ctrno = 0 and
090300010601     C                             §ctrmo = 0 and
090400010530     C                             flgStato <> '2'
090500990910     C* ... restituisco esito OK.
090600990921     C                   eval      wrkesito = '0'
090700990910     C                   else
090800010601     C                   if        §ctrok > 0
090900990921     C                   eval      wrkesito = '1'
091000000710     C                   else
091100010615     C                   if        flgOk = '0'
091200010615     C                   eval      wrkesito = '2'
091300010615     C                   else
091400010615     C                   eval      wrkesito = '6'
091500010615     C                   endif
091600990910     C                   endif
091700000710     C                   endif
091800010530     C                   else
091900010530     C                   eval      wrkesito = '9'
092000010530     C                   endif
092100050209     C*
092200050209     C                   else
092300050209     C                   eval      wrkesito = '2'
092400050209     C                   endif
092500990910     C*
092600990914     C                   if        %open(tivin00r)
092700990908     C                   close     tivin00r
092800990914     C                   endif
092900020913     C                   if        %open(fivabwwr)
093000020913     C                   close     fivabwwr
093100990914     C                   endif
093200050209     C                   if        %open(fivatwwr)
093300050209     C                   close     fivatwwr
093400050209     C                   endif
093500010530     C*
093600010530     C                   if        vlrpoi <> 999
093700010530     C                   eval      invfil = vlrpoi
093800010530     C                   endif
093900990910     C*
094000990920     C                   if        §ctrok > 0
094100010530     C                             and invfil > *zeros
094200000613     C                   exsr      invio
094300990920     C                   endif
094400010612     C*
094500010612     C                   if        flgGiro = '1'
094600010612     C                   exsr      endela
094700010612     C                   endif
094800990920     C*
094900910830     C                   ENDSR
095000000613     C***
095100050209
095200050209
095300050209
095400050209     C*----------------------------------------------------*
095500050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
095600050209     C*----------------------------------------------------*
095700050209     C     PREVAT        BEGSR
095800050209     C*
095900050209     C* Compongo il nome del membro da dare al FIVATWWR
096000050209     C                   eval      parmbr = vlrhdl
096100050209     C                   movel     'M'           parmbr
096200050209     C                   eval      parccm = %subst(vlrKSC:2:7)
096300050209     C                   eval      paropz = '1'
096400050209     C* Effettuo la chiamata al CLLE preposto
096500050209     C                   call(e)   'TITVVTC'
096600050209     C                   parm                    parccm
096700050209     C                   parm                    parmbr
096800050209     C                   parm                    paropz
096900050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
097000050209     C                   if        %error
097100050209     C                   movel     '1'           chkcall
097200050209     C                   else
097300050209     C                   movel     '0'           chkcall
097400050209     C                   endif
097500050209     C*
097600050209     C                   ENDSR
097700050209
097800050209
097900990920
098000050209     C*----------------------------------------------------*
098100050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
098200050209     C*----------------------------------------------------*
098300050209     C     WRIVAT        BEGSR
098400050209     C*
098500050209     C* Valorizzo il buffer del file dettagli barcode colli
098600050209     C                   EVAL      VATFGS = VABFGS
098700050209     C                   EVAL      VATAAS = VABAAS
098800050209     C                   EVAL      VATLNP = VABLNP
098900050209     C                   EVAL      VATNSP = VABNSP
099000050209     C                   EVAL      VATCCM = VABCCM
099100050209     C                   EVAL      VATTRC='E'
099200050303     C*
099300050303     C* Compongo tanti "chi sono" quanti sono i colli della spedizione
099400050303     C     1             DO        VABNCL        wColloNum         3 0
099500050303     C                   MOVE(P)   wColloNum     wColloAlf         3
099600050303     C                   EVAL      VATNOT = %trim(%editc(VABRMN:'Z'))+wColloAlf
099700050303     C                   IF        VATNOT <> *BLANKS
099800050303     C                   WRITE     FIVAT000
099900050303     C                   ENDIF
100000050303     C                   ENDDO
100100050209     C*
100200050209     C                   ENDSR
100300010530
100400010530
100500050209
100600010530
100700010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
100800010530     C     repfil        BEGSR
100900010530     C*
101000010530     C                   if        invfil = *zeros and
101100010530     C                             depfil > *zeros and
101200010613     C                             (vinflg = *blanks or
101300010613     C                              vinflg = *zeros)
101400010530     C
101500010530     C                   eval      invfil = depfil
101600010530     C                   endif
101700010530     C*
101800010530     C                   if        depfil <> invfil and
101900010530     C                             invfil > *zeros
102000010530     C                   eval      flgMulti = '1'
102100010530     C                   if        vinflg = *blanks
102200010530     C                   add       1             cntNonEl
102300010530     C                   endif
102400010530     C                   endif
102500010530     C*
102600010530     C                   if        vinflg = '2'
102700010530     C                   eval      flgStato = '2'
102800010530     C                   endif
102900010530     C*
103000010530     C                   ENDSR
103100010530     C***
103200050209
103300050209
103400050209
103500050209
103600050209      /TITLE Invio dei dati al punto operativo.
103700050209     C     invio         BEGSR
103800050209     C*
103900050209     C* 1° invio FIVAT
104000050209     C                   reset                   dscmz
104100050209     C                   move      vlrpoi        cmzdst
104200050209     C                   eval      cmzfld = 'FIVATWWR'
104300050209     C                   eval      cmzmbd = vlrhdl
104400050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
104500050209     C***                if        prmfir = *blanks
104600050209     C                   eval      cmzfla = 'FIVAT00F'
104700050209     C                   eval      cmzmba = 'FIVAT00F'
104800050209     C***                else
104900050209     C***                eval      cmzfla = prmfir
105000050209     C***                eval      cmzmba = prmfir
105100050209     C***                endif
105200050209     C                   eval      cmznrr = *zeros
105300050209     C                   move      §ctrok        cmznrr
105400050209     C                   eval      cmzlba = vlrfl1
105500050209     C                   call(e)   'TIS711C'
105600050209     C                   parm                    dscmz
105700050209     C                   parm      *blanks       esito
105800050209     C                   if        %error
105900050209     C                             or cmzerr = '1'
106000050209     C                             or esito  = '1'
106100050209     C                   eval      wrkesito = '3'
106200050209     C                   else
106300050209     C*
106400050209     C* 2° invio FIVAB
106500050209     C                   reset                   dscmz
106600050209     C                   move      vlrpoi        cmzdst
106700050209     C                   eval      cmzfld = vlrfou
106800050209     C                   eval      cmzmbd = vlrhdl
106900050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
107000050209     C***                if        prmfir = *blanks
107100050209     C                   eval      cmzfla = 'FIVAB00F'
107200050209     C                   eval      cmzmba = 'FIVAB00F'
107300050209     C***                else
107400050209     C***                eval      cmzfla = prmfir
107500050209     C***                eval      cmzmba = prmfir
107600050209     C***                endif
107700050209     C                   eval      cmznrr = *zeros
107800050209     C                   move      §ctrok        cmznrr
107900050209     C                   eval      cmzlba = vlrfl1
108000050209     C                   call(e)   'TIS711C'
108100050209     C                   parm                    dscmz
108200050209     C                   parm      *blanks       esito
108300050209     C                   if        %error
108400050209     C                             or cmzerr = '1'
108500050209     C                             or esito  = '1'
108600050209     C                   eval      wrkesito = '3'
108700050209     C                   endif
108800050209     C                   endif
108900050209     C*
109000050209     C                   ENDSR
109100050209     C***
109200010530
109300010530
109400010530
109500010530
109600010530      /TITLE Invio dei dati al punto operativo.
109700010530     C     opeini        BEGSR
109800010530     C*
109900010530     C* Inizializzo flag e contatori operativi
110000010530     C                   movel     '0'           flgGiro           1
110100010530     C                   movel     '0'           flgMulti          1
110200010530     C                   movel     '1'           flgStato          1
110300010615     C                   movel     '0'           flgOk             1
110400010530     C                   z-add     *zeros        cntNonEl         10 0
110500010530     C                   z-add     *zeros        depfil            3 0
110600010530     C                   z-add     *zeros        invfil            3 0
110700030407     C*
110800050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
110900030407     C                   if        %subst(vlrppt:1:1) = 'S'
111000030407     C                   seton                                        50
111100030407     C                   else
111200030407     C                   setoff                                       50
111300030407     C                   endif
111400041001     C*
111500050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
111600041001     C                   if        %subst(vlrppt:2:1) = 'S'
111700041001     C                   seton                                        51
111800041001     C                   else
111900041001     C                   setoff                                       51
112000041001     C                   endif
112100010530     C*
112200010530     C                   ENDSR
112300010530     C***
112400010530
112500010530
112600010530
112700990910
112800000613     C     *inzsr        BEGSR
112900990910     C*
113000990910     C     *entry        plist
113100990920     C                   parm                    tivlrds
113200990921     C                   parm      wrkesito      esito
113300000724     C                   parm                    prmlit
113400000710     C                   parm                    prmfir
113500010606     C*
113600010606     C* CALCOLA LA DATA CORRENTE
113700010606     C                   time                    wn14             14 0
113800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
113900010606     C                   z-add     wn8           g08dat
114000010606     C                   z-add     *zeros        g08inv
114100010606     C                   movel     '0'           g08err
114200010606     C                   call      'XSRDA8'
114300010606     C                   parm                    wlbda8
114400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
114500000613     C*
114600000613     C                   ENDSR
114700000613     C***
114800990908
114900050303     Otitvi5p   E            riepilogo         2
115000990915     O                                              'Upload via Internet'
115100990915     O                                           +1 'Traduzione TIVIN00R -'
115200020913     O                                           +1 'FIVABWWR'
115300990915     O                                           +1 'Testate bolle'
115400990915     O          E            riepilogo   2
115500990915     O                       wrkdata
115600990915     O                       wrkora              +1
115700990915     O                       procname            +1
115800990915     O          E            riepilogo   2
115900990915     O                                              'Cliente..................:'
116000990915     O                       VABCCM        z     +1
116100990915     O          E            riepilogo   2
116200990920     O                                              'Riferimento Strategi.....:'
116300990920     O                       vlrhdl              +1
116400990915     O          E            riepilogo   2
116500990915     O                                              'Giusti...................:'
116600971022     O                       §CTROK        2   +  1
116700990915     O          E            riepilogo   2
116800990915     O                                              'Sbagliati e corretti.....:'
116900971022     O                       §CTRMO        2   +  1
117000990915     O          E            riepilogo   2
117100990915     O                                              'Sbagliati e scartati.....:'
117200971022     O                       §CTRNO        2   +  1
117300000613
117400050303     Otitvi5ps  E            testdett          2
117500000613     O                                              'Upload via Internet'
117600000613     O                                           +1 'Traduzione TIVIN00R -'
117700020913     O                                           +1 'FIVABWWR'
117800000613     O                                           +1 'Testate bolle'
117900000616     O          E            testdett    3
118000000613     O                                           +2 'N° rec'
118100000613     O                                           +3 'Anteprima contenuto'
118200000616     O          E            rigadett    2
118300000613     O                       rrnum               +2
118400000621     O                       recko               +3
118500000616     O          E            findett     2
118600000613     O                       wrkdata
118700000613     O                       wrkora              +1
118800000613     O                       procname            +1
118900000616     O          E            findett     2
119000000613     O                                              'Cliente..................:'
119100000613     O                       VABCCM        z     +1
119200000616     O          E            findett     2
119300000613     O                                              'Riferimento Strategi.....:'
119400000613     O                       vlrhdl              +1
119500000616     O          E            findett     2
119600000613     O                                              'Giusti...................:'
119700000613     O                       §CTROK        2   +  1
119800000616     O          E            findett     2
119900000613     O                                              'Sbagliati e corretti.....:'
120000000613     O                       §CTRMO        2   +  1
120100000616     O          E            findett     2
120200000613     O                                              'Sbagliati e scartati.....:'
120300000613     O                       §CTRNO        2   +  1
120400000512** CMD - COMANDI CL
120500050303OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
120600050303OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
120700050303DLTOVR FILE(TITVI5P TITVI5PS) LVL(*)
120800000512
120900000512
