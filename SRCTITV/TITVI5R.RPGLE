000100050303      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente RS Components - Cod. 0051406)
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020913     FFIVABwwr  O    E             DISK    usropn
000700050209     FFIVATwwr  O    E             DISK    usropn
000800050303     Ftitvi5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050303     Ftitvi5ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000512     D*------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300060308     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002400060308     D* 10+3
002500060308     D                 DS
002600060308     D  WI15                   1     10
002700060308     D  WD15                  11     13
002800060308     D  WASC15                 1     13
002900060308     D*  6+1
003000060308     D                 DS
003100060308     D  WI09                   1      6
003200060308     D  WD09                   7      7
003300060308     D  WASC09                 1      7
003400060308     D*  2+3
003500060308     D                 DS
003600060308     D  WI07                   1      2
003700060308     D  WD07                   3      5
003800060308     D  WASC07                 1      5
003900990920     D tivlrds       e ds                  extname(tivlr00f)
004000990910     D esito           s              1
004100000724     D prmlit          s             10
004200000710     D prmfir          s             10
004300990921     D wrkesito        s                   like(esito)
004400990915     D wrkdata         s               d
004500990915     D wrkora          s               t
004600000613     D rrnum           s              6  0 INZ(*zeros)
004700000621     D recko           s            150    INZ(*blanks)
004800011123     D depcmd          s            150    INZ(*blanks)
004900050209     D parccm          s              8    INZ(*blanks)
005000050209     D parmbr          s             10    INZ(*blanks)
005100050209     D paropz          s              1    INZ(*blanks)
005200050209     D chkcall         s              1    INZ(*blanks)
005300010606     D*------------------
005400010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500010606     D*------------------
005600010606     D WLBDA8          DS                  INZ
005700010606     D  G08DAT                 1      8  0
005800010606     D  G08INV                 9     16  0
005900010606     D  G08ERR                17     17
006000010606     D  G08TGI                18     22  0
006100050209     D*------------------
006200050209     D* DS REPERIMENTO NUMERATORE
006300050209     D*------------------
006400050303     D trul33ds      e ds                  inz
006500050209     D*------------------
006600050209     D* DS ARCHITETTURA
006700050209     D*------------------
006800050209     D kpjba         e ds                  inz
006900050209     D*------------------
007000060308     D*---------------------------------------------------------
007100060308     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
007200060308     D*---------------------------------------------------------
007300060308     D                 ds
007400060308     D vhbatb                  1      1
007500060308     D vhbccm                  2      8
007600060308     D vhblnp                  9     11
007700060308     D vhbaas                 12     15
007800060308     D vhbmgs                 16     19
007900060308     D vhbnrs                 20     21
008000060308     D vhbnsp                 22     28
008100060308     D vhbcbo                 29     30
008200060308     D vhblna                 31     33
008300060308     D vhbrsd                 34     68
008400060308     D vhbrd2                 69    103
008500060308     D vhbind                104    138
008600060308     D vhbcad                139    147
008700060308     D vhblod                148    182
008800060308     D vhbprd                183    184
008900060308     D vhbnzd                185    187
009000060308     D vhbgc1                188    189
009100060308     D vhbgc2                190    191
009200060308     D vhbctr                192    194
009300060308     D vhbtsp                195    195
009400060308     D vhbias                196    208
009500060308     D vhbvas                209    211
009600060308     D vhbnas                212    226
009700060308     D vhbncl                227    231
009800060308     D vhbpkb                232    238
009900060308     D vhbvlb                239    243
010000060308     D vhbqft                244    256
010100060308     D vhbcas                257    269
010200060308     D vhbtic                270    271
010300060308     D vhbvca                272    274
010400060308     D vhbgca                275    276
010500060308     D vhbrmn                277    291
010600060308     D vhbrma                292    306
010700060308     D vhbncd                307    313
010800060308     D vhbnca                314    320
010900060308     D vhbxco                321    321
011000060308     D vhbnot                322    356
011100060308     D vhbnt2                357    391
011200060308     D vhbznc                392    393
011300060308     D vhbctm                394    395
011400060308     D vhbffd                396    396
011500060308     D vhbdcr                397    404
011600060308     D vhbtcr                405    405
011700060308     D vhbhcr                406    409
011800060308     D vhbcts                410    411
011900060308     D vhbftm                412    412
012000060308     D vhbvmd                413    425
012100060308     D vhbvad                426    428
012200060308     D vhbgma                429    430
012300060308     D vhbgga                431    432
012400060308     D vhbgva                433    434
012500060308     D vhbtc1                435    435
012600060308     D vhbtc2                436    436
012700060308     D vhbscl                437    437
012800060308     D vhbant                438    446
012900060308     D vhbrmo                447    471
013000060308     D vhbcmo                472    480
013100060308     D vhbnmo                481    483
013200060308     D  X_rec                  1    483    inz
013300990908
013400050209
013500050209
013600050209     C* /MAIN
013700990915     C                   time                    wrkdata
013800990915     C                   time                    wrkora
013900000913     C                   reset                   rrnum
014000990921     C                   reset                   esito
014100990921     C                   reset                   wrkesito
014200010530     C*
014300010530     C                   exsr      opeini
014400010530     C                   exsr      rwvab
014500050209     C*
014600050209     C* Effettuo la chiamata al CLLE preposto
014700050209     C                   call(e)   'TITVVTC'
014800050209     C                   parm                    parccm
014900050209     C                   parm                    parmbr
015000050209     C                   parm      '2'           paropz
015100010530     C*
015200010530     C                   seton                                        lr
015300010530
015400010530
015500010530
015600010530
015700010530     C*--------------------------------------------------------
015800010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
015900010530     C*--------------------------------------------------------
016000010530     C     PREELA        BEGSR
016100000724     C*
016200000724     C* SE OCCORRE SPEDIRE IN FILIALE
016300010530     C                   if        invfil <> *zeros and
016400010530     C                             flgGiro = '0'
016500010530     C*
016600010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
016700010530     C                   eval      flgGiro = '1'
016800000724     C*
016900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
017000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
017100010530     C     invfil        chain     azorg01l
017200000724     C                   if        %found
017300000616     C                   movel(p)  CMD(1)        depcmd
017400020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
017500000616     C*
017600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
017700011123     C                   Z-ADD     150           LENGH            15 5
017800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017900000616     C                   PARM                    depcmd
018000000616     C                   PARM                    LENGH
018100000724     C*
018200000724     C                   endif
018300000616     C*
018400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
018500000616     C                   movel(p)  CMD(2)        depcmd
018600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
018700000616     C*
018800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
018900011123     C                   Z-ADD     150           LENGH            15 5
019000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019100000616     C                   PARM                    depcmd
019200000616     C                   PARM                    LENGH
019300000616     C*
019400050303     C                   if        not %open(titvi5ps)
019500050303     C                   open      titvi5ps
019600030407     C   50              except    testdett
019700000613     C                   endif
019800010530     C*
019900010530     C                   endif
020000010530     C*
020100010530     C                   ENDSR
020200010530     C***
020300010530
020400010530
020500010530
020600010530
020700010530
020800010530
020900010530     C*--------------------------------------------------------
021000010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
021100010530     C*--------------------------------------------------------
021200010530     C     ENDELA        BEGSR
021300010530     C*
021400010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
021500000613     C*
021600050303     C                   if        %open(titvi5ps)
021700030407     C   50              except    findett
021800050303     C                   close     titvi5ps
021900000613     C                   endif
022000000616     C*
022100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
022200011123     C                   Z-ADD     150           LENGH            15 5
022300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
022400000616     C                   PARM                    CMD(3)
022500000616     C                   PARM                    LENGH
022600010530     C*
022700010530     C                   ENDSR
022800010530     C***
022900000613
023000010530
023100010530
023200000613
023300000613     C*--------------------------------------------------------
023400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
023500000613     C*--------------------------------------------------------
023600000621     C     STPR          BEGSR
023700000613     C*
023800050303     C                   if        not %open(titvi5p)
023900050303     C                   open      titvi5p
024000990915     C                   endif
024100990915     C*
024200030407     C   50              except    riepilogo
024300990915     C*
024400050303     C                   if        %open(titvi5p)
024500050303     C                   close     titvi5p
024600990914     C                   endif
024700990910     C*
024800000613     C                   ENDSR
024900000613     C***
025000990908
025100010530
025200010530
025300010530
025400910830     C*--------------------------------------------------------
025500020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
025600910830     C*--------------------------------------------------------
025700941229     C     RWVAB         BEGSR
025800990910     C*
025900990914     C                   if        not %open(tivin00r)
026000990908     C                   open      tivin00r
026100990914     C                   endif
026200020913     C                   if        not %open(fivabwwr)
026300020913     C                   open      fivabwwr
026400990914     C                   endif
026500050209     C*
026600050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
026700050209     C                   exsr      prevat
026800050209     C*
026900050209     C                   if        chkcall = '0'
027000050209     C*
027100050209     C                   if        not %open(fivatwwr)
027200050209     C                   open      fivatwwr
027300050209     C                   endif
027400050209     C*
027500990910     C                   clear                   §CTROK
027600990910     C                   clear                   §CTRMO
027700990910     C                   clear                   §CTRNO
027800990910     C*
027900921023     C                   DO        *HIVAL
028000990913     C*
028100990915     C                   READ      tivin00r                               70
028200010618     C*
028300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
028400010618     C                   if        vinflg = '1'
028500010618     C                   eval      flgOk = '1'
028600010618     C                   endif
028700010705     C*
028800010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
028900010705     C                   if        %subst(vindta:1:1) = '='
029000010705     C                   eval      %subst(vindta:1:1) = *blanks
029100010705     C                   endif
029200010705     C*
029300000905     C                   if        vindta > *blanks
029400000613     C                   add       1             rrnum
029500010601     C* ...elimino eventuali segni
029600010601     C     '+':' '       xlate     vindta        vindta
029700990913     C*
029800010601     C                   if        *in70 = *off and
029900990913     C                             (vinflg = *blanks
030000010711     C                              or vinflg = '0'
030100010711     C                              or vinflg = '2')
030200990913     C*
030300020913     C                   clear                   FIVAB000
030400050209     C                   clear                   FIVAT000
030500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
030600010711     C                   if        vinflg = *blanks or vinflg = '0'
030700010711     C                   clear                   vinmsg
030800010711     C                   endif
030900010711     C*
031000060308     C                   eval      x_rec  = vindta
031100991022     C*
031200991022     C                   testb     x'3F'         vhbatb                   02
031300991022     C                   if        not *in02
031400991022     C*
031500971021     C                   SETOFF                                       303132
031600971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
031700971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
031800971023     C*    OPPURE TUTTI BLANKS
031900010530     C*
032000010530     C                   TESTN                   VHBLNP               30
032100010530     C  N30              TESTN                   VHBLNP                 30
032200010530     C  N30              TESTN                   VHBLNP                   30
032300010530     C                   MOVE      VHBLNP        W001A             1
032400010530     C     *IN30         IFEQ      *OFF
032500010530     C     W001A         ORLT      '0'
032600010530     C                   SETON                                        31
032700010530     C                   eval      vinmsg = %trimr(vinmsg)
032800010530     C                             + ' ' + 'VHBLNP'
032900010530     C                   ELSE
033000010530     C                   MOVE      VHBLNP        VABLNP
033100010530     C     VABLNP        IFLE      *ZEROS
033200010530     C                   SETON                                        31
033300010530     C                   eval      vinmsg = %trimr(vinmsg)
033400010530     C                             + ' ' + 'VHBLNP'
033500010530     C                   ENDIF
033600010530     C                   ENDIF
033700010530     C*
033800010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
033900010530     C                   eval      depfil = VABLNP
034000010530     C                   exsr      repfil
034100010601     C                   if        depfil = invfil
034200021014     C                   if        vlrpoi = 999
034300021010     C                   MOVE(P)   invfil        VABFGS
034400021014     C                   else
034500021014     C                   MOVE(P)   vlrpoi        VABFGS
034600021014     C                   endif
034700010530     C*
034800010530     C                   exsr      PREELA
034900010530     C*
035000971023     C                   TESTN                   VHBCCM               30
035100971023     C  N30              TESTN                   VHBCCM                 30
035200971023     C  N30              TESTN                   VHBCCM                   30
035300971021     C                   MOVE      VHBCCM        W001A             1
035400971021     C     *IN30         IFEQ      *OFF
035500971021     C     W001A         ORLT      '0'
035600971021     C                   SETON                                        31
035700010604     C                   eval      vinmsg = %trimr(vinmsg)
035800010604     C                             + ' ' + 'VHBCCM'
035900971021     C                   ELSE
036000971021     C                   MOVE      VHBCCM        VABCCM
036100971024     C     VABCCM        IFLE      *ZEROS
036200971024     C                   SETON                                        31
036300010604     C                   eval      vinmsg = %trimr(vinmsg)
036400010604     C                             + ' ' + 'VHBCCM'
036500971024     C                   ENDIF
036600971021     C                   ENDIF
036700971021     C*
036800971023     C                   TESTN                   VHBAAS               30
036900971023     C  N30              TESTN                   VHBAAS                 30
037000971023     C  N30              TESTN                   VHBAAS                   30
037100971021     C                   MOVE      VHBAAS        W001A             1
037200971021     C     *IN30         IFEQ      *OFF
037300971021     C     W001A         ORLT      '0'
037400971023     C     W001A         ANDNE     *BLANK
037500971021     C                   SETON                                        32
037600010606     C                   MOVEL     datcor        VABAAS
037700010604     C                   eval      vinmsg = %trimr(vinmsg)
037800010604     C                             + ' ' + 'VHBAAS'
037900971021     C                   ELSE
038000971021     C                   MOVE      VHBAAS        VABAAS
038100971021     C                   ENDIF
038200971021     C*
038300971023     C                   TESTN                   VHBMGS               30
038400971023     C  N30              TESTN                   VHBMGS                 30
038500971023     C  N30              TESTN                   VHBMGS                   30
038600971021     C                   MOVE      VHBMGS        W001A             1
038700971021     C     *IN30         IFEQ      *OFF
038800971021     C     W001A         ORLT      '0'
038900971023     C     W001A         ANDNE     *BLANK
039000971021     C                   SETON                                        32
039100010601     C                   MOVE      '0101'        VABMGS
039200010604     C                   eval      vinmsg = %trimr(vinmsg)
039300010604     C                             + ' ' + 'VHBMGS'
039400971021     C                   ELSE
039500971021     C                   MOVE      VHBMGS        VABMGS
039600971021     C                   ENDIF
039700971021     C*
039800971023     C                   TESTN                   VHBNRS               30
039900971023     C  N30              TESTN                   VHBNRS                 30
040000971023     C  N30              TESTN                   VHBNRS                   30
040100971021     C                   MOVE      VHBNRS        W001A             1
040200971021     C     *IN30         IFEQ      *OFF
040300971021     C     W001A         ORLT      '0'
040400971023     C     W001A         ANDNE     *BLANK
040500971021     C                   SETON                                        32
040600971021     C                   MOVE      *ZERO         VABNRS
040700010604     C                   eval      vinmsg = %trimr(vinmsg)
040800010604     C                             + ' ' + 'VHBNRS'
040900971021     C                   ELSE
041000971021     C                   MOVE      VHBNRS        VABNRS
041100971021     C                   ENDIF
041200971021     C*
041300971023     C                   TESTN                   VHBNSP               30
041400971023     C  N30              TESTN                   VHBNSP                 30
041500971023     C  N30              TESTN                   VHBNSP                   30
041600971021     C                   MOVE      VHBNSP        W001A             1
041700971021     C     *IN30         IFEQ      *OFF
041800971021     C     W001A         ORLT      '0'
041900971023     C     W001A         ANDNE     *BLANK
042000971021     C                   SETON                                        32
042100971021     C                   MOVE      *ZERO         VABNSP
042200050303     C                   MOVE      *ZERO         VATNSP
042300010604     C                   eval      vinmsg = %trimr(vinmsg)
042400010604     C                             + ' ' + 'VHBNSP'
042500971021     C                   ELSE
042600971021     C                   MOVE      VHBNSP        VABNSP
042700050303     C                   MOVE      VHBNSP        VATNSP
042800971021     C                   ENDIF
042900971021     C*
043000971023     C                   TESTN                   VHBLNA               30
043100971023     C  N30              TESTN                   VHBLNA                 30
043200971023     C  N30              TESTN                   VHBLNA                   30
043300971021     C                   MOVE      VHBLNA        W001A             1
043400971021     C     *IN30         IFEQ      *OFF
043500971021     C     W001A         ORLT      '0'
043600971023     C     W001A         ANDNE     *BLANK
043700971021     C                   SETON                                        32
043800971021     C                   MOVE      *ZERO         VABLNA
043900010604     C                   eval      vinmsg = %trimr(vinmsg)
044000010604     C                             + ' ' + 'VHBLNA'
044100971021     C                   ELSE
044200971021     C                   MOVE      VHBLNA        VABLNA
044300971021     C                   ENDIF
044400971021     C*
044500971023     C                   TESTN                   VHBCTR               30
044600971023     C  N30              TESTN                   VHBCTR                 30
044700971023     C  N30              TESTN                   VHBCTR                   30
044800971021     C                   MOVE      VHBCTR        W001A             1
044900971021     C     *IN30         IFEQ      *OFF
045000971021     C     W001A         ORLT      '0'
045100971023     C     W001A         ANDNE     *BLANK
045200971021     C                   SETON                                        32
045300971021     C                   MOVE      *ZERO         VABCTR
045400010604     C                   eval      vinmsg = %trimr(vinmsg)
045500010604     C                             + ' ' + 'VHBCTR'
045600971021     C                   ELSE
045700971021     C                   MOVE      VHBCTR        VABCTR
045800971021     C                   ENDIF
045900941229     C*
046000971021     C                   MOVE      *ZERO         VABIAS
046100941229     C                   MOVE      VHBIAS        WASC15
046200971023     C                   TESTN                   WI15                 30
046300971023     C  N30              TESTN                   WI15                   30
046400971023     C  N30              TESTN                   WI15                     30
046500971021     C                   MOVE      WI15          W001A             1
046600971021     C     *IN30         IFEQ      *OFF
046700971021     C     W001A         ORLT      '0'
046800971023     C     W001A         ANDNE     *BLANK
046900971021     C                   SETON                                        32
047000971021     C                   MOVE      *ZERO         VABIAS
047100010604     C                   eval      vinmsg = %trimr(vinmsg)
047200010604     C                             + ' ' + 'VHBIAS'
047300971021     C                   ELSE
047400971021     C                   MOVEL     WI15          VABIAS
047500971023     C                   TESTN                   WD15                 30
047600971023     C  N30              TESTN                   WD15                   30
047700971023     C  N30              TESTN                   WD15                     30
047800971021     C                   MOVE      WD15          W001A             1
047900971021     C     *IN30         IFEQ      *OFF
048000971021     C     W001A         ORLT      '0'
048100971023     C     W001A         ANDNE     *BLANK
048200971021     C                   SETON                                        32
048300971021     C                   MOVE      *ZERO         VABIAS
048400010604     C                   eval      vinmsg = %trimr(vinmsg)
048500010604     C                             + ' ' + 'VHBIAS'
048600971021     C                   ELSE
048700971021     C                   MOVE      WD15          VABIAS
048800971021     C                   ENDIF
048900050118     C                   ENDIF
049000941229     C*
049100971023     C                   TESTN                   VHBNCL               30
049200971023     C  N30              TESTN                   VHBNCL                 30
049300971023     C  N30              TESTN                   VHBNCL                   30
049400971021     C                   MOVE      VHBNCL        W001A             1
049500971021     C     *IN30         IFEQ      *OFF
049600971021     C     W001A         ORLT      '0'
049700971021     C                   SETON                                        32
049800971021     C                   MOVE      *ZERO         VABNCL
049900010604     C                   eval      vinmsg = %trimr(vinmsg)
050000010604     C                             + ' ' + 'VHBNCL'
050100971021     C                   ELSE
050200971021     C                   MOVE      VHBNCL        VABNCL
050300971021     C                   ENDIF
050400941229     C*
050500971021     C                   MOVE      *ZERO         VABPKB
050600971021     C                   MOVE      VHBPKB        WASC09
050700010719     C                   IF        WI09 = *blanks
050800010719     C                   EVAL      WI09 = *zeros
050900010719     C                   ENDIF
051000971023     C                   TESTN                   WI09                 30
051100971023     C  N30              TESTN                   WI09                   30
051200971023     C  N30              TESTN                   WI09                     30
051300971021     C                   MOVE      WI09          W001A             1
051400971021     C     *IN30         IFEQ      *OFF
051500971021     C     W001A         ORLT      '0'
051600971021     C                   SETON                                        32
051700971021     C                   MOVE      *ZERO         VABPKB
051800010604     C                   eval      vinmsg = %trimr(vinmsg)
051900010604     C                             + ' ' + 'VHBPKB'
052000971021     C                   ELSE
052100971021     C                   MOVEL     WI09          VABPKB
052200010719     C                   IF        WD09 = *blanks
052300010719     C                   EVAL      WD09 = *zeros
052400010719     C                   ENDIF
052500971023     C                   TESTN                   WD09                 30
052600971023     C  N30              TESTN                   WD09                     30
052700971021     C                   MOVE      WD09          W001A             1
052800971021     C     *IN30         IFEQ      *OFF
052900971021     C     W001A         ORLT      '0'
053000971023     C     W001A         ANDNE     *BLANK
053100971021     C                   SETON                                        32
053200971021     C                   MOVE      *ZERO         VABPKB
053300010604     C                   eval      vinmsg = %trimr(vinmsg)
053400010604     C                             + ' ' + 'VHBPKB'
053500971021     C                   ELSE
053600971021     C                   MOVE      WD09          VABPKB
053700971021     C                   ENDIF
053800050118     C                   ENDIF
053900971021     C*
054000041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
054100041001     C                   IF        *IN51 = *OFF
054200971021     C                   MOVE      *ZERO         VABVLB
054300971021     C                   MOVE      VHBVLB        WASC07
054400010719     C                   IF        WI07 = *blanks
054500010719     C                   EVAL      WI07 = *zeros
054600010719     C                   ENDIF
054700971023     C                   TESTN                   WI07                 30
054800971023     C  N30              TESTN                   WI07                   30
054900971023     C  N30              TESTN                   WI07                     30
055000971021     C                   MOVE      WI07          W001A             1
055100971021     C     *IN30         IFEQ      *OFF
055200971021     C     W001A         ORLT      '0'
055300971023     C     W001A         ANDNE     *BLANK
055400971021     C                   SETON                                        32
055500971021     C                   MOVE      *ZERO         VABVLB
055600010604     C                   eval      vinmsg = %trimr(vinmsg)
055700010604     C                             + ' ' + 'VHBVLB'
055800971021     C                   ELSE
055900971021     C                   MOVEL     WI07          VABVLB
056000010719     C                   IF        WD07 = *blanks
056100010719     C                   EVAL      WD07 = *zeros
056200010719     C                   ENDIF
056300971023     C                   TESTN                   WD07                 30
056400971023     C  N30              TESTN                   WD07                   30
056500971023     C  N30              TESTN                   WD07                     30
056600971021     C                   MOVE      WD07          W001A             1
056700971021     C     *IN30         IFEQ      *OFF
056800971021     C     W001A         ORLT      '0'
056900971023     C     W001A         ANDNE     *BLANK
057000971021     C                   SETON                                        32
057100971021     C                   MOVE      *ZERO         VABVLB
057200010604     C                   eval      vinmsg = %trimr(vinmsg)
057300010604     C                             + ' ' + 'VHBVLB'
057400971021     C                   ELSE
057500971021     C                   MOVE      WD07          VABVLB
057600971021     C                   ENDIF
057700050118     C                   ENDIF
057800041001     C                   ELSE
057900041001     C* ...altrimenti forzo azzeramento del volume
058000041001     C                   Z-ADD     *zeros        VABVLB
058100041001     C                   ENDIF
058200941229     C*
058300971021     C                   MOVE      *ZERO         VABQFT
058400971021     C                   MOVE      VHBQFT        WASC15
058500010719     C                   IF        WI15 = *blanks
058600010719     C                   EVAL      WI15 = *zeros
058700010719     C                   ENDIF
058800971023     C                   TESTN                   WI15                 30
058900971023     C  N30              TESTN                   WI15                   30
059000971023     C  N30              TESTN                   WI15                     30
059100971021     C                   MOVE      WI15          W001A             1
059200971021     C     *IN30         IFEQ      *OFF
059300971021     C     W001A         ORLT      '0'
059400971023     C     W001A         ANDNE     *BLANK
059500971021     C                   SETON                                        32
059600971021     C                   MOVE      *ZERO         VABQFT
059700010604     C                   eval      vinmsg = %trimr(vinmsg)
059800010604     C                             + ' ' + 'VHBQFT'
059900971021     C                   ELSE
060000971021     C                   MOVEL     WI15          VABQFT
060100010719     C                   IF        WD15 = *blanks
060200010719     C                   EVAL      WD15 = *zeros
060300010719     C                   ENDIF
060400971023     C                   TESTN                   WD15                 30
060500971023     C  N30              TESTN                   WD15                   30
060600971023     C  N30              TESTN                   WD15                     30
060700971021     C                   MOVE      WD15          W001A             1
060800971021     C     *IN30         IFEQ      *OFF
060900971021     C     W001A         ORLT      '0'
061000971023     C     W001A         ANDNE     *BLANK
061100971021     C                   SETON                                        32
061200971021     C                   MOVE      *ZERO         VABQFT
061300010604     C                   eval      vinmsg = %trimr(vinmsg)
061400010604     C                             + ' ' + 'VHBQFT'
061500971021     C                   ELSE
061600971021     C                   MOVE      WD15          VABQFT
061700971021     C                   ENDIF
061800050118     C                   ENDIF
061900941229     C*
062000971021     C                   MOVE      *ZERO         VABCAS
062100971021     C                   MOVE      VHBCAS        WASC15
062200010719     C                   IF        WI15 = *blanks
062300010719     C                   EVAL      WI15 = *zeros
062400010719     C                   ENDIF
062500971023     C                   TESTN                   WI15                 30
062600971023     C  N30              TESTN                   WI15                   30
062700971023     C  N30              TESTN                   WI15                     30
062800971021     C                   MOVE      WI15          W001A             1
062900971021     C     *IN30         IFEQ      *OFF
063000971021     C     W001A         ORLT      '0'
063100971023     C     W001A         ANDNE     *BLANK
063200971021     C                   SETON                                        32
063300971021     C                   MOVE      *ZERO         VABCAS
063400010604     C                   eval      vinmsg = %trimr(vinmsg)
063500010604     C                             + ' ' + 'VHBCAS'
063600971021     C                   ELSE
063700971021     C                   MOVEL     WI15          VABCAS
063800010719     C                   IF        WD15 = *blanks
063900010719     C                   EVAL      WD15 = *zeros
064000010719     C                   ENDIF
064100971023     C                   TESTN                   WD15                 30
064200971023     C  N30              TESTN                   WD15                   30
064300971023     C  N30              TESTN                   WD15                     30
064400971021     C                   MOVE      WD15          W001A             1
064500971021     C     *IN30         IFEQ      *OFF
064600971021     C     W001A         ORLT      '0'
064700971023     C     W001A         ANDNE     *BLANK
064800971021     C                   SETON                                        32
064900971021     C                   MOVE      *ZERO         VABCAS
065000010604     C                   eval      vinmsg = %trimr(vinmsg)
065100010604     C                             + ' ' + 'VHBCAS'
065200971021     C                   ELSE
065300971021     C                   MOVE      WD15          VABCAS
065400971021     C                   ENDIF
065500040827     C                   ENDIF
065600971021     C*
065700971023     C                   TESTN                   VHBRMN               30
065800971023     C  N30              TESTN                   VHBRMN                 30
065900971023     C  N30              TESTN                   VHBRMN                   30
066000971021     C                   MOVE      VHBRMN        W001A             1
066100971021     C     *IN30         IFEQ      *OFF
066200971021     C     W001A         ORLT      '0'
066300971021     C                   SETON                                        32
066400971021     C                   MOVE      *ZERO         VABRMN
066500010604     C                   eval      vinmsg = %trimr(vinmsg)
066600010604     C                             + ' ' + 'VHBRMN'
066700971021     C                   ELSE
066800971021     C                   MOVE      VHBRMN        VABRMN
066900971021     C                   ENDIF
067000971021     C*
067100971023     C                   TESTN                   VHBNCD               30
067200971023     C  N30              TESTN                   VHBNCD                 30
067300971023     C  N30              TESTN                   VHBNCD                   30
067400971021     C                   MOVE      VHBNCD        W001A             1
067500971021     C     *IN30         IFEQ      *OFF
067600971021     C     W001A         ORLT      '0'
067700971023     C     W001A         ANDNE     *BLANK
067800971021     C                   SETON                                        32
067900971021     C                   MOVE      *ZERO         VABNCD
068000010604     C                   eval      vinmsg = %trimr(vinmsg)
068100010604     C                             + ' ' + 'VHBNCD'
068200971021     C                   ELSE
068300971021     C                   MOVE      VHBNCD        VABNCD
068400971021     C                   ENDIF
068500971021     C*
068600971023     C                   TESTN                   VHBNCA               30
068700971023     C  N30              TESTN                   VHBNCA                 30
068800971023     C  N30              TESTN                   VHBNCA                   30
068900971021     C                   MOVE      VHBNCA        W001A             1
069000971021     C     *IN30         IFEQ      *OFF
069100971021     C     W001A         ORLT      '0'
069200971023     C     W001A         ANDNE     *BLANK
069300971021     C                   SETON                                        32
069400971021     C                   MOVE      *ZERO         VABNCA
069500010604     C                   eval      vinmsg = %trimr(vinmsg)
069600010604     C                             + ' ' + 'VHBNCA'
069700971021     C                   ELSE
069800971021     C                   MOVE      VHBNCA        VABNCA
069900971021     C                   ENDIF
070000971021     C*
070100971023     C                   TESTN                   VHBZNC               30
070200971023     C  N30              TESTN                   VHBZNC                 30
070300971023     C  N30              TESTN                   VHBZNC                   30
070400971021     C                   MOVE      VHBZNC        W001A             1
070500971021     C     *IN30         IFEQ      *OFF
070600971021     C     W001A         ORLT      '0'
070700971023     C     W001A         ANDNE     *BLANK
070800971021     C                   SETON                                        32
070900971021     C                   MOVE      *ZERO         VABZNC
071000010604     C                   eval      vinmsg = %trimr(vinmsg)
071100010604     C                             + ' ' + 'VHBZNC'
071200971021     C                   ELSE
071300971021     C                   MOVE      VHBZNC        VABZNC
071400971021     C                   ENDIF
071500971021     C*
071600971023     C                   TESTN                   VHBDCR               30
071700971023     C  N30              TESTN                   VHBDCR                 30
071800971023     C  N30              TESTN                   VHBDCR                   30
071900971021     C                   MOVE      VHBDCR        W001A             1
072000971021     C     *IN30         IFEQ      *OFF
072100971021     C     W001A         ORLT      '0'
072200971023     C     W001A         ANDNE     *BLANK
072300971021     C                   SETON                                        32
072400971021     C                   MOVE      *ZERO         VABDCR
072500010604     C                   eval      vinmsg = %trimr(vinmsg)
072600010604     C                             + ' ' + 'VHBDCR'
072700971021     C                   ELSE
072800971021     C                   MOVE      VHBDCR        VABDCR
072900971021     C                   ENDIF
073000971021     C*
073100971023     C                   TESTN                   VHBHCR               30
073200971023     C  N30              TESTN                   VHBHCR                 30
073300971023     C  N30              TESTN                   VHBHCR                   30
073400971021     C                   MOVE      VHBHCR        W001A             1
073500971021     C     *IN30         IFEQ      *OFF
073600971021     C     W001A         ORLT      '0'
073700971023     C     W001A         ANDNE     *BLANK
073800971021     C                   SETON                                        32
073900971021     C                   MOVE      *ZERO         VABHCR
074000010604     C                   eval      vinmsg = %trimr(vinmsg)
074100010604     C                             + ' ' + 'VHBHCR'
074200971021     C                   ELSE
074300971021     C                   MOVE      VHBHCR        VABHCR
074400971021     C                   ENDIF
074500941229     C*
074600971021     C                   MOVE      *ZERO         VABVMD
074700971021     C                   MOVE      VHBVMD        WASC15
074800010719     C                   IF        WI15 = *blanks
074900010719     C                   EVAL      WI15 = *zeros
075000010719     C                   ENDIF
075100971023     C                   TESTN                   WI15                 30
075200971023     C  N30              TESTN                   WI15                   30
075300971023     C  N30              TESTN                   WI15                     30
075400971021     C                   MOVE      WI15          W001A             1
075500971021     C     *IN30         IFEQ      *OFF
075600971021     C     W001A         ORLT      '0'
075700971023     C     W001A         ANDNE     *BLANK
075800971021     C                   SETON                                        32
075900971021     C                   MOVE      *ZERO         VABVMD
076000010604     C                   eval      vinmsg = %trimr(vinmsg)
076100010604     C                             + ' ' + 'VHBVMD'
076200971021     C                   ELSE
076300971021     C                   MOVEL     WI15          VABVMD
076400010719     C                   IF        WD15 = *blanks
076500010719     C                   EVAL      WD15 = *zeros
076600010719     C                   ENDIF
076700971023     C                   TESTN                   WD15                 30
076800971023     C  N30              TESTN                   WD15                   30
076900971023     C  N30              TESTN                   WD15                     30
077000971021     C                   MOVE      WD15          W001A             1
077100971021     C     *IN30         IFEQ      *OFF
077200971021     C     W001A         ORLT      '0'
077300971023     C     W001A         ANDNE     *BLANK
077400971021     C                   SETON                                        32
077500971021     C                   MOVE      *ZERO         VABVMD
077600010604     C                   eval      vinmsg = %trimr(vinmsg)
077700010604     C                             + ' ' + 'VHBVMD'
077800971021     C                   ELSE
077900971021     C                   MOVE      WD15          VABVMD
078000971021     C                   ENDIF
078100050118     C                   ENDIF
078200971021     C*
078300971023     C                   TESTN                   VHBANT               30
078400971023     C  N30              TESTN                   VHBANT                 30
078500971023     C  N30              TESTN                   VHBANT                   30
078600971021     C                   MOVE      VHBANT        W001A             1
078700971021     C     *IN30         IFEQ      *OFF
078800971021     C     W001A         ORLT      '0'
078900971023     C     W001A         ANDNE     *BLANK
079000971021     C                   SETON                                        32
079100971021     C                   MOVE      *ZERO         VABANT
079200010604     C                   eval      vinmsg = %trimr(vinmsg)
079300010604     C                             + ' ' + 'VHBANT'
079400971021     C                   ELSE
079500971021     C                   MOVE      VHBANT        VABANT
079600971021     C                   ENDIF
079700971021     C*
079800971021     C* CAMPI ALFABETICI:
079900971021     C                   MOVE      VHBATB        VABATB
080000971021     C                   MOVE      VHBCBO        VABCBO
080100971021     C                   MOVE      VHBRSD        VABRSD
080200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
080300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
080400020117     C* ==
080500971021     C                   MOVE      VHBRD2        VABRD2
080600971021     C                   MOVE      VHBIND        VABIND
080700971021     C                   MOVE      VHBCAD        VABCAD
080800971021     C                   MOVE      VHBLOD        VABLOD
080900971021     C                   MOVE      VHBPRD        VABPRD
081000971021     C                   MOVE      VHBNZD        VABNZD
081100971021     C                   MOVE      VHBGC1        VABGC1
081200971021     C                   MOVE      VHBGC2        VABGC2
081300971021     C                   MOVE      VHBTSP        VABTSP
081400971021     C                   MOVE      VHBVAS        VABVAS
081500971021     C                   MOVE      VHBNAS        VABNAS
081600971021     C                   MOVE      VHBTIC        VABTIC
081700971021     C                   MOVE      VHBVCA        VABVCA
081800971021     C                   MOVE      VHBGCA        VABGCA
081900971021     C                   MOVE      VHBRMA        VABRMA
082000971021     C                   MOVE      VHBXCO        VABXCO
082100971021     C                   MOVE      VHBNOT        VABNOT
082200971021     C                   MOVE      VHBNT2        VABNT2
082300971021     C                   MOVE      VHBCTM        VABCTM
082400971021     C                   MOVE      VHBFFD        VABFFD
082500971021     C                   MOVE      VHBTCR        VABTCR
082600971021     C                   MOVE      VHBCTS        VABCTS
082700971021     C                   MOVE      VHBFTM        VABFTM
082800971021     C                   MOVE      VHBVAD        VABVAD
082900971021     C                   MOVE      VHBGMA        VABGMA
083000971021     C                   MOVE      VHBGGA        VABGGA
083100971021     C                   MOVE      VHBGVA        VABGVA
083200971021     C                   MOVE      VHBTC1        VABTC1
083300971021     C                   MOVE      VHBTC2        VABTC2
083400971021     C                   MOVE      VHBSCL        VABSCL
083500971021     C                   MOVE      VHBRMO        VABRMO
083600971021     C                   MOVE      VHBCMO        VABCMO
083700971021     C                   MOVE      VHBNMO        VABNMO
083800971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
083900971021     C* NELLA NAZIONE ORIGINALE
084000971021     C                   MOVE      VHBNMO        WA1D              1
084100971021     C                   MOVEL     VHBNMO        WA1S              1
084200971021     C     WA1D          IFNE      *BLANK
084300971021     C     WA1S          ANDEQ     *BLANK
084400971021     C                   MOVE      *BLANK        VABNMO
084500971021     C                   END
084600050209     C*
084700050303     C* NSP => Se nn valorizzata numero spedizione => stacco numeratore da AZNUM
084800050303     C                   if        VABNSP = *zeros
084900050209     C                   clear                   TRUL33DS
085000050209     C                   eval      I33OPE = *zeros
085100050209     C                   eval      I33CNU = 302
085200050209     C                   eval      I33NUM = 1
085300050209     C                   movel     TRUL33DS      KPJBU
085400050209     C                   call      'TRUL33R'
085500050209     C                   parm                    KPJBA
085600050209     C                   movel     KPJBU         TRUL33DS
085700050209     C                   if        O33ERR = *zeros
085800050209     C                   z-add     O33NRF        VABNSP
085900050209     C                   z-add     O33NRF        VATNSP
086000050209     C                   else
086100050209     C                   SETON                                        31
086200050209     C                   EVAL      vinmsg = %trimr(vinmsg)
086300050209     C                             + ' ' + 'VABNSP VATNSP'
086400050209     C                   endif
086500050303     C                   endif
086600050209     C*
086700050209     C* Forzo il codice trattamento merce a 7Q
086800050209     C                   EVAL      VABCTM = '7Q'
086900930409     C*
087000010604     C  N31              ADD       1             §CTROK            7 0
087100010604     C   32              ADD       1             §CTRMO            7 0
087200010604     C   31              ADD       1             §CTRNO            7 0
087300020913     C  N31              WRITE     FIVAB000
087400050209     C  N31              EXSR      WRIVAT
087500990910     C*
087600010601     C                   if        *in31 = *off and
087700010601     C                             *in32 = *off
087800990910     C                   eval      vinflg = '1'
087900990910     C                   else
088000000621     C                   eval      recko = vindta
088100050303     C                   if        %open(titvi5ps)
088200030407     C   50              except    rigadett
088300010612     C                   endif
088400990910     C                   eval      vinflg = '2'
088500990910     C                   endif
088600991022     C                   endif
088700990910     C                   endif
088800010601     C*
088900010601     C                   endif
089000000905     C*
089100000905     C                   else
089200000905     C                   eval      vinflg = '1'
089300000905     C                   endif
089400000905     C*
089500000905     C  N70              update    tivin000
089600991022     C*
089700991022     C  N70              ENDdo
089800010530     C
089900010530     C*
090000010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
090100010530     C                   if        cntNonEl = *zeros or
090200010530     C                             flgMulti = '0'
090300990910     C* Se non ci sono record con errori ...
090400010530     C                   if        §ctrno = 0 and
090500010601     C                             §ctrmo = 0 and
090600010530     C                             flgStato <> '2'
090700990910     C* ... restituisco esito OK.
090800990921     C                   eval      wrkesito = '0'
090900990910     C                   else
091000010601     C                   if        §ctrok > 0
091100990921     C                   eval      wrkesito = '1'
091200000710     C                   else
091300010615     C                   if        flgOk = '0'
091400010615     C                   eval      wrkesito = '2'
091500010615     C                   else
091600010615     C                   eval      wrkesito = '6'
091700010615     C                   endif
091800990910     C                   endif
091900000710     C                   endif
092000010530     C                   else
092100010530     C                   eval      wrkesito = '9'
092200010530     C                   endif
092300050209     C*
092400050209     C                   else
092500050209     C                   eval      wrkesito = '2'
092600050209     C                   endif
092700990910     C*
092800990914     C                   if        %open(tivin00r)
092900990908     C                   close     tivin00r
093000990914     C                   endif
093100020913     C                   if        %open(fivabwwr)
093200020913     C                   close     fivabwwr
093300990914     C                   endif
093400050209     C                   if        %open(fivatwwr)
093500050209     C                   close     fivatwwr
093600050209     C                   endif
093700010530     C*
093800010530     C                   if        vlrpoi <> 999
093900010530     C                   eval      invfil = vlrpoi
094000010530     C                   endif
094100990910     C*
094200990920     C                   if        §ctrok > 0
094300010530     C                             and invfil > *zeros
094400000613     C                   exsr      invio
094500990920     C                   endif
094600010612     C*
094700010612     C                   if        flgGiro = '1'
094800010612     C                   exsr      endela
094900010612     C                   endif
095000990920     C*
095100910830     C                   ENDSR
095200000613     C***
095300050209
095400050209
095500050209
095600050209     C*----------------------------------------------------*
095700050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
095800050209     C*----------------------------------------------------*
095900050209     C     PREVAT        BEGSR
096000050209     C*
096100050209     C* Compongo il nome del membro da dare al FIVATWWR
096200050209     C                   eval      parmbr = vlrhdl
096300050209     C                   movel     'M'           parmbr
096400050209     C                   eval      parccm = %subst(vlrKSC:2:7)
096500050209     C                   eval      paropz = '1'
096600050209     C* Effettuo la chiamata al CLLE preposto
096700050209     C                   call(e)   'TITVVTC'
096800050209     C                   parm                    parccm
096900050209     C                   parm                    parmbr
097000050209     C                   parm                    paropz
097100050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
097200050209     C                   if        %error
097300050209     C                   movel     '1'           chkcall
097400050209     C                   else
097500050209     C                   movel     '0'           chkcall
097600050209     C                   endif
097700050209     C*
097800050209     C                   ENDSR
097900050209
098000050209
098100990920
098200050209     C*----------------------------------------------------*
098300050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
098400050209     C*----------------------------------------------------*
098500050209     C     WRIVAT        BEGSR
098600050209     C*
098700050209     C* Valorizzo il buffer del file dettagli barcode colli
098800050209     C                   EVAL      VATFGS = VABFGS
098900050209     C                   EVAL      VATAAS = VABAAS
099000050209     C                   EVAL      VATLNP = VABLNP
099100050209     C                   EVAL      VATNSP = VABNSP
099200050209     C                   EVAL      VATCCM = VABCCM
099300050209     C                   EVAL      VATTRC='E'
099400070103     C                   EVAL      VATNOT = %trim(%editc(VABRMN:'Z'))
099500050303     C*
099600050303     C* Compongo tanti "chi sono" quanti sono i colli della spedizione
099700070103     C***  1             DO        VABNCL        wColloNum         3 0
099800070103     C***                MOVE(P)   wColloNum     wColloAlf         3
099900070103     C***                EVAL      VATNOT = %trim(%editc(VABRMN:'Z'))+wColloAlf
100000050303     C                   IF        VATNOT <> *BLANKS
100100050303     C                   WRITE     FIVAT000
100200050303     C                   ENDIF
100300070103     C***                ENDDO
100400050209     C*
100500050209     C                   ENDSR
100600010530
100700010530
100800050209
100900010530
101000010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
101100010530     C     repfil        BEGSR
101200010530     C*
101300010530     C                   if        invfil = *zeros and
101400010530     C                             depfil > *zeros and
101500010613     C                             (vinflg = *blanks or
101600010613     C                              vinflg = *zeros)
101700010530     C
101800010530     C                   eval      invfil = depfil
101900010530     C                   endif
102000010530     C*
102100010530     C                   if        depfil <> invfil and
102200010530     C                             invfil > *zeros
102300010530     C                   eval      flgMulti = '1'
102400010530     C                   if        vinflg = *blanks
102500010530     C                   add       1             cntNonEl
102600010530     C                   endif
102700010530     C                   endif
102800010530     C*
102900010530     C                   if        vinflg = '2'
103000010530     C                   eval      flgStato = '2'
103100010530     C                   endif
103200010530     C*
103300010530     C                   ENDSR
103400010530     C***
103500050209
103600050209
103700050209
103800050209
103900050209      /TITLE Invio dei dati al punto operativo.
104000050209     C     invio         BEGSR
104100050209     C*
104200050209     C* 1° invio FIVAT
104300050209     C                   reset                   dscmz
104400050209     C                   move      vlrpoi        cmzdst
104500050209     C                   eval      cmzfld = 'FIVATWWR'
104600050209     C                   eval      cmzmbd = vlrhdl
104700050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
104800050209     C***                if        prmfir = *blanks
104900050209     C                   eval      cmzfla = 'FIVAT00F'
105000050209     C                   eval      cmzmba = 'FIVAT00F'
105100050209     C***                else
105200050209     C***                eval      cmzfla = prmfir
105300050209     C***                eval      cmzmba = prmfir
105400050209     C***                endif
105500050209     C                   eval      cmznrr = *zeros
105600050209     C                   move      §ctrok        cmznrr
105700050209     C                   eval      cmzlba = vlrfl1
105800050209     C                   call(e)   'TIS711C'
105900050209     C                   parm                    dscmz
106000050209     C                   parm      *blanks       esito
106100050209     C                   if        %error
106200050209     C                             or cmzerr = '1'
106300050209     C                             or esito  = '1'
106400050209     C                   eval      wrkesito = '3'
106500050209     C                   else
106600050209     C*
106700050209     C* 2° invio FIVAB
106800050209     C                   reset                   dscmz
106900050209     C                   move      vlrpoi        cmzdst
107000050209     C                   eval      cmzfld = vlrfou
107100050209     C                   eval      cmzmbd = vlrhdl
107200050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
107300050209     C***                if        prmfir = *blanks
107400050209     C                   eval      cmzfla = 'FIVAB00F'
107500050209     C                   eval      cmzmba = 'FIVAB00F'
107600050209     C***                else
107700050209     C***                eval      cmzfla = prmfir
107800050209     C***                eval      cmzmba = prmfir
107900050209     C***                endif
108000050209     C                   eval      cmznrr = *zeros
108100050209     C                   move      §ctrok        cmznrr
108200050209     C                   eval      cmzlba = vlrfl1
108300050209     C                   call(e)   'TIS711C'
108400050209     C                   parm                    dscmz
108500050209     C                   parm      *blanks       esito
108600050209     C                   if        %error
108700050209     C                             or cmzerr = '1'
108800050209     C                             or esito  = '1'
108900050209     C                   eval      wrkesito = '3'
109000050209     C                   endif
109100050209     C                   endif
109200050209     C*
109300050209     C                   ENDSR
109400050209     C***
109500010530
109600010530
109700010530
109800010530
109900010530      /TITLE Invio dei dati al punto operativo.
110000010530     C     opeini        BEGSR
110100010530     C*
110200010530     C* Inizializzo flag e contatori operativi
110300010530     C                   movel     '0'           flgGiro           1
110400010530     C                   movel     '0'           flgMulti          1
110500010530     C                   movel     '1'           flgStato          1
110600010615     C                   movel     '0'           flgOk             1
110700010530     C                   z-add     *zeros        cntNonEl         10 0
110800010530     C                   z-add     *zeros        depfil            3 0
110900010530     C                   z-add     *zeros        invfil            3 0
111000030407     C*
111100050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
111200030407     C                   if        %subst(vlrppt:1:1) = 'S'
111300030407     C                   seton                                        50
111400030407     C                   else
111500030407     C                   setoff                                       50
111600030407     C                   endif
111700041001     C*
111800050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
111900041001     C                   if        %subst(vlrppt:2:1) = 'S'
112000041001     C                   seton                                        51
112100041001     C                   else
112200041001     C                   setoff                                       51
112300041001     C                   endif
112400010530     C*
112500010530     C                   ENDSR
112600010530     C***
112700010530
112800010530
112900070411
113000070411     C     *pssr         BEGSR
113100070411     C*
113200070411     C                   if        %open(tivin00r)
113300070411     C                   close     tivin00r
113400070411     C                   endif
113500070411     C                   if        %open(fivabwwr)
113600070411     C                   close     fivabwwr
113700070411     C                   endif
113800070411     C                   if        %open(fivatwwr)
113900070411     C                   close     fivatwwr
114000070411     C                   endif
114100070411     C*
114200070411     C* Effettuo la chiamata al CLLE preposto
114300070411     C                   call(e)   'TITVVTC'
114400070411     C                   parm                    parccm
114500070411     C                   parm                    parmbr
114600070411     C                   parm      '2'           paropz
114700070411     C*
114800070411     C                   eval      wrkesito = '2'
114900070411     C*
115000070411     C                   seton                                        LR
115100070411     C*
115200070411     C                   ENDSR     '*CANCL'
115300070411     C***
115400070411
115500010530
115600990910
115700000613     C     *inzsr        BEGSR
115800990910     C*
115900990910     C     *entry        plist
116000990920     C                   parm                    tivlrds
116100990921     C                   parm      wrkesito      esito
116200000724     C                   parm                    prmlit
116300000710     C                   parm                    prmfir
116400010606     C*
116500010606     C* CALCOLA LA DATA CORRENTE
116600010606     C                   time                    wn14             14 0
116700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
116800010606     C                   z-add     wn8           g08dat
116900010606     C                   z-add     *zeros        g08inv
117000010606     C                   movel     '0'           g08err
117100010606     C                   call      'XSRDA8'
117200010606     C                   parm                    wlbda8
117300010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
117400000613     C*
117500000613     C                   ENDSR
117600000613     C***
117700990908
117800050303     Otitvi5p   E            riepilogo         2
117900990915     O                                              'Upload via Internet'
118000990915     O                                           +1 'Traduzione TIVIN00R -'
118100020913     O                                           +1 'FIVABWWR'
118200990915     O                                           +1 'Testate bolle'
118300990915     O          E            riepilogo   2
118400990915     O                       wrkdata
118500990915     O                       wrkora              +1
118600990915     O                       procname            +1
118700990915     O          E            riepilogo   2
118800990915     O                                              'Cliente..................:'
118900990915     O                       VABCCM        z     +1
119000990915     O          E            riepilogo   2
119100990920     O                                              'Riferimento Strategi.....:'
119200990920     O                       vlrhdl              +1
119300990915     O          E            riepilogo   2
119400990915     O                                              'Giusti...................:'
119500971022     O                       §CTROK        2   +  1
119600990915     O          E            riepilogo   2
119700990915     O                                              'Sbagliati e corretti.....:'
119800971022     O                       §CTRMO        2   +  1
119900990915     O          E            riepilogo   2
120000990915     O                                              'Sbagliati e scartati.....:'
120100971022     O                       §CTRNO        2   +  1
120200000613
120300050303     Otitvi5ps  E            testdett          2
120400000613     O                                              'Upload via Internet'
120500000613     O                                           +1 'Traduzione TIVIN00R -'
120600020913     O                                           +1 'FIVABWWR'
120700000613     O                                           +1 'Testate bolle'
120800000616     O          E            testdett    3
120900000613     O                                           +2 'N° rec'
121000000613     O                                           +3 'Anteprima contenuto'
121100000616     O          E            rigadett    2
121200000613     O                       rrnum               +2
121300000621     O                       recko               +3
121400000616     O          E            findett     2
121500000613     O                       wrkdata
121600000613     O                       wrkora              +1
121700000613     O                       procname            +1
121800000616     O          E            findett     2
121900000613     O                                              'Cliente..................:'
122000000613     O                       VABCCM        z     +1
122100000616     O          E            findett     2
122200000613     O                                              'Riferimento Strategi.....:'
122300000613     O                       vlrhdl              +1
122400000616     O          E            findett     2
122500000613     O                                              'Giusti...................:'
122600000613     O                       §CTROK        2   +  1
122700000616     O          E            findett     2
122800000613     O                                              'Sbagliati e corretti.....:'
122900000613     O                       §CTRMO        2   +  1
123000000616     O          E            findett     2
123100000613     O                                              'Sbagliati e scartati.....:'
123200000613     O                       §CTRNO        2   +  1
123300000512** CMD - COMANDI CL
123400050303OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
123500050303OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
123600050303DLTOVR FILE(TITVI5P TITVI5PS) LVL(*)
123700000512
123800000512
