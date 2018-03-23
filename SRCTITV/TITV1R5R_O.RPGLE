000100020916      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400090511     Ftntbe01l  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700020916     FFIVABwwr  O    E             DISK    usropn
000800000613     Ftis7t4p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000000616     Ftis7t4ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000724     D*------------
001800990920     D dscmz         e ds                  inz
001900990910     D psds           sds
002000990910     D  procname         *PROC
002100941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002200941229     D* 10+3
002300941229     D                 DS
002400000313     D  WI15                   1     10
002500000313     D  WD15                  11     13
002600000313     D  WASC15                 1     13
002700941229     D*  6+1
002800941229     D                 DS
002900000313     D  WI09                   1      6
003000000313     D  WD09                   7      7
003100000313     D  WASC09                 1      7
003200941229     D*  2+3
003300941229     D                 DS
003400000313     D  WI07                   1      2
003500000313     D  WD07                   3      5
003600000313     D  WASC07                 1      5
003700000313     D*
003800990920     D tivlrds       e ds                  extname(tivlr00f)
003900041025     D tisi95ds      e ds
004000000313     D*---------------------------------------------------------
004100000313     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
004200000313     D*---------------------------------------------------------
004300000313     D                 ds
004400000313     D vhbatb                  1      1
004500000313     D vhbccm                  2      8
004600000313     D vhblnp                  9     11
004700000313     D vhbaas                 12     15
004800000313     D vhbmgs                 16     19
004900000313     D vhbnrs                 20     21
005000000313     D vhbnsp                 22     28
005100000313     D vhbcbo                 29     30
005200000313     D vhblna                 31     33
005300000313     D vhbrsd                 34     68
005400000313     D vhbrd2                 69    103
005500000313     D vhbind                104    138
005600000313     D vhbcad                139    147
005700000313     D vhblod                148    182
005800000313     D vhbprd                183    184
005900000313     D vhbnzd                185    187
006000000313     D vhbgc1                188    189
006100000313     D vhbgc2                190    191
006200000313     D vhbctr                192    194
006300000313     D vhbtsp                195    195
006400000313     D vhbias                196    208
006500000313     D vhbvas                209    211
006600000313     D vhbnas                212    226
006700000313     D vhbncl                227    231
006800000313     D vhbpkb                232    238
006900000313     D vhbvlb                239    243
007000000313     D vhbqft                244    256
007100000313     D vhbcas                257    269
007200000313     D vhbtic                270    271
007300000313     D vhbvca                272    274
007400000313     D vhbgca                275    276
007500000313     D vhbrmn                277    291
007600000313     D vhbrma                292    306
007700000313     D vhbncd                307    313
007800000313     D vhbnca                314    320
007900000313     D vhbxco                321    321
008000000313     D vhbnot                322    356
008100000313     D vhbnt2                357    391
008200000313     D vhbznc                392    393
008300000313     D vhbctm                394    395
008400000313     D vhbffd                396    396
008500000313     D vhbdcr                397    404
008600000313     D vhbtcr                405    405
008700000313     D vhbhcr                406    409
008800000313     D vhbcts                410    411
008900000313     D vhbftm                412    412
009000000313     D vhbvmd                413    425
009100000313     D vhbvad                426    428
009200000313     D vhbgma                429    430
009300000313     D vhbgga                431    432
009400000313     D vhbgva                433    434
009500000313     D vhbtc1                435    435
009600000313     D vhbtc2                436    436
009700000313     D vhbscl                437    437
009800000313     D vhbant                438    446
009900000313     D vhbrmo                447    471
010000000313     D vhbcmo                472    480
010100000313     D vhbnmo                481    483
010200000313     D  X_rec                  1    483    inz
010300000313     D*
010400990910     D esito           s              1
010500000724     D prmlit          s             10
010600000710     D prmfir          s             10
010700990921     D wrkesito        s                   like(esito)
010800990915     D wrkdata         s               d
010900990915     D wrkora          s               t
011000000613     D rrnum           s              6  0 INZ(*zeros)
011100000621     D recko           s            150    INZ(*blanks)
011200011123     D depcmd          s            150    INZ(*blanks)
011300000313     D*
011400090511
011500090511     D wNSR            s              2    INZ
011600090511     D fivabds       e ds                  extname(fivab00f)
011700090511     D wfivabds        s                   LIKE(fivabds)
011800090511
011900090511     D dosr          e ds                  inz
012000090511     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
012100090511     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
012200090511     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
012300090511     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
012400090511     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
012500090511     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
012600090511     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
012700090511     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
012800090511     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
012900090511     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
013000090511     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
013100090511
013200010606     D*------------------
013300010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
013400010606     D*------------------
013500010606     D WLBDA8          DS                  INZ
013600010606     D  G08DAT                 1      8  0
013700010606     D  G08INV                 9     16  0
013800010606     D  G08ERR                17     17
013900010606     D  G08TGI                18     22  0
014000090511     D*------------------
014100090511     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
014200090511     D*------------------
014300090511     D SkCliOSR        s             10    DIM(900)
014400090511     D*
014500090511     D DsCliOSR        ds
014600090511     D  wLnpOSR                       3
014700090511     D  wCliOSR                       7
014800090511     D*
014900090511     D SkLnpNrsOSR     s              5    DIM(900)
015000010606     D*
015100040407     D TxtInOut        S           2048
015200040407     D ElencoChar      S            256
015300040407     D TipoElenco      S              1
015400040407     D CharSost        S              1
015500040407     D UpperCase       S              1
015600040407     D ChkNull         S              1
015700040407     D CharNull        S              1
015800040407     D EsitoBon        S              1
015900010606
016000010606
016100010606
016200010606
016300990915     C                   time                    wrkdata
016400990915     C                   time                    wrkora
016500000913     C                   reset                   rrnum
016600990921     C                   reset                   esito
016700990921     C                   reset                   wrkesito
016800090511     C*
016900090511     C* CARICO LE TABELLE OCCORRENTI
017000090511     C                   exsr      CARTAB
017100010601     C*
017200010601     C                   exsr      opeini
017300010601     C                   exsr      rwvab
017400041025     C*
017500041025     C* Esegue lancio TISI95R solo x chiusura
017600041025     C                   CLEAR                   TISI95DS
017700041025     C                   EVAL      I95TLA = 'C'
017800041025     C                   CALL      'TISI95R'
017900041025     C                   PARM                    TISI95DS
018000010601     C*
018100010601     C                   seton                                        lr
018200010601
018300010601
018400010601
018500010601
018600010601     C*--------------------------------------------------------
018700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
018800010601     C*--------------------------------------------------------
018900010601     C     PREELA        BEGSR
019000010601     C*
019100010601     C* SE OCCORRE SPEDIRE IN FILIALE
019200010601     C                   if        invfil <> *zeros and
019300010601     C                             flgGiro = '0'
019400010601     C*
019500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
019600010601     C                   eval      flgGiro = '1'
019700000724     C*
019800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
019900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
020000010601     C     invfil        chain     azorg01l
020100000724     C                   if        %found
020200000724     C                   movel(p)  CMD(1)        depcmd
020300020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
020400000724     C*
020500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
020600011123     C                   Z-ADD     150           LENGH            15 5
020700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020800000724     C                   PARM                    depcmd
020900000724     C                   PARM                    LENGH
021000000724     C*
021100000724     C                   endif
021200000616     C*
021300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
021400000616     C                   movel(p)  CMD(2)        depcmd
021500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
021600000616     C*
021700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
021800011123     C                   Z-ADD     150           LENGH            15 5
021900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
022000000616     C                   PARM                    depcmd
022100000616     C                   PARM                    LENGH
022200000613     C*
022300000616     C                   if        not %open(tis7t4ps)
022400000616     C                   open      tis7t4ps
022500030407     C   50              except    testdett
022600000613     C                   endif
022700010601     C*
022800010601     C                   endif
022900010601     C*
023000010601     C                   ENDSR
023100010601     C***
023200010601
023300010601
023400010601
023500010601
023600010601
023700010601
023800010601     C*--------------------------------------------------------
023900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
024000010601     C*--------------------------------------------------------
024100010601     C     ENDELA        BEGSR
024200990915     C*
024300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
024400000613     C*
024500000616     C                   if        %open(tis7t4ps)
024600030407     C   50              except    findett
024700000616     C                   close     tis7t4ps
024800000613     C                   endif
024900000616     C*
025000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
025100011123     C                   Z-ADD     150           LENGH            15 5
025200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
025300000616     C                   PARM                    CMD(3)
025400000616     C                   PARM                    LENGH
025500090511     C*
025600090511     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
025700090511     C   54              call(e)   'TITV84R'
025800090511     C                   parm      'C'           InTipl
025900090511     C                   parm                    tivlrds
026000090511     C                   parm                    wfivabds
026100010601     C*
026200010601     C                   ENDSR
026300010601     C***
026400010601
026500010601
026600000613
026700000613
026800000613
026900000613     C*--------------------------------------------------------
027000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
027100000613     C*--------------------------------------------------------
027200000621     C     STPR          BEGSR
027300000613     C*
027400000613     C                   if        not %open(tis7t4p)
027500000613     C                   open      tis7t4p
027600990915     C                   endif
027700990915     C*
027800030407     C   50              except    riepilogo
027900990915     C*
028000000613     C                   if        %open(tis7t4p)
028100000613     C                   close     tis7t4p
028200990914     C                   endif
028300000613     C*
028400000613     C                   ENDSR
028500000613     C***
028600000613
028700010601
028800010601
028900010601
029000010601
029100910830     C*--------------------------------------------------------
029200020916     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
029300910830     C*--------------------------------------------------------
029400941229     C     RWVAB         BEGSR
029500990910     C*
029600990914     C                   if        not %open(tivin00r)
029700990908     C                   open      tivin00r
029800990914     C                   endif
029900020916     C                   if        not %open(fivabwwr)
030000020916     C                   open      fivabwwr
030100990914     C                   endif
030200990910     C*
030300990910     C                   clear                   §CTROK
030400990910     C                   clear                   §CTRMO
030500990910     C                   clear                   §CTRNO
030600990910     C*
030700921023     C                   DO        *HIVAL
030800990913     C*
030900990915     C                   READ      tivin00r                               70
031000010618     C*
031100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
031200010618     C                   if        vinflg = '1'
031300010618     C                   eval      flgOk = '1'
031400010618     C                   endif
031500010618     C*
031600051017     C                   if        vindta  > *blanks
031700000613     C                   add       1             rrnum
031800990913     C*
031900010601     C                   if        *in70 = *off and
032000990913     C                             (vinflg = *blanks
032100010711     C                              or vinflg = '0'
032200010711     C                              or vinflg = '2')
032300990913     C*
032400020916     C                   clear                   FIVAB000
032500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
032600010711     C                   if        vinflg = *blanks or vinflg = '0'
032700010711     C                   clear                   vinmsg
032800010711     C                   endif
032900010711     C*
033000000313     C                   eval      x_rec  = vindta
033100991022     C*
033200991022     C                   testb     x'3F'         vhbatb                   02
033300991022     C                   if        not *in02
033400991022     C*
033500971021     C                   SETOFF                                       303132
033600971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
033700971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
033800971023     C*    OPPURE TUTTI BLANKS
033900010601     C*
034000010601     C                   TESTN                   VHBLNP               30
034100010601     C  N30              TESTN                   VHBLNP                 30
034200010601     C  N30              TESTN                   VHBLNP                   30
034300010601     C                   MOVE      VHBLNP        W001A             1
034400010601     C     *IN30         IFEQ      *OFF
034500010601     C     W001A         ORLT      '0'
034600010601     C                   SETON                                        31
034700010601     C                   eval      vinmsg = %trimr(vinmsg)
034800010601     C                             + ' ' + 'VHBLNP'
034900010601     C                   ELSE
035000010601     C                   MOVE      VHBLNP        VABLNP
035100010601     C     VABLNP        IFLE      *ZEROS
035200010601     C                   SETON                                        31
035300010601     C                   eval      vinmsg = %trimr(vinmsg)
035400010601     C                             + ' ' + 'VHBLNP'
035500010601     C                   ENDIF
035600010601     C                   ENDIF
035700010601     C*
035800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
035900010601     C                   eval      depfil = VABLNP
036000010601     C                   exsr      repfil
036100010601     C                   if        depfil = invfil
036200021014     C                   if        vlrpoi = 999
036300021010     C                   MOVE(P)   invfil        VABFGS
036400021014     C                   else
036500021014     C                   MOVE(P)   vlrpoi        VABFGS
036600021014     C                   endif
036700010601     C*
036800010601     C                   exsr      PREELA
036900010601     C*
037000971023     C                   TESTN                   VHBCCM               30
037100971023     C  N30              TESTN                   VHBCCM                 30
037200971023     C  N30              TESTN                   VHBCCM                   30
037300971021     C                   MOVE      VHBCCM        W001A             1
037400971021     C     *IN30         IFEQ      *OFF
037500971021     C     W001A         ORLT      '0'
037600971021     C                   SETON                                        31
037700010604     C                   eval      vinmsg = %trimr(vinmsg)
037800010604     C                             + ' ' + 'VHBCCM'
037900971021     C                   ELSE
038000971021     C                   MOVE      VHBCCM        VABCCM
038100971024     C     VABCCM        IFLE      *ZEROS
038200971024     C                   SETON                                        31
038300010604     C                   eval      vinmsg = %trimr(vinmsg)
038400010604     C                             + ' ' + 'VHBCCM'
038500971024     C                   ENDIF
038600971021     C                   ENDIF
038700971021     C*
038800971023     C                   TESTN                   VHBAAS               30
038900971023     C  N30              TESTN                   VHBAAS                 30
039000971023     C  N30              TESTN                   VHBAAS                   30
039100971021     C                   MOVE      VHBAAS        W001A             1
039200971021     C     *IN30         IFEQ      *OFF
039300971021     C     W001A         ORLT      '0'
039400971023     C     W001A         ANDNE     *BLANK
039500971021     C                   SETON                                        32
039600010606     C                   MOVEL     datcor        VABAAS
039700010604     C                   eval      vinmsg = %trimr(vinmsg)
039800010604     C                             + ' ' + 'VHBAAS'
039900971021     C                   ELSE
040000971021     C                   MOVE      VHBAAS        VABAAS
040100971021     C                   ENDIF
040200971021     C*
040300971023     C                   TESTN                   VHBMGS               30
040400971023     C  N30              TESTN                   VHBMGS                 30
040500971023     C  N30              TESTN                   VHBMGS                   30
040600971021     C                   MOVE      VHBMGS        W001A             1
040700971021     C     *IN30         IFEQ      *OFF
040800971021     C     W001A         ORLT      '0'
040900971023     C     W001A         ANDNE     *BLANK
041000971021     C                   SETON                                        32
041100010601     C                   MOVE      '0101'        VABMGS
041200010604     C                   eval      vinmsg = %trimr(vinmsg)
041300010604     C                             + ' ' + 'VHBMGS'
041400971021     C                   ELSE
041500971021     C                   MOVE      VHBMGS        VABMGS
041600971021     C                   ENDIF
041700971021     C*
041800971023     C                   TESTN                   VHBNRS               30
041900971023     C  N30              TESTN                   VHBNRS                 30
042000971023     C  N30              TESTN                   VHBNRS                   30
042100971021     C                   MOVE      VHBNRS        W001A             1
042200971021     C     *IN30         IFEQ      *OFF
042300971021     C     W001A         ORLT      '0'
042400971023     C     W001A         ANDNE     *BLANK
042500971021     C                   SETON                                        32
042600971021     C                   MOVE      *ZERO         VABNRS
042700010604     C                   eval      vinmsg = %trimr(vinmsg)
042800010604     C                             + ' ' + 'VHBNRS'
042900971021     C                   ELSE
043000971021     C                   MOVE      VHBNRS        VABNRS
043100971021     C                   ENDIF
043200971021     C*
043300971023     C                   TESTN                   VHBNSP               30
043400971023     C  N30              TESTN                   VHBNSP                 30
043500971023     C  N30              TESTN                   VHBNSP                   30
043600971021     C                   MOVE      VHBNSP        W001A             1
043700971021     C     *IN30         IFEQ      *OFF
043800971021     C     W001A         ORLT      '0'
043900971023     C     W001A         ANDNE     *BLANK
044000971021     C                   SETON                                        32
044100971021     C                   MOVE      *ZERO         VABNSP
044200010604     C                   eval      vinmsg = %trimr(vinmsg)
044300010604     C                             + ' ' + 'VHBNSP'
044400971021     C                   ELSE
044500971021     C                   MOVE      VHBNSP        VABNSP
044600971021     C                   ENDIF
044700971021     C*
044800971023     C                   TESTN                   VHBLNA               30
044900971023     C  N30              TESTN                   VHBLNA                 30
045000971023     C  N30              TESTN                   VHBLNA                   30
045100971021     C                   MOVE      VHBLNA        W001A             1
045200971021     C     *IN30         IFEQ      *OFF
045300971021     C     W001A         ORLT      '0'
045400971023     C     W001A         ANDNE     *BLANK
045500971021     C                   SETON                                        32
045600971021     C                   MOVE      *ZERO         VABLNA
045700010604     C                   eval      vinmsg = %trimr(vinmsg)
045800010604     C                             + ' ' + 'VHBLNA'
045900971021     C                   ELSE
046000971021     C                   MOVE      VHBLNA        VABLNA
046100971021     C                   ENDIF
046200971021     C*
046300971023     C                   TESTN                   VHBCTR               30
046400971023     C  N30              TESTN                   VHBCTR                 30
046500971023     C  N30              TESTN                   VHBCTR                   30
046600971021     C                   MOVE      VHBCTR        W001A             1
046700971021     C     *IN30         IFEQ      *OFF
046800971021     C     W001A         ORLT      '0'
046900971023     C     W001A         ANDNE     *BLANK
047000971021     C                   SETON                                        32
047100971021     C                   MOVE      *ZERO         VABCTR
047200010604     C                   eval      vinmsg = %trimr(vinmsg)
047300010604     C                             + ' ' + 'VHBCTR'
047400971021     C                   ELSE
047500971021     C                   MOVE      VHBCTR        VABCTR
047600971021     C                   ENDIF
047700941229     C*
047800971021     C                   MOVE      *ZERO         VABIAS
047900941229     C                   MOVE      VHBIAS        WASC15
048000971023     C                   TESTN                   WI15                 30
048100971023     C  N30              TESTN                   WI15                   30
048200971023     C  N30              TESTN                   WI15                     30
048300971021     C                   MOVE      WI15          W001A             1
048400971021     C     *IN30         IFEQ      *OFF
048500971021     C     W001A         ORLT      '0'
048600971023     C     W001A         ANDNE     *BLANK
048700971021     C                   SETON                                        32
048800971021     C                   MOVE      *ZERO         VABIAS
048900010604     C                   eval      vinmsg = %trimr(vinmsg)
049000010604     C                             + ' ' + 'VHBIAS'
049100971021     C                   ELSE
049200971021     C                   MOVEL     WI15          VABIAS
049300971023     C                   TESTN                   WD15                 30
049400971023     C  N30              TESTN                   WD15                   30
049500971023     C  N30              TESTN                   WD15                     30
049600971021     C                   MOVE      WD15          W001A             1
049700971021     C     *IN30         IFEQ      *OFF
049800971021     C     W001A         ORLT      '0'
049900971023     C     W001A         ANDNE     *BLANK
050000971021     C                   SETON                                        32
050100971021     C                   MOVE      *ZERO         VABIAS
050200010604     C                   eval      vinmsg = %trimr(vinmsg)
050300010604     C                             + ' ' + 'VHBIAS'
050400971021     C                   ELSE
050500971021     C                   MOVE      WD15          VABIAS
050600971021     C                   ENDIF
050700050118     C                   ENDIF
050800941229     C*
050900971023     C                   TESTN                   VHBNCL               30
051000971023     C  N30              TESTN                   VHBNCL                 30
051100971023     C  N30              TESTN                   VHBNCL                   30
051200971021     C                   MOVE      VHBNCL        W001A             1
051300971021     C     *IN30         IFEQ      *OFF
051400971021     C     W001A         ORLT      '0'
051500971021     C                   SETON                                        32
051600971021     C                   MOVE      *ZERO         VABNCL
051700010604     C                   eval      vinmsg = %trimr(vinmsg)
051800010604     C                             + ' ' + 'VHBNCL'
051900971021     C                   ELSE
052000971021     C                   MOVE      VHBNCL        VABNCL
052100971021     C                   ENDIF
052200941229     C*
052300971021     C                   MOVE      *ZERO         VABPKB
052400971021     C                   MOVE      VHBPKB        WASC09
052500050503     C                   IF        WI09 = *blanks
052600050503     C                   EVAL      WI09 = *zeros
052700050503     C                   ENDIF
052800971023     C                   TESTN                   WI09                 30
052900971023     C  N30              TESTN                   WI09                   30
053000971023     C  N30              TESTN                   WI09                     30
053100971021     C                   MOVE      WI09          W001A             1
053200971021     C     *IN30         IFEQ      *OFF
053300971021     C     W001A         ORLT      '0'
053400971021     C                   SETON                                        32
053500971021     C                   MOVE      *ZERO         VABPKB
053600010604     C                   eval      vinmsg = %trimr(vinmsg)
053700010604     C                             + ' ' + 'VHBPKB'
053800971021     C                   ELSE
053900971021     C                   MOVEL     WI09          VABPKB
054000971023     C                   TESTN                   WD09                 30
054100971023     C  N30              TESTN                   WD09                     30
054200971021     C                   MOVE      WD09          W001A             1
054300971021     C     *IN30         IFEQ      *OFF
054400971021     C     W001A         ORLT      '0'
054500971023     C     W001A         ANDNE     *BLANK
054600971021     C                   SETON                                        32
054700971021     C                   MOVE      *ZERO         VABPKB
054800010604     C                   eval      vinmsg = %trimr(vinmsg)
054900010604     C                             + ' ' + 'VHBPKB'
055000971021     C                   ELSE
055100971021     C                   MOVE      WD09          VABPKB
055200971021     C                   ENDIF
055300050118     C                   ENDIF
055400971021     C*
055500041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
055600041001     C                   IF        *IN51 = *OFF
055700971021     C                   MOVE      *ZERO         VABVLB
055800971021     C                   MOVE      VHBVLB        WASC07
055900050503     C                   IF        WI07 = *blanks
056000050503     C                   EVAL      WI07 = *zeros
056100050503     C                   ENDIF
056200971023     C                   TESTN                   WI07                 30
056300971023     C  N30              TESTN                   WI07                   30
056400971023     C  N30              TESTN                   WI07                     30
056500971021     C                   MOVE      WI07          W001A             1
056600971021     C     *IN30         IFEQ      *OFF
056700971021     C     W001A         ORLT      '0'
056800971023     C     W001A         ANDNE     *BLANK
056900971021     C                   SETON                                        32
057000971021     C                   MOVE      *ZERO         VABVLB
057100010604     C                   eval      vinmsg = %trimr(vinmsg)
057200010604     C                             + ' ' + 'VHBVLB'
057300971021     C                   ELSE
057400971021     C                   MOVEL     WI07          VABVLB
057500971023     C                   TESTN                   WD07                 30
057600971023     C  N30              TESTN                   WD07                   30
057700971023     C  N30              TESTN                   WD07                     30
057800971021     C                   MOVE      WD07          W001A             1
057900971021     C     *IN30         IFEQ      *OFF
058000971021     C     W001A         ORLT      '0'
058100971023     C     W001A         ANDNE     *BLANK
058200971021     C                   SETON                                        32
058300971021     C                   MOVE      *ZERO         VABVLB
058400010604     C                   eval      vinmsg = %trimr(vinmsg)
058500010604     C                             + ' ' + 'VHBVLB'
058600971021     C                   ELSE
058700971021     C                   MOVE      WD07          VABVLB
058800971021     C                   ENDIF
058900050118     C                   ENDIF
059000041001     C                   ELSE
059100041001     C* ...altrimenti forzo azzeramento del volume
059200041001     C                   Z-ADD     *zeros        VABVLB
059300041001     C                   ENDIF
059400941229     C*
059500971021     C                   MOVE      *ZERO         VABQFT
059600971021     C                   MOVE      VHBQFT        WASC15
059700971023     C                   TESTN                   WI15                 30
059800971023     C  N30              TESTN                   WI15                   30
059900971023     C  N30              TESTN                   WI15                     30
060000971021     C                   MOVE      WI15          W001A             1
060100971021     C     *IN30         IFEQ      *OFF
060200971021     C     W001A         ORLT      '0'
060300971023     C     W001A         ANDNE     *BLANK
060400971021     C                   SETON                                        32
060500971021     C                   MOVE      *ZERO         VABQFT
060600010604     C                   eval      vinmsg = %trimr(vinmsg)
060700010604     C                             + ' ' + 'VHBQFT'
060800971021     C                   ELSE
060900971021     C                   MOVEL     WI15          VABQFT
061000971023     C                   TESTN                   WD15                 30
061100971023     C  N30              TESTN                   WD15                   30
061200971023     C  N30              TESTN                   WD15                     30
061300971021     C                   MOVE      WD15          W001A             1
061400971021     C     *IN30         IFEQ      *OFF
061500971021     C     W001A         ORLT      '0'
061600971023     C     W001A         ANDNE     *BLANK
061700971021     C                   SETON                                        32
061800971021     C                   MOVE      *ZERO         VABQFT
061900010604     C                   eval      vinmsg = %trimr(vinmsg)
062000010604     C                             + ' ' + 'VHBQFT'
062100971021     C                   ELSE
062200971021     C                   MOVE      WD15          VABQFT
062300971021     C                   ENDIF
062400050118     C                   ENDIF
062500941229     C*
062600971021     C                   MOVE      *ZERO         VABCAS
062700971021     C                   MOVE      VHBCAS        WASC15
062800971023     C                   TESTN                   WI15                 30
062900971023     C  N30              TESTN                   WI15                   30
063000971023     C  N30              TESTN                   WI15                     30
063100971021     C                   MOVE      WI15          W001A             1
063200971021     C     *IN30         IFEQ      *OFF
063300971021     C     W001A         ORLT      '0'
063400971023     C     W001A         ANDNE     *BLANK
063500971021     C                   SETON                                        32
063600971021     C                   MOVE      *ZERO         VABCAS
063700010604     C                   eval      vinmsg = %trimr(vinmsg)
063800010604     C                             + ' ' + 'VHBCAS'
063900971021     C                   ELSE
064000971021     C                   MOVEL     WI15          VABCAS
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
074800971023     C                   TESTN                   WI15                 30
074900971023     C  N30              TESTN                   WI15                   30
075000971023     C  N30              TESTN                   WI15                     30
075100971021     C                   MOVE      WI15          W001A             1
075200971021     C     *IN30         IFEQ      *OFF
075300971021     C     W001A         ORLT      '0'
075400971023     C     W001A         ANDNE     *BLANK
075500971021     C                   SETON                                        32
075600971021     C                   MOVE      *ZERO         VABVMD
075700010604     C                   eval      vinmsg = %trimr(vinmsg)
075800010604     C                             + ' ' + 'VHBVMD'
075900971021     C                   ELSE
076000971021     C                   MOVEL     WI15          VABVMD
076100971023     C                   TESTN                   WD15                 30
076200971023     C  N30              TESTN                   WD15                   30
076300971023     C  N30              TESTN                   WD15                     30
076400971021     C                   MOVE      WD15          W001A             1
076500971021     C     *IN30         IFEQ      *OFF
076600971021     C     W001A         ORLT      '0'
076700971023     C     W001A         ANDNE     *BLANK
076800971021     C                   SETON                                        32
076900971021     C                   MOVE      *ZERO         VABVMD
077000010604     C                   eval      vinmsg = %trimr(vinmsg)
077100010604     C                             + ' ' + 'VHBVMD'
077200971021     C                   ELSE
077300971021     C                   MOVE      WD15          VABVMD
077400971021     C                   ENDIF
077500050118     C                   ENDIF
077600971021     C*
077700971023     C                   TESTN                   VHBANT               30
077800971023     C  N30              TESTN                   VHBANT                 30
077900971023     C  N30              TESTN                   VHBANT                   30
078000971021     C                   MOVE      VHBANT        W001A             1
078100971021     C     *IN30         IFEQ      *OFF
078200971021     C     W001A         ORLT      '0'
078300971023     C     W001A         ANDNE     *BLANK
078400971021     C                   SETON                                        32
078500971021     C                   MOVE      *ZERO         VABANT
078600010604     C                   eval      vinmsg = %trimr(vinmsg)
078700010604     C                             + ' ' + 'VHBANT'
078800971021     C                   ELSE
078900971021     C                   MOVE      VHBANT        VABANT
079000971021     C                   ENDIF
079100971021     C*
079200971021     C* CAMPI ALFABETICI:
079300971021     C                   MOVE      VHBATB        VABATB
079400971021     C                   MOVE      VHBCBO        VABCBO
079500971021     C                   MOVE      VHBRSD        VABRSD
079600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
079700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
079800020117     C* ==
079900971021     C                   MOVE      VHBRD2        VABRD2
080000971021     C                   MOVE      VHBIND        VABIND
080100971021     C                   MOVE      VHBCAD        VABCAD
080200971021     C                   MOVE      VHBLOD        VABLOD
080300971021     C                   MOVE      VHBPRD        VABPRD
080400971021     C                   MOVE      VHBNZD        VABNZD
080500971021     C                   MOVE      VHBGC1        VABGC1
080600971021     C                   MOVE      VHBGC2        VABGC2
080700971021     C                   MOVE      VHBTSP        VABTSP
080800971021     C                   MOVE      VHBVAS        VABVAS
080900971021     C                   MOVE      VHBNAS        VABNAS
081000971021     C                   MOVE      VHBTIC        VABTIC
081100971021     C                   MOVE      VHBVCA        VABVCA
081200971021     C                   MOVE      VHBGCA        VABGCA
081300971021     C                   MOVE      VHBRMA        VABRMA
081400971021     C                   MOVE      VHBXCO        VABXCO
081500971021     C                   MOVE      VHBNOT        VABNOT
081600971021     C                   MOVE      VHBNT2        VABNT2
081700971021     C                   MOVE      VHBCTM        VABCTM
081800971021     C                   MOVE      VHBFFD        VABFFD
081900971021     C                   MOVE      VHBTCR        VABTCR
082000971021     C                   MOVE      VHBCTS        VABCTS
082100971021     C                   MOVE      VHBFTM        VABFTM
082200971021     C                   MOVE      VHBVAD        VABVAD
082300971021     C                   MOVE      VHBGMA        VABGMA
082400971021     C                   MOVE      VHBGGA        VABGGA
082500971021     C                   MOVE      VHBGVA        VABGVA
082600971021     C                   MOVE      VHBTC1        VABTC1
082700971021     C                   MOVE      VHBTC2        VABTC2
082800971021     C                   MOVE      VHBSCL        VABSCL
082900971021     C                   MOVE      VHBRMO        VABRMO
083000971021     C                   MOVE      VHBCMO        VABCMO
083100971021     C                   MOVE      VHBNMO        VABNMO
083200971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
083300060329     C* NELLA NAZIONE MITTENTE ORIGINALE
083400060331     C                   IF        %trim(VHBNMO) < 'A'
083500060329     C                   MOVE      *BLANKS       VABNMO
083600060329     C                   ENDIF
083700090807     C*
083800090807     C* Apporto le personalizzazioni cliente
083900090807     C                   EVAL      VABCCM = 1050762
084000090807     C                   EVAL      VABLNP = 105
084100090807     C                   EVAL      VABNRS = 00
084200090807     C                   EVAL      VABCTR = 000
084300090807     C                   EVAL      VABTSP = 'D'
084400090807     C                   EVAL      VABCBO = '1'
084500090807     C                   EVAL      VABCTM = '7Q'
084600090807     C                   Z-ADD     VABRMN        VABNSP
084700090807     C*
084800090807     C* Considerazioni sul contenuto di campi precedentemente valorizzati
084900090807     C                   IF        VABCAS  > *zeros
085000090807     C                   IF        VABCBO = '1'
085100090807     C                   EVAL      VABCBO = '4'
085200090807     C                   ENDIF
085300090807     C                   IF        VABCBO = '2'
085400090807     C                   EVAL      VABCBO = '6'
085500090807     C                   ENDIF
085600090807     C                   ENDIF
085700050418     C*
085800050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
085900050418     C                   IF        %trim(VABNZD) = 'I'   OR
086000050418     C                             %trim(VABNZD) = 'IT'  OR
086100050418     C                             %trim(VABNZD) = 'ITA'
086200050418     C                   EVAL      VABNZD = *blanks
086300050418     C                   ENDIF
086400041025     C*
086500041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
086600041025     C                   IF        *IN52 = *ON
086700041025     C* Reperisco la provincia dal CAP e dalla località
086800041025     C                   IF        VABCAD <> *blanks AND
086900050907     C                             VABPRD  = *blanks AND
087000050907     C                             VABNZD  = *blanks
087100041025     C                   CLEAR                   TISI95DS
087200041025     C                   EVAL      I95TCN = '3'
087300041025     C                   Z-ADD     datcor        I95DAT
087400041025     C                   EVAL      I95CAP = VABCAD
087500041025     C                   EVAL      I95LOC = VABLOD
087600041025     C                   CALL      'TISI95R'
087700041025     C                   PARM                    TISI95DS
087800041025     C                   EVAL      VABPRD = O95PRV
087900041025     C                   ENDIF
088000041025     C                   ENDIF
088100090216     C*
088200040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
088300040630     C                   exsr      exeBon
088400090511     C*
088500090511     C* Se presente particolarità consegna 'RC' e se il codice
088600090511     C* cliente mittente è valorizzato e numero serie ORM nn passata
088700090511     C* nei parametri verifico se il cliente è abilitato in tabella 'OSR'
088800090511     C                   IF        VABGMA = 'RC'
088900090511     C                   IF        VABCCM > *ZEROS
089000090511     C                   IF        wNSR = *BLANKS
089100090511     C                   Z-ADD     1             jOSR
089200090511     C                   move(p)   VABLNP        wLnpOSR
089300090511     C                   move(p)   VABCCM        wCliOSR
089400090511     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
089500090511     C                   IF        %found
089600090511     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
089700090511     C                   EVAL      %subst(vlrppt:4:2) = wNSR
089800090511     C                   ELSE
089900090511     C                   SETON                                        31
090000090511     C                   EVAL      vinmsg = %trimr(vinmsg)
090100090511     C                             + ' ' + 'ORMNRS(tabellaOSR)'
090200090511     C                   ENDIF
090300090511     C                   ENDIF
090400090511     C                   ENDIF
090500090511     C                   ENDIF
090600930409     C*
090700010604     C  N31              ADD       1             §CTROK            7 0
090800010604     C   32              ADD       1             §CTRMO            7 0
090900010604     C   31              ADD       1             §CTRNO            7 0
091000020916     C  N31              WRITE     FIVAB000
091100090511     C*
091200090511     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
091300090511     C                   if        *IN31 = *OFF AND
091400090511     C                             vabGMA = 'RC'
091500090511     C*
091600090511     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
091700090511     C                   seton                                        54
091800090511     C*
091900090511     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
092000090511     C                   eval      wfivabds = fivabds
092100090511     C                   call      'TITV84R'
092200090511     C                   parm      *blanks       InTipl            1
092300090511     C                   parm                    tivlrds
092400090511     C                   parm                    wfivabds
092500090511     C*
092600090511     C                   endif
092700090511     C*
092800990910     C*
092900010601     C                   if        *in31 = *off and
093000010601     C                             *in32 = *off
093100990910     C                   eval      vinflg = '1'
093200990910     C                   else
093300000621     C                   eval      recko = vindta
093400010612     C                   if        %open(tis7t4ps)
093500030407     C   50              except    rigadett
093600010612     C                   endif
093700990910     C                   eval      vinflg = '2'
093800990910     C                   endif
093900000905     C                   endif
094000000905     C                   endif
094100010601     C*
094200010601     C                   endif
094300000905     C*
094400000905     C                   else
094500000905     C                   eval      vinflg = '1'
094600000905     C                   endif
094700000905     C*
094800000905     C  N70              update    tivin000
094900000905     C*
095000991022     C  N70              ENDdo
095100010601     C
095200010601     C*
095300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
095400010601     C                   if        cntNonEl = *zeros or
095500010601     C                             flgMulti = '0'
095600990910     C* Se non ci sono record con errori ...
095700010601     C                   if        §ctrno = 0 and
095800010601     C                             §ctrmo = 0 and
095900010601     C                             flgStato <> '2'
096000990910     C* ... restituisco esito OK.
096100990921     C                   eval      wrkesito = '0'
096200990910     C                   else
096300010601     C                   if        §ctrok > 0
096400000710     C                   eval      wrkesito = '1'
096500000710     C                   else
096600010615     C                   if        flgOk = '0'
096700010615     C                   eval      wrkesito = '2'
096800010615     C                   else
096900010615     C                   eval      wrkesito = '6'
097000010615     C                   endif
097100000710     C                   endif
097200010615     C                   endif
097300010601     C                   else
097400010601     C                   eval      wrkesito = '9'
097500010601     C                   endif
097600990910     C*
097700990914     C                   if        %open(tivin00r)
097800990908     C                   close     tivin00r
097900990914     C                   endif
098000020916     C                   if        %open(fivabwwr)
098100020916     C                   close     fivabwwr
098200990914     C                   endif
098300010601     C*
098400010601     C                   if        vlrpoi <> 999
098500010601     C                   eval      invfil = vlrpoi
098600010601     C                   endif
098700990910     C*
098800990920     C                   if        §ctrok > 0
098900010601     C                             and invfil > *zeros
099000990920     C                   exsr      invio
099100990920     C                   endif
099200010612     C*
099300010612     C                   if        flgGiro = '1'
099400010612     C                   exsr      endela
099500010612     C                   endif
099600990920     C*
099700910830     C                   ENDSR
099800000613     C***
099900990920
100000010601
100100010601
100200010601
100300010601
100400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
100500010601     C     repfil        BEGSR
100600010601     C*
100700010601     C                   if        invfil = *zeros and
100800010601     C                             depfil > *zeros and
100900010613     C                             (vinflg = *blanks or
101000010613     C                              vinflg = *zeros)
101100010601     C
101200010601     C                   eval      invfil = depfil
101300010601     C                   endif
101400010601     C*
101500010601     C                   if        depfil <> invfil and
101600010601     C                             invfil > *zeros
101700010601     C                   eval      flgMulti = '1'
101800010601     C                   if        vinflg = *blanks
101900010601     C                   add       1             cntNonEl
102000010601     C                   endif
102100010601     C                   endif
102200010601     C*
102300010601     C                   if        vinflg = '2'
102400010601     C                   eval      flgStato = '2'
102500010601     C                   endif
102600010601     C*
102700010601     C                   ENDSR
102800010601     C***
102900010601
103000010601
103100010601
103200010601
103300010601
103400010601
103500010601
103600990920      /TITLE Invio dei dati al punto operativo.
103700000613     C     invio         BEGSR
103800990920     C*
103900990920     C                   reset                   dscmz
104000010601     C                   move      invfil        cmzdst
104100990920     C                   eval      cmzfld = vlrfou
104200990920     C                   eval      cmzmbd = vlrhdl
104300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
104400000710     C                   if        prmfir = *blanks
104500020916     C                   eval      cmzfla = 'FIVAB00F'
104600020916     C                   eval      cmzmba = 'FIVAB00F'
104700000710     C                   else
104800000710     C                   eval      cmzfla = prmfir
104900000710     C                   eval      cmzmba = prmfir
105000000710     C                   endif
105100990920     C                   eval      cmznrr = *zeros
105200990920     C                   move      §ctrok        cmznrr
105300021003     C                   eval      cmzlba = vlrfl1
105400990920     C                   call(e)   'TIS711C'
105500990920     C                   parm                    dscmz
105600990921     C                   parm      *blanks       esito
105700990920     C                   if        %error
105800990920     C                             or cmzerr = '1'
105900990921     C                             or esito  = '1'
106000000710     C                   eval      wrkesito = '3'
106100990920     C                   endif
106200990920     C*
106300000613     C                   ENDSR
106400010601
106500010601
106600010601
106700010601
106800010601      /TITLE Invio dei dati al punto operativo.
106900010601     C     opeini        BEGSR
107000010601     C*
107100010601     C* Inizializzo flag e contatori operativi
107200010601     C                   movel     '0'           flgGiro           1
107300010601     C                   movel     '0'           flgMulti          1
107400010601     C                   movel     '1'           flgStato          1
107500010615     C                   movel     '0'           flgOk             1
107600010601     C                   z-add     *zeros        cntNonEl         10 0
107700010601     C                   z-add     *zeros        depfil            3 0
107800010601     C                   z-add     *zeros        invfil            3 0
107900030407     C*
108000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
108100030407     C                   if        %subst(vlrppt:1:1) = 'S'
108200030407     C                   seton                                        50
108300030407     C                   else
108400030407     C                   setoff                                       50
108500030407     C                   endif
108600041001     C*
108700041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
108800041001     C                   if        %subst(vlrppt:2:1) = 'S'
108900041001     C                   seton                                        51
109000041001     C                   else
109100041001     C                   setoff                                       51
109200041001     C                   endif
109300041025     C*
109400041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
109500041025     C* (naturalmente solo se nn già presente)
109600041025     C                   if        %subst(vlrppt:3:1) = 'S'
109700041025     C                   seton                                        52
109800041025     C                   else
109900041025     C                   setoff                                       52
110000041025     C                   endif
110100010601     C*
110200010601     C                   ENDSR
110300010601     C***
110400040630
110500040630
110600040630
110700040630
110800040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
110900040630     C     exeBon        BEGSR
111000040630     C*
111100040630     C* Imposto i parametri x il *pgm d "bonifica"
111200040630     C                   EVAL      ElencoChar = *blanks
111300040630     C                   EVAL      TipoElenco = *blanks
111400040630     C                   EVAL      CharSost   = *blanks
111500040630     C                   EVAL      UpperCase  = '1'
111600040630     C                   EVAL      ChkNull    = '1'
111700040630     C                   EVAL      CharNull   = *blanks
111800040630     C*
111900040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
112000040630     C                   EVAL      TxtInOut   = VABRSD
112100040630     C                   EVAL      EsitoBon   = *blanks
112200040630     C*
112300040630     C                   CALL      'XCHKCHAR'
112400040630     C                   PARM                    TxtInOut
112500040630     C                   PARM                    ElencoChar
112600040630     C                   PARM                    TipoElenco
112700040630     C                   PARM                    CharSost
112800040630     C                   PARM                    UpperCase
112900040630     C                   PARM                    ChkNull
113000040630     C                   PARM                    CharNull
113100040630     C                   PARM                    EsitoBon
113200040630     C*
113300040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
113400040630     C                   IF        EsitoBon = '1'
113500040630     C                   EVAL      VABRSD = TxtInOut
113600040630     C                   ENDIF
113700040630     C*
113800040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
113900040630     C                   EVAL      TxtInOut   = VABIND
114000040630     C                   EVAL      EsitoBon   = *blanks
114100040630     C*
114200040630     C                   CALL      'XCHKCHAR'
114300040630     C                   PARM                    TxtInOut
114400040630     C                   PARM                    ElencoChar
114500040630     C                   PARM                    TipoElenco
114600040630     C                   PARM                    CharSost
114700040630     C                   PARM                    UpperCase
114800040630     C                   PARM                    ChkNull
114900040630     C                   PARM                    CharNull
115000040630     C                   PARM                    EsitoBon
115100040630     C*
115200040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
115300040630     C                   IF        EsitoBon = '1'
115400040630     C                   EVAL      VABIND = TxtInOut
115500040630     C                   ENDIF
115600040630     C*
115700040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
115800040630     C                   EVAL      TxtInOut   = VABLOD
115900040630     C                   EVAL      EsitoBon   = *blanks
116000040630     C*
116100040630     C                   CALL      'XCHKCHAR'
116200040630     C                   PARM                    TxtInOut
116300040630     C                   PARM                    ElencoChar
116400040630     C                   PARM                    TipoElenco
116500040630     C                   PARM                    CharSost
116600040630     C                   PARM                    UpperCase
116700040630     C                   PARM                    ChkNull
116800040630     C                   PARM                    CharNull
116900040630     C                   PARM                    EsitoBon
117000040630     C*
117100040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
117200040630     C                   IF        EsitoBon = '1'
117300040630     C                   EVAL      VABLOD = TxtInOut
117400040630     C                   ENDIF
117500040630     C*
117600040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
117700040630     C                   EVAL      TxtInOut   = VABNOT
117800040630     C                   EVAL      EsitoBon   = *blanks
117900040630     C                   EVAL      UpperCase  = *blanks
118000040630     C*
118100040630     C                   CALL      'XCHKCHAR'
118200040630     C                   PARM                    TxtInOut
118300040630     C                   PARM                    ElencoChar
118400040630     C                   PARM                    TipoElenco
118500040630     C                   PARM                    CharSost
118600040630     C                   PARM                    UpperCase
118700040630     C                   PARM                    ChkNull
118800040630     C                   PARM                    CharNull
118900040630     C                   PARM                    EsitoBon
119000040630     C*
119100040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
119200040630     C                   IF        EsitoBon = '1'
119300040630     C                   EVAL      VABNOT = TxtInOut
119400040630     C                   ENDIF
119500040630     C*
119600040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
119700040630     C                   EVAL      TxtInOut   = VABNT2
119800040630     C                   EVAL      EsitoBon   = *blanks
119900040630     C                   EVAL      UpperCase  = *blanks
120000040630     C*
120100040630     C                   CALL      'XCHKCHAR'
120200040630     C                   PARM                    TxtInOut
120300040630     C                   PARM                    ElencoChar
120400040630     C                   PARM                    TipoElenco
120500040630     C                   PARM                    CharSost
120600040630     C                   PARM                    UpperCase
120700040630     C                   PARM                    ChkNull
120800040630     C                   PARM                    CharNull
120900040630     C                   PARM                    EsitoBon
121000040630     C*
121100040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
121200040630     C                   IF        EsitoBon = '1'
121300040630     C                   EVAL      VABNT2 = TxtInOut
121400040630     C                   ENDIF
121500040630     C*
121600040630     C                   ENDSR
121700090511
121800090511
121900090511
122000090511     C*--------------------------------------------------------
122100090511     C* CARTAB - CARICAMENTO TABELLE                          *
122200090511     C*--------------------------------------------------------
122300090511     C     CARTAB        BEGSR
122400090511     C*
122500090511     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
122600090511     C                   z-add     *zeros        jOSR              3 0
122700090511     C     'OSR'         setll     tntbe01l
122800090511     C                   if        %equal(tntbe01l)
122900090511     C     'OSR'         reade     tntbe01l
123000090511     C                   dow       not %eof(tntbe01l)
123100090511     C                   eval      DOSR = tbeUNI
123200090511     c*
123300090511     c* Verifica e forzatura x nuovi campi aggiunti
123400090511     c                   if        aD§OSRcl6 = *blanks
123500090511     c                   eval      D§OSRcl6 = *zeros
123600090511     c                   endif
123700090511     c                   if        aD§OSRcl7 = *blanks
123800090511     c                   eval      D§OSRcl7 = *zeros
123900090511     c                   endif
124000090511     c                   if        aD§OSRcl8 = *blanks
124100090511     c                   eval      D§OSRcl8 = *zeros
124200090511     c                   endif
124300090511     c                   if        aD§OSRcl9 = *blanks
124400090511     c                   eval      D§OSRcl9 = *zeros
124500090511     c                   endif
124600090511     c                   if        aD§OSRcl10 = *blanks
124700090511     c                   eval      D§OSRcl10 = *zeros
124800090511     c                   endif
124900090511     c                   if        aD§OSRcl11 = *blanks
125000090511     c                   eval      D§OSRcl11 = *zeros
125100090511     c                   endif
125200090511     c                   if        aD§OSRcl12 = *blanks
125300090511     c                   eval      D§OSRcl12 = *zeros
125400090511     c                   endif
125500090511     c                   if        aD§OSRcl13 = *blanks
125600090511     c                   eval      D§OSRcl13 = *zeros
125700090511     c                   endif
125800090511     c                   if        aD§OSRcl14 = *blanks
125900090511     c                   eval      D§OSRcl14 = *zeros
126000090511     c                   endif
126100090511     c                   if        aD§OSRcl15 = *blanks
126200090511     c                   eval      D§OSRcl15 = *zeros
126300090511     c                   endif
126400090511     c                   if        aD§OSRcl16 = *blanks
126500090511     c                   eval      D§OSRcl16 = *zeros
126600090511     c                   endif
126700090511     c*
126800090511     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
126900090511     C                   if        D§OSRCLI <> *zeros
127000090511     C                   add       1             jOSR
127100090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
127200090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
127300090511     C                   movel     D§OSRCLI      wCliOSR
127400090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
127500090511     C                   endif
127600090511     C*
127700090511     C                   if        D§OSRCL2 <> *zeros
127800090511     C                   add       1             jOSR
127900090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
128000090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
128100090511     C                   movel     D§OSRCL2      wCliOSR
128200090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
128300090511     C                   endif
128400090511     C*
128500090511     C                   if        D§OSRCL3 <> *zeros
128600090511     C                   add       1             jOSR
128700090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
128800090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
128900090511     C                   movel     D§OSRCL3      wCliOSR
129000090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
129100090511     C                   endif
129200090511     C*
129300090511     C                   if        D§OSRCL4 <> *zeros
129400090511     C                   add       1             jOSR
129500090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
129600090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
129700090511     C                   movel     D§OSRCL4      wCliOSR
129800090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
129900090511     C                   endif
130000090511     C*
130100090511     C                   if        D§OSRCL5 <> *zeros
130200090511     C                   add       1             jOSR
130300090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
130400090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
130500090511     C                   movel     D§OSRCL5      wCliOSR
130600090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
130700090511     C                   endif
130800090511     C*
130900090511     C                   if        D§OSRCL6 <> *zeros
131000090511     C                   add       1             jOSR
131100090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
131200090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
131300090511     C                   movel     D§OSRCL6      wCliOSR
131400090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
131500090511     C                   endif
131600090511     C*
131700090511     C                   if        D§OSRCL7 <> *zeros
131800090511     C                   add       1             jOSR
131900090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
132000090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
132100090511     C                   movel     D§OSRCL7      wCliOSR
132200090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
132300090511     C                   endif
132400090511     C*
132500090511     C                   if        D§OSRCL8 <> *zeros
132600090511     C                   add       1             jOSR
132700090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
132800090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
132900090511     C                   movel     D§OSRCL8      wCliOSR
133000090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
133100090511     C                   endif
133200090511     C*
133300090511     C                   if        D§OSRCL9 <> *zeros
133400090511     C                   add       1             jOSR
133500090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
133600090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
133700090511     C                   movel     D§OSRCL9      wCliOSR
133800090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
133900090511     C                   endif
134000090511     C*
134100090511     C                   if        D§OSRCL10 <> *zeros
134200090511     C                   add       1             jOSR
134300090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
134400090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
134500090511     C                   movel     D§OSRCL10     wCliOSR
134600090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
134700090511     C                   endif
134800090511     C*
134900090511     C                   if        D§OSRCL11 <> *zeros
135000090511     C                   add       1             jOSR
135100090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
135200090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
135300090511     C                   movel     D§OSRCL11     wCliOSR
135400090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
135500090511     C                   endif
135600090511     C*
135700090511     C                   if        D§OSRCL12 <> *zeros
135800090511     C                   add       1             jOSR
135900090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
136000090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
136100090511     C                   movel     D§OSRCL12     wCliOSR
136200090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
136300090511     C                   endif
136400090511     C*
136500090511     C                   if        D§OSRCL13 <> *zeros
136600090511     C                   add       1             jOSR
136700090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
136800090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
136900090511     C                   movel     D§OSRCL13     wCliOSR
137000090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
137100090511     C                   endif
137200090511     C*
137300090511     C                   if        D§OSRCL14 <> *zeros
137400090511     C                   add       1             jOSR
137500090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
137600090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
137700090511     C                   movel     D§OSRCL14     wCliOSR
137800090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
137900090511     C                   endif
138000090511     C*
138100090511     C                   if        D§OSRCL15 <> *zeros
138200090511     C                   add       1             jOSR
138300090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
138400090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
138500090511     C                   movel     D§OSRCL15     wCliOSR
138600090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
138700090511     C                   endif
138800090511     C*
138900090511     C                   if        D§OSRCL16 <> *zeros
139000090511     C                   add       1             jOSR
139100090511     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
139200090511     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
139300090511     C                   movel     D§OSRCL16     wCliOSR
139400090511     C                   eval      SkCliOSR(jOSR) = DsCliOSR
139500090511     C                   endif
139600090511     C*
139700090511     C     'OSR'         reade     tntbe01l
139800090511     C                   enddo
139900090511     C                   endif
140000090511     C*
140100090511     C                   ENDSR
140200090511     C***
140300010601
140400010601
140500990910
140600000613     C     *inzsr        BEGSR
140700990910     C*
140800990910     C     *entry        plist
140900990920     C                   parm                    tivlrds
141000990921     C                   parm      wrkesito      esito
141100000724     C                   parm                    prmlit
141200000710     C                   parm                    prmfir
141300010606     C*
141400010606     C* CALCOLA LA DATA CORRENTE
141500010606     C                   time                    wn14             14 0
141600010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
141700010606     C                   z-add     wn8           g08dat
141800010606     C                   z-add     *zeros        g08inv
141900010606     C                   movel     '0'           g08err
142000010606     C                   call      'XSRDA8'
142100010606     C                   parm                    wlbda8
142200010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
142300000613     C*
142400000613     C                   ENDSR
142500000613     C***
142600990908
142700000613     Otis7t4p   E            riepilogo         2
142800990915     O                                              'Upload via Internet'
142900990915     O                                           +1 'Traduzione TIVIN00R -'
143000020916     O                                           +1 'FIVABWWR'
143100990915     O                                           +1 'Testate bolle'
143200990915     O          E            riepilogo   2
143300990915     O                       wrkdata
143400990915     O                       wrkora              +1
143500990915     O                       procname            +1
143600990915     O          E            riepilogo   2
143700990915     O                                              'Cliente..................:'
143800990915     O                       VABCCM        z     +1
143900990915     O          E            riepilogo   2
144000990920     O                                              'Riferimento Strategi.....:'
144100990920     O                       vlrhdl              +1
144200990915     O          E            riepilogo   2
144300990915     O                                              'Giusti...................:'
144400971022     O                       §CTROK        2   +  1
144500990915     O          E            riepilogo   2
144600990915     O                                              'Sbagliati e corretti.....:'
144700971022     O                       §CTRMO        2   +  1
144800990915     O          E            riepilogo   2
144900990915     O                                              'Sbagliati e scartati.....:'
145000971022     O                       §CTRNO        2   +  1
145100000613
145200000616     Otis7t4ps  E            testdett          2
145300000613     O                                              'Upload via Internet'
145400000613     O                                           +1 'Traduzione TIVIN00R -'
145500020916     O                                           +1 'FIVABWWR'
145600000613     O                                           +1 'Testate bolle'
145700000616     O          E            testdett    3
145800000613     O                                           +2 'N° rec'
145900000613     O                                           +3 'Anteprima contenuto'
146000000616     O          E            rigadett    2
146100000613     O                       rrnum               +2
146200000621     O                       recko               +3
146300000616     O          E            findett     2
146400000613     O                       wrkdata
146500000613     O                       wrkora              +1
146600000613     O                       procname            +1
146700000616     O          E            findett     2
146800000613     O                                              'Cliente..................:'
146900000613     O                       VABCCM        z     +1
147000000616     O          E            findett     2
147100000613     O                                              'Riferimento Strategi.....:'
147200000613     O                       vlrhdl              +1
147300000616     O          E            findett     2
147400000613     O                                              'Giusti...................:'
147500000613     O                       §CTROK        2   +  1
147600000616     O          E            findett     2
147700000613     O                                              'Sbagliati e corretti.....:'
147800000613     O                       §CTRMO        2   +  1
147900000616     O          E            findett     2
148000000613     O                                              'Sbagliati e scartati.....:'
148100000613     O                       §CTRNO        2   +  1
148200000613** CMD - COMANDI CL
148300011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4P)  FORMTYPE(RICCLI) OUTQ(
148400011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4PS) FORMTYPE(RICCLI) OUTQ(
148500000616DLTOVR FILE(TIS7T4P TIS7T4PS) LVL(*)
148600000613
148700000613
