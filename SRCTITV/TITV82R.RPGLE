000100020916      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400040227     Ftntbe01l  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700020916     FFIVABwwr  O    E             DISK    usropn
000800031229     Ftitv82p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000031229     Ftitv82ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000724     D*------------
001800030811     D* DS ESTERNE
001900030811     D*------------
002000040108     D kpjba         e ds                  inz
002100040108     D fivabds       e ds                  extname(fivab00f)
002200040227     D tivlrds       e ds                  extname(tivlr00f)
002300041025     D tisi95ds      e ds
002400030811     D dscmz         e ds                  inz
002500040227     D dosr          e ds                  inz
002600080502     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
002700080502     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
002800080502     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
002900080502     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
003000080502     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
003100080502     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
003200080502     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
003300080502     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
003400080502     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
003500080502     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
003600080502     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
003700990910     D psds           sds
003800990910     D  procname         *PROC
003900030811     D*------------
004000030811     D* DS INTERNE
004100030811     D*------------
004200941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
004300941229     D* 10+3
004400941229     D                 DS
004500000313     D  WI15                   1     10
004600000313     D  WD15                  11     13
004700000313     D  WASC15                 1     13
004800941229     D*  6+1
004900941229     D                 DS
005000000313     D  WI09                   1      6
005100000313     D  WD09                   7      7
005200000313     D  WASC09                 1      7
005300941229     D*  2+3
005400941229     D                 DS
005500000313     D  WI07                   1      2
005600000313     D  WD07                   3      5
005700000313     D  WASC07                 1      5
005800000313     D*---------------------------------------------------------
005900000313     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
006000000313     D*---------------------------------------------------------
006100000313     D                 ds
006200000313     D vhbatb                  1      1
006300000313     D vhbccm                  2      8
006400000313     D vhblnp                  9     11
006500000313     D vhbaas                 12     15
006600000313     D vhbmgs                 16     19
006700000313     D vhbnrs                 20     21
006800000313     D vhbnsp                 22     28
006900000313     D vhbcbo                 29     30
007000000313     D vhblna                 31     33
007100000313     D vhbrsd                 34     68
007200000313     D vhbrd2                 69    103
007300000313     D vhbind                104    138
007400000313     D vhbcad                139    147
007500000313     D vhblod                148    182
007600000313     D vhbprd                183    184
007700000313     D vhbnzd                185    187
007800000313     D vhbgc1                188    189
007900000313     D vhbgc2                190    191
008000000313     D vhbctr                192    194
008100000313     D vhbtsp                195    195
008200000313     D vhbias                196    208
008300000313     D vhbvas                209    211
008400000313     D vhbnas                212    226
008500000313     D vhbncl                227    231
008600000313     D vhbpkb                232    238
008700000313     D vhbvlb                239    243
008800000313     D vhbqft                244    256
008900000313     D vhbcas                257    269
009000000313     D vhbtic                270    271
009100000313     D vhbvca                272    274
009200000313     D vhbgca                275    276
009300000313     D vhbrmn                277    291
009400000313     D vhbrma                292    306
009500000313     D vhbncd                307    313
009600000313     D vhbnca                314    320
009700000313     D vhbxco                321    321
009800000313     D vhbnot                322    356
009900000313     D vhbnt2                357    391
010000000313     D vhbznc                392    393
010100000313     D vhbctm                394    395
010200000313     D vhbffd                396    396
010300000313     D vhbdcr                397    404
010400000313     D vhbtcr                405    405
010500000313     D vhbhcr                406    409
010600000313     D vhbcts                410    411
010700000313     D vhbftm                412    412
010800000313     D vhbvmd                413    425
010900000313     D vhbvad                426    428
011000000313     D vhbgma                429    430
011100000313     D vhbgga                431    432
011200000313     D vhbgva                433    434
011300000313     D vhbtc1                435    435
011400000313     D vhbtc2                436    436
011500000313     D vhbscl                437    437
011600000313     D vhbant                438    446
011700000313     D vhbrmo                447    471
011800000313     D vhbcmo                472    480
011900000313     D vhbnmo                481    483
012000000313     D  X_rec                  1    483    inz
012100000313     D*
012200990910     D esito           s              1
012300000724     D prmlit          s             10
012400000710     D prmfir          s             10
012500990921     D wrkesito        s                   like(esito)
012600990915     D wrkdata         s               d
012700990915     D wrkora          s               t
012800000613     D rrnum           s              6  0 INZ(*zeros)
012900000621     D recko           s            150    INZ(*blanks)
013000011123     D depcmd          s            150    INZ(*blanks)
013100031229     D wCTM            s                   LIKE(vabCTM)
013200040227     D wNSR            s              2    INZ
013300040227     D wCRC            s             10    INZ
013400040227     D wKSC            s              7    INZ
013500040227     D wCTR            s              3    INZ
013600040108     D wfivabds        s                   LIKE(fivabds)
013700010606     D*------------------
013800010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
013900010606     D*------------------
014000010606     D WLBDA8          DS                  INZ
014100010606     D  G08DAT                 1      8  0
014200010606     D  G08INV                 9     16  0
014300010606     D  G08ERR                17     17
014400010606     D  G08TGI                18     22  0
014500040227     D*------------------
014600040227     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
014700040227     D*------------------
014800080502     D SkCliOSR        s             10    DIM(900)
014900080502     D*
015000080502     D DsCliOSR        ds
015100080502     D  wLnpOSR                       3
015200080502     D  wCliOSR                       7
015300080502     D*
015400080502     D SkLnpNrsOSR     s              5    DIM(900)
015500010606
015600010606
015700010606
015800010606
015900990915     C                   time                    wrkdata
016000990915     C                   time                    wrkora
016100000913     C                   reset                   rrnum
016200990921     C                   reset                   esito
016300990921     C                   reset                   wrkesito
016400010601     C*
016500010601     C                   exsr      opeini
016600010601     C                   exsr      rwvab
016700041025     C*
016800041025     C* Esegue lancio TISI95R solo x chiusura
016900041025     C                   CLEAR                   TISI95DS
017000041025     C                   EVAL      I95TLA = 'C'
017100041025     C                   CALL      'TISI95R'
017200041025     C                   PARM                    TISI95DS
017300010601     C*
017400010601     C                   seton                                        lr
017500010601
017600010601
017700010601
017800010601
017900010601     C*--------------------------------------------------------
018000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
018100010601     C*--------------------------------------------------------
018200010601     C     PREELA        BEGSR
018300010601     C*
018400010601     C* SE OCCORRE SPEDIRE IN FILIALE
018500010601     C                   if        invfil <> *zeros and
018600010601     C                             flgGiro = '0'
018700010601     C*
018800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
018900010601     C                   eval      flgGiro = '1'
019000000724     C*
019100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
019200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
019300010601     C     invfil        chain     azorg01l
019400000724     C                   if        %found
019500000724     C                   movel(p)  CMD(1)        depcmd
019600020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
019700000724     C*
019800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
019900011123     C                   Z-ADD     150           LENGH            15 5
020000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020100000724     C                   PARM                    depcmd
020200000724     C                   PARM                    LENGH
020300000724     C*
020400000724     C                   endif
020500000616     C*
020600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
020700000616     C                   movel(p)  CMD(2)        depcmd
020800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
020900000616     C*
021000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
021100011123     C                   Z-ADD     150           LENGH            15 5
021200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
021300000616     C                   PARM                    depcmd
021400000616     C                   PARM                    LENGH
021500000613     C*
021600031229     C                   if        not %open(titv82ps)
021700031229     C                   open      titv82ps
021800030407     C   50              except    testdett
021900000613     C                   endif
022000010601     C*
022100010601     C                   endif
022200010601     C*
022300010601     C                   ENDSR
022400010601     C***
022500010601
022600010601
022700010601
022800010601     C*--------------------------------------------------------
022900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
023000010601     C*--------------------------------------------------------
023100010601     C     ENDELA        BEGSR
023200990915     C*
023300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
023400000613     C*
023500031229     C                   if        %open(titv82ps)
023600030407     C   50              except    findett
023700031229     C                   close     titv82ps
023800000613     C                   endif
023900000616     C*
024000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
024100011123     C                   Z-ADD     150           LENGH            15 5
024200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
024300000616     C                   PARM                    CMD(3)
024400000616     C                   PARM                    LENGH
024500040108     C*
024600040108     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
024700040923     C   54              call(e)   'TITV84R'
024800040108     C                   parm      'C'           InTipl
024900040108     C                   parm                    tivlrds
025000040108     C                   parm                    wfivabds
025100010601     C*
025200010601     C                   ENDSR
025300010601     C***
025400010601
025500010601
025600000613
025700000613
025800000613
025900000613     C*--------------------------------------------------------
026000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
026100000613     C*--------------------------------------------------------
026200000621     C     STPR          BEGSR
026300000613     C*
026400031229     C                   if        not %open(titv82p)
026500031229     C                   open      titv82p
026600990915     C                   endif
026700990915     C*
026800030407     C   50              except    riepilogo
026900990915     C*
027000031229     C                   if        %open(titv82p)
027100031229     C                   close     titv82p
027200990914     C                   endif
027300000613     C*
027400000613     C                   ENDSR
027500000613     C***
027600000613
027700010601
027800010601
027900010601
028000010601
028100910830     C*--------------------------------------------------------
028200020916     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
028300910830     C*--------------------------------------------------------
028400941229     C     RWVAB         BEGSR
028500990910     C*
028600990914     C                   if        not %open(tivin00r)
028700990908     C                   open      tivin00r
028800990914     C                   endif
028900020916     C                   if        not %open(fivabwwr)
029000020916     C                   open      fivabwwr
029100990914     C                   endif
029200990910     C*
029300990910     C                   clear                   §CTROK
029400990910     C                   clear                   §CTRMO
029500990910     C                   clear                   §CTRNO
029600990910     C*
029700921023     C                   DO        *HIVAL
029800990913     C*
029900990915     C                   READ      tivin00r                               70
030000010618     C*
030100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
030200010618     C                   if        vinflg = '1'
030300010618     C                   eval      flgOk = '1'
030400010618     C                   endif
030500010618     C*
030600000905     C                   if        vindta > *blanks
030700000613     C                   add       1             rrnum
030800990913     C*
030900010601     C                   if        *in70 = *off and
031000990913     C                             (vinflg = *blanks
031100010711     C                              or vinflg = '0'
031200010711     C                              or vinflg = '2')
031300990913     C*
031400020916     C                   clear                   FIVAB000
031500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
031600010711     C                   if        vinflg = *blanks or vinflg = '0'
031700010711     C                   clear                   vinmsg
031800010711     C                   endif
031900010711     C*
032000000313     C                   eval      x_rec  = vindta
032100991022     C*
032200991022     C                   testb     x'3F'         vhbatb                   02
032300991022     C                   if        not *in02
032400991022     C*
032500971021     C                   SETOFF                                       303132
032600971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
032700971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
032800971023     C*    OPPURE TUTTI BLANKS
032900010601     C*
033000010601     C                   TESTN                   VHBLNP               30
033100010601     C  N30              TESTN                   VHBLNP                 30
033200010601     C  N30              TESTN                   VHBLNP                   30
033300010601     C                   MOVE      VHBLNP        W001A             1
033400010601     C     *IN30         IFEQ      *OFF
033500010601     C     W001A         ORLT      '0'
033600010601     C                   SETON                                        31
033700010601     C                   eval      vinmsg = %trimr(vinmsg)
033800010601     C                             + ' ' + 'VHBLNP'
033900010601     C                   ELSE
034000010601     C                   MOVE      VHBLNP        VABLNP
034100010601     C     VABLNP        IFLE      *ZEROS
034200010601     C                   SETON                                        31
034300010601     C                   eval      vinmsg = %trimr(vinmsg)
034400010601     C                             + ' ' + 'VHBLNP'
034500010601     C                   ENDIF
034600010601     C                   ENDIF
034700010601     C*
034800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
034900010601     C                   eval      depfil = VABLNP
035000010601     C                   exsr      repfil
035100010601     C                   if        depfil = invfil
035200021014     C                   if        vlrpoi = 999
035300021010     C                   MOVE(P)   invfil        VABFGS
035400021014     C                   else
035500021014     C                   MOVE(P)   vlrpoi        VABFGS
035600021014     C                   endif
035700010601     C*
035800010601     C                   exsr      PREELA
035900010601     C*
036000971023     C                   TESTN                   VHBCCM               30
036100971023     C  N30              TESTN                   VHBCCM                 30
036200971023     C  N30              TESTN                   VHBCCM                   30
036300971021     C                   MOVE      VHBCCM        W001A             1
036400971021     C     *IN30         IFEQ      *OFF
036500971021     C     W001A         ORLT      '0'
036600971021     C                   SETON                                        31
036700010604     C                   eval      vinmsg = %trimr(vinmsg)
036800010604     C                             + ' ' + 'VHBCCM'
036900971021     C                   ELSE
037000971021     C                   MOVE      VHBCCM        VABCCM
037100971024     C     VABCCM        IFLE      *ZEROS
037200971024     C                   SETON                                        31
037300010604     C                   eval      vinmsg = %trimr(vinmsg)
037400010604     C                             + ' ' + 'VHBCCM'
037500971024     C                   ENDIF
037600971021     C                   ENDIF
037700971021     C*
037800971023     C                   TESTN                   VHBAAS               30
037900971023     C  N30              TESTN                   VHBAAS                 30
038000971023     C  N30              TESTN                   VHBAAS                   30
038100971021     C                   MOVE      VHBAAS        W001A             1
038200971021     C     *IN30         IFEQ      *OFF
038300971021     C     W001A         ORLT      '0'
038400971023     C     W001A         ANDNE     *BLANK
038500971021     C                   SETON                                        32
038600010606     C                   MOVEL     datcor        VABAAS
038700010604     C                   eval      vinmsg = %trimr(vinmsg)
038800010604     C                             + ' ' + 'VHBAAS'
038900971021     C                   ELSE
039000971021     C                   MOVE      VHBAAS        VABAAS
039100971021     C                   ENDIF
039200971021     C*
039300971023     C                   TESTN                   VHBMGS               30
039400971023     C  N30              TESTN                   VHBMGS                 30
039500971023     C  N30              TESTN                   VHBMGS                   30
039600971021     C                   MOVE      VHBMGS        W001A             1
039700971021     C     *IN30         IFEQ      *OFF
039800971021     C     W001A         ORLT      '0'
039900971023     C     W001A         ANDNE     *BLANK
040000971021     C                   SETON                                        32
040100010601     C                   MOVE      '0101'        VABMGS
040200010604     C                   eval      vinmsg = %trimr(vinmsg)
040300010604     C                             + ' ' + 'VHBMGS'
040400971021     C                   ELSE
040500971021     C                   MOVE      VHBMGS        VABMGS
040600971021     C                   ENDIF
040700971021     C*
040800971023     C                   TESTN                   VHBNRS               30
040900971023     C  N30              TESTN                   VHBNRS                 30
041000971023     C  N30              TESTN                   VHBNRS                   30
041100971021     C                   MOVE      VHBNRS        W001A             1
041200971021     C     *IN30         IFEQ      *OFF
041300971021     C     W001A         ORLT      '0'
041400971023     C     W001A         ANDNE     *BLANK
041500971021     C                   SETON                                        32
041600971021     C                   MOVE      *ZERO         VABNRS
041700010604     C                   eval      vinmsg = %trimr(vinmsg)
041800010604     C                             + ' ' + 'VHBNRS'
041900971021     C                   ELSE
042000971021     C                   MOVE      VHBNRS        VABNRS
042100971021     C                   ENDIF
042200971021     C*
042300971023     C                   TESTN                   VHBNSP               30
042400971023     C  N30              TESTN                   VHBNSP                 30
042500971023     C  N30              TESTN                   VHBNSP                   30
042600971021     C                   MOVE      VHBNSP        W001A             1
042700971021     C     *IN30         IFEQ      *OFF
042800971021     C     W001A         ORLT      '0'
042900971023     C     W001A         ANDNE     *BLANK
043000971021     C                   SETON                                        32
043100971021     C                   MOVE      *ZERO         VABNSP
043200010604     C                   eval      vinmsg = %trimr(vinmsg)
043300010604     C                             + ' ' + 'VHBNSP'
043400971021     C                   ELSE
043500971021     C                   MOVE      VHBNSP        VABNSP
043600971021     C                   ENDIF
043700971021     C*
043800971023     C                   TESTN                   VHBLNA               30
043900971023     C  N30              TESTN                   VHBLNA                 30
044000971023     C  N30              TESTN                   VHBLNA                   30
044100971021     C                   MOVE      VHBLNA        W001A             1
044200971021     C     *IN30         IFEQ      *OFF
044300971021     C     W001A         ORLT      '0'
044400971023     C     W001A         ANDNE     *BLANK
044500971021     C                   SETON                                        32
044600971021     C                   MOVE      *ZERO         VABLNA
044700010604     C                   eval      vinmsg = %trimr(vinmsg)
044800010604     C                             + ' ' + 'VHBLNA'
044900971021     C                   ELSE
045000971021     C                   MOVE      VHBLNA        VABLNA
045100971021     C                   ENDIF
045200971021     C*
045300971023     C                   TESTN                   VHBCTR               30
045400971023     C  N30              TESTN                   VHBCTR                 30
045500971023     C  N30              TESTN                   VHBCTR                   30
045600971021     C                   MOVE      VHBCTR        W001A             1
045700971021     C     *IN30         IFEQ      *OFF
045800971021     C     W001A         ORLT      '0'
045900971023     C     W001A         ANDNE     *BLANK
046000971021     C                   SETON                                        32
046100971021     C                   MOVE      *ZERO         VABCTR
046200010604     C                   eval      vinmsg = %trimr(vinmsg)
046300010604     C                             + ' ' + 'VHBCTR'
046400971021     C                   ELSE
046500971021     C                   MOVE      VHBCTR        VABCTR
046600971021     C                   ENDIF
046700941229     C*
046800971021     C                   MOVE      *ZERO         VABIAS
046900941229     C                   MOVE      VHBIAS        WASC15
047000971023     C                   TESTN                   WI15                 30
047100971023     C  N30              TESTN                   WI15                   30
047200971023     C  N30              TESTN                   WI15                     30
047300971021     C                   MOVE      WI15          W001A             1
047400971021     C     *IN30         IFEQ      *OFF
047500971021     C     W001A         ORLT      '0'
047600971023     C     W001A         ANDNE     *BLANK
047700971021     C                   SETON                                        32
047800971021     C                   MOVE      *ZERO         VABIAS
047900010604     C                   eval      vinmsg = %trimr(vinmsg)
048000010604     C                             + ' ' + 'VHBIAS'
048100971021     C                   ELSE
048200971021     C                   MOVEL     WI15          VABIAS
048300971023     C                   TESTN                   WD15                 30
048400971023     C  N30              TESTN                   WD15                   30
048500971023     C  N30              TESTN                   WD15                     30
048600971021     C                   MOVE      WD15          W001A             1
048700971021     C     *IN30         IFEQ      *OFF
048800971021     C     W001A         ORLT      '0'
048900971023     C     W001A         ANDNE     *BLANK
049000971021     C                   SETON                                        32
049100971021     C                   MOVE      *ZERO         VABIAS
049200010604     C                   eval      vinmsg = %trimr(vinmsg)
049300010604     C                             + ' ' + 'VHBIAS'
049400971021     C                   ELSE
049500971021     C                   MOVE      WD15          VABIAS
049600971021     C                   ENDIF
049700050118     C                   ENDIF
049800941229     C*
049900971023     C                   TESTN                   VHBNCL               30
050000971023     C  N30              TESTN                   VHBNCL                 30
050100971023     C  N30              TESTN                   VHBNCL                   30
050200971021     C                   MOVE      VHBNCL        W001A             1
050300971021     C     *IN30         IFEQ      *OFF
050400971021     C     W001A         ORLT      '0'
050500971021     C                   SETON                                        32
050600971021     C                   MOVE      *ZERO         VABNCL
050700010604     C                   eval      vinmsg = %trimr(vinmsg)
050800010604     C                             + ' ' + 'VHBNCL'
050900971021     C                   ELSE
051000971021     C                   MOVE      VHBNCL        VABNCL
051100971021     C                   ENDIF
051200941229     C*
051300971021     C                   MOVE      *ZERO         VABPKB
051400971021     C                   MOVE      VHBPKB        WASC09
051500050503     C                   IF        WI09 = *blanks
051600050503     C                   EVAL      WI09 = *zeros
051700050503     C                   ENDIF
051800971023     C                   TESTN                   WI09                 30
051900971023     C  N30              TESTN                   WI09                   30
052000971023     C  N30              TESTN                   WI09                     30
052100971021     C                   MOVE      WI09          W001A             1
052200971021     C     *IN30         IFEQ      *OFF
052300971021     C     W001A         ORLT      '0'
052400971021     C                   SETON                                        32
052500971021     C                   MOVE      *ZERO         VABPKB
052600010604     C                   eval      vinmsg = %trimr(vinmsg)
052700010604     C                             + ' ' + 'VHBPKB'
052800971021     C                   ELSE
052900971021     C                   MOVEL     WI09          VABPKB
053000971023     C                   TESTN                   WD09                 30
053100971023     C  N30              TESTN                   WD09                     30
053200971021     C                   MOVE      WD09          W001A             1
053300971021     C     *IN30         IFEQ      *OFF
053400971021     C     W001A         ORLT      '0'
053500971023     C     W001A         ANDNE     *BLANK
053600971021     C                   SETON                                        32
053700971021     C                   MOVE      *ZERO         VABPKB
053800010604     C                   eval      vinmsg = %trimr(vinmsg)
053900010604     C                             + ' ' + 'VHBPKB'
054000971021     C                   ELSE
054100971021     C                   MOVE      WD09          VABPKB
054200971021     C                   ENDIF
054300050118     C                   ENDIF
054400971021     C*
054500041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
054600041001     C                   IF        *IN51 = *OFF
054700971021     C                   MOVE      *ZERO         VABVLB
054800971021     C                   MOVE      VHBVLB        WASC07
054900050503     C                   IF        WI07 = *blanks
055000050503     C                   EVAL      WI07 = *zeros
055100050503     C                   ENDIF
055200971023     C                   TESTN                   WI07                 30
055300971023     C  N30              TESTN                   WI07                   30
055400971023     C  N30              TESTN                   WI07                     30
055500971021     C                   MOVE      WI07          W001A             1
055600971021     C     *IN30         IFEQ      *OFF
055700971021     C     W001A         ORLT      '0'
055800971023     C     W001A         ANDNE     *BLANK
055900971021     C                   SETON                                        32
056000971021     C                   MOVE      *ZERO         VABVLB
056100010604     C                   eval      vinmsg = %trimr(vinmsg)
056200010604     C                             + ' ' + 'VHBVLB'
056300971021     C                   ELSE
056400971021     C                   MOVEL     WI07          VABVLB
056500971023     C                   TESTN                   WD07                 30
056600971023     C  N30              TESTN                   WD07                   30
056700971023     C  N30              TESTN                   WD07                     30
056800971021     C                   MOVE      WD07          W001A             1
056900971021     C     *IN30         IFEQ      *OFF
057000971021     C     W001A         ORLT      '0'
057100971023     C     W001A         ANDNE     *BLANK
057200971021     C                   SETON                                        32
057300971021     C                   MOVE      *ZERO         VABVLB
057400010604     C                   eval      vinmsg = %trimr(vinmsg)
057500010604     C                             + ' ' + 'VHBVLB'
057600971021     C                   ELSE
057700971021     C                   MOVE      WD07          VABVLB
057800971021     C                   ENDIF
057900050118     C                   ENDIF
058000041001     C                   ELSE
058100041001     C* ...altrimenti forzo azzeramento del volume
058200041001     C                   Z-ADD     *zeros        VABVLB
058300041001     C                   ENDIF
058400941229     C*
058500971021     C                   MOVE      *ZERO         VABQFT
058600971021     C                   MOVE      VHBQFT        WASC15
058700971023     C                   TESTN                   WI15                 30
058800971023     C  N30              TESTN                   WI15                   30
058900971023     C  N30              TESTN                   WI15                     30
059000971021     C                   MOVE      WI15          W001A             1
059100971021     C     *IN30         IFEQ      *OFF
059200971021     C     W001A         ORLT      '0'
059300971023     C     W001A         ANDNE     *BLANK
059400971021     C                   SETON                                        32
059500971021     C                   MOVE      *ZERO         VABQFT
059600010604     C                   eval      vinmsg = %trimr(vinmsg)
059700010604     C                             + ' ' + 'VHBQFT'
059800971021     C                   ELSE
059900971021     C                   MOVEL     WI15          VABQFT
060000971023     C                   TESTN                   WD15                 30
060100971023     C  N30              TESTN                   WD15                   30
060200971023     C  N30              TESTN                   WD15                     30
060300971021     C                   MOVE      WD15          W001A             1
060400971021     C     *IN30         IFEQ      *OFF
060500971021     C     W001A         ORLT      '0'
060600971023     C     W001A         ANDNE     *BLANK
060700971021     C                   SETON                                        32
060800971021     C                   MOVE      *ZERO         VABQFT
060900010604     C                   eval      vinmsg = %trimr(vinmsg)
061000010604     C                             + ' ' + 'VHBQFT'
061100971021     C                   ELSE
061200971021     C                   MOVE      WD15          VABQFT
061300971021     C                   ENDIF
061400050118     C                   ENDIF
061500941229     C*
061600971021     C                   MOVE      *ZERO         VABCAS
061700971021     C                   MOVE      VHBCAS        WASC15
061800971023     C                   TESTN                   WI15                 30
061900971023     C  N30              TESTN                   WI15                   30
062000971023     C  N30              TESTN                   WI15                     30
062100971021     C                   MOVE      WI15          W001A             1
062200971021     C     *IN30         IFEQ      *OFF
062300971021     C     W001A         ORLT      '0'
062400971023     C     W001A         ANDNE     *BLANK
062500971021     C                   SETON                                        32
062600971021     C                   MOVE      *ZERO         VABCAS
062700010604     C                   eval      vinmsg = %trimr(vinmsg)
062800010604     C                             + ' ' + 'VHBCAS'
062900971021     C                   ELSE
063000971021     C                   MOVEL     WI15          VABCAS
063100971023     C                   TESTN                   WD15                 30
063200971023     C  N30              TESTN                   WD15                   30
063300971023     C  N30              TESTN                   WD15                     30
063400971021     C                   MOVE      WD15          W001A             1
063500971021     C     *IN30         IFEQ      *OFF
063600971021     C     W001A         ORLT      '0'
063700971023     C     W001A         ANDNE     *BLANK
063800971021     C                   SETON                                        32
063900971021     C                   MOVE      *ZERO         VABCAS
064000010604     C                   eval      vinmsg = %trimr(vinmsg)
064100010604     C                             + ' ' + 'VHBCAS'
064200971021     C                   ELSE
064300971021     C                   MOVE      WD15          VABCAS
064400971021     C                   ENDIF
064500050118     C                   ENDIF
064600971021     C*
064700971023     C                   TESTN                   VHBRMN               30
064800971023     C  N30              TESTN                   VHBRMN                 30
064900971023     C  N30              TESTN                   VHBRMN                   30
065000971021     C                   MOVE      VHBRMN        W001A             1
065100971021     C     *IN30         IFEQ      *OFF
065200971021     C     W001A         ORLT      '0'
065300971021     C                   SETON                                        32
065400971021     C                   MOVE      *ZERO         VABRMN
065500010604     C                   eval      vinmsg = %trimr(vinmsg)
065600010604     C                             + ' ' + 'VHBRMN'
065700971021     C                   ELSE
065800971021     C                   MOVE      VHBRMN        VABRMN
065900971021     C                   ENDIF
066000971021     C*
066100971023     C                   TESTN                   VHBNCD               30
066200971023     C  N30              TESTN                   VHBNCD                 30
066300971023     C  N30              TESTN                   VHBNCD                   30
066400971021     C                   MOVE      VHBNCD        W001A             1
066500971021     C     *IN30         IFEQ      *OFF
066600971021     C     W001A         ORLT      '0'
066700971023     C     W001A         ANDNE     *BLANK
066800971021     C                   SETON                                        32
066900971021     C                   MOVE      *ZERO         VABNCD
067000010604     C                   eval      vinmsg = %trimr(vinmsg)
067100010604     C                             + ' ' + 'VHBNCD'
067200971021     C                   ELSE
067300971021     C                   MOVE      VHBNCD        VABNCD
067400971021     C                   ENDIF
067500971021     C*
067600971023     C                   TESTN                   VHBNCA               30
067700971023     C  N30              TESTN                   VHBNCA                 30
067800971023     C  N30              TESTN                   VHBNCA                   30
067900971021     C                   MOVE      VHBNCA        W001A             1
068000971021     C     *IN30         IFEQ      *OFF
068100971021     C     W001A         ORLT      '0'
068200971023     C     W001A         ANDNE     *BLANK
068300971021     C                   SETON                                        32
068400971021     C                   MOVE      *ZERO         VABNCA
068500010604     C                   eval      vinmsg = %trimr(vinmsg)
068600010604     C                             + ' ' + 'VHBNCA'
068700971021     C                   ELSE
068800971021     C                   MOVE      VHBNCA        VABNCA
068900971021     C                   ENDIF
069000971021     C*
069100971023     C                   TESTN                   VHBZNC               30
069200971023     C  N30              TESTN                   VHBZNC                 30
069300971023     C  N30              TESTN                   VHBZNC                   30
069400971021     C                   MOVE      VHBZNC        W001A             1
069500971021     C     *IN30         IFEQ      *OFF
069600971021     C     W001A         ORLT      '0'
069700971023     C     W001A         ANDNE     *BLANK
069800971021     C                   SETON                                        32
069900971021     C                   MOVE      *ZERO         VABZNC
070000010604     C                   eval      vinmsg = %trimr(vinmsg)
070100010604     C                             + ' ' + 'VHBZNC'
070200971021     C                   ELSE
070300971021     C                   MOVE      VHBZNC        VABZNC
070400971021     C                   ENDIF
070500971021     C*
070600971023     C                   TESTN                   VHBDCR               30
070700971023     C  N30              TESTN                   VHBDCR                 30
070800971023     C  N30              TESTN                   VHBDCR                   30
070900971021     C                   MOVE      VHBDCR        W001A             1
071000971021     C     *IN30         IFEQ      *OFF
071100971021     C     W001A         ORLT      '0'
071200971023     C     W001A         ANDNE     *BLANK
071300971021     C                   SETON                                        32
071400971021     C                   MOVE      *ZERO         VABDCR
071500010604     C                   eval      vinmsg = %trimr(vinmsg)
071600010604     C                             + ' ' + 'VHBDCR'
071700971021     C                   ELSE
071800971021     C                   MOVE      VHBDCR        VABDCR
071900971021     C                   ENDIF
072000971021     C*
072100971023     C                   TESTN                   VHBHCR               30
072200971023     C  N30              TESTN                   VHBHCR                 30
072300971023     C  N30              TESTN                   VHBHCR                   30
072400971021     C                   MOVE      VHBHCR        W001A             1
072500971021     C     *IN30         IFEQ      *OFF
072600971021     C     W001A         ORLT      '0'
072700971023     C     W001A         ANDNE     *BLANK
072800971021     C                   SETON                                        32
072900971021     C                   MOVE      *ZERO         VABHCR
073000010604     C                   eval      vinmsg = %trimr(vinmsg)
073100010604     C                             + ' ' + 'VHBHCR'
073200971021     C                   ELSE
073300971021     C                   MOVE      VHBHCR        VABHCR
073400971021     C                   ENDIF
073500941229     C*
073600971021     C                   MOVE      *ZERO         VABVMD
073700971021     C                   MOVE      VHBVMD        WASC15
073800971023     C                   TESTN                   WI15                 30
073900971023     C  N30              TESTN                   WI15                   30
074000971023     C  N30              TESTN                   WI15                     30
074100971021     C                   MOVE      WI15          W001A             1
074200971021     C     *IN30         IFEQ      *OFF
074300971021     C     W001A         ORLT      '0'
074400971023     C     W001A         ANDNE     *BLANK
074500971021     C                   SETON                                        32
074600971021     C                   MOVE      *ZERO         VABVMD
074700010604     C                   eval      vinmsg = %trimr(vinmsg)
074800010604     C                             + ' ' + 'VHBVMD'
074900971021     C                   ELSE
075000971021     C                   MOVEL     WI15          VABVMD
075100971023     C                   TESTN                   WD15                 30
075200971023     C  N30              TESTN                   WD15                   30
075300971023     C  N30              TESTN                   WD15                     30
075400971021     C                   MOVE      WD15          W001A             1
075500971021     C     *IN30         IFEQ      *OFF
075600971021     C     W001A         ORLT      '0'
075700971023     C     W001A         ANDNE     *BLANK
075800971021     C                   SETON                                        32
075900971021     C                   MOVE      *ZERO         VABVMD
076000010604     C                   eval      vinmsg = %trimr(vinmsg)
076100010604     C                             + ' ' + 'VHBVMD'
076200971021     C                   ELSE
076300971021     C                   MOVE      WD15          VABVMD
076400971021     C                   ENDIF
076500050118     C                   ENDIF
076600971021     C*
076700971023     C                   TESTN                   VHBANT               30
076800971023     C  N30              TESTN                   VHBANT                 30
076900971023     C  N30              TESTN                   VHBANT                   30
077000971021     C                   MOVE      VHBANT        W001A             1
077100971021     C     *IN30         IFEQ      *OFF
077200971021     C     W001A         ORLT      '0'
077300971023     C     W001A         ANDNE     *BLANK
077400971021     C                   SETON                                        32
077500971021     C                   MOVE      *ZERO         VABANT
077600010604     C                   eval      vinmsg = %trimr(vinmsg)
077700010604     C                             + ' ' + 'VHBANT'
077800971021     C                   ELSE
077900971021     C                   MOVE      VHBANT        VABANT
078000971021     C                   ENDIF
078100971021     C*
078200971021     C* CAMPI ALFABETICI:
078300971021     C                   MOVE      VHBATB        VABATB
078400971021     C                   MOVE      VHBCBO        VABCBO
078500971021     C                   MOVE      VHBRSD        VABRSD
078600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
078700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
078800020117     C* ==
078900971021     C                   MOVE      VHBRD2        VABRD2
079000971021     C                   MOVE      VHBIND        VABIND
079100971021     C                   MOVE      VHBCAD        VABCAD
079200971021     C                   MOVE      VHBLOD        VABLOD
079300971021     C                   MOVE      VHBPRD        VABPRD
079400971021     C                   MOVE      VHBNZD        VABNZD
079500971021     C                   MOVE      VHBGC1        VABGC1
079600971021     C                   MOVE      VHBGC2        VABGC2
079700971021     C                   MOVE      VHBTSP        VABTSP
079800971021     C                   MOVE      VHBVAS        VABVAS
079900971021     C                   MOVE      VHBNAS        VABNAS
080000971021     C                   MOVE      VHBTIC        VABTIC
080100971021     C                   MOVE      VHBVCA        VABVCA
080200971021     C                   MOVE      VHBGCA        VABGCA
080300971021     C                   MOVE      VHBRMA        VABRMA
080400971021     C                   MOVE      VHBXCO        VABXCO
080500971021     C                   MOVE      VHBNOT        VABNOT
080600971021     C                   MOVE      VHBNT2        VABNT2
080700971021     C                   MOVE      VHBCTM        VABCTM
080800971021     C                   MOVE      VHBFFD        VABFFD
080900971021     C                   MOVE      VHBTCR        VABTCR
081000971021     C                   MOVE      VHBCTS        VABCTS
081100971021     C                   MOVE      VHBFTM        VABFTM
081200971021     C                   MOVE      VHBVAD        VABVAD
081300971021     C                   MOVE      VHBGMA        VABGMA
081400971021     C                   MOVE      VHBGGA        VABGGA
081500971021     C                   MOVE      VHBGVA        VABGVA
081600971021     C                   MOVE      VHBTC1        VABTC1
081700971021     C                   MOVE      VHBTC2        VABTC2
081800971021     C                   MOVE      VHBSCL        VABSCL
081900971021     C                   MOVE      VHBRMO        VABRMO
082000971021     C                   MOVE      VHBCMO        VABCMO
082100971021     C                   MOVE      VHBNMO        VABNMO
082200971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
082300971021     C* NELLA NAZIONE ORIGINALE
082400971021     C                   MOVE      VHBNMO        WA1D              1
082500971021     C                   MOVEL     VHBNMO        WA1S              1
082600971021     C     WA1D          IFNE      *BLANK
082700971021     C     WA1S          ANDEQ     *BLANK
082800971021     C                   MOVE      *BLANK        VABNMO
082900040227     C                   ENDIF
083000031229     C*
083100031229     C* Se il codice trattamento merce nn viene passato dal cliente lo valorizzo con il valore
083200031229     C* d default passato come parametro dall'anagrafica clienti/tipo file
083300031229     C                   IF        vabCTM = *blanks
083400031229     C                   EVAL      vabCTM = wCTM
083500031229     C                   ENDIF
083600031229     C*
083700031229     C* Verifica codice trattamento merce: utilizzabili solo i '6C' '6D' '7P' (x mantenimento
083800031229     C* numero d spedizione)
083900040302     C*                  IF        vabCTM = '6C' OR
084000040302     C*                            vabCTM = '6D' OR
084100040302     C*                            vabCTM = '7P'
084200040302     C*                  ELSE
084300040302     C*                  SETON                                        31
084400040302     C*                  EVAL      vinmsg = %trimr(vinmsg)
084500040302     C*                            + ' ' + 'VABCTM(non valido)'
084600040302     C*                  ENDIF
084700040227     C*
084800040227     C* Se presente particolarità consegna 'RC' e se il codice
084900040227     C* cliente mittente è valorizzato e numero serie ORM nn passata
085000040227     C* nei parametri verifico se il cliente è abilitato in tabella 'OSR'
085100040227     C                   IF        VABGMA = 'RC'
085200040227     C                   IF        VABCCM > *ZEROS
085300040227     C                   IF        wNSR = *BLANKS
085400040227     C                   Z-ADD     1             jOSR
085500080502     C                   move(p)   VABLNP        wLnpOSR
085600080502     C                   move(p)   VABCCM        wCliOSR
085700080502     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
085800080502     C                   IF        %found
085900040227     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
086000040227     C                   EVAL      %subst(vlrppt:4:2) = wNSR
086100040227     C                   ELSE
086200040227     C                   SETON                                        31
086300040227     C                   EVAL      vinmsg = %trimr(vinmsg)
086400040227     C                             + ' ' + 'ORMNRS(tabellaOSR)'
086500040227     C                   ENDIF
086600040227     C                   ENDIF
086700040227     C                   ENDIF
086800040227     C                   ENDIF
086900041025     C*
087000041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
087100041025     C                   IF        *IN52 = *ON
087200041025     C* Reperisco la provincia dal CAP e dalla località
087300041025     C                   IF        VABCAD <> *blanks AND
087400050907     C                             VABPRD  = *blanks AND
087500050907     C                             VABNZD  = *blanks
087600041025     C                   CLEAR                   TISI95DS
087700041025     C                   EVAL      I95TCN = '3'
087800041025     C                   Z-ADD     datcor        I95DAT
087900041025     C                   EVAL      I95CAP = VABCAD
088000041025     C                   EVAL      I95LOC = VABLOD
088100041025     C                   CALL      'TISI95R'
088200041025     C                   PARM                    TISI95DS
088300041025     C                   EVAL      VABPRD = O95PRV
088400041025     C                   ENDIF
088500041025     C                   ENDIF
088600930409     C*
088700010604     C  N31              ADD       1             §CTROK            7 0
088800010604     C   32              ADD       1             §CTRMO            7 0
088900010604     C   31              ADD       1             §CTRNO            7 0
089000020916     C  N31              WRITE     FIVAB000
089100030811     C*
089200040108     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
089300030811     C                   if        *IN31 = *OFF AND
089400030811     C                             vabGMA = 'RC'
089500040923     C*
089600040923     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
089700040923     C                   seton                                        54
089800040108     C*
089900040108     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
090000040108     C                   eval      wfivabds = fivabds
090100040920     C                   call      'TITV84R'
090200040108     C                   parm      *blanks       InTipl            1
090300040108     C                   parm                    tivlrds
090400040108     C                   parm                    wfivabds
090500040108     C*
090600030811     C                   endif
090700990910     C*
090800010601     C                   if        *in31 = *off and
090900010601     C                             *in32 = *off
091000990910     C                   eval      vinflg = '1'
091100990910     C                   else
091200000621     C                   eval      recko = vindta
091300031229     C                   if        %open(titv82ps)
091400030407     C   50              except    rigadett
091500010612     C                   endif
091600990910     C                   eval      vinflg = '2'
091700990910     C                   endif
091800000905     C                   endif
091900000905     C                   endif
092000010601     C*
092100010601     C                   endif
092200000905     C*
092300000905     C                   else
092400000905     C                   eval      vinflg = '1'
092500000905     C                   endif
092600000905     C*
092700000905     C  N70              update    tivin000
092800000905     C*
092900991022     C  N70              ENDdo
093000010601     C
093100010601     C*
093200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
093300010601     C                   if        cntNonEl = *zeros or
093400010601     C                             flgMulti = '0'
093500990910     C* Se non ci sono record con errori ...
093600010601     C                   if        §ctrno = 0 and
093700010601     C                             §ctrmo = 0 and
093800010601     C                             flgStato <> '2'
093900990910     C* ... restituisco esito OK.
094000990921     C                   eval      wrkesito = '0'
094100990910     C                   else
094200010601     C                   if        §ctrok > 0
094300000710     C                   eval      wrkesito = '1'
094400000710     C                   else
094500010615     C                   if        flgOk = '0'
094600010615     C                   eval      wrkesito = '2'
094700010615     C                   else
094800010615     C                   eval      wrkesito = '6'
094900010615     C                   endif
095000000710     C                   endif
095100010615     C                   endif
095200010601     C                   else
095300010601     C                   eval      wrkesito = '9'
095400010601     C                   endif
095500990910     C*
095600990914     C                   if        %open(tivin00r)
095700990908     C                   close     tivin00r
095800990914     C                   endif
095900020916     C                   if        %open(fivabwwr)
096000020916     C                   close     fivabwwr
096100990914     C                   endif
096200010601     C*
096300010601     C                   if        vlrpoi <> 999
096400010601     C                   eval      invfil = vlrpoi
096500010601     C                   endif
096600990910     C*
096700990920     C                   if        §ctrok > 0
096800010601     C                             and invfil > *zeros
096900990920     C                   exsr      invio
097000990920     C                   endif
097100010612     C*
097200010612     C                   if        flgGiro = '1'
097300040108     C                   exsr      endela
097400010612     C                   endif
097500990920     C*
097600910830     C                   ENDSR
097700000613     C***
097800030811
097900030811
098000030811
098100010601
098200010601
098300010601
098400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
098500010601     C     repfil        BEGSR
098600010601     C*
098700010601     C                   if        invfil = *zeros and
098800010601     C                             depfil > *zeros and
098900010613     C                             (vinflg = *blanks or
099000010613     C                              vinflg = *zeros)
099100010601     C
099200010601     C                   eval      invfil = depfil
099300010601     C                   endif
099400010601     C*
099500010601     C                   if        depfil <> invfil and
099600010601     C                             invfil > *zeros
099700010601     C                   eval      flgMulti = '1'
099800010601     C                   if        vinflg = *blanks
099900010601     C                   add       1             cntNonEl
100000010601     C                   endif
100100010601     C                   endif
100200010601     C*
100300010601     C                   if        vinflg = '2'
100400010601     C                   eval      flgStato = '2'
100500010601     C                   endif
100600010601     C*
100700010601     C                   ENDSR
100800010601     C***
100900010601
101000010601
101100030811
101200010601
101300990920      /TITLE Invio dei dati al punto operativo.
101400000613     C     invio         BEGSR
101500990920     C*
101600990920     C                   reset                   dscmz
101700010601     C                   move      invfil        cmzdst
101800990920     C                   eval      cmzfld = vlrfou
101900990920     C                   eval      cmzmbd = vlrhdl
102000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
102100000710     C                   if        prmfir = *blanks
102200020916     C                   eval      cmzfla = 'FIVAB00F'
102300020916     C                   eval      cmzmba = 'FIVAB00F'
102400000710     C                   else
102500000710     C                   eval      cmzfla = prmfir
102600000710     C                   eval      cmzmba = prmfir
102700000710     C                   endif
102800990920     C                   eval      cmznrr = *zeros
102900990920     C                   move      §ctrok        cmznrr
103000021003     C                   eval      cmzlba = vlrfl1
103100990920     C                   call(e)   'TIS711C'
103200990920     C                   parm                    dscmz
103300990921     C                   parm      *blanks       esito
103400990920     C                   if        %error
103500990920     C                             or cmzerr = '1'
103600990921     C                             or esito  = '1'
103700000710     C                   eval      wrkesito = '3'
103800990920     C                   endif
103900990920     C*
104000000613     C                   ENDSR
104100010601
104200010601
104300010601
104400010601
104500031229      /TITLE Operazioni iniziali e impostazoini d default
104600010601     C     opeini        BEGSR
104700010601     C*
104800010601     C* Inizializzo flag e contatori operativi
104900010601     C                   movel     '0'           flgGiro           1
105000010601     C                   movel     '0'           flgMulti          1
105100010601     C                   movel     '1'           flgStato          1
105200010615     C                   movel     '0'           flgOk             1
105300010601     C                   z-add     *zeros        cntNonEl         10 0
105400010601     C                   z-add     *zeros        depfil            3 0
105500010601     C                   z-add     *zeros        invfil            3 0
105600030407     C*
105700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
105800030407     C                   if        %subst(vlrppt:1:1) = 'S'
105900030407     C                   seton                                        50
106000030407     C                   else
106100030407     C                   setoff                                       50
106200030407     C                   endif
106300041001     C*
106400041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
106500041001     C                   if        %subst(vlrppt:26:1) = 'S'
106600041001     C                   seton                                        51
106700041001     C                   else
106800041001     C                   setoff                                       51
106900041001     C                   endif
107000041025     C*
107100041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
107200041025     C* (naturalmente solo se nn già presente)
107300041025     C                   if        %subst(vlrppt:27:1) = 'S'
107400041025     C                   seton                                        52
107500041025     C                   else
107600041025     C                   setoff                                       52
107700041025     C                   endif
107800031229     C*
107900031229     C* Verifico se impostato il parametro relativo al codice trattamento merce (VABCTM)
108000031229     C                   if        %subst(vlrppt:2:2) <> *blanks
108100031229     C                   eval      wCTM = %subst(vlrppt:2:2)
108200031229     C                   endif
108300031229     C*
108400031229     C* Verifico se impostato il parametro relativo al numero serie orm (ORMNSR)
108500031229     C                   if        %subst(vlrppt:4:2) <> *blanks
108600031229     C                   eval      wNSR = %subst(vlrppt:4:2)
108700031229     C                   endif
108800031229     C*
108900031229     C* Verifico se impostato il parametro relativo al codice consegna (ORMCRC)
109000031229     C                   if        %subst(vlrppt:6:10) <> *blanks
109100031229     C                   eval      wCRC = %subst(vlrppt:6:10)
109200031229     C                   endif
109300031229     C*
109400031229     C* Verifico se impostato il parametro relativo al codice pagante (ORMKSC)
109500031229     C                   if        %subst(vlrppt:16:7) <> *blanks
109600031229     C                   eval      wKSC = %subst(vlrppt:16:7)
109700031229     C                   endif
109800031229     C*
109900031229     C* Verifico se impostato il parametro relativo al codice pagante (ORMCTR)
110000031229     C                   if        %subst(vlrppt:23:3) <> *blanks
110100031229     C                   eval      wCTR = %subst(vlrppt:23:3)
110200031229     C                   endif
110300010601     C*
110400010601     C                   ENDSR
110500010601     C***
110600040227
110700040227
110800040227
110900040227     C*--------------------------------------------------------
111000040227     C* CARTAB - CARICAMENTO TABELLE                          *
111100040227     C*--------------------------------------------------------
111200040227     C     CARTAB        BEGSR
111300040227     C*
111400040227     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
111500040227     C                   z-add     *zeros        jOSR              3 0
111600040227     C     'OSR'         setll     tntbe01l
111700040227     C                   if        %equal(tntbe01l)
111800040227     C     'OSR'         reade     tntbe01l
111900040227     C                   dow       not %eof(tntbe01l)
112000040302     C                   eval      DOSR = tbeUNI
112100080502     c*
112200080502     c* Verifica e forzatura x nuovi campi aggiunti
112300080502     c                   if        aD§OSRcl6 = *blanks
112400080502     c                   eval      D§OSRcl6 = *zeros
112500080502     c                   endif
112600080502     c                   if        aD§OSRcl7 = *blanks
112700080502     c                   eval      D§OSRcl7 = *zeros
112800080502     c                   endif
112900080502     c                   if        aD§OSRcl8 = *blanks
113000080502     c                   eval      D§OSRcl8 = *zeros
113100080502     c                   endif
113200080502     c                   if        aD§OSRcl9 = *blanks
113300080502     c                   eval      D§OSRcl9 = *zeros
113400080502     c                   endif
113500080502     c                   if        aD§OSRcl10 = *blanks
113600080502     c                   eval      D§OSRcl10 = *zeros
113700080502     c                   endif
113800080502     c                   if        aD§OSRcl11 = *blanks
113900080502     c                   eval      D§OSRcl11 = *zeros
114000080502     c                   endif
114100080502     c                   if        aD§OSRcl12 = *blanks
114200080502     c                   eval      D§OSRcl12 = *zeros
114300080502     c                   endif
114400080502     c                   if        aD§OSRcl13 = *blanks
114500080502     c                   eval      D§OSRcl13 = *zeros
114600080502     c                   endif
114700080502     c                   if        aD§OSRcl14 = *blanks
114800080502     c                   eval      D§OSRcl14 = *zeros
114900080502     c                   endif
115000080502     c                   if        aD§OSRcl15 = *blanks
115100080502     c                   eval      D§OSRcl15 = *zeros
115200080502     c                   endif
115300080502     c                   if        aD§OSRcl16 = *blanks
115400080502     c                   eval      D§OSRcl16 = *zeros
115500080502     c                   endif
115600080502     c*
115700080502     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
115800040302     C                   if        D§OSRCLI <> *zeros
115900040302     C                   add       1             jOSR
116000040227     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
116100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
116200080502     C                   movel     D§OSRCLI      wCliOSR
116300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
116400040302     C                   endif
116500040302     C*
116600040302     C                   if        D§OSRCL2 <> *zeros
116700040302     C                   add       1             jOSR
116800040302     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
116900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
117000080502     C                   movel     D§OSRCL2      wCliOSR
117100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
117200040302     C                   endif
117300040302     C*
117400040302     C                   if        D§OSRCL3 <> *zeros
117500040302     C                   add       1             jOSR
117600040302     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
117700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
117800080502     C                   movel     D§OSRCL3      wCliOSR
117900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
118000040302     C                   endif
118100040302     C*
118200040302     C                   if        D§OSRCL4 <> *zeros
118300040302     C                   add       1             jOSR
118400040302     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
118500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
118600080502     C                   movel     D§OSRCL4      wCliOSR
118700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
118800040302     C                   endif
118900040302     C*
119000040302     C                   if        D§OSRCL5 <> *zeros
119100040302     C                   add       1             jOSR
119200040302     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
119300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
119400080502     C                   movel     D§OSRCL5      wCliOSR
119500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
119600040302     C                   endif
119700080502     C*
119800080502     C                   if        D§OSRCL6 <> *zeros
119900080502     C                   add       1             jOSR
120000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
120100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
120200080502     C                   movel     D§OSRCL6      wCliOSR
120300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
120400080502     C                   endif
120500080502     C*
120600080502     C                   if        D§OSRCL7 <> *zeros
120700080502     C                   add       1             jOSR
120800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
120900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
121000080502     C                   movel     D§OSRCL7      wCliOSR
121100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
121200080502     C                   endif
121300080502     C*
121400080502     C                   if        D§OSRCL8 <> *zeros
121500080502     C                   add       1             jOSR
121600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
121700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
121800080502     C                   movel     D§OSRCL8      wCliOSR
121900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
122000080502     C                   endif
122100080502     C*
122200080502     C                   if        D§OSRCL9 <> *zeros
122300080502     C                   add       1             jOSR
122400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
122500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
122600080502     C                   movel     D§OSRCL9      wCliOSR
122700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
122800080502     C                   endif
122900080502     C*
123000080502     C                   if        D§OSRCL10 <> *zeros
123100080502     C                   add       1             jOSR
123200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
123300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
123400080502     C                   movel     D§OSRCL10     wCliOSR
123500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
123600080502     C                   endif
123700080502     C*
123800080502     C                   if        D§OSRCL11 <> *zeros
123900080502     C                   add       1             jOSR
124000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
124100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
124200080502     C                   movel     D§OSRCL11     wCliOSR
124300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
124400080502     C                   endif
124500080502     C*
124600080502     C                   if        D§OSRCL12 <> *zeros
124700080502     C                   add       1             jOSR
124800080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
124900080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
125000080502     C                   movel     D§OSRCL12     wCliOSR
125100080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
125200080502     C                   endif
125300080502     C*
125400080502     C                   if        D§OSRCL13 <> *zeros
125500080502     C                   add       1             jOSR
125600080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
125700080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
125800080502     C                   movel     D§OSRCL13     wCliOSR
125900080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
126000080502     C                   endif
126100080502     C*
126200080502     C                   if        D§OSRCL14 <> *zeros
126300080502     C                   add       1             jOSR
126400080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
126500080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
126600080502     C                   movel     D§OSRCL14     wCliOSR
126700080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
126800080502     C                   endif
126900080502     C*
127000080502     C                   if        D§OSRCL15 <> *zeros
127100080502     C                   add       1             jOSR
127200080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
127300080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
127400080502     C                   movel     D§OSRCL15     wCliOSR
127500080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
127600080502     C                   endif
127700080502     C*
127800080502     C                   if        D§OSRCL16 <> *zeros
127900080502     C                   add       1             jOSR
128000080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
128100080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
128200080502     C                   movel     D§OSRCL16     wCliOSR
128300080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
128400080502     C                   endif
128500040302     C*
128600040227     C     'OSR'         reade     tntbe01l
128700040227     C                   enddo
128800040227     C                   endif
128900040227     C*
129000040227     C                   ENDSR
129100040227     C***
129200010601
129300010601
129400990910
129500000613     C     *inzsr        BEGSR
129600990910     C*
129700990910     C     *entry        plist
129800990920     C                   parm                    tivlrds
129900990921     C                   parm      wrkesito      esito
130000000724     C                   parm                    prmlit
130100000710     C                   parm                    prmfir
130200040227     C*
130300040227     C* CARICO LE TABELLE OCCORRENTI
130400040227     C                   exsr      CARTAB
130500010606     C*
130600010606     C* CALCOLA LA DATA CORRENTE
130700010606     C                   time                    wn14             14 0
130800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
130900030811     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
131000010606     C                   z-add     wn8           g08dat
131100010606     C                   z-add     *zeros        g08inv
131200010606     C                   movel     '0'           g08err
131300010606     C                   call      'XSRDA8'
131400010606     C                   parm                    wlbda8
131500010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
131600000613     C*
131700000613     C                   ENDSR
131800000613     C***
131900990908
132000031229     Otitv82p   E            riepilogo         2
132100990915     O                                              'Upload via Internet'
132200990915     O                                           +1 'Traduzione TIVIN00R -'
132300020916     O                                           +1 'FIVABWWR'
132400990915     O                                           +1 'Testate bolle'
132500990915     O          E            riepilogo   2
132600990915     O                       wrkdata
132700990915     O                       wrkora              +1
132800990915     O                       procname            +1
132900990915     O          E            riepilogo   2
133000990915     O                                              'Cliente..................:'
133100990915     O                       VABCCM        z     +1
133200990915     O          E            riepilogo   2
133300990920     O                                              'Riferimento Strategi.....:'
133400990920     O                       vlrhdl              +1
133500990915     O          E            riepilogo   2
133600990915     O                                              'Giusti...................:'
133700971022     O                       §CTROK        2   +  1
133800990915     O          E            riepilogo   2
133900990915     O                                              'Sbagliati e corretti.....:'
134000971022     O                       §CTRMO        2   +  1
134100990915     O          E            riepilogo   2
134200990915     O                                              'Sbagliati e scartati.....:'
134300971022     O                       §CTRNO        2   +  1
134400000613
134500031229     Otitv82ps  E            testdett          2
134600000613     O                                              'Upload via Internet'
134700000613     O                                           +1 'Traduzione TIVIN00R -'
134800020916     O                                           +1 'FIVABWWR'
134900000613     O                                           +1 'Testate bolle'
135000000616     O          E            testdett    3
135100000613     O                                           +2 'N° rec'
135200000613     O                                           +3 'Anteprima contenuto'
135300000616     O          E            rigadett    2
135400000613     O                       rrnum               +2
135500000621     O                       recko               +3
135600000616     O          E            findett     2
135700000613     O                       wrkdata
135800000613     O                       wrkora              +1
135900000613     O                       procname            +1
136000000616     O          E            findett     2
136100000613     O                                              'Cliente..................:'
136200000613     O                       VABCCM        z     +1
136300000616     O          E            findett     2
136400000613     O                                              'Riferimento Strategi.....:'
136500000613     O                       vlrhdl              +1
136600000616     O          E            findett     2
136700000613     O                                              'Giusti...................:'
136800000613     O                       §CTROK        2   +  1
136900000616     O          E            findett     2
137000000613     O                                              'Sbagliati e corretti.....:'
137100000613     O                       §CTRMO        2   +  1
137200000616     O          E            findett     2
137300000613     O                                              'Sbagliati e scartati.....:'
137400000613     O                       §CTRNO        2   +  1
137500000613** CMD - COMANDI CL
137600031229OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV82P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
137700031229OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV82PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
137800031229DLTOVR FILE(TITV82P TITV82PS) LVL(*)
137900000613
138000000613
