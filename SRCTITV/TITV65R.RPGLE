000100020916      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500030811     Ffnacr01l  if   e           k disk
000600090618     Ftntbe01l  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800020916     FFIVABwwr  O    E             DISK    usropn
000900030811     FFNORM00F  O    E             DISK
001000030811     FFNORF00F  O    E             DISK
001100070709     ffnorg00f  o    e             disk
001200050826     FTIVGD00F  O    E             DISK
001300030811     FTIORI00F  O    E             DISK
001400030811     Ftitv65p   O    f  132        PRINTER usropn
001500000621     F                                     oflind(*inoa)
001600030811     Ftitv65ps  O    f  198        PRINTER usropn
001700000621     F                                     oflind(*inob)
001800000313     D*
001900000613     D*------------
002000000613     D* COMANDI
002100000613     D*------------
002200011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002300000724     D*------------
002400030811     D* DS ESTERNE
002500030811     D*------------
002600030811     D kpjba         e ds                  inz
002700030811     D dscmz         e ds                  inz
002800030811     D tisi95ds      e ds                  inz
002900030811     D trul33ds      e ds                  inz
003000030811     D trul13ds      e ds                  inz
003100030811     D dfar          e ds                  inz
003200030811     D dorm01        e ds                  inz
003300990910     D psds           sds
003400990910     D  procname         *PROC
003500030811     D*------------
003600030811     D* DS INTERNE
003700030811     D*------------
003800941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
003900941229     D* 10+3
004000941229     D                 DS
004100000313     D  WI15                   1     10
004200000313     D  WD15                  11     13
004300000313     D  WASC15                 1     13
004400941229     D*  6+1
004500941229     D                 DS
004600000313     D  WI09                   1      6
004700000313     D  WD09                   7      7
004800000313     D  WASC09                 1      7
004900941229     D*  2+3
005000941229     D                 DS
005100000313     D  WI07                   1      2
005200000313     D  WD07                   3      5
005300000313     D  WASC07                 1      5
005400000313     D*
005500990920     D tivlrds       e ds                  extname(tivlr00f)
005600050826     D fnvapds       e ds                  extname(fnvap00f)
005700000313     D*---------------------------------------------------------
005800000313     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
005900000313     D*---------------------------------------------------------
006000000313     D                 ds
006100000313     D vhbatb                  1      1
006200000313     D vhbccm                  2      8
006300000313     D vhblnp                  9     11
006400000313     D vhbaas                 12     15
006500000313     D vhbmgs                 16     19
006600000313     D vhbnrs                 20     21
006700000313     D vhbnsp                 22     28
006800000313     D vhbcbo                 29     30
006900000313     D vhblna                 31     33
007000000313     D vhbrsd                 34     68
007100000313     D vhbrd2                 69    103
007200000313     D vhbind                104    138
007300000313     D vhbcad                139    147
007400000313     D vhblod                148    182
007500000313     D vhbprd                183    184
007600000313     D vhbnzd                185    187
007700000313     D vhbgc1                188    189
007800000313     D vhbgc2                190    191
007900000313     D vhbctr                192    194
008000000313     D vhbtsp                195    195
008100000313     D vhbias                196    208
008200000313     D vhbvas                209    211
008300000313     D vhbnas                212    226
008400000313     D vhbncl                227    231
008500000313     D vhbpkb                232    238
008600000313     D vhbvlb                239    243
008700000313     D vhbqft                244    256
008800000313     D vhbcas                257    269
008900000313     D vhbtic                270    271
009000000313     D vhbvca                272    274
009100000313     D vhbgca                275    276
009200000313     D vhbrmn                277    291
009300000313     D vhbrma                292    306
009400000313     D vhbncd                307    313
009500000313     D vhbnca                314    320
009600000313     D vhbxco                321    321
009700000313     D vhbnot                322    356
009800000313     D vhbnt2                357    391
009900000313     D vhbznc                392    393
010000000313     D vhbctm                394    395
010100000313     D vhbffd                396    396
010200000313     D vhbdcr                397    404
010300000313     D vhbtcr                405    405
010400000313     D vhbhcr                406    409
010500000313     D vhbcts                410    411
010600000313     D vhbftm                412    412
010700000313     D vhbvmd                413    425
010800000313     D vhbvad                426    428
010900000313     D vhbgma                429    430
011000000313     D vhbgga                431    432
011100000313     D vhbgva                433    434
011200000313     D vhbtc1                435    435
011300000313     D vhbtc2                436    436
011400000313     D vhbscl                437    437
011500000313     D vhbant                438    446
011600000313     D vhbrmo                447    471
011700000313     D vhbcmo                472    480
011800000313     D vhbnmo                481    483
011900000313     D  X_rec                  1    483    inz
012000000313     D*
012100990910     D esito           s              1
012200000724     D prmlit          s             10
012300000710     D prmfir          s             10
012400990921     D wrkesito        s                   like(esito)
012500990915     D wrkdata         s               d
012600990915     D wrkora          s               t
012700000613     D rrnum           s              6  0 INZ(*zeros)
012800000621     D recko           s            150    INZ(*blanks)
012900011123     D depcmd          s            150    INZ(*blanks)
013000030811     D Ktbcod          s                   LIKE(tbeCOD)
013100030811     D Ktbke1          s                   LIKE(tbeKE1)
013200030915     D flgORM          s              1    INZ('N')
013300090618     d §numori         s              7  0 inz
013400050907     D*
013500050907     D TxtInOut        S           2048
013600050907     D ElencoChar      S            256
013700050907     D TipoElenco      S              1
013800050907     D CharSost        S              1
013900050907     D UpperCase       S              1
014000050907     D ChkNull         S              1
014100050907     D CharNull        S              1
014200050907     D EsitoBon        S              1
014300000313     D*
014400010606     D*------------------
014500010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
014600010606     D*------------------
014700010606     D WLBDA8          DS                  INZ
014800010606     D  G08DAT                 1      8  0
014900010606     D  G08INV                 9     16  0
015000010606     D  G08ERR                17     17
015100010606     D  G08TGI                18     22  0
015200060113     D*------------------
015300060113     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
015400060113     D*------------------
015500060113     D TISIT0DS      E DS
015600060113     D DDSTFLO       E DS
015700030811     D*------------------
015800030811     D* DS RIDEFINIZIONE CHIAVE BOLLA ALFA
015900030811     D*------------------
016000030811     D                 DS                  INZ
016100030811     D  vabAAS                 1      4  0
016200030811     D  vabLNP                 5      7  0
016300030811     D  vabNRS                 8      9  0
016400030811     D  vabNSP                10     16  0
016500030811     D  vabKEY                 1     16
016600010606     D*
016700010606
016800010606
016900010606
017000010606
017100990915     C                   time                    wrkdata
017200990915     C                   time                    wrkora
017300000913     C                   reset                   rrnum
017400990921     C                   reset                   esito
017500990921     C                   reset                   wrkesito
017600010601     C*
017700050907     C                   exsr      exebon
017800010601     C                   exsr      opeini
017900010601     C                   exsr      rwvab
018000030811     C                   exsr      exefin
018100010601     C*
018200010601     C                   seton                                        lr
018300010601
018400010601
018500010601
018600010601
018700010601     C*--------------------------------------------------------
018800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
018900010601     C*--------------------------------------------------------
019000010601     C     PREELA        BEGSR
019100010601     C*
019200010601     C* SE OCCORRE SPEDIRE IN FILIALE
019300010601     C                   if        invfil <> *zeros and
019400010601     C                             flgGiro = '0'
019500010601     C*
019600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
019700010601     C                   eval      flgGiro = '1'
019800000724     C*
019900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
020000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
020100010601     C     invfil        chain     azorg01l
020200000724     C                   if        %found
020300000724     C                   movel(p)  CMD(1)        depcmd
020400020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
020500000724     C*
020600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
020700011123     C                   Z-ADD     150           LENGH            15 5
020800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020900000724     C                   PARM                    depcmd
021000000724     C                   PARM                    LENGH
021100000724     C*
021200000724     C                   endif
021300000616     C*
021400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
021500000616     C                   movel(p)  CMD(2)        depcmd
021600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
021700000616     C*
021800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
021900011123     C                   Z-ADD     150           LENGH            15 5
022000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
022100000616     C                   PARM                    depcmd
022200000616     C                   PARM                    LENGH
022300000613     C*
022400030811     C                   if        not %open(titv65ps)
022500030811     C                   open      titv65ps
022600030407     C   50              except    testdett
022700000613     C                   endif
022800010601     C*
022900010601     C                   endif
023000010601     C*
023100010601     C                   ENDSR
023200010601     C***
023300010601
023400010601
023500010601
023600010601
023700010601
023800010601
023900010601     C*--------------------------------------------------------
024000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
024100010601     C*--------------------------------------------------------
024200010601     C     ENDELA        BEGSR
024300990915     C*
024400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
024500000613     C*
024600030811     C                   if        %open(titv65ps)
024700030407     C   50              except    findett
024800030811     C                   close     titv65ps
024900000613     C                   endif
025000000616     C*
025100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
025200011123     C                   Z-ADD     150           LENGH            15 5
025300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
025400000616     C                   PARM                    CMD(3)
025500000616     C                   PARM                    LENGH
025600010601     C*
025700010601     C                   ENDSR
025800010601     C***
025900010601
026000010601
026100000613
026200000613
026300000613
026400000613     C*--------------------------------------------------------
026500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
026600000613     C*--------------------------------------------------------
026700000621     C     STPR          BEGSR
026800000613     C*
026900030811     C                   if        not %open(titv65p)
027000030811     C                   open      titv65p
027100990915     C                   endif
027200990915     C*
027300030407     C   50              except    riepilogo
027400990915     C*
027500030811     C                   if        %open(titv65p)
027600030811     C                   close     titv65p
027700990914     C                   endif
027800000613     C*
027900000613     C                   ENDSR
028000000613     C***
028100000613
028200010601
028300010601
028400010601
028500010601
028600910830     C*--------------------------------------------------------
028700020916     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
028800910830     C*--------------------------------------------------------
028900941229     C     RWVAB         BEGSR
029000990910     C*
029100990914     C                   if        not %open(tivin00r)
029200990908     C                   open      tivin00r
029300990914     C                   endif
029400020916     C                   if        not %open(fivabwwr)
029500020916     C                   open      fivabwwr
029600990914     C                   endif
029700990910     C*
029800990910     C                   clear                   §CTROK
029900990910     C                   clear                   §CTRMO
030000990910     C                   clear                   §CTRNO
030100990910     C*
030200921023     C                   DO        *HIVAL
030300990913     C*
030400990915     C                   READ      tivin00r                               70
030500010618     C*
030600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
030700010618     C                   if        vinflg = '1'
030800010618     C                   eval      flgOk = '1'
030900010618     C                   endif
031000010618     C*
031100000905     C                   if        vindta > *blanks
031200000613     C                   add       1             rrnum
031300990913     C*
031400010601     C                   if        *in70 = *off and
031500990913     C                             (vinflg = *blanks
031600010711     C                              or vinflg = '0'
031700010711     C                              or vinflg = '2')
031800990913     C*
031900020916     C                   clear                   FIVAB000
032000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
032100010711     C                   if        vinflg = *blanks or vinflg = '0'
032200010711     C                   clear                   vinmsg
032300010711     C                   endif
032400010711     C*
032500000313     C                   eval      x_rec  = vindta
032600991022     C*
032700991022     C                   testb     x'3F'         vhbatb                   02
032800991022     C                   if        not *in02
032900991022     C*
033000971021     C                   SETOFF                                       303132
033100971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
033200971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
033300971023     C*    OPPURE TUTTI BLANKS
033400010601     C*
033500010601     C                   TESTN                   VHBLNP               30
033600010601     C  N30              TESTN                   VHBLNP                 30
033700010601     C  N30              TESTN                   VHBLNP                   30
033800010601     C                   MOVE      VHBLNP        W001A             1
033900010601     C     *IN30         IFEQ      *OFF
034000010601     C     W001A         ORLT      '0'
034100010601     C                   SETON                                        31
034200010601     C                   eval      vinmsg = %trimr(vinmsg)
034300010601     C                             + ' ' + 'VHBLNP'
034400010601     C                   ELSE
034500010601     C                   MOVE      VHBLNP        VABLNP
034600010601     C     VABLNP        IFLE      *ZEROS
034700010601     C                   SETON                                        31
034800010601     C                   eval      vinmsg = %trimr(vinmsg)
034900010601     C                             + ' ' + 'VHBLNP'
035000010601     C                   ENDIF
035100010601     C                   ENDIF
035200010601     C*
035300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
035400010601     C                   eval      depfil = VABLNP
035500010601     C                   exsr      repfil
035600010601     C                   if        depfil = invfil
035700021014     C                   if        vlrpoi = 999
035800021010     C                   MOVE(P)   invfil        VABFGS
035900021014     C                   else
036000021014     C                   MOVE(P)   vlrpoi        VABFGS
036100021014     C                   endif
036200010601     C*
036300010601     C                   exsr      PREELA
036400010601     C*
036500971023     C                   TESTN                   VHBCCM               30
036600971023     C  N30              TESTN                   VHBCCM                 30
036700971023     C  N30              TESTN                   VHBCCM                   30
036800971021     C                   MOVE      VHBCCM        W001A             1
036900971021     C     *IN30         IFEQ      *OFF
037000971021     C     W001A         ORLT      '0'
037100971021     C                   SETON                                        31
037200010604     C                   eval      vinmsg = %trimr(vinmsg)
037300010604     C                             + ' ' + 'VHBCCM'
037400971021     C                   ELSE
037500971021     C                   MOVE      VHBCCM        VABCCM
037600971024     C     VABCCM        IFLE      *ZEROS
037700971024     C                   SETON                                        31
037800010604     C                   eval      vinmsg = %trimr(vinmsg)
037900010604     C                             + ' ' + 'VHBCCM'
038000971024     C                   ENDIF
038100971021     C                   ENDIF
038200971021     C*
038300971023     C                   TESTN                   VHBAAS               30
038400971023     C  N30              TESTN                   VHBAAS                 30
038500971023     C  N30              TESTN                   VHBAAS                   30
038600971021     C                   MOVE      VHBAAS        W001A             1
038700971021     C     *IN30         IFEQ      *OFF
038800971021     C     W001A         ORLT      '0'
038900971023     C     W001A         ANDNE     *BLANK
039000971021     C                   SETON                                        32
039100010606     C                   MOVEL     datcor        VABAAS
039200010604     C                   eval      vinmsg = %trimr(vinmsg)
039300010604     C                             + ' ' + 'VHBAAS'
039400971021     C                   ELSE
039500971021     C                   MOVE      VHBAAS        VABAAS
039600971021     C                   ENDIF
039700971021     C*
039800971023     C                   TESTN                   VHBMGS               30
039900971023     C  N30              TESTN                   VHBMGS                 30
040000971023     C  N30              TESTN                   VHBMGS                   30
040100971021     C                   MOVE      VHBMGS        W001A             1
040200971021     C     *IN30         IFEQ      *OFF
040300971021     C     W001A         ORLT      '0'
040400971023     C     W001A         ANDNE     *BLANK
040500971021     C                   SETON                                        32
040600010601     C                   MOVE      '0101'        VABMGS
040700010604     C                   eval      vinmsg = %trimr(vinmsg)
040800010604     C                             + ' ' + 'VHBMGS'
040900971021     C                   ELSE
041000971021     C                   MOVE      VHBMGS        VABMGS
041100971021     C                   ENDIF
041200971021     C*
041300971023     C                   TESTN                   VHBNRS               30
041400971023     C  N30              TESTN                   VHBNRS                 30
041500971023     C  N30              TESTN                   VHBNRS                   30
041600971021     C                   MOVE      VHBNRS        W001A             1
041700971021     C     *IN30         IFEQ      *OFF
041800971021     C     W001A         ORLT      '0'
041900971023     C     W001A         ANDNE     *BLANK
042000971021     C                   SETON                                        32
042100971021     C                   MOVE      *ZERO         VABNRS
042200010604     C                   eval      vinmsg = %trimr(vinmsg)
042300010604     C                             + ' ' + 'VHBNRS'
042400971021     C                   ELSE
042500971021     C                   MOVE      VHBNRS        VABNRS
042600971021     C                   ENDIF
042700971021     C*
042800971023     C                   TESTN                   VHBNSP               30
042900971023     C  N30              TESTN                   VHBNSP                 30
043000971023     C  N30              TESTN                   VHBNSP                   30
043100971021     C                   MOVE      VHBNSP        W001A             1
043200971021     C     *IN30         IFEQ      *OFF
043300971021     C     W001A         ORLT      '0'
043400971023     C     W001A         ANDNE     *BLANK
043500971021     C                   SETON                                        32
043600971021     C                   MOVE      *ZERO         VABNSP
043700010604     C                   eval      vinmsg = %trimr(vinmsg)
043800010604     C                             + ' ' + 'VHBNSP'
043900971021     C                   ELSE
044000971021     C                   MOVE      VHBNSP        VABNSP
044100971021     C                   ENDIF
044200971021     C*
044300971023     C                   TESTN                   VHBLNA               30
044400971023     C  N30              TESTN                   VHBLNA                 30
044500971023     C  N30              TESTN                   VHBLNA                   30
044600971021     C                   MOVE      VHBLNA        W001A             1
044700971021     C     *IN30         IFEQ      *OFF
044800971021     C     W001A         ORLT      '0'
044900971023     C     W001A         ANDNE     *BLANK
045000971021     C                   SETON                                        32
045100971021     C                   MOVE      *ZERO         VABLNA
045200010604     C                   eval      vinmsg = %trimr(vinmsg)
045300010604     C                             + ' ' + 'VHBLNA'
045400971021     C                   ELSE
045500971021     C                   MOVE      VHBLNA        VABLNA
045600971021     C                   ENDIF
045700971021     C*
045800971023     C                   TESTN                   VHBCTR               30
045900971023     C  N30              TESTN                   VHBCTR                 30
046000971023     C  N30              TESTN                   VHBCTR                   30
046100971021     C                   MOVE      VHBCTR        W001A             1
046200971021     C     *IN30         IFEQ      *OFF
046300971021     C     W001A         ORLT      '0'
046400971023     C     W001A         ANDNE     *BLANK
046500971021     C                   SETON                                        32
046600971021     C                   MOVE      *ZERO         VABCTR
046700010604     C                   eval      vinmsg = %trimr(vinmsg)
046800010604     C                             + ' ' + 'VHBCTR'
046900971021     C                   ELSE
047000971021     C                   MOVE      VHBCTR        VABCTR
047100971021     C                   ENDIF
047200941229     C*
047300971021     C                   MOVE      *ZERO         VABIAS
047400941229     C                   MOVE      VHBIAS        WASC15
047500971023     C                   TESTN                   WI15                 30
047600971023     C  N30              TESTN                   WI15                   30
047700971023     C  N30              TESTN                   WI15                     30
047800971021     C                   MOVE      WI15          W001A             1
047900971021     C     *IN30         IFEQ      *OFF
048000971021     C     W001A         ORLT      '0'
048100971023     C     W001A         ANDNE     *BLANK
048200971021     C                   SETON                                        32
048300971021     C                   MOVE      *ZERO         VABIAS
048400010604     C                   eval      vinmsg = %trimr(vinmsg)
048500010604     C                             + ' ' + 'VHBIAS'
048600971021     C                   ELSE
048700971021     C                   MOVEL     WI15          VABIAS
048800971021     C                   ENDIF
048900971023     C                   TESTN                   WD15                 30
049000971023     C  N30              TESTN                   WD15                   30
049100971023     C  N30              TESTN                   WD15                     30
049200971021     C                   MOVE      WD15          W001A             1
049300971021     C     *IN30         IFEQ      *OFF
049400971021     C     W001A         ORLT      '0'
049500971023     C     W001A         ANDNE     *BLANK
049600971021     C                   SETON                                        32
049700971021     C                   MOVE      *ZERO         VABIAS
049800010604     C                   eval      vinmsg = %trimr(vinmsg)
049900010604     C                             + ' ' + 'VHBIAS'
050000971021     C                   ELSE
050100971021     C                   MOVE      WD15          VABIAS
050200971021     C                   ENDIF
050300941229     C*
050400971023     C                   TESTN                   VHBNCL               30
050500971023     C  N30              TESTN                   VHBNCL                 30
050600971023     C  N30              TESTN                   VHBNCL                   30
050700971021     C                   MOVE      VHBNCL        W001A             1
050800971021     C     *IN30         IFEQ      *OFF
050900971021     C     W001A         ORLT      '0'
051000971021     C                   SETON                                        32
051100971021     C                   MOVE      *ZERO         VABNCL
051200010604     C                   eval      vinmsg = %trimr(vinmsg)
051300010604     C                             + ' ' + 'VHBNCL'
051400971021     C                   ELSE
051500971021     C                   MOVE      VHBNCL        VABNCL
051600971021     C                   ENDIF
051700941229     C*
051800971021     C                   MOVE      *ZERO         VABPKB
051900971021     C                   MOVE      VHBPKB        WASC09
052000971023     C                   TESTN                   WI09                 30
052100971023     C  N30              TESTN                   WI09                   30
052200971023     C  N30              TESTN                   WI09                     30
052300971021     C                   MOVE      WI09          W001A             1
052400971021     C     *IN30         IFEQ      *OFF
052500971021     C     W001A         ORLT      '0'
052600971021     C                   SETON                                        32
052700971021     C                   MOVE      *ZERO         VABPKB
052800010604     C                   eval      vinmsg = %trimr(vinmsg)
052900010604     C                             + ' ' + 'VHBPKB'
053000971021     C                   ELSE
053100971021     C                   MOVEL     WI09          VABPKB
053200971021     C                   ENDIF
053300971023     C                   TESTN                   WD09                 30
053400971023     C  N30              TESTN                   WD09                     30
053500971021     C                   MOVE      WD09          W001A             1
053600971021     C     *IN30         IFEQ      *OFF
053700971021     C     W001A         ORLT      '0'
053800971023     C     W001A         ANDNE     *BLANK
053900971021     C                   SETON                                        32
054000971021     C                   MOVE      *ZERO         VABPKB
054100010604     C                   eval      vinmsg = %trimr(vinmsg)
054200010604     C                             + ' ' + 'VHBPKB'
054300971021     C                   ELSE
054400971021     C                   MOVE      WD09          VABPKB
054500971021     C                   ENDIF
054600971021     C*
054700971021     C                   MOVE      *ZERO         VABVLB
054800971021     C                   MOVE      VHBVLB        WASC07
054900971023     C                   TESTN                   WI07                 30
055000971023     C  N30              TESTN                   WI07                   30
055100971023     C  N30              TESTN                   WI07                     30
055200971021     C                   MOVE      WI07          W001A             1
055300971021     C     *IN30         IFEQ      *OFF
055400971021     C     W001A         ORLT      '0'
055500971023     C     W001A         ANDNE     *BLANK
055600971021     C                   SETON                                        32
055700971021     C                   MOVE      *ZERO         VABVLB
055800010604     C                   eval      vinmsg = %trimr(vinmsg)
055900010604     C                             + ' ' + 'VHBVLB'
056000971021     C                   ELSE
056100971021     C                   MOVEL     WI07          VABVLB
056200971021     C                   ENDIF
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
057700941229     C*
057800971021     C                   MOVE      *ZERO         VABQFT
057900971021     C                   MOVE      VHBQFT        WASC15
058000971023     C                   TESTN                   WI15                 30
058100971023     C  N30              TESTN                   WI15                   30
058200971023     C  N30              TESTN                   WI15                     30
058300971021     C                   MOVE      WI15          W001A             1
058400971021     C     *IN30         IFEQ      *OFF
058500971021     C     W001A         ORLT      '0'
058600971023     C     W001A         ANDNE     *BLANK
058700971021     C                   SETON                                        32
058800971021     C                   MOVE      *ZERO         VABQFT
058900010604     C                   eval      vinmsg = %trimr(vinmsg)
059000010604     C                             + ' ' + 'VHBQFT'
059100971021     C                   ELSE
059200971021     C                   MOVEL     WI15          VABQFT
059300971021     C                   ENDIF
059400971023     C                   TESTN                   WD15                 30
059500971023     C  N30              TESTN                   WD15                   30
059600971023     C  N30              TESTN                   WD15                     30
059700971021     C                   MOVE      WD15          W001A             1
059800971021     C     *IN30         IFEQ      *OFF
059900971021     C     W001A         ORLT      '0'
060000971023     C     W001A         ANDNE     *BLANK
060100971021     C                   SETON                                        32
060200971021     C                   MOVE      *ZERO         VABQFT
060300010604     C                   eval      vinmsg = %trimr(vinmsg)
060400010604     C                             + ' ' + 'VHBQFT'
060500971021     C                   ELSE
060600971021     C                   MOVE      WD15          VABQFT
060700971021     C                   ENDIF
060800941229     C*
060900971021     C                   MOVE      *ZERO         VABCAS
061000971021     C                   MOVE      VHBCAS        WASC15
061100971023     C                   TESTN                   WI15                 30
061200971023     C  N30              TESTN                   WI15                   30
061300971023     C  N30              TESTN                   WI15                     30
061400971021     C                   MOVE      WI15          W001A             1
061500971021     C     *IN30         IFEQ      *OFF
061600971021     C     W001A         ORLT      '0'
061700971023     C     W001A         ANDNE     *BLANK
061800971021     C                   SETON                                        32
061900971021     C                   MOVE      *ZERO         VABCAS
062000010604     C                   eval      vinmsg = %trimr(vinmsg)
062100010604     C                             + ' ' + 'VHBCAS'
062200971021     C                   ELSE
062300971021     C                   MOVEL     WI15          VABCAS
062400971021     C                   ENDIF
062500971023     C                   TESTN                   WD15                 30
062600971023     C  N30              TESTN                   WD15                   30
062700971023     C  N30              TESTN                   WD15                     30
062800971021     C                   MOVE      WD15          W001A             1
062900971021     C     *IN30         IFEQ      *OFF
063000971021     C     W001A         ORLT      '0'
063100971023     C     W001A         ANDNE     *BLANK
063200971021     C                   SETON                                        32
063300971021     C                   MOVE      *ZERO         VABCAS
063400010604     C                   eval      vinmsg = %trimr(vinmsg)
063500010604     C                             + ' ' + 'VHBCAS'
063600971021     C                   ELSE
063700971021     C                   MOVE      WD15          VABCAS
063800971021     C                   ENDIF
063900971021     C*
064000971023     C                   TESTN                   VHBRMN               30
064100971023     C  N30              TESTN                   VHBRMN                 30
064200971023     C  N30              TESTN                   VHBRMN                   30
064300971021     C                   MOVE      VHBRMN        W001A             1
064400971021     C     *IN30         IFEQ      *OFF
064500971021     C     W001A         ORLT      '0'
064600971021     C                   SETON                                        32
064700971021     C                   MOVE      *ZERO         VABRMN
064800010604     C                   eval      vinmsg = %trimr(vinmsg)
064900010604     C                             + ' ' + 'VHBRMN'
065000971021     C                   ELSE
065100971021     C                   MOVE      VHBRMN        VABRMN
065200971021     C                   ENDIF
065300971021     C*
065400971023     C                   TESTN                   VHBNCD               30
065500971023     C  N30              TESTN                   VHBNCD                 30
065600971023     C  N30              TESTN                   VHBNCD                   30
065700971021     C                   MOVE      VHBNCD        W001A             1
065800971021     C     *IN30         IFEQ      *OFF
065900971021     C     W001A         ORLT      '0'
066000971023     C     W001A         ANDNE     *BLANK
066100971021     C                   SETON                                        32
066200971021     C                   MOVE      *ZERO         VABNCD
066300010604     C                   eval      vinmsg = %trimr(vinmsg)
066400010604     C                             + ' ' + 'VHBNCD'
066500971021     C                   ELSE
066600971021     C                   MOVE      VHBNCD        VABNCD
066700971021     C                   ENDIF
066800971021     C*
066900971023     C                   TESTN                   VHBNCA               30
067000971023     C  N30              TESTN                   VHBNCA                 30
067100971023     C  N30              TESTN                   VHBNCA                   30
067200971021     C                   MOVE      VHBNCA        W001A             1
067300971021     C     *IN30         IFEQ      *OFF
067400971021     C     W001A         ORLT      '0'
067500971023     C     W001A         ANDNE     *BLANK
067600971021     C                   SETON                                        32
067700971021     C                   MOVE      *ZERO         VABNCA
067800010604     C                   eval      vinmsg = %trimr(vinmsg)
067900010604     C                             + ' ' + 'VHBNCA'
068000971021     C                   ELSE
068100971021     C                   MOVE      VHBNCA        VABNCA
068200971021     C                   ENDIF
068300971021     C*
068400971023     C                   TESTN                   VHBZNC               30
068500971023     C  N30              TESTN                   VHBZNC                 30
068600971023     C  N30              TESTN                   VHBZNC                   30
068700971021     C                   MOVE      VHBZNC        W001A             1
068800971021     C     *IN30         IFEQ      *OFF
068900971021     C     W001A         ORLT      '0'
069000971023     C     W001A         ANDNE     *BLANK
069100971021     C                   SETON                                        32
069200971021     C                   MOVE      *ZERO         VABZNC
069300010604     C                   eval      vinmsg = %trimr(vinmsg)
069400010604     C                             + ' ' + 'VHBZNC'
069500971021     C                   ELSE
069600971021     C                   MOVE      VHBZNC        VABZNC
069700971021     C                   ENDIF
069800971021     C*
069900971023     C                   TESTN                   VHBDCR               30
070000971023     C  N30              TESTN                   VHBDCR                 30
070100971023     C  N30              TESTN                   VHBDCR                   30
070200971021     C                   MOVE      VHBDCR        W001A             1
070300971021     C     *IN30         IFEQ      *OFF
070400971021     C     W001A         ORLT      '0'
070500971023     C     W001A         ANDNE     *BLANK
070600971021     C                   SETON                                        32
070700971021     C                   MOVE      *ZERO         VABDCR
070800010604     C                   eval      vinmsg = %trimr(vinmsg)
070900010604     C                             + ' ' + 'VHBDCR'
071000971021     C                   ELSE
071100971021     C                   MOVE      VHBDCR        VABDCR
071200971021     C                   ENDIF
071300971021     C*
071400971023     C                   TESTN                   VHBHCR               30
071500971023     C  N30              TESTN                   VHBHCR                 30
071600971023     C  N30              TESTN                   VHBHCR                   30
071700971021     C                   MOVE      VHBHCR        W001A             1
071800971021     C     *IN30         IFEQ      *OFF
071900971021     C     W001A         ORLT      '0'
072000971023     C     W001A         ANDNE     *BLANK
072100971021     C                   SETON                                        32
072200971021     C                   MOVE      *ZERO         VABHCR
072300010604     C                   eval      vinmsg = %trimr(vinmsg)
072400010604     C                             + ' ' + 'VHBHCR'
072500971021     C                   ELSE
072600971021     C                   MOVE      VHBHCR        VABHCR
072700971021     C                   ENDIF
072800941229     C*
072900971021     C                   MOVE      *ZERO         VABVMD
073000971021     C                   MOVE      VHBVMD        WASC15
073100971023     C                   TESTN                   WI15                 30
073200971023     C  N30              TESTN                   WI15                   30
073300971023     C  N30              TESTN                   WI15                     30
073400971021     C                   MOVE      WI15          W001A             1
073500971021     C     *IN30         IFEQ      *OFF
073600971021     C     W001A         ORLT      '0'
073700971023     C     W001A         ANDNE     *BLANK
073800971021     C                   SETON                                        32
073900971021     C                   MOVE      *ZERO         VABVMD
074000010604     C                   eval      vinmsg = %trimr(vinmsg)
074100010604     C                             + ' ' + 'VHBVMD'
074200971021     C                   ELSE
074300971021     C                   MOVEL     WI15          VABVMD
074400971021     C                   ENDIF
074500971023     C                   TESTN                   WD15                 30
074600971023     C  N30              TESTN                   WD15                   30
074700971023     C  N30              TESTN                   WD15                     30
074800971021     C                   MOVE      WD15          W001A             1
074900971021     C     *IN30         IFEQ      *OFF
075000971021     C     W001A         ORLT      '0'
075100971023     C     W001A         ANDNE     *BLANK
075200971021     C                   SETON                                        32
075300971021     C                   MOVE      *ZERO         VABVMD
075400010604     C                   eval      vinmsg = %trimr(vinmsg)
075500010604     C                             + ' ' + 'VHBVMD'
075600971021     C                   ELSE
075700971021     C                   MOVE      WD15          VABVMD
075800971021     C                   ENDIF
075900971021     C*
076000971023     C                   TESTN                   VHBANT               30
076100971023     C  N30              TESTN                   VHBANT                 30
076200971023     C  N30              TESTN                   VHBANT                   30
076300971021     C                   MOVE      VHBANT        W001A             1
076400971021     C     *IN30         IFEQ      *OFF
076500971021     C     W001A         ORLT      '0'
076600971023     C     W001A         ANDNE     *BLANK
076700971021     C                   SETON                                        32
076800971021     C                   MOVE      *ZERO         VABANT
076900010604     C                   eval      vinmsg = %trimr(vinmsg)
077000010604     C                             + ' ' + 'VHBANT'
077100971021     C                   ELSE
077200971021     C                   MOVE      VHBANT        VABANT
077300971021     C                   ENDIF
077400971021     C*
077500971021     C* CAMPI ALFABETICI:
077600971021     C                   MOVE      VHBATB        VABATB
077700971021     C                   MOVE      VHBCBO        VABCBO
077800971021     C                   MOVE      VHBRSD        VABRSD
077900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
078000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
078100020117     C* ==
078200971021     C                   MOVE      VHBRD2        VABRD2
078300971021     C                   MOVE      VHBIND        VABIND
078400971021     C                   MOVE      VHBCAD        VABCAD
078500971021     C                   MOVE      VHBLOD        VABLOD
078600971021     C                   MOVE      VHBPRD        VABPRD
078700971021     C                   MOVE      VHBNZD        VABNZD
078800971021     C                   MOVE      VHBGC1        VABGC1
078900971021     C                   MOVE      VHBGC2        VABGC2
079000971021     C                   MOVE      VHBTSP        VABTSP
079100971021     C                   MOVE      VHBVAS        VABVAS
079200971021     C                   MOVE      VHBNAS        VABNAS
079300971021     C                   MOVE      VHBTIC        VABTIC
079400971021     C                   MOVE      VHBVCA        VABVCA
079500971021     C                   MOVE      VHBGCA        VABGCA
079600971021     C                   MOVE      VHBRMA        VABRMA
079700971021     C                   MOVE      VHBXCO        VABXCO
079800971021     C                   MOVE      VHBNOT        VABNOT
079900971021     C                   MOVE      VHBNT2        VABNT2
080000971021     C                   MOVE      VHBCTM        VABCTM
080100971021     C                   MOVE      VHBFFD        VABFFD
080200971021     C                   MOVE      VHBTCR        VABTCR
080300971021     C                   MOVE      VHBCTS        VABCTS
080400971021     C                   MOVE      VHBFTM        VABFTM
080500971021     C                   MOVE      VHBVAD        VABVAD
080600971021     C                   MOVE      VHBGMA        VABGMA
080700971021     C                   MOVE      VHBGGA        VABGGA
080800971021     C                   MOVE      VHBGVA        VABGVA
080900971021     C                   MOVE      VHBTC1        VABTC1
081000971021     C                   MOVE      VHBTC2        VABTC2
081100971021     C                   MOVE      VHBSCL        VABSCL
081200971021     C                   MOVE      VHBRMO        VABRMO
081300971021     C                   MOVE      VHBCMO        VABCMO
081400971021     C                   MOVE      VHBNMO        VABNMO
081500971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
081600971021     C* NELLA NAZIONE ORIGINALE
081700971021     C                   MOVE      VHBNMO        WA1D              1
081800971021     C                   MOVEL     VHBNMO        WA1S              1
081900971021     C     WA1D          IFNE      *BLANK
082000971021     C     WA1S          ANDEQ     *BLANK
082100971021     C                   MOVE      *BLANK        VABNMO
082200031229     C                   END
082300031229     C*
082400031229     C* Se gestione particolarità consegna nn è valorizzato lo forzo ad "NR"
082500031229     C                   IF        vabGMA = *blanks
082600031229     C                   EVAL      vabGMA = 'NR'
082700031229     C                   ENDIF
082800930409     C*
082900010604     C  N31              ADD       1             §CTROK            7 0
083000010604     C   32              ADD       1             §CTRMO            7 0
083100010604     C   31              ADD       1             §CTRNO            7 0
083200020916     C  N31              WRITE     FIVAB000
083300030811     C*
083400030811     C* Eseguo operazioni particolari x LOTTOMATICA x gestire ritiro contestuale alla consegna
083500030811     C                   if        *IN31 = *OFF AND
083600030811     C                             vabGMA = 'RC'
083700030915     C                   eval      flgORM = 'S'
083800030811     C                   exsr      wriORM
083900030811     C                   exsr      wriORF
084000070709     C                   exsr      wriORG
084100030811     C                   exsr      wriORI
084200050826     C                   exsr      wriVGD
084300030811     C                   endif
084400990910     C*
084500010601     C                   if        *in31 = *off and
084600010601     C                             *in32 = *off
084700990910     C                   eval      vinflg = '1'
084800990910     C                   else
084900000621     C                   eval      recko = vindta
085000030811     C                   if        %open(titv65ps)
085100030407     C   50              except    rigadett
085200010612     C                   endif
085300990910     C                   eval      vinflg = '2'
085400990910     C                   endif
085500000905     C                   endif
085600000905     C                   endif
085700010601     C*
085800010601     C                   endif
085900000905     C*
086000000905     C                   else
086100000905     C                   eval      vinflg = '1'
086200000905     C                   endif
086300000905     C*
086400000905     C  N70              update    tivin000
086500000905     C*
086600991022     C  N70              ENDdo
086700010601     C
086800010601     C*
086900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
087000010601     C                   if        cntNonEl = *zeros or
087100010601     C                             flgMulti = '0'
087200990910     C* Se non ci sono record con errori ...
087300010601     C                   if        §ctrno = 0 and
087400010601     C                             §ctrmo = 0 and
087500010601     C                             flgStato <> '2'
087600990910     C* ... restituisco esito OK.
087700990921     C                   eval      wrkesito = '0'
087800990910     C                   else
087900010601     C                   if        §ctrok > 0
088000000710     C                   eval      wrkesito = '1'
088100000710     C                   else
088200010615     C                   if        flgOk = '0'
088300010615     C                   eval      wrkesito = '2'
088400010615     C                   else
088500010615     C                   eval      wrkesito = '6'
088600010615     C                   endif
088700000710     C                   endif
088800010615     C                   endif
088900010601     C                   else
089000010601     C                   eval      wrkesito = '9'
089100010601     C                   endif
089200990910     C*
089300990914     C                   if        %open(tivin00r)
089400990908     C                   close     tivin00r
089500990914     C                   endif
089600020916     C                   if        %open(fivabwwr)
089700020916     C                   close     fivabwwr
089800990914     C                   endif
089900010601     C*
090000010601     C                   if        vlrpoi <> 999
090100010601     C                   eval      invfil = vlrpoi
090200010601     C                   endif
090300990910     C*
090400990920     C                   if        §ctrok > 0
090500010601     C                             and invfil > *zeros
090600990920     C                   exsr      invio
090700040910     C**!!!              if        flgORM = 'S'
090800040910     C**!!!              exsr      invioORM
090900040910     C**!!!              endif
091000990920     C                   endif
091100010612     C*
091200010612     C                   if        flgGiro = '1'
091300010612     C                   exsr      endela
091400010612     C                   endif
091500990920     C*
091600910830     C                   ENDSR
091700000613     C***
091800030811
091900030811
092000030811
092100030811      /TITLE Scrittura record in FNORM00F
092200030811     C     wriORM        BEGSR
092300030811     C*
092400030811     C                   clear                   FNORM000
092500030811     C* Valorizzo buffer record
092600030811     C                   eval      ormATB = *blanks
092700030811     C                   eval      ormPOE = 001
092800030811     C                   eval      ormNSR = 70
092900030811     C* Stacco un numeratore da AZNUM
093000030811     C                   clear                   TRUL33DS
093100030811     C                   eval      I33OPE = *zeros
093200030811     C                   eval      I33PO1 = 046
093300030811     C                   eval      I33CNU = 300
093400030811     C                   eval      I33NUM = 1
093500030811     C                   movel     TRUL33DS      KPJBU
093600030811     C                   call      'TRUL33R'
093700030811     C                   parm                    KPJBA
093800030811     C                   movel     KPJBU         TRUL33DS
093900030811     C                   if        O33ERR = *zeros
094000030811     C                   eval      ormNOR = O33NRF
094100030811     C                   else
094200030811     C                   eval      ormNOR = *all'9'
094300030811     C                   endif
094400030811     C*
094500030811     C                   eval      ormNRV = *zeros
094600030811     C                   eval      ormTOR = 'S'
094700030811     C                   eval      ormTCO = 'F'
094800030811     C                   eval      ormDAO = datcor
094900030811     C                   eval      ormOAO = oracor
095000030811     C                   eval      ormCOR = vabCCM*1000+333
095100030811     C* Reperisco i dati anagrafici (dall'anagrafica ritiri) del codice ORDINANTE
095200030811     C     ormCOR        chain     fnacr01l
095300030811     C                   if        %found(fnacr01l)
095400030811     C                   eval      ormRSO = acrRSR
095500030811     C                   eval      ormINO = acrINR
095600030811     C                   eval      ormCAO = acrCAR
095700030811     C                   eval      ormLOO = acrLOR
095800030811     C                   eval      ormPRO = acrPRR
095900030811     C                   eval      ormNAO = acrNAR
096000030811     C                   eval      ormNAM = acrNTM
096100030811     C                   else
096200030811     C                   eval      ormRSO = *all'*'
096300030811     C                   eval      ormINO = *all'*'
096400030811     C                   eval      ormCAO = *all'0'
096500030811     C                   eval      ormLOO = *all'*'
096600030811     C                   eval      ormPRO = *all'*'
096700030811     C                   eval      ormNAO = *all'*'
096800030811     C                   eval      ormNAM = *all'*'
096900030811     C                   endif
097000030811     C                   eval      ormCRA = *zeros
097100030811     C                   eval      ormRSR = vabRSD
097200030811     C                   eval      ormINR = vabIND
097300030811     C                   eval      ormCAR = vabCAD
097400030811     C                   eval      ormLOR = vabLOD
097500030811     C                   eval      ormPRR = vabPRD
097600030811     C                   eval      ormNAR = vabNZD
097700030909     C                   eval      ormRER = 'ORM CONTESTUALE CONSEGNA'
097800030909     C                   eval      ormTER = 'ORM CONT. CONS.'
097900051206     C*** OLD            eval      ormCRC = 0632740000
098000051206     C                   eval      ormCRC = 0632740444
098100030811     C* Reperisco i dati anagrafici (dall'anagrafica ritiri) del codice CONSEGNA
098200030811     C     ormCRC        chain     fnacr01l
098300030811     C                   if        %found(fnacr01l)
098400030811     C                   eval      ormRSC = acrRSR
098500030811     C                   eval      ormINC = acrINR
098600030811     C                   eval      ormCAC = acrCAR
098700030811     C                   eval      ormLOC = acrLOR
098800030811     C                   eval      ormPRC = acrPRR
098900030811     C                   eval      ormNAC = acrNAR
099000030811     C                   else
099100030811     C                   eval      ormRSC = *all'*'
099200030811     C                   eval      ormINC = *all'*'
099300030811     C                   eval      ormCAC = *all'0'
099400030811     C                   eval      ormLOC = *all'*'
099500030811     C                   eval      ormPRC = *all'*'
099600030811     C                   eval      ormNAC = *all'*'
099700030811     C                   endif
099800030811     C                   eval      ormFFD = 'N'
099900030811     C* Calcolo la data ritiro richiesta aggiungendo alla data aperura ORM 1 gg. lavorativo
100000030811     C* (tramite TRUL13R)
100100030811     C                   z-add     1100          wOra              4 0
100200030811     C                   clear                   TRUL13DS
100300030811     C                   eval      I13MOD = 'P'
100400030811     C                   eval      I13FIL = ormPOE
100500030811     C                   eval      I13DIN = ormDAO
100600030811     C                   eval      I13HIN = wOra
100700030811     C                   eval      I13GIO = 1
100800030811     C                   eval      I13ORE = *zeros
100900030811     C                   call      'TRUL13R'
101000030811     C                   parm                    TRUL13DS
101100030811     C                   if        O13ERR = *blanks
101200030811     C                   eval      ormDAR = O13DFI
101300030811     C                   else
101400030811     C                   eval      ormDAR = ormDAO
101500030811     C                   endif
101600030811     C*
101700030811     C                   eval      ormORR = wORA
101800030811     C                   eval      ormRMP = *blanks
101900030811     C                   eval      ormNCL = 1
102000030811     C                   eval      ormPKG = 1
102100030811     C                   eval      ormVLM = *zeros
102200030811     C                   eval      ormBNC = *zeros
102300030811     C                   eval      ormBLC = *zeros
102400030811     C                   eval      ormATT = *zeros
102500030811     C                   eval      ormMTC = *zeros
102600030811     C                   eval      ormPAG = 'D'
102700030811     C                   eval      ormKSC = 0632740
102800030811     C                   eval      ormCTR = '000'
102900030811     C                   eval      ormPOR = vabLNA
103000030811     C                   movel     vabZNC        ormZOR
103100030811     C* Reperisco P.O. Consegna tramite TISI95R richiamato con i dati instradamento del destinatario
103200030811     C                   clear                   TISI95DS
103300030811     C                   eval      I95TCN = '3'
103400030811     C                   eval      I95NAR = ormNAC
103500030811     C                   eval      I95CAP = ormCAC
103600030811     C                   eval      I95LOC = ormLOC
103700030811     C                   eval      I95PRV = ormPRC
103800030811     C                   eval      I95LKG = ormPKG
103900030811     C                   call      'TISI95R'
104000030811     C                   parm                    TISI95DS
104100030811     C                   if        O95ERR = *blanks
104200030811     C                   eval      ormPOC = O95LNA
104300030811     C                   else
104400030811     C                   eval      ormPOC = *zeros
104500030811     C                   endif
104600060113     C*
104700060113     C* Eseguo controllo x eventuale destinatario presente in anagrafiuco dest. particolari
104800060113     C* x forzatura linea/zona instradamento prestabilito
104900060113     C                   clear                   tisit0ds
105000060113     C                   movel     'E'           it0TLA
105100060113     C                   movel     ormNAC        it0NAZ
105200060113     C                   movel     ormPRC        it0PRV
105300060113     C                   movel     ormCAC        it0CAP
105400060113     C                   movel     ormLOC        it0LOC
105500060113     C                   movel     ormINC        it0IND
105600060113     C                   movel     ormRSC        it0RAG
105700060113     C                   call      'TISIT5R'
105800060113     C                   parm                    tisit0ds
105900060113     C*
106000060113     C* Se trovato destinatario tra i dest. particolari verifico se presenti linea/zona
106100060113     C* instradamento prestabiliti
106200060113     C                   if        ot0ERR = *blanks
106300060113     C                   eval      DDSTFLO = ot0FLO
106400060113     C                   if        §DSTLNA <> *blanks
106500060113     C                   move(p)   §DSTLNA       ormPOC
106600060113     C                   endif
106700060113     C                   endif
106800030811     C*
106900030811     C                   eval      ormDDT = 'N'
107000030811     C                   eval      ormCST = *zeros
107100030811     C                   eval      ormVCS = *blanks
107200030811     C                   eval      ormFCS = *blanks
107300030811     C                   eval      ormDFO = datcor
107400030811     C                   eval      ormOFO = oracor
107500030811     C                   eval      ormFAO = 100
107600030811     C                   eval      ormSTO = *zeros
107700030811     C                   eval      ormDST = datcor
107800030811     C                   eval      ormNPG = *zeros
107900030811     C                   eval      ormNDC = *zeros
108000030811     C                   eval      ormDDC = *zeros
108100030811     C                   eval      ormSTP = *zeros
108200030811     C                   eval      ormPOS = *zeros
108300030811     C                   eval      ormORS = *zeros
108400030811     C                   eval      ormRFA = 'B' + %subst(vabKEY:3:14)
108500030811     C                   eval      ormTAP = *blanks
108600070709     C**!!!              eval      ormETI = *blanks
108700030811     C                   eval      ormSPI = *blanks
108800070709
108900070709      * imposto il campo ORMETI (falg orm in fase assegnabile) in base alla fase
109000070709      * impostata in precedenza
109100070709     c                   movel(p)  'FAR'         KtbCOD
109200070709     c                   movel(p)  ormfao        KtbKE1
109300070709     c     KEYtbe        chain     tntbe01l
109400070709     c                   if        %found(tntbe01l)
109500070709     c                   movel(P)  tbeuni        DFAR
109600070709     c                   else
109700070709     c                   clear                   DFAR
109800070709     c                   endif
109900070709     c                   eval      ormeti = d§farass
110000070709
110100030811     C* Valorizzo il campo dei FLag Operativi secondo la DS DORM01
110200030811     C                   clear                   DORM01
110300030811     C                   eval      §ormCB = *blanks
110400030811     C                   eval      §ormFR = *blanks
110500030811     C                   eval      §ormKS = 'S'
110600030811     C                   eval      §ormFD = 'S'
110700030811     C                   eval      §ormIC = *blanks
110800030811     C                   eval      §orCOM = *blanks
110900030811     C                   eval      §orPFB = 'A'
111000030811     C                   eval      §ormXX = *blanks
111100090618
111200090618     C* stacco un numeratore da AZNUM
111300090618     C                   clear                   trul33ds
111400090618     C                   eval      I33OPE = *zeros
111500090618     C                   eval      I33CNU = 600
111600090618     C                   eval      I33NUM = 1
111700090618     C                   movel     TRUL33DS      KPJBU
111800090618     C                   call      'TRUL33R'
111900090618     C                   parm                    KPJBA
112000090618     C                   movel     KPJBU         TRUL33DS
112100090618     C                   if        O33ERR = *zeros
112200090618     C                   z-add     O33NRF        §numori
112300090618     c                   else
112400090618     C                   eval      §numori = *all'9'
112500090618     C                   endif
112600030811     C                   movel     §numori       §ormPG
112700030811     C*
112800030811     C                   eval      ormFLO = DORM01
112900040910     C                   eval      ormDTT = datcor
113000030811     C*
113100030811     C* Scarico il buffer su file
113200030811     C                   write     FNORM000
113300030811     C*
113400030811     C                   ENDSR
113500030811     C***
113600030811
113700030811
113800030811
113900030811      /TITLE Scrittura record in FNORF00F
114000030811     C     wriORF        BEGSR
114100030811     C*
114200030811     C                   clear                   FNORF000
114300030811     C* Valorizzo buffer record
114400030811     C                   eval      orfATB = *blanks
114500030811     C                   eval      orfPOE = ormPOE
114600030811     C                   eval      orfNSR = ormNSR
114700030811     C                   eval      orfNOR = ormNOR
114800030811     C                   eval      orfNRV = ormNRV
114900030811     C                   eval      orfPOG = ormPOE
115000030811     C                   eval      orfDAE = datcor
115100030811     C                   eval      orfORE = oracor
115200030811     C                   eval      orfFAR = ormFAO
115300030811     C                   eval      orfCAR = *blanks
115400030811     C                   eval      orfPUE = 'BATCH'
115500030811     C*
115600030811     C* Scarico il buffer su file
115700030811     C                   write     FNORF000
115800030811     C*
115900030811     C                   ENDSR
116000030811     C***
116100070709
116200070709
116300070709
116400070709      /TITLE Scrittura record in FNORG00F
116500070709     C     wriORG        BEGSR
116600070709     C*
116700070709     C                   clear                   FNORG000
116800070709     C* Valorizzo buffer record
116900070709     C                   eval      orgATB = *blanks
117000070709     C                   eval      orgPOE = ormPOE
117100070709     C                   eval      orgNSR = ormNSR
117200070709     C                   eval      orgNOR = ormNOR
117300070709     C                   eval      orgNRV = ormNRV
117400070709     C                   eval      orgPOR = ormPOR
117500070709      * imposto il peso = a quello dell'ORM
117600070709     c                   eval      orgPKG = ormPKG
117700070709      * imposto il volume in base al rapporto peso/volume
117800070709     c                   eval(h)   orgVLM = ormPKG / 200
117900070709     C*
118000070709     C* Scarico il buffer su file
118100070709     C                   write     FNORG000
118200070709     C*
118300070709     C                   ENDSR
118400030811
118500030811
118600030811
118700030811      /TITLE Scrittura record in TIORI00F
118800030811     C     wriORI        BEGSR
118900030811     C*
119000030811     C                   clear                   TIORI000
119100030811     C* Valorizzo buffer record
119200030811     C                   movel     §ormPG        oriPRG
119300030811     C                   eval      oriDER = datcor
119400030811     C                   eval      oriOER = oracor
119500030811     C                   eval      oriIDC = vlrKSC
119600030811     C                   eval      oriPOE = ormPOE
119700030811     C                   eval      oriNSR = ormNSR
119800030811     C                   eval      oriNOR = ormNOR
119900030811     C                   eval      oriNRV = ormNRV
120000030811     C                   eval      oriFLO = *blanks
120100030811     C                   eval      vaoATB = ormATB
120200030811     C                   eval      vaoPOE = ormPOE
120300030811     C                   eval      vaoNSR = ormNSR
120400030811     C                   eval      vaoNOR = ormNOR
120500030811     C                   eval      vaoNRV = ormNRV
120600030811     C                   eval      vaoTOR = ormTOR
120700030811     C                   eval      vaoTCO = ormTCO
120800030811     C                   eval      vaoDAO = ormDAO
120900030811     C                   eval      vaoOAO = ormOAO
121000030811     C                   eval      vaoCOR = ormCOR
121100030811     C                   eval      vaoRSO = ormRSO
121200030811     C                   eval      vaoINO = ormINO
121300030811     C                   eval      vaoCAO = ormCAO
121400030811     C                   eval      vaoLOO = ormLOO
121500030811     C                   eval      vaoPRO = ormPRO
121600030811     C                   eval      vaoNAO = ormNAO
121700030811     C                   eval      vaoCRA = ormCRA
121800030811     C                   eval      vaoRSR = ormRSR
121900030811     C                   eval      vaoINR = ormINR
122000030811     C                   eval      vaoCAR = ormCAR
122100030811     C                   eval      vaoLOR = ormLOR
122200030811     C                   eval      vaoPRR = ormPRR
122300030811     C                   eval      vaoNAR = ormNAR
122400030811     C                   eval      vaoRER = ormRER
122500030811     C                   eval      vaoTER = ormTER
122600030811     C                   eval      vaoCRC = ormCRC
122700030811     C                   eval      vaoRSC = ormRSC
122800030811     C                   eval      vaoINC = ormINC
122900030811     C                   eval      vaoCAC = ormCAC
123000030811     C                   eval      vaoLOC = ormLOC
123100030811     C                   eval      vaoPRC = ormPRC
123200030811     C                   eval      vaoNAC = ormNAC
123300030811     C                   eval      vaoFFD = ormFFD
123400030811     C                   eval      vaoDAR = ormDAR
123500030811     C                   eval      vaoORR = ormORR
123600030811     C                   eval      vaoRMP = ormRMP
123700030811     C                   eval      vaoNAM = ormNAM
123800030811     C                   eval      vaoNCL = ormNCL
123900030811     C                   eval      vaoPKG = ormPKG
124000030811     C                   eval      vaoVLM = ormVLM
124100030811     C                   eval      vaoBNC = ormBNC
124200030811     C                   eval      vaoBLC = ormBLC
124300030811     C                   eval      vaoATT = ormATT
124400030811     C                   eval      vaoMTC = ormMTC
124500030811     C                   eval      vaoPAG = ormPAG
124600030811     C                   eval      vaoKSC = ormKSC
124700030811     C                   eval      vaoCTR = ormCTR
124800030811     C                   eval      vaoPOR = ormPOR
124900030811     C                   eval      vaoZOR = ormZOR
125000030811     C                   eval      vaoPOC = ormPOC
125100030811     C                   eval      vaoNO1 = ormNO1
125200030811     C                   eval      vaoNO2 = ormNO2
125300030811     C                   eval      vaoDDT = ormDDT
125400030811     C                   eval      vaoRFA = ormRFA
125500030811     C                   eval      vaoSTO = ormSTO
125600030811     C                   eval      vaoSPI = ormSPI
125700030811     C                   eval      vaoFLO = ormFLO
125800030811     C*
125900030811     C* Scarico il buffer su file
126000030811     C                   write     TIORI000
126100030811     C*
126200030811     C                   ENDSR
126300030811     C***
126400050826
126500050826
126600050826
126700050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
126800050826     C     wriVGD        BEGSR
126900050826     C*
127000050826     C* Reperisco la descrizione della fase da tabella
127100050826     C                   movel(p)  'FAR'         KtbCOD
127200050826     C                   movel(p)  '000'         KtbKE1
127300050826     C     KEYtbe        chain     tntbe01l
127400050826     C                   if        %found(tntbe01l)
127500050826     C                   movel(P)  tbeuni        DFAR
127600050826     C                   else
127700050826     C                   clear                   DFAR
127800050826     C                   endif
127900050826     C*
128000050826     C* Valorizzo buffer record
128100050826     C                   clear                   FNVAPDS
128200050826     C                   eval      vapIDC = oriIDC
128300050826     C                   eval      vapPOE = oriPOE
128400050826     C                   eval      vapRFA = vaoRFA
128500050826     C                   eval      vapPOG = oriPOE
128600050826     C                   eval      vapDAE = oriDER
128700050826     C                   eval      vapORE = oriOER
128800050826     C                   if        d§fardva <> *blanks
128900050826     C                   eval      vapDFA = d§fardva
129000050826     C                   else
129100050826     C                   eval      vapDFA = d§fardes
129200050826     C                   endif
129300050826     C*
129400050826     C                   clear                   tivgd000
129500080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
129600050826     C                   eval      vgdTIP = 'VP'
129700050826     C                   eval      vgdKSU = vapIDC
129800050826     C                   eval      vgdTSC = 'WW'
129900050826     C                   eval      vgdDAT = datcor
130000050826     C                   eval      vgdPGM = 'TITV65R'
130100050826     C                   write     tivgd000
130200050826     C*
130300050826     C                   ENDSR
130400050826     C*------------------------------------------------------------------------*
130500050907
130600050907
130700050907
130800050907
130900050907      /TITLE "Bonifica" dati bolla da caratteri nn voluti (tramite XCHKCHAR)
131000050907     C     exebon        BEGSR
131100050907     C*
131200050907     C* Imposto i parametri x il *pgm d "bonifica"
131300050907     C                   EVAL      ElencoChar = *blanks
131400050907     C                   EVAL      TipoElenco = *blanks
131500050907     C                   EVAL      CharSost   = *blanks
131600050907     C                   EVAL      UpperCase  = '1'
131700050907     C                   EVAL      ChkNull    = '1'
131800050907     C                   EVAL      CharNull   = *blanks
131900050907     C*
132000050907     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
132100050907     C                   EVAL      TxtInOut   = VABRSD
132200050907     C                   EVAL      EsitoBon   = *blanks
132300050907     C*
132400050907     C                   CALL      'XCHKCHAR'
132500050907     C                   PARM                    TxtInOut
132600050907     C                   PARM                    ElencoChar
132700050907     C                   PARM                    TipoElenco
132800050907     C                   PARM                    CharSost
132900050907     C                   PARM                    UpperCase
133000050907     C                   PARM                    ChkNull
133100050907     C                   PARM                    CharNull
133200050907     C                   PARM                    EsitoBon
133300050907     C*
133400050907     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
133500050907     C                   IF        EsitoBon = '1'
133600050907     C                   EVAL      VABRSD = TxtInOut
133700050907     C                   ENDIF
133800050907     C*
133900050907     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
134000050907     C                   EVAL      TxtInOut   = VABIND
134100050907     C                   EVAL      EsitoBon   = *blanks
134200050907     C*
134300050907     C                   CALL      'XCHKCHAR'
134400050907     C                   PARM                    TxtInOut
134500050907     C                   PARM                    ElencoChar
134600050907     C                   PARM                    TipoElenco
134700050907     C                   PARM                    CharSost
134800050907     C                   PARM                    UpperCase
134900050907     C                   PARM                    ChkNull
135000050907     C                   PARM                    CharNull
135100050907     C                   PARM                    EsitoBon
135200050907     C*
135300050907     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
135400050907     C                   IF        EsitoBon = '1'
135500050907     C                   EVAL      VABIND = TxtInOut
135600050907     C                   ENDIF
135700050907     C*
135800050907     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
135900050907     C                   EVAL      TxtInOut   = VABLOD
136000050907     C                   EVAL      EsitoBon   = *blanks
136100050907     C*
136200050907     C                   CALL      'XCHKCHAR'
136300050907     C                   PARM                    TxtInOut
136400050907     C                   PARM                    ElencoChar
136500050907     C                   PARM                    TipoElenco
136600050907     C                   PARM                    CharSost
136700050907     C                   PARM                    UpperCase
136800050907     C                   PARM                    ChkNull
136900050907     C                   PARM                    CharNull
137000050907     C                   PARM                    EsitoBon
137100050907     C*
137200050907     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
137300050907     C                   IF        EsitoBon = '1'
137400050907     C                   EVAL      VABLOD = TxtInOut
137500050907     C                   ENDIF
137600050907     C*
137700050907     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABPRD
137800050907     C                   EVAL      TxtInOut   = VABPRD
137900050907     C                   EVAL      EsitoBon   = *blanks
138000050907     C                   EVAL      UpperCase  = *blanks
138100050907     C*
138200050907     C                   CALL      'XCHKCHAR'
138300050907     C                   PARM                    TxtInOut
138400050907     C                   PARM                    ElencoChar
138500050907     C                   PARM                    TipoElenco
138600050907     C                   PARM                    CharSost
138700050907     C                   PARM                    UpperCase
138800050907     C                   PARM                    ChkNull
138900050907     C                   PARM                    CharNull
139000050907     C                   PARM                    EsitoBon
139100050907     C*
139200050907     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
139300050907     C                   IF        EsitoBon = '1'
139400050907     C                   EVAL      VABPRD = TxtInOut
139500050907     C                   ENDIF
139600050907     C*
139700050907     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNZD
139800050907     C                   EVAL      TxtInOut   = VABNZD
139900050907     C                   EVAL      EsitoBon   = *blanks
140000050907     C                   EVAL      UpperCase  = *blanks
140100050907     C*
140200050907     C                   CALL      'XCHKCHAR'
140300050907     C                   PARM                    TxtInOut
140400050907     C                   PARM                    ElencoChar
140500050907     C                   PARM                    TipoElenco
140600050907     C                   PARM                    CharSost
140700050907     C                   PARM                    UpperCase
140800050907     C                   PARM                    ChkNull
140900050907     C                   PARM                    CharNull
141000050907     C                   PARM                    EsitoBon
141100050907     C*
141200050907     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
141300050907     C                   IF        EsitoBon = '1'
141400050907     C                   EVAL      VABNZD = TxtInOut
141500050907     C                   ENDIF
141600050907     C*
141700050907     C                   ENDSR
141800050907     C***
141900010601
142000010601
142100010601
142200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
142300010601     C     repfil        BEGSR
142400010601     C*
142500010601     C                   if        invfil = *zeros and
142600010601     C                             depfil > *zeros and
142700010613     C                             (vinflg = *blanks or
142800010613     C                              vinflg = *zeros)
142900010601     C
143000010601     C                   eval      invfil = depfil
143100010601     C                   endif
143200010601     C*
143300010601     C                   if        depfil <> invfil and
143400010601     C                             invfil > *zeros
143500010601     C                   eval      flgMulti = '1'
143600010601     C                   if        vinflg = *blanks
143700010601     C                   add       1             cntNonEl
143800010601     C                   endif
143900010601     C                   endif
144000010601     C*
144100010601     C                   if        vinflg = '2'
144200010601     C                   eval      flgStato = '2'
144300010601     C                   endif
144400010601     C*
144500010601     C                   ENDSR
144600010601     C***
144700010601
144800010601
144900030811
145000010601
145100990920      /TITLE Invio dei dati al punto operativo.
145200000613     C     invio         BEGSR
145300990920     C*
145400990920     C                   reset                   dscmz
145500010601     C                   move      invfil        cmzdst
145600990920     C                   eval      cmzfld = vlrfou
145700990920     C                   eval      cmzmbd = vlrhdl
145800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
145900000710     C                   if        prmfir = *blanks
146000020916     C                   eval      cmzfla = 'FIVAB00F'
146100020916     C                   eval      cmzmba = 'FIVAB00F'
146200000710     C                   else
146300000710     C                   eval      cmzfla = prmfir
146400000710     C                   eval      cmzmba = prmfir
146500000710     C                   endif
146600990920     C                   eval      cmznrr = *zeros
146700990920     C                   move      §ctrok        cmznrr
146800021003     C                   eval      cmzlba = vlrfl1
146900990920     C                   call(e)   'TIS711C'
147000990920     C                   parm                    dscmz
147100990921     C                   parm      *blanks       esito
147200990920     C                   if        %error
147300990920     C                             or cmzerr = '1'
147400990921     C                             or esito  = '1'
147500000710     C                   eval      wrkesito = '3'
147600990920     C                   endif
147700990920     C*
147800000613     C                   ENDSR
147900030915
148000030915
148100030915
148200030915
148300030915      /TITLE Invio ORM
148400040910     C**!!!invioORM      BEGSR
148500030915     C*
148600030915     C* IMPOSTO MANUALMENTE LA KPJBA
148700040910     C**!!!              CLEAR                   KPJBA
148800040910     C**!!!              EVAL      KNMUS = 'EDPCED'
148900040910     C**!!!              EVAL      KNMTD = 'DSP01'
149000040910     C**!!!              EVAL      KSTJO = 'N'
149100040910     C**!!!              EVAL      KTPAZ = 'B'
149200040910     C**!!!              EVAL      KNMEB = 'QPGMR'
149300040910     C**!!!              EVAL      KEXCN = 'J'
149400040910     C**!!!              EVAL      KCOJB = 'P3'
149500040910     C**!!!              EVAL      KCANC = 'N'
149600040910     C**!!!              EVAL      KNSIF = 'GAITRA201'
149700040910     C**!!!              EVAL      KCOAZ = 'OR92'
149800040910     C**!!!              EVAL      KQEBI = 'KSDINO'
149900030915     C*
150000030915     C* SOTTOMISSIONE AZIONE
150100040910     C**!!!              CALL(e)   'BCH10'
150200040910     C**!!!              PARM                    kpjba
150300030915     C*
150400040910     C**!!!              ENDSR
150500010601
150600010601
150700010601
150800010601
150900010601      /TITLE Invio dei dati al punto operativo.
151000010601     C     opeini        BEGSR
151100010601     C*
151200010601     C* Inizializzo flag e contatori operativi
151300010601     C                   movel     '0'           flgGiro           1
151400010601     C                   movel     '0'           flgMulti          1
151500010601     C                   movel     '1'           flgStato          1
151600010615     C                   movel     '0'           flgOk             1
151700010601     C                   z-add     *zeros        cntNonEl         10 0
151800010601     C                   z-add     *zeros        depfil            3 0
151900010601     C                   z-add     *zeros        invfil            3 0
152000030407     C*
152100030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
152200030407     C                   if        %subst(vlrppt:1:1) = 'S'
152300030407     C                   seton                                        50
152400030407     C                   else
152500030407     C                   setoff                                       50
152600030407     C                   endif
152700010601     C*
152800010601     C                   ENDSR
152900010601     C***
153000010601
153100030811
153200030811
153300030811
153400030811      /TITLE Operazioni finali di chiusura *PGM di utilità
153500030811     C     exefin        BEGSR
153600030811     C*
153700030811     C* Chiusura TRUL13R
153800030811     C                   clear                   TRUL13DS
153900030811     C                   eval      I13TLA = 'C'
154000030811     C                   call      'TRUL13R'
154100030811     C                   parm                    TRUL13DS
154200030811     C*
154300030811     C* Chiusura TRUL33R
154400030811     C                   clear                   TRUL33DS
154500030811     C                   eval      I33TLA = 'C'
154600030908     C                   movel     TRUL33DS      KPJBU
154700030811     C                   call      'TRUL33R'
154800030908     C                   parm                    KPJBA
154900030811     C*
155000030811     C* Chiusura TISI95R
155100030811     C                   clear                   TISI95DS
155200030811     C                   eval      I95TLA = 'C'
155300030811     C                   call      'TISI95R'
155400030811     C                   parm                    TISI95DS
155500060113     C*
155600060113     C* Chiusura TISIT5R
155700060113     C                   clear                   TISIT0DS
155800060113     C                   eval      IT0TLA = 'C'
155900060113     C                   call      'TISIT5R'
156000060113     C                   parm                    TISIT0DS
156100030811     C*
156200030811     C                   ENDSR
156300030811     C***
156400010601
156500010601
156600990910
156700000613     C     *inzsr        BEGSR
156800990910     C*
156900990910     C     *entry        plist
157000990920     C                   parm                    tivlrds
157100990921     C                   parm      wrkesito      esito
157200000724     C                   parm                    prmlit
157300000710     C                   parm                    prmfir
157400030811     C*
157500030811     C* DEFINIZIONE CHIAVI
157600030811     C*
157700030811     C* TNTBE01L - parziale
157800030811     C     KEYtbe        klist
157900030811     C                   kfld                    KtbCOD                         *UTENTE
158000030811     C                   kfld                    KtbKE1                         *CODICE
158100010606     C*
158200010606     C* CALCOLA LA DATA CORRENTE
158300010606     C                   time                    wn14             14 0
158400010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
158500030811     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
158600010606     C                   z-add     wn8           g08dat
158700010606     C                   z-add     *zeros        g08inv
158800010606     C                   movel     '0'           g08err
158900010606     C                   call      'XSRDA8'
159000010606     C                   parm                    wlbda8
159100010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
159200000613     C*
159300000613     C                   ENDSR
159400000613     C***
159500990908
159600030811     Otitv65p   E            riepilogo         2
159700990915     O                                              'Upload via Internet'
159800990915     O                                           +1 'Traduzione TIVIN00R -'
159900020916     O                                           +1 'FIVABWWR'
160000990915     O                                           +1 'Testate bolle'
160100990915     O          E            riepilogo   2
160200990915     O                       wrkdata
160300990915     O                       wrkora              +1
160400990915     O                       procname            +1
160500990915     O          E            riepilogo   2
160600990915     O                                              'Cliente..................:'
160700990915     O                       VABCCM        z     +1
160800990915     O          E            riepilogo   2
160900990920     O                                              'Riferimento Strategi.....:'
161000990920     O                       vlrhdl              +1
161100990915     O          E            riepilogo   2
161200990915     O                                              'Giusti...................:'
161300971022     O                       §CTROK        2   +  1
161400990915     O          E            riepilogo   2
161500990915     O                                              'Sbagliati e corretti.....:'
161600971022     O                       §CTRMO        2   +  1
161700990915     O          E            riepilogo   2
161800990915     O                                              'Sbagliati e scartati.....:'
161900971022     O                       §CTRNO        2   +  1
162000000613
162100030811     Otitv65ps  E            testdett          2
162200000613     O                                              'Upload via Internet'
162300000613     O                                           +1 'Traduzione TIVIN00R -'
162400020916     O                                           +1 'FIVABWWR'
162500000613     O                                           +1 'Testate bolle'
162600000616     O          E            testdett    3
162700000613     O                                           +2 'N° rec'
162800000613     O                                           +3 'Anteprima contenuto'
162900000616     O          E            rigadett    2
163000000613     O                       rrnum               +2
163100000621     O                       recko               +3
163200000616     O          E            findett     2
163300000613     O                       wrkdata
163400000613     O                       wrkora              +1
163500000613     O                       procname            +1
163600000616     O          E            findett     2
163700000613     O                                              'Cliente..................:'
163800000613     O                       VABCCM        z     +1
163900000616     O          E            findett     2
164000000613     O                                              'Riferimento Strategi.....:'
164100000613     O                       vlrhdl              +1
164200000616     O          E            findett     2
164300000613     O                                              'Giusti...................:'
164400000613     O                       §CTROK        2   +  1
164500000616     O          E            findett     2
164600000613     O                                              'Sbagliati e corretti.....:'
164700000613     O                       §CTRMO        2   +  1
164800000616     O          E            findett     2
164900000613     O                                              'Sbagliati e scartati.....:'
165000000613     O                       §CTRNO        2   +  1
165100000613** CMD - COMANDI CL
165200030811OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV65P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
165300030811OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV65PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
165400030811DLTOVR FILE(TITV65P TITV65PS) LVL(*)
165500000613
165600000613
