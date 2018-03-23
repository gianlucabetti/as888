000100020916      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020916     FFIVABwwr  O    E             DISK    usropn
000700000613     Ftis7t4p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900000616     Ftis7t4ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000724     D*------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002100941229     D* 10+3
002200941229     D                 DS
002300000313     D  WI15                   1     10
002400000313     D  WD15                  11     13
002500000313     D  WASC15                 1     13
002600941229     D*  6+1
002700941229     D                 DS
002800000313     D  WI09                   1      6
002900000313     D  WD09                   7      7
003000000313     D  WASC09                 1      7
003100941229     D*  2+3
003200941229     D                 DS
003300000313     D  WI07                   1      2
003400000313     D  WD07                   3      5
003500000313     D  WASC07                 1      5
003600000313     D*
003700990920     D tivlrds       e ds                  extname(tivlr00f)
003800041025     D tisi95ds      e ds
003900000313     D*---------------------------------------------------------
004000000313     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
004100000313     D*---------------------------------------------------------
004200000313     D                 ds
004300000313     D vhbatb                  1      1
004400000313     D vhbccm                  2      8
004500000313     D vhblnp                  9     11
004600000313     D vhbaas                 12     15
004700000313     D vhbmgs                 16     19
004800000313     D vhbnrs                 20     21
004900000313     D vhbnsp                 22     28
005000000313     D vhbcbo                 29     30
005100000313     D vhblna                 31     33
005200000313     D vhbrsd                 34     68
005300000313     D vhbrd2                 69    103
005400000313     D vhbind                104    138
005500000313     D vhbcad                139    147
005600000313     D vhblod                148    182
005700000313     D vhbprd                183    184
005800000313     D vhbnzd                185    187
005900000313     D vhbgc1                188    189
006000000313     D vhbgc2                190    191
006100000313     D vhbctr                192    194
006200000313     D vhbtsp                195    195
006300000313     D vhbias                196    208
006400000313     D vhbvas                209    211
006500000313     D vhbnas                212    226
006600000313     D vhbncl                227    231
006700000313     D vhbpkb                232    238
006800000313     D vhbvlb                239    243
006900000313     D vhbqft                244    256
007000000313     D vhbcas                257    269
007100000313     D vhbtic                270    271
007200000313     D vhbvca                272    274
007300000313     D vhbgca                275    276
007400000313     D vhbrmn                277    291
007500000313     D vhbrma                292    306
007600000313     D vhbncd                307    313
007700000313     D vhbnca                314    320
007800000313     D vhbxco                321    321
007900000313     D vhbnot                322    356
008000000313     D vhbnt2                357    391
008100000313     D vhbznc                392    393
008200000313     D vhbctm                394    395
008300000313     D vhbffd                396    396
008400000313     D vhbdcr                397    404
008500000313     D vhbtcr                405    405
008600000313     D vhbhcr                406    409
008700000313     D vhbcts                410    411
008800000313     D vhbftm                412    412
008900000313     D vhbvmd                413    425
009000000313     D vhbvad                426    428
009100000313     D vhbgma                429    430
009200000313     D vhbgga                431    432
009300000313     D vhbgva                433    434
009400000313     D vhbtc1                435    435
009500000313     D vhbtc2                436    436
009600000313     D vhbscl                437    437
009700000313     D vhbant                438    446
009800000313     D vhbrmo                447    471
009900000313     D vhbcmo                472    480
010000000313     D vhbnmo                481    483
010100000313     D  X_rec                  1    483    inz
010200000313     D*
010300990910     D esito           s              1
010400000724     D prmlit          s             10
010500000710     D prmfir          s             10
010600990921     D wrkesito        s                   like(esito)
010700990915     D wrkdata         s               d
010800990915     D wrkora          s               t
010900000613     D rrnum           s              6  0 INZ(*zeros)
011000000621     D recko           s            150    INZ(*blanks)
011100011123     D depcmd          s            150    INZ(*blanks)
011200000313     D*
011300090511
011400090511
011500010606     D*------------------
011600010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
011700010606     D*------------------
011800010606     D WLBDA8          DS                  INZ
011900010606     D  G08DAT                 1      8  0
012000010606     D  G08INV                 9     16  0
012100010606     D  G08ERR                17     17
012200010606     D  G08TGI                18     22  0
012300010606     D*
012400040407     D TxtInOut        S           2048
012500040407     D ElencoChar      S            256
012600040407     D TipoElenco      S              1
012700040407     D CharSost        S              1
012800040407     D UpperCase       S              1
012900040407     D ChkNull         S              1
013000040407     D CharNull        S              1
013100040407     D EsitoBon        S              1
013200010606
013300010606
013400010606
013500010606
013600990915     C                   time                    wrkdata
013700990915     C                   time                    wrkora
013800000913     C                   reset                   rrnum
013900990921     C                   reset                   esito
014000990921     C                   reset                   wrkesito
014100090511     C*
014200010601     C*
014300010601     C                   exsr      opeini
014400010601     C                   exsr      rwvab
014500041025     C*
014600041025     C* Esegue lancio TISI95R solo x chiusura
014700041025     C                   CLEAR                   TISI95DS
014800041025     C                   EVAL      I95TLA = 'C'
014900041025     C                   CALL      'TISI95R'
015000041025     C                   PARM                    TISI95DS
015100010601     C*
015200010601     C                   seton                                        lr
015300010601
015400010601
015500010601
015600010601
015700010601     C*--------------------------------------------------------
015800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
015900010601     C*--------------------------------------------------------
016000010601     C     PREELA        BEGSR
016100010601     C*
016200010601     C* SE OCCORRE SPEDIRE IN FILIALE
016300010601     C                   if        invfil <> *zeros and
016400010601     C                             flgGiro = '0'
016500010601     C*
016600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
016700010601     C                   eval      flgGiro = '1'
016800000724     C*
016900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
017000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
017100010601     C     invfil        chain     azorg01l
017200000724     C                   if        %found
017300000724     C                   movel(p)  CMD(1)        depcmd
017400020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
017500000724     C*
017600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
017700011123     C                   Z-ADD     150           LENGH            15 5
017800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017900000724     C                   PARM                    depcmd
018000000724     C                   PARM                    LENGH
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
019300000613     C*
019400000616     C                   if        not %open(tis7t4ps)
019500000616     C                   open      tis7t4ps
019600030407     C   50              except    testdett
019700000613     C                   endif
019800010601     C*
019900010601     C                   endif
020000010601     C*
020100010601     C                   ENDSR
020200010601     C***
020300010601
020400010601
020500010601
020600010601
020700010601
020800010601
020900010601     C*--------------------------------------------------------
021000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
021100010601     C*--------------------------------------------------------
021200010601     C     ENDELA        BEGSR
021300990915     C*
021400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
021500000613     C*
021600000616     C                   if        %open(tis7t4ps)
021700030407     C   50              except    findett
021800000616     C                   close     tis7t4ps
021900000613     C                   endif
022000000616     C*
022100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
022200011123     C                   Z-ADD     150           LENGH            15 5
022300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
022400000616     C                   PARM                    CMD(3)
022500000616     C                   PARM                    LENGH
022600010601     C*
022700010601     C                   ENDSR
022800010601     C***
022900010601
023000010601
023100000613
023200000613
023300000613
023400000613     C*--------------------------------------------------------
023500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
023600000613     C*--------------------------------------------------------
023700000621     C     STPR          BEGSR
023800000613     C*
023900000613     C                   if        not %open(tis7t4p)
024000000613     C                   open      tis7t4p
024100990915     C                   endif
024200990915     C*
024300030407     C   50              except    riepilogo
024400990915     C*
024500000613     C                   if        %open(tis7t4p)
024600000613     C                   close     tis7t4p
024700990914     C                   endif
024800000613     C*
024900000613     C                   ENDSR
025000000613     C***
025100000613
025200010601
025300010601
025400010601
025500010601
025600910830     C*--------------------------------------------------------
025700020916     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
025800910830     C*--------------------------------------------------------
025900941229     C     RWVAB         BEGSR
026000990910     C*
026100990914     C                   if        not %open(tivin00r)
026200990908     C                   open      tivin00r
026300990914     C                   endif
026400020916     C                   if        not %open(fivabwwr)
026500020916     C                   open      fivabwwr
026600990914     C                   endif
026700990910     C*
026800990910     C                   clear                   §CTROK
026900990910     C                   clear                   §CTRMO
027000990910     C                   clear                   §CTRNO
027100990910     C*
027200921023     C                   DO        *HIVAL
027300990913     C*
027400990915     C                   READ      tivin00r                               70
027500010618     C*
027600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
027700010618     C                   if        vinflg = '1'
027800010618     C                   eval      flgOk = '1'
027900010618     C                   endif
028000010618     C*
028100051017     C                   if        vindta  > *blanks
028200000613     C                   add       1             rrnum
028300990913     C*
028400010601     C                   if        *in70 = *off and
028500990913     C                             (vinflg = *blanks
028600010711     C                              or vinflg = '0'
028700010711     C                              or vinflg = '2')
028800990913     C*
028900020916     C                   clear                   FIVAB000
029000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029100010711     C                   if        vinflg = *blanks or vinflg = '0'
029200010711     C                   clear                   vinmsg
029300010711     C                   endif
029400010711     C*
029500000313     C                   eval      x_rec  = vindta
029600991022     C*
029700991022     C                   testb     x'3F'         vhbatb                   02
029800991022     C                   if        not *in02
029900991022     C*
030000971021     C                   SETOFF                                       303132
030100971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
030200971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
030300971023     C*    OPPURE TUTTI BLANKS
030400010601     C*
030500010601     C                   TESTN                   VHBLNP               30
030600010601     C  N30              TESTN                   VHBLNP                 30
030700010601     C  N30              TESTN                   VHBLNP                   30
030800010601     C                   MOVE      VHBLNP        W001A             1
030900010601     C     *IN30         IFEQ      *OFF
031000010601     C     W001A         ORLT      '0'
031100010601     C                   SETON                                        31
031200010601     C                   eval      vinmsg = %trimr(vinmsg)
031300010601     C                             + ' ' + 'VHBLNP'
031400010601     C                   ELSE
031500010601     C                   MOVE      VHBLNP        VABLNP
031600010601     C     VABLNP        IFLE      *ZEROS
031700010601     C                   SETON                                        31
031800010601     C                   eval      vinmsg = %trimr(vinmsg)
031900010601     C                             + ' ' + 'VHBLNP'
032000010601     C                   ENDIF
032100010601     C                   ENDIF
032200010601     C*
032300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
032400010601     C                   eval      depfil = VABLNP
032500010601     C                   exsr      repfil
032600010601     C                   if        depfil = invfil
032700021014     C                   if        vlrpoi = 999
032800021010     C                   MOVE(P)   invfil        VABFGS
032900021014     C                   else
033000021014     C                   MOVE(P)   vlrpoi        VABFGS
033100021014     C                   endif
033200010601     C*
033300010601     C                   exsr      PREELA
033400010601     C*
033500971023     C                   TESTN                   VHBCCM               30
033600971023     C  N30              TESTN                   VHBCCM                 30
033700971023     C  N30              TESTN                   VHBCCM                   30
033800971021     C                   MOVE      VHBCCM        W001A             1
033900971021     C     *IN30         IFEQ      *OFF
034000971021     C     W001A         ORLT      '0'
034100971021     C                   SETON                                        31
034200010604     C                   eval      vinmsg = %trimr(vinmsg)
034300010604     C                             + ' ' + 'VHBCCM'
034400971021     C                   ELSE
034500971021     C                   MOVE      VHBCCM        VABCCM
034600971024     C     VABCCM        IFLE      *ZEROS
034700971024     C                   SETON                                        31
034800010604     C                   eval      vinmsg = %trimr(vinmsg)
034900010604     C                             + ' ' + 'VHBCCM'
035000971024     C                   ENDIF
035100971021     C                   ENDIF
035200971021     C*
035300971023     C                   TESTN                   VHBAAS               30
035400971023     C  N30              TESTN                   VHBAAS                 30
035500971023     C  N30              TESTN                   VHBAAS                   30
035600971021     C                   MOVE      VHBAAS        W001A             1
035700971021     C     *IN30         IFEQ      *OFF
035800971021     C     W001A         ORLT      '0'
035900971023     C     W001A         ANDNE     *BLANK
036000971021     C                   SETON                                        32
036100010606     C                   MOVEL     datcor        VABAAS
036200010604     C                   eval      vinmsg = %trimr(vinmsg)
036300010604     C                             + ' ' + 'VHBAAS'
036400971021     C                   ELSE
036500971021     C                   MOVE      VHBAAS        VABAAS
036600971021     C                   ENDIF
036700971021     C*
036800971023     C                   TESTN                   VHBMGS               30
036900971023     C  N30              TESTN                   VHBMGS                 30
037000971023     C  N30              TESTN                   VHBMGS                   30
037100971021     C                   MOVE      VHBMGS        W001A             1
037200971021     C     *IN30         IFEQ      *OFF
037300971021     C     W001A         ORLT      '0'
037400971023     C     W001A         ANDNE     *BLANK
037500971021     C                   SETON                                        32
037600010601     C                   MOVE      '0101'        VABMGS
037700010604     C                   eval      vinmsg = %trimr(vinmsg)
037800010604     C                             + ' ' + 'VHBMGS'
037900971021     C                   ELSE
038000971021     C                   MOVE      VHBMGS        VABMGS
038100971021     C                   ENDIF
038200971021     C*
038300971023     C                   TESTN                   VHBNRS               30
038400971023     C  N30              TESTN                   VHBNRS                 30
038500971023     C  N30              TESTN                   VHBNRS                   30
038600971021     C                   MOVE      VHBNRS        W001A             1
038700971021     C     *IN30         IFEQ      *OFF
038800971021     C     W001A         ORLT      '0'
038900971023     C     W001A         ANDNE     *BLANK
039000971021     C                   SETON                                        32
039100971021     C                   MOVE      *ZERO         VABNRS
039200010604     C                   eval      vinmsg = %trimr(vinmsg)
039300010604     C                             + ' ' + 'VHBNRS'
039400971021     C                   ELSE
039500971021     C                   MOVE      VHBNRS        VABNRS
039600971021     C                   ENDIF
039700971021     C*
039800971023     C                   TESTN                   VHBNSP               30
039900971023     C  N30              TESTN                   VHBNSP                 30
040000971023     C  N30              TESTN                   VHBNSP                   30
040100971021     C                   MOVE      VHBNSP        W001A             1
040200971021     C     *IN30         IFEQ      *OFF
040300971021     C     W001A         ORLT      '0'
040400971023     C     W001A         ANDNE     *BLANK
040500971021     C                   SETON                                        32
040600971021     C                   MOVE      *ZERO         VABNSP
040700010604     C                   eval      vinmsg = %trimr(vinmsg)
040800010604     C                             + ' ' + 'VHBNSP'
040900971021     C                   ELSE
041000971021     C                   MOVE      VHBNSP        VABNSP
041100971021     C                   ENDIF
041200971021     C*
041300971023     C                   TESTN                   VHBLNA               30
041400971023     C  N30              TESTN                   VHBLNA                 30
041500971023     C  N30              TESTN                   VHBLNA                   30
041600971021     C                   MOVE      VHBLNA        W001A             1
041700971021     C     *IN30         IFEQ      *OFF
041800971021     C     W001A         ORLT      '0'
041900971023     C     W001A         ANDNE     *BLANK
042000971021     C                   SETON                                        32
042100971021     C                   MOVE      *ZERO         VABLNA
042200010604     C                   eval      vinmsg = %trimr(vinmsg)
042300010604     C                             + ' ' + 'VHBLNA'
042400971021     C                   ELSE
042500971021     C                   MOVE      VHBLNA        VABLNA
042600971021     C                   ENDIF
042700971021     C*
042800971023     C                   TESTN                   VHBCTR               30
042900971023     C  N30              TESTN                   VHBCTR                 30
043000971023     C  N30              TESTN                   VHBCTR                   30
043100971021     C                   MOVE      VHBCTR        W001A             1
043200971021     C     *IN30         IFEQ      *OFF
043300971021     C     W001A         ORLT      '0'
043400971023     C     W001A         ANDNE     *BLANK
043500971021     C                   SETON                                        32
043600971021     C                   MOVE      *ZERO         VABCTR
043700010604     C                   eval      vinmsg = %trimr(vinmsg)
043800010604     C                             + ' ' + 'VHBCTR'
043900971021     C                   ELSE
044000971021     C                   MOVE      VHBCTR        VABCTR
044100971021     C                   ENDIF
044200941229     C*
044300971021     C                   MOVE      *ZERO         VABIAS
044400941229     C                   MOVE      VHBIAS        WASC15
044500971023     C                   TESTN                   WI15                 30
044600971023     C  N30              TESTN                   WI15                   30
044700971023     C  N30              TESTN                   WI15                     30
044800971021     C                   MOVE      WI15          W001A             1
044900971021     C     *IN30         IFEQ      *OFF
045000971021     C     W001A         ORLT      '0'
045100971023     C     W001A         ANDNE     *BLANK
045200971021     C                   SETON                                        32
045300971021     C                   MOVE      *ZERO         VABIAS
045400010604     C                   eval      vinmsg = %trimr(vinmsg)
045500010604     C                             + ' ' + 'VHBIAS'
045600971021     C                   ELSE
045700971021     C                   MOVEL     WI15          VABIAS
045800971023     C                   TESTN                   WD15                 30
045900971023     C  N30              TESTN                   WD15                   30
046000971023     C  N30              TESTN                   WD15                     30
046100971021     C                   MOVE      WD15          W001A             1
046200971021     C     *IN30         IFEQ      *OFF
046300971021     C     W001A         ORLT      '0'
046400971023     C     W001A         ANDNE     *BLANK
046500971021     C                   SETON                                        32
046600971021     C                   MOVE      *ZERO         VABIAS
046700010604     C                   eval      vinmsg = %trimr(vinmsg)
046800010604     C                             + ' ' + 'VHBIAS'
046900971021     C                   ELSE
047000971021     C                   MOVE      WD15          VABIAS
047100971021     C                   ENDIF
047200050118     C                   ENDIF
047300941229     C*
047400971023     C                   TESTN                   VHBNCL               30
047500971023     C  N30              TESTN                   VHBNCL                 30
047600971023     C  N30              TESTN                   VHBNCL                   30
047700971021     C                   MOVE      VHBNCL        W001A             1
047800971021     C     *IN30         IFEQ      *OFF
047900971021     C     W001A         ORLT      '0'
048000971021     C                   SETON                                        32
048100971021     C                   MOVE      *ZERO         VABNCL
048200010604     C                   eval      vinmsg = %trimr(vinmsg)
048300010604     C                             + ' ' + 'VHBNCL'
048400971021     C                   ELSE
048500971021     C                   MOVE      VHBNCL        VABNCL
048600971021     C                   ENDIF
048700941229     C*
048800971021     C                   MOVE      *ZERO         VABPKB
048900971021     C                   MOVE      VHBPKB        WASC09
049000050503     C                   IF        WI09 = *blanks
049100050503     C                   EVAL      WI09 = *zeros
049200050503     C                   ENDIF
049300971023     C                   TESTN                   WI09                 30
049400971023     C  N30              TESTN                   WI09                   30
049500971023     C  N30              TESTN                   WI09                     30
049600971021     C                   MOVE      WI09          W001A             1
049700971021     C     *IN30         IFEQ      *OFF
049800971021     C     W001A         ORLT      '0'
049900971021     C                   SETON                                        32
050000971021     C                   MOVE      *ZERO         VABPKB
050100010604     C                   eval      vinmsg = %trimr(vinmsg)
050200010604     C                             + ' ' + 'VHBPKB'
050300971021     C                   ELSE
050400971021     C                   MOVEL     WI09          VABPKB
050500971023     C                   TESTN                   WD09                 30
050600971023     C  N30              TESTN                   WD09                     30
050700971021     C                   MOVE      WD09          W001A             1
050800971021     C     *IN30         IFEQ      *OFF
050900971021     C     W001A         ORLT      '0'
051000971023     C     W001A         ANDNE     *BLANK
051100971021     C                   SETON                                        32
051200971021     C                   MOVE      *ZERO         VABPKB
051300010604     C                   eval      vinmsg = %trimr(vinmsg)
051400010604     C                             + ' ' + 'VHBPKB'
051500971021     C                   ELSE
051600971021     C                   MOVE      WD09          VABPKB
051700971021     C                   ENDIF
051800050118     C                   ENDIF
051900971021     C*
052000041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
052100041001     C                   IF        *IN51 = *OFF
052200971021     C                   MOVE      *ZERO         VABVLB
052300971021     C                   MOVE      VHBVLB        WASC07
052400050503     C                   IF        WI07 = *blanks
052500050503     C                   EVAL      WI07 = *zeros
052600050503     C                   ENDIF
052700971023     C                   TESTN                   WI07                 30
052800971023     C  N30              TESTN                   WI07                   30
052900971023     C  N30              TESTN                   WI07                     30
053000971021     C                   MOVE      WI07          W001A             1
053100971021     C     *IN30         IFEQ      *OFF
053200971021     C     W001A         ORLT      '0'
053300971023     C     W001A         ANDNE     *BLANK
053400971021     C                   SETON                                        32
053500971021     C                   MOVE      *ZERO         VABVLB
053600010604     C                   eval      vinmsg = %trimr(vinmsg)
053700010604     C                             + ' ' + 'VHBVLB'
053800971021     C                   ELSE
053900971021     C                   MOVEL     WI07          VABVLB
054000971023     C                   TESTN                   WD07                 30
054100971023     C  N30              TESTN                   WD07                   30
054200971023     C  N30              TESTN                   WD07                     30
054300971021     C                   MOVE      WD07          W001A             1
054400971021     C     *IN30         IFEQ      *OFF
054500971021     C     W001A         ORLT      '0'
054600971023     C     W001A         ANDNE     *BLANK
054700971021     C                   SETON                                        32
054800971021     C                   MOVE      *ZERO         VABVLB
054900010604     C                   eval      vinmsg = %trimr(vinmsg)
055000010604     C                             + ' ' + 'VHBVLB'
055100971021     C                   ELSE
055200971021     C                   MOVE      WD07          VABVLB
055300971021     C                   ENDIF
055400050118     C                   ENDIF
055500041001     C                   ELSE
055600041001     C* ...altrimenti forzo azzeramento del volume
055700041001     C                   Z-ADD     *zeros        VABVLB
055800041001     C                   ENDIF
055900941229     C*
056000971021     C                   MOVE      *ZERO         VABQFT
056100971021     C                   MOVE      VHBQFT        WASC15
056200971023     C                   TESTN                   WI15                 30
056300971023     C  N30              TESTN                   WI15                   30
056400971023     C  N30              TESTN                   WI15                     30
056500971021     C                   MOVE      WI15          W001A             1
056600971021     C     *IN30         IFEQ      *OFF
056700971021     C     W001A         ORLT      '0'
056800971023     C     W001A         ANDNE     *BLANK
056900971021     C                   SETON                                        32
057000971021     C                   MOVE      *ZERO         VABQFT
057100010604     C                   eval      vinmsg = %trimr(vinmsg)
057200010604     C                             + ' ' + 'VHBQFT'
057300971021     C                   ELSE
057400971021     C                   MOVEL     WI15          VABQFT
057500971023     C                   TESTN                   WD15                 30
057600971023     C  N30              TESTN                   WD15                   30
057700971023     C  N30              TESTN                   WD15                     30
057800971021     C                   MOVE      WD15          W001A             1
057900971021     C     *IN30         IFEQ      *OFF
058000971021     C     W001A         ORLT      '0'
058100971023     C     W001A         ANDNE     *BLANK
058200971021     C                   SETON                                        32
058300971021     C                   MOVE      *ZERO         VABQFT
058400010604     C                   eval      vinmsg = %trimr(vinmsg)
058500010604     C                             + ' ' + 'VHBQFT'
058600971021     C                   ELSE
058700971021     C                   MOVE      WD15          VABQFT
058800971021     C                   ENDIF
058900050118     C                   ENDIF
059000941229     C*
059100971021     C                   MOVE      *ZERO         VABCAS
059200971021     C                   MOVE      VHBCAS        WASC15
059300971023     C                   TESTN                   WI15                 30
059400971023     C  N30              TESTN                   WI15                   30
059500971023     C  N30              TESTN                   WI15                     30
059600971021     C                   MOVE      WI15          W001A             1
059700971021     C     *IN30         IFEQ      *OFF
059800971021     C     W001A         ORLT      '0'
059900971023     C     W001A         ANDNE     *BLANK
060000971021     C                   SETON                                        32
060100971021     C                   MOVE      *ZERO         VABCAS
060200010604     C                   eval      vinmsg = %trimr(vinmsg)
060300010604     C                             + ' ' + 'VHBCAS'
060400971021     C                   ELSE
060500971021     C                   MOVEL     WI15          VABCAS
060600971023     C                   TESTN                   WD15                 30
060700971023     C  N30              TESTN                   WD15                   30
060800971023     C  N30              TESTN                   WD15                     30
060900971021     C                   MOVE      WD15          W001A             1
061000971021     C     *IN30         IFEQ      *OFF
061100971021     C     W001A         ORLT      '0'
061200971023     C     W001A         ANDNE     *BLANK
061300971021     C                   SETON                                        32
061400971021     C                   MOVE      *ZERO         VABCAS
061500010604     C                   eval      vinmsg = %trimr(vinmsg)
061600010604     C                             + ' ' + 'VHBCAS'
061700971021     C                   ELSE
061800971021     C                   MOVE      WD15          VABCAS
061900971021     C                   ENDIF
062000040827     C                   ENDIF
062100971021     C*
062200971023     C                   TESTN                   VHBRMN               30
062300971023     C  N30              TESTN                   VHBRMN                 30
062400971023     C  N30              TESTN                   VHBRMN                   30
062500971021     C                   MOVE      VHBRMN        W001A             1
062600971021     C     *IN30         IFEQ      *OFF
062700971021     C     W001A         ORLT      '0'
062800971021     C                   SETON                                        32
062900971021     C                   MOVE      *ZERO         VABRMN
063000010604     C                   eval      vinmsg = %trimr(vinmsg)
063100010604     C                             + ' ' + 'VHBRMN'
063200971021     C                   ELSE
063300971021     C                   MOVE      VHBRMN        VABRMN
063400971021     C                   ENDIF
063500971021     C*
063600971023     C                   TESTN                   VHBNCD               30
063700971023     C  N30              TESTN                   VHBNCD                 30
063800971023     C  N30              TESTN                   VHBNCD                   30
063900971021     C                   MOVE      VHBNCD        W001A             1
064000971021     C     *IN30         IFEQ      *OFF
064100971021     C     W001A         ORLT      '0'
064200971023     C     W001A         ANDNE     *BLANK
064300971021     C                   SETON                                        32
064400971021     C                   MOVE      *ZERO         VABNCD
064500010604     C                   eval      vinmsg = %trimr(vinmsg)
064600010604     C                             + ' ' + 'VHBNCD'
064700971021     C                   ELSE
064800971021     C                   MOVE      VHBNCD        VABNCD
064900971021     C                   ENDIF
065000971021     C*
065100971023     C                   TESTN                   VHBNCA               30
065200971023     C  N30              TESTN                   VHBNCA                 30
065300971023     C  N30              TESTN                   VHBNCA                   30
065400971021     C                   MOVE      VHBNCA        W001A             1
065500971021     C     *IN30         IFEQ      *OFF
065600971021     C     W001A         ORLT      '0'
065700971023     C     W001A         ANDNE     *BLANK
065800971021     C                   SETON                                        32
065900971021     C                   MOVE      *ZERO         VABNCA
066000010604     C                   eval      vinmsg = %trimr(vinmsg)
066100010604     C                             + ' ' + 'VHBNCA'
066200971021     C                   ELSE
066300971021     C                   MOVE      VHBNCA        VABNCA
066400971021     C                   ENDIF
066500971021     C*
066600971023     C                   TESTN                   VHBZNC               30
066700971023     C  N30              TESTN                   VHBZNC                 30
066800971023     C  N30              TESTN                   VHBZNC                   30
066900971021     C                   MOVE      VHBZNC        W001A             1
067000971021     C     *IN30         IFEQ      *OFF
067100971021     C     W001A         ORLT      '0'
067200971023     C     W001A         ANDNE     *BLANK
067300971021     C                   SETON                                        32
067400971021     C                   MOVE      *ZERO         VABZNC
067500010604     C                   eval      vinmsg = %trimr(vinmsg)
067600010604     C                             + ' ' + 'VHBZNC'
067700971021     C                   ELSE
067800971021     C                   MOVE      VHBZNC        VABZNC
067900971021     C                   ENDIF
068000971021     C*
068100971023     C                   TESTN                   VHBDCR               30
068200971023     C  N30              TESTN                   VHBDCR                 30
068300971023     C  N30              TESTN                   VHBDCR                   30
068400971021     C                   MOVE      VHBDCR        W001A             1
068500971021     C     *IN30         IFEQ      *OFF
068600971021     C     W001A         ORLT      '0'
068700971023     C     W001A         ANDNE     *BLANK
068800971021     C                   SETON                                        32
068900971021     C                   MOVE      *ZERO         VABDCR
069000010604     C                   eval      vinmsg = %trimr(vinmsg)
069100010604     C                             + ' ' + 'VHBDCR'
069200971021     C                   ELSE
069300971021     C                   MOVE      VHBDCR        VABDCR
069400971021     C                   ENDIF
069500971021     C*
069600971023     C                   TESTN                   VHBHCR               30
069700971023     C  N30              TESTN                   VHBHCR                 30
069800971023     C  N30              TESTN                   VHBHCR                   30
069900971021     C                   MOVE      VHBHCR        W001A             1
070000971021     C     *IN30         IFEQ      *OFF
070100971021     C     W001A         ORLT      '0'
070200971023     C     W001A         ANDNE     *BLANK
070300971021     C                   SETON                                        32
070400971021     C                   MOVE      *ZERO         VABHCR
070500010604     C                   eval      vinmsg = %trimr(vinmsg)
070600010604     C                             + ' ' + 'VHBHCR'
070700971021     C                   ELSE
070800971021     C                   MOVE      VHBHCR        VABHCR
070900971021     C                   ENDIF
071000941229     C*
071100971021     C                   MOVE      *ZERO         VABVMD
071200971021     C                   MOVE      VHBVMD        WASC15
071300971023     C                   TESTN                   WI15                 30
071400971023     C  N30              TESTN                   WI15                   30
071500971023     C  N30              TESTN                   WI15                     30
071600971021     C                   MOVE      WI15          W001A             1
071700971021     C     *IN30         IFEQ      *OFF
071800971021     C     W001A         ORLT      '0'
071900971023     C     W001A         ANDNE     *BLANK
072000971021     C                   SETON                                        32
072100971021     C                   MOVE      *ZERO         VABVMD
072200010604     C                   eval      vinmsg = %trimr(vinmsg)
072300010604     C                             + ' ' + 'VHBVMD'
072400971021     C                   ELSE
072500971021     C                   MOVEL     WI15          VABVMD
072600971023     C                   TESTN                   WD15                 30
072700971023     C  N30              TESTN                   WD15                   30
072800971023     C  N30              TESTN                   WD15                     30
072900971021     C                   MOVE      WD15          W001A             1
073000971021     C     *IN30         IFEQ      *OFF
073100971021     C     W001A         ORLT      '0'
073200971023     C     W001A         ANDNE     *BLANK
073300971021     C                   SETON                                        32
073400971021     C                   MOVE      *ZERO         VABVMD
073500010604     C                   eval      vinmsg = %trimr(vinmsg)
073600010604     C                             + ' ' + 'VHBVMD'
073700971021     C                   ELSE
073800971021     C                   MOVE      WD15          VABVMD
073900971021     C                   ENDIF
074000050118     C                   ENDIF
074100971021     C*
074200971023     C                   TESTN                   VHBANT               30
074300971023     C  N30              TESTN                   VHBANT                 30
074400971023     C  N30              TESTN                   VHBANT                   30
074500971021     C                   MOVE      VHBANT        W001A             1
074600971021     C     *IN30         IFEQ      *OFF
074700971021     C     W001A         ORLT      '0'
074800971023     C     W001A         ANDNE     *BLANK
074900971021     C                   SETON                                        32
075000971021     C                   MOVE      *ZERO         VABANT
075100010604     C                   eval      vinmsg = %trimr(vinmsg)
075200010604     C                             + ' ' + 'VHBANT'
075300971021     C                   ELSE
075400971021     C                   MOVE      VHBANT        VABANT
075500971021     C                   ENDIF
075600971021     C*
075700971021     C* CAMPI ALFABETICI:
075800971021     C                   MOVE      VHBATB        VABATB
075900971021     C                   MOVE      VHBCBO        VABCBO
076000971021     C                   MOVE      VHBRSD        VABRSD
076100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
076200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
076300020117     C* ==
076400971021     C                   MOVE      VHBRD2        VABRD2
076500971021     C                   MOVE      VHBIND        VABIND
076600971021     C                   MOVE      VHBCAD        VABCAD
076700971021     C                   MOVE      VHBLOD        VABLOD
076800971021     C                   MOVE      VHBPRD        VABPRD
076900971021     C                   MOVE      VHBNZD        VABNZD
077000971021     C                   MOVE      VHBGC1        VABGC1
077100971021     C                   MOVE      VHBGC2        VABGC2
077200971021     C                   MOVE      VHBTSP        VABTSP
077300971021     C                   MOVE      VHBVAS        VABVAS
077400971021     C                   MOVE      VHBNAS        VABNAS
077500971021     C                   MOVE      VHBTIC        VABTIC
077600971021     C                   MOVE      VHBVCA        VABVCA
077700971021     C                   MOVE      VHBGCA        VABGCA
077800971021     C                   MOVE      VHBRMA        VABRMA
077900971021     C                   MOVE      VHBXCO        VABXCO
078000971021     C                   MOVE      VHBNOT        VABNOT
078100971021     C                   MOVE      VHBNT2        VABNT2
078200971021     C                   MOVE      VHBCTM        VABCTM
078300971021     C                   MOVE      VHBFFD        VABFFD
078400971021     C                   MOVE      VHBTCR        VABTCR
078500971021     C                   MOVE      VHBCTS        VABCTS
078600971021     C                   MOVE      VHBFTM        VABFTM
078700971021     C                   MOVE      VHBVAD        VABVAD
078800971021     C                   MOVE      VHBGMA        VABGMA
078900971021     C                   MOVE      VHBGGA        VABGGA
079000971021     C                   MOVE      VHBGVA        VABGVA
079100971021     C                   MOVE      VHBTC1        VABTC1
079200971021     C                   MOVE      VHBTC2        VABTC2
079300971021     C                   MOVE      VHBSCL        VABSCL
079400971021     C                   MOVE      VHBRMO        VABRMO
079500971021     C                   MOVE      VHBCMO        VABCMO
079600971021     C                   MOVE      VHBNMO        VABNMO
079700971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
079800060329     C* NELLA NAZIONE MITTENTE ORIGINALE
079900060331     C                   IF        %trim(VHBNMO) < 'A'
080000060329     C                   MOVE      *BLANKS       VABNMO
080100060329     C                   ENDIF
080200050418     C*
080300050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
080400050418     C                   IF        %trim(VABNZD) = 'I'   OR
080500050418     C                             %trim(VABNZD) = 'IT'  OR
080600050418     C                             %trim(VABNZD) = 'ITA'
080700050418     C                   EVAL      VABNZD = *blanks
080800050418     C                   ENDIF
080900041025     C*
081000041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
081100041025     C                   IF        *IN52 = *ON
081200041025     C* Reperisco la provincia dal CAP e dalla località
081300041025     C                   IF        VABCAD <> *blanks AND
081400050907     C                             VABPRD  = *blanks AND
081500050907     C                             VABNZD  = *blanks
081600041025     C                   CLEAR                   TISI95DS
081700041025     C                   EVAL      I95TCN = '3'
081800041025     C                   Z-ADD     datcor        I95DAT
081900041025     C                   EVAL      I95CAP = VABCAD
082000041025     C                   EVAL      I95LOC = VABLOD
082100041025     C                   CALL      'TISI95R'
082200041025     C                   PARM                    TISI95DS
082300041025     C                   EVAL      VABPRD = O95PRV
082400041025     C                   ENDIF
082500041025     C                   ENDIF
082600090216     C*
082700090216     C* Forzatura x aggiungere il secolo al riferimenti mittente numerico passato
082800090216     C* dal cliente (x T&T via link diretto XML)
082900090216     C                   MOVEL     *blanks       wRMN_Alfa15      15
083000090216     C                   MOVE(P)   VABRMN        wRMN_Alfa06       6
083100090216     C                   MOVE(P)   VABAAS        wAAS_Alfa         2
083200090216     C                   EVALR     wRMN_Alfa15=wAAS_Alfa+wRMN_Alfa06
083300090216     C                   MOVE      wRMN_Alfa15   VABRMN
083400040630     C*
083500040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
083600040630     C                   exsr      exeBon
083700930409     C*
083800010604     C  N31              ADD       1             §CTROK            7 0
083900010604     C   32              ADD       1             §CTRMO            7 0
084000010604     C   31              ADD       1             §CTRNO            7 0
084100020916     C  N31              WRITE     FIVAB000
084200990910     C*
084300010601     C                   if        *in31 = *off and
084400010601     C                             *in32 = *off
084500990910     C                   eval      vinflg = '1'
084600990910     C                   else
084700000621     C                   eval      recko = vindta
084800010612     C                   if        %open(tis7t4ps)
084900030407     C   50              except    rigadett
085000010612     C                   endif
085100990910     C                   eval      vinflg = '2'
085200990910     C                   endif
085300000905     C                   endif
085400000905     C                   endif
085500010601     C*
085600010601     C                   endif
085700000905     C*
085800000905     C                   else
085900000905     C                   eval      vinflg = '1'
086000000905     C                   endif
086100000905     C*
086200000905     C  N70              update    tivin000
086300000905     C*
086400991022     C  N70              ENDdo
086500010601     C
086600010601     C*
086700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
086800010601     C                   if        cntNonEl = *zeros or
086900010601     C                             flgMulti = '0'
087000990910     C* Se non ci sono record con errori ...
087100010601     C                   if        §ctrno = 0 and
087200010601     C                             §ctrmo = 0 and
087300010601     C                             flgStato <> '2'
087400990910     C* ... restituisco esito OK.
087500990921     C                   eval      wrkesito = '0'
087600990910     C                   else
087700010601     C                   if        §ctrok > 0
087800000710     C                   eval      wrkesito = '1'
087900000710     C                   else
088000010615     C                   if        flgOk = '0'
088100010615     C                   eval      wrkesito = '2'
088200010615     C                   else
088300010615     C                   eval      wrkesito = '6'
088400010615     C                   endif
088500000710     C                   endif
088600010615     C                   endif
088700010601     C                   else
088800010601     C                   eval      wrkesito = '9'
088900010601     C                   endif
089000990910     C*
089100990914     C                   if        %open(tivin00r)
089200990908     C                   close     tivin00r
089300990914     C                   endif
089400020916     C                   if        %open(fivabwwr)
089500020916     C                   close     fivabwwr
089600990914     C                   endif
089700010601     C*
089800010601     C                   if        vlrpoi <> 999
089900010601     C                   eval      invfil = vlrpoi
090000010601     C                   endif
090100990910     C*
090200990920     C                   if        §ctrok > 0
090300010601     C                             and invfil > *zeros
090400990920     C                   exsr      invio
090500990920     C                   endif
090600010612     C*
090700010612     C                   if        flgGiro = '1'
090800010612     C                   exsr      endela
090900010612     C                   endif
091000990920     C*
091100910830     C                   ENDSR
091200000613     C***
091300990920
091400010601
091500010601
091600010601
091700010601
091800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
091900010601     C     repfil        BEGSR
092000010601     C*
092100010601     C                   if        invfil = *zeros and
092200010601     C                             depfil > *zeros and
092300010613     C                             (vinflg = *blanks or
092400010613     C                              vinflg = *zeros)
092500010601     C
092600010601     C                   eval      invfil = depfil
092700010601     C                   endif
092800010601     C*
092900010601     C                   if        depfil <> invfil and
093000010601     C                             invfil > *zeros
093100010601     C                   eval      flgMulti = '1'
093200010601     C                   if        vinflg = *blanks
093300010601     C                   add       1             cntNonEl
093400010601     C                   endif
093500010601     C                   endif
093600010601     C*
093700010601     C                   if        vinflg = '2'
093800010601     C                   eval      flgStato = '2'
093900010601     C                   endif
094000010601     C*
094100010601     C                   ENDSR
094200010601     C***
094300010601
094400010601
094500010601
094600010601
094700010601
094800010601
094900010601
095000990920      /TITLE Invio dei dati al punto operativo.
095100000613     C     invio         BEGSR
095200990920     C*
095300990920     C                   reset                   dscmz
095400010601     C                   move      invfil        cmzdst
095500990920     C                   eval      cmzfld = vlrfou
095600990920     C                   eval      cmzmbd = vlrhdl
095700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
095800000710     C                   if        prmfir = *blanks
095900020916     C                   eval      cmzfla = 'FIVAB00F'
096000020916     C                   eval      cmzmba = 'FIVAB00F'
096100000710     C                   else
096200000710     C                   eval      cmzfla = prmfir
096300000710     C                   eval      cmzmba = prmfir
096400000710     C                   endif
096500990920     C                   eval      cmznrr = *zeros
096600990920     C                   move      §ctrok        cmznrr
096700021003     C                   eval      cmzlba = vlrfl1
096800990920     C                   call(e)   'TIS711C'
096900990920     C                   parm                    dscmz
097000990921     C                   parm      *blanks       esito
097100990920     C                   if        %error
097200990920     C                             or cmzerr = '1'
097300990921     C                             or esito  = '1'
097400000710     C                   eval      wrkesito = '3'
097500990920     C                   endif
097600990920     C*
097700000613     C                   ENDSR
097800010601
097900010601
098000010601
098100010601
098200010601      /TITLE Invio dei dati al punto operativo.
098300010601     C     opeini        BEGSR
098400010601     C*
098500010601     C* Inizializzo flag e contatori operativi
098600010601     C                   movel     '0'           flgGiro           1
098700010601     C                   movel     '0'           flgMulti          1
098800010601     C                   movel     '1'           flgStato          1
098900010615     C                   movel     '0'           flgOk             1
099000010601     C                   z-add     *zeros        cntNonEl         10 0
099100010601     C                   z-add     *zeros        depfil            3 0
099200010601     C                   z-add     *zeros        invfil            3 0
099300030407     C*
099400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
099500030407     C                   if        %subst(vlrppt:1:1) = 'S'
099600030407     C                   seton                                        50
099700030407     C                   else
099800030407     C                   setoff                                       50
099900030407     C                   endif
100000041001     C*
100100041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
100200041001     C                   if        %subst(vlrppt:2:1) = 'S'
100300041001     C                   seton                                        51
100400041001     C                   else
100500041001     C                   setoff                                       51
100600041001     C                   endif
100700041025     C*
100800041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
100900041025     C* (naturalmente solo se nn già presente)
101000041025     C                   if        %subst(vlrppt:3:1) = 'S'
101100041025     C                   seton                                        52
101200041025     C                   else
101300041025     C                   setoff                                       52
101400041025     C                   endif
101500010601     C*
101600010601     C                   ENDSR
101700010601     C***
101800040630
101900040630
102000040630
102100040630
102200040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
102300040630     C     exeBon        BEGSR
102400040630     C*
102500040630     C* Imposto i parametri x il *pgm d "bonifica"
102600040630     C                   EVAL      ElencoChar = *blanks
102700040630     C                   EVAL      TipoElenco = *blanks
102800040630     C                   EVAL      CharSost   = *blanks
102900040630     C                   EVAL      UpperCase  = '1'
103000040630     C                   EVAL      ChkNull    = '1'
103100040630     C                   EVAL      CharNull   = *blanks
103200040630     C*
103300040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
103400040630     C                   EVAL      TxtInOut   = VABRSD
103500040630     C                   EVAL      EsitoBon   = *blanks
103600040630     C*
103700040630     C                   CALL      'XCHKCHAR'
103800040630     C                   PARM                    TxtInOut
103900040630     C                   PARM                    ElencoChar
104000040630     C                   PARM                    TipoElenco
104100040630     C                   PARM                    CharSost
104200040630     C                   PARM                    UpperCase
104300040630     C                   PARM                    ChkNull
104400040630     C                   PARM                    CharNull
104500040630     C                   PARM                    EsitoBon
104600040630     C*
104700040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
104800040630     C                   IF        EsitoBon = '1'
104900040630     C                   EVAL      VABRSD = TxtInOut
105000040630     C                   ENDIF
105100040630     C*
105200040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
105300040630     C                   EVAL      TxtInOut   = VABIND
105400040630     C                   EVAL      EsitoBon   = *blanks
105500040630     C*
105600040630     C                   CALL      'XCHKCHAR'
105700040630     C                   PARM                    TxtInOut
105800040630     C                   PARM                    ElencoChar
105900040630     C                   PARM                    TipoElenco
106000040630     C                   PARM                    CharSost
106100040630     C                   PARM                    UpperCase
106200040630     C                   PARM                    ChkNull
106300040630     C                   PARM                    CharNull
106400040630     C                   PARM                    EsitoBon
106500040630     C*
106600040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
106700040630     C                   IF        EsitoBon = '1'
106800040630     C                   EVAL      VABIND = TxtInOut
106900040630     C                   ENDIF
107000040630     C*
107100040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
107200040630     C                   EVAL      TxtInOut   = VABLOD
107300040630     C                   EVAL      EsitoBon   = *blanks
107400040630     C*
107500040630     C                   CALL      'XCHKCHAR'
107600040630     C                   PARM                    TxtInOut
107700040630     C                   PARM                    ElencoChar
107800040630     C                   PARM                    TipoElenco
107900040630     C                   PARM                    CharSost
108000040630     C                   PARM                    UpperCase
108100040630     C                   PARM                    ChkNull
108200040630     C                   PARM                    CharNull
108300040630     C                   PARM                    EsitoBon
108400040630     C*
108500040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
108600040630     C                   IF        EsitoBon = '1'
108700040630     C                   EVAL      VABLOD = TxtInOut
108800040630     C                   ENDIF
108900040630     C*
109000040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
109100040630     C                   EVAL      TxtInOut   = VABNOT
109200040630     C                   EVAL      EsitoBon   = *blanks
109300040630     C                   EVAL      UpperCase  = *blanks
109400040630     C*
109500040630     C                   CALL      'XCHKCHAR'
109600040630     C                   PARM                    TxtInOut
109700040630     C                   PARM                    ElencoChar
109800040630     C                   PARM                    TipoElenco
109900040630     C                   PARM                    CharSost
110000040630     C                   PARM                    UpperCase
110100040630     C                   PARM                    ChkNull
110200040630     C                   PARM                    CharNull
110300040630     C                   PARM                    EsitoBon
110400040630     C*
110500040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
110600040630     C                   IF        EsitoBon = '1'
110700040630     C                   EVAL      VABNOT = TxtInOut
110800040630     C                   ENDIF
110900040630     C*
111000040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
111100040630     C                   EVAL      TxtInOut   = VABNT2
111200040630     C                   EVAL      EsitoBon   = *blanks
111300040630     C                   EVAL      UpperCase  = *blanks
111400040630     C*
111500040630     C                   CALL      'XCHKCHAR'
111600040630     C                   PARM                    TxtInOut
111700040630     C                   PARM                    ElencoChar
111800040630     C                   PARM                    TipoElenco
111900040630     C                   PARM                    CharSost
112000040630     C                   PARM                    UpperCase
112100040630     C                   PARM                    ChkNull
112200040630     C                   PARM                    CharNull
112300040630     C                   PARM                    EsitoBon
112400040630     C*
112500040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
112600040630     C                   IF        EsitoBon = '1'
112700040630     C                   EVAL      VABNT2 = TxtInOut
112800040630     C                   ENDIF
112900040630     C*
113000040630     C                   ENDSR
113100090511
113200090511
113300090511
113400010601
113500010601
113600990910
113700000613     C     *inzsr        BEGSR
113800990910     C*
113900990910     C     *entry        plist
114000990920     C                   parm                    tivlrds
114100990921     C                   parm      wrkesito      esito
114200000724     C                   parm                    prmlit
114300000710     C                   parm                    prmfir
114400010606     C*
114500010606     C* CALCOLA LA DATA CORRENTE
114600010606     C                   time                    wn14             14 0
114700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
114800010606     C                   z-add     wn8           g08dat
114900010606     C                   z-add     *zeros        g08inv
115000010606     C                   movel     '0'           g08err
115100010606     C                   call      'XSRDA8'
115200010606     C                   parm                    wlbda8
115300010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
115400000613     C*
115500000613     C                   ENDSR
115600000613     C***
115700990908
115800000613     Otis7t4p   E            riepilogo         2
115900990915     O                                              'Upload via Internet'
116000990915     O                                           +1 'Traduzione TIVIN00R -'
116100020916     O                                           +1 'FIVABWWR'
116200990915     O                                           +1 'Testate bolle'
116300990915     O          E            riepilogo   2
116400990915     O                       wrkdata
116500990915     O                       wrkora              +1
116600990915     O                       procname            +1
116700990915     O          E            riepilogo   2
116800990915     O                                              'Cliente..................:'
116900990915     O                       VABCCM        z     +1
117000990915     O          E            riepilogo   2
117100990920     O                                              'Riferimento Strategi.....:'
117200990920     O                       vlrhdl              +1
117300990915     O          E            riepilogo   2
117400990915     O                                              'Giusti...................:'
117500971022     O                       §CTROK        2   +  1
117600990915     O          E            riepilogo   2
117700990915     O                                              'Sbagliati e corretti.....:'
117800971022     O                       §CTRMO        2   +  1
117900990915     O          E            riepilogo   2
118000990915     O                                              'Sbagliati e scartati.....:'
118100971022     O                       §CTRNO        2   +  1
118200000613
118300000616     Otis7t4ps  E            testdett          2
118400000613     O                                              'Upload via Internet'
118500000613     O                                           +1 'Traduzione TIVIN00R -'
118600020916     O                                           +1 'FIVABWWR'
118700000613     O                                           +1 'Testate bolle'
118800000616     O          E            testdett    3
118900000613     O                                           +2 'N° rec'
119000000613     O                                           +3 'Anteprima contenuto'
119100000616     O          E            rigadett    2
119200000613     O                       rrnum               +2
119300000621     O                       recko               +3
119400000616     O          E            findett     2
119500000613     O                       wrkdata
119600000613     O                       wrkora              +1
119700000613     O                       procname            +1
119800000616     O          E            findett     2
119900000613     O                                              'Cliente..................:'
120000000613     O                       VABCCM        z     +1
120100000616     O          E            findett     2
120200000613     O                                              'Riferimento Strategi.....:'
120300000613     O                       vlrhdl              +1
120400000616     O          E            findett     2
120500000613     O                                              'Giusti...................:'
120600000613     O                       §CTROK        2   +  1
120700000616     O          E            findett     2
120800000613     O                                              'Sbagliati e corretti.....:'
120900000613     O                       §CTRMO        2   +  1
121000000616     O          E            findett     2
121100000613     O                                              'Sbagliati e scartati.....:'
121200000613     O                       §CTRNO        2   +  1
121300000613** CMD - COMANDI CL
121400011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4P)  FORMTYPE(RICCLI) OUTQ(
121500011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4PS) FORMTYPE(RICCLI) OUTQ(
121600000616DLTOVR FILE(TIS7T4P TIS7T4PS) LVL(*)
121700000613
121800000613
