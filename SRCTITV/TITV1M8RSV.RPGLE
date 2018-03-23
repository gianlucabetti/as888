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
011300010606     D*------------------
011400010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
011500010606     D*------------------
011600010606     D WLBDA8          DS                  INZ
011700010606     D  G08DAT                 1      8  0
011800010606     D  G08INV                 9     16  0
011900010606     D  G08ERR                17     17
012000010606     D  G08TGI                18     22  0
012100010606     D*
012200040407     D TxtInOut        S           2048
012300040407     D ElencoChar      S            256
012400040407     D TipoElenco      S              1
012500040407     D CharSost        S              1
012600040407     D UpperCase       S              1
012700040407     D ChkNull         S              1
012800040407     D CharNull        S              1
012900040407     D EsitoBon        S              1
013000010606
013100010606
013200010606
013300010606
013400990915     C                   time                    wrkdata
013500990915     C                   time                    wrkora
013600000913     C                   reset                   rrnum
013700990921     C                   reset                   esito
013800990921     C                   reset                   wrkesito
013900010601     C*
014000010601     C                   exsr      opeini
014100010601     C                   exsr      rwvab
014200041025     C*
014300041025     C* Esegue lancio TISI95R solo x chiusura
014400041025     C                   CLEAR                   TISI95DS
014500041025     C                   EVAL      I95TLA = 'C'
014600041025     C                   CALL      'TISI95R'
014700041025     C                   PARM                    TISI95DS
014800010601     C*
014900010601     C                   seton                                        lr
015000010601
015100010601
015200010601
015300010601
015400010601     C*--------------------------------------------------------
015500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
015600010601     C*--------------------------------------------------------
015700010601     C     PREELA        BEGSR
015800010601     C*
015900010601     C* SE OCCORRE SPEDIRE IN FILIALE
016000010601     C                   if        invfil <> *zeros and
016100010601     C                             flgGiro = '0'
016200010601     C*
016300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
016400010601     C                   eval      flgGiro = '1'
016500000724     C*
016600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
016700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016800010601     C     invfil        chain     azorg01l
016900000724     C                   if        %found
017000000724     C                   movel(p)  CMD(1)        depcmd
017100020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
017200000724     C*
017300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
017400011123     C                   Z-ADD     150           LENGH            15 5
017500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017600000724     C                   PARM                    depcmd
017700000724     C                   PARM                    LENGH
017800000724     C*
017900000724     C                   endif
018000000616     C*
018100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
018200000616     C                   movel(p)  CMD(2)        depcmd
018300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
018400000616     C*
018500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
018600011123     C                   Z-ADD     150           LENGH            15 5
018700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018800000616     C                   PARM                    depcmd
018900000616     C                   PARM                    LENGH
019000000613     C*
019100000616     C                   if        not %open(tis7t4ps)
019200000616     C                   open      tis7t4ps
019300030407     C   50              except    testdett
019400000613     C                   endif
019500010601     C*
019600010601     C                   endif
019700010601     C*
019800010601     C                   ENDSR
019900010601     C***
020000010601
020100010601
020200010601
020300010601
020400010601
020500010601
020600010601     C*--------------------------------------------------------
020700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
020800010601     C*--------------------------------------------------------
020900010601     C     ENDELA        BEGSR
021000990915     C*
021100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
021200000613     C*
021300000616     C                   if        %open(tis7t4ps)
021400030407     C   50              except    findett
021500000616     C                   close     tis7t4ps
021600000613     C                   endif
021700000616     C*
021800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
021900011123     C                   Z-ADD     150           LENGH            15 5
022000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
022100000616     C                   PARM                    CMD(3)
022200000616     C                   PARM                    LENGH
022300010601     C*
022400010601     C                   ENDSR
022500010601     C***
022600010601
022700010601
022800000613
022900000613
023000000613
023100000613     C*--------------------------------------------------------
023200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
023300000613     C*--------------------------------------------------------
023400000621     C     STPR          BEGSR
023500000613     C*
023600000613     C                   if        not %open(tis7t4p)
023700000613     C                   open      tis7t4p
023800990915     C                   endif
023900990915     C*
024000030407     C   50              except    riepilogo
024100990915     C*
024200000613     C                   if        %open(tis7t4p)
024300000613     C                   close     tis7t4p
024400990914     C                   endif
024500000613     C*
024600000613     C                   ENDSR
024700000613     C***
024800000613
024900010601
025000010601
025100010601
025200010601
025300910830     C*--------------------------------------------------------
025400020916     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
025500910830     C*--------------------------------------------------------
025600941229     C     RWVAB         BEGSR
025700990910     C*
025800990914     C                   if        not %open(tivin00r)
025900990908     C                   open      tivin00r
026000990914     C                   endif
026100020916     C                   if        not %open(fivabwwr)
026200020916     C                   open      fivabwwr
026300990914     C                   endif
026400990910     C*
026500990910     C                   clear                   §CTROK
026600990910     C                   clear                   §CTRMO
026700990910     C                   clear                   §CTRNO
026800990910     C*
026900921023     C                   DO        *HIVAL
027000990913     C*
027100990915     C                   READ      tivin00r                               70
027200010618     C*
027300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
027400010618     C                   if        vinflg = '1'
027500010618     C                   eval      flgOk = '1'
027600010618     C                   endif
027700010618     C*
027800051017     C                   if        vindta  > *blanks
027900000613     C                   add       1             rrnum
028000990913     C*
028100010601     C                   if        *in70 = *off and
028200990913     C                             (vinflg = *blanks
028300010711     C                              or vinflg = '0'
028400010711     C                              or vinflg = '2')
028500990913     C*
028600020916     C                   clear                   FIVAB000
028700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028800010711     C                   if        vinflg = *blanks or vinflg = '0'
028900010711     C                   clear                   vinmsg
029000010711     C                   endif
029100010711     C*
029200000313     C                   eval      x_rec  = vindta
029300991022     C*
029400991022     C                   testb     x'3F'         vhbatb                   02
029500991022     C                   if        not *in02
029600991022     C*
029700971021     C                   SETOFF                                       303132
029800971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
029900971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
030000971023     C*    OPPURE TUTTI BLANKS
030100010601     C*
030200010601     C                   TESTN                   VHBLNP               30
030300010601     C  N30              TESTN                   VHBLNP                 30
030400010601     C  N30              TESTN                   VHBLNP                   30
030500010601     C                   MOVE      VHBLNP        W001A             1
030600010601     C     *IN30         IFEQ      *OFF
030700010601     C     W001A         ORLT      '0'
030800010601     C                   SETON                                        31
030900010601     C                   eval      vinmsg = %trimr(vinmsg)
031000010601     C                             + ' ' + 'VHBLNP'
031100010601     C                   ELSE
031200010601     C                   MOVE      VHBLNP        VABLNP
031300010601     C     VABLNP        IFLE      *ZEROS
031400010601     C                   SETON                                        31
031500010601     C                   eval      vinmsg = %trimr(vinmsg)
031600010601     C                             + ' ' + 'VHBLNP'
031700010601     C                   ENDIF
031800010601     C                   ENDIF
031900010601     C*
032000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
032100010601     C                   eval      depfil = VABLNP
032200010601     C                   exsr      repfil
032300010601     C                   if        depfil = invfil
032400021014     C                   if        vlrpoi = 999
032500021010     C                   MOVE(P)   invfil        VABFGS
032600021014     C                   else
032700021014     C                   MOVE(P)   vlrpoi        VABFGS
032800021014     C                   endif
032900010601     C*
033000010601     C                   exsr      PREELA
033100010601     C*
033200971023     C                   TESTN                   VHBCCM               30
033300971023     C  N30              TESTN                   VHBCCM                 30
033400971023     C  N30              TESTN                   VHBCCM                   30
033500971021     C                   MOVE      VHBCCM        W001A             1
033600971021     C     *IN30         IFEQ      *OFF
033700971021     C     W001A         ORLT      '0'
033800971021     C                   SETON                                        31
033900010604     C                   eval      vinmsg = %trimr(vinmsg)
034000010604     C                             + ' ' + 'VHBCCM'
034100971021     C                   ELSE
034200971021     C                   MOVE      VHBCCM        VABCCM
034300971024     C     VABCCM        IFLE      *ZEROS
034400971024     C                   SETON                                        31
034500010604     C                   eval      vinmsg = %trimr(vinmsg)
034600010604     C                             + ' ' + 'VHBCCM'
034700971024     C                   ENDIF
034800971021     C                   ENDIF
034900971021     C*
035000971023     C                   TESTN                   VHBAAS               30
035100971023     C  N30              TESTN                   VHBAAS                 30
035200971023     C  N30              TESTN                   VHBAAS                   30
035300971021     C                   MOVE      VHBAAS        W001A             1
035400971021     C     *IN30         IFEQ      *OFF
035500971021     C     W001A         ORLT      '0'
035600971023     C     W001A         ANDNE     *BLANK
035700971021     C                   SETON                                        32
035800010606     C                   MOVEL     datcor        VABAAS
035900010604     C                   eval      vinmsg = %trimr(vinmsg)
036000010604     C                             + ' ' + 'VHBAAS'
036100971021     C                   ELSE
036200971021     C                   MOVE      VHBAAS        VABAAS
036300971021     C                   ENDIF
036400971021     C*
036500971023     C                   TESTN                   VHBMGS               30
036600971023     C  N30              TESTN                   VHBMGS                 30
036700971023     C  N30              TESTN                   VHBMGS                   30
036800971021     C                   MOVE      VHBMGS        W001A             1
036900971021     C     *IN30         IFEQ      *OFF
037000971021     C     W001A         ORLT      '0'
037100971023     C     W001A         ANDNE     *BLANK
037200971021     C                   SETON                                        32
037300010601     C                   MOVE      '0101'        VABMGS
037400010604     C                   eval      vinmsg = %trimr(vinmsg)
037500010604     C                             + ' ' + 'VHBMGS'
037600971021     C                   ELSE
037700971021     C                   MOVE      VHBMGS        VABMGS
037800971021     C                   ENDIF
037900971021     C*
038000971023     C                   TESTN                   VHBNRS               30
038100971023     C  N30              TESTN                   VHBNRS                 30
038200971023     C  N30              TESTN                   VHBNRS                   30
038300971021     C                   MOVE      VHBNRS        W001A             1
038400971021     C     *IN30         IFEQ      *OFF
038500971021     C     W001A         ORLT      '0'
038600971023     C     W001A         ANDNE     *BLANK
038700971021     C                   SETON                                        32
038800971021     C                   MOVE      *ZERO         VABNRS
038900010604     C                   eval      vinmsg = %trimr(vinmsg)
039000010604     C                             + ' ' + 'VHBNRS'
039100971021     C                   ELSE
039200971021     C                   MOVE      VHBNRS        VABNRS
039300971021     C                   ENDIF
039400971021     C*
039500971023     C                   TESTN                   VHBNSP               30
039600971023     C  N30              TESTN                   VHBNSP                 30
039700971023     C  N30              TESTN                   VHBNSP                   30
039800971021     C                   MOVE      VHBNSP        W001A             1
039900971021     C     *IN30         IFEQ      *OFF
040000971021     C     W001A         ORLT      '0'
040100971023     C     W001A         ANDNE     *BLANK
040200971021     C                   SETON                                        32
040300971021     C                   MOVE      *ZERO         VABNSP
040400010604     C                   eval      vinmsg = %trimr(vinmsg)
040500010604     C                             + ' ' + 'VHBNSP'
040600971021     C                   ELSE
040700971021     C                   MOVE      VHBNSP        VABNSP
040800971021     C                   ENDIF
040900971021     C*
041000971023     C                   TESTN                   VHBLNA               30
041100971023     C  N30              TESTN                   VHBLNA                 30
041200971023     C  N30              TESTN                   VHBLNA                   30
041300971021     C                   MOVE      VHBLNA        W001A             1
041400971021     C     *IN30         IFEQ      *OFF
041500971021     C     W001A         ORLT      '0'
041600971023     C     W001A         ANDNE     *BLANK
041700971021     C                   SETON                                        32
041800971021     C                   MOVE      *ZERO         VABLNA
041900010604     C                   eval      vinmsg = %trimr(vinmsg)
042000010604     C                             + ' ' + 'VHBLNA'
042100971021     C                   ELSE
042200971021     C                   MOVE      VHBLNA        VABLNA
042300971021     C                   ENDIF
042400971021     C*
042500971023     C                   TESTN                   VHBCTR               30
042600971023     C  N30              TESTN                   VHBCTR                 30
042700971023     C  N30              TESTN                   VHBCTR                   30
042800971021     C                   MOVE      VHBCTR        W001A             1
042900971021     C     *IN30         IFEQ      *OFF
043000971021     C     W001A         ORLT      '0'
043100971023     C     W001A         ANDNE     *BLANK
043200971021     C                   SETON                                        32
043300971021     C                   MOVE      *ZERO         VABCTR
043400010604     C                   eval      vinmsg = %trimr(vinmsg)
043500010604     C                             + ' ' + 'VHBCTR'
043600971021     C                   ELSE
043700971021     C                   MOVE      VHBCTR        VABCTR
043800971021     C                   ENDIF
043900941229     C*
044000971021     C                   MOVE      *ZERO         VABIAS
044100941229     C                   MOVE      VHBIAS        WASC15
044200971023     C                   TESTN                   WI15                 30
044300971023     C  N30              TESTN                   WI15                   30
044400971023     C  N30              TESTN                   WI15                     30
044500971021     C                   MOVE      WI15          W001A             1
044600971021     C     *IN30         IFEQ      *OFF
044700971021     C     W001A         ORLT      '0'
044800971023     C     W001A         ANDNE     *BLANK
044900971021     C                   SETON                                        32
045000971021     C                   MOVE      *ZERO         VABIAS
045100010604     C                   eval      vinmsg = %trimr(vinmsg)
045200010604     C                             + ' ' + 'VHBIAS'
045300971021     C                   ELSE
045400971021     C                   MOVEL     WI15          VABIAS
045500971023     C                   TESTN                   WD15                 30
045600971023     C  N30              TESTN                   WD15                   30
045700971023     C  N30              TESTN                   WD15                     30
045800971021     C                   MOVE      WD15          W001A             1
045900971021     C     *IN30         IFEQ      *OFF
046000971021     C     W001A         ORLT      '0'
046100971023     C     W001A         ANDNE     *BLANK
046200971021     C                   SETON                                        32
046300971021     C                   MOVE      *ZERO         VABIAS
046400010604     C                   eval      vinmsg = %trimr(vinmsg)
046500010604     C                             + ' ' + 'VHBIAS'
046600971021     C                   ELSE
046700971021     C                   MOVE      WD15          VABIAS
046800971021     C                   ENDIF
046900050118     C                   ENDIF
047000941229     C*
047100971023     C                   TESTN                   VHBNCL               30
047200971023     C  N30              TESTN                   VHBNCL                 30
047300971023     C  N30              TESTN                   VHBNCL                   30
047400971021     C                   MOVE      VHBNCL        W001A             1
047500971021     C     *IN30         IFEQ      *OFF
047600971021     C     W001A         ORLT      '0'
047700971021     C                   SETON                                        32
047800971021     C                   MOVE      *ZERO         VABNCL
047900010604     C                   eval      vinmsg = %trimr(vinmsg)
048000010604     C                             + ' ' + 'VHBNCL'
048100971021     C                   ELSE
048200971021     C                   MOVE      VHBNCL        VABNCL
048300971021     C                   ENDIF
048400941229     C*
048500971021     C                   MOVE      *ZERO         VABPKB
048600971021     C                   MOVE      VHBPKB        WASC09
048700050503     C                   IF        WI09 = *blanks
048800050503     C                   EVAL      WI09 = *zeros
048900050503     C                   ENDIF
049000971023     C                   TESTN                   WI09                 30
049100971023     C  N30              TESTN                   WI09                   30
049200971023     C  N30              TESTN                   WI09                     30
049300971021     C                   MOVE      WI09          W001A             1
049400971021     C     *IN30         IFEQ      *OFF
049500971021     C     W001A         ORLT      '0'
049600971021     C                   SETON                                        32
049700971021     C                   MOVE      *ZERO         VABPKB
049800010604     C                   eval      vinmsg = %trimr(vinmsg)
049900010604     C                             + ' ' + 'VHBPKB'
050000971021     C                   ELSE
050100971021     C                   MOVEL     WI09          VABPKB
050200971023     C                   TESTN                   WD09                 30
050300971023     C  N30              TESTN                   WD09                     30
050400971021     C                   MOVE      WD09          W001A             1
050500971021     C     *IN30         IFEQ      *OFF
050600971021     C     W001A         ORLT      '0'
050700971023     C     W001A         ANDNE     *BLANK
050800971021     C                   SETON                                        32
050900971021     C                   MOVE      *ZERO         VABPKB
051000010604     C                   eval      vinmsg = %trimr(vinmsg)
051100010604     C                             + ' ' + 'VHBPKB'
051200971021     C                   ELSE
051300971021     C                   MOVE      WD09          VABPKB
051400971021     C                   ENDIF
051500050118     C                   ENDIF
051600971021     C*
051700041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
051800041001     C                   IF        *IN51 = *OFF
051900971021     C                   MOVE      *ZERO         VABVLB
052000971021     C                   MOVE      VHBVLB        WASC07
052100050503     C                   IF        WI07 = *blanks
052200050503     C                   EVAL      WI07 = *zeros
052300050503     C                   ENDIF
052400971023     C                   TESTN                   WI07                 30
052500971023     C  N30              TESTN                   WI07                   30
052600971023     C  N30              TESTN                   WI07                     30
052700971021     C                   MOVE      WI07          W001A             1
052800971021     C     *IN30         IFEQ      *OFF
052900971021     C     W001A         ORLT      '0'
053000971023     C     W001A         ANDNE     *BLANK
053100971021     C                   SETON                                        32
053200971021     C                   MOVE      *ZERO         VABVLB
053300010604     C                   eval      vinmsg = %trimr(vinmsg)
053400010604     C                             + ' ' + 'VHBVLB'
053500971021     C                   ELSE
053600971021     C                   MOVEL     WI07          VABVLB
053700971023     C                   TESTN                   WD07                 30
053800971023     C  N30              TESTN                   WD07                   30
053900971023     C  N30              TESTN                   WD07                     30
054000971021     C                   MOVE      WD07          W001A             1
054100971021     C     *IN30         IFEQ      *OFF
054200971021     C     W001A         ORLT      '0'
054300971023     C     W001A         ANDNE     *BLANK
054400971021     C                   SETON                                        32
054500971021     C                   MOVE      *ZERO         VABVLB
054600010604     C                   eval      vinmsg = %trimr(vinmsg)
054700010604     C                             + ' ' + 'VHBVLB'
054800971021     C                   ELSE
054900971021     C                   MOVE      WD07          VABVLB
055000971021     C                   ENDIF
055100050118     C                   ENDIF
055200041001     C                   ELSE
055300041001     C* ...altrimenti forzo azzeramento del volume
055400041001     C                   Z-ADD     *zeros        VABVLB
055500041001     C                   ENDIF
055600941229     C*
055700971021     C                   MOVE      *ZERO         VABQFT
055800971021     C                   MOVE      VHBQFT        WASC15
055900971023     C                   TESTN                   WI15                 30
056000971023     C  N30              TESTN                   WI15                   30
056100971023     C  N30              TESTN                   WI15                     30
056200971021     C                   MOVE      WI15          W001A             1
056300971021     C     *IN30         IFEQ      *OFF
056400971021     C     W001A         ORLT      '0'
056500971023     C     W001A         ANDNE     *BLANK
056600971021     C                   SETON                                        32
056700971021     C                   MOVE      *ZERO         VABQFT
056800010604     C                   eval      vinmsg = %trimr(vinmsg)
056900010604     C                             + ' ' + 'VHBQFT'
057000971021     C                   ELSE
057100971021     C                   MOVEL     WI15          VABQFT
057200971023     C                   TESTN                   WD15                 30
057300971023     C  N30              TESTN                   WD15                   30
057400971023     C  N30              TESTN                   WD15                     30
057500971021     C                   MOVE      WD15          W001A             1
057600971021     C     *IN30         IFEQ      *OFF
057700971021     C     W001A         ORLT      '0'
057800971023     C     W001A         ANDNE     *BLANK
057900971021     C                   SETON                                        32
058000971021     C                   MOVE      *ZERO         VABQFT
058100010604     C                   eval      vinmsg = %trimr(vinmsg)
058200010604     C                             + ' ' + 'VHBQFT'
058300971021     C                   ELSE
058400971021     C                   MOVE      WD15          VABQFT
058500971021     C                   ENDIF
058600050118     C                   ENDIF
058700941229     C*
058800971021     C                   MOVE      *ZERO         VABCAS
058900971021     C                   MOVE      VHBCAS        WASC15
059000971023     C                   TESTN                   WI15                 30
059100971023     C  N30              TESTN                   WI15                   30
059200971023     C  N30              TESTN                   WI15                     30
059300971021     C                   MOVE      WI15          W001A             1
059400971021     C     *IN30         IFEQ      *OFF
059500971021     C     W001A         ORLT      '0'
059600971023     C     W001A         ANDNE     *BLANK
059700971021     C                   SETON                                        32
059800971021     C                   MOVE      *ZERO         VABCAS
059900010604     C                   eval      vinmsg = %trimr(vinmsg)
060000010604     C                             + ' ' + 'VHBCAS'
060100971021     C                   ELSE
060200971021     C                   MOVEL     WI15          VABCAS
060300971023     C                   TESTN                   WD15                 30
060400971023     C  N30              TESTN                   WD15                   30
060500971023     C  N30              TESTN                   WD15                     30
060600971021     C                   MOVE      WD15          W001A             1
060700971021     C     *IN30         IFEQ      *OFF
060800971021     C     W001A         ORLT      '0'
060900971023     C     W001A         ANDNE     *BLANK
061000971021     C                   SETON                                        32
061100971021     C                   MOVE      *ZERO         VABCAS
061200010604     C                   eval      vinmsg = %trimr(vinmsg)
061300010604     C                             + ' ' + 'VHBCAS'
061400971021     C                   ELSE
061500971021     C                   MOVE      WD15          VABCAS
061600971021     C                   ENDIF
061700040827     C                   ENDIF
061800971021     C*
061900971023     C                   TESTN                   VHBRMN               30
062000971023     C  N30              TESTN                   VHBRMN                 30
062100971023     C  N30              TESTN                   VHBRMN                   30
062200971021     C                   MOVE      VHBRMN        W001A             1
062300971021     C     *IN30         IFEQ      *OFF
062400971021     C     W001A         ORLT      '0'
062500971021     C                   SETON                                        32
062600971021     C                   MOVE      *ZERO         VABRMN
062700010604     C                   eval      vinmsg = %trimr(vinmsg)
062800010604     C                             + ' ' + 'VHBRMN'
062900971021     C                   ELSE
063000971021     C                   MOVE      VHBRMN        VABRMN
063100971021     C                   ENDIF
063200971021     C*
063300971023     C                   TESTN                   VHBNCD               30
063400971023     C  N30              TESTN                   VHBNCD                 30
063500971023     C  N30              TESTN                   VHBNCD                   30
063600971021     C                   MOVE      VHBNCD        W001A             1
063700971021     C     *IN30         IFEQ      *OFF
063800971021     C     W001A         ORLT      '0'
063900971023     C     W001A         ANDNE     *BLANK
064000971021     C                   SETON                                        32
064100971021     C                   MOVE      *ZERO         VABNCD
064200010604     C                   eval      vinmsg = %trimr(vinmsg)
064300010604     C                             + ' ' + 'VHBNCD'
064400971021     C                   ELSE
064500971021     C                   MOVE      VHBNCD        VABNCD
064600971021     C                   ENDIF
064700971021     C*
064800971023     C                   TESTN                   VHBNCA               30
064900971023     C  N30              TESTN                   VHBNCA                 30
065000971023     C  N30              TESTN                   VHBNCA                   30
065100971021     C                   MOVE      VHBNCA        W001A             1
065200971021     C     *IN30         IFEQ      *OFF
065300971021     C     W001A         ORLT      '0'
065400971023     C     W001A         ANDNE     *BLANK
065500971021     C                   SETON                                        32
065600971021     C                   MOVE      *ZERO         VABNCA
065700010604     C                   eval      vinmsg = %trimr(vinmsg)
065800010604     C                             + ' ' + 'VHBNCA'
065900971021     C                   ELSE
066000971021     C                   MOVE      VHBNCA        VABNCA
066100971021     C                   ENDIF
066200971021     C*
066300971023     C                   TESTN                   VHBZNC               30
066400971023     C  N30              TESTN                   VHBZNC                 30
066500971023     C  N30              TESTN                   VHBZNC                   30
066600971021     C                   MOVE      VHBZNC        W001A             1
066700971021     C     *IN30         IFEQ      *OFF
066800971021     C     W001A         ORLT      '0'
066900971023     C     W001A         ANDNE     *BLANK
067000971021     C                   SETON                                        32
067100971021     C                   MOVE      *ZERO         VABZNC
067200010604     C                   eval      vinmsg = %trimr(vinmsg)
067300010604     C                             + ' ' + 'VHBZNC'
067400971021     C                   ELSE
067500971021     C                   MOVE      VHBZNC        VABZNC
067600971021     C                   ENDIF
067700971021     C*
067800971023     C                   TESTN                   VHBDCR               30
067900971023     C  N30              TESTN                   VHBDCR                 30
068000971023     C  N30              TESTN                   VHBDCR                   30
068100971021     C                   MOVE      VHBDCR        W001A             1
068200971021     C     *IN30         IFEQ      *OFF
068300971021     C     W001A         ORLT      '0'
068400971023     C     W001A         ANDNE     *BLANK
068500971021     C                   SETON                                        32
068600971021     C                   MOVE      *ZERO         VABDCR
068700010604     C                   eval      vinmsg = %trimr(vinmsg)
068800010604     C                             + ' ' + 'VHBDCR'
068900971021     C                   ELSE
069000971021     C                   MOVE      VHBDCR        VABDCR
069100971021     C                   ENDIF
069200971021     C*
069300971023     C                   TESTN                   VHBHCR               30
069400971023     C  N30              TESTN                   VHBHCR                 30
069500971023     C  N30              TESTN                   VHBHCR                   30
069600971021     C                   MOVE      VHBHCR        W001A             1
069700971021     C     *IN30         IFEQ      *OFF
069800971021     C     W001A         ORLT      '0'
069900971023     C     W001A         ANDNE     *BLANK
070000971021     C                   SETON                                        32
070100971021     C                   MOVE      *ZERO         VABHCR
070200010604     C                   eval      vinmsg = %trimr(vinmsg)
070300010604     C                             + ' ' + 'VHBHCR'
070400971021     C                   ELSE
070500971021     C                   MOVE      VHBHCR        VABHCR
070600971021     C                   ENDIF
070700941229     C*
070800971021     C                   MOVE      *ZERO         VABVMD
070900971021     C                   MOVE      VHBVMD        WASC15
071000971023     C                   TESTN                   WI15                 30
071100971023     C  N30              TESTN                   WI15                   30
071200971023     C  N30              TESTN                   WI15                     30
071300971021     C                   MOVE      WI15          W001A             1
071400971021     C     *IN30         IFEQ      *OFF
071500971021     C     W001A         ORLT      '0'
071600971023     C     W001A         ANDNE     *BLANK
071700971021     C                   SETON                                        32
071800971021     C                   MOVE      *ZERO         VABVMD
071900010604     C                   eval      vinmsg = %trimr(vinmsg)
072000010604     C                             + ' ' + 'VHBVMD'
072100971021     C                   ELSE
072200971021     C                   MOVEL     WI15          VABVMD
072300971023     C                   TESTN                   WD15                 30
072400971023     C  N30              TESTN                   WD15                   30
072500971023     C  N30              TESTN                   WD15                     30
072600971021     C                   MOVE      WD15          W001A             1
072700971021     C     *IN30         IFEQ      *OFF
072800971021     C     W001A         ORLT      '0'
072900971023     C     W001A         ANDNE     *BLANK
073000971021     C                   SETON                                        32
073100971021     C                   MOVE      *ZERO         VABVMD
073200010604     C                   eval      vinmsg = %trimr(vinmsg)
073300010604     C                             + ' ' + 'VHBVMD'
073400971021     C                   ELSE
073500971021     C                   MOVE      WD15          VABVMD
073600971021     C                   ENDIF
073700050118     C                   ENDIF
073800971021     C*
073900971023     C                   TESTN                   VHBANT               30
074000971023     C  N30              TESTN                   VHBANT                 30
074100971023     C  N30              TESTN                   VHBANT                   30
074200971021     C                   MOVE      VHBANT        W001A             1
074300971021     C     *IN30         IFEQ      *OFF
074400971021     C     W001A         ORLT      '0'
074500971023     C     W001A         ANDNE     *BLANK
074600971021     C                   SETON                                        32
074700971021     C                   MOVE      *ZERO         VABANT
074800010604     C                   eval      vinmsg = %trimr(vinmsg)
074900010604     C                             + ' ' + 'VHBANT'
075000971021     C                   ELSE
075100971021     C                   MOVE      VHBANT        VABANT
075200971021     C                   ENDIF
075300971021     C*
075400971021     C* CAMPI ALFABETICI:
075500971021     C                   MOVE      VHBATB        VABATB
075600971021     C                   MOVE      VHBCBO        VABCBO
075700971021     C                   MOVE      VHBRSD        VABRSD
075800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
075900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
076000020117     C* ==
076100971021     C                   MOVE      VHBRD2        VABRD2
076200971021     C                   MOVE      VHBIND        VABIND
076300971021     C                   MOVE      VHBCAD        VABCAD
076400971021     C                   MOVE      VHBLOD        VABLOD
076500971021     C                   MOVE      VHBPRD        VABPRD
076600971021     C                   MOVE      VHBNZD        VABNZD
076700971021     C                   MOVE      VHBGC1        VABGC1
076800971021     C                   MOVE      VHBGC2        VABGC2
076900971021     C                   MOVE      VHBTSP        VABTSP
077000971021     C                   MOVE      VHBVAS        VABVAS
077100971021     C                   MOVE      VHBNAS        VABNAS
077200971021     C                   MOVE      VHBTIC        VABTIC
077300971021     C                   MOVE      VHBVCA        VABVCA
077400971021     C                   MOVE      VHBGCA        VABGCA
077500971021     C                   MOVE      VHBRMA        VABRMA
077600971021     C                   MOVE      VHBXCO        VABXCO
077700971021     C                   MOVE      VHBNOT        VABNOT
077800971021     C                   MOVE      VHBNT2        VABNT2
077900971021     C                   MOVE      VHBCTM        VABCTM
078000971021     C                   MOVE      VHBFFD        VABFFD
078100971021     C                   MOVE      VHBTCR        VABTCR
078200971021     C                   MOVE      VHBCTS        VABCTS
078300971021     C                   MOVE      VHBFTM        VABFTM
078400971021     C                   MOVE      VHBVAD        VABVAD
078500971021     C                   MOVE      VHBGMA        VABGMA
078600971021     C                   MOVE      VHBGGA        VABGGA
078700971021     C                   MOVE      VHBGVA        VABGVA
078800971021     C                   MOVE      VHBTC1        VABTC1
078900971021     C                   MOVE      VHBTC2        VABTC2
079000971021     C                   MOVE      VHBSCL        VABSCL
079100971021     C                   MOVE      VHBRMO        VABRMO
079200971021     C                   MOVE      VHBCMO        VABCMO
079300971021     C                   MOVE      VHBNMO        VABNMO
079400971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
079500060329     C* NELLA NAZIONE MITTENTE ORIGINALE
079600060331     C                   IF        %trim(VHBNMO) < 'A'
079700060329     C                   MOVE      *BLANKS       VABNMO
079800060329     C                   ENDIF
079900050418     C*
080000050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
080100050418     C                   IF        %trim(VABNZD) = 'I'   OR
080200050418     C                             %trim(VABNZD) = 'IT'  OR
080300050418     C                             %trim(VABNZD) = 'ITA'
080400050418     C                   EVAL      VABNZD = *blanks
080500050418     C                   ENDIF
080600041025     C*
080700041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
080800041025     C                   IF        *IN52 = *ON
080900041025     C* Reperisco la provincia dal CAP e dalla località
081000041025     C                   IF        VABCAD <> *blanks AND
081100050907     C                             VABPRD  = *blanks AND
081200050907     C                             VABNZD  = *blanks
081300041025     C                   CLEAR                   TISI95DS
081400041025     C                   EVAL      I95TCN = '3'
081500041025     C                   Z-ADD     datcor        I95DAT
081600041025     C                   EVAL      I95CAP = VABCAD
081700041025     C                   EVAL      I95LOC = VABLOD
081800041025     C                   CALL      'TISI95R'
081900041025     C                   PARM                    TISI95DS
082000041025     C                   EVAL      VABPRD = O95PRV
082100041025     C                   ENDIF
082200041025     C                   ENDIF
082300090216     C*
082400090216     C* Forzatura x aggiungere il secolo al riferimenti mittente numerico passato
082500090216     C* dal cliente (x T&T via link diretto XML)
082600090216     C                   MOVEL     *blanks       wRMN_Alfa15      15
082700090216     C                   MOVE(P)   VABRMN        wRMN_Alfa06       6
082800090216     C                   MOVE(P)   VABAAS        wAAS_Alfa         2
082900090216     C                   EVALR     wRMN_Alfa15=wAAS_Alfa+wRMN_Alfa06
083000090216     C                   MOVE      wRMN_Alfa15   VABRMN
083100040630     C*
083200040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
083300040630     C                   exsr      exeBon
083400930409     C*
083500010604     C  N31              ADD       1             §CTROK            7 0
083600010604     C   32              ADD       1             §CTRMO            7 0
083700010604     C   31              ADD       1             §CTRNO            7 0
083800020916     C  N31              WRITE     FIVAB000
083900990910     C*
084000010601     C                   if        *in31 = *off and
084100010601     C                             *in32 = *off
084200990910     C                   eval      vinflg = '1'
084300990910     C                   else
084400000621     C                   eval      recko = vindta
084500010612     C                   if        %open(tis7t4ps)
084600030407     C   50              except    rigadett
084700010612     C                   endif
084800990910     C                   eval      vinflg = '2'
084900990910     C                   endif
085000000905     C                   endif
085100000905     C                   endif
085200010601     C*
085300010601     C                   endif
085400000905     C*
085500000905     C                   else
085600000905     C                   eval      vinflg = '1'
085700000905     C                   endif
085800000905     C*
085900000905     C  N70              update    tivin000
086000000905     C*
086100991022     C  N70              ENDdo
086200010601     C
086300010601     C*
086400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
086500010601     C                   if        cntNonEl = *zeros or
086600010601     C                             flgMulti = '0'
086700990910     C* Se non ci sono record con errori ...
086800010601     C                   if        §ctrno = 0 and
086900010601     C                             §ctrmo = 0 and
087000010601     C                             flgStato <> '2'
087100990910     C* ... restituisco esito OK.
087200990921     C                   eval      wrkesito = '0'
087300990910     C                   else
087400010601     C                   if        §ctrok > 0
087500000710     C                   eval      wrkesito = '1'
087600000710     C                   else
087700010615     C                   if        flgOk = '0'
087800010615     C                   eval      wrkesito = '2'
087900010615     C                   else
088000010615     C                   eval      wrkesito = '6'
088100010615     C                   endif
088200000710     C                   endif
088300010615     C                   endif
088400010601     C                   else
088500010601     C                   eval      wrkesito = '9'
088600010601     C                   endif
088700990910     C*
088800990914     C                   if        %open(tivin00r)
088900990908     C                   close     tivin00r
089000990914     C                   endif
089100020916     C                   if        %open(fivabwwr)
089200020916     C                   close     fivabwwr
089300990914     C                   endif
089400010601     C*
089500010601     C                   if        vlrpoi <> 999
089600010601     C                   eval      invfil = vlrpoi
089700010601     C                   endif
089800990910     C*
089900990920     C                   if        §ctrok > 0
090000010601     C                             and invfil > *zeros
090100990920     C                   exsr      invio
090200990920     C                   endif
090300010612     C*
090400010612     C                   if        flgGiro = '1'
090500010612     C                   exsr      endela
090600010612     C                   endif
090700990920     C*
090800910830     C                   ENDSR
090900000613     C***
091000990920
091100010601
091200010601
091300010601
091400010601
091500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
091600010601     C     repfil        BEGSR
091700010601     C*
091800010601     C                   if        invfil = *zeros and
091900010601     C                             depfil > *zeros and
092000010613     C                             (vinflg = *blanks or
092100010613     C                              vinflg = *zeros)
092200010601     C
092300010601     C                   eval      invfil = depfil
092400010601     C                   endif
092500010601     C*
092600010601     C                   if        depfil <> invfil and
092700010601     C                             invfil > *zeros
092800010601     C                   eval      flgMulti = '1'
092900010601     C                   if        vinflg = *blanks
093000010601     C                   add       1             cntNonEl
093100010601     C                   endif
093200010601     C                   endif
093300010601     C*
093400010601     C                   if        vinflg = '2'
093500010601     C                   eval      flgStato = '2'
093600010601     C                   endif
093700010601     C*
093800010601     C                   ENDSR
093900010601     C***
094000010601
094100010601
094200010601
094300010601
094400010601
094500010601
094600010601
094700990920      /TITLE Invio dei dati al punto operativo.
094800000613     C     invio         BEGSR
094900990920     C*
095000990920     C                   reset                   dscmz
095100010601     C                   move      invfil        cmzdst
095200990920     C                   eval      cmzfld = vlrfou
095300990920     C                   eval      cmzmbd = vlrhdl
095400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
095500000710     C                   if        prmfir = *blanks
095600020916     C                   eval      cmzfla = 'FIVAB00F'
095700020916     C                   eval      cmzmba = 'FIVAB00F'
095800000710     C                   else
095900000710     C                   eval      cmzfla = prmfir
096000000710     C                   eval      cmzmba = prmfir
096100000710     C                   endif
096200990920     C                   eval      cmznrr = *zeros
096300990920     C                   move      §ctrok        cmznrr
096400021003     C                   eval      cmzlba = vlrfl1
096500990920     C                   call(e)   'TIS711C'
096600990920     C                   parm                    dscmz
096700990921     C                   parm      *blanks       esito
096800990920     C                   if        %error
096900990920     C                             or cmzerr = '1'
097000990921     C                             or esito  = '1'
097100000710     C                   eval      wrkesito = '3'
097200990920     C                   endif
097300990920     C*
097400000613     C                   ENDSR
097500010601
097600010601
097700010601
097800010601
097900010601      /TITLE Invio dei dati al punto operativo.
098000010601     C     opeini        BEGSR
098100010601     C*
098200010601     C* Inizializzo flag e contatori operativi
098300010601     C                   movel     '0'           flgGiro           1
098400010601     C                   movel     '0'           flgMulti          1
098500010601     C                   movel     '1'           flgStato          1
098600010615     C                   movel     '0'           flgOk             1
098700010601     C                   z-add     *zeros        cntNonEl         10 0
098800010601     C                   z-add     *zeros        depfil            3 0
098900010601     C                   z-add     *zeros        invfil            3 0
099000030407     C*
099100030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
099200030407     C                   if        %subst(vlrppt:1:1) = 'S'
099300030407     C                   seton                                        50
099400030407     C                   else
099500030407     C                   setoff                                       50
099600030407     C                   endif
099700041001     C*
099800041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
099900041001     C                   if        %subst(vlrppt:2:1) = 'S'
100000041001     C                   seton                                        51
100100041001     C                   else
100200041001     C                   setoff                                       51
100300041001     C                   endif
100400041025     C*
100500041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
100600041025     C* (naturalmente solo se nn già presente)
100700041025     C                   if        %subst(vlrppt:3:1) = 'S'
100800041025     C                   seton                                        52
100900041025     C                   else
101000041025     C                   setoff                                       52
101100041025     C                   endif
101200010601     C*
101300010601     C                   ENDSR
101400010601     C***
101500040630
101600040630
101700040630
101800040630
101900040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
102000040630     C     exeBon        BEGSR
102100040630     C*
102200040630     C* Imposto i parametri x il *pgm d "bonifica"
102300040630     C                   EVAL      ElencoChar = *blanks
102400040630     C                   EVAL      TipoElenco = *blanks
102500040630     C                   EVAL      CharSost   = *blanks
102600040630     C                   EVAL      UpperCase  = '1'
102700040630     C                   EVAL      ChkNull    = '1'
102800040630     C                   EVAL      CharNull   = *blanks
102900040630     C*
103000040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
103100040630     C                   EVAL      TxtInOut   = VABRSD
103200040630     C                   EVAL      EsitoBon   = *blanks
103300040630     C*
103400040630     C                   CALL      'XCHKCHAR'
103500040630     C                   PARM                    TxtInOut
103600040630     C                   PARM                    ElencoChar
103700040630     C                   PARM                    TipoElenco
103800040630     C                   PARM                    CharSost
103900040630     C                   PARM                    UpperCase
104000040630     C                   PARM                    ChkNull
104100040630     C                   PARM                    CharNull
104200040630     C                   PARM                    EsitoBon
104300040630     C*
104400040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
104500040630     C                   IF        EsitoBon = '1'
104600040630     C                   EVAL      VABRSD = TxtInOut
104700040630     C                   ENDIF
104800040630     C*
104900040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
105000040630     C                   EVAL      TxtInOut   = VABIND
105100040630     C                   EVAL      EsitoBon   = *blanks
105200040630     C*
105300040630     C                   CALL      'XCHKCHAR'
105400040630     C                   PARM                    TxtInOut
105500040630     C                   PARM                    ElencoChar
105600040630     C                   PARM                    TipoElenco
105700040630     C                   PARM                    CharSost
105800040630     C                   PARM                    UpperCase
105900040630     C                   PARM                    ChkNull
106000040630     C                   PARM                    CharNull
106100040630     C                   PARM                    EsitoBon
106200040630     C*
106300040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
106400040630     C                   IF        EsitoBon = '1'
106500040630     C                   EVAL      VABIND = TxtInOut
106600040630     C                   ENDIF
106700040630     C*
106800040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
106900040630     C                   EVAL      TxtInOut   = VABLOD
107000040630     C                   EVAL      EsitoBon   = *blanks
107100040630     C*
107200040630     C                   CALL      'XCHKCHAR'
107300040630     C                   PARM                    TxtInOut
107400040630     C                   PARM                    ElencoChar
107500040630     C                   PARM                    TipoElenco
107600040630     C                   PARM                    CharSost
107700040630     C                   PARM                    UpperCase
107800040630     C                   PARM                    ChkNull
107900040630     C                   PARM                    CharNull
108000040630     C                   PARM                    EsitoBon
108100040630     C*
108200040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
108300040630     C                   IF        EsitoBon = '1'
108400040630     C                   EVAL      VABLOD = TxtInOut
108500040630     C                   ENDIF
108600040630     C*
108700040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
108800040630     C                   EVAL      TxtInOut   = VABNOT
108900040630     C                   EVAL      EsitoBon   = *blanks
109000040630     C                   EVAL      UpperCase  = *blanks
109100040630     C*
109200040630     C                   CALL      'XCHKCHAR'
109300040630     C                   PARM                    TxtInOut
109400040630     C                   PARM                    ElencoChar
109500040630     C                   PARM                    TipoElenco
109600040630     C                   PARM                    CharSost
109700040630     C                   PARM                    UpperCase
109800040630     C                   PARM                    ChkNull
109900040630     C                   PARM                    CharNull
110000040630     C                   PARM                    EsitoBon
110100040630     C*
110200040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
110300040630     C                   IF        EsitoBon = '1'
110400040630     C                   EVAL      VABNOT = TxtInOut
110500040630     C                   ENDIF
110600040630     C*
110700040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
110800040630     C                   EVAL      TxtInOut   = VABNT2
110900040630     C                   EVAL      EsitoBon   = *blanks
111000040630     C                   EVAL      UpperCase  = *blanks
111100040630     C*
111200040630     C                   CALL      'XCHKCHAR'
111300040630     C                   PARM                    TxtInOut
111400040630     C                   PARM                    ElencoChar
111500040630     C                   PARM                    TipoElenco
111600040630     C                   PARM                    CharSost
111700040630     C                   PARM                    UpperCase
111800040630     C                   PARM                    ChkNull
111900040630     C                   PARM                    CharNull
112000040630     C                   PARM                    EsitoBon
112100040630     C*
112200040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
112300040630     C                   IF        EsitoBon = '1'
112400040630     C                   EVAL      VABNT2 = TxtInOut
112500040630     C                   ENDIF
112600040630     C*
112700040630     C                   ENDSR
112800040630
112900010601
113000010601
113100990910
113200000613     C     *inzsr        BEGSR
113300990910     C*
113400990910     C     *entry        plist
113500990920     C                   parm                    tivlrds
113600990921     C                   parm      wrkesito      esito
113700000724     C                   parm                    prmlit
113800000710     C                   parm                    prmfir
113900010606     C*
114000010606     C* CALCOLA LA DATA CORRENTE
114100010606     C                   time                    wn14             14 0
114200010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
114300010606     C                   z-add     wn8           g08dat
114400010606     C                   z-add     *zeros        g08inv
114500010606     C                   movel     '0'           g08err
114600010606     C                   call      'XSRDA8'
114700010606     C                   parm                    wlbda8
114800010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
114900000613     C*
115000000613     C                   ENDSR
115100000613     C***
115200990908
115300000613     Otis7t4p   E            riepilogo         2
115400990915     O                                              'Upload via Internet'
115500990915     O                                           +1 'Traduzione TIVIN00R -'
115600020916     O                                           +1 'FIVABWWR'
115700990915     O                                           +1 'Testate bolle'
115800990915     O          E            riepilogo   2
115900990915     O                       wrkdata
116000990915     O                       wrkora              +1
116100990915     O                       procname            +1
116200990915     O          E            riepilogo   2
116300990915     O                                              'Cliente..................:'
116400990915     O                       VABCCM        z     +1
116500990915     O          E            riepilogo   2
116600990920     O                                              'Riferimento Strategi.....:'
116700990920     O                       vlrhdl              +1
116800990915     O          E            riepilogo   2
116900990915     O                                              'Giusti...................:'
117000971022     O                       §CTROK        2   +  1
117100990915     O          E            riepilogo   2
117200990915     O                                              'Sbagliati e corretti.....:'
117300971022     O                       §CTRMO        2   +  1
117400990915     O          E            riepilogo   2
117500990915     O                                              'Sbagliati e scartati.....:'
117600971022     O                       §CTRNO        2   +  1
117700000613
117800000616     Otis7t4ps  E            testdett          2
117900000613     O                                              'Upload via Internet'
118000000613     O                                           +1 'Traduzione TIVIN00R -'
118100020916     O                                           +1 'FIVABWWR'
118200000613     O                                           +1 'Testate bolle'
118300000616     O          E            testdett    3
118400000613     O                                           +2 'N° rec'
118500000613     O                                           +3 'Anteprima contenuto'
118600000616     O          E            rigadett    2
118700000613     O                       rrnum               +2
118800000621     O                       recko               +3
118900000616     O          E            findett     2
119000000613     O                       wrkdata
119100000613     O                       wrkora              +1
119200000613     O                       procname            +1
119300000616     O          E            findett     2
119400000613     O                                              'Cliente..................:'
119500000613     O                       VABCCM        z     +1
119600000616     O          E            findett     2
119700000613     O                                              'Riferimento Strategi.....:'
119800000613     O                       vlrhdl              +1
119900000616     O          E            findett     2
120000000613     O                                              'Giusti...................:'
120100000613     O                       §CTROK        2   +  1
120200000616     O          E            findett     2
120300000613     O                                              'Sbagliati e corretti.....:'
120400000613     O                       §CTRMO        2   +  1
120500000616     O          E            findett     2
120600000613     O                                              'Sbagliati e scartati.....:'
120700000613     O                       §CTRNO        2   +  1
120800000613** CMD - COMANDI CL
120900011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4P)  FORMTYPE(RICCLI) OUTQ(
121000011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4PS) FORMTYPE(RICCLI) OUTQ(
121100000616DLTOVR FILE(TIS7T4P TIS7T4PS) LVL(*)
121200000613
121300000613
