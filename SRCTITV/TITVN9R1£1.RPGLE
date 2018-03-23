000100090108      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090108     FEDIVABwr  O    E             DISK    usropn
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
005400080611     D vhbtelx                89    103
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
011400010606     D*------------------
011500010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
011600010606     D*------------------
011700010606     D WLBDA8          DS                  INZ
011800010606     D  G08DAT                 1      8  0
011900010606     D  G08INV                 9     16  0
012000010606     D  G08ERR                17     17
012100010606     D  G08TGI                18     22  0
012200010606     D*
012300040407     D TxtInOut        S           2048
012400040407     D ElencoChar      S            256
012500040407     D TipoElenco      S              1
012600040407     D CharSost        S              1
012700040407     D UpperCase       S              1
012800040407     D ChkNull         S              1
012900040407     D CharNull        S              1
013000040407     D EsitoBon        S              1
013100010606
013200010606
013300010606
013400010606
013500990915     C                   time                    wrkdata
013600990915     C                   time                    wrkora
013700000913     C                   reset                   rrnum
013800990921     C                   reset                   esito
013900990921     C                   reset                   wrkesito
014000010601     C*
014100010601     C                   exsr      opeini
014200010601     C                   exsr      rwvab
014300041025     C*
014400041025     C* Esegue lancio TISI95R solo x chiusura
014500041025     C                   CLEAR                   TISI95DS
014600041025     C                   EVAL      I95TLA = 'C'
014700041025     C                   CALL      'TISI95R'
014800041025     C                   PARM                    TISI95DS
014900010601     C*
015000010601     C                   seton                                        lr
015100010601
015200010601
015300010601
015400010601
015500010601     C*--------------------------------------------------------
015600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
015700010601     C*--------------------------------------------------------
015800010601     C     PREELA        BEGSR
015900010601     C*
016000010601     C* SE OCCORRE SPEDIRE IN FILIALE
016100010601     C                   if        invfil <> *zeros and
016200010601     C                             flgGiro = '0'
016300010601     C*
016400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
016500010601     C                   eval      flgGiro = '1'
016600000724     C*
016700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
016800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016900010601     C     invfil        chain     azorg01l
017000000724     C                   if        %found
017100000724     C                   movel(p)  CMD(1)        depcmd
017200020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
017300000724     C*
017400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
017500011123     C                   Z-ADD     150           LENGH            15 5
017600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017700000724     C                   PARM                    depcmd
017800000724     C                   PARM                    LENGH
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
019100000613     C*
019200000616     C                   if        not %open(tis7t4ps)
019300000616     C                   open      tis7t4ps
019400030407     C   50              except    testdett
019500000613     C                   endif
019600010601     C*
019700010601     C                   endif
019800010601     C*
019900010601     C                   ENDSR
020000010601     C***
020100010601
020200010601
020300010601
020400010601
020500010601
020600010601
020700010601     C*--------------------------------------------------------
020800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
020900010601     C*--------------------------------------------------------
021000010601     C     ENDELA        BEGSR
021100990915     C*
021200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
021300000613     C*
021400000616     C                   if        %open(tis7t4ps)
021500030407     C   50              except    findett
021600000616     C                   close     tis7t4ps
021700000613     C                   endif
021800000616     C*
021900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
022000011123     C                   Z-ADD     150           LENGH            15 5
022100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
022200000616     C                   PARM                    CMD(3)
022300000616     C                   PARM                    LENGH
022400010601     C*
022500010601     C                   ENDSR
022600010601     C***
022700010601
022800010601
022900000613
023000000613
023100000613
023200000613     C*--------------------------------------------------------
023300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
023400000613     C*--------------------------------------------------------
023500000621     C     STPR          BEGSR
023600000613     C*
023700000613     C                   if        not %open(tis7t4p)
023800000613     C                   open      tis7t4p
023900990915     C                   endif
024000990915     C*
024100030407     C   50              except    riepilogo
024200990915     C*
024300000613     C                   if        %open(tis7t4p)
024400000613     C                   close     tis7t4p
024500990914     C                   endif
024600000613     C*
024700000613     C                   ENDSR
024800000613     C***
024900000613
025000010601
025100010601
025200010601
025300010601
025400910830     C*--------------------------------------------------------
025500090108     C* RWVAB   LEGGE tivin00r E SCRIVE EDIVABWF              *
025600910830     C*--------------------------------------------------------
025700941229     C     RWVAB         BEGSR
025800990910     C*
025900990914     C                   if        not %open(tivin00r)
026000990908     C                   open      tivin00r
026100990914     C                   endif
026200090108     C                   if        not %open(edivabwr)
026300090108     C                   open      edivabwr
026400990914     C                   endif
026500990910     C*
026600990910     C                   clear                   §CTROK
026700990910     C                   clear                   §CTRMO
026800990910     C                   clear                   §CTRNO
026900990910     C*
027000921023     C                   DO        *HIVAL
027100990913     C*
027200990915     C                   READ      tivin00r                               70
027300010618     C*
027400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
027500010618     C                   if        vinflg = '1'
027600010618     C                   eval      flgOk = '1'
027700010618     C                   endif
027800010618     C*
027900000905     C                   if        vindta > *blanks
028000000613     C                   add       1             rrnum
028100990913     C*
028200010601     C                   if        *in70 = *off and
028300990913     C                             (vinflg = *blanks
028400010711     C                              or vinflg = '0'
028500010711     C                              or vinflg = '2')
028600990913     C*
028700090108     C                   clear                   EDIVAB00
028800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028900010711     C                   if        vinflg = *blanks or vinflg = '0'
029000010711     C                   clear                   vinmsg
029100010711     C                   endif
029200010711     C*
029300000313     C                   eval      x_rec  = vindta
029400991022     C*
029500991022     C                   testb     x'3F'         vhbatb                   02
029600991022     C                   if        not *in02
029700991022     C*
029800971021     C                   SETOFF                                       303132
029900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
030000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
030100971023     C*    OPPURE TUTTI BLANKS
030200010601     C*
030300010601     C                   TESTN                   VHBLNP               30
030400010601     C  N30              TESTN                   VHBLNP                 30
030500010601     C  N30              TESTN                   VHBLNP                   30
030600010601     C                   MOVE      VHBLNP        W001A             1
030700010601     C     *IN30         IFEQ      *OFF
030800010601     C     W001A         ORLT      '0'
030900010601     C                   SETON                                        31
031000010601     C                   eval      vinmsg = %trimr(vinmsg)
031100010601     C                             + ' ' + 'VHBLNP'
031200010601     C                   ELSE
031300010601     C                   MOVE      VHBLNP        VABLNP
031400010601     C     VABLNP        IFLE      *ZEROS
031500010601     C                   SETON                                        31
031600010601     C                   eval      vinmsg = %trimr(vinmsg)
031700010601     C                             + ' ' + 'VHBLNP'
031800010601     C                   ENDIF
031900010601     C                   ENDIF
032000010601     C*
032100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
032200010601     C                   eval      depfil = VABLNP
032300010601     C                   exsr      repfil
032400010601     C                   if        depfil = invfil
032500021014     C                   if        vlrpoi = 999
032600021010     C                   MOVE(P)   invfil        VABFGS
032700021014     C                   else
032800021014     C                   MOVE(P)   vlrpoi        VABFGS
032900021014     C                   endif
033000010601     C*
033100010601     C                   exsr      PREELA
033200010601     C*
033300971023     C                   TESTN                   VHBCCM               30
033400971023     C  N30              TESTN                   VHBCCM                 30
033500971023     C  N30              TESTN                   VHBCCM                   30
033600971021     C                   MOVE      VHBCCM        W001A             1
033700971021     C     *IN30         IFEQ      *OFF
033800971021     C     W001A         ORLT      '0'
033900971021     C                   SETON                                        31
034000010604     C                   eval      vinmsg = %trimr(vinmsg)
034100010604     C                             + ' ' + 'VHBCCM'
034200971021     C                   ELSE
034300971021     C                   MOVE      VHBCCM        VABCCM
034400971024     C     VABCCM        IFLE      *ZEROS
034500971024     C                   SETON                                        31
034600010604     C                   eval      vinmsg = %trimr(vinmsg)
034700010604     C                             + ' ' + 'VHBCCM'
034800971024     C                   ENDIF
034900971021     C                   ENDIF
035000971021     C*
035100971023     C                   TESTN                   VHBAAS               30
035200971023     C  N30              TESTN                   VHBAAS                 30
035300971023     C  N30              TESTN                   VHBAAS                   30
035400971021     C                   MOVE      VHBAAS        W001A             1
035500971021     C     *IN30         IFEQ      *OFF
035600971021     C     W001A         ORLT      '0'
035700971023     C     W001A         ANDNE     *BLANK
035800971021     C                   SETON                                        32
035900010606     C                   MOVEL     datcor        VABAAS
036000010604     C                   eval      vinmsg = %trimr(vinmsg)
036100010604     C                             + ' ' + 'VHBAAS'
036200971021     C                   ELSE
036300971021     C                   MOVE      VHBAAS        VABAAS
036400971021     C                   ENDIF
036500971021     C*
036600971023     C                   TESTN                   VHBMGS               30
036700971023     C  N30              TESTN                   VHBMGS                 30
036800971023     C  N30              TESTN                   VHBMGS                   30
036900971021     C                   MOVE      VHBMGS        W001A             1
037000971021     C     *IN30         IFEQ      *OFF
037100971021     C     W001A         ORLT      '0'
037200971023     C     W001A         ANDNE     *BLANK
037300971021     C                   SETON                                        32
037400010601     C                   MOVE      '0101'        VABMGS
037500010604     C                   eval      vinmsg = %trimr(vinmsg)
037600010604     C                             + ' ' + 'VHBMGS'
037700971021     C                   ELSE
037800971021     C                   MOVE      VHBMGS        VABMGS
037900971021     C                   ENDIF
038000971021     C*
038100971023     C                   TESTN                   VHBNRS               30
038200971023     C  N30              TESTN                   VHBNRS                 30
038300971023     C  N30              TESTN                   VHBNRS                   30
038400971021     C                   MOVE      VHBNRS        W001A             1
038500971021     C     *IN30         IFEQ      *OFF
038600971021     C     W001A         ORLT      '0'
038700971023     C     W001A         ANDNE     *BLANK
038800971021     C                   SETON                                        32
038900971021     C                   MOVE      *ZERO         VABNRS
039000010604     C                   eval      vinmsg = %trimr(vinmsg)
039100010604     C                             + ' ' + 'VHBNRS'
039200971021     C                   ELSE
039300971021     C                   MOVE      VHBNRS        VABNRS
039400971021     C                   ENDIF
039500971021     C*
039600971023     C                   TESTN                   VHBNSP               30
039700971023     C  N30              TESTN                   VHBNSP                 30
039800971023     C  N30              TESTN                   VHBNSP                   30
039900971021     C                   MOVE      VHBNSP        W001A             1
040000971021     C     *IN30         IFEQ      *OFF
040100971021     C     W001A         ORLT      '0'
040200971023     C     W001A         ANDNE     *BLANK
040300971021     C                   SETON                                        32
040400971021     C                   MOVE      *ZERO         VABNSP
040500010604     C                   eval      vinmsg = %trimr(vinmsg)
040600010604     C                             + ' ' + 'VHBNSP'
040700971021     C                   ELSE
040800971021     C                   MOVE      VHBNSP        VABNSP
040900971021     C                   ENDIF
041000971021     C*
041100971023     C                   TESTN                   VHBLNA               30
041200971023     C  N30              TESTN                   VHBLNA                 30
041300971023     C  N30              TESTN                   VHBLNA                   30
041400971021     C                   MOVE      VHBLNA        W001A             1
041500971021     C     *IN30         IFEQ      *OFF
041600971021     C     W001A         ORLT      '0'
041700971023     C     W001A         ANDNE     *BLANK
041800971021     C                   SETON                                        32
041900971021     C                   MOVE      *ZERO         VABLNA
042000010604     C                   eval      vinmsg = %trimr(vinmsg)
042100010604     C                             + ' ' + 'VHBLNA'
042200971021     C                   ELSE
042300971021     C                   MOVE      VHBLNA        VABLNA
042400971021     C                   ENDIF
042500971021     C*
042600971023     C                   TESTN                   VHBCTR               30
042700971023     C  N30              TESTN                   VHBCTR                 30
042800971023     C  N30              TESTN                   VHBCTR                   30
042900971021     C                   MOVE      VHBCTR        W001A             1
043000971021     C     *IN30         IFEQ      *OFF
043100971021     C     W001A         ORLT      '0'
043200971023     C     W001A         ANDNE     *BLANK
043300971021     C                   SETON                                        32
043400971021     C                   MOVE      *ZERO         VABCTR
043500010604     C                   eval      vinmsg = %trimr(vinmsg)
043600010604     C                             + ' ' + 'VHBCTR'
043700971021     C                   ELSE
043800971021     C                   MOVE      VHBCTR        VABCTR
043900971021     C                   ENDIF
044000941229     C*
044100971021     C                   MOVE      *ZERO         VABIAS
044200941229     C                   MOVE      VHBIAS        WASC15
044300971023     C                   TESTN                   WI15                 30
044400971023     C  N30              TESTN                   WI15                   30
044500971023     C  N30              TESTN                   WI15                     30
044600971021     C                   MOVE      WI15          W001A             1
044700971021     C     *IN30         IFEQ      *OFF
044800971021     C     W001A         ORLT      '0'
044900971023     C     W001A         ANDNE     *BLANK
045000971021     C                   SETON                                        32
045100971021     C                   MOVE      *ZERO         VABIAS
045200010604     C                   eval      vinmsg = %trimr(vinmsg)
045300010604     C                             + ' ' + 'VHBIAS'
045400971021     C                   ELSE
045500971021     C                   MOVEL     WI15          VABIAS
045600971023     C                   TESTN                   WD15                 30
045700971023     C  N30              TESTN                   WD15                   30
045800971023     C  N30              TESTN                   WD15                     30
045900971021     C                   MOVE      WD15          W001A             1
046000971021     C     *IN30         IFEQ      *OFF
046100971021     C     W001A         ORLT      '0'
046200971023     C     W001A         ANDNE     *BLANK
046300971021     C                   SETON                                        32
046400971021     C                   MOVE      *ZERO         VABIAS
046500010604     C                   eval      vinmsg = %trimr(vinmsg)
046600010604     C                             + ' ' + 'VHBIAS'
046700971021     C                   ELSE
046800971021     C                   MOVE      WD15          VABIAS
046900971021     C                   ENDIF
047000050118     C                   ENDIF
047100941229     C*
047200971023     C                   TESTN                   VHBNCL               30
047300971023     C  N30              TESTN                   VHBNCL                 30
047400971023     C  N30              TESTN                   VHBNCL                   30
047500971021     C                   MOVE      VHBNCL        W001A             1
047600971021     C     *IN30         IFEQ      *OFF
047700971021     C     W001A         ORLT      '0'
047800971021     C                   SETON                                        32
047900971021     C                   MOVE      *ZERO         VABNCL
048000010604     C                   eval      vinmsg = %trimr(vinmsg)
048100010604     C                             + ' ' + 'VHBNCL'
048200971021     C                   ELSE
048300971021     C                   MOVE      VHBNCL        VABNCL
048400971021     C                   ENDIF
048500941229     C*
048600971021     C                   MOVE      *ZERO         VABPKB
048700971021     C                   MOVE      VHBPKB        WASC09
048800050503     C                   IF        WI09 = *blanks
048900050503     C                   EVAL      WI09 = *zeros
049000050503     C                   ENDIF
049100971023     C                   TESTN                   WI09                 30
049200971023     C  N30              TESTN                   WI09                   30
049300971023     C  N30              TESTN                   WI09                     30
049400971021     C                   MOVE      WI09          W001A             1
049500971021     C     *IN30         IFEQ      *OFF
049600971021     C     W001A         ORLT      '0'
049700971021     C                   SETON                                        32
049800971021     C                   MOVE      *ZERO         VABPKB
049900010604     C                   eval      vinmsg = %trimr(vinmsg)
050000010604     C                             + ' ' + 'VHBPKB'
050100971021     C                   ELSE
050200971021     C                   MOVEL     WI09          VABPKB
050300971023     C                   TESTN                   WD09                 30
050400971023     C  N30              TESTN                   WD09                     30
050500971021     C                   MOVE      WD09          W001A             1
050600971021     C     *IN30         IFEQ      *OFF
050700971021     C     W001A         ORLT      '0'
050800971023     C     W001A         ANDNE     *BLANK
050900971021     C                   SETON                                        32
051000971021     C                   MOVE      *ZERO         VABPKB
051100010604     C                   eval      vinmsg = %trimr(vinmsg)
051200010604     C                             + ' ' + 'VHBPKB'
051300971021     C                   ELSE
051400971021     C                   MOVE      WD09          VABPKB
051500971021     C                   ENDIF
051600050118     C                   ENDIF
051700971021     C*
051800041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
051900041001     C                   IF        *IN51 = *OFF
052000971021     C                   MOVE      *ZERO         VABVLB
052100971021     C                   MOVE      VHBVLB        WASC07
052200050503     C                   IF        WI07 = *blanks
052300050503     C                   EVAL      WI07 = *zeros
052400050503     C                   ENDIF
052500971023     C                   TESTN                   WI07                 30
052600971023     C  N30              TESTN                   WI07                   30
052700971023     C  N30              TESTN                   WI07                     30
052800971021     C                   MOVE      WI07          W001A             1
052900971021     C     *IN30         IFEQ      *OFF
053000971021     C     W001A         ORLT      '0'
053100971023     C     W001A         ANDNE     *BLANK
053200971021     C                   SETON                                        32
053300971021     C                   MOVE      *ZERO         VABVLB
053400010604     C                   eval      vinmsg = %trimr(vinmsg)
053500010604     C                             + ' ' + 'VHBVLB'
053600971021     C                   ELSE
053700971021     C                   MOVEL     WI07          VABVLB
053800971023     C                   TESTN                   WD07                 30
053900971023     C  N30              TESTN                   WD07                   30
054000971023     C  N30              TESTN                   WD07                     30
054100971021     C                   MOVE      WD07          W001A             1
054200971021     C     *IN30         IFEQ      *OFF
054300971021     C     W001A         ORLT      '0'
054400971023     C     W001A         ANDNE     *BLANK
054500971021     C                   SETON                                        32
054600971021     C                   MOVE      *ZERO         VABVLB
054700010604     C                   eval      vinmsg = %trimr(vinmsg)
054800010604     C                             + ' ' + 'VHBVLB'
054900971021     C                   ELSE
055000971021     C                   MOVE      WD07          VABVLB
055100971021     C                   ENDIF
055200050118     C                   ENDIF
055300041001     C                   ELSE
055400041001     C* ...altrimenti forzo azzeramento del volume
055500041001     C                   Z-ADD     *zeros        VABVLB
055600041001     C                   ENDIF
055700941229     C*
055800971021     C                   MOVE      *ZERO         VABQFT
055900971021     C                   MOVE      VHBQFT        WASC15
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
057300971023     C                   TESTN                   WD15                 30
057400971023     C  N30              TESTN                   WD15                   30
057500971023     C  N30              TESTN                   WD15                     30
057600971021     C                   MOVE      WD15          W001A             1
057700971021     C     *IN30         IFEQ      *OFF
057800971021     C     W001A         ORLT      '0'
057900971023     C     W001A         ANDNE     *BLANK
058000971021     C                   SETON                                        32
058100971021     C                   MOVE      *ZERO         VABQFT
058200010604     C                   eval      vinmsg = %trimr(vinmsg)
058300010604     C                             + ' ' + 'VHBQFT'
058400971021     C                   ELSE
058500971021     C                   MOVE      WD15          VABQFT
058600971021     C                   ENDIF
058700050118     C                   ENDIF
058800941229     C*
058900971021     C                   MOVE      *ZERO         VABCAS
059000971021     C                   MOVE      VHBCAS        WASC15
059100971023     C                   TESTN                   WI15                 30
059200971023     C  N30              TESTN                   WI15                   30
059300971023     C  N30              TESTN                   WI15                     30
059400971021     C                   MOVE      WI15          W001A             1
059500971021     C     *IN30         IFEQ      *OFF
059600971021     C     W001A         ORLT      '0'
059700971023     C     W001A         ANDNE     *BLANK
059800971021     C                   SETON                                        32
059900971021     C                   MOVE      *ZERO         VABCAS
060000010604     C                   eval      vinmsg = %trimr(vinmsg)
060100010604     C                             + ' ' + 'VHBCAS'
060200971021     C                   ELSE
060300971021     C                   MOVEL     WI15          VABCAS
060400971023     C                   TESTN                   WD15                 30
060500971023     C  N30              TESTN                   WD15                   30
060600971023     C  N30              TESTN                   WD15                     30
060700971021     C                   MOVE      WD15          W001A             1
060800971021     C     *IN30         IFEQ      *OFF
060900971021     C     W001A         ORLT      '0'
061000971023     C     W001A         ANDNE     *BLANK
061100971021     C                   SETON                                        32
061200971021     C                   MOVE      *ZERO         VABCAS
061300010604     C                   eval      vinmsg = %trimr(vinmsg)
061400010604     C                             + ' ' + 'VHBCAS'
061500971021     C                   ELSE
061600971021     C                   MOVE      WD15          VABCAS
061700971021     C                   ENDIF
061800040827     C                   ENDIF
061900971021     C*
062000971023     C                   TESTN                   VHBRMN               30
062100971023     C  N30              TESTN                   VHBRMN                 30
062200971023     C  N30              TESTN                   VHBRMN                   30
062300971021     C                   MOVE      VHBRMN        W001A             1
062400971021     C     *IN30         IFEQ      *OFF
062500971021     C     W001A         ORLT      '0'
062600971021     C                   SETON                                        32
062700971021     C                   MOVE      *ZERO         VABRMN
062800010604     C                   eval      vinmsg = %trimr(vinmsg)
062900010604     C                             + ' ' + 'VHBRMN'
063000971021     C                   ELSE
063100971021     C                   MOVE      VHBRMN        VABRMN
063200971021     C                   ENDIF
063300971021     C*
063400971023     C                   TESTN                   VHBNCD               30
063500971023     C  N30              TESTN                   VHBNCD                 30
063600971023     C  N30              TESTN                   VHBNCD                   30
063700971021     C                   MOVE      VHBNCD        W001A             1
063800971021     C     *IN30         IFEQ      *OFF
063900971021     C     W001A         ORLT      '0'
064000971023     C     W001A         ANDNE     *BLANK
064100971021     C                   SETON                                        32
064200971021     C                   MOVE      *ZERO         VABNCD
064300010604     C                   eval      vinmsg = %trimr(vinmsg)
064400010604     C                             + ' ' + 'VHBNCD'
064500971021     C                   ELSE
064600971021     C                   MOVE      VHBNCD        VABNCD
064700971021     C                   ENDIF
064800971021     C*
064900971023     C                   TESTN                   VHBNCA               30
065000971023     C  N30              TESTN                   VHBNCA                 30
065100971023     C  N30              TESTN                   VHBNCA                   30
065200971021     C                   MOVE      VHBNCA        W001A             1
065300971021     C     *IN30         IFEQ      *OFF
065400971021     C     W001A         ORLT      '0'
065500971023     C     W001A         ANDNE     *BLANK
065600971021     C                   SETON                                        32
065700971021     C                   MOVE      *ZERO         VABNCA
065800010604     C                   eval      vinmsg = %trimr(vinmsg)
065900010604     C                             + ' ' + 'VHBNCA'
066000971021     C                   ELSE
066100971021     C                   MOVE      VHBNCA        VABNCA
066200971021     C                   ENDIF
066300971021     C*
066400971023     C                   TESTN                   VHBZNC               30
066500971023     C  N30              TESTN                   VHBZNC                 30
066600971023     C  N30              TESTN                   VHBZNC                   30
066700971021     C                   MOVE      VHBZNC        W001A             1
066800971021     C     *IN30         IFEQ      *OFF
066900971021     C     W001A         ORLT      '0'
067000971023     C     W001A         ANDNE     *BLANK
067100971021     C                   SETON                                        32
067200971021     C                   MOVE      *ZERO         VABZNC
067300010604     C                   eval      vinmsg = %trimr(vinmsg)
067400010604     C                             + ' ' + 'VHBZNC'
067500971021     C                   ELSE
067600971021     C                   MOVE      VHBZNC        VABZNC
067700971021     C                   ENDIF
067800971021     C*
067900971023     C                   TESTN                   VHBDCR               30
068000971023     C  N30              TESTN                   VHBDCR                 30
068100971023     C  N30              TESTN                   VHBDCR                   30
068200971021     C                   MOVE      VHBDCR        W001A             1
068300971021     C     *IN30         IFEQ      *OFF
068400971021     C     W001A         ORLT      '0'
068500971023     C     W001A         ANDNE     *BLANK
068600971021     C                   SETON                                        32
068700971021     C                   MOVE      *ZERO         VABDCR
068800010604     C                   eval      vinmsg = %trimr(vinmsg)
068900010604     C                             + ' ' + 'VHBDCR'
069000971021     C                   ELSE
069100971021     C                   MOVE      VHBDCR        VABDCR
069200971021     C                   ENDIF
069300971021     C*
069400971023     C                   TESTN                   VHBHCR               30
069500971023     C  N30              TESTN                   VHBHCR                 30
069600971023     C  N30              TESTN                   VHBHCR                   30
069700971021     C                   MOVE      VHBHCR        W001A             1
069800971021     C     *IN30         IFEQ      *OFF
069900971021     C     W001A         ORLT      '0'
070000971023     C     W001A         ANDNE     *BLANK
070100971021     C                   SETON                                        32
070200971021     C                   MOVE      *ZERO         VABHCR
070300010604     C                   eval      vinmsg = %trimr(vinmsg)
070400010604     C                             + ' ' + 'VHBHCR'
070500971021     C                   ELSE
070600971021     C                   MOVE      VHBHCR        VABHCR
070700971021     C                   ENDIF
070800941229     C*
070900971021     C                   MOVE      *ZERO         VABVMD
071000971021     C                   MOVE      VHBVMD        WASC15
071100971023     C                   TESTN                   WI15                 30
071200971023     C  N30              TESTN                   WI15                   30
071300971023     C  N30              TESTN                   WI15                     30
071400971021     C                   MOVE      WI15          W001A             1
071500971021     C     *IN30         IFEQ      *OFF
071600971021     C     W001A         ORLT      '0'
071700971023     C     W001A         ANDNE     *BLANK
071800971021     C                   SETON                                        32
071900971021     C                   MOVE      *ZERO         VABVMD
072000010604     C                   eval      vinmsg = %trimr(vinmsg)
072100010604     C                             + ' ' + 'VHBVMD'
072200971021     C                   ELSE
072300971021     C                   MOVEL     WI15          VABVMD
072400971023     C                   TESTN                   WD15                 30
072500971023     C  N30              TESTN                   WD15                   30
072600971023     C  N30              TESTN                   WD15                     30
072700971021     C                   MOVE      WD15          W001A             1
072800971021     C     *IN30         IFEQ      *OFF
072900971021     C     W001A         ORLT      '0'
073000971023     C     W001A         ANDNE     *BLANK
073100971021     C                   SETON                                        32
073200971021     C                   MOVE      *ZERO         VABVMD
073300010604     C                   eval      vinmsg = %trimr(vinmsg)
073400010604     C                             + ' ' + 'VHBVMD'
073500971021     C                   ELSE
073600971021     C                   MOVE      WD15          VABVMD
073700971021     C                   ENDIF
073800050118     C                   ENDIF
073900971021     C*
074000971023     C                   TESTN                   VHBANT               30
074100971023     C  N30              TESTN                   VHBANT                 30
074200971023     C  N30              TESTN                   VHBANT                   30
074300971021     C                   MOVE      VHBANT        W001A             1
074400971021     C     *IN30         IFEQ      *OFF
074500971021     C     W001A         ORLT      '0'
074600971023     C     W001A         ANDNE     *BLANK
074700971021     C                   SETON                                        32
074800971021     C                   MOVE      *ZERO         VABANT
074900010604     C                   eval      vinmsg = %trimr(vinmsg)
075000010604     C                             + ' ' + 'VHBANT'
075100971021     C                   ELSE
075200971021     C                   MOVE      VHBANT        VABANT
075300971021     C                   ENDIF
075400971021     C*
075500971021     C* CAMPI ALFABETICI:
075600971021     C                   MOVE      VHBATB        VABATB
075700971021     C                   MOVE      VHBCBO        VABCBO
075800971021     C                   MOVE      VHBRSD        VABRSD
075900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
076000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
076100020117     C* ==
076200971021     C                   MOVE      VHBRD2        VABRD2
076300971021     C                   MOVE      VHBIND        VABIND
076400971021     C                   MOVE      VHBCAD        VABCAD
076500971021     C                   MOVE      VHBLOD        VABLOD
076600971021     C                   MOVE      VHBPRD        VABPRD
076700971021     C                   MOVE      VHBNZD        VABNZD
076800971021     C                   MOVE      VHBGC1        VABGC1
076900971021     C                   MOVE      VHBGC2        VABGC2
077000971021     C                   MOVE      VHBTSP        VABTSP
077100971021     C                   MOVE      VHBVAS        VABVAS
077200971021     C                   MOVE      VHBNAS        VABNAS
077300971021     C                   MOVE      VHBTIC        VABTIC
077400971021     C                   MOVE      VHBVCA        VABVCA
077500971021     C                   MOVE      VHBGCA        VABGCA
077600971021     C                   MOVE      VHBRMA        VABRMA
077700971021     C                   MOVE      VHBXCO        VABXCO
077800971021     C                   MOVE      VHBNOT        VABNOT
077900971021     C                   MOVE      VHBNT2        VABNT2
078000971021     C                   MOVE      VHBCTM        VABCTM
078100971021     C                   MOVE      VHBFFD        VABFFD
078200971021     C                   MOVE      VHBTCR        VABTCR
078300971021     C                   MOVE      VHBCTS        VABCTS
078400971021     C                   MOVE      VHBFTM        VABFTM
078500971021     C                   MOVE      VHBVAD        VABVAD
078600971021     C                   MOVE      VHBGMA        VABGMA
078700971021     C                   MOVE      VHBGGA        VABGGA
078800971021     C                   MOVE      VHBGVA        VABGVA
078900971021     C                   MOVE      VHBTC1        VABTC1
079000971021     C                   MOVE      VHBTC2        VABTC2
079100971021     C                   MOVE      VHBSCL        VABSCL
079200971021     C                   MOVE      VHBRMO        VABRMO
079300971021     C                   MOVE      VHBCMO        VABCMO
079400971021     C                   MOVE      VHBNMO        VABNMO
079500971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
079600971021     C* NELLA NAZIONE ORIGINALE
079700971021     C                   MOVE      VHBNMO        WA1D              1
079800971021     C                   MOVEL     VHBNMO        WA1S              1
079900971021     C     WA1D          IFNE      *BLANK
080000971021     C     WA1S          ANDEQ     *BLANK
080100971021     C                   MOVE      *BLANK        VABNMO
080200971021     C                   END
080300050418     C*
080400050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
080500050418     C                   IF        %trim(VABNZD) = 'I'   OR
080600050418     C                             %trim(VABNZD) = 'IT'  OR
080700050418     C                             %trim(VABNZD) = 'ITA'
080800050418     C                   EVAL      VABNZD = *blanks
080900050418     C                   ENDIF
081000041025     C*
081100041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
081200041025     C                   IF        *IN52 = *ON
081300041025     C* Reperisco la provincia dal CAP e dalla località
081400041025     C                   IF        VABCAD <> *blanks AND
081500041025     C                             VABPRD  = *blanks
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
082600050905     C*
082700050905     C* EFfettuo considerazioni x forzatura codici clienti mittenti pilotati dal cliente
082800080110     C                   IF        VABSCL = '1' AND
082900050905     C                             VABCCM = 0431004
083000050905     C                   EVAL      VABCCM = 0433068
083100050905     C                   ENDIF
083200050905     C                   IF        VABSCL = '2' AND
083300050905     C                             VABCCM = 0430093
083400050905     C                   EVAL      VABCCM = 0433069
083500050905     C                   ENDIF
083600080611     C*
083700080611     C* Effettuo considerazioni x evidenziazione telefono destinatario
083800080611     C                   MOVEL     *BLANKS       wNOTE            70
083900080611     C                   EVAL      wNOTE  = %trim(VABNOT) + ' ' +
084000080611     C                                      %trim(VABNT2)
084100080611     C                   EVAL      wNOTE  = %trim(wNOTE)  + ' ' +
084200080611     C                                      'TEL.' + %trim(VHBTELX)
084300080611     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
084400080611     C                   EVAL      VABNT2 = %subst(wNOTE:36)
084500040630     C*
084600040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
084700040630     C                   exsr      exeBon
084800090108     C*
084900090108     C* VALORIZZO CAMPI RELATIVI AL "CMR"
085000090108     C                   eval      VABCMR = %char(datcor) + %char(oracor)
085100090108     C                   eval      VABDCM = DATCOR
085200090108     C                   eval      VABDTS = DATCOR
085300090108     C                   eval      VABHMS = ORACOR
085400090108     C                   eval      VABCNT = 1
085500930409     C*
085600010604     C  N31              ADD       1             §CTROK            7 0
085700010604     C   32              ADD       1             §CTRMO            7 0
085800010604     C   31              ADD       1             §CTRNO            7 0
085900090108     C  N31              WRITE     EDIVAB00
086000990910     C*
086100010601     C                   if        *in31 = *off and
086200010601     C                             *in32 = *off
086300990910     C                   eval      vinflg = '1'
086400990910     C                   else
086500000621     C                   eval      recko = vindta
086600010612     C                   if        %open(tis7t4ps)
086700030407     C   50              except    rigadett
086800010612     C                   endif
086900990910     C                   eval      vinflg = '2'
087000990910     C                   endif
087100000905     C                   endif
087200000905     C                   endif
087300010601     C*
087400010601     C                   endif
087500000905     C*
087600000905     C                   else
087700000905     C                   eval      vinflg = '1'
087800000905     C                   endif
087900000905     C*
088000000905     C  N70              update    tivin000
088100000905     C*
088200991022     C  N70              ENDdo
088300010601     C
088400010601     C*
088500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
088600010601     C                   if        cntNonEl = *zeros or
088700010601     C                             flgMulti = '0'
088800990910     C* Se non ci sono record con errori ...
088900010601     C                   if        §ctrno = 0 and
089000010601     C                             §ctrmo = 0 and
089100010601     C                             flgStato <> '2'
089200990910     C* ... restituisco esito OK.
089300990921     C                   eval      wrkesito = '0'
089400990910     C                   else
089500010601     C                   if        §ctrok > 0
089600000710     C                   eval      wrkesito = '1'
089700000710     C                   else
089800010615     C                   if        flgOk = '0'
089900010615     C                   eval      wrkesito = '2'
090000010615     C                   else
090100010615     C                   eval      wrkesito = '6'
090200010615     C                   endif
090300000710     C                   endif
090400010615     C                   endif
090500010601     C                   else
090600010601     C                   eval      wrkesito = '9'
090700010601     C                   endif
090800990910     C*
090900990914     C                   if        %open(tivin00r)
091000990908     C                   close     tivin00r
091100990914     C                   endif
091200090108     C                   if        %open(edivabwr)
091300090108     C                   close     edivabwr
091400990914     C                   endif
091500010601     C*
091600010601     C                   if        vlrpoi <> 999
091700010601     C                   eval      invfil = vlrpoi
091800010601     C                   endif
091900990910     C*
092000990920     C                   if        §ctrok > 0
092100010601     C                             and invfil > *zeros
092200990920     C                   exsr      invio
092300990920     C                   endif
092400010612     C*
092500010612     C                   if        flgGiro = '1'
092600010612     C                   exsr      endela
092700010612     C                   endif
092800990920     C*
092900910830     C                   ENDSR
093000000613     C***
093100990920
093200010601
093300010601
093400010601
093500010601
093600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
093700010601     C     repfil        BEGSR
093800010601     C*
093900010601     C                   if        invfil = *zeros and
094000010601     C                             depfil > *zeros and
094100010613     C                             (vinflg = *blanks or
094200010613     C                              vinflg = *zeros)
094300010601     C
094400010601     C                   eval      invfil = depfil
094500010601     C                   endif
094600010601     C*
094700010601     C                   if        depfil <> invfil and
094800010601     C                             invfil > *zeros
094900010601     C                   eval      flgMulti = '1'
095000010601     C                   if        vinflg = *blanks
095100010601     C                   add       1             cntNonEl
095200010601     C                   endif
095300010601     C                   endif
095400010601     C*
095500010601     C                   if        vinflg = '2'
095600010601     C                   eval      flgStato = '2'
095700010601     C                   endif
095800010601     C*
095900010601     C                   ENDSR
096000010601     C***
096100010601
096200010601
096300010601
096400010601
096500010601
096600010601
096700010601
096800990920      /TITLE Invio dei dati al punto operativo.
096900000613     C     invio         BEGSR
097000990920     C*
097100990920     C                   reset                   dscmz
097200010601     C                   move      invfil        cmzdst
097300990920     C                   eval      cmzfld = vlrfou
097400990920     C                   eval      cmzmbd = vlrhdl
097500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
097600000710     C                   if        prmfir = *blanks
097700090108     C                   eval      cmzfla = 'EDIVAB0F'
097800090108     C                   eval      cmzmba = 'EDIVAB0F'
097900000710     C                   else
098000000710     C                   eval      cmzfla = prmfir
098100000710     C                   eval      cmzmba = prmfir
098200000710     C                   endif
098300990920     C                   eval      cmznrr = *zeros
098400990920     C                   move      §ctrok        cmznrr
098500021003     C                   eval      cmzlba = vlrfl1
098600990920     C                   call(e)   'TIS711C'
098700990920     C                   parm                    dscmz
098800990921     C                   parm      *blanks       esito
098900990920     C                   if        %error
099000990920     C                             or cmzerr = '1'
099100990921     C                             or esito  = '1'
099200000710     C                   eval      wrkesito = '3'
099300990920     C                   endif
099400990920     C*
099500000613     C                   ENDSR
099600010601
099700010601
099800010601
099900010601
100000010601      /TITLE Invio dei dati al punto operativo.
100100010601     C     opeini        BEGSR
100200010601     C*
100300010601     C* Inizializzo flag e contatori operativi
100400010601     C                   movel     '0'           flgGiro           1
100500010601     C                   movel     '0'           flgMulti          1
100600010601     C                   movel     '1'           flgStato          1
100700010615     C                   movel     '0'           flgOk             1
100800010601     C                   z-add     *zeros        cntNonEl         10 0
100900010601     C                   z-add     *zeros        depfil            3 0
101000010601     C                   z-add     *zeros        invfil            3 0
101100030407     C*
101200030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
101300030407     C                   if        %subst(vlrppt:1:1) = 'S'
101400030407     C                   seton                                        50
101500030407     C                   else
101600030407     C                   setoff                                       50
101700030407     C                   endif
101800041001     C*
101900041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
102000041001     C                   if        %subst(vlrppt:2:1) = 'S'
102100041001     C                   seton                                        51
102200041001     C                   else
102300041001     C                   setoff                                       51
102400041001     C                   endif
102500041025     C*
102600041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
102700041025     C* (naturalmente solo se nn già presente)
102800041025     C                   if        %subst(vlrppt:3:1) = 'S'
102900041025     C                   seton                                        52
103000041025     C                   else
103100041025     C                   setoff                                       52
103200041025     C                   endif
103300010601     C*
103400010601     C                   ENDSR
103500010601     C***
103600040630
103700040630
103800040630
103900040630
104000040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
104100040630     C     exeBon        BEGSR
104200040630     C*
104300040630     C* Imposto i parametri x il *pgm d "bonifica"
104400040630     C                   EVAL      ElencoChar = *blanks
104500040630     C                   EVAL      TipoElenco = *blanks
104600040630     C                   EVAL      CharSost   = *blanks
104700040630     C                   EVAL      UpperCase  = '1'
104800040630     C                   EVAL      ChkNull    = '1'
104900040630     C                   EVAL      CharNull   = *blanks
105000040630     C*
105100040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
105200040630     C                   EVAL      TxtInOut   = VABRSD
105300040630     C                   EVAL      EsitoBon   = *blanks
105400040630     C*
105500040630     C                   CALL      'XCHKCHAR'
105600040630     C                   PARM                    TxtInOut
105700040630     C                   PARM                    ElencoChar
105800040630     C                   PARM                    TipoElenco
105900040630     C                   PARM                    CharSost
106000040630     C                   PARM                    UpperCase
106100040630     C                   PARM                    ChkNull
106200040630     C                   PARM                    CharNull
106300040630     C                   PARM                    EsitoBon
106400040630     C*
106500040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
106600040630     C                   IF        EsitoBon = '1'
106700040630     C                   EVAL      VABRSD = TxtInOut
106800040630     C                   ENDIF
106900040630     C*
107000040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
107100040630     C                   EVAL      TxtInOut   = VABIND
107200040630     C                   EVAL      EsitoBon   = *blanks
107300040630     C*
107400040630     C                   CALL      'XCHKCHAR'
107500040630     C                   PARM                    TxtInOut
107600040630     C                   PARM                    ElencoChar
107700040630     C                   PARM                    TipoElenco
107800040630     C                   PARM                    CharSost
107900040630     C                   PARM                    UpperCase
108000040630     C                   PARM                    ChkNull
108100040630     C                   PARM                    CharNull
108200040630     C                   PARM                    EsitoBon
108300040630     C*
108400040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
108500040630     C                   IF        EsitoBon = '1'
108600040630     C                   EVAL      VABIND = TxtInOut
108700040630     C                   ENDIF
108800040630     C*
108900040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
109000040630     C                   EVAL      TxtInOut   = VABLOD
109100040630     C                   EVAL      EsitoBon   = *blanks
109200040630     C*
109300040630     C                   CALL      'XCHKCHAR'
109400040630     C                   PARM                    TxtInOut
109500040630     C                   PARM                    ElencoChar
109600040630     C                   PARM                    TipoElenco
109700040630     C                   PARM                    CharSost
109800040630     C                   PARM                    UpperCase
109900040630     C                   PARM                    ChkNull
110000040630     C                   PARM                    CharNull
110100040630     C                   PARM                    EsitoBon
110200040630     C*
110300040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
110400040630     C                   IF        EsitoBon = '1'
110500040630     C                   EVAL      VABLOD = TxtInOut
110600040630     C                   ENDIF
110700040630     C*
110800040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
110900040630     C                   EVAL      TxtInOut   = VABNOT
111000040630     C                   EVAL      EsitoBon   = *blanks
111100040630     C                   EVAL      UpperCase  = *blanks
111200040630     C*
111300040630     C                   CALL      'XCHKCHAR'
111400040630     C                   PARM                    TxtInOut
111500040630     C                   PARM                    ElencoChar
111600040630     C                   PARM                    TipoElenco
111700040630     C                   PARM                    CharSost
111800040630     C                   PARM                    UpperCase
111900040630     C                   PARM                    ChkNull
112000040630     C                   PARM                    CharNull
112100040630     C                   PARM                    EsitoBon
112200040630     C*
112300040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
112400040630     C                   IF        EsitoBon = '1'
112500040630     C                   EVAL      VABNOT = TxtInOut
112600040630     C                   ENDIF
112700040630     C*
112800040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
112900040630     C                   EVAL      TxtInOut   = VABNT2
113000040630     C                   EVAL      EsitoBon   = *blanks
113100040630     C                   EVAL      UpperCase  = *blanks
113200040630     C*
113300040630     C                   CALL      'XCHKCHAR'
113400040630     C                   PARM                    TxtInOut
113500040630     C                   PARM                    ElencoChar
113600040630     C                   PARM                    TipoElenco
113700040630     C                   PARM                    CharSost
113800040630     C                   PARM                    UpperCase
113900040630     C                   PARM                    ChkNull
114000040630     C                   PARM                    CharNull
114100040630     C                   PARM                    EsitoBon
114200040630     C*
114300040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
114400040630     C                   IF        EsitoBon = '1'
114500040630     C                   EVAL      VABNT2 = TxtInOut
114600040630     C                   ENDIF
114700040630     C*
114800040630     C                   ENDSR
114900040630
115000010601
115100010601
115200990910
115300000613     C     *inzsr        BEGSR
115400990910     C*
115500990910     C     *entry        plist
115600990920     C                   parm                    tivlrds
115700990921     C                   parm      wrkesito      esito
115800000724     C                   parm                    prmlit
115900000710     C                   parm                    prmfir
116000010606     C*
116100010606     C* CALCOLA LA DATA CORRENTE
116200010606     C                   time                    wn14             14 0
116300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
116400090108     C                   movel     wn14          oracor            6 0          *ORA
116500010606     C                   z-add     wn8           g08dat
116600010606     C                   z-add     *zeros        g08inv
116700010606     C                   movel     '0'           g08err
116800010606     C                   call      'XSRDA8'
116900010606     C                   parm                    wlbda8
117000010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
117100000613     C*
117200000613     C                   ENDSR
117300000613     C***
117400990908
117500000613     Otis7t4p   E            riepilogo         2
117600990915     O                                              'Upload via Internet'
117700990915     O                                           +1 'Traduzione TIVIN00R -'
117800090108     O                                           +1 'EDIVABWR'
117900990915     O                                           +1 'Testate bolle'
118000990915     O          E            riepilogo   2
118100990915     O                       wrkdata
118200990915     O                       wrkora              +1
118300990915     O                       procname            +1
118400990915     O          E            riepilogo   2
118500990915     O                                              'Cliente..................:'
118600990915     O                       VABCCM        z     +1
118700990915     O          E            riepilogo   2
118800990920     O                                              'Riferimento Strategi.....:'
118900990920     O                       vlrhdl              +1
119000990915     O          E            riepilogo   2
119100990915     O                                              'Giusti...................:'
119200971022     O                       §CTROK        2   +  1
119300990915     O          E            riepilogo   2
119400990915     O                                              'Sbagliati e corretti.....:'
119500971022     O                       §CTRMO        2   +  1
119600990915     O          E            riepilogo   2
119700990915     O                                              'Sbagliati e scartati.....:'
119800971022     O                       §CTRNO        2   +  1
119900000613
120000000616     Otis7t4ps  E            testdett          2
120100000613     O                                              'Upload via Internet'
120200000613     O                                           +1 'Traduzione TIVIN00R -'
120300090108     O                                           +1 'EDIVABWR'
120400000613     O                                           +1 'Testate bolle'
120500000616     O          E            testdett    3
120600000613     O                                           +2 'N° rec'
120700000613     O                                           +3 'Anteprima contenuto'
120800000616     O          E            rigadett    2
120900000613     O                       rrnum               +2
121000000621     O                       recko               +3
121100000616     O          E            findett     2
121200000613     O                       wrkdata
121300000613     O                       wrkora              +1
121400000613     O                       procname            +1
121500000616     O          E            findett     2
121600000613     O                                              'Cliente..................:'
121700000613     O                       VABCCM        z     +1
121800000616     O          E            findett     2
121900000613     O                                              'Riferimento Strategi.....:'
122000000613     O                       vlrhdl              +1
122100000616     O          E            findett     2
122200000613     O                                              'Giusti...................:'
122300000613     O                       §CTROK        2   +  1
122400000616     O          E            findett     2
122500000613     O                                              'Sbagliati e corretti.....:'
122600000613     O                       §CTRMO        2   +  1
122700000616     O          E            findett     2
122800000613     O                                              'Sbagliati e scartati.....:'
122900000613     O                       §CTRNO        2   +  1
123000000613** CMD - COMANDI CL
123100011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4P)  FORMTYPE(RICCLI) OUTQ(
123200011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4PS) FORMTYPE(RICCLI) OUTQ(
123300000616DLTOVR FILE(TIS7T4P TIS7T4PS) LVL(*)
123400000613
123500000613
