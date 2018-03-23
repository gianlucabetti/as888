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
004901070420     D vhbldv                 18     28
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
038000070420     C*                  TESTN                   VHBNRS               30
038100070420     C* N30              TESTN                   VHBNRS                 30
038200070420     C* N30              TESTN                   VHBNRS                   30
038300070420     C*                  MOVE      VHBNRS        W001A             1
038400070420     C*    *IN30         IFEQ      *OFF
038500070420     C*    W001A         ORLT      '0'
038600070420     C*    W001A         ANDNE     *BLANK
038700070420     C*                  SETON                                        32
038800070420     C*                  MOVE      *ZERO         VABNRS
038900070420     C*                  eval      vinmsg = %trimr(vinmsg)
039000070420     C*                            + ' ' + 'VHBNRS'
039100070420     C*                  ELSE
039200070420     C*                  MOVE      VHBNRS        VABNRS
039300070420     C*                  ENDIF
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
048500051130     C                   EVAL      PiStr=VHBPKB
048600051130     C                   EXSR      CHKNUM
048700051130     C                   IF        PiNum=*on
048800051130     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 decim.
048900051130     C                   Z-ADD(H)  PiVal         VABPKB
049000051130     C                   ELSE
049100971021     C                   SETON                                        32
049200051130     C                   Z-ADD     *ZERO         VABPKB
049300010604     C                   eval      vinmsg = %trimr(vinmsg)
049400010604     C                             + ' ' + 'VHBPKB'
049500050118     C                   ENDIF
049600971021     C*
049700041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
049800041001     C                   IF        *IN51 = *OFF
049900971021     C                   MOVE      *ZERO         VABVLB
050000971021     C                   MOVE      VHBVLB        WASC07
050100050503     C                   IF        WI07 = *blanks
050200050503     C                   EVAL      WI07 = *zeros
050300050503     C                   ENDIF
050400971023     C                   TESTN                   WI07                 30
050500971023     C  N30              TESTN                   WI07                   30
050600971023     C  N30              TESTN                   WI07                     30
050700971021     C                   MOVE      WI07          W001A             1
050800971021     C     *IN30         IFEQ      *OFF
050900971021     C     W001A         ORLT      '0'
051000971023     C     W001A         ANDNE     *BLANK
051100971021     C                   SETON                                        32
051200971021     C                   MOVE      *ZERO         VABVLB
051300010604     C                   eval      vinmsg = %trimr(vinmsg)
051400010604     C                             + ' ' + 'VHBVLB'
051500971021     C                   ELSE
051600971021     C                   MOVEL     WI07          VABVLB
051700971023     C                   TESTN                   WD07                 30
051800971023     C  N30              TESTN                   WD07                   30
051900971023     C  N30              TESTN                   WD07                     30
052000971021     C                   MOVE      WD07          W001A             1
052100971021     C     *IN30         IFEQ      *OFF
052200971021     C     W001A         ORLT      '0'
052300971023     C     W001A         ANDNE     *BLANK
052400971021     C                   SETON                                        32
052500971021     C                   MOVE      *ZERO         VABVLB
052600010604     C                   eval      vinmsg = %trimr(vinmsg)
052700010604     C                             + ' ' + 'VHBVLB'
052800971021     C                   ELSE
052900971021     C                   MOVE      WD07          VABVLB
053000971021     C                   ENDIF
053100050118     C                   ENDIF
053200041001     C                   ELSE
053300041001     C* ...altrimenti forzo azzeramento del volume
053400041001     C                   Z-ADD     *zeros        VABVLB
053500041001     C                   ENDIF
053600941229     C*
053700971021     C                   MOVE      *ZERO         VABQFT
053800971021     C                   MOVE      VHBQFT        WASC15
053900971023     C                   TESTN                   WI15                 30
054000971023     C  N30              TESTN                   WI15                   30
054100971023     C  N30              TESTN                   WI15                     30
054200971021     C                   MOVE      WI15          W001A             1
054300971021     C     *IN30         IFEQ      *OFF
054400971021     C     W001A         ORLT      '0'
054500971023     C     W001A         ANDNE     *BLANK
054600971021     C                   SETON                                        32
054700971021     C                   MOVE      *ZERO         VABQFT
054800010604     C                   eval      vinmsg = %trimr(vinmsg)
054900010604     C                             + ' ' + 'VHBQFT'
055000971021     C                   ELSE
055100971021     C                   MOVEL     WI15          VABQFT
055200971023     C                   TESTN                   WD15                 30
055300971023     C  N30              TESTN                   WD15                   30
055400971023     C  N30              TESTN                   WD15                     30
055500971021     C                   MOVE      WD15          W001A             1
055600971021     C     *IN30         IFEQ      *OFF
055700971021     C     W001A         ORLT      '0'
055800971023     C     W001A         ANDNE     *BLANK
055900971021     C                   SETON                                        32
056000971021     C                   MOVE      *ZERO         VABQFT
056100010604     C                   eval      vinmsg = %trimr(vinmsg)
056200010604     C                             + ' ' + 'VHBQFT'
056300971021     C                   ELSE
056400971021     C                   MOVE      WD15          VABQFT
056500971021     C                   ENDIF
056600050118     C                   ENDIF
056700941229     C*
056800971021     C                   MOVE      *ZERO         VABCAS
056900971021     C                   MOVE      VHBCAS        WASC15
057000971023     C                   TESTN                   WI15                 30
057100971023     C  N30              TESTN                   WI15                   30
057200971023     C  N30              TESTN                   WI15                     30
057300971021     C                   MOVE      WI15          W001A             1
057400971021     C     *IN30         IFEQ      *OFF
057500971021     C     W001A         ORLT      '0'
057600971023     C     W001A         ANDNE     *BLANK
057700971021     C                   SETON                                        32
057800971021     C                   MOVE      *ZERO         VABCAS
057900010604     C                   eval      vinmsg = %trimr(vinmsg)
058000010604     C                             + ' ' + 'VHBCAS'
058100971021     C                   ELSE
058200971021     C                   MOVEL     WI15          VABCAS
058300971023     C                   TESTN                   WD15                 30
058400971023     C  N30              TESTN                   WD15                   30
058500971023     C  N30              TESTN                   WD15                     30
058600971021     C                   MOVE      WD15          W001A             1
058700971021     C     *IN30         IFEQ      *OFF
058800971021     C     W001A         ORLT      '0'
058900971023     C     W001A         ANDNE     *BLANK
059000971021     C                   SETON                                        32
059100971021     C                   MOVE      *ZERO         VABCAS
059200010604     C                   eval      vinmsg = %trimr(vinmsg)
059300010604     C                             + ' ' + 'VHBCAS'
059400971021     C                   ELSE
059500971021     C                   MOVE      WD15          VABCAS
059600971021     C                   ENDIF
059700040827     C                   ENDIF
059800971021     C*
059900971023     C                   TESTN                   VHBRMN               30
060000971023     C  N30              TESTN                   VHBRMN                 30
060100971023     C  N30              TESTN                   VHBRMN                   30
060200971021     C                   MOVE      VHBRMN        W001A             1
060300971021     C     *IN30         IFEQ      *OFF
060400971021     C     W001A         ORLT      '0'
060500971021     C                   SETON                                        32
060600971021     C                   MOVE      *ZERO         VABRMN
060700010604     C                   eval      vinmsg = %trimr(vinmsg)
060800010604     C                             + ' ' + 'VHBRMN'
060900971021     C                   ELSE
061000971021     C                   MOVE      VHBRMN        VABRMN
061100971021     C                   ENDIF
061200971021     C*
061300971023     C                   TESTN                   VHBNCD               30
061400971023     C  N30              TESTN                   VHBNCD                 30
061500971023     C  N30              TESTN                   VHBNCD                   30
061600971021     C                   MOVE      VHBNCD        W001A             1
061700971021     C     *IN30         IFEQ      *OFF
061800971021     C     W001A         ORLT      '0'
061900971023     C     W001A         ANDNE     *BLANK
062000971021     C                   SETON                                        32
062100971021     C                   MOVE      *ZERO         VABNCD
062200010604     C                   eval      vinmsg = %trimr(vinmsg)
062300010604     C                             + ' ' + 'VHBNCD'
062400971021     C                   ELSE
062500971021     C                   MOVE      VHBNCD        VABNCD
062600971021     C                   ENDIF
062700971021     C*
062800971023     C                   TESTN                   VHBNCA               30
062900971023     C  N30              TESTN                   VHBNCA                 30
063000971023     C  N30              TESTN                   VHBNCA                   30
063100971021     C                   MOVE      VHBNCA        W001A             1
063200971021     C     *IN30         IFEQ      *OFF
063300971021     C     W001A         ORLT      '0'
063400971023     C     W001A         ANDNE     *BLANK
063500971021     C                   SETON                                        32
063600971021     C                   MOVE      *ZERO         VABNCA
063700010604     C                   eval      vinmsg = %trimr(vinmsg)
063800010604     C                             + ' ' + 'VHBNCA'
063900971021     C                   ELSE
064000971021     C                   MOVE      VHBNCA        VABNCA
064100971021     C                   ENDIF
064200971021     C*
064300971023     C                   TESTN                   VHBZNC               30
064400971023     C  N30              TESTN                   VHBZNC                 30
064500971023     C  N30              TESTN                   VHBZNC                   30
064600971021     C                   MOVE      VHBZNC        W001A             1
064700971021     C     *IN30         IFEQ      *OFF
064800971021     C     W001A         ORLT      '0'
064900971023     C     W001A         ANDNE     *BLANK
065000971021     C                   SETON                                        32
065100971021     C                   MOVE      *ZERO         VABZNC
065200010604     C                   eval      vinmsg = %trimr(vinmsg)
065300010604     C                             + ' ' + 'VHBZNC'
065400971021     C                   ELSE
065500971021     C                   MOVE      VHBZNC        VABZNC
065600971021     C                   ENDIF
065700971021     C*
065800971023     C                   TESTN                   VHBDCR               30
065900971023     C  N30              TESTN                   VHBDCR                 30
066000971023     C  N30              TESTN                   VHBDCR                   30
066100971021     C                   MOVE      VHBDCR        W001A             1
066200971021     C     *IN30         IFEQ      *OFF
066300971021     C     W001A         ORLT      '0'
066400971023     C     W001A         ANDNE     *BLANK
066500971021     C                   SETON                                        32
066600971021     C                   MOVE      *ZERO         VABDCR
066700010604     C                   eval      vinmsg = %trimr(vinmsg)
066800010604     C                             + ' ' + 'VHBDCR'
066900971021     C                   ELSE
067000971021     C                   MOVE      VHBDCR        VABDCR
067100971021     C                   ENDIF
067200971021     C*
067300971023     C                   TESTN                   VHBHCR               30
067400971023     C  N30              TESTN                   VHBHCR                 30
067500971023     C  N30              TESTN                   VHBHCR                   30
067600971021     C                   MOVE      VHBHCR        W001A             1
067700971021     C     *IN30         IFEQ      *OFF
067800971021     C     W001A         ORLT      '0'
067900971023     C     W001A         ANDNE     *BLANK
068000971021     C                   SETON                                        32
068100971021     C                   MOVE      *ZERO         VABHCR
068200010604     C                   eval      vinmsg = %trimr(vinmsg)
068300010604     C                             + ' ' + 'VHBHCR'
068400971021     C                   ELSE
068500971021     C                   MOVE      VHBHCR        VABHCR
068600971021     C                   ENDIF
068700941229     C*
068800971021     C                   MOVE      *ZERO         VABVMD
068900971021     C                   MOVE      VHBVMD        WASC15
069000971023     C                   TESTN                   WI15                 30
069100971023     C  N30              TESTN                   WI15                   30
069200971023     C  N30              TESTN                   WI15                     30
069300971021     C                   MOVE      WI15          W001A             1
069400971021     C     *IN30         IFEQ      *OFF
069500971021     C     W001A         ORLT      '0'
069600971023     C     W001A         ANDNE     *BLANK
069700971021     C                   SETON                                        32
069800971021     C                   MOVE      *ZERO         VABVMD
069900010604     C                   eval      vinmsg = %trimr(vinmsg)
070000010604     C                             + ' ' + 'VHBVMD'
070100971021     C                   ELSE
070200971021     C                   MOVEL     WI15          VABVMD
070300971023     C                   TESTN                   WD15                 30
070400971023     C  N30              TESTN                   WD15                   30
070500971023     C  N30              TESTN                   WD15                     30
070600971021     C                   MOVE      WD15          W001A             1
070700971021     C     *IN30         IFEQ      *OFF
070800971021     C     W001A         ORLT      '0'
070900971023     C     W001A         ANDNE     *BLANK
071000971021     C                   SETON                                        32
071100971021     C                   MOVE      *ZERO         VABVMD
071200010604     C                   eval      vinmsg = %trimr(vinmsg)
071300010604     C                             + ' ' + 'VHBVMD'
071400971021     C                   ELSE
071500971021     C                   MOVE      WD15          VABVMD
071600971021     C                   ENDIF
071700050118     C                   ENDIF
071800971021     C*
071900971023     C                   TESTN                   VHBANT               30
072000971023     C  N30              TESTN                   VHBANT                 30
072100971023     C  N30              TESTN                   VHBANT                   30
072200971021     C                   MOVE      VHBANT        W001A             1
072300971021     C     *IN30         IFEQ      *OFF
072400971021     C     W001A         ORLT      '0'
072500971023     C     W001A         ANDNE     *BLANK
072600971021     C                   SETON                                        32
072700971021     C                   MOVE      *ZERO         VABANT
072800010604     C                   eval      vinmsg = %trimr(vinmsg)
072900010604     C                             + ' ' + 'VHBANT'
073000971021     C                   ELSE
073100971021     C                   MOVE      VHBANT        VABANT
073200971021     C                   ENDIF
073300971021     C*
073400971021     C* CAMPI ALFABETICI:
073500971021     C                   MOVE      VHBATB        VABATB
073600971021     C                   MOVE      VHBCBO        VABCBO
073700971021     C                   MOVE      VHBRSD        VABRSD
073800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
073900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
074000020117     C* ==
074100971021     C                   MOVE      VHBRD2        VABRD2
074200971021     C                   MOVE      VHBIND        VABIND
074300971021     C                   MOVE      VHBCAD        VABCAD
074400971021     C                   MOVE      VHBLOD        VABLOD
074500971021     C                   MOVE      VHBPRD        VABPRD
074600971021     C                   MOVE      VHBNZD        VABNZD
074700971021     C                   MOVE      VHBGC1        VABGC1
074800971021     C                   MOVE      VHBGC2        VABGC2
074900971021     C                   MOVE      VHBTSP        VABTSP
075000971021     C                   MOVE      VHBVAS        VABVAS
075100971021     C                   MOVE      VHBNAS        VABNAS
075200971021     C                   MOVE      VHBTIC        VABTIC
075300971021     C                   MOVE      VHBVCA        VABVCA
075400971021     C                   MOVE      VHBGCA        VABGCA
075500971021     C                   MOVE      VHBRMA        VABRMA
075600971021     C                   MOVE      VHBXCO        VABXCO
075700971021     C                   MOVE      VHBNOT        VABNOT
075800971021     C                   MOVE      VHBNT2        VABNT2
075900971021     C                   MOVE      VHBCTM        VABCTM
075901070419     C*
075902070419     C                   IF        VABCTM = *blanks
075903070419     C                   EVAL      VABCTM = '7T'
075905070419     C                   ENDIF
075906070419     C*
076000971021     C                   MOVE      VHBFFD        VABFFD
076100971021     C                   MOVE      VHBTCR        VABTCR
076200971021     C                   MOVE      VHBCTS        VABCTS
076300971021     C                   MOVE      VHBFTM        VABFTM
076400971021     C                   MOVE      VHBVAD        VABVAD
076500971021     C                   MOVE      VHBGMA        VABGMA
076600971021     C                   MOVE      VHBGGA        VABGGA
076700971021     C                   MOVE      VHBGVA        VABGVA
076800971021     C                   MOVE      VHBTC1        VABTC1
076900971021     C                   MOVE      VHBTC2        VABTC2
077000971021     C                   MOVE      VHBSCL        VABSCL
077100971021     C                   MOVE      VHBRMO        VABRMO
077200971021     C                   MOVE      VHBCMO        VABCMO
077300971021     C                   MOVE      VHBNMO        VABNMO
077400971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
077500971021     C* NELLA NAZIONE ORIGINALE
077600971021     C                   MOVE      VHBNMO        WA1D              1
077700971021     C                   MOVEL     VHBNMO        WA1S              1
077800971021     C     WA1D          IFNE      *BLANK
077900971021     C     WA1S          ANDEQ     *BLANK
078000971021     C                   MOVE      *BLANK        VABNMO
078100971021     C                   END
078200050418     C*
078300050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
078400050418     C                   IF        %trim(VABNZD) = 'I'   OR
078500050418     C                             %trim(VABNZD) = 'IT'  OR
078600050418     C                             %trim(VABNZD) = 'ITA'
078700050418     C                   EVAL      VABNZD = *blanks
078800050418     C                   ENDIF
078900041025     C*
079000041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
079100041025     C                   IF        *IN52 = *ON
079200041025     C* Reperisco la provincia dal CAP e dalla località
079300041025     C                   IF        VABCAD <> *blanks AND
079400050907     C                             VABPRD  = *blanks AND
079500050907     C                             VABNZD  = *blanks
079600041025     C                   CLEAR                   TISI95DS
079700041025     C                   EVAL      I95TCN = '3'
079800041025     C                   Z-ADD     datcor        I95DAT
079900041025     C                   EVAL      I95CAP = VABCAD
080000041025     C                   EVAL      I95LOC = VABLOD
080100041025     C                   CALL      'TISI95R'
080200041025     C                   PARM                    TISI95DS
080300041025     C                   EVAL      VABPRD = O95PRV
080400041025     C                   ENDIF
080500041025     C                   ENDIF
080600040630     C*
080700040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
080800040630     C                   exsr      exeBon
080900930409     C*
081000010604     C  N31              ADD       1             §CTROK            7 0
081100010604     C   32              ADD       1             §CTRMO            7 0
081200010604     C   31              ADD       1             §CTRNO            7 0
081300020916     C  N31              WRITE     FIVAB000
081400990910     C*
081500010601     C                   if        *in31 = *off and
081600010601     C                             *in32 = *off
081700990910     C                   eval      vinflg = '1'
081800990910     C                   else
081900000621     C                   eval      recko = vindta
082000010612     C                   if        %open(tis7t4ps)
082100030407     C   50              except    rigadett
082200010612     C                   endif
082300990910     C                   eval      vinflg = '2'
082400990910     C                   endif
082500000905     C                   endif
082600000905     C                   endif
082700010601     C*
082800010601     C                   endif
082900000905     C*
083000000905     C                   else
083100000905     C                   eval      vinflg = '1'
083200000905     C                   endif
083300000905     C*
083400000905     C  N70              update    tivin000
083500000905     C*
083600991022     C  N70              ENDdo
083700010601     C
083800010601     C*
083900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
084000010601     C                   if        cntNonEl = *zeros or
084100010601     C                             flgMulti = '0'
084200990910     C* Se non ci sono record con errori ...
084300010601     C                   if        §ctrno = 0 and
084400010601     C                             §ctrmo = 0 and
084500010601     C                             flgStato <> '2'
084600990910     C* ... restituisco esito OK.
084700990921     C                   eval      wrkesito = '0'
084800990910     C                   else
084900010601     C                   if        §ctrok > 0
085000000710     C                   eval      wrkesito = '1'
085100000710     C                   else
085200010615     C                   if        flgOk = '0'
085300010615     C                   eval      wrkesito = '2'
085400010615     C                   else
085500010615     C                   eval      wrkesito = '6'
085600010615     C                   endif
085700000710     C                   endif
085800010615     C                   endif
085900010601     C                   else
086000010601     C                   eval      wrkesito = '9'
086100010601     C                   endif
086200990910     C*
086300990914     C                   if        %open(tivin00r)
086400990908     C                   close     tivin00r
086500990914     C                   endif
086600020916     C                   if        %open(fivabwwr)
086700020916     C                   close     fivabwwr
086800990914     C                   endif
086900010601     C*
087000010601     C                   if        vlrpoi <> 999
087100010601     C                   eval      invfil = vlrpoi
087200010601     C                   endif
087300990910     C*
087400990920     C                   if        §ctrok > 0
087500010601     C                             and invfil > *zeros
087600990920     C                   exsr      invio
087700990920     C                   endif
087800010612     C*
087900010612     C                   if        flgGiro = '1'
088000010612     C                   exsr      endela
088100010612     C                   endif
088200990920     C*
088300910830     C                   ENDSR
088400000613     C***
088500051130
088600051130
088700051130     C*----------------------------------------------------*
088800051130     C*  CONTROLLO NUMERICITA' CAMPI
088900051130     C*----------------------------------------------------*
089000051130     C     CHKNUM        BEGSR
089100051130     C*
089200051130     C                   call(e)   'ISNUMERIC'
089300051130     C                   PARM                    PiStr            30
089400051130     C                   PARM      ','           PiDecChr          1
089500051130     C                   PARM      *ZEROS        PiVal            30 9
089600051130     C                   PARM      '0'           PiInt             1
089700051130     C                   PARM      '0'           PiNum             1
089800051130     C                   IF        %error
089900051130     C                   EVAL      PiInt=*off
090000051130     C                   ENDIF
090100051130     C*
090200051130     C                   ENDSR
090300051130     C***
090400990920
090500010601
090600010601
090700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
090800010601     C     repfil        BEGSR
090900010601     C*
091000010601     C                   if        invfil = *zeros and
091100010601     C                             depfil > *zeros and
091200010613     C                             (vinflg = *blanks or
091300010613     C                              vinflg = *zeros)
091400010601     C
091500010601     C                   eval      invfil = depfil
091600010601     C                   endif
091700010601     C*
091800010601     C                   if        depfil <> invfil and
091900010601     C                             invfil > *zeros
092000010601     C                   eval      flgMulti = '1'
092100010601     C                   if        vinflg = *blanks
092200010601     C                   add       1             cntNonEl
092300010601     C                   endif
092400010601     C                   endif
092500010601     C*
092600010601     C                   if        vinflg = '2'
092700010601     C                   eval      flgStato = '2'
092800010601     C                   endif
092900010601     C*
093000010601     C                   ENDSR
093100010601     C***
093200010601
093300010601
093400010601
093500010601
093600010601
093700010601
093800010601
093900990920      /TITLE Invio dei dati al punto operativo.
094000000613     C     invio         BEGSR
094100990920     C*
094200990920     C                   reset                   dscmz
094300010601     C                   move      invfil        cmzdst
094400990920     C                   eval      cmzfld = vlrfou
094500990920     C                   eval      cmzmbd = vlrhdl
094600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
094700000710     C                   if        prmfir = *blanks
094800020916     C                   eval      cmzfla = 'FIVAB00F'
094900020916     C                   eval      cmzmba = 'FIVAB00F'
095000000710     C                   else
095100000710     C                   eval      cmzfla = prmfir
095200000710     C                   eval      cmzmba = prmfir
095300000710     C                   endif
095400990920     C                   eval      cmznrr = *zeros
095500990920     C                   move      §ctrok        cmznrr
095600021003     C                   eval      cmzlba = vlrfl1
095700990920     C                   call(e)   'TIS711C'
095800990920     C                   parm                    dscmz
095900990921     C                   parm      *blanks       esito
096000990920     C                   if        %error
096100990920     C                             or cmzerr = '1'
096200990921     C                             or esito  = '1'
096300000710     C                   eval      wrkesito = '3'
096400990920     C                   endif
096500990920     C*
096600000613     C                   ENDSR
096700010601
096800010601
096900010601
097000010601
097100010601      /TITLE Invio dei dati al punto operativo.
097200010601     C     opeini        BEGSR
097300010601     C*
097400010601     C* Inizializzo flag e contatori operativi
097500010601     C                   movel     '0'           flgGiro           1
097600010601     C                   movel     '0'           flgMulti          1
097700010601     C                   movel     '1'           flgStato          1
097800010615     C                   movel     '0'           flgOk             1
097900010601     C                   z-add     *zeros        cntNonEl         10 0
098000010601     C                   z-add     *zeros        depfil            3 0
098100010601     C                   z-add     *zeros        invfil            3 0
098200030407     C*
098300030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
098400030407     C                   if        %subst(vlrppt:1:1) = 'S'
098500030407     C                   seton                                        50
098600030407     C                   else
098700030407     C                   setoff                                       50
098800030407     C                   endif
098900041001     C*
099000041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
099100041001     C                   if        %subst(vlrppt:2:1) = 'S'
099200041001     C                   seton                                        51
099300041001     C                   else
099400041001     C                   setoff                                       51
099500041001     C                   endif
099600041025     C*
099700041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
099800041025     C* (naturalmente solo se nn già presente)
099900041025     C                   if        %subst(vlrppt:3:1) = 'S'
100000041025     C                   seton                                        52
100100041025     C                   else
100200041025     C                   setoff                                       52
100300041025     C                   endif
100400010601     C*
100500010601     C                   ENDSR
100600010601     C***
100700040630
100800040630
100900040630
101000040630
101100040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
101200040630     C     exeBon        BEGSR
101300040630     C*
101400040630     C* Imposto i parametri x il *pgm d "bonifica"
101500040630     C                   EVAL      ElencoChar = *blanks
101600040630     C                   EVAL      TipoElenco = *blanks
101700040630     C                   EVAL      CharSost   = *blanks
101800040630     C                   EVAL      UpperCase  = '1'
101900040630     C                   EVAL      ChkNull    = '1'
102000040630     C                   EVAL      CharNull   = *blanks
102100040630     C*
102200040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
102300040630     C                   EVAL      TxtInOut   = VABRSD
102400040630     C                   EVAL      EsitoBon   = *blanks
102500040630     C*
102600040630     C                   CALL      'XCHKCHAR'
102700040630     C                   PARM                    TxtInOut
102800040630     C                   PARM                    ElencoChar
102900040630     C                   PARM                    TipoElenco
103000040630     C                   PARM                    CharSost
103100040630     C                   PARM                    UpperCase
103200040630     C                   PARM                    ChkNull
103300040630     C                   PARM                    CharNull
103400040630     C                   PARM                    EsitoBon
103500040630     C*
103600040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
103700040630     C                   IF        EsitoBon = '1'
103800040630     C                   EVAL      VABRSD = TxtInOut
103900040630     C                   ENDIF
104000040630     C*
104100040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
104200040630     C                   EVAL      TxtInOut   = VABIND
104300040630     C                   EVAL      EsitoBon   = *blanks
104400040630     C*
104500040630     C                   CALL      'XCHKCHAR'
104600040630     C                   PARM                    TxtInOut
104700040630     C                   PARM                    ElencoChar
104800040630     C                   PARM                    TipoElenco
104900040630     C                   PARM                    CharSost
105000040630     C                   PARM                    UpperCase
105100040630     C                   PARM                    ChkNull
105200040630     C                   PARM                    CharNull
105300040630     C                   PARM                    EsitoBon
105400040630     C*
105500040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
105600040630     C                   IF        EsitoBon = '1'
105700040630     C                   EVAL      VABIND = TxtInOut
105800040630     C                   ENDIF
105900040630     C*
106000040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
106100040630     C                   EVAL      TxtInOut   = VABLOD
106200040630     C                   EVAL      EsitoBon   = *blanks
106300040630     C*
106400040630     C                   CALL      'XCHKCHAR'
106500040630     C                   PARM                    TxtInOut
106600040630     C                   PARM                    ElencoChar
106700040630     C                   PARM                    TipoElenco
106800040630     C                   PARM                    CharSost
106900040630     C                   PARM                    UpperCase
107000040630     C                   PARM                    ChkNull
107100040630     C                   PARM                    CharNull
107200040630     C                   PARM                    EsitoBon
107300040630     C*
107400040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
107500040630     C                   IF        EsitoBon = '1'
107600040630     C                   EVAL      VABLOD = TxtInOut
107700040630     C                   ENDIF
107800040630     C*
107900040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
108000040630     C                   EVAL      TxtInOut   = VABNOT
108100040630     C                   EVAL      EsitoBon   = *blanks
108200040630     C                   EVAL      UpperCase  = *blanks
108300040630     C*
108400040630     C                   CALL      'XCHKCHAR'
108500040630     C                   PARM                    TxtInOut
108600040630     C                   PARM                    ElencoChar
108700040630     C                   PARM                    TipoElenco
108800040630     C                   PARM                    CharSost
108900040630     C                   PARM                    UpperCase
109000040630     C                   PARM                    ChkNull
109100040630     C                   PARM                    CharNull
109200040630     C                   PARM                    EsitoBon
109300040630     C*
109400040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
109500040630     C                   IF        EsitoBon = '1'
109600040630     C                   EVAL      VABNOT = TxtInOut
109700040630     C                   ENDIF
109800040630     C*
109900040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
110000040630     C                   EVAL      TxtInOut   = VABNT2
110100040630     C                   EVAL      EsitoBon   = *blanks
110200040630     C                   EVAL      UpperCase  = *blanks
110300040630     C*
110400040630     C                   CALL      'XCHKCHAR'
110500040630     C                   PARM                    TxtInOut
110600040630     C                   PARM                    ElencoChar
110700040630     C                   PARM                    TipoElenco
110800040630     C                   PARM                    CharSost
110900040630     C                   PARM                    UpperCase
111000040630     C                   PARM                    ChkNull
111100040630     C                   PARM                    CharNull
111200040630     C                   PARM                    EsitoBon
111300040630     C*
111400040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
111500040630     C                   IF        EsitoBon = '1'
111600040630     C                   EVAL      VABNT2 = TxtInOut
111700040630     C                   ENDIF
111800040630     C*
111900040630     C                   ENDSR
112000040630
112100010601
112200010601
112300990910
112400000613     C     *inzsr        BEGSR
112500990910     C*
112600990910     C     *entry        plist
112700990920     C                   parm                    tivlrds
112800990921     C                   parm      wrkesito      esito
112900000724     C                   parm                    prmlit
113000000710     C                   parm                    prmfir
113100010606     C*
113200010606     C* CALCOLA LA DATA CORRENTE
113300010606     C                   time                    wn14             14 0
113400010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
113500010606     C                   z-add     wn8           g08dat
113600010606     C                   z-add     *zeros        g08inv
113700010606     C                   movel     '0'           g08err
113800010606     C                   call      'XSRDA8'
113900010606     C                   parm                    wlbda8
114000010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
114100000613     C*
114200000613     C                   ENDSR
114300000613     C***
114400990908
114500000613     Otis7t4p   E            riepilogo         2
114600990915     O                                              'Upload via Internet'
114700990915     O                                           +1 'Traduzione TIVIN00R -'
114800020916     O                                           +1 'FIVABWWR'
114900990915     O                                           +1 'Testate bolle'
115000990915     O          E            riepilogo   2
115100990915     O                       wrkdata
115200990915     O                       wrkora              +1
115300990915     O                       procname            +1
115400990915     O          E            riepilogo   2
115500990915     O                                              'Cliente..................:'
115600990915     O                       VABCCM        z     +1
115700990915     O          E            riepilogo   2
115800990920     O                                              'Riferimento Strategi.....:'
115900990920     O                       vlrhdl              +1
116000990915     O          E            riepilogo   2
116100990915     O                                              'Giusti...................:'
116200971022     O                       §CTROK        2   +  1
116300990915     O          E            riepilogo   2
116400990915     O                                              'Sbagliati e corretti.....:'
116500971022     O                       §CTRMO        2   +  1
116600990915     O          E            riepilogo   2
116700990915     O                                              'Sbagliati e scartati.....:'
116800971022     O                       §CTRNO        2   +  1
116900000613
117000000616     Otis7t4ps  E            testdett          2
117100000613     O                                              'Upload via Internet'
117200000613     O                                           +1 'Traduzione TIVIN00R -'
117300020916     O                                           +1 'FIVABWWR'
117400000613     O                                           +1 'Testate bolle'
117500000616     O          E            testdett    3
117600000613     O                                           +2 'N° rec'
117700000613     O                                           +3 'Anteprima contenuto'
117800000616     O          E            rigadett    2
117900000613     O                       rrnum               +2
118000000621     O                       recko               +3
118100000616     O          E            findett     2
118200000613     O                       wrkdata
118300000613     O                       wrkora              +1
118400000613     O                       procname            +1
118500000616     O          E            findett     2
118600000613     O                                              'Cliente..................:'
118700000613     O                       VABCCM        z     +1
118800000616     O          E            findett     2
118900000613     O                                              'Riferimento Strategi.....:'
119000000613     O                       vlrhdl              +1
119100000616     O          E            findett     2
119200000613     O                                              'Giusti...................:'
119300000613     O                       §CTROK        2   +  1
119400000616     O          E            findett     2
119500000613     O                                              'Sbagliati e corretti.....:'
119600000613     O                       §CTRMO        2   +  1
119700000616     O          E            findett     2
119800000613     O                                              'Sbagliati e scartati.....:'
119900000613     O                       §CTRNO        2   +  1
120000000613** CMD - COMANDI CL
120100011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4P)  FORMTYPE(RICCLI) OUTQ(
120200011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4PS) FORMTYPE(RICCLI) OUTQ(
120300000616DLTOVR FILE(TIS7T4P TIS7T4PS) LVL(*)
120400000613
120500000613
