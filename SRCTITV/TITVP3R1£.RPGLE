000100060112      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600060112     FEDIVABwr  O    E             DISK    usropn
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
005000070326     D vhbldv                 18     28
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
025500060112     C* RWVAB   LEGGE tivin00r E SCRIVE EDIVABWF              *
025600910830     C*--------------------------------------------------------
025700941229     C     RWVAB         BEGSR
025800990910     C*
025900990914     C                   if        not %open(tivin00r)
026000990908     C                   open      tivin00r
026100990914     C                   endif
026200060112     C                   if        not %open(edivabwr)
026300060112     C                   open      edivabwr
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
027900051017     C                   if        vindta  > *blanks
028000000613     C                   add       1             rrnum
028100990913     C*
028200010601     C                   if        *in70 = *off and
028300990913     C                             (vinflg = *blanks
028400010711     C                              or vinflg = '0'
028500010711     C                              or vinflg = '2')
028600990913     C*
028700060112     C                   clear                   EDIVAB00
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
036600070215     C*                  TESTN                   VHBMGS               30
036700070215     C* N30              TESTN                   VHBMGS                 30
036800070215     C* N30              TESTN                   VHBMGS                   30
036900070215     C*                  MOVE      VHBMGS        W001A             1
037000070215     C*    *IN30         IFEQ      *OFF
037100070215     C*    W001A         ORLT      '0'
037200070215     C*    W001A         ANDNE     *BLANK
037300070215     C*                  SETON                                        32
037400070215     C*                  MOVE      '0101'        VABMGS
037500070215     C*                  eval      vinmsg = %trimr(vinmsg)
037600070215     C*                            + ' ' + 'VHBMGS'
037700070215     C*                  ELSE
037800070215     C*                  MOVE      VHBMGS        VABMGS
037900070215     C*                  ENDIF
038000971021     C*
038100070215     C*                  TESTN                   VHBNRS               30
038200070215     C* N30              TESTN                   VHBNRS                 30
038300070215     C* N30              TESTN                   VHBNRS                   30
038400070215     C*                  MOVE      VHBNRS        W001A             1
038500070215     C*    *IN30         IFEQ      *OFF
038600070215     C*    W001A         ORLT      '0'
038700070215     C*    W001A         ANDNE     *BLANK
038800070215     C*                  SETON                                        32
038900070215     C*                  MOVE      *ZERO         VABNRS
039000070215     C*                  eval      vinmsg = %trimr(vinmsg)
039100070215     C*                            + ' ' + 'VHBNRS'
039200070215     C*                  ELSE
039300070215     C*                  MOVE      VHBNRS        VABNRS
039400070215     C*                  ENDIF
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
048600051130     C                   EVAL      PiStr=VHBPKB
048700051130     C                   EXSR      CHKNUM
048800051130     C                   IF        PiNum=*on
048900051130     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 decim.
049000051130     C                   Z-ADD(H)  PiVal         VABPKB
049100051130     C                   ELSE
049200971021     C                   SETON                                        32
049300051130     C                   Z-ADD     *ZERO         VABPKB
049400010604     C                   eval      vinmsg = %trimr(vinmsg)
049500010604     C                             + ' ' + 'VHBPKB'
049600050118     C                   ENDIF
049700971021     C*
049800041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
049900041001     C                   IF        *IN51 = *OFF
050000971021     C                   MOVE      *ZERO         VABVLB
050100971021     C                   MOVE      VHBVLB        WASC07
050200050503     C                   IF        WI07 = *blanks
050300050503     C                   EVAL      WI07 = *zeros
050400050503     C                   ENDIF
050500971023     C                   TESTN                   WI07                 30
050600971023     C  N30              TESTN                   WI07                   30
050700971023     C  N30              TESTN                   WI07                     30
050800971021     C                   MOVE      WI07          W001A             1
050900971021     C     *IN30         IFEQ      *OFF
051000971021     C     W001A         ORLT      '0'
051100971023     C     W001A         ANDNE     *BLANK
051200971021     C                   SETON                                        32
051300971021     C                   MOVE      *ZERO         VABVLB
051400010604     C                   eval      vinmsg = %trimr(vinmsg)
051500010604     C                             + ' ' + 'VHBVLB'
051600971021     C                   ELSE
051700971021     C                   MOVEL     WI07          VABVLB
051800971023     C                   TESTN                   WD07                 30
051900971023     C  N30              TESTN                   WD07                   30
052000971023     C  N30              TESTN                   WD07                     30
052100971021     C                   MOVE      WD07          W001A             1
052200971021     C     *IN30         IFEQ      *OFF
052300971021     C     W001A         ORLT      '0'
052400971023     C     W001A         ANDNE     *BLANK
052500971021     C                   SETON                                        32
052600971021     C                   MOVE      *ZERO         VABVLB
052700010604     C                   eval      vinmsg = %trimr(vinmsg)
052800010604     C                             + ' ' + 'VHBVLB'
052900971021     C                   ELSE
053000971021     C                   MOVE      WD07          VABVLB
053100971021     C                   ENDIF
053200050118     C                   ENDIF
053300041001     C                   ELSE
053400041001     C* ...altrimenti forzo azzeramento del volume
053500041001     C                   Z-ADD     *zeros        VABVLB
053600041001     C                   ENDIF
053700941229     C*
053800971021     C                   MOVE      *ZERO         VABQFT
053900971021     C                   MOVE      VHBQFT        WASC15
054000971023     C                   TESTN                   WI15                 30
054100971023     C  N30              TESTN                   WI15                   30
054200971023     C  N30              TESTN                   WI15                     30
054300971021     C                   MOVE      WI15          W001A             1
054400971021     C     *IN30         IFEQ      *OFF
054500971021     C     W001A         ORLT      '0'
054600971023     C     W001A         ANDNE     *BLANK
054700971021     C                   SETON                                        32
054800971021     C                   MOVE      *ZERO         VABQFT
054900010604     C                   eval      vinmsg = %trimr(vinmsg)
055000010604     C                             + ' ' + 'VHBQFT'
055100971021     C                   ELSE
055200971021     C                   MOVEL     WI15          VABQFT
055300971023     C                   TESTN                   WD15                 30
055400971023     C  N30              TESTN                   WD15                   30
055500971023     C  N30              TESTN                   WD15                     30
055600971021     C                   MOVE      WD15          W001A             1
055700971021     C     *IN30         IFEQ      *OFF
055800971021     C     W001A         ORLT      '0'
055900971023     C     W001A         ANDNE     *BLANK
056000971021     C                   SETON                                        32
056100971021     C                   MOVE      *ZERO         VABQFT
056200010604     C                   eval      vinmsg = %trimr(vinmsg)
056300010604     C                             + ' ' + 'VHBQFT'
056400971021     C                   ELSE
056500971021     C                   MOVE      WD15          VABQFT
056600971021     C                   ENDIF
056700050118     C                   ENDIF
056800941229     C*
056900971021     C                   MOVE      *ZERO         VABCAS
057000971021     C                   MOVE      VHBCAS        WASC15
057100971023     C                   TESTN                   WI15                 30
057200971023     C  N30              TESTN                   WI15                   30
057300971023     C  N30              TESTN                   WI15                     30
057400971021     C                   MOVE      WI15          W001A             1
057500971021     C     *IN30         IFEQ      *OFF
057600971021     C     W001A         ORLT      '0'
057700971023     C     W001A         ANDNE     *BLANK
057800971021     C                   SETON                                        32
057900971021     C                   MOVE      *ZERO         VABCAS
058000010604     C                   eval      vinmsg = %trimr(vinmsg)
058100010604     C                             + ' ' + 'VHBCAS'
058200971021     C                   ELSE
058300971021     C                   MOVEL     WI15          VABCAS
058400971023     C                   TESTN                   WD15                 30
058500971023     C  N30              TESTN                   WD15                   30
058600971023     C  N30              TESTN                   WD15                     30
058700971021     C                   MOVE      WD15          W001A             1
058800971021     C     *IN30         IFEQ      *OFF
058900971021     C     W001A         ORLT      '0'
059000971023     C     W001A         ANDNE     *BLANK
059100971021     C                   SETON                                        32
059200971021     C                   MOVE      *ZERO         VABCAS
059300010604     C                   eval      vinmsg = %trimr(vinmsg)
059400010604     C                             + ' ' + 'VHBCAS'
059500971021     C                   ELSE
059600971021     C                   MOVE      WD15          VABCAS
059700971021     C                   ENDIF
059800040827     C                   ENDIF
059900971021     C*
060000971023     C                   TESTN                   VHBRMN               30
060100971023     C  N30              TESTN                   VHBRMN                 30
060200971023     C  N30              TESTN                   VHBRMN                   30
060300971021     C                   MOVE      VHBRMN        W001A             1
060400971021     C     *IN30         IFEQ      *OFF
060500971021     C     W001A         ORLT      '0'
060600971021     C                   SETON                                        32
060700971021     C                   MOVE      *ZERO         VABRMN
060800010604     C                   eval      vinmsg = %trimr(vinmsg)
060900010604     C                             + ' ' + 'VHBRMN'
061000971021     C                   ELSE
061100971021     C                   MOVE      VHBRMN        VABRMN
061200971021     C                   ENDIF
061300971021     C*
061400971023     C                   TESTN                   VHBNCD               30
061500971023     C  N30              TESTN                   VHBNCD                 30
061600971023     C  N30              TESTN                   VHBNCD                   30
061700971021     C                   MOVE      VHBNCD        W001A             1
061800971021     C     *IN30         IFEQ      *OFF
061900971021     C     W001A         ORLT      '0'
062000971023     C     W001A         ANDNE     *BLANK
062100971021     C                   SETON                                        32
062200971021     C                   MOVE      *ZERO         VABNCD
062300010604     C                   eval      vinmsg = %trimr(vinmsg)
062400010604     C                             + ' ' + 'VHBNCD'
062500971021     C                   ELSE
062600971021     C                   MOVE      VHBNCD        VABNCD
062700971021     C                   ENDIF
062800971021     C*
062900971023     C                   TESTN                   VHBNCA               30
063000971023     C  N30              TESTN                   VHBNCA                 30
063100971023     C  N30              TESTN                   VHBNCA                   30
063200971021     C                   MOVE      VHBNCA        W001A             1
063300971021     C     *IN30         IFEQ      *OFF
063400971021     C     W001A         ORLT      '0'
063500971023     C     W001A         ANDNE     *BLANK
063600971021     C                   SETON                                        32
063700971021     C                   MOVE      *ZERO         VABNCA
063800010604     C                   eval      vinmsg = %trimr(vinmsg)
063900010604     C                             + ' ' + 'VHBNCA'
064000971021     C                   ELSE
064100971021     C                   MOVE      VHBNCA        VABNCA
064200971021     C                   ENDIF
064300971021     C*
064400971023     C                   TESTN                   VHBZNC               30
064500971023     C  N30              TESTN                   VHBZNC                 30
064600971023     C  N30              TESTN                   VHBZNC                   30
064700971021     C                   MOVE      VHBZNC        W001A             1
064800971021     C     *IN30         IFEQ      *OFF
064900971021     C     W001A         ORLT      '0'
065000971023     C     W001A         ANDNE     *BLANK
065100971021     C                   SETON                                        32
065200971021     C                   MOVE      *ZERO         VABZNC
065300010604     C                   eval      vinmsg = %trimr(vinmsg)
065400010604     C                             + ' ' + 'VHBZNC'
065500971021     C                   ELSE
065600971021     C                   MOVE      VHBZNC        VABZNC
065700971021     C                   ENDIF
065800971021     C*
065900971023     C                   TESTN                   VHBDCR               30
066000971023     C  N30              TESTN                   VHBDCR                 30
066100971023     C  N30              TESTN                   VHBDCR                   30
066200971021     C                   MOVE      VHBDCR        W001A             1
066300971021     C     *IN30         IFEQ      *OFF
066400971021     C     W001A         ORLT      '0'
066500971023     C     W001A         ANDNE     *BLANK
066600971021     C                   SETON                                        32
066700971021     C                   MOVE      *ZERO         VABDCR
066800010604     C                   eval      vinmsg = %trimr(vinmsg)
066900010604     C                             + ' ' + 'VHBDCR'
067000971021     C                   ELSE
067100971021     C                   MOVE      VHBDCR        VABDCR
067200971021     C                   ENDIF
067300971021     C*
067400971023     C                   TESTN                   VHBHCR               30
067500971023     C  N30              TESTN                   VHBHCR                 30
067600971023     C  N30              TESTN                   VHBHCR                   30
067700971021     C                   MOVE      VHBHCR        W001A             1
067800971021     C     *IN30         IFEQ      *OFF
067900971021     C     W001A         ORLT      '0'
068000971023     C     W001A         ANDNE     *BLANK
068100971021     C                   SETON                                        32
068200971021     C                   MOVE      *ZERO         VABHCR
068300010604     C                   eval      vinmsg = %trimr(vinmsg)
068400010604     C                             + ' ' + 'VHBHCR'
068500971021     C                   ELSE
068600971021     C                   MOVE      VHBHCR        VABHCR
068700971021     C                   ENDIF
068800941229     C*
068900971021     C                   MOVE      *ZERO         VABVMD
069000971021     C                   MOVE      VHBVMD        WASC15
069100971023     C                   TESTN                   WI15                 30
069200971023     C  N30              TESTN                   WI15                   30
069300971023     C  N30              TESTN                   WI15                     30
069400971021     C                   MOVE      WI15          W001A             1
069500971021     C     *IN30         IFEQ      *OFF
069600971021     C     W001A         ORLT      '0'
069700971023     C     W001A         ANDNE     *BLANK
069800971021     C                   SETON                                        32
069900971021     C                   MOVE      *ZERO         VABVMD
070000010604     C                   eval      vinmsg = %trimr(vinmsg)
070100010604     C                             + ' ' + 'VHBVMD'
070200971021     C                   ELSE
070300971021     C                   MOVEL     WI15          VABVMD
070400971023     C                   TESTN                   WD15                 30
070500971023     C  N30              TESTN                   WD15                   30
070600971023     C  N30              TESTN                   WD15                     30
070700971021     C                   MOVE      WD15          W001A             1
070800971021     C     *IN30         IFEQ      *OFF
070900971021     C     W001A         ORLT      '0'
071000971023     C     W001A         ANDNE     *BLANK
071100971021     C                   SETON                                        32
071200971021     C                   MOVE      *ZERO         VABVMD
071300010604     C                   eval      vinmsg = %trimr(vinmsg)
071400010604     C                             + ' ' + 'VHBVMD'
071500971021     C                   ELSE
071600971021     C                   MOVE      WD15          VABVMD
071700971021     C                   ENDIF
071800050118     C                   ENDIF
071900971021     C*
072000971023     C                   TESTN                   VHBANT               30
072100971023     C  N30              TESTN                   VHBANT                 30
072200971023     C  N30              TESTN                   VHBANT                   30
072300971021     C                   MOVE      VHBANT        W001A             1
072400971021     C     *IN30         IFEQ      *OFF
072500971021     C     W001A         ORLT      '0'
072600971023     C     W001A         ANDNE     *BLANK
072700971021     C                   SETON                                        32
072800971021     C                   MOVE      *ZERO         VABANT
072900010604     C                   eval      vinmsg = %trimr(vinmsg)
073000010604     C                             + ' ' + 'VHBANT'
073100971021     C                   ELSE
073200971021     C                   MOVE      VHBANT        VABANT
073300971021     C                   ENDIF
073400971021     C*
073500971021     C* CAMPI ALFABETICI:
073600971021     C                   MOVE      VHBATB        VABATB
073700971021     C                   MOVE      VHBCBO        VABCBO
073800971021     C                   MOVE      VHBRSD        VABRSD
073900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
074000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
074100020117     C* ==
074200971021     C                   MOVE      VHBRD2        VABRD2
074300971021     C                   MOVE      VHBIND        VABIND
074400971021     C                   MOVE      VHBCAD        VABCAD
074500971021     C                   MOVE      VHBLOD        VABLOD
074600971021     C                   MOVE      VHBPRD        VABPRD
074700971021     C                   MOVE      VHBNZD        VABNZD
074800971021     C                   MOVE      VHBGC1        VABGC1
074900971021     C                   MOVE      VHBGC2        VABGC2
075000971021     C                   MOVE      VHBTSP        VABTSP
075100971021     C                   MOVE      VHBVAS        VABVAS
075200971021     C                   MOVE      VHBNAS        VABNAS
075300971021     C                   MOVE      VHBTIC        VABTIC
075400971021     C                   MOVE      VHBVCA        VABVCA
075500971021     C                   MOVE      VHBGCA        VABGCA
075600971021     C                   MOVE      VHBRMA        VABRMA
075700971021     C                   MOVE      VHBXCO        VABXCO
075800971021     C                   MOVE      VHBNOT        VABNOT
075900971021     C                   MOVE      VHBNT2        VABNT2
076000971021     C                   MOVE      VHBCTM        VABCTM
076001070419     C*
076002070419     C                   IF        VABCTM = *blanks
076003070419     C                   EVAL      VABCTM = '7T'
076004070419     C                   ENDIF
076005070419     C*
076100971021     C                   MOVE      VHBFFD        VABFFD
076200971021     C                   MOVE      VHBTCR        VABTCR
076300971021     C                   MOVE      VHBCTS        VABCTS
076400971021     C                   MOVE      VHBFTM        VABFTM
076500971021     C                   MOVE      VHBVAD        VABVAD
076600971021     C                   MOVE      VHBGMA        VABGMA
076700971021     C                   MOVE      VHBGGA        VABGGA
076800971021     C                   MOVE      VHBGVA        VABGVA
076900971021     C                   MOVE      VHBTC1        VABTC1
077000971021     C                   MOVE      VHBTC2        VABTC2
077100971021     C                   MOVE      VHBSCL        VABSCL
077200971021     C                   MOVE      VHBRMO        VABRMO
077300971021     C                   MOVE      VHBCMO        VABCMO
077400971021     C                   MOVE      VHBNMO        VABNMO
077500971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
077600971021     C* NELLA NAZIONE ORIGINALE
077700971021     C                   MOVE      VHBNMO        WA1D              1
077800971021     C                   MOVEL     VHBNMO        WA1S              1
077900971021     C     WA1D          IFNE      *BLANK
078000971021     C     WA1S          ANDEQ     *BLANK
078100971021     C                   MOVE      *BLANK        VABNMO
078200971021     C                   END
078300050418     C*
078400050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
078500050418     C                   IF        %trim(VABNZD) = 'I'   OR
078600050418     C                             %trim(VABNZD) = 'IT'  OR
078700050418     C                             %trim(VABNZD) = 'ITA'
078800050418     C                   EVAL      VABNZD = *blanks
078900050418     C                   ENDIF
079000070214     C*
079100070214     C* Se richiesto nei parametri determinato codice trattamento merce gestisco
079200070214     C                   if        %subst(vlrppt:4:2) <> *blanks
079300070214     C                   eval      vabctm = %subst(vlrppt:4:2)
079400070214     C                   endif
079500041025     C*
079600041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
079700041025     C                   IF        *IN52 = *ON
079800041025     C* Reperisco la provincia dal CAP e dalla località
079900041025     C                   IF        VABCAD <> *blanks AND
080000050907     C                             VABPRD  = *blanks AND
080100050907     C                             VABNZD  = *blanks
080200041025     C                   CLEAR                   TISI95DS
080300041025     C                   EVAL      I95TCN = '3'
080400041025     C                   Z-ADD     datcor        I95DAT
080500041025     C                   EVAL      I95CAP = VABCAD
080600041025     C                   EVAL      I95LOC = VABLOD
080700041025     C                   CALL      'TISI95R'
080800041025     C                   PARM                    TISI95DS
080900041025     C                   EVAL      VABPRD = O95PRV
081000041025     C                   ENDIF
081100041025     C                   ENDIF
081200040630     C*
081300040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
081400040630     C                   exsr      exeBon
081500060112     C*
081600060112     C* Valorizzo i campi relativi al CMR
081700070417     C                   EVAL      VABCMR = %trim(vhbldv)
081800060112     C                   EVAL      VABDCM = datcor
081900060112     C                   EVAL      VABDTS = datcor
082000060112     C                   EVAL      VABHMS = oracor
082100060112     C                   EVAL      VABCNT = 1
082200930409     C*
082300010604     C  N31              ADD       1             §CTROK            7 0
082400010604     C   32              ADD       1             §CTRMO            7 0
082500010604     C   31              ADD       1             §CTRNO            7 0
082600060112     C  N31              WRITE     EDIVAB00
082700990910     C*
082800010601     C                   if        *in31 = *off and
082900010601     C                             *in32 = *off
083000990910     C                   eval      vinflg = '1'
083100990910     C                   else
083200000621     C                   eval      recko = vindta
083300010612     C                   if        %open(tis7t4ps)
083400030407     C   50              except    rigadett
083500010612     C                   endif
083600990910     C                   eval      vinflg = '2'
083700990910     C                   endif
083800000905     C                   endif
083900000905     C                   endif
084000010601     C*
084100010601     C                   endif
084200000905     C*
084300000905     C                   else
084400000905     C                   eval      vinflg = '1'
084500000905     C                   endif
084600000905     C*
084700000905     C  N70              update    tivin000
084800000905     C*
084900991022     C  N70              ENDdo
085000010601     C
085100010601     C*
085200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
085300010601     C                   if        cntNonEl = *zeros or
085400010601     C                             flgMulti = '0'
085500990910     C* Se non ci sono record con errori ...
085600010601     C                   if        §ctrno = 0 and
085700010601     C                             §ctrmo = 0 and
085800010601     C                             flgStato <> '2'
085900990910     C* ... restituisco esito OK.
086000990921     C                   eval      wrkesito = '0'
086100990910     C                   else
086200010601     C                   if        §ctrok > 0
086300000710     C                   eval      wrkesito = '1'
086400000710     C                   else
086500010615     C                   if        flgOk = '0'
086600010615     C                   eval      wrkesito = '2'
086700010615     C                   else
086800010615     C                   eval      wrkesito = '6'
086900010615     C                   endif
087000000710     C                   endif
087100010615     C                   endif
087200010601     C                   else
087300010601     C                   eval      wrkesito = '9'
087400010601     C                   endif
087500990910     C*
087600990914     C                   if        %open(tivin00r)
087700990908     C                   close     tivin00r
087800990914     C                   endif
087900060112     C                   if        %open(edivabwr)
088000060112     C                   close     edivabwr
088100990914     C                   endif
088200010601     C*
088300010601     C                   if        vlrpoi <> 999
088400010601     C                   eval      invfil = vlrpoi
088500010601     C                   endif
088600990910     C*
088700990920     C                   if        §ctrok > 0
088800010601     C                             and invfil > *zeros
088900990920     C                   exsr      invio
089000990920     C                   endif
089100010612     C*
089200010612     C                   if        flgGiro = '1'
089300010612     C                   exsr      endela
089400010612     C                   endif
089500990920     C*
089600910830     C                   ENDSR
089700000613     C***
089800051130
089900051130
090000051130     C*----------------------------------------------------*
090100051130     C*  CONTROLLO NUMERICITA' CAMPI
090200051130     C*----------------------------------------------------*
090300051130     C     CHKNUM        BEGSR
090400051130     C*
090500051130     C                   call(e)   'ISNUMERIC'
090600051130     C                   PARM                    PiStr            30
090700051130     C                   PARM      ','           PiDecChr          1
090800051130     C                   PARM      *ZEROS        PiVal            30 9
090900051130     C                   PARM      '0'           PiInt             1
091000051130     C                   PARM      '0'           PiNum             1
091100051130     C                   IF        %error
091200051130     C                   EVAL      PiInt=*off
091300051130     C                   ENDIF
091400051130     C*
091500051130     C                   ENDSR
091600051130     C***
091700990920
091800010601
091900010601
092000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
092100010601     C     repfil        BEGSR
092200010601     C*
092300010601     C                   if        invfil = *zeros and
092400010601     C                             depfil > *zeros and
092500010613     C                             (vinflg = *blanks or
092600010613     C                              vinflg = *zeros)
092700010601     C
092800010601     C                   eval      invfil = depfil
092900010601     C                   endif
093000010601     C*
093100010601     C                   if        depfil <> invfil and
093200010601     C                             invfil > *zeros
093300010601     C                   eval      flgMulti = '1'
093400010601     C                   if        vinflg = *blanks
093500010601     C                   add       1             cntNonEl
093600010601     C                   endif
093700010601     C                   endif
093800010601     C*
093900010601     C                   if        vinflg = '2'
094000010601     C                   eval      flgStato = '2'
094100010601     C                   endif
094200010601     C*
094300010601     C                   ENDSR
094400010601     C***
094500010601
094600010601
094700010601
094800010601
094900010601
095000010601
095100010601
095200990920      /TITLE Invio dei dati al punto operativo.
095300000613     C     invio         BEGSR
095400990920     C*
095500990920     C                   reset                   dscmz
095600010601     C                   move      invfil        cmzdst
095700990920     C                   eval      cmzfld = vlrfou
095800990920     C                   eval      cmzmbd = vlrhdl
095900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
096000000710     C                   if        prmfir = *blanks
096100060112     C                   eval      cmzfla = 'EDIVAB0F'
096200060112     C                   eval      cmzmba = 'EDIVAB0F'
096300000710     C                   else
096400000710     C                   eval      cmzfla = prmfir
096500000710     C                   eval      cmzmba = prmfir
096600000710     C                   endif
096700990920     C                   eval      cmznrr = *zeros
096800990920     C                   move      §ctrok        cmznrr
096900021003     C                   eval      cmzlba = vlrfl1
097000990920     C                   call(e)   'TIS711C'
097100990920     C                   parm                    dscmz
097200990921     C                   parm      *blanks       esito
097300990920     C                   if        %error
097400990920     C                             or cmzerr = '1'
097500990921     C                             or esito  = '1'
097600000710     C                   eval      wrkesito = '3'
097700990920     C                   endif
097800990920     C*
097900000613     C                   ENDSR
098000010601
098100010601
098200010601
098300010601
098400010601      /TITLE Invio dei dati al punto operativo.
098500010601     C     opeini        BEGSR
098600010601     C*
098700010601     C* Inizializzo flag e contatori operativi
098800010601     C                   movel     '0'           flgGiro           1
098900010601     C                   movel     '0'           flgMulti          1
099000010601     C                   movel     '1'           flgStato          1
099100010615     C                   movel     '0'           flgOk             1
099200010601     C                   z-add     *zeros        cntNonEl         10 0
099300010601     C                   z-add     *zeros        depfil            3 0
099400010601     C                   z-add     *zeros        invfil            3 0
099500030407     C*
099600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
099700030407     C                   if        %subst(vlrppt:1:1) = 'S'
099800030407     C                   seton                                        50
099900030407     C                   else
100000030407     C                   setoff                                       50
100100030407     C                   endif
100200041001     C*
100300041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
100400041001     C                   if        %subst(vlrppt:2:1) = 'S'
100500041001     C                   seton                                        51
100600041001     C                   else
100700041001     C                   setoff                                       51
100800041001     C                   endif
100900041025     C*
101000041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
101100041025     C* (naturalmente solo se nn già presente)
101200041025     C                   if        %subst(vlrppt:3:1) = 'S'
101300041025     C                   seton                                        52
101400041025     C                   else
101500041025     C                   setoff                                       52
101600041025     C                   endif
101700010601     C*
101800010601     C                   ENDSR
101900010601     C***
102000040630
102100040630
102200040630
102300040630
102400040630      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
102500040630     C     exeBon        BEGSR
102600040630     C*
102700040630     C* Imposto i parametri x il *pgm d "bonifica"
102800040630     C                   EVAL      ElencoChar = *blanks
102900040630     C                   EVAL      TipoElenco = *blanks
103000040630     C                   EVAL      CharSost   = *blanks
103100040630     C                   EVAL      UpperCase  = '1'
103200040630     C                   EVAL      ChkNull    = '1'
103300040630     C                   EVAL      CharNull   = *blanks
103400040630     C*
103500040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
103600040630     C                   EVAL      TxtInOut   = VABRSD
103700040630     C                   EVAL      EsitoBon   = *blanks
103800040630     C*
103900040630     C                   CALL      'XCHKCHAR'
104000040630     C                   PARM                    TxtInOut
104100040630     C                   PARM                    ElencoChar
104200040630     C                   PARM                    TipoElenco
104300040630     C                   PARM                    CharSost
104400040630     C                   PARM                    UpperCase
104500040630     C                   PARM                    ChkNull
104600040630     C                   PARM                    CharNull
104700040630     C                   PARM                    EsitoBon
104800040630     C*
104900040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
105000040630     C                   IF        EsitoBon = '1'
105100040630     C                   EVAL      VABRSD = TxtInOut
105200040630     C                   ENDIF
105300040630     C*
105400040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
105500040630     C                   EVAL      TxtInOut   = VABIND
105600040630     C                   EVAL      EsitoBon   = *blanks
105700040630     C*
105800040630     C                   CALL      'XCHKCHAR'
105900040630     C                   PARM                    TxtInOut
106000040630     C                   PARM                    ElencoChar
106100040630     C                   PARM                    TipoElenco
106200040630     C                   PARM                    CharSost
106300040630     C                   PARM                    UpperCase
106400040630     C                   PARM                    ChkNull
106500040630     C                   PARM                    CharNull
106600040630     C                   PARM                    EsitoBon
106700040630     C*
106800040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
106900040630     C                   IF        EsitoBon = '1'
107000040630     C                   EVAL      VABIND = TxtInOut
107100040630     C                   ENDIF
107200040630     C*
107300040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
107400040630     C                   EVAL      TxtInOut   = VABLOD
107500040630     C                   EVAL      EsitoBon   = *blanks
107600040630     C*
107700040630     C                   CALL      'XCHKCHAR'
107800040630     C                   PARM                    TxtInOut
107900040630     C                   PARM                    ElencoChar
108000040630     C                   PARM                    TipoElenco
108100040630     C                   PARM                    CharSost
108200040630     C                   PARM                    UpperCase
108300040630     C                   PARM                    ChkNull
108400040630     C                   PARM                    CharNull
108500040630     C                   PARM                    EsitoBon
108600040630     C*
108700040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
108800040630     C                   IF        EsitoBon = '1'
108900040630     C                   EVAL      VABLOD = TxtInOut
109000040630     C                   ENDIF
109100040630     C*
109200040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
109300040630     C                   EVAL      TxtInOut   = VABNOT
109400040630     C                   EVAL      EsitoBon   = *blanks
109500040630     C                   EVAL      UpperCase  = *blanks
109600040630     C*
109700040630     C                   CALL      'XCHKCHAR'
109800040630     C                   PARM                    TxtInOut
109900040630     C                   PARM                    ElencoChar
110000040630     C                   PARM                    TipoElenco
110100040630     C                   PARM                    CharSost
110200040630     C                   PARM                    UpperCase
110300040630     C                   PARM                    ChkNull
110400040630     C                   PARM                    CharNull
110500040630     C                   PARM                    EsitoBon
110600040630     C*
110700040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
110800040630     C                   IF        EsitoBon = '1'
110900040630     C                   EVAL      VABNOT = TxtInOut
111000040630     C                   ENDIF
111100040630     C*
111200040630     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
111300040630     C                   EVAL      TxtInOut   = VABNT2
111400040630     C                   EVAL      EsitoBon   = *blanks
111500040630     C                   EVAL      UpperCase  = *blanks
111600040630     C*
111700040630     C                   CALL      'XCHKCHAR'
111800040630     C                   PARM                    TxtInOut
111900040630     C                   PARM                    ElencoChar
112000040630     C                   PARM                    TipoElenco
112100040630     C                   PARM                    CharSost
112200040630     C                   PARM                    UpperCase
112300040630     C                   PARM                    ChkNull
112400040630     C                   PARM                    CharNull
112500040630     C                   PARM                    EsitoBon
112600040630     C*
112700040630     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
112800040630     C                   IF        EsitoBon = '1'
112900040630     C                   EVAL      VABNT2 = TxtInOut
113000040630     C                   ENDIF
113100040630     C*
113200040630     C                   ENDSR
113300040630
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
114800060112     C                   movel     wn14          oracor            6 0          *ORA  (8) IN HH/MM/S
114900010606     C                   z-add     wn8           g08dat
115000010606     C                   z-add     *zeros        g08inv
115100010606     C                   movel     '0'           g08err
115200010606     C                   call      'XSRDA8'
115300010606     C                   parm                    wlbda8
115400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
115500000613     C*
115600000613     C                   ENDSR
115700000613     C***
115800990908
115900000613     Otis7t4p   E            riepilogo         2
116000990915     O                                              'Upload via Internet'
116100990915     O                                           +1 'Traduzione TIVIN00R -'
116200060112     O                                           +1 'EDIVABWR'
116300990915     O                                           +1 'Testate bolle'
116400990915     O          E            riepilogo   2
116500990915     O                       wrkdata
116600990915     O                       wrkora              +1
116700990915     O                       procname            +1
116800990915     O          E            riepilogo   2
116900990915     O                                              'Cliente..................:'
117000990915     O                       VABCCM        z     +1
117100990915     O          E            riepilogo   2
117200990920     O                                              'Riferimento Strategi.....:'
117300990920     O                       vlrhdl              +1
117400990915     O          E            riepilogo   2
117500990915     O                                              'Giusti...................:'
117600971022     O                       §CTROK        2   +  1
117700990915     O          E            riepilogo   2
117800990915     O                                              'Sbagliati e corretti.....:'
117900971022     O                       §CTRMO        2   +  1
118000990915     O          E            riepilogo   2
118100990915     O                                              'Sbagliati e scartati.....:'
118200971022     O                       §CTRNO        2   +  1
118300000613
118400000616     Otis7t4ps  E            testdett          2
118500000613     O                                              'Upload via Internet'
118600000613     O                                           +1 'Traduzione TIVIN00R -'
118700060112     O                                           +1 'EDIVABWR'
118800000613     O                                           +1 'Testate bolle'
118900000616     O          E            testdett    3
119000000613     O                                           +2 'N° rec'
119100000613     O                                           +3 'Anteprima contenuto'
119200000616     O          E            rigadett    2
119300000613     O                       rrnum               +2
119400000621     O                       recko               +3
119500000616     O          E            findett     2
119600000613     O                       wrkdata
119700000613     O                       wrkora              +1
119800000613     O                       procname            +1
119900000616     O          E            findett     2
120000000613     O                                              'Cliente..................:'
120100000613     O                       VABCCM        z     +1
120200000616     O          E            findett     2
120300000613     O                                              'Riferimento Strategi.....:'
120400000613     O                       vlrhdl              +1
120500000616     O          E            findett     2
120600000613     O                                              'Giusti...................:'
120700000613     O                       §CTROK        2   +  1
120800000616     O          E            findett     2
120900000613     O                                              'Sbagliati e corretti.....:'
121000000613     O                       §CTRMO        2   +  1
121100000616     O          E            findett     2
121200000613     O                                              'Sbagliati e scartati.....:'
121300000613     O                       §CTRNO        2   +  1
121400000613** CMD - COMANDI CL
121500011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4P)  FORMTYPE(RICCLI) OUTQ(
121600011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4PS) FORMTYPE(RICCLI) OUTQ(
121700000616DLTOVR FILE(TIS7T4P TIS7T4PS) LVL(*)
121800000613
121900000613
