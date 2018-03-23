000100020916      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500030811     Ffnacr01l  if   e           k disk
000600030811     Ftntbe01l  uf   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800020916     FFIVABwwr  O    E             DISK    usropn
000900030926     FFNORM00F  O    E             DISK    usropn
001000030929     FFNORMTMPL uF   E           K DISK    usropn rename(fnorm000:fnormtmp)
001100030929     F                                     prefix(t_)
001200030926     FFNORF00F  O    E             DISK    usropn
001300030926     FFNVAP00F  O    E             DISK    usropn
001400030926     FTIORI00F  O    E             DISK    usropn
001500030811     Ftitv65p   O    f  132        PRINTER usropn
001600000621     F                                     oflind(*inoa)
001700030811     Ftitv65ps  O    f  198        PRINTER usropn
001800000621     F                                     oflind(*inob)
001900000313     D*
002000000613     D*------------
002100000613     D* COMANDI
002200000613     D*------------
002300011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002400000724     D*------------
002500030811     D* DS ESTERNE
002600030811     D*------------
002700030811     D kpjba         e ds                  inz
002800030811     D dscmz         e ds                  inz
002900030811     D tisi95ds      e ds                  inz
003000030811     D trul33ds      e ds                  inz
003100030811     D trul13ds      e ds                  inz
003200030811     D dfar          e ds                  inz
003300030811     D dorm01        e ds                  inz
003400030811     D dnum          e ds                  inz
003500990910     D psds           sds
003600990910     D  procname         *PROC
003700030811     D*------------
003800030811     D* DS INTERNE
003900030811     D*------------
004000941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
004100941229     D* 10+3
004200941229     D                 DS
004300000313     D  WI15                   1     10
004400000313     D  WD15                  11     13
004500000313     D  WASC15                 1     13
004600941229     D*  6+1
004700941229     D                 DS
004800000313     D  WI09                   1      6
004900000313     D  WD09                   7      7
005000000313     D  WASC09                 1      7
005100941229     D*  2+3
005200941229     D                 DS
005300000313     D  WI07                   1      2
005400000313     D  WD07                   3      5
005500000313     D  WASC07                 1      5
005600000313     D*
005700990920     D tivlrds       e ds                  extname(tivlr00f)
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
013100030811     D Ktbcod          s                   LIKE(tbeCOD)
013200030811     D Ktbke1          s                   LIKE(tbeKE1)
013300030915     D flgORM          s              1    INZ('N')
013400000313     D*
013500010606     D*------------------
013600010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
013700010606     D*------------------
013800010606     D WLBDA8          DS                  INZ
013900010606     D  G08DAT                 1      8  0
014000010606     D  G08INV                 9     16  0
014100010606     D  G08ERR                17     17
014200010606     D  G08TGI                18     22  0
014300030811     D*------------------
014400030811     D* DS RIDEFINIZIONE CHIAVE BOLLA ALFA
014500030811     D*------------------
014600030811     D                 DS                  INZ
014700030811     D  vabAAS                 1      4  0
014800030811     D  vabLNP                 5      7  0
014900030811     D  vabNRS                 8      9  0
015000030811     D  vabNSP                10     16  0
015100030811     D  vabKEY                 1     16
015200010606     D*
015300010606
015400010606
015500010606
015600010606
015700990915     C                   time                    wrkdata
015800990915     C                   time                    wrkora
015900000913     C                   reset                   rrnum
016000990921     C                   reset                   esito
016100990921     C                   reset                   wrkesito
016200030926     C*
016300030926     C* AGGIUNGO IN TESTA LA LIBRERIA "LOTTOMATIC"
016400030929     C                   Z-ADD     100           LENGH            15 5
016500030926     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016600030926     C                   PARM                    CMD(4)
016700030926     C                   PARM                    LENGH
016800030926     C*
016900030926     C* ESEGUO APERTURE FILE "ORM"
017000030926     C                   open      FNORM00F
017100030926     C                   open      FNORF00F
017200030926     C                   open      FNVAP00F
017300030926     C                   open      TIORI00F
017400030929     C                   open      FNORMTMPL
017500010601     C*
017600010601     C                   exsr      opeini
017700010601     C                   exsr      rwvab
017800030811     C                   exsr      exefin
017900030926     C*
018000030926     C* ESEGUO CHIUSURE FILE "ORM"
018100030926     C                   close     FNORM00F
018200030926     C                   close     FNORF00F
018300030926     C                   close     FNVAP00F
018400030926     C                   close     TIORI00F
018500030929     C                   close     FNORMTMPL
018600030926     C*
018700030926     C* RIMUOVO DALLA *LIBL LA LIBRERIA "LOTTOMATIC"
018800030929     C                   Z-ADD     100           LENGH            15 5
018900030926     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019000030926     C                   PARM                    CMD(5)
019100030926     C                   PARM                    LENGH
019200010601     C*
019300010601     C                   seton                                        lr
019400010601
019500010601
019600010601
019700010601
019800010601     C*--------------------------------------------------------
019900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
020000010601     C*--------------------------------------------------------
020100010601     C     PREELA        BEGSR
020200010601     C*
020300010601     C* SE OCCORRE SPEDIRE IN FILIALE
020400010601     C                   if        invfil <> *zeros and
020500010601     C                             flgGiro = '0'
020600010601     C*
020700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
020800010601     C                   eval      flgGiro = '1'
020900000724     C*
021000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
021100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
021200010601     C     invfil        chain     azorg01l
021300000724     C                   if        %found
021400000724     C                   movel(p)  CMD(1)        depcmd
021500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
021600000724     C*
021700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
021800011123     C                   Z-ADD     150           LENGH            15 5
021900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
022000000724     C                   PARM                    depcmd
022100000724     C                   PARM                    LENGH
022200000724     C*
022300000724     C                   endif
022400000616     C*
022500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
022600000616     C                   movel(p)  CMD(2)        depcmd
022700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
022800000616     C*
022900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
023000011123     C                   Z-ADD     150           LENGH            15 5
023100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023200000616     C                   PARM                    depcmd
023300000616     C                   PARM                    LENGH
023400000613     C*
023500030811     C                   if        not %open(titv65ps)
023600030811     C                   open      titv65ps
023700030407     C   50              except    testdett
023800000613     C                   endif
023900010601     C*
024000010601     C                   endif
024100010601     C*
024200010601     C                   ENDSR
024300010601     C***
024400010601
024500010601
024600010601
024700010601
024800010601
024900010601
025000010601     C*--------------------------------------------------------
025100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
025200010601     C*--------------------------------------------------------
025300010601     C     ENDELA        BEGSR
025400990915     C*
025500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
025600000613     C*
025700030811     C                   if        %open(titv65ps)
025800030407     C   50              except    findett
025900030811     C                   close     titv65ps
026000000613     C                   endif
026100000616     C*
026200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
026300011123     C                   Z-ADD     150           LENGH            15 5
026400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
026500000616     C                   PARM                    CMD(3)
026600000616     C                   PARM                    LENGH
026700010601     C*
026800010601     C                   ENDSR
026900010601     C***
027000010601
027100010601
027200000613
027300000613
027400000613
027500000613     C*--------------------------------------------------------
027600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
027700000613     C*--------------------------------------------------------
027800000621     C     STPR          BEGSR
027900000613     C*
028000030811     C                   if        not %open(titv65p)
028100030811     C                   open      titv65p
028200990915     C                   endif
028300990915     C*
028400030407     C   50              except    riepilogo
028500990915     C*
028600030811     C                   if        %open(titv65p)
028700030811     C                   close     titv65p
028800990914     C                   endif
028900000613     C*
029000000613     C                   ENDSR
029100000613     C***
029200000613
029300010601
029400010601
029500010601
029600010601
029700910830     C*--------------------------------------------------------
029800020916     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
029900910830     C*--------------------------------------------------------
030000941229     C     RWVAB         BEGSR
030100990910     C*
030200990914     C                   if        not %open(tivin00r)
030300990908     C                   open      tivin00r
030400990914     C                   endif
030500020916     C                   if        not %open(fivabwwr)
030600020916     C                   open      fivabwwr
030700990914     C                   endif
030800990910     C*
030900990910     C                   clear                   §CTROK
031000990910     C                   clear                   §CTRMO
031100990910     C                   clear                   §CTRNO
031200990910     C*
031300921023     C                   DO        *HIVAL
031400990913     C*
031500990915     C                   READ      tivin00r                               70
031600010618     C*
031700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
031800010618     C                   if        vinflg = '1'
031900010618     C                   eval      flgOk = '1'
032000010618     C                   endif
032100010618     C*
032200000905     C                   if        vindta > *blanks
032300000613     C                   add       1             rrnum
032400990913     C*
032500010601     C                   if        *in70 = *off and
032600990913     C                             (vinflg = *blanks
032700010711     C                              or vinflg = '0'
032800010711     C                              or vinflg = '2')
032900990913     C*
033000020916     C                   clear                   FIVAB000
033100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
033200010711     C                   if        vinflg = *blanks or vinflg = '0'
033300010711     C                   clear                   vinmsg
033400010711     C                   endif
033500010711     C*
033600000313     C                   eval      x_rec  = vindta
033700991022     C*
033800991022     C                   testb     x'3F'         vhbatb                   02
033900991022     C                   if        not *in02
034000991022     C*
034100971021     C                   SETOFF                                       303132
034200971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
034300971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
034400971023     C*    OPPURE TUTTI BLANKS
034500010601     C*
034600010601     C                   TESTN                   VHBLNP               30
034700010601     C  N30              TESTN                   VHBLNP                 30
034800010601     C  N30              TESTN                   VHBLNP                   30
034900010601     C                   MOVE      VHBLNP        W001A             1
035000010601     C     *IN30         IFEQ      *OFF
035100010601     C     W001A         ORLT      '0'
035200010601     C                   SETON                                        31
035300010601     C                   eval      vinmsg = %trimr(vinmsg)
035400010601     C                             + ' ' + 'VHBLNP'
035500010601     C                   ELSE
035600010601     C                   MOVE      VHBLNP        VABLNP
035700010601     C     VABLNP        IFLE      *ZEROS
035800010601     C                   SETON                                        31
035900010601     C                   eval      vinmsg = %trimr(vinmsg)
036000010601     C                             + ' ' + 'VHBLNP'
036100010601     C                   ENDIF
036200010601     C                   ENDIF
036300010601     C*
036400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
036500010601     C                   eval      depfil = VABLNP
036600010601     C                   exsr      repfil
036700010601     C                   if        depfil = invfil
036800021014     C                   if        vlrpoi = 999
036900021010     C                   MOVE(P)   invfil        VABFGS
037000021014     C                   else
037100021014     C                   MOVE(P)   vlrpoi        VABFGS
037200021014     C                   endif
037300010601     C*
037400010601     C                   exsr      PREELA
037500010601     C*
037600971023     C                   TESTN                   VHBCCM               30
037700971023     C  N30              TESTN                   VHBCCM                 30
037800971023     C  N30              TESTN                   VHBCCM                   30
037900971021     C                   MOVE      VHBCCM        W001A             1
038000971021     C     *IN30         IFEQ      *OFF
038100971021     C     W001A         ORLT      '0'
038200971021     C                   SETON                                        31
038300010604     C                   eval      vinmsg = %trimr(vinmsg)
038400010604     C                             + ' ' + 'VHBCCM'
038500971021     C                   ELSE
038600971021     C                   MOVE      VHBCCM        VABCCM
038700971024     C     VABCCM        IFLE      *ZEROS
038800971024     C                   SETON                                        31
038900010604     C                   eval      vinmsg = %trimr(vinmsg)
039000010604     C                             + ' ' + 'VHBCCM'
039100971024     C                   ENDIF
039200971021     C                   ENDIF
039300971021     C*
039400971023     C                   TESTN                   VHBAAS               30
039500971023     C  N30              TESTN                   VHBAAS                 30
039600971023     C  N30              TESTN                   VHBAAS                   30
039700971021     C                   MOVE      VHBAAS        W001A             1
039800971021     C     *IN30         IFEQ      *OFF
039900971021     C     W001A         ORLT      '0'
040000971023     C     W001A         ANDNE     *BLANK
040100971021     C                   SETON                                        32
040200010606     C                   MOVEL     datcor        VABAAS
040300010604     C                   eval      vinmsg = %trimr(vinmsg)
040400010604     C                             + ' ' + 'VHBAAS'
040500971021     C                   ELSE
040600971021     C                   MOVE      VHBAAS        VABAAS
040700971021     C                   ENDIF
040800971021     C*
040900971023     C                   TESTN                   VHBMGS               30
041000971023     C  N30              TESTN                   VHBMGS                 30
041100971023     C  N30              TESTN                   VHBMGS                   30
041200971021     C                   MOVE      VHBMGS        W001A             1
041300971021     C     *IN30         IFEQ      *OFF
041400971021     C     W001A         ORLT      '0'
041500971023     C     W001A         ANDNE     *BLANK
041600971021     C                   SETON                                        32
041700010601     C                   MOVE      '0101'        VABMGS
041800010604     C                   eval      vinmsg = %trimr(vinmsg)
041900010604     C                             + ' ' + 'VHBMGS'
042000971021     C                   ELSE
042100971021     C                   MOVE      VHBMGS        VABMGS
042200971021     C                   ENDIF
042300971021     C*
042400971023     C                   TESTN                   VHBNRS               30
042500971023     C  N30              TESTN                   VHBNRS                 30
042600971023     C  N30              TESTN                   VHBNRS                   30
042700971021     C                   MOVE      VHBNRS        W001A             1
042800971021     C     *IN30         IFEQ      *OFF
042900971021     C     W001A         ORLT      '0'
043000971023     C     W001A         ANDNE     *BLANK
043100971021     C                   SETON                                        32
043200971021     C                   MOVE      *ZERO         VABNRS
043300010604     C                   eval      vinmsg = %trimr(vinmsg)
043400010604     C                             + ' ' + 'VHBNRS'
043500971021     C                   ELSE
043600971021     C                   MOVE      VHBNRS        VABNRS
043700971021     C                   ENDIF
043800971021     C*
043900971023     C                   TESTN                   VHBNSP               30
044000971023     C  N30              TESTN                   VHBNSP                 30
044100971023     C  N30              TESTN                   VHBNSP                   30
044200971021     C                   MOVE      VHBNSP        W001A             1
044300971021     C     *IN30         IFEQ      *OFF
044400971021     C     W001A         ORLT      '0'
044500971023     C     W001A         ANDNE     *BLANK
044600971021     C                   SETON                                        32
044700971021     C                   MOVE      *ZERO         VABNSP
044800010604     C                   eval      vinmsg = %trimr(vinmsg)
044900010604     C                             + ' ' + 'VHBNSP'
045000971021     C                   ELSE
045100971021     C                   MOVE      VHBNSP        VABNSP
045200971021     C                   ENDIF
045300971021     C*
045400971023     C                   TESTN                   VHBLNA               30
045500971023     C  N30              TESTN                   VHBLNA                 30
045600971023     C  N30              TESTN                   VHBLNA                   30
045700971021     C                   MOVE      VHBLNA        W001A             1
045800971021     C     *IN30         IFEQ      *OFF
045900971021     C     W001A         ORLT      '0'
046000971023     C     W001A         ANDNE     *BLANK
046100971021     C                   SETON                                        32
046200971021     C                   MOVE      *ZERO         VABLNA
046300010604     C                   eval      vinmsg = %trimr(vinmsg)
046400010604     C                             + ' ' + 'VHBLNA'
046500971021     C                   ELSE
046600971021     C                   MOVE      VHBLNA        VABLNA
046700971021     C                   ENDIF
046800971021     C*
046900971023     C                   TESTN                   VHBCTR               30
047000971023     C  N30              TESTN                   VHBCTR                 30
047100971023     C  N30              TESTN                   VHBCTR                   30
047200971021     C                   MOVE      VHBCTR        W001A             1
047300971021     C     *IN30         IFEQ      *OFF
047400971021     C     W001A         ORLT      '0'
047500971023     C     W001A         ANDNE     *BLANK
047600971021     C                   SETON                                        32
047700971021     C                   MOVE      *ZERO         VABCTR
047800010604     C                   eval      vinmsg = %trimr(vinmsg)
047900010604     C                             + ' ' + 'VHBCTR'
048000971021     C                   ELSE
048100971021     C                   MOVE      VHBCTR        VABCTR
048200971021     C                   ENDIF
048300941229     C*
048400971021     C                   MOVE      *ZERO         VABIAS
048500941229     C                   MOVE      VHBIAS        WASC15
048600971023     C                   TESTN                   WI15                 30
048700971023     C  N30              TESTN                   WI15                   30
048800971023     C  N30              TESTN                   WI15                     30
048900971021     C                   MOVE      WI15          W001A             1
049000971021     C     *IN30         IFEQ      *OFF
049100971021     C     W001A         ORLT      '0'
049200971023     C     W001A         ANDNE     *BLANK
049300971021     C                   SETON                                        32
049400971021     C                   MOVE      *ZERO         VABIAS
049500010604     C                   eval      vinmsg = %trimr(vinmsg)
049600010604     C                             + ' ' + 'VHBIAS'
049700971021     C                   ELSE
049800971021     C                   MOVEL     WI15          VABIAS
049900971021     C                   ENDIF
050000971023     C                   TESTN                   WD15                 30
050100971023     C  N30              TESTN                   WD15                   30
050200971023     C  N30              TESTN                   WD15                     30
050300971021     C                   MOVE      WD15          W001A             1
050400971021     C     *IN30         IFEQ      *OFF
050500971021     C     W001A         ORLT      '0'
050600971023     C     W001A         ANDNE     *BLANK
050700971021     C                   SETON                                        32
050800971021     C                   MOVE      *ZERO         VABIAS
050900010604     C                   eval      vinmsg = %trimr(vinmsg)
051000010604     C                             + ' ' + 'VHBIAS'
051100971021     C                   ELSE
051200971021     C                   MOVE      WD15          VABIAS
051300971021     C                   ENDIF
051400941229     C*
051500971023     C                   TESTN                   VHBNCL               30
051600971023     C  N30              TESTN                   VHBNCL                 30
051700971023     C  N30              TESTN                   VHBNCL                   30
051800971021     C                   MOVE      VHBNCL        W001A             1
051900971021     C     *IN30         IFEQ      *OFF
052000971021     C     W001A         ORLT      '0'
052100971021     C                   SETON                                        32
052200971021     C                   MOVE      *ZERO         VABNCL
052300010604     C                   eval      vinmsg = %trimr(vinmsg)
052400010604     C                             + ' ' + 'VHBNCL'
052500971021     C                   ELSE
052600971021     C                   MOVE      VHBNCL        VABNCL
052700971021     C                   ENDIF
052800941229     C*
052900971021     C                   MOVE      *ZERO         VABPKB
053000971021     C                   MOVE      VHBPKB        WASC09
053100971023     C                   TESTN                   WI09                 30
053200971023     C  N30              TESTN                   WI09                   30
053300971023     C  N30              TESTN                   WI09                     30
053400971021     C                   MOVE      WI09          W001A             1
053500971021     C     *IN30         IFEQ      *OFF
053600971021     C     W001A         ORLT      '0'
053700971021     C                   SETON                                        32
053800971021     C                   MOVE      *ZERO         VABPKB
053900010604     C                   eval      vinmsg = %trimr(vinmsg)
054000010604     C                             + ' ' + 'VHBPKB'
054100971021     C                   ELSE
054200971021     C                   MOVEL     WI09          VABPKB
054300971021     C                   ENDIF
054400971023     C                   TESTN                   WD09                 30
054500971023     C  N30              TESTN                   WD09                     30
054600971021     C                   MOVE      WD09          W001A             1
054700971021     C     *IN30         IFEQ      *OFF
054800971021     C     W001A         ORLT      '0'
054900971023     C     W001A         ANDNE     *BLANK
055000971021     C                   SETON                                        32
055100971021     C                   MOVE      *ZERO         VABPKB
055200010604     C                   eval      vinmsg = %trimr(vinmsg)
055300010604     C                             + ' ' + 'VHBPKB'
055400971021     C                   ELSE
055500971021     C                   MOVE      WD09          VABPKB
055600971021     C                   ENDIF
055700971021     C*
055800971021     C                   MOVE      *ZERO         VABVLB
055900971021     C                   MOVE      VHBVLB        WASC07
056000971023     C                   TESTN                   WI07                 30
056100971023     C  N30              TESTN                   WI07                   30
056200971023     C  N30              TESTN                   WI07                     30
056300971021     C                   MOVE      WI07          W001A             1
056400971021     C     *IN30         IFEQ      *OFF
056500971021     C     W001A         ORLT      '0'
056600971023     C     W001A         ANDNE     *BLANK
056700971021     C                   SETON                                        32
056800971021     C                   MOVE      *ZERO         VABVLB
056900010604     C                   eval      vinmsg = %trimr(vinmsg)
057000010604     C                             + ' ' + 'VHBVLB'
057100971021     C                   ELSE
057200971021     C                   MOVEL     WI07          VABVLB
057300971021     C                   ENDIF
057400971023     C                   TESTN                   WD07                 30
057500971023     C  N30              TESTN                   WD07                   30
057600971023     C  N30              TESTN                   WD07                     30
057700971021     C                   MOVE      WD07          W001A             1
057800971021     C     *IN30         IFEQ      *OFF
057900971021     C     W001A         ORLT      '0'
058000971023     C     W001A         ANDNE     *BLANK
058100971021     C                   SETON                                        32
058200971021     C                   MOVE      *ZERO         VABVLB
058300010604     C                   eval      vinmsg = %trimr(vinmsg)
058400010604     C                             + ' ' + 'VHBVLB'
058500971021     C                   ELSE
058600971021     C                   MOVE      WD07          VABVLB
058700971021     C                   ENDIF
058800941229     C*
058900971021     C                   MOVE      *ZERO         VABQFT
059000971021     C                   MOVE      VHBQFT        WASC15
059100971023     C                   TESTN                   WI15                 30
059200971023     C  N30              TESTN                   WI15                   30
059300971023     C  N30              TESTN                   WI15                     30
059400971021     C                   MOVE      WI15          W001A             1
059500971021     C     *IN30         IFEQ      *OFF
059600971021     C     W001A         ORLT      '0'
059700971023     C     W001A         ANDNE     *BLANK
059800971021     C                   SETON                                        32
059900971021     C                   MOVE      *ZERO         VABQFT
060000010604     C                   eval      vinmsg = %trimr(vinmsg)
060100010604     C                             + ' ' + 'VHBQFT'
060200971021     C                   ELSE
060300971021     C                   MOVEL     WI15          VABQFT
060400971021     C                   ENDIF
060500971023     C                   TESTN                   WD15                 30
060600971023     C  N30              TESTN                   WD15                   30
060700971023     C  N30              TESTN                   WD15                     30
060800971021     C                   MOVE      WD15          W001A             1
060900971021     C     *IN30         IFEQ      *OFF
061000971021     C     W001A         ORLT      '0'
061100971023     C     W001A         ANDNE     *BLANK
061200971021     C                   SETON                                        32
061300971021     C                   MOVE      *ZERO         VABQFT
061400010604     C                   eval      vinmsg = %trimr(vinmsg)
061500010604     C                             + ' ' + 'VHBQFT'
061600971021     C                   ELSE
061700971021     C                   MOVE      WD15          VABQFT
061800971021     C                   ENDIF
061900941229     C*
062000971021     C                   MOVE      *ZERO         VABCAS
062100971021     C                   MOVE      VHBCAS        WASC15
062200971023     C                   TESTN                   WI15                 30
062300971023     C  N30              TESTN                   WI15                   30
062400971023     C  N30              TESTN                   WI15                     30
062500971021     C                   MOVE      WI15          W001A             1
062600971021     C     *IN30         IFEQ      *OFF
062700971021     C     W001A         ORLT      '0'
062800971023     C     W001A         ANDNE     *BLANK
062900971021     C                   SETON                                        32
063000971021     C                   MOVE      *ZERO         VABCAS
063100010604     C                   eval      vinmsg = %trimr(vinmsg)
063200010604     C                             + ' ' + 'VHBCAS'
063300971021     C                   ELSE
063400971021     C                   MOVEL     WI15          VABCAS
063500971021     C                   ENDIF
063600971023     C                   TESTN                   WD15                 30
063700971023     C  N30              TESTN                   WD15                   30
063800971023     C  N30              TESTN                   WD15                     30
063900971021     C                   MOVE      WD15          W001A             1
064000971021     C     *IN30         IFEQ      *OFF
064100971021     C     W001A         ORLT      '0'
064200971023     C     W001A         ANDNE     *BLANK
064300971021     C                   SETON                                        32
064400971021     C                   MOVE      *ZERO         VABCAS
064500010604     C                   eval      vinmsg = %trimr(vinmsg)
064600010604     C                             + ' ' + 'VHBCAS'
064700971021     C                   ELSE
064800971021     C                   MOVE      WD15          VABCAS
064900971021     C                   ENDIF
065000971021     C*
065100971023     C                   TESTN                   VHBRMN               30
065200971023     C  N30              TESTN                   VHBRMN                 30
065300971023     C  N30              TESTN                   VHBRMN                   30
065400971021     C                   MOVE      VHBRMN        W001A             1
065500971021     C     *IN30         IFEQ      *OFF
065600971021     C     W001A         ORLT      '0'
065700971021     C                   SETON                                        32
065800971021     C                   MOVE      *ZERO         VABRMN
065900010604     C                   eval      vinmsg = %trimr(vinmsg)
066000010604     C                             + ' ' + 'VHBRMN'
066100971021     C                   ELSE
066200971021     C                   MOVE      VHBRMN        VABRMN
066300971021     C                   ENDIF
066400971021     C*
066500971023     C                   TESTN                   VHBNCD               30
066600971023     C  N30              TESTN                   VHBNCD                 30
066700971023     C  N30              TESTN                   VHBNCD                   30
066800971021     C                   MOVE      VHBNCD        W001A             1
066900971021     C     *IN30         IFEQ      *OFF
067000971021     C     W001A         ORLT      '0'
067100971023     C     W001A         ANDNE     *BLANK
067200971021     C                   SETON                                        32
067300971021     C                   MOVE      *ZERO         VABNCD
067400010604     C                   eval      vinmsg = %trimr(vinmsg)
067500010604     C                             + ' ' + 'VHBNCD'
067600971021     C                   ELSE
067700971021     C                   MOVE      VHBNCD        VABNCD
067800971021     C                   ENDIF
067900971021     C*
068000971023     C                   TESTN                   VHBNCA               30
068100971023     C  N30              TESTN                   VHBNCA                 30
068200971023     C  N30              TESTN                   VHBNCA                   30
068300971021     C                   MOVE      VHBNCA        W001A             1
068400971021     C     *IN30         IFEQ      *OFF
068500971021     C     W001A         ORLT      '0'
068600971023     C     W001A         ANDNE     *BLANK
068700971021     C                   SETON                                        32
068800971021     C                   MOVE      *ZERO         VABNCA
068900010604     C                   eval      vinmsg = %trimr(vinmsg)
069000010604     C                             + ' ' + 'VHBNCA'
069100971021     C                   ELSE
069200971021     C                   MOVE      VHBNCA        VABNCA
069300971021     C                   ENDIF
069400971021     C*
069500971023     C                   TESTN                   VHBZNC               30
069600971023     C  N30              TESTN                   VHBZNC                 30
069700971023     C  N30              TESTN                   VHBZNC                   30
069800971021     C                   MOVE      VHBZNC        W001A             1
069900971021     C     *IN30         IFEQ      *OFF
070000971021     C     W001A         ORLT      '0'
070100971023     C     W001A         ANDNE     *BLANK
070200971021     C                   SETON                                        32
070300971021     C                   MOVE      *ZERO         VABZNC
070400010604     C                   eval      vinmsg = %trimr(vinmsg)
070500010604     C                             + ' ' + 'VHBZNC'
070600971021     C                   ELSE
070700971021     C                   MOVE      VHBZNC        VABZNC
070800971021     C                   ENDIF
070900971021     C*
071000971023     C                   TESTN                   VHBDCR               30
071100971023     C  N30              TESTN                   VHBDCR                 30
071200971023     C  N30              TESTN                   VHBDCR                   30
071300971021     C                   MOVE      VHBDCR        W001A             1
071400971021     C     *IN30         IFEQ      *OFF
071500971021     C     W001A         ORLT      '0'
071600971023     C     W001A         ANDNE     *BLANK
071700971021     C                   SETON                                        32
071800971021     C                   MOVE      *ZERO         VABDCR
071900010604     C                   eval      vinmsg = %trimr(vinmsg)
072000010604     C                             + ' ' + 'VHBDCR'
072100971021     C                   ELSE
072200971021     C                   MOVE      VHBDCR        VABDCR
072300971021     C                   ENDIF
072400971021     C*
072500971023     C                   TESTN                   VHBHCR               30
072600971023     C  N30              TESTN                   VHBHCR                 30
072700971023     C  N30              TESTN                   VHBHCR                   30
072800971021     C                   MOVE      VHBHCR        W001A             1
072900971021     C     *IN30         IFEQ      *OFF
073000971021     C     W001A         ORLT      '0'
073100971023     C     W001A         ANDNE     *BLANK
073200971021     C                   SETON                                        32
073300971021     C                   MOVE      *ZERO         VABHCR
073400010604     C                   eval      vinmsg = %trimr(vinmsg)
073500010604     C                             + ' ' + 'VHBHCR'
073600971021     C                   ELSE
073700971021     C                   MOVE      VHBHCR        VABHCR
073800971021     C                   ENDIF
073900941229     C*
074000971021     C                   MOVE      *ZERO         VABVMD
074100971021     C                   MOVE      VHBVMD        WASC15
074200971023     C                   TESTN                   WI15                 30
074300971023     C  N30              TESTN                   WI15                   30
074400971023     C  N30              TESTN                   WI15                     30
074500971021     C                   MOVE      WI15          W001A             1
074600971021     C     *IN30         IFEQ      *OFF
074700971021     C     W001A         ORLT      '0'
074800971023     C     W001A         ANDNE     *BLANK
074900971021     C                   SETON                                        32
075000971021     C                   MOVE      *ZERO         VABVMD
075100010604     C                   eval      vinmsg = %trimr(vinmsg)
075200010604     C                             + ' ' + 'VHBVMD'
075300971021     C                   ELSE
075400971021     C                   MOVEL     WI15          VABVMD
075500971021     C                   ENDIF
075600971023     C                   TESTN                   WD15                 30
075700971023     C  N30              TESTN                   WD15                   30
075800971023     C  N30              TESTN                   WD15                     30
075900971021     C                   MOVE      WD15          W001A             1
076000971021     C     *IN30         IFEQ      *OFF
076100971021     C     W001A         ORLT      '0'
076200971023     C     W001A         ANDNE     *BLANK
076300971021     C                   SETON                                        32
076400971021     C                   MOVE      *ZERO         VABVMD
076500010604     C                   eval      vinmsg = %trimr(vinmsg)
076600010604     C                             + ' ' + 'VHBVMD'
076700971021     C                   ELSE
076800971021     C                   MOVE      WD15          VABVMD
076900971021     C                   ENDIF
077000971021     C*
077100971023     C                   TESTN                   VHBANT               30
077200971023     C  N30              TESTN                   VHBANT                 30
077300971023     C  N30              TESTN                   VHBANT                   30
077400971021     C                   MOVE      VHBANT        W001A             1
077500971021     C     *IN30         IFEQ      *OFF
077600971021     C     W001A         ORLT      '0'
077700971023     C     W001A         ANDNE     *BLANK
077800971021     C                   SETON                                        32
077900971021     C                   MOVE      *ZERO         VABANT
078000010604     C                   eval      vinmsg = %trimr(vinmsg)
078100010604     C                             + ' ' + 'VHBANT'
078200971021     C                   ELSE
078300971021     C                   MOVE      VHBANT        VABANT
078400971021     C                   ENDIF
078500971021     C*
078600971021     C* CAMPI ALFABETICI:
078700971021     C                   MOVE      VHBATB        VABATB
078800971021     C                   MOVE      VHBCBO        VABCBO
078900971021     C                   MOVE      VHBRSD        VABRSD
079000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
079100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
079200020117     C* ==
079300971021     C                   MOVE      VHBRD2        VABRD2
079400971021     C                   MOVE      VHBIND        VABIND
079500971021     C                   MOVE      VHBCAD        VABCAD
079600971021     C                   MOVE      VHBLOD        VABLOD
079700971021     C                   MOVE      VHBPRD        VABPRD
079800971021     C                   MOVE      VHBNZD        VABNZD
079900971021     C                   MOVE      VHBGC1        VABGC1
080000971021     C                   MOVE      VHBGC2        VABGC2
080100971021     C                   MOVE      VHBTSP        VABTSP
080200971021     C                   MOVE      VHBVAS        VABVAS
080300971021     C                   MOVE      VHBNAS        VABNAS
080400971021     C                   MOVE      VHBTIC        VABTIC
080500971021     C                   MOVE      VHBVCA        VABVCA
080600971021     C                   MOVE      VHBGCA        VABGCA
080700971021     C                   MOVE      VHBRMA        VABRMA
080800971021     C                   MOVE      VHBXCO        VABXCO
080900971021     C                   MOVE      VHBNOT        VABNOT
081000971021     C                   MOVE      VHBNT2        VABNT2
081100971021     C                   MOVE      VHBCTM        VABCTM
081200971021     C                   MOVE      VHBFFD        VABFFD
081300971021     C                   MOVE      VHBTCR        VABTCR
081400971021     C                   MOVE      VHBCTS        VABCTS
081500971021     C                   MOVE      VHBFTM        VABFTM
081600971021     C                   MOVE      VHBVAD        VABVAD
081700030929     C                   MOVE      VHBGMA        VABGMA
081800971021     C                   MOVE      VHBGGA        VABGGA
081900971021     C                   MOVE      VHBGVA        VABGVA
082000971021     C                   MOVE      VHBTC1        VABTC1
082100971021     C                   MOVE      VHBTC2        VABTC2
082200971021     C                   MOVE      VHBSCL        VABSCL
082300971021     C                   MOVE      VHBRMO        VABRMO
082400971021     C                   MOVE      VHBCMO        VABCMO
082500971021     C                   MOVE      VHBNMO        VABNMO
082600971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
082700971021     C* NELLA NAZIONE ORIGINALE
082800971021     C                   MOVE      VHBNMO        WA1D              1
082900971021     C                   MOVEL     VHBNMO        WA1S              1
083000971021     C     WA1D          IFNE      *BLANK
083100971021     C     WA1S          ANDEQ     *BLANK
083200971021     C                   MOVE      *BLANK        VABNMO
083300971021     C                   END
083400930409     C*
083500010604     C  N31              ADD       1             §CTROK            7 0
083600010604     C   32              ADD       1             §CTRMO            7 0
083700010604     C   31              ADD       1             §CTRNO            7 0
083800030929     C*
083900030929     C* Se la bolla già presenta il codice particolarità consegna 'RC' NON FACCIO NULLA
084000030929     C                   IF        VABGMA = *blanks
084100030929     C                   EVAL      VABGMA = 'RC'
084200030929     C                   SETON                                        37
084300030929     C                   ELSE
084400030929     C                   SETOFF                                       37
084500030929     C                   ENDIF
084600030929     C*
084700030929     C                   IF        *IN37 = *ON
084800030929     C                   EXSR      CHKORMTAS
084900030929     C                   ENDIF
085000030929     C                   IF        *IN37 = *ON
085100030929     C                   IF        *IN38 = *ON
085200030929     C                   EVAL      VABGMA = *blanks
085300030929     C                   ENDIF
085400020916     C  N31              WRITE     FIVAB000
085500030929     C                   ENDIF
085600030811     C*
085700030811     C* Eseguo operazioni particolari x LOTTOMATICA x gestire ritiro contestuale alla consegna
085800030929     C                   if        *IN31 = *OFF AND *IN37 = *ON AND *IN38 = *OFF
085900030929     C                                     AND vabGMA = 'RC'
086000030915     C                   eval      flgORM = 'S'
086100030811     C                   exsr      wriORM
086200030811     C                   exsr      wriORF
086300030811     C                   exsr      wriORI
086400030811     C                   exsr      wriVAP
086500030811     C                   endif
086600990910     C*
086700010601     C                   if        *in31 = *off and
086800010601     C                             *in32 = *off
086900990910     C                   eval      vinflg = '1'
087000990910     C                   else
087100000621     C                   eval      recko = vindta
087200030811     C                   if        %open(titv65ps)
087300030407     C   50              except    rigadett
087400010612     C                   endif
087500990910     C                   eval      vinflg = '2'
087600990910     C                   endif
087700000905     C                   endif
087800000905     C                   endif
087900010601     C*
088000010601     C                   endif
088100000905     C*
088200000905     C                   else
088300000905     C                   eval      vinflg = '1'
088400000905     C                   endif
088500000905     C*
088600000905     C  N70              update    tivin000
088700000905     C*
088800991022     C  N70              ENDdo
088900010601     C
089000010601     C*
089100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
089200010601     C                   if        cntNonEl = *zeros or
089300010601     C                             flgMulti = '0'
089400990910     C* Se non ci sono record con errori ...
089500010601     C                   if        §ctrno = 0 and
089600010601     C                             §ctrmo = 0 and
089700010601     C                             flgStato <> '2'
089800990910     C* ... restituisco esito OK.
089900990921     C                   eval      wrkesito = '0'
090000990910     C                   else
090100010601     C                   if        §ctrok > 0
090200000710     C                   eval      wrkesito = '1'
090300000710     C                   else
090400010615     C                   if        flgOk = '0'
090500010615     C                   eval      wrkesito = '2'
090600010615     C                   else
090700010615     C                   eval      wrkesito = '6'
090800010615     C                   endif
090900000710     C                   endif
091000010615     C                   endif
091100010601     C                   else
091200010601     C                   eval      wrkesito = '9'
091300010601     C                   endif
091400990910     C*
091500990914     C                   if        %open(tivin00r)
091600990908     C                   close     tivin00r
091700990914     C                   endif
091800020916     C                   if        %open(fivabwwr)
091900020916     C                   close     fivabwwr
092000990914     C                   endif
092100010601     C*
092200010601     C                   if        vlrpoi <> 999
092300010601     C                   eval      invfil = vlrpoi
092400010601     C                   endif
092500990910     C*
092600990920     C                   if        §ctrok > 0
092700010601     C                             and invfil > *zeros
092800990920     C                   exsr      invio
092900030915     C                   if        flgORM = 'S'
093000030915     C                   exsr      invioORM
093100030915     C                   endif
093200990920     C                   endif
093300010612     C*
093400010612     C                   if        flgGiro = '1'
093500010612     C                   exsr      endela
093600010612     C                   endif
093700990920     C*
093800910830     C                   ENDSR
093900000613     C***
094000030929
094100030929
094200030929
094300030929
094400030929      /TITLE Verifica esistenza ORM corrente già in wrk-file-tmp FNORMTMPL
094500030929     C     CHKORMTAS     BEGSR
094600030929     C*
094700030929     C     KEYormtmpl    chain     fnormtmpl
094800030929     C                   if        %found(fnormtmpl)
094900030929     C                   eval      t_ormatb = '*'
095000030929     C                   update    fnormtmp
095100030929     C                   seton                                        38
095200030929     C                   else
095300030929     C                   setoff                                       38
095400030929     C                   endif
095500030929     C*
095600030929     C                   ENDSR
095700030929     C***
095800030811
095900030811
096000030811
096100030811      /TITLE Scrittura record in FNORM00F
096200030811     C     wriORM        BEGSR
096300030811     C*
096400030811     C                   clear                   FNORM000
096500030811     C* Valorizzo buffer record
096600030811     C                   eval      ormATB = *blanks
096700030811     C                   eval      ormPOE = 001
096800030811     C                   eval      ormNSR = 70
096900030926     C* Stacco un numeratore da AZNUM
097000030811     C                   clear                   TRUL33DS
097100030811     C                   eval      I33OPE = *zeros
097200030811     C                   eval      I33PO1 = 046
097300030811     C                   eval      I33CNU = 300
097400030811     C                   eval      I33NUM = 1
097500030811     C                   movel     TRUL33DS      KPJBU
097600030811     C                   call      'TRUL33R'
097700030811     C                   parm                    KPJBA
097800030811     C                   movel     KPJBU         TRUL33DS
097900030811     C                   if        O33ERR = *zeros
098000030811     C                   eval      ormNOR = O33NRF
098100030811     C                   else
098200030811     C                   eval      ormNOR = *all'9'
098300030811     C                   endif
098400030811     C*
098500030811     C                   eval      ormNRV = *zeros
098600030811     C                   eval      ormTOR = 'S'
098700030811     C                   eval      ormTCO = 'F'
098800030811     C                   eval      ormDAO = datcor
098900030811     C                   eval      ormOAO = oracor
099000030926     C                   eval      ormCOR = vabCCM*1000+333
099100030811     C* Reperisco i dati anagrafici (dall'anagrafica ritiri) del codice ORDINANTE
099200030926     C     ormCOR        chain     fnacr01l
099300030811     C                   if        %found(fnacr01l)
099400030811     C                   eval      ormRSO = acrRSR
099500030811     C                   eval      ormINO = acrINR
099600030811     C                   eval      ormCAO = acrCAR
099700030811     C                   eval      ormLOO = acrLOR
099800030811     C                   eval      ormPRO = acrPRR
099900030811     C                   eval      ormNAO = acrNAR
100000030811     C                   eval      ormNAM = acrNTM
100100030811     C                   else
100200030811     C                   eval      ormRSO = *all'*'
100300030811     C                   eval      ormINO = *all'*'
100400030811     C                   eval      ormCAO = *all'0'
100500030811     C                   eval      ormLOO = *all'*'
100600030811     C                   eval      ormPRO = *all'*'
100700030811     C                   eval      ormNAO = *all'*'
100800030811     C                   eval      ormNAM = *all'*'
100900030811     C                   endif
101000030811     C                   eval      ormCRA = *zeros
101100030811     C                   eval      ormRSR = vabRSD
101200030811     C                   eval      ormINR = vabIND
101300030811     C                   eval      ormCAR = vabCAD
101400030811     C                   eval      ormLOR = vabLOD
101500030811     C                   eval      ormPRR = vabPRD
101600030811     C                   eval      ormNAR = vabNZD
101700030926     C                   eval      ormRER = '.'
101800030926     C                   eval      ormTER = '.'
101900030926     C                   eval      ormNO1 = 'RITIRO SCHEDINE ERRATE '
102000030926     C                   eval      ormNO2 = 'TOTOGOL CONCORSO N°8 '
102100030926     C                   eval      ormCRC = 0011947001
102200030811     C* Reperisco i dati anagrafici (dall'anagrafica ritiri) del codice CONSEGNA
102300030926     C     ormCRC        chain     fnacr01l
102400030811     C                   if        %found(fnacr01l)
102500030811     C                   eval      ormRSC = acrRSR
102600030811     C                   eval      ormINC = acrINR
102700030811     C                   eval      ormCAC = acrCAR
102800030811     C                   eval      ormLOC = acrLOR
102900030811     C                   eval      ormPRC = acrPRR
103000030811     C                   eval      ormNAC = acrNAR
103100030811     C                   else
103200030811     C                   eval      ormRSC = *all'*'
103300030811     C                   eval      ormINC = *all'*'
103400030811     C                   eval      ormCAC = *all'0'
103500030811     C                   eval      ormLOC = *all'*'
103600030811     C                   eval      ormPRC = *all'*'
103700030811     C                   eval      ormNAC = *all'*'
103800030811     C                   endif
103900030811     C                   eval      ormFFD = 'N'
104000030811     C* Calcolo la data ritiro richiesta aggiungendo alla data aperura ORM 1 gg. lavorativo
104100030811     C* (tramite TRUL13R)
104200030926     C                   z-add     1100          wOra              4 0
104300030811     C                   clear                   TRUL13DS
104400030811     C                   eval      I13MOD = 'P'
104500030811     C                   eval      I13FIL = ormPOE
104600030811     C                   eval      I13DIN = ormDAO
104700030811     C                   eval      I13HIN = wOra
104800030811     C                   eval      I13GIO = 1
104900030811     C                   eval      I13ORE = *zeros
105000030811     C                   call      'TRUL13R'
105100030811     C                   parm                    TRUL13DS
105200030811     C                   if        O13ERR = *blanks
105300030811     C                   eval      ormDAR = O13DFI
105400030811     C                   else
105500030811     C                   eval      ormDAR = ormDAO
105600030811     C                   endif
105700030811     C*
105800030811     C                   eval      ormORR = wORA
105900030811     C                   eval      ormRMP = *blanks
106000030811     C                   eval      ormNCL = 1
106100030811     C                   eval      ormPKG = 1
106200030811     C                   eval      ormVLM = *zeros
106300030811     C                   eval      ormBNC = *zeros
106400030811     C                   eval      ormBLC = *zeros
106500030811     C                   eval      ormATT = *zeros
106600030811     C                   eval      ormMTC = *zeros
106700030926     C                   eval      ormPAG = 'D'
106800030926     C                   eval      ormKSC = 0011947
106900030929     C                   eval      ormCTR = *blanks
107000030926     C                   eval      ormPOR = vabLNA
107100030811     C                   movel     vabZNC        ormZOR
107200030811     C* Reperisco P.O. Consegna tramite TISI95R richiamato con i dati instradamento del destinatario
107300030811     C                   clear                   TISI95DS
107400030811     C                   eval      I95TCN = '3'
107500030811     C                   eval      I95NAR = ormNAC
107600030811     C                   eval      I95CAP = ormCAC
107700030811     C                   eval      I95LOC = ormLOC
107800030811     C                   eval      I95PRV = ormPRC
107900030811     C                   eval      I95LKG = ormPKG
108000030811     C                   call      'TISI95R'
108100030811     C                   parm                    TISI95DS
108200030811     C                   if        O95ERR = *blanks
108300030926     C                   eval      ormPOC = O95LNA
108400030811     C                   else
108500030811     C                   eval      ormPOC = *zeros
108600030811     C                   endif
108700030926     C                   eval      ormPOC = 001
108800030811     C*
108900030811     C                   eval      ormDDT = 'N'
109000030811     C                   eval      ormCST = *zeros
109100030811     C                   eval      ormVCS = *blanks
109200030811     C                   eval      ormFCS = *blanks
109300030811     C                   eval      ormDFO = datcor
109400030811     C                   eval      ormOFO = oracor
109500030926     C                   eval      ormFAO = 100
109600030811     C                   eval      ormSTO = *zeros
109700030811     C                   eval      ormDST = datcor
109800030811     C                   eval      ormNPG = *zeros
109900030811     C                   eval      ormNDC = *zeros
110000030811     C                   eval      ormDDC = *zeros
110100030811     C                   eval      ormSTP = *zeros
110200030811     C                   eval      ormPOS = *zeros
110300030811     C                   eval      ormORS = *zeros
110400030926     C                   eval      ormRFA = 'B' + %subst(vabKEY:3:14)
110500030811     C                   eval      ormTAP = *blanks
110600030811     C                   eval      ormETI = *blanks
110700030811     C                   eval      ormSPI = *blanks
110800030811     C* Valorizzo il campo dei FLag Operativi secondo la DS DORM01
110900030811     C                   clear                   DORM01
111000030811     C                   eval      §ormCB = *blanks
111100030811     C                   eval      §ormFR = *blanks
111200030811     C                   eval      §ormKS = 'S'
111300030811     C                   eval      §ormFD = 'S'
111400030811     C                   eval      §ormIC = *blanks
111500030811     C                   eval      §orCOM = *blanks
111600030811     C                   eval      §orPFB = 'A'
111700030811     C                   eval      §ormXX = *blanks
111800030926     C* Stacco un numeratore dalla tabella "NUM"
111900030811     C                   movel(p)  'NUM'         KtbCOD
112000030811     C                   movel(p)  '1'           KtbKE1
112100030811     C     KEYtbe        chain     tntbe01l
112200030811     C                   if        %found(tntbe01l)
112300030811     C                   movel(P)  tbeuni        DNUM
112400030811     C                   eval      §numori = §numori + 1
112500030811     C                   movel     §numori       §ormPG
112600030811     C                   movel     DNUM          tbeuni
112700030811     C                   update    TNTBE000
112800030811     C                   else
112900030811     C                   eval      §ormPG = *all'9'
113000030811     C                   endif
113100030811     C*
113200030811     C                   eval      ormFLO = DORM01
113300030811     C                   eval      ormFLT = *blanks
113400030811     C                   eval      ormPOT = ormPOE
113500030811     C                   eval      ormDTT = *zeros
113600030811     C*
113700030811     C* Scarico il buffer su file
113800030811     C                   write     FNORM000
113900030811     C*
114000030811     C                   ENDSR
114100030811     C***
114200030811
114300030811
114400030811
114500030811      /TITLE Scrittura record in FNORF00F
114600030811     C     wriORF        BEGSR
114700030811     C*
114800030811     C                   clear                   FNORF000
114900030811     C* Valorizzo buffer record
115000030811     C                   eval      orfATB = *blanks
115100030811     C                   eval      orfPOE = ormPOE
115200030811     C                   eval      orfNSR = ormNSR
115300030811     C                   eval      orfNOR = ormNOR
115400030811     C                   eval      orfNRV = ormNRV
115500030811     C                   eval      orfPOG = ormPOE
115600030811     C                   eval      orfDAE = datcor
115700030811     C                   eval      orfORE = oracor
115800030811     C                   eval      orfFAR = ormFAO
115900030811     C                   eval      orfCAR = *blanks
116000030811     C                   eval      orfPUE = 'BATCH'
116100030811     C                   eval      orfFTR = *blanks
116200030811     C                   eval      orfDTR = *zeros
116300030811     C*
116400030811     C* Scarico il buffer su file
116500030811     C                   write     FNORF000
116600030811     C*
116700030811     C                   ENDSR
116800030811     C***
116900030811
117000030811
117100030811
117200030811      /TITLE Scrittura record in TIORI00F
117300030811     C     wriORI        BEGSR
117400030811     C*
117500030811     C                   clear                   TIORI000
117600030811     C* Valorizzo buffer record
117700030811     C                   movel     §ormPG        oriPRG
117800030811     C                   eval      oriDER = datcor
117900030811     C                   eval      oriOER = oracor
118000030926     C                   eval      oriIDC = vlrKSC
118100030811     C                   eval      oriPOE = ormPOE
118200030811     C                   eval      oriNSR = ormNSR
118300030811     C                   eval      oriNOR = ormNOR
118400030811     C                   eval      oriNRV = ormNRV
118500030811     C                   eval      oriFLO = *blanks
118600030811     C                   eval      vaoATB = ormATB
118700030811     C                   eval      vaoPOE = ormPOE
118800030811     C                   eval      vaoNSR = ormNSR
118900030811     C                   eval      vaoNOR = ormNOR
119000030811     C                   eval      vaoNRV = ormNRV
119100030811     C                   eval      vaoTOR = ormTOR
119200030811     C                   eval      vaoTCO = ormTCO
119300030811     C                   eval      vaoDAO = ormDAO
119400030811     C                   eval      vaoOAO = ormOAO
119500030811     C                   eval      vaoCOR = ormCOR
119600030811     C                   eval      vaoRSO = ormRSO
119700030811     C                   eval      vaoINO = ormINO
119800030811     C                   eval      vaoCAO = ormCAO
119900030811     C                   eval      vaoLOO = ormLOO
120000030811     C                   eval      vaoPRO = ormPRO
120100030811     C                   eval      vaoNAO = ormNAO
120200030811     C                   eval      vaoCRA = ormCRA
120300030811     C                   eval      vaoRSR = ormRSR
120400030811     C                   eval      vaoINR = ormINR
120500030811     C                   eval      vaoCAR = ormCAR
120600030811     C                   eval      vaoLOR = ormLOR
120700030811     C                   eval      vaoPRR = ormPRR
120800030811     C                   eval      vaoNAR = ormNAR
120900030811     C                   eval      vaoRER = ormRER
121000030811     C                   eval      vaoTER = ormTER
121100030811     C                   eval      vaoCRC = ormCRC
121200030811     C                   eval      vaoRSC = ormRSC
121300030811     C                   eval      vaoINC = ormINC
121400030811     C                   eval      vaoCAC = ormCAC
121500030811     C                   eval      vaoLOC = ormLOC
121600030811     C                   eval      vaoPRC = ormPRC
121700030811     C                   eval      vaoNAC = ormNAC
121800030811     C                   eval      vaoFFD = ormFFD
121900030811     C                   eval      vaoDAR = ormDAR
122000030811     C                   eval      vaoORR = ormORR
122100030811     C                   eval      vaoRMP = ormRMP
122200030811     C                   eval      vaoNAM = ormNAM
122300030811     C                   eval      vaoNCL = ormNCL
122400030811     C                   eval      vaoPKG = ormPKG
122500030811     C                   eval      vaoVLM = ormVLM
122600030811     C                   eval      vaoBNC = ormBNC
122700030811     C                   eval      vaoBLC = ormBLC
122800030811     C                   eval      vaoATT = ormATT
122900030811     C                   eval      vaoMTC = ormMTC
123000030811     C                   eval      vaoPAG = ormPAG
123100030811     C                   eval      vaoKSC = ormKSC
123200030811     C                   eval      vaoCTR = ormCTR
123300030811     C                   eval      vaoPOR = ormPOR
123400030811     C                   eval      vaoZOR = ormZOR
123500030811     C                   eval      vaoPOC = ormPOC
123600030811     C                   eval      vaoNO1 = ormNO1
123700030811     C                   eval      vaoNO2 = ormNO2
123800030811     C                   eval      vaoDDT = ormDDT
123900030811     C                   eval      vaoRFA = ormRFA
124000030811     C                   eval      vaoSTO = ormSTO
124100030811     C                   eval      vaoSPI = ormSPI
124200030811     C                   eval      vaoFLO = ormFLO
124300030811     C*
124400030811     C* Scarico il buffer su file
124500030811     C                   write     TIORI000
124600030811     C*
124700030811     C                   ENDSR
124800030811     C***
124900030811
125000030811
125100030811
125200030811      /TITLE Scrittura record in FNVAP00F
125300030811     C     wriVAP        BEGSR
125400030811     C*
125500030811     C* Reperisco la descrizione della fase da tabella
125600030926     C                   movel(p)  'FAR'         KtbCOD
125700030926     C                   movel(p)  '000'         KtbKE1
125800030811     C     KEYtbe        chain     tntbe01l
125900030811     C                   if        %found(tntbe01l)
126000030811     C                   movel(P)  tbeuni        DFAR
126100030811     C                   else
126200030811     C                   clear                   DFAR
126300030811     C                   endif
126400030811     C*
126500030811     C* Valorizzo buffer record
126600030811     C                   clear                   FNVAP000
126700030811     C                   eval      vapIDC = oriIDC
126800030811     C                   eval      vapPOE = oriPOE
126900030811     C                   eval      vapRFA = vaoRFA
127000030811     C                   eval      vapPOG = oriPOE
127100030811     C                   eval      vapDAE = oriDER
127200030811     C                   eval      vapORE = oriOER
127300030811     C                   if        d§fardva <> *blanks
127400030811     C                   eval      vapDFA = d§fardva
127500030811     C                   else
127600030811     C                   eval      vapDFA = d§fardes
127700030811     C                   endif
127800030811     C*
127900030811     C* Scarico il buffer su file
128000030811     C                   write     FNVAP000
128100030811     C*
128200030811     C                   ENDSR
128300030811     C***
128400010601
128500010601
128600010601
128700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
128800010601     C     repfil        BEGSR
128900010601     C*
129000010601     C                   if        invfil = *zeros and
129100010601     C                             depfil > *zeros and
129200010613     C                             (vinflg = *blanks or
129300010613     C                              vinflg = *zeros)
129400010601     C
129500010601     C                   eval      invfil = depfil
129600010601     C                   endif
129700010601     C*
129800010601     C                   if        depfil <> invfil and
129900010601     C                             invfil > *zeros
130000010601     C                   eval      flgMulti = '1'
130100010601     C                   if        vinflg = *blanks
130200010601     C                   add       1             cntNonEl
130300010601     C                   endif
130400010601     C                   endif
130500010601     C*
130600010601     C                   if        vinflg = '2'
130700010601     C                   eval      flgStato = '2'
130800010601     C                   endif
130900010601     C*
131000010601     C                   ENDSR
131100010601     C***
131200010601
131300010601
131400030811
131500010601
131600990920      /TITLE Invio dei dati al punto operativo.
131700000613     C     invio         BEGSR
131800990920     C*
131900990920     C                   reset                   dscmz
132000010601     C                   move      invfil        cmzdst
132100990920     C                   eval      cmzfld = vlrfou
132200990920     C                   eval      cmzmbd = vlrhdl
132300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
132400000710     C                   if        prmfir = *blanks
132500020916     C                   eval      cmzfla = 'FIVAB00F'
132600020916     C                   eval      cmzmba = 'FIVAB00F'
132700000710     C                   else
132800000710     C                   eval      cmzfla = prmfir
132900000710     C                   eval      cmzmba = prmfir
133000000710     C                   endif
133100990920     C                   eval      cmznrr = *zeros
133200990920     C                   move      §ctrok        cmznrr
133300021003     C                   eval      cmzlba = vlrfl1
133400990920     C                   call(e)   'TIS711C'
133500990920     C                   parm                    dscmz
133600990921     C                   parm      *blanks       esito
133700990920     C                   if        %error
133800990920     C                             or cmzerr = '1'
133900990921     C                             or esito  = '1'
134000000710     C                   eval      wrkesito = '3'
134100990920     C                   endif
134200990920     C*
134300000613     C                   ENDSR
134400030915
134500030915
134600030915
134700030915
134800030915      /TITLE Invio ORM
134900030915     C     invioORM      BEGSR
135000030915     C*
135100030915     C* IMPOSTO MANUALMENTE LA KPJBA
135200030915     C                   CLEAR                   KPJBA
135300030915     C                   EVAL      KNMUS = 'EDPCED'
135400030915     C                   EVAL      KNMTD = 'DSP01'
135500030915     C                   EVAL      KSTJO = 'N'
135600030915     C                   EVAL      KTPAZ = 'B'
135700030915     C                   EVAL      KNMEB = 'QPGMR'
135800030915     C                   EVAL      KEXCN = 'J'
135900030915     C                   EVAL      KCOJB = 'P3'
136000030915     C                   EVAL      KCANC = 'N'
136100030915     C                   EVAL      KNSIF = 'GAITRA201'
136200030915     C                   EVAL      KCOAZ = 'OR92'
136300030915     C                   EVAL      KQEBI = 'KSIMMED'
136400030915     C*
136500030915     C* SOTTOMISSIONE AZIONE
136600030915     C                   CALL(e)   'BCH10'
136700030915     C                   PARM                    kpjba
136800030915     C*
136900030915     C                   ENDSR
137000010601
137100010601
137200010601
137300010601
137400010601      /TITLE Invio dei dati al punto operativo.
137500010601     C     opeini        BEGSR
137600010601     C*
137700010601     C* Inizializzo flag e contatori operativi
137800010601     C                   movel     '0'           flgGiro           1
137900010601     C                   movel     '0'           flgMulti          1
138000010601     C                   movel     '1'           flgStato          1
138100010615     C                   movel     '0'           flgOk             1
138200010601     C                   z-add     *zeros        cntNonEl         10 0
138300010601     C                   z-add     *zeros        depfil            3 0
138400010601     C                   z-add     *zeros        invfil            3 0
138500030407     C*
138600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
138700030407     C                   if        %subst(vlrppt:1:1) = 'S'
138800030407     C                   seton                                        50
138900030407     C                   else
139000030407     C                   setoff                                       50
139100030407     C                   endif
139200010601     C*
139300010601     C                   ENDSR
139400010601     C***
139500010601
139600030811
139700030811
139800030811
139900030811      /TITLE Operazioni finali di chiusura *PGM di utilità
140000030811     C     exefin        BEGSR
140100030811     C*
140200030811     C* Chiusura TRUL13R
140300030811     C                   clear                   TRUL13DS
140400030811     C                   eval      I13TLA = 'C'
140500030811     C                   call      'TRUL13R'
140600030811     C                   parm                    TRUL13DS
140700030811     C*
140800030811     C* Chiusura TRUL33R
140900030811     C                   clear                   TRUL33DS
141000030811     C                   eval      I33TLA = 'C'
141100030908     C                   movel     TRUL33DS      KPJBU
141200030811     C                   call      'TRUL33R'
141300030908     C                   parm                    KPJBA
141400030811     C*
141500030811     C* Chiusura TISI95R
141600030811     C                   clear                   TISI95DS
141700030811     C                   eval      I95TLA = 'C'
141800030811     C                   call      'TISI95R'
141900030811     C                   parm                    TISI95DS
142000030811     C*
142100030811     C                   ENDSR
142200030811     C***
142300010601
142400010601
142500990910
142600000613     C     *inzsr        BEGSR
142700990910     C*
142800990910     C     *entry        plist
142900990920     C                   parm                    tivlrds
143000990921     C                   parm      wrkesito      esito
143100000724     C                   parm                    prmlit
143200000710     C                   parm                    prmfir
143300030811     C*
143400030811     C* DEFINIZIONE CHIAVI
143500030811     C*
143600030811     C* TNTBE01L - parziale
143700030811     C     KEYtbe        klist
143800030811     C                   kfld                    KtbCOD                         *UTENTE
143900030811     C                   kfld                    KtbKE1                         *CODICE
144000030929     C*
144100030929     C* FNORMTMPL- completa
144200030929     C     KEYormtmpl    klist
144300030929     C                   kfld                    vabRSD
144400030929     C                   kfld                    vabIND
144500030929     C                   kfld                    vabCAD
144600030929     C                   kfld                    vabLOD
144700030929     C                   kfld                    vabPRD
144800030929     C                   kfld                    vabNZD
144900010606     C*
145000010606     C* CALCOLA LA DATA CORRENTE
145100010606     C                   time                    wn14             14 0
145200010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
145300030811     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
145400010606     C                   z-add     wn8           g08dat
145500010606     C                   z-add     *zeros        g08inv
145600010606     C                   movel     '0'           g08err
145700010606     C                   call      'XSRDA8'
145800010606     C                   parm                    wlbda8
145900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
146000000613     C*
146100000613     C                   ENDSR
146200000613     C***
146300990908
146400030811     Otitv65p   E            riepilogo         2
146500990915     O                                              'Upload via Internet'
146600990915     O                                           +1 'Traduzione TIVIN00R -'
146700020916     O                                           +1 'FIVABWWR'
146800990915     O                                           +1 'Testate bolle'
146900990915     O          E            riepilogo   2
147000990915     O                       wrkdata
147100990915     O                       wrkora              +1
147200990915     O                       procname            +1
147300990915     O          E            riepilogo   2
147400990915     O                                              'Cliente..................:'
147500990915     O                       VABCCM        z     +1
147600990915     O          E            riepilogo   2
147700990920     O                                              'Riferimento Strategi.....:'
147800990920     O                       vlrhdl              +1
147900990915     O          E            riepilogo   2
148000990915     O                                              'Giusti...................:'
148100971022     O                       §CTROK        2   +  1
148200990915     O          E            riepilogo   2
148300990915     O                                              'Sbagliati e corretti.....:'
148400971022     O                       §CTRMO        2   +  1
148500990915     O          E            riepilogo   2
148600990915     O                                              'Sbagliati e scartati.....:'
148700971022     O                       §CTRNO        2   +  1
148800000613
148900030811     Otitv65ps  E            testdett          2
149000000613     O                                              'Upload via Internet'
149100000613     O                                           +1 'Traduzione TIVIN00R -'
149200020916     O                                           +1 'FIVABWWR'
149300000613     O                                           +1 'Testate bolle'
149400000616     O          E            testdett    3
149500000613     O                                           +2 'N° rec'
149600000613     O                                           +3 'Anteprima contenuto'
149700000616     O          E            rigadett    2
149800000613     O                       rrnum               +2
149900000621     O                       recko               +3
150000000616     O          E            findett     2
150100000613     O                       wrkdata
150200000613     O                       wrkora              +1
150300000613     O                       procname            +1
150400000616     O          E            findett     2
150500000613     O                                              'Cliente..................:'
150600000613     O                       VABCCM        z     +1
150700000616     O          E            findett     2
150800000613     O                                              'Riferimento Strategi.....:'
150900000613     O                       vlrhdl              +1
151000000616     O          E            findett     2
151100000613     O                                              'Giusti...................:'
151200000613     O                       §CTROK        2   +  1
151300000616     O          E            findett     2
151400000613     O                                              'Sbagliati e corretti.....:'
151500000613     O                       §CTRMO        2   +  1
151600000616     O          E            findett     2
151700000613     O                                              'Sbagliati e scartati.....:'
151800000613     O                       §CTRNO        2   +  1
151900000613** CMD - COMANDI CL
152000030811OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV65P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
152100030811OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV65PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
152200030811DLTOVR FILE(TITV65P TITV65PS) LVL(*)
152300030929ADDLIBLE LOTTOMATIC
152400030929RMVLIBLE LOTTOMATIC
