000100100719      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200100719     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020913     FFIVABwwr  O    E             DISK    usropn
000700050209     FFIVATwwr  O    E             DISK    usropn
000800100719     Ftis7t4p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000100719     Ftis7t4ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200990908
001300000512     D*------------
001400000512     D* COMANDI
001500000512     D*------------
001600011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000512     D*------------
001800990920     D dscmz         e ds                  inz
001900990910     D psds           sds
002000100719     D  procname         *PROC
002100100719     D*------------
002200100719     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002300100719     D*------------
002400100719     D* 10+3
002500100719     D                 DS
002600100719     D  WI15                   1     10
002700100719     D  WD15                  11     13
002800100719     D  WASC15                 1     13
002900100719     D*  6+1
003000100719     D                 DS
003100100719     D  WI09                   1      6
003200100719     D  WD09                   7      7
003300100719     D  WASC09                 1      7
003400100719     D*  2+3
003500100719     D                 DS
003600100719     D  WI07                   1      2
003700100719     D  WD07                   3      5
003800100719     D  WASC07                 1      5
003900100719     D*
004000990920     D tivlrds       e ds                  extname(tivlr00f)
004100100719     D tisi95ds      e ds                  inz
004200990910     D esito           s              1
004300000724     D prmlit          s             10
004400000710     D prmfir          s             10
004500990921     D wrkesito        s                   like(esito)
004600990915     D wrkdata         s               d
004700990915     D wrkora          s               t
004800000613     D rrnum           s              6  0 INZ(*zeros)
004900000621     D recko           s            150    INZ(*blanks)
005000011123     D depcmd          s            150    INZ(*blanks)
005100050209     D parccm          s              8    INZ(*blanks)
005200050209     D parmbr          s             10    INZ(*blanks)
005300050209     D paropz          s              1    INZ(*blanks)
005400050209     D chkcall         s              1    INZ(*blanks)
005500100719     D*---------------------------------------------------------
005600100719     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
005700100719     D*---------------------------------------------------------
005800100719     D                 ds
005900100719     D vhbatb                  1      1
006000100719     D vhbccm                  2      8
006100100719     D vhblnp                  9     11
006200100719     D vhbaas                 12     15
006300100719     D vhbmgs                 16     19
006400100719     D vhbnrs                 20     21
006500100719     D vhbnsp                 22     28
006600100719     D vhbcbo                 29     30
006700100719     D vhblna                 31     33
006800100719     D vhbrsd                 34     68
006900100719     D vhbrd2                 69    103
007000100719     D vhbind                104    138
007100100719     D vhbcad                139    147
007200100719     D vhblod                148    182
007300100719     D vhbprd                183    184
007400100719     D vhbnzd                185    187
007500100719     D vhbgc1                188    189
007600100719     D vhbgc2                190    191
007700100719     D vhbctr                192    194
007800100719     D vhbtsp                195    195
007900100719     D vhbias                196    208
008000100719     D vhbvas                209    211
008100100719     D vhbnas                212    226
008200100719     D vhbncl                227    231
008300100719     D vhbpkb                232    238
008400100719     D vhbvlb                239    243
008500100719     D vhbqft                244    256
008600100719     D vhbcas                257    269
008700100719     D vhbtic                270    271
008800100719     D vhbvca                272    274
008900100719     D vhbgca                275    276
009000100719     D vhbrmn                277    291
009100100719     D vhbrma                292    306
009200100719     D vhbncd                307    313
009300100719     D vhbnca                314    320
009400100719     D vhbxco                321    321
009500100719     D vhbnot                322    356
009600100719     D vhbnt2                357    391
009700100719     D vhbznc                392    393
009800100719     D vhbctm                394    395
009900100719     D vhbffd                396    396
010000100719     D vhbdcr                397    404
010100100719     D vhbtcr                405    405
010200100719     D vhbhcr                406    409
010300100719     D vhbcts                410    411
010400100719     D vhbftm                412    412
010500100719     D vhbvmd                413    425
010600100719     D vhbvad                426    428
010700100719     D vhbgma                429    430
010800100719     D vhbgga                431    432
010900100719     D vhbgva                433    434
011000100719     D vhbtc1                435    435
011100100719     D vhbtc2                436    436
011200100719     D vhbscl                437    437
011300100719     D vhbant                438    446
011400100719     D vhbrmo                447    471
011500100719     D vhbcmo                472    480
011600100719     D vhbnmo                481    483
011700100719     D  X_rec                  1    483    inz
011800100719     D*
011900050209     D*------------------
012000050209     D* DS REPERIMENTO NUMERATORE
012100050209     D*------------------
012200050303     D trul33ds      e ds                  inz
012300050209     D*------------------
012400050209     D* DS ARCHITETTURA
012500050209     D*------------------
012600050209     D kpjba         e ds                  inz
012700050209     D*------------------
012800100719
012900100719
013000100719     D*------------------
013100100719     D* VARIABILI X *PGM UTILITA XCHKCHAR
013200100719     D*------------------
013300100719     D TxtInOut        S           2048
013400100719     D ElencoChar      S            256
013500100719     D TipoElenco      S              1
013600100719     D CharSost        S              1
013700100719     D UpperCase       S              1
013800100719     D ChkNull         S              1
013900100719     D CharNull        S              1
014000100719     D EsitoBon        S              1
014100100719
014200100719
014300100719     D*------------------
014400100719     D* LINKING A DEFINIZIONI ESTERNE
014500100719     D*------------------
014600100719     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
014700100719     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
014800100719
014900100719
015000050209
015100050209     C* /MAIN
015200990915     C                   time                    wrkdata
015300990915     C                   time                    wrkora
015400000913     C                   reset                   rrnum
015500990921     C                   reset                   esito
015600990921     C                   reset                   wrkesito
015700010530     C*
015800010530     C                   exsr      opeini
015900010530     C                   exsr      rwvab
016000050209     C*
016100050209     C* Effettuo la chiamata al CLLE preposto
016200050209     C                   call(e)   'TITVVTC'
016300050209     C                   parm                    parccm
016400050209     C                   parm                    parmbr
016500050209     C                   parm      '2'           paropz
016600010530     C*
016700010530     C                   seton                                        lr
016800010530
016900010530
017000010530
017100010530
017200010530     C*--------------------------------------------------------
017300010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
017400010530     C*--------------------------------------------------------
017500010530     C     PREELA        BEGSR
017600000724     C*
017700000724     C* SE OCCORRE SPEDIRE IN FILIALE
017800010530     C                   if        invfil <> *zeros and
017900010530     C                             flgGiro = '0'
018000010530     C*
018100010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
018200010530     C                   eval      flgGiro = '1'
018300000724     C*
018400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
018500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
018600010530     C     invfil        chain     azorg01l
018700000724     C                   if        %found
018800000616     C                   movel(p)  CMD(1)        depcmd
018900020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
019000000616     C*
019100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
019200011123     C                   Z-ADD     150           LENGH            15 5
019300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019400000616     C                   PARM                    depcmd
019500000616     C                   PARM                    LENGH
019600000724     C*
019700000724     C                   endif
019800000616     C*
019900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
020000000616     C                   movel(p)  CMD(2)        depcmd
020100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
020200000616     C*
020300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
020400011123     C                   Z-ADD     150           LENGH            15 5
020500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020600000616     C                   PARM                    depcmd
020700000616     C                   PARM                    LENGH
020800000616     C*
020900100719     C                   if        not %open(tis7t4ps)
021000100719     C                   open      tis7t4ps
021100030407     C   50              except    testdett
021200000613     C                   endif
021300010530     C*
021400010530     C                   endif
021500010530     C*
021600010530     C                   ENDSR
021700010530     C***
021800010530
021900010530
022000010530
022100010530
022200010530
022300010530
022400010530     C*--------------------------------------------------------
022500010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
022600010530     C*--------------------------------------------------------
022700010530     C     ENDELA        BEGSR
022800010530     C*
022900010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
023000000613     C*
023100100719     C                   if        %open(tis7t4ps)
023200030407     C   50              except    findett
023300100719     C                   close     tis7t4ps
023400000613     C                   endif
023500000616     C*
023600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
023700011123     C                   Z-ADD     150           LENGH            15 5
023800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
023900000616     C                   PARM                    CMD(3)
024000000616     C                   PARM                    LENGH
024100010530     C*
024200010530     C                   ENDSR
024300010530     C***
024400000613
024500010530
024600010530
024700000613
024800000613     C*--------------------------------------------------------
024900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
025000000613     C*--------------------------------------------------------
025100000621     C     STPR          BEGSR
025200000613     C*
025300100719     C                   if        not %open(tis7t4p)
025400100719     C                   open      tis7t4p
025500990915     C                   endif
025600990915     C*
025700030407     C   50              except    riepilogo
025800990915     C*
025900100719     C                   if        %open(tis7t4p)
026000100719     C                   close     tis7t4p
026100990914     C                   endif
026200990910     C*
026300000613     C                   ENDSR
026400000613     C***
026500990908
026600010530
026700010530
026800010530
026900910830     C*--------------------------------------------------------
027000020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
027100910830     C*--------------------------------------------------------
027200941229     C     RWVAB         BEGSR
027300990910     C*
027400990914     C                   if        not %open(tivin00r)
027500990908     C                   open      tivin00r
027600990914     C                   endif
027700020913     C                   if        not %open(fivabwwr)
027800020913     C                   open      fivabwwr
027900990914     C                   endif
028000050209     C*
028100050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
028200050209     C                   exsr      prevat
028300050209     C*
028400050209     C                   if        chkcall = '0'
028500050209     C*
028600050209     C                   if        not %open(fivatwwr)
028700050209     C                   open      fivatwwr
028800050209     C                   endif
028900050209     C*
029000990910     C                   clear                   §CTROK
029100990910     C                   clear                   §CTRMO
029200990910     C                   clear                   §CTRNO
029300990910     C*
029400921023     C                   DO        *HIVAL
029500990913     C*
029600990915     C                   READ      tivin00r                               70
029700010618     C*
029800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
029900010618     C                   if        vinflg = '1'
030000010618     C                   eval      flgOk = '1'
030100010618     C                   endif
030200010705     C*
030300010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
030400010705     C                   if        %subst(vindta:1:1) = '='
030500010705     C                   eval      %subst(vindta:1:1) = *blanks
030600010705     C                   endif
030700010705     C*
030800000905     C                   if        vindta > *blanks
030900000613     C                   add       1             rrnum
031000010601     C* ...elimino eventuali segni
031100010601     C     '+':' '       xlate     vindta        vindta
031200990913     C*
031300010601     C                   if        *in70 = *off and
031400990913     C                             (vinflg = *blanks
031500010711     C                              or vinflg = '0'
031600010711     C                              or vinflg = '2')
031700990913     C*
031800020913     C                   clear                   FIVAB000
031900050209     C                   clear                   FIVAT000
032000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
032100010711     C                   if        vinflg = *blanks or vinflg = '0'
032200010711     C                   clear                   vinmsg
032300010711     C                   endif
032400010711     C*
032500100719     C                   eval      x_rec  = vindta
032600991022     C*
032700991022     C                   testb     x'3F'         vhbatb                   02
032800991022     C                   if        not *in02
032900991022     C*
033000971021     C                   SETOFF                                       303132
033100971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
033200971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
033300971023     C*    OPPURE TUTTI BLANKS
033400100719     C*
033500100719     C                   TESTN                   VHBLNP               30
033600100719     C  N30              TESTN                   VHBLNP                 30
033700100719     C  N30              TESTN                   VHBLNP                   30
033800100719     C                   MOVE      VHBLNP        W001A             1
033900100719     C     *IN30         IFEQ      *OFF
034000100719     C     W001A         ORLT      '0'
034100100719     C                   SETON                                        31
034200100719     C                   eval      vinmsg = %trimr(vinmsg)
034300100719     C                             + ' ' + 'VHBLNP'
034400100719     C                   ELSE
034500100719     C                   MOVE      VHBLNP        VABLNP
034600100719     C     VABLNP        IFLE      *ZEROS
034700100719     C                   SETON                                        31
034800100719     C                   eval      vinmsg = %trimr(vinmsg)
034900100719     C                             + ' ' + 'VHBLNP'
035000100719     C                   ENDIF
035100100719     C                   ENDIF
035200100719     C*
035300100719     C* Effettuo considerazioni x elaborazioni "multi-filiale"
035400100719     C                   eval      depfil = VABLNP
035500100719     C                   exsr      repfil
035600100719     C                   if        depfil = invfil
035700100719     C                   if        vlrpoi = 999
035800100719     C                   MOVE(P)   invfil        VABFGS
035900100719     C                   else
036000100719     C                   MOVE(P)   vlrpoi        VABFGS
036100100719     C                   endif
036200100719     C*
036300100719     C                   exsr      PREELA
036400100719     C*
036500100719     C                   TESTN                   VHBCCM               30
036600100719     C  N30              TESTN                   VHBCCM                 30
036700100719     C  N30              TESTN                   VHBCCM                   30
036800100719     C                   MOVE      VHBCCM        W001A             1
036900100719     C     *IN30         IFEQ      *OFF
037000100719     C     W001A         ORLT      '0'
037100100719     C                   SETON                                        31
037200100719     C                   eval      vinmsg = %trimr(vinmsg)
037300100719     C                             + ' ' + 'VHBCCM'
037400100719     C                   ELSE
037500100719     C                   MOVE      VHBCCM        VABCCM
037600100719     C     VABCCM        IFLE      *ZEROS
037700100719     C                   SETON                                        31
037800100719     C                   eval      vinmsg = %trimr(vinmsg)
037900100719     C                             + ' ' + 'VHBCCM'
038000100719     C                   ENDIF
038100100719     C                   ENDIF
038200100719     C*
038300100719     C                   TESTN                   VHBAAS               30
038400100719     C  N30              TESTN                   VHBAAS                 30
038500100719     C  N30              TESTN                   VHBAAS                   30
038600100719     C                   MOVE      VHBAAS        W001A             1
038700100719     C     *IN30         IFEQ      *OFF
038800100719     C     W001A         ORLT      '0'
038900100719     C     W001A         ANDNE     *BLANK
039000100719     C                   SETON                                        32
039100100719     C                   MOVEL     datcor        VABAAS
039200100719     C                   eval      vinmsg = %trimr(vinmsg)
039300100719     C                             + ' ' + 'VHBAAS'
039400100719     C                   ELSE
039500100719     C                   MOVE      VHBAAS        VABAAS
039600100719     C                   ENDIF
039700100719     C*
039800100719     C                   TESTN                   VHBMGS               30
039900100719     C  N30              TESTN                   VHBMGS                 30
040000100719     C  N30              TESTN                   VHBMGS                   30
040100100719     C                   MOVE      VHBMGS        W001A             1
040200100719     C     *IN30         IFEQ      *OFF
040300100719     C     W001A         ORLT      '0'
040400100719     C     W001A         ANDNE     *BLANK
040500100719     C                   SETON                                        32
040600100719     C                   MOVE      '0101'        VABMGS
040700100719     C                   eval      vinmsg = %trimr(vinmsg)
040800100719     C                             + ' ' + 'VHBMGS'
040900100719     C                   ELSE
041000100719     C                   MOVE      VHBMGS        VABMGS
041100100719     C                   ENDIF
041200100719     C*
041300100719     C                   TESTN                   VHBNRS               30
041400100719     C  N30              TESTN                   VHBNRS                 30
041500100719     C  N30              TESTN                   VHBNRS                   30
041600100719     C                   MOVE      VHBNRS        W001A             1
041700100719     C     *IN30         IFEQ      *OFF
041800100719     C     W001A         ORLT      '0'
041900100719     C     W001A         ANDNE     *BLANK
042000100719     C                   SETON                                        32
042100100719     C                   MOVE      *ZERO         VABNRS
042200100719     C                   eval      vinmsg = %trimr(vinmsg)
042300100719     C                             + ' ' + 'VHBNRS'
042400100719     C                   ELSE
042500100719     C                   MOVE      VHBNRS        VABNRS
042600100719     C                   ENDIF
042700100719     C*
042800100719     C                   TESTN                   VHBNSP               30
042900100719     C  N30              TESTN                   VHBNSP                 30
043000100719     C  N30              TESTN                   VHBNSP                   30
043100100719     C                   MOVE      VHBNSP        W001A             1
043200100719     C     *IN30         IFEQ      *OFF
043300100719     C     W001A         ORLT      '0'
043400100719     C     W001A         ANDNE     *BLANK
043500100719     C                   SETON                                        32
043600100719     C                   MOVE      *ZERO         VABNSP
043700100719     C                   eval      vinmsg = %trimr(vinmsg)
043800100719     C                             + ' ' + 'VHBNSP'
043900100719     C                   ELSE
044000100719     C                   MOVE      VHBNSP        VABNSP
044100100719     C                   ENDIF
044200100719     C*
044300100719     C                   TESTN                   VHBLNA               30
044400100719     C  N30              TESTN                   VHBLNA                 30
044500100719     C  N30              TESTN                   VHBLNA                   30
044600100719     C                   MOVE      VHBLNA        W001A             1
044700100719     C     *IN30         IFEQ      *OFF
044800100719     C     W001A         ORLT      '0'
044900100719     C     W001A         ANDNE     *BLANK
045000100719     C                   SETON                                        32
045100100719     C                   MOVE      *ZERO         VABLNA
045200100719     C                   eval      vinmsg = %trimr(vinmsg)
045300100719     C                             + ' ' + 'VHBLNA'
045400100719     C                   ELSE
045500100719     C                   MOVE      VHBLNA        VABLNA
045600100719     C                   ENDIF
045700100719     C*
045800100719     C                   TESTN                   VHBCTR               30
045900100719     C  N30              TESTN                   VHBCTR                 30
046000100719     C  N30              TESTN                   VHBCTR                   30
046100100719     C                   MOVE      VHBCTR        W001A             1
046200100719     C     *IN30         IFEQ      *OFF
046300100719     C     W001A         ORLT      '0'
046400100719     C     W001A         ANDNE     *BLANK
046500100719     C                   SETON                                        32
046600100719     C                   MOVE      *ZERO         VABCTR
046700100719     C                   eval      vinmsg = %trimr(vinmsg)
046800100719     C                             + ' ' + 'VHBCTR'
046900100719     C                   ELSE
047000100719     C                   MOVE      VHBCTR        VABCTR
047100100719     C                   ENDIF
047200100719     C*
047300100719     C                   MOVE      *ZERO         VABIAS
047400100719     C                   MOVE      VHBIAS        WASC15
047500100719     C                   TESTN                   WI15                 30
047600100719     C  N30              TESTN                   WI15                   30
047700100719     C  N30              TESTN                   WI15                     30
047800100719     C                   MOVE      WI15          W001A             1
047900100719     C     *IN30         IFEQ      *OFF
048000100719     C     W001A         ORLT      '0'
048100100719     C     W001A         ANDNE     *BLANK
048200100719     C                   SETON                                        32
048300100719     C                   MOVE      *ZERO         VABIAS
048400100719     C                   eval      vinmsg = %trimr(vinmsg)
048500100719     C                             + ' ' + 'VHBIAS'
048600100719     C                   ELSE
048700100719     C                   MOVEL     WI15          VABIAS
048800100719     C                   TESTN                   WD15                 30
048900100719     C  N30              TESTN                   WD15                   30
049000100719     C  N30              TESTN                   WD15                     30
049100100719     C                   MOVE      WD15          W001A             1
049200100719     C     *IN30         IFEQ      *OFF
049300100719     C     W001A         ORLT      '0'
049400100719     C     W001A         ANDNE     *BLANK
049500100719     C                   SETON                                        32
049600100719     C                   MOVE      *ZERO         VABIAS
049700100719     C                   eval      vinmsg = %trimr(vinmsg)
049800100719     C                             + ' ' + 'VHBIAS'
049900100719     C                   ELSE
050000100719     C                   MOVE      WD15          VABIAS
050100100719     C                   ENDIF
050200100719     C                   ENDIF
050300100719     C*
050400100719     C                   TESTN                   VHBNCL               30
050500100719     C  N30              TESTN                   VHBNCL                 30
050600100719     C  N30              TESTN                   VHBNCL                   30
050700100719     C                   MOVE      VHBNCL        W001A             1
050800100719     C     *IN30         IFEQ      *OFF
050900100719     C     W001A         ORLT      '0'
051000100719     C                   SETON                                        32
051100100719     C                   MOVE      *ZERO         VABNCL
051200100719     C                   eval      vinmsg = %trimr(vinmsg)
051300100719     C                             + ' ' + 'VHBNCL'
051400100719     C                   ELSE
051500100719     C                   MOVE      VHBNCL        VABNCL
051600100719     C                   ENDIF
051700100719     C*
051800100719     C                   MOVE      *ZERO         VABPKB
051900100719     C                   MOVE      VHBPKB        WASC09
052000100719     C                   IF        WI09 = *blanks
052100100719     C                   EVAL      WI09 = *zeros
052200100719     C                   ENDIF
052300100719     C                   TESTN                   WI09                 30
052400100719     C  N30              TESTN                   WI09                   30
052500100719     C  N30              TESTN                   WI09                     30
052600100719     C                   MOVE      WI09          W001A             1
052700100719     C     *IN30         IFEQ      *OFF
052800100719     C     W001A         ORLT      '0'
052900100719     C                   SETON                                        32
053000100719     C                   MOVE      *ZERO         VABPKB
053100100719     C                   eval      vinmsg = %trimr(vinmsg)
053200100719     C                             + ' ' + 'VHBPKB'
053300100719     C                   ELSE
053400100719     C                   MOVEL     WI09          VABPKB
053500100719     C                   TESTN                   WD09                 30
053600100719     C  N30              TESTN                   WD09                     30
053700100719     C                   MOVE      WD09          W001A             1
053800100719     C     *IN30         IFEQ      *OFF
053900100719     C     W001A         ORLT      '0'
054000100719     C     W001A         ANDNE     *BLANK
054100100719     C                   SETON                                        32
054200100719     C                   MOVE      *ZERO         VABPKB
054300100719     C                   eval      vinmsg = %trimr(vinmsg)
054400100719     C                             + ' ' + 'VHBPKB'
054500100719     C                   ELSE
054600100719     C                   MOVE      WD09          VABPKB
054700100719     C                   ENDIF
054800100719     C                   ENDIF
054900100719     C*
055000100719     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
055100100719     C                   IF        *IN51 = *OFF
055200100719     C                   MOVE      *ZERO         VABVLB
055300100719     C                   MOVE      VHBVLB        WASC07
055400100719     C                   IF        WI07 = *blanks
055500100719     C                   EVAL      WI07 = *zeros
055600100719     C                   ENDIF
055700100719     C                   TESTN                   WI07                 30
055800100719     C  N30              TESTN                   WI07                   30
055900100719     C  N30              TESTN                   WI07                     30
056000100719     C                   MOVE      WI07          W001A             1
056100100719     C     *IN30         IFEQ      *OFF
056200100719     C     W001A         ORLT      '0'
056300100719     C     W001A         ANDNE     *BLANK
056400100719     C                   SETON                                        32
056500100719     C                   MOVE      *ZERO         VABVLB
056600100719     C                   eval      vinmsg = %trimr(vinmsg)
056700100719     C                             + ' ' + 'VHBVLB'
056800100719     C                   ELSE
056900100719     C                   MOVEL     WI07          VABVLB
057000100719     C                   TESTN                   WD07                 30
057100100719     C  N30              TESTN                   WD07                   30
057200100719     C  N30              TESTN                   WD07                     30
057300100719     C                   MOVE      WD07          W001A             1
057400100719     C     *IN30         IFEQ      *OFF
057500100719     C     W001A         ORLT      '0'
057600100719     C     W001A         ANDNE     *BLANK
057700100719     C                   SETON                                        32
057800100719     C                   MOVE      *ZERO         VABVLB
057900100719     C                   eval      vinmsg = %trimr(vinmsg)
058000100719     C                             + ' ' + 'VHBVLB'
058100100719     C                   ELSE
058200100719     C                   MOVE      WD07          VABVLB
058300100719     C                   ENDIF
058400100719     C                   ENDIF
058500100719     C                   ELSE
058600100719     C* ...altrimenti forzo azzeramento del volume
058700100719     C                   Z-ADD     *zeros        VABVLB
058800100719     C                   ENDIF
058900100719     C*
059000100719     C                   MOVE      *ZERO         VABQFT
059100100719     C                   MOVE      VHBQFT        WASC15
059200100719     C                   TESTN                   WI15                 30
059300100719     C  N30              TESTN                   WI15                   30
059400100719     C  N30              TESTN                   WI15                     30
059500100719     C                   MOVE      WI15          W001A             1
059600100719     C     *IN30         IFEQ      *OFF
059700100719     C     W001A         ORLT      '0'
059800100719     C     W001A         ANDNE     *BLANK
059900100719     C                   SETON                                        32
060000100719     C                   MOVE      *ZERO         VABQFT
060100100719     C                   eval      vinmsg = %trimr(vinmsg)
060200100719     C                             + ' ' + 'VHBQFT'
060300100719     C                   ELSE
060400100719     C                   MOVEL     WI15          VABQFT
060500100719     C                   TESTN                   WD15                 30
060600100719     C  N30              TESTN                   WD15                   30
060700100719     C  N30              TESTN                   WD15                     30
060800100719     C                   MOVE      WD15          W001A             1
060900100719     C     *IN30         IFEQ      *OFF
061000100719     C     W001A         ORLT      '0'
061100100719     C     W001A         ANDNE     *BLANK
061200100719     C                   SETON                                        32
061300100719     C                   MOVE      *ZERO         VABQFT
061400100719     C                   eval      vinmsg = %trimr(vinmsg)
061500100719     C                             + ' ' + 'VHBQFT'
061600100719     C                   ELSE
061700100719     C                   MOVE      WD15          VABQFT
061800100719     C                   ENDIF
061900100719     C                   ENDIF
062000100719     C*
062100100719     C                   MOVE      *ZERO         VABCAS
062200100719     C                   MOVE      VHBCAS        WASC15
062300100719     C                   TESTN                   WI15                 30
062400100719     C  N30              TESTN                   WI15                   30
062500100719     C  N30              TESTN                   WI15                     30
062600100719     C                   MOVE      WI15          W001A             1
062700100719     C     *IN30         IFEQ      *OFF
062800100719     C     W001A         ORLT      '0'
062900100719     C     W001A         ANDNE     *BLANK
063000100719     C                   SETON                                        32
063100100719     C                   MOVE      *ZERO         VABCAS
063200100719     C                   eval      vinmsg = %trimr(vinmsg)
063300100719     C                             + ' ' + 'VHBCAS'
063400100719     C                   ELSE
063500100719     C                   MOVEL     WI15          VABCAS
063600100719     C                   TESTN                   WD15                 30
063700100719     C  N30              TESTN                   WD15                   30
063800100719     C  N30              TESTN                   WD15                     30
063900100719     C                   MOVE      WD15          W001A             1
064000100719     C     *IN30         IFEQ      *OFF
064100100719     C     W001A         ORLT      '0'
064200100719     C     W001A         ANDNE     *BLANK
064300100719     C                   SETON                                        32
064400100719     C                   MOVE      *ZERO         VABCAS
064500100719     C                   eval      vinmsg = %trimr(vinmsg)
064600100719     C                             + ' ' + 'VHBCAS'
064700100719     C                   ELSE
064800100719     C                   MOVE      WD15          VABCAS
064900100719     C                   ENDIF
065000100719     C                   ENDIF
065100100719     C*
065200100719     C                   TESTN                   VHBRMN               30
065300100719     C  N30              TESTN                   VHBRMN                 30
065400100719     C  N30              TESTN                   VHBRMN                   30
065500100719     C                   MOVE      VHBRMN        W001A             1
065600100719     C     *IN30         IFEQ      *OFF
065700100719     C     W001A         ORLT      '0'
065800100719     C                   SETON                                        32
065900100719     C                   MOVE      *ZERO         VABRMN
066000100719     C                   eval      vinmsg = %trimr(vinmsg)
066100100719     C                             + ' ' + 'VHBRMN'
066200100719     C                   ELSE
066300100719     C                   MOVE      VHBRMN        VABRMN
066400100719     C                   ENDIF
066500100719     C*
066600100719     C                   TESTN                   VHBNCD               30
066700100719     C  N30              TESTN                   VHBNCD                 30
066800100719     C  N30              TESTN                   VHBNCD                   30
066900100719     C                   MOVE      VHBNCD        W001A             1
067000100719     C     *IN30         IFEQ      *OFF
067100100719     C     W001A         ORLT      '0'
067200100719     C     W001A         ANDNE     *BLANK
067300100719     C                   SETON                                        32
067400100719     C                   MOVE      *ZERO         VABNCD
067500100719     C                   eval      vinmsg = %trimr(vinmsg)
067600100719     C                             + ' ' + 'VHBNCD'
067700100719     C                   ELSE
067800100719     C                   MOVE      VHBNCD        VABNCD
067900100719     C                   ENDIF
068000100719     C*
068100100719     C                   TESTN                   VHBNCA               30
068200100719     C  N30              TESTN                   VHBNCA                 30
068300100719     C  N30              TESTN                   VHBNCA                   30
068400100719     C                   MOVE      VHBNCA        W001A             1
068500100719     C     *IN30         IFEQ      *OFF
068600100719     C     W001A         ORLT      '0'
068700100719     C     W001A         ANDNE     *BLANK
068800100719     C                   SETON                                        32
068900100719     C                   MOVE      *ZERO         VABNCA
069000100719     C                   eval      vinmsg = %trimr(vinmsg)
069100100719     C                             + ' ' + 'VHBNCA'
069200100719     C                   ELSE
069300100719     C                   MOVE      VHBNCA        VABNCA
069400100719     C                   ENDIF
069500100719     C*
069600100719     C                   TESTN                   VHBZNC               30
069700100719     C  N30              TESTN                   VHBZNC                 30
069800100719     C  N30              TESTN                   VHBZNC                   30
069900100719     C                   MOVE      VHBZNC        W001A             1
070000100719     C     *IN30         IFEQ      *OFF
070100100719     C     W001A         ORLT      '0'
070200100719     C     W001A         ANDNE     *BLANK
070300100719     C                   SETON                                        32
070400100719     C                   MOVE      *ZERO         VABZNC
070500100719     C                   eval      vinmsg = %trimr(vinmsg)
070600100719     C                             + ' ' + 'VHBZNC'
070700100719     C                   ELSE
070800100719     C                   MOVE      VHBZNC        VABZNC
070900100719     C                   ENDIF
071000100719     C*
071100100719     C                   TESTN                   VHBDCR               30
071200100719     C  N30              TESTN                   VHBDCR                 30
071300100719     C  N30              TESTN                   VHBDCR                   30
071400100719     C                   MOVE      VHBDCR        W001A             1
071500100719     C     *IN30         IFEQ      *OFF
071600100719     C     W001A         ORLT      '0'
071700100719     C     W001A         ANDNE     *BLANK
071800100719     C                   SETON                                        32
071900100719     C                   MOVE      *ZERO         VABDCR
072000100719     C                   eval      vinmsg = %trimr(vinmsg)
072100100719     C                             + ' ' + 'VHBDCR'
072200100719     C                   ELSE
072300100719     C                   MOVE      VHBDCR        VABDCR
072400100719     C                   ENDIF
072500100719     C*
072600100719     C                   TESTN                   VHBHCR               30
072700100719     C  N30              TESTN                   VHBHCR                 30
072800100719     C  N30              TESTN                   VHBHCR                   30
072900100719     C                   MOVE      VHBHCR        W001A             1
073000100719     C     *IN30         IFEQ      *OFF
073100100719     C     W001A         ORLT      '0'
073200100719     C     W001A         ANDNE     *BLANK
073300100719     C                   SETON                                        32
073400100719     C                   MOVE      *ZERO         VABHCR
073500100719     C                   eval      vinmsg = %trimr(vinmsg)
073600100719     C                             + ' ' + 'VHBHCR'
073700100719     C                   ELSE
073800100719     C                   MOVE      VHBHCR        VABHCR
073900100719     C                   ENDIF
074000100719     C*
074100100719     C                   MOVE      *ZERO         VABVMD
074200100719     C                   MOVE      VHBVMD        WASC15
074300100719     C                   TESTN                   WI15                 30
074400100719     C  N30              TESTN                   WI15                   30
074500100719     C  N30              TESTN                   WI15                     30
074600100719     C                   MOVE      WI15          W001A             1
074700100719     C     *IN30         IFEQ      *OFF
074800100719     C     W001A         ORLT      '0'
074900100719     C     W001A         ANDNE     *BLANK
075000100719     C                   SETON                                        32
075100100719     C                   MOVE      *ZERO         VABVMD
075200100719     C                   eval      vinmsg = %trimr(vinmsg)
075300100719     C                             + ' ' + 'VHBVMD'
075400100719     C                   ELSE
075500100719     C                   MOVEL     WI15          VABVMD
075600100719     C                   TESTN                   WD15                 30
075700100719     C  N30              TESTN                   WD15                   30
075800100719     C  N30              TESTN                   WD15                     30
075900100719     C                   MOVE      WD15          W001A             1
076000100719     C     *IN30         IFEQ      *OFF
076100100719     C     W001A         ORLT      '0'
076200100719     C     W001A         ANDNE     *BLANK
076300100719     C                   SETON                                        32
076400100719     C                   MOVE      *ZERO         VABVMD
076500100719     C                   eval      vinmsg = %trimr(vinmsg)
076600100719     C                             + ' ' + 'VHBVMD'
076700100719     C                   ELSE
076800100719     C                   MOVE      WD15          VABVMD
076900100719     C                   ENDIF
077000100719     C                   ENDIF
077100100719     C*
077200100719     C                   TESTN                   VHBANT               30
077300100719     C  N30              TESTN                   VHBANT                 30
077400100719     C  N30              TESTN                   VHBANT                   30
077500100719     C                   MOVE      VHBANT        W001A             1
077600100719     C     *IN30         IFEQ      *OFF
077700100719     C     W001A         ORLT      '0'
077800100719     C     W001A         ANDNE     *BLANK
077900100719     C                   SETON                                        32
078000100719     C                   MOVE      *ZERO         VABANT
078100100719     C                   eval      vinmsg = %trimr(vinmsg)
078200100719     C                             + ' ' + 'VHBANT'
078300100719     C                   ELSE
078400100719     C                   MOVE      VHBANT        VABANT
078500100719     C                   ENDIF
078600100719     C*
078700100719     C* CAMPI ALFABETICI:
078800100719     C                   MOVE      VHBATB        VABATB
078900100719     C                   MOVE      VHBCBO        VABCBO
079000100719     C                   MOVE      VHBRSD        VABRSD
079100100719     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
079200100719     C     '@':'A'       XLATE     VABRSD        VABRSD
079300100719     C* ==
079400100719     C                   MOVE      VHBRD2        VABRD2
079500100719     C                   MOVE      VHBIND        VABIND
079600100719     C                   MOVE      VHBCAD        VABCAD
079700100719     C                   MOVE      VHBLOD        VABLOD
079800100719     C                   MOVE      VHBPRD        VABPRD
079900100719     C                   MOVE      VHBNZD        VABNZD
080000100719     C                   MOVE      VHBGC1        VABGC1
080100100719     C                   MOVE      VHBGC2        VABGC2
080200100719     C                   MOVE      VHBTSP        VABTSP
080300100719     C                   MOVE      VHBVAS        VABVAS
080400100719     C                   MOVE      VHBNAS        VABNAS
080500100719     C                   MOVE      VHBTIC        VABTIC
080600100719     C                   MOVE      VHBVCA        VABVCA
080700100719     C                   MOVE      VHBGCA        VABGCA
080800100719     C                   MOVE      VHBRMA        VABRMA
080900100719     C                   MOVE      VHBXCO        VABXCO
081000100719     C                   MOVE      VHBNOT        VABNOT
081100100719     C                   MOVE      VHBNT2        VABNT2
081200100719     C  N53              MOVE      VHBCTM        VABCTM
081300100719     C                   MOVE      VHBFFD        VABFFD
081400100719     C                   MOVE      VHBTCR        VABTCR
081500100719     C                   MOVE      VHBCTS        VABCTS
081600100719     C                   MOVE      VHBFTM        VABFTM
081700100719     C                   MOVE      VHBVAD        VABVAD
081800100719     C                   MOVE      VHBGMA        VABGMA
081900100719     C                   MOVE      VHBGGA        VABGGA
082000100719     C                   MOVE      VHBGVA        VABGVA
082100100719     C                   MOVE      VHBTC1        VABTC1
082200100719     C                   MOVE      VHBTC2        VABTC2
082300100719     C                   MOVE      VHBSCL        VABSCL
082400100719     C                   MOVE      VHBRMO        VABRMO
082500100719     C                   MOVE      VHBCMO        VABCMO
082600100719     C                   MOVE      VHBNMO        VABNMO
082700050209     C*
082800050303     C* NSP => Se nn valorizzata numero spedizione => stacco numeratore da AZNUM
082900050303     C                   if        VABNSP = *zeros
083000050209     C                   clear                   TRUL33DS
083100050209     C                   eval      I33OPE = *zeros
083200050209     C                   eval      I33CNU = 302
083300050209     C                   eval      I33NUM = 1
083400050209     C                   movel     TRUL33DS      KPJBU
083500050209     C                   call      'TRUL33R'
083600050209     C                   parm                    KPJBA
083700050209     C                   movel     KPJBU         TRUL33DS
083800050209     C                   if        O33ERR = *zeros
083900050209     C                   z-add     O33NRF        VABNSP
084000050209     C                   z-add     O33NRF        VATNSP
084100050209     C                   else
084200050209     C                   SETON                                        31
084300050209     C                   EVAL      vinmsg = %trimr(vinmsg)
084400050209     C                             + ' ' + 'VABNSP VATNSP'
084500050209     C                   endif
084600050303     C                   endif
084700100719     C*
084800100719     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
084900100719     C* NELLA NAZIONE MITTENTE ORIGINALE
085000100719     C                   IF        %trim(VHBNMO) < 'A'
085100100719     C                   MOVE      *BLANKS       VABNMO
085200100719     C                   ENDIF
085300100719     C*
085400100719     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
085500100719     C                   IF        %trim(VABNZD) = 'I'   OR
085600100719     C                             %trim(VABNZD) = 'IT'  OR
085700100719     C                             %trim(VABNZD) = 'ITA'
085800100719     C                   EVAL      VABNZD = *blanks
085900100719     C                   ENDIF
086000100719     C*
086100100719     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
086200100719     C                   IF        *IN52 = *ON
086300100719     C* Reperisco la provincia dal CAP e dalla località
086400100719     C                   IF        VABCAD <> *blanks AND
086500100719     C                             VABPRD  = *blanks AND
086600100719     C                             VABNZD  = *blanks
086700100719     C                   CLEAR                   TISI95DS
086800100719     C                   EVAL      I95TCN = '3'
086900100719     C                   Z-ADD     datcor        I95DAT
087000100719     C                   EVAL      I95CAP = VABCAD
087100100719     C                   EVAL      I95LOC = VABLOD
087200100719     C                   CALL      'TISI95R'
087300100719     C                   PARM                    TISI95DS
087400100719     C                   EVAL      VABPRD = O95PRV
087500100719     C                   ENDIF
087600100719     C                   ENDIF
087700100719     C*
087800100719     C* "bonifico" i dati d input da eventuali caratteri indesiderati
087900100719     C                   exsr      exeBon
088000100719     C*
088100100719     C* Forzatura su campi DIVISE
088200100719     C                   IF        VABVAS = *blanks
088300100719     C                   EVAL      VABVAS = 'EUR'
088400100719     C                   ENDIF
088500100719     C                   IF        VABVCA = *blanks
088600100719     C                   EVAL      VABVCA = 'EUR'
088700100719     C                   ENDIF
088800100719     C                   IF        VABVAD = *blanks
088900100719     C                   EVAL      VABVAD = 'EUR'
089000100719     C                   ENDIF
089100930409     C*
089200010604     C  N31              ADD       1             §CTROK            7 0
089300010604     C   32              ADD       1             §CTRMO            7 0
089400010604     C   31              ADD       1             §CTRNO            7 0
089500020913     C  N31              WRITE     FIVAB000
089600100719     C  N31
089700100719     CAN 55              EXSR      WRIVAT
089800990910     C*
089900010601     C                   if        *in31 = *off and
090000010601     C                             *in32 = *off
090100990910     C                   eval      vinflg = '1'
090200990910     C                   else
090300000621     C                   eval      recko = vindta
090400100719     C                   if        %open(tis7t4ps)
090500030407     C   50              except    rigadett
090600010612     C                   endif
090700990910     C                   eval      vinflg = '2'
090800990910     C                   endif
090900991022     C                   endif
091000990910     C                   endif
091100010601     C*
091200010601     C                   endif
091300000905     C*
091400000905     C                   else
091500000905     C                   eval      vinflg = '1'
091600000905     C                   endif
091700000905     C*
091800000905     C  N70              update    tivin000
091900991022     C*
092000991022     C  N70              ENDdo
092100010530     C
092200010530     C*
092300010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
092400010530     C                   if        cntNonEl = *zeros or
092500010530     C                             flgMulti = '0'
092600990910     C* Se non ci sono record con errori ...
092700010530     C                   if        §ctrno = 0 and
092800010601     C                             §ctrmo = 0 and
092900010530     C                             flgStato <> '2'
093000990910     C* ... restituisco esito OK.
093100990921     C                   eval      wrkesito = '0'
093200990910     C                   else
093300010601     C                   if        §ctrok > 0
093400990921     C                   eval      wrkesito = '1'
093500000710     C                   else
093600010615     C                   if        flgOk = '0'
093700010615     C                   eval      wrkesito = '2'
093800010615     C                   else
093900010615     C                   eval      wrkesito = '6'
094000010615     C                   endif
094100990910     C                   endif
094200000710     C                   endif
094300010530     C                   else
094400010530     C                   eval      wrkesito = '9'
094500010530     C                   endif
094600050209     C*
094700050209     C                   else
094800050209     C                   eval      wrkesito = '2'
094900050209     C                   endif
095000990910     C*
095100990914     C                   if        %open(tivin00r)
095200990908     C                   close     tivin00r
095300990914     C                   endif
095400020913     C                   if        %open(fivabwwr)
095500020913     C                   close     fivabwwr
095600990914     C                   endif
095700050209     C                   if        %open(fivatwwr)
095800050209     C                   close     fivatwwr
095900050209     C                   endif
096000010530     C*
096100010530     C                   if        vlrpoi <> 999
096200010530     C                   eval      invfil = vlrpoi
096300010530     C                   endif
096400990910     C*
096500990920     C                   if        §ctrok > 0
096600010530     C                             and invfil > *zeros
096700000613     C                   exsr      invio
096800990920     C                   endif
096900010612     C*
097000010612     C                   if        flgGiro = '1'
097100010612     C                   exsr      endela
097200010612     C                   endif
097300990920     C*
097400910830     C                   ENDSR
097500000613     C***
097600050209
097700050209
097800050209
097900050209     C*----------------------------------------------------*
098000050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
098100050209     C*----------------------------------------------------*
098200050209     C     PREVAT        BEGSR
098300050209     C*
098400050209     C* Compongo il nome del membro da dare al FIVATWWR
098500050209     C                   eval      parmbr = vlrhdl
098600050209     C                   movel     'M'           parmbr
098700050209     C                   eval      parccm = %subst(vlrKSC:2:7)
098800050209     C                   eval      paropz = '1'
098900050209     C* Effettuo la chiamata al CLLE preposto
099000050209     C                   call(e)   'TITVVTC'
099100050209     C                   parm                    parccm
099200050209     C                   parm                    parmbr
099300050209     C                   parm                    paropz
099400050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
099500050209     C                   if        %error
099600050209     C                   movel     '1'           chkcall
099700050209     C                   else
099800050209     C                   movel     '0'           chkcall
099900050209     C                   endif
100000050209     C*
100100050209     C                   ENDSR
100200050209
100300050209
100400990920
100500050209     C*----------------------------------------------------*
100600050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
100700050209     C*----------------------------------------------------*
100800050209     C     WRIVAT        BEGSR
100900050209     C*
101000050209     C* Valorizzo il buffer del file dettagli barcode colli
101100050209     C                   EVAL      VATFGS = VABFGS
101200050209     C                   EVAL      VATAAS = VABAAS
101300050209     C                   EVAL      VATLNP = VABLNP
101400050209     C                   EVAL      VATNSP = VABNSP
101500050209     C                   EVAL      VATCCM = VABCCM
101600050209     C                   EVAL      VATTRC='E'
101700050303     C*
101800100719     C* Compongo tanti "chi sono" quanti sono i colli della spedizione sulla base del RMN
101900100719     C                   z-add     10            wIdxRMN           9 0
102000100719     C                   z-add     10            wIdxRMNsav        9 0
102100100719     C     2             do        wMultRMN
102200100719     C                   eval      wIdxRMN = wIdxRMN * 10
102300100719     C                   enddo
102400100719     C*
102500100719     C                   eval      wIdxRMNsav = wIdxRMN
102600100719     C                   movel     *blanks       wVATNOT_E        35
102700100719     C*
102800100719     C                   if        %subst(vlrppt:8:1) = 'N'
102900100719     C                   eval      wVATNOT_E = %trim(%editc(VABRMN:'Z'))
103000100719     C                   else
103100100719     C                   eval      wVATNOT_E = %trim(VABRMA)
103200100719     C                   endif
103300100719     C*
103400100719     C     1             do        VABNCL        wColloNum         9 0
103500100719     C                   eval      wIdxRMN = wIdxRMNsav + wColloNum
103600100719     C                   eval      VATNOT = %trim(wVATNOT_E) +
103700100719     C                                      %subst(%trim(%char(wIdxRMN)):2)
103800100719     C                   if        VATNOT <> *blanks
103900100719     C                   write     FIVAT000
104000100719     C                   endif
104100100719     C                   enddo
104200050209     C*
104300050209     C                   ENDSR
104400010530
104500010530
104600050209
104700010530
104800010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
104900010530     C     repfil        BEGSR
105000010530     C*
105100010530     C                   if        invfil = *zeros and
105200010530     C                             depfil > *zeros and
105300010613     C                             (vinflg = *blanks or
105400010613     C                              vinflg = *zeros)
105500010530     C
105600010530     C                   eval      invfil = depfil
105700010530     C                   endif
105800010530     C*
105900010530     C                   if        depfil <> invfil and
106000010530     C                             invfil > *zeros
106100010530     C                   eval      flgMulti = '1'
106200010530     C                   if        vinflg = *blanks
106300010530     C                   add       1             cntNonEl
106400010530     C                   endif
106500010530     C                   endif
106600010530     C*
106700010530     C                   if        vinflg = '2'
106800010530     C                   eval      flgStato = '2'
106900010530     C                   endif
107000010530     C*
107100010530     C                   ENDSR
107200010530     C***
107300050209
107400050209
107500050209
107600050209
107700050209      /TITLE Invio dei dati al punto operativo.
107800050209     C     invio         BEGSR
107900050209     C*
108000050209     C* 1° invio FIVAT
108100050209     C                   reset                   dscmz
108200050209     C                   move      vlrpoi        cmzdst
108300050209     C                   eval      cmzfld = 'FIVATWWR'
108400050209     C                   eval      cmzmbd = vlrhdl
108500050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
108600050209     C***                if        prmfir = *blanks
108700050209     C                   eval      cmzfla = 'FIVAT00F'
108800050209     C                   eval      cmzmba = 'FIVAT00F'
108900050209     C***                else
109000050209     C***                eval      cmzfla = prmfir
109100050209     C***                eval      cmzmba = prmfir
109200050209     C***                endif
109300050209     C                   eval      cmznrr = *zeros
109400050209     C                   move      §ctrok        cmznrr
109500050209     C                   eval      cmzlba = vlrfl1
109600050209     C                   call(e)   'TIS711C'
109700050209     C                   parm                    dscmz
109800050209     C                   parm      *blanks       esito
109900050209     C                   if        %error
110000050209     C                             or cmzerr = '1'
110100050209     C                             or esito  = '1'
110200050209     C                   eval      wrkesito = '3'
110300050209     C                   else
110400050209     C*
110500050209     C* 2° invio FIVAB
110600050209     C                   reset                   dscmz
110700050209     C                   move      vlrpoi        cmzdst
110800050209     C                   eval      cmzfld = vlrfou
110900050209     C                   eval      cmzmbd = vlrhdl
111000050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
111100050209     C***                if        prmfir = *blanks
111200050209     C                   eval      cmzfla = 'FIVAB00F'
111300050209     C                   eval      cmzmba = 'FIVAB00F'
111400050209     C***                else
111500050209     C***                eval      cmzfla = prmfir
111600050209     C***                eval      cmzmba = prmfir
111700050209     C***                endif
111800050209     C                   eval      cmznrr = *zeros
111900050209     C                   move      §ctrok        cmznrr
112000050209     C                   eval      cmzlba = vlrfl1
112100050209     C                   call(e)   'TIS711C'
112200050209     C                   parm                    dscmz
112300050209     C                   parm      *blanks       esito
112400050209     C                   if        %error
112500050209     C                             or cmzerr = '1'
112600050209     C                             or esito  = '1'
112700050209     C                   eval      wrkesito = '3'
112800050209     C                   endif
112900050209     C                   endif
113000050209     C*
113100050209     C                   ENDSR
113200050209     C***
113300010530
113400010530
113500010530
113600010530
113700010530      /TITLE Invio dei dati al punto operativo.
113800010530     C     opeini        BEGSR
113900010530     C*
114000010530     C* Inizializzo flag e contatori operativi
114100010530     C                   movel     '0'           flgGiro           1
114200010530     C                   movel     '0'           flgMulti          1
114300010530     C                   movel     '1'           flgStato          1
114400010615     C                   movel     '0'           flgOk             1
114500010530     C                   z-add     *zeros        cntNonEl         10 0
114600010530     C                   z-add     *zeros        depfil            3 0
114700010530     C                   z-add     *zeros        invfil            3 0
114800100719     C*
114900100719     C                   z-add     *zeros        wMultRMN          1 0
115000030407     C*
115100050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
115200030407     C                   if        %subst(vlrppt:1:1) = 'S'
115300030407     C                   seton                                        50
115400030407     C                   else
115500030407     C                   setoff                                       50
115600030407     C                   endif
115700041001     C*
115800050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
115900041001     C                   if        %subst(vlrppt:2:1) = 'S'
116000041001     C                   seton                                        51
116100041001     C                   else
116200041001     C                   setoff                                       51
116300041001     C                   endif
116400100719     C*
116500100719     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
116600100719     C* (naturalmente solo se nn già presente)
116700100719     C                   if        %subst(vlrppt:3:1) = 'S'
116800100719     C                   seton                                        52
116900100719     C                   else
117000100719     C                   setoff                                       52
117100100719     C                   endif
117200100719     C*
117300100719     C* Altri parametri...
117400100719     C                   setoff                                       53
117500100719     C                   if        %subst(vlrppt:4:2) <> *blanks
117600100719     C                   eval      vabctm = %subst(vlrppt:4:2)
117700100719     C                   seton                                        53
117800100719     C                   endif
117900010530     C*
118000100719     C                   setoff                                       54
118100100719     C                   if        %subst(vlrppt:6:1) <> *blanks
118200100719     C                   eval      PiDecChr = %subst(vlrppt:6:1)
118300100719     C                   seton                                        54
118400100719     C                   endif
118500100719     C*
118600100719     C                   setoff                                       55
118700100719     C                   if        %subst(vlrppt:7:1) <> *blanks
118800100719     C                   eval      PiStr=%trim(%subst(vlrppt:7:1))
118900100719     C                   exsr      CHKNUM
119000100719     C                   if        PiInt=*on
119100100719     C                   z-add     PiVal         wMultRMN
119200100719     C                   seton                                        55
119300100719     C                   endif
119400100719     C                   endif
119500100719     C*
119600100719     C                   if        *in55 = *on        and
119700100719     C                             (%subst(vlrppt:8:1) = 'N' or
119800100719     C                              %subst(vlrppt:8:1) = 'A')
119900100719     C                   else
120000100719     C                   setoff                                       55
120100100719     C                   endif
120200100719     C*
120300010530     C                   ENDSR
120400010530     C***
120500100719
120600100719
120700100719
120800100719
120900100719      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
121000100719     C     exeBon        BEGSR
121100100719     C*
121200100719     C* Imposto i parametri x il *pgm d "bonifica"
121300100719     C                   EVAL      ElencoChar = *blanks
121400100719     C                   EVAL      TipoElenco = *blanks
121500100719     C                   EVAL      CharSost   = *blanks
121600100719     C                   EVAL      UpperCase  = '1'
121700100719     C                   EVAL      ChkNull    = '1'
121800100719     C                   EVAL      CharNull   = *blanks
121900100719     C*
122000100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
122100100719     C                   EVAL      TxtInOut   = VABRSD
122200100719     C                   EVAL      EsitoBon   = *blanks
122300100719     C*
122400100719     C                   CALL      'XCHKCHAR'
122500100719     C                   PARM                    TxtInOut
122600100719     C                   PARM                    ElencoChar
122700100719     C                   PARM                    TipoElenco
122800100719     C                   PARM                    CharSost
122900100719     C                   PARM                    UpperCase
123000100719     C                   PARM                    ChkNull
123100100719     C                   PARM                    CharNull
123200100719     C                   PARM                    EsitoBon
123300100719     C*
123400100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
123500100719     C                   IF        EsitoBon = '1'
123600100719     C                   EVAL      VABRSD = TxtInOut
123700100719     C                   ENDIF
123800100719     C*
123900100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
124000100719     C                   EVAL      TxtInOut   = VABIND
124100100719     C                   EVAL      EsitoBon   = *blanks
124200100719     C*
124300100719     C                   CALL      'XCHKCHAR'
124400100719     C                   PARM                    TxtInOut
124500100719     C                   PARM                    ElencoChar
124600100719     C                   PARM                    TipoElenco
124700100719     C                   PARM                    CharSost
124800100719     C                   PARM                    UpperCase
124900100719     C                   PARM                    ChkNull
125000100719     C                   PARM                    CharNull
125100100719     C                   PARM                    EsitoBon
125200100719     C*
125300100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
125400100719     C                   IF        EsitoBon = '1'
125500100719     C                   EVAL      VABIND = TxtInOut
125600100719     C                   ENDIF
125700100719     C*
125800100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
125900100719     C                   EVAL      TxtInOut   = VABLOD
126000100719     C                   EVAL      EsitoBon   = *blanks
126100100719     C*
126200100719     C                   CALL      'XCHKCHAR'
126300100719     C                   PARM                    TxtInOut
126400100719     C                   PARM                    ElencoChar
126500100719     C                   PARM                    TipoElenco
126600100719     C                   PARM                    CharSost
126700100719     C                   PARM                    UpperCase
126800100719     C                   PARM                    ChkNull
126900100719     C                   PARM                    CharNull
127000100719     C                   PARM                    EsitoBon
127100100719     C*
127200100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
127300100719     C                   IF        EsitoBon = '1'
127400100719     C                   EVAL      VABLOD = TxtInOut
127500100719     C                   ENDIF
127600100719     C*
127700100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
127800100719     C                   EVAL      TxtInOut   = VABNOT
127900100719     C                   EVAL      EsitoBon   = *blanks
128000100719     C                   EVAL      UpperCase  = *blanks
128100100719     C*
128200100719     C                   CALL      'XCHKCHAR'
128300100719     C                   PARM                    TxtInOut
128400100719     C                   PARM                    ElencoChar
128500100719     C                   PARM                    TipoElenco
128600100719     C                   PARM                    CharSost
128700100719     C                   PARM                    UpperCase
128800100719     C                   PARM                    ChkNull
128900100719     C                   PARM                    CharNull
129000100719     C                   PARM                    EsitoBon
129100100719     C*
129200100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
129300100719     C                   IF        EsitoBon = '1'
129400100719     C                   EVAL      VABNOT = TxtInOut
129500100719     C                   ENDIF
129600100719     C*
129700100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
129800100719     C                   EVAL      TxtInOut   = VABNT2
129900100719     C                   EVAL      EsitoBon   = *blanks
130000100719     C                   EVAL      UpperCase  = *blanks
130100100719     C*
130200100719     C                   CALL      'XCHKCHAR'
130300100719     C                   PARM                    TxtInOut
130400100719     C                   PARM                    ElencoChar
130500100719     C                   PARM                    TipoElenco
130600100719     C                   PARM                    CharSost
130700100719     C                   PARM                    UpperCase
130800100719     C                   PARM                    ChkNull
130900100719     C                   PARM                    CharNull
131000100719     C                   PARM                    EsitoBon
131100100719     C*
131200100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
131300100719     C                   IF        EsitoBon = '1'
131400100719     C                   EVAL      VABNT2 = TxtInOut
131500100719     C                   ENDIF
131600100719     C*
131700100719     C                   ENDSR
131800100719
131900100719
132000100719
132100100719     C*----------------------------------------------------*
132200100719     C*  CONTROLLO NUMERICITA' CAMPI
132300100719     C*----------------------------------------------------*
132400100719     C     CHKNUM        BEGSR
132500100719     C*
132600100719     C                   IF        PiDecChr = *blanks
132700100719     C  N54              EVAL      PiDecChr = '.'
132800100719     C                   ENDIF
132900100719     C*
133000100719     C                   callp(e)  UBISNUM_Check(PiStr
133100100719     C                                          :PiDecChr
133200100719     C                                          :PiVal
133300100719     C                                          :PiNum
133400100719     C                                          :PiInt)
133500100719     C*
133600100719     C                   IF        %error
133700100719     C                   EVAL      PiInt=*off
133800100719     C                   ENDIF
133900100719     C*
134000100719     C                   ENDSR
134100100719     C***
134200100719
134300010530
134400990910
134500000613     C     *inzsr        BEGSR
134600990910     C*
134700990910     C     *entry        plist
134800990920     C                   parm                    tivlrds
134900990921     C                   parm      wrkesito      esito
135000000724     C                   parm                    prmlit
135100000710     C                   parm                    prmfir
135200010606     C*
135300010606     C* CALCOLA LA DATA CORRENTE
135400100719     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
135500100719     C                   eval      datcor = %dec(%date() : *ISO)
135600000613     C*
135700000613     C                   ENDSR
135800000613     C***
135900990908
136000100719     Otis7t4p   E            riepilogo         2
136100990915     O                                              'Upload via Internet'
136200990915     O                                           +1 'Traduzione TIVIN00R -'
136300020913     O                                           +1 'FIVABWWR'
136400990915     O                                           +1 'Testate bolle'
136500990915     O          E            riepilogo   2
136600990915     O                       wrkdata
136700990915     O                       wrkora              +1
136800990915     O                       procname            +1
136900990915     O          E            riepilogo   2
137000990915     O                                              'Cliente..................:'
137100990915     O                       VABCCM        z     +1
137200990915     O          E            riepilogo   2
137300990920     O                                              'Riferimento Strategi.....:'
137400990920     O                       vlrhdl              +1
137500990915     O          E            riepilogo   2
137600990915     O                                              'Giusti...................:'
137700971022     O                       §CTROK        2   +  1
137800990915     O          E            riepilogo   2
137900990915     O                                              'Sbagliati e corretti.....:'
138000971022     O                       §CTRMO        2   +  1
138100990915     O          E            riepilogo   2
138200990915     O                                              'Sbagliati e scartati.....:'
138300971022     O                       §CTRNO        2   +  1
138400000613
138500100719     Otis7t4ps  E            testdett          2
138600000613     O                                              'Upload via Internet'
138700000613     O                                           +1 'Traduzione TIVIN00R -'
138800020913     O                                           +1 'FIVABWWR'
138900000613     O                                           +1 'Testate bolle'
139000000616     O          E            testdett    3
139100000613     O                                           +2 'N° rec'
139200000613     O                                           +3 'Anteprima contenuto'
139300000616     O          E            rigadett    2
139400000613     O                       rrnum               +2
139500000621     O                       recko               +3
139600000616     O          E            findett     2
139700000613     O                       wrkdata
139800000613     O                       wrkora              +1
139900000613     O                       procname            +1
140000000616     O          E            findett     2
140100000613     O                                              'Cliente..................:'
140200000613     O                       VABCCM        z     +1
140300000616     O          E            findett     2
140400000613     O                                              'Riferimento Strategi.....:'
140500000613     O                       vlrhdl              +1
140600000616     O          E            findett     2
140700000613     O                                              'Giusti...................:'
140800000613     O                       §CTROK        2   +  1
140900000616     O          E            findett     2
141000000613     O                                              'Sbagliati e corretti.....:'
141100000613     O                       §CTRMO        2   +  1
141200000616     O          E            findett     2
141300000613     O                                              'Sbagliati e scartati.....:'
141400000613     O                       §CTRNO        2   +  1
141500000512** CMD - COMANDI CL
141600100719OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
141700100719OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
141800100719DLTOVR FILE(TIS7T4P TIS7T4PS) LVL(*)
141900000512
142000000512
