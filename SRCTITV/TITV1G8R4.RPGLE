000100061106      /TITLE Upload via Internet: traduzione in EDIVAXWR.
000200081202     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600061106     FEDIVAXwr  O    E             DISK    usropn
000700050114     Ftis7t14p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050114     Ftis7t14ps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011123     D depcmd          s            150    INZ(*blanks)
003200061106     D wNomeFile       s             30    INZ(*blanks)
003300061106     D wCMR            s             35    INZ(*blanks)
003400061106     D wPosDaA         s              2    INZ(*blanks)
003500061106     D wPosDa          s              2  0 INZ(*zeros)
003600061106     D wLungA          s              2    INZ(*blanks)
003700061106     D wLung           s              2  0 INZ(*zeros)
003800010330     D*------------------
003900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004000010330     D*------------------
004100010330     D WLBDA8          DS                  INZ
004200010330     D  G08DAT                 1      8  0
004300010330     D  G08INV                 9     16  0
004400010330     D  G08ERR                17     17
004500010330     D  G08TGI                18     22  0
004600010612     D*
004700040524     D fnvaxaar        ds                  inz
004800040524     D  a_vaxatb               1      1
004900050114     D  a_vaxccm               2      8
005000050114     D  a_vaxlnp               9     11
005100050114     D  a_vaxaas              12     15
005200050114     D  a_vaxnrs              16     17
005300050114     D  a_vaxnsp              18     24
005400050114     D  a_vaxtmd              25     28
005500050114     D  a_vaxprr              29     32
005600050114     D  a_vaxtrk              33     34
005700050114     D  a_vaxdtt              35    534
005800010330
005900081202
006000081202     D*------------------
006100081202     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
006200081202     D*------------------
006300081202     D PiStr           S            100A   INZ
006400081202     D PiDecChr        S              1A   INZ
006500081202     D PiVal           S             63S30 INZ
006600081202     D PiInt           S               N   INZ
006700081202     D PiNum           S               N   INZ
006800081202
006900081202     D*------------------
007000081202     D* LINKING A DEFINIZIONI ESTERNE
007100081202     D*------------------
007200081202     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007300081202
007400081202
007500010330
007600010330
007700990915     C                   time                    wrkdata
007800990915     C                   time                    wrkora
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200010601     C*
008300010601     C                   exsr      opeini
008400040524     C                   exsr      rwvax
008500010601     C*
008600010601     C                   seton                                        lr
008700010601
008800010601
008900010601
009000010601
009100010601     C*--------------------------------------------------------
009200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009300010601     C*--------------------------------------------------------
009400010601     C     PREELA        BEGSR
009500010601     C*
009600010601     C* SE OCCORRE SPEDIRE IN FILIALE
009700010601     C                   if        invfil <> *zeros and
009800010601     C                             flgGiro = '0'
009900010601     C*
010000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010100010601     C                   eval      flgGiro = '1'
010200000724     C*
010300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010500010601     C     invfil        chain     azorg01l
010600000724     C                   if        %found
010700000724     C                   movel(p)  CMD(1)        depcmd
010800020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010900000724     C*
011000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011100011123     C                   Z-ADD     150           LENGH            15 5
011200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011300000724     C                   PARM                    depcmd
011400000724     C                   PARM                    LENGH
011500000724     C*
011600000724     C                   endif
011700000616     C*
011800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011900000616     C                   movel(p)  CMD(2)        depcmd
012000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012100000616     C*
012200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012300011123     C                   Z-ADD     150           LENGH            15 5
012400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    depcmd
012600000616     C                   PARM                    LENGH
012700000613     C*
012800050114     C                   if        not %open(tis7t14ps)
012900050114     C                   open      tis7t14ps
013000030407     C   50              except    testdett
013100000613     C                   endif
013200010601     C*
013300010601     C                   endif
013400010601     C*
013500010601     C                   ENDSR
013600010601     C***
013700010601
013800010601
013900010601
014000010601
014100010601
014200010601
014300010601
014400010601     C*--------------------------------------------------------
014500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014600010601     C*--------------------------------------------------------
014700010601     C     ENDELA        BEGSR
014800990915     C*
014900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015000000613     C*
015100050114     C                   if        %open(tis7t14ps)
015200030407     C   50              except    findett
015300050114     C                   close     tis7t14ps
015400000613     C                   endif
015500000616     C*
015600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015700011123     C                   Z-ADD     150           LENGH            15 5
015800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015900000616     C                   PARM                    CMD(3)
016000000616     C                   PARM                    LENGH
016100000616     C*
016200010601     C                   ENDSR
016300010601     C***
016400010601
016500010601
016600010601
016700010601
016800000613
016900000613     C*--------------------------------------------------------
017000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017100000613     C*--------------------------------------------------------
017200000621     C     STPR          BEGSR
017300000613     C*
017400050114     C                   if        not %open(tis7t14p)
017500050114     C                   open      tis7t14p
017600990915     C                   endif
017700990915     C*
017800030407     C   50              except    riepilogo
017900990915     C*
018000050114     C                   if        %open(tis7t14p)
018100050114     C                   close     tis7t14p
018200990914     C                   endif
018300000613     C*
018400000613     C                   ENDSR
018500000613     C***
018600000613
018700010330
018800010601
018900010601
019000010601
019100010330     C*--------------------------------------------------------
019200061106     C* RWVAX   LEGGE TIVIN00R E SCRIVE EDIVAXWF              *
019300010330     C*--------------------------------------------------------
019400040524     C     RWVAX         BEGSR
019500061106     C*
019600061106     C                   clear                   §CTDOK
019700061106     C                   clear                   §CTDNO
019800061106     C*
019900061106     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
020000061106     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
020100061106     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
020200061106     C                   if        *in60 = *on
020300010330     C                   if        not %open(tivin00r)
020400010330     C                   open      tivin00r
020500010330     C                   endif
020600061106     C                   if        not %open(edivaxwr)
020700061106     C                   open      edivaxwr
020800010330     C                   endif
020900010330     C*
021000010330     C                   DO        *HIVAL
021100010330     C*
021200010330     C                   READ      tivin00r                               70
021300010618     C*
021400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021500010618     C                   if        vinflg = '1'
021600010618     C                   eval      flgOk = '1'
021700010618     C                   endif
021800010618     C*
021900010330     C                   if        vindta > *blanks
022000010330     C                   add       1             rrnum
022100010330     C*
022200010601     C                   if        *in70 = *off and
022300010330     C                             (vinflg = *blanks
022400010330     C                              or vinflg = '0'
022500010330     C                              or vinflg = '2')
022600010711     C*
022700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022800010711     C                   if        vinflg = *blanks or vinflg = '0'
022900010711     C                   clear                   vinmsg
023000010711     C                   endif
023100010711     C*
023200040524     C                   eval      fnvaxaar = vindta
023300010601     C*
023400010330     C                   exsr      inzvar
023500010330     C                   exsr      defcam
023600040524     C                   exsr      impvax
023700010601     C*
023800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023900040524     C                   eval      depfil = VAXLNP
024000010601     C                   exsr      repfil
024100021010     C                   if        depfil = invfil
024200021014     C                   if        vlrpoi = 999
024300040524     C                   MOVE(P)   invfil        VAXFGS
024400021014     C                   else
024500040524     C                   MOVE(P)   vlrpoi        VAXFGS
024600021014     C                   endif
024700010601     C*
024800010601     C                   exsr      PREELA
024900061106     C*
025000061106     C* VALORIZZO CAMPI RELATIVI AL "CMR"
025100061106     C                   EVAL      VAXCMR = wCMR
025200061106     C                   EVAL      VAXCNT = 1
025300010601     C*
025400010604     C* Ebbene...
025500010604     C*
025600061106     C  N31              ADD       1             §CTDOK            7 0
025700061106     C   32
025800061106     COR 31              ADD       1             §CTDNO            7 0
025900061106     C  N31              WRITE     EDIVAX00
026000010604     C*
026100010604     C                   if        *in31 = *off and
026200010604     C                             *in32 = *off
026300010604     C                   eval      vinflg = '1'
026400010604     C                   else
026500010604     C                   eval      recko = vindta
026600050114     C                   if        %open(tis7t14ps)
026700030407     C   50              except    rigadett
026800010612     C                   endif
026900010604     C                   eval      vinflg = '2'
027000010604     C                   endif
027100010604     C                   endif
027200010601     C*
027300010604     C                   endif
027400010604     C*
027500010330     C                   else
027600010330     C                   eval      vinflg = '1'
027700010330     C                   endif
027800010601     C*
027900010601     C  N70              update    tivin000
028000010330     C*
028100010330     C  N70              ENDdo
028200010601     C
028300010601     C*
028400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028500010601     C                   if        cntNonEl = *zeros or
028600010601     C                             flgMulti = '0'
028700010330     C* Se non ci sono record con errori ...
028800061106     C                   if        §ctdno = 0 and
028900010601     C                             flgStato <> '2'
029000010330     C* ... restituisco esito OK.
029100010330     C                   eval      wrkesito = '0'
029200010330     C                   else
029300061106     C                   if        §ctdok > 0
029400010330     C                   eval      wrkesito = '1'
029500010330     C                   else
029600010615     C                   if        flgOk = '0'
029700010615     C                   eval      wrkesito = '2'
029800010615     C                   else
029900010615     C                   eval      wrkesito = '6'
030000010615     C                   endif
030100010330     C                   endif
030200010330     C                   endif
030300010601     C                   else
030400010601     C                   eval      wrkesito = '9'
030500010601     C                   endif
030600061106     C*
030700061106 X  1C                   else
030800061106     C                   eval      wrkesito = '8'
030900061106     C                   eval      flgGiro = '1'
031000061106 E  1C                   endif
031100010330     C*
031200010330     C                   if        %open(tivin00r)
031300010330     C                   close     tivin00r
031400010330     C                   endif
031500061106     C                   if        %open(edivaxwr)
031600061106     C                   close     edivaxwr
031700010330     C                   endif
031800010601     C*
031900010601     C                   if        vlrpoi <> 999
032000010601     C                   eval      invfil = vlrpoi
032100010601     C                   endif
032200010330     C*
032300061106     C                   if        §ctdok > 0
032400010601     C                             and invfil > *zeros
032500010330     C                   exsr      invio
032600010330     C                   endif
032700010601     C*
032800010612     C                   if        flgGiro = '1'
032900010601     C                   exsr      endela
033000010612     C                   endif
033100010330     C*
033200010330     C                   ENDSR
033300010330     C***
033400010330
033500010601
033600010601
033700010601
033800010601
033900010330     C*----------------------------------------------------*
034000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034100010330     C*----------------------------------------------------*
034200010330     C     INZVAR        BEGSR
034300010330     C*
034400010330     C*
034500010330     C                   ENDSR
034600010330     C*----------------------------------------------------*
034700010330     C*  IMPOSTAZIONE CAMPI COSTANTI
034800010330     C*----------------------------------------------------*
034900010330     C     DEFCAM        BEGSR
035000010330     C*
035100061106     C                   CLEAR                   EDIVAX00
035200080606     C*
035300080606     C                   IF        %trim(%subst(vlrppt:16:7)) <> *blanks
035400080606     C                   EVAL      PiStr=%trim(%subst(vlrppt:16:7))
035500080606     C                   EXSR      CHKNUM
035600080606     C                   IF        PiInt=*on
035700080606     C                   Z-ADD     PiVal         VAXCCM
035800080606     C                   ENDIF
035900080606     C                   ENDIF
036000080606     C                   IF        %trim(%subst(vlrppt:23:3)) <> *blanks
036100080606     C                   EVAL      PiStr=%trim(%subst(vlrppt:23:3))
036200080606     C                   EXSR      CHKNUM
036300080606     C                   IF        PiInt=*on
036400080606     C                   Z-ADD     PiVal         VAXLNP
036500080606     C                   ENDIF
036600080606     C                   ENDIF
036700010330     C*
036800010330     C                   ENDSR
036900010330     C*----------------------------------------------------*
037000061106     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAX)
037100010330     C*----------------------------------------------------*
037200040524     C     IMPVAX        BEGSR
037300010330     C*
037400010604     C                   SETOFF                                       3132
037500010612     C*
037600010612     C                   SETOFF                                       303132
037700010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
037800040524     C                   TESTN                   a_vaxccm             30
037900040524     C  N30              TESTN                   a_vaxccm               30
038000040524     C  N30              TESTN                   a_vaxccm                 30
038100040524     C                   MOVE      a_vaxccm      W001A             1
038200010612     C     *IN30         IFEQ      *OFF
038300010612     C     W001A         ORLT      '0'
038400010612     C                   SETON                                        31
038500040524     C                   Z-ADD     *zeros        VAXCCM
038600040524     C                   EVAL      vinmsg = 'VAXCCM'
038700010612     C                   ELSE
038800040524     C                   MOVE      a_vaxccm      VAXCCM
038900040524     C     VAXCCM        IFLE      *ZEROS
039000010612     C                   SETON                                        31
039100040524     C                   Z-ADD     *zeros        VAXCCM
039200040524     C                   EVAL      vinmsg = 'VAXCCM'
039300010612     C                   ENDIF
039400010612     C                   ENDIF
039500010612     C*
039600040524     C                   TESTN                   a_vaxlnp             30
039700040524     C  N30              TESTN                   a_vaxlnp               30
039800040524     C  N30              TESTN                   a_vaxlnp                 30
039900040524     C                   MOVE      a_vaxlnp      W001A             1
040000010612     C     *IN30         IFEQ      *OFF
040100010612     C     W001A         ORLT      '0'
040200010612     C                   SETON                                        31
040300040524     C                   Z-ADD     *zeros        VAXLNP
040400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
040500040524     C                             + ' ' + 'VAXLNP'
040600010612     C                   ELSE
040700040524     C                   MOVE      a_vaxlnp      VAXLNP
040800040524     C     VAXLNP        IFLE      *ZEROS
040900010612     C                   SETON                                        31
041000040524     C                   Z-ADD     *zeros        VAXLNP
041100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
041200040524     C                             + ' ' + 'VAXLNP'
041300010612     C                   ENDIF
041400010612     C                   ENDIF
041500010612     C*
041600040524     C                   TESTN                   a_vaxaas             30
041700040524     C  N30              TESTN                   a_vaxaas               30
041800040524     C  N30              TESTN                   a_vaxaas                 30
041900040524     C                   MOVE      a_vaxaas      W001A             1
042000010612     C     *IN30         IFEQ      *OFF
042100010612     C     W001A         ORLT      '0'
042200010612     C                   SETON                                        31
042300040524     C                   MOVEL(P)  datcor        VAXAAS
042400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
042500040524     C                             + ' ' + 'VAXAAS'
042600010612     C                   ELSE
042700040524     C                   MOVE      a_vaxaas      VAXAAS
042800010612     C                   ENDIF
042900010612     C*
043000040524     C                   TESTN                   a_vaxnrs             30
043100040524     C  N30              TESTN                   a_vaxnrs               30
043200040524     C  N30              TESTN                   a_vaxnrs                 30
043300040524     C                   MOVE      a_vaxnrs      W001A             1
043400010612     C     *IN30         IFEQ      *OFF
043500010612     C     W001A         ORLT      '0'
043600010612     C     W001A         ANDNE     *BLANK
043700010612     C                   SETON                                        31
043800040524     C                   Z-ADD     *zeros        VAXNRS
043900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
044000040524     C                             + ' ' + 'VAXNRS'
044100010612     C                   ELSE
044200040524     C                   MOVE      a_vaxnrs      VAXNRS
044300010612     C                   ENDIF
044400010612     C*
044500040524     C                   TESTN                   a_vaxnsp             30
044600040524     C  N30              TESTN                   a_vaxnsp               30
044700040524     C  N30              TESTN                   a_vaxnsp                 30
044800040524     C                   MOVE      a_vaxnsp      W001A             1
044900010612     C     *IN30         IFEQ      *OFF
045000010612     C     W001A         ORLT      '0'
045100010612     C                   SETON                                        31
045200040524     C                   Z-ADD     *zeros        VAXNSP
045300010612     C                   EVAL      vinmsg = %trimr(vinmsg)
045400040524     C                             + ' ' + 'VAXNSP'
045500010612     C                   ELSE
045600040524     C                   MOVE      a_vaxnsp      VAXNSP
045700010612     C                   ENDIF
045800040524     C*
045900040524     C                   TESTN                   a_vaxprr             30
046000040524     C  N30              TESTN                   a_vaxprr               30
046100040524     C  N30              TESTN                   a_vaxprr                 30
046200040524     C                   MOVE      a_vaxprr      W001A             1
046300040524     C     *IN30         IFEQ      *OFF
046400040524     C     W001A         ORLT      '0'
046500040524     C                   SETON                                        31
046600040524     C                   Z-ADD     *zeros        VAXPRR
046700040524     C                   EVAL      vinmsg = %trimr(vinmsg)
046800040524     C                             + ' ' + 'VAXPRR'
046900040524     C                   ELSE
047000040524     C                   MOVE      a_vaxprr      VAXPRR
047100040524     C                   ENDIF
047200010612     C*
047300010612     C* ALFABETICI
047400040524     C                   MOVE      a_vaxatb      VAXATB
047500040524     C                   MOVE      a_vaxtmd      VAXTMD
047600040524     C                   MOVE      a_vaxtrk      VAXTRK
047700090107     C                   IF        %subst(a_vaxdtt:1:1) = '.'
047800090107     C                   EVAL      %subst(a_vaxdtt:1:1) = ' '
047900090107     C                   ENDIF
048000040524     C                   MOVE      a_vaxdtt      VAXDTT
048100010330     C*
048200010330     C                   ENDSR
048300010612     C*----------------------------------------------------*
048400990920
048500010601
048600010601
048700010601
048800010601
048900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
049000010601     C     repfil        BEGSR
049100010601     C*
049200010601     C                   if        invfil = *zeros and
049300010601     C                             depfil > *zeros and
049400010613     C                             (vinflg = *blanks or
049500010613     C                              vinflg = *zeros)
049600010601     C
049700010601     C                   eval      invfil = depfil
049800010601     C                   endif
049900010601     C*
050000010601     C                   if        depfil <> invfil and
050100010601     C                             invfil > *zeros
050200010601     C                   eval      flgMulti = '1'
050300010601     C                   if        vinflg = *blanks
050400010601     C                   add       1             cntNonEl
050500010601     C                   endif
050600010601     C                   endif
050700010601     C*
050800010601     C                   if        vinflg = '2'
050900010601     C                   eval      flgStato = '2'
051000010601     C                   endif
051100010601     C*
051200010601     C                   ENDSR
051300010601     C***
051400010601
051500010601
051600010601
051700010330
051800010330
051900010330
052000990920      /TITLE Invio dei dati al punto operativo.
052100000613     C     invio         BEGSR
052200990920     C*
052300990920     C                   reset                   dscmz
052400010601     C                   move      invfil        cmzdst
052500990920     C                   eval      cmzfld = vlrfou
052600990920     C                   eval      cmzmbd = vlrhdl
052700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
052800000710     C                   if        prmfir = *blanks
052900061106     C                   eval      cmzfla = 'EDIVAX0F'
053000061106     C                   eval      cmzmba = 'EDIVAX0F'
053100000710     C                   else
053200000710     C                   eval      cmzfla = prmfir
053300000710     C                   eval      cmzmba = prmfir
053400000710     C                   endif
053500990920     C                   eval      cmznrr = *zeros
053600061106     C                   move      §ctdok        cmznrr
053700021003     C                   eval      cmzlba = vlrfl1
053800990920     C                   call(e)   'TIS711C'
053900990920     C                   parm                    dscmz
054000990921     C                   parm      *blanks       esito
054100990920     C                   if        %error
054200990920     C                             or cmzerr = '1'
054300990921     C                             or esito  = '1'
054400000710     C                   eval      wrkesito = '3'
054500990920     C                   endif
054600990920     C*
054700000613     C                   ENDSR
054800990910
054900010601
055000010601
055100010601
055200010601
055300010601      /TITLE Invio dei dati al punto operativo.
055400010601     C     opeini        BEGSR
055500010601     C*
055600010601     C* Inizializzo flag e contatori operativi
055700010601     C                   movel     '0'           flgGiro           1
055800010601     C                   movel     '0'           flgMulti          1
055900010601     C                   movel     '1'           flgStato          1
056000010615     C                   movel     '0'           flgOk             1
056100010601     C                   z-add     *zeros        cntNonEl         10 0
056200010601     C                   z-add     *zeros        depfil            3 0
056300010601     C                   z-add     *zeros        invfil            3 0
056400030407     C*
056500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
056600030407     C                   if        %subst(vlrppt:1:1) = 'S'
056700030407     C                   seton                                        50
056800030407     C                   else
056900030407     C                   setoff                                       50
057000030407     C                   endif
057100010601     C*
057200010601     C                   ENDSR
057300010601     C***
057400010601
057500081202
057600081202
057700081202
057800081202     C*----------------------------------------------------*
057900081202     C*  CONTROLLO NUMERICITA' CAMPI
058000081202     C*----------------------------------------------------*
058100081202     C     CHKNUM        BEGSR
058200081202     C*
058300081202     C                   IF        PiDecChr = *blanks
058400081202     C                   EVAL      PiDecChr = '.'
058500081202     C                   ENDIF
058600081202     C*
058700081202     C                   callp(e)  UBISNUM_Check(PiStr
058800081202     C                                          :PiDecChr
058900081202     C                                          :PiVal
059000081202     C                                          :PiNum
059100081202     C                                          :PiInt)
059200081202     C*
059300081202     C                   IF        %error
059400081202     C                   EVAL      PiInt=*off
059500081202     C                   ENDIF
059600081202     C*
059700081202     C                   ENDSR
059800081202     C***
059900081202
060000010601
060100010330
060200010330
060300000613     C     *inzsr        BEGSR
060400990910     C*
060500990910     C     *entry        plist
060600990920     C                   parm                    tivlrds
060700990921     C                   parm      wrkesito      esito
060800000724     C                   parm                    prmlit
060900000710     C                   parm                    prmfir
061000061106     C*
061100061106     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
061200061106     C                   move(p)   vlrMSG        wNomeFile
061300061106     C*
061400070830     C* Se nn forzato CMR nei parametri provo a reperirlo dal nome file corrente quindi =>
061500070830     C* reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
061600061106     C* del codice CMR dal nome del file importato.
061700070830     C                   if        %subst(vlrppt:6:10) <> *blanks
061800070830     C                   eval      wCMR = %trim(%subst(vlrppt:6:10))
061900070830     C                   seton                                        60
062000070830     C                   else
062100061106     C                   if        wNomeFile <> *blanks AND
062200061106     C                             vlrppt<>*blanks
062300061106     C                   if        %subst(vlrppt:1:1) = 'S'
062400061106     C                   seton                                        50
062500061106     C                   else
062600061106     C                   setoff                                       50
062700061106     C                   endif
062800081202     C                   if        %subst(vlrppt:2:4) > *zeros
062900061106     C                   eval      wPosDaA = %subst(vlrppt:2:2)
063000061106     C                   move(p)   wPosDaA       wPosDa
063100061106     C                   eval      wLungA  = %subst(vlrppt:4:2)
063200061106     C                   move(p)   wLungA        wLung
063300061106     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
063400081202     C                   else
063500081202     C                   eval      wCMR = 'CEVA - TIM'
063600081202     C                   endif
063700061106     C                   seton                                        60
063800061106     C                   else
063900061106     C                   setoff                                       60
064000061106     C                   endif
064100070830     C                   endif
064200010330     C*
064300010330     C* CALCOLA LA DATA CORRENTE
064400010330     C                   time                    wn14             14 0
064500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064600010330     C                   z-add     wn8           g08dat
064700010330     C                   z-add     *zeros        g08inv
064800010330     C                   movel     '0'           g08err
064900010330     C                   call      'XSRDA8'
065000010330     C                   parm                    wlbda8
065100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065200000613     C*
065300000613     C                   ENDSR
065400000613     C***
065500990908
065600050114     Otis7t14p  E            riepilogo         2
065700990915     O                                              'Upload via Internet'
065800990915     O                                           +1 'Traduzione TIVIN00R -'
065900061106     O                                           +1 'EDIVAXWR'
066000990915     O                                           +1 'Testate bolle'
066100990915     O          E            riepilogo   2
066200990915     O                       wrkdata
066300990915     O                       wrkora              +1
066400990915     O                       procname            +1
066500990915     O          E            riepilogo   2
066600990915     O                                              'Cliente..................:'
066700040524     O                       VAXCCM        z     +1
066800990915     O          E            riepilogo   2
066900990920     O                                              'Riferimento Strategi.....:'
067000990920     O                       vlrhdl              +1
067100990915     O          E            riepilogo   2
067200990915     O                                              'Giusti...................:'
067300061106     O                       §CTDOK        2   +  1
067400990915     O          E            riepilogo   2
067500990915     O                                              'Sbagliati e scartati.....:'
067600061106     O                       §CTDNO        2   +  1
067700000613
067800050114     Otis7t14ps E            testdett          2
067900000613     O                                              'Upload via Internet'
068000000613     O                                           +1 'Traduzione TIVIN00R -'
068100061106     O                                           +1 'EDIVAXWR'
068200000613     O                                           +1 'Testate bolle'
068300000616     O          E            testdett    3
068400000613     O                                           +2 'N° rec'
068500000613     O                                           +3 'Anteprima contenuto'
068600000616     O          E            rigadett    2
068700000613     O                       rrnum               +2
068800000621     O                       recko               +3
068900000616     O          E            findett     2
069000000613     O                       wrkdata
069100000613     O                       wrkora              +1
069200000613     O                       procname            +1
069300000616     O          E            findett     2
069400000613     O                                              'Cliente..................:'
069500040524     O                       VAXCCM        z     +1
069600000616     O          E            findett     2
069700000613     O                                              'Riferimento Strategi.....:'
069800000613     O                       vlrhdl              +1
069900000616     O          E            findett     2
070000000613     O                                              'Giusti...................:'
070100061106     O                       §CTDOK        2   +  1
070200000616     O          E            findett     2
070300000613     O                                              'Sbagliati e scartati.....:'
070400061106     O                       §CTDNO        2   +  1
070500000613** CMD - COMANDI CL
070600050114OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T14P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070700050114OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T14PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070800050114DLTOVR FILE(TIS7T14P TIS7T14PS) LVL(*)
070900010606
071000000613
