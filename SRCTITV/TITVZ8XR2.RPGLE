000100080925      /TITLE Upload via Internet: traduzione in FIVAXWWR.
000200080925     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080925     FFIVAXWwr  O    E             DISK    usropn
000700080925     FFIVAX01L  if   e           k disk    usropn
000800080925     F                                     prefix(w_)
000900080925     F                                     rename(fivax000:fivax001)
001000080925     F                                     extfile(LibFile)
001100050114     Ftis7t14p  O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300050114     Ftis7t14ps O    f  198        PRINTER usropn
001400000621     F                                     oflind(*inob)
001500000313     D*
001600000613     D*------------
001700000613     D* COMANDI
001800000613     D*------------
001900011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700010330     D esito           s              1
002800010330     D prmlit          s             10
002900010330     D prmfir          s             10
003000010330     D wrkesito        s                   like(esito)
003100010330     D wrkdata         s               d
003200010330     D wrkora          s               t
003300010330     D rrnum           s              6  0 INZ(*zeros)
003400010330     D recko           s            150    INZ(*blanks)
003500011123     D depcmd          s            150    INZ(*blanks)
003600080925     D LibFile         s             21A   inz
003700080925     D currSysNeta     s              8A   inz('*NULL')
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
005900080925
006000080925     D*------------------
006100080925     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
006200080925     D*------------------
006300080925     D PiStr           S            100A   INZ
006400080925     D PiDecChr        S              1A   INZ
006500080925     D PiVal           S             63S30 INZ
006600080925     D PiInt           S               N   INZ
006700080925     D PiNum           S               N   INZ
006800080925
006900080925
007000080925     D*------------------
007100080925     D* PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
007200080925     D*------------------
007300080925     D  PiOutSysNeta   S              8A
007400080925
007500080925
007600080925     D*------------------
007700080925     D* LINKING A DEFINIZIONI ESTERNE
007800080925     D*------------------
007900080925     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008000080925     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
008100080925
008200010330
008300010330
008400080925     C*
008500080925     C* Reperisco il sistema AS/400 corrente
008600080925     C                   callp     UBRTVNETA_Rtv(currSysNeta)
008700080925     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
008800080925     C                   eval      LibFile = 'FILTRAGRU/FIVAX01L'
008900080925     C                   else
009000080925     C                   eval      LibFile = 'FILTRAGRPF/FIVAX01L'
009100080925     C                   endif
009200080925     C*
009300080925     C                   if        not %open(FIVAX01L)
009400080925     C                   open      FIVAX01L
009500080925     C                   endif
009600080925     C*
009700990915     C                   time                    wrkdata
009800990915     C                   time                    wrkora
009900000913     C                   reset                   rrnum
010000990921     C                   reset                   esito
010100990921     C                   reset                   wrkesito
010200010601     C*
010300010601     C                   exsr      opeini
010400040524     C                   exsr      rwvax
010500080925     C*
010600080925     C                   if        %open(FIVAX01L)
010700080925     C                   close     FIVAX01L
010800080925     C                   endif
010900010601     C*
011000010601     C                   seton                                        lr
011100010601
011200010601
011300010601
011400010601
011500010601     C*--------------------------------------------------------
011600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011700010601     C*--------------------------------------------------------
011800010601     C     PREELA        BEGSR
011900010601     C*
012000010601     C* SE OCCORRE SPEDIRE IN FILIALE
012100010601     C                   if        invfil <> *zeros and
012200010601     C                             flgGiro = '0'
012300010601     C*
012400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012500010601     C                   eval      flgGiro = '1'
012600000724     C*
012700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012900010601     C     invfil        chain     azorg01l
013000000724     C                   if        %found
013100000724     C                   movel(p)  CMD(1)        depcmd
013200020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013300000724     C*
013400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013500011123     C                   Z-ADD     150           LENGH            15 5
013600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013700000724     C                   PARM                    depcmd
013800000724     C                   PARM                    LENGH
013900000724     C*
014000000724     C                   endif
014100000616     C*
014200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014300000616     C                   movel(p)  CMD(2)        depcmd
014400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014500000616     C*
014600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014700011123     C                   Z-ADD     150           LENGH            15 5
014800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014900000616     C                   PARM                    depcmd
015000000616     C                   PARM                    LENGH
015100000613     C*
015200050114     C                   if        not %open(tis7t14ps)
015300050114     C                   open      tis7t14ps
015400030407     C   50              except    testdett
015500000613     C                   endif
015600010601     C*
015700010601     C                   endif
015800010601     C*
015900010601     C                   ENDSR
016000010601     C***
016100010601
016200010601
016300010601
016400010601
016500010601
016600010601
016700010601
016800010601     C*--------------------------------------------------------
016900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017000010601     C*--------------------------------------------------------
017100010601     C     ENDELA        BEGSR
017200990915     C*
017300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017400000613     C*
017500050114     C                   if        %open(tis7t14ps)
017600030407     C   50              except    findett
017700050114     C                   close     tis7t14ps
017800000613     C                   endif
017900000616     C*
018000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018100011123     C                   Z-ADD     150           LENGH            15 5
018200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018300000616     C                   PARM                    CMD(3)
018400000616     C                   PARM                    LENGH
018500000616     C*
018600010601     C                   ENDSR
018700010601     C***
018800010601
018900010601
019000010601
019100010601
019200000613
019300000613     C*--------------------------------------------------------
019400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019500000613     C*--------------------------------------------------------
019600000621     C     STPR          BEGSR
019700000613     C*
019800050114     C                   if        not %open(tis7t14p)
019900050114     C                   open      tis7t14p
020000990915     C                   endif
020100990915     C*
020200030407     C   50              except    riepilogo
020300990915     C*
020400050114     C                   if        %open(tis7t14p)
020500050114     C                   close     tis7t14p
020600990914     C                   endif
020700000613     C*
020800000613     C                   ENDSR
020900000613     C***
021000000613
021100010330
021200010601
021300010601
021400010601
021500010330     C*--------------------------------------------------------
021600080925     C* RWVAX   LEGGE TIVIN00R E SCRIVE FIVAXWWF              *
021700010330     C*--------------------------------------------------------
021800040524     C     RWVAX         BEGSR
021900061106     C*
022000061106     C                   clear                   §CTDOK
022100061106     C                   clear                   §CTDNO
022200061106     C*
022300010330     C                   if        not %open(tivin00r)
022400010330     C                   open      tivin00r
022500010330     C                   endif
022600080925     C                   if        not %open(fivaxwwr)
022700080925     C                   open      fivaxwwr
022800010330     C                   endif
022900010330     C*
023000010330     C                   DO        *HIVAL
023100010330     C*
023200010330     C                   READ      tivin00r                               70
023300010618     C*
023400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023500010618     C                   if        vinflg = '1'
023600010618     C                   eval      flgOk = '1'
023700010618     C                   endif
023800010618     C*
023900010330     C                   if        vindta > *blanks
024000010330     C                   add       1             rrnum
024100010330     C*
024200010601     C                   if        *in70 = *off and
024300010330     C                             (vinflg = *blanks
024400010330     C                              or vinflg = '0'
024500010330     C                              or vinflg = '2')
024600010711     C*
024700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
024800010711     C                   if        vinflg = *blanks or vinflg = '0'
024900010711     C                   clear                   vinmsg
025000010711     C                   endif
025100010711     C*
025200040524     C                   eval      fnvaxaar = vindta
025300010601     C*
025400010330     C                   exsr      inzvar
025500010330     C                   exsr      defcam
025600040524     C                   exsr      impvax
025700010601     C*
025800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025900040524     C                   eval      depfil = VAXLNP
026000010601     C                   exsr      repfil
026100021010     C                   if        depfil = invfil
026200021014     C                   if        vlrpoi = 999
026300040524     C                   MOVE(P)   invfil        VAXFGS
026400021014     C                   else
026500040524     C                   MOVE(P)   vlrpoi        VAXFGS
026600021014     C                   endif
026700010601     C*
026800010601     C                   exsr      PREELA
026900010601     C*
027000010604     C* Ebbene...
027100010604     C*
027200061106     C  N31              ADD       1             §CTDOK            7 0
027300061106     C   32
027400061106     COR 31              ADD       1             §CTDNO            7 0
027500080925     C  N31              EXSR      WRIVAX
027600010604     C*
027700010604     C                   if        *in31 = *off and
027800010604     C                             *in32 = *off
027900010604     C                   eval      vinflg = '1'
028000010604     C                   else
028100010604     C                   eval      recko = vindta
028200050114     C                   if        %open(tis7t14ps)
028300030407     C   50              except    rigadett
028400010612     C                   endif
028500010604     C                   eval      vinflg = '2'
028600010604     C                   endif
028700010604     C                   endif
028800010601     C*
028900010604     C                   endif
029000010604     C*
029100010330     C                   else
029200010330     C                   eval      vinflg = '1'
029300010330     C                   endif
029400010601     C*
029500010601     C  N70              update    tivin000
029600010330     C*
029700010330     C  N70              ENDdo
029800010601     C
029900010601     C*
030000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030100010601     C                   if        cntNonEl = *zeros or
030200010601     C                             flgMulti = '0'
030300010330     C* Se non ci sono record con errori ...
030400061106     C                   if        §ctdno = 0 and
030500010601     C                             flgStato <> '2'
030600010330     C* ... restituisco esito OK.
030700010330     C                   eval      wrkesito = '0'
030800010330     C                   else
030900061106     C                   if        §ctdok > 0
031000010330     C                   eval      wrkesito = '1'
031100010330     C                   else
031200010615     C                   if        flgOk = '0'
031300010615     C                   eval      wrkesito = '2'
031400010615     C                   else
031500010615     C                   eval      wrkesito = '6'
031600010615     C                   endif
031700010330     C                   endif
031800010330     C                   endif
031900010601     C                   else
032000010601     C                   eval      wrkesito = '9'
032100010601     C                   endif
032200010330     C*
032300010330     C                   if        %open(tivin00r)
032400010330     C                   close     tivin00r
032500010330     C                   endif
032600080925     C                   if        %open(fivaxwwr)
032700080925     C                   close     fivaxwwr
032800010330     C                   endif
032900010601     C*
033000010601     C                   if        vlrpoi <> 999
033100010601     C                   eval      invfil = vlrpoi
033200010601     C                   endif
033300010330     C*
033400061106     C                   if        §ctdok > 0
033500010601     C                             and invfil > *zeros
033600010330     C                   exsr      invio
033700010330     C                   endif
033800010601     C*
033900010612     C                   if        flgGiro = '1'
034000010601     C                   exsr      endela
034100010612     C                   endif
034200010330     C*
034300010330     C                   ENDSR
034400010330     C***
034500010330
034600010601
034700010601
034800010601
034900010601
035000010330     C*----------------------------------------------------*
035100010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035200010330     C*----------------------------------------------------*
035300010330     C     INZVAR        BEGSR
035400010330     C*
035500010330     C*
035600010330     C                   ENDSR
035700010330     C*----------------------------------------------------*
035800010330     C*  IMPOSTAZIONE CAMPI COSTANTI
035900010330     C*----------------------------------------------------*
036000010330     C     DEFCAM        BEGSR
036100010330     C*
036200080925     C                   CLEAR                   FIVAX000
036300080606     C*
036400080606     C                   IF        %trim(%subst(vlrppt:16:7)) <> *blanks
036500080606     C                   EVAL      PiStr=%trim(%subst(vlrppt:16:7))
036600080606     C                   EXSR      CHKNUM
036700080606     C                   IF        PiInt=*on
036800080606     C                   Z-ADD     PiVal         VAXCCM
036900080606     C                   ENDIF
037000080606     C                   ENDIF
037100080606     C                   IF        %trim(%subst(vlrppt:23:3)) <> *blanks
037200080606     C                   EVAL      PiStr=%trim(%subst(vlrppt:23:3))
037300080606     C                   EXSR      CHKNUM
037400080606     C                   IF        PiInt=*on
037500080606     C                   Z-ADD     PiVal         VAXLNP
037600080606     C                   ENDIF
037700080606     C                   ENDIF
037800010330     C*
037900010330     C                   ENDSR
038000010330     C*----------------------------------------------------*
038100080925     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
038200010330     C*----------------------------------------------------*
038300040524     C     IMPVAX        BEGSR
038400010330     C*
038500010604     C                   SETOFF                                       3132
038600010612     C*
038700010612     C                   SETOFF                                       303132
038800010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
038900040524     C                   TESTN                   a_vaxccm             30
039000040524     C  N30              TESTN                   a_vaxccm               30
039100040524     C  N30              TESTN                   a_vaxccm                 30
039200040524     C                   MOVE      a_vaxccm      W001A             1
039300010612     C     *IN30         IFEQ      *OFF
039400010612     C     W001A         ORLT      '0'
039500010612     C                   SETON                                        31
039600040524     C                   Z-ADD     *zeros        VAXCCM
039700040524     C                   EVAL      vinmsg = 'VAXCCM'
039800010612     C                   ELSE
039900040524     C                   MOVE      a_vaxccm      VAXCCM
040000040524     C     VAXCCM        IFLE      *ZEROS
040100010612     C                   SETON                                        31
040200040524     C                   Z-ADD     *zeros        VAXCCM
040300040524     C                   EVAL      vinmsg = 'VAXCCM'
040400010612     C                   ENDIF
040500010612     C                   ENDIF
040600010612     C*
040700040524     C                   TESTN                   a_vaxlnp             30
040800040524     C  N30              TESTN                   a_vaxlnp               30
040900040524     C  N30              TESTN                   a_vaxlnp                 30
041000040524     C                   MOVE      a_vaxlnp      W001A             1
041100010612     C     *IN30         IFEQ      *OFF
041200010612     C     W001A         ORLT      '0'
041300010612     C                   SETON                                        31
041400040524     C                   Z-ADD     *zeros        VAXLNP
041500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
041600040524     C                             + ' ' + 'VAXLNP'
041700010612     C                   ELSE
041800040524     C                   MOVE      a_vaxlnp      VAXLNP
041900040524     C     VAXLNP        IFLE      *ZEROS
042000010612     C                   SETON                                        31
042100040524     C                   Z-ADD     *zeros        VAXLNP
042200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
042300040524     C                             + ' ' + 'VAXLNP'
042400010612     C                   ENDIF
042500010612     C                   ENDIF
042600010612     C*
042700040524     C                   TESTN                   a_vaxaas             30
042800040524     C  N30              TESTN                   a_vaxaas               30
042900040524     C  N30              TESTN                   a_vaxaas                 30
043000040524     C                   MOVE      a_vaxaas      W001A             1
043100010612     C     *IN30         IFEQ      *OFF
043200010612     C     W001A         ORLT      '0'
043300010612     C                   SETON                                        31
043400040524     C                   MOVEL(P)  datcor        VAXAAS
043500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
043600040524     C                             + ' ' + 'VAXAAS'
043700010612     C                   ELSE
043800040524     C                   MOVE      a_vaxaas      VAXAAS
043900010612     C                   ENDIF
044000010612     C*
044100040524     C                   TESTN                   a_vaxnrs             30
044200040524     C  N30              TESTN                   a_vaxnrs               30
044300040524     C  N30              TESTN                   a_vaxnrs                 30
044400040524     C                   MOVE      a_vaxnrs      W001A             1
044500010612     C     *IN30         IFEQ      *OFF
044600010612     C     W001A         ORLT      '0'
044700010612     C     W001A         ANDNE     *BLANK
044800010612     C                   SETON                                        31
044900040524     C                   Z-ADD     *zeros        VAXNRS
045000010612     C                   EVAL      vinmsg = %trimr(vinmsg)
045100040524     C                             + ' ' + 'VAXNRS'
045200010612     C                   ELSE
045300040524     C                   MOVE      a_vaxnrs      VAXNRS
045400010612     C                   ENDIF
045500010612     C*
045600040524     C                   TESTN                   a_vaxnsp             30
045700040524     C  N30              TESTN                   a_vaxnsp               30
045800040524     C  N30              TESTN                   a_vaxnsp                 30
045900040524     C                   MOVE      a_vaxnsp      W001A             1
046000010612     C     *IN30         IFEQ      *OFF
046100010612     C     W001A         ORLT      '0'
046200010612     C                   SETON                                        31
046300040524     C                   Z-ADD     *zeros        VAXNSP
046400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
046500040524     C                             + ' ' + 'VAXNSP'
046600010612     C                   ELSE
046700040524     C                   MOVE      a_vaxnsp      VAXNSP
046800010612     C                   ENDIF
046900040524     C*
047000040524     C                   TESTN                   a_vaxprr             30
047100040524     C  N30              TESTN                   a_vaxprr               30
047200040524     C  N30              TESTN                   a_vaxprr                 30
047300040524     C                   MOVE      a_vaxprr      W001A             1
047400040524     C     *IN30         IFEQ      *OFF
047500040524     C     W001A         ORLT      '0'
047600040524     C                   SETON                                        31
047700040524     C                   Z-ADD     *zeros        VAXPRR
047800040524     C                   EVAL      vinmsg = %trimr(vinmsg)
047900040524     C                             + ' ' + 'VAXPRR'
048000040524     C                   ELSE
048100040524     C                   MOVE      a_vaxprr      VAXPRR
048200040524     C                   ENDIF
048300010612     C*
048400010612     C* ALFABETICI
048500040524     C                   MOVE      a_vaxatb      VAXATB
048600040524     C                   MOVE      a_vaxtmd      VAXTMD
048700040524     C                   MOVE      a_vaxtrk      VAXTRK
048800040524     C                   MOVE      a_vaxdtt      VAXDTT
048900010330     C*
049000010330     C                   ENDSR
049100010612     C*----------------------------------------------------*
049200990920
049300010601
049400080925
049500080925
049600080925     C*--------------------------------------------------------
049700080925     C* WRIVAX - EFFETTUA CONTROLLI D "NN DOPPIETA" E SCARICA IL BUFFER
049800080925     C*--------------------------------------------------------
049900080925     C     WRIVAX        BEGSR
050000080925     C*
050100080925     C* Verifico che stesso packing-list nn già presente
050200080925     C     KEYvax01_C    SETLL     fivax01l
050300080925     C                   IF        %equal(fivax01l)
050400080925     C                   ELSE
050500080925     C                   WRITE     FIVAX000
050600080925     C                   ENDIF
050700080925     C*
050800080925     C                   ENDSR
050900080925     C***
051000080925
051100010601
051200010601
051300010601
051400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051500010601     C     repfil        BEGSR
051600010601     C*
051700010601     C                   if        invfil = *zeros and
051800010601     C                             depfil > *zeros and
051900010613     C                             (vinflg = *blanks or
052000010613     C                              vinflg = *zeros)
052100010601     C
052200010601     C                   eval      invfil = depfil
052300010601     C                   endif
052400010601     C*
052500010601     C                   if        depfil <> invfil and
052600010601     C                             invfil > *zeros
052700010601     C                   eval      flgMulti = '1'
052800010601     C                   if        vinflg = *blanks
052900010601     C                   add       1             cntNonEl
053000010601     C                   endif
053100010601     C                   endif
053200010601     C*
053300010601     C                   if        vinflg = '2'
053400010601     C                   eval      flgStato = '2'
053500010601     C                   endif
053600010601     C*
053700010601     C                   ENDSR
053800010601     C***
053900010601
054000010601
054100010601
054200010330
054300010330
054400010330
054500990920      /TITLE Invio dei dati al punto operativo.
054600000613     C     invio         BEGSR
054700990920     C*
054800990920     C                   reset                   dscmz
054900010601     C                   move      invfil        cmzdst
055000990920     C                   eval      cmzfld = vlrfou
055100990920     C                   eval      cmzmbd = vlrhdl
055200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
055300000710     C                   if        prmfir = *blanks
055400080925     C                   eval      cmzfla = 'FIVAX00F'
055500080925     C                   eval      cmzmba = 'FIVAX00F'
055600000710     C                   else
055700000710     C                   eval      cmzfla = prmfir
055800000710     C                   eval      cmzmba = prmfir
055900000710     C                   endif
056000990920     C                   eval      cmznrr = *zeros
056100061106     C                   move      §ctdok        cmznrr
056200021003     C                   eval      cmzlba = vlrfl1
056300990920     C                   call(e)   'TIS711C'
056400990920     C                   parm                    dscmz
056500990921     C                   parm      *blanks       esito
056600990920     C                   if        %error
056700990920     C                             or cmzerr = '1'
056800990921     C                             or esito  = '1'
056900000710     C                   eval      wrkesito = '3'
057000990920     C                   endif
057100990920     C*
057200000613     C                   ENDSR
057300990910
057400010601
057500010601
057600010601
057700010601
057800010601      /TITLE Invio dei dati al punto operativo.
057900010601     C     opeini        BEGSR
058000010601     C*
058100010601     C* Inizializzo flag e contatori operativi
058200010601     C                   movel     '0'           flgGiro           1
058300010601     C                   movel     '0'           flgMulti          1
058400010601     C                   movel     '1'           flgStato          1
058500010615     C                   movel     '0'           flgOk             1
058600010601     C                   z-add     *zeros        cntNonEl         10 0
058700010601     C                   z-add     *zeros        depfil            3 0
058800010601     C                   z-add     *zeros        invfil            3 0
058900030407     C*
059000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
059100030407     C                   if        %subst(vlrppt:1:1) = 'S'
059200030407     C                   seton                                        50
059300030407     C                   else
059400030407     C                   setoff                                       50
059500030407     C                   endif
059600010601     C*
059700010601     C                   ENDSR
059800010601     C***
059900080606
060000080606
060100080606
060200080606     C*----------------------------------------------------*
060300080606     C*  CONTROLLO NUMERICITA' CAMPI
060400080606     C*----------------------------------------------------*
060500080606     C     CHKNUM        BEGSR
060600080606     C*
060700080606     C                   IF        PiDecChr = *blanks
060800080606     C                   EVAL      PiDecChr = ','
060900080606     C                   ENDIF
061000080606     C*
061100080925     C                   callp     UBISNUM_Check(PiStr
061200080925     C                                          :PiDecChr
061300080925     C                                          :PiVal
061400080925     C                                          :PiNum
061500080925     C                                          :PiInt)
061600080606     C*
061700080606     C                   ENDSR
061800080606     C***
061900010601
062000010601
062100010601
062200010330
062300010330
062400000613     C     *inzsr        BEGSR
062500990910     C*
062600990910     C     *entry        plist
062700990920     C                   parm                    tivlrds
062800990921     C                   parm      wrkesito      esito
062900000724     C                   parm                    prmlit
063000000710     C                   parm                    prmfir
063100080925     C*
063200080925     C* FIVAX01L - completa
063300080925     C     KEYvax01_C    klist
063400080925     C                   kfld                    VAXFGS
063500080925     C                   kfld                    VAXCCM
063600080925     C                   kfld                    VAXAAS
063700080925     C                   kfld                    VAXLNP
063800080925     C                   kfld                    VAXNRS
063900080925     C                   kfld                    VAXNSP
064000080925     C                   kfld                    VAXTMD
064100080925     C                   kfld                    VAXPRR
064200080925     C                   kfld                    VAXTRK
064300010330     C*
064400010330     C* CALCOLA LA DATA CORRENTE
064500010330     C                   time                    wn14             14 0
064600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064700010330     C                   z-add     wn8           g08dat
064800010330     C                   z-add     *zeros        g08inv
064900010330     C                   movel     '0'           g08err
065000010330     C                   call      'XSRDA8'
065100010330     C                   parm                    wlbda8
065200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065300000613     C*
065400000613     C                   ENDSR
065500000613     C***
065600990908
065700050114     Otis7t14p  E            riepilogo         2
065800990915     O                                              'Upload via Internet'
065900990915     O                                           +1 'Traduzione TIVIN00R -'
066000080925     O                                           +1 'FIVAXWWR'
066100990915     O                                           +1 'Testate bolle'
066200990915     O          E            riepilogo   2
066300990915     O                       wrkdata
066400990915     O                       wrkora              +1
066500990915     O                       procname            +1
066600990915     O          E            riepilogo   2
066700990915     O                                              'Cliente..................:'
066800040524     O                       VAXCCM        z     +1
066900990915     O          E            riepilogo   2
067000990920     O                                              'Riferimento Strategi.....:'
067100990920     O                       vlrhdl              +1
067200990915     O          E            riepilogo   2
067300990915     O                                              'Giusti...................:'
067400061106     O                       §CTDOK        2   +  1
067500990915     O          E            riepilogo   2
067600990915     O                                              'Sbagliati e scartati.....:'
067700061106     O                       §CTDNO        2   +  1
067800000613
067900050114     Otis7t14ps E            testdett          2
068000000613     O                                              'Upload via Internet'
068100000613     O                                           +1 'Traduzione TIVIN00R -'
068200080925     O                                           +1 'FIVAXWWR'
068300000613     O                                           +1 'Testate bolle'
068400000616     O          E            testdett    3
068500000613     O                                           +2 'N° rec'
068600000613     O                                           +3 'Anteprima contenuto'
068700000616     O          E            rigadett    2
068800000613     O                       rrnum               +2
068900000621     O                       recko               +3
069000000616     O          E            findett     2
069100000613     O                       wrkdata
069200000613     O                       wrkora              +1
069300000613     O                       procname            +1
069400000616     O          E            findett     2
069500000613     O                                              'Cliente..................:'
069600040524     O                       VAXCCM        z     +1
069700000616     O          E            findett     2
069800000613     O                                              'Riferimento Strategi.....:'
069900000613     O                       vlrhdl              +1
070000000616     O          E            findett     2
070100000613     O                                              'Giusti...................:'
070200061106     O                       §CTDOK        2   +  1
070300000616     O          E            findett     2
070400000613     O                                              'Sbagliati e scartati.....:'
070500061106     O                       §CTDNO        2   +  1
070600000613** CMD - COMANDI CL
070700050114OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T14P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070800050114OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T14PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070900050114DLTOVR FILE(TIS7T14P TIS7T14PS) LVL(*)
071000010606
071100000613
