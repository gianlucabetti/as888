000100020916      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200090611     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020916     FFIVATwwr  O    E             DISK    usropn
000700010612     Ftis7t6p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010612     Ftis7t6ps  O    f  198        PRINTER usropn
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
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000010612     D*
004100010612     D fnvataar        ds                  inz
004200010612     D  a_vatatb               1      1
004300010612     D  a_vatccm               3      9
004400010612     D  a_vatlnp              11     13
004500010612     D  a_vataas              15     18
004600010612     D  a_vatnrs              20     21
004700010612     D  a_vatnsp              23     29
004800010612     D  a_vattrc              30     30
004900010612     D  a_vatnot              31     65
005000010330
005100090611
005200090611     D*------------------
005300090611     D* LINKING A DEFINIZIONI ESTERNE
005400090611     D*------------------
005500090611     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005600090611     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005700090611
005800010330
005900010330
006000990915     C                   time                    wrkdata
006100990915     C                   time                    wrkora
006200000913     C                   reset                   rrnum
006300990921     C                   reset                   esito
006400990921     C                   reset                   wrkesito
006500010601     C*
006600010601     C                   exsr      opeini
006700010612     C                   exsr      rwvat
006800010601     C*
006900010601     C                   seton                                        lr
007000010601
007100010601
007200010601
007300010601
007400010601     C*--------------------------------------------------------
007500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007600010601     C*--------------------------------------------------------
007700010601     C     PREELA        BEGSR
007800010601     C*
007900010601     C* SE OCCORRE SPEDIRE IN FILIALE
008000010601     C                   if        invfil <> *zeros and
008100010601     C                             flgGiro = '0'
008200010601     C*
008300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008400010601     C                   eval      flgGiro = '1'
008500000724     C*
008600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800010601     C     invfil        chain     azorg01l
008900000724     C                   if        %found
009000000724     C                   movel(p)  CMD(1)        depcmd
009100020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009200000724     C*
009300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009400011123     C                   Z-ADD     150           LENGH            15 5
009500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009600000724     C                   PARM                    depcmd
009700000724     C                   PARM                    LENGH
009800000724     C*
009900000724     C                   endif
010000000616     C*
010100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010200000616     C                   movel(p)  CMD(2)        depcmd
010300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010400000616     C*
010500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010600011123     C                   Z-ADD     150           LENGH            15 5
010700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010800000616     C                   PARM                    depcmd
010900000616     C                   PARM                    LENGH
011000000613     C*
011100010612     C                   if        not %open(tis7t6ps)
011200010612     C                   open      tis7t6ps
011300030407     C   50              except    testdett
011400000613     C                   endif
011500010601     C*
011600010601     C                   endif
011700010601     C*
011800010601     C                   ENDSR
011900010601     C***
012000010601
012100010601
012200010601
012300010601
012400010601
012500010601
012600010601
012700010601     C*--------------------------------------------------------
012800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012900010601     C*--------------------------------------------------------
013000010601     C     ENDELA        BEGSR
013100990915     C*
013200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013300000613     C*
013400010612     C                   if        %open(tis7t6ps)
013500030407     C   50              except    findett
013600010612     C                   close     tis7t6ps
013700000613     C                   endif
013800000616     C*
013900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014000011123     C                   Z-ADD     150           LENGH            15 5
014100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014200000616     C                   PARM                    CMD(3)
014300000616     C                   PARM                    LENGH
014400000616     C*
014500010601     C                   ENDSR
014600010601     C***
014700010601
014800010601
014900010601
015000010601
015100000613
015200000613     C*--------------------------------------------------------
015300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015400000613     C*--------------------------------------------------------
015500000621     C     STPR          BEGSR
015600000613     C*
015700010612     C                   if        not %open(tis7t6p)
015800010612     C                   open      tis7t6p
015900990915     C                   endif
016000990915     C*
016100030407     C   50              except    riepilogo
016200990915     C*
016300010612     C                   if        %open(tis7t6p)
016400010612     C                   close     tis7t6p
016500990914     C                   endif
016600000613     C*
016700000613     C                   ENDSR
016800000613     C***
016900000613
017000010330
017100010601
017200010601
017300010601
017400010330     C*--------------------------------------------------------
017500020916     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
017600010330     C*--------------------------------------------------------
017700010612     C     RWVAT         BEGSR
017800010330     C*
017900010330     C                   if        not %open(tivin00r)
018000010330     C                   open      tivin00r
018100010330     C                   endif
018200020916     C                   if        not %open(fivatwwr)
018300020916     C                   open      fivatwwr
018400010330     C                   endif
018500010330     C*
018600010604     C                   clear                   §CTROK
018700010604     C                   clear                   §CTRMO
018800010604     C                   clear                   §CTRNO
018900010330     C*
019000010330     C                   DO        *HIVAL
019100010330     C*
019200010330     C                   READ      tivin00r                               70
019300010618     C*
019400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019500010618     C                   if        vinflg = '1'
019600010618     C                   eval      flgOk = '1'
019700010618     C                   endif
019800010618     C*
019900010330     C                   if        vindta > *blanks
020000010330     C                   add       1             rrnum
020100010330     C*
020200010601     C                   if        *in70 = *off and
020300010330     C                             (vinflg = *blanks
020400010330     C                              or vinflg = '0'
020500010330     C                              or vinflg = '2')
020600010711     C*
020700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020800010711     C                   if        vinflg = *blanks or vinflg = '0'
020900010711     C                   clear                   vinmsg
021000010711     C                   endif
021100010711     C*
021200010613     C                   eval      fnvataar = vindta
021300010601     C*
021400010330     C                   exsr      inzvar
021500010330     C                   exsr      defcam
021600010612     C                   exsr      impvat
021700010601     C*
021800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021900010612     C                   eval      depfil = VATLNP
022000010601     C                   exsr      repfil
022100021010     C                   if        depfil = invfil
022200021014     C                   if        vlrpoi = 999
022300021010     C                   MOVE(P)   invfil        VATFGS
022400021014     C                   else
022500021014     C                   MOVE(P)   vlrpoi        VATFGS
022600021014     C                   endif
022700010601     C*
022800010601     C                   exsr      PREELA
022900090611     C*
023000090611     C* Forzatura parametri anagrafica
023100090611     C                   IF        %subst(vlrppt:2:7) <> *blanks
023200090611     C                   EVAL      PiStr=%trim(%subst(vlrppt:2:7))
023300090611     C                   EXSR      CHKNUM
023400090611     C                   IF        PiInt=*on
023500090611     C                   Z-ADD     PiVal         VATCCM
023600090611     C                   ENDIF
023700090611     C                   ENDIF
023800090611     C*
023900090611     C                   IF        %subst(vlrppt:9:3) <> *blanks
024000090611     C                   EVAL      PiStr=%trim(%subst(vlrppt:9:3))
024100090611     C                   EXSR      CHKNUM
024200090611     C                   IF        PiInt=*on
024300090611     C                   Z-ADD     PiVal         VATLNP
024400090611     C                   ENDIF
024500090611     C                   ENDIF
024600090611     C*
024700010601     C*
024800010604     C* Ebbene...
024900010604     C*
025000010604     C  N31              ADD       1             §CTROK            7 0
025100010604     C   32              ADD       1             §CTRMO            7 0
025200010604     C   31              ADD       1             §CTRNO            7 0
025300020916     C  N31              WRITE     FIVAT000
025400010604     C*
025500010604     C                   if        *in31 = *off and
025600010604     C                             *in32 = *off
025700010604     C                   eval      vinflg = '1'
025800010604     C                   else
025900010604     C                   eval      recko = vindta
026000010612     C                   if        %open(tis7t6ps)
026100030407     C   50              except    rigadett
026200010612     C                   endif
026300010604     C                   eval      vinflg = '2'
026400010604     C                   endif
026500010604     C                   endif
026600010601     C*
026700010604     C                   endif
026800010604     C*
026900010330     C                   else
027000010330     C                   eval      vinflg = '1'
027100010330     C                   endif
027200010601     C*
027300010601     C  N70              update    tivin000
027400010330     C*
027500010330     C  N70              ENDdo
027600010601     C
027700010601     C*
027800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027900010601     C                   if        cntNonEl = *zeros or
028000010601     C                             flgMulti = '0'
028100010330     C* Se non ci sono record con errori ...
028200010601     C                   if        §ctrno = 0 and
028300010604     C                             §ctrmo = 0 and
028400010601     C                             flgStato <> '2'
028500010330     C* ... restituisco esito OK.
028600010330     C                   eval      wrkesito = '0'
028700010330     C                   else
028800010330     C                   if        §ctrok > 0
028900010330     C                   eval      wrkesito = '1'
029000010330     C                   else
029100010615     C                   if        flgOk = '0'
029200010615     C                   eval      wrkesito = '2'
029300010615     C                   else
029400010615     C                   eval      wrkesito = '6'
029500010615     C                   endif
029600010330     C                   endif
029700010330     C                   endif
029800010601     C                   else
029900010601     C                   eval      wrkesito = '9'
030000010601     C                   endif
030100010330     C*
030200010330     C                   if        %open(tivin00r)
030300010330     C                   close     tivin00r
030400010330     C                   endif
030500020916     C                   if        %open(fivatwwr)
030600020916     C                   close     fivatwwr
030700010330     C                   endif
030800010601     C*
030900010601     C                   if        vlrpoi <> 999
031000010601     C                   eval      invfil = vlrpoi
031100010601     C                   endif
031200010330     C*
031300010330     C                   if        §ctrok > 0
031400010601     C                             and invfil > *zeros
031500010330     C                   exsr      invio
031600010330     C                   endif
031700010601     C*
031800010612     C                   if        flgGiro = '1'
031900010601     C                   exsr      endela
032000010612     C                   endif
032100010330     C*
032200010330     C                   ENDSR
032300010330     C***
032400010330
032500010601
032600010601
032700010601
032800010601
032900010330     C*----------------------------------------------------*
033000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033100010330     C*----------------------------------------------------*
033200010330     C     INZVAR        BEGSR
033300010330     C*
033400010330     C*
033500010330     C                   ENDSR
033600010330     C*----------------------------------------------------*
033700010330     C*  IMPOSTAZIONE CAMPI COSTANTI
033800010330     C*----------------------------------------------------*
033900010330     C     DEFCAM        BEGSR
034000010330     C*
034100020916     C                   CLEAR                   FIVAT000
034200010330     C*
034300010330     C                   ENDSR
034400010330     C*----------------------------------------------------*
034500020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
034600010330     C*----------------------------------------------------*
034700010612     C     IMPVAT        BEGSR
034800010330     C*
034900010604     C                   SETOFF                                       3132
035000010612     C*
035100010612     C                   SETOFF                                       303132
035200010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
035300010612     C                   TESTN                   a_vatccm             30
035400010612     C  N30              TESTN                   a_vatccm               30
035500010612     C  N30              TESTN                   a_vatccm                 30
035600010612     C                   MOVE      a_vatccm      W001A             1
035700010612     C     *IN30         IFEQ      *OFF
035800010612     C     W001A         ORLT      '0'
035900010612     C                   SETON                                        31
036000010612     C                   Z-ADD     *zeros        VATCCM
036100010612     C                   EVAL      vinmsg = 'VATCCM'
036200010612     C                   ELSE
036300010612     C                   MOVE      a_vatccm      VATCCM
036400010612     C     VATCCM        IFLE      *ZEROS
036500010612     C                   SETON                                        31
036600010612     C                   Z-ADD     *zeros        VATCCM
036700010612     C                   EVAL      vinmsg = 'VATCCM'
036800010612     C                   ENDIF
036900010612     C                   ENDIF
037000010612     C*
037100010612     C                   TESTN                   a_vatlnp             30
037200010612     C  N30              TESTN                   a_vatlnp               30
037300010612     C  N30              TESTN                   a_vatlnp                 30
037400010612     C                   MOVE      a_vatlnp      W001A             1
037500010612     C     *IN30         IFEQ      *OFF
037600010612     C     W001A         ORLT      '0'
037700010612     C                   SETON                                        31
037800010612     C                   Z-ADD     *zeros        VATLNP
037900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
038000021010     C                             + ' ' + 'VATLNP'
038100010612     C                   ELSE
038200010612     C                   MOVE      a_vatlnp      VATLNP
038300010612     C     VATLNP        IFLE      *ZEROS
038400010612     C                   SETON                                        31
038500010612     C                   Z-ADD     *zeros        VATLNP
038600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
038700021010     C                             + ' ' + 'VATLNP'
038800010612     C                   ENDIF
038900010612     C                   ENDIF
039000010612     C*
039100010612     C                   TESTN                   a_vataas             30
039200010612     C  N30              TESTN                   a_vataas               30
039300010612     C  N30              TESTN                   a_vataas                 30
039400010612     C                   MOVE      a_vataas      W001A             1
039500010612     C     *IN30         IFEQ      *OFF
039600010612     C     W001A         ORLT      '0'
039700010612     C                   SETON                                        31
039800010612     C                   MOVEL(P)  datcor        VATAAS
039900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
040000010612     C                             + ' ' + 'VATAAS'
040100010612     C                   ELSE
040200010612     C                   MOVE      a_vataas      VATAAS
040300010612     C                   ENDIF
040400010612     C*
040500010612     C                   TESTN                   a_vatnrs             30
040600010612     C  N30              TESTN                   a_vatnrs               30
040700010612     C  N30              TESTN                   a_vatnrs                 30
040800010612     C                   MOVE      a_vatnrs      W001A             1
040900010612     C     *IN30         IFEQ      *OFF
041000010612     C     W001A         ORLT      '0'
041100010612     C     W001A         ANDNE     *BLANK
041200010612     C                   SETON                                        31
041300010612     C                   Z-ADD     *zeros        VATNRS
041400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
041500010612     C                             + ' ' + 'VATNRS'
041600010612     C                   ELSE
041700010612     C                   MOVE      a_vatnrs      VATNRS
041800010612     C                   ENDIF
041900010612     C*
042000010612     C                   TESTN                   a_vatnsp             30
042100010612     C  N30              TESTN                   a_vatnsp               30
042200010612     C  N30              TESTN                   a_vatnsp                 30
042300010612     C                   MOVE      a_vatnsp      W001A             1
042400010612     C     *IN30         IFEQ      *OFF
042500010612     C     W001A         ORLT      '0'
042600010612     C                   SETON                                        31
042700010612     C                   Z-ADD     *zeros        VATNSP
042800010612     C                   EVAL      vinmsg = %trimr(vinmsg)
042900010612     C                             + ' ' + 'VATNSP'
043000010612     C                   ELSE
043100010612     C                   MOVE      a_vatnsp      VATNSP
043200010612     C                   ENDIF
043300010612     C*
043400010612     C*
043500010612     C* ALFABETICI
043600010612     C                   MOVE      a_vatatb      VATATB
043700010612     C                   MOVE      a_vattrc      VATTRC
043800020725     C                   EVAL      VATNOT = %trim(a_vatnot)
043900010330     C*
044000010330     C                   ENDSR
044100010612     C*----------------------------------------------------*
044200990920
044300010601
044400010601
044500010601
044600010601
044700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
044800010601     C     repfil        BEGSR
044900010601     C*
045000010601     C                   if        invfil = *zeros and
045100010601     C                             depfil > *zeros and
045200010613     C                             (vinflg = *blanks or
045300010613     C                              vinflg = *zeros)
045400010601     C
045500010601     C                   eval      invfil = depfil
045600010601     C                   endif
045700010601     C*
045800010601     C                   if        depfil <> invfil and
045900010601     C                             invfil > *zeros
046000010601     C                   eval      flgMulti = '1'
046100010601     C                   if        vinflg = *blanks
046200010601     C                   add       1             cntNonEl
046300010601     C                   endif
046400010601     C                   endif
046500010601     C*
046600010601     C                   if        vinflg = '2'
046700010601     C                   eval      flgStato = '2'
046800010601     C                   endif
046900010601     C*
047000010601     C                   ENDSR
047100010601     C***
047200010601
047300010601
047400010601
047500010330
047600010330
047700010330
047800990920      /TITLE Invio dei dati al punto operativo.
047900000613     C     invio         BEGSR
048000990920     C*
048100990920     C                   reset                   dscmz
048200010601     C                   move      invfil        cmzdst
048300990920     C                   eval      cmzfld = vlrfou
048400990920     C                   eval      cmzmbd = vlrhdl
048500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
048600000710     C                   if        prmfir = *blanks
048700020916     C                   eval      cmzfla = 'FIVAT00F'
048800020916     C                   eval      cmzmba = 'FIVAT00F'
048900000710     C                   else
049000000710     C                   eval      cmzfla = prmfir
049100000710     C                   eval      cmzmba = prmfir
049200000710     C                   endif
049300990920     C                   eval      cmznrr = *zeros
049400990920     C                   move      §ctrok        cmznrr
049500021003     C                   eval      cmzlba = vlrfl1
049600990920     C                   call(e)   'TIS711C'
049700990920     C                   parm                    dscmz
049800990921     C                   parm      *blanks       esito
049900990920     C                   if        %error
050000990920     C                             or cmzerr = '1'
050100990921     C                             or esito  = '1'
050200000710     C                   eval      wrkesito = '3'
050300990920     C                   endif
050400990920     C*
050500000613     C                   ENDSR
050600990910
050700010601
050800010601
050900010601
051000010601
051100010601      /TITLE Invio dei dati al punto operativo.
051200010601     C     opeini        BEGSR
051300010601     C*
051400010601     C* Inizializzo flag e contatori operativi
051500010601     C                   movel     '0'           flgGiro           1
051600010601     C                   movel     '0'           flgMulti          1
051700010601     C                   movel     '1'           flgStato          1
051800010615     C                   movel     '0'           flgOk             1
051900010601     C                   z-add     *zeros        cntNonEl         10 0
052000010601     C                   z-add     *zeros        depfil            3 0
052100010601     C                   z-add     *zeros        invfil            3 0
052200030407     C*
052300030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
052400030407     C                   if        %subst(vlrppt:1:1) = 'S'
052500030407     C                   seton                                        50
052600030407     C                   else
052700030407     C                   setoff                                       50
052800030407     C                   endif
052900010601     C*
053000010601     C                   ENDSR
053100010601     C***
053200090611
053300090611
053400090611
053500090611     C*----------------------------------------------------*
053600090611     C*  CONTROLLO NUMERICITA' CAMPI
053700090611     C*----------------------------------------------------*
053800090611     C     CHKNUM        BEGSR
053900090611     C*
054000090611     C                   IF        PiDecChr = *blanks
054100090611     C                   EVAL      PiDecChr = ','
054200090611     C                   ENDIF
054300090611     C*
054400090611     C                   callp(e)  UBISNUM_Check(PiStr
054500090611     C                                          :PiDecChr
054600090611     C                                          :PiVal
054700090611     C                                          :PiNum
054800090611     C                                          :PiInt)
054900090611     C*
055000090611     C                   IF        %error
055100090611     C                   EVAL      PiInt=*off
055200090611     C                   ENDIF
055300090611     C*
055400090611     C                   ENDSR
055500090611     C***
055600010601
055700010330
055800010330
055900000613     C     *inzsr        BEGSR
056000990910     C*
056100990910     C     *entry        plist
056200990920     C                   parm                    tivlrds
056300990921     C                   parm      wrkesito      esito
056400000724     C                   parm                    prmlit
056500000710     C                   parm                    prmfir
056600010330     C*
056700010330     C* CALCOLA LA DATA CORRENTE
056800010330     C                   time                    wn14             14 0
056900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057000010330     C                   z-add     wn8           g08dat
057100010330     C                   z-add     *zeros        g08inv
057200010330     C                   movel     '0'           g08err
057300010330     C                   call      'XSRDA8'
057400010330     C                   parm                    wlbda8
057500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057600000613     C*
057700000613     C                   ENDSR
057800000613     C***
057900990908
058000010612     Otis7t6p   E            riepilogo         2
058100990915     O                                              'Upload via Internet'
058200990915     O                                           +1 'Traduzione TIVIN00R -'
058300020916     O                                           +1 'FIVATWWR'
058400990915     O                                           +1 'Testate bolle'
058500990915     O          E            riepilogo   2
058600990915     O                       wrkdata
058700990915     O                       wrkora              +1
058800990915     O                       procname            +1
058900990915     O          E            riepilogo   2
059000990915     O                                              'Cliente..................:'
059100010612     O                       VATCCM        z     +1
059200990915     O          E            riepilogo   2
059300990920     O                                              'Riferimento Strategi.....:'
059400990920     O                       vlrhdl              +1
059500990915     O          E            riepilogo   2
059600990915     O                                              'Giusti...................:'
059700971022     O                       §CTROK        2   +  1
059800990915     O          E            riepilogo   2
059900990915     O                                              'Sbagliati e corretti.....:'
060000971022     O                       §CTRMO        2   +  1
060100990915     O          E            riepilogo   2
060200990915     O                                              'Sbagliati e scartati.....:'
060300971022     O                       §CTRNO        2   +  1
060400000613
060500010612     Otis7t6ps  E            testdett          2
060600000613     O                                              'Upload via Internet'
060700000613     O                                           +1 'Traduzione TIVIN00R -'
060800020916     O                                           +1 'FIVATWWR'
060900000613     O                                           +1 'Testate bolle'
061000000616     O          E            testdett    3
061100000613     O                                           +2 'N° rec'
061200000613     O                                           +3 'Anteprima contenuto'
061300000616     O          E            rigadett    2
061400000613     O                       rrnum               +2
061500000621     O                       recko               +3
061600000616     O          E            findett     2
061700000613     O                       wrkdata
061800000613     O                       wrkora              +1
061900000613     O                       procname            +1
062000000616     O          E            findett     2
062100000613     O                                              'Cliente..................:'
062200010612     O                       VATCCM        z     +1
062300000616     O          E            findett     2
062400000613     O                                              'Riferimento Strategi.....:'
062500000613     O                       vlrhdl              +1
062600000616     O          E            findett     2
062700000613     O                                              'Giusti...................:'
062800000613     O                       §CTROK        2   +  1
062900000616     O          E            findett     2
063000000613     O                                              'Sbagliati e corretti.....:'
063100000613     O                       §CTRMO        2   +  1
063200000616     O          E            findett     2
063300000613     O                                              'Sbagliati e scartati.....:'
063400000613     O                       §CTRNO        2   +  1
063500000613** CMD - COMANDI CL
063600011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
063700011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
063800010612DLTOVR FILE(TIS7T6P TIS7T6PS) LVL(*)
063900010606
064000000613
