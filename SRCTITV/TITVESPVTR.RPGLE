000100020916      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200990908     H dftactgrp(*yes)
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
005100010330
005200010330
005300990915     C                   time                    wrkdata
005400990915     C                   time                    wrkora
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800010601     C*
005900010601     C                   exsr      opeini
006000010612     C                   exsr      rwvat
006100010601     C*
006200010601     C                   seton                                        lr
006300010601
006400010601
006500010601
006600010601
006700010601     C*--------------------------------------------------------
006800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006900010601     C*--------------------------------------------------------
007000010601     C     PREELA        BEGSR
007100010601     C*
007200010601     C* SE OCCORRE SPEDIRE IN FILIALE
007300010601     C                   if        invfil <> *zeros and
007400010601     C                             flgGiro = '0'
007500010601     C*
007600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007700010601     C                   eval      flgGiro = '1'
007800000724     C*
007900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008100010601     C     invfil        chain     azorg01l
008200000724     C                   if        %found
008300000724     C                   movel(p)  CMD(1)        depcmd
008400020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008500000724     C*
008600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008700011123     C                   Z-ADD     150           LENGH            15 5
008800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000724     C                   PARM                    depcmd
009000000724     C                   PARM                    LENGH
009100000724     C*
009200000724     C                   endif
009300000616     C*
009400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009500000616     C                   movel(p)  CMD(2)        depcmd
009600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009700000616     C*
009800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009900011123     C                   Z-ADD     150           LENGH            15 5
010000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010100000616     C                   PARM                    depcmd
010200000616     C                   PARM                    LENGH
010300000613     C*
010400010612     C                   if        not %open(tis7t6ps)
010500010612     C                   open      tis7t6ps
010600030407     C   50              except    testdett
010700000613     C                   endif
010800010601     C*
010900010601     C                   endif
011000010601     C*
011100010601     C                   ENDSR
011200010601     C***
011300010601
011400010601
011500010601
011600010601
011700010601
011800010601
011900010601
012000010601     C*--------------------------------------------------------
012100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012200010601     C*--------------------------------------------------------
012300010601     C     ENDELA        BEGSR
012400990915     C*
012500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012600000613     C*
012700010612     C                   if        %open(tis7t6ps)
012800030407     C   50              except    findett
012900010612     C                   close     tis7t6ps
013000000613     C                   endif
013100000616     C*
013200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013300011123     C                   Z-ADD     150           LENGH            15 5
013400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013500000616     C                   PARM                    CMD(3)
013600000616     C                   PARM                    LENGH
013700000616     C*
013800010601     C                   ENDSR
013900010601     C***
014000010601
014100010601
014200010601
014300010601
014400000613
014500000613     C*--------------------------------------------------------
014600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014700000613     C*--------------------------------------------------------
014800000621     C     STPR          BEGSR
014900000613     C*
015000010612     C                   if        not %open(tis7t6p)
015100010612     C                   open      tis7t6p
015200990915     C                   endif
015300990915     C*
015400030407     C   50              except    riepilogo
015500990915     C*
015600010612     C                   if        %open(tis7t6p)
015700010612     C                   close     tis7t6p
015800990914     C                   endif
015900000613     C*
016000000613     C                   ENDSR
016100000613     C***
016200000613
016300010330
016400010601
016500010601
016600010601
016700010330     C*--------------------------------------------------------
016800020916     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
016900010330     C*--------------------------------------------------------
017000010612     C     RWVAT         BEGSR
017100010330     C*
017200010330     C                   if        not %open(tivin00r)
017300010330     C                   open      tivin00r
017400010330     C                   endif
017500020916     C                   if        not %open(fivatwwr)
017600020916     C                   open      fivatwwr
017700010330     C                   endif
017800010330     C*
017900010604     C                   clear                   §CTROK
018000010604     C                   clear                   §CTRMO
018100010604     C                   clear                   §CTRNO
018200010330     C*
018300010330     C                   DO        *HIVAL
018400010330     C*
018500010330     C                   READ      tivin00r                               70
018600010618     C*
018700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018800010618     C                   if        vinflg = '1'
018900010618     C                   eval      flgOk = '1'
019000010618     C                   endif
019100010618     C*
019200010330     C                   if        vindta > *blanks
019300010330     C                   add       1             rrnum
019400010330     C*
019500010601     C                   if        *in70 = *off and
019600010330     C                             (vinflg = *blanks
019700010330     C                              or vinflg = '0'
019800010330     C                              or vinflg = '2')
019900010711     C*
020000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020100010711     C                   if        vinflg = *blanks or vinflg = '0'
020200010711     C                   clear                   vinmsg
020300010711     C                   endif
020400010711     C*
020500010613     C                   eval      fnvataar = vindta
020600010601     C*
020700010330     C                   exsr      inzvar
020800010330     C                   exsr      defcam
020900010612     C                   exsr      impvat
021000070115     C*
021100070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021200070115     C                   eval      depfil = VATLNP
021300070115     C*
021400070115     C* Effettuo forzatura ESPRINET x invio a terminal d competenza rispetto al CCM indicato dal
021500070115     C* cliente
021600070507     C                   if        VATCCM = 0893943 OR
021601070507     C                             VATCCM = 0894169
021700070115     C                   eval      depfil = 050
021800070115     C                   eval      invfil = 050
021900070115     C                   eval      vatfgs = 050
022000070115     C                   endif
022100070507     C                   if        VATCCM = 0893942 OR
022101070507     C                             VATCCM = 0894168
022200070115     C                   eval      depfil = 005
022300070115     C                   eval      invfil = 005
022400070115     C                   eval      vatfgs = 005
022500070115     C                   endif
022600010601     C*
022700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022800010601     C                   exsr      repfil
022900021010     C                   if        depfil = invfil
023000021014     C                   if        vlrpoi = 999
023100021010     C                   MOVE(P)   invfil        VATFGS
023200021014     C                   else
023300021014     C                   MOVE(P)   vlrpoi        VATFGS
023400021014     C                   endif
023500010601     C*
023600010601     C                   exsr      PREELA
023700010601     C*
023800010604     C* Ebbene...
023900010604     C*
024000010604     C  N31              ADD       1             §CTROK            7 0
024100010604     C   32              ADD       1             §CTRMO            7 0
024200010604     C   31              ADD       1             §CTRNO            7 0
024300020916     C  N31              WRITE     FIVAT000
024400010604     C*
024500010604     C                   if        *in31 = *off and
024600010604     C                             *in32 = *off
024700010604     C                   eval      vinflg = '1'
024800010604     C                   else
024900010604     C                   eval      recko = vindta
025000010612     C                   if        %open(tis7t6ps)
025100030407     C   50              except    rigadett
025200010612     C                   endif
025300010604     C                   eval      vinflg = '2'
025400010604     C                   endif
025500010604     C                   endif
025600010601     C*
025700010604     C                   endif
025800010604     C*
025900010330     C                   else
026000010330     C                   eval      vinflg = '1'
026100010330     C                   endif
026200010601     C*
026300010601     C  N70              update    tivin000
026400010330     C*
026500010330     C  N70              ENDdo
026600010601     C
026700010601     C*
026800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026900010601     C                   if        cntNonEl = *zeros or
027000010601     C                             flgMulti = '0'
027100010330     C* Se non ci sono record con errori ...
027200010601     C                   if        §ctrno = 0 and
027300010604     C                             §ctrmo = 0 and
027400010601     C                             flgStato <> '2'
027500010330     C* ... restituisco esito OK.
027600010330     C                   eval      wrkesito = '0'
027700010330     C                   else
027800010330     C                   if        §ctrok > 0
027900010330     C                   eval      wrkesito = '1'
028000010330     C                   else
028100010615     C                   if        flgOk = '0'
028200010615     C                   eval      wrkesito = '2'
028300010615     C                   else
028400010615     C                   eval      wrkesito = '6'
028500010615     C                   endif
028600010330     C                   endif
028700010330     C                   endif
028800010601     C                   else
028900010601     C                   eval      wrkesito = '9'
029000010601     C                   endif
029100010330     C*
029200010330     C                   if        %open(tivin00r)
029300010330     C                   close     tivin00r
029400010330     C                   endif
029500020916     C                   if        %open(fivatwwr)
029600020916     C                   close     fivatwwr
029700010330     C                   endif
029800010601     C*
029900010601     C                   if        vlrpoi <> 999
030000010601     C                   eval      invfil = vlrpoi
030100010601     C                   endif
030200010330     C*
030300010330     C                   if        §ctrok > 0
030400010601     C                             and invfil > *zeros
030500010330     C                   exsr      invio
030600010330     C                   endif
030700010601     C*
030800010612     C                   if        flgGiro = '1'
030900010601     C                   exsr      endela
031000010612     C                   endif
031100010330     C*
031200010330     C                   ENDSR
031300010330     C***
031400010330
031500010601
031600010601
031700010601
031800010601
031900010330     C*----------------------------------------------------*
032000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032100010330     C*----------------------------------------------------*
032200010330     C     INZVAR        BEGSR
032300010330     C*
032400010330     C*
032500010330     C                   ENDSR
032600010330     C*----------------------------------------------------*
032700010330     C*  IMPOSTAZIONE CAMPI COSTANTI
032800010330     C*----------------------------------------------------*
032900010330     C     DEFCAM        BEGSR
033000010330     C*
033100020916     C                   CLEAR                   FIVAT000
033200010330     C*
033300010330     C                   ENDSR
033400010330     C*----------------------------------------------------*
033500020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
033600010330     C*----------------------------------------------------*
033700010612     C     IMPVAT        BEGSR
033800010330     C*
033900010604     C                   SETOFF                                       3132
034000010612     C*
034100010612     C                   SETOFF                                       303132
034200010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
034300010612     C                   TESTN                   a_vatccm             30
034400010612     C  N30              TESTN                   a_vatccm               30
034500010612     C  N30              TESTN                   a_vatccm                 30
034600010612     C                   MOVE      a_vatccm      W001A             1
034700010612     C     *IN30         IFEQ      *OFF
034800010612     C     W001A         ORLT      '0'
034900010612     C                   SETON                                        31
035000010612     C                   Z-ADD     *zeros        VATCCM
035100010612     C                   EVAL      vinmsg = 'VATCCM'
035200010612     C                   ELSE
035300010612     C                   MOVE      a_vatccm      VATCCM
035400010612     C     VATCCM        IFLE      *ZEROS
035500010612     C                   SETON                                        31
035600010612     C                   Z-ADD     *zeros        VATCCM
035700010612     C                   EVAL      vinmsg = 'VATCCM'
035800010612     C                   ENDIF
035900010612     C                   ENDIF
036000010612     C*
036100010612     C                   TESTN                   a_vatlnp             30
036200010612     C  N30              TESTN                   a_vatlnp               30
036300010612     C  N30              TESTN                   a_vatlnp                 30
036400010612     C                   MOVE      a_vatlnp      W001A             1
036500010612     C     *IN30         IFEQ      *OFF
036600010612     C     W001A         ORLT      '0'
036700010612     C                   SETON                                        31
036800010612     C                   Z-ADD     *zeros        VATLNP
036900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
037000021010     C                             + ' ' + 'VATLNP'
037100010612     C                   ELSE
037200010612     C                   MOVE      a_vatlnp      VATLNP
037300010612     C     VATLNP        IFLE      *ZEROS
037400010612     C                   SETON                                        31
037500010612     C                   Z-ADD     *zeros        VATLNP
037600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
037700021010     C                             + ' ' + 'VATLNP'
037800010612     C                   ENDIF
037900010612     C                   ENDIF
038000010612     C*
038100010612     C                   TESTN                   a_vataas             30
038200010612     C  N30              TESTN                   a_vataas               30
038300010612     C  N30              TESTN                   a_vataas                 30
038400010612     C                   MOVE      a_vataas      W001A             1
038500010612     C     *IN30         IFEQ      *OFF
038600010612     C     W001A         ORLT      '0'
038700010612     C                   SETON                                        31
038800010612     C                   MOVEL(P)  datcor        VATAAS
038900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
039000010612     C                             + ' ' + 'VATAAS'
039100010612     C                   ELSE
039200010612     C                   MOVE      a_vataas      VATAAS
039300010612     C                   ENDIF
039400010612     C*
039500010612     C                   TESTN                   a_vatnrs             30
039600010612     C  N30              TESTN                   a_vatnrs               30
039700010612     C  N30              TESTN                   a_vatnrs                 30
039800010612     C                   MOVE      a_vatnrs      W001A             1
039900010612     C     *IN30         IFEQ      *OFF
040000010612     C     W001A         ORLT      '0'
040100010612     C     W001A         ANDNE     *BLANK
040200010612     C                   SETON                                        31
040300010612     C                   Z-ADD     *zeros        VATNRS
040400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
040500010612     C                             + ' ' + 'VATNRS'
040600010612     C                   ELSE
040700010612     C                   MOVE      a_vatnrs      VATNRS
040800010612     C                   ENDIF
040900010612     C*
041000010612     C                   TESTN                   a_vatnsp             30
041100010612     C  N30              TESTN                   a_vatnsp               30
041200010612     C  N30              TESTN                   a_vatnsp                 30
041300010612     C                   MOVE      a_vatnsp      W001A             1
041400010612     C     *IN30         IFEQ      *OFF
041500010612     C     W001A         ORLT      '0'
041600010612     C                   SETON                                        31
041700010612     C                   Z-ADD     *zeros        VATNSP
041800010612     C                   EVAL      vinmsg = %trimr(vinmsg)
041900010612     C                             + ' ' + 'VATNSP'
042000010612     C                   ELSE
042100010612     C                   MOVE      a_vatnsp      VATNSP
042200010612     C                   ENDIF
042300010612     C*
042400010612     C*
042500010612     C* ALFABETICI
042600010612     C                   MOVE      a_vatatb      VATATB
042700010612     C                   MOVE      a_vattrc      VATTRC
042800020725     C                   EVAL      VATNOT = %trim(a_vatnot)
042900010330     C*
043000010330     C                   ENDSR
043100010612     C*----------------------------------------------------*
043200990920
043300010601
043400010601
043500010601
043600010601
043700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
043800010601     C     repfil        BEGSR
043900010601     C*
044000010601     C                   if        invfil = *zeros and
044100010601     C                             depfil > *zeros and
044200010613     C                             (vinflg = *blanks or
044300010613     C                              vinflg = *zeros)
044400010601     C
044500010601     C                   eval      invfil = depfil
044600010601     C                   endif
044700010601     C*
044800010601     C                   if        depfil <> invfil and
044900010601     C                             invfil > *zeros
045000010601     C                   eval      flgMulti = '1'
045100010601     C                   if        vinflg = *blanks
045200010601     C                   add       1             cntNonEl
045300010601     C                   endif
045400010601     C                   endif
045500010601     C*
045600010601     C                   if        vinflg = '2'
045700010601     C                   eval      flgStato = '2'
045800010601     C                   endif
045900010601     C*
046000010601     C                   ENDSR
046100010601     C***
046200010601
046300010601
046400010601
046500010330
046600010330
046700010330
046800990920      /TITLE Invio dei dati al punto operativo.
046900000613     C     invio         BEGSR
047000990920     C*
047100990920     C                   reset                   dscmz
047200010601     C                   move      invfil        cmzdst
047300990920     C                   eval      cmzfld = vlrfou
047400990920     C                   eval      cmzmbd = vlrhdl
047500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
047600000710     C                   if        prmfir = *blanks
047700020916     C                   eval      cmzfla = 'FIVAT00F'
047800020916     C                   eval      cmzmba = 'FIVAT00F'
047900000710     C                   else
048000000710     C                   eval      cmzfla = prmfir
048100000710     C                   eval      cmzmba = prmfir
048200000710     C                   endif
048300990920     C                   eval      cmznrr = *zeros
048400990920     C                   move      §ctrok        cmznrr
048500021003     C                   eval      cmzlba = vlrfl1
048600990920     C                   call(e)   'TIS711C'
048700990920     C                   parm                    dscmz
048800990921     C                   parm      *blanks       esito
048900990920     C                   if        %error
049000990920     C                             or cmzerr = '1'
049100990921     C                             or esito  = '1'
049200000710     C                   eval      wrkesito = '3'
049300990920     C                   endif
049400990920     C*
049500000613     C                   ENDSR
049600990910
049700010601
049800010601
049900010601
050000010601
050100010601      /TITLE Invio dei dati al punto operativo.
050200010601     C     opeini        BEGSR
050300010601     C*
050400010601     C* Inizializzo flag e contatori operativi
050500010601     C                   movel     '0'           flgGiro           1
050600010601     C                   movel     '0'           flgMulti          1
050700010601     C                   movel     '1'           flgStato          1
050800010615     C                   movel     '0'           flgOk             1
050900010601     C                   z-add     *zeros        cntNonEl         10 0
051000010601     C                   z-add     *zeros        depfil            3 0
051100010601     C                   z-add     *zeros        invfil            3 0
051200030407     C*
051300030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
051400030407     C                   if        %subst(vlrppt:1:1) = 'S'
051500030407     C                   seton                                        50
051600030407     C                   else
051700030407     C                   setoff                                       50
051800030407     C                   endif
051900010601     C*
052000010601     C                   ENDSR
052100010601     C***
052200010601
052300010601
052400010601
052500010330
052600010330
052700000613     C     *inzsr        BEGSR
052800990910     C*
052900990910     C     *entry        plist
053000990920     C                   parm                    tivlrds
053100990921     C                   parm      wrkesito      esito
053200000724     C                   parm                    prmlit
053300000710     C                   parm                    prmfir
053400010330     C*
053500010330     C* CALCOLA LA DATA CORRENTE
053600010330     C                   time                    wn14             14 0
053700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053800010330     C                   z-add     wn8           g08dat
053900010330     C                   z-add     *zeros        g08inv
054000010330     C                   movel     '0'           g08err
054100010330     C                   call      'XSRDA8'
054200010330     C                   parm                    wlbda8
054300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054400000613     C*
054500000613     C                   ENDSR
054600000613     C***
054700990908
054800010612     Otis7t6p   E            riepilogo         2
054900990915     O                                              'Upload via Internet'
055000990915     O                                           +1 'Traduzione TIVIN00R -'
055100020916     O                                           +1 'FIVATWWR'
055200990915     O                                           +1 'Testate bolle'
055300990915     O          E            riepilogo   2
055400990915     O                       wrkdata
055500990915     O                       wrkora              +1
055600990915     O                       procname            +1
055700990915     O          E            riepilogo   2
055800990915     O                                              'Cliente..................:'
055900010612     O                       VATCCM        z     +1
056000990915     O          E            riepilogo   2
056100990920     O                                              'Riferimento Strategi.....:'
056200990920     O                       vlrhdl              +1
056300990915     O          E            riepilogo   2
056400990915     O                                              'Giusti...................:'
056500971022     O                       §CTROK        2   +  1
056600990915     O          E            riepilogo   2
056700990915     O                                              'Sbagliati e corretti.....:'
056800971022     O                       §CTRMO        2   +  1
056900990915     O          E            riepilogo   2
057000990915     O                                              'Sbagliati e scartati.....:'
057100971022     O                       §CTRNO        2   +  1
057200000613
057300010612     Otis7t6ps  E            testdett          2
057400000613     O                                              'Upload via Internet'
057500000613     O                                           +1 'Traduzione TIVIN00R -'
057600020916     O                                           +1 'FIVATWWR'
057700000613     O                                           +1 'Testate bolle'
057800000616     O          E            testdett    3
057900000613     O                                           +2 'N° rec'
058000000613     O                                           +3 'Anteprima contenuto'
058100000616     O          E            rigadett    2
058200000613     O                       rrnum               +2
058300000621     O                       recko               +3
058400000616     O          E            findett     2
058500000613     O                       wrkdata
058600000613     O                       wrkora              +1
058700000613     O                       procname            +1
058800000616     O          E            findett     2
058900000613     O                                              'Cliente..................:'
059000010612     O                       VATCCM        z     +1
059100000616     O          E            findett     2
059200000613     O                                              'Riferimento Strategi.....:'
059300000613     O                       vlrhdl              +1
059400000616     O          E            findett     2
059500000613     O                                              'Giusti...................:'
059600000613     O                       §CTROK        2   +  1
059700000616     O          E            findett     2
059800000613     O                                              'Sbagliati e corretti.....:'
059900000613     O                       §CTRMO        2   +  1
060000000616     O          E            findett     2
060100000613     O                                              'Sbagliati e scartati.....:'
060200000613     O                       §CTRNO        2   +  1
060300000613** CMD - COMANDI CL
060400011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
060500011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
060600010612DLTOVR FILE(TIS7T6P TIS7T6PS) LVL(*)
060700010606
060800000613
