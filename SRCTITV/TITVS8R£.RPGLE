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
021000060504     C                   exsr      forpar
021100060504     C                   exsr      chkdat
021200010601     C*
021300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021400010612     C                   eval      depfil = VATLNP
021500010601     C                   exsr      repfil
021600021010     C                   if        depfil = invfil
021700021014     C                   if        vlrpoi = 999
021800021010     C                   MOVE(P)   invfil        VATFGS
021900021014     C                   else
022000021014     C                   MOVE(P)   vlrpoi        VATFGS
022100021014     C                   endif
022200010601     C*
022300010601     C                   exsr      PREELA
022400010601     C*
022500010604     C* Ebbene...
022600010604     C*
022700010604     C  N31              ADD       1             §CTROK            7 0
022800010604     C   32              ADD       1             §CTRMO            7 0
022900010604     C   31              ADD       1             §CTRNO            7 0
023000020916     C  N31              WRITE     FIVAT000
023100010604     C*
023200010604     C                   if        *in31 = *off and
023300010604     C                             *in32 = *off
023400010604     C                   eval      vinflg = '1'
023500010604     C                   else
023600010604     C                   eval      recko = vindta
023700010612     C                   if        %open(tis7t6ps)
023800030407     C   50              except    rigadett
023900010612     C                   endif
024000010604     C                   eval      vinflg = '2'
024100010604     C                   endif
024200010604     C                   endif
024300010601     C*
024400010604     C                   endif
024500010604     C*
024600010330     C                   else
024700010330     C                   eval      vinflg = '1'
024800010330     C                   endif
024900010601     C*
025000010601     C  N70              update    tivin000
025100010330     C*
025200010330     C  N70              ENDdo
025300010601     C
025400010601     C*
025500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025600010601     C                   if        cntNonEl = *zeros or
025700010601     C                             flgMulti = '0'
025800010330     C* Se non ci sono record con errori ...
025900010601     C                   if        §ctrno = 0 and
026000010604     C                             §ctrmo = 0 and
026100010601     C                             flgStato <> '2'
026200010330     C* ... restituisco esito OK.
026300010330     C                   eval      wrkesito = '0'
026400010330     C                   else
026500010330     C                   if        §ctrok > 0
026600010330     C                   eval      wrkesito = '1'
026700010330     C                   else
026800010615     C                   if        flgOk = '0'
026900010615     C                   eval      wrkesito = '2'
027000010615     C                   else
027100010615     C                   eval      wrkesito = '6'
027200010615     C                   endif
027300010330     C                   endif
027400010330     C                   endif
027500010601     C                   else
027600010601     C                   eval      wrkesito = '9'
027700010601     C                   endif
027800010330     C*
027900010330     C                   if        %open(tivin00r)
028000010330     C                   close     tivin00r
028100010330     C                   endif
028200020916     C                   if        %open(fivatwwr)
028300020916     C                   close     fivatwwr
028400010330     C                   endif
028500010601     C*
028600010601     C                   if        vlrpoi <> 999
028700010601     C                   eval      invfil = vlrpoi
028800010601     C                   endif
028900010330     C*
029000010330     C                   if        §ctrok > 0
029100010601     C                             and invfil > *zeros
029200010330     C                   exsr      invio
029300010330     C                   endif
029400010601     C*
029500010612     C                   if        flgGiro = '1'
029600010601     C                   exsr      endela
029700010612     C                   endif
029800010330     C*
029900010330     C                   ENDSR
030000010330     C***
030100010330
030200010601
030300010601
030400010601
030500010601
030600010330     C*----------------------------------------------------*
030700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030800010330     C*----------------------------------------------------*
030900010330     C     INZVAR        BEGSR
031000010330     C*
031100010330     C*
031200010330     C                   ENDSR
031300010330     C*----------------------------------------------------*
031400010330     C*  IMPOSTAZIONE CAMPI COSTANTI
031500010330     C*----------------------------------------------------*
031600010330     C     DEFCAM        BEGSR
031700010330     C*
031800020916     C                   CLEAR                   FIVAT000
031900010330     C*
032000010330     C                   ENDSR
032100060504     C*----------------------------------------------------*
032200060504     C*  FORZATURE DA PARAMETRI
032300060504     C*----------------------------------------------------*
032400060504     C     FORPAR        BEGSR
032500060504     C*
032600060504     C* Verifico i valori d default passati come parametri
032700060504     C                   IF        vlrppt > *blanks
032800060504     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
032900060504     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033000060504     C                   EXSR      CHKNUM
033100060504     C                   IF        PiInt=*on
033200060504     C                   Z-ADD     PiVal         VATCCM
033300060504     C                   ENDIF
033400060504     C                   ENDIF
033500060504     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
033600060504     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033700060504     C                   EXSR      CHKNUM
033800060504     C                   IF        PiInt=*on
033900060504     C                   Z-ADD     PiVal         VATLNP
034000060504     C                   ENDIF
034100060504     C                   ENDIF
034200060504     C                   IF        %trim(%subst(vlrppt:11:1)) <> *blanks
034300060504     C                   EVAL      VATTRC = %trim(%subst(vlrppt:11:1))
034400060504     C                   ENDIF
034500060504     C                   ENDIF
034600060504     C*
034700060504     C                   ENDSR
034800060504     C*----------------------------------------------------*
034900060504     C*  VERIFICA DATI ESSENZIALI
035000060504     C*----------------------------------------------------*
035100060504     C     CHKDAT        BEGSR
035200060504     C*
035300060504     C                   SETOFF                                       31
035400060504     C*
035500060504     C* CCM
035600060504     C                   IF        VATCCM = *zeros
035700060504     C                   SETON                                        31
035800060504     C                   ENDIF
035900060504     C*
036000060504     C* LNP
036100060504     C                   IF        VATLNP = *zeros
036200060504     C                   SETON                                        31
036300060504     C                   ENDIF
036400060504     C*
036500060504     C* AAS
036600060504     C                   IF        VATAAS = *zeros
036700060504     C                   SETON                                        31
036800060504     C                   ENDIF
036900060504     C*
037000060504     C* NSP
037100060504     C                   IF        VATNSP = *zeros
037200060504     C                   SETON                                        31
037300060504     C                   ENDIF
037400060504     C*
037500060504     C                   ENDSR
037600010330     C*----------------------------------------------------*
037700020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
037800010330     C*----------------------------------------------------*
037900010612     C     IMPVAT        BEGSR
038000010330     C*
038100010604     C                   SETOFF                                       3132
038200010612     C*
038300010612     C                   SETOFF                                       303132
038400010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
038500010612     C                   TESTN                   a_vatccm             30
038600010612     C  N30              TESTN                   a_vatccm               30
038700010612     C  N30              TESTN                   a_vatccm                 30
038800010612     C                   MOVE      a_vatccm      W001A             1
038900010612     C     *IN30         IFEQ      *OFF
039000010612     C     W001A         ORLT      '0'
039100010612     C                   SETON                                        31
039200010612     C                   Z-ADD     *zeros        VATCCM
039300010612     C                   EVAL      vinmsg = 'VATCCM'
039400010612     C                   ELSE
039500010612     C                   MOVE      a_vatccm      VATCCM
039600010612     C     VATCCM        IFLE      *ZEROS
039700010612     C                   SETON                                        31
039800010612     C                   Z-ADD     *zeros        VATCCM
039900010612     C                   EVAL      vinmsg = 'VATCCM'
040000010612     C                   ENDIF
040100010612     C                   ENDIF
040200010612     C*
040300010612     C                   TESTN                   a_vatlnp             30
040400010612     C  N30              TESTN                   a_vatlnp               30
040500010612     C  N30              TESTN                   a_vatlnp                 30
040600010612     C                   MOVE      a_vatlnp      W001A             1
040700010612     C     *IN30         IFEQ      *OFF
040800010612     C     W001A         ORLT      '0'
040900010612     C                   SETON                                        31
041000010612     C                   Z-ADD     *zeros        VATLNP
041100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
041200021010     C                             + ' ' + 'VATLNP'
041300010612     C                   ELSE
041400010612     C                   MOVE      a_vatlnp      VATLNP
041500010612     C     VATLNP        IFLE      *ZEROS
041600010612     C                   SETON                                        31
041700010612     C                   Z-ADD     *zeros        VATLNP
041800010612     C                   EVAL      vinmsg = %trimr(vinmsg)
041900021010     C                             + ' ' + 'VATLNP'
042000010612     C                   ENDIF
042100010612     C                   ENDIF
042200010612     C*
042300010612     C                   TESTN                   a_vataas             30
042400010612     C  N30              TESTN                   a_vataas               30
042500010612     C  N30              TESTN                   a_vataas                 30
042600010612     C                   MOVE      a_vataas      W001A             1
042700010612     C     *IN30         IFEQ      *OFF
042800010612     C     W001A         ORLT      '0'
042900010612     C                   SETON                                        31
043000010612     C                   MOVEL(P)  datcor        VATAAS
043100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
043200010612     C                             + ' ' + 'VATAAS'
043300010612     C                   ELSE
043400010612     C                   MOVE      a_vataas      VATAAS
043500010612     C                   ENDIF
043600010612     C*
043700010612     C                   TESTN                   a_vatnrs             30
043800010612     C  N30              TESTN                   a_vatnrs               30
043900010612     C  N30              TESTN                   a_vatnrs                 30
044000010612     C                   MOVE      a_vatnrs      W001A             1
044100010612     C     *IN30         IFEQ      *OFF
044200010612     C     W001A         ORLT      '0'
044300010612     C     W001A         ANDNE     *BLANK
044400010612     C                   SETON                                        31
044500010612     C                   Z-ADD     *zeros        VATNRS
044600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
044700010612     C                             + ' ' + 'VATNRS'
044800010612     C                   ELSE
044900010612     C                   MOVE      a_vatnrs      VATNRS
045000010612     C                   ENDIF
045100010612     C*
045200010612     C                   TESTN                   a_vatnsp             30
045300010612     C  N30              TESTN                   a_vatnsp               30
045400010612     C  N30              TESTN                   a_vatnsp                 30
045500010612     C                   MOVE      a_vatnsp      W001A             1
045600010612     C     *IN30         IFEQ      *OFF
045700010612     C     W001A         ORLT      '0'
045800010612     C                   SETON                                        31
045900010612     C                   Z-ADD     *zeros        VATNSP
046000010612     C                   EVAL      vinmsg = %trimr(vinmsg)
046100010612     C                             + ' ' + 'VATNSP'
046200010612     C                   ELSE
046300010612     C                   MOVE      a_vatnsp      VATNSP
046400010612     C                   ENDIF
046500010612     C*
046600010612     C*
046700010612     C* ALFABETICI
046800010612     C                   MOVE      a_vatatb      VATATB
046900010612     C                   MOVE      a_vattrc      VATTRC
047000020725     C                   EVAL      VATNOT = %trim(a_vatnot)
047100010330     C*
047200010330     C                   ENDSR
047300010612     C*----------------------------------------------------*
047400990920
047500060504
047600060504
047700060504     C*----------------------------------------------------*
047800060504     C*  CONTROLLO NUMERICITA' CAMPI
047900060504     C*----------------------------------------------------*
048000060504     C     CHKNUM        BEGSR
048100060504     C*
048200060504     C                   IF        PiDecChr = *blanks
048300060504     C                   EVAL      PiDecChr = ','
048400060504     C                   ENDIF
048500060504     C*
048600060504     C                   CALL(e)   'ISNUMERIC'
048700060504     C                   PARM                    PiStr            30
048800060504     C                   PARM                    PiDecChr          1
048900060504     C                   PARM      *ZEROS        PiVal            30 9
049000060504     C                   PARM      '0'           PiInt             1
049100060504     C                   PARM      '0'           PiNum             1
049200060504     C                   IF        %error
049300060504     C                   EVAL      PiNum=*off
049400060504     C                   ENDIF
049500060504     C*
049600060504     C                   ENDSR
049700060504     C***
049800010601
049900010601
050000010601
050100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
050200010601     C     repfil        BEGSR
050300010601     C*
050400010601     C                   if        invfil = *zeros and
050500010601     C                             depfil > *zeros and
050600010613     C                             (vinflg = *blanks or
050700010613     C                              vinflg = *zeros)
050800010601     C
050900010601     C                   eval      invfil = depfil
051000010601     C                   endif
051100010601     C*
051200010601     C                   if        depfil <> invfil and
051300010601     C                             invfil > *zeros
051400010601     C                   eval      flgMulti = '1'
051500010601     C                   if        vinflg = *blanks
051600010601     C                   add       1             cntNonEl
051700010601     C                   endif
051800010601     C                   endif
051900010601     C*
052000010601     C                   if        vinflg = '2'
052100010601     C                   eval      flgStato = '2'
052200010601     C                   endif
052300010601     C*
052400010601     C                   ENDSR
052500010601     C***
052600010601
052700010601
052800010601
052900010330
053000010330
053100010330
053200990920      /TITLE Invio dei dati al punto operativo.
053300000613     C     invio         BEGSR
053400990920     C*
053500990920     C                   reset                   dscmz
053600010601     C                   move      invfil        cmzdst
053700990920     C                   eval      cmzfld = vlrfou
053800990920     C                   eval      cmzmbd = vlrhdl
053900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
054000000710     C                   if        prmfir = *blanks
054100020916     C                   eval      cmzfla = 'FIVAT00F'
054200020916     C                   eval      cmzmba = 'FIVAT00F'
054300000710     C                   else
054400000710     C                   eval      cmzfla = prmfir
054500000710     C                   eval      cmzmba = prmfir
054600000710     C                   endif
054700990920     C                   eval      cmznrr = *zeros
054800990920     C                   move      §ctrok        cmznrr
054900021003     C                   eval      cmzlba = vlrfl1
055000990920     C                   call(e)   'TIS711C'
055100990920     C                   parm                    dscmz
055200990921     C                   parm      *blanks       esito
055300990920     C                   if        %error
055400990920     C                             or cmzerr = '1'
055500990921     C                             or esito  = '1'
055600000710     C                   eval      wrkesito = '3'
055700990920     C                   endif
055800990920     C*
055900000613     C                   ENDSR
056000990910
056100010601
056200010601
056300010601
056400010601
056500010601      /TITLE Invio dei dati al punto operativo.
056600010601     C     opeini        BEGSR
056700010601     C*
056800010601     C* Inizializzo flag e contatori operativi
056900010601     C                   movel     '0'           flgGiro           1
057000010601     C                   movel     '0'           flgMulti          1
057100010601     C                   movel     '1'           flgStato          1
057200010615     C                   movel     '0'           flgOk             1
057300010601     C                   z-add     *zeros        cntNonEl         10 0
057400010601     C                   z-add     *zeros        depfil            3 0
057500010601     C                   z-add     *zeros        invfil            3 0
057600030407     C*
057700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
057800030407     C                   if        %subst(vlrppt:1:1) = 'S'
057900030407     C                   seton                                        50
058000030407     C                   else
058100030407     C                   setoff                                       50
058200030407     C                   endif
058300010601     C*
058400010601     C                   ENDSR
058500010601     C***
058600010601
058700010601
058800010601
058900010330
059000010330
059100000613     C     *inzsr        BEGSR
059200990910     C*
059300990910     C     *entry        plist
059400990920     C                   parm                    tivlrds
059500990921     C                   parm      wrkesito      esito
059600000724     C                   parm                    prmlit
059700000710     C                   parm                    prmfir
059800010330     C*
059900010330     C* CALCOLA LA DATA CORRENTE
060000010330     C                   time                    wn14             14 0
060100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060200010330     C                   z-add     wn8           g08dat
060300010330     C                   z-add     *zeros        g08inv
060400010330     C                   movel     '0'           g08err
060500010330     C                   call      'XSRDA8'
060600010330     C                   parm                    wlbda8
060700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060800000613     C*
060900000613     C                   ENDSR
061000000613     C***
061100990908
061200010612     Otis7t6p   E            riepilogo         2
061300990915     O                                              'Upload via Internet'
061400990915     O                                           +1 'Traduzione TIVIN00R -'
061500020916     O                                           +1 'FIVATWWR'
061600990915     O                                           +1 'Testate bolle'
061700990915     O          E            riepilogo   2
061800990915     O                       wrkdata
061900990915     O                       wrkora              +1
062000990915     O                       procname            +1
062100990915     O          E            riepilogo   2
062200990915     O                                              'Cliente..................:'
062300010612     O                       VATCCM        z     +1
062400990915     O          E            riepilogo   2
062500990920     O                                              'Riferimento Strategi.....:'
062600990920     O                       vlrhdl              +1
062700990915     O          E            riepilogo   2
062800990915     O                                              'Giusti...................:'
062900971022     O                       §CTROK        2   +  1
063000990915     O          E            riepilogo   2
063100990915     O                                              'Sbagliati e corretti.....:'
063200971022     O                       §CTRMO        2   +  1
063300990915     O          E            riepilogo   2
063400990915     O                                              'Sbagliati e scartati.....:'
063500971022     O                       §CTRNO        2   +  1
063600000613
063700010612     Otis7t6ps  E            testdett          2
063800000613     O                                              'Upload via Internet'
063900000613     O                                           +1 'Traduzione TIVIN00R -'
064000020916     O                                           +1 'FIVATWWR'
064100000613     O                                           +1 'Testate bolle'
064200000616     O          E            testdett    3
064300000613     O                                           +2 'N° rec'
064400000613     O                                           +3 'Anteprima contenuto'
064500000616     O          E            rigadett    2
064600000613     O                       rrnum               +2
064700000621     O                       recko               +3
064800000616     O          E            findett     2
064900000613     O                       wrkdata
065000000613     O                       wrkora              +1
065100000613     O                       procname            +1
065200000616     O          E            findett     2
065300000613     O                                              'Cliente..................:'
065400010612     O                       VATCCM        z     +1
065500000616     O          E            findett     2
065600000613     O                                              'Riferimento Strategi.....:'
065700000613     O                       vlrhdl              +1
065800000616     O          E            findett     2
065900000613     O                                              'Giusti...................:'
066000000613     O                       §CTROK        2   +  1
066100000616     O          E            findett     2
066200000613     O                                              'Sbagliati e corretti.....:'
066300000613     O                       §CTRMO        2   +  1
066400000616     O          E            findett     2
066500000613     O                                              'Sbagliati e scartati.....:'
066600000613     O                       §CTRNO        2   +  1
066700000613** CMD - COMANDI CL
066800011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
066900011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
067000010612DLTOVR FILE(TIS7T6P TIS7T6PS) LVL(*)
067100010606
067200000613
