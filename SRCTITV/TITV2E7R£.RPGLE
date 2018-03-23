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
021000010601     C*
021100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021200010612     C                   eval      depfil = VATLNP
021300010601     C                   exsr      repfil
021400021010     C                   if        depfil = invfil
021500021014     C                   if        vlrpoi = 999
021600021010     C                   MOVE(P)   invfil        VATFGS
021700021014     C                   else
021800021014     C                   MOVE(P)   vlrpoi        VATFGS
021900021014     C                   endif
022000010601     C*
022100010601     C                   exsr      PREELA
022200110429     C*
022300110429     C* Forzature x diversa gestione traffici ILLY
022400110429     C*
022500110429     C* CCM
022600110429     C                   MOVEL     VATNSP        wCLI              1
022700110429     C                   IF        wCLI = '7'
022800110429     C                   EVAL      VATCCM = 1162074
022900110429     C                   ENDIF
023000010601     C*
023100010604     C* Ebbene...
023200010604     C*
023300010604     C  N31              ADD       1             §CTROK            7 0
023400010604     C   32              ADD       1             §CTRMO            7 0
023500010604     C   31              ADD       1             §CTRNO            7 0
023600020916     C  N31              WRITE     FIVAT000
023700010604     C*
023800010604     C                   if        *in31 = *off and
023900010604     C                             *in32 = *off
024000010604     C                   eval      vinflg = '1'
024100010604     C                   else
024200010604     C                   eval      recko = vindta
024300010612     C                   if        %open(tis7t6ps)
024400030407     C   50              except    rigadett
024500010612     C                   endif
024600010604     C                   eval      vinflg = '2'
024700010604     C                   endif
024800010604     C                   endif
024900010601     C*
025000010604     C                   endif
025100010604     C*
025200010330     C                   else
025300010330     C                   eval      vinflg = '1'
025400010330     C                   endif
025500010601     C*
025600010601     C  N70              update    tivin000
025700010330     C*
025800010330     C  N70              ENDdo
025900010601     C
026000010601     C*
026100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026200010601     C                   if        cntNonEl = *zeros or
026300010601     C                             flgMulti = '0'
026400010330     C* Se non ci sono record con errori ...
026500010601     C                   if        §ctrno = 0 and
026600010604     C                             §ctrmo = 0 and
026700010601     C                             flgStato <> '2'
026800010330     C* ... restituisco esito OK.
026900010330     C                   eval      wrkesito = '0'
027000010330     C                   else
027100010330     C                   if        §ctrok > 0
027200010330     C                   eval      wrkesito = '1'
027300010330     C                   else
027400010615     C                   if        flgOk = '0'
027500010615     C                   eval      wrkesito = '2'
027600010615     C                   else
027700010615     C                   eval      wrkesito = '6'
027800010615     C                   endif
027900010330     C                   endif
028000010330     C                   endif
028100010601     C                   else
028200010601     C                   eval      wrkesito = '9'
028300010601     C                   endif
028400010330     C*
028500010330     C                   if        %open(tivin00r)
028600010330     C                   close     tivin00r
028700010330     C                   endif
028800020916     C                   if        %open(fivatwwr)
028900020916     C                   close     fivatwwr
029000010330     C                   endif
029100010601     C*
029200010601     C                   if        vlrpoi <> 999
029300010601     C                   eval      invfil = vlrpoi
029400010601     C                   endif
029500010330     C*
029600010330     C                   if        §ctrok > 0
029700010601     C                             and invfil > *zeros
029800010330     C                   exsr      invio
029900010330     C                   endif
030000010601     C*
030100010612     C                   if        flgGiro = '1'
030200010601     C                   exsr      endela
030300010612     C                   endif
030400010330     C*
030500010330     C                   ENDSR
030600010330     C***
030700010330
030800010601
030900010601
031000010601
031100010601
031200010330     C*----------------------------------------------------*
031300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031400010330     C*----------------------------------------------------*
031500010330     C     INZVAR        BEGSR
031600010330     C*
031700010330     C*
031800010330     C                   ENDSR
031900010330     C*----------------------------------------------------*
032000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
032100010330     C*----------------------------------------------------*
032200010330     C     DEFCAM        BEGSR
032300010330     C*
032400020916     C                   CLEAR                   FIVAT000
032500010330     C*
032600010330     C                   ENDSR
032700010330     C*----------------------------------------------------*
032800020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
032900010330     C*----------------------------------------------------*
033000010612     C     IMPVAT        BEGSR
033100010330     C*
033200010604     C                   SETOFF                                       3132
033300010612     C*
033400010612     C                   SETOFF                                       303132
033500010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
033600010612     C                   TESTN                   a_vatccm             30
033700010612     C  N30              TESTN                   a_vatccm               30
033800010612     C  N30              TESTN                   a_vatccm                 30
033900010612     C                   MOVE      a_vatccm      W001A             1
034000010612     C     *IN30         IFEQ      *OFF
034100010612     C     W001A         ORLT      '0'
034200010612     C                   SETON                                        31
034300010612     C                   Z-ADD     *zeros        VATCCM
034400010612     C                   EVAL      vinmsg = 'VATCCM'
034500010612     C                   ELSE
034600010612     C                   MOVE      a_vatccm      VATCCM
034700010612     C     VATCCM        IFLE      *ZEROS
034800010612     C                   SETON                                        31
034900010612     C                   Z-ADD     *zeros        VATCCM
035000010612     C                   EVAL      vinmsg = 'VATCCM'
035100010612     C                   ENDIF
035200010612     C                   ENDIF
035300010612     C*
035400010612     C                   TESTN                   a_vatlnp             30
035500010612     C  N30              TESTN                   a_vatlnp               30
035600010612     C  N30              TESTN                   a_vatlnp                 30
035700010612     C                   MOVE      a_vatlnp      W001A             1
035800010612     C     *IN30         IFEQ      *OFF
035900010612     C     W001A         ORLT      '0'
036000010612     C                   SETON                                        31
036100010612     C                   Z-ADD     *zeros        VATLNP
036200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
036300021010     C                             + ' ' + 'VATLNP'
036400010612     C                   ELSE
036500010612     C                   MOVE      a_vatlnp      VATLNP
036600010612     C     VATLNP        IFLE      *ZEROS
036700010612     C                   SETON                                        31
036800010612     C                   Z-ADD     *zeros        VATLNP
036900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
037000021010     C                             + ' ' + 'VATLNP'
037100010612     C                   ENDIF
037200010612     C                   ENDIF
037300010612     C*
037400010612     C                   TESTN                   a_vataas             30
037500010612     C  N30              TESTN                   a_vataas               30
037600010612     C  N30              TESTN                   a_vataas                 30
037700010612     C                   MOVE      a_vataas      W001A             1
037800010612     C     *IN30         IFEQ      *OFF
037900010612     C     W001A         ORLT      '0'
038000010612     C                   SETON                                        31
038100010612     C                   MOVEL(P)  datcor        VATAAS
038200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
038300010612     C                             + ' ' + 'VATAAS'
038400010612     C                   ELSE
038500010612     C                   MOVE      a_vataas      VATAAS
038600010612     C                   ENDIF
038700010612     C*
038800010612     C                   TESTN                   a_vatnrs             30
038900010612     C  N30              TESTN                   a_vatnrs               30
039000010612     C  N30              TESTN                   a_vatnrs                 30
039100010612     C                   MOVE      a_vatnrs      W001A             1
039200010612     C     *IN30         IFEQ      *OFF
039300010612     C     W001A         ORLT      '0'
039400010612     C     W001A         ANDNE     *BLANK
039500010612     C                   SETON                                        31
039600010612     C                   Z-ADD     *zeros        VATNRS
039700010612     C                   EVAL      vinmsg = %trimr(vinmsg)
039800010612     C                             + ' ' + 'VATNRS'
039900010612     C                   ELSE
040000010612     C                   MOVE      a_vatnrs      VATNRS
040100010612     C                   ENDIF
040200010612     C*
040300010612     C                   TESTN                   a_vatnsp             30
040400010612     C  N30              TESTN                   a_vatnsp               30
040500010612     C  N30              TESTN                   a_vatnsp                 30
040600010612     C                   MOVE      a_vatnsp      W001A             1
040700010612     C     *IN30         IFEQ      *OFF
040800010612     C     W001A         ORLT      '0'
040900010612     C                   SETON                                        31
041000010612     C                   Z-ADD     *zeros        VATNSP
041100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
041200010612     C                             + ' ' + 'VATNSP'
041300010612     C                   ELSE
041400010612     C                   MOVE      a_vatnsp      VATNSP
041500010612     C                   ENDIF
041600010612     C*
041700010612     C*
041800010612     C* ALFABETICI
041900010612     C                   MOVE      a_vatatb      VATATB
042000010612     C                   MOVE      a_vattrc      VATTRC
042100020725     C                   EVAL      VATNOT = %trim(a_vatnot)
042200010330     C*
042300010330     C                   ENDSR
042400010612     C*----------------------------------------------------*
042500990920
042600010601
042700010601
042800010601
042900010601
043000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
043100010601     C     repfil        BEGSR
043200010601     C*
043300010601     C                   if        invfil = *zeros and
043400010601     C                             depfil > *zeros and
043500010613     C                             (vinflg = *blanks or
043600010613     C                              vinflg = *zeros)
043700010601     C
043800010601     C                   eval      invfil = depfil
043900010601     C                   endif
044000010601     C*
044100010601     C                   if        depfil <> invfil and
044200010601     C                             invfil > *zeros
044300010601     C                   eval      flgMulti = '1'
044400010601     C                   if        vinflg = *blanks
044500010601     C                   add       1             cntNonEl
044600010601     C                   endif
044700010601     C                   endif
044800010601     C*
044900010601     C                   if        vinflg = '2'
045000010601     C                   eval      flgStato = '2'
045100010601     C                   endif
045200010601     C*
045300010601     C                   ENDSR
045400010601     C***
045500010601
045600010601
045700010601
045800010330
045900010330
046000010330
046100990920      /TITLE Invio dei dati al punto operativo.
046200000613     C     invio         BEGSR
046300990920     C*
046400990920     C                   reset                   dscmz
046500010601     C                   move      invfil        cmzdst
046600990920     C                   eval      cmzfld = vlrfou
046700990920     C                   eval      cmzmbd = vlrhdl
046800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
046900000710     C                   if        prmfir = *blanks
047000020916     C                   eval      cmzfla = 'FIVAT00F'
047100020916     C                   eval      cmzmba = 'FIVAT00F'
047200000710     C                   else
047300000710     C                   eval      cmzfla = prmfir
047400000710     C                   eval      cmzmba = prmfir
047500000710     C                   endif
047600990920     C                   eval      cmznrr = *zeros
047700990920     C                   move      §ctrok        cmznrr
047800021003     C                   eval      cmzlba = vlrfl1
047900990920     C                   call(e)   'TIS711C'
048000990920     C                   parm                    dscmz
048100990921     C                   parm      *blanks       esito
048200990920     C                   if        %error
048300990920     C                             or cmzerr = '1'
048400990921     C                             or esito  = '1'
048500000710     C                   eval      wrkesito = '3'
048600990920     C                   endif
048700990920     C*
048800000613     C                   ENDSR
048900990910
049000010601
049100010601
049200010601
049300010601
049400010601      /TITLE Invio dei dati al punto operativo.
049500010601     C     opeini        BEGSR
049600010601     C*
049700010601     C* Inizializzo flag e contatori operativi
049800010601     C                   movel     '0'           flgGiro           1
049900010601     C                   movel     '0'           flgMulti          1
050000010601     C                   movel     '1'           flgStato          1
050100010615     C                   movel     '0'           flgOk             1
050200010601     C                   z-add     *zeros        cntNonEl         10 0
050300010601     C                   z-add     *zeros        depfil            3 0
050400010601     C                   z-add     *zeros        invfil            3 0
050500030407     C*
050600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
050700030407     C                   if        %subst(vlrppt:1:1) = 'S'
050800030407     C                   seton                                        50
050900030407     C                   else
051000030407     C                   setoff                                       50
051100030407     C                   endif
051200010601     C*
051300010601     C                   ENDSR
051400010601     C***
051500010601
051600010601
051700010601
051800010330
051900010330
052000000613     C     *inzsr        BEGSR
052100990910     C*
052200990910     C     *entry        plist
052300990920     C                   parm                    tivlrds
052400990921     C                   parm      wrkesito      esito
052500000724     C                   parm                    prmlit
052600000710     C                   parm                    prmfir
052700010330     C*
052800010330     C* CALCOLA LA DATA CORRENTE
052900010330     C                   time                    wn14             14 0
053000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053100010330     C                   z-add     wn8           g08dat
053200010330     C                   z-add     *zeros        g08inv
053300010330     C                   movel     '0'           g08err
053400010330     C                   call      'XSRDA8'
053500010330     C                   parm                    wlbda8
053600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
053700000613     C*
053800000613     C                   ENDSR
053900000613     C***
054000990908
054100010612     Otis7t6p   E            riepilogo         2
054200990915     O                                              'Upload via Internet'
054300990915     O                                           +1 'Traduzione TIVIN00R -'
054400020916     O                                           +1 'FIVATWWR'
054500990915     O                                           +1 'Testate bolle'
054600990915     O          E            riepilogo   2
054700990915     O                       wrkdata
054800990915     O                       wrkora              +1
054900990915     O                       procname            +1
055000990915     O          E            riepilogo   2
055100990915     O                                              'Cliente..................:'
055200010612     O                       VATCCM        z     +1
055300990915     O          E            riepilogo   2
055400990920     O                                              'Riferimento Strategi.....:'
055500990920     O                       vlrhdl              +1
055600990915     O          E            riepilogo   2
055700990915     O                                              'Giusti...................:'
055800971022     O                       §CTROK        2   +  1
055900990915     O          E            riepilogo   2
056000990915     O                                              'Sbagliati e corretti.....:'
056100971022     O                       §CTRMO        2   +  1
056200990915     O          E            riepilogo   2
056300990915     O                                              'Sbagliati e scartati.....:'
056400971022     O                       §CTRNO        2   +  1
056500000613
056600010612     Otis7t6ps  E            testdett          2
056700000613     O                                              'Upload via Internet'
056800000613     O                                           +1 'Traduzione TIVIN00R -'
056900020916     O                                           +1 'FIVATWWR'
057000000613     O                                           +1 'Testate bolle'
057100000616     O          E            testdett    3
057200000613     O                                           +2 'N° rec'
057300000613     O                                           +3 'Anteprima contenuto'
057400000616     O          E            rigadett    2
057500000613     O                       rrnum               +2
057600000621     O                       recko               +3
057700000616     O          E            findett     2
057800000613     O                       wrkdata
057900000613     O                       wrkora              +1
058000000613     O                       procname            +1
058100000616     O          E            findett     2
058200000613     O                                              'Cliente..................:'
058300010612     O                       VATCCM        z     +1
058400000616     O          E            findett     2
058500000613     O                                              'Riferimento Strategi.....:'
058600000613     O                       vlrhdl              +1
058700000616     O          E            findett     2
058800000613     O                                              'Giusti...................:'
058900000613     O                       §CTROK        2   +  1
059000000616     O          E            findett     2
059100000613     O                                              'Sbagliati e corretti.....:'
059200000613     O                       §CTRMO        2   +  1
059300000616     O          E            findett     2
059400000613     O                                              'Sbagliati e scartati.....:'
059500000613     O                       §CTRNO        2   +  1
059600000613** CMD - COMANDI CL
059700011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
059800011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
059900010612DLTOVR FILE(TIS7T6P TIS7T6PS) LVL(*)
060000010606
060100000613
