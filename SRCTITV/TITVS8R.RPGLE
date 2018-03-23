000100020916      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020916     FFIVATwwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400010330     D esito           s              1
001500010330     D prmlit          s             10
001600010330     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800010330     D rrnum           s              6  0 INZ(*zeros)
001900010330     D*------------------
002000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002100010330     D*------------------
002200010330     D WLBDA8          DS                  INZ
002300010330     D  G08DAT                 1      8  0
002400010330     D  G08INV                 9     16  0
002500010330     D  G08ERR                17     17
002600010330     D  G08TGI                18     22  0
002700010612     D*
002800010612     D fnvataar        ds                  inz
002900010612     D  a_vatatb               1      1
003000010612     D  a_vatccm               3      9
003100010612     D  a_vatlnp              11     13
003200010612     D  a_vataas              15     18
003300010612     D  a_vatnrs              20     21
003400010612     D  a_vatnsp              23     29
003500010612     D  a_vattrc              30     30
003600010612     D  a_vatnot              31     65
003700010330
003800010330
003900010330
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300010601     C*
004400010601     C                   exsr      opeini
004500010612     C                   exsr      rwvat
004600010601     C*
004700010601     C                   seton                                        lr
004800010601
004900010601
005000010601
005100010601
005200010601     C*--------------------------------------------------------
005300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005400010601     C*--------------------------------------------------------
005500010601     C     PREELA        BEGSR
005600010601     C*
005700010601     C* SE OCCORRE SPEDIRE IN FILIALE
005800010601     C                   if        invfil <> *zeros and
005900010601     C                             flgGiro = '0'
006000010601     C*
006100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006200010601     C                   eval      flgGiro = '1'
006300010601     C*
006400010601     C                   endif
006500010601     C*
006600010601     C                   ENDSR
006700010601     C***
006800010601
006900010601
007000010601
007100010601
007200010601
007300010601
007400010601
007500010601     C*--------------------------------------------------------
007600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007700010601     C*--------------------------------------------------------
007800010601     C     ENDELA        BEGSR
007900000616     C*
008000010601     C                   ENDSR
008100010601     C***
008200000613
008300010330
008400010601
008500010601
008600010601
008700010330     C*--------------------------------------------------------
008800020916     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
008900010330     C*--------------------------------------------------------
009000010612     C     RWVAT         BEGSR
009100010330     C*
009200010330     C                   if        not %open(tivin00r)
009300010330     C                   open      tivin00r
009400010330     C                   endif
009500020916     C                   if        not %open(fivatwwr)
009600020916     C                   open      fivatwwr
009700010330     C                   endif
009800010330     C*
009900010604     C                   clear                   §CTROK
010000010604     C                   clear                   §CTRMO
010100010604     C                   clear                   §CTRNO
010200010330     C*
010300010330     C                   DO        *HIVAL
010400010330     C*
010500010330     C                   READ      tivin00r                               70
010600010618     C*
010700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010800010618     C                   if        vinflg = '1'
010900010618     C                   eval      flgOk = '1'
011000010618     C                   endif
011100010618     C*
011200010330     C                   if        vindta > *blanks
011300010330     C                   add       1             rrnum
011400010330     C*
011500010601     C                   if        *in70 = *off and
011600010330     C                             (vinflg = *blanks
011700010330     C                              or vinflg = '0'
011800010330     C                              or vinflg = '2')
011900010711     C*
012000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012100010711     C                   if        vinflg = *blanks or vinflg = '0'
012200010711     C                   clear                   vinmsg
012300010711     C                   endif
012400010711     C*
012500010613     C                   eval      fnvataar = vindta
012600010601     C*
012700010330     C                   exsr      inzvar
012800010330     C                   exsr      defcam
012900010612     C                   exsr      impvat
013000060504     C                   exsr      forpar
013100060504     C                   exsr      chkdat
013200010601     C*
013300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013400010612     C                   eval      depfil = VATLNP
013500010601     C                   exsr      repfil
013600021010     C                   if        depfil = invfil
013700021014     C                   if        vlrpoi = 999
013800021010     C                   MOVE(P)   invfil        VATFGS
013900021014     C                   else
014000021014     C                   MOVE(P)   vlrpoi        VATFGS
014100021014     C                   endif
014200010601     C*
014300010601     C                   exsr      PREELA
014400010601     C*
014500010604     C* Ebbene...
014600010604     C*
014700010604     C  N31              ADD       1             §CTROK            7 0
014800010604     C   32              ADD       1             §CTRMO            7 0
014900010604     C   31              ADD       1             §CTRNO            7 0
015000020916     C  N31              WRITE     FIVAT000
015100010604     C*
015200010604     C                   if        *in31 = *off and
015300010604     C                             *in32 = *off
015400010604     C                   eval      vinflg = '1'
015500010604     C                   else
015600010604     C                   eval      vinflg = '2'
015700010604     C                   endif
015800010604     C                   endif
015900010601     C*
016000010604     C                   endif
016100010604     C*
016200010330     C                   else
016300010330     C                   eval      vinflg = '1'
016400010330     C                   endif
016500010601     C*
016600010601     C  N70              update    tivin000
016700010330     C*
016800010330     C  N70              ENDdo
016900010601     C
017000010601     C*
017100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017200010601     C                   if        cntNonEl = *zeros or
017300010601     C                             flgMulti = '0'
017400010330     C* Se non ci sono record con errori ...
017500010601     C                   if        §ctrno = 0 and
017600010604     C                             §ctrmo = 0 and
017700010601     C                             flgStato <> '2'
017800010330     C* ... restituisco esito OK.
017900010330     C                   eval      wrkesito = '0'
018000010330     C                   else
018100010330     C                   if        §ctrok > 0
018200010330     C                   eval      wrkesito = '1'
018300010330     C                   else
018400010615     C                   if        flgOk = '0'
018500010615     C                   eval      wrkesito = '2'
018600010615     C                   else
018700010615     C                   eval      wrkesito = '6'
018800010615     C                   endif
018900010330     C                   endif
019000010330     C                   endif
019100010601     C                   else
019200010601     C                   eval      wrkesito = '9'
019300010601     C                   endif
019400010330     C*
019500010330     C                   if        %open(tivin00r)
019600010330     C                   close     tivin00r
019700010330     C                   endif
019800020916     C                   if        %open(fivatwwr)
019900020916     C                   close     fivatwwr
020000010330     C                   endif
020100010601     C*
020200010601     C                   if        vlrpoi <> 999
020300010601     C                   eval      invfil = vlrpoi
020400010601     C                   endif
020500010330     C*
020600010330     C                   if        §ctrok > 0
020700010601     C                             and invfil > *zeros
020800010330     C                   exsr      invio
020900010330     C                   endif
021000010601     C*
021100010612     C                   if        flgGiro = '1'
021200010601     C                   exsr      endela
021300010612     C                   endif
021400010330     C*
021500010330     C                   ENDSR
021600010330     C***
021700010330
021800010601
021900010601
022000010601
022100010601
022200010330     C*----------------------------------------------------*
022300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022400010330     C*----------------------------------------------------*
022500010330     C     INZVAR        BEGSR
022600010330     C*
022700010330     C*
022800010330     C                   ENDSR
022900010330     C*----------------------------------------------------*
023000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023100010330     C*----------------------------------------------------*
023200010330     C     DEFCAM        BEGSR
023300010330     C*
023400020916     C                   CLEAR                   FIVAT000
023500010330     C*
023600010330     C                   ENDSR
023700060504     C*----------------------------------------------------*
023800060504     C*  FORZATURE DA PARAMETRI
023900060504     C*----------------------------------------------------*
024000060504     C     FORPAR        BEGSR
024100060504     C*
024200060504     C* Verifico i valori d default passati come parametri
024300060504     C                   IF        vlrppt > *blanks
024400060504     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
024500060504     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024600060504     C                   EXSR      CHKNUM
024700060504     C                   IF        PiInt=*on
024800060504     C                   Z-ADD     PiVal         VATCCM
024900060504     C                   ENDIF
025000060504     C                   ENDIF
025100060504     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
025200060504     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025300060504     C                   EXSR      CHKNUM
025400060504     C                   IF        PiInt=*on
025500060504     C                   Z-ADD     PiVal         VATLNP
025600060504     C                   ENDIF
025700060504     C                   ENDIF
025800060504     C                   IF        %trim(%subst(vlrppt:11:1)) <> *blanks
025900060504     C                   EVAL      VATTRC = %trim(%subst(vlrppt:11:1))
026000060504     C                   ENDIF
026100060504     C                   ENDIF
026200060504     C*
026300060504     C                   ENDSR
026400060504     C*----------------------------------------------------*
026500060504     C*  VERIFICA DATI ESSENZIALI
026600060504     C*----------------------------------------------------*
026700060504     C     CHKDAT        BEGSR
026800060504     C*
026900060504     C                   SETOFF                                       31
027000060504     C*
027100060504     C* CCM
027200060504     C                   IF        VATCCM = *zeros
027300060504     C                   SETON                                        31
027400060504     C                   ENDIF
027500060504     C*
027600060504     C* LNP
027700060504     C                   IF        VATLNP = *zeros
027800060504     C                   SETON                                        31
027900060504     C                   ENDIF
028000060504     C*
028100060504     C* AAS
028200060504     C                   IF        VATAAS = *zeros
028300060504     C                   SETON                                        31
028400060504     C                   ENDIF
028500060504     C*
028600060504     C* NSP
028700060504     C                   IF        VATNSP = *zeros
028800060504     C                   SETON                                        31
028900060504     C                   ENDIF
029000060504     C*
029100060504     C                   ENDSR
029200010330     C*----------------------------------------------------*
029300020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
029400010330     C*----------------------------------------------------*
029500010612     C     IMPVAT        BEGSR
029600010330     C*
029700010604     C                   SETOFF                                       3132
029800010612     C*
029900010612     C                   SETOFF                                       303132
030000010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
030100010612     C                   TESTN                   a_vatccm             30
030200010612     C  N30              TESTN                   a_vatccm               30
030300010612     C  N30              TESTN                   a_vatccm                 30
030400010612     C                   MOVE      a_vatccm      W001A             1
030500010612     C     *IN30         IFEQ      *OFF
030600010612     C     W001A         ORLT      '0'
030700010612     C                   SETON                                        31
030800010612     C                   Z-ADD     *zeros        VATCCM
030900010612     C                   EVAL      vinmsg = 'VATCCM'
031000010612     C                   ELSE
031100010612     C                   MOVE      a_vatccm      VATCCM
031200010612     C     VATCCM        IFLE      *ZEROS
031300010612     C                   SETON                                        31
031400010612     C                   Z-ADD     *zeros        VATCCM
031500010612     C                   EVAL      vinmsg = 'VATCCM'
031600010612     C                   ENDIF
031700010612     C                   ENDIF
031800010612     C*
031900010612     C                   TESTN                   a_vatlnp             30
032000010612     C  N30              TESTN                   a_vatlnp               30
032100010612     C  N30              TESTN                   a_vatlnp                 30
032200010612     C                   MOVE      a_vatlnp      W001A             1
032300010612     C     *IN30         IFEQ      *OFF
032400010612     C     W001A         ORLT      '0'
032500010612     C                   SETON                                        31
032600010612     C                   Z-ADD     *zeros        VATLNP
032700010612     C                   EVAL      vinmsg = %trimr(vinmsg)
032800021010     C                             + ' ' + 'VATLNP'
032900010612     C                   ELSE
033000010612     C                   MOVE      a_vatlnp      VATLNP
033100010612     C     VATLNP        IFLE      *ZEROS
033200010612     C                   SETON                                        31
033300010612     C                   Z-ADD     *zeros        VATLNP
033400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
033500021010     C                             + ' ' + 'VATLNP'
033600010612     C                   ENDIF
033700010612     C                   ENDIF
033800010612     C*
033900010612     C                   TESTN                   a_vataas             30
034000010612     C  N30              TESTN                   a_vataas               30
034100010612     C  N30              TESTN                   a_vataas                 30
034200010612     C                   MOVE      a_vataas      W001A             1
034300010612     C     *IN30         IFEQ      *OFF
034400010612     C     W001A         ORLT      '0'
034500010612     C                   SETON                                        31
034600010612     C                   MOVEL(P)  datcor        VATAAS
034700010612     C                   EVAL      vinmsg = %trimr(vinmsg)
034800010612     C                             + ' ' + 'VATAAS'
034900010612     C                   ELSE
035000010612     C                   MOVE      a_vataas      VATAAS
035100010612     C                   ENDIF
035200010612     C*
035300010612     C                   TESTN                   a_vatnrs             30
035400010612     C  N30              TESTN                   a_vatnrs               30
035500010612     C  N30              TESTN                   a_vatnrs                 30
035600010612     C                   MOVE      a_vatnrs      W001A             1
035700010612     C     *IN30         IFEQ      *OFF
035800010612     C     W001A         ORLT      '0'
035900010612     C     W001A         ANDNE     *BLANK
036000010612     C                   SETON                                        31
036100010612     C                   Z-ADD     *zeros        VATNRS
036200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
036300010612     C                             + ' ' + 'VATNRS'
036400010612     C                   ELSE
036500010612     C                   MOVE      a_vatnrs      VATNRS
036600010612     C                   ENDIF
036700010612     C*
036800010612     C                   TESTN                   a_vatnsp             30
036900010612     C  N30              TESTN                   a_vatnsp               30
037000010612     C  N30              TESTN                   a_vatnsp                 30
037100010612     C                   MOVE      a_vatnsp      W001A             1
037200010612     C     *IN30         IFEQ      *OFF
037300010612     C     W001A         ORLT      '0'
037400010612     C                   SETON                                        31
037500010612     C                   Z-ADD     *zeros        VATNSP
037600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
037700010612     C                             + ' ' + 'VATNSP'
037800010612     C                   ELSE
037900010612     C                   MOVE      a_vatnsp      VATNSP
038000010612     C                   ENDIF
038100010612     C*
038200010612     C*
038300010612     C* ALFABETICI
038400010612     C                   MOVE      a_vatatb      VATATB
038500010612     C                   MOVE      a_vattrc      VATTRC
038600020725     C                   EVAL      VATNOT = %trim(a_vatnot)
038700010330     C*
038800010330     C                   ENDSR
038900010612     C*----------------------------------------------------*
039000990920
039100060504
039200060504
039300060504     C*----------------------------------------------------*
039400060504     C*  CONTROLLO NUMERICITA' CAMPI
039500060504     C*----------------------------------------------------*
039600060504     C     CHKNUM        BEGSR
039700060504     C*
039800060504     C                   IF        PiDecChr = *blanks
039900060504     C                   EVAL      PiDecChr = ','
040000060504     C                   ENDIF
040100060504     C*
040200060504     C                   CALL(e)   'ISNUMERIC'
040300060504     C                   PARM                    PiStr            30
040400060504     C                   PARM                    PiDecChr          1
040500060504     C                   PARM      *ZEROS        PiVal            30 9
040600060504     C                   PARM      '0'           PiInt             1
040700060504     C                   PARM      '0'           PiNum             1
040800060504     C                   IF        %error
040900060504     C                   EVAL      PiNum=*off
041000060504     C                   ENDIF
041100060504     C*
041200060504     C                   ENDSR
041300060504     C***
041400010601
041500010601
041600010601
041700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
041800010601     C     repfil        BEGSR
041900010601     C*
042000010601     C                   if        invfil = *zeros and
042100010601     C                             depfil > *zeros and
042200010613     C                             (vinflg = *blanks or
042300010613     C                              vinflg = *zeros)
042400010601     C
042500010601     C                   eval      invfil = depfil
042600010601     C                   endif
042700010601     C*
042800010601     C                   if        depfil <> invfil and
042900010601     C                             invfil > *zeros
043000010601     C                   eval      flgMulti = '1'
043100010601     C                   if        vinflg = *blanks
043200010601     C                   add       1             cntNonEl
043300010601     C                   endif
043400010601     C                   endif
043500010601     C*
043600010601     C                   if        vinflg = '2'
043700010601     C                   eval      flgStato = '2'
043800010601     C                   endif
043900010601     C*
044000010601     C                   ENDSR
044100010601     C***
044200010601
044300010601
044400010601
044500010330
044600010330
044700010330
044800990920      /TITLE Invio dei dati al punto operativo.
044900000613     C     invio         BEGSR
045000990920     C*
045100990920     C                   reset                   dscmz
045200010601     C                   move      invfil        cmzdst
045300990920     C                   eval      cmzfld = vlrfou
045400990920     C                   eval      cmzmbd = vlrhdl
045500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
045600000710     C                   if        prmfir = *blanks
045700020916     C                   eval      cmzfla = 'FIVAT00F'
045800020916     C                   eval      cmzmba = 'FIVAT00F'
045900000710     C                   else
046000000710     C                   eval      cmzfla = prmfir
046100000710     C                   eval      cmzmba = prmfir
046200000710     C                   endif
046300990920     C                   eval      cmznrr = *zeros
046400990920     C                   move      §ctrok        cmznrr
046500021003     C                   eval      cmzlba = vlrfl1
046600990920     C                   call(e)   'TIS711C'
046700990920     C                   parm                    dscmz
046800990921     C                   parm      *blanks       esito
046900990920     C                   if        %error
047000990920     C                             or cmzerr = '1'
047100990921     C                             or esito  = '1'
047200000710     C                   eval      wrkesito = '3'
047300990920     C                   endif
047400990920     C*
047500000613     C                   ENDSR
047600990910
047700010601
047800010601
047900010601
048000010601
048100010601      /TITLE Invio dei dati al punto operativo.
048200010601     C     opeini        BEGSR
048300010601     C*
048400010601     C* Inizializzo flag e contatori operativi
048500010601     C                   movel     '0'           flgGiro           1
048600010601     C                   movel     '0'           flgMulti          1
048700010601     C                   movel     '1'           flgStato          1
048800010615     C                   movel     '0'           flgOk             1
048900010601     C                   z-add     *zeros        cntNonEl         10 0
049000010601     C                   z-add     *zeros        depfil            3 0
049100010601     C                   z-add     *zeros        invfil            3 0
049200030407     C*
049300030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
049400030407     C                   if        %subst(vlrppt:1:1) = 'S'
049500030407     C                   seton                                        50
049600030407     C                   else
049700030407     C                   setoff                                       50
049800030407     C                   endif
049900010601     C*
050000010601     C                   ENDSR
050100010601     C***
050200010601
050300010601
050400010601
050500010330
050600010330
050700000613     C     *inzsr        BEGSR
050800990910     C*
050900990910     C     *entry        plist
051000990920     C                   parm                    tivlrds
051100990921     C                   parm      wrkesito      esito
051200000724     C                   parm                    prmlit
051300000710     C                   parm                    prmfir
051400010330     C*
051500010330     C* CALCOLA LA DATA CORRENTE
051600010330     C                   time                    wn14             14 0
051700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
051800010330     C                   z-add     wn8           g08dat
051900010330     C                   z-add     *zeros        g08inv
052000010330     C                   movel     '0'           g08err
052100010330     C                   call      'XSRDA8'
052200010330     C                   parm                    wlbda8
052300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
052400000613     C*
052500000613     C                   ENDSR
052600000613     C***
