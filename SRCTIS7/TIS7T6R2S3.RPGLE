000100020916      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
001900010612     D*
002000010612     D fnvataar        ds                  inz
002100010612     D  a_vatatb               1      1
002200010612     D  a_vatccm               3      9
002300010612     D  a_vatlnp              11     13
002400010612     D  a_vataas              15     18
002500010612     D  a_vatnrs              20     21
002600010612     D  a_vatnsp              23     29
002700010612     D  a_vattrc              30     30
002800010612     D  a_vatnot              31     65
002900100301
003000100301     D*------------------
003100100301     D* DS FIVAT DA EASYWEB
003200100301     D*------------------
003300100301     D ds_easyweb      ds
003400100301     D   ds_vat                            like(fnvataar)
003500100301     D   a_filler1                    1
003600100301     D   a_vatfgs                          like(a_vatlnp)
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
012500100301     C                   eval      ds_easyweb = vindta
012600100301     C                   eval      fnvataar   = ds_vat
012700010601     C*
012800010330     C                   exsr      inzvar
012900010330     C                   exsr      defcam
013000010612     C                   exsr      impvat
013100010601     C*
013200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013300100301     C                   eval      depfil = VATFGS
013400010601     C                   exsr      repfil
013500021010     C                   if        depfil = invfil
013600021014     C                   if        vlrpoi = 999
013700021010     C                   MOVE(P)   invfil        VATFGS
013800021014     C                   else
013900021014     C                   MOVE(P)   vlrpoi        VATFGS
014000021014     C                   endif
014100010601     C*
014200010601     C                   exsr      PREELA
014300010601     C*
014400010604     C* Ebbene...
014500010604     C*
014600010604     C  N31              ADD       1             §CTROK            7 0
014700010604     C   32              ADD       1             §CTRMO            7 0
014800010604     C   31              ADD       1             §CTRNO            7 0
014900110418     C*
015000110418     C* Verifico se sui parametri del traduttore è richiesta l'esclusione del 1020005
015100110418     C                   IF        VATCCM = 1020005 and %subst(vlrppt:2:1) = 'S'
015200110418     C                   ELSE
015300020916     C  N31              WRITE     FIVAT000
015400110418     C                   ENDIF
015500010604     C*
015600010604     C                   if        *in31 = *off and
015700010604     C                             *in32 = *off
015800010604     C                   eval      vinflg = '1'
015900010604     C                   else
016000010604     C                   eval      vinflg = '2'
016100010604     C                   endif
016200010604     C                   endif
016300010601     C*
016400010604     C                   endif
016500010604     C*
016600010330     C                   else
016700010330     C                   eval      vinflg = '1'
016800010330     C                   endif
016900010601     C*
017000010601     C  N70              update    tivin000
017100010330     C*
017200010330     C  N70              ENDdo
017300010601     C
017400010601     C*
017500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017600010601     C                   if        cntNonEl = *zeros or
017700010601     C                             flgMulti = '0'
017800010330     C* Se non ci sono record con errori ...
017900010601     C                   if        §ctrno = 0 and
018000010604     C                             §ctrmo = 0 and
018100010601     C                             flgStato <> '2'
018200010330     C* ... restituisco esito OK.
018300010330     C                   eval      wrkesito = '0'
018400010330     C                   else
018500010330     C                   if        §ctrok > 0
018600010330     C                   eval      wrkesito = '1'
018700010330     C                   else
018800010615     C                   if        flgOk = '0'
018900010615     C                   eval      wrkesito = '2'
019000010615     C                   else
019100010615     C                   eval      wrkesito = '6'
019200010615     C                   endif
019300010330     C                   endif
019400010330     C                   endif
019500010601     C                   else
019600010601     C                   eval      wrkesito = '9'
019700010601     C                   endif
019800010330     C*
019900010330     C                   if        %open(tivin00r)
020000010330     C                   close     tivin00r
020100010330     C                   endif
020200020916     C                   if        %open(fivatwwr)
020300020916     C                   close     fivatwwr
020400010330     C                   endif
020500010601     C*
020600010601     C                   if        vlrpoi <> 999
020700010601     C                   eval      invfil = vlrpoi
020800010601     C                   endif
020900010330     C*
021000010330     C                   if        §ctrok > 0
021100010601     C                             and invfil > *zeros
021200010330     C                   exsr      invio
021300010330     C                   endif
021400010601     C*
021500010612     C                   if        flgGiro = '1'
021600010601     C                   exsr      endela
021700010612     C                   endif
021800010330     C*
021900010330     C                   ENDSR
022000010330     C***
022100010330
022200010601
022300010601
022400010601
022500010601
022600010330     C*----------------------------------------------------*
022700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022800010330     C*----------------------------------------------------*
022900010330     C     INZVAR        BEGSR
023000010330     C*
023100010330     C*
023200010330     C                   ENDSR
023300010330     C*----------------------------------------------------*
023400010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023500010330     C*----------------------------------------------------*
023600010330     C     DEFCAM        BEGSR
023700010330     C*
023800020916     C                   CLEAR                   FIVAT000
023900010330     C*
024000010330     C                   ENDSR
024100010330     C*----------------------------------------------------*
024200020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
024300010330     C*----------------------------------------------------*
024400010612     C     IMPVAT        BEGSR
024500010330     C*
024600010604     C                   SETOFF                                       3132
024700010612     C*
024800010612     C                   SETOFF                                       303132
024900010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
025000010612     C                   TESTN                   a_vatccm             30
025100010612     C  N30              TESTN                   a_vatccm               30
025200010612     C  N30              TESTN                   a_vatccm                 30
025300010612     C                   MOVE      a_vatccm      W001A             1
025400010612     C     *IN30         IFEQ      *OFF
025500010612     C     W001A         ORLT      '0'
025600010612     C                   SETON                                        31
025700010612     C                   Z-ADD     *zeros        VATCCM
025800010612     C                   EVAL      vinmsg = 'VATCCM'
025900010612     C                   ELSE
026000010612     C                   MOVE      a_vatccm      VATCCM
026100010612     C     VATCCM        IFLE      *ZEROS
026200010612     C                   SETON                                        31
026300010612     C                   Z-ADD     *zeros        VATCCM
026400010612     C                   EVAL      vinmsg = 'VATCCM'
026500010612     C                   ENDIF
026600010612     C                   ENDIF
026700010612     C*
026800010612     C                   TESTN                   a_vatlnp             30
026900010612     C  N30              TESTN                   a_vatlnp               30
027000010612     C  N30              TESTN                   a_vatlnp                 30
027100010612     C                   MOVE      a_vatlnp      W001A             1
027200010612     C     *IN30         IFEQ      *OFF
027300010612     C     W001A         ORLT      '0'
027400010612     C                   SETON                                        31
027500010612     C                   Z-ADD     *zeros        VATLNP
027600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
027700021010     C                             + ' ' + 'VATLNP'
027800010612     C                   ELSE
027900010612     C                   MOVE      a_vatlnp      VATLNP
028000010612     C     VATLNP        IFLE      *ZEROS
028100010612     C                   SETON                                        31
028200010612     C                   Z-ADD     *zeros        VATLNP
028300010612     C                   EVAL      vinmsg = %trimr(vinmsg)
028400021010     C                             + ' ' + 'VATLNP'
028500010612     C                   ENDIF
028600010612     C                   ENDIF
028700100301     C*
028800100301     C                   TESTN                   a_vatfgs             30
028900100301     C  N30              TESTN                   a_vatfgs               30
029000100301     C  N30              TESTN                   a_vatfgs                 30
029100100301     C                   MOVE      a_vatfgs      W001A             1
029200100301     C     *IN30         IFEQ      *OFF
029300100301     C     W001A         ORLT      '0'
029400100301     C                   SETON                                        31
029500100301     C                   Z-ADD     *zeros        VATFGS
029600100301     C                   EVAL      vinmsg = %trimr(vinmsg)
029700100301     C                             + ' ' + 'VATFGS'
029800100301     C                   ELSE
029900100301     C                   MOVE      a_vatfgs      VATFGS
030000100301     C     VATFGS        IFLE      *ZEROS
030100100301     C                   SETON                                        31
030200100301     C                   Z-ADD     *zeros        VATFGS
030300100301     C                   EVAL      vinmsg = %trimr(vinmsg)
030400100301     C                             + ' ' + 'VATFGS'
030500100301     C                   ENDIF
030600100301     C                   ENDIF
030700010612     C*
030800010612     C                   TESTN                   a_vataas             30
030900010612     C  N30              TESTN                   a_vataas               30
031000010612     C  N30              TESTN                   a_vataas                 30
031100010612     C                   MOVE      a_vataas      W001A             1
031200010612     C     *IN30         IFEQ      *OFF
031300010612     C     W001A         ORLT      '0'
031400010612     C                   SETON                                        31
031500010612     C                   MOVEL(P)  datcor        VATAAS
031600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
031700010612     C                             + ' ' + 'VATAAS'
031800010612     C                   ELSE
031900010612     C                   MOVE      a_vataas      VATAAS
032000010612     C                   ENDIF
032100010612     C*
032200010612     C                   TESTN                   a_vatnrs             30
032300010612     C  N30              TESTN                   a_vatnrs               30
032400010612     C  N30              TESTN                   a_vatnrs                 30
032500010612     C                   MOVE      a_vatnrs      W001A             1
032600010612     C     *IN30         IFEQ      *OFF
032700010612     C     W001A         ORLT      '0'
032800010612     C     W001A         ANDNE     *BLANK
032900010612     C                   SETON                                        31
033000010612     C                   Z-ADD     *zeros        VATNRS
033100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
033200010612     C                             + ' ' + 'VATNRS'
033300010612     C                   ELSE
033400010612     C                   MOVE      a_vatnrs      VATNRS
033500010612     C                   ENDIF
033600010612     C*
033700010612     C                   TESTN                   a_vatnsp             30
033800010612     C  N30              TESTN                   a_vatnsp               30
033900010612     C  N30              TESTN                   a_vatnsp                 30
034000010612     C                   MOVE      a_vatnsp      W001A             1
034100010612     C     *IN30         IFEQ      *OFF
034200010612     C     W001A         ORLT      '0'
034300010612     C                   SETON                                        31
034400010612     C                   Z-ADD     *zeros        VATNSP
034500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
034600010612     C                             + ' ' + 'VATNSP'
034700010612     C                   ELSE
034800010612     C                   MOVE      a_vatnsp      VATNSP
034900010612     C                   ENDIF
035000010612     C*
035100010612     C*
035200010612     C* ALFABETICI
035300010612     C                   MOVE      a_vatatb      VATATB
035400010612     C                   MOVE      a_vattrc      VATTRC
035500131018     C                   IF        a_vattrc <> 'S'
035600020725     C                   EVAL      VATNOT = %trim(a_vatnot)
035700131023     C                   ELSE
035800131023     C                   EVAL      VATNOT = a_vatnot
035900131018     C                   ENDIF
036000010330     C*
036100010330     C                   ENDSR
036200010612     C*----------------------------------------------------*
036300990920
036400010601
036500010601
036600010601
036700010601
036800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
036900010601     C     repfil        BEGSR
037000010601     C*
037100010601     C                   if        invfil = *zeros and
037200010601     C                             depfil > *zeros and
037300010613     C                             (vinflg = *blanks or
037400010613     C                              vinflg = *zeros)
037500010601     C
037600010601     C                   eval      invfil = depfil
037700010601     C                   endif
037800010601     C*
037900010601     C                   if        depfil <> invfil and
038000010601     C                             invfil > *zeros
038100010601     C                   eval      flgMulti = '1'
038200010601     C                   if        vinflg = *blanks
038300010601     C                   add       1             cntNonEl
038400010601     C                   endif
038500010601     C                   endif
038600010601     C*
038700010601     C                   if        vinflg = '2'
038800010601     C                   eval      flgStato = '2'
038900010601     C                   endif
039000010601     C*
039100010601     C                   ENDSR
039200010601     C***
039300010601
039400010601
039500010601
039600010330
039700010330
039800010330
039900990920      /TITLE Invio dei dati al punto operativo.
040000000613     C     invio         BEGSR
040100990920     C*
040200990920     C                   reset                   dscmz
040300010601     C                   move      invfil        cmzdst
040400990920     C                   eval      cmzfld = vlrfou
040500990920     C                   eval      cmzmbd = vlrhdl
040600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
040700000710     C                   if        prmfir = *blanks
040800020916     C                   eval      cmzfla = 'FIVAT00F'
040900020916     C                   eval      cmzmba = 'FIVAT00F'
041000000710     C                   else
041100000710     C                   eval      cmzfla = prmfir
041200000710     C                   eval      cmzmba = prmfir
041300000710     C                   endif
041400990920     C                   eval      cmznrr = *zeros
041500990920     C                   move      §ctrok        cmznrr
041600021003     C                   eval      cmzlba = vlrfl1
041700990920     C                   call(e)   'TIS711C'
041800990920     C                   parm                    dscmz
041900990921     C                   parm      *blanks       esito
042000990920     C                   if        %error
042100990920     C                             or cmzerr = '1'
042200990921     C                             or esito  = '1'
042300000710     C                   eval      wrkesito = '3'
042400990920     C                   endif
042500990920     C*
042600000613     C                   ENDSR
042700990910
042800010601
042900010601
043000010601
043100010601
043200010601      /TITLE Invio dei dati al punto operativo.
043300010601     C     opeini        BEGSR
043400010601     C*
043500010601     C* Inizializzo flag e contatori operativi
043600010601     C                   movel     '0'           flgGiro           1
043700010601     C                   movel     '0'           flgMulti          1
043800010601     C                   movel     '1'           flgStato          1
043900010615     C                   movel     '0'           flgOk             1
044000010601     C                   z-add     *zeros        cntNonEl         10 0
044100010601     C                   z-add     *zeros        depfil            3 0
044200010601     C                   z-add     *zeros        invfil            3 0
044300030407     C*
044400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
044500030407     C                   if        %subst(vlrppt:1:1) = 'S'
044600030407     C                   seton                                        50
044700030407     C                   else
044800030407     C                   setoff                                       50
044900030407     C                   endif
045000010601     C*
045100010601     C                   ENDSR
045200010601     C***
045300010601
045400010601
045500010601
045600010330
045700010330
045800000613     C     *inzsr        BEGSR
045900990910     C*
046000990910     C     *entry        plist
046100990920     C                   parm                    tivlrds
046200990921     C                   parm      wrkesito      esito
046300000724     C                   parm                    prmlit
046400000710     C                   parm                    prmfir
046500010330     C*
046600010330     C* CALCOLA LA DATA CORRENTE
046700100301     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
046800100301     C                   eval      datcor = %dec(%date() : *ISO)
046900000613     C*
047000000613     C                   ENDSR
047100000613     C***
