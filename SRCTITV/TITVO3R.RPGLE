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
007900990915     C*
008000000616     C*
008100010601     C                   ENDSR
008200010601     C***
008300000613
008400010330
008500010601
008600010601
008700010601
008800010330     C*--------------------------------------------------------
008900020916     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
009000010330     C*--------------------------------------------------------
009100010612     C     RWVAT         BEGSR
009200010330     C*
009300010330     C                   if        not %open(tivin00r)
009400010330     C                   open      tivin00r
009500010330     C                   endif
009600020916     C                   if        not %open(fivatwwr)
009700020916     C                   open      fivatwwr
009800010330     C                   endif
009900010330     C*
010000010604     C                   clear                   §CTROK
010100010604     C                   clear                   §CTRMO
010200010604     C                   clear                   §CTRNO
010300010330     C*
010400010330     C                   DO        *HIVAL
010500010330     C*
010600010330     C                   READ      tivin00r                               70
010700010618     C*
010800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010900010618     C                   if        vinflg = '1'
011000010618     C                   eval      flgOk = '1'
011100010618     C                   endif
011200010618     C*
011300010330     C                   if        vindta > *blanks
011400010330     C                   add       1             rrnum
011500010330     C*
011600010601     C                   if        *in70 = *off and
011700010330     C                             (vinflg = *blanks
011800010330     C                              or vinflg = '0'
011900010330     C                              or vinflg = '2')
012000010711     C*
012100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012200010711     C                   if        vinflg = *blanks or vinflg = '0'
012300010711     C                   clear                   vinmsg
012400010711     C                   endif
012500010711     C*
012600010613     C                   eval      fnvataar = vindta
012700010601     C*
012800010330     C                   exsr      inzvar
012900010330     C                   exsr      defcam
013000010612     C                   exsr      impvat
013100010601     C*
013200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013300010612     C                   eval      depfil = VATLNP
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
014900020916     C  N31              WRITE     FIVAT000
015000010604     C*
015100010604     C                   if        *in31 = *off and
015200010604     C                             *in32 = *off
015300010604     C                   eval      vinflg = '1'
015400010604     C                   else
015500010604     C                   eval      vinflg = '2'
015600010604     C                   endif
015700010604     C                   endif
015800010601     C*
015900010604     C                   endif
016000010604     C*
016100010330     C                   else
016200010330     C                   eval      vinflg = '1'
016300010330     C                   endif
016400010601     C*
016500010601     C  N70              update    tivin000
016600010330     C*
016700010330     C  N70              ENDdo
016800010601     C
016900010601     C*
017000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017100010601     C                   if        cntNonEl = *zeros or
017200010601     C                             flgMulti = '0'
017300010330     C* Se non ci sono record con errori ...
017400010601     C                   if        §ctrno = 0 and
017500010604     C                             §ctrmo = 0 and
017600010601     C                             flgStato <> '2'
017700010330     C* ... restituisco esito OK.
017800010330     C                   eval      wrkesito = '0'
017900010330     C                   else
018000010330     C                   if        §ctrok > 0
018100010330     C                   eval      wrkesito = '1'
018200010330     C                   else
018300010615     C                   if        flgOk = '0'
018400010615     C                   eval      wrkesito = '2'
018500010615     C                   else
018600010615     C                   eval      wrkesito = '6'
018700010615     C                   endif
018800010330     C                   endif
018900010330     C                   endif
019000010601     C                   else
019100010601     C                   eval      wrkesito = '9'
019200010601     C                   endif
019300010330     C*
019400010330     C                   if        %open(tivin00r)
019500010330     C                   close     tivin00r
019600010330     C                   endif
019700020916     C                   if        %open(fivatwwr)
019800020916     C                   close     fivatwwr
019900010330     C                   endif
020000010601     C*
020100010601     C                   if        vlrpoi <> 999
020200010601     C                   eval      invfil = vlrpoi
020300010601     C                   endif
020400010330     C*
020500010330     C                   if        §ctrok > 0
020600010601     C                             and invfil > *zeros
020700010330     C                   exsr      invio
020800010330     C                   endif
020900010601     C*
021000010612     C                   if        flgGiro = '1'
021100010601     C                   exsr      endela
021200010612     C                   endif
021300010330     C*
021400010330     C                   ENDSR
021500010330     C***
021600010330
021700010601
021800010601
021900010601
022000010601
022100010330     C*----------------------------------------------------*
022200010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022300010330     C*----------------------------------------------------*
022400010330     C     INZVAR        BEGSR
022500010330     C*
022600010330     C*
022700010330     C                   ENDSR
022800010330     C*----------------------------------------------------*
022900010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023000010330     C*----------------------------------------------------*
023100010330     C     DEFCAM        BEGSR
023200010330     C*
023300020916     C                   CLEAR                   FIVAT000
023400010330     C*
023500010330     C                   ENDSR
023600010330     C*----------------------------------------------------*
023700020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
023800010330     C*----------------------------------------------------*
023900010612     C     IMPVAT        BEGSR
024000010330     C*
024100010604     C                   SETOFF                                       3132
024200010612     C*
024300010612     C                   SETOFF                                       303132
024400010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
024500010612     C                   TESTN                   a_vatccm             30
024600010612     C  N30              TESTN                   a_vatccm               30
024700010612     C  N30              TESTN                   a_vatccm                 30
024800010612     C                   MOVE      a_vatccm      W001A             1
024900010612     C     *IN30         IFEQ      *OFF
025000010612     C     W001A         ORLT      '0'
025100010612     C                   SETON                                        31
025200010612     C                   Z-ADD     *zeros        VATCCM
025300010612     C                   EVAL      vinmsg = 'VATCCM'
025400010612     C                   ELSE
025500010612     C                   MOVE      a_vatccm      VATCCM
025600010612     C     VATCCM        IFLE      *ZEROS
025700010612     C                   SETON                                        31
025800010612     C                   Z-ADD     *zeros        VATCCM
025900010612     C                   EVAL      vinmsg = 'VATCCM'
026000010612     C                   ENDIF
026100010612     C                   ENDIF
026200010612     C*
026300010612     C                   TESTN                   a_vatlnp             30
026400010612     C  N30              TESTN                   a_vatlnp               30
026500010612     C  N30              TESTN                   a_vatlnp                 30
026600010612     C                   MOVE      a_vatlnp      W001A             1
026700010612     C     *IN30         IFEQ      *OFF
026800010612     C     W001A         ORLT      '0'
026900010612     C                   SETON                                        31
027000010612     C                   Z-ADD     *zeros        VATLNP
027100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
027200021010     C                             + ' ' + 'VATLNP'
027300010612     C                   ELSE
027400010612     C                   MOVE      a_vatlnp      VATLNP
027500010612     C     VATLNP        IFLE      *ZEROS
027600010612     C                   SETON                                        31
027700010612     C                   Z-ADD     *zeros        VATLNP
027800010612     C                   EVAL      vinmsg = %trimr(vinmsg)
027900021010     C                             + ' ' + 'VATLNP'
028000010612     C                   ENDIF
028100010612     C                   ENDIF
028200010612     C*
028300010612     C                   TESTN                   a_vataas             30
028400010612     C  N30              TESTN                   a_vataas               30
028500010612     C  N30              TESTN                   a_vataas                 30
028600010612     C                   MOVE      a_vataas      W001A             1
028700010612     C     *IN30         IFEQ      *OFF
028800010612     C     W001A         ORLT      '0'
028900010612     C                   SETON                                        31
029000010612     C                   MOVEL(P)  datcor        VATAAS
029100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
029200010612     C                             + ' ' + 'VATAAS'
029300010612     C                   ELSE
029400010612     C                   MOVE      a_vataas      VATAAS
029500010612     C                   ENDIF
029600010612     C*
029700010612     C                   TESTN                   a_vatnrs             30
029800010612     C  N30              TESTN                   a_vatnrs               30
029900010612     C  N30              TESTN                   a_vatnrs                 30
030000010612     C                   MOVE      a_vatnrs      W001A             1
030100010612     C     *IN30         IFEQ      *OFF
030200010612     C     W001A         ORLT      '0'
030300010612     C     W001A         ANDNE     *BLANK
030400010612     C                   SETON                                        31
030500010612     C                   Z-ADD     *zeros        VATNRS
030600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
030700010612     C                             + ' ' + 'VATNRS'
030800010612     C                   ELSE
030900010612     C                   MOVE      a_vatnrs      VATNRS
031000010612     C                   ENDIF
031100010612     C*
031200010612     C                   TESTN                   a_vatnsp             30
031300010612     C  N30              TESTN                   a_vatnsp               30
031400010612     C  N30              TESTN                   a_vatnsp                 30
031500010612     C                   MOVE      a_vatnsp      W001A             1
031600010612     C     *IN30         IFEQ      *OFF
031700010612     C     W001A         ORLT      '0'
031800010612     C                   SETON                                        31
031900010612     C                   Z-ADD     *zeros        VATNSP
032000010612     C                   EVAL      vinmsg = %trimr(vinmsg)
032100010612     C                             + ' ' + 'VATNSP'
032200010612     C                   ELSE
032300010612     C                   MOVE      a_vatnsp      VATNSP
032400010612     C                   ENDIF
032500010612     C*
032600010612     C*
032700010612     C* ALFABETICI
032800010612     C                   MOVE      a_vatatb      VATATB
032900010612     C                   MOVE      a_vattrc      VATTRC
033000020725     C                   EVAL      VATNOT = %trim(a_vatnot)
033100070129     C*
033200070129     C* Solo x tipo reecord E x i "chi sono"
033300070129     C                   IF        VATTRC = 'E'
033400050913     C                   IF        %subst(VATNOT:1:1) = '0'
033500050913     C                   EVAL      VATNOT=%subst(VATNOT:2)
033600070129     C                   ENDIF
033700050913     C                   ENDIF
033800010330     C*
033900010330     C                   ENDSR
034000010612     C*----------------------------------------------------*
034100990920
034200010601
034300010601
034400010601
034500010601
034600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
034700010601     C     repfil        BEGSR
034800010601     C*
034900010601     C                   if        invfil = *zeros and
035000010601     C                             depfil > *zeros and
035100010613     C                             (vinflg = *blanks or
035200010613     C                              vinflg = *zeros)
035300010601     C
035400010601     C                   eval      invfil = depfil
035500010601     C                   endif
035600010601     C*
035700010601     C                   if        depfil <> invfil and
035800010601     C                             invfil > *zeros
035900010601     C                   eval      flgMulti = '1'
036000010601     C                   if        vinflg = *blanks
036100010601     C                   add       1             cntNonEl
036200010601     C                   endif
036300010601     C                   endif
036400010601     C*
036500010601     C                   if        vinflg = '2'
036600010601     C                   eval      flgStato = '2'
036700010601     C                   endif
036800010601     C*
036900010601     C                   ENDSR
037000010601     C***
037100010601
037200010601
037300010601
037400010330
037500010330
037600010330
037700990920      /TITLE Invio dei dati al punto operativo.
037800000613     C     invio         BEGSR
037900990920     C*
038000990920     C                   reset                   dscmz
038100010601     C                   move      invfil        cmzdst
038200990920     C                   eval      cmzfld = vlrfou
038300990920     C                   eval      cmzmbd = vlrhdl
038400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
038500000710     C                   if        prmfir = *blanks
038600020916     C                   eval      cmzfla = 'FIVAT00F'
038700020916     C                   eval      cmzmba = 'FIVAT00F'
038800000710     C                   else
038900000710     C                   eval      cmzfla = prmfir
039000000710     C                   eval      cmzmba = prmfir
039100000710     C                   endif
039200990920     C                   eval      cmznrr = *zeros
039300990920     C                   move      §ctrok        cmznrr
039400021003     C                   eval      cmzlba = vlrfl1
039500990920     C                   call(e)   'TIS711C'
039600990920     C                   parm                    dscmz
039700990921     C                   parm      *blanks       esito
039800990920     C                   if        %error
039900990920     C                             or cmzerr = '1'
040000990921     C                             or esito  = '1'
040100000710     C                   eval      wrkesito = '3'
040200990920     C                   endif
040300990920     C*
040400000613     C                   ENDSR
040500990910
040600010601
040700010601
040800010601
040900010601
041000010601      /TITLE Invio dei dati al punto operativo.
041100010601     C     opeini        BEGSR
041200010601     C*
041300010601     C* Inizializzo flag e contatori operativi
041400010601     C                   movel     '0'           flgGiro           1
041500010601     C                   movel     '0'           flgMulti          1
041600010601     C                   movel     '1'           flgStato          1
041700010615     C                   movel     '0'           flgOk             1
041800010601     C                   z-add     *zeros        cntNonEl         10 0
041900010601     C                   z-add     *zeros        depfil            3 0
042000010601     C                   z-add     *zeros        invfil            3 0
042100030407     C*
042200030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
042300030407     C                   if        %subst(vlrppt:1:1) = 'S'
042400030407     C                   seton                                        50
042500030407     C                   else
042600030407     C                   setoff                                       50
042700030407     C                   endif
042800010601     C*
042900010601     C                   ENDSR
043000010601     C***
043100010601
043200010601
043300010601
043400010330
043500010330
043600000613     C     *inzsr        BEGSR
043700990910     C*
043800990910     C     *entry        plist
043900990920     C                   parm                    tivlrds
044000990921     C                   parm      wrkesito      esito
044100000724     C                   parm                    prmlit
044200000710     C                   parm                    prmfir
044300010330     C*
044400010330     C* CALCOLA LA DATA CORRENTE
044500010330     C                   time                    wn14             14 0
044600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
044700010330     C                   z-add     wn8           g08dat
044800010330     C                   z-add     *zeros        g08inv
044900010330     C                   movel     '0'           g08err
045000010330     C                   call      'XSRDA8'
045100010330     C                   parm                    wlbda8
045200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
045300000613     C*
045400000613     C                   ENDSR
045500000613     C***
