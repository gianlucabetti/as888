000100110126      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500110126     FEDIVATwr  O    E             DISK    usropn
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
001900110126     D wNomeFile       s             30    INZ(*blanks)
002000010330     D*------------------
002100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002200010330     D*------------------
002300010330     D WLBDA8          DS                  INZ
002400010330     D  G08DAT                 1      8  0
002500010330     D  G08INV                 9     16  0
002600010330     D  G08ERR                17     17
002700010330     D  G08TGI                18     22  0
002800010612     D*
002900010612     D fnvataar        ds                  inz
003000010612     D  a_vatatb               1      1
003100010612     D  a_vatccm               3      9
003200010612     D  a_vatlnp              11     13
003300010612     D  a_vataas              15     18
003400010612     D  a_vatnrs              20     21
003500010612     D  a_vatnsp              23     29
003600010612     D  a_vattrc              30     30
003700010612     D  a_vatnot              31     65
003800010330
003900010330
004000010330
004100000913     C                   reset                   rrnum
004200990921     C                   reset                   esito
004300990921     C                   reset                   wrkesito
004400010601     C*
004500010601     C                   exsr      opeini
004600010612     C                   exsr      rwvat
004700010601     C*
004800010601     C                   seton                                        lr
004900010601
005000010601
005100010601
005200010601
005300010601     C*--------------------------------------------------------
005400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005500010601     C*--------------------------------------------------------
005600010601     C     PREELA        BEGSR
005700010601     C*
005800010601     C* SE OCCORRE SPEDIRE IN FILIALE
005900010601     C                   if        invfil <> *zeros and
006000010601     C                             flgGiro = '0'
006100010601     C*
006200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006300010601     C                   eval      flgGiro = '1'
006400010601     C*
006500010601     C                   endif
006600010601     C*
006700010601     C                   ENDSR
006800010601     C***
006900010601
007000010601
007100010601
007200010601
007300010601
007400010601
007500010601
007600010601     C*--------------------------------------------------------
007700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007800010601     C*--------------------------------------------------------
007900010601     C     ENDELA        BEGSR
008000000616     C*
008100010601     C                   ENDSR
008200010601     C***
008300000613
008400010330
008500010601
008600010601
008700010601
008800010330     C*--------------------------------------------------------
008900110126     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
009000010330     C*--------------------------------------------------------
009100010612     C     RWVAT         BEGSR
009200010330     C*
009300010330     C                   if        not %open(tivin00r)
009400010330     C                   open      tivin00r
009500010330     C                   endif
009600110126     C                   if        not %open(edivatwr)
009700110126     C                   open      edivatwr
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
013100070115     C*
013200070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013300070115     C                   eval      depfil = VATLNP
013400070115     C*
013500070115     C* Effettuo forzatura ESPRINET x invio a terminal d competenza rispetto al CCM indicato dal
013600070115     C* cliente
013700070507     C                   if        VATCCM = 0893943 OR
013800070507     C                             VATCCM = 0894169
013900070115     C                   eval      depfil = 050
014000070115     C                   eval      invfil = 050
014100070115     C                   eval      vatfgs = 050
014200070115     C                   endif
014300070507     C                   if        VATCCM = 0893942 OR
014400070507     C                             VATCCM = 0894168
014500070115     C                   eval      depfil = 005
014600070115     C                   eval      invfil = 005
014700070115     C                   eval      vatfgs = 005
014800070115     C                   endif
014900010601     C*
015000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
015100010601     C                   exsr      repfil
015200021010     C                   if        depfil = invfil
015300021014     C                   if        vlrpoi = 999
015400021010     C                   MOVE(P)   invfil        VATFGS
015500021014     C                   else
015600021014     C                   MOVE(P)   vlrpoi        VATFGS
015700021014     C                   endif
015800010601     C*
015900010601     C                   exsr      PREELA
016000010601     C*
016100010604     C* Ebbene...
016200010604     C*
016300010604     C  N31              ADD       1             §CTROK            7 0
016400010604     C   32              ADD       1             §CTRMO            7 0
016500010604     C   31              ADD       1             §CTRNO            7 0
016600110126     C*
016700110126     C* VALORIZZO CAMPI RELATIVI AL "CMR"
016800110126     C                   IF        not *in31
016900110126     C                   EVAL      VATCMR = %subst(wNomeFile:8:8)
017000110126     C                   EVAL      VATCNT = 1
017100110126     C                   ENDIF
017200110126     C*
017300110126     C  N31              WRITE     EDIVAT00
017400010604     C*
017500010604     C                   if        *in31 = *off and
017600010604     C                             *in32 = *off
017700010604     C                   eval      vinflg = '1'
017800010604     C                   else
017900010604     C                   eval      vinflg = '2'
018000010604     C                   endif
018100010604     C                   endif
018200010601     C*
018300010604     C                   endif
018400010604     C*
018500010330     C                   else
018600010330     C                   eval      vinflg = '1'
018700010330     C                   endif
018800010601     C*
018900010601     C  N70              update    tivin000
019000010330     C*
019100010330     C  N70              ENDdo
019200010601     C
019300010601     C*
019400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019500010601     C                   if        cntNonEl = *zeros or
019600010601     C                             flgMulti = '0'
019700010330     C* Se non ci sono record con errori ...
019800010601     C                   if        §ctrno = 0 and
019900010604     C                             §ctrmo = 0 and
020000010601     C                             flgStato <> '2'
020100010330     C* ... restituisco esito OK.
020200010330     C                   eval      wrkesito = '0'
020300010330     C                   else
020400010330     C                   if        §ctrok > 0
020500010330     C                   eval      wrkesito = '1'
020600010330     C                   else
020700010615     C                   if        flgOk = '0'
020800010615     C                   eval      wrkesito = '2'
020900010615     C                   else
021000010615     C                   eval      wrkesito = '6'
021100010615     C                   endif
021200010330     C                   endif
021300010330     C                   endif
021400010601     C                   else
021500010601     C                   eval      wrkesito = '9'
021600010601     C                   endif
021700010330     C*
021800010330     C                   if        %open(tivin00r)
021900010330     C                   close     tivin00r
022000010330     C                   endif
022100110126     C                   if        %open(edivatwr)
022200110126     C                   close     edivatwr
022300010330     C                   endif
022400010601     C*
022500010601     C                   if        vlrpoi <> 999
022600010601     C                   eval      invfil = vlrpoi
022700010601     C                   endif
022800010330     C*
022900010330     C                   if        §ctrok > 0
023000010601     C                             and invfil > *zeros
023100010330     C                   exsr      invio
023200010330     C                   endif
023300010601     C*
023400010612     C                   if        flgGiro = '1'
023500010601     C                   exsr      endela
023600010612     C                   endif
023700010330     C*
023800010330     C                   ENDSR
023900010330     C***
024000010330
024100010601
024200010601
024300010601
024400010601
024500010330     C*----------------------------------------------------*
024600010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024700010330     C*----------------------------------------------------*
024800010330     C     INZVAR        BEGSR
024900010330     C*
025000010330     C*
025100010330     C                   ENDSR
025200010330     C*----------------------------------------------------*
025300010330     C*  IMPOSTAZIONE CAMPI COSTANTI
025400010330     C*----------------------------------------------------*
025500010330     C     DEFCAM        BEGSR
025600010330     C*
025700110126     C                   CLEAR                   EDIVAT00
025800010330     C*
025900010330     C                   ENDSR
026000010330     C*----------------------------------------------------*
026100020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
026200010330     C*----------------------------------------------------*
026300010612     C     IMPVAT        BEGSR
026400010330     C*
026500010604     C                   SETOFF                                       3132
026600010612     C*
026700010612     C                   SETOFF                                       303132
026800010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
026900010612     C                   TESTN                   a_vatccm             30
027000010612     C  N30              TESTN                   a_vatccm               30
027100010612     C  N30              TESTN                   a_vatccm                 30
027200010612     C                   MOVE      a_vatccm      W001A             1
027300010612     C     *IN30         IFEQ      *OFF
027400010612     C     W001A         ORLT      '0'
027500010612     C                   SETON                                        31
027600010612     C                   Z-ADD     *zeros        VATCCM
027700010612     C                   EVAL      vinmsg = 'VATCCM'
027800010612     C                   ELSE
027900010612     C                   MOVE      a_vatccm      VATCCM
028000010612     C     VATCCM        IFLE      *ZEROS
028100010612     C                   SETON                                        31
028200010612     C                   Z-ADD     *zeros        VATCCM
028300010612     C                   EVAL      vinmsg = 'VATCCM'
028400010612     C                   ENDIF
028500010612     C                   ENDIF
028600010612     C*
028700010612     C                   TESTN                   a_vatlnp             30
028800010612     C  N30              TESTN                   a_vatlnp               30
028900010612     C  N30              TESTN                   a_vatlnp                 30
029000010612     C                   MOVE      a_vatlnp      W001A             1
029100010612     C     *IN30         IFEQ      *OFF
029200010612     C     W001A         ORLT      '0'
029300010612     C                   SETON                                        31
029400010612     C                   Z-ADD     *zeros        VATLNP
029500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
029600021010     C                             + ' ' + 'VATLNP'
029700010612     C                   ELSE
029800010612     C                   MOVE      a_vatlnp      VATLNP
029900010612     C     VATLNP        IFLE      *ZEROS
030000010612     C                   SETON                                        31
030100010612     C                   Z-ADD     *zeros        VATLNP
030200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
030300021010     C                             + ' ' + 'VATLNP'
030400010612     C                   ENDIF
030500010612     C                   ENDIF
030600010612     C*
030700010612     C                   TESTN                   a_vataas             30
030800010612     C  N30              TESTN                   a_vataas               30
030900010612     C  N30              TESTN                   a_vataas                 30
031000010612     C                   MOVE      a_vataas      W001A             1
031100010612     C     *IN30         IFEQ      *OFF
031200010612     C     W001A         ORLT      '0'
031300010612     C                   SETON                                        31
031400010612     C                   MOVEL(P)  datcor        VATAAS
031500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
031600010612     C                             + ' ' + 'VATAAS'
031700010612     C                   ELSE
031800010612     C                   MOVE      a_vataas      VATAAS
031900010612     C                   ENDIF
032000010612     C*
032100010612     C                   TESTN                   a_vatnrs             30
032200010612     C  N30              TESTN                   a_vatnrs               30
032300010612     C  N30              TESTN                   a_vatnrs                 30
032400010612     C                   MOVE      a_vatnrs      W001A             1
032500010612     C     *IN30         IFEQ      *OFF
032600010612     C     W001A         ORLT      '0'
032700010612     C     W001A         ANDNE     *BLANK
032800010612     C                   SETON                                        31
032900010612     C                   Z-ADD     *zeros        VATNRS
033000010612     C                   EVAL      vinmsg = %trimr(vinmsg)
033100010612     C                             + ' ' + 'VATNRS'
033200010612     C                   ELSE
033300010612     C                   MOVE      a_vatnrs      VATNRS
033400010612     C                   ENDIF
033500010612     C*
033600010612     C                   TESTN                   a_vatnsp             30
033700010612     C  N30              TESTN                   a_vatnsp               30
033800010612     C  N30              TESTN                   a_vatnsp                 30
033900010612     C                   MOVE      a_vatnsp      W001A             1
034000010612     C     *IN30         IFEQ      *OFF
034100010612     C     W001A         ORLT      '0'
034200010612     C                   SETON                                        31
034300010612     C                   Z-ADD     *zeros        VATNSP
034400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
034500010612     C                             + ' ' + 'VATNSP'
034600010612     C                   ELSE
034700010612     C                   MOVE      a_vatnsp      VATNSP
034800010612     C                   ENDIF
034900010612     C*
035000010612     C*
035100010612     C* ALFABETICI
035200010612     C                   MOVE      a_vatatb      VATATB
035300010612     C                   MOVE      a_vattrc      VATTRC
035400020725     C                   EVAL      VATNOT = %trim(a_vatnot)
035500010330     C*
035600010330     C                   ENDSR
035700010612     C*----------------------------------------------------*
035800990920
035900010601
036000010601
036100010601
036200010601
036300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
036400010601     C     repfil        BEGSR
036500010601     C*
036600010601     C                   if        invfil = *zeros and
036700010601     C                             depfil > *zeros and
036800010613     C                             (vinflg = *blanks or
036900010613     C                              vinflg = *zeros)
037000010601     C
037100010601     C                   eval      invfil = depfil
037200010601     C                   endif
037300010601     C*
037400010601     C                   if        depfil <> invfil and
037500010601     C                             invfil > *zeros
037600010601     C                   eval      flgMulti = '1'
037700010601     C                   if        vinflg = *blanks
037800010601     C                   add       1             cntNonEl
037900010601     C                   endif
038000010601     C                   endif
038100010601     C*
038200010601     C                   if        vinflg = '2'
038300010601     C                   eval      flgStato = '2'
038400010601     C                   endif
038500010601     C*
038600010601     C                   ENDSR
038700010601     C***
038800010601
038900010601
039000010601
039100010330
039200010330
039300010330
039400990920      /TITLE Invio dei dati al punto operativo.
039500000613     C     invio         BEGSR
039600990920     C*
039700990920     C                   reset                   dscmz
039800010601     C                   move      invfil        cmzdst
039900990920     C                   eval      cmzfld = vlrfou
040000990920     C                   eval      cmzmbd = vlrhdl
040100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
040200000710     C                   if        prmfir = *blanks
040300110126     C                   eval      cmzfla = 'EDIVAT0F'
040400110126     C                   eval      cmzmba = 'EDIVAT0F'
040500000710     C                   else
040600000710     C                   eval      cmzfla = prmfir
040700000710     C                   eval      cmzmba = prmfir
040800000710     C                   endif
040900990920     C                   eval      cmznrr = *zeros
041000990920     C                   move      §ctrok        cmznrr
041100021003     C                   eval      cmzlba = vlrfl1
041200990920     C                   call(e)   'TIS711C'
041300990920     C                   parm                    dscmz
041400990921     C                   parm      *blanks       esito
041500990920     C                   if        %error
041600990920     C                             or cmzerr = '1'
041700990921     C                             or esito  = '1'
041800000710     C                   eval      wrkesito = '3'
041900990920     C                   endif
042000990920     C*
042100000613     C                   ENDSR
042200990910
042300010601
042400010601
042500010601
042600010601
042700010601      /TITLE Invio dei dati al punto operativo.
042800010601     C     opeini        BEGSR
042900010601     C*
043000010601     C* Inizializzo flag e contatori operativi
043100010601     C                   movel     '0'           flgGiro           1
043200010601     C                   movel     '0'           flgMulti          1
043300010601     C                   movel     '1'           flgStato          1
043400010615     C                   movel     '0'           flgOk             1
043500010601     C                   z-add     *zeros        cntNonEl         10 0
043600010601     C                   z-add     *zeros        depfil            3 0
043700010601     C                   z-add     *zeros        invfil            3 0
043800030407     C*
043900030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
044000030407     C                   if        %subst(vlrppt:1:1) = 'S'
044100030407     C                   seton                                        50
044200030407     C                   else
044300030407     C                   setoff                                       50
044400030407     C                   endif
044500010601     C*
044600010601     C                   ENDSR
044700010601     C***
044800010601
044900010601
045000010601
045100010330
045200010330
045300000613     C     *inzsr        BEGSR
045400990910     C*
045500990910     C     *entry        plist
045600990920     C                   parm                    tivlrds
045700990921     C                   parm      wrkesito      esito
045800000724     C                   parm                    prmlit
045900000710     C                   parm                    prmfir
046000110126     C*
046100110126     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
046200110126     C                   move(p)   vlrMSG        wNomeFile
046300010330     C*
046400010330     C* CALCOLA LA DATA CORRENTE
046500010330     C                   time                    wn14             14 0
046600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
046700010330     C                   z-add     wn8           g08dat
046800010330     C                   z-add     *zeros        g08inv
046900010330     C                   movel     '0'           g08err
047000010330     C                   call      'XSRDA8'
047100010330     C                   parm                    wlbda8
047200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
047300000613     C*
047400000613     C                   ENDSR
047500000613     C***
