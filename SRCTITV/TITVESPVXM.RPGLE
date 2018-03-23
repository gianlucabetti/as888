000100110126      /TITLE Upload via Internet: traduzione in EDIVAXWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500110126     FEDIVAXwr  O    E             DISK    usropn
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
002900040524     D fnvaxaar        ds                  inz
003000040524     D  a_vaxatb               1      1
003100040524     D  a_vaxccm               3      9
003200040524     D  a_vaxlnp              11     13
003300040524     D  a_vaxaas              15     18
003400040524     D  a_vaxnrs              20     21
003500040524     D  a_vaxnsp              23     29
003600040524     D  a_vaxtmd              30     33
003700040524     D  a_vaxprr              35     38
003800040524     D  a_vaxtrk              39     40
003900040524     D  a_vaxdtt              41    540
004000010330
004100010330
004200010330
004300000913     C                   reset                   rrnum
004400990921     C                   reset                   esito
004500990921     C                   reset                   wrkesito
004600010601     C*
004700010601     C                   exsr      opeini
004800040524     C                   exsr      rwvax
004900010601     C*
005000010601     C                   seton                                        lr
005100010601
005200010601
005300010601
005400010601
005500010601     C*--------------------------------------------------------
005600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005700010601     C*--------------------------------------------------------
005800010601     C     PREELA        BEGSR
005900010601     C*
006000010601     C* SE OCCORRE SPEDIRE IN FILIALE
006100010601     C                   if        invfil <> *zeros and
006200010601     C                             flgGiro = '0'
006300010601     C*
006400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006500010601     C                   eval      flgGiro = '1'
006600010601     C*
006700010601     C                   endif
006800010601     C*
006900010601     C                   ENDSR
007000010601     C***
007100010601
007200010601
007300010601
007400010601
007500010601
007600010601
007700010601
007800010601     C*--------------------------------------------------------
007900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008000010601     C*--------------------------------------------------------
008100010601     C     ENDELA        BEGSR
008200000616     C*
008300010601     C                   ENDSR
008400010601     C***
008500000613
008600010330
008700010601
008800010601
008900010601
009000010330     C*--------------------------------------------------------
009100110126     C* RWVAX   LEGGE TIVIN00R E SCRIVE EDIVAXWF              *
009200010330     C*--------------------------------------------------------
009300040524     C     RWVAX         BEGSR
009400010330     C*
009500010330     C                   if        not %open(tivin00r)
009600010330     C                   open      tivin00r
009700010330     C                   endif
009800110126     C                   if        not %open(edivaxwr)
009900110126     C                   open      edivaxwr
010000010330     C                   endif
010100010330     C*
010200010604     C                   clear                   §CTROK
010300010604     C                   clear                   §CTRMO
010400010604     C                   clear                   §CTRNO
010500010330     C*
010600010330     C                   DO        *HIVAL
010700010330     C*
010800010330     C                   READ      tivin00r                               70
010900010618     C*
011000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011100010618     C                   if        vinflg = '1'
011200010618     C                   eval      flgOk = '1'
011300010618     C                   endif
011400010618     C*
011500010330     C                   if        vindta > *blanks
011600010330     C                   add       1             rrnum
011700010330     C*
011800010601     C                   if        *in70 = *off and
011900010330     C                             (vinflg = *blanks
012000010330     C                              or vinflg = '0'
012100010330     C                              or vinflg = '2')
012200010711     C*
012300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012400010711     C                   if        vinflg = *blanks or vinflg = '0'
012500010711     C                   clear                   vinmsg
012600010711     C                   endif
012700010711     C*
012800040524     C                   eval      fnvaxaar = vindta
012900010601     C*
013000010330     C                   exsr      inzvar
013100010330     C                   exsr      defcam
013200040524     C                   exsr      impvax
013300070115     C*
013400070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013500070115     C                   eval      depfil = VAXLNP
013600070115     C*
013700070115     C* Effettuo forzatura ESPRINET x invio a terminal d competenza rispetto al CCM indicato dal
013800070115     C* cliente
013900070507     C                   if        VAXCCM = 0893943 OR
014000070507     C                             VAXCCM = 0894169
014100070115     C                   eval      depfil = 050
014200070115     C                   eval      invfil = 050
014300070115     C                   eval      vaxfgs = 050
014400070115     C                   endif
014500070507     C                   if        VAXCCM = 0893942 OR
014600070507     C                             VAXCCM = 0894168
014700070115     C                   eval      depfil = 005
014800070115     C                   eval      invfil = 005
014900070115     C                   eval      vaxfgs = 005
015000070115     C                   endif
015100010601     C*
015200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
015300010601     C                   exsr      repfil
015400021010     C                   if        depfil = invfil
015500021014     C                   if        vlrpoi = 999
015600040524     C                   MOVE(P)   invfil        VAXFGS
015700021014     C                   else
015800040524     C                   MOVE(P)   vlrpoi        VAXFGS
015900021014     C                   endif
016000010601     C*
016100010601     C                   exsr      PREELA
016200010601     C*
016300010604     C* Ebbene...
016400010604     C*
016500010604     C  N31              ADD       1             §CTROK            7 0
016600010604     C   32              ADD       1             §CTRMO            7 0
016700010604     C   31              ADD       1             §CTRNO            7 0
016800110126     C*
016900110126     C* VALORIZZO CAMPI RELATIVI AL "CMR"
017000110126     C                   IF        not *in31
017100110126     C                   EVAL      VAXCMR = %subst(wNomeFile:8:8)
017200110126     C                   EVAL      VAXCNT = 1
017300110126     C                   ENDIF
017400110207     C*
017500110207     C* FORZATURA X NON-SCRITTURA PACKING LIST X TRAFFICO 0896002
017600120515     C***                IF        VAXCCM = 0896002
017700120515     C***                EVAL      *IN31 = *on
017800120515     C***                ENDIF
017900110126     C*
018000110126     C  N31              WRITE     EDIVAX00
018100010604     C*
018200010604     C                   if        *in31 = *off and
018300010604     C                             *in32 = *off
018400010604     C                   eval      vinflg = '1'
018500010604     C                   else
018600010604     C                   eval      vinflg = '2'
018700010604     C                   endif
018800010604     C                   endif
018900010601     C*
019000010604     C                   endif
019100010604     C*
019200010330     C                   else
019300010330     C                   eval      vinflg = '1'
019400010330     C                   endif
019500010601     C*
019600010601     C  N70              update    tivin000
019700010330     C*
019800010330     C  N70              ENDdo
019900010601     C
020000010601     C*
020100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
020200010601     C                   if        cntNonEl = *zeros or
020300010601     C                             flgMulti = '0'
020400010330     C* Se non ci sono record con errori ...
020500010601     C                   if        §ctrno = 0 and
020600010604     C                             §ctrmo = 0 and
020700010601     C                             flgStato <> '2'
020800010330     C* ... restituisco esito OK.
020900010330     C                   eval      wrkesito = '0'
021000010330     C                   else
021100010330     C                   if        §ctrok > 0
021200010330     C                   eval      wrkesito = '1'
021300010330     C                   else
021400010615     C                   if        flgOk = '0'
021500010615     C                   eval      wrkesito = '2'
021600010615     C                   else
021700010615     C                   eval      wrkesito = '6'
021800010615     C                   endif
021900010330     C                   endif
022000010330     C                   endif
022100010601     C                   else
022200010601     C                   eval      wrkesito = '9'
022300010601     C                   endif
022400010330     C*
022500010330     C                   if        %open(tivin00r)
022600010330     C                   close     tivin00r
022700010330     C                   endif
022800110126     C                   if        %open(edivaxwr)
022900110126     C                   close     edivaxwr
023000010330     C                   endif
023100010601     C*
023200010601     C                   if        vlrpoi <> 999
023300010601     C                   eval      invfil = vlrpoi
023400010601     C                   endif
023500010330     C*
023600010330     C                   if        §ctrok > 0
023700010601     C                             and invfil > *zeros
023800010330     C                   exsr      invio
023900010330     C                   endif
024000010601     C*
024100010612     C                   if        flgGiro = '1'
024200010601     C                   exsr      endela
024300010612     C                   endif
024400010330     C*
024500010330     C                   ENDSR
024600010330     C***
024700010330
024800010601
024900010601
025000010601
025100010601
025200010330     C*----------------------------------------------------*
025300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025400010330     C*----------------------------------------------------*
025500010330     C     INZVAR        BEGSR
025600010330     C*
025700010330     C*
025800010330     C                   ENDSR
025900010330     C*----------------------------------------------------*
026000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
026100010330     C*----------------------------------------------------*
026200010330     C     DEFCAM        BEGSR
026300010330     C*
026400110126     C                   CLEAR                   EDIVAX00
026500010330     C*
026600010330     C                   ENDSR
026700010330     C*----------------------------------------------------*
026800040524     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
026900010330     C*----------------------------------------------------*
027000040524     C     IMPVAX        BEGSR
027100010330     C*
027200010604     C                   SETOFF                                       3132
027300010612     C*
027400010612     C                   SETOFF                                       303132
027500010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
027600040524     C                   TESTN                   a_vaxccm             30
027700040524     C  N30              TESTN                   a_vaxccm               30
027800040524     C  N30              TESTN                   a_vaxccm                 30
027900040524     C                   MOVE      a_vaxccm      W001A             1
028000010612     C     *IN30         IFEQ      *OFF
028100010612     C     W001A         ORLT      '0'
028200010612     C                   SETON                                        31
028300040524     C                   Z-ADD     *zeros        VAXCCM
028400040524     C                   EVAL      vinmsg = 'VAXCCM'
028500010612     C                   ELSE
028600040524     C                   MOVE      a_vaxccm      VAXCCM
028700040524     C     VAXCCM        IFLE      *ZEROS
028800010612     C                   SETON                                        31
028900040524     C                   Z-ADD     *zeros        VAXCCM
029000040524     C                   EVAL      vinmsg = 'VAXCCM'
029100010612     C                   ENDIF
029200010612     C                   ENDIF
029300010612     C*
029400040524     C                   TESTN                   a_vaxlnp             30
029500040524     C  N30              TESTN                   a_vaxlnp               30
029600040524     C  N30              TESTN                   a_vaxlnp                 30
029700040524     C                   MOVE      a_vaxlnp      W001A             1
029800010612     C     *IN30         IFEQ      *OFF
029900010612     C     W001A         ORLT      '0'
030000010612     C                   SETON                                        31
030100040524     C                   Z-ADD     *zeros        VAXLNP
030200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
030300040524     C                             + ' ' + 'VAXLNP'
030400010612     C                   ELSE
030500040524     C                   MOVE      a_vaxlnp      VAXLNP
030600040524     C     VAXLNP        IFLE      *ZEROS
030700010612     C                   SETON                                        31
030800040524     C                   Z-ADD     *zeros        VAXLNP
030900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
031000040524     C                             + ' ' + 'VAXLNP'
031100010612     C                   ENDIF
031200010612     C                   ENDIF
031300010612     C*
031400040524     C                   TESTN                   a_vaxaas             30
031500040524     C  N30              TESTN                   a_vaxaas               30
031600040524     C  N30              TESTN                   a_vaxaas                 30
031700040524     C                   MOVE      a_vaxaas      W001A             1
031800010612     C     *IN30         IFEQ      *OFF
031900010612     C     W001A         ORLT      '0'
032000010612     C                   SETON                                        31
032100040524     C                   MOVEL(P)  datcor        VAXAAS
032200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
032300040524     C                             + ' ' + 'VAXAAS'
032400010612     C                   ELSE
032500040524     C                   MOVE      a_vaxaas      VAXAAS
032600010612     C                   ENDIF
032700010612     C*
032800040524     C                   TESTN                   a_vaxnrs             30
032900040524     C  N30              TESTN                   a_vaxnrs               30
033000040524     C  N30              TESTN                   a_vaxnrs                 30
033100040524     C                   MOVE      a_vaxnrs      W001A             1
033200010612     C     *IN30         IFEQ      *OFF
033300010612     C     W001A         ORLT      '0'
033400010612     C     W001A         ANDNE     *BLANK
033500010612     C                   SETON                                        31
033600040524     C                   Z-ADD     *zeros        VAXNRS
033700010612     C                   EVAL      vinmsg = %trimr(vinmsg)
033800040524     C                             + ' ' + 'VAXNRS'
033900010612     C                   ELSE
034000040524     C                   MOVE      a_vaxnrs      VAXNRS
034100010612     C                   ENDIF
034200010612     C*
034300040524     C                   TESTN                   a_vaxnsp             30
034400040524     C  N30              TESTN                   a_vaxnsp               30
034500040524     C  N30              TESTN                   a_vaxnsp                 30
034600040524     C                   MOVE      a_vaxnsp      W001A             1
034700010612     C     *IN30         IFEQ      *OFF
034800010612     C     W001A         ORLT      '0'
034900010612     C                   SETON                                        31
035000040524     C                   Z-ADD     *zeros        VAXNSP
035100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
035200040524     C                             + ' ' + 'VAXNSP'
035300010612     C                   ELSE
035400040524     C                   MOVE      a_vaxnsp      VAXNSP
035500010612     C                   ENDIF
035600040524     C*
035700040524     C                   TESTN                   a_vaxprr             30
035800040524     C  N30              TESTN                   a_vaxprr               30
035900040524     C  N30              TESTN                   a_vaxprr                 30
036000040524     C                   MOVE      a_vaxprr      W001A             1
036100040524     C     *IN30         IFEQ      *OFF
036200040524     C     W001A         ORLT      '0'
036300040524     C                   SETON                                        31
036400040524     C                   Z-ADD     *zeros        VAXPRR
036500040524     C                   EVAL      vinmsg = %trimr(vinmsg)
036600040524     C                             + ' ' + 'VAXPRR'
036700040524     C                   ELSE
036800040524     C                   MOVE      a_vaxprr      VAXPRR
036900040524     C                   ENDIF
037000010612     C*
037100010612     C* ALFABETICI
037200040524     C                   MOVE      a_vaxatb      VAXATB
037300040524     C                   MOVE      a_vaxtmd      VAXTMD
037400040524     C                   MOVE      a_vaxtrk      VAXTRK
037500040524     C                   MOVE      a_vaxdtt      VAXDTT
037600010330     C*
037700010330     C                   ENDSR
037800010612     C*----------------------------------------------------*
037900990920
038000010601
038100010601
038200010601
038300010601
038400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
038500010601     C     repfil        BEGSR
038600010601     C*
038700010601     C                   if        invfil = *zeros and
038800010601     C                             depfil > *zeros and
038900010613     C                             (vinflg = *blanks or
039000010613     C                              vinflg = *zeros)
039100010601     C
039200010601     C                   eval      invfil = depfil
039300010601     C                   endif
039400010601     C*
039500010601     C                   if        depfil <> invfil and
039600010601     C                             invfil > *zeros
039700010601     C                   eval      flgMulti = '1'
039800010601     C                   if        vinflg = *blanks
039900010601     C                   add       1             cntNonEl
040000010601     C                   endif
040100010601     C                   endif
040200010601     C*
040300010601     C                   if        vinflg = '2'
040400010601     C                   eval      flgStato = '2'
040500010601     C                   endif
040600010601     C*
040700010601     C                   ENDSR
040800010601     C***
040900010601
041000010601
041100010601
041200010330
041300010330
041400010330
041500990920      /TITLE Invio dei dati al punto operativo.
041600000613     C     invio         BEGSR
041700990920     C*
041800990920     C                   reset                   dscmz
041900010601     C                   move      invfil        cmzdst
042000990920     C                   eval      cmzfld = vlrfou
042100990920     C                   eval      cmzmbd = vlrhdl
042200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
042300000710     C                   if        prmfir = *blanks
042400110126     C                   eval      cmzfla = 'EDIVAX0F'
042500110126     C                   eval      cmzmba = 'EDIVAX0F'
042600000710     C                   else
042700000710     C                   eval      cmzfla = prmfir
042800000710     C                   eval      cmzmba = prmfir
042900000710     C                   endif
043000990920     C                   eval      cmznrr = *zeros
043100990920     C                   move      §ctrok        cmznrr
043200021003     C                   eval      cmzlba = vlrfl1
043300990920     C                   call(e)   'TIS711C'
043400990920     C                   parm                    dscmz
043500990921     C                   parm      *blanks       esito
043600990920     C                   if        %error
043700990920     C                             or cmzerr = '1'
043800990921     C                             or esito  = '1'
043900000710     C                   eval      wrkesito = '3'
044000990920     C                   endif
044100990920     C*
044200000613     C                   ENDSR
044300990910
044400010601
044500010601
044600010601
044700010601
044800010601      /TITLE Invio dei dati al punto operativo.
044900010601     C     opeini        BEGSR
045000010601     C*
045100010601     C* Inizializzo flag e contatori operativi
045200010601     C                   movel     '0'           flgGiro           1
045300010601     C                   movel     '0'           flgMulti          1
045400010601     C                   movel     '1'           flgStato          1
045500010615     C                   movel     '0'           flgOk             1
045600010601     C                   z-add     *zeros        cntNonEl         10 0
045700010601     C                   z-add     *zeros        depfil            3 0
045800010601     C                   z-add     *zeros        invfil            3 0
045900030407     C*
046000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
046100030407     C                   if        %subst(vlrppt:1:1) = 'S'
046200030407     C                   seton                                        50
046300030407     C                   else
046400030407     C                   setoff                                       50
046500030407     C                   endif
046600010601     C*
046700010601     C                   ENDSR
046800010601     C***
046900010601
047000010601
047100010601
047200010330
047300010330
047400000613     C     *inzsr        BEGSR
047500990910     C*
047600990910     C     *entry        plist
047700990920     C                   parm                    tivlrds
047800990921     C                   parm      wrkesito      esito
047900000724     C                   parm                    prmlit
048000000710     C                   parm                    prmfir
048100110126     C*
048200110126     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
048300110126     C                   move(p)   vlrMSG        wNomeFile
048400010330     C*
048500010330     C* CALCOLA LA DATA CORRENTE
048600010330     C                   time                    wn14             14 0
048700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
048800010330     C                   z-add     wn8           g08dat
048900010330     C                   z-add     *zeros        g08inv
049000010330     C                   movel     '0'           g08err
049100010330     C                   call      'XSRDA8'
049200010330     C                   parm                    wlbda8
049300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
049400000613     C*
049500000613     C                   ENDSR
049600000613     C***
