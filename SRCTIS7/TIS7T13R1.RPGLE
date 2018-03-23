000100040524      /TITLE Upload via Internet: traduzione in FIVAXWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040524     FFIVAXwwr  O    E             DISK    usropn
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
002900010330     D rrnum           s              6  0 INZ(*zeros)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000010612     D*
004100040524     D fnvaxaar        ds                  inz
004200040524     D  a_vaxatb               1      1
004300040524     D  a_vaxccm               3      9
004400040524     D  a_vaxlnp              11     13
004500040524     D  a_vaxaas              15     18
004600040524     D  a_vaxnrs              20     21
004700040524     D  a_vaxnsp              23     29
004800040524     D  a_vaxtmd              30     33
004900040524     D  a_vaxprr              35     38
005000040524     D  a_vaxtrk              39     40
005100040524     D  a_vaxdtt              41    540
005200010330
005300010330
005400010330
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000010601     C*
006100010601     C                   exsr      opeini
006200040524     C                   exsr      rwvax
006300010601     C*
006400010601     C                   seton                                        lr
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007100010601     C*--------------------------------------------------------
007200010601     C     PREELA        BEGSR
007300010601     C*
007400010601     C* SE OCCORRE SPEDIRE IN FILIALE
007500010601     C                   if        invfil <> *zeros and
007600010601     C                             flgGiro = '0'
007700010601     C*
007800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007900010601     C                   eval      flgGiro = '1'
011000010601     C*
011100010601     C                   endif
011200010601     C*
011300010601     C                   ENDSR
011400010601     C***
011500010601
011600010601
011700010601
011800010601
011900010601
012000010601
012100010601
012200010601     C*--------------------------------------------------------
012300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012400010601     C*--------------------------------------------------------
012500010601     C     ENDELA        BEGSR
013900000616     C*
014000010601     C                   ENDSR
014100010601     C***
016400000613
016500010330
016600010601
016700010601
016800010601
016900010330     C*--------------------------------------------------------
017000040524     C* RWVAX   LEGGE TIVIN00R E SCRIVE FIVAXWWF              *
017100010330     C*--------------------------------------------------------
017200040524     C     RWVAX         BEGSR
017300010330     C*
017400010330     C                   if        not %open(tivin00r)
017500010330     C                   open      tivin00r
017600010330     C                   endif
017700040524     C                   if        not %open(fivaxwwr)
017800040524     C                   open      fivaxwwr
017900010330     C                   endif
018000010330     C*
018100010604     C                   clear                   §CTROK
018200010604     C                   clear                   §CTRMO
018300010604     C                   clear                   §CTRNO
018400010330     C*
018500010330     C                   DO        *HIVAL
018600010330     C*
018700010330     C                   READ      tivin00r                               70
018800010618     C*
018900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019000010618     C                   if        vinflg = '1'
019100010618     C                   eval      flgOk = '1'
019200010618     C                   endif
019300010618     C*
019400010330     C                   if        vindta > *blanks
019500010330     C                   add       1             rrnum
019600010330     C*
019700010601     C                   if        *in70 = *off and
019800010330     C                             (vinflg = *blanks
019900010330     C                              or vinflg = '0'
020000010330     C                              or vinflg = '2')
020100010711     C*
020200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020300010711     C                   if        vinflg = *blanks or vinflg = '0'
020400010711     C                   clear                   vinmsg
020500010711     C                   endif
020600010711     C*
020700040524     C                   eval      fnvaxaar = vindta
020800010601     C*
020900010330     C                   exsr      inzvar
021000010330     C                   exsr      defcam
021100040524     C                   exsr      impvax
021200010601     C*
021300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021400040524     C                   eval      depfil = VAXLNP
021500010601     C                   exsr      repfil
021600021010     C                   if        depfil = invfil
021700021014     C                   if        vlrpoi = 999
021800040524     C                   MOVE(P)   invfil        VAXFGS
021900021014     C                   else
022000040524     C                   MOVE(P)   vlrpoi        VAXFGS
022100021014     C                   endif
022200010601     C*
022300010601     C                   exsr      PREELA
022400010601     C*
022500010604     C* Ebbene...
022600010604     C*
022700010604     C  N31              ADD       1             §CTROK            7 0
022800010604     C   32              ADD       1             §CTRMO            7 0
022900010604     C   31              ADD       1             §CTRNO            7 0
023000040524     C  N31              WRITE     FIVAX000
023100010604     C*
023200010604     C                   if        *in31 = *off and
023300010604     C                             *in32 = *off
023400010604     C                   eval      vinflg = '1'
023500010604     C                   else
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
028200040524     C                   if        %open(fivaxwwr)
028300040524     C                   close     fivaxwwr
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
031800040524     C                   CLEAR                   FIVAX000
031900010330     C*
032000010330     C                   ENDSR
032100010330     C*----------------------------------------------------*
032200040524     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
032300010330     C*----------------------------------------------------*
032400040524     C     IMPVAX        BEGSR
032500010330     C*
032600010604     C                   SETOFF                                       3132
032700010612     C*
032800010612     C                   SETOFF                                       303132
032900010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
033000040524     C                   TESTN                   a_vaxccm             30
033100040524     C  N30              TESTN                   a_vaxccm               30
033200040524     C  N30              TESTN                   a_vaxccm                 30
033300040524     C                   MOVE      a_vaxccm      W001A             1
033400010612     C     *IN30         IFEQ      *OFF
033500010612     C     W001A         ORLT      '0'
033600010612     C                   SETON                                        31
033700040524     C                   Z-ADD     *zeros        VAXCCM
033800040524     C                   EVAL      vinmsg = 'VAXCCM'
033900010612     C                   ELSE
034000040524     C                   MOVE      a_vaxccm      VAXCCM
034100040524     C     VAXCCM        IFLE      *ZEROS
034200010612     C                   SETON                                        31
034300040524     C                   Z-ADD     *zeros        VAXCCM
034400040524     C                   EVAL      vinmsg = 'VAXCCM'
034500010612     C                   ENDIF
034600010612     C                   ENDIF
034700010612     C*
034800040524     C                   TESTN                   a_vaxlnp             30
034900040524     C  N30              TESTN                   a_vaxlnp               30
035000040524     C  N30              TESTN                   a_vaxlnp                 30
035100040524     C                   MOVE      a_vaxlnp      W001A             1
035200010612     C     *IN30         IFEQ      *OFF
035300010612     C     W001A         ORLT      '0'
035400010612     C                   SETON                                        31
035500040524     C                   Z-ADD     *zeros        VAXLNP
035600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
035700040524     C                             + ' ' + 'VAXLNP'
035800010612     C                   ELSE
035900040524     C                   MOVE      a_vaxlnp      VAXLNP
036000040524     C     VAXLNP        IFLE      *ZEROS
036100010612     C                   SETON                                        31
036200040524     C                   Z-ADD     *zeros        VAXLNP
036300010612     C                   EVAL      vinmsg = %trimr(vinmsg)
036400040524     C                             + ' ' + 'VAXLNP'
036500010612     C                   ENDIF
036600010612     C                   ENDIF
036700010612     C*
036800040524     C                   TESTN                   a_vaxaas             30
036900040524     C  N30              TESTN                   a_vaxaas               30
037000040524     C  N30              TESTN                   a_vaxaas                 30
037100040524     C                   MOVE      a_vaxaas      W001A             1
037200010612     C     *IN30         IFEQ      *OFF
037300010612     C     W001A         ORLT      '0'
037400010612     C                   SETON                                        31
037500040524     C                   MOVEL(P)  datcor        VAXAAS
037600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
037700040524     C                             + ' ' + 'VAXAAS'
037800010612     C                   ELSE
037900040524     C                   MOVE      a_vaxaas      VAXAAS
038000010612     C                   ENDIF
038100010612     C*
038200040524     C                   TESTN                   a_vaxnrs             30
038300040524     C  N30              TESTN                   a_vaxnrs               30
038400040524     C  N30              TESTN                   a_vaxnrs                 30
038500040524     C                   MOVE      a_vaxnrs      W001A             1
038600010612     C     *IN30         IFEQ      *OFF
038700010612     C     W001A         ORLT      '0'
038800010612     C     W001A         ANDNE     *BLANK
038900010612     C                   SETON                                        31
039000040524     C                   Z-ADD     *zeros        VAXNRS
039100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
039200040524     C                             + ' ' + 'VAXNRS'
039300010612     C                   ELSE
039400040524     C                   MOVE      a_vaxnrs      VAXNRS
039500010612     C                   ENDIF
039600010612     C*
039700040524     C                   TESTN                   a_vaxnsp             30
039800040524     C  N30              TESTN                   a_vaxnsp               30
039900040524     C  N30              TESTN                   a_vaxnsp                 30
040000040524     C                   MOVE      a_vaxnsp      W001A             1
040100010612     C     *IN30         IFEQ      *OFF
040200010612     C     W001A         ORLT      '0'
040300010612     C                   SETON                                        31
040400040524     C                   Z-ADD     *zeros        VAXNSP
040500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
040600040524     C                             + ' ' + 'VAXNSP'
040700010612     C                   ELSE
040800040524     C                   MOVE      a_vaxnsp      VAXNSP
040900010612     C                   ENDIF
041000040524     C*
041100040524     C                   TESTN                   a_vaxprr             30
041200040524     C  N30              TESTN                   a_vaxprr               30
041300040524     C  N30              TESTN                   a_vaxprr                 30
041400040524     C                   MOVE      a_vaxprr      W001A             1
041500040524     C     *IN30         IFEQ      *OFF
041600040524     C     W001A         ORLT      '0'
041700040524     C                   SETON                                        31
041800040524     C                   Z-ADD     *zeros        VAXPRR
041900040524     C                   EVAL      vinmsg = %trimr(vinmsg)
042000040524     C                             + ' ' + 'VAXPRR'
042100040524     C                   ELSE
042200040524     C                   MOVE      a_vaxprr      VAXPRR
042300040524     C                   ENDIF
042400010612     C*
042500010612     C* ALFABETICI
042600040524     C                   MOVE      a_vaxatb      VAXATB
042700040524     C                   MOVE      a_vaxtmd      VAXTMD
042800040524     C                   MOVE      a_vaxtrk      VAXTRK
042900040524     C                   MOVE      a_vaxdtt      VAXDTT
043000010330     C*
043100010330     C                   ENDSR
043200010612     C*----------------------------------------------------*
043300990920
043400010601
043500010601
043600010601
043700010601
043800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
043900010601     C     repfil        BEGSR
044000010601     C*
044100010601     C                   if        invfil = *zeros and
044200010601     C                             depfil > *zeros and
044300010613     C                             (vinflg = *blanks or
044400010613     C                              vinflg = *zeros)
044500010601     C
044600010601     C                   eval      invfil = depfil
044700010601     C                   endif
044800010601     C*
044900010601     C                   if        depfil <> invfil and
045000010601     C                             invfil > *zeros
045100010601     C                   eval      flgMulti = '1'
045200010601     C                   if        vinflg = *blanks
045300010601     C                   add       1             cntNonEl
045400010601     C                   endif
045500010601     C                   endif
045600010601     C*
045700010601     C                   if        vinflg = '2'
045800010601     C                   eval      flgStato = '2'
045900010601     C                   endif
046000010601     C*
046100010601     C                   ENDSR
046200010601     C***
046300010601
046400010601
046500010601
046600010330
046700010330
046800010330
046900990920      /TITLE Invio dei dati al punto operativo.
047000000613     C     invio         BEGSR
047100990920     C*
047200990920     C                   reset                   dscmz
047300010601     C                   move      invfil        cmzdst
047400990920     C                   eval      cmzfld = vlrfou
047500990920     C                   eval      cmzmbd = vlrhdl
047600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
047700000710     C                   if        prmfir = *blanks
047800040524     C                   eval      cmzfla = 'FIVAX00F'
047900040524     C                   eval      cmzmba = 'FIVAX00F'
048000000710     C                   else
048100000710     C                   eval      cmzfla = prmfir
048200000710     C                   eval      cmzmba = prmfir
048300000710     C                   endif
048400990920     C                   eval      cmznrr = *zeros
048500990920     C                   move      §ctrok        cmznrr
048600021003     C                   eval      cmzlba = vlrfl1
048700990920     C                   call(e)   'TIS711C'
048800990920     C                   parm                    dscmz
048900990921     C                   parm      *blanks       esito
049000990920     C                   if        %error
049100990920     C                             or cmzerr = '1'
049200990921     C                             or esito  = '1'
049300000710     C                   eval      wrkesito = '3'
049400990920     C                   endif
049500990920     C*
049600000613     C                   ENDSR
049700990910
049800010601
049900010601
050000010601
050100010601
050200010601      /TITLE Invio dei dati al punto operativo.
050300010601     C     opeini        BEGSR
050400010601     C*
050500010601     C* Inizializzo flag e contatori operativi
050600010601     C                   movel     '0'           flgGiro           1
050700010601     C                   movel     '0'           flgMulti          1
050800010601     C                   movel     '1'           flgStato          1
050900010615     C                   movel     '0'           flgOk             1
051000010601     C                   z-add     *zeros        cntNonEl         10 0
051100010601     C                   z-add     *zeros        depfil            3 0
051200010601     C                   z-add     *zeros        invfil            3 0
051300030407     C*
051400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
051500030407     C                   if        %subst(vlrppt:1:1) = 'S'
051600030407     C                   seton                                        50
051700030407     C                   else
051800030407     C                   setoff                                       50
051900030407     C                   endif
052000010601     C*
052100010601     C                   ENDSR
052200010601     C***
052300010601
052400010601
052500010601
052600010330
052700010330
052800000613     C     *inzsr        BEGSR
052900990910     C*
053000990910     C     *entry        plist
053100990920     C                   parm                    tivlrds
053200990921     C                   parm      wrkesito      esito
053300000724     C                   parm                    prmlit
053400000710     C                   parm                    prmfir
053500010330     C*
053600010330     C* CALCOLA LA DATA CORRENTE
053700010330     C                   time                    wn14             14 0
053800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053900010330     C                   z-add     wn8           g08dat
054000010330     C                   z-add     *zeros        g08inv
054100010330     C                   movel     '0'           g08err
054200010330     C                   call      'XSRDA8'
054300010330     C                   parm                    wlbda8
054400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054500000613     C*
054600000613     C                   ENDSR
054700000613     C***
