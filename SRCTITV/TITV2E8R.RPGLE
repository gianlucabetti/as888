000100040524      /TITLE Upload via Internet: traduzione in FIVAXWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040524     FFIVAXwwr  O    E             DISK    usropn
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
002800040524     D fnvaxaar        ds                  inz
002900040524     D  a_vaxatb               1      1
003000040524     D  a_vaxccm               3      9
003100040524     D  a_vaxlnp              11     13
003200040524     D  a_vaxaas              15     18
003300040524     D  a_vaxnrs              20     21
003400040524     D  a_vaxnsp              23     29
003500040524     D  a_vaxtmd              30     33
003600040524     D  a_vaxprr              35     38
003700040524     D  a_vaxtrk              39     40
003800040524     D  a_vaxdtt              41    540
003900010330
004000010330
004100010330
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500010601     C*
004600010601     C                   exsr      opeini
004700040524     C                   exsr      rwvax
004800010601     C*
004900010601     C                   seton                                        lr
005000010601
005100010601
005200010601
005300010601
005400010601     C*--------------------------------------------------------
005500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005600010601     C*--------------------------------------------------------
005700010601     C     PREELA        BEGSR
005800010601     C*
005900010601     C* SE OCCORRE SPEDIRE IN FILIALE
006000010601     C                   if        invfil <> *zeros and
006100010601     C                             flgGiro = '0'
006200010601     C*
006300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006400010601     C                   eval      flgGiro = '1'
006500010601     C*
006600010601     C                   endif
006700010601     C*
006800010601     C                   ENDSR
006900010601     C***
007000010601
007100010601
007200010601
007300010601
007400010601
007500010601
007600010601
007700010601     C*--------------------------------------------------------
007800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007900010601     C*--------------------------------------------------------
008000010601     C     ENDELA        BEGSR
008100000616     C*
008200010601     C                   ENDSR
008300010601     C***
008400000613
008500010330
008600010601
008700010601
008800010601
008900010330     C*--------------------------------------------------------
009000040524     C* RWVAX   LEGGE TIVIN00R E SCRIVE FIVAXWWF              *
009100010330     C*--------------------------------------------------------
009200040524     C     RWVAX         BEGSR
009300010330     C*
009400010330     C                   if        not %open(tivin00r)
009500010330     C                   open      tivin00r
009600010330     C                   endif
009700040524     C                   if        not %open(fivaxwwr)
009800040524     C                   open      fivaxwwr
009900010330     C                   endif
010000010330     C*
010100010604     C                   clear                   §CTROK
010200010604     C                   clear                   §CTRMO
010300010604     C                   clear                   §CTRNO
010400010330     C*
010500010330     C                   DO        *HIVAL
010600010330     C*
010700010330     C                   READ      tivin00r                               70
010800010618     C*
010900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011000010618     C                   if        vinflg = '1'
011100010618     C                   eval      flgOk = '1'
011200010618     C                   endif
011300010618     C*
011400010330     C                   if        vindta > *blanks
011500010330     C                   add       1             rrnum
011600010330     C*
011700010601     C                   if        *in70 = *off and
011800010330     C                             (vinflg = *blanks
011900010330     C                              or vinflg = '0'
012000010330     C                              or vinflg = '2')
012100010711     C*
012200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012300010711     C                   if        vinflg = *blanks or vinflg = '0'
012400010711     C                   clear                   vinmsg
012500010711     C                   endif
012600010711     C*
012700040524     C                   eval      fnvaxaar = vindta
012800010601     C*
012900010330     C                   exsr      inzvar
013000010330     C                   exsr      defcam
013100040524     C                   exsr      impvax
013200010601     C*
013300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013400040524     C                   eval      depfil = VAXLNP
013500010601     C                   exsr      repfil
013600021010     C                   if        depfil = invfil
013700021014     C                   if        vlrpoi = 999
013800040524     C                   MOVE(P)   invfil        VAXFGS
013900021014     C                   else
014000040524     C                   MOVE(P)   vlrpoi        VAXFGS
014100021014     C                   endif
014200010601     C*
014300010601     C                   exsr      PREELA
014400110429     C*
014500110429     C* Forzature x diversa gestione traffici ILLY
014600110429     C*
014700110429     C* CCM
014800110429     C                   MOVEL     VAXNSP        wCLI              1
014900110429     C                   IF        wCLI = '7'
015000110429     C                   EVAL      VAXCCM = 1162074
015100110429     C                   ENDIF
015200010601     C*
015300010604     C* Ebbene...
015400010604     C*
015500010604     C  N31              ADD       1             §CTROK            7 0
015600010604     C   32              ADD       1             §CTRMO            7 0
015700010604     C   31              ADD       1             §CTRNO            7 0
015800040524     C  N31              WRITE     FIVAX000
015900010604     C*
016000010604     C                   if        *in31 = *off and
016100010604     C                             *in32 = *off
016200010604     C                   eval      vinflg = '1'
016300010604     C                   else
016400010604     C                   eval      vinflg = '2'
016500010604     C                   endif
016600010604     C                   endif
016700010601     C*
016800010604     C                   endif
016900010604     C*
017000010330     C                   else
017100010330     C                   eval      vinflg = '1'
017200010330     C                   endif
017300010601     C*
017400010601     C  N70              update    tivin000
017500010330     C*
017600010330     C  N70              ENDdo
017700010601     C
017800010601     C*
017900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018000010601     C                   if        cntNonEl = *zeros or
018100010601     C                             flgMulti = '0'
018200010330     C* Se non ci sono record con errori ...
018300010601     C                   if        §ctrno = 0 and
018400010604     C                             §ctrmo = 0 and
018500010601     C                             flgStato <> '2'
018600010330     C* ... restituisco esito OK.
018700010330     C                   eval      wrkesito = '0'
018800010330     C                   else
018900010330     C                   if        §ctrok > 0
019000010330     C                   eval      wrkesito = '1'
019100010330     C                   else
019200010615     C                   if        flgOk = '0'
019300010615     C                   eval      wrkesito = '2'
019400010615     C                   else
019500010615     C                   eval      wrkesito = '6'
019600010615     C                   endif
019700010330     C                   endif
019800010330     C                   endif
019900010601     C                   else
020000010601     C                   eval      wrkesito = '9'
020100010601     C                   endif
020200010330     C*
020300010330     C                   if        %open(tivin00r)
020400010330     C                   close     tivin00r
020500010330     C                   endif
020600040524     C                   if        %open(fivaxwwr)
020700040524     C                   close     fivaxwwr
020800010330     C                   endif
020900010601     C*
021000010601     C                   if        vlrpoi <> 999
021100010601     C                   eval      invfil = vlrpoi
021200010601     C                   endif
021300010330     C*
021400010330     C                   if        §ctrok > 0
021500010601     C                             and invfil > *zeros
021600010330     C                   exsr      invio
021700010330     C                   endif
021800010601     C*
021900010612     C                   if        flgGiro = '1'
022000010601     C                   exsr      endela
022100010612     C                   endif
022200010330     C*
022300010330     C                   ENDSR
022400010330     C***
022500010330
022600010601
022700010601
022800010601
022900010601
023000010330     C*----------------------------------------------------*
023100010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023200010330     C*----------------------------------------------------*
023300010330     C     INZVAR        BEGSR
023400010330     C*
023500010330     C*
023600010330     C                   ENDSR
023700010330     C*----------------------------------------------------*
023800010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023900010330     C*----------------------------------------------------*
024000010330     C     DEFCAM        BEGSR
024100010330     C*
024200040524     C                   CLEAR                   FIVAX000
024300010330     C*
024400010330     C                   ENDSR
024500010330     C*----------------------------------------------------*
024600040524     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
024700010330     C*----------------------------------------------------*
024800040524     C     IMPVAX        BEGSR
024900010330     C*
025000010604     C                   SETOFF                                       3132
025100010612     C*
025200010612     C                   SETOFF                                       303132
025300010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
025400040524     C                   TESTN                   a_vaxccm             30
025500040524     C  N30              TESTN                   a_vaxccm               30
025600040524     C  N30              TESTN                   a_vaxccm                 30
025700040524     C                   MOVE      a_vaxccm      W001A             1
025800010612     C     *IN30         IFEQ      *OFF
025900010612     C     W001A         ORLT      '0'
026000010612     C                   SETON                                        31
026100040524     C                   Z-ADD     *zeros        VAXCCM
026200040524     C                   EVAL      vinmsg = 'VAXCCM'
026300010612     C                   ELSE
026400040524     C                   MOVE      a_vaxccm      VAXCCM
026500040524     C     VAXCCM        IFLE      *ZEROS
026600010612     C                   SETON                                        31
026700040524     C                   Z-ADD     *zeros        VAXCCM
026800040524     C                   EVAL      vinmsg = 'VAXCCM'
026900010612     C                   ENDIF
027000010612     C                   ENDIF
027100010612     C*
027200040524     C                   TESTN                   a_vaxlnp             30
027300040524     C  N30              TESTN                   a_vaxlnp               30
027400040524     C  N30              TESTN                   a_vaxlnp                 30
027500040524     C                   MOVE      a_vaxlnp      W001A             1
027600010612     C     *IN30         IFEQ      *OFF
027700010612     C     W001A         ORLT      '0'
027800010612     C                   SETON                                        31
027900040524     C                   Z-ADD     *zeros        VAXLNP
028000010612     C                   EVAL      vinmsg = %trimr(vinmsg)
028100040524     C                             + ' ' + 'VAXLNP'
028200010612     C                   ELSE
028300040524     C                   MOVE      a_vaxlnp      VAXLNP
028400040524     C     VAXLNP        IFLE      *ZEROS
028500010612     C                   SETON                                        31
028600040524     C                   Z-ADD     *zeros        VAXLNP
028700010612     C                   EVAL      vinmsg = %trimr(vinmsg)
028800040524     C                             + ' ' + 'VAXLNP'
028900010612     C                   ENDIF
029000010612     C                   ENDIF
029100010612     C*
029200040524     C                   TESTN                   a_vaxaas             30
029300040524     C  N30              TESTN                   a_vaxaas               30
029400040524     C  N30              TESTN                   a_vaxaas                 30
029500040524     C                   MOVE      a_vaxaas      W001A             1
029600010612     C     *IN30         IFEQ      *OFF
029700010612     C     W001A         ORLT      '0'
029800010612     C                   SETON                                        31
029900040524     C                   MOVEL(P)  datcor        VAXAAS
030000010612     C                   EVAL      vinmsg = %trimr(vinmsg)
030100040524     C                             + ' ' + 'VAXAAS'
030200010612     C                   ELSE
030300040524     C                   MOVE      a_vaxaas      VAXAAS
030400010612     C                   ENDIF
030500010612     C*
030600040524     C                   TESTN                   a_vaxnrs             30
030700040524     C  N30              TESTN                   a_vaxnrs               30
030800040524     C  N30              TESTN                   a_vaxnrs                 30
030900040524     C                   MOVE      a_vaxnrs      W001A             1
031000010612     C     *IN30         IFEQ      *OFF
031100010612     C     W001A         ORLT      '0'
031200010612     C     W001A         ANDNE     *BLANK
031300010612     C                   SETON                                        31
031400040524     C                   Z-ADD     *zeros        VAXNRS
031500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
031600040524     C                             + ' ' + 'VAXNRS'
031700010612     C                   ELSE
031800040524     C                   MOVE      a_vaxnrs      VAXNRS
031900010612     C                   ENDIF
032000010612     C*
032100040524     C                   TESTN                   a_vaxnsp             30
032200040524     C  N30              TESTN                   a_vaxnsp               30
032300040524     C  N30              TESTN                   a_vaxnsp                 30
032400040524     C                   MOVE      a_vaxnsp      W001A             1
032500010612     C     *IN30         IFEQ      *OFF
032600010612     C     W001A         ORLT      '0'
032700010612     C                   SETON                                        31
032800040524     C                   Z-ADD     *zeros        VAXNSP
032900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
033000040524     C                             + ' ' + 'VAXNSP'
033100010612     C                   ELSE
033200040524     C                   MOVE      a_vaxnsp      VAXNSP
033300010612     C                   ENDIF
033400040524     C*
033500040524     C                   TESTN                   a_vaxprr             30
033600040524     C  N30              TESTN                   a_vaxprr               30
033700040524     C  N30              TESTN                   a_vaxprr                 30
033800040524     C                   MOVE      a_vaxprr      W001A             1
033900040524     C     *IN30         IFEQ      *OFF
034000040524     C     W001A         ORLT      '0'
034100040524     C                   SETON                                        31
034200040524     C                   Z-ADD     *zeros        VAXPRR
034300040524     C                   EVAL      vinmsg = %trimr(vinmsg)
034400040524     C                             + ' ' + 'VAXPRR'
034500040524     C                   ELSE
034600040524     C                   MOVE      a_vaxprr      VAXPRR
034700040524     C                   ENDIF
034800010612     C*
034900010612     C* ALFABETICI
035000040524     C                   MOVE      a_vaxatb      VAXATB
035100040524     C                   MOVE      a_vaxtmd      VAXTMD
035200040524     C                   MOVE      a_vaxtrk      VAXTRK
035300040524     C                   MOVE      a_vaxdtt      VAXDTT
035400010330     C*
035500010330     C                   ENDSR
035600010612     C*----------------------------------------------------*
035700990920
035800010601
035900010601
036000010601
036100010601
036200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
036300010601     C     repfil        BEGSR
036400010601     C*
036500010601     C                   if        invfil = *zeros and
036600010601     C                             depfil > *zeros and
036700010613     C                             (vinflg = *blanks or
036800010613     C                              vinflg = *zeros)
036900010601     C
037000010601     C                   eval      invfil = depfil
037100010601     C                   endif
037200010601     C*
037300010601     C                   if        depfil <> invfil and
037400010601     C                             invfil > *zeros
037500010601     C                   eval      flgMulti = '1'
037600010601     C                   if        vinflg = *blanks
037700010601     C                   add       1             cntNonEl
037800010601     C                   endif
037900010601     C                   endif
038000010601     C*
038100010601     C                   if        vinflg = '2'
038200010601     C                   eval      flgStato = '2'
038300010601     C                   endif
038400010601     C*
038500010601     C                   ENDSR
038600010601     C***
038700010601
038800010601
038900010601
039000010330
039100010330
039200010330
039300990920      /TITLE Invio dei dati al punto operativo.
039400000613     C     invio         BEGSR
039500990920     C*
039600990920     C                   reset                   dscmz
039700010601     C                   move      invfil        cmzdst
039800990920     C                   eval      cmzfld = vlrfou
039900990920     C                   eval      cmzmbd = vlrhdl
040000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
040100000710     C                   if        prmfir = *blanks
040200040524     C                   eval      cmzfla = 'FIVAX00F'
040300040524     C                   eval      cmzmba = 'FIVAX00F'
040400000710     C                   else
040500000710     C                   eval      cmzfla = prmfir
040600000710     C                   eval      cmzmba = prmfir
040700000710     C                   endif
040800990920     C                   eval      cmznrr = *zeros
040900990920     C                   move      §ctrok        cmznrr
041000021003     C                   eval      cmzlba = vlrfl1
041100990920     C                   call(e)   'TIS711C'
041200990920     C                   parm                    dscmz
041300990921     C                   parm      *blanks       esito
041400990920     C                   if        %error
041500990920     C                             or cmzerr = '1'
041600990921     C                             or esito  = '1'
041700000710     C                   eval      wrkesito = '3'
041800990920     C                   endif
041900990920     C*
042000000613     C                   ENDSR
042100990910
042200010601
042300010601
042400010601
042500010601
042600010601      /TITLE Invio dei dati al punto operativo.
042700010601     C     opeini        BEGSR
042800010601     C*
042900010601     C* Inizializzo flag e contatori operativi
043000010601     C                   movel     '0'           flgGiro           1
043100010601     C                   movel     '0'           flgMulti          1
043200010601     C                   movel     '1'           flgStato          1
043300010615     C                   movel     '0'           flgOk             1
043400010601     C                   z-add     *zeros        cntNonEl         10 0
043500010601     C                   z-add     *zeros        depfil            3 0
043600010601     C                   z-add     *zeros        invfil            3 0
043700030407     C*
043800030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
043900030407     C                   if        %subst(vlrppt:1:1) = 'S'
044000030407     C                   seton                                        50
044100030407     C                   else
044200030407     C                   setoff                                       50
044300030407     C                   endif
044400010601     C*
044500010601     C                   ENDSR
044600010601     C***
044700010601
044800010601
044900010601
045000010330
045100010330
045200000613     C     *inzsr        BEGSR
045300990910     C*
045400990910     C     *entry        plist
045500990920     C                   parm                    tivlrds
045600990921     C                   parm      wrkesito      esito
045700000724     C                   parm                    prmlit
045800000710     C                   parm                    prmfir
045900010330     C*
046000010330     C* CALCOLA LA DATA CORRENTE
046100010330     C                   time                    wn14             14 0
046200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
046300010330     C                   z-add     wn8           g08dat
046400010330     C                   z-add     *zeros        g08inv
046500010330     C                   movel     '0'           g08err
046600010330     C                   call      'XSRDA8'
046700010330     C                   parm                    wlbda8
046800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
046900000613     C*
047000000613     C                   ENDSR
047100000613     C***
