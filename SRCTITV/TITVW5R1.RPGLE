000100070201      /TITLE Upload via Internet: traduzione in EDIVAXWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500070201     FEDIVAXwr  O    E             DISK    usropn
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
002900070413     D  a_vaxccm               2      8  0
003000070326     D  a_vaxldv              16     26
003100070202     D  a_vaxnsp              20     26
003200070201     D  a_vaxprr              29     32
003300070201     D  a_vaxtrk              33     34
003400070201     D  a_vaxdtt              35    534
003500010330
003600010330
003700010330
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100010601     C*
004200010601     C                   exsr      opeini
004300040524     C                   exsr      rwvax
004400010601     C*
004500010601     C                   seton                                        lr
004600010601
004700010601
004800010601
004900010601
005000010601     C*--------------------------------------------------------
005100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005200010601     C*--------------------------------------------------------
005300010601     C     PREELA        BEGSR
005400010601     C*
005500010601     C* SE OCCORRE SPEDIRE IN FILIALE
005600010601     C                   if        invfil <> *zeros and
005700010601     C                             flgGiro = '0'
005800010601     C*
005900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006000010601     C                   eval      flgGiro = '1'
006100010601     C*
006200010601     C                   endif
006300010601     C*
006400010601     C                   ENDSR
006500010601     C***
006600010601
006700010601
006800010601
006900010601
007000010601
007100010601
007200010601
007300010601     C*--------------------------------------------------------
007400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007500010601     C*--------------------------------------------------------
007600010601     C     ENDELA        BEGSR
007700000616     C*
007800010601     C                   ENDSR
007900010601     C***
008000000613
008100010330
008200010601
008300010601
008400010601
008500010330     C*--------------------------------------------------------
008600070201     C* RWVAX   LEGGE TIVIN00R E SCRIVE EDIVAXWF              *
008700010330     C*--------------------------------------------------------
008800040524     C     RWVAX         BEGSR
008900010330     C*
009000010330     C                   if        not %open(tivin00r)
009100010330     C                   open      tivin00r
009200010330     C                   endif
009300070201     C                   if        not %open(edivaxwr)
009400070201     C                   open      edivaxwr
009500010330     C                   endif
009600010330     C*
009700010604     C                   clear                   §CTROK
009800010604     C                   clear                   §CTRMO
009900010604     C                   clear                   §CTRNO
010000010330     C*
010100010330     C                   DO        *HIVAL
010200010330     C*
010300010330     C                   READ      tivin00r                               70
010400010618     C*
010500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010600010618     C                   if        vinflg = '1'
010700010618     C                   eval      flgOk = '1'
010800010618     C                   endif
010900010618     C*
011000010330     C                   if        vindta > *blanks
011100010330     C                   add       1             rrnum
011200010330     C*
011300010601     C                   if        *in70 = *off and
011400010330     C                             (vinflg = *blanks
011500010330     C                              or vinflg = '0'
011600010330     C                              or vinflg = '2')
011700010711     C*
011800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011900010711     C                   if        vinflg = *blanks or vinflg = '0'
012000010711     C                   clear                   vinmsg
012100010711     C                   endif
012200010711     C*
012300040524     C                   eval      fnvaxaar = vindta
012400010601     C*
012500010330     C                   exsr      inzvar
012600010330     C                   exsr      defcam
012700040524     C                   exsr      impvax
012800070115     C*
012900070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013000070115     C                   eval      depfil = VAXLNP
013100010601     C*
013200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013300010601     C                   exsr      repfil
013400021010     C                   if        depfil = invfil
013500021014     C                   if        vlrpoi = 999
013600040524     C                   MOVE(P)   invfil        VAXFGS
013700021014     C                   else
013800040524     C                   MOVE(P)   vlrpoi        VAXFGS
013900021014     C                   endif
014000010601     C*
014100010601     C                   exsr      PREELA
014200070202     C*
014300070202     C* VALORIZZO CAMPI RELATIVI AL "CMR"
014400070417     C                   EVAL      VAXCMR = %trim(a_vaxldv)
014500070202     C                   EVAL      VAXCNT = 1
014600010601     C*
014700010604     C* Ebbene...
014800010604     C*
014900010604     C  N31              ADD       1             §CTROK            7 0
015000010604     C   32              ADD       1             §CTRMO            7 0
015100010604     C   31              ADD       1             §CTRNO            7 0
015200070201     C  N31              WRITE     EDIVAX00
015300010604     C*
015400010604     C                   if        *in31 = *off and
015500010604     C                             *in32 = *off
015600010604     C                   eval      vinflg = '1'
015700010604     C                   else
015800010604     C                   eval      vinflg = '2'
015900010604     C                   endif
016000010604     C                   endif
016100010601     C*
016200010604     C                   endif
016300010604     C*
016400010330     C                   else
016500010330     C                   eval      vinflg = '1'
016600010330     C                   endif
016700010601     C*
016800010601     C  N70              update    tivin000
016900010330     C*
017000010330     C  N70              ENDdo
017100010601     C
017200010601     C*
017300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017400010601     C                   if        cntNonEl = *zeros or
017500010601     C                             flgMulti = '0'
017600010330     C* Se non ci sono record con errori ...
017700010601     C                   if        §ctrno = 0 and
017800010604     C                             §ctrmo = 0 and
017900010601     C                             flgStato <> '2'
018000010330     C* ... restituisco esito OK.
018100010330     C                   eval      wrkesito = '0'
018200010330     C                   else
018300010330     C                   if        §ctrok > 0
018400010330     C                   eval      wrkesito = '1'
018500010330     C                   else
018600010615     C                   if        flgOk = '0'
018700010615     C                   eval      wrkesito = '2'
018800010615     C                   else
018900010615     C                   eval      wrkesito = '6'
019000010615     C                   endif
019100010330     C                   endif
019200010330     C                   endif
019300010601     C                   else
019400010601     C                   eval      wrkesito = '9'
019500010601     C                   endif
019600010330     C*
019700010330     C                   if        %open(tivin00r)
019800010330     C                   close     tivin00r
019900010330     C                   endif
020000070201     C                   if        %open(edivaxwr)
020100070201     C                   close     edivaxwr
020200010330     C                   endif
020300010601     C*
020400010601     C                   if        vlrpoi <> 999
020500010601     C                   eval      invfil = vlrpoi
020600010601     C                   endif
020700010330     C*
020800010330     C                   if        §ctrok > 0
020900010601     C                             and invfil > *zeros
021000010330     C                   exsr      invio
021100010330     C                   endif
021200010601     C*
021300010612     C                   if        flgGiro = '1'
021400010601     C                   exsr      endela
021500010612     C                   endif
021600010330     C*
021700010330     C                   ENDSR
021800010330     C***
021900010330
022000010601
022100010601
022200010601
022300010601
022400010330     C*----------------------------------------------------*
022500010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022600010330     C*----------------------------------------------------*
022700010330     C     INZVAR        BEGSR
022800010330     C*
022900010330     C*
023000010330     C                   ENDSR
023100010330     C*----------------------------------------------------*
023200010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023300010330     C*----------------------------------------------------*
023400010330     C     DEFCAM        BEGSR
023500010330     C*
023600070201     C                   CLEAR                   EDIVAX00
023700070201     C*
023800070201     C* Imposto i valori d default
023900070413     C                   EVAL      VAXCCM = a_vaxccm
024000070201     C                   EVAL      VAXLNP = 062
024100090311     C                   IF        VAXCCM = 0622258
024200090311     C                   EVAL      VAXTMD = 'ITR1'
024300090311     C                   ENDIF
024400070413     C                   IF        VAXCCM = 0621257
024500070413     C                   EVAL      VAXTMD = 'ITR1'
024600070413     C                   ENDIF
024700070413     C                   IF        VAXCCM = 0620804
024800070413     C                   EVAL      VAXTMD = 'ITR2'
024900070413     C                   ENDIF
025000080716     C                   IF        VAXCCM = 0621602
025100081121     C                   EVAL      VAXTMD = 'PLUS'
025200080716     C                   ENDIF
025300090326     C                   IF        VAXCCM = 0622264
025400090326     C                   EVAL      VAXTMD = 'PLUS'
025500090326     C                   ENDIF
025600081121     C                   IF        VAXCCM = 0621208
025700081121     C                   EVAL      VAXTMD = 'MALO'
025800081121     C                   ENDIF
025900110216     C                   IF        VAXCCM = 0623090
026000110217     C                   EVAL      VAXTMD = 'ITR4'
026100110216     C                   ENDIF
026200070202     C                   MOVEL     datcor        VAXAAS
026300010330     C*
026400010330     C                   ENDSR
026500010330     C*----------------------------------------------------*
026600070201     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAX)
026700010330     C*----------------------------------------------------*
026800040524     C     IMPVAX        BEGSR
026900010330     C*
027000010604     C                   SETOFF                                       3132
027100010612     C*
027200010612     C                   SETOFF                                       303132
027300010612     C*
027400040524     C                   TESTN                   a_vaxnsp             30
027500040524     C  N30              TESTN                   a_vaxnsp               30
027600040524     C  N30              TESTN                   a_vaxnsp                 30
027700040524     C                   MOVE      a_vaxnsp      W001A             1
027800010612     C     *IN30         IFEQ      *OFF
027900010612     C     W001A         ORLT      '0'
028000010612     C                   SETON                                        31
028100040524     C                   Z-ADD     *zeros        VAXNSP
028200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
028300040524     C                             + ' ' + 'VAXNSP'
028400010612     C                   ELSE
028500040524     C                   MOVE      a_vaxnsp      VAXNSP
028600010612     C                   ENDIF
028700040524     C*
028800040524     C                   TESTN                   a_vaxprr             30
028900040524     C  N30              TESTN                   a_vaxprr               30
029000040524     C  N30              TESTN                   a_vaxprr                 30
029100040524     C                   MOVE      a_vaxprr      W001A             1
029200040524     C     *IN30         IFEQ      *OFF
029300040524     C     W001A         ORLT      '0'
029400040524     C                   SETON                                        31
029500040524     C                   Z-ADD     *zeros        VAXPRR
029600040524     C                   EVAL      vinmsg = %trimr(vinmsg)
029700040524     C                             + ' ' + 'VAXPRR'
029800040524     C                   ELSE
029900040524     C                   MOVE      a_vaxprr      VAXPRR
030000040524     C                   ENDIF
030100010612     C*
030200010612     C* ALFABETICI
030300040524     C                   MOVE      a_vaxtrk      VAXTRK
030400040524     C                   MOVE      a_vaxdtt      VAXDTT
030500010330     C*
030600010330     C                   ENDSR
030700010612     C*----------------------------------------------------*
030800990920
030900010601
031000010601
031100010601
031200010601
031300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
031400010601     C     repfil        BEGSR
031500010601     C*
031600010601     C                   if        invfil = *zeros and
031700010601     C                             depfil > *zeros and
031800010613     C                             (vinflg = *blanks or
031900010613     C                              vinflg = *zeros)
032000010601     C
032100010601     C                   eval      invfil = depfil
032200010601     C                   endif
032300010601     C*
032400010601     C                   if        depfil <> invfil and
032500010601     C                             invfil > *zeros
032600010601     C                   eval      flgMulti = '1'
032700010601     C                   if        vinflg = *blanks
032800010601     C                   add       1             cntNonEl
032900010601     C                   endif
033000010601     C                   endif
033100010601     C*
033200010601     C                   if        vinflg = '2'
033300010601     C                   eval      flgStato = '2'
033400010601     C                   endif
033500010601     C*
033600010601     C                   ENDSR
033700010601     C***
033800010601
033900010601
034000010601
034100010330
034200010330
034300010330
034400990920      /TITLE Invio dei dati al punto operativo.
034500000613     C     invio         BEGSR
034600990920     C*
034700990920     C                   reset                   dscmz
034800010601     C                   move      invfil        cmzdst
034900990920     C                   eval      cmzfld = vlrfou
035000990920     C                   eval      cmzmbd = vlrhdl
035100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
035200000710     C                   if        prmfir = *blanks
035300070201     C                   eval      cmzfla = 'EDIVAX0F'
035400070201     C                   eval      cmzmba = 'EDIVAX0F'
035500000710     C                   else
035600000710     C                   eval      cmzfla = prmfir
035700000710     C                   eval      cmzmba = prmfir
035800000710     C                   endif
035900990920     C                   eval      cmznrr = *zeros
036000990920     C                   move      §ctrok        cmznrr
036100021003     C                   eval      cmzlba = vlrfl1
036200990920     C                   call(e)   'TIS711C'
036300990920     C                   parm                    dscmz
036400990921     C                   parm      *blanks       esito
036500990920     C                   if        %error
036600990920     C                             or cmzerr = '1'
036700990921     C                             or esito  = '1'
036800000710     C                   eval      wrkesito = '3'
036900990920     C                   endif
037000990920     C*
037100000613     C                   ENDSR
037200990910
037300010601
037400010601
037500010601
037600010601
037700010601      /TITLE Invio dei dati al punto operativo.
037800010601     C     opeini        BEGSR
037900010601     C*
038000010601     C* Inizializzo flag e contatori operativi
038100010601     C                   movel     '0'           flgGiro           1
038200010601     C                   movel     '0'           flgMulti          1
038300010601     C                   movel     '1'           flgStato          1
038400010615     C                   movel     '0'           flgOk             1
038500010601     C                   z-add     *zeros        cntNonEl         10 0
038600010601     C                   z-add     *zeros        depfil            3 0
038700010601     C                   z-add     *zeros        invfil            3 0
038800030407     C*
038900030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
039000030407     C                   if        %subst(vlrppt:1:1) = 'S'
039100030407     C                   seton                                        50
039200030407     C                   else
039300030407     C                   setoff                                       50
039400030407     C                   endif
039500010601     C*
039600010601     C                   ENDSR
039700010601     C***
039800010601
039900010601
040000010601
040100010330
040200010330
040300000613     C     *inzsr        BEGSR
040400990910     C*
040500990910     C     *entry        plist
040600990920     C                   parm                    tivlrds
040700990921     C                   parm      wrkesito      esito
040800000724     C                   parm                    prmlit
040900000710     C                   parm                    prmfir
041000010330     C*
041100010330     C* CALCOLA LA DATA CORRENTE
041200010330     C                   time                    wn14             14 0
041300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
041400010330     C                   z-add     wn8           g08dat
041500010330     C                   z-add     *zeros        g08inv
041600010330     C                   movel     '0'           g08err
041700010330     C                   call      'XSRDA8'
041800010330     C                   parm                    wlbda8
041900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
042000000613     C*
042100000613     C                   ENDSR
042200000613     C***
