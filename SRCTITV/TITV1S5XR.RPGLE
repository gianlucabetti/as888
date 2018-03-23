000100061106      /TITLE Upload via Internet: traduzione in EDIVAXWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500061106     FEDIVAXwr  O    E             DISK    usropn
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
002000040524     D fnvaxaar        ds                  inz
002100040524     D  a_vaxatb               1      1
002200050114     D  a_vaxccm               2      8
002300050114     D  a_vaxlnp               9     11
002400050114     D  a_vaxaas              12     15
002500050114     D  a_vaxnrs              16     17
002600050114     D  a_vaxnsp              18     24
002700050114     D  a_vaxtmd              25     28
002800050114     D  a_vaxprr              29     32
002900050114     D  a_vaxtrk              33     34
003000050114     D  a_vaxdtt              35    534
003100010330
003200010330
003300010330
003400000913     C                   reset                   rrnum
003500990921     C                   reset                   esito
003600990921     C                   reset                   wrkesito
003700010601     C*
003800010601     C                   exsr      opeini
003900040524     C                   exsr      rwvax
004000010601     C*
004100010601     C                   seton                                        lr
004200010601
004300010601
004400010601
004500010601
004600010601     C*--------------------------------------------------------
004700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004800010601     C*--------------------------------------------------------
004900010601     C     PREELA        BEGSR
005000010601     C*
005100010601     C* SE OCCORRE SPEDIRE IN FILIALE
005200010601     C                   if        invfil <> *zeros and
005300010601     C                             flgGiro = '0'
005400010601     C*
005500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005600010601     C                   eval      flgGiro = '1'
005700000724     C*
005800010601     C                   endif
005900010601     C*
006000010601     C                   ENDSR
006100010601     C***
006200010601
006300010601
006400010601
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007100010601     C*--------------------------------------------------------
007200010601     C     ENDELA        BEGSR
007300990915     C*
007400000616     C*
007500010601     C                   ENDSR
007600010601     C***
007700000613
007800010330
007900010601
008000010601
008100010601
008200010330     C*--------------------------------------------------------
008300061106     C* RWVAX   LEGGE TIVIN00R E SCRIVE EDIVAXWF              *
008400010330     C*--------------------------------------------------------
008500040524     C     RWVAX         BEGSR
008600061106     C*
008700061106     C                   clear                   §CTDOK
008800061106     C                   clear                   §CTDNO
008900091110     C*
009000010330     C                   if        not %open(tivin00r)
009100010330     C                   open      tivin00r
009200010330     C                   endif
009300061106     C                   if        not %open(edivaxwr)
009400061106     C                   open      edivaxwr
009500010330     C                   endif
009600010330     C*
009700010330     C                   DO        *HIVAL
009800010330     C*
009900010330     C                   READ      tivin00r                               70
010000010618     C*
010100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010200010618     C                   if        vinflg = '1'
010300010618     C                   eval      flgOk = '1'
010400010618     C                   endif
010500010618     C*
010600010330     C                   if        vindta > *blanks
010700010330     C                   add       1             rrnum
010800010330     C*
010900010601     C                   if        *in70 = *off and
011000010330     C                             (vinflg = *blanks
011100010330     C                              or vinflg = '0'
011200010330     C                              or vinflg = '2')
011300010711     C*
011400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011500010711     C                   if        vinflg = *blanks or vinflg = '0'
011600010711     C                   clear                   vinmsg
011700010711     C                   endif
011800010711     C*
011900040524     C                   eval      fnvaxaar = vindta
012000010601     C*
012100010330     C                   exsr      inzvar
012200010330     C                   exsr      defcam
012300040524     C                   exsr      impvax
012400010601     C*
012500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012600040524     C                   eval      depfil = VAXLNP
012700010601     C                   exsr      repfil
012800021010     C                   if        depfil = invfil
012900021014     C                   if        vlrpoi = 999
013000040524     C                   MOVE(P)   invfil        VAXFGS
013100021014     C                   else
013200040524     C                   MOVE(P)   vlrpoi        VAXFGS
013300021014     C                   endif
013400010601     C*
013500010601     C                   exsr      PREELA
013600061106     C*
013700061106     C* VALORIZZO CAMPI RELATIVI AL "CMR"
013800110204     C                   EVAL      VAXCMR = %char(vaxCCM)
013900061106     C                   EVAL      VAXCNT = 1
014000010601     C*
014100010604     C* Ebbene...
014200010604     C*
014300061106     C  N31              ADD       1             §CTDOK            7 0
014400061106     C   32
014500061106     COR 31              ADD       1             §CTDNO            7 0
014600061106     C  N31              WRITE     EDIVAX00
014700010604     C*
014800010604     C                   if        *in31 = *off and
014900010604     C                             *in32 = *off
015000010604     C                   eval      vinflg = '1'
015100010604     C                   else
015200010604     C                   eval      vinflg = '2'
015300010604     C                   endif
015400010604     C                   endif
015500010601     C*
015600010604     C                   endif
015700010604     C*
015800010330     C                   else
015900010330     C                   eval      vinflg = '1'
016000010330     C                   endif
016100010601     C*
016200010601     C  N70              update    tivin000
016300010330     C*
016400010330     C  N70              ENDdo
016500010601     C
016600010601     C*
016700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016800010601     C                   if        cntNonEl = *zeros or
016900010601     C                             flgMulti = '0'
017000010330     C* Se non ci sono record con errori ...
017100061106     C                   if        §ctdno = 0 and
017200010601     C                             flgStato <> '2'
017300010330     C* ... restituisco esito OK.
017400010330     C                   eval      wrkesito = '0'
017500010330     C                   else
017600061106     C                   if        §ctdok > 0
017700010330     C                   eval      wrkesito = '1'
017800010330     C                   else
017900010615     C                   if        flgOk = '0'
018000010615     C                   eval      wrkesito = '2'
018100010615     C                   else
018200010615     C                   eval      wrkesito = '6'
018300010615     C                   endif
018400010330     C                   endif
018500010330     C                   endif
018600010601     C                   else
018700010601     C                   eval      wrkesito = '9'
018800010601     C                   endif
018900010330     C*
019000010330     C                   if        %open(tivin00r)
019100010330     C                   close     tivin00r
019200010330     C                   endif
019300061106     C                   if        %open(edivaxwr)
019400061106     C                   close     edivaxwr
019500010330     C                   endif
019600010601     C*
019700010601     C                   if        vlrpoi <> 999
019800010601     C                   eval      invfil = vlrpoi
019900010601     C                   endif
020000010330     C*
020100061106     C                   if        §ctdok > 0
020200010601     C                             and invfil > *zeros
020300010330     C                   exsr      invio
020400010330     C                   endif
020500010601     C*
020600010612     C                   if        flgGiro = '1'
020700010601     C                   exsr      endela
020800010612     C                   endif
020900010330     C*
021000010330     C                   ENDSR
021100010330     C***
021200010330
021300010601
021400010601
021500010601
021600010601
021700010330     C*----------------------------------------------------*
021800010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021900010330     C*----------------------------------------------------*
022000010330     C     INZVAR        BEGSR
022100010330     C*
022200010330     C*
022300010330     C                   ENDSR
022400010330     C*----------------------------------------------------*
022500010330     C*  IMPOSTAZIONE CAMPI COSTANTI
022600010330     C*----------------------------------------------------*
022700010330     C     DEFCAM        BEGSR
022800010330     C*
022900061106     C                   CLEAR                   EDIVAX00
023000080606     C*
023100091110     C                   IF        %subst(vlrppt:8:3) <> *blanks
023200091110     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023300091110     C                   EXSR      CHKNUM
023400091110     C                   IF        PiInt=*on
023500091110     C                   Z-ADD     PiVal         VAXLNP
023600091110     C                   ENDIF
023700091110     C                   ENDIF
023800010330     C*
023900010330     C                   ENDSR
024000010330     C*----------------------------------------------------*
024100061106     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAX)
024200010330     C*----------------------------------------------------*
024300040524     C     IMPVAX        BEGSR
024400010330     C*
024500010604     C                   SETOFF                                       3132
024600010612     C*
024700010612     C                   SETOFF                                       303132
024800010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
024900040524     C                   TESTN                   a_vaxccm             30
025000040524     C  N30              TESTN                   a_vaxccm               30
025100040524     C  N30              TESTN                   a_vaxccm                 30
025200040524     C                   MOVE      a_vaxccm      W001A             1
025300010612     C     *IN30         IFEQ      *OFF
025400010612     C     W001A         ORLT      '0'
025500010612     C                   SETON                                        31
025600040524     C                   Z-ADD     *zeros        VAXCCM
025700040524     C                   EVAL      vinmsg = 'VAXCCM'
025800010612     C                   ELSE
025900040524     C                   MOVE      a_vaxccm      VAXCCM
026000040524     C     VAXCCM        IFLE      *ZEROS
026100010612     C                   SETON                                        31
026200040524     C                   Z-ADD     *zeros        VAXCCM
026300040524     C                   EVAL      vinmsg = 'VAXCCM'
026400010612     C                   ENDIF
026500010612     C                   ENDIF
026600010612     C*
026700040524     C                   TESTN                   a_vaxlnp             30
026800040524     C  N30              TESTN                   a_vaxlnp               30
026900040524     C  N30              TESTN                   a_vaxlnp                 30
027000040524     C                   MOVE      a_vaxlnp      W001A             1
027100010612     C     *IN30         IFEQ      *OFF
027200010612     C     W001A         ORLT      '0'
027300010612     C                   SETON                                        31
027400040524     C                   Z-ADD     *zeros        VAXLNP
027500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
027600040524     C                             + ' ' + 'VAXLNP'
027700010612     C                   ELSE
027800040524     C                   MOVE      a_vaxlnp      VAXLNP
027900040524     C     VAXLNP        IFLE      *ZEROS
028000010612     C                   SETON                                        31
028100040524     C                   Z-ADD     *zeros        VAXLNP
028200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
028300040524     C                             + ' ' + 'VAXLNP'
028400010612     C                   ENDIF
028500010612     C                   ENDIF
028600010612     C*
028700040524     C                   TESTN                   a_vaxaas             30
028800040524     C  N30              TESTN                   a_vaxaas               30
028900040524     C  N30              TESTN                   a_vaxaas                 30
029000040524     C                   MOVE      a_vaxaas      W001A             1
029100010612     C     *IN30         IFEQ      *OFF
029200010612     C     W001A         ORLT      '0'
029300010612     C                   SETON                                        31
029400040524     C                   MOVEL(P)  datcor        VAXAAS
029500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
029600040524     C                             + ' ' + 'VAXAAS'
029700010612     C                   ELSE
029800040524     C                   MOVE      a_vaxaas      VAXAAS
029900010612     C                   ENDIF
030000010612     C*
030100040524     C                   TESTN                   a_vaxnrs             30
030200040524     C  N30              TESTN                   a_vaxnrs               30
030300040524     C  N30              TESTN                   a_vaxnrs                 30
030400040524     C                   MOVE      a_vaxnrs      W001A             1
030500010612     C     *IN30         IFEQ      *OFF
030600010612     C     W001A         ORLT      '0'
030700010612     C     W001A         ANDNE     *BLANK
030800010612     C                   SETON                                        31
030900040524     C                   Z-ADD     *zeros        VAXNRS
031000010612     C                   EVAL      vinmsg = %trimr(vinmsg)
031100040524     C                             + ' ' + 'VAXNRS'
031200010612     C                   ELSE
031300040524     C                   MOVE      a_vaxnrs      VAXNRS
031400010612     C                   ENDIF
031500010612     C*
031600040524     C                   TESTN                   a_vaxnsp             30
031700040524     C  N30              TESTN                   a_vaxnsp               30
031800040524     C  N30              TESTN                   a_vaxnsp                 30
031900040524     C                   MOVE      a_vaxnsp      W001A             1
032000010612     C     *IN30         IFEQ      *OFF
032100010612     C     W001A         ORLT      '0'
032200010612     C                   SETON                                        31
032300040524     C                   Z-ADD     *zeros        VAXNSP
032400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
032500040524     C                             + ' ' + 'VAXNSP'
032600010612     C                   ELSE
032700040524     C                   MOVE      a_vaxnsp      VAXNSP
032800010612     C                   ENDIF
032900040524     C*
033000040524     C                   TESTN                   a_vaxprr             30
033100040524     C  N30              TESTN                   a_vaxprr               30
033200040524     C  N30              TESTN                   a_vaxprr                 30
033300040524     C                   MOVE      a_vaxprr      W001A             1
033400040524     C     *IN30         IFEQ      *OFF
033500040524     C     W001A         ORLT      '0'
033600040524     C                   SETON                                        31
033700040524     C                   Z-ADD     *zeros        VAXPRR
033800040524     C                   EVAL      vinmsg = %trimr(vinmsg)
033900040524     C                             + ' ' + 'VAXPRR'
034000040524     C                   ELSE
034100040524     C                   MOVE      a_vaxprr      VAXPRR
034200040524     C                   ENDIF
034300010612     C*
034400010612     C* ALFABETICI
034500040524     C                   MOVE      a_vaxatb      VAXATB
034600040524     C                   MOVE      a_vaxtmd      VAXTMD
034700040524     C                   MOVE      a_vaxtrk      VAXTRK
034800040524     C                   MOVE      a_vaxdtt      VAXDTT
034900010330     C*
035000010330     C                   ENDSR
035100010612     C*----------------------------------------------------*
035200990920
035300010601
035400010601
035500010601
035600010601
035700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
035800010601     C     repfil        BEGSR
035900010601     C*
036000010601     C                   if        invfil = *zeros and
036100010601     C                             depfil > *zeros and
036200010613     C                             (vinflg = *blanks or
036300010613     C                              vinflg = *zeros)
036400010601     C
036500010601     C                   eval      invfil = depfil
036600010601     C                   endif
036700010601     C*
036800010601     C                   if        depfil <> invfil and
036900010601     C                             invfil > *zeros
037000010601     C                   eval      flgMulti = '1'
037100010601     C                   if        vinflg = *blanks
037200010601     C                   add       1             cntNonEl
037300010601     C                   endif
037400010601     C                   endif
037500010601     C*
037600010601     C                   if        vinflg = '2'
037700010601     C                   eval      flgStato = '2'
037800010601     C                   endif
037900010601     C*
038000010601     C                   ENDSR
038100010601     C***
038200010601
038300010601
038400010601
038500010330
038600010330
038700010330
038800990920      /TITLE Invio dei dati al punto operativo.
038900000613     C     invio         BEGSR
039000990920     C*
039100990920     C                   reset                   dscmz
039200010601     C                   move      invfil        cmzdst
039300990920     C                   eval      cmzfld = vlrfou
039400990920     C                   eval      cmzmbd = vlrhdl
039500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
039600000710     C                   if        prmfir = *blanks
039700061106     C                   eval      cmzfla = 'EDIVAX0F'
039800061106     C                   eval      cmzmba = 'EDIVAX0F'
039900000710     C                   else
040000000710     C                   eval      cmzfla = prmfir
040100000710     C                   eval      cmzmba = prmfir
040200000710     C                   endif
040300990920     C                   eval      cmznrr = *zeros
040400061106     C                   move      §ctdok        cmznrr
040500021003     C                   eval      cmzlba = vlrfl1
040600990920     C                   call(e)   'TIS711C'
040700990920     C                   parm                    dscmz
040800990921     C                   parm      *blanks       esito
040900990920     C                   if        %error
041000990920     C                             or cmzerr = '1'
041100990921     C                             or esito  = '1'
041200000710     C                   eval      wrkesito = '3'
041300990920     C                   endif
041400990920     C*
041500000613     C                   ENDSR
041600990910
041700010601
041800010601
041900010601
042000010601
042100010601      /TITLE Invio dei dati al punto operativo.
042200010601     C     opeini        BEGSR
042300010601     C*
042400010601     C* Inizializzo flag e contatori operativi
042500010601     C                   movel     '0'           flgGiro           1
042600010601     C                   movel     '0'           flgMulti          1
042700010601     C                   movel     '1'           flgStato          1
042800010615     C                   movel     '0'           flgOk             1
042900010601     C                   z-add     *zeros        cntNonEl         10 0
043000010601     C                   z-add     *zeros        depfil            3 0
043100010601     C                   z-add     *zeros        invfil            3 0
043200030407     C*
043300030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
043400030407     C                   if        %subst(vlrppt:1:1) = 'S'
043500030407     C                   seton                                        50
043600030407     C                   else
043700030407     C                   setoff                                       50
043800030407     C                   endif
043900010601     C*
044000010601     C                   ENDSR
044100010601     C***
044200080606
044300080606
044400080606
044500080606     C*----------------------------------------------------*
044600080606     C*  CONTROLLO NUMERICITA' CAMPI
044700080606     C*----------------------------------------------------*
044800080606     C     CHKNUM        BEGSR
044900080606     C*
045000080606     C                   IF        PiDecChr = *blanks
045100080606     C                   EVAL      PiDecChr = ','
045200080606     C                   ENDIF
045300080606     C*
045400080606     C                   CALL(e)   'ISNUMERIC'
045500080606     C                   PARM                    PiStr            30
045600080606     C                   PARM                    PiDecChr          1
045700080606     C                   PARM      *ZEROS        PiVal            30 9
045800080606     C                   PARM      '0'           PiInt             1
045900080606     C                   PARM      '0'           PiNum             1
046000080606     C                   IF        %error
046100080606     C                   EVAL      PiNum=*off
046200080606     C                   ENDIF
046300080606     C*
046400080606     C                   ENDSR
046500080606     C***
046600010601
046700010601
046800010601
046900010330
047000010330
047100000613     C     *inzsr        BEGSR
047200990910     C*
047300990910     C     *entry        plist
047400990920     C                   parm                    tivlrds
047500990921     C                   parm      wrkesito      esito
047600000724     C                   parm                    prmlit
047700000710     C                   parm                    prmfir
047800010330     C*
047900010330     C* CALCOLA LA DATA CORRENTE
048000091110     C                   z-add     *zeros        datcor            8 0
048100091110     C                   eval      datcor = %dec(%date() : *ISO)
048200000613     C*
048300000613     C                   ENDSR
048400000613     C***
