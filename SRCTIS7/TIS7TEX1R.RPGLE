000100061106      /TITLE Upload via Internet: traduzione in EDIVAXWR.
000200130115     H dftactgrp(*no) actgrp(*caller)
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
001900061106     D wNomeFile       s             30    INZ(*blanks)
002000061106     D wCMR            s             35    INZ(*blanks)
002100061106     D wPosDaA         s              2    INZ(*blanks)
002200061106     D wPosDa          s              2  0 INZ(*zeros)
002300061106     D wLungA          s              2    INZ(*blanks)
002400061106     D wLung           s              2  0 INZ(*zeros)
002500010330     D*------------------
002600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002700010330     D*------------------
002800010330     D WLBDA8          DS                  INZ
002900010330     D  G08DAT                 1      8  0
003000010330     D  G08INV                 9     16  0
003100010330     D  G08ERR                17     17
003200010330     D  G08TGI                18     22  0
003300010612     D*
003400040524     D fnvaxaar        ds                  inz
003500040524     D  a_vaxatb               1      1
003600050114     D  a_vaxccm               2      8
003700050114     D  a_vaxlnp               9     11
003800050114     D  a_vaxaas              12     15
003900050114     D  a_vaxnrs              16     17
004000050114     D  a_vaxnsp              18     24
004100050114     D  a_vaxtmd              25     28
004200050114     D  a_vaxprr              29     32
004300050114     D  a_vaxtrk              33     34
004400050114     D  a_vaxdtt              35    534
004500010330
004600010330
004700010330
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100010601     C*
005200010601     C                   exsr      opeini
005300040524     C                   exsr      rwvax
005400010601     C*
005500010601     C                   seton                                        lr
005600010601
005700010601
005800010601
005900010601
006000010601     C*--------------------------------------------------------
006100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006200010601     C*--------------------------------------------------------
006300010601     C     PREELA        BEGSR
006400010601     C*
006500010601     C* SE OCCORRE SPEDIRE IN FILIALE
006600010601     C                   if        invfil <> *zeros and
006700010601     C                             flgGiro = '0'
006800010601     C*
006900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007000010601     C                   eval      flgGiro = '1'
007100010601     C*
007200010601     C                   endif
007300010601     C*
007400010601     C                   ENDSR
007500010601     C***
007600010601
007700010601
007800010601
007900010601
008000010601
008100010601
008200010601
008300010601     C*--------------------------------------------------------
008400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008500010601     C*--------------------------------------------------------
008600010601     C     ENDELA        BEGSR
008700000616     C*
008800010601     C                   ENDSR
008900010601     C***
009000000613
009100010330
009200010601
009300010601
009400010601
009500010330     C*--------------------------------------------------------
009600061106     C* RWVAX   LEGGE TIVIN00R E SCRIVE EDIVAXWF              *
009700010330     C*--------------------------------------------------------
009800040524     C     RWVAX         BEGSR
009900061106     C*
010000061106     C                   clear                   §CTDOK
010100061106     C                   clear                   §CTDNO
010200061106     C*
010300061106     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
010400061106     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
010500061106     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
010600061106     C                   if        *in60 = *on
010700010330     C                   if        not %open(tivin00r)
010800010330     C                   open      tivin00r
010900010330     C                   endif
011000061106     C                   if        not %open(edivaxwr)
011100061106     C                   open      edivaxwr
011200010330     C                   endif
011300010330     C*
011400010330     C                   DO        *HIVAL
011500010330     C*
011600010330     C                   READ      tivin00r                               70
011700010618     C*
011800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011900010618     C                   if        vinflg = '1'
012000010618     C                   eval      flgOk = '1'
012100010618     C                   endif
012200010618     C*
012300010330     C                   if        vindta > *blanks
012400010330     C                   add       1             rrnum
012500010330     C*
012600010601     C                   if        *in70 = *off and
012700010330     C                             (vinflg = *blanks
012800010330     C                              or vinflg = '0'
012900010330     C                              or vinflg = '2')
013000010711     C*
013100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013200010711     C                   if        vinflg = *blanks or vinflg = '0'
013300010711     C                   clear                   vinmsg
013400010711     C                   endif
013500010711     C*
013600040524     C                   eval      fnvaxaar = vindta
013700010601     C*
013800010330     C                   exsr      inzvar
013900010330     C                   exsr      defcam
014000040524     C                   exsr      impvax
014100010601     C*
014200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014300040524     C                   eval      depfil = VAXLNP
014400010601     C                   exsr      repfil
014500021010     C                   if        depfil = invfil
014600021014     C                   if        vlrpoi = 999
014700040524     C                   MOVE(P)   invfil        VAXFGS
014800021014     C                   else
014900040524     C                   MOVE(P)   vlrpoi        VAXFGS
015000021014     C                   endif
015100010601     C*
015200010601     C                   exsr      PREELA
015300061106     C*
015400061106     C* VALORIZZO CAMPI RELATIVI AL "CMR"
015500061106     C                   EVAL      VAXCMR = wCMR
015600061106     C                   EVAL      VAXCNT = 1
015700010601     C*
015800010604     C* Ebbene...
015900010604     C*
016000061106     C  N31              ADD       1             §CTDOK            7 0
016100061106     C   32
016200061106     COR 31              ADD       1             §CTDNO            7 0
016300061106     C  N31              WRITE     EDIVAX00
016400010604     C*
016500010604     C                   if        *in31 = *off and
016600010604     C                             *in32 = *off
016700010604     C                   eval      vinflg = '1'
016800010604     C                   else
016900010604     C                   eval      vinflg = '2'
017000010604     C                   endif
017100010604     C                   endif
017200010601     C*
017300010604     C                   endif
017400010604     C*
017500010330     C                   else
017600010330     C                   eval      vinflg = '1'
017700010330     C                   endif
017800010601     C*
017900010601     C  N70              update    tivin000
018000010330     C*
018100010330     C  N70              ENDdo
018200010601     C
018300010601     C*
018400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018500010601     C                   if        cntNonEl = *zeros or
018600010601     C                             flgMulti = '0'
018700010330     C* Se non ci sono record con errori ...
018800061106     C                   if        §ctdno = 0 and
018900010601     C                             flgStato <> '2'
019000010330     C* ... restituisco esito OK.
019100010330     C                   eval      wrkesito = '0'
019200010330     C                   else
019300061106     C                   if        §ctdok > 0
019400010330     C                   eval      wrkesito = '1'
019500010330     C                   else
019600010615     C                   if        flgOk = '0'
019700010615     C                   eval      wrkesito = '2'
019800010615     C                   else
019900010615     C                   eval      wrkesito = '6'
020000010615     C                   endif
020100010330     C                   endif
020200010330     C                   endif
020300010601     C                   else
020400010601     C                   eval      wrkesito = '9'
020500010601     C                   endif
020600061106     C*
020700061106 X  1C                   else
020800061106     C                   eval      wrkesito = '8'
020900061106     C                   eval      flgGiro = '1'
021000061106 E  1C                   endif
021100010330     C*
021200010330     C                   if        %open(tivin00r)
021300010330     C                   close     tivin00r
021400010330     C                   endif
021500061106     C                   if        %open(edivaxwr)
021600061106     C                   close     edivaxwr
021700010330     C                   endif
021800010601     C*
021900010601     C                   if        vlrpoi <> 999
022000010601     C                   eval      invfil = vlrpoi
022100010601     C                   endif
022200010330     C*
022300061106     C                   if        §ctdok > 0
022400010601     C                             and invfil > *zeros
022500010330     C                   exsr      invio
022600010330     C                   endif
022700010601     C*
022800010612     C                   if        flgGiro = '1'
022900010601     C                   exsr      endela
023000010612     C                   endif
023100010330     C*
023200010330     C                   ENDSR
023300010330     C***
023400010330
023500010601
023600010601
023700010601
023800010601
023900010330     C*----------------------------------------------------*
024000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024100010330     C*----------------------------------------------------*
024200010330     C     INZVAR        BEGSR
024300010330     C*
024400010330     C*
024500010330     C                   ENDSR
024600010330     C*----------------------------------------------------*
024700010330     C*  IMPOSTAZIONE CAMPI COSTANTI
024800010330     C*----------------------------------------------------*
024900010330     C     DEFCAM        BEGSR
025000010330     C*
025100061106     C                   CLEAR                   EDIVAX00
025200080606     C*
025300080606     C                   IF        %trim(%subst(vlrppt:16:7)) <> *blanks
025400080606     C                   EVAL      PiStr=%trim(%subst(vlrppt:16:7))
025500080606     C                   EXSR      CHKNUM
025600080606     C                   IF        PiInt=*on
025700080606     C                   Z-ADD     PiVal         VAXCCM
025800080606     C                   ENDIF
025900080606     C                   ENDIF
026000080606     C                   IF        %trim(%subst(vlrppt:23:3)) <> *blanks
026100080606     C                   EVAL      PiStr=%trim(%subst(vlrppt:23:3))
026200080606     C                   EXSR      CHKNUM
026300080606     C                   IF        PiInt=*on
026400080606     C                   Z-ADD     PiVal         VAXLNP
026500080606     C                   ENDIF
026600080606     C                   ENDIF
026700010330     C*
026800010330     C                   ENDSR
026900010330     C*----------------------------------------------------*
027000061106     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAX)
027100010330     C*----------------------------------------------------*
027200040524     C     IMPVAX        BEGSR
027300010330     C*
027400010604     C                   SETOFF                                       3132
027500010612     C*
027600010612     C                   SETOFF                                       303132
027700010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
027800040524     C                   TESTN                   a_vaxccm             30
027900040524     C  N30              TESTN                   a_vaxccm               30
028000040524     C  N30              TESTN                   a_vaxccm                 30
028100040524     C                   MOVE      a_vaxccm      W001A             1
028200010612     C     *IN30         IFEQ      *OFF
028300010612     C     W001A         ORLT      '0'
028400010612     C                   SETON                                        31
028500040524     C                   Z-ADD     *zeros        VAXCCM
028600040524     C                   EVAL      vinmsg = 'VAXCCM'
028700010612     C                   ELSE
028800040524     C                   MOVE      a_vaxccm      VAXCCM
028900040524     C     VAXCCM        IFLE      *ZEROS
029000010612     C                   SETON                                        31
029100040524     C                   Z-ADD     *zeros        VAXCCM
029200040524     C                   EVAL      vinmsg = 'VAXCCM'
029300010612     C                   ENDIF
029400010612     C                   ENDIF
029500010612     C*
029600040524     C                   TESTN                   a_vaxlnp             30
029700040524     C  N30              TESTN                   a_vaxlnp               30
029800040524     C  N30              TESTN                   a_vaxlnp                 30
029900040524     C                   MOVE      a_vaxlnp      W001A             1
030000010612     C     *IN30         IFEQ      *OFF
030100010612     C     W001A         ORLT      '0'
030200010612     C                   SETON                                        31
030300040524     C                   Z-ADD     *zeros        VAXLNP
030400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
030500040524     C                             + ' ' + 'VAXLNP'
030600010612     C                   ELSE
030700040524     C                   MOVE      a_vaxlnp      VAXLNP
030800040524     C     VAXLNP        IFLE      *ZEROS
030900010612     C                   SETON                                        31
031000040524     C                   Z-ADD     *zeros        VAXLNP
031100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
031200040524     C                             + ' ' + 'VAXLNP'
031300010612     C                   ENDIF
031400010612     C                   ENDIF
031500010612     C*
031600040524     C                   TESTN                   a_vaxaas             30
031700040524     C  N30              TESTN                   a_vaxaas               30
031800040524     C  N30              TESTN                   a_vaxaas                 30
031900040524     C                   MOVE      a_vaxaas      W001A             1
032000010612     C     *IN30         IFEQ      *OFF
032100010612     C     W001A         ORLT      '0'
032200010612     C                   SETON                                        31
032300040524     C                   MOVEL(P)  datcor        VAXAAS
032400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
032500040524     C                             + ' ' + 'VAXAAS'
032600010612     C                   ELSE
032700040524     C                   MOVE      a_vaxaas      VAXAAS
032800010612     C                   ENDIF
032900010612     C*
033000040524     C                   TESTN                   a_vaxnrs             30
033100040524     C  N30              TESTN                   a_vaxnrs               30
033200040524     C  N30              TESTN                   a_vaxnrs                 30
033300040524     C                   MOVE      a_vaxnrs      W001A             1
033400010612     C     *IN30         IFEQ      *OFF
033500010612     C     W001A         ORLT      '0'
033600010612     C     W001A         ANDNE     *BLANK
033700010612     C                   SETON                                        31
033800040524     C                   Z-ADD     *zeros        VAXNRS
033900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
034000040524     C                             + ' ' + 'VAXNRS'
034100010612     C                   ELSE
034200040524     C                   MOVE      a_vaxnrs      VAXNRS
034300010612     C                   ENDIF
034400010612     C*
034500040524     C                   TESTN                   a_vaxnsp             30
034600040524     C  N30              TESTN                   a_vaxnsp               30
034700040524     C  N30              TESTN                   a_vaxnsp                 30
034800040524     C                   MOVE      a_vaxnsp      W001A             1
034900010612     C     *IN30         IFEQ      *OFF
035000010612     C     W001A         ORLT      '0'
035100010612     C                   SETON                                        31
035200040524     C                   Z-ADD     *zeros        VAXNSP
035300010612     C                   EVAL      vinmsg = %trimr(vinmsg)
035400040524     C                             + ' ' + 'VAXNSP'
035500010612     C                   ELSE
035600040524     C                   MOVE      a_vaxnsp      VAXNSP
035700010612     C                   ENDIF
035800040524     C*
035900040524     C                   TESTN                   a_vaxprr             30
036000040524     C  N30              TESTN                   a_vaxprr               30
036100040524     C  N30              TESTN                   a_vaxprr                 30
036200040524     C                   MOVE      a_vaxprr      W001A             1
036300040524     C     *IN30         IFEQ      *OFF
036400040524     C     W001A         ORLT      '0'
036500040524     C                   SETON                                        31
036600040524     C                   Z-ADD     *zeros        VAXPRR
036700040524     C                   EVAL      vinmsg = %trimr(vinmsg)
036800040524     C                             + ' ' + 'VAXPRR'
036900040524     C                   ELSE
037000040524     C                   MOVE      a_vaxprr      VAXPRR
037100040524     C                   ENDIF
037200010612     C*
037300010612     C* ALFABETICI
037400040524     C                   MOVE      a_vaxatb      VAXATB
037500040524     C                   MOVE      a_vaxtmd      VAXTMD
037600040524     C                   MOVE      a_vaxtrk      VAXTRK
037700040524     C                   MOVE      a_vaxdtt      VAXDTT
037800010330     C*
037900010330     C                   ENDSR
038000010612     C*----------------------------------------------------*
038100990920
038200010601
038300010601
038400010601
038500010601
038600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
038700010601     C     repfil        BEGSR
038800010601     C*
038900010601     C                   if        invfil = *zeros and
039000010601     C                             depfil > *zeros and
039100010613     C                             (vinflg = *blanks or
039200010613     C                              vinflg = *zeros)
039300010601     C
039400010601     C                   eval      invfil = depfil
039500010601     C                   endif
039600010601     C*
039700010601     C                   if        depfil <> invfil and
039800010601     C                             invfil > *zeros
039900010601     C                   eval      flgMulti = '1'
040000010601     C                   if        vinflg = *blanks
040100010601     C                   add       1             cntNonEl
040200010601     C                   endif
040300010601     C                   endif
040400010601     C*
040500010601     C                   if        vinflg = '2'
040600010601     C                   eval      flgStato = '2'
040700010601     C                   endif
040800010601     C*
040900010601     C                   ENDSR
041000010601     C***
041100010601
041200010601
041300010601
041400010330
041500010330
041600010330
041700990920      /TITLE Invio dei dati al punto operativo.
041800000613     C     invio         BEGSR
041900990920     C*
042000990920     C                   reset                   dscmz
042100010601     C                   move      invfil        cmzdst
042200990920     C                   eval      cmzfld = vlrfou
042300990920     C                   eval      cmzmbd = vlrhdl
042400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
042500000710     C                   if        prmfir = *blanks
042600061106     C                   eval      cmzfla = 'EDIVAX0F'
042700061106     C                   eval      cmzmba = 'EDIVAX0F'
042800000710     C                   else
042900000710     C                   eval      cmzfla = prmfir
043000000710     C                   eval      cmzmba = prmfir
043100000710     C                   endif
043200990920     C                   eval      cmznrr = *zeros
043300061106     C                   move      §ctdok        cmznrr
043400021003     C                   eval      cmzlba = vlrfl1
043500990920     C                   call(e)   'TIS711C'
043600990920     C                   parm                    dscmz
043700990921     C                   parm      *blanks       esito
043800990920     C                   if        %error
043900990920     C                             or cmzerr = '1'
044000990921     C                             or esito  = '1'
044100000710     C                   eval      wrkesito = '3'
044200990920     C                   endif
044300990920     C*
044400000613     C                   ENDSR
044500990910
044600010601
044700010601
044800010601
044900010601
045000010601      /TITLE Invio dei dati al punto operativo.
045100010601     C     opeini        BEGSR
045200010601     C*
045300010601     C* Inizializzo flag e contatori operativi
045400010601     C                   movel     '0'           flgGiro           1
045500010601     C                   movel     '0'           flgMulti          1
045600010601     C                   movel     '1'           flgStato          1
045700010615     C                   movel     '0'           flgOk             1
045800010601     C                   z-add     *zeros        cntNonEl         10 0
045900010601     C                   z-add     *zeros        depfil            3 0
046000010601     C                   z-add     *zeros        invfil            3 0
046100030407     C*
046200030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
046300030407     C                   if        %subst(vlrppt:1:1) = 'S'
046400030407     C                   seton                                        50
046500030407     C                   else
046600030407     C                   setoff                                       50
046700030407     C                   endif
046800010601     C*
046900010601     C                   ENDSR
047000010601     C***
047100080606
047200080606
047300080606
047400080606     C*----------------------------------------------------*
047500080606     C*  CONTROLLO NUMERICITA' CAMPI
047600080606     C*----------------------------------------------------*
047700080606     C     CHKNUM        BEGSR
047800080606     C*
047900080606     C                   IF        PiDecChr = *blanks
048000080606     C                   EVAL      PiDecChr = ','
048100080606     C                   ENDIF
048200080606     C*
048300080606     C                   CALL(e)   'ISNUMERIC'
048400080606     C                   PARM                    PiStr            30
048500080606     C                   PARM                    PiDecChr          1
048600080606     C                   PARM      *ZEROS        PiVal            30 9
048700080606     C                   PARM      '0'           PiInt             1
048800080606     C                   PARM      '0'           PiNum             1
048900080606     C                   IF        %error
049000080606     C                   EVAL      PiNum=*off
049100080606     C                   ENDIF
049200080606     C*
049300080606     C                   ENDSR
049400080606     C***
049500010601
049600010601
049700010601
049800010330
049900010330
050000000613     C     *inzsr        BEGSR
050100990910     C*
050200990910     C     *entry        plist
050300990920     C                   parm                    tivlrds
050400990921     C                   parm      wrkesito      esito
050500000724     C                   parm                    prmlit
050600000710     C                   parm                    prmfir
050700061106     C*
050800061106     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
050900061106     C                   move(p)   vlrMSG        wNomeFile
051000061106     C*
051100070830     C* Se nn forzato CMR nei parametri provo a reperirlo dal nome file corrente quindi =>
051200070830     C* reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
051300061106     C* del codice CMR dal nome del file importato.
051400070830     C                   if        %subst(vlrppt:6:10) <> *blanks
051500070830     C                   eval      wCMR = %trim(%subst(vlrppt:6:10))
051600070830     C                   seton                                        60
051700070830     C                   else
051800061106     C                   if        wNomeFile <> *blanks AND
051900061106     C                             vlrppt<>*blanks
052000061106     C                   if        %subst(vlrppt:1:1) = 'S'
052100061106     C                   seton                                        50
052200061106     C                   else
052300061106     C                   setoff                                       50
052400061106     C                   endif
052500061106     C                   eval      wPosDaA = %subst(vlrppt:2:2)
052600061106     C                   move(p)   wPosDaA       wPosDa
052700061106     C                   eval      wLungA  = %subst(vlrppt:4:2)
052800061106     C                   move(p)   wLungA        wLung
052900061106     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
053000061106     C                   seton                                        60
053100061106     C                   else
053200061106     C                   setoff                                       60
053300061106     C                   endif
053400070830     C                   endif
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
