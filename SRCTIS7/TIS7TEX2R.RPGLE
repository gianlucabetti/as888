000100061106      /TITLE Upload via Internet: traduzione in EDIVAXWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
003500101209     D  a_vaxatb                      1
003600101209     D  a_vaxfgs                      3
003700101209     D  a_vaxccm                      7
003800101209     D  a_vaxlnp                      3
003900101209     D  a_vaxaas                      4
004000101209     D  a_vaxnrs                      2
004100101209     D  a_vaxnsp                      7
004200101209     D  a_vaxtmd                      4
004300101209     D  a_vaxprr                      4
004400101209     D  a_vaxtrk                      2
004500101209     D  a_vaxdtt                    500
004600010330
004700010330
004800010330
004900000913     C                   reset                   rrnum
005000990921     C                   reset                   esito
005100990921     C                   reset                   wrkesito
005200010601     C*
005300010601     C                   exsr      opeini
005400040524     C                   exsr      rwvax
005500010601     C*
005600010601     C                   seton                                        lr
005700010601
005800010601
005900010601
006000010601
006100010601     C*--------------------------------------------------------
006200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006300010601     C*--------------------------------------------------------
006400010601     C     PREELA        BEGSR
006500010601     C*
006600010601     C* SE OCCORRE SPEDIRE IN FILIALE
006700010601     C                   if        invfil <> *zeros and
006800010601     C                             flgGiro = '0'
006900010601     C*
007000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007100010601     C                   eval      flgGiro = '1'
007200010601     C*
007300010601     C                   endif
007400010601     C*
007500010601     C                   ENDSR
007600010601     C***
007700010601
007800010601
007900010601
008000010601
008100010601
008200010601
008300010601
008400010601     C*--------------------------------------------------------
008500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008600010601     C*--------------------------------------------------------
008700010601     C     ENDELA        BEGSR
008800000616     C*
008900010601     C                   ENDSR
009000010601     C***
009100000613
009200010330
009300010601
009400010601
009500010601
009600010330     C*--------------------------------------------------------
009700061106     C* RWVAX   LEGGE TIVIN00R E SCRIVE EDIVAXWF              *
009800010330     C*--------------------------------------------------------
009900040524     C     RWVAX         BEGSR
010000061106     C*
010100061106     C                   clear                   §CTDOK
010200061106     C                   clear                   §CTDNO
010300061106     C*
010400061106     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
010500061106     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
010600061106     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
010700061106     C                   if        *in60 = *on
010800010330     C                   if        not %open(tivin00r)
010900010330     C                   open      tivin00r
011000010330     C                   endif
011100061106     C                   if        not %open(edivaxwr)
011200061106     C                   open      edivaxwr
011300010330     C                   endif
011400010330     C*
011500010330     C                   DO        *HIVAL
011600010330     C*
011700010330     C                   READ      tivin00r                               70
011800010618     C*
011900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012000010618     C                   if        vinflg = '1'
012100010618     C                   eval      flgOk = '1'
012200010618     C                   endif
012300010618     C*
012400010330     C                   if        vindta > *blanks
012500010330     C                   add       1             rrnum
012600010330     C*
012700010601     C                   if        *in70 = *off and
012800010330     C                             (vinflg = *blanks
012900010330     C                              or vinflg = '0'
013000010330     C                              or vinflg = '2')
013100010711     C*
013200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013300010711     C                   if        vinflg = *blanks or vinflg = '0'
013400010711     C                   clear                   vinmsg
013500010711     C                   endif
013600010711     C*
013700040524     C                   eval      fnvaxaar = vindta
013800010601     C*
013900010330     C                   exsr      inzvar
014000010330     C                   exsr      defcam
014100040524     C                   exsr      impvax
014200010601     C*
014300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014400101209     C                   eval      depfil = VAXFGS
014500010601     C                   exsr      repfil
014600021010     C                   if        depfil = invfil
014700021014     C                   if        vlrpoi = 999
014800040524     C                   MOVE(P)   invfil        VAXFGS
014900021014     C                   else
015000040524     C                   MOVE(P)   vlrpoi        VAXFGS
015100021014     C                   endif
015200010601     C*
015300010601     C                   exsr      PREELA
015400061106     C*
015500061106     C* VALORIZZO CAMPI RELATIVI AL "CMR"
015600061106     C                   EVAL      VAXCMR = wCMR
015700061106     C                   EVAL      VAXCNT = 1
015800010601     C*
015900010604     C* Ebbene...
016000010604     C*
016100061106     C  N31              ADD       1             §CTDOK            7 0
016200061106     C   32
016300061106     COR 31              ADD       1             §CTDNO            7 0
016400061106     C  N31              WRITE     EDIVAX00
016500010604     C*
016600010604     C                   if        *in31 = *off and
016700010604     C                             *in32 = *off
016800010604     C                   eval      vinflg = '1'
016900010604     C                   else
017000010604     C                   eval      vinflg = '2'
017100010604     C                   endif
017200010604     C                   endif
017300010601     C*
017400010604     C                   endif
017500010604     C*
017600010330     C                   else
017700010330     C                   eval      vinflg = '1'
017800010330     C                   endif
017900010601     C*
018000010601     C  N70              update    tivin000
018100010330     C*
018200010330     C  N70              ENDdo
018300010601     C
018400010601     C*
018500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018600010601     C                   if        cntNonEl = *zeros or
018700010601     C                             flgMulti = '0'
018800010330     C* Se non ci sono record con errori ...
018900061106     C                   if        §ctdno = 0 and
019000010601     C                             flgStato <> '2'
019100010330     C* ... restituisco esito OK.
019200010330     C                   eval      wrkesito = '0'
019300010330     C                   else
019400061106     C                   if        §ctdok > 0
019500010330     C                   eval      wrkesito = '1'
019600010330     C                   else
019700010615     C                   if        flgOk = '0'
019800010615     C                   eval      wrkesito = '2'
019900010615     C                   else
020000010615     C                   eval      wrkesito = '6'
020100010615     C                   endif
020200010330     C                   endif
020300010330     C                   endif
020400010601     C                   else
020500010601     C                   eval      wrkesito = '9'
020600010601     C                   endif
020700061106     C*
020800061106 X  1C                   else
020900061106     C                   eval      wrkesito = '8'
021000061106     C                   eval      flgGiro = '1'
021100061106 E  1C                   endif
021200010330     C*
021300010330     C                   if        %open(tivin00r)
021400010330     C                   close     tivin00r
021500010330     C                   endif
021600061106     C                   if        %open(edivaxwr)
021700061106     C                   close     edivaxwr
021800010330     C                   endif
021900010601     C*
022000010601     C                   if        vlrpoi <> 999
022100010601     C                   eval      invfil = vlrpoi
022200010601     C                   endif
022300010330     C*
022400061106     C                   if        §ctdok > 0
022500010601     C                             and invfil > *zeros
022600010330     C                   exsr      invio
022700010330     C                   endif
022800010601     C*
022900010612     C                   if        flgGiro = '1'
023000010601     C                   exsr      endela
023100010612     C                   endif
023200010330     C*
023300010330     C                   ENDSR
023400010330     C***
023500010330
023600010601
023700010601
023800010601
023900010601
024000010330     C*----------------------------------------------------*
024100010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024200010330     C*----------------------------------------------------*
024300010330     C     INZVAR        BEGSR
024400010330     C*
024500010330     C*
024600010330     C                   ENDSR
024700010330     C*----------------------------------------------------*
024800010330     C*  IMPOSTAZIONE CAMPI COSTANTI
024900010330     C*----------------------------------------------------*
025000010330     C     DEFCAM        BEGSR
025100010330     C*
025200061106     C                   CLEAR                   EDIVAX00
025300080606     C*
025400080606     C                   IF        %trim(%subst(vlrppt:16:7)) <> *blanks
025500080606     C                   EVAL      PiStr=%trim(%subst(vlrppt:16:7))
025600080606     C                   EXSR      CHKNUM
025700080606     C                   IF        PiInt=*on
025800080606     C                   Z-ADD     PiVal         VAXCCM
025900080606     C                   ENDIF
026000080606     C                   ENDIF
026100080606     C                   IF        %trim(%subst(vlrppt:23:3)) <> *blanks
026200080606     C                   EVAL      PiStr=%trim(%subst(vlrppt:23:3))
026300080606     C                   EXSR      CHKNUM
026400080606     C                   IF        PiInt=*on
026500080606     C                   Z-ADD     PiVal         VAXLNP
026600080606     C                   ENDIF
026700080606     C                   ENDIF
026800010330     C*
026900010330     C                   ENDSR
027000010330     C*----------------------------------------------------*
027100061106     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAX)
027200010330     C*----------------------------------------------------*
027300040524     C     IMPVAX        BEGSR
027400010330     C*
027500010604     C                   SETOFF                                       3132
027600010612     C*
027700010612     C                   SETOFF                                       303132
027800101209     C*
027900101209     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
028000101209     C                   TESTN                   a_vaxfgs             30
028100101209     C  N30              TESTN                   a_vaxfgs               30
028200101209     C  N30              TESTN                   a_vaxfgs                 30
028300101209     C                   MOVE      a_vaxfgs      W001A             1
028400101209     C     *IN30         IFEQ      *OFF
028500101209     C     W001A         ORLT      '0'
028600101209     C                   SETON                                        31
028700101209     C                   Z-ADD     *zeros        VAXFGS
028800101209     C                   EVAL      vinmsg = %trimr(vinmsg)
028900101209     C                             + ' ' + 'VAXFGS'
029000101209     C                   ELSE
029100101209     C                   MOVE      a_vaxfgs      VAXFGS
029200101209     C     VAXFGS        IFLE      *ZEROS
029300101209     C                   SETON                                        31
029400101209     C                   Z-ADD     *zeros        VAXFGS
029500101209     C                   EVAL      vinmsg = %trimr(vinmsg)
029600101209     C                             + ' ' + 'VAXFGS'
029700101209     C                   ENDIF
029800101209     C                   ENDIF
029900101209     C*
030000040524     C                   TESTN                   a_vaxccm             30
030100040524     C  N30              TESTN                   a_vaxccm               30
030200040524     C  N30              TESTN                   a_vaxccm                 30
030300040524     C                   MOVE      a_vaxccm      W001A             1
030400010612     C     *IN30         IFEQ      *OFF
030500010612     C     W001A         ORLT      '0'
030600010612     C                   SETON                                        31
030700040524     C                   Z-ADD     *zeros        VAXCCM
030800040524     C                   EVAL      vinmsg = 'VAXCCM'
030900010612     C                   ELSE
031000040524     C                   MOVE      a_vaxccm      VAXCCM
031100040524     C     VAXCCM        IFLE      *ZEROS
031200010612     C                   SETON                                        31
031300040524     C                   Z-ADD     *zeros        VAXCCM
031400040524     C                   EVAL      vinmsg = 'VAXCCM'
031500010612     C                   ENDIF
031600010612     C                   ENDIF
031700010612     C*
031800040524     C                   TESTN                   a_vaxlnp             30
031900040524     C  N30              TESTN                   a_vaxlnp               30
032000040524     C  N30              TESTN                   a_vaxlnp                 30
032100040524     C                   MOVE      a_vaxlnp      W001A             1
032200010612     C     *IN30         IFEQ      *OFF
032300010612     C     W001A         ORLT      '0'
032400010612     C                   SETON                                        31
032500040524     C                   Z-ADD     *zeros        VAXLNP
032600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
032700040524     C                             + ' ' + 'VAXLNP'
032800010612     C                   ELSE
032900040524     C                   MOVE      a_vaxlnp      VAXLNP
033000040524     C     VAXLNP        IFLE      *ZEROS
033100010612     C                   SETON                                        31
033200040524     C                   Z-ADD     *zeros        VAXLNP
033300010612     C                   EVAL      vinmsg = %trimr(vinmsg)
033400040524     C                             + ' ' + 'VAXLNP'
033500010612     C                   ENDIF
033600010612     C                   ENDIF
033700010612     C*
033800040524     C                   TESTN                   a_vaxaas             30
033900040524     C  N30              TESTN                   a_vaxaas               30
034000040524     C  N30              TESTN                   a_vaxaas                 30
034100040524     C                   MOVE      a_vaxaas      W001A             1
034200010612     C     *IN30         IFEQ      *OFF
034300010612     C     W001A         ORLT      '0'
034400010612     C                   SETON                                        31
034500040524     C                   MOVEL(P)  datcor        VAXAAS
034600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
034700040524     C                             + ' ' + 'VAXAAS'
034800010612     C                   ELSE
034900040524     C                   MOVE      a_vaxaas      VAXAAS
035000010612     C                   ENDIF
035100010612     C*
035200040524     C                   TESTN                   a_vaxnrs             30
035300040524     C  N30              TESTN                   a_vaxnrs               30
035400040524     C  N30              TESTN                   a_vaxnrs                 30
035500040524     C                   MOVE      a_vaxnrs      W001A             1
035600010612     C     *IN30         IFEQ      *OFF
035700010612     C     W001A         ORLT      '0'
035800010612     C     W001A         ANDNE     *BLANK
035900010612     C                   SETON                                        31
036000040524     C                   Z-ADD     *zeros        VAXNRS
036100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
036200040524     C                             + ' ' + 'VAXNRS'
036300010612     C                   ELSE
036400040524     C                   MOVE      a_vaxnrs      VAXNRS
036500010612     C                   ENDIF
036600010612     C*
036700040524     C                   TESTN                   a_vaxnsp             30
036800040524     C  N30              TESTN                   a_vaxnsp               30
036900040524     C  N30              TESTN                   a_vaxnsp                 30
037000040524     C                   MOVE      a_vaxnsp      W001A             1
037100010612     C     *IN30         IFEQ      *OFF
037200010612     C     W001A         ORLT      '0'
037300010612     C                   SETON                                        31
037400040524     C                   Z-ADD     *zeros        VAXNSP
037500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
037600040524     C                             + ' ' + 'VAXNSP'
037700010612     C                   ELSE
037800040524     C                   MOVE      a_vaxnsp      VAXNSP
037900010612     C                   ENDIF
038000040524     C*
038100040524     C                   TESTN                   a_vaxprr             30
038200040524     C  N30              TESTN                   a_vaxprr               30
038300040524     C  N30              TESTN                   a_vaxprr                 30
038400040524     C                   MOVE      a_vaxprr      W001A             1
038500040524     C     *IN30         IFEQ      *OFF
038600040524     C     W001A         ORLT      '0'
038700040524     C                   SETON                                        31
038800040524     C                   Z-ADD     *zeros        VAXPRR
038900040524     C                   EVAL      vinmsg = %trimr(vinmsg)
039000040524     C                             + ' ' + 'VAXPRR'
039100040524     C                   ELSE
039200040524     C                   MOVE      a_vaxprr      VAXPRR
039300040524     C                   ENDIF
039400010612     C*
039500010612     C* ALFABETICI
039600040524     C                   MOVE      a_vaxatb      VAXATB
039700040524     C                   MOVE      a_vaxtmd      VAXTMD
039800040524     C                   MOVE      a_vaxtrk      VAXTRK
039900040524     C                   MOVE      a_vaxdtt      VAXDTT
040000010330     C*
040100010330     C                   ENDSR
040200010612     C*----------------------------------------------------*
040300990920
040400010601
040500010601
040600010601
040700010601
040800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
040900010601     C     repfil        BEGSR
041000010601     C*
041100010601     C                   if        invfil = *zeros and
041200010601     C                             depfil > *zeros and
041300010613     C                             (vinflg = *blanks or
041400010613     C                              vinflg = *zeros)
041500010601     C
041600010601     C                   eval      invfil = depfil
041700010601     C                   endif
041800010601     C*
041900010601     C                   if        depfil <> invfil and
042000010601     C                             invfil > *zeros
042100010601     C                   eval      flgMulti = '1'
042200010601     C                   if        vinflg = *blanks
042300010601     C                   add       1             cntNonEl
042400010601     C                   endif
042500010601     C                   endif
042600010601     C*
042700010601     C                   if        vinflg = '2'
042800010601     C                   eval      flgStato = '2'
042900010601     C                   endif
043000010601     C*
043100010601     C                   ENDSR
043200010601     C***
043300010601
043400010601
043500010601
043600010330
043700010330
043800010330
043900990920      /TITLE Invio dei dati al punto operativo.
044000000613     C     invio         BEGSR
044100990920     C*
044200990920     C                   reset                   dscmz
044300010601     C                   move      invfil        cmzdst
044400990920     C                   eval      cmzfld = vlrfou
044500990920     C                   eval      cmzmbd = vlrhdl
044600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
044700000710     C                   if        prmfir = *blanks
044800061106     C                   eval      cmzfla = 'EDIVAX0F'
044900061106     C                   eval      cmzmba = 'EDIVAX0F'
045000000710     C                   else
045100000710     C                   eval      cmzfla = prmfir
045200000710     C                   eval      cmzmba = prmfir
045300000710     C                   endif
045400990920     C                   eval      cmznrr = *zeros
045500061106     C                   move      §ctdok        cmznrr
045600021003     C                   eval      cmzlba = vlrfl1
045700990920     C                   call(e)   'TIS711C'
045800990920     C                   parm                    dscmz
045900990921     C                   parm      *blanks       esito
046000990920     C                   if        %error
046100990920     C                             or cmzerr = '1'
046200990921     C                             or esito  = '1'
046300000710     C                   eval      wrkesito = '3'
046400990920     C                   endif
046500990920     C*
046600000613     C                   ENDSR
046700990910
046800010601
046900010601
047000010601
047100010601
047200010601      /TITLE Invio dei dati al punto operativo.
047300010601     C     opeini        BEGSR
047400010601     C*
047500010601     C* Inizializzo flag e contatori operativi
047600010601     C                   movel     '0'           flgGiro           1
047700010601     C                   movel     '0'           flgMulti          1
047800010601     C                   movel     '1'           flgStato          1
047900010615     C                   movel     '0'           flgOk             1
048000010601     C                   z-add     *zeros        cntNonEl         10 0
048100010601     C                   z-add     *zeros        depfil            3 0
048200010601     C                   z-add     *zeros        invfil            3 0
048300030407     C*
048400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
048500030407     C                   if        %subst(vlrppt:1:1) = 'S'
048600030407     C                   seton                                        50
048700030407     C                   else
048800030407     C                   setoff                                       50
048900030407     C                   endif
049000010601     C*
049100010601     C                   ENDSR
049200010601     C***
049300080606
049400080606
049500080606
049600080606     C*----------------------------------------------------*
049700080606     C*  CONTROLLO NUMERICITA' CAMPI
049800080606     C*----------------------------------------------------*
049900080606     C     CHKNUM        BEGSR
050000080606     C*
050100080606     C                   IF        PiDecChr = *blanks
050200080606     C                   EVAL      PiDecChr = ','
050300080606     C                   ENDIF
050400080606     C*
050500080606     C                   CALL(e)   'ISNUMERIC'
050600080606     C                   PARM                    PiStr            30
050700080606     C                   PARM                    PiDecChr          1
050800080606     C                   PARM      *ZEROS        PiVal            30 9
050900080606     C                   PARM      '0'           PiInt             1
051000080606     C                   PARM      '0'           PiNum             1
051100080606     C                   IF        %error
051200080606     C                   EVAL      PiNum=*off
051300080606     C                   ENDIF
051400080606     C*
051500080606     C                   ENDSR
051600080606     C***
051700010601
051800010601
051900010601
052000010330
052100010330
052200000613     C     *inzsr        BEGSR
052300990910     C*
052400990910     C     *entry        plist
052500990920     C                   parm                    tivlrds
052600990921     C                   parm      wrkesito      esito
052700000724     C                   parm                    prmlit
052800000710     C                   parm                    prmfir
052900061106     C*
053000061106     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
053100061106     C                   move(p)   vlrMSG        wNomeFile
053200061106     C*
053300070830     C* Se nn forzato CMR nei parametri provo a reperirlo dal nome file corrente quindi =>
053400070830     C* reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
053500061106     C* del codice CMR dal nome del file importato.
053600070830     C                   if        %subst(vlrppt:6:10) <> *blanks
053700070830     C                   eval      wCMR = %trim(%subst(vlrppt:6:10))
053800070830     C                   seton                                        60
053900070830     C                   else
054000061106     C                   if        wNomeFile <> *blanks AND
054100061106     C                             vlrppt<>*blanks
054200061106     C                   if        %subst(vlrppt:1:1) = 'S'
054300061106     C                   seton                                        50
054400061106     C                   else
054500061106     C                   setoff                                       50
054600061106     C                   endif
054700061106     C                   eval      wPosDaA = %subst(vlrppt:2:2)
054800061106     C                   move(p)   wPosDaA       wPosDa
054900061106     C                   eval      wLungA  = %subst(vlrppt:4:2)
055000061106     C                   move(p)   wLungA        wLung
055100061106     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
055200061106     C                   seton                                        60
055300061106     C                   else
055400061106     C                   setoff                                       60
055500061106     C                   endif
055600070830     C                   endif
055700010330     C*
055800010330     C* CALCOLA LA DATA CORRENTE
055900010330     C                   time                    wn14             14 0
056000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
056100010330     C                   z-add     wn8           g08dat
056200010330     C                   z-add     *zeros        g08inv
056300010330     C                   movel     '0'           g08err
056400010330     C                   call      'XSRDA8'
056500010330     C                   parm                    wlbda8
056600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
056700000613     C*
056800000613     C                   ENDSR
056900000613     C***
