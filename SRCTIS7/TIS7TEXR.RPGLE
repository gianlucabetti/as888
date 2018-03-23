000100040712      /TITLE Upload via Internet: traduzione in EDIVAXWR.
000200130115     H dftactgrp(*no) actgrp(*caller)
000300021014
000400990914     Ftivin00r  uF   E             DISK    usropn
000500040712     FEDIVAXWR  O    E             DISK
000600990914
000700000724     D*------------
000800990921     D dscmz         e ds                  inz
000900990914     D psds           sds
001000990914     D  procname         *PROC
001100990921     D tivlrds       e ds                  extname(tivlr00f)
001200990914     D esito           s              1
001300000724     D prmlit          s             10
001400000710     D prmfir          s             10
001500990921     D wrkesito        s                   like(esito)
001600000613     D rrnum           s              6  0 INZ(*zeros)
001700030616     D wNomeFile       s             30    INZ(*blanks)
001800030710     D wCMR            s             35    INZ(*blanks)
001900030710     D wPosDaA         s              2    INZ(*blanks)
002000030710     D wPosDa          s              2  0 INZ(*zeros)
002100030710     D wLungA          s              2    INZ(*blanks)
002200030710     D wLung           s              2  0 INZ(*zeros)
002300010606     D*------------------
002400010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
002500010606     D*------------------
002600010606     D WLBDA8          DS                  INZ
002700010606     D  G08DAT                 1      8  0
002800010606     D  G08INV                 9     16  0
002900010606     D  G08ERR                17     17
003000010606     D  G08TGI                18     22  0
003100040325     D*
003200040712     D fnvaxaar        ds                  inz
003300040712     D  a_vaxatb               1      1
003400040712     D  a_vaxccm               3      9
003500040712     D  a_vaxlnp              11     13
003600040712     D  a_vaxaas              15     18
003700040712     D  a_vaxnrs              20     21
003800040712     D  a_vaxnsp              23     29
003900040712     D  a_vaxtmd              30     33
004000040712     D  a_vaxprr              35     38
004100040712     D  a_vaxtrk              39     40
004200040712     D  a_vaxdtt              41    540
004300010606
004400010606
004500010606
004600990914
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000010531     C*
005100010531     C                   exsr      opeini
005200040712     C                   exsr      rwvax
005300010531     C*
005400010531     C                   seton                                        lr
005500010531
005600010531
005700010531
005800010531
005900010531     C*--------------------------------------------------------
006000010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006100010531     C*--------------------------------------------------------
006200010531     C     PREELA        BEGSR
006300000724     C*
006400000724     C* SE OCCORRE SPEDIRE IN FILIALE
006500010531     C                   if        invfil <> *zeros and
006600010531     C                             flgGiro = '0'
006700010531     C*
006800010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006900010531     C                   eval      flgGiro = '1'
007000010531     C*
007100010531     C                   endif
007200010531     C*
007300010531     C                   ENDSR
007400010531     C***
007500010531
007600010531
007700010531
007800010531
007900010531
008000010531
008100010531
008200010531     C*--------------------------------------------------------
008300010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008400010531     C*--------------------------------------------------------
008500010531     C     ENDELA        BEGSR
008600000616     C*
008700010531     C                   ENDSR
008800010531     C***
008900990914
009000010531
009100010531
009200010531
009300010531
009400990914     C*--------------------------------------------------------
009500040712     C* RWVAX  LEGGE TIVIN00R E SCRIVE EDIVAX0F              *
009600990914     C*--------------------------------------------------------
009700040712     C     RWVAX         BEGSR
009800030710     C*
009900030710     C                   clear                   §CTDOK
010000030710     C                   clear                   §CTDNO
010100990914     C*
010200030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
010300030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
010400030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
010500030710     C                   if        *in60 = *on
010600990914     C                   if        not %open(tivin00r)
010700990914     C                   open      tivin00r
010800990914     C                   endif
010900040712     C                   if        not %open(edivaxwr)
011000040712     C                   open      edivaxwr
011100990914     C                   endif
011200990914     C*
011300990914     C                   DO        *HIVAL
011400010618     C*
011500990914     C                   READ      tivin00r                               70
011600010618     C*
011700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011800010618     C                   if        vinflg = '1'
011900010618     C                   eval      flgOk = '1'
012000010618     C                   endif
012100010618     C*
012200000905     C                   if        vindta > *blanks
012300000613     C                   add       1             rrnum
012400990914     C*
012500010601     C                   if        *in70 = *off and
012600990914     C                             (vinflg = *blanks
012700010711     C                              or vinflg = '0'
012800010711     C                              or vinflg = '2')
012900990914     C*
013000040712     C                   clear                   EDIVAX00
013100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013200010711     C                   if        vinflg = *blanks or vinflg = '0'
013300010711     C                   clear                   vinmsg
013400010711     C                   endif
013500010711     C*
013600040712     C                   eval      fnvaxaar = vindta
013700040325     C*
013800040712     C                   testb     x'3F'         a_vaxatb                 02
013900040325     C                   if        not *in02
014000991022     C*
014100040712     C                   exsr      impvax
014200010531     C*
014300010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014400040712     C                   eval      depfil = VAXLNP
014500010531     C                   exsr      repfil
014600010601     C                   if        depfil = invfil
014700021014     C                   if        vlrpoi = 999
014800040712     C                   MOVE(p)   invfil        VAXFGS
014900021014     C                   else
015000040712     C                   MOVE(p)   vlrpoi        VAXFGS
015100021014     C                   endif
015200010531     C*
015300010531     C                   exsr      PREELA
015400010531     C*
015500030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
015600040712     C                   EVAL      VAXCMR = wCMR
015700040712     C                   EVAL      VAXCNT = 1
015800010601     C*
015900010604     C  N31              ADD       1             §CTDOK            7 0
016000010601     C   32
016100010604     COR 31              ADD       1             §CTDNO            7 0
016200040712     C  N31              WRITE     EDIVAX00
016300010601     C*
016400010601     C                   if        *in31 = *off and
016500010601     C                             *in32 = *off
016600990914     C                   eval      vinflg = '1'
016700990914     C                   else
016800990914     C                   eval      vinflg = '2'
016900990914     C                   endif
017000000905     C                   endif
017100000905     C                   endif
017200010601     C*
017300010601     C                   endif
017400000905     C*
017500000905     C                   else
017600000905     C                   eval      vinflg = '1'
017700000905     C                   endif
017800000905     C*
017900000905     C  N70              update    tivin000
018000991022     C*
018100991022     C  N70              ENDdo
018200010531     C
018300010531     C*
018400010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018500010531     C                   if        cntNonEl = *zeros or
018600010531     C                             flgMulti = '0'
018700990914     C* Se non ci sono record con errori ...
018800010531     C                   if        §ctdno = 0 and
018900010531     C                             flgStato <> '2'
019000990914     C* ... restituisco esito OK.
019100990921     C                   eval      wrkesito = '0'
019200990914     C                   else
019300000710     C                   if        §ctdok > 0
019400990921     C                   eval      wrkesito = '1'
019500000710     C                   else
019600010615     C                   if        flgOk = '0'
019700010615     C                   eval      wrkesito = '2'
019800010615     C                   else
019900010615     C                   eval      wrkesito = '6'
020000010615     C                   endif
020100990914     C                   endif
020200000710     C                   endif
020300010531     C                   else
020400010531     C                   eval      wrkesito = '9'
020500010531     C                   endif
020600030710     C*
020700030710 X  1C                   else
020800030710     C                   eval      wrkesito = '8'
020900030710     C                   eval      flgGiro = '1'
021000030710 E  1C                   endif
021100990914     C*
021200990914     C                   if        %open(tivin00r)
021300990914     C                   close     tivin00r
021400990914     C                   endif
021500040712     C                   if        %open(edivaxwr)
021600040712     C                   close     edivaxwr
021700990914     C                   endif
021800010531     C*
021900010531     C                   if        vlrpoi <> 999
022000010531     C                   eval      invfil = vlrpoi
022100010531     C                   endif
022200990914     C*
022300990921     C                   if        §ctdok > 0
022400010531     C                             and invfil > *zeros
022500990921     C                   exsr      invio
022600990921     C                   endif
022700010612     C*
022800010612     C                   if        flgGiro = '1'
022900010612     C                   exsr      endela
023000010612     C                   endif
023100990921     C*
023200990914     C                   ENDSR
023300000613     C***
023400040325
023500040325
023600040325
023700040325
023800040325     C*----------------------------------------------------*
023900040712     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
024000040325     C*----------------------------------------------------*
024100040712     C     IMPVAX        BEGSR
024200040325     C*
024300040325     C                   SETOFF                                       3132
024400040325     C*
024500040325     C                   SETOFF                                       303132
024600040325     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
024700040712     C                   TESTN                   a_vaxccm             30
024800040712     C  N30              TESTN                   a_vaxccm               30
024900040712     C  N30              TESTN                   a_vaxccm                 30
025000040712     C                   MOVE      a_vaxccm      W001A             1
025100040325     C     *IN30         IFEQ      *OFF
025200040325     C     W001A         ORLT      '0'
025300040325     C                   SETON                                        31
025400040712     C                   Z-ADD     *zeros        VAXCCM
025500040712     C                   EVAL      vinmsg = 'VAXCCM'
025600040325     C                   ELSE
025700040712     C                   MOVE      a_vaxccm      VAXCCM
025800040712     C     VAXCCM        IFLE      *ZEROS
025900040325     C                   SETON                                        31
026000040712     C                   Z-ADD     *zeros        VAXCCM
026100040712     C                   EVAL      vinmsg = 'VAXCCM'
026200040325     C                   ENDIF
026300040325     C                   ENDIF
026400040325     C*
026500040712     C                   TESTN                   a_vaxlnp             30
026600040712     C  N30              TESTN                   a_vaxlnp               30
026700040712     C  N30              TESTN                   a_vaxlnp                 30
026800040712     C                   MOVE      a_vaxlnp      W001A             1
026900040325     C     *IN30         IFEQ      *OFF
027000040325     C     W001A         ORLT      '0'
027100040325     C                   SETON                                        31
027200040712     C                   Z-ADD     *zeros        VAXLNP
027300040325     C                   EVAL      vinmsg = %trimr(vinmsg)
027400040712     C                             + ' ' + 'VAXLNP'
027500040325     C                   ELSE
027600040712     C                   MOVE      a_vaxlnp      VAXLNP
027700040712     C     VAXLNP        IFLE      *ZEROS
027800040325     C                   SETON                                        31
027900040712     C                   Z-ADD     *zeros        VAXLNP
028000040325     C                   EVAL      vinmsg = %trimr(vinmsg)
028100040712     C                             + ' ' + 'VAXLNP'
028200040325     C                   ENDIF
028300040325     C                   ENDIF
028400040325     C*
028500040712     C                   TESTN                   a_vaxaas             30
028600040712     C  N30              TESTN                   a_vaxaas               30
028700040712     C  N30              TESTN                   a_vaxaas                 30
028800040712     C                   MOVE      a_vaxaas      W001A             1
028900040325     C     *IN30         IFEQ      *OFF
029000040325     C     W001A         ORLT      '0'
029100040325     C                   SETON                                        31
029200040712     C                   MOVEL(P)  datcor        VAXAAS
029300040325     C                   EVAL      vinmsg = %trimr(vinmsg)
029400040712     C                             + ' ' + 'VAXAAS'
029500040325     C                   ELSE
029600040712     C                   MOVE      a_vaxaas      VAXAAS
029700040325     C                   ENDIF
029800040325     C*
029900040712     C                   TESTN                   a_vaxnrs             30
030000040712     C  N30              TESTN                   a_vaxnrs               30
030100040712     C  N30              TESTN                   a_vaxnrs                 30
030200040712     C                   MOVE      a_vaxnrs      W001A             1
030300040325     C     *IN30         IFEQ      *OFF
030400040325     C     W001A         ORLT      '0'
030500040325     C     W001A         ANDNE     *BLANK
030600040325     C                   SETON                                        31
030700040712     C                   Z-ADD     *zeros        VAXNRS
030800040325     C                   EVAL      vinmsg = %trimr(vinmsg)
030900040712     C                             + ' ' + 'VAXNRS'
031000040325     C                   ELSE
031100040712     C                   MOVE      a_vaxnrs      VAXNRS
031200040325     C                   ENDIF
031300040325     C*
031400040712     C                   TESTN                   a_vaxnsp             30
031500040712     C  N30              TESTN                   a_vaxnsp               30
031600040712     C  N30              TESTN                   a_vaxnsp                 30
031700040712     C                   MOVE      a_vaxnsp      W001A             1
031800040325     C     *IN30         IFEQ      *OFF
031900040325     C     W001A         ORLT      '0'
032000040325     C                   SETON                                        31
032100040712     C                   Z-ADD     *zeros        VAXNSP
032200040325     C                   EVAL      vinmsg = %trimr(vinmsg)
032300040712     C                             + ' ' + 'VAXNSP'
032400040325     C                   ELSE
032500040712     C                   MOVE      a_vaxnsp      VAXNSP
032600040325     C                   ENDIF
032700040712     C*
032800040712     C                   TESTN                   a_vaxprr             30
032900040712     C  N30              TESTN                   a_vaxprr               30
033000040712     C  N30              TESTN                   a_vaxprr                 30
033100040712     C                   MOVE      a_vaxprr      W001A             1
033200040712     C     *IN30         IFEQ      *OFF
033300040712     C     W001A         ORLT      '0'
033400040712     C                   SETON                                        31
033500040712     C                   Z-ADD     *zeros        VAXPRR
033600040712     C                   EVAL      vinmsg = %trimr(vinmsg)
033700040712     C                             + ' ' + 'VAXPRR'
033800040712     C                   ELSE
033900040712     C                   MOVE      a_vaxprr      VAXPRR
034000040712     C                   ENDIF
034100040325     C*
034200040325     C* ALFABETICI
034300040712     C                   MOVE      a_vaxatb      VAXATB
034400040712     C                   MOVE      a_vaxtmd      VAXTMD
034500040712     C                   MOVE      a_vaxtrk      VAXTRK
034600040712     C                   MOVE      a_vaxdtt      VAXDTT
034700040325     C*
034800040325     C                   ENDSR
034900040325     C*----------------------------------------------------*
035000990921
035100010531
035200010531
035300010531
035400010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
035500010531     C     repfil        BEGSR
035600010531     C*
035700010531     C                   if        invfil = *zeros and
035800010531     C                             depfil > *zeros and
035900010613     C                             (vinflg = *blanks or
036000010613     C                              vinflg = *zeros)
036100010531     C
036200010531     C                   eval      invfil = depfil
036300010531     C                   endif
036400010531     C*
036500010531     C                   if        depfil <> invfil and
036600010531     C                             invfil > *zeros
036700010531     C                   eval      flgMulti = '1'
036800010531     C                   if        vinflg = *blanks
036900010531     C                   add       1             cntNonEl
037000010531     C                   endif
037100010531     C                   endif
037200010531     C*
037300010531     C                   if        vinflg = '2'
037400010531     C                   eval      flgStato = '2'
037500010531     C                   endif
037600010531     C*
037700010531     C                   ENDSR
037800010531     C***
037900010531
038000010531
038100010531
038200010531
038300010531
038400990921      /TITLE Invio dei dati al punto operativo.
038500000613     C     invio         BEGSR
038600990921     C*
038700990921     C                   reset                   dscmz
038800030618     C                   move      invfil        cmzdst
038900030618     C***                move      '789'         cmzdst
039000990921     C                   eval      cmzfld = vlrfou
039100990921     C                   eval      cmzmbd = vlrhdl
039200990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
039300000710     C                   if        prmfir = *blanks
039400040712     C                   eval      cmzfla = 'EDIVAX0F'
039500040712     C                   eval      cmzmba = 'EDIVAX0F'
039600000710     C                   else
039700000710     C                   eval      cmzfla = prmfir
039800000710     C                   eval      cmzmba = prmfir
039900000710     C                   endif
040000990921     C                   eval      cmznrr = *zeros
040100990921     C                   move      §ctdok        cmznrr
040200021003     C                   eval      cmzlba = vlrfl1
040300990921     C                   call(e)   'TIS711C'
040400990921     C                   parm                    dscmz
040500990921     C                   parm      *blanks       esito
040600990921     C                   if        %error
040700990921     C                             or cmzerr = '1'
040800990921     C                             or esito  = '1'
040900000710     C                   eval      wrkesito = '3'
041000990921     C                   endif
041100990921     C*
041200000613     C                   ENDSR
041300000613     C***
041400990914
041500010531
041600010531
041700010531
041800010531      /TITLE Invio dei dati al punto operativo.
041900010531     C     opeini        BEGSR
042000010531     C*
042100010531     C* Inizializzo flag e contatori operativi
042200010531     C                   movel     '0'           flgGiro           1
042300010531     C                   movel     '0'           flgMulti          1
042400010531     C                   movel     '1'           flgStato          1
042500010615     C                   movel     '0'           flgOk             1
042600010531     C                   z-add     *zeros        cntNonEl         10 0
042700010531     C                   z-add     *zeros        depfil            3 0
042800010531     C                   z-add     *zeros        invfil            3 0
042900030407     C*
043000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
043100030407     C                   if        %subst(vlrppt:1:1) = 'S'
043200030407     C                   seton                                        50
043300030407     C                   else
043400030407     C                   setoff                                       50
043500030407     C                   endif
043600010531     C*
043700010531     C                   ENDSR
043800010531     C***
043900010531
044000010531
044100010531
044200010531
044300000613     C     *inzsr        BEGSR
044400990914     C*
044500990914     C     *entry        plist
044600990921     C                   parm                    tivlrds
044700990921     C                   parm      wrkesito      esito
044800000724     C                   parm                    prmlit
044900000710     C                   parm                    prmfir
045000030710     C*
045100030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
045200030710     C                   move(p)   vlrMSG        wNomeFile
045300030710     C*
045400030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
045500030710     C* del codice CMR dal nome del file importato.
045600030710     C                   if        wNomeFile <> *blanks AND
045700030710     C                             vlrppt<>*blanks
045800030710     C                   if        %subst(vlrppt:1:1) = 'S'
045900030710     C                   seton                                        50
046000030710     C                   else
046100030710     C                   setoff                                       50
046200030710     C                   endif
046300030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
046400030710     C                   move(p)   wPosDaA       wPosDa
046500030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
046600030710     C                   move(p)   wLungA        wLung
046700030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
046800030710     C                   seton                                        60
046900030710     C                   else
047000030710     C                   setoff                                       60
047100030710     C                   endif
047200010606     C*
047300010606     C* CALCOLA LA DATA CORRENTE
047400010606     C                   time                    wn14             14 0
047500010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
047600010606     C                   z-add     wn8           g08dat
047700010606     C                   z-add     *zeros        g08inv
047800010606     C                   movel     '0'           g08err
047900010606     C                   call      'XSRDA8'
048000010606     C                   parm                    wlbda8
048100010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
048200000613     C*
048300000613     C                   ENDSR
048400000613     C***
