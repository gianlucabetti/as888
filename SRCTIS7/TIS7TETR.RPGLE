000100040325      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200130115     H dftactgrp(*no) actgrp(*caller)
000300021014
000400990914     Ftivin00r  uF   E             DISK    usropn
000500040325     FEDIVATWR  O    E             DISK
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
003200040325     D fnvataar        ds                  inz
003300040325     D  a_vatatb               1      1
003400040325     D  a_vatccm               3      9
003500040325     D  a_vatlnp              11     13
003600040325     D  a_vataas              15     18
003700040325     D  a_vatnrs              20     21
003800040325     D  a_vatnsp              23     29
003900040325     D  a_vattrc              30     30
004000040325     D  a_vatnot              31     65
004100010606
004200010606
004300010606
004400990914
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010531     C*
004900010531     C                   exsr      opeini
005000040325     C                   exsr      rwvat
005100010531     C*
005200010531     C                   seton                                        lr
005300010531
005400010531
005500010531
005600010531
005700010531     C*--------------------------------------------------------
005800010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005900010531     C*--------------------------------------------------------
006000010531     C     PREELA        BEGSR
006100000724     C*
006200000724     C* SE OCCORRE SPEDIRE IN FILIALE
006300010531     C                   if        invfil <> *zeros and
006400010531     C                             flgGiro = '0'
006500010531     C*
006600010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006700010531     C                   eval      flgGiro = '1'
006800010531     C*
006900010531     C                   endif
007000010531     C*
007100010531     C                   ENDSR
007200010531     C***
007300010531
007400010531
007500010531
007600010531
007700010531
007800010531
007900010531
008000010531     C*--------------------------------------------------------
008100010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008200010531     C*--------------------------------------------------------
008300010531     C     ENDELA        BEGSR
008400000616     C*
008500010531     C                   ENDSR
008600010531     C***
008700990914
008800010531
008900010531
009000010531
009100010531
009200990914     C*--------------------------------------------------------
009300040325     C* RWVAT  LEGGE TIVIN00R E SCRIVE EDIVAT0F              *
009400990914     C*--------------------------------------------------------
009500040325     C     RWVAT         BEGSR
009600030710     C*
009700030710     C                   clear                   §CTDOK
009800030710     C                   clear                   §CTDNO
009900990914     C*
010000030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
010100030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
010200030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
010300030710     C                   if        *in60 = *on
010400990914     C                   if        not %open(tivin00r)
010500990914     C                   open      tivin00r
010600990914     C                   endif
010700040325     C                   if        not %open(edivatwr)
010800040325     C                   open      edivatwr
010900990914     C                   endif
011000990914     C*
011100990914     C                   DO        *HIVAL
011200010618     C*
011300990914     C                   READ      tivin00r                               70
011400010618     C*
011500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011600010618     C                   if        vinflg = '1'
011700010618     C                   eval      flgOk = '1'
011800010618     C                   endif
011900010618     C*
012000000905     C                   if        vindta > *blanks
012100000613     C                   add       1             rrnum
012200990914     C*
012300010601     C                   if        *in70 = *off and
012400990914     C                             (vinflg = *blanks
012500010711     C                              or vinflg = '0'
012600010711     C                              or vinflg = '2')
012700990914     C*
012800040325     C                   clear                   EDIVAT00
012900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013000010711     C                   if        vinflg = *blanks or vinflg = '0'
013100010711     C                   clear                   vinmsg
013200010711     C                   endif
013300010711     C*
013400040325     C                   eval      fnvataar = vindta
013500040325     C*
013600040325     C                   testb     x'3F'         a_vatatb                 02
013700040325     C                   if        not *in02
013800991022     C*
013900040325     C                   exsr      impvat
014000010531     C*
014100010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014200040325     C                   eval      depfil = VATLNP
014300010531     C                   exsr      repfil
014400010601     C                   if        depfil = invfil
014500021014     C                   if        vlrpoi = 999
014600040325     C                   MOVE(p)   invfil        VATFGS
014700021014     C                   else
014800040325     C                   MOVE(p)   vlrpoi        VATFGS
014900021014     C                   endif
015000010531     C*
015100010531     C                   exsr      PREELA
015200010531     C*
015300030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
015400040325     C                   EVAL      VATCMR = wCMR
015500040325     C                   EVAL      VATCNT = 1
015600010601     C*
015700010604     C  N31              ADD       1             §CTDOK            7 0
015800010601     C   32
015900010604     COR 31              ADD       1             §CTDNO            7 0
016000040325     C  N31              WRITE     EDIVAT00
016100010601     C*
016200010601     C                   if        *in31 = *off and
016300010601     C                             *in32 = *off
016400990914     C                   eval      vinflg = '1'
016500990914     C                   else
016600990914     C                   eval      vinflg = '2'
016700990914     C                   endif
016800000905     C                   endif
016900000905     C                   endif
017000010601     C*
017100010601     C                   endif
017200000905     C*
017300000905     C                   else
017400000905     C                   eval      vinflg = '1'
017500000905     C                   endif
017600000905     C*
017700000905     C  N70              update    tivin000
017800991022     C*
017900991022     C  N70              ENDdo
018000010531     C
018100010531     C*
018200010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018300010531     C                   if        cntNonEl = *zeros or
018400010531     C                             flgMulti = '0'
018500990914     C* Se non ci sono record con errori ...
018600010531     C                   if        §ctdno = 0 and
018700010531     C                             flgStato <> '2'
018800990914     C* ... restituisco esito OK.
018900990921     C                   eval      wrkesito = '0'
019000990914     C                   else
019100000710     C                   if        §ctdok > 0
019200990921     C                   eval      wrkesito = '1'
019300000710     C                   else
019400010615     C                   if        flgOk = '0'
019500010615     C                   eval      wrkesito = '2'
019600010615     C                   else
019700010615     C                   eval      wrkesito = '6'
019800010615     C                   endif
019900990914     C                   endif
020000000710     C                   endif
020100010531     C                   else
020200010531     C                   eval      wrkesito = '9'
020300010531     C                   endif
020400030710     C*
020500030710 X  1C                   else
020600030710     C                   eval      wrkesito = '8'
020700030710     C                   eval      flgGiro = '1'
020800030710 E  1C                   endif
020900990914     C*
021000990914     C                   if        %open(tivin00r)
021100990914     C                   close     tivin00r
021200990914     C                   endif
021300040325     C                   if        %open(edivatwr)
021400040325     C                   close     edivatwr
021500990914     C                   endif
021600010531     C*
021700010531     C                   if        vlrpoi <> 999
021800010531     C                   eval      invfil = vlrpoi
021900010531     C                   endif
022000990914     C*
022100990921     C                   if        §ctdok > 0
022200010531     C                             and invfil > *zeros
022300990921     C                   exsr      invio
022400990921     C                   endif
022500010612     C*
022600010612     C                   if        flgGiro = '1'
022700010612     C                   exsr      endela
022800010612     C                   endif
022900990921     C*
023000990914     C                   ENDSR
023100000613     C***
023200040325
023300040325
023400040325
023500040325
023600040325     C*----------------------------------------------------*
023700040325     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
023800040325     C*----------------------------------------------------*
023900040325     C     IMPVAT        BEGSR
024000040325     C*
024100040325     C                   SETOFF                                       3132
024200040325     C*
024300040325     C                   SETOFF                                       303132
024400040325     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
024500040325     C                   TESTN                   a_vatccm             30
024600040325     C  N30              TESTN                   a_vatccm               30
024700040325     C  N30              TESTN                   a_vatccm                 30
024800040325     C                   MOVE      a_vatccm      W001A             1
024900040325     C     *IN30         IFEQ      *OFF
025000040325     C     W001A         ORLT      '0'
025100040325     C                   SETON                                        31
025200040325     C                   Z-ADD     *zeros        VATCCM
025300040325     C                   EVAL      vinmsg = 'VATCCM'
025400040325     C                   ELSE
025500040325     C                   MOVE      a_vatccm      VATCCM
025600040325     C     VATCCM        IFLE      *ZEROS
025700040325     C                   SETON                                        31
025800040325     C                   Z-ADD     *zeros        VATCCM
025900040325     C                   EVAL      vinmsg = 'VATCCM'
026000040325     C                   ENDIF
026100040325     C                   ENDIF
026200040325     C*
026300040325     C                   TESTN                   a_vatlnp             30
026400040325     C  N30              TESTN                   a_vatlnp               30
026500040325     C  N30              TESTN                   a_vatlnp                 30
026600040325     C                   MOVE      a_vatlnp      W001A             1
026700040325     C     *IN30         IFEQ      *OFF
026800040325     C     W001A         ORLT      '0'
026900040325     C                   SETON                                        31
027000040325     C                   Z-ADD     *zeros        VATLNP
027100040325     C                   EVAL      vinmsg = %trimr(vinmsg)
027200040325     C                             + ' ' + 'VATLNP'
027300040325     C                   ELSE
027400040325     C                   MOVE      a_vatlnp      VATLNP
027500040325     C     VATLNP        IFLE      *ZEROS
027600040325     C                   SETON                                        31
027700040325     C                   Z-ADD     *zeros        VATLNP
027800040325     C                   EVAL      vinmsg = %trimr(vinmsg)
027900040325     C                             + ' ' + 'VATLNP'
028000040325     C                   ENDIF
028100040325     C                   ENDIF
028200040325     C*
028300040325     C                   TESTN                   a_vataas             30
028400040325     C  N30              TESTN                   a_vataas               30
028500040325     C  N30              TESTN                   a_vataas                 30
028600040325     C                   MOVE      a_vataas      W001A             1
028700040325     C     *IN30         IFEQ      *OFF
028800040325     C     W001A         ORLT      '0'
028900040325     C                   SETON                                        31
029000040325     C                   MOVEL(P)  datcor        VATAAS
029100040325     C                   EVAL      vinmsg = %trimr(vinmsg)
029200040325     C                             + ' ' + 'VATAAS'
029300040325     C                   ELSE
029400040325     C                   MOVE      a_vataas      VATAAS
029500040325     C                   ENDIF
029600040325     C*
029700040325     C                   TESTN                   a_vatnrs             30
029800040325     C  N30              TESTN                   a_vatnrs               30
029900040325     C  N30              TESTN                   a_vatnrs                 30
030000040325     C                   MOVE      a_vatnrs      W001A             1
030100040325     C     *IN30         IFEQ      *OFF
030200040325     C     W001A         ORLT      '0'
030300040325     C     W001A         ANDNE     *BLANK
030400040325     C                   SETON                                        31
030500040325     C                   Z-ADD     *zeros        VATNRS
030600040325     C                   EVAL      vinmsg = %trimr(vinmsg)
030700040325     C                             + ' ' + 'VATNRS'
030800040325     C                   ELSE
030900040325     C                   MOVE      a_vatnrs      VATNRS
031000040325     C                   ENDIF
031100040325     C*
031200040325     C                   TESTN                   a_vatnsp             30
031300040325     C  N30              TESTN                   a_vatnsp               30
031400040325     C  N30              TESTN                   a_vatnsp                 30
031500040325     C                   MOVE      a_vatnsp      W001A             1
031600040325     C     *IN30         IFEQ      *OFF
031700040325     C     W001A         ORLT      '0'
031800040325     C                   SETON                                        31
031900040325     C                   Z-ADD     *zeros        VATNSP
032000040325     C                   EVAL      vinmsg = %trimr(vinmsg)
032100040325     C                             + ' ' + 'VATNSP'
032200040325     C                   ELSE
032300040325     C                   MOVE      a_vatnsp      VATNSP
032400040325     C                   ENDIF
032500040325     C*
032600040325     C*
032700040325     C* ALFABETICI
032800040325     C                   MOVE      a_vatatb      VATATB
032900040325     C                   MOVE      a_vattrc      VATTRC
033000131018     C                   IF        a_vattrc <> 'S'
033100040325     C                   EVAL      VATNOT = %trim(a_vatnot)
033200131023     C                   ELSE
033300131023     C                   EVAL      VATNOT = a_vatnot
033400131018     C                   ENDIF
033500040325     C*
033600040325     C                   ENDSR
033700040325     C*----------------------------------------------------*
033800990921
033900010531
034000010531
034100010531
034200010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
034300010531     C     repfil        BEGSR
034400010531     C*
034500010531     C                   if        invfil = *zeros and
034600010531     C                             depfil > *zeros and
034700010613     C                             (vinflg = *blanks or
034800010613     C                              vinflg = *zeros)
034900010531     C
035000010531     C                   eval      invfil = depfil
035100010531     C                   endif
035200010531     C*
035300010531     C                   if        depfil <> invfil and
035400010531     C                             invfil > *zeros
035500010531     C                   eval      flgMulti = '1'
035600010531     C                   if        vinflg = *blanks
035700010531     C                   add       1             cntNonEl
035800010531     C                   endif
035900010531     C                   endif
036000010531     C*
036100010531     C                   if        vinflg = '2'
036200010531     C                   eval      flgStato = '2'
036300010531     C                   endif
036400010531     C*
036500010531     C                   ENDSR
036600010531     C***
036700010531
036800010531
036900010531
037000010531
037100010531
037200990921      /TITLE Invio dei dati al punto operativo.
037300000613     C     invio         BEGSR
037400990921     C*
037500990921     C                   reset                   dscmz
037600030618     C                   move      invfil        cmzdst
037700030618     C***                move      '789'         cmzdst
037800990921     C                   eval      cmzfld = vlrfou
037900990921     C                   eval      cmzmbd = vlrhdl
038000990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
038100000710     C                   if        prmfir = *blanks
038200040325     C                   eval      cmzfla = 'EDIVAT0F'
038300040325     C                   eval      cmzmba = 'EDIVAT0F'
038400000710     C                   else
038500000710     C                   eval      cmzfla = prmfir
038600000710     C                   eval      cmzmba = prmfir
038700000710     C                   endif
038800990921     C                   eval      cmznrr = *zeros
038900990921     C                   move      §ctdok        cmznrr
039000021003     C                   eval      cmzlba = vlrfl1
039100990921     C                   call(e)   'TIS711C'
039200990921     C                   parm                    dscmz
039300990921     C                   parm      *blanks       esito
039400990921     C                   if        %error
039500990921     C                             or cmzerr = '1'
039600990921     C                             or esito  = '1'
039700000710     C                   eval      wrkesito = '3'
039800990921     C                   endif
039900990921     C*
040000000613     C                   ENDSR
040100000613     C***
040200990914
040300010531
040400010531
040500010531
040600010531      /TITLE Invio dei dati al punto operativo.
040700010531     C     opeini        BEGSR
040800010531     C*
040900010531     C* Inizializzo flag e contatori operativi
041000010531     C                   movel     '0'           flgGiro           1
041100010531     C                   movel     '0'           flgMulti          1
041200010531     C                   movel     '1'           flgStato          1
041300010615     C                   movel     '0'           flgOk             1
041400010531     C                   z-add     *zeros        cntNonEl         10 0
041500010531     C                   z-add     *zeros        depfil            3 0
041600010531     C                   z-add     *zeros        invfil            3 0
041700030407     C*
041800030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
041900030407     C                   if        %subst(vlrppt:1:1) = 'S'
042000030407     C                   seton                                        50
042100030407     C                   else
042200030407     C                   setoff                                       50
042300030407     C                   endif
042400010531     C*
042500010531     C                   ENDSR
042600010531     C***
042700010531
042800010531
042900010531
043000010531
043100000613     C     *inzsr        BEGSR
043200990914     C*
043300990914     C     *entry        plist
043400990921     C                   parm                    tivlrds
043500990921     C                   parm      wrkesito      esito
043600000724     C                   parm                    prmlit
043700000710     C                   parm                    prmfir
043800030710     C*
043900030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
044000030710     C                   move(p)   vlrMSG        wNomeFile
044100030710     C*
044200030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
044300030710     C* del codice CMR dal nome del file importato.
044400030710     C                   if        wNomeFile <> *blanks AND
044500030710     C                             vlrppt<>*blanks
044600030710     C                   if        %subst(vlrppt:1:1) = 'S'
044700030710     C                   seton                                        50
044800030710     C                   else
044900030710     C                   setoff                                       50
045000030710     C                   endif
045100030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
045200030710     C                   move(p)   wPosDaA       wPosDa
045300030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
045400030710     C                   move(p)   wLungA        wLung
045500030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
045600030710     C                   seton                                        60
045700030710     C                   else
045800030710     C                   setoff                                       60
045900030710     C                   endif
046000010606     C*
046100010606     C* CALCOLA LA DATA CORRENTE
046200010606     C                   time                    wn14             14 0
046300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
046400010606     C                   z-add     wn8           g08dat
046500010606     C                   z-add     *zeros        g08inv
046600010606     C                   movel     '0'           g08err
046700010606     C                   call      'XSRDA8'
046800010606     C                   parm                    wlbda8
046900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
047000000613     C*
047100000613     C                   ENDSR
047200000613     C***
