000100040325      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200130116     H dftactgrp(*no) actgrp(*caller)
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
001800040325     D*
001900040325     D fnvataar        ds                  inz
002000040325     D  a_vatatb               1      1
002100040325     D  a_vatccm               3      9
002200040325     D  a_vatlnp              11     13
002300040325     D  a_vataas              15     18
002400040325     D  a_vatnrs              20     21
002500040325     D  a_vatnsp              23     29
002600040325     D  a_vattrc              30     30
002700040325     D  a_vatnot              31     65
002800010606
002900010606
003000010606
003100990914
003200000913     C                   reset                   rrnum
003300990921     C                   reset                   esito
003400990921     C                   reset                   wrkesito
003500010531     C*
003600010531     C                   exsr      opeini
003700040325     C                   exsr      rwvat
003800010531     C*
003900010531     C                   seton                                        lr
004000010531
004100010531
004200010531
004300010531
004400010531     C*--------------------------------------------------------
004500010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004600010531     C*--------------------------------------------------------
004700010531     C     PREELA        BEGSR
004800000724     C*
004900000724     C* SE OCCORRE SPEDIRE IN FILIALE
005000010531     C                   if        invfil <> *zeros and
005100010531     C                             flgGiro = '0'
005200010531     C*
005300010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005400010531     C                   eval      flgGiro = '1'
005500010531     C*
005600010531     C                   endif
005700010531     C*
005800010531     C                   ENDSR
005900010531     C***
006000010531
006100010531
006200010531
006300010531
006400010531
006500010531
006600010531
006700010531     C*--------------------------------------------------------
006800010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006900010531     C*--------------------------------------------------------
007000010531     C     ENDELA        BEGSR
007100000616     C*
007200010531     C                   ENDSR
007300010531     C***
007400990914
007500010531
007600010531
007700010531
007800010531
007900990914     C*--------------------------------------------------------
008000040325     C* RWVAT  LEGGE TIVIN00R E SCRIVE EDIVAT0F              *
008100990914     C*--------------------------------------------------------
008200040325     C     RWVAT         BEGSR
008300030710     C*
008400030710     C                   clear                   §CTDOK
008500030710     C                   clear                   §CTDNO
008600990914     C*
008700990914     C                   if        not %open(tivin00r)
008800990914     C                   open      tivin00r
008900990914     C                   endif
009000040325     C                   if        not %open(edivatwr)
009100040325     C                   open      edivatwr
009200990914     C                   endif
009300990914     C*
009400990914     C                   DO        *HIVAL
009500010618     C*
009600990914     C                   READ      tivin00r                               70
009700010618     C*
009800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009900010618     C                   if        vinflg = '1'
010000010618     C                   eval      flgOk = '1'
010100010618     C                   endif
010200010618     C*
010300000905     C                   if        vindta > *blanks
010400000613     C                   add       1             rrnum
010500990914     C*
010600010601     C                   if        *in70 = *off and
010700990914     C                             (vinflg = *blanks
010800010711     C                              or vinflg = '0'
010900010711     C                              or vinflg = '2')
011000990914     C*
011100040325     C                   clear                   EDIVAT00
011200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011300010711     C                   if        vinflg = *blanks or vinflg = '0'
011400010711     C                   clear                   vinmsg
011500010711     C                   endif
011600010711     C*
011700040325     C                   eval      fnvataar = vindta
011800040325     C*
011900040325     C                   testb     x'3F'         a_vatatb                 02
012000040325     C                   if        not *in02
012100991022     C*
012200040325     C                   exsr      impvat
012300010531     C*
012400010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012500040325     C                   eval      depfil = VATLNP
012600010531     C                   exsr      repfil
012700010601     C                   if        depfil = invfil
012800021014     C                   if        vlrpoi = 999
012900040325     C                   MOVE(p)   invfil        VATFGS
013000021014     C                   else
013100040325     C                   MOVE(p)   vlrpoi        VATFGS
013200021014     C                   endif
013300010531     C*
013400010531     C                   exsr      PREELA
013500010531     C*
013600120313     C* VALORIZZO CAMPI RELATIVI AL "CMR"
013700120313     C                   EVAL      VATCMR = 'WURTH - ' + %subst(wNomeFile:4:5)
013800120313     C                   EVAL      VATCNT = 1
013900010601     C*
014000010604     C  N31              ADD       1             §CTDOK            7 0
014100010601     C   32
014200010604     COR 31              ADD       1             §CTDNO            7 0
014300040325     C  N31              WRITE     EDIVAT00
014400010601     C*
014500010601     C                   if        *in31 = *off and
014600010601     C                             *in32 = *off
014700990914     C                   eval      vinflg = '1'
014800990914     C                   else
014900990914     C                   eval      vinflg = '2'
015000990914     C                   endif
015100000905     C                   endif
015200000905     C                   endif
015300010601     C*
015400010601     C                   endif
015500000905     C*
015600000905     C                   else
015700000905     C                   eval      vinflg = '1'
015800000905     C                   endif
015900000905     C*
016000000905     C  N70              update    tivin000
016100991022     C*
016200991022     C  N70              ENDdo
016300010531     C
016400010531     C*
016500010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016600010531     C                   if        cntNonEl = *zeros or
016700010531     C                             flgMulti = '0'
016800990914     C* Se non ci sono record con errori ...
016900010531     C                   if        §ctdno = 0 and
017000010531     C                             flgStato <> '2'
017100990914     C* ... restituisco esito OK.
017200990921     C                   eval      wrkesito = '0'
017300990914     C                   else
017400000710     C                   if        §ctdok > 0
017500990921     C                   eval      wrkesito = '1'
017600000710     C                   else
017700010615     C                   if        flgOk = '0'
017800010615     C                   eval      wrkesito = '2'
017900010615     C                   else
018000010615     C                   eval      wrkesito = '6'
018100010615     C                   endif
018200990914     C                   endif
018300000710     C                   endif
018400010531     C                   else
018500010531     C                   eval      wrkesito = '9'
018600010531     C                   endif
018700990914     C*
018800990914     C                   if        %open(tivin00r)
018900990914     C                   close     tivin00r
019000990914     C                   endif
019100040325     C                   if        %open(edivatwr)
019200040325     C                   close     edivatwr
019300990914     C                   endif
019400010531     C*
019500010531     C                   if        vlrpoi <> 999
019600010531     C                   eval      invfil = vlrpoi
019700010531     C                   endif
019800990914     C*
019900990921     C                   if        §ctdok > 0
020000010531     C                             and invfil > *zeros
020100990921     C                   exsr      invio
020200990921     C                   endif
020300010612     C*
020400010612     C                   if        flgGiro = '1'
020500010612     C                   exsr      endela
020600010612     C                   endif
020700990921     C*
020800990914     C                   ENDSR
020900000613     C***
021000040325
021100040325
021200040325
021300040325
021400040325     C*----------------------------------------------------*
021500040325     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
021600040325     C*----------------------------------------------------*
021700040325     C     IMPVAT        BEGSR
021800040325     C*
021900040325     C                   SETOFF                                       3132
022000040325     C*
022100040325     C                   SETOFF                                       303132
022200040325     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
022300040325     C                   TESTN                   a_vatccm             30
022400040325     C  N30              TESTN                   a_vatccm               30
022500040325     C  N30              TESTN                   a_vatccm                 30
022600040325     C                   MOVE      a_vatccm      W001A             1
022700040325     C     *IN30         IFEQ      *OFF
022800040325     C     W001A         ORLT      '0'
022900040325     C                   SETON                                        31
023000040325     C                   Z-ADD     *zeros        VATCCM
023100040325     C                   EVAL      vinmsg = 'VATCCM'
023200040325     C                   ELSE
023300040325     C                   MOVE      a_vatccm      VATCCM
023400040325     C     VATCCM        IFLE      *ZEROS
023500040325     C                   SETON                                        31
023600040325     C                   Z-ADD     *zeros        VATCCM
023700040325     C                   EVAL      vinmsg = 'VATCCM'
023800040325     C                   ENDIF
023900040325     C                   ENDIF
024000040325     C*
024100040325     C                   TESTN                   a_vatlnp             30
024200040325     C  N30              TESTN                   a_vatlnp               30
024300040325     C  N30              TESTN                   a_vatlnp                 30
024400040325     C                   MOVE      a_vatlnp      W001A             1
024500040325     C     *IN30         IFEQ      *OFF
024600040325     C     W001A         ORLT      '0'
024700040325     C                   SETON                                        31
024800040325     C                   Z-ADD     *zeros        VATLNP
024900040325     C                   EVAL      vinmsg = %trimr(vinmsg)
025000040325     C                             + ' ' + 'VATLNP'
025100040325     C                   ELSE
025200040325     C                   MOVE      a_vatlnp      VATLNP
025300040325     C     VATLNP        IFLE      *ZEROS
025400040325     C                   SETON                                        31
025500040325     C                   Z-ADD     *zeros        VATLNP
025600040325     C                   EVAL      vinmsg = %trimr(vinmsg)
025700040325     C                             + ' ' + 'VATLNP'
025800040325     C                   ENDIF
025900040325     C                   ENDIF
026000040325     C*
026100040325     C                   TESTN                   a_vataas             30
026200040325     C  N30              TESTN                   a_vataas               30
026300040325     C  N30              TESTN                   a_vataas                 30
026400040325     C                   MOVE      a_vataas      W001A             1
026500040325     C     *IN30         IFEQ      *OFF
026600040325     C     W001A         ORLT      '0'
026700040325     C                   SETON                                        31
026800040325     C                   MOVEL(P)  datcor        VATAAS
026900040325     C                   EVAL      vinmsg = %trimr(vinmsg)
027000040325     C                             + ' ' + 'VATAAS'
027100040325     C                   ELSE
027200040325     C                   MOVE      a_vataas      VATAAS
027300040325     C                   ENDIF
027400040325     C*
027500040325     C                   TESTN                   a_vatnrs             30
027600040325     C  N30              TESTN                   a_vatnrs               30
027700040325     C  N30              TESTN                   a_vatnrs                 30
027800040325     C                   MOVE      a_vatnrs      W001A             1
027900040325     C     *IN30         IFEQ      *OFF
028000040325     C     W001A         ORLT      '0'
028100040325     C     W001A         ANDNE     *BLANK
028200040325     C                   SETON                                        31
028300040325     C                   Z-ADD     *zeros        VATNRS
028400040325     C                   EVAL      vinmsg = %trimr(vinmsg)
028500040325     C                             + ' ' + 'VATNRS'
028600040325     C                   ELSE
028700040325     C                   MOVE      a_vatnrs      VATNRS
028800040325     C                   ENDIF
028900040325     C*
029000040325     C                   TESTN                   a_vatnsp             30
029100040325     C  N30              TESTN                   a_vatnsp               30
029200040325     C  N30              TESTN                   a_vatnsp                 30
029300040325     C                   MOVE      a_vatnsp      W001A             1
029400040325     C     *IN30         IFEQ      *OFF
029500040325     C     W001A         ORLT      '0'
029600040325     C                   SETON                                        31
029700040325     C                   Z-ADD     *zeros        VATNSP
029800040325     C                   EVAL      vinmsg = %trimr(vinmsg)
029900040325     C                             + ' ' + 'VATNSP'
030000040325     C                   ELSE
030100040325     C                   MOVE      a_vatnsp      VATNSP
030200040325     C                   ENDIF
030300040325     C*
030400040325     C*
030500040325     C* ALFABETICI
030600040325     C                   MOVE      a_vatatb      VATATB
030700040325     C                   MOVE      a_vattrc      VATTRC
030800131018     C                   IF        a_vattrc <> 'S'
030900040325     C                   EVAL      VATNOT = %trim(a_vatnot)
031000131023     C                   ELSE
031100131023     C                   EVAL      VATNOT = a_vatnot
031200131018     C                   ENDIF
031300040325     C*
031400040325     C                   ENDSR
031500040325     C*----------------------------------------------------*
031600990921
031700010531
031800010531
031900010531
032000010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
032100010531     C     repfil        BEGSR
032200010531     C*
032300010531     C                   if        invfil = *zeros and
032400010531     C                             depfil > *zeros and
032500010613     C                             (vinflg = *blanks or
032600010613     C                              vinflg = *zeros)
032700010531     C
032800010531     C                   eval      invfil = depfil
032900010531     C                   endif
033000010531     C*
033100010531     C                   if        depfil <> invfil and
033200010531     C                             invfil > *zeros
033300010531     C                   eval      flgMulti = '1'
033400010531     C                   if        vinflg = *blanks
033500010531     C                   add       1             cntNonEl
033600010531     C                   endif
033700010531     C                   endif
033800010531     C*
033900010531     C                   if        vinflg = '2'
034000010531     C                   eval      flgStato = '2'
034100010531     C                   endif
034200010531     C*
034300010531     C                   ENDSR
034400010531     C***
034500010531
034600010531
034700010531
034800010531
034900010531
035000990921      /TITLE Invio dei dati al punto operativo.
035100000613     C     invio         BEGSR
035200990921     C*
035300990921     C                   reset                   dscmz
035400030618     C                   move      invfil        cmzdst
035500030618     C***                move      '789'         cmzdst
035600990921     C                   eval      cmzfld = vlrfou
035700990921     C                   eval      cmzmbd = vlrhdl
035800990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
035900000710     C                   if        prmfir = *blanks
036000040325     C                   eval      cmzfla = 'EDIVAT0F'
036100040325     C                   eval      cmzmba = 'EDIVAT0F'
036200000710     C                   else
036300000710     C                   eval      cmzfla = prmfir
036400000710     C                   eval      cmzmba = prmfir
036500000710     C                   endif
036600990921     C                   eval      cmznrr = *zeros
036700990921     C                   move      §ctdok        cmznrr
036800021003     C                   eval      cmzlba = vlrfl1
036900990921     C                   call(e)   'TIS711C'
037000990921     C                   parm                    dscmz
037100990921     C                   parm      *blanks       esito
037200990921     C                   if        %error
037300990921     C                             or cmzerr = '1'
037400990921     C                             or esito  = '1'
037500000710     C                   eval      wrkesito = '3'
037600990921     C                   endif
037700990921     C*
037800000613     C                   ENDSR
037900000613     C***
038000990914
038100010531
038200010531
038300010531
038400010531      /TITLE Invio dei dati al punto operativo.
038500010531     C     opeini        BEGSR
038600010531     C*
038700010531     C* Inizializzo flag e contatori operativi
038800010531     C                   movel     '0'           flgGiro           1
038900010531     C                   movel     '0'           flgMulti          1
039000010531     C                   movel     '1'           flgStato          1
039100010615     C                   movel     '0'           flgOk             1
039200010531     C                   z-add     *zeros        cntNonEl         10 0
039300010531     C                   z-add     *zeros        depfil            3 0
039400010531     C                   z-add     *zeros        invfil            3 0
039500030407     C*
039600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
039700030407     C                   if        %subst(vlrppt:1:1) = 'S'
039800030407     C                   seton                                        50
039900030407     C                   else
040000030407     C                   setoff                                       50
040100030407     C                   endif
040200010531     C*
040300010531     C                   ENDSR
040400010531     C***
040500010531
040600010531
040700010531
040800010531
040900000613     C     *inzsr        BEGSR
041000990914     C*
041100990914     C     *entry        plist
041200990921     C                   parm                    tivlrds
041300990921     C                   parm      wrkesito      esito
041400000724     C                   parm                    prmlit
041500000710     C                   parm                    prmfir
041600120313     C*
041700120313     C* SE RICHIESTO NEI PARAMETRI DI TRADUZIONE
041800120313     C* REPERISCO SUBITO IL NOME DEL FILE "UPLOADATO" (SE C'E') E REPERISCO LA FILIALE A CUI INVIARE
041900120313     C                   MOVE(P)   vlrMSG        wNomeFile
042000120313     C                   IF        %subst(vlrPPT:2:1) = 'S'
042100120313     C                   SELECT
042200120313     C                   WHEN      %subst(wNomeFile:7:2) = '60' OR
042300120313     C                             %subst(wNomeFile:7:2) = '61' OR
042400120313     C                             %subst(wNomeFile:7:2) = '62' OR
042500120313     C                             %subst(wNomeFile:7:2) = '63' OR
042600120313     C                             %subst(wNomeFile:7:2) = '64' OR
042700120313     C                             %subst(wNomeFile:7:2) = '65' OR
042800120313     C                             %subst(wNomeFile:7:2) = '66' OR
042900120313     C                             %subst(wNomeFile:7:2) = '67' OR
043000120313     C                             %subst(wNomeFile:7:2) = '68' OR
043100120313     C                             %subst(wNomeFile:7:2) = '69'
043200120313     C                   EVAL      vlrpoi = 999
043300120313     C                   WHEN      %subst(wNomeFile:7:2) = '99'
043400120313     C                   EVAL      vlrpoi = 048
043500120313     C                   WHEN      %subst(wNomeFile:7:2) = '01' OR
043600120313     C                             %subst(wNomeFile:7:2) = '02' OR
043700120313     C                             %subst(wNomeFile:7:2) = '03' OR
043800120313     C                             %subst(wNomeFile:7:2) = '04' OR
043900120313     C                             %subst(wNomeFile:7:2) = '13' OR
044000120313     C                             %subst(wNomeFile:7:2) = '24' OR
044100120313     C                             %subst(wNomeFile:7:2) = '25' OR
044200120313     C                             %subst(wNomeFile:7:2) = '55'
044300120313     C                   EVAL      vlrpoi = 277
044400130220     C                   WHEN      %subst(wNomeFile:7:2) = '95' OR
044500130220     C                             %subst(wNomeFile:7:2) = '96'
044600130220     C                   EVAL      vlrpoi = 043
044700120313     C                   OTHER
044800120313     C                   EVAL      vlrpoi = 043
044900120313     C                   ENDSL
045000120313     C                   ENDIF
045100120313     C*
045200120313     C* CALCOLA LA DATA CORRENTE
045300120313     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
045400120313     C                   eval      datcor = %dec(%date() : *ISO)
045500000613     C*
045600000613     C                   ENDSR
045700000613     C***
