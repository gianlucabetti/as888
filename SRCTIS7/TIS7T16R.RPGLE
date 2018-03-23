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
013000150903     C* FORZATURA PER SPOSTAMENTO WURTH A ROVERETO
013100150903     C                   if        VATFGS = 043
013200150903     C                   eval      VATFGS = 172
013300150903     C                   endif
013400150903     C*
013500021014     C                   else
013600040325     C                   MOVE(p)   vlrpoi        VATFGS
013700021014     C                   endif
013800010531     C*
013900010531     C                   exsr      PREELA
014000010531     C*
014100120313     C* VALORIZZO CAMPI RELATIVI AL "CMR"
014200120313     C                   EVAL      VATCMR = 'WURTH - ' + %subst(wNomeFile:4:5)
014300120313     C                   EVAL      VATCNT = 1
014400010601     C*
014500010604     C  N31              ADD       1             §CTDOK            7 0
014600010601     C   32
014700010604     COR 31              ADD       1             §CTDNO            7 0
014800040325     C  N31              WRITE     EDIVAT00
014900010601     C*
015000010601     C                   if        *in31 = *off and
015100010601     C                             *in32 = *off
015200990914     C                   eval      vinflg = '1'
015300990914     C                   else
015400990914     C                   eval      vinflg = '2'
015500990914     C                   endif
015600000905     C                   endif
015700000905     C                   endif
015800010601     C*
015900010601     C                   endif
016000000905     C*
016100000905     C                   else
016200000905     C                   eval      vinflg = '1'
016300000905     C                   endif
016400000905     C*
016500000905     C  N70              update    tivin000
016600991022     C*
016700991022     C  N70              ENDdo
016800010531     C
016900010531     C*
017000010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017100010531     C                   if        cntNonEl = *zeros or
017200010531     C                             flgMulti = '0'
017300990914     C* Se non ci sono record con errori ...
017400010531     C                   if        §ctdno = 0 and
017500010531     C                             flgStato <> '2'
017600990914     C* ... restituisco esito OK.
017700990921     C                   eval      wrkesito = '0'
017800990914     C                   else
017900000710     C                   if        §ctdok > 0
018000990921     C                   eval      wrkesito = '1'
018100000710     C                   else
018200010615     C                   if        flgOk = '0'
018300010615     C                   eval      wrkesito = '2'
018400010615     C                   else
018500010615     C                   eval      wrkesito = '6'
018600010615     C                   endif
018700990914     C                   endif
018800000710     C                   endif
018900010531     C                   else
019000010531     C                   eval      wrkesito = '9'
019100010531     C                   endif
019200990914     C*
019300990914     C                   if        %open(tivin00r)
019400990914     C                   close     tivin00r
019500990914     C                   endif
019600040325     C                   if        %open(edivatwr)
019700040325     C                   close     edivatwr
019800990914     C                   endif
019900010531     C*
020000010531     C                   if        vlrpoi <> 999
020100010531     C                   eval      invfil = vlrpoi
020200010531     C                   endif
020300990914     C*
020400990921     C                   if        §ctdok > 0
020500010531     C                             and invfil > *zeros
020600990921     C                   exsr      invio
020700990921     C                   endif
020800010612     C*
020900010612     C                   if        flgGiro = '1'
021000010612     C                   exsr      endela
021100010612     C                   endif
021200990921     C*
021300990914     C                   ENDSR
021400000613     C***
021500040325
021600040325
021700040325
021800040325
021900040325     C*----------------------------------------------------*
022000040325     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
022100040325     C*----------------------------------------------------*
022200040325     C     IMPVAT        BEGSR
022300040325     C*
022400040325     C                   SETOFF                                       3132
022500040325     C*
022600040325     C                   SETOFF                                       303132
022700040325     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
022800040325     C                   TESTN                   a_vatccm             30
022900040325     C  N30              TESTN                   a_vatccm               30
023000040325     C  N30              TESTN                   a_vatccm                 30
023100040325     C                   MOVE      a_vatccm      W001A             1
023200040325     C     *IN30         IFEQ      *OFF
023300040325     C     W001A         ORLT      '0'
023400040325     C                   SETON                                        31
023500040325     C                   Z-ADD     *zeros        VATCCM
023600040325     C                   EVAL      vinmsg = 'VATCCM'
023700040325     C                   ELSE
023800040325     C                   MOVE      a_vatccm      VATCCM
023900040325     C     VATCCM        IFLE      *ZEROS
024000040325     C                   SETON                                        31
024100040325     C                   Z-ADD     *zeros        VATCCM
024200040325     C                   EVAL      vinmsg = 'VATCCM'
024300040325     C                   ENDIF
024400040325     C                   ENDIF
024500040325     C*
024600040325     C                   TESTN                   a_vatlnp             30
024700040325     C  N30              TESTN                   a_vatlnp               30
024800040325     C  N30              TESTN                   a_vatlnp                 30
024900040325     C                   MOVE      a_vatlnp      W001A             1
025000040325     C     *IN30         IFEQ      *OFF
025100040325     C     W001A         ORLT      '0'
025200040325     C                   SETON                                        31
025300040325     C                   Z-ADD     *zeros        VATLNP
025400040325     C                   EVAL      vinmsg = %trimr(vinmsg)
025500040325     C                             + ' ' + 'VATLNP'
025600040325     C                   ELSE
025700040325     C                   MOVE      a_vatlnp      VATLNP
025800040325     C     VATLNP        IFLE      *ZEROS
025900040325     C                   SETON                                        31
026000040325     C                   Z-ADD     *zeros        VATLNP
026100040325     C                   EVAL      vinmsg = %trimr(vinmsg)
026200040325     C                             + ' ' + 'VATLNP'
026300040325     C                   ENDIF
026400040325     C                   ENDIF
026500040325     C*
026600040325     C                   TESTN                   a_vataas             30
026700040325     C  N30              TESTN                   a_vataas               30
026800040325     C  N30              TESTN                   a_vataas                 30
026900040325     C                   MOVE      a_vataas      W001A             1
027000040325     C     *IN30         IFEQ      *OFF
027100040325     C     W001A         ORLT      '0'
027200040325     C                   SETON                                        31
027300040325     C                   MOVEL(P)  datcor        VATAAS
027400040325     C                   EVAL      vinmsg = %trimr(vinmsg)
027500040325     C                             + ' ' + 'VATAAS'
027600040325     C                   ELSE
027700040325     C                   MOVE      a_vataas      VATAAS
027800040325     C                   ENDIF
027900040325     C*
028000040325     C                   TESTN                   a_vatnrs             30
028100040325     C  N30              TESTN                   a_vatnrs               30
028200040325     C  N30              TESTN                   a_vatnrs                 30
028300040325     C                   MOVE      a_vatnrs      W001A             1
028400040325     C     *IN30         IFEQ      *OFF
028500040325     C     W001A         ORLT      '0'
028600040325     C     W001A         ANDNE     *BLANK
028700040325     C                   SETON                                        31
028800040325     C                   Z-ADD     *zeros        VATNRS
028900040325     C                   EVAL      vinmsg = %trimr(vinmsg)
029000040325     C                             + ' ' + 'VATNRS'
029100040325     C                   ELSE
029200040325     C                   MOVE      a_vatnrs      VATNRS
029300040325     C                   ENDIF
029400040325     C*
029500040325     C                   TESTN                   a_vatnsp             30
029600040325     C  N30              TESTN                   a_vatnsp               30
029700040325     C  N30              TESTN                   a_vatnsp                 30
029800040325     C                   MOVE      a_vatnsp      W001A             1
029900040325     C     *IN30         IFEQ      *OFF
030000040325     C     W001A         ORLT      '0'
030100040325     C                   SETON                                        31
030200040325     C                   Z-ADD     *zeros        VATNSP
030300040325     C                   EVAL      vinmsg = %trimr(vinmsg)
030400040325     C                             + ' ' + 'VATNSP'
030500040325     C                   ELSE
030600040325     C                   MOVE      a_vatnsp      VATNSP
030700040325     C                   ENDIF
030800040325     C*
030900040325     C*
031000040325     C* ALFABETICI
031100040325     C                   MOVE      a_vatatb      VATATB
031200040325     C                   MOVE      a_vattrc      VATTRC
031300131018     C                   IF        a_vattrc <> 'S'
031400040325     C                   EVAL      VATNOT = %trim(a_vatnot)
031500131023     C                   ELSE
031600131023     C                   EVAL      VATNOT = a_vatnot
031700131018     C                   ENDIF
031800040325     C*
031900040325     C                   ENDSR
032000040325     C*----------------------------------------------------*
032100990921
032200010531
032300010531
032400010531
032500010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
032600010531     C     repfil        BEGSR
032700010531     C*
032800010531     C                   if        invfil = *zeros and
032900010531     C                             depfil > *zeros and
033000010613     C                             (vinflg = *blanks or
033100010613     C                              vinflg = *zeros)
033200010531     C
033300010531     C                   eval      invfil = depfil
033400010531     C                   endif
033500010531     C*
033600010531     C                   if        depfil <> invfil and
033700010531     C                             invfil > *zeros
033800010531     C                   eval      flgMulti = '1'
033900010531     C                   if        vinflg = *blanks
034000010531     C                   add       1             cntNonEl
034100010531     C                   endif
034200010531     C                   endif
034300010531     C*
034400010531     C                   if        vinflg = '2'
034500010531     C                   eval      flgStato = '2'
034600010531     C                   endif
034700010531     C*
034800010531     C                   ENDSR
034900010531     C***
035000010531
035100010531
035200010531
035300010531
035400010531
035500990921      /TITLE Invio dei dati al punto operativo.
035600000613     C     invio         BEGSR
035700990921     C*
035800990921     C                   reset                   dscmz
035900030618     C                   move      invfil        cmzdst
036000030618     C***                move      '789'         cmzdst
036100990921     C                   eval      cmzfld = vlrfou
036200990921     C                   eval      cmzmbd = vlrhdl
036300990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
036400000710     C                   if        prmfir = *blanks
036500040325     C                   eval      cmzfla = 'EDIVAT0F'
036600040325     C                   eval      cmzmba = 'EDIVAT0F'
036700000710     C                   else
036800000710     C                   eval      cmzfla = prmfir
036900000710     C                   eval      cmzmba = prmfir
037000000710     C                   endif
037100990921     C                   eval      cmznrr = *zeros
037200990921     C                   move      §ctdok        cmznrr
037300021003     C                   eval      cmzlba = vlrfl1
037400990921     C                   call(e)   'TIS711C'
037500990921     C                   parm                    dscmz
037600990921     C                   parm      *blanks       esito
037700990921     C                   if        %error
037800990921     C                             or cmzerr = '1'
037900990921     C                             or esito  = '1'
038000000710     C                   eval      wrkesito = '3'
038100990921     C                   endif
038200990921     C*
038300000613     C                   ENDSR
038400000613     C***
038500990914
038600010531
038700010531
038800010531
038900010531      /TITLE Invio dei dati al punto operativo.
039000010531     C     opeini        BEGSR
039100010531     C*
039200010531     C* Inizializzo flag e contatori operativi
039300010531     C                   movel     '0'           flgGiro           1
039400010531     C                   movel     '0'           flgMulti          1
039500010531     C                   movel     '1'           flgStato          1
039600010615     C                   movel     '0'           flgOk             1
039700010531     C                   z-add     *zeros        cntNonEl         10 0
039800010531     C                   z-add     *zeros        depfil            3 0
039900010531     C                   z-add     *zeros        invfil            3 0
040000030407     C*
040100030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
040200030407     C                   if        %subst(vlrppt:1:1) = 'S'
040300030407     C                   seton                                        50
040400030407     C                   else
040500030407     C                   setoff                                       50
040600030407     C                   endif
040700010531     C*
040800010531     C                   ENDSR
040900010531     C***
041000010531
041100010531
041200010531
041300010531
041400000613     C     *inzsr        BEGSR
041500990914     C*
041600990914     C     *entry        plist
041700990921     C                   parm                    tivlrds
041800990921     C                   parm      wrkesito      esito
041900000724     C                   parm                    prmlit
042000000710     C                   parm                    prmfir
042100120313     C*
042200120313     C* SE RICHIESTO NEI PARAMETRI DI TRADUZIONE
042300120313     C* REPERISCO SUBITO IL NOME DEL FILE "UPLOADATO" (SE C'E') E REPERISCO LA FILIALE A CUI INVIARE
042400120313     C                   MOVE(P)   vlrMSG        wNomeFile
042500120313     C                   IF        %subst(vlrPPT:2:1) = 'S'
042600120313     C                   SELECT
042700120313     C                   WHEN      %subst(wNomeFile:7:2) = '60' OR
042800120313     C                             %subst(wNomeFile:7:2) = '61' OR
042900120313     C                             %subst(wNomeFile:7:2) = '62' OR
043000120313     C                             %subst(wNomeFile:7:2) = '63' OR
043100120313     C                             %subst(wNomeFile:7:2) = '64' OR
043200120313     C                             %subst(wNomeFile:7:2) = '65' OR
043300120313     C                             %subst(wNomeFile:7:2) = '66' OR
043400120313     C                             %subst(wNomeFile:7:2) = '67' OR
043500120313     C                             %subst(wNomeFile:7:2) = '68' OR
043600120313     C                             %subst(wNomeFile:7:2) = '69'
043700150903     C                   EVAL      vlrpoi = 999
043800120313     C                   WHEN      %subst(wNomeFile:7:2) = '99'
043900120313     C                   EVAL      vlrpoi = 048
044000120313     C                   WHEN      %subst(wNomeFile:7:2) = '01' OR
044100120313     C                             %subst(wNomeFile:7:2) = '02' OR
044200120313     C                             %subst(wNomeFile:7:2) = '03' OR
044300120313     C                             %subst(wNomeFile:7:2) = '04' OR
044400120313     C                             %subst(wNomeFile:7:2) = '13' OR
044500120313     C                             %subst(wNomeFile:7:2) = '24' OR
044600120313     C                             %subst(wNomeFile:7:2) = '25' OR
044700120313     C                             %subst(wNomeFile:7:2) = '55'
044800120313     C                   EVAL      vlrpoi = 277
044900130220     C                   WHEN      %subst(wNomeFile:7:2) = '95' OR
045000130220     C                             %subst(wNomeFile:7:2) = '96'
045100150831     C                   EVAL      vlrpoi = 172
045200120313     C                   OTHER
045300150831     C                   EVAL      vlrpoi = 172
045400120313     C                   ENDSL
045500120313     C                   ENDIF
045600120313     C*
045700120313     C* CALCOLA LA DATA CORRENTE
045800120313     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
045900120313     C                   eval      datcor = %dec(%date() : *ISO)
046000000613     C*
046100000613     C                   ENDSR
046200000613     C***
