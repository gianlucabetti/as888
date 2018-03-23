000100040325      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200990914     H dftactgrp(*yes)
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
001700040325     D*
001800040325     D fnvataar        ds                  inz
001900040325     D  a_vatatb               1      1
002000040325     D  a_vatccm               3      9
002100040325     D  a_vatlnp              11     13
002200040325     D  a_vataas              15     18
002300040325     D  a_vatnrs              20     21
002400040325     D  a_vatnsp              23     29
002500040325     D  a_vattrc              30     30
002600040325     D  a_vatnot              31     65
002700010606
002800110413
002900110413     D*------------------
003000110413     D* DEFINIZIONE DS DI LETTURA VIA SWL ARCHIVIO FNVAB
003100110413     D*------------------
003200110413     D EDIVAB_DS       DS                  INZ
003300110415     D  VABCMR                       35
003400110413     D  CONTEGGIO                     9  0
003500110413     D*
003600110413
003700010606
003800990914
003900120309     C*
004000120309     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004100120309     C
004200120309     C/EXEC SQL
004300120309     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
004400120309     C/END-EXEC
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
008400010531     C*
008500000616     C*
008600010531     C                   ENDSR
008700010531     C***
008800990914
008900010531
009000010531
009100010531
009200010531
009300990914     C*--------------------------------------------------------
009400040325     C* RWVAT  LEGGE TIVIN00R E SCRIVE EDIVAT0F              *
009500990914     C*--------------------------------------------------------
009600040325     C     RWVAT         BEGSR
009700030710     C*
009800030710     C                   clear                   §CTDOK
009900030710     C                   clear                   §CTDNO
010000990914     C*
010100990914     C                   if        not %open(tivin00r)
010200990914     C                   open      tivin00r
010300990914     C                   endif
010400040325     C                   if        not %open(edivatwr)
010500040325     C                   open      edivatwr
010600990914     C                   endif
010700990914     C*
010800990914     C                   DO        *HIVAL
010900010618     C*
011000990914     C                   READ      tivin00r                               70
011100010618     C*
011200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011300010618     C                   if        vinflg = '1'
011400010618     C                   eval      flgOk = '1'
011500010618     C                   endif
011600010618     C*
011700000905     C                   if        vindta > *blanks
011800000613     C                   add       1             rrnum
011900990914     C*
012000010601     C                   if        *in70 = *off and
012100990914     C                             (vinflg = *blanks
012200010711     C                              or vinflg = '0'
012300010711     C                              or vinflg = '2')
012400990914     C*
012500040325     C                   clear                   EDIVAT00
012600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012700010711     C                   if        vinflg = *blanks or vinflg = '0'
012800010711     C                   clear                   vinmsg
012900010711     C                   endif
013000010711     C*
013100040325     C                   eval      fnvataar = vindta
013200040325     C*
013300040325     C                   testb     x'3F'         a_vatatb                 02
013400040325     C                   if        not *in02
013500991022     C*
013600040325     C                   exsr      impvat
013700010531     C*
013800010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013900040325     C                   eval      depfil = VATLNP
014000010531     C                   exsr      repfil
014100010601     C                   if        depfil = invfil
014200021014     C                   if        vlrpoi = 999
014300040325     C                   MOVE(p)   invfil        VATFGS
014400021014     C                   else
014500040325     C                   MOVE(p)   vlrpoi        VATFGS
014600021014     C                   endif
014700010531     C*
014800010531     C                   exsr      PREELA
014900110413     C*
015000110413     C* Gestione partcolare x tentativo match su EDIVAB
015100110413     C*
015200110413     C/EXEC SQL
015300110413     C+ declare C1 cursor for
015400110415     C+ select vabcmr, count(*)
015500110413     C+ from filtragru/edivab0f
015600110413     C+ where vabccm = :VATCCM and vabaas = :VATAAS and vablnp = :VATLNP
015700110413     C+ and vabnrs = :VATNRS and vabnsp = :VATNSP
015800110413     C+ group by
015900110415     C+ vabccm, vabaas, vablnp, vabnrs, vabnsp, vabcmr
016000110413     C+ for read only
016100110413     C/END-EXEC
016200110413     C
016300110413     C/EXEC SQL
016400110413     C+ open C1
016500110413     C/END-EXEC
016600110413     C
016700110413     C/EXEC SQL
016800110413     C+ Fetch C1 into :EDIVAB_DS
016900110413     C/END-EXEC
017000110413     C
017100110413     C/EXEC SQL
017200110413     C+ close C1
017300110413     C/END-EXEC
017400110413     C*
017500110413     C                   if        conteggio = 1
017600010531     C*
017700030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
017800110415     C                   EVAL      VATCMR = VABCMR
017900040325     C                   EVAL      VATCNT = 1
018000010601     C*
018100010604     C  N31              ADD       1             §CTDOK            7 0
018200010601     C   32
018300010604     COR 31              ADD       1             §CTDNO            7 0
018400040325     C  N31              WRITE     EDIVAT00
018500110413     C*
018600010601     C                   if        *in31 = *off and
018700010601     C                             *in32 = *off
018800990914     C                   eval      vinflg = '1'
018900990914     C                   else
019000990914     C                   eval      vinflg = '2'
019100990914     C                   endif
019200110413     C*
019300110413     C* ---> match fallito su EDIVAB
019400110413     C                   else
019500110413     C                   eval      cntNonEl = *hival
019600110413     C                   eval      flgMulti = '1'
019700110413     C                   endif
019800110413     C*
019900000905     C                   endif
020000000905     C                   endif
020100010601     C*
020200010601     C                   endif
020300000905     C*
020400000905     C                   else
020500000905     C                   eval      vinflg = '1'
020600000905     C                   endif
020700000905     C*
020800000905     C  N70              update    tivin000
020900991022     C*
021000991022     C  N70              ENDdo
021100010531     C
021200010531     C*
021300010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
021400010531     C                   if        cntNonEl = *zeros or
021500010531     C                             flgMulti = '0'
021600990914     C* Se non ci sono record con errori ...
021700010531     C                   if        §ctdno = 0 and
021800010531     C                             flgStato <> '2'
021900990914     C* ... restituisco esito OK.
022000990921     C                   eval      wrkesito = '0'
022100990914     C                   else
022200000710     C                   if        §ctdok > 0
022300990921     C                   eval      wrkesito = '1'
022400000710     C                   else
022500010615     C                   if        flgOk = '0'
022600010615     C                   eval      wrkesito = '2'
022700010615     C                   else
022800010615     C                   eval      wrkesito = '6'
022900010615     C                   endif
023000990914     C                   endif
023100000710     C                   endif
023200010531     C                   else
023300010531     C                   eval      wrkesito = '9'
023400010531     C                   endif
023500990914     C*
023600990914     C                   if        %open(tivin00r)
023700990914     C                   close     tivin00r
023800990914     C                   endif
023900040325     C                   if        %open(edivatwr)
024000040325     C                   close     edivatwr
024100990914     C                   endif
024200010531     C*
024300010531     C                   if        vlrpoi <> 999
024400010531     C                   eval      invfil = vlrpoi
024500010531     C                   endif
024600990914     C*
024700990921     C                   if        §ctdok > 0
024800010531     C                             and invfil > *zeros
024900990921     C                   exsr      invio
025000990921     C                   endif
025100010612     C*
025200010612     C                   if        flgGiro = '1'
025300010612     C                   exsr      endela
025400010612     C                   endif
025500990921     C*
025600990914     C                   ENDSR
025700000613     C***
025800040325
025900040325
026000040325
026100040325
026200040325     C*----------------------------------------------------*
026300040325     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
026400040325     C*----------------------------------------------------*
026500040325     C     IMPVAT        BEGSR
026600040325     C*
026700040325     C                   SETOFF                                       3132
026800040325     C*
026900040325     C                   SETOFF                                       303132
027000040325     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
027100040325     C                   TESTN                   a_vatccm             30
027200040325     C  N30              TESTN                   a_vatccm               30
027300040325     C  N30              TESTN                   a_vatccm                 30
027400040325     C                   MOVE      a_vatccm      W001A             1
027500040325     C     *IN30         IFEQ      *OFF
027600040325     C     W001A         ORLT      '0'
027700040325     C                   SETON                                        31
027800040325     C                   Z-ADD     *zeros        VATCCM
027900040325     C                   EVAL      vinmsg = 'VATCCM'
028000040325     C                   ELSE
028100040325     C                   MOVE      a_vatccm      VATCCM
028200040325     C     VATCCM        IFLE      *ZEROS
028300040325     C                   SETON                                        31
028400040325     C                   Z-ADD     *zeros        VATCCM
028500040325     C                   EVAL      vinmsg = 'VATCCM'
028600040325     C                   ENDIF
028700040325     C                   ENDIF
028800040325     C*
028900040325     C                   TESTN                   a_vatlnp             30
029000040325     C  N30              TESTN                   a_vatlnp               30
029100040325     C  N30              TESTN                   a_vatlnp                 30
029200040325     C                   MOVE      a_vatlnp      W001A             1
029300040325     C     *IN30         IFEQ      *OFF
029400040325     C     W001A         ORLT      '0'
029500040325     C                   SETON                                        31
029600040325     C                   Z-ADD     *zeros        VATLNP
029700040325     C                   EVAL      vinmsg = %trimr(vinmsg)
029800040325     C                             + ' ' + 'VATLNP'
029900040325     C                   ELSE
030000040325     C                   MOVE      a_vatlnp      VATLNP
030100040325     C     VATLNP        IFLE      *ZEROS
030200040325     C                   SETON                                        31
030300040325     C                   Z-ADD     *zeros        VATLNP
030400040325     C                   EVAL      vinmsg = %trimr(vinmsg)
030500040325     C                             + ' ' + 'VATLNP'
030600040325     C                   ENDIF
030700040325     C                   ENDIF
030800040325     C*
030900040325     C                   TESTN                   a_vataas             30
031000040325     C  N30              TESTN                   a_vataas               30
031100040325     C  N30              TESTN                   a_vataas                 30
031200040325     C                   MOVE      a_vataas      W001A             1
031300040325     C     *IN30         IFEQ      *OFF
031400040325     C     W001A         ORLT      '0'
031500040325     C                   SETON                                        31
031600040325     C                   MOVEL(P)  datcor        VATAAS
031700040325     C                   EVAL      vinmsg = %trimr(vinmsg)
031800040325     C                             + ' ' + 'VATAAS'
031900040325     C                   ELSE
032000040325     C                   MOVE      a_vataas      VATAAS
032100040325     C                   ENDIF
032200040325     C*
032300040325     C                   TESTN                   a_vatnrs             30
032400040325     C  N30              TESTN                   a_vatnrs               30
032500040325     C  N30              TESTN                   a_vatnrs                 30
032600040325     C                   MOVE      a_vatnrs      W001A             1
032700040325     C     *IN30         IFEQ      *OFF
032800040325     C     W001A         ORLT      '0'
032900040325     C     W001A         ANDNE     *BLANK
033000040325     C                   SETON                                        31
033100040325     C                   Z-ADD     *zeros        VATNRS
033200040325     C                   EVAL      vinmsg = %trimr(vinmsg)
033300040325     C                             + ' ' + 'VATNRS'
033400040325     C                   ELSE
033500040325     C                   MOVE      a_vatnrs      VATNRS
033600040325     C                   ENDIF
033700040325     C*
033800040325     C                   TESTN                   a_vatnsp             30
033900040325     C  N30              TESTN                   a_vatnsp               30
034000040325     C  N30              TESTN                   a_vatnsp                 30
034100040325     C                   MOVE      a_vatnsp      W001A             1
034200040325     C     *IN30         IFEQ      *OFF
034300040325     C     W001A         ORLT      '0'
034400040325     C                   SETON                                        31
034500040325     C                   Z-ADD     *zeros        VATNSP
034600040325     C                   EVAL      vinmsg = %trimr(vinmsg)
034700040325     C                             + ' ' + 'VATNSP'
034800040325     C                   ELSE
034900040325     C                   MOVE      a_vatnsp      VATNSP
035000040325     C                   ENDIF
035100040325     C*
035200040325     C*
035300040325     C* ALFABETICI
035400040325     C                   MOVE      a_vatatb      VATATB
035500040325     C                   MOVE      a_vattrc      VATTRC
035600040325     C                   EVAL      VATNOT = %trim(a_vatnot)
035700040325     C*
035800040325     C                   ENDSR
035900040325     C*----------------------------------------------------*
036000990921
036100010531
036200010531
036300010531
036400010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
036500010531     C     repfil        BEGSR
036600010531     C*
036700010531     C                   if        invfil = *zeros and
036800010531     C                             depfil > *zeros and
036900010613     C                             (vinflg = *blanks or
037000010613     C                              vinflg = *zeros)
037100010531     C
037200010531     C                   eval      invfil = depfil
037300010531     C                   endif
037400010531     C*
037500010531     C                   if        depfil <> invfil and
037600010531     C                             invfil > *zeros
037700010531     C                   eval      flgMulti = '1'
037800010531     C                   if        vinflg = *blanks
037900010531     C                   add       1             cntNonEl
038000010531     C                   endif
038100010531     C                   endif
038200010531     C*
038300010531     C                   if        vinflg = '2'
038400010531     C                   eval      flgStato = '2'
038500010531     C                   endif
038600010531     C*
038700010531     C                   ENDSR
038800010531     C***
038900010531
039000010531
039100010531
039200010531
039300010531
039400990921      /TITLE Invio dei dati al punto operativo.
039500000613     C     invio         BEGSR
039600990921     C*
039700990921     C                   reset                   dscmz
039800030618     C                   move      invfil        cmzdst
039900030618     C***                move      '789'         cmzdst
040000990921     C                   eval      cmzfld = vlrfou
040100990921     C                   eval      cmzmbd = vlrhdl
040200990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
040300000710     C                   if        prmfir = *blanks
040400040325     C                   eval      cmzfla = 'EDIVAT0F'
040500040325     C                   eval      cmzmba = 'EDIVAT0F'
040600000710     C                   else
040700000710     C                   eval      cmzfla = prmfir
040800000710     C                   eval      cmzmba = prmfir
040900000710     C                   endif
041000990921     C                   eval      cmznrr = *zeros
041100990921     C                   move      §ctdok        cmznrr
041200021003     C                   eval      cmzlba = vlrfl1
041300990921     C                   call(e)   'TIS711C'
041400990921     C                   parm                    dscmz
041500990921     C                   parm      *blanks       esito
041600990921     C                   if        %error
041700990921     C                             or cmzerr = '1'
041800990921     C                             or esito  = '1'
041900000710     C                   eval      wrkesito = '3'
042000990921     C                   endif
042100990921     C*
042200000613     C                   ENDSR
042300000613     C***
042400990914
042500010531
042600010531
042700010531
042800010531      /TITLE Invio dei dati al punto operativo.
042900010531     C     opeini        BEGSR
043000010531     C*
043100010531     C* Inizializzo flag e contatori operativi
043200010531     C                   movel     '0'           flgGiro           1
043300010531     C                   movel     '0'           flgMulti          1
043400010531     C                   movel     '1'           flgStato          1
043500010615     C                   movel     '0'           flgOk             1
043600010531     C                   z-add     *zeros        cntNonEl         10 0
043700010531     C                   z-add     *zeros        depfil            3 0
043800010531     C                   z-add     *zeros        invfil            3 0
043900030407     C*
044000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
044100030407     C                   if        %subst(vlrppt:1:1) = 'S'
044200030407     C                   seton                                        50
044300030407     C                   else
044400030407     C                   setoff                                       50
044500030407     C                   endif
044600010531     C*
044700010531     C                   ENDSR
044800010531     C***
044900010531
045000010531
045100010531
045200010531
045300000613     C     *inzsr        BEGSR
045400990914     C*
045500990914     C     *entry        plist
045600990921     C                   parm                    tivlrds
045700990921     C                   parm      wrkesito      esito
045800000724     C                   parm                    prmlit
045900000710     C                   parm                    prmfir
046000110413     C*
046100110413     C* CALCOLA LA DATA CORRENTE
046200110413     C                   time                    wn14             14 0
046300110413     C                   movel     wn14          oracor            6 0          *ORA
046400110413     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
046500110413     C                   eval      datcor = %dec(%date() : *ISO)
046600000613     C*
046700000613     C                   ENDSR
046800000613     C***
