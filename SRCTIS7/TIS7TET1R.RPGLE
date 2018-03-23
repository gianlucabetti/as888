000100040325      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
002300040325     D*
002400040325     D fnvataar        ds                  inz
002500050614     D  a_vatatb                      1
002600050614     D  a_vatccm                      7
002700050614     D  a_vatlnp                      3
002800050614     D  a_vataas                      4
002900050614     D  a_vatnrs                      2
003000050614     D  a_vatnsp                      7
003100050614     D  a_vattrc                      1
003200050614     D  a_vatnot                     35
003300010606
003400100309     D*-------------------
003500100309     D* COSTANTI
003600100309     D*-------------------
003700100309     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
003800100309     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
003900100309
004000100309
004100100309     D*------------------
004200100309     D* LINKING A DEFINIZIONI ESTERNE
004300100309     D*------------------
004400100309     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004500100309     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004600100309
004700100309
004800010606
004900010606
005000990914
005100000913     C                   reset                   rrnum
005200990921     C                   reset                   esito
005300990921     C                   reset                   wrkesito
005400010531     C*
005500010531     C                   exsr      opeini
005600040325     C                   exsr      rwvat
005700010531     C*
005800010531     C                   seton                                        lr
005900010531
006000010531
006100010531
006200010531
006300010531     C*--------------------------------------------------------
006400010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006500010531     C*--------------------------------------------------------
006600010531     C     PREELA        BEGSR
006700000724     C*
006800000724     C* SE OCCORRE SPEDIRE IN FILIALE
006900010531     C                   if        invfil <> *zeros and
007000010531     C                             flgGiro = '0'
007100010531     C*
007200010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007300010531     C                   eval      flgGiro = '1'
007400010531     C*
007500010531     C                   endif
007600010531     C*
007700010531     C                   ENDSR
007800010531     C***
007900010531
008000010531
008100010531
008200010531
008300010531
008400010531
008500010531
008600010531     C*--------------------------------------------------------
008700010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008800010531     C*--------------------------------------------------------
008900010531     C     ENDELA        BEGSR
009000000616     C*
009100010531     C                   ENDSR
009200010531     C***
009300990914
009400010531
009500010531
009600010531
009700010531
009800990914     C*--------------------------------------------------------
009900040325     C* RWVAT  LEGGE TIVIN00R E SCRIVE EDIVAT0F              *
010000990914     C*--------------------------------------------------------
010100040325     C     RWVAT         BEGSR
010200030710     C*
010300030710     C                   clear                   §CTDOK
010400030710     C                   clear                   §CTDNO
010500990914     C*
010600030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
010700030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
010800030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
010900030710     C                   if        *in60 = *on
011000990914     C                   if        not %open(tivin00r)
011100990914     C                   open      tivin00r
011200990914     C                   endif
011300040325     C                   if        not %open(edivatwr)
011400040325     C                   open      edivatwr
011500990914     C                   endif
011600990914     C*
011700990914     C                   DO        *HIVAL
011800010618     C*
011900990914     C                   READ      tivin00r                               70
012000010618     C*
012100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012200010618     C                   if        vinflg = '1'
012300010618     C                   eval      flgOk = '1'
012400010618     C                   endif
012500010618     C*
012600000905     C                   if        vindta > *blanks
012700000613     C                   add       1             rrnum
012800990914     C*
012900010601     C                   if        *in70 = *off and
013000990914     C                             (vinflg = *blanks
013100010711     C                              or vinflg = '0'
013200010711     C                              or vinflg = '2')
013300990914     C*
013400040325     C                   clear                   EDIVAT00
013500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013600010711     C                   if        vinflg = *blanks or vinflg = '0'
013700010711     C                   clear                   vinmsg
013800010711     C                   endif
013900010711     C*
014000040325     C                   eval      fnvataar = vindta
014100040325     C*
014200040325     C                   testb     x'3F'         a_vatatb                 02
014300040325     C                   if        not *in02
014400991022     C*
014500040325     C                   exsr      impvat
014600010531     C*
014700010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014800040325     C                   eval      depfil = VATLNP
014900010531     C                   exsr      repfil
015000010601     C                   if        depfil = invfil
015100021014     C                   if        vlrpoi = 999
015200040325     C                   MOVE(p)   invfil        VATFGS
015300021014     C                   else
015400040325     C                   MOVE(p)   vlrpoi        VATFGS
015500021014     C                   endif
015600010531     C*
015700010531     C                   exsr      PREELA
015800010531     C*
015900030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
016000040325     C                   EVAL      VATCMR = wCMR
016100040325     C                   EVAL      VATCNT = 1
016200010601     C*
016300010604     C  N31              ADD       1             §CTDOK            7 0
016400010601     C   32
016500010604     COR 31              ADD       1             §CTDNO            7 0
016600040325     C  N31              WRITE     EDIVAT00
016700010601     C*
016800010601     C                   if        *in31 = *off and
016900010601     C                             *in32 = *off
017000990914     C                   eval      vinflg = '1'
017100990914     C                   else
017200990914     C                   eval      vinflg = '2'
017300990914     C                   endif
017400000905     C                   endif
017500000905     C                   endif
017600010601     C*
017700010601     C                   endif
017800000905     C*
017900000905     C                   else
018000000905     C                   eval      vinflg = '1'
018100000905     C                   endif
018200000905     C*
018300000905     C  N70              update    tivin000
018400991022     C*
018500991022     C  N70              ENDdo
018600010531     C
018700010531     C*
018800010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018900010531     C                   if        cntNonEl = *zeros or
019000010531     C                             flgMulti = '0'
019100990914     C* Se non ci sono record con errori ...
019200010531     C                   if        §ctdno = 0 and
019300010531     C                             flgStato <> '2'
019400990914     C* ... restituisco esito OK.
019500990921     C                   eval      wrkesito = '0'
019600990914     C                   else
019700000710     C                   if        §ctdok > 0
019800990921     C                   eval      wrkesito = '1'
019900000710     C                   else
020000010615     C                   if        flgOk = '0'
020100010615     C                   eval      wrkesito = '2'
020200010615     C                   else
020300010615     C                   eval      wrkesito = '6'
020400010615     C                   endif
020500990914     C                   endif
020600000710     C                   endif
020700010531     C                   else
020800010531     C                   eval      wrkesito = '9'
020900010531     C                   endif
021000030710     C*
021100030710 X  1C                   else
021200030710     C                   eval      wrkesito = '8'
021300030710     C                   eval      flgGiro = '1'
021400030710 E  1C                   endif
021500990914     C*
021600990914     C                   if        %open(tivin00r)
021700990914     C                   close     tivin00r
021800990914     C                   endif
021900040325     C                   if        %open(edivatwr)
022000040325     C                   close     edivatwr
022100990914     C                   endif
022200010531     C*
022300010531     C                   if        vlrpoi <> 999
022400010531     C                   eval      invfil = vlrpoi
022500010531     C                   endif
022600990914     C*
022700990921     C                   if        §ctdok > 0
022800010531     C                             and invfil > *zeros
022900990921     C                   exsr      invio
023000990921     C                   endif
023100010612     C*
023200010612     C                   if        flgGiro = '1'
023300010612     C                   exsr      endela
023400010612     C                   endif
023500990921     C*
023600990914     C                   ENDSR
023700000613     C***
023800040325
023900100309
024000100309
024100100309
024200100309     C*----------------------------------------------------*
024300100309     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
024400100309     C*----------------------------------------------------*
024500100309     C     IMPVAT        BEGSR
024600100309     C*
024700100309     C                   SETOFF                                       3132
024800100309     C*
024900100309     C* Reperimento campi ALFA
025000100309     C                   EVAL      VATATB=a_vatatb
025100100309     C                   EVAL      VATTRC=a_vattrc
025200131018     C                   IF        a_vattrc <> 'S'
025300100309     C                   EVAL      VATNOT=%trim(a_vatnot)
025400131023     C                   ELSE
025500131023     C                   EVAL      VATNOT=a_vatnot
025600131018     C                   ENDIF
025700100309     C*
025800100309     C                   EVAL      PiStr=%trim(a_vatccm)
025900100309     C                   EXSR      CHKNUM
026000100309     C                   IF        PiInt=*on
026100100309     C                   Z-ADD     PiVal         VATCCM
026200100309     C                   ELSE
026300100309     C                   SETON                                        31
026400100309     C                   EVAL      vinmsg = %trimr(vinmsg)
026500100309     C                             + ' ' + 'VATCCM'
026600100309     C                   ENDIF
026700100309     C*
026800100309     C                   EVAL      PiStr=%trim(a_vatlnp)
026900100309     C                   EXSR      CHKNUM
027000100309     C                   IF        PiInt=*on
027100100309     C                   Z-ADD     PiVal         VATLNP
027200100309     C                   ELSE
027300100309     C                   SETON                                        31
027400100309     C                   EVAL      vinmsg = %trimr(vinmsg)
027500100309     C                             + ' ' + 'VATLNP'
027600100309     C                   ENDIF
027700100309     C*
027800100309     C                   EVAL      PiStr=%trim(a_vataas)
027900100309     C                   EXSR      CHKNUM
028000100309     C                   IF        PiInt=*on
028100100309     C                   Z-ADD     PiVal         VATAAS
028200100309     C                   ELSE
028300100309     C                   SETON                                        31
028400100309     C                   EVAL      vinmsg = %trimr(vinmsg)
028500100309     C                             + ' ' + 'VATAAS'
028600100309     C                   ENDIF
028700100309     C*
028800100309     C                   EVAL      PiStr=%trim(a_vatnrs)
028900100309     C                   IF        PiStr = *blanks
029000100309     C                   EVAL      PiStr = *zeros
029100100309     C                   ELSE
029200100309     C                   EXSR      CHKNUM
029300100309     C                   IF        PiInt=*on
029400100309     C                   Z-ADD     PiVal         VATNRS
029500100309     C                   ELSE
029600100309     C                   SETON                                        31
029700100309     C                   EVAL      vinmsg = %trimr(vinmsg)
029800100309     C                             + ' ' + 'VATNRS'
029900100309     C                   ENDIF
030000100309     C                   ENDIF
030100100309     C*
030200100309     C                   EVAL      PiStr=%trim(a_vatnsp)
030300100309     C                   EXSR      CHKNUM
030400100309     C                   IF        PiInt=*on
030500100309     C                   Z-ADD     PiVal         VATNSP
030600100309     C                   ELSE
030700100309     C                   SETON                                        31
030800100309     C                   EVAL      vinmsg = %trimr(vinmsg)
030900100309     C                             + ' ' + 'VATNSP'
031000100309     C                   ENDIF
031100100309     C*
031200100309     C                   ENDSR
031300100309     C*----------------------------------------------------*
031400100309
031500100309
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
040500100309
040600100309
040700100309
040800100309     C*----------------------------------------------------*
040900100309     C*  CONTROLLO NUMERICITA' CAMPI
041000100309     C*----------------------------------------------------*
041100100309     C     CHKNUM        BEGSR
041200100309     C*
041300100309     C                   IF        PiDecChr = *blanks
041400100309     C                   EVAL      PiDecChr = ','
041500100309     C                   ENDIF
041600100309     C*
041700100309     C                   callp(e)  UBISNUM_Check(PiStr
041800100309     C                                          :PiDecChr
041900100309     C                                          :PiVal
042000100309     C                                          :PiNum
042100100309     C                                          :PiInt)
042200100309     C*
042300100309     C                   IF        %error
042400100309     C                   EVAL      PiInt=*off
042500100309     C                   ENDIF
042600100309     C*
042700100309     C                   ENDSR
042800100309     C***
042900010531
043000010531
043100010531
043200000613     C     *inzsr        BEGSR
043300990914     C*
043400990914     C     *entry        plist
043500990921     C                   parm                    tivlrds
043600990921     C                   parm      wrkesito      esito
043700000724     C                   parm                    prmlit
043800000710     C                   parm                    prmfir
043900030710     C*
044000030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
044100030710     C                   move(p)   vlrMSG        wNomeFile
044200030710     C*
044300030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
044400030710     C* del codice CMR dal nome del file importato.
044500030710     C                   if        wNomeFile <> *blanks AND
044600030710     C                             vlrppt<>*blanks
044700030710     C                   if        %subst(vlrppt:1:1) = 'S'
044800030710     C                   seton                                        50
044900030710     C                   else
045000030710     C                   setoff                                       50
045100030710     C                   endif
045200100309     C*
045300100309     C                   if        %subst(vlrppt:6:1) = 'M'
045400100309     C     minu:maiu     XLATE     wNomeFile     wNomeFile                      *Minus -> Maiuscolo
045500100309     C                   endif
045600100309     C*
045700030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
045800030710     C                   move(p)   wPosDaA       wPosDa
045900030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
046000030710     C                   move(p)   wLungA        wLung
046100030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
046200030710     C                   seton                                        60
046300030710     C                   else
046400030710     C                   setoff                                       60
046500030710     C                   endif
046600010606     C*
046700010606     C* CALCOLA LA DATA CORRENTE
046800100309     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
046900100309     C                   eval      datcor = %dec(%date() : *ISO)
047000000613     C*
047100000613     C                   ENDSR
047200000613     C***
