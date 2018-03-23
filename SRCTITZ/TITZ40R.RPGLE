000100040325      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
008000010531     C*--------------------------------------------------------
008100010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008200010531     C*--------------------------------------------------------
008300010531     C     ENDELA        BEGSR
008400000616     C*
008500010531     C                   ENDSR
008600010531     C***
008700010531
008800990914     C*--------------------------------------------------------
008900040325     C* RWVAT  LEGGE TIVIN00R E SCRIVE EDIVAT0F              *
009000990914     C*--------------------------------------------------------
009100040325     C     RWVAT         BEGSR
009200030710     C*
009300030710     C                   clear                   §CTDOK
009400030710     C                   clear                   §CTDNO
009500990914     C*
009600030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
009700030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
009800030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
009900030710     C                   if        *in60 = *on
010000990914     C                   if        not %open(tivin00r)
010100990914     C                   open      tivin00r
010200990914     C                   endif
010300040325     C                   if        not %open(edivatwr)
010400040325     C                   open      edivatwr
010500990914     C                   endif
010600990914     C*
010700990914     C                   DO        *HIVAL
010800010618     C*
010900990914     C                   READ      tivin00r                               70
011000010618     C*
011100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011200010618     C                   if        vinflg = '1'
011300010618     C                   eval      flgOk = '1'
011400010618     C                   endif
011500010618     C*
011600000905     C                   if        vindta > *blanks
011700000613     C                   add       1             rrnum
011800990914     C*
011900010601     C                   if        *in70 = *off and
012000990914     C                             (vinflg = *blanks
012100010711     C                              or vinflg = '0'
012200010711     C                              or vinflg = '2')
012300990914     C*
012400040325     C                   clear                   EDIVAT00
012500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012600010711     C                   if        vinflg = *blanks or vinflg = '0'
012700010711     C                   clear                   vinmsg
012800010711     C                   endif
012900010711     C*
013000040325     C                   eval      fnvataar = vindta
013100040325     C*
013200040325     C                   testb     x'3F'         a_vatatb                 02
013300040325     C                   if        not *in02
013400991022     C*
013500040325     C                   exsr      impvat
013600010531     C*
013700010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013800040325     C                   eval      depfil = VATLNP
013900010531     C                   exsr      repfil
014000010601     C                   if        depfil = invfil
014100021014     C                   if        vlrpoi = 999
014200040325     C                   MOVE(p)   invfil        VATFGS
014300021014     C                   else
014400040325     C                   MOVE(p)   vlrpoi        VATFGS
014500021014     C                   endif
014600010531     C*
014700010531     C                   exsr      PREELA
014800010531     C*
014900030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
015000040325     C                   EVAL      VATCMR = wCMR
015100040325     C                   EVAL      VATCNT = 1
015200010601     C*
015300010604     C  N31              ADD       1             §CTDOK            7 0
015400010601     C   32
015500010604     COR 31              ADD       1             §CTDNO            7 0
015600040325     C  N31              WRITE     EDIVAT00
015700010601     C*
015800010601     C                   if        *in31 = *off and
015900010601     C                             *in32 = *off
016000990914     C                   eval      vinflg = '1'
016100990914     C                   else
016200990914     C                   eval      vinflg = '2'
016300990914     C                   endif
016400000905     C                   endif
016500000905     C                   endif
016600010601     C*
016700010601     C                   endif
016800000905     C*
016900000905     C                   else
017000000905     C                   eval      vinflg = '1'
017100000905     C                   endif
017200000905     C*
017300000905     C  N70              update    tivin000
017400991022     C*
017500991022     C  N70              ENDdo
017600010531     C
017700010531     C*
017800010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017900010531     C                   if        cntNonEl = *zeros or
018000010531     C                             flgMulti = '0'
018100990914     C* Se non ci sono record con errori ...
018200010531     C                   if        §ctdno = 0 and
018300010531     C                             flgStato <> '2'
018400990914     C* ... restituisco esito OK.
018500990921     C                   eval      wrkesito = '0'
018600990914     C                   else
018700000710     C                   if        §ctdok > 0
018800990921     C                   eval      wrkesito = '1'
018900000710     C                   else
019000010615     C                   if        flgOk = '0'
019100010615     C                   eval      wrkesito = '2'
019200010615     C                   else
019300010615     C                   eval      wrkesito = '6'
019400010615     C                   endif
019500990914     C                   endif
019600000710     C                   endif
019700010531     C                   else
019800010531     C                   eval      wrkesito = '9'
019900010531     C                   endif
020000030710     C*
020100030710 X  1C                   else
020200030710     C                   eval      wrkesito = '8'
020300030710     C                   eval      flgGiro = '1'
020400030710 E  1C                   endif
020500990914     C*
020600990914     C                   if        %open(tivin00r)
020700990914     C                   close     tivin00r
020800990914     C                   endif
020900040325     C                   if        %open(edivatwr)
021000040325     C                   close     edivatwr
021100990914     C                   endif
021200010531     C*
021300010531     C                   if        vlrpoi <> 999
021400010531     C                   eval      invfil = vlrpoi
021500010531     C                   endif
021600990914     C*
021700990921     C                   if        §ctdok > 0
021800010531     C                             and invfil > *zeros
021900990921     C                   exsr      invio
022000990921     C                   endif
022100010612     C*
022200010612     C                   if        flgGiro = '1'
022300010612     C                   exsr      endela
022400010612     C                   endif
022500990921     C*
022600990914     C                   ENDSR
022700000613     C***
022800040325
022900100309
023000100309
023100100309
023200100309     C*----------------------------------------------------*
023300100309     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
023400100309     C*----------------------------------------------------*
023500100309     C     IMPVAT        BEGSR
023600100309     C*
023700100309     C                   SETOFF                                       3132
023800100309     C*
023900100309     C* Reperimento campi ALFA
024000100309     C                   EVAL      VATATB=a_vatatb
024100100309     C                   EVAL      VATTRC=a_vattrc
024200100309     C                   EVAL      VATNOT=%trim(a_vatnot)
024300100309     C*
024400100309     C                   EVAL      PiStr=%trim(a_vatccm)
024500100309     C                   EXSR      CHKNUM
024600100309     C                   IF        PiInt=*on
024700100309     C                   Z-ADD     PiVal         VATCCM
024800100309     C                   ELSE
024900100309     C                   SETON                                        31
025000100309     C                   EVAL      vinmsg = %trimr(vinmsg)
025100100309     C                             + ' ' + 'VATCCM'
025200100309     C                   ENDIF
025300100309     C*
025400100309     C                   EVAL      PiStr=%trim(a_vatlnp)
025500100309     C                   EXSR      CHKNUM
025600100309     C                   IF        PiInt=*on
025700100309     C                   Z-ADD     PiVal         VATLNP
025800120703     C*
025900120703     C* se VATLNP = 0 lo valorizzo coi primi 3 digits di VATCCM
026000120703     C                   IF        VATLNP = 0
026100120703     C                   MOVEL     VATCCM        VATLNP
026200120703     C                   ENDIF
026300120703     C*
026400100309     C                   ELSE
026500100309     C                   SETON                                        31
026600100309     C                   EVAL      vinmsg = %trimr(vinmsg)
026700100309     C                             + ' ' + 'VATLNP'
026800100309     C                   ENDIF
026900100309     C*
027000100309     C                   EVAL      PiStr=%trim(a_vataas)
027100100309     C                   EXSR      CHKNUM
027200100309     C                   IF        PiInt=*on
027300100309     C                   Z-ADD     PiVal         VATAAS
027400100309     C                   ELSE
027500100309     C                   SETON                                        31
027600100309     C                   EVAL      vinmsg = %trimr(vinmsg)
027700100309     C                             + ' ' + 'VATAAS'
027800100309     C                   ENDIF
027900100309     C*
028000100309     C                   EVAL      PiStr=%trim(a_vatnrs)
028100100309     C                   IF        PiStr = *blanks
028200100309     C                   EVAL      PiStr = *zeros
028300100309     C                   ELSE
028400100309     C                   EXSR      CHKNUM
028500100309     C                   IF        PiInt=*on
028600100309     C                   Z-ADD     PiVal         VATNRS
028700100309     C                   ELSE
028800100309     C                   SETON                                        31
028900100309     C                   EVAL      vinmsg = %trimr(vinmsg)
029000100309     C                             + ' ' + 'VATNRS'
029100100309     C                   ENDIF
029200100309     C                   ENDIF
029300100309     C*
029400100309     C                   EVAL      PiStr=%trim(a_vatnsp)
029500100309     C                   EXSR      CHKNUM
029600100309     C                   IF        PiInt=*on
029700100309     C                   Z-ADD     PiVal         VATNSP
029800100309     C                   ELSE
029900100309     C                   SETON                                        31
030000100309     C                   EVAL      vinmsg = %trimr(vinmsg)
030100100309     C                             + ' ' + 'VATNSP'
030200100309     C                   ENDIF
030300100309     C*
030400100309     C                   ENDSR
030500100309     C*----------------------------------------------------*
030600100309
030700100309
030800990921
030900010531
031000010531
031100010531
031200010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
031300010531     C     repfil        BEGSR
031400010531     C*
031500010531     C                   if        invfil = *zeros and
031600010531     C                             depfil > *zeros and
031700010613     C                             (vinflg = *blanks or
031800010613     C                              vinflg = *zeros)
031900010531     C
032000010531     C                   eval      invfil = depfil
032100010531     C                   endif
032200010531     C*
032300010531     C                   if        depfil <> invfil and
032400010531     C                             invfil > *zeros
032500010531     C                   eval      flgMulti = '1'
032600010531     C                   if        vinflg = *blanks
032700010531     C                   add       1             cntNonEl
032800010531     C                   endif
032900010531     C                   endif
033000010531     C*
033100010531     C                   if        vinflg = '2'
033200010531     C                   eval      flgStato = '2'
033300010531     C                   endif
033400010531     C*
033500010531     C                   ENDSR
033600010531     C***
033700010531
033800010531
033900010531
034000010531
034100010531
034200990921      /TITLE Invio dei dati al punto operativo.
034300000613     C     invio         BEGSR
034400990921     C*
034500990921     C                   reset                   dscmz
034600030618     C                   move      invfil        cmzdst
034700030618     C***                move      '789'         cmzdst
034800990921     C                   eval      cmzfld = vlrfou
034900990921     C                   eval      cmzmbd = vlrhdl
035000990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
035100000710     C                   if        prmfir = *blanks
035200040325     C                   eval      cmzfla = 'EDIVAT0F'
035300040325     C                   eval      cmzmba = 'EDIVAT0F'
035400000710     C                   else
035500000710     C                   eval      cmzfla = prmfir
035600000710     C                   eval      cmzmba = prmfir
035700000710     C                   endif
035800990921     C                   eval      cmznrr = *zeros
035900990921     C                   move      §ctdok        cmznrr
036000021003     C                   eval      cmzlba = vlrfl1
036100990921     C                   call(e)   'TIS711C'
036200990921     C                   parm                    dscmz
036300990921     C                   parm      *blanks       esito
036400990921     C                   if        %error
036500990921     C                             or cmzerr = '1'
036600990921     C                             or esito  = '1'
036700000710     C                   eval      wrkesito = '3'
036800990921     C                   endif
036900990921     C*
037000000613     C                   ENDSR
037100000613     C***
037200990914
037300010531
037400010531
037500010531
037600010531      /TITLE Invio dei dati al punto operativo.
037700010531     C     opeini        BEGSR
037800010531     C*
037900010531     C* Inizializzo flag e contatori operativi
038000010531     C                   movel     '0'           flgGiro           1
038100010531     C                   movel     '0'           flgMulti          1
038200010531     C                   movel     '1'           flgStato          1
038300010615     C                   movel     '0'           flgOk             1
038400010531     C                   z-add     *zeros        cntNonEl         10 0
038500010531     C                   z-add     *zeros        depfil            3 0
038600010531     C                   z-add     *zeros        invfil            3 0
038700030407     C*
038800030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
038900030407     C                   if        %subst(vlrppt:1:1) = 'S'
039000030407     C                   seton                                        50
039100030407     C                   else
039200030407     C                   setoff                                       50
039300030407     C                   endif
039400010531     C*
039500010531     C                   ENDSR
039600010531     C***
039700100309
039800100309
039900100309
040000100309     C*----------------------------------------------------*
040100100309     C*  CONTROLLO NUMERICITA' CAMPI
040200100309     C*----------------------------------------------------*
040300100309     C     CHKNUM        BEGSR
040400100309     C*
040500100309     C                   IF        PiDecChr = *blanks
040600100309     C                   EVAL      PiDecChr = ','
040700100309     C                   ENDIF
040800100309     C*
040900100309     C                   callp(e)  UBISNUM_Check(PiStr
041000100309     C                                          :PiDecChr
041100100309     C                                          :PiVal
041200100309     C                                          :PiNum
041300100309     C                                          :PiInt)
041400100309     C*
041500100309     C                   IF        %error
041600100309     C                   EVAL      PiInt=*off
041700100309     C                   ENDIF
041800100309     C*
041900100309     C                   ENDSR
042000100309     C***
042100010531
042200010531
042300010531
042400000613     C     *inzsr        BEGSR
042500990914     C*
042600990914     C     *entry        plist
042700990921     C                   parm                    tivlrds
042800990921     C                   parm      wrkesito      esito
042900000724     C                   parm                    prmlit
043000000710     C                   parm                    prmfir
043100030710     C*
043200030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
043300030710     C                   move(p)   vlrMSG        wNomeFile
043400030710     C*
043500030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
043600030710     C* del codice CMR dal nome del file importato.
043700030710     C                   if        wNomeFile <> *blanks AND
043800030710     C                             vlrppt<>*blanks
043900030710     C                   if        %subst(vlrppt:1:1) = 'S'
044000030710     C                   seton                                        50
044100030710     C                   else
044200030710     C                   setoff                                       50
044300030710     C                   endif
044400100309     C*
044500100309     C                   if        %subst(vlrppt:6:1) = 'M'
044600100309     C     minu:maiu     XLATE     wNomeFile     wNomeFile                      *Minus -> Maiuscolo
044700100309     C                   endif
044800100309     C*
044900030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
045000030710     C                   move(p)   wPosDaA       wPosDa
045100030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
045200030710     C                   move(p)   wLungA        wLung
045300030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
045400030710     C                   seton                                        60
045500030710     C                   else
045600030710     C                   setoff                                       60
045700030710     C                   endif
045800010606     C*
045900010606     C* CALCOLA LA DATA CORRENTE
046000100309     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
046100100309     C                   eval      datcor = %dec(%date() : *ISO)
046200000613     C*
046300000613     C                   ENDSR
