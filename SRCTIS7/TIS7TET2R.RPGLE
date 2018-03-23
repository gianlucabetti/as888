000100040325      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200100309     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
002600101209     D  a_vatfgs                      3
002700050614     D  a_vatccm                      7
002800050614     D  a_vatlnp                      3
002900050614     D  a_vataas                      4
003000050614     D  a_vatnrs                      2
003100050614     D  a_vatnsp                      7
003200050614     D  a_vattrc                      1
003300050614     D  a_vatnot                     35
003400010606
003500100309     D*-------------------
003600100309     D* COSTANTI
003700100309     D*-------------------
003800100309     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
003900100309     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004000100309
004100100309
004200100309     D*------------------
004300100309     D* LINKING A DEFINIZIONI ESTERNE
004400100309     D*------------------
004500100309     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004600100309     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004700100309
004800100309
004900010606
005000010606
005100990914
005200000913     C                   reset                   rrnum
005300990921     C                   reset                   esito
005400990921     C                   reset                   wrkesito
005500010531     C*
005600010531     C                   exsr      opeini
005700040325     C                   exsr      rwvat
005800010531     C*
005900010531     C                   seton                                        lr
006000010531
006100010531
006200010531
006300010531
006400010531     C*--------------------------------------------------------
006500010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006600010531     C*--------------------------------------------------------
006700010531     C     PREELA        BEGSR
006800000724     C*
006900000724     C* SE OCCORRE SPEDIRE IN FILIALE
007000010531     C                   if        invfil <> *zeros and
007100010531     C                             flgGiro = '0'
007200010531     C*
007300010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007400010531     C                   eval      flgGiro = '1'
007500010531     C*
007600010531     C                   endif
007700010531     C*
007800010531     C                   ENDSR
007900010531     C***
008000010531
008100010531
008200010531
008300010531
008400010531
008500010531
008600010531
008700010531     C*--------------------------------------------------------
008800010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008900010531     C*--------------------------------------------------------
009000010531     C     ENDELA        BEGSR
009100000616     C*
009200010531     C                   ENDSR
009300010531     C***
009400990914
009500010531
009600010531
009700010531
009800010531
009900990914     C*--------------------------------------------------------
010000040325     C* RWVAT  LEGGE TIVIN00R E SCRIVE EDIVAT0F              *
010100990914     C*--------------------------------------------------------
010200040325     C     RWVAT         BEGSR
010300030710     C*
010400030710     C                   clear                   §CTDOK
010500030710     C                   clear                   §CTDNO
010600990914     C*
010700030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
010800030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
010900030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
011000030710     C                   if        *in60 = *on
011100990914     C                   if        not %open(tivin00r)
011200990914     C                   open      tivin00r
011300990914     C                   endif
011400040325     C                   if        not %open(edivatwr)
011500040325     C                   open      edivatwr
011600990914     C                   endif
011700990914     C*
011800990914     C                   DO        *HIVAL
011900010618     C*
012000990914     C                   READ      tivin00r                               70
012100010618     C*
012200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012300010618     C                   if        vinflg = '1'
012400010618     C                   eval      flgOk = '1'
012500010618     C                   endif
012600010618     C*
012700000905     C                   if        vindta > *blanks
012800000613     C                   add       1             rrnum
012900990914     C*
013000010601     C                   if        *in70 = *off and
013100990914     C                             (vinflg = *blanks
013200010711     C                              or vinflg = '0'
013300010711     C                              or vinflg = '2')
013400990914     C*
013500040325     C                   clear                   EDIVAT00
013600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013700010711     C                   if        vinflg = *blanks or vinflg = '0'
013800010711     C                   clear                   vinmsg
013900010711     C                   endif
014000010711     C*
014100040325     C                   eval      fnvataar = vindta
014200040325     C*
014300040325     C                   testb     x'3F'         a_vatatb                 02
014400040325     C                   if        not *in02
014500991022     C*
014600040325     C                   exsr      impvat
014700010531     C*
014800010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014900101209     C                   eval      depfil = VATFGS
015000010531     C                   exsr      repfil
015100010601     C                   if        depfil = invfil
015200021014     C                   if        vlrpoi = 999
015300040325     C                   MOVE(p)   invfil        VATFGS
015400021014     C                   else
015500040325     C                   MOVE(p)   vlrpoi        VATFGS
015600021014     C                   endif
015700010531     C*
015800010531     C                   exsr      PREELA
015900010531     C*
016000030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
016100040325     C                   EVAL      VATCMR = wCMR
016200040325     C                   EVAL      VATCNT = 1
016300010601     C*
016400010604     C  N31              ADD       1             §CTDOK            7 0
016500010601     C   32
016600010604     COR 31              ADD       1             §CTDNO            7 0
016700040325     C  N31              WRITE     EDIVAT00
016800010601     C*
016900010601     C                   if        *in31 = *off and
017000010601     C                             *in32 = *off
017100990914     C                   eval      vinflg = '1'
017200990914     C                   else
017300990914     C                   eval      vinflg = '2'
017400990914     C                   endif
017500000905     C                   endif
017600000905     C                   endif
017700010601     C*
017800010601     C                   endif
017900000905     C*
018000000905     C                   else
018100000905     C                   eval      vinflg = '1'
018200000905     C                   endif
018300000905     C*
018400000905     C  N70              update    tivin000
018500991022     C*
018600991022     C  N70              ENDdo
018700010531     C
018800010531     C*
018900010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019000010531     C                   if        cntNonEl = *zeros or
019100010531     C                             flgMulti = '0'
019200990914     C* Se non ci sono record con errori ...
019300010531     C                   if        §ctdno = 0 and
019400010531     C                             flgStato <> '2'
019500990914     C* ... restituisco esito OK.
019600990921     C                   eval      wrkesito = '0'
019700990914     C                   else
019800000710     C                   if        §ctdok > 0
019900990921     C                   eval      wrkesito = '1'
020000000710     C                   else
020100010615     C                   if        flgOk = '0'
020200010615     C                   eval      wrkesito = '2'
020300010615     C                   else
020400010615     C                   eval      wrkesito = '6'
020500010615     C                   endif
020600990914     C                   endif
020700000710     C                   endif
020800010531     C                   else
020900010531     C                   eval      wrkesito = '9'
021000010531     C                   endif
021100030710     C*
021200030710 X  1C                   else
021300030710     C                   eval      wrkesito = '8'
021400030710     C                   eval      flgGiro = '1'
021500030710 E  1C                   endif
021600990914     C*
021700990914     C                   if        %open(tivin00r)
021800990914     C                   close     tivin00r
021900990914     C                   endif
022000040325     C                   if        %open(edivatwr)
022100040325     C                   close     edivatwr
022200990914     C                   endif
022300010531     C*
022400010531     C                   if        vlrpoi <> 999
022500010531     C                   eval      invfil = vlrpoi
022600010531     C                   endif
022700990914     C*
022800990921     C                   if        §ctdok > 0
022900010531     C                             and invfil > *zeros
023000990921     C                   exsr      invio
023100990921     C                   endif
023200010612     C*
023300010612     C                   if        flgGiro = '1'
023400010612     C                   exsr      endela
023500010612     C                   endif
023600990921     C*
023700990914     C                   ENDSR
023800000613     C***
023900040325
024000100309
024100100309
024200100309
024300100309     C*----------------------------------------------------*
024400100309     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
024500100309     C*----------------------------------------------------*
024600100309     C     IMPVAT        BEGSR
024700100309     C*
024800100309     C                   SETOFF                                       3132
024900100309     C*
025000100309     C* Reperimento campi ALFA
025100100309     C                   EVAL      VATATB=a_vatatb
025200100309     C                   EVAL      VATTRC=a_vattrc
025300131018     C                   IF        a_vattrc <> 'S'
025400100309     C                   EVAL      VATNOT=%trim(a_vatnot)
025500131023     C                   ELSE
025600131023     C                   EVAL      VATNOT=a_vatnot
025700131018     C                   ENDIF
025800101209     C*
025900101209     C                   EVAL      PiStr=%trim(a_vatfgs)
026000101209     C                   EXSR      CHKNUM
026100101209     C                   IF        PiInt=*on
026200101209     C                   Z-ADD     PiVal         VATFGS
026300101209     C                   ELSE
026400101209     C                   SETON                                        31
026500101209     C                   EVAL      vinmsg = %trimr(vinmsg)
026600101209     C                             + ' ' + 'VATFGS'
026700101209     C                   ENDIF
026800100309     C*
026900100309     C                   EVAL      PiStr=%trim(a_vatccm)
027000100309     C                   EXSR      CHKNUM
027100100309     C                   IF        PiInt=*on
027200100309     C                   Z-ADD     PiVal         VATCCM
027300100309     C                   ELSE
027400100309     C                   SETON                                        31
027500100309     C                   EVAL      vinmsg = %trimr(vinmsg)
027600100309     C                             + ' ' + 'VATCCM'
027700100309     C                   ENDIF
027800100309     C*
027900100309     C                   EVAL      PiStr=%trim(a_vatlnp)
028000100309     C                   EXSR      CHKNUM
028100100309     C                   IF        PiInt=*on
028200100309     C                   Z-ADD     PiVal         VATLNP
028300100309     C                   ELSE
028400100309     C                   SETON                                        31
028500100309     C                   EVAL      vinmsg = %trimr(vinmsg)
028600100309     C                             + ' ' + 'VATLNP'
028700100309     C                   ENDIF
028800100309     C*
028900100309     C                   EVAL      PiStr=%trim(a_vataas)
029000100309     C                   EXSR      CHKNUM
029100100309     C                   IF        PiInt=*on
029200100309     C                   Z-ADD     PiVal         VATAAS
029300100309     C                   ELSE
029400100309     C                   SETON                                        31
029500100309     C                   EVAL      vinmsg = %trimr(vinmsg)
029600100309     C                             + ' ' + 'VATAAS'
029700100309     C                   ENDIF
029800100309     C*
029900100309     C                   EVAL      PiStr=%trim(a_vatnrs)
030000100309     C                   IF        PiStr = *blanks
030100100309     C                   EVAL      PiStr = *zeros
030200100309     C                   ELSE
030300100309     C                   EXSR      CHKNUM
030400100309     C                   IF        PiInt=*on
030500100309     C                   Z-ADD     PiVal         VATNRS
030600100309     C                   ELSE
030700100309     C                   SETON                                        31
030800100309     C                   EVAL      vinmsg = %trimr(vinmsg)
030900100309     C                             + ' ' + 'VATNRS'
031000100309     C                   ENDIF
031100100309     C                   ENDIF
031200100309     C*
031300100309     C                   EVAL      PiStr=%trim(a_vatnsp)
031400100309     C                   EXSR      CHKNUM
031500100309     C                   IF        PiInt=*on
031600100309     C                   Z-ADD     PiVal         VATNSP
031700100309     C                   ELSE
031800100309     C                   SETON                                        31
031900100309     C                   EVAL      vinmsg = %trimr(vinmsg)
032000100309     C                             + ' ' + 'VATNSP'
032100100309     C                   ENDIF
032200100309     C*
032300100309     C                   ENDSR
032400100309     C*----------------------------------------------------*
032500100309
032600100309
032700990921
032800010531
032900010531
033000010531
033100010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
033200010531     C     repfil        BEGSR
033300010531     C*
033400010531     C                   if        invfil = *zeros and
033500010531     C                             depfil > *zeros and
033600010613     C                             (vinflg = *blanks or
033700010613     C                              vinflg = *zeros)
033800010531     C
033900010531     C                   eval      invfil = depfil
034000010531     C                   endif
034100010531     C*
034200010531     C                   if        depfil <> invfil and
034300010531     C                             invfil > *zeros
034400010531     C                   eval      flgMulti = '1'
034500010531     C                   if        vinflg = *blanks
034600010531     C                   add       1             cntNonEl
034700010531     C                   endif
034800010531     C                   endif
034900010531     C*
035000010531     C                   if        vinflg = '2'
035100010531     C                   eval      flgStato = '2'
035200010531     C                   endif
035300010531     C*
035400010531     C                   ENDSR
035500010531     C***
035600010531
035700010531
035800010531
035900010531
036000010531
036100990921      /TITLE Invio dei dati al punto operativo.
036200000613     C     invio         BEGSR
036300990921     C*
036400990921     C                   reset                   dscmz
036500030618     C                   move      invfil        cmzdst
036600030618     C***                move      '789'         cmzdst
036700990921     C                   eval      cmzfld = vlrfou
036800990921     C                   eval      cmzmbd = vlrhdl
036900990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
037000000710     C                   if        prmfir = *blanks
037100040325     C                   eval      cmzfla = 'EDIVAT0F'
037200040325     C                   eval      cmzmba = 'EDIVAT0F'
037300000710     C                   else
037400000710     C                   eval      cmzfla = prmfir
037500000710     C                   eval      cmzmba = prmfir
037600000710     C                   endif
037700990921     C                   eval      cmznrr = *zeros
037800990921     C                   move      §ctdok        cmznrr
037900021003     C                   eval      cmzlba = vlrfl1
038000990921     C                   call(e)   'TIS711C'
038100990921     C                   parm                    dscmz
038200990921     C                   parm      *blanks       esito
038300990921     C                   if        %error
038400990921     C                             or cmzerr = '1'
038500990921     C                             or esito  = '1'
038600000710     C                   eval      wrkesito = '3'
038700990921     C                   endif
038800990921     C*
038900000613     C                   ENDSR
039000000613     C***
039100990914
039200010531
039300010531
039400010531
039500010531      /TITLE Invio dei dati al punto operativo.
039600010531     C     opeini        BEGSR
039700010531     C*
039800010531     C* Inizializzo flag e contatori operativi
039900010531     C                   movel     '0'           flgGiro           1
040000010531     C                   movel     '0'           flgMulti          1
040100010531     C                   movel     '1'           flgStato          1
040200010615     C                   movel     '0'           flgOk             1
040300010531     C                   z-add     *zeros        cntNonEl         10 0
040400010531     C                   z-add     *zeros        depfil            3 0
040500010531     C                   z-add     *zeros        invfil            3 0
040600030407     C*
040700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
040800030407     C                   if        %subst(vlrppt:1:1) = 'S'
040900030407     C                   seton                                        50
041000030407     C                   else
041100030407     C                   setoff                                       50
041200030407     C                   endif
041300010531     C*
041400010531     C                   ENDSR
041500010531     C***
041600100309
041700100309
041800100309
041900100309     C*----------------------------------------------------*
042000100309     C*  CONTROLLO NUMERICITA' CAMPI
042100100309     C*----------------------------------------------------*
042200100309     C     CHKNUM        BEGSR
042300100309     C*
042400100309     C                   IF        PiDecChr = *blanks
042500100309     C                   EVAL      PiDecChr = ','
042600100309     C                   ENDIF
042700100309     C*
042800100309     C                   callp(e)  UBISNUM_Check(PiStr
042900100309     C                                          :PiDecChr
043000100309     C                                          :PiVal
043100100309     C                                          :PiNum
043200100309     C                                          :PiInt)
043300100309     C*
043400100309     C                   IF        %error
043500100309     C                   EVAL      PiInt=*off
043600100309     C                   ENDIF
043700100309     C*
043800100309     C                   ENDSR
043900100309     C***
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
046300100309     C*
046400100309     C                   if        %subst(vlrppt:6:1) = 'M'
046500100309     C     minu:maiu     XLATE     wNomeFile     wNomeFile                      *Minus -> Maiuscolo
046600100309     C                   endif
046700100309     C*
046800030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
046900030710     C                   move(p)   wPosDaA       wPosDa
047000030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
047100030710     C                   move(p)   wLungA        wLung
047200030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
047300030710     C                   seton                                        60
047400030710     C                   else
047500030710     C                   setoff                                       60
047600030710     C                   endif
047700010606     C*
047800010606     C* CALCOLA LA DATA CORRENTE
047900100309     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
048000100309     C                   eval      datcor = %dec(%date() : *ISO)
048100000613     C*
048200000613     C                   ENDSR
048300000613     C***
