000100101025      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200091006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600101025     FEDIVABwr  O    E             DISK    usropn
000700101025     FEDIVATwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600060725     D tisi95ds      e ds
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600010201
002700091006
002800091006     D*------------------
002900091006     D* LINKING A DEFINIZIONI ESTERNE
003000091006     D*------------------
003100091006     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
003200091006     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
003300091006
003400010201
003500000913     C                   reset                   rrnum
003600990921     C                   reset                   esito
003700990921     C                   reset                   wrkesito
003800000613     C*
003900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004000000613     C*
004100010202     C* Effettuo la chiamata al CLLE preposto
004200101025     C                   call(e)   'TITVEVTC'
004300010202     C                   parm                    parccm
004400010202     C                   parm                    parmbr
004500010202     C                   parm      '2'           paropz
004600050201     C*
004700050201     C* Effettuo lancio TISI95 solo x chiusura
004800050201     C                   CLEAR                   TISI95DS
004900050201     C                   EVAL      I95TLA = 'C'
005000050201     C                   CALL      'TISI95R'
005100050201     C                   PARM                    TISI95DS
005200000616     C*
005300000801     C
005400010201     C                   seton                                        LR
005500000801
005600910830     C*--------------------------------------------------------
005700101025     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
005800910830     C*--------------------------------------------------------
005900040526     C     RWFILE        BEGSR
006000990910     C*
006100990914     C                   if        not %open(tivin00r)
006200990908     C                   open      tivin00r
006300990914     C                   endif
006400101025     C                   if        not %open(edivabwr)
006500101025     C                   open      edivabwr
006600990914     C                   endif
006700101025     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
006800020305     C                   exsr      prevat
006900010201     C*
007000010202     C                   if        chkcall = '0'
007100010202     C*
007200101025     C                   if        not %open(edivatwr)
007300101025     C                   open      edivatwr
007400010201     C                   endif
007500990910     C*
007600010201     C                   clear                   §CTROKVB          5 0
007700020305     C                   clear                   §CTROKVT          5 0
007800000801     C                   clear                   §CTRMO            5 0
007900000801     C                   clear                   §CTRNO            5 0
008000990910     C*
008100921023     C                   DO        *HIVAL
008200990913     C*
008300990915     C                   READ      tivin00r                               70
008400050627     C                   if        vindta > *blanks
008500000613     C                   add       1             rrnum
008600000801     C*
008700000801     C                   if        *in70 = *off
008800000801     C                             and
008900000801     C                             (vinflg = *blanks
009000000801     C                              or vinflg = '0'
009100000801     C                              or vinflg = '2')
009200000801     C*
009300000801     C                   clear                   vinmsg
009400000801     C                   eval      vinflg = '1'
009500140308     C*
009600101025     C                   clear                   edivab00
009700101025     C                   clear                   edivat00
009800050628     C                   exsr      impvab                                       => carico VAB
009900060725     C                   exsr      wrivab                                       => scarico VAB
010000000905     C*
010100000905     C                   else
010200000905     C                   eval      vinflg = '1'
010300050628     C                   endif
010400000905     C                   endif
010500000905     C*
010600000905     C  N70              update    tivin000
010700000905     C*
010800991022     C  N70              ENDdo
010900010202     C*
011000010202     C                   endif
011100990910
011200990910     C* Se non ci sono record con errori ...
011300000710     C                   if        §ctrno = 0
011400990910     C* ... restituisco esito OK.
011500990921     C                   eval      wrkesito = '0'
011600990910     C                   else
011700010201     C                   if        §ctrokvb > 0
011800990921     C                   eval      wrkesito = '1'
011900000710     C                   else
012000000710     C                   eval      wrkesito = '2'
012100990910     C                   endif
012200000710     C                   endif
012300990910     C*
012400990914     C                   if        %open(tivin00r)
012500990908     C                   close     tivin00r
012600990914     C                   endif
012700101025     C                   if        %open(edivabwr)
012800101025     C                   close     edivabwr
012900990914     C                   endif
013000101025     C                   if        %open(edivatwr)
013100101025     C                   close     edivatwr
013200010201     C                   endif
013300990910     C*
013400010201     C                   if        §ctrokvb > 0
013500000724     C                             and vlrpoi <> *zeros
013600010202     C                   exsr      invio
013700990920     C                   endif
013800990920     C*
013900910830     C                   ENDSR
014000000613     C***
014100010305
014200010305     C*----------------------------------------------------*
014300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
014400010305     C*----------------------------------------------------*
014500020305     C     WRIVAB        BEGSR
014600101025     C*
014700101025     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
014800140308     C                   EVAL      VABCMR = %char(datcor) + ' ' + %char(oracor)
014900101025     C                   EVAL      VABDCM = DATCOR
015000101025     C                   EVAL      VABDTS = DATCOR
015100101025     C                   EVAL      VABHMS = ORACOR
015200101025     C                   EVAL      VABCNT = 1
015300010305     C*
015400060225     C* Quindi scarico il buffer del file d testata
015500101025     C                   write     edivab00                                     => scarico il VAB
015600010305     C*
015700010305     C                   ENDSR
015800101025
015900101025     C*----------------------------------------------------*
016000101025     C*  SCARICAMENTO BUFFER RECORDS VAT
016100101025     C*----------------------------------------------------*
016200101025     C     WRIVAT        BEGSR
016300101025     C*
016400101025     C                   if        vatnot <> *blanks
016500101025     C*
016600101025     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
016700140308     C                   EVAL      VATCMR = %char(datcor) + ' ' + %char(oracor)
016800101025     C                   EVAL      VATCNT = 1
016900101025     C*
017000101025     C* Quindi scarico il buffer del file d dettaglio
017100101025     C                   write     edivat00
017200101025     C                   add       1             §CTROKVT
017300101025     C                   endif
017400101025     C*
017500101025     C                   ENDSR
017600990920
017700000801     C*----------------------------------------------------*
017800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017900000801     C*----------------------------------------------------*
018000010201     C     INZVAR        BEGSR
018100000801     C*
018200040802     C                   Z-ADD     *zeros        Num5_0            5 0
018300040802     C                   MOVEL     '0'           FlgCAS            1
018400000801     C*
018500000801     C                   ENDSR
018600000801     C*----------------------------------------------------*
018700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
018800000801     C*----------------------------------------------------*
018900000801     C     DEFCAM        BEGSR
019000000801     C*
019100020619     C* Imposto i valori di default...
019200171020     C                   Z-ADD     0898888       VABCCM
019300171020     C                   Z-ADD     0898888       VATCCM
019400171020     C                   Z-ADD     089           VABLNP
019500171020     C                   Z-ADD     089           VATLNP
019600171020     C                   Z-ADD     101           VABCTR
019700171020     C                   MOVEL     '2'           VABCTM
019800060725     C                   MOVEL     '1'           VABCBO
019900020619     C* ... e poi verifico se sono stati passati come parametri
020000020619     C                   IF        vlrppt > *blanks
020100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020300020619     C                   EXSR      CHKNUM
020400020619     C                   IF        PiInt=*on
020500020619     C                   Z-ADD     PiVal         VABCCM
020600020619     C                   Z-ADD     PiVal         VATCCM
020700020619     C                   ENDIF
020800040506     C                   ENDIF
020900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021100020619     C                   EXSR      CHKNUM
021200020619     C                   IF        PiInt=*on
021300020619     C                   Z-ADD     PiVal         VABLNP
021400020619     C                   Z-ADD     PiVal         VATLNP
021500040506     C                   ENDIF
021600020619     C                   ENDIF
021700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021900020619     C                   EXSR      CHKNUM
022000020619     C                   IF        PiInt=*on
022100020619     C                   Z-ADD     PiVal         VABCTR
022200040506     C                   ENDIF
022300020619     C                   ENDIF
022400020619     C                   ENDIF
022500000801     C*
022600000801     C                   ENDSR
022700000801     C*----------------------------------------------------*
022800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
022900000801     C*----------------------------------------------------*
023000040823     C     IMPVAB        BEGSR
023100040823     C*
023200020305     C                   EXSR      INZVAR
023300020305     C                   EXSR      DEFCAM
023400010305     C*
023500000801     C                   Z-ADD     *zeros        errore            1 0
023600000830     C                   MOVEL     datcor        VABAAS
023700020305     C                   MOVEL     datcor        VATAAS
023800040526     C                   MOVE      datcor        VABMGS
023900060529     C                   MOVE(P)   vlrpoi        VABFGS
024000060529     C                   MOVE(P)   vlrpoi        VATFGS
024100050628     C*
024200140308     C                   EVAL      VABRSD=%trim(%subst(vindta:156:35))
024300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
024500020117     C* ==
024600140308     C                   EVAL      VABRD2=%trim(%subst(vindta:191:35))
024700140308     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
024800171020     C                   EVAL      VABCAD=%trim(%subst(vindta:316:7))
024900140308     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
025000140308     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
025100171221     C***                EVAL      VABRMA=%trim(%subst(vindta:80:15))
025200171221     C                   EVAL      VABRMA=%trim(%subst(vindta:578:11))
025300171129     C                   EVAL      VABGMA=%trim(%subst(vindta:464:2))
025400171020     C* NSP / RMN
025500171020     C                   EVAL      PiStr=%trim(%subst(vindta:95:15))
025600060529     C                   EXSR      CHKNUM
025700060529     C                   IF        PiInt=*on
025800171020     C                   Z-ADD     PiVal         VABNSP
025900171020     C                   Z-ADD     PiVal         VABRMN
026000060529     C                   ELSE
026100060529     C                   ADD       1             errore
026200060529     C                   EVAL      vinmsg = %trimr(vinmsg)
026300171020     C                             + ' ' + 'VABNSP VABRMN'
026400060529     C                   ENDIF
026500091006     C*
026600040506     C* Reperisco la provincia dal CAP e dalla località
026700140308     C                   IF        VABNZD  = *blanks AND
026800140308     C                             VABLOD <> *blanks AND
026900140308     C                             VABCAD <> *blanks AND
027000040526     C                             VABPRD  = *blanks
027100040506     C                   CLEAR                   TISI95DS
027200040506     C                   EVAL      I95TCN = '3'
027300040506     C                   Z-ADD     datcor        I95DAT
027400040506     C                   EVAL      I95CAP = VABCAD
027500040506     C                   EVAL      I95LOC = VABLOD
027600050627     C                   EVAL      I95NAR = VABNZD
027700040506     C                   CALL      'TISI95R'
027800040506     C                   PARM                    TISI95DS
027900040506     C                   EVAL      VABPRD = O95PRV
028000040506     C                   ENDIF
028100091006     C* NCL
028200140308     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
028300060725     C                   EXSR      CHKNUM
028400060725     C                   IF        PiInt=*on
028500140308     C                   Z-ADD     PiVal         VABNCL
028600060725     C                   ELSE
028700060725     C                   ADD       1             errore
028800060725     C                   EVAL      vinmsg = %trimr(vinmsg)
028900060725     C                             + ' ' + 'VABNCL'
029000060725     C                   ENDIF
029100040506     C* PKB
029200171129     C* se passano tutti 0 metto forzatamente a 1 kg
029300171129     C                   IF        %subst(vindta:360:7) <> '0000000'
029400171129     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
029500010201     C                   EXSR      CHKNUM
029600010201     C                   IF        PiNum=*on
029700140308     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 dec.
029800060725     C                   Z-ADD(H)  PiVal         VABPKB
029900010201     C                   ELSE
030000010201     C                   ADD       1             errore
030100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030200010201     C                             + ' ' + 'VABPKB'
030300010201     C                   ENDIF
030400171129     C                   ELSE
030500171129     C                   Z-ADD     1             VABPKB
030600171129     C                   ENDIF
030700060529     C*
030800091006     C* Gestione estensioni "VAT"
030900171020     C***                exsr      exeVATB
031000010205     C*
031100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
031200040802     C                   IF        FlgCAS <> '0'
031300071207     C                   IF        VABCBO = '1'
031400071207     C                   EVAL      VABCBO = '4'
031500071207     C                   ENDIF
031600071207     C                   IF        VABCBO = '2'
031700071207     C                   EVAL      VABCBO = '6'
031800071207     C                   ENDIF
031900071207     C                   ENDIF
032000020305     C*
032100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
032200011113     C                   EXSR      CHKIMPDIV
032300010202     C*
032400000801     C* Ebbene...
032500000801     C                   ADD       1             §CTRMO
032600010201     C                   IF        errore <> *zeros
032700000801     C                   ADD       1             §CTRNO
032800000801     C                   EVAL      vinflg = '2'
032900000801     C                   ELSE
033000010201     C                   ADD       1             §CTROKVB
033100000801     C                   ENDIF
033200000801     C*
033300000801     C                   ENDSR
033400091006     C*----------------------------------------------------*
033500091109     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
033600091006     C*----------------------------------------------------*
033700091109     C     EXEVATB       BEGSR
033800091006     C*
033900091109     C                   EVAL      VATTRC = 'B'
034000091006     C                   EVAL      VATNOT = %subst(vindta:428:10)
034100091006     C                   IF        VATNOT <> *blanks
034200101025     C                   exsr      wrivat
034300091006     C                   ENDIF
034400091006     C*
034500091006     C                   ENDSR
034600010202     C*----------------------------------------------------*
034700101025     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
034800010202     C*----------------------------------------------------*
034900020305     C     PREVAT        BEGSR
035000010202     C*
035100101025     C* Compongo il nome del membro da dare al EDIVATWR
035200010202     C                   eval      parmbr = vlrhdl
035300010202     C                   movel     'M'           parmbr
035400050627     C                   eval      parccm = %subst(vlrKSC:2:7)
035500010202     C                   eval      paropz = '1'
035600010202     C* Effettuo la chiamata al CLLE preposto
035700101025     C                   call(e)   'TITVEVTC'
035800010202     C                   parm                    parccm
035900010202     C                   parm                    parmbr
036000010202     C                   parm                    paropz
036100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
036200010202     C                   if        %error
036300010202     C                   movel     '1'           chkcall
036400010202     C                   else
036500010202     C                   movel     '0'           chkcall
036600010202     C                   endif
036700010202     C*
036800010202     C                   ENDSR
036900000801     C*----------------------------------------------------*
037000000801     C*  CONTROLLO NUMERICITA' CAMPI
037100000801     C*----------------------------------------------------*
037200091006     C     CHKNUM        BEGSR
037300091006     C*
037400091006     C                   IF        PiDecChr = *blanks
037500091006     C                   EVAL      PiDecChr = ','
037600091006     C                   ENDIF
037700091006     C*
037800091006     C                   callp(e)  UBISNUM_Check(PiStr
037900091006     C                                          :PiDecChr
038000091006     C                                          :PiVal
038100091006     C                                          :PiNum
038200091006     C                                          :PiInt)
038300091006     C*
038400091006     C                   IF        %error
038500091006     C                   EVAL      PiInt=*off
038600091006     C                   ENDIF
038700000801     C*
038800000801     C                   ENDSR
038900000801     C***
039000000801
039100011113
039200011113     C*----------------------------------------------------*
039300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
039400011113     C*----------------------------------------------------*
039500011113     C     CHKIMPDIV     BEGSR
039600011113     C*
039700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
039800011113     C                   Z-ADD     *zeros        wrkDec            9 9
039900011113     C*
040000011113     C* Come prima cosa effettuo considerazioni sulla divisa
040100011113     C                   IF        vabIAS > *zeros
040200011113     C                   IF        vabVAS <> 'EUR'
040300011113     C                   EVAL      vabVAS =  'ITL'
040400011113     C                   ENDIF
040500011113     C                   ENDIF
040600011113     C*
040700011113     C                   IF        vabCAS > *zeros
040800011113     C                   IF        vabVCA <> 'EUR'
040900011113     C                   EVAL      vabVCA =  'ITL'
041000011113     C                   ENDIF
041100011113     C                   ENDIF
041200011113     C*
041300011113     C                   IF        vabVMD > *zeros
041400020305     C                   IF        vabVAD <> 'EUR'
041500011113     C                   EVAL      vabVAD =  'ITL'
041600011113     C                   ENDIF
041700011113     C                   ENDIF
041800011113     C*
041900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
042000011113     C                   Z-ADD     vabIAS        wrkDec
042100011113     C                   IF        wrkDec > *zeros
042200011113     C                   IF        vabVAS = 'ITL'
042300011113     C                   EVAL      vabIAS = *zeros
042400011113     C                   ENDIF
042500011113     C                   ENDIF
042600011113     C*
042700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
042800011113     C                   Z-ADD     vabCAS        wrkDec
042900011113     C                   IF        wrkDec > *zeros
043000011113     C                   IF        vabVCA = 'ITL'
043100011113     C                   EVAL      vabCAS = *zeros
043200011113     C                   ENDIF
043300011113     C                   ENDIF
043400011113     C*
043500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
043600011113     C                   Z-ADD     vabVMD        wrkDec
043700011113     C                   IF        wrkDec > *zeros
043800011113     C                   IF        vabVAD = 'ITL'
043900011113     C                   EVAL      vabVMD = *zeros
044000011113     C                   ENDIF
044100011113     C                   ENDIF
044200011113     C*
044300011113     C                   ENDSR
044400011113     C***
044500011113
044600011113
044700000801
044800000801
044900990920      /TITLE Invio dei dati al punto operativo.
045000010202     C     invio         BEGSR
045100990920     C*
045200021113     C* 1° invio FIVAT
045300010201     C                   reset                   dscmz
045400010201     C                   move      vlrpoi        cmzdst
045500101025     C                   eval      cmzfld = 'EDIVATWR'
045600010201     C                   eval      cmzmbd = vlrhdl
045700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
045800101025     C                   eval      cmzfla = 'EDIVAT0F'
045900101025     C                   eval      cmzmba = 'EDIVAT0F'
046000010201     C                   eval      cmznrr = *zeros
046100020305     C                   move      §ctrokvt      cmznrr
046200021018     C                   eval      cmzlba = vlrfl1
046300010201     C                   call(e)   'TIS711C'
046400010201     C                   parm                    dscmz
046500010201     C                   parm      *blanks       esito
046600010205     C                   if        %error
046700010205     C                             or cmzerr = '1'
046800010205     C                             or esito  = '1'
046900010205     C                   eval      wrkesito = '3'
047000010205     C                   else
047100010201     C*
047200021113     C* 2° invio FIVAB
047300010201     C                   reset                   dscmz
047400010201     C                   move      vlrpoi        cmzdst
047500010201     C                   eval      cmzfld = vlrfou
047600010201     C                   eval      cmzmbd = vlrhdl
047700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047800101025     C                   eval      cmzfla = 'EDIVAB0F'
047900101025     C                   eval      cmzmba = 'EDIVAB0F'
048000010201     C                   eval      cmznrr = *zeros
048100010201     C                   move      §ctrokvb      cmznrr
048200021018     C                   eval      cmzlba = vlrfl1
048300010201     C                   call(e)   'TIS711C'
048400010201     C                   parm                    dscmz
048500010201     C                   parm      *blanks       esito
048600010201     C                   if        %error
048700010201     C                             or cmzerr = '1'
048800010201     C                             or esito  = '1'
048900010201     C                   eval      wrkesito = '3'
049000010201     C                   endif
049100010205     C                   endif
049200990920     C*
049300000613     C                   ENDSR
049400000613     C***
049500070411
049600070411     C     *pssr         BEGSR
049700070411     C*
049800070411     C                   if        %open(tivin00r)
049900070411     C                   close     tivin00r
050000070411     C                   endif
050100101025     C                   if        %open(edivabwr)
050200101025     C                   close     edivabwr
050300070411     C                   endif
050400101025     C                   if        %open(edivatwr)
050500101025     C                   close     edivatwr
050600070411     C                   endif
050700070411     C*
050800070411     C* Effettuo la chiamata al CLLE preposto
050900101025     C                   call(e)   'TITVEVTC'
051000070411     C                   parm                    parccm
051100070411     C                   parm                    parmbr
051200070411     C                   parm      '2'           paropz
051300070411     C*
051400070411     C                   eval      wrkesito = '2'
051500070411     C*
051600070411     C                   seton                                        LR
051700070411     C*
051800070411     C                   ENDSR     '*CANCL'
051900070411     C***
052000070411
052100990910
052200000613     C     *inzsr        BEGSR
052300990910     C*
052400990910     C     *entry        plist
052500990920     C                   parm                    tivlrds
052600990921     C                   parm      wrkesito      esito
052700000724     C                   parm                    prmlit
052800000710     C                   parm                    prmfir
052900000613     C*
053000000830     C* CALCOLA LA DATA CORRENTE
053100000830     C                   time                    wn14             14 0
053200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053300101025     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
053400140308     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
053500140308     C                   eval      datcor = %dec(%date() :*iso)
053600000830     C*
053700000613     C                   ENDSR
053800000613     C***
