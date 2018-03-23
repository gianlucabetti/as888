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
025100140308     C                   EVAL      VABRMA=%trim(%subst(vindta:95:15))
025200140308     C                   EVAL      VABNAS=%trim(%subst(vindta:95:15))
025300171020     C* NSP / RMN
025400171020     C                   EVAL      PiStr=%trim(%subst(vindta:95:15))
025500060529     C                   EXSR      CHKNUM
025600060529     C                   IF        PiInt=*on
025700171020     C                   Z-ADD     PiVal         VABNSP
025800171020     C                   Z-ADD     PiVal         VABRMN
025900060529     C                   ELSE
026000060529     C                   ADD       1             errore
026100060529     C                   EVAL      vinmsg = %trimr(vinmsg)
026200171020     C                             + ' ' + 'VABNSP VABRMN'
026300060529     C                   ENDIF
026400091006     C*
026500040506     C* Reperisco la provincia dal CAP e dalla località
026600140308     C                   IF        VABNZD  = *blanks AND
026700140308     C                             VABLOD <> *blanks AND
026800140308     C                             VABCAD <> *blanks AND
026900040526     C                             VABPRD  = *blanks
027000040506     C                   CLEAR                   TISI95DS
027100040506     C                   EVAL      I95TCN = '3'
027200040506     C                   Z-ADD     datcor        I95DAT
027300040506     C                   EVAL      I95CAP = VABCAD
027400040506     C                   EVAL      I95LOC = VABLOD
027500050627     C                   EVAL      I95NAR = VABNZD
027600040506     C                   CALL      'TISI95R'
027700040506     C                   PARM                    TISI95DS
027800040506     C                   EVAL      VABPRD = O95PRV
027900040506     C                   ENDIF
028000091006     C* NCL
028100140308     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
028200060725     C                   EXSR      CHKNUM
028300060725     C                   IF        PiInt=*on
028400140308     C                   Z-ADD     PiVal         VABNCL
028500060725     C                   ELSE
028600060725     C                   ADD       1             errore
028700060725     C                   EVAL      vinmsg = %trimr(vinmsg)
028800060725     C                             + ' ' + 'VABNCL'
028900060725     C                   ENDIF
029000040506     C* PKB
029100140308     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
029200010201     C                   EXSR      CHKNUM
029300010201     C                   IF        PiNum=*on
029400140308     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 dec.
029500060725     C                   Z-ADD(H)  PiVal         VABPKB
029600010201     C                   ELSE
029700010201     C                   ADD       1             errore
029800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
029900010201     C                             + ' ' + 'VABPKB'
030000010201     C                   ENDIF
030100060529     C*
030200091006     C* Gestione estensioni "VAT"
030300171020     C***                exsr      exeVATB
030400010205     C*
030500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
030600040802     C                   IF        FlgCAS <> '0'
030700071207     C                   IF        VABCBO = '1'
030800071207     C                   EVAL      VABCBO = '4'
030900071207     C                   ENDIF
031000071207     C                   IF        VABCBO = '2'
031100071207     C                   EVAL      VABCBO = '6'
031200071207     C                   ENDIF
031300071207     C                   ENDIF
031400020305     C*
031500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
031600011113     C                   EXSR      CHKIMPDIV
031700010202     C*
031800000801     C* Ebbene...
031900000801     C                   ADD       1             §CTRMO
032000010201     C                   IF        errore <> *zeros
032100000801     C                   ADD       1             §CTRNO
032200000801     C                   EVAL      vinflg = '2'
032300000801     C                   ELSE
032400010201     C                   ADD       1             §CTROKVB
032500000801     C                   ENDIF
032600000801     C*
032700000801     C                   ENDSR
032800091006     C*----------------------------------------------------*
032900091109     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
033000091006     C*----------------------------------------------------*
033100091109     C     EXEVATB       BEGSR
033200091006     C*
033300091109     C                   EVAL      VATTRC = 'B'
033400091006     C                   EVAL      VATNOT = %subst(vindta:428:10)
033500091006     C                   IF        VATNOT <> *blanks
033600101025     C                   exsr      wrivat
033700091006     C                   ENDIF
033800091006     C*
033900091006     C                   ENDSR
034000010202     C*----------------------------------------------------*
034100101025     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
034200010202     C*----------------------------------------------------*
034300020305     C     PREVAT        BEGSR
034400010202     C*
034500101025     C* Compongo il nome del membro da dare al EDIVATWR
034600010202     C                   eval      parmbr = vlrhdl
034700010202     C                   movel     'M'           parmbr
034800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
034900010202     C                   eval      paropz = '1'
035000010202     C* Effettuo la chiamata al CLLE preposto
035100101025     C                   call(e)   'TITVEVTC'
035200010202     C                   parm                    parccm
035300010202     C                   parm                    parmbr
035400010202     C                   parm                    paropz
035500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
035600010202     C                   if        %error
035700010202     C                   movel     '1'           chkcall
035800010202     C                   else
035900010202     C                   movel     '0'           chkcall
036000010202     C                   endif
036100010202     C*
036200010202     C                   ENDSR
036300000801     C*----------------------------------------------------*
036400000801     C*  CONTROLLO NUMERICITA' CAMPI
036500000801     C*----------------------------------------------------*
036600091006     C     CHKNUM        BEGSR
036700091006     C*
036800091006     C                   IF        PiDecChr = *blanks
036900091006     C                   EVAL      PiDecChr = ','
037000091006     C                   ENDIF
037100091006     C*
037200091006     C                   callp(e)  UBISNUM_Check(PiStr
037300091006     C                                          :PiDecChr
037400091006     C                                          :PiVal
037500091006     C                                          :PiNum
037600091006     C                                          :PiInt)
037700091006     C*
037800091006     C                   IF        %error
037900091006     C                   EVAL      PiInt=*off
038000091006     C                   ENDIF
038100000801     C*
038200000801     C                   ENDSR
038300000801     C***
038400000801
038500011113
038600011113     C*----------------------------------------------------*
038700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
038800011113     C*----------------------------------------------------*
038900011113     C     CHKIMPDIV     BEGSR
039000011113     C*
039100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
039200011113     C                   Z-ADD     *zeros        wrkDec            9 9
039300011113     C*
039400011113     C* Come prima cosa effettuo considerazioni sulla divisa
039500011113     C                   IF        vabIAS > *zeros
039600011113     C                   IF        vabVAS <> 'EUR'
039700011113     C                   EVAL      vabVAS =  'ITL'
039800011113     C                   ENDIF
039900011113     C                   ENDIF
040000011113     C*
040100011113     C                   IF        vabCAS > *zeros
040200011113     C                   IF        vabVCA <> 'EUR'
040300011113     C                   EVAL      vabVCA =  'ITL'
040400011113     C                   ENDIF
040500011113     C                   ENDIF
040600011113     C*
040700011113     C                   IF        vabVMD > *zeros
040800020305     C                   IF        vabVAD <> 'EUR'
040900011113     C                   EVAL      vabVAD =  'ITL'
041000011113     C                   ENDIF
041100011113     C                   ENDIF
041200011113     C*
041300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
041400011113     C                   Z-ADD     vabIAS        wrkDec
041500011113     C                   IF        wrkDec > *zeros
041600011113     C                   IF        vabVAS = 'ITL'
041700011113     C                   EVAL      vabIAS = *zeros
041800011113     C                   ENDIF
041900011113     C                   ENDIF
042000011113     C*
042100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
042200011113     C                   Z-ADD     vabCAS        wrkDec
042300011113     C                   IF        wrkDec > *zeros
042400011113     C                   IF        vabVCA = 'ITL'
042500011113     C                   EVAL      vabCAS = *zeros
042600011113     C                   ENDIF
042700011113     C                   ENDIF
042800011113     C*
042900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
043000011113     C                   Z-ADD     vabVMD        wrkDec
043100011113     C                   IF        wrkDec > *zeros
043200011113     C                   IF        vabVAD = 'ITL'
043300011113     C                   EVAL      vabVMD = *zeros
043400011113     C                   ENDIF
043500011113     C                   ENDIF
043600011113     C*
043700011113     C                   ENDSR
043800011113     C***
043900011113
044000011113
044100000801
044200000801
044300990920      /TITLE Invio dei dati al punto operativo.
044400010202     C     invio         BEGSR
044500990920     C*
044600021113     C* 1° invio FIVAT
044700010201     C                   reset                   dscmz
044800010201     C                   move      vlrpoi        cmzdst
044900101025     C                   eval      cmzfld = 'EDIVATWR'
045000010201     C                   eval      cmzmbd = vlrhdl
045100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
045200101025     C                   eval      cmzfla = 'EDIVAT0F'
045300101025     C                   eval      cmzmba = 'EDIVAT0F'
045400010201     C                   eval      cmznrr = *zeros
045500020305     C                   move      §ctrokvt      cmznrr
045600021018     C                   eval      cmzlba = vlrfl1
045700010201     C                   call(e)   'TIS711C'
045800010201     C                   parm                    dscmz
045900010201     C                   parm      *blanks       esito
046000010205     C                   if        %error
046100010205     C                             or cmzerr = '1'
046200010205     C                             or esito  = '1'
046300010205     C                   eval      wrkesito = '3'
046400010205     C                   else
046500010201     C*
046600021113     C* 2° invio FIVAB
046700010201     C                   reset                   dscmz
046800010201     C                   move      vlrpoi        cmzdst
046900010201     C                   eval      cmzfld = vlrfou
047000010201     C                   eval      cmzmbd = vlrhdl
047100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047200101025     C                   eval      cmzfla = 'EDIVAB0F'
047300101025     C                   eval      cmzmba = 'EDIVAB0F'
047400010201     C                   eval      cmznrr = *zeros
047500010201     C                   move      §ctrokvb      cmznrr
047600021018     C                   eval      cmzlba = vlrfl1
047700010201     C                   call(e)   'TIS711C'
047800010201     C                   parm                    dscmz
047900010201     C                   parm      *blanks       esito
048000010201     C                   if        %error
048100010201     C                             or cmzerr = '1'
048200010201     C                             or esito  = '1'
048300010201     C                   eval      wrkesito = '3'
048400010201     C                   endif
048500010205     C                   endif
048600990920     C*
048700000613     C                   ENDSR
048800000613     C***
048900070411
049000070411     C     *pssr         BEGSR
049100070411     C*
049200070411     C                   if        %open(tivin00r)
049300070411     C                   close     tivin00r
049400070411     C                   endif
049500101025     C                   if        %open(edivabwr)
049600101025     C                   close     edivabwr
049700070411     C                   endif
049800101025     C                   if        %open(edivatwr)
049900101025     C                   close     edivatwr
050000070411     C                   endif
050100070411     C*
050200070411     C* Effettuo la chiamata al CLLE preposto
050300101025     C                   call(e)   'TITVEVTC'
050400070411     C                   parm                    parccm
050500070411     C                   parm                    parmbr
050600070411     C                   parm      '2'           paropz
050700070411     C*
050800070411     C                   eval      wrkesito = '2'
050900070411     C*
051000070411     C                   seton                                        LR
051100070411     C*
051200070411     C                   ENDSR     '*CANCL'
051300070411     C***
051400070411
051500990910
051600000613     C     *inzsr        BEGSR
051700990910     C*
051800990910     C     *entry        plist
051900990920     C                   parm                    tivlrds
052000990921     C                   parm      wrkesito      esito
052100000724     C                   parm                    prmlit
052200000710     C                   parm                    prmfir
052300000613     C*
052400000830     C* CALCOLA LA DATA CORRENTE
052500000830     C                   time                    wn14             14 0
052600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
052700101025     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
052800140308     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
052900140308     C                   eval      datcor = %dec(%date() :*iso)
053000000830     C*
053100000613     C                   ENDSR
053200000613     C***
