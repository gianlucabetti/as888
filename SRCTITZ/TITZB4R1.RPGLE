000100140408      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200140221      *
000300140221      * PARTICOLARITA':
000400140221      *
000500140221     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700990910     Ftivin00r  uF   E             DISK    usropn
000800140408     FEDIVABwr  O    E             DISK    usropn
000900140408     FEDIVATwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800060307     D tisi95ds      e ds
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800140221     D curSped         s             15    INZ(*blanks)
002900140221     D depSped         s             15    INZ(*blanks)
003000140221     D w70             s             70    INZ(*blanks)
003100100118
003200100118     D*------------------
003300100118     D* DS REPERIMENTO NUMERATORE
003400100118     D*------------------
003500100118     D trul33ds      e ds                  inz
003600100118     D*------------------
003700100118     D* DS ARCHITETTURA
003800100118     D*------------------
003900100118     D kpjba         e ds                  inz
004000100118
004100010201
004200010201
004300090127     C*
004400000913     C                   reset                   rrnum
004500990921     C                   reset                   esito
004600990921     C                   reset                   wrkesito
004700000613     C*
004800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004900000613     C*
005000010202     C* Effettuo la chiamata al CLLE preposto
005100140408     C                   call(e)   'TITVEVTC'
005200010202     C                   parm                    parccm
005300010202     C                   parm                    parmbr
005400010202     C                   parm      '2'           paropz
005500050201     C*
005600050201     C* Effettuo lancio TISI95 solo x chiusura
005700050201     C                   CLEAR                   TISI95DS
005800050201     C                   EVAL      I95TLA = 'C'
005900050201     C                   CALL      'TISI95R'
006000050201     C                   PARM                    TISI95DS
006100000616     C*
006200000801     C
006300010201     C                   seton                                        LR
006400990908
006500000801
006600910830     C*--------------------------------------------------------
006700140408     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006800910830     C*--------------------------------------------------------
006900040526     C     RWFILE        BEGSR
007000990910     C*
007100990914     C                   if        not %open(tivin00r)
007200990908     C                   open      tivin00r
007300990914     C                   endif
007400140408     C                   if        not %open(EDIVABwr)
007500140408     C                   open      EDIVABwr
007600140408     C                   endif
007700140408     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007800020305     C                   exsr      prevat
007900010201     C*
008000010202     C                   if        chkcall = '0'
008100010202     C*
008200140408     C                   if        not %open(EDIVATwr)
008300140408     C                   open      EDIVATwr
008400010201     C                   endif
008500990910     C*
008600010201     C                   clear                   §CTROKVB          5 0
008700020305     C                   clear                   §CTROKVT          5 0
008800000801     C                   clear                   §CTRMO            5 0
008900000801     C                   clear                   §CTRNO            5 0
009000990910     C*
009100921023     C                   DO        *HIVAL
009200990913     C*
009300990915     C                   READ      tivin00r                               70
009400050627     C                   if        vindta > *blanks
009500000613     C                   add       1             rrnum
009600000801     C*
009700000801     C                   if        *in70 = *off
009800000801     C                             and
009900000801     C                             (vinflg = *blanks
010000000801     C                              or vinflg = '0'
010100000801     C                              or vinflg = '2')
010200000801     C*
010300000801     C                   clear                   vinmsg
010400000801     C                   eval      vinflg = '1'
010500070402     C*
010600070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010700140221     C                   eval      curSped = %subst(vindta:46:15)
010800070402     C                   if        curSped <> depSped
010900140408     C                   clear                   edivab00
011000140408     C                   clear                   edivat00
011100050628     C*
011200060315     C                   exsr      impvab                                       => carico  VAB
011300090127     C*
011400060315     C                   exsr      wrivab                                       => scarico VAB
011500070402     C*
011600070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011700140304     C                   exsr      exevata
011800070402     C*
011900070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
012000070402     C                   exsr      exevatb
012100140226     C*
012200140226     C* Indirizzo e-mail
012300140226     C                   exsr      exevati
012400140226     C*
012500140226     C* Cellulare per SMS
012600140226     C                   exsr      exevats
012700070402     C*
012800070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
012900070402     C                   exsr      exevate
013000070402     C*
013100070402     C* Salvo il raggruppamento spedizione cliente corrente
013200070402     C                   eval      depSped = curSped
013300070402     C*
013400070402     C                   else
013500070402     C*
013600070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013700070402     C                   exsr      exevate
013800070402     C                   endif
013900000905     C*
014000000905     C                   else
014100000905     C                   eval      vinflg = '1'
014200050628     C                   endif
014300000905     C                   endif
014400000905     C*
014500000905     C  N70              update    tivin000
014600000905     C*
014700991022     C  N70              ENDdo
014800140408     C*
014900140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
015000140408     C                   EVAL      VABCMR=%char(datcor)+'-'+%char(oracor)
015100140408     C                   EVAL      VABDCM = datcor
015200140408     C                   EVAL      VABDTS = datcor
015300140408     C                   EVAL      VABHMS = oracor
015400140408     C                   EVAL      VABCNT = 1
015500140408     C*
015600140408     C                   WRITE     EDIVAB00
015700010202     C*
015800010202     C                   endif
015900990910
016000990910     C* Se non ci sono record con errori ...
016100000710     C                   if        §ctrno = 0
016200990910     C* ... restituisco esito OK.
016300990921     C                   eval      wrkesito = '0'
016400990910     C                   else
016500010201     C                   if        §ctrokvb > 0
016600990921     C                   eval      wrkesito = '1'
016700000710     C                   else
016800000710     C                   eval      wrkesito = '2'
016900990910     C                   endif
017000000710     C                   endif
017100990910     C*
017200990914     C                   if        %open(tivin00r)
017300990908     C                   close     tivin00r
017400990914     C                   endif
017500140408     C                   if        %open(EDIVABwr)
017600140408     C                   close     EDIVABwr
017700140408     C                   endif
017800140408     C                   if        %open(EDIVATwr)
017900140408     C                   close     EDIVATwr
018000140408     C                   endif
018100990910     C*
018200010201     C                   if        §ctrokvb > 0
018300000724     C                             and vlrpoi <> *zeros
018400010202     C                   exsr      invio
018500990920     C                   endif
018600990920     C*
018700910830     C                   ENDSR
018800000613     C***
018900010305
019000010305     C*----------------------------------------------------*
019100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019200010305     C*----------------------------------------------------*
019300090127     C     WRIVAB        BEGSR
019400140408     C*
019500140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
019600140408     C                   EVAL      VABCMR=%char(datcor)+'-'+%char(oracor)
019700140408     C                   EVAL      VABDCM = datcor
019800140408     C                   EVAL      VABDTS = datcor
019900140408     C                   EVAL      VABHMS = oracor
020000140408     C                   EVAL      VABCNT = 1
020100090127     C*
020200090127     C* Quindi scarico il buffer del file d testata
020300140408     C                   WRITE     EDIVAB00
020400010305     C*
020500010305     C                   ENDSR
020600990920
020700000801     C*----------------------------------------------------*
020800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020900000801     C*----------------------------------------------------*
021000010201     C     INZVAR        BEGSR
021100000801     C*
021200040802     C                   Z-ADD     *zeros        Num5_0            5 0
021300040802     C                   MOVEL     '0'           FlgCAS            1
021400000801     C*
021500000801     C                   ENDSR
021600000801     C*----------------------------------------------------*
021700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021800000801     C*----------------------------------------------------*
021900000801     C     DEFCAM        BEGSR
022000070718     C*
022100070718     C* Imposto i valori di default...
022200140224     C                   Z-ADD     1662271       VABCCM
022300140224     C                   Z-ADD     1662271       VATCCM
022400140224     C                   Z-ADD     166           VABLNP
022500140224     C                   Z-ADD     166           VATLNP
022600140224     C                   Z-ADD     001           VABCTR
022700140224     C                   Z-ADD     001           VABNCL
022800140224     C                   MOVEL     '1'           VABCBO
022900140224     C                   MOVEL     '7Q'          VABCTM
023000020619     C* ... e poi verifico se sono stati passati come parametri
023100020619     C                   IF        vlrppt > *blanks
023200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023400020619     C                   EXSR      CHKNUM
023500020619     C                   IF        PiInt=*on
023600020619     C                   Z-ADD     PiVal         VABCCM
023700020619     C                   Z-ADD     PiVal         VATCCM
023800020619     C                   ENDIF
023900040506     C                   ENDIF
024000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024200020619     C                   EXSR      CHKNUM
024300020619     C                   IF        PiInt=*on
024400020619     C                   Z-ADD     PiVal         VABLNP
024500020619     C                   Z-ADD     PiVal         VATLNP
024600040506     C                   ENDIF
024700020619     C                   ENDIF
024800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025000020619     C                   EXSR      CHKNUM
025100020619     C                   IF        PiInt=*on
025200020619     C                   Z-ADD     PiVal         VABCTR
025300040506     C                   ENDIF
025400020619     C                   ENDIF
025500140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
025600140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
025700140224     C                   ENDIF
025800020619     C                   ENDIF
025900000801     C*
026000000801     C                   ENDSR
026100000801     C*----------------------------------------------------*
026200140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026300000801     C*----------------------------------------------------*
026400040823     C     IMPVAB        BEGSR
026500040823     C*
026600020305     C                   EXSR      INZVAR
026700020305     C                   EXSR      DEFCAM
026800010305     C*
026900000801     C                   Z-ADD     *zeros        errore            1 0
027000000830     C                   MOVEL     datcor        VABAAS
027100020305     C                   MOVEL     datcor        VATAAS
027200040526     C                   MOVE      datcor        VABMGS
027300040823     C                   MOVE(P)   vlrpoi        VABFGS
027400040823     C                   MOVE(P)   vlrpoi        VATFGS
027500050628     C*
027600140221     C                   EVAL      VABRSD=%trim(%subst(vindta:276:35))
027700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027900020117     C* ==
028000140224     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
028100140221     C                   EVAL      VABIND=%trim(%subst(vindta:341:35))
028200070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
028300070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
028400140224     C                   EVAL      VABRMA=%trim(%subst(vindta:13:15))
028500140304     C                   IF        %subst(vindta:514:30) <> *blank
028600140304     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
028700140304     C                   EVAL      VABNT2=%trim(%subst(vindta:663:50))
028800140304     C                   ELSE
028900140304     C                   EVAL      w70=*blank
029000140304     C                   EVAL      w70=%trim(%subst(vindta:663:50))
029100140304     C                   EVAL      VABNOT=%subst(w70:1:35)
029200140304     C                   EVAL      VABNT2=%subst(w70:36:35)
029300140304     C                   ENDIF
029400050628     C* CAP
029500140224     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
029600010201     C                   EXSR      CHKNUM
029700010201     C                   IF        PiInt=*on
029800140224     C                   Z-ADD     PiVal         Num5_0
029900040506     C                   MOVEL(P)  Num5_0        VABCAD
030000010201     C                   ELSE
030100040506     C                   ADD       1             errore
030200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030300040506     C                             + ' ' + 'VABCAD'
030400010201     C                   ENDIF
030500040506     C* Reperisco la provincia dal CAP e dalla località
030600040526     C                   IF        VABCAD <> *blanks AND
030700040526     C                             VABPRD  = *blanks
030800040506     C                   CLEAR                   TISI95DS
030900040506     C                   EVAL      I95TCN = '3'
031000040506     C                   Z-ADD     datcor        I95DAT
031100040506     C                   EVAL      I95CAP = VABCAD
031200040506     C                   EVAL      I95LOC = VABLOD
031300050627     C                   EVAL      I95NAR = VABNZD
031400040506     C                   CALL      'TISI95R'
031500040506     C                   PARM                    TISI95DS
031600040506     C                   EVAL      VABPRD = O95PRV
031700040506     C                   ENDIF
031800040506     C* PKB
031900070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
032000010201     C                   EXSR      CHKNUM
032100010201     C                   IF        PiNum=*on
032200070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
032300060316     C                   Z-ADD     PiVal         VABPKB
032400010201     C                   ELSE
032500010201     C                   ADD       1             errore
032600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032700010201     C                             + ' ' + 'VABPKB'
032800010201     C                   ENDIF
032900140224     C* RMN e NSP
033000140226     C                   EVAL      PiStr=%trim(%subst(vindta:46+11:9))
033100090511     C                   EXSR      CHKNUM
033200090511     C                   IF        PiInt=*on
033300090511     C                   Z-ADD     PiVal         VABRMN
033400140224     C                   Z-ADD     PiVal         VABNSP
033500140224     C                   Z-ADD     PiVal         VATNSP
033600090511     C                   ELSE
033700090511     C                   ADD       1             errore
033800090511     C                   EVAL      vinmsg = %trimr(vinmsg)
033900140224     C                             + ' ' + 'VABRMN VABNSP VATNSP'
034000090511     C                   ENDIF
034100060307     C* CAS
034200140224     C                   IF        %trim(%subst(vindta:502:9))<>'000000,00' AND
034300090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
034400090608     C                             %trim(%subst(vindta:502:9))<>*zeros
034500070102     C                   EVAL      VABVCA = 'EUR'
034600060307     C                   EVAL      FlgCAS = '1'
034700070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
034800060307     C                   EXSR      CHKNUM
034900060307     C                   IF        PiNum=*on
035000060316     C                   Z-ADD     PiVal         VABCAS
035100060307     C                   ELSE
035200060307     C                   ADD       1             errore
035300060307     C                   EVAL      vinmsg = %trimr(vinmsg)
035400060307     C                             + ' ' + 'VABCAS'
035500060307     C                   ENDIF
035600070102     C                   ENDIF
035700010205     C*
035800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
035900040802     C                   IF        FlgCAS <> '0'
036000070102     C                   IF        VABCBO = '1'
036100010205     C                   EVAL      VABCBO = '4'
036200010205     C                   ELSE
036300070102     C                   EVAL      VABCBO = '6'
036400070102     C                   ENDIF
036500140224     C                   EVAL      VABTIC = 'BM'
036600010205     C                   ENDIF
036700020305     C*
036800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036900011113     C                   EXSR      CHKIMPDIV
037000010202     C*
037100000801     C* Ebbene...
037200000801     C                   ADD       1             §CTRMO
037300010201     C                   IF        errore <> *zeros
037400000801     C                   ADD       1             §CTRNO
037500000801     C                   EVAL      vinflg = '2'
037600000801     C                   ELSE
037700010201     C                   ADD       1             §CTROKVB
037800000801     C                   ENDIF
037900000801     C*
038000000801     C                   ENDSR
038100070102     C*----------------------------------------------------*
038200140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
038300070102     C*----------------------------------------------------*
038400070102     C     EXEVATA       BEGSR
038500070102     C*
038600070102     C                   EXSR      INZVAR
038700070102     C                   EXSR      DEFCAM
038800070102     C*
038900070102     C                   EVAL      VATTRC='A'
039000140224     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
039100070102     C                   exsr      wrivat                                       => scarico VAT
039200070102     C*
039300070102     C                   ENDSR
039400060307     C*----------------------------------------------------*
039500140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
039600060307     C*----------------------------------------------------*
039700060307     C     EXEVATB       BEGSR
039800060307     C*
039900060307     C                   EXSR      INZVAR
040000060307     C                   EXSR      DEFCAM
040100060307     C*
040200060307     C                   EVAL      VATTRC='B'
040300140304     C                   EVAL      VATNOT = %trim(%subst(vindta:713:14))
040400060307     C                   exsr      wrivat                                       => scarico VAT
040500060307     C*
040600060307     C                   ENDSR
040700070402     C*----------------------------------------------------*
040800140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "E"
040900070402     C*----------------------------------------------------*
041000070402     C     EXEVATE       BEGSR
041100070402     C*
041200070402     C                   EXSR      INZVAR
041300070402     C                   EXSR      DEFCAM
041400070402     C*
041500070402     C                   EVAL      VATTRC='E'
041600140224     C                   EVAL      VATNOT = %trim(%subst(vindta:46:10))
041700070402     C                   exsr      wrivat                                       => scarico VAT
041800070402     C*
041900070402     C                   ENDSR
042000140224     C*----------------------------------------------------*
042100140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
042200140224     C*----------------------------------------------------*
042300140224     C     EXEVATI       BEGSR
042400140224     C*
042500140224     C                   EXSR      INZVAR
042600140224     C                   EXSR      DEFCAM
042700140224     C*
042800140224     C                   EVAL      VATTRC='I'
042900140224     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
043000140224     C                   exsr      wrivat                                       => scarico VAT
043100140224     C*
043200140224     C                   EVAL      VATTRC='J'
043300140224     C                   EVAL      VATNOT = %trim(%subst(vindta:663+35:15))
043400140224     C                   exsr      wrivat                                       => scarico VAT
043500140224     C*
043600140224     C                   ENDSR
043700140226     C*----------------------------------------------------*
043800140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "S"
043900140226     C*----------------------------------------------------*
044000140226     C     EXEVATS       BEGSR
044100140226     C*
044200140226     C                   EXSR      INZVAR
044300140226     C                   EXSR      DEFCAM
044400140226     C*
044500140226     C                   EVAL      VATTRC='S'
044600140226     C                   EVAL      VATNOT=*blank
044700140226     C                   EVAL      %subst(VATNOT:1:14) =
044800140226     C                              %trim(%subst(vindta:713:14))
044900140226     C                   IF        VATNOT <> *blank
045000140226     C                   EVAL      %subst(VATNOT:17:1) = 'S'
045100140226     C                   ENDIF
045200140226     C                   exsr      wrivat                                       => scarico VAT
045300140226     C*
045400140226     C                   ENDSR
045500010201     C*----------------------------------------------------*
045600140408     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
045700010201     C*----------------------------------------------------*
045800020305     C     WRIVAT        BEGSR
045900140408     C*
046000140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
046100140408     C                   EVAL      VATCMR=%char(datcor)+'-'+%char(oracor)
046200140408     C                   EVAL      VATCNT = 1
046300050628     C*
046400060223     C* Scrivo solo se valorizzato qualcosa
046500060223     C                   IF        VATNOT <> *blanks
046600140408     C                   WRITE     EDIVAT00
046700060223     C                   ENDIF
046800010201     C*
046900010201     C                   ENDSR
047000010202     C*----------------------------------------------------*
047100140408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
047200010202     C*----------------------------------------------------*
047300020305     C     PREVAT        BEGSR
047400010202     C*
047500140408     C* Compongo il nome del membro da dare al EDIVATWR
047600010202     C                   eval      parmbr = vlrhdl
047700010202     C                   movel     'M'           parmbr
047800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
047900010202     C                   eval      paropz = '1'
048000010202     C* Effettuo la chiamata al CLLE preposto
048100140408     C                   call(e)   'TITVEVTC'
048200010202     C                   parm                    parccm
048300010202     C                   parm                    parmbr
048400010202     C                   parm                    paropz
048500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048600010202     C                   if        %error
048700010202     C                   movel     '1'           chkcall
048800010202     C                   else
048900010202     C                   movel     '0'           chkcall
049000010202     C                   endif
049100010202     C*
049200010202     C                   ENDSR
049300000801     C*----------------------------------------------------*
049400000801     C*  CONTROLLO NUMERICITA' CAMPI
049500000801     C*----------------------------------------------------*
049600000801     C     CHKNUM        BEGSR
049700000801     C*
049800000801     C                   call(e)   'ISNUMERIC'
049900000801     C                   PARM                    PiStr            30
050000140226     C                   PARM      '.'           PiDecChr          1
050100000801     C                   PARM      *ZEROS        PiVal            30 9
050200000801     C                   PARM      '0'           PiInt             1
050300000801     C                   PARM      '0'           PiNum             1
050400000801     C                   IF        %error
050500000801     C                   EVAL      PiInt=*off
050600000801     C                   ENDIF
050700000801     C*
050800000801     C                   ENDSR
050900000801     C***
051000000801
051100011113
051200011113     C*----------------------------------------------------*
051300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051400011113     C*----------------------------------------------------*
051500011113     C     CHKIMPDIV     BEGSR
051600011113     C*
051700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051800011113     C                   Z-ADD     *zeros        wrkDec            9 9
051900011113     C*
052000011113     C* Come prima cosa effettuo considerazioni sulla divisa
052100011113     C                   IF        vabIAS > *zeros
052200011113     C                   IF        vabVAS <> 'EUR'
052300011113     C                   EVAL      vabVAS =  'ITL'
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C                   IF        vabCAS > *zeros
052800011113     C                   IF        vabVCA <> 'EUR'
052900011113     C                   EVAL      vabVCA =  'ITL'
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C                   IF        vabVMD > *zeros
053400020305     C                   IF        vabVAD <> 'EUR'
053500011113     C                   EVAL      vabVAD =  'ITL'
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054000011113     C                   Z-ADD     vabIAS        wrkDec
054100011113     C                   IF        wrkDec > *zeros
054200011113     C                   IF        vabVAS = 'ITL'
054300011113     C                   EVAL      vabIAS = *zeros
054400011113     C                   ENDIF
054500011113     C                   ENDIF
054600011113     C*
054700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054800011113     C                   Z-ADD     vabCAS        wrkDec
054900011113     C                   IF        wrkDec > *zeros
055000011113     C                   IF        vabVCA = 'ITL'
055100011113     C                   EVAL      vabCAS = *zeros
055200011113     C                   ENDIF
055300011113     C                   ENDIF
055400011113     C*
055500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055600011113     C                   Z-ADD     vabVMD        wrkDec
055700011113     C                   IF        wrkDec > *zeros
055800011113     C                   IF        vabVAD = 'ITL'
055900011113     C                   EVAL      vabVMD = *zeros
056000011113     C                   ENDIF
056100011113     C                   ENDIF
056200011113     C*
056300011113     C                   ENDSR
056400011113     C***
056500011113
056600011113
056700000801
056800000801
056900990920      /TITLE Invio dei dati al punto operativo.
057000010202     C     invio         BEGSR
057100990920     C*
057200140408     C* 1° invio EDIVAT
057300010201     C                   reset                   dscmz
057400010201     C                   move      vlrpoi        cmzdst
057500140408     C                   eval      cmzfld = 'EDIVATWR'
057600010201     C                   eval      cmzmbd = vlrhdl
057700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057800021009     C***                if        prmfir = *blanks
057900140408     C                   eval      cmzfla = 'EDIVAT0F'
058000140408     C                   eval      cmzmba = 'EDIVAT0F'
058100021009     C***                else
058200021009     C***                eval      cmzfla = prmfir
058300021009     C***                eval      cmzmba = prmfir
058400021009     C***                endif
058500010201     C                   eval      cmznrr = *zeros
058600020305     C                   move      §ctrokvt      cmznrr
058700021018     C                   eval      cmzlba = vlrfl1
058800010201     C                   call(e)   'TIS711C'
058900010201     C                   parm                    dscmz
059000010201     C                   parm      *blanks       esito
059100010205     C                   if        %error
059200010205     C                             or cmzerr = '1'
059300010205     C                             or esito  = '1'
059400010205     C                   eval      wrkesito = '3'
059500010205     C                   else
059600010201     C*
059700140408     C* 2° invio EùDIVAB
059800010201     C                   reset                   dscmz
059900010201     C                   move      vlrpoi        cmzdst
060000010201     C                   eval      cmzfld = vlrfou
060100010201     C                   eval      cmzmbd = vlrhdl
060200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060300021009     C***                if        prmfir = *blanks
060400140408     C                   eval      cmzfla = 'EDIVAB0F'
060500140408     C                   eval      cmzmba = 'EDIVAB0F'
060600021009     C***                else
060700021009     C***                eval      cmzfla = prmfir
060800021009     C***                eval      cmzmba = prmfir
060900021009     C***                endif
061000010201     C                   eval      cmznrr = *zeros
061100010201     C                   move      §ctrokvb      cmznrr
061200021018     C                   eval      cmzlba = vlrfl1
061300010201     C                   call(e)   'TIS711C'
061400010201     C                   parm                    dscmz
061500010201     C                   parm      *blanks       esito
061600010201     C                   if        %error
061700010201     C                             or cmzerr = '1'
061800010201     C                             or esito  = '1'
061900010201     C                   eval      wrkesito = '3'
062000010201     C                   endif
062100010205     C                   endif
062200990920     C*
062300000613     C                   ENDSR
062400000613     C***
062500990910
062600000613     C     *inzsr        BEGSR
062700990910     C*
062800990910     C     *entry        plist
062900990920     C                   parm                    tivlrds
063000990921     C                   parm      wrkesito      esito
063100000724     C                   parm                    prmlit
063200000710     C                   parm                    prmfir
063300000613     C*
063400000830     C* CALCOLA LA DATA CORRENTE
063500140408     C                   time                    wn14             14 0
063600140408     C                   movel     wn14          oracor            6 0          *ORA
063700100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
063800100118     C                   eval      datcor = %dec(%date() : *ISO)
063900000830     C*
064000000613     C                   ENDSR
064100000613     C***
