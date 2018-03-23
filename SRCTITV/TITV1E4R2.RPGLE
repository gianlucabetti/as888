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
002800160210     D curSped         s              6    INZ(*blanks)
002900160210     D depSped         s              6    INZ(*blanks)
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
010700160210     C                   eval      curSped = %subst(vindta:114:6)
010800070402     C                   if        curSped <> depSped
010900140408     C                   clear                   edivab00
011000140408     C                   clear                   edivat00
011100050628     C*
011200060315     C                   exsr      impvab                                       => carico  VAB
011300070402     C*
011400070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011500160210     C***                exsr      exevata
011600070402     C*
011700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
011800070402     C                   exsr      exevatb
011900140226     C*
012000140226     C* Indirizzo e-mail
012100160210     C***                exsr      exevati
012200160517     C*
012300160517     C* Packing-list PDF
012400160517     C                   exsr      exevatp
012500140226     C*
012600140226     C* Cellulare per SMS
012700160210     C***                exsr      exevats
012800070402     C*
012900070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013000160210     C***                exsr      exevate
013100160517     C*
013200160517     C* Scarico buffer di output
013300160517     C                   exsr      wrivab                                       => scarico VAB
013400070402     C*
013500070402     C* Salvo il raggruppamento spedizione cliente corrente
013600070402     C                   eval      depSped = curSped
013700070402     C*
013800070402     C                   else
013900070402     C*
014000070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
014100160210     C***                exsr      exevate
014200070402     C                   endif
014300000905     C*
014400000905     C                   else
014500000905     C                   eval      vinflg = '1'
014600050628     C                   endif
014700000905     C                   endif
014800000905     C*
014900000905     C  N70              update    tivin000
015000000905     C*
015100991022     C  N70              ENDdo
015200140408     C*
015300160210     C***
015400160210     C**VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
015500160210     C***                EVAL      VABCMR=%char(datcor)+'-'+%char(oracor)
015600160210     C***                EVAL      VABDCM = datcor
015700160210     C***                EVAL      VABDTS = datcor
015800160210     C***                EVAL      VABHMS = oracor
015900160210     C***                EVAL      VABCNT = 1
016000160210     C***
016100160210     C***                WRITE     EDIVAB00
016200160210     C***
016300010202     C*
016400010202     C                   endif
016500990910
016600990910     C* Se non ci sono record con errori ...
016700000710     C                   if        §ctrno = 0
016800990910     C* ... restituisco esito OK.
016900990921     C                   eval      wrkesito = '0'
017000990910     C                   else
017100010201     C                   if        §ctrokvb > 0
017200990921     C                   eval      wrkesito = '1'
017300000710     C                   else
017400000710     C                   eval      wrkesito = '2'
017500990910     C                   endif
017600000710     C                   endif
017700990910     C*
017800990914     C                   if        %open(tivin00r)
017900990908     C                   close     tivin00r
018000990914     C                   endif
018100140408     C                   if        %open(EDIVABwr)
018200140408     C                   close     EDIVABwr
018300140408     C                   endif
018400140408     C                   if        %open(EDIVATwr)
018500140408     C                   close     EDIVATwr
018600140408     C                   endif
018700990910     C*
018800010201     C                   if        §ctrokvb > 0
018900000724     C                             and vlrpoi <> *zeros
019000010202     C                   exsr      invio
019100990920     C                   endif
019200990920     C*
019300910830     C                   ENDSR
019400000613     C***
019500010305
019600010305     C*----------------------------------------------------*
019700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019800010305     C*----------------------------------------------------*
019900090127     C     WRIVAB        BEGSR
020000140408     C*
020100140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
020200140408     C                   EVAL      VABCMR=%char(datcor)+'-'+%char(oracor)
020300140408     C                   EVAL      VABDCM = datcor
020400140408     C                   EVAL      VABDTS = datcor
020500140408     C                   EVAL      VABHMS = oracor
020600140408     C                   EVAL      VABCNT = 1
020700090127     C*
020800090127     C* Quindi scarico il buffer del file d testata
020900140408     C                   WRITE     EDIVAB00
021000010305     C*
021100010305     C                   ENDSR
021200990920
021300000801     C*----------------------------------------------------*
021400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021500000801     C*----------------------------------------------------*
021600010201     C     INZVAR        BEGSR
021700000801     C*
021800040802     C                   Z-ADD     *zeros        Num5_0            5 0
021900040802     C                   MOVEL     '0'           FlgCAS            1
022000000801     C*
022100000801     C                   ENDSR
022200000801     C*----------------------------------------------------*
022300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
022400000801     C*----------------------------------------------------*
022500000801     C     DEFCAM        BEGSR
022600070718     C*
022700070718     C* Imposto i valori di default...
022800160210     C                   EVAL      VABCCM = 0891093
022900160210     C                   EVAL      VABLNP = 089
023000160210     C                   EVAL      VATCCM = 0891093
023100160210     C                   EVAL      VATLNP = 089
023200160210     C                   EVAL      VABCTR = 000
023300160210     C                   EVAL      VABTSP = 'C'
023400160210     C                   EVAL      VABCBO = '1'
023500020619     C* ... e poi verifico se sono stati passati come parametri
023600020619     C                   IF        vlrppt > *blanks
023700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023900020619     C                   EXSR      CHKNUM
024000020619     C                   IF        PiInt=*on
024100020619     C                   Z-ADD     PiVal         VABCCM
024200020619     C                   Z-ADD     PiVal         VATCCM
024300020619     C                   ENDIF
024400040506     C                   ENDIF
024500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024700020619     C                   EXSR      CHKNUM
024800020619     C                   IF        PiInt=*on
024900020619     C                   Z-ADD     PiVal         VABLNP
025000020619     C                   Z-ADD     PiVal         VATLNP
025100040506     C                   ENDIF
025200020619     C                   ENDIF
025300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
025400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025500020619     C                   EXSR      CHKNUM
025600020619     C                   IF        PiInt=*on
025700020619     C                   Z-ADD     PiVal         VABCTR
025800040506     C                   ENDIF
025900020619     C                   ENDIF
026000140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
026100140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
026200140224     C                   ENDIF
026300020619     C                   ENDIF
026400000801     C*
026500000801     C                   ENDSR
026600000801     C*----------------------------------------------------*
026700140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026800000801     C*----------------------------------------------------*
026900040823     C     IMPVAB        BEGSR
027000040823     C*
027100020305     C                   EXSR      INZVAR
027200020305     C                   EXSR      DEFCAM
027300010305     C*
027400000801     C                   Z-ADD     *zeros        errore            1 0
027500000830     C                   MOVEL     datcor        VABAAS
027600020305     C                   MOVEL     datcor        VATAAS
027700040526     C                   MOVE      datcor        VABMGS
027800040823     C                   MOVE(P)   vlrpoi        VABFGS
027900040823     C                   MOVE(P)   vlrpoi        VATFGS
028000160210     C*
028100160210     C* Reperimento campi ALFA
028200160210     C*
028300160210     C* Considerazioni sulla ragione sociale del destinatario da indicare
028400160210     C                   EVAL      VABRSD=%trim(%subst(vindta:14:35))
028500160210     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
028600160210     C     '@':'A'       XLATE     VABRSD        VABRSD
028700160210     C* ==
028800160210     C                   EVAL      VABIND=%trim(%subst(vindta:49:35))
028900160210     C                   EVAL      VABLOD=%trim(%subst(vindta:84:20))
029000160210     C                   EVAL      VABPRD=%trim(%subst(vindta:109:2))
029100160210     C                   EVAL      VABRMA=%trim(%subst(vindta:133:6))
029200160210     C                   EVAL      VABTC1=%trim(%subst(vindta:166:1))
029300160210     C*
029400160210     C* Reperimento campi NUMERICI
029500160210     C*
029600160210     C* Considerazioni relative al CCM
029700160210     C                   IF        %trim(%subst(vindta:151:1)) = '*'
029800160210     C                   EVAL      VABCCM=0891092
029900160210     C                   EVAL      VATCCM=0891092
030000160210     C                   ENDIF
030100160210     C* NSP/RMN
030200160210     C                   EVAL      PiStr=%trim(%subst(vindta:114:6))
030300160210     C                   EXSR      CHKNUM
030400160210     C                   IF        PiInt=*on
030500160210     C                   Z-ADD     PiVal         VABNSP
030600160210     C                   Z-ADD     PiVal         VATNSP
030700160210     C                   Z-ADD     PiVal         VABRMN
030800160210     C                   ELSE
030900160210     C                   ADD       1             errore
031000160210     C                   Z-ADD     *zeros        VABNSP
031100160210     C                   Z-ADD     *zeros        VATNSP
031200160210     C                   Z-ADD     1             VABRMN
031300160210     C                   EVAL      vinmsg = %trimr(vinmsg)
031400160210     C                             + ' ' + 'VABNSP VATNSP VABRMN'
031500160210     C                   ENDIF
031600160210     C* CAD
031700160210     C                   EVAL      PiStr=%trim(%subst(vindta:104:5))
031800160210     C                   EXSR      CHKNUM
031900160210     C                   IF        PiInt=*on
032000160210     C                   Z-ADD     PiVal         Num5_0
032100160210     C                   MOVEL(p)  Num5_0        VABCAD
032200160210     C                   ELSE
032300160210     C                   ADD       1             errore
032400160210     C                   EVAL      VABCAD = *zeros
032500160210     C                   EVAL      vinmsg = %trimr(vinmsg)
032600160210     C                             + ' ' + 'VABCAD'
032700160210     C                   ENDIF
032800160210     C* Reperisco la provincia dal CAP e dalla località
032900160210     C                   IF        VABCAD <> *blanks AND
033000160210     C                             VABPRD  = *blanks
033100160210     C                   CLEAR                   TISI95DS
033200160210     C                   EVAL      I95TCN = '3'
033300160210     C                   Z-ADD     datcor        I95DAT
033400160210     C                   EVAL      I95CAP = VABCAD
033500160210     C                   EVAL      I95LOC = VABLOD
033600160210     C                   EVAL      I95NAR = VABNZD
033700160210     C                   CALL      'TISI95R'
033800160210     C                   PARM                    TISI95DS
033900160210     C                   EVAL      VABPRD = O95PRV
034000160210     C                   ENDIF
034100160210     C* NCL
034200160210     C                   EVAL      PiStr=%trim(%subst(vindta:139:3))
034300160210     C                   EXSR      CHKNUM
034400160210     C                   IF        PiInt=*on
034500160210     C                   Z-ADD     PiVal         VABNCL
034600160210     C                   ELSE
034700160210     C                   ADD       1             errore
034800160210     C                   Z-ADD     *zeros        VABNCL
034900160210     C                   EVAL      vinmsg = %trimr(vinmsg)
035000160210     C                             + ' ' + 'VABNCL'
035100160210     C                   ENDIF
035200160210     C* PKB
035300160210     C                   EVAL      PiStr=%trim(%subst(vindta:142:6))
035400160210     C                   EXSR      CHKNUM
035500160210     C                   IF        PiNum=*on
035600160210     C                   Z-ADD(H)  PiVal         VABPKB
035700160210     C                   ELSE
035800160210     C                   ADD       1             errore
035900160210     C                   Z-ADD     *zeros        VABPKB
036000160210     C                   EVAL      vinmsg = %trimr(vinmsg)
036100160210     C                             + ' ' + 'VABPKB'
036200160210     C                   ENDIF
036300010205     C*
036400010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036500040802     C                   IF        FlgCAS <> '0'
036600070102     C                   IF        VABCBO = '1'
036700010205     C                   EVAL      VABCBO = '4'
036800010205     C                   ELSE
036900070102     C                   EVAL      VABCBO = '6'
037000070102     C                   ENDIF
037100140224     C                   EVAL      VABTIC = 'BM'
037200010205     C                   ENDIF
037300020305     C*
037400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037500011113     C                   EXSR      CHKIMPDIV
037600010202     C*
037700000801     C* Ebbene...
037800000801     C                   ADD       1             §CTRMO
037900010201     C                   IF        errore <> *zeros
038000000801     C                   ADD       1             §CTRNO
038100000801     C                   EVAL      vinflg = '2'
038200000801     C                   ELSE
038300010201     C                   ADD       1             §CTROKVB
038400000801     C                   ENDIF
038500000801     C*
038600000801     C                   ENDSR
038700070102     C*----------------------------------------------------*
038800140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
038900070102     C*----------------------------------------------------*
039000070102     C     EXEVATA       BEGSR
039100070102     C*
039200070102     C                   EXSR      INZVAR
039300070102     C                   EXSR      DEFCAM
039400070102     C*
039500070102     C                   EVAL      VATTRC='A'
039600160210     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
039700070102     C                   exsr      wrivat                                       => scarico VAT
039800070102     C*
039900070102     C                   ENDSR
040000060307     C*----------------------------------------------------*
040100140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
040200060307     C*----------------------------------------------------*
040300060307     C     EXEVATB       BEGSR
040400060307     C*
040500060307     C                   EXSR      INZVAR
040600060307     C                   EXSR      DEFCAM
040700060307     C*
040800060307     C                   EVAL      VATTRC='B'
040900160210     C                   EVAL      VATNOT = %trim(%subst(vindta:167:20))
041000060307     C                   exsr      wrivat                                       => scarico VAT
041100060307     C*
041200060307     C                   ENDSR
041300070402     C*----------------------------------------------------*
041400140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "E"
041500070402     C*----------------------------------------------------*
041600070402     C     EXEVATE       BEGSR
041700070402     C*
041800070402     C                   EXSR      INZVAR
041900070402     C                   EXSR      DEFCAM
042000070402     C*
042100070402     C                   EVAL      VATTRC='E'
042200160210     C***                EVAL      VATNOT = %trim(%subst(vindta:46:10))
042300070402     C                   exsr      wrivat                                       => scarico VAT
042400070402     C*
042500070402     C                   ENDSR
042600140224     C*----------------------------------------------------*
042700140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
042800140224     C*----------------------------------------------------*
042900140224     C     EXEVATI       BEGSR
043000140224     C*
043100140224     C                   EXSR      INZVAR
043200140224     C                   EXSR      DEFCAM
043300140224     C*
043400140224     C                   EVAL      VATTRC='I'
043500160210     C***                EVAL      VATNOT = %trim(%subst(vindta:663:35))
043600140224     C                   exsr      wrivat                                       => scarico VAT
043700140224     C*
043800140224     C                   EVAL      VATTRC='J'
043900140224     C                   EVAL      VATNOT = %trim(%subst(vindta:663+35:15))
044000140224     C                   exsr      wrivat                                       => scarico VAT
044100140224     C*
044200140224     C                   ENDSR
044300140226     C*----------------------------------------------------*
044400140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "S"
044500140226     C*----------------------------------------------------*
044600140226     C     EXEVATS       BEGSR
044700140226     C*
044800140226     C                   EXSR      INZVAR
044900140226     C                   EXSR      DEFCAM
045000140226     C*
045100140226     C                   EVAL      VATTRC='S'
045200140226     C                   EVAL      VATNOT=*blank
045300160210     C***                EVAL      %subst(VATNOT:1:14) =
045400160210     C***                           %trim(%subst(vindta:713:14))
045500140226     C                   IF        VATNOT <> *blank
045600140226     C                   EVAL      %subst(VATNOT:17:1) = 'S'
045700140226     C                   ENDIF
045800140226     C                   exsr      wrivat                                       => scarico VAT
045900140226     C*
046000140226     C                   ENDSR
046100160517     C*----------------------------------------------------*
046200160517     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "P"
046300160517     C*----------------------------------------------------*
046400160517     C     EXEVATP       BEGSR
046500160517     C*
046600160517     C                   EXSR      INZVAR
046700160517     C                   EXSR      DEFCAM
046800160517     C*
046900160517     C                   IF        %trim(%subst(vindta:187:16)) <> *blanks
047000160517     C                   EVAL      VATTRC='P'
047100160517     C                   EVAL      VATNOT=%trim(%subst(vindta:187:16))+'.pdf'
047200160517     C                   EVAL      %subst(VATNOT:34:2) = '10'
047300160517     C                   exsr      wrivat                                       => scarico VAT
047400160517     C                   select
047500160517     C                   when      VABCTM = *blanks
047600160517     C                   eval      VABCTM = '7A'
047700160517     C                   when      VABCTM = '7Q'
047800160517     C                   eval      VABCTM = '7T'
047900160517     C                   endsl
048000160517     C                   ENDIF
048100160517     C*
048200160517     C                   ENDSR
048300010201     C*----------------------------------------------------*
048400140408     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
048500010201     C*----------------------------------------------------*
048600020305     C     WRIVAT        BEGSR
048700140408     C*
048800140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
048900140408     C                   EVAL      VATCMR=%char(datcor)+'-'+%char(oracor)
049000140408     C                   EVAL      VATCNT = 1
049100050628     C*
049200060223     C* Scrivo solo se valorizzato qualcosa
049300060223     C                   IF        VATNOT <> *blanks
049400140408     C                   WRITE     EDIVAT00
049500060223     C                   ENDIF
049600010201     C*
049700010201     C                   ENDSR
049800010202     C*----------------------------------------------------*
049900140408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
050000010202     C*----------------------------------------------------*
050100020305     C     PREVAT        BEGSR
050200010202     C*
050300140408     C* Compongo il nome del membro da dare al EDIVATWR
050400010202     C                   eval      parmbr = vlrhdl
050500010202     C                   movel     'M'           parmbr
050600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050700010202     C                   eval      paropz = '1'
050800010202     C* Effettuo la chiamata al CLLE preposto
050900140408     C                   call(e)   'TITVEVTC'
051000010202     C                   parm                    parccm
051100010202     C                   parm                    parmbr
051200010202     C                   parm                    paropz
051300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051400010202     C                   if        %error
051500010202     C                   movel     '1'           chkcall
051600010202     C                   else
051700010202     C                   movel     '0'           chkcall
051800010202     C                   endif
051900010202     C*
052000010202     C                   ENDSR
052100000801     C*----------------------------------------------------*
052200000801     C*  CONTROLLO NUMERICITA' CAMPI
052300000801     C*----------------------------------------------------*
052400000801     C     CHKNUM        BEGSR
052500160210     C*
052600160210     C                   IF        PiDecChr = *blanks
052700160210     C                   EVAL      PiDecChr = '.'
052800160210     C                   ENDIF
052900000801     C*
053000000801     C                   call(e)   'ISNUMERIC'
053100000801     C                   PARM                    PiStr            30
053200160210     C                   PARM                    PiDecChr          1
053300000801     C                   PARM      *ZEROS        PiVal            30 9
053400000801     C                   PARM      '0'           PiInt             1
053500000801     C                   PARM      '0'           PiNum             1
053600000801     C                   IF        %error
053700000801     C                   EVAL      PiInt=*off
053800000801     C                   ENDIF
053900000801     C*
054000000801     C                   ENDSR
054100000801     C***
054200000801
054300011113
054400011113     C*----------------------------------------------------*
054500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054600011113     C*----------------------------------------------------*
054700011113     C     CHKIMPDIV     BEGSR
054800011113     C*
054900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055000011113     C                   Z-ADD     *zeros        wrkDec            9 9
055100011113     C*
055200011113     C* Come prima cosa effettuo considerazioni sulla divisa
055300011113     C                   IF        vabIAS > *zeros
055400011113     C                   IF        vabVAS <> 'EUR'
055500011113     C                   EVAL      vabVAS =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C                   IF        vabCAS > *zeros
056000011113     C                   IF        vabVCA <> 'EUR'
056100011113     C                   EVAL      vabVCA =  'ITL'
056200011113     C                   ENDIF
056300011113     C                   ENDIF
056400011113     C*
056500011113     C                   IF        vabVMD > *zeros
056600020305     C                   IF        vabVAD <> 'EUR'
056700011113     C                   EVAL      vabVAD =  'ITL'
056800011113     C                   ENDIF
056900011113     C                   ENDIF
057000011113     C*
057100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057200011113     C                   Z-ADD     vabIAS        wrkDec
057300011113     C                   IF        wrkDec > *zeros
057400011113     C                   IF        vabVAS = 'ITL'
057500011113     C                   EVAL      vabIAS = *zeros
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058000011113     C                   Z-ADD     vabCAS        wrkDec
058100011113     C                   IF        wrkDec > *zeros
058200011113     C                   IF        vabVCA = 'ITL'
058300011113     C                   EVAL      vabCAS = *zeros
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058800011113     C                   Z-ADD     vabVMD        wrkDec
058900011113     C                   IF        wrkDec > *zeros
059000011113     C                   IF        vabVAD = 'ITL'
059100011113     C                   EVAL      vabVMD = *zeros
059200011113     C                   ENDIF
059300011113     C                   ENDIF
059400011113     C*
059500011113     C                   ENDSR
059600011113     C***
059700011113
059800011113
059900000801
060000000801
060100990920      /TITLE Invio dei dati al punto operativo.
060200010202     C     invio         BEGSR
060300990920     C*
060400140408     C* 1° invio EDIVAT
060500010201     C                   reset                   dscmz
060600010201     C                   move      vlrpoi        cmzdst
060700140408     C                   eval      cmzfld = 'EDIVATWR'
060800010201     C                   eval      cmzmbd = vlrhdl
060900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061000021009     C***                if        prmfir = *blanks
061100140408     C                   eval      cmzfla = 'EDIVAT0F'
061200140408     C                   eval      cmzmba = 'EDIVAT0F'
061300021009     C***                else
061400021009     C***                eval      cmzfla = prmfir
061500021009     C***                eval      cmzmba = prmfir
061600021009     C***                endif
061700010201     C                   eval      cmznrr = *zeros
061800020305     C                   move      §ctrokvt      cmznrr
061900021018     C                   eval      cmzlba = vlrfl1
062000010201     C                   call(e)   'TIS711C'
062100010201     C                   parm                    dscmz
062200010201     C                   parm      *blanks       esito
062300010205     C                   if        %error
062400010205     C                             or cmzerr = '1'
062500010205     C                             or esito  = '1'
062600010205     C                   eval      wrkesito = '3'
062700010205     C                   else
062800010201     C*
062900140408     C* 2° invio EùDIVAB
063000010201     C                   reset                   dscmz
063100010201     C                   move      vlrpoi        cmzdst
063200010201     C                   eval      cmzfld = vlrfou
063300010201     C                   eval      cmzmbd = vlrhdl
063400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063500021009     C***                if        prmfir = *blanks
063600140408     C                   eval      cmzfla = 'EDIVAB0F'
063700140408     C                   eval      cmzmba = 'EDIVAB0F'
063800021009     C***                else
063900021009     C***                eval      cmzfla = prmfir
064000021009     C***                eval      cmzmba = prmfir
064100021009     C***                endif
064200010201     C                   eval      cmznrr = *zeros
064300010201     C                   move      §ctrokvb      cmznrr
064400021018     C                   eval      cmzlba = vlrfl1
064500010201     C                   call(e)   'TIS711C'
064600010201     C                   parm                    dscmz
064700010201     C                   parm      *blanks       esito
064800010201     C                   if        %error
064900010201     C                             or cmzerr = '1'
065000010201     C                             or esito  = '1'
065100010201     C                   eval      wrkesito = '3'
065200010201     C                   endif
065300010205     C                   endif
065400990920     C*
065500000613     C                   ENDSR
065600000613     C***
065700990910
065800000613     C     *inzsr        BEGSR
065900990910     C*
066000990910     C     *entry        plist
066100990920     C                   parm                    tivlrds
066200990921     C                   parm      wrkesito      esito
066300000724     C                   parm                    prmlit
066400000710     C                   parm                    prmfir
066500000613     C*
066600000830     C* CALCOLA LA DATA CORRENTE
066700140408     C                   time                    wn14             14 0
066800140408     C                   movel     wn14          oracor            6 0          *ORA
066900100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
067000100118     C                   eval      datcor = %dec(%date() : *ISO)
067100000830     C*
067200000613     C                   ENDSR
067300000613     C***
