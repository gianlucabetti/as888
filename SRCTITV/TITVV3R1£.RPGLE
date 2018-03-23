000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200140221      *
000300140221      * PARTICOLARITA':
000400140221      *
000500140221     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700990910     Ftivin00r  uF   E             DISK    usropn
000800021113     FFIVABwwr  O    E             DISK    usropn
000900021113     FFIVATwwr  O    E             DISK    usropn
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
005100040506     C                   call(e)   'TITVVTC'
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
006700040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006800910830     C*--------------------------------------------------------
006900040526     C     RWFILE        BEGSR
007000990910     C*
007100990914     C                   if        not %open(tivin00r)
007200990908     C                   open      tivin00r
007300990914     C                   endif
007400021113     C                   if        not %open(fivabwwr)
007500021113     C                   open      fivabwwr
007600990914     C                   endif
007700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007800020305     C                   exsr      prevat
007900010201     C*
008000010202     C                   if        chkcall = '0'
008100010202     C*
008200021113     C                   if        not %open(fivatwwr)
008300021113     C                   open      fivatwwr
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
010900070402     C                   clear                   fivab000
011000070402     C                   clear                   fivat000
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
014800010202     C*
014900010202     C                   endif
015000990910
015100990910     C* Se non ci sono record con errori ...
015200000710     C                   if        §ctrno = 0
015300990910     C* ... restituisco esito OK.
015400990921     C                   eval      wrkesito = '0'
015500990910     C                   else
015600010201     C                   if        §ctrokvb > 0
015700990921     C                   eval      wrkesito = '1'
015800000710     C                   else
015900000710     C                   eval      wrkesito = '2'
016000990910     C                   endif
016100000710     C                   endif
016200990910     C*
016300990914     C                   if        %open(tivin00r)
016400990908     C                   close     tivin00r
016500990914     C                   endif
016600021113     C                   if        %open(fivabwwr)
016700021113     C                   close     fivabwwr
016800990914     C                   endif
016900021113     C                   if        %open(fivatwwr)
017000021113     C                   close     fivatwwr
017100010201     C                   endif
017200990910     C*
017300010201     C                   if        §ctrokvb > 0
017400000724     C                             and vlrpoi <> *zeros
017500010202     C                   exsr      invio
017600990920     C                   endif
017700990920     C*
017800910830     C                   ENDSR
017900000613     C***
018000010305
018100010305     C*----------------------------------------------------*
018200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018300010305     C*----------------------------------------------------*
018400090127     C     WRIVAB        BEGSR
018500090127     C*
018600090127     C* Quindi scarico il buffer del file d testata
018700090127     C                   write     fivab000                                     => scarico il VAB
018800010305     C*
018900010305     C                   ENDSR
019000990920
019100000801     C*----------------------------------------------------*
019200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019300000801     C*----------------------------------------------------*
019400010201     C     INZVAR        BEGSR
019500000801     C*
019600040802     C                   Z-ADD     *zeros        Num5_0            5 0
019700040802     C                   MOVEL     '0'           FlgCAS            1
019800000801     C*
019900000801     C                   ENDSR
020000000801     C*----------------------------------------------------*
020100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020200000801     C*----------------------------------------------------*
020300000801     C     DEFCAM        BEGSR
020400070718     C*
020500070718     C* Imposto i valori di default...
020600140224     C                   Z-ADD     1662271       VABCCM
020700140224     C                   Z-ADD     1662271       VATCCM
020800140224     C                   Z-ADD     166           VABLNP
020900140224     C                   Z-ADD     166           VATLNP
021000140224     C                   Z-ADD     001           VABCTR
021100140224     C                   Z-ADD     001           VABNCL
021200140224     C                   MOVEL     '1'           VABCBO
021300140224     C                   MOVEL     '7Q'          VABCTM
021400020619     C* ... e poi verifico se sono stati passati come parametri
021500020619     C                   IF        vlrppt > *blanks
021600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021800020619     C                   EXSR      CHKNUM
021900020619     C                   IF        PiInt=*on
022000020619     C                   Z-ADD     PiVal         VABCCM
022100020619     C                   Z-ADD     PiVal         VATCCM
022200020619     C                   ENDIF
022300040506     C                   ENDIF
022400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022600020619     C                   EXSR      CHKNUM
022700020619     C                   IF        PiInt=*on
022800020619     C                   Z-ADD     PiVal         VABLNP
022900020619     C                   Z-ADD     PiVal         VATLNP
023000040506     C                   ENDIF
023100020619     C                   ENDIF
023200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023400020619     C                   EXSR      CHKNUM
023500020619     C                   IF        PiInt=*on
023600020619     C                   Z-ADD     PiVal         VABCTR
023700040506     C                   ENDIF
023800020619     C                   ENDIF
023900140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
024000140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
024100140224     C                   ENDIF
024200020619     C                   ENDIF
024300000801     C*
024400000801     C                   ENDSR
024500000801     C*----------------------------------------------------*
024600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024700000801     C*----------------------------------------------------*
024800040823     C     IMPVAB        BEGSR
024900040823     C*
025000020305     C                   EXSR      INZVAR
025100020305     C                   EXSR      DEFCAM
025200010305     C*
025300000801     C                   Z-ADD     *zeros        errore            1 0
025400000830     C                   MOVEL     datcor        VABAAS
025500020305     C                   MOVEL     datcor        VATAAS
025600040526     C                   MOVE      datcor        VABMGS
025700040823     C                   MOVE(P)   vlrpoi        VABFGS
025800040823     C                   MOVE(P)   vlrpoi        VATFGS
025900050628     C*
026000140221     C                   EVAL      VABRSD=%trim(%subst(vindta:276:35))
026100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026300020117     C* ==
026400140224     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
026500140221     C                   EVAL      VABIND=%trim(%subst(vindta:341:35))
026600070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
026700070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
026800140224     C                   EVAL      VABRMA=%trim(%subst(vindta:13:15))
026900140304     C                   IF        %subst(vindta:514:30) <> *blank
027000140304     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
027100140304     C                   EVAL      VABNT2=%trim(%subst(vindta:663:50))
027200140304     C                   ELSE
027300140304     C                   EVAL      w70=*blank
027400140304     C                   EVAL      w70=%trim(%subst(vindta:663:50))
027500140304     C                   EVAL      VABNOT=%subst(w70:1:35)
027600140304     C                   EVAL      VABNT2=%subst(w70:36:35)
027700140304     C                   ENDIF
027800050628     C* CAP
027900140224     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
028000010201     C                   EXSR      CHKNUM
028100010201     C                   IF        PiInt=*on
028200140224     C                   Z-ADD     PiVal         Num5_0
028300040506     C                   MOVEL(P)  Num5_0        VABCAD
028400010201     C                   ELSE
028500040506     C                   ADD       1             errore
028600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
028700040506     C                             + ' ' + 'VABCAD'
028800010201     C                   ENDIF
028900040506     C* Reperisco la provincia dal CAP e dalla località
029000040526     C                   IF        VABCAD <> *blanks AND
029100040526     C                             VABPRD  = *blanks
029200040506     C                   CLEAR                   TISI95DS
029300040506     C                   EVAL      I95TCN = '3'
029400040506     C                   Z-ADD     datcor        I95DAT
029500040506     C                   EVAL      I95CAP = VABCAD
029600040506     C                   EVAL      I95LOC = VABLOD
029700050627     C                   EVAL      I95NAR = VABNZD
029800040506     C                   CALL      'TISI95R'
029900040506     C                   PARM                    TISI95DS
030000040506     C                   EVAL      VABPRD = O95PRV
030100040506     C                   ENDIF
030200040506     C* PKB
030300070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
030400010201     C                   EXSR      CHKNUM
030500010201     C                   IF        PiNum=*on
030600070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
030700060316     C                   Z-ADD     PiVal         VABPKB
030800010201     C                   ELSE
030900010201     C                   ADD       1             errore
031000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031100010201     C                             + ' ' + 'VABPKB'
031200010201     C                   ENDIF
031300140224     C* RMN e NSP
031400140226     C                   EVAL      PiStr=%trim(%subst(vindta:46+11:9))
031500090511     C                   EXSR      CHKNUM
031600090511     C                   IF        PiInt=*on
031700090511     C                   Z-ADD     PiVal         VABRMN
031800140224     C                   Z-ADD     PiVal         VABNSP
031900140224     C                   Z-ADD     PiVal         VATNSP
032000090511     C                   ELSE
032100090511     C                   ADD       1             errore
032200090511     C                   EVAL      vinmsg = %trimr(vinmsg)
032300140224     C                             + ' ' + 'VABRMN VABNSP VATNSP'
032400090511     C                   ENDIF
032500060307     C* CAS
032600140224     C                   IF        %trim(%subst(vindta:502:9))<>'000000,00' AND
032700090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
032800090608     C                             %trim(%subst(vindta:502:9))<>*zeros
032900070102     C                   EVAL      VABVCA = 'EUR'
033000060307     C                   EVAL      FlgCAS = '1'
033100070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
033200060307     C                   EXSR      CHKNUM
033300060307     C                   IF        PiNum=*on
033400060316     C                   Z-ADD     PiVal         VABCAS
033500060307     C                   ELSE
033600060307     C                   ADD       1             errore
033700060307     C                   EVAL      vinmsg = %trimr(vinmsg)
033800060307     C                             + ' ' + 'VABCAS'
033900060307     C                   ENDIF
034000070102     C                   ENDIF
034100010205     C*
034200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034300040802     C                   IF        FlgCAS <> '0'
034400070102     C                   IF        VABCBO = '1'
034500010205     C                   EVAL      VABCBO = '4'
034600010205     C                   ELSE
034700070102     C                   EVAL      VABCBO = '6'
034800070102     C                   ENDIF
034900140224     C                   EVAL      VABTIC = 'BM'
035000010205     C                   ENDIF
035100020305     C*
035200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035300011113     C                   EXSR      CHKIMPDIV
035400010202     C*
035500000801     C* Ebbene...
035600000801     C                   ADD       1             §CTRMO
035700010201     C                   IF        errore <> *zeros
035800000801     C                   ADD       1             §CTRNO
035900000801     C                   EVAL      vinflg = '2'
036000000801     C                   ELSE
036100010201     C                   ADD       1             §CTROKVB
036200000801     C                   ENDIF
036300000801     C*
036400000801     C                   ENDSR
036500070102     C*----------------------------------------------------*
036600070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
036700070102     C*----------------------------------------------------*
036800070102     C     EXEVATA       BEGSR
036900070102     C*
037000070102     C                   EXSR      INZVAR
037100070102     C                   EXSR      DEFCAM
037200070102     C*
037300070102     C                   EVAL      VATTRC='A'
037400140224     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
037500070102     C                   exsr      wrivat                                       => scarico VAT
037600070102     C*
037700070102     C                   ENDSR
037800060307     C*----------------------------------------------------*
037900060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
038000060307     C*----------------------------------------------------*
038100060307     C     EXEVATB       BEGSR
038200060307     C*
038300060307     C                   EXSR      INZVAR
038400060307     C                   EXSR      DEFCAM
038500060307     C*
038600060307     C                   EVAL      VATTRC='B'
038700140304     C                   EVAL      VATNOT = %trim(%subst(vindta:713:14))
038800060307     C                   exsr      wrivat                                       => scarico VAT
038900060307     C*
039000060307     C                   ENDSR
039100070402     C*----------------------------------------------------*
039200070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
039300070402     C*----------------------------------------------------*
039400070402     C     EXEVATE       BEGSR
039500070402     C*
039600070402     C                   EXSR      INZVAR
039700070402     C                   EXSR      DEFCAM
039800070402     C*
039900070402     C                   EVAL      VATTRC='E'
040000140224     C                   EVAL      VATNOT = %trim(%subst(vindta:46:10))
040100070402     C                   exsr      wrivat                                       => scarico VAT
040200070402     C*
040300070402     C                   ENDSR
040400140224     C*----------------------------------------------------*
040500140224     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
040600140224     C*----------------------------------------------------*
040700140224     C     EXEVATI       BEGSR
040800140224     C*
040900140224     C                   EXSR      INZVAR
041000140224     C                   EXSR      DEFCAM
041100140224     C*
041200140224     C                   EVAL      VATTRC='I'
041300140224     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
041400140224     C                   exsr      wrivat                                       => scarico VAT
041500140224     C*
041600140224     C                   EVAL      VATTRC='J'
041700140224     C                   EVAL      VATNOT = %trim(%subst(vindta:663+35:15))
041800140224     C                   exsr      wrivat                                       => scarico VAT
041900140224     C*
042000140224     C                   ENDSR
042100140226     C*----------------------------------------------------*
042200140226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
042300140226     C*----------------------------------------------------*
042400140226     C     EXEVATS       BEGSR
042500140226     C*
042600140226     C                   EXSR      INZVAR
042700140226     C                   EXSR      DEFCAM
042800140226     C*
042900140226     C                   EVAL      VATTRC='S'
043000140226     C                   EVAL      VATNOT=*blank
043100140226     C                   EVAL      %subst(VATNOT:1:14) =
043200140226     C                              %trim(%subst(vindta:713:14))
043300140226     C                   IF        VATNOT <> *blank
043400140226     C                   EVAL      %subst(VATNOT:17:1) = 'S'
043500140226     C                   ENDIF
043600140226     C                   exsr      wrivat                                       => scarico VAT
043700140226     C*
043800140226     C                   ENDSR
043900010201     C*----------------------------------------------------*
044000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044100010201     C*----------------------------------------------------*
044200020305     C     WRIVAT        BEGSR
044300050628     C*
044400060223     C* Scrivo solo se valorizzato qualcosa
044500060223     C                   IF        VATNOT <> *blanks
044600040802     C                   WRITE     FIVAT000
044700060223     C                   ENDIF
044800010201     C*
044900010201     C                   ENDSR
045000010202     C*----------------------------------------------------*
045100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045200010202     C*----------------------------------------------------*
045300020305     C     PREVAT        BEGSR
045400010202     C*
045500021113     C* Compongo il nome del membro da dare al FIVATWWR
045600010202     C                   eval      parmbr = vlrhdl
045700010202     C                   movel     'M'           parmbr
045800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
045900010202     C                   eval      paropz = '1'
046000010202     C* Effettuo la chiamata al CLLE preposto
046100040506     C                   call(e)   'TITVVTC'
046200010202     C                   parm                    parccm
046300010202     C                   parm                    parmbr
046400010202     C                   parm                    paropz
046500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046600010202     C                   if        %error
046700010202     C                   movel     '1'           chkcall
046800010202     C                   else
046900010202     C                   movel     '0'           chkcall
047000010202     C                   endif
047100010202     C*
047200010202     C                   ENDSR
047300000801     C*----------------------------------------------------*
047400000801     C*  CONTROLLO NUMERICITA' CAMPI
047500000801     C*----------------------------------------------------*
047600000801     C     CHKNUM        BEGSR
047700000801     C*
047800000801     C                   call(e)   'ISNUMERIC'
047900000801     C                   PARM                    PiStr            30
048000140226     C                   PARM      '.'           PiDecChr          1
048100000801     C                   PARM      *ZEROS        PiVal            30 9
048200000801     C                   PARM      '0'           PiInt             1
048300000801     C                   PARM      '0'           PiNum             1
048400000801     C                   IF        %error
048500000801     C                   EVAL      PiInt=*off
048600000801     C                   ENDIF
048700000801     C*
048800000801     C                   ENDSR
048900000801     C***
049000000801
049100011113
049200011113     C*----------------------------------------------------*
049300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049400011113     C*----------------------------------------------------*
049500011113     C     CHKIMPDIV     BEGSR
049600011113     C*
049700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049800011113     C                   Z-ADD     *zeros        wrkDec            9 9
049900011113     C*
050000011113     C* Come prima cosa effettuo considerazioni sulla divisa
050100011113     C                   IF        vabIAS > *zeros
050200011113     C                   IF        vabVAS <> 'EUR'
050300011113     C                   EVAL      vabVAS =  'ITL'
050400011113     C                   ENDIF
050500011113     C                   ENDIF
050600011113     C*
050700011113     C                   IF        vabCAS > *zeros
050800011113     C                   IF        vabVCA <> 'EUR'
050900011113     C                   EVAL      vabVCA =  'ITL'
051000011113     C                   ENDIF
051100011113     C                   ENDIF
051200011113     C*
051300011113     C                   IF        vabVMD > *zeros
051400020305     C                   IF        vabVAD <> 'EUR'
051500011113     C                   EVAL      vabVAD =  'ITL'
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052000011113     C                   Z-ADD     vabIAS        wrkDec
052100011113     C                   IF        wrkDec > *zeros
052200011113     C                   IF        vabVAS = 'ITL'
052300011113     C                   EVAL      vabIAS = *zeros
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052800011113     C                   Z-ADD     vabCAS        wrkDec
052900011113     C                   IF        wrkDec > *zeros
053000011113     C                   IF        vabVCA = 'ITL'
053100011113     C                   EVAL      vabCAS = *zeros
053200011113     C                   ENDIF
053300011113     C                   ENDIF
053400011113     C*
053500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053600011113     C                   Z-ADD     vabVMD        wrkDec
053700011113     C                   IF        wrkDec > *zeros
053800011113     C                   IF        vabVAD = 'ITL'
053900011113     C                   EVAL      vabVMD = *zeros
054000011113     C                   ENDIF
054100011113     C                   ENDIF
054200011113     C*
054300011113     C                   ENDSR
054400011113     C***
054500011113
054600011113
054700000801
054800000801
054900990920      /TITLE Invio dei dati al punto operativo.
055000010202     C     invio         BEGSR
055100990920     C*
055200021113     C* 1° invio FIVAT
055300010201     C                   reset                   dscmz
055400010201     C                   move      vlrpoi        cmzdst
055500021113     C                   eval      cmzfld = 'FIVATWWR'
055600010201     C                   eval      cmzmbd = vlrhdl
055700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055800021009     C***                if        prmfir = *blanks
055900021113     C                   eval      cmzfla = 'FIVAT00F'
056000021113     C                   eval      cmzmba = 'FIVAT00F'
056100021009     C***                else
056200021009     C***                eval      cmzfla = prmfir
056300021009     C***                eval      cmzmba = prmfir
056400021009     C***                endif
056500010201     C                   eval      cmznrr = *zeros
056600020305     C                   move      §ctrokvt      cmznrr
056700021018     C                   eval      cmzlba = vlrfl1
056800010201     C                   call(e)   'TIS711C'
056900010201     C                   parm                    dscmz
057000010201     C                   parm      *blanks       esito
057100010205     C                   if        %error
057200010205     C                             or cmzerr = '1'
057300010205     C                             or esito  = '1'
057400010205     C                   eval      wrkesito = '3'
057500010205     C                   else
057600010201     C*
057700021113     C* 2° invio FIVAB
057800010201     C                   reset                   dscmz
057900010201     C                   move      vlrpoi        cmzdst
058000010201     C                   eval      cmzfld = vlrfou
058100010201     C                   eval      cmzmbd = vlrhdl
058200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058300021009     C***                if        prmfir = *blanks
058400021113     C                   eval      cmzfla = 'FIVAB00F'
058500021113     C                   eval      cmzmba = 'FIVAB00F'
058600021009     C***                else
058700021009     C***                eval      cmzfla = prmfir
058800021009     C***                eval      cmzmba = prmfir
058900021009     C***                endif
059000010201     C                   eval      cmznrr = *zeros
059100010201     C                   move      §ctrokvb      cmznrr
059200021018     C                   eval      cmzlba = vlrfl1
059300010201     C                   call(e)   'TIS711C'
059400010201     C                   parm                    dscmz
059500010201     C                   parm      *blanks       esito
059600010201     C                   if        %error
059700010201     C                             or cmzerr = '1'
059800010201     C                             or esito  = '1'
059900010201     C                   eval      wrkesito = '3'
060000010201     C                   endif
060100010205     C                   endif
060200990920     C*
060300000613     C                   ENDSR
060400000613     C***
060500990910
060600000613     C     *inzsr        BEGSR
060700990910     C*
060800990910     C     *entry        plist
060900990920     C                   parm                    tivlrds
061000990921     C                   parm      wrkesito      esito
061100000724     C                   parm                    prmlit
061200000710     C                   parm                    prmfir
061300000613     C*
061400000830     C* CALCOLA LA DATA CORRENTE
061500100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
061600100118     C                   eval      datcor = %dec(%date() : *ISO)
061700000830     C*
061800000613     C                   ENDSR
061900000613     C***
