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
002800150225     D curSped         s              7    INZ(*blanks)
002900150225     D depSped         s              7    INZ(*blanks)
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
010700150225     C                   eval      curSped = %subst(vindta:52:7)
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
020600150225     C                   Z-ADD     0936016       VABCCM
020700150225     C                   Z-ADD     0936016       VATCCM
020800150225     C                   Z-ADD     093           VABLNP
020900150225     C                   Z-ADD     093           VATLNP
021000150225     C                   Z-ADD     300           VABCTR
021100150225     C                   MOVEL     '1'           VABCBO
021200150225     C                   MOVEL     'C'           VABTSP
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
026400150225     C                   EVAL      VABRD2=%trim(%subst(vindta:276+35:5))
026500140221     C                   EVAL      VABIND=%trim(%subst(vindta:341:35))
026600070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
026700070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
026800150225     C                   EVAL      VABRMA=%trim(%subst(vindta:46:13))
026900050628     C* CAP
027000140224     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
027100010201     C                   EXSR      CHKNUM
027200010201     C                   IF        PiInt=*on
027300140224     C                   Z-ADD     PiVal         Num5_0
027400040506     C                   MOVEL(P)  Num5_0        VABCAD
027500010201     C                   ELSE
027600040506     C                   ADD       1             errore
027700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027800040506     C                             + ' ' + 'VABCAD'
027900010201     C                   ENDIF
028000040506     C* Reperisco la provincia dal CAP e dalla località
028100040526     C                   IF        VABCAD <> *blanks AND
028200040526     C                             VABPRD  = *blanks
028300040506     C                   CLEAR                   TISI95DS
028400040506     C                   EVAL      I95TCN = '3'
028500040506     C                   Z-ADD     datcor        I95DAT
028600040506     C                   EVAL      I95CAP = VABCAD
028700040506     C                   EVAL      I95LOC = VABLOD
028800050627     C                   EVAL      I95NAR = VABNZD
028900040506     C                   CALL      'TISI95R'
029000040506     C                   PARM                    TISI95DS
029100040506     C                   EVAL      VABPRD = O95PRV
029200040506     C                   ENDIF
029300040506     C* PKB
029400150225     C                   EVAL      PiStr=%trim(%subst(vindta:435:5))
029500010201     C                   EXSR      CHKNUM
029600010201     C                   IF        PiNum=*on
029700150225     C                   EVAL      PiVal=PiVal/10                               kg con 1 decimale
029800060316     C                   Z-ADD     PiVal         VABPKB
029900010201     C                   ELSE
030000010201     C                   ADD       1             errore
030100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030200010201     C                             + ' ' + 'VABPKB'
030300010201     C                   ENDIF
030400150225     C* RMN
030500150225     C                   EVAL      PiStr=%trim(%subst(vindta:46:13))
030600090511     C                   EXSR      CHKNUM
030700090511     C                   IF        PiInt=*on
030800090511     C                   Z-ADD     PiVal         VABRMN
030900090511     C                   ELSE
031000090511     C                   ADD       1             errore
031100090511     C                   EVAL      vinmsg = %trimr(vinmsg)
031200150225     C                             + ' ' + 'VABRMN'
031300090511     C                   ENDIF
031400150225     C* NSP
031500150225     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
031600150225     C                   EXSR      CHKNUM
031700150225     C                   IF        PiInt=*on
031800150225     C                   Z-ADD     PiVal         VABNSP
031900150225     C                   Z-ADD     PiVal         VATNSP
032000150225     C                   ELSE
032100150225     C                   ADD       1             errore
032200150225     C                   EVAL      vinmsg = %trimr(vinmsg)
032300150225     C                             + ' ' + 'VABNSP VATNSP'
032400150225     C                   ENDIF
032500150225     C* NCL
032600150225     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
032700150225     C                   EXSR      CHKNUM
032800150225     C                   IF        PiInt=*on
032900150225     C                   Z-ADD     PiVal         VABNCL
033000150225     C                   ELSE
033100150225     C                   ADD       1             errore
033200150225     C                   EVAL      vinmsg = %trimr(vinmsg)
033300150225     C                             + ' ' + 'VABNCL'
033400150225     C                   ENDIF
033500060307     C* CAS
033600150225     C***                IF        %trim(%subst(vindta:502:9))<>'000000,00' AND
033700150225     C***                          %trim(%subst(vindta:502:9))<>*blanks     AND
033800150225     C***                          %trim(%subst(vindta:502:9))<>*zeros
033900150225     C***                EVAL      VABVCA = 'EUR'
034000150225     C***                EVAL      FlgCAS = '1'
034100150225     C***                EVAL      PiStr=%trim(%subst(vindta:502:9))
034200150225     C***                EXSR      CHKNUM
034300150225     C***                IF        PiNum=*on
034400150225     C***                Z-ADD     PiVal         VABCAS
034500150225     C***                ELSE
034600150225     C***                ADD       1             errore
034700150225     C***                EVAL      vinmsg = %trimr(vinmsg)
034800150225     C***                          + ' ' + 'VABCAS'
034900150225     C***                ENDIF
035000150225     C***                ENDIF
035100010205     C*
035200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
035300040802     C                   IF        FlgCAS <> '0'
035400070102     C                   IF        VABCBO = '1'
035500010205     C                   EVAL      VABCBO = '4'
035600010205     C                   ELSE
035700070102     C                   EVAL      VABCBO = '6'
035800070102     C                   ENDIF
035900010205     C                   ENDIF
036000020305     C*
036100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036200011113     C                   EXSR      CHKIMPDIV
036300010202     C*
036400000801     C* Ebbene...
036500000801     C                   ADD       1             §CTRMO
036600010201     C                   IF        errore <> *zeros
036700000801     C                   ADD       1             §CTRNO
036800000801     C                   EVAL      vinflg = '2'
036900000801     C                   ELSE
037000010201     C                   ADD       1             §CTROKVB
037100000801     C                   ENDIF
037200000801     C*
037300000801     C                   ENDSR
037400070102     C*----------------------------------------------------*
037500070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
037600070102     C*----------------------------------------------------*
037700070102     C     EXEVATA       BEGSR
037800070102     C*
037900070102     C                   EXSR      INZVAR
038000070102     C                   EXSR      DEFCAM
038100070102     C*
038200070102     C                   EVAL      VATTRC='A'
038300150225     C                   EVAL      VATNOT = %trim(%subst(vindta:316:25))
038400070102     C                   exsr      wrivat                                       => scarico VAT
038500070102     C*
038600070102     C                   ENDSR
038700060307     C*----------------------------------------------------*
038800060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
038900060307     C*----------------------------------------------------*
039000060307     C     EXEVATB       BEGSR
039100060307     C*
039200060307     C                   EXSR      INZVAR
039300060307     C                   EXSR      DEFCAM
039400060307     C*
039500060307     C                   EVAL      VATTRC='B'
039600150225     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
039700060307     C                   exsr      wrivat                                       => scarico VAT
039800060307     C*
039900060307     C                   ENDSR
040000070402     C*----------------------------------------------------*
040100070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
040200070402     C*----------------------------------------------------*
040300070402     C     EXEVATE       BEGSR
040400070402     C*
040500070402     C                   EXSR      INZVAR
040600070402     C                   EXSR      DEFCAM
040700070402     C*
040800070402     C                   EVAL      VATTRC='E'
040900150225     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
041000070402     C                   exsr      wrivat                                       => scarico VAT
041100070402     C*
041200070402     C                   ENDSR
041300140224     C*----------------------------------------------------*
041400140224     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
041500140224     C*----------------------------------------------------*
041600140224     C     EXEVATI       BEGSR
041700150225     C***
041800150225     C***                EXSR      INZVAR
041900150225     C***                EXSR      DEFCAM
042000150225     C***
042100150225     C***                EVAL      VATTRC='I'
042200150225     C***                EVAL      VATNOT = %trim(%subst(vindta:663:35))
042300150225     C***                exsr      wrivat                                       => scarico VAT
042400150225     C***
042500150225     C***                EVAL      VATTRC='J'
042600150225     C***                EVAL      VATNOT = %trim(%subst(vindta:663+35:15))
042700150225     C***                exsr      wrivat                                       => scarico VAT
042800150225     C***
042900140224     C                   ENDSR
043000140226     C*----------------------------------------------------*
043100140226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
043200140226     C*----------------------------------------------------*
043300140226     C     EXEVATS       BEGSR
043400150225     C***
043500150225     C***                EXSR      INZVAR
043600150225     C***                EXSR      DEFCAM
043700150225     C***
043800150225     C***                EVAL      VATTRC='S'
043900150225     C***                EVAL      VATNOT=*blank
044000150225     C***                EVAL      %subst(VATNOT:1:14) =
044100150225     C***                           %trim(%subst(vindta:713:14))
044200150225     C***                IF        VATNOT <> *blank
044300150225     C***                EVAL      %subst(VATNOT:17:1) = 'S'
044400150225     C***                ENDIF
044500150225     C***                exsr      wrivat                                       => scarico VAT
044600150225     C***
044700140226     C                   ENDSR
044800010201     C*----------------------------------------------------*
044900040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
045000010201     C*----------------------------------------------------*
045100020305     C     WRIVAT        BEGSR
045200050628     C*
045300060223     C* Scrivo solo se valorizzato qualcosa
045400060223     C                   IF        VATNOT <> *blanks
045500040802     C                   WRITE     FIVAT000
045600060223     C                   ENDIF
045700010201     C*
045800010201     C                   ENDSR
045900010202     C*----------------------------------------------------*
046000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046100010202     C*----------------------------------------------------*
046200020305     C     PREVAT        BEGSR
046300010202     C*
046400021113     C* Compongo il nome del membro da dare al FIVATWWR
046500010202     C                   eval      parmbr = vlrhdl
046600010202     C                   movel     'M'           parmbr
046700050627     C                   eval      parccm = %subst(vlrKSC:2:7)
046800010202     C                   eval      paropz = '1'
046900010202     C* Effettuo la chiamata al CLLE preposto
047000040506     C                   call(e)   'TITVVTC'
047100010202     C                   parm                    parccm
047200010202     C                   parm                    parmbr
047300010202     C                   parm                    paropz
047400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047500010202     C                   if        %error
047600010202     C                   movel     '1'           chkcall
047700010202     C                   else
047800010202     C                   movel     '0'           chkcall
047900010202     C                   endif
048000010202     C*
048100010202     C                   ENDSR
048200000801     C*----------------------------------------------------*
048300000801     C*  CONTROLLO NUMERICITA' CAMPI
048400000801     C*----------------------------------------------------*
048500000801     C     CHKNUM        BEGSR
048600000801     C*
048700000801     C                   call(e)   'ISNUMERIC'
048800000801     C                   PARM                    PiStr            30
048900140226     C                   PARM      '.'           PiDecChr          1
049000000801     C                   PARM      *ZEROS        PiVal            30 9
049100000801     C                   PARM      '0'           PiInt             1
049200000801     C                   PARM      '0'           PiNum             1
049300000801     C                   IF        %error
049400000801     C                   EVAL      PiInt=*off
049500000801     C                   ENDIF
049600000801     C*
049700000801     C                   ENDSR
049800000801     C***
049900000801
050000011113
050100011113     C*----------------------------------------------------*
050200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050300011113     C*----------------------------------------------------*
050400011113     C     CHKIMPDIV     BEGSR
050500011113     C*
050600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050700011113     C                   Z-ADD     *zeros        wrkDec            9 9
050800011113     C*
050900011113     C* Come prima cosa effettuo considerazioni sulla divisa
051000011113     C                   IF        vabIAS > *zeros
051100011113     C                   IF        vabVAS <> 'EUR'
051200011113     C                   EVAL      vabVAS =  'ITL'
051300011113     C                   ENDIF
051400011113     C                   ENDIF
051500011113     C*
051600011113     C                   IF        vabCAS > *zeros
051700011113     C                   IF        vabVCA <> 'EUR'
051800011113     C                   EVAL      vabVCA =  'ITL'
051900011113     C                   ENDIF
052000011113     C                   ENDIF
052100011113     C*
052200011113     C                   IF        vabVMD > *zeros
052300020305     C                   IF        vabVAD <> 'EUR'
052400011113     C                   EVAL      vabVAD =  'ITL'
052500011113     C                   ENDIF
052600011113     C                   ENDIF
052700011113     C*
052800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052900011113     C                   Z-ADD     vabIAS        wrkDec
053000011113     C                   IF        wrkDec > *zeros
053100011113     C                   IF        vabVAS = 'ITL'
053200011113     C                   EVAL      vabIAS = *zeros
053300011113     C                   ENDIF
053400011113     C                   ENDIF
053500011113     C*
053600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
053700011113     C                   Z-ADD     vabCAS        wrkDec
053800011113     C                   IF        wrkDec > *zeros
053900011113     C                   IF        vabVCA = 'ITL'
054000011113     C                   EVAL      vabCAS = *zeros
054100011113     C                   ENDIF
054200011113     C                   ENDIF
054300011113     C*
054400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054500011113     C                   Z-ADD     vabVMD        wrkDec
054600011113     C                   IF        wrkDec > *zeros
054700011113     C                   IF        vabVAD = 'ITL'
054800011113     C                   EVAL      vabVMD = *zeros
054900011113     C                   ENDIF
055000011113     C                   ENDIF
055100011113     C*
055200011113     C                   ENDSR
055300011113     C***
055400011113
055500011113
055600000801
055700000801
055800990920      /TITLE Invio dei dati al punto operativo.
055900010202     C     invio         BEGSR
056000990920     C*
056100021113     C* 1° invio FIVAT
056200010201     C                   reset                   dscmz
056300010201     C                   move      vlrpoi        cmzdst
056400021113     C                   eval      cmzfld = 'FIVATWWR'
056500010201     C                   eval      cmzmbd = vlrhdl
056600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056700021009     C***                if        prmfir = *blanks
056800021113     C                   eval      cmzfla = 'FIVAT00F'
056900021113     C                   eval      cmzmba = 'FIVAT00F'
057000021009     C***                else
057100021009     C***                eval      cmzfla = prmfir
057200021009     C***                eval      cmzmba = prmfir
057300021009     C***                endif
057400010201     C                   eval      cmznrr = *zeros
057500020305     C                   move      §ctrokvt      cmznrr
057600021018     C                   eval      cmzlba = vlrfl1
057700010201     C                   call(e)   'TIS711C'
057800010201     C                   parm                    dscmz
057900010201     C                   parm      *blanks       esito
058000010205     C                   if        %error
058100010205     C                             or cmzerr = '1'
058200010205     C                             or esito  = '1'
058300010205     C                   eval      wrkesito = '3'
058400010205     C                   else
058500010201     C*
058600021113     C* 2° invio FIVAB
058700010201     C                   reset                   dscmz
058800010201     C                   move      vlrpoi        cmzdst
058900010201     C                   eval      cmzfld = vlrfou
059000010201     C                   eval      cmzmbd = vlrhdl
059100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059200021009     C***                if        prmfir = *blanks
059300021113     C                   eval      cmzfla = 'FIVAB00F'
059400021113     C                   eval      cmzmba = 'FIVAB00F'
059500021009     C***                else
059600021009     C***                eval      cmzfla = prmfir
059700021009     C***                eval      cmzmba = prmfir
059800021009     C***                endif
059900010201     C                   eval      cmznrr = *zeros
060000010201     C                   move      §ctrokvb      cmznrr
060100021018     C                   eval      cmzlba = vlrfl1
060200010201     C                   call(e)   'TIS711C'
060300010201     C                   parm                    dscmz
060400010201     C                   parm      *blanks       esito
060500010201     C                   if        %error
060600010201     C                             or cmzerr = '1'
060700010201     C                             or esito  = '1'
060800010201     C                   eval      wrkesito = '3'
060900010201     C                   endif
061000010205     C                   endif
061100990920     C*
061200000613     C                   ENDSR
061300000613     C***
061400990910
061500000613     C     *inzsr        BEGSR
061600990910     C*
061700990910     C     *entry        plist
061800990920     C                   parm                    tivlrds
061900990921     C                   parm      wrkesito      esito
062000000724     C                   parm                    prmlit
062100000710     C                   parm                    prmfir
062200000613     C*
062300000830     C* CALCOLA LA DATA CORRENTE
062400100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
062500100118     C                   eval      datcor = %dec(%date() : *ISO)
062600000830     C*
062700000613     C                   ENDSR
062800000613     C***
