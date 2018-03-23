000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200140221      *
000300140221      * PARTICOLARITA':
000400150413      * disk A
000500140221      *
000600140221     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000700990908
000800990910     Ftivin00r  uF   E             DISK    usropn
000900021113     FFIVABwwr  O    E             DISK    usropn
001000021113     FFIVATwwr  O    E             DISK    usropn
001100990908
001200000801     D*----------------------------------------------------
001300000801     D* DICHIARAZIOINE VARIABILI DI WRK
001400000801     D*----------------------------------------------------
001500990920     D dscmz         e ds                  inz
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900060307     D tisi95ds      e ds
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400000613     D rrnum           s              6  0 INZ(*zeros)
002500010202     D parccm          s              8    INZ(*blanks)
002600010202     D parmbr          s             10    INZ(*blanks)
002700010202     D paropz          s              1    INZ(*blanks)
002800010202     D chkcall         s              1    INZ(*blanks)
002900150225     D curSped         s              7    INZ(*blanks)
003000150225     D depSped         s              7    INZ(*blanks)
003100140221     D w70             s             70    INZ(*blanks)
003200100118
003300100118     D*------------------
003400100118     D* DS REPERIMENTO NUMERATORE
003500100118     D*------------------
003600100118     D trul33ds      e ds                  inz
003700100118     D*------------------
003800100118     D* DS ARCHITETTURA
003900100118     D*------------------
004000100118     D kpjba         e ds                  inz
004100100118
004200010201
004300010201
004400090127     C*
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800000613     C*
004900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005000000613     C*
005100010202     C* Effettuo la chiamata al CLLE preposto
005200040506     C                   call(e)   'TITVVTC'
005300010202     C                   parm                    parccm
005400010202     C                   parm                    parmbr
005500010202     C                   parm      '2'           paropz
005600050201     C*
005700050201     C* Effettuo lancio TISI95 solo x chiusura
005800050201     C                   CLEAR                   TISI95DS
005900050201     C                   EVAL      I95TLA = 'C'
006000050201     C                   CALL      'TISI95R'
006100050201     C                   PARM                    TISI95DS
006200000616     C*
006300000801     C
006400010201     C                   seton                                        LR
006500990908
006600000801
006700910830     C*--------------------------------------------------------
006800040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006900910830     C*--------------------------------------------------------
007000040526     C     RWFILE        BEGSR
007100990910     C*
007200990914     C                   if        not %open(tivin00r)
007300990908     C                   open      tivin00r
007400990914     C                   endif
007500021113     C                   if        not %open(fivabwwr)
007600021113     C                   open      fivabwwr
007700990914     C                   endif
007800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007900020305     C                   exsr      prevat
008000010201     C*
008100010202     C                   if        chkcall = '0'
008200010202     C*
008300021113     C                   if        not %open(fivatwwr)
008400021113     C                   open      fivatwwr
008500010201     C                   endif
008600990910     C*
008700010201     C                   clear                   §CTROKVB          5 0
008800020305     C                   clear                   §CTROKVT          5 0
008900000801     C                   clear                   §CTRMO            5 0
009000000801     C                   clear                   §CTRNO            5 0
009100990910     C*
009200921023     C                   DO        *HIVAL
009300990913     C*
009400990915     C                   READ      tivin00r                               70
009500050627     C                   if        vindta > *blanks
009600000613     C                   add       1             rrnum
009700000801     C*
009800000801     C                   if        *in70 = *off
009900000801     C                             and
010000000801     C                             (vinflg = *blanks
010100000801     C                              or vinflg = '0'
010200000801     C                              or vinflg = '2')
010300000801     C*
010400000801     C                   clear                   vinmsg
010500000801     C                   eval      vinflg = '1'
010600070402     C*
010700070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010800160707     C                   eval      curSped = %subst(vindta:140:7)
010900070402     C                   if        curSped <> depSped
011000070402     C                   clear                   fivab000
011100070402     C                   clear                   fivat000
011200050628     C*
011300060315     C                   exsr      impvab                                       => carico  VAB
011400090127     C*
011500060315     C                   exsr      wrivab                                       => scarico VAB
011600070402     C*
011700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011800150413     C***                exsr      exevata
011900070402     C*
012000070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
012100150413     C***                exsr      exevatb
012200140226     C*
012300140226     C* Indirizzo e-mail
012400150413     C***                exsr      exevati
012500140226     C*
012600140226     C* Cellulare per SMS
012700150413     C***                exsr      exevats
012800070402     C*
012900070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013000150413     C***                exsr      exevate
013100070402     C*
013200070402     C* Salvo il raggruppamento spedizione cliente corrente
013300070402     C                   eval      depSped = curSped
013400070402     C*
013500070402     C                   else
013600070402     C*
013700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013800150413     C***                exsr      exevate
013900070402     C                   endif
014000000905     C*
014100000905     C                   else
014200000905     C                   eval      vinflg = '1'
014300050628     C                   endif
014400000905     C                   endif
014500000905     C*
014600000905     C  N70              update    tivin000
014700000905     C*
014800991022     C  N70              ENDdo
014900010202     C*
015000010202     C                   endif
015100990910
015200990910     C* Se non ci sono record con errori ...
015300000710     C                   if        §ctrno = 0
015400990910     C* ... restituisco esito OK.
015500990921     C                   eval      wrkesito = '0'
015600990910     C                   else
015700010201     C                   if        §ctrokvb > 0
015800990921     C                   eval      wrkesito = '1'
015900000710     C                   else
016000000710     C                   eval      wrkesito = '2'
016100990910     C                   endif
016200000710     C                   endif
016300990910     C*
016400990914     C                   if        %open(tivin00r)
016500990908     C                   close     tivin00r
016600990914     C                   endif
016700021113     C                   if        %open(fivabwwr)
016800021113     C                   close     fivabwwr
016900990914     C                   endif
017000021113     C                   if        %open(fivatwwr)
017100021113     C                   close     fivatwwr
017200010201     C                   endif
017300990910     C*
017400010201     C                   if        §ctrokvb > 0
017500000724     C                             and vlrpoi <> *zeros
017600010202     C                   exsr      invio
017700990920     C                   endif
017800990920     C*
017900910830     C                   ENDSR
018000000613     C***
018100010305
018200010305     C*----------------------------------------------------*
018300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018400010305     C*----------------------------------------------------*
018500090127     C     WRIVAB        BEGSR
018600090127     C*
018700090127     C* Quindi scarico il buffer del file d testata
018800090127     C                   write     fivab000                                     => scarico il VAB
018900010305     C*
019000010305     C                   ENDSR
019100990920
019200000801     C*----------------------------------------------------*
019300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019400000801     C*----------------------------------------------------*
019500010201     C     INZVAR        BEGSR
019600000801     C*
019700040802     C                   Z-ADD     *zeros        Num5_0            5 0
019800040802     C                   MOVEL     '0'           FlgCAS            1
019900000801     C*
020000000801     C                   ENDSR
020100000801     C*----------------------------------------------------*
020200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020300000801     C*----------------------------------------------------*
020400000801     C     DEFCAM        BEGSR
020500070718     C*
020600070718     C* Imposto i valori di default...
020700150413     C                   Z-ADD     0016181       VABCCM
020800150413     C                   Z-ADD     0016181       VATCCM
020900150413     C                   Z-ADD     001           VABLNP
021000150413     C                   Z-ADD     001           VATLNP
021100150413     C                   Z-ADD     000           VABCTR
021200150225     C                   MOVEL     '1'           VABCBO
021300020619     C* ... e poi verifico se sono stati passati come parametri
021400020619     C                   IF        vlrppt > *blanks
021500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021700020619     C                   EXSR      CHKNUM
021800020619     C                   IF        PiInt=*on
021900020619     C                   Z-ADD     PiVal         VABCCM
022000020619     C                   Z-ADD     PiVal         VATCCM
022100020619     C                   ENDIF
022200040506     C                   ENDIF
022300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022500020619     C                   EXSR      CHKNUM
022600020619     C                   IF        PiInt=*on
022700020619     C                   Z-ADD     PiVal         VABLNP
022800020619     C                   Z-ADD     PiVal         VATLNP
022900040506     C                   ENDIF
023000020619     C                   ENDIF
023100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023300020619     C                   EXSR      CHKNUM
023400020619     C                   IF        PiInt=*on
023500020619     C                   Z-ADD     PiVal         VABCTR
023600040506     C                   ENDIF
023700020619     C                   ENDIF
023800140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
023900140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
024000140224     C                   ENDIF
024100020619     C                   ENDIF
024200000801     C*
024300000801     C                   ENDSR
024400000801     C*----------------------------------------------------*
024500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024600000801     C*----------------------------------------------------*
024700040823     C     IMPVAB        BEGSR
024800040823     C*
024900020305     C                   EXSR      INZVAR
025000020305     C                   EXSR      DEFCAM
025100010305     C*
025200000801     C                   Z-ADD     *zeros        errore            1 0
025300000830     C                   MOVEL     datcor        VABAAS
025400020305     C                   MOVEL     datcor        VATAAS
025500040526     C                   MOVE      datcor        VABMGS
025600040823     C                   MOVE(P)   vlrpoi        VABFGS
025700040823     C                   MOVE(P)   vlrpoi        VATFGS
025800050628     C*
025900150413     C                   EVAL      VABRSD=%trim(%subst(vindta:3:30))
026000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026200020117     C* ==
026300160707     C                   EVAL      VABIND=%trim(%subst(vindta:33:35))
026400160707     C                   EVAL      VABLOD=%trim(%subst(vindta:68:30))
026500160707     C                   EVAL      VABPRD=%trim(%subst(vindta:103:2))
026600160707     C                   EVAL      VABRMA=%trim(%subst(vindta:233:10))
026700050628     C* CAP
026800160707     C                   EVAL      PiStr=%trim(%subst(vindta:98:5))
026900010201     C                   EXSR      CHKNUM
027000010201     C                   IF        PiInt=*on
027100140224     C                   Z-ADD     PiVal         Num5_0
027200040506     C                   MOVEL(P)  Num5_0        VABCAD
027300010201     C                   ELSE
027400040506     C                   ADD       1             errore
027500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027600040506     C                             + ' ' + 'VABCAD'
027700010201     C                   ENDIF
027800040506     C* Reperisco la provincia dal CAP e dalla località
027900040526     C                   IF        VABCAD <> *blanks AND
028000040526     C                             VABPRD  = *blanks
028100040506     C                   CLEAR                   TISI95DS
028200040506     C                   EVAL      I95TCN = '3'
028300040506     C                   Z-ADD     datcor        I95DAT
028400040506     C                   EVAL      I95CAP = VABCAD
028500040506     C                   EVAL      I95LOC = VABLOD
028600050627     C                   EVAL      I95NAR = VABNZD
028700040506     C                   CALL      'TISI95R'
028800040506     C                   PARM                    TISI95DS
028900040506     C                   EVAL      VABPRD = O95PRV
029000040506     C                   ENDIF
029100150413     C* NCL
029200160707     C                   EVAL      PiStr=%trim(%subst(vindta:105:4))
029300150413     C                   EXSR      CHKNUM
029400150413     C                   IF        PiInt=*on
029500150413     C                   Z-ADD     PiVal         VABNCL
029600150413     C                   ELSE
029700150413     C                   ADD       1             errore
029800150413     C                   EVAL      vinmsg = %trimr(vinmsg)
029900150413     C                             + ' ' + 'VABNCL'
030000150413     C                   ENDIF
030100040506     C* PKB
030200160707     C                   EVAL      PiStr=%trim(%subst(vindta:109:15))
030300010201     C                   EXSR      CHKNUM
030400010201     C                   IF        PiNum=*on
030500150225     C                   EVAL      PiVal=PiVal/10                               kg con 1 decimale
030600060316     C                   Z-ADD     PiVal         VABPKB
030700010201     C                   ELSE
030800010201     C                   ADD       1             errore
030900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031000010201     C                             + ' ' + 'VABPKB'
031100010201     C                   ENDIF
031200150413     C* RMN / NSP
031300160707     C                   EVAL      PiStr=%trim(%subst(vindta:140:7))
031400090511     C                   EXSR      CHKNUM
031500090511     C                   IF        PiInt=*on
031600090511     C                   Z-ADD     PiVal         VABRMN
031700150413     C                   Z-ADD     PiVal         VABNSP
031800150413     C                   Z-ADD     PiVal         VATNSP
031900090511     C                   ELSE
032000090511     C                   ADD       1             errore
032100090511     C                   EVAL      vinmsg = %trimr(vinmsg)
032200150413     C                             + ' ' + 'VABRMN VABNSP VATNSP'
032300090511     C                   ENDIF
032400060307     C* CAS
032500160707     C                   IF        %trim(%subst(vindta:206:9))<>*blanks     AND
032600160707     C                             %trim(%subst(vindta:206:9))<>*zeros
032700150413     C                   EVAL      VABVCA = 'EUR'
032800150413     C                   EVAL      FlgCAS = '1'
032900160707     C                   EVAL      PiStr=%trim(%subst(vindta:206:9))
033000150413     C                   EXSR      CHKNUM
033100150413     C                   IF        PiNum=*on
033200150420     C                   EVAL      VABCAS = PiVal/100
033300150413     C                   ELSE
033400150413     C                   ADD       1             errore
033500150413     C                   EVAL      vinmsg = %trimr(vinmsg)
033600150413     C                             + ' ' + 'VABCAS'
033700150413     C                   ENDIF
033800150413     C                   ENDIF
033900010205     C*
034000010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034100040802     C                   IF        FlgCAS <> '0'
034200150413     C                   MOVEL     'TM'          VABTIC
034300070102     C                   IF        VABCBO = '1'
034400010205     C                   EVAL      VABCBO = '4'
034500010205     C                   ELSE
034600070102     C                   EVAL      VABCBO = '6'
034700070102     C                   ENDIF
034800010205     C                   ENDIF
034900020305     C*
035000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035100011113     C                   EXSR      CHKIMPDIV
035200010202     C*
035300000801     C* Ebbene...
035400000801     C                   ADD       1             §CTRMO
035500010201     C                   IF        errore <> *zeros
035600000801     C                   ADD       1             §CTRNO
035700000801     C                   EVAL      vinflg = '2'
035800000801     C                   ELSE
035900010201     C                   ADD       1             §CTROKVB
036000000801     C                   ENDIF
036100000801     C*
036200000801     C                   ENDSR
036300070102     C*----------------------------------------------------*
036400070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
036500070102     C*----------------------------------------------------*
036600070102     C     EXEVATA       BEGSR
036700070102     C*
036800070102     C                   EXSR      INZVAR
036900070102     C                   EXSR      DEFCAM
037000070102     C*
037100070102     C                   EVAL      VATTRC='A'
037200160707     C                   EVAL      VATNOT = %trim(%subst(vindta:326:25))
037300070102     C                   exsr      wrivat                                       => scarico VAT
037400070102     C*
037500070102     C                   ENDSR
037600060307     C*----------------------------------------------------*
037700060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
037800060307     C*----------------------------------------------------*
037900060307     C     EXEVATB       BEGSR
038000060307     C*
038100060307     C                   EXSR      INZVAR
038200060307     C                   EXSR      DEFCAM
038300060307     C*
038400060307     C                   EVAL      VATTRC='B'
038500160707     C                   EVAL      VATNOT = %trim(%subst(vindta:386:15))
038600060307     C                   exsr      wrivat                                       => scarico VAT
038700060307     C*
038800060307     C                   ENDSR
038900070402     C*----------------------------------------------------*
039000070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
039100070402     C*----------------------------------------------------*
039200070402     C     EXEVATE       BEGSR
039300070402     C*
039400070402     C                   EXSR      INZVAR
039500070402     C                   EXSR      DEFCAM
039600070402     C*
039700070402     C                   EVAL      VATTRC='E'
039800160707     C                   EVAL      VATNOT = %trim(%subst(vindta:564:25))
039900070402     C                   exsr      wrivat                                       => scarico VAT
040000070402     C*
040100070402     C                   ENDSR
040200140224     C*----------------------------------------------------*
040300140224     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
040400140224     C*----------------------------------------------------*
040500140224     C     EXEVATI       BEGSR
040600150225     C***
040700150225     C***                EXSR      INZVAR
040800150225     C***                EXSR      DEFCAM
040900150225     C***
041000150225     C***                EVAL      VATTRC='I'
041100160707     C***                EVAL      VATNOT = %trim(%subst(vindta:673:35))
041200150225     C***                exsr      wrivat                                       => scarico VAT
041300150225     C***
041400150225     C***                EVAL      VATTRC='J'
041500160707     C***                EVAL      VATNOT = %trim(%subst(vindta:673+35:15))
041600150225     C***                exsr      wrivat                                       => scarico VAT
041700150225     C***
041800140224     C                   ENDSR
041900140226     C*----------------------------------------------------*
042000140226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
042100140226     C*----------------------------------------------------*
042200140226     C     EXEVATS       BEGSR
042300150225     C***
042400150225     C***                EXSR      INZVAR
042500150225     C***                EXSR      DEFCAM
042600150225     C***
042700150225     C***                EVAL      VATTRC='S'
042800150225     C***                EVAL      VATNOT=*blank
042900150225     C***                EVAL      %subst(VATNOT:1:14) =
043000160707     C***                           %trim(%subst(vindta:723:14))
043100150225     C***                IF        VATNOT <> *blank
043200150225     C***                EVAL      %subst(VATNOT:17:1) = 'S'
043300150225     C***                ENDIF
043400150225     C***                exsr      wrivat                                       => scarico VAT
043500150225     C***
043600140226     C                   ENDSR
043700010201     C*----------------------------------------------------*
043800040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043900010201     C*----------------------------------------------------*
044000020305     C     WRIVAT        BEGSR
044100050628     C*
044200060223     C* Scrivo solo se valorizzato qualcosa
044300060223     C                   IF        VATNOT <> *blanks
044400040802     C                   WRITE     FIVAT000
044500060223     C                   ENDIF
044600010201     C*
044700010201     C                   ENDSR
044800010202     C*----------------------------------------------------*
044900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045000010202     C*----------------------------------------------------*
045100020305     C     PREVAT        BEGSR
045200010202     C*
045300021113     C* Compongo il nome del membro da dare al FIVATWWR
045400010202     C                   eval      parmbr = vlrhdl
045500010202     C                   movel     'M'           parmbr
045600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
045700010202     C                   eval      paropz = '1'
045800010202     C* Effettuo la chiamata al CLLE preposto
045900040506     C                   call(e)   'TITVVTC'
046000010202     C                   parm                    parccm
046100010202     C                   parm                    parmbr
046200010202     C                   parm                    paropz
046300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046400010202     C                   if        %error
046500010202     C                   movel     '1'           chkcall
046600010202     C                   else
046700010202     C                   movel     '0'           chkcall
046800010202     C                   endif
046900010202     C*
047000010202     C                   ENDSR
047100000801     C*----------------------------------------------------*
047200000801     C*  CONTROLLO NUMERICITA' CAMPI
047300000801     C*----------------------------------------------------*
047400000801     C     CHKNUM        BEGSR
047500000801     C*
047600000801     C                   call(e)   'ISNUMERIC'
047700000801     C                   PARM                    PiStr            30
047800140226     C                   PARM      '.'           PiDecChr          1
047900000801     C                   PARM      *ZEROS        PiVal            30 9
048000000801     C                   PARM      '0'           PiInt             1
048100000801     C                   PARM      '0'           PiNum             1
048200000801     C                   IF        %error
048300000801     C                   EVAL      PiInt=*off
048400000801     C                   ENDIF
048500000801     C*
048600000801     C                   ENDSR
048700000801     C***
048800000801
048900011113
049000011113     C*----------------------------------------------------*
049100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049200011113     C*----------------------------------------------------*
049300011113     C     CHKIMPDIV     BEGSR
049400011113     C*
049500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049600011113     C                   Z-ADD     *zeros        wrkDec            9 9
049700011113     C*
049800011113     C* Come prima cosa effettuo considerazioni sulla divisa
049900011113     C                   IF        vabIAS > *zeros
050000011113     C                   IF        vabVAS <> 'EUR'
050100011113     C                   EVAL      vabVAS =  'ITL'
050200011113     C                   ENDIF
050300011113     C                   ENDIF
050400011113     C*
050500011113     C                   IF        vabCAS > *zeros
050600011113     C                   IF        vabVCA <> 'EUR'
050700011113     C                   EVAL      vabVCA =  'ITL'
050800011113     C                   ENDIF
050900011113     C                   ENDIF
051000011113     C*
051100011113     C                   IF        vabVMD > *zeros
051200020305     C                   IF        vabVAD <> 'EUR'
051300011113     C                   EVAL      vabVAD =  'ITL'
051400011113     C                   ENDIF
051500011113     C                   ENDIF
051600011113     C*
051700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051800011113     C                   Z-ADD     vabIAS        wrkDec
051900011113     C                   IF        wrkDec > *zeros
052000011113     C                   IF        vabVAS = 'ITL'
052100011113     C                   EVAL      vabIAS = *zeros
052200011113     C                   ENDIF
052300011113     C                   ENDIF
052400011113     C*
052500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052600011113     C                   Z-ADD     vabCAS        wrkDec
052700011113     C                   IF        wrkDec > *zeros
052800011113     C                   IF        vabVCA = 'ITL'
052900011113     C                   EVAL      vabCAS = *zeros
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053400011113     C                   Z-ADD     vabVMD        wrkDec
053500011113     C                   IF        wrkDec > *zeros
053600011113     C                   IF        vabVAD = 'ITL'
053700011113     C                   EVAL      vabVMD = *zeros
053800011113     C                   ENDIF
053900011113     C                   ENDIF
054000011113     C*
054100011113     C                   ENDSR
054200011113     C***
054300011113
054400011113
054500000801
054600000801
054700990920      /TITLE Invio dei dati al punto operativo.
054800010202     C     invio         BEGSR
054900990920     C*
055000021113     C* 1° invio FIVAT
055100010201     C                   reset                   dscmz
055200010201     C                   move      vlrpoi        cmzdst
055300021113     C                   eval      cmzfld = 'FIVATWWR'
055400010201     C                   eval      cmzmbd = vlrhdl
055500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055600021009     C***                if        prmfir = *blanks
055700021113     C                   eval      cmzfla = 'FIVAT00F'
055800021113     C                   eval      cmzmba = 'FIVAT00F'
055900021009     C***                else
056000021009     C***                eval      cmzfla = prmfir
056100021009     C***                eval      cmzmba = prmfir
056200021009     C***                endif
056300010201     C                   eval      cmznrr = *zeros
056400020305     C                   move      §ctrokvt      cmznrr
056500021018     C                   eval      cmzlba = vlrfl1
056600010201     C                   call(e)   'TIS711C'
056700010201     C                   parm                    dscmz
056800010201     C                   parm      *blanks       esito
056900010205     C                   if        %error
057000010205     C                             or cmzerr = '1'
057100010205     C                             or esito  = '1'
057200010205     C                   eval      wrkesito = '3'
057300010205     C                   else
057400010201     C*
057500021113     C* 2° invio FIVAB
057600010201     C                   reset                   dscmz
057700010201     C                   move      vlrpoi        cmzdst
057800010201     C                   eval      cmzfld = vlrfou
057900010201     C                   eval      cmzmbd = vlrhdl
058000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058100021009     C***                if        prmfir = *blanks
058200021113     C                   eval      cmzfla = 'FIVAB00F'
058300021113     C                   eval      cmzmba = 'FIVAB00F'
058400021009     C***                else
058500021009     C***                eval      cmzfla = prmfir
058600021009     C***                eval      cmzmba = prmfir
058700021009     C***                endif
058800010201     C                   eval      cmznrr = *zeros
058900010201     C                   move      §ctrokvb      cmznrr
059000021018     C                   eval      cmzlba = vlrfl1
059100010201     C                   call(e)   'TIS711C'
059200010201     C                   parm                    dscmz
059300010201     C                   parm      *blanks       esito
059400010201     C                   if        %error
059500010201     C                             or cmzerr = '1'
059600010201     C                             or esito  = '1'
059700010201     C                   eval      wrkesito = '3'
059800010201     C                   endif
059900010205     C                   endif
060000990920     C*
060100000613     C                   ENDSR
060200000613     C***
060300990910
060400000613     C     *inzsr        BEGSR
060500990910     C*
060600990910     C     *entry        plist
060700990920     C                   parm                    tivlrds
060800990921     C                   parm      wrkesito      esito
060900000724     C                   parm                    prmlit
061000000710     C                   parm                    prmfir
061100000613     C*
061200000830     C* CALCOLA LA DATA CORRENTE
061300100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
061400100118     C                   eval      datcor = %dec(%date() : *ISO)
061500000830     C*
061600000613     C                   ENDSR
061700000613     C***
