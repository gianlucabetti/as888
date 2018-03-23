000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130213     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500060307     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500141229     D curSped         s             10    INZ(*blanks)
002600141229     D depSped         s             10    INZ(*blanks)
002700090113
002800090113     D*------------------
002900090113     D* LINKING A DEFINIZIONI ESTERNE
003000090113     D*------------------
003100090113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
003200090113     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
003300090113
003400010201
003500000913     C                   reset                   rrnum
003600990921     C                   reset                   esito
003700990921     C                   reset                   wrkesito
003800000613     C*
003900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004000000613     C*
004100010202     C* Effettuo la chiamata al CLLE preposto
004200040506     C                   call(e)   'TITVVTC'
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
005500990908
005600000801
005700910830     C*--------------------------------------------------------
005800040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
005900910830     C*--------------------------------------------------------
006000040526     C     RWFILE        BEGSR
006100990910     C*
006200990914     C                   if        not %open(tivin00r)
006300990908     C                   open      tivin00r
006400990914     C                   endif
006500021113     C                   if        not %open(fivabwwr)
006600021113     C                   open      fivabwwr
006700990914     C                   endif
006800070103     C*
006900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007000020305     C                   exsr      prevat
007100010201     C*
007200010202     C                   if        chkcall = '0'
007300010202     C*
007400021113     C                   if        not %open(fivatwwr)
007500021113     C                   open      fivatwwr
007600010201     C                   endif
007700990910     C*
007800010201     C                   clear                   §CTROKVB          5 0
007900020305     C                   clear                   §CTROKVT          5 0
008000000801     C                   clear                   §CTRMO            5 0
008100000801     C                   clear                   §CTRNO            5 0
008200990910     C*
008300921023     C                   DO        *HIVAL
008400990913     C*
008500990915     C                   READ      tivin00r                               70
008600050627     C                   if        vindta > *blanks
008700000613     C                   add       1             rrnum
008800000801     C*
008900000801     C                   if        *in70 = *off
009000000801     C                             and
009100000801     C                             (vinflg = *blanks
009200000801     C                              or vinflg = '0'
009300000801     C                              or vinflg = '2')
009400000801     C*
009500000801     C                   clear                   vinmsg
009600000801     C                   eval      vinflg = '1'
009700070103     C*
009800070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
009900141229     C* se si sta leggendo un rcd di testata, la posizione di rottura è questa
010000141229     C                   if        %subst(vindta:49:1) = 'A'
010100141229     C                   eval      curSped = %subst(vindta:118:10)
010200141229     C                   endif
010300141229     C* se si sta leggendo un rcd di dettaglio, la posizione di rottura è questa
010400141229     C                   if        %subst(vindta:49:1) = 'B'
010500141229     C                   eval      curSped = %subst(vindta:80:10)
010600141229     C                   endif
010700070213     C                   if        curSped <> depSped
010800070213     C* Se prima bolla => importo bolla corrente
010900070213     C                   if        §CTRMO = *zeros
011000070213     C                   exsr      inzvar
011100070213     C                   exsr      defcam
011200070213     C                   exsr      impvab                                       => carico  VAB
011300070213     C                   else
011400070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
011500070213     C                   exsr      wrivab                                       => scarico VAB
011600070213     C                   exsr      inzvar
011700070213     C                   exsr      defcam
011800070213     C                   exsr      impvab                                       => carico  VAB
011900070213     C                   endif
012000071121     C                   exsr      exevate                                      => write VAT-E
012100070103     C* Salvo il raggruppamento spedizione cliente corrente
012200070103     C                   eval      depSped = curSped
012300070213     C*
012400070213     C* Se collo successivo x stessa bolla
012500070213     C                   else
012600070213     C                   exsr      impvab                                       => carico  VAB
012700070213     C                   exsr      exevate                                      => write VAT-E
012800070103     C                   endif
012900000905     C*
013000000905     C                   else
013100000905     C                   eval      vinflg = '1'
013200050628     C                   endif
013300000905     C                   endif
013400000905     C*
013500000905     C  N70              update    tivin000
013600000905     C*
013700991022     C  N70              ENDdo
013800070213     C*
013900070213     C* Scarico testata bolla rimasta in sospesa
014000070213     C                   exsr      wrivab                                       => scarico VAB
014100010202     C*
014200010202     C                   endif
014300990910
014400990910     C* Se non ci sono record con errori ...
014500000710     C                   if        §ctrno = 0
014600990910     C* ... restituisco esito OK.
014700990921     C                   eval      wrkesito = '0'
014800990910     C                   else
014900010201     C                   if        §ctrokvb > 0
015000990921     C                   eval      wrkesito = '1'
015100000710     C                   else
015200000710     C                   eval      wrkesito = '2'
015300990910     C                   endif
015400000710     C                   endif
015500990910     C*
015600990914     C                   if        %open(tivin00r)
015700990908     C                   close     tivin00r
015800990914     C                   endif
015900021113     C                   if        %open(fivabwwr)
016000021113     C                   close     fivabwwr
016100990914     C                   endif
016200021113     C                   if        %open(fivatwwr)
016300021113     C                   close     fivatwwr
016400010201     C                   endif
016500990910     C*
016600010201     C                   if        §ctrokvb > 0
016700000724     C                             and vlrpoi <> *zeros
016800010202     C                   exsr      invio
016900990920     C                   endif
017000990920     C*
017100910830     C                   ENDSR
017200000613     C***
017300010305
017400010305     C*----------------------------------------------------*
017500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017600010305     C*----------------------------------------------------*
017700020305     C     WRIVAB        BEGSR
017800010305     C*
017900060225     C* Quindi scarico il buffer del file d testata
018000021113     C                   write     fivab000                                     => scarico il VAB
018100010305     C*
018200010305     C                   ENDSR
018300990920
018400000801     C*----------------------------------------------------*
018500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018600000801     C*----------------------------------------------------*
018700010201     C     INZVAR        BEGSR
018800000801     C*
018900070213     C                   CLEAR                   FIVAB000
019000070213     C                   CLEAR                   FIVAT000
019100070213     C*
019200040802     C                   Z-ADD     *zeros        Num5_0            5 0
019300040802     C                   MOVEL     '0'           FlgCAS            1
019400000801     C*
019500000801     C                   ENDSR
019600000801     C*----------------------------------------------------*
019700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019800000801     C*----------------------------------------------------*
019900000801     C     DEFCAM        BEGSR
020000000801     C*
020100020619     C* Imposto i valori di default...
020200150112     C                   Z-ADD     0871538       VABCCM
020300150112     C                   Z-ADD     0871538       VATCCM
020400150112     C                   Z-ADD     087           VABLNP
020500150112     C                   Z-ADD     087           VATLNP
020600090113     C                   Z-ADD     000           VABCTR
020700120802     C                   MOVEL     '7Q'          VABCTM
020800040823     C                   MOVEL     '1'           VABCBO
020900020619     C* ... e poi verifico se sono stati passati come parametri
021000020619     C                   IF        vlrppt > *blanks
021100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021300020619     C                   EXSR      CHKNUM
021400020619     C                   IF        PiInt=*on
021500020619     C                   Z-ADD     PiVal         VABCCM
021600020619     C                   Z-ADD     PiVal         VATCCM
021700020619     C                   ENDIF
021800040506     C                   ENDIF
021900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABLNP
022400020619     C                   Z-ADD     PiVal         VATLNP
022500040506     C                   ENDIF
022600020619     C                   ENDIF
022700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022900020619     C                   EXSR      CHKNUM
023000020619     C                   IF        PiInt=*on
023100020619     C                   Z-ADD     PiVal         VABCTR
023200040506     C                   ENDIF
023300020619     C                   ENDIF
023400020619     C                   ENDIF
023500000801     C*
023600000801     C                   ENDSR
023700000801     C*----------------------------------------------------*
023800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023900000801     C*----------------------------------------------------*
024000040823     C     IMPVAB        BEGSR
024100010305     C*
024200000801     C                   Z-ADD     *zeros        errore            1 0
024300120813     C*
024400120813     C* Considero il record corrrente unicamente se trattasi d record testata
024500120813     C                   if        %subst(vindta:49:1) = 'A'
024600120813     C*
024700000830     C                   MOVEL     datcor        VABAAS
024800020305     C                   MOVEL     datcor        VATAAS
024900040526     C                   MOVE      datcor        VABMGS
025000040823     C                   MOVE(P)   vlrpoi        VABFGS
025100040823     C                   MOVE(P)   vlrpoi        VATFGS
025200080409     C*
025300080409     C* Reperimento campi ALFA
025400080409     C*
025500080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
025600090213     C                   EVAL      VABRSD=%trim(%subst(vindta:668:35))
025700080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025800080409     C     '@':'A'       XLATE     VABRSD        VABRSD
025900080409     C* ==
026000090213     C                   EVAL      VABRD2=%trim(%subst(vindta:668+35:50-35))
026100090213     C                   EVAL      VABIND=%trim(%subst(vindta:550:30))
026200090213     C                   EVAL      VABLOD=%trim(%subst(vindta:718:30))
026300090213     C                   EVAL      VABPRD=%trim(%subst(vindta:786:2))
026400090213     C                   EVAL      VABRMA=%trim(%subst(vindta:118:24))
026500101007     C***                EVAL      VABTSP=%trim(%subst(vindta:78:1))
026600090213     C                   EVAL      VABNOT=%trim(%subst(vindta:142:35))
026700090213     C                   EVAL      VABNT2=%trim(%subst(vindta:142+35:60-35))
026800080409     C*
026900080409     C* Reperimento campi NUMERICI
027000090113     C*
027100141229     C* NSP/RMN
027200141229     C                   EVAL      PiStr=%trim(%subst(vindta:123:7))
027300080409     C                   EXSR      CHKNUM
027400080409     C                   IF        PiInt=*on
027500080409     C                   Z-ADD     PiVal         VABNSP
027600080409     C                   Z-ADD     PiVal         VATNSP
027700130213     C                   Z-ADD     PiVal         VABRMN
027800080409     C                   ELSE
027900080409     C                   ADD       1             errore
028000080409     C                   Z-ADD     *zeros        VABNSP
028100130213     C                   Z-ADD     *zeros        VABRMN
028200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
028300130213     C                             + ' ' + 'VABNSP VATNSP VABRMN'
028400080409     C                   ENDIF
028500120905     C*
028600120905     C* RMN
028700130213     C***                EVAL      PiStr=%trim(%subst(vindta:122:11))
028800130213     C***                EXSR      CHKNUM
028900130213     C***                IF        PiInt=*on
029000130213     C***                Z-ADD     PiVal         VABRMN
029100130213     C***                ELSE
029200130213     C***                ADD       1             errore
029300130213     C***                Z-ADD     1             VABRMN
029400130213     C***                EVAL      vinmsg = %trimr(vinmsg)
029500130213     C***                          + ' ' + 'VABRMN'
029600130213     C***                ENDIF
029700080409     C* CAD
029800090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
029900080409     C                   EXSR      CHKNUM
030000080409     C                   IF        PiInt=*on
030100080409     C                   Z-ADD     PiVal         Num5_0
030200080409     C                   MOVEL(p)  Num5_0        VABCAD
030300080409     C                   ELSE
030400080409     C                   ADD       1             errore
030500080409     C                   EVAL      VABCAD = *zeros
030600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
030700080409     C                             + ' ' + 'VABCAD'
030800080409     C                   ENDIF
030900080409     C* NCL
031000090213     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
031100080409     C                   EXSR      CHKNUM
031200080409     C                   IF        PiInt=*on
031300080409     C                   Z-ADD     PiVal         VABNCL
031400080409     C                   ELSE
031500080409     C                   ADD       1             errore
031600080409     C                   Z-ADD     *zeros        VABNCL
031700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031800080409     C                             + ' ' + 'VABNCL'
031900080409     C                   ENDIF
032000080409     C* PKB
032100090213     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
032200080409     C                   EXSR      CHKNUM
032300080409     C                   IF        PiNum=*on
032400130213     C                   EVAL      PiVal = PiVal / 1000                         3 dec. senza virgola
032500090113     C                   Z-ADD     PiVal         VABPKB
032600080409     C                   ELSE
032700080409     C                   ADD       1             errore
032800080409     C                   Z-ADD     *zeros        VABPKB
032900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
033000080409     C                             + ' ' + 'VABPKB'
033100080409     C                   ENDIF
033200121005     C* VLB
033300130213     C***                EVAL      PiStr=%trim(%subst(vindta:63:7))
033400130213     C***                EXSR      CHKNUM
033500130213     C***                IF        PiNum=*on
033600130213     C***                EVAL      PiVal = PiVal / 1000                         3 dec. senza virgola
033700130213     C***                Z-ADD     PiVal         VABVLB
033800130213     C***                ELSE
033900130213     C***                ADD       1             errore
034000130213     C***                Z-ADD     *zeros        VABVLB
034100130213     C***                EVAL      vinmsg = %trimr(vinmsg)
034200130213     C***                          + ' ' + 'VABVLB'
034300130213     C***                ENDIF
034400090213     C* IAS
034500130213     C***                IF        %trim(%subst(vindta:82:13)) <> *blanks AND
034600130213     C***                          %trim(%subst(vindta:82:13)) <> *zeros
034700130213     C***                EVAL      VABVAS = %trim(%subst(vindta:95:3))
034800130213     C***                EVAL      PiStr=%trim(%subst(vindta:82:13))
034900130213     C***                EXSR      CHKNUM
035000130213     C***                IF        PiNum=*on
035100130213     C***                EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
035200130213     C***                Z-ADD     PiVal         VABIAS
035300130213     C***                ELSE
035400130213     C***                ADD       1             errore
035500130213     C***                Z-ADD     *zeros        VABIAS
035600130213     C***                EVAL      vinmsg = %trimr(vinmsg)
035700130213     C***                          + ' ' + 'VABIAS'
035800130213     C***                ENDIF
035900130213     C***                ENDIF
036000101007     C* CAS
036100101007     C                   IF        %trim(%subst(vindta:1523:13)) <> *blanks AND
036200101007     C                             %trim(%subst(vindta:1523:13)) <> *zeros
036300101007     C                   MOVEL     '1'           FlgCAS
036400150112     C                   EVAL      VABVCA='EUR'
036500150112     C                   EVAL      VABTIC='TM'
036600101007     C                   EVAL      PiStr=%trim(%subst(vindta:1523:13))
036700101007     C                   EXSR      CHKNUM
036800101007     C                   IF        PiNum=*on
036900101007     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
037000101007     C                   Z-ADD     PiVal         VABCAS
037100101007     C                   ELSE
037200101007     C                   ADD       1             errore
037300101007     C                   Z-ADD     *zeros        VABCAS
037400101007     C                   EVAL      vinmsg = %trimr(vinmsg)
037500101007     C                             + ' ' + 'VABCAS'
037600101007     C                   ENDIF
037700101007     C                   ENDIF
037800071121     C*
037900080415     C* Carico l'estensioni A e B del FNVAT
038000150220     C* e I + J
038100080415     C                   exsr      exevata
038200071121     C                   exsr      exevatb
038300150220     C                   exsr      exevatij
038400010205     C*
038500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038600040802     C                   IF        FlgCAS <> '0'
038700010205     C                   EVAL      VABCBO = '4'
038800010205     C                   ENDIF
038900020305     C*
039000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039100011113     C                   EXSR      CHKIMPDIV
039200120813     C*
039300120813     C                   endif
039400010202     C*
039500000801     C* Ebbene...
039600000801     C                   ADD       1             §CTRMO
039700010201     C                   IF        errore <> *zeros
039800000801     C                   ADD       1             §CTRNO
039900000801     C                   EVAL      vinflg = '2'
040000000801     C                   ELSE
040100010201     C                   ADD       1             §CTROKVB
040200000801     C                   ENDIF
040300000801     C*
040400000801     C                   ENDSR
040500070102     C*----------------------------------------------------*
040600070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
040700070102     C*----------------------------------------------------*
040800070103     C     EXEVATE       BEGSR
040900120813     C*
041000120813     C* Considero il record corrrente unicamente se trattasi d record dettaglio
041100120813     C                   if        %subst(vindta:49:1) = 'B'
041200101007     C*
041300141229     C                   EVAL      VATTRC='E'
041400150113     C                   EVAL      VATNOT=%trim(%subst(vindta:63:8))
041500101007     C*
041600120802     C                   exsr      wrivat
041700120813     C*
041800120813     C                   endif
041900070102     C*
042000070102     C                   ENDSR
042100080415     C*----------------------------------------------------*
042200080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
042300080415     C*----------------------------------------------------*
042400080415     C     EXEVATA       BEGSR
042500080415     C*
042600120802     C                   EVAL      VATTRC='A'
042700120802     C                   EVAL      VATNOT=%trim(%subst(vindta:748:22))
042800080415     C*
042900130213     C***                exsr      wrivat                                       => scarico VAT
043000080415     C*
043100080415     C                   ENDSR
043200071121     C*----------------------------------------------------*
043300071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
043400071121     C*----------------------------------------------------*
043500071121     C     EXEVATB       BEGSR
043600071121     C*
043700090213     C                   EVAL      VATTRC='B'
043800120802     C                   EVAL      VATNOT=%trim(%subst(vindta:652:7+9))
043900071121     C*
044000090213     C                   exsr      wrivat                                       => scarico VAT
044100071121     C*
044200071121     C                   ENDSR
044300150220     C*----------------------------------------------------*
044400150220     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" "J"
044500150220     C*----------------------------------------------------*
044600150220     C     EXEVATIJ      BEGSR
044700150220     C*
044800150220     C                   EVAL      VATTRC='I'
044900150220     C                   EVAL      VATNOT=%trim(%subst(vindta:1549:35))
045000150220     C*
045100150220     C                   exsr      wrivat                                       => scarico VAT
045200150220     C*
045300150220     C                   EVAL      VATTRC='J'
045400150220     C                   EVAL      VATNOT=%trim(%subst(vindta:1549+35:18))
045500150220     C*
045600150220     C                   exsr      wrivat                                       => scarico VAT
045700150220     C*
045800150220     C                   ENDSR
045900010201     C*----------------------------------------------------*
046000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046100010201     C*----------------------------------------------------*
046200020305     C     WRIVAT        BEGSR
046300050628     C*
046400060223     C* Scrivo solo se valorizzato qualcosa
046500060223     C                   IF        VATNOT <> *blanks
046600040802     C                   WRITE     FIVAT000
046700060223     C                   ENDIF
046800010201     C*
046900010201     C                   ENDSR
047000010202     C*----------------------------------------------------*
047100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047200010202     C*----------------------------------------------------*
047300020305     C     PREVAT        BEGSR
047400010202     C*
047500021113     C* Compongo il nome del membro da dare al FIVATWWR
047600010202     C                   eval      parmbr = vlrhdl
047700010202     C                   movel     'M'           parmbr
047800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
047900010202     C                   eval      paropz = '1'
048000010202     C* Effettuo la chiamata al CLLE preposto
048100040506     C                   call(e)   'TITVVTC'
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
049700090113     C*
049800090113     C                   IF        PiDecChr = *blanks
049900090113     C                   EVAL      PiDecChr = ','
050000090113     C                   ENDIF
050100090113     C*
050200090113     C                   callp(e)  UBISNUM_Check(PiStr
050300090113     C                                          :PiDecChr
050400090113     C                                          :PiVal
050500090113     C                                          :PiNum
050600090113     C                                          :PiInt)
050700090113     C*
050800090113     C                   IF        %error
050900090113     C                   EVAL      PiInt=*off
051000090113     C                   ENDIF
051100000801     C*
051200000801     C                   ENDSR
051300000801     C***
051400000801
051500011113
051600011113     C*----------------------------------------------------*
051700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051800011113     C*----------------------------------------------------*
051900011113     C     CHKIMPDIV     BEGSR
052000011113     C*
052100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052200011113     C                   Z-ADD     *zeros        wrkDec            9 9
052300011113     C*
052400011113     C* Come prima cosa effettuo considerazioni sulla divisa
052500011113     C                   IF        vabIAS > *zeros
052600011113     C                   IF        vabVAS <> 'EUR'
052700011113     C                   EVAL      vabVAS =  'ITL'
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C                   IF        vabCAS > *zeros
053200011113     C                   IF        vabVCA <> 'EUR'
053300011113     C                   EVAL      vabVCA =  'ITL'
053400011113     C                   ENDIF
053500011113     C                   ENDIF
053600011113     C*
053700011113     C                   IF        vabVMD > *zeros
053800020305     C                   IF        vabVAD <> 'EUR'
053900011113     C                   EVAL      vabVAD =  'ITL'
054000011113     C                   ENDIF
054100011113     C                   ENDIF
054200011113     C*
054300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054400011113     C                   Z-ADD     vabIAS        wrkDec
054500011113     C                   IF        wrkDec > *zeros
054600011113     C                   IF        vabVAS = 'ITL'
054700011113     C                   EVAL      vabIAS = *zeros
054800011113     C                   ENDIF
054900011113     C                   ENDIF
055000011113     C*
055100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055200011113     C                   Z-ADD     vabCAS        wrkDec
055300011113     C                   IF        wrkDec > *zeros
055400011113     C                   IF        vabVCA = 'ITL'
055500011113     C                   EVAL      vabCAS = *zeros
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056000011113     C                   Z-ADD     vabVMD        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVAD = 'ITL'
056300011113     C                   EVAL      vabVMD = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C                   ENDSR
056800011113     C***
056900011113
057000011113
057100000801
057200000801
057300990920      /TITLE Invio dei dati al punto operativo.
057400010202     C     invio         BEGSR
057500990920     C*
057600021113     C* 1° invio FIVAT
057700010201     C                   reset                   dscmz
057800010201     C                   move      vlrpoi        cmzdst
057900021113     C                   eval      cmzfld = 'FIVATWWR'
058000010201     C                   eval      cmzmbd = vlrhdl
058100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058200021009     C***                if        prmfir = *blanks
058300021113     C                   eval      cmzfla = 'FIVAT00F'
058400021113     C                   eval      cmzmba = 'FIVAT00F'
058500021009     C***                else
058600021009     C***                eval      cmzfla = prmfir
058700021009     C***                eval      cmzmba = prmfir
058800021009     C***                endif
058900010201     C                   eval      cmznrr = *zeros
059000020305     C                   move      §ctrokvt      cmznrr
059100021018     C                   eval      cmzlba = vlrfl1
059200010201     C                   call(e)   'TIS711C'
059300010201     C                   parm                    dscmz
059400010201     C                   parm      *blanks       esito
059500010205     C                   if        %error
059600010205     C                             or cmzerr = '1'
059700010205     C                             or esito  = '1'
059800010205     C                   eval      wrkesito = '3'
059900010205     C                   else
060000010201     C*
060100021113     C* 2° invio FIVAB
060200010201     C                   reset                   dscmz
060300010201     C                   move      vlrpoi        cmzdst
060400010201     C                   eval      cmzfld = vlrfou
060500010201     C                   eval      cmzmbd = vlrhdl
060600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060700021009     C***                if        prmfir = *blanks
060800021113     C                   eval      cmzfla = 'FIVAB00F'
060900021113     C                   eval      cmzmba = 'FIVAB00F'
061000021009     C***                else
061100021009     C***                eval      cmzfla = prmfir
061200021009     C***                eval      cmzmba = prmfir
061300021009     C***                endif
061400010201     C                   eval      cmznrr = *zeros
061500010201     C                   move      §ctrokvb      cmznrr
061600021018     C                   eval      cmzlba = vlrfl1
061700010201     C                   call(e)   'TIS711C'
061800010201     C                   parm                    dscmz
061900010201     C                   parm      *blanks       esito
062000010201     C                   if        %error
062100010201     C                             or cmzerr = '1'
062200010201     C                             or esito  = '1'
062300010201     C                   eval      wrkesito = '3'
062400010201     C                   endif
062500010205     C                   endif
062600990920     C*
062700000613     C                   ENDSR
062800000613     C***
062900070411
063000070411     C     *pssr         BEGSR
063100070411     C*
063200070411     C                   if        %open(tivin00r)
063300070411     C                   close     tivin00r
063400070411     C                   endif
063500070411     C                   if        %open(fivabwwr)
063600070411     C                   close     fivabwwr
063700070411     C                   endif
063800070411     C                   if        %open(fivatwwr)
063900070411     C                   close     fivatwwr
064000070411     C                   endif
064100070411     C*
064200070411     C* Effettuo la chiamata al CLLE preposto
064300070411     C                   call(e)   'TITVVTC'
064400070411     C                   parm                    parccm
064500070411     C                   parm                    parmbr
064600070411     C                   parm      '2'           paropz
064700070411     C*
064800070411     C                   eval      wrkesito = '2'
064900070411     C*
065000070411     C                   seton                                        LR
065100070411     C*
065200070411     C                   ENDSR     '*CANCL'
065300070411     C***
065400070411
065500990910
065600000613     C     *inzsr        BEGSR
065700990910     C*
065800990910     C     *entry        plist
065900990920     C                   parm                    tivlrds
066000990921     C                   parm      wrkesito      esito
066100000724     C                   parm                    prmlit
066200000710     C                   parm                    prmfir
066300000613     C*
066400000830     C* CALCOLA LA DATA CORRENTE
066500120813     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
066600120813     C                   eval      datcor = %dec(%date() : *ISO)
066700000830     C*
066800000613     C                   ENDSR
066900000613     C***
