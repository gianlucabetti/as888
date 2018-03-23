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
025200180206     C*
025300180206     C* a pos. 25 c'è il codice cliente
025400180206     C                   SELECT
025500180206     C                   WHEN      %trim(%subst(vindta:25:7))='0871538'
025600180206     C                   Z-ADD     0871538       VABCCM
025700180206     C                   Z-ADD     0871538       VATCCM
025800180206     C                   Z-ADD     087           VABLNP
025900180206     C                   Z-ADD     087           VATLNP
026000180212     C                   WHEN      %trim(%subst(vindta:25:7))='0871887'
026100180212     C                   Z-ADD     0871887       VABCCM
026200180212     C                   Z-ADD     0871887       VATCCM
026300180206     C                   Z-ADD     087           VABLNP
026400180206     C                   Z-ADD     087           VATLNP
026500180212     C                   Z-ADD     000           VABCTR
026600180206     C                   ENDSL
026700080409     C*
026800080409     C* Reperimento campi ALFA
026900080409     C*
027000080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
027100090213     C                   EVAL      VABRSD=%trim(%subst(vindta:668:35))
027200080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027300080409     C     '@':'A'       XLATE     VABRSD        VABRSD
027400080409     C* ==
027500090213     C                   EVAL      VABRD2=%trim(%subst(vindta:668+35:50-35))
027600090213     C                   EVAL      VABIND=%trim(%subst(vindta:550:30))
027700090213     C                   EVAL      VABLOD=%trim(%subst(vindta:718:30))
027800090213     C                   EVAL      VABPRD=%trim(%subst(vindta:786:2))
027900090213     C                   EVAL      VABRMA=%trim(%subst(vindta:118:24))
028000101007     C***                EVAL      VABTSP=%trim(%subst(vindta:78:1))
028100090213     C                   EVAL      VABNOT=%trim(%subst(vindta:142:35))
028200090213     C                   EVAL      VABNT2=%trim(%subst(vindta:142+35:60-35))
028300180221     C                   SELECT
028400180221     C                   WHEN      VABCCM = 0871887
028500180221     C                   EVAL      VABRMO=%trim(%subst(vindta:2:12))
028600180221     C                   ENDSL
028700080409     C*
028800080409     C* Reperimento campi NUMERICI
028900090113     C*
029000141229     C* NSP/RMN
029100141229     C                   EVAL      PiStr=%trim(%subst(vindta:123:7))
029200080409     C                   EXSR      CHKNUM
029300080409     C                   IF        PiInt=*on
029400080409     C                   Z-ADD     PiVal         VABNSP
029500080409     C                   Z-ADD     PiVal         VATNSP
029600130213     C                   Z-ADD     PiVal         VABRMN
029700080409     C                   ELSE
029800080409     C                   ADD       1             errore
029900080409     C                   Z-ADD     *zeros        VABNSP
030000130213     C                   Z-ADD     *zeros        VABRMN
030100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
030200130213     C                             + ' ' + 'VABNSP VATNSP VABRMN'
030300080409     C                   ENDIF
030400120905     C*
030500120905     C* RMN
030600130213     C***                EVAL      PiStr=%trim(%subst(vindta:122:11))
030700130213     C***                EXSR      CHKNUM
030800130213     C***                IF        PiInt=*on
030900130213     C***                Z-ADD     PiVal         VABRMN
031000130213     C***                ELSE
031100130213     C***                ADD       1             errore
031200130213     C***                Z-ADD     1             VABRMN
031300130213     C***                EVAL      vinmsg = %trimr(vinmsg)
031400130213     C***                          + ' ' + 'VABRMN'
031500130213     C***                ENDIF
031600080409     C* CAD
031700090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
031800080409     C                   EXSR      CHKNUM
031900080409     C                   IF        PiInt=*on
032000080409     C                   Z-ADD     PiVal         Num5_0
032100080409     C                   MOVEL(p)  Num5_0        VABCAD
032200080409     C                   ELSE
032300080409     C                   ADD       1             errore
032400080409     C                   EVAL      VABCAD = *zeros
032500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032600080409     C                             + ' ' + 'VABCAD'
032700080409     C                   ENDIF
032800080409     C* NCL
032900090213     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
033000080409     C                   EXSR      CHKNUM
033100080409     C                   IF        PiInt=*on
033200080409     C                   Z-ADD     PiVal         VABNCL
033300080409     C                   ELSE
033400080409     C                   ADD       1             errore
033500080409     C                   Z-ADD     *zeros        VABNCL
033600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
033700080409     C                             + ' ' + 'VABNCL'
033800080409     C                   ENDIF
033900080409     C* PKB
034000090213     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
034100080409     C                   EXSR      CHKNUM
034200080409     C                   IF        PiNum=*on
034300130213     C                   EVAL      PiVal = PiVal / 1000                         3 dec. senza virgola
034400090113     C                   Z-ADD     PiVal         VABPKB
034500080409     C                   ELSE
034600080409     C                   ADD       1             errore
034700080409     C                   Z-ADD     *zeros        VABPKB
034800080409     C                   EVAL      vinmsg = %trimr(vinmsg)
034900080409     C                             + ' ' + 'VABPKB'
035000080409     C                   ENDIF
035100121005     C* VLB
035200130213     C***                EVAL      PiStr=%trim(%subst(vindta:63:7))
035300130213     C***                EXSR      CHKNUM
035400130213     C***                IF        PiNum=*on
035500130213     C***                EVAL      PiVal = PiVal / 1000                         3 dec. senza virgola
035600130213     C***                Z-ADD     PiVal         VABVLB
035700130213     C***                ELSE
035800130213     C***                ADD       1             errore
035900130213     C***                Z-ADD     *zeros        VABVLB
036000130213     C***                EVAL      vinmsg = %trimr(vinmsg)
036100130213     C***                          + ' ' + 'VABVLB'
036200130213     C***                ENDIF
036300090213     C* IAS
036400130213     C***                IF        %trim(%subst(vindta:82:13)) <> *blanks AND
036500130213     C***                          %trim(%subst(vindta:82:13)) <> *zeros
036600130213     C***                EVAL      VABVAS = %trim(%subst(vindta:95:3))
036700130213     C***                EVAL      PiStr=%trim(%subst(vindta:82:13))
036800130213     C***                EXSR      CHKNUM
036900130213     C***                IF        PiNum=*on
037000130213     C***                EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
037100130213     C***                Z-ADD     PiVal         VABIAS
037200130213     C***                ELSE
037300130213     C***                ADD       1             errore
037400130213     C***                Z-ADD     *zeros        VABIAS
037500130213     C***                EVAL      vinmsg = %trimr(vinmsg)
037600130213     C***                          + ' ' + 'VABIAS'
037700130213     C***                ENDIF
037800130213     C***                ENDIF
037900101007     C* CAS
038000101007     C                   IF        %trim(%subst(vindta:1523:13)) <> *blanks AND
038100101007     C                             %trim(%subst(vindta:1523:13)) <> *zeros
038200101007     C                   MOVEL     '1'           FlgCAS
038300150112     C                   EVAL      VABVCA='EUR'
038400180221     C                   SELECT
038500180221     C                   WHEN      VABCCM = 0871538
038600180221     C                   EVAL      VABTIC='TM'
038700180221     C                   WHEN      VABCCM = 0871887
038800180221     C                   EVAL      VABTIC='OM'
038900180221     C                   ENDSL
039000101007     C                   EVAL      PiStr=%trim(%subst(vindta:1523:13))
039100101007     C                   EXSR      CHKNUM
039200101007     C                   IF        PiNum=*on
039300101007     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
039400101007     C                   Z-ADD     PiVal         VABCAS
039500101007     C                   ELSE
039600101007     C                   ADD       1             errore
039700101007     C                   Z-ADD     *zeros        VABCAS
039800101007     C                   EVAL      vinmsg = %trimr(vinmsg)
039900101007     C                             + ' ' + 'VABCAS'
040000101007     C                   ENDIF
040100101007     C                   ENDIF
040200071121     C*
040300080415     C* Carico l'estensioni A e B del FNVAT
040400150220     C* e I + J
040500080415     C                   exsr      exevata
040600071121     C                   exsr      exevatb
040700150220     C                   exsr      exevatij
040800010205     C*
040900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041000040802     C                   IF        FlgCAS <> '0'
041100010205     C                   EVAL      VABCBO = '4'
041200010205     C                   ENDIF
041300020305     C*
041400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041500011113     C                   EXSR      CHKIMPDIV
041600120813     C*
041700120813     C                   endif
041800010202     C*
041900000801     C* Ebbene...
042000000801     C                   ADD       1             §CTRMO
042100010201     C                   IF        errore <> *zeros
042200000801     C                   ADD       1             §CTRNO
042300000801     C                   EVAL      vinflg = '2'
042400000801     C                   ELSE
042500010201     C                   ADD       1             §CTROKVB
042600000801     C                   ENDIF
042700000801     C*
042800000801     C                   ENDSR
042900070102     C*----------------------------------------------------*
043000070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
043100070102     C*----------------------------------------------------*
043200070103     C     EXEVATE       BEGSR
043300120813     C*
043400120813     C* Considero il record corrrente unicamente se trattasi d record dettaglio
043500120813     C                   if        %subst(vindta:49:1) = 'B'
043600101007     C*
043700141229     C                   EVAL      VATTRC='E'
043800150113     C                   EVAL      VATNOT=%trim(%subst(vindta:63:8))
043900101007     C*
044000120802     C                   exsr      wrivat
044100120813     C*
044200120813     C                   endif
044300070102     C*
044400070102     C                   ENDSR
044500080415     C*----------------------------------------------------*
044600080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
044700080415     C*----------------------------------------------------*
044800080415     C     EXEVATA       BEGSR
044900080415     C*
045000120802     C                   EVAL      VATTRC='A'
045100120802     C                   EVAL      VATNOT=%trim(%subst(vindta:748:22))
045200080415     C*
045300130213     C***                exsr      wrivat                                       => scarico VAT
045400080415     C*
045500080415     C                   ENDSR
045600071121     C*----------------------------------------------------*
045700071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
045800071121     C*----------------------------------------------------*
045900071121     C     EXEVATB       BEGSR
046000071121     C*
046100090213     C                   EVAL      VATTRC='B'
046200120802     C                   EVAL      VATNOT=%trim(%subst(vindta:652:7+9))
046300071121     C*
046400090213     C                   exsr      wrivat                                       => scarico VAT
046500071121     C*
046600071121     C                   ENDSR
046700150220     C*----------------------------------------------------*
046800150220     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" "J"
046900150220     C*----------------------------------------------------*
047000150220     C     EXEVATIJ      BEGSR
047100150220     C*
047200150220     C                   EVAL      VATTRC='I'
047300150220     C                   EVAL      VATNOT=%trim(%subst(vindta:1549:35))
047400150220     C*
047500150220     C                   exsr      wrivat                                       => scarico VAT
047600150220     C*
047700150220     C                   EVAL      VATTRC='J'
047800150220     C                   EVAL      VATNOT=%trim(%subst(vindta:1549+35:18))
047900150220     C*
048000150220     C                   exsr      wrivat                                       => scarico VAT
048100150220     C*
048200150220     C                   ENDSR
048300010201     C*----------------------------------------------------*
048400040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048500010201     C*----------------------------------------------------*
048600020305     C     WRIVAT        BEGSR
048700050628     C*
048800060223     C* Scrivo solo se valorizzato qualcosa
048900060223     C                   IF        VATNOT <> *blanks
049000040802     C                   WRITE     FIVAT000
049100060223     C                   ENDIF
049200010201     C*
049300010201     C                   ENDSR
049400010202     C*----------------------------------------------------*
049500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049600010202     C*----------------------------------------------------*
049700020305     C     PREVAT        BEGSR
049800010202     C*
049900021113     C* Compongo il nome del membro da dare al FIVATWWR
050000010202     C                   eval      parmbr = vlrhdl
050100010202     C                   movel     'M'           parmbr
050200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050300010202     C                   eval      paropz = '1'
050400010202     C* Effettuo la chiamata al CLLE preposto
050500040506     C                   call(e)   'TITVVTC'
050600010202     C                   parm                    parccm
050700010202     C                   parm                    parmbr
050800010202     C                   parm                    paropz
050900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051000010202     C                   if        %error
051100010202     C                   movel     '1'           chkcall
051200010202     C                   else
051300010202     C                   movel     '0'           chkcall
051400010202     C                   endif
051500010202     C*
051600010202     C                   ENDSR
051700000801     C*----------------------------------------------------*
051800000801     C*  CONTROLLO NUMERICITA' CAMPI
051900000801     C*----------------------------------------------------*
052000000801     C     CHKNUM        BEGSR
052100090113     C*
052200090113     C                   IF        PiDecChr = *blanks
052300090113     C                   EVAL      PiDecChr = ','
052400090113     C                   ENDIF
052500090113     C*
052600090113     C                   callp(e)  UBISNUM_Check(PiStr
052700090113     C                                          :PiDecChr
052800090113     C                                          :PiVal
052900090113     C                                          :PiNum
053000090113     C                                          :PiInt)
053100090113     C*
053200090113     C                   IF        %error
053300090113     C                   EVAL      PiInt=*off
053400090113     C                   ENDIF
053500000801     C*
053600000801     C                   ENDSR
053700000801     C***
053800000801
053900011113
054000011113     C*----------------------------------------------------*
054100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054200011113     C*----------------------------------------------------*
054300011113     C     CHKIMPDIV     BEGSR
054400011113     C*
054500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054600011113     C                   Z-ADD     *zeros        wrkDec            9 9
054700011113     C*
054800011113     C* Come prima cosa effettuo considerazioni sulla divisa
054900011113     C                   IF        vabIAS > *zeros
055000011113     C                   IF        vabVAS <> 'EUR'
055100011113     C                   EVAL      vabVAS =  'ITL'
055200011113     C                   ENDIF
055300011113     C                   ENDIF
055400011113     C*
055500011113     C                   IF        vabCAS > *zeros
055600011113     C                   IF        vabVCA <> 'EUR'
055700011113     C                   EVAL      vabVCA =  'ITL'
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C                   IF        vabVMD > *zeros
056200020305     C                   IF        vabVAD <> 'EUR'
056300011113     C                   EVAL      vabVAD =  'ITL'
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056800011113     C                   Z-ADD     vabIAS        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVAS = 'ITL'
057100011113     C                   EVAL      vabIAS = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057600011113     C                   Z-ADD     vabCAS        wrkDec
057700011113     C                   IF        wrkDec > *zeros
057800011113     C                   IF        vabVCA = 'ITL'
057900011113     C                   EVAL      vabCAS = *zeros
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058400011113     C                   Z-ADD     vabVMD        wrkDec
058500011113     C                   IF        wrkDec > *zeros
058600011113     C                   IF        vabVAD = 'ITL'
058700011113     C                   EVAL      vabVMD = *zeros
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C                   ENDSR
059200011113     C***
059300011113
059400011113
059500000801
059600000801
059700990920      /TITLE Invio dei dati al punto operativo.
059800010202     C     invio         BEGSR
059900990920     C*
060000021113     C* 1° invio FIVAT
060100010201     C                   reset                   dscmz
060200010201     C                   move      vlrpoi        cmzdst
060300021113     C                   eval      cmzfld = 'FIVATWWR'
060400010201     C                   eval      cmzmbd = vlrhdl
060500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060600021009     C***                if        prmfir = *blanks
060700021113     C                   eval      cmzfla = 'FIVAT00F'
060800021113     C                   eval      cmzmba = 'FIVAT00F'
060900021009     C***                else
061000021009     C***                eval      cmzfla = prmfir
061100021009     C***                eval      cmzmba = prmfir
061200021009     C***                endif
061300010201     C                   eval      cmznrr = *zeros
061400020305     C                   move      §ctrokvt      cmznrr
061500021018     C                   eval      cmzlba = vlrfl1
061600010201     C                   call(e)   'TIS711C'
061700010201     C                   parm                    dscmz
061800010201     C                   parm      *blanks       esito
061900010205     C                   if        %error
062000010205     C                             or cmzerr = '1'
062100010205     C                             or esito  = '1'
062200010205     C                   eval      wrkesito = '3'
062300010205     C                   else
062400010201     C*
062500021113     C* 2° invio FIVAB
062600010201     C                   reset                   dscmz
062700010201     C                   move      vlrpoi        cmzdst
062800010201     C                   eval      cmzfld = vlrfou
062900010201     C                   eval      cmzmbd = vlrhdl
063000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063100021009     C***                if        prmfir = *blanks
063200021113     C                   eval      cmzfla = 'FIVAB00F'
063300021113     C                   eval      cmzmba = 'FIVAB00F'
063400021009     C***                else
063500021009     C***                eval      cmzfla = prmfir
063600021009     C***                eval      cmzmba = prmfir
063700021009     C***                endif
063800010201     C                   eval      cmznrr = *zeros
063900010201     C                   move      §ctrokvb      cmznrr
064000021018     C                   eval      cmzlba = vlrfl1
064100010201     C                   call(e)   'TIS711C'
064200010201     C                   parm                    dscmz
064300010201     C                   parm      *blanks       esito
064400010201     C                   if        %error
064500010201     C                             or cmzerr = '1'
064600010201     C                             or esito  = '1'
064700010201     C                   eval      wrkesito = '3'
064800010201     C                   endif
064900010205     C                   endif
065000990920     C*
065100000613     C                   ENDSR
065200000613     C***
065300070411
065400070411     C     *pssr         BEGSR
065500070411     C*
065600070411     C                   if        %open(tivin00r)
065700070411     C                   close     tivin00r
065800070411     C                   endif
065900070411     C                   if        %open(fivabwwr)
066000070411     C                   close     fivabwwr
066100070411     C                   endif
066200070411     C                   if        %open(fivatwwr)
066300070411     C                   close     fivatwwr
066400070411     C                   endif
066500070411     C*
066600070411     C* Effettuo la chiamata al CLLE preposto
066700070411     C                   call(e)   'TITVVTC'
066800070411     C                   parm                    parccm
066900070411     C                   parm                    parmbr
067000070411     C                   parm      '2'           paropz
067100070411     C*
067200070411     C                   eval      wrkesito = '2'
067300070411     C*
067400070411     C                   seton                                        LR
067500070411     C*
067600070411     C                   ENDSR     '*CANCL'
067700070411     C***
067800070411
067900990910
068000000613     C     *inzsr        BEGSR
068100990910     C*
068200990910     C     *entry        plist
068300990920     C                   parm                    tivlrds
068400990921     C                   parm      wrkesito      esito
068500000724     C                   parm                    prmlit
068600000710     C                   parm                    prmfir
068700000613     C*
068800000830     C* CALCOLA LA DATA CORRENTE
068900120813     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
069000120813     C                   eval      datcor = %dec(%date() : *ISO)
069100000830     C*
069200000613     C                   ENDSR
069300000613     C***
