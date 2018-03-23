000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200090113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
002500150302     D curSped         s              9    INZ(*blanks)
002600150302     D depSped         s              9    INZ(*blanks)
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
009900150302     C                   eval      curSped = %subst(vindta:25:9)
010000070213     C                   if        curSped <> depSped
010100070213     C* Se prima bolla => importo bolla corrente
010200070213     C                   if        §CTRMO = *zeros
010300070213     C                   exsr      inzvar
010400070213     C                   exsr      defcam
010500070213     C                   exsr      impvab                                       => carico  VAB
010600070213     C                   else
010700070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
010800070213     C                   exsr      wrivab                                       => scarico VAB
010900070213     C                   exsr      inzvar
011000070213     C                   exsr      defcam
011100070213     C                   exsr      impvab                                       => carico  VAB
011200070213     C                   endif
011300071121     C                   exsr      exevate                                      => write VAT-E
011400070103     C* Salvo il raggruppamento spedizione cliente corrente
011500070103     C                   eval      depSped = curSped
011600070213     C*
011700070213     C* Se collo successivo x stessa bolla
011800070213     C                   else
011900070213     C                   exsr      impvab                                       => carico  VAB
012000070213     C                   exsr      exevate                                      => write VAT-E
012100070103     C                   endif
012200000905     C*
012300000905     C                   else
012400000905     C                   eval      vinflg = '1'
012500050628     C                   endif
012600000905     C                   endif
012700000905     C*
012800000905     C  N70              update    tivin000
012900000905     C*
013000991022     C  N70              ENDdo
013100070213     C*
013200070213     C* Scarico testata bolla rimasta in sospesa
013300070213     C                   exsr      wrivab                                       => scarico VAB
013400010202     C*
013500010202     C                   endif
013600990910
013700990910     C* Se non ci sono record con errori ...
013800000710     C                   if        §ctrno = 0
013900990910     C* ... restituisco esito OK.
014000990921     C                   eval      wrkesito = '0'
014100990910     C                   else
014200010201     C                   if        §ctrokvb > 0
014300990921     C                   eval      wrkesito = '1'
014400000710     C                   else
014500000710     C                   eval      wrkesito = '2'
014600990910     C                   endif
014700000710     C                   endif
014800990910     C*
014900990914     C                   if        %open(tivin00r)
015000990908     C                   close     tivin00r
015100990914     C                   endif
015200021113     C                   if        %open(fivabwwr)
015300021113     C                   close     fivabwwr
015400990914     C                   endif
015500021113     C                   if        %open(fivatwwr)
015600021113     C                   close     fivatwwr
015700010201     C                   endif
015800990910     C*
015900010201     C                   if        §ctrokvb > 0
016000000724     C                             and vlrpoi <> *zeros
016100010202     C                   exsr      invio
016200990920     C                   endif
016300990920     C*
016400910830     C                   ENDSR
016500000613     C***
016600010305
016700010305     C*----------------------------------------------------*
016800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
016900010305     C*----------------------------------------------------*
017000020305     C     WRIVAB        BEGSR
017100010305     C*
017200060225     C* Quindi scarico il buffer del file d testata
017300021113     C                   write     fivab000                                     => scarico il VAB
017400010305     C*
017500010305     C                   ENDSR
017600990920
017700000801     C*----------------------------------------------------*
017800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017900000801     C*----------------------------------------------------*
018000010201     C     INZVAR        BEGSR
018100000801     C*
018200070213     C                   CLEAR                   FIVAB000
018300070213     C                   CLEAR                   FIVAT000
018400070213     C*
018500040802     C                   Z-ADD     *zeros        Num5_0            5 0
018600040802     C                   MOVEL     '0'           FlgCAS            1
018700000801     C*
018800000801     C                   ENDSR
018900000801     C*----------------------------------------------------*
019000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019100000801     C*----------------------------------------------------*
019200000801     C     DEFCAM        BEGSR
019300000801     C*
019400020619     C* Imposto i valori di default...
019500150302     C                   Z-ADD     1700208       VABCCM
019600150302     C                   Z-ADD     1700208       VATCCM
019700150302     C                   Z-ADD     170           VABLNP
019800150302     C                   Z-ADD     170           VATLNP
019900090113     C                   Z-ADD     000           VABCTR
020000120802     C                   MOVEL     '7Q'          VABCTM
020100040823     C                   MOVEL     '1'           VABCBO
020200020619     C* ... e poi verifico se sono stati passati come parametri
020300020619     C                   IF        vlrppt > *blanks
020400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020600020619     C                   EXSR      CHKNUM
020700020619     C                   IF        PiInt=*on
020800020619     C                   Z-ADD     PiVal         VABCCM
020900020619     C                   Z-ADD     PiVal         VATCCM
021000020619     C                   ENDIF
021100040506     C                   ENDIF
021200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021400020619     C                   EXSR      CHKNUM
021500020619     C                   IF        PiInt=*on
021600020619     C                   Z-ADD     PiVal         VABLNP
021700020619     C                   Z-ADD     PiVal         VATLNP
021800040506     C                   ENDIF
021900020619     C                   ENDIF
022000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022200020619     C                   EXSR      CHKNUM
022300020619     C                   IF        PiInt=*on
022400020619     C                   Z-ADD     PiVal         VABCTR
022500040506     C                   ENDIF
022600020619     C                   ENDIF
022700020619     C                   ENDIF
022800000801     C*
022900000801     C                   ENDSR
023000000801     C*----------------------------------------------------*
023100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023200000801     C*----------------------------------------------------*
023300040823     C     IMPVAB        BEGSR
023400010305     C*
023500000801     C                   Z-ADD     *zeros        errore            1 0
023600120813     C*
023700120813     C* Considero il record corrrente unicamente se trattasi d record testata
023800120813     C                   if        %subst(vindta:49:1) = 'A'
023900120813     C*
024000000830     C                   MOVEL     datcor        VABAAS
024100020305     C                   MOVEL     datcor        VATAAS
024200040526     C                   MOVE      datcor        VABMGS
024300040823     C                   MOVE(P)   vlrpoi        VABFGS
024400040823     C                   MOVE(P)   vlrpoi        VATFGS
024500080409     C*
024600080409     C* Reperimento campi ALFA
024700080409     C*
024800080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
024900090213     C                   EVAL      VABRSD=%trim(%subst(vindta:668:35))
025000080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025100080409     C     '@':'A'       XLATE     VABRSD        VABRSD
025200080409     C* ==
025300090213     C                   EVAL      VABRD2=%trim(%subst(vindta:668+35:50-35))
025400090213     C                   EVAL      VABIND=%trim(%subst(vindta:550:30))
025500090213     C                   EVAL      VABLOD=%trim(%subst(vindta:718:30))
025600090213     C                   EVAL      VABPRD=%trim(%subst(vindta:786:2))
025700090213     C                   EVAL      VABRMA=%trim(%subst(vindta:118:24))
025800101007     C***                EVAL      VABTSP=%trim(%subst(vindta:78:1))
025900090213     C                   EVAL      VABNOT=%trim(%subst(vindta:142:35))
026000090213     C                   EVAL      VABNT2=%trim(%subst(vindta:142+35:60-35))
026100120823     C                   IF        %subst(vindta:649:2) = 'IT'
026200090213     C                   EVAL      VABNZD=*blanks
026300090213     C                   ELSE
026400090213     C                   EVAL      VABNZD=%trim(%subst(vindta:649:3))
026500090213     C                   ENDIF
026600090213     C                   IF        %subst(vindta:202:1) = 'R'
026700090213     C                   EVAL      VABCBO='2'
026800090213     C                   ELSE
026900090213     C                   EVAL      VABCBO='1'
027000090213     C                   ENDIF
027100080409     C*
027200080409     C* Reperimento campi NUMERICI
027300090113     C*
027400120905     C* NSP
027500150302     C***                EVAL      PiStr=%trim(%subst(vindta:118:10))
027600150302     C***                EVAL      PiStr=%trim(%subst(vindta:34+2:15-2))
027700150302     C                   EVAL      PiStr=%trim(%subst(vindta:25:9))
027800080409     C                   EXSR      CHKNUM
027900080409     C                   IF        PiInt=*on
028000080409     C                   Z-ADD     PiVal         VABNSP
028100080409     C                   Z-ADD     PiVal         VATNSP
028200150302     C                   Z-ADD     PiVal         VABRMN
028300080409     C                   ELSE
028400080409     C                   ADD       1             errore
028500080409     C                   Z-ADD     *zeros        VABNSP
028600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
028700150302     C                             + ' ' + 'VABNSP VATNSP VABRMN'
028800080409     C                   ENDIF
028900120905     C*
029000120905     C* RMN
029100150302     C***
029200150302     C**se i caratteri che si dovrebbero prendere sono più di 5
029300150302     C***                IF        %len(%trim(%subst(vindta:122:11))) > 5
029400150302     C**considero gli ultimi 5 che dovrebbero essere i soli sicuramente numerici
029500150302     C***                EVAL      PiStr=%subst(
029600150302     C***                                %trim(%subst(vindta:122:11)) :
029700150302     C***                           %len(%trim(%subst(vindta:122:11))) - 4 :
029800150302     C***                                5)
029900150302     C***                ELSE
030000150302     C*altrimenti prendo tutto
030100150302     C***                EVAL      PiStr=%trim(%subst(vindta:122:11))
030200150302     C***                ENDIF
030300150302     C***                EVAL      PiStr=%trim(%subst(vindta:123:11))
030400150302     C***                EXSR      CHKNUM
030500150302     C***                IF        PiInt=*on
030600150302     C***                Z-ADD     PiVal         VABRMN
030700150302     C***                ELSE
030800150302     C***                ADD       1             errore
030900150302     C***                Z-ADD     1             VABRMN
031000150302     C***                EVAL      vinmsg = %trimr(vinmsg)
031100150302     C***                          + ' ' + 'VABRMN'
031200150302     C***                ENDIF
031300080409     C* CAD
031400090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
031500080409     C                   EXSR      CHKNUM
031600080409     C                   IF        PiInt=*on
031700080409     C                   Z-ADD     PiVal         Num5_0
031800080409     C                   MOVEL(p)  Num5_0        VABCAD
031900080409     C                   ELSE
032000080409     C                   ADD       1             errore
032100080409     C                   EVAL      VABCAD = *zeros
032200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032300080409     C                             + ' ' + 'VABCAD'
032400080409     C                   ENDIF
032500080409     C* NCL
032600090213     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
032700080409     C                   EXSR      CHKNUM
032800080409     C                   IF        PiInt=*on
032900080409     C                   Z-ADD     PiVal         VABNCL
033000080409     C                   ELSE
033100080409     C                   ADD       1             errore
033200080409     C                   Z-ADD     *zeros        VABNCL
033300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
033400080409     C                             + ' ' + 'VABNCL'
033500080409     C                   ENDIF
033600080409     C* PKB
033700090213     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
033800080409     C                   EXSR      CHKNUM
033900080409     C                   IF        PiNum=*on
034000090213     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
034100090113     C                   Z-ADD     PiVal         VABPKB
034200080409     C                   ELSE
034300080409     C                   ADD       1             errore
034400080409     C                   Z-ADD     *zeros        VABPKB
034500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
034600080409     C                             + ' ' + 'VABPKB'
034700080409     C                   ENDIF
034800121005     C* VLB
034900121005     C                   EVAL      PiStr=%trim(%subst(vindta:63:7))
035000121005     C                   EXSR      CHKNUM
035100121005     C                   IF        PiNum=*on
035200121005     C                   EVAL      PiVal = PiVal / 1000                         3 dec. senza virgola
035300121005     C                   Z-ADD     PiVal         VABVLB
035400121005     C                   ELSE
035500121005     C                   ADD       1             errore
035600121005     C                   Z-ADD     *zeros        VABVLB
035700121005     C                   EVAL      vinmsg = %trimr(vinmsg)
035800121005     C                             + ' ' + 'VABVLB'
035900121005     C                   ENDIF
036000090213     C* IAS
036100101007     C                   IF        %trim(%subst(vindta:82:13)) <> *blanks AND
036200101007     C                             %trim(%subst(vindta:82:13)) <> *zeros
036300101007     C                   EVAL      VABVAS = %trim(%subst(vindta:95:3))
036400090213     C                   EVAL      PiStr=%trim(%subst(vindta:82:13))
036500080409     C                   EXSR      CHKNUM
036600080409     C                   IF        PiNum=*on
036700090213     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
036800090213     C                   Z-ADD     PiVal         VABIAS
036900080409     C                   ELSE
037000080409     C                   ADD       1             errore
037100090213     C                   Z-ADD     *zeros        VABIAS
037200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
037300090213     C                             + ' ' + 'VABIAS'
037400080409     C                   ENDIF
037500080409     C                   ENDIF
037600101007     C* CAS
037700101007     C                   IF        %trim(%subst(vindta:1523:13)) <> *blanks AND
037800101007     C                             %trim(%subst(vindta:1523:13)) <> *zeros
037900101007     C                   MOVEL     '1'           FlgCAS
038000101007     C                   EVAL      VABVCA = 'EUR'
038100101007     C                   EVAL      PiStr=%trim(%subst(vindta:1523:13))
038200101007     C                   EXSR      CHKNUM
038300101007     C                   IF        PiNum=*on
038400101007     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
038500101007     C                   Z-ADD     PiVal         VABCAS
038600101007     C                   ELSE
038700101007     C                   ADD       1             errore
038800101007     C                   Z-ADD     *zeros        VABCAS
038900101007     C                   EVAL      vinmsg = %trimr(vinmsg)
039000101007     C                             + ' ' + 'VABCAS'
039100101007     C                   ENDIF
039200101007     C                   ENDIF
039300071121     C*
039400080415     C* Carico l'estensioni A e B del FNVAT
039500080415     C                   exsr      exevata
039600071121     C                   exsr      exevatb
039700010205     C*
039800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039900040802     C                   IF        FlgCAS <> '0'
040000010205     C                   EVAL      VABCBO = '4'
040100010205     C                   ENDIF
040200020305     C*
040300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040400011113     C                   EXSR      CHKIMPDIV
040500120813     C*
040600120813     C                   endif
040700010202     C*
040800000801     C* Ebbene...
040900000801     C                   ADD       1             §CTRMO
041000010201     C                   IF        errore <> *zeros
041100000801     C                   ADD       1             §CTRNO
041200000801     C                   EVAL      vinflg = '2'
041300000801     C                   ELSE
041400010201     C                   ADD       1             §CTROKVB
041500000801     C                   ENDIF
041600000801     C*
041700000801     C                   ENDSR
041800070102     C*----------------------------------------------------*
041900070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
042000070102     C*----------------------------------------------------*
042100070103     C     EXEVATE       BEGSR
042200120813     C*
042300120813     C* Considero il record corrrente unicamente se trattasi d record dettaglio
042400120813     C                   if        %subst(vindta:49:1) = 'B'
042500101007     C*
042600120802     C                   EVAL      VATTRC='E'
042700120813     C                   EVAL      VATNOT=%trim(%subst(vindta:56:24))
042800101007     C*
042900120802     C                   exsr      wrivat
043000120813     C*
043100120813     C                   endif
043200070102     C*
043300070102     C                   ENDSR
043400080415     C*----------------------------------------------------*
043500080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
043600080415     C*----------------------------------------------------*
043700080415     C     EXEVATA       BEGSR
043800080415     C*
043900120802     C                   EVAL      VATTRC='A'
044000120802     C                   EVAL      VATNOT=%trim(%subst(vindta:748:22))
044100080415     C*
044200120802     C                   exsr      wrivat                                       => scarico VAT
044300080415     C*
044400080415     C                   ENDSR
044500071121     C*----------------------------------------------------*
044600071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
044700071121     C*----------------------------------------------------*
044800071121     C     EXEVATB       BEGSR
044900071121     C*
045000090213     C                   EVAL      VATTRC='B'
045100120802     C                   EVAL      VATNOT=%trim(%subst(vindta:652:7+9))
045200071121     C*
045300090213     C                   exsr      wrivat                                       => scarico VAT
045400071121     C*
045500071121     C                   ENDSR
045600010201     C*----------------------------------------------------*
045700040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
045800010201     C*----------------------------------------------------*
045900020305     C     WRIVAT        BEGSR
046000050628     C*
046100060223     C* Scrivo solo se valorizzato qualcosa
046200060223     C                   IF        VATNOT <> *blanks
046300040802     C                   WRITE     FIVAT000
046400060223     C                   ENDIF
046500010201     C*
046600010201     C                   ENDSR
046700010202     C*----------------------------------------------------*
046800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046900010202     C*----------------------------------------------------*
047000020305     C     PREVAT        BEGSR
047100010202     C*
047200021113     C* Compongo il nome del membro da dare al FIVATWWR
047300010202     C                   eval      parmbr = vlrhdl
047400010202     C                   movel     'M'           parmbr
047500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
047600010202     C                   eval      paropz = '1'
047700010202     C* Effettuo la chiamata al CLLE preposto
047800040506     C                   call(e)   'TITVVTC'
047900010202     C                   parm                    parccm
048000010202     C                   parm                    parmbr
048100010202     C                   parm                    paropz
048200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048300010202     C                   if        %error
048400010202     C                   movel     '1'           chkcall
048500010202     C                   else
048600010202     C                   movel     '0'           chkcall
048700010202     C                   endif
048800010202     C*
048900010202     C                   ENDSR
049000000801     C*----------------------------------------------------*
049100000801     C*  CONTROLLO NUMERICITA' CAMPI
049200000801     C*----------------------------------------------------*
049300000801     C     CHKNUM        BEGSR
049400090113     C*
049500090113     C                   IF        PiDecChr = *blanks
049600090113     C                   EVAL      PiDecChr = ','
049700090113     C                   ENDIF
049800090113     C*
049900090113     C                   callp(e)  UBISNUM_Check(PiStr
050000090113     C                                          :PiDecChr
050100090113     C                                          :PiVal
050200090113     C                                          :PiNum
050300090113     C                                          :PiInt)
050400090113     C*
050500090113     C                   IF        %error
050600090113     C                   EVAL      PiInt=*off
050700090113     C                   ENDIF
050800000801     C*
050900000801     C                   ENDSR
051000000801     C***
051100000801
051200011113
051300011113     C*----------------------------------------------------*
051400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051500011113     C*----------------------------------------------------*
051600011113     C     CHKIMPDIV     BEGSR
051700011113     C*
051800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051900011113     C                   Z-ADD     *zeros        wrkDec            9 9
052000011113     C*
052100011113     C* Come prima cosa effettuo considerazioni sulla divisa
052200011113     C                   IF        vabIAS > *zeros
052300011113     C                   IF        vabVAS <> 'EUR'
052400011113     C                   EVAL      vabVAS =  'ITL'
052500011113     C                   ENDIF
052600011113     C                   ENDIF
052700011113     C*
052800011113     C                   IF        vabCAS > *zeros
052900011113     C                   IF        vabVCA <> 'EUR'
053000011113     C                   EVAL      vabVCA =  'ITL'
053100011113     C                   ENDIF
053200011113     C                   ENDIF
053300011113     C*
053400011113     C                   IF        vabVMD > *zeros
053500020305     C                   IF        vabVAD <> 'EUR'
053600011113     C                   EVAL      vabVAD =  'ITL'
053700011113     C                   ENDIF
053800011113     C                   ENDIF
053900011113     C*
054000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054100011113     C                   Z-ADD     vabIAS        wrkDec
054200011113     C                   IF        wrkDec > *zeros
054300011113     C                   IF        vabVAS = 'ITL'
054400011113     C                   EVAL      vabIAS = *zeros
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054900011113     C                   Z-ADD     vabCAS        wrkDec
055000011113     C                   IF        wrkDec > *zeros
055100011113     C                   IF        vabVCA = 'ITL'
055200011113     C                   EVAL      vabCAS = *zeros
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055700011113     C                   Z-ADD     vabVMD        wrkDec
055800011113     C                   IF        wrkDec > *zeros
055900011113     C                   IF        vabVAD = 'ITL'
056000011113     C                   EVAL      vabVMD = *zeros
056100011113     C                   ENDIF
056200011113     C                   ENDIF
056300011113     C*
056400011113     C                   ENDSR
056500011113     C***
056600011113
056700011113
056800000801
056900000801
057000990920      /TITLE Invio dei dati al punto operativo.
057100010202     C     invio         BEGSR
057200990920     C*
057300021113     C* 1° invio FIVAT
057400010201     C                   reset                   dscmz
057500010201     C                   move      vlrpoi        cmzdst
057600021113     C                   eval      cmzfld = 'FIVATWWR'
057700010201     C                   eval      cmzmbd = vlrhdl
057800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057900021009     C***                if        prmfir = *blanks
058000021113     C                   eval      cmzfla = 'FIVAT00F'
058100021113     C                   eval      cmzmba = 'FIVAT00F'
058200021009     C***                else
058300021009     C***                eval      cmzfla = prmfir
058400021009     C***                eval      cmzmba = prmfir
058500021009     C***                endif
058600010201     C                   eval      cmznrr = *zeros
058700020305     C                   move      §ctrokvt      cmznrr
058800021018     C                   eval      cmzlba = vlrfl1
058900010201     C                   call(e)   'TIS711C'
059000010201     C                   parm                    dscmz
059100010201     C                   parm      *blanks       esito
059200010205     C                   if        %error
059300010205     C                             or cmzerr = '1'
059400010205     C                             or esito  = '1'
059500010205     C                   eval      wrkesito = '3'
059600010205     C                   else
059700010201     C*
059800021113     C* 2° invio FIVAB
059900010201     C                   reset                   dscmz
060000010201     C                   move      vlrpoi        cmzdst
060100010201     C                   eval      cmzfld = vlrfou
060200010201     C                   eval      cmzmbd = vlrhdl
060300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060400021009     C***                if        prmfir = *blanks
060500021113     C                   eval      cmzfla = 'FIVAB00F'
060600021113     C                   eval      cmzmba = 'FIVAB00F'
060700021009     C***                else
060800021009     C***                eval      cmzfla = prmfir
060900021009     C***                eval      cmzmba = prmfir
061000021009     C***                endif
061100010201     C                   eval      cmznrr = *zeros
061200010201     C                   move      §ctrokvb      cmznrr
061300021018     C                   eval      cmzlba = vlrfl1
061400010201     C                   call(e)   'TIS711C'
061500010201     C                   parm                    dscmz
061600010201     C                   parm      *blanks       esito
061700010201     C                   if        %error
061800010201     C                             or cmzerr = '1'
061900010201     C                             or esito  = '1'
062000010201     C                   eval      wrkesito = '3'
062100010201     C                   endif
062200010205     C                   endif
062300990920     C*
062400000613     C                   ENDSR
062500000613     C***
062600070411
062700070411     C     *pssr         BEGSR
062800070411     C*
062900070411     C                   if        %open(tivin00r)
063000070411     C                   close     tivin00r
063100070411     C                   endif
063200070411     C                   if        %open(fivabwwr)
063300070411     C                   close     fivabwwr
063400070411     C                   endif
063500070411     C                   if        %open(fivatwwr)
063600070411     C                   close     fivatwwr
063700070411     C                   endif
063800070411     C*
063900070411     C* Effettuo la chiamata al CLLE preposto
064000070411     C                   call(e)   'TITVVTC'
064100070411     C                   parm                    parccm
064200070411     C                   parm                    parmbr
064300070411     C                   parm      '2'           paropz
064400070411     C*
064500070411     C                   eval      wrkesito = '2'
064600070411     C*
064700070411     C                   seton                                        LR
064800070411     C*
064900070411     C                   ENDSR     '*CANCL'
065000070411     C***
065100070411
065200990910
065300000613     C     *inzsr        BEGSR
065400990910     C*
065500990910     C     *entry        plist
065600990920     C                   parm                    tivlrds
065700990921     C                   parm      wrkesito      esito
065800000724     C                   parm                    prmlit
065900000710     C                   parm                    prmfir
066000000613     C*
066100000830     C* CALCOLA LA DATA CORRENTE
066200120813     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
066300120813     C                   eval      datcor = %dec(%date() : *ISO)
066400000830     C*
066500000613     C                   ENDSR
066600000613     C***
