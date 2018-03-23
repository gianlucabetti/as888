000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002500120813     D curSped         s             15    INZ(*blanks)
002600120813     D depSped         s             15    INZ(*blanks)
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
009900120813     C                   eval      curSped = %subst(vindta:34:15)
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
019500120802     C                   Z-ADD     0065613       VABCCM
019600120802     C                   Z-ADD     0065613       VATCCM
019700120802     C                   Z-ADD     214           VABLNP
019800120802     C                   Z-ADD     214           VATLNP
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
026100120823     C* se nelle note, ovvero da posizione 142 a 201 e' contenuta la dicitura "Prenotare consegna"
026200120823     C* valorizza il campo VABTC1 ad 'A'
026300120823     C                   IF        %scan('Prenotare consegna' :
026400120823     C                              %trim(%subst(vindta:142:60)))
026500120823     C                             > 0
026600120823     C                   EVAL      VABTC1='A'
026700120823     C                   ENDIF
026800120823     C                   IF        %subst(vindta:649:2) = 'IT'
026900090213     C                   EVAL      VABNZD=*blanks
027000090213     C                   ELSE
027100090213     C                   EVAL      VABNZD=%trim(%subst(vindta:649:3))
027200090213     C                   ENDIF
027300090213     C                   IF        %subst(vindta:202:1) = 'R'
027400090213     C                   EVAL      VABCBO='2'
027500090213     C                   ELSE
027600090213     C                   EVAL      VABCBO='1'
027700090213     C                   ENDIF
027800080409     C*
027900080409     C* Reperimento campi NUMERICI
028000090113     C*
028100120905     C* NSP
028200120802     C***                EVAL      PiStr=%trim(%subst(vindta:118:10))
028300120802     C                   EVAL      PiStr=%trim(%subst(vindta:34+2:15-2))
028400080409     C                   EXSR      CHKNUM
028500080409     C                   IF        PiInt=*on
028600080409     C                   Z-ADD     PiVal         VABNSP
028700080409     C                   Z-ADD     PiVal         VATNSP
028800080409     C                   ELSE
028900080409     C                   ADD       1             errore
029000080409     C                   Z-ADD     *zeros        VABNSP
029100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029200120905     C                             + ' ' + 'VABNSP VATNSP'
029300080409     C                   ENDIF
029400120905     C*
029500120905     C* RMN
029600120907     C* se i caratteri che si dovrebbero prendere sono più di 5
029700120907     C                   IF        %len(%trim(%subst(vindta:122:11))) > 5
029800120907     C* considero gli ultimi 5 che dovrebbero essere i soli sicuramente numerici
029900120907     C                   EVAL      PiStr=%subst(
030000120907     C                                   %trim(%subst(vindta:122:11)) :
030100120907     C                              %len(%trim(%subst(vindta:122:11))) - 4 :
030200120907     C                                   5)
030300120907     C                   ELSE
030400120907     C*altrimenti prendo tutto
030500120907     C                   EVAL      PiStr=%trim(%subst(vindta:122:11))
030600120907     C                   ENDIF
030700120907     C***                EVAL      PiStr=%trim(%subst(vindta:123:11))
030800120905     C                   EXSR      CHKNUM
030900120905     C                   IF        PiInt=*on
031000120905     C                   Z-ADD     PiVal         VABRMN
031100120905     C                   ELSE
031200120905     C                   ADD       1             errore
031300120905     C                   Z-ADD     1             VABRMN
031400120905     C                   EVAL      vinmsg = %trimr(vinmsg)
031500120905     C                             + ' ' + 'VABRMN'
031600120905     C                   ENDIF
031700080409     C* CAD
031800090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
031900080409     C                   EXSR      CHKNUM
032000080409     C                   IF        PiInt=*on
032100080409     C                   Z-ADD     PiVal         Num5_0
032200080409     C                   MOVEL(p)  Num5_0        VABCAD
032300080409     C                   ELSE
032400080409     C                   ADD       1             errore
032500080409     C                   EVAL      VABCAD = *zeros
032600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032700080409     C                             + ' ' + 'VABCAD'
032800080409     C                   ENDIF
032900080409     C* NCL
033000090213     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
033100080409     C                   EXSR      CHKNUM
033200080409     C                   IF        PiInt=*on
033300080409     C                   Z-ADD     PiVal         VABNCL
033400080409     C                   ELSE
033500080409     C                   ADD       1             errore
033600080409     C                   Z-ADD     *zeros        VABNCL
033700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
033800080409     C                             + ' ' + 'VABNCL'
033900080409     C                   ENDIF
034000080409     C* PKB
034100090213     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
034200080409     C                   EXSR      CHKNUM
034300080409     C                   IF        PiNum=*on
034400090213     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
034500090113     C                   Z-ADD     PiVal         VABPKB
034600080409     C                   ELSE
034700080409     C                   ADD       1             errore
034800080409     C                   Z-ADD     *zeros        VABPKB
034900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
035000080409     C                             + ' ' + 'VABPKB'
035100080409     C                   ENDIF
035200090213     C* IAS
035300101007     C                   IF        %trim(%subst(vindta:82:13)) <> *blanks AND
035400101007     C                             %trim(%subst(vindta:82:13)) <> *zeros
035500101007     C                   EVAL      VABVAS = %trim(%subst(vindta:95:3))
035600090213     C                   EVAL      PiStr=%trim(%subst(vindta:82:13))
035700080409     C                   EXSR      CHKNUM
035800080409     C                   IF        PiNum=*on
035900090213     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
036000090213     C                   Z-ADD     PiVal         VABIAS
036100080409     C                   ELSE
036200080409     C                   ADD       1             errore
036300090213     C                   Z-ADD     *zeros        VABIAS
036400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
036500090213     C                             + ' ' + 'VABIAS'
036600080409     C                   ENDIF
036700080409     C                   ENDIF
036800101007     C* CAS
036900101007     C                   IF        %trim(%subst(vindta:1523:13)) <> *blanks AND
037000101007     C                             %trim(%subst(vindta:1523:13)) <> *zeros
037100101007     C                   MOVEL     '1'           FlgCAS
037200101007     C                   EVAL      VABVCA = 'EUR'
037300101007     C                   EVAL      PiStr=%trim(%subst(vindta:1523:13))
037400101007     C                   EXSR      CHKNUM
037500101007     C                   IF        PiNum=*on
037600101007     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
037700101007     C                   Z-ADD     PiVal         VABCAS
037800101007     C                   ELSE
037900101007     C                   ADD       1             errore
038000101007     C                   Z-ADD     *zeros        VABCAS
038100101007     C                   EVAL      vinmsg = %trimr(vinmsg)
038200101007     C                             + ' ' + 'VABCAS'
038300101007     C                   ENDIF
038400101007     C                   ENDIF
038500071121     C*
038600080415     C* Carico l'estensioni A e B del FNVAT
038700080415     C                   exsr      exevata
038800071121     C                   exsr      exevatb
038900010205     C*
039000010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039100040802     C                   IF        FlgCAS <> '0'
039200010205     C                   EVAL      VABCBO = '4'
039300010205     C                   ENDIF
039400020305     C*
039500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039600011113     C                   EXSR      CHKIMPDIV
039700120813     C*
039800120813     C                   endif
039900010202     C*
040000000801     C* Ebbene...
040100000801     C                   ADD       1             §CTRMO
040200010201     C                   IF        errore <> *zeros
040300000801     C                   ADD       1             §CTRNO
040400000801     C                   EVAL      vinflg = '2'
040500000801     C                   ELSE
040600010201     C                   ADD       1             §CTROKVB
040700000801     C                   ENDIF
040800000801     C*
040900000801     C                   ENDSR
041000070102     C*----------------------------------------------------*
041100070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
041200070102     C*----------------------------------------------------*
041300070103     C     EXEVATE       BEGSR
041400120813     C*
041500120813     C* Considero il record corrrente unicamente se trattasi d record dettaglio
041600120813     C                   if        %subst(vindta:49:1) = 'B'
041700101007     C*
041800120802     C                   EVAL      VATTRC='E'
041900120813     C                   EVAL      VATNOT=%trim(%subst(vindta:56:24))
042000101007     C*
042100120802     C                   exsr      wrivat
042200120813     C*
042300120813     C                   endif
042400070102     C*
042500070102     C                   ENDSR
042600080415     C*----------------------------------------------------*
042700080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
042800080415     C*----------------------------------------------------*
042900080415     C     EXEVATA       BEGSR
043000080415     C*
043100120802     C                   EVAL      VATTRC='A'
043200120802     C                   EVAL      VATNOT=%trim(%subst(vindta:748:22))
043300080415     C*
043400120802     C                   exsr      wrivat                                       => scarico VAT
043500080415     C*
043600080415     C                   ENDSR
043700071121     C*----------------------------------------------------*
043800071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
043900071121     C*----------------------------------------------------*
044000071121     C     EXEVATB       BEGSR
044100071121     C*
044200090213     C                   EVAL      VATTRC='B'
044300120802     C                   EVAL      VATNOT=%trim(%subst(vindta:652:7+9))
044400071121     C*
044500090213     C                   exsr      wrivat                                       => scarico VAT
044600071121     C*
044700071121     C                   ENDSR
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
048600090113     C*
048700090113     C                   IF        PiDecChr = *blanks
048800090113     C                   EVAL      PiDecChr = ','
048900090113     C                   ENDIF
049000090113     C*
049100090113     C                   callp(e)  UBISNUM_Check(PiStr
049200090113     C                                          :PiDecChr
049300090113     C                                          :PiVal
049400090113     C                                          :PiNum
049500090113     C                                          :PiInt)
049600090113     C*
049700090113     C                   IF        %error
049800090113     C                   EVAL      PiInt=*off
049900090113     C                   ENDIF
050000000801     C*
050100000801     C                   ENDSR
050200000801     C***
050300000801
050400011113
050500011113     C*----------------------------------------------------*
050600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050700011113     C*----------------------------------------------------*
050800011113     C     CHKIMPDIV     BEGSR
050900011113     C*
051000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051100011113     C                   Z-ADD     *zeros        wrkDec            9 9
051200011113     C*
051300011113     C* Come prima cosa effettuo considerazioni sulla divisa
051400011113     C                   IF        vabIAS > *zeros
051500011113     C                   IF        vabVAS <> 'EUR'
051600011113     C                   EVAL      vabVAS =  'ITL'
051700011113     C                   ENDIF
051800011113     C                   ENDIF
051900011113     C*
052000011113     C                   IF        vabCAS > *zeros
052100011113     C                   IF        vabVCA <> 'EUR'
052200011113     C                   EVAL      vabVCA =  'ITL'
052300011113     C                   ENDIF
052400011113     C                   ENDIF
052500011113     C*
052600011113     C                   IF        vabVMD > *zeros
052700020305     C                   IF        vabVAD <> 'EUR'
052800011113     C                   EVAL      vabVAD =  'ITL'
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053300011113     C                   Z-ADD     vabIAS        wrkDec
053400011113     C                   IF        wrkDec > *zeros
053500011113     C                   IF        vabVAS = 'ITL'
053600011113     C                   EVAL      vabIAS = *zeros
053700011113     C                   ENDIF
053800011113     C                   ENDIF
053900011113     C*
054000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054100011113     C                   Z-ADD     vabCAS        wrkDec
054200011113     C                   IF        wrkDec > *zeros
054300011113     C                   IF        vabVCA = 'ITL'
054400011113     C                   EVAL      vabCAS = *zeros
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054900011113     C                   Z-ADD     vabVMD        wrkDec
055000011113     C                   IF        wrkDec > *zeros
055100011113     C                   IF        vabVAD = 'ITL'
055200011113     C                   EVAL      vabVMD = *zeros
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C                   ENDSR
055700011113     C***
055800011113
055900011113
056000000801
056100000801
056200990920      /TITLE Invio dei dati al punto operativo.
056300010202     C     invio         BEGSR
056400990920     C*
056500021113     C* 1° invio FIVAT
056600010201     C                   reset                   dscmz
056700010201     C                   move      vlrpoi        cmzdst
056800021113     C                   eval      cmzfld = 'FIVATWWR'
056900010201     C                   eval      cmzmbd = vlrhdl
057000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057100021009     C***                if        prmfir = *blanks
057200021113     C                   eval      cmzfla = 'FIVAT00F'
057300021113     C                   eval      cmzmba = 'FIVAT00F'
057400021009     C***                else
057500021009     C***                eval      cmzfla = prmfir
057600021009     C***                eval      cmzmba = prmfir
057700021009     C***                endif
057800010201     C                   eval      cmznrr = *zeros
057900020305     C                   move      §ctrokvt      cmznrr
058000021018     C                   eval      cmzlba = vlrfl1
058100010201     C                   call(e)   'TIS711C'
058200010201     C                   parm                    dscmz
058300010201     C                   parm      *blanks       esito
058400010205     C                   if        %error
058500010205     C                             or cmzerr = '1'
058600010205     C                             or esito  = '1'
058700010205     C                   eval      wrkesito = '3'
058800010205     C                   else
058900010201     C*
059000021113     C* 2° invio FIVAB
059100010201     C                   reset                   dscmz
059200010201     C                   move      vlrpoi        cmzdst
059300010201     C                   eval      cmzfld = vlrfou
059400010201     C                   eval      cmzmbd = vlrhdl
059500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059600021009     C***                if        prmfir = *blanks
059700021113     C                   eval      cmzfla = 'FIVAB00F'
059800021113     C                   eval      cmzmba = 'FIVAB00F'
059900021009     C***                else
060000021009     C***                eval      cmzfla = prmfir
060100021009     C***                eval      cmzmba = prmfir
060200021009     C***                endif
060300010201     C                   eval      cmznrr = *zeros
060400010201     C                   move      §ctrokvb      cmznrr
060500021018     C                   eval      cmzlba = vlrfl1
060600010201     C                   call(e)   'TIS711C'
060700010201     C                   parm                    dscmz
060800010201     C                   parm      *blanks       esito
060900010201     C                   if        %error
061000010201     C                             or cmzerr = '1'
061100010201     C                             or esito  = '1'
061200010201     C                   eval      wrkesito = '3'
061300010201     C                   endif
061400010205     C                   endif
061500990920     C*
061600000613     C                   ENDSR
061700000613     C***
061800070411
061900070411     C     *pssr         BEGSR
062000070411     C*
062100070411     C                   if        %open(tivin00r)
062200070411     C                   close     tivin00r
062300070411     C                   endif
062400070411     C                   if        %open(fivabwwr)
062500070411     C                   close     fivabwwr
062600070411     C                   endif
062700070411     C                   if        %open(fivatwwr)
062800070411     C                   close     fivatwwr
062900070411     C                   endif
063000070411     C*
063100070411     C* Effettuo la chiamata al CLLE preposto
063200070411     C                   call(e)   'TITVVTC'
063300070411     C                   parm                    parccm
063400070411     C                   parm                    parmbr
063500070411     C                   parm      '2'           paropz
063600070411     C*
063700070411     C                   eval      wrkesito = '2'
063800070411     C*
063900070411     C                   seton                                        LR
064000070411     C*
064100070411     C                   ENDSR     '*CANCL'
064200070411     C***
064300070411
064400990910
064500000613     C     *inzsr        BEGSR
064600990910     C*
064700990910     C     *entry        plist
064800990920     C                   parm                    tivlrds
064900990921     C                   parm      wrkesito      esito
065000000724     C                   parm                    prmlit
065100000710     C                   parm                    prmfir
065200000613     C*
065300000830     C* CALCOLA LA DATA CORRENTE
065400120813     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
065500120813     C                   eval      datcor = %dec(%date() : *ISO)
065600000830     C*
065700000613     C                   ENDSR
065800000613     C***
