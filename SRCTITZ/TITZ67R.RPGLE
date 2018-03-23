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
019500130213     C                   Z-ADD     0641039       VABCCM
019600130213     C                   Z-ADD     0641039       VATCCM
019700130213     C                   Z-ADD     064           VABLNP
019800130213     C                   Z-ADD     064           VATLNP
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
026100080409     C*
026200080409     C* Reperimento campi NUMERICI
026300090113     C*
026400120905     C* NSP
026500130213     C                   EVAL      PiStr=%trim(%subst(vindta:34:15))
026600080409     C                   EXSR      CHKNUM
026700080409     C                   IF        PiInt=*on
026800080409     C                   Z-ADD     PiVal         VABNSP
026900080409     C                   Z-ADD     PiVal         VATNSP
027000130213     C                   Z-ADD     PiVal         VABRMN
027100080409     C                   ELSE
027200080409     C                   ADD       1             errore
027300080409     C                   Z-ADD     *zeros        VABNSP
027400130213     C                   Z-ADD     *zeros        VABRMN
027500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
027600130213     C                             + ' ' + 'VABNSP VATNSP VABRMN'
027700080409     C                   ENDIF
027800120905     C*
027900120905     C* RMN
028000130213     C***                EVAL      PiStr=%trim(%subst(vindta:122:11))
028100130213     C***                EXSR      CHKNUM
028200130213     C***                IF        PiInt=*on
028300130213     C***                Z-ADD     PiVal         VABRMN
028400130213     C***                ELSE
028500130213     C***                ADD       1             errore
028600130213     C***                Z-ADD     1             VABRMN
028700130213     C***                EVAL      vinmsg = %trimr(vinmsg)
028800130213     C***                          + ' ' + 'VABRMN'
028900130213     C***                ENDIF
029000080409     C* CAD
029100090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
029200080409     C                   EXSR      CHKNUM
029300080409     C                   IF        PiInt=*on
029400080409     C                   Z-ADD     PiVal         Num5_0
029500080409     C                   MOVEL(p)  Num5_0        VABCAD
029600080409     C                   ELSE
029700080409     C                   ADD       1             errore
029800080409     C                   EVAL      VABCAD = *zeros
029900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
030000080409     C                             + ' ' + 'VABCAD'
030100080409     C                   ENDIF
030200080409     C* NCL
030300090213     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
030400080409     C                   EXSR      CHKNUM
030500080409     C                   IF        PiInt=*on
030600080409     C                   Z-ADD     PiVal         VABNCL
030700080409     C                   ELSE
030800080409     C                   ADD       1             errore
030900080409     C                   Z-ADD     *zeros        VABNCL
031000080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031100080409     C                             + ' ' + 'VABNCL'
031200080409     C                   ENDIF
031300080409     C* PKB
031400090213     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
031500080409     C                   EXSR      CHKNUM
031600080409     C                   IF        PiNum=*on
031700130213     C                   EVAL      PiVal = PiVal / 1000                         3 dec. senza virgola
031800090113     C                   Z-ADD     PiVal         VABPKB
031900080409     C                   ELSE
032000080409     C                   ADD       1             errore
032100080409     C                   Z-ADD     *zeros        VABPKB
032200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032300080409     C                             + ' ' + 'VABPKB'
032400080409     C                   ENDIF
032500121005     C* VLB
032600130213     C***                EVAL      PiStr=%trim(%subst(vindta:63:7))
032700130213     C***                EXSR      CHKNUM
032800130213     C***                IF        PiNum=*on
032900130213     C***                EVAL      PiVal = PiVal / 1000                         3 dec. senza virgola
033000130213     C***                Z-ADD     PiVal         VABVLB
033100130213     C***                ELSE
033200130213     C***                ADD       1             errore
033300130213     C***                Z-ADD     *zeros        VABVLB
033400130213     C***                EVAL      vinmsg = %trimr(vinmsg)
033500130213     C***                          + ' ' + 'VABVLB'
033600130213     C***                ENDIF
033700090213     C* IAS
033800130213     C***                IF        %trim(%subst(vindta:82:13)) <> *blanks AND
033900130213     C***                          %trim(%subst(vindta:82:13)) <> *zeros
034000130213     C***                EVAL      VABVAS = %trim(%subst(vindta:95:3))
034100130213     C***                EVAL      PiStr=%trim(%subst(vindta:82:13))
034200130213     C***                EXSR      CHKNUM
034300130213     C***                IF        PiNum=*on
034400130213     C***                EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
034500130213     C***                Z-ADD     PiVal         VABIAS
034600130213     C***                ELSE
034700130213     C***                ADD       1             errore
034800130213     C***                Z-ADD     *zeros        VABIAS
034900130213     C***                EVAL      vinmsg = %trimr(vinmsg)
035000130213     C***                          + ' ' + 'VABIAS'
035100130213     C***                ENDIF
035200130213     C***                ENDIF
035300101007     C* CAS
035400101007     C                   IF        %trim(%subst(vindta:1523:13)) <> *blanks AND
035500101007     C                             %trim(%subst(vindta:1523:13)) <> *zeros
035600101007     C                   MOVEL     '1'           FlgCAS
035700130213     C                   EVAL      VABVCA=%trim(%subst(vindta:1536:3))
035800101007     C                   EVAL      PiStr=%trim(%subst(vindta:1523:13))
035900101007     C                   EXSR      CHKNUM
036000101007     C                   IF        PiNum=*on
036100101007     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
036200101007     C                   Z-ADD     PiVal         VABCAS
036300101007     C                   ELSE
036400101007     C                   ADD       1             errore
036500101007     C                   Z-ADD     *zeros        VABCAS
036600101007     C                   EVAL      vinmsg = %trimr(vinmsg)
036700101007     C                             + ' ' + 'VABCAS'
036800101007     C                   ENDIF
036900101007     C                   ENDIF
037000071121     C*
037100080415     C* Carico l'estensioni A e B del FNVAT
037200080415     C                   exsr      exevata
037300071121     C                   exsr      exevatb
037400010205     C*
037500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
037600040802     C                   IF        FlgCAS <> '0'
037700010205     C                   EVAL      VABCBO = '4'
037800010205     C                   ENDIF
037900020305     C*
038000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038100011113     C                   EXSR      CHKIMPDIV
038200120813     C*
038300120813     C                   endif
038400010202     C*
038500000801     C* Ebbene...
038600000801     C                   ADD       1             §CTRMO
038700010201     C                   IF        errore <> *zeros
038800000801     C                   ADD       1             §CTRNO
038900000801     C                   EVAL      vinflg = '2'
039000000801     C                   ELSE
039100010201     C                   ADD       1             §CTROKVB
039200000801     C                   ENDIF
039300000801     C*
039400000801     C                   ENDSR
039500070102     C*----------------------------------------------------*
039600070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
039700070102     C*----------------------------------------------------*
039800070103     C     EXEVATE       BEGSR
039900120813     C*
040000120813     C* Considero il record corrrente unicamente se trattasi d record dettaglio
040100120813     C                   if        %subst(vindta:49:1) = 'B'
040200101007     C*
040300120802     C                   EVAL      VATTRC='E'
040400120813     C                   EVAL      VATNOT=%trim(%subst(vindta:56:24))
040500101007     C*
040600120802     C                   exsr      wrivat
040700120813     C*
040800120813     C                   endif
040900070102     C*
041000070102     C                   ENDSR
041100080415     C*----------------------------------------------------*
041200080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
041300080415     C*----------------------------------------------------*
041400080415     C     EXEVATA       BEGSR
041500080415     C*
041600120802     C                   EVAL      VATTRC='A'
041700120802     C                   EVAL      VATNOT=%trim(%subst(vindta:748:22))
041800080415     C*
041900130213     C***                exsr      wrivat                                       => scarico VAT
042000080415     C*
042100080415     C                   ENDSR
042200071121     C*----------------------------------------------------*
042300071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
042400071121     C*----------------------------------------------------*
042500071121     C     EXEVATB       BEGSR
042600071121     C*
042700090213     C                   EVAL      VATTRC='B'
042800120802     C                   EVAL      VATNOT=%trim(%subst(vindta:652:7+9))
042900071121     C*
043000090213     C                   exsr      wrivat                                       => scarico VAT
043100071121     C*
043200071121     C                   ENDSR
043300010201     C*----------------------------------------------------*
043400040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043500010201     C*----------------------------------------------------*
043600020305     C     WRIVAT        BEGSR
043700050628     C*
043800060223     C* Scrivo solo se valorizzato qualcosa
043900060223     C                   IF        VATNOT <> *blanks
044000040802     C                   WRITE     FIVAT000
044100060223     C                   ENDIF
044200010201     C*
044300010201     C                   ENDSR
044400010202     C*----------------------------------------------------*
044500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044600010202     C*----------------------------------------------------*
044700020305     C     PREVAT        BEGSR
044800010202     C*
044900021113     C* Compongo il nome del membro da dare al FIVATWWR
045000010202     C                   eval      parmbr = vlrhdl
045100010202     C                   movel     'M'           parmbr
045200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
045300010202     C                   eval      paropz = '1'
045400010202     C* Effettuo la chiamata al CLLE preposto
045500040506     C                   call(e)   'TITVVTC'
045600010202     C                   parm                    parccm
045700010202     C                   parm                    parmbr
045800010202     C                   parm                    paropz
045900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046000010202     C                   if        %error
046100010202     C                   movel     '1'           chkcall
046200010202     C                   else
046300010202     C                   movel     '0'           chkcall
046400010202     C                   endif
046500010202     C*
046600010202     C                   ENDSR
046700000801     C*----------------------------------------------------*
046800000801     C*  CONTROLLO NUMERICITA' CAMPI
046900000801     C*----------------------------------------------------*
047000000801     C     CHKNUM        BEGSR
047100090113     C*
047200090113     C                   IF        PiDecChr = *blanks
047300090113     C                   EVAL      PiDecChr = ','
047400090113     C                   ENDIF
047500090113     C*
047600090113     C                   callp(e)  UBISNUM_Check(PiStr
047700090113     C                                          :PiDecChr
047800090113     C                                          :PiVal
047900090113     C                                          :PiNum
048000090113     C                                          :PiInt)
048100090113     C*
048200090113     C                   IF        %error
048300090113     C                   EVAL      PiInt=*off
048400090113     C                   ENDIF
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
060300070411
060400070411     C     *pssr         BEGSR
060500070411     C*
060600070411     C                   if        %open(tivin00r)
060700070411     C                   close     tivin00r
060800070411     C                   endif
060900070411     C                   if        %open(fivabwwr)
061000070411     C                   close     fivabwwr
061100070411     C                   endif
061200070411     C                   if        %open(fivatwwr)
061300070411     C                   close     fivatwwr
061400070411     C                   endif
061500070411     C*
061600070411     C* Effettuo la chiamata al CLLE preposto
061700070411     C                   call(e)   'TITVVTC'
061800070411     C                   parm                    parccm
061900070411     C                   parm                    parmbr
062000070411     C                   parm      '2'           paropz
062100070411     C*
062200070411     C                   eval      wrkesito = '2'
062300070411     C*
062400070411     C                   seton                                        LR
062500070411     C*
062600070411     C                   ENDSR     '*CANCL'
062700070411     C***
062800070411
062900990910
063000000613     C     *inzsr        BEGSR
063100990910     C*
063200990910     C     *entry        plist
063300990920     C                   parm                    tivlrds
063400990921     C                   parm      wrkesito      esito
063500000724     C                   parm                    prmlit
063600000710     C                   parm                    prmfir
063700000613     C*
063800000830     C* CALCOLA LA DATA CORRENTE
063900120813     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
064000120813     C                   eval      datcor = %dec(%date() : *ISO)
064100000830     C*
064200000613     C                   ENDSR
064300000613     C***
