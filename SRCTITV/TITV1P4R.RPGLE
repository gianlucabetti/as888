000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500090114     FFIVABwwr  O    E             DISK    usropn
000600090114     FFIVATwwr  O    E             DISK    usropn
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
002500090504     D curSped         s              9    INZ(*blanks)
002600090504     D depSped         s              9    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600090114
003700081217
003800081217     D*------------------
003900081217     D* LINKING A DEFINIZIONI ESTERNE
004000081217     D*------------------
004100081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004200081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004300081217
004400081217
004500010201
004600081217
004700081217     C
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100000613     C*
005200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005300000613     C*
005400010202     C* Effettuo la chiamata al CLLE preposto
005500090114     C                   call(e)   'TITVVTC'
005600010202     C                   parm                    parccm
005700010202     C                   parm                    parmbr
005800010202     C                   parm      '2'           paropz
005900050201     C*
006000050201     C* Effettuo lancio TISI95 solo x chiusura
006100050201     C                   CLEAR                   TISI95DS
006200050201     C                   EVAL      I95TLA = 'C'
006300050201     C                   CALL      'TISI95R'
006400050201     C                   PARM                    TISI95DS
006500000616     C*
006600000801     C
006700010201     C                   seton                                        LR
006800990908
006900000801
007000910830     C*--------------------------------------------------------
007100090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007200910830     C*--------------------------------------------------------
007300040526     C     RWFILE        BEGSR
007400990910     C*
007500990914     C                   if        not %open(tivin00r)
007600990908     C                   open      tivin00r
007700990914     C                   endif
007800090114     C                   if        not %open(fivabwwr)
007900090114     C                   open      fivabwwr
008000990914     C                   endif
008100070103     C*
008200090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008300020305     C                   exsr      prevat
008400010201     C*
008500010202     C                   if        chkcall = '0'
008600010202     C*
008700090114     C                   if        not %open(fivatwwr)
008800090114     C                   open      fivatwwr
008900010201     C                   endif
009000990910     C*
009100010201     C                   clear                   §CTROKVB          5 0
009200020305     C                   clear                   §CTROKVT          5 0
009300000801     C                   clear                   §CTRMO            5 0
009400000801     C                   clear                   §CTRNO            5 0
009500990910     C*
009600921023     C                   DO        *HIVAL
009700990913     C*
009800990915     C                   READ      tivin00r                               70
009900050627     C                   if        vindta > *blanks
010000000613     C                   add       1             rrnum
010100000801     C*
010200000801     C                   if        *in70 = *off
010300000801     C                             and
010400000801     C                             (vinflg = *blanks
010500000801     C                              or vinflg = '0'
010600000801     C                              or vinflg = '2')
010700000801     C*
010800000801     C                   clear                   vinmsg
010900000801     C                   eval      vinflg = '1'
011000070103     C*
011100070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011200090504     C                   eval      curSped = %subst(vindta:188:9)
011300070213     C                   if        curSped <> depSped
011400070213     C* Se prima bolla => importo bolla corrente
011500070213     C                   if        §CTRMO = *zeros
011600090309     C                   EXSR      INZVAR
011700090309     C                   EXSR      DEFCAM
011800070213     C                   exsr      impvab                                       => carico  VAB
011900070213     C                   else
012000070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
012100070213     C                   exsr      wrivab                                       => scarico VAB
012200090309     C                   EXSR      INZVAR
012300090309     C                   EXSR      DEFCAM
012400070213     C                   exsr      impvab                                       => carico  VAB
012500070213     C                   endif
012600071121     C                   exsr      exevate                                      => write VAT-E
012700070103     C* Salvo il raggruppamento spedizione cliente corrente
012800070103     C                   eval      depSped = curSped
012900070213     C*
013000070213     C* Se collo successivo x stessa bolla
013100070213     C                   else
013200070213     C                   exsr      impvab                                       => carico  VAB
013300070213     C                   exsr      exevate                                      => write VAT-E
013400070103     C                   endif
013500000905     C*
013600000905     C                   else
013700000905     C                   eval      vinflg = '1'
013800050628     C                   endif
013900000905     C                   endif
014000000905     C*
014100000905     C  N70              update    tivin000
014200000905     C*
014300991022     C  N70              ENDdo
014400070213     C*
014500070213     C* Scarico testata bolla rimasta in sospesa
014600070213     C                   exsr      wrivab                                       => scarico VAB
014700010202     C*
014800010202     C                   endif
014900990910
015000990910     C* Se non ci sono record con errori ...
015100000710     C                   if        §ctrno = 0
015200990910     C* ... restituisco esito OK.
015300990921     C                   eval      wrkesito = '0'
015400990910     C                   else
015500010201     C                   if        §ctrokvb > 0
015600990921     C                   eval      wrkesito = '1'
015700000710     C                   else
015800000710     C                   eval      wrkesito = '2'
015900990910     C                   endif
016000000710     C                   endif
016100990910     C*
016200990914     C                   if        %open(tivin00r)
016300990908     C                   close     tivin00r
016400990914     C                   endif
016500090114     C                   if        %open(fivabwwr)
016600090114     C                   close     fivabwwr
016700990914     C                   endif
016800090114     C                   if        %open(fivatwwr)
016900090114     C                   close     fivatwwr
017000010201     C                   endif
017100990910     C*
017200010201     C                   if        §ctrokvb > 0
017300000724     C                             and vlrpoi <> *zeros
017400010202     C                   exsr      invio
017500990920     C                   endif
017600990920     C*
017700910830     C                   ENDSR
017800000613     C***
017900010305
018000010305     C*----------------------------------------------------*
018100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018200010305     C*----------------------------------------------------*
018300020305     C     WRIVAB        BEGSR
018400010305     C*
018500060225     C* Quindi scarico il buffer del file d testata
018600090114     C                   write     fivab000                                     => scarico il VAB
018700010305     C*
018800010305     C                   ENDSR
018900990920
019000000801     C*----------------------------------------------------*
019100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019200000801     C*----------------------------------------------------*
019300010201     C     INZVAR        BEGSR
019400000801     C*
019500090114     C                   CLEAR                   FIVAB000
019600090114     C                   CLEAR                   FIVAT000
019700070213     C*
019800040802     C                   Z-ADD     *zeros        Num5_0            5 0
019900040802     C                   MOVEL     '0'           FlgCAS            1
020000000801     C*
020100000801     C                   ENDSR
020200000801     C*----------------------------------------------------*
020300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020400000801     C*----------------------------------------------------*
020500000801     C     DEFCAM        BEGSR
020600000801     C*
020700020619     C* Imposto i valori di default...
020800090504     C                   Z-ADD     0895102       VABCCM
020900090504     C                   Z-ADD     0895102       VATCCM
021000090428     C                   Z-ADD     089           VABLNP
021100090428     C                   Z-ADD     089           VATLNP
021200090504     C                   Z-ADD     100           VABCTR
021300070103     C                   MOVEL     '7Q'          VABCTM
021400040823     C                   MOVEL     '1'           VABCBO
021500020619     C* ... e poi verifico se sono stati passati come parametri
021600020619     C                   IF        vlrppt > *blanks
021700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021900020619     C                   EXSR      CHKNUM
022000020619     C                   IF        PiInt=*on
022100020619     C                   Z-ADD     PiVal         VABCCM
022200020619     C                   Z-ADD     PiVal         VATCCM
022300020619     C                   ENDIF
022400040506     C                   ENDIF
022500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022700020619     C                   EXSR      CHKNUM
022800020619     C                   IF        PiInt=*on
022900020619     C                   Z-ADD     PiVal         VABLNP
023000020619     C                   Z-ADD     PiVal         VATLNP
023100040506     C                   ENDIF
023200020619     C                   ENDIF
023300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023500020619     C                   EXSR      CHKNUM
023600020619     C                   IF        PiInt=*on
023700020619     C                   Z-ADD     PiVal         VABCTR
023800040506     C                   ENDIF
023900020619     C                   ENDIF
024000111115     C                   IF        %subst(vlrppt:14:2) <> *blanks
024100111115     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
024200111115     C                   ENDIF
024300020619     C                   ENDIF
024400000801     C*
024500000801     C                   ENDSR
024600000801     C*----------------------------------------------------*
024700090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024800000801     C*----------------------------------------------------*
024900040823     C     IMPVAB        BEGSR
025000010305     C*
025100000801     C                   Z-ADD     *zeros        errore            1 0
025200000830     C                   MOVEL     datcor        VABAAS
025300020305     C                   MOVEL     datcor        VATAAS
025400040526     C                   MOVE      datcor        VABMGS
025500040823     C                   MOVE(P)   vlrpoi        VABFGS
025600040823     C                   MOVE(P)   vlrpoi        VATFGS
025700080409     C*
025800080409     C* Reperimento campi ALFA
025900080409     C*
026000080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
026100090504     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
026200080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026300080409     C     '@':'A'       XLATE     VABRSD        VABRSD
026400080409     C* ==
026500090504     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
026600090504     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
026700090504     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
026800090504     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
026900090504     C                   MOVEL     *blanks       wNOTE            70
027000090504     C                   EVAL      wNOTE=%trim(%subst(vindta:92:15))+' '+
027100090504     C                                   %trim(%subst(vindta:197:80))
027200090504     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
027300090504     C                   EVAL      VABNT2=%subst(wNOTE:35+1:35)
027400100305     C* TSP
027500100305     C                   SELECT
027600100305     C                   WHEN      %subst(vindta:12:1) = 'N'
027700100305     C                   EVAL      VABTSP = 'C'
027800100305     C                   WHEN      %subst(vindta:12:1) = 'P'
027900100305     C                   EVAL      VABTSP = 'E'
028000100305     C                   WHEN      %subst(vindta:12:1) = 'D'
028100100305     C                   EVAL      VABTSP = 'H'
028200100305     C                   OTHER
028300100305     C                   EVAL      VABTSP = 'C'
028400100305     C                   ENDSL
028500090504     C*
028600090504     C                   SELECT
028700090504     C                   WHEN      %subst(vindta:390:1) = '1'
028800090504     C                   EVAL      VABCBO = '1'
028900090504     C                   WHEN      %subst(vindta:390:1) = '3'
029000090504     C                   EVAL      VABCBO = '1'
029100090504     C                   WHEN      %subst(vindta:390:1) = '2'
029200090504     C                   EVAL      VABCBO = '2'
029300090504     C                   WHEN      %subst(vindta:390:1) = 'A'
029400090504     C                   EVAL      VABCBO = '1'
029500090504     C                   EVAL      VABFFD = 'S'
029600090504     C                   WHEN      %subst(vindta:390:1) = 'C'
029700090504     C                   EVAL      VABCBO = '1'
029800090504     C                   EVAL      VABFFD = 'S'
029900090504     C                   WHEN      %subst(vindta:390:1) = 'B'
030000090504     C                   EVAL      VABCBO = '2'
030100090504     C                   EVAL      VABFFD = 'S'
030200090504     C                   OTHER
030300090504     C                   EVAL      VABCBO = '1'
030400090504     C                   ENDSL
030500080409     C*
030600080409     C* Reperimento campi NUMERICI
030700090309     C* NSP
030800090504     C                   EVAL      PiStr=%trim(%subst(vindta:188:9))
030900090309     C                   EXSR      CHKNUM
031000090309     C                   IF        PiInt=*on
031100090309     C                   Z-ADD     PiVal         VABNSP
031200090309     C                   Z-ADD     PiVal         VATNSP
031300090309     C                   ELSE
031400090309     C                   EVAL      vinmsg = %trimr(vinmsg)
031500090309     C                             + ' ' + 'VABNSP VATNSP'
031600090309     C                   ENDIF
031700090309     C* RMN
031800090504     C                   EVAL      PiStr=%trim(%subst(vindta:188:9))
031900081217     C                   EXSR      CHKNUM
032000081217     C                   IF        PiInt=*on
032100081217     C                   Z-ADD     PiVal         VABRMN
032200081217     C                   ELSE
032300081217     C                   ADD       1             errore
032400081217     C                   Z-ADD     1             VABRMN
032500081217     C                   EVAL      vinmsg = %trimr(vinmsg)
032600081217     C                             + ' ' + 'VABRMN'
032700081217     C                   ENDIF
032800080409     C* CAD
032900090504     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
033000090114     C                   EXSR      CHKNUM
033100090114     C                   IF        PiInt=*on
033200131009     C                   Z-ADD     PiVal         Num5_0
033300090114     C                   MOVEL(p)  Num5_0        VABCAD
033400090114     C                   ELSE
033500090114     C                   ADD       1             errore
033600090114     C                   EVAL      VABCAD = *zeros
033700090114     C                   EVAL      vinmsg = %trimr(vinmsg)
033800090114     C                             + ' ' + 'VABCAD'
033900090114     C                   ENDIF
034000081217     C* NCL
034100090504     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
034200081217     C                   EXSR      CHKNUM
034300081217     C                   IF        PiInt=*on
034400081217     C                   Z-ADD     PiVal         VABNCL
034500081217     C                   ELSE
034600090309     C                   ADD       1             errore
034700081217     C                   EVAL      vinmsg = %trimr(vinmsg)
034800081217     C                             + ' ' + 'VABNCL'
034900081217     C                   ENDIF
035000090312     C* PKB
035100090504     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
035200090312     C                   EXSR      CHKNUM
035300090312     C                   IF        PiNum=*on
035400090428     C                   Z-ADD     PiVal         VABPKB
035500090312     C                   ELSE
035600090312     C                   ADD       1             errore
035700090312     C                   EVAL      vinmsg = %trimr(vinmsg)
035800090312     C                             + ' ' + 'VABPKB'
035900090312     C                   ENDIF
036000090504     C* VLB
036100090504     C                   IF        %subst(vindta:308:7) <> *blanks AND
036200090504     C                             %subst(vindta:308:7) <> *zeros  AND
036300090504     C                             %subst(vindta:308:7) <> '0000.00'
036400090504     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
036500090504     C                   EXSR      CHKNUM
036600090504     C                   IF        PiNum=*on
036700090504     C                   Z-ADD     PiVal         VABVLB
036800090504     C                   ELSE
036900090504     C                   ADD       1             errore
037000090504     C                   EVAL      vinmsg = %trimr(vinmsg)
037100090504     C                             + ' ' + 'VABVLB'
037200090504     C                   ENDIF
037300090504     C                   ENDIF
037400090428     C* CAS
037500090504     C                   IF        %subst(vindta:280:12) <> *blanks AND
037600090504     C                             %subst(vindta:280:12) <> *zeros  AND
037700090504     C                             %subst(vindta:280:12) <> '000000000.00'
037800090428     C                   EVAL      FlgCAS = '1'
037900090504     C                   EVAL      VABVCA = %trim(%subst(vindta:277:3))
038000090504     C                   EVAL      VABTIC = 'BM'
038100090504     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
038200090428     C                   EXSR      CHKNUM
038300090428     C                   IF        PiNum=*on
038400090428     C                   Z-ADD     PiVal         VABCAS
038500090428     C                   ELSE
038600090428     C                   ADD       1             errore
038700090428     C                   EVAL      vinmsg = %trimr(vinmsg)
038800090428     C                             + ' ' + 'VABCAS'
038900090428     C                   ENDIF
039000090428     C                   ENDIF
039100090504     C* IAS
039200090504     C                   IF        %subst(vindta:295:12) <> *blanks AND
039300090504     C                             %subst(vindta:295:12) <> *zeros  AND
039400090504     C                             %subst(vindta:295:12) <> '000000000.00'
039500090504     C                   EVAL      FlgCAS = '1'
039600090504     C                   EVAL      VABVAS = %trim(%subst(vindta:292:3))
039700090504     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
039800090504     C                   EXSR      CHKNUM
039900090504     C                   IF        PiNum=*on
040000090504     C                   Z-ADD     PiVal         VABIAS
040100090504     C                   ELSE
040200090504     C                   ADD       1             errore
040300090504     C                   EVAL      vinmsg = %trimr(vinmsg)
040400090504     C                             + ' ' + 'VABIAS'
040500090504     C                   ENDIF
040600090504     C                   ENDIF
040700131009     C* CTR
040800131009     C                   IF        %subst(vindta:412:03) <> *blanks
040900131009     C                   EVAL      PiStr=%trim(%subst(vindta:412:03))
041000131009     C                   EXSR      CHKNUM
041100131009     C                   IF        PiInt=*on
041200131009     C                   Z-ADD     PiVal         VABCTR
041300131009     C                   ELSE
041400131009     C                   ADD       1             errore
041500131009     C                   EVAL      vinmsg = %trimr(vinmsg)
041600131009     C                             + ' ' + 'VABCTR'
041700131009     C                   ENDIF
041800131009     C                   ENDIF
041900071121     C*
042000170628     C* Carico l'estensioni A, B, I del FNVAT
042100080512     C                   exsr      exevata
042200080512     C                   exsr      exevatb
042300170628     C                   exsr      exevati
042400010205     C*
042500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042600040802     C                   IF        FlgCAS <> '0'
042700010205     C                   EVAL      VABCBO = '4'
042800010205     C                   ENDIF
042900020305     C*
043000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043100011113     C                   EXSR      CHKIMPDIV
043200010202     C*
043300000801     C* Ebbene...
043400000801     C                   ADD       1             §CTRMO
043500010201     C                   IF        errore <> *zeros
043600000801     C                   ADD       1             §CTRNO
043700000801     C                   EVAL      vinflg = '2'
043800000801     C                   ELSE
043900010201     C                   ADD       1             §CTROKVB
044000000801     C                   ENDIF
044100000801     C*
044200000801     C                   ENDSR
044300070102     C*----------------------------------------------------*
044400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044500070102     C*----------------------------------------------------*
044600070103     C     EXEVATE       BEGSR
044700070102     C*
044800090428     C                   EVAL      VATTRC = 'E'
044900090428     C*
045000090504     C***                exsr      wrivat                                       => scarico VAT
045100070102     C*
045200070102     C                   ENDSR
045300080415     C*----------------------------------------------------*
045400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
045500080415     C*----------------------------------------------------*
045600080415     C     EXEVATA       BEGSR
045700080415     C*
045800090504     C                   EVAL      VATTRC = 'A'
045900080415     C*
046000080415     C***                exsr      wrivat                                       => scarico VAT
046100080415     C*
046200080415     C                   ENDSR
046300071121     C*----------------------------------------------------*
046400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
046500071121     C*----------------------------------------------------*
046600071121     C     EXEVATB       BEGSR
046700071121     C*
046800090504     C                   EVAL      VATTRC = 'B'
046900071121     C*
047000080409     C***                exsr      wrivat                                       => scarico VAT
047100071121     C*
047200071121     C                   ENDSR
047300170628     C*----------------------------------------------------*
047400170628     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
047500170628     C*----------------------------------------------------*
047600170628     C     EXEVATI       BEGSR
047700170628     C*
047800170628     C                   EVAL      VATTRC = 'I'
047900170628     C                   EVAL      VATNOT = %trim(%subst(vindta:415:35))
048000170628     C*
048100170628     C***                exsr      wrivat                                       => scarico VAT
048200170628     C*
048300170628     C                   ENDSR
048400010201     C*----------------------------------------------------*
048500090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048600010201     C*----------------------------------------------------*
048700020305     C     WRIVAT        BEGSR
048800050628     C*
048900060223     C* Scrivo solo se valorizzato qualcosa
049000060223     C                   IF        VATNOT <> *blanks
049100090114     C                   WRITE     FIVAT000
049200060223     C                   ENDIF
049300010201     C*
049400010201     C                   ENDSR
049500010202     C*----------------------------------------------------*
049600090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049700010202     C*----------------------------------------------------*
049800020305     C     PREVAT        BEGSR
049900010202     C*
050000090114     C* Compongo il nome del membro da dare al FIVATWWR
050100010202     C                   eval      parmbr = vlrhdl
050200010202     C                   movel     'M'           parmbr
050300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050400010202     C                   eval      paropz = '1'
050500010202     C* Effettuo la chiamata al CLLE preposto
050600090114     C                   call(e)   'TITVVTC'
050700010202     C                   parm                    parccm
050800010202     C                   parm                    parmbr
050900010202     C                   parm                    paropz
051000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051100010202     C                   if        %error
051200010202     C                   movel     '1'           chkcall
051300010202     C                   else
051400010202     C                   movel     '0'           chkcall
051500010202     C                   endif
051600010202     C*
051700010202     C                   ENDSR
051800000801     C*----------------------------------------------------*
051900000801     C*  CONTROLLO NUMERICITA' CAMPI
052000000801     C*----------------------------------------------------*
052100000801     C     CHKNUM        BEGSR
052200081217     C*
052300081217     C                   IF        PiDecChr = *blanks
052400090504     C                   EVAL      PiDecChr = '.'
052500081217     C                   ENDIF
052600081217     C*
052700081217     C                   callp(e)  UBISNUM_Check(PiStr
052800081217     C                                          :PiDecChr
052900081217     C                                          :PiVal
053000081217     C                                          :PiNum
053100081217     C                                          :PiInt)
053200081217     C*
053300081217     C                   IF        %error
053400081217     C                   EVAL      PiInt=*off
053500081217     C                   ENDIF
053600000801     C*
053700000801     C                   ENDSR
053800000801     C***
053900000801
054000011113
054100011113     C*----------------------------------------------------*
054200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054300011113     C*----------------------------------------------------*
054400011113     C     CHKIMPDIV     BEGSR
054500011113     C*
054600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054700011113     C                   Z-ADD     *zeros        wrkDec            9 9
054800011113     C*
054900011113     C* Come prima cosa effettuo considerazioni sulla divisa
055000011113     C                   IF        vabIAS > *zeros
055100011113     C                   IF        vabVAS <> 'EUR'
055200011113     C                   EVAL      vabVAS =  'ITL'
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C                   IF        vabCAS > *zeros
055700011113     C                   IF        vabVCA <> 'EUR'
055800011113     C                   EVAL      vabVCA =  'ITL'
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C                   IF        vabVMD > *zeros
056300020305     C                   IF        vabVAD <> 'EUR'
056400011113     C                   EVAL      vabVAD =  'ITL'
056500011113     C                   ENDIF
056600011113     C                   ENDIF
056700011113     C*
056800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056900011113     C                   Z-ADD     vabIAS        wrkDec
057000011113     C                   IF        wrkDec > *zeros
057100011113     C                   IF        vabVAS = 'ITL'
057200011113     C                   EVAL      vabIAS = *zeros
057300011113     C                   ENDIF
057400011113     C                   ENDIF
057500011113     C*
057600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057700011113     C                   Z-ADD     vabCAS        wrkDec
057800011113     C                   IF        wrkDec > *zeros
057900011113     C                   IF        vabVCA = 'ITL'
058000011113     C                   EVAL      vabCAS = *zeros
058100011113     C                   ENDIF
058200011113     C                   ENDIF
058300011113     C*
058400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058500011113     C                   Z-ADD     vabVMD        wrkDec
058600011113     C                   IF        wrkDec > *zeros
058700011113     C                   IF        vabVAD = 'ITL'
058800011113     C                   EVAL      vabVMD = *zeros
058900011113     C                   ENDIF
059000011113     C                   ENDIF
059100011113     C*
059200011113     C                   ENDSR
059300011113     C***
059400011113
059500011113
059600000801
059700000801
059800990920      /TITLE Invio dei dati al punto operativo.
059900010202     C     invio         BEGSR
060000990920     C*
060100090114     C* 1° invio FIVAT
060200010201     C                   reset                   dscmz
060300010201     C                   move      vlrpoi        cmzdst
060400090114     C                   eval      cmzfld = 'FIVATWWR'
060500010201     C                   eval      cmzmbd = vlrhdl
060600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060700021009     C***                if        prmfir = *blanks
060800090114     C                   eval      cmzfla = 'FIVAT00F'
060900090114     C                   eval      cmzmba = 'FIVAT00F'
061000021009     C***                else
061100021009     C***                eval      cmzfla = prmfir
061200021009     C***                eval      cmzmba = prmfir
061300021009     C***                endif
061400010201     C                   eval      cmznrr = *zeros
061500020305     C                   move      §ctrokvt      cmznrr
061600021018     C                   eval      cmzlba = vlrfl1
061700010201     C                   call(e)   'TIS711C'
061800010201     C                   parm                    dscmz
061900010201     C                   parm      *blanks       esito
062000010205     C                   if        %error
062100010205     C                             or cmzerr = '1'
062200010205     C                             or esito  = '1'
062300010205     C                   eval      wrkesito = '3'
062400010205     C                   else
062500010201     C*
062600090114     C* 2° invio FIVAB
062700010201     C                   reset                   dscmz
062800010201     C                   move      vlrpoi        cmzdst
062900010201     C                   eval      cmzfld = vlrfou
063000010201     C                   eval      cmzmbd = vlrhdl
063100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063200021009     C***                if        prmfir = *blanks
063300090114     C                   eval      cmzfla = 'FIVAB00F'
063400090114     C                   eval      cmzmba = 'FIVAB00F'
063500021009     C***                else
063600021009     C***                eval      cmzfla = prmfir
063700021009     C***                eval      cmzmba = prmfir
063800021009     C***                endif
063900010201     C                   eval      cmznrr = *zeros
064000010201     C                   move      §ctrokvb      cmznrr
064100021018     C                   eval      cmzlba = vlrfl1
064200010201     C                   call(e)   'TIS711C'
064300010201     C                   parm                    dscmz
064400010201     C                   parm      *blanks       esito
064500010201     C                   if        %error
064600010201     C                             or cmzerr = '1'
064700010201     C                             or esito  = '1'
064800010201     C                   eval      wrkesito = '3'
064900010201     C                   endif
065000010205     C                   endif
065100990920     C*
065200000613     C                   ENDSR
065300000613     C***
065400070411
065500090331
065600090331
065700090331
065800070411     C     *pssr         BEGSR
065900070411     C*
066000070411     C                   if        %open(tivin00r)
066100070411     C                   close     tivin00r
066200070411     C                   endif
066300090114     C                   if        %open(fivabwwr)
066400090114     C                   close     fivabwwr
066500070411     C                   endif
066600090114     C                   if        %open(fivatwwr)
066700090114     C                   close     fivatwwr
066800070411     C                   endif
066900070411     C*
067000070411     C* Effettuo la chiamata al CLLE preposto
067100090114     C                   call(e)   'TITVVTC'
067200070411     C                   parm                    parccm
067300070411     C                   parm                    parmbr
067400070411     C                   parm      '2'           paropz
067500070411     C*
067600070411     C                   eval      wrkesito = '2'
067700070411     C*
067800070411     C                   seton                                        LR
067900070411     C*
068000070411     C                   ENDSR     '*CANCL'
068100070411     C***
068200070411
068300090331
068400090331
068500990910
068600000613     C     *inzsr        BEGSR
068700990910     C*
068800990910     C     *entry        plist
068900990920     C                   parm                    tivlrds
069000990921     C                   parm      wrkesito      esito
069100000724     C                   parm                    prmlit
069200000710     C                   parm                    prmfir
069300000613     C*
069400000830     C* CALCOLA LA DATA CORRENTE
069500000830     C                   time                    wn14             14 0
069600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069700080512     C                   movel     wn14          oracor            6 0          *ORA
069800000830     C                   z-add     wn8           g08dat
069900000830     C                   z-add     *zeros        g08inv
070000000830     C                   movel     '0'           g08err
070100000830     C                   call      'XSRDA8'
070200000830     C                   parm                    wlbda8
070300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
070400000830     C*
070500000613     C                   ENDSR
070600000613     C***
