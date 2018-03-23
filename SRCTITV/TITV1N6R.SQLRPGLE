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
002500090309     D curSped         s              6    INZ(*blanks)
002600090309     D depSped         s              6    INZ(*blanks)
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
004700081217     C*
004800081217     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004900081217     C
005000081217     C/EXEC SQL
005100081217     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005200081217     C/END-EXEC
005300081217     C
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000613     C*
005800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005900000613     C*
006000010202     C* Effettuo la chiamata al CLLE preposto
006100090114     C                   call(e)   'TITVVTC'
006200010202     C                   parm                    parccm
006300010202     C                   parm                    parmbr
006400010202     C                   parm      '2'           paropz
006500050201     C*
006600050201     C* Effettuo lancio TISI95 solo x chiusura
006700050201     C                   CLEAR                   TISI95DS
006800050201     C                   EVAL      I95TLA = 'C'
006900050201     C                   CALL      'TISI95R'
007000050201     C                   PARM                    TISI95DS
007100000616     C*
007200000801     C
007300010201     C                   seton                                        LR
007400990908
007500000801
007600910830     C*--------------------------------------------------------
007700090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007800910830     C*--------------------------------------------------------
007900040526     C     RWFILE        BEGSR
008000990910     C*
008100990914     C                   if        not %open(tivin00r)
008200990908     C                   open      tivin00r
008300990914     C                   endif
008400090114     C                   if        not %open(fivabwwr)
008500090114     C                   open      fivabwwr
008600990914     C                   endif
008700070103     C*
008800090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008900020305     C                   exsr      prevat
009000010201     C*
009100010202     C                   if        chkcall = '0'
009200010202     C*
009300090114     C                   if        not %open(fivatwwr)
009400090114     C                   open      fivatwwr
009500010201     C                   endif
009600990910     C*
009700010201     C                   clear                   §CTROKVB          5 0
009800020305     C                   clear                   §CTROKVT          5 0
009900000801     C                   clear                   §CTRMO            5 0
010000000801     C                   clear                   §CTRNO            5 0
010100990910     C*
010200921023     C                   DO        *HIVAL
010300990913     C*
010400990915     C                   READ      tivin00r                               70
010500050627     C                   if        vindta > *blanks
010600000613     C                   add       1             rrnum
010700000801     C*
010800000801     C                   if        *in70 = *off
010900000801     C                             and
011000000801     C                             (vinflg = *blanks
011100000801     C                              or vinflg = '0'
011200000801     C                              or vinflg = '2')
011300000801     C*
011400000801     C                   clear                   vinmsg
011500000801     C                   eval      vinflg = '1'
011600070103     C*
011700070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011800090309     C                   eval      curSped = %subst(vindta:415:6)
011900070213     C                   if        curSped <> depSped
012000070213     C* Se prima bolla => importo bolla corrente
012100070213     C                   if        §CTRMO = *zeros
012200090309     C                   EXSR      INZVAR
012300090309     C                   EXSR      DEFCAM
012400070213     C                   exsr      impvab                                       => carico  VAB
012500070213     C                   else
012600070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
012700070213     C                   exsr      wrivab                                       => scarico VAB
012800090309     C                   EXSR      INZVAR
012900090309     C                   EXSR      DEFCAM
013000070213     C                   exsr      impvab                                       => carico  VAB
013100070213     C                   endif
013200071121     C                   exsr      exevate                                      => write VAT-E
013300070103     C* Salvo il raggruppamento spedizione cliente corrente
013400070103     C                   eval      depSped = curSped
013500070213     C*
013600070213     C* Se collo successivo x stessa bolla
013700070213     C                   else
013800070213     C                   exsr      impvab                                       => carico  VAB
013900070213     C                   exsr      exevate                                      => write VAT-E
014000070103     C                   endif
014100000905     C*
014200000905     C                   else
014300000905     C                   eval      vinflg = '1'
014400050628     C                   endif
014500000905     C                   endif
014600000905     C*
014700000905     C  N70              update    tivin000
014800000905     C*
014900991022     C  N70              ENDdo
015000070213     C*
015100070213     C* Scarico testata bolla rimasta in sospesa
015200070213     C                   exsr      wrivab                                       => scarico VAB
015300010202     C*
015400010202     C                   endif
015500990910
015600990910     C* Se non ci sono record con errori ...
015700000710     C                   if        §ctrno = 0
015800990910     C* ... restituisco esito OK.
015900990921     C                   eval      wrkesito = '0'
016000990910     C                   else
016100010201     C                   if        §ctrokvb > 0
016200990921     C                   eval      wrkesito = '1'
016300000710     C                   else
016400000710     C                   eval      wrkesito = '2'
016500990910     C                   endif
016600000710     C                   endif
016700990910     C*
016800990914     C                   if        %open(tivin00r)
016900990908     C                   close     tivin00r
017000990914     C                   endif
017100090114     C                   if        %open(fivabwwr)
017200090114     C                   close     fivabwwr
017300990914     C                   endif
017400090114     C                   if        %open(fivatwwr)
017500090114     C                   close     fivatwwr
017600010201     C                   endif
017700990910     C*
017800010201     C                   if        §ctrokvb > 0
017900000724     C                             and vlrpoi <> *zeros
018000010202     C                   exsr      invio
018100990920     C                   endif
018200990920     C*
018300910830     C                   ENDSR
018400000613     C***
018500010305
018600010305     C*----------------------------------------------------*
018700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018800010305     C*----------------------------------------------------*
018900020305     C     WRIVAB        BEGSR
019000010305     C*
019100060225     C* Quindi scarico il buffer del file d testata
019200090114     C                   write     fivab000                                     => scarico il VAB
019300010305     C*
019400010305     C                   ENDSR
019500990920
019600000801     C*----------------------------------------------------*
019700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019800000801     C*----------------------------------------------------*
019900010201     C     INZVAR        BEGSR
020000000801     C*
020100090114     C                   CLEAR                   FIVAB000
020200090114     C                   CLEAR                   FIVAT000
020300070213     C*
020400040802     C                   Z-ADD     *zeros        Num5_0            5 0
020500040802     C                   MOVEL     '0'           FlgCAS            1
020600000801     C*
020700000801     C                   ENDSR
020800000801     C*----------------------------------------------------*
020900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021000000801     C*----------------------------------------------------*
021100000801     C     DEFCAM        BEGSR
021200000801     C*
021300020619     C* Imposto i valori di default...
021400090309     C                   Z-ADD     0011310       VABCCM
021500090309     C                   Z-ADD     0011310       VATCCM
021600090309     C                   Z-ADD     275           VABLNP
021700090309     C                   Z-ADD     275           VATLNP
021800081217     C                   Z-ADD     000           VABCTR
021900070103     C                   MOVEL     '7Q'          VABCTM
022000040823     C                   MOVEL     '1'           VABCBO
022100020619     C* ... e poi verifico se sono stati passati come parametri
022200020619     C                   IF        vlrppt > *blanks
022300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022500020619     C                   EXSR      CHKNUM
022600020619     C                   IF        PiInt=*on
022700020619     C                   Z-ADD     PiVal         VABCCM
022800020619     C                   Z-ADD     PiVal         VATCCM
022900020619     C                   ENDIF
023000040506     C                   ENDIF
023100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023300020619     C                   EXSR      CHKNUM
023400020619     C                   IF        PiInt=*on
023500020619     C                   Z-ADD     PiVal         VABLNP
023600020619     C                   Z-ADD     PiVal         VATLNP
023700040506     C                   ENDIF
023800020619     C                   ENDIF
023900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024100020619     C                   EXSR      CHKNUM
024200020619     C                   IF        PiInt=*on
024300020619     C                   Z-ADD     PiVal         VABCTR
024400040506     C                   ENDIF
024500020619     C                   ENDIF
024600020619     C                   ENDIF
024700000801     C*
024800000801     C                   ENDSR
024900000801     C*----------------------------------------------------*
025000090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025100000801     C*----------------------------------------------------*
025200040823     C     IMPVAB        BEGSR
025300010305     C*
025400000801     C                   Z-ADD     *zeros        errore            1 0
025500000830     C                   MOVEL     datcor        VABAAS
025600020305     C                   MOVEL     datcor        VATAAS
025700040526     C                   MOVE      datcor        VABMGS
025800040823     C                   MOVE(P)   vlrpoi        VABFGS
025900040823     C                   MOVE(P)   vlrpoi        VATFGS
026000080409     C*
026100080409     C* Reperimento campi ALFA
026200080409     C*
026300080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
026400090309     C                   EVAL      VABRSD=%trim(%subst(vindta:37:35))
026500080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026600080409     C     '@':'A'       XLATE     VABRSD        VABRSD
026700080409     C* ==
026800090309     C                   EVAL      VABIND=%trim(%subst(vindta:72:35))
026900090309     C                   EVAL      VABLOD=%trim(%subst(vindta:137:35))
027000090309     C                   EVAL      VABPRD=%trim(%subst(vindta:127:2))
027100090316     C                   EVAL      VABRMA=%trim(%subst(vindta:408:13))
027200090402     C                   IF        %subst(vindta:593:2) = 'PA'
027300090402     C                   EVAL      VABCBO = '2'
027400090402     C                   ENDIF
027500080409     C*
027600080409     C* Reperimento campi NUMERICI
027700130607     C* LNP
027800130607     C                   IF        %subst(vindta:603:3) <> *zeros  AND
027900130607     C                             %subst(vindta:603:3) <> *blanks
028000130607     C                   EVAL      PiStr=%trim(%subst(vindta:603:3))
028100090309     C                   EXSR      CHKNUM
028200090309     C                   IF        PiInt=*on
028300130607     C                   Z-ADD     PiVal         VABLNP
028400130607     C                   Z-ADD     PiVal         VATLNP
028500130607     C                   IF        VLRPOI = 999
028600130607     C                   Z-ADD     PiVal         VABFGS
028700130607     C                   Z-ADD     PiVal         VATFGS
028800130607     C                   ENDIF
028900090309     C                   ELSE
029000090309     C                   EVAL      vinmsg = %trimr(vinmsg)
029100130607     C                             + ' ' + 'VABLNP VATLNP'
029200090309     C                   ENDIF
029300130607     C                   ENDIF
029400130607     C* NSP
029500130607     C                   EVAL      PiStr=%trim(%subst(vindta:415:6))
029600130607     C                   EXSR      CHKNUM
029700130607     C                   IF        PiInt=*on
029800130607     C                   Z-ADD     PiVal         VABNSP
029900130607     C                   Z-ADD     PiVal         VATNSP
030000130607     C                   ELSE
030100130607     C                   EVAL      vinmsg = %trimr(vinmsg)
030200130607     C                             + ' ' + 'VABNSP VATNSP'
030300130607     C                   ENDIF
030400090309     C* RMN
030500090309     C                   EVAL      PiStr=%trim(%subst(vindta:578:15))
030600081217     C                   EXSR      CHKNUM
030700081217     C                   IF        PiInt=*on
030800081217     C                   Z-ADD     PiVal         VABRMN
030900081217     C                   ELSE
031000081217     C                   ADD       1             errore
031100081217     C                   Z-ADD     1             VABRMN
031200081217     C                   EVAL      vinmsg = %trimr(vinmsg)
031300081217     C                             + ' ' + 'VABRMN'
031400081217     C                   ENDIF
031500080409     C* CAD
031600090309     C                   EVAL      PiStr=%trim(%subst(vindta:122:5))
031700090114     C                   EXSR      CHKNUM
031800090114     C                   IF        PiInt=*on
031900090114     C                   Z-ADD     PiVal         Num5_0
032000090114     C                   MOVEL(p)  Num5_0        VABCAD
032100090114     C                   ELSE
032200090114     C                   ADD       1             errore
032300090114     C                   EVAL      VABCAD = *zeros
032400090114     C                   EVAL      vinmsg = %trimr(vinmsg)
032500090114     C                             + ' ' + 'VABCAD'
032600090114     C                   ENDIF
032700081217     C* NCL
032800090309     C                   EVAL      PiStr=%trim(%subst(vindta:390:5))
032900081217     C                   EXSR      CHKNUM
033000081217     C                   IF        PiInt=*on
033100081217     C                   Z-ADD     PiVal         VABNCL
033200081217     C                   ELSE
033300090309     C                   ADD       1             errore
033400081217     C                   EVAL      vinmsg = %trimr(vinmsg)
033500081217     C                             + ' ' + 'VABNCL'
033600081217     C                   ENDIF
033700090309     C* IAS
033800090309     C                   IF        %subst(vindta:174:8) <> *zeros  AND
033900090309     C                             %subst(vindta:174:8) <> *blanks
034000090309     C                   EVAL      PiStr=%trim(%subst(vindta:174:8))
034100090309     C                   EXSR      CHKNUM
034200090309     C                   IF        PiNum=*on
034300090309     C                   EVAL      VABVAS = 'EUR'
034400090309     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec
034500090309     C                   Z-ADD(H)  PiVal         VABIAS
034600090309     C                   ELSE
034700090309     C                   ADD       1             errore
034800090309     C                   EVAL      vinmsg = %trimr(vinmsg)
034900090309     C                             + ' ' + 'VABIAS'
035000090309     C                   ENDIF
035100090309     C                   ENDIF
035200090309     C* CAS
035300090309     C                   IF        %subst(vindta:182:8) <> *zeros  AND
035400090309     C                             %subst(vindta:182:8) <> *blanks
035500090309     C                   EVAL      FlgCAS = '1'
035600090309     C                   EVAL      PiStr=%trim(%subst(vindta:182:8))
035700090309     C                   EXSR      CHKNUM
035800090309     C                   IF        PiNum=*on
035900090309     C                   EVAL      VABVCA = 'EUR'
036000090309     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec
036100090309     C                   Z-ADD(H)  PiVal         VABCAS
036200090309     C                   ELSE
036300090309     C                   ADD       1             errore
036400090309     C                   EVAL      vinmsg = %trimr(vinmsg)
036500090309     C                             + ' ' + 'VABCAS'
036600090309     C                   ENDIF
036700090309     C                   ENDIF
036800090318     C* PKB
036900090318     C                   EVAL      PiStr=%trim(%subst(vindta:395:6))
037000090318     C                   EXSR      CHKNUM
037100090318     C                   IF        PiNum=*on
037200090318     C                   Z-ADD     PiVal         VABPKB
037300090318     C                   ELSE
037400090318     C                   Z-ADD     *zeros        VABPKB
037500090318     C                   ADD       1             errore
037600090318     C                   EVAL      vinmsg = %trimr(vinmsg)
037700090318     C                             + ' ' + 'VABPKB'
037800090318     C                   ENDIF
037900071121     C*
038000080415     C* Carico l'estensioni A e B del FNVAT
038100080512     C                   exsr      exevata
038200080512     C                   exsr      exevatb
038300010205     C*
038400010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038500040802     C                   IF        FlgCAS <> '0'
038600130404     C                   SELECT
038700130404     C                   WHEN      VABCBO = '1'
038800010205     C                   EVAL      VABCBO = '4'
038900130404     C                   WHEN      VABCBO = '2'
039000130404     C                   EVAL      VABCBO = '6'
039100130404     C                   ENDSL
039200010205     C                   ENDIF
039300020305     C*
039400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039500011113     C                   EXSR      CHKIMPDIV
039600010202     C*
039700000801     C* Ebbene...
039800000801     C                   ADD       1             §CTRMO
039900010201     C                   IF        errore <> *zeros
040000000801     C                   ADD       1             §CTRNO
040100000801     C                   EVAL      vinflg = '2'
040200000801     C                   ELSE
040300010201     C                   ADD       1             §CTROKVB
040400000801     C                   ENDIF
040500000801     C*
040600000801     C                   ENDSR
040700070102     C*----------------------------------------------------*
040800090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
040900070102     C*----------------------------------------------------*
041000070103     C     EXEVATE       BEGSR
041100070102     C*
041200070103     C                   EVAL      VATTRC='E'
041300090319     C                   EVAL      VATNOT=%trim(%subst(vindta:3:8))
041400080512     C*
041500080512     C                   exsr      wrivat                                       => scarico VAT
041600070102     C*
041700070102     C                   ENDSR
041800080415     C*----------------------------------------------------*
041900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
042000080415     C*----------------------------------------------------*
042100080415     C     EXEVATA       BEGSR
042200080415     C*
042300080415     C*
042400080415     C***                exsr      wrivat                                       => scarico VAT
042500080415     C*
042600080415     C                   ENDSR
042700071121     C*----------------------------------------------------*
042800090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
042900071121     C*----------------------------------------------------*
043000071121     C     EXEVATB       BEGSR
043100071121     C*
043200071121     C*
043300080409     C***                exsr      wrivat                                       => scarico VAT
043400071121     C*
043500071121     C                   ENDSR
043600010201     C*----------------------------------------------------*
043700090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043800010201     C*----------------------------------------------------*
043900020305     C     WRIVAT        BEGSR
044000050628     C*
044100060223     C* Scrivo solo se valorizzato qualcosa
044200060223     C                   IF        VATNOT <> *blanks
044300090114     C                   WRITE     FIVAT000
044400060223     C                   ENDIF
044500010201     C*
044600010201     C                   ENDSR
044700010202     C*----------------------------------------------------*
044800090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044900010202     C*----------------------------------------------------*
045000020305     C     PREVAT        BEGSR
045100010202     C*
045200090114     C* Compongo il nome del membro da dare al FIVATWWR
045300010202     C                   eval      parmbr = vlrhdl
045400010202     C                   movel     'M'           parmbr
045500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
045600010202     C                   eval      paropz = '1'
045700010202     C* Effettuo la chiamata al CLLE preposto
045800090114     C                   call(e)   'TITVVTC'
045900010202     C                   parm                    parccm
046000010202     C                   parm                    parmbr
046100010202     C                   parm                    paropz
046200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046300010202     C                   if        %error
046400010202     C                   movel     '1'           chkcall
046500010202     C                   else
046600010202     C                   movel     '0'           chkcall
046700010202     C                   endif
046800010202     C*
046900010202     C                   ENDSR
047000000801     C*----------------------------------------------------*
047100000801     C*  CONTROLLO NUMERICITA' CAMPI
047200000801     C*----------------------------------------------------*
047300000801     C     CHKNUM        BEGSR
047400081217     C*
047500081217     C                   IF        PiDecChr = *blanks
047600081217     C                   EVAL      PiDecChr = '.'
047700081217     C                   ENDIF
047800081217     C*
047900081217     C                   callp(e)  UBISNUM_Check(PiStr
048000081217     C                                          :PiDecChr
048100081217     C                                          :PiVal
048200081217     C                                          :PiNum
048300081217     C                                          :PiInt)
048400081217     C*
048500081217     C                   IF        %error
048600081217     C                   EVAL      PiInt=*off
048700081217     C                   ENDIF
048800000801     C*
048900000801     C                   ENDSR
049000000801     C***
049100000801
049200011113
049300011113     C*----------------------------------------------------*
049400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049500011113     C*----------------------------------------------------*
049600011113     C     CHKIMPDIV     BEGSR
049700011113     C*
049800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049900011113     C                   Z-ADD     *zeros        wrkDec            9 9
050000011113     C*
050100011113     C* Come prima cosa effettuo considerazioni sulla divisa
050200011113     C                   IF        vabIAS > *zeros
050300011113     C                   IF        vabVAS <> 'EUR'
050400011113     C                   EVAL      vabVAS =  'ITL'
050500011113     C                   ENDIF
050600011113     C                   ENDIF
050700011113     C*
050800011113     C                   IF        vabCAS > *zeros
050900011113     C                   IF        vabVCA <> 'EUR'
051000011113     C                   EVAL      vabVCA =  'ITL'
051100011113     C                   ENDIF
051200011113     C                   ENDIF
051300011113     C*
051400011113     C                   IF        vabVMD > *zeros
051500020305     C                   IF        vabVAD <> 'EUR'
051600011113     C                   EVAL      vabVAD =  'ITL'
051700011113     C                   ENDIF
051800011113     C                   ENDIF
051900011113     C*
052000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052100011113     C                   Z-ADD     vabIAS        wrkDec
052200011113     C                   IF        wrkDec > *zeros
052300011113     C                   IF        vabVAS = 'ITL'
052400011113     C                   EVAL      vabIAS = *zeros
052500011113     C                   ENDIF
052600011113     C                   ENDIF
052700011113     C*
052800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052900011113     C                   Z-ADD     vabCAS        wrkDec
053000011113     C                   IF        wrkDec > *zeros
053100011113     C                   IF        vabVCA = 'ITL'
053200011113     C                   EVAL      vabCAS = *zeros
053300011113     C                   ENDIF
053400011113     C                   ENDIF
053500011113     C*
053600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053700011113     C                   Z-ADD     vabVMD        wrkDec
053800011113     C                   IF        wrkDec > *zeros
053900011113     C                   IF        vabVAD = 'ITL'
054000011113     C                   EVAL      vabVMD = *zeros
054100011113     C                   ENDIF
054200011113     C                   ENDIF
054300011113     C*
054400011113     C                   ENDSR
054500011113     C***
054600011113
054700011113
054800000801
054900000801
055000990920      /TITLE Invio dei dati al punto operativo.
055100010202     C     invio         BEGSR
055200990920     C*
055300090114     C* 1° invio FIVAT
055400010201     C                   reset                   dscmz
055500130610     C                   move      vatfgs        cmzdst
055600090114     C                   eval      cmzfld = 'FIVATWWR'
055700010201     C                   eval      cmzmbd = vlrhdl
055800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055900021009     C***                if        prmfir = *blanks
056000090114     C                   eval      cmzfla = 'FIVAT00F'
056100090114     C                   eval      cmzmba = 'FIVAT00F'
056200021009     C***                else
056300021009     C***                eval      cmzfla = prmfir
056400021009     C***                eval      cmzmba = prmfir
056500021009     C***                endif
056600010201     C                   eval      cmznrr = *zeros
056700020305     C                   move      §ctrokvt      cmznrr
056800021018     C                   eval      cmzlba = vlrfl1
056900010201     C                   call(e)   'TIS711C'
057000010201     C                   parm                    dscmz
057100010201     C                   parm      *blanks       esito
057200010205     C                   if        %error
057300010205     C                             or cmzerr = '1'
057400010205     C                             or esito  = '1'
057500010205     C                   eval      wrkesito = '3'
057600010205     C                   else
057700010201     C*
057800090114     C* 2° invio FIVAB
057900010201     C                   reset                   dscmz
058000130610     C                   move      vabfgs        cmzdst
058100010201     C                   eval      cmzfld = vlrfou
058200010201     C                   eval      cmzmbd = vlrhdl
058300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058400021009     C***                if        prmfir = *blanks
058500090114     C                   eval      cmzfla = 'FIVAB00F'
058600090114     C                   eval      cmzmba = 'FIVAB00F'
058700021009     C***                else
058800021009     C***                eval      cmzfla = prmfir
058900021009     C***                eval      cmzmba = prmfir
059000021009     C***                endif
059100010201     C                   eval      cmznrr = *zeros
059200010201     C                   move      §ctrokvb      cmznrr
059300021018     C                   eval      cmzlba = vlrfl1
059400010201     C                   call(e)   'TIS711C'
059500010201     C                   parm                    dscmz
059600010201     C                   parm      *blanks       esito
059700010201     C                   if        %error
059800010201     C                             or cmzerr = '1'
059900010201     C                             or esito  = '1'
060000010201     C                   eval      wrkesito = '3'
060100010201     C                   endif
060200010205     C                   endif
060300990920     C*
060400000613     C                   ENDSR
060500000613     C***
060600070411
060700070411     C     *pssr         BEGSR
060800070411     C*
060900070411     C                   if        %open(tivin00r)
061000070411     C                   close     tivin00r
061100070411     C                   endif
061200090114     C                   if        %open(fivabwwr)
061300090114     C                   close     fivabwwr
061400070411     C                   endif
061500090114     C                   if        %open(fivatwwr)
061600090114     C                   close     fivatwwr
061700070411     C                   endif
061800070411     C*
061900070411     C* Effettuo la chiamata al CLLE preposto
062000090114     C                   call(e)   'TITVVTC'
062100070411     C                   parm                    parccm
062200070411     C                   parm                    parmbr
062300070411     C                   parm      '2'           paropz
062400070411     C*
062500070411     C                   eval      wrkesito = '2'
062600070411     C*
062700070411     C                   seton                                        LR
062800070411     C*
062900070411     C                   ENDSR     '*CANCL'
063000070411     C***
063100070411
063200990910
063300000613     C     *inzsr        BEGSR
063400990910     C*
063500990910     C     *entry        plist
063600990920     C                   parm                    tivlrds
063700990921     C                   parm      wrkesito      esito
063800000724     C                   parm                    prmlit
063900000710     C                   parm                    prmfir
064000000613     C*
064100000830     C* CALCOLA LA DATA CORRENTE
064200000830     C                   time                    wn14             14 0
064300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064400080512     C                   movel     wn14          oracor            6 0          *ORA
064500000830     C                   z-add     wn8           g08dat
064600000830     C                   z-add     *zeros        g08inv
064700000830     C                   movel     '0'           g08err
064800000830     C                   call      'XSRDA8'
064900000830     C                   parm                    wlbda8
065000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065100000830     C*
065200000613     C                   ENDSR
065300000613     C***
