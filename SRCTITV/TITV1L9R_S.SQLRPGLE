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
002500090114     D curSped         s             20    INZ(*blanks)
002600090114     D depSped         s             20    INZ(*blanks)
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
003700090114
003800090114     D*------------------
003900090114     D* DS REPERIMENTO NUMERATORE
004000090114     D*------------------
004100090114     D trul33ds      e ds                  inz
004200090114     D*------------------
004300090114     D* DS ARCHITETTURA
004400090114     D*------------------
004500090114     D kpjba         e ds                  inz
004600090114
004700081217
004800081217     D*------------------
004900081217     D* LINKING A DEFINIZIONI ESTERNE
005000081217     D*------------------
005100081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005200081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005300081217
005400081217
005500010201
005600081217
005700081217     C*
005800081217     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005900081217     C
006000081217     C/EXEC SQL
006100081217     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006200081217     C/END-EXEC
006300081217     C
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700000613     C*
006800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006900000613     C*
007000010202     C* Effettuo la chiamata al CLLE preposto
007100090114     C                   call(e)   'TITVVTC'
007200010202     C                   parm                    parccm
007300010202     C                   parm                    parmbr
007400010202     C                   parm      '2'           paropz
007500050201     C*
007600050201     C* Effettuo lancio TISI95 solo x chiusura
007700050201     C                   CLEAR                   TISI95DS
007800050201     C                   EVAL      I95TLA = 'C'
007900050201     C                   CALL      'TISI95R'
008000050201     C                   PARM                    TISI95DS
008100000616     C*
008200000801     C
008300010201     C                   seton                                        LR
008400990908
008500000801
008600910830     C*--------------------------------------------------------
008700090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008800910830     C*--------------------------------------------------------
008900040526     C     RWFILE        BEGSR
009000990910     C*
009100990914     C                   if        not %open(tivin00r)
009200990908     C                   open      tivin00r
009300990914     C                   endif
009400090114     C                   if        not %open(fivabwwr)
009500090114     C                   open      fivabwwr
009600990914     C                   endif
009700070103     C*
009800090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009900020305     C                   exsr      prevat
010000010201     C*
010100010202     C                   if        chkcall = '0'
010200010202     C*
010300090114     C                   if        not %open(fivatwwr)
010400090114     C                   open      fivatwwr
010500010201     C                   endif
010600990910     C*
010700010201     C                   clear                   §CTROKVB          5 0
010800020305     C                   clear                   §CTROKVT          5 0
010900000801     C                   clear                   §CTRMO            5 0
011000000801     C                   clear                   §CTRNO            5 0
011100990910     C*
011200921023     C                   DO        *HIVAL
011300990913     C*
011400990915     C                   READ      tivin00r                               70
011500050627     C                   if        vindta > *blanks
011600000613     C                   add       1             rrnum
011700000801     C*
011800000801     C                   if        *in70 = *off
011900000801     C                             and
012000000801     C                             (vinflg = *blanks
012100000801     C                              or vinflg = '0'
012200000801     C                              or vinflg = '2')
012300000801     C*
012400000801     C                   clear                   vinmsg
012500000801     C                   eval      vinflg = '1'
012600070103     C*
012700070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
012800090114     C                   eval      curSped = %subst(vindta:1:20)
012900070213     C                   if        curSped <> depSped
013000070213     C* Se prima bolla => importo bolla corrente
013100070213     C                   if        §CTRMO = *zeros
013200090114     C                   exsr      repNSP
013300070213     C                   exsr      impvab                                       => carico  VAB
013400070213     C                   else
013500070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
013600070213     C                   exsr      wrivab                                       => scarico VAB
013700090114     C                   exsr      repNSP
013800070213     C                   exsr      impvab                                       => carico  VAB
013900070213     C                   endif
014000071121     C                   exsr      exevate                                      => write VAT-E
014100070103     C* Salvo il raggruppamento spedizione cliente corrente
014200070103     C                   eval      depSped = curSped
014300070213     C*
014400070213     C* Se collo successivo x stessa bolla
014500070213     C                   else
014600070213     C                   exsr      impvab                                       => carico  VAB
014700070213     C                   exsr      exevate                                      => write VAT-E
014800070103     C                   endif
014900000905     C*
015000000905     C                   else
015100000905     C                   eval      vinflg = '1'
015200050628     C                   endif
015300000905     C                   endif
015400000905     C*
015500000905     C  N70              update    tivin000
015600000905     C*
015700991022     C  N70              ENDdo
015800070213     C*
015900070213     C* Scarico testata bolla rimasta in sospesa
016000070213     C                   exsr      wrivab                                       => scarico VAB
016100010202     C*
016200010202     C                   endif
016300990910
016400990910     C* Se non ci sono record con errori ...
016500000710     C                   if        §ctrno = 0
016600990910     C* ... restituisco esito OK.
016700990921     C                   eval      wrkesito = '0'
016800990910     C                   else
016900010201     C                   if        §ctrokvb > 0
017000990921     C                   eval      wrkesito = '1'
017100000710     C                   else
017200000710     C                   eval      wrkesito = '2'
017300990910     C                   endif
017400000710     C                   endif
017500990910     C*
017600990914     C                   if        %open(tivin00r)
017700990908     C                   close     tivin00r
017800990914     C                   endif
017900090114     C                   if        %open(fivabwwr)
018000090114     C                   close     fivabwwr
018100990914     C                   endif
018200090114     C                   if        %open(fivatwwr)
018300090114     C                   close     fivatwwr
018400010201     C                   endif
018500990910     C*
018600010201     C                   if        §ctrokvb > 0
018700000724     C                             and vlrpoi <> *zeros
018800010202     C                   exsr      invio
018900990920     C                   endif
019000990920     C*
019100910830     C                   ENDSR
019200000613     C***
019300010305
019400010305     C*----------------------------------------------------*
019500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019600010305     C*----------------------------------------------------*
019700020305     C     WRIVAB        BEGSR
019800010305     C*
019900060225     C* Quindi scarico il buffer del file d testata
020000090114     C                   write     fivab000                                     => scarico il VAB
020100010305     C*
020200010305     C                   ENDSR
020300990920
020400000801     C*----------------------------------------------------*
020500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020600000801     C*----------------------------------------------------*
020700010201     C     INZVAR        BEGSR
020800000801     C*
020900090114     C                   CLEAR                   FIVAB000
021000090114     C                   CLEAR                   FIVAT000
021100070213     C*
021200040802     C                   Z-ADD     *zeros        Num5_0            5 0
021300040802     C                   MOVEL     '0'           FlgCAS            1
021400000801     C*
021500000801     C                   ENDSR
021600000801     C*----------------------------------------------------*
021700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021800000801     C*----------------------------------------------------*
021900000801     C     DEFCAM        BEGSR
022000000801     C*
022100020619     C* Imposto i valori di default...
022200090114     C                   Z-ADD     0895020       VABCCM
022300090114     C                   Z-ADD     0895020       VATCCM
022400090114     C                   Z-ADD     089           VABLNP
022500090114     C                   Z-ADD     089           VATLNP
022600081217     C                   Z-ADD     000           VABCTR
022700090114     C                   MOVEL     'D'           VABTSP
022800090114     C                   EVAL      VABNAS = 'ABBIGLIAMENTO'
022900070103     C                   MOVEL     '7Q'          VABCTM
023000040823     C                   MOVEL     '1'           VABCBO
023100020619     C* ... e poi verifico se sono stati passati come parametri
023200020619     C                   IF        vlrppt > *blanks
023300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023500020619     C                   EXSR      CHKNUM
023600020619     C                   IF        PiInt=*on
023700020619     C                   Z-ADD     PiVal         VABCCM
023800020619     C                   Z-ADD     PiVal         VATCCM
023900020619     C                   ENDIF
024000040506     C                   ENDIF
024100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024300020619     C                   EXSR      CHKNUM
024400020619     C                   IF        PiInt=*on
024500020619     C                   Z-ADD     PiVal         VABLNP
024600020619     C                   Z-ADD     PiVal         VATLNP
024700040506     C                   ENDIF
024800020619     C                   ENDIF
024900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
025000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025100020619     C                   EXSR      CHKNUM
025200020619     C                   IF        PiInt=*on
025300020619     C                   Z-ADD     PiVal         VABCTR
025400040506     C                   ENDIF
025500020619     C                   ENDIF
025600020619     C                   ENDIF
025700000801     C*
025800000801     C                   ENDSR
025900090114     C*----------------------------------------------------*
026000090114     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
026100090114     C*----------------------------------------------------*
026200090114     C     REPNSP        BEGSR
026300090114     C*
026400090114     C                   EXSR      INZVAR
026500090114     C                   EXSR      DEFCAM
026600090114     C*
026700090114     C* NSP => Stacco un numeratore da AZNUM
026800090114     C                   clear                   TRUL33DS
026900090114     C                   eval      I33OPE = *zeros
027000090114     C                   eval      I33CNU = 302
027100090114     C                   eval      I33NUM = 1
027200090114     C                   movel     TRUL33DS      KPJBU
027300090114     C                   call      'TRUL33R'
027400090114     C                   parm                    KPJBA
027500090114     C                   movel     KPJBU         TRUL33DS
027600090114     C                   if        O33ERR = *zeros
027700090114     C                   z-add     O33NRF        VABNSP
027800090114     C                   z-add     O33NRF        VATNSP
027900090114     C                   else
028000090114     C                   SETON                                        31
028100090114     C                   EVAL      vinmsg = %trimr(vinmsg)
028200090114     C                             + ' ' + 'VABNSP VATNSP'
028300090114     C                   endif
028400090114     C*
028500090114     C                   ENDSR
028600000801     C*----------------------------------------------------*
028700090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028800000801     C*----------------------------------------------------*
028900040823     C     IMPVAB        BEGSR
029000010305     C*
029100000801     C                   Z-ADD     *zeros        errore            1 0
029200000830     C                   MOVEL     datcor        VABAAS
029300020305     C                   MOVEL     datcor        VATAAS
029400040526     C                   MOVE      datcor        VABMGS
029500040823     C                   MOVE(P)   vlrpoi        VABFGS
029600040823     C                   MOVE(P)   vlrpoi        VATFGS
029700080409     C*
029800080409     C* Reperimento campi ALFA
029900080409     C*
030000080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
030100090114     C                   EVAL      VABRSD=%trim(%subst(vindta:29:35))
030200080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030300080409     C     '@':'A'       XLATE     VABRSD        VABRSD
030400080409     C* ==
030500090114     C                   EVAL      VABRD2=%trim(%subst(vindta:64:35))
030600090114     C                   EVAL      VABIND=%trim(%subst(vindta:129:35))
030700090114     C                   EVAL      VABLOD=%trim(%subst(vindta:229:35))
030800090114     C                   EVAL      VABPRD=%trim(%subst(vindta:299:2))
030900090114     C                   EVAL      VABRMA=%trim(%subst(vindta:1:15))
031000090114     C                   EVAL      VABNOT=%trim(%subst(vindta:381:17))
031100090114     C                   EVAL      VABNT2=%trim(%subst(vindta:164:35))
031200080409     C*
031300080409     C* Reperimento campi NUMERICI
031400081217     C* RMN
031500090114     C                   EVAL      PiStr=%trim(%subst(vindta:16:5))
031600081217     C                   EXSR      CHKNUM
031700081217     C                   IF        PiInt=*on
031800081217     C                   Z-ADD     PiVal         VABRMN
031900081217     C                   ELSE
032000081217     C                   ADD       1             errore
032100081217     C                   Z-ADD     1             VABRMN
032200081217     C                   EVAL      vinmsg = %trimr(vinmsg)
032300081217     C                             + ' ' + 'VABRMN'
032400081217     C                   ENDIF
032500080409     C* CAD
032600090114     C                   EVAL      PiStr=%trim(%subst(vindta:290:9))
032700090114     C                   EXSR      CHKNUM
032800090114     C                   IF        PiInt=*on
032900090114     C                   Z-ADD     PiVal         Num5_0
033000090114     C                   MOVEL(p)  Num5_0        VABCAD
033100090114     C                   ELSE
033200090114     C                   ADD       1             errore
033300090114     C                   EVAL      VABCAD = *zeros
033400090114     C                   EVAL      vinmsg = %trimr(vinmsg)
033500090114     C                             + ' ' + 'VABCAD'
033600090114     C                   ENDIF
033700081217     C* NCL
033800090114     C                   EVAL      PiStr=%trim(%subst(vindta:351:5))
033900081217     C                   EXSR      CHKNUM
034000081217     C                   IF        PiInt=*on
034100081217     C                   Z-ADD     PiVal         VABNCL
034200081217     C                   ELSE
034300081217     C                   EVAL      vinmsg = %trimr(vinmsg)
034400081217     C                             + ' ' + 'VABNCL'
034500081217     C                   ENDIF
034600081217     C* PKB
034700081217     C*
034800081217     C/EXEC SQL
034900081217     C+ declare C1 cursor for
035000090114     C+ select sum(decimal(substr(vindta, 373, 8))/1000) as peso
035100081217     C+ from tivin00r
035200090114     C+ where substr(vindta, 1, 20) = :curSped
035300081217     C+ for read only
035400081217     C/END-EXEC
035500081217     C
035600081217     C/EXEC SQL
035700081217     C+ open C1
035800081217     C/END-EXEC
035900081217     C                   IF        sqlcod <= *zeros
036000081217     C
036100081217     C/EXEC SQL
036200081217     C+ Fetch C1 into :VABPKB
036300081217     C/END-EXEC
036400081217     C
036500081217     C/EXEC SQL
036600081217     C+ close C1
036700081217     C/END-EXEC
036800081217     C*
036900081217     C                   ELSE
037000081217     C                   ADD       1             errore
037100081217     C                   EVAL      vinmsg = %trimr(vinmsg)
037200081217     C                             + ' ' + 'VABPKB'
037300081217     C                   ENDIF
037400090114     C* VLB
037500090114     C*
037600090114     C/EXEC SQL
037700090114     C+ declare C2 cursor for
037800090114     C+ select sum(decimal(substr(vindta, 365, 8))/100000) as volume
037900090114     C+ from tivin00r
038000090114     C+ where substr(vindta, 1, 20) = :curSped
038100090114     C+ for read only
038200090114     C/END-EXEC
038300090114     C
038400090114     C/EXEC SQL
038500090114     C+ open C2
038600090114     C/END-EXEC
038700090114     C                   IF        sqlcod <= *zeros
038800090114     C
038900090114     C/EXEC SQL
039000090114     C+ Fetch C2 into :VABVLB
039100090114     C/END-EXEC
039200090114     C
039300090114     C/EXEC SQL
039400090114     C+ close C2
039500090114     C/END-EXEC
039600090114     C*
039700090114     C                   ELSE
039800090114     C                   ADD       1             errore
039900090114     C                   EVAL      vinmsg = %trimr(vinmsg)
040000090114     C                             + ' ' + 'VABVLB'
040100090114     C                   ENDIF
040200071121     C*
040300080415     C* Carico l'estensioni A e B del FNVAT
040400080512     C                   exsr      exevata
040500080512     C                   exsr      exevatb
040600010205     C*
040700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
040800040802     C                   IF        FlgCAS <> '0'
040900010205     C                   EVAL      VABCBO = '4'
041000010205     C                   ENDIF
041100020305     C*
041200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041300011113     C                   EXSR      CHKIMPDIV
041400010202     C*
041500000801     C* Ebbene...
041600000801     C                   ADD       1             §CTRMO
041700010201     C                   IF        errore <> *zeros
041800000801     C                   ADD       1             §CTRNO
041900000801     C                   EVAL      vinflg = '2'
042000000801     C                   ELSE
042100010201     C                   ADD       1             §CTROKVB
042200000801     C                   ENDIF
042300000801     C*
042400000801     C                   ENDSR
042500070102     C*----------------------------------------------------*
042600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
042700070102     C*----------------------------------------------------*
042800070103     C     EXEVATE       BEGSR
042900070102     C*
043000070103     C                   EVAL      VATTRC='E'
043100090324     C                   EVAL      VATNOT=%trim(%subst(vindta:356:9))
043200090324     C                   IF        %subst(vindta:10:1) = '5'
043300100201     C                   EVAL      VATNOT=%subst(vindta:13:2)+'5'+
043400100201     C                                    %trim(%subst(vindta:358:7))
043500090324     C                   ENDIF
043600090324     C                   IF        %subst(vindta:10:1) = '4'
043700110120     C                   EVAL      VATNOT=%subst(vindta:13:2)+'4'+
043800110120     C                                    %trim(%subst(vindta:358:7))
043900110120     C*******            EVAL      VATNOT=%subst(vindta:13:2)+'40'+
044000110120     C*******                             %trim(%subst(vindta:359:6))
044100090324     C                   ENDIF
044200080512     C*
044300080512     C                   exsr      wrivat                                       => scarico VAT
044400070102     C*
044500070102     C                   ENDSR
044600080415     C*----------------------------------------------------*
044700090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
044800080415     C*----------------------------------------------------*
044900080415     C     EXEVATA       BEGSR
045000080415     C*
045100080415     C*
045200080415     C***                exsr      wrivat                                       => scarico VAT
045300080415     C*
045400080415     C                   ENDSR
045500071121     C*----------------------------------------------------*
045600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
045700071121     C*----------------------------------------------------*
045800071121     C     EXEVATB       BEGSR
045900071121     C*
046000071121     C*
046100080409     C***                exsr      wrivat                                       => scarico VAT
046200071121     C*
046300071121     C                   ENDSR
046400010201     C*----------------------------------------------------*
046500090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046600010201     C*----------------------------------------------------*
046700020305     C     WRIVAT        BEGSR
046800050628     C*
046900060223     C* Scrivo solo se valorizzato qualcosa
047000060223     C                   IF        VATNOT <> *blanks
047100090114     C                   WRITE     FIVAT000
047200060223     C                   ENDIF
047300010201     C*
047400010201     C                   ENDSR
047500010202     C*----------------------------------------------------*
047600090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047700010202     C*----------------------------------------------------*
047800020305     C     PREVAT        BEGSR
047900010202     C*
048000090114     C* Compongo il nome del membro da dare al FIVATWWR
048100010202     C                   eval      parmbr = vlrhdl
048200010202     C                   movel     'M'           parmbr
048300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
048400010202     C                   eval      paropz = '1'
048500010202     C* Effettuo la chiamata al CLLE preposto
048600090114     C                   call(e)   'TITVVTC'
048700010202     C                   parm                    parccm
048800010202     C                   parm                    parmbr
048900010202     C                   parm                    paropz
049000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049100010202     C                   if        %error
049200010202     C                   movel     '1'           chkcall
049300010202     C                   else
049400010202     C                   movel     '0'           chkcall
049500010202     C                   endif
049600010202     C*
049700010202     C                   ENDSR
049800000801     C*----------------------------------------------------*
049900000801     C*  CONTROLLO NUMERICITA' CAMPI
050000000801     C*----------------------------------------------------*
050100000801     C     CHKNUM        BEGSR
050200081217     C*
050300081217     C                   IF        PiDecChr = *blanks
050400081217     C                   EVAL      PiDecChr = '.'
050500081217     C                   ENDIF
050600081217     C*
050700081217     C                   callp(e)  UBISNUM_Check(PiStr
050800081217     C                                          :PiDecChr
050900081217     C                                          :PiVal
051000081217     C                                          :PiNum
051100081217     C                                          :PiInt)
051200081217     C*
051300081217     C                   IF        %error
051400081217     C                   EVAL      PiInt=*off
051500081217     C                   ENDIF
051600000801     C*
051700000801     C                   ENDSR
051800000801     C***
051900000801
052000011113
052100011113     C*----------------------------------------------------*
052200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052300011113     C*----------------------------------------------------*
052400011113     C     CHKIMPDIV     BEGSR
052500011113     C*
052600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052700011113     C                   Z-ADD     *zeros        wrkDec            9 9
052800011113     C*
052900011113     C* Come prima cosa effettuo considerazioni sulla divisa
053000011113     C                   IF        vabIAS > *zeros
053100011113     C                   IF        vabVAS <> 'EUR'
053200011113     C                   EVAL      vabVAS =  'ITL'
053300011113     C                   ENDIF
053400011113     C                   ENDIF
053500011113     C*
053600011113     C                   IF        vabCAS > *zeros
053700011113     C                   IF        vabVCA <> 'EUR'
053800011113     C                   EVAL      vabVCA =  'ITL'
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C                   IF        vabVMD > *zeros
054300020305     C                   IF        vabVAD <> 'EUR'
054400011113     C                   EVAL      vabVAD =  'ITL'
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054900011113     C                   Z-ADD     vabIAS        wrkDec
055000011113     C                   IF        wrkDec > *zeros
055100011113     C                   IF        vabVAS = 'ITL'
055200011113     C                   EVAL      vabIAS = *zeros
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055700011113     C                   Z-ADD     vabCAS        wrkDec
055800011113     C                   IF        wrkDec > *zeros
055900011113     C                   IF        vabVCA = 'ITL'
056000011113     C                   EVAL      vabCAS = *zeros
056100011113     C                   ENDIF
056200011113     C                   ENDIF
056300011113     C*
056400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056500011113     C                   Z-ADD     vabVMD        wrkDec
056600011113     C                   IF        wrkDec > *zeros
056700011113     C                   IF        vabVAD = 'ITL'
056800011113     C                   EVAL      vabVMD = *zeros
056900011113     C                   ENDIF
057000011113     C                   ENDIF
057100011113     C*
057200011113     C                   ENDSR
057300011113     C***
057400011113
057500011113
057600000801
057700000801
057800990920      /TITLE Invio dei dati al punto operativo.
057900010202     C     invio         BEGSR
058000990920     C*
058100090114     C* 1° invio FIVAT
058200010201     C                   reset                   dscmz
058300010201     C                   move      vlrpoi        cmzdst
058400090114     C                   eval      cmzfld = 'FIVATWWR'
058500010201     C                   eval      cmzmbd = vlrhdl
058600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058700021009     C***                if        prmfir = *blanks
058800090114     C                   eval      cmzfla = 'FIVAT00F'
058900090114     C                   eval      cmzmba = 'FIVAT00F'
059000021009     C***                else
059100021009     C***                eval      cmzfla = prmfir
059200021009     C***                eval      cmzmba = prmfir
059300021009     C***                endif
059400010201     C                   eval      cmznrr = *zeros
059500020305     C                   move      §ctrokvt      cmznrr
059600021018     C                   eval      cmzlba = vlrfl1
059700010201     C                   call(e)   'TIS711C'
059800010201     C                   parm                    dscmz
059900010201     C                   parm      *blanks       esito
060000010205     C                   if        %error
060100010205     C                             or cmzerr = '1'
060200010205     C                             or esito  = '1'
060300010205     C                   eval      wrkesito = '3'
060400010205     C                   else
060500010201     C*
060600090114     C* 2° invio FIVAB
060700010201     C                   reset                   dscmz
060800010201     C                   move      vlrpoi        cmzdst
060900010201     C                   eval      cmzfld = vlrfou
061000010201     C                   eval      cmzmbd = vlrhdl
061100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061200021009     C***                if        prmfir = *blanks
061300090114     C                   eval      cmzfla = 'FIVAB00F'
061400090114     C                   eval      cmzmba = 'FIVAB00F'
061500021009     C***                else
061600021009     C***                eval      cmzfla = prmfir
061700021009     C***                eval      cmzmba = prmfir
061800021009     C***                endif
061900010201     C                   eval      cmznrr = *zeros
062000010201     C                   move      §ctrokvb      cmznrr
062100021018     C                   eval      cmzlba = vlrfl1
062200010201     C                   call(e)   'TIS711C'
062300010201     C                   parm                    dscmz
062400010201     C                   parm      *blanks       esito
062500010201     C                   if        %error
062600010201     C                             or cmzerr = '1'
062700010201     C                             or esito  = '1'
062800010201     C                   eval      wrkesito = '3'
062900010201     C                   endif
063000010205     C                   endif
063100990920     C*
063200000613     C                   ENDSR
063300000613     C***
063400070411
063500070411     C     *pssr         BEGSR
063600070411     C*
063700070411     C                   if        %open(tivin00r)
063800070411     C                   close     tivin00r
063900070411     C                   endif
064000090114     C                   if        %open(fivabwwr)
064100090114     C                   close     fivabwwr
064200070411     C                   endif
064300090114     C                   if        %open(fivatwwr)
064400090114     C                   close     fivatwwr
064500070411     C                   endif
064600070411     C*
064700070411     C* Effettuo la chiamata al CLLE preposto
064800090114     C                   call(e)   'TITVVTC'
064900070411     C                   parm                    parccm
065000070411     C                   parm                    parmbr
065100070411     C                   parm      '2'           paropz
065200070411     C*
065300070411     C                   eval      wrkesito = '2'
065400070411     C*
065500070411     C                   seton                                        LR
065600070411     C*
065700070411     C                   ENDSR     '*CANCL'
065800070411     C***
065900070411
066000990910
066100000613     C     *inzsr        BEGSR
066200990910     C*
066300990910     C     *entry        plist
066400990920     C                   parm                    tivlrds
066500990921     C                   parm      wrkesito      esito
066600000724     C                   parm                    prmlit
066700000710     C                   parm                    prmfir
066800000613     C*
066900000830     C* CALCOLA LA DATA CORRENTE
067000000830     C                   time                    wn14             14 0
067100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067200080512     C                   movel     wn14          oracor            6 0          *ORA
067300000830     C                   z-add     wn8           g08dat
067400000830     C                   z-add     *zeros        g08inv
067500000830     C                   movel     '0'           g08err
067600000830     C                   call      'XSRDA8'
067700000830     C                   parm                    wlbda8
067800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067900000830     C*
068000000613     C                   ENDSR
068100000613     C***
