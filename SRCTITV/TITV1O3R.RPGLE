000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081118     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300120208
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
002500090331     D curSped         s             10    INZ(*blanks)
002600090331     D depSped         s             10    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600081118
003700081118     D*------------------
003800081118     D* LINKING A DEFINIZIONI ESTERNE
003900081118     D*------------------
004000081118     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004100081218     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004200081118
004300081118
004400010201
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800000616     C*
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
007800070103     C*
007900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008000020305     C                   exsr      prevat
008100010201     C*
008200010202     C                   if        chkcall = '0'
008300010202     C*
008400021113     C                   if        not %open(fivatwwr)
008500021113     C                   open      fivatwwr
008600010201     C                   endif
008700990910     C*
008800010201     C                   clear                   §CTROKVB          5 0
008900020305     C                   clear                   §CTROKVT          5 0
009000000801     C                   clear                   §CTRMO            5 0
009100000801     C                   clear                   §CTRNO            5 0
009200990910     C*
009300921023     C                   DO        *HIVAL
009400990913     C*
009500990915     C                   READ      tivin00r                               70
009600050627     C                   if        vindta > *blanks
009700000613     C                   add       1             rrnum
009800000801     C*
009900000801     C                   if        *in70 = *off
010000000801     C                             and
010100000801     C                             (vinflg = *blanks
010200000801     C                              or vinflg = '0'
010300000801     C                              or vinflg = '2')
010400000801     C*
010500000801     C                   clear                   vinmsg
010600000801     C                   eval      vinflg = '1'
010700070103     C*
010800070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010900090331     C                   eval      curSped = %trim(%subst(vindta:191:10))
011000070213     C                   if        curSped <> depSped
011100070213     C* Se prima bolla => importo bolla corrente
011200070213     C                   if        §CTRMO = *zeros
011300070213     C                   exsr      inzvar
011400070213     C                   exsr      defcam
011500070213     C                   exsr      impvab                                       => carico  VAB
011600070213     C                   else
011700070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
011800070213     C                   exsr      wrivab                                       => scarico VAB
011900070213     C                   exsr      inzvar
012000070213     C                   exsr      defcam
012100070213     C                   exsr      impvab                                       => carico  VAB
012200070213     C                   endif
012300071121     C                   exsr      exevate                                      => write VAT-E
012400070103     C* Salvo il raggruppamento spedizione cliente corrente
012500070103     C                   eval      depSped = curSped
012600070213     C*
012700070213     C* Se collo successivo x stessa bolla
012800070213     C                   else
012900070213     C                   exsr      impvab                                       => carico  VAB
013000070213     C                   exsr      exevate                                      => write VAT-E
013100070103     C                   endif
013200000905     C*
013300000905     C                   else
013400000905     C                   eval      vinflg = '1'
013500050628     C                   endif
013600000905     C                   endif
013700000905     C*
013800000905     C  N70              update    tivin000
013900000905     C*
014000991022     C  N70              ENDdo
014100070213     C*
014200070213     C* Scarico testata bolla rimasta in sospesa
014300070213     C                   exsr      wrivab                                       => scarico VAB
014400010202     C*
014500010202     C                   endif
014600990910
014700990910     C* Se non ci sono record con errori ...
014800000710     C                   if        §ctrno = 0
014900990910     C* ... restituisco esito OK.
015000990921     C                   eval      wrkesito = '0'
015100990910     C                   else
015200010201     C                   if        §ctrokvb > 0
015300990921     C                   eval      wrkesito = '1'
015400000710     C                   else
015500000710     C                   eval      wrkesito = '2'
015600990910     C                   endif
015700000710     C                   endif
015800990910     C*
015900990914     C                   if        %open(tivin00r)
016000990908     C                   close     tivin00r
016100990914     C                   endif
016200021113     C                   if        %open(fivabwwr)
016300021113     C                   close     fivabwwr
016400990914     C                   endif
016500021113     C                   if        %open(fivatwwr)
016600021113     C                   close     fivatwwr
016700010201     C                   endif
016800990910     C*
016900010201     C                   if        §ctrokvb > 0
017000000724     C                             and vlrpoi <> *zeros
017100010202     C                   exsr      invio
017200990920     C                   endif
017300990920     C*
017400910830     C                   ENDSR
017500000613     C***
017600010305
017700010305     C*----------------------------------------------------*
017800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017900010305     C*----------------------------------------------------*
018000020305     C     WRIVAB        BEGSR
018100010305     C*
018200060225     C* Quindi scarico il buffer del file d testata
018300021113     C                   write     fivab000                                     => scarico il VAB
018400010305     C*
018500010305     C                   ENDSR
018600990920
018700000801     C*----------------------------------------------------*
018800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018900000801     C*----------------------------------------------------*
019000010201     C     INZVAR        BEGSR
019100000801     C*
019200070213     C                   CLEAR                   FIVAB000
019300070213     C                   CLEAR                   FIVAT000
019400070213     C*
019500040802     C                   Z-ADD     *zeros        Num5_0            5 0
019600040802     C                   MOVEL     '0'           FlgCAS            1
019700000801     C*
019800000801     C                   ENDSR
019900000801     C*----------------------------------------------------*
020000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020100000801     C*----------------------------------------------------*
020200000801     C     DEFCAM        BEGSR
020300000801     C*
020400020619     C* Imposto i valori di default...
020500090331     C                   Z-ADD     0690237       VABCCM
020600090331     C                   Z-ADD     0690237       VATCCM
020700090331     C                   Z-ADD     069           VABLNP
020800090331     C                   Z-ADD     069           VATLNP
020900090402     C                   Z-ADD     300           VABCTR
021000070103     C                   MOVEL     '7Q'          VABCTM
021100040823     C                   MOVEL     '1'           VABCBO
021200020619     C* ... e poi verifico se sono stati passati come parametri
021300020619     C                   IF        vlrppt > *blanks
021400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABCCM
021900020619     C                   Z-ADD     PiVal         VATCCM
022000020619     C                   ENDIF
022100040506     C                   ENDIF
022200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022400020619     C                   EXSR      CHKNUM
022500020619     C                   IF        PiInt=*on
022600020619     C                   Z-ADD     PiVal         VABLNP
022700020619     C                   Z-ADD     PiVal         VATLNP
022800040506     C                   ENDIF
022900020619     C                   ENDIF
023000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023200020619     C                   EXSR      CHKNUM
023300020619     C                   IF        PiInt=*on
023400020619     C                   Z-ADD     PiVal         VABCTR
023500040506     C                   ENDIF
023600020619     C                   ENDIF
023700120213     C                   IF        %subst(vlrppt:14:3) <> *blanks
023800120213     C                   EVAL      VABNZD = %trim(%subst(vlrppt:14:3))
023900120213     C                   ENDIF
024000020619     C                   ENDIF
024100000801     C*
024200000801     C                   ENDSR
024300000801     C*----------------------------------------------------*
024400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024500000801     C*----------------------------------------------------*
024600040823     C     IMPVAB        BEGSR
024700010305     C*
024800000801     C                   Z-ADD     *zeros        errore            1 0
024900000830     C                   MOVEL     datcor        VABAAS
025000020305     C                   MOVEL     datcor        VATAAS
025100040526     C                   MOVE      datcor        VABMGS
025200040823     C                   MOVE(P)   vlrpoi        VABFGS
025300040823     C                   MOVE(P)   vlrpoi        VATFGS
025400080409     C*
025500080409     C* Reperimento campi ALFA
025600080409     C*
025700080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
025800090331     C                   EVAL      VABRSD=%trim(%subst(vindta:3:30))
025900080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026000080409     C     '@':'A'       XLATE     VABRSD        VABRSD
026100080409     C* ==
026200090331     C                   EVAL      VABIND=%trim(%subst(vindta:33:25))
026300090331     C                   EVAL      VABLOD=%trim(%subst(vindta:63:30))
026400090331     C                   EVAL      VABPRD=%trim(%subst(vindta:98:2))
026500090403     C                   EVAL      VABRMA=%trim(%subst(vindta:191:10))
026600090331     C                   EVAL      VABNOT=%trim(%subst(vindta:209:35))
026700090331     C                   EVAL      VABNT2=%trim(%subst(vindta:209+35:50-35))
026800120214     C                   EVAL      VABNZD=%trim(%subst(vindta:328:3))
026900120213     C*
027000120213     C* Gestione particolare x export
027100120213     C                   IF        VABNZD <> *blanks
027200120214     C                   EVAL      VABNOT=%trim(%subst(vindta:293:35))
027300120213     C                   ENDIF
027400080409     C*
027500080409     C* Reperimento campi NUMERICI
027600081218     C
027700090403     C* NSP
027800090331     C                   EVAL      PiStr=%trim(%subst(vindta:191+4:10-4))
027900080409     C                   EXSR      CHKNUM
028000080409     C                   IF        PiInt=*on
028100080409     C                   Z-ADD     PiVal         VABNSP
028200080409     C                   Z-ADD     PiVal         VATNSP
028300080409     C                   ELSE
028400080409     C                   ADD       1             errore
028500080409     C                   Z-ADD     *zeros        VABNSP
028600090403     C                   Z-ADD     *zeros        VATNSP
028700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
028800090403     C                             + ' ' + 'VABNSP VATNSP'
028900080409     C                   ENDIF
029000090403     C* RMN
029100120216     C                   EVAL      PiStr=%trim(%subst(vindta:283+4:10-4))
029200090403     C                   EXSR      CHKNUM
029300090403     C                   IF        PiInt=*on
029400090403     C                   Z-ADD     PiVal         VABRMN
029500090403     C                   ELSE
029600090403     C                   ADD       1             errore
029700090403     C                   Z-ADD     1             VABRMN
029800090403     C                   EVAL      vinmsg = %trimr(vinmsg)
029900090403     C                             + ' ' + 'VABRMN'
030000090403     C                   ENDIF
030100080409     C* CAD
030200090331     C                   EVAL      PiStr=%trim(%subst(vindta:93:5))
030300080409     C                   EXSR      CHKNUM
030400080409     C                   IF        PiInt=*on
030500080409     C                   Z-ADD     PiVal         Num5_0
030600080409     C                   MOVEL(p)  Num5_0        VABCAD
030700080409     C                   ELSE
030800080409     C                   ADD       1             errore
030900080409     C                   EVAL      VABCAD = *zeros
031000080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031100080409     C                             + ' ' + 'VABCAD'
031200080409     C                   ENDIF
031300080409     C* NCL
031400090331     C                   EVAL      PiStr=%trim(%subst(vindta:100:4))
031500080409     C                   EXSR      CHKNUM
031600080409     C                   IF        PiInt=*on
031700080409     C                   Z-ADD     PiVal         VABNCL
031800080409     C                   ELSE
031900080409     C                   ADD       1             errore
032000080409     C                   Z-ADD     *zeros        VABNCL
032100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032200080409     C                             + ' ' + 'VABNCL'
032300080409     C                   ENDIF
032400090107     C* DCR
032500090331     C                   IF        %subst(vindta:271:8) <> *blanks AND
032600090331     C                             %subst(vindta:271:8) <> *zeros
032700120224     C                   EVAL      PiStr=%subst(vindta:271:8)
032800120224     C***                EVAL      PiStr=%subst(vindta:275:4)+
032900120224     C***                                %subst(vindta:273:2)+
033000120224     C***                                %subst(vindta:271:2)
033100090107     C                   EXSR      CHKNUM
033200090107     C                   IF        PiInt=*on
033300090107     C                   Z-ADD     PiVal         VABDCR
033400090107     C                   ELSE
033500090107     C                   ADD       1             errore
033600090107     C                   Z-ADD     *zeros        VABDCR
033700090107     C                   EVAL      vinmsg = %trimr(vinmsg)
033800090107     C                             + ' ' + 'VABDCR'
033900090107     C                   ENDIF
034000090107     C                   ENDIF
034100090331     C* VLB
034200090331     C                   EVAL      PiStr=%trim(%subst(vindta:110:8))
034300090331     C                   EXSR      CHKNUM
034400090331     C                   IF        PiNum=*on
034500090331     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
034600090331     C                   Z-ADD(H)  PiVal         VABVLB
034700090331     C                   ELSE
034800090331     C                   ADD       1             errore
034900090331     C                   Z-ADD     *zeros        VABVLB
035000090331     C                   EVAL      vinmsg = %trimr(vinmsg)
035100090331     C                             + ' ' + 'VABVLB'
035200090331     C                   ENDIF
035300080409     C* PKB
035400090331     C                   EVAL      PiStr=%trim(%subst(vindta:118:8))
035500080409     C                   EXSR      CHKNUM
035600080409     C                   IF        PiNum=*on
035700090331     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
035800081218     C                   Z-ADD(H)  PiVal         VABPKB
035900080409     C                   ELSE
036000080409     C                   ADD       1             errore
036100080409     C                   Z-ADD     *zeros        VABPKB
036200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
036300080409     C                             + ' ' + 'VABPKB'
036400080409     C                   ENDIF
036500090107     C* CAS
036600090331     C                   IF        %subst(vindta:259:10) <> *blanks AND
036700090331     C                             %subst(vindta:259:10) <> *zeros
036800090107     C                   EVAL      FlgCAS = '1'
036900090107     C                   EVAL      VABVCA = 'EUR'
037000090331     C                   EVAL      PiStr=%trim(%subst(vindta:259:10))
037100090107     C                   EXSR      CHKNUM
037200090107     C                   IF        PiInt=*on
037300090331     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
037400090107     C                   Z-ADD     PiVal         VABCAS
037500090107     C                   ELSE
037600090107     C                   ADD       1             errore
037700090107     C                   Z-ADD     *zeros        VABCAS
037800090107     C                   EVAL      vinmsg = %trimr(vinmsg)
037900090107     C                             + ' ' + 'VABCAS'
038000090107     C                   ENDIF
038100090107     C                   ENDIF
038200071121     C*
038300080415     C* Carico l'estensioni A e B del FNVAT
038400080415     C                   exsr      exevata
038500071121     C                   exsr      exevatb
038600010205     C*
038700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038800040802     C                   IF        FlgCAS <> '0'
038900081118     C                   IF        VABCBO = '1'
039000010205     C                   EVAL      VABCBO = '4'
039100010205     C                   ENDIF
039200081118     C                   IF        VABCBO = '2'
039300081118     C                   EVAL      VABCBO = '6'
039400081118     C                   ENDIF
039500081118     C                   ENDIF
039600020305     C*
039700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039800011113     C                   EXSR      CHKIMPDIV
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
041400081118     C*
041500070102     C*
041600081118     C***                exsr      wrivat                                       => scarico VAT
041700070102     C*
041800070102     C                   ENDSR
041900080415     C*----------------------------------------------------*
042000080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
042100080415     C*----------------------------------------------------*
042200080415     C     EXEVATA       BEGSR
042300080415     C*
042400080415     C*
042500080415     C***                exsr      wrivat                                       => scarico VAT
042600080415     C*
042700080415     C                   ENDSR
042800071121     C*----------------------------------------------------*
042900071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
043000071121     C*----------------------------------------------------*
043100071121     C     EXEVATB       BEGSR
043200071121     C*
043300090331     C                   eval      VATTRC='B'
043400090512     C                   eval      VATTRC=%trim(%subst(vindta:138:20)) + ' ' +
043500090512     C                                    %trim(%subst(vindta:405:20))
043600071121     C*
043700090331     C                   exsr      wrivat                                       => scarico VAT
043800071121     C*
043900071121     C                   ENDSR
044000010201     C*----------------------------------------------------*
044100040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044200010201     C*----------------------------------------------------*
044300020305     C     WRIVAT        BEGSR
044400081118     C*
044500081118     C* Imposto gli stessi campi chiave della testata relativa
044600081118     C                   EVAL      VATAAS = VABAAS
044700081118     C                   EVAL      VATLNP = VABLNP
044800081118     C                   EVAL      VATNRS = VABNRS
044900081118     C                   EVAL      VATNSP = VABNSP
045000081118     C                   EVAL      VATCCM = VABCCM
045100050628     C*
045200060223     C* Scrivo solo se valorizzato qualcosa
045300060223     C                   IF        VATNOT <> *blanks
045400040802     C                   WRITE     FIVAT000
045500060223     C                   ENDIF
045600010201     C*
045700010201     C                   ENDSR
045800010202     C*----------------------------------------------------*
045900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046000010202     C*----------------------------------------------------*
046100020305     C     PREVAT        BEGSR
046200010202     C*
046300021113     C* Compongo il nome del membro da dare al FIVATWWR
046400010202     C                   eval      parmbr = vlrhdl
046500010202     C                   movel     'M'           parmbr
046600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
046700010202     C                   eval      paropz = '1'
046800010202     C* Effettuo la chiamata al CLLE preposto
046900040506     C                   call(e)   'TITVVTC'
047000010202     C                   parm                    parccm
047100010202     C                   parm                    parmbr
047200010202     C                   parm                    paropz
047300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047400010202     C                   if        %error
047500010202     C                   movel     '1'           chkcall
047600010202     C                   else
047700010202     C                   movel     '0'           chkcall
047800010202     C                   endif
047900010202     C*
048000010202     C                   ENDSR
048100000801     C*----------------------------------------------------*
048200000801     C*  CONTROLLO NUMERICITA' CAMPI
048300000801     C*----------------------------------------------------*
048400000801     C     CHKNUM        BEGSR
048500081118     C*
048600081118     C                   IF        PiDecChr = *blanks
048700081118     C                   EVAL      PiDecChr = ','
048800081118     C                   ENDIF
048900081118     C*
049000081118     C                   callp(e)  UBISNUM_Check(PiStr
049100081118     C                                          :PiDecChr
049200081118     C                                          :PiVal
049300081118     C                                          :PiNum
049400081118     C                                          :PiInt)
049500081118     C*
049600081118     C                   IF        %error
049700081118     C                   EVAL      PiInt=*off
049800081118     C                   ENDIF
049900000801     C*
050000000801     C                   ENDSR
050100000801     C***
050200000801
050300011113
050400011113     C*----------------------------------------------------*
050500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050600011113     C*----------------------------------------------------*
050700011113     C     CHKIMPDIV     BEGSR
050800011113     C*
050900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051000011113     C                   Z-ADD     *zeros        wrkDec            9 9
051100011113     C*
051200011113     C* Come prima cosa effettuo considerazioni sulla divisa
051300011113     C                   IF        vabIAS > *zeros
051400011113     C                   IF        vabVAS <> 'EUR'
051500011113     C                   EVAL      vabVAS =  'ITL'
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C                   IF        vabCAS > *zeros
052000011113     C                   IF        vabVCA <> 'EUR'
052100011113     C                   EVAL      vabVCA =  'ITL'
052200011113     C                   ENDIF
052300011113     C                   ENDIF
052400011113     C*
052500011113     C                   IF        vabVMD > *zeros
052600020305     C                   IF        vabVAD <> 'EUR'
052700011113     C                   EVAL      vabVAD =  'ITL'
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053200011113     C                   Z-ADD     vabIAS        wrkDec
053300011113     C                   IF        wrkDec > *zeros
053400011113     C                   IF        vabVAS = 'ITL'
053500011113     C                   EVAL      vabIAS = *zeros
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054000011113     C                   Z-ADD     vabCAS        wrkDec
054100011113     C                   IF        wrkDec > *zeros
054200011113     C                   IF        vabVCA = 'ITL'
054300011113     C                   EVAL      vabCAS = *zeros
054400011113     C                   ENDIF
054500011113     C                   ENDIF
054600011113     C*
054700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054800011113     C                   Z-ADD     vabVMD        wrkDec
054900011113     C                   IF        wrkDec > *zeros
055000011113     C                   IF        vabVAD = 'ITL'
055100011113     C                   EVAL      vabVMD = *zeros
055200011113     C                   ENDIF
055300011113     C                   ENDIF
055400011113     C*
055500011113     C                   ENDSR
055600011113     C***
055700011113
055800011113
055900000801
056000000801
056100990920      /TITLE Invio dei dati al punto operativo.
056200010202     C     invio         BEGSR
056300990920     C*
056400021113     C* 1° invio FIVAT
056500010201     C                   reset                   dscmz
056600010201     C                   move      vlrpoi        cmzdst
056700021113     C                   eval      cmzfld = 'FIVATWWR'
056800010201     C                   eval      cmzmbd = vlrhdl
056900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057000021009     C***                if        prmfir = *blanks
057100021113     C                   eval      cmzfla = 'FIVAT00F'
057200021113     C                   eval      cmzmba = 'FIVAT00F'
057300021009     C***                else
057400021009     C***                eval      cmzfla = prmfir
057500021009     C***                eval      cmzmba = prmfir
057600021009     C***                endif
057700010201     C                   eval      cmznrr = *zeros
057800020305     C                   move      §ctrokvt      cmznrr
057900021018     C                   eval      cmzlba = vlrfl1
058000010201     C                   call(e)   'TIS711C'
058100010201     C                   parm                    dscmz
058200010201     C                   parm      *blanks       esito
058300010205     C                   if        %error
058400010205     C                             or cmzerr = '1'
058500010205     C                             or esito  = '1'
058600010205     C                   eval      wrkesito = '3'
058700010205     C                   else
058800010201     C*
058900021113     C* 2° invio FIVAB
059000010201     C                   reset                   dscmz
059100010201     C                   move      vlrpoi        cmzdst
059200010201     C                   eval      cmzfld = vlrfou
059300010201     C                   eval      cmzmbd = vlrhdl
059400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059500021009     C***                if        prmfir = *blanks
059600021113     C                   eval      cmzfla = 'FIVAB00F'
059700021113     C                   eval      cmzmba = 'FIVAB00F'
059800021009     C***                else
059900021009     C***                eval      cmzfla = prmfir
060000021009     C***                eval      cmzmba = prmfir
060100021009     C***                endif
060200010201     C                   eval      cmznrr = *zeros
060300010201     C                   move      §ctrokvb      cmznrr
060400021018     C                   eval      cmzlba = vlrfl1
060500010201     C                   call(e)   'TIS711C'
060600010201     C                   parm                    dscmz
060700010201     C                   parm      *blanks       esito
060800010201     C                   if        %error
060900010201     C                             or cmzerr = '1'
061000010201     C                             or esito  = '1'
061100010201     C                   eval      wrkesito = '3'
061200010201     C                   endif
061300010205     C                   endif
061400990920     C*
061500000613     C                   ENDSR
061600000613     C***
061700070411
061800070411     C     *pssr         BEGSR
061900070411     C*
062000070411     C                   if        %open(tivin00r)
062100070411     C                   close     tivin00r
062200070411     C                   endif
062300070411     C                   if        %open(fivabwwr)
062400070411     C                   close     fivabwwr
062500070411     C                   endif
062600070411     C                   if        %open(fivatwwr)
062700070411     C                   close     fivatwwr
062800070411     C                   endif
062900070411     C*
063000070411     C* Effettuo la chiamata al CLLE preposto
063100070411     C                   call(e)   'TITVVTC'
063200070411     C                   parm                    parccm
063300070411     C                   parm                    parmbr
063400070411     C                   parm      '2'           paropz
063500070411     C*
063600070411     C                   eval      wrkesito = '2'
063700070411     C*
063800070411     C                   seton                                        LR
063900070411     C*
064000070411     C                   ENDSR     '*CANCL'
064100070411     C***
064200070411
064300990910
064400000613     C     *inzsr        BEGSR
064500990910     C*
064600990910     C     *entry        plist
064700990920     C                   parm                    tivlrds
064800990921     C                   parm      wrkesito      esito
064900000724     C                   parm                    prmlit
065000000710     C                   parm                    prmfir
065100000613     C*
065200000830     C* CALCOLA LA DATA CORRENTE
065300000830     C                   time                    wn14             14 0
065400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
065500000830     C                   z-add     wn8           g08dat
065600000830     C                   z-add     *zeros        g08inv
065700000830     C                   movel     '0'           g08err
065800000830     C                   call      'XSRDA8'
065900000830     C                   parm                    wlbda8
066000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066100000830     C*
066200000613     C                   ENDSR
