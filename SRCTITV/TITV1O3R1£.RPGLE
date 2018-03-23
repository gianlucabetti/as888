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
026600130802     C                   EVAL      VABNOT=%trim(%subst(vindta:388:35))
026700130802     C                   EVAL      VABNT2=%trim(%subst(vindta:388+35:50-35))
026800130802     C***                EVAL      VABNZD=%trim(%subst(vindta:328:3))
026900120213     C*
027000120213     C* Gestione particolare x export
027100120213     C                   IF        VABNZD <> *blanks
027200130802     C***                EVAL      VABNOT=%trim(%subst(vindta:293:35))
027300120213     C                   ENDIF
027400130802     C*
027500130802     C* Gestione appuntamento
027600130802     C                   IF        %subst(vindta:387:1) = 'S'
027700130802     C                   EVAL      VABTC1='A'
027800130802     C                   ENDIF
027900080409     C*
028000080409     C* Reperimento campi NUMERICI
028100081218     C
028200090403     C* NSP
028300090331     C                   EVAL      PiStr=%trim(%subst(vindta:191+4:10-4))
028400080409     C                   EXSR      CHKNUM
028500080409     C                   IF        PiInt=*on
028600080409     C                   Z-ADD     PiVal         VABNSP
028700080409     C                   Z-ADD     PiVal         VATNSP
028800080409     C                   ELSE
028900080409     C                   ADD       1             errore
029000080409     C                   Z-ADD     *zeros        VABNSP
029100090403     C                   Z-ADD     *zeros        VATNSP
029200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029300090403     C                             + ' ' + 'VABNSP VATNSP'
029400080409     C                   ENDIF
029500090403     C* RMN
029600130802     C                   EVAL      PiStr=%trim(%subst(vindta:321:15))
029700090403     C                   EXSR      CHKNUM
029800090403     C                   IF        PiInt=*on
029900090403     C                   Z-ADD     PiVal         VABRMN
030000090403     C                   ELSE
030100090403     C                   ADD       1             errore
030200090403     C                   Z-ADD     1             VABRMN
030300090403     C                   EVAL      vinmsg = %trimr(vinmsg)
030400090403     C                             + ' ' + 'VABRMN'
030500090403     C                   ENDIF
030600080409     C* CAD
030700090331     C                   EVAL      PiStr=%trim(%subst(vindta:93:5))
030800080409     C                   EXSR      CHKNUM
030900080409     C                   IF        PiInt=*on
031000080409     C                   Z-ADD     PiVal         Num5_0
031100080409     C                   MOVEL(p)  Num5_0        VABCAD
031200080409     C                   ELSE
031300080409     C                   ADD       1             errore
031400080409     C                   EVAL      VABCAD = *zeros
031500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031600080409     C                             + ' ' + 'VABCAD'
031700080409     C                   ENDIF
031800080409     C* NCL
031900090331     C                   EVAL      PiStr=%trim(%subst(vindta:100:4))
032000080409     C                   EXSR      CHKNUM
032100080409     C                   IF        PiInt=*on
032200080409     C                   Z-ADD     PiVal         VABNCL
032300080409     C                   ELSE
032400080409     C                   ADD       1             errore
032500080409     C                   Z-ADD     *zeros        VABNCL
032600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032700080409     C                             + ' ' + 'VABNCL'
032800080409     C                   ENDIF
032900090107     C* DCR
033000130802     C                   IF        %subst(vindta:313:8) <> *blanks AND
033100130802     C                             %subst(vindta:313:8) <> *zeros
033200130802     C                   EVAL      PiStr=%subst(vindta:313:8)
033300120224     C***                EVAL      PiStr=%subst(vindta:275:4)+
033400120224     C***                                %subst(vindta:273:2)+
033500120224     C***                                %subst(vindta:271:2)
033600090107     C                   EXSR      CHKNUM
033700090107     C                   IF        PiInt=*on
033800130802     C* la data arriva in formato ggmmaaaa e va girata in aaaammgg
033900131122     C***                EVAL      VABDCR = %dec(%subst(vindta:313:2):2:0) +
034000131122     C***                           %dec(%subst(vindta:313+2:2):2:0)*100 +
034100131122     C***                           %dec(%subst(vindta:313+4:4):4:0)*1000
034101131122     C* la data arriva in già formato aaaammgg
034200131122     C                   Z-ADD     PiVal         VABDCR
034300090107     C                   ELSE
034400090107     C                   ADD       1             errore
034500090107     C                   Z-ADD     *zeros        VABDCR
034600090107     C                   EVAL      vinmsg = %trimr(vinmsg)
034700090107     C                             + ' ' + 'VABDCR'
034800090107     C                   ENDIF
034900090107     C                   ENDIF
035000090331     C* VLB
035100090331     C                   EVAL      PiStr=%trim(%subst(vindta:110:8))
035200090331     C                   EXSR      CHKNUM
035300090331     C                   IF        PiNum=*on
035400090331     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
035500090331     C                   Z-ADD(H)  PiVal         VABVLB
035600090331     C                   ELSE
035700090331     C                   ADD       1             errore
035800090331     C                   Z-ADD     *zeros        VABVLB
035900090331     C                   EVAL      vinmsg = %trimr(vinmsg)
036000090331     C                             + ' ' + 'VABVLB'
036100090331     C                   ENDIF
036200080409     C* PKB
036300090331     C                   EVAL      PiStr=%trim(%subst(vindta:118:8))
036400080409     C                   EXSR      CHKNUM
036500080409     C                   IF        PiNum=*on
036600090331     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
036700081218     C                   Z-ADD(H)  PiVal         VABPKB
036800080409     C                   ELSE
036900080409     C                   ADD       1             errore
037000080409     C                   Z-ADD     *zeros        VABPKB
037100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
037200080409     C                             + ' ' + 'VABPKB'
037300080409     C                   ENDIF
037400090107     C* CAS
037500090331     C                   IF        %subst(vindta:259:10) <> *blanks AND
037600090331     C                             %subst(vindta:259:10) <> *zeros
037700090107     C                   EVAL      FlgCAS = '1'
037800090107     C                   EVAL      VABVCA = 'EUR'
037900090331     C                   EVAL      PiStr=%trim(%subst(vindta:259:10))
038000090107     C                   EXSR      CHKNUM
038100090107     C                   IF        PiInt=*on
038200090331     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
038300090107     C                   Z-ADD     PiVal         VABCAS
038400090107     C                   ELSE
038500090107     C                   ADD       1             errore
038600090107     C                   Z-ADD     *zeros        VABCAS
038700090107     C                   EVAL      vinmsg = %trimr(vinmsg)
038800090107     C                             + ' ' + 'VABCAS'
038900090107     C                   ENDIF
039000090107     C                   ENDIF
039100071121     C*
039200080415     C* Carico l'estensioni A e B del FNVAT
039300080415     C                   exsr      exevata
039400071121     C                   exsr      exevatb
039500010205     C*
039600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039700040802     C                   IF        FlgCAS <> '0'
039800081118     C                   IF        VABCBO = '1'
039900010205     C                   EVAL      VABCBO = '4'
040000010205     C                   ENDIF
040100081118     C                   IF        VABCBO = '2'
040200081118     C                   EVAL      VABCBO = '6'
040300081118     C                   ENDIF
040400081118     C                   ENDIF
040500020305     C*
040600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040700011113     C                   EXSR      CHKIMPDIV
040800010202     C*
040900000801     C* Ebbene...
041000000801     C                   ADD       1             §CTRMO
041100010201     C                   IF        errore <> *zeros
041200000801     C                   ADD       1             §CTRNO
041300000801     C                   EVAL      vinflg = '2'
041400000801     C                   ELSE
041500010201     C                   ADD       1             §CTROKVB
041600000801     C                   ENDIF
041700000801     C*
041800000801     C                   ENDSR
041900070102     C*----------------------------------------------------*
042000070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
042100070102     C*----------------------------------------------------*
042200070103     C     EXEVATE       BEGSR
042300081118     C*
042400070102     C*
042500081118     C***                exsr      wrivat                                       => scarico VAT
042600070102     C*
042700070102     C                   ENDSR
042800080415     C*----------------------------------------------------*
042900080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
043000080415     C*----------------------------------------------------*
043100080415     C     EXEVATA       BEGSR
043200080415     C*
043300130802     C                   eval      VATTRC='A'
043400130802     C                   eval      VATNOT=%trim(%subst(vindta:351:15))
043500080415     C*
043600130802     C                   exsr      wrivat                                       => scarico VAT
043700080415     C*
043800080415     C                   ENDSR
043900071121     C*----------------------------------------------------*
044000071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
044100071121     C*----------------------------------------------------*
044200071121     C     EXEVATB       BEGSR
044300071121     C*
044400090331     C                   eval      VATTRC='B'
044500130802     C                   eval      VATNOT=%trim(%subst(vindta:366:20))
044600071121     C*
044700090331     C                   exsr      wrivat                                       => scarico VAT
044800071121     C*
044900071121     C                   ENDSR
045000010201     C*----------------------------------------------------*
045100040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
045200010201     C*----------------------------------------------------*
045300020305     C     WRIVAT        BEGSR
045400081118     C*
045500081118     C* Imposto gli stessi campi chiave della testata relativa
045600081118     C                   EVAL      VATAAS = VABAAS
045700081118     C                   EVAL      VATLNP = VABLNP
045800081118     C                   EVAL      VATNRS = VABNRS
045900081118     C                   EVAL      VATNSP = VABNSP
046000081118     C                   EVAL      VATCCM = VABCCM
046100050628     C*
046200060223     C* Scrivo solo se valorizzato qualcosa
046300060223     C                   IF        VATNOT <> *blanks
046400040802     C                   WRITE     FIVAT000
046500060223     C                   ENDIF
046600010201     C*
046700010201     C                   ENDSR
046800010202     C*----------------------------------------------------*
046900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047000010202     C*----------------------------------------------------*
047100020305     C     PREVAT        BEGSR
047200010202     C*
047300021113     C* Compongo il nome del membro da dare al FIVATWWR
047400010202     C                   eval      parmbr = vlrhdl
047500010202     C                   movel     'M'           parmbr
047600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
047700010202     C                   eval      paropz = '1'
047800010202     C* Effettuo la chiamata al CLLE preposto
047900040506     C                   call(e)   'TITVVTC'
048000010202     C                   parm                    parccm
048100010202     C                   parm                    parmbr
048200010202     C                   parm                    paropz
048300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048400010202     C                   if        %error
048500010202     C                   movel     '1'           chkcall
048600010202     C                   else
048700010202     C                   movel     '0'           chkcall
048800010202     C                   endif
048900010202     C*
049000010202     C                   ENDSR
049100000801     C*----------------------------------------------------*
049200000801     C*  CONTROLLO NUMERICITA' CAMPI
049300000801     C*----------------------------------------------------*
049400000801     C     CHKNUM        BEGSR
049500081118     C*
049600081118     C                   IF        PiDecChr = *blanks
049700081118     C                   EVAL      PiDecChr = ','
049800081118     C                   ENDIF
049900081118     C*
050000081118     C                   callp(e)  UBISNUM_Check(PiStr
050100081118     C                                          :PiDecChr
050200081118     C                                          :PiVal
050300081118     C                                          :PiNum
050400081118     C                                          :PiInt)
050500081118     C*
050600081118     C                   IF        %error
050700081118     C                   EVAL      PiInt=*off
050800081118     C                   ENDIF
050900000801     C*
051000000801     C                   ENDSR
051100000801     C***
051200000801
051300011113
051400011113     C*----------------------------------------------------*
051500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051600011113     C*----------------------------------------------------*
051700011113     C     CHKIMPDIV     BEGSR
051800011113     C*
051900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052000011113     C                   Z-ADD     *zeros        wrkDec            9 9
052100011113     C*
052200011113     C* Come prima cosa effettuo considerazioni sulla divisa
052300011113     C                   IF        vabIAS > *zeros
052400011113     C                   IF        vabVAS <> 'EUR'
052500011113     C                   EVAL      vabVAS =  'ITL'
052600011113     C                   ENDIF
052700011113     C                   ENDIF
052800011113     C*
052900011113     C                   IF        vabCAS > *zeros
053000011113     C                   IF        vabVCA <> 'EUR'
053100011113     C                   EVAL      vabVCA =  'ITL'
053200011113     C                   ENDIF
053300011113     C                   ENDIF
053400011113     C*
053500011113     C                   IF        vabVMD > *zeros
053600020305     C                   IF        vabVAD <> 'EUR'
053700011113     C                   EVAL      vabVAD =  'ITL'
053800011113     C                   ENDIF
053900011113     C                   ENDIF
054000011113     C*
054100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054200011113     C                   Z-ADD     vabIAS        wrkDec
054300011113     C                   IF        wrkDec > *zeros
054400011113     C                   IF        vabVAS = 'ITL'
054500011113     C                   EVAL      vabIAS = *zeros
054600011113     C                   ENDIF
054700011113     C                   ENDIF
054800011113     C*
054900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055000011113     C                   Z-ADD     vabCAS        wrkDec
055100011113     C                   IF        wrkDec > *zeros
055200011113     C                   IF        vabVCA = 'ITL'
055300011113     C                   EVAL      vabCAS = *zeros
055400011113     C                   ENDIF
055500011113     C                   ENDIF
055600011113     C*
055700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055800011113     C                   Z-ADD     vabVMD        wrkDec
055900011113     C                   IF        wrkDec > *zeros
056000011113     C                   IF        vabVAD = 'ITL'
056100011113     C                   EVAL      vabVMD = *zeros
056200011113     C                   ENDIF
056300011113     C                   ENDIF
056400011113     C*
056500011113     C                   ENDSR
056600011113     C***
056700011113
056800011113
056900000801
057000000801
057100990920      /TITLE Invio dei dati al punto operativo.
057200010202     C     invio         BEGSR
057300990920     C*
057400021113     C* 1° invio FIVAT
057500010201     C                   reset                   dscmz
057600010201     C                   move      vlrpoi        cmzdst
057700021113     C                   eval      cmzfld = 'FIVATWWR'
057800010201     C                   eval      cmzmbd = vlrhdl
057900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058000021009     C***                if        prmfir = *blanks
058100021113     C                   eval      cmzfla = 'FIVAT00F'
058200021113     C                   eval      cmzmba = 'FIVAT00F'
058300021009     C***                else
058400021009     C***                eval      cmzfla = prmfir
058500021009     C***                eval      cmzmba = prmfir
058600021009     C***                endif
058700010201     C                   eval      cmznrr = *zeros
058800020305     C                   move      §ctrokvt      cmznrr
058900021018     C                   eval      cmzlba = vlrfl1
059000010201     C                   call(e)   'TIS711C'
059100010201     C                   parm                    dscmz
059200010201     C                   parm      *blanks       esito
059300010205     C                   if        %error
059400010205     C                             or cmzerr = '1'
059500010205     C                             or esito  = '1'
059600010205     C                   eval      wrkesito = '3'
059700010205     C                   else
059800010201     C*
059900021113     C* 2° invio FIVAB
060000010201     C                   reset                   dscmz
060100010201     C                   move      vlrpoi        cmzdst
060200010201     C                   eval      cmzfld = vlrfou
060300010201     C                   eval      cmzmbd = vlrhdl
060400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060500021009     C***                if        prmfir = *blanks
060600021113     C                   eval      cmzfla = 'FIVAB00F'
060700021113     C                   eval      cmzmba = 'FIVAB00F'
060800021009     C***                else
060900021009     C***                eval      cmzfla = prmfir
061000021009     C***                eval      cmzmba = prmfir
061100021009     C***                endif
061200010201     C                   eval      cmznrr = *zeros
061300010201     C                   move      §ctrokvb      cmznrr
061400021018     C                   eval      cmzlba = vlrfl1
061500010201     C                   call(e)   'TIS711C'
061600010201     C                   parm                    dscmz
061700010201     C                   parm      *blanks       esito
061800010201     C                   if        %error
061900010201     C                             or cmzerr = '1'
062000010201     C                             or esito  = '1'
062100010201     C                   eval      wrkesito = '3'
062200010201     C                   endif
062300010205     C                   endif
062400990920     C*
062500000613     C                   ENDSR
062600000613     C***
062700070411
062800070411     C     *pssr         BEGSR
062900070411     C*
063000070411     C                   if        %open(tivin00r)
063100070411     C                   close     tivin00r
063200070411     C                   endif
063300070411     C                   if        %open(fivabwwr)
063400070411     C                   close     fivabwwr
063500070411     C                   endif
063600070411     C                   if        %open(fivatwwr)
063700070411     C                   close     fivatwwr
063800070411     C                   endif
063900070411     C*
064000070411     C* Effettuo la chiamata al CLLE preposto
064100070411     C                   call(e)   'TITVVTC'
064200070411     C                   parm                    parccm
064300070411     C                   parm                    parmbr
064400070411     C                   parm      '2'           paropz
064500070411     C*
064600070411     C                   eval      wrkesito = '2'
064700070411     C*
064800070411     C                   seton                                        LR
064900070411     C*
065000070411     C                   ENDSR     '*CANCL'
065100070411     C***
065200070411
065300990910
065400000613     C     *inzsr        BEGSR
065500990910     C*
065600990910     C     *entry        plist
065700990920     C                   parm                    tivlrds
065800990921     C                   parm      wrkesito      esito
065900000724     C                   parm                    prmlit
066000000710     C                   parm                    prmfir
066100000613     C*
066200000830     C* CALCOLA LA DATA CORRENTE
066300000830     C                   time                    wn14             14 0
066400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066500000830     C                   z-add     wn8           g08dat
066600000830     C                   z-add     *zeros        g08inv
066700000830     C                   movel     '0'           g08err
066800000830     C                   call      'XSRDA8'
066900000830     C                   parm                    wlbda8
067000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067100000830     C*
067200000613     C                   ENDSR
