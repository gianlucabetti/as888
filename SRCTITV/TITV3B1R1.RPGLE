000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200170927     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300990908
000400171003     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600060307     D tisi95ds      e ds
001700171003     D ds15          e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700150225     D curSped         s              7    INZ(*blanks)
002800150225     D depSped         s              7    INZ(*blanks)
002900171003     D wNAZ            s              2    INZ(*blanks)
003000171003     D VABCCMDPD       s                   INZ(*zeros)  LIKE(VABCCM)
003100171003     D VABLNPDPD       s                   INZ(*zeros)  LIKE(VABLNP)
003200171003     D VABCTRDPD       s                   INZ(*zeros)  LIKE(VABCTR)
003300171005     D VABCTMDPD       s                   INZ(*blanks) LIKE(VABCTM)
003400171005     D wCurRec         s              7  0 INZ(*zeros)
003500171005     D oracor          s              6  0 INZ(*zeros)
003600180215     D wEsteroCon      s              1    inz(*blank)
003700100118
003800100118     D*------------------
003900100118     D* DS REPERIMENTO NUMERATORE
004000100118     D*------------------
004100100118     D trul33ds      e ds                  inz
004200100118     D*------------------
004300100118     D* DS ARCHITETTURA
004400100118     D*------------------
004500100118     D kpjba         e ds                  inz
004600100118
004700171003     D*------------
004800171003     D jNAZ            s              5  0 INZ(*zeros)
004900171003     D skNAZISO        S              2    DIM(1000)
005000171003     D skNAZBAR        S              3    DIM(1000)
005100010201
005200010201
005300090127     C*
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000613     C*
005800171003     C                   exsr      cartab
005900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006000000613     C*
006100010202     C* Effettuo la chiamata al CLLE preposto
006200040506     C                   call(e)   'TITVVTC'
006300010202     C                   parm                    parccm
006400010202     C                   parm                    parmbr
006500010202     C                   parm      '2'           paropz
006600050201     C*
006700050201     C* Effettuo lancio TISI95 solo x chiusura
006800050201     C                   CLEAR                   TISI95DS
006900050201     C                   EVAL      I95TLA = 'C'
007000050201     C                   CALL      'TISI95R'
007100050201     C                   PARM                    TISI95DS
007200000616     C*
007300000801     C
007400010201     C                   seton                                        LR
007500990908
007600000801
007700910830     C*--------------------------------------------------------
007800040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007900910830     C*--------------------------------------------------------
008000040526     C     RWFILE        BEGSR
008100990910     C*
008200990914     C                   if        not %open(tivin00r)
008300990908     C                   open      tivin00r
008400990914     C                   endif
008500021113     C                   if        not %open(fivabwwr)
008600021113     C                   open      fivabwwr
008700990914     C                   endif
008800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008900020305     C                   exsr      prevat
009000010201     C*
009100010202     C                   if        chkcall = '0'
009200010202     C*
009300021113     C                   if        not %open(fivatwwr)
009400021113     C                   open      fivatwwr
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
011600070402     C*
011700070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011800171109     C***                eval      curSped = %subst(vindta:46+1:5-1)
011900171109     C                   eval      curSped = %subst(vindta:46+1:5)
012000180215     C* Raggruppo per Italia, ma anche per Estero se richiesto trasporto EEX e non DPD
012100171003     C                   if        curSped <> depSped OR
012200180215     C* purtroppo la prima volta non ho ancora preparato i flag appositi, per cui devo guardare
012300180215     C* direttamente i dati
012400180215     C                             (%subst(vindta:615:2)<>'IT' and
012500180215     C                              %subst(vlrppt:33:1) = 'D')
012600070402     C                   clear                   fivab000
012700070402     C                   clear                   fivat000
012800050628     C*
012900060315     C                   exsr      impvab                                       => carico  VAB
013000090127     C*
013100060315     C                   exsr      wrivab                                       => scarico VAB
013200070402     C*
013300070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
013400140304     C                   exsr      exevata
013500070402     C*
013600070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
013700070402     C                   exsr      exevatb
013800140226     C*
013900140226     C* Indirizzo e-mail
014000140226     C                   exsr      exevati
014100140226     C*
014200140226     C* Cellulare per SMS
014300140226     C                   exsr      exevats
014400070402     C*
014500070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
014600070402     C                   exsr      exevate
014700070402     C*
014800070402     C* Salvo il raggruppamento spedizione cliente corrente
014900070402     C                   eval      depSped = curSped
015000070402     C*
015100070402     C                   else
015200070402     C*
015300070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
015400070402     C                   exsr      exevate
015500070402     C                   endif
015600000905     C*
015700000905     C                   else
015800000905     C                   eval      vinflg = '1'
015900050628     C                   endif
016000000905     C                   endif
016100000905     C*
016200000905     C  N70              update    tivin000
016300000905     C*
016400991022     C  N70              ENDdo
016500010202     C*
016600010202     C                   endif
016700990910
016800990910     C* Se non ci sono record con errori ...
016900000710     C                   if        §ctrno = 0
017000990910     C* ... restituisco esito OK.
017100990921     C                   eval      wrkesito = '0'
017200990910     C                   else
017300010201     C                   if        §ctrokvb > 0
017400990921     C                   eval      wrkesito = '1'
017500000710     C                   else
017600000710     C                   eval      wrkesito = '2'
017700990910     C                   endif
017800000710     C                   endif
017900990910     C*
018000990914     C                   if        %open(tivin00r)
018100990908     C                   close     tivin00r
018200990914     C                   endif
018300021113     C                   if        %open(fivabwwr)
018400021113     C                   close     fivabwwr
018500990914     C                   endif
018600021113     C                   if        %open(fivatwwr)
018700021113     C                   close     fivatwwr
018800010201     C                   endif
018900990910     C*
019000010201     C                   if        §ctrokvb > 0
019100000724     C                             and vlrpoi <> *zeros
019200010202     C                   exsr      invio
019300990920     C                   endif
019400990920     C*
019500910830     C                   ENDSR
019600000613     C***
019700010305
019800010305     C*----------------------------------------------------*
019900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
020000010305     C*----------------------------------------------------*
020100090127     C     WRIVAB        BEGSR
020200090127     C*
020300090127     C* Quindi scarico il buffer del file d testata
020400090127     C                   write     fivab000                                     => scarico il VAB
020500010305     C*
020600010305     C                   ENDSR
020700990920
020800000801     C*----------------------------------------------------*
020900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021000000801     C*----------------------------------------------------*
021100010201     C     INZVAR        BEGSR
021200000801     C*
021300040802     C                   Z-ADD     *zeros        Num5_0            5 0
021400040802     C                   MOVEL     '0'           FlgCAS            1
021500000801     C*
021600000801     C                   ENDSR
021700000801     C*----------------------------------------------------*
021800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021900000801     C*----------------------------------------------------*
022000000801     C     DEFCAM        BEGSR
022100070718     C*
022200070718     C* Imposto i valori di default...
022300170927     C                   Z-ADD     2103003       VABCCM
022400170927     C                   Z-ADD     2103003       VATCCM
022500170927     C                   Z-ADD     210           VABLNP
022600170927     C                   Z-ADD     210           VATLNP
022700170927     C                   Z-ADD     000           VABCTR
022800150225     C                   MOVEL     '1'           VABCBO
022900150225     C                   MOVEL     'C'           VABTSP
023000140224     C                   MOVEL     '7Q'          VABCTM
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
025600140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
025700140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
025800140224     C                   ENDIF
025900171003     C                   IF        %subst(vlrppt:18:7) <> *blanks
026000171003     C                   EVAL      PiStr=%trim(%subst(vlrppt:18:7))
026100171003     C                   EXSR      CHKNUM
026200171003     C                   IF        PiInt=*on
026300171003     C                   Z-ADD     PiVal         VABCCMDPD
026400171003     C                   ENDIF
026500171003     C                   ENDIF
026600171003     C                   IF        %subst(vlrppt:25:3) <> *blanks
026700171003     C                   EVAL      PiStr=%trim(%subst(vlrppt:25:3))
026800171003     C                   EXSR      CHKNUM
026900171003     C                   IF        PiInt=*on
027000171003     C                   Z-ADD     PiVal         VABLNPDPD
027100171003     C                   ENDIF
027200171003     C                   ENDIF
027300171003     C                   IF        %subst(vlrppt:28:3) <> *blanks
027400171003     C                   EVAL      PiStr=%trim(%subst(vlrppt:28:3))
027500171003     C                   EXSR      CHKNUM
027600171003     C                   IF        PiInt=*on
027700171003     C                   Z-ADD     PiVal         VABCTRDPD
027800171003     C                   ENDIF
027900171003     C                   ENDIF
028000171005     C                   IF        %subst(vlrppt:31:2) <> *blanks
028100171005     C                   EVAL      VABCTMDPD = %subst(vlrppt:31:2)
028200171005     C                   ENDIF
028300180215     C* flag ' '=DPD (se estero) 'E'=EEX
028400180215     C                   IF        %subst(vlrppt:33:1) <> *blanks
028500180215     C                   SELECT
028600180215     C                   WHEN      %subst(vlrppt:33:1) ='E'
028700180215     C                   EVAL      wEsteroCon='E'
028800180215     C                   WHEN      %subst(vlrppt:33:1) =*blank
028900180215     C                   EVAL      wEsteroCon='D'
029000180215     C                   ENDSL
029100180215     C                   ENDIF
029200020619     C                   ENDIF
029300000801     C*
029400000801     C                   ENDSR
029500000801     C*----------------------------------------------------*
029600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029700000801     C*----------------------------------------------------*
029800040823     C     IMPVAB        BEGSR
029900040823     C*
030000020305     C                   EXSR      INZVAR
030100020305     C                   EXSR      DEFCAM
030200010305     C*
030300000801     C                   Z-ADD     *zeros        errore            1 0
030400000830     C                   MOVEL     datcor        VABAAS
030500020305     C                   MOVEL     datcor        VATAAS
030600040526     C                   MOVE      datcor        VABMGS
030700040823     C                   MOVE(P)   vlrpoi        VABFGS
030800040823     C                   MOVE(P)   vlrpoi        VATFGS
030900050628     C*
031000140221     C                   EVAL      VABRSD=%trim(%subst(vindta:276:35))
031100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031300020117     C* ==
031400140221     C                   EVAL      VABIND=%trim(%subst(vindta:341:35))
031500070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
031600171109     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
031700171109     C***                EVAL      VABRMA=%trim(%subst(vindta:46:5))
031800171109     C                   EVAL      VABRMA=%trim(%subst(vindta:46:6))
031900171003     C* NZD
032000171003     C                   EVAL      VABNZD=%trim(%subst(vindta:615:2))
032100180215     C* nel pgm TITV3B1R la gestione estera è DPD per cui monocollo
032200180215     C* in questo pgm    la gestione estera è EEX per cui multicollo
032300171003     C                   IF        VABNZD = 'IT'
032400171003     C                   MOVEL     '1'           wNTW              1
032500180215     C***                SETOFF                                       55
032600171003     C                   CLEAR                   VABNZD
032700171003     C                   ELSE
032800180215     C                   EVAL      wNAZ = %trim(VABNZD)
032900180215     C                   Z-ADD     1             jNAZ
033000180215     C     wNAZ          LOOKUP    skNAZISO(jNAZ)                         13
033100180215     C                   IF        %found
033200180215     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
033300180215     C                   ENDIF
033400180215     C*
033500180215     C                   SELECT
033600180215     C                   WHEN      wEsteroCon = 'D'
033700171003     C                   MOVEL     '2'           wNTW
033800180215     C***                SETON                                        55
033900171005     C                   ADD       1             wCurRec
034000171005     C                   EVAL      oracor = %dec(%time() : *iso)
034100171003     C                   EVAL      VABCCM = VABCCMDPD
034200171003     C                   EVAL      VABLNP = VABLNPDPD
034300171003     C                   EVAL      VABCTR = VABCTRDPD
034400171005     C                   EVAL      VABCTM = VABCTMDPD
034500171005     C                   EVAL      VABPRD = *blanks
034600180215     C                   WHEN      wEsteroCon = 'E'
034700180215     C                   MOVEL     '3'           wNTW
034800180215     C                   EVAL      VABCCM = VABCCMDPD
034900180215     C                   EVAL      VABLNP = VABLNPDPD
035000180215     C                   EVAL      VABCTR = VABCTRDPD
035100180215     C                   EVAL      VABCTM = VABCTMDPD
035200180215     C                   EVAL      VABPRD = *blanks
035300180215     C                   ENDSL
035400180215     C*
035500171003     C                   ENDIF
035600050628     C* CAP
035700140224     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
035800010201     C                   EXSR      CHKNUM
035900010201     C                   IF        PiInt=*on
036000140224     C                   Z-ADD     PiVal         Num5_0
036100040506     C                   MOVEL(P)  Num5_0        VABCAD
036200010201     C                   ELSE
036300040506     C                   ADD       1             errore
036400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036500040506     C                             + ' ' + 'VABCAD'
036600010201     C                   ENDIF
036700040506     C* Reperisco la provincia dal CAP e dalla località
036800040526     C                   IF        VABCAD <> *blanks AND
036900040526     C                             VABPRD  = *blanks
037000040506     C                   CLEAR                   TISI95DS
037100040506     C                   EVAL      I95TCN = '3'
037200040506     C                   Z-ADD     datcor        I95DAT
037300040506     C                   EVAL      I95CAP = VABCAD
037400040506     C                   EVAL      I95LOC = VABLOD
037500050627     C                   EVAL      I95NAR = VABNZD
037600040506     C                   CALL      'TISI95R'
037700040506     C                   PARM                    TISI95DS
037800040506     C                   EVAL      VABPRD = O95PRV
037900040506     C                   ENDIF
038000040506     C* PKB
038100150225     C                   EVAL      PiStr=%trim(%subst(vindta:435:5))
038200010201     C                   EXSR      CHKNUM
038300010201     C                   IF        PiNum=*on
038400150225     C                   EVAL      PiVal=PiVal/10                               kg con 1 decimale
038500060316     C                   Z-ADD     PiVal         VABPKB
038600010201     C                   ELSE
038700010201     C                   ADD       1             errore
038800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038900010201     C                             + ' ' + 'VABPKB'
039000010201     C                   ENDIF
039100150225     C* RMN
039200171109     C***                EVAL      PiStr=%trim(%subst(vindta:46+1:5-1))
039300171109     C                   EVAL      PiStr=%trim(%subst(vindta:46+1:5))
039400090511     C                   EXSR      CHKNUM
039500090511     C                   IF        PiInt=*on
039600090511     C                   Z-ADD     PiVal         VABRMN
039700090511     C                   ELSE
039800090511     C                   ADD       1             errore
039900090511     C                   EVAL      vinmsg = %trimr(vinmsg)
040000150225     C                             + ' ' + 'VABRMN'
040100090511     C                   ENDIF
040200150225     C* NSP
040300180215     C                   SELECT
040400180215     C* DPD
040500180215     C                   WHEN      wNTW = '2'
040600180215     C                   EVAL      VABNSP=oracor+wCurRec
040700180215     C                   EVAL      VATNSP=oracor+wCurRec
040800180215     C* Italia
040900180215     C                   WHEN      wNTW = '1'
041000180215     C                   EVAL      PiStr=wNTW+%trim(%subst(vindta:46+1:5))
041100180215     C                   EXSR      CHKNUM
041200180215     C                   IF        PiInt=*on
041300150225     C                   Z-ADD     PiVal         VABNSP
041400150225     C                   Z-ADD     PiVal         VATNSP
041500150225     C                   ELSE
041600150225     C                   ADD       1             errore
041700150225     C                   EVAL      vinmsg = %trimr(vinmsg)
041800150225     C                             + ' ' + 'VABNSP VATNSP'
041900150225     C                   ENDIF
042000180215     C* EEX
042100180215     C                   WHEN      wNTW = '3'
042200180215     C                   EVAL      PiStr=wNTW+%trim(%subst(vindta:46+1:5))
042300180215     C                   EXSR      CHKNUM
042400180215     C                   IF        PiInt=*on
042500180215     C                   Z-ADD     PiVal         VABNSP
042600180215     C                   Z-ADD     PiVal         VATNSP
042700180215     C                   ELSE
042800180215     C                   ADD       1             errore
042900180215     C                   EVAL      vinmsg = %trimr(vinmsg)
043000180215     C                             + ' ' + 'VABNSP VATNSP'
043100180215     C                   ENDIF
043200180215     C                   ENDSL
043300150225     C* NCL
043400150225     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
043500150225     C                   EXSR      CHKNUM
043600150225     C                   IF        PiInt=*on
043700180215     C                   SELECT
043800180215     C* Italia è multicollo
043900180215     C                   WHEN      wNTW = '1'
044000180215     C                   Z-ADD     PiVal         VABNCL
044100180215     C* DPD è monocollo
044200180215     C                   WHEN      wNTW = '2'
044300180215     C                   Z-ADD     1             VABNCL
044400180215     C* EEX è multicollo
044500180215     C                   WHEN      wNTW = '3'
044600180215     C                   Z-ADD     PiVal         VABNCL
044700180215     C                   ENDSL
044800150225     C                   ELSE
044900150225     C                   ADD       1             errore
045000150225     C                   EVAL      vinmsg = %trimr(vinmsg)
045100150225     C                             + ' ' + 'VABNCL'
045200150225     C                   ENDIF
045300060307     C* CAS
045400170927     C                   IF        %trim(%subst(vindta:502:9))<>'000000,00' AND
045500170927     C                             %trim(%subst(vindta:502:9))<>'000000.00' AND
045600170927     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
045700170927     C                             %trim(%subst(vindta:502:9))<>*zeros
045800170927     C                   EVAL      VABVCA = 'EUR'
045900170927     C                   EVAL      FlgCAS = '1'
046000170927     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
046100170927     C                   EXSR      CHKNUM
046200170927     C                   IF        PiNum=*on
046300170927     C                   Z-ADD     PiVal         VABCAS
046400170927     C                   ELSE
046500170927     C                   ADD       1             errore
046600170927     C                   EVAL      vinmsg = %trimr(vinmsg)
046700170927     C                             + ' ' + 'VABCAS'
046800170927     C                   ENDIF
046900170927     C                   ENDIF
047000010205     C*
047100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
047200040802     C                   IF        FlgCAS <> '0'
047300070102     C                   IF        VABCBO = '1'
047400010205     C                   EVAL      VABCBO = '4'
047500010205     C                   ELSE
047600070102     C                   EVAL      VABCBO = '6'
047700070102     C                   ENDIF
047800010205     C                   ENDIF
047900020305     C*
048000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048100011113     C                   EXSR      CHKIMPDIV
048200010202     C*
048300000801     C* Ebbene...
048400000801     C                   ADD       1             §CTRMO
048500010201     C                   IF        errore <> *zeros
048600000801     C                   ADD       1             §CTRNO
048700000801     C                   EVAL      vinflg = '2'
048800000801     C                   ELSE
048900010201     C                   ADD       1             §CTROKVB
049000000801     C                   ENDIF
049100000801     C*
049200000801     C                   ENDSR
049300070102     C*----------------------------------------------------*
049400070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
049500070102     C*----------------------------------------------------*
049600070102     C     EXEVATA       BEGSR
049700070102     C*
049800070102     C                   EXSR      INZVAR
049900070102     C                   EXSR      DEFCAM
050000070102     C*
050100070102     C                   EVAL      VATTRC='A'
050200150225     C                   EVAL      VATNOT = %trim(%subst(vindta:316:25))
050300070102     C                   exsr      wrivat                                       => scarico VAT
050400070102     C*
050500070102     C                   ENDSR
050600060307     C*----------------------------------------------------*
050700060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
050800060307     C*----------------------------------------------------*
050900060307     C     EXEVATB       BEGSR
051000060307     C*
051100060307     C                   EXSR      INZVAR
051200060307     C                   EXSR      DEFCAM
051300060307     C*
051400060307     C                   EVAL      VATTRC='B'
051500150225     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
051600060307     C                   exsr      wrivat                                       => scarico VAT
051700060307     C*
051800060307     C                   ENDSR
051900070402     C*----------------------------------------------------*
052000070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
052100070402     C*----------------------------------------------------*
052200070402     C     EXEVATE       BEGSR
052300070402     C*
052400070402     C                   EXSR      INZVAR
052500070402     C                   EXSR      DEFCAM
052600070402     C*
052700070402     C                   EVAL      VATTRC='E'
052800150225     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
052900070402     C                   exsr      wrivat                                       => scarico VAT
053000070402     C*
053100070402     C                   ENDSR
053200140224     C*----------------------------------------------------*
053300140224     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
053400140224     C*----------------------------------------------------*
053500140224     C     EXEVATI       BEGSR
053600170927     C*
053700170927     C                   EXSR      INZVAR
053800170927     C                   EXSR      DEFCAM
053900170927     C
054000170927     C                   EVAL      VATTRC='I'
054100170927     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
054200170927     C                   exsr      wrivat                                       => scarico VAT
054300170927     C
054400170927     C                   EVAL      VATTRC='J'
054500170927     C                   EVAL      VATNOT = %trim(%subst(vindta:663+35:15))
054600170927     C                   exsr      wrivat                                       => scarico VAT
054700170927     C*
054800140224     C                   ENDSR
054900140226     C*----------------------------------------------------*
055000140226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
055100140226     C*----------------------------------------------------*
055200140226     C     EXEVATS       BEGSR
055300150225     C***
055400150225     C***                EXSR      INZVAR
055500150225     C***                EXSR      DEFCAM
055600150225     C***
055700150225     C***                EVAL      VATTRC='S'
055800150225     C***                EVAL      VATNOT=*blank
055900150225     C***                EVAL      %subst(VATNOT:1:14) =
056000150225     C***                           %trim(%subst(vindta:713:14))
056100150225     C***                IF        VATNOT <> *blank
056200150225     C***                EVAL      %subst(VATNOT:17:1) = 'S'
056300150225     C***                ENDIF
056400150225     C***                exsr      wrivat                                       => scarico VAT
056500150225     C***
056600140226     C                   ENDSR
056700010201     C*----------------------------------------------------*
056800040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
056900010201     C*----------------------------------------------------*
057000020305     C     WRIVAT        BEGSR
057100171005     C*
057200180215     C                   SELECT
057300180215     C* Per l'estero uso il secondo codice
057400180215     C                   WHEN      wNTW = '2'
057500180215     C                             or wNTW = '3'
057600180215     C                   EVAL      VATCCM = VABCCMDPD
057700180215     C                   EVAL      VATLNP = VABLNPDPD
057800180215     C* Per l'Italia è già impostato
057900180215     C                   ENDSL
058000050628     C*
058100060223     C* Scrivo solo se valorizzato qualcosa
058200060223     C                   IF        VATNOT <> *blanks
058300040802     C                   WRITE     FIVAT000
058400060223     C                   ENDIF
058500010201     C*
058600010201     C                   ENDSR
058700010202     C*----------------------------------------------------*
058800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
058900010202     C*----------------------------------------------------*
059000020305     C     PREVAT        BEGSR
059100010202     C*
059200021113     C* Compongo il nome del membro da dare al FIVATWWR
059300010202     C                   eval      parmbr = vlrhdl
059400010202     C                   movel     'M'           parmbr
059500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
059600010202     C                   eval      paropz = '1'
059700010202     C* Effettuo la chiamata al CLLE preposto
059800040506     C                   call(e)   'TITVVTC'
059900010202     C                   parm                    parccm
060000010202     C                   parm                    parmbr
060100010202     C                   parm                    paropz
060200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
060300010202     C                   if        %error
060400010202     C                   movel     '1'           chkcall
060500010202     C                   else
060600010202     C                   movel     '0'           chkcall
060700010202     C                   endif
060800010202     C*
060900010202     C                   ENDSR
061000000801     C*----------------------------------------------------*
061100000801     C*  CONTROLLO NUMERICITA' CAMPI
061200000801     C*----------------------------------------------------*
061300000801     C     CHKNUM        BEGSR
061400000801     C*
061500000801     C                   call(e)   'ISNUMERIC'
061600000801     C                   PARM                    PiStr            30
061700140226     C                   PARM      '.'           PiDecChr          1
061800000801     C                   PARM      *ZEROS        PiVal            30 9
061900000801     C                   PARM      '0'           PiInt             1
062000000801     C                   PARM      '0'           PiNum             1
062100000801     C                   IF        %error
062200000801     C                   EVAL      PiInt=*off
062300000801     C                   ENDIF
062400000801     C*
062500000801     C                   ENDSR
062600000801     C***
062700000801
062800011113
062900011113     C*----------------------------------------------------*
063000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
063100011113     C*----------------------------------------------------*
063200011113     C     CHKIMPDIV     BEGSR
063300011113     C*
063400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063500011113     C                   Z-ADD     *zeros        wrkDec            9 9
063600011113     C*
063700011113     C* Come prima cosa effettuo considerazioni sulla divisa
063800011113     C                   IF        vabIAS > *zeros
063900011113     C                   IF        vabVAS <> 'EUR'
064000011113     C                   EVAL      vabVAS =  'ITL'
064100011113     C                   ENDIF
064200011113     C                   ENDIF
064300011113     C*
064400011113     C                   IF        vabCAS > *zeros
064500011113     C                   IF        vabVCA <> 'EUR'
064600011113     C                   EVAL      vabVCA =  'ITL'
064700011113     C                   ENDIF
064800011113     C                   ENDIF
064900011113     C*
065000011113     C                   IF        vabVMD > *zeros
065100020305     C                   IF        vabVAD <> 'EUR'
065200011113     C                   EVAL      vabVAD =  'ITL'
065300011113     C                   ENDIF
065400011113     C                   ENDIF
065500011113     C*
065600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
065700011113     C                   Z-ADD     vabIAS        wrkDec
065800011113     C                   IF        wrkDec > *zeros
065900011113     C                   IF        vabVAS = 'ITL'
066000011113     C                   EVAL      vabIAS = *zeros
066100011113     C                   ENDIF
066200011113     C                   ENDIF
066300011113     C*
066400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
066500011113     C                   Z-ADD     vabCAS        wrkDec
066600011113     C                   IF        wrkDec > *zeros
066700011113     C                   IF        vabVCA = 'ITL'
066800011113     C                   EVAL      vabCAS = *zeros
066900011113     C                   ENDIF
067000011113     C                   ENDIF
067100011113     C*
067200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
067300011113     C                   Z-ADD     vabVMD        wrkDec
067400011113     C                   IF        wrkDec > *zeros
067500011113     C                   IF        vabVAD = 'ITL'
067600011113     C                   EVAL      vabVMD = *zeros
067700011113     C                   ENDIF
067800011113     C                   ENDIF
067900011113     C*
068000011113     C                   ENDSR
068100011113     C***
068200011113
068300011113
068400000801
068500000801
068600990920      /TITLE Invio dei dati al punto operativo.
068700010202     C     invio         BEGSR
068800990920     C*
068900021113     C* 1° invio FIVAT
069000010201     C                   reset                   dscmz
069100010201     C                   move      vlrpoi        cmzdst
069200021113     C                   eval      cmzfld = 'FIVATWWR'
069300010201     C                   eval      cmzmbd = vlrhdl
069400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069500021009     C***                if        prmfir = *blanks
069600021113     C                   eval      cmzfla = 'FIVAT00F'
069700021113     C                   eval      cmzmba = 'FIVAT00F'
069800021009     C***                else
069900021009     C***                eval      cmzfla = prmfir
070000021009     C***                eval      cmzmba = prmfir
070100021009     C***                endif
070200010201     C                   eval      cmznrr = *zeros
070300020305     C                   move      §ctrokvt      cmznrr
070400021018     C                   eval      cmzlba = vlrfl1
070500010201     C                   call(e)   'TIS711C'
070600010201     C                   parm                    dscmz
070700010201     C                   parm      *blanks       esito
070800010205     C                   if        %error
070900010205     C                             or cmzerr = '1'
071000010205     C                             or esito  = '1'
071100010205     C                   eval      wrkesito = '3'
071200010205     C                   else
071300010201     C*
071400021113     C* 2° invio FIVAB
071500010201     C                   reset                   dscmz
071600010201     C                   move      vlrpoi        cmzdst
071700010201     C                   eval      cmzfld = vlrfou
071800010201     C                   eval      cmzmbd = vlrhdl
071900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072000021009     C***                if        prmfir = *blanks
072100021113     C                   eval      cmzfla = 'FIVAB00F'
072200021113     C                   eval      cmzmba = 'FIVAB00F'
072300021009     C***                else
072400021009     C***                eval      cmzfla = prmfir
072500021009     C***                eval      cmzmba = prmfir
072600021009     C***                endif
072700010201     C                   eval      cmznrr = *zeros
072800010201     C                   move      §ctrokvb      cmznrr
072900021018     C                   eval      cmzlba = vlrfl1
073000010201     C                   call(e)   'TIS711C'
073100010201     C                   parm                    dscmz
073200010201     C                   parm      *blanks       esito
073300010201     C                   if        %error
073400010201     C                             or cmzerr = '1'
073500010201     C                             or esito  = '1'
073600010201     C                   eval      wrkesito = '3'
073700010201     C                   endif
073800010205     C                   endif
073900990920     C*
074000000613     C                   ENDSR
074100000613     C***
074200171003
074300171003
074400171003
074500171003     C*--------------------------------------------------------
074600171003     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
074700171003     C*--------------------------------------------------------
074800171003     C     CARTAB        BEGSR
074900171003     C*
075000171003     C* TABELLA '15' - NAZIONI
075100171003     C                   clear                   skNAZISO
075200171003     C                   clear                   skNAZBAR
075300171003     C                   eval      tblKUT = 1
075400171003     C                   eval      tblCOD = '15'
075500171003     C     KEYtabP       setll     tabel00f
075600171003     C     KEYtabP       reade     tabel00f
075700171003     C                   dow       not %eof(tabel00f)
075800171003     C                   if        tblFLG = *blanks
075900171003     C                   movel(p)  tblUNI        ds15
076000171003     C                   add       1             jNAZ
076100171003     C                   eval      skNAZBAR(jNAZ) = tblKEY
076200171005     C                   eval      skNAZISO(jNAZ) = %trim(§15COD)
076300171003     C                   endif
076400171003     C     KEYtabP       reade     tabel00f
076500171003     C                   enddo
076600171003     C*
076700171003     C                   ENDSR
076800171003     C***
076900171003
077000171003
077100990910
077200000613     C     *inzsr        BEGSR
077300990910     C*
077400990910     C     *entry        plist
077500990920     C                   parm                    tivlrds
077600990921     C                   parm      wrkesito      esito
077700000724     C                   parm                    prmlit
077800000710     C                   parm                    prmfir
077900000613     C*
078000000830     C* CALCOLA LA DATA CORRENTE
078100100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078200100118     C                   eval      datcor = %dec(%date() : *ISO)
078300171003     C*
078400171003     C* Chiave su TABEL00F - parziale
078500171003     C     KEYtabP       KLIST
078600171003     C                   KFLD                    tblKUT
078700171003     C                   KFLD                    tblCOD
078800000830     C*
078900000613     C                   ENDSR
079000000613     C***
