000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130206     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*caller)
000300990908
000400130206     Ftabel00f  if   e           k disk
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
001700130206     D ds15          e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700081120     D curSped         s              6    INZ(*blanks)
002800081120     D depSped         s              6    INZ(*blanks)
002900130206     D jNAZ            s              5  0 INZ(*zeros)
003000130206
003100130206     D*------------
003200130206     D* SCHIERE
003300130206     D*------------
003400130206     D skNAZISO        S              3    DIM(1000)
003500130206     D skNAZBAR        S              3    DIM(1000)
003600081118
003700081118     D*------------------
003800081118     D* LINKING A DEFINIZIONI ESTERNE
003900081118     D*------------------
004000081118     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004100110421     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004200081118
004300081118
004400010201
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800081121     C*
004900081121     C* SALVO IL P.O. INVIO INIZIALE
005000081121     C                   z-add     vlrpoi        sv_vlrpoi         3 0
005100000613     C*
005200130206     C                   EXSR      CARTAB                                       CARICA TABELLE
005300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005400000613     C*
005500010202     C* Effettuo la chiamata al CLLE preposto
005600040506     C                   call(e)   'TITVVTC'
005700010202     C                   parm                    parccm
005800010202     C                   parm                    parmbr
005900010202     C                   parm      '2'           paropz
006000050201     C*
006100050201     C* Effettuo lancio TISI95 solo x chiusura
006200050201     C                   CLEAR                   TISI95DS
006300050201     C                   EVAL      I95TLA = 'C'
006400050201     C                   CALL      'TISI95R'
006500050201     C                   PARM                    TISI95DS
006600000616     C*
006700000801     C
006800010201     C                   seton                                        LR
006900990908
007000000801
007100910830     C*--------------------------------------------------------
007200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007300910830     C*--------------------------------------------------------
007400040526     C     RWFILE        BEGSR
007500990910     C*
007600990914     C                   if        not %open(tivin00r)
007700990908     C                   open      tivin00r
007800990914     C                   endif
007900021113     C                   if        not %open(fivabwwr)
008000021113     C                   open      fivabwwr
008100990914     C                   endif
008200070103     C*
008300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008400020305     C                   exsr      prevat
008500010201     C*
008600010202     C                   if        chkcall = '0'
008700010202     C*
008800021113     C                   if        not %open(fivatwwr)
008900021113     C                   open      fivatwwr
009000010201     C                   endif
009100990910     C*
009200010201     C                   clear                   §CTROKVB          5 0
009300020305     C                   clear                   §CTROKVT          5 0
009400000801     C                   clear                   §CTRMO            5 0
009500000801     C                   clear                   §CTRNO            5 0
009600990910     C*
009700921023     C                   DO        *HIVAL
009800990913     C*
009900990915     C                   READ      tivin00r                               70
010000050627     C                   if        vindta > *blanks
010100000613     C                   add       1             rrnum
010200000801     C*
010300000801     C                   if        *in70 = *off
010400000801     C                             and
010500000801     C                             (vinflg = *blanks
010600000801     C                              or vinflg = '0'
010700000801     C                              or vinflg = '2')
010800000801     C*
010900000801     C                   clear                   vinmsg
011000000801     C                   eval      vinflg = '1'
011100070103     C*
011200070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011300110421     C                   eval      curSped = %subst(vindta:202:6)
011400070213     C                   if        curSped <> depSped
011500070213     C* Se prima bolla => importo bolla corrente
011600070213     C                   if        §CTRMO = *zeros
011700070213     C                   exsr      inzvar
011800070213     C                   exsr      defcam
011900070213     C                   exsr      impvab                                       => carico  VAB
012000070213     C                   else
012100070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
012200070213     C                   exsr      wrivab                                       => scarico VAB
012300070213     C                   exsr      inzvar
012400070213     C                   exsr      defcam
012500070213     C                   exsr      impvab                                       => carico  VAB
012600070213     C                   endif
012700071121     C                   exsr      exevate                                      => write VAT-E
012800070103     C* Salvo il raggruppamento spedizione cliente corrente
012900070103     C                   eval      depSped = curSped
013000070213     C*
013100070213     C* Se collo successivo x stessa bolla
013200070213     C                   else
013300070213     C                   exsr      impvab                                       => carico  VAB
013400070213     C                   exsr      exevate                                      => write VAT-E
013500070103     C                   endif
013600000905     C*
013700000905     C                   else
013800000905     C                   eval      vinflg = '1'
013900050628     C                   endif
014000000905     C                   endif
014100000905     C*
014200000905     C  N70              update    tivin000
014300000905     C*
014400991022     C  N70              ENDdo
014500070213     C*
014600070213     C* Scarico testata bolla rimasta in sospesa
014700070213     C                   exsr      wrivab                                       => scarico VAB
014800010202     C*
014900010202     C                   endif
015000990910
015100990910     C* Se non ci sono record con errori ...
015200000710     C                   if        §ctrno = 0
015300990910     C* ... restituisco esito OK.
015400990921     C                   eval      wrkesito = '0'
015500990910     C                   else
015600010201     C                   if        §ctrokvb > 0
015700990921     C                   eval      wrkesito = '1'
015800000710     C                   else
015900000710     C                   eval      wrkesito = '2'
016000990910     C                   endif
016100000710     C                   endif
016200990910     C*
016300990914     C                   if        %open(tivin00r)
016400990908     C                   close     tivin00r
016500990914     C                   endif
016600021113     C                   if        %open(fivabwwr)
016700021113     C                   close     fivabwwr
016800990914     C                   endif
016900021113     C                   if        %open(fivatwwr)
017000021113     C                   close     fivatwwr
017100010201     C                   endif
017200990910     C*
017300010201     C                   if        §ctrokvb > 0
017400000724     C                             and vlrpoi <> *zeros
017500010202     C                   exsr      invio
017600990920     C                   endif
017700990920     C*
017800910830     C                   ENDSR
017900000613     C***
018000010305
018100010305     C*----------------------------------------------------*
018200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018300010305     C*----------------------------------------------------*
018400020305     C     WRIVAB        BEGSR
018500010305     C*
018600060225     C* Quindi scarico il buffer del file d testata
018700021113     C                   write     fivab000                                     => scarico il VAB
018800010305     C*
018900010305     C                   ENDSR
019000990920
019100000801     C*----------------------------------------------------*
019200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019300000801     C*----------------------------------------------------*
019400010201     C     INZVAR        BEGSR
019500000801     C*
019600070213     C                   CLEAR                   FIVAB000
019700070213     C                   CLEAR                   FIVAT000
019800070213     C*
019900040802     C                   Z-ADD     *zeros        Num5_0            5 0
020000040802     C                   MOVEL     '0'           FlgCAS            1
020100000801     C*
020200000801     C                   ENDSR
020300000801     C*----------------------------------------------------*
020400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020500000801     C*----------------------------------------------------*
020600000801     C     DEFCAM        BEGSR
020700000801     C*
020800020619     C* Imposto i valori di default...
020900110421     C                   Z-ADD     2750814       VABCCM
021000110421     C                   Z-ADD     2750814       VATCCM
021100110421     C                   Z-ADD     275           VABLNP
021200110421     C                   Z-ADD     275           VATLNP
021300081118     C                   Z-ADD     000           VABCTR
021400070103     C                   MOVEL     '7Q'          VABCTM
021500040823     C                   MOVEL     '1'           VABCBO
021600081121     C                   MOVEL     'C'           VABTSP
021700020619     C* ... e poi verifico se sono stati passati come parametri
021800020619     C                   IF        vlrppt > *blanks
021900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABCCM
022400020619     C                   Z-ADD     PiVal         VATCCM
022500020619     C                   ENDIF
022600040506     C                   ENDIF
022700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022900020619     C                   EXSR      CHKNUM
023000020619     C                   IF        PiInt=*on
023100020619     C                   Z-ADD     PiVal         VABLNP
023200020619     C                   Z-ADD     PiVal         VATLNP
023300040506     C                   ENDIF
023400020619     C                   ENDIF
023500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023700020619     C                   EXSR      CHKNUM
023800020619     C                   IF        PiInt=*on
023900020619     C                   Z-ADD     PiVal         VABCTR
024000040506     C                   ENDIF
024100020619     C                   ENDIF
024200020619     C                   ENDIF
024300000801     C*
024400000801     C                   ENDSR
024500000801     C*----------------------------------------------------*
024600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024700000801     C*----------------------------------------------------*
024800040823     C     IMPVAB        BEGSR
024900010305     C*
025000000801     C                   Z-ADD     *zeros        errore            1 0
025100000830     C                   MOVEL     datcor        VABAAS
025200020305     C                   MOVEL     datcor        VATAAS
025300040526     C                   MOVE      datcor        VABMGS
025400040823     C                   MOVE(P)   vlrpoi        VABFGS
025500040823     C                   MOVE(P)   vlrpoi        VATFGS
025600080409     C*
025700080409     C* Reperimento campi ALFA
025800080409     C*
025900080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
026000110421     C                   EVAL      VABRSD=%trim(%subst(vindta:216:40))
026100080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026200080409     C     '@':'A'       XLATE     VABRSD        VABRSD
026300080409     C* ==
026400110421     C                   EVAL      VABRD2=%trim(%subst(vindta:256:41))
026500110421     C                   EVAL      VABIND=%trim(%subst(vindta:297:35))
026600110421     C                   EVAL      VABLOD=%trim(%subst(vindta:341:30))
026700130206     C* per la nazione consideriamo il dato come la nazione ISO e poi la
026800130206     C* sostituiamo con la nostra
026900130208     C* siccome per l'Italia viene passato il codice ITA che non è ISO,
027000130208     C* se VABNZD = ITA , lo sblankiamo
027100130206     C                   EVAL      VABNZD=%trim(%subst(vindta:373:3))
027200130206     C                   Z-ADD     1             jNAZ
027300130206     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
027400130206     C                   IF        %found
027500130206     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
027600130206     C                   ENDIF
027700130208     C                   IF        VABNZD='ITA'
027800130208     C                   EVAL      VABNZD = *blank
027900130208     C                   ENDIF
028000110421     C                   EVAL      VABPRD=%trim(%subst(vindta:371:2))
028100110421     C                   EVAL      VABRMA=%trim(%subst(vindta:193:15))
028200081120     C                   MOVEL     *blanks       wNOTE            70
028300110421     C                   EVAL      wNOTE =%trim(%subst(vindta:436:50))+
028400110421     C                                    %trim(%subst(vindta:376:18))
028500081120     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
028600081118     C                   EVAL      VABNT2=%trim(%subst(wNOTE:35+1:35))
028700081120     C                   SELECT
028800110509     C                   WHEN      %subst(vindta:435:1) = 'A'
028900081120     C                   EVAL      VABCBO = '2'
029000081120     C                   ENDSL
029100080409     C*
029200080409     C* Reperimento campi NUMERICI
029300080409     C                   MOVEL     DATCOR        VABAAS
029400080409     C                   MOVE      DATCOR        VABMGS
029500081120     C* CCM
029600130312     C                   IF        %subst(vindta:1:7) <> *blanks
029700110421     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
029800081120     C                   EXSR      CHKNUM
029900081120     C                   IF        PiInt=*on
030000081120     C                   Z-ADD     PiVal         VABCCM
030100081120     C                   MOVEL     VABCCM        VABLNP
030200081121     C                   MOVEL     VABCCM        VATLNP
030300081121     C                   IF        sv_vlrpoi = 999
030400081121     C                   Z-ADD     VABLNP        VABFGS
030500081121     C                   Z-ADD     VATLNP        VATFGS
030600081121     C                   Z-ADD     VABFGS        vlrpoi
030700081121     C                   ENDIF
030800081120     C                   ELSE
030900081120     C                   ADD       1             errore
031000081120     C                   Z-ADD     *zeros        VABCCM
031100081120     C                   Z-ADD     *zeros        VABLNP
031200081120     C                   EVAL      vinmsg = %trimr(vinmsg)
031300081120     C                             + ' ' + 'VABCCM VABLNP'
031400081120     C                   ENDIF
031500130312     C                   ENDIF
031600130312     C*
031700130312     C* CCM => forzatura x gestione export
031800130312     C* il cliente mi dice se la spedizione è nazionale o estera
031900130312     C                   SELECT
032000130312     C                   WHEN      %trim(%subst(vindta:670:1)) = 'D'
032100130312     C                   EVAL      VABCTR = 353
032200130312     C                   EVAL      VABCCM = 0250285
032300130312     C                   EVAL      VATCCM = 0250285
032400130312     C                   WHEN      %trim(%subst(vindta:670:1)) = 'E'
032500130312     C                   EVAL      VABCTR = 052
032600130312     C                   EVAL      VABCCM = 2750814
032700130312     C                   EVAL      VATCCM = 2750814
032800130312     C                   ENDSL
032900130312     C                   SELECT
033000130312     C                   WHEN      %subst(vindta:539:3) = 'X'
033100130312     C                   EVAL      VABTIC='TM'
033200130312     C                   WHEN      %subst(vindta:539:3) = 'K'
033300130312     C                   EVAL      VABTIC='BM'
033400130312     C                   ENDSL
033500080409     C* NSP/RMN
033600110421     C                   EVAL      PiStr=%trim(%subst(vindta:202:6))
033700080409     C                   EXSR      CHKNUM
033800080409     C                   IF        PiInt=*on
033900080409     C                   Z-ADD     PiVal         VABNSP
034000080409     C                   Z-ADD     PiVal         VATNSP
034100080409     C                   Z-ADD     PiVal         VABRMN
034200080409     C                   ELSE
034300080409     C                   ADD       1             errore
034400080409     C                   Z-ADD     *zeros        VABNSP
034500080409     C                   Z-ADD     1             VABRMN
034600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
034700080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
034800080409     C                   ENDIF
034900080409     C* CAD
035000110421     C                   EVAL      PiStr=%trim(%subst(vindta:332:9))
035100080409     C                   EXSR      CHKNUM
035200080409     C                   IF        PiInt=*on
035300080409     C                   Z-ADD     PiVal         Num5_0
035400080409     C                   MOVEL(p)  Num5_0        VABCAD
035500080409     C                   ELSE
035600080409     C                   ADD       1             errore
035700130206     C***                EVAL      VABCAD = *zeros
035800080409     C                   EVAL      vinmsg = %trimr(vinmsg)
035900080409     C                             + ' ' + 'VABCAD'
036000080409     C                   ENDIF
036100080409     C* NCL
036200110421     C                   EVAL      PiStr=%trim(%subst(vindta:486:6))
036300080409     C                   EXSR      CHKNUM
036400080409     C                   IF        PiInt=*on
036500080409     C                   Z-ADD     PiVal         VABNCL
036600080409     C                   ELSE
036700080409     C                   ADD       1             errore
036800080409     C                   Z-ADD     *zeros        VABNCL
036900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
037000080409     C                             + ' ' + 'VABNCL'
037100080409     C                   ENDIF
037200080409     C* PKB
037300110421     C                   EVAL      PiStr=%trim(%subst(vindta:492:9))
037400080409     C                   EXSR      CHKNUM
037500080409     C                   IF        PiNum=*on
037600081120     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
037700081120     C                   Z-ADD(H)  PiVal         VABPKB
037800080409     C                   ELSE
037900080409     C                   ADD       1             errore
038000080409     C                   Z-ADD     *zeros        VABPKB
038100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
038200080409     C                             + ' ' + 'VABPKB'
038300080409     C                   ENDIF
038400081120     C* VLB
038500110421     C                   EVAL      PiStr=%trim(%subst(vindta:501:17))
038600081120     C                   EXSR      CHKNUM
038700081120     C                   IF        PiNum=*on
038800081120     C                   EVAL      PiVal = PiVal / 1000000                      * gestisco 6 dec.
038900081120     C                   Z-ADD(H)  PiVal         VABVLB
039000081120     C                   ELSE
039100081120     C                   ADD       1             errore
039200081120     C                   Z-ADD     *zeros        VABVLB
039300081120     C                   EVAL      vinmsg = %trimr(vinmsg)
039400081120     C                             + ' ' + 'VABVLB'
039500081120     C                   ENDIF
039600080409     C* CAS
039700110509     C                   IF        %subst(vindta:521:1) = 'Y'
039800080409     C                   EVAL      FlgCAS = '1'
039900080415     C                   EVAL      VABVCA = 'EUR'
040000110421     C                   EVAL      PiStr=%trim(%subst(vindta:522:17))
040100080409     C                   EXSR      CHKNUM
040200080409     C                   IF        PiNum=*on
040300090507     C                   EVAL      PiVal = PiVal / 10000                        * gestisco 4 dec.
040400090507     C                   Z-ADD(H)  PiVal         VABCAS
040500080409     C                   ELSE
040600080409     C                   ADD       1             errore
040700080409     C                   Z-ADD     *zeros        VABCAS
040800080409     C                   EVAL      vinmsg = %trimr(vinmsg)
040900080409     C                             + ' ' + 'VABCAS'
041000080409     C                   ENDIF
041100080409     C                   ENDIF
041200071121     C*
041300080415     C* Carico l'estensioni A e B del FNVAT
041400080415     C                   exsr      exevata
041500071121     C                   exsr      exevatb
041600010205     C*
041700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041800040802     C                   IF        FlgCAS <> '0'
041900081118     C                   IF        VABCBO = '1'
042000010205     C                   EVAL      VABCBO = '4'
042100010205     C                   ENDIF
042200081118     C                   IF        VABCBO = '2'
042300081118     C                   EVAL      VABCBO = '6'
042400081118     C                   ENDIF
042500081118     C                   ENDIF
042600020305     C*
042700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042800011113     C                   EXSR      CHKIMPDIV
042900010202     C*
043000000801     C* Ebbene...
043100000801     C                   ADD       1             §CTRMO
043200010201     C                   IF        errore <> *zeros
043300000801     C                   ADD       1             §CTRNO
043400000801     C                   EVAL      vinflg = '2'
043500000801     C                   ELSE
043600010201     C                   ADD       1             §CTROKVB
043700000801     C                   ENDIF
043800000801     C*
043900000801     C                   ENDSR
044000070102     C*----------------------------------------------------*
044100070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044200070102     C*----------------------------------------------------*
044300070103     C     EXEVATE       BEGSR
044400081118     C*
044500081120     C                   EVAL      VATTRC = 'E'
044600110421     C                   EVAL      VATNOT=%subst(vindta:652:18)
044700070102     C*
044800081120     C                   exsr      wrivat                                       => scarico VAT
044900070102     C*
045000070102     C                   ENDSR
045100080415     C*----------------------------------------------------*
045200080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
045300080415     C*----------------------------------------------------*
045400080415     C     EXEVATA       BEGSR
045500080415     C*
045600080415     C*
045700080415     C***                exsr      wrivat                                       => scarico VAT
045800080415     C*
045900080415     C                   ENDSR
046000071121     C*----------------------------------------------------*
046100071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
046200071121     C*----------------------------------------------------*
046300071121     C     EXEVATB       BEGSR
046400071121     C*
046500071121     C*
046600080409     C***                exsr      wrivat                                       => scarico VAT
046700071121     C*
046800071121     C                   ENDSR
046900010201     C*----------------------------------------------------*
047000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047100010201     C*----------------------------------------------------*
047200020305     C     WRIVAT        BEGSR
047300081118     C*
047400081118     C* Imposto gli stessi campi chiave della testata relativa
047500081216     C                   EVAL      VATFGS = VABFGS
047600081118     C                   EVAL      VATAAS = VABAAS
047700081118     C                   EVAL      VATLNP = VABLNP
047800081118     C                   EVAL      VATNRS = VABNRS
047900081118     C                   EVAL      VATNSP = VABNSP
048000081118     C                   EVAL      VATCCM = VABCCM
048100050628     C*
048200060223     C* Scrivo solo se valorizzato qualcosa
048300060223     C                   IF        VATNOT <> *blanks
048400040802     C                   WRITE     FIVAT000
048500060223     C                   ENDIF
048600010201     C*
048700010201     C                   ENDSR
048800010202     C*----------------------------------------------------*
048900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049000010202     C*----------------------------------------------------*
049100020305     C     PREVAT        BEGSR
049200010202     C*
049300021113     C* Compongo il nome del membro da dare al FIVATWWR
049400010202     C                   eval      parmbr = vlrhdl
049500010202     C                   movel     'M'           parmbr
049600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049700010202     C                   eval      paropz = '1'
049800010202     C* Effettuo la chiamata al CLLE preposto
049900040506     C                   call(e)   'TITVVTC'
050000010202     C                   parm                    parccm
050100010202     C                   parm                    parmbr
050200010202     C                   parm                    paropz
050300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050400010202     C                   if        %error
050500010202     C                   movel     '1'           chkcall
050600010202     C                   else
050700010202     C                   movel     '0'           chkcall
050800010202     C                   endif
050900010202     C*
051000010202     C                   ENDSR
051100000801     C*----------------------------------------------------*
051200000801     C*  CONTROLLO NUMERICITA' CAMPI
051300000801     C*----------------------------------------------------*
051400000801     C     CHKNUM        BEGSR
051500081118     C*
051600081118     C                   IF        PiDecChr = *blanks
051700081118     C                   EVAL      PiDecChr = ','
051800081118     C                   ENDIF
051900081118     C*
052000081118     C                   callp(e)  UBISNUM_Check(PiStr
052100081118     C                                          :PiDecChr
052200081118     C                                          :PiVal
052300081118     C                                          :PiNum
052400081118     C                                          :PiInt)
052500081118     C*
052600081118     C                   IF        %error
052700081118     C                   EVAL      PiInt=*off
052800081118     C                   ENDIF
052900000801     C*
053000000801     C                   ENDSR
053100000801     C***
053200000801
053300011113
053400011113     C*----------------------------------------------------*
053500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053600011113     C*----------------------------------------------------*
053700011113     C     CHKIMPDIV     BEGSR
053800011113     C*
053900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054000011113     C                   Z-ADD     *zeros        wrkDec            9 9
054100011113     C*
054200011113     C* Come prima cosa effettuo considerazioni sulla divisa
054300011113     C                   IF        vabIAS > *zeros
054400011113     C                   IF        vabVAS <> 'EUR'
054500011113     C                   EVAL      vabVAS =  'ITL'
054600011113     C                   ENDIF
054700011113     C                   ENDIF
054800011113     C*
054900011113     C                   IF        vabCAS > *zeros
055000011113     C                   IF        vabVCA <> 'EUR'
055100011113     C                   EVAL      vabVCA =  'ITL'
055200011113     C                   ENDIF
055300011113     C                   ENDIF
055400011113     C*
055500011113     C                   IF        vabVMD > *zeros
055600020305     C                   IF        vabVAD <> 'EUR'
055700011113     C                   EVAL      vabVAD =  'ITL'
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056200011113     C                   Z-ADD     vabIAS        wrkDec
056300011113     C                   IF        wrkDec > *zeros
056400011113     C                   IF        vabVAS = 'ITL'
056500011113     C                   EVAL      vabIAS = *zeros
056600011113     C                   ENDIF
056700011113     C                   ENDIF
056800011113     C*
056900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057000011113     C                   Z-ADD     vabCAS        wrkDec
057100011113     C                   IF        wrkDec > *zeros
057200011113     C                   IF        vabVCA = 'ITL'
057300011113     C                   EVAL      vabCAS = *zeros
057400011113     C                   ENDIF
057500011113     C                   ENDIF
057600011113     C*
057700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057800011113     C                   Z-ADD     vabVMD        wrkDec
057900011113     C                   IF        wrkDec > *zeros
058000011113     C                   IF        vabVAD = 'ITL'
058100011113     C                   EVAL      vabVMD = *zeros
058200011113     C                   ENDIF
058300011113     C                   ENDIF
058400011113     C*
058500011113     C                   ENDSR
058600011113     C***
058700011113
058800011113
058900000801
059000000801
059100990920      /TITLE Invio dei dati al punto operativo.
059200010202     C     invio         BEGSR
059300990920     C*
059400021113     C* 1° invio FIVAT
059500010201     C                   reset                   dscmz
059600010201     C                   move      vlrpoi        cmzdst
059700021113     C                   eval      cmzfld = 'FIVATWWR'
059800010201     C                   eval      cmzmbd = vlrhdl
059900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060000021009     C***                if        prmfir = *blanks
060100021113     C                   eval      cmzfla = 'FIVAT00F'
060200021113     C                   eval      cmzmba = 'FIVAT00F'
060300021009     C***                else
060400021009     C***                eval      cmzfla = prmfir
060500021009     C***                eval      cmzmba = prmfir
060600021009     C***                endif
060700010201     C                   eval      cmznrr = *zeros
060800020305     C                   move      §ctrokvt      cmznrr
060900021018     C                   eval      cmzlba = vlrfl1
061000010201     C                   call(e)   'TIS711C'
061100010201     C                   parm                    dscmz
061200010201     C                   parm      *blanks       esito
061300010205     C                   if        %error
061400010205     C                             or cmzerr = '1'
061500010205     C                             or esito  = '1'
061600010205     C                   eval      wrkesito = '3'
061700010205     C                   else
061800010201     C*
061900021113     C* 2° invio FIVAB
062000010201     C                   reset                   dscmz
062100010201     C                   move      vlrpoi        cmzdst
062200010201     C                   eval      cmzfld = vlrfou
062300010201     C                   eval      cmzmbd = vlrhdl
062400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062500021009     C***                if        prmfir = *blanks
062600021113     C                   eval      cmzfla = 'FIVAB00F'
062700021113     C                   eval      cmzmba = 'FIVAB00F'
062800021009     C***                else
062900021009     C***                eval      cmzfla = prmfir
063000021009     C***                eval      cmzmba = prmfir
063100021009     C***                endif
063200010201     C                   eval      cmznrr = *zeros
063300010201     C                   move      §ctrokvb      cmznrr
063400021018     C                   eval      cmzlba = vlrfl1
063500010201     C                   call(e)   'TIS711C'
063600010201     C                   parm                    dscmz
063700010201     C                   parm      *blanks       esito
063800010201     C                   if        %error
063900010201     C                             or cmzerr = '1'
064000010201     C                             or esito  = '1'
064100010201     C                   eval      wrkesito = '3'
064200010201     C                   endif
064300010205     C                   endif
064400990920     C*
064500000613     C                   ENDSR
064600000613     C***
064700130206
064800130206     C*--------------------------------------------------------
064900130206     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
065000130206     C*--------------------------------------------------------
065100130206     C     CARTAB        BEGSR
065200130206     C*
065300130206     C* TABELLA '15' - NAZIONI
065400130206     C                   clear                   skNAZISO
065500130206     C                   clear                   skNAZBAR
065600130206     C                   eval      tblKUT = 1
065700130206     C                   eval      tblCOD = '15'
065800130206     C     KEYtabP       setll     tabel00f
065900130206     C     KEYtabP       reade     tabel00f
066000130206     C                   dow       not %eof(tabel00f)
066100130206     C                   if        tblFLG = *blanks
066200130206     C                   movel(p)  tblUNI        ds15
066300130206     C                   add       1             jNAZ
066400130206     C                   eval      skNAZBAR(jNAZ) = tblKEY
066500130206     C                   eval      skNAZISO(jNAZ) = §15COD
066600130206     C                   endif
066700130206     C     KEYtabP       reade     tabel00f
066800130206     C                   enddo
066900130206     C*
067000130206     C                   ENDSR
067100130206     C***
067200070411
067300070411     C     *pssr         BEGSR
067400070411     C*
067500070411     C                   if        %open(tivin00r)
067600070411     C                   close     tivin00r
067700070411     C                   endif
067800070411     C                   if        %open(fivabwwr)
067900070411     C                   close     fivabwwr
068000070411     C                   endif
068100070411     C                   if        %open(fivatwwr)
068200070411     C                   close     fivatwwr
068300070411     C                   endif
068400070411     C*
068500070411     C* Effettuo la chiamata al CLLE preposto
068600070411     C                   call(e)   'TITVVTC'
068700070411     C                   parm                    parccm
068800070411     C                   parm                    parmbr
068900070411     C                   parm      '2'           paropz
069000070411     C*
069100070411     C                   eval      wrkesito = '2'
069200070411     C*
069300070411     C                   seton                                        LR
069400070411     C*
069500070411     C                   ENDSR     '*CANCL'
069600070411     C***
069700070411
069800990910
069900000613     C     *inzsr        BEGSR
070000990910     C*
070100990910     C     *entry        plist
070200990920     C                   parm                    tivlrds
070300990921     C                   parm      wrkesito      esito
070400000724     C                   parm                    prmlit
070500000710     C                   parm                    prmfir
070600000613     C*
070700000830     C* CALCOLA LA DATA CORRENTE
070800110421     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
070900110421     C                   eval      datcor = %dec(%date() : *ISO)
071000130206     C*
071100130206     C* Chiave su TABEL00F - parziale
071200130206     C     KEYtabP       KLIST
071300130206     C                   KFLD                    tblKUT
071400130206     C                   KFLD                    tblCOD
071500000830     C*
071600000613     C                   ENDSR
071700000613     C***
