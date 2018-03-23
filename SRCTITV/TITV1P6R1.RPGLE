000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400090127     Ftntbe01l  if   e           k disk
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
001700090127     D tivlrds_rc    e ds                  extname(tivlr00f) prefix(rc_)
001800090127     D fivabds       e ds                  extname(fivab00f)
001900090127     D dosr          e ds                  inz
002000090127     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
002100090127     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
002200090127     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
002300090127     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
002400090127     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
002500090127     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
002600090127     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
002700090127     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
002800090127     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
002900090127     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
003000090127     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
003100990910     D esito           s              1
003200000724     D prmlit          s             10
003300000710     D prmfir          s             10
003400990921     D wrkesito        s                   like(esito)
003500000613     D rrnum           s              6  0 INZ(*zeros)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000070402     D curSped         s             25    INZ(*blanks)
004100070402     D depSped         s             25    INZ(*blanks)
004200090127     D wNSR            s              2    INZ
004300090127     D wfivabds        s                   LIKE(fivabds)
004400000830
004500000830     D*------------------
004600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700000830     D*------------------
004800000830     D WLBDA8          DS                  INZ
004900000830     D  G08DAT                 1      8  0
005000000830     D  G08INV                 9     16  0
005100000830     D  G08ERR                17     17
005200000830     D  G08TGI                18     22  0
005300000830     D*
005400010201
005500090127     D*------------------
005600090127     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
005700090127     D*------------------
005800090127     D SkCliOSR        s             10    DIM(900)
005900090127     D*
006000090127     D DsCliOSR        ds
006100090127     D  wLnpOSR                       3
006200090127     D  wCliOSR                       7
006300090127     D*
006400090127     D SkLnpNrsOSR     s              5    DIM(900)
006500010201
006600090127
006700090127
006800090127     C*
006900090127     C* CARICO LE TABELLE OCCORRENTI
007000090127     C                   exsr      CARTAB
007100090127     C*
007200000913     C                   reset                   rrnum
007300990921     C                   reset                   esito
007400990921     C                   reset                   wrkesito
007500000613     C*
007600040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
007700000613     C*
007800010202     C* Effettuo la chiamata al CLLE preposto
007900040506     C                   call(e)   'TITVVTC'
008000010202     C                   parm                    parccm
008100010202     C                   parm                    parmbr
008200010202     C                   parm      '2'           paropz
008300050201     C*
008400050201     C* Effettuo lancio TISI95 solo x chiusura
008500050201     C                   CLEAR                   TISI95DS
008600050201     C                   EVAL      I95TLA = 'C'
008700050201     C                   CALL      'TISI95R'
008800050201     C                   PARM                    TISI95DS
008900090127     C*
009000090127     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
009100090127     C   54              call(e)   'TITV84R'
009200090127     C                   parm      'C'           InTipl
009300090127     C                   parm                    tivlrds_rc
009400090127     C                   parm                    wfivabds
009500000616     C*
009600000801     C
009700010201     C                   seton                                        LR
009800990908
009900000801
010000910830     C*--------------------------------------------------------
010100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010200910830     C*--------------------------------------------------------
010300040526     C     RWFILE        BEGSR
010400990910     C*
010500990914     C                   if        not %open(tivin00r)
010600990908     C                   open      tivin00r
010700990914     C                   endif
010800021113     C                   if        not %open(fivabwwr)
010900021113     C                   open      fivabwwr
011000990914     C                   endif
011100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011200020305     C                   exsr      prevat
011300010201     C*
011400010202     C                   if        chkcall = '0'
011500010202     C*
011600021113     C                   if        not %open(fivatwwr)
011700021113     C                   open      fivatwwr
011800010201     C                   endif
011900990910     C*
012000010201     C                   clear                   §CTROKVB          5 0
012100020305     C                   clear                   §CTROKVT          5 0
012200000801     C                   clear                   §CTRMO            5 0
012300000801     C                   clear                   §CTRNO            5 0
012400990910     C*
012500921023     C                   DO        *HIVAL
012600990913     C*
012700990915     C                   READ      tivin00r                               70
012800050627     C                   if        vindta > *blanks
012900000613     C                   add       1             rrnum
013000000801     C*
013100000801     C                   if        *in70 = *off
013200000801     C                             and
013300000801     C                             (vinflg = *blanks
013400000801     C                              or vinflg = '0'
013500000801     C                              or vinflg = '2')
013600000801     C*
013700000801     C                   clear                   vinmsg
013800000801     C                   eval      vinflg = '1'
013900070402     C*
014000070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
014100070402     C                   eval      curSped = %subst(vindta:46:25)
014200070402     C                   if        curSped <> depSped
014300070402     C                   clear                   fivab000
014400070402     C                   clear                   fivat000
014500050628     C*
014600060315     C                   exsr      impvab                                       => carico  VAB
014700090127     C*
014800090127     C* Se richeisto ritiro contestuale reperisco il numero serie ORM in tabella 'OSR'
014900090127     C                   IF        VABGMA = 'RC'
015000090127     C                   Z-ADD     1             jOSR
015100090127     C                   move(p)   VABLNP        wLnpOSR
015200090127     C                   move(p)   VABCCM        wCliOSR
015300090127     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
015400090127     C                   IF        %found
015500090127     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
015600090127     C                   EVAL      %subst(rc_vlrppt:4:2) = wNSR
015700090127     C                   ELSE
015800090127     C                   SETON                                        31
015900090127     C                   EVAL      vinmsg = %trimr(vinmsg)
016000090127     C                             + ' ' + 'ORMNRS(tabellaOSR)'
016100090127     C                   ENDIF
016200090127     C*
016300090127     C* Forso che l'orm contestuale venga generato con colli e peso = quelli della bolla originale
016400090127     C                   EVAL      %subst(rc_vlrppt:29:1) = 'S'
016500090707     C*
016600090707     C                   EVAL      rc_vlrKSC = vlrKSC
016700090707     C*
016800090127     C                   ENDIF
016900090127     C*
017000060315     C                   exsr      wrivab                                       => scarico VAB
017100070402     C*
017200070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
017300070402     C                   exsr      exevata
017400070402     C*
017500070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
017600070402     C                   exsr      exevatb
017700160907     C*
017800160907     C* Scrivo anche estensione dettaglio bolle - tipo record "S" => CELLULARE DESTINATARIO
017900160907     C                   exsr      exevats
018000160907     C*
018100160907     C* Scrivo anche estensione dettaglio bolle - tipo record "I" => EMAIL DESTINATARIO
018200160907     C                   exsr      exevati
018300070402     C*
018400070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
018500070402     C                   exsr      exevate
018600070402     C*
018700070402     C* Salvo il raggruppamento spedizione cliente corrente
018800070402     C                   eval      depSped = curSped
018900070402     C*
019000070402     C                   else
019100070402     C*
019200070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
019300070402     C                   exsr      exevate
019400070402     C                   endif
019500000905     C*
019600000905     C                   else
019700000905     C                   eval      vinflg = '1'
019800050628     C                   endif
019900000905     C                   endif
020000000905     C*
020100000905     C  N70              update    tivin000
020200000905     C*
020300991022     C  N70              ENDdo
020400010202     C*
020500010202     C                   endif
020600990910
020700990910     C* Se non ci sono record con errori ...
020800000710     C                   if        §ctrno = 0
020900990910     C* ... restituisco esito OK.
021000990921     C                   eval      wrkesito = '0'
021100990910     C                   else
021200010201     C                   if        §ctrokvb > 0
021300990921     C                   eval      wrkesito = '1'
021400000710     C                   else
021500000710     C                   eval      wrkesito = '2'
021600990910     C                   endif
021700000710     C                   endif
021800990910     C*
021900990914     C                   if        %open(tivin00r)
022000990908     C                   close     tivin00r
022100990914     C                   endif
022200021113     C                   if        %open(fivabwwr)
022300021113     C                   close     fivabwwr
022400990914     C                   endif
022500021113     C                   if        %open(fivatwwr)
022600021113     C                   close     fivatwwr
022700010201     C                   endif
022800990910     C*
022900010201     C                   if        §ctrokvb > 0
023000000724     C                             and vlrpoi <> *zeros
023100010202     C                   exsr      invio
023200990920     C                   endif
023300990920     C*
023400910830     C                   ENDSR
023500000613     C***
023600010305
023700010305     C*----------------------------------------------------*
023800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
023900010305     C*----------------------------------------------------*
024000090127     C     WRIVAB        BEGSR
024100090127     C*
024200090127     C* Quindi scarico il buffer del file d testata
024300090127     C                   write     fivab000                                     => scarico il VAB
024400090127     C*
024500090127     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
024600090127     C                   if        *in60  = *on  AND
024700090127     C                             vabGMA = 'RC'
024800090127     C*
024900090127     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
025000090127     C                   seton                                        54
025100090127     C*
025200090127     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
025300090127     C                   eval      wfivabds = fivabds
025400090127     C                   call      'TITV84R'
025500090127     C                   parm      *blanks       InTipl            1
025600090127     C                   parm                    tivlrds_rc
025700090127     C                   parm                    wfivabds
025800090127     C                   endif
025900010305     C*
026000010305     C                   ENDSR
026100990920
026200000801     C*----------------------------------------------------*
026300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026400000801     C*----------------------------------------------------*
026500010201     C     INZVAR        BEGSR
026600000801     C*
026700040802     C                   Z-ADD     *zeros        Num5_0            5 0
026800040802     C                   MOVEL     '0'           FlgCAS            1
026900000801     C*
027000000801     C                   ENDSR
027100000801     C*----------------------------------------------------*
027200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027300000801     C*----------------------------------------------------*
027400000801     C     DEFCAM        BEGSR
027500070718     C*
027600070718     C* Imposto i valori di default...
027700090511     C                   Z-ADD     0055052       VABCCM
027800090511     C                   Z-ADD     0055052       VATCCM
027900090127     C                   Z-ADD     005           VABLNP
028000090127     C                   Z-ADD     005           VATLNP
028100090511     C                   Z-ADD     300           VABCTR
028200070718     C                   MOVEL     '1'           VABCBO
028300070718     C                   MOVEL     '7Q'          VABCTM
028400020619     C* ... e poi verifico se sono stati passati come parametri
028500020619     C                   IF        vlrppt > *blanks
028600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028800020619     C                   EXSR      CHKNUM
028900020619     C                   IF        PiInt=*on
029000020619     C                   Z-ADD     PiVal         VABCCM
029100020619     C                   Z-ADD     PiVal         VATCCM
029200020619     C                   ENDIF
029300040506     C                   ENDIF
029400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029600020619     C                   EXSR      CHKNUM
029700020619     C                   IF        PiInt=*on
029800020619     C                   Z-ADD     PiVal         VABLNP
029900020619     C                   Z-ADD     PiVal         VATLNP
030000040506     C                   ENDIF
030100020619     C                   ENDIF
030200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
030300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030400020619     C                   EXSR      CHKNUM
030500020619     C                   IF        PiInt=*on
030600020619     C                   Z-ADD     PiVal         VABCTR
030700040506     C                   ENDIF
030800020619     C                   ENDIF
030900090127     C                   IF        %subst(vlrppt:14:2) = 'RC'
031000090127     C                   SETON                                        60
031100090127     C                   ELSE
031200090127     C                   SETOFF                                       60
031300090127     C                   ENDIF
031400020619     C                   ENDIF
031500000801     C*
031600000801     C                   ENDSR
031700000801     C*----------------------------------------------------*
031800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031900000801     C*----------------------------------------------------*
032000040823     C     IMPVAB        BEGSR
032100040823     C*
032200020305     C                   EXSR      INZVAR
032300020305     C                   EXSR      DEFCAM
032400010305     C*
032500000801     C                   Z-ADD     *zeros        errore            1 0
032600000830     C                   MOVEL     datcor        VABAAS
032700020305     C                   MOVEL     datcor        VATAAS
032800040526     C                   MOVE      datcor        VABMGS
032900040823     C                   MOVE(P)   vlrpoi        VABFGS
033000040823     C                   MOVE(P)   vlrpoi        VATFGS
033100050628     C*
033200070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
033300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033500020117     C* ==
033600070719     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
033700090427     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
033800070102     C                                    %trim(%subst(vindta:341:30))+' '+
033900090427     C                                    %trim(%subst(vindta:371:5)))
034000070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
034100070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
034200090511     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
034300070102     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
034400161121     C                   EVAL      VABRMO=%trim(%subst(vindta:100:40))
034500070102     C                   IF        %subst(vindta:43:1) = 'N'
034600070102     C                   EVAL      VABCBO='1'
034700070102     C                   ENDIF
034800070102     C                   IF        %subst(vindta:43:1) = 'P'
034900070102     C                   EVAL      VABCBO='2'
035000070102     C                   ENDIF
035100070102     C                   IF        %subst(vindta:511:3) = 'ABM'
035200070102     C                   EVAL      VABTIC='BM'
035300070102     C                   ENDIF
035400070102     C                   IF        %subst(vindta:511:3) = 'ABS'
035500070102     C                   EVAL      VABTIC='B'
035600070102     C                   ENDIF
035700070102     C                   IF        %subst(vindta:511:3) = 'CON'
035800070102     C                   EVAL      VABTIC=*blanks
035900070102     C                   ENDIF
036000070102     C                   IF        %subst(vindta:511:3) = 'ACM'
036100070102     C                   EVAL      VABTIC='CM'
036200070102     C                   ENDIF
036300090127     C                   IF        %subst(vindta:44:1) = 'S'
036400090127     C                   EVAL      VABGMA = 'RC'
036500090127     C                   ENDIF
036600050628     C* CAP
036700070719     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
036800010201     C                   EXSR      CHKNUM
036900010201     C                   IF        PiInt=*on
037000010201     C                   Z-ADD     PiVal         Num5_0
037100040506     C                   MOVEL(P)  Num5_0        VABCAD
037200010201     C                   ELSE
037300040506     C                   ADD       1             errore
037400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037500040506     C                             + ' ' + 'VABCAD'
037600010201     C                   ENDIF
037700040506     C* Reperisco la provincia dal CAP e dalla località
037800040526     C                   IF        VABCAD <> *blanks AND
037900040526     C                             VABPRD  = *blanks
038000040506     C                   CLEAR                   TISI95DS
038100040506     C                   EVAL      I95TCN = '3'
038200040506     C                   Z-ADD     datcor        I95DAT
038300040506     C                   EVAL      I95CAP = VABCAD
038400040506     C                   EVAL      I95LOC = VABLOD
038500050627     C                   EVAL      I95NAR = VABNZD
038600040506     C                   CALL      'TISI95R'
038700040506     C                   PARM                    TISI95DS
038800040506     C                   EVAL      VABPRD = O95PRV
038900040506     C                   ENDIF
039000101213     C* CCM
039100101213     C                   EVAL      PiStr=%trim(%subst(vindta:4:9))
039200101213     C                   EXSR      CHKNUM
039300101213     C                   IF        PiInt=*on
039400101213     C                   IF        PiVal  = 0055877
039500101213     C                   EVAL      VABCCM = PiVal
039600101213     C                   EVAL      VATCCM = PiVal
039700101213     C                   ENDIF
039800110325     C                   IF        PiVal  = 0055052
039900110325     C                   EVAL      VABCCM = PiVal
040000110325     C                   EVAL      VATCCM = PiVal
040100110325     C                   ENDIF
040200101213     C                   ENDIF
040300060307     C* NCL
040400070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
040500060307     C                   EXSR      CHKNUM
040600060307     C                   IF        PiInt=*on
040700060307     C                   ADD       PiVal         VABNCL
040800060307     C                   ELSE
040900060307     C                   ADD       1             errore
041000060307     C                   EVAL      vinmsg = %trimr(vinmsg)
041100060307     C                             + ' ' + 'VABNCL'
041200060307     C                   ENDIF
041300040506     C* PKB
041400070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
041500010201     C                   EXSR      CHKNUM
041600010201     C                   IF        PiNum=*on
041700070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
041800060316     C                   Z-ADD     PiVal         VABPKB
041900010201     C                   ELSE
042000010201     C                   ADD       1             errore
042100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
042200010201     C                             + ' ' + 'VABPKB'
042300010201     C                   ENDIF
042400090511     C* NSP
042500090127     C                   EVAL      PiStr=%trim(%subst(vindta:46:25))
042600050928     C                   EXSR      CHKNUM
042700050928     C                   IF        PiInt=*on
042800060316     C                   Z-ADD     PiVal         VABNSP
042900060316     C                   Z-ADD     PiVal         VATNSP
043000050928     C                   ELSE
043100050928     C                   ADD       1             errore
043200050928     C                   EVAL      vinmsg = %trimr(vinmsg)
043300090511     C                             + ' ' + 'VABNSP VATNSP'
043400050928     C                   ENDIF
043500090511     C* RMN
043600161121     C                   EVAL      PiStr=%trim(%subst(vindta:46:25))
043700090511     C                   EXSR      CHKNUM
043800090511     C                   IF        PiInt=*on
043900090511     C                   Z-ADD     PiVal         VABRMN
044000090511     C                   ELSE
044100090511     C                   ADD       1             errore
044200090511     C                   EVAL      vinmsg = %trimr(vinmsg)
044300090511     C                             + ' ' + 'VABRMN'
044400090511     C                   ENDIF
044500060307     C* CAS
044600090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
044700090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
044800090608     C                             %trim(%subst(vindta:502:9))<>*zeros
044900070102     C                   EVAL      VABVCA = 'EUR'
045000060307     C                   EVAL      FlgCAS = '1'
045100070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
045200060307     C                   EXSR      CHKNUM
045300060307     C                   IF        PiNum=*on
045400060316     C                   Z-ADD     PiVal         VABCAS
045500060307     C                   ELSE
045600060307     C                   ADD       1             errore
045700060307     C                   EVAL      vinmsg = %trimr(vinmsg)
045800060307     C                             + ' ' + 'VABCAS'
045900060307     C                   ENDIF
046000070102     C                   ENDIF
046100060316     C* IAS
046200070719     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
046300070102     C                   EVAL      VABVAS = 'EUR'
046400070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
046500060316     C                   EXSR      CHKNUM
046600060316     C                   IF        PiNum=*on
046700060316     C                   Z-ADD     PiVal         VABIAS
046800060316     C                   ELSE
046900060316     C                   ADD       1             errore
047000060316     C                   EVAL      vinmsg = %trimr(vinmsg)
047100060316     C                             + ' ' + 'VABIAS'
047200070102     C                   ENDIF
047300060316     C                   ENDIF
047400010205     C*
047500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
047600040802     C                   IF        FlgCAS <> '0'
047700070102     C                   IF        VABCBO = '1'
047800010205     C                   EVAL      VABCBO = '4'
047900010205     C                   ELSE
048000070102     C                   EVAL      VABCBO = '6'
048100070102     C                   ENDIF
048200010205     C                   ENDIF
048300020305     C*
048400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048500011113     C                   EXSR      CHKIMPDIV
048600010202     C*
048700000801     C* Ebbene...
048800000801     C                   ADD       1             §CTRMO
048900010201     C                   IF        errore <> *zeros
049000000801     C                   ADD       1             §CTRNO
049100000801     C                   EVAL      vinflg = '2'
049200000801     C                   ELSE
049300010201     C                   ADD       1             §CTROKVB
049400000801     C                   ENDIF
049500000801     C*
049600000801     C                   ENDSR
049700070102     C*----------------------------------------------------*
049800070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
049900070102     C*----------------------------------------------------*
050000070102     C     EXEVATA       BEGSR
050100070102     C*
050200070102     C                   EXSR      INZVAR
050300070102     C                   EXSR      DEFCAM
050400070102     C*
050500070102     C                   EVAL      VATTRC='A'
050600090127     C                   EVAL      VATNOT = %trim(%subst(vindta:581:30))
050700070102     C                   exsr      wrivat                                       => scarico VAT
050800070102     C*
050900070102     C                   ENDSR
051000060307     C*----------------------------------------------------*
051100060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
051200060307     C*----------------------------------------------------*
051300060307     C     EXEVATB       BEGSR
051400060307     C*
051500060307     C                   EXSR      INZVAR
051600060307     C                   EXSR      DEFCAM
051700060307     C*
051800060307     C                   EVAL      VATTRC='B'
051900070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
052000060307     C                   exsr      wrivat                                       => scarico VAT
052100060307     C*
052200060307     C                   ENDSR
052300160907     C*----------------------------------------------------*
052400160907     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
052500160907     C*----------------------------------------------------*
052600160907     C     EXEVATS       BEGSR
052700160907     C*
052800160907     C                   EXSR      INZVAR
052900160907     C                   EXSR      DEFCAM
053000160907     C*
053100160907     C                   EVAL      VATTRC='S'
053200160907     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
053300160907     C                   exsr      wrivat                                       => scarico VAT
053400160907     C*
053500160907     C                   ENDSR
053600160907     C*----------------------------------------------------*
053700160907     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
053800160907     C*----------------------------------------------------*
053900160907     C     EXEVATI       BEGSR
054000160907     C*
054100160907     C                   EXSR      INZVAR
054200160907     C                   EXSR      DEFCAM
054300160907     C*
054400160907     C                   EVAL      VATTRC='I'
054500160907     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
054600160907     C                   exsr      wrivat                                       => scarico VAT
054700160907     C*
054800160907     C                   ENDSR
054900070402     C*----------------------------------------------------*
055000070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
055100070402     C*----------------------------------------------------*
055200070402     C     EXEVATE       BEGSR
055300070402     C*
055400070402     C                   EXSR      INZVAR
055500070402     C                   EXSR      DEFCAM
055600070402     C*
055700070402     C                   EVAL      VATTRC='E'
055800070402     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
055900070402     C                   exsr      wrivat                                       => scarico VAT
056000070402     C*
056100070402     C                   ENDSR
056200010201     C*----------------------------------------------------*
056300040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
056400010201     C*----------------------------------------------------*
056500020305     C     WRIVAT        BEGSR
056600050628     C*
056700060223     C* Scrivo solo se valorizzato qualcosa
056800060223     C                   IF        VATNOT <> *blanks
056900040802     C                   WRITE     FIVAT000
057000060223     C                   ENDIF
057100010201     C*
057200010201     C                   ENDSR
057300010202     C*----------------------------------------------------*
057400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
057500010202     C*----------------------------------------------------*
057600020305     C     PREVAT        BEGSR
057700010202     C*
057800021113     C* Compongo il nome del membro da dare al FIVATWWR
057900010202     C                   eval      parmbr = vlrhdl
058000010202     C                   movel     'M'           parmbr
058100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
058200010202     C                   eval      paropz = '1'
058300010202     C* Effettuo la chiamata al CLLE preposto
058400040506     C                   call(e)   'TITVVTC'
058500010202     C                   parm                    parccm
058600010202     C                   parm                    parmbr
058700010202     C                   parm                    paropz
058800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058900010202     C                   if        %error
059000010202     C                   movel     '1'           chkcall
059100010202     C                   else
059200010202     C                   movel     '0'           chkcall
059300010202     C                   endif
059400010202     C*
059500010202     C                   ENDSR
059600000801     C*----------------------------------------------------*
059700000801     C*  CONTROLLO NUMERICITA' CAMPI
059800000801     C*----------------------------------------------------*
059900000801     C     CHKNUM        BEGSR
060000000801     C*
060100000801     C                   call(e)   'ISNUMERIC'
060200000801     C                   PARM                    PiStr            30
060300070102     C                   PARM      '.'           PiDecChr          1
060400000801     C                   PARM      *ZEROS        PiVal            30 9
060500000801     C                   PARM      '0'           PiInt             1
060600000801     C                   PARM      '0'           PiNum             1
060700000801     C                   IF        %error
060800000801     C                   EVAL      PiInt=*off
060900000801     C                   ENDIF
061000000801     C*
061100000801     C                   ENDSR
061200000801     C***
061300000801
061400011113
061500011113     C*----------------------------------------------------*
061600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061700011113     C*----------------------------------------------------*
061800011113     C     CHKIMPDIV     BEGSR
061900011113     C*
062000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
062100011113     C                   Z-ADD     *zeros        wrkDec            9 9
062200011113     C*
062300011113     C* Come prima cosa effettuo considerazioni sulla divisa
062400011113     C                   IF        vabIAS > *zeros
062500011113     C                   IF        vabVAS <> 'EUR'
062600011113     C                   EVAL      vabVAS =  'ITL'
062700011113     C                   ENDIF
062800011113     C                   ENDIF
062900011113     C*
063000011113     C                   IF        vabCAS > *zeros
063100011113     C                   IF        vabVCA <> 'EUR'
063200011113     C                   EVAL      vabVCA =  'ITL'
063300011113     C                   ENDIF
063400011113     C                   ENDIF
063500011113     C*
063600011113     C                   IF        vabVMD > *zeros
063700020305     C                   IF        vabVAD <> 'EUR'
063800011113     C                   EVAL      vabVAD =  'ITL'
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064300011113     C                   Z-ADD     vabIAS        wrkDec
064400011113     C                   IF        wrkDec > *zeros
064500011113     C                   IF        vabVAS = 'ITL'
064600011113     C                   EVAL      vabIAS = *zeros
064700011113     C                   ENDIF
064800011113     C                   ENDIF
064900011113     C*
065000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
065100011113     C                   Z-ADD     vabCAS        wrkDec
065200011113     C                   IF        wrkDec > *zeros
065300011113     C                   IF        vabVCA = 'ITL'
065400011113     C                   EVAL      vabCAS = *zeros
065500011113     C                   ENDIF
065600011113     C                   ENDIF
065700011113     C*
065800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065900011113     C                   Z-ADD     vabVMD        wrkDec
066000011113     C                   IF        wrkDec > *zeros
066100011113     C                   IF        vabVAD = 'ITL'
066200011113     C                   EVAL      vabVMD = *zeros
066300011113     C                   ENDIF
066400011113     C                   ENDIF
066500011113     C*
066600011113     C                   ENDSR
066700011113     C***
066800011113
066900011113
067000000801
067100000801
067200990920      /TITLE Invio dei dati al punto operativo.
067300010202     C     invio         BEGSR
067400990920     C*
067500021113     C* 1° invio FIVAT
067600010201     C                   reset                   dscmz
067700010201     C                   move      vlrpoi        cmzdst
067800021113     C                   eval      cmzfld = 'FIVATWWR'
067900010201     C                   eval      cmzmbd = vlrhdl
068000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068100021009     C***                if        prmfir = *blanks
068200021113     C                   eval      cmzfla = 'FIVAT00F'
068300021113     C                   eval      cmzmba = 'FIVAT00F'
068400021009     C***                else
068500021009     C***                eval      cmzfla = prmfir
068600021009     C***                eval      cmzmba = prmfir
068700021009     C***                endif
068800010201     C                   eval      cmznrr = *zeros
068900020305     C                   move      §ctrokvt      cmznrr
069000021018     C                   eval      cmzlba = vlrfl1
069100010201     C                   call(e)   'TIS711C'
069200010201     C                   parm                    dscmz
069300010201     C                   parm      *blanks       esito
069400010205     C                   if        %error
069500010205     C                             or cmzerr = '1'
069600010205     C                             or esito  = '1'
069700010205     C                   eval      wrkesito = '3'
069800010205     C                   else
069900010201     C*
070000021113     C* 2° invio FIVAB
070100010201     C                   reset                   dscmz
070200010201     C                   move      vlrpoi        cmzdst
070300010201     C                   eval      cmzfld = vlrfou
070400010201     C                   eval      cmzmbd = vlrhdl
070500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070600021009     C***                if        prmfir = *blanks
070700021113     C                   eval      cmzfla = 'FIVAB00F'
070800021113     C                   eval      cmzmba = 'FIVAB00F'
070900021009     C***                else
071000021009     C***                eval      cmzfla = prmfir
071100021009     C***                eval      cmzmba = prmfir
071200021009     C***                endif
071300010201     C                   eval      cmznrr = *zeros
071400010201     C                   move      §ctrokvb      cmznrr
071500021018     C                   eval      cmzlba = vlrfl1
071600010201     C                   call(e)   'TIS711C'
071700010201     C                   parm                    dscmz
071800010201     C                   parm      *blanks       esito
071900010201     C                   if        %error
072000010201     C                             or cmzerr = '1'
072100010201     C                             or esito  = '1'
072200010201     C                   eval      wrkesito = '3'
072300010201     C                   endif
072400010205     C                   endif
072500990920     C*
072600000613     C                   ENDSR
072700000613     C***
072800090127
072900090127
073000090127
073100090127     C*--------------------------------------------------------
073200090127     C* CARTAB - CARICAMENTO TABELLE                          *
073300090127     C*--------------------------------------------------------
073400090127     C     CARTAB        BEGSR
073500090127     C*
073600090127     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
073700090127     C                   z-add     *zeros        jOSR              3 0
073800090127     C     'OSR'         setll     tntbe01l
073900090127     C                   if        %equal(tntbe01l)
074000090127     C     'OSR'         reade     tntbe01l
074100090127     C                   dow       not %eof(tntbe01l)
074200090127     C                   eval      DOSR = tbeUNI
074300090127     c*
074400090127     c* Verifica e forzatura x nuovi campi aggiunti
074500090127     c                   if        aD§OSRcl6 = *blanks
074600090127     c                   eval      D§OSRcl6 = *zeros
074700090127     c                   endif
074800090127     c                   if        aD§OSRcl7 = *blanks
074900090127     c                   eval      D§OSRcl7 = *zeros
075000090127     c                   endif
075100090127     c                   if        aD§OSRcl8 = *blanks
075200090127     c                   eval      D§OSRcl8 = *zeros
075300090127     c                   endif
075400090127     c                   if        aD§OSRcl9 = *blanks
075500090127     c                   eval      D§OSRcl9 = *zeros
075600090127     c                   endif
075700090127     c                   if        aD§OSRcl10 = *blanks
075800090127     c                   eval      D§OSRcl10 = *zeros
075900090127     c                   endif
076000090127     c                   if        aD§OSRcl11 = *blanks
076100090127     c                   eval      D§OSRcl11 = *zeros
076200090127     c                   endif
076300090127     c                   if        aD§OSRcl12 = *blanks
076400090127     c                   eval      D§OSRcl12 = *zeros
076500090127     c                   endif
076600090127     c                   if        aD§OSRcl13 = *blanks
076700090127     c                   eval      D§OSRcl13 = *zeros
076800090127     c                   endif
076900090127     c                   if        aD§OSRcl14 = *blanks
077000090127     c                   eval      D§OSRcl14 = *zeros
077100090127     c                   endif
077200090127     c                   if        aD§OSRcl15 = *blanks
077300090127     c                   eval      D§OSRcl15 = *zeros
077400090127     c                   endif
077500090127     c                   if        aD§OSRcl16 = *blanks
077600090127     c                   eval      D§OSRcl16 = *zeros
077700090127     c                   endif
077800090127     c*
077900090127     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
078000090127     C                   if        D§OSRCLI <> *zeros
078100090127     C                   add       1             jOSR
078200090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
078300090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
078400090127     C                   movel     D§OSRCLI      wCliOSR
078500090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
078600090127     C                   endif
078700090127     C*
078800090127     C                   if        D§OSRCL2 <> *zeros
078900090127     C                   add       1             jOSR
079000090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
079100090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
079200090127     C                   movel     D§OSRCL2      wCliOSR
079300090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
079400090127     C                   endif
079500090127     C*
079600090127     C                   if        D§OSRCL3 <> *zeros
079700090127     C                   add       1             jOSR
079800090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
079900090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
080000090127     C                   movel     D§OSRCL3      wCliOSR
080100090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
080200090127     C                   endif
080300090127     C*
080400090127     C                   if        D§OSRCL4 <> *zeros
080500090127     C                   add       1             jOSR
080600090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
080700090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
080800090127     C                   movel     D§OSRCL4      wCliOSR
080900090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
081000090127     C                   endif
081100090127     C*
081200090127     C                   if        D§OSRCL5 <> *zeros
081300090127     C                   add       1             jOSR
081400090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
081500090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
081600090127     C                   movel     D§OSRCL5      wCliOSR
081700090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
081800090127     C                   endif
081900090127     C*
082000090127     C                   if        D§OSRCL6 <> *zeros
082100090127     C                   add       1             jOSR
082200090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
082300090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
082400090127     C                   movel     D§OSRCL6      wCliOSR
082500090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
082600090127     C                   endif
082700090127     C*
082800090127     C                   if        D§OSRCL7 <> *zeros
082900090127     C                   add       1             jOSR
083000090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
083100090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
083200090127     C                   movel     D§OSRCL7      wCliOSR
083300090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
083400090127     C                   endif
083500090127     C*
083600090127     C                   if        D§OSRCL8 <> *zeros
083700090127     C                   add       1             jOSR
083800090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
083900090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
084000090127     C                   movel     D§OSRCL8      wCliOSR
084100090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
084200090127     C                   endif
084300090127     C*
084400090127     C                   if        D§OSRCL9 <> *zeros
084500090127     C                   add       1             jOSR
084600090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
084700090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
084800090127     C                   movel     D§OSRCL9      wCliOSR
084900090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
085000090127     C                   endif
085100090127     C*
085200090127     C                   if        D§OSRCL10 <> *zeros
085300090127     C                   add       1             jOSR
085400090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
085500090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
085600090127     C                   movel     D§OSRCL10     wCliOSR
085700090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
085800090127     C                   endif
085900090127     C*
086000090127     C                   if        D§OSRCL11 <> *zeros
086100090127     C                   add       1             jOSR
086200090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
086300090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
086400090127     C                   movel     D§OSRCL11     wCliOSR
086500090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
086600090127     C                   endif
086700090127     C*
086800090127     C                   if        D§OSRCL12 <> *zeros
086900090127     C                   add       1             jOSR
087000090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
087100090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
087200090127     C                   movel     D§OSRCL12     wCliOSR
087300090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
087400090127     C                   endif
087500090127     C*
087600090127     C                   if        D§OSRCL13 <> *zeros
087700090127     C                   add       1             jOSR
087800090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
087900090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
088000090127     C                   movel     D§OSRCL13     wCliOSR
088100090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
088200090127     C                   endif
088300090127     C*
088400090127     C                   if        D§OSRCL14 <> *zeros
088500090127     C                   add       1             jOSR
088600090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
088700090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
088800090127     C                   movel     D§OSRCL14     wCliOSR
088900090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
089000090127     C                   endif
089100090127     C*
089200090127     C                   if        D§OSRCL15 <> *zeros
089300090127     C                   add       1             jOSR
089400090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
089500090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
089600090127     C                   movel     D§OSRCL15     wCliOSR
089700090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
089800090127     C                   endif
089900090127     C*
090000090127     C                   if        D§OSRCL16 <> *zeros
090100090127     C                   add       1             jOSR
090200090127     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
090300090127     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
090400090127     C                   movel     D§OSRCL16     wCliOSR
090500090127     C                   eval      SkCliOSR(jOSR) = DsCliOSR
090600090127     C                   endif
090700090127     C*
090800090127     C     'OSR'         reade     tntbe01l
090900090127     C                   enddo
091000090127     C                   endif
091100090127     C*
091200090127     C                   ENDSR
091300090127     C***
091400090127
091500090127
091600990910
091700000613     C     *inzsr        BEGSR
091800990910     C*
091900990910     C     *entry        plist
092000990920     C                   parm                    tivlrds
092100990921     C                   parm      wrkesito      esito
092200000724     C                   parm                    prmlit
092300000710     C                   parm                    prmfir
092400000613     C*
092500000830     C* CALCOLA LA DATA CORRENTE
092600000830     C                   time                    wn14             14 0
092700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
092800000830     C                   z-add     wn8           g08dat
092900000830     C                   z-add     *zeros        g08inv
093000000830     C                   movel     '0'           g08err
093100000830     C                   call      'XSRDA8'
093200000830     C                   parm                    wlbda8
093300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
093400000830     C*
093500000613     C                   ENDSR
093600000613     C***
