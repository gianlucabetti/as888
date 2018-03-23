000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
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
002500060307     D depspe          s             15    INZ(*blanks)
002600060307     D curspe          s             15    INZ(*blanks)
002700060307     D cntspe          s              7  0 INZ(*zeros)
002800000830
002900000830     D*------------------
003000000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003100000830     D*------------------
003200000830     D WLBDA8          DS                  INZ
003300000830     D  G08DAT                 1      8  0
003400000830     D  G08INV                 9     16  0
003500000830     D  G08ERR                17     17
003600000830     D  G08TGI                18     22  0
003700000830     D*
003800990908
003900060307     D*------------------
004000060307     D* DS REPERIMENTO NUMERATORE
004100060307     D*------------------
004200060307     D trul33ds      e ds                  inz
004300060307     D*------------------
004400060307     D* DS ARCHITETTURA
004500060307     D*------------------
004600060307     D kpjba         e ds                  inz
004700060307     D*------------------
004800010201
004900010201
005000000913     C                   reset                   rrnum
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300000613     C*
005400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005500000613     C*
005600010202     C* Effettuo la chiamata al CLLE preposto
005700040506     C                   call(e)   'TITVVTC'
005800010202     C                   parm                    parccm
005900010202     C                   parm                    parmbr
006000010202     C                   parm      '2'           paropz
006100050201     C*
006200050201     C* Effettuo lancio TISI95 solo x chiusura
006300050201     C                   CLEAR                   TISI95DS
006400050201     C                   EVAL      I95TLA = 'C'
006500050201     C                   CALL      'TISI95R'
006600050201     C                   PARM                    TISI95DS
006700000616     C*
006800000801     C
006900010201     C                   seton                                        LR
007000990908
007100000801
007200910830     C*--------------------------------------------------------
007300040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007400910830     C*--------------------------------------------------------
007500040526     C     RWFILE        BEGSR
007600990910     C*
007700990914     C                   if        not %open(tivin00r)
007800990908     C                   open      tivin00r
007900990914     C                   endif
008000021113     C                   if        not %open(fivabwwr)
008100021113     C                   open      fivabwwr
008200990914     C                   endif
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
011100010305     C*
011200050628     C* Determino il numero di Spedizione e a rottura eseguo operazioni
011300060307     C                   eval      PiStr=%trim(%subst(vindta:221:15))
011400050628     C                   movel(p)  PiStr         curspe
011500050628     C                   add       1             cntspe
011600050628     C*
011700050628     C                   if        cntspe = 1
011800050628     C* A rottura inizializzo campi di totalizzazione
011900050628     C                   clear                   fivab000
012000050628     C*
012100050628     C                   exsr      impvab                                       => carico VAB
012200050628     C                   eval      depspe = curspe
012300050628     C                   else
012400050628     C                   if        curspe <> depspe
012500040802     C                   exsr      wrivab
012600050628     C* A rottura inizializzo campi di totalizzazione
012700050628     C                   clear                   fivab000
012800050628     C*
012900050628     C                   exsr      impvab                                       => carico VAB
013000050628     C                   eval      depspe = curspe
013100050628     C                   else
013200050628     C                   exsr      impvab                                       => carico VAB
013300050628     C                   endif
013400050628     C                   endif
013500000905     C*
013600000905     C                   else
013700000905     C                   eval      vinflg = '1'
013800050628     C                   endif
013900000905     C                   endif
014000000905     C*
014100000905     C  N70              update    tivin000
014200000905     C*
014300991022     C  N70              ENDdo
014400040823     C*
014500040823     C* Al termine della lettura del file scarico il buffer del "VAB" rimasto "in canna"
014600040823     C                   exsr      wrivab
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
016500021113     C                   if        %open(fivabwwr)
016600021113     C                   close     fivabwwr
016700990914     C                   endif
016800021113     C                   if        %open(fivatwwr)
016900021113     C                   close     fivatwwr
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
018600021113     C                   write     fivab000                                     => scarico il VAB
018700010305     C*
018800010305     C                   ENDSR
018900990920
019000000801     C*----------------------------------------------------*
019100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019200000801     C*----------------------------------------------------*
019300010201     C     INZVAR        BEGSR
019400000801     C*
019500040802     C                   Z-ADD     *zeros        Num5_0            5 0
019600040802     C                   MOVEL     '0'           FlgCAS            1
019700060509     C                   MOVEL     *blanks       FlgDSK            1
019800000801     C*
019900000801     C                   ENDSR
020000000801     C*----------------------------------------------------*
020100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020200000801     C*----------------------------------------------------*
020300000801     C     DEFCAM        BEGSR
020400000801     C*
020500020619     C* Imposto i valori di default...
020600060307     C                   Z-ADD     0502978       VABCCM
020700060307     C                   Z-ADD     0502978       VATCCM
020800060307     C                   Z-ADD     050           VABLNP
020900060307     C                   Z-ADD     050           VATLNP
021000040714     C                   Z-ADD     000           VABCTR
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
023700060509     C                   IF        %subst(vlrppt:14:1) <> *blanks
023800060509     C                   EVAL      FlgDSK = %subst(vlrppt:14:1)
023900060509     C                   ENDIF
024000020619     C                   ENDIF
024100000801     C*
024200000801     C                   ENDSR
024300000801     C*----------------------------------------------------*
024400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024500000801     C*----------------------------------------------------*
024600040823     C     IMPVAB        BEGSR
024700040823     C*
024800020305     C                   EXSR      INZVAR
024900020305     C                   EXSR      DEFCAM
025000010305     C*
025100000801     C                   Z-ADD     *zeros        errore            1 0
025200000830     C                   MOVEL     datcor        VABAAS
025300020305     C                   MOVEL     datcor        VATAAS
025400040526     C                   MOVE      datcor        VABMGS
025500040823     C                   MOVE(P)   vlrpoi        VABFGS
025600040823     C                   MOVE(P)   vlrpoi        VATFGS
025700050628     C*
025800060307     C                   EVAL      VABRSD=%trim(%subst(vindta:20:35))
025900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026100020117     C* ==
026200060307     C                   EVAL      VABIND=%trim(%subst(vindta:55:35))
026300061120     C                   EVAL      VABCAD=%trim(%subst(vindta:90:9))
026400060307     C                   EVAL      VABLOD=%trim(%subst(vindta:99:25))
026500060307     C                   EVAL      VABPRD=%trim(%subst(vindta:124:2))
026600060307     C                   EVAL      VABNZD=%trim(%subst(vindta:126:3))
026700060307     C                   EVAL      VABRMA=%trim(%subst(vindta:221:15))
026800060307     C                   EVAL      VABNAS=%trim(%subst(vindta:238:35))
026900060307     C                   EVAL      VABNOT=%trim(%subst(vindta:154:35))
027000060307     C                   EVAL      VABNT2=%trim(%subst(vindta:189:20))
027100060307     C*
027200060307     C* Determino il tipo porto della spedizione corrente
027300081008     C                   IF        %subst(vindta:406:1) = 'M'
027400060307     C                   EVAL      VABCBO = '1'
027500060307     C                   ENDIF
027600081008     C                   IF        %subst(vindta:406:1) = 'D'
027700060307     C                   EVAL      VABCBO = '2'
027800060307     C                   ENDIF
027900060704     C*
028000060704     C* Determino il tipo d traffico il questione
028100060801     C                   IF        %subst(vindta:319:3) = 'DSI'
028200060704     C                   EVAL      VABCTR = 000
028300060704     C                   ENDIF
028400060704     C                   IF        %subst(vindta:319:3) = 'BMC'
028500060914     C                   EVAL      VABCCM = 0503181
028600060704     C                   EVAL      VABCTR = 399
028700060704     C                   ENDIF
028800061016     C                   IF        %subst(vindta:319:3) = 'EEB'
028900061016     C                   EVAL      VABCCM = 0503181
029000061018     C                   EVAL      VABCTR = 091
029100061016     C                   ENDIF
029200050628     C* CAP
029300061120     C*                  EVAL      PiStr=%trim(%subst(vindta:90:9))
029400061120     C*                  EXSR      CHKNUM
029500061120     C*                  IF        PiInt=*on
029600061120     C*                  Z-ADD     PiVal         Num5_0
029700061120     C*                  MOVEL(P)  Num5_0        VABCAD
029800061120     C*                  ELSE
029900061120     C*                  ADD       1             errore
030000061120     C*                  EVAL      vinmsg = %trimr(vinmsg)
030100061120     C*                            + ' ' + 'VABCAD'
030200061120     C*                  ENDIF
030300040506     C* Reperisco la provincia dal CAP e dalla località
030400040526     C                   IF        VABCAD <> *blanks AND
030500040526     C                             VABPRD  = *blanks
030600040506     C                   CLEAR                   TISI95DS
030700040506     C                   EVAL      I95TCN = '3'
030800040506     C                   Z-ADD     datcor        I95DAT
030900040506     C                   EVAL      I95CAP = VABCAD
031000040506     C                   EVAL      I95LOC = VABLOD
031100050627     C                   EVAL      I95NAR = VABNZD
031200040506     C                   CALL      'TISI95R'
031300040506     C                   PARM                    TISI95DS
031400040506     C                   EVAL      VABPRD = O95PRV
031500040506     C                   ENDIF
031600060307     C* NCL
031700060307     C                   EVAL      PiStr=%trim(%subst(vindta:209:3))
031800060307     C                   EXSR      CHKNUM
031900060307     C                   IF        PiInt=*on
032000060307     C                   ADD       PiVal         VABNCL
032100060307     C                   ELSE
032200060307     C                   ADD       1             errore
032300060307     C                   EVAL      vinmsg = %trimr(vinmsg)
032400060307     C                             + ' ' + 'VABNCL'
032500060307     C                   ENDIF
032600040506     C* PKB
032700060307     C                   EVAL      PiStr=%trim(%subst(vindta:212:7))
032800010201     C                   EXSR      CHKNUM
032900010201     C                   IF        PiNum=*on
033000050628     C                   EVAL(h)   Pival = Pival/100                            * gestisco 2 decim.
033100060307     C                   ADD       PiVal         VABPKB
033200010201     C                   ELSE
033300010201     C                   ADD       1             errore
033400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033500010201     C                             + ' ' + 'VABPKB'
033600010201     C                   ENDIF
033700070117     C* RMN
033800070117     C***                EVAL      PiStr=%trim(%subst(vindta:221+3:15-3))
033900070117     C                   EVAL      PiStr=%trim(%subst(vindta:331:30))
034000050928     C                   EXSR      CHKNUM
034100050928     C                   IF        PiInt=*on
034200050928     C                   Z-ADD     PiVal         VABRMN
034300050928     C                   ELSE
034400050928     C                   ADD       1             errore
034500050928     C                   EVAL      vinmsg = %trimr(vinmsg)
034600060307     C                             + ' ' + 'VABRMN'
034700050928     C                   ENDIF
034800060307     C* NSP => Stacco un numeratore da AZNUM
034900060307     C                   clear                   TRUL33DS
035000060307     C                   eval      I33OPE = *zeros
035100060307     C                   eval      I33CNU = 302
035200060307     C                   eval      I33NUM = 1
035300060307     C                   movel     TRUL33DS      KPJBU
035400060307     C                   call      'TRUL33R'
035500060307     C                   parm                    KPJBA
035600060307     C                   movel     KPJBU         TRUL33DS
035700060307     C                   if        O33ERR = *zeros
035800060307     C                   z-add     O33NRF        VABNSP
035900060307     C                   z-add     O33NRF        VATNSP
036000060307     C                   else
036100060307     C                   Z-ADD     1             errore
036200060307     C                   EVAL      vinmsg = %trimr(vinmsg)
036300060307     C                             + ' ' + 'VABNSP VATNSP'
036400060307     C                   endif
036500060307     C* CAS
036600060307     C                   IF        %subst(vindta:273:13) <> *zeros
036700060307     C                   EVAL      FlgCAS = '1'
036800060307     C                   EVAL      VABVCA=%trim(%subst(vindta:286:3))
036900060307     C                   ENDIF
037000060307     C                   EVAL      PiStr=%trim(%subst(vindta:273:13))
037100060307     C                   EXSR      CHKNUM
037200060307     C                   IF        PiNum=*on
037300060307     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
037400060307     C                   ELSE
037500060307     C                   ADD       1             errore
037600060307     C                   EVAL      vinmsg = %trimr(vinmsg)
037700060307     C                             + ' ' + 'VABCAS'
037800060307     C                   ENDIF
037900060307     C* VMD
038000060307     C                   EVAL      PiStr=%trim(%subst(vindta:290:13))
038100060307     C                   EXSR      CHKNUM
038200060307     C                   IF        PiNum=*on
038300060307     C                   EVAL(H)   VABVMD=PiVal/100                             * gestisco 2 dec.
038400060307     C                   ELSE
038500060307     C                   ADD       1             errore
038600060307     C                   EVAL      vinmsg = %trimr(vinmsg)
038700060307     C                             + ' ' + 'VABVMD'
038800060307     C                   ENDIF
038900060307     C* VAD
039000060307     C                   EVAL      VABVAD=%trim(%subst(vindta:286:3))
039100060307     C* IAS
039200060307     C                   EVAL      PiStr=%trim(%subst(vindta:303:13))
039300060307     C                   EXSR      CHKNUM
039400060307     C                   IF        PiNum=*on
039500060307     C                   EVAL(H)   VABIAS=PiVal/100                             * gestisco 2 dec.
039600060307     C                   ELSE
039700060307     C                   ADD       1             errore
039800060307     C                   EVAL      vinmsg = %trimr(vinmsg)
039900060307     C                             + ' ' + 'VABIAS'
040000060307     C                   ENDIF
040100060307     C* VAS
040200060307     C                   EVAL      VABVAS=%trim(%subst(vindta:286:3))
040300060307     C*
040400060307     C* Calcolo il volume del'intera spedizione
040500060307     C                   EVAL      PiStr=%trim(%subst(vindta:322:3))
040600060307     C                   EXSR      CHKNUM
040700060307     C                   IF        PiInt=*on
040800060307     C                   Z-ADD(H)  PiVal         wAlt              7 3
040900060308     C                   EVAL      wAlt  = wAlt/100                             * da cm a metri
041000060307     C                   EVAL      PiStr=%trim(%subst(vindta:325:3))
041100060307     C                   EXSR      CHKNUM
041200060307     C                   IF        PiInt=*on
041300060307     C                   Z-ADD(H)  PiVal         wLarg             7 3
041400060308     C                   EVAL      wLarg = wLarg/100                            * da cm a metri
041500060307     C                   EVAL      PiStr=%trim(%subst(vindta:328:3))
041600060307     C                   EXSR      CHKNUM
041700060307     C                   IF        PiInt=*on
041800060307     C                   Z-ADD(H)  PiVal         wProf             7 3
041900060307     C                   EVAL      wProf = wProf/100                            * da cm a metri
042000060307     C                   ENDIF
042100060307     C                   ENDIF
042200060307     C                   ENDIF
042300060331     C***                EVAL      VABVLB=VABVLB+(wAlt*wLarg*wProf*VABNCL)
042400060307     C*
042500060509     C* Se richiesto nei parametri scrivo estensione dettaglio bolle - tipo record "E" => "CHI SONO"
042600060509     C                   if        FlgDSK = 'C'
042700060509     C                   MOVEL     '7Q'          VABCTM
042800060307     C                   exsr      exevate
042900060509     C                   endif
043000060307     C* ...scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE DEST.
043100060307     C                   exsr      exevata
043200060307     C* ...scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DEST.
043300060307     C                   exsr      exevatb
043400010205     C*
043500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043600040802     C                   IF        FlgCAS <> '0'
043700081008     C                   IF        VABCBO = '1'
043800010205     C                   EVAL      VABCBO = '4'
043900081008     C                   ENDIF
044000081008     C                   IF        VABCBO = '2'
044100081008     C                   EVAL      VABCBO = '6'
044200010205     C                   ENDIF
044300081008     C                   ENDIF
044400020305     C*
044500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044600011113     C                   EXSR      CHKIMPDIV
044700010202     C*
044800000801     C* Ebbene...
044900000801     C                   ADD       1             §CTRMO
045000010201     C                   IF        errore <> *zeros
045100000801     C                   ADD       1             §CTRNO
045200000801     C                   EVAL      vinflg = '2'
045300000801     C                   ELSE
045400010201     C                   ADD       1             §CTROKVB
045500000801     C                   ENDIF
045600000801     C*
045700000801     C                   ENDSR
045800050628     C*----------------------------------------------------*
045900060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
046000050628     C*----------------------------------------------------*
046100060307     C     EXEVATE       BEGSR
046200050628     C*
046300050628     C                   EXSR      INZVAR
046400060705     C                   EVAL      VATAAS = VABAAS
046500060705     C                   EVAL      VATLNP = VABLNP
046600060705     C                   EVAL      VATNSP = VABNSP
046700060705     C                   EVAL      VATCCM = VABCCM
046800050628     C*
046900050628     C* Valorizzo il barcode
047000061016     C                   IF        %subst(vindta:319:3) = 'DSI' OR
047100061016     C                             %subst(vindta:319:3) = 'EEB'
047200050628     C                   EVAL      VATTRC='E'
047300070111     C                   EVAL      VATNOT = %trim(%subst(vindta:331:30))
047400070118     C                   Z-ADD     *zeros        wStep             3 0
047500070117     C                   Z-ADD     *zeros        wLen              2 0
047600070117     C                   EVAL      wLen = %len(%trim(VATNOT))
047700070117     C                   IF        wLen <= 7
047800070117     C                   EVAL      wStep = 100
047900070117     C                   ENDIF
048000070117     C                   IF        wLen  = 8
048100070117     C                   EVAL      wStep = 10
048200070117     C                   ENDIF
048300070117     C                   IF        wLen >= 9
048400070117     C                   EVAL      wStep = 1
048500070117     C                   ENDIF
048600070117     C                   MOVEL(P)  VATNOT        wBarcode          9 0
048700060801     C                   Z-ADD     0             wCollo            5 0
048800060801     C                   DOW       wCollo <  VABNCL
048900050628     C                   exsr      wrivat                                       => scarico VAT
049000060801     C                   ADD       1             wCollo
049100070117     C                   ADD       wStep         wBarcode
049200070117     C                   MOVEL(P)  wBarcode      VATNOT
049300070117     C                   EVAL      VATNOT = %subst(VATNOT:1:wLen)
049400060801     C                   ENDDO
049500060801     C                   ELSE
049600060801     C                   EVAL      VATTRC='E'
049700060801     C                   EVAL      VATNOT = %trim(%subst(vindta:331:30))
049800060801     C                   exsr      wrivat                                       => scarico VAT
049900060801     C                   ENDIF
050000050628     C*
050100050628     C                   ENDSR
050200060307     C*----------------------------------------------------*
050300060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
050400060307     C*----------------------------------------------------*
050500060307     C     EXEVATA       BEGSR
050600060307     C*
050700060307     C                   EXSR      INZVAR
050800060705     C                   EVAL      VATAAS = VABAAS
050900060705     C                   EVAL      VATLNP = VABLNP
051000060705     C                   EVAL      VATNSP = VABNSP
051100060705     C                   EVAL      VATCCM = VABCCM
051200060307     C*
051300060307     C* Valorizzo il barcode
051400060307     C                   EVAL      VATTRC='A'
051500060307     C                   EVAL      VATNOT = %trim(%subst(vindta:154:35))
051600060307     C                   exsr      wrivat                                       => scarico VAT
051700060307     C*
051800060307     C                   ENDSR
051900060307     C*----------------------------------------------------*
052000060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
052100060307     C*----------------------------------------------------*
052200060307     C     EXEVATB       BEGSR
052300060307     C*
052400060307     C                   EXSR      INZVAR
052500060705     C                   EVAL      VATAAS = VABAAS
052600060705     C                   EVAL      VATLNP = VABLNP
052700060705     C                   EVAL      VATNSP = VABNSP
052800060705     C                   EVAL      VATCCM = VABCCM
052900060307     C*
053000060307     C* Valorizzo il barcode
053100060307     C                   EVAL      VATTRC='B'
053200060307     C                   EVAL      VATNOT = %trim(%subst(vindta:189:20))
053300060307     C                   exsr      wrivat                                       => scarico VAT
053400060307     C*
053500060307     C                   ENDSR
053600010201     C*----------------------------------------------------*
053700040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
053800010201     C*----------------------------------------------------*
053900020305     C     WRIVAT        BEGSR
054000050628     C*
054100060223     C* Scrivo solo se valorizzato qualcosa
054200060223     C                   IF        VATNOT <> *blanks
054300040802     C                   WRITE     FIVAT000
054400060223     C                   ENDIF
054500010201     C*
054600010201     C                   ENDSR
054700010202     C*----------------------------------------------------*
054800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054900010202     C*----------------------------------------------------*
055000020305     C     PREVAT        BEGSR
055100010202     C*
055200021113     C* Compongo il nome del membro da dare al FIVATWWR
055300010202     C                   eval      parmbr = vlrhdl
055400010202     C                   movel     'M'           parmbr
055500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
055600010202     C                   eval      paropz = '1'
055700010202     C* Effettuo la chiamata al CLLE preposto
055800040506     C                   call(e)   'TITVVTC'
055900010202     C                   parm                    parccm
056000010202     C                   parm                    parmbr
056100010202     C                   parm                    paropz
056200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056300010202     C                   if        %error
056400010202     C                   movel     '1'           chkcall
056500010202     C                   else
056600010202     C                   movel     '0'           chkcall
056700010202     C                   endif
056800010202     C*
056900010202     C                   ENDSR
057000000801     C*----------------------------------------------------*
057100000801     C*  CONTROLLO NUMERICITA' CAMPI
057200000801     C*----------------------------------------------------*
057300000801     C     CHKNUM        BEGSR
057400000801     C*
057500000801     C                   call(e)   'ISNUMERIC'
057600000801     C                   PARM                    PiStr            30
057700050627     C                   PARM      ','           PiDecChr          1
057800000801     C                   PARM      *ZEROS        PiVal            30 9
057900000801     C                   PARM      '0'           PiInt             1
058000000801     C                   PARM      '0'           PiNum             1
058100000801     C                   IF        %error
058200000801     C                   EVAL      PiInt=*off
058300000801     C                   ENDIF
058400000801     C*
058500000801     C                   ENDSR
058600000801     C***
058700000801
058800011113
058900011113     C*----------------------------------------------------*
059000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059100011113     C*----------------------------------------------------*
059200011113     C     CHKIMPDIV     BEGSR
059300011113     C*
059400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059500011113     C                   Z-ADD     *zeros        wrkDec            9 9
059600011113     C*
059700011113     C* Come prima cosa effettuo considerazioni sulla divisa
059800011113     C                   IF        vabIAS > *zeros
059900011113     C                   IF        vabVAS <> 'EUR'
060000011113     C                   EVAL      vabVAS =  'ITL'
060100011113     C                   ENDIF
060200011113     C                   ENDIF
060300011113     C*
060400011113     C                   IF        vabCAS > *zeros
060500011113     C                   IF        vabVCA <> 'EUR'
060600011113     C                   EVAL      vabVCA =  'ITL'
060700011113     C                   ENDIF
060800011113     C                   ENDIF
060900011113     C*
061000011113     C                   IF        vabVMD > *zeros
061100020305     C                   IF        vabVAD <> 'EUR'
061200011113     C                   EVAL      vabVAD =  'ITL'
061300011113     C                   ENDIF
061400011113     C                   ENDIF
061500011113     C*
061600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061700011113     C                   Z-ADD     vabIAS        wrkDec
061800011113     C                   IF        wrkDec > *zeros
061900011113     C                   IF        vabVAS = 'ITL'
062000011113     C                   EVAL      vabIAS = *zeros
062100011113     C                   ENDIF
062200011113     C                   ENDIF
062300011113     C*
062400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062500011113     C                   Z-ADD     vabCAS        wrkDec
062600011113     C                   IF        wrkDec > *zeros
062700011113     C                   IF        vabVCA = 'ITL'
062800011113     C                   EVAL      vabCAS = *zeros
062900011113     C                   ENDIF
063000011113     C                   ENDIF
063100011113     C*
063200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063300011113     C                   Z-ADD     vabVMD        wrkDec
063400011113     C                   IF        wrkDec > *zeros
063500011113     C                   IF        vabVAD = 'ITL'
063600011113     C                   EVAL      vabVMD = *zeros
063700011113     C                   ENDIF
063800011113     C                   ENDIF
063900011113     C*
064000011113     C                   ENDSR
064100011113     C***
064200011113
064300011113
064400000801
064500000801
064600990920      /TITLE Invio dei dati al punto operativo.
064700010202     C     invio         BEGSR
064800990920     C*
064900021113     C* 1° invio FIVAT
065000010201     C                   reset                   dscmz
065100010201     C                   move      vlrpoi        cmzdst
065200021113     C                   eval      cmzfld = 'FIVATWWR'
065300010201     C                   eval      cmzmbd = vlrhdl
065400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065500021009     C***                if        prmfir = *blanks
065600021113     C                   eval      cmzfla = 'FIVAT00F'
065700021113     C                   eval      cmzmba = 'FIVAT00F'
065800021009     C***                else
065900021009     C***                eval      cmzfla = prmfir
066000021009     C***                eval      cmzmba = prmfir
066100021009     C***                endif
066200010201     C                   eval      cmznrr = *zeros
066300020305     C                   move      §ctrokvt      cmznrr
066400021018     C                   eval      cmzlba = vlrfl1
066500010201     C                   call(e)   'TIS711C'
066600010201     C                   parm                    dscmz
066700010201     C                   parm      *blanks       esito
066800010205     C                   if        %error
066900010205     C                             or cmzerr = '1'
067000010205     C                             or esito  = '1'
067100010205     C                   eval      wrkesito = '3'
067200010205     C                   else
067300010201     C*
067400021113     C* 2° invio FIVAB
067500010201     C                   reset                   dscmz
067600010201     C                   move      vlrpoi        cmzdst
067700010201     C                   eval      cmzfld = vlrfou
067800010201     C                   eval      cmzmbd = vlrhdl
067900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068000021009     C***                if        prmfir = *blanks
068100021113     C                   eval      cmzfla = 'FIVAB00F'
068200021113     C                   eval      cmzmba = 'FIVAB00F'
068300021009     C***                else
068400021009     C***                eval      cmzfla = prmfir
068500021009     C***                eval      cmzmba = prmfir
068600021009     C***                endif
068700010201     C                   eval      cmznrr = *zeros
068800010201     C                   move      §ctrokvb      cmznrr
068900021018     C                   eval      cmzlba = vlrfl1
069000010201     C                   call(e)   'TIS711C'
069100010201     C                   parm                    dscmz
069200010201     C                   parm      *blanks       esito
069300010201     C                   if        %error
069400010201     C                             or cmzerr = '1'
069500010201     C                             or esito  = '1'
069600010201     C                   eval      wrkesito = '3'
069700010201     C                   endif
069800010205     C                   endif
069900990920     C*
070000000613     C                   ENDSR
070100000613     C***
070200070411
070300070411     C     *pssr         BEGSR
070400070411     C*
070500070411     C                   if        %open(tivin00r)
070600070411     C                   close     tivin00r
070700070411     C                   endif
070800070411     C                   if        %open(fivabwwr)
070900070411     C                   close     fivabwwr
071000070411     C                   endif
071100070411     C                   if        %open(fivatwwr)
071200070411     C                   close     fivatwwr
071300070411     C                   endif
071400070411     C*
071500070411     C* Effettuo la chiamata al CLLE preposto
071600070411     C                   call(e)   'TITVVTC'
071700070411     C                   parm                    parccm
071800070411     C                   parm                    parmbr
071900070411     C                   parm      '2'           paropz
072000070411     C*
072100070411     C                   eval      wrkesito = '2'
072200070411     C*
072300070411     C                   seton                                        LR
072400070411     C*
072500070411     C                   ENDSR     '*CANCL'
072600070411     C***
072700070411
072800990910
072900000613     C     *inzsr        BEGSR
073000990910     C*
073100990910     C     *entry        plist
073200990920     C                   parm                    tivlrds
073300990921     C                   parm      wrkesito      esito
073400000724     C                   parm                    prmlit
073500000710     C                   parm                    prmfir
073600000613     C*
073700000830     C* CALCOLA LA DATA CORRENTE
073800000830     C                   time                    wn14             14 0
073900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
074000000830     C                   z-add     wn8           g08dat
074100000830     C                   z-add     *zeros        g08inv
074200000830     C                   movel     '0'           g08err
074300000830     C                   call      'XSRDA8'
074400000830     C                   parm                    wlbda8
074500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
074600000830     C*
074700000613     C                   ENDSR
074800000613     C***
