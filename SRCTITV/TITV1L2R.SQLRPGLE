000100080512      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500080512     FEDIVABwr  O    E             DISK    usropn
000600080512     FEDIVATwr  O    E             DISK    usropn
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
002500081217     D curSped         s              6    INZ(*blanks)
002600081217     D depSped         s              6    INZ(*blanks)
002700081217     D wNomeFile       s             30    INZ(*blanks)
002800000830
002900000830     D*------------------
003000000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003100000830     D*------------------
003200000830     D WLBDA8          DS                  INZ
003300000830     D  G08DAT                 1      8  0
003400000830     D  G08INV                 9     16  0
003500000830     D  G08ERR                17     17
003600000830     D  G08TGI                18     22  0
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
006100080512     C                   call(e)   'TITVEVTC'
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
007700080512     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007800910830     C*--------------------------------------------------------
007900040526     C     RWFILE        BEGSR
008000990910     C*
008100990914     C                   if        not %open(tivin00r)
008200990908     C                   open      tivin00r
008300990914     C                   endif
008400080512     C                   if        not %open(edivabwr)
008500080512     C                   open      edivabwr
008600990914     C                   endif
008700070103     C*
008800080512     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008900020305     C                   exsr      prevat
009000010201     C*
009100010202     C                   if        chkcall = '0'
009200010202     C*
009300080512     C                   if        not %open(edivatwr)
009400080512     C                   open      edivatwr
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
011800081217     C                   eval      curSped = %subst(vindta:1:6)
011900070213     C                   if        curSped <> depSped
012000070213     C* Se prima bolla => importo bolla corrente
012100070213     C                   if        §CTRMO = *zeros
012200070213     C                   exsr      inzvar
012300070213     C                   exsr      defcam
012400070213     C                   exsr      impvab                                       => carico  VAB
012500070213     C                   else
012600070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
012700070213     C                   exsr      wrivab                                       => scarico VAB
012800070213     C                   exsr      inzvar
012900070213     C                   exsr      defcam
013000070213     C                   exsr      impvab                                       => carico  VAB
013100070213     C                   endif
013200070103     C* Salvo il raggruppamento spedizione cliente corrente
013300070103     C                   eval      depSped = curSped
013400070213     C*
013500070213     C* Se collo successivo x stessa bolla
013600070213     C                   else
013700070213     C                   exsr      impvab                                       => carico  VAB
013800070103     C                   endif
013900090618     C*
014000090618     C* Scrivo sempre le estensioni del record corrente
014100090618     C                   exsr      exevate                                      => write VAT-E
014200090618     C                   exsr      exevata
014300090618     C                   exsr      exevatb
014400000905     C*
014500000905     C                   else
014600000905     C                   eval      vinflg = '1'
014700050628     C                   endif
014800000905     C                   endif
014900000905     C*
015000000905     C  N70              update    tivin000
015100000905     C*
015200991022     C  N70              ENDdo
015300070213     C*
015400070213     C* Scarico testata bolla rimasta in sospesa
015500070213     C                   exsr      wrivab                                       => scarico VAB
015600010202     C*
015700010202     C                   endif
015800990910
015900990910     C* Se non ci sono record con errori ...
016000000710     C                   if        §ctrno = 0
016100990910     C* ... restituisco esito OK.
016200990921     C                   eval      wrkesito = '0'
016300990910     C                   else
016400010201     C                   if        §ctrokvb > 0
016500990921     C                   eval      wrkesito = '1'
016600000710     C                   else
016700000710     C                   eval      wrkesito = '2'
016800990910     C                   endif
016900000710     C                   endif
017000990910     C*
017100990914     C                   if        %open(tivin00r)
017200990908     C                   close     tivin00r
017300990914     C                   endif
017400080512     C                   if        %open(edivabwr)
017500080512     C                   close     edivabwr
017600990914     C                   endif
017700080512     C                   if        %open(edivatwr)
017800080512     C                   close     edivatwr
017900010201     C                   endif
018000990910     C*
018100010201     C                   if        §ctrokvb > 0
018200000724     C                             and vlrpoi <> *zeros
018300010202     C                   exsr      invio
018400990920     C                   endif
018500990920     C*
018600910830     C                   ENDSR
018700000613     C***
018800010305
018900010305     C*----------------------------------------------------*
019000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019100010305     C*----------------------------------------------------*
019200020305     C     WRIVAB        BEGSR
019300080512     C*
019400080512     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
019500090115     C                   EVAL      VABCMR = %trim(wNomeFile) + ' ' +
019600090115     C                                      %char(datcor)
019700080512     C                   EVAL      VABDCM = DATCOR
019800080512     C                   EVAL      VABDTS = DATCOR
019900080512     C                   EVAL      VABHMS = ORACOR
020000080512     C                   EVAL      VABCNT = 1
020100010305     C*
020200060225     C* Quindi scarico il buffer del file d testata
020300080512     C                   write     edivab00                                     => scarico il VAB
020400010305     C*
020500010305     C                   ENDSR
020600990920
020700000801     C*----------------------------------------------------*
020800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020900000801     C*----------------------------------------------------*
021000010201     C     INZVAR        BEGSR
021100000801     C*
021200080512     C                   CLEAR                   EDIVAB00
021300080512     C                   CLEAR                   EDIVAT00
021400070213     C*
021500040802     C                   Z-ADD     *zeros        Num5_0            5 0
021600040802     C                   MOVEL     '0'           FlgCAS            1
021700000801     C*
021800000801     C                   ENDSR
021900000801     C*----------------------------------------------------*
022000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
022100000801     C*----------------------------------------------------*
022200000801     C     DEFCAM        BEGSR
022300000801     C*
022400020619     C* Imposto i valori di default...
022500081217     C                   Z-ADD     1161477       VABCCM
022600081217     C                   Z-ADD     1161477       VATCCM
022700081217     C                   Z-ADD     116           VABLNP
022800081217     C                   Z-ADD     116           VATLNP
022900081217     C                   Z-ADD     000           VABCTR
023000070103     C                   MOVEL     '7Q'          VABCTM
023100040823     C                   MOVEL     '1'           VABCBO
023200020619     C* ... e poi verifico se sono stati passati come parametri
023300020619     C                   IF        vlrppt > *blanks
023400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023600020619     C                   EXSR      CHKNUM
023700020619     C                   IF        PiInt=*on
023800020619     C                   Z-ADD     PiVal         VABCCM
023900020619     C                   Z-ADD     PiVal         VATCCM
024000020619     C                   ENDIF
024100040506     C                   ENDIF
024200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024400020619     C                   EXSR      CHKNUM
024500020619     C                   IF        PiInt=*on
024600020619     C                   Z-ADD     PiVal         VABLNP
024700020619     C                   Z-ADD     PiVal         VATLNP
024800040506     C                   ENDIF
024900020619     C                   ENDIF
025000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
025100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025200020619     C                   EXSR      CHKNUM
025300020619     C                   IF        PiInt=*on
025400020619     C                   Z-ADD     PiVal         VABCTR
025500040506     C                   ENDIF
025600020619     C                   ENDIF
025700020619     C                   ENDIF
025800000801     C*
025900000801     C                   ENDSR
026000000801     C*----------------------------------------------------*
026100080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026200000801     C*----------------------------------------------------*
026300040823     C     IMPVAB        BEGSR
026400010305     C*
026500000801     C                   Z-ADD     *zeros        errore            1 0
026600000830     C                   MOVEL     datcor        VABAAS
026700020305     C                   MOVEL     datcor        VATAAS
026800040526     C                   MOVE      datcor        VABMGS
026900040823     C                   MOVE(P)   vlrpoi        VABFGS
027000040823     C                   MOVE(P)   vlrpoi        VATFGS
027100080409     C*
027200080409     C* Reperimento campi ALFA
027300080409     C*
027400080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
027500081217     C                   EVAL      VABRSD=%trim(%subst(vindta:58:32))
027600080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027700080409     C     '@':'A'       XLATE     VABRSD        VABRSD
027800080409     C* ==
027900081217     C                   EVAL      VABRD2=%trim(%subst(vindta:90:30))
028000081217     C                   EVAL      VABIND=%trim(%subst(vindta:120:31))
028100081217     C                   EVAL      VABLOD=%trim(%subst(vindta:162:31))
028200090113     C***                EVAL      VABPRD=%trim(%subst(vindta:193:2))
028300081217     C                   EVAL      VABRMA=%trim(%subst(vindta:32:5))
028400090415     C*
028500090415     C                   IF        %subst(vindta:32:5) = 'R1666'
028600090415     C                   EVAL      VABCCM = 1161545
028700090415     C                   EVAL      VATCCM = 1161545
028800090415     C                   ENDIF
028900090415     C                   IF        %subst(vindta:32:5) = 'N1551'
029000090415     C                   EVAL      VABCCM = 1161548
029100090415     C                   EVAL      VATCCM = 1161548
029200090415     C                   ENDIF
029300090415     C                   IF        %subst(vindta:32:5) = 'V1665'
029400090415     C                   EVAL      VABCCM = 1161544
029500090415     C                   EVAL      VATCCM = 1161544
029600090415     C                   ENDIF
029700090415     C                   IF        %subst(vindta:32:5) = 'W1470'
029800090415     C                   EVAL      VABCCM = 1161477
029900090415     C                   EVAL      VATCCM = 1161477
030000090415     C                   ENDIF
030100090415     C                   IF        %subst(vindta:32:5) = 'M1552'
030200160125     C                   EVAL      VABCCM = 0055069
030300160125     C                   EVAL      VATCCM = 0055069
030400090415     C                   ENDIF
030500090415     C                   IF        %subst(vindta:32:5) = 'L1549'
030600090415     C                   EVAL      VABCCM = 1161477
030700090415     C                   EVAL      VATCCM = 1161477
030800090415     C                   ENDIF
030900090519     C*
031000090519     C                   IF        %subst(vindta:32:5) = 'TNF  '
031100090519     C                   EVAL      VABCCM = 0055068
031200090519     C                   EVAL      VATCCM = 0055068
031300090519     C                   ENDIF
031400090519     C*
031500090519     C                   IF        %subst(vindta:32:5) = 'SAM  '
031600090519     C                   EVAL      VABCCM = 0055069
031700090519     C                   EVAL      VATCCM = 0055069
031800090519     C                   ENDIF
031900090519     C*
032000090519     C                   IF        %subst(vindta:32:5) = '7FAM '
032100090519     C                   EVAL      VABCCM = 0055072
032200090519     C                   EVAL      VATCCM = 0055072
032300090519     C                   ENDIF
032400090519     C*
032500090519     C                   IF        %subst(vindta:32:5) = 'EP   '
032600090519     C                   EVAL      VABCCM = 0055074
032700090519     C                   EVAL      VATCCM = 0055074
032800090519     C                   ENDIF
032900090519     C*
033000090519     C                   IF        %subst(vindta:32:5) = 'KIP  '
033100090519     C                   EVAL      VABCCM = 0055075
033200090519     C                   EVAL      VATCCM = 0055075
033300090519     C                   ENDIF
033400100614     C*
033500100614     C                   IF        %subst(vindta:32:5) = 'TNFE '
033600100614     C                   EVAL      VABCCM = 0055622
033700100614     C                   EVAL      VATCCM = 0055622
033800100614     C                   ENDIF
033900090415     C*
034000081217     C                   IF        %subst(vindta:38:1) = 'Y'
034100081217     C                   EVAL      FlgCAS = '1'
034200081217     C                   EVAL      VABTIC = 'CM'
034300081217     C                   ENDIF
034400080409     C*
034500080409     C* Reperimento campi NUMERICI
034600081217     C* NSP
034700081217     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
034800080409     C                   EXSR      CHKNUM
034900080409     C                   IF        PiInt=*on
035000081217     C                   Z-ADD     PiVal         VABNSP
035100081217     C                   Z-ADD     PiVal         VATNSP
035200080409     C                   ELSE
035300080409     C                   ADD       1             errore
035400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
035500081217     C                             + ' ' + 'VABNSP VATNSP'
035600080409     C                   ENDIF
035700081217     C* RMN
035800081217     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
035900081217     C                   EXSR      CHKNUM
036000081217     C                   IF        PiInt=*on
036100081217     C                   Z-ADD     PiVal         VABRMN
036200081217     C                   ELSE
036300081217     C                   ADD       1             errore
036400081217     C                   Z-ADD     1             VABRMN
036500081217     C                   EVAL      vinmsg = %trimr(vinmsg)
036600081217     C                             + ' ' + 'VABRMN'
036700081217     C                   ENDIF
036800080409     C* CAD
036900081223     C                   EVAL      VABCAD=%trim(%subst(vindta:151:9))
037000081223     C                   EVAL      VABNZD=%trim(%subst(vindta:224:3))
037100081223     C                   IF        VABNZD='IT'
037200081223     C                   EVAL      VABNZD=*blanks
037300081223     C                   ENDIF
037400081223     C*                  EVAL      PiStr=%trim(%subst(vindta:151:5))
037500081223     C*                  EXSR      CHKNUM
037600081223     C*                  IF        PiInt=*on
037700081223     C*                  Z-ADD     PiVal         Num5_0
037800081223     C*                  MOVEL(p)  Num5_0        VABCAD
037900081223     C*                  ELSE
038000081223     C*                  ADD       1             errore
038100081223     C*                  EVAL      VABCAD = *zeros
038200081223     C*                  EVAL      vinmsg = %trimr(vinmsg)
038300081223     C*                            + ' ' + 'VABCAD'
038400081223     C*                  ENDIF
038500090113     C*
038600090113     C* Reperisco la provincia dal CAP e dalla località
038700090113     C                   IF        VABCAD <> *blanks
038800090113     C                   CLEAR                   TISI95DS
038900090113     C                   EVAL      I95TCN = '3'
039000090113     C                   Z-ADD     datcor        I95DAT
039100090113     C                   EVAL      I95CAP = VABCAD
039200090113     C                   EVAL      I95LOC = VABLOD
039300090113     C                   CALL      'TISI95R'
039400090113     C                   PARM                    TISI95DS
039500090113     C                   EVAL      VABPRD = O95PRV
039600090113     C                   ENDIF
039700081217     C* NCL
039800081217     C                   EVAL      PiStr=%trim(%subst(vindta:18:4))
039900081217     C                   EXSR      CHKNUM
040000081217     C                   IF        PiInt=*on
040100081217     C                   Z-ADD     PiVal         VABNCL
040200081217     C                   ELSE
040300081217     C                   EVAL      vinmsg = %trimr(vinmsg)
040400081217     C                             + ' ' + 'VABNCL'
040500081217     C                   ENDIF
040600081217     C* PKB
040700081217     C*
040800081217     C/EXEC SQL
040900081217     C+ declare C1 cursor for
041000081217     C+ select sum(decimal(substr(vindta, 24, 7))/1000) as peso
041100081217     C+ from tivin00r
041200081217     C+ where substr(vindta, 1, 6) = :curSped
041300081217     C+ for read only
041400081217     C/END-EXEC
041500081217     C
041600081217     C/EXEC SQL
041700081217     C+ open C1
041800081217     C/END-EXEC
041900081217     C                   IF        sqlcod <= *zeros
042000081217     C
042100081217     C/EXEC SQL
042200081217     C+ Fetch C1 into :VABPKB
042300081217     C/END-EXEC
042400081217     C
042500081217     C/EXEC SQL
042600081217     C+ close C1
042700081217     C/END-EXEC
042800081217     C*
042900081217     C                   ELSE
043000081217     C                   ADD       1             errore
043100081217     C                   EVAL      vinmsg = %trimr(vinmsg)
043200081217     C                             + ' ' + 'VABPKB'
043300081217     C                   ENDIF
043400090115     C*
043500090115     C* ==> STEP 2
043600090115     C                   IF        %subst(vindta:255:404-255+1) <> *blanks
043700090115     C*
043800090218     C                   EVAL      VABTC1=%trim(%subst(vindta:435:1))
043900090115     C                   IF        %subst(vindta:376:1) = 'E'
044000090115     C                   EVAL      VABTSP = 'E'
044100090115     C                   ENDIF
044200090115     C* CAS
044300090115     C                   IF        FlgCAS = '1'
044400090115     C                   IF        %trim(%subst(vindta:267:16))<>
044500090115     C                             '0000000000000.00'
044600090115     C                   EVAL      VABVCA = 'EUR'
044700090115     C                   EVAL      PiStr=%trim(%subst(vindta:267:16))
044800090115     C                   EXSR      CHKNUM
044900090115     C                   IF        PiNum=*on
045000090115     C                   Z-ADD     PiVal         VABCAS
045100090115     C                   ELSE
045200090115     C                   ADD       1             errore
045300090115     C                   Z-ADD     *zeros        VABCAS
045400090115     C                   EVAL      vinmsg = %trimr(vinmsg)
045500090115     C                             + ' ' + 'VABCAS'
045600090115     C                   ENDIF
045700090115     C                   ENDIF
045800090115     C                   ENDIF
045900090115     C* IAS
046000090115     C                   IF        %trim(%subst(vindta:378:16))<>
046100090115     C                             '0000000000000.00'
046200090115     C                   EVAL      VABVAS = 'EUR'
046300090115     C                   EVAL      PiStr=%trim(%subst(vindta:378:16))
046400090115     C                   EXSR      CHKNUM
046500090115     C                   IF        PiNum=*on
046600090115     C                   Z-ADD     PiVal         VABIAS
046700090115     C                   ELSE
046800090115     C                   ADD       1             errore
046900090115     C                   Z-ADD     *zeros        VABIAS
047000090115     C                   EVAL      vinmsg = %trimr(vinmsg)
047100090115     C                             + ' ' + 'VABIAS'
047200090115     C                   ENDIF
047300090115     C                   ENDIF
047400090115     C* DCR
047500090115     C                   IF        %subst(vindta:395:8) <> *blanks
047600090115     C                   EVAL      PiStr=%subst(vindta:395:8)
047700090115     C                   EXSR      CHKNUM
047800090115     C                   IF        PiInt=*on
047900090115     C                   Z-ADD     PiVal         VABDCR
048000090115     C                   ELSE
048100090115     C                   ADD       1             errore
048200090115     C                   Z-ADD     *zeros        VABDCR
048300090115     C                   EVAL      vinmsg = %trimr(vinmsg)
048400090115     C                             + ' ' + 'VABDCR'
048500090115     C                   ENDIF
048600090115     C                   ENDIF
048700090115     C*
048800090115     C                   ENDIF                                                  <= STEP 2
048900010205     C*
049000010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
049100040802     C                   IF        FlgCAS <> '0'
049200010205     C                   EVAL      VABCBO = '4'
049300010205     C                   ENDIF
049400020305     C*
049500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049600011113     C                   EXSR      CHKIMPDIV
049700010202     C*
049800000801     C* Ebbene...
049900000801     C                   ADD       1             §CTRMO
050000010201     C                   IF        errore <> *zeros
050100000801     C                   ADD       1             §CTRNO
050200000801     C                   EVAL      vinflg = '2'
050300000801     C                   ELSE
050400010201     C                   ADD       1             §CTROKVB
050500000801     C                   ENDIF
050600000801     C*
050700000801     C                   ENDSR
050800070102     C*----------------------------------------------------*
050900080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "E"
051000070102     C*----------------------------------------------------*
051100070103     C     EXEVATE       BEGSR
051200070102     C*
051300070103     C                   EVAL      VATTRC='E'
051400081217     C                   EVAL      VATNOT=%trim(%subst(vindta:8:8))
051500080512     C*
051600080512     C                   exsr      wrivat                                       => scarico VAT
051700070102     C*
051800070102     C                   ENDSR
051900080415     C*----------------------------------------------------*
052000080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
052100080415     C*----------------------------------------------------*
052200080415     C     EXEVATA       BEGSR
052300080415     C*
052400090115     C                   EVAL      VATTRC='A'
052500090115     C                   EVAL      VATNOT=%trim(%subst(vindta:284:30))
052600090115     C*
052700090115     C                   exsr      wrivat                                       => scarico VAT
052800080415     C*
052900080415     C                   ENDSR
053000071121     C*----------------------------------------------------*
053100080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
053200071121     C*----------------------------------------------------*
053300071121     C     EXEVATB       BEGSR
053400071121     C*
053500090115     C                   EVAL      VATTRC='B'
053600090115     C                   EVAL      VATNOT=%trim(%subst(vindta:315:19))
053700071121     C*
053800090115     C                   exsr      wrivat                                       => scarico VAT
053900071121     C*
054000071121     C                   ENDSR
054100010201     C*----------------------------------------------------*
054200080512     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
054300010201     C*----------------------------------------------------*
054400020305     C     WRIVAT        BEGSR
054500080512     C*
054600080512     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
054700090115     C                   EVAL      VATCMR = %trim(wNomeFile) + ' ' +
054800090115     C                                      %char(datcor)
054900080512     C                   EVAL      VATCNT = 1
055000050628     C*
055100060223     C* Scrivo solo se valorizzato qualcosa
055200060223     C                   IF        VATNOT <> *blanks
055300080512     C                   WRITE     EDIVAT00
055400060223     C                   ENDIF
055500010201     C*
055600010201     C                   ENDSR
055700010202     C*----------------------------------------------------*
055800080512     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
055900010202     C*----------------------------------------------------*
056000020305     C     PREVAT        BEGSR
056100010202     C*
056200080512     C* Compongo il nome del membro da dare al EDIVATWR
056300010202     C                   eval      parmbr = vlrhdl
056400010202     C                   movel     'M'           parmbr
056500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
056600010202     C                   eval      paropz = '1'
056700010202     C* Effettuo la chiamata al CLLE preposto
056800080512     C                   call(e)   'TITVEVTC'
056900010202     C                   parm                    parccm
057000010202     C                   parm                    parmbr
057100010202     C                   parm                    paropz
057200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057300010202     C                   if        %error
057400010202     C                   movel     '1'           chkcall
057500010202     C                   else
057600010202     C                   movel     '0'           chkcall
057700010202     C                   endif
057800010202     C*
057900010202     C                   ENDSR
058000000801     C*----------------------------------------------------*
058100000801     C*  CONTROLLO NUMERICITA' CAMPI
058200000801     C*----------------------------------------------------*
058300000801     C     CHKNUM        BEGSR
058400081217     C*
058500081217     C                   IF        PiDecChr = *blanks
058600081217     C                   EVAL      PiDecChr = '.'
058700081217     C                   ENDIF
058800081217     C*
058900081217     C                   callp(e)  UBISNUM_Check(PiStr
059000081217     C                                          :PiDecChr
059100081217     C                                          :PiVal
059200081217     C                                          :PiNum
059300081217     C                                          :PiInt)
059400081217     C*
059500081217     C                   IF        %error
059600081217     C                   EVAL      PiInt=*off
059700081217     C                   ENDIF
059800000801     C*
059900000801     C                   ENDSR
060000000801     C***
060100000801
060200011113
060300011113     C*----------------------------------------------------*
060400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060500011113     C*----------------------------------------------------*
060600011113     C     CHKIMPDIV     BEGSR
060700011113     C*
060800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060900011113     C                   Z-ADD     *zeros        wrkDec            9 9
061000011113     C*
061100011113     C* Come prima cosa effettuo considerazioni sulla divisa
061200011113     C                   IF        vabIAS > *zeros
061300011113     C                   IF        vabVAS <> 'EUR'
061400011113     C                   EVAL      vabVAS =  'ITL'
061500011113     C                   ENDIF
061600011113     C                   ENDIF
061700011113     C*
061800011113     C                   IF        vabCAS > *zeros
061900011113     C                   IF        vabVCA <> 'EUR'
062000011113     C                   EVAL      vabVCA =  'ITL'
062100011113     C                   ENDIF
062200011113     C                   ENDIF
062300011113     C*
062400011113     C                   IF        vabVMD > *zeros
062500020305     C                   IF        vabVAD <> 'EUR'
062600011113     C                   EVAL      vabVAD =  'ITL'
062700011113     C                   ENDIF
062800011113     C                   ENDIF
062900011113     C*
063000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063100011113     C                   Z-ADD     vabIAS        wrkDec
063200011113     C                   IF        wrkDec > *zeros
063300011113     C                   IF        vabVAS = 'ITL'
063400011113     C                   EVAL      vabIAS = *zeros
063500011113     C                   ENDIF
063600011113     C                   ENDIF
063700011113     C*
063800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063900011113     C                   Z-ADD     vabCAS        wrkDec
064000011113     C                   IF        wrkDec > *zeros
064100011113     C                   IF        vabVCA = 'ITL'
064200011113     C                   EVAL      vabCAS = *zeros
064300011113     C                   ENDIF
064400011113     C                   ENDIF
064500011113     C*
064600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064700011113     C                   Z-ADD     vabVMD        wrkDec
064800011113     C                   IF        wrkDec > *zeros
064900011113     C                   IF        vabVAD = 'ITL'
065000011113     C                   EVAL      vabVMD = *zeros
065100011113     C                   ENDIF
065200011113     C                   ENDIF
065300011113     C*
065400011113     C                   ENDSR
065500011113     C***
065600011113
065700011113
065800000801
065900000801
066000990920      /TITLE Invio dei dati al punto operativo.
066100010202     C     invio         BEGSR
066200990920     C*
066300080512     C* 1° invio EDIVAT
066400010201     C                   reset                   dscmz
066500010201     C                   move      vlrpoi        cmzdst
066600080512     C                   eval      cmzfld = 'EDIVATWR'
066700010201     C                   eval      cmzmbd = vlrhdl
066800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066900021009     C***                if        prmfir = *blanks
067000080512     C                   eval      cmzfla = 'EDIVAT0F'
067100080512     C                   eval      cmzmba = 'EDIVAT0F'
067200021009     C***                else
067300021009     C***                eval      cmzfla = prmfir
067400021009     C***                eval      cmzmba = prmfir
067500021009     C***                endif
067600010201     C                   eval      cmznrr = *zeros
067700020305     C                   move      §ctrokvt      cmznrr
067800021018     C                   eval      cmzlba = vlrfl1
067900010201     C                   call(e)   'TIS711C'
068000010201     C                   parm                    dscmz
068100010201     C                   parm      *blanks       esito
068200010205     C                   if        %error
068300010205     C                             or cmzerr = '1'
068400010205     C                             or esito  = '1'
068500010205     C                   eval      wrkesito = '3'
068600010205     C                   else
068700010201     C*
068800080512     C* 2° invio EDIVAB
068900010201     C                   reset                   dscmz
069000010201     C                   move      vlrpoi        cmzdst
069100010201     C                   eval      cmzfld = vlrfou
069200010201     C                   eval      cmzmbd = vlrhdl
069300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069400021009     C***                if        prmfir = *blanks
069500080512     C                   eval      cmzfla = 'EDIVAB0F'
069600080512     C                   eval      cmzmba = 'EDIVAB0F'
069700021009     C***                else
069800021009     C***                eval      cmzfla = prmfir
069900021009     C***                eval      cmzmba = prmfir
070000021009     C***                endif
070100010201     C                   eval      cmznrr = *zeros
070200010201     C                   move      §ctrokvb      cmznrr
070300021018     C                   eval      cmzlba = vlrfl1
070400010201     C                   call(e)   'TIS711C'
070500010201     C                   parm                    dscmz
070600010201     C                   parm      *blanks       esito
070700010201     C                   if        %error
070800010201     C                             or cmzerr = '1'
070900010201     C                             or esito  = '1'
071000010201     C                   eval      wrkesito = '3'
071100010201     C                   endif
071200010205     C                   endif
071300990920     C*
071400000613     C                   ENDSR
071500000613     C***
071600070411
071700070411     C     *pssr         BEGSR
071800070411     C*
071900070411     C                   if        %open(tivin00r)
072000070411     C                   close     tivin00r
072100070411     C                   endif
072200080512     C                   if        %open(edivabwr)
072300080512     C                   close     edivabwr
072400070411     C                   endif
072500080512     C                   if        %open(edivatwr)
072600080512     C                   close     edivatwr
072700070411     C                   endif
072800070411     C*
072900070411     C* Effettuo la chiamata al CLLE preposto
073000080512     C                   call(e)   'TITVEVTC'
073100070411     C                   parm                    parccm
073200070411     C                   parm                    parmbr
073300070411     C                   parm      '2'           paropz
073400070411     C*
073500070411     C                   eval      wrkesito = '2'
073600070411     C*
073700070411     C                   seton                                        LR
073800070411     C*
073900070411     C                   ENDSR     '*CANCL'
074000070411     C***
074100070411
074200990910
074300000613     C     *inzsr        BEGSR
074400990910     C*
074500990910     C     *entry        plist
074600990920     C                   parm                    tivlrds
074700990921     C                   parm      wrkesito      esito
074800000724     C                   parm                    prmlit
074900000710     C                   parm                    prmfir
075000081217     C*
075100081217     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
075200081217     C                   move(p)   vlrMSG        wNomeFile
075300000613     C*
075400000830     C* CALCOLA LA DATA CORRENTE
075500000830     C                   time                    wn14             14 0
075600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075700080512     C                   movel     wn14          oracor            6 0          *ORA
075800000830     C                   z-add     wn8           g08dat
075900000830     C                   z-add     *zeros        g08inv
076000000830     C                   movel     '0'           g08err
076100000830     C                   call      'XSRDA8'
076200000830     C                   parm                    wlbda8
076300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
076400000830     C*
076500000613     C                   ENDSR
076600000613     C***
