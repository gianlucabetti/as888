000100080512      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200130507      *
000300130507      * PARTICOLARITA':
000400130507      * copiato da TITV1L2R e modificato con altre richieste, per cui non sono gestiti i parametri
000500130507      * dopo il 13° char.
000600130507      *
000700081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000800990908
000900990910     Ftivin00r  uF   E             DISK    usropn
001000080512     FEDIVABwr  O    E             DISK    usropn
001100080512     FEDIVATwr  O    E             DISK    usropn
001200990908
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000060307     D tisi95ds      e ds
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500000613     D rrnum           s              6  0 INZ(*zeros)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000130507     D curSped         s              7    INZ(*blanks)
003100130507     D depSped         s              7    INZ(*blanks)
003200081217     D wNomeFile       s             30    INZ(*blanks)
003300000830
003400000830     D*------------------
003500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600000830     D*------------------
003700000830     D WLBDA8          DS                  INZ
003800000830     D  G08DAT                 1      8  0
003900000830     D  G08INV                 9     16  0
004000000830     D  G08ERR                17     17
004100000830     D  G08TGI                18     22  0
004200081217
004300081217     D*------------------
004400081217     D* LINKING A DEFINIZIONI ESTERNE
004500081217     D*------------------
004600081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004700081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004800081217
004900081217
005000010201
005100081217
005200081217     C*
005300081217     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005400081217     C
005500081217     C/EXEC SQL
005600081217     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005700081217     C/END-EXEC
005800081217     C
005900000913     C                   reset                   rrnum
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200000613     C*
006300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006400000613     C*
006500010202     C* Effettuo la chiamata al CLLE preposto
006600080512     C                   call(e)   'TITVEVTC'
006700010202     C                   parm                    parccm
006800010202     C                   parm                    parmbr
006900010202     C                   parm      '2'           paropz
007000050201     C*
007100050201     C* Effettuo lancio TISI95 solo x chiusura
007200050201     C                   CLEAR                   TISI95DS
007300050201     C                   EVAL      I95TLA = 'C'
007400050201     C                   CALL      'TISI95R'
007500050201     C                   PARM                    TISI95DS
007600000616     C*
007700000801     C
007800010201     C                   seton                                        LR
007900990908
008000000801
008100910830     C*--------------------------------------------------------
008200080512     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
008300910830     C*--------------------------------------------------------
008400040526     C     RWFILE        BEGSR
008500990910     C*
008600990914     C                   if        not %open(tivin00r)
008700990908     C                   open      tivin00r
008800990914     C                   endif
008900080512     C                   if        not %open(edivabwr)
009000080512     C                   open      edivabwr
009100990914     C                   endif
009200070103     C*
009300080512     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
009400020305     C                   exsr      prevat
009500010201     C*
009600010202     C                   if        chkcall = '0'
009700010202     C*
009800080512     C                   if        not %open(edivatwr)
009900080512     C                   open      edivatwr
010000010201     C                   endif
010100990910     C*
010200010201     C                   clear                   §CTROKVB          5 0
010300020305     C                   clear                   §CTROKVT          5 0
010400000801     C                   clear                   §CTRMO            5 0
010500000801     C                   clear                   §CTRNO            5 0
010600990910     C*
010700921023     C                   DO        *HIVAL
010800990913     C*
010900990915     C                   READ      tivin00r                               70
011000050627     C                   if        vindta > *blanks
011100000613     C                   add       1             rrnum
011200000801     C*
011300000801     C                   if        *in70 = *off
011400000801     C                             and
011500000801     C                             (vinflg = *blanks
011600000801     C                              or vinflg = '0'
011700000801     C                              or vinflg = '2')
011800000801     C*
011900000801     C                   clear                   vinmsg
012000000801     C                   eval      vinflg = '1'
012100070103     C*
012200070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
012300130507     C                   eval      curSped = %subst(vindta:4:7)
012400070213     C                   if        curSped <> depSped
012500070213     C* Se prima bolla => importo bolla corrente
012600070213     C                   if        §CTRMO = *zeros
012700070213     C                   exsr      inzvar
012800070213     C                   exsr      defcam
012900070213     C                   exsr      impvab                                       => carico  VAB
013000070213     C                   else
013100070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
013200070213     C                   exsr      wrivab                                       => scarico VAB
013300070213     C                   exsr      inzvar
013400070213     C                   exsr      defcam
013500070213     C                   exsr      impvab                                       => carico  VAB
013600070213     C                   endif
013700070103     C* Salvo il raggruppamento spedizione cliente corrente
013800070103     C                   eval      depSped = curSped
013900070213     C*
014000070213     C* Se collo successivo x stessa bolla
014100070213     C                   else
014200070213     C                   exsr      impvab                                       => carico  VAB
014300070103     C                   endif
014400090618     C*
014500090618     C* Scrivo sempre le estensioni del record corrente
014600090618     C                   exsr      exevate                                      => write VAT-E
014700090618     C                   exsr      exevata
014800090618     C                   exsr      exevatb
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
017900080512     C                   if        %open(edivabwr)
018000080512     C                   close     edivabwr
018100990914     C                   endif
018200080512     C                   if        %open(edivatwr)
018300080512     C                   close     edivatwr
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
019800080512     C*
019900080512     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
020000090115     C                   EVAL      VABCMR = %trim(wNomeFile) + ' ' +
020100090115     C                                      %char(datcor)
020200080512     C                   EVAL      VABDCM = DATCOR
020300080512     C                   EVAL      VABDTS = DATCOR
020400080512     C                   EVAL      VABHMS = ORACOR
020500080512     C                   EVAL      VABCNT = 1
020600010305     C*
020700060225     C* Quindi scarico il buffer del file d testata
020800080512     C                   write     edivab00                                     => scarico il VAB
020900010305     C*
021000010305     C                   ENDSR
021100990920
021200000801     C*----------------------------------------------------*
021300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021400000801     C*----------------------------------------------------*
021500010201     C     INZVAR        BEGSR
021600000801     C*
021700080512     C                   CLEAR                   EDIVAB00
021800080512     C                   CLEAR                   EDIVAT00
021900070213     C*
022000040802     C                   Z-ADD     *zeros        Num5_0            5 0
022100040802     C                   MOVEL     '0'           FlgCAS            1
022200000801     C*
022300000801     C                   ENDSR
022400000801     C*----------------------------------------------------*
022500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
022600000801     C*----------------------------------------------------*
022700000801     C     DEFCAM        BEGSR
022800000801     C*
022900020619     C* Imposto i valori di default...
023000130507     C                   Z-ADD     0055072       VABCCM
023100130507     C                   Z-ADD     0055072       VATCCM
023200130507     C                   Z-ADD     005           VABLNP
023300130507     C                   Z-ADD     005           VATLNP
023400081217     C                   Z-ADD     000           VABCTR
023500070103     C                   MOVEL     '7Q'          VABCTM
023600040823     C                   MOVEL     '1'           VABCBO
023700020619     C* ... e poi verifico se sono stati passati come parametri
023800020619     C                   IF        vlrppt > *blanks
023900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
024000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024100020619     C                   EXSR      CHKNUM
024200020619     C                   IF        PiInt=*on
024300020619     C                   Z-ADD     PiVal         VABCCM
024400020619     C                   Z-ADD     PiVal         VATCCM
024500020619     C                   ENDIF
024600040506     C                   ENDIF
024700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024900020619     C                   EXSR      CHKNUM
025000020619     C                   IF        PiInt=*on
025100020619     C                   Z-ADD     PiVal         VABLNP
025200020619     C                   Z-ADD     PiVal         VATLNP
025300040506     C                   ENDIF
025400020619     C                   ENDIF
025500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
025600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025700020619     C                   EXSR      CHKNUM
025800020619     C                   IF        PiInt=*on
025900020619     C                   Z-ADD     PiVal         VABCTR
026000040506     C                   ENDIF
026100020619     C                   ENDIF
026200020619     C                   ENDIF
026300000801     C*
026400000801     C                   ENDSR
026500000801     C*----------------------------------------------------*
026600080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026700000801     C*----------------------------------------------------*
026800040823     C     IMPVAB        BEGSR
026900010305     C*
027000000801     C                   Z-ADD     *zeros        errore            1 0
027100000830     C                   MOVEL     datcor        VABAAS
027200020305     C                   MOVEL     datcor        VATAAS
027300040526     C                   MOVE      datcor        VABMGS
027400040823     C                   MOVE(P)   vlrpoi        VABFGS
027500040823     C                   MOVE(P)   vlrpoi        VATFGS
027600080409     C*
027700080409     C* Reperimento campi ALFA
027800080409     C*
027900080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
028000130507     C                   EVAL      VABRSD=%trim(%subst(vindta:76:35))
028100080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
028200080409     C     '@':'A'       XLATE     VABRSD        VABRSD
028300080409     C* ==
028400130507     C                   EVAL      VABRD2=%trim(%subst(vindta:196:35))
028500130507     C                   EVAL      VABIND=%trim(%subst(vindta:112:35))
028600130507     C                   EVAL      VABLOD=%trim(%subst(vindta:148:35))
028700130507     C                   EVAL      VABRMA=%trim(%subst(vindta:51:4))
028800130507     C                   EVAL      VABTIC=%trim(%subst(vindta:476:2))
028900090415     C*
029000130507     C                   IF        %subst(vindta:51:4) = 'TNF '
029100130507     C                   EVAL      VABCCM = 0055068
029200130507     C                   EVAL      VATCCM = 0055068
029300090415     C                   ENDIF
029400090519     C*
029500130507     C                   IF        %subst(vindta:51:4) = '7FAM'
029600090519     C                   EVAL      VABCCM = 0055072
029700090519     C                   EVAL      VATCCM = 0055072
029800090519     C                   ENDIF
029900150504     C*
030000150504     C                   IF        %subst(vindta:51:4) = 'SMW '
030100150504     C                   EVAL      VABCCM = 0057952
030200150504     C                   EVAL      VATCCM = 0057952
030300150504     C                   ENDIF
030400151102     C*
030500160112     C                   IF        %subst(vindta:51:5) = 'M1552'
030600151102     C                   EVAL      VABCCM = 0055069
030700151102     C                   EVAL      VATCCM = 0055069
030800151102     C                   ENDIF
030900090415     C*
031000130507     C                   IF        %subst(vindta:56:1) = 'Y'
031100081217     C                   EVAL      FlgCAS = '1'
031200081217     C                   ENDIF
031300080409     C*
031400080409     C* Reperimento campi NUMERICI
031500081217     C* NSP
031600130507     C                   EVAL      PiStr=%trim(%subst(vindta:4:7))
031700080409     C                   EXSR      CHKNUM
031800080409     C                   IF        PiInt=*on
031900081217     C                   Z-ADD     PiVal         VABNSP
032000081217     C                   Z-ADD     PiVal         VATNSP
032100080409     C                   ELSE
032200080409     C                   ADD       1             errore
032300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032400081217     C                             + ' ' + 'VABNSP VATNSP'
032500080409     C                   ENDIF
032600081217     C* RMN
032700130507     C                   EVAL      PiStr=%trim(%subst(vindta:1:10))
032800081217     C                   EXSR      CHKNUM
032900081217     C                   IF        PiInt=*on
033000081217     C                   Z-ADD     PiVal         VABRMN
033100081217     C                   ELSE
033200081217     C                   ADD       1             errore
033300081217     C                   Z-ADD     1             VABRMN
033400081217     C                   EVAL      vinmsg = %trimr(vinmsg)
033500081217     C                             + ' ' + 'VABRMN'
033600081217     C                   ENDIF
033700080409     C* CAD
033800130507     C                   EVAL      PiStr=%trim(%subst(vindta:184:11))
033900130507     C                   EXSR      CHKNUM
034000130507     C                   IF        PiInt=*on
034100130507     C                   Z-ADD     PiVal         Num5_0
034200130507     C                   MOVEL(p)  Num5_0        VABCAD
034300130507     C                   ELSE
034400130507     C                   ADD       1             errore
034500130507     C                   EVAL      VABCAD = *zeros
034600130507     C                   EVAL      vinmsg = %trimr(vinmsg)
034700130507     C                             + ' ' + 'VABCAD'
034800130507     C                   ENDIF
034900090113     C*
035000090113     C* Reperisco la provincia dal CAP e dalla località
035100090113     C                   IF        VABCAD <> *blanks
035200090113     C                   CLEAR                   TISI95DS
035300090113     C                   EVAL      I95TCN = '3'
035400090113     C                   Z-ADD     datcor        I95DAT
035500090113     C                   EVAL      I95CAP = VABCAD
035600090113     C                   EVAL      I95LOC = VABLOD
035700090113     C                   CALL      'TISI95R'
035800090113     C                   PARM                    TISI95DS
035900090113     C                   EVAL      VABPRD = O95PRV
036000090113     C                   ENDIF
036100081217     C* NCL
036200130507     C                   EVAL      PiStr=%trim(%subst(vindta:33:8))
036300081217     C                   EXSR      CHKNUM
036400081217     C                   IF        PiInt=*on
036500081217     C                   Z-ADD     PiVal         VABNCL
036600081217     C                   ELSE
036700081217     C                   EVAL      vinmsg = %trimr(vinmsg)
036800081217     C                             + ' ' + 'VABNCL'
036900081217     C                   ENDIF
037000081217     C* PKB
037100081217     C*
037200081217     C/EXEC SQL
037300081217     C+ declare C1 cursor for
037400130507     C+ select sum(decimal(substr(vindta, 43, 7))/1000) as peso
037500081217     C+ from tivin00r
037600130507     C+ where substr(vindta, 4, 7) = :curSped
037700081217     C+ for read only
037800081217     C/END-EXEC
037900081217     C
038000081217     C/EXEC SQL
038100081217     C+ open C1
038200081217     C/END-EXEC
038300081217     C                   IF        sqlcod <= *zeros
038400081217     C
038500081217     C/EXEC SQL
038600081217     C+ Fetch C1 into :VABPKB
038700081217     C/END-EXEC
038800081217     C
038900081217     C/EXEC SQL
039000081217     C+ close C1
039100081217     C/END-EXEC
039200081217     C*
039300081217     C                   ELSE
039400081217     C                   ADD       1             errore
039500081217     C                   EVAL      vinmsg = %trimr(vinmsg)
039600081217     C                             + ' ' + 'VABPKB'
039700081217     C                   ENDIF
039800090115     C*
039900130507     C                   EVAL      VABTC1=%trim(%subst(vindta:474:1))
040000090115     C* CAS
040100090115     C                   IF        FlgCAS = '1'
040200130507     C                   IF        %trim(%subst(vindta:306:16))<>
040300090115     C                             '0000000000000.00'
040400090115     C                   EVAL      VABVCA = 'EUR'
040500130507     C                   EVAL      PiStr=%trim(%subst(vindta:306:16))
040600090115     C                   EXSR      CHKNUM
040700090115     C                   IF        PiNum=*on
040800090115     C                   Z-ADD     PiVal         VABCAS
040900090115     C                   ELSE
041000090115     C                   ADD       1             errore
041100090115     C                   Z-ADD     *zeros        VABCAS
041200090115     C                   EVAL      vinmsg = %trimr(vinmsg)
041300090115     C                             + ' ' + 'VABCAS'
041400090115     C                   ENDIF
041500090115     C                   ENDIF
041600090115     C                   ENDIF
041700010205     C*
041800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041900040802     C                   IF        FlgCAS <> '0'
042000010205     C                   EVAL      VABCBO = '4'
042100010205     C                   ENDIF
042200020305     C*
042300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042400011113     C                   EXSR      CHKIMPDIV
042500010202     C*
042600000801     C* Ebbene...
042700000801     C                   ADD       1             §CTRMO
042800010201     C                   IF        errore <> *zeros
042900000801     C                   ADD       1             §CTRNO
043000000801     C                   EVAL      vinflg = '2'
043100000801     C                   ELSE
043200010201     C                   ADD       1             §CTROKVB
043300000801     C                   ENDIF
043400000801     C*
043500000801     C                   ENDSR
043600070102     C*----------------------------------------------------*
043700080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "E"
043800070102     C*----------------------------------------------------*
043900070103     C     EXEVATE       BEGSR
044000070102     C*
044100070103     C                   EVAL      VATTRC='E'
044200130507     C                   EVAL      VATNOT=%trim(%subst(vindta:12:20))
044300080512     C*
044400080512     C                   exsr      wrivat                                       => scarico VAT
044500070102     C*
044600070102     C                   ENDSR
044700080415     C*----------------------------------------------------*
044800080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
044900080415     C*----------------------------------------------------*
045000080415     C     EXEVATA       BEGSR
045100080415     C*
045200090115     C                   EVAL      VATTRC='A'
045300130507     C                   EVAL      VATNOT=%trim(%subst(vindta:323:30))
045400090115     C*
045500090115     C                   exsr      wrivat                                       => scarico VAT
045600080415     C*
045700080415     C                   ENDSR
045800071121     C*----------------------------------------------------*
045900080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
046000071121     C*----------------------------------------------------*
046100071121     C     EXEVATB       BEGSR
046200071121     C*
046300090115     C                   EVAL      VATTRC='B'
046400130507     C                   EVAL      VATNOT=%trim(%subst(vindta:354:19))
046500071121     C*
046600090115     C                   exsr      wrivat                                       => scarico VAT
046700071121     C*
046800071121     C                   ENDSR
046900010201     C*----------------------------------------------------*
047000080512     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
047100010201     C*----------------------------------------------------*
047200020305     C     WRIVAT        BEGSR
047300080512     C*
047400080512     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
047500090115     C                   EVAL      VATCMR = %trim(wNomeFile) + ' ' +
047600090115     C                                      %char(datcor)
047700080512     C                   EVAL      VATCNT = 1
047800050628     C*
047900060223     C* Scrivo solo se valorizzato qualcosa
048000060223     C                   IF        VATNOT <> *blanks
048100080512     C                   WRITE     EDIVAT00
048200060223     C                   ENDIF
048300010201     C*
048400010201     C                   ENDSR
048500010202     C*----------------------------------------------------*
048600080512     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
048700010202     C*----------------------------------------------------*
048800020305     C     PREVAT        BEGSR
048900010202     C*
049000080512     C* Compongo il nome del membro da dare al EDIVATWR
049100010202     C                   eval      parmbr = vlrhdl
049200010202     C                   movel     'M'           parmbr
049300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049400010202     C                   eval      paropz = '1'
049500010202     C* Effettuo la chiamata al CLLE preposto
049600080512     C                   call(e)   'TITVEVTC'
049700010202     C                   parm                    parccm
049800010202     C                   parm                    parmbr
049900010202     C                   parm                    paropz
050000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050100010202     C                   if        %error
050200010202     C                   movel     '1'           chkcall
050300010202     C                   else
050400010202     C                   movel     '0'           chkcall
050500010202     C                   endif
050600010202     C*
050700010202     C                   ENDSR
050800000801     C*----------------------------------------------------*
050900000801     C*  CONTROLLO NUMERICITA' CAMPI
051000000801     C*----------------------------------------------------*
051100000801     C     CHKNUM        BEGSR
051200081217     C*
051300081217     C                   IF        PiDecChr = *blanks
051400081217     C                   EVAL      PiDecChr = '.'
051500081217     C                   ENDIF
051600081217     C*
051700081217     C                   callp(e)  UBISNUM_Check(PiStr
051800081217     C                                          :PiDecChr
051900081217     C                                          :PiVal
052000081217     C                                          :PiNum
052100081217     C                                          :PiInt)
052200081217     C*
052300081217     C                   IF        %error
052400081217     C                   EVAL      PiInt=*off
052500081217     C                   ENDIF
052600000801     C*
052700000801     C                   ENDSR
052800000801     C***
052900000801
053000011113
053100011113     C*----------------------------------------------------*
053200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053300011113     C*----------------------------------------------------*
053400011113     C     CHKIMPDIV     BEGSR
053500011113     C*
053600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053700011113     C                   Z-ADD     *zeros        wrkDec            9 9
053800011113     C*
053900011113     C* Come prima cosa effettuo considerazioni sulla divisa
054000011113     C                   IF        vabIAS > *zeros
054100011113     C                   IF        vabVAS <> 'EUR'
054200011113     C                   EVAL      vabVAS =  'ITL'
054300011113     C                   ENDIF
054400011113     C                   ENDIF
054500011113     C*
054600011113     C                   IF        vabCAS > *zeros
054700011113     C                   IF        vabVCA <> 'EUR'
054800011113     C                   EVAL      vabVCA =  'ITL'
054900011113     C                   ENDIF
055000011113     C                   ENDIF
055100011113     C*
055200011113     C                   IF        vabVMD > *zeros
055300020305     C                   IF        vabVAD <> 'EUR'
055400011113     C                   EVAL      vabVAD =  'ITL'
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055900011113     C                   Z-ADD     vabIAS        wrkDec
056000011113     C                   IF        wrkDec > *zeros
056100011113     C                   IF        vabVAS = 'ITL'
056200011113     C                   EVAL      vabIAS = *zeros
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056700011113     C                   Z-ADD     vabCAS        wrkDec
056800011113     C                   IF        wrkDec > *zeros
056900011113     C                   IF        vabVCA = 'ITL'
057000011113     C                   EVAL      vabCAS = *zeros
057100011113     C                   ENDIF
057200011113     C                   ENDIF
057300011113     C*
057400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057500011113     C                   Z-ADD     vabVMD        wrkDec
057600011113     C                   IF        wrkDec > *zeros
057700011113     C                   IF        vabVAD = 'ITL'
057800011113     C                   EVAL      vabVMD = *zeros
057900011113     C                   ENDIF
058000011113     C                   ENDIF
058100011113     C*
058200011113     C                   ENDSR
058300011113     C***
058400011113
058500011113
058600000801
058700000801
058800990920      /TITLE Invio dei dati al punto operativo.
058900010202     C     invio         BEGSR
059000990920     C*
059100080512     C* 1° invio EDIVAT
059200010201     C                   reset                   dscmz
059300010201     C                   move      vlrpoi        cmzdst
059400080512     C                   eval      cmzfld = 'EDIVATWR'
059500010201     C                   eval      cmzmbd = vlrhdl
059600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059700021009     C***                if        prmfir = *blanks
059800080512     C                   eval      cmzfla = 'EDIVAT0F'
059900080512     C                   eval      cmzmba = 'EDIVAT0F'
060000021009     C***                else
060100021009     C***                eval      cmzfla = prmfir
060200021009     C***                eval      cmzmba = prmfir
060300021009     C***                endif
060400010201     C                   eval      cmznrr = *zeros
060500020305     C                   move      §ctrokvt      cmznrr
060600021018     C                   eval      cmzlba = vlrfl1
060700010201     C                   call(e)   'TIS711C'
060800010201     C                   parm                    dscmz
060900010201     C                   parm      *blanks       esito
061000010205     C                   if        %error
061100010205     C                             or cmzerr = '1'
061200010205     C                             or esito  = '1'
061300010205     C                   eval      wrkesito = '3'
061400010205     C                   else
061500010201     C*
061600080512     C* 2° invio EDIVAB
061700010201     C                   reset                   dscmz
061800010201     C                   move      vlrpoi        cmzdst
061900010201     C                   eval      cmzfld = vlrfou
062000010201     C                   eval      cmzmbd = vlrhdl
062100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062200021009     C***                if        prmfir = *blanks
062300080512     C                   eval      cmzfla = 'EDIVAB0F'
062400080512     C                   eval      cmzmba = 'EDIVAB0F'
062500021009     C***                else
062600021009     C***                eval      cmzfla = prmfir
062700021009     C***                eval      cmzmba = prmfir
062800021009     C***                endif
062900010201     C                   eval      cmznrr = *zeros
063000010201     C                   move      §ctrokvb      cmznrr
063100021018     C                   eval      cmzlba = vlrfl1
063200010201     C                   call(e)   'TIS711C'
063300010201     C                   parm                    dscmz
063400010201     C                   parm      *blanks       esito
063500010201     C                   if        %error
063600010201     C                             or cmzerr = '1'
063700010201     C                             or esito  = '1'
063800010201     C                   eval      wrkesito = '3'
063900010201     C                   endif
064000010205     C                   endif
064100990920     C*
064200000613     C                   ENDSR
064300000613     C***
064400070411
064500070411     C     *pssr         BEGSR
064600070411     C*
064700070411     C                   if        %open(tivin00r)
064800070411     C                   close     tivin00r
064900070411     C                   endif
065000080512     C                   if        %open(edivabwr)
065100080512     C                   close     edivabwr
065200070411     C                   endif
065300080512     C                   if        %open(edivatwr)
065400080512     C                   close     edivatwr
065500070411     C                   endif
065600070411     C*
065700070411     C* Effettuo la chiamata al CLLE preposto
065800080512     C                   call(e)   'TITVEVTC'
065900070411     C                   parm                    parccm
066000070411     C                   parm                    parmbr
066100070411     C                   parm      '2'           paropz
066200070411     C*
066300070411     C                   eval      wrkesito = '2'
066400070411     C*
066500070411     C                   seton                                        LR
066600070411     C*
066700070411     C                   ENDSR     '*CANCL'
066800070411     C***
066900070411
067000990910
067100000613     C     *inzsr        BEGSR
067200990910     C*
067300990910     C     *entry        plist
067400990920     C                   parm                    tivlrds
067500990921     C                   parm      wrkesito      esito
067600000724     C                   parm                    prmlit
067700000710     C                   parm                    prmfir
067800081217     C*
067900081217     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
068000081217     C                   move(p)   vlrMSG        wNomeFile
068100000613     C*
068200000830     C* CALCOLA LA DATA CORRENTE
068300000830     C                   time                    wn14             14 0
068400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068500080512     C                   movel     wn14          oracor            6 0          *ORA
068600000830     C                   z-add     wn8           g08dat
068700000830     C                   z-add     *zeros        g08inv
068800000830     C                   movel     '0'           g08err
068900000830     C                   call      'XSRDA8'
069000000830     C                   parm                    wlbda8
069100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069200000830     C*
069300000613     C                   ENDSR
069400000613     C***
