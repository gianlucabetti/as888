000100140408      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200140221      *
000300140221      * PARTICOLARITA':
000400140221      *
000500140221     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700990910     Ftivin00r  uF   E             DISK    usropn
000800140408     FEDIVABwr  O    E             DISK    usropn
000900140408     FEDIVATwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800060307     D tisi95ds      e ds
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800170822     D curSped         s              6    INZ(*blanks)
002900170822     D depSped         s              6    INZ(*blanks)
003000140221     D w70             s             70    INZ(*blanks)
003100150212     D wNomeFile       s             30    INZ(*blanks)
003200100118
003300100118     D*------------------
003400100118     D* DS REPERIMENTO NUMERATORE
003500100118     D*------------------
003600100118     D trul33ds      e ds                  inz
003700100118     D*------------------
003800100118     D* DS ARCHITETTURA
003900100118     D*------------------
004000100118     D kpjba         e ds                  inz
004100100118
004200010201
004300010201
004400090127     C*
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800000613     C*
004900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005000000613     C*
005100010202     C* Effettuo la chiamata al CLLE preposto
005200140408     C                   call(e)   'TITVEVTC'
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
006800140408     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006900910830     C*--------------------------------------------------------
007000040526     C     RWFILE        BEGSR
007100990910     C*
007200990914     C                   if        not %open(tivin00r)
007300990908     C                   open      tivin00r
007400990914     C                   endif
007500140408     C                   if        not %open(EDIVABwr)
007600140408     C                   open      EDIVABwr
007700140408     C                   endif
007800140408     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007900020305     C                   exsr      prevat
008000010201     C*
008100010202     C                   if        chkcall = '0'
008200010202     C*
008300140408     C                   if        not %open(EDIVATwr)
008400140408     C                   open      EDIVATwr
008500010201     C                   endif
008600990910     C*
008700010201     C                   clear                   §CTROKVB          5 0
008800020305     C                   clear                   §CTROKVT          5 0
008900000801     C                   clear                   §CTRMO            5 0
009000000801     C                   clear                   §CTRNO            5 0
009100990910     C*
009200921023     C                   DO        *HIVAL
009300990913     C*
009400990915     C                   READ      tivin00r                               70
009500050627     C                   if        vindta > *blanks
009600000613     C                   add       1             rrnum
009700000801     C*
009800000801     C                   if        *in70 = *off
009900000801     C                             and
010000000801     C                             (vinflg = *blanks
010100000801     C                              or vinflg = '0'
010200000801     C                              or vinflg = '2')
010300000801     C*
010400000801     C                   clear                   vinmsg
010500000801     C                   eval      vinflg = '1'
010600070402     C*
010700070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010800170907     C                   eval      curSped = %subst(vindta:25:6)
010900070402     C                   if        curSped <> depSped
011000140408     C                   clear                   edivab00
011100140408     C                   clear                   edivat00
011200050628     C*
011300170720     C                   setoff                                       55
011400060315     C                   exsr      impvab                                       => carico  VAB
011500090127     C*
011600170720     C  N55              exsr      wrivab                                       => scarico VAB
011700070402     C*
011800070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011900170720     C  N55              exsr      exevata
012000070402     C*
012100070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
012200170720     C  N55              exsr      exevatb
012300140226     C*
012400140226     C* Indirizzo e-mail
012500170720     C  N55              exsr      exevati
012600140226     C*
012700140226     C* Cellulare per SMS
012800170720     C  N55              exsr      exevats
012900070402     C*
013000070402     C* Salvo il raggruppamento spedizione cliente corrente
013100070402     C                   eval      depSped = curSped
013200070402     C*
013300070402     C                   endif
013400000905     C*
013500000905     C                   else
013600000905     C                   eval      vinflg = '1'
013700050628     C                   endif
013800000905     C                   endif
013900000905     C*
014000000905     C  N70              update    tivin000
014100000905     C*
014200991022     C  N70              ENDdo
014300140408     C*
014400150212     C* Siccome il file in questione non ha più righe per spedizione essendo come un disk A
014500150212     C* questa ulteriore WRITE non serve, in quanto utile se avevo i dati di VAB in memoria
014600150212     C* in attesa di aver scritto tutti i VAT_E
014700150212     C***
014800140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
014900150212     C***                MOVE(P)   vlrMSG        wNomeFile
015000170822     C***                EVAL      VABCMR=%char(datcor)+'-'+%char(oracor)
015100150212     C***                EVAL      VABDCM = datcor
015200150212     C***                EVAL      VABDTS = datcor
015300150212     C***                EVAL      VABHMS = oracor
015400150212     C***                EVAL      VABCNT = 1
015500150212     C***
015600150212     C***                WRITE     EDIVAB00
015700010202     C*
015800010202     C                   endif
015900990910
016000990910     C* Se non ci sono record con errori ...
016100000710     C                   if        §ctrno = 0
016200990910     C* ... restituisco esito OK.
016300990921     C                   eval      wrkesito = '0'
016400990910     C                   else
016500010201     C                   if        §ctrokvb > 0
016600990921     C                   eval      wrkesito = '1'
016700000710     C                   else
016800000710     C                   eval      wrkesito = '2'
016900990910     C                   endif
017000000710     C                   endif
017100990910     C*
017200990914     C                   if        %open(tivin00r)
017300990908     C                   close     tivin00r
017400990914     C                   endif
017500140408     C                   if        %open(EDIVABwr)
017600140408     C                   close     EDIVABwr
017700140408     C                   endif
017800140408     C                   if        %open(EDIVATwr)
017900140408     C                   close     EDIVATwr
018000140408     C                   endif
018100990910     C*
018200010201     C                   if        §ctrokvb > 0
018300000724     C                             and vlrpoi <> *zeros
018400010202     C                   exsr      invio
018500990920     C                   endif
018600990920     C*
018700910830     C                   ENDSR
018800000613     C***
018900010305
019000010305     C*----------------------------------------------------*
019100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019200010305     C*----------------------------------------------------*
019300090127     C     WRIVAB        BEGSR
019400140408     C*
019500140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
019600170907     C***                EVAL      VABCMR =%char(datcor)+'-'+%char(oracor)
019700170907     C                   EVAL      VABCMR ='KASANOVA'
019800140408     C                   EVAL      VABDCM = datcor
019900140408     C                   EVAL      VABDTS = datcor
020000140408     C                   EVAL      VABHMS = oracor
020100140408     C                   EVAL      VABCNT = 1
020200090127     C*
020300090127     C* Quindi scarico il buffer del file d testata
020400140408     C                   WRITE     EDIVAB00
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
022300170822     C                   Z-ADD     2495207       VABCCM
022400170822     C                   Z-ADD     2495207       VATCCM
022500170822     C                   Z-ADD     249           VABLNP
022600170822     C                   Z-ADD     249           VATLNP
022700170822     C                   Z-ADD     501           VABCTR
022800140224     C                   MOVEL     '1'           VABCBO
022900140224     C                   MOVEL     '7Q'          VABCTM
023000020619     C* ... e poi verifico se sono stati passati come parametri
023100020619     C                   IF        vlrppt > *blanks
023200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023400020619     C                   EXSR      CHKNUM
023500020619     C                   IF        PiInt=*on
023600020619     C                   Z-ADD     PiVal         VABCCM
023700020619     C                   Z-ADD     PiVal         VATCCM
023800020619     C                   ENDIF
023900040506     C                   ENDIF
024000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024200020619     C                   EXSR      CHKNUM
024300020619     C                   IF        PiInt=*on
024400020619     C                   Z-ADD     PiVal         VABLNP
024500020619     C                   Z-ADD     PiVal         VATLNP
024600040506     C                   ENDIF
024700020619     C                   ENDIF
024800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025000020619     C                   EXSR      CHKNUM
025100020619     C                   IF        PiInt=*on
025200020619     C                   Z-ADD     PiVal         VABCTR
025300040506     C                   ENDIF
025400020619     C                   ENDIF
025500140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
025600140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
025700140224     C                   ENDIF
025800020619     C                   ENDIF
025900000801     C*
026000000801     C                   ENDSR
026100000801     C*----------------------------------------------------*
026200140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026300000801     C*----------------------------------------------------*
026400040823     C     IMPVAB        BEGSR
026500040823     C*
026600020305     C                   EXSR      INZVAR
026700020305     C                   EXSR      DEFCAM
026800010305     C*
026900000801     C                   Z-ADD     *zeros        errore            1 0
027000000830     C                   MOVEL     datcor        VABAAS
027100020305     C                   MOVEL     datcor        VATAAS
027200040526     C                   MOVE      datcor        VABMGS
027300040823     C                   MOVE(P)   vlrpoi        VABFGS
027400040823     C                   MOVE(P)   vlrpoi        VATFGS
027500050628     C*
027600170907     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
027700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027900020117     C* ==
028000170822     C                   SELECT
028100170907     C                   WHEN      %trim(%subst(vindta:22:3)) = 'DDT'
028200170822     C* uso il valore di default
028300170907     C                   WHEN      %trim(%subst(vindta:22:3)) = 'TSP'
028400170822     C                   EVAL      VABCCM=2495829
028500171005     C                   EVAL      VATCCM=VABCCM
028600171012     C                   EVAL      VABCTR=501
028700170926     C                   WHEN      %trim(%subst(vindta:22:3)) = 'MGT'
028800170926     C                   EVAL      VABCCM=2495848
028900171005     C                   EVAL      VATCCM=VABCCM
029000171012     C                   EVAL      VABCTR=501
029100171005     C                   WHEN      %trim(%subst(vindta:22:3)) = 'MGD'
029200171005     C                   EVAL      VABCCM=2495853
029300171005     C                   EVAL      VATCCM=VABCCM
029400171012     C                   EVAL      VABCTR=501
029500171005     C                   WHEN      %trim(%subst(vindta:22:3)) = 'COT'
029600171005     C                   EVAL      VABCCM=0899623
029700171005     C                   EVAL      VATCCM=VABCCM
029800171005     C                   Z-ADD     089           VABLNP
029900171005     C                   Z-ADD     089           VATLNP
030000171017     C                   EVAL      VABCTR=501
030100171005     C                   WHEN      %trim(%subst(vindta:22:3)) = 'COD'
030200171005     C                   EVAL      VABCCM=0899068
030300171005     C                   EVAL      VATCCM=VABCCM
030400171005     C                   Z-ADD     089           VABLNP
030500171005     C                   Z-ADD     089           VATLNP
030600171017     C                   EVAL      VABCTR=501
030700171009     C                   WHEN      %trim(%subst(vindta:20:3)) = 'TUV'
030800171005     C                   EVAL      VABCCM=0898676
030900171005     C                   EVAL      VATCCM=VABCCM
031000171005     C                   Z-ADD     202           VABLNP
031100171005     C                   Z-ADD     202           VATLNP
031200171012     C                   EVAL      VABCTR=501
031300170822     C                   OTHER
031400170822     C* altrimenti ignoro il rcd
031500170822     C                   seton                                        55
031600170822     C                   ENDSL
031700171006     C*
031800171006     C* Forzatura su FGS
031900171006     C                   EVAL      VABFGS = VABLNP
032000171006     C                   EVAL      VATFGS = VATLNP
032100171006     C*
032200171016     C                   MOVEL     *blanks       wIND            126
032300171016     C                   EVAL      wIND=%trim(%subst(vindta:574:126))
032400171016     C*
032500171016     C                   EVAL      VABIND=%subst(wIND:1:35)
032600171016     C                   EVAL      VABRD2=%subst(wIND:36:35)
032700170907     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
032800170907     C                   EVAL      VABPRD=%trim(%subst(vindta:314:5))
032900170907     C                   EVAL      VABRMA=%trim(%subst(vindta:11:20))
033000170907     C                   EVAL      VABTCR=%trim(%subst(vindta:490:1))
033100170907     C                   EVAL      w70   =%trim(%subst(vindta:491:40))
033200170822     C                   EVAL      VABNOT=%subst(w70:1:35)
033300170822     C                   EVAL      VABNT2=%subst(w70:36:35)
033400170711     C* CAP
033500170907     C                   EVAL      PiStr=%trim(%subst(vindta:319:8))
033600170711     C                   EXSR      CHKNUM
033700170711     C                   IF        PiInt=*on
033800170711     C                   Z-ADD     PiVal         Num5_0
033900170711     C                   MOVEL(P)  Num5_0        VABCAD
034000170711     C                   ELSE
034100170711     C                   ADD       1             errore
034200170711     C                   EVAL      vinmsg = %trimr(vinmsg)
034300170711     C                             + ' ' + 'VABCAD'
034400170711     C                   ENDIF
034500040506     C* Reperisco la provincia dal CAP e dalla località
034600040526     C                   IF        VABCAD <> *blanks AND
034700040526     C                             VABPRD  = *blanks
034800040506     C                   CLEAR                   TISI95DS
034900040506     C                   EVAL      I95TCN = '3'
035000040506     C                   Z-ADD     datcor        I95DAT
035100040506     C                   EVAL      I95CAP = VABCAD
035200040506     C                   EVAL      I95LOC = VABLOD
035300050627     C                   EVAL      I95NAR = VABNZD
035400040506     C                   CALL      'TISI95R'
035500040506     C                   PARM                    TISI95DS
035600040506     C                   EVAL      VABPRD = O95PRV
035700040506     C                   ENDIF
035800170908     C* PKB
035900170908     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
036000010201     C                   EXSR      CHKNUM
036100170822     C                   IF        PiInt=*on
036200170908     C                   EVAL      VABPKB = PiVal / 100
036300010201     C                   ELSE
036400010201     C                   ADD       1             errore
036500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036600170908     C                             + ' ' + 'VABPKB'
036700010201     C                   ENDIF
036800170908     C* VLB / QFT
036900170908     C                   EVAL      PiStr=%trim(%subst(vindta:369:5))
037000170908     C                   EXSR      CHKNUM
037100170908     C                   IF        PiInt=*on
037200170921     C                   EVAL      VABVLB = PiVal / 100
037300170921     C                   EVAL      VABQFT = PiVal / 100
037400170908     C                   ELSE
037500170908     C                   ADD       1             errore
037600170908     C                   EVAL      vinmsg = %trimr(vinmsg)
037700170908     C                             + ' ' + 'VABVLB VABQFT'
037800170908     C                   ENDIF
037900150212     C* NCL
038000170822     C                   SELECT
038100170907     C                   WHEN      %subst(vindta:352:5) <> *blank and
038200170907     C                             %subst(vindta:352:5) <> *zeros
038300170907     C                   EVAL      PiStr=%trim(%subst(vindta:352:5))
038400090511     C                   EXSR      CHKNUM
038500090511     C                   IF        PiInt=*on
038600150212     C                   Z-ADD     PiVal         VABNCL
038700090511     C                   ELSE
038800090511     C                   ADD       1             errore
038900090511     C                   EVAL      vinmsg = %trimr(vinmsg)
039000150212     C                             + ' ' + 'VABNCL'
039100090511     C                   ENDIF
039200170907     C                   WHEN      %subst(vindta:364:5) <> *blank and
039300170907     C                             %subst(vindta:364:5) <> *zeros
039400170907     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
039500170822     C                   EXSR      CHKNUM
039600170822     C                   IF        PiInt=*on
039700170822     C                   Z-ADD     PiVal         VABNCL
039800170822     C                   ELSE
039900170822     C                   ADD       1             errore
040000170822     C                   EVAL      vinmsg = %trimr(vinmsg)
040100170822     C                             + ' ' + 'VABNCL'
040200170822     C                   ENDIF
040300170822     C                   OTHER
040400170822     C                   ADD       1             errore
040500170822     C                   EVAL      vinmsg = %trimr(vinmsg)
040600170822     C                             + ' ' + 'VABNCL'
040700170822     C                   ENDSL
040800170822     C* NSP / RMN
040900170907     C                   EVAL      PiStr=%trim(%subst(vindta:25:6))
041000150212     C                   EXSR      CHKNUM
041100150212     C                   IF        PiInt=*on
041200150212     C                   Z-ADD     PiVal         VABNSP
041300150212     C                   Z-ADD     PiVal         VATNSP
041400150212     C                   Z-ADD     PiVal         VABRMN
041500150212     C                   ELSE
041600150212     C                   ADD       1             errore
041700150212     C                   EVAL      vinmsg = %trimr(vinmsg)
041800150212     C                             + ' ' + 'VABNSP VATNSP VABRMN'
041900150212     C                   ENDIF
042000060307     C* CAS
042100170907     C                   IF        %trim(%subst(vindta:388:13))<>*zeros      AND
042200170907     C                             %trim(%subst(vindta:388:13))<>*blanks
042300170711     C                   EVAL      VABVCA = 'EUR'
042400060307     C                   EVAL      FlgCAS = '1'
042500170907     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
042600060307     C                   EXSR      CHKNUM
042700170822     C                   IF        PiInt=*on
042800170822     C                   EVAL      VABCAS = PiVal / 1000
042900060307     C                   ELSE
043000060307     C                   ADD       1             errore
043100060307     C                   EVAL      vinmsg = %trimr(vinmsg)
043200060307     C                             + ' ' + 'VABCAS'
043300060307     C                   ENDIF
043400070102     C                   ENDIF
043500170711     C* DCR
043600170907     C                   IF        %trim(%subst(vindta:482:8))<>*zeros      AND
043700170907     C                             %trim(%subst(vindta:482:8))<>*blanks
043800170907     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
043900150212     C                   EXSR      CHKNUM
044000150212     C                   IF        PiInt=*on
044100170711     C                   EVAL      VABDCR=PiVal
044200150212     C                   ELSE
044300150212     C                   ADD       1             errore
044400150212     C                   EVAL      vinmsg = %trimr(vinmsg)
044500170711     C                             + ' ' + 'VABDCR'
044600150212     C                   ENDIF
044700150212     C                   ENDIF
044800010205     C*
044900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045000040802     C                   IF        FlgCAS <> '0'
045100070102     C                   IF        VABCBO = '1'
045200010205     C                   EVAL      VABCBO = '4'
045300010205     C                   ELSE
045400070102     C                   EVAL      VABCBO = '6'
045500070102     C                   ENDIF
045600010205     C                   ENDIF
045700020305     C*
045800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045900011113     C                   EXSR      CHKIMPDIV
046000010202     C*
046100000801     C* Ebbene...
046200000801     C                   ADD       1             §CTRMO
046300010201     C                   IF        errore <> *zeros
046400000801     C                   ADD       1             §CTRNO
046500000801     C                   EVAL      vinflg = '2'
046600000801     C                   ELSE
046700010201     C                   ADD       1             §CTROKVB
046800000801     C                   ENDIF
046900000801     C*
047000000801     C                   ENDSR
047100070102     C*----------------------------------------------------*
047200140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
047300070102     C*----------------------------------------------------*
047400070102     C     EXEVATA       BEGSR
047500070102     C*
047600070102     C                   EVAL      VATTRC='A'
047700170711     C***                EVAL      VATNOT = %trim(%subst(vindta:353:35))
047800070102     C                   exsr      wrivat                                       => scarico VAT
047900070102     C*
048000070102     C                   ENDSR
048100060307     C*----------------------------------------------------*
048200140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
048300060307     C*----------------------------------------------------*
048400060307     C     EXEVATB       BEGSR
048500060307     C*
048600060307     C                   EVAL      VATTRC='B'
048700170822     C***                EVAL      VATNOT = %trim(%subst(vindta:692:30))
048800060307     C                   exsr      wrivat                                       => scarico VAT
048900060307     C*
049000060307     C                   ENDSR
049100140224     C*----------------------------------------------------*
049200140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
049300140224     C*----------------------------------------------------*
049400140224     C     EXEVATI       BEGSR
049500140224     C*
049600140224     C                   EVAL      VATTRC='I'
049700170822     C***                EVAL      VATNOT = %trim(%subst(vindta:612:35))
049800140224     C                   exsr      wrivat                                       => scarico VAT
049900140224     C*
050000140224     C                   EVAL      VATTRC='J'
050100170822     C***                EVAL      VATNOT = %trim(%subst(vindta:612+35:35))
050200140224     C                   exsr      wrivat                                       => scarico VAT
050300140224     C*
050400140224     C                   ENDSR
050500140226     C*----------------------------------------------------*
050600140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "S"
050700140226     C*----------------------------------------------------*
050800140226     C     EXEVATS       BEGSR
050900140226     C*
051000140226     C                   EVAL      VATTRC='S'
051100170822     C***                EVAL      VATNOT =
051200170822     C***                           %trim(%subst(vindta:722:30))
051300140226     C                   IF        VATNOT <> *blank
051400170711     C                   EVAL      %subst(VATNOT:17:2) = ' N'
051500170711     C                   ELSE
051600170711     C                   EVAL      %subst(VATNOT:17:2) = '  '
051700140226     C                   ENDIF
051800140226     C                   exsr      wrivat                                       => scarico VAT
051900140226     C*
052000140226     C                   ENDSR
052100010201     C*----------------------------------------------------*
052200140408     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
052300010201     C*----------------------------------------------------*
052400020305     C     WRIVAT        BEGSR
052500140408     C*
052600140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
052700170907     C***                EVAL      VATCMR =%char(datcor)+'-'+%char(oracor)
052800170907     C                   EVAL      VATCMR ='KASANOVA'
052900140408     C                   EVAL      VATCNT = 1
053000050628     C*
053100060223     C* Scrivo solo se valorizzato qualcosa
053200060223     C                   IF        VATNOT <> *blanks
053300140408     C                   WRITE     EDIVAT00
053400060223     C                   ENDIF
053500010201     C*
053600010201     C                   ENDSR
053700010202     C*----------------------------------------------------*
053800140408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
053900010202     C*----------------------------------------------------*
054000020305     C     PREVAT        BEGSR
054100010202     C*
054200140408     C* Compongo il nome del membro da dare al EDIVATWR
054300010202     C                   eval      parmbr = vlrhdl
054400010202     C                   movel     'M'           parmbr
054500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
054600010202     C                   eval      paropz = '1'
054700010202     C* Effettuo la chiamata al CLLE preposto
054800140408     C                   call(e)   'TITVEVTC'
054900010202     C                   parm                    parccm
055000010202     C                   parm                    parmbr
055100010202     C                   parm                    paropz
055200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055300010202     C                   if        %error
055400010202     C                   movel     '1'           chkcall
055500010202     C                   else
055600010202     C                   movel     '0'           chkcall
055700010202     C                   endif
055800010202     C*
055900010202     C                   ENDSR
056000000801     C*----------------------------------------------------*
056100000801     C*  CONTROLLO NUMERICITA' CAMPI
056200000801     C*----------------------------------------------------*
056300000801     C     CHKNUM        BEGSR
056400000801     C*
056500000801     C                   call(e)   'ISNUMERIC'
056600000801     C                   PARM                    PiStr            30
056700170711     C                   PARM      ','           PiDecChr          1
056800000801     C                   PARM      *ZEROS        PiVal            30 9
056900000801     C                   PARM      '0'           PiInt             1
057000000801     C                   PARM      '0'           PiNum             1
057100000801     C                   IF        %error
057200000801     C                   EVAL      PiInt=*off
057300000801     C                   ENDIF
057400000801     C*
057500000801     C                   ENDSR
057600000801     C***
057700000801
057800011113
057900011113     C*----------------------------------------------------*
058000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058100011113     C*----------------------------------------------------*
058200011113     C     CHKIMPDIV     BEGSR
058300011113     C*
058400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058500011113     C                   Z-ADD     *zeros        wrkDec            9 9
058600011113     C*
058700011113     C* Come prima cosa effettuo considerazioni sulla divisa
058800011113     C                   IF        vabIAS > *zeros
058900011113     C                   IF        vabVAS <> 'EUR'
059000011113     C                   EVAL      vabVAS =  'ITL'
059100011113     C                   ENDIF
059200011113     C                   ENDIF
059300011113     C*
059400011113     C                   IF        vabCAS > *zeros
059500011113     C                   IF        vabVCA <> 'EUR'
059600011113     C                   EVAL      vabVCA =  'ITL'
059700011113     C                   ENDIF
059800011113     C                   ENDIF
059900011113     C*
060000011113     C                   IF        vabVMD > *zeros
060100020305     C                   IF        vabVAD <> 'EUR'
060200011113     C                   EVAL      vabVAD =  'ITL'
060300011113     C                   ENDIF
060400011113     C                   ENDIF
060500011113     C*
060600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060700011113     C                   Z-ADD     vabIAS        wrkDec
060800011113     C                   IF        wrkDec > *zeros
060900011113     C                   IF        vabVAS = 'ITL'
061000011113     C                   EVAL      vabIAS = *zeros
061100011113     C                   ENDIF
061200011113     C                   ENDIF
061300011113     C*
061400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061500011113     C                   Z-ADD     vabCAS        wrkDec
061600011113     C                   IF        wrkDec > *zeros
061700011113     C                   IF        vabVCA = 'ITL'
061800011113     C                   EVAL      vabCAS = *zeros
061900011113     C                   ENDIF
062000011113     C                   ENDIF
062100011113     C*
062200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062300011113     C                   Z-ADD     vabVMD        wrkDec
062400011113     C                   IF        wrkDec > *zeros
062500011113     C                   IF        vabVAD = 'ITL'
062600011113     C                   EVAL      vabVMD = *zeros
062700011113     C                   ENDIF
062800011113     C                   ENDIF
062900011113     C*
063000011113     C                   ENDSR
063100011113     C***
063200011113
063300011113
063400000801
063500000801
063600990920      /TITLE Invio dei dati al punto operativo.
063700010202     C     invio         BEGSR
063800990920     C*
063900140408     C* 1° invio EDIVAT
064000010201     C                   reset                   dscmz
064100010201     C                   move      vlrpoi        cmzdst
064200140408     C                   eval      cmzfld = 'EDIVATWR'
064300010201     C                   eval      cmzmbd = vlrhdl
064400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064500021009     C***                if        prmfir = *blanks
064600140408     C                   eval      cmzfla = 'EDIVAT0F'
064700140408     C                   eval      cmzmba = 'EDIVAT0F'
064800021009     C***                else
064900021009     C***                eval      cmzfla = prmfir
065000021009     C***                eval      cmzmba = prmfir
065100021009     C***                endif
065200010201     C                   eval      cmznrr = *zeros
065300020305     C                   move      §ctrokvt      cmznrr
065400021018     C                   eval      cmzlba = vlrfl1
065500010201     C                   call(e)   'TIS711C'
065600010201     C                   parm                    dscmz
065700010201     C                   parm      *blanks       esito
065800010205     C                   if        %error
065900010205     C                             or cmzerr = '1'
066000010205     C                             or esito  = '1'
066100010205     C                   eval      wrkesito = '3'
066200010205     C                   else
066300010201     C*
066400140408     C* 2° invio EùDIVAB
066500010201     C                   reset                   dscmz
066600010201     C                   move      vlrpoi        cmzdst
066700010201     C                   eval      cmzfld = vlrfou
066800010201     C                   eval      cmzmbd = vlrhdl
066900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067000021009     C***                if        prmfir = *blanks
067100140408     C                   eval      cmzfla = 'EDIVAB0F'
067200140408     C                   eval      cmzmba = 'EDIVAB0F'
067300021009     C***                else
067400021009     C***                eval      cmzfla = prmfir
067500021009     C***                eval      cmzmba = prmfir
067600021009     C***                endif
067700010201     C                   eval      cmznrr = *zeros
067800010201     C                   move      §ctrokvb      cmznrr
067900021018     C                   eval      cmzlba = vlrfl1
068000010201     C                   call(e)   'TIS711C'
068100010201     C                   parm                    dscmz
068200010201     C                   parm      *blanks       esito
068300010201     C                   if        %error
068400010201     C                             or cmzerr = '1'
068500010201     C                             or esito  = '1'
068600010201     C                   eval      wrkesito = '3'
068700010201     C                   endif
068800010205     C                   endif
068900990920     C*
069000000613     C                   ENDSR
069100000613     C***
069200990910
069300000613     C     *inzsr        BEGSR
069400990910     C*
069500990910     C     *entry        plist
069600990920     C                   parm                    tivlrds
069700990921     C                   parm      wrkesito      esito
069800000724     C                   parm                    prmlit
069900000710     C                   parm                    prmfir
070000000613     C*
070100000830     C* CALCOLA LA DATA CORRENTE
070200140408     C                   time                    wn14             14 0
070300140408     C                   movel     wn14          oracor            6 0          *ORA
070400100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
070500100118     C                   eval      datcor = %dec(%date() : *ISO)
070600000830     C*
070700000613     C                   ENDSR
070800000613     C***
