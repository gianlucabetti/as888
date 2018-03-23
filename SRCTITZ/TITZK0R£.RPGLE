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
002800170711     D curSped         s             15    INZ(*blanks)
002900170711     D depSped         s             15    INZ(*blanks)
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
010800170711     C                   eval      curSped = %subst(vindta:297:15)
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
015000150212     C***                EVAL      VABCMR=%subst(wNomeFile:7:12)
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
019600170720     C                   EVAL      VABCMR ='SIFARMA'
019700140408     C                   EVAL      VABDCM = datcor
019800140408     C                   EVAL      VABDTS = datcor
019900140408     C                   EVAL      VABHMS = oracor
020000140408     C                   EVAL      VABCNT = 1
020100090127     C*
020200090127     C* Quindi scarico il buffer del file d testata
020300140408     C                   WRITE     EDIVAB00
020400010305     C*
020500010305     C                   ENDSR
020600990920
020700000801     C*----------------------------------------------------*
020800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020900000801     C*----------------------------------------------------*
021000010201     C     INZVAR        BEGSR
021100000801     C*
021200040802     C                   Z-ADD     *zeros        Num5_0            5 0
021300040802     C                   MOVEL     '0'           FlgCAS            1
021400000801     C*
021500000801     C                   ENDSR
021600000801     C*----------------------------------------------------*
021700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021800000801     C*----------------------------------------------------*
021900000801     C     DEFCAM        BEGSR
022000070718     C*
022100070718     C* Imposto i valori di default...
022200170711     C                   Z-ADD     0057811       VABCCM
022300170711     C                   Z-ADD     0057811       VATCCM
022400150212     C                   Z-ADD     005           VABLNP
022500150212     C                   Z-ADD     005           VATLNP
022600150212     C                   Z-ADD     000           VABCTR
022700140224     C                   MOVEL     '1'           VABCBO
022800140224     C                   MOVEL     '7Q'          VABCTM
022900020619     C* ... e poi verifico se sono stati passati come parametri
023000020619     C                   IF        vlrppt > *blanks
023100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023300020619     C                   EXSR      CHKNUM
023400020619     C                   IF        PiInt=*on
023500020619     C                   Z-ADD     PiVal         VABCCM
023600020619     C                   Z-ADD     PiVal         VATCCM
023700020619     C                   ENDIF
023800040506     C                   ENDIF
023900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024100020619     C                   EXSR      CHKNUM
024200020619     C                   IF        PiInt=*on
024300020619     C                   Z-ADD     PiVal         VABLNP
024400020619     C                   Z-ADD     PiVal         VATLNP
024500040506     C                   ENDIF
024600020619     C                   ENDIF
024700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024900020619     C                   EXSR      CHKNUM
025000020619     C                   IF        PiInt=*on
025100020619     C                   Z-ADD     PiVal         VABCTR
025200040506     C                   ENDIF
025300020619     C                   ENDIF
025400140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
025500140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
025600140224     C                   ENDIF
025700020619     C                   ENDIF
025800000801     C*
025900000801     C                   ENDSR
026000000801     C*----------------------------------------------------*
026100140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026200000801     C*----------------------------------------------------*
026300040823     C     IMPVAB        BEGSR
026400040823     C*
026500020305     C                   EXSR      INZVAR
026600020305     C                   EXSR      DEFCAM
026700010305     C*
026800000801     C                   Z-ADD     *zeros        errore            1 0
026900000830     C                   MOVEL     datcor        VABAAS
027000020305     C                   MOVEL     datcor        VATAAS
027100040526     C                   MOVE      datcor        VABMGS
027200040823     C                   MOVE(P)   vlrpoi        VABFGS
027300040823     C                   MOVE(P)   vlrpoi        VATFGS
027400050628     C*
027500170711     C                   EVAL      VABRSD=%trim(%subst(vindta:41:35))
027600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027800020117     C* ==
027900170711     C                   EVAL      VABRD2=%trim(%subst(vindta:76:35))
028000170711     C                   EVAL      VABIND=%trim(%subst(vindta:111:35))
028100170711     C                   EVAL      VABLOD=%trim(%subst(vindta:155:35))
028200170711     C                   EVAL      VABPRD=%trim(%subst(vindta:190:2))
028300170711     C                   EVAL      VABNZD=%trim(%subst(vindta:192:2))
028400150212     C                   IF        VABNZD = 'I' or
028500170711     C                             VABNZD = 'IT'
028600150212     C                   EVAL      VABNZD = *blank
028700150212     C                   ENDIF
028800170711     C                   EVAL      VABTSP=%trim(%subst(vindta:203:1))
028900170711     C                   SELECT
029000170711     C                   WHEN      %trim(%subst(vindta:462:1)) = 'A'
029100170711     C                   EVAL      VABTC1='A'
029200170711     C                   WHEN      %trim(%subst(vindta:462:1)) = 'F'
029300170711     C                   EVAL      VABFFD='S'
029400170711     C                   ENDSL
029500170711     C                   EVAL      VABTIC=%trim(%subst(vindta:752:2))
029600170711     C* CCM
029700170711     C                   EVAL      PiStr=%trim(%subst(vindta:3:7))
029800010201     C                   EXSR      CHKNUM
029900010201     C                   IF        PiInt=*on
030000170711     C                   Z-ADD     PiVal         VABCCM
030100170719     C                   Z-ADD     PiVal         VATCCM
030200010201     C                   ELSE
030300040506     C                   ADD       1             errore
030400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
030500170719     C                             + ' ' + 'VABCCM VATCCM'
030600010201     C                   ENDIF
030700170711     C* CAP
030800170711     C                   EVAL      PiStr=%trim(%subst(vindta:146:9))
030900170711     C                   EXSR      CHKNUM
031000170711     C                   IF        PiInt=*on
031100170711     C                   Z-ADD     PiVal         Num5_0
031200170711     C                   MOVEL(P)  Num5_0        VABCAD
031300170711     C                   ELSE
031400170711     C                   ADD       1             errore
031500170711     C                   EVAL      vinmsg = %trimr(vinmsg)
031600170711     C                             + ' ' + 'VABCAD'
031700170711     C                   ENDIF
031800040506     C* Reperisco la provincia dal CAP e dalla località
031900040526     C                   IF        VABCAD <> *blanks AND
032000040526     C                             VABPRD  = *blanks
032100040506     C                   CLEAR                   TISI95DS
032200040506     C                   EVAL      I95TCN = '3'
032300040506     C                   Z-ADD     datcor        I95DAT
032400040506     C                   EVAL      I95CAP = VABCAD
032500040506     C                   EVAL      I95LOC = VABLOD
032600050627     C                   EVAL      I95NAR = VABNZD
032700040506     C                   CALL      'TISI95R'
032800040506     C                   PARM                    TISI95DS
032900040506     C                   EVAL      VABPRD = O95PRV
033000040506     C                   ENDIF
033100040506     C* PKB
033200170711     C                   EVAL      PiStr=%trim(%subst(vindta:244:8))
033300010201     C                   EXSR      CHKNUM
033400010201     C                   IF        PiNum=*on
033500060316     C                   Z-ADD     PiVal         VABPKB
033600010201     C                   ELSE
033700010201     C                   ADD       1             errore
033800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033900010201     C                             + ' ' + 'VABPKB'
034000010201     C                   ENDIF
034100150212     C* NCL
034200170711     C                   EVAL      PiStr=%trim(%subst(vindta:238:5))
034300090511     C                   EXSR      CHKNUM
034400090511     C                   IF        PiInt=*on
034500150212     C                   Z-ADD     PiVal         VABNCL
034600090511     C                   ELSE
034700090511     C                   ADD       1             errore
034800090511     C                   EVAL      vinmsg = %trimr(vinmsg)
034900150212     C                             + ' ' + 'VABNCL'
035000090511     C                   ENDIF
035100170720     C*
035200170720     C* Se numero colli è zero => il record è da NON considerare
035300170720     C                   IF        VABNCL = *zeros
035400170720     C                   seton                                        55
035500170720     C                   ENDIF
035600150212     C* NSP / RMN
035700170711     C                   EVAL      PiStr=%trim(%subst(vindta:297:15))
035800150212     C                   EXSR      CHKNUM
035900150212     C                   IF        PiInt=*on
036000150212     C                   Z-ADD     PiVal         VABNSP
036100150212     C                   Z-ADD     PiVal         VATNSP
036200150212     C                   Z-ADD     PiVal         VABRMN
036300150212     C                   ELSE
036400150212     C                   ADD       1             errore
036500150212     C                   EVAL      vinmsg = %trimr(vinmsg)
036600150212     C                             + ' ' + 'VABNSP VATNSP VABRMN'
036700150212     C                   ENDIF
036800060307     C* CAS
036900170711     C                   IF        %trim(%subst(vindta:597:15))<>*zeros      AND
037000170719     C                             %trim(%subst(vindta:597:15))<>*blanks     AND
037100170719     C                             %trim(%subst(vindta:597:15))<>'0,00'
037200170711     C                   EVAL      VABVCA = 'EUR'
037300060307     C                   EVAL      FlgCAS = '1'
037400170711     C                   EVAL      PiStr=%trim(%subst(vindta:597:15))
037500060307     C                   EXSR      CHKNUM
037600060307     C                   IF        PiNum=*on
037700060316     C                   Z-ADD     PiVal         VABCAS
037800060307     C                   ELSE
037900060307     C                   ADD       1             errore
038000060307     C                   EVAL      vinmsg = %trimr(vinmsg)
038100060307     C                             + ' ' + 'VABCAS'
038200060307     C                   ENDIF
038300070102     C                   ENDIF
038400170711     C* DCR
038500170711     C                   IF        %trim(%subst(vindta:421:8))<>*zeros      AND
038600170711     C                             %trim(%subst(vindta:421:8))<>*blanks
038700170711     C                   EVAL      PiStr=%trim(%subst(vindta:421:8))
038800150212     C                   EXSR      CHKNUM
038900150212     C                   IF        PiInt=*on
039000170711     C                   EVAL      VABDCR=PiVal
039100150212     C                   ELSE
039200150212     C                   ADD       1             errore
039300150212     C                   EVAL      vinmsg = %trimr(vinmsg)
039400170711     C                             + ' ' + 'VABDCR'
039500150212     C                   ENDIF
039600150212     C                   ENDIF
039700010205     C*
039800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039900040802     C                   IF        FlgCAS <> '0'
040000070102     C                   IF        VABCBO = '1'
040100010205     C                   EVAL      VABCBO = '4'
040200010205     C                   ELSE
040300070102     C                   EVAL      VABCBO = '6'
040400070102     C                   ENDIF
040500010205     C                   ENDIF
040600020305     C*
040700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040800011113     C                   EXSR      CHKIMPDIV
040900010202     C*
041000000801     C* Ebbene...
041100000801     C                   ADD       1             §CTRMO
041200010201     C                   IF        errore <> *zeros
041300000801     C                   ADD       1             §CTRNO
041400000801     C                   EVAL      vinflg = '2'
041500000801     C                   ELSE
041600010201     C                   ADD       1             §CTROKVB
041700000801     C                   ENDIF
041800000801     C*
041900000801     C                   ENDSR
042000070102     C*----------------------------------------------------*
042100140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
042200070102     C*----------------------------------------------------*
042300070102     C     EXEVATA       BEGSR
042400070102     C*
042500070102     C                   EVAL      VATTRC='A'
042600170711     C***                EVAL      VATNOT = %trim(%subst(vindta:353:35))
042700070102     C                   exsr      wrivat                                       => scarico VAT
042800070102     C*
042900070102     C                   ENDSR
043000060307     C*----------------------------------------------------*
043100140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
043200060307     C*----------------------------------------------------*
043300060307     C     EXEVATB       BEGSR
043400060307     C*
043500060307     C                   EVAL      VATTRC='B'
043600170711     C                   EVAL      VATNOT = %trim(%subst(vindta:692:30))
043700060307     C                   exsr      wrivat                                       => scarico VAT
043800060307     C*
043900060307     C                   ENDSR
044000140224     C*----------------------------------------------------*
044100140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
044200140224     C*----------------------------------------------------*
044300140224     C     EXEVATI       BEGSR
044400140224     C*
044500140224     C                   EVAL      VATTRC='I'
044600170711     C                   EVAL      VATNOT = %trim(%subst(vindta:612:35))
044700140224     C                   exsr      wrivat                                       => scarico VAT
044800140224     C*
044900140224     C                   EVAL      VATTRC='J'
045000170711     C                   EVAL      VATNOT = %trim(%subst(vindta:612+35:35))
045100140224     C                   exsr      wrivat                                       => scarico VAT
045200140224     C*
045300140224     C                   ENDSR
045400140226     C*----------------------------------------------------*
045500140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "S"
045600140226     C*----------------------------------------------------*
045700140226     C     EXEVATS       BEGSR
045800140226     C*
045900140226     C                   EVAL      VATTRC='S'
046000150212     C                   EVAL      VATNOT =
046100170711     C                              %trim(%subst(vindta:722:30))
046200140226     C                   IF        VATNOT <> *blank
046300170711     C                   EVAL      %subst(VATNOT:17:2) = ' N'
046400170711     C                   ELSE
046500170711     C                   EVAL      %subst(VATNOT:17:2) = '  '
046600140226     C                   ENDIF
046700140226     C                   exsr      wrivat                                       => scarico VAT
046800140226     C*
046900140226     C                   ENDSR
047000010201     C*----------------------------------------------------*
047100140408     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
047200010201     C*----------------------------------------------------*
047300020305     C     WRIVAT        BEGSR
047400140408     C*
047500140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
047600170720     C                   EVAL      VATCMR ='SIFARMA'
047700140408     C                   EVAL      VATCNT = 1
047800050628     C*
047900060223     C* Scrivo solo se valorizzato qualcosa
048000060223     C                   IF        VATNOT <> *blanks
048100140408     C                   WRITE     EDIVAT00
048200060223     C                   ENDIF
048300010201     C*
048400010201     C                   ENDSR
048500010202     C*----------------------------------------------------*
048600140408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
048700010202     C*----------------------------------------------------*
048800020305     C     PREVAT        BEGSR
048900010202     C*
049000140408     C* Compongo il nome del membro da dare al EDIVATWR
049100010202     C                   eval      parmbr = vlrhdl
049200010202     C                   movel     'M'           parmbr
049300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049400010202     C                   eval      paropz = '1'
049500010202     C* Effettuo la chiamata al CLLE preposto
049600140408     C                   call(e)   'TITVEVTC'
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
051200000801     C*
051300000801     C                   call(e)   'ISNUMERIC'
051400000801     C                   PARM                    PiStr            30
051500170711     C                   PARM      ','           PiDecChr          1
051600000801     C                   PARM      *ZEROS        PiVal            30 9
051700000801     C                   PARM      '0'           PiInt             1
051800000801     C                   PARM      '0'           PiNum             1
051900000801     C                   IF        %error
052000000801     C                   EVAL      PiInt=*off
052100000801     C                   ENDIF
052200000801     C*
052300000801     C                   ENDSR
052400000801     C***
052500000801
052600011113
052700011113     C*----------------------------------------------------*
052800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052900011113     C*----------------------------------------------------*
053000011113     C     CHKIMPDIV     BEGSR
053100011113     C*
053200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053300011113     C                   Z-ADD     *zeros        wrkDec            9 9
053400011113     C*
053500011113     C* Come prima cosa effettuo considerazioni sulla divisa
053600011113     C                   IF        vabIAS > *zeros
053700011113     C                   IF        vabVAS <> 'EUR'
053800011113     C                   EVAL      vabVAS =  'ITL'
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C                   IF        vabCAS > *zeros
054300011113     C                   IF        vabVCA <> 'EUR'
054400011113     C                   EVAL      vabVCA =  'ITL'
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C                   IF        vabVMD > *zeros
054900020305     C                   IF        vabVAD <> 'EUR'
055000011113     C                   EVAL      vabVAD =  'ITL'
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055500011113     C                   Z-ADD     vabIAS        wrkDec
055600011113     C                   IF        wrkDec > *zeros
055700011113     C                   IF        vabVAS = 'ITL'
055800011113     C                   EVAL      vabIAS = *zeros
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056300011113     C                   Z-ADD     vabCAS        wrkDec
056400011113     C                   IF        wrkDec > *zeros
056500011113     C                   IF        vabVCA = 'ITL'
056600011113     C                   EVAL      vabCAS = *zeros
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057100011113     C                   Z-ADD     vabVMD        wrkDec
057200011113     C                   IF        wrkDec > *zeros
057300011113     C                   IF        vabVAD = 'ITL'
057400011113     C                   EVAL      vabVMD = *zeros
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C                   ENDSR
057900011113     C***
058000011113
058100011113
058200000801
058300000801
058400990920      /TITLE Invio dei dati al punto operativo.
058500010202     C     invio         BEGSR
058600990920     C*
058700140408     C* 1° invio EDIVAT
058800010201     C                   reset                   dscmz
058900010201     C                   move      vlrpoi        cmzdst
059000140408     C                   eval      cmzfld = 'EDIVATWR'
059100010201     C                   eval      cmzmbd = vlrhdl
059200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059300021009     C***                if        prmfir = *blanks
059400140408     C                   eval      cmzfla = 'EDIVAT0F'
059500140408     C                   eval      cmzmba = 'EDIVAT0F'
059600021009     C***                else
059700021009     C***                eval      cmzfla = prmfir
059800021009     C***                eval      cmzmba = prmfir
059900021009     C***                endif
060000010201     C                   eval      cmznrr = *zeros
060100020305     C                   move      §ctrokvt      cmznrr
060200021018     C                   eval      cmzlba = vlrfl1
060300010201     C                   call(e)   'TIS711C'
060400010201     C                   parm                    dscmz
060500010201     C                   parm      *blanks       esito
060600010205     C                   if        %error
060700010205     C                             or cmzerr = '1'
060800010205     C                             or esito  = '1'
060900010205     C                   eval      wrkesito = '3'
061000010205     C                   else
061100010201     C*
061200140408     C* 2° invio EùDIVAB
061300010201     C                   reset                   dscmz
061400010201     C                   move      vlrpoi        cmzdst
061500010201     C                   eval      cmzfld = vlrfou
061600010201     C                   eval      cmzmbd = vlrhdl
061700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061800021009     C***                if        prmfir = *blanks
061900140408     C                   eval      cmzfla = 'EDIVAB0F'
062000140408     C                   eval      cmzmba = 'EDIVAB0F'
062100021009     C***                else
062200021009     C***                eval      cmzfla = prmfir
062300021009     C***                eval      cmzmba = prmfir
062400021009     C***                endif
062500010201     C                   eval      cmznrr = *zeros
062600010201     C                   move      §ctrokvb      cmznrr
062700021018     C                   eval      cmzlba = vlrfl1
062800010201     C                   call(e)   'TIS711C'
062900010201     C                   parm                    dscmz
063000010201     C                   parm      *blanks       esito
063100010201     C                   if        %error
063200010201     C                             or cmzerr = '1'
063300010201     C                             or esito  = '1'
063400010201     C                   eval      wrkesito = '3'
063500010201     C                   endif
063600010205     C                   endif
063700990920     C*
063800000613     C                   ENDSR
063900000613     C***
064000990910
064100000613     C     *inzsr        BEGSR
064200990910     C*
064300990910     C     *entry        plist
064400990920     C                   parm                    tivlrds
064500990921     C                   parm      wrkesito      esito
064600000724     C                   parm                    prmlit
064700000710     C                   parm                    prmfir
064800000613     C*
064900000830     C* CALCOLA LA DATA CORRENTE
065000140408     C                   time                    wn14             14 0
065100140408     C                   movel     wn14          oracor            6 0          *ORA
065200100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
065300100118     C                   eval      datcor = %dec(%date() : *ISO)
065400000830     C*
065500000613     C                   ENDSR
065600000613     C***
