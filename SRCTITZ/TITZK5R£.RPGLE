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
010800170906     C                   eval      curSped = %subst(vindta:35:15)
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
019600170906     C                   EVAL      VABCMR ='HASBRO'
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
022200170906     C                   Z-ADD     1663914       VABCCM
022300170906     C                   Z-ADD     1663914       VATCCM
022400170906     C                   Z-ADD     166           VABLNP
022500170906     C                   Z-ADD     166           VATLNP
022600150212     C                   Z-ADD     000           VABCTR
022700140224     C                   MOVEL     '1'           VABCBO
022800140224     C                   MOVEL     '7Q'          VABCTM
022900170906     C                   MOVEL     'C'           VABTSP
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
027600170906     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
027700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027900020117     C* ==
028000170906     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
028100170906     C                   EVAL      VABLOD=%trim(%subst(vindta:120:35))
028200170906     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
028300170906     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
028400150212     C                   IF        VABNZD = 'I' or
028500170906     C                             VABNZD = 'IT' or
028600170906     C                             VABNZD = 'ITA'
028700150212     C                   EVAL      VABNZD = *blank
028800150212     C                   ENDIF
028900171010     C                   EVAL      VABRMA=%trim(%subst(vindta:35:15))
029000170906     C                   EVAL      w70   =%trim(%subst(vindta:306:50))
029100170906     C                   EVAL      VABNOT=%subst(w70:1:35)
029200170906     C                   EVAL      VABNT2=%subst(w70:36:35)
029300170711     C* CAP
029400170906     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
029500170711     C                   EXSR      CHKNUM
029600170711     C                   IF        PiInt=*on
029700170711     C                   Z-ADD     PiVal         Num5_0
029800170711     C                   MOVEL(P)  Num5_0        VABCAD
029900170711     C                   ELSE
030000170711     C                   ADD       1             errore
030100170711     C                   EVAL      vinmsg = %trimr(vinmsg)
030200170711     C                             + ' ' + 'VABCAD'
030300170711     C                   ENDIF
030400040506     C* Reperisco la provincia dal CAP e dalla località
030500040526     C                   IF        VABCAD <> *blanks AND
030600040526     C                             VABPRD  = *blanks
030700040506     C                   CLEAR                   TISI95DS
030800040506     C                   EVAL      I95TCN = '3'
030900040506     C                   Z-ADD     datcor        I95DAT
031000040506     C                   EVAL      I95CAP = VABCAD
031100040506     C                   EVAL      I95LOC = VABLOD
031200050627     C                   EVAL      I95NAR = VABNZD
031300040506     C                   CALL      'TISI95R'
031400040506     C                   PARM                    TISI95DS
031500040506     C                   EVAL      VABPRD = O95PRV
031600040506     C                   ENDIF
031700040506     C* PKB
031800170906     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
031900010201     C                   EXSR      CHKNUM
032000010201     C                   IF        PiNum=*on
032100060316     C                   Z-ADD     PiVal         VABPKB
032200010201     C                   ELSE
032300010201     C                   ADD       1             errore
032400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032500010201     C                             + ' ' + 'VABPKB'
032600010201     C                   ENDIF
032700150212     C* NCL
032800170906     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
032900090511     C                   EXSR      CHKNUM
033000090511     C                   IF        PiInt=*on
033100150212     C                   Z-ADD     PiVal         VABNCL
033200090511     C                   ELSE
033300090511     C                   ADD       1             errore
033400090511     C                   EVAL      vinmsg = %trimr(vinmsg)
033500150212     C                             + ' ' + 'VABNCL'
033600090511     C                   ENDIF
033700170906     C* VLB
033800170906     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
033900170906     C                   EXSR      CHKNUM
034000170906     C                   IF        PiInt=*on
034100170906     C                   EVAL      VABVLB = PiVal / 1000
034200170906     C                   ELSE
034300170906     C                   ADD       1             errore
034400170906     C                   EVAL      vinmsg = %trimr(vinmsg)
034500170906     C                             + ' ' + 'VABVLB'
034600170906     C                   ENDIF
034700150212     C* NSP / RMN
034800170906     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
034900150212     C                   EXSR      CHKNUM
035000150212     C                   IF        PiInt=*on
035100150212     C                   Z-ADD     PiVal         VABNSP
035200150212     C                   Z-ADD     PiVal         VATNSP
035300150212     C                   Z-ADD     PiVal         VABRMN
035400150212     C                   ELSE
035500150212     C                   ADD       1             errore
035600150212     C                   EVAL      vinmsg = %trimr(vinmsg)
035700150212     C                             + ' ' + 'VABNSP VATNSP VABRMN'
035800150212     C                   ENDIF
035900010205     C*
036000010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036100040802     C                   IF        FlgCAS <> '0'
036200070102     C                   IF        VABCBO = '1'
036300010205     C                   EVAL      VABCBO = '4'
036400010205     C                   ELSE
036500070102     C                   EVAL      VABCBO = '6'
036600070102     C                   ENDIF
036700010205     C                   ENDIF
036800020305     C*
036900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037000011113     C                   EXSR      CHKIMPDIV
037100010202     C*
037200000801     C* Ebbene...
037300000801     C                   ADD       1             §CTRMO
037400010201     C                   IF        errore <> *zeros
037500000801     C                   ADD       1             §CTRNO
037600000801     C                   EVAL      vinflg = '2'
037700000801     C                   ELSE
037800010201     C                   ADD       1             §CTROKVB
037900000801     C                   ENDIF
038000000801     C*
038100000801     C                   ENDSR
038200070102     C*----------------------------------------------------*
038300140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
038400070102     C*----------------------------------------------------*
038500070102     C     EXEVATA       BEGSR
038600070102     C*
038700070102     C                   EVAL      VATTRC='A'
038800170906     C                   EVAL      VATNOT = *blank
038900170711     C***                EVAL      VATNOT = %trim(%subst(vindta:353:35))
039000070102     C                   exsr      wrivat                                       => scarico VAT
039100070102     C*
039200070102     C                   ENDSR
039300060307     C*----------------------------------------------------*
039400140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
039500060307     C*----------------------------------------------------*
039600060307     C     EXEVATB       BEGSR
039700060307     C*
039800060307     C                   EVAL      VATTRC='B'
039900171010     C                   EVAL      VATNOT = %trim(%subst(vindta:406:21))
040000060307     C                   exsr      wrivat                                       => scarico VAT
040100060307     C*
040200060307     C                   ENDSR
040300140224     C*----------------------------------------------------*
040400140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
040500140224     C*----------------------------------------------------*
040600140224     C     EXEVATI       BEGSR
040700140224     C*
040800140224     C                   EVAL      VATTRC='I'
040900170906     C                   EVAL      VATNOT = %trim(%subst(vindta:356:35))
041000140224     C                   exsr      wrivat                                       => scarico VAT
041100140224     C*
041200140224     C                   EVAL      VATTRC='J'
041300171010     C                   EVAL      VATNOT = %trim(%subst(vindta:356+35:15))
041400140224     C                   exsr      wrivat                                       => scarico VAT
041500140224     C*
041600140224     C                   ENDSR
041700140226     C*----------------------------------------------------*
041800140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "S"
041900140226     C*----------------------------------------------------*
042000140226     C     EXEVATS       BEGSR
042100140226     C*
042200140226     C                   EVAL      VATTRC='S'
042300170906     C                   EVAL      VATNOT = *blank
042400170906     C***                           %trim(%subst(vindta:722:30))
042500140226     C                   IF        VATNOT <> *blank
042600170711     C                   EVAL      %subst(VATNOT:17:2) = ' N'
042700170711     C                   ELSE
042800170711     C                   EVAL      %subst(VATNOT:17:2) = '  '
042900140226     C                   ENDIF
043000140226     C                   exsr      wrivat                                       => scarico VAT
043100140226     C*
043200140226     C                   ENDSR
043300010201     C*----------------------------------------------------*
043400140408     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
043500010201     C*----------------------------------------------------*
043600020305     C     WRIVAT        BEGSR
043700140408     C*
043800140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
043900170906     C                   EVAL      VATCMR ='HASBRO'
044000140408     C                   EVAL      VATCNT = 1
044100050628     C*
044200060223     C* Scrivo solo se valorizzato qualcosa
044300060223     C                   IF        VATNOT <> *blanks
044400140408     C                   WRITE     EDIVAT00
044500060223     C                   ENDIF
044600010201     C*
044700010201     C                   ENDSR
044800010202     C*----------------------------------------------------*
044900140408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
045000010202     C*----------------------------------------------------*
045100020305     C     PREVAT        BEGSR
045200010202     C*
045300140408     C* Compongo il nome del membro da dare al EDIVATWR
045400010202     C                   eval      parmbr = vlrhdl
045500010202     C                   movel     'M'           parmbr
045600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
045700010202     C                   eval      paropz = '1'
045800010202     C* Effettuo la chiamata al CLLE preposto
045900140408     C                   call(e)   'TITVEVTC'
046000010202     C                   parm                    parccm
046100010202     C                   parm                    parmbr
046200010202     C                   parm                    paropz
046300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046400010202     C                   if        %error
046500010202     C                   movel     '1'           chkcall
046600010202     C                   else
046700010202     C                   movel     '0'           chkcall
046800010202     C                   endif
046900010202     C*
047000010202     C                   ENDSR
047100000801     C*----------------------------------------------------*
047200000801     C*  CONTROLLO NUMERICITA' CAMPI
047300000801     C*----------------------------------------------------*
047400000801     C     CHKNUM        BEGSR
047500000801     C*
047600000801     C                   call(e)   'ISNUMERIC'
047700000801     C                   PARM                    PiStr            30
047800170711     C                   PARM      ','           PiDecChr          1
047900000801     C                   PARM      *ZEROS        PiVal            30 9
048000000801     C                   PARM      '0'           PiInt             1
048100000801     C                   PARM      '0'           PiNum             1
048200000801     C                   IF        %error
048300000801     C                   EVAL      PiInt=*off
048400000801     C                   ENDIF
048500000801     C*
048600000801     C                   ENDSR
048700000801     C***
048800000801
048900011113
049000011113     C*----------------------------------------------------*
049100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049200011113     C*----------------------------------------------------*
049300011113     C     CHKIMPDIV     BEGSR
049400011113     C*
049500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049600011113     C                   Z-ADD     *zeros        wrkDec            9 9
049700011113     C*
049800011113     C* Come prima cosa effettuo considerazioni sulla divisa
049900011113     C                   IF        vabIAS > *zeros
050000011113     C                   IF        vabVAS <> 'EUR'
050100011113     C                   EVAL      vabVAS =  'ITL'
050200011113     C                   ENDIF
050300011113     C                   ENDIF
050400011113     C*
050500011113     C                   IF        vabCAS > *zeros
050600011113     C                   IF        vabVCA <> 'EUR'
050700011113     C                   EVAL      vabVCA =  'ITL'
050800011113     C                   ENDIF
050900011113     C                   ENDIF
051000011113     C*
051100011113     C                   IF        vabVMD > *zeros
051200020305     C                   IF        vabVAD <> 'EUR'
051300011113     C                   EVAL      vabVAD =  'ITL'
051400011113     C                   ENDIF
051500011113     C                   ENDIF
051600011113     C*
051700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051800011113     C                   Z-ADD     vabIAS        wrkDec
051900011113     C                   IF        wrkDec > *zeros
052000011113     C                   IF        vabVAS = 'ITL'
052100011113     C                   EVAL      vabIAS = *zeros
052200011113     C                   ENDIF
052300011113     C                   ENDIF
052400011113     C*
052500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052600011113     C                   Z-ADD     vabCAS        wrkDec
052700011113     C                   IF        wrkDec > *zeros
052800011113     C                   IF        vabVCA = 'ITL'
052900011113     C                   EVAL      vabCAS = *zeros
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053400011113     C                   Z-ADD     vabVMD        wrkDec
053500011113     C                   IF        wrkDec > *zeros
053600011113     C                   IF        vabVAD = 'ITL'
053700011113     C                   EVAL      vabVMD = *zeros
053800011113     C                   ENDIF
053900011113     C                   ENDIF
054000011113     C*
054100011113     C                   ENDSR
054200011113     C***
054300011113
054400011113
054500000801
054600000801
054700990920      /TITLE Invio dei dati al punto operativo.
054800010202     C     invio         BEGSR
054900990920     C*
055000140408     C* 1° invio EDIVAT
055100010201     C                   reset                   dscmz
055200010201     C                   move      vlrpoi        cmzdst
055300140408     C                   eval      cmzfld = 'EDIVATWR'
055400010201     C                   eval      cmzmbd = vlrhdl
055500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055600021009     C***                if        prmfir = *blanks
055700140408     C                   eval      cmzfla = 'EDIVAT0F'
055800140408     C                   eval      cmzmba = 'EDIVAT0F'
055900021009     C***                else
056000021009     C***                eval      cmzfla = prmfir
056100021009     C***                eval      cmzmba = prmfir
056200021009     C***                endif
056300010201     C                   eval      cmznrr = *zeros
056400020305     C                   move      §ctrokvt      cmznrr
056500021018     C                   eval      cmzlba = vlrfl1
056600010201     C                   call(e)   'TIS711C'
056700010201     C                   parm                    dscmz
056800010201     C                   parm      *blanks       esito
056900010205     C                   if        %error
057000010205     C                             or cmzerr = '1'
057100010205     C                             or esito  = '1'
057200010205     C                   eval      wrkesito = '3'
057300010205     C                   else
057400010201     C*
057500140408     C* 2° invio EùDIVAB
057600010201     C                   reset                   dscmz
057700010201     C                   move      vlrpoi        cmzdst
057800010201     C                   eval      cmzfld = vlrfou
057900010201     C                   eval      cmzmbd = vlrhdl
058000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058100021009     C***                if        prmfir = *blanks
058200140408     C                   eval      cmzfla = 'EDIVAB0F'
058300140408     C                   eval      cmzmba = 'EDIVAB0F'
058400021009     C***                else
058500021009     C***                eval      cmzfla = prmfir
058600021009     C***                eval      cmzmba = prmfir
058700021009     C***                endif
058800010201     C                   eval      cmznrr = *zeros
058900010201     C                   move      §ctrokvb      cmznrr
059000021018     C                   eval      cmzlba = vlrfl1
059100010201     C                   call(e)   'TIS711C'
059200010201     C                   parm                    dscmz
059300010201     C                   parm      *blanks       esito
059400010201     C                   if        %error
059500010201     C                             or cmzerr = '1'
059600010201     C                             or esito  = '1'
059700010201     C                   eval      wrkesito = '3'
059800010201     C                   endif
059900010205     C                   endif
060000990920     C*
060100000613     C                   ENDSR
060200000613     C***
060300990910
060400000613     C     *inzsr        BEGSR
060500990910     C*
060600990910     C     *entry        plist
060700990920     C                   parm                    tivlrds
060800990921     C                   parm      wrkesito      esito
060900000724     C                   parm                    prmlit
061000000710     C                   parm                    prmfir
061100000613     C*
061200000830     C* CALCOLA LA DATA CORRENTE
061300140408     C                   time                    wn14             14 0
061400140408     C                   movel     wn14          oracor            6 0          *ORA
061500100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
061600100118     C                   eval      datcor = %dec(%date() : *ISO)
061700000830     C*
061800000613     C                   ENDSR
061900000613     C***
