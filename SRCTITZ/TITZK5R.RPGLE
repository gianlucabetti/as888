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
012900171026     C*
013000171026     C* PDF x packing list
013100171026     C  N55              exsr      exevatp
013200070402     C*
013300070402     C* Salvo il raggruppamento spedizione cliente corrente
013400070402     C                   eval      depSped = curSped
013500070402     C*
013600070402     C                   endif
013700000905     C*
013800000905     C                   else
013900000905     C                   eval      vinflg = '1'
014000050628     C                   endif
014100000905     C                   endif
014200000905     C*
014300000905     C  N70              update    tivin000
014400000905     C*
014500991022     C  N70              ENDdo
014600140408     C*
014700150212     C* Siccome il file in questione non ha più righe per spedizione essendo come un disk A
014800150212     C* questa ulteriore WRITE non serve, in quanto utile se avevo i dati di VAB in memoria
014900150212     C* in attesa di aver scritto tutti i VAT_E
015000150212     C***
015100140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
015200150212     C***                MOVE(P)   vlrMSG        wNomeFile
015300150212     C***                EVAL      VABCMR=%subst(wNomeFile:7:12)
015400150212     C***                EVAL      VABDCM = datcor
015500150212     C***                EVAL      VABDTS = datcor
015600150212     C***                EVAL      VABHMS = oracor
015700150212     C***                EVAL      VABCNT = 1
015800150212     C***
015900150212     C***                WRITE     EDIVAB00
016000010202     C*
016100010202     C                   endif
016200990910
016300990910     C* Se non ci sono record con errori ...
016400000710     C                   if        §ctrno = 0
016500990910     C* ... restituisco esito OK.
016600990921     C                   eval      wrkesito = '0'
016700990910     C                   else
016800010201     C                   if        §ctrokvb > 0
016900990921     C                   eval      wrkesito = '1'
017000000710     C                   else
017100000710     C                   eval      wrkesito = '2'
017200990910     C                   endif
017300000710     C                   endif
017400990910     C*
017500990914     C                   if        %open(tivin00r)
017600990908     C                   close     tivin00r
017700990914     C                   endif
017800140408     C                   if        %open(EDIVABwr)
017900140408     C                   close     EDIVABwr
018000140408     C                   endif
018100140408     C                   if        %open(EDIVATwr)
018200140408     C                   close     EDIVATwr
018300140408     C                   endif
018400990910     C*
018500010201     C                   if        §ctrokvb > 0
018600000724     C                             and vlrpoi <> *zeros
018700010202     C                   exsr      invio
018800990920     C                   endif
018900990920     C*
019000910830     C                   ENDSR
019100000613     C***
019200010305
019300010305     C*----------------------------------------------------*
019400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019500010305     C*----------------------------------------------------*
019600090127     C     WRIVAB        BEGSR
019700140408     C*
019800140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
019900170906     C                   EVAL      VABCMR ='HASBRO'
020000140408     C                   EVAL      VABDCM = datcor
020100140408     C                   EVAL      VABDTS = datcor
020200140408     C                   EVAL      VABHMS = oracor
020300140408     C                   EVAL      VABCNT = 1
020400090127     C*
020500090127     C* Quindi scarico il buffer del file d testata
020600140408     C                   WRITE     EDIVAB00
020700010305     C*
020800010305     C                   ENDSR
020900990920
021000000801     C*----------------------------------------------------*
021100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021200000801     C*----------------------------------------------------*
021300010201     C     INZVAR        BEGSR
021400000801     C*
021500040802     C                   Z-ADD     *zeros        Num5_0            5 0
021600040802     C                   MOVEL     '0'           FlgCAS            1
021700000801     C*
021800000801     C                   ENDSR
021900000801     C*----------------------------------------------------*
022000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
022100000801     C*----------------------------------------------------*
022200000801     C     DEFCAM        BEGSR
022300070718     C*
022400070718     C* Imposto i valori di default...
022500170906     C                   Z-ADD     1663914       VABCCM
022600170906     C                   Z-ADD     1663914       VATCCM
022700170906     C                   Z-ADD     166           VABLNP
022800170906     C                   Z-ADD     166           VATLNP
022900150212     C                   Z-ADD     000           VABCTR
023000140224     C                   MOVEL     '1'           VABCBO
023100171026     C***                MOVEL     '7Q'          VABCTM
023200171026     C                   MOVEL     '7T'          VABCTM
023300170906     C                   MOVEL     'C'           VABTSP
023400020619     C* ... e poi verifico se sono stati passati come parametri
023500020619     C                   IF        vlrppt > *blanks
023600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023800020619     C                   EXSR      CHKNUM
023900020619     C                   IF        PiInt=*on
024000020619     C                   Z-ADD     PiVal         VABCCM
024100020619     C                   Z-ADD     PiVal         VATCCM
024200020619     C                   ENDIF
024300040506     C                   ENDIF
024400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024600020619     C                   EXSR      CHKNUM
024700020619     C                   IF        PiInt=*on
024800020619     C                   Z-ADD     PiVal         VABLNP
024900020619     C                   Z-ADD     PiVal         VATLNP
025000040506     C                   ENDIF
025100020619     C                   ENDIF
025200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
025300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025400020619     C                   EXSR      CHKNUM
025500020619     C                   IF        PiInt=*on
025600020619     C                   Z-ADD     PiVal         VABCTR
025700040506     C                   ENDIF
025800020619     C                   ENDIF
025900140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
026000140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
026100140224     C                   ENDIF
026200020619     C                   ENDIF
026300000801     C*
026400000801     C                   ENDSR
026500000801     C*----------------------------------------------------*
026600140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026700000801     C*----------------------------------------------------*
026800040823     C     IMPVAB        BEGSR
026900040823     C*
027000020305     C                   EXSR      INZVAR
027100020305     C                   EXSR      DEFCAM
027200010305     C*
027300000801     C                   Z-ADD     *zeros        errore            1 0
027400000830     C                   MOVEL     datcor        VABAAS
027500020305     C                   MOVEL     datcor        VATAAS
027600040526     C                   MOVE      datcor        VABMGS
027700040823     C                   MOVE(P)   vlrpoi        VABFGS
027800040823     C                   MOVE(P)   vlrpoi        VATFGS
027900050628     C*
028000170906     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
028100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
028200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
028300020117     C* ==
028400170906     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
028500170906     C                   EVAL      VABLOD=%trim(%subst(vindta:120:35))
028600170906     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
028700170906     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
028800150212     C                   IF        VABNZD = 'I' or
028900170906     C                             VABNZD = 'IT' or
029000170906     C                             VABNZD = 'ITA'
029100150212     C                   EVAL      VABNZD = *blank
029200150212     C                   ENDIF
029300171010     C                   EVAL      VABRMA=%trim(%subst(vindta:35:15))
029400170906     C                   EVAL      w70   =%trim(%subst(vindta:306:50))
029500170906     C                   EVAL      VABNOT=%subst(w70:1:35)
029600170906     C                   EVAL      VABNT2=%subst(w70:36:35)
029700170711     C* CAP
029800170906     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
029900170711     C                   EXSR      CHKNUM
030000170711     C                   IF        PiInt=*on
030100170711     C                   Z-ADD     PiVal         Num5_0
030200170711     C                   MOVEL(P)  Num5_0        VABCAD
030300170711     C                   ELSE
030400170711     C                   ADD       1             errore
030500170711     C                   EVAL      vinmsg = %trimr(vinmsg)
030600170711     C                             + ' ' + 'VABCAD'
030700170711     C                   ENDIF
030800040506     C* Reperisco la provincia dal CAP e dalla località
030900040526     C                   IF        VABCAD <> *blanks AND
031000040526     C                             VABPRD  = *blanks
031100040506     C                   CLEAR                   TISI95DS
031200040506     C                   EVAL      I95TCN = '3'
031300040506     C                   Z-ADD     datcor        I95DAT
031400040506     C                   EVAL      I95CAP = VABCAD
031500040506     C                   EVAL      I95LOC = VABLOD
031600050627     C                   EVAL      I95NAR = VABNZD
031700040506     C                   CALL      'TISI95R'
031800040506     C                   PARM                    TISI95DS
031900040506     C                   EVAL      VABPRD = O95PRV
032000040506     C                   ENDIF
032100040506     C* PKB
032200170906     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
032300010201     C                   EXSR      CHKNUM
032400010201     C                   IF        PiNum=*on
032500060316     C                   Z-ADD     PiVal         VABPKB
032600010201     C                   ELSE
032700010201     C                   ADD       1             errore
032800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032900010201     C                             + ' ' + 'VABPKB'
033000010201     C                   ENDIF
033100150212     C* NCL
033200170906     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
033300090511     C                   EXSR      CHKNUM
033400090511     C                   IF        PiInt=*on
033500150212     C                   Z-ADD     PiVal         VABNCL
033600090511     C                   ELSE
033700090511     C                   ADD       1             errore
033800090511     C                   EVAL      vinmsg = %trimr(vinmsg)
033900150212     C                             + ' ' + 'VABNCL'
034000090511     C                   ENDIF
034100170906     C* VLB
034200170906     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
034300170906     C                   EXSR      CHKNUM
034400170906     C                   IF        PiInt=*on
034500170906     C                   EVAL      VABVLB = PiVal / 1000
034600170906     C                   ELSE
034700170906     C                   ADD       1             errore
034800170906     C                   EVAL      vinmsg = %trimr(vinmsg)
034900170906     C                             + ' ' + 'VABVLB'
035000170906     C                   ENDIF
035100150212     C* NSP / RMN
035200170906     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
035300150212     C                   EXSR      CHKNUM
035400150212     C                   IF        PiInt=*on
035500150212     C                   Z-ADD     PiVal         VABNSP
035600150212     C                   Z-ADD     PiVal         VATNSP
035700150212     C                   Z-ADD     PiVal         VABRMN
035800150212     C                   ELSE
035900150212     C                   ADD       1             errore
036000150212     C                   EVAL      vinmsg = %trimr(vinmsg)
036100150212     C                             + ' ' + 'VABNSP VATNSP VABRMN'
036200150212     C                   ENDIF
036300010205     C*
036400010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036500040802     C                   IF        FlgCAS <> '0'
036600070102     C                   IF        VABCBO = '1'
036700010205     C                   EVAL      VABCBO = '4'
036800010205     C                   ELSE
036900070102     C                   EVAL      VABCBO = '6'
037000070102     C                   ENDIF
037100010205     C                   ENDIF
037200020305     C*
037300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037400011113     C                   EXSR      CHKIMPDIV
037500010202     C*
037600000801     C* Ebbene...
037700000801     C                   ADD       1             §CTRMO
037800010201     C                   IF        errore <> *zeros
037900000801     C                   ADD       1             §CTRNO
038000000801     C                   EVAL      vinflg = '2'
038100000801     C                   ELSE
038200010201     C                   ADD       1             §CTROKVB
038300000801     C                   ENDIF
038400000801     C*
038500000801     C                   ENDSR
038600070102     C*----------------------------------------------------*
038700140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
038800070102     C*----------------------------------------------------*
038900070102     C     EXEVATA       BEGSR
039000070102     C*
039100070102     C                   EVAL      VATTRC='A'
039200170906     C                   EVAL      VATNOT = *blank
039300170711     C***                EVAL      VATNOT = %trim(%subst(vindta:353:35))
039400070102     C                   exsr      wrivat                                       => scarico VAT
039500070102     C*
039600070102     C                   ENDSR
039700060307     C*----------------------------------------------------*
039800140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
039900060307     C*----------------------------------------------------*
040000060307     C     EXEVATB       BEGSR
040100060307     C*
040200060307     C                   EVAL      VATTRC='B'
040300171010     C                   EVAL      VATNOT = %trim(%subst(vindta:406:21))
040400060307     C                   exsr      wrivat                                       => scarico VAT
040500060307     C*
040600060307     C                   ENDSR
040700140224     C*----------------------------------------------------*
040800140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
040900140224     C*----------------------------------------------------*
041000140224     C     EXEVATI       BEGSR
041100140224     C*
041200140224     C                   EVAL      VATTRC='I'
041300170906     C                   EVAL      VATNOT = %trim(%subst(vindta:356:35))
041400140224     C                   exsr      wrivat                                       => scarico VAT
041500140224     C*
041600140224     C                   EVAL      VATTRC='J'
041700171010     C                   EVAL      VATNOT = %trim(%subst(vindta:356+35:15))
041800140224     C                   exsr      wrivat                                       => scarico VAT
041900140224     C*
042000140224     C                   ENDSR
042100140226     C*----------------------------------------------------*
042200140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "S"
042300140226     C*----------------------------------------------------*
042400140226     C     EXEVATS       BEGSR
042500140226     C*
042600140226     C                   EVAL      VATTRC='S'
042700170906     C                   EVAL      VATNOT = *blank
042800170906     C***                           %trim(%subst(vindta:722:30))
042900140226     C                   IF        VATNOT <> *blank
043000170711     C                   EVAL      %subst(VATNOT:17:2) = ' N'
043100170711     C                   ELSE
043200170711     C                   EVAL      %subst(VATNOT:17:2) = '  '
043300140226     C                   ENDIF
043400140226     C                   exsr      wrivat                                       => scarico VAT
043500140226     C*
043600140226     C                   ENDSR
043700171026     C*----------------------------------------------------*
043800171026     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "P"
043900171026     C*----------------------------------------------------*
044000171026     C     EXEVATP       BEGSR
044100171026     C*
044200171026     C                   EVAL      VATTRC='P'
044300171027     C                   EVAL      VATNOT = %trim(%editc(VABRMN:'Z'))
044400171027     C                                       + '.pdf'
044500171027     C                   EVAL      %subst(VATNOT : 34 : 2) = '10'
044600171026     C                   exsr      wrivat                                       => scarico VAT
044700171026     C*
044800171026     C                   ENDSR
044900010201     C*----------------------------------------------------*
045000140408     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
045100010201     C*----------------------------------------------------*
045200020305     C     WRIVAT        BEGSR
045300140408     C*
045400140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
045500170906     C                   EVAL      VATCMR ='HASBRO'
045600140408     C                   EVAL      VATCNT = 1
045700050628     C*
045800060223     C* Scrivo solo se valorizzato qualcosa
045900060223     C                   IF        VATNOT <> *blanks
046000140408     C                   WRITE     EDIVAT00
046100060223     C                   ENDIF
046200010201     C*
046300010201     C                   ENDSR
046400010202     C*----------------------------------------------------*
046500140408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
046600010202     C*----------------------------------------------------*
046700020305     C     PREVAT        BEGSR
046800010202     C*
046900140408     C* Compongo il nome del membro da dare al EDIVATWR
047000010202     C                   eval      parmbr = vlrhdl
047100010202     C                   movel     'M'           parmbr
047200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
047300010202     C                   eval      paropz = '1'
047400010202     C* Effettuo la chiamata al CLLE preposto
047500140408     C                   call(e)   'TITVEVTC'
047600010202     C                   parm                    parccm
047700010202     C                   parm                    parmbr
047800010202     C                   parm                    paropz
047900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048000010202     C                   if        %error
048100010202     C                   movel     '1'           chkcall
048200010202     C                   else
048300010202     C                   movel     '0'           chkcall
048400010202     C                   endif
048500010202     C*
048600010202     C                   ENDSR
048700000801     C*----------------------------------------------------*
048800000801     C*  CONTROLLO NUMERICITA' CAMPI
048900000801     C*----------------------------------------------------*
049000000801     C     CHKNUM        BEGSR
049100000801     C*
049200000801     C                   call(e)   'ISNUMERIC'
049300000801     C                   PARM                    PiStr            30
049400170711     C                   PARM      ','           PiDecChr          1
049500000801     C                   PARM      *ZEROS        PiVal            30 9
049600000801     C                   PARM      '0'           PiInt             1
049700000801     C                   PARM      '0'           PiNum             1
049800000801     C                   IF        %error
049900000801     C                   EVAL      PiInt=*off
050000000801     C                   ENDIF
050100000801     C*
050200000801     C                   ENDSR
050300000801     C***
050400000801
050500011113
050600011113     C*----------------------------------------------------*
050700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050800011113     C*----------------------------------------------------*
050900011113     C     CHKIMPDIV     BEGSR
051000011113     C*
051100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051200011113     C                   Z-ADD     *zeros        wrkDec            9 9
051300011113     C*
051400011113     C* Come prima cosa effettuo considerazioni sulla divisa
051500011113     C                   IF        vabIAS > *zeros
051600011113     C                   IF        vabVAS <> 'EUR'
051700011113     C                   EVAL      vabVAS =  'ITL'
051800011113     C                   ENDIF
051900011113     C                   ENDIF
052000011113     C*
052100011113     C                   IF        vabCAS > *zeros
052200011113     C                   IF        vabVCA <> 'EUR'
052300011113     C                   EVAL      vabVCA =  'ITL'
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C                   IF        vabVMD > *zeros
052800020305     C                   IF        vabVAD <> 'EUR'
052900011113     C                   EVAL      vabVAD =  'ITL'
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053400011113     C                   Z-ADD     vabIAS        wrkDec
053500011113     C                   IF        wrkDec > *zeros
053600011113     C                   IF        vabVAS = 'ITL'
053700011113     C                   EVAL      vabIAS = *zeros
053800011113     C                   ENDIF
053900011113     C                   ENDIF
054000011113     C*
054100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054200011113     C                   Z-ADD     vabCAS        wrkDec
054300011113     C                   IF        wrkDec > *zeros
054400011113     C                   IF        vabVCA = 'ITL'
054500011113     C                   EVAL      vabCAS = *zeros
054600011113     C                   ENDIF
054700011113     C                   ENDIF
054800011113     C*
054900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055000011113     C                   Z-ADD     vabVMD        wrkDec
055100011113     C                   IF        wrkDec > *zeros
055200011113     C                   IF        vabVAD = 'ITL'
055300011113     C                   EVAL      vabVMD = *zeros
055400011113     C                   ENDIF
055500011113     C                   ENDIF
055600011113     C*
055700011113     C                   ENDSR
055800011113     C***
055900011113
056000011113
056100000801
056200000801
056300990920      /TITLE Invio dei dati al punto operativo.
056400010202     C     invio         BEGSR
056500990920     C*
056600140408     C* 1° invio EDIVAT
056700010201     C                   reset                   dscmz
056800010201     C                   move      vlrpoi        cmzdst
056900140408     C                   eval      cmzfld = 'EDIVATWR'
057000010201     C                   eval      cmzmbd = vlrhdl
057100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057200021009     C***                if        prmfir = *blanks
057300140408     C                   eval      cmzfla = 'EDIVAT0F'
057400140408     C                   eval      cmzmba = 'EDIVAT0F'
057500021009     C***                else
057600021009     C***                eval      cmzfla = prmfir
057700021009     C***                eval      cmzmba = prmfir
057800021009     C***                endif
057900010201     C                   eval      cmznrr = *zeros
058000020305     C                   move      §ctrokvt      cmznrr
058100021018     C                   eval      cmzlba = vlrfl1
058200010201     C                   call(e)   'TIS711C'
058300010201     C                   parm                    dscmz
058400010201     C                   parm      *blanks       esito
058500010205     C                   if        %error
058600010205     C                             or cmzerr = '1'
058700010205     C                             or esito  = '1'
058800010205     C                   eval      wrkesito = '3'
058900010205     C                   else
059000010201     C*
059100140408     C* 2° invio EùDIVAB
059200010201     C                   reset                   dscmz
059300010201     C                   move      vlrpoi        cmzdst
059400010201     C                   eval      cmzfld = vlrfou
059500010201     C                   eval      cmzmbd = vlrhdl
059600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059700021009     C***                if        prmfir = *blanks
059800140408     C                   eval      cmzfla = 'EDIVAB0F'
059900140408     C                   eval      cmzmba = 'EDIVAB0F'
060000021009     C***                else
060100021009     C***                eval      cmzfla = prmfir
060200021009     C***                eval      cmzmba = prmfir
060300021009     C***                endif
060400010201     C                   eval      cmznrr = *zeros
060500010201     C                   move      §ctrokvb      cmznrr
060600021018     C                   eval      cmzlba = vlrfl1
060700010201     C                   call(e)   'TIS711C'
060800010201     C                   parm                    dscmz
060900010201     C                   parm      *blanks       esito
061000010201     C                   if        %error
061100010201     C                             or cmzerr = '1'
061200010201     C                             or esito  = '1'
061300010201     C                   eval      wrkesito = '3'
061400010201     C                   endif
061500010205     C                   endif
061600990920     C*
061700000613     C                   ENDSR
061800000613     C***
061900990910
062000000613     C     *inzsr        BEGSR
062100990910     C*
062200990910     C     *entry        plist
062300990920     C                   parm                    tivlrds
062400990921     C                   parm      wrkesito      esito
062500000724     C                   parm                    prmlit
062600000710     C                   parm                    prmfir
062700000613     C*
062800000830     C* CALCOLA LA DATA CORRENTE
062900140408     C                   time                    wn14             14 0
063000140408     C                   movel     wn14          oracor            6 0          *ORA
063100100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
063200100118     C                   eval      datcor = %dec(%date() : *ISO)
063300000830     C*
063400000613     C                   ENDSR
063500000613     C***
