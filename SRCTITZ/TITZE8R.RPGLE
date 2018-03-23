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
002800150212     D curSped         s             35    INZ(*blanks)
002900150212     D depSped         s             35    INZ(*blanks)
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
010800150212     C                   eval      curSped = %subst(vindta:469:35)
010900070402     C                   if        curSped <> depSped
011000140408     C                   clear                   edivab00
011100140408     C                   clear                   edivat00
011200050628     C*
011300060315     C                   exsr      impvab                                       => carico  VAB
011400090127     C*
011500060315     C                   exsr      wrivab                                       => scarico VAB
011600070402     C*
011700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011800140304     C                   exsr      exevata
011900070402     C*
012000070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
012100070402     C                   exsr      exevatb
012200140226     C*
012300140226     C* Indirizzo e-mail
012400140226     C                   exsr      exevati
012500140226     C*
012600140226     C* Cellulare per SMS
012700140226     C                   exsr      exevats
012800070402     C*
012900070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013000070402     C                   exsr      exevate
013100070402     C*
013200070402     C* Salvo il raggruppamento spedizione cliente corrente
013300070402     C                   eval      depSped = curSped
013400070402     C*
013500070402     C                   else
013600070402     C*
013700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013800070402     C                   exsr      exevate
013900070402     C                   endif
014000000905     C*
014100000905     C                   else
014200000905     C                   eval      vinflg = '1'
014300050628     C                   endif
014400000905     C                   endif
014500000905     C*
014600000905     C  N70              update    tivin000
014700000905     C*
014800991022     C  N70              ENDdo
014900140408     C*
015000150212     C* Siccome il file in questione non ha più righe per spedizione essendo come un disk A
015100150212     C* questa ulteriore WRITE non serve, in quanto utile se avevo i dati di VAB in memoria
015200150212     C* in attesa di aver scritto tutti i VAT_E
015300150212     C***
015400140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
015500150212     C***                MOVE(P)   vlrMSG        wNomeFile
015600150212     C***                EVAL      VABCMR=%subst(wNomeFile:7:12)
015700150212     C***                EVAL      VABDCM = datcor
015800150212     C***                EVAL      VABDTS = datcor
015900150212     C***                EVAL      VABHMS = oracor
016000150212     C***                EVAL      VABCNT = 1
016100150212     C***
016200150212     C***                WRITE     EDIVAB00
016300010202     C*
016400010202     C                   endif
016500990910
016600990910     C* Se non ci sono record con errori ...
016700000710     C                   if        §ctrno = 0
016800990910     C* ... restituisco esito OK.
016900990921     C                   eval      wrkesito = '0'
017000990910     C                   else
017100010201     C                   if        §ctrokvb > 0
017200990921     C                   eval      wrkesito = '1'
017300000710     C                   else
017400000710     C                   eval      wrkesito = '2'
017500990910     C                   endif
017600000710     C                   endif
017700990910     C*
017800990914     C                   if        %open(tivin00r)
017900990908     C                   close     tivin00r
018000990914     C                   endif
018100140408     C                   if        %open(EDIVABwr)
018200140408     C                   close     EDIVABwr
018300140408     C                   endif
018400140408     C                   if        %open(EDIVATwr)
018500140408     C                   close     EDIVATwr
018600140408     C                   endif
018700990910     C*
018800010201     C                   if        §ctrokvb > 0
018900000724     C                             and vlrpoi <> *zeros
019000010202     C                   exsr      invio
019100990920     C                   endif
019200990920     C*
019300910830     C                   ENDSR
019400000613     C***
019500010305
019600010305     C*----------------------------------------------------*
019700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019800010305     C*----------------------------------------------------*
019900090127     C     WRIVAB        BEGSR
020000140408     C*
020100140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
020200150212     C                   MOVE(P)   vlrMSG        wNomeFile
020300150212     C                   EVAL      VABCMR=%subst(wNomeFile:7:12)
020400140408     C                   EVAL      VABDCM = datcor
020500140408     C                   EVAL      VABDTS = datcor
020600140408     C                   EVAL      VABHMS = oracor
020700140408     C                   EVAL      VABCNT = 1
020800090127     C*
020900090127     C* Quindi scarico il buffer del file d testata
021000140408     C                   WRITE     EDIVAB00
021100010305     C*
021200010305     C                   ENDSR
021300990920
021400000801     C*----------------------------------------------------*
021500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021600000801     C*----------------------------------------------------*
021700010201     C     INZVAR        BEGSR
021800000801     C*
021900040802     C                   Z-ADD     *zeros        Num5_0            5 0
022000040802     C                   MOVEL     '0'           FlgCAS            1
022100000801     C*
022200000801     C                   ENDSR
022300000801     C*----------------------------------------------------*
022400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
022500000801     C*----------------------------------------------------*
022600000801     C     DEFCAM        BEGSR
022700070718     C*
022800070718     C* Imposto i valori di default...
022900150212     C                   Z-ADD     0057923       VABCCM
023000150212     C                   Z-ADD     0057923       VATCCM
023100150212     C                   Z-ADD     005           VABLNP
023200150212     C                   Z-ADD     005           VATLNP
023300150212     C                   Z-ADD     000           VABCTR
023400140224     C                   MOVEL     '1'           VABCBO
023500140224     C                   MOVEL     '7Q'          VABCTM
023600020619     C* ... e poi verifico se sono stati passati come parametri
023700020619     C                   IF        vlrppt > *blanks
023800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024000020619     C                   EXSR      CHKNUM
024100020619     C                   IF        PiInt=*on
024200020619     C                   Z-ADD     PiVal         VABCCM
024300020619     C                   Z-ADD     PiVal         VATCCM
024400020619     C                   ENDIF
024500040506     C                   ENDIF
024600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024800020619     C                   EXSR      CHKNUM
024900020619     C                   IF        PiInt=*on
025000020619     C                   Z-ADD     PiVal         VABLNP
025100020619     C                   Z-ADD     PiVal         VATLNP
025200040506     C                   ENDIF
025300020619     C                   ENDIF
025400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
025500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025600020619     C                   EXSR      CHKNUM
025700020619     C                   IF        PiInt=*on
025800020619     C                   Z-ADD     PiVal         VABCTR
025900040506     C                   ENDIF
026000020619     C                   ENDIF
026100140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
026200140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
026300140224     C                   ENDIF
026400020619     C                   ENDIF
026500000801     C*
026600000801     C                   ENDSR
026700000801     C*----------------------------------------------------*
026800140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026900000801     C*----------------------------------------------------*
027000040823     C     IMPVAB        BEGSR
027100040823     C*
027200020305     C                   EXSR      INZVAR
027300020305     C                   EXSR      DEFCAM
027400010305     C*
027500000801     C                   Z-ADD     *zeros        errore            1 0
027600000830     C                   MOVEL     datcor        VABAAS
027700020305     C                   MOVEL     datcor        VATAAS
027800040526     C                   MOVE      datcor        VABMGS
027900040823     C                   MOVE(P)   vlrpoi        VABFGS
028000040823     C                   MOVE(P)   vlrpoi        VATFGS
028100050628     C*
028200150212     C                   EVAL      VABRSD=%trim(%subst(vindta:177:35))
028300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
028400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
028500020117     C* ==
028600150212     C                   EVAL      VABIND=%trim(%subst(vindta:212:35)) + ' '
028700150212     C                                  + %trim(%subst(vindta:247:35))
028800150212     C                   EVAL      VABLOD=%trim(%subst(vindta:291:35))
028900150212     C                   EVAL      VABNZD=%trim(%subst(vindta:326:2))
029000150212     C                   IF        VABNZD = 'I' or
029100150212     C                             VABNZD = 'IT' or
029200150212     C                             VABNZD = 'ITA'
029300150212     C                   EVAL      VABNZD = *blank
029400150212     C                   ENDIF
029500150212     C                   EVAL      VABRMA=%trim(%subst(vindta:431:35))
029600150212     C                   EVAL      VABNAS=%trim(%subst(vindta:587:26))
029700150212     C                   IF        %subst(vindta:850:70) <> *blank
029800150212     C                   EVAL      w70=*blank
029900150212     C                   EVAL      w70=%trim(%subst(vindta:850:70))
030000150212     C                   EVAL      VABNOT=%subst(w70:1:35)
030100150212     C                   EVAL      VABNT2=%subst(w70:36:35)
030200150212     C                   ENDIF
030300150302     C                   EVAL      VABTIC=%trim(%subst(vindta:625:70))
030400050628     C* CAP
030500150212     C                   EVAL      PiStr=%trim(%subst(vindta:282:9))
030600010201     C                   EXSR      CHKNUM
030700010201     C                   IF        PiInt=*on
030800140224     C                   Z-ADD     PiVal         Num5_0
030900040506     C                   MOVEL(P)  Num5_0        VABCAD
031000010201     C                   ELSE
031100040506     C                   ADD       1             errore
031200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031300040506     C                             + ' ' + 'VABCAD'
031400010201     C                   ENDIF
031500040506     C* Reperisco la provincia dal CAP e dalla località
031600040526     C                   IF        VABCAD <> *blanks AND
031700040526     C                             VABPRD  = *blanks
031800040506     C                   CLEAR                   TISI95DS
031900040506     C                   EVAL      I95TCN = '3'
032000040506     C                   Z-ADD     datcor        I95DAT
032100040506     C                   EVAL      I95CAP = VABCAD
032200040506     C                   EVAL      I95LOC = VABLOD
032300050627     C                   EVAL      I95NAR = VABNZD
032400040506     C                   CALL      'TISI95R'
032500040506     C                   PARM                    TISI95DS
032600040506     C                   EVAL      VABPRD = O95PRV
032700040506     C                   ENDIF
032800040506     C* PKB
032900150212     C                   EVAL      PiStr=%trim(%subst(vindta:522:15))
033000010201     C                   EXSR      CHKNUM
033100010201     C                   IF        PiNum=*on
033200070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
033300060316     C                   Z-ADD     PiVal         VABPKB
033400010201     C                   ELSE
033500010201     C                   ADD       1             errore
033600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033700010201     C                             + ' ' + 'VABPKB'
033800010201     C                   ENDIF
033900150212     C* VLB
034000150212     C                   EVAL      PiStr=%trim(%subst(vindta:537:15))
034100150212     C                   EXSR      CHKNUM
034200150212     C                   IF        PiNum=*on
034300150212     C                   EVAL      PiVal=PiVal/100                              da dm3 a m3
034400150212     C                   Z-ADD     PiVal         VABVLB
034500150212     C                   ELSE
034600150212     C                   ADD       1             errore
034700150212     C                   EVAL      vinmsg = %trimr(vinmsg)
034800150212     C                             + ' ' + 'VABVLB'
034900150212     C                   ENDIF
035000150212     C* NCL
035100150212     C                   EVAL      PiStr=%trim(%subst(vindta:504:6))
035200090511     C                   EXSR      CHKNUM
035300090511     C                   IF        PiInt=*on
035400150212     C                   Z-ADD     PiVal         VABNCL
035500090511     C                   ELSE
035600090511     C                   ADD       1             errore
035700090511     C                   EVAL      vinmsg = %trimr(vinmsg)
035800150212     C                             + ' ' + 'VABNCL'
035900090511     C                   ENDIF
036000150212     C* NSP / RMN
036100150212     C                   EVAL      PiStr=%trim(%subst(vindta:469:35))
036200150212     C                   EXSR      CHKNUM
036300150212     C                   IF        PiInt=*on
036400150212     C                   Z-ADD     PiVal         VABNSP
036500150212     C                   Z-ADD     PiVal         VATNSP
036600150212     C                   Z-ADD     PiVal         VABRMN
036700150212     C                   ELSE
036800150212     C                   ADD       1             errore
036900150212     C                   EVAL      vinmsg = %trimr(vinmsg)
037000150212     C                             + ' ' + 'VABNSP VATNSP VABRMN'
037100150212     C                   ENDIF
037200060307     C* CAS
037300150212     C                   IF        %trim(%subst(vindta:695:15))<>*zeros      AND
037400150212     C                             %trim(%subst(vindta:695:15))<>*blanks
037500150212     C                   EVAL      VABVCA = %trim(%subst(vindta:710:3))
037600060307     C                   EVAL      FlgCAS = '1'
037700150212     C                   EVAL      PiStr=%trim(%subst(vindta:695:15))
037800060307     C                   EXSR      CHKNUM
037900060307     C                   IF        PiNum=*on
038000150212     C                   EVAL      PiVal=PiVal/100                              da cent a unità
038100060316     C                   Z-ADD     PiVal         VABCAS
038200060307     C                   ELSE
038300060307     C                   ADD       1             errore
038400060307     C                   EVAL      vinmsg = %trimr(vinmsg)
038500060307     C                             + ' ' + 'VABCAS'
038600060307     C                   ENDIF
038700070102     C                   ENDIF
038800150212     C* IAS
038900150212     C                   IF        %trim(%subst(vindta:713:15))<>*zeros      AND
039000150212     C                             %trim(%subst(vindta:713:15))<>*blanks
039100150212     C                   EVAL      VABVAS = 'EUR'
039200150212     C                   EVAL      PiStr=%trim(%subst(vindta:713:15))
039300150212     C                   EXSR      CHKNUM
039400150212     C                   IF        PiNum=*on
039500150212     C                   EVAL      PiVal=PiVal/100                              da cent a unità
039600150212     C                   Z-ADD     PiVal         VABIAS
039700150212     C                   ELSE
039800150212     C                   ADD       1             errore
039900150212     C                   EVAL      vinmsg = %trimr(vinmsg)
040000150212     C                             + ' ' + 'VABIAS'
040100150212     C                   ENDIF
040200150212     C                   ENDIF
040300150212     C* DCR / HCR
040400150212     C                   IF        %trim(%subst(vindta:613:12))<>*zeros      AND
040500150212     C                             %trim(%subst(vindta:613:12))<>*blanks
040600150212     C                   EVAL      PiStr=%trim(%subst(vindta:613:12))
040700150212     C                   EXSR      CHKNUM
040800150212     C                   IF        PiInt=*on
040900150212     C                   EVAL      VABDCR=
041000150212     C                             %dec(%subst(%editc(PiVal:'X'):1:8):8:0)
041100150212     C                   EVAL      VABHCR=
041200150212     C                              %dec(%subst(%editc(PiVal:'X'):9:4):4:0)
041300150212     C                   ELSE
041400150212     C                   ADD       1             errore
041500150212     C                   EVAL      vinmsg = %trimr(vinmsg)
041600150212     C                             + ' ' + 'VABDCR VABHCR'
041700150212     C                   ENDIF
041800150212     C                   ENDIF
041900010205     C*
042000010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042100040802     C                   IF        FlgCAS <> '0'
042200070102     C                   IF        VABCBO = '1'
042300010205     C                   EVAL      VABCBO = '4'
042400010205     C                   ELSE
042500070102     C                   EVAL      VABCBO = '6'
042600070102     C                   ENDIF
042700010205     C                   ENDIF
042800020305     C*
042900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043000011113     C                   EXSR      CHKIMPDIV
043100010202     C*
043200000801     C* Ebbene...
043300000801     C                   ADD       1             §CTRMO
043400010201     C                   IF        errore <> *zeros
043500000801     C                   ADD       1             §CTRNO
043600000801     C                   EVAL      vinflg = '2'
043700000801     C                   ELSE
043800010201     C                   ADD       1             §CTROKVB
043900000801     C                   ENDIF
044000000801     C*
044100000801     C                   ENDSR
044200070102     C*----------------------------------------------------*
044300140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
044400070102     C*----------------------------------------------------*
044500070102     C     EXEVATA       BEGSR
044600070102     C*
044700070102     C                   EXSR      INZVAR
044800070102     C                   EXSR      DEFCAM
044900070102     C*
045000070102     C                   EVAL      VATTRC='A'
045100150212     C                   EVAL      VATNOT = %trim(%subst(vindta:353:35))
045200070102     C                   exsr      wrivat                                       => scarico VAT
045300070102     C*
045400070102     C                   ENDSR
045500060307     C*----------------------------------------------------*
045600140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
045700060307     C*----------------------------------------------------*
045800060307     C     EXEVATB       BEGSR
045900060307     C*
046000060307     C                   EXSR      INZVAR
046100060307     C                   EXSR      DEFCAM
046200060307     C*
046300060307     C                   EVAL      VATTRC='B'
046400150212     C                   EVAL      VATNOT = %trim(%subst(vindta:328:25))
046500060307     C                   exsr      wrivat                                       => scarico VAT
046600060307     C*
046700060307     C                   ENDSR
046800070402     C*----------------------------------------------------*
046900140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "E"
047000070402     C*----------------------------------------------------*
047100070402     C     EXEVATE       BEGSR
047200150212     C***
047300150212     C***                EXSR      INZVAR
047400150212     C***                EXSR      DEFCAM
047500150212     C***
047600150212     C***                EVAL      VATTRC='E'
047700150212     C***                EVAL      VATNOT = %trim(%subst(vindta:46:10))
047800150212     C***                exsr      wrivat                                       => scarico VAT
047900150212     C***
048000070402     C                   ENDSR
048100140224     C*----------------------------------------------------*
048200140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
048300140224     C*----------------------------------------------------*
048400140224     C     EXEVATI       BEGSR
048500140224     C*
048600140224     C                   EXSR      INZVAR
048700140224     C                   EXSR      DEFCAM
048800140224     C*
048900140224     C                   EVAL      VATTRC='I'
049000150212     C                   EVAL      VATNOT = %trim(%subst(vindta:734:35))
049100140224     C                   exsr      wrivat                                       => scarico VAT
049200140224     C*
049300140224     C                   EVAL      VATTRC='J'
049400150212     C                   EVAL      VATNOT = %trim(%subst(vindta:769:35))
049500140224     C                   exsr      wrivat                                       => scarico VAT
049600140224     C*
049700140224     C                   ENDSR
049800140226     C*----------------------------------------------------*
049900140408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "S"
050000140226     C*----------------------------------------------------*
050100140226     C     EXEVATS       BEGSR
050200140226     C*
050300140226     C                   EXSR      INZVAR
050400140226     C                   EXSR      DEFCAM
050500140226     C*
050600140226     C                   EVAL      VATTRC='S'
050700150212     C                   EVAL      VATNOT =
050800150212     C                              %trim(%subst(vindta:804:35))
050900140226     C                   IF        VATNOT <> *blank
051000150212     C                   EVAL      %subst(VATNOT:17:2) = '  '
051100140226     C                   ENDIF
051200140226     C                   exsr      wrivat                                       => scarico VAT
051300140226     C*
051400140226     C                   ENDSR
051500010201     C*----------------------------------------------------*
051600140408     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
051700010201     C*----------------------------------------------------*
051800020305     C     WRIVAT        BEGSR
051900140408     C*
052000140408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
052100150212     C                   MOVE(P)   vlrMSG        wNomeFile
052200150212     C                   EVAL      VATCMR=%subst(wNomeFile:7:12)
052300140408     C                   EVAL      VATCNT = 1
052400050628     C*
052500060223     C* Scrivo solo se valorizzato qualcosa
052600060223     C                   IF        VATNOT <> *blanks
052700140408     C                   WRITE     EDIVAT00
052800060223     C                   ENDIF
052900010201     C*
053000010201     C                   ENDSR
053100010202     C*----------------------------------------------------*
053200140408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
053300010202     C*----------------------------------------------------*
053400020305     C     PREVAT        BEGSR
053500010202     C*
053600140408     C* Compongo il nome del membro da dare al EDIVATWR
053700010202     C                   eval      parmbr = vlrhdl
053800010202     C                   movel     'M'           parmbr
053900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
054000010202     C                   eval      paropz = '1'
054100010202     C* Effettuo la chiamata al CLLE preposto
054200140408     C                   call(e)   'TITVEVTC'
054300010202     C                   parm                    parccm
054400010202     C                   parm                    parmbr
054500010202     C                   parm                    paropz
054600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054700010202     C                   if        %error
054800010202     C                   movel     '1'           chkcall
054900010202     C                   else
055000010202     C                   movel     '0'           chkcall
055100010202     C                   endif
055200010202     C*
055300010202     C                   ENDSR
055400000801     C*----------------------------------------------------*
055500000801     C*  CONTROLLO NUMERICITA' CAMPI
055600000801     C*----------------------------------------------------*
055700000801     C     CHKNUM        BEGSR
055800000801     C*
055900000801     C                   call(e)   'ISNUMERIC'
056000000801     C                   PARM                    PiStr            30
056100140226     C                   PARM      '.'           PiDecChr          1
056200000801     C                   PARM      *ZEROS        PiVal            30 9
056300000801     C                   PARM      '0'           PiInt             1
056400000801     C                   PARM      '0'           PiNum             1
056500000801     C                   IF        %error
056600000801     C                   EVAL      PiInt=*off
056700000801     C                   ENDIF
056800000801     C*
056900000801     C                   ENDSR
057000000801     C***
057100000801
057200011113
057300011113     C*----------------------------------------------------*
057400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057500011113     C*----------------------------------------------------*
057600011113     C     CHKIMPDIV     BEGSR
057700011113     C*
057800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057900011113     C                   Z-ADD     *zeros        wrkDec            9 9
058000011113     C*
058100011113     C* Come prima cosa effettuo considerazioni sulla divisa
058200011113     C                   IF        vabIAS > *zeros
058300011113     C                   IF        vabVAS <> 'EUR'
058400011113     C                   EVAL      vabVAS =  'ITL'
058500011113     C                   ENDIF
058600011113     C                   ENDIF
058700011113     C*
058800011113     C                   IF        vabCAS > *zeros
058900011113     C                   IF        vabVCA <> 'EUR'
059000011113     C                   EVAL      vabVCA =  'ITL'
059100011113     C                   ENDIF
059200011113     C                   ENDIF
059300011113     C*
059400011113     C                   IF        vabVMD > *zeros
059500020305     C                   IF        vabVAD <> 'EUR'
059600011113     C                   EVAL      vabVAD =  'ITL'
059700011113     C                   ENDIF
059800011113     C                   ENDIF
059900011113     C*
060000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060100011113     C                   Z-ADD     vabIAS        wrkDec
060200011113     C                   IF        wrkDec > *zeros
060300011113     C                   IF        vabVAS = 'ITL'
060400011113     C                   EVAL      vabIAS = *zeros
060500011113     C                   ENDIF
060600011113     C                   ENDIF
060700011113     C*
060800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060900011113     C                   Z-ADD     vabCAS        wrkDec
061000011113     C                   IF        wrkDec > *zeros
061100011113     C                   IF        vabVCA = 'ITL'
061200011113     C                   EVAL      vabCAS = *zeros
061300011113     C                   ENDIF
061400011113     C                   ENDIF
061500011113     C*
061600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061700011113     C                   Z-ADD     vabVMD        wrkDec
061800011113     C                   IF        wrkDec > *zeros
061900011113     C                   IF        vabVAD = 'ITL'
062000011113     C                   EVAL      vabVMD = *zeros
062100011113     C                   ENDIF
062200011113     C                   ENDIF
062300011113     C*
062400011113     C                   ENDSR
062500011113     C***
062600011113
062700011113
062800000801
062900000801
063000990920      /TITLE Invio dei dati al punto operativo.
063100010202     C     invio         BEGSR
063200990920     C*
063300140408     C* 1° invio EDIVAT
063400010201     C                   reset                   dscmz
063500010201     C                   move      vlrpoi        cmzdst
063600140408     C                   eval      cmzfld = 'EDIVATWR'
063700010201     C                   eval      cmzmbd = vlrhdl
063800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063900021009     C***                if        prmfir = *blanks
064000140408     C                   eval      cmzfla = 'EDIVAT0F'
064100140408     C                   eval      cmzmba = 'EDIVAT0F'
064200021009     C***                else
064300021009     C***                eval      cmzfla = prmfir
064400021009     C***                eval      cmzmba = prmfir
064500021009     C***                endif
064600010201     C                   eval      cmznrr = *zeros
064700020305     C                   move      §ctrokvt      cmznrr
064800021018     C                   eval      cmzlba = vlrfl1
064900010201     C                   call(e)   'TIS711C'
065000010201     C                   parm                    dscmz
065100010201     C                   parm      *blanks       esito
065200010205     C                   if        %error
065300010205     C                             or cmzerr = '1'
065400010205     C                             or esito  = '1'
065500010205     C                   eval      wrkesito = '3'
065600010205     C                   else
065700010201     C*
065800140408     C* 2° invio EùDIVAB
065900010201     C                   reset                   dscmz
066000010201     C                   move      vlrpoi        cmzdst
066100010201     C                   eval      cmzfld = vlrfou
066200010201     C                   eval      cmzmbd = vlrhdl
066300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066400021009     C***                if        prmfir = *blanks
066500140408     C                   eval      cmzfla = 'EDIVAB0F'
066600140408     C                   eval      cmzmba = 'EDIVAB0F'
066700021009     C***                else
066800021009     C***                eval      cmzfla = prmfir
066900021009     C***                eval      cmzmba = prmfir
067000021009     C***                endif
067100010201     C                   eval      cmznrr = *zeros
067200010201     C                   move      §ctrokvb      cmznrr
067300021018     C                   eval      cmzlba = vlrfl1
067400010201     C                   call(e)   'TIS711C'
067500010201     C                   parm                    dscmz
067600010201     C                   parm      *blanks       esito
067700010201     C                   if        %error
067800010201     C                             or cmzerr = '1'
067900010201     C                             or esito  = '1'
068000010201     C                   eval      wrkesito = '3'
068100010201     C                   endif
068200010205     C                   endif
068300990920     C*
068400000613     C                   ENDSR
068500000613     C***
068600990910
068700000613     C     *inzsr        BEGSR
068800990910     C*
068900990910     C     *entry        plist
069000990920     C                   parm                    tivlrds
069100990921     C                   parm      wrkesito      esito
069200000724     C                   parm                    prmlit
069300000710     C                   parm                    prmfir
069400000613     C*
069500000830     C* CALCOLA LA DATA CORRENTE
069600140408     C                   time                    wn14             14 0
069700140408     C                   movel     wn14          oracor            6 0          *ORA
069800100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
069900100118     C                   eval      datcor = %dec(%date() : *ISO)
070000000830     C*
070100000613     C                   ENDSR
070200000613     C***
