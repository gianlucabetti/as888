000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500090114     FFIVABwwr  O    E             DISK    usropn
000600090114     FFIVATwwr  O    E             DISK    usropn
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
002500000830
002600000830     D*------------------
002700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800000830     D*------------------
002900000830     D WLBDA8          DS                  INZ
003000000830     D  G08DAT                 1      8  0
003100000830     D  G08INV                 9     16  0
003200000830     D  G08ERR                17     17
003300000830     D  G08TGI                18     22  0
003400090114
003500081217
003600081217     D*------------------
003700081217     D* LINKING A DEFINIZIONI ESTERNE
003800081217     D*------------------
003900081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004000081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004100081217
004200081217
004300010201
004400081217
004500081217     C
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900000613     C*
005000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005100000613     C*
005200010202     C* Effettuo la chiamata al CLLE preposto
005300090114     C                   call(e)   'TITVVTC'
005400010202     C                   parm                    parccm
005500010202     C                   parm                    parmbr
005600010202     C                   parm      '2'           paropz
005700050201     C*
005800050201     C* Effettuo lancio TISI95 solo x chiusura
005900050201     C                   CLEAR                   TISI95DS
006000050201     C                   EVAL      I95TLA = 'C'
006100050201     C                   CALL      'TISI95R'
006200050201     C                   PARM                    TISI95DS
006300000616     C*
006400000801     C
006500010201     C                   seton                                        LR
006600990908
006700000801
006800910830     C*--------------------------------------------------------
006900090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007000910830     C*--------------------------------------------------------
007100040526     C     RWFILE        BEGSR
007200990910     C*
007300990914     C                   if        not %open(tivin00r)
007400990908     C                   open      tivin00r
007500990914     C                   endif
007600090114     C                   if        not %open(fivabwwr)
007700090114     C                   open      fivabwwr
007800990914     C                   endif
007900070103     C*
008000090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008100020305     C                   exsr      prevat
008200010201     C*
008300010202     C                   if        chkcall = '0'
008400010202     C*
008500090114     C                   if        not %open(fivatwwr)
008600090114     C                   open      fivatwwr
008700010201     C                   endif
008800990910     C*
008900010201     C                   clear                   §CTROKVB          5 0
009000020305     C                   clear                   §CTROKVT          5 0
009100000801     C                   clear                   §CTRMO            5 0
009200000801     C                   clear                   §CTRNO            5 0
009300990910     C*
009400921023     C                   DO        *HIVAL
009500990913     C*
009600990915     C                   READ      tivin00r                               70
009700050627     C                   if        vindta > *blanks
009800000613     C                   add       1             rrnum
009900000801     C*
010000000801     C                   if        *in70 = *off
010100000801     C                             and
010200000801     C                             (vinflg = *blanks
010300000801     C                              or vinflg = '0'
010400000801     C                              or vinflg = '2')
010500000801     C*
010600000801     C                   clear                   vinmsg
010700000801     C                   eval      vinflg = '1'
010800070103     C*
010900090309     C                   EXSR      INZVAR
011000090309     C                   EXSR      DEFCAM
011100090527     C*
011200070213     C                   exsr      impvab                                       => carico  VAB
011300090714     C                   exsr      wrivab                                       => scarico VAB
011400090714     C                   exsr      exevata
011500090714     C                   exsr      exevatb
011600000905     C*
011700000905     C                   else
011800000905     C                   eval      vinflg = '1'
011900050628     C                   endif
012000000905     C                   endif
012100000905     C*
012200000905     C  N70              update    tivin000
012300000905     C*
012400991022     C  N70              ENDdo
012500010202     C*
012600010202     C                   endif
012700990910
012800990910     C* Se non ci sono record con errori ...
012900000710     C                   if        §ctrno = 0
013000990910     C* ... restituisco esito OK.
013100990921     C                   eval      wrkesito = '0'
013200990910     C                   else
013300010201     C                   if        §ctrokvb > 0
013400990921     C                   eval      wrkesito = '1'
013500000710     C                   else
013600000710     C                   eval      wrkesito = '2'
013700990910     C                   endif
013800000710     C                   endif
013900990910     C*
014000990914     C                   if        %open(tivin00r)
014100990908     C                   close     tivin00r
014200990914     C                   endif
014300090114     C                   if        %open(fivabwwr)
014400090114     C                   close     fivabwwr
014500990914     C                   endif
014600090114     C                   if        %open(fivatwwr)
014700090114     C                   close     fivatwwr
014800010201     C                   endif
014900990910     C*
015000010201     C                   if        §ctrokvb > 0
015100000724     C                             and vlrpoi <> *zeros
015200010202     C                   exsr      invio
015300990920     C                   endif
015400990920     C*
015500910830     C                   ENDSR
015600000613     C***
015700010305
015800010305     C*----------------------------------------------------*
015900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
016000010305     C*----------------------------------------------------*
016100020305     C     WRIVAB        BEGSR
016200010305     C*
016300060225     C* Quindi scarico il buffer del file d testata
016400090114     C                   write     fivab000                                     => scarico il VAB
016500010305     C*
016600010305     C                   ENDSR
016700990920
016800000801     C*----------------------------------------------------*
016900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017000000801     C*----------------------------------------------------*
017100010201     C     INZVAR        BEGSR
017200000801     C*
017300090114     C                   CLEAR                   FIVAB000
017400090114     C                   CLEAR                   FIVAT000
017500070213     C*
017600040802     C                   Z-ADD     *zeros        Num5_0            5 0
017700040802     C                   MOVEL     '0'           FlgCAS            1
017800000801     C*
017900000801     C                   ENDSR
018000000801     C*----------------------------------------------------*
018100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
018200000801     C*----------------------------------------------------*
018300000801     C     DEFCAM        BEGSR
018400000801     C*
018500020619     C* Imposto i valori di default...
018600090527     C                   MOVEL     datcor        VABAAS
018700090527     C                   MOVEL     datcor        VATAAS
018800090527     C                   MOVE(P)   vlrpoi        VABFGS
018900090527     C                   MOVE(P)   vlrpoi        VATFGS
019000090527     C*
019100090714     C                   Z-ADD     0061248       VABCCM
019200090714     C                   Z-ADD     0061248       VATCCM
019300090714     C                   Z-ADD     006           VABLNP
019400090714     C                   Z-ADD     006           VATLNP
019500090720     C                   Z-ADD     300           VABCTR
019600040823     C                   MOVEL     '1'           VABCBO
019700090527     C                   MOVEL     '7Q'          VABCTM
019800020619     C* ... e poi verifico se sono stati passati come parametri
019900020619     C                   IF        vlrppt > *blanks
020000040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020200020619     C                   EXSR      CHKNUM
020300020619     C                   IF        PiInt=*on
020400020619     C                   Z-ADD     PiVal         VABCCM
020500020619     C                   Z-ADD     PiVal         VATCCM
020600020619     C                   ENDIF
020700040506     C                   ENDIF
020800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021000020619     C                   EXSR      CHKNUM
021100020619     C                   IF        PiInt=*on
021200020619     C                   Z-ADD     PiVal         VABLNP
021300020619     C                   Z-ADD     PiVal         VATLNP
021400040506     C                   ENDIF
021500020619     C                   ENDIF
021600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021800020619     C                   EXSR      CHKNUM
021900020619     C                   IF        PiInt=*on
022000020619     C                   Z-ADD     PiVal         VABCTR
022100040506     C                   ENDIF
022200020619     C                   ENDIF
022300120104     C                   IF        %subst(vlrppt:14:2) <> *blanks
022400120104     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
022500120104     C                   ENDIF
022600020619     C                   ENDIF
022700000801     C*
022800000801     C                   ENDSR
022900000801     C*----------------------------------------------------*
023000090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023100000801     C*----------------------------------------------------*
023200040823     C     IMPVAB        BEGSR
023300010305     C*
023400000801     C                   Z-ADD     *zeros        errore            1 0
023500040526     C                   MOVE      datcor        VABMGS
023600080409     C*
023700080409     C* Reperimento campi ALFA
023800080409     C*
023900080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
024000090714     C                   EVAL      VABRSD=%trim(%subst(vindta:51:40))
024100080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024200080409     C     '@':'A'       XLATE     VABRSD        VABRSD
024300080409     C* ==
024400090714     C                   EVAL      VABRD2=%trim(%subst(vindta:91:40))
024500090714     C                   EVAL      VABIND=%trim(%subst(vindta:131:40))+
024600090714     C                                    %trim(%subst(vindta:171:40))
024700090714     C                   EVAL      VABLOD=%trim(%subst(vindta:211:40))
024800090714     C                   EVAL      VABPRD=%trim(%subst(vindta:260:2))
024900090714     C                   EVAL      VABRMA=%trim(%subst(vindta:26:15))
025000120104     C                   EVAL      VABTIC=%trim(%subst(vindta:471:2))
025100080409     C*
025200080409     C* Reperimento campi NUMERICI
025300090609     C*
025400140312     C* AAS
025500140312     C                   EVAL      PiStr=%trim(%subst(vindta:43:4))
025600140312     C                   EXSR      CHKNUM
025700140312     C                   IF        PiInt=*on
025800140312     C                   Z-ADD     PiVal         VABAAS
025900140312     C                   Z-ADD     PiVal         VATAAS
026000140312     C                   ELSE
026100140312     C                   EVAL      vinmsg = %trimr(vinmsg)
026200140312     C                             + ' ' + 'VABAAS VATAAS'
026300140312     C                   ENDIF
026400111213     C* CCM
026500111213     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
026600111213     C                   EXSR      CHKNUM
026700111213     C                   IF        PiInt=*on
026800111213     C                   Z-ADD     PiVal         VABCCM
026900111213     C                   Z-ADD     PiVal         VATCCM
027000111213     C                   ELSE
027100111213     C                   EVAL      vinmsg = %trimr(vinmsg)
027200111213     C                             + ' ' + 'VABCCM VATCCM'
027300111213     C                   ENDIF
027400140311     C*
027500140311     C* Forzatura su codice / linea di partenza
027600140319     C                   IF        VABCCM = 0066123 OR
027700140319     C                             VABCCM = 0066266
027800140311     C                   EVAL      VABLNP = 059
027900140311     C                   EVAL      VATLNP = 059
028000140311     C                   EVAL      VABFGS = 059
028100140311     C                   EVAL      VATFGS = 059
028200140311     C                   ENDIF
028300140717     C* LNP/FGS
028400140717     C                   IF        %subst(vindta:473:3) <> *blank
028500140717     C                   EVAL      PiStr=%trim(%subst(vindta:473:3))
028600140717     C                   EXSR      CHKNUM
028700140717     C                   IF        PiInt=*on
028800140717     C                   Z-ADD     PiVal         VABLNP
028900140717     C                   Z-ADD     PiVal         VABFGS
029000140717     C                   Z-ADD     PiVal         VATLNP
029100140717     C                   Z-ADD     PiVal         VATFGS
029200140717     C                   ELSE
029300140717     C                   EVAL      vinmsg = %trimr(vinmsg)
029400140717     C                             + ' ' + 'VABLNP VABFGS VATLNP VATFGS'
029500140717     C                   ENDIF
029600140717     C                   ENDIF
029700090309     C* NSP
029800090714     C                   EVAL      PiStr=%trim(%subst(vindta:26:15))
029900090309     C                   EXSR      CHKNUM
030000090309     C                   IF        PiInt=*on
030100090309     C                   Z-ADD     PiVal         VABNSP
030200090309     C                   Z-ADD     PiVal         VATNSP
030300090309     C                   ELSE
030400090309     C                   EVAL      vinmsg = %trimr(vinmsg)
030500090309     C                             + ' ' + 'VABNSP VATNSP'
030600090309     C                   ENDIF
030700090309     C* RMN
030800090714     C                   EVAL      PiStr=%trim(%subst(vindta:26:15))
030900081217     C                   EXSR      CHKNUM
031000081217     C                   IF        PiInt=*on
031100081217     C                   Z-ADD     PiVal         VABRMN
031200081217     C                   ELSE
031300081217     C                   ADD       1             errore
031400081217     C                   Z-ADD     1             VABRMN
031500081217     C                   EVAL      vinmsg = %trimr(vinmsg)
031600081217     C                             + ' ' + 'VABRMN'
031700081217     C                   ENDIF
031800080409     C* CAD
031900090714     C                   EVAL      PiStr=%trim(%subst(vindta:251:9))
032000090114     C                   EXSR      CHKNUM
032100090114     C                   IF        PiInt=*on
032200090114     C                   Z-ADD     PiVal         Num5_0
032300090114     C                   MOVEL(p)  Num5_0        VABCAD
032400090114     C                   ELSE
032500090114     C                   ADD       1             errore
032600090114     C                   EVAL      VABCAD = *zeros
032700090114     C                   EVAL      vinmsg = %trimr(vinmsg)
032800090114     C                             + ' ' + 'VABCAD'
032900090114     C                   ENDIF
033000081217     C* NCL
033100090714     C                   EVAL      PiStr=%trim(%subst(vindta:288:5))
033200081217     C                   EXSR      CHKNUM
033300081217     C                   IF        PiInt=*on
033400081217     C                   Z-ADD     PiVal         VABNCL
033500081217     C                   ELSE
033600090309     C                   ADD       1             errore
033700081217     C                   EVAL      vinmsg = %trimr(vinmsg)
033800081217     C                             + ' ' + 'VABNCL'
033900081217     C                   ENDIF
034000090312     C* PKB
034100090714     C                   EVAL      PiStr=%trim(%subst(vindta:293:7))
034200090312     C                   EXSR      CHKNUM
034300090312     C                   IF        PiNum=*on
034400090714     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
034500090527     C                   Z-ADD     PiVal         VABPKB
034600090312     C                   ELSE
034700090312     C                   ADD       1             errore
034800090312     C                   EVAL      vinmsg = %trimr(vinmsg)
034900090312     C                             + ' ' + 'VABPKB'
035000090312     C                   ENDIF
035100090527     C* VLB
035200090714     C                   EVAL      PiStr=%trim(%subst(vindta:300:7))
035300090527     C                   EXSR      CHKNUM
035400090527     C                   IF        PiNum=*on
035500090609     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
035600090527     C                   Z-ADD     PiVal         VABVLB
035700090527     C                   ELSE
035800090527     C                   ADD       1             errore
035900090527     C                   EVAL      vinmsg = %trimr(vinmsg)
036000090527     C                             + ' ' + 'VABVLB'
036100090527     C                   ENDIF
036200090527     C* CAS
036300090714     C                   IF        %subst(vindta:311:15) <> *blanks AND
036400090714     C                             %subst(vindta:311:15) <> *zeros
036500090609     C                   MOVEL     '1'           FlgCAS
036600090714     C                   EVAL      VABVCA='EUR'
036700090714     C                   EVAL      PiStr=%trim(%subst(vindta:311:15))
036800090428     C                   EXSR      CHKNUM
036900090428     C                   IF        PiNum=*on
037000090714     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
037100090527     C                   Z-ADD     PiVal         VABCAS
037200090428     C                   ELSE
037300090428     C                   ADD       1             errore
037400090428     C                   EVAL      vinmsg = %trimr(vinmsg)
037500090527     C                             + ' ' + 'VABCAS'
037600090428     C                   ENDIF
037700090428     C                   ENDIF
037800010205     C*
037900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038000040802     C                   IF        FlgCAS <> '0'
038100090609     C                   IF        VABCBO = '1'
038200010205     C                   EVAL      VABCBO = '4'
038300090609     C                   ENDIF
038400090609     C                   IF        VABCBO = '2'
038500090609     C                   EVAL      VABCBO = '6'
038600090609     C                   ENDIF
038700010205     C                   ENDIF
038800020305     C*
038900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039000011113     C                   EXSR      CHKIMPDIV
039100010202     C*
039200000801     C* Ebbene...
039300000801     C                   ADD       1             §CTRMO
039400010201     C                   IF        errore <> *zeros
039500000801     C                   ADD       1             §CTRNO
039600000801     C                   EVAL      vinflg = '2'
039700000801     C                   ELSE
039800010201     C                   ADD       1             §CTROKVB
039900000801     C                   ENDIF
040000000801     C*
040100000801     C                   ENDSR
040200070102     C*----------------------------------------------------*
040300090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
040400070102     C*----------------------------------------------------*
040500070103     C     EXEVATE       BEGSR
040600090527     C*
040700090527     C                   EVAL      VATTRC = 'E'
040800090428     C*
040900090609     C***                exsr      wrivat                                       => scarico VAT
041000070102     C*
041100070102     C                   ENDSR
041200080415     C*----------------------------------------------------*
041300090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
041400080415     C*----------------------------------------------------*
041500080415     C     EXEVATA       BEGSR
041600080415     C*
041700090714     C***                EVAL      VATTRC = 'A'
041800090714     C***                EVAL      VATNOT = %trim(%subst(vindta:495:22))
041900080415     C*
042000090609     C                   exsr      wrivat                                       => scarico VAT
042100080415     C*
042200080415     C                   ENDSR
042300071121     C*----------------------------------------------------*
042400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
042500071121     C*----------------------------------------------------*
042600071121     C     EXEVATB       BEGSR
042700071121     C*
042800090504     C                   EVAL      VATTRC = 'B'
042900090714     C                   EVAL      VATNOT = %trim(%subst(vindta:386:20))
043000071121     C*
043100090609     C                   exsr      wrivat                                       => scarico VAT
043200071121     C*
043300071121     C                   ENDSR
043400010201     C*----------------------------------------------------*
043500090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043600010201     C*----------------------------------------------------*
043700020305     C     WRIVAT        BEGSR
043800050628     C*
043900060223     C* Scrivo solo se valorizzato qualcosa
044000060223     C                   IF        VATNOT <> *blanks
044100090114     C                   WRITE     FIVAT000
044200060223     C                   ENDIF
044300010201     C*
044400010201     C                   ENDSR
044500010202     C*----------------------------------------------------*
044600090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044700010202     C*----------------------------------------------------*
044800020305     C     PREVAT        BEGSR
044900010202     C*
045000090114     C* Compongo il nome del membro da dare al FIVATWWR
045100010202     C                   eval      parmbr = vlrhdl
045200010202     C                   movel     'M'           parmbr
045300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
045400010202     C                   eval      paropz = '1'
045500010202     C* Effettuo la chiamata al CLLE preposto
045600090114     C                   call(e)   'TITVVTC'
045700010202     C                   parm                    parccm
045800010202     C                   parm                    parmbr
045900010202     C                   parm                    paropz
046000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046100010202     C                   if        %error
046200010202     C                   movel     '1'           chkcall
046300010202     C                   else
046400010202     C                   movel     '0'           chkcall
046500010202     C                   endif
046600010202     C*
046700010202     C                   ENDSR
046800000801     C*----------------------------------------------------*
046900000801     C*  CONTROLLO NUMERICITA' CAMPI
047000000801     C*----------------------------------------------------*
047100000801     C     CHKNUM        BEGSR
047200081217     C*
047300081217     C                   IF        PiDecChr = *blanks
047400090519     C                   EVAL      PiDecChr = ','
047500081217     C                   ENDIF
047600081217     C*
047700081217     C                   callp(e)  UBISNUM_Check(PiStr
047800081217     C                                          :PiDecChr
047900081217     C                                          :PiVal
048000081217     C                                          :PiNum
048100081217     C                                          :PiInt)
048200081217     C*
048300081217     C                   IF        %error
048400081217     C                   EVAL      PiInt=*off
048500081217     C                   ENDIF
048600000801     C*
048700000801     C                   ENDSR
048800000801     C***
048900000801
049000011113
049100011113     C*----------------------------------------------------*
049200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049300011113     C*----------------------------------------------------*
049400011113     C     CHKIMPDIV     BEGSR
049500011113     C*
049600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049700011113     C                   Z-ADD     *zeros        wrkDec            9 9
049800011113     C*
049900011113     C* Come prima cosa effettuo considerazioni sulla divisa
050000011113     C                   IF        vabIAS > *zeros
050100011113     C                   IF        vabVAS <> 'EUR'
050200011113     C                   EVAL      vabVAS =  'ITL'
050300011113     C                   ENDIF
050400011113     C                   ENDIF
050500011113     C*
050600011113     C                   IF        vabCAS > *zeros
050700011113     C                   IF        vabVCA <> 'EUR'
050800011113     C                   EVAL      vabVCA =  'ITL'
050900011113     C                   ENDIF
051000011113     C                   ENDIF
051100011113     C*
051200011113     C                   IF        vabVMD > *zeros
051300020305     C                   IF        vabVAD <> 'EUR'
051400011113     C                   EVAL      vabVAD =  'ITL'
051500011113     C                   ENDIF
051600011113     C                   ENDIF
051700011113     C*
051800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051900011113     C                   Z-ADD     vabIAS        wrkDec
052000011113     C                   IF        wrkDec > *zeros
052100011113     C                   IF        vabVAS = 'ITL'
052200011113     C                   EVAL      vabIAS = *zeros
052300011113     C                   ENDIF
052400011113     C                   ENDIF
052500011113     C*
052600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052700011113     C                   Z-ADD     vabCAS        wrkDec
052800011113     C                   IF        wrkDec > *zeros
052900011113     C                   IF        vabVCA = 'ITL'
053000011113     C                   EVAL      vabCAS = *zeros
053100011113     C                   ENDIF
053200011113     C                   ENDIF
053300011113     C*
053400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053500011113     C                   Z-ADD     vabVMD        wrkDec
053600011113     C                   IF        wrkDec > *zeros
053700011113     C                   IF        vabVAD = 'ITL'
053800011113     C                   EVAL      vabVMD = *zeros
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C                   ENDSR
054300011113     C***
054400011113
054500011113
054600000801
054700000801
054800990920      /TITLE Invio dei dati al punto operativo.
054900010202     C     invio         BEGSR
055000990920     C*
055100090114     C* 1° invio FIVAT
055200010201     C                   reset                   dscmz
055300010201     C                   move      vlrpoi        cmzdst
055400090114     C                   eval      cmzfld = 'FIVATWWR'
055500010201     C                   eval      cmzmbd = vlrhdl
055600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055700021009     C***                if        prmfir = *blanks
055800090114     C                   eval      cmzfla = 'FIVAT00F'
055900090114     C                   eval      cmzmba = 'FIVAT00F'
056000021009     C***                else
056100021009     C***                eval      cmzfla = prmfir
056200021009     C***                eval      cmzmba = prmfir
056300021009     C***                endif
056400010201     C                   eval      cmznrr = *zeros
056500020305     C                   move      §ctrokvt      cmznrr
056600021018     C                   eval      cmzlba = vlrfl1
056700010201     C                   call(e)   'TIS711C'
056800010201     C                   parm                    dscmz
056900010201     C                   parm      *blanks       esito
057000010205     C                   if        %error
057100010205     C                             or cmzerr = '1'
057200010205     C                             or esito  = '1'
057300010205     C                   eval      wrkesito = '3'
057400010205     C                   else
057500010201     C*
057600090114     C* 2° invio FIVAB
057700010201     C                   reset                   dscmz
057800010201     C                   move      vlrpoi        cmzdst
057900010201     C                   eval      cmzfld = vlrfou
058000010201     C                   eval      cmzmbd = vlrhdl
058100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058200021009     C***                if        prmfir = *blanks
058300090114     C                   eval      cmzfla = 'FIVAB00F'
058400090114     C                   eval      cmzmba = 'FIVAB00F'
058500021009     C***                else
058600021009     C***                eval      cmzfla = prmfir
058700021009     C***                eval      cmzmba = prmfir
058800021009     C***                endif
058900010201     C                   eval      cmznrr = *zeros
059000010201     C                   move      §ctrokvb      cmznrr
059100021018     C                   eval      cmzlba = vlrfl1
059200010201     C                   call(e)   'TIS711C'
059300010201     C                   parm                    dscmz
059400010201     C                   parm      *blanks       esito
059500010201     C                   if        %error
059600010201     C                             or cmzerr = '1'
059700010201     C                             or esito  = '1'
059800010201     C                   eval      wrkesito = '3'
059900010201     C                   endif
060000010205     C                   endif
060100990920     C*
060200000613     C                   ENDSR
060300000613     C***
060400070411
060500090331
060600090331
060700090331
060800070411     C     *pssr         BEGSR
060900070411     C*
061000070411     C                   if        %open(tivin00r)
061100070411     C                   close     tivin00r
061200070411     C                   endif
061300090114     C                   if        %open(fivabwwr)
061400090114     C                   close     fivabwwr
061500070411     C                   endif
061600090114     C                   if        %open(fivatwwr)
061700090114     C                   close     fivatwwr
061800070411     C                   endif
061900070411     C*
062000070411     C* Effettuo la chiamata al CLLE preposto
062100090114     C                   call(e)   'TITVVTC'
062200070411     C                   parm                    parccm
062300070411     C                   parm                    parmbr
062400070411     C                   parm      '2'           paropz
062500070411     C*
062600070411     C                   eval      wrkesito = '2'
062700070411     C*
062800070411     C                   seton                                        LR
062900070411     C*
063000070411     C                   ENDSR     '*CANCL'
063100070411     C***
063200070411
063300090331
063400090331
063500990910
063600000613     C     *inzsr        BEGSR
063700990910     C*
063800990910     C     *entry        plist
063900990920     C                   parm                    tivlrds
064000990921     C                   parm      wrkesito      esito
064100000724     C                   parm                    prmlit
064200000710     C                   parm                    prmfir
064300000613     C*
064400000830     C* CALCOLA LA DATA CORRENTE
064500000830     C                   time                    wn14             14 0
064600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064700080512     C                   movel     wn14          oracor            6 0          *ORA
064800000830     C                   z-add     wn8           g08dat
064900000830     C                   z-add     *zeros        g08inv
065000000830     C                   movel     '0'           g08err
065100000830     C                   call      'XSRDA8'
065200000830     C                   parm                    wlbda8
065300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065400000830     C*
065500000613     C                   ENDSR
065600000613     C***
