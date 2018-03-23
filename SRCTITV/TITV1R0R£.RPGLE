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
028300090309     C* NSP
028400090714     C                   EVAL      PiStr=%trim(%subst(vindta:26:15))
028500090309     C                   EXSR      CHKNUM
028600090309     C                   IF        PiInt=*on
028700090309     C                   Z-ADD     PiVal         VABNSP
028800090309     C                   Z-ADD     PiVal         VATNSP
028900090309     C                   ELSE
029000090309     C                   EVAL      vinmsg = %trimr(vinmsg)
029100090309     C                             + ' ' + 'VABNSP VATNSP'
029200090309     C                   ENDIF
029300090309     C* RMN
029400090714     C                   EVAL      PiStr=%trim(%subst(vindta:26:15))
029500081217     C                   EXSR      CHKNUM
029600081217     C                   IF        PiInt=*on
029700081217     C                   Z-ADD     PiVal         VABRMN
029800081217     C                   ELSE
029900081217     C                   ADD       1             errore
030000081217     C                   Z-ADD     1             VABRMN
030100081217     C                   EVAL      vinmsg = %trimr(vinmsg)
030200081217     C                             + ' ' + 'VABRMN'
030300081217     C                   ENDIF
030400080409     C* CAD
030500090714     C                   EVAL      PiStr=%trim(%subst(vindta:251:9))
030600090114     C                   EXSR      CHKNUM
030700090114     C                   IF        PiInt=*on
030800090114     C                   Z-ADD     PiVal         Num5_0
030900090114     C                   MOVEL(p)  Num5_0        VABCAD
031000090114     C                   ELSE
031100090114     C                   ADD       1             errore
031200090114     C                   EVAL      VABCAD = *zeros
031300090114     C                   EVAL      vinmsg = %trimr(vinmsg)
031400090114     C                             + ' ' + 'VABCAD'
031500090114     C                   ENDIF
031600081217     C* NCL
031700090714     C                   EVAL      PiStr=%trim(%subst(vindta:288:5))
031800081217     C                   EXSR      CHKNUM
031900081217     C                   IF        PiInt=*on
032000081217     C                   Z-ADD     PiVal         VABNCL
032100081217     C                   ELSE
032200090309     C                   ADD       1             errore
032300081217     C                   EVAL      vinmsg = %trimr(vinmsg)
032400081217     C                             + ' ' + 'VABNCL'
032500081217     C                   ENDIF
032600090312     C* PKB
032700090714     C                   EVAL      PiStr=%trim(%subst(vindta:293:7))
032800090312     C                   EXSR      CHKNUM
032900090312     C                   IF        PiNum=*on
033000090714     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
033100090527     C                   Z-ADD     PiVal         VABPKB
033200090312     C                   ELSE
033300090312     C                   ADD       1             errore
033400090312     C                   EVAL      vinmsg = %trimr(vinmsg)
033500090312     C                             + ' ' + 'VABPKB'
033600090312     C                   ENDIF
033700090527     C* VLB
033800090714     C                   EVAL      PiStr=%trim(%subst(vindta:300:7))
033900090527     C                   EXSR      CHKNUM
034000090527     C                   IF        PiNum=*on
034100090609     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
034200090527     C                   Z-ADD     PiVal         VABVLB
034300090527     C                   ELSE
034400090527     C                   ADD       1             errore
034500090527     C                   EVAL      vinmsg = %trimr(vinmsg)
034600090527     C                             + ' ' + 'VABVLB'
034700090527     C                   ENDIF
034800090527     C* CAS
034900090714     C                   IF        %subst(vindta:311:15) <> *blanks AND
035000090714     C                             %subst(vindta:311:15) <> *zeros
035100090609     C                   MOVEL     '1'           FlgCAS
035200090714     C                   EVAL      VABVCA='EUR'
035300090714     C                   EVAL      PiStr=%trim(%subst(vindta:311:15))
035400090428     C                   EXSR      CHKNUM
035500090428     C                   IF        PiNum=*on
035600090714     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
035700090527     C                   Z-ADD     PiVal         VABCAS
035800090428     C                   ELSE
035900090428     C                   ADD       1             errore
036000090428     C                   EVAL      vinmsg = %trimr(vinmsg)
036100090527     C                             + ' ' + 'VABCAS'
036200090428     C                   ENDIF
036300090428     C                   ENDIF
036400010205     C*
036500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036600040802     C                   IF        FlgCAS <> '0'
036700090609     C                   IF        VABCBO = '1'
036800010205     C                   EVAL      VABCBO = '4'
036900090609     C                   ENDIF
037000090609     C                   IF        VABCBO = '2'
037100090609     C                   EVAL      VABCBO = '6'
037200090609     C                   ENDIF
037300010205     C                   ENDIF
037400020305     C*
037500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037600011113     C                   EXSR      CHKIMPDIV
037700010202     C*
037800000801     C* Ebbene...
037900000801     C                   ADD       1             §CTRMO
038000010201     C                   IF        errore <> *zeros
038100000801     C                   ADD       1             §CTRNO
038200000801     C                   EVAL      vinflg = '2'
038300000801     C                   ELSE
038400010201     C                   ADD       1             §CTROKVB
038500000801     C                   ENDIF
038600000801     C*
038700000801     C                   ENDSR
038800070102     C*----------------------------------------------------*
038900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
039000070102     C*----------------------------------------------------*
039100070103     C     EXEVATE       BEGSR
039200090527     C*
039300090527     C                   EVAL      VATTRC = 'E'
039400090428     C*
039500090609     C***                exsr      wrivat                                       => scarico VAT
039600070102     C*
039700070102     C                   ENDSR
039800080415     C*----------------------------------------------------*
039900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
040000080415     C*----------------------------------------------------*
040100080415     C     EXEVATA       BEGSR
040200080415     C*
040300090714     C***                EVAL      VATTRC = 'A'
040400090714     C***                EVAL      VATNOT = %trim(%subst(vindta:495:22))
040500080415     C*
040600090609     C                   exsr      wrivat                                       => scarico VAT
040700080415     C*
040800080415     C                   ENDSR
040900071121     C*----------------------------------------------------*
041000090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
041100071121     C*----------------------------------------------------*
041200071121     C     EXEVATB       BEGSR
041300071121     C*
041400090504     C                   EVAL      VATTRC = 'B'
041500090714     C                   EVAL      VATNOT = %trim(%subst(vindta:386:20))
041600071121     C*
041700090609     C                   exsr      wrivat                                       => scarico VAT
041800071121     C*
041900071121     C                   ENDSR
042000010201     C*----------------------------------------------------*
042100090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042200010201     C*----------------------------------------------------*
042300020305     C     WRIVAT        BEGSR
042400050628     C*
042500060223     C* Scrivo solo se valorizzato qualcosa
042600060223     C                   IF        VATNOT <> *blanks
042700090114     C                   WRITE     FIVAT000
042800060223     C                   ENDIF
042900010201     C*
043000010201     C                   ENDSR
043100010202     C*----------------------------------------------------*
043200090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043300010202     C*----------------------------------------------------*
043400020305     C     PREVAT        BEGSR
043500010202     C*
043600090114     C* Compongo il nome del membro da dare al FIVATWWR
043700010202     C                   eval      parmbr = vlrhdl
043800010202     C                   movel     'M'           parmbr
043900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
044000010202     C                   eval      paropz = '1'
044100010202     C* Effettuo la chiamata al CLLE preposto
044200090114     C                   call(e)   'TITVVTC'
044300010202     C                   parm                    parccm
044400010202     C                   parm                    parmbr
044500010202     C                   parm                    paropz
044600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044700010202     C                   if        %error
044800010202     C                   movel     '1'           chkcall
044900010202     C                   else
045000010202     C                   movel     '0'           chkcall
045100010202     C                   endif
045200010202     C*
045300010202     C                   ENDSR
045400000801     C*----------------------------------------------------*
045500000801     C*  CONTROLLO NUMERICITA' CAMPI
045600000801     C*----------------------------------------------------*
045700000801     C     CHKNUM        BEGSR
045800081217     C*
045900081217     C                   IF        PiDecChr = *blanks
046000090519     C                   EVAL      PiDecChr = ','
046100081217     C                   ENDIF
046200081217     C*
046300081217     C                   callp(e)  UBISNUM_Check(PiStr
046400081217     C                                          :PiDecChr
046500081217     C                                          :PiVal
046600081217     C                                          :PiNum
046700081217     C                                          :PiInt)
046800081217     C*
046900081217     C                   IF        %error
047000081217     C                   EVAL      PiInt=*off
047100081217     C                   ENDIF
047200000801     C*
047300000801     C                   ENDSR
047400000801     C***
047500000801
047600011113
047700011113     C*----------------------------------------------------*
047800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047900011113     C*----------------------------------------------------*
048000011113     C     CHKIMPDIV     BEGSR
048100011113     C*
048200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048300011113     C                   Z-ADD     *zeros        wrkDec            9 9
048400011113     C*
048500011113     C* Come prima cosa effettuo considerazioni sulla divisa
048600011113     C                   IF        vabIAS > *zeros
048700011113     C                   IF        vabVAS <> 'EUR'
048800011113     C                   EVAL      vabVAS =  'ITL'
048900011113     C                   ENDIF
049000011113     C                   ENDIF
049100011113     C*
049200011113     C                   IF        vabCAS > *zeros
049300011113     C                   IF        vabVCA <> 'EUR'
049400011113     C                   EVAL      vabVCA =  'ITL'
049500011113     C                   ENDIF
049600011113     C                   ENDIF
049700011113     C*
049800011113     C                   IF        vabVMD > *zeros
049900020305     C                   IF        vabVAD <> 'EUR'
050000011113     C                   EVAL      vabVAD =  'ITL'
050100011113     C                   ENDIF
050200011113     C                   ENDIF
050300011113     C*
050400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050500011113     C                   Z-ADD     vabIAS        wrkDec
050600011113     C                   IF        wrkDec > *zeros
050700011113     C                   IF        vabVAS = 'ITL'
050800011113     C                   EVAL      vabIAS = *zeros
050900011113     C                   ENDIF
051000011113     C                   ENDIF
051100011113     C*
051200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051300011113     C                   Z-ADD     vabCAS        wrkDec
051400011113     C                   IF        wrkDec > *zeros
051500011113     C                   IF        vabVCA = 'ITL'
051600011113     C                   EVAL      vabCAS = *zeros
051700011113     C                   ENDIF
051800011113     C                   ENDIF
051900011113     C*
052000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052100011113     C                   Z-ADD     vabVMD        wrkDec
052200011113     C                   IF        wrkDec > *zeros
052300011113     C                   IF        vabVAD = 'ITL'
052400011113     C                   EVAL      vabVMD = *zeros
052500011113     C                   ENDIF
052600011113     C                   ENDIF
052700011113     C*
052800011113     C                   ENDSR
052900011113     C***
053000011113
053100011113
053200000801
053300000801
053400990920      /TITLE Invio dei dati al punto operativo.
053500010202     C     invio         BEGSR
053600990920     C*
053700090114     C* 1° invio FIVAT
053800010201     C                   reset                   dscmz
053900010201     C                   move      vlrpoi        cmzdst
054000090114     C                   eval      cmzfld = 'FIVATWWR'
054100010201     C                   eval      cmzmbd = vlrhdl
054200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054300021009     C***                if        prmfir = *blanks
054400090114     C                   eval      cmzfla = 'FIVAT00F'
054500090114     C                   eval      cmzmba = 'FIVAT00F'
054600021009     C***                else
054700021009     C***                eval      cmzfla = prmfir
054800021009     C***                eval      cmzmba = prmfir
054900021009     C***                endif
055000010201     C                   eval      cmznrr = *zeros
055100020305     C                   move      §ctrokvt      cmznrr
055200021018     C                   eval      cmzlba = vlrfl1
055300010201     C                   call(e)   'TIS711C'
055400010201     C                   parm                    dscmz
055500010201     C                   parm      *blanks       esito
055600010205     C                   if        %error
055700010205     C                             or cmzerr = '1'
055800010205     C                             or esito  = '1'
055900010205     C                   eval      wrkesito = '3'
056000010205     C                   else
056100010201     C*
056200090114     C* 2° invio FIVAB
056300010201     C                   reset                   dscmz
056400010201     C                   move      vlrpoi        cmzdst
056500010201     C                   eval      cmzfld = vlrfou
056600010201     C                   eval      cmzmbd = vlrhdl
056700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056800021009     C***                if        prmfir = *blanks
056900090114     C                   eval      cmzfla = 'FIVAB00F'
057000090114     C                   eval      cmzmba = 'FIVAB00F'
057100021009     C***                else
057200021009     C***                eval      cmzfla = prmfir
057300021009     C***                eval      cmzmba = prmfir
057400021009     C***                endif
057500010201     C                   eval      cmznrr = *zeros
057600010201     C                   move      §ctrokvb      cmznrr
057700021018     C                   eval      cmzlba = vlrfl1
057800010201     C                   call(e)   'TIS711C'
057900010201     C                   parm                    dscmz
058000010201     C                   parm      *blanks       esito
058100010201     C                   if        %error
058200010201     C                             or cmzerr = '1'
058300010201     C                             or esito  = '1'
058400010201     C                   eval      wrkesito = '3'
058500010201     C                   endif
058600010205     C                   endif
058700990920     C*
058800000613     C                   ENDSR
058900000613     C***
059000070411
059100090331
059200090331
059300090331
059400070411     C     *pssr         BEGSR
059500070411     C*
059600070411     C                   if        %open(tivin00r)
059700070411     C                   close     tivin00r
059800070411     C                   endif
059900090114     C                   if        %open(fivabwwr)
060000090114     C                   close     fivabwwr
060100070411     C                   endif
060200090114     C                   if        %open(fivatwwr)
060300090114     C                   close     fivatwwr
060400070411     C                   endif
060500070411     C*
060600070411     C* Effettuo la chiamata al CLLE preposto
060700090114     C                   call(e)   'TITVVTC'
060800070411     C                   parm                    parccm
060900070411     C                   parm                    parmbr
061000070411     C                   parm      '2'           paropz
061100070411     C*
061200070411     C                   eval      wrkesito = '2'
061300070411     C*
061400070411     C                   seton                                        LR
061500070411     C*
061600070411     C                   ENDSR     '*CANCL'
061700070411     C***
061800070411
061900090331
062000090331
062100990910
062200000613     C     *inzsr        BEGSR
062300990910     C*
062400990910     C     *entry        plist
062500990920     C                   parm                    tivlrds
062600990921     C                   parm      wrkesito      esito
062700000724     C                   parm                    prmlit
062800000710     C                   parm                    prmfir
062900000613     C*
063000000830     C* CALCOLA LA DATA CORRENTE
063100000830     C                   time                    wn14             14 0
063200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063300080512     C                   movel     wn14          oracor            6 0          *ORA
063400000830     C                   z-add     wn8           g08dat
063500000830     C                   z-add     *zeros        g08inv
063600000830     C                   movel     '0'           g08err
063700000830     C                   call      'XSRDA8'
063800000830     C                   parm                    wlbda8
063900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064000000830     C*
064100000613     C                   ENDSR
064200000613     C***
