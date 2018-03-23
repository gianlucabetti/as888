000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130115     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
011200090527     C* Se trattasi d record "chi sono"...
011300090527     C                   if        %subst(vindta:1:5) = 'C7411'
011400090527     C                   exsr      exevate                                      => write VAT-E
011500090527     C                   endif
011600090527     C*
011700090527     C* Se trattasi d record testata....
011800090527     C                   if        %subst(vindta:1:5) = *blanks
011900070213     C                   exsr      impvab                                       => carico  VAB
012000090527     C                   exsr      wrivab                                       => scarico VAB
012100090527     C                   endif
012200000905     C*
012300000905     C                   else
012400000905     C                   eval      vinflg = '1'
012500050628     C                   endif
012600000905     C                   endif
012700000905     C*
012800000905     C  N70              update    tivin000
012900000905     C*
013000991022     C  N70              ENDdo
013100010202     C*
013200010202     C                   endif
013300990910
013400990910     C* Se non ci sono record con errori ...
013500000710     C                   if        §ctrno = 0
013600990910     C* ... restituisco esito OK.
013700990921     C                   eval      wrkesito = '0'
013800990910     C                   else
013900010201     C                   if        §ctrokvb > 0
014000990921     C                   eval      wrkesito = '1'
014100000710     C                   else
014200000710     C                   eval      wrkesito = '2'
014300990910     C                   endif
014400000710     C                   endif
014500990910     C*
014600990914     C                   if        %open(tivin00r)
014700990908     C                   close     tivin00r
014800990914     C                   endif
014900090114     C                   if        %open(fivabwwr)
015000090114     C                   close     fivabwwr
015100990914     C                   endif
015200090114     C                   if        %open(fivatwwr)
015300090114     C                   close     fivatwwr
015400010201     C                   endif
015500990910     C*
015600010201     C                   if        §ctrokvb > 0
015700000724     C                             and vlrpoi <> *zeros
015800010202     C                   exsr      invio
015900990920     C                   endif
016000990920     C*
016100910830     C                   ENDSR
016200000613     C***
016300010305
016400010305     C*----------------------------------------------------*
016500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
016600010305     C*----------------------------------------------------*
016700020305     C     WRIVAB        BEGSR
016800010305     C*
016900060225     C* Quindi scarico il buffer del file d testata
017000090114     C                   write     fivab000                                     => scarico il VAB
017100010305     C*
017200010305     C                   ENDSR
017300990920
017400000801     C*----------------------------------------------------*
017500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017600000801     C*----------------------------------------------------*
017700010201     C     INZVAR        BEGSR
017800000801     C*
017900090114     C                   CLEAR                   FIVAB000
018000090114     C                   CLEAR                   FIVAT000
018100070213     C*
018200040802     C                   Z-ADD     *zeros        Num5_0            5 0
018300040802     C                   MOVEL     '0'           FlgCAS            1
018400000801     C*
018500000801     C                   ENDSR
018600000801     C*----------------------------------------------------*
018700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
018800000801     C*----------------------------------------------------*
018900000801     C     DEFCAM        BEGSR
019000000801     C*
019100020619     C* Imposto i valori di default...
019200090527     C                   MOVEL     datcor        VABAAS
019300090527     C                   MOVEL     datcor        VATAAS
019400090527     C                   MOVE(P)   vlrpoi        VABFGS
019500090527     C                   MOVE(P)   vlrpoi        VATFGS
019600090527     C*
019700090527     C                   Z-ADD     2220563       VABCCM
019800090527     C                   Z-ADD     2220563       VATCCM
019900090527     C                   Z-ADD     222           VABLNP
020000090527     C                   Z-ADD     222           VATLNP
020100090519     C                   Z-ADD     000           VABCTR
020200040823     C                   MOVEL     '1'           VABCBO
020300090527     C                   MOVEL     '7Q'          VABCTM
020400020619     C* ... e poi verifico se sono stati passati come parametri
020500020619     C                   IF        vlrppt > *blanks
020600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020800020619     C                   EXSR      CHKNUM
020900020619     C                   IF        PiInt=*on
021000020619     C                   Z-ADD     PiVal         VABCCM
021100020619     C                   Z-ADD     PiVal         VATCCM
021200020619     C                   ENDIF
021300040506     C                   ENDIF
021400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABLNP
021900020619     C                   Z-ADD     PiVal         VATLNP
022000040506     C                   ENDIF
022100020619     C                   ENDIF
022200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022400020619     C                   EXSR      CHKNUM
022500020619     C                   IF        PiInt=*on
022600020619     C                   Z-ADD     PiVal         VABCTR
022700040506     C                   ENDIF
022800020619     C                   ENDIF
022900020619     C                   ENDIF
023000000801     C*
023100000801     C                   ENDSR
023200000801     C*----------------------------------------------------*
023300090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023400000801     C*----------------------------------------------------*
023500040823     C     IMPVAB        BEGSR
023600010305     C*
023700000801     C                   Z-ADD     *zeros        errore            1 0
023800040526     C                   MOVE      datcor        VABMGS
023900080409     C*
024000080409     C* Reperimento campi ALFA
024100080409     C*
024200080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
024300090527     C                   EVAL      VABRSD=%trim(%subst(vindta:51:30))
024400080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024500080409     C     '@':'A'       XLATE     VABRSD        VABRSD
024600080409     C* ==
024700090527     C                   EVAL      VABIND=%trim(%subst(vindta:81:30))
024800090527     C                   EVAL      VABLOD=%trim(%subst(vindta:111:30))
024900090527     C                   EVAL      VABPRD=%trim(%subst(vindta:141:2))
025000090527     C                   EVAL      VABRMA=%trim(%subst(vindta:30:3))+
025100090527     C                                    %trim(%subst(vindta:33:1))+
025200090527     C                                    %trim(%subst(vindta:34:8))
025300080409     C*
025400080409     C* Reperimento campi NUMERICI
025500090309     C* NSP
025600090527     C                   EVAL      PiStr=%trim(%subst(vindta:34:8))
025700090309     C                   EXSR      CHKNUM
025800090309     C                   IF        PiInt=*on
025900090309     C                   Z-ADD     PiVal         VABNSP
026000090309     C                   Z-ADD     PiVal         VATNSP
026100090309     C                   ELSE
026200090309     C                   EVAL      vinmsg = %trimr(vinmsg)
026300090309     C                             + ' ' + 'VABNSP VATNSP'
026400090309     C                   ENDIF
026500090309     C* RMN
026600090527     C                   EVAL      PiStr=%trim(%subst(vindta:34:8))
026700081217     C                   EXSR      CHKNUM
026800081217     C                   IF        PiInt=*on
026900081217     C                   Z-ADD     PiVal         VABRMN
027000081217     C                   ELSE
027100081217     C                   ADD       1             errore
027200081217     C                   Z-ADD     1             VABRMN
027300081217     C                   EVAL      vinmsg = %trimr(vinmsg)
027400081217     C                             + ' ' + 'VABRMN'
027500081217     C                   ENDIF
027600080409     C* CAD
027700090527     C                   EVAL      PiStr=%trim(%subst(vindta:143:5))
027800090114     C                   EXSR      CHKNUM
027900090114     C                   IF        PiInt=*on
028000090114     C                   Z-ADD     PiVal         Num5_0
028100090114     C                   MOVEL(p)  Num5_0        VABCAD
028200090114     C                   ELSE
028300090114     C                   ADD       1             errore
028400090114     C                   EVAL      VABCAD = *zeros
028500090114     C                   EVAL      vinmsg = %trimr(vinmsg)
028600090114     C                             + ' ' + 'VABCAD'
028700090114     C                   ENDIF
028800081217     C* NCL
028900090527     C                   EVAL      PiStr=%trim(%subst(vindta:148:5))
029000081217     C                   EXSR      CHKNUM
029100081217     C                   IF        PiInt=*on
029200081217     C                   Z-ADD     PiVal         VABNCL
029300081217     C                   ELSE
029400090309     C                   ADD       1             errore
029500081217     C                   EVAL      vinmsg = %trimr(vinmsg)
029600081217     C                             + ' ' + 'VABNCL'
029700081217     C                   ENDIF
029800090312     C* PKB
029900090527     C                   EVAL      PiStr=%trim(%subst(vindta:153:7))
030000090312     C                   EXSR      CHKNUM
030100090312     C                   IF        PiNum=*on
030200090527     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
030300090527     C                   Z-ADD     PiVal         VABPKB
030400090312     C                   ELSE
030500090312     C                   ADD       1             errore
030600090312     C                   EVAL      vinmsg = %trimr(vinmsg)
030700090312     C                             + ' ' + 'VABPKB'
030800090312     C                   ENDIF
030900090527     C* VLB
031000090527     C                   EVAL      PiStr=%trim(%subst(vindta:160:7))
031100090527     C                   EXSR      CHKNUM
031200090527     C                   IF        PiNum=*on
031300090527     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
031400090527     C                   Z-ADD     PiVal         VABVLB
031500090527     C                   ELSE
031600090527     C                   ADD       1             errore
031700090527     C                   EVAL      vinmsg = %trimr(vinmsg)
031800090527     C                             + ' ' + 'VABVLB'
031900090527     C                   ENDIF
032000090527     C* CAS
032100090527     C                   IF        %subst(vindta:187:9) <> *blanks AND
032200090527     C                             %subst(vindta:187:9) <> *zeros
032300090527     C                   EVAL      VABVCA = 'EUR'
032400090527     C                   EVAL      PiStr=%trim(%subst(vindta:187:9))
032500090428     C                   EXSR      CHKNUM
032600090428     C                   IF        PiNum=*on
032700090527     C                   Z-ADD     PiVal         VABCAS
032800090428     C                   ELSE
032900090428     C                   ADD       1             errore
033000090428     C                   EVAL      vinmsg = %trimr(vinmsg)
033100090527     C                             + ' ' + 'VABCAS'
033200090428     C                   ENDIF
033300090428     C                   ENDIF
033400010205     C*
033500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
033600040802     C                   IF        FlgCAS <> '0'
033700010205     C                   EVAL      VABCBO = '4'
033800010205     C                   ENDIF
033900020305     C*
034000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
034100011113     C                   EXSR      CHKIMPDIV
034200010202     C*
034300000801     C* Ebbene...
034400000801     C                   ADD       1             §CTRMO
034500010201     C                   IF        errore <> *zeros
034600000801     C                   ADD       1             §CTRNO
034700000801     C                   EVAL      vinflg = '2'
034800000801     C                   ELSE
034900010201     C                   ADD       1             §CTROKVB
035000000801     C                   ENDIF
035100000801     C*
035200000801     C                   ENDSR
035300070102     C*----------------------------------------------------*
035400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
035500070102     C*----------------------------------------------------*
035600070103     C     EXEVATE       BEGSR
035700090527     C*
035800090527     C                   EVAL      VATTRC = 'E'
035900090527     C* NCL
036000090527     C                   EVAL      PiStr=%trim(%subst(vindta:9:8))
036100090527     C                   EXSR      CHKNUM
036200090527     C                   IF        PiInt=*on
036300090527     C                   Z-ADD     PiVal         VATNSP
036400090527     C                   EVAL      VATNOT = %trim(%subst(vindta:21:32))
036500090527     C                   ELSE
036600090527     C                   ADD       1             errore
036700090527     C                   EVAL      vinmsg = %trimr(vinmsg)
036800090527     C                             + ' ' + 'VATNSP'
036900090527     C                   ENDIF
037000090428     C*
037100090527     C                   exsr      wrivat                                       => scarico VAT
037200070102     C*
037300070102     C                   ENDSR
037400080415     C*----------------------------------------------------*
037500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
037600080415     C*----------------------------------------------------*
037700080415     C     EXEVATA       BEGSR
037800080415     C*
037900090504     C                   EVAL      VATTRC = 'A'
038000080415     C*
038100080415     C***                exsr      wrivat                                       => scarico VAT
038200080415     C*
038300080415     C                   ENDSR
038400071121     C*----------------------------------------------------*
038500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
038600071121     C*----------------------------------------------------*
038700071121     C     EXEVATB       BEGSR
038800071121     C*
038900090504     C                   EVAL      VATTRC = 'B'
039000071121     C*
039100080409     C***                exsr      wrivat                                       => scarico VAT
039200071121     C*
039300071121     C                   ENDSR
039400010201     C*----------------------------------------------------*
039500090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
039600010201     C*----------------------------------------------------*
039700020305     C     WRIVAT        BEGSR
039800050628     C*
039900060223     C* Scrivo solo se valorizzato qualcosa
040000060223     C                   IF        VATNOT <> *blanks
040100090114     C                   WRITE     FIVAT000
040200060223     C                   ENDIF
040300010201     C*
040400010201     C                   ENDSR
040500010202     C*----------------------------------------------------*
040600090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040700010202     C*----------------------------------------------------*
040800020305     C     PREVAT        BEGSR
040900010202     C*
041000090114     C* Compongo il nome del membro da dare al FIVATWWR
041100010202     C                   eval      parmbr = vlrhdl
041200010202     C                   movel     'M'           parmbr
041300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
041400010202     C                   eval      paropz = '1'
041500010202     C* Effettuo la chiamata al CLLE preposto
041600090114     C                   call(e)   'TITVVTC'
041700010202     C                   parm                    parccm
041800010202     C                   parm                    parmbr
041900010202     C                   parm                    paropz
042000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042100010202     C                   if        %error
042200010202     C                   movel     '1'           chkcall
042300010202     C                   else
042400010202     C                   movel     '0'           chkcall
042500010202     C                   endif
042600010202     C*
042700010202     C                   ENDSR
042800000801     C*----------------------------------------------------*
042900000801     C*  CONTROLLO NUMERICITA' CAMPI
043000000801     C*----------------------------------------------------*
043100000801     C     CHKNUM        BEGSR
043200081217     C*
043300081217     C                   IF        PiDecChr = *blanks
043400090519     C                   EVAL      PiDecChr = ','
043500081217     C                   ENDIF
043600081217     C*
043700081217     C                   callp(e)  UBISNUM_Check(PiStr
043800081217     C                                          :PiDecChr
043900081217     C                                          :PiVal
044000081217     C                                          :PiNum
044100081217     C                                          :PiInt)
044200081217     C*
044300081217     C                   IF        %error
044400081217     C                   EVAL      PiInt=*off
044500081217     C                   ENDIF
044600000801     C*
044700000801     C                   ENDSR
044800000801     C***
044900000801
045000011113
045100011113     C*----------------------------------------------------*
045200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045300011113     C*----------------------------------------------------*
045400011113     C     CHKIMPDIV     BEGSR
045500011113     C*
045600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045700011113     C                   Z-ADD     *zeros        wrkDec            9 9
045800011113     C*
045900011113     C* Come prima cosa effettuo considerazioni sulla divisa
046000011113     C                   IF        vabIAS > *zeros
046100011113     C                   IF        vabVAS <> 'EUR'
046200011113     C                   EVAL      vabVAS =  'ITL'
046300011113     C                   ENDIF
046400011113     C                   ENDIF
046500011113     C*
046600011113     C                   IF        vabCAS > *zeros
046700011113     C                   IF        vabVCA <> 'EUR'
046800011113     C                   EVAL      vabVCA =  'ITL'
046900011113     C                   ENDIF
047000011113     C                   ENDIF
047100011113     C*
047200011113     C                   IF        vabVMD > *zeros
047300020305     C                   IF        vabVAD <> 'EUR'
047400011113     C                   EVAL      vabVAD =  'ITL'
047500011113     C                   ENDIF
047600011113     C                   ENDIF
047700011113     C*
047800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047900011113     C                   Z-ADD     vabIAS        wrkDec
048000011113     C                   IF        wrkDec > *zeros
048100011113     C                   IF        vabVAS = 'ITL'
048200011113     C                   EVAL      vabIAS = *zeros
048300011113     C                   ENDIF
048400011113     C                   ENDIF
048500011113     C*
048600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
048700011113     C                   Z-ADD     vabCAS        wrkDec
048800011113     C                   IF        wrkDec > *zeros
048900011113     C                   IF        vabVCA = 'ITL'
049000011113     C                   EVAL      vabCAS = *zeros
049100011113     C                   ENDIF
049200011113     C                   ENDIF
049300011113     C*
049400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049500011113     C                   Z-ADD     vabVMD        wrkDec
049600011113     C                   IF        wrkDec > *zeros
049700011113     C                   IF        vabVAD = 'ITL'
049800011113     C                   EVAL      vabVMD = *zeros
049900011113     C                   ENDIF
050000011113     C                   ENDIF
050100011113     C*
050200011113     C                   ENDSR
050300011113     C***
050400011113
050500011113
050600000801
050700000801
050800990920      /TITLE Invio dei dati al punto operativo.
050900010202     C     invio         BEGSR
051000990920     C*
051100090114     C* 1° invio FIVAT
051200010201     C                   reset                   dscmz
051300010201     C                   move      vlrpoi        cmzdst
051400090114     C                   eval      cmzfld = 'FIVATWWR'
051500010201     C                   eval      cmzmbd = vlrhdl
051600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
051700021009     C***                if        prmfir = *blanks
051800090114     C                   eval      cmzfla = 'FIVAT00F'
051900090114     C                   eval      cmzmba = 'FIVAT00F'
052000021009     C***                else
052100021009     C***                eval      cmzfla = prmfir
052200021009     C***                eval      cmzmba = prmfir
052300021009     C***                endif
052400010201     C                   eval      cmznrr = *zeros
052500020305     C                   move      §ctrokvt      cmznrr
052600021018     C                   eval      cmzlba = vlrfl1
052700010201     C                   call(e)   'TIS711C'
052800010201     C                   parm                    dscmz
052900010201     C                   parm      *blanks       esito
053000010205     C                   if        %error
053100010205     C                             or cmzerr = '1'
053200010205     C                             or esito  = '1'
053300010205     C                   eval      wrkesito = '3'
053400010205     C                   else
053500010201     C*
053600090114     C* 2° invio FIVAB
053700010201     C                   reset                   dscmz
053800010201     C                   move      vlrpoi        cmzdst
053900010201     C                   eval      cmzfld = vlrfou
054000010201     C                   eval      cmzmbd = vlrhdl
054100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054200021009     C***                if        prmfir = *blanks
054300090114     C                   eval      cmzfla = 'FIVAB00F'
054400090114     C                   eval      cmzmba = 'FIVAB00F'
054500021009     C***                else
054600021009     C***                eval      cmzfla = prmfir
054700021009     C***                eval      cmzmba = prmfir
054800021009     C***                endif
054900010201     C                   eval      cmznrr = *zeros
055000010201     C                   move      §ctrokvb      cmznrr
055100021018     C                   eval      cmzlba = vlrfl1
055200010201     C                   call(e)   'TIS711C'
055300010201     C                   parm                    dscmz
055400010201     C                   parm      *blanks       esito
055500010201     C                   if        %error
055600010201     C                             or cmzerr = '1'
055700010201     C                             or esito  = '1'
055800010201     C                   eval      wrkesito = '3'
055900010201     C                   endif
056000010205     C                   endif
056100990920     C*
056200000613     C                   ENDSR
056300000613     C***
056400070411
056500090331
056600090331
056700090331
056800070411     C     *pssr         BEGSR
056900070411     C*
057000070411     C                   if        %open(tivin00r)
057100070411     C                   close     tivin00r
057200070411     C                   endif
057300090114     C                   if        %open(fivabwwr)
057400090114     C                   close     fivabwwr
057500070411     C                   endif
057600090114     C                   if        %open(fivatwwr)
057700090114     C                   close     fivatwwr
057800070411     C                   endif
057900070411     C*
058000070411     C* Effettuo la chiamata al CLLE preposto
058100090114     C                   call(e)   'TITVVTC'
058200070411     C                   parm                    parccm
058300070411     C                   parm                    parmbr
058400070411     C                   parm      '2'           paropz
058500070411     C*
058600070411     C                   eval      wrkesito = '2'
058700070411     C*
058800070411     C                   seton                                        LR
058900070411     C*
059000070411     C                   ENDSR     '*CANCL'
059100070411     C***
059200070411
059300090331
059400090331
059500990910
059600000613     C     *inzsr        BEGSR
059700990910     C*
059800990910     C     *entry        plist
059900990920     C                   parm                    tivlrds
060000990921     C                   parm      wrkesito      esito
060100000724     C                   parm                    prmlit
060200000710     C                   parm                    prmfir
060300000613     C*
060400000830     C* CALCOLA LA DATA CORRENTE
060500000830     C                   time                    wn14             14 0
060600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060700080512     C                   movel     wn14          oracor            6 0          *ORA
060800000830     C                   z-add     wn8           g08dat
060900000830     C                   z-add     *zeros        g08inv
061000000830     C                   movel     '0'           g08err
061100000830     C                   call      'XSRDA8'
061200000830     C                   parm                    wlbda8
061300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061400000830     C*
061500000613     C                   ENDSR
061600000613     C***
