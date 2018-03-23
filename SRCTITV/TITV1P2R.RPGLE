000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
002500090428     D curSped         s              5    INZ(*blanks)
002600090428     D depSped         s              5    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600090114
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
004700081217     C
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100000613     C*
005200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005300000613     C*
005400010202     C* Effettuo la chiamata al CLLE preposto
005500090114     C                   call(e)   'TITVVTC'
005600010202     C                   parm                    parccm
005700010202     C                   parm                    parmbr
005800010202     C                   parm      '2'           paropz
005900050201     C*
006000050201     C* Effettuo lancio TISI95 solo x chiusura
006100050201     C                   CLEAR                   TISI95DS
006200050201     C                   EVAL      I95TLA = 'C'
006300050201     C                   CALL      'TISI95R'
006400050201     C                   PARM                    TISI95DS
006500000616     C*
006600000801     C
006700010201     C                   seton                                        LR
006800990908
006900000801
007000910830     C*--------------------------------------------------------
007100090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007200910830     C*--------------------------------------------------------
007300040526     C     RWFILE        BEGSR
007400990910     C*
007500990914     C                   if        not %open(tivin00r)
007600990908     C                   open      tivin00r
007700990914     C                   endif
007800090114     C                   if        not %open(fivabwwr)
007900090114     C                   open      fivabwwr
008000990914     C                   endif
008100070103     C*
008200090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008300020305     C                   exsr      prevat
008400010201     C*
008500010202     C                   if        chkcall = '0'
008600010202     C*
008700090114     C                   if        not %open(fivatwwr)
008800090114     C                   open      fivatwwr
008900010201     C                   endif
009000990910     C*
009100010201     C                   clear                   §CTROKVB          5 0
009200020305     C                   clear                   §CTROKVT          5 0
009300000801     C                   clear                   §CTRMO            5 0
009400000801     C                   clear                   §CTRNO            5 0
009500990910     C*
009600921023     C                   DO        *HIVAL
009700990913     C*
009800990915     C                   READ      tivin00r                               70
009900050627     C                   if        vindta > *blanks
010000000613     C                   add       1             rrnum
010100000801     C*
010200000801     C                   if        *in70 = *off
010300000801     C                             and
010400000801     C                             (vinflg = *blanks
010500000801     C                              or vinflg = '0'
010600000801     C                              or vinflg = '2')
010700000801     C*
010800000801     C                   clear                   vinmsg
010900000801     C                   eval      vinflg = '1'
011000070103     C*
011100070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011200090428     C                   eval      curSped = %subst(vindta:109:5)
011300070213     C                   if        curSped <> depSped
011400070213     C* Se prima bolla => importo bolla corrente
011500070213     C                   if        §CTRMO = *zeros
011600090309     C                   EXSR      INZVAR
011700090309     C                   EXSR      DEFCAM
011800070213     C                   exsr      impvab                                       => carico  VAB
011900070213     C                   else
012000070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
012100070213     C                   exsr      wrivab                                       => scarico VAB
012200090309     C                   EXSR      INZVAR
012300090309     C                   EXSR      DEFCAM
012400070213     C                   exsr      impvab                                       => carico  VAB
012500070213     C                   endif
012600071121     C                   exsr      exevate                                      => write VAT-E
012700070103     C* Salvo il raggruppamento spedizione cliente corrente
012800070103     C                   eval      depSped = curSped
012900070213     C*
013000070213     C* Se collo successivo x stessa bolla
013100070213     C                   else
013200070213     C                   exsr      impvab                                       => carico  VAB
013300070213     C                   exsr      exevate                                      => write VAT-E
013400070103     C                   endif
013500000905     C*
013600000905     C                   else
013700000905     C                   eval      vinflg = '1'
013800050628     C                   endif
013900000905     C                   endif
014000000905     C*
014100000905     C  N70              update    tivin000
014200000905     C*
014300991022     C  N70              ENDdo
014400070213     C*
014500070213     C* Scarico testata bolla rimasta in sospesa
014600070213     C                   exsr      wrivab                                       => scarico VAB
014700010202     C*
014800010202     C                   endif
014900990910
015000990910     C* Se non ci sono record con errori ...
015100000710     C                   if        §ctrno = 0
015200990910     C* ... restituisco esito OK.
015300990921     C                   eval      wrkesito = '0'
015400990910     C                   else
015500010201     C                   if        §ctrokvb > 0
015600990921     C                   eval      wrkesito = '1'
015700000710     C                   else
015800000710     C                   eval      wrkesito = '2'
015900990910     C                   endif
016000000710     C                   endif
016100990910     C*
016200990914     C                   if        %open(tivin00r)
016300990908     C                   close     tivin00r
016400990914     C                   endif
016500090114     C                   if        %open(fivabwwr)
016600090114     C                   close     fivabwwr
016700990914     C                   endif
016800090114     C                   if        %open(fivatwwr)
016900090114     C                   close     fivatwwr
017000010201     C                   endif
017100990910     C*
017200010201     C                   if        §ctrokvb > 0
017300000724     C                             and vlrpoi <> *zeros
017400010202     C                   exsr      invio
017500990920     C                   endif
017600990920     C*
017700910830     C                   ENDSR
017800000613     C***
017900010305
018000010305     C*----------------------------------------------------*
018100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018200010305     C*----------------------------------------------------*
018300020305     C     WRIVAB        BEGSR
018400010305     C*
018500060225     C* Quindi scarico il buffer del file d testata
018600090114     C                   write     fivab000                                     => scarico il VAB
018700010305     C*
018800010305     C                   ENDSR
018900990920
019000000801     C*----------------------------------------------------*
019100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019200000801     C*----------------------------------------------------*
019300010201     C     INZVAR        BEGSR
019400000801     C*
019500090114     C                   CLEAR                   FIVAB000
019600090114     C                   CLEAR                   FIVAT000
019700070213     C*
019800040802     C                   Z-ADD     *zeros        Num5_0            5 0
019900040802     C                   MOVEL     '0'           FlgCAS            1
020000000801     C*
020100000801     C                   ENDSR
020200000801     C*----------------------------------------------------*
020300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020400000801     C*----------------------------------------------------*
020500000801     C     DEFCAM        BEGSR
020600000801     C*
020700020619     C* Imposto i valori di default...
020800090428     C                   Z-ADD     0895107       VABCCM
020900090428     C                   Z-ADD     0895107       VATCCM
021000090428     C                   Z-ADD     089           VABLNP
021100090428     C                   Z-ADD     089           VATLNP
021200090428     C                   Z-ADD     052           VABCTR
021300070103     C                   MOVEL     '7Q'          VABCTM
021400040823     C                   MOVEL     '1'           VABCBO
021500020619     C* ... e poi verifico se sono stati passati come parametri
021600020619     C                   IF        vlrppt > *blanks
021700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021900020619     C                   EXSR      CHKNUM
022000020619     C                   IF        PiInt=*on
022100020619     C                   Z-ADD     PiVal         VABCCM
022200020619     C                   Z-ADD     PiVal         VATCCM
022300020619     C                   ENDIF
022400040506     C                   ENDIF
022500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022700020619     C                   EXSR      CHKNUM
022800020619     C                   IF        PiInt=*on
022900020619     C                   Z-ADD     PiVal         VABLNP
023000020619     C                   Z-ADD     PiVal         VATLNP
023100040506     C                   ENDIF
023200020619     C                   ENDIF
023300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023500020619     C                   EXSR      CHKNUM
023600020619     C                   IF        PiInt=*on
023700020619     C                   Z-ADD     PiVal         VABCTR
023800040506     C                   ENDIF
023900020619     C                   ENDIF
024000020619     C                   ENDIF
024100000801     C*
024200000801     C                   ENDSR
024300000801     C*----------------------------------------------------*
024400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024500000801     C*----------------------------------------------------*
024600040823     C     IMPVAB        BEGSR
024700010305     C*
024800000801     C                   Z-ADD     *zeros        errore            1 0
024900000830     C                   MOVEL     datcor        VABAAS
025000020305     C                   MOVEL     datcor        VATAAS
025100040526     C                   MOVE      datcor        VABMGS
025200040823     C                   MOVE(P)   vlrpoi        VABFGS
025300040823     C                   MOVE(P)   vlrpoi        VATFGS
025400080409     C*
025500080409     C* Reperimento campi ALFA
025600080409     C*
025700080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
025800090428     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
025900080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026000080409     C     '@':'A'       XLATE     VABRSD        VABRSD
026100080409     C* ==
026200090428     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
026300090903     C                   EVAL      VABLOD=%trim(%subst(vindta:77:30))
026400090428     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
026500090520     C                   EVAL      VABNZD=%trim(%subst(vindta:195:3))
026600090520     C                   EVAL      VABNOT=%trim(%subst(vindta:160:35))
026700090428     C                   EVAL      VABRMA=%trim(%subst(vindta:109:5))
026800080409     C*
026900080409     C* Reperimento campi NUMERICI
027000090309     C* NSP
027100090428     C                   EVAL      PiStr=%trim(%subst(vindta:109:5))
027200090309     C                   EXSR      CHKNUM
027300090309     C                   IF        PiInt=*on
027400090309     C                   Z-ADD     PiVal         VABNSP
027500090309     C                   Z-ADD     PiVal         VATNSP
027600090309     C                   ELSE
027700090309     C                   EVAL      vinmsg = %trimr(vinmsg)
027800090309     C                             + ' ' + 'VABNSP VATNSP'
027900090309     C                   ENDIF
028000100624     C* CCM
028100100624     C                   EVAL      PiStr=%trim(%subst(vindta:201:7))
028200100624     C                   EXSR      CHKNUM
028300100624     C                   IF        PiInt=*on
028400100624     C                   Z-ADD     PiVal         VABCCM
028500100624     C                   Z-ADD     PiVal         VATCCM
028600100624     C                   ELSE
028700100624     C                   EVAL      vinmsg = %trimr(vinmsg)
028800100624     C                             + ' ' + 'VABCCM VATCCM'
028900100624     C                   ENDIF
029000090309     C* RMN
029100090428     C                   EVAL      PiStr=%trim(%subst(vindta:109:5))
029200081217     C                   EXSR      CHKNUM
029300081217     C                   IF        PiInt=*on
029400081217     C                   Z-ADD     PiVal         VABRMN
029500081217     C                   ELSE
029600081217     C                   ADD       1             errore
029700081217     C                   Z-ADD     1             VABRMN
029800081217     C                   EVAL      vinmsg = %trimr(vinmsg)
029900081217     C                             + ' ' + 'VABRMN'
030000081217     C                   ENDIF
030100080409     C* CAD
030200150130     C                   EVAL      VABCAD=%trim(%subst(vindta:71:5))
030300150130     C***                EVAL      PiStr=%trim(%subst(vindta:71:5))
030400150130     C***                EXSR      CHKNUM
030500150130     C***                IF        PiInt=*on
030600150130     C***                Z-ADD     PiVal         Num5_0
030700150130     C***                MOVEL(p)  Num5_0        VABCAD
030800150130     C***                ELSE
030900150130     C***                ADD       1             errore
031000150130     C***                EVAL      VABCAD = *zeros
031100150130     C***                EVAL      vinmsg = %trimr(vinmsg)
031200150130     C***                          + ' ' + 'VABCAD'
031300150130     C***                ENDIF
031400081217     C* NCL
031500090428     C                   EVAL      PiStr=%trim(%subst(vindta:129:6))
031600081217     C                   EXSR      CHKNUM
031700081217     C                   IF        PiInt=*on
031800081217     C                   Z-ADD     PiVal         VABNCL
031900081217     C                   ELSE
032000090309     C                   ADD       1             errore
032100081217     C                   EVAL      vinmsg = %trimr(vinmsg)
032200081217     C                             + ' ' + 'VABNCL'
032300081217     C                   ENDIF
032400090312     C* PKB
032500090428     C                   EVAL      PiStr=%trim(%subst(vindta:136:9))
032600090312     C                   EXSR      CHKNUM
032700090312     C                   IF        PiNum=*on
032800090428     C                   Z-ADD     PiVal         VABPKB
032900090312     C                   ELSE
033000090312     C                   ADD       1             errore
033100090312     C                   EVAL      vinmsg = %trimr(vinmsg)
033200090312     C                             + ' ' + 'VABPKB'
033300090312     C                   ENDIF
033400090428     C* CAS
033500090903     C                   IF        %subst(vindta:148:12) <> *blanks AND
033600090903     C                             %subst(vindta:148:12) <> *zeros  AND
033700090903     C                             %subst(vindta:148:12) <> '000000000.00'
033800090428     C                   EVAL      FlgCAS = '1'
033900090428     C                   EVAL      VABVCA = 'EUR'
034000090903     C                   EVAL      PiStr=%trim(%subst(vindta:148:12))
034100090428     C                   EXSR      CHKNUM
034200090428     C                   IF        PiNum=*on
034300090428     C                   Z-ADD     PiVal         VABCAS
034400090428     C                   ELSE
034500090428     C                   ADD       1             errore
034600090428     C                   EVAL      vinmsg = %trimr(vinmsg)
034700090428     C                             + ' ' + 'VABCAS'
034800090428     C                   ENDIF
034900090428     C                   ENDIF
035000090428     C* CTR
035100090520     C                   IF        %subst(vindta:198:3) <> *blanks
035200090520     C                   EVAL      PiStr=%trim(%subst(vindta:198:3))
035300090428     C                   EXSR      CHKNUM
035400090428     C                   IF        PiInt=*on
035500090428     C                   Z-ADD     PiVal         VABCTR
035600090428     C                   ELSE
035700090428     C                   ADD       1             errore
035800090428     C                   EVAL      vinmsg = %trimr(vinmsg)
035900090428     C                             + ' ' + 'VABCTR'
036000090428     C                   ENDIF
036100090428     C                   ENDIF
036200090428     C*
036300090428     C                   IF        VABCTR = 353
036400090428     C                   EVAL      VABCTM = '7R'
036500090428     C                   ENDIF
036600071121     C*
036700080415     C* Carico l'estensioni A e B del FNVAT
036800080512     C                   exsr      exevata
036900080512     C                   exsr      exevatb
037000010205     C*
037100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
037200040802     C                   IF        FlgCAS <> '0'
037300010205     C                   EVAL      VABCBO = '4'
037400010205     C                   ENDIF
037500020305     C*
037600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037700011113     C                   EXSR      CHKIMPDIV
037800010202     C*
037900000801     C* Ebbene...
038000000801     C                   ADD       1             §CTRMO
038100010201     C                   IF        errore <> *zeros
038200000801     C                   ADD       1             §CTRNO
038300000801     C                   EVAL      vinflg = '2'
038400000801     C                   ELSE
038500010201     C                   ADD       1             §CTROKVB
038600000801     C                   ENDIF
038700000801     C*
038800000801     C                   ENDSR
038900070102     C*----------------------------------------------------*
039000090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
039100070102     C*----------------------------------------------------*
039200070103     C     EXEVATE       BEGSR
039300070102     C*
039400090428     C                   EVAL      VATTRC = 'E'
039500090612     C                   eval      VATNOT = %trim(%subst(vindta:114:12))
039600090428     C*
039700090428     C                   exsr      wrivat                                       => scarico VAT
039800070102     C*
039900070102     C                   ENDSR
040000080415     C*----------------------------------------------------*
040100090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
040200080415     C*----------------------------------------------------*
040300080415     C     EXEVATA       BEGSR
040400080415     C*
040500080415     C*
040600080415     C***                exsr      wrivat                                       => scarico VAT
040700080415     C*
040800080415     C                   ENDSR
040900071121     C*----------------------------------------------------*
041000090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
041100071121     C*----------------------------------------------------*
041200071121     C     EXEVATB       BEGSR
041300071121     C*
041400071121     C*
041500080409     C***                exsr      wrivat                                       => scarico VAT
041600071121     C*
041700071121     C                   ENDSR
041800010201     C*----------------------------------------------------*
041900090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042000010201     C*----------------------------------------------------*
042100020305     C     WRIVAT        BEGSR
042200050628     C*
042300060223     C* Scrivo solo se valorizzato qualcosa
042400060223     C                   IF        VATNOT <> *blanks
042500090114     C                   WRITE     FIVAT000
042600060223     C                   ENDIF
042700010201     C*
042800010201     C                   ENDSR
042900010202     C*----------------------------------------------------*
043000090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043100010202     C*----------------------------------------------------*
043200020305     C     PREVAT        BEGSR
043300010202     C*
043400090114     C* Compongo il nome del membro da dare al FIVATWWR
043500010202     C                   eval      parmbr = vlrhdl
043600010202     C                   movel     'M'           parmbr
043700050627     C                   eval      parccm = %subst(vlrKSC:2:7)
043800010202     C                   eval      paropz = '1'
043900010202     C* Effettuo la chiamata al CLLE preposto
044000090114     C                   call(e)   'TITVVTC'
044100010202     C                   parm                    parccm
044200010202     C                   parm                    parmbr
044300010202     C                   parm                    paropz
044400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044500010202     C                   if        %error
044600010202     C                   movel     '1'           chkcall
044700010202     C                   else
044800010202     C                   movel     '0'           chkcall
044900010202     C                   endif
045000010202     C*
045100010202     C                   ENDSR
045200000801     C*----------------------------------------------------*
045300000801     C*  CONTROLLO NUMERICITA' CAMPI
045400000801     C*----------------------------------------------------*
045500000801     C     CHKNUM        BEGSR
045600081217     C*
045700081217     C                   IF        PiDecChr = *blanks
045800090505     C                   EVAL      PiDecChr = '.'
045900081217     C                   ENDIF
046000081217     C*
046100081217     C                   callp(e)  UBISNUM_Check(PiStr
046200081217     C                                          :PiDecChr
046300081217     C                                          :PiVal
046400081217     C                                          :PiNum
046500081217     C                                          :PiInt)
046600081217     C*
046700081217     C                   IF        %error
046800081217     C                   EVAL      PiInt=*off
046900081217     C                   ENDIF
047000000801     C*
047100000801     C                   ENDSR
047200000801     C***
047300000801
047400011113
047500011113     C*----------------------------------------------------*
047600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047700011113     C*----------------------------------------------------*
047800011113     C     CHKIMPDIV     BEGSR
047900011113     C*
048000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048100011113     C                   Z-ADD     *zeros        wrkDec            9 9
048200011113     C*
048300011113     C* Come prima cosa effettuo considerazioni sulla divisa
048400011113     C                   IF        vabIAS > *zeros
048500011113     C                   IF        vabVAS <> 'EUR'
048600011113     C                   EVAL      vabVAS =  'ITL'
048700011113     C                   ENDIF
048800011113     C                   ENDIF
048900011113     C*
049000011113     C                   IF        vabCAS > *zeros
049100011113     C                   IF        vabVCA <> 'EUR'
049200011113     C                   EVAL      vabVCA =  'ITL'
049300011113     C                   ENDIF
049400011113     C                   ENDIF
049500011113     C*
049600011113     C                   IF        vabVMD > *zeros
049700020305     C                   IF        vabVAD <> 'EUR'
049800011113     C                   EVAL      vabVAD =  'ITL'
049900011113     C                   ENDIF
050000011113     C                   ENDIF
050100011113     C*
050200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050300011113     C                   Z-ADD     vabIAS        wrkDec
050400011113     C                   IF        wrkDec > *zeros
050500011113     C                   IF        vabVAS = 'ITL'
050600011113     C                   EVAL      vabIAS = *zeros
050700011113     C                   ENDIF
050800011113     C                   ENDIF
050900011113     C*
051000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051100011113     C                   Z-ADD     vabCAS        wrkDec
051200011113     C                   IF        wrkDec > *zeros
051300011113     C                   IF        vabVCA = 'ITL'
051400011113     C                   EVAL      vabCAS = *zeros
051500011113     C                   ENDIF
051600011113     C                   ENDIF
051700011113     C*
051800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051900011113     C                   Z-ADD     vabVMD        wrkDec
052000011113     C                   IF        wrkDec > *zeros
052100011113     C                   IF        vabVAD = 'ITL'
052200011113     C                   EVAL      vabVMD = *zeros
052300011113     C                   ENDIF
052400011113     C                   ENDIF
052500011113     C*
052600011113     C                   ENDSR
052700011113     C***
052800011113
052900011113
053000000801
053100000801
053200990920      /TITLE Invio dei dati al punto operativo.
053300010202     C     invio         BEGSR
053400990920     C*
053500090114     C* 1° invio FIVAT
053600010201     C                   reset                   dscmz
053700010201     C                   move      vlrpoi        cmzdst
053800090114     C                   eval      cmzfld = 'FIVATWWR'
053900010201     C                   eval      cmzmbd = vlrhdl
054000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054100021009     C***                if        prmfir = *blanks
054200090114     C                   eval      cmzfla = 'FIVAT00F'
054300090114     C                   eval      cmzmba = 'FIVAT00F'
054400021009     C***                else
054500021009     C***                eval      cmzfla = prmfir
054600021009     C***                eval      cmzmba = prmfir
054700021009     C***                endif
054800010201     C                   eval      cmznrr = *zeros
054900020305     C                   move      §ctrokvt      cmznrr
055000021018     C                   eval      cmzlba = vlrfl1
055100010201     C                   call(e)   'TIS711C'
055200010201     C                   parm                    dscmz
055300010201     C                   parm      *blanks       esito
055400010205     C                   if        %error
055500010205     C                             or cmzerr = '1'
055600010205     C                             or esito  = '1'
055700010205     C                   eval      wrkesito = '3'
055800010205     C                   else
055900010201     C*
056000090114     C* 2° invio FIVAB
056100010201     C                   reset                   dscmz
056200010201     C                   move      vlrpoi        cmzdst
056300010201     C                   eval      cmzfld = vlrfou
056400010201     C                   eval      cmzmbd = vlrhdl
056500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056600021009     C***                if        prmfir = *blanks
056700090114     C                   eval      cmzfla = 'FIVAB00F'
056800090114     C                   eval      cmzmba = 'FIVAB00F'
056900021009     C***                else
057000021009     C***                eval      cmzfla = prmfir
057100021009     C***                eval      cmzmba = prmfir
057200021009     C***                endif
057300010201     C                   eval      cmznrr = *zeros
057400010201     C                   move      §ctrokvb      cmznrr
057500021018     C                   eval      cmzlba = vlrfl1
057600010201     C                   call(e)   'TIS711C'
057700010201     C                   parm                    dscmz
057800010201     C                   parm      *blanks       esito
057900010201     C                   if        %error
058000010201     C                             or cmzerr = '1'
058100010201     C                             or esito  = '1'
058200010201     C                   eval      wrkesito = '3'
058300010201     C                   endif
058400010205     C                   endif
058500990920     C*
058600000613     C                   ENDSR
058700000613     C***
058800070411
058900090331
059000090331
059100090331
059200070411     C     *pssr         BEGSR
059300070411     C*
059400070411     C                   if        %open(tivin00r)
059500070411     C                   close     tivin00r
059600070411     C                   endif
059700090114     C                   if        %open(fivabwwr)
059800090114     C                   close     fivabwwr
059900070411     C                   endif
060000090114     C                   if        %open(fivatwwr)
060100090114     C                   close     fivatwwr
060200070411     C                   endif
060300070411     C*
060400070411     C* Effettuo la chiamata al CLLE preposto
060500090114     C                   call(e)   'TITVVTC'
060600070411     C                   parm                    parccm
060700070411     C                   parm                    parmbr
060800070411     C                   parm      '2'           paropz
060900070411     C*
061000070411     C                   eval      wrkesito = '2'
061100070411     C*
061200070411     C                   seton                                        LR
061300070411     C*
061400070411     C                   ENDSR     '*CANCL'
061500070411     C***
061600070411
061700090331
061800090331
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
062900000830     C                   time                    wn14             14 0
063000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063100080512     C                   movel     wn14          oracor            6 0          *ORA
063200000830     C                   z-add     wn8           g08dat
063300000830     C                   z-add     *zeros        g08inv
063400000830     C                   movel     '0'           g08err
063500000830     C                   call      'XSRDA8'
063600000830     C                   parm                    wlbda8
063700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063800000830     C*
063900000613     C                   ENDSR
