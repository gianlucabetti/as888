000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081118     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
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
002500081218     D curSped         s              8    INZ(*blanks)
002600081218     D depSped         s              8    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600081118
003700081118     D*------------------
003800081118     D* LINKING A DEFINIZIONI ESTERNE
003900081118     D*------------------
004000081118     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004100081218     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004200081118
004300081118
004400010201
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800000613     C*
004900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005000000613     C*
005100010202     C* Effettuo la chiamata al CLLE preposto
005200040506     C                   call(e)   'TITVVTC'
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
006800040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006900910830     C*--------------------------------------------------------
007000040526     C     RWFILE        BEGSR
007100990910     C*
007200990914     C                   if        not %open(tivin00r)
007300990908     C                   open      tivin00r
007400990914     C                   endif
007500021113     C                   if        not %open(fivabwwr)
007600021113     C                   open      fivabwwr
007700990914     C                   endif
007800070103     C*
007900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008000020305     C                   exsr      prevat
008100010201     C*
008200010202     C                   if        chkcall = '0'
008300010202     C*
008400021113     C                   if        not %open(fivatwwr)
008500021113     C                   open      fivatwwr
008600010201     C                   endif
008700990910     C*
008800010201     C                   clear                   §CTROKVB          5 0
008900020305     C                   clear                   §CTROKVT          5 0
009000000801     C                   clear                   §CTRMO            5 0
009100000801     C                   clear                   §CTRNO            5 0
009200990910     C*
009300921023     C                   DO        *HIVAL
009400990913     C*
009500990915     C                   READ      tivin00r                               70
009600050627     C                   if        vindta > *blanks
009700000613     C                   add       1             rrnum
009800000801     C*
009900000801     C                   if        *in70 = *off
010000000801     C                             and
010100000801     C                             (vinflg = *blanks
010200000801     C                              or vinflg = '0'
010300000801     C                              or vinflg = '2')
010400000801     C*
010500000801     C                   clear                   vinmsg
010600000801     C                   eval      vinflg = '1'
010700070103     C*
010800070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010900081218     C                   eval      curSped = %trim(%subst(vindta:9:2)) +
011000081218     C                                       %trim(%subst(vindta:13:6))
011100070213     C                   if        curSped <> depSped
011200070213     C* Se prima bolla => importo bolla corrente
011300070213     C                   if        §CTRMO = *zeros
011400070213     C                   exsr      inzvar
011500070213     C                   exsr      defcam
011600070213     C                   exsr      impvab                                       => carico  VAB
011700070213     C                   else
011800070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
011900070213     C                   exsr      wrivab                                       => scarico VAB
012000070213     C                   exsr      inzvar
012100070213     C                   exsr      defcam
012200070213     C                   exsr      impvab                                       => carico  VAB
012300070213     C                   endif
012400071121     C                   exsr      exevate                                      => write VAT-E
012500070103     C* Salvo il raggruppamento spedizione cliente corrente
012600070103     C                   eval      depSped = curSped
012700070213     C*
012800070213     C* Se collo successivo x stessa bolla
012900070213     C                   else
013000070213     C                   exsr      impvab                                       => carico  VAB
013100070213     C                   exsr      exevate                                      => write VAT-E
013200070103     C                   endif
013300000905     C*
013400000905     C                   else
013500000905     C                   eval      vinflg = '1'
013600050628     C                   endif
013700000905     C                   endif
013800000905     C*
013900000905     C  N70              update    tivin000
014000000905     C*
014100991022     C  N70              ENDdo
014200070213     C*
014300070213     C* Scarico testata bolla rimasta in sospesa
014400070213     C                   exsr      wrivab                                       => scarico VAB
014500010202     C*
014600010202     C                   endif
014700990910
014800990910     C* Se non ci sono record con errori ...
014900000710     C                   if        §ctrno = 0
015000990910     C* ... restituisco esito OK.
015100990921     C                   eval      wrkesito = '0'
015200990910     C                   else
015300010201     C                   if        §ctrokvb > 0
015400990921     C                   eval      wrkesito = '1'
015500000710     C                   else
015600000710     C                   eval      wrkesito = '2'
015700990910     C                   endif
015800000710     C                   endif
015900990910     C*
016000990914     C                   if        %open(tivin00r)
016100990908     C                   close     tivin00r
016200990914     C                   endif
016300021113     C                   if        %open(fivabwwr)
016400021113     C                   close     fivabwwr
016500990914     C                   endif
016600021113     C                   if        %open(fivatwwr)
016700021113     C                   close     fivatwwr
016800010201     C                   endif
016900990910     C*
017000010201     C                   if        §ctrokvb > 0
017100000724     C                             and vlrpoi <> *zeros
017200010202     C                   exsr      invio
017300990920     C                   endif
017400990920     C*
017500910830     C                   ENDSR
017600000613     C***
017700010305
017800010305     C*----------------------------------------------------*
017900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018000010305     C*----------------------------------------------------*
018100020305     C     WRIVAB        BEGSR
018200010305     C*
018300060225     C* Quindi scarico il buffer del file d testata
018400021113     C                   write     fivab000                                     => scarico il VAB
018500010305     C*
018600010305     C                   ENDSR
018700990920
018800000801     C*----------------------------------------------------*
018900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019000000801     C*----------------------------------------------------*
019100010201     C     INZVAR        BEGSR
019200000801     C*
019300070213     C                   CLEAR                   FIVAB000
019400070213     C                   CLEAR                   FIVAT000
019500070213     C*
019600040802     C                   Z-ADD     *zeros        Num5_0            5 0
019700040802     C                   MOVEL     '0'           FlgCAS            1
019800000801     C*
019900000801     C                   ENDSR
020000000801     C*----------------------------------------------------*
020100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020200000801     C*----------------------------------------------------*
020300000801     C     DEFCAM        BEGSR
020400000801     C*
020500020619     C* Imposto i valori di default...
020600081218     C                   Z-ADD     0064052       VABCCM
020700081218     C                   Z-ADD     0064052       VATCCM
020800081218     C                   Z-ADD     006           VABLNP
020900081218     C                   Z-ADD     006           VATLNP
021000081218     C                   Z-ADD     501           VABCTR
021100070103     C                   MOVEL     '7Q'          VABCTM
021200040823     C                   MOVEL     '1'           VABCBO
021300020619     C* ... e poi verifico se sono stati passati come parametri
021400020619     C                   IF        vlrppt > *blanks
021500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021700020619     C                   EXSR      CHKNUM
021800020619     C                   IF        PiInt=*on
021900020619     C                   Z-ADD     PiVal         VABCCM
022000020619     C                   Z-ADD     PiVal         VATCCM
022100020619     C                   ENDIF
022200040506     C                   ENDIF
022300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022500020619     C                   EXSR      CHKNUM
022600020619     C                   IF        PiInt=*on
022700020619     C                   Z-ADD     PiVal         VABLNP
022800020619     C                   Z-ADD     PiVal         VATLNP
022900040506     C                   ENDIF
023000020619     C                   ENDIF
023100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023300020619     C                   EXSR      CHKNUM
023400020619     C                   IF        PiInt=*on
023500020619     C                   Z-ADD     PiVal         VABCTR
023600040506     C                   ENDIF
023700020619     C                   ENDIF
023800020619     C                   ENDIF
023900000801     C*
024000000801     C                   ENDSR
024100000801     C*----------------------------------------------------*
024200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024300000801     C*----------------------------------------------------*
024400040823     C     IMPVAB        BEGSR
024500010305     C*
024600000801     C                   Z-ADD     *zeros        errore            1 0
024700000830     C                   MOVEL     datcor        VABAAS
024800020305     C                   MOVEL     datcor        VATAAS
024900040526     C                   MOVE      datcor        VABMGS
025000040823     C                   MOVE(P)   vlrpoi        VABFGS
025100040823     C                   MOVE(P)   vlrpoi        VATFGS
025200080409     C*
025300080409     C* Reperimento campi ALFA
025400080409     C*
025500080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
025600081218     C                   EVAL      VABRSD=%trim(%subst(vindta:38:30))
025700080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025800080409     C     '@':'A'       XLATE     VABRSD        VABRSD
025900080409     C* ==
026000081218     C                   EVAL      VABRD2=%trim(%subst(vindta:68:20))
026100081218     C                   EVAL      VABIND=%trim(%subst(vindta:88:30))
026200081218     C                   EVAL      VABLOD=%trim(%subst(vindta:123:30))
026300081218     C                   EVAL      VABPRD=%trim(%subst(vindta:153:3))
026400081218     C                   EVAL      VABRMA=%trim(%subst(vindta:6:13))
026500090720     C***                IF        %trim(%subst(vindta:355:4)) = '001' OR
026600090720     C***                          %trim(%subst(vindta:355:4)) = '003'
026700090720     C***                EVAL      VABCBO = '1'
026800090720     C***                ENDIF
026900090720     C***                IF        %trim(%subst(vindta:355:4)) = '002' OR
027000090720     C***                          %trim(%subst(vindta:355:4)) = '006'
027100090720     C***                EVAL      VABCBO = '2'
027200090720     C***                ENDIF
027300090907     C                   IF        %trim(%subst(vindta:251:30))<>*blanks
027400090907     C                   EVAL      VABTC1='A'
027500090907     C                   ENDIF
027600080409     C*
027700080409     C* Reperimento campi NUMERICI
027800081218     C
027900080409     C* NSP/RMN
028000081218     C                   EVAL      PiStr=%trim(%subst(vindta:9:2)) +
028100081218     C                                   %trim(%subst(vindta:13:6))
028200080409     C                   EXSR      CHKNUM
028300080409     C                   IF        PiInt=*on
028400080409     C                   Z-ADD     PiVal         VABNSP
028500080409     C                   Z-ADD     PiVal         VATNSP
028600080409     C                   Z-ADD     PiVal         VABRMN
028700080409     C                   ELSE
028800080409     C                   ADD       1             errore
028900080409     C                   Z-ADD     *zeros        VABNSP
029000080409     C                   Z-ADD     1             VABRMN
029100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029200080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
029300080409     C                   ENDIF
029400080409     C* CAD
029500081218     C                   EVAL      PiStr=%trim(%subst(vindta:118:5))
029600080409     C                   EXSR      CHKNUM
029700080409     C                   IF        PiInt=*on
029800080409     C                   Z-ADD     PiVal         Num5_0
029900080409     C                   MOVEL(p)  Num5_0        VABCAD
030000080409     C                   ELSE
030100080409     C                   ADD       1             errore
030200080409     C                   EVAL      VABCAD = *zeros
030300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
030400080409     C                             + ' ' + 'VABCAD'
030500080409     C                   ENDIF
030600080409     C* NCL
030700081218     C                   EVAL      PiStr=%trim(%subst(vindta:396:5))
030800080409     C                   EXSR      CHKNUM
030900080409     C                   IF        PiInt=*on
031000080409     C                   Z-ADD     PiVal         VABNCL
031100080409     C                   ELSE
031200080409     C                   ADD       1             errore
031300080409     C                   Z-ADD     *zeros        VABNCL
031400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031500080409     C                             + ' ' + 'VABNCL'
031600080409     C                   ENDIF
031700080409     C* PKB
031800081218     C                   EVAL      PiStr=%trim(%subst(vindta:376:10))
031900080409     C                   EXSR      CHKNUM
032000080409     C                   IF        PiNum=*on
032100081218     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
032200081218     C                   Z-ADD(H)  PiVal         VABPKB
032300080409     C                   ELSE
032400080409     C                   ADD       1             errore
032500080409     C                   Z-ADD     *zeros        VABPKB
032600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032700080409     C                             + ' ' + 'VABPKB'
032800080409     C                   ENDIF
032900081218     C* VLB/QFT
033000081218     C                   EVAL      PiStr=%trim(%subst(vindta:386:10))
033100081118     C                   EXSR      CHKNUM
033200081118     C                   IF        PiNum=*on
033300081218     C                   EVAL      PiVal = PiVal / 1000000                      * gestisco 6 dec.
033400081218     C                   Z-ADD(H)  PiVal         VABVLB
033500081218     C                   Z-ADD(H)  PiVal         VABQFT
033600081118     C                   ELSE
033700081118     C                   ADD       1             errore
033800081118     C                   EVAL      vinmsg = %trimr(vinmsg)
033900081218     C                             + ' ' + 'VABVLB VABQFT'
034000081118     C                   ENDIF
034100071121     C*
034200080415     C* Carico l'estensioni A e B del FNVAT
034300080415     C                   exsr      exevata
034400071121     C                   exsr      exevatb
034500010205     C*
034600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034700040802     C                   IF        FlgCAS <> '0'
034800081118     C                   IF        VABCBO = '1'
034900010205     C                   EVAL      VABCBO = '4'
035000010205     C                   ENDIF
035100081118     C                   IF        VABCBO = '2'
035200081118     C                   EVAL      VABCBO = '6'
035300081118     C                   ENDIF
035400081118     C                   ENDIF
035500020305     C*
035600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035700011113     C                   EXSR      CHKIMPDIV
035800010202     C*
035900000801     C* Ebbene...
036000000801     C                   ADD       1             §CTRMO
036100010201     C                   IF        errore <> *zeros
036200000801     C                   ADD       1             §CTRNO
036300000801     C                   EVAL      vinflg = '2'
036400000801     C                   ELSE
036500010201     C                   ADD       1             §CTROKVB
036600000801     C                   ENDIF
036700000801     C*
036800000801     C                   ENDSR
036900070102     C*----------------------------------------------------*
037000070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
037100070102     C*----------------------------------------------------*
037200070103     C     EXEVATE       BEGSR
037300081118     C*
037400070102     C*
037500081118     C***                exsr      wrivat                                       => scarico VAT
037600070102     C*
037700070102     C                   ENDSR
037800080415     C*----------------------------------------------------*
037900080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
038000080415     C*----------------------------------------------------*
038100080415     C     EXEVATA       BEGSR
038200080415     C*
038300090907     C*
038400090907     C                   eval      VATNOT=%trim(%subst(vindta:251:30))
038500090907     C                   eval      VATTRC='A'
038600080415     C*
038700090907     C                   exsr      wrivat                                       => scarico VAT
038800080415     C*
038900080415     C                   ENDSR
039000071121     C*----------------------------------------------------*
039100071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
039200071121     C*----------------------------------------------------*
039300071121     C     EXEVATB       BEGSR
039400071121     C*
039500090907     C                   eval      VATNOT=%trim(%subst(vindta:251:30))
039600090907     C                   eval      VATTRC='B'
039700071121     C*
039800090907     C                   exsr      wrivat                                       => scarico VAT
039900071121     C*
040000071121     C                   ENDSR
040100010201     C*----------------------------------------------------*
040200040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
040300010201     C*----------------------------------------------------*
040400020305     C     WRIVAT        BEGSR
040500081118     C*
040600081118     C* Imposto gli stessi campi chiave della testata relativa
040700081118     C                   EVAL      VATAAS = VABAAS
040800081118     C                   EVAL      VATLNP = VABLNP
040900081118     C                   EVAL      VATNRS = VABNRS
041000081118     C                   EVAL      VATNSP = VABNSP
041100081118     C                   EVAL      VATCCM = VABCCM
041200050628     C*
041300060223     C* Scrivo solo se valorizzato qualcosa
041400060223     C                   IF        VATNOT <> *blanks
041500040802     C                   WRITE     FIVAT000
041600060223     C                   ENDIF
041700010201     C*
041800010201     C                   ENDSR
041900010202     C*----------------------------------------------------*
042000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042100010202     C*----------------------------------------------------*
042200020305     C     PREVAT        BEGSR
042300010202     C*
042400021113     C* Compongo il nome del membro da dare al FIVATWWR
042500010202     C                   eval      parmbr = vlrhdl
042600010202     C                   movel     'M'           parmbr
042700050627     C                   eval      parccm = %subst(vlrKSC:2:7)
042800010202     C                   eval      paropz = '1'
042900010202     C* Effettuo la chiamata al CLLE preposto
043000040506     C                   call(e)   'TITVVTC'
043100010202     C                   parm                    parccm
043200010202     C                   parm                    parmbr
043300010202     C                   parm                    paropz
043400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043500010202     C                   if        %error
043600010202     C                   movel     '1'           chkcall
043700010202     C                   else
043800010202     C                   movel     '0'           chkcall
043900010202     C                   endif
044000010202     C*
044100010202     C                   ENDSR
044200000801     C*----------------------------------------------------*
044300000801     C*  CONTROLLO NUMERICITA' CAMPI
044400000801     C*----------------------------------------------------*
044500000801     C     CHKNUM        BEGSR
044600081118     C*
044700081118     C                   IF        PiDecChr = *blanks
044800081118     C                   EVAL      PiDecChr = ','
044900081118     C                   ENDIF
045000081118     C*
045100081118     C                   callp(e)  UBISNUM_Check(PiStr
045200081118     C                                          :PiDecChr
045300081118     C                                          :PiVal
045400081118     C                                          :PiNum
045500081118     C                                          :PiInt)
045600081118     C*
045700081118     C                   IF        %error
045800081118     C                   EVAL      PiInt=*off
045900081118     C                   ENDIF
046000000801     C*
046100000801     C                   ENDSR
046200000801     C***
046300000801
046400011113
046500011113     C*----------------------------------------------------*
046600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046700011113     C*----------------------------------------------------*
046800011113     C     CHKIMPDIV     BEGSR
046900011113     C*
047000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047100011113     C                   Z-ADD     *zeros        wrkDec            9 9
047200011113     C*
047300011113     C* Come prima cosa effettuo considerazioni sulla divisa
047400011113     C                   IF        vabIAS > *zeros
047500011113     C                   IF        vabVAS <> 'EUR'
047600011113     C                   EVAL      vabVAS =  'ITL'
047700011113     C                   ENDIF
047800011113     C                   ENDIF
047900011113     C*
048000011113     C                   IF        vabCAS > *zeros
048100011113     C                   IF        vabVCA <> 'EUR'
048200011113     C                   EVAL      vabVCA =  'ITL'
048300011113     C                   ENDIF
048400011113     C                   ENDIF
048500011113     C*
048600011113     C                   IF        vabVMD > *zeros
048700020305     C                   IF        vabVAD <> 'EUR'
048800011113     C                   EVAL      vabVAD =  'ITL'
048900011113     C                   ENDIF
049000011113     C                   ENDIF
049100011113     C*
049200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049300011113     C                   Z-ADD     vabIAS        wrkDec
049400011113     C                   IF        wrkDec > *zeros
049500011113     C                   IF        vabVAS = 'ITL'
049600011113     C                   EVAL      vabIAS = *zeros
049700011113     C                   ENDIF
049800011113     C                   ENDIF
049900011113     C*
050000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
050100011113     C                   Z-ADD     vabCAS        wrkDec
050200011113     C                   IF        wrkDec > *zeros
050300011113     C                   IF        vabVCA = 'ITL'
050400011113     C                   EVAL      vabCAS = *zeros
050500011113     C                   ENDIF
050600011113     C                   ENDIF
050700011113     C*
050800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050900011113     C                   Z-ADD     vabVMD        wrkDec
051000011113     C                   IF        wrkDec > *zeros
051100011113     C                   IF        vabVAD = 'ITL'
051200011113     C                   EVAL      vabVMD = *zeros
051300011113     C                   ENDIF
051400011113     C                   ENDIF
051500011113     C*
051600011113     C                   ENDSR
051700011113     C***
051800011113
051900011113
052000000801
052100000801
052200990920      /TITLE Invio dei dati al punto operativo.
052300010202     C     invio         BEGSR
052400990920     C*
052500021113     C* 1° invio FIVAT
052600010201     C                   reset                   dscmz
052700010201     C                   move      vlrpoi        cmzdst
052800021113     C                   eval      cmzfld = 'FIVATWWR'
052900010201     C                   eval      cmzmbd = vlrhdl
053000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053100021009     C***                if        prmfir = *blanks
053200021113     C                   eval      cmzfla = 'FIVAT00F'
053300021113     C                   eval      cmzmba = 'FIVAT00F'
053400021009     C***                else
053500021009     C***                eval      cmzfla = prmfir
053600021009     C***                eval      cmzmba = prmfir
053700021009     C***                endif
053800010201     C                   eval      cmznrr = *zeros
053900020305     C                   move      §ctrokvt      cmznrr
054000021018     C                   eval      cmzlba = vlrfl1
054100010201     C                   call(e)   'TIS711C'
054200010201     C                   parm                    dscmz
054300010201     C                   parm      *blanks       esito
054400010205     C                   if        %error
054500010205     C                             or cmzerr = '1'
054600010205     C                             or esito  = '1'
054700010205     C                   eval      wrkesito = '3'
054800010205     C                   else
054900010201     C*
055000021113     C* 2° invio FIVAB
055100010201     C                   reset                   dscmz
055200010201     C                   move      vlrpoi        cmzdst
055300010201     C                   eval      cmzfld = vlrfou
055400010201     C                   eval      cmzmbd = vlrhdl
055500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055600021009     C***                if        prmfir = *blanks
055700021113     C                   eval      cmzfla = 'FIVAB00F'
055800021113     C                   eval      cmzmba = 'FIVAB00F'
055900021009     C***                else
056000021009     C***                eval      cmzfla = prmfir
056100021009     C***                eval      cmzmba = prmfir
056200021009     C***                endif
056300010201     C                   eval      cmznrr = *zeros
056400010201     C                   move      §ctrokvb      cmznrr
056500021018     C                   eval      cmzlba = vlrfl1
056600010201     C                   call(e)   'TIS711C'
056700010201     C                   parm                    dscmz
056800010201     C                   parm      *blanks       esito
056900010201     C                   if        %error
057000010201     C                             or cmzerr = '1'
057100010201     C                             or esito  = '1'
057200010201     C                   eval      wrkesito = '3'
057300010201     C                   endif
057400010205     C                   endif
057500990920     C*
057600000613     C                   ENDSR
057700000613     C***
057800070411
057900070411     C     *pssr         BEGSR
058000070411     C*
058100070411     C                   if        %open(tivin00r)
058200070411     C                   close     tivin00r
058300070411     C                   endif
058400070411     C                   if        %open(fivabwwr)
058500070411     C                   close     fivabwwr
058600070411     C                   endif
058700070411     C                   if        %open(fivatwwr)
058800070411     C                   close     fivatwwr
058900070411     C                   endif
059000070411     C*
059100070411     C* Effettuo la chiamata al CLLE preposto
059200070411     C                   call(e)   'TITVVTC'
059300070411     C                   parm                    parccm
059400070411     C                   parm                    parmbr
059500070411     C                   parm      '2'           paropz
059600070411     C*
059700070411     C                   eval      wrkesito = '2'
059800070411     C*
059900070411     C                   seton                                        LR
060000070411     C*
060100070411     C                   ENDSR     '*CANCL'
060200070411     C***
060300070411
060400990910
060500000613     C     *inzsr        BEGSR
060600990910     C*
060700990910     C     *entry        plist
060800990920     C                   parm                    tivlrds
060900990921     C                   parm      wrkesito      esito
061000000724     C                   parm                    prmlit
061100000710     C                   parm                    prmfir
061200000613     C*
061300000830     C* CALCOLA LA DATA CORRENTE
061400000830     C                   time                    wn14             14 0
061500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061600000830     C                   z-add     wn8           g08dat
061700000830     C                   z-add     *zeros        g08inv
061800000830     C                   movel     '0'           g08err
061900000830     C                   call      'XSRDA8'
062000000830     C                   parm                    wlbda8
062100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062200000830     C*
062300000613     C                   ENDSR
062400000613     C***
