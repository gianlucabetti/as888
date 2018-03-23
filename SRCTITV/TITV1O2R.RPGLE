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
002500090331     D curSped         s              6    INZ(*blanks)
002600090331     D depSped         s              6    INZ(*blanks)
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
006500010202     C*
006600000801     C
006700010201     C                   seton                                        LR
006800000801
006900910830     C*--------------------------------------------------------
007000090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007100910830     C*--------------------------------------------------------
007200040526     C     RWFILE        BEGSR
007300990910     C*
007400990914     C                   if        not %open(tivin00r)
007500990908     C                   open      tivin00r
007600990914     C                   endif
007700090114     C                   if        not %open(fivabwwr)
007800090114     C                   open      fivabwwr
007900990914     C                   endif
008000070103     C*
008100090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008200020305     C                   exsr      prevat
008300010201     C*
008400010202     C                   if        chkcall = '0'
008500010202     C*
008600090114     C                   if        not %open(fivatwwr)
008700090114     C                   open      fivatwwr
008800010201     C                   endif
008900990910     C*
009000010201     C                   clear                   §CTROKVB          5 0
009100020305     C                   clear                   §CTROKVT          5 0
009200000801     C                   clear                   §CTRMO            5 0
009300000801     C                   clear                   §CTRNO            5 0
009400990910     C*
009500921023     C                   DO        *HIVAL
009600990913     C*
009700990915     C                   READ      tivin00r                               70
009800050627     C                   if        vindta > *blanks
009900000613     C                   add       1             rrnum
010000000801     C*
010100000801     C                   if        *in70 = *off
010200000801     C                             and
010300000801     C                             (vinflg = *blanks
010400000801     C                              or vinflg = '0'
010500000801     C                              or vinflg = '2')
010600000801     C*
010700000801     C                   clear                   vinmsg
010800000801     C                   eval      vinflg = '1'
010900070103     C*
011000070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011100090331     C                   eval      curSped = %subst(vindta:1:6)
011200070213     C                   if        curSped <> depSped
011300070213     C* Se prima bolla => importo bolla corrente
011400070213     C                   if        §CTRMO = *zeros
011500090309     C                   EXSR      INZVAR
011600090309     C                   EXSR      DEFCAM
011700070213     C                   exsr      impvab                                       => carico  VAB
011800070213     C                   else
011900070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
012000070213     C                   exsr      wrivab                                       => scarico VAB
012100090309     C                   EXSR      INZVAR
012200090309     C                   EXSR      DEFCAM
012300070213     C                   exsr      impvab                                       => carico  VAB
012400070213     C                   endif
012500071121     C                   exsr      exevate                                      => write VAT-E
012600070103     C* Salvo il raggruppamento spedizione cliente corrente
012700070103     C                   eval      depSped = curSped
012800070213     C*
012900070213     C* Se collo successivo x stessa bolla
013000070213     C                   else
013100070213     C                   exsr      impvab                                       => carico  VAB
013200070213     C                   exsr      exevate                                      => write VAT-E
013300070103     C                   endif
013400000905     C*
013500000905     C                   else
013600000905     C                   eval      vinflg = '1'
013700050628     C                   endif
013800000905     C                   endif
013900000905     C*
014000000905     C  N70              update    tivin000
014100000905     C*
014200991022     C  N70              ENDdo
014300070213     C*
014400070213     C* Scarico testata bolla rimasta in sospesa
014500070213     C                   exsr      wrivab                                       => scarico VAB
014600010202     C*
014700010202     C                   endif
014800990910
014900990910     C* Se non ci sono record con errori ...
015000000710     C                   if        §ctrno = 0
015100990910     C* ... restituisco esito OK.
015200990921     C                   eval      wrkesito = '0'
015300990910     C                   else
015400010201     C                   if        §ctrokvb > 0
015500990921     C                   eval      wrkesito = '1'
015600000710     C                   else
015700000710     C                   eval      wrkesito = '2'
015800990910     C                   endif
015900000710     C                   endif
016000990910     C*
016100990914     C                   if        %open(tivin00r)
016200990908     C                   close     tivin00r
016300990914     C                   endif
016400090114     C                   if        %open(fivabwwr)
016500090114     C                   close     fivabwwr
016600990914     C                   endif
016700090114     C                   if        %open(fivatwwr)
016800090114     C                   close     fivatwwr
016900010201     C                   endif
017000990910     C*
017100010201     C                   if        §ctrokvb > 0
017200000724     C                             and vlrpoi <> *zeros
017300010202     C                   exsr      invio
017400990920     C                   endif
017500990920     C*
017600910830     C                   ENDSR
017700000613     C***
017800010305
017900010305     C*----------------------------------------------------*
018000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018100010305     C*----------------------------------------------------*
018200020305     C     WRIVAB        BEGSR
018300010305     C*
018400060225     C* Quindi scarico il buffer del file d testata
018500090114     C                   write     fivab000                                     => scarico il VAB
018600010305     C*
018700010305     C                   ENDSR
018800990920
018900000801     C*----------------------------------------------------*
019000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019100000801     C*----------------------------------------------------*
019200010201     C     INZVAR        BEGSR
019300000801     C*
019400090114     C                   CLEAR                   FIVAB000
019500090114     C                   CLEAR                   FIVAT000
019600070213     C*
019700040802     C                   Z-ADD     *zeros        Num5_0            5 0
019800040802     C                   MOVEL     '0'           FlgCAS            1
019900000801     C*
020000000801     C                   ENDSR
020100000801     C*----------------------------------------------------*
020200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020300000801     C*----------------------------------------------------*
020400000801     C     DEFCAM        BEGSR
020500000801     C*
020600020619     C* Imposto i valori di default...
020700090331     C                   Z-ADD     2493971       VABCCM
020800090331     C                   Z-ADD     2493971       VATCCM
020900090331     C                   Z-ADD     005           VABLNP
021000090331     C                   Z-ADD     005           VATLNP
021100081217     C                   Z-ADD     000           VABCTR
021200070103     C                   MOVEL     '7Q'          VABCTM
021300040823     C                   MOVEL     '1'           VABCBO
021400020619     C* ... e poi verifico se sono stati passati come parametri
021500020619     C                   IF        vlrppt > *blanks
021600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021800020619     C                   EXSR      CHKNUM
021900020619     C                   IF        PiInt=*on
022000020619     C                   Z-ADD     PiVal         VABCCM
022100020619     C                   Z-ADD     PiVal         VATCCM
022200020619     C                   ENDIF
022300040506     C                   ENDIF
022400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022600020619     C                   EXSR      CHKNUM
022700020619     C                   IF        PiInt=*on
022800020619     C                   Z-ADD     PiVal         VABLNP
022900020619     C                   Z-ADD     PiVal         VATLNP
023000040506     C                   ENDIF
023100020619     C                   ENDIF
023200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023400020619     C                   EXSR      CHKNUM
023500020619     C                   IF        PiInt=*on
023600020619     C                   Z-ADD     PiVal         VABCTR
023700040506     C                   ENDIF
023800020619     C                   ENDIF
023900020619     C                   ENDIF
024000000801     C*
024100000801     C                   ENDSR
024200000801     C*----------------------------------------------------*
024300090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024400000801     C*----------------------------------------------------*
024500040823     C     IMPVAB        BEGSR
024600010305     C*
024700000801     C                   Z-ADD     *zeros        errore            1 0
024800000830     C                   MOVEL     datcor        VABAAS
024900020305     C                   MOVEL     datcor        VATAAS
025000040526     C                   MOVE      datcor        VABMGS
025100040823     C                   MOVE(P)   vlrpoi        VABFGS
025200040823     C                   MOVE(P)   vlrpoi        VATFGS
025300080409     C*
025400080409     C* Reperimento campi ALFA
025500080409     C*
025600080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
025700090331     C                   EVAL      VABRSD=%trim(%subst(vindta:13:35))
025800080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025900080409     C     '@':'A'       XLATE     VABRSD        VABRSD
026000080409     C* ==
026100090331     C                   EVAL      VABRD2=%trim(%subst(vindta:13+35:40-35))
026200090331     C                   EVAL      VABIND=%trim(%subst(vindta:53:30))
026300090331     C                   EVAL      VABLOD=%trim(%subst(vindta:88:28))
026400090331     C                   EVAL      VABPRD=%trim(%subst(vindta:116:2))
026500090331     C                   EVAL      VABRMA=%trim(%subst(vindta:1:6))
026600080409     C*
026700080409     C* Reperimento campi NUMERICI
026800090309     C* NSP
026900090331     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
027000090309     C                   EXSR      CHKNUM
027100090309     C                   IF        PiInt=*on
027200090309     C                   Z-ADD     PiVal         VABNSP
027300090309     C                   Z-ADD     PiVal         VATNSP
027400090309     C                   ELSE
027500090309     C                   EVAL      vinmsg = %trimr(vinmsg)
027600090309     C                             + ' ' + 'VABNSP VATNSP'
027700090309     C                   ENDIF
027800090309     C* RMN
027900090331     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
028000081217     C                   EXSR      CHKNUM
028100081217     C                   IF        PiInt=*on
028200081217     C                   Z-ADD     PiVal         VABRMN
028300081217     C                   ELSE
028400081217     C                   ADD       1             errore
028500081217     C                   Z-ADD     1             VABRMN
028600081217     C                   EVAL      vinmsg = %trimr(vinmsg)
028700081217     C                             + ' ' + 'VABRMN'
028800081217     C                   ENDIF
028900080409     C* CAD
029000090331     C                   EVAL      PiStr=%trim(%subst(vindta:83:5))
029100090114     C                   EXSR      CHKNUM
029200090114     C                   IF        PiInt=*on
029300090114     C                   Z-ADD     PiVal         Num5_0
029400090114     C                   MOVEL(p)  Num5_0        VABCAD
029500090114     C                   ELSE
029600090114     C                   ADD       1             errore
029700090114     C                   EVAL      VABCAD = *zeros
029800090114     C                   EVAL      vinmsg = %trimr(vinmsg)
029900090114     C                             + ' ' + 'VABCAD'
030000090114     C                   ENDIF
030100081217     C* NCL
030200090331     C                   EVAL      PiStr=%trim(%subst(vindta:124:16))
030300081217     C                   EXSR      CHKNUM
030400081217     C                   IF        PiInt=*on
030500081217     C                   Z-ADD     PiVal         VABNCL
030600081217     C                   ELSE
030700090309     C                   ADD       1             errore
030800081217     C                   EVAL      vinmsg = %trimr(vinmsg)
030900081217     C                             + ' ' + 'VABNCL'
031000081217     C                   ENDIF
031100090312     C* PKB
031200090331     C                   EVAL      PiStr=%trim(%subst(vindta:118:6))
031300090312     C                   EXSR      CHKNUM
031400090312     C                   IF        PiNum=*on
031500090331     C                   EVAL      PiVal=PiVal / 10                             * gestisco 1 dec.
031600090312     C                   Z-ADD(H)  PiVal         VABPKB
031700090312     C                   ELSE
031800090312     C                   ADD       1             errore
031900090312     C                   EVAL      vinmsg = %trimr(vinmsg)
032000090312     C                             + ' ' + 'VABPKB'
032100090312     C                   ENDIF
032200071121     C*
032300080415     C* Carico l'estensioni A e B del FNVAT
032400080512     C                   exsr      exevata
032500080512     C                   exsr      exevatb
032600010205     C*
032700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
032800040802     C                   IF        FlgCAS <> '0'
032900010205     C                   EVAL      VABCBO = '4'
033000010205     C                   ENDIF
033100020305     C*
033200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
033300011113     C                   EXSR      CHKIMPDIV
033400010202     C*
033500000801     C* Ebbene...
033600000801     C                   ADD       1             §CTRMO
033700010201     C                   IF        errore <> *zeros
033800000801     C                   ADD       1             §CTRNO
033900000801     C                   EVAL      vinflg = '2'
034000000801     C                   ELSE
034100010201     C                   ADD       1             §CTROKVB
034200000801     C                   ENDIF
034300000801     C*
034400000801     C                   ENDSR
034500070102     C*----------------------------------------------------*
034600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
034700070102     C*----------------------------------------------------*
034800070103     C     EXEVATE       BEGSR
034900070102     C*
035000070103     C                   EVAL      VATTRC='E'
035100090312     C*
035200090312     C* X ogni collo indicato in testata
035300121221     C                   z-add     1             wColli            5 0
035400090312     C*
035500090312     C                   dow       wColli <= VABNCL
035600090312     C*
035700090312     C* Valorizzo il buffer di scrittura del FIVAT
035800090331     C                   move(p)   wColli        wColliAlfa        4
035900090331     C                   eval      VATNOT = '0319'+%trim(%subst(vindta:7:2))+
036000090331     C                                             %trim(%subst(vindta:1:6))+
036100090331     C                                             wColliAlfa+'A'
036200090312     C                   exsr      wrivat                                       => scarico VAT
036300090312     C*
036400090312     C                   add       1             wColli
036500090312     C                   enddo
036600070102     C*
036700070102     C                   ENDSR
036800080415     C*----------------------------------------------------*
036900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
037000080415     C*----------------------------------------------------*
037100080415     C     EXEVATA       BEGSR
037200080415     C*
037300080415     C*
037400080415     C***                exsr      wrivat                                       => scarico VAT
037500080415     C*
037600080415     C                   ENDSR
037700071121     C*----------------------------------------------------*
037800090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
037900071121     C*----------------------------------------------------*
038000071121     C     EXEVATB       BEGSR
038100071121     C*
038200071121     C*
038300080409     C***                exsr      wrivat                                       => scarico VAT
038400071121     C*
038500071121     C                   ENDSR
038600010201     C*----------------------------------------------------*
038700090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
038800010201     C*----------------------------------------------------*
038900020305     C     WRIVAT        BEGSR
039000050628     C*
039100060223     C* Scrivo solo se valorizzato qualcosa
039200060223     C                   IF        VATNOT <> *blanks
039300090114     C                   WRITE     FIVAT000
039400060223     C                   ENDIF
039500010201     C*
039600010201     C                   ENDSR
039700010202     C*----------------------------------------------------*
039800090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
039900010202     C*----------------------------------------------------*
040000020305     C     PREVAT        BEGSR
040100010202     C*
040200090114     C* Compongo il nome del membro da dare al FIVATWWR
040300010202     C                   eval      parmbr = vlrhdl
040400010202     C                   movel     'M'           parmbr
040500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
040600010202     C                   eval      paropz = '1'
040700010202     C* Effettuo la chiamata al CLLE preposto
040800090114     C                   call(e)   'TITVVTC'
040900010202     C                   parm                    parccm
041000010202     C                   parm                    parmbr
041100010202     C                   parm                    paropz
041200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041300010202     C                   if        %error
041400010202     C                   movel     '1'           chkcall
041500010202     C                   else
041600010202     C                   movel     '0'           chkcall
041700010202     C                   endif
041800010202     C*
041900010202     C                   ENDSR
042000000801     C*----------------------------------------------------*
042100000801     C*  CONTROLLO NUMERICITA' CAMPI
042200000801     C*----------------------------------------------------*
042300000801     C     CHKNUM        BEGSR
042400081217     C*
042500081217     C                   IF        PiDecChr = *blanks
042600090312     C                   EVAL      PiDecChr = ','
042700081217     C                   ENDIF
042800081217     C*
042900081217     C                   callp(e)  UBISNUM_Check(PiStr
043000081217     C                                          :PiDecChr
043100081217     C                                          :PiVal
043200081217     C                                          :PiNum
043300081217     C                                          :PiInt)
043400081217     C*
043500081217     C                   IF        %error
043600081217     C                   EVAL      PiInt=*off
043700081217     C                   ENDIF
043800000801     C*
043900000801     C                   ENDSR
044000000801     C***
044100000801
044200011113
044300011113     C*----------------------------------------------------*
044400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044500011113     C*----------------------------------------------------*
044600011113     C     CHKIMPDIV     BEGSR
044700011113     C*
044800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044900011113     C                   Z-ADD     *zeros        wrkDec            9 9
045000011113     C*
045100011113     C* Come prima cosa effettuo considerazioni sulla divisa
045200011113     C                   IF        vabIAS > *zeros
045300011113     C                   IF        vabVAS <> 'EUR'
045400011113     C                   EVAL      vabVAS =  'ITL'
045500011113     C                   ENDIF
045600011113     C                   ENDIF
045700011113     C*
045800011113     C                   IF        vabCAS > *zeros
045900011113     C                   IF        vabVCA <> 'EUR'
046000011113     C                   EVAL      vabVCA =  'ITL'
046100011113     C                   ENDIF
046200011113     C                   ENDIF
046300011113     C*
046400011113     C                   IF        vabVMD > *zeros
046500020305     C                   IF        vabVAD <> 'EUR'
046600011113     C                   EVAL      vabVAD =  'ITL'
046700011113     C                   ENDIF
046800011113     C                   ENDIF
046900011113     C*
047000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047100011113     C                   Z-ADD     vabIAS        wrkDec
047200011113     C                   IF        wrkDec > *zeros
047300011113     C                   IF        vabVAS = 'ITL'
047400011113     C                   EVAL      vabIAS = *zeros
047500011113     C                   ENDIF
047600011113     C                   ENDIF
047700011113     C*
047800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
047900011113     C                   Z-ADD     vabCAS        wrkDec
048000011113     C                   IF        wrkDec > *zeros
048100011113     C                   IF        vabVCA = 'ITL'
048200011113     C                   EVAL      vabCAS = *zeros
048300011113     C                   ENDIF
048400011113     C                   ENDIF
048500011113     C*
048600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048700011113     C                   Z-ADD     vabVMD        wrkDec
048800011113     C                   IF        wrkDec > *zeros
048900011113     C                   IF        vabVAD = 'ITL'
049000011113     C                   EVAL      vabVMD = *zeros
049100011113     C                   ENDIF
049200011113     C                   ENDIF
049300011113     C*
049400011113     C                   ENDSR
049500011113     C***
049600011113
049700011113
049800000801
049900000801
050000990920      /TITLE Invio dei dati al punto operativo.
050100010202     C     invio         BEGSR
050200990920     C*
050300090114     C* 1° invio FIVAT
050400010201     C                   reset                   dscmz
050500010201     C                   move      vlrpoi        cmzdst
050600090114     C                   eval      cmzfld = 'FIVATWWR'
050700010201     C                   eval      cmzmbd = vlrhdl
050800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
050900021009     C***                if        prmfir = *blanks
051000090114     C                   eval      cmzfla = 'FIVAT00F'
051100090114     C                   eval      cmzmba = 'FIVAT00F'
051200021009     C***                else
051300021009     C***                eval      cmzfla = prmfir
051400021009     C***                eval      cmzmba = prmfir
051500021009     C***                endif
051600010201     C                   eval      cmznrr = *zeros
051700020305     C                   move      §ctrokvt      cmznrr
051800021018     C                   eval      cmzlba = vlrfl1
051900010201     C                   call(e)   'TIS711C'
052000010201     C                   parm                    dscmz
052100010201     C                   parm      *blanks       esito
052200010205     C                   if        %error
052300010205     C                             or cmzerr = '1'
052400010205     C                             or esito  = '1'
052500010205     C                   eval      wrkesito = '3'
052600010205     C                   else
052700010201     C*
052800090114     C* 2° invio FIVAB
052900010201     C                   reset                   dscmz
053000010201     C                   move      vlrpoi        cmzdst
053100010201     C                   eval      cmzfld = vlrfou
053200010201     C                   eval      cmzmbd = vlrhdl
053300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053400021009     C***                if        prmfir = *blanks
053500090114     C                   eval      cmzfla = 'FIVAB00F'
053600090114     C                   eval      cmzmba = 'FIVAB00F'
053700021009     C***                else
053800021009     C***                eval      cmzfla = prmfir
053900021009     C***                eval      cmzmba = prmfir
054000021009     C***                endif
054100010201     C                   eval      cmznrr = *zeros
054200010201     C                   move      §ctrokvb      cmznrr
054300021018     C                   eval      cmzlba = vlrfl1
054400010201     C                   call(e)   'TIS711C'
054500010201     C                   parm                    dscmz
054600010201     C                   parm      *blanks       esito
054700010201     C                   if        %error
054800010201     C                             or cmzerr = '1'
054900010201     C                             or esito  = '1'
055000010201     C                   eval      wrkesito = '3'
055100010201     C                   endif
055200010205     C                   endif
055300990920     C*
055400000613     C                   ENDSR
055500000613     C***
055600070411
055700090331
055800090331
055900090331
056000070411     C     *pssr         BEGSR
056100070411     C*
056200070411     C                   if        %open(tivin00r)
056300070411     C                   close     tivin00r
056400070411     C                   endif
056500090114     C                   if        %open(fivabwwr)
056600090114     C                   close     fivabwwr
056700070411     C                   endif
056800090114     C                   if        %open(fivatwwr)
056900090114     C                   close     fivatwwr
057000070411     C                   endif
057100070411     C*
057200070411     C* Effettuo la chiamata al CLLE preposto
057300090114     C                   call(e)   'TITVVTC'
057400070411     C                   parm                    parccm
057500070411     C                   parm                    parmbr
057600070411     C                   parm      '2'           paropz
057700070411     C*
057800070411     C                   eval      wrkesito = '2'
057900070411     C*
058000070411     C                   seton                                        LR
058100070411     C*
058200070411     C                   ENDSR     '*CANCL'
058300070411     C***
058400070411
058500090331
058600090331
058700990910
058800000613     C     *inzsr        BEGSR
058900990910     C*
059000990910     C     *entry        plist
059100990920     C                   parm                    tivlrds
059200990921     C                   parm      wrkesito      esito
059300000724     C                   parm                    prmlit
059400000710     C                   parm                    prmfir
059500000613     C*
059600000830     C* CALCOLA LA DATA CORRENTE
059700000830     C                   time                    wn14             14 0
059800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059900080512     C                   movel     wn14          oracor            6 0          *ORA
060000000830     C                   z-add     wn8           g08dat
060100000830     C                   z-add     *zeros        g08inv
060200000830     C                   movel     '0'           g08err
060300000830     C                   call      'XSRDA8'
060400000830     C                   parm                    wlbda8
060500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060600000830     C*
060700000613     C                   ENDSR
060800000613     C***
