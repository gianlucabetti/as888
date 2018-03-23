000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200090113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
002500090223     D curSped         s             13    INZ(*blanks)
002600090223     D depSped         s             13    INZ(*blanks)
002700110705
002800110705     D*------------------
002900110705     D* DS RIDEFINIZIONE BARCODE CLIENTE
003000110705     D*------------------
003100110705     D DS_BARCODE      DS                  INZ
003200110705     D  DS_B_PAR01                   12
003300110705     D  DS_B_PAR02                    4  0
003400110705     D  DS_B_PAR03                    4  0
003500090113
003600090113     D*------------------
003700090113     D* LINKING A DEFINIZIONI ESTERNE
003800090113     D*------------------
003900090113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004000090113     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004100090113
004200010201
004300000913     C                   reset                   rrnum
004400990921     C                   reset                   esito
004500990921     C                   reset                   wrkesito
004600000613     C*
004700040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004800000613     C*
004900010202     C* Effettuo la chiamata al CLLE preposto
005000040506     C                   call(e)   'TITVVTC'
005100010202     C                   parm                    parccm
005200010202     C                   parm                    parmbr
005300010202     C                   parm      '2'           paropz
005400050201     C*
005500050201     C* Effettuo lancio TISI95 solo x chiusura
005600050201     C                   CLEAR                   TISI95DS
005700050201     C                   EVAL      I95TLA = 'C'
005800050201     C                   CALL      'TISI95R'
005900050201     C                   PARM                    TISI95DS
006000000616     C*
006100000801     C
006200010201     C                   seton                                        LR
006300990908
006400000801
006500910830     C*--------------------------------------------------------
006600040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006700910830     C*--------------------------------------------------------
006800040526     C     RWFILE        BEGSR
006900990910     C*
007000990914     C                   if        not %open(tivin00r)
007100990908     C                   open      tivin00r
007200990914     C                   endif
007300021113     C                   if        not %open(fivabwwr)
007400021113     C                   open      fivabwwr
007500990914     C                   endif
007600070103     C*
007700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007800020305     C                   exsr      prevat
007900010201     C*
008000010202     C                   if        chkcall = '0'
008100010202     C*
008200021113     C                   if        not %open(fivatwwr)
008300021113     C                   open      fivatwwr
008400010201     C                   endif
008500990910     C*
008600010201     C                   clear                   §CTROKVB          5 0
008700020305     C                   clear                   §CTROKVT          5 0
008800000801     C                   clear                   §CTRMO            5 0
008900000801     C                   clear                   §CTRNO            5 0
009000990910     C*
009100921023     C                   DO        *HIVAL
009200990913     C*
009300990915     C                   READ      tivin00r                               70
009400050627     C                   if        vindta > *blanks
009500000613     C                   add       1             rrnum
009600000801     C*
009700000801     C                   if        *in70 = *off
009800000801     C                             and
009900000801     C                             (vinflg = *blanks
010000000801     C                              or vinflg = '0'
010100000801     C                              or vinflg = '2')
010200000801     C*
010300000801     C                   clear                   vinmsg
010400000801     C                   eval      vinflg = '1'
010500070103     C*
010600070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010700090223     C                   eval      curSped = %subst(vindta:1:13)
010800070213     C                   if        curSped <> depSped
010900070213     C* Se prima bolla => importo bolla corrente
011000070213     C                   if        §CTRMO = *zeros
011100070213     C                   exsr      inzvar
011200070213     C                   exsr      defcam
011300070213     C                   exsr      impvab                                       => carico  VAB
011400070213     C                   else
011500070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
011600070213     C                   exsr      wrivab                                       => scarico VAB
011700070213     C                   exsr      inzvar
011800070213     C                   exsr      defcam
011900070213     C                   exsr      impvab                                       => carico  VAB
012000070213     C                   endif
012100071121     C                   exsr      exevate                                      => write VAT-E
012200070103     C* Salvo il raggruppamento spedizione cliente corrente
012300070103     C                   eval      depSped = curSped
012400070213     C*
012500070213     C* Se collo successivo x stessa bolla
012600070213     C                   else
012700070213     C                   exsr      impvab                                       => carico  VAB
012800070213     C                   exsr      exevate                                      => write VAT-E
012900070103     C                   endif
013000000905     C*
013100000905     C                   else
013200000905     C                   eval      vinflg = '1'
013300050628     C                   endif
013400000905     C                   endif
013500000905     C*
013600000905     C  N70              update    tivin000
013700000905     C*
013800991022     C  N70              ENDdo
013900070213     C*
014000070213     C* Scarico testata bolla rimasta in sospesa
014100070213     C                   exsr      wrivab                                       => scarico VAB
014200010202     C*
014300010202     C                   endif
014400990910
014500990910     C* Se non ci sono record con errori ...
014600000710     C                   if        §ctrno = 0
014700990910     C* ... restituisco esito OK.
014800990921     C                   eval      wrkesito = '0'
014900990910     C                   else
015000010201     C                   if        §ctrokvb > 0
015100990921     C                   eval      wrkesito = '1'
015200000710     C                   else
015300000710     C                   eval      wrkesito = '2'
015400990910     C                   endif
015500000710     C                   endif
015600990910     C*
015700990914     C                   if        %open(tivin00r)
015800990908     C                   close     tivin00r
015900990914     C                   endif
016000021113     C                   if        %open(fivabwwr)
016100021113     C                   close     fivabwwr
016200990914     C                   endif
016300021113     C                   if        %open(fivatwwr)
016400021113     C                   close     fivatwwr
016500010201     C                   endif
016600990910     C*
016700010201     C                   if        §ctrokvb > 0
016800000724     C                             and vlrpoi <> *zeros
016900010202     C                   exsr      invio
017000990920     C                   endif
017100990920     C*
017200910830     C                   ENDSR
017300000613     C***
017400010305
017500010305     C*----------------------------------------------------*
017600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017700010305     C*----------------------------------------------------*
017800020305     C     WRIVAB        BEGSR
017900010305     C*
018000060225     C* Quindi scarico il buffer del file d testata
018100021113     C                   write     fivab000                                     => scarico il VAB
018200010305     C*
018300010305     C                   ENDSR
018400990920
018500000801     C*----------------------------------------------------*
018600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018700000801     C*----------------------------------------------------*
018800010201     C     INZVAR        BEGSR
018900000801     C*
019000070213     C                   CLEAR                   FIVAB000
019100070213     C                   CLEAR                   FIVAT000
019200070213     C*
019300040802     C                   Z-ADD     *zeros        Num5_0            5 0
019400040802     C                   MOVEL     '0'           FlgCAS            1
019500000801     C*
019600000801     C                   ENDSR
019700000801     C*----------------------------------------------------*
019800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019900000801     C*----------------------------------------------------*
020000000801     C     DEFCAM        BEGSR
020100000801     C*
020200020619     C* Imposto i valori di default...
020300090223     C                   Z-ADD     0500821       VABCCM
020400090223     C                   Z-ADD     0500821       VATCCM
020500090223     C                   Z-ADD     050           VABLNP
020600090223     C                   Z-ADD     050           VATLNP
020700090113     C                   Z-ADD     000           VABCTR
020800040823     C                   MOVEL     '1'           VABCBO
020900110705     C                   MOVEL     '7Q'          VABCTM
021000020619     C* ... e poi verifico se sono stati passati come parametri
021100020619     C                   IF        vlrppt > *blanks
021200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021400020619     C                   EXSR      CHKNUM
021500020619     C                   IF        PiInt=*on
021600020619     C                   Z-ADD     PiVal         VABCCM
021700020619     C                   Z-ADD     PiVal         VATCCM
021800020619     C                   ENDIF
021900040506     C                   ENDIF
022000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022200020619     C                   EXSR      CHKNUM
022300020619     C                   IF        PiInt=*on
022400020619     C                   Z-ADD     PiVal         VABLNP
022500020619     C                   Z-ADD     PiVal         VATLNP
022600040506     C                   ENDIF
022700020619     C                   ENDIF
022800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023000020619     C                   EXSR      CHKNUM
023100020619     C                   IF        PiInt=*on
023200020619     C                   Z-ADD     PiVal         VABCTR
023300040506     C                   ENDIF
023400020619     C                   ENDIF
023500020619     C                   ENDIF
023600000801     C*
023700000801     C                   ENDSR
023800000801     C*----------------------------------------------------*
023900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024000000801     C*----------------------------------------------------*
024100040823     C     IMPVAB        BEGSR
024200010305     C*
024300000801     C                   Z-ADD     *zeros        errore            1 0
024400000830     C                   MOVEL     datcor        VABAAS
024500020305     C                   MOVEL     datcor        VATAAS
024600040526     C                   MOVE      datcor        VABMGS
024700040823     C                   MOVE(P)   vlrpoi        VABFGS
024800040823     C                   MOVE(P)   vlrpoi        VATFGS
024900080409     C*
025000080409     C* Reperimento campi ALFA
025100080409     C*
025200080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
025300090223     C                   EVAL      VABRSD=%trim(%subst(vindta:24:30))
025400080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025500080409     C     '@':'A'       XLATE     VABRSD        VABRSD
025600080409     C* ==
025700090223     C                   EVAL      VABRD2=%trim(%subst(vindta:55:30))
025800090223     C                   EVAL      VABIND=%trim(%subst(vindta:86:30))
025900090223     C                   EVAL      VABLOD=%trim(%subst(vindta:122:23))
026000090223     C                   EVAL      VABPRD=%trim(%subst(vindta:145:2))
026100090223     C                   EVAL      VABRMA=%trim(%subst(vindta:1:8))
026200090223     C                   EVAL      VABNOT=%trim(%subst(vindta:169:35))
026300090223     C                   EVAL      VABNT2=%trim(%subst(vindta:205:35))
026400090223     C                   EVAL      VABTC1=%trim(%subst(vindta:255:1))
026500090223     C                   EVAL      VABTSP=%trim(%subst(vindta:274:1))
026600080409     C*
026700080409     C* Reperimento campi NUMERICI
026800090113     C*
026900080409     C* NSP/RMN
027000090223     C                   EVAL      PiStr=%trim(%subst(vindta:9:5))
027100080409     C                   EXSR      CHKNUM
027200080409     C                   IF        PiInt=*on
027300080409     C                   Z-ADD     PiVal         VABNSP
027400080409     C                   Z-ADD     PiVal         VATNSP
027500080409     C                   Z-ADD     PiVal         VABRMN
027600080409     C                   ELSE
027700080409     C                   ADD       1             errore
027800080409     C                   Z-ADD     *zeros        VABNSP
027900080409     C                   Z-ADD     1             VABRMN
028000080409     C                   EVAL      vinmsg = %trimr(vinmsg)
028100080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
028200080409     C                   ENDIF
028300080409     C* CAD
028400090223     C                   EVAL      PiStr=%trim(%subst(vindta:117:5))
028500080409     C                   EXSR      CHKNUM
028600080409     C                   IF        PiInt=*on
028700080409     C                   Z-ADD     PiVal         Num5_0
028800080409     C                   MOVEL(p)  Num5_0        VABCAD
028900080409     C                   ELSE
029000080409     C                   ADD       1             errore
029100080409     C                   EVAL      VABCAD = *zeros
029200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029300080409     C                             + ' ' + 'VABCAD'
029400080409     C                   ENDIF
029500080409     C* NCL
029600090223     C                   EVAL      PiStr=%trim(%subst(vindta:148:5))
029700080409     C                   EXSR      CHKNUM
029800080409     C                   IF        PiInt=*on
029900080409     C                   Z-ADD     PiVal         VABNCL
030000080409     C                   ELSE
030100080409     C                   ADD       1             errore
030200080409     C                   Z-ADD     *zeros        VABNCL
030300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
030400080409     C                             + ' ' + 'VABNCL'
030500080409     C                   ENDIF
030600080409     C* PKB
030700090223     C                   EVAL      PiStr=%trim(%subst(vindta:154:7))
030800080409     C                   EXSR      CHKNUM
030900080409     C                   IF        PiNum=*on
031000090113     C                   Z-ADD     PiVal         VABPKB
031100080409     C                   ELSE
031200080409     C                   ADD       1             errore
031300080409     C                   Z-ADD     *zeros        VABPKB
031400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031500080409     C                             + ' ' + 'VABPKB'
031600080409     C                   ENDIF
031700090223     C* VLB
031800090223     C                   EVAL      PiStr=%trim(%subst(vindta:162:6))
031900090223     C                   EXSR      CHKNUM
032000090223     C                   IF        PiNum=*on
032100090223     C                   Z-ADD     PiVal         VABVLB
032200090223     C                   ELSE
032300090223     C                   ADD       1             errore
032400090223     C                   Z-ADD     *zeros        VABVLB
032500090223     C                   EVAL      vinmsg = %trimr(vinmsg)
032600090223     C                             + ' ' + 'VABVLB'
032700090223     C                   ENDIF
032800090223     C* DCR
032900090428     C                   IF        %subst(vindta:241:8) <> *blanks AND
033000090428     C                             %subst(vindta:241:8) <> *zeros
033100090223     C                   EVAL      PiStr=%trim(%subst(vindta:241:8))
033200090223     C                   EXSR      CHKNUM
033300090223     C                   IF        PiInt=*on
033400090223     C                   Z-ADD     PiVal         VABDCR
033500090223     C                   ELSE
033600090223     C                   ADD       1             errore
033700090223     C                   Z-ADD     *zeros        VABDCR
033800090223     C                   EVAL      vinmsg = %trimr(vinmsg)
033900090223     C                             + ' ' + 'VABDCR'
034000090223     C                   ENDIF
034100090428     C                   ENDIF
034200090223     C* HCR
034300090428     C                   IF        %subst(vindta:250:4) <> *blanks AND
034400090428     C                             %subst(vindta:250:4) <> *zeros
034500090223     C                   EVAL      PiStr=%trim(%subst(vindta:250:4))
034600090223     C                   EXSR      CHKNUM
034700090223     C                   IF        PiInt=*on
034800090223     C                   Z-ADD     PiVal         VABHCR
034900090223     C                   ELSE
035000090223     C                   ADD       1             errore
035100090223     C                   Z-ADD     *zeros        VABHCR
035200090223     C                   EVAL      vinmsg = %trimr(vinmsg)
035300090223     C                             + ' ' + 'VABHCR'
035400090223     C                   ENDIF
035500090428     C                   ENDIF
035600071121     C*
035700080415     C* Carico l'estensioni A e B del FNVAT
035800080415     C                   exsr      exevata
035900071121     C                   exsr      exevatb
036000010205     C*
036100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036200040802     C                   IF        FlgCAS <> '0'
036300010205     C                   EVAL      VABCBO = '4'
036400010205     C                   ENDIF
036500020305     C*
036600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036700011113     C                   EXSR      CHKIMPDIV
036800010202     C*
036900000801     C* Ebbene...
037000000801     C                   ADD       1             §CTRMO
037100010201     C                   IF        errore <> *zeros
037200000801     C                   ADD       1             §CTRNO
037300000801     C                   EVAL      vinflg = '2'
037400000801     C                   ELSE
037500010201     C                   ADD       1             §CTROKVB
037600000801     C                   ENDIF
037700000801     C*
037800000801     C                   ENDSR
037900070102     C*----------------------------------------------------*
038000070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
038100070102     C*----------------------------------------------------*
038200070103     C     EXEVATE       BEGSR
038300110705     C*
038400110705     C* Sviluppati i "CHI SONO" in relazione al numero colli
038500110705     C                   eval      DS_BARCODE = %subst(vindta:276:20)
038600110705     C*
038700110705     C                   dow       DS_B_PAR02 <= DS_B_PAR03
038800110705     C                   eval      VATNOT = DS_B_PAR01            +
038900110705     C                                      %editc(DS_B_PAR02:'X')+
039000110705     C                                      %editc(DS_B_PAR03:'X')
039100110705     C                   eval      VATTRC = 'E'
039200110705     C                   exsr      wrivat                                       => scarico VAT
039300110705     C*
039400110705     C                   eval      DS_B_PAR02 = DS_B_PAR02 + 1
039500110705     C                   enddo
039600070102     C*
039700070102     C                   ENDSR
039800080415     C*----------------------------------------------------*
039900080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
040000080415     C*----------------------------------------------------*
040100080415     C     EXEVATA       BEGSR
040200080415     C*
040300080415     C*
040400080415     C***                exsr      wrivat                                       => scarico VAT
040500080415     C*
040600080415     C                   ENDSR
040700071121     C*----------------------------------------------------*
040800071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
040900071121     C*----------------------------------------------------*
041000071121     C     EXEVATB       BEGSR
041100071121     C*
041200090213     C                   EVAL      VATTRC='B'
041300090223     C                   EVAL      VATNOT=%trim(%subst(vindta:257:16))
041400071121     C*
041500090213     C                   exsr      wrivat                                       => scarico VAT
041600071121     C*
041700071121     C                   ENDSR
041800010201     C*----------------------------------------------------*
041900040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042000010201     C*----------------------------------------------------*
042100020305     C     WRIVAT        BEGSR
042200050628     C*
042300060223     C* Scrivo solo se valorizzato qualcosa
042400060223     C                   IF        VATNOT <> *blanks
042500110705     C                   ADD       1             §CTROKVT
042600040802     C                   WRITE     FIVAT000
042700060223     C                   ENDIF
042800010201     C*
042900010201     C                   ENDSR
043000010202     C*----------------------------------------------------*
043100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043200010202     C*----------------------------------------------------*
043300020305     C     PREVAT        BEGSR
043400010202     C*
043500021113     C* Compongo il nome del membro da dare al FIVATWWR
043600010202     C                   eval      parmbr = vlrhdl
043700010202     C                   movel     'M'           parmbr
043800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
043900010202     C                   eval      paropz = '1'
044000010202     C* Effettuo la chiamata al CLLE preposto
044100040506     C                   call(e)   'TITVVTC'
044200010202     C                   parm                    parccm
044300010202     C                   parm                    parmbr
044400010202     C                   parm                    paropz
044500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044600010202     C                   if        %error
044700010202     C                   movel     '1'           chkcall
044800010202     C                   else
044900010202     C                   movel     '0'           chkcall
045000010202     C                   endif
045100010202     C*
045200010202     C                   ENDSR
045300000801     C*----------------------------------------------------*
045400000801     C*  CONTROLLO NUMERICITA' CAMPI
045500000801     C*----------------------------------------------------*
045600000801     C     CHKNUM        BEGSR
045700090113     C*
045800090113     C                   IF        PiDecChr = *blanks
045900090113     C                   EVAL      PiDecChr = ','
046000090113     C                   ENDIF
046100090113     C*
046200090113     C                   callp(e)  UBISNUM_Check(PiStr
046300090113     C                                          :PiDecChr
046400090113     C                                          :PiVal
046500090113     C                                          :PiNum
046600090113     C                                          :PiInt)
046700090113     C*
046800090113     C                   IF        %error
046900090113     C                   EVAL      PiInt=*off
047000090113     C                   ENDIF
047100000801     C*
047200000801     C                   ENDSR
047300000801     C***
047400000801
047500011113
047600011113     C*----------------------------------------------------*
047700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047800011113     C*----------------------------------------------------*
047900011113     C     CHKIMPDIV     BEGSR
048000011113     C*
048100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048200011113     C                   Z-ADD     *zeros        wrkDec            9 9
048300011113     C*
048400011113     C* Come prima cosa effettuo considerazioni sulla divisa
048500011113     C                   IF        vabIAS > *zeros
048600011113     C                   IF        vabVAS <> 'EUR'
048700011113     C                   EVAL      vabVAS =  'ITL'
048800011113     C                   ENDIF
048900011113     C                   ENDIF
049000011113     C*
049100011113     C                   IF        vabCAS > *zeros
049200011113     C                   IF        vabVCA <> 'EUR'
049300011113     C                   EVAL      vabVCA =  'ITL'
049400011113     C                   ENDIF
049500011113     C                   ENDIF
049600011113     C*
049700011113     C                   IF        vabVMD > *zeros
049800020305     C                   IF        vabVAD <> 'EUR'
049900011113     C                   EVAL      vabVAD =  'ITL'
050000011113     C                   ENDIF
050100011113     C                   ENDIF
050200011113     C*
050300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050400011113     C                   Z-ADD     vabIAS        wrkDec
050500011113     C                   IF        wrkDec > *zeros
050600011113     C                   IF        vabVAS = 'ITL'
050700011113     C                   EVAL      vabIAS = *zeros
050800011113     C                   ENDIF
050900011113     C                   ENDIF
051000011113     C*
051100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051200011113     C                   Z-ADD     vabCAS        wrkDec
051300011113     C                   IF        wrkDec > *zeros
051400011113     C                   IF        vabVCA = 'ITL'
051500011113     C                   EVAL      vabCAS = *zeros
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052000011113     C                   Z-ADD     vabVMD        wrkDec
052100011113     C                   IF        wrkDec > *zeros
052200011113     C                   IF        vabVAD = 'ITL'
052300011113     C                   EVAL      vabVMD = *zeros
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C                   ENDSR
052800011113     C***
052900011113
053000011113
053100000801
053200000801
053300990920      /TITLE Invio dei dati al punto operativo.
053400010202     C     invio         BEGSR
053500990920     C*
053600021113     C* 1° invio FIVAT
053700010201     C                   reset                   dscmz
053800010201     C                   move      vlrpoi        cmzdst
053900021113     C                   eval      cmzfld = 'FIVATWWR'
054000010201     C                   eval      cmzmbd = vlrhdl
054100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054200021009     C***                if        prmfir = *blanks
054300021113     C                   eval      cmzfla = 'FIVAT00F'
054400021113     C                   eval      cmzmba = 'FIVAT00F'
054500021009     C***                else
054600021009     C***                eval      cmzfla = prmfir
054700021009     C***                eval      cmzmba = prmfir
054800021009     C***                endif
054900010201     C                   eval      cmznrr = *zeros
055000020305     C                   move      §ctrokvt      cmznrr
055100021018     C                   eval      cmzlba = vlrfl1
055200010201     C                   call(e)   'TIS711C'
055300010201     C                   parm                    dscmz
055400010201     C                   parm      *blanks       esito
055500010205     C                   if        %error
055600010205     C                             or cmzerr = '1'
055700010205     C                             or esito  = '1'
055800010205     C                   eval      wrkesito = '3'
055900010205     C                   else
056000010201     C*
056100021113     C* 2° invio FIVAB
056200010201     C                   reset                   dscmz
056300010201     C                   move      vlrpoi        cmzdst
056400010201     C                   eval      cmzfld = vlrfou
056500010201     C                   eval      cmzmbd = vlrhdl
056600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056700021009     C***                if        prmfir = *blanks
056800021113     C                   eval      cmzfla = 'FIVAB00F'
056900021113     C                   eval      cmzmba = 'FIVAB00F'
057000021009     C***                else
057100021009     C***                eval      cmzfla = prmfir
057200021009     C***                eval      cmzmba = prmfir
057300021009     C***                endif
057400010201     C                   eval      cmznrr = *zeros
057500010201     C                   move      §ctrokvb      cmznrr
057600021018     C                   eval      cmzlba = vlrfl1
057700010201     C                   call(e)   'TIS711C'
057800010201     C                   parm                    dscmz
057900010201     C                   parm      *blanks       esito
058000010201     C                   if        %error
058100010201     C                             or cmzerr = '1'
058200010201     C                             or esito  = '1'
058300010201     C                   eval      wrkesito = '3'
058400010201     C                   endif
058500010205     C                   endif
058600990920     C*
058700000613     C                   ENDSR
058800000613     C***
058900070411
059000070411     C     *pssr         BEGSR
059100070411     C*
059200070411     C                   if        %open(tivin00r)
059300070411     C                   close     tivin00r
059400070411     C                   endif
059500070411     C                   if        %open(fivabwwr)
059600070411     C                   close     fivabwwr
059700070411     C                   endif
059800070411     C                   if        %open(fivatwwr)
059900070411     C                   close     fivatwwr
060000070411     C                   endif
060100070411     C*
060200070411     C* Effettuo la chiamata al CLLE preposto
060300070411     C                   call(e)   'TITVVTC'
060400070411     C                   parm                    parccm
060500070411     C                   parm                    parmbr
060600070411     C                   parm      '2'           paropz
060700070411     C*
060800070411     C                   eval      wrkesito = '2'
060900070411     C*
061000070411     C                   seton                                        LR
061100070411     C*
061200070411     C                   ENDSR     '*CANCL'
061300070411     C***
061400070411
061500990910
061600000613     C     *inzsr        BEGSR
061700990910     C*
061800990910     C     *entry        plist
061900990920     C                   parm                    tivlrds
062000990921     C                   parm      wrkesito      esito
062100000724     C                   parm                    prmlit
062200000710     C                   parm                    prmfir
062300000613     C*
062400000830     C* CALCOLA LA DATA CORRENTE
062500110705     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
062600110705     C                   eval      datcor = %dec(%date : *ISO)
062700000830     C*
062800000613     C                   ENDSR
062900000613     C***
