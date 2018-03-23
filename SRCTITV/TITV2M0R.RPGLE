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
002500120802     D curSped         s             24    INZ(*blanks)
002600120802     D depSped         s             24    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600090113
003700090113     D*------------------
003800090113     D* LINKING A DEFINIZIONI ESTERNE
003900090113     D*------------------
004000090113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004100090113     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004200090113
004300010201
004400000913     C                   reset                   rrnum
004500990921     C                   reset                   esito
004600990921     C                   reset                   wrkesito
004700000613     C*
004800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004900000613     C*
005000010202     C* Effettuo la chiamata al CLLE preposto
005100040506     C                   call(e)   'TITVVTC'
005200010202     C                   parm                    parccm
005300010202     C                   parm                    parmbr
005400010202     C                   parm      '2'           paropz
005500050201     C*
005600050201     C* Effettuo lancio TISI95 solo x chiusura
005700050201     C                   CLEAR                   TISI95DS
005800050201     C                   EVAL      I95TLA = 'C'
005900050201     C                   CALL      'TISI95R'
006000050201     C                   PARM                    TISI95DS
006100000616     C*
006200000801     C
006300010201     C                   seton                                        LR
006400990908
006500000801
006600910830     C*--------------------------------------------------------
006700040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006800910830     C*--------------------------------------------------------
006900040526     C     RWFILE        BEGSR
007000990910     C*
007100990914     C                   if        not %open(tivin00r)
007200990908     C                   open      tivin00r
007300990914     C                   endif
007400021113     C                   if        not %open(fivabwwr)
007500021113     C                   open      fivabwwr
007600990914     C                   endif
007700070103     C*
007800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007900020305     C                   exsr      prevat
008000010201     C*
008100010202     C                   if        chkcall = '0'
008200010202     C*
008300021113     C                   if        not %open(fivatwwr)
008400021113     C                   open      fivatwwr
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
010600070103     C*
010700070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010800120802     C                   eval      curSped = %subst(vindta:118:24)
010900070213     C                   if        curSped <> depSped
011000070213     C* Se prima bolla => importo bolla corrente
011100070213     C                   if        §CTRMO = *zeros
011200070213     C                   exsr      inzvar
011300070213     C                   exsr      defcam
011400070213     C                   exsr      impvab                                       => carico  VAB
011500070213     C                   else
011600070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
011700070213     C                   exsr      wrivab                                       => scarico VAB
011800070213     C                   exsr      inzvar
011900070213     C                   exsr      defcam
012000070213     C                   exsr      impvab                                       => carico  VAB
012100070213     C                   endif
012200071121     C                   exsr      exevate                                      => write VAT-E
012300070103     C* Salvo il raggruppamento spedizione cliente corrente
012400070103     C                   eval      depSped = curSped
012500070213     C*
012600070213     C* Se collo successivo x stessa bolla
012700070213     C                   else
012800070213     C                   exsr      impvab                                       => carico  VAB
012900070213     C                   exsr      exevate                                      => write VAT-E
013000070103     C                   endif
013100000905     C*
013200000905     C                   else
013300000905     C                   eval      vinflg = '1'
013400050628     C                   endif
013500000905     C                   endif
013600000905     C*
013700000905     C  N70              update    tivin000
013800000905     C*
013900991022     C  N70              ENDdo
014000070213     C*
014100070213     C* Scarico testata bolla rimasta in sospesa
014200070213     C                   exsr      wrivab                                       => scarico VAB
014300010202     C*
014400010202     C                   endif
014500990910
014600990910     C* Se non ci sono record con errori ...
014700000710     C                   if        §ctrno = 0
014800990910     C* ... restituisco esito OK.
014900990921     C                   eval      wrkesito = '0'
015000990910     C                   else
015100010201     C                   if        §ctrokvb > 0
015200990921     C                   eval      wrkesito = '1'
015300000710     C                   else
015400000710     C                   eval      wrkesito = '2'
015500990910     C                   endif
015600000710     C                   endif
015700990910     C*
015800990914     C                   if        %open(tivin00r)
015900990908     C                   close     tivin00r
016000990914     C                   endif
016100021113     C                   if        %open(fivabwwr)
016200021113     C                   close     fivabwwr
016300990914     C                   endif
016400021113     C                   if        %open(fivatwwr)
016500021113     C                   close     fivatwwr
016600010201     C                   endif
016700990910     C*
016800010201     C                   if        §ctrokvb > 0
016900000724     C                             and vlrpoi <> *zeros
017000010202     C                   exsr      invio
017100990920     C                   endif
017200990920     C*
017300910830     C                   ENDSR
017400000613     C***
017500010305
017600010305     C*----------------------------------------------------*
017700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017800010305     C*----------------------------------------------------*
017900020305     C     WRIVAB        BEGSR
018000010305     C*
018100060225     C* Quindi scarico il buffer del file d testata
018200021113     C                   write     fivab000                                     => scarico il VAB
018300010305     C*
018400010305     C                   ENDSR
018500990920
018600000801     C*----------------------------------------------------*
018700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018800000801     C*----------------------------------------------------*
018900010201     C     INZVAR        BEGSR
019000000801     C*
019100070213     C                   CLEAR                   FIVAB000
019200070213     C                   CLEAR                   FIVAT000
019300070213     C*
019400040802     C                   Z-ADD     *zeros        Num5_0            5 0
019500040802     C                   MOVEL     '0'           FlgCAS            1
019600000801     C*
019700000801     C                   ENDSR
019800000801     C*----------------------------------------------------*
019900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020000000801     C*----------------------------------------------------*
020100000801     C     DEFCAM        BEGSR
020200000801     C*
020300020619     C* Imposto i valori di default...
020400120802     C                   Z-ADD     0433881       VABCCM
020500120802     C                   Z-ADD     0433881       VATCCM
020600120802     C                   Z-ADD     277           VABLNP
020700120802     C                   Z-ADD     277           VATLNP
020800090113     C                   Z-ADD     000           VABCTR
020900120802     C                   MOVEL     '7Q'          VABCTM
021000040823     C                   MOVEL     '1'           VABCBO
021100020619     C* ... e poi verifico se sono stati passati come parametri
021200020619     C                   IF        vlrppt > *blanks
021300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021500020619     C                   EXSR      CHKNUM
021600020619     C                   IF        PiInt=*on
021700020619     C                   Z-ADD     PiVal         VABCCM
021800020619     C                   Z-ADD     PiVal         VATCCM
021900020619     C                   ENDIF
022000040506     C                   ENDIF
022100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022300020619     C                   EXSR      CHKNUM
022400020619     C                   IF        PiInt=*on
022500020619     C                   Z-ADD     PiVal         VABLNP
022600020619     C                   Z-ADD     PiVal         VATLNP
022700040506     C                   ENDIF
022800020619     C                   ENDIF
022900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023100020619     C                   EXSR      CHKNUM
023200020619     C                   IF        PiInt=*on
023300020619     C                   Z-ADD     PiVal         VABCTR
023400040506     C                   ENDIF
023500020619     C                   ENDIF
023600020619     C                   ENDIF
023700000801     C*
023800000801     C                   ENDSR
023900000801     C*----------------------------------------------------*
024000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024100000801     C*----------------------------------------------------*
024200040823     C     IMPVAB        BEGSR
024300010305     C*
024400000801     C                   Z-ADD     *zeros        errore            1 0
024500000830     C                   MOVEL     datcor        VABAAS
024600020305     C                   MOVEL     datcor        VATAAS
024700040526     C                   MOVE      datcor        VABMGS
024800040823     C                   MOVE(P)   vlrpoi        VABFGS
024900040823     C                   MOVE(P)   vlrpoi        VATFGS
025000080409     C*
025100080409     C* Reperimento campi ALFA
025200080409     C*
025300080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
025400090213     C                   EVAL      VABRSD=%trim(%subst(vindta:668:35))
025500080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025600080409     C     '@':'A'       XLATE     VABRSD        VABRSD
025700080409     C* ==
025800090213     C                   EVAL      VABRD2=%trim(%subst(vindta:668+35:50-35))
025900090213     C                   EVAL      VABIND=%trim(%subst(vindta:550:30))
026000090213     C                   EVAL      VABLOD=%trim(%subst(vindta:718:30))
026100090213     C                   EVAL      VABPRD=%trim(%subst(vindta:786:2))
026200090213     C                   EVAL      VABRMA=%trim(%subst(vindta:118:24))
026300101007     C***                EVAL      VABTSP=%trim(%subst(vindta:78:1))
026400090213     C                   EVAL      VABNOT=%trim(%subst(vindta:142:35))
026500090213     C                   EVAL      VABNT2=%trim(%subst(vindta:142+35:60-35))
026600090213     C                   IF        %subst(vindta:649:2) = 'IT'
026700090213     C                   EVAL      VABNZD=*blanks
026800090213     C                   ELSE
026900090213     C                   EVAL      VABNZD=%trim(%subst(vindta:649:3))
027000090213     C                   ENDIF
027100090213     C                   IF        %subst(vindta:202:1) = 'R'
027200090213     C                   EVAL      VABCBO='2'
027300090213     C                   ELSE
027400090213     C                   EVAL      VABCBO='1'
027500090213     C                   ENDIF
027600080409     C*
027700080409     C* Reperimento campi NUMERICI
027800090113     C*
027900080409     C* NSP/RMN
028000120802     C***                EVAL      PiStr=%trim(%subst(vindta:118:10))
028100120802     C                   EVAL      PiStr=%trim(%subst(vindta:34+2:15-2))
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
029500090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
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
030700090213     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
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
031800090213     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
031900080409     C                   EXSR      CHKNUM
032000080409     C                   IF        PiNum=*on
032100090213     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
032200090113     C                   Z-ADD     PiVal         VABPKB
032300080409     C                   ELSE
032400080409     C                   ADD       1             errore
032500080409     C                   Z-ADD     *zeros        VABPKB
032600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032700080409     C                             + ' ' + 'VABPKB'
032800080409     C                   ENDIF
032900090213     C* IAS
033000101007     C                   IF        %trim(%subst(vindta:82:13)) <> *blanks AND
033100101007     C                             %trim(%subst(vindta:82:13)) <> *zeros
033200101007     C                   EVAL      VABVAS = %trim(%subst(vindta:95:3))
033300090213     C                   EVAL      PiStr=%trim(%subst(vindta:82:13))
033400080409     C                   EXSR      CHKNUM
033500080409     C                   IF        PiNum=*on
033600090213     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
033700090213     C                   Z-ADD     PiVal         VABIAS
033800080409     C                   ELSE
033900080409     C                   ADD       1             errore
034000090213     C                   Z-ADD     *zeros        VABIAS
034100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
034200090213     C                             + ' ' + 'VABIAS'
034300080409     C                   ENDIF
034400080409     C                   ENDIF
034500101007     C* CAS
034600101007     C                   IF        %trim(%subst(vindta:1523:13)) <> *blanks AND
034700101007     C                             %trim(%subst(vindta:1523:13)) <> *zeros
034800101007     C                   MOVEL     '1'           FlgCAS
034900101007     C                   EVAL      VABVCA = 'EUR'
035000101007     C                   EVAL      PiStr=%trim(%subst(vindta:1523:13))
035100101007     C                   EXSR      CHKNUM
035200101007     C                   IF        PiNum=*on
035300101007     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
035400101007     C                   Z-ADD     PiVal         VABCAS
035500101007     C                   ELSE
035600101007     C                   ADD       1             errore
035700101007     C                   Z-ADD     *zeros        VABCAS
035800101007     C                   EVAL      vinmsg = %trimr(vinmsg)
035900101007     C                             + ' ' + 'VABCAS'
036000101007     C                   ENDIF
036100101007     C                   ENDIF
036200071121     C*
036300080415     C* Carico l'estensioni A e B del FNVAT
036400080415     C                   exsr      exevata
036500071121     C                   exsr      exevatb
036600010205     C*
036700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036800040802     C                   IF        FlgCAS <> '0'
036900010205     C                   EVAL      VABCBO = '4'
037000010205     C                   ENDIF
037100020305     C*
037200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037300011113     C                   EXSR      CHKIMPDIV
037400010202     C*
037500000801     C* Ebbene...
037600000801     C                   ADD       1             §CTRMO
037700010201     C                   IF        errore <> *zeros
037800000801     C                   ADD       1             §CTRNO
037900000801     C                   EVAL      vinflg = '2'
038000000801     C                   ELSE
038100010201     C                   ADD       1             §CTROKVB
038200000801     C                   ENDIF
038300000801     C*
038400000801     C                   ENDSR
038500070102     C*----------------------------------------------------*
038600070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
038700070102     C*----------------------------------------------------*
038800070103     C     EXEVATE       BEGSR
038900101007     C*
039000120802     C                   EVAL      VATTRC='E'
039100120802     C                   EVAL      VATNOT=%trim(%subst(vindta:34:15))
039200101007     C*
039300120802     C                   exsr      wrivat
039400070102     C*
039500070102     C                   ENDSR
039600080415     C*----------------------------------------------------*
039700080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
039800080415     C*----------------------------------------------------*
039900080415     C     EXEVATA       BEGSR
040000080415     C*
040100120802     C                   EVAL      VATTRC='A'
040200120802     C                   EVAL      VATNOT=%trim(%subst(vindta:748:22))
040300080415     C*
040400120802     C                   exsr      wrivat                                       => scarico VAT
040500080415     C*
040600080415     C                   ENDSR
040700071121     C*----------------------------------------------------*
040800071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
040900071121     C*----------------------------------------------------*
041000071121     C     EXEVATB       BEGSR
041100071121     C*
041200090213     C                   EVAL      VATTRC='B'
041300120802     C                   EVAL      VATNOT=%trim(%subst(vindta:652:7+9))
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
042500040802     C                   WRITE     FIVAT000
042600060223     C                   ENDIF
042700010201     C*
042800010201     C                   ENDSR
042900010202     C*----------------------------------------------------*
043000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043100010202     C*----------------------------------------------------*
043200020305     C     PREVAT        BEGSR
043300010202     C*
043400021113     C* Compongo il nome del membro da dare al FIVATWWR
043500010202     C                   eval      parmbr = vlrhdl
043600010202     C                   movel     'M'           parmbr
043700050627     C                   eval      parccm = %subst(vlrKSC:2:7)
043800010202     C                   eval      paropz = '1'
043900010202     C* Effettuo la chiamata al CLLE preposto
044000040506     C                   call(e)   'TITVVTC'
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
045600090113     C*
045700090113     C                   IF        PiDecChr = *blanks
045800090113     C                   EVAL      PiDecChr = ','
045900090113     C                   ENDIF
046000090113     C*
046100090113     C                   callp(e)  UBISNUM_Check(PiStr
046200090113     C                                          :PiDecChr
046300090113     C                                          :PiVal
046400090113     C                                          :PiNum
046500090113     C                                          :PiInt)
046600090113     C*
046700090113     C                   IF        %error
046800090113     C                   EVAL      PiInt=*off
046900090113     C                   ENDIF
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
053500021113     C* 1° invio FIVAT
053600010201     C                   reset                   dscmz
053700010201     C                   move      vlrpoi        cmzdst
053800021113     C                   eval      cmzfld = 'FIVATWWR'
053900010201     C                   eval      cmzmbd = vlrhdl
054000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054100021009     C***                if        prmfir = *blanks
054200021113     C                   eval      cmzfla = 'FIVAT00F'
054300021113     C                   eval      cmzmba = 'FIVAT00F'
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
056000021113     C* 2° invio FIVAB
056100010201     C                   reset                   dscmz
056200010201     C                   move      vlrpoi        cmzdst
056300010201     C                   eval      cmzfld = vlrfou
056400010201     C                   eval      cmzmbd = vlrhdl
056500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056600021009     C***                if        prmfir = *blanks
056700021113     C                   eval      cmzfla = 'FIVAB00F'
056800021113     C                   eval      cmzmba = 'FIVAB00F'
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
058900070411     C     *pssr         BEGSR
059000070411     C*
059100070411     C                   if        %open(tivin00r)
059200070411     C                   close     tivin00r
059300070411     C                   endif
059400070411     C                   if        %open(fivabwwr)
059500070411     C                   close     fivabwwr
059600070411     C                   endif
059700070411     C                   if        %open(fivatwwr)
059800070411     C                   close     fivatwwr
059900070411     C                   endif
060000070411     C*
060100070411     C* Effettuo la chiamata al CLLE preposto
060200070411     C                   call(e)   'TITVVTC'
060300070411     C                   parm                    parccm
060400070411     C                   parm                    parmbr
060500070411     C                   parm      '2'           paropz
060600070411     C*
060700070411     C                   eval      wrkesito = '2'
060800070411     C*
060900070411     C                   seton                                        LR
061000070411     C*
061100070411     C                   ENDSR     '*CANCL'
061200070411     C***
061300070411
061400990910
061500000613     C     *inzsr        BEGSR
061600990910     C*
061700990910     C     *entry        plist
061800990920     C                   parm                    tivlrds
061900990921     C                   parm      wrkesito      esito
062000000724     C                   parm                    prmlit
062100000710     C                   parm                    prmfir
062200000613     C*
062300000830     C* CALCOLA LA DATA CORRENTE
062400000830     C                   time                    wn14             14 0
062500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062600000830     C                   z-add     wn8           g08dat
062700000830     C                   z-add     *zeros        g08inv
062800000830     C                   movel     '0'           g08err
062900000830     C                   call      'XSRDA8'
063000000830     C                   parm                    wlbda8
063100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063200000830     C*
063300000613     C                   ENDSR
063400000613     C***
