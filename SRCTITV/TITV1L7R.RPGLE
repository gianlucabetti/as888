000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130125     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
002500090107     D curSped         s             15    INZ(*blanks)
002600090107     D depSped         s             15    INZ(*blanks)
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
010900090107     C                   eval      curSped = %trim(%subst(vindta:163:15))
011000070213     C                   if        curSped <> depSped
011100070213     C* Se prima bolla => importo bolla corrente
011200070213     C                   if        §CTRMO = *zeros
011300070213     C                   exsr      inzvar
011400070213     C                   exsr      defcam
011500070213     C                   exsr      impvab                                       => carico  VAB
011600070213     C                   else
011700070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
011800070213     C                   exsr      wrivab                                       => scarico VAB
011900070213     C                   exsr      inzvar
012000070213     C                   exsr      defcam
012100070213     C                   exsr      impvab                                       => carico  VAB
012200070213     C                   endif
012300071121     C                   exsr      exevate                                      => write VAT-E
012400070103     C* Salvo il raggruppamento spedizione cliente corrente
012500070103     C                   eval      depSped = curSped
012600070213     C*
012700070213     C* Se collo successivo x stessa bolla
012800070213     C                   else
012900070213     C                   exsr      impvab                                       => carico  VAB
013000070213     C                   exsr      exevate                                      => write VAT-E
013100070103     C                   endif
013200000905     C*
013300000905     C                   else
013400000905     C                   eval      vinflg = '1'
013500050628     C                   endif
013600000905     C                   endif
013700000905     C*
013800000905     C  N70              update    tivin000
013900000905     C*
014000991022     C  N70              ENDdo
014100070213     C*
014200070213     C* Scarico testata bolla rimasta in sospesa
014300070213     C                   exsr      wrivab                                       => scarico VAB
014400010202     C*
014500010202     C                   endif
014600990910
014700990910     C* Se non ci sono record con errori ...
014800000710     C                   if        §ctrno = 0
014900990910     C* ... restituisco esito OK.
015000990921     C                   eval      wrkesito = '0'
015100990910     C                   else
015200010201     C                   if        §ctrokvb > 0
015300990921     C                   eval      wrkesito = '1'
015400000710     C                   else
015500000710     C                   eval      wrkesito = '2'
015600990910     C                   endif
015700000710     C                   endif
015800990910     C*
015900990914     C                   if        %open(tivin00r)
016000990908     C                   close     tivin00r
016100990914     C                   endif
016200021113     C                   if        %open(fivabwwr)
016300021113     C                   close     fivabwwr
016400990914     C                   endif
016500021113     C                   if        %open(fivatwwr)
016600021113     C                   close     fivatwwr
016700010201     C                   endif
016800990910     C*
016900010201     C                   if        §ctrokvb > 0
017000000724     C                             and vlrpoi <> *zeros
017100010202     C                   exsr      invio
017200990920     C                   endif
017300990920     C*
017400910830     C                   ENDSR
017500000613     C***
017600010305
017700010305     C*----------------------------------------------------*
017800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017900010305     C*----------------------------------------------------*
018000020305     C     WRIVAB        BEGSR
018100010305     C*
018200060225     C* Quindi scarico il buffer del file d testata
018300021113     C                   write     fivab000                                     => scarico il VAB
018400010305     C*
018500010305     C                   ENDSR
018600990920
018700000801     C*----------------------------------------------------*
018800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018900000801     C*----------------------------------------------------*
019000010201     C     INZVAR        BEGSR
019100000801     C*
019200070213     C                   CLEAR                   FIVAB000
019300070213     C                   CLEAR                   FIVAT000
019400070213     C*
019500040802     C                   Z-ADD     *zeros        Num5_0            5 0
019600040802     C                   MOVEL     '0'           FlgCAS            1
019700000801     C*
019800000801     C                   ENDSR
019900000801     C*----------------------------------------------------*
020000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020100000801     C*----------------------------------------------------*
020200000801     C     DEFCAM        BEGSR
020300000801     C*
020400020619     C* Imposto i valori di default...
020500090107     C                   Z-ADD     0895008       VABCCM
020600090107     C                   Z-ADD     0895008       VATCCM
020700090107     C                   Z-ADD     089           VABLNP
020800090107     C                   Z-ADD     089           VATLNP
020900090107     C                   Z-ADD     000           VABCTR
021000070103     C                   MOVEL     '7Q'          VABCTM
021100040823     C                   MOVEL     '1'           VABCBO
021200020619     C* ... e poi verifico se sono stati passati come parametri
021300020619     C                   IF        vlrppt > *blanks
021400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABCCM
021900020619     C                   Z-ADD     PiVal         VATCCM
022000020619     C                   ENDIF
022100040506     C                   ENDIF
022200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022400020619     C                   EXSR      CHKNUM
022500020619     C                   IF        PiInt=*on
022600020619     C                   Z-ADD     PiVal         VABLNP
022700020619     C                   Z-ADD     PiVal         VATLNP
022800040506     C                   ENDIF
022900020619     C                   ENDIF
023000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023200020619     C                   EXSR      CHKNUM
023300020619     C                   IF        PiInt=*on
023400020619     C                   Z-ADD     PiVal         VABCTR
023500040506     C                   ENDIF
023600020619     C                   ENDIF
023700020619     C                   ENDIF
023800000801     C*
023900000801     C                   ENDSR
024000000801     C*----------------------------------------------------*
024100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024200000801     C*----------------------------------------------------*
024300040823     C     IMPVAB        BEGSR
024400010305     C*
024500000801     C                   Z-ADD     *zeros        errore            1 0
024600000830     C                   MOVEL     datcor        VABAAS
024700020305     C                   MOVEL     datcor        VATAAS
024800040526     C                   MOVE      datcor        VABMGS
024900040823     C                   MOVE(P)   vlrpoi        VABFGS
025000040823     C                   MOVE(P)   vlrpoi        VATFGS
025100080409     C*
025200080409     C* Reperimento campi ALFA
025300080409     C*
025400080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
025500090107     C                   EVAL      VABRSD=%trim(%subst(vindta:11:40))
025600080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025700080409     C     '@':'A'       XLATE     VABRSD        VABRSD
025800080409     C* ==
025900090107     C                   EVAL      VABIND=%trim(%subst(vindta:91:32))
026000090107     C                   EVAL      VABLOD=%trim(%subst(vindta:123:32))
026100090107     C                   EVAL      VABPRD=%trim(%subst(vindta:155:2))
026200090107     C                   EVAL      VABRMA=%trim(%subst(vindta:163:15))
026300090107     C                   EVAL      VABNAS=%trim(%subst(vindta:207:15))
026400090107     C                   EVAL      VABNOT=%trim(%subst(vindta:259:35))
026500090107     C                   EVAL      VABNT2=%trim(%subst(vindta:259+35:54-35))
026600090107     C                   IF        %trim(%subst(vindta:162:1)) = 'F'
026700081218     C                   EVAL      VABCBO = '1'
026800081121     C                   ENDIF
026900090107     C                   IF        %trim(%subst(vindta:162:1)) = 'A'
027000081218     C                   EVAL      VABCBO = '2'
027100081218     C                   ENDIF
027200090107     C                   IF        %trim(%subst(vindta:344:1)) = 'T'
027300090107     C                   EVAL      VABTC1 = 'A'
027400090107     C                   ENDIF
027500080409     C*
027600080409     C* Reperimento campi NUMERICI
027700081218     C
027800080409     C* NSP/RMN
027900090107     C                   EVAL      PiStr=%trim(%subst(vindta:163:15))
028000080409     C                   EXSR      CHKNUM
028100080409     C                   IF        PiInt=*on
028200080409     C                   Z-ADD     PiVal         VABNSP
028300080409     C                   Z-ADD     PiVal         VATNSP
028400080409     C                   Z-ADD     PiVal         VABRMN
028500080409     C                   ELSE
028600080409     C                   ADD       1             errore
028700080409     C                   Z-ADD     *zeros        VABNSP
028800080409     C                   Z-ADD     1             VABRMN
028900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029000080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
029100080409     C                   ENDIF
029200080409     C* CAD
029300090107     C                   EVAL      PiStr=%trim(%subst(vindta:157:5))
029400080409     C                   EXSR      CHKNUM
029500080409     C                   IF        PiInt=*on
029600080409     C                   Z-ADD     PiVal         Num5_0
029700080409     C                   MOVEL(p)  Num5_0        VABCAD
029800080409     C                   ELSE
029900080409     C                   ADD       1             errore
030000080409     C                   EVAL      VABCAD = *zeros
030100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
030200080409     C                             + ' ' + 'VABCAD'
030300080409     C                   ENDIF
030400080409     C* NCL
030500090107     C                   EVAL      PiStr=%trim(%subst(vindta:222:5))
030600080409     C                   EXSR      CHKNUM
030700080409     C                   IF        PiInt=*on
030800080409     C                   Z-ADD     PiVal         VABNCL
030900080409     C                   ELSE
031000080409     C                   ADD       1             errore
031100080409     C                   Z-ADD     *zeros        VABNCL
031200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031300080409     C                             + ' ' + 'VABNCL'
031400080409     C                   ENDIF
031500090107     C* DCR
031600090107     C                   IF        %subst(vindta:201:6) <> *blanks AND
031700090107     C                             %subst(vindta:201:6) <> *zeros
031800090107     C                   EVAL      PiStr='20'+%subst(vindta:201:6)
031900090107     C                   EXSR      CHKNUM
032000090107     C                   IF        PiInt=*on
032100090107     C                   Z-ADD     PiVal         VABDCR
032200090107     C                   ELSE
032300090107     C                   ADD       1             errore
032400090107     C                   Z-ADD     *zeros        VABDCR
032500090107     C                   EVAL      vinmsg = %trimr(vinmsg)
032600090107     C                             + ' ' + 'VABDCR'
032700090107     C                   ENDIF
032800090107     C                   ENDIF
032900080409     C* PKB
033000090107     C                   EVAL      PiStr=%trim(%subst(vindta:227:5))
033100080409     C                   EXSR      CHKNUM
033200080409     C                   IF        PiNum=*on
033300081218     C                   Z-ADD(H)  PiVal         VABPKB
033400080409     C                   ELSE
033500080409     C                   ADD       1             errore
033600080409     C                   Z-ADD     *zeros        VABPKB
033700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
033800080409     C                             + ' ' + 'VABPKB'
033900080409     C                   ENDIF
034000090107     C* CAS
034100090107     C                   IF        %subst(vindta:313:13) <> *blanks AND
034200090107     C                             %subst(vindta:313:13) <> *zeros
034300090107     C                   EVAL      FlgCAS = '1'
034400090107     C                   EVAL      VABVCA = 'EUR'
034500090107     C                   EVAL      PiStr=%trim(%subst(vindta:313:13))
034600090107     C                   EXSR      CHKNUM
034700090107     C                   IF        PiInt=*on
034800090107     C                   Z-ADD     PiVal         VABCAS
034900090107     C                   ELSE
035000090107     C                   ADD       1             errore
035100090107     C                   Z-ADD     *zeros        VABCAS
035200090107     C                   EVAL      vinmsg = %trimr(vinmsg)
035300090107     C                             + ' ' + 'VABCAS'
035400090107     C                   ENDIF
035500090107     C                   ENDIF
035600071121     C*
035700080415     C* Carico l'estensioni A e B del FNVAT
035800080415     C                   exsr      exevata
035900071121     C                   exsr      exevatb
036000010205     C*
036100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036200040802     C                   IF        FlgCAS <> '0'
036300081118     C                   IF        VABCBO = '1'
036400010205     C                   EVAL      VABCBO = '4'
036500010205     C                   ENDIF
036600081118     C                   IF        VABCBO = '2'
036700081118     C                   EVAL      VABCBO = '6'
036800081118     C                   ENDIF
036900081118     C                   ENDIF
037000020305     C*
037100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037200011113     C                   EXSR      CHKIMPDIV
037300010202     C*
037400000801     C* Ebbene...
037500000801     C                   ADD       1             §CTRMO
037600010201     C                   IF        errore <> *zeros
037700000801     C                   ADD       1             §CTRNO
037800000801     C                   EVAL      vinflg = '2'
037900000801     C                   ELSE
038000010201     C                   ADD       1             §CTROKVB
038100000801     C                   ENDIF
038200000801     C*
038300000801     C                   ENDSR
038400070102     C*----------------------------------------------------*
038500070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
038600070102     C*----------------------------------------------------*
038700070103     C     EXEVATE       BEGSR
038800081118     C*
038900070102     C*
039000081118     C***                exsr      wrivat                                       => scarico VAT
039100070102     C*
039200070102     C                   ENDSR
039300080415     C*----------------------------------------------------*
039400080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
039500080415     C*----------------------------------------------------*
039600080415     C     EXEVATA       BEGSR
039700080415     C*
039800080415     C*
039900080415     C***                exsr      wrivat                                       => scarico VAT
040000080415     C*
040100080415     C                   ENDSR
040200071121     C*----------------------------------------------------*
040300071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
040400071121     C*----------------------------------------------------*
040500071121     C     EXEVATB       BEGSR
040600071121     C*
040700071121     C*
040800080409     C***                exsr      wrivat                                       => scarico VAT
040900071121     C*
041000071121     C                   ENDSR
041100010201     C*----------------------------------------------------*
041200040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041300010201     C*----------------------------------------------------*
041400020305     C     WRIVAT        BEGSR
041500081118     C*
041600081118     C* Imposto gli stessi campi chiave della testata relativa
041700081118     C                   EVAL      VATAAS = VABAAS
041800081118     C                   EVAL      VATLNP = VABLNP
041900081118     C                   EVAL      VATNRS = VABNRS
042000081118     C                   EVAL      VATNSP = VABNSP
042100081118     C                   EVAL      VATCCM = VABCCM
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
045600081118     C*
045700081118     C                   IF        PiDecChr = *blanks
045800081118     C                   EVAL      PiDecChr = ','
045900081118     C                   ENDIF
046000081118     C*
046100081118     C                   callp(e)  UBISNUM_Check(PiStr
046200081118     C                                          :PiDecChr
046300081118     C                                          :PiVal
046400081118     C                                          :PiNum
046500081118     C                                          :PiInt)
046600081118     C*
046700081118     C                   IF        %error
046800081118     C                   EVAL      PiInt=*off
046900081118     C                   ENDIF
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
