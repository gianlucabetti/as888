000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081027     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
002500081027     D curSped         s             11    INZ(*blanks)
002600081027     D depSped         s             11    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600010201
003700081027     D*------------------
003800081027     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
003900081027     D*------------------
004000081027     D PiStr           S            100A   INZ
004100081027     D PiDecChr        S              1A   INZ
004200081027     D PiVal           S             63S30 INZ
004300081027     D PiInt           S               N   INZ
004400081027     D PiNum           S               N   INZ
004500081027
004600081027     D*------------------
004700081027     D* LINKING A DEFINIZIONI ESTERNE
004800081027     D*------------------
004900081027     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005000081027
005100081027
005200010201
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600000613     C*
005700040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005800000613     C*
005900010202     C* Effettuo la chiamata al CLLE preposto
006000040506     C                   call(e)   'TITVVTC'
006100010202     C                   parm                    parccm
006200010202     C                   parm                    parmbr
006300010202     C                   parm      '2'           paropz
006400050201     C*
006500050201     C* Effettuo lancio TISI95 solo x chiusura
006600050201     C                   CLEAR                   TISI95DS
006700050201     C                   EVAL      I95TLA = 'C'
006800050201     C                   CALL      'TISI95R'
006900050201     C                   PARM                    TISI95DS
007000000616     C*
007100000801     C
007200010201     C                   seton                                        LR
007300990908
007400000801
007500910830     C*--------------------------------------------------------
007600040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007700910830     C*--------------------------------------------------------
007800040526     C     RWFILE        BEGSR
007900990910     C*
008000990914     C                   if        not %open(tivin00r)
008100990908     C                   open      tivin00r
008200990914     C                   endif
008300021113     C                   if        not %open(fivabwwr)
008400021113     C                   open      fivabwwr
008500990914     C                   endif
008600070103     C*
008700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008800020305     C                   exsr      prevat
008900010201     C*
009000010202     C                   if        chkcall = '0'
009100010202     C*
009200021113     C                   if        not %open(fivatwwr)
009300021113     C                   open      fivatwwr
009400010201     C                   endif
009500990910     C*
009600010201     C                   clear                   §CTROKVB          5 0
009700020305     C                   clear                   §CTROKVT          5 0
009800000801     C                   clear                   §CTRMO            5 0
009900000801     C                   clear                   §CTRNO            5 0
010000990910     C*
010100921023     C                   DO        *HIVAL
010200990913     C*
010300990915     C                   READ      tivin00r                               70
010400050627     C                   if        vindta > *blanks
010500000613     C                   add       1             rrnum
010600000801     C*
010700000801     C                   if        *in70 = *off
010800000801     C                             and
010900000801     C                             (vinflg = *blanks
011000000801     C                              or vinflg = '0'
011100000801     C                              or vinflg = '2')
011200000801     C*
011300000801     C                   clear                   vinmsg
011400000801     C                   eval      vinflg = '1'
011500070103     C*
011600070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011700081027     C                   eval      curSped = %subst(vindta:26:11)
011800070213     C                   if        curSped <> depSped
011900070213     C* Se prima bolla => importo bolla corrente
012000070213     C                   if        §CTRMO = *zeros
012100070213     C                   exsr      inzvar
012200070213     C                   exsr      defcam
012300070213     C                   exsr      impvab                                       => carico  VAB
012400070213     C                   else
012500070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
012600070213     C                   exsr      wrivab                                       => scarico VAB
012700070213     C                   exsr      inzvar
012800070213     C                   exsr      defcam
012900070213     C                   exsr      impvab                                       => carico  VAB
013000070213     C                   endif
013100070213     C                   exsr      exevate                                      => write VAT-E
013200070103     C* Salvo il raggruppamento spedizione cliente corrente
013300070103     C                   eval      depSped = curSped
013400070213     C*
013500070213     C* Se collo successivo x stessa bolla
013600070213     C                   else
013700070213     C                   exsr      impvab                                       => carico  VAB
013800070213     C                   exsr      exevate                                      => write VAT-E
013900070103     C                   endif
014000000905     C*
014100000905     C                   else
014200000905     C                   eval      vinflg = '1'
014300050628     C                   endif
014400000905     C                   endif
014500000905     C*
014600000905     C  N70              update    tivin000
014700000905     C*
014800991022     C  N70              ENDdo
014900070213     C*
015000070213     C* Scarico testata bolla rimasta in sospesa
015100070213     C                   exsr      wrivab                                       => scarico VAB
015200010202     C*
015300010202     C                   endif
015400990910
015500990910     C* Se non ci sono record con errori ...
015600000710     C                   if        §ctrno = 0
015700990910     C* ... restituisco esito OK.
015800990921     C                   eval      wrkesito = '0'
015900990910     C                   else
016000010201     C                   if        §ctrokvb > 0
016100990921     C                   eval      wrkesito = '1'
016200000710     C                   else
016300000710     C                   eval      wrkesito = '2'
016400990910     C                   endif
016500000710     C                   endif
016600990910     C*
016700990914     C                   if        %open(tivin00r)
016800990908     C                   close     tivin00r
016900990914     C                   endif
017000021113     C                   if        %open(fivabwwr)
017100021113     C                   close     fivabwwr
017200990914     C                   endif
017300021113     C                   if        %open(fivatwwr)
017400021113     C                   close     fivatwwr
017500010201     C                   endif
017600990910     C*
017700010201     C                   if        §ctrokvb > 0
017800000724     C                             and vlrpoi <> *zeros
017900010202     C                   exsr      invio
018000990920     C                   endif
018100990920     C*
018200910830     C                   ENDSR
018300000613     C***
018400010305
018500010305     C*----------------------------------------------------*
018600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018700010305     C*----------------------------------------------------*
018800020305     C     WRIVAB        BEGSR
018900010305     C*
019000060225     C* Quindi scarico il buffer del file d testata
019100021113     C                   write     fivab000                                     => scarico il VAB
019200010305     C*
019300010305     C                   ENDSR
019400990920
019500000801     C*----------------------------------------------------*
019600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019700000801     C*----------------------------------------------------*
019800010201     C     INZVAR        BEGSR
019900000801     C*
020000070213     C                   CLEAR                   FIVAB000
020100070213     C                   CLEAR                   FIVAT000
020200070213     C*
020300040802     C                   Z-ADD     *zeros        Num5_0            5 0
020400040802     C                   MOVEL     '0'           FlgCAS            1
020500000801     C*
020600000801     C                   ENDSR
020700000801     C*----------------------------------------------------*
020800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020900000801     C*----------------------------------------------------*
021000000801     C     DEFCAM        BEGSR
021100000801     C*
021200020619     C* Imposto i valori di default...
021300081027     C                   Z-ADD     0054790       VABCCM
021400081028     C                   Z-ADD     0054790       VATCCM
021500081027     C                   Z-ADD     005           VABLNP
021600081027     C                   Z-ADD     005           VATLNP
021700070927     C                   Z-ADD     000           VABCTR
021800070103     C                   MOVEL     '7Q'          VABCTM
021900040823     C                   MOVEL     '1'           VABCBO
022000020619     C* ... e poi verifico se sono stati passati come parametri
022100020619     C                   IF        vlrppt > *blanks
022200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022400020619     C                   EXSR      CHKNUM
022500020619     C                   IF        PiInt=*on
022600020619     C                   Z-ADD     PiVal         VABCCM
022700020619     C                   Z-ADD     PiVal         VATCCM
022800020619     C                   ENDIF
022900040506     C                   ENDIF
023000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023200020619     C                   EXSR      CHKNUM
023300020619     C                   IF        PiInt=*on
023400020619     C                   Z-ADD     PiVal         VABLNP
023500020619     C                   Z-ADD     PiVal         VATLNP
023600040506     C                   ENDIF
023700020619     C                   ENDIF
023800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024000020619     C                   EXSR      CHKNUM
024100020619     C                   IF        PiInt=*on
024200020619     C                   Z-ADD     PiVal         VABCTR
024300040506     C                   ENDIF
024400020619     C                   ENDIF
024500020619     C                   ENDIF
024600000801     C*
024700000801     C                   ENDSR
024800000801     C*----------------------------------------------------*
024900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025000000801     C*----------------------------------------------------*
025100040823     C     IMPVAB        BEGSR
025200010305     C*
025300000801     C                   Z-ADD     *zeros        errore            1 0
025400000830     C                   MOVEL     datcor        VABAAS
025500020305     C                   MOVEL     datcor        VATAAS
025600040526     C                   MOVE      datcor        VABMGS
025700040823     C                   MOVE(P)   vlrpoi        VABFGS
025800040823     C                   MOVE(P)   vlrpoi        VATFGS
025900050628     C*
026000081027     C                   EVAL      VABRSD=%trim(%subst(vindta:68:35))
026100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026300020117     C* ==
026400081028     C                   EVAL      VABIND=%trim(%subst(vindta:103:35))
026500081028     C***                EVAL      VABRD2=%trim(%subst(vindta:103:35))
026600081028     C***                EVAL      VABIND=%trim(%subst(vindta:138:35))
026700081027     C                   EVAL      VABLOD=%trim(%subst(vindta:173:35))
026800081027     C                   EVAL      VABPRD=%trim(%subst(vindta:218:2))
026900081104     C***                EVAL      VABRMA=%trim(%subst(vindta:265:10))
027000090625     C                   EVAL      VABRMA=%trim(%subst(vindta:44:10))
027100090528     C                   EVAL      VABRMO='Johnson & Johnson Medical'
027200081027     C                   EVAL      VABNOT=%trim(%subst(vindta:229:35))
027300050628     C* CAP
027400170419     C                   EVAL      VABCAD=%trim(%subst(vindta:208:10))
027500170419     C***                EVAL      PiStr=%trim(%subst(vindta:208:10))
027600170419     C***                EXSR      CHKNUM
027700170419     C***                IF        PiInt=*on
027800170419     C***                Z-ADD     PiVal         Num5_0
027900170419     C***                MOVEL(P)  Num5_0        VABCAD
028000170419     C***                ELSE
028100170419     C***                ADD       1             errore
028200170419     C***                EVAL      vinmsg = %trimr(vinmsg)
028300170419     C***                          + ' ' + 'VABCAD'
028400170419     C***                ENDIF
028500040506     C* Reperisco la provincia dal CAP e dalla località
028600170419     C                   IF        VABNZD  = *blanks AND
028700170419     C                             VABLOD <> *blanks AND
028800170419     C                             VABCAD <> *blanks AND
028900040526     C                             VABPRD  = *blanks
029000040506     C                   CLEAR                   TISI95DS
029100040506     C                   EVAL      I95TCN = '3'
029200040506     C                   Z-ADD     datcor        I95DAT
029300040506     C                   EVAL      I95CAP = VABCAD
029400040506     C                   EVAL      I95LOC = VABLOD
029500050627     C                   EVAL      I95NAR = VABNZD
029600040506     C                   CALL      'TISI95R'
029700040506     C                   PARM                    TISI95DS
029800040506     C                   EVAL      VABPRD = O95PRV
029900040506     C                   ENDIF
030000081027     C* NCL
030100081027     C                   EVAL      PiStr=%trim(%subst(vindta:40:4))
030200081027     C                   EXSR      CHKNUM
030300081027     C                   IF        PiInt=*on
030400081027     C                   ADD       PiVal         VABNCL
030500081027     C                   ELSE
030600081027     C                   ADD       1             errore
030700081027     C                   EVAL      vinmsg = %trimr(vinmsg)
030800081027     C                             + ' ' + 'VABNCL'
030900081027     C                   ENDIF
031000040506     C* PKB
031100081027     C                   EVAL      PiStr=%trim(%subst(vindta:60:8))
031200010201     C                   EXSR      CHKNUM
031300010201     C                   IF        PiNum=*on
031400081027     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
031500070927     C                   ADD(H)    PiVal         VABPKB
031600010201     C                   ELSE
031700010201     C                   ADD       1             errore
031800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031900010201     C                             + ' ' + 'VABPKB'
032000010201     C                   ENDIF
032100081027     C* CAS
032200081027     C                   IF        %subst(vindta:220:9) <> *all'0'
032300081027     C                   MOVEL     '1'           FlgCAS
032400090528     C***                EVAL      VABTIC = 'BM'
032500090528     C                   EVAL      VABTIC = 'OM'
032600081027     C                   EVAL      VABVCA = 'EUR'
032700081027     C                   EVAL      PiStr=%trim(%subst(vindta:220:9))
032800081027     C                   EXSR      CHKNUM
032900081027     C                   IF        PiNum=*on
033000081027     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
033100081027     C                   ADD(H)    PiVal         VABCAS
033200081027     C                   ELSE
033300081027     C                   ADD       1             errore
033400081027     C                   EVAL      vinmsg = %trimr(vinmsg)
033500081027     C                             + ' ' + 'VABCAS'
033600081027     C                   ENDIF
033700081027     C                   ENDIF
033800071019     C* NSP/RMN
033900081027     C                   EVAL      PiStr=%trim(%subst(vindta:26:11))
034000071019     C                   EXSR      CHKNUM
034100071019     C                   IF        PiInt=*on
034200071019     C                   Z-ADD     PiVal         VABNSP
034300071019     C                   Z-ADD     PiVal         VATNSP
034400071019     C                   Z-ADD     PiVal         VABRMN
034500071019     C                   ELSE
034600071019     C                   ADD       1             errore
034700071019     C                   EVAL      vinmsg = %trimr(vinmsg)
034800071019     C                             + ' ' + 'VABNSP VATNSP VABRMN'
034900071019     C                   ENDIF
035000081111     C*
035100081111     C* Considerazioni particolari su campo note rispettio a particolatità consegna
035200081111     C                   IF        %scan('*':%subst(vindta:229:3)) > *zeros
035300081111     C                   EVAL      VABTC1='A'
035400081111     C                   EXSR      exevatb
035500081111     C                   ENDIF
035600081111     C                   IF        %scan('&':%subst(vindta:229:3)) > *zeros
035700081111     C                   EXSR      exevatb
035800081111     C                   EVAL      VABFFD='S'
035900081111     C                   ENDIF
036000081111     C                   IF        %scan('!':%subst(vindta:229:3)) > *zeros
036100081111     C                   EVAL      VABTC2='P'
036200081111     C                   ENDIF
036300081111     C                   IF        %scan('%':%subst(vindta:229:3)) > *zeros
036400090331     C                   EVAL      VABTIC=*blanks
036500081111     C                   ENDIF
036600081111     C                   IF        %scan('$':%subst(vindta:229:3)) > *zeros
036700090331     C                   EVAL      VABTIC='OM'
036800081111     C                   ENDIF
036900010205     C*
037000010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
037100040802     C                   IF        FlgCAS <> '0'
037200010205     C                   EVAL      VABCBO = '4'
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
038900070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
039000070102     C*----------------------------------------------------*
039100070103     C     EXEVATE       BEGSR
039200070102     C*
039300070103     C                   EVAL      VATTRC='E'
039400081027     C                   EVAL      VATNOT = %trim(%subst(vindta:44:16))
039500071019     C*
039600070102     C                   exsr      wrivat                                       => scarico VAT
039700070102     C*
039800070102     C                   ENDSR
039900081104     C*----------------------------------------------------*
040000081104     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
040100081104     C*----------------------------------------------------*
040200081104     C     EXEVATB       BEGSR
040300081104     C*
040400081104     C                   EVAL      VATTRC='B'
040500081110     C                   EVAL      VATNOT = %trim(%subst(vindta:231:18))
040600081104     C*
040700081104     C                   exsr      wrivat                                       => scarico VAT
040800081104     C*
040900081104     C                   ENDSR
041000010201     C*----------------------------------------------------*
041100040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041200010201     C*----------------------------------------------------*
041300020305     C     WRIVAT        BEGSR
041400050628     C*
041500060223     C* Scrivo solo se valorizzato qualcosa
041600060223     C                   IF        VATNOT <> *blanks
041700040802     C                   WRITE     FIVAT000
041800081028     C                   ADD       1             §CTROKVT
041900060223     C                   ENDIF
042000010201     C*
042100010201     C                   ENDSR
042200010202     C*----------------------------------------------------*
042300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042400010202     C*----------------------------------------------------*
042500020305     C     PREVAT        BEGSR
042600010202     C*
042700021113     C* Compongo il nome del membro da dare al FIVATWWR
042800010202     C                   eval      parmbr = vlrhdl
042900010202     C                   movel     'M'           parmbr
043000050627     C                   eval      parccm = %subst(vlrKSC:2:7)
043100010202     C                   eval      paropz = '1'
043200010202     C* Effettuo la chiamata al CLLE preposto
043300040506     C                   call(e)   'TITVVTC'
043400010202     C                   parm                    parccm
043500010202     C                   parm                    parmbr
043600010202     C                   parm                    paropz
043700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043800010202     C                   if        %error
043900010202     C                   movel     '1'           chkcall
044000010202     C                   else
044100010202     C                   movel     '0'           chkcall
044200010202     C                   endif
044300010202     C*
044400010202     C                   ENDSR
044500000801     C*----------------------------------------------------*
044600000801     C*  CONTROLLO NUMERICITA' CAMPI
044700000801     C*----------------------------------------------------*
044800000801     C     CHKNUM        BEGSR
044900081027     C*
045000081027     C                   IF        PiDecChr = *blanks
045100081027     C                   EVAL      PiDecChr = ','
045200081027     C                   ENDIF
045300081027     C*
045400081027     C                   callp     UBISNUM_Check(PiStr
045500081027     C                                          :PiDecChr
045600081027     C                                          :PiVal
045700081027     C                                          :PiNum
045800081027     C                                          :PiInt)
045900000801     C*
046000000801     C                   ENDSR
046100000801     C***
046200000801
046300011113
046400011113     C*----------------------------------------------------*
046500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046600011113     C*----------------------------------------------------*
046700011113     C     CHKIMPDIV     BEGSR
046800011113     C*
046900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047000011113     C                   Z-ADD     *zeros        wrkDec            9 9
047100011113     C*
047200011113     C* Come prima cosa effettuo considerazioni sulla divisa
047300011113     C                   IF        vabIAS > *zeros
047400011113     C                   IF        vabVAS <> 'EUR'
047500011113     C                   EVAL      vabVAS =  'ITL'
047600011113     C                   ENDIF
047700011113     C                   ENDIF
047800011113     C*
047900011113     C                   IF        vabCAS > *zeros
048000011113     C                   IF        vabVCA <> 'EUR'
048100011113     C                   EVAL      vabVCA =  'ITL'
048200011113     C                   ENDIF
048300011113     C                   ENDIF
048400011113     C*
048500011113     C                   IF        vabVMD > *zeros
048600020305     C                   IF        vabVAD <> 'EUR'
048700011113     C                   EVAL      vabVAD =  'ITL'
048800011113     C                   ENDIF
048900011113     C                   ENDIF
049000011113     C*
049100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049200011113     C                   Z-ADD     vabIAS        wrkDec
049300011113     C                   IF        wrkDec > *zeros
049400011113     C                   IF        vabVAS = 'ITL'
049500011113     C                   EVAL      vabIAS = *zeros
049600011113     C                   ENDIF
049700011113     C                   ENDIF
049800011113     C*
049900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
050000011113     C                   Z-ADD     vabCAS        wrkDec
050100011113     C                   IF        wrkDec > *zeros
050200011113     C                   IF        vabVCA = 'ITL'
050300011113     C                   EVAL      vabCAS = *zeros
050400011113     C                   ENDIF
050500011113     C                   ENDIF
050600011113     C*
050700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050800011113     C                   Z-ADD     vabVMD        wrkDec
050900011113     C                   IF        wrkDec > *zeros
051000011113     C                   IF        vabVAD = 'ITL'
051100011113     C                   EVAL      vabVMD = *zeros
051200011113     C                   ENDIF
051300011113     C                   ENDIF
051400011113     C*
051500011113     C                   ENDSR
051600011113     C***
051700011113
051800011113
051900000801
052000000801
052100990920      /TITLE Invio dei dati al punto operativo.
052200010202     C     invio         BEGSR
052300990920     C*
052400021113     C* 1° invio FIVAT
052500010201     C                   reset                   dscmz
052600010201     C                   move      vlrpoi        cmzdst
052700021113     C                   eval      cmzfld = 'FIVATWWR'
052800010201     C                   eval      cmzmbd = vlrhdl
052900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053000021009     C***                if        prmfir = *blanks
053100021113     C                   eval      cmzfla = 'FIVAT00F'
053200021113     C                   eval      cmzmba = 'FIVAT00F'
053300021009     C***                else
053400021009     C***                eval      cmzfla = prmfir
053500021009     C***                eval      cmzmba = prmfir
053600021009     C***                endif
053700010201     C                   eval      cmznrr = *zeros
053800020305     C                   move      §ctrokvt      cmznrr
053900021018     C                   eval      cmzlba = vlrfl1
054000010201     C                   call(e)   'TIS711C'
054100010201     C                   parm                    dscmz
054200010201     C                   parm      *blanks       esito
054300010205     C                   if        %error
054400010205     C                             or cmzerr = '1'
054500010205     C                             or esito  = '1'
054600010205     C                   eval      wrkesito = '3'
054700010205     C                   else
054800010201     C*
054900021113     C* 2° invio FIVAB
055000010201     C                   reset                   dscmz
055100010201     C                   move      vlrpoi        cmzdst
055200010201     C                   eval      cmzfld = vlrfou
055300010201     C                   eval      cmzmbd = vlrhdl
055400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055500021009     C***                if        prmfir = *blanks
055600021113     C                   eval      cmzfla = 'FIVAB00F'
055700021113     C                   eval      cmzmba = 'FIVAB00F'
055800021009     C***                else
055900021009     C***                eval      cmzfla = prmfir
056000021009     C***                eval      cmzmba = prmfir
056100021009     C***                endif
056200010201     C                   eval      cmznrr = *zeros
056300010201     C                   move      §ctrokvb      cmznrr
056400021018     C                   eval      cmzlba = vlrfl1
056500010201     C                   call(e)   'TIS711C'
056600010201     C                   parm                    dscmz
056700010201     C                   parm      *blanks       esito
056800010201     C                   if        %error
056900010201     C                             or cmzerr = '1'
057000010201     C                             or esito  = '1'
057100010201     C                   eval      wrkesito = '3'
057200010201     C                   endif
057300010205     C                   endif
057400990920     C*
057500000613     C                   ENDSR
057600000613     C***
057700070411
057800070411     C     *pssr         BEGSR
057900070411     C*
058000070411     C                   if        %open(tivin00r)
058100070411     C                   close     tivin00r
058200070411     C                   endif
058300070411     C                   if        %open(fivabwwr)
058400070411     C                   close     fivabwwr
058500070411     C                   endif
058600070411     C                   if        %open(fivatwwr)
058700070411     C                   close     fivatwwr
058800070411     C                   endif
058900070411     C*
059000070411     C* Effettuo la chiamata al CLLE preposto
059100070411     C                   call(e)   'TITVVTC'
059200070411     C                   parm                    parccm
059300070411     C                   parm                    parmbr
059400070411     C                   parm      '2'           paropz
059500070411     C*
059600070411     C                   eval      wrkesito = '2'
059700070411     C*
059800070411     C                   seton                                        LR
059900070411     C*
060000070411     C                   ENDSR     '*CANCL'
060100070411     C***
060200070411
060300990910
060400000613     C     *inzsr        BEGSR
060500990910     C*
060600990910     C     *entry        plist
060700990920     C                   parm                    tivlrds
060800990921     C                   parm      wrkesito      esito
060900000724     C                   parm                    prmlit
061000000710     C                   parm                    prmfir
061100000613     C*
061200000830     C* CALCOLA LA DATA CORRENTE
061300000830     C                   time                    wn14             14 0
061400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061500000830     C                   z-add     wn8           g08dat
061600000830     C                   z-add     *zeros        g08inv
061700000830     C                   movel     '0'           g08err
061800000830     C                   call      'XSRDA8'
061900000830     C                   parm                    wlbda8
062000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062100000830     C*
062200000613     C                   ENDSR
062300000613     C***
