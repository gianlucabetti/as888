000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
002500080807     D curSped         s             15    INZ(*blanks)
002600080807     D depSped         s             15    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600080626
003700080626     D*------------------
003800080626     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
003900080626     D*------------------
004000080626     D PiStr           S            100A   INZ
004100080626     D PiDecChr        S              1A   INZ
004200080626     D PiVal           S             63S30 INZ
004300080626     D PiInt           S               N   INZ
004400080626     D PiNum           S               N   INZ
004500080626
004600080626     D*------------------
004700080626     D* LINKING A DEFINIZIONI ESTERNE
004800080626     D*------------------
004900080626     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005000010201
005100010201
005200000913     C                   reset                   rrnum
005300990921     C                   reset                   esito
005400990921     C                   reset                   wrkesito
005500000613     C*
005600040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005700000613     C*
005800010202     C* Effettuo la chiamata al CLLE preposto
005900040506     C                   call(e)   'TITVVTC'
006000010202     C                   parm                    parccm
006100010202     C                   parm                    parmbr
006200010202     C                   parm      '2'           paropz
006300050201     C*
006400050201     C* Effettuo lancio TISI95 solo x chiusura
006500050201     C                   CLEAR                   TISI95DS
006600050201     C                   EVAL      I95TLA = 'C'
006700050201     C                   CALL      'TISI95R'
006800050201     C                   PARM                    TISI95DS
006900000616     C*
007000000801     C
007100010201     C                   seton                                        LR
007200990908
007300000801
007400910830     C*--------------------------------------------------------
007500040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007600910830     C*--------------------------------------------------------
007700040526     C     RWFILE        BEGSR
007800990910     C*
007900990914     C                   if        not %open(tivin00r)
008000990908     C                   open      tivin00r
008100990914     C                   endif
008200021113     C                   if        not %open(fivabwwr)
008300021113     C                   open      fivabwwr
008400990914     C                   endif
008500070103     C*
008600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008700020305     C                   exsr      prevat
008800010201     C*
008900010202     C                   if        chkcall = '0'
009000010202     C*
009100021113     C                   if        not %open(fivatwwr)
009200021113     C                   open      fivatwwr
009300010201     C                   endif
009400990910     C*
009500010201     C                   clear                   §CTROKVB          5 0
009600020305     C                   clear                   §CTROKVT          5 0
009700000801     C                   clear                   §CTRMO            5 0
009800000801     C                   clear                   §CTRNO            5 0
009900990910     C*
010000921023     C                   DO        *HIVAL
010100990913     C*
010200990915     C                   READ      tivin00r                               70
010300050627     C                   if        vindta > *blanks
010400000613     C                   add       1             rrnum
010500000801     C*
010600000801     C                   if        *in70 = *off
010700000801     C                             and
010800000801     C                             (vinflg = *blanks
010900000801     C                              or vinflg = '0'
011000000801     C                              or vinflg = '2')
011100000801     C*
011200000801     C                   clear                   vinmsg
011300000801     C                   eval      vinflg = '1'
011400070103     C*
011500070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011600080807     C                   eval      curSped = %subst(vindta:175:15)
011700070213     C                   if        curSped <> depSped
011800070213     C* Se prima bolla => importo bolla corrente
011900070213     C                   if        §CTRMO = *zeros
012000070213     C                   exsr      inzvar
012100070213     C                   exsr      defcam
012200070213     C                   exsr      impvab                                       => carico  VAB
012300070213     C                   else
012400070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
012500070213     C                   exsr      wrivab                                       => scarico VAB
012600070213     C                   exsr      inzvar
012700070213     C                   exsr      defcam
012800070213     C                   exsr      impvab                                       => carico  VAB
012900070213     C                   endif
013000070213     C                   exsr      exevate                                      => write VAT-E
013100070103     C* Salvo il raggruppamento spedizione cliente corrente
013200070103     C                   eval      depSped = curSped
013300070213     C*
013400070213     C* Se collo successivo x stessa bolla
013500070213     C                   else
013600070213     C                   exsr      impvab                                       => carico  VAB
013700070213     C                   exsr      exevate                                      => write VAT-E
013800070103     C                   endif
013900000905     C*
014000000905     C                   else
014100000905     C                   eval      vinflg = '1'
014200050628     C                   endif
014300000905     C                   endif
014400000905     C*
014500000905     C  N70              update    tivin000
014600000905     C*
014700991022     C  N70              ENDdo
014800070213     C*
014900070213     C* Scarico testata bolla rimasta in sospesa
015000070213     C                   exsr      wrivab                                       => scarico VAB
015100010202     C*
015200010202     C                   endif
015300990910
015400990910     C* Se non ci sono record con errori ...
015500000710     C                   if        §ctrno = 0
015600990910     C* ... restituisco esito OK.
015700990921     C                   eval      wrkesito = '0'
015800990910     C                   else
015900010201     C                   if        §ctrokvb > 0
016000990921     C                   eval      wrkesito = '1'
016100000710     C                   else
016200000710     C                   eval      wrkesito = '2'
016300990910     C                   endif
016400000710     C                   endif
016500990910     C*
016600990914     C                   if        %open(tivin00r)
016700990908     C                   close     tivin00r
016800990914     C                   endif
016900021113     C                   if        %open(fivabwwr)
017000021113     C                   close     fivabwwr
017100990914     C                   endif
017200021113     C                   if        %open(fivatwwr)
017300021113     C                   close     fivatwwr
017400010201     C                   endif
017500990910     C*
017600010201     C                   if        §ctrokvb > 0
017700000724     C                             and vlrpoi <> *zeros
017800010202     C                   exsr      invio
017900990920     C                   endif
018000990920     C*
018100910830     C                   ENDSR
018200000613     C***
018300010305
018400010305     C*----------------------------------------------------*
018500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018600010305     C*----------------------------------------------------*
018700020305     C     WRIVAB        BEGSR
018800010305     C*
018900060225     C* Quindi scarico il buffer del file d testata
019000021113     C                   write     fivab000                                     => scarico il VAB
019100010305     C*
019200010305     C                   ENDSR
019300990920
019400000801     C*----------------------------------------------------*
019500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019600000801     C*----------------------------------------------------*
019700010201     C     INZVAR        BEGSR
019800000801     C*
019900070213     C                   CLEAR                   FIVAB000
020000070213     C                   CLEAR                   FIVAT000
020100070213     C*
020200040802     C                   Z-ADD     *zeros        Num5_0            5 0
020300040802     C                   MOVEL     '0'           FlgCAS            1
020400000801     C*
020500000801     C                   ENDSR
020600000801     C*----------------------------------------------------*
020700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020800000801     C*----------------------------------------------------*
020900000801     C     DEFCAM        BEGSR
021000000801     C*
021100020619     C* Imposto i valori di default...
021200080807     C                   Z-ADD     0053428       VABCCM
021300080807     C                   Z-ADD     0053428       VATCCM
021400080807     C                   Z-ADD     005           VABLNP
021500080807     C                   Z-ADD     005           VATLNP
021600070927     C                   Z-ADD     000           VABCTR
021700040823     C                   MOVEL     '1'           VABCBO
021800080807     C                   MOVEL     '7Q'          VABCTM
021900020619     C* ... e poi verifico se sono stati passati come parametri
022000020619     C                   IF        vlrppt > *blanks
022100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022300020619     C                   EXSR      CHKNUM
022400020619     C                   IF        PiInt=*on
022500020619     C                   Z-ADD     PiVal         VABCCM
022600020619     C                   Z-ADD     PiVal         VATCCM
022700020619     C                   ENDIF
022800040506     C                   ENDIF
022900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023100020619     C                   EXSR      CHKNUM
023200020619     C                   IF        PiInt=*on
023300020619     C                   Z-ADD     PiVal         VABLNP
023400020619     C                   Z-ADD     PiVal         VATLNP
023500040506     C                   ENDIF
023600020619     C                   ENDIF
023700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023900020619     C                   EXSR      CHKNUM
024000020619     C                   IF        PiInt=*on
024100020619     C                   Z-ADD     PiVal         VABCTR
024200040506     C                   ENDIF
024300020619     C                   ENDIF
024400020619     C                   ENDIF
024500000801     C*
024600000801     C                   ENDSR
024700000801     C*----------------------------------------------------*
024800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024900000801     C*----------------------------------------------------*
025000040823     C     IMPVAB        BEGSR
025100010305     C*
025200000801     C                   Z-ADD     *zeros        errore            1 0
025300000830     C                   MOVEL     datcor        VABAAS
025400020305     C                   MOVEL     datcor        VATAAS
025500040526     C                   MOVE      datcor        VABMGS
025600040823     C                   MOVE(P)   vlrpoi        VABFGS
025700040823     C                   MOVE(P)   vlrpoi        VATFGS
025800050628     C*
025900080807     C                   EVAL      VABRSD=%trim(%subst(vindta:21:35))
026000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026200020117     C* ==
026300080807     C                   EVAL      VABRD2=%trim(%subst(vindta:56:35))
026400080807     C                   EVAL      VABIND=%trim(%subst(vindta:91:35))
026500080807     C                   EVAL      VABLOD=%trim(%subst(vindta:135:35))
026600080807     C                   EVAL      VABPRD=%trim(%subst(vindta:170:2))
026700080807     C                   EVAL      VABRMA=%trim(%subst(vindta:175:15))
026800050628     C* CAP
026900080807     C                   EVAL      PiStr=%trim(%subst(vindta:126:9))
027000010201     C                   EXSR      CHKNUM
027100010201     C                   IF        PiInt=*on
027200010201     C                   Z-ADD     PiVal         Num5_0
027300040506     C                   MOVEL(P)  Num5_0        VABCAD
027400010201     C                   ELSE
027500040506     C                   ADD       1             errore
027600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027700040506     C                             + ' ' + 'VABCAD'
027800010201     C                   ENDIF
027900040506     C* Reperisco la provincia dal CAP e dalla località
028000040526     C                   IF        VABCAD <> *blanks AND
028100040526     C                             VABPRD  = *blanks
028200040506     C                   CLEAR                   TISI95DS
028300040506     C                   EVAL      I95TCN = '3'
028400040506     C                   Z-ADD     datcor        I95DAT
028500040506     C                   EVAL      I95CAP = VABCAD
028600040506     C                   EVAL      I95LOC = VABLOD
028700050627     C                   EVAL      I95NAR = VABNZD
028800040506     C                   CALL      'TISI95R'
028900040506     C                   PARM                    TISI95DS
029000040506     C                   EVAL      VABPRD = O95PRV
029100040506     C                   ENDIF
029200060307     C* NCL
029300080807     C                   EVAL      PiStr=%trim(%subst(vindta:225:5))
029400080626     C                   EXSR      CHKNUM
029500080626     C                   IF        PiInt=*on
029600080626     C                   ADD(H)    PiVal         VABNCL
029700080626     C                   ELSE
029800080626     C                   ADD       1             errore
029900080626     C                   EVAL      vinmsg = %trimr(vinmsg)
030000080626     C                             + ' ' + 'VABNCL'
030100080626     C                   ENDIF
030200040506     C* PKB
030300080807     C                   EVAL      PiStr=%trim(%subst(vindta:230:7))
030400010201     C                   EXSR      CHKNUM
030500010201     C                   IF        PiNum=*on
030600080807     C                   EVAL      PiVal=PiVal/10                               * gestisco 1 dec.
030700070927     C                   ADD(H)    PiVal         VABPKB
030800010201     C                   ELSE
030900010201     C                   ADD       1             errore
031000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031100010201     C                             + ' ' + 'VABPKB'
031200010201     C                   ENDIF
031300080807     C* RMN/RMN
031400080807     C                   EVAL      PiStr=%trim(%subst(vindta:175:15))
031500080807     C                   EXSR      CHKNUM
031600080807     C                   IF        PiInt=*on
031700080807     C                   Z-ADD     PiVal         VABNSP
031800080807     C                   Z-ADD     PiVal         VATNSP
031900080807     C                   Z-ADD     PiVal         VABRMN
032000080807     C                   ELSE
032100080807     C                   ADD       1             errore
032200080807     C                   EVAL      vinmsg = %trimr(vinmsg)
032300080807     C                             + ' ' + 'VABNSP VATNSP VABRMN'
032400080807     C                   ENDIF
032500010205     C*
032600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
032700040802     C                   IF        FlgCAS <> '0'
032800070102     C                   IF        VABCBO = '1'
032900010205     C                   EVAL      VABCBO = '4'
033000010205     C                   ELSE
033100070102     C                   EVAL      VABCBO = '6'
033200070102     C                   ENDIF
033300010205     C                   ENDIF
033400020305     C*
033500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
033600011113     C                   EXSR      CHKIMPDIV
033700010202     C*
033800000801     C* Ebbene...
033900000801     C                   ADD       1             §CTRMO
034000010201     C                   IF        errore <> *zeros
034100000801     C                   ADD       1             §CTRNO
034200000801     C                   EVAL      vinflg = '2'
034300000801     C                   ELSE
034400010201     C                   ADD       1             §CTROKVB
034500000801     C                   ENDIF
034600000801     C*
034700000801     C                   ENDSR
034800070102     C*----------------------------------------------------*
034900070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
035000070102     C*----------------------------------------------------*
035100070103     C     EXEVATE       BEGSR
035200070102     C*
035300080807     C                   EVAL      VATNOT=%trim(%subst(vindta:190:7))
035400080807     C                   EVAL      VATTRC='E'
035500071019     C*
035600070102     C                   exsr      wrivat                                       => scarico VAT
035700070102     C*
035800070102     C                   ENDSR
035900010201     C*----------------------------------------------------*
036000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
036100010201     C*----------------------------------------------------*
036200020305     C     WRIVAT        BEGSR
036300050628     C*
036400060223     C* Scrivo solo se valorizzato qualcosa
036500060223     C                   IF        VATNOT <> *blanks
036600040802     C                   WRITE     FIVAT000
036700060223     C                   ENDIF
036800010201     C*
036900010201     C                   ENDSR
037000010202     C*----------------------------------------------------*
037100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
037200010202     C*----------------------------------------------------*
037300020305     C     PREVAT        BEGSR
037400010202     C*
037500021113     C* Compongo il nome del membro da dare al FIVATWWR
037600010202     C                   eval      parmbr = vlrhdl
037700010202     C                   movel     'M'           parmbr
037800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
037900010202     C                   eval      paropz = '1'
038000010202     C* Effettuo la chiamata al CLLE preposto
038100040506     C                   call(e)   'TITVVTC'
038200010202     C                   parm                    parccm
038300010202     C                   parm                    parmbr
038400010202     C                   parm                    paropz
038500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
038600010202     C                   if        %error
038700010202     C                   movel     '1'           chkcall
038800010202     C                   else
038900010202     C                   movel     '0'           chkcall
039000010202     C                   endif
039100010202     C*
039200010202     C                   ENDSR
039300080626
039400080626
039500080626
039600080626     C*----------------------------------------------------*
039700080626     C*  CONTROLLO NUMERICITA' CAMPI
039800080626     C*----------------------------------------------------*
039900080626     C     CHKNUM        BEGSR
040000080626     C*
040100080626     C                   IF        PiDecChr = *blanks
040200080626     C                   EVAL      PiDecChr = '.'
040300080626     C                   ENDIF
040400080626     C*
040500080626     C                   callp     UBISNUM_Check(PiStr
040600080626     C                                          :PiDecChr
040700080626     C                                          :PiVal
040800080626     C                                          :PiNum
040900080626     C                                          :PiInt)
041000080626     C*
041100080626     C                   ENDSR
041200080626     C***
041300000801
041400011113
041500011113     C*----------------------------------------------------*
041600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
041700011113     C*----------------------------------------------------*
041800011113     C     CHKIMPDIV     BEGSR
041900011113     C*
042000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
042100011113     C                   Z-ADD     *zeros        wrkDec            9 9
042200011113     C*
042300011113     C* Come prima cosa effettuo considerazioni sulla divisa
042400011113     C                   IF        vabIAS > *zeros
042500011113     C                   IF        vabVAS <> 'EUR'
042600011113     C                   EVAL      vabVAS =  'ITL'
042700011113     C                   ENDIF
042800011113     C                   ENDIF
042900011113     C*
043000011113     C                   IF        vabCAS > *zeros
043100011113     C                   IF        vabVCA <> 'EUR'
043200011113     C                   EVAL      vabVCA =  'ITL'
043300011113     C                   ENDIF
043400011113     C                   ENDIF
043500011113     C*
043600011113     C                   IF        vabVMD > *zeros
043700020305     C                   IF        vabVAD <> 'EUR'
043800011113     C                   EVAL      vabVAD =  'ITL'
043900011113     C                   ENDIF
044000011113     C                   ENDIF
044100011113     C*
044200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
044300011113     C                   Z-ADD     vabIAS        wrkDec
044400011113     C                   IF        wrkDec > *zeros
044500011113     C                   IF        vabVAS = 'ITL'
044600011113     C                   EVAL      vabIAS = *zeros
044700011113     C                   ENDIF
044800011113     C                   ENDIF
044900011113     C*
045000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
045100011113     C                   Z-ADD     vabCAS        wrkDec
045200011113     C                   IF        wrkDec > *zeros
045300011113     C                   IF        vabVCA = 'ITL'
045400011113     C                   EVAL      vabCAS = *zeros
045500011113     C                   ENDIF
045600011113     C                   ENDIF
045700011113     C*
045800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045900011113     C                   Z-ADD     vabVMD        wrkDec
046000011113     C                   IF        wrkDec > *zeros
046100011113     C                   IF        vabVAD = 'ITL'
046200011113     C                   EVAL      vabVMD = *zeros
046300011113     C                   ENDIF
046400011113     C                   ENDIF
046500011113     C*
046600011113     C                   ENDSR
046700011113     C***
046800011113
046900011113
047000000801
047100000801
047200990920      /TITLE Invio dei dati al punto operativo.
047300010202     C     invio         BEGSR
047400990920     C*
047500021113     C* 1° invio FIVAT
047600010201     C                   reset                   dscmz
047700010201     C                   move      vlrpoi        cmzdst
047800021113     C                   eval      cmzfld = 'FIVATWWR'
047900010201     C                   eval      cmzmbd = vlrhdl
048000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
048100021009     C***                if        prmfir = *blanks
048200021113     C                   eval      cmzfla = 'FIVAT00F'
048300021113     C                   eval      cmzmba = 'FIVAT00F'
048400021009     C***                else
048500021009     C***                eval      cmzfla = prmfir
048600021009     C***                eval      cmzmba = prmfir
048700021009     C***                endif
048800010201     C                   eval      cmznrr = *zeros
048900020305     C                   move      §ctrokvt      cmznrr
049000021018     C                   eval      cmzlba = vlrfl1
049100010201     C                   call(e)   'TIS711C'
049200010201     C                   parm                    dscmz
049300010201     C                   parm      *blanks       esito
049400010205     C                   if        %error
049500010205     C                             or cmzerr = '1'
049600010205     C                             or esito  = '1'
049700010205     C                   eval      wrkesito = '3'
049800010205     C                   else
049900010201     C*
050000021113     C* 2° invio FIVAB
050100010201     C                   reset                   dscmz
050200010201     C                   move      vlrpoi        cmzdst
050300010201     C                   eval      cmzfld = vlrfou
050400010201     C                   eval      cmzmbd = vlrhdl
050500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
050600021009     C***                if        prmfir = *blanks
050700021113     C                   eval      cmzfla = 'FIVAB00F'
050800021113     C                   eval      cmzmba = 'FIVAB00F'
050900021009     C***                else
051000021009     C***                eval      cmzfla = prmfir
051100021009     C***                eval      cmzmba = prmfir
051200021009     C***                endif
051300010201     C                   eval      cmznrr = *zeros
051400010201     C                   move      §ctrokvb      cmznrr
051500021018     C                   eval      cmzlba = vlrfl1
051600010201     C                   call(e)   'TIS711C'
051700010201     C                   parm                    dscmz
051800010201     C                   parm      *blanks       esito
051900010201     C                   if        %error
052000010201     C                             or cmzerr = '1'
052100010201     C                             or esito  = '1'
052200010201     C                   eval      wrkesito = '3'
052300010201     C                   endif
052400010205     C                   endif
052500990920     C*
052600000613     C                   ENDSR
052700000613     C***
052800070411
052900070411     C     *pssr         BEGSR
053000070411     C*
053100070411     C                   if        %open(tivin00r)
053200070411     C                   close     tivin00r
053300070411     C                   endif
053400070411     C                   if        %open(fivabwwr)
053500070411     C                   close     fivabwwr
053600070411     C                   endif
053700070411     C                   if        %open(fivatwwr)
053800070411     C                   close     fivatwwr
053900070411     C                   endif
054000070411     C*
054100070411     C* Effettuo la chiamata al CLLE preposto
054200070411     C                   call(e)   'TITVVTC'
054300070411     C                   parm                    parccm
054400070411     C                   parm                    parmbr
054500070411     C                   parm      '2'           paropz
054600070411     C*
054700070411     C                   eval      wrkesito = '2'
054800070411     C*
054900070411     C                   seton                                        LR
055000070411     C*
055100070411     C                   ENDSR     '*CANCL'
055200070411     C***
055300070411
055400990910
055500000613     C     *inzsr        BEGSR
055600990910     C*
055700990910     C     *entry        plist
055800990920     C                   parm                    tivlrds
055900990921     C                   parm      wrkesito      esito
056000000724     C                   parm                    prmlit
056100000710     C                   parm                    prmfir
056200000613     C*
056300000830     C* CALCOLA LA DATA CORRENTE
056400000830     C                   time                    wn14             14 0
056500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
056600000830     C                   z-add     wn8           g08dat
056700000830     C                   z-add     *zeros        g08inv
056800000830     C                   movel     '0'           g08err
056900000830     C                   call      'XSRDA8'
057000000830     C                   parm                    wlbda8
057100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057200000830     C*
057300000613     C                   ENDSR
