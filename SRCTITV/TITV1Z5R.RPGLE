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
002500100803     D curSped         s             15    INZ(*blanks)
002600100803     D depSped         s             15    INZ(*blanks)
002700000830
002800090113
002900090113     D*------------------
003000090113     D* LINKING A DEFINIZIONI ESTERNE
003100090113     D*------------------
003200090113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
003300090113     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
003400090113
003500010201
003600000913     C                   reset                   rrnum
003700990921     C                   reset                   esito
003800990921     C                   reset                   wrkesito
003900000613     C*
004000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004100000613     C*
004200010202     C* Effettuo la chiamata al CLLE preposto
004300040506     C                   call(e)   'TITVVTC'
004400010202     C                   parm                    parccm
004500010202     C                   parm                    parmbr
004600010202     C                   parm      '2'           paropz
004700050201     C*
004800050201     C* Effettuo lancio TISI95 solo x chiusura
004900050201     C                   CLEAR                   TISI95DS
005000050201     C                   EVAL      I95TLA = 'C'
005100050201     C                   CALL      'TISI95R'
005200050201     C                   PARM                    TISI95DS
005300000616     C*
005400000801     C
005500010201     C                   seton                                        LR
005600990908
005700000801
005800910830     C*--------------------------------------------------------
005900040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006000910830     C*--------------------------------------------------------
006100040526     C     RWFILE        BEGSR
006200990910     C*
006300990914     C                   if        not %open(tivin00r)
006400990908     C                   open      tivin00r
006500990914     C                   endif
006600021113     C                   if        not %open(fivabwwr)
006700021113     C                   open      fivabwwr
006800990914     C                   endif
006900070103     C*
007000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007100020305     C                   exsr      prevat
007200010201     C*
007300010202     C                   if        chkcall = '0'
007400010202     C*
007500021113     C                   if        not %open(fivatwwr)
007600021113     C                   open      fivatwwr
007700010201     C                   endif
007800990910     C*
007900010201     C                   clear                   §CTROKVB          5 0
008000020305     C                   clear                   §CTROKVT          5 0
008100000801     C                   clear                   §CTRMO            5 0
008200000801     C                   clear                   §CTRNO            5 0
008300990910     C*
008400921023     C                   DO        *HIVAL
008500990913     C*
008600990915     C                   READ      tivin00r                               70
008700050627     C                   if        vindta > *blanks
008800000613     C                   add       1             rrnum
008900000801     C*
009000000801     C                   if        *in70 = *off
009100000801     C                             and
009200000801     C                             (vinflg = *blanks
009300000801     C                              or vinflg = '0'
009400000801     C                              or vinflg = '2')
009500000801     C*
009600000801     C                   clear                   vinmsg
009700000801     C                   eval      vinflg = '1'
009800070103     C*
009900070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010000100803     C                   eval      curSped = %subst(vindta:210:15)
010100070213     C                   if        curSped <> depSped
010200070213     C* Se prima bolla => importo bolla corrente
010300070213     C                   if        §CTRMO = *zeros
010400070213     C                   exsr      inzvar
010500070213     C                   exsr      defcam
010600070213     C                   exsr      impvab                                       => carico  VAB
010700070213     C                   else
010800070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
010900070213     C                   exsr      wrivab                                       => scarico VAB
011000070213     C                   exsr      inzvar
011100070213     C                   exsr      defcam
011200070213     C                   exsr      impvab                                       => carico  VAB
011300070213     C                   endif
011400071121     C                   exsr      exevate                                      => write VAT-E
011500070103     C* Salvo il raggruppamento spedizione cliente corrente
011600070103     C                   eval      depSped = curSped
011700070213     C*
011800070213     C* Se collo successivo x stessa bolla
011900070213     C                   else
012000070213     C                   exsr      impvab                                       => carico  VAB
012100070213     C                   exsr      exevate                                      => write VAT-E
012200070103     C                   endif
012300000905     C*
012400000905     C                   else
012500000905     C                   eval      vinflg = '1'
012600050628     C                   endif
012700000905     C                   endif
012800000905     C*
012900000905     C  N70              update    tivin000
013000000905     C*
013100991022     C  N70              ENDdo
013200070213     C*
013300070213     C* Scarico testata bolla rimasta in sospesa
013400070213     C                   exsr      wrivab                                       => scarico VAB
013500010202     C*
013600010202     C                   endif
013700990910
013800990910     C* Se non ci sono record con errori ...
013900000710     C                   if        §ctrno = 0
014000990910     C* ... restituisco esito OK.
014100990921     C                   eval      wrkesito = '0'
014200990910     C                   else
014300010201     C                   if        §ctrokvb > 0
014400990921     C                   eval      wrkesito = '1'
014500000710     C                   else
014600000710     C                   eval      wrkesito = '2'
014700990910     C                   endif
014800000710     C                   endif
014900990910     C*
015000990914     C                   if        %open(tivin00r)
015100990908     C                   close     tivin00r
015200990914     C                   endif
015300021113     C                   if        %open(fivabwwr)
015400021113     C                   close     fivabwwr
015500990914     C                   endif
015600021113     C                   if        %open(fivatwwr)
015700021113     C                   close     fivatwwr
015800010201     C                   endif
015900990910     C*
016000010201     C                   if        §ctrokvb > 0
016100000724     C                             and vlrpoi <> *zeros
016200010202     C                   exsr      invio
016300990920     C                   endif
016400990920     C*
016500910830     C                   ENDSR
016600000613     C***
016700010305
016800010305     C*----------------------------------------------------*
016900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017000010305     C*----------------------------------------------------*
017100020305     C     WRIVAB        BEGSR
017200010305     C*
017300060225     C* Quindi scarico il buffer del file d testata
017400021113     C                   write     fivab000                                     => scarico il VAB
017500010305     C*
017600010305     C                   ENDSR
017700990920
017800000801     C*----------------------------------------------------*
017900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018000000801     C*----------------------------------------------------*
018100010201     C     INZVAR        BEGSR
018200000801     C*
018300070213     C                   CLEAR                   FIVAB000
018400070213     C                   CLEAR                   FIVAT000
018500070213     C*
018600040802     C                   Z-ADD     *zeros        Num5_0            5 0
018700040802     C                   MOVEL     '0'           FlgCAS            1
018800000801     C*
018900000801     C                   ENDSR
019000000801     C*----------------------------------------------------*
019100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019200000801     C*----------------------------------------------------*
019300000801     C     DEFCAM        BEGSR
019400000801     C*
019500020619     C* Imposto i valori di default...
019600100803     C                   Z-ADD     0064508       VABCCM
019700100803     C                   Z-ADD     0064508       VATCCM
019800100803     C                   Z-ADD     006           VABLNP
019900100803     C                   Z-ADD     006           VATLNP
020000100803     C                   Z-ADD     300           VABCTR
020100100803     C                   Z-ADD     1             VABNCL
020200100803     C                   Z-ADD     1             VABPKB
020300070103     C                   MOVEL     '7Q'          VABCTM
020400040823     C                   MOVEL     '1'           VABCBO
020500020619     C* ... e poi verifico se sono stati passati come parametri
020600020619     C                   IF        vlrppt > *blanks
020700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020900020619     C                   EXSR      CHKNUM
021000020619     C                   IF        PiInt=*on
021100020619     C                   Z-ADD     PiVal         VABCCM
021200020619     C                   Z-ADD     PiVal         VATCCM
021300020619     C                   ENDIF
021400040506     C                   ENDIF
021500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021700020619     C                   EXSR      CHKNUM
021800020619     C                   IF        PiInt=*on
021900020619     C                   Z-ADD     PiVal         VABLNP
022000020619     C                   Z-ADD     PiVal         VATLNP
022100040506     C                   ENDIF
022200020619     C                   ENDIF
022300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022500020619     C                   EXSR      CHKNUM
022600020619     C                   IF        PiInt=*on
022700020619     C                   Z-ADD     PiVal         VABCTR
022800040506     C                   ENDIF
022900020619     C                   ENDIF
023000020619     C                   ENDIF
023100000801     C*
023200000801     C                   ENDSR
023300000801     C*----------------------------------------------------*
023400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023500000801     C*----------------------------------------------------*
023600040823     C     IMPVAB        BEGSR
023700010305     C*
023800000801     C                   Z-ADD     *zeros        errore            1 0
023900000830     C                   MOVEL     datcor        VABAAS
024000020305     C                   MOVEL     datcor        VATAAS
024100040526     C                   MOVE      datcor        VABMGS
024200040823     C                   MOVE(P)   vlrpoi        VABFGS
024300040823     C                   MOVE(P)   vlrpoi        VATFGS
024400080409     C*
024500080409     C* Reperimento campi ALFA
024600080409     C*
024700080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
024800090213     C                   EVAL      VABRSD=%trim(%subst(vindta:668:35))
024900080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025000080409     C     '@':'A'       XLATE     VABRSD        VABRSD
025100080409     C* ==
025200090213     C                   EVAL      VABRD2=%trim(%subst(vindta:668+35:50-35))
025300090213     C                   EVAL      VABIND=%trim(%subst(vindta:550:30))
025400090213     C                   EVAL      VABLOD=%trim(%subst(vindta:718:30))
025500090213     C                   EVAL      VABPRD=%trim(%subst(vindta:786:2))
025600100803     C                   EVAL      VABRMA=%trim(%subst(vindta:210:15))
025700100803     C                   EVAL      VABTSP=%trim(%subst(vindta:225:1))
025800090213     C                   EVAL      VABNOT=%trim(%subst(vindta:142:35))
025900090213     C                   EVAL      VABNT2=%trim(%subst(vindta:142+35:60-35))
026000090213     C                   IF        %subst(vindta:649:2) = 'IT'
026100090213     C                   EVAL      VABNZD=*blanks
026200090213     C                   ELSE
026300090213     C                   EVAL      VABNZD=%trim(%subst(vindta:649:3))
026400090213     C                   ENDIF
026500080409     C*
026600080409     C* Reperimento campi NUMERICI
026700090113     C*
026800080409     C* NSP/RMN
026900100803     C                   EVAL      PiStr=%trim(%subst(vindta:210:15))
027000080409     C                   EXSR      CHKNUM
027100080409     C                   IF        PiInt=*on
027200080409     C                   Z-ADD     PiVal         VABNSP
027300080409     C                   Z-ADD     PiVal         VATNSP
027400080409     C                   Z-ADD     PiVal         VABRMN
027500080409     C                   ELSE
027600080409     C                   ADD       1             errore
027700080409     C                   Z-ADD     *zeros        VABNSP
027800080409     C                   Z-ADD     1             VABRMN
027900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
028000080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
028100080409     C                   ENDIF
028200080409     C* CAD
028300090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
028400080409     C                   EXSR      CHKNUM
028500080409     C                   IF        PiInt=*on
028600080409     C                   Z-ADD     PiVal         Num5_0
028700080409     C                   MOVEL(p)  Num5_0        VABCAD
028800080409     C                   ELSE
028900080409     C                   ADD       1             errore
029000080409     C                   EVAL      VABCAD = *zeros
029100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029200080409     C                             + ' ' + 'VABCAD'
029300080409     C                   ENDIF
029400080409     C* NCL
029500100803     C***                EVAL      PiStr=%trim(%subst(vindta:70:5))
029600100803     C***                EXSR      CHKNUM
029700100803     C***                IF        PiInt=*on
029800100803     C***                Z-ADD     PiVal         VABNCL
029900100803     C***                ELSE
030000100803     C***                ADD       1             errore
030100100803     C***                Z-ADD     *zeros        VABNCL
030200100803     C***                EVAL      vinmsg = %trimr(vinmsg)
030300100803     C***                          + ' ' + 'VABNCL'
030400100803     C***                ENDIF
030500080409     C* PKB
030600100803     C***                EVAL      PiStr=%trim(%subst(vindta:55:8))
030700100803     C***                EXSR      CHKNUM
030800100803     C***                IF        PiNum=*on
030900100803     C***                EVAL      PiVal = PiVal / 1000                         * da grammi a kg
031000100803     C***                Z-ADD     PiVal         VABPKB
031100100803     C***                ELSE
031200100803     C***                ADD       1             errore
031300100803     C***                Z-ADD     *zeros        VABPKB
031400100803     C***                EVAL      vinmsg = %trimr(vinmsg)
031500100803     C***                          + ' ' + 'VABPKB'
031600100803     C***                ENDIF
031700090213     C* IAS
031800100803     C***                IF        %trim(%subst(vindta:82:13)) <> *blanks
031900100803     C***                EVAL      VABVCA = %trim(%subst(vindta:95:3))
032000100803     C***                EVAL      PiStr=%trim(%subst(vindta:82:13))
032100100803     C***                EXSR      CHKNUM
032200100803     C***                IF        PiNum=*on
032300100803     C***                EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
032400100803     C***                Z-ADD     PiVal         VABIAS
032500100803     C***                ELSE
032600100803     C***                ADD       1             errore
032700100803     C***                Z-ADD     *zeros        VABIAS
032800100803     C***                EVAL      vinmsg = %trimr(vinmsg)
032900100803     C***                          + ' ' + 'VABIAS'
033000100803     C***                ENDIF
033100100803     C***                ENDIF
033200101105     C* CAS
033300101105     C                   IF        %trim(%subst(vindta:1523:13)) <> *blanks AND
033400101105     C                             %trim(%subst(vindta:1523:13)) <> *zeros
033500101105     C                   MOVEL     '1'           FlgCAS
033600101105     C                   EVAL      VABVCA = %trim(%subst(vindta:1536:3))
033700101105     C                   EVAL      PiStr=%trim(%subst(vindta:1523:13))
033800101105     C                   EXSR      CHKNUM
033900101105     C                   IF        PiNum=*on
034000101105     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
034100101110     C                   Z-ADD     PiVal         VABCAS
034200101105     C                   ELSE
034300101105     C                   ADD       1             errore
034400101105     C                   Z-ADD     *zeros        VABCAS
034500101105     C                   EVAL      vinmsg = %trimr(vinmsg)
034600101105     C                             + ' ' + 'VABCAS'
034700101105     C                   ENDIF
034800101105     C                   ENDIF
034900071121     C*
035000080415     C* Carico l'estensioni A e B del FNVAT
035100080415     C                   exsr      exevata
035200071121     C                   exsr      exevatb
035300010205     C*
035400010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
035500040802     C                   IF        FlgCAS <> '0'
035600100803     C                   IF        VABCBO = '1'
035700010205     C                   EVAL      VABCBO = '4'
035800100803     C                   ENDIF
035900100803     C                   IF        VABCBO = '2'
036000100803     C                   EVAL      VABCBO = '6'
036100100803     C                   ENDIF
036200010205     C                   ENDIF
036300020305     C*
036400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036500011113     C                   EXSR      CHKIMPDIV
036600010202     C*
036700000801     C* Ebbene...
036800000801     C                   ADD       1             §CTRMO
036900010201     C                   IF        errore <> *zeros
037000000801     C                   ADD       1             §CTRNO
037100000801     C                   EVAL      vinflg = '2'
037200000801     C                   ELSE
037300010201     C                   ADD       1             §CTROKVB
037400000801     C                   ENDIF
037500000801     C*
037600000801     C                   ENDSR
037700070102     C*----------------------------------------------------*
037800070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
037900070102     C*----------------------------------------------------*
038000070103     C     EXEVATE       BEGSR
038100070102     C*
038200070103     C                   EVAL      VATTRC='E'
038300100803     C                   EVAL      VATNOT=%trim(%subst(vindta:34:15))
038400100803     C*
038500070102     C                   exsr      wrivat                                       => scarico VAT
038600070102     C*
038700070102     C                   ENDSR
038800080415     C*----------------------------------------------------*
038900080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
039000080415     C*----------------------------------------------------*
039100080415     C     EXEVATA       BEGSR
039200080415     C*
039300100802     C                   EVAL      VATTRC='A'
039400100802     C                   EVAL      VATNOT=%trim(%subst(vindta:748:22))
039500080415     C*
039600100802     C                   exsr      wrivat                                       => scarico VAT
039700080415     C*
039800080415     C                   ENDSR
039900071121     C*----------------------------------------------------*
040000071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
040100071121     C*----------------------------------------------------*
040200071121     C     EXEVATB       BEGSR
040300071121     C*
040400090213     C                   EVAL      VATTRC='B'
040500100802     C                   EVAL      VATNOT=%trim(%subst(vindta:652:16))
040600071121     C*
040700090213     C                   exsr      wrivat                                       => scarico VAT
040800071121     C*
040900071121     C                   ENDSR
041000010201     C*----------------------------------------------------*
041100040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041200010201     C*----------------------------------------------------*
041300020305     C     WRIVAT        BEGSR
041400050628     C*
041500060223     C* Scrivo solo se valorizzato qualcosa
041600060223     C                   IF        VATNOT <> *blanks
041700040802     C                   WRITE     FIVAT000
041800060223     C                   ENDIF
041900010201     C*
042000010201     C                   ENDSR
042100010202     C*----------------------------------------------------*
042200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042300010202     C*----------------------------------------------------*
042400020305     C     PREVAT        BEGSR
042500010202     C*
042600021113     C* Compongo il nome del membro da dare al FIVATWWR
042700010202     C                   eval      parmbr = vlrhdl
042800010202     C                   movel     'M'           parmbr
042900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
043000010202     C                   eval      paropz = '1'
043100010202     C* Effettuo la chiamata al CLLE preposto
043200040506     C                   call(e)   'TITVVTC'
043300010202     C                   parm                    parccm
043400010202     C                   parm                    parmbr
043500010202     C                   parm                    paropz
043600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043700010202     C                   if        %error
043800010202     C                   movel     '1'           chkcall
043900010202     C                   else
044000010202     C                   movel     '0'           chkcall
044100010202     C                   endif
044200010202     C*
044300010202     C                   ENDSR
044400000801     C*----------------------------------------------------*
044500000801     C*  CONTROLLO NUMERICITA' CAMPI
044600000801     C*----------------------------------------------------*
044700000801     C     CHKNUM        BEGSR
044800090113     C*
044900090113     C                   IF        PiDecChr = *blanks
045000090113     C                   EVAL      PiDecChr = ','
045100090113     C                   ENDIF
045200090113     C*
045300090113     C                   callp(e)  UBISNUM_Check(PiStr
045400090113     C                                          :PiDecChr
045500090113     C                                          :PiVal
045600090113     C                                          :PiNum
045700090113     C                                          :PiInt)
045800090113     C*
045900090113     C                   IF        %error
046000090113     C                   EVAL      PiInt=*off
046100090113     C                   ENDIF
046200000801     C*
046300000801     C                   ENDSR
046400000801     C***
046500000801
046600011113
046700011113     C*----------------------------------------------------*
046800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046900011113     C*----------------------------------------------------*
047000011113     C     CHKIMPDIV     BEGSR
047100011113     C*
047200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047300011113     C                   Z-ADD     *zeros        wrkDec            9 9
047400011113     C*
047500011113     C* Come prima cosa effettuo considerazioni sulla divisa
047600011113     C                   IF        vabIAS > *zeros
047700011113     C                   IF        vabVAS <> 'EUR'
047800011113     C                   EVAL      vabVAS =  'ITL'
047900011113     C                   ENDIF
048000011113     C                   ENDIF
048100011113     C*
048200011113     C                   IF        vabCAS > *zeros
048300011113     C                   IF        vabVCA <> 'EUR'
048400011113     C                   EVAL      vabVCA =  'ITL'
048500011113     C                   ENDIF
048600011113     C                   ENDIF
048700011113     C*
048800011113     C                   IF        vabVMD > *zeros
048900020305     C                   IF        vabVAD <> 'EUR'
049000011113     C                   EVAL      vabVAD =  'ITL'
049100011113     C                   ENDIF
049200011113     C                   ENDIF
049300011113     C*
049400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049500011113     C                   Z-ADD     vabIAS        wrkDec
049600011113     C                   IF        wrkDec > *zeros
049700011113     C                   IF        vabVAS = 'ITL'
049800011113     C                   EVAL      vabIAS = *zeros
049900011113     C                   ENDIF
050000011113     C                   ENDIF
050100011113     C*
050200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
050300011113     C                   Z-ADD     vabCAS        wrkDec
050400011113     C                   IF        wrkDec > *zeros
050500011113     C                   IF        vabVCA = 'ITL'
050600011113     C                   EVAL      vabCAS = *zeros
050700011113     C                   ENDIF
050800011113     C                   ENDIF
050900011113     C*
051000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051100011113     C                   Z-ADD     vabVMD        wrkDec
051200011113     C                   IF        wrkDec > *zeros
051300011113     C                   IF        vabVAD = 'ITL'
051400011113     C                   EVAL      vabVMD = *zeros
051500011113     C                   ENDIF
051600011113     C                   ENDIF
051700011113     C*
051800011113     C                   ENDSR
051900011113     C***
052000011113
052100011113
052200000801
052300000801
052400990920      /TITLE Invio dei dati al punto operativo.
052500010202     C     invio         BEGSR
052600990920     C*
052700021113     C* 1° invio FIVAT
052800010201     C                   reset                   dscmz
052900010201     C                   move      vlrpoi        cmzdst
053000021113     C                   eval      cmzfld = 'FIVATWWR'
053100010201     C                   eval      cmzmbd = vlrhdl
053200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053300021009     C***                if        prmfir = *blanks
053400021113     C                   eval      cmzfla = 'FIVAT00F'
053500021113     C                   eval      cmzmba = 'FIVAT00F'
053600021009     C***                else
053700021009     C***                eval      cmzfla = prmfir
053800021009     C***                eval      cmzmba = prmfir
053900021009     C***                endif
054000010201     C                   eval      cmznrr = *zeros
054100020305     C                   move      §ctrokvt      cmznrr
054200021018     C                   eval      cmzlba = vlrfl1
054300010201     C                   call(e)   'TIS711C'
054400010201     C                   parm                    dscmz
054500010201     C                   parm      *blanks       esito
054600010205     C                   if        %error
054700010205     C                             or cmzerr = '1'
054800010205     C                             or esito  = '1'
054900010205     C                   eval      wrkesito = '3'
055000010205     C                   else
055100010201     C*
055200021113     C* 2° invio FIVAB
055300010201     C                   reset                   dscmz
055400010201     C                   move      vlrpoi        cmzdst
055500010201     C                   eval      cmzfld = vlrfou
055600010201     C                   eval      cmzmbd = vlrhdl
055700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055800021009     C***                if        prmfir = *blanks
055900021113     C                   eval      cmzfla = 'FIVAB00F'
056000021113     C                   eval      cmzmba = 'FIVAB00F'
056100021009     C***                else
056200021009     C***                eval      cmzfla = prmfir
056300021009     C***                eval      cmzmba = prmfir
056400021009     C***                endif
056500010201     C                   eval      cmznrr = *zeros
056600010201     C                   move      §ctrokvb      cmznrr
056700021018     C                   eval      cmzlba = vlrfl1
056800010201     C                   call(e)   'TIS711C'
056900010201     C                   parm                    dscmz
057000010201     C                   parm      *blanks       esito
057100010201     C                   if        %error
057200010201     C                             or cmzerr = '1'
057300010201     C                             or esito  = '1'
057400010201     C                   eval      wrkesito = '3'
057500010201     C                   endif
057600010205     C                   endif
057700990920     C*
057800000613     C                   ENDSR
057900000613     C***
058000070411
058100070411     C     *pssr         BEGSR
058200070411     C*
058300070411     C                   if        %open(tivin00r)
058400070411     C                   close     tivin00r
058500070411     C                   endif
058600070411     C                   if        %open(fivabwwr)
058700070411     C                   close     fivabwwr
058800070411     C                   endif
058900070411     C                   if        %open(fivatwwr)
059000070411     C                   close     fivatwwr
059100070411     C                   endif
059200070411     C*
059300070411     C* Effettuo la chiamata al CLLE preposto
059400070411     C                   call(e)   'TITVVTC'
059500070411     C                   parm                    parccm
059600070411     C                   parm                    parmbr
059700070411     C                   parm      '2'           paropz
059800070411     C*
059900070411     C                   eval      wrkesito = '2'
060000070411     C*
060100070411     C                   seton                                        LR
060200070411     C*
060300070411     C                   ENDSR     '*CANCL'
060400070411     C***
060500070411
060600990910
060700000613     C     *inzsr        BEGSR
060800990910     C*
060900990910     C     *entry        plist
061000990920     C                   parm                    tivlrds
061100990921     C                   parm      wrkesito      esito
061200000724     C                   parm                    prmlit
061300000710     C                   parm                    prmfir
061400000613     C*
061500000830     C* CALCOLA LA DATA CORRENTE
061600100802     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
061700100802     C                   eval      datcor = %dec(%date() : *ISO)
061800000830     C*
061900000613     C                   ENDSR
062000000613     C***
