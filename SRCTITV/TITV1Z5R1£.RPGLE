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
002500101213     D curSped         s              7    INZ(*blanks)
002600101213     D depSped         s              7    INZ(*blanks)
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
010000101213     C                   eval      curSped = %subst(vindta:135:7)
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
020100070103     C                   MOVEL     '7Q'          VABCTM
020200040823     C                   MOVEL     '1'           VABCBO
020300020619     C* ... e poi verifico se sono stati passati come parametri
020400020619     C                   IF        vlrppt > *blanks
020500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020700020619     C                   EXSR      CHKNUM
020800020619     C                   IF        PiInt=*on
020900020619     C                   Z-ADD     PiVal         VABCCM
021000020619     C                   Z-ADD     PiVal         VATCCM
021100020619     C                   ENDIF
021200040506     C                   ENDIF
021300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021500020619     C                   EXSR      CHKNUM
021600020619     C                   IF        PiInt=*on
021700020619     C                   Z-ADD     PiVal         VABLNP
021800020619     C                   Z-ADD     PiVal         VATLNP
021900040506     C                   ENDIF
022000020619     C                   ENDIF
022100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022300020619     C                   EXSR      CHKNUM
022400020619     C                   IF        PiInt=*on
022500020619     C                   Z-ADD     PiVal         VABCTR
022600040506     C                   ENDIF
022700020619     C                   ENDIF
022800020619     C                   ENDIF
022900000801     C*
023000000801     C                   ENDSR
023100000801     C*----------------------------------------------------*
023200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023300000801     C*----------------------------------------------------*
023400040823     C     IMPVAB        BEGSR
023500010305     C*
023600000801     C                   Z-ADD     *zeros        errore            1 0
023700000830     C                   MOVEL     datcor        VABAAS
023800020305     C                   MOVEL     datcor        VATAAS
023900040526     C                   MOVE      datcor        VABMGS
024000040823     C                   MOVE(P)   vlrpoi        VABFGS
024100040823     C                   MOVE(P)   vlrpoi        VATFGS
024200080409     C*
024300080409     C* Reperimento campi ALFA
024400080409     C*
024500080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
024600090213     C                   EVAL      VABRSD=%trim(%subst(vindta:668:35))
024700080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024800080409     C     '@':'A'       XLATE     VABRSD        VABRSD
024900080409     C* ==
025000090213     C                   EVAL      VABRD2=%trim(%subst(vindta:668+35:50-35))
025100090213     C                   EVAL      VABIND=%trim(%subst(vindta:550:30))
025200090213     C                   EVAL      VABLOD=%trim(%subst(vindta:718:30))
025300090213     C                   EVAL      VABPRD=%trim(%subst(vindta:786:2))
025400101213     C                   EVAL      VABRMA=%trim(%subst(vindta:135:7))
025500100803     C                   EVAL      VABTSP=%trim(%subst(vindta:225:1))
025600090213     C                   EVAL      VABNOT=%trim(%subst(vindta:142:35))
025700090213     C                   EVAL      VABNT2=%trim(%subst(vindta:142+35:60-35))
025800090213     C                   IF        %subst(vindta:649:2) = 'IT'
025900090213     C                   EVAL      VABNZD=*blanks
026000090213     C                   ELSE
026100090213     C                   EVAL      VABNZD=%trim(%subst(vindta:649:3))
026200090213     C                   ENDIF
026300080409     C*
026400080409     C* Reperimento campi NUMERICI
026500090113     C*
026600080409     C* NSP/RMN
026700100803     C                   EVAL      PiStr=%trim(%subst(vindta:210:15))
026800080409     C                   EXSR      CHKNUM
026900080409     C                   IF        PiInt=*on
027000080409     C                   Z-ADD     PiVal         VABNSP
027100080409     C                   Z-ADD     PiVal         VATNSP
027200080409     C                   Z-ADD     PiVal         VABRMN
027300080409     C                   ELSE
027400080409     C                   ADD       1             errore
027500080409     C                   Z-ADD     *zeros        VABNSP
027600080409     C                   Z-ADD     1             VABRMN
027700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
027800080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
027900080409     C                   ENDIF
028000080409     C* CAD
028100090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
028200080409     C                   EXSR      CHKNUM
028300080409     C                   IF        PiInt=*on
028400080409     C                   Z-ADD     PiVal         Num5_0
028500080409     C                   MOVEL(p)  Num5_0        VABCAD
028600080409     C                   ELSE
028700080409     C                   ADD       1             errore
028800080409     C                   EVAL      VABCAD = *zeros
028900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029000080409     C                             + ' ' + 'VABCAD'
029100080409     C                   ENDIF
029200080409     C* NCL
029300101213     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
029400101213     C                   EXSR      CHKNUM
029500101213     C                   IF        PiInt=*on
029600101213     C                   Z-ADD     PiVal         VABNCL
029700101213     C                   ELSE
029800101213     C                   ADD       1             errore
029900101213     C                   Z-ADD     *zeros        VABNCL
030000101213     C                   EVAL      vinmsg = %trimr(vinmsg)
030100101213     C                             + ' ' + 'VABNCL'
030200101213     C                   ENDIF
030300080409     C* PKB
030400101213     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
030500101213     C                   EXSR      CHKNUM
030600101213     C                   IF        PiNum=*on
030700101213     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
030800101213     C                   Z-ADD     PiVal         VABPKB
030900101213     C                   ELSE
031000101213     C                   ADD       1             errore
031100101213     C                   Z-ADD     *zeros        VABPKB
031200101213     C                   EVAL      vinmsg = %trimr(vinmsg)
031300101213     C                             + ' ' + 'VABPKB'
031400101213     C                   ENDIF
031500090213     C* IAS
031600100803     C***                IF        %trim(%subst(vindta:82:13)) <> *blanks
031700100803     C***                EVAL      VABVCA = %trim(%subst(vindta:95:3))
031800100803     C***                EVAL      PiStr=%trim(%subst(vindta:82:13))
031900100803     C***                EXSR      CHKNUM
032000100803     C***                IF        PiNum=*on
032100100803     C***                EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
032200100803     C***                Z-ADD     PiVal         VABIAS
032300100803     C***                ELSE
032400100803     C***                ADD       1             errore
032500100803     C***                Z-ADD     *zeros        VABIAS
032600100803     C***                EVAL      vinmsg = %trimr(vinmsg)
032700100803     C***                          + ' ' + 'VABIAS'
032800100803     C***                ENDIF
032900100803     C***                ENDIF
033000101105     C* CAS
033100101105     C                   IF        %trim(%subst(vindta:1523:13)) <> *blanks AND
033200101105     C                             %trim(%subst(vindta:1523:13)) <> *zeros
033300101105     C                   MOVEL     '1'           FlgCAS
033400101105     C                   EVAL      VABVCA = %trim(%subst(vindta:1536:3))
033500101105     C                   EVAL      PiStr=%trim(%subst(vindta:1523:13))
033600101105     C                   EXSR      CHKNUM
033700101105     C                   IF        PiNum=*on
033800101105     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
033900101110     C                   Z-ADD     PiVal         VABCAS
034000101105     C                   ELSE
034100101105     C                   ADD       1             errore
034200101105     C                   Z-ADD     *zeros        VABCAS
034300101105     C                   EVAL      vinmsg = %trimr(vinmsg)
034400101105     C                             + ' ' + 'VABCAS'
034500101105     C                   ENDIF
034600101105     C                   ENDIF
034700071121     C*
034800080415     C* Carico l'estensioni A e B del FNVAT
034900080415     C                   exsr      exevata
035000071121     C                   exsr      exevatb
035100010205     C*
035200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
035300040802     C                   IF        FlgCAS <> '0'
035400100803     C                   IF        VABCBO = '1'
035500010205     C                   EVAL      VABCBO = '4'
035600100803     C                   ENDIF
035700100803     C                   IF        VABCBO = '2'
035800100803     C                   EVAL      VABCBO = '6'
035900100803     C                   ENDIF
036000010205     C                   ENDIF
036100020305     C*
036200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036300011113     C                   EXSR      CHKIMPDIV
036400010202     C*
036500000801     C* Ebbene...
036600000801     C                   ADD       1             §CTRMO
036700010201     C                   IF        errore <> *zeros
036800000801     C                   ADD       1             §CTRNO
036900000801     C                   EVAL      vinflg = '2'
037000000801     C                   ELSE
037100010201     C                   ADD       1             §CTROKVB
037200000801     C                   ENDIF
037300000801     C*
037400000801     C                   ENDSR
037500070102     C*----------------------------------------------------*
037600070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
037700070102     C*----------------------------------------------------*
037800070103     C     EXEVATE       BEGSR
037900070102     C*
038000070103     C                   EVAL      VATTRC='E'
038100100803     C                   EVAL      VATNOT=%trim(%subst(vindta:34:15))
038200100803     C*
038300070102     C                   exsr      wrivat                                       => scarico VAT
038400070102     C*
038500070102     C                   ENDSR
038600080415     C*----------------------------------------------------*
038700080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
038800080415     C*----------------------------------------------------*
038900080415     C     EXEVATA       BEGSR
039000080415     C*
039100100802     C                   EVAL      VATTRC='A'
039200100802     C                   EVAL      VATNOT=%trim(%subst(vindta:748:22))
039300080415     C*
039400100802     C                   exsr      wrivat                                       => scarico VAT
039500080415     C*
039600080415     C                   ENDSR
039700071121     C*----------------------------------------------------*
039800071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
039900071121     C*----------------------------------------------------*
040000071121     C     EXEVATB       BEGSR
040100071121     C*
040200090213     C                   EVAL      VATTRC='B'
040300100802     C                   EVAL      VATNOT=%trim(%subst(vindta:652:16))
040400071121     C*
040500090213     C                   exsr      wrivat                                       => scarico VAT
040600071121     C*
040700071121     C                   ENDSR
040800010201     C*----------------------------------------------------*
040900040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041000010201     C*----------------------------------------------------*
041100020305     C     WRIVAT        BEGSR
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
044600090113     C*
044700090113     C                   IF        PiDecChr = *blanks
044800090113     C                   EVAL      PiDecChr = ','
044900090113     C                   ENDIF
045000090113     C*
045100090113     C                   callp(e)  UBISNUM_Check(PiStr
045200090113     C                                          :PiDecChr
045300090113     C                                          :PiVal
045400090113     C                                          :PiNum
045500090113     C                                          :PiInt)
045600090113     C*
045700090113     C                   IF        %error
045800090113     C                   EVAL      PiInt=*off
045900090113     C                   ENDIF
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
061400100802     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
061500100802     C                   eval      datcor = %dec(%date() : *ISO)
061600000830     C*
061700000613     C                   ENDSR
061800000613     C***
