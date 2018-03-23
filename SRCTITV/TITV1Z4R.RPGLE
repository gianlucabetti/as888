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
002500100802     D curSped         s              7    INZ(*blanks)
002600100802     D depSped         s              7    INZ(*blanks)
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
010000100802     C                   eval      curSped = %subst(vindta:118:7)
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
011400110225 xxx C***                exsr      exevate                                      => write VAT-E
011500070103     C* Salvo il raggruppamento spedizione cliente corrente
011600070103     C                   eval      depSped = curSped
011700070213     C*
011800070213     C* Se collo successivo x stessa bolla
011900070213     C                   else
012000070213     C                   exsr      impvab                                       => carico  VAB
012100110225 xxx C***                exsr      exevate                                      => write VAT-E
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
019600100802     C                   Z-ADD     0055685       VABCCM
019700100802     C                   Z-ADD     0055685       VATCCM
019800090213     C                   Z-ADD     005           VABLNP
019900090213     C                   Z-ADD     005           VATLNP
020000090113     C                   Z-ADD     000           VABCTR
020100110225 xxx C***                MOVEL     '7Q'          VABCTM
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
025400090213     C                   EVAL      VABRMA=%trim(%subst(vindta:118:24))
025500100802     C***                EVAL      VABTSP=%trim(%subst(vindta:78:1))
025600090213     C                   EVAL      VABNOT=%trim(%subst(vindta:142:35))
025700090213     C                   EVAL      VABNT2=%trim(%subst(vindta:142+35:60-35))
025800090213     C                   IF        %subst(vindta:649:2) = 'IT'
025900090213     C                   EVAL      VABNZD=*blanks
026000090213     C                   ELSE
026100090213     C                   EVAL      VABNZD=%trim(%subst(vindta:649:3))
026200090213     C                   ENDIF
026300090213     C                   IF        %subst(vindta:202:1) = 'R'
026400090213     C                   EVAL      VABCBO='2'
026500090213     C                   ELSE
026600090213     C                   EVAL      VABCBO='1'
026700090213     C                   ENDIF
026800080409     C*
026900080409     C* Reperimento campi NUMERICI
027000090113     C*
027100080409     C* NSP/RMN
027200100802     C                   EVAL      PiStr=%trim(%subst(vindta:118:7))
027300080409     C                   EXSR      CHKNUM
027400080409     C                   IF        PiInt=*on
027500080409     C                   Z-ADD     PiVal         VABNSP
027600080409     C                   Z-ADD     PiVal         VATNSP
027700080409     C                   Z-ADD     PiVal         VABRMN
027800080409     C                   ELSE
027900080409     C                   ADD       1             errore
028000080409     C                   Z-ADD     *zeros        VABNSP
028100080409     C                   Z-ADD     1             VABRMN
028200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
028300080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
028400080409     C                   ENDIF
028500080409     C* CAD
028600090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
028700080409     C                   EXSR      CHKNUM
028800080409     C                   IF        PiInt=*on
028900080409     C                   Z-ADD     PiVal         Num5_0
029000080409     C                   MOVEL(p)  Num5_0        VABCAD
029100080409     C                   ELSE
029200080409     C                   ADD       1             errore
029300080409     C                   EVAL      VABCAD = *zeros
029400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029500080409     C                             + ' ' + 'VABCAD'
029600080409     C                   ENDIF
029700080409     C* NCL
029800090213     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
029900080409     C                   EXSR      CHKNUM
030000080409     C                   IF        PiInt=*on
030100080409     C                   Z-ADD     PiVal         VABNCL
030200080409     C                   ELSE
030300080409     C                   ADD       1             errore
030400080409     C                   Z-ADD     *zeros        VABNCL
030500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
030600080409     C                             + ' ' + 'VABNCL'
030700080409     C                   ENDIF
030800080409     C* PKB
030900090213     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
031000080409     C                   EXSR      CHKNUM
031100080409     C                   IF        PiNum=*on
031200090213     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
031300090113     C                   Z-ADD     PiVal         VABPKB
031400080409     C                   ELSE
031500080409     C                   ADD       1             errore
031600080409     C                   Z-ADD     *zeros        VABPKB
031700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031800080409     C                             + ' ' + 'VABPKB'
031900080409     C                   ENDIF
032000090213     C* IAS
032100090213     C                   IF        %trim(%subst(vindta:82:13)) <> *blanks
032200090213     C                   EVAL      VABVCA = %trim(%subst(vindta:95:3))
032300090213     C                   EVAL      PiStr=%trim(%subst(vindta:82:13))
032400080409     C                   EXSR      CHKNUM
032500080409     C                   IF        PiNum=*on
032600090213     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
032700090213     C                   Z-ADD     PiVal         VABIAS
032800080409     C                   ELSE
032900080409     C                   ADD       1             errore
033000090213     C                   Z-ADD     *zeros        VABIAS
033100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
033200090213     C                             + ' ' + 'VABIAS'
033300080409     C                   ENDIF
033400080409     C                   ENDIF
033500071121     C*
033600080415     C* Carico l'estensioni A e B del FNVAT
033700080415     C                   exsr      exevata
033800071121     C                   exsr      exevatb
033900010205     C*
034000010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034100040802     C                   IF        FlgCAS <> '0'
034200010205     C                   EVAL      VABCBO = '4'
034300010205     C                   ENDIF
034400020305     C*
034500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
034600011113     C                   EXSR      CHKIMPDIV
034700010202     C*
034800000801     C* Ebbene...
034900000801     C                   ADD       1             §CTRMO
035000010201     C                   IF        errore <> *zeros
035100000801     C                   ADD       1             §CTRNO
035200000801     C                   EVAL      vinflg = '2'
035300000801     C                   ELSE
035400010201     C                   ADD       1             §CTROKVB
035500000801     C                   ENDIF
035600000801     C*
035700000801     C                   ENDSR
035800070102     C*----------------------------------------------------*
035900070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
036000070102     C*----------------------------------------------------*
036100070103     C     EXEVATE       BEGSR
036200070102     C*
036300090113     C                   Z-ADD     1             wCollo            2 0
036400090213     C                   MOVEL     *zeros        wColloAlfa1      12
036500080415     C*
036600070103     C                   EVAL      VATTRC='E'
036700100802     C                   EVAL      wColloAlfa1=%subst(vindta:118:7)
036800071019     C*
036900080415     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
037000090113     C                   dow       wCollo <= VABNCL and wCollo < 99
037100090219     C                   eval      VATNOT = %trim(wColloAlfa1) + %char(wCollo)
037200070102     C                   exsr      wrivat                                       => scarico VAT
037300080415     C                   add       1             wCollo
037400080415     C                   enddo
037500070102     C*
037600070102     C                   ENDSR
037700080415     C*----------------------------------------------------*
037800080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
037900080415     C*----------------------------------------------------*
038000080415     C     EXEVATA       BEGSR
038100080415     C*
038200100802     C                   EVAL      VATTRC='A'
038300100802     C                   EVAL      VATNOT=%trim(%subst(vindta:748:22))
038400080415     C*
038500100802     C                   exsr      wrivat                                       => scarico VAT
038600080415     C*
038700080415     C                   ENDSR
038800071121     C*----------------------------------------------------*
038900071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
039000071121     C*----------------------------------------------------*
039100071121     C     EXEVATB       BEGSR
039200071121     C*
039300090213     C                   EVAL      VATTRC='B'
039400100802     C                   EVAL      VATNOT=%trim(%subst(vindta:652:16))
039500071121     C*
039600090213     C                   exsr      wrivat                                       => scarico VAT
039700071121     C*
039800071121     C                   ENDSR
039900010201     C*----------------------------------------------------*
040000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
040100010201     C*----------------------------------------------------*
040200020305     C     WRIVAT        BEGSR
040300050628     C*
040400060223     C* Scrivo solo se valorizzato qualcosa
040500060223     C                   IF        VATNOT <> *blanks
040600040802     C                   WRITE     FIVAT000
040700060223     C                   ENDIF
040800010201     C*
040900010201     C                   ENDSR
041000010202     C*----------------------------------------------------*
041100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041200010202     C*----------------------------------------------------*
041300020305     C     PREVAT        BEGSR
041400010202     C*
041500021113     C* Compongo il nome del membro da dare al FIVATWWR
041600010202     C                   eval      parmbr = vlrhdl
041700010202     C                   movel     'M'           parmbr
041800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
041900010202     C                   eval      paropz = '1'
042000010202     C* Effettuo la chiamata al CLLE preposto
042100040506     C                   call(e)   'TITVVTC'
042200010202     C                   parm                    parccm
042300010202     C                   parm                    parmbr
042400010202     C                   parm                    paropz
042500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042600010202     C                   if        %error
042700010202     C                   movel     '1'           chkcall
042800010202     C                   else
042900010202     C                   movel     '0'           chkcall
043000010202     C                   endif
043100010202     C*
043200010202     C                   ENDSR
043300000801     C*----------------------------------------------------*
043400000801     C*  CONTROLLO NUMERICITA' CAMPI
043500000801     C*----------------------------------------------------*
043600000801     C     CHKNUM        BEGSR
043700090113     C*
043800090113     C                   IF        PiDecChr = *blanks
043900090113     C                   EVAL      PiDecChr = ','
044000090113     C                   ENDIF
044100090113     C*
044200090113     C                   callp(e)  UBISNUM_Check(PiStr
044300090113     C                                          :PiDecChr
044400090113     C                                          :PiVal
044500090113     C                                          :PiNum
044600090113     C                                          :PiInt)
044700090113     C*
044800090113     C                   IF        %error
044900090113     C                   EVAL      PiInt=*off
045000090113     C                   ENDIF
045100000801     C*
045200000801     C                   ENDSR
045300000801     C***
045400000801
045500011113
045600011113     C*----------------------------------------------------*
045700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045800011113     C*----------------------------------------------------*
045900011113     C     CHKIMPDIV     BEGSR
046000011113     C*
046100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046200011113     C                   Z-ADD     *zeros        wrkDec            9 9
046300011113     C*
046400011113     C* Come prima cosa effettuo considerazioni sulla divisa
046500011113     C                   IF        vabIAS > *zeros
046600011113     C                   IF        vabVAS <> 'EUR'
046700011113     C                   EVAL      vabVAS =  'ITL'
046800011113     C                   ENDIF
046900011113     C                   ENDIF
047000011113     C*
047100011113     C                   IF        vabCAS > *zeros
047200011113     C                   IF        vabVCA <> 'EUR'
047300011113     C                   EVAL      vabVCA =  'ITL'
047400011113     C                   ENDIF
047500011113     C                   ENDIF
047600011113     C*
047700011113     C                   IF        vabVMD > *zeros
047800020305     C                   IF        vabVAD <> 'EUR'
047900011113     C                   EVAL      vabVAD =  'ITL'
048000011113     C                   ENDIF
048100011113     C                   ENDIF
048200011113     C*
048300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048400011113     C                   Z-ADD     vabIAS        wrkDec
048500011113     C                   IF        wrkDec > *zeros
048600011113     C                   IF        vabVAS = 'ITL'
048700011113     C                   EVAL      vabIAS = *zeros
048800011113     C                   ENDIF
048900011113     C                   ENDIF
049000011113     C*
049100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
049200011113     C                   Z-ADD     vabCAS        wrkDec
049300011113     C                   IF        wrkDec > *zeros
049400011113     C                   IF        vabVCA = 'ITL'
049500011113     C                   EVAL      vabCAS = *zeros
049600011113     C                   ENDIF
049700011113     C                   ENDIF
049800011113     C*
049900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050000011113     C                   Z-ADD     vabVMD        wrkDec
050100011113     C                   IF        wrkDec > *zeros
050200011113     C                   IF        vabVAD = 'ITL'
050300011113     C                   EVAL      vabVMD = *zeros
050400011113     C                   ENDIF
050500011113     C                   ENDIF
050600011113     C*
050700011113     C                   ENDSR
050800011113     C***
050900011113
051000011113
051100000801
051200000801
051300990920      /TITLE Invio dei dati al punto operativo.
051400010202     C     invio         BEGSR
051500990920     C*
051600021113     C* 1° invio FIVAT
051700010201     C                   reset                   dscmz
051800010201     C                   move      vlrpoi        cmzdst
051900021113     C                   eval      cmzfld = 'FIVATWWR'
052000010201     C                   eval      cmzmbd = vlrhdl
052100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052200021009     C***                if        prmfir = *blanks
052300021113     C                   eval      cmzfla = 'FIVAT00F'
052400021113     C                   eval      cmzmba = 'FIVAT00F'
052500021009     C***                else
052600021009     C***                eval      cmzfla = prmfir
052700021009     C***                eval      cmzmba = prmfir
052800021009     C***                endif
052900010201     C                   eval      cmznrr = *zeros
053000020305     C                   move      §ctrokvt      cmznrr
053100021018     C                   eval      cmzlba = vlrfl1
053200010201     C                   call(e)   'TIS711C'
053300010201     C                   parm                    dscmz
053400010201     C                   parm      *blanks       esito
053500010205     C                   if        %error
053600010205     C                             or cmzerr = '1'
053700010205     C                             or esito  = '1'
053800010205     C                   eval      wrkesito = '3'
053900010205     C                   else
054000010201     C*
054100021113     C* 2° invio FIVAB
054200010201     C                   reset                   dscmz
054300010201     C                   move      vlrpoi        cmzdst
054400010201     C                   eval      cmzfld = vlrfou
054500010201     C                   eval      cmzmbd = vlrhdl
054600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054700021009     C***                if        prmfir = *blanks
054800021113     C                   eval      cmzfla = 'FIVAB00F'
054900021113     C                   eval      cmzmba = 'FIVAB00F'
055000021009     C***                else
055100021009     C***                eval      cmzfla = prmfir
055200021009     C***                eval      cmzmba = prmfir
055300021009     C***                endif
055400010201     C                   eval      cmznrr = *zeros
055500010201     C                   move      §ctrokvb      cmznrr
055600021018     C                   eval      cmzlba = vlrfl1
055700010201     C                   call(e)   'TIS711C'
055800010201     C                   parm                    dscmz
055900010201     C                   parm      *blanks       esito
056000010201     C                   if        %error
056100010201     C                             or cmzerr = '1'
056200010201     C                             or esito  = '1'
056300010201     C                   eval      wrkesito = '3'
056400010201     C                   endif
056500010205     C                   endif
056600990920     C*
056700000613     C                   ENDSR
056800000613     C***
056900070411
057000070411     C     *pssr         BEGSR
057100070411     C*
057200070411     C                   if        %open(tivin00r)
057300070411     C                   close     tivin00r
057400070411     C                   endif
057500070411     C                   if        %open(fivabwwr)
057600070411     C                   close     fivabwwr
057700070411     C                   endif
057800070411     C                   if        %open(fivatwwr)
057900070411     C                   close     fivatwwr
058000070411     C                   endif
058100070411     C*
058200070411     C* Effettuo la chiamata al CLLE preposto
058300070411     C                   call(e)   'TITVVTC'
058400070411     C                   parm                    parccm
058500070411     C                   parm                    parmbr
058600070411     C                   parm      '2'           paropz
058700070411     C*
058800070411     C                   eval      wrkesito = '2'
058900070411     C*
059000070411     C                   seton                                        LR
059100070411     C*
059200070411     C                   ENDSR     '*CANCL'
059300070411     C***
059400070411
059500990910
059600000613     C     *inzsr        BEGSR
059700990910     C*
059800990910     C     *entry        plist
059900990920     C                   parm                    tivlrds
060000990921     C                   parm      wrkesito      esito
060100000724     C                   parm                    prmlit
060200000710     C                   parm                    prmfir
060300000613     C*
060400000830     C* CALCOLA LA DATA CORRENTE
060500100802     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
060600100802     C                   eval      datcor = %dec(%date() : *ISO)
060700000830     C*
060800000613     C                   ENDSR
060900000613     C***
