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
002500090113     D curSped         s             10    INZ(*blanks)
002600090113     D depSped         s             10    INZ(*blanks)
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
010800090213     C                   eval      curSped = %subst(vindta:118:10)
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
020400090213     C                   Z-ADD     0054911       VABCCM
020500090213     C                   Z-ADD     0054911       VATCCM
020600090213     C                   Z-ADD     005           VABLNP
020700090213     C                   Z-ADD     005           VATLNP
020800130304     C                   Z-ADD     300           VABCTR
020900070103     C                   MOVEL     '7Q'          VABCTM
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
026300100622     C                   EVAL      VABTSP=%trim(%subst(vindta:78:1))
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
028000090213     C                   EVAL      PiStr=%trim(%subst(vindta:118:10))
028100080409     C                   EXSR      CHKNUM
028200080409     C                   IF        PiInt=*on
028300080409     C                   Z-ADD     PiVal         VABNSP
028400080409     C                   Z-ADD     PiVal         VATNSP
028500080409     C                   Z-ADD     PiVal         VABRMN
028600080409     C                   ELSE
028700080409     C                   ADD       1             errore
028800080409     C                   Z-ADD     *zeros        VABNSP
028900080409     C                   Z-ADD     1             VABRMN
029000080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029100080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
029200080409     C                   ENDIF
029300080409     C* CAD
029400090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
029500080409     C                   EXSR      CHKNUM
029600080409     C                   IF        PiInt=*on
029700080409     C                   Z-ADD     PiVal         Num5_0
029800080409     C                   MOVEL(p)  Num5_0        VABCAD
029900080409     C                   ELSE
030000080409     C                   ADD       1             errore
030100080409     C                   EVAL      VABCAD = *zeros
030200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
030300080409     C                             + ' ' + 'VABCAD'
030400080409     C                   ENDIF
030500080409     C* NCL
030600090213     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
030700080409     C                   EXSR      CHKNUM
030800080409     C                   IF        PiInt=*on
030900080409     C                   Z-ADD     PiVal         VABNCL
031000080409     C                   ELSE
031100080409     C                   ADD       1             errore
031200080409     C                   Z-ADD     *zeros        VABNCL
031300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031400080409     C                             + ' ' + 'VABNCL'
031500080409     C                   ENDIF
031600080409     C* PKB
031700090213     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
031800080409     C                   EXSR      CHKNUM
031900080409     C                   IF        PiNum=*on
032000090213     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
032100090113     C                   Z-ADD     PiVal         VABPKB
032200080409     C                   ELSE
032300080409     C                   ADD       1             errore
032400080409     C                   Z-ADD     *zeros        VABPKB
032500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032600080409     C                             + ' ' + 'VABPKB'
032700080409     C                   ENDIF
032800090213     C* IAS
032900090213     C                   IF        %trim(%subst(vindta:82:13)) <> *blanks
033000090213     C                   EVAL      VABVCA = %trim(%subst(vindta:95:3))
033100090213     C                   EVAL      PiStr=%trim(%subst(vindta:82:13))
033200080409     C                   EXSR      CHKNUM
033300080409     C                   IF        PiNum=*on
033400090213     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
033500090213     C                   Z-ADD     PiVal         VABIAS
033600080409     C                   ELSE
033700080409     C                   ADD       1             errore
033800090213     C                   Z-ADD     *zeros        VABIAS
033900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
034000090213     C                             + ' ' + 'VABIAS'
034100080409     C                   ENDIF
034200080409     C                   ENDIF
034300071121     C*
034400080415     C* Carico l'estensioni A e B del FNVAT
034500080415     C                   exsr      exevata
034600071121     C                   exsr      exevatb
034700010205     C*
034800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034900040802     C                   IF        FlgCAS <> '0'
035000010205     C                   EVAL      VABCBO = '4'
035100010205     C                   ENDIF
035200020305     C*
035300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035400011113     C                   EXSR      CHKIMPDIV
035500010202     C*
035600000801     C* Ebbene...
035700000801     C                   ADD       1             §CTRMO
035800010201     C                   IF        errore <> *zeros
035900000801     C                   ADD       1             §CTRNO
036000000801     C                   EVAL      vinflg = '2'
036100000801     C                   ELSE
036200010201     C                   ADD       1             §CTROKVB
036300000801     C                   ENDIF
036400000801     C*
036500000801     C                   ENDSR
036600070102     C*----------------------------------------------------*
036700070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
036800070102     C*----------------------------------------------------*
036900070103     C     EXEVATE       BEGSR
037000070102     C*
037100090113     C                   Z-ADD     1             wCollo            2 0
037200090213     C                   MOVEL     *zeros        wColloAlfa1      12
037300080415     C*
037400070103     C                   EVAL      VATTRC='E'
037500090213     C                   EVAL      wColloAlfa1=%subst(vindta:118:10)
037600071019     C*
037700080415     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
037800090113     C                   dow       wCollo <= VABNCL and wCollo < 99
037900090219     C                   eval      VATNOT = %trim(wColloAlfa1) + %char(wCollo)
038000070102     C                   exsr      wrivat                                       => scarico VAT
038100080415     C                   add       1             wCollo
038200080415     C                   enddo
038300070102     C*
038400070102     C                   ENDSR
038500080415     C*----------------------------------------------------*
038600080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
038700080415     C*----------------------------------------------------*
038800080415     C     EXEVATA       BEGSR
038900080415     C*
039000080415     C*
039100080415     C***                exsr      wrivat                                       => scarico VAT
039200080415     C*
039300080415     C                   ENDSR
039400071121     C*----------------------------------------------------*
039500071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
039600071121     C*----------------------------------------------------*
039700071121     C     EXEVATB       BEGSR
039800071121     C*
039900090213     C                   EVAL      VATTRC='B'
040000090213     C                   EVAL      VATNOT=%trim(%subst(vindta:652:7))
040100071121     C*
040200090213     C                   exsr      wrivat                                       => scarico VAT
040300071121     C*
040400071121     C                   ENDSR
040500010201     C*----------------------------------------------------*
040600040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
040700010201     C*----------------------------------------------------*
040800020305     C     WRIVAT        BEGSR
040900050628     C*
041000060223     C* Scrivo solo se valorizzato qualcosa
041100060223     C                   IF        VATNOT <> *blanks
041200040802     C                   WRITE     FIVAT000
041300060223     C                   ENDIF
041400010201     C*
041500010201     C                   ENDSR
041600010202     C*----------------------------------------------------*
041700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041800010202     C*----------------------------------------------------*
041900020305     C     PREVAT        BEGSR
042000010202     C*
042100021113     C* Compongo il nome del membro da dare al FIVATWWR
042200010202     C                   eval      parmbr = vlrhdl
042300010202     C                   movel     'M'           parmbr
042400050627     C                   eval      parccm = %subst(vlrKSC:2:7)
042500010202     C                   eval      paropz = '1'
042600010202     C* Effettuo la chiamata al CLLE preposto
042700040506     C                   call(e)   'TITVVTC'
042800010202     C                   parm                    parccm
042900010202     C                   parm                    parmbr
043000010202     C                   parm                    paropz
043100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043200010202     C                   if        %error
043300010202     C                   movel     '1'           chkcall
043400010202     C                   else
043500010202     C                   movel     '0'           chkcall
043600010202     C                   endif
043700010202     C*
043800010202     C                   ENDSR
043900000801     C*----------------------------------------------------*
044000000801     C*  CONTROLLO NUMERICITA' CAMPI
044100000801     C*----------------------------------------------------*
044200000801     C     CHKNUM        BEGSR
044300090113     C*
044400090113     C                   IF        PiDecChr = *blanks
044500090113     C                   EVAL      PiDecChr = ','
044600090113     C                   ENDIF
044700090113     C*
044800090113     C                   callp(e)  UBISNUM_Check(PiStr
044900090113     C                                          :PiDecChr
045000090113     C                                          :PiVal
045100090113     C                                          :PiNum
045200090113     C                                          :PiInt)
045300090113     C*
045400090113     C                   IF        %error
045500090113     C                   EVAL      PiInt=*off
045600090113     C                   ENDIF
045700000801     C*
045800000801     C                   ENDSR
045900000801     C***
046000000801
046100011113
046200011113     C*----------------------------------------------------*
046300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046400011113     C*----------------------------------------------------*
046500011113     C     CHKIMPDIV     BEGSR
046600011113     C*
046700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046800011113     C                   Z-ADD     *zeros        wrkDec            9 9
046900011113     C*
047000011113     C* Come prima cosa effettuo considerazioni sulla divisa
047100011113     C                   IF        vabIAS > *zeros
047200011113     C                   IF        vabVAS <> 'EUR'
047300011113     C                   EVAL      vabVAS =  'ITL'
047400011113     C                   ENDIF
047500011113     C                   ENDIF
047600011113     C*
047700011113     C                   IF        vabCAS > *zeros
047800011113     C                   IF        vabVCA <> 'EUR'
047900011113     C                   EVAL      vabVCA =  'ITL'
048000011113     C                   ENDIF
048100011113     C                   ENDIF
048200011113     C*
048300011113     C                   IF        vabVMD > *zeros
048400020305     C                   IF        vabVAD <> 'EUR'
048500011113     C                   EVAL      vabVAD =  'ITL'
048600011113     C                   ENDIF
048700011113     C                   ENDIF
048800011113     C*
048900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049000011113     C                   Z-ADD     vabIAS        wrkDec
049100011113     C                   IF        wrkDec > *zeros
049200011113     C                   IF        vabVAS = 'ITL'
049300011113     C                   EVAL      vabIAS = *zeros
049400011113     C                   ENDIF
049500011113     C                   ENDIF
049600011113     C*
049700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
049800011113     C                   Z-ADD     vabCAS        wrkDec
049900011113     C                   IF        wrkDec > *zeros
050000011113     C                   IF        vabVCA = 'ITL'
050100011113     C                   EVAL      vabCAS = *zeros
050200011113     C                   ENDIF
050300011113     C                   ENDIF
050400011113     C*
050500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050600011113     C                   Z-ADD     vabVMD        wrkDec
050700011113     C                   IF        wrkDec > *zeros
050800011113     C                   IF        vabVAD = 'ITL'
050900011113     C                   EVAL      vabVMD = *zeros
051000011113     C                   ENDIF
051100011113     C                   ENDIF
051200011113     C*
051300011113     C                   ENDSR
051400011113     C***
051500011113
051600011113
051700000801
051800000801
051900990920      /TITLE Invio dei dati al punto operativo.
052000010202     C     invio         BEGSR
052100990920     C*
052200021113     C* 1° invio FIVAT
052300010201     C                   reset                   dscmz
052400010201     C                   move      vlrpoi        cmzdst
052500021113     C                   eval      cmzfld = 'FIVATWWR'
052600010201     C                   eval      cmzmbd = vlrhdl
052700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052800021009     C***                if        prmfir = *blanks
052900021113     C                   eval      cmzfla = 'FIVAT00F'
053000021113     C                   eval      cmzmba = 'FIVAT00F'
053100021009     C***                else
053200021009     C***                eval      cmzfla = prmfir
053300021009     C***                eval      cmzmba = prmfir
053400021009     C***                endif
053500010201     C                   eval      cmznrr = *zeros
053600020305     C                   move      §ctrokvt      cmznrr
053700021018     C                   eval      cmzlba = vlrfl1
053800010201     C                   call(e)   'TIS711C'
053900010201     C                   parm                    dscmz
054000010201     C                   parm      *blanks       esito
054100010205     C                   if        %error
054200010205     C                             or cmzerr = '1'
054300010205     C                             or esito  = '1'
054400010205     C                   eval      wrkesito = '3'
054500010205     C                   else
054600010201     C*
054700021113     C* 2° invio FIVAB
054800010201     C                   reset                   dscmz
054900010201     C                   move      vlrpoi        cmzdst
055000010201     C                   eval      cmzfld = vlrfou
055100010201     C                   eval      cmzmbd = vlrhdl
055200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055300021009     C***                if        prmfir = *blanks
055400021113     C                   eval      cmzfla = 'FIVAB00F'
055500021113     C                   eval      cmzmba = 'FIVAB00F'
055600021009     C***                else
055700021009     C***                eval      cmzfla = prmfir
055800021009     C***                eval      cmzmba = prmfir
055900021009     C***                endif
056000010201     C                   eval      cmznrr = *zeros
056100010201     C                   move      §ctrokvb      cmznrr
056200021018     C                   eval      cmzlba = vlrfl1
056300010201     C                   call(e)   'TIS711C'
056400010201     C                   parm                    dscmz
056500010201     C                   parm      *blanks       esito
056600010201     C                   if        %error
056700010201     C                             or cmzerr = '1'
056800010201     C                             or esito  = '1'
056900010201     C                   eval      wrkesito = '3'
057000010201     C                   endif
057100010205     C                   endif
057200990920     C*
057300000613     C                   ENDSR
057400000613     C***
057500070411
057600070411     C     *pssr         BEGSR
057700070411     C*
057800070411     C                   if        %open(tivin00r)
057900070411     C                   close     tivin00r
058000070411     C                   endif
058100070411     C                   if        %open(fivabwwr)
058200070411     C                   close     fivabwwr
058300070411     C                   endif
058400070411     C                   if        %open(fivatwwr)
058500070411     C                   close     fivatwwr
058600070411     C                   endif
058700070411     C*
058800070411     C* Effettuo la chiamata al CLLE preposto
058900070411     C                   call(e)   'TITVVTC'
059000070411     C                   parm                    parccm
059100070411     C                   parm                    parmbr
059200070411     C                   parm      '2'           paropz
059300070411     C*
059400070411     C                   eval      wrkesito = '2'
059500070411     C*
059600070411     C                   seton                                        LR
059700070411     C*
059800070411     C                   ENDSR     '*CANCL'
059900070411     C***
060000070411
060100990910
060200000613     C     *inzsr        BEGSR
060300990910     C*
060400990910     C     *entry        plist
060500990920     C                   parm                    tivlrds
060600990921     C                   parm      wrkesito      esito
060700000724     C                   parm                    prmlit
060800000710     C                   parm                    prmfir
060900000613     C*
061000000830     C* CALCOLA LA DATA CORRENTE
061100000830     C                   time                    wn14             14 0
061200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061300000830     C                   z-add     wn8           g08dat
061400000830     C                   z-add     *zeros        g08inv
061500000830     C                   movel     '0'           g08err
061600000830     C                   call      'XSRDA8'
061700000830     C                   parm                    wlbda8
061800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061900000830     C*
062000000613     C                   ENDSR
062100000613     C***
