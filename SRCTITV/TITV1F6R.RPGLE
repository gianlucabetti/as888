000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
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
002500080415     D curSped         s              8    INZ(*blanks)
002600080415     D depSped         s              8    INZ(*blanks)
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
003700010201
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100000613     C*
004200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004300000613     C*
004400010202     C* Effettuo la chiamata al CLLE preposto
004500040506     C                   call(e)   'TITVVTC'
004600010202     C                   parm                    parccm
004700010202     C                   parm                    parmbr
004800010202     C                   parm      '2'           paropz
004900050201     C*
005000050201     C* Effettuo lancio TISI95 solo x chiusura
005100050201     C                   CLEAR                   TISI95DS
005200050201     C                   EVAL      I95TLA = 'C'
005300050201     C                   CALL      'TISI95R'
005400050201     C                   PARM                    TISI95DS
005500000616     C*
005600000801     C
005700010201     C                   seton                                        LR
005800990908
005900000801
006000910830     C*--------------------------------------------------------
006100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006200910830     C*--------------------------------------------------------
006300040526     C     RWFILE        BEGSR
006400990910     C*
006500990914     C                   if        not %open(tivin00r)
006600990908     C                   open      tivin00r
006700990914     C                   endif
006800021113     C                   if        not %open(fivabwwr)
006900021113     C                   open      fivabwwr
007000990914     C                   endif
007100070103     C*
007200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007300020305     C                   exsr      prevat
007400010201     C*
007500010202     C                   if        chkcall = '0'
007600010202     C*
007700021113     C                   if        not %open(fivatwwr)
007800021113     C                   open      fivatwwr
007900010201     C                   endif
008000990910     C*
008100010201     C                   clear                   §CTROKVB          5 0
008200020305     C                   clear                   §CTROKVT          5 0
008300000801     C                   clear                   §CTRMO            5 0
008400000801     C                   clear                   §CTRNO            5 0
008500990910     C*
008600921023     C                   DO        *HIVAL
008700990913     C*
008800990915     C                   READ      tivin00r                               70
008900050627     C                   if        vindta > *blanks
009000000613     C                   add       1             rrnum
009100000801     C*
009200000801     C                   if        *in70 = *off
009300000801     C                             and
009400000801     C                             (vinflg = *blanks
009500000801     C                              or vinflg = '0'
009600000801     C                              or vinflg = '2')
009700000801     C*
009800000801     C                   clear                   vinmsg
009900000801     C                   eval      vinflg = '1'
010000070103     C*
010100070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010200080417     C                   eval      curSped = %subst(vindta:6:8)
010300070213     C                   if        curSped <> depSped
010400070213     C* Se prima bolla => importo bolla corrente
010500070213     C                   if        §CTRMO = *zeros
010600070213     C                   exsr      inzvar
010700070213     C                   exsr      defcam
010800070213     C                   exsr      impvab                                       => carico  VAB
010900070213     C                   else
011000070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
011100070213     C                   exsr      wrivab                                       => scarico VAB
011200070213     C                   exsr      inzvar
011300070213     C                   exsr      defcam
011400070213     C                   exsr      impvab                                       => carico  VAB
011500070213     C                   endif
011600071121     C                   exsr      exevate                                      => write VAT-E
011700070103     C* Salvo il raggruppamento spedizione cliente corrente
011800070103     C                   eval      depSped = curSped
011900070213     C*
012000070213     C* Se collo successivo x stessa bolla
012100070213     C                   else
012200070213     C                   exsr      impvab                                       => carico  VAB
012300070213     C                   exsr      exevate                                      => write VAT-E
012400070103     C                   endif
012500000905     C*
012600000905     C                   else
012700000905     C                   eval      vinflg = '1'
012800050628     C                   endif
012900000905     C                   endif
013000000905     C*
013100000905     C  N70              update    tivin000
013200000905     C*
013300991022     C  N70              ENDdo
013400070213     C*
013500070213     C* Scarico testata bolla rimasta in sospesa
013600070213     C                   exsr      wrivab                                       => scarico VAB
013700010202     C*
013800010202     C                   endif
013900990910
014000990910     C* Se non ci sono record con errori ...
014100000710     C                   if        §ctrno = 0
014200990910     C* ... restituisco esito OK.
014300990921     C                   eval      wrkesito = '0'
014400990910     C                   else
014500010201     C                   if        §ctrokvb > 0
014600990921     C                   eval      wrkesito = '1'
014700000710     C                   else
014800000710     C                   eval      wrkesito = '2'
014900990910     C                   endif
015000000710     C                   endif
015100990910     C*
015200990914     C                   if        %open(tivin00r)
015300990908     C                   close     tivin00r
015400990914     C                   endif
015500021113     C                   if        %open(fivabwwr)
015600021113     C                   close     fivabwwr
015700990914     C                   endif
015800021113     C                   if        %open(fivatwwr)
015900021113     C                   close     fivatwwr
016000010201     C                   endif
016100990910     C*
016200010201     C                   if        §ctrokvb > 0
016300000724     C                             and vlrpoi <> *zeros
016400010202     C                   exsr      invio
016500990920     C                   endif
016600990920     C*
016700910830     C                   ENDSR
016800000613     C***
016900010305
017000010305     C*----------------------------------------------------*
017100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017200010305     C*----------------------------------------------------*
017300020305     C     WRIVAB        BEGSR
017400010305     C*
017500060225     C* Quindi scarico il buffer del file d testata
017600021113     C                   write     fivab000                                     => scarico il VAB
017700010305     C*
017800010305     C                   ENDSR
017900990920
018000000801     C*----------------------------------------------------*
018100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018200000801     C*----------------------------------------------------*
018300010201     C     INZVAR        BEGSR
018400000801     C*
018500070213     C                   CLEAR                   FIVAB000
018600070213     C                   CLEAR                   FIVAT000
018700070213     C*
018800040802     C                   Z-ADD     *zeros        Num5_0            5 0
018900040802     C                   MOVEL     '0'           FlgCAS            1
019000000801     C*
019100000801     C                   ENDSR
019200000801     C*----------------------------------------------------*
019300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019400000801     C*----------------------------------------------------*
019500000801     C     DEFCAM        BEGSR
019600000801     C*
019700020619     C* Imposto i valori di default...
019800080415     C                   Z-ADD     1050579       VABCCM
019900080415     C                   Z-ADD     1050579       VATCCM
020000080415     C                   Z-ADD     105           VABLNP
020100080415     C                   Z-ADD     105           VATLNP
020200080415     C                   Z-ADD     300           VABCTR
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
024800080415     C                   EVAL      VABRSD=%trim(%subst(vindta:39:40))
024900080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025000080409     C     '@':'A'       XLATE     VABRSD        VABRSD
025100080409     C* ==
025200080415     C                   EVAL      VABIND=%trim(%subst(vindta:79:40))
025300080415     C                   EVAL      VABLOD=%trim(%subst(vindta:119:20))
025400080415     C                   EVAL      VABPRD=%trim(%subst(vindta:139:2))
025500080415     C                   EVAL      VABRMA=%trim(%subst(vindta:6:8))
025600080415     C                   EVAL      VABNOT=%trim(%subst(vindta:146:35))
025700080415     C                   EVAL      VABNT2=%trim(%subst(vindta:146+35:60-35))
025800080409     C*
025900080409     C* Reperimento campi NUMERICI
026000080409     C                   MOVEL     DATCOR        VABAAS
026100080409     C                   MOVE      DATCOR        VABMGS
026200080415     C* DCR
026300080415     C                   IF        %subst(vindta:30:1) = 'X'
026400080415     C                   EVAL      PiStr=%trim(%subst(vindta:22:8))
026500080415     C                   EXSR      CHKNUM
026600080415     C                   IF        PiInt=*on
026700080415     C                   Z-ADD     PiVal         VABDCR
026800080415     C                   ELSE
026900080415     C                   ADD       1             errore
027000080415     C                   Z-ADD     *zeros        VABDCR
027100080415     C                   EVAL      vinmsg = %trimr(vinmsg)
027200080415     C                             + ' ' + 'VABDCR'
027300080415     C                   ENDIF
027400080415     C                   ENDIF
027500080409     C* NSP/RMN
027600080415     C                   EVAL      PiStr=%trim(%subst(vindta:6:8))
027700080409     C                   EXSR      CHKNUM
027800080409     C                   IF        PiInt=*on
027900080409     C                   Z-ADD     PiVal         VABNSP
028000080409     C                   Z-ADD     PiVal         VATNSP
028100080409     C                   Z-ADD     PiVal         VABRMN
028200080409     C                   ELSE
028300080409     C                   ADD       1             errore
028400080409     C                   Z-ADD     *zeros        VABNSP
028500080409     C                   Z-ADD     1             VABRMN
028600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
028700080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
028800080409     C                   ENDIF
028900080409     C* CAD
029000080415     C                   EVAL      PiStr=%trim(%subst(vindta:141:5))
029100080409     C                   EXSR      CHKNUM
029200080409     C                   IF        PiInt=*on
029300080409     C                   Z-ADD     PiVal         Num5_0
029400080409     C                   MOVEL(p)  Num5_0        VABCAD
029500080409     C                   ELSE
029600080409     C                   ADD       1             errore
029700080409     C                   EVAL      VABCAD = *zeros
029800080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029900080409     C                             + ' ' + 'VABCAD'
030000080409     C                   ENDIF
030100080409     C* NCL
030200080415     C                   EVAL      PiStr=%trim(%subst(vindta:289:6))
030300080409     C                   EXSR      CHKNUM
030400080409     C                   IF        PiInt=*on
030500080409     C                   Z-ADD     PiVal         VABNCL
030600080409     C                   ELSE
030700080409     C                   ADD       1             errore
030800080409     C                   Z-ADD     *zeros        VABNCL
030900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
031000080409     C                             + ' ' + 'VABNCL'
031100080409     C                   ENDIF
031200080409     C* PKB
031300080415     C                   EVAL      PiStr=%trim(%subst(vindta:295:7))
031400080409     C                   EXSR      CHKNUM
031500080409     C                   IF        PiNum=*on
031600080415     C                   EVAL      PiVal = PiVal / 10                           * gestisco 1 dec.
031700080409     C                   Z-ADD(H)  PiVal         VABPKB
031800080409     C                   ELSE
031900080409     C                   ADD       1             errore
032000080409     C                   Z-ADD     *zeros        VABPKB
032100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032200080409     C                             + ' ' + 'VABPKB'
032300080409     C                   ENDIF
032400080409     C* CAS
032500080415     C                   IF        %trim(%subst(vindta:206:15)) <> *all'0'
032600080409     C                   EVAL      FlgCAS = '1'
032700080415     C                   EVAL      VABVCA = 'EUR'
032800110526     C                   EVAL      VABTIC = 'TM'
032900141029     C                   EVAL      VABTC1 = 'A'
033000080415     C                   EVAL      PiStr=%trim(%subst(vindta:206:15))
033100080409     C                   EXSR      CHKNUM
033200080409     C                   IF        PiNum=*on
033300080415     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
033400080409     C                   Z-ADD     PiVal         VABCAS
033500080409     C                   ELSE
033600080409     C                   ADD       1             errore
033700080409     C                   Z-ADD     *zeros        VABCAS
033800080409     C                   EVAL      vinmsg = %trimr(vinmsg)
033900080409     C                             + ' ' + 'VABCAS'
034000080409     C                   ENDIF
034100080409     C                   ENDIF
034200071121     C*
034300080415     C* Carico l'estensioni A e B del FNVAT
034400080415     C                   exsr      exevata
034500071121     C                   exsr      exevatb
034600010205     C*
034700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034800040802     C                   IF        FlgCAS <> '0'
034900010205     C                   EVAL      VABCBO = '4'
035000010205     C                   ENDIF
035100020305     C*
035200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035300011113     C                   EXSR      CHKIMPDIV
035400010202     C*
035500000801     C* Ebbene...
035600000801     C                   ADD       1             §CTRMO
035700010201     C                   IF        errore <> *zeros
035800000801     C                   ADD       1             §CTRNO
035900000801     C                   EVAL      vinflg = '2'
036000000801     C                   ELSE
036100010201     C                   ADD       1             §CTROKVB
036200000801     C                   ENDIF
036300000801     C*
036400000801     C                   ENDSR
036500070102     C*----------------------------------------------------*
036600070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
036700070102     C*----------------------------------------------------*
036800070103     C     EXEVATE       BEGSR
036900070102     C*
037000080415     C                   Z-ADD     1             wCollo            3 0
037100080415     C                   MOVEL     *zeros        wColloAlfa1       8
037200080415     C                   MOVEL     *zeros        wColloAlfa2       3
037300080415     C*
037400070103     C                   EVAL      VATTRC='E'
037500080415     C                   EVAL      wColloAlfa1=%subst(vindta:6:8)
037600071019     C*
037700080415     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
037800080415     C                   dow       wCollo <= VABNCL and wCollo < 999
037900080415     C                   move(p)   wCollo        wColloAlfa2
038000080415     C                   eval      VATNOT = %trim(wColloAlfa1) + wColloAlfa2
038100070102     C                   exsr      wrivat                                       => scarico VAT
038200080415     C                   add       1             wCollo
038300080415     C                   enddo
038400070102     C*
038500070102     C                   ENDSR
038600080415     C*----------------------------------------------------*
038700080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
038800080415     C*----------------------------------------------------*
038900080415     C     EXEVATA       BEGSR
039000080415     C*
039100080415     C*
039200080415     C***                exsr      wrivat                                       => scarico VAT
039300080415     C*
039400080415     C                   ENDSR
039500071121     C*----------------------------------------------------*
039600071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
039700071121     C*----------------------------------------------------*
039800071121     C     EXEVATB       BEGSR
039900071121     C*
040000071121     C*
040100080409     C***                exsr      wrivat                                       => scarico VAT
040200071121     C*
040300071121     C                   ENDSR
040400010201     C*----------------------------------------------------*
040500040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
040600010201     C*----------------------------------------------------*
040700020305     C     WRIVAT        BEGSR
040800050628     C*
040900060223     C* Scrivo solo se valorizzato qualcosa
041000060223     C                   IF        VATNOT <> *blanks
041100040802     C                   WRITE     FIVAT000
041200060223     C                   ENDIF
041300010201     C*
041400010201     C                   ENDSR
041500010202     C*----------------------------------------------------*
041600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041700010202     C*----------------------------------------------------*
041800020305     C     PREVAT        BEGSR
041900010202     C*
042000021113     C* Compongo il nome del membro da dare al FIVATWWR
042100010202     C                   eval      parmbr = vlrhdl
042200010202     C                   movel     'M'           parmbr
042300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
042400010202     C                   eval      paropz = '1'
042500010202     C* Effettuo la chiamata al CLLE preposto
042600040506     C                   call(e)   'TITVVTC'
042700010202     C                   parm                    parccm
042800010202     C                   parm                    parmbr
042900010202     C                   parm                    paropz
043000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043100010202     C                   if        %error
043200010202     C                   movel     '1'           chkcall
043300010202     C                   else
043400010202     C                   movel     '0'           chkcall
043500010202     C                   endif
043600010202     C*
043700010202     C                   ENDSR
043800000801     C*----------------------------------------------------*
043900000801     C*  CONTROLLO NUMERICITA' CAMPI
044000000801     C*----------------------------------------------------*
044100000801     C     CHKNUM        BEGSR
044200000801     C*
044300000801     C                   call(e)   'ISNUMERIC'
044400000801     C                   PARM                    PiStr            30
044500070213     C                   PARM      ','           PiDecChr          1
044600000801     C                   PARM      *ZEROS        PiVal            30 9
044700000801     C                   PARM      '0'           PiInt             1
044800000801     C                   PARM      '0'           PiNum             1
044900000801     C                   IF        %error
045000000801     C                   EVAL      PiInt=*off
045100000801     C                   ENDIF
045200000801     C*
045300000801     C                   ENDSR
045400000801     C***
045500000801
045600011113
045700011113     C*----------------------------------------------------*
045800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045900011113     C*----------------------------------------------------*
046000011113     C     CHKIMPDIV     BEGSR
046100011113     C*
046200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046300011113     C                   Z-ADD     *zeros        wrkDec            9 9
046400011113     C*
046500011113     C* Come prima cosa effettuo considerazioni sulla divisa
046600011113     C                   IF        vabIAS > *zeros
046700011113     C                   IF        vabVAS <> 'EUR'
046800011113     C                   EVAL      vabVAS =  'ITL'
046900011113     C                   ENDIF
047000011113     C                   ENDIF
047100011113     C*
047200011113     C                   IF        vabCAS > *zeros
047300011113     C                   IF        vabVCA <> 'EUR'
047400011113     C                   EVAL      vabVCA =  'ITL'
047500011113     C                   ENDIF
047600011113     C                   ENDIF
047700011113     C*
047800011113     C                   IF        vabVMD > *zeros
047900020305     C                   IF        vabVAD <> 'EUR'
048000011113     C                   EVAL      vabVAD =  'ITL'
048100011113     C                   ENDIF
048200011113     C                   ENDIF
048300011113     C*
048400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048500011113     C                   Z-ADD     vabIAS        wrkDec
048600011113     C                   IF        wrkDec > *zeros
048700011113     C                   IF        vabVAS = 'ITL'
048800011113     C                   EVAL      vabIAS = *zeros
048900011113     C                   ENDIF
049000011113     C                   ENDIF
049100011113     C*
049200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
049300011113     C                   Z-ADD     vabCAS        wrkDec
049400011113     C                   IF        wrkDec > *zeros
049500011113     C                   IF        vabVCA = 'ITL'
049600011113     C                   EVAL      vabCAS = *zeros
049700011113     C                   ENDIF
049800011113     C                   ENDIF
049900011113     C*
050000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050100011113     C                   Z-ADD     vabVMD        wrkDec
050200011113     C                   IF        wrkDec > *zeros
050300011113     C                   IF        vabVAD = 'ITL'
050400011113     C                   EVAL      vabVMD = *zeros
050500011113     C                   ENDIF
050600011113     C                   ENDIF
050700011113     C*
050800011113     C                   ENDSR
050900011113     C***
051000011113
051100011113
051200000801
051300000801
051400990920      /TITLE Invio dei dati al punto operativo.
051500010202     C     invio         BEGSR
051600990920     C*
051700021113     C* 1° invio FIVAT
051800010201     C                   reset                   dscmz
051900010201     C                   move      vlrpoi        cmzdst
052000021113     C                   eval      cmzfld = 'FIVATWWR'
052100010201     C                   eval      cmzmbd = vlrhdl
052200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052300021009     C***                if        prmfir = *blanks
052400021113     C                   eval      cmzfla = 'FIVAT00F'
052500021113     C                   eval      cmzmba = 'FIVAT00F'
052600021009     C***                else
052700021009     C***                eval      cmzfla = prmfir
052800021009     C***                eval      cmzmba = prmfir
052900021009     C***                endif
053000010201     C                   eval      cmznrr = *zeros
053100020305     C                   move      §ctrokvt      cmznrr
053200021018     C                   eval      cmzlba = vlrfl1
053300010201     C                   call(e)   'TIS711C'
053400010201     C                   parm                    dscmz
053500010201     C                   parm      *blanks       esito
053600010205     C                   if        %error
053700010205     C                             or cmzerr = '1'
053800010205     C                             or esito  = '1'
053900010205     C                   eval      wrkesito = '3'
054000010205     C                   else
054100010201     C*
054200021113     C* 2° invio FIVAB
054300010201     C                   reset                   dscmz
054400010201     C                   move      vlrpoi        cmzdst
054500010201     C                   eval      cmzfld = vlrfou
054600010201     C                   eval      cmzmbd = vlrhdl
054700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054800021009     C***                if        prmfir = *blanks
054900021113     C                   eval      cmzfla = 'FIVAB00F'
055000021113     C                   eval      cmzmba = 'FIVAB00F'
055100021009     C***                else
055200021009     C***                eval      cmzfla = prmfir
055300021009     C***                eval      cmzmba = prmfir
055400021009     C***                endif
055500010201     C                   eval      cmznrr = *zeros
055600010201     C                   move      §ctrokvb      cmznrr
055700021018     C                   eval      cmzlba = vlrfl1
055800010201     C                   call(e)   'TIS711C'
055900010201     C                   parm                    dscmz
056000010201     C                   parm      *blanks       esito
056100010201     C                   if        %error
056200010201     C                             or cmzerr = '1'
056300010201     C                             or esito  = '1'
056400010201     C                   eval      wrkesito = '3'
056500010201     C                   endif
056600010205     C                   endif
056700990920     C*
056800000613     C                   ENDSR
056900000613     C***
057000070411
057100070411     C     *pssr         BEGSR
057200070411     C*
057300070411     C                   if        %open(tivin00r)
057400070411     C                   close     tivin00r
057500070411     C                   endif
057600070411     C                   if        %open(fivabwwr)
057700070411     C                   close     fivabwwr
057800070411     C                   endif
057900070411     C                   if        %open(fivatwwr)
058000070411     C                   close     fivatwwr
058100070411     C                   endif
058200070411     C*
058300070411     C* Effettuo la chiamata al CLLE preposto
058400070411     C                   call(e)   'TITVVTC'
058500070411     C                   parm                    parccm
058600070411     C                   parm                    parmbr
058700070411     C                   parm      '2'           paropz
058800070411     C*
058900070411     C                   eval      wrkesito = '2'
059000070411     C*
059100070411     C                   seton                                        LR
059200070411     C*
059300070411     C                   ENDSR     '*CANCL'
059400070411     C***
059500070411
059600990910
059700000613     C     *inzsr        BEGSR
059800990910     C*
059900990910     C     *entry        plist
060000990920     C                   parm                    tivlrds
060100990921     C                   parm      wrkesito      esito
060200000724     C                   parm                    prmlit
060300000710     C                   parm                    prmfir
060400000613     C*
060500000830     C* CALCOLA LA DATA CORRENTE
060600000830     C                   time                    wn14             14 0
060700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060800000830     C                   z-add     wn8           g08dat
060900000830     C                   z-add     *zeros        g08inv
061000000830     C                   movel     '0'           g08err
061100000830     C                   call      'XSRDA8'
061200000830     C                   parm                    wlbda8
061300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061400000830     C*
061500000613     C                   ENDSR
061600000613     C***
