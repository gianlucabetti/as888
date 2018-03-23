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
002500080604     D curSped         s             10    INZ(*blanks)
002600080604     D depSped         s             10    INZ(*blanks)
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
010200080604     C                   eval      curSped = %subst(vindta:60:10)
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
011600080604     C                   exsr      exevate                                      => write VAT-E
011700070103     C* Salvo il raggruppamento spedizione cliente corrente
011800070103     C                   eval      depSped = curSped
011900070213     C*
012000070213     C* Se collo successivo x stessa bolla
012100070213     C                   else
012200070213     C                   exsr      impvab                                       => carico  VAB
012300080604     C                   exsr      exevate                                      => write VAT-E
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
017600080825     C                   if        vabnsp > *zeros
017700021113     C                   write     fivab000                                     => scarico il VAB
017800080825     C                   endif
017900010305     C*
018000010305     C                   ENDSR
018100990920
018200000801     C*----------------------------------------------------*
018300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018400000801     C*----------------------------------------------------*
018500010201     C     INZVAR        BEGSR
018600000801     C*
018700070213     C                   CLEAR                   FIVAB000
018800070213     C                   CLEAR                   FIVAT000
018900070213     C*
019000040802     C                   Z-ADD     *zeros        Num5_0            5 0
019100040802     C                   MOVEL     '0'           FlgCAS            1
019200000801     C*
019300000801     C                   ENDSR
019400000801     C*----------------------------------------------------*
019500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019600000801     C*----------------------------------------------------*
019700000801     C     DEFCAM        BEGSR
019800000801     C*
019900020619     C* Imposto i valori di default...
020000080604     C                   Z-ADD     0410828       VABCCM
020100080604     C                   Z-ADD     041           VABLNP
020200080604     C                   Z-ADD     000           VABCTR
020300070103     C                   MOVEL     '7Q'          VABCTM
020400040823     C                   MOVEL     '1'           VABCBO
020500020619     C* ... e poi verifico se sono stati passati come parametri
020600020619     C                   IF        vlrppt > *blanks
020700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020900020619     C                   EXSR      CHKNUM
021000020619     C                   IF        PiInt=*on
021100020619     C                   Z-ADD     PiVal         VABCCM
021200020619     C                   ENDIF
021300040506     C                   ENDIF
021400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABLNP
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
023600080604     C* Elaboro solamente i tipi record "Dettagliio"
023700080604     C                   IF        %subst(vindta:1:1) = 'D'
023800080604     C*
023900000801     C                   Z-ADD     *zeros        errore            1 0
024000000830     C                   MOVEL     datcor        VABAAS
024100040526     C                   MOVE      datcor        VABMGS
024200040823     C                   MOVE(P)   vlrpoi        VABFGS
024300080604     C*
024400080604     C* Verifica campi derivati
024500080604     C                   IF        %subst(vindta:41:1) = 'F' OR
024600080604     C                             %subst(vindta:41:1) = 'O'
024700081215     C***                EVAL      VABCCM = 0410829
024800080604     C                   ENDIF
024900080604     C*
025000080604     C                   IF        %trim(%subst(vindta:344:2)) = '3'
025100080604     C                   EVAL      VABCBO = '1'
025200080604     C                   ENDIF
025300080604     C                   IF        %trim(%subst(vindta:344:2)) = '4'
025400080604     C                   EVAL      VABCBO = '2'
025500080604     C                   ENDIF
025600080409     C*
025700080409     C* Reperimento campi ALFA
025800080409     C*
025900080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
026000080604     C                   EVAL      VABRSD=%trim(%subst(vindta:96:35))
026100080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026200080409     C     '@':'A'       XLATE     VABRSD        VABRSD
026300080409     C* ==
026400080604     C                   EVAL      VABRD2=%trim(%subst(vindta:131:35))
026500080604     C                   EVAL      VABIND=%trim(%subst(vindta:166:35))
026600080604     C                   EVAL      VABLOD=%trim(%subst(vindta:201:25))
026700080604     C                   EVAL      VABPRD=%trim(%subst(vindta:226:2))
026800110530     C                   EVAL      VABRMA=%trim(%subst(vindta:454:15))
026900080409     C*
027000080409     C* Reperimento campi NUMERICI
027100080409     C                   MOVEL     DATCOR        VABAAS
027200080409     C                   MOVE      DATCOR        VABMGS
027300080409     C* NSP/RMN
027400080604     C                   EVAL      PiStr=%trim(%subst(vindta:60:10))
027500080409     C                   EXSR      CHKNUM
027600080409     C                   IF        PiInt=*on
027700080409     C                   Z-ADD     PiVal         VABNSP
027800080409     C                   Z-ADD     PiVal         VABRMN
027900080409     C                   ELSE
028000080409     C                   ADD       1             errore
028100080409     C                   Z-ADD     *zeros        VABNSP
028200080409     C                   Z-ADD     1             VABRMN
028300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
028400080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
028500080409     C                   ENDIF
028600080409     C* CAD
028700080604     C                   EVAL      PiStr=%trim(%subst(vindta:261:9))
028800080409     C                   EXSR      CHKNUM
028900080409     C                   IF        PiInt=*on
029000080409     C                   Z-ADD     PiVal         Num5_0
029100080409     C                   MOVEL(p)  Num5_0        VABCAD
029200080409     C                   ELSE
029300080409     C                   ADD       1             errore
029400080409     C                   EVAL      VABCAD = *zeros
029500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
029600080409     C                             + ' ' + 'VABCAD'
029700080409     C                   ENDIF
029800080604     C* NCL - 1
029900080604     C                   EVAL      PiStr=%trim(%subst(vindta:295:5))
030000080409     C                   EXSR      CHKNUM
030100080409     C                   IF        PiInt=*on
030200080604     C                   ADD       PiVal         VABNCL
030300080409     C                   ELSE
030400080409     C                   ADD       1             errore
030500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
030600080409     C                             + ' ' + 'VABNCL'
030700080409     C                   ENDIF
030800080604     C* NCL - 2
030900080604     C                   EVAL      PiStr=%trim(%subst(vindta:305:5))
031000080604     C                   EXSR      CHKNUM
031100080604     C                   IF        PiInt=*on
031200080604     C                   ADD       PiVal         VABNCL
031300080604     C                   ELSE
031400080604     C                   ADD       1             errore
031500080604     C                   EVAL      vinmsg = %trimr(vinmsg)
031600080604     C                             + ' ' + 'VABNCL'
031700080604     C                   ENDIF
031800080604     C* PKB
031900080604     C                   EVAL      PiStr=%trim(%subst(vindta:325:7))
032000080409     C                   EXSR      CHKNUM
032100080409     C                   IF        PiNum=*on
032200080604     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
032300080604     C                   ADD(H)    PiVal         VABPKB
032400080409     C                   ELSE
032500080409     C                   ADD       1             errore
032600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032700080409     C                             + ' ' + 'VABPKB'
032800080409     C                   ENDIF
032900080604     C* VLB
033000080604     C                   EVAL      PiStr=%trim(%subst(vindta:332:12))
033100080604     C                   EXSR      CHKNUM
033200080604     C                   IF        PiNum=*on
033300080604     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
033400080604     C                   ADD(H)    PiVal         VABVLB
033500080604     C                   ELSE
033600080604     C                   ADD       1             errore
033700080604     C                   EVAL      vinmsg = %trimr(vinmsg)
033800080604     C                             + ' ' + 'VABVLB'
033900080604     C                   ENDIF
034000071121     C*
034100080415     C* Carico l'estensioni A e B del FNVAT
034200080415     C                   exsr      exevata
034300071121     C                   exsr      exevatb
034400010205     C*
034500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034600040802     C                   IF        FlgCAS <> '0'
034700010205     C                   ENDIF
034800020305     C*
034900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035000011113     C                   EXSR      CHKIMPDIV
035100010202     C*
035200000801     C* Ebbene...
035300000801     C                   ADD       1             §CTRMO
035400010201     C                   IF        errore <> *zeros
035500000801     C                   ADD       1             §CTRNO
035600000801     C                   EVAL      vinflg = '2'
035700000801     C                   ELSE
035800010201     C                   ADD       1             §CTROKVB
035900000801     C                   ENDIF
036000080604     C*
036100080604     C                   ENDIF
036200000801     C*
036300000801     C                   ENDSR
036400070102     C*----------------------------------------------------*
036500070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
036600070102     C*----------------------------------------------------*
036700070103     C     EXEVATE       BEGSR
036800070102     C*
036900080604     C***                exsr      wrivat                                       => scarico VAT
037000070102     C*
037100070102     C                   ENDSR
037200080415     C*----------------------------------------------------*
037300080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
037400080415     C*----------------------------------------------------*
037500080415     C     EXEVATA       BEGSR
037600080604     C*
037700080604     C                   eval      vatTRC = 'A'
037800080604     C                   eval      vatNOT = '.'
037900080604     C*
038000080604     C                   exsr      wrivat                                       => scarico VAT
038100080415     C*
038200080415     C                   ENDSR
038300071121     C*----------------------------------------------------*
038400071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
038500071121     C*----------------------------------------------------*
038600071121     C     EXEVATB       BEGSR
038700080604     C*
038800080604     C                   eval      vatTRC = 'B'
038900080604     C                   eval      vatNOT = %trim(%subst(vindta:270:20))
039000071121     C*
039100080604     C                   exsr      wrivat                                       => scarico VAT
039200071121     C*
039300071121     C                   ENDSR
039400010201     C*----------------------------------------------------*
039500040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
039600010201     C*----------------------------------------------------*
039700020305     C     WRIVAT        BEGSR
039800080604     C*
039900080604     C                   eval      vatAAS = vabAAS
040000080604     C                   eval      vatLNP = vabLNP
040100080604     C                   eval      vatNRS = vabNRS
040200080604     C                   eval      vatNSP = vabNSP
040300080604     C                   eval      vatCCM = vabCCM
040400080604     C                   eval      vatFGS = vabFGS
040500050628     C*
040600060223     C* Scrivo solo se valorizzato qualcosa
040700080825     C                   IF        VATNOT <> *blanks AND
040800080825     C                             VATNSP  > *zeros
040900040802     C                   WRITE     FIVAT000
041000060223     C                   ENDIF
041100010201     C*
041200010201     C                   ENDSR
041300010202     C*----------------------------------------------------*
041400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041500010202     C*----------------------------------------------------*
041600020305     C     PREVAT        BEGSR
041700010202     C*
041800021113     C* Compongo il nome del membro da dare al FIVATWWR
041900010202     C                   eval      parmbr = vlrhdl
042000010202     C                   movel     'M'           parmbr
042100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
042200010202     C                   eval      paropz = '1'
042300010202     C* Effettuo la chiamata al CLLE preposto
042400040506     C                   call(e)   'TITVVTC'
042500010202     C                   parm                    parccm
042600010202     C                   parm                    parmbr
042700010202     C                   parm                    paropz
042800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042900010202     C                   if        %error
043000010202     C                   movel     '1'           chkcall
043100010202     C                   else
043200010202     C                   movel     '0'           chkcall
043300010202     C                   endif
043400010202     C*
043500010202     C                   ENDSR
043600000801     C*----------------------------------------------------*
043700000801     C*  CONTROLLO NUMERICITA' CAMPI
043800000801     C*----------------------------------------------------*
043900000801     C     CHKNUM        BEGSR
044000000801     C*
044100000801     C                   call(e)   'ISNUMERIC'
044200000801     C                   PARM                    PiStr            30
044300070213     C                   PARM      ','           PiDecChr          1
044400000801     C                   PARM      *ZEROS        PiVal            30 9
044500000801     C                   PARM      '0'           PiInt             1
044600000801     C                   PARM      '0'           PiNum             1
044700000801     C                   IF        %error
044800000801     C                   EVAL      PiInt=*off
044900000801     C                   ENDIF
045000000801     C*
045100000801     C                   ENDSR
045200000801     C***
045300000801
045400011113
045500011113     C*----------------------------------------------------*
045600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045700011113     C*----------------------------------------------------*
045800011113     C     CHKIMPDIV     BEGSR
045900011113     C*
046000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046100011113     C                   Z-ADD     *zeros        wrkDec            9 9
046200011113     C*
046300011113     C* Come prima cosa effettuo considerazioni sulla divisa
046400011113     C                   IF        vabIAS > *zeros
046500011113     C                   IF        vabVAS <> 'EUR'
046600011113     C                   EVAL      vabVAS =  'ITL'
046700011113     C                   ENDIF
046800011113     C                   ENDIF
046900011113     C*
047000011113     C                   IF        vabCAS > *zeros
047100011113     C                   IF        vabVCA <> 'EUR'
047200011113     C                   EVAL      vabVCA =  'ITL'
047300011113     C                   ENDIF
047400011113     C                   ENDIF
047500011113     C*
047600011113     C                   IF        vabVMD > *zeros
047700020305     C                   IF        vabVAD <> 'EUR'
047800011113     C                   EVAL      vabVAD =  'ITL'
047900011113     C                   ENDIF
048000011113     C                   ENDIF
048100011113     C*
048200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048300011113     C                   Z-ADD     vabIAS        wrkDec
048400011113     C                   IF        wrkDec > *zeros
048500011113     C                   IF        vabVAS = 'ITL'
048600011113     C                   EVAL      vabIAS = *zeros
048700011113     C                   ENDIF
048800011113     C                   ENDIF
048900011113     C*
049000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
049100011113     C                   Z-ADD     vabCAS        wrkDec
049200011113     C                   IF        wrkDec > *zeros
049300011113     C                   IF        vabVCA = 'ITL'
049400011113     C                   EVAL      vabCAS = *zeros
049500011113     C                   ENDIF
049600011113     C                   ENDIF
049700011113     C*
049800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049900011113     C                   Z-ADD     vabVMD        wrkDec
050000011113     C                   IF        wrkDec > *zeros
050100011113     C                   IF        vabVAD = 'ITL'
050200011113     C                   EVAL      vabVMD = *zeros
050300011113     C                   ENDIF
050400011113     C                   ENDIF
050500011113     C*
050600011113     C                   ENDSR
050700011113     C***
050800011113
050900011113
051000000801
051100000801
051200990920      /TITLE Invio dei dati al punto operativo.
051300010202     C     invio         BEGSR
051400990920     C*
051500021113     C* 1° invio FIVAT
051600010201     C                   reset                   dscmz
051700010201     C                   move      vlrpoi        cmzdst
051800021113     C                   eval      cmzfld = 'FIVATWWR'
051900010201     C                   eval      cmzmbd = vlrhdl
052000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052100021009     C***                if        prmfir = *blanks
052200021113     C                   eval      cmzfla = 'FIVAT00F'
052300021113     C                   eval      cmzmba = 'FIVAT00F'
052400021009     C***                else
052500021009     C***                eval      cmzfla = prmfir
052600021009     C***                eval      cmzmba = prmfir
052700021009     C***                endif
052800010201     C                   eval      cmznrr = *zeros
052900020305     C                   move      §ctrokvt      cmznrr
053000021018     C                   eval      cmzlba = vlrfl1
053100010201     C                   call(e)   'TIS711C'
053200010201     C                   parm                    dscmz
053300010201     C                   parm      *blanks       esito
053400010205     C                   if        %error
053500010205     C                             or cmzerr = '1'
053600010205     C                             or esito  = '1'
053700010205     C                   eval      wrkesito = '3'
053800010205     C                   else
053900010201     C*
054000021113     C* 2° invio FIVAB
054100010201     C                   reset                   dscmz
054200010201     C                   move      vlrpoi        cmzdst
054300010201     C                   eval      cmzfld = vlrfou
054400010201     C                   eval      cmzmbd = vlrhdl
054500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054600021009     C***                if        prmfir = *blanks
054700021113     C                   eval      cmzfla = 'FIVAB00F'
054800021113     C                   eval      cmzmba = 'FIVAB00F'
054900021009     C***                else
055000021009     C***                eval      cmzfla = prmfir
055100021009     C***                eval      cmzmba = prmfir
055200021009     C***                endif
055300010201     C                   eval      cmznrr = *zeros
055400010201     C                   move      §ctrokvb      cmznrr
055500021018     C                   eval      cmzlba = vlrfl1
055600010201     C                   call(e)   'TIS711C'
055700010201     C                   parm                    dscmz
055800010201     C                   parm      *blanks       esito
055900010201     C                   if        %error
056000010201     C                             or cmzerr = '1'
056100010201     C                             or esito  = '1'
056200010201     C                   eval      wrkesito = '3'
056300010201     C                   endif
056400010205     C                   endif
056500990920     C*
056600000613     C                   ENDSR
056700000613     C***
056800070411
056900070411     C     *pssr         BEGSR
057000070411     C*
057100070411     C                   if        %open(tivin00r)
057200070411     C                   close     tivin00r
057300070411     C                   endif
057400070411     C                   if        %open(fivabwwr)
057500070411     C                   close     fivabwwr
057600070411     C                   endif
057700070411     C                   if        %open(fivatwwr)
057800070411     C                   close     fivatwwr
057900070411     C                   endif
058000070411     C*
058100070411     C* Effettuo la chiamata al CLLE preposto
058200070411     C                   call(e)   'TITVVTC'
058300070411     C                   parm                    parccm
058400070411     C                   parm                    parmbr
058500070411     C                   parm      '2'           paropz
058600070411     C*
058700070411     C                   eval      wrkesito = '2'
058800070411     C*
058900070411     C                   seton                                        LR
059000070411     C*
059100070411     C                   ENDSR     '*CANCL'
059200070411     C***
059300070411
059400990910
059500000613     C     *inzsr        BEGSR
059600990910     C*
059700990910     C     *entry        plist
059800990920     C                   parm                    tivlrds
059900990921     C                   parm      wrkesito      esito
060000000724     C                   parm                    prmlit
060100000710     C                   parm                    prmfir
060200000613     C*
060300000830     C* CALCOLA LA DATA CORRENTE
060400000830     C                   time                    wn14             14 0
060500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060600000830     C                   z-add     wn8           g08dat
060700000830     C                   z-add     *zeros        g08inv
060800000830     C                   movel     '0'           g08err
060900000830     C                   call      'XSRDA8'
061000000830     C                   parm                    wlbda8
061100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061200000830     C*
061300000613     C                   ENDSR
061400000613     C***
