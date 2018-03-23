000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200090113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800090223     Ftitv1n3p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090223     Ftitv1n3ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600060307     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000090223     D curSped         s             13    INZ(*blanks)
004100090223     D depSped         s             13    INZ(*blanks)
004200110705
004300110705     D*------------------
004400110705     D* DS RIDEFINIZIONE BARCODE CLIENTE
004500110705     D*------------------
004600110705     D DS_BARCODE      DS                  INZ
004700110705     D  DS_B_PAR01                   12
004800110705     D  DS_B_PAR02                    4  0
004900110705     D  DS_B_PAR03                    4  0
005000090113
005100090113     D*------------------
005200090113     D* LINKING A DEFINIZIONI ESTERNE
005300090113     D*------------------
005400090113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005500090113     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005600090113
005700010201
005800990915     C                   time                    wrkdata
005900990915     C                   time                    wrkora
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300000724     C*
006400000724     C* SE OCCORRE SPEDIRE IN FILIALE
006500000724     C                   if        vlrpoi <> *zeros
006600000724     C*
006700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006900000724     C     vlrpoi        chain     azorg01l
007000000724     C                   if        %found
007100000616     C                   movel(p)  CMD(1)        depcmd
007200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007300000616     C*
007400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007500011113     C                   Z-ADD     150           LENGH            15 5
007600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007700000616     C                   PARM                    depcmd
007800000616     C                   PARM                    LENGH
007900000724     C*
008000000724     C                   endif
008100000724     C                   endif
008200000616     C*
008300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008400000616     C                   movel(p)  CMD(2)        depcmd
008500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008600000616     C*
008700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008800011113     C                   Z-ADD     150           LENGH            15 5
008900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009000000616     C                   PARM                    depcmd
009100000616     C                   PARM                    LENGH
009200000616     C*
009300090223     C                   if        not %open(titv1n3ps)
009400090223     C                   open      titv1n3ps
009500000616     C                   except    testdett
009600000613     C                   endif
009700000613     C*
009800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010000000613     C*
010100010202     C* Effettuo la chiamata al CLLE preposto
010200040506     C                   call(e)   'TITVVTC'
010300010202     C                   parm                    parccm
010400010202     C                   parm                    parmbr
010500010202     C                   parm      '2'           paropz
010600050201     C*
010700050201     C* Effettuo lancio TISI95 solo x chiusura
010800050201     C                   CLEAR                   TISI95DS
010900050201     C                   EVAL      I95TLA = 'C'
011000050201     C                   CALL      'TISI95R'
011100050201     C                   PARM                    TISI95DS
011200010202     C*
011300090223     C                   if        %open(titv1n3ps)
011400000616     C                   except    findett
011500090223     C                   close     titv1n3ps
011600000613     C                   endif
011700000616     C*
011800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    CMD(3)
012200000616     C                   PARM                    LENGH
012300000616     C*
012400000801     C
012500010201     C                   seton                                        LR
012600000613
012700000613
012800000613     C*--------------------------------------------------------
012900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013000000613     C*--------------------------------------------------------
013100000621     C     STPR          BEGSR
013200000613     C*
013300090223     C                   if        not %open(titv1n3p)
013400090223     C                   open      titv1n3p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900090223     C                   if        %open(titv1n3p)
014000090223     C                   close     titv1n3p
014100990914     C                   endif
014200990910     C*
014300000613     C                   ENDSR
014400000613     C***
014500990908
014600000801
014700910830     C*--------------------------------------------------------
014800040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014900910830     C*--------------------------------------------------------
015000040526     C     RWFILE        BEGSR
015100990910     C*
015200990914     C                   if        not %open(tivin00r)
015300990908     C                   open      tivin00r
015400990914     C                   endif
015500021113     C                   if        not %open(fivabwwr)
015600021113     C                   open      fivabwwr
015700990914     C                   endif
015800070103     C*
015900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016000020305     C                   exsr      prevat
016100010201     C*
016200010202     C                   if        chkcall = '0'
016300010202     C*
016400021113     C                   if        not %open(fivatwwr)
016500021113     C                   open      fivatwwr
016600010201     C                   endif
016700990910     C*
016800010201     C                   clear                   §CTROKVB          5 0
016900020305     C                   clear                   §CTROKVT          5 0
017000000801     C                   clear                   §CTRMO            5 0
017100000801     C                   clear                   §CTRNO            5 0
017200990910     C*
017300921023     C                   DO        *HIVAL
017400990913     C*
017500990915     C                   READ      tivin00r                               70
017600050627     C                   if        vindta > *blanks
017700000613     C                   add       1             rrnum
017800000801     C*
017900000801     C                   if        *in70 = *off
018000000801     C                             and
018100000801     C                             (vinflg = *blanks
018200000801     C                              or vinflg = '0'
018300000801     C                              or vinflg = '2')
018400000801     C*
018500000801     C                   clear                   vinmsg
018600000801     C                   eval      vinflg = '1'
018700070103     C*
018800070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
018900090223     C                   eval      curSped = %subst(vindta:1:13)
019000070213     C                   if        curSped <> depSped
019100070213     C* Se prima bolla => importo bolla corrente
019200070213     C                   if        §CTRMO = *zeros
019300070213     C                   exsr      inzvar
019400070213     C                   exsr      defcam
019500070213     C                   exsr      impvab                                       => carico  VAB
019600070213     C                   else
019700070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
019800070213     C                   exsr      wrivab                                       => scarico VAB
019900070213     C                   exsr      inzvar
020000070213     C                   exsr      defcam
020100070213     C                   exsr      impvab                                       => carico  VAB
020200070213     C                   endif
020300071121     C                   exsr      exevate                                      => write VAT-E
020400070103     C* Salvo il raggruppamento spedizione cliente corrente
020500070103     C                   eval      depSped = curSped
020600070213     C*
020700070213     C* Se collo successivo x stessa bolla
020800070213     C                   else
020900070213     C                   exsr      impvab                                       => carico  VAB
021000070213     C                   exsr      exevate                                      => write VAT-E
021100070103     C                   endif
021200000905     C*
021300000905     C                   else
021400000905     C                   eval      vinflg = '1'
021500050628     C                   endif
021600000905     C                   endif
021700000905     C*
021800000905     C  N70              update    tivin000
021900000905     C*
022000991022     C  N70              ENDdo
022100070213     C*
022200070213     C* Scarico testata bolla rimasta in sospesa
022300070213     C                   exsr      wrivab                                       => scarico VAB
022400010202     C*
022500010202     C                   endif
022600990910
022700990910     C* Se non ci sono record con errori ...
022800000710     C                   if        §ctrno = 0
022900990910     C* ... restituisco esito OK.
023000990921     C                   eval      wrkesito = '0'
023100990910     C                   else
023200010201     C                   if        §ctrokvb > 0
023300990921     C                   eval      wrkesito = '1'
023400000710     C                   else
023500000710     C                   eval      wrkesito = '2'
023600990910     C                   endif
023700000710     C                   endif
023800990910     C*
023900990914     C                   if        %open(tivin00r)
024000990908     C                   close     tivin00r
024100990914     C                   endif
024200021113     C                   if        %open(fivabwwr)
024300021113     C                   close     fivabwwr
024400990914     C                   endif
024500021113     C                   if        %open(fivatwwr)
024600021113     C                   close     fivatwwr
024700010201     C                   endif
024800990910     C*
024900010201     C                   if        §ctrokvb > 0
025000000724     C                             and vlrpoi <> *zeros
025100010202     C                   exsr      invio
025200990920     C                   endif
025300990920     C*
025400910830     C                   ENDSR
025500000613     C***
025600010305
025700010305     C*----------------------------------------------------*
025800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025900010305     C*----------------------------------------------------*
026000020305     C     WRIVAB        BEGSR
026100010305     C*
026200060225     C* Quindi scarico il buffer del file d testata
026300021113     C                   write     fivab000                                     => scarico il VAB
026400010305     C*
026500010305     C                   ENDSR
026600990920
026700000801     C*----------------------------------------------------*
026800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026900000801     C*----------------------------------------------------*
027000010201     C     INZVAR        BEGSR
027100000801     C*
027200070213     C                   CLEAR                   FIVAB000
027300070213     C                   CLEAR                   FIVAT000
027400070213     C*
027500040802     C                   Z-ADD     *zeros        Num5_0            5 0
027600040802     C                   MOVEL     '0'           FlgCAS            1
027700000801     C*
027800000801     C                   ENDSR
027900000801     C*----------------------------------------------------*
028000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028100000801     C*----------------------------------------------------*
028200000801     C     DEFCAM        BEGSR
028300000801     C*
028400020619     C* Imposto i valori di default...
028500090223     C                   Z-ADD     0500821       VABCCM
028600090223     C                   Z-ADD     0500821       VATCCM
028700090223     C                   Z-ADD     050           VABLNP
028800090223     C                   Z-ADD     050           VATLNP
028900090113     C                   Z-ADD     000           VABCTR
029000040823     C                   MOVEL     '1'           VABCBO
029100110705     C                   MOVEL     '7Q'          VABCTM
029200020619     C* ... e poi verifico se sono stati passati come parametri
029300020619     C                   IF        vlrppt > *blanks
029400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029600020619     C                   EXSR      CHKNUM
029700020619     C                   IF        PiInt=*on
029800020619     C                   Z-ADD     PiVal         VABCCM
029900020619     C                   Z-ADD     PiVal         VATCCM
030000020619     C                   ENDIF
030100040506     C                   ENDIF
030200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030400020619     C                   EXSR      CHKNUM
030500020619     C                   IF        PiInt=*on
030600020619     C                   Z-ADD     PiVal         VABLNP
030700020619     C                   Z-ADD     PiVal         VATLNP
030800040506     C                   ENDIF
030900020619     C                   ENDIF
031000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031200020619     C                   EXSR      CHKNUM
031300020619     C                   IF        PiInt=*on
031400020619     C                   Z-ADD     PiVal         VABCTR
031500040506     C                   ENDIF
031600020619     C                   ENDIF
031700020619     C                   ENDIF
031800000801     C*
031900000801     C                   ENDSR
032000000801     C*----------------------------------------------------*
032100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032200000801     C*----------------------------------------------------*
032300040823     C     IMPVAB        BEGSR
032400010305     C*
032500000801     C                   Z-ADD     *zeros        errore            1 0
032600000830     C                   MOVEL     datcor        VABAAS
032700020305     C                   MOVEL     datcor        VATAAS
032800040526     C                   MOVE      datcor        VABMGS
032900040823     C                   MOVE(P)   vlrpoi        VABFGS
033000040823     C                   MOVE(P)   vlrpoi        VATFGS
033100080409     C*
033200080409     C* Reperimento campi ALFA
033300080409     C*
033400080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
033500090223     C                   EVAL      VABRSD=%trim(%subst(vindta:24:30))
033600080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033700080409     C     '@':'A'       XLATE     VABRSD        VABRSD
033800080409     C* ==
033900090223     C                   EVAL      VABRD2=%trim(%subst(vindta:55:30))
034000090223     C                   EVAL      VABIND=%trim(%subst(vindta:86:30))
034100090223     C                   EVAL      VABLOD=%trim(%subst(vindta:122:23))
034200090223     C                   EVAL      VABPRD=%trim(%subst(vindta:145:2))
034300090223     C                   EVAL      VABRMA=%trim(%subst(vindta:1:8))
034400090223     C                   EVAL      VABNOT=%trim(%subst(vindta:169:35))
034500090223     C                   EVAL      VABNT2=%trim(%subst(vindta:205:35))
034600090223     C                   EVAL      VABTC1=%trim(%subst(vindta:255:1))
034700090223     C                   EVAL      VABTSP=%trim(%subst(vindta:274:1))
034800080409     C*
034900080409     C* Reperimento campi NUMERICI
035000090113     C*
035100080409     C* NSP/RMN
035200090223     C                   EVAL      PiStr=%trim(%subst(vindta:9:5))
035300080409     C                   EXSR      CHKNUM
035400080409     C                   IF        PiInt=*on
035500080409     C                   Z-ADD     PiVal         VABNSP
035600080409     C                   Z-ADD     PiVal         VATNSP
035700080409     C                   Z-ADD     PiVal         VABRMN
035800080409     C                   ELSE
035900080409     C                   ADD       1             errore
036000080409     C                   Z-ADD     *zeros        VABNSP
036100080409     C                   Z-ADD     1             VABRMN
036200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
036300080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
036400080409     C                   ENDIF
036500080409     C* CAD
036600090223     C                   EVAL      PiStr=%trim(%subst(vindta:117:5))
036700080409     C                   EXSR      CHKNUM
036800080409     C                   IF        PiInt=*on
036900080409     C                   Z-ADD     PiVal         Num5_0
037000080409     C                   MOVEL(p)  Num5_0        VABCAD
037100080409     C                   ELSE
037200080409     C                   ADD       1             errore
037300080409     C                   EVAL      VABCAD = *zeros
037400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
037500080409     C                             + ' ' + 'VABCAD'
037600080409     C                   ENDIF
037700080409     C* NCL
037800090223     C                   EVAL      PiStr=%trim(%subst(vindta:148:5))
037900080409     C                   EXSR      CHKNUM
038000080409     C                   IF        PiInt=*on
038100080409     C                   Z-ADD     PiVal         VABNCL
038200080409     C                   ELSE
038300080409     C                   ADD       1             errore
038400080409     C                   Z-ADD     *zeros        VABNCL
038500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
038600080409     C                             + ' ' + 'VABNCL'
038700080409     C                   ENDIF
038800080409     C* PKB
038900090223     C                   EVAL      PiStr=%trim(%subst(vindta:154:7))
039000080409     C                   EXSR      CHKNUM
039100080409     C                   IF        PiNum=*on
039200090113     C                   Z-ADD     PiVal         VABPKB
039300080409     C                   ELSE
039400080409     C                   ADD       1             errore
039500080409     C                   Z-ADD     *zeros        VABPKB
039600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
039700080409     C                             + ' ' + 'VABPKB'
039800080409     C                   ENDIF
039900090223     C* VLB
040000090223     C                   EVAL      PiStr=%trim(%subst(vindta:162:6))
040100090223     C                   EXSR      CHKNUM
040200090223     C                   IF        PiNum=*on
040300090223     C                   Z-ADD     PiVal         VABVLB
040400090223     C                   ELSE
040500090223     C                   ADD       1             errore
040600090223     C                   Z-ADD     *zeros        VABVLB
040700090223     C                   EVAL      vinmsg = %trimr(vinmsg)
040800090223     C                             + ' ' + 'VABVLB'
040900090223     C                   ENDIF
041000090223     C* DCR
041100090428     C                   IF        %subst(vindta:241:8) <> *blanks AND
041200090428     C                             %subst(vindta:241:8) <> *zeros
041300090223     C                   EVAL      PiStr=%trim(%subst(vindta:241:8))
041400090223     C                   EXSR      CHKNUM
041500090223     C                   IF        PiInt=*on
041600090223     C                   Z-ADD     PiVal         VABDCR
041700090223     C                   ELSE
041800090223     C                   ADD       1             errore
041900090223     C                   Z-ADD     *zeros        VABDCR
042000090223     C                   EVAL      vinmsg = %trimr(vinmsg)
042100090223     C                             + ' ' + 'VABDCR'
042200090223     C                   ENDIF
042300090428     C                   ENDIF
042400090223     C* HCR
042500090428     C                   IF        %subst(vindta:250:4) <> *blanks AND
042600090428     C                             %subst(vindta:250:4) <> *zeros
042700090223     C                   EVAL      PiStr=%trim(%subst(vindta:250:4))
042800090223     C                   EXSR      CHKNUM
042900090223     C                   IF        PiInt=*on
043000090223     C                   Z-ADD     PiVal         VABHCR
043100090223     C                   ELSE
043200090223     C                   ADD       1             errore
043300090223     C                   Z-ADD     *zeros        VABHCR
043400090223     C                   EVAL      vinmsg = %trimr(vinmsg)
043500090223     C                             + ' ' + 'VABHCR'
043600090223     C                   ENDIF
043700090428     C                   ENDIF
043800071121     C*
043900080415     C* Carico l'estensioni A e B del FNVAT
044000080415     C                   exsr      exevata
044100071121     C                   exsr      exevatb
044200010205     C*
044300010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
044400040802     C                   IF        FlgCAS <> '0'
044500010205     C                   EVAL      VABCBO = '4'
044600010205     C                   ENDIF
044700020305     C*
044800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044900011113     C                   EXSR      CHKIMPDIV
045000010202     C*
045100000801     C* Ebbene...
045200000801     C                   ADD       1             §CTRMO
045300010201     C                   IF        errore <> *zeros
045400000801     C                   ADD       1             §CTRNO
045500000801     C                   EVAL      recko = vindta
045600000801     C                   EXCEPT    rigadett
045700000801     C                   EVAL      vinflg = '2'
045800000801     C                   ELSE
045900010201     C                   ADD       1             §CTROKVB
046000000801     C                   ENDIF
046100000801     C*
046200000801     C                   ENDSR
046300070102     C*----------------------------------------------------*
046400070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
046500070102     C*----------------------------------------------------*
046600070103     C     EXEVATE       BEGSR
046700110705     C*
046800110705     C* Sviluppati i "CHI SONO" in relazione al numero colli
046900110705     C                   eval      DS_BARCODE = %subst(vindta:276:20)
047000110705     C*
047100110705     C                   dow       DS_B_PAR02 <= DS_B_PAR03
047200110705     C                   eval      VATNOT = DS_B_PAR01            +
047300110705     C                                      %editc(DS_B_PAR02:'X')+
047400110705     C                                      %editc(DS_B_PAR03:'X')
047500110705     C                   eval      VATTRC = 'E'
047600110705     C                   exsr      wrivat                                       => scarico VAT
047700110705     C*
047800110705     C                   eval      DS_B_PAR02 = DS_B_PAR02 + 1
047900110705     C                   enddo
048000070102     C*
048100070102     C                   ENDSR
048200080415     C*----------------------------------------------------*
048300080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
048400080415     C*----------------------------------------------------*
048500080415     C     EXEVATA       BEGSR
048600080415     C*
048700080415     C*
048800080415     C***                exsr      wrivat                                       => scarico VAT
048900080415     C*
049000080415     C                   ENDSR
049100071121     C*----------------------------------------------------*
049200071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
049300071121     C*----------------------------------------------------*
049400071121     C     EXEVATB       BEGSR
049500071121     C*
049600090213     C                   EVAL      VATTRC='B'
049700090223     C                   EVAL      VATNOT=%trim(%subst(vindta:257:16))
049800071121     C*
049900090213     C                   exsr      wrivat                                       => scarico VAT
050000071121     C*
050100071121     C                   ENDSR
050200010201     C*----------------------------------------------------*
050300040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
050400010201     C*----------------------------------------------------*
050500020305     C     WRIVAT        BEGSR
050600050628     C*
050700060223     C* Scrivo solo se valorizzato qualcosa
050800060223     C                   IF        VATNOT <> *blanks
050900110705     C                   ADD       1             §CTROKVT
051000040802     C                   WRITE     FIVAT000
051100060223     C                   ENDIF
051200010201     C*
051300010201     C                   ENDSR
051400010202     C*----------------------------------------------------*
051500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051600010202     C*----------------------------------------------------*
051700020305     C     PREVAT        BEGSR
051800010202     C*
051900021113     C* Compongo il nome del membro da dare al FIVATWWR
052000010202     C                   eval      parmbr = vlrhdl
052100010202     C                   movel     'M'           parmbr
052200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
052300010202     C                   eval      paropz = '1'
052400010202     C* Effettuo la chiamata al CLLE preposto
052500040506     C                   call(e)   'TITVVTC'
052600010202     C                   parm                    parccm
052700010202     C                   parm                    parmbr
052800010202     C                   parm                    paropz
052900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053000010202     C                   if        %error
053100010202     C                   movel     '1'           chkcall
053200010202     C                   else
053300010202     C                   movel     '0'           chkcall
053400010202     C                   endif
053500010202     C*
053600010202     C                   ENDSR
053700000801     C*----------------------------------------------------*
053800000801     C*  CONTROLLO NUMERICITA' CAMPI
053900000801     C*----------------------------------------------------*
054000000801     C     CHKNUM        BEGSR
054100090113     C*
054200090113     C                   IF        PiDecChr = *blanks
054300090113     C                   EVAL      PiDecChr = ','
054400090113     C                   ENDIF
054500090113     C*
054600090113     C                   callp(e)  UBISNUM_Check(PiStr
054700090113     C                                          :PiDecChr
054800090113     C                                          :PiVal
054900090113     C                                          :PiNum
055000090113     C                                          :PiInt)
055100090113     C*
055200090113     C                   IF        %error
055300090113     C                   EVAL      PiInt=*off
055400090113     C                   ENDIF
055500000801     C*
055600000801     C                   ENDSR
055700000801     C***
055800000801
055900011113
056000011113     C*----------------------------------------------------*
056100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056200011113     C*----------------------------------------------------*
056300011113     C     CHKIMPDIV     BEGSR
056400011113     C*
056500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056600011113     C                   Z-ADD     *zeros        wrkDec            9 9
056700011113     C*
056800011113     C* Come prima cosa effettuo considerazioni sulla divisa
056900011113     C                   IF        vabIAS > *zeros
057000011113     C                   IF        vabVAS <> 'EUR'
057100011113     C                   EVAL      vabVAS =  'ITL'
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C                   IF        vabCAS > *zeros
057600011113     C                   IF        vabVCA <> 'EUR'
057700011113     C                   EVAL      vabVCA =  'ITL'
057800011113     C                   ENDIF
057900011113     C                   ENDIF
058000011113     C*
058100011113     C                   IF        vabVMD > *zeros
058200020305     C                   IF        vabVAD <> 'EUR'
058300011113     C                   EVAL      vabVAD =  'ITL'
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058800011113     C                   Z-ADD     vabIAS        wrkDec
058900011113     C                   IF        wrkDec > *zeros
059000011113     C                   IF        vabVAS = 'ITL'
059100011113     C                   EVAL      vabIAS = *zeros
059200011113     C                   ENDIF
059300011113     C                   ENDIF
059400011113     C*
059500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
059600011113     C                   Z-ADD     vabCAS        wrkDec
059700011113     C                   IF        wrkDec > *zeros
059800011113     C                   IF        vabVCA = 'ITL'
059900011113     C                   EVAL      vabCAS = *zeros
060000011113     C                   ENDIF
060100011113     C                   ENDIF
060200011113     C*
060300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060400011113     C                   Z-ADD     vabVMD        wrkDec
060500011113     C                   IF        wrkDec > *zeros
060600011113     C                   IF        vabVAD = 'ITL'
060700011113     C                   EVAL      vabVMD = *zeros
060800011113     C                   ENDIF
060900011113     C                   ENDIF
061000011113     C*
061100011113     C                   ENDSR
061200011113     C***
061300011113
061400011113
061500000801
061600000801
061700990920      /TITLE Invio dei dati al punto operativo.
061800010202     C     invio         BEGSR
061900990920     C*
062000021113     C* 1° invio FIVAT
062100010201     C                   reset                   dscmz
062200010201     C                   move      vlrpoi        cmzdst
062300021113     C                   eval      cmzfld = 'FIVATWWR'
062400010201     C                   eval      cmzmbd = vlrhdl
062500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062600021009     C***                if        prmfir = *blanks
062700021113     C                   eval      cmzfla = 'FIVAT00F'
062800021113     C                   eval      cmzmba = 'FIVAT00F'
062900021009     C***                else
063000021009     C***                eval      cmzfla = prmfir
063100021009     C***                eval      cmzmba = prmfir
063200021009     C***                endif
063300010201     C                   eval      cmznrr = *zeros
063400020305     C                   move      §ctrokvt      cmznrr
063500021018     C                   eval      cmzlba = vlrfl1
063600010201     C                   call(e)   'TIS711C'
063700010201     C                   parm                    dscmz
063800010201     C                   parm      *blanks       esito
063900010205     C                   if        %error
064000010205     C                             or cmzerr = '1'
064100010205     C                             or esito  = '1'
064200010205     C                   eval      wrkesito = '3'
064300010205     C                   else
064400010201     C*
064500021113     C* 2° invio FIVAB
064600010201     C                   reset                   dscmz
064700010201     C                   move      vlrpoi        cmzdst
064800010201     C                   eval      cmzfld = vlrfou
064900010201     C                   eval      cmzmbd = vlrhdl
065000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065100021009     C***                if        prmfir = *blanks
065200021113     C                   eval      cmzfla = 'FIVAB00F'
065300021113     C                   eval      cmzmba = 'FIVAB00F'
065400021009     C***                else
065500021009     C***                eval      cmzfla = prmfir
065600021009     C***                eval      cmzmba = prmfir
065700021009     C***                endif
065800010201     C                   eval      cmznrr = *zeros
065900010201     C                   move      §ctrokvb      cmznrr
066000021018     C                   eval      cmzlba = vlrfl1
066100010201     C                   call(e)   'TIS711C'
066200010201     C                   parm                    dscmz
066300010201     C                   parm      *blanks       esito
066400010201     C                   if        %error
066500010201     C                             or cmzerr = '1'
066600010201     C                             or esito  = '1'
066700010201     C                   eval      wrkesito = '3'
066800010201     C                   endif
066900010205     C                   endif
067000990920     C*
067100000613     C                   ENDSR
067200000613     C***
067300070411
067400070411     C     *pssr         BEGSR
067500070411     C*
067600070411     C                   if        %open(tivin00r)
067700070411     C                   close     tivin00r
067800070411     C                   endif
067900070411     C                   if        %open(fivabwwr)
068000070411     C                   close     fivabwwr
068100070411     C                   endif
068200070411     C                   if        %open(fivatwwr)
068300070411     C                   close     fivatwwr
068400070411     C                   endif
068500070411     C*
068600070411     C* Effettuo la chiamata al CLLE preposto
068700070411     C                   call(e)   'TITVVTC'
068800070411     C                   parm                    parccm
068900070411     C                   parm                    parmbr
069000070411     C                   parm      '2'           paropz
069100070411     C*
069200070411     C                   eval      wrkesito = '2'
069300070411     C*
069400070411     C                   seton                                        LR
069500070411     C*
069600070411     C                   ENDSR     '*CANCL'
069700070411     C***
069800070411
069900990910
070000000613     C     *inzsr        BEGSR
070100990910     C*
070200990910     C     *entry        plist
070300990920     C                   parm                    tivlrds
070400990921     C                   parm      wrkesito      esito
070500000724     C                   parm                    prmlit
070600000710     C                   parm                    prmfir
070700000613     C*
070800000830     C* CALCOLA LA DATA CORRENTE
070900110705     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
071000110705     C                   eval      datcor = %dec(%date : *ISO)
071100000830     C*
071200000613     C                   ENDSR
071300000613     C***
071400990908
071500090223     Otitv1n3p  E            riepilogo         2
071600990915     O                                              'Upload via Internet'
071700990915     O                                           +1 'Traduzione TIVIN00R -'
071800021113     O                                           +1 'FIVABWWR/FIVATWWR'
071900010201     O                                           +1 'Report testate bolle'
072000990915     O          E            riepilogo   2
072100990915     O                       wrkdata
072200990915     O                       wrkora              +1
072300990915     O                       procname            +1
072400990915     O          E            riepilogo   2
072500990915     O                                              'Cliente..................:'
072600990915     O                       VABCCM        z     +1
072700990915     O          E            riepilogo   2
072800990920     O                                              'Riferimento Strategi.....:'
072900990920     O                       vlrhdl              +1
073000990915     O          E            riepilogo   2
073100990915     O                                              'Giusti...................:'
073200010201     O                       §CTROKVB      2   +  1
073300990915     O          E            riepilogo   2
073400990915     O                                              'Sbagliati e corretti.....:'
073500971022     O                       §CTRMO        2   +  1
073600990915     O          E            riepilogo   2
073700990915     O                                              'Sbagliati e scartati.....:'
073800971022     O                       §CTRNO        2   +  1
073900000613
074000090223     Otitv1n3ps E            testdett          2
074100000613     O                                              'Upload via Internet'
074200000613     O                                           +1 'Traduzione TIVIN00R -'
074300021113     O                                           +1 'FIVABWWR/FIVATWWR'
074400010201     O                                           +1 'Report testate bolle'
074500000616     O          E            testdett    3
074600000613     O                                           +2 'N° rec'
074700000613     O                                           +3 'Anteprima contenuto'
074800000616     O          E            rigadett    2
074900000613     O                       rrnum               +2
075000000621     O                       recko               +3
075100000616     O          E            findett     2
075200000613     O                       wrkdata
075300000613     O                       wrkora              +1
075400000613     O                       procname            +1
075500000616     O          E            findett     2
075600000613     O                                              'Cliente..................:'
075700000613     O                       VABCCM        z     +1
075800000616     O          E            findett     2
075900000613     O                                              'Riferimento Strategi.....:'
076000000613     O                       vlrhdl              +1
076100000616     O          E            findett     2
076200000613     O                                              'Giusti...................:'
076300010201     O                       §CTROKVB      2   +  1
076400000616     O          E            findett     2
076500000613     O                                              'Sbagliati e corretti.....:'
076600000613     O                       §CTRMO        2   +  1
076700000616     O          E            findett     2
076800000613     O                                              'Sbagliati e scartati.....:'
076900000613     O                       §CTRNO        2   +  1
077000000512** CMD - COMANDI CL
077100090223OVRPRTF FILE(TITV1N3P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
077200090223OVRPRTF FILE(TITV1N3PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
077300090223DLTOVR FILE(TITV1N3P TITV1N3PS) LVL(*)
077400000512
077500000512
