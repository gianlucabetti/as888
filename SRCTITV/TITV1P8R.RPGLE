000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090114     FFIVABwwr  O    E             DISK    usropn
000700090114     FFIVATwwr  O    E             DISK    usropn
000800090519     Ftitv1p8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090519     Ftitv1p8ps O    f  198        PRINTER usropn
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
004000090519     D curSped         s              6    INZ(*blanks)
004100090519     D depSped         s              6    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100090114
005200081217
005300081217     D*------------------
005400081217     D* LINKING A DEFINIZIONI ESTERNE
005500081217     D*------------------
005600081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005700081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005800081217
005900081217
006000010201
006100081217
006200081217     C
006300990915     C                   time                    wrkdata
006400990915     C                   time                    wrkora
006500000913     C                   reset                   rrnum
006600990921     C                   reset                   esito
006700990921     C                   reset                   wrkesito
006800000724     C*
006900000724     C* SE OCCORRE SPEDIRE IN FILIALE
007000000724     C                   if        vlrpoi <> *zeros
007100000724     C*
007200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007400000724     C     vlrpoi        chain     azorg01l
007500000724     C                   if        %found
007600000616     C                   movel(p)  CMD(1)        depcmd
007700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007800000616     C*
007900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008000011113     C                   Z-ADD     150           LENGH            15 5
008100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200000616     C                   PARM                    depcmd
008300000616     C                   PARM                    LENGH
008400000724     C*
008500000724     C                   endif
008600000724     C                   endif
008700000616     C*
008800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008900000616     C                   movel(p)  CMD(2)        depcmd
009000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009100000616     C*
009200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009300011113     C                   Z-ADD     150           LENGH            15 5
009400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009500000616     C                   PARM                    depcmd
009600000616     C                   PARM                    LENGH
009700000616     C*
009800090519     C                   if        not %open(titv1p8ps)
009900090519     C                   open      titv1p8ps
010000000616     C                   except    testdett
010100000613     C                   endif
010200000613     C*
010300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010500000613     C*
010600010202     C* Effettuo la chiamata al CLLE preposto
010700090114     C                   call(e)   'TITVVTC'
010800010202     C                   parm                    parccm
010900010202     C                   parm                    parmbr
011000010202     C                   parm      '2'           paropz
011100050201     C*
011200050201     C* Effettuo lancio TISI95 solo x chiusura
011300050201     C                   CLEAR                   TISI95DS
011400050201     C                   EVAL      I95TLA = 'C'
011500050201     C                   CALL      'TISI95R'
011600050201     C                   PARM                    TISI95DS
011700010202     C*
011800090519     C                   if        %open(titv1p8ps)
011900000616     C                   except    findett
012000090519     C                   close     titv1p8ps
012100000613     C                   endif
012200000616     C*
012300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012400011113     C                   Z-ADD     150           LENGH            15 5
012500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    CMD(3)
012700000616     C                   PARM                    LENGH
012800000616     C*
012900000801     C
013000010201     C                   seton                                        LR
013100000613
013200000613
013300000613     C*--------------------------------------------------------
013400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013500000613     C*--------------------------------------------------------
013600000621     C     STPR          BEGSR
013700000613     C*
013800090519     C                   if        not %open(titv1p8p)
013900090519     C                   open      titv1p8p
014000990915     C                   endif
014100990915     C*
014200990915     C                   except    riepilogo
014300990915     C*
014400090519     C                   if        %open(titv1p8p)
014500090519     C                   close     titv1p8p
014600990914     C                   endif
014700990910     C*
014800000613     C                   ENDSR
014900000613     C***
015000990908
015100000801
015200910830     C*--------------------------------------------------------
015300090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015400910830     C*--------------------------------------------------------
015500040526     C     RWFILE        BEGSR
015600990910     C*
015700990914     C                   if        not %open(tivin00r)
015800990908     C                   open      tivin00r
015900990914     C                   endif
016000090114     C                   if        not %open(fivabwwr)
016100090114     C                   open      fivabwwr
016200990914     C                   endif
016300070103     C*
016400090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016500020305     C                   exsr      prevat
016600010201     C*
016700010202     C                   if        chkcall = '0'
016800010202     C*
016900090114     C                   if        not %open(fivatwwr)
017000090114     C                   open      fivatwwr
017100010201     C                   endif
017200990910     C*
017300010201     C                   clear                   §CTROKVB          5 0
017400020305     C                   clear                   §CTROKVT          5 0
017500000801     C                   clear                   §CTRMO            5 0
017600000801     C                   clear                   §CTRNO            5 0
017700990910     C*
017800921023     C                   DO        *HIVAL
017900990913     C*
018000990915     C                   READ      tivin00r                               70
018100050627     C                   if        vindta > *blanks
018200000613     C                   add       1             rrnum
018300000801     C*
018400000801     C                   if        *in70 = *off
018500000801     C                             and
018600000801     C                             (vinflg = *blanks
018700000801     C                              or vinflg = '0'
018800000801     C                              or vinflg = '2')
018900000801     C*
019000000801     C                   clear                   vinmsg
019100000801     C                   eval      vinflg = '1'
019200070103     C*
019300070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
019400090519     C                   eval      curSped = %subst(vindta:1:6)
019500070213     C                   if        curSped <> depSped
019600070213     C* Se prima bolla => importo bolla corrente
019700070213     C                   if        §CTRMO = *zeros
019800090309     C                   EXSR      INZVAR
019900090309     C                   EXSR      DEFCAM
020000070213     C                   exsr      impvab                                       => carico  VAB
020100070213     C                   else
020200070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
020300070213     C                   exsr      wrivab                                       => scarico VAB
020400090309     C                   EXSR      INZVAR
020500090309     C                   EXSR      DEFCAM
020600070213     C                   exsr      impvab                                       => carico  VAB
020700070213     C                   endif
020800071121     C                   exsr      exevate                                      => write VAT-E
020900070103     C* Salvo il raggruppamento spedizione cliente corrente
021000070103     C                   eval      depSped = curSped
021100070213     C*
021200070213     C* Se collo successivo x stessa bolla
021300070213     C                   else
021400070213     C                   exsr      impvab                                       => carico  VAB
021500070213     C                   exsr      exevate                                      => write VAT-E
021600070103     C                   endif
021700000905     C*
021800000905     C                   else
021900000905     C                   eval      vinflg = '1'
022000050628     C                   endif
022100000905     C                   endif
022200000905     C*
022300000905     C  N70              update    tivin000
022400000905     C*
022500991022     C  N70              ENDdo
022600070213     C*
022700070213     C* Scarico testata bolla rimasta in sospesa
022800070213     C                   exsr      wrivab                                       => scarico VAB
022900010202     C*
023000010202     C                   endif
023100990910
023200990910     C* Se non ci sono record con errori ...
023300000710     C                   if        §ctrno = 0
023400990910     C* ... restituisco esito OK.
023500990921     C                   eval      wrkesito = '0'
023600990910     C                   else
023700010201     C                   if        §ctrokvb > 0
023800990921     C                   eval      wrkesito = '1'
023900000710     C                   else
024000000710     C                   eval      wrkesito = '2'
024100990910     C                   endif
024200000710     C                   endif
024300990910     C*
024400990914     C                   if        %open(tivin00r)
024500990908     C                   close     tivin00r
024600990914     C                   endif
024700090114     C                   if        %open(fivabwwr)
024800090114     C                   close     fivabwwr
024900990914     C                   endif
025000090114     C                   if        %open(fivatwwr)
025100090114     C                   close     fivatwwr
025200010201     C                   endif
025300990910     C*
025400010201     C                   if        §ctrokvb > 0
025500000724     C                             and vlrpoi <> *zeros
025600010202     C                   exsr      invio
025700990920     C                   endif
025800990920     C*
025900910830     C                   ENDSR
026000000613     C***
026100010305
026200010305     C*----------------------------------------------------*
026300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026400010305     C*----------------------------------------------------*
026500020305     C     WRIVAB        BEGSR
026600010305     C*
026700060225     C* Quindi scarico il buffer del file d testata
026800090114     C                   write     fivab000                                     => scarico il VAB
026900010305     C*
027000010305     C                   ENDSR
027100990920
027200000801     C*----------------------------------------------------*
027300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027400000801     C*----------------------------------------------------*
027500010201     C     INZVAR        BEGSR
027600000801     C*
027700090114     C                   CLEAR                   FIVAB000
027800090114     C                   CLEAR                   FIVAT000
027900070213     C*
028000040802     C                   Z-ADD     *zeros        Num5_0            5 0
028100040802     C                   MOVEL     '0'           FlgCAS            1
028200000801     C*
028300000801     C                   ENDSR
028400000801     C*----------------------------------------------------*
028500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028600000801     C*----------------------------------------------------*
028700000801     C     DEFCAM        BEGSR
028800000801     C*
028900020619     C* Imposto i valori di default...
029000090519     C                   Z-ADD     0501688       VABCCM
029100090519     C                   Z-ADD     0501688       VATCCM
029200090519     C                   Z-ADD     050           VABLNP
029300090519     C                   Z-ADD     050           VATLNP
029400090519     C                   Z-ADD     000           VABCTR
029500040823     C                   MOVEL     '1'           VABCBO
029600020619     C* ... e poi verifico se sono stati passati come parametri
029700020619     C                   IF        vlrppt > *blanks
029800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030000020619     C                   EXSR      CHKNUM
030100020619     C                   IF        PiInt=*on
030200020619     C                   Z-ADD     PiVal         VABCCM
030300020619     C                   Z-ADD     PiVal         VATCCM
030400020619     C                   ENDIF
030500040506     C                   ENDIF
030600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030800020619     C                   EXSR      CHKNUM
030900020619     C                   IF        PiInt=*on
031000020619     C                   Z-ADD     PiVal         VABLNP
031100020619     C                   Z-ADD     PiVal         VATLNP
031200040506     C                   ENDIF
031300020619     C                   ENDIF
031400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031600020619     C                   EXSR      CHKNUM
031700020619     C                   IF        PiInt=*on
031800020619     C                   Z-ADD     PiVal         VABCTR
031900040506     C                   ENDIF
032000020619     C                   ENDIF
032100020619     C                   ENDIF
032200000801     C*
032300000801     C                   ENDSR
032400000801     C*----------------------------------------------------*
032500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032600000801     C*----------------------------------------------------*
032700040823     C     IMPVAB        BEGSR
032800010305     C*
032900000801     C                   Z-ADD     *zeros        errore            1 0
033000000830     C                   MOVEL     datcor        VABAAS
033100020305     C                   MOVEL     datcor        VATAAS
033200040526     C                   MOVE      datcor        VABMGS
033300040823     C                   MOVE(P)   vlrpoi        VABFGS
033400040823     C                   MOVE(P)   vlrpoi        VATFGS
033500080409     C*
033600080409     C* Reperimento campi ALFA
033700080409     C*
033800080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
033900090519     C                   EVAL      VABRSD=%trim(%subst(vindta:27:30))
034000080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034100080409     C     '@':'A'       XLATE     VABRSD        VABRSD
034200080409     C* ==
034300090519     C                   EVAL      VABIND=%trim(%subst(vindta:57:15)) +
034400090519     C                                    %trim(%subst(vindta:72:24))
034500090519     C                   EVAL      VABLOD=%trim(%subst(vindta:96:24))
034600090519     C                   EVAL      VABPRD=%trim(%subst(vindta:125:2))
034700090519     C                   EVAL      VABRMA=%trim(%subst(vindta:1:6))
034800090525     C                   IF        %subst(vindta:138:1) = 'A'
034900090525     C                   EVAL      VABCBO='2'
035000090525     C                   ELSE
035100090525     C                   EVAL      VABCBO='1'
035200090525     C                   ENDIF
035300090519     C                   EVAL      VABTCR=%trim(%subst(vindta:147:1))
035400080409     C*
035500080409     C* Reperimento campi NUMERICI
035600090309     C* NSP
035700090519     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
035800090309     C                   EXSR      CHKNUM
035900090309     C                   IF        PiInt=*on
036000090309     C                   Z-ADD     PiVal         VABNSP
036100090309     C                   Z-ADD     PiVal         VATNSP
036200090309     C                   ELSE
036300090309     C                   EVAL      vinmsg = %trimr(vinmsg)
036400090309     C                             + ' ' + 'VABNSP VATNSP'
036500090309     C                   ENDIF
036600090309     C* RMN
036700090519     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
036800081217     C                   EXSR      CHKNUM
036900081217     C                   IF        PiInt=*on
037000081217     C                   Z-ADD     PiVal         VABRMN
037100081217     C                   ELSE
037200081217     C                   ADD       1             errore
037300081217     C                   Z-ADD     1             VABRMN
037400081217     C                   EVAL      vinmsg = %trimr(vinmsg)
037500081217     C                             + ' ' + 'VABRMN'
037600081217     C                   ENDIF
037700080409     C* CAD
037800090519     C                   EVAL      PiStr=%trim(%subst(vindta:120:5))
037900090114     C                   EXSR      CHKNUM
038000090114     C                   IF        PiInt=*on
038100090114     C                   Z-ADD     PiVal         Num5_0
038200090114     C                   MOVEL(p)  Num5_0        VABCAD
038300090114     C                   ELSE
038400090114     C                   ADD       1             errore
038500090114     C                   EVAL      VABCAD = *zeros
038600090114     C                   EVAL      vinmsg = %trimr(vinmsg)
038700090114     C                             + ' ' + 'VABCAD'
038800090114     C                   ENDIF
038900090519     C* CCM
039000090519     C                   EVAL      PiStr=%trim(%subst(vindta:20:7))
039100090519     C                   EXSR      CHKNUM
039200090519     C                   IF        PiInt=*on
039300090519     C                   Z-ADD     PiVal         VABCCM
039400090519     C                   ELSE
039500090519     C                   ADD       1             errore
039600090519     C                   EVAL      vinmsg = %trimr(vinmsg)
039700090519     C                             + ' ' + 'VABCCM'
039800090519     C                   ENDIF
039900081217     C* NCL
040000090519     C                   EVAL      PiStr=%trim(%subst(vindta:127:5))
040100081217     C                   EXSR      CHKNUM
040200081217     C                   IF        PiInt=*on
040300081217     C                   Z-ADD     PiVal         VABNCL
040400081217     C                   ELSE
040500090309     C                   ADD       1             errore
040600081217     C                   EVAL      vinmsg = %trimr(vinmsg)
040700081217     C                             + ' ' + 'VABNCL'
040800081217     C                   ENDIF
040900090312     C* PKB
041000090519     C                   EVAL      PiStr=%trim(%subst(vindta:132:6))
041100090312     C                   EXSR      CHKNUM
041200090312     C                   IF        PiNum=*on
041300090428     C                   Z-ADD     PiVal         VABPKB
041400090312     C                   ELSE
041500090312     C                   ADD       1             errore
041600090312     C                   EVAL      vinmsg = %trimr(vinmsg)
041700090312     C                             + ' ' + 'VABPKB'
041800090312     C                   ENDIF
041900090519     C* DCR
042000090519     C                   IF        %subst(vindta:139:8) <> *blanks AND
042100090519     C                             %subst(vindta:139:8) <> *zeros  AND
042200090519     C                             %subst(vindta:139:8) <> '00000000'
042300090519     C                   EVAL      PiStr=%subst(vindta:143:4) +
042400090519     C                                   %subst(vindta:141:2) +
042500090519     C                                   %subst(vindta:139:2)
042600090519     C                   EXSR      CHKNUM
042700090519     C                   IF        PiInt=*on
042800090519     C                   Z-ADD     PiVal         VABDCR
042900090519     C                   ELSE
043000090519     C                   ADD       1             errore
043100090519     C                   EVAL      vinmsg = %trimr(vinmsg)
043200090519     C                             + ' ' + 'VABDCR'
043300090519     C                   ENDIF
043400090519     C                   ENDIF
043500090519     C* IAS
043600090519     C                   IF        %subst(vindta:148:12) <> *blanks AND
043700090519     C                             %subst(vindta:148:12) <> *zeros  AND
043800090519     C                             %subst(vindta:148:12) <> '000000000,00'
043900090519     C                   EVAL      VABVAS = 'EUR'
044000090519     C                   EVAL      PiStr=%trim(%subst(vindta:148:12))
044100090428     C                   EXSR      CHKNUM
044200090428     C                   IF        PiNum=*on
044300090519     C                   Z-ADD     PiVal         VABIAS
044400090428     C                   ELSE
044500090428     C                   ADD       1             errore
044600090428     C                   EVAL      vinmsg = %trimr(vinmsg)
044700090519     C                             + ' ' + 'VABIAS'
044800090428     C                   ENDIF
044900090428     C                   ENDIF
045000071121     C*
045100080415     C* Carico l'estensioni A e B del FNVAT
045200080512     C                   exsr      exevata
045300080512     C                   exsr      exevatb
045400010205     C*
045500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045600040802     C                   IF        FlgCAS <> '0'
045700010205     C                   EVAL      VABCBO = '4'
045800010205     C                   ENDIF
045900020305     C*
046000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046100011113     C                   EXSR      CHKIMPDIV
046200010202     C*
046300000801     C* Ebbene...
046400000801     C                   ADD       1             §CTRMO
046500010201     C                   IF        errore <> *zeros
046600000801     C                   ADD       1             §CTRNO
046700000801     C                   EVAL      recko = vindta
046800000801     C                   EXCEPT    rigadett
046900000801     C                   EVAL      vinflg = '2'
047000000801     C                   ELSE
047100010201     C                   ADD       1             §CTROKVB
047200000801     C                   ENDIF
047300000801     C*
047400000801     C                   ENDSR
047500070102     C*----------------------------------------------------*
047600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
047700070102     C*----------------------------------------------------*
047800070103     C     EXEVATE       BEGSR
047900070102     C*
048000090428     C                   EVAL      VATTRC = 'E'
048100090428     C*
048200090504     C***                exsr      wrivat                                       => scarico VAT
048300070102     C*
048400070102     C                   ENDSR
048500080415     C*----------------------------------------------------*
048600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
048700080415     C*----------------------------------------------------*
048800080415     C     EXEVATA       BEGSR
048900080415     C*
049000090504     C                   EVAL      VATTRC = 'A'
049100080415     C*
049200080415     C***                exsr      wrivat                                       => scarico VAT
049300080415     C*
049400080415     C                   ENDSR
049500071121     C*----------------------------------------------------*
049600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
049700071121     C*----------------------------------------------------*
049800071121     C     EXEVATB       BEGSR
049900071121     C*
050000090504     C                   EVAL      VATTRC = 'B'
050100071121     C*
050200080409     C***                exsr      wrivat                                       => scarico VAT
050300071121     C*
050400071121     C                   ENDSR
050500010201     C*----------------------------------------------------*
050600090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
050700010201     C*----------------------------------------------------*
050800020305     C     WRIVAT        BEGSR
050900050628     C*
051000060223     C* Scrivo solo se valorizzato qualcosa
051100060223     C                   IF        VATNOT <> *blanks
051200090114     C                   WRITE     FIVAT000
051300060223     C                   ENDIF
051400010201     C*
051500010201     C                   ENDSR
051600010202     C*----------------------------------------------------*
051700090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051800010202     C*----------------------------------------------------*
051900020305     C     PREVAT        BEGSR
052000010202     C*
052100090114     C* Compongo il nome del membro da dare al FIVATWWR
052200010202     C                   eval      parmbr = vlrhdl
052300010202     C                   movel     'M'           parmbr
052400050627     C                   eval      parccm = %subst(vlrKSC:2:7)
052500010202     C                   eval      paropz = '1'
052600010202     C* Effettuo la chiamata al CLLE preposto
052700090114     C                   call(e)   'TITVVTC'
052800010202     C                   parm                    parccm
052900010202     C                   parm                    parmbr
053000010202     C                   parm                    paropz
053100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053200010202     C                   if        %error
053300010202     C                   movel     '1'           chkcall
053400010202     C                   else
053500010202     C                   movel     '0'           chkcall
053600010202     C                   endif
053700010202     C*
053800010202     C                   ENDSR
053900000801     C*----------------------------------------------------*
054000000801     C*  CONTROLLO NUMERICITA' CAMPI
054100000801     C*----------------------------------------------------*
054200000801     C     CHKNUM        BEGSR
054300081217     C*
054400081217     C                   IF        PiDecChr = *blanks
054500090519     C                   EVAL      PiDecChr = ','
054600081217     C                   ENDIF
054700081217     C*
054800081217     C                   callp(e)  UBISNUM_Check(PiStr
054900081217     C                                          :PiDecChr
055000081217     C                                          :PiVal
055100081217     C                                          :PiNum
055200081217     C                                          :PiInt)
055300081217     C*
055400081217     C                   IF        %error
055500081217     C                   EVAL      PiInt=*off
055600081217     C                   ENDIF
055700000801     C*
055800000801     C                   ENDSR
055900000801     C***
056000000801
056100011113
056200011113     C*----------------------------------------------------*
056300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056400011113     C*----------------------------------------------------*
056500011113     C     CHKIMPDIV     BEGSR
056600011113     C*
056700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056800011113     C                   Z-ADD     *zeros        wrkDec            9 9
056900011113     C*
057000011113     C* Come prima cosa effettuo considerazioni sulla divisa
057100011113     C                   IF        vabIAS > *zeros
057200011113     C                   IF        vabVAS <> 'EUR'
057300011113     C                   EVAL      vabVAS =  'ITL'
057400011113     C                   ENDIF
057500011113     C                   ENDIF
057600011113     C*
057700011113     C                   IF        vabCAS > *zeros
057800011113     C                   IF        vabVCA <> 'EUR'
057900011113     C                   EVAL      vabVCA =  'ITL'
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C                   IF        vabVMD > *zeros
058400020305     C                   IF        vabVAD <> 'EUR'
058500011113     C                   EVAL      vabVAD =  'ITL'
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059000011113     C                   Z-ADD     vabIAS        wrkDec
059100011113     C                   IF        wrkDec > *zeros
059200011113     C                   IF        vabVAS = 'ITL'
059300011113     C                   EVAL      vabIAS = *zeros
059400011113     C                   ENDIF
059500011113     C                   ENDIF
059600011113     C*
059700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
059800011113     C                   Z-ADD     vabCAS        wrkDec
059900011113     C                   IF        wrkDec > *zeros
060000011113     C                   IF        vabVCA = 'ITL'
060100011113     C                   EVAL      vabCAS = *zeros
060200011113     C                   ENDIF
060300011113     C                   ENDIF
060400011113     C*
060500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060600011113     C                   Z-ADD     vabVMD        wrkDec
060700011113     C                   IF        wrkDec > *zeros
060800011113     C                   IF        vabVAD = 'ITL'
060900011113     C                   EVAL      vabVMD = *zeros
061000011113     C                   ENDIF
061100011113     C                   ENDIF
061200011113     C*
061300011113     C                   ENDSR
061400011113     C***
061500011113
061600011113
061700000801
061800000801
061900990920      /TITLE Invio dei dati al punto operativo.
062000010202     C     invio         BEGSR
062100990920     C*
062200090114     C* 1° invio FIVAT
062300010201     C                   reset                   dscmz
062400010201     C                   move      vlrpoi        cmzdst
062500090114     C                   eval      cmzfld = 'FIVATWWR'
062600010201     C                   eval      cmzmbd = vlrhdl
062700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062800021009     C***                if        prmfir = *blanks
062900090114     C                   eval      cmzfla = 'FIVAT00F'
063000090114     C                   eval      cmzmba = 'FIVAT00F'
063100021009     C***                else
063200021009     C***                eval      cmzfla = prmfir
063300021009     C***                eval      cmzmba = prmfir
063400021009     C***                endif
063500010201     C                   eval      cmznrr = *zeros
063600020305     C                   move      §ctrokvt      cmznrr
063700021018     C                   eval      cmzlba = vlrfl1
063800010201     C                   call(e)   'TIS711C'
063900010201     C                   parm                    dscmz
064000010201     C                   parm      *blanks       esito
064100010205     C                   if        %error
064200010205     C                             or cmzerr = '1'
064300010205     C                             or esito  = '1'
064400010205     C                   eval      wrkesito = '3'
064500010205     C                   else
064600010201     C*
064700090114     C* 2° invio FIVAB
064800010201     C                   reset                   dscmz
064900010201     C                   move      vlrpoi        cmzdst
065000010201     C                   eval      cmzfld = vlrfou
065100010201     C                   eval      cmzmbd = vlrhdl
065200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065300021009     C***                if        prmfir = *blanks
065400090114     C                   eval      cmzfla = 'FIVAB00F'
065500090114     C                   eval      cmzmba = 'FIVAB00F'
065600021009     C***                else
065700021009     C***                eval      cmzfla = prmfir
065800021009     C***                eval      cmzmba = prmfir
065900021009     C***                endif
066000010201     C                   eval      cmznrr = *zeros
066100010201     C                   move      §ctrokvb      cmznrr
066200021018     C                   eval      cmzlba = vlrfl1
066300010201     C                   call(e)   'TIS711C'
066400010201     C                   parm                    dscmz
066500010201     C                   parm      *blanks       esito
066600010201     C                   if        %error
066700010201     C                             or cmzerr = '1'
066800010201     C                             or esito  = '1'
066900010201     C                   eval      wrkesito = '3'
067000010201     C                   endif
067100010205     C                   endif
067200990920     C*
067300000613     C                   ENDSR
067400000613     C***
067500070411
067600090331
067700090331
067800090331
067900070411     C     *pssr         BEGSR
068000070411     C*
068100070411     C                   if        %open(tivin00r)
068200070411     C                   close     tivin00r
068300070411     C                   endif
068400090114     C                   if        %open(fivabwwr)
068500090114     C                   close     fivabwwr
068600070411     C                   endif
068700090114     C                   if        %open(fivatwwr)
068800090114     C                   close     fivatwwr
068900070411     C                   endif
069000070411     C*
069100070411     C* Effettuo la chiamata al CLLE preposto
069200090114     C                   call(e)   'TITVVTC'
069300070411     C                   parm                    parccm
069400070411     C                   parm                    parmbr
069500070411     C                   parm      '2'           paropz
069600070411     C*
069700070411     C                   eval      wrkesito = '2'
069800070411     C*
069900070411     C                   seton                                        LR
070000070411     C*
070100070411     C                   ENDSR     '*CANCL'
070200070411     C***
070300070411
070400090331
070500090331
070600990910
070700000613     C     *inzsr        BEGSR
070800990910     C*
070900990910     C     *entry        plist
071000990920     C                   parm                    tivlrds
071100990921     C                   parm      wrkesito      esito
071200000724     C                   parm                    prmlit
071300000710     C                   parm                    prmfir
071400000613     C*
071500000830     C* CALCOLA LA DATA CORRENTE
071600000830     C                   time                    wn14             14 0
071700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
071800080512     C                   movel     wn14          oracor            6 0          *ORA
071900000830     C                   z-add     wn8           g08dat
072000000830     C                   z-add     *zeros        g08inv
072100000830     C                   movel     '0'           g08err
072200000830     C                   call      'XSRDA8'
072300000830     C                   parm                    wlbda8
072400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
072500000830     C*
072600000613     C                   ENDSR
072700000613     C***
072800990908
072900090331
073000090331
073100090519     Otitv1p8p  E            riepilogo         2
073200990915     O                                              'Upload via Internet'
073300990915     O                                           +1 'Traduzione TIVIN00R -'
073400090114     O                                           +1 'FIVABWWR/FIVATWWR'
073500010201     O                                           +1 'Report testate bolle'
073600990915     O          E            riepilogo   2
073700990915     O                       wrkdata
073800990915     O                       wrkora              +1
073900990915     O                       procname            +1
074000990915     O          E            riepilogo   2
074100990915     O                                              'Cliente..................:'
074200990915     O                       VABCCM        z     +1
074300990915     O          E            riepilogo   2
074400990920     O                                              'Riferimento Strategi.....:'
074500990920     O                       vlrhdl              +1
074600990915     O          E            riepilogo   2
074700990915     O                                              'Giusti...................:'
074800010201     O                       §CTROKVB      2   +  1
074900990915     O          E            riepilogo   2
075000990915     O                                              'Sbagliati e corretti.....:'
075100971022     O                       §CTRMO        2   +  1
075200990915     O          E            riepilogo   2
075300990915     O                                              'Sbagliati e scartati.....:'
075400971022     O                       §CTRNO        2   +  1
075500000613
075600090519     Otitv1p8ps E            testdett          2
075700000613     O                                              'Upload via Internet'
075800000613     O                                           +1 'Traduzione TIVIN00R -'
075900090114     O                                           +1 'FIVABWWR/FIVATWWR'
076000010201     O                                           +1 'Report testate bolle'
076100000616     O          E            testdett    3
076200000613     O                                           +2 'N° rec'
076300000613     O                                           +3 'Anteprima contenuto'
076400000616     O          E            rigadett    2
076500000613     O                       rrnum               +2
076600000621     O                       recko               +3
076700000616     O          E            findett     2
076800000613     O                       wrkdata
076900000613     O                       wrkora              +1
077000000613     O                       procname            +1
077100000616     O          E            findett     2
077200000613     O                                              'Cliente..................:'
077300000613     O                       VABCCM        z     +1
077400000616     O          E            findett     2
077500000613     O                                              'Riferimento Strategi.....:'
077600000613     O                       vlrhdl              +1
077700000616     O          E            findett     2
077800000613     O                                              'Giusti...................:'
077900010201     O                       §CTROKVB      2   +  1
078000000616     O          E            findett     2
078100000613     O                                              'Sbagliati e corretti.....:'
078200000613     O                       §CTRMO        2   +  1
078300000616     O          E            findett     2
078400000613     O                                              'Sbagliati e scartati.....:'
078500000613     O                       §CTRNO        2   +  1
078600000512** CMD - COMANDI CL
078700090519OVRPRTF FILE(TITV1P8P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
078800090519OVRPRTF FILE(TITV1P8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
078900090519DLTOVR FILE(TITV1P8P TITV1P8PS) LVL(*)
079000000512
079100000512
