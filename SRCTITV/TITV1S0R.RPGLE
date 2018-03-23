000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090114     FFIVABwwr  O    E             DISK    usropn
000700090114     FFIVATwwr  O    E             DISK    usropn
000800090922     Ftitv1s0p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090922     Ftitv1s0ps O    f  198        PRINTER usropn
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
004000090922     D curSped         s              7    INZ(*blanks)
004100090922     D depSped         s              7    INZ(*blanks)
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
009800090922     C                   if        not %open(titv1s0ps)
009900090922     C                   open      titv1s0ps
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
011800090922     C                   if        %open(titv1s0ps)
011900000616     C                   except    findett
012000090922     C                   close     titv1s0ps
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
013800090922     C                   if        not %open(titv1s0p)
013900090922     C                   open      titv1s0p
014000990915     C                   endif
014100990915     C*
014200990915     C                   except    riepilogo
014300990915     C*
014400090922     C                   if        %open(titv1s0p)
014500090922     C                   close     titv1s0p
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
019400090922     C                   eval      curSped = %subst(vindta:51:7)
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
029000090922     C                   Z-ADD     0601227       VABCCM
029100090925     C                   Z-ADD     0601227       VATCCM
029200090922     C                   Z-ADD     060           VABLNP
029300090922     C                   Z-ADD     060           VATLNP
029400090519     C                   Z-ADD     000           VABCTR
029500040823     C                   MOVEL     '1'           VABCBO
029600090922     C                   MOVEL     '7Q'          VABCTM
029700020619     C* ... e poi verifico se sono stati passati come parametri
029800020619     C                   IF        vlrppt > *blanks
029900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
030000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030100020619     C                   EXSR      CHKNUM
030200020619     C                   IF        PiInt=*on
030300020619     C                   Z-ADD     PiVal         VABCCM
030400020619     C                   Z-ADD     PiVal         VATCCM
030500020619     C                   ENDIF
030600040506     C                   ENDIF
030700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030900020619     C                   EXSR      CHKNUM
031000020619     C                   IF        PiInt=*on
031100020619     C                   Z-ADD     PiVal         VABLNP
031200020619     C                   Z-ADD     PiVal         VATLNP
031300040506     C                   ENDIF
031400020619     C                   ENDIF
031500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031700020619     C                   EXSR      CHKNUM
031800020619     C                   IF        PiInt=*on
031900020619     C                   Z-ADD     PiVal         VABCTR
032000040506     C                   ENDIF
032100020619     C                   ENDIF
032200020619     C                   ENDIF
032300000801     C*
032400000801     C                   ENDSR
032500000801     C*----------------------------------------------------*
032600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032700000801     C*----------------------------------------------------*
032800040823     C     IMPVAB        BEGSR
032900010305     C*
033000000801     C                   Z-ADD     *zeros        errore            1 0
033100000830     C                   MOVEL     datcor        VABAAS
033200020305     C                   MOVEL     datcor        VATAAS
033300040526     C                   MOVE      datcor        VABMGS
033400040823     C                   MOVE(P)   vlrpoi        VABFGS
033500040823     C                   MOVE(P)   vlrpoi        VATFGS
033600080409     C*
033700080409     C* Reperimento campi ALFA
033800080409     C*
033900080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
034000090922     C                   EVAL      VABRSD=%trim(%subst(vindta:276:35))
034100080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034200080409     C     '@':'A'       XLATE     VABRSD        VABRSD
034300080409     C* ==
034400090922     C                   EVAL      VABIND=%trim(%subst(vindta:341:35))
034500090922     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
034600090922     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
034700090922     C                   EVAL      VABRMA=%trim(%subst(vindta:46:12))
034800090922     C                   IF        %subst(vindta:43:1) = 'F'
034900090922     C                   EVAL      VABCBO='1'
035000090525     C                   ELSE
035100090922     C                   EVAL      VABCBO='2'
035200090525     C                   ENDIF
035300090922     C                   IF        %subst(vindta:457:1) = 'E'
035400090922     C                   EVAL      VABTSP='E'
035500090922     C                   ENDIF
035600080409     C*
035700080409     C* Reperimento campi NUMERICI
035800090309     C* NSP
035900090922     C                   EVAL      PiStr=%trim(%subst(vindta:51:7))
036000090309     C                   EXSR      CHKNUM
036100090309     C                   IF        PiInt=*on
036200090309     C                   Z-ADD     PiVal         VABNSP
036300090309     C                   Z-ADD     PiVal         VATNSP
036400090309     C                   ELSE
036500090309     C                   EVAL      vinmsg = %trimr(vinmsg)
036600090309     C                             + ' ' + 'VABNSP VATNSP'
036700090309     C                   ENDIF
036800090309     C* RMN
036900090922     C                   EVAL      PiStr=%trim(%subst(vindta:46:4)) +
037000090922     C                                   %trim(%subst(vindta:51:7))
037100081217     C                   EXSR      CHKNUM
037200081217     C                   IF        PiInt=*on
037300081217     C                   Z-ADD     PiVal         VABRMN
037400081217     C                   ELSE
037500081217     C                   ADD       1             errore
037600081217     C                   Z-ADD     1             VABRMN
037700081217     C                   EVAL      vinmsg = %trimr(vinmsg)
037800081217     C                             + ' ' + 'VABRMN'
037900081217     C                   ENDIF
038000080409     C* CAD
038100090922     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
038200090114     C                   EXSR      CHKNUM
038300090114     C                   IF        PiInt=*on
038400090114     C                   Z-ADD     PiVal         Num5_0
038500090114     C                   MOVEL(p)  Num5_0        VABCAD
038600090114     C                   ELSE
038700090114     C                   ADD       1             errore
038800090114     C                   EVAL      VABCAD = *zeros
038900090114     C                   EVAL      vinmsg = %trimr(vinmsg)
039000090114     C                             + ' ' + 'VABCAD'
039100090114     C                   ENDIF
039200081217     C* NCL
039300090922     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
039400081217     C                   EXSR      CHKNUM
039500081217     C                   IF        PiInt=*on
039600081217     C                   Z-ADD     PiVal         VABNCL
039700081217     C                   ELSE
039800090309     C                   ADD       1             errore
039900081217     C                   EVAL      vinmsg = %trimr(vinmsg)
040000081217     C                             + ' ' + 'VABNCL'
040100081217     C                   ENDIF
040200090312     C* PKB
040300090922     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
040400090312     C                   EXSR      CHKNUM
040500090312     C                   IF        PiNum=*on
040600090922     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a Kg.
040700090922     C                   Z-ADD(H)  PiVal         VABPKB
040800090312     C                   ELSE
040900090312     C                   ADD       1             errore
041000090312     C                   EVAL      vinmsg = %trimr(vinmsg)
041100090312     C                             + ' ' + 'VABPKB'
041200090312     C                   ENDIF
041300090922     C* IAS
041400090922     C                   IF        %subst(vindta:544:10) <> *blanks AND
041500090922     C                             %subst(vindta:544:10) <> *zeros  AND
041600090922     C                             %subst(vindta:544:10) <> '0000000.00'
041700090519     C                   EVAL      VABVAS = 'EUR'
041800090922     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
041900090428     C                   EXSR      CHKNUM
042000090428     C                   IF        PiNum=*on
042100090519     C                   Z-ADD     PiVal         VABIAS
042200090428     C                   ELSE
042300090428     C                   ADD       1             errore
042400090428     C                   EVAL      vinmsg = %trimr(vinmsg)
042500090519     C                             + ' ' + 'VABIAS'
042600090428     C                   ENDIF
042700090428     C                   ENDIF
042800090922     C* CAS
042900090922     C                   IF        %subst(vindta:502:9) <> *blanks AND
043000090922     C                             %subst(vindta:502:9) <> *zeros  AND
043100090922     C                             %subst(vindta:502:9) <> '000000.00'
043200090922     C                   EVAL      FlgCAS = '1'
043300090922     C                   EVAL      VABVCA = 'EUR'
043400090925     C*
043500090925     C                   SELECT
043600090925     C                   WHEN      %trim(%subst(vindta:511:3)) = 'ABM'
043700090925     C                   EVAL      VABTIC = 'BM'
043800090925     C                   WHEN      %trim(%subst(vindta:511:3)) = 'ACM'
043900090925     C                   EVAL      VABTIC = 'CM'
044000090925     C                   WHEN      %trim(%subst(vindta:511:3)) = 'CON'
044100090925     C                   EVAL      VABTIC = *blanks
044200090925     C                   ENDSL
044300090925     C*
044400090922     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
044500090922     C                   EXSR      CHKNUM
044600090922     C                   IF        PiNum=*on
044700090922     C                   Z-ADD     PiVal         VABCAS
044800090922     C                   ELSE
044900090922     C                   ADD       1             errore
045000090922     C                   EVAL      vinmsg = %trimr(vinmsg)
045100090922     C                             + ' ' + 'VABCAS'
045200090922     C                   ENDIF
045300090922     C                   ENDIF
045400071121     C*
045500080415     C* Carico l'estensioni A e B del FNVAT
045600080512     C                   exsr      exevata
045700080512     C                   exsr      exevatb
045800010205     C*
045900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
046000040802     C                   IF        FlgCAS <> '0'
046100090922     C                   IF        VABCBO = '1'
046200010205     C                   EVAL      VABCBO = '4'
046300090922     C                   ELSE
046400090922     C                   EVAL      VABCBO = '6'
046500090922     C                   ENDIF
046600010205     C                   ENDIF
046700020305     C*
046800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046900011113     C                   EXSR      CHKIMPDIV
047000010202     C*
047100000801     C* Ebbene...
047200000801     C                   ADD       1             §CTRMO
047300010201     C                   IF        errore <> *zeros
047400000801     C                   ADD       1             §CTRNO
047500000801     C                   EVAL      recko = vindta
047600000801     C                   EXCEPT    rigadett
047700000801     C                   EVAL      vinflg = '2'
047800000801     C                   ELSE
047900010201     C                   ADD       1             §CTROKVB
048000000801     C                   ENDIF
048100000801     C*
048200000801     C                   ENDSR
048300070102     C*----------------------------------------------------*
048400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
048500070102     C*----------------------------------------------------*
048600070103     C     EXEVATE       BEGSR
048700070102     C*
048800090428     C                   EVAL      VATTRC = 'E'
048900090922     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
049000090428     C*
049100090922     C                   exsr      wrivat                                       => scarico VAT
049200070102     C*
049300070102     C                   ENDSR
049400080415     C*----------------------------------------------------*
049500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
049600080415     C*----------------------------------------------------*
049700080415     C     EXEVATA       BEGSR
049800080415     C*
049900090504     C                   EVAL      VATTRC = 'A'
050000090922     C                   EVAL      VATNOT = %trim(%subst(vindta:316:20))
050100080415     C*
050200090922     C                   exsr      wrivat                                       => scarico VAT
050300080415     C*
050400080415     C                   ENDSR
050500071121     C*----------------------------------------------------*
050600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
050700071121     C*----------------------------------------------------*
050800071121     C     EXEVATB       BEGSR
050900071121     C*
051000090504     C                   EVAL      VATTRC = 'B'
051100090922     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
051200071121     C*
051300090922     C                   exsr      wrivat                                       => scarico VAT
051400071121     C*
051500071121     C                   ENDSR
051600010201     C*----------------------------------------------------*
051700090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
051800010201     C*----------------------------------------------------*
051900020305     C     WRIVAT        BEGSR
052000050628     C*
052100060223     C* Scrivo solo se valorizzato qualcosa
052200060223     C                   IF        VATNOT <> *blanks
052300090922     C                   WRITE     FIVAT000
052400090922     C                   ADD       1             §ctrokvt
052500060223     C                   ENDIF
052600010201     C*
052700010201     C                   ENDSR
052800010202     C*----------------------------------------------------*
052900090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053000010202     C*----------------------------------------------------*
053100020305     C     PREVAT        BEGSR
053200010202     C*
053300090114     C* Compongo il nome del membro da dare al FIVATWWR
053400010202     C                   eval      parmbr = vlrhdl
053500010202     C                   movel     'M'           parmbr
053600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
053700010202     C                   eval      paropz = '1'
053800010202     C* Effettuo la chiamata al CLLE preposto
053900090114     C                   call(e)   'TITVVTC'
054000010202     C                   parm                    parccm
054100010202     C                   parm                    parmbr
054200010202     C                   parm                    paropz
054300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054400010202     C                   if        %error
054500010202     C                   movel     '1'           chkcall
054600010202     C                   else
054700010202     C                   movel     '0'           chkcall
054800010202     C                   endif
054900010202     C*
055000010202     C                   ENDSR
055100000801     C*----------------------------------------------------*
055200000801     C*  CONTROLLO NUMERICITA' CAMPI
055300000801     C*----------------------------------------------------*
055400000801     C     CHKNUM        BEGSR
055500081217     C*
055600081217     C                   IF        PiDecChr = *blanks
055700090922     C                   EVAL      PiDecChr = '.'
055800081217     C                   ENDIF
055900081217     C*
056000081217     C                   callp(e)  UBISNUM_Check(PiStr
056100081217     C                                          :PiDecChr
056200081217     C                                          :PiVal
056300081217     C                                          :PiNum
056400081217     C                                          :PiInt)
056500081217     C*
056600081217     C                   IF        %error
056700081217     C                   EVAL      PiInt=*off
056800081217     C                   ENDIF
056900000801     C*
057000000801     C                   ENDSR
057100000801     C***
057200000801
057300011113
057400011113     C*----------------------------------------------------*
057500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057600011113     C*----------------------------------------------------*
057700011113     C     CHKIMPDIV     BEGSR
057800011113     C*
057900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058000011113     C                   Z-ADD     *zeros        wrkDec            9 9
058100011113     C*
058200011113     C* Come prima cosa effettuo considerazioni sulla divisa
058300011113     C                   IF        vabIAS > *zeros
058400011113     C                   IF        vabVAS <> 'EUR'
058500011113     C                   EVAL      vabVAS =  'ITL'
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C                   IF        vabCAS > *zeros
059000011113     C                   IF        vabVCA <> 'EUR'
059100011113     C                   EVAL      vabVCA =  'ITL'
059200011113     C                   ENDIF
059300011113     C                   ENDIF
059400011113     C*
059500011113     C                   IF        vabVMD > *zeros
059600020305     C                   IF        vabVAD <> 'EUR'
059700011113     C                   EVAL      vabVAD =  'ITL'
059800011113     C                   ENDIF
059900011113     C                   ENDIF
060000011113     C*
060100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060200011113     C                   Z-ADD     vabIAS        wrkDec
060300011113     C                   IF        wrkDec > *zeros
060400011113     C                   IF        vabVAS = 'ITL'
060500011113     C                   EVAL      vabIAS = *zeros
060600011113     C                   ENDIF
060700011113     C                   ENDIF
060800011113     C*
060900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061000011113     C                   Z-ADD     vabCAS        wrkDec
061100011113     C                   IF        wrkDec > *zeros
061200011113     C                   IF        vabVCA = 'ITL'
061300011113     C                   EVAL      vabCAS = *zeros
061400011113     C                   ENDIF
061500011113     C                   ENDIF
061600011113     C*
061700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061800011113     C                   Z-ADD     vabVMD        wrkDec
061900011113     C                   IF        wrkDec > *zeros
062000011113     C                   IF        vabVAD = 'ITL'
062100011113     C                   EVAL      vabVMD = *zeros
062200011113     C                   ENDIF
062300011113     C                   ENDIF
062400011113     C*
062500011113     C                   ENDSR
062600011113     C***
062700011113
062800011113
062900000801
063000000801
063100990920      /TITLE Invio dei dati al punto operativo.
063200010202     C     invio         BEGSR
063300990920     C*
063400090114     C* 1° invio FIVAT
063500010201     C                   reset                   dscmz
063600010201     C                   move      vlrpoi        cmzdst
063700090114     C                   eval      cmzfld = 'FIVATWWR'
063800010201     C                   eval      cmzmbd = vlrhdl
063900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064000021009     C***                if        prmfir = *blanks
064100090114     C                   eval      cmzfla = 'FIVAT00F'
064200090114     C                   eval      cmzmba = 'FIVAT00F'
064300021009     C***                else
064400021009     C***                eval      cmzfla = prmfir
064500021009     C***                eval      cmzmba = prmfir
064600021009     C***                endif
064700010201     C                   eval      cmznrr = *zeros
064800020305     C                   move      §ctrokvt      cmznrr
064900021018     C                   eval      cmzlba = vlrfl1
065000010201     C                   call(e)   'TIS711C'
065100010201     C                   parm                    dscmz
065200010201     C                   parm      *blanks       esito
065300010205     C                   if        %error
065400010205     C                             or cmzerr = '1'
065500010205     C                             or esito  = '1'
065600010205     C                   eval      wrkesito = '3'
065700010205     C                   else
065800010201     C*
065900090114     C* 2° invio FIVAB
066000010201     C                   reset                   dscmz
066100010201     C                   move      vlrpoi        cmzdst
066200010201     C                   eval      cmzfld = vlrfou
066300010201     C                   eval      cmzmbd = vlrhdl
066400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066500021009     C***                if        prmfir = *blanks
066600090114     C                   eval      cmzfla = 'FIVAB00F'
066700090114     C                   eval      cmzmba = 'FIVAB00F'
066800021009     C***                else
066900021009     C***                eval      cmzfla = prmfir
067000021009     C***                eval      cmzmba = prmfir
067100021009     C***                endif
067200010201     C                   eval      cmznrr = *zeros
067300010201     C                   move      §ctrokvb      cmznrr
067400021018     C                   eval      cmzlba = vlrfl1
067500010201     C                   call(e)   'TIS711C'
067600010201     C                   parm                    dscmz
067700010201     C                   parm      *blanks       esito
067800010201     C                   if        %error
067900010201     C                             or cmzerr = '1'
068000010201     C                             or esito  = '1'
068100010201     C                   eval      wrkesito = '3'
068200010201     C                   endif
068300010205     C                   endif
068400990920     C*
068500000613     C                   ENDSR
068600000613     C***
068700070411
068800090331
068900090331
069000090331
069100070411     C     *pssr         BEGSR
069200070411     C*
069300070411     C                   if        %open(tivin00r)
069400070411     C                   close     tivin00r
069500070411     C                   endif
069600090114     C                   if        %open(fivabwwr)
069700090114     C                   close     fivabwwr
069800070411     C                   endif
069900090114     C                   if        %open(fivatwwr)
070000090114     C                   close     fivatwwr
070100070411     C                   endif
070200070411     C*
070300070411     C* Effettuo la chiamata al CLLE preposto
070400090114     C                   call(e)   'TITVVTC'
070500070411     C                   parm                    parccm
070600070411     C                   parm                    parmbr
070700070411     C                   parm      '2'           paropz
070800070411     C*
070900070411     C                   eval      wrkesito = '2'
071000070411     C*
071100070411     C                   seton                                        LR
071200070411     C*
071300070411     C                   ENDSR     '*CANCL'
071400070411     C***
071500070411
071600090331
071700090331
071800990910
071900000613     C     *inzsr        BEGSR
072000990910     C*
072100990910     C     *entry        plist
072200990920     C                   parm                    tivlrds
072300990921     C                   parm      wrkesito      esito
072400000724     C                   parm                    prmlit
072500000710     C                   parm                    prmfir
072600000613     C*
072700000830     C* CALCOLA LA DATA CORRENTE
072800000830     C                   time                    wn14             14 0
072900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
073000080512     C                   movel     wn14          oracor            6 0          *ORA
073100000830     C                   z-add     wn8           g08dat
073200000830     C                   z-add     *zeros        g08inv
073300000830     C                   movel     '0'           g08err
073400000830     C                   call      'XSRDA8'
073500000830     C                   parm                    wlbda8
073600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
073700000830     C*
073800000613     C                   ENDSR
073900000613     C***
074000990908
074100090331
074200090331
074300090922     Otitv1s0p  E            riepilogo         2
074400990915     O                                              'Upload via Internet'
074500990915     O                                           +1 'Traduzione TIVIN00R -'
074600090114     O                                           +1 'FIVABWWR/FIVATWWR'
074700010201     O                                           +1 'Report testate bolle'
074800990915     O          E            riepilogo   2
074900990915     O                       wrkdata
075000990915     O                       wrkora              +1
075100990915     O                       procname            +1
075200990915     O          E            riepilogo   2
075300990915     O                                              'Cliente..................:'
075400990915     O                       VABCCM        z     +1
075500990915     O          E            riepilogo   2
075600990920     O                                              'Riferimento Strategi.....:'
075700990920     O                       vlrhdl              +1
075800990915     O          E            riepilogo   2
075900990915     O                                              'Giusti...................:'
076000010201     O                       §CTROKVB      2   +  1
076100990915     O          E            riepilogo   2
076200990915     O                                              'Sbagliati e corretti.....:'
076300971022     O                       §CTRMO        2   +  1
076400990915     O          E            riepilogo   2
076500990915     O                                              'Sbagliati e scartati.....:'
076600971022     O                       §CTRNO        2   +  1
076700000613
076800090922     Otitv1s0ps E            testdett          2
076900000613     O                                              'Upload via Internet'
077000000613     O                                           +1 'Traduzione TIVIN00R -'
077100090114     O                                           +1 'FIVABWWR/FIVATWWR'
077200010201     O                                           +1 'Report testate bolle'
077300000616     O          E            testdett    3
077400000613     O                                           +2 'N° rec'
077500000613     O                                           +3 'Anteprima contenuto'
077600000616     O          E            rigadett    2
077700000613     O                       rrnum               +2
077800000621     O                       recko               +3
077900000616     O          E            findett     2
078000000613     O                       wrkdata
078100000613     O                       wrkora              +1
078200000613     O                       procname            +1
078300000616     O          E            findett     2
078400000613     O                                              'Cliente..................:'
078500000613     O                       VABCCM        z     +1
078600000616     O          E            findett     2
078700000613     O                                              'Riferimento Strategi.....:'
078800000613     O                       vlrhdl              +1
078900000616     O          E            findett     2
079000000613     O                                              'Giusti...................:'
079100010201     O                       §CTROKVB      2   +  1
079200000616     O          E            findett     2
079300000613     O                                              'Sbagliati e corretti.....:'
079400000613     O                       §CTRMO        2   +  1
079500000616     O          E            findett     2
079600000613     O                                              'Sbagliati e scartati.....:'
079700000613     O                       §CTRNO        2   +  1
079800000512** CMD - COMANDI CL
079900090922OVRPRTF FILE(TITV1S0P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
080000090922OVRPRTF FILE(TITV1S0PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
080100090922DLTOVR FILE(TITV1S0P TITV1S0PS) LVL(*)
080200000512
080300000512
