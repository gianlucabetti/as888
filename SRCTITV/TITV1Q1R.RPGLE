000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090114     FFIVABwwr  O    E             DISK    usropn
000700090114     FFIVATwwr  O    E             DISK    usropn
000800090609     Ftitv1q1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090609     Ftitv1q1ps O    f  198        PRINTER usropn
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
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900090114
005000081217
005100081217     D*------------------
005200081217     D* LINKING A DEFINIZIONI ESTERNE
005300081217     D*------------------
005400081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005500081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005600081217
005700081217
005800010201
005900081217
006000081217     C
006100990915     C                   time                    wrkdata
006200990915     C                   time                    wrkora
006300000913     C                   reset                   rrnum
006400990921     C                   reset                   esito
006500990921     C                   reset                   wrkesito
006600000724     C*
006700000724     C* SE OCCORRE SPEDIRE IN FILIALE
006800000724     C                   if        vlrpoi <> *zeros
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200000724     C     vlrpoi        chain     azorg01l
007300000724     C                   if        %found
007400000616     C                   movel(p)  CMD(1)        depcmd
007500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000616     C*
007700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000616     C                   PARM                    depcmd
008100000616     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
008400000724     C                   endif
008500000616     C*
008600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008700000616     C                   movel(p)  CMD(2)        depcmd
008800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008900000616     C*
009000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009100011113     C                   Z-ADD     150           LENGH            15 5
009200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009300000616     C                   PARM                    depcmd
009400000616     C                   PARM                    LENGH
009500000616     C*
009600090609     C                   if        not %open(titv1q1ps)
009700090609     C                   open      titv1q1ps
009800000616     C                   except    testdett
009900000613     C                   endif
010000000613     C*
010100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010300000613     C*
010400010202     C* Effettuo la chiamata al CLLE preposto
010500090114     C                   call(e)   'TITVVTC'
010600010202     C                   parm                    parccm
010700010202     C                   parm                    parmbr
010800010202     C                   parm      '2'           paropz
010900050201     C*
011000050201     C* Effettuo lancio TISI95 solo x chiusura
011100050201     C                   CLEAR                   TISI95DS
011200050201     C                   EVAL      I95TLA = 'C'
011300050201     C                   CALL      'TISI95R'
011400050201     C                   PARM                    TISI95DS
011500010202     C*
011600090609     C                   if        %open(titv1q1ps)
011700000616     C                   except    findett
011800090609     C                   close     titv1q1ps
011900000613     C                   endif
012000000616     C*
012100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    CMD(3)
012500000616     C                   PARM                    LENGH
012600000616     C*
012700000801     C
012800010201     C                   seton                                        LR
012900000613
013000000613
013100000613     C*--------------------------------------------------------
013200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013300000613     C*--------------------------------------------------------
013400000621     C     STPR          BEGSR
013500000613     C*
013600090609     C                   if        not %open(titv1q1p)
013700090609     C                   open      titv1q1p
013800990915     C                   endif
013900990915     C*
014000990915     C                   except    riepilogo
014100990915     C*
014200090609     C                   if        %open(titv1q1p)
014300090609     C                   close     titv1q1p
014400990914     C                   endif
014500990910     C*
014600000613     C                   ENDSR
014700000613     C***
014800990908
014900000801
015000910830     C*--------------------------------------------------------
015100090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015200910830     C*--------------------------------------------------------
015300040526     C     RWFILE        BEGSR
015400990910     C*
015500990914     C                   if        not %open(tivin00r)
015600990908     C                   open      tivin00r
015700990914     C                   endif
015800090114     C                   if        not %open(fivabwwr)
015900090114     C                   open      fivabwwr
016000990914     C                   endif
016100070103     C*
016200090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016300020305     C                   exsr      prevat
016400010201     C*
016500010202     C                   if        chkcall = '0'
016600010202     C*
016700090114     C                   if        not %open(fivatwwr)
016800090114     C                   open      fivatwwr
016900010201     C                   endif
017000990910     C*
017100010201     C                   clear                   §CTROKVB          5 0
017200020305     C                   clear                   §CTROKVT          5 0
017300000801     C                   clear                   §CTRMO            5 0
017400000801     C                   clear                   §CTRNO            5 0
017500990910     C*
017600921023     C                   DO        *HIVAL
017700990913     C*
017800990915     C                   READ      tivin00r                               70
017900050627     C                   if        vindta > *blanks
018000000613     C                   add       1             rrnum
018100000801     C*
018200000801     C                   if        *in70 = *off
018300000801     C                             and
018400000801     C                             (vinflg = *blanks
018500000801     C                              or vinflg = '0'
018600000801     C                              or vinflg = '2')
018700000801     C*
018800000801     C                   clear                   vinmsg
018900000801     C                   eval      vinflg = '1'
019000070103     C*
019100090309     C                   EXSR      INZVAR
019200090309     C                   EXSR      DEFCAM
019300090527     C*
019400070213     C                   exsr      impvab                                       => carico  VAB
019500090527     C                   exsr      wrivab                                       => scarico VAB
019600000905     C*
019700000905     C                   else
019800000905     C                   eval      vinflg = '1'
019900050628     C                   endif
020000000905     C                   endif
020100000905     C*
020200000905     C  N70              update    tivin000
020300000905     C*
020400991022     C  N70              ENDdo
020500010202     C*
020600010202     C                   endif
020700990910
020800990910     C* Se non ci sono record con errori ...
020900000710     C                   if        §ctrno = 0
021000990910     C* ... restituisco esito OK.
021100990921     C                   eval      wrkesito = '0'
021200990910     C                   else
021300010201     C                   if        §ctrokvb > 0
021400990921     C                   eval      wrkesito = '1'
021500000710     C                   else
021600000710     C                   eval      wrkesito = '2'
021700990910     C                   endif
021800000710     C                   endif
021900990910     C*
022000990914     C                   if        %open(tivin00r)
022100990908     C                   close     tivin00r
022200990914     C                   endif
022300090114     C                   if        %open(fivabwwr)
022400090114     C                   close     fivabwwr
022500990914     C                   endif
022600090114     C                   if        %open(fivatwwr)
022700090114     C                   close     fivatwwr
022800010201     C                   endif
022900990910     C*
023000010201     C                   if        §ctrokvb > 0
023100000724     C                             and vlrpoi <> *zeros
023200010202     C                   exsr      invio
023300990920     C                   endif
023400990920     C*
023500910830     C                   ENDSR
023600000613     C***
023700010305
023800010305     C*----------------------------------------------------*
023900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024000010305     C*----------------------------------------------------*
024100020305     C     WRIVAB        BEGSR
024200010305     C*
024300060225     C* Quindi scarico il buffer del file d testata
024400090114     C                   write     fivab000                                     => scarico il VAB
024500010305     C*
024600010305     C                   ENDSR
024700990920
024800000801     C*----------------------------------------------------*
024900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025000000801     C*----------------------------------------------------*
025100010201     C     INZVAR        BEGSR
025200000801     C*
025300090114     C                   CLEAR                   FIVAB000
025400090114     C                   CLEAR                   FIVAT000
025500070213     C*
025600040802     C                   Z-ADD     *zeros        Num5_0            5 0
025700040802     C                   MOVEL     '0'           FlgCAS            1
025800000801     C*
025900000801     C                   ENDSR
026000000801     C*----------------------------------------------------*
026100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026200000801     C*----------------------------------------------------*
026300000801     C     DEFCAM        BEGSR
026400000801     C*
026500020619     C* Imposto i valori di default...
026600090527     C                   MOVEL     datcor        VABAAS
026700090527     C                   MOVEL     datcor        VATAAS
026800090527     C                   MOVE(P)   vlrpoi        VABFGS
026900090527     C                   MOVE(P)   vlrpoi        VATFGS
027000090527     C*
027100090609     C                   Z-ADD     0560921       VABCCM
027200090609     C                   Z-ADD     0560921       VATCCM
027300090609     C                   Z-ADD     056           VABLNP
027400090609     C                   Z-ADD     056           VATLNP
027500090519     C                   Z-ADD     000           VABCTR
027600040823     C                   MOVEL     '1'           VABCBO
027700090527     C                   MOVEL     '7Q'          VABCTM
027800020619     C* ... e poi verifico se sono stati passati come parametri
027900020619     C                   IF        vlrppt > *blanks
028000040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028200020619     C                   EXSR      CHKNUM
028300020619     C                   IF        PiInt=*on
028400020619     C                   Z-ADD     PiVal         VABCCM
028500020619     C                   Z-ADD     PiVal         VATCCM
028600020619     C                   ENDIF
028700040506     C                   ENDIF
028800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
028900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029000020619     C                   EXSR      CHKNUM
029100020619     C                   IF        PiInt=*on
029200020619     C                   Z-ADD     PiVal         VABLNP
029300020619     C                   Z-ADD     PiVal         VATLNP
029400040506     C                   ENDIF
029500020619     C                   ENDIF
029600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
029700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029800020619     C                   EXSR      CHKNUM
029900020619     C                   IF        PiInt=*on
030000020619     C                   Z-ADD     PiVal         VABCTR
030100040506     C                   ENDIF
030200020619     C                   ENDIF
030300020619     C                   ENDIF
030400000801     C*
030500000801     C                   ENDSR
030600000801     C*----------------------------------------------------*
030700090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030800000801     C*----------------------------------------------------*
030900040823     C     IMPVAB        BEGSR
031000010305     C*
031100000801     C                   Z-ADD     *zeros        errore            1 0
031200040526     C                   MOVE      datcor        VABMGS
031300080409     C*
031400080409     C* Reperimento campi ALFA
031500080409     C*
031600080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
031700090609     C                   EVAL      VABRSD=%trim(%subst(vindta:290:50))
031800080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031900080409     C     '@':'A'       XLATE     VABRSD        VABRSD
032000080409     C* ==
032100090609     C                   EVAL      VABRD2=%trim(%subst(vindta:380:40))
032200090609     C                   EVAL      VABIND=%trim(%subst(vindta:340:40))
032300090609     C                   EVAL      VABLOD=%trim(%subst(vindta:459:30))
032400090609     C                   EVAL      VABPRD=%trim(%subst(vindta:489:3))
032500090609     C                   EVAL      VABRMA=%trim(%subst(vindta:1:15))
032600080409     C*
032700080409     C* Reperimento campi NUMERICI
032800090609     C*
032900090609     C* CCM
033000090609     C                   IF        %trim(%subst(vindta:16:7)) = '120'
033100090609     C                   EVAL      VABCCM = 0560921
033200090609     C                   EVAL      VATCCM = 0560921
033300090609     C                   ENDIF
033400090609     C                   IF        %trim(%subst(vindta:16:7)) = '123'
033500090609     C                   EVAL      VABCCM = 0560923
033600090609     C                   EVAL      VATCCM = 0560923
033700090609     C                   ENDIF
033800090609     C                   IF        %trim(%subst(vindta:16:7)) = '013'
033900090609     C                   EVAL      VABCCM = 0560927
034000090609     C                   EVAL      VATCCM = 0560927
034100090609     C                   ENDIF
034200090309     C* NSP
034300090609     C                   EVAL      PiStr=%trim(%subst(vindta:1038:7))
034400090309     C                   EXSR      CHKNUM
034500090309     C                   IF        PiInt=*on
034600090309     C                   Z-ADD     PiVal         VABNSP
034700090309     C                   Z-ADD     PiVal         VATNSP
034800090309     C                   ELSE
034900090309     C                   EVAL      vinmsg = %trimr(vinmsg)
035000090309     C                             + ' ' + 'VABNSP VATNSP'
035100090309     C                   ENDIF
035200090309     C* RMN
035300090609     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
035400081217     C                   EXSR      CHKNUM
035500081217     C                   IF        PiInt=*on
035600081217     C                   Z-ADD     PiVal         VABRMN
035700081217     C                   ELSE
035800081217     C                   ADD       1             errore
035900081217     C                   Z-ADD     1             VABRMN
036000081217     C                   EVAL      vinmsg = %trimr(vinmsg)
036100081217     C                             + ' ' + 'VABRMN'
036200081217     C                   ENDIF
036300080409     C* CAD
036400090609     C                   EVAL      PiStr=%trim(%subst(vindta:450:9))
036500090114     C                   EXSR      CHKNUM
036600090114     C                   IF        PiInt=*on
036700090114     C                   Z-ADD     PiVal         Num5_0
036800090114     C                   MOVEL(p)  Num5_0        VABCAD
036900090114     C                   ELSE
037000090114     C                   ADD       1             errore
037100090114     C                   EVAL      VABCAD = *zeros
037200090114     C                   EVAL      vinmsg = %trimr(vinmsg)
037300090114     C                             + ' ' + 'VABCAD'
037400090114     C                   ENDIF
037500081217     C* NCL
037600090609     C                   EVAL      PiStr=%trim(%subst(vindta:977:5))
037700081217     C                   EXSR      CHKNUM
037800081217     C                   IF        PiInt=*on
037900081217     C                   Z-ADD     PiVal         VABNCL
038000081217     C                   ELSE
038100090309     C                   ADD       1             errore
038200081217     C                   EVAL      vinmsg = %trimr(vinmsg)
038300081217     C                             + ' ' + 'VABNCL'
038400081217     C                   ENDIF
038500090312     C* PKB
038600090609     C                   EVAL      PiStr=%trim(%subst(vindta:999:8))
038700090312     C                   EXSR      CHKNUM
038800090312     C                   IF        PiNum=*on
038900090609     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
039000090527     C                   Z-ADD     PiVal         VABPKB
039100090312     C                   ELSE
039200090312     C                   ADD       1             errore
039300090312     C                   EVAL      vinmsg = %trimr(vinmsg)
039400090312     C                             + ' ' + 'VABPKB'
039500090312     C                   ENDIF
039600090527     C* VLB
039700090609     C                   EVAL      PiStr=%trim(%subst(vindta:989:10))
039800090527     C                   EXSR      CHKNUM
039900090527     C                   IF        PiNum=*on
040000090609     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
040100090527     C                   Z-ADD     PiVal         VABVLB
040200090527     C                   ELSE
040300090527     C                   ADD       1             errore
040400090527     C                   EVAL      vinmsg = %trimr(vinmsg)
040500090527     C                             + ' ' + 'VABVLB'
040600090527     C                   ENDIF
040700090527     C* CAS
040800090609     C                   IF        %subst(vindta:833:13) <> *blanks AND
040900090609     C                             %subst(vindta:833:13) <> *zeros
041000090609     C                   MOVEL     '1'           FlgCAS
041100090527     C                   EVAL      VABVCA = 'EUR'
041200090609     C                   EVAL      PiStr=%trim(%subst(vindta:833:13))
041300090428     C                   EXSR      CHKNUM
041400090428     C                   IF        PiNum=*on
041500090609     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
041600090527     C                   Z-ADD     PiVal         VABCAS
041700090428     C                   ELSE
041800090428     C                   ADD       1             errore
041900090428     C                   EVAL      vinmsg = %trimr(vinmsg)
042000090527     C                             + ' ' + 'VABCAS'
042100090428     C                   ENDIF
042200090428     C                   ENDIF
042300010205     C*
042400010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042500040802     C                   IF        FlgCAS <> '0'
042600090609     C                   IF        VABCBO = '1'
042700010205     C                   EVAL      VABCBO = '4'
042800090609     C                   ENDIF
042900090609     C                   IF        VABCBO = '2'
043000090609     C                   EVAL      VABCBO = '6'
043100090609     C                   ENDIF
043200010205     C                   ENDIF
043300020305     C*
043400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043500011113     C                   EXSR      CHKIMPDIV
043600010202     C*
043700000801     C* Ebbene...
043800000801     C                   ADD       1             §CTRMO
043900010201     C                   IF        errore <> *zeros
044000000801     C                   ADD       1             §CTRNO
044100000801     C                   EVAL      recko = vindta
044200000801     C                   EXCEPT    rigadett
044300000801     C                   EVAL      vinflg = '2'
044400000801     C                   ELSE
044500010201     C                   ADD       1             §CTROKVB
044600000801     C                   ENDIF
044700000801     C*
044800000801     C                   ENDSR
044900070102     C*----------------------------------------------------*
045000090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045100070102     C*----------------------------------------------------*
045200070103     C     EXEVATE       BEGSR
045300090527     C*
045400090527     C                   EVAL      VATTRC = 'E'
045500090428     C*
045600090609     C***                exsr      wrivat                                       => scarico VAT
045700070102     C*
045800070102     C                   ENDSR
045900080415     C*----------------------------------------------------*
046000090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
046100080415     C*----------------------------------------------------*
046200080415     C     EXEVATA       BEGSR
046300080415     C*
046400090504     C                   EVAL      VATTRC = 'A'
046500090609     C                   EVAL      VATNOT = %trim(%subst(vindta:495:22))
046600080415     C*
046700090609     C                   exsr      wrivat                                       => scarico VAT
046800080415     C*
046900080415     C                   ENDSR
047000071121     C*----------------------------------------------------*
047100090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
047200071121     C*----------------------------------------------------*
047300071121     C     EXEVATB       BEGSR
047400071121     C*
047500090504     C                   EVAL      VATTRC = 'B'
047600090609     C                   EVAL      VATNOT = %trim(%subst(vindta:517:16))
047700071121     C*
047800090609     C                   exsr      wrivat                                       => scarico VAT
047900071121     C*
048000071121     C                   ENDSR
048100010201     C*----------------------------------------------------*
048200090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048300010201     C*----------------------------------------------------*
048400020305     C     WRIVAT        BEGSR
048500050628     C*
048600060223     C* Scrivo solo se valorizzato qualcosa
048700060223     C                   IF        VATNOT <> *blanks
048800090114     C                   WRITE     FIVAT000
048900060223     C                   ENDIF
049000010201     C*
049100010201     C                   ENDSR
049200010202     C*----------------------------------------------------*
049300090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049400010202     C*----------------------------------------------------*
049500020305     C     PREVAT        BEGSR
049600010202     C*
049700090114     C* Compongo il nome del membro da dare al FIVATWWR
049800010202     C                   eval      parmbr = vlrhdl
049900010202     C                   movel     'M'           parmbr
050000050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050100010202     C                   eval      paropz = '1'
050200010202     C* Effettuo la chiamata al CLLE preposto
050300090114     C                   call(e)   'TITVVTC'
050400010202     C                   parm                    parccm
050500010202     C                   parm                    parmbr
050600010202     C                   parm                    paropz
050700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050800010202     C                   if        %error
050900010202     C                   movel     '1'           chkcall
051000010202     C                   else
051100010202     C                   movel     '0'           chkcall
051200010202     C                   endif
051300010202     C*
051400010202     C                   ENDSR
051500000801     C*----------------------------------------------------*
051600000801     C*  CONTROLLO NUMERICITA' CAMPI
051700000801     C*----------------------------------------------------*
051800000801     C     CHKNUM        BEGSR
051900081217     C*
052000081217     C                   IF        PiDecChr = *blanks
052100090519     C                   EVAL      PiDecChr = ','
052200081217     C                   ENDIF
052300081217     C*
052400081217     C                   callp(e)  UBISNUM_Check(PiStr
052500081217     C                                          :PiDecChr
052600081217     C                                          :PiVal
052700081217     C                                          :PiNum
052800081217     C                                          :PiInt)
052900081217     C*
053000081217     C                   IF        %error
053100081217     C                   EVAL      PiInt=*off
053200081217     C                   ENDIF
053300000801     C*
053400000801     C                   ENDSR
053500000801     C***
053600000801
053700011113
053800011113     C*----------------------------------------------------*
053900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054000011113     C*----------------------------------------------------*
054100011113     C     CHKIMPDIV     BEGSR
054200011113     C*
054300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054400011113     C                   Z-ADD     *zeros        wrkDec            9 9
054500011113     C*
054600011113     C* Come prima cosa effettuo considerazioni sulla divisa
054700011113     C                   IF        vabIAS > *zeros
054800011113     C                   IF        vabVAS <> 'EUR'
054900011113     C                   EVAL      vabVAS =  'ITL'
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C                   IF        vabCAS > *zeros
055400011113     C                   IF        vabVCA <> 'EUR'
055500011113     C                   EVAL      vabVCA =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C                   IF        vabVMD > *zeros
056000020305     C                   IF        vabVAD <> 'EUR'
056100011113     C                   EVAL      vabVAD =  'ITL'
056200011113     C                   ENDIF
056300011113     C                   ENDIF
056400011113     C*
056500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056600011113     C                   Z-ADD     vabIAS        wrkDec
056700011113     C                   IF        wrkDec > *zeros
056800011113     C                   IF        vabVAS = 'ITL'
056900011113     C                   EVAL      vabIAS = *zeros
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057400011113     C                   Z-ADD     vabCAS        wrkDec
057500011113     C                   IF        wrkDec > *zeros
057600011113     C                   IF        vabVCA = 'ITL'
057700011113     C                   EVAL      vabCAS = *zeros
057800011113     C                   ENDIF
057900011113     C                   ENDIF
058000011113     C*
058100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058200011113     C                   Z-ADD     vabVMD        wrkDec
058300011113     C                   IF        wrkDec > *zeros
058400011113     C                   IF        vabVAD = 'ITL'
058500011113     C                   EVAL      vabVMD = *zeros
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C                   ENDSR
059000011113     C***
059100011113
059200011113
059300000801
059400000801
059500990920      /TITLE Invio dei dati al punto operativo.
059600010202     C     invio         BEGSR
059700990920     C*
059800090114     C* 1° invio FIVAT
059900010201     C                   reset                   dscmz
060000010201     C                   move      vlrpoi        cmzdst
060100090114     C                   eval      cmzfld = 'FIVATWWR'
060200010201     C                   eval      cmzmbd = vlrhdl
060300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060400021009     C***                if        prmfir = *blanks
060500090114     C                   eval      cmzfla = 'FIVAT00F'
060600090114     C                   eval      cmzmba = 'FIVAT00F'
060700021009     C***                else
060800021009     C***                eval      cmzfla = prmfir
060900021009     C***                eval      cmzmba = prmfir
061000021009     C***                endif
061100010201     C                   eval      cmznrr = *zeros
061200020305     C                   move      §ctrokvt      cmznrr
061300021018     C                   eval      cmzlba = vlrfl1
061400010201     C                   call(e)   'TIS711C'
061500010201     C                   parm                    dscmz
061600010201     C                   parm      *blanks       esito
061700010205     C                   if        %error
061800010205     C                             or cmzerr = '1'
061900010205     C                             or esito  = '1'
062000010205     C                   eval      wrkesito = '3'
062100010205     C                   else
062200010201     C*
062300090114     C* 2° invio FIVAB
062400010201     C                   reset                   dscmz
062500010201     C                   move      vlrpoi        cmzdst
062600010201     C                   eval      cmzfld = vlrfou
062700010201     C                   eval      cmzmbd = vlrhdl
062800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062900021009     C***                if        prmfir = *blanks
063000090114     C                   eval      cmzfla = 'FIVAB00F'
063100090114     C                   eval      cmzmba = 'FIVAB00F'
063200021009     C***                else
063300021009     C***                eval      cmzfla = prmfir
063400021009     C***                eval      cmzmba = prmfir
063500021009     C***                endif
063600010201     C                   eval      cmznrr = *zeros
063700010201     C                   move      §ctrokvb      cmznrr
063800021018     C                   eval      cmzlba = vlrfl1
063900010201     C                   call(e)   'TIS711C'
064000010201     C                   parm                    dscmz
064100010201     C                   parm      *blanks       esito
064200010201     C                   if        %error
064300010201     C                             or cmzerr = '1'
064400010201     C                             or esito  = '1'
064500010201     C                   eval      wrkesito = '3'
064600010201     C                   endif
064700010205     C                   endif
064800990920     C*
064900000613     C                   ENDSR
065000000613     C***
065100070411
065200090331
065300090331
065400090331
065500070411     C     *pssr         BEGSR
065600070411     C*
065700070411     C                   if        %open(tivin00r)
065800070411     C                   close     tivin00r
065900070411     C                   endif
066000090114     C                   if        %open(fivabwwr)
066100090114     C                   close     fivabwwr
066200070411     C                   endif
066300090114     C                   if        %open(fivatwwr)
066400090114     C                   close     fivatwwr
066500070411     C                   endif
066600070411     C*
066700070411     C* Effettuo la chiamata al CLLE preposto
066800090114     C                   call(e)   'TITVVTC'
066900070411     C                   parm                    parccm
067000070411     C                   parm                    parmbr
067100070411     C                   parm      '2'           paropz
067200070411     C*
067300070411     C                   eval      wrkesito = '2'
067400070411     C*
067500070411     C                   seton                                        LR
067600070411     C*
067700070411     C                   ENDSR     '*CANCL'
067800070411     C***
067900070411
068000090331
068100090331
068200990910
068300000613     C     *inzsr        BEGSR
068400990910     C*
068500990910     C     *entry        plist
068600990920     C                   parm                    tivlrds
068700990921     C                   parm      wrkesito      esito
068800000724     C                   parm                    prmlit
068900000710     C                   parm                    prmfir
069000000613     C*
069100000830     C* CALCOLA LA DATA CORRENTE
069200000830     C                   time                    wn14             14 0
069300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069400080512     C                   movel     wn14          oracor            6 0          *ORA
069500000830     C                   z-add     wn8           g08dat
069600000830     C                   z-add     *zeros        g08inv
069700000830     C                   movel     '0'           g08err
069800000830     C                   call      'XSRDA8'
069900000830     C                   parm                    wlbda8
070000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
070100000830     C*
070200000613     C                   ENDSR
070300000613     C***
070400990908
070500090331
070600090331
070700090609     Otitv1q1p  E            riepilogo         2
070800990915     O                                              'Upload via Internet'
070900990915     O                                           +1 'Traduzione TIVIN00R -'
071000090114     O                                           +1 'FIVABWWR/FIVATWWR'
071100010201     O                                           +1 'Report testate bolle'
071200990915     O          E            riepilogo   2
071300990915     O                       wrkdata
071400990915     O                       wrkora              +1
071500990915     O                       procname            +1
071600990915     O          E            riepilogo   2
071700990915     O                                              'Cliente..................:'
071800990915     O                       VABCCM        z     +1
071900990915     O          E            riepilogo   2
072000990920     O                                              'Riferimento Strategi.....:'
072100990920     O                       vlrhdl              +1
072200990915     O          E            riepilogo   2
072300990915     O                                              'Giusti...................:'
072400010201     O                       §CTROKVB      2   +  1
072500990915     O          E            riepilogo   2
072600990915     O                                              'Sbagliati e corretti.....:'
072700971022     O                       §CTRMO        2   +  1
072800990915     O          E            riepilogo   2
072900990915     O                                              'Sbagliati e scartati.....:'
073000971022     O                       §CTRNO        2   +  1
073100000613
073200090609     Otitv1q1ps E            testdett          2
073300000613     O                                              'Upload via Internet'
073400000613     O                                           +1 'Traduzione TIVIN00R -'
073500090114     O                                           +1 'FIVABWWR/FIVATWWR'
073600010201     O                                           +1 'Report testate bolle'
073700000616     O          E            testdett    3
073800000613     O                                           +2 'N° rec'
073900000613     O                                           +3 'Anteprima contenuto'
074000000616     O          E            rigadett    2
074100000613     O                       rrnum               +2
074200000621     O                       recko               +3
074300000616     O          E            findett     2
074400000613     O                       wrkdata
074500000613     O                       wrkora              +1
074600000613     O                       procname            +1
074700000616     O          E            findett     2
074800000613     O                                              'Cliente..................:'
074900000613     O                       VABCCM        z     +1
075000000616     O          E            findett     2
075100000613     O                                              'Riferimento Strategi.....:'
075200000613     O                       vlrhdl              +1
075300000616     O          E            findett     2
075400000613     O                                              'Giusti...................:'
075500010201     O                       §CTROKVB      2   +  1
075600000616     O          E            findett     2
075700000613     O                                              'Sbagliati e corretti.....:'
075800000613     O                       §CTRMO        2   +  1
075900000616     O          E            findett     2
076000000613     O                                              'Sbagliati e scartati.....:'
076100000613     O                       §CTRNO        2   +  1
076200000512** CMD - COMANDI CL
076300090609OVRPRTF FILE(TITV1Q1P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076400090609OVRPRTF FILE(TITV1Q1PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076500090609DLTOVR FILE(TITV1Q1P TITV1Q1PS) LVL(*)
076600000512
076700000512
