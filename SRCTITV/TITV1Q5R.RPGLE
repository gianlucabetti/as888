000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090114     FFIVABwwr  O    E             DISK    usropn
000700090114     FFIVATwwr  O    E             DISK    usropn
000800090617     Ftitv1q5p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090617     Ftitv1q5ps O    f  198        PRINTER usropn
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
009600090617     C                   if        not %open(titv1q5ps)
009700090617     C                   open      titv1q5ps
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
011600090617     C                   if        %open(titv1q5ps)
011700000616     C                   except    findett
011800090617     C                   close     titv1q5ps
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
013600090617     C                   if        not %open(titv1q5p)
013700090617     C                   open      titv1q5p
013800990915     C                   endif
013900990915     C*
014000990915     C                   except    riepilogo
014100990915     C*
014200090617     C                   if        %open(titv1q5p)
014300090617     C                   close     titv1q5p
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
027100090617     C                   Z-ADD     2751073       VABCCM
027200090617     C                   Z-ADD     2751073       VATCCM
027300090617     C                   Z-ADD     275           VABLNP
027400090617     C                   Z-ADD     275           VATLNP
027500090519     C                   Z-ADD     000           VABCTR
027600040823     C                   MOVEL     '1'           VABCBO
027700020619     C* ... e poi verifico se sono stati passati come parametri
027800020619     C                   IF        vlrppt > *blanks
027900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028100020619     C                   EXSR      CHKNUM
028200020619     C                   IF        PiInt=*on
028300020619     C                   Z-ADD     PiVal         VABCCM
028400020619     C                   Z-ADD     PiVal         VATCCM
028500020619     C                   ENDIF
028600040506     C                   ENDIF
028700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
028800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028900020619     C                   EXSR      CHKNUM
029000020619     C                   IF        PiInt=*on
029100020619     C                   Z-ADD     PiVal         VABLNP
029200020619     C                   Z-ADD     PiVal         VATLNP
029300040506     C                   ENDIF
029400020619     C                   ENDIF
029500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
029600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029700020619     C                   EXSR      CHKNUM
029800020619     C                   IF        PiInt=*on
029900020619     C                   Z-ADD     PiVal         VABCTR
030000040506     C                   ENDIF
030100020619     C                   ENDIF
030200020619     C                   ENDIF
030300000801     C*
030400000801     C                   ENDSR
030500000801     C*----------------------------------------------------*
030600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030700000801     C*----------------------------------------------------*
030800040823     C     IMPVAB        BEGSR
030900010305     C*
031000000801     C                   Z-ADD     *zeros        errore            1 0
031100040526     C                   MOVE      datcor        VABMGS
031200080409     C*
031300080409     C* Reperimento campi ALFA
031400080409     C*
031500080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
031600090617     C                   EVAL      VABRSD=%trim(%subst(vindta:244:35))
031700080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031800080409     C     '@':'A'       XLATE     VABRSD        VABRSD
031900080409     C* ==
032000090617     C                   EVAL      VABRD2=%trim(%subst(vindta:278:35))
032100090617     C                   EVAL      VABIND=%trim(%subst(vindta:314:50))
032200090617     C                   EVAL      VABLOD=%trim(%subst(vindta:364:40))
032300090617     C                   EVAL      VABPRD=%trim(%subst(vindta:409:2))
032400090617     C                   EVAL      VABNOT=%trim(%subst(vindta:440:35))
032500090617     C                   EVAL      VABNT2=%trim(%subst(vindta:475:35))
032600090629     C                   IF        %trim(%subst(vindta:36:6)) = '0'
032700090629     C                   EVAL      VABRMA=%trim(%subst(vindta:57:7))
032800090629     C                   ELSE
032900090629     C                   EVAL      VABRMA=%trim(%subst(vindta:36:6))
033000090629     C                   ENDIF
033100080409     C*
033200080409     C* Reperimento campi NUMERICI
033300090609     C*
033400090309     C* NSP
033500090629     C                   IF        %trim(%subst(vindta:36:6)) = '0'
033600090629     C                   EVAL      PiStr=%trim(%subst(vindta:57:7))
033700090629     C                   ELSE
033800090629     C                   EVAL      PiStr=%trim(%subst(vindta:36:6))
033900090629     C                   ENDIF
034000090309     C                   EXSR      CHKNUM
034100090309     C                   IF        PiInt=*on
034200090309     C                   Z-ADD     PiVal         VABNSP
034300090309     C                   Z-ADD     PiVal         VATNSP
034400090309     C                   ELSE
034500090309     C                   EVAL      vinmsg = %trimr(vinmsg)
034600090309     C                             + ' ' + 'VABNSP VATNSP'
034700090309     C                   ENDIF
034800090309     C* RMN
034900090629     C                   IF        %trim(%subst(vindta:36:6)) = '0'
035000090629     C                   EVAL      PiStr=%trim(%subst(vindta:57:7))
035100090629     C                   ELSE
035200090629     C                   EVAL      PiStr=%trim(%subst(vindta:36:6))
035300090629     C                   ENDIF
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
036400090617     C                   EVAL      PiStr=%trim(%subst(vindta:404:5))
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
037600090617     C                   EVAL      PiStr=%trim(%subst(vindta:414:4))
037700081217     C                   EXSR      CHKNUM
037800081217     C                   IF        PiInt=*on
037900081217     C                   Z-ADD     PiVal         VABNCL
038000081217     C                   ELSE
038100090309     C                   ADD       1             errore
038200081217     C                   EVAL      vinmsg = %trimr(vinmsg)
038300081217     C                             + ' ' + 'VABNCL'
038400081217     C                   ENDIF
038500090312     C* PKB
038600090617     C                   EVAL      PiStr=%trim(%subst(vindta:420:4))
038700090312     C                   EXSR      CHKNUM
038800090312     C                   IF        PiNum=*on
038900090527     C                   Z-ADD     PiVal         VABPKB
039000090312     C                   ELSE
039100090312     C                   ADD       1             errore
039200090312     C                   EVAL      vinmsg = %trimr(vinmsg)
039300090312     C                             + ' ' + 'VABPKB'
039400090312     C                   ENDIF
039500090527     C* CAS
039600090617     C                   IF        %subst(vindta:426:8) <> *blanks AND
039700090617     C                             %subst(vindta:426:8) <> *zeros  AND
039800090617     C                             %trim(%subst(vindta:426:8)) <> '.00'
039900090609     C                   MOVEL     '1'           FlgCAS
040000090527     C                   EVAL      VABVCA = 'EUR'
040100090907     C                   EVAL      VABTIC = 'BM'
040200090617     C                   EVAL      PiStr=%trim(%subst(vindta:426:8))
040300090428     C                   EXSR      CHKNUM
040400090428     C                   IF        PiNum=*on
040500090527     C                   Z-ADD     PiVal         VABCAS
040600090428     C                   ELSE
040700090428     C                   ADD       1             errore
040800090428     C                   EVAL      vinmsg = %trimr(vinmsg)
040900090527     C                             + ' ' + 'VABCAS'
041000090428     C                   ENDIF
041100090428     C                   ENDIF
041200010205     C*
041300010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041400040802     C                   IF        FlgCAS <> '0'
041500090609     C                   IF        VABCBO = '1'
041600010205     C                   EVAL      VABCBO = '4'
041700090609     C                   ENDIF
041800090609     C                   IF        VABCBO = '2'
041900090609     C                   EVAL      VABCBO = '6'
042000090609     C                   ENDIF
042100010205     C                   ENDIF
042200020305     C*
042300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042400011113     C                   EXSR      CHKIMPDIV
042500010202     C*
042600000801     C* Ebbene...
042700000801     C                   ADD       1             §CTRMO
042800010201     C                   IF        errore <> *zeros
042900000801     C                   ADD       1             §CTRNO
043000000801     C                   EVAL      recko = vindta
043100000801     C                   EXCEPT    rigadett
043200000801     C                   EVAL      vinflg = '2'
043300000801     C                   ELSE
043400010201     C                   ADD       1             §CTROKVB
043500000801     C                   ENDIF
043600000801     C*
043700000801     C                   ENDSR
043800070102     C*----------------------------------------------------*
043900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044000070102     C*----------------------------------------------------*
044100070103     C     EXEVATE       BEGSR
044200090527     C*
044300090617     C***                EVAL      VATTRC = 'E'
044400090617     C***                EVAL      VATNOT = %trim(%subst(vindta:495:22))
044500090428     C*
044600090609     C***                exsr      wrivat                                       => scarico VAT
044700070102     C*
044800070102     C                   ENDSR
044900080415     C*----------------------------------------------------*
045000090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
045100080415     C*----------------------------------------------------*
045200080415     C     EXEVATA       BEGSR
045300080415     C*
045400090617     C***                EVAL      VATTRC = 'A'
045500090617     C***                EVAL      VATNOT = %trim(%subst(vindta:495:22))
045600080415     C*
045700090617     C***                exsr      wrivat                                       => scarico VAT
045800080415     C*
045900080415     C                   ENDSR
046000071121     C*----------------------------------------------------*
046100090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
046200071121     C*----------------------------------------------------*
046300071121     C     EXEVATB       BEGSR
046400071121     C*
046500090617     C***                EVAL      VATTRC = 'B'
046600090617     C***                EVAL      VATNOT = %trim(%subst(vindta:517:16))
046700071121     C*
046800090617     C***                exsr      wrivat                                       => scarico VAT
046900071121     C*
047000071121     C                   ENDSR
047100010201     C*----------------------------------------------------*
047200090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047300010201     C*----------------------------------------------------*
047400020305     C     WRIVAT        BEGSR
047500050628     C*
047600060223     C* Scrivo solo se valorizzato qualcosa
047700060223     C                   IF        VATNOT <> *blanks
047800090114     C                   WRITE     FIVAT000
047900060223     C                   ENDIF
048000010201     C*
048100010201     C                   ENDSR
048200010202     C*----------------------------------------------------*
048300090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048400010202     C*----------------------------------------------------*
048500020305     C     PREVAT        BEGSR
048600010202     C*
048700090114     C* Compongo il nome del membro da dare al FIVATWWR
048800010202     C                   eval      parmbr = vlrhdl
048900010202     C                   movel     'M'           parmbr
049000050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049100010202     C                   eval      paropz = '1'
049200010202     C* Effettuo la chiamata al CLLE preposto
049300090114     C                   call(e)   'TITVVTC'
049400010202     C                   parm                    parccm
049500010202     C                   parm                    parmbr
049600010202     C                   parm                    paropz
049700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049800010202     C                   if        %error
049900010202     C                   movel     '1'           chkcall
050000010202     C                   else
050100010202     C                   movel     '0'           chkcall
050200010202     C                   endif
050300010202     C*
050400010202     C                   ENDSR
050500000801     C*----------------------------------------------------*
050600000801     C*  CONTROLLO NUMERICITA' CAMPI
050700000801     C*----------------------------------------------------*
050800000801     C     CHKNUM        BEGSR
050900081217     C*
051000081217     C                   IF        PiDecChr = *blanks
051100090617     C                   EVAL      PiDecChr = '.'
051200081217     C                   ENDIF
051300081217     C*
051400081217     C                   callp(e)  UBISNUM_Check(PiStr
051500081217     C                                          :PiDecChr
051600081217     C                                          :PiVal
051700081217     C                                          :PiNum
051800081217     C                                          :PiInt)
051900081217     C*
052000081217     C                   IF        %error
052100081217     C                   EVAL      PiInt=*off
052200081217     C                   ENDIF
052300000801     C*
052400000801     C                   ENDSR
052500000801     C***
052600000801
052700011113
052800011113     C*----------------------------------------------------*
052900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053000011113     C*----------------------------------------------------*
053100011113     C     CHKIMPDIV     BEGSR
053200011113     C*
053300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053400011113     C                   Z-ADD     *zeros        wrkDec            9 9
053500011113     C*
053600011113     C* Come prima cosa effettuo considerazioni sulla divisa
053700011113     C                   IF        vabIAS > *zeros
053800011113     C                   IF        vabVAS <> 'EUR'
053900011113     C                   EVAL      vabVAS =  'ITL'
054000011113     C                   ENDIF
054100011113     C                   ENDIF
054200011113     C*
054300011113     C                   IF        vabCAS > *zeros
054400011113     C                   IF        vabVCA <> 'EUR'
054500011113     C                   EVAL      vabVCA =  'ITL'
054600011113     C                   ENDIF
054700011113     C                   ENDIF
054800011113     C*
054900011113     C                   IF        vabVMD > *zeros
055000020305     C                   IF        vabVAD <> 'EUR'
055100011113     C                   EVAL      vabVAD =  'ITL'
055200011113     C                   ENDIF
055300011113     C                   ENDIF
055400011113     C*
055500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055600011113     C                   Z-ADD     vabIAS        wrkDec
055700011113     C                   IF        wrkDec > *zeros
055800011113     C                   IF        vabVAS = 'ITL'
055900011113     C                   EVAL      vabIAS = *zeros
056000011113     C                   ENDIF
056100011113     C                   ENDIF
056200011113     C*
056300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056400011113     C                   Z-ADD     vabCAS        wrkDec
056500011113     C                   IF        wrkDec > *zeros
056600011113     C                   IF        vabVCA = 'ITL'
056700011113     C                   EVAL      vabCAS = *zeros
056800011113     C                   ENDIF
056900011113     C                   ENDIF
057000011113     C*
057100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057200011113     C                   Z-ADD     vabVMD        wrkDec
057300011113     C                   IF        wrkDec > *zeros
057400011113     C                   IF        vabVAD = 'ITL'
057500011113     C                   EVAL      vabVMD = *zeros
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C                   ENDSR
058000011113     C***
058100011113
058200011113
058300000801
058400000801
058500990920      /TITLE Invio dei dati al punto operativo.
058600010202     C     invio         BEGSR
058700990920     C*
058800090114     C* 1° invio FIVAT
058900010201     C                   reset                   dscmz
059000010201     C                   move      vlrpoi        cmzdst
059100090114     C                   eval      cmzfld = 'FIVATWWR'
059200010201     C                   eval      cmzmbd = vlrhdl
059300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059400021009     C***                if        prmfir = *blanks
059500090114     C                   eval      cmzfla = 'FIVAT00F'
059600090114     C                   eval      cmzmba = 'FIVAT00F'
059700021009     C***                else
059800021009     C***                eval      cmzfla = prmfir
059900021009     C***                eval      cmzmba = prmfir
060000021009     C***                endif
060100010201     C                   eval      cmznrr = *zeros
060200020305     C                   move      §ctrokvt      cmznrr
060300021018     C                   eval      cmzlba = vlrfl1
060400010201     C                   call(e)   'TIS711C'
060500010201     C                   parm                    dscmz
060600010201     C                   parm      *blanks       esito
060700010205     C                   if        %error
060800010205     C                             or cmzerr = '1'
060900010205     C                             or esito  = '1'
061000010205     C                   eval      wrkesito = '3'
061100010205     C                   else
061200010201     C*
061300090114     C* 2° invio FIVAB
061400010201     C                   reset                   dscmz
061500010201     C                   move      vlrpoi        cmzdst
061600010201     C                   eval      cmzfld = vlrfou
061700010201     C                   eval      cmzmbd = vlrhdl
061800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061900021009     C***                if        prmfir = *blanks
062000090114     C                   eval      cmzfla = 'FIVAB00F'
062100090114     C                   eval      cmzmba = 'FIVAB00F'
062200021009     C***                else
062300021009     C***                eval      cmzfla = prmfir
062400021009     C***                eval      cmzmba = prmfir
062500021009     C***                endif
062600010201     C                   eval      cmznrr = *zeros
062700010201     C                   move      §ctrokvb      cmznrr
062800021018     C                   eval      cmzlba = vlrfl1
062900010201     C                   call(e)   'TIS711C'
063000010201     C                   parm                    dscmz
063100010201     C                   parm      *blanks       esito
063200010201     C                   if        %error
063300010201     C                             or cmzerr = '1'
063400010201     C                             or esito  = '1'
063500010201     C                   eval      wrkesito = '3'
063600010201     C                   endif
063700010205     C                   endif
063800990920     C*
063900000613     C                   ENDSR
064000000613     C***
064100070411
064200090331
064300090331
064400090331
064500070411     C     *pssr         BEGSR
064600070411     C*
064700070411     C                   if        %open(tivin00r)
064800070411     C                   close     tivin00r
064900070411     C                   endif
065000090114     C                   if        %open(fivabwwr)
065100090114     C                   close     fivabwwr
065200070411     C                   endif
065300090114     C                   if        %open(fivatwwr)
065400090114     C                   close     fivatwwr
065500070411     C                   endif
065600070411     C*
065700070411     C* Effettuo la chiamata al CLLE preposto
065800090114     C                   call(e)   'TITVVTC'
065900070411     C                   parm                    parccm
066000070411     C                   parm                    parmbr
066100070411     C                   parm      '2'           paropz
066200070411     C*
066300070411     C                   eval      wrkesito = '2'
066400070411     C*
066500070411     C                   seton                                        LR
066600070411     C*
066700070411     C                   ENDSR     '*CANCL'
066800070411     C***
066900070411
067000090331
067100090331
067200990910
067300000613     C     *inzsr        BEGSR
067400990910     C*
067500990910     C     *entry        plist
067600990920     C                   parm                    tivlrds
067700990921     C                   parm      wrkesito      esito
067800000724     C                   parm                    prmlit
067900000710     C                   parm                    prmfir
068000000613     C*
068100000830     C* CALCOLA LA DATA CORRENTE
068200000830     C                   time                    wn14             14 0
068300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068400080512     C                   movel     wn14          oracor            6 0          *ORA
068500000830     C                   z-add     wn8           g08dat
068600000830     C                   z-add     *zeros        g08inv
068700000830     C                   movel     '0'           g08err
068800000830     C                   call      'XSRDA8'
068900000830     C                   parm                    wlbda8
069000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069100000830     C*
069200000613     C                   ENDSR
069300000613     C***
069400990908
069500090331
069600090331
069700090617     Otitv1q5p  E            riepilogo         2
069800990915     O                                              'Upload via Internet'
069900990915     O                                           +1 'Traduzione TIVIN00R -'
070000090114     O                                           +1 'FIVABWWR/FIVATWWR'
070100010201     O                                           +1 'Report testate bolle'
070200990915     O          E            riepilogo   2
070300990915     O                       wrkdata
070400990915     O                       wrkora              +1
070500990915     O                       procname            +1
070600990915     O          E            riepilogo   2
070700990915     O                                              'Cliente..................:'
070800990915     O                       VABCCM        z     +1
070900990915     O          E            riepilogo   2
071000990920     O                                              'Riferimento Strategi.....:'
071100990920     O                       vlrhdl              +1
071200990915     O          E            riepilogo   2
071300990915     O                                              'Giusti...................:'
071400010201     O                       §CTROKVB      2   +  1
071500990915     O          E            riepilogo   2
071600990915     O                                              'Sbagliati e corretti.....:'
071700971022     O                       §CTRMO        2   +  1
071800990915     O          E            riepilogo   2
071900990915     O                                              'Sbagliati e scartati.....:'
072000971022     O                       §CTRNO        2   +  1
072100000613
072200090617     Otitv1q5ps E            testdett          2
072300000613     O                                              'Upload via Internet'
072400000613     O                                           +1 'Traduzione TIVIN00R -'
072500090114     O                                           +1 'FIVABWWR/FIVATWWR'
072600010201     O                                           +1 'Report testate bolle'
072700000616     O          E            testdett    3
072800000613     O                                           +2 'N° rec'
072900000613     O                                           +3 'Anteprima contenuto'
073000000616     O          E            rigadett    2
073100000613     O                       rrnum               +2
073200000621     O                       recko               +3
073300000616     O          E            findett     2
073400000613     O                       wrkdata
073500000613     O                       wrkora              +1
073600000613     O                       procname            +1
073700000616     O          E            findett     2
073800000613     O                                              'Cliente..................:'
073900000613     O                       VABCCM        z     +1
074000000616     O          E            findett     2
074100000613     O                                              'Riferimento Strategi.....:'
074200000613     O                       vlrhdl              +1
074300000616     O          E            findett     2
074400000613     O                                              'Giusti...................:'
074500010201     O                       §CTROKVB      2   +  1
074600000616     O          E            findett     2
074700000613     O                                              'Sbagliati e corretti.....:'
074800000613     O                       §CTRMO        2   +  1
074900000616     O          E            findett     2
075000000613     O                                              'Sbagliati e scartati.....:'
075100000613     O                       §CTRNO        2   +  1
075200000512** CMD - COMANDI CL
075300090617OVRPRTF FILE(TITV1Q5P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075400090617OVRPRTF FILE(TITV1Q5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075500090617DLTOVR FILE(TITV1Q5P TITV1Q5PS) LVL(*)
075600000512
075700000512
