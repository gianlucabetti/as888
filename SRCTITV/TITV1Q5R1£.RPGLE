000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
031600100531     C                   EVAL      VABRSD=%trim(%subst(vindta:236:35))
031700080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031800080409     C     '@':'A'       XLATE     VABRSD        VABRSD
031900080409     C* ==
032000100531     C                   EVAL      VABRD2=%trim(%subst(vindta:271:35))
032100100531     C                   EVAL      VABIND=%trim(%subst(vindta:306:50))
032200100531     C                   EVAL      VABLOD=%trim(%subst(vindta:356:40))
032300100531     C                   EVAL      VABPRD=%trim(%subst(vindta:401:2))
032400100531     C                   EVAL      VABNOT=%trim(%subst(vindta:422:35))
032500100531     C                   EVAL      VABNT2=%trim(%subst(vindta:457:35))
032600100531     C                   EVAL      VABRMA=%trim(%subst(vindta:35:7))
032700080409     C*
032800080409     C* Reperimento campi NUMERICI
032900090609     C*
033000090309     C* NSP
033100100531     C                   EVAL      PiStr=%trim(%subst(vindta:35:7))
033200090309     C                   EXSR      CHKNUM
033300090309     C                   IF        PiInt=*on
033400090309     C                   Z-ADD     PiVal         VABNSP
033500090309     C                   Z-ADD     PiVal         VATNSP
033600090309     C                   ELSE
033700090309     C                   EVAL      vinmsg = %trimr(vinmsg)
033800090309     C                             + ' ' + 'VABNSP VATNSP'
033900090309     C                   ENDIF
034000090309     C* RMN
034100100531     C                   EVAL      PiStr=%trim(%subst(vindta:35:7))
034200081217     C                   EXSR      CHKNUM
034300081217     C                   IF        PiInt=*on
034400081217     C                   Z-ADD     PiVal         VABRMN
034500081217     C                   ELSE
034600081217     C                   ADD       1             errore
034700081217     C                   Z-ADD     1             VABRMN
034800081217     C                   EVAL      vinmsg = %trimr(vinmsg)
034900081217     C                             + ' ' + 'VABRMN'
035000081217     C                   ENDIF
035100080409     C* CAD
035200100531     C                   EVAL      PiStr=%trim(%subst(vindta:396:5))
035300090114     C                   EXSR      CHKNUM
035400090114     C                   IF        PiInt=*on
035500090114     C                   Z-ADD     PiVal         Num5_0
035600090114     C                   MOVEL(p)  Num5_0        VABCAD
035700090114     C                   ELSE
035800090114     C                   ADD       1             errore
035900090114     C                   EVAL      VABCAD = *zeros
036000090114     C                   EVAL      vinmsg = %trimr(vinmsg)
036100090114     C                             + ' ' + 'VABCAD'
036200090114     C                   ENDIF
036300081217     C* NCL
036400100531     C                   EVAL      PiStr=%trim(%subst(vindta:406:4))
036500081217     C                   EXSR      CHKNUM
036600081217     C                   IF        PiInt=*on
036700081217     C                   Z-ADD     PiVal         VABNCL
036800081217     C                   ELSE
036900090309     C                   ADD       1             errore
037000081217     C                   EVAL      vinmsg = %trimr(vinmsg)
037100081217     C                             + ' ' + 'VABNCL'
037200081217     C                   ENDIF
037300090312     C* PKB
037400100531     C                   EVAL      PiStr=%trim(%subst(vindta:410:4))
037500090312     C                   EXSR      CHKNUM
037600090312     C                   IF        PiNum=*on
037700090527     C                   Z-ADD     PiVal         VABPKB
037800090312     C                   ELSE
037900090312     C                   ADD       1             errore
038000090312     C                   EVAL      vinmsg = %trimr(vinmsg)
038100090312     C                             + ' ' + 'VABPKB'
038200090312     C                   ENDIF
038300090527     C* CAS
038400100531     C                   IF        %subst(vindta:414:8)        <> *blanks AND
038500100531     C                             %trim(%subst(vindta:414:8)) <> *zeros
038600090609     C                   MOVEL     '1'           FlgCAS
038700090527     C                   EVAL      VABVCA = 'EUR'
038800090907     C                   EVAL      VABTIC = 'BM'
038900100531     C                   EVAL      PiStr=%trim(%subst(vindta:414:8))
039000090428     C                   EXSR      CHKNUM
039100090428     C                   IF        PiNum=*on
039200100531     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
039300090527     C                   Z-ADD     PiVal         VABCAS
039400090428     C                   ELSE
039500090428     C                   ADD       1             errore
039600090428     C                   EVAL      vinmsg = %trimr(vinmsg)
039700090527     C                             + ' ' + 'VABCAS'
039800090428     C                   ENDIF
039900090428     C                   ENDIF
040000010205     C*
040100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
040200040802     C                   IF        FlgCAS <> '0'
040300090609     C                   IF        VABCBO = '1'
040400010205     C                   EVAL      VABCBO = '4'
040500090609     C                   ENDIF
040600090609     C                   IF        VABCBO = '2'
040700090609     C                   EVAL      VABCBO = '6'
040800090609     C                   ENDIF
040900010205     C                   ENDIF
041000020305     C*
041100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041200011113     C                   EXSR      CHKIMPDIV
041300010202     C*
041400000801     C* Ebbene...
041500000801     C                   ADD       1             §CTRMO
041600010201     C                   IF        errore <> *zeros
041700000801     C                   ADD       1             §CTRNO
041800000801     C                   EVAL      recko = vindta
041900000801     C                   EXCEPT    rigadett
042000000801     C                   EVAL      vinflg = '2'
042100000801     C                   ELSE
042200010201     C                   ADD       1             §CTROKVB
042300000801     C                   ENDIF
042400000801     C*
042500000801     C                   ENDSR
042600070102     C*----------------------------------------------------*
042700090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
042800070102     C*----------------------------------------------------*
042900070103     C     EXEVATE       BEGSR
043000090527     C*
043100090617     C***                EVAL      VATTRC = 'E'
043200090617     C***                EVAL      VATNOT = %trim(%subst(vindta:495:22))
043300090428     C*
043400090609     C***                exsr      wrivat                                       => scarico VAT
043500070102     C*
043600070102     C                   ENDSR
043700080415     C*----------------------------------------------------*
043800090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
043900080415     C*----------------------------------------------------*
044000080415     C     EXEVATA       BEGSR
044100080415     C*
044200090617     C***                EVAL      VATTRC = 'A'
044300090617     C***                EVAL      VATNOT = %trim(%subst(vindta:495:22))
044400080415     C*
044500090617     C***                exsr      wrivat                                       => scarico VAT
044600080415     C*
044700080415     C                   ENDSR
044800071121     C*----------------------------------------------------*
044900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
045000071121     C*----------------------------------------------------*
045100071121     C     EXEVATB       BEGSR
045200071121     C*
045300090617     C***                EVAL      VATTRC = 'B'
045400090617     C***                EVAL      VATNOT = %trim(%subst(vindta:517:16))
045500071121     C*
045600090617     C***                exsr      wrivat                                       => scarico VAT
045700071121     C*
045800071121     C                   ENDSR
045900010201     C*----------------------------------------------------*
046000090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046100010201     C*----------------------------------------------------*
046200020305     C     WRIVAT        BEGSR
046300050628     C*
046400060223     C* Scrivo solo se valorizzato qualcosa
046500060223     C                   IF        VATNOT <> *blanks
046600090114     C                   WRITE     FIVAT000
046700060223     C                   ENDIF
046800010201     C*
046900010201     C                   ENDSR
047000010202     C*----------------------------------------------------*
047100090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047200010202     C*----------------------------------------------------*
047300020305     C     PREVAT        BEGSR
047400010202     C*
047500090114     C* Compongo il nome del membro da dare al FIVATWWR
047600010202     C                   eval      parmbr = vlrhdl
047700010202     C                   movel     'M'           parmbr
047800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
047900010202     C                   eval      paropz = '1'
048000010202     C* Effettuo la chiamata al CLLE preposto
048100090114     C                   call(e)   'TITVVTC'
048200010202     C                   parm                    parccm
048300010202     C                   parm                    parmbr
048400010202     C                   parm                    paropz
048500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048600010202     C                   if        %error
048700010202     C                   movel     '1'           chkcall
048800010202     C                   else
048900010202     C                   movel     '0'           chkcall
049000010202     C                   endif
049100010202     C*
049200010202     C                   ENDSR
049300000801     C*----------------------------------------------------*
049400000801     C*  CONTROLLO NUMERICITA' CAMPI
049500000801     C*----------------------------------------------------*
049600000801     C     CHKNUM        BEGSR
049700081217     C*
049800081217     C                   IF        PiDecChr = *blanks
049900090617     C                   EVAL      PiDecChr = '.'
050000081217     C                   ENDIF
050100081217     C*
050200081217     C                   callp(e)  UBISNUM_Check(PiStr
050300081217     C                                          :PiDecChr
050400081217     C                                          :PiVal
050500081217     C                                          :PiNum
050600081217     C                                          :PiInt)
050700081217     C*
050800081217     C                   IF        %error
050900081217     C                   EVAL      PiInt=*off
051000081217     C                   ENDIF
051100000801     C*
051200000801     C                   ENDSR
051300000801     C***
051400000801
051500011113
051600011113     C*----------------------------------------------------*
051700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051800011113     C*----------------------------------------------------*
051900011113     C     CHKIMPDIV     BEGSR
052000011113     C*
052100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052200011113     C                   Z-ADD     *zeros        wrkDec            9 9
052300011113     C*
052400011113     C* Come prima cosa effettuo considerazioni sulla divisa
052500011113     C                   IF        vabIAS > *zeros
052600011113     C                   IF        vabVAS <> 'EUR'
052700011113     C                   EVAL      vabVAS =  'ITL'
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C                   IF        vabCAS > *zeros
053200011113     C                   IF        vabVCA <> 'EUR'
053300011113     C                   EVAL      vabVCA =  'ITL'
053400011113     C                   ENDIF
053500011113     C                   ENDIF
053600011113     C*
053700011113     C                   IF        vabVMD > *zeros
053800020305     C                   IF        vabVAD <> 'EUR'
053900011113     C                   EVAL      vabVAD =  'ITL'
054000011113     C                   ENDIF
054100011113     C                   ENDIF
054200011113     C*
054300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054400011113     C                   Z-ADD     vabIAS        wrkDec
054500011113     C                   IF        wrkDec > *zeros
054600011113     C                   IF        vabVAS = 'ITL'
054700011113     C                   EVAL      vabIAS = *zeros
054800011113     C                   ENDIF
054900011113     C                   ENDIF
055000011113     C*
055100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055200011113     C                   Z-ADD     vabCAS        wrkDec
055300011113     C                   IF        wrkDec > *zeros
055400011113     C                   IF        vabVCA = 'ITL'
055500011113     C                   EVAL      vabCAS = *zeros
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056000011113     C                   Z-ADD     vabVMD        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVAD = 'ITL'
056300011113     C                   EVAL      vabVMD = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C                   ENDSR
056800011113     C***
056900011113
057000011113
057100000801
057200000801
057300990920      /TITLE Invio dei dati al punto operativo.
057400010202     C     invio         BEGSR
057500990920     C*
057600090114     C* 1° invio FIVAT
057700010201     C                   reset                   dscmz
057800010201     C                   move      vlrpoi        cmzdst
057900090114     C                   eval      cmzfld = 'FIVATWWR'
058000010201     C                   eval      cmzmbd = vlrhdl
058100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058200021009     C***                if        prmfir = *blanks
058300090114     C                   eval      cmzfla = 'FIVAT00F'
058400090114     C                   eval      cmzmba = 'FIVAT00F'
058500021009     C***                else
058600021009     C***                eval      cmzfla = prmfir
058700021009     C***                eval      cmzmba = prmfir
058800021009     C***                endif
058900010201     C                   eval      cmznrr = *zeros
059000020305     C                   move      §ctrokvt      cmznrr
059100021018     C                   eval      cmzlba = vlrfl1
059200010201     C                   call(e)   'TIS711C'
059300010201     C                   parm                    dscmz
059400010201     C                   parm      *blanks       esito
059500010205     C                   if        %error
059600010205     C                             or cmzerr = '1'
059700010205     C                             or esito  = '1'
059800010205     C                   eval      wrkesito = '3'
059900010205     C                   else
060000010201     C*
060100090114     C* 2° invio FIVAB
060200010201     C                   reset                   dscmz
060300010201     C                   move      vlrpoi        cmzdst
060400010201     C                   eval      cmzfld = vlrfou
060500010201     C                   eval      cmzmbd = vlrhdl
060600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060700021009     C***                if        prmfir = *blanks
060800090114     C                   eval      cmzfla = 'FIVAB00F'
060900090114     C                   eval      cmzmba = 'FIVAB00F'
061000021009     C***                else
061100021009     C***                eval      cmzfla = prmfir
061200021009     C***                eval      cmzmba = prmfir
061300021009     C***                endif
061400010201     C                   eval      cmznrr = *zeros
061500010201     C                   move      §ctrokvb      cmznrr
061600021018     C                   eval      cmzlba = vlrfl1
061700010201     C                   call(e)   'TIS711C'
061800010201     C                   parm                    dscmz
061900010201     C                   parm      *blanks       esito
062000010201     C                   if        %error
062100010201     C                             or cmzerr = '1'
062200010201     C                             or esito  = '1'
062300010201     C                   eval      wrkesito = '3'
062400010201     C                   endif
062500010205     C                   endif
062600990920     C*
062700000613     C                   ENDSR
062800000613     C***
062900070411
063000090331
063100090331
063200090331
063300070411     C     *pssr         BEGSR
063400070411     C*
063500070411     C                   if        %open(tivin00r)
063600070411     C                   close     tivin00r
063700070411     C                   endif
063800090114     C                   if        %open(fivabwwr)
063900090114     C                   close     fivabwwr
064000070411     C                   endif
064100090114     C                   if        %open(fivatwwr)
064200090114     C                   close     fivatwwr
064300070411     C                   endif
064400070411     C*
064500070411     C* Effettuo la chiamata al CLLE preposto
064600090114     C                   call(e)   'TITVVTC'
064700070411     C                   parm                    parccm
064800070411     C                   parm                    parmbr
064900070411     C                   parm      '2'           paropz
065000070411     C*
065100070411     C                   eval      wrkesito = '2'
065200070411     C*
065300070411     C                   seton                                        LR
065400070411     C*
065500070411     C                   ENDSR     '*CANCL'
065600070411     C***
065700070411
065800090331
065900090331
066000990910
066100000613     C     *inzsr        BEGSR
066200990910     C*
066300990910     C     *entry        plist
066400990920     C                   parm                    tivlrds
066500990921     C                   parm      wrkesito      esito
066600000724     C                   parm                    prmlit
066700000710     C                   parm                    prmfir
066800000613     C*
066900000830     C* CALCOLA LA DATA CORRENTE
067000000830     C                   time                    wn14             14 0
067100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067200080512     C                   movel     wn14          oracor            6 0          *ORA
067300000830     C                   z-add     wn8           g08dat
067400000830     C                   z-add     *zeros        g08inv
067500000830     C                   movel     '0'           g08err
067600000830     C                   call      'XSRDA8'
067700000830     C                   parm                    wlbda8
067800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067900000830     C*
068000000613     C                   ENDSR
068100000613     C***
068200990908
068300090331
068400090331
068500090617     Otitv1q5p  E            riepilogo         2
068600990915     O                                              'Upload via Internet'
068700990915     O                                           +1 'Traduzione TIVIN00R -'
068800090114     O                                           +1 'FIVABWWR/FIVATWWR'
068900010201     O                                           +1 'Report testate bolle'
069000990915     O          E            riepilogo   2
069100990915     O                       wrkdata
069200990915     O                       wrkora              +1
069300990915     O                       procname            +1
069400990915     O          E            riepilogo   2
069500990915     O                                              'Cliente..................:'
069600990915     O                       VABCCM        z     +1
069700990915     O          E            riepilogo   2
069800990920     O                                              'Riferimento Strategi.....:'
069900990920     O                       vlrhdl              +1
070000990915     O          E            riepilogo   2
070100990915     O                                              'Giusti...................:'
070200010201     O                       §CTROKVB      2   +  1
070300990915     O          E            riepilogo   2
070400990915     O                                              'Sbagliati e corretti.....:'
070500971022     O                       §CTRMO        2   +  1
070600990915     O          E            riepilogo   2
070700990915     O                                              'Sbagliati e scartati.....:'
070800971022     O                       §CTRNO        2   +  1
070900000613
071000090617     Otitv1q5ps E            testdett          2
071100000613     O                                              'Upload via Internet'
071200000613     O                                           +1 'Traduzione TIVIN00R -'
071300090114     O                                           +1 'FIVABWWR/FIVATWWR'
071400010201     O                                           +1 'Report testate bolle'
071500000616     O          E            testdett    3
071600000613     O                                           +2 'N° rec'
071700000613     O                                           +3 'Anteprima contenuto'
071800000616     O          E            rigadett    2
071900000613     O                       rrnum               +2
072000000621     O                       recko               +3
072100000616     O          E            findett     2
072200000613     O                       wrkdata
072300000613     O                       wrkora              +1
072400000613     O                       procname            +1
072500000616     O          E            findett     2
072600000613     O                                              'Cliente..................:'
072700000613     O                       VABCCM        z     +1
072800000616     O          E            findett     2
072900000613     O                                              'Riferimento Strategi.....:'
073000000613     O                       vlrhdl              +1
073100000616     O          E            findett     2
073200000613     O                                              'Giusti...................:'
073300010201     O                       §CTROKVB      2   +  1
073400000616     O          E            findett     2
073500000613     O                                              'Sbagliati e corretti.....:'
073600000613     O                       §CTRMO        2   +  1
073700000616     O          E            findett     2
073800000613     O                                              'Sbagliati e scartati.....:'
073900000613     O                       §CTRNO        2   +  1
074000000512** CMD - COMANDI CL
074100130114OVRPRTF FILE(TITV1Q5P)  TOFILE(TIS7T7P)  OVRSCOPE(*JOB) FORMTYPE(RICCLI) OUTQ(
074200130114OVRPRTF FILE(TITV1Q5PS) TOFILE(TIS7T7PS) OVRSCOPE(*JOB) FORMTYPE(RICCLI) OUTQ(
074300090617DLTOVR FILE(TITV1Q5P TITV1Q5PS) LVL(*)
074400000512
074500000512
