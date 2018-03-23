000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090114     FFIVABwwr  O    E             DISK    usropn
000700090114     FFIVATwwr  O    E             DISK    usropn
000800090715     Ftitv1r2p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090715     Ftitv1r2ps O    f  198        PRINTER usropn
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
009600090715     C                   if        not %open(titv1r2ps)
009700090715     C                   open      titv1r2ps
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
011600090715     C                   if        %open(titv1r2ps)
011700000616     C                   except    findett
011800090715     C                   close     titv1r2ps
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
013600090715     C                   if        not %open(titv1r2p)
013700090715     C                   open      titv1r2p
013800990915     C                   endif
013900990915     C*
014000990915     C                   except    riepilogo
014100990915     C*
014200090715     C                   if        %open(titv1r2p)
014300090715     C                   close     titv1r2p
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
019500090714     C                   exsr      wrivab                                       => scarico VAB
019600090715     C                   exsr      exevate
019700090714     C                   exsr      exevata
019800090714     C                   exsr      exevatb
019900000905     C*
020000000905     C                   else
020100000905     C                   eval      vinflg = '1'
020200050628     C                   endif
020300000905     C                   endif
020400000905     C*
020500000905     C  N70              update    tivin000
020600000905     C*
020700991022     C  N70              ENDdo
020800010202     C*
020900010202     C                   endif
021000990910
021100990910     C* Se non ci sono record con errori ...
021200000710     C                   if        §ctrno = 0
021300990910     C* ... restituisco esito OK.
021400990921     C                   eval      wrkesito = '0'
021500990910     C                   else
021600010201     C                   if        §ctrokvb > 0
021700990921     C                   eval      wrkesito = '1'
021800000710     C                   else
021900000710     C                   eval      wrkesito = '2'
022000990910     C                   endif
022100000710     C                   endif
022200990910     C*
022300990914     C                   if        %open(tivin00r)
022400990908     C                   close     tivin00r
022500990914     C                   endif
022600090114     C                   if        %open(fivabwwr)
022700090114     C                   close     fivabwwr
022800990914     C                   endif
022900090114     C                   if        %open(fivatwwr)
023000090114     C                   close     fivatwwr
023100010201     C                   endif
023200990910     C*
023300010201     C                   if        §ctrokvb > 0
023400000724     C                             and vlrpoi <> *zeros
023500010202     C                   exsr      invio
023600990920     C                   endif
023700990920     C*
023800910830     C                   ENDSR
023900000613     C***
024000010305
024100010305     C*----------------------------------------------------*
024200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024300010305     C*----------------------------------------------------*
024400020305     C     WRIVAB        BEGSR
024500010305     C*
024600060225     C* Quindi scarico il buffer del file d testata
024700090114     C                   write     fivab000                                     => scarico il VAB
024800010305     C*
024900010305     C                   ENDSR
025000990920
025100000801     C*----------------------------------------------------*
025200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025300000801     C*----------------------------------------------------*
025400010201     C     INZVAR        BEGSR
025500000801     C*
025600090114     C                   CLEAR                   FIVAB000
025700090114     C                   CLEAR                   FIVAT000
025800070213     C*
025900040802     C                   Z-ADD     *zeros        Num5_0            5 0
026000040802     C                   MOVEL     '0'           FlgCAS            1
026100000801     C*
026200000801     C                   ENDSR
026300000801     C*----------------------------------------------------*
026400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026500000801     C*----------------------------------------------------*
026600000801     C     DEFCAM        BEGSR
026700000801     C*
026800020619     C* Imposto i valori di default...
026900090527     C                   MOVEL     datcor        VABAAS
027000090527     C                   MOVEL     datcor        VATAAS
027100090527     C                   MOVE(P)   vlrpoi        VABFGS
027200090527     C                   MOVE(P)   vlrpoi        VATFGS
027300090527     C*
027400090715     C                   Z-ADD     0055202       VABCCM
027500090715     C                   Z-ADD     0055202       VATCCM
027600090715     C                   Z-ADD     005           VABLNP
027700090715     C                   Z-ADD     005           VATLNP
027800090519     C                   Z-ADD     000           VABCTR
027900040823     C                   MOVEL     '1'           VABCBO
028000020619     C* ... e poi verifico se sono stati passati come parametri
028100020619     C                   IF        vlrppt > *blanks
028200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028400020619     C                   EXSR      CHKNUM
028500020619     C                   IF        PiInt=*on
028600020619     C                   Z-ADD     PiVal         VABCCM
028700020619     C                   Z-ADD     PiVal         VATCCM
028800020619     C                   ENDIF
028900040506     C                   ENDIF
029000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029200020619     C                   EXSR      CHKNUM
029300020619     C                   IF        PiInt=*on
029400020619     C                   Z-ADD     PiVal         VABLNP
029500020619     C                   Z-ADD     PiVal         VATLNP
029600040506     C                   ENDIF
029700020619     C                   ENDIF
029800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
029900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030000020619     C                   EXSR      CHKNUM
030100020619     C                   IF        PiInt=*on
030200020619     C                   Z-ADD     PiVal         VABCTR
030300040506     C                   ENDIF
030400020619     C                   ENDIF
030500020619     C                   ENDIF
030600000801     C*
030700000801     C                   ENDSR
030800000801     C*----------------------------------------------------*
030900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031000000801     C*----------------------------------------------------*
031100040823     C     IMPVAB        BEGSR
031200010305     C*
031300000801     C                   Z-ADD     *zeros        errore            1 0
031400040526     C                   MOVE      datcor        VABMGS
031500080409     C*
031600080409     C* Reperimento campi ALFA
031700080409     C*
031800080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
031900090715     C                   EVAL      VABRSD=%trim(%subst(vindta:16:25))
032000080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032100080409     C     '@':'A'       XLATE     VABRSD        VABRSD
032200080409     C* ==
032300091029     C***                EVAL      VABIND=%trim(%subst(vindta:41:25))
032400091029     C                   EVAL      VABIND=%trim(%subst(vindta:416:35))
032500090715     C                   EVAL      VABLOD=%trim(%subst(vindta:71:20))
032600090715     C                   EVAL      VABPRD=%trim(%subst(vindta:91:2))
032700090715     C                   EVAL      VABTSP=%trim(%subst(vindta:93:1))
032800090715     C                   EVAL      VABNAS=%trim(%subst(vindta:107:15))
032900090715     C                   EVAL      VABRMA=%trim(%subst(vindta:160:9))
033000090715     C                   EVAL      VABRMO=%trim(%subst(vindta:169:25))
033100090715     C                   MOVEL     *blanks       wNOTE            70
033200090715     C                   EVAL      wNOTE=%trim(%subst(vindta:325:91))
033300090715     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
033400090715     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:35))
033500090715     C*
033600090715     C                   SELECT
033700090715     C                   WHEN      %trim(%subst(vindta:229:1))='1'
033800090715     C                   EVAL      VABCBO='1'
033900090715     C                   WHEN      %trim(%subst(vindta:229:1))='3'
034000090715     C                   EVAL      VABCBO='2'
034100090715     C                   ENDSL
034200080409     C*
034300080409     C* Reperimento campi NUMERICI
034400090609     C*
034500090309     C* NSP
034600090715     C                   EVAL      PiStr=%trim(%subst(vindta:9:7))
034700090309     C                   EXSR      CHKNUM
034800090309     C                   IF        PiInt=*on
034900090309     C                   Z-ADD     PiVal         VABNSP
035000090309     C                   Z-ADD     PiVal         VATNSP
035100090309     C                   ELSE
035200090309     C                   EVAL      vinmsg = %trimr(vinmsg)
035300090309     C                             + ' ' + 'VABNSP VATNSP'
035400090309     C                   ENDIF
035500090309     C* RMN
035600090715     C                   EVAL      PiStr=%trim(%subst(vindta:151:9))
035700081217     C                   EXSR      CHKNUM
035800081217     C                   IF        PiInt=*on
035900081217     C                   Z-ADD     PiVal         VABRMN
036000081217     C                   ELSE
036100081217     C                   ADD       1             errore
036200081217     C                   Z-ADD     1             VABRMN
036300081217     C                   EVAL      vinmsg = %trimr(vinmsg)
036400081217     C                             + ' ' + 'VABRMN'
036500081217     C                   ENDIF
036600080409     C* CAD
036700090715     C                   EVAL      PiStr=%trim(%subst(vindta:66:5))
036800090114     C                   EXSR      CHKNUM
036900090114     C                   IF        PiInt=*on
037000090114     C                   Z-ADD     PiVal         Num5_0
037100090114     C                   MOVEL(p)  Num5_0        VABCAD
037200090114     C                   ELSE
037300090114     C                   ADD       1             errore
037400090114     C                   EVAL      VABCAD = *zeros
037500090114     C                   EVAL      vinmsg = %trimr(vinmsg)
037600090114     C                             + ' ' + 'VABCAD'
037700090114     C                   ENDIF
037800081217     C* NCL
037900090715     C                   EVAL      PiStr=%trim(%subst(vindta:122:5))
038000081217     C                   EXSR      CHKNUM
038100081217     C                   IF        PiInt=*on
038200081217     C                   Z-ADD     PiVal         VABNCL
038300081217     C                   ELSE
038400090309     C                   ADD       1             errore
038500081217     C                   EVAL      vinmsg = %trimr(vinmsg)
038600081217     C                             + ' ' + 'VABNCL'
038700081217     C                   ENDIF
038800090312     C* PKB
038900090715     C                   EVAL      PiStr=%trim(%subst(vindta:127:6))
039000090312     C                   EXSR      CHKNUM
039100090312     C                   IF        PiNum=*on
039200090715     C                   EVAL      PiVal=PiVal/10                               * gestisco 1 dec.
039300090527     C                   Z-ADD     PiVal         VABPKB
039400090312     C                   ELSE
039500090312     C                   ADD       1             errore
039600090312     C                   EVAL      vinmsg = %trimr(vinmsg)
039700090312     C                             + ' ' + 'VABPKB'
039800090312     C                   ENDIF
039900090527     C* VLB
040000090715     C                   EVAL      PiStr=%trim(%subst(vindta:133:5))
040100090527     C                   EXSR      CHKNUM
040200090527     C                   IF        PiNum=*on
040300090715     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
040400090527     C                   Z-ADD     PiVal         VABVLB
040500090527     C                   ELSE
040600090527     C                   ADD       1             errore
040700090527     C                   EVAL      vinmsg = %trimr(vinmsg)
040800090527     C                             + ' ' + 'VABVLB'
040900090527     C                   ENDIF
041000090527     C* CAS
041100090715     C                   IF        %subst(vindta:138:12) <> *blanks AND
041200090715     C                             %subst(vindta:138:12) <> *zeros
041300090609     C                   MOVEL     '1'           FlgCAS
041400090716     C                   EVAL      VABVCA='EUR'
041500091029     C***                EVAL      VABTIC='OM'
041600091029     C                   EVAL      VABTIC=%trim(%subst(vindta:289:3))
041700090715     C                   EVAL      PiStr=%trim(%subst(vindta:138:12))
041800090428     C                   EXSR      CHKNUM
041900090428     C                   IF        PiNum=*on
042000090715     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
042100090527     C                   Z-ADD     PiVal         VABCAS
042200090428     C                   ELSE
042300090428     C                   ADD       1             errore
042400090428     C                   EVAL      vinmsg = %trimr(vinmsg)
042500090527     C                             + ' ' + 'VABCAS'
042600090428     C                   ENDIF
042700090428     C                   ENDIF
042800010205     C*
042900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043000040802     C                   IF        FlgCAS <> '0'
043100090609     C                   IF        VABCBO = '1'
043200010205     C                   EVAL      VABCBO = '4'
043300090609     C                   ENDIF
043400090609     C                   IF        VABCBO = '2'
043500090609     C                   EVAL      VABCBO = '6'
043600090609     C                   ENDIF
043700010205     C                   ENDIF
043800020305     C*
043900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044000011113     C                   EXSR      CHKIMPDIV
044100010202     C*
044200000801     C* Ebbene...
044300000801     C                   ADD       1             §CTRMO
044400010201     C                   IF        errore <> *zeros
044500000801     C                   ADD       1             §CTRNO
044600000801     C                   EVAL      recko = vindta
044700000801     C                   EXCEPT    rigadett
044800000801     C                   EVAL      vinflg = '2'
044900000801     C                   ELSE
045000010201     C                   ADD       1             §CTROKVB
045100000801     C                   ENDIF
045200000801     C*
045300000801     C                   ENDSR
045400070102     C*----------------------------------------------------*
045500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045600070102     C*----------------------------------------------------*
045700070103     C     EXEVATE       BEGSR
045800090527     C*
045900090715     C***                EVAL      VATTRC = 'E'
046000090715     C***                EVAL      VATNOT = %trim(%subst(vindta:495:22))
046100090428     C*
046200090609     C***                exsr      wrivat                                       => scarico VAT
046300070102     C*
046400070102     C                   ENDSR
046500080415     C*----------------------------------------------------*
046600090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
046700080415     C*----------------------------------------------------*
046800080415     C     EXEVATA       BEGSR
046900080415     C*
047000090714     C***                EVAL      VATTRC = 'A'
047100090714     C***                EVAL      VATNOT = %trim(%subst(vindta:495:22))
047200080415     C*
047300090609     C                   exsr      wrivat                                       => scarico VAT
047400080415     C*
047500080415     C                   ENDSR
047600071121     C*----------------------------------------------------*
047700090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
047800071121     C*----------------------------------------------------*
047900071121     C     EXEVATB       BEGSR
048000071121     C*
048100090715     C***                EVAL      VATTRC = 'B'
048200090715     C***                EVAL      VATNOT = %trim(%subst(vindta:386:20))
048300071121     C*
048400090609     C                   exsr      wrivat                                       => scarico VAT
048500071121     C*
048600071121     C                   ENDSR
048700010201     C*----------------------------------------------------*
048800090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048900010201     C*----------------------------------------------------*
049000020305     C     WRIVAT        BEGSR
049100050628     C*
049200060223     C* Scrivo solo se valorizzato qualcosa
049300060223     C                   IF        VATNOT <> *blanks
049400090114     C                   WRITE     FIVAT000
049500060223     C                   ENDIF
049600010201     C*
049700010201     C                   ENDSR
049800010202     C*----------------------------------------------------*
049900090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050000010202     C*----------------------------------------------------*
050100020305     C     PREVAT        BEGSR
050200010202     C*
050300090114     C* Compongo il nome del membro da dare al FIVATWWR
050400010202     C                   eval      parmbr = vlrhdl
050500010202     C                   movel     'M'           parmbr
050600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050700010202     C                   eval      paropz = '1'
050800010202     C* Effettuo la chiamata al CLLE preposto
050900090114     C                   call(e)   'TITVVTC'
051000010202     C                   parm                    parccm
051100010202     C                   parm                    parmbr
051200010202     C                   parm                    paropz
051300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051400010202     C                   if        %error
051500010202     C                   movel     '1'           chkcall
051600010202     C                   else
051700010202     C                   movel     '0'           chkcall
051800010202     C                   endif
051900010202     C*
052000010202     C                   ENDSR
052100000801     C*----------------------------------------------------*
052200000801     C*  CONTROLLO NUMERICITA' CAMPI
052300000801     C*----------------------------------------------------*
052400000801     C     CHKNUM        BEGSR
052500081217     C*
052600081217     C                   IF        PiDecChr = *blanks
052700090519     C                   EVAL      PiDecChr = ','
052800081217     C                   ENDIF
052900081217     C*
053000081217     C                   callp(e)  UBISNUM_Check(PiStr
053100081217     C                                          :PiDecChr
053200081217     C                                          :PiVal
053300081217     C                                          :PiNum
053400081217     C                                          :PiInt)
053500081217     C*
053600081217     C                   IF        %error
053700081217     C                   EVAL      PiInt=*off
053800081217     C                   ENDIF
053900000801     C*
054000000801     C                   ENDSR
054100000801     C***
054200000801
054300011113
054400011113     C*----------------------------------------------------*
054500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054600011113     C*----------------------------------------------------*
054700011113     C     CHKIMPDIV     BEGSR
054800011113     C*
054900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055000011113     C                   Z-ADD     *zeros        wrkDec            9 9
055100011113     C*
055200011113     C* Come prima cosa effettuo considerazioni sulla divisa
055300011113     C                   IF        vabIAS > *zeros
055400011113     C                   IF        vabVAS <> 'EUR'
055500011113     C                   EVAL      vabVAS =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C                   IF        vabCAS > *zeros
056000011113     C                   IF        vabVCA <> 'EUR'
056100011113     C                   EVAL      vabVCA =  'ITL'
056200011113     C                   ENDIF
056300011113     C                   ENDIF
056400011113     C*
056500011113     C                   IF        vabVMD > *zeros
056600020305     C                   IF        vabVAD <> 'EUR'
056700011113     C                   EVAL      vabVAD =  'ITL'
056800011113     C                   ENDIF
056900011113     C                   ENDIF
057000011113     C*
057100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057200011113     C                   Z-ADD     vabIAS        wrkDec
057300011113     C                   IF        wrkDec > *zeros
057400011113     C                   IF        vabVAS = 'ITL'
057500011113     C                   EVAL      vabIAS = *zeros
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058000011113     C                   Z-ADD     vabCAS        wrkDec
058100011113     C                   IF        wrkDec > *zeros
058200011113     C                   IF        vabVCA = 'ITL'
058300011113     C                   EVAL      vabCAS = *zeros
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058800011113     C                   Z-ADD     vabVMD        wrkDec
058900011113     C                   IF        wrkDec > *zeros
059000011113     C                   IF        vabVAD = 'ITL'
059100011113     C                   EVAL      vabVMD = *zeros
059200011113     C                   ENDIF
059300011113     C                   ENDIF
059400011113     C*
059500011113     C                   ENDSR
059600011113     C***
059700011113
059800011113
059900000801
060000000801
060100990920      /TITLE Invio dei dati al punto operativo.
060200010202     C     invio         BEGSR
060300990920     C*
060400090114     C* 1° invio FIVAT
060500010201     C                   reset                   dscmz
060600010201     C                   move      vlrpoi        cmzdst
060700090114     C                   eval      cmzfld = 'FIVATWWR'
060800010201     C                   eval      cmzmbd = vlrhdl
060900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061000021009     C***                if        prmfir = *blanks
061100090114     C                   eval      cmzfla = 'FIVAT00F'
061200090114     C                   eval      cmzmba = 'FIVAT00F'
061300021009     C***                else
061400021009     C***                eval      cmzfla = prmfir
061500021009     C***                eval      cmzmba = prmfir
061600021009     C***                endif
061700010201     C                   eval      cmznrr = *zeros
061800020305     C                   move      §ctrokvt      cmznrr
061900021018     C                   eval      cmzlba = vlrfl1
062000010201     C                   call(e)   'TIS711C'
062100010201     C                   parm                    dscmz
062200010201     C                   parm      *blanks       esito
062300010205     C                   if        %error
062400010205     C                             or cmzerr = '1'
062500010205     C                             or esito  = '1'
062600010205     C                   eval      wrkesito = '3'
062700010205     C                   else
062800010201     C*
062900090114     C* 2° invio FIVAB
063000010201     C                   reset                   dscmz
063100010201     C                   move      vlrpoi        cmzdst
063200010201     C                   eval      cmzfld = vlrfou
063300010201     C                   eval      cmzmbd = vlrhdl
063400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063500021009     C***                if        prmfir = *blanks
063600090114     C                   eval      cmzfla = 'FIVAB00F'
063700090114     C                   eval      cmzmba = 'FIVAB00F'
063800021009     C***                else
063900021009     C***                eval      cmzfla = prmfir
064000021009     C***                eval      cmzmba = prmfir
064100021009     C***                endif
064200010201     C                   eval      cmznrr = *zeros
064300010201     C                   move      §ctrokvb      cmznrr
064400021018     C                   eval      cmzlba = vlrfl1
064500010201     C                   call(e)   'TIS711C'
064600010201     C                   parm                    dscmz
064700010201     C                   parm      *blanks       esito
064800010201     C                   if        %error
064900010201     C                             or cmzerr = '1'
065000010201     C                             or esito  = '1'
065100010201     C                   eval      wrkesito = '3'
065200010201     C                   endif
065300010205     C                   endif
065400990920     C*
065500000613     C                   ENDSR
065600000613     C***
065700070411
065800090331
065900090331
066000090331
066100070411     C     *pssr         BEGSR
066200070411     C*
066300070411     C                   if        %open(tivin00r)
066400070411     C                   close     tivin00r
066500070411     C                   endif
066600090114     C                   if        %open(fivabwwr)
066700090114     C                   close     fivabwwr
066800070411     C                   endif
066900090114     C                   if        %open(fivatwwr)
067000090114     C                   close     fivatwwr
067100070411     C                   endif
067200070411     C*
067300070411     C* Effettuo la chiamata al CLLE preposto
067400090114     C                   call(e)   'TITVVTC'
067500070411     C                   parm                    parccm
067600070411     C                   parm                    parmbr
067700070411     C                   parm      '2'           paropz
067800070411     C*
067900070411     C                   eval      wrkesito = '2'
068000070411     C*
068100070411     C                   seton                                        LR
068200070411     C*
068300070411     C                   ENDSR     '*CANCL'
068400070411     C***
068500070411
068600090331
068700090331
068800990910
068900000613     C     *inzsr        BEGSR
069000990910     C*
069100990910     C     *entry        plist
069200990920     C                   parm                    tivlrds
069300990921     C                   parm      wrkesito      esito
069400000724     C                   parm                    prmlit
069500000710     C                   parm                    prmfir
069600000613     C*
069700000830     C* CALCOLA LA DATA CORRENTE
069800000830     C                   time                    wn14             14 0
069900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
070000080512     C                   movel     wn14          oracor            6 0          *ORA
070100000830     C                   z-add     wn8           g08dat
070200000830     C                   z-add     *zeros        g08inv
070300000830     C                   movel     '0'           g08err
070400000830     C                   call      'XSRDA8'
070500000830     C                   parm                    wlbda8
070600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
070700000830     C*
070800000613     C                   ENDSR
070900000613     C***
071000990908
071100090331
071200090331
071300090715     Otitv1r2p  E            riepilogo         2
071400990915     O                                              'Upload via Internet'
071500990915     O                                           +1 'Traduzione TIVIN00R -'
071600090114     O                                           +1 'FIVABWWR/FIVATWWR'
071700010201     O                                           +1 'Report testate bolle'
071800990915     O          E            riepilogo   2
071900990915     O                       wrkdata
072000990915     O                       wrkora              +1
072100990915     O                       procname            +1
072200990915     O          E            riepilogo   2
072300990915     O                                              'Cliente..................:'
072400990915     O                       VABCCM        z     +1
072500990915     O          E            riepilogo   2
072600990920     O                                              'Riferimento Strategi.....:'
072700990920     O                       vlrhdl              +1
072800990915     O          E            riepilogo   2
072900990915     O                                              'Giusti...................:'
073000010201     O                       §CTROKVB      2   +  1
073100990915     O          E            riepilogo   2
073200990915     O                                              'Sbagliati e corretti.....:'
073300971022     O                       §CTRMO        2   +  1
073400990915     O          E            riepilogo   2
073500990915     O                                              'Sbagliati e scartati.....:'
073600971022     O                       §CTRNO        2   +  1
073700000613
073800090715     Otitv1r2ps E            testdett          2
073900000613     O                                              'Upload via Internet'
074000000613     O                                           +1 'Traduzione TIVIN00R -'
074100090114     O                                           +1 'FIVABWWR/FIVATWWR'
074200010201     O                                           +1 'Report testate bolle'
074300000616     O          E            testdett    3
074400000613     O                                           +2 'N° rec'
074500000613     O                                           +3 'Anteprima contenuto'
074600000616     O          E            rigadett    2
074700000613     O                       rrnum               +2
074800000621     O                       recko               +3
074900000616     O          E            findett     2
075000000613     O                       wrkdata
075100000613     O                       wrkora              +1
075200000613     O                       procname            +1
075300000616     O          E            findett     2
075400000613     O                                              'Cliente..................:'
075500000613     O                       VABCCM        z     +1
075600000616     O          E            findett     2
075700000613     O                                              'Riferimento Strategi.....:'
075800000613     O                       vlrhdl              +1
075900000616     O          E            findett     2
076000000613     O                                              'Giusti...................:'
076100010201     O                       §CTROKVB      2   +  1
076200000616     O          E            findett     2
076300000613     O                                              'Sbagliati e corretti.....:'
076400000613     O                       §CTRMO        2   +  1
076500000616     O          E            findett     2
076600000613     O                                              'Sbagliati e scartati.....:'
076700000613     O                       §CTRNO        2   +  1
076800000512** CMD - COMANDI CL
076900090715OVRPRTF FILE(TITV1R2P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
077000090715OVRPRTF FILE(TITV1R2PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
077100090715DLTOVR FILE(TITV1R2P TITV1R2PS) LVL(*)
077200000512
077300000512
