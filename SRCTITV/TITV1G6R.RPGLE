000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800080604     Ftitv1g6p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100080604     Ftitv1g6ps O    f  198        PRINTER usropn
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
004000080604     D curSped         s              8    INZ(*blanks)
004100080604     D depSped         s              8    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100010201
005200010201
005300990915     C                   time                    wrkdata
005400990915     C                   time                    wrkora
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800000724     C*
005900000724     C* SE OCCORRE SPEDIRE IN FILIALE
006000000724     C                   if        vlrpoi <> *zeros
006100000724     C*
006200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006400000724     C     vlrpoi        chain     azorg01l
006500000724     C                   if        %found
006600000616     C                   movel(p)  CMD(1)        depcmd
006700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006800000616     C*
006900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007000011113     C                   Z-ADD     150           LENGH            15 5
007100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007200000616     C                   PARM                    depcmd
007300000616     C                   PARM                    LENGH
007400000724     C*
007500000724     C                   endif
007600000724     C                   endif
007700000616     C*
007800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900000616     C                   movel(p)  CMD(2)        depcmd
008000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008100000616     C*
008200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000616     C                   PARM                    depcmd
008600000616     C                   PARM                    LENGH
008700000616     C*
008800080604     C                   if        not %open(titv1g6ps)
008900080604     C                   open      titv1g6ps
009000000616     C                   except    testdett
009100000613     C                   endif
009200000613     C*
009300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009500000613     C*
009600010202     C* Effettuo la chiamata al CLLE preposto
009700040506     C                   call(e)   'TITVVTC'
009800010202     C                   parm                    parccm
009900010202     C                   parm                    parmbr
010000010202     C                   parm      '2'           paropz
010100050201     C*
010200050201     C* Effettuo lancio TISI95 solo x chiusura
010300050201     C                   CLEAR                   TISI95DS
010400050201     C                   EVAL      I95TLA = 'C'
010500050201     C                   CALL      'TISI95R'
010600050201     C                   PARM                    TISI95DS
010700010202     C*
010800080604     C                   if        %open(titv1g6ps)
010900000616     C                   except    findett
011000080604     C                   close     titv1g6ps
011100000613     C                   endif
011200000616     C*
011300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000616     C                   PARM                    CMD(3)
011700000616     C                   PARM                    LENGH
011800000616     C*
011900000801     C
012000010201     C                   seton                                        LR
012100000613
012200000613
012300000613     C*--------------------------------------------------------
012400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012500000613     C*--------------------------------------------------------
012600000621     C     STPR          BEGSR
012700000613     C*
012800080604     C                   if        not %open(titv1g6p)
012900080604     C                   open      titv1g6p
013000990915     C                   endif
013100990915     C*
013200990915     C                   except    riepilogo
013300990915     C*
013400080604     C                   if        %open(titv1g6p)
013500080604     C                   close     titv1g6p
013600990914     C                   endif
013700990910     C*
013800000613     C                   ENDSR
013900000613     C***
014000990908
014100000801
014200910830     C*--------------------------------------------------------
014300040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014400910830     C*--------------------------------------------------------
014500040526     C     RWFILE        BEGSR
014600990910     C*
014700990914     C                   if        not %open(tivin00r)
014800990908     C                   open      tivin00r
014900990914     C                   endif
015000021113     C                   if        not %open(fivabwwr)
015100021113     C                   open      fivabwwr
015200990914     C                   endif
015300070103     C*
015400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015500020305     C                   exsr      prevat
015600010201     C*
015700010202     C                   if        chkcall = '0'
015800010202     C*
015900021113     C                   if        not %open(fivatwwr)
016000021113     C                   open      fivatwwr
016100010201     C                   endif
016200990910     C*
016300010201     C                   clear                   §CTROKVB          5 0
016400020305     C                   clear                   §CTROKVT          5 0
016500000801     C                   clear                   §CTRMO            5 0
016600000801     C                   clear                   §CTRNO            5 0
016700990910     C*
016800921023     C                   DO        *HIVAL
016900990913     C*
017000990915     C                   READ      tivin00r                               70
017100050627     C                   if        vindta > *blanks
017200000613     C                   add       1             rrnum
017300000801     C*
017400000801     C                   if        *in70 = *off
017500000801     C                             and
017600000801     C                             (vinflg = *blanks
017700000801     C                              or vinflg = '0'
017800000801     C                              or vinflg = '2')
017900000801     C*
018000000801     C                   clear                   vinmsg
018100000801     C                   eval      vinflg = '1'
018200080604     C*
018300070213     C                   exsr      inzvar
018400070213     C                   exsr      defcam
018500070213     C                   exsr      impvab                                       => carico  VAB
018600080604     C                   exsr      wrivab                                       => scarico VAB
018700000905     C*
018800000905     C                   else
018900000905     C                   eval      vinflg = '1'
019000050628     C                   endif
019100000905     C                   endif
019200000905     C*
019300000905     C  N70              update    tivin000
019400000905     C*
019500991022     C  N70              ENDdo
019600010202     C*
019700010202     C                   endif
019800990910
019900990910     C* Se non ci sono record con errori ...
020000000710     C                   if        §ctrno = 0
020100990910     C* ... restituisco esito OK.
020200990921     C                   eval      wrkesito = '0'
020300990910     C                   else
020400010201     C                   if        §ctrokvb > 0
020500990921     C                   eval      wrkesito = '1'
020600000710     C                   else
020700000710     C                   eval      wrkesito = '2'
020800990910     C                   endif
020900000710     C                   endif
021000990910     C*
021100990914     C                   if        %open(tivin00r)
021200990908     C                   close     tivin00r
021300990914     C                   endif
021400021113     C                   if        %open(fivabwwr)
021500021113     C                   close     fivabwwr
021600990914     C                   endif
021700021113     C                   if        %open(fivatwwr)
021800021113     C                   close     fivatwwr
021900010201     C                   endif
022000990910     C*
022100010201     C                   if        §ctrokvb > 0
022200000724     C                             and vlrpoi <> *zeros
022300010202     C                   exsr      invio
022400990920     C                   endif
022500990920     C*
022600910830     C                   ENDSR
022700000613     C***
022800010305
022900010305     C*----------------------------------------------------*
023000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
023100010305     C*----------------------------------------------------*
023200020305     C     WRIVAB        BEGSR
023300010305     C*
023400060225     C* Quindi scarico il buffer del file d testata
023500021113     C                   write     fivab000                                     => scarico il VAB
023600010305     C*
023700010305     C                   ENDSR
023800990920
023900000801     C*----------------------------------------------------*
024000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024100000801     C*----------------------------------------------------*
024200010201     C     INZVAR        BEGSR
024300000801     C*
024400070213     C                   CLEAR                   FIVAB000
024500070213     C                   CLEAR                   FIVAT000
024600070213     C*
024700040802     C                   Z-ADD     *zeros        Num5_0            5 0
024800040802     C                   MOVEL     '0'           FlgCAS            1
024900000801     C*
025000000801     C                   ENDSR
025100000801     C*----------------------------------------------------*
025200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
025300000801     C*----------------------------------------------------*
025400000801     C     DEFCAM        BEGSR
025500000801     C*
025600020619     C* Imposto i valori di default...
025700080604     C                   Z-ADD     0054070       VABCCM
025800080604     C                   Z-ADD     249           VABLNP
025900080604     C                   Z-ADD     000           VABCTR
026000070103     C                   MOVEL     '7Q'          VABCTM
026100040823     C                   MOVEL     '1'           VABCBO
026200020619     C* ... e poi verifico se sono stati passati come parametri
026300020619     C                   IF        vlrppt > *blanks
026400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
026500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026600020619     C                   EXSR      CHKNUM
026700020619     C                   IF        PiInt=*on
026800020619     C                   Z-ADD     PiVal         VABCCM
026900020619     C                   ENDIF
027000040506     C                   ENDIF
027100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
027200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027300020619     C                   EXSR      CHKNUM
027400020619     C                   IF        PiInt=*on
027500020619     C                   Z-ADD     PiVal         VABLNP
027600040506     C                   ENDIF
027700020619     C                   ENDIF
027800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028000020619     C                   EXSR      CHKNUM
028100020619     C                   IF        PiInt=*on
028200020619     C                   Z-ADD     PiVal         VABCTR
028300040506     C                   ENDIF
028400020619     C                   ENDIF
028500020619     C                   ENDIF
028600000801     C*
028700000801     C                   ENDSR
028800000801     C*----------------------------------------------------*
028900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029000000801     C*----------------------------------------------------*
029100040823     C     IMPVAB        BEGSR
029200080604     C*
029300000801     C                   Z-ADD     *zeros        errore            1 0
029400000830     C                   MOVEL     datcor        VABAAS
029500040526     C                   MOVE      datcor        VABMGS
029600040823     C                   MOVE(P)   vlrpoi        VABFGS
029700080604     C*
029800080604     C* Verifica campi derivati
029900080409     C*
030000080409     C* Reperimento campi ALFA
030100080409     C*
030200080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
030300080604     C                   EVAL      VABRSD=%trim(%subst(vindta:134:30))
030400080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030500080409     C     '@':'A'       XLATE     VABRSD        VABRSD
030600080409     C* ==
030700080604     C                   EVAL      VABIND=%trim(%subst(vindta:164:30))
030800080604     C                   EVAL      VABLOD=%trim(%subst(vindta:194:30))
030900080604     C                   EVAL      VABPRD=%trim(%subst(vindta:224:2))
031000080604     C                   EVAL      VABRMA=%trim(%subst(vindta:15:15))
031100080409     C*
031200080409     C* Reperimento campi NUMERICI
031300080409     C                   MOVEL     DATCOR        VABAAS
031400080409     C                   MOVE      DATCOR        VABMGS
031500080409     C* NSP/RMN
031600080604     C                   EVAL      PiStr=%trim(%subst(vindta:7:8))
031700080409     C                   EXSR      CHKNUM
031800080409     C                   IF        PiInt=*on
031900080409     C                   Z-ADD     PiVal         VABNSP
032000080409     C                   Z-ADD     PiVal         VABRMN
032100080409     C                   ELSE
032200080409     C                   ADD       1             errore
032300080409     C                   Z-ADD     *zeros        VABNSP
032400080409     C                   Z-ADD     1             VABRMN
032500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
032600080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
032700080409     C                   ENDIF
032800080409     C* CAD
032900080604     C                   EVAL      PiStr=%trim(%subst(vindta:226:9))
033000080409     C                   EXSR      CHKNUM
033100080409     C                   IF        PiInt=*on
033200080409     C                   Z-ADD     PiVal         Num5_0
033300080409     C                   MOVEL(p)  Num5_0        VABCAD
033400080409     C                   ELSE
033500080409     C                   ADD       1             errore
033600080409     C                   EVAL      VABCAD = *zeros
033700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
033800080409     C                             + ' ' + 'VABCAD'
033900080409     C                   ENDIF
034000080604     C* CAD
034100080604     C                   IF        %trim(%subst(vindta:235:3)) = 'IT'
034200080604     C                   EVAL      VABNZD = *blanks
034300080604     C                   ELSE
034400080604     C                   EVAL      VABNZD = %trim(%subst(vindta:235:3))
034500080604     C                   ENDIF
034600080604     C* NCL
034700080604     C                   EVAL      PiStr=%trim(%subst(vindta:293:9))
034800080409     C                   EXSR      CHKNUM
034900080409     C                   IF        PiInt=*on
035000080604     C                   Z-ADD     PiVal         VABNCL
035100080409     C                   ELSE
035200080409     C                   ADD       1             errore
035300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
035400080409     C                             + ' ' + 'VABNCL'
035500080409     C                   ENDIF
035600080604     C* PKB
035700080604     C                   EVAL      PiStr=%trim(%subst(vindta:304:13))
035800080409     C                   EXSR      CHKNUM
035900080409     C                   IF        PiNum=*on
036000080604     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
036100080604     C                   Z-ADD(H)  PiVal         VABPKB
036200080409     C                   ELSE
036300080409     C                   ADD       1             errore
036400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
036500080409     C                             + ' ' + 'VABPKB'
036600080409     C                   ENDIF
036700080604     C* CAS
036800080604     C                   IF        %trim(%subst(vindta:317:10)) <> *all'0' AND
036900080604     C                             %trim(%subst(vindta:317:10)) <> *blanks
037000080604     C                   EVAL      FlgCAS = '1'
037100080604     C                   EVAL      VABVCA = 'EUR'
037200080604     C                   EVAL      PiStr=%trim(%subst(vindta:317:10))
037300080604     C                   EXSR      CHKNUM
037400080604     C                   IF        PiNum=*on
037500080604     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
037600080604     C                   Z-ADD     PiVal         VABCAS
037700080604     C                   ELSE
037800080604     C                   ADD       1             errore
037900080604     C                   Z-ADD     *zeros        VABCAS
038000080604     C                   EVAL      vinmsg = %trimr(vinmsg)
038100080604     C                             + ' ' + 'VABCAS'
038200080604     C                   ENDIF
038300080604     C                   ENDIF
038400071121     C*
038500080415     C* Carico l'estensioni A e B del FNVAT
038600080415     C                   exsr      exevata
038700071121     C                   exsr      exevatb
038800010205     C*
038900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039000040802     C                   IF        FlgCAS <> '0'
039100080604     C                   IF        VABCBO = '1'
039200080604     C                   EVAL      VABCBO = '4'
039300080604     C                   ENDIF
039400080604     C                   IF        VABCBO = '2'
039500080604     C                   EVAL      VABCBO = '6'
039600080604     C                   ENDIF
039700010205     C                   ENDIF
039800020305     C*
039900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040000011113     C                   EXSR      CHKIMPDIV
040100010202     C*
040200000801     C* Ebbene...
040300000801     C                   ADD       1             §CTRMO
040400010201     C                   IF        errore <> *zeros
040500000801     C                   ADD       1             §CTRNO
040600000801     C                   EVAL      recko = vindta
040700000801     C                   EXCEPT    rigadett
040800000801     C                   EVAL      vinflg = '2'
040900000801     C                   ELSE
041000010201     C                   ADD       1             §CTROKVB
041100000801     C                   ENDIF
041200000801     C*
041300000801     C                   ENDSR
041400070102     C*----------------------------------------------------*
041500070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
041600070102     C*----------------------------------------------------*
041700070103     C     EXEVATE       BEGSR
041800070102     C*
041900080604     C***                exsr      wrivat                                       => scarico VAT
042000070102     C*
042100070102     C                   ENDSR
042200080415     C*----------------------------------------------------*
042300080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
042400080415     C*----------------------------------------------------*
042500080415     C     EXEVATA       BEGSR
042600080604     C*
042700080604     C***                exsr      wrivat                                       => scarico VAT
042800080415     C*
042900080415     C                   ENDSR
043000071121     C*----------------------------------------------------*
043100071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
043200071121     C*----------------------------------------------------*
043300071121     C     EXEVATB       BEGSR
043400080604     C*
043500080604     C***                exsr      wrivat                                       => scarico VAT
043600071121     C*
043700071121     C                   ENDSR
043800010201     C*----------------------------------------------------*
043900040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044000010201     C*----------------------------------------------------*
044100020305     C     WRIVAT        BEGSR
044200080604     C*
044300080604     C                   eval      vatAAS = vabAAS
044400080604     C                   eval      vatLNP = vabLNP
044500080604     C                   eval      vatNRS = vabNRS
044600080604     C                   eval      vatNSP = vabNSP
044700080604     C                   eval      vatCCM = vabCCM
044800080604     C                   eval      vatFGS = vabFGS
044900050628     C*
045000060223     C* Scrivo solo se valorizzato qualcosa
045100060223     C                   IF        VATNOT <> *blanks
045200040802     C                   WRITE     FIVAT000
045300060223     C                   ENDIF
045400010201     C*
045500010201     C                   ENDSR
045600010202     C*----------------------------------------------------*
045700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045800010202     C*----------------------------------------------------*
045900020305     C     PREVAT        BEGSR
046000010202     C*
046100021113     C* Compongo il nome del membro da dare al FIVATWWR
046200010202     C                   eval      parmbr = vlrhdl
046300010202     C                   movel     'M'           parmbr
046400050627     C                   eval      parccm = %subst(vlrKSC:2:7)
046500010202     C                   eval      paropz = '1'
046600010202     C* Effettuo la chiamata al CLLE preposto
046700040506     C                   call(e)   'TITVVTC'
046800010202     C                   parm                    parccm
046900010202     C                   parm                    parmbr
047000010202     C                   parm                    paropz
047100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047200010202     C                   if        %error
047300010202     C                   movel     '1'           chkcall
047400010202     C                   else
047500010202     C                   movel     '0'           chkcall
047600010202     C                   endif
047700010202     C*
047800010202     C                   ENDSR
047900000801     C*----------------------------------------------------*
048000000801     C*  CONTROLLO NUMERICITA' CAMPI
048100000801     C*----------------------------------------------------*
048200000801     C     CHKNUM        BEGSR
048300000801     C*
048400000801     C                   call(e)   'ISNUMERIC'
048500000801     C                   PARM                    PiStr            30
048600070213     C                   PARM      ','           PiDecChr          1
048700000801     C                   PARM      *ZEROS        PiVal            30 9
048800000801     C                   PARM      '0'           PiInt             1
048900000801     C                   PARM      '0'           PiNum             1
049000000801     C                   IF        %error
049100000801     C                   EVAL      PiInt=*off
049200000801     C                   ENDIF
049300000801     C*
049400000801     C                   ENDSR
049500000801     C***
049600000801
049700011113
049800011113     C*----------------------------------------------------*
049900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050000011113     C*----------------------------------------------------*
050100011113     C     CHKIMPDIV     BEGSR
050200011113     C*
050300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050400011113     C                   Z-ADD     *zeros        wrkDec            9 9
050500011113     C*
050600011113     C* Come prima cosa effettuo considerazioni sulla divisa
050700011113     C                   IF        vabIAS > *zeros
050800011113     C                   IF        vabVAS <> 'EUR'
050900011113     C                   EVAL      vabVAS =  'ITL'
051000011113     C                   ENDIF
051100011113     C                   ENDIF
051200011113     C*
051300011113     C                   IF        vabCAS > *zeros
051400011113     C                   IF        vabVCA <> 'EUR'
051500011113     C                   EVAL      vabVCA =  'ITL'
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C                   IF        vabVMD > *zeros
052000020305     C                   IF        vabVAD <> 'EUR'
052100011113     C                   EVAL      vabVAD =  'ITL'
052200011113     C                   ENDIF
052300011113     C                   ENDIF
052400011113     C*
052500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052600011113     C                   Z-ADD     vabIAS        wrkDec
052700011113     C                   IF        wrkDec > *zeros
052800011113     C                   IF        vabVAS = 'ITL'
052900011113     C                   EVAL      vabIAS = *zeros
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
053400011113     C                   Z-ADD     vabCAS        wrkDec
053500011113     C                   IF        wrkDec > *zeros
053600011113     C                   IF        vabVCA = 'ITL'
053700011113     C                   EVAL      vabCAS = *zeros
053800011113     C                   ENDIF
053900011113     C                   ENDIF
054000011113     C*
054100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054200011113     C                   Z-ADD     vabVMD        wrkDec
054300011113     C                   IF        wrkDec > *zeros
054400011113     C                   IF        vabVAD = 'ITL'
054500011113     C                   EVAL      vabVMD = *zeros
054600011113     C                   ENDIF
054700011113     C                   ENDIF
054800011113     C*
054900011113     C                   ENDSR
055000011113     C***
055100011113
055200011113
055300000801
055400000801
055500990920      /TITLE Invio dei dati al punto operativo.
055600010202     C     invio         BEGSR
055700990920     C*
055800021113     C* 1° invio FIVAT
055900010201     C                   reset                   dscmz
056000010201     C                   move      vlrpoi        cmzdst
056100021113     C                   eval      cmzfld = 'FIVATWWR'
056200010201     C                   eval      cmzmbd = vlrhdl
056300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056400021009     C***                if        prmfir = *blanks
056500021113     C                   eval      cmzfla = 'FIVAT00F'
056600021113     C                   eval      cmzmba = 'FIVAT00F'
056700021009     C***                else
056800021009     C***                eval      cmzfla = prmfir
056900021009     C***                eval      cmzmba = prmfir
057000021009     C***                endif
057100010201     C                   eval      cmznrr = *zeros
057200020305     C                   move      §ctrokvt      cmznrr
057300021018     C                   eval      cmzlba = vlrfl1
057400010201     C                   call(e)   'TIS711C'
057500010201     C                   parm                    dscmz
057600010201     C                   parm      *blanks       esito
057700010205     C                   if        %error
057800010205     C                             or cmzerr = '1'
057900010205     C                             or esito  = '1'
058000010205     C                   eval      wrkesito = '3'
058100010205     C                   else
058200010201     C*
058300021113     C* 2° invio FIVAB
058400010201     C                   reset                   dscmz
058500010201     C                   move      vlrpoi        cmzdst
058600010201     C                   eval      cmzfld = vlrfou
058700010201     C                   eval      cmzmbd = vlrhdl
058800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058900021009     C***                if        prmfir = *blanks
059000021113     C                   eval      cmzfla = 'FIVAB00F'
059100021113     C                   eval      cmzmba = 'FIVAB00F'
059200021009     C***                else
059300021009     C***                eval      cmzfla = prmfir
059400021009     C***                eval      cmzmba = prmfir
059500021009     C***                endif
059600010201     C                   eval      cmznrr = *zeros
059700010201     C                   move      §ctrokvb      cmznrr
059800021018     C                   eval      cmzlba = vlrfl1
059900010201     C                   call(e)   'TIS711C'
060000010201     C                   parm                    dscmz
060100010201     C                   parm      *blanks       esito
060200010201     C                   if        %error
060300010201     C                             or cmzerr = '1'
060400010201     C                             or esito  = '1'
060500010201     C                   eval      wrkesito = '3'
060600010201     C                   endif
060700010205     C                   endif
060800990920     C*
060900000613     C                   ENDSR
061000000613     C***
061100070411
061200070411     C     *pssr         BEGSR
061300070411     C*
061400070411     C                   if        %open(tivin00r)
061500070411     C                   close     tivin00r
061600070411     C                   endif
061700070411     C                   if        %open(fivabwwr)
061800070411     C                   close     fivabwwr
061900070411     C                   endif
062000070411     C                   if        %open(fivatwwr)
062100070411     C                   close     fivatwwr
062200070411     C                   endif
062300070411     C*
062400070411     C* Effettuo la chiamata al CLLE preposto
062500070411     C                   call(e)   'TITVVTC'
062600070411     C                   parm                    parccm
062700070411     C                   parm                    parmbr
062800070411     C                   parm      '2'           paropz
062900070411     C*
063000070411     C                   eval      wrkesito = '2'
063100070411     C*
063200070411     C                   seton                                        LR
063300070411     C*
063400070411     C                   ENDSR     '*CANCL'
063500070411     C***
063600070411
063700990910
063800000613     C     *inzsr        BEGSR
063900990910     C*
064000990910     C     *entry        plist
064100990920     C                   parm                    tivlrds
064200990921     C                   parm      wrkesito      esito
064300000724     C                   parm                    prmlit
064400000710     C                   parm                    prmfir
064500000613     C*
064600000830     C* CALCOLA LA DATA CORRENTE
064700000830     C                   time                    wn14             14 0
064800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064900000830     C                   z-add     wn8           g08dat
065000000830     C                   z-add     *zeros        g08inv
065100000830     C                   movel     '0'           g08err
065200000830     C                   call      'XSRDA8'
065300000830     C                   parm                    wlbda8
065400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065500000830     C*
065600000613     C                   ENDSR
065700000613     C***
065800990908
065900080604     Otitv1g6p  E            riepilogo         2
066000990915     O                                              'Upload via Internet'
066100990915     O                                           +1 'Traduzione TIVIN00R -'
066200021113     O                                           +1 'FIVABWWR/FIVATWWR'
066300010201     O                                           +1 'Report testate bolle'
066400990915     O          E            riepilogo   2
066500990915     O                       wrkdata
066600990915     O                       wrkora              +1
066700990915     O                       procname            +1
066800990915     O          E            riepilogo   2
066900990915     O                                              'Cliente..................:'
067000990915     O                       VABCCM        z     +1
067100990915     O          E            riepilogo   2
067200990920     O                                              'Riferimento Strategi.....:'
067300990920     O                       vlrhdl              +1
067400990915     O          E            riepilogo   2
067500990915     O                                              'Giusti...................:'
067600010201     O                       §CTROKVB      2   +  1
067700990915     O          E            riepilogo   2
067800990915     O                                              'Sbagliati e corretti.....:'
067900971022     O                       §CTRMO        2   +  1
068000990915     O          E            riepilogo   2
068100990915     O                                              'Sbagliati e scartati.....:'
068200971022     O                       §CTRNO        2   +  1
068300000613
068400080604     Otitv1g6ps E            testdett          2
068500000613     O                                              'Upload via Internet'
068600000613     O                                           +1 'Traduzione TIVIN00R -'
068700021113     O                                           +1 'FIVABWWR/FIVATWWR'
068800010201     O                                           +1 'Report testate bolle'
068900000616     O          E            testdett    3
069000000613     O                                           +2 'N° rec'
069100000613     O                                           +3 'Anteprima contenuto'
069200000616     O          E            rigadett    2
069300000613     O                       rrnum               +2
069400000621     O                       recko               +3
069500000616     O          E            findett     2
069600000613     O                       wrkdata
069700000613     O                       wrkora              +1
069800000613     O                       procname            +1
069900000616     O          E            findett     2
070000000613     O                                              'Cliente..................:'
070100000613     O                       VABCCM        z     +1
070200000616     O          E            findett     2
070300000613     O                                              'Riferimento Strategi.....:'
070400000613     O                       vlrhdl              +1
070500000616     O          E            findett     2
070600000613     O                                              'Giusti...................:'
070700010201     O                       §CTROKVB      2   +  1
070800000616     O          E            findett     2
070900000613     O                                              'Sbagliati e corretti.....:'
071000000613     O                       §CTRMO        2   +  1
071100000616     O          E            findett     2
071200000613     O                                              'Sbagliati e scartati.....:'
071300000613     O                       §CTRNO        2   +  1
071400000512** CMD - COMANDI CL
071500080604OVRPRTF FILE(TITV1G6P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071600080604OVRPRTF FILE(TITV1G6PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071700080604DLTOVR FILE(TITV1G6P TITV1G6PS) LVL(*)
071800000512
071900000512
