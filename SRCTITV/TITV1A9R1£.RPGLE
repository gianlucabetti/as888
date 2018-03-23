000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800070927     Ftitv1a9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070927     Ftitv1a9ps O    f  198        PRINTER usropn
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
004000100122 xxx D curSped         s              7    INZ(*blanks)
004100100122 xxx D depSped         s              7    INZ(*blanks)
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
008800070927     C                   if        not %open(titv1a9ps)
008900070927     C                   open      titv1a9ps
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
010800070927     C                   if        %open(titv1a9ps)
010900000616     C                   except    findett
011000070927     C                   close     titv1a9ps
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
012800070927     C                   if        not %open(titv1a9p)
012900070927     C                   open      titv1a9p
013000990915     C                   endif
013100990915     C*
013200990915     C                   except    riepilogo
013300990915     C*
013400070927     C                   if        %open(titv1a9p)
013500070927     C                   close     titv1a9p
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
018200070103     C*
018300070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
018400100122 xxx C                   eval      curSped = %subst(vindta:6:7)
018500070213     C                   if        curSped <> depSped
018600100122 xxx C*
018700100122 xxx C* Forzatura x errore in modifica al file lato cliente
018800100122 xxx C                   Z-ADD     *zeros        wNCLn             3 0
018900100122 xxx C                   MOVEL     *blanks       wNCLa             3
019000100112     C*
019100070213     C* Se prima bolla => importo bolla corrente
019200070213     C                   if        §CTRMO = *zeros
019300070213     C                   exsr      inzvar
019400070213     C                   exsr      defcam
019500070213     C                   exsr      impvab                                       => carico  VAB
019600070213     C                   else
019700100112     C*
019800070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
019900070213     C                   exsr      wrivab                                       => scarico VAB
020000070213     C                   exsr      inzvar
020100070213     C                   exsr      defcam
020200070213     C                   exsr      impvab                                       => carico  VAB
020300070213     C                   endif
020400100122     C                   exsr      exevate                                      => write VAT-E
020500100112     C*
020600070103     C* Salvo il raggruppamento spedizione cliente corrente
020700070103     C                   eval      depSped = curSped
020800070213     C*
020900070213     C* Se collo successivo x stessa bolla
021000070213     C                   else
021100100112     C*
021200070213     C                   exsr      impvab                                       => carico  VAB
021300100122     C                   exsr      exevate                                      => write VAT-E
021400100112     C*
021500070103     C                   endif
021600000905     C*
021700000905     C                   else
021800000905     C                   eval      vinflg = '1'
021900050628     C                   endif
022000000905     C                   endif
022100000905     C*
022200000905     C  N70              update    tivin000
022300000905     C*
022400991022     C  N70              ENDdo
022500070213     C*
022600070213     C* Scarico testata bolla rimasta in sospesa
022700070213     C                   exsr      wrivab                                       => scarico VAB
022800010202     C*
022900010202     C                   endif
023000990910
023100990910     C* Se non ci sono record con errori ...
023200000710     C                   if        §ctrno = 0
023300990910     C* ... restituisco esito OK.
023400990921     C                   eval      wrkesito = '0'
023500990910     C                   else
023600010201     C                   if        §ctrokvb > 0
023700990921     C                   eval      wrkesito = '1'
023800000710     C                   else
023900000710     C                   eval      wrkesito = '2'
024000990910     C                   endif
024100000710     C                   endif
024200990910     C*
024300990914     C                   if        %open(tivin00r)
024400990908     C                   close     tivin00r
024500990914     C                   endif
024600021113     C                   if        %open(fivabwwr)
024700021113     C                   close     fivabwwr
024800990914     C                   endif
024900021113     C                   if        %open(fivatwwr)
025000021113     C                   close     fivatwwr
025100010201     C                   endif
025200990910     C*
025300010201     C                   if        §ctrokvb > 0
025400000724     C                             and vlrpoi <> *zeros
025500010202     C                   exsr      invio
025600990920     C                   endif
025700990920     C*
025800910830     C                   ENDSR
025900000613     C***
026000010305
026100010305     C*----------------------------------------------------*
026200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026300010305     C*----------------------------------------------------*
026400020305     C     WRIVAB        BEGSR
026500010305     C*
026600060225     C* Quindi scarico il buffer del file d testata
026700021113     C                   write     fivab000                                     => scarico il VAB
026800010305     C*
026900010305     C                   ENDSR
027000990920
027100000801     C*----------------------------------------------------*
027200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027300000801     C*----------------------------------------------------*
027400010201     C     INZVAR        BEGSR
027500000801     C*
027600070213     C                   CLEAR                   FIVAB000
027700070213     C                   CLEAR                   FIVAT000
027800070213     C*
027900040802     C                   Z-ADD     *zeros        Num5_0            5 0
028000040802     C                   MOVEL     '0'           FlgCAS            1
028100000801     C*
028200000801     C                   ENDSR
028300000801     C*----------------------------------------------------*
028400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028500000801     C*----------------------------------------------------*
028600000801     C     DEFCAM        BEGSR
028700000801     C*
028800020619     C* Imposto i valori di default...
028900100909     C                   Z-ADD     2230012       VABCCM
029000100909     C                   Z-ADD     2230012       VATCCM
029100100909     C                   Z-ADD     223           VABLNP
029200100909     C                   Z-ADD     223           VATLNP
029300070927     C                   Z-ADD     000           VABCTR
029400040823     C                   MOVEL     '1'           VABCBO
029500070928     C                   MOVEL     'E'           VABTSP
029600100126     C                   MOVEL     '7Q'          VABCTM
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
032600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032700000801     C*----------------------------------------------------*
032800040823     C     IMPVAB        BEGSR
032900100112     C*
033000100122 xxx C                   EVAL      wNCLa=%trim(%subst(vindta:16:3))
033100100122 xxx C                   MOVE(P)   wNCLa         wNCLn
033200100122 xxx C                   IF        VABNCL < wNCLn
033300010305     C*
033400000801     C                   Z-ADD     *zeros        errore            1 0
033500000830     C                   MOVEL     datcor        VABAAS
033600020305     C                   MOVEL     datcor        VATAAS
033700040526     C                   MOVE      datcor        VABMGS
033800040823     C                   MOVE(P)   vlrpoi        VABFGS
033900040823     C                   MOVE(P)   vlrpoi        VATFGS
034000050628     C*
034100100112     C                   EVAL      VABRSD=%trim(%subst(vindta:174:35))
034200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034400020117     C* ==
034500100112     C                   EVAL      VABRD2=%trim(%subst(vindta:209:35))
034600100112     C                   EVAL      VABIND=%trim(%subst(vindta:244:35))
034700100112     C                   EVAL      VABLOD=%trim(%subst(vindta:279:35))
034800100112     C                   EVAL      VABPRD=%trim(%subst(vindta:341:2))
034900100112     C                   EVAL      VABRMA=%trim(%subst(vindta:89:6))
035000100112     C                   EVAL      VABNOT=%trim(%subst(vindta:482:35))
035100100112     C                   EVAL      VABNT2=%trim(%subst(vindta:482+35:60-35))
035200050628     C* CAP
035300100112     C                   EVAL      PiStr=%trim(%subst(vindta:334:7))
035400010201     C                   EXSR      CHKNUM
035500010201     C                   IF        PiInt=*on
035600010201     C                   Z-ADD     PiVal         Num5_0
035700040506     C                   MOVEL(P)  Num5_0        VABCAD
035800010201     C                   ELSE
035900040506     C                   ADD       1             errore
036000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036100040506     C                             + ' ' + 'VABCAD'
036200010201     C                   ENDIF
036300040506     C* Reperisco la provincia dal CAP e dalla località
036400040526     C                   IF        VABCAD <> *blanks AND
036500040526     C                             VABPRD  = *blanks
036600040506     C                   CLEAR                   TISI95DS
036700040506     C                   EVAL      I95TCN = '3'
036800040506     C                   Z-ADD     datcor        I95DAT
036900040506     C                   EVAL      I95CAP = VABCAD
037000040506     C                   EVAL      I95LOC = VABLOD
037100050627     C                   EVAL      I95NAR = VABNZD
037200040506     C                   CALL      'TISI95R'
037300040506     C                   PARM                    TISI95DS
037400040506     C                   EVAL      VABPRD = O95PRV
037500040506     C                   ENDIF
037600100122 xxx C* NCL
037700100122 xxx C                   EVAL      VABNCL = VABNCL + 1
037800040506     C* PKB
037900100112     C                   EVAL      PiStr=%trim(%subst(vindta:378:7))
038000010201     C                   EXSR      CHKNUM
038100010201     C                   IF        PiNum=*on
038200070927     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
038300070927     C                   ADD(H)    PiVal         VABPKB
038400010201     C                   ELSE
038500010201     C                   ADD       1             errore
038600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038700010201     C                             + ' ' + 'VABPKB'
038800010201     C                   ENDIF
038900080111     C* VLB
039000100112     C                   EVAL      PiStr=%trim(%subst(vindta:403:7))
039100080111     C                   EXSR      CHKNUM
039200080111     C                   IF        PiNum=*on
039300080111     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
039400080111     C                   ADD(H)    PiVal         VABVLB
039500080111     C                   ELSE
039600080111     C                   ADD       1             errore
039700080111     C                   EVAL      vinmsg = %trimr(vinmsg)
039800080111     C                             + ' ' + 'VABVLB'
039900080111     C                   ENDIF
040000071019     C* RMN
040100100112     C*                  EVAL      PiStr=%trim(%subst(vindta:6:7))
040200071019     C*                  EXSR      CHKNUM
040300071019     C*                  IF        PiInt=*on
040400071019     C*                  Z-ADD     PiVal         VABRMN
040500071019     C*                  ELSE
040600071019     C*                  ADD       1             errore
040700071019     C*                  EVAL      vinmsg = %trimr(vinmsg)
040800071019     C*                            + ' ' + 'VABRMN'
040900071019     C*                  ENDIF
041000071019     C* NSP/RMN
041100100122 xxx C                   EVAL      PiStr=%trim(%subst(vindta:101:7))
041200071019     C                   EXSR      CHKNUM
041300071019     C                   IF        PiInt=*on
041400071019     C                   Z-ADD     PiVal         VABNSP
041500071019     C                   Z-ADD     PiVal         VATNSP
041600071019     C                   Z-ADD     PiVal         VABRMN
041700071019     C                   ELSE
041800071019     C                   ADD       1             errore
041900071019     C                   EVAL      vinmsg = %trimr(vinmsg)
042000071019     C                             + ' ' + 'VABNSP VATNSP VABRMN'
042100071019     C                   ENDIF
042200010205     C*
042300010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042400040802     C                   IF        FlgCAS <> '0'
042500070102     C                   IF        VABCBO = '1'
042600010205     C                   EVAL      VABCBO = '4'
042700010205     C                   ELSE
042800070102     C                   EVAL      VABCBO = '6'
042900070102     C                   ENDIF
043000010205     C                   ENDIF
043100020305     C*
043200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043300011113     C                   EXSR      CHKIMPDIV
043400100112     C*
043500100122 xxx C                   ENDIF
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
045000070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045100070102     C*----------------------------------------------------*
045200070103     C     EXEVATE       BEGSR
045300070102     C*
045400100126     C                   EVAL      VATTRC='E'
045500100126     C***                EVAL      VATNOT = %trim(%subst(vindta:1:19))
045600100126 xxx C                   EVAL      VATNOT = %trim(%subst(vindta:467:15))
045700071019     C* Occorre togliere gli spazi eventualmente presenti all'interno della stringa barcode
045800071019     C                   Z-ADD     2             i                 2 0
045900071019     C                   DOW       i < %size(VATNOT)
046000071019     C                   IF        %subst(VATNOT:i:1) = *blanks
046100071019     C                   EVAL      VATNOT = %subst(VATNOT:1:i-1) +
046200071019     C                                      %subst(VATNOT:i+1)
046300071019     C                   ENDIF
046400071019     C                   ADD       1             i
046500071019     C                   ENDDO
046600071019     C*
046700070102     C                   exsr      wrivat                                       => scarico VAT
046800070102     C*
046900070102     C                   ENDSR
047000010201     C*----------------------------------------------------*
047100040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047200010201     C*----------------------------------------------------*
047300020305     C     WRIVAT        BEGSR
047400050628     C*
047500060223     C* Scrivo solo se valorizzato qualcosa
047600060223     C                   IF        VATNOT <> *blanks
047700040802     C                   WRITE     FIVAT000
047800060223     C                   ENDIF
047900010201     C*
048000010201     C                   ENDSR
048100010202     C*----------------------------------------------------*
048200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048300010202     C*----------------------------------------------------*
048400020305     C     PREVAT        BEGSR
048500010202     C*
048600021113     C* Compongo il nome del membro da dare al FIVATWWR
048700010202     C                   eval      parmbr = vlrhdl
048800010202     C                   movel     'M'           parmbr
048900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049000010202     C                   eval      paropz = '1'
049100010202     C* Effettuo la chiamata al CLLE preposto
049200040506     C                   call(e)   'TITVVTC'
049300010202     C                   parm                    parccm
049400010202     C                   parm                    parmbr
049500010202     C                   parm                    paropz
049600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049700010202     C                   if        %error
049800010202     C                   movel     '1'           chkcall
049900010202     C                   else
050000010202     C                   movel     '0'           chkcall
050100010202     C                   endif
050200010202     C*
050300010202     C                   ENDSR
050400000801     C*----------------------------------------------------*
050500000801     C*  CONTROLLO NUMERICITA' CAMPI
050600000801     C*----------------------------------------------------*
050700000801     C     CHKNUM        BEGSR
050800000801     C*
050900000801     C                   call(e)   'ISNUMERIC'
051000000801     C                   PARM                    PiStr            30
051100070213     C                   PARM      ','           PiDecChr          1
051200000801     C                   PARM      *ZEROS        PiVal            30 9
051300000801     C                   PARM      '0'           PiInt             1
051400000801     C                   PARM      '0'           PiNum             1
051500000801     C                   IF        %error
051600000801     C                   EVAL      PiInt=*off
051700000801     C                   ENDIF
051800000801     C*
051900000801     C                   ENDSR
052000000801     C***
052100000801
052200011113
052300011113     C*----------------------------------------------------*
052400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052500011113     C*----------------------------------------------------*
052600011113     C     CHKIMPDIV     BEGSR
052700011113     C*
052800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052900011113     C                   Z-ADD     *zeros        wrkDec            9 9
053000011113     C*
053100011113     C* Come prima cosa effettuo considerazioni sulla divisa
053200011113     C                   IF        vabIAS > *zeros
053300011113     C                   IF        vabVAS <> 'EUR'
053400011113     C                   EVAL      vabVAS =  'ITL'
053500011113     C                   ENDIF
053600011113     C                   ENDIF
053700011113     C*
053800011113     C                   IF        vabCAS > *zeros
053900011113     C                   IF        vabVCA <> 'EUR'
054000011113     C                   EVAL      vabVCA =  'ITL'
054100011113     C                   ENDIF
054200011113     C                   ENDIF
054300011113     C*
054400011113     C                   IF        vabVMD > *zeros
054500020305     C                   IF        vabVAD <> 'EUR'
054600011113     C                   EVAL      vabVAD =  'ITL'
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055100011113     C                   Z-ADD     vabIAS        wrkDec
055200011113     C                   IF        wrkDec > *zeros
055300011113     C                   IF        vabVAS = 'ITL'
055400011113     C                   EVAL      vabIAS = *zeros
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055900011113     C                   Z-ADD     vabCAS        wrkDec
056000011113     C                   IF        wrkDec > *zeros
056100011113     C                   IF        vabVCA = 'ITL'
056200011113     C                   EVAL      vabCAS = *zeros
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056700011113     C                   Z-ADD     vabVMD        wrkDec
056800011113     C                   IF        wrkDec > *zeros
056900011113     C                   IF        vabVAD = 'ITL'
057000011113     C                   EVAL      vabVMD = *zeros
057100011113     C                   ENDIF
057200011113     C                   ENDIF
057300011113     C*
057400011113     C                   ENDSR
057500011113     C***
057600011113
057700011113
057800000801
057900000801
058000990920      /TITLE Invio dei dati al punto operativo.
058100010202     C     invio         BEGSR
058200990920     C*
058300021113     C* 1° invio FIVAT
058400010201     C                   reset                   dscmz
058500010201     C                   move      vlrpoi        cmzdst
058600021113     C                   eval      cmzfld = 'FIVATWWR'
058700010201     C                   eval      cmzmbd = vlrhdl
058800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058900021009     C***                if        prmfir = *blanks
059000021113     C                   eval      cmzfla = 'FIVAT00F'
059100021113     C                   eval      cmzmba = 'FIVAT00F'
059200021009     C***                else
059300021009     C***                eval      cmzfla = prmfir
059400021009     C***                eval      cmzmba = prmfir
059500021009     C***                endif
059600010201     C                   eval      cmznrr = *zeros
059700020305     C                   move      §ctrokvt      cmznrr
059800021018     C                   eval      cmzlba = vlrfl1
059900010201     C                   call(e)   'TIS711C'
060000010201     C                   parm                    dscmz
060100010201     C                   parm      *blanks       esito
060200010205     C                   if        %error
060300010205     C                             or cmzerr = '1'
060400010205     C                             or esito  = '1'
060500010205     C                   eval      wrkesito = '3'
060600010205     C                   else
060700010201     C*
060800021113     C* 2° invio FIVAB
060900010201     C                   reset                   dscmz
061000010201     C                   move      vlrpoi        cmzdst
061100010201     C                   eval      cmzfld = vlrfou
061200010201     C                   eval      cmzmbd = vlrhdl
061300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061400021009     C***                if        prmfir = *blanks
061500021113     C                   eval      cmzfla = 'FIVAB00F'
061600021113     C                   eval      cmzmba = 'FIVAB00F'
061700021009     C***                else
061800021009     C***                eval      cmzfla = prmfir
061900021009     C***                eval      cmzmba = prmfir
062000021009     C***                endif
062100010201     C                   eval      cmznrr = *zeros
062200010201     C                   move      §ctrokvb      cmznrr
062300021018     C                   eval      cmzlba = vlrfl1
062400010201     C                   call(e)   'TIS711C'
062500010201     C                   parm                    dscmz
062600010201     C                   parm      *blanks       esito
062700010201     C                   if        %error
062800010201     C                             or cmzerr = '1'
062900010201     C                             or esito  = '1'
063000010201     C                   eval      wrkesito = '3'
063100010201     C                   endif
063200010205     C                   endif
063300990920     C*
063400000613     C                   ENDSR
063500000613     C***
063600070411
063700070411     C     *pssr         BEGSR
063800070411     C*
063900070411     C                   if        %open(tivin00r)
064000070411     C                   close     tivin00r
064100070411     C                   endif
064200070411     C                   if        %open(fivabwwr)
064300070411     C                   close     fivabwwr
064400070411     C                   endif
064500070411     C                   if        %open(fivatwwr)
064600070411     C                   close     fivatwwr
064700070411     C                   endif
064800070411     C*
064900070411     C* Effettuo la chiamata al CLLE preposto
065000070411     C                   call(e)   'TITVVTC'
065100070411     C                   parm                    parccm
065200070411     C                   parm                    parmbr
065300070411     C                   parm      '2'           paropz
065400070411     C*
065500070411     C                   eval      wrkesito = '2'
065600070411     C*
065700070411     C                   seton                                        LR
065800070411     C*
065900070411     C                   ENDSR     '*CANCL'
066000070411     C***
066100070411
066200990910
066300000613     C     *inzsr        BEGSR
066400990910     C*
066500990910     C     *entry        plist
066600990920     C                   parm                    tivlrds
066700990921     C                   parm      wrkesito      esito
066800000724     C                   parm                    prmlit
066900000710     C                   parm                    prmfir
067000000613     C*
067100000830     C* CALCOLA LA DATA CORRENTE
067200000830     C                   time                    wn14             14 0
067300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067400000830     C                   z-add     wn8           g08dat
067500000830     C                   z-add     *zeros        g08inv
067600000830     C                   movel     '0'           g08err
067700000830     C                   call      'XSRDA8'
067800000830     C                   parm                    wlbda8
067900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068000000830     C*
068100000613     C                   ENDSR
068200000613     C***
068300990908
068400070927     Otitv1a9p  E            riepilogo         2
068500990915     O                                              'Upload via Internet'
068600990915     O                                           +1 'Traduzione TIVIN00R -'
068700021113     O                                           +1 'FIVABWWR/FIVATWWR'
068800010201     O                                           +1 'Report testate bolle'
068900990915     O          E            riepilogo   2
069000990915     O                       wrkdata
069100990915     O                       wrkora              +1
069200990915     O                       procname            +1
069300990915     O          E            riepilogo   2
069400990915     O                                              'Cliente..................:'
069500990915     O                       VABCCM        z     +1
069600990915     O          E            riepilogo   2
069700990920     O                                              'Riferimento Strategi.....:'
069800990920     O                       vlrhdl              +1
069900990915     O          E            riepilogo   2
070000990915     O                                              'Giusti...................:'
070100010201     O                       §CTROKVB      2   +  1
070200990915     O          E            riepilogo   2
070300990915     O                                              'Sbagliati e corretti.....:'
070400971022     O                       §CTRMO        2   +  1
070500990915     O          E            riepilogo   2
070600990915     O                                              'Sbagliati e scartati.....:'
070700971022     O                       §CTRNO        2   +  1
070800000613
070900070927     Otitv1a9ps E            testdett          2
071000000613     O                                              'Upload via Internet'
071100000613     O                                           +1 'Traduzione TIVIN00R -'
071200021113     O                                           +1 'FIVABWWR/FIVATWWR'
071300010201     O                                           +1 'Report testate bolle'
071400000616     O          E            testdett    3
071500000613     O                                           +2 'N° rec'
071600000613     O                                           +3 'Anteprima contenuto'
071700000616     O          E            rigadett    2
071800000613     O                       rrnum               +2
071900000621     O                       recko               +3
072000000616     O          E            findett     2
072100000613     O                       wrkdata
072200000613     O                       wrkora              +1
072300000613     O                       procname            +1
072400000616     O          E            findett     2
072500000613     O                                              'Cliente..................:'
072600000613     O                       VABCCM        z     +1
072700000616     O          E            findett     2
072800000613     O                                              'Riferimento Strategi.....:'
072900000613     O                       vlrhdl              +1
073000000616     O          E            findett     2
073100000613     O                                              'Giusti...................:'
073200010201     O                       §CTROKVB      2   +  1
073300000616     O          E            findett     2
073400000613     O                                              'Sbagliati e corretti.....:'
073500000613     O                       §CTRMO        2   +  1
073600000616     O          E            findett     2
073700000613     O                                              'Sbagliati e scartati.....:'
073800000613     O                       §CTRNO        2   +  1
073900000512** CMD - COMANDI CL
074000070927OVRPRTF FILE(TITV1A9P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074100070927OVRPRTF FILE(TITV1A9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074200070927DLTOVR FILE(TITV1A9P TITV1A9PS) LVL(*)
074300000512
074400000512
