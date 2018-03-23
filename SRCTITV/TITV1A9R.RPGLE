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
004000070927     D curSped         s              7    INZ(*blanks)
004100070927     D depSped         s              7    INZ(*blanks)
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
018400100112     C                   eval      curSped = %subst(vindta:6:7)
018500070213     C                   if        curSped <> depSped
018600070213     C* Se prima bolla => importo bolla corrente
018700070213     C                   if        §CTRMO = *zeros
018800070213     C                   exsr      inzvar
018900070213     C                   exsr      defcam
019000070213     C                   exsr      impvab                                       => carico  VAB
019100070213     C                   else
019200070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
019300070213     C                   exsr      wrivab                                       => scarico VAB
019400070213     C                   exsr      inzvar
019500070213     C                   exsr      defcam
019600070213     C                   exsr      impvab                                       => carico  VAB
019700070213     C                   endif
019800070213     C                   exsr      exevate                                      => write VAT-E
019900070103     C* Salvo il raggruppamento spedizione cliente corrente
020000070103     C                   eval      depSped = curSped
020100070213     C*
020200070213     C* Se collo successivo x stessa bolla
020300070213     C                   else
020400070213     C                   exsr      impvab                                       => carico  VAB
020500070213     C                   exsr      exevate                                      => write VAT-E
020600070103     C                   endif
020700000905     C*
020800000905     C                   else
020900000905     C                   eval      vinflg = '1'
021000050628     C                   endif
021100000905     C                   endif
021200000905     C*
021300000905     C  N70              update    tivin000
021400000905     C*
021500991022     C  N70              ENDdo
021600070213     C*
021700070213     C* Scarico testata bolla rimasta in sospesa
021800070213     C                   exsr      wrivab                                       => scarico VAB
021900010202     C*
022000010202     C                   endif
022100990910
022200990910     C* Se non ci sono record con errori ...
022300000710     C                   if        §ctrno = 0
022400990910     C* ... restituisco esito OK.
022500990921     C                   eval      wrkesito = '0'
022600990910     C                   else
022700010201     C                   if        §ctrokvb > 0
022800990921     C                   eval      wrkesito = '1'
022900000710     C                   else
023000000710     C                   eval      wrkesito = '2'
023100990910     C                   endif
023200000710     C                   endif
023300990910     C*
023400990914     C                   if        %open(tivin00r)
023500990908     C                   close     tivin00r
023600990914     C                   endif
023700021113     C                   if        %open(fivabwwr)
023800021113     C                   close     fivabwwr
023900990914     C                   endif
024000021113     C                   if        %open(fivatwwr)
024100021113     C                   close     fivatwwr
024200010201     C                   endif
024300990910     C*
024400010201     C                   if        §ctrokvb > 0
024500000724     C                             and vlrpoi <> *zeros
024600010202     C                   exsr      invio
024700990920     C                   endif
024800990920     C*
024900910830     C                   ENDSR
025000000613     C***
025100010305
025200010305     C*----------------------------------------------------*
025300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025400010305     C*----------------------------------------------------*
025500020305     C     WRIVAB        BEGSR
025600010305     C*
025700060225     C* Quindi scarico il buffer del file d testata
025800021113     C                   write     fivab000                                     => scarico il VAB
025900010305     C*
026000010305     C                   ENDSR
026100990920
026200000801     C*----------------------------------------------------*
026300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026400000801     C*----------------------------------------------------*
026500010201     C     INZVAR        BEGSR
026600000801     C*
026700070213     C                   CLEAR                   FIVAB000
026800070213     C                   CLEAR                   FIVAT000
026900070213     C*
027000040802     C                   Z-ADD     *zeros        Num5_0            5 0
027100040802     C                   MOVEL     '0'           FlgCAS            1
027200000801     C*
027300000801     C                   ENDSR
027400000801     C*----------------------------------------------------*
027500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027600000801     C*----------------------------------------------------*
027700000801     C     DEFCAM        BEGSR
027800000801     C*
027900020619     C* Imposto i valori di default...
028000070927     C                   Z-ADD     0013410       VABCCM
028100070927     C                   Z-ADD     0013410       VATCCM
028200070927     C                   Z-ADD     275           VABLNP
028300070927     C                   Z-ADD     275           VATLNP
028400070927     C                   Z-ADD     000           VABCTR
028500070103     C                   MOVEL     '7Q'          VABCTM
028600040823     C                   MOVEL     '1'           VABCBO
028700070928     C                   MOVEL     'E'           VABTSP
028800020619     C* ... e poi verifico se sono stati passati come parametri
028900020619     C                   IF        vlrppt > *blanks
029000040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029200020619     C                   EXSR      CHKNUM
029300020619     C                   IF        PiInt=*on
029400020619     C                   Z-ADD     PiVal         VABCCM
029500020619     C                   Z-ADD     PiVal         VATCCM
029600020619     C                   ENDIF
029700040506     C                   ENDIF
029800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030000020619     C                   EXSR      CHKNUM
030100020619     C                   IF        PiInt=*on
030200020619     C                   Z-ADD     PiVal         VABLNP
030300020619     C                   Z-ADD     PiVal         VATLNP
030400040506     C                   ENDIF
030500020619     C                   ENDIF
030600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
030700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030800020619     C                   EXSR      CHKNUM
030900020619     C                   IF        PiInt=*on
031000020619     C                   Z-ADD     PiVal         VABCTR
031100040506     C                   ENDIF
031200020619     C                   ENDIF
031300020619     C                   ENDIF
031400000801     C*
031500000801     C                   ENDSR
031600000801     C*----------------------------------------------------*
031700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031800000801     C*----------------------------------------------------*
031900040823     C     IMPVAB        BEGSR
032000010305     C*
032100000801     C                   Z-ADD     *zeros        errore            1 0
032200000830     C                   MOVEL     datcor        VABAAS
032300020305     C                   MOVEL     datcor        VATAAS
032400040526     C                   MOVE      datcor        VABMGS
032500040823     C                   MOVE(P)   vlrpoi        VABFGS
032600040823     C                   MOVE(P)   vlrpoi        VATFGS
032700050628     C*
032800100112     C                   EVAL      VABRSD=%trim(%subst(vindta:174:35))
032900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033100020117     C* ==
033200100112     C                   EVAL      VABRD2=%trim(%subst(vindta:209:35))
033300100112     C                   EVAL      VABIND=%trim(%subst(vindta:244:35))
033400100112     C                   EVAL      VABLOD=%trim(%subst(vindta:279:35))
033500100112     C                   EVAL      VABPRD=%trim(%subst(vindta:341:2))
033600100112     C                   EVAL      VABRMA=%trim(%subst(vindta:89:6))
033700100112     C                   EVAL      VABNOT=%trim(%subst(vindta:482:35))
033800100112     C                   EVAL      VABNT2=%trim(%subst(vindta:482+35:60-35))
033900050628     C* CAP
034000100112     C                   EVAL      PiStr=%trim(%subst(vindta:334:7))
034100010201     C                   EXSR      CHKNUM
034200010201     C                   IF        PiInt=*on
034300010201     C                   Z-ADD     PiVal         Num5_0
034400040506     C                   MOVEL(P)  Num5_0        VABCAD
034500010201     C                   ELSE
034600040506     C                   ADD       1             errore
034700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034800040506     C                             + ' ' + 'VABCAD'
034900010201     C                   ENDIF
035000040506     C* Reperisco la provincia dal CAP e dalla località
035100040526     C                   IF        VABCAD <> *blanks AND
035200040526     C                             VABPRD  = *blanks
035300040506     C                   CLEAR                   TISI95DS
035400040506     C                   EVAL      I95TCN = '3'
035500040506     C                   Z-ADD     datcor        I95DAT
035600040506     C                   EVAL      I95CAP = VABCAD
035700040506     C                   EVAL      I95LOC = VABLOD
035800050627     C                   EVAL      I95NAR = VABNZD
035900040506     C                   CALL      'TISI95R'
036000040506     C                   PARM                    TISI95DS
036100040506     C                   EVAL      VABPRD = O95PRV
036200040506     C                   ENDIF
036300060307     C* NCL
036400070213     C                   EVAL      VABNCL = VABNCL + 1
036500040506     C* PKB
036600100112     C                   EVAL      PiStr=%trim(%subst(vindta:378:7))
036700010201     C                   EXSR      CHKNUM
036800010201     C                   IF        PiNum=*on
036900070927     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
037000070927     C                   ADD(H)    PiVal         VABPKB
037100010201     C                   ELSE
037200010201     C                   ADD       1             errore
037300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037400010201     C                             + ' ' + 'VABPKB'
037500010201     C                   ENDIF
037600080111     C* VLB
037700100112     C                   EVAL      PiStr=%trim(%subst(vindta:403:7))
037800080111     C                   EXSR      CHKNUM
037900080111     C                   IF        PiNum=*on
038000080111     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
038100080111     C                   ADD(H)    PiVal         VABVLB
038200080111     C                   ELSE
038300080111     C                   ADD       1             errore
038400080111     C                   EVAL      vinmsg = %trimr(vinmsg)
038500080111     C                             + ' ' + 'VABVLB'
038600080111     C                   ENDIF
038700071019     C* RMN
038800100112     C*                  EVAL      PiStr=%trim(%subst(vindta:6:7))
038900071019     C*                  EXSR      CHKNUM
039000071019     C*                  IF        PiInt=*on
039100071019     C*                  Z-ADD     PiVal         VABRMN
039200071019     C*                  ELSE
039300071019     C*                  ADD       1             errore
039400071019     C*                  EVAL      vinmsg = %trimr(vinmsg)
039500071019     C*                            + ' ' + 'VABRMN'
039600071019     C*                  ENDIF
039700071019     C* NSP/RMN
039800100112     C                   EVAL      PiStr=%trim(%subst(vindta:101:7))
039900071019     C                   EXSR      CHKNUM
040000071019     C                   IF        PiInt=*on
040100071019     C                   Z-ADD     PiVal         VABNSP
040200071019     C                   Z-ADD     PiVal         VATNSP
040300071019     C                   Z-ADD     PiVal         VABRMN
040400071019     C                   ELSE
040500071019     C                   ADD       1             errore
040600071019     C                   EVAL      vinmsg = %trimr(vinmsg)
040700071019     C                             + ' ' + 'VABNSP VATNSP VABRMN'
040800071019     C                   ENDIF
040900010205     C*
041000010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041100040802     C                   IF        FlgCAS <> '0'
041200070102     C                   IF        VABCBO = '1'
041300010205     C                   EVAL      VABCBO = '4'
041400010205     C                   ELSE
041500070102     C                   EVAL      VABCBO = '6'
041600070102     C                   ENDIF
041700010205     C                   ENDIF
041800020305     C*
041900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042000011113     C                   EXSR      CHKIMPDIV
042100010202     C*
042200000801     C* Ebbene...
042300000801     C                   ADD       1             §CTRMO
042400010201     C                   IF        errore <> *zeros
042500000801     C                   ADD       1             §CTRNO
042600000801     C                   EVAL      recko = vindta
042700000801     C                   EXCEPT    rigadett
042800000801     C                   EVAL      vinflg = '2'
042900000801     C                   ELSE
043000010201     C                   ADD       1             §CTROKVB
043100000801     C                   ENDIF
043200000801     C*
043300000801     C                   ENDSR
043400070102     C*----------------------------------------------------*
043500070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
043600070102     C*----------------------------------------------------*
043700070103     C     EXEVATE       BEGSR
043800070102     C*
043900070103     C                   EVAL      VATTRC='E'
044000100112     C                   EVAL      VATNOT = %trim(%subst(vindta:1:19))
044100071019     C* Occorre togliere gli spazi eventualmente presenti all'interno della stringa barcode
044200071019     C                   Z-ADD     2             i                 2 0
044300071019     C                   DOW       i < %size(VATNOT)
044400071019     C                   IF        %subst(VATNOT:i:1) = *blanks
044500071019     C                   EVAL      VATNOT = %subst(VATNOT:1:i-1) +
044600071019     C                                      %subst(VATNOT:i+1)
044700071019     C                   ENDIF
044800071019     C                   ADD       1             i
044900071019     C                   ENDDO
045000071019     C*
045100070102     C                   exsr      wrivat                                       => scarico VAT
045200070102     C*
045300070102     C                   ENDSR
045400010201     C*----------------------------------------------------*
045500040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
045600010201     C*----------------------------------------------------*
045700020305     C     WRIVAT        BEGSR
045800050628     C*
045900060223     C* Scrivo solo se valorizzato qualcosa
046000060223     C                   IF        VATNOT <> *blanks
046100040802     C                   WRITE     FIVAT000
046200060223     C                   ENDIF
046300010201     C*
046400010201     C                   ENDSR
046500010202     C*----------------------------------------------------*
046600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046700010202     C*----------------------------------------------------*
046800020305     C     PREVAT        BEGSR
046900010202     C*
047000021113     C* Compongo il nome del membro da dare al FIVATWWR
047100010202     C                   eval      parmbr = vlrhdl
047200010202     C                   movel     'M'           parmbr
047300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
047400010202     C                   eval      paropz = '1'
047500010202     C* Effettuo la chiamata al CLLE preposto
047600040506     C                   call(e)   'TITVVTC'
047700010202     C                   parm                    parccm
047800010202     C                   parm                    parmbr
047900010202     C                   parm                    paropz
048000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048100010202     C                   if        %error
048200010202     C                   movel     '1'           chkcall
048300010202     C                   else
048400010202     C                   movel     '0'           chkcall
048500010202     C                   endif
048600010202     C*
048700010202     C                   ENDSR
048800000801     C*----------------------------------------------------*
048900000801     C*  CONTROLLO NUMERICITA' CAMPI
049000000801     C*----------------------------------------------------*
049100000801     C     CHKNUM        BEGSR
049200000801     C*
049300000801     C                   call(e)   'ISNUMERIC'
049400000801     C                   PARM                    PiStr            30
049500070213     C                   PARM      ','           PiDecChr          1
049600000801     C                   PARM      *ZEROS        PiVal            30 9
049700000801     C                   PARM      '0'           PiInt             1
049800000801     C                   PARM      '0'           PiNum             1
049900000801     C                   IF        %error
050000000801     C                   EVAL      PiInt=*off
050100000801     C                   ENDIF
050200000801     C*
050300000801     C                   ENDSR
050400000801     C***
050500000801
050600011113
050700011113     C*----------------------------------------------------*
050800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050900011113     C*----------------------------------------------------*
051000011113     C     CHKIMPDIV     BEGSR
051100011113     C*
051200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051300011113     C                   Z-ADD     *zeros        wrkDec            9 9
051400011113     C*
051500011113     C* Come prima cosa effettuo considerazioni sulla divisa
051600011113     C                   IF        vabIAS > *zeros
051700011113     C                   IF        vabVAS <> 'EUR'
051800011113     C                   EVAL      vabVAS =  'ITL'
051900011113     C                   ENDIF
052000011113     C                   ENDIF
052100011113     C*
052200011113     C                   IF        vabCAS > *zeros
052300011113     C                   IF        vabVCA <> 'EUR'
052400011113     C                   EVAL      vabVCA =  'ITL'
052500011113     C                   ENDIF
052600011113     C                   ENDIF
052700011113     C*
052800011113     C                   IF        vabVMD > *zeros
052900020305     C                   IF        vabVAD <> 'EUR'
053000011113     C                   EVAL      vabVAD =  'ITL'
053100011113     C                   ENDIF
053200011113     C                   ENDIF
053300011113     C*
053400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053500011113     C                   Z-ADD     vabIAS        wrkDec
053600011113     C                   IF        wrkDec > *zeros
053700011113     C                   IF        vabVAS = 'ITL'
053800011113     C                   EVAL      vabIAS = *zeros
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054300011113     C                   Z-ADD     vabCAS        wrkDec
054400011113     C                   IF        wrkDec > *zeros
054500011113     C                   IF        vabVCA = 'ITL'
054600011113     C                   EVAL      vabCAS = *zeros
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055100011113     C                   Z-ADD     vabVMD        wrkDec
055200011113     C                   IF        wrkDec > *zeros
055300011113     C                   IF        vabVAD = 'ITL'
055400011113     C                   EVAL      vabVMD = *zeros
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C                   ENDSR
055900011113     C***
056000011113
056100011113
056200000801
056300000801
056400990920      /TITLE Invio dei dati al punto operativo.
056500010202     C     invio         BEGSR
056600990920     C*
056700021113     C* 1° invio FIVAT
056800010201     C                   reset                   dscmz
056900010201     C                   move      vlrpoi        cmzdst
057000021113     C                   eval      cmzfld = 'FIVATWWR'
057100010201     C                   eval      cmzmbd = vlrhdl
057200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057300021009     C***                if        prmfir = *blanks
057400021113     C                   eval      cmzfla = 'FIVAT00F'
057500021113     C                   eval      cmzmba = 'FIVAT00F'
057600021009     C***                else
057700021009     C***                eval      cmzfla = prmfir
057800021009     C***                eval      cmzmba = prmfir
057900021009     C***                endif
058000010201     C                   eval      cmznrr = *zeros
058100020305     C                   move      §ctrokvt      cmznrr
058200021018     C                   eval      cmzlba = vlrfl1
058300010201     C                   call(e)   'TIS711C'
058400010201     C                   parm                    dscmz
058500010201     C                   parm      *blanks       esito
058600010205     C                   if        %error
058700010205     C                             or cmzerr = '1'
058800010205     C                             or esito  = '1'
058900010205     C                   eval      wrkesito = '3'
059000010205     C                   else
059100010201     C*
059200021113     C* 2° invio FIVAB
059300010201     C                   reset                   dscmz
059400010201     C                   move      vlrpoi        cmzdst
059500010201     C                   eval      cmzfld = vlrfou
059600010201     C                   eval      cmzmbd = vlrhdl
059700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059800021009     C***                if        prmfir = *blanks
059900021113     C                   eval      cmzfla = 'FIVAB00F'
060000021113     C                   eval      cmzmba = 'FIVAB00F'
060100021009     C***                else
060200021009     C***                eval      cmzfla = prmfir
060300021009     C***                eval      cmzmba = prmfir
060400021009     C***                endif
060500010201     C                   eval      cmznrr = *zeros
060600010201     C                   move      §ctrokvb      cmznrr
060700021018     C                   eval      cmzlba = vlrfl1
060800010201     C                   call(e)   'TIS711C'
060900010201     C                   parm                    dscmz
061000010201     C                   parm      *blanks       esito
061100010201     C                   if        %error
061200010201     C                             or cmzerr = '1'
061300010201     C                             or esito  = '1'
061400010201     C                   eval      wrkesito = '3'
061500010201     C                   endif
061600010205     C                   endif
061700990920     C*
061800000613     C                   ENDSR
061900000613     C***
062000070411
062100070411     C     *pssr         BEGSR
062200070411     C*
062300070411     C                   if        %open(tivin00r)
062400070411     C                   close     tivin00r
062500070411     C                   endif
062600070411     C                   if        %open(fivabwwr)
062700070411     C                   close     fivabwwr
062800070411     C                   endif
062900070411     C                   if        %open(fivatwwr)
063000070411     C                   close     fivatwwr
063100070411     C                   endif
063200070411     C*
063300070411     C* Effettuo la chiamata al CLLE preposto
063400070411     C                   call(e)   'TITVVTC'
063500070411     C                   parm                    parccm
063600070411     C                   parm                    parmbr
063700070411     C                   parm      '2'           paropz
063800070411     C*
063900070411     C                   eval      wrkesito = '2'
064000070411     C*
064100070411     C                   seton                                        LR
064200070411     C*
064300070411     C                   ENDSR     '*CANCL'
064400070411     C***
064500070411
064600990910
064700000613     C     *inzsr        BEGSR
064800990910     C*
064900990910     C     *entry        plist
065000990920     C                   parm                    tivlrds
065100990921     C                   parm      wrkesito      esito
065200000724     C                   parm                    prmlit
065300000710     C                   parm                    prmfir
065400000613     C*
065500000830     C* CALCOLA LA DATA CORRENTE
065600000830     C                   time                    wn14             14 0
065700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
065800000830     C                   z-add     wn8           g08dat
065900000830     C                   z-add     *zeros        g08inv
066000000830     C                   movel     '0'           g08err
066100000830     C                   call      'XSRDA8'
066200000830     C                   parm                    wlbda8
066300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066400000830     C*
066500000613     C                   ENDSR
066600000613     C***
066700990908
066800070927     Otitv1a9p  E            riepilogo         2
066900990915     O                                              'Upload via Internet'
067000990915     O                                           +1 'Traduzione TIVIN00R -'
067100021113     O                                           +1 'FIVABWWR/FIVATWWR'
067200010201     O                                           +1 'Report testate bolle'
067300990915     O          E            riepilogo   2
067400990915     O                       wrkdata
067500990915     O                       wrkora              +1
067600990915     O                       procname            +1
067700990915     O          E            riepilogo   2
067800990915     O                                              'Cliente..................:'
067900990915     O                       VABCCM        z     +1
068000990915     O          E            riepilogo   2
068100990920     O                                              'Riferimento Strategi.....:'
068200990920     O                       vlrhdl              +1
068300990915     O          E            riepilogo   2
068400990915     O                                              'Giusti...................:'
068500010201     O                       §CTROKVB      2   +  1
068600990915     O          E            riepilogo   2
068700990915     O                                              'Sbagliati e corretti.....:'
068800971022     O                       §CTRMO        2   +  1
068900990915     O          E            riepilogo   2
069000990915     O                                              'Sbagliati e scartati.....:'
069100971022     O                       §CTRNO        2   +  1
069200000613
069300070927     Otitv1a9ps E            testdett          2
069400000613     O                                              'Upload via Internet'
069500000613     O                                           +1 'Traduzione TIVIN00R -'
069600021113     O                                           +1 'FIVABWWR/FIVATWWR'
069700010201     O                                           +1 'Report testate bolle'
069800000616     O          E            testdett    3
069900000613     O                                           +2 'N° rec'
070000000613     O                                           +3 'Anteprima contenuto'
070100000616     O          E            rigadett    2
070200000613     O                       rrnum               +2
070300000621     O                       recko               +3
070400000616     O          E            findett     2
070500000613     O                       wrkdata
070600000613     O                       wrkora              +1
070700000613     O                       procname            +1
070800000616     O          E            findett     2
070900000613     O                                              'Cliente..................:'
071000000613     O                       VABCCM        z     +1
071100000616     O          E            findett     2
071200000613     O                                              'Riferimento Strategi.....:'
071300000613     O                       vlrhdl              +1
071400000616     O          E            findett     2
071500000613     O                                              'Giusti...................:'
071600010201     O                       §CTROKVB      2   +  1
071700000616     O          E            findett     2
071800000613     O                                              'Sbagliati e corretti.....:'
071900000613     O                       §CTRMO        2   +  1
072000000616     O          E            findett     2
072100000613     O                                              'Sbagliati e scartati.....:'
072200000613     O                       §CTRNO        2   +  1
072300000512** CMD - COMANDI CL
072400070927OVRPRTF FILE(TITV1A9P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
072500070927OVRPRTF FILE(TITV1A9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
072600070927DLTOVR FILE(TITV1A9P TITV1A9PS) LVL(*)
072700000512
072800000512
