000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800080604     Ftitv1g4p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100080604     Ftitv1g4ps O    f  198        PRINTER usropn
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
004000080604     D curSped         s             10    INZ(*blanks)
004100080604     D depSped         s             10    INZ(*blanks)
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
008800080604     C                   if        not %open(titv1g4ps)
008900080604     C                   open      titv1g4ps
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
010800080604     C                   if        %open(titv1g4ps)
010900000616     C                   except    findett
011000080604     C                   close     titv1g4ps
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
012800080604     C                   if        not %open(titv1g4p)
012900080604     C                   open      titv1g4p
013000990915     C                   endif
013100990915     C*
013200990915     C                   except    riepilogo
013300990915     C*
013400080604     C                   if        %open(titv1g4p)
013500080604     C                   close     titv1g4p
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
018400080604     C                   eval      curSped = %subst(vindta:60:10)
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
019800080604     C                   exsr      exevate                                      => write VAT-E
019900070103     C* Salvo il raggruppamento spedizione cliente corrente
020000070103     C                   eval      depSped = curSped
020100070213     C*
020200070213     C* Se collo successivo x stessa bolla
020300070213     C                   else
020400070213     C                   exsr      impvab                                       => carico  VAB
020500080604     C                   exsr      exevate                                      => write VAT-E
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
025800080825     C                   if        vabnsp > *zeros
025900021113     C                   write     fivab000                                     => scarico il VAB
026000080825     C                   endif
026100010305     C*
026200010305     C                   ENDSR
026300990920
026400000801     C*----------------------------------------------------*
026500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026600000801     C*----------------------------------------------------*
026700010201     C     INZVAR        BEGSR
026800000801     C*
026900070213     C                   CLEAR                   FIVAB000
027000070213     C                   CLEAR                   FIVAT000
027100070213     C*
027200040802     C                   Z-ADD     *zeros        Num5_0            5 0
027300040802     C                   MOVEL     '0'           FlgCAS            1
027400000801     C*
027500000801     C                   ENDSR
027600000801     C*----------------------------------------------------*
027700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027800000801     C*----------------------------------------------------*
027900000801     C     DEFCAM        BEGSR
028000000801     C*
028100020619     C* Imposto i valori di default...
028200080604     C                   Z-ADD     0410828       VABCCM
028300080604     C                   Z-ADD     041           VABLNP
028400080604     C                   Z-ADD     000           VABCTR
028500070103     C                   MOVEL     '7Q'          VABCTM
028600040823     C                   MOVEL     '1'           VABCBO
028700020619     C* ... e poi verifico se sono stati passati come parametri
028800020619     C                   IF        vlrppt > *blanks
028900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029100020619     C                   EXSR      CHKNUM
029200020619     C                   IF        PiInt=*on
029300020619     C                   Z-ADD     PiVal         VABCCM
029400020619     C                   ENDIF
029500040506     C                   ENDIF
029600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029800020619     C                   EXSR      CHKNUM
029900020619     C                   IF        PiInt=*on
030000020619     C                   Z-ADD     PiVal         VABLNP
030100040506     C                   ENDIF
030200020619     C                   ENDIF
030300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
030400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030500020619     C                   EXSR      CHKNUM
030600020619     C                   IF        PiInt=*on
030700020619     C                   Z-ADD     PiVal         VABCTR
030800040506     C                   ENDIF
030900020619     C                   ENDIF
031000020619     C                   ENDIF
031100000801     C*
031200000801     C                   ENDSR
031300000801     C*----------------------------------------------------*
031400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031500000801     C*----------------------------------------------------*
031600040823     C     IMPVAB        BEGSR
031700010305     C*
031800080604     C* Elaboro solamente i tipi record "Dettagliio"
031900080604     C                   IF        %subst(vindta:1:1) = 'D'
032000080604     C*
032100000801     C                   Z-ADD     *zeros        errore            1 0
032200000830     C                   MOVEL     datcor        VABAAS
032300040526     C                   MOVE      datcor        VABMGS
032400040823     C                   MOVE(P)   vlrpoi        VABFGS
032500080604     C*
032600080604     C* Verifica campi derivati
032700080604     C                   IF        %subst(vindta:41:1) = 'F' OR
032800080604     C                             %subst(vindta:41:1) = 'O'
032900081215     C***                EVAL      VABCCM = 0410829
033000080604     C                   ENDIF
033100080604     C*
033200080604     C                   IF        %trim(%subst(vindta:344:2)) = '3'
033300080604     C                   EVAL      VABCBO = '1'
033400080604     C                   ENDIF
033500080604     C                   IF        %trim(%subst(vindta:344:2)) = '4'
033600080604     C                   EVAL      VABCBO = '2'
033700080604     C                   ENDIF
033800080409     C*
033900080409     C* Reperimento campi ALFA
034000080409     C*
034100080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
034200080604     C                   EVAL      VABRSD=%trim(%subst(vindta:96:35))
034300080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034400080409     C     '@':'A'       XLATE     VABRSD        VABRSD
034500080409     C* ==
034600080604     C                   EVAL      VABRD2=%trim(%subst(vindta:131:35))
034700080604     C                   EVAL      VABIND=%trim(%subst(vindta:166:35))
034800080604     C                   EVAL      VABLOD=%trim(%subst(vindta:201:25))
034900080604     C                   EVAL      VABPRD=%trim(%subst(vindta:226:2))
035000110530     C                   EVAL      VABRMA=%trim(%subst(vindta:454:15))
035100080409     C*
035200080409     C* Reperimento campi NUMERICI
035300080409     C                   MOVEL     DATCOR        VABAAS
035400080409     C                   MOVE      DATCOR        VABMGS
035500080409     C* NSP/RMN
035600080604     C                   EVAL      PiStr=%trim(%subst(vindta:60:10))
035700080409     C                   EXSR      CHKNUM
035800080409     C                   IF        PiInt=*on
035900080409     C                   Z-ADD     PiVal         VABNSP
036000080409     C                   Z-ADD     PiVal         VABRMN
036100080409     C                   ELSE
036200080409     C                   ADD       1             errore
036300080409     C                   Z-ADD     *zeros        VABNSP
036400080409     C                   Z-ADD     1             VABRMN
036500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
036600080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
036700080409     C                   ENDIF
036800080409     C* CAD
036900080604     C                   EVAL      PiStr=%trim(%subst(vindta:261:9))
037000080409     C                   EXSR      CHKNUM
037100080409     C                   IF        PiInt=*on
037200080409     C                   Z-ADD     PiVal         Num5_0
037300080409     C                   MOVEL(p)  Num5_0        VABCAD
037400080409     C                   ELSE
037500080409     C                   ADD       1             errore
037600080409     C                   EVAL      VABCAD = *zeros
037700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
037800080409     C                             + ' ' + 'VABCAD'
037900080409     C                   ENDIF
038000080604     C* NCL - 1
038100080604     C                   EVAL      PiStr=%trim(%subst(vindta:295:5))
038200080409     C                   EXSR      CHKNUM
038300080409     C                   IF        PiInt=*on
038400080604     C                   ADD       PiVal         VABNCL
038500080409     C                   ELSE
038600080409     C                   ADD       1             errore
038700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
038800080409     C                             + ' ' + 'VABNCL'
038900080409     C                   ENDIF
039000080604     C* NCL - 2
039100080604     C                   EVAL      PiStr=%trim(%subst(vindta:305:5))
039200080604     C                   EXSR      CHKNUM
039300080604     C                   IF        PiInt=*on
039400080604     C                   ADD       PiVal         VABNCL
039500080604     C                   ELSE
039600080604     C                   ADD       1             errore
039700080604     C                   EVAL      vinmsg = %trimr(vinmsg)
039800080604     C                             + ' ' + 'VABNCL'
039900080604     C                   ENDIF
040000080604     C* PKB
040100080604     C                   EVAL      PiStr=%trim(%subst(vindta:325:7))
040200080409     C                   EXSR      CHKNUM
040300080409     C                   IF        PiNum=*on
040400080604     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
040500080604     C                   ADD(H)    PiVal         VABPKB
040600080409     C                   ELSE
040700080409     C                   ADD       1             errore
040800080409     C                   EVAL      vinmsg = %trimr(vinmsg)
040900080409     C                             + ' ' + 'VABPKB'
041000080409     C                   ENDIF
041100080604     C* VLB
041200080604     C                   EVAL      PiStr=%trim(%subst(vindta:332:12))
041300080604     C                   EXSR      CHKNUM
041400080604     C                   IF        PiNum=*on
041500080604     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
041600080604     C                   ADD(H)    PiVal         VABVLB
041700080604     C                   ELSE
041800080604     C                   ADD       1             errore
041900080604     C                   EVAL      vinmsg = %trimr(vinmsg)
042000080604     C                             + ' ' + 'VABVLB'
042100080604     C                   ENDIF
042200071121     C*
042300080415     C* Carico l'estensioni A e B del FNVAT
042400080415     C                   exsr      exevata
042500071121     C                   exsr      exevatb
042600010205     C*
042700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042800040802     C                   IF        FlgCAS <> '0'
042900010205     C                   ENDIF
043000020305     C*
043100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043200011113     C                   EXSR      CHKIMPDIV
043300010202     C*
043400000801     C* Ebbene...
043500000801     C                   ADD       1             §CTRMO
043600010201     C                   IF        errore <> *zeros
043700000801     C                   ADD       1             §CTRNO
043800000801     C                   EVAL      recko = vindta
043900000801     C                   EXCEPT    rigadett
044000000801     C                   EVAL      vinflg = '2'
044100000801     C                   ELSE
044200010201     C                   ADD       1             §CTROKVB
044300000801     C                   ENDIF
044400080604     C*
044500080604     C                   ENDIF
044600000801     C*
044700000801     C                   ENDSR
044800070102     C*----------------------------------------------------*
044900070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045000070102     C*----------------------------------------------------*
045100070103     C     EXEVATE       BEGSR
045200070102     C*
045300080604     C***                exsr      wrivat                                       => scarico VAT
045400070102     C*
045500070102     C                   ENDSR
045600080415     C*----------------------------------------------------*
045700080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
045800080415     C*----------------------------------------------------*
045900080415     C     EXEVATA       BEGSR
046000080604     C*
046100080604     C                   eval      vatTRC = 'A'
046200080604     C                   eval      vatNOT = '.'
046300080604     C*
046400080604     C                   exsr      wrivat                                       => scarico VAT
046500080415     C*
046600080415     C                   ENDSR
046700071121     C*----------------------------------------------------*
046800071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
046900071121     C*----------------------------------------------------*
047000071121     C     EXEVATB       BEGSR
047100080604     C*
047200080604     C                   eval      vatTRC = 'B'
047300080604     C                   eval      vatNOT = %trim(%subst(vindta:270:20))
047400071121     C*
047500080604     C                   exsr      wrivat                                       => scarico VAT
047600071121     C*
047700071121     C                   ENDSR
047800010201     C*----------------------------------------------------*
047900040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048000010201     C*----------------------------------------------------*
048100020305     C     WRIVAT        BEGSR
048200080604     C*
048300080604     C                   eval      vatAAS = vabAAS
048400080604     C                   eval      vatLNP = vabLNP
048500080604     C                   eval      vatNRS = vabNRS
048600080604     C                   eval      vatNSP = vabNSP
048700080604     C                   eval      vatCCM = vabCCM
048800080604     C                   eval      vatFGS = vabFGS
048900050628     C*
049000060223     C* Scrivo solo se valorizzato qualcosa
049100080825     C                   IF        VATNOT <> *blanks AND
049200080825     C                             VATNSP  > *zeros
049300040802     C                   WRITE     FIVAT000
049400060223     C                   ENDIF
049500010201     C*
049600010201     C                   ENDSR
049700010202     C*----------------------------------------------------*
049800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049900010202     C*----------------------------------------------------*
050000020305     C     PREVAT        BEGSR
050100010202     C*
050200021113     C* Compongo il nome del membro da dare al FIVATWWR
050300010202     C                   eval      parmbr = vlrhdl
050400010202     C                   movel     'M'           parmbr
050500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050600010202     C                   eval      paropz = '1'
050700010202     C* Effettuo la chiamata al CLLE preposto
050800040506     C                   call(e)   'TITVVTC'
050900010202     C                   parm                    parccm
051000010202     C                   parm                    parmbr
051100010202     C                   parm                    paropz
051200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051300010202     C                   if        %error
051400010202     C                   movel     '1'           chkcall
051500010202     C                   else
051600010202     C                   movel     '0'           chkcall
051700010202     C                   endif
051800010202     C*
051900010202     C                   ENDSR
052000000801     C*----------------------------------------------------*
052100000801     C*  CONTROLLO NUMERICITA' CAMPI
052200000801     C*----------------------------------------------------*
052300000801     C     CHKNUM        BEGSR
052400000801     C*
052500000801     C                   call(e)   'ISNUMERIC'
052600000801     C                   PARM                    PiStr            30
052700070213     C                   PARM      ','           PiDecChr          1
052800000801     C                   PARM      *ZEROS        PiVal            30 9
052900000801     C                   PARM      '0'           PiInt             1
053000000801     C                   PARM      '0'           PiNum             1
053100000801     C                   IF        %error
053200000801     C                   EVAL      PiInt=*off
053300000801     C                   ENDIF
053400000801     C*
053500000801     C                   ENDSR
053600000801     C***
053700000801
053800011113
053900011113     C*----------------------------------------------------*
054000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054100011113     C*----------------------------------------------------*
054200011113     C     CHKIMPDIV     BEGSR
054300011113     C*
054400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054500011113     C                   Z-ADD     *zeros        wrkDec            9 9
054600011113     C*
054700011113     C* Come prima cosa effettuo considerazioni sulla divisa
054800011113     C                   IF        vabIAS > *zeros
054900011113     C                   IF        vabVAS <> 'EUR'
055000011113     C                   EVAL      vabVAS =  'ITL'
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C                   IF        vabCAS > *zeros
055500011113     C                   IF        vabVCA <> 'EUR'
055600011113     C                   EVAL      vabVCA =  'ITL'
055700011113     C                   ENDIF
055800011113     C                   ENDIF
055900011113     C*
056000011113     C                   IF        vabVMD > *zeros
056100020305     C                   IF        vabVAD <> 'EUR'
056200011113     C                   EVAL      vabVAD =  'ITL'
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056700011113     C                   Z-ADD     vabIAS        wrkDec
056800011113     C                   IF        wrkDec > *zeros
056900011113     C                   IF        vabVAS = 'ITL'
057000011113     C                   EVAL      vabIAS = *zeros
057100011113     C                   ENDIF
057200011113     C                   ENDIF
057300011113     C*
057400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057500011113     C                   Z-ADD     vabCAS        wrkDec
057600011113     C                   IF        wrkDec > *zeros
057700011113     C                   IF        vabVCA = 'ITL'
057800011113     C                   EVAL      vabCAS = *zeros
057900011113     C                   ENDIF
058000011113     C                   ENDIF
058100011113     C*
058200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058300011113     C                   Z-ADD     vabVMD        wrkDec
058400011113     C                   IF        wrkDec > *zeros
058500011113     C                   IF        vabVAD = 'ITL'
058600011113     C                   EVAL      vabVMD = *zeros
058700011113     C                   ENDIF
058800011113     C                   ENDIF
058900011113     C*
059000011113     C                   ENDSR
059100011113     C***
059200011113
059300011113
059400000801
059500000801
059600990920      /TITLE Invio dei dati al punto operativo.
059700010202     C     invio         BEGSR
059800990920     C*
059900021113     C* 1° invio FIVAT
060000010201     C                   reset                   dscmz
060100010201     C                   move      vlrpoi        cmzdst
060200021113     C                   eval      cmzfld = 'FIVATWWR'
060300010201     C                   eval      cmzmbd = vlrhdl
060400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060500021009     C***                if        prmfir = *blanks
060600021113     C                   eval      cmzfla = 'FIVAT00F'
060700021113     C                   eval      cmzmba = 'FIVAT00F'
060800021009     C***                else
060900021009     C***                eval      cmzfla = prmfir
061000021009     C***                eval      cmzmba = prmfir
061100021009     C***                endif
061200010201     C                   eval      cmznrr = *zeros
061300020305     C                   move      §ctrokvt      cmznrr
061400021018     C                   eval      cmzlba = vlrfl1
061500010201     C                   call(e)   'TIS711C'
061600010201     C                   parm                    dscmz
061700010201     C                   parm      *blanks       esito
061800010205     C                   if        %error
061900010205     C                             or cmzerr = '1'
062000010205     C                             or esito  = '1'
062100010205     C                   eval      wrkesito = '3'
062200010205     C                   else
062300010201     C*
062400021113     C* 2° invio FIVAB
062500010201     C                   reset                   dscmz
062600010201     C                   move      vlrpoi        cmzdst
062700010201     C                   eval      cmzfld = vlrfou
062800010201     C                   eval      cmzmbd = vlrhdl
062900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063000021009     C***                if        prmfir = *blanks
063100021113     C                   eval      cmzfla = 'FIVAB00F'
063200021113     C                   eval      cmzmba = 'FIVAB00F'
063300021009     C***                else
063400021009     C***                eval      cmzfla = prmfir
063500021009     C***                eval      cmzmba = prmfir
063600021009     C***                endif
063700010201     C                   eval      cmznrr = *zeros
063800010201     C                   move      §ctrokvb      cmznrr
063900021018     C                   eval      cmzlba = vlrfl1
064000010201     C                   call(e)   'TIS711C'
064100010201     C                   parm                    dscmz
064200010201     C                   parm      *blanks       esito
064300010201     C                   if        %error
064400010201     C                             or cmzerr = '1'
064500010201     C                             or esito  = '1'
064600010201     C                   eval      wrkesito = '3'
064700010201     C                   endif
064800010205     C                   endif
064900990920     C*
065000000613     C                   ENDSR
065100000613     C***
065200070411
065300070411     C     *pssr         BEGSR
065400070411     C*
065500070411     C                   if        %open(tivin00r)
065600070411     C                   close     tivin00r
065700070411     C                   endif
065800070411     C                   if        %open(fivabwwr)
065900070411     C                   close     fivabwwr
066000070411     C                   endif
066100070411     C                   if        %open(fivatwwr)
066200070411     C                   close     fivatwwr
066300070411     C                   endif
066400070411     C*
066500070411     C* Effettuo la chiamata al CLLE preposto
066600070411     C                   call(e)   'TITVVTC'
066700070411     C                   parm                    parccm
066800070411     C                   parm                    parmbr
066900070411     C                   parm      '2'           paropz
067000070411     C*
067100070411     C                   eval      wrkesito = '2'
067200070411     C*
067300070411     C                   seton                                        LR
067400070411     C*
067500070411     C                   ENDSR     '*CANCL'
067600070411     C***
067700070411
067800990910
067900000613     C     *inzsr        BEGSR
068000990910     C*
068100990910     C     *entry        plist
068200990920     C                   parm                    tivlrds
068300990921     C                   parm      wrkesito      esito
068400000724     C                   parm                    prmlit
068500000710     C                   parm                    prmfir
068600000613     C*
068700000830     C* CALCOLA LA DATA CORRENTE
068800000830     C                   time                    wn14             14 0
068900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069000000830     C                   z-add     wn8           g08dat
069100000830     C                   z-add     *zeros        g08inv
069200000830     C                   movel     '0'           g08err
069300000830     C                   call      'XSRDA8'
069400000830     C                   parm                    wlbda8
069500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069600000830     C*
069700000613     C                   ENDSR
069800000613     C***
069900990908
070000080604     Otitv1g4p  E            riepilogo         2
070100990915     O                                              'Upload via Internet'
070200990915     O                                           +1 'Traduzione TIVIN00R -'
070300021113     O                                           +1 'FIVABWWR/FIVATWWR'
070400010201     O                                           +1 'Report testate bolle'
070500990915     O          E            riepilogo   2
070600990915     O                       wrkdata
070700990915     O                       wrkora              +1
070800990915     O                       procname            +1
070900990915     O          E            riepilogo   2
071000990915     O                                              'Cliente..................:'
071100990915     O                       VABCCM        z     +1
071200990915     O          E            riepilogo   2
071300990920     O                                              'Riferimento Strategi.....:'
071400990920     O                       vlrhdl              +1
071500990915     O          E            riepilogo   2
071600990915     O                                              'Giusti...................:'
071700010201     O                       §CTROKVB      2   +  1
071800990915     O          E            riepilogo   2
071900990915     O                                              'Sbagliati e corretti.....:'
072000971022     O                       §CTRMO        2   +  1
072100990915     O          E            riepilogo   2
072200990915     O                                              'Sbagliati e scartati.....:'
072300971022     O                       §CTRNO        2   +  1
072400000613
072500080604     Otitv1g4ps E            testdett          2
072600000613     O                                              'Upload via Internet'
072700000613     O                                           +1 'Traduzione TIVIN00R -'
072800021113     O                                           +1 'FIVABWWR/FIVATWWR'
072900010201     O                                           +1 'Report testate bolle'
073000000616     O          E            testdett    3
073100000613     O                                           +2 'N° rec'
073200000613     O                                           +3 'Anteprima contenuto'
073300000616     O          E            rigadett    2
073400000613     O                       rrnum               +2
073500000621     O                       recko               +3
073600000616     O          E            findett     2
073700000613     O                       wrkdata
073800000613     O                       wrkora              +1
073900000613     O                       procname            +1
074000000616     O          E            findett     2
074100000613     O                                              'Cliente..................:'
074200000613     O                       VABCCM        z     +1
074300000616     O          E            findett     2
074400000613     O                                              'Riferimento Strategi.....:'
074500000613     O                       vlrhdl              +1
074600000616     O          E            findett     2
074700000613     O                                              'Giusti...................:'
074800010201     O                       §CTROKVB      2   +  1
074900000616     O          E            findett     2
075000000613     O                                              'Sbagliati e corretti.....:'
075100000613     O                       §CTRMO        2   +  1
075200000616     O          E            findett     2
075300000613     O                                              'Sbagliati e scartati.....:'
075400000613     O                       §CTRNO        2   +  1
075500000512** CMD - COMANDI CL
075600080604OVRPRTF FILE(TITV1G4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075700080604OVRPRTF FILE(TITV1G4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075800080604DLTOVR FILE(TITV1G4P TITV1G4PS) LVL(*)
075900000512
076000000512
