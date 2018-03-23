000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090114     FFIVABwwr  O    E             DISK    usropn
000700090114     FFIVATwwr  O    E             DISK    usropn
000800090416     Ftitv1o8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090416     Ftitv1o8ps O    f  198        PRINTER usropn
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
004000090331     D curSped         s              6    INZ(*blanks)
004100090331     D depSped         s              6    INZ(*blanks)
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
009800090416     C                   if        not %open(titv1o8ps)
009900090416     C                   open      titv1o8ps
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
011800090416     C                   if        %open(titv1o8ps)
011900000616     C                   except    findett
012000090416     C                   close     titv1o8ps
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
013800090416     C                   if        not %open(titv1o8p)
013900090416     C                   open      titv1o8p
014000990915     C                   endif
014100990915     C*
014200990915     C                   except    riepilogo
014300990915     C*
014400090416     C                   if        %open(titv1o8p)
014500090416     C                   close     titv1o8p
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
019400090331     C                   eval      curSped = %subst(vindta:1:6)
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
029000090331     C                   Z-ADD     2493971       VABCCM
029100090331     C                   Z-ADD     2493971       VATCCM
029200090331     C                   Z-ADD     005           VABLNP
029300090331     C                   Z-ADD     005           VATLNP
029400081217     C                   Z-ADD     000           VABCTR
029500070103     C                   MOVEL     '7Q'          VABCTM
029600040823     C                   MOVEL     '1'           VABCBO
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
034000090331     C                   EVAL      VABRSD=%trim(%subst(vindta:13:35))
034100080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034200080409     C     '@':'A'       XLATE     VABRSD        VABRSD
034300080409     C* ==
034400090331     C                   EVAL      VABRD2=%trim(%subst(vindta:13+35:40-35))
034500090331     C                   EVAL      VABIND=%trim(%subst(vindta:53:30))
034600090331     C                   EVAL      VABLOD=%trim(%subst(vindta:88:28))
034700090331     C                   EVAL      VABPRD=%trim(%subst(vindta:116:2))
034800090331     C                   EVAL      VABRMA=%trim(%subst(vindta:1:6))
034900080409     C*
035000080409     C* Reperimento campi NUMERICI
035100090309     C* NSP
035200090331     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
035300090309     C                   EXSR      CHKNUM
035400090309     C                   IF        PiInt=*on
035500090309     C                   Z-ADD     PiVal         VABNSP
035600090309     C                   Z-ADD     PiVal         VATNSP
035700090309     C                   ELSE
035800090309     C                   EVAL      vinmsg = %trimr(vinmsg)
035900090309     C                             + ' ' + 'VABNSP VATNSP'
036000090309     C                   ENDIF
036100090309     C* RMN
036200090331     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
036300081217     C                   EXSR      CHKNUM
036400081217     C                   IF        PiInt=*on
036500081217     C                   Z-ADD     PiVal         VABRMN
036600081217     C                   ELSE
036700081217     C                   ADD       1             errore
036800081217     C                   Z-ADD     1             VABRMN
036900081217     C                   EVAL      vinmsg = %trimr(vinmsg)
037000081217     C                             + ' ' + 'VABRMN'
037100081217     C                   ENDIF
037200080409     C* CAD
037300090331     C                   EVAL      PiStr=%trim(%subst(vindta:83:5))
037400090114     C                   EXSR      CHKNUM
037500090114     C                   IF        PiInt=*on
037600090114     C                   Z-ADD     PiVal         Num5_0
037700090114     C                   MOVEL(p)  Num5_0        VABCAD
037800090114     C                   ELSE
037900090114     C                   ADD       1             errore
038000090114     C                   EVAL      VABCAD = *zeros
038100090114     C                   EVAL      vinmsg = %trimr(vinmsg)
038200090114     C                             + ' ' + 'VABCAD'
038300090114     C                   ENDIF
038400081217     C* NCL
038500090331     C                   EVAL      PiStr=%trim(%subst(vindta:124:16))
038600081217     C                   EXSR      CHKNUM
038700081217     C                   IF        PiInt=*on
038800081217     C                   Z-ADD     PiVal         VABNCL
038900081217     C                   ELSE
039000090309     C                   ADD       1             errore
039100081217     C                   EVAL      vinmsg = %trimr(vinmsg)
039200081217     C                             + ' ' + 'VABNCL'
039300081217     C                   ENDIF
039400090312     C* PKB
039500090331     C                   EVAL      PiStr=%trim(%subst(vindta:118:6))
039600090312     C                   EXSR      CHKNUM
039700090312     C                   IF        PiNum=*on
039800090331     C                   EVAL      PiVal=PiVal / 10                             * gestisco 1 dec.
039900090312     C                   Z-ADD(H)  PiVal         VABPKB
040000090312     C                   ELSE
040100090312     C                   ADD       1             errore
040200090312     C                   EVAL      vinmsg = %trimr(vinmsg)
040300090312     C                             + ' ' + 'VABPKB'
040400090312     C                   ENDIF
040500071121     C*
040600080415     C* Carico l'estensioni A e B del FNVAT
040700080512     C                   exsr      exevata
040800080512     C                   exsr      exevatb
040900010205     C*
041000010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041100040802     C                   IF        FlgCAS <> '0'
041200010205     C                   EVAL      VABCBO = '4'
041300010205     C                   ENDIF
041400020305     C*
041500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041600011113     C                   EXSR      CHKIMPDIV
041700010202     C*
041800000801     C* Ebbene...
041900000801     C                   ADD       1             §CTRMO
042000010201     C                   IF        errore <> *zeros
042100000801     C                   ADD       1             §CTRNO
042200000801     C                   EVAL      recko = vindta
042300000801     C                   EXCEPT    rigadett
042400000801     C                   EVAL      vinflg = '2'
042500000801     C                   ELSE
042600010201     C                   ADD       1             §CTROKVB
042700000801     C                   ENDIF
042800000801     C*
042900000801     C                   ENDSR
043000070102     C*----------------------------------------------------*
043100090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
043200070102     C*----------------------------------------------------*
043300070103     C     EXEVATE       BEGSR
043400070102     C*
043500070103     C                   EVAL      VATTRC='E'
043600090312     C*
043700090312     C* X ogni collo indicato in testata
043800090331     C                   z-add     1             wColli            4 0
043900090312     C*
044000090312     C                   dow       wColli <= VABNCL
044100090312     C*
044200090312     C* Valorizzo il buffer di scrittura del FIVAT
044300090331     C                   move(p)   wColli        wColliAlfa        4
044400090331     C                   eval      VATNOT = '0319'+%trim(%subst(vindta:7:2))+
044500090331     C                                             %trim(%subst(vindta:1:6))+
044600090331     C                                             wColliAlfa+'A'
044700090312     C                   exsr      wrivat                                       => scarico VAT
044800090312     C*
044900090312     C                   add       1             wColli
045000090312     C                   enddo
045100070102     C*
045200070102     C                   ENDSR
045300080415     C*----------------------------------------------------*
045400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
045500080415     C*----------------------------------------------------*
045600080415     C     EXEVATA       BEGSR
045700080415     C*
045800080415     C*
045900080415     C***                exsr      wrivat                                       => scarico VAT
046000080415     C*
046100080415     C                   ENDSR
046200071121     C*----------------------------------------------------*
046300090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
046400071121     C*----------------------------------------------------*
046500071121     C     EXEVATB       BEGSR
046600071121     C*
046700071121     C*
046800080409     C***                exsr      wrivat                                       => scarico VAT
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
051100090312     C                   EVAL      PiDecChr = ','
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
069700090416     Otitv1o8p  E            riepilogo         2
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
072200090416     Otitv1o8ps E            testdett          2
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
075300090416OVRPRTF FILE(TITV1O8P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075400090416OVRPRTF FILE(TITV1O8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075500090416DLTOVR FILE(TITV1O8P TITV1O8PS) LVL(*)
075600000512
075700000512
