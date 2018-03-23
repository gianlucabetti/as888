000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090114     FFIVABwwr  O    E             DISK    usropn
000700090114     FFIVATwwr  O    E             DISK    usropn
000800090331     Ftitv1o2p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090331     Ftitv1o2ps O    f  198        PRINTER usropn
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
009800090331     C                   if        not %open(titv1o2ps)
009900090331     C                   open      titv1o2ps
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
011800090331     C                   if        %open(titv1o2ps)
011900000616     C                   except    findett
012000090331     C                   close     titv1o2ps
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
013800090331     C                   if        not %open(titv1o2p)
013900090331     C                   open      titv1o2p
014000990915     C                   endif
014100990915     C*
014200990915     C                   except    riepilogo
014300990915     C*
014400090331     C                   if        %open(titv1o2p)
014500090331     C                   close     titv1o2p
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
034301090331     C                   EVAL      VABRD2=%trim(%subst(vindta:13+35:40-35))
034500090331     C                   EVAL      VABIND=%trim(%subst(vindta:53:30))
034700090331     C                   EVAL      VABLOD=%trim(%subst(vindta:88:28))
034800090331     C                   EVAL      VABPRD=%trim(%subst(vindta:116:2))
034900090331     C                   EVAL      VABRMA=%trim(%subst(vindta:1:6))
036900080409     C*
037000080409     C* Reperimento campi NUMERICI
037100090309     C* NSP
037200090331     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
037300090309     C                   EXSR      CHKNUM
037400090309     C                   IF        PiInt=*on
037500090309     C                   Z-ADD     PiVal         VABNSP
037600090309     C                   Z-ADD     PiVal         VATNSP
037700090309     C                   ELSE
037800090309     C                   EVAL      vinmsg = %trimr(vinmsg)
037900090309     C                             + ' ' + 'VABNSP VATNSP'
038000090309     C                   ENDIF
038100090309     C* RMN
038200090331     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
038300081217     C                   EXSR      CHKNUM
038400081217     C                   IF        PiInt=*on
038500081217     C                   Z-ADD     PiVal         VABRMN
038600081217     C                   ELSE
038700081217     C                   ADD       1             errore
038800081217     C                   Z-ADD     1             VABRMN
038900081217     C                   EVAL      vinmsg = %trimr(vinmsg)
039000081217     C                             + ' ' + 'VABRMN'
039100081217     C                   ENDIF
039200080409     C* CAD
039300090331     C                   EVAL      PiStr=%trim(%subst(vindta:83:5))
039400090114     C                   EXSR      CHKNUM
039500090114     C                   IF        PiInt=*on
039600090114     C                   Z-ADD     PiVal         Num5_0
039700090114     C                   MOVEL(p)  Num5_0        VABCAD
039800090114     C                   ELSE
039900090114     C                   ADD       1             errore
040000090114     C                   EVAL      VABCAD = *zeros
040100090114     C                   EVAL      vinmsg = %trimr(vinmsg)
040200090114     C                             + ' ' + 'VABCAD'
040300090114     C                   ENDIF
040400081217     C* NCL
040500090331     C                   EVAL      PiStr=%trim(%subst(vindta:124:16))
040600081217     C                   EXSR      CHKNUM
040700081217     C                   IF        PiInt=*on
040800081217     C                   Z-ADD     PiVal         VABNCL
040900081217     C                   ELSE
041000090309     C                   ADD       1             errore
041100081217     C                   EVAL      vinmsg = %trimr(vinmsg)
041200081217     C                             + ' ' + 'VABNCL'
041300081217     C                   ENDIF
044200090312     C* PKB
044300090331     C                   EVAL      PiStr=%trim(%subst(vindta:118:6))
044400090312     C                   EXSR      CHKNUM
044500090312     C                   IF        PiNum=*on
044501090331     C                   EVAL      PiVal=PiVal / 10                             * gestisco 1 dec.
044600090312     C                   Z-ADD(H)  PiVal         VABPKB
044700090312     C                   ELSE
044800090312     C                   ADD       1             errore
044900090312     C                   EVAL      vinmsg = %trimr(vinmsg)
045000090312     C                             + ' ' + 'VABPKB'
045100090312     C                   ENDIF
046200071121     C*
046300080415     C* Carico l'estensioni A e B del FNVAT
046400080512     C                   exsr      exevata
046500080512     C                   exsr      exevatb
046600010205     C*
046700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
046800040802     C                   IF        FlgCAS <> '0'
046900010205     C                   EVAL      VABCBO = '4'
047000010205     C                   ENDIF
047100020305     C*
047200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047300011113     C                   EXSR      CHKIMPDIV
047400010202     C*
047500000801     C* Ebbene...
047600000801     C                   ADD       1             §CTRMO
047700010201     C                   IF        errore <> *zeros
047800000801     C                   ADD       1             §CTRNO
047900000801     C                   EVAL      recko = vindta
048000000801     C                   EXCEPT    rigadett
048100000801     C                   EVAL      vinflg = '2'
048200000801     C                   ELSE
048300010201     C                   ADD       1             §CTROKVB
048400000801     C                   ENDIF
048500000801     C*
048600000801     C                   ENDSR
048700070102     C*----------------------------------------------------*
048800090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
048900070102     C*----------------------------------------------------*
049000070103     C     EXEVATE       BEGSR
049100070102     C*
049200070103     C                   EVAL      VATTRC='E'
049300090312     C*
049400090312     C* X ogni collo indicato in testata
049500090331     C                   z-add     1             wColli            4 0
049600090312     C*
049700090312     C                   dow       wColli <= VABNCL
049800090312     C*
049900090312     C* Valorizzo il buffer di scrittura del FIVAT
050000090331     C                   move(p)   wColli        wColliAlfa        4
050100090331     C                   eval      VATNOT = '0319'+%trim(%subst(vindta:7:2))+
050101090331     C                                             %trim(%subst(vindta:1:6))+
050200090331     C                                             wColliAlfa+'A'
050300090312     C                   exsr      wrivat                                       => scarico VAT
050400090312     C*
050500090312     C                   add       1             wColli
050600090312     C                   enddo
050700070102     C*
050800070102     C                   ENDSR
050900080415     C*----------------------------------------------------*
051000090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
051100080415     C*----------------------------------------------------*
051200080415     C     EXEVATA       BEGSR
051300080415     C*
051400080415     C*
051500080415     C***                exsr      wrivat                                       => scarico VAT
051600080415     C*
051700080415     C                   ENDSR
051800071121     C*----------------------------------------------------*
051900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
052000071121     C*----------------------------------------------------*
052100071121     C     EXEVATB       BEGSR
052200071121     C*
052300071121     C*
052400080409     C***                exsr      wrivat                                       => scarico VAT
052500071121     C*
052600071121     C                   ENDSR
052700010201     C*----------------------------------------------------*
052800090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
052900010201     C*----------------------------------------------------*
053000020305     C     WRIVAT        BEGSR
053100050628     C*
053200060223     C* Scrivo solo se valorizzato qualcosa
053300060223     C                   IF        VATNOT <> *blanks
053400090114     C                   WRITE     FIVAT000
053500060223     C                   ENDIF
053600010201     C*
053700010201     C                   ENDSR
053800010202     C*----------------------------------------------------*
053900090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054000010202     C*----------------------------------------------------*
054100020305     C     PREVAT        BEGSR
054200010202     C*
054300090114     C* Compongo il nome del membro da dare al FIVATWWR
054400010202     C                   eval      parmbr = vlrhdl
054500010202     C                   movel     'M'           parmbr
054600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
054700010202     C                   eval      paropz = '1'
054800010202     C* Effettuo la chiamata al CLLE preposto
054900090114     C                   call(e)   'TITVVTC'
055000010202     C                   parm                    parccm
055100010202     C                   parm                    parmbr
055200010202     C                   parm                    paropz
055300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055400010202     C                   if        %error
055500010202     C                   movel     '1'           chkcall
055600010202     C                   else
055700010202     C                   movel     '0'           chkcall
055800010202     C                   endif
055900010202     C*
056000010202     C                   ENDSR
056100000801     C*----------------------------------------------------*
056200000801     C*  CONTROLLO NUMERICITA' CAMPI
056300000801     C*----------------------------------------------------*
056400000801     C     CHKNUM        BEGSR
056500081217     C*
056600081217     C                   IF        PiDecChr = *blanks
056700090312     C                   EVAL      PiDecChr = ','
056800081217     C                   ENDIF
056900081217     C*
057000081217     C                   callp(e)  UBISNUM_Check(PiStr
057100081217     C                                          :PiDecChr
057200081217     C                                          :PiVal
057300081217     C                                          :PiNum
057400081217     C                                          :PiInt)
057500081217     C*
057600081217     C                   IF        %error
057700081217     C                   EVAL      PiInt=*off
057800081217     C                   ENDIF
057900000801     C*
058000000801     C                   ENDSR
058100000801     C***
058200000801
058300011113
058400011113     C*----------------------------------------------------*
058500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058600011113     C*----------------------------------------------------*
058700011113     C     CHKIMPDIV     BEGSR
058800011113     C*
058900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059000011113     C                   Z-ADD     *zeros        wrkDec            9 9
059100011113     C*
059200011113     C* Come prima cosa effettuo considerazioni sulla divisa
059300011113     C                   IF        vabIAS > *zeros
059400011113     C                   IF        vabVAS <> 'EUR'
059500011113     C                   EVAL      vabVAS =  'ITL'
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C                   IF        vabCAS > *zeros
060000011113     C                   IF        vabVCA <> 'EUR'
060100011113     C                   EVAL      vabVCA =  'ITL'
060200011113     C                   ENDIF
060300011113     C                   ENDIF
060400011113     C*
060500011113     C                   IF        vabVMD > *zeros
060600020305     C                   IF        vabVAD <> 'EUR'
060700011113     C                   EVAL      vabVAD =  'ITL'
060800011113     C                   ENDIF
060900011113     C                   ENDIF
061000011113     C*
061100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061200011113     C                   Z-ADD     vabIAS        wrkDec
061300011113     C                   IF        wrkDec > *zeros
061400011113     C                   IF        vabVAS = 'ITL'
061500011113     C                   EVAL      vabIAS = *zeros
061600011113     C                   ENDIF
061700011113     C                   ENDIF
061800011113     C*
061900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062000011113     C                   Z-ADD     vabCAS        wrkDec
062100011113     C                   IF        wrkDec > *zeros
062200011113     C                   IF        vabVCA = 'ITL'
062300011113     C                   EVAL      vabCAS = *zeros
062400011113     C                   ENDIF
062500011113     C                   ENDIF
062600011113     C*
062700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062800011113     C                   Z-ADD     vabVMD        wrkDec
062900011113     C                   IF        wrkDec > *zeros
063000011113     C                   IF        vabVAD = 'ITL'
063100011113     C                   EVAL      vabVMD = *zeros
063200011113     C                   ENDIF
063300011113     C                   ENDIF
063400011113     C*
063500011113     C                   ENDSR
063600011113     C***
063700011113
063800011113
063900000801
064000000801
064100990920      /TITLE Invio dei dati al punto operativo.
064200010202     C     invio         BEGSR
064300990920     C*
064400090114     C* 1° invio FIVAT
064500010201     C                   reset                   dscmz
064600010201     C                   move      vlrpoi        cmzdst
064700090114     C                   eval      cmzfld = 'FIVATWWR'
064800010201     C                   eval      cmzmbd = vlrhdl
064900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065000021009     C***                if        prmfir = *blanks
065100090114     C                   eval      cmzfla = 'FIVAT00F'
065200090114     C                   eval      cmzmba = 'FIVAT00F'
065300021009     C***                else
065400021009     C***                eval      cmzfla = prmfir
065500021009     C***                eval      cmzmba = prmfir
065600021009     C***                endif
065700010201     C                   eval      cmznrr = *zeros
065800020305     C                   move      §ctrokvt      cmznrr
065900021018     C                   eval      cmzlba = vlrfl1
066000010201     C                   call(e)   'TIS711C'
066100010201     C                   parm                    dscmz
066200010201     C                   parm      *blanks       esito
066300010205     C                   if        %error
066400010205     C                             or cmzerr = '1'
066500010205     C                             or esito  = '1'
066600010205     C                   eval      wrkesito = '3'
066700010205     C                   else
066800010201     C*
066900090114     C* 2° invio FIVAB
067000010201     C                   reset                   dscmz
067100010201     C                   move      vlrpoi        cmzdst
067200010201     C                   eval      cmzfld = vlrfou
067300010201     C                   eval      cmzmbd = vlrhdl
067400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067500021009     C***                if        prmfir = *blanks
067600090114     C                   eval      cmzfla = 'FIVAB00F'
067700090114     C                   eval      cmzmba = 'FIVAB00F'
067800021009     C***                else
067900021009     C***                eval      cmzfla = prmfir
068000021009     C***                eval      cmzmba = prmfir
068100021009     C***                endif
068200010201     C                   eval      cmznrr = *zeros
068300010201     C                   move      §ctrokvb      cmznrr
068400021018     C                   eval      cmzlba = vlrfl1
068500010201     C                   call(e)   'TIS711C'
068600010201     C                   parm                    dscmz
068700010201     C                   parm      *blanks       esito
068800010201     C                   if        %error
068900010201     C                             or cmzerr = '1'
069000010201     C                             or esito  = '1'
069100010201     C                   eval      wrkesito = '3'
069200010201     C                   endif
069300010205     C                   endif
069400990920     C*
069500000613     C                   ENDSR
069600000613     C***
069700070411
069701090331
069702090331
069703090331
069800070411     C     *pssr         BEGSR
069900070411     C*
070000070411     C                   if        %open(tivin00r)
070100070411     C                   close     tivin00r
070200070411     C                   endif
070300090114     C                   if        %open(fivabwwr)
070400090114     C                   close     fivabwwr
070500070411     C                   endif
070600090114     C                   if        %open(fivatwwr)
070700090114     C                   close     fivatwwr
070800070411     C                   endif
070900070411     C*
071000070411     C* Effettuo la chiamata al CLLE preposto
071100090114     C                   call(e)   'TITVVTC'
071200070411     C                   parm                    parccm
071300070411     C                   parm                    parmbr
071400070411     C                   parm      '2'           paropz
071500070411     C*
071600070411     C                   eval      wrkesito = '2'
071700070411     C*
071800070411     C                   seton                                        LR
071900070411     C*
072000070411     C                   ENDSR     '*CANCL'
072100070411     C***
072200070411
072201090331
072202090331
072300990910
072400000613     C     *inzsr        BEGSR
072500990910     C*
072600990910     C     *entry        plist
072700990920     C                   parm                    tivlrds
072800990921     C                   parm      wrkesito      esito
072900000724     C                   parm                    prmlit
073000000710     C                   parm                    prmfir
073100000613     C*
073200000830     C* CALCOLA LA DATA CORRENTE
073300000830     C                   time                    wn14             14 0
073400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
073500080512     C                   movel     wn14          oracor            6 0          *ORA
073600000830     C                   z-add     wn8           g08dat
073700000830     C                   z-add     *zeros        g08inv
073800000830     C                   movel     '0'           g08err
073900000830     C                   call      'XSRDA8'
074000000830     C                   parm                    wlbda8
074100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
074200000830     C*
074300000613     C                   ENDSR
074400000613     C***
074500990908
074501090331
074502090331
074600090331     Otitv1o2p  E            riepilogo         2
074700990915     O                                              'Upload via Internet'
074800990915     O                                           +1 'Traduzione TIVIN00R -'
074900090114     O                                           +1 'FIVABWWR/FIVATWWR'
075000010201     O                                           +1 'Report testate bolle'
075100990915     O          E            riepilogo   2
075200990915     O                       wrkdata
075300990915     O                       wrkora              +1
075400990915     O                       procname            +1
075500990915     O          E            riepilogo   2
075600990915     O                                              'Cliente..................:'
075700990915     O                       VABCCM        z     +1
075800990915     O          E            riepilogo   2
075900990920     O                                              'Riferimento Strategi.....:'
076000990920     O                       vlrhdl              +1
076100990915     O          E            riepilogo   2
076200990915     O                                              'Giusti...................:'
076300010201     O                       §CTROKVB      2   +  1
076400990915     O          E            riepilogo   2
076500990915     O                                              'Sbagliati e corretti.....:'
076600971022     O                       §CTRMO        2   +  1
076700990915     O          E            riepilogo   2
076800990915     O                                              'Sbagliati e scartati.....:'
076900971022     O                       §CTRNO        2   +  1
077000000613
077100090331     Otitv1o2ps E            testdett          2
077200000613     O                                              'Upload via Internet'
077300000613     O                                           +1 'Traduzione TIVIN00R -'
077400090114     O                                           +1 'FIVABWWR/FIVATWWR'
077500010201     O                                           +1 'Report testate bolle'
077600000616     O          E            testdett    3
077700000613     O                                           +2 'N° rec'
077800000613     O                                           +3 'Anteprima contenuto'
077900000616     O          E            rigadett    2
078000000613     O                       rrnum               +2
078100000621     O                       recko               +3
078200000616     O          E            findett     2
078300000613     O                       wrkdata
078400000613     O                       wrkora              +1
078500000613     O                       procname            +1
078600000616     O          E            findett     2
078700000613     O                                              'Cliente..................:'
078800000613     O                       VABCCM        z     +1
078900000616     O          E            findett     2
079000000613     O                                              'Riferimento Strategi.....:'
079100000613     O                       vlrhdl              +1
079200000616     O          E            findett     2
079300000613     O                                              'Giusti...................:'
079400010201     O                       §CTROKVB      2   +  1
079500000616     O          E            findett     2
079600000613     O                                              'Sbagliati e corretti.....:'
079700000613     O                       §CTRMO        2   +  1
079800000616     O          E            findett     2
079900000613     O                                              'Sbagliati e scartati.....:'
080000000613     O                       §CTRNO        2   +  1
080100000512** CMD - COMANDI CL
080200090331OVRPRTF FILE(TITV1O2P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
080300090331OVRPRTF FILE(TITV1O2PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
080400090331DLTOVR FILE(TITV1O2P TITV1O2PS) LVL(*)
080500000512
080600000512
