000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081118     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800081118     Ftitv1k3p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100081118     Ftitv1k3ps O    f  198        PRINTER usropn
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
004000081118     D curSped         s             15    INZ(*blanks)
004100081118     D depSped         s             15    INZ(*blanks)
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
005200081118
005300081118     D*------------------
005400081118     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
005500081118     D*------------------
005600081118     D PiStr           S            100A   INZ
005700081118     D PiDecChr        S              1A   INZ
005800081118     D PiVal           S             63S30 INZ
005900081118     D PiInt           S               N   INZ
006000081118     D PiNum           S               N   INZ
006100081118
006200081118     D*------------------
006300081118     D* LINKING A DEFINIZIONI ESTERNE
006400081118     D*------------------
006500081118     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006600081118
006700081118
006800010201
006900990915     C                   time                    wrkdata
007000990915     C                   time                    wrkora
007100000913     C                   reset                   rrnum
007200990921     C                   reset                   esito
007300990921     C                   reset                   wrkesito
007400000724     C*
007500000724     C* SE OCCORRE SPEDIRE IN FILIALE
007600000724     C                   if        vlrpoi <> *zeros
007700000724     C*
007800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008000000724     C     vlrpoi        chain     azorg01l
008100000724     C                   if        %found
008200000616     C                   movel(p)  CMD(1)        depcmd
008300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008400000616     C*
008500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008600011113     C                   Z-ADD     150           LENGH            15 5
008700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008800000616     C                   PARM                    depcmd
008900000616     C                   PARM                    LENGH
009000000724     C*
009100000724     C                   endif
009200000724     C                   endif
009300000616     C*
009400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009500000616     C                   movel(p)  CMD(2)        depcmd
009600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009700000616     C*
009800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009900011113     C                   Z-ADD     150           LENGH            15 5
010000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010100000616     C                   PARM                    depcmd
010200000616     C                   PARM                    LENGH
010300000616     C*
010400081118     C                   if        not %open(titv1k3ps)
010500081118     C                   open      titv1k3ps
010600000616     C                   except    testdett
010700000613     C                   endif
010800000613     C*
010900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
011000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011100000613     C*
011200010202     C* Effettuo la chiamata al CLLE preposto
011300040506     C                   call(e)   'TITVVTC'
011400010202     C                   parm                    parccm
011500010202     C                   parm                    parmbr
011600010202     C                   parm      '2'           paropz
011700050201     C*
011800050201     C* Effettuo lancio TISI95 solo x chiusura
011900050201     C                   CLEAR                   TISI95DS
012000050201     C                   EVAL      I95TLA = 'C'
012100050201     C                   CALL      'TISI95R'
012200050201     C                   PARM                    TISI95DS
012300010202     C*
012400081118     C                   if        %open(titv1k3ps)
012500000616     C                   except    findett
012600081118     C                   close     titv1k3ps
012700000613     C                   endif
012800000616     C*
012900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013000011113     C                   Z-ADD     150           LENGH            15 5
013100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013200000616     C                   PARM                    CMD(3)
013300000616     C                   PARM                    LENGH
013400000616     C*
013500000801     C
013600010201     C                   seton                                        LR
013700000613
013800000613
013900000613     C*--------------------------------------------------------
014000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014100000613     C*--------------------------------------------------------
014200000621     C     STPR          BEGSR
014300000613     C*
014400081118     C                   if        not %open(titv1k3p)
014500081118     C                   open      titv1k3p
014600990915     C                   endif
014700990915     C*
014800990915     C                   except    riepilogo
014900990915     C*
015000081118     C                   if        %open(titv1k3p)
015100081118     C                   close     titv1k3p
015200990914     C                   endif
015300990910     C*
015400000613     C                   ENDSR
015500000613     C***
015600990908
015700000801
015800910830     C*--------------------------------------------------------
015900040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
016000910830     C*--------------------------------------------------------
016100040526     C     RWFILE        BEGSR
016200990910     C*
016300990914     C                   if        not %open(tivin00r)
016400990908     C                   open      tivin00r
016500990914     C                   endif
016600021113     C                   if        not %open(fivabwwr)
016700021113     C                   open      fivabwwr
016800990914     C                   endif
016900070103     C*
017000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017100020305     C                   exsr      prevat
017200010201     C*
017300010202     C                   if        chkcall = '0'
017400010202     C*
017500021113     C                   if        not %open(fivatwwr)
017600021113     C                   open      fivatwwr
017700010201     C                   endif
017800990910     C*
017900010201     C                   clear                   §CTROKVB          5 0
018000020305     C                   clear                   §CTROKVT          5 0
018100000801     C                   clear                   §CTRMO            5 0
018200000801     C                   clear                   §CTRNO            5 0
018300990910     C*
018400921023     C                   DO        *HIVAL
018500990913     C*
018600990915     C                   READ      tivin00r                               70
018700050627     C                   if        vindta > *blanks
018800000613     C                   add       1             rrnum
018900000801     C*
019000000801     C                   if        *in70 = *off
019100000801     C                             and
019200000801     C                             (vinflg = *blanks
019300000801     C                              or vinflg = '0'
019400000801     C                              or vinflg = '2')
019500000801     C*
019600000801     C                   clear                   vinmsg
019700000801     C                   eval      vinflg = '1'
019800070103     C*
019900070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
020000081118     C                   eval      curSped = %subst(vindta:35:15)
020100070213     C                   if        curSped <> depSped
020200070213     C* Se prima bolla => importo bolla corrente
020300070213     C                   if        §CTRMO = *zeros
020400070213     C                   exsr      inzvar
020500070213     C                   exsr      defcam
020600070213     C                   exsr      impvab                                       => carico  VAB
020700070213     C                   else
020800070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
020900070213     C                   exsr      wrivab                                       => scarico VAB
021000070213     C                   exsr      inzvar
021100070213     C                   exsr      defcam
021200070213     C                   exsr      impvab                                       => carico  VAB
021300070213     C                   endif
021400071121     C                   exsr      exevate                                      => write VAT-E
021500070103     C* Salvo il raggruppamento spedizione cliente corrente
021600070103     C                   eval      depSped = curSped
021700070213     C*
021800070213     C* Se collo successivo x stessa bolla
021900070213     C                   else
022000070213     C                   exsr      impvab                                       => carico  VAB
022100070213     C                   exsr      exevate                                      => write VAT-E
022200070103     C                   endif
022300000905     C*
022400000905     C                   else
022500000905     C                   eval      vinflg = '1'
022600050628     C                   endif
022700000905     C                   endif
022800000905     C*
022900000905     C  N70              update    tivin000
023000000905     C*
023100991022     C  N70              ENDdo
023200070213     C*
023300070213     C* Scarico testata bolla rimasta in sospesa
023400070213     C                   exsr      wrivab                                       => scarico VAB
023500010202     C*
023600010202     C                   endif
023700990910
023800990910     C* Se non ci sono record con errori ...
023900000710     C                   if        §ctrno = 0
024000990910     C* ... restituisco esito OK.
024100990921     C                   eval      wrkesito = '0'
024200990910     C                   else
024300010201     C                   if        §ctrokvb > 0
024400990921     C                   eval      wrkesito = '1'
024500000710     C                   else
024600000710     C                   eval      wrkesito = '2'
024700990910     C                   endif
024800000710     C                   endif
024900990910     C*
025000990914     C                   if        %open(tivin00r)
025100990908     C                   close     tivin00r
025200990914     C                   endif
025300021113     C                   if        %open(fivabwwr)
025400021113     C                   close     fivabwwr
025500990914     C                   endif
025600021113     C                   if        %open(fivatwwr)
025700021113     C                   close     fivatwwr
025800010201     C                   endif
025900990910     C*
026000010201     C                   if        §ctrokvb > 0
026100000724     C                             and vlrpoi <> *zeros
026200010202     C                   exsr      invio
026300990920     C                   endif
026400990920     C*
026500910830     C                   ENDSR
026600000613     C***
026700010305
026800010305     C*----------------------------------------------------*
026900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
027000010305     C*----------------------------------------------------*
027100020305     C     WRIVAB        BEGSR
027200010305     C*
027300060225     C* Quindi scarico il buffer del file d testata
027400021113     C                   write     fivab000                                     => scarico il VAB
027500010305     C*
027600010305     C                   ENDSR
027700990920
027800000801     C*----------------------------------------------------*
027900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028000000801     C*----------------------------------------------------*
028100010201     C     INZVAR        BEGSR
028200000801     C*
028300070213     C                   CLEAR                   FIVAB000
028400070213     C                   CLEAR                   FIVAT000
028500070213     C*
028600040802     C                   Z-ADD     *zeros        Num5_0            5 0
028700040802     C                   MOVEL     '0'           FlgCAS            1
028800000801     C*
028900000801     C                   ENDSR
029000000801     C*----------------------------------------------------*
029100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029200000801     C*----------------------------------------------------*
029300000801     C     DEFCAM        BEGSR
029400000801     C*
029500020619     C* Imposto i valori di default...
029600081118     C                   Z-ADD     0160967       VABCCM
029700081118     C                   Z-ADD     0160967       VATCCM
029800081118     C                   Z-ADD     059           VABLNP
029900081118     C                   Z-ADD     059           VATLNP
030000081118     C                   Z-ADD     000           VABCTR
030100070103     C                   MOVEL     '7Q'          VABCTM
030200040823     C                   MOVEL     '1'           VABCBO
030300020619     C* ... e poi verifico se sono stati passati come parametri
030400020619     C                   IF        vlrppt > *blanks
030500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
030600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030700020619     C                   EXSR      CHKNUM
030800020619     C                   IF        PiInt=*on
030900020619     C                   Z-ADD     PiVal         VABCCM
031000020619     C                   Z-ADD     PiVal         VATCCM
031100020619     C                   ENDIF
031200040506     C                   ENDIF
031300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
031400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031500020619     C                   EXSR      CHKNUM
031600020619     C                   IF        PiInt=*on
031700020619     C                   Z-ADD     PiVal         VABLNP
031800020619     C                   Z-ADD     PiVal         VATLNP
031900040506     C                   ENDIF
032000020619     C                   ENDIF
032100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
032200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032300020619     C                   EXSR      CHKNUM
032400020619     C                   IF        PiInt=*on
032500020619     C                   Z-ADD     PiVal         VABCTR
032600040506     C                   ENDIF
032700020619     C                   ENDIF
032800020619     C                   ENDIF
032900000801     C*
033000000801     C                   ENDSR
033100000801     C*----------------------------------------------------*
033200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033300000801     C*----------------------------------------------------*
033400040823     C     IMPVAB        BEGSR
033500010305     C*
033600000801     C                   Z-ADD     *zeros        errore            1 0
033700000830     C                   MOVEL     datcor        VABAAS
033800020305     C                   MOVEL     datcor        VATAAS
033900040526     C                   MOVE      datcor        VABMGS
034000040823     C                   MOVE(P)   vlrpoi        VABFGS
034100040823     C                   MOVE(P)   vlrpoi        VATFGS
034200080409     C*
034300080409     C* Reperimento campi ALFA
034400080409     C*
034500080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
034600081118     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
034700080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034800080409     C     '@':'A'       XLATE     VABRSD        VABRSD
034900080409     C* ==
035000081118     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
035100081118     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
035200081118     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
035300081118     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
035400081118     C                   EVAL      VABRMA=%trim(%subst(vindta:35:15))
035500081118     C                   MOVEL     *blanks       wNOTE            70
035600081118     C                   EVAL      wNOTE =%trim(%subst(vindta:306:50)) +
035700081118     C                                    %trim(%subst(vindta:427:50))
035800081118     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
035900081118     C                   EVAL      VABNT2=%trim(%subst(wNOTE:35+1:35))
036000081118     C                   EVAL      VABCBO=%trim(%subst(vindta:10:1))
036100081121     C                   IF        %subst(vindta:210:1) = 'C'
036200081121     C                   EVAL      VABTIC = *blanks
036300081121     C                   ENDIF
036400081121     C                   IF        %subst(vindta:210:1) = 'A'
036500081121     C                   EVAL      VABTIC = 'BM'
036600081121     C                   ENDIF
036700080409     C*
036800080409     C* Reperimento campi NUMERICI
036900080409     C                   MOVEL     DATCOR        VABAAS
037000080409     C                   MOVE      DATCOR        VABMGS
037100080415     C* DCR
037200081118     C                   IF        %subst(vindta:11:8) <> *all'0'
037300081118     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
037400080415     C                   EXSR      CHKNUM
037500080415     C                   IF        PiInt=*on
037600080415     C                   Z-ADD     PiVal         VABDCR
037700080415     C                   ELSE
037800080415     C                   ADD       1             errore
037900080415     C                   Z-ADD     *zeros        VABDCR
038000080415     C                   EVAL      vinmsg = %trimr(vinmsg)
038100080415     C                             + ' ' + 'VABDCR'
038200080415     C                   ENDIF
038300081118     C                   ENDIF
038400080409     C* NSP/RMN
038500081118     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
038600080409     C                   EXSR      CHKNUM
038700080409     C                   IF        PiInt=*on
038800080409     C                   Z-ADD     PiVal         VABNSP
038900080409     C                   Z-ADD     PiVal         VATNSP
039000080409     C                   Z-ADD     PiVal         VABRMN
039100080409     C                   ELSE
039200080409     C                   ADD       1             errore
039300080409     C                   Z-ADD     *zeros        VABNSP
039400080409     C                   Z-ADD     1             VABRMN
039500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
039600080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
039700080409     C                   ENDIF
039800080409     C* CAD
039900081118     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
040000080409     C                   EXSR      CHKNUM
040100080409     C                   IF        PiInt=*on
040200080409     C                   Z-ADD     PiVal         Num5_0
040300080409     C                   MOVEL(p)  Num5_0        VABCAD
040400080409     C                   ELSE
040500080409     C                   ADD       1             errore
040600080409     C                   EVAL      VABCAD = *zeros
040700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
040800080409     C                             + ' ' + 'VABCAD'
040900080409     C                   ENDIF
041000080409     C* NCL
041100081118     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
041200080409     C                   EXSR      CHKNUM
041300080409     C                   IF        PiInt=*on
041400080409     C                   Z-ADD     PiVal         VABNCL
041500080409     C                   ELSE
041600080409     C                   ADD       1             errore
041700080409     C                   Z-ADD     *zeros        VABNCL
041800080409     C                   EVAL      vinmsg = %trimr(vinmsg)
041900080409     C                             + ' ' + 'VABNCL'
042000080409     C                   ENDIF
042100080409     C* PKB
042200081118     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
042300080409     C                   EXSR      CHKNUM
042400080409     C                   IF        PiNum=*on
042500081118     C                   Z-ADD     PiVal         VABPKB
042600080409     C                   ELSE
042700080409     C                   ADD       1             errore
042800080409     C                   Z-ADD     *zeros        VABPKB
042900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
043000080409     C                             + ' ' + 'VABPKB'
043100080409     C                   ENDIF
043200081118     C* VLB
043300081118     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
043400081118     C                   EXSR      CHKNUM
043500081118     C                   IF        PiNum=*on
043600081118     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
043700081204     C                   Z-ADD     PiVal         VABVLB
043800081118     C                   ELSE
043900081118     C                   ADD       1             errore
044000081204     C                   Z-ADD     *zeros        VABVLB
044100081118     C                   EVAL      vinmsg = %trimr(vinmsg)
044200081204     C                             + ' ' + 'VABVLB'
044300081118     C                   ENDIF
044400080409     C* CAS
044500081118     C                   IF        %subst(vindta:196:14) <> *all'0'
044600080409     C                   EVAL      FlgCAS = '1'
044700080415     C                   EVAL      VABVCA = 'EUR'
044800081118     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
044900080409     C                   EXSR      CHKNUM
045000080409     C                   IF        PiNum=*on
045100081118     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
045200080409     C                   Z-ADD     PiVal         VABCAS
045300080409     C                   ELSE
045400080409     C                   ADD       1             errore
045500080409     C                   Z-ADD     *zeros        VABCAS
045600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
045700080409     C                             + ' ' + 'VABCAS'
045800080409     C                   ENDIF
045900080409     C                   ENDIF
046000071121     C*
046100080415     C* Carico l'estensioni A e B del FNVAT
046200080415     C                   exsr      exevata
046300071121     C                   exsr      exevatb
046400010205     C*
046500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
046600040802     C                   IF        FlgCAS <> '0'
046700081118     C                   IF        VABCBO = '1'
046800010205     C                   EVAL      VABCBO = '4'
046900010205     C                   ENDIF
047000081118     C                   IF        VABCBO = '2'
047100081118     C                   EVAL      VABCBO = '6'
047200081118     C                   ENDIF
047300081118     C                   ENDIF
047400020305     C*
047500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047600011113     C                   EXSR      CHKIMPDIV
047700010202     C*
047800000801     C* Ebbene...
047900000801     C                   ADD       1             §CTRMO
048000010201     C                   IF        errore <> *zeros
048100000801     C                   ADD       1             §CTRNO
048200000801     C                   EVAL      recko = vindta
048300000801     C                   EXCEPT    rigadett
048400000801     C                   EVAL      vinflg = '2'
048500000801     C                   ELSE
048600010201     C                   ADD       1             §CTROKVB
048700000801     C                   ENDIF
048800000801     C*
048900000801     C                   ENDSR
049000070102     C*----------------------------------------------------*
049100070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
049200070102     C*----------------------------------------------------*
049300070103     C     EXEVATE       BEGSR
049400081118     C*
049500070102     C*
049600081118     C***                exsr      wrivat                                       => scarico VAT
049700070102     C*
049800070102     C                   ENDSR
049900080415     C*----------------------------------------------------*
050000080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
050100080415     C*----------------------------------------------------*
050200080415     C     EXEVATA       BEGSR
050300080415     C*
050400080415     C*
050500080415     C***                exsr      wrivat                                       => scarico VAT
050600080415     C*
050700080415     C                   ENDSR
050800071121     C*----------------------------------------------------*
050900071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
051000071121     C*----------------------------------------------------*
051100071121     C     EXEVATB       BEGSR
051200071121     C*
051300071121     C*
051400080409     C***                exsr      wrivat                                       => scarico VAT
051500071121     C*
051600071121     C                   ENDSR
051700010201     C*----------------------------------------------------*
051800040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
051900010201     C*----------------------------------------------------*
052000020305     C     WRIVAT        BEGSR
052100081118     C*
052200081118     C* Imposto gli stessi campi chiave della testata relativa
052300081118     C                   EVAL      VATAAS = VABAAS
052400081118     C                   EVAL      VATLNP = VABLNP
052500081118     C                   EVAL      VATNRS = VABNRS
052600081118     C                   EVAL      VATNSP = VABNSP
052700081118     C                   EVAL      VATCCM = VABCCM
052800050628     C*
052900060223     C* Scrivo solo se valorizzato qualcosa
053000060223     C                   IF        VATNOT <> *blanks
053100040802     C                   WRITE     FIVAT000
053200060223     C                   ENDIF
053300010201     C*
053400010201     C                   ENDSR
053500010202     C*----------------------------------------------------*
053600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053700010202     C*----------------------------------------------------*
053800020305     C     PREVAT        BEGSR
053900010202     C*
054000021113     C* Compongo il nome del membro da dare al FIVATWWR
054100010202     C                   eval      parmbr = vlrhdl
054200010202     C                   movel     'M'           parmbr
054300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
054400010202     C                   eval      paropz = '1'
054500010202     C* Effettuo la chiamata al CLLE preposto
054600040506     C                   call(e)   'TITVVTC'
054700010202     C                   parm                    parccm
054800010202     C                   parm                    parmbr
054900010202     C                   parm                    paropz
055000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055100010202     C                   if        %error
055200010202     C                   movel     '1'           chkcall
055300010202     C                   else
055400010202     C                   movel     '0'           chkcall
055500010202     C                   endif
055600010202     C*
055700010202     C                   ENDSR
055800000801     C*----------------------------------------------------*
055900000801     C*  CONTROLLO NUMERICITA' CAMPI
056000000801     C*----------------------------------------------------*
056100000801     C     CHKNUM        BEGSR
056200081118     C*
056300081118     C                   IF        PiDecChr = *blanks
056400081118     C                   EVAL      PiDecChr = ','
056500081118     C                   ENDIF
056600081118     C*
056700081118     C                   callp(e)  UBISNUM_Check(PiStr
056800081118     C                                          :PiDecChr
056900081118     C                                          :PiVal
057000081118     C                                          :PiNum
057100081118     C                                          :PiInt)
057200081118     C*
057300081118     C                   IF        %error
057400081118     C                   EVAL      PiInt=*off
057500081118     C                   ENDIF
057600000801     C*
057700000801     C                   ENDSR
057800000801     C***
057900000801
058000011113
058100011113     C*----------------------------------------------------*
058200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058300011113     C*----------------------------------------------------*
058400011113     C     CHKIMPDIV     BEGSR
058500011113     C*
058600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058700011113     C                   Z-ADD     *zeros        wrkDec            9 9
058800011113     C*
058900011113     C* Come prima cosa effettuo considerazioni sulla divisa
059000011113     C                   IF        vabIAS > *zeros
059100011113     C                   IF        vabVAS <> 'EUR'
059200011113     C                   EVAL      vabVAS =  'ITL'
059300011113     C                   ENDIF
059400011113     C                   ENDIF
059500011113     C*
059600011113     C                   IF        vabCAS > *zeros
059700011113     C                   IF        vabVCA <> 'EUR'
059800011113     C                   EVAL      vabVCA =  'ITL'
059900011113     C                   ENDIF
060000011113     C                   ENDIF
060100011113     C*
060200011113     C                   IF        vabVMD > *zeros
060300020305     C                   IF        vabVAD <> 'EUR'
060400011113     C                   EVAL      vabVAD =  'ITL'
060500011113     C                   ENDIF
060600011113     C                   ENDIF
060700011113     C*
060800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060900011113     C                   Z-ADD     vabIAS        wrkDec
061000011113     C                   IF        wrkDec > *zeros
061100011113     C                   IF        vabVAS = 'ITL'
061200011113     C                   EVAL      vabIAS = *zeros
061300011113     C                   ENDIF
061400011113     C                   ENDIF
061500011113     C*
061600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061700011113     C                   Z-ADD     vabCAS        wrkDec
061800011113     C                   IF        wrkDec > *zeros
061900011113     C                   IF        vabVCA = 'ITL'
062000011113     C                   EVAL      vabCAS = *zeros
062100011113     C                   ENDIF
062200011113     C                   ENDIF
062300011113     C*
062400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062500011113     C                   Z-ADD     vabVMD        wrkDec
062600011113     C                   IF        wrkDec > *zeros
062700011113     C                   IF        vabVAD = 'ITL'
062800011113     C                   EVAL      vabVMD = *zeros
062900011113     C                   ENDIF
063000011113     C                   ENDIF
063100011113     C*
063200011113     C                   ENDSR
063300011113     C***
063400011113
063500011113
063600000801
063700000801
063800990920      /TITLE Invio dei dati al punto operativo.
063900010202     C     invio         BEGSR
064000990920     C*
064100021113     C* 1° invio FIVAT
064200010201     C                   reset                   dscmz
064300010201     C                   move      vlrpoi        cmzdst
064400021113     C                   eval      cmzfld = 'FIVATWWR'
064500010201     C                   eval      cmzmbd = vlrhdl
064600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064700021009     C***                if        prmfir = *blanks
064800021113     C                   eval      cmzfla = 'FIVAT00F'
064900021113     C                   eval      cmzmba = 'FIVAT00F'
065000021009     C***                else
065100021009     C***                eval      cmzfla = prmfir
065200021009     C***                eval      cmzmba = prmfir
065300021009     C***                endif
065400010201     C                   eval      cmznrr = *zeros
065500020305     C                   move      §ctrokvt      cmznrr
065600021018     C                   eval      cmzlba = vlrfl1
065700010201     C                   call(e)   'TIS711C'
065800010201     C                   parm                    dscmz
065900010201     C                   parm      *blanks       esito
066000010205     C                   if        %error
066100010205     C                             or cmzerr = '1'
066200010205     C                             or esito  = '1'
066300010205     C                   eval      wrkesito = '3'
066400010205     C                   else
066500010201     C*
066600021113     C* 2° invio FIVAB
066700010201     C                   reset                   dscmz
066800010201     C                   move      vlrpoi        cmzdst
066900010201     C                   eval      cmzfld = vlrfou
067000010201     C                   eval      cmzmbd = vlrhdl
067100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067200021009     C***                if        prmfir = *blanks
067300021113     C                   eval      cmzfla = 'FIVAB00F'
067400021113     C                   eval      cmzmba = 'FIVAB00F'
067500021009     C***                else
067600021009     C***                eval      cmzfla = prmfir
067700021009     C***                eval      cmzmba = prmfir
067800021009     C***                endif
067900010201     C                   eval      cmznrr = *zeros
068000010201     C                   move      §ctrokvb      cmznrr
068100021018     C                   eval      cmzlba = vlrfl1
068200010201     C                   call(e)   'TIS711C'
068300010201     C                   parm                    dscmz
068400010201     C                   parm      *blanks       esito
068500010201     C                   if        %error
068600010201     C                             or cmzerr = '1'
068700010201     C                             or esito  = '1'
068800010201     C                   eval      wrkesito = '3'
068900010201     C                   endif
069000010205     C                   endif
069100990920     C*
069200000613     C                   ENDSR
069300000613     C***
069400070411
069500070411     C     *pssr         BEGSR
069600070411     C*
069700070411     C                   if        %open(tivin00r)
069800070411     C                   close     tivin00r
069900070411     C                   endif
070000070411     C                   if        %open(fivabwwr)
070100070411     C                   close     fivabwwr
070200070411     C                   endif
070300070411     C                   if        %open(fivatwwr)
070400070411     C                   close     fivatwwr
070500070411     C                   endif
070600070411     C*
070700070411     C* Effettuo la chiamata al CLLE preposto
070800070411     C                   call(e)   'TITVVTC'
070900070411     C                   parm                    parccm
071000070411     C                   parm                    parmbr
071100070411     C                   parm      '2'           paropz
071200070411     C*
071300070411     C                   eval      wrkesito = '2'
071400070411     C*
071500070411     C                   seton                                        LR
071600070411     C*
071700070411     C                   ENDSR     '*CANCL'
071800070411     C***
071900070411
072000990910
072100000613     C     *inzsr        BEGSR
072200990910     C*
072300990910     C     *entry        plist
072400990920     C                   parm                    tivlrds
072500990921     C                   parm      wrkesito      esito
072600000724     C                   parm                    prmlit
072700000710     C                   parm                    prmfir
072800000613     C*
072900000830     C* CALCOLA LA DATA CORRENTE
073000000830     C                   time                    wn14             14 0
073100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
073200000830     C                   z-add     wn8           g08dat
073300000830     C                   z-add     *zeros        g08inv
073400000830     C                   movel     '0'           g08err
073500000830     C                   call      'XSRDA8'
073600000830     C                   parm                    wlbda8
073700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
073800000830     C*
073900000613     C                   ENDSR
074000000613     C***
074100990908
074200081118     Otitv1k3p  E            riepilogo         2
074300990915     O                                              'Upload via Internet'
074400990915     O                                           +1 'Traduzione TIVIN00R -'
074500021113     O                                           +1 'FIVABWWR/FIVATWWR'
074600010201     O                                           +1 'Report testate bolle'
074700990915     O          E            riepilogo   2
074800990915     O                       wrkdata
074900990915     O                       wrkora              +1
075000990915     O                       procname            +1
075100990915     O          E            riepilogo   2
075200990915     O                                              'Cliente..................:'
075300990915     O                       VABCCM        z     +1
075400990915     O          E            riepilogo   2
075500990920     O                                              'Riferimento Strategi.....:'
075600990920     O                       vlrhdl              +1
075700990915     O          E            riepilogo   2
075800990915     O                                              'Giusti...................:'
075900010201     O                       §CTROKVB      2   +  1
076000990915     O          E            riepilogo   2
076100990915     O                                              'Sbagliati e corretti.....:'
076200971022     O                       §CTRMO        2   +  1
076300990915     O          E            riepilogo   2
076400990915     O                                              'Sbagliati e scartati.....:'
076500971022     O                       §CTRNO        2   +  1
076600000613
076700081118     Otitv1k3ps E            testdett          2
076800000613     O                                              'Upload via Internet'
076900000613     O                                           +1 'Traduzione TIVIN00R -'
077000021113     O                                           +1 'FIVABWWR/FIVATWWR'
077100010201     O                                           +1 'Report testate bolle'
077200000616     O          E            testdett    3
077300000613     O                                           +2 'N° rec'
077400000613     O                                           +3 'Anteprima contenuto'
077500000616     O          E            rigadett    2
077600000613     O                       rrnum               +2
077700000621     O                       recko               +3
077800000616     O          E            findett     2
077900000613     O                       wrkdata
078000000613     O                       wrkora              +1
078100000613     O                       procname            +1
078200000616     O          E            findett     2
078300000613     O                                              'Cliente..................:'
078400000613     O                       VABCCM        z     +1
078500000616     O          E            findett     2
078600000613     O                                              'Riferimento Strategi.....:'
078700000613     O                       vlrhdl              +1
078800000616     O          E            findett     2
078900000613     O                                              'Giusti...................:'
079000010201     O                       §CTROKVB      2   +  1
079100000616     O          E            findett     2
079200000613     O                                              'Sbagliati e corretti.....:'
079300000613     O                       §CTRMO        2   +  1
079400000616     O          E            findett     2
079500000613     O                                              'Sbagliati e scartati.....:'
079600000613     O                       §CTRNO        2   +  1
079700000512** CMD - COMANDI CL
079800081118OVRPRTF FILE(TITV1K3P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
079900081118OVRPRTF FILE(TITV1K3PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
080000081118DLTOVR FILE(TITV1K3P TITV1K3PS) LVL(*)
080100000512
080200000512
