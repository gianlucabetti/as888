000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200090113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800090312     Ftitv1n9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090312     Ftitv1n9ps O    f  198        PRINTER usropn
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
004000090113     D curSped         s             10    INZ(*blanks)
004100090113     D depSped         s             10    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100090113
005200090113     D*------------------
005300090113     D* LINKING A DEFINIZIONI ESTERNE
005400090113     D*------------------
005500090113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005600090113     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005700090113
005800010201
005900990915     C                   time                    wrkdata
006000990915     C                   time                    wrkora
006100000913     C                   reset                   rrnum
006200990921     C                   reset                   esito
006300990921     C                   reset                   wrkesito
006400000724     C*
006500000724     C* SE OCCORRE SPEDIRE IN FILIALE
006600000724     C                   if        vlrpoi <> *zeros
006700000724     C*
006800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007000000724     C     vlrpoi        chain     azorg01l
007100000724     C                   if        %found
007200000616     C                   movel(p)  CMD(1)        depcmd
007300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007400000616     C*
007500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007600011113     C                   Z-ADD     150           LENGH            15 5
007700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007800000616     C                   PARM                    depcmd
007900000616     C                   PARM                    LENGH
008000000724     C*
008100000724     C                   endif
008200000724     C                   endif
008300000616     C*
008400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008500000616     C                   movel(p)  CMD(2)        depcmd
008600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008700000616     C*
008800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008900011113     C                   Z-ADD     150           LENGH            15 5
009000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000616     C                   PARM                    depcmd
009200000616     C                   PARM                    LENGH
009300000616     C*
009400090312     C                   if        not %open(titv1n9ps)
009500090312     C                   open      titv1n9ps
009600000616     C                   except    testdett
009700000613     C                   endif
009800000613     C*
009900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010100000613     C*
010200010202     C* Effettuo la chiamata al CLLE preposto
010300040506     C                   call(e)   'TITVVTC'
010400010202     C                   parm                    parccm
010500010202     C                   parm                    parmbr
010600010202     C                   parm      '2'           paropz
010700050201     C*
010800050201     C* Effettuo lancio TISI95 solo x chiusura
010900050201     C                   CLEAR                   TISI95DS
011000050201     C                   EVAL      I95TLA = 'C'
011100050201     C                   CALL      'TISI95R'
011200050201     C                   PARM                    TISI95DS
011300010202     C*
011400090312     C                   if        %open(titv1n9ps)
011500000616     C                   except    findett
011600090312     C                   close     titv1n9ps
011700000613     C                   endif
011800000616     C*
011900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    CMD(3)
012300000616     C                   PARM                    LENGH
012400000616     C*
012500000801     C
012600010201     C                   seton                                        LR
012700000613
012800000613
012900000613     C*--------------------------------------------------------
013000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013100000613     C*--------------------------------------------------------
013200000621     C     STPR          BEGSR
013300000613     C*
013400090312     C                   if        not %open(titv1n9p)
013500090312     C                   open      titv1n9p
013600990915     C                   endif
013700990915     C*
013800990915     C                   except    riepilogo
013900990915     C*
014000090312     C                   if        %open(titv1n9p)
014100090312     C                   close     titv1n9p
014200990914     C                   endif
014300990910     C*
014400000613     C                   ENDSR
014500000613     C***
014600990908
014700000801
014800910830     C*--------------------------------------------------------
014900040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015000910830     C*--------------------------------------------------------
015100040526     C     RWFILE        BEGSR
015200990910     C*
015300990914     C                   if        not %open(tivin00r)
015400990908     C                   open      tivin00r
015500990914     C                   endif
015600021113     C                   if        not %open(fivabwwr)
015700021113     C                   open      fivabwwr
015800990914     C                   endif
015900070103     C*
016000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016100020305     C                   exsr      prevat
016200010201     C*
016300010202     C                   if        chkcall = '0'
016400010202     C*
016500021113     C                   if        not %open(fivatwwr)
016600021113     C                   open      fivatwwr
016700010201     C                   endif
016800990910     C*
016900010201     C                   clear                   §CTROKVB          5 0
017000020305     C                   clear                   §CTROKVT          5 0
017100000801     C                   clear                   §CTRMO            5 0
017200000801     C                   clear                   §CTRNO            5 0
017300990910     C*
017400921023     C                   DO        *HIVAL
017500990913     C*
017600990915     C                   READ      tivin00r                               70
017700050627     C                   if        vindta > *blanks
017800000613     C                   add       1             rrnum
017900000801     C*
018000000801     C                   if        *in70 = *off
018100000801     C                             and
018200000801     C                             (vinflg = *blanks
018300000801     C                              or vinflg = '0'
018400000801     C                              or vinflg = '2')
018500000801     C*
018600000801     C                   clear                   vinmsg
018700000801     C                   eval      vinflg = '1'
018800070103     C*
018900070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
019000090312     C                   eval      curSped = %subst(vindta:16:10)
019100070213     C                   if        curSped <> depSped
019200070213     C* Se prima bolla => importo bolla corrente
019300070213     C                   if        §CTRMO = *zeros
019400070213     C                   exsr      inzvar
019500070213     C                   exsr      defcam
019600070213     C                   exsr      impvab                                       => carico  VAB
019700070213     C                   else
019800070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
019900070213     C                   exsr      wrivab                                       => scarico VAB
020000070213     C                   exsr      inzvar
020100070213     C                   exsr      defcam
020200070213     C                   exsr      impvab                                       => carico  VAB
020300070213     C                   endif
020400071121     C                   exsr      exevate                                      => write VAT-E
020500070103     C* Salvo il raggruppamento spedizione cliente corrente
020600070103     C                   eval      depSped = curSped
020700070213     C*
020800070213     C* Se collo successivo x stessa bolla
020900070213     C                   else
021000070213     C                   exsr      impvab                                       => carico  VAB
021100070213     C                   exsr      exevate                                      => write VAT-E
021200070103     C                   endif
021300000905     C*
021400000905     C                   else
021500000905     C                   eval      vinflg = '1'
021600050628     C                   endif
021700000905     C                   endif
021800000905     C*
021900000905     C  N70              update    tivin000
022000000905     C*
022100991022     C  N70              ENDdo
022200070213     C*
022300070213     C* Scarico testata bolla rimasta in sospesa
022400070213     C                   exsr      wrivab                                       => scarico VAB
022500010202     C*
022600010202     C                   endif
022700990910
022800990910     C* Se non ci sono record con errori ...
022900000710     C                   if        §ctrno = 0
023000990910     C* ... restituisco esito OK.
023100990921     C                   eval      wrkesito = '0'
023200990910     C                   else
023300010201     C                   if        §ctrokvb > 0
023400990921     C                   eval      wrkesito = '1'
023500000710     C                   else
023600000710     C                   eval      wrkesito = '2'
023700990910     C                   endif
023800000710     C                   endif
023900990910     C*
024000990914     C                   if        %open(tivin00r)
024100990908     C                   close     tivin00r
024200990914     C                   endif
024300021113     C                   if        %open(fivabwwr)
024400021113     C                   close     fivabwwr
024500990914     C                   endif
024600021113     C                   if        %open(fivatwwr)
024700021113     C                   close     fivatwwr
024800010201     C                   endif
024900990910     C*
025000010201     C                   if        §ctrokvb > 0
025100000724     C                             and vlrpoi <> *zeros
025200010202     C                   exsr      invio
025300990920     C                   endif
025400990920     C*
025500910830     C                   ENDSR
025600000613     C***
025700010305
025800010305     C*----------------------------------------------------*
025900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026000010305     C*----------------------------------------------------*
026100020305     C     WRIVAB        BEGSR
026200010305     C*
026300060225     C* Quindi scarico il buffer del file d testata
026400021113     C                   write     fivab000                                     => scarico il VAB
026500010305     C*
026600010305     C                   ENDSR
026700990920
026800000801     C*----------------------------------------------------*
026900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027000000801     C*----------------------------------------------------*
027100010201     C     INZVAR        BEGSR
027200000801     C*
027300070213     C                   CLEAR                   FIVAB000
027400070213     C                   CLEAR                   FIVAT000
027500070213     C*
027600040802     C                   Z-ADD     *zeros        Num5_0            5 0
027700040802     C                   MOVEL     '0'           FlgCAS            1
027800000801     C*
027900000801     C                   ENDSR
028000000801     C*----------------------------------------------------*
028100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028200000801     C*----------------------------------------------------*
028300000801     C     DEFCAM        BEGSR
028400000801     C*
028500020619     C* Imposto i valori di default...
028600090312     C                   Z-ADD     0593793       VABCCM
028700090312     C                   Z-ADD     0593793       VATCCM
028800090312     C                   Z-ADD     059           VABLNP
028900090312     C                   Z-ADD     059           VATLNP
029000090113     C                   Z-ADD     000           VABCTR
029100040823     C                   MOVEL     '1'           VABCBO
029200020619     C* ... e poi verifico se sono stati passati come parametri
029300020619     C                   IF        vlrppt > *blanks
029400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029600020619     C                   EXSR      CHKNUM
029700020619     C                   IF        PiInt=*on
029800020619     C                   Z-ADD     PiVal         VABCCM
029900020619     C                   Z-ADD     PiVal         VATCCM
030000020619     C                   ENDIF
030100040506     C                   ENDIF
030200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030400020619     C                   EXSR      CHKNUM
030500020619     C                   IF        PiInt=*on
030600020619     C                   Z-ADD     PiVal         VABLNP
030700020619     C                   Z-ADD     PiVal         VATLNP
030800040506     C                   ENDIF
030900020619     C                   ENDIF
031000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031200020619     C                   EXSR      CHKNUM
031300020619     C                   IF        PiInt=*on
031400020619     C                   Z-ADD     PiVal         VABCTR
031500040506     C                   ENDIF
031600020619     C                   ENDIF
031700020619     C                   ENDIF
031800000801     C*
031900000801     C                   ENDSR
032000000801     C*----------------------------------------------------*
032100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032200000801     C*----------------------------------------------------*
032300040823     C     IMPVAB        BEGSR
032400010305     C*
032500000801     C                   Z-ADD     *zeros        errore            1 0
032600000830     C                   MOVEL     datcor        VABAAS
032700020305     C                   MOVEL     datcor        VATAAS
032800040526     C                   MOVE      datcor        VABMGS
032900040823     C                   MOVE(P)   vlrpoi        VABFGS
033000040823     C                   MOVE(P)   vlrpoi        VATFGS
033100080409     C*
033200080409     C* Reperimento campi ALFA
033300080409     C*
033400080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
033500090312     C                   EVAL      VABRSD=%trim(%subst(vindta:34:30))
033600080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033700080409     C     '@':'A'       XLATE     VABRSD        VABRSD
033800080409     C* ==
033900090312     C                   EVAL      VABIND=%trim(%subst(vindta:64:30))
034000090312     C                   EVAL      VABLOD=%trim(%subst(vindta:94:30))
034100090312     C                   EVAL      VABPRD=%trim(%subst(vindta:124:2))
034200090312     C                   EVAL      VABRMA=%trim(%subst(vindta:16:10))
034300090312     C                   EVAL      VABNOT=%trim(%subst(vindta:188:40))
034400090316     C                   IF        %subst(vindta:167:1)<>'0'
034500090316     C                   EVAL      VABCBO=%subst(vindta:167:1)
034600090316     C                   ENDIF
034700090312     C                   IF        %subst(vindta:159:3)='000'
034800090312     C                   EVAL      VABTIC=*blanks
034900090312     C                   ENDIF
035000090312     C                   IF        %subst(vindta:159:3)='001'
035100090312     C                   EVAL      VABTIC='BM'
035200090312     C                   ENDIF
035300080409     C*
035400080409     C* Reperimento campi NUMERICI
035500090113     C*
035600080409     C* NSP/RMN
035700090312     C                   EVAL      PiStr=%trim(%subst(vindta:16+3:10-3))
035800080409     C                   EXSR      CHKNUM
035900080409     C                   IF        PiInt=*on
036000080409     C                   Z-ADD     PiVal         VABNSP
036100080409     C                   Z-ADD     PiVal         VATNSP
036200080409     C                   Z-ADD     PiVal         VABRMN
036300080409     C                   ELSE
036400080409     C                   ADD       1             errore
036500080409     C                   Z-ADD     *zeros        VABNSP
036600080409     C                   Z-ADD     1             VABRMN
036700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
036800080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
036900080409     C                   ENDIF
037000080409     C* CAD
037100090312     C                   EVAL      PiStr=%trim(%subst(vindta:126:5))
037200080409     C                   EXSR      CHKNUM
037300080409     C                   IF        PiInt=*on
037400080409     C                   Z-ADD     PiVal         Num5_0
037500080409     C                   MOVEL(p)  Num5_0        VABCAD
037600080409     C                   ELSE
037700080409     C                   ADD       1             errore
037800080409     C                   EVAL      VABCAD = *zeros
037900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
038000080409     C                             + ' ' + 'VABCAD'
038100080409     C                   ENDIF
038200080409     C* NCL
038300090312     C                   EVAL      PiStr=%trim(%subst(vindta:138:5))
038400080409     C                   EXSR      CHKNUM
038500080409     C                   IF        PiInt=*on
038600080409     C                   Z-ADD     PiVal         VABNCL
038700080409     C                   ELSE
038800080409     C                   ADD       1             errore
038900080409     C                   Z-ADD     *zeros        VABNCL
039000080409     C                   EVAL      vinmsg = %trimr(vinmsg)
039100080409     C                             + ' ' + 'VABNCL'
039200080409     C                   ENDIF
039300080409     C* PKB
039400090312     C                   EVAL      PiStr=%trim(%subst(vindta:131:7))
039500080409     C                   EXSR      CHKNUM
039600080409     C                   IF        PiNum=*on
039700090312     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
039800090113     C                   Z-ADD     PiVal         VABPKB
039900080409     C                   ELSE
040000080409     C                   ADD       1             errore
040100080409     C                   Z-ADD     *zeros        VABPKB
040200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
040300080409     C                             + ' ' + 'VABPKB'
040400080409     C                   ENDIF
040500090312     C* VLB
040600090312     C                   EVAL      PiStr=%trim(%subst(vindta:143:7))
040700090312     C                   EXSR      CHKNUM
040800090312     C                   IF        PiNum=*on
040900090312     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
041000090312     C                   Z-ADD     PiVal         VABVLB
041100090312     C                   ELSE
041200090312     C                   ADD       1             errore
041300090312     C                   Z-ADD     *zeros        VABVLB
041400090312     C                   EVAL      vinmsg = %trimr(vinmsg)
041500090312     C                             + ' ' + 'VABVLB'
041600090312     C                   ENDIF
041700090312     C* CAS
041800090312     C                   IF        %trim(%subst(vindta:150:9)) <> *blanks AND
041900090312     C                             %trim(%subst(vindta:150:9)) <> *all'0'
042000090316     C                   EVAL      FlgCAS = '1'
042100090312     C                   EVAL      VABVCA = 'EUR'
042200090312     C                   EVAL      PiStr=%trim(%subst(vindta:150:9))
042300080409     C                   EXSR      CHKNUM
042400080409     C                   IF        PiNum=*on
042500090213     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
042600090312     C                   Z-ADD     PiVal         VABCAS
042700080409     C                   ELSE
042800080409     C                   ADD       1             errore
042900090312     C                   Z-ADD     *zeros        VABCAS
043000080409     C                   EVAL      vinmsg = %trimr(vinmsg)
043100090312     C                             + ' ' + 'VABCAS'
043200080409     C                   ENDIF
043300080409     C                   ENDIF
043400071121     C*
043500080415     C* Carico l'estensioni A e B del FNVAT
043600080415     C                   exsr      exevata
043700071121     C                   exsr      exevatb
043800010205     C*
043900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
044000040802     C                   IF        FlgCAS <> '0'
044100010205     C                   EVAL      VABCBO = '4'
044200010205     C                   ENDIF
044300020305     C*
044400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044500011113     C                   EXSR      CHKIMPDIV
044600010202     C*
044700000801     C* Ebbene...
044800000801     C                   ADD       1             §CTRMO
044900010201     C                   IF        errore <> *zeros
045000000801     C                   ADD       1             §CTRNO
045100000801     C                   EVAL      recko = vindta
045200000801     C                   EXCEPT    rigadett
045300000801     C                   EVAL      vinflg = '2'
045400000801     C                   ELSE
045500010201     C                   ADD       1             §CTROKVB
045600000801     C                   ENDIF
045700000801     C*
045800000801     C                   ENDSR
045900070102     C*----------------------------------------------------*
046000070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
046100070102     C*----------------------------------------------------*
046200070103     C     EXEVATE       BEGSR
046300070102     C*
046400090312     C***                EVAL      VATTRC='E'
046500090312     C*
046600090312     C***                exsr      wrivat                                       => scarico VAT
046700070102     C*
046800070102     C                   ENDSR
046900080415     C*----------------------------------------------------*
047000080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
047100080415     C*----------------------------------------------------*
047200080415     C     EXEVATA       BEGSR
047300080415     C*
047400090312     C***                EVAL      VATTRC='A'
047500080415     C*
047600080415     C***                exsr      wrivat                                       => scarico VAT
047700080415     C*
047800080415     C                   ENDSR
047900071121     C*----------------------------------------------------*
048000071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
048100071121     C*----------------------------------------------------*
048200071121     C     EXEVATB       BEGSR
048300071121     C*
048400090312     C***                EVAL      VATTRC='B'
048500071121     C*
048600090312     C***                exsr      wrivat                                       => scarico VAT
048700071121     C*
048800071121     C                   ENDSR
048900010201     C*----------------------------------------------------*
049000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
049100010201     C*----------------------------------------------------*
049200020305     C     WRIVAT        BEGSR
049300050628     C*
049400060223     C* Scrivo solo se valorizzato qualcosa
049500060223     C                   IF        VATNOT <> *blanks
049600040802     C                   WRITE     FIVAT000
049700060223     C                   ENDIF
049800010201     C*
049900010201     C                   ENDSR
050000010202     C*----------------------------------------------------*
050100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050200010202     C*----------------------------------------------------*
050300020305     C     PREVAT        BEGSR
050400010202     C*
050500021113     C* Compongo il nome del membro da dare al FIVATWWR
050600010202     C                   eval      parmbr = vlrhdl
050700010202     C                   movel     'M'           parmbr
050800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050900010202     C                   eval      paropz = '1'
051000010202     C* Effettuo la chiamata al CLLE preposto
051100040506     C                   call(e)   'TITVVTC'
051200010202     C                   parm                    parccm
051300010202     C                   parm                    parmbr
051400010202     C                   parm                    paropz
051500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051600010202     C                   if        %error
051700010202     C                   movel     '1'           chkcall
051800010202     C                   else
051900010202     C                   movel     '0'           chkcall
052000010202     C                   endif
052100010202     C*
052200010202     C                   ENDSR
052300000801     C*----------------------------------------------------*
052400000801     C*  CONTROLLO NUMERICITA' CAMPI
052500000801     C*----------------------------------------------------*
052600000801     C     CHKNUM        BEGSR
052700090113     C*
052800090113     C                   IF        PiDecChr = *blanks
052900090113     C                   EVAL      PiDecChr = ','
053000090113     C                   ENDIF
053100090113     C*
053200090113     C                   callp(e)  UBISNUM_Check(PiStr
053300090113     C                                          :PiDecChr
053400090113     C                                          :PiVal
053500090113     C                                          :PiNum
053600090113     C                                          :PiInt)
053700090113     C*
053800090113     C                   IF        %error
053900090113     C                   EVAL      PiInt=*off
054000090113     C                   ENDIF
054100000801     C*
054200000801     C                   ENDSR
054300000801     C***
054400000801
054500011113
054600011113     C*----------------------------------------------------*
054700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054800011113     C*----------------------------------------------------*
054900011113     C     CHKIMPDIV     BEGSR
055000011113     C*
055100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055200011113     C                   Z-ADD     *zeros        wrkDec            9 9
055300011113     C*
055400011113     C* Come prima cosa effettuo considerazioni sulla divisa
055500011113     C                   IF        vabIAS > *zeros
055600011113     C                   IF        vabVAS <> 'EUR'
055700011113     C                   EVAL      vabVAS =  'ITL'
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C                   IF        vabCAS > *zeros
056200011113     C                   IF        vabVCA <> 'EUR'
056300011113     C                   EVAL      vabVCA =  'ITL'
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C                   IF        vabVMD > *zeros
056800020305     C                   IF        vabVAD <> 'EUR'
056900011113     C                   EVAL      vabVAD =  'ITL'
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057400011113     C                   Z-ADD     vabIAS        wrkDec
057500011113     C                   IF        wrkDec > *zeros
057600011113     C                   IF        vabVAS = 'ITL'
057700011113     C                   EVAL      vabIAS = *zeros
057800011113     C                   ENDIF
057900011113     C                   ENDIF
058000011113     C*
058100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058200011113     C                   Z-ADD     vabCAS        wrkDec
058300011113     C                   IF        wrkDec > *zeros
058400011113     C                   IF        vabVCA = 'ITL'
058500011113     C                   EVAL      vabCAS = *zeros
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059000011113     C                   Z-ADD     vabVMD        wrkDec
059100011113     C                   IF        wrkDec > *zeros
059200011113     C                   IF        vabVAD = 'ITL'
059300011113     C                   EVAL      vabVMD = *zeros
059400011113     C                   ENDIF
059500011113     C                   ENDIF
059600011113     C*
059700011113     C                   ENDSR
059800011113     C***
059900011113
060000011113
060100000801
060200000801
060300990920      /TITLE Invio dei dati al punto operativo.
060400010202     C     invio         BEGSR
060500990920     C*
060600021113     C* 1° invio FIVAT
060700010201     C                   reset                   dscmz
060800010201     C                   move      vlrpoi        cmzdst
060900021113     C                   eval      cmzfld = 'FIVATWWR'
061000010201     C                   eval      cmzmbd = vlrhdl
061100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061200021009     C***                if        prmfir = *blanks
061300021113     C                   eval      cmzfla = 'FIVAT00F'
061400021113     C                   eval      cmzmba = 'FIVAT00F'
061500021009     C***                else
061600021009     C***                eval      cmzfla = prmfir
061700021009     C***                eval      cmzmba = prmfir
061800021009     C***                endif
061900010201     C                   eval      cmznrr = *zeros
062000020305     C                   move      §ctrokvt      cmznrr
062100021018     C                   eval      cmzlba = vlrfl1
062200010201     C                   call(e)   'TIS711C'
062300010201     C                   parm                    dscmz
062400010201     C                   parm      *blanks       esito
062500010205     C                   if        %error
062600010205     C                             or cmzerr = '1'
062700010205     C                             or esito  = '1'
062800010205     C                   eval      wrkesito = '3'
062900010205     C                   else
063000010201     C*
063100021113     C* 2° invio FIVAB
063200010201     C                   reset                   dscmz
063300010201     C                   move      vlrpoi        cmzdst
063400010201     C                   eval      cmzfld = vlrfou
063500010201     C                   eval      cmzmbd = vlrhdl
063600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063700021009     C***                if        prmfir = *blanks
063800021113     C                   eval      cmzfla = 'FIVAB00F'
063900021113     C                   eval      cmzmba = 'FIVAB00F'
064000021009     C***                else
064100021009     C***                eval      cmzfla = prmfir
064200021009     C***                eval      cmzmba = prmfir
064300021009     C***                endif
064400010201     C                   eval      cmznrr = *zeros
064500010201     C                   move      §ctrokvb      cmznrr
064600021018     C                   eval      cmzlba = vlrfl1
064700010201     C                   call(e)   'TIS711C'
064800010201     C                   parm                    dscmz
064900010201     C                   parm      *blanks       esito
065000010201     C                   if        %error
065100010201     C                             or cmzerr = '1'
065200010201     C                             or esito  = '1'
065300010201     C                   eval      wrkesito = '3'
065400010201     C                   endif
065500010205     C                   endif
065600990920     C*
065700000613     C                   ENDSR
065800000613     C***
065900070411
066000090312
066100070411     C     *pssr         BEGSR
066200070411     C*
066300070411     C                   if        %open(tivin00r)
066400070411     C                   close     tivin00r
066500070411     C                   endif
066600070411     C                   if        %open(fivabwwr)
066700070411     C                   close     fivabwwr
066800070411     C                   endif
066900070411     C                   if        %open(fivatwwr)
067000070411     C                   close     fivatwwr
067100070411     C                   endif
067200070411     C*
067300070411     C* Effettuo la chiamata al CLLE preposto
067400070411     C                   call(e)   'TITVVTC'
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
068600990910
068700000613     C     *inzsr        BEGSR
068800990910     C*
068900990910     C     *entry        plist
069000990920     C                   parm                    tivlrds
069100990921     C                   parm      wrkesito      esito
069200000724     C                   parm                    prmlit
069300000710     C                   parm                    prmfir
069400000613     C*
069500000830     C* CALCOLA LA DATA CORRENTE
069600000830     C                   time                    wn14             14 0
069700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069800000830     C                   z-add     wn8           g08dat
069900000830     C                   z-add     *zeros        g08inv
070000000830     C                   movel     '0'           g08err
070100000830     C                   call      'XSRDA8'
070200000830     C                   parm                    wlbda8
070300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
070400000830     C*
070500000613     C                   ENDSR
070600000613     C***
070700990908
070800090312     Otitv1n9p  E            riepilogo         2
070900990915     O                                              'Upload via Internet'
071000990915     O                                           +1 'Traduzione TIVIN00R -'
071100021113     O                                           +1 'FIVABWWR/FIVATWWR'
071200010201     O                                           +1 'Report testate bolle'
071300990915     O          E            riepilogo   2
071400990915     O                       wrkdata
071500990915     O                       wrkora              +1
071600990915     O                       procname            +1
071700990915     O          E            riepilogo   2
071800990915     O                                              'Cliente..................:'
071900990915     O                       VABCCM        z     +1
072000990915     O          E            riepilogo   2
072100990920     O                                              'Riferimento Strategi.....:'
072200990920     O                       vlrhdl              +1
072300990915     O          E            riepilogo   2
072400990915     O                                              'Giusti...................:'
072500010201     O                       §CTROKVB      2   +  1
072600990915     O          E            riepilogo   2
072700990915     O                                              'Sbagliati e corretti.....:'
072800971022     O                       §CTRMO        2   +  1
072900990915     O          E            riepilogo   2
073000990915     O                                              'Sbagliati e scartati.....:'
073100971022     O                       §CTRNO        2   +  1
073200000613
073300090312     Otitv1n9ps E            testdett          2
073400000613     O                                              'Upload via Internet'
073500000613     O                                           +1 'Traduzione TIVIN00R -'
073600021113     O                                           +1 'FIVABWWR/FIVATWWR'
073700010201     O                                           +1 'Report testate bolle'
073800000616     O          E            testdett    3
073900000613     O                                           +2 'N° rec'
074000000613     O                                           +3 'Anteprima contenuto'
074100000616     O          E            rigadett    2
074200000613     O                       rrnum               +2
074300000621     O                       recko               +3
074400000616     O          E            findett     2
074500000613     O                       wrkdata
074600000613     O                       wrkora              +1
074700000613     O                       procname            +1
074800000616     O          E            findett     2
074900000613     O                                              'Cliente..................:'
075000000613     O                       VABCCM        z     +1
075100000616     O          E            findett     2
075200000613     O                                              'Riferimento Strategi.....:'
075300000613     O                       vlrhdl              +1
075400000616     O          E            findett     2
075500000613     O                                              'Giusti...................:'
075600010201     O                       §CTROKVB      2   +  1
075700000616     O          E            findett     2
075800000613     O                                              'Sbagliati e corretti.....:'
075900000613     O                       §CTRMO        2   +  1
076000000616     O          E            findett     2
076100000613     O                                              'Sbagliati e scartati.....:'
076200000613     O                       §CTRNO        2   +  1
076300000512** CMD - COMANDI CL
076400090312OVRPRTF FILE(TITV1N9P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076500090312OVRPRTF FILE(TITV1N9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076600090312DLTOVR FILE(TITV1N9P TITV1N9PS) LVL(*)
076700000512
076800000512
