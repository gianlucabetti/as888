000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200090113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800101029     Ftitv2a9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100101029     Ftitv2a9ps O    f  198        PRINTER usropn
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
004000101029     D curSped         s              5    INZ(*blanks)
004100101029     D depSped         s              5    INZ(*blanks)
004200000830
004300090113
004400090113     D*------------------
004500090113     D* LINKING A DEFINIZIONI ESTERNE
004600090113     D*------------------
004700090113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004800090113     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004900090113
005000010201
005100990915     C                   time                    wrkdata
005200990915     C                   time                    wrkora
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600000724     C*
005700000724     C* SE OCCORRE SPEDIRE IN FILIALE
005800000724     C                   if        vlrpoi <> *zeros
005900000724     C*
006000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006200000724     C     vlrpoi        chain     azorg01l
006300000724     C                   if        %found
006400000616     C                   movel(p)  CMD(1)        depcmd
006500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006600000616     C*
006700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006800011113     C                   Z-ADD     150           LENGH            15 5
006900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007000000616     C                   PARM                    depcmd
007100000616     C                   PARM                    LENGH
007200000724     C*
007300000724     C                   endif
007400000724     C                   endif
007500000616     C*
007600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700000616     C                   movel(p)  CMD(2)        depcmd
007800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
007900000616     C*
008000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008100011113     C                   Z-ADD     150           LENGH            15 5
008200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000616     C                   PARM                    depcmd
008400000616     C                   PARM                    LENGH
008500000616     C*
008600101029     C                   if        not %open(titv2a9ps)
008700101029     C                   open      titv2a9ps
008800000616     C                   except    testdett
008900000613     C                   endif
009000000613     C*
009100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009300000613     C*
009400010202     C* Effettuo la chiamata al CLLE preposto
009500040506     C                   call(e)   'TITVVTC'
009600010202     C                   parm                    parccm
009700010202     C                   parm                    parmbr
009800010202     C                   parm      '2'           paropz
009900050201     C*
010000050201     C* Effettuo lancio TISI95 solo x chiusura
010100050201     C                   CLEAR                   TISI95DS
010200050201     C                   EVAL      I95TLA = 'C'
010300050201     C                   CALL      'TISI95R'
010400050201     C                   PARM                    TISI95DS
010500010202     C*
010600101029     C                   if        %open(titv2a9ps)
010700000616     C                   except    findett
010800101029     C                   close     titv2a9ps
010900000613     C                   endif
011000000616     C*
011100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011200011113     C                   Z-ADD     150           LENGH            15 5
011300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011400000616     C                   PARM                    CMD(3)
011500000616     C                   PARM                    LENGH
011600000616     C*
011700000801     C
011800010201     C                   seton                                        LR
011900000613
012000000613
012100000613     C*--------------------------------------------------------
012200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012300000613     C*--------------------------------------------------------
012400000621     C     STPR          BEGSR
012500000613     C*
012600101029     C                   if        not %open(titv2a9p)
012700101029     C                   open      titv2a9p
012800990915     C                   endif
012900990915     C*
013000990915     C                   except    riepilogo
013100990915     C*
013200101029     C                   if        %open(titv2a9p)
013300101029     C                   close     titv2a9p
013400990914     C                   endif
013500990910     C*
013600000613     C                   ENDSR
013700000613     C***
013800990908
013900000801
014000910830     C*--------------------------------------------------------
014100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014200910830     C*--------------------------------------------------------
014300040526     C     RWFILE        BEGSR
014400990910     C*
014500990914     C                   if        not %open(tivin00r)
014600990908     C                   open      tivin00r
014700990914     C                   endif
014800021113     C                   if        not %open(fivabwwr)
014900021113     C                   open      fivabwwr
015000990914     C                   endif
015100070103     C*
015200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015300020305     C                   exsr      prevat
015400010201     C*
015500010202     C                   if        chkcall = '0'
015600010202     C*
015700021113     C                   if        not %open(fivatwwr)
015800021113     C                   open      fivatwwr
015900010201     C                   endif
016000990910     C*
016100010201     C                   clear                   §CTROKVB          5 0
016200020305     C                   clear                   §CTROKVT          5 0
016300000801     C                   clear                   §CTRMO            5 0
016400000801     C                   clear                   §CTRNO            5 0
016500990910     C*
016600921023     C                   DO        *HIVAL
016700990913     C*
016800990915     C                   READ      tivin00r                               70
016900050627     C                   if        vindta > *blanks
017000000613     C                   add       1             rrnum
017100000801     C*
017200000801     C                   if        *in70 = *off
017300000801     C                             and
017400000801     C                             (vinflg = *blanks
017500000801     C                              or vinflg = '0'
017600000801     C                              or vinflg = '2')
017700000801     C*
017800000801     C                   clear                   vinmsg
017900000801     C                   eval      vinflg = '1'
018000070103     C*
018100070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
018200101029     C                   eval      curSped = %subst(vindta:2:5)
018300070213     C                   if        curSped <> depSped
018400070213     C* Se prima bolla => importo bolla corrente
018500070213     C                   if        §CTRMO = *zeros
018600070213     C                   exsr      inzvar
018700070213     C                   exsr      defcam
018800070213     C                   exsr      impvab                                       => carico  VAB
018900070213     C                   else
019000070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
019100070213     C                   exsr      wrivab                                       => scarico VAB
019200070213     C                   exsr      inzvar
019300070213     C                   exsr      defcam
019400070213     C                   exsr      impvab                                       => carico  VAB
019500070213     C                   endif
019600071121     C                   exsr      exevate                                      => write VAT-E
019700070103     C* Salvo il raggruppamento spedizione cliente corrente
019800070103     C                   eval      depSped = curSped
019900070213     C*
020000070213     C* Se collo successivo x stessa bolla
020100070213     C                   else
020200070213     C                   exsr      impvab                                       => carico  VAB
020300070213     C                   exsr      exevate                                      => write VAT-E
020400070103     C                   endif
020500000905     C*
020600000905     C                   else
020700000905     C                   eval      vinflg = '1'
020800050628     C                   endif
020900000905     C                   endif
021000000905     C*
021100000905     C  N70              update    tivin000
021200000905     C*
021300991022     C  N70              ENDdo
021400070213     C*
021500070213     C* Scarico testata bolla rimasta in sospesa
021600070213     C                   exsr      wrivab                                       => scarico VAB
021700010202     C*
021800010202     C                   endif
021900990910
022000990910     C* Se non ci sono record con errori ...
022100000710     C                   if        §ctrno = 0
022200990910     C* ... restituisco esito OK.
022300990921     C                   eval      wrkesito = '0'
022400990910     C                   else
022500010201     C                   if        §ctrokvb > 0
022600990921     C                   eval      wrkesito = '1'
022700000710     C                   else
022800000710     C                   eval      wrkesito = '2'
022900990910     C                   endif
023000000710     C                   endif
023100990910     C*
023200990914     C                   if        %open(tivin00r)
023300990908     C                   close     tivin00r
023400990914     C                   endif
023500021113     C                   if        %open(fivabwwr)
023600021113     C                   close     fivabwwr
023700990914     C                   endif
023800021113     C                   if        %open(fivatwwr)
023900021113     C                   close     fivatwwr
024000010201     C                   endif
024100990910     C*
024200010201     C                   if        §ctrokvb > 0
024300000724     C                             and vlrpoi <> *zeros
024400010202     C                   exsr      invio
024500990920     C                   endif
024600990920     C*
024700910830     C                   ENDSR
024800000613     C***
024900010305
025000010305     C*----------------------------------------------------*
025100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025200010305     C*----------------------------------------------------*
025300020305     C     WRIVAB        BEGSR
025400010305     C*
025500060225     C* Quindi scarico il buffer del file d testata
025600021113     C                   write     fivab000                                     => scarico il VAB
025700010305     C*
025800010305     C                   ENDSR
025900990920
026000000801     C*----------------------------------------------------*
026100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026200000801     C*----------------------------------------------------*
026300010201     C     INZVAR        BEGSR
026400000801     C*
026500070213     C                   CLEAR                   FIVAB000
026600070213     C                   CLEAR                   FIVAT000
026700070213     C*
026800040802     C                   Z-ADD     *zeros        Num5_0            5 0
026900040802     C                   MOVEL     '0'           FlgCAS            1
027000000801     C*
027100000801     C                   ENDSR
027200000801     C*----------------------------------------------------*
027300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027400000801     C*----------------------------------------------------*
027500000801     C     DEFCAM        BEGSR
027600000801     C*
027700020619     C* Imposto i valori di default...
027800101029     C                   Z-ADD     0831256       VABCCM
027900101029     C                   Z-ADD     0831256       VATCCM
028000101029     C                   Z-ADD     083           VABLNP
028100101029     C                   Z-ADD     083           VATLNP
028200090113     C                   Z-ADD     000           VABCTR
028300070103     C                   MOVEL     '7Q'          VABCTM
028400101029     C                   MOVEL     'C'           VABTSP
028500040823     C                   MOVEL     '1'           VABCBO
028600020619     C* ... e poi verifico se sono stati passati come parametri
028700020619     C                   IF        vlrppt > *blanks
028800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029000020619     C                   EXSR      CHKNUM
029100020619     C                   IF        PiInt=*on
029200020619     C                   Z-ADD     PiVal         VABCCM
029300020619     C                   Z-ADD     PiVal         VATCCM
029400020619     C                   ENDIF
029500040506     C                   ENDIF
029600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029800020619     C                   EXSR      CHKNUM
029900020619     C                   IF        PiInt=*on
030000020619     C                   Z-ADD     PiVal         VABLNP
030100020619     C                   Z-ADD     PiVal         VATLNP
030200040506     C                   ENDIF
030300020619     C                   ENDIF
030400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
030500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030600020619     C                   EXSR      CHKNUM
030700020619     C                   IF        PiInt=*on
030800020619     C                   Z-ADD     PiVal         VABCTR
030900040506     C                   ENDIF
031000020619     C                   ENDIF
031100020619     C                   ENDIF
031200000801     C*
031300000801     C                   ENDSR
031400000801     C*----------------------------------------------------*
031500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031600000801     C*----------------------------------------------------*
031700040823     C     IMPVAB        BEGSR
031800010305     C*
031900000801     C                   Z-ADD     *zeros        errore            1 0
032000000830     C                   MOVEL     datcor        VABAAS
032100020305     C                   MOVEL     datcor        VATAAS
032200040526     C                   MOVE      datcor        VABMGS
032300040823     C                   MOVE(P)   vlrpoi        VABFGS
032400040823     C                   MOVE(P)   vlrpoi        VATFGS
032500080409     C*
032600080409     C* Reperimento campi ALFA
032700080409     C*
032800080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
032900101029     C                   EVAL      VABRSD=%trim(%subst(vindta:10:35))
033000080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033100080409     C     '@':'A'       XLATE     VABRSD        VABRSD
033200080409     C* ==
033300101029     C                   EVAL      VABRD2=%trim(%subst(vindta:10+35:35))
033400101029     C                   EVAL      VABIND=%trim(%subst(vindta:80:35))
033500101029     C                   EVAL      VABLOD=%trim(%subst(vindta:136:30))
033600101029     C                   EVAL      VABPRD=%trim(%subst(vindta:166:2))
033700101029     C                   EVAL      VABRMA=%trim(%subst(vindta:2:5))
033800101029     C                   EVAL      VABNOT=%trim(%subst(vindta:187:35))
033900101029     C                   EVAL      VABNT2=%trim(%subst(vindta:187+35:35))
034000101029     C                   IF        %subst(vindta:176:1) = 'A'
034100101029     C                   EVAL      VABCBO='2'
034200090213     C                   ELSE
034300090213     C                   EVAL      VABCBO='1'
034400090213     C                   ENDIF
034500101029     C*
034600101029     C* CCM
034700101029     C                   SELECT
034800101029     C                   WHEN      %subst(vindta:1:1) = 'H'
034900101029     C                   EVAL      VABCCM = 0831256
035000101029     C                   EVAL      VATCCM = 0831256
035100101029     C                   WHEN      %subst(vindta:1:1) = 'U'
035200101029     C                   EVAL      VABCCM = 0831257
035300101029     C                   EVAL      VATCCM = 0831257
035400101029     C                   WHEN      %subst(vindta:1:1) = 'T'
035500101029     C                   EVAL      VABCCM = 0831258
035600101029     C                   EVAL      VATCCM = 0831258
035700101029     C                   ENDSL
035800080409     C*
035900080409     C* Reperimento campi NUMERICI
036000090113     C*
036100080409     C* NSP/RMN
036200101029     C                   EVAL      PiStr=%trim(%subst(vindta:2:5))
036300080409     C                   EXSR      CHKNUM
036400080409     C                   IF        PiInt=*on
036500080409     C                   Z-ADD     PiVal         VABNSP
036600080409     C                   Z-ADD     PiVal         VATNSP
036700080409     C                   Z-ADD     PiVal         VABRMN
036800080409     C                   ELSE
036900080409     C                   ADD       1             errore
037000080409     C                   Z-ADD     *zeros        VABNSP
037100080409     C                   Z-ADD     1             VABRMN
037200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
037300080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
037400080409     C                   ENDIF
037500080409     C* CAD
037600101029     C                   EVAL      PiStr=%trim(%subst(vindta:130:5))
037700080409     C                   EXSR      CHKNUM
037800080409     C                   IF        PiInt=*on
037900080409     C                   Z-ADD     PiVal         Num5_0
038000080409     C                   MOVEL(p)  Num5_0        VABCAD
038100080409     C                   ELSE
038200080409     C                   ADD       1             errore
038300080409     C                   EVAL      VABCAD = *zeros
038400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
038500080409     C                             + ' ' + 'VABCAD'
038600080409     C                   ENDIF
038700080409     C* NCL
038800101029     C                   EVAL      PiStr=%trim(%subst(vindta:169:2))
038900080409     C                   EXSR      CHKNUM
039000080409     C                   IF        PiInt=*on
039100080409     C                   Z-ADD     PiVal         VABNCL
039200080409     C                   ELSE
039300080409     C                   ADD       1             errore
039400080409     C                   Z-ADD     *zeros        VABNCL
039500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
039600080409     C                             + ' ' + 'VABNCL'
039700080409     C                   ENDIF
039800080409     C* PKB
039900101029     C                   EVAL      PiStr=%trim(%subst(vindta:172:4))
040000080409     C                   EXSR      CHKNUM
040100080409     C                   IF        PiNum=*on
040200090113     C                   Z-ADD     PiVal         VABPKB
040300080409     C                   ELSE
040400080409     C                   ADD       1             errore
040500080409     C                   Z-ADD     *zeros        VABPKB
040600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
040700080409     C                             + ' ' + 'VABPKB'
040800080409     C                   ENDIF
040900101029     C* CAS
041000101029     C                   IF        %trim(%subst(vindta:179:8)) <> *blanks AND
041100101029     C                             %trim(%subst(vindta:179:8)) <> *zeros  AND
041200101029     C                             %trim(%subst(vindta:179:8)) <> '0,00'
041300101029     C                   MOVEL     '1'           FlgCAS
041400101104     C                   EVAL      VABTIC = 'BM'
041500101029     C                   EVAL      VABVCA = 'EUR'
041600101029     C                   EVAL      PiStr=%trim(%subst(vindta:179:8))
041700080409     C                   EXSR      CHKNUM
041800080409     C                   IF        PiNum=*on
041900101029     C                   Z-ADD     PiVal         VABCAS
042000080409     C                   ELSE
042100080409     C                   ADD       1             errore
042200101029     C                   Z-ADD     *zeros        VABCAS
042300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
042400101029     C                             + ' ' + 'VABCAS'
042500080409     C                   ENDIF
042600080409     C                   ENDIF
042700071121     C*
042800080415     C* Carico l'estensioni A e B del FNVAT
042900080415     C                   exsr      exevata
043000071121     C                   exsr      exevatb
043100010205     C*
043200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043300040802     C                   IF        FlgCAS <> '0'
043400010205     C                   EVAL      VABCBO = '4'
043500010205     C                   ENDIF
043600020305     C*
043700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043800011113     C                   EXSR      CHKIMPDIV
043900010202     C*
044000000801     C* Ebbene...
044100000801     C                   ADD       1             §CTRMO
044200010201     C                   IF        errore <> *zeros
044300000801     C                   ADD       1             §CTRNO
044400000801     C                   EVAL      recko = vindta
044500000801     C                   EXCEPT    rigadett
044600000801     C                   EVAL      vinflg = '2'
044700000801     C                   ELSE
044800010201     C                   ADD       1             §CTROKVB
044900000801     C                   ENDIF
045000000801     C*
045100000801     C                   ENDSR
045200070102     C*----------------------------------------------------*
045300070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045400070102     C*----------------------------------------------------*
045500070103     C     EXEVATE       BEGSR
045600070102     C*
045700090113     C                   Z-ADD     1             wCollo            2 0
045800101029     C                   MOVEL     *zeros        wColloAlfa1      12
045900080415     C*
046000070103     C                   EVAL      VATTRC='E'
046100101029     C                   EVAL      wColloAlfa1=%subst(vindta:1:9)+'B'
046200071019     C*
046300080415     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
046400090113     C                   dow       wCollo <= VABNCL and wCollo < 99
046500101029     C                   eval      VATNOT = %trim(wColloAlfa1) +
046600101029     C                                      %trim(%editc(wCollo:'X'))
046700070102     C                   exsr      wrivat                                       => scarico VAT
046800080415     C                   add       1             wCollo
046900080415     C                   enddo
047000070102     C*
047100070102     C                   ENDSR
047200080415     C*----------------------------------------------------*
047300080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
047400080415     C*----------------------------------------------------*
047500080415     C     EXEVATA       BEGSR
047600080415     C*
047700101029     C***                EVAL      VATTRC='A'
047800101029     C***                EVAL      VATNOT=%trim(%subst(vindta:748:22))
047900080415     C*
048000101029     C***                exsr      wrivat                                       => scarico VAT
048100080415     C*
048200080415     C                   ENDSR
048300071121     C*----------------------------------------------------*
048400071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
048500071121     C*----------------------------------------------------*
048600071121     C     EXEVATB       BEGSR
048700071121     C*
048800101029     C***                EVAL      VATTRC='B'
048900101029     C***                EVAL      VATNOT=%trim(%subst(vindta:652:16))
049000071121     C*
049100101029     C***                exsr      wrivat                                       => scarico VAT
049200071121     C*
049300071121     C                   ENDSR
049400010201     C*----------------------------------------------------*
049500040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
049600010201     C*----------------------------------------------------*
049700020305     C     WRIVAT        BEGSR
049800050628     C*
049900060223     C* Scrivo solo se valorizzato qualcosa
050000060223     C                   IF        VATNOT <> *blanks
050100040802     C                   WRITE     FIVAT000
050200060223     C                   ENDIF
050300010201     C*
050400010201     C                   ENDSR
050500010202     C*----------------------------------------------------*
050600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050700010202     C*----------------------------------------------------*
050800020305     C     PREVAT        BEGSR
050900010202     C*
051000021113     C* Compongo il nome del membro da dare al FIVATWWR
051100010202     C                   eval      parmbr = vlrhdl
051200010202     C                   movel     'M'           parmbr
051300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
051400010202     C                   eval      paropz = '1'
051500010202     C* Effettuo la chiamata al CLLE preposto
051600040506     C                   call(e)   'TITVVTC'
051700010202     C                   parm                    parccm
051800010202     C                   parm                    parmbr
051900010202     C                   parm                    paropz
052000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052100010202     C                   if        %error
052200010202     C                   movel     '1'           chkcall
052300010202     C                   else
052400010202     C                   movel     '0'           chkcall
052500010202     C                   endif
052600010202     C*
052700010202     C                   ENDSR
052800000801     C*----------------------------------------------------*
052900000801     C*  CONTROLLO NUMERICITA' CAMPI
053000000801     C*----------------------------------------------------*
053100000801     C     CHKNUM        BEGSR
053200090113     C*
053300090113     C                   IF        PiDecChr = *blanks
053400090113     C                   EVAL      PiDecChr = ','
053500090113     C                   ENDIF
053600090113     C*
053700090113     C                   callp(e)  UBISNUM_Check(PiStr
053800090113     C                                          :PiDecChr
053900090113     C                                          :PiVal
054000090113     C                                          :PiNum
054100090113     C                                          :PiInt)
054200090113     C*
054300090113     C                   IF        %error
054400090113     C                   EVAL      PiInt=*off
054500090113     C                   ENDIF
054600000801     C*
054700000801     C                   ENDSR
054800000801     C***
054900000801
055000011113
055100011113     C*----------------------------------------------------*
055200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055300011113     C*----------------------------------------------------*
055400011113     C     CHKIMPDIV     BEGSR
055500011113     C*
055600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055700011113     C                   Z-ADD     *zeros        wrkDec            9 9
055800011113     C*
055900011113     C* Come prima cosa effettuo considerazioni sulla divisa
056000011113     C                   IF        vabIAS > *zeros
056100011113     C                   IF        vabVAS <> 'EUR'
056200011113     C                   EVAL      vabVAS =  'ITL'
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C                   IF        vabCAS > *zeros
056700011113     C                   IF        vabVCA <> 'EUR'
056800011113     C                   EVAL      vabVCA =  'ITL'
056900011113     C                   ENDIF
057000011113     C                   ENDIF
057100011113     C*
057200011113     C                   IF        vabVMD > *zeros
057300020305     C                   IF        vabVAD <> 'EUR'
057400011113     C                   EVAL      vabVAD =  'ITL'
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057900011113     C                   Z-ADD     vabIAS        wrkDec
058000011113     C                   IF        wrkDec > *zeros
058100011113     C                   IF        vabVAS = 'ITL'
058200011113     C                   EVAL      vabIAS = *zeros
058300011113     C                   ENDIF
058400011113     C                   ENDIF
058500011113     C*
058600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058700011113     C                   Z-ADD     vabCAS        wrkDec
058800011113     C                   IF        wrkDec > *zeros
058900011113     C                   IF        vabVCA = 'ITL'
059000011113     C                   EVAL      vabCAS = *zeros
059100011113     C                   ENDIF
059200011113     C                   ENDIF
059300011113     C*
059400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059500011113     C                   Z-ADD     vabVMD        wrkDec
059600011113     C                   IF        wrkDec > *zeros
059700011113     C                   IF        vabVAD = 'ITL'
059800011113     C                   EVAL      vabVMD = *zeros
059900011113     C                   ENDIF
060000011113     C                   ENDIF
060100011113     C*
060200011113     C                   ENDSR
060300011113     C***
060400011113
060500011113
060600000801
060700000801
060800990920      /TITLE Invio dei dati al punto operativo.
060900010202     C     invio         BEGSR
061000990920     C*
061100021113     C* 1° invio FIVAT
061200010201     C                   reset                   dscmz
061300010201     C                   move      vlrpoi        cmzdst
061400021113     C                   eval      cmzfld = 'FIVATWWR'
061500010201     C                   eval      cmzmbd = vlrhdl
061600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061700021009     C***                if        prmfir = *blanks
061800021113     C                   eval      cmzfla = 'FIVAT00F'
061900021113     C                   eval      cmzmba = 'FIVAT00F'
062000021009     C***                else
062100021009     C***                eval      cmzfla = prmfir
062200021009     C***                eval      cmzmba = prmfir
062300021009     C***                endif
062400010201     C                   eval      cmznrr = *zeros
062500020305     C                   move      §ctrokvt      cmznrr
062600021018     C                   eval      cmzlba = vlrfl1
062700010201     C                   call(e)   'TIS711C'
062800010201     C                   parm                    dscmz
062900010201     C                   parm      *blanks       esito
063000010205     C                   if        %error
063100010205     C                             or cmzerr = '1'
063200010205     C                             or esito  = '1'
063300010205     C                   eval      wrkesito = '3'
063400010205     C                   else
063500010201     C*
063600021113     C* 2° invio FIVAB
063700010201     C                   reset                   dscmz
063800010201     C                   move      vlrpoi        cmzdst
063900010201     C                   eval      cmzfld = vlrfou
064000010201     C                   eval      cmzmbd = vlrhdl
064100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064200021009     C***                if        prmfir = *blanks
064300021113     C                   eval      cmzfla = 'FIVAB00F'
064400021113     C                   eval      cmzmba = 'FIVAB00F'
064500021009     C***                else
064600021009     C***                eval      cmzfla = prmfir
064700021009     C***                eval      cmzmba = prmfir
064800021009     C***                endif
064900010201     C                   eval      cmznrr = *zeros
065000010201     C                   move      §ctrokvb      cmznrr
065100021018     C                   eval      cmzlba = vlrfl1
065200010201     C                   call(e)   'TIS711C'
065300010201     C                   parm                    dscmz
065400010201     C                   parm      *blanks       esito
065500010201     C                   if        %error
065600010201     C                             or cmzerr = '1'
065700010201     C                             or esito  = '1'
065800010201     C                   eval      wrkesito = '3'
065900010201     C                   endif
066000010205     C                   endif
066100990920     C*
066200000613     C                   ENDSR
066300000613     C***
066400070411
066500070411     C     *pssr         BEGSR
066600070411     C*
066700070411     C                   if        %open(tivin00r)
066800070411     C                   close     tivin00r
066900070411     C                   endif
067000070411     C                   if        %open(fivabwwr)
067100070411     C                   close     fivabwwr
067200070411     C                   endif
067300070411     C                   if        %open(fivatwwr)
067400070411     C                   close     fivatwwr
067500070411     C                   endif
067600070411     C*
067700070411     C* Effettuo la chiamata al CLLE preposto
067800070411     C                   call(e)   'TITVVTC'
067900070411     C                   parm                    parccm
068000070411     C                   parm                    parmbr
068100070411     C                   parm      '2'           paropz
068200070411     C*
068300070411     C                   eval      wrkesito = '2'
068400070411     C*
068500070411     C                   seton                                        LR
068600070411     C*
068700070411     C                   ENDSR     '*CANCL'
068800070411     C***
068900070411
069000990910
069100000613     C     *inzsr        BEGSR
069200990910     C*
069300990910     C     *entry        plist
069400990920     C                   parm                    tivlrds
069500990921     C                   parm      wrkesito      esito
069600000724     C                   parm                    prmlit
069700000710     C                   parm                    prmfir
069800000613     C*
069900000830     C* CALCOLA LA DATA CORRENTE
070000100802     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
070100100802     C                   eval      datcor = %dec(%date() : *ISO)
070200000830     C*
070300000613     C                   ENDSR
070400000613     C***
070500990908
070600101029     Otitv2a9p  E            riepilogo         2
070700990915     O                                              'Upload via Internet'
070800990915     O                                           +1 'Traduzione TIVIN00R -'
070900021113     O                                           +1 'FIVABWWR/FIVATWWR'
071000010201     O                                           +1 'Report testate bolle'
071100990915     O          E            riepilogo   2
071200990915     O                       wrkdata
071300990915     O                       wrkora              +1
071400990915     O                       procname            +1
071500990915     O          E            riepilogo   2
071600990915     O                                              'Cliente..................:'
071700990915     O                       VABCCM        z     +1
071800990915     O          E            riepilogo   2
071900990920     O                                              'Riferimento Strategi.....:'
072000990920     O                       vlrhdl              +1
072100990915     O          E            riepilogo   2
072200990915     O                                              'Giusti...................:'
072300010201     O                       §CTROKVB      2   +  1
072400990915     O          E            riepilogo   2
072500990915     O                                              'Sbagliati e corretti.....:'
072600971022     O                       §CTRMO        2   +  1
072700990915     O          E            riepilogo   2
072800990915     O                                              'Sbagliati e scartati.....:'
072900971022     O                       §CTRNO        2   +  1
073000000613
073100101029     Otitv2a9ps E            testdett          2
073200000613     O                                              'Upload via Internet'
073300000613     O                                           +1 'Traduzione TIVIN00R -'
073400021113     O                                           +1 'FIVABWWR/FIVATWWR'
073500010201     O                                           +1 'Report testate bolle'
073600000616     O          E            testdett    3
073700000613     O                                           +2 'N° rec'
073800000613     O                                           +3 'Anteprima contenuto'
073900000616     O          E            rigadett    2
074000000613     O                       rrnum               +2
074100000621     O                       recko               +3
074200000616     O          E            findett     2
074300000613     O                       wrkdata
074400000613     O                       wrkora              +1
074500000613     O                       procname            +1
074600000616     O          E            findett     2
074700000613     O                                              'Cliente..................:'
074800000613     O                       VABCCM        z     +1
074900000616     O          E            findett     2
075000000613     O                                              'Riferimento Strategi.....:'
075100000613     O                       vlrhdl              +1
075200000616     O          E            findett     2
075300000613     O                                              'Giusti...................:'
075400010201     O                       §CTROKVB      2   +  1
075500000616     O          E            findett     2
075600000613     O                                              'Sbagliati e corretti.....:'
075700000613     O                       §CTRMO        2   +  1
075800000616     O          E            findett     2
075900000613     O                                              'Sbagliati e scartati.....:'
076000000613     O                       §CTRNO        2   +  1
076100000512** CMD - COMANDI CL
076200101029OVRPRTF FILE(TITV2A9P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076300101029OVRPRTF FILE(TITV2A9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076400101029DLTOVR FILE(TITV2A9P TITV2A9PS) LVL(*)
076500000512
076600000512
