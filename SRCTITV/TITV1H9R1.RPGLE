000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200090113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800090113     Ftitv1h9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090113     Ftitv1h9ps O    f  198        PRINTER usropn
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
009400090113     C                   if        not %open(titv1h9ps)
009500090113     C                   open      titv1h9ps
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
011400090113     C                   if        %open(titv1h9ps)
011500000616     C                   except    findett
011600090113     C                   close     titv1h9ps
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
013400090113     C                   if        not %open(titv1h9p)
013500090113     C                   open      titv1h9p
013600990915     C                   endif
013700990915     C*
013800990915     C                   except    riepilogo
013900990915     C*
014000090113     C                   if        %open(titv1h9p)
014100090113     C                   close     titv1h9p
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
019000090113     C                   eval      curSped = %subst(vindta:143:10)
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
028600090113     C                   Z-ADD     0540817       VABCCM
028700090113     C                   Z-ADD     0540817       VATCCM
028800090113     C                   Z-ADD     054           VABLNP
028900090113     C                   Z-ADD     054           VATLNP
029000090113     C                   Z-ADD     000           VABCTR
029100070103     C                   MOVEL     '7Q'          VABCTM
029200040823     C                   MOVEL     '1'           VABCBO
029300020619     C* ... e poi verifico se sono stati passati come parametri
029400020619     C                   IF        vlrppt > *blanks
029500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029700020619     C                   EXSR      CHKNUM
029800020619     C                   IF        PiInt=*on
029900020619     C                   Z-ADD     PiVal         VABCCM
030000020619     C                   Z-ADD     PiVal         VATCCM
030100020619     C                   ENDIF
030200040506     C                   ENDIF
030300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030500020619     C                   EXSR      CHKNUM
030600020619     C                   IF        PiInt=*on
030700020619     C                   Z-ADD     PiVal         VABLNP
030800020619     C                   Z-ADD     PiVal         VATLNP
030900040506     C                   ENDIF
031000020619     C                   ENDIF
031100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031300020619     C                   EXSR      CHKNUM
031400020619     C                   IF        PiInt=*on
031500020619     C                   Z-ADD     PiVal         VABCTR
031600040506     C                   ENDIF
031700020619     C                   ENDIF
031800020619     C                   ENDIF
031900000801     C*
032000000801     C                   ENDSR
032100000801     C*----------------------------------------------------*
032200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032300000801     C*----------------------------------------------------*
032400040823     C     IMPVAB        BEGSR
032500010305     C*
032600000801     C                   Z-ADD     *zeros        errore            1 0
032700000830     C                   MOVEL     datcor        VABAAS
032800020305     C                   MOVEL     datcor        VATAAS
032900040526     C                   MOVE      datcor        VABMGS
033000040823     C                   MOVE(P)   vlrpoi        VABFGS
033100040823     C                   MOVE(P)   vlrpoi        VATFGS
033200080409     C*
033300080409     C* Reperimento campi ALFA
033400080409     C*
033500080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
033600090113     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
033700080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033800080409     C     '@':'A'       XLATE     VABRSD        VABRSD
033900080409     C* ==
034000090113     C                   EVAL      VABRD2=%trim(%subst(vindta:1+35:35))
034100090113     C                   EVAL      VABIND=%trim(%subst(vindta:71:35))
034200090113     C                   EVAL      VABLOD=%trim(%subst(vindta:106:30))
034300090113     C                   EVAL      VABPRD=%trim(%subst(vindta:141:2))
034400090113     C                   EVAL      VABRMA=%trim(%subst(vindta:143:10))
034500090113     C                   EVAL      VABNOT=%trim(%subst(vindta:182:35))
034600090113     C                   EVAL      VABNT2=%trim(%subst(vindta:182+35:60-35))
034700090113     C                   EVAL      VABFFD=%trim(%subst(vindta:280:1))
034800080409     C*
034900080409     C* Reperimento campi NUMERICI
035000090113     C*
035100080409     C* NSP/RMN
035200090113     C                   EVAL      PiStr=%trim(%subst(vindta:143:10))
035300080409     C                   EXSR      CHKNUM
035400080409     C                   IF        PiInt=*on
035500080409     C                   Z-ADD     PiVal         VABNSP
035600080409     C                   Z-ADD     PiVal         VATNSP
035700080409     C                   Z-ADD     PiVal         VABRMN
035800080409     C                   ELSE
035900080409     C                   ADD       1             errore
036000080409     C                   Z-ADD     *zeros        VABNSP
036100080409     C                   Z-ADD     1             VABRMN
036200080409     C                   EVAL      vinmsg = %trimr(vinmsg)
036300080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
036400080409     C                   ENDIF
036500080409     C* CAD
036600090113     C                   EVAL      PiStr=%trim(%subst(vindta:136:5))
036700080409     C                   EXSR      CHKNUM
036800080409     C                   IF        PiInt=*on
036900080409     C                   Z-ADD     PiVal         Num5_0
037000080409     C                   MOVEL(p)  Num5_0        VABCAD
037100080409     C                   ELSE
037200080409     C                   ADD       1             errore
037300080409     C                   EVAL      VABCAD = *zeros
037400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
037500080409     C                             + ' ' + 'VABCAD'
037600080409     C                   ENDIF
037700080409     C* NCL
037800090113     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
037900080409     C                   EXSR      CHKNUM
038000080409     C                   IF        PiInt=*on
038100080409     C                   Z-ADD     PiVal         VABNCL
038200080409     C                   ELSE
038300080409     C                   ADD       1             errore
038400080409     C                   Z-ADD     *zeros        VABNCL
038500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
038600080409     C                             + ' ' + 'VABNCL'
038700080409     C                   ENDIF
038800080409     C* PKB
038900090113     C                   EVAL      PiStr=%trim(%subst(vindta:164:8))
039000080409     C                   EXSR      CHKNUM
039100080409     C                   IF        PiNum=*on
039200090113     C                   Z-ADD     PiVal         VABPKB
039300080409     C                   ELSE
039400080409     C                   ADD       1             errore
039500080409     C                   Z-ADD     *zeros        VABPKB
039600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
039700080409     C                             + ' ' + 'VABPKB'
039800080409     C                   ENDIF
039900080409     C* CAS
040000090113     C                   IF        %trim(%subst(vindta:172:10)) <> '0000000,00'
040100080409     C                   EVAL      FlgCAS = '1'
040200080415     C                   EVAL      VABVCA = 'EUR'
040300090113     C                   EVAL      PiStr=%trim(%subst(vindta:172:10))
040400080409     C                   EXSR      CHKNUM
040500080409     C                   IF        PiNum=*on
040600080409     C                   Z-ADD     PiVal         VABCAS
040700080409     C                   ELSE
040800080409     C                   ADD       1             errore
040900080409     C                   Z-ADD     *zeros        VABCAS
041000080409     C                   EVAL      vinmsg = %trimr(vinmsg)
041100080409     C                             + ' ' + 'VABCAS'
041200080409     C                   ENDIF
041300080409     C                   ENDIF
041400071121     C*
041500080415     C* Carico l'estensioni A e B del FNVAT
041600080415     C                   exsr      exevata
041700071121     C                   exsr      exevatb
041800010205     C*
041900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042000040802     C                   IF        FlgCAS <> '0'
042100010205     C                   EVAL      VABCBO = '4'
042200010205     C                   ENDIF
042300020305     C*
042400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042500011113     C                   EXSR      CHKIMPDIV
042600010202     C*
042700000801     C* Ebbene...
042800000801     C                   ADD       1             §CTRMO
042900010201     C                   IF        errore <> *zeros
043000000801     C                   ADD       1             §CTRNO
043100000801     C                   EVAL      recko = vindta
043200000801     C                   EXCEPT    rigadett
043300000801     C                   EVAL      vinflg = '2'
043400000801     C                   ELSE
043500010201     C                   ADD       1             §CTROKVB
043600000801     C                   ENDIF
043700000801     C*
043800000801     C                   ENDSR
043900070102     C*----------------------------------------------------*
044000070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044100070102     C*----------------------------------------------------*
044200070103     C     EXEVATE       BEGSR
044300070102     C*
044400090113     C                   Z-ADD     1             wCollo            2 0
044500090113     C                   MOVEL     *zeros        wColloAlfa1      14
044600090113     C                   MOVEL     *zeros        wColloAlfa2       2
044700080415     C*
044800070103     C                   EVAL      VATTRC='E'
044900090121     C                   EVAL      wColloAlfa1='540817'+%subst(vindta:147:6)
045000071019     C*
045100080415     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
045200090113     C                   dow       wCollo <= VABNCL and wCollo < 99
045300080415     C                   move(p)   wCollo        wColloAlfa2
045400080415     C                   eval      VATNOT = %trim(wColloAlfa1) + wColloAlfa2
045500070102     C                   exsr      wrivat                                       => scarico VAT
045600080415     C                   add       1             wCollo
045700080415     C                   enddo
045800070102     C*
045900070102     C                   ENDSR
046000080415     C*----------------------------------------------------*
046100080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
046200080415     C*----------------------------------------------------*
046300080415     C     EXEVATA       BEGSR
046400080415     C*
046500080415     C*
046600080415     C***                exsr      wrivat                                       => scarico VAT
046700080415     C*
046800080415     C                   ENDSR
046900071121     C*----------------------------------------------------*
047000071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
047100071121     C*----------------------------------------------------*
047200071121     C     EXEVATB       BEGSR
047300071121     C*
047400071121     C*
047500080409     C***                exsr      wrivat                                       => scarico VAT
047600071121     C*
047700071121     C                   ENDSR
047800010201     C*----------------------------------------------------*
047900040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048000010201     C*----------------------------------------------------*
048100020305     C     WRIVAT        BEGSR
048200050628     C*
048300060223     C* Scrivo solo se valorizzato qualcosa
048400060223     C                   IF        VATNOT <> *blanks
048500040802     C                   WRITE     FIVAT000
048600060223     C                   ENDIF
048700010201     C*
048800010201     C                   ENDSR
048900010202     C*----------------------------------------------------*
049000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049100010202     C*----------------------------------------------------*
049200020305     C     PREVAT        BEGSR
049300010202     C*
049400021113     C* Compongo il nome del membro da dare al FIVATWWR
049500010202     C                   eval      parmbr = vlrhdl
049600010202     C                   movel     'M'           parmbr
049700050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049800010202     C                   eval      paropz = '1'
049900010202     C* Effettuo la chiamata al CLLE preposto
050000040506     C                   call(e)   'TITVVTC'
050100010202     C                   parm                    parccm
050200010202     C                   parm                    parmbr
050300010202     C                   parm                    paropz
050400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050500010202     C                   if        %error
050600010202     C                   movel     '1'           chkcall
050700010202     C                   else
050800010202     C                   movel     '0'           chkcall
050900010202     C                   endif
051000010202     C*
051100010202     C                   ENDSR
051200000801     C*----------------------------------------------------*
051300000801     C*  CONTROLLO NUMERICITA' CAMPI
051400000801     C*----------------------------------------------------*
051500000801     C     CHKNUM        BEGSR
051600090113     C*
051700090113     C                   IF        PiDecChr = *blanks
051800090113     C                   EVAL      PiDecChr = ','
051900090113     C                   ENDIF
052000090113     C*
052100090113     C                   callp(e)  UBISNUM_Check(PiStr
052200090113     C                                          :PiDecChr
052300090113     C                                          :PiVal
052400090113     C                                          :PiNum
052500090113     C                                          :PiInt)
052600090113     C*
052700090113     C                   IF        %error
052800090113     C                   EVAL      PiInt=*off
052900090113     C                   ENDIF
053000000801     C*
053100000801     C                   ENDSR
053200000801     C***
053300000801
053400011113
053500011113     C*----------------------------------------------------*
053600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053700011113     C*----------------------------------------------------*
053800011113     C     CHKIMPDIV     BEGSR
053900011113     C*
054000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054100011113     C                   Z-ADD     *zeros        wrkDec            9 9
054200011113     C*
054300011113     C* Come prima cosa effettuo considerazioni sulla divisa
054400011113     C                   IF        vabIAS > *zeros
054500011113     C                   IF        vabVAS <> 'EUR'
054600011113     C                   EVAL      vabVAS =  'ITL'
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C                   IF        vabCAS > *zeros
055100011113     C                   IF        vabVCA <> 'EUR'
055200011113     C                   EVAL      vabVCA =  'ITL'
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C                   IF        vabVMD > *zeros
055700020305     C                   IF        vabVAD <> 'EUR'
055800011113     C                   EVAL      vabVAD =  'ITL'
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056300011113     C                   Z-ADD     vabIAS        wrkDec
056400011113     C                   IF        wrkDec > *zeros
056500011113     C                   IF        vabVAS = 'ITL'
056600011113     C                   EVAL      vabIAS = *zeros
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057100011113     C                   Z-ADD     vabCAS        wrkDec
057200011113     C                   IF        wrkDec > *zeros
057300011113     C                   IF        vabVCA = 'ITL'
057400011113     C                   EVAL      vabCAS = *zeros
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057900011113     C                   Z-ADD     vabVMD        wrkDec
058000011113     C                   IF        wrkDec > *zeros
058100011113     C                   IF        vabVAD = 'ITL'
058200011113     C                   EVAL      vabVMD = *zeros
058300011113     C                   ENDIF
058400011113     C                   ENDIF
058500011113     C*
058600011113     C                   ENDSR
058700011113     C***
058800011113
058900011113
059000000801
059100000801
059200990920      /TITLE Invio dei dati al punto operativo.
059300010202     C     invio         BEGSR
059400990920     C*
059500021113     C* 1° invio FIVAT
059600010201     C                   reset                   dscmz
059700010201     C                   move      vlrpoi        cmzdst
059800021113     C                   eval      cmzfld = 'FIVATWWR'
059900010201     C                   eval      cmzmbd = vlrhdl
060000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060100021009     C***                if        prmfir = *blanks
060200021113     C                   eval      cmzfla = 'FIVAT00F'
060300021113     C                   eval      cmzmba = 'FIVAT00F'
060400021009     C***                else
060500021009     C***                eval      cmzfla = prmfir
060600021009     C***                eval      cmzmba = prmfir
060700021009     C***                endif
060800010201     C                   eval      cmznrr = *zeros
060900020305     C                   move      §ctrokvt      cmznrr
061000021018     C                   eval      cmzlba = vlrfl1
061100010201     C                   call(e)   'TIS711C'
061200010201     C                   parm                    dscmz
061300010201     C                   parm      *blanks       esito
061400010205     C                   if        %error
061500010205     C                             or cmzerr = '1'
061600010205     C                             or esito  = '1'
061700010205     C                   eval      wrkesito = '3'
061800010205     C                   else
061900010201     C*
062000021113     C* 2° invio FIVAB
062100010201     C                   reset                   dscmz
062200010201     C                   move      vlrpoi        cmzdst
062300010201     C                   eval      cmzfld = vlrfou
062400010201     C                   eval      cmzmbd = vlrhdl
062500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062600021009     C***                if        prmfir = *blanks
062700021113     C                   eval      cmzfla = 'FIVAB00F'
062800021113     C                   eval      cmzmba = 'FIVAB00F'
062900021009     C***                else
063000021009     C***                eval      cmzfla = prmfir
063100021009     C***                eval      cmzmba = prmfir
063200021009     C***                endif
063300010201     C                   eval      cmznrr = *zeros
063400010201     C                   move      §ctrokvb      cmznrr
063500021018     C                   eval      cmzlba = vlrfl1
063600010201     C                   call(e)   'TIS711C'
063700010201     C                   parm                    dscmz
063800010201     C                   parm      *blanks       esito
063900010201     C                   if        %error
064000010201     C                             or cmzerr = '1'
064100010201     C                             or esito  = '1'
064200010201     C                   eval      wrkesito = '3'
064300010201     C                   endif
064400010205     C                   endif
064500990920     C*
064600000613     C                   ENDSR
064700000613     C***
064800070411
064900070411     C     *pssr         BEGSR
065000070411     C*
065100070411     C                   if        %open(tivin00r)
065200070411     C                   close     tivin00r
065300070411     C                   endif
065400070411     C                   if        %open(fivabwwr)
065500070411     C                   close     fivabwwr
065600070411     C                   endif
065700070411     C                   if        %open(fivatwwr)
065800070411     C                   close     fivatwwr
065900070411     C                   endif
066000070411     C*
066100070411     C* Effettuo la chiamata al CLLE preposto
066200070411     C                   call(e)   'TITVVTC'
066300070411     C                   parm                    parccm
066400070411     C                   parm                    parmbr
066500070411     C                   parm      '2'           paropz
066600070411     C*
066700070411     C                   eval      wrkesito = '2'
066800070411     C*
066900070411     C                   seton                                        LR
067000070411     C*
067100070411     C                   ENDSR     '*CANCL'
067200070411     C***
067300070411
067400990910
067500000613     C     *inzsr        BEGSR
067600990910     C*
067700990910     C     *entry        plist
067800990920     C                   parm                    tivlrds
067900990921     C                   parm      wrkesito      esito
068000000724     C                   parm                    prmlit
068100000710     C                   parm                    prmfir
068200000613     C*
068300000830     C* CALCOLA LA DATA CORRENTE
068400000830     C                   time                    wn14             14 0
068500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068600000830     C                   z-add     wn8           g08dat
068700000830     C                   z-add     *zeros        g08inv
068800000830     C                   movel     '0'           g08err
068900000830     C                   call      'XSRDA8'
069000000830     C                   parm                    wlbda8
069100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069200000830     C*
069300000613     C                   ENDSR
069400000613     C***
069500990908
069600090113     Otitv1h9p  E            riepilogo         2
069700990915     O                                              'Upload via Internet'
069800990915     O                                           +1 'Traduzione TIVIN00R -'
069900021113     O                                           +1 'FIVABWWR/FIVATWWR'
070000010201     O                                           +1 'Report testate bolle'
070100990915     O          E            riepilogo   2
070200990915     O                       wrkdata
070300990915     O                       wrkora              +1
070400990915     O                       procname            +1
070500990915     O          E            riepilogo   2
070600990915     O                                              'Cliente..................:'
070700990915     O                       VABCCM        z     +1
070800990915     O          E            riepilogo   2
070900990920     O                                              'Riferimento Strategi.....:'
071000990920     O                       vlrhdl              +1
071100990915     O          E            riepilogo   2
071200990915     O                                              'Giusti...................:'
071300010201     O                       §CTROKVB      2   +  1
071400990915     O          E            riepilogo   2
071500990915     O                                              'Sbagliati e corretti.....:'
071600971022     O                       §CTRMO        2   +  1
071700990915     O          E            riepilogo   2
071800990915     O                                              'Sbagliati e scartati.....:'
071900971022     O                       §CTRNO        2   +  1
072000000613
072100090113     Otitv1h9ps E            testdett          2
072200000613     O                                              'Upload via Internet'
072300000613     O                                           +1 'Traduzione TIVIN00R -'
072400021113     O                                           +1 'FIVABWWR/FIVATWWR'
072500010201     O                                           +1 'Report testate bolle'
072600000616     O          E            testdett    3
072700000613     O                                           +2 'N° rec'
072800000613     O                                           +3 'Anteprima contenuto'
072900000616     O          E            rigadett    2
073000000613     O                       rrnum               +2
073100000621     O                       recko               +3
073200000616     O          E            findett     2
073300000613     O                       wrkdata
073400000613     O                       wrkora              +1
073500000613     O                       procname            +1
073600000616     O          E            findett     2
073700000613     O                                              'Cliente..................:'
073800000613     O                       VABCCM        z     +1
073900000616     O          E            findett     2
074000000613     O                                              'Riferimento Strategi.....:'
074100000613     O                       vlrhdl              +1
074200000616     O          E            findett     2
074300000613     O                                              'Giusti...................:'
074400010201     O                       §CTROKVB      2   +  1
074500000616     O          E            findett     2
074600000613     O                                              'Sbagliati e corretti.....:'
074700000613     O                       §CTRMO        2   +  1
074800000616     O          E            findett     2
074900000613     O                                              'Sbagliati e scartati.....:'
075000000613     O                       §CTRNO        2   +  1
075100000512** CMD - COMANDI CL
075200090113OVRPRTF FILE(TITV1H9P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075300090113OVRPRTF FILE(TITV1H9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075400090113DLTOVR FILE(TITV1H9P TITV1H9PS) LVL(*)
075500000512
075600000512
