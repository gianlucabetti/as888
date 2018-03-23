000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200090113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100802     Ftitv1z4p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100802     Ftitv1z4ps O    f  198        PRINTER usropn
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
004000100802     D curSped         s              7    INZ(*blanks)
004100100802     D depSped         s              7    INZ(*blanks)
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
008600100802     C                   if        not %open(titv1z4ps)
008700100802     C                   open      titv1z4ps
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
010600100802     C                   if        %open(titv1z4ps)
010700000616     C                   except    findett
010800100802     C                   close     titv1z4ps
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
012600100802     C                   if        not %open(titv1z4p)
012700100802     C                   open      titv1z4p
012800990915     C                   endif
012900990915     C*
013000990915     C                   except    riepilogo
013100990915     C*
013200100802     C                   if        %open(titv1z4p)
013300100802     C                   close     titv1z4p
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
018200100802     C                   eval      curSped = %subst(vindta:118:7)
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
019600110225 xxx C***                exsr      exevate                                      => write VAT-E
019700070103     C* Salvo il raggruppamento spedizione cliente corrente
019800070103     C                   eval      depSped = curSped
019900070213     C*
020000070213     C* Se collo successivo x stessa bolla
020100070213     C                   else
020200070213     C                   exsr      impvab                                       => carico  VAB
020300110225 xxx C***                exsr      exevate                                      => write VAT-E
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
027800100802     C                   Z-ADD     0055685       VABCCM
027900100802     C                   Z-ADD     0055685       VATCCM
028000090213     C                   Z-ADD     005           VABLNP
028100090213     C                   Z-ADD     005           VATLNP
028200090113     C                   Z-ADD     000           VABCTR
028300110225 xxx C***                MOVEL     '7Q'          VABCTM
028400040823     C                   MOVEL     '1'           VABCBO
028500020619     C* ... e poi verifico se sono stati passati come parametri
028600020619     C                   IF        vlrppt > *blanks
028700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028900020619     C                   EXSR      CHKNUM
029000020619     C                   IF        PiInt=*on
029100020619     C                   Z-ADD     PiVal         VABCCM
029200020619     C                   Z-ADD     PiVal         VATCCM
029300020619     C                   ENDIF
029400040506     C                   ENDIF
029500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029700020619     C                   EXSR      CHKNUM
029800020619     C                   IF        PiInt=*on
029900020619     C                   Z-ADD     PiVal         VABLNP
030000020619     C                   Z-ADD     PiVal         VATLNP
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
031800000801     C                   Z-ADD     *zeros        errore            1 0
031900000830     C                   MOVEL     datcor        VABAAS
032000020305     C                   MOVEL     datcor        VATAAS
032100040526     C                   MOVE      datcor        VABMGS
032200040823     C                   MOVE(P)   vlrpoi        VABFGS
032300040823     C                   MOVE(P)   vlrpoi        VATFGS
032400080409     C*
032500080409     C* Reperimento campi ALFA
032600080409     C*
032700080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
032800090213     C                   EVAL      VABRSD=%trim(%subst(vindta:668:35))
032900080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033000080409     C     '@':'A'       XLATE     VABRSD        VABRSD
033100080409     C* ==
033200090213     C                   EVAL      VABRD2=%trim(%subst(vindta:668+35:50-35))
033300090213     C                   EVAL      VABIND=%trim(%subst(vindta:550:30))
033400090213     C                   EVAL      VABLOD=%trim(%subst(vindta:718:30))
033500090213     C                   EVAL      VABPRD=%trim(%subst(vindta:786:2))
033600090213     C                   EVAL      VABRMA=%trim(%subst(vindta:118:24))
033700100802     C***                EVAL      VABTSP=%trim(%subst(vindta:78:1))
033800090213     C                   EVAL      VABNOT=%trim(%subst(vindta:142:35))
033900090213     C                   EVAL      VABNT2=%trim(%subst(vindta:142+35:60-35))
034000090213     C                   IF        %subst(vindta:649:2) = 'IT'
034100090213     C                   EVAL      VABNZD=*blanks
034200090213     C                   ELSE
034300090213     C                   EVAL      VABNZD=%trim(%subst(vindta:649:3))
034400090213     C                   ENDIF
034500090213     C                   IF        %subst(vindta:202:1) = 'R'
034600090213     C                   EVAL      VABCBO='2'
034700090213     C                   ELSE
034800090213     C                   EVAL      VABCBO='1'
034900090213     C                   ENDIF
035000080409     C*
035100080409     C* Reperimento campi NUMERICI
035200090113     C*
035300080409     C* NSP/RMN
035400100802     C                   EVAL      PiStr=%trim(%subst(vindta:118:7))
035500080409     C                   EXSR      CHKNUM
035600080409     C                   IF        PiInt=*on
035700080409     C                   Z-ADD     PiVal         VABNSP
035800080409     C                   Z-ADD     PiVal         VATNSP
035900080409     C                   Z-ADD     PiVal         VABRMN
036000080409     C                   ELSE
036100080409     C                   ADD       1             errore
036200080409     C                   Z-ADD     *zeros        VABNSP
036300080409     C                   Z-ADD     1             VABRMN
036400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
036500080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
036600080409     C                   ENDIF
036700080409     C* CAD
036800090213     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
036900080409     C                   EXSR      CHKNUM
037000080409     C                   IF        PiInt=*on
037100080409     C                   Z-ADD     PiVal         Num5_0
037200080409     C                   MOVEL(p)  Num5_0        VABCAD
037300080409     C                   ELSE
037400080409     C                   ADD       1             errore
037500080409     C                   EVAL      VABCAD = *zeros
037600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
037700080409     C                             + ' ' + 'VABCAD'
037800080409     C                   ENDIF
037900080409     C* NCL
038000090213     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
038100080409     C                   EXSR      CHKNUM
038200080409     C                   IF        PiInt=*on
038300080409     C                   Z-ADD     PiVal         VABNCL
038400080409     C                   ELSE
038500080409     C                   ADD       1             errore
038600080409     C                   Z-ADD     *zeros        VABNCL
038700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
038800080409     C                             + ' ' + 'VABNCL'
038900080409     C                   ENDIF
039000080409     C* PKB
039100090213     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
039200080409     C                   EXSR      CHKNUM
039300080409     C                   IF        PiNum=*on
039400090213     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
039500090113     C                   Z-ADD     PiVal         VABPKB
039600080409     C                   ELSE
039700080409     C                   ADD       1             errore
039800080409     C                   Z-ADD     *zeros        VABPKB
039900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
040000080409     C                             + ' ' + 'VABPKB'
040100080409     C                   ENDIF
040200090213     C* IAS
040300090213     C                   IF        %trim(%subst(vindta:82:13)) <> *blanks
040400090213     C                   EVAL      VABVCA = %trim(%subst(vindta:95:3))
040500090213     C                   EVAL      PiStr=%trim(%subst(vindta:82:13))
040600080409     C                   EXSR      CHKNUM
040700080409     C                   IF        PiNum=*on
040800090213     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
040900090213     C                   Z-ADD     PiVal         VABIAS
041000080409     C                   ELSE
041100080409     C                   ADD       1             errore
041200090213     C                   Z-ADD     *zeros        VABIAS
041300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
041400090213     C                             + ' ' + 'VABIAS'
041500080409     C                   ENDIF
041600080409     C                   ENDIF
041700071121     C*
041800080415     C* Carico l'estensioni A e B del FNVAT
041900080415     C                   exsr      exevata
042000071121     C                   exsr      exevatb
042100010205     C*
042200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042300040802     C                   IF        FlgCAS <> '0'
042400010205     C                   EVAL      VABCBO = '4'
042500010205     C                   ENDIF
042600020305     C*
042700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042800011113     C                   EXSR      CHKIMPDIV
042900010202     C*
043000000801     C* Ebbene...
043100000801     C                   ADD       1             §CTRMO
043200010201     C                   IF        errore <> *zeros
043300000801     C                   ADD       1             §CTRNO
043400000801     C                   EVAL      recko = vindta
043500000801     C                   EXCEPT    rigadett
043600000801     C                   EVAL      vinflg = '2'
043700000801     C                   ELSE
043800010201     C                   ADD       1             §CTROKVB
043900000801     C                   ENDIF
044000000801     C*
044100000801     C                   ENDSR
044200070102     C*----------------------------------------------------*
044300070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044400070102     C*----------------------------------------------------*
044500070103     C     EXEVATE       BEGSR
044600070102     C*
044700090113     C                   Z-ADD     1             wCollo            2 0
044800090213     C                   MOVEL     *zeros        wColloAlfa1      12
044900080415     C*
045000070103     C                   EVAL      VATTRC='E'
045100100802     C                   EVAL      wColloAlfa1=%subst(vindta:118:7)
045200071019     C*
045300080415     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
045400090113     C                   dow       wCollo <= VABNCL and wCollo < 99
045500090219     C                   eval      VATNOT = %trim(wColloAlfa1) + %char(wCollo)
045600070102     C                   exsr      wrivat                                       => scarico VAT
045700080415     C                   add       1             wCollo
045800080415     C                   enddo
045900070102     C*
046000070102     C                   ENDSR
046100080415     C*----------------------------------------------------*
046200080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
046300080415     C*----------------------------------------------------*
046400080415     C     EXEVATA       BEGSR
046500080415     C*
046600100802     C                   EVAL      VATTRC='A'
046700100802     C                   EVAL      VATNOT=%trim(%subst(vindta:748:22))
046800080415     C*
046900100802     C                   exsr      wrivat                                       => scarico VAT
047000080415     C*
047100080415     C                   ENDSR
047200071121     C*----------------------------------------------------*
047300071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
047400071121     C*----------------------------------------------------*
047500071121     C     EXEVATB       BEGSR
047600071121     C*
047700090213     C                   EVAL      VATTRC='B'
047800100802     C                   EVAL      VATNOT=%trim(%subst(vindta:652:16))
047900071121     C*
048000090213     C                   exsr      wrivat                                       => scarico VAT
048100071121     C*
048200071121     C                   ENDSR
048300010201     C*----------------------------------------------------*
048400040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048500010201     C*----------------------------------------------------*
048600020305     C     WRIVAT        BEGSR
048700050628     C*
048800060223     C* Scrivo solo se valorizzato qualcosa
048900060223     C                   IF        VATNOT <> *blanks
049000040802     C                   WRITE     FIVAT000
049100060223     C                   ENDIF
049200010201     C*
049300010201     C                   ENDSR
049400010202     C*----------------------------------------------------*
049500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049600010202     C*----------------------------------------------------*
049700020305     C     PREVAT        BEGSR
049800010202     C*
049900021113     C* Compongo il nome del membro da dare al FIVATWWR
050000010202     C                   eval      parmbr = vlrhdl
050100010202     C                   movel     'M'           parmbr
050200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050300010202     C                   eval      paropz = '1'
050400010202     C* Effettuo la chiamata al CLLE preposto
050500040506     C                   call(e)   'TITVVTC'
050600010202     C                   parm                    parccm
050700010202     C                   parm                    parmbr
050800010202     C                   parm                    paropz
050900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051000010202     C                   if        %error
051100010202     C                   movel     '1'           chkcall
051200010202     C                   else
051300010202     C                   movel     '0'           chkcall
051400010202     C                   endif
051500010202     C*
051600010202     C                   ENDSR
051700000801     C*----------------------------------------------------*
051800000801     C*  CONTROLLO NUMERICITA' CAMPI
051900000801     C*----------------------------------------------------*
052000000801     C     CHKNUM        BEGSR
052100090113     C*
052200090113     C                   IF        PiDecChr = *blanks
052300090113     C                   EVAL      PiDecChr = ','
052400090113     C                   ENDIF
052500090113     C*
052600090113     C                   callp(e)  UBISNUM_Check(PiStr
052700090113     C                                          :PiDecChr
052800090113     C                                          :PiVal
052900090113     C                                          :PiNum
053000090113     C                                          :PiInt)
053100090113     C*
053200090113     C                   IF        %error
053300090113     C                   EVAL      PiInt=*off
053400090113     C                   ENDIF
053500000801     C*
053600000801     C                   ENDSR
053700000801     C***
053800000801
053900011113
054000011113     C*----------------------------------------------------*
054100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054200011113     C*----------------------------------------------------*
054300011113     C     CHKIMPDIV     BEGSR
054400011113     C*
054500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054600011113     C                   Z-ADD     *zeros        wrkDec            9 9
054700011113     C*
054800011113     C* Come prima cosa effettuo considerazioni sulla divisa
054900011113     C                   IF        vabIAS > *zeros
055000011113     C                   IF        vabVAS <> 'EUR'
055100011113     C                   EVAL      vabVAS =  'ITL'
055200011113     C                   ENDIF
055300011113     C                   ENDIF
055400011113     C*
055500011113     C                   IF        vabCAS > *zeros
055600011113     C                   IF        vabVCA <> 'EUR'
055700011113     C                   EVAL      vabVCA =  'ITL'
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C                   IF        vabVMD > *zeros
056200020305     C                   IF        vabVAD <> 'EUR'
056300011113     C                   EVAL      vabVAD =  'ITL'
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056800011113     C                   Z-ADD     vabIAS        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVAS = 'ITL'
057100011113     C                   EVAL      vabIAS = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057600011113     C                   Z-ADD     vabCAS        wrkDec
057700011113     C                   IF        wrkDec > *zeros
057800011113     C                   IF        vabVCA = 'ITL'
057900011113     C                   EVAL      vabCAS = *zeros
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058400011113     C                   Z-ADD     vabVMD        wrkDec
058500011113     C                   IF        wrkDec > *zeros
058600011113     C                   IF        vabVAD = 'ITL'
058700011113     C                   EVAL      vabVMD = *zeros
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C                   ENDSR
059200011113     C***
059300011113
059400011113
059500000801
059600000801
059700990920      /TITLE Invio dei dati al punto operativo.
059800010202     C     invio         BEGSR
059900990920     C*
060000021113     C* 1° invio FIVAT
060100010201     C                   reset                   dscmz
060200010201     C                   move      vlrpoi        cmzdst
060300021113     C                   eval      cmzfld = 'FIVATWWR'
060400010201     C                   eval      cmzmbd = vlrhdl
060500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060600021009     C***                if        prmfir = *blanks
060700021113     C                   eval      cmzfla = 'FIVAT00F'
060800021113     C                   eval      cmzmba = 'FIVAT00F'
060900021009     C***                else
061000021009     C***                eval      cmzfla = prmfir
061100021009     C***                eval      cmzmba = prmfir
061200021009     C***                endif
061300010201     C                   eval      cmznrr = *zeros
061400020305     C                   move      §ctrokvt      cmznrr
061500021018     C                   eval      cmzlba = vlrfl1
061600010201     C                   call(e)   'TIS711C'
061700010201     C                   parm                    dscmz
061800010201     C                   parm      *blanks       esito
061900010205     C                   if        %error
062000010205     C                             or cmzerr = '1'
062100010205     C                             or esito  = '1'
062200010205     C                   eval      wrkesito = '3'
062300010205     C                   else
062400010201     C*
062500021113     C* 2° invio FIVAB
062600010201     C                   reset                   dscmz
062700010201     C                   move      vlrpoi        cmzdst
062800010201     C                   eval      cmzfld = vlrfou
062900010201     C                   eval      cmzmbd = vlrhdl
063000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063100021009     C***                if        prmfir = *blanks
063200021113     C                   eval      cmzfla = 'FIVAB00F'
063300021113     C                   eval      cmzmba = 'FIVAB00F'
063400021009     C***                else
063500021009     C***                eval      cmzfla = prmfir
063600021009     C***                eval      cmzmba = prmfir
063700021009     C***                endif
063800010201     C                   eval      cmznrr = *zeros
063900010201     C                   move      §ctrokvb      cmznrr
064000021018     C                   eval      cmzlba = vlrfl1
064100010201     C                   call(e)   'TIS711C'
064200010201     C                   parm                    dscmz
064300010201     C                   parm      *blanks       esito
064400010201     C                   if        %error
064500010201     C                             or cmzerr = '1'
064600010201     C                             or esito  = '1'
064700010201     C                   eval      wrkesito = '3'
064800010201     C                   endif
064900010205     C                   endif
065000990920     C*
065100000613     C                   ENDSR
065200000613     C***
065300070411
065400070411     C     *pssr         BEGSR
065500070411     C*
065600070411     C                   if        %open(tivin00r)
065700070411     C                   close     tivin00r
065800070411     C                   endif
065900070411     C                   if        %open(fivabwwr)
066000070411     C                   close     fivabwwr
066100070411     C                   endif
066200070411     C                   if        %open(fivatwwr)
066300070411     C                   close     fivatwwr
066400070411     C                   endif
066500070411     C*
066600070411     C* Effettuo la chiamata al CLLE preposto
066700070411     C                   call(e)   'TITVVTC'
066800070411     C                   parm                    parccm
066900070411     C                   parm                    parmbr
067000070411     C                   parm      '2'           paropz
067100070411     C*
067200070411     C                   eval      wrkesito = '2'
067300070411     C*
067400070411     C                   seton                                        LR
067500070411     C*
067600070411     C                   ENDSR     '*CANCL'
067700070411     C***
067800070411
067900990910
068000000613     C     *inzsr        BEGSR
068100990910     C*
068200990910     C     *entry        plist
068300990920     C                   parm                    tivlrds
068400990921     C                   parm      wrkesito      esito
068500000724     C                   parm                    prmlit
068600000710     C                   parm                    prmfir
068700000613     C*
068800000830     C* CALCOLA LA DATA CORRENTE
068900100802     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
069000100802     C                   eval      datcor = %dec(%date() : *ISO)
069100000830     C*
069200000613     C                   ENDSR
069300000613     C***
069400990908
069500100802     Otitv1z4p  E            riepilogo         2
069600990915     O                                              'Upload via Internet'
069700990915     O                                           +1 'Traduzione TIVIN00R -'
069800021113     O                                           +1 'FIVABWWR/FIVATWWR'
069900010201     O                                           +1 'Report testate bolle'
070000990915     O          E            riepilogo   2
070100990915     O                       wrkdata
070200990915     O                       wrkora              +1
070300990915     O                       procname            +1
070400990915     O          E            riepilogo   2
070500990915     O                                              'Cliente..................:'
070600990915     O                       VABCCM        z     +1
070700990915     O          E            riepilogo   2
070800990920     O                                              'Riferimento Strategi.....:'
070900990920     O                       vlrhdl              +1
071000990915     O          E            riepilogo   2
071100990915     O                                              'Giusti...................:'
071200010201     O                       §CTROKVB      2   +  1
071300990915     O          E            riepilogo   2
071400990915     O                                              'Sbagliati e corretti.....:'
071500971022     O                       §CTRMO        2   +  1
071600990915     O          E            riepilogo   2
071700990915     O                                              'Sbagliati e scartati.....:'
071800971022     O                       §CTRNO        2   +  1
071900000613
072000100802     Otitv1z4ps E            testdett          2
072100000613     O                                              'Upload via Internet'
072200000613     O                                           +1 'Traduzione TIVIN00R -'
072300021113     O                                           +1 'FIVABWWR/FIVATWWR'
072400010201     O                                           +1 'Report testate bolle'
072500000616     O          E            testdett    3
072600000613     O                                           +2 'N° rec'
072700000613     O                                           +3 'Anteprima contenuto'
072800000616     O          E            rigadett    2
072900000613     O                       rrnum               +2
073000000621     O                       recko               +3
073100000616     O          E            findett     2
073200000613     O                       wrkdata
073300000613     O                       wrkora              +1
073400000613     O                       procname            +1
073500000616     O          E            findett     2
073600000613     O                                              'Cliente..................:'
073700000613     O                       VABCCM        z     +1
073800000616     O          E            findett     2
073900000613     O                                              'Riferimento Strategi.....:'
074000000613     O                       vlrhdl              +1
074100000616     O          E            findett     2
074200000613     O                                              'Giusti...................:'
074300010201     O                       §CTROKVB      2   +  1
074400000616     O          E            findett     2
074500000613     O                                              'Sbagliati e corretti.....:'
074600000613     O                       §CTRMO        2   +  1
074700000616     O          E            findett     2
074800000613     O                                              'Sbagliati e scartati.....:'
074900000613     O                       §CTRNO        2   +  1
075000000512** CMD - COMANDI CL
075100100802OVRPRTF FILE(TITV1Z4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075200100802OVRPRTF FILE(TITV1Z4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075300100802DLTOVR FILE(TITV1Z4P TITV1Z4PS) LVL(*)
075400000512
075500000512
