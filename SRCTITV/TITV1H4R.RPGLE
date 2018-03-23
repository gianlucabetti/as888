000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080626     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800080626     Ftitv1h4p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100080626     Ftitv1h4ps O    f  198        PRINTER usropn
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
004000080626     D curSped         s              6    INZ(*blanks)
004100080626     D depSped         s              6    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100080626
005200080626     D*------------------
005300080626     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
005400080626     D*------------------
005500080626     D PiStr           S            100A   INZ
005600080626     D PiDecChr        S              1A   INZ
005700080626     D PiVal           S             63S30 INZ
005800080626     D PiInt           S               N   INZ
005900080626     D PiNum           S               N   INZ
006000080626
006100080626     D*------------------
006200080626     D* LINKING A DEFINIZIONI ESTERNE
006300080626     D*------------------
006400080626     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006500010201
006600010201
006700990915     C                   time                    wrkdata
006800990915     C                   time                    wrkora
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200000724     C*
007300000724     C* SE OCCORRE SPEDIRE IN FILIALE
007400000724     C                   if        vlrpoi <> *zeros
007500000724     C*
007600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007800000724     C     vlrpoi        chain     azorg01l
007900000724     C                   if        %found
008000000616     C                   movel(p)  CMD(1)        depcmd
008100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008200000616     C*
008300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008400011113     C                   Z-ADD     150           LENGH            15 5
008500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008600000616     C                   PARM                    depcmd
008700000616     C                   PARM                    LENGH
008800000724     C*
008900000724     C                   endif
009000000724     C                   endif
009100000616     C*
009200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000616     C                   movel(p)  CMD(2)        depcmd
009400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009500000616     C*
009600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009700011113     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    depcmd
010000000616     C                   PARM                    LENGH
010100000616     C*
010200080626     C                   if        not %open(titv1h4ps)
010300080626     C                   open      titv1h4ps
010400000616     C                   except    testdett
010500000613     C                   endif
010600000613     C*
010700040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010900000613     C*
011000010202     C* Effettuo la chiamata al CLLE preposto
011100040506     C                   call(e)   'TITVVTC'
011200010202     C                   parm                    parccm
011300010202     C                   parm                    parmbr
011400010202     C                   parm      '2'           paropz
011500050201     C*
011600050201     C* Effettuo lancio TISI95 solo x chiusura
011700050201     C                   CLEAR                   TISI95DS
011800050201     C                   EVAL      I95TLA = 'C'
011900050201     C                   CALL      'TISI95R'
012000050201     C                   PARM                    TISI95DS
012100010202     C*
012200080626     C                   if        %open(titv1h4ps)
012300000616     C                   except    findett
012400080626     C                   close     titv1h4ps
012500000613     C                   endif
012600000616     C*
012700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012800011113     C                   Z-ADD     150           LENGH            15 5
012900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013000000616     C                   PARM                    CMD(3)
013100000616     C                   PARM                    LENGH
013200000616     C*
013300000801     C
013400010201     C                   seton                                        LR
013500000613
013600000613
013700000613     C*--------------------------------------------------------
013800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013900000613     C*--------------------------------------------------------
014000000621     C     STPR          BEGSR
014100000613     C*
014200080626     C                   if        not %open(titv1h4p)
014300080626     C                   open      titv1h4p
014400990915     C                   endif
014500990915     C*
014600990915     C                   except    riepilogo
014700990915     C*
014800080626     C                   if        %open(titv1h4p)
014900080626     C                   close     titv1h4p
015000990914     C                   endif
015100990910     C*
015200000613     C                   ENDSR
015300000613     C***
015400990908
015500000801
015600910830     C*--------------------------------------------------------
015700040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015800910830     C*--------------------------------------------------------
015900040526     C     RWFILE        BEGSR
016000990910     C*
016100990914     C                   if        not %open(tivin00r)
016200990908     C                   open      tivin00r
016300990914     C                   endif
016400021113     C                   if        not %open(fivabwwr)
016500021113     C                   open      fivabwwr
016600990914     C                   endif
016700070103     C*
016800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016900020305     C                   exsr      prevat
017000010201     C*
017100010202     C                   if        chkcall = '0'
017200010202     C*
017300021113     C                   if        not %open(fivatwwr)
017400021113     C                   open      fivatwwr
017500010201     C                   endif
017600990910     C*
017700010201     C                   clear                   §CTROKVB          5 0
017800020305     C                   clear                   §CTROKVT          5 0
017900000801     C                   clear                   §CTRMO            5 0
018000000801     C                   clear                   §CTRNO            5 0
018100990910     C*
018200921023     C                   DO        *HIVAL
018300990913     C*
018400990915     C                   READ      tivin00r                               70
018500050627     C                   if        vindta > *blanks
018600000613     C                   add       1             rrnum
018700000801     C*
018800000801     C                   if        *in70 = *off
018900000801     C                             and
019000000801     C                             (vinflg = *blanks
019100000801     C                              or vinflg = '0'
019200000801     C                              or vinflg = '2')
019300000801     C*
019400000801     C                   clear                   vinmsg
019500000801     C                   eval      vinflg = '1'
019600070103     C*
019700070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
019800080626     C                   eval      curSped = %subst(vindta:187:6)
019900070213     C                   if        curSped <> depSped
020000070213     C* Se prima bolla => importo bolla corrente
020100070213     C                   if        §CTRMO = *zeros
020200070213     C                   exsr      inzvar
020300070213     C                   exsr      defcam
020400070213     C                   exsr      impvab                                       => carico  VAB
020500070213     C                   else
020600070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
020700070213     C                   exsr      wrivab                                       => scarico VAB
020800070213     C                   exsr      inzvar
020900070213     C                   exsr      defcam
021000070213     C                   exsr      impvab                                       => carico  VAB
021100070213     C                   endif
021200070213     C                   exsr      exevate                                      => write VAT-E
021300070103     C* Salvo il raggruppamento spedizione cliente corrente
021400070103     C                   eval      depSped = curSped
021500070213     C*
021600070213     C* Se collo successivo x stessa bolla
021700070213     C                   else
021800070213     C                   exsr      impvab                                       => carico  VAB
021900070213     C                   exsr      exevate                                      => write VAT-E
022000070103     C                   endif
022100000905     C*
022200000905     C                   else
022300000905     C                   eval      vinflg = '1'
022400050628     C                   endif
022500000905     C                   endif
022600000905     C*
022700000905     C  N70              update    tivin000
022800000905     C*
022900991022     C  N70              ENDdo
023000070213     C*
023100070213     C* Scarico testata bolla rimasta in sospesa
023200070213     C                   exsr      wrivab                                       => scarico VAB
023300010202     C*
023400010202     C                   endif
023500990910
023600990910     C* Se non ci sono record con errori ...
023700000710     C                   if        §ctrno = 0
023800990910     C* ... restituisco esito OK.
023900990921     C                   eval      wrkesito = '0'
024000990910     C                   else
024100010201     C                   if        §ctrokvb > 0
024200990921     C                   eval      wrkesito = '1'
024300000710     C                   else
024400000710     C                   eval      wrkesito = '2'
024500990910     C                   endif
024600000710     C                   endif
024700990910     C*
024800990914     C                   if        %open(tivin00r)
024900990908     C                   close     tivin00r
025000990914     C                   endif
025100021113     C                   if        %open(fivabwwr)
025200021113     C                   close     fivabwwr
025300990914     C                   endif
025400021113     C                   if        %open(fivatwwr)
025500021113     C                   close     fivatwwr
025600010201     C                   endif
025700990910     C*
025800010201     C                   if        §ctrokvb > 0
025900000724     C                             and vlrpoi <> *zeros
026000010202     C                   exsr      invio
026100990920     C                   endif
026200990920     C*
026300910830     C                   ENDSR
026400000613     C***
026500010305
026600010305     C*----------------------------------------------------*
026700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026800010305     C*----------------------------------------------------*
026900020305     C     WRIVAB        BEGSR
027000010305     C*
027100060225     C* Quindi scarico il buffer del file d testata
027200021113     C                   write     fivab000                                     => scarico il VAB
027300010305     C*
027400010305     C                   ENDSR
027500990920
027600000801     C*----------------------------------------------------*
027700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027800000801     C*----------------------------------------------------*
027900010201     C     INZVAR        BEGSR
028000000801     C*
028100070213     C                   CLEAR                   FIVAB000
028200070213     C                   CLEAR                   FIVAT000
028300070213     C*
028400040802     C                   Z-ADD     *zeros        Num5_0            5 0
028500040802     C                   MOVEL     '0'           FlgCAS            1
028600000801     C*
028700000801     C                   ENDSR
028800000801     C*----------------------------------------------------*
028900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029000000801     C*----------------------------------------------------*
029100000801     C     DEFCAM        BEGSR
029200000801     C*
029300020619     C* Imposto i valori di default...
029400080626     C                   Z-ADD     0961170       VABCCM
029500080626     C                   Z-ADD     0961170       VATCCM
029600080626     C                   Z-ADD     096           VABLNP
029700080626     C                   Z-ADD     096           VATLNP
029800070927     C                   Z-ADD     000           VABCTR
029900080626     C                   MOVEL     'C'           VABTSP
030000040823     C                   MOVEL     '1'           VABCBO
030100020619     C* ... e poi verifico se sono stati passati come parametri
030200020619     C                   IF        vlrppt > *blanks
030300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
030400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030500020619     C                   EXSR      CHKNUM
030600020619     C                   IF        PiInt=*on
030700020619     C                   Z-ADD     PiVal         VABCCM
030800020619     C                   Z-ADD     PiVal         VATCCM
030900020619     C                   ENDIF
031000040506     C                   ENDIF
031100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
031200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031300020619     C                   EXSR      CHKNUM
031400020619     C                   IF        PiInt=*on
031500020619     C                   Z-ADD     PiVal         VABLNP
031600020619     C                   Z-ADD     PiVal         VATLNP
031700040506     C                   ENDIF
031800020619     C                   ENDIF
031900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
032000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032100020619     C                   EXSR      CHKNUM
032200020619     C                   IF        PiInt=*on
032300020619     C                   Z-ADD     PiVal         VABCTR
032400040506     C                   ENDIF
032500020619     C                   ENDIF
032600020619     C                   ENDIF
032700000801     C*
032800000801     C                   ENDSR
032900000801     C*----------------------------------------------------*
033000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033100000801     C*----------------------------------------------------*
033200040823     C     IMPVAB        BEGSR
033300010305     C*
033400000801     C                   Z-ADD     *zeros        errore            1 0
033500000830     C                   MOVEL     datcor        VABAAS
033600020305     C                   MOVEL     datcor        VATAAS
033700040526     C                   MOVE      datcor        VABMGS
033800040823     C                   MOVE(P)   vlrpoi        VABFGS
033900040823     C                   MOVE(P)   vlrpoi        VATFGS
034000050628     C*
034100080626     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
034200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034400020117     C* ==
034500080626     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
034600080626     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
034700080626     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
034800080626     C                   EVAL      VABRMA=%trim(%subst(vindta:187:6))
034900080626     C                   EVAL      VABNOT=%trim(%subst(vindta:92:15))
035000050628     C* CAP
035100080626     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
035200010201     C                   EXSR      CHKNUM
035300010201     C                   IF        PiInt=*on
035400010201     C                   Z-ADD     PiVal         Num5_0
035500040506     C                   MOVEL(P)  Num5_0        VABCAD
035600010201     C                   ELSE
035700040506     C                   ADD       1             errore
035800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035900040506     C                             + ' ' + 'VABCAD'
036000010201     C                   ENDIF
036100040506     C* Reperisco la provincia dal CAP e dalla località
036200040526     C                   IF        VABCAD <> *blanks AND
036300040526     C                             VABPRD  = *blanks
036400040506     C                   CLEAR                   TISI95DS
036500040506     C                   EVAL      I95TCN = '3'
036600040506     C                   Z-ADD     datcor        I95DAT
036700040506     C                   EVAL      I95CAP = VABCAD
036800040506     C                   EVAL      I95LOC = VABLOD
036900050627     C                   EVAL      I95NAR = VABNZD
037000040506     C                   CALL      'TISI95R'
037100040506     C                   PARM                    TISI95DS
037200040506     C                   EVAL      VABPRD = O95PRV
037300040506     C                   ENDIF
037400060307     C* NCL
037500080626     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
037600080626     C                   EXSR      CHKNUM
037700080626     C                   IF        PiInt=*on
037800080626     C                   ADD(H)    PiVal         VABNCL
037900080626     C                   ELSE
038000080626     C                   ADD       1             errore
038100080626     C                   EVAL      vinmsg = %trimr(vinmsg)
038200080626     C                             + ' ' + 'VABNCL'
038300080626     C                   ENDIF
038400040506     C* PKB
038500080626     C                   EVAL      PiStr=%trim(%subst(vindta:170:6))
038600010201     C                   EXSR      CHKNUM
038700010201     C                   IF        PiNum=*on
038800070927     C                   ADD(H)    PiVal         VABPKB
038900010201     C                   ELSE
039000010201     C                   ADD       1             errore
039100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
039200010201     C                             + ' ' + 'VABPKB'
039300010201     C                   ENDIF
039400071019     C* NSP/RMN
039500080626     C                   EVAL      PiStr=%trim(%subst(vindta:187:6))
039600071019     C                   EXSR      CHKNUM
039700071019     C                   IF        PiInt=*on
039800071019     C                   Z-ADD     PiVal         VABNSP
039900071019     C                   Z-ADD     PiVal         VATNSP
040000071019     C                   Z-ADD     PiVal         VABRMN
040100071019     C                   ELSE
040200071019     C                   ADD       1             errore
040300071019     C                   EVAL      vinmsg = %trimr(vinmsg)
040400071019     C                             + ' ' + 'VABNSP VATNSP VABRMN'
040500071019     C                   ENDIF
040600010205     C*
040700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
040800040802     C                   IF        FlgCAS <> '0'
040900070102     C                   IF        VABCBO = '1'
041000010205     C                   EVAL      VABCBO = '4'
041100010205     C                   ELSE
041200070102     C                   EVAL      VABCBO = '6'
041300070102     C                   ENDIF
041400010205     C                   ENDIF
041500020305     C*
041600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041700011113     C                   EXSR      CHKIMPDIV
041800010202     C*
041900000801     C* Ebbene...
042000000801     C                   ADD       1             §CTRMO
042100010201     C                   IF        errore <> *zeros
042200000801     C                   ADD       1             §CTRNO
042300000801     C                   EVAL      recko = vindta
042400000801     C                   EXCEPT    rigadett
042500000801     C                   EVAL      vinflg = '2'
042600000801     C                   ELSE
042700010201     C                   ADD       1             §CTROKVB
042800000801     C                   ENDIF
042900000801     C*
043000000801     C                   ENDSR
043100070102     C*----------------------------------------------------*
043200070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
043300070102     C*----------------------------------------------------*
043400070103     C     EXEVATE       BEGSR
043500070102     C*
043600071019     C*
043700070102     C                   exsr      wrivat                                       => scarico VAT
043800070102     C*
043900070102     C                   ENDSR
044000010201     C*----------------------------------------------------*
044100040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044200010201     C*----------------------------------------------------*
044300020305     C     WRIVAT        BEGSR
044400050628     C*
044500060223     C* Scrivo solo se valorizzato qualcosa
044600060223     C                   IF        VATNOT <> *blanks
044700040802     C                   WRITE     FIVAT000
044800060223     C                   ENDIF
044900010201     C*
045000010201     C                   ENDSR
045100010202     C*----------------------------------------------------*
045200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045300010202     C*----------------------------------------------------*
045400020305     C     PREVAT        BEGSR
045500010202     C*
045600021113     C* Compongo il nome del membro da dare al FIVATWWR
045700010202     C                   eval      parmbr = vlrhdl
045800010202     C                   movel     'M'           parmbr
045900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
046000010202     C                   eval      paropz = '1'
046100010202     C* Effettuo la chiamata al CLLE preposto
046200040506     C                   call(e)   'TITVVTC'
046300010202     C                   parm                    parccm
046400010202     C                   parm                    parmbr
046500010202     C                   parm                    paropz
046600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046700010202     C                   if        %error
046800010202     C                   movel     '1'           chkcall
046900010202     C                   else
047000010202     C                   movel     '0'           chkcall
047100010202     C                   endif
047200010202     C*
047300010202     C                   ENDSR
047400080626
047500080626
047600080626
047700080626     C*----------------------------------------------------*
047800080626     C*  CONTROLLO NUMERICITA' CAMPI
047900080626     C*----------------------------------------------------*
048000080626     C     CHKNUM        BEGSR
048100080626     C*
048200080626     C                   IF        PiDecChr = *blanks
048300080626     C                   EVAL      PiDecChr = '.'
048400080626     C                   ENDIF
048500080626     C*
048600080626     C                   callp     UBISNUM_Check(PiStr
048700080626     C                                          :PiDecChr
048800080626     C                                          :PiVal
048900080626     C                                          :PiNum
049000080626     C                                          :PiInt)
049100080626     C*
049200080626     C                   ENDSR
049300080626     C***
049400000801
049500011113
049600011113     C*----------------------------------------------------*
049700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049800011113     C*----------------------------------------------------*
049900011113     C     CHKIMPDIV     BEGSR
050000011113     C*
050100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050200011113     C                   Z-ADD     *zeros        wrkDec            9 9
050300011113     C*
050400011113     C* Come prima cosa effettuo considerazioni sulla divisa
050500011113     C                   IF        vabIAS > *zeros
050600011113     C                   IF        vabVAS <> 'EUR'
050700011113     C                   EVAL      vabVAS =  'ITL'
050800011113     C                   ENDIF
050900011113     C                   ENDIF
051000011113     C*
051100011113     C                   IF        vabCAS > *zeros
051200011113     C                   IF        vabVCA <> 'EUR'
051300011113     C                   EVAL      vabVCA =  'ITL'
051400011113     C                   ENDIF
051500011113     C                   ENDIF
051600011113     C*
051700011113     C                   IF        vabVMD > *zeros
051800020305     C                   IF        vabVAD <> 'EUR'
051900011113     C                   EVAL      vabVAD =  'ITL'
052000011113     C                   ENDIF
052100011113     C                   ENDIF
052200011113     C*
052300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052400011113     C                   Z-ADD     vabIAS        wrkDec
052500011113     C                   IF        wrkDec > *zeros
052600011113     C                   IF        vabVAS = 'ITL'
052700011113     C                   EVAL      vabIAS = *zeros
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
053200011113     C                   Z-ADD     vabCAS        wrkDec
053300011113     C                   IF        wrkDec > *zeros
053400011113     C                   IF        vabVCA = 'ITL'
053500011113     C                   EVAL      vabCAS = *zeros
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054000011113     C                   Z-ADD     vabVMD        wrkDec
054100011113     C                   IF        wrkDec > *zeros
054200011113     C                   IF        vabVAD = 'ITL'
054300011113     C                   EVAL      vabVMD = *zeros
054400011113     C                   ENDIF
054500011113     C                   ENDIF
054600011113     C*
054700011113     C                   ENDSR
054800011113     C***
054900011113
055000011113
055100000801
055200000801
055300990920      /TITLE Invio dei dati al punto operativo.
055400010202     C     invio         BEGSR
055500990920     C*
055600021113     C* 1° invio FIVAT
055700010201     C                   reset                   dscmz
055800010201     C                   move      vlrpoi        cmzdst
055900021113     C                   eval      cmzfld = 'FIVATWWR'
056000010201     C                   eval      cmzmbd = vlrhdl
056100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056200021009     C***                if        prmfir = *blanks
056300021113     C                   eval      cmzfla = 'FIVAT00F'
056400021113     C                   eval      cmzmba = 'FIVAT00F'
056500021009     C***                else
056600021009     C***                eval      cmzfla = prmfir
056700021009     C***                eval      cmzmba = prmfir
056800021009     C***                endif
056900010201     C                   eval      cmznrr = *zeros
057000020305     C                   move      §ctrokvt      cmznrr
057100021018     C                   eval      cmzlba = vlrfl1
057200010201     C                   call(e)   'TIS711C'
057300010201     C                   parm                    dscmz
057400010201     C                   parm      *blanks       esito
057500010205     C                   if        %error
057600010205     C                             or cmzerr = '1'
057700010205     C                             or esito  = '1'
057800010205     C                   eval      wrkesito = '3'
057900010205     C                   else
058000010201     C*
058100021113     C* 2° invio FIVAB
058200010201     C                   reset                   dscmz
058300010201     C                   move      vlrpoi        cmzdst
058400010201     C                   eval      cmzfld = vlrfou
058500010201     C                   eval      cmzmbd = vlrhdl
058600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058700021009     C***                if        prmfir = *blanks
058800021113     C                   eval      cmzfla = 'FIVAB00F'
058900021113     C                   eval      cmzmba = 'FIVAB00F'
059000021009     C***                else
059100021009     C***                eval      cmzfla = prmfir
059200021009     C***                eval      cmzmba = prmfir
059300021009     C***                endif
059400010201     C                   eval      cmznrr = *zeros
059500010201     C                   move      §ctrokvb      cmznrr
059600021018     C                   eval      cmzlba = vlrfl1
059700010201     C                   call(e)   'TIS711C'
059800010201     C                   parm                    dscmz
059900010201     C                   parm      *blanks       esito
060000010201     C                   if        %error
060100010201     C                             or cmzerr = '1'
060200010201     C                             or esito  = '1'
060300010201     C                   eval      wrkesito = '3'
060400010201     C                   endif
060500010205     C                   endif
060600990920     C*
060700000613     C                   ENDSR
060800000613     C***
060900070411
061000070411     C     *pssr         BEGSR
061100070411     C*
061200070411     C                   if        %open(tivin00r)
061300070411     C                   close     tivin00r
061400070411     C                   endif
061500070411     C                   if        %open(fivabwwr)
061600070411     C                   close     fivabwwr
061700070411     C                   endif
061800070411     C                   if        %open(fivatwwr)
061900070411     C                   close     fivatwwr
062000070411     C                   endif
062100070411     C*
062200070411     C* Effettuo la chiamata al CLLE preposto
062300070411     C                   call(e)   'TITVVTC'
062400070411     C                   parm                    parccm
062500070411     C                   parm                    parmbr
062600070411     C                   parm      '2'           paropz
062700070411     C*
062800070411     C                   eval      wrkesito = '2'
062900070411     C*
063000070411     C                   seton                                        LR
063100070411     C*
063200070411     C                   ENDSR     '*CANCL'
063300070411     C***
063400070411
063500990910
063600000613     C     *inzsr        BEGSR
063700990910     C*
063800990910     C     *entry        plist
063900990920     C                   parm                    tivlrds
064000990921     C                   parm      wrkesito      esito
064100000724     C                   parm                    prmlit
064200000710     C                   parm                    prmfir
064300000613     C*
064400000830     C* CALCOLA LA DATA CORRENTE
064500000830     C                   time                    wn14             14 0
064600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064700000830     C                   z-add     wn8           g08dat
064800000830     C                   z-add     *zeros        g08inv
064900000830     C                   movel     '0'           g08err
065000000830     C                   call      'XSRDA8'
065100000830     C                   parm                    wlbda8
065200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065300000830     C*
065400000613     C                   ENDSR
065500000613     C***
065600990908
065700080626     Otitv1h4p  E            riepilogo         2
065800990915     O                                              'Upload via Internet'
065900990915     O                                           +1 'Traduzione TIVIN00R -'
066000021113     O                                           +1 'FIVABWWR/FIVATWWR'
066100010201     O                                           +1 'Report testate bolle'
066200990915     O          E            riepilogo   2
066300990915     O                       wrkdata
066400990915     O                       wrkora              +1
066500990915     O                       procname            +1
066600990915     O          E            riepilogo   2
066700990915     O                                              'Cliente..................:'
066800990915     O                       VABCCM        z     +1
066900990915     O          E            riepilogo   2
067000990920     O                                              'Riferimento Strategi.....:'
067100990920     O                       vlrhdl              +1
067200990915     O          E            riepilogo   2
067300990915     O                                              'Giusti...................:'
067400010201     O                       §CTROKVB      2   +  1
067500990915     O          E            riepilogo   2
067600990915     O                                              'Sbagliati e corretti.....:'
067700971022     O                       §CTRMO        2   +  1
067800990915     O          E            riepilogo   2
067900990915     O                                              'Sbagliati e scartati.....:'
068000971022     O                       §CTRNO        2   +  1
068100000613
068200080626     Otitv1h4ps E            testdett          2
068300000613     O                                              'Upload via Internet'
068400000613     O                                           +1 'Traduzione TIVIN00R -'
068500021113     O                                           +1 'FIVABWWR/FIVATWWR'
068600010201     O                                           +1 'Report testate bolle'
068700000616     O          E            testdett    3
068800000613     O                                           +2 'N° rec'
068900000613     O                                           +3 'Anteprima contenuto'
069000000616     O          E            rigadett    2
069100000613     O                       rrnum               +2
069200000621     O                       recko               +3
069300000616     O          E            findett     2
069400000613     O                       wrkdata
069500000613     O                       wrkora              +1
069600000613     O                       procname            +1
069700000616     O          E            findett     2
069800000613     O                                              'Cliente..................:'
069900000613     O                       VABCCM        z     +1
070000000616     O          E            findett     2
070100000613     O                                              'Riferimento Strategi.....:'
070200000613     O                       vlrhdl              +1
070300000616     O          E            findett     2
070400000613     O                                              'Giusti...................:'
070500010201     O                       §CTROKVB      2   +  1
070600000616     O          E            findett     2
070700000613     O                                              'Sbagliati e corretti.....:'
070800000613     O                       §CTRMO        2   +  1
070900000616     O          E            findett     2
071000000613     O                                              'Sbagliati e scartati.....:'
071100000613     O                       §CTRNO        2   +  1
071200000512** CMD - COMANDI CL
071300080626OVRPRTF FILE(TITV1H4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071400080626OVRPRTF FILE(TITV1H4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071500080626DLTOVR FILE(TITV1H4P TITV1H4PS) LVL(*)
071600000512
071700000512
