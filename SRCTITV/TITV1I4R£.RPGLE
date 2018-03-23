000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800080807     Ftitv1i4p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100080807     Ftitv1i4ps O    f  198        PRINTER usropn
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
004000080807     D curSped         s             15    INZ(*blanks)
004100080807     D depSped         s             15    INZ(*blanks)
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
010200080807     C                   if        not %open(titv1i4ps)
010300080807     C                   open      titv1i4ps
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
012200080807     C                   if        %open(titv1i4ps)
012300000616     C                   except    findett
012400080807     C                   close     titv1i4ps
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
014200080807     C                   if        not %open(titv1i4p)
014300080807     C                   open      titv1i4p
014400990915     C                   endif
014500990915     C*
014600990915     C                   except    riepilogo
014700990915     C*
014800080807     C                   if        %open(titv1i4p)
014900080807     C                   close     titv1i4p
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
019800080807     C                   eval      curSped = %subst(vindta:175:15)
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
029400080807     C                   Z-ADD     0053428       VABCCM
029500080807     C                   Z-ADD     0053428       VATCCM
029600080807     C                   Z-ADD     005           VABLNP
029700080807     C                   Z-ADD     005           VATLNP
029800070927     C                   Z-ADD     000           VABCTR
029900040823     C                   MOVEL     '1'           VABCBO
030000080807     C                   MOVEL     '7Q'          VABCTM
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
034100080807     C                   EVAL      VABRSD=%trim(%subst(vindta:21:35))
034200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034400020117     C* ==
034500080807     C                   EVAL      VABRD2=%trim(%subst(vindta:56:35))
034600080807     C                   EVAL      VABIND=%trim(%subst(vindta:91:35))
034700080807     C                   EVAL      VABLOD=%trim(%subst(vindta:135:35))
034800080807     C                   EVAL      VABPRD=%trim(%subst(vindta:170:2))
034900080807     C                   EVAL      VABRMA=%trim(%subst(vindta:175:15))
035000050628     C* CAP
035100080807     C                   EVAL      PiStr=%trim(%subst(vindta:126:9))
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
037500080807     C                   EVAL      PiStr=%trim(%subst(vindta:225:5))
037600080626     C                   EXSR      CHKNUM
037700080626     C                   IF        PiInt=*on
037800080626     C                   ADD(H)    PiVal         VABNCL
037900080626     C                   ELSE
038000080626     C                   ADD       1             errore
038100080626     C                   EVAL      vinmsg = %trimr(vinmsg)
038200080626     C                             + ' ' + 'VABNCL'
038300080626     C                   ENDIF
038400040506     C* PKB
038500080807     C                   EVAL      PiStr=%trim(%subst(vindta:230:7))
038600010201     C                   EXSR      CHKNUM
038700010201     C                   IF        PiNum=*on
038800080807     C                   EVAL      PiVal=PiVal/10                               * gestisco 1 dec.
038900070927     C                   ADD(H)    PiVal         VABPKB
039000010201     C                   ELSE
039100010201     C                   ADD       1             errore
039200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
039300010201     C                             + ' ' + 'VABPKB'
039400010201     C                   ENDIF
039500080807     C* RMN/RMN
039600080807     C                   EVAL      PiStr=%trim(%subst(vindta:175:15))
039700080807     C                   EXSR      CHKNUM
039800080807     C                   IF        PiInt=*on
039900080807     C                   Z-ADD     PiVal         VABNSP
040000080807     C                   Z-ADD     PiVal         VATNSP
040100080807     C                   Z-ADD     PiVal         VABRMN
040200080807     C                   ELSE
040300080807     C                   ADD       1             errore
040400080807     C                   EVAL      vinmsg = %trimr(vinmsg)
040500080807     C                             + ' ' + 'VABNSP VATNSP VABRMN'
040600080807     C                   ENDIF
040700010205     C*
040800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
040900040802     C                   IF        FlgCAS <> '0'
041000070102     C                   IF        VABCBO = '1'
041100010205     C                   EVAL      VABCBO = '4'
041200010205     C                   ELSE
041300070102     C                   EVAL      VABCBO = '6'
041400070102     C                   ENDIF
041500010205     C                   ENDIF
041600020305     C*
041700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041800011113     C                   EXSR      CHKIMPDIV
041900010202     C*
042000000801     C* Ebbene...
042100000801     C                   ADD       1             §CTRMO
042200010201     C                   IF        errore <> *zeros
042300000801     C                   ADD       1             §CTRNO
042400000801     C                   EVAL      recko = vindta
042500000801     C                   EXCEPT    rigadett
042600000801     C                   EVAL      vinflg = '2'
042700000801     C                   ELSE
042800010201     C                   ADD       1             §CTROKVB
042900000801     C                   ENDIF
043000000801     C*
043100000801     C                   ENDSR
043200070102     C*----------------------------------------------------*
043300070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
043400070102     C*----------------------------------------------------*
043500070103     C     EXEVATE       BEGSR
043600070102     C*
043700080807     C                   EVAL      VATNOT=%trim(%subst(vindta:190:7))
043800080807     C                   EVAL      VATTRC='E'
043900071019     C*
044000070102     C                   exsr      wrivat                                       => scarico VAT
044100070102     C*
044200070102     C                   ENDSR
044300010201     C*----------------------------------------------------*
044400040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044500010201     C*----------------------------------------------------*
044600020305     C     WRIVAT        BEGSR
044700050628     C*
044800060223     C* Scrivo solo se valorizzato qualcosa
044900060223     C                   IF        VATNOT <> *blanks
045000040802     C                   WRITE     FIVAT000
045100060223     C                   ENDIF
045200010201     C*
045300010201     C                   ENDSR
045400010202     C*----------------------------------------------------*
045500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045600010202     C*----------------------------------------------------*
045700020305     C     PREVAT        BEGSR
045800010202     C*
045900021113     C* Compongo il nome del membro da dare al FIVATWWR
046000010202     C                   eval      parmbr = vlrhdl
046100010202     C                   movel     'M'           parmbr
046200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
046300010202     C                   eval      paropz = '1'
046400010202     C* Effettuo la chiamata al CLLE preposto
046500040506     C                   call(e)   'TITVVTC'
046600010202     C                   parm                    parccm
046700010202     C                   parm                    parmbr
046800010202     C                   parm                    paropz
046900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047000010202     C                   if        %error
047100010202     C                   movel     '1'           chkcall
047200010202     C                   else
047300010202     C                   movel     '0'           chkcall
047400010202     C                   endif
047500010202     C*
047600010202     C                   ENDSR
047700080626
047800080626
047900080626
048000080626     C*----------------------------------------------------*
048100080626     C*  CONTROLLO NUMERICITA' CAMPI
048200080626     C*----------------------------------------------------*
048300080626     C     CHKNUM        BEGSR
048400080626     C*
048500080626     C                   IF        PiDecChr = *blanks
048600080626     C                   EVAL      PiDecChr = '.'
048700080626     C                   ENDIF
048800080626     C*
048900080626     C                   callp     UBISNUM_Check(PiStr
049000080626     C                                          :PiDecChr
049100080626     C                                          :PiVal
049200080626     C                                          :PiNum
049300080626     C                                          :PiInt)
049400080626     C*
049500080626     C                   ENDSR
049600080626     C***
049700000801
049800011113
049900011113     C*----------------------------------------------------*
050000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050100011113     C*----------------------------------------------------*
050200011113     C     CHKIMPDIV     BEGSR
050300011113     C*
050400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050500011113     C                   Z-ADD     *zeros        wrkDec            9 9
050600011113     C*
050700011113     C* Come prima cosa effettuo considerazioni sulla divisa
050800011113     C                   IF        vabIAS > *zeros
050900011113     C                   IF        vabVAS <> 'EUR'
051000011113     C                   EVAL      vabVAS =  'ITL'
051100011113     C                   ENDIF
051200011113     C                   ENDIF
051300011113     C*
051400011113     C                   IF        vabCAS > *zeros
051500011113     C                   IF        vabVCA <> 'EUR'
051600011113     C                   EVAL      vabVCA =  'ITL'
051700011113     C                   ENDIF
051800011113     C                   ENDIF
051900011113     C*
052000011113     C                   IF        vabVMD > *zeros
052100020305     C                   IF        vabVAD <> 'EUR'
052200011113     C                   EVAL      vabVAD =  'ITL'
052300011113     C                   ENDIF
052400011113     C                   ENDIF
052500011113     C*
052600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052700011113     C                   Z-ADD     vabIAS        wrkDec
052800011113     C                   IF        wrkDec > *zeros
052900011113     C                   IF        vabVAS = 'ITL'
053000011113     C                   EVAL      vabIAS = *zeros
053100011113     C                   ENDIF
053200011113     C                   ENDIF
053300011113     C*
053400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
053500011113     C                   Z-ADD     vabCAS        wrkDec
053600011113     C                   IF        wrkDec > *zeros
053700011113     C                   IF        vabVCA = 'ITL'
053800011113     C                   EVAL      vabCAS = *zeros
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054300011113     C                   Z-ADD     vabVMD        wrkDec
054400011113     C                   IF        wrkDec > *zeros
054500011113     C                   IF        vabVAD = 'ITL'
054600011113     C                   EVAL      vabVMD = *zeros
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C                   ENDSR
055100011113     C***
055200011113
055300011113
055400000801
055500000801
055600990920      /TITLE Invio dei dati al punto operativo.
055700010202     C     invio         BEGSR
055800990920     C*
055900021113     C* 1° invio FIVAT
056000010201     C                   reset                   dscmz
056100010201     C                   move      vlrpoi        cmzdst
056200021113     C                   eval      cmzfld = 'FIVATWWR'
056300010201     C                   eval      cmzmbd = vlrhdl
056400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056500021009     C***                if        prmfir = *blanks
056600021113     C                   eval      cmzfla = 'FIVAT00F'
056700021113     C                   eval      cmzmba = 'FIVAT00F'
056800021009     C***                else
056900021009     C***                eval      cmzfla = prmfir
057000021009     C***                eval      cmzmba = prmfir
057100021009     C***                endif
057200010201     C                   eval      cmznrr = *zeros
057300020305     C                   move      §ctrokvt      cmznrr
057400021018     C                   eval      cmzlba = vlrfl1
057500010201     C                   call(e)   'TIS711C'
057600010201     C                   parm                    dscmz
057700010201     C                   parm      *blanks       esito
057800010205     C                   if        %error
057900010205     C                             or cmzerr = '1'
058000010205     C                             or esito  = '1'
058100010205     C                   eval      wrkesito = '3'
058200010205     C                   else
058300010201     C*
058400021113     C* 2° invio FIVAB
058500010201     C                   reset                   dscmz
058600010201     C                   move      vlrpoi        cmzdst
058700010201     C                   eval      cmzfld = vlrfou
058800010201     C                   eval      cmzmbd = vlrhdl
058900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059000021009     C***                if        prmfir = *blanks
059100021113     C                   eval      cmzfla = 'FIVAB00F'
059200021113     C                   eval      cmzmba = 'FIVAB00F'
059300021009     C***                else
059400021009     C***                eval      cmzfla = prmfir
059500021009     C***                eval      cmzmba = prmfir
059600021009     C***                endif
059700010201     C                   eval      cmznrr = *zeros
059800010201     C                   move      §ctrokvb      cmznrr
059900021018     C                   eval      cmzlba = vlrfl1
060000010201     C                   call(e)   'TIS711C'
060100010201     C                   parm                    dscmz
060200010201     C                   parm      *blanks       esito
060300010201     C                   if        %error
060400010201     C                             or cmzerr = '1'
060500010201     C                             or esito  = '1'
060600010201     C                   eval      wrkesito = '3'
060700010201     C                   endif
060800010205     C                   endif
060900990920     C*
061000000613     C                   ENDSR
061100000613     C***
061200070411
061300070411     C     *pssr         BEGSR
061400070411     C*
061500070411     C                   if        %open(tivin00r)
061600070411     C                   close     tivin00r
061700070411     C                   endif
061800070411     C                   if        %open(fivabwwr)
061900070411     C                   close     fivabwwr
062000070411     C                   endif
062100070411     C                   if        %open(fivatwwr)
062200070411     C                   close     fivatwwr
062300070411     C                   endif
062400070411     C*
062500070411     C* Effettuo la chiamata al CLLE preposto
062600070411     C                   call(e)   'TITVVTC'
062700070411     C                   parm                    parccm
062800070411     C                   parm                    parmbr
062900070411     C                   parm      '2'           paropz
063000070411     C*
063100070411     C                   eval      wrkesito = '2'
063200070411     C*
063300070411     C                   seton                                        LR
063400070411     C*
063500070411     C                   ENDSR     '*CANCL'
063600070411     C***
063700070411
063800990910
063900000613     C     *inzsr        BEGSR
064000990910     C*
064100990910     C     *entry        plist
064200990920     C                   parm                    tivlrds
064300990921     C                   parm      wrkesito      esito
064400000724     C                   parm                    prmlit
064500000710     C                   parm                    prmfir
064600000613     C*
064700000830     C* CALCOLA LA DATA CORRENTE
064800000830     C                   time                    wn14             14 0
064900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
065000000830     C                   z-add     wn8           g08dat
065100000830     C                   z-add     *zeros        g08inv
065200000830     C                   movel     '0'           g08err
065300000830     C                   call      'XSRDA8'
065400000830     C                   parm                    wlbda8
065500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065600000830     C*
065700000613     C                   ENDSR
065800000613     C***
065900990908
066000080807     Otitv1i4p  E            riepilogo         2
066100990915     O                                              'Upload via Internet'
066200990915     O                                           +1 'Traduzione TIVIN00R -'
066300021113     O                                           +1 'FIVABWWR/FIVATWWR'
066400010201     O                                           +1 'Report testate bolle'
066500990915     O          E            riepilogo   2
066600990915     O                       wrkdata
066700990915     O                       wrkora              +1
066800990915     O                       procname            +1
066900990915     O          E            riepilogo   2
067000990915     O                                              'Cliente..................:'
067100990915     O                       VABCCM        z     +1
067200990915     O          E            riepilogo   2
067300990920     O                                              'Riferimento Strategi.....:'
067400990920     O                       vlrhdl              +1
067500990915     O          E            riepilogo   2
067600990915     O                                              'Giusti...................:'
067700010201     O                       §CTROKVB      2   +  1
067800990915     O          E            riepilogo   2
067900990915     O                                              'Sbagliati e corretti.....:'
068000971022     O                       §CTRMO        2   +  1
068100990915     O          E            riepilogo   2
068200990915     O                                              'Sbagliati e scartati.....:'
068300971022     O                       §CTRNO        2   +  1
068400000613
068500080807     Otitv1i4ps E            testdett          2
068600000613     O                                              'Upload via Internet'
068700000613     O                                           +1 'Traduzione TIVIN00R -'
068800021113     O                                           +1 'FIVABWWR/FIVATWWR'
068900010201     O                                           +1 'Report testate bolle'
069000000616     O          E            testdett    3
069100000613     O                                           +2 'N° rec'
069200000613     O                                           +3 'Anteprima contenuto'
069300000616     O          E            rigadett    2
069400000613     O                       rrnum               +2
069500000621     O                       recko               +3
069600000616     O          E            findett     2
069700000613     O                       wrkdata
069800000613     O                       wrkora              +1
069900000613     O                       procname            +1
070000000616     O          E            findett     2
070100000613     O                                              'Cliente..................:'
070200000613     O                       VABCCM        z     +1
070300000616     O          E            findett     2
070400000613     O                                              'Riferimento Strategi.....:'
070500000613     O                       vlrhdl              +1
070600000616     O          E            findett     2
070700000613     O                                              'Giusti...................:'
070800010201     O                       §CTROKVB      2   +  1
070900000616     O          E            findett     2
071000000613     O                                              'Sbagliati e corretti.....:'
071100000613     O                       §CTRMO        2   +  1
071200000616     O          E            findett     2
071300000613     O                                              'Sbagliati e scartati.....:'
071400000613     O                       §CTRNO        2   +  1
071500000512** CMD - COMANDI CL
071600080807OVRPRTF FILE(TITV1I4P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071700080807OVRPRTF FILE(TITV1I4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071800080807DLTOVR FILE(TITV1I4P TITV1I4PS) LVL(*)
071900000512
072000000512
