000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800050216     Ftitvh9p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050216     Ftitvh9ps  O    f  198        PRINTER usropn
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
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500010202     D parccm          s              8    INZ(*blanks)
003600010202     D parmbr          s             10    INZ(*blanks)
003700010202     D paropz          s              1    INZ(*blanks)
003800010202     D chkcall         s              1    INZ(*blanks)
003900000830
004000000830     D*------------------
004100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004200000830     D*------------------
004300000830     D WLBDA8          DS                  INZ
004400000830     D  G08DAT                 1      8  0
004500000830     D  G08INV                 9     16  0
004600000830     D  G08ERR                17     17
004700000830     D  G08TGI                18     22  0
004800000830     D*
004900990908
005000010201
005100010201
005200990915     C                   time                    wrkdata
005300990915     C                   time                    wrkora
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000724     C*
005800000724     C* SE OCCORRE SPEDIRE IN FILIALE
005900000724     C                   if        vlrpoi <> *zeros
006000000724     C*
006100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006300000724     C     vlrpoi        chain     azorg01l
006400000724     C                   if        %found
006500000616     C                   movel(p)  CMD(1)        depcmd
006600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006700000616     C*
006800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006900011113     C                   Z-ADD     150           LENGH            15 5
007000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007100000616     C                   PARM                    depcmd
007200000616     C                   PARM                    LENGH
007300000724     C*
007400000724     C                   endif
007500000724     C                   endif
007600000616     C*
007700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007800000616     C                   movel(p)  CMD(2)        depcmd
007900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008000000616     C*
008100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008200011113     C                   Z-ADD     150           LENGH            15 5
008300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008400000616     C                   PARM                    depcmd
008500000616     C                   PARM                    LENGH
008600000616     C*
008700050216     C                   if        not %open(titvh9ps)
008800050216     C                   open      titvh9ps
008900000616     C                   except    testdett
009000000613     C                   endif
009100000613     C*
009200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009400000613     C*
009500010202     C* Effettuo la chiamata al CLLE preposto
009600040506     C                   call(e)   'TITVVTC'
009700010202     C                   parm                    parccm
009800010202     C                   parm                    parmbr
009900010202     C                   parm      '2'           paropz
010000010202     C*
010100050216     C                   if        %open(titvh9ps)
010200000616     C                   except    findett
010300050216     C                   close     titvh9ps
010400000613     C                   endif
010500000616     C*
010600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
010700011113     C                   Z-ADD     150           LENGH            15 5
010800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010900000616     C                   PARM                    CMD(3)
011000000616     C                   PARM                    LENGH
011100000616     C*
011200000801     C
011300010201     C                   seton                                        LR
011400000613
011500000613
011600000613     C*--------------------------------------------------------
011700000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
011800000613     C*--------------------------------------------------------
011900000621     C     STPR          BEGSR
012000000613     C*
012100050216     C                   if        not %open(titvh9p)
012200050216     C                   open      titvh9p
012300990915     C                   endif
012400990915     C*
012500990915     C                   except    riepilogo
012600990915     C*
012700050216     C                   if        %open(titvh9p)
012800050216     C                   close     titvh9p
012900990914     C                   endif
013000990910     C*
013100000613     C                   ENDSR
013200000613     C***
013300990908
013400000801
013500910830     C*--------------------------------------------------------
013600040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
013700910830     C*--------------------------------------------------------
013800040526     C     RWFILE        BEGSR
013900990910     C*
014000990914     C                   if        not %open(tivin00r)
014100990908     C                   open      tivin00r
014200990914     C                   endif
014300021113     C                   if        not %open(fivabwwr)
014400021113     C                   open      fivabwwr
014500990914     C                   endif
014600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014700020305     C                   exsr      prevat
014800010201     C*
014900010202     C                   if        chkcall = '0'
015000010202     C*
015100021113     C                   if        not %open(fivatwwr)
015200021113     C                   open      fivatwwr
015300010201     C                   endif
015400990910     C*
015500010201     C                   clear                   §CTROKVB          5 0
015600020305     C                   clear                   §CTROKVT          5 0
015700000801     C                   clear                   §CTRMO            5 0
015800000801     C                   clear                   §CTRNO            5 0
015900990910     C*
016000921023     C                   DO        *HIVAL
016100990913     C*
016200990915     C                   READ      tivin00r                               70
016300000905     C                   if        vindta > *blanks
016400000613     C                   add       1             rrnum
016500000801     C*
016600000801     C                   if        *in70 = *off
016700000801     C                             and
016800000801     C                             (vinflg = *blanks
016900000801     C                              or vinflg = '0'
017000000801     C                              or vinflg = '2')
017100000801     C*
017200000801     C                   clear                   vinmsg
017300000801     C                   eval      vinflg = '1'
017400010305     C*
017500040802     C                   exsr      impvab
017600040802     C                   exsr      wrivab
017700020305     C                   exsr      wrivat                                       => carico VAT
017800040802     C*
017900010305     C                   endif
018000000905     C*
018100000905     C                   else
018200000905     C                   eval      vinflg = '1'
018300000905     C                   endif
018400000905     C*
018500000905     C  N70              update    tivin000
018600000905     C*
018700991022     C  N70              ENDdo
018800010202     C*
018900010202     C                   endif
019000990910
019100990910     C* Se non ci sono record con errori ...
019200000710     C                   if        §ctrno = 0
019300990910     C* ... restituisco esito OK.
019400990921     C                   eval      wrkesito = '0'
019500990910     C                   else
019600010201     C                   if        §ctrokvb > 0
019700990921     C                   eval      wrkesito = '1'
019800000710     C                   else
019900000710     C                   eval      wrkesito = '2'
020000990910     C                   endif
020100000710     C                   endif
020200990910     C*
020300990914     C                   if        %open(tivin00r)
020400990908     C                   close     tivin00r
020500990914     C                   endif
020600021113     C                   if        %open(fivabwwr)
020700021113     C                   close     fivabwwr
020800990914     C                   endif
020900021113     C                   if        %open(fivatwwr)
021000021113     C                   close     fivatwwr
021100010201     C                   endif
021200990910     C*
021300010201     C                   if        §ctrokvb > 0
021400000724     C                             and vlrpoi <> *zeros
021500010202     C                   exsr      invio
021600990920     C                   endif
021700990920     C*
021800910830     C                   ENDSR
021900000613     C***
022000010305
022100010305     C*----------------------------------------------------*
022200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022300010305     C*----------------------------------------------------*
022400020305     C     WRIVAB        BEGSR
022500010305     C*
022600021113     C                   write     fivab000                                     => scarico il VAB
022700010305     C*
022800010305     C                   ENDSR
022900990920
023000000801     C*----------------------------------------------------*
023100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023200000801     C*----------------------------------------------------*
023300010201     C     INZVAR        BEGSR
023400000801     C*
023500040802     C                   Z-ADD     *zeros        Num5_0            5 0
023600040802     C                   MOVEL     '0'           FlgCAS            1
023700000801     C*
023800000801     C                   ENDSR
023900000801     C*----------------------------------------------------*
024000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024100000801     C*----------------------------------------------------*
024200000801     C     DEFCAM        BEGSR
024300000801     C*
024400021113     C                   CLEAR                   FIVAB000
024500040802     C                   CLEAR                   FIVAT000
024600020619     C* Imposto i valori di default...
024700050216     C                   Z-ADD     0502599       VABCCM
024800050216     C                   Z-ADD     0502599       VATCCM
024900050216     C                   Z-ADD     050           VABLNP
025000050216     C                   Z-ADD     050           VATLNP
025100040714     C                   Z-ADD     000           VABCTR
025200020619     C* ... e poi verifico se sono stati passati come parametri
025300020619     C                   IF        vlrppt > *blanks
025400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
025500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025600020619     C                   EXSR      CHKNUM
025700020619     C                   IF        PiInt=*on
025800020619     C                   Z-ADD     PiVal         VABCCM
025900020619     C                   Z-ADD     PiVal         VATCCM
026000020619     C                   ENDIF
026100040506     C                   ENDIF
026200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
026300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026400020619     C                   EXSR      CHKNUM
026500020619     C                   IF        PiInt=*on
026600020619     C                   Z-ADD     PiVal         VABLNP
026700020619     C                   Z-ADD     PiVal         VATLNP
026800040506     C                   ENDIF
026900020619     C                   ENDIF
027000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027200020619     C                   EXSR      CHKNUM
027300020619     C                   IF        PiInt=*on
027400020619     C                   Z-ADD     PiVal         VABCTR
027500040506     C                   ENDIF
027600020619     C                   ENDIF
027700020619     C                   ENDIF
027800000801     C*
027900000801     C                   ENDSR
028000000801     C*----------------------------------------------------*
028100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028200000801     C*----------------------------------------------------*
028300010201     C     IMPVAB        BEGSR
028400000801     C*
028500020305     C                   EXSR      INZVAR
028600020305     C                   EXSR      DEFCAM
028700010305     C*
028800000801     C                   Z-ADD     *zeros        errore            1 0
028900000830     C                   MOVEL     datcor        VABAAS
029000020305     C                   MOVEL     datcor        VATAAS
029100040526     C                   MOVE      datcor        VABMGS
029200021113     C                   MOVE(P)   vlrpoi        VABFGS
029300021113     C                   MOVE(P)   vlrpoi        VATFGS
029400050216     C*
029500050216     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
029600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
029800020117     C* ==
029900050216     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
030000041201     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
030100041201     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
030200050216     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
030300050216     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
030400050216     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
030500050216     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:35))
030600050520     C                   EVAL      VABRMO=%trim(%subst(vindta:362:4))
030700110218     C* TC1
030800110218     C                   IF        %subst(vindta:13:1) = 'A'
030900110218     C                   EVAL      VABTC1='A'
031000110218     C                   ENDIF
031100110218     C* TC2
031200110218     C                   IF        %subst(vindta:15:1) = 'P'
031300110218     C                   EVAL      VABTC2='P'
031400110218     C                   ENDIF
031500040506     C* CAD
031600041201     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
031700010201     C                   EXSR      CHKNUM
031800010201     C                   IF        PiInt=*on
031900010201     C                   Z-ADD     PiVal         Num5_0
032000040506     C                   MOVEL(P)  Num5_0        VABCAD
032100010201     C                   ELSE
032200040506     C                   ADD       1             errore
032300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032400040506     C                             + ' ' + 'VABCAD'
032500010201     C                   ENDIF
032600050926     C* CCM
032700061106     C*                  EVAL      PiStr=%trim(%subst(vindta:1:7))
032800061106     C*                  EXSR      CHKNUM
032900061106     C*                  IF        PiInt=*on
033000061106     C*                  Z-ADD     PiVal         VABCCM
033100061106     C*                  ELSE
033200061106     C*                  ADD       1             errore
033300061106     C*                  EVAL      vinmsg = %trimr(vinmsg)
033400061106     C*                            + ' ' + 'VABCCM'
033500061106     C*                  ENDIF
033600041201     C* NCL
033700041201     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
033800040506     C                   EXSR      CHKNUM
033900040506     C                   IF        PiInt=*on
034000040506     C                   Z-ADD     PiVal         VABNCL
034100040506     C                   ELSE
034200040506     C                   ADD       1             errore
034300040506     C                   EVAL      vinmsg = %trimr(vinmsg)
034400040506     C                             + ' ' + 'VABNCL'
034500040506     C                   ENDIF
034600040506     C* PKB
034700050216     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
034800010201     C                   EXSR      CHKNUM
034900010201     C                   IF        PiNum=*on
035000011115     C                   Z-ADD     PiVal         VABPKB
035100010201     C                   ELSE
035200010201     C                   ADD       1             errore
035300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035400010201     C                             + ' ' + 'VABPKB'
035500010201     C                   ENDIF
035600041201     C* VLB
035700050216     C                   IF        %trim(%subst(vindta:308:7)) <> *blanks
035800050216     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
035900041201     C                   EXSR      CHKNUM
036000041201     C                   IF        PiNum=*on
036100090122     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
036200090122     C                   Z-ADD(H)  PiVal         VABVLB
036300041201     C                   ELSE
036400041201     C                   ADD       1             errore
036500041201     C                   EVAL      vinmsg = %trimr(vinmsg)
036600041201     C                             + ' ' + 'VABVLB'
036700041201     C                   ENDIF
036800050216     C                   ENDIF
036900040506     C* NSP/RMN
037000050216     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
037100010201     C                   EXSR      CHKNUM
037200010201     C                   IF        PiInt=*on
037300040506     C                   Z-ADD     PiVal         VABNSP
037400040506     C                   Z-ADD     PiVal         VATNSP
037500040506     C                   Z-ADD     PiVal         VABRMN
037600010201     C                   ELSE
037700010201     C                   ADD       1             errore
037800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037900040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
038000040526     C                   ENDIF
038100041201     C* CAS
038200050216     C                   IF        %trim(%subst(vindta:280:12)) <> *blanks
038300040802     C                   EVAL      FlgCAS = '1'
038400050216     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
038500050216     C                   EVAL      VABVCA = %trim(%subst(vindta:277:3))
038600040714     C                   EXSR      CHKNUM
038700040714     C                   IF        PiNum=*on
038800050216     C                   Z-ADD     PiVal         VABCAS
038900040714     C                   ELSE
039000040714     C                   ADD       1             errore
039100040714     C                   EVAL      vinmsg = %trimr(vinmsg)
039200040802     C                             + ' ' + 'VABCAS'
039300040714     C                   ENDIF
039400040802     C                   ENDIF
039500050216     C* IAS
039600050216     C                   IF        %trim(%subst(vindta:295:12)) <> *blanks
039700050216     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
039800050216     C                   EVAL      VABVAS = %trim(%subst(vindta:292:3))
039900050216     C                   EXSR      CHKNUM
040000050216     C                   IF        PiNum=*on
040100050216     C                   Z-ADD     PiVal         VABIAS
040200050216     C                   ELSE
040300050216     C                   ADD       1             errore
040400050216     C                   EVAL      vinmsg = %trimr(vinmsg)
040500050216     C                             + ' ' + 'VABIAS'
040600050216     C                   ENDIF
040700050216     C                   ENDIF
040800010205     C*
040900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041000040802     C                   IF        FlgCAS <> '0'
041100010205     C                   EVAL      VABCBO = '4'
041200010205     C                   ELSE
041300010205     C                   EVAL      VABCBO = '1'
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
043100010201     C*----------------------------------------------------*
043200040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043300010201     C*----------------------------------------------------*
043400020305     C     WRIVAT        BEGSR
043500040802     C*
043600040802     C* Valorizzo i campi chiave
043700041201     C                   EVAL      VATTRC = 'B'
043800040802     C*
043900041201     C* Compongo i barcode e scarico il buffer del FIVAT
044000050216     C                   EVAL      VATNOT = %subst(vindta:92:15)
044100050216     C                   IF        VATNOT <> *blanks
044200040802     C                   WRITE     FIVAT000
044300050216     C                   ENDIF
044400010201     C*
044500010201     C                   ENDSR
044600010202     C*----------------------------------------------------*
044700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044800010202     C*----------------------------------------------------*
044900020305     C     PREVAT        BEGSR
045000010202     C*
045100021113     C* Compongo il nome del membro da dare al FIVATWWR
045200010202     C                   eval      parmbr = vlrhdl
045300010202     C                   movel     'M'           parmbr
045400041201     C                   eval      parccm = vlrksc
045500010202     C                   eval      paropz = '1'
045600010202     C* Effettuo la chiamata al CLLE preposto
045700040506     C                   call(e)   'TITVVTC'
045800010202     C                   parm                    parccm
045900010202     C                   parm                    parmbr
046000010202     C                   parm                    paropz
046100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046200010202     C                   if        %error
046300010202     C                   movel     '1'           chkcall
046400010202     C                   else
046500010202     C                   movel     '0'           chkcall
046600010202     C                   endif
046700010202     C*
046800010202     C                   ENDSR
046900000801     C*----------------------------------------------------*
047000000801     C*  CONTROLLO NUMERICITA' CAMPI
047100000801     C*----------------------------------------------------*
047200000801     C     CHKNUM        BEGSR
047300000801     C*
047400000801     C                   call(e)   'ISNUMERIC'
047500000801     C                   PARM                    PiStr            30
047600050216     C                   PARM      '.'           PiDecChr          1
047700000801     C                   PARM      *ZEROS        PiVal            30 9
047800000801     C                   PARM      '0'           PiInt             1
047900000801     C                   PARM      '0'           PiNum             1
048000000801     C                   IF        %error
048100000801     C                   EVAL      PiInt=*off
048200000801     C                   ENDIF
048300000801     C*
048400000801     C                   ENDSR
048500000801     C***
048600000801
048700011113
048800011113     C*----------------------------------------------------*
048900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049000011113     C*----------------------------------------------------*
049100011113     C     CHKIMPDIV     BEGSR
049200011113     C*
049300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049400011113     C                   Z-ADD     *zeros        wrkDec            9 9
049500011113     C*
049600011113     C* Come prima cosa effettuo considerazioni sulla divisa
049700011113     C                   IF        vabIAS > *zeros
049800011113     C                   IF        vabVAS <> 'EUR'
049900011113     C                   EVAL      vabVAS =  'ITL'
050000011113     C                   ENDIF
050100011113     C                   ENDIF
050200011113     C*
050300011113     C                   IF        vabCAS > *zeros
050400011113     C                   IF        vabVCA <> 'EUR'
050500011113     C                   EVAL      vabVCA =  'ITL'
050600011113     C                   ENDIF
050700011113     C                   ENDIF
050800011113     C*
050900011113     C                   IF        vabVMD > *zeros
051000020305     C                   IF        vabVAD <> 'EUR'
051100011113     C                   EVAL      vabVAD =  'ITL'
051200011113     C                   ENDIF
051300011113     C                   ENDIF
051400011113     C*
051500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051600011113     C                   Z-ADD     vabIAS        wrkDec
051700011113     C                   IF        wrkDec > *zeros
051800011113     C                   IF        vabVAS = 'ITL'
051900011113     C                   EVAL      vabIAS = *zeros
052000011113     C                   ENDIF
052100011113     C                   ENDIF
052200011113     C*
052300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052400011113     C                   Z-ADD     vabCAS        wrkDec
052500011113     C                   IF        wrkDec > *zeros
052600011113     C                   IF        vabVCA = 'ITL'
052700011113     C                   EVAL      vabCAS = *zeros
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053200011113     C                   Z-ADD     vabVMD        wrkDec
053300011113     C                   IF        wrkDec > *zeros
053400011113     C                   IF        vabVAD = 'ITL'
053500011113     C                   EVAL      vabVMD = *zeros
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C                   ENDSR
054000011113     C***
054100011113
054200011113
054300000801
054400000801
054500990920      /TITLE Invio dei dati al punto operativo.
054600010202     C     invio         BEGSR
054700990920     C*
054800021113     C* 1° invio FIVAT
054900010201     C                   reset                   dscmz
055000010201     C                   move      vlrpoi        cmzdst
055100021113     C                   eval      cmzfld = 'FIVATWWR'
055200010201     C                   eval      cmzmbd = vlrhdl
055300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055400021009     C***                if        prmfir = *blanks
055500021113     C                   eval      cmzfla = 'FIVAT00F'
055600021113     C                   eval      cmzmba = 'FIVAT00F'
055700021009     C***                else
055800021009     C***                eval      cmzfla = prmfir
055900021009     C***                eval      cmzmba = prmfir
056000021009     C***                endif
056100010201     C                   eval      cmznrr = *zeros
056200020305     C                   move      §ctrokvt      cmznrr
056300021018     C                   eval      cmzlba = vlrfl1
056400010201     C                   call(e)   'TIS711C'
056500010201     C                   parm                    dscmz
056600010201     C                   parm      *blanks       esito
056700010205     C                   if        %error
056800010205     C                             or cmzerr = '1'
056900010205     C                             or esito  = '1'
057000010205     C                   eval      wrkesito = '3'
057100010205     C                   else
057200010201     C*
057300021113     C* 2° invio FIVAB
057400010201     C                   reset                   dscmz
057500010201     C                   move      vlrpoi        cmzdst
057600010201     C                   eval      cmzfld = vlrfou
057700010201     C                   eval      cmzmbd = vlrhdl
057800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057900021009     C***                if        prmfir = *blanks
058000021113     C                   eval      cmzfla = 'FIVAB00F'
058100021113     C                   eval      cmzmba = 'FIVAB00F'
058200021009     C***                else
058300021009     C***                eval      cmzfla = prmfir
058400021009     C***                eval      cmzmba = prmfir
058500021009     C***                endif
058600010201     C                   eval      cmznrr = *zeros
058700010201     C                   move      §ctrokvb      cmznrr
058800021018     C                   eval      cmzlba = vlrfl1
058900010201     C                   call(e)   'TIS711C'
059000010201     C                   parm                    dscmz
059100010201     C                   parm      *blanks       esito
059200010201     C                   if        %error
059300010201     C                             or cmzerr = '1'
059400010201     C                             or esito  = '1'
059500010201     C                   eval      wrkesito = '3'
059600010201     C                   endif
059700010205     C                   endif
059800990920     C*
059900000613     C                   ENDSR
060000000613     C***
060100070411
060200070411     C     *pssr         BEGSR
060300070411     C*
060400070411     C                   if        %open(tivin00r)
060500070411     C                   close     tivin00r
060600070411     C                   endif
060700070411     C                   if        %open(fivabwwr)
060800070411     C                   close     fivabwwr
060900070411     C                   endif
061000070411     C                   if        %open(fivatwwr)
061100070411     C                   close     fivatwwr
061200070411     C                   endif
061300070411     C*
061400070411     C* Effettuo la chiamata al CLLE preposto
061500070411     C                   call(e)   'TITVVTC'
061600070411     C                   parm                    parccm
061700070411     C                   parm                    parmbr
061800070411     C                   parm      '2'           paropz
061900070411     C*
062000070411     C                   eval      wrkesito = '2'
062100070411     C*
062200070411     C                   seton                                        LR
062300070411     C*
062400070411     C                   ENDSR     '*CANCL'
062500070411     C***
062600070411
062700990910
062800000613     C     *inzsr        BEGSR
062900990910     C*
063000990910     C     *entry        plist
063100990920     C                   parm                    tivlrds
063200990921     C                   parm      wrkesito      esito
063300000724     C                   parm                    prmlit
063400000710     C                   parm                    prmfir
063500000613     C*
063600000830     C* CALCOLA LA DATA CORRENTE
063700000830     C                   time                    wn14             14 0
063800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063900000830     C                   z-add     wn8           g08dat
064000000830     C                   z-add     *zeros        g08inv
064100000830     C                   movel     '0'           g08err
064200000830     C                   call      'XSRDA8'
064300000830     C                   parm                    wlbda8
064400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064500000830     C*
064600000613     C                   ENDSR
064700000613     C***
064800990908
064900050216     Otitvh9p   E            riepilogo         2
065000990915     O                                              'Upload via Internet'
065100990915     O                                           +1 'Traduzione TIVIN00R -'
065200021113     O                                           +1 'FIVABWWR/FIVATWWR'
065300010201     O                                           +1 'Report testate bolle'
065400990915     O          E            riepilogo   2
065500990915     O                       wrkdata
065600990915     O                       wrkora              +1
065700990915     O                       procname            +1
065800990915     O          E            riepilogo   2
065900990915     O                                              'Cliente..................:'
066000990915     O                       VABCCM        z     +1
066100990915     O          E            riepilogo   2
066200990920     O                                              'Riferimento Strategi.....:'
066300990920     O                       vlrhdl              +1
066400990915     O          E            riepilogo   2
066500990915     O                                              'Giusti...................:'
066600010201     O                       §CTROKVB      2   +  1
066700990915     O          E            riepilogo   2
066800990915     O                                              'Sbagliati e corretti.....:'
066900971022     O                       §CTRMO        2   +  1
067000990915     O          E            riepilogo   2
067100990915     O                                              'Sbagliati e scartati.....:'
067200971022     O                       §CTRNO        2   +  1
067300000613
067400050216     Otitvh9ps  E            testdett          2
067500000613     O                                              'Upload via Internet'
067600000613     O                                           +1 'Traduzione TIVIN00R -'
067700021113     O                                           +1 'FIVABWWR/FIVATWWR'
067800010201     O                                           +1 'Report testate bolle'
067900000616     O          E            testdett    3
068000000613     O                                           +2 'N° rec'
068100000613     O                                           +3 'Anteprima contenuto'
068200000616     O          E            rigadett    2
068300000613     O                       rrnum               +2
068400000621     O                       recko               +3
068500000616     O          E            findett     2
068600000613     O                       wrkdata
068700000613     O                       wrkora              +1
068800000613     O                       procname            +1
068900000616     O          E            findett     2
069000000613     O                                              'Cliente..................:'
069100000613     O                       VABCCM        z     +1
069200000616     O          E            findett     2
069300000613     O                                              'Riferimento Strategi.....:'
069400000613     O                       vlrhdl              +1
069500000616     O          E            findett     2
069600000613     O                                              'Giusti...................:'
069700010201     O                       §CTROKVB      2   +  1
069800000616     O          E            findett     2
069900000613     O                                              'Sbagliati e corretti.....:'
070000000613     O                       §CTRMO        2   +  1
070100000616     O          E            findett     2
070200000613     O                                              'Sbagliati e scartati.....:'
070300000613     O                       §CTRNO        2   +  1
070400000512** CMD - COMANDI CL
070500050216OVRPRTF FILE(TITVH9P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
070600050216OVRPRTF FILE(TITVH9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
070700050216DLTOVR FILE(TITVH9P TITVH9PS) LVL(*)
070800000512
070900000512
