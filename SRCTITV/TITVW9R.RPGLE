000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800070213     Ftitvw9p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070213     Ftitvw9ps  O    f  198        PRINTER usropn
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
004000081114     D curSped         s              6    INZ(*blanks)
004100081114     D depSped         s              6    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100070103     D*------------------
005200070103     D* DS REPERIMENTO NUMERATORE
005300070103     D*------------------
005400070103     D trul33ds      e ds                  inz
005500070103     D*------------------
005600070103     D* DS ARCHITETTURA
005700070103     D*------------------
005800070103     D kpjba         e ds                  inz
005900000830     D*
006000010201
006100010201
006200990915     C                   time                    wrkdata
006300990915     C                   time                    wrkora
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700000724     C*
006800000724     C* SE OCCORRE SPEDIRE IN FILIALE
006900000724     C                   if        vlrpoi <> *zeros
007000000724     C*
007100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007300000724     C     vlrpoi        chain     azorg01l
007400000724     C                   if        %found
007500000616     C                   movel(p)  CMD(1)        depcmd
007600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007700000616     C*
007800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007900011113     C                   Z-ADD     150           LENGH            15 5
008000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008100000616     C                   PARM                    depcmd
008200000616     C                   PARM                    LENGH
008300000724     C*
008400000724     C                   endif
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000616     C*
009700070213     C                   if        not %open(titvw9ps)
009800070213     C                   open      titvw9ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100000613     C*
010200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010400000613     C*
010500010202     C* Effettuo la chiamata al CLLE preposto
010600040506     C                   call(e)   'TITVVTC'
010700010202     C                   parm                    parccm
010800010202     C                   parm                    parmbr
010900010202     C                   parm      '2'           paropz
011000050201     C*
011100050201     C* Effettuo lancio TISI95 solo x chiusura
011200050201     C                   CLEAR                   TISI95DS
011300050201     C                   EVAL      I95TLA = 'C'
011400050201     C                   CALL      'TISI95R'
011500050201     C                   PARM                    TISI95DS
011600010202     C*
011700070213     C                   if        %open(titvw9ps)
011800000616     C                   except    findett
011900070213     C                   close     titvw9ps
012000000613     C                   endif
012100000616     C*
012200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    CMD(3)
012600000616     C                   PARM                    LENGH
012700000616     C*
012800000801     C
012900010201     C                   seton                                        LR
013000000613
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700070213     C                   if        not %open(titvw9p)
013800070213     C                   open      titvw9p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300070213     C                   if        %open(titvw9p)
014400070213     C                   close     titvw9p
014500990914     C                   endif
014600990910     C*
014700000613     C                   ENDSR
014800000613     C***
014900990908
015000000801
015100910830     C*--------------------------------------------------------
015200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015300910830     C*--------------------------------------------------------
015400040526     C     RWFILE        BEGSR
015500990910     C*
015600990914     C                   if        not %open(tivin00r)
015700990908     C                   open      tivin00r
015800990914     C                   endif
015900021113     C                   if        not %open(fivabwwr)
016000021113     C                   open      fivabwwr
016100990914     C                   endif
016200070103     C*
016300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016400020305     C                   exsr      prevat
016500010201     C*
016600010202     C                   if        chkcall = '0'
016700010202     C*
016800021113     C                   if        not %open(fivatwwr)
016900021113     C                   open      fivatwwr
017000010201     C                   endif
017100990910     C*
017200010201     C                   clear                   §CTROKVB          5 0
017300020305     C                   clear                   §CTROKVT          5 0
017400000801     C                   clear                   §CTRMO            5 0
017500000801     C                   clear                   §CTRNO            5 0
017600990910     C*
017700921023     C                   DO        *HIVAL
017800990913     C*
017900990915     C                   READ      tivin00r                               70
018000050627     C                   if        vindta > *blanks
018100000613     C                   add       1             rrnum
018200000801     C*
018300000801     C                   if        *in70 = *off
018400000801     C                             and
018500000801     C                             (vinflg = *blanks
018600000801     C                              or vinflg = '0'
018700000801     C                              or vinflg = '2')
018800000801     C*
018900000801     C                   clear                   vinmsg
019000000801     C                   eval      vinflg = '1'
019100070103     C*
019200070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
019300081114     C                   eval      curSped = %subst(vindta:163:6)
019400070213     C                   if        curSped <> depSped
019500070213     C* Se prima bolla => importo bolla corrente
019600070213     C                   if        §CTRMO = *zeros
019700070213     C                   exsr      inzvar
019800070213     C                   exsr      defcam
019900070213     C                   exsr      repnum
020000070213     C                   exsr      impvab                                       => carico  VAB
020100070213     C                   else
020200070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
020300070213     C                   exsr      wrivab                                       => scarico VAB
020400070213     C                   exsr      inzvar
020500070213     C                   exsr      defcam
020600070213     C                   exsr      repnum
020700070213     C                   exsr      impvab                                       => carico  VAB
020800070213     C                   endif
020900070213     C                   exsr      exevate                                      => write VAT-E
021000070103     C* Salvo il raggruppamento spedizione cliente corrente
021100070103     C                   eval      depSped = curSped
021200070213     C*
021300070213     C* Se collo successivo x stessa bolla
021400070213     C                   else
021500070213     C                   exsr      impvab                                       => carico  VAB
021600070213     C                   exsr      exevate                                      => write VAT-E
021700070103     C                   endif
021800000905     C*
021900000905     C                   else
022000000905     C                   eval      vinflg = '1'
022100050628     C                   endif
022200000905     C                   endif
022300000905     C*
022400000905     C  N70              update    tivin000
022500000905     C*
022600991022     C  N70              ENDdo
022700070213     C*
022800070213     C* Scarico testata bolla rimasta in sospesa
022900070213     C                   exsr      wrivab                                       => scarico VAB
023000010202     C*
023100010202     C                   endif
023200990910
023300990910     C* Se non ci sono record con errori ...
023400000710     C                   if        §ctrno = 0
023500990910     C* ... restituisco esito OK.
023600990921     C                   eval      wrkesito = '0'
023700990910     C                   else
023800010201     C                   if        §ctrokvb > 0
023900990921     C                   eval      wrkesito = '1'
024000000710     C                   else
024100000710     C                   eval      wrkesito = '2'
024200990910     C                   endif
024300000710     C                   endif
024400990910     C*
024500990914     C                   if        %open(tivin00r)
024600990908     C                   close     tivin00r
024700990914     C                   endif
024800021113     C                   if        %open(fivabwwr)
024900021113     C                   close     fivabwwr
025000990914     C                   endif
025100021113     C                   if        %open(fivatwwr)
025200021113     C                   close     fivatwwr
025300010201     C                   endif
025400990910     C*
025500010201     C                   if        §ctrokvb > 0
025600000724     C                             and vlrpoi <> *zeros
025700010202     C                   exsr      invio
025800990920     C                   endif
025900990920     C*
026000910830     C                   ENDSR
026100000613     C***
026200010305
026300010305     C*----------------------------------------------------*
026400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026500010305     C*----------------------------------------------------*
026600020305     C     WRIVAB        BEGSR
026700010305     C*
026800060225     C* Quindi scarico il buffer del file d testata
026900021113     C                   write     fivab000                                     => scarico il VAB
027000010305     C*
027100010305     C                   ENDSR
027200990920
027300000801     C*----------------------------------------------------*
027400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027500000801     C*----------------------------------------------------*
027600010201     C     INZVAR        BEGSR
027700000801     C*
027800070213     C                   CLEAR                   FIVAB000
027900070213     C                   CLEAR                   FIVAT000
028000070213     C*
028100040802     C                   Z-ADD     *zeros        Num5_0            5 0
028200040802     C                   MOVEL     '0'           FlgCAS            1
028300000801     C*
028400000801     C                   ENDSR
028500000801     C*----------------------------------------------------*
028600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028700000801     C*----------------------------------------------------*
028800000801     C     DEFCAM        BEGSR
028900000801     C*
029000020619     C* Imposto i valori di default...
029100070213     C                   Z-ADD     0951267       VABCCM
029200070213     C                   Z-ADD     0951267       VATCCM
029300070213     C                   Z-ADD     050           VABLNP
029400070213     C                   Z-ADD     050           VATLNP
029500070213     C                   Z-ADD     101           VABCTR
029600070103     C                   MOVEL     '7Q'          VABCTM
029700040823     C                   MOVEL     '1'           VABCBO
029800070213     C                   MOVEL     'C'           VABTSP
029900020619     C* ... e poi verifico se sono stati passati come parametri
030000020619     C                   IF        vlrppt > *blanks
030100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
030200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030300020619     C                   EXSR      CHKNUM
030400020619     C                   IF        PiInt=*on
030500020619     C                   Z-ADD     PiVal         VABCCM
030600020619     C                   Z-ADD     PiVal         VATCCM
030700020619     C                   ENDIF
030800040506     C                   ENDIF
030900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
031000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031100020619     C                   EXSR      CHKNUM
031200020619     C                   IF        PiInt=*on
031300020619     C                   Z-ADD     PiVal         VABLNP
031400020619     C                   Z-ADD     PiVal         VATLNP
031500040506     C                   ENDIF
031600020619     C                   ENDIF
031700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031900020619     C                   EXSR      CHKNUM
032000020619     C                   IF        PiInt=*on
032100020619     C                   Z-ADD     PiVal         VABCTR
032200040506     C                   ENDIF
032300020619     C                   ENDIF
032400020619     C                   ENDIF
032500000801     C*
032600000801     C                   ENDSR
032700000801     C*----------------------------------------------------*
032800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032900000801     C*----------------------------------------------------*
033000040823     C     IMPVAB        BEGSR
033100010305     C*
033200000801     C                   Z-ADD     *zeros        errore            1 0
033300000830     C                   MOVEL     datcor        VABAAS
033400020305     C                   MOVEL     datcor        VATAAS
033500040526     C                   MOVE      datcor        VABMGS
033600040823     C                   MOVE(P)   vlrpoi        VABFGS
033700040823     C                   MOVE(P)   vlrpoi        VATFGS
033800050628     C*
033900070213     C                   EVAL      VABRSD=%trim(%subst(vindta:24:35))
034000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034200020117     C* ==
034300070213     C                   EVAL      VABRD2=%trim(%subst(vindta:24+35:43-35))
034400070213     C                   EVAL      VABIND=%trim(%subst(vindta:67:50))
034500070213     C                   EVAL      VABLOD=%trim(%subst(vindta:117:36))
034600070213     C                   EVAL      VABPRD=%trim(%subst(vindta:158:2))
034700081114     C                   EVAL      VABRMA=%trim(%subst(vindta:163:6))
034800071008     C                   EVAL      VABTSP=%trim(%subst(vindta:200:1))
034900090910     C                   EVAL      VABNOT=%trim(%subst(vindta:203:35))
035000090910     C                   EVAL      VABNT2=%trim(%subst(vindta:203+35:255-35))
035100050628     C* CAP
035200070213     C                   EVAL      PiStr=%trim(%subst(vindta:153:5))
035300010201     C                   EXSR      CHKNUM
035400010201     C                   IF        PiInt=*on
035500010201     C                   Z-ADD     PiVal         Num5_0
035600040506     C                   MOVEL(P)  Num5_0        VABCAD
035700010201     C                   ELSE
035800040506     C                   ADD       1             errore
035900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036000040506     C                             + ' ' + 'VABCAD'
036100010201     C                   ENDIF
036200040506     C* Reperisco la provincia dal CAP e dalla località
036300040526     C                   IF        VABCAD <> *blanks AND
036400040526     C                             VABPRD  = *blanks
036500040506     C                   CLEAR                   TISI95DS
036600040506     C                   EVAL      I95TCN = '3'
036700040506     C                   Z-ADD     datcor        I95DAT
036800040506     C                   EVAL      I95CAP = VABCAD
036900040506     C                   EVAL      I95LOC = VABLOD
037000050627     C                   EVAL      I95NAR = VABNZD
037100040506     C                   CALL      'TISI95R'
037200040506     C                   PARM                    TISI95DS
037300040506     C                   EVAL      VABPRD = O95PRV
037400040506     C                   ENDIF
037500060307     C* NCL
037600070213     C                   EVAL      VABNCL = VABNCL + 1
037700040506     C* PKB
037800070213     C                   EVAL      PiStr=%trim(%subst(vindta:190:5))
037900010201     C                   EXSR      CHKNUM
038000010201     C                   IF        PiNum=*on
038100070213     C                   ADD       PiVal         VABPKB
038200010201     C                   ELSE
038300010201     C                   ADD       1             errore
038400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038500010201     C                             + ' ' + 'VABPKB'
038600010201     C                   ENDIF
038700070213     C* VLB
038800070213     C                   EVAL      PiStr=%trim(%subst(vindta:195:5))
038900070213     C                   EXSR      CHKNUM
039000070213     C                   IF        PiNum=*on
039100070213     C                   ADD       PiVal         VABVLB
039200070213     C                   ELSE
039300070213     C                   ADD       1             errore
039400070213     C                   EVAL      vinmsg = %trimr(vinmsg)
039500070213     C                             + ' ' + 'VABVLB'
039600070213     C                   ENDIF
039700070103     C* RMN
039800081114     C                   EVAL      PiStr=%trim(%subst(vindta:163:6))
039900050928     C                   EXSR      CHKNUM
040000050928     C                   IF        PiInt=*on
040100050928     C                   Z-ADD     PiVal         VABRMN
040200050928     C                   ELSE
040300050928     C                   ADD       1             errore
040400050928     C                   EVAL      vinmsg = %trimr(vinmsg)
040500070103     C                             + ' ' + 'VABRMN'
040600050928     C                   ENDIF
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
043200070213     C*----------------------------------------------------*
043300070213     C*  REPERIMENTO NUMERATORE X TESTATA BOLLE
043400070213     C*----------------------------------------------------*
043500070213     C     REPNUM        BEGSR
043600070213     C*
043700070213     C* NSP => Stacco un numeratore da AZNUM
043800070213     C                   clear                   TRUL33DS
043900070213     C                   eval      I33OPE = *zeros
044000070213     C                   eval      I33CNU = 302
044100070213     C                   eval      I33NUM = 1
044200070213     C                   movel     TRUL33DS      KPJBU
044300070213     C                   call      'TRUL33R'
044400070213     C                   parm                    KPJBA
044500070213     C                   movel     KPJBU         TRUL33DS
044600070213     C                   if        O33ERR = *zeros
044700070213     C                   z-add     O33NRF        VABNSP
044800070213     C                   z-add     O33NRF        VATNSP
044900070213     C                   else
045000070213     C                   Z-ADD     1             errore
045100070213     C                   EVAL      vinmsg = %trimr(vinmsg)
045200070213     C                             + ' ' + 'VABNSP VATNSP'
045300070213     C                   endif
045400070213     C*
045500070213     C                   ENDSR
045600070102     C*----------------------------------------------------*
045700070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045800070102     C*----------------------------------------------------*
045900070103     C     EXEVATE       BEGSR
046000070102     C*
046100070103     C                   EVAL      VATTRC='E'
046200070213     C                   EVAL      VATNOT = %trim(%subst(vindta:180:10))
046300070102     C                   exsr      wrivat                                       => scarico VAT
046400070102     C*
046500070102     C                   ENDSR
046600010201     C*----------------------------------------------------*
046700040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046800010201     C*----------------------------------------------------*
046900020305     C     WRIVAT        BEGSR
047000050628     C*
047100060223     C* Scrivo solo se valorizzato qualcosa
047200060223     C                   IF        VATNOT <> *blanks
047300040802     C                   WRITE     FIVAT000
047400060223     C                   ENDIF
047500010201     C*
047600010201     C                   ENDSR
047700010202     C*----------------------------------------------------*
047800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047900010202     C*----------------------------------------------------*
048000020305     C     PREVAT        BEGSR
048100010202     C*
048200021113     C* Compongo il nome del membro da dare al FIVATWWR
048300010202     C                   eval      parmbr = vlrhdl
048400010202     C                   movel     'M'           parmbr
048500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
048600010202     C                   eval      paropz = '1'
048700010202     C* Effettuo la chiamata al CLLE preposto
048800040506     C                   call(e)   'TITVVTC'
048900010202     C                   parm                    parccm
049000010202     C                   parm                    parmbr
049100010202     C                   parm                    paropz
049200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049300010202     C                   if        %error
049400010202     C                   movel     '1'           chkcall
049500010202     C                   else
049600010202     C                   movel     '0'           chkcall
049700010202     C                   endif
049800010202     C*
049900010202     C                   ENDSR
050000000801     C*----------------------------------------------------*
050100000801     C*  CONTROLLO NUMERICITA' CAMPI
050200000801     C*----------------------------------------------------*
050300000801     C     CHKNUM        BEGSR
050400000801     C*
050500000801     C                   call(e)   'ISNUMERIC'
050600000801     C                   PARM                    PiStr            30
050700070213     C                   PARM      ','           PiDecChr          1
050800000801     C                   PARM      *ZEROS        PiVal            30 9
050900000801     C                   PARM      '0'           PiInt             1
051000000801     C                   PARM      '0'           PiNum             1
051100000801     C                   IF        %error
051200000801     C                   EVAL      PiInt=*off
051300000801     C                   ENDIF
051400000801     C*
051500000801     C                   ENDSR
051600000801     C***
051700000801
051800011113
051900011113     C*----------------------------------------------------*
052000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052100011113     C*----------------------------------------------------*
052200011113     C     CHKIMPDIV     BEGSR
052300011113     C*
052400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052500011113     C                   Z-ADD     *zeros        wrkDec            9 9
052600011113     C*
052700011113     C* Come prima cosa effettuo considerazioni sulla divisa
052800011113     C                   IF        vabIAS > *zeros
052900011113     C                   IF        vabVAS <> 'EUR'
053000011113     C                   EVAL      vabVAS =  'ITL'
053100011113     C                   ENDIF
053200011113     C                   ENDIF
053300011113     C*
053400011113     C                   IF        vabCAS > *zeros
053500011113     C                   IF        vabVCA <> 'EUR'
053600011113     C                   EVAL      vabVCA =  'ITL'
053700011113     C                   ENDIF
053800011113     C                   ENDIF
053900011113     C*
054000011113     C                   IF        vabVMD > *zeros
054100020305     C                   IF        vabVAD <> 'EUR'
054200011113     C                   EVAL      vabVAD =  'ITL'
054300011113     C                   ENDIF
054400011113     C                   ENDIF
054500011113     C*
054600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054700011113     C                   Z-ADD     vabIAS        wrkDec
054800011113     C                   IF        wrkDec > *zeros
054900011113     C                   IF        vabVAS = 'ITL'
055000011113     C                   EVAL      vabIAS = *zeros
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055500011113     C                   Z-ADD     vabCAS        wrkDec
055600011113     C                   IF        wrkDec > *zeros
055700011113     C                   IF        vabVCA = 'ITL'
055800011113     C                   EVAL      vabCAS = *zeros
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056300011113     C                   Z-ADD     vabVMD        wrkDec
056400011113     C                   IF        wrkDec > *zeros
056500011113     C                   IF        vabVAD = 'ITL'
056600011113     C                   EVAL      vabVMD = *zeros
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C                   ENDSR
057100011113     C***
057200011113
057300011113
057400000801
057500000801
057600990920      /TITLE Invio dei dati al punto operativo.
057700010202     C     invio         BEGSR
057800990920     C*
057900021113     C* 1° invio FIVAT
058000010201     C                   reset                   dscmz
058100010201     C                   move      vlrpoi        cmzdst
058200021113     C                   eval      cmzfld = 'FIVATWWR'
058300010201     C                   eval      cmzmbd = vlrhdl
058400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058500021009     C***                if        prmfir = *blanks
058600021113     C                   eval      cmzfla = 'FIVAT00F'
058700021113     C                   eval      cmzmba = 'FIVAT00F'
058800021009     C***                else
058900021009     C***                eval      cmzfla = prmfir
059000021009     C***                eval      cmzmba = prmfir
059100021009     C***                endif
059200010201     C                   eval      cmznrr = *zeros
059300020305     C                   move      §ctrokvt      cmznrr
059400021018     C                   eval      cmzlba = vlrfl1
059500010201     C                   call(e)   'TIS711C'
059600010201     C                   parm                    dscmz
059700010201     C                   parm      *blanks       esito
059800010205     C                   if        %error
059900010205     C                             or cmzerr = '1'
060000010205     C                             or esito  = '1'
060100010205     C                   eval      wrkesito = '3'
060200010205     C                   else
060300010201     C*
060400021113     C* 2° invio FIVAB
060500010201     C                   reset                   dscmz
060600010201     C                   move      vlrpoi        cmzdst
060700010201     C                   eval      cmzfld = vlrfou
060800010201     C                   eval      cmzmbd = vlrhdl
060900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061000021009     C***                if        prmfir = *blanks
061100021113     C                   eval      cmzfla = 'FIVAB00F'
061200021113     C                   eval      cmzmba = 'FIVAB00F'
061300021009     C***                else
061400021009     C***                eval      cmzfla = prmfir
061500021009     C***                eval      cmzmba = prmfir
061600021009     C***                endif
061700010201     C                   eval      cmznrr = *zeros
061800010201     C                   move      §ctrokvb      cmznrr
061900021018     C                   eval      cmzlba = vlrfl1
062000010201     C                   call(e)   'TIS711C'
062100010201     C                   parm                    dscmz
062200010201     C                   parm      *blanks       esito
062300010201     C                   if        %error
062400010201     C                             or cmzerr = '1'
062500010201     C                             or esito  = '1'
062600010201     C                   eval      wrkesito = '3'
062700010201     C                   endif
062800010205     C                   endif
062900990920     C*
063000000613     C                   ENDSR
063100000613     C***
063200070411
063300070411     C     *pssr         BEGSR
063400070411     C*
063500070411     C                   if        %open(tivin00r)
063600070411     C                   close     tivin00r
063700070411     C                   endif
063800070411     C                   if        %open(fivabwwr)
063900070411     C                   close     fivabwwr
064000070411     C                   endif
064100070411     C                   if        %open(fivatwwr)
064200070411     C                   close     fivatwwr
064300070411     C                   endif
064400070411     C*
064500070411     C* Effettuo la chiamata al CLLE preposto
064600070411     C                   call(e)   'TITVVTC'
064700070411     C                   parm                    parccm
064800070411     C                   parm                    parmbr
064900070411     C                   parm      '2'           paropz
065000070411     C*
065100070411     C                   eval      wrkesito = '2'
065200070411     C*
065300070411     C                   seton                                        LR
065400070411     C*
065500070411     C                   ENDSR     '*CANCL'
065600070411     C***
065700070411
065800990910
065900000613     C     *inzsr        BEGSR
066000990910     C*
066100990910     C     *entry        plist
066200990920     C                   parm                    tivlrds
066300990921     C                   parm      wrkesito      esito
066400000724     C                   parm                    prmlit
066500000710     C                   parm                    prmfir
066600000613     C*
066700000830     C* CALCOLA LA DATA CORRENTE
066800000830     C                   time                    wn14             14 0
066900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067000000830     C                   z-add     wn8           g08dat
067100000830     C                   z-add     *zeros        g08inv
067200000830     C                   movel     '0'           g08err
067300000830     C                   call      'XSRDA8'
067400000830     C                   parm                    wlbda8
067500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067600000830     C*
067700000613     C                   ENDSR
067800000613     C***
067900990908
068000070213     Otitvw9p   E            riepilogo         2
068100990915     O                                              'Upload via Internet'
068200990915     O                                           +1 'Traduzione TIVIN00R -'
068300021113     O                                           +1 'FIVABWWR/FIVATWWR'
068400010201     O                                           +1 'Report testate bolle'
068500990915     O          E            riepilogo   2
068600990915     O                       wrkdata
068700990915     O                       wrkora              +1
068800990915     O                       procname            +1
068900990915     O          E            riepilogo   2
069000990915     O                                              'Cliente..................:'
069100990915     O                       VABCCM        z     +1
069200990915     O          E            riepilogo   2
069300990920     O                                              'Riferimento Strategi.....:'
069400990920     O                       vlrhdl              +1
069500990915     O          E            riepilogo   2
069600990915     O                                              'Giusti...................:'
069700010201     O                       §CTROKVB      2   +  1
069800990915     O          E            riepilogo   2
069900990915     O                                              'Sbagliati e corretti.....:'
070000971022     O                       §CTRMO        2   +  1
070100990915     O          E            riepilogo   2
070200990915     O                                              'Sbagliati e scartati.....:'
070300971022     O                       §CTRNO        2   +  1
070400000613
070500070213     Otitvw9ps  E            testdett          2
070600000613     O                                              'Upload via Internet'
070700000613     O                                           +1 'Traduzione TIVIN00R -'
070800021113     O                                           +1 'FIVABWWR/FIVATWWR'
070900010201     O                                           +1 'Report testate bolle'
071000000616     O          E            testdett    3
071100000613     O                                           +2 'N° rec'
071200000613     O                                           +3 'Anteprima contenuto'
071300000616     O          E            rigadett    2
071400000613     O                       rrnum               +2
071500000621     O                       recko               +3
071600000616     O          E            findett     2
071700000613     O                       wrkdata
071800000613     O                       wrkora              +1
071900000613     O                       procname            +1
072000000616     O          E            findett     2
072100000613     O                                              'Cliente..................:'
072200000613     O                       VABCCM        z     +1
072300000616     O          E            findett     2
072400000613     O                                              'Riferimento Strategi.....:'
072500000613     O                       vlrhdl              +1
072600000616     O          E            findett     2
072700000613     O                                              'Giusti...................:'
072800010201     O                       §CTROKVB      2   +  1
072900000616     O          E            findett     2
073000000613     O                                              'Sbagliati e corretti.....:'
073100000613     O                       §CTRMO        2   +  1
073200000616     O          E            findett     2
073300000613     O                                              'Sbagliati e scartati.....:'
073400000613     O                       §CTRNO        2   +  1
073500000512** CMD - COMANDI CL
073600070213OVRPRTF FILE(TITVW9P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
073700070213OVRPRTF FILE(TITVW9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
073800070213DLTOVR FILE(TITVW9P TITVW9PS) LVL(*)
073900000512
074000000512
