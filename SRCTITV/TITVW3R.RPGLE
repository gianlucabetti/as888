000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800070103     Ftitvw3p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070103     Ftitvw3ps  O    f  198        PRINTER usropn
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
004000070103     D curSped         s             16    INZ(*blanks)
004100070103     D depSped         s             16    INZ(*blanks)
004200070103     D*------------------
004300070103     D* DS REPERIMENTO NUMERATORE
004400070103     D*------------------
004500070103     D trul33ds      e ds                  inz
004600070103     D*------------------
004700070103     D* DS ARCHITETTURA
004800070103     D*------------------
004900070103     D kpjba         e ds                  inz
005000000830     D*
005100080128     D*------------------
005200080128     D* DS RMO "MIROGLIO"
005300080128     D*------------------
005400080128     D  wDS_RMO        ds                  inz
005500080128     D   wRMO_TIPM                    1
005600080128     D   wRMO_CLIM                   11
005700080128     D   wRMO_TSER                    1
005800100113
005900100113     D*------------------
006000100113     D* DS NAS "MIROGLIO"
006100100113     D*------------------
006200100113     D  wDS_NAS        ds
006300100113     D   wNAS_CONST1                  8    inz('ABB.TO  ')
006400100113     D   wNAS_TNCL_B                  2  0 inz(*zeros)
006500100113     D   wNAS_CONST2                  1    inz('B')
006600100113     D   wNAS_TNCL_C                  2  0 inz(*zeros)
006700100113     D   wNAS_CONST3                  1    inz('C')
006800010201
006900010201
007000990915     C                   time                    wrkdata
007100990915     C                   time                    wrkora
007200000913     C                   reset                   rrnum
007300990921     C                   reset                   esito
007400990921     C                   reset                   wrkesito
007500000724     C*
007600000724     C* SE OCCORRE SPEDIRE IN FILIALE
007700000724     C                   if        vlrpoi <> *zeros
007800000724     C*
007900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008100000724     C     vlrpoi        chain     azorg01l
008200000724     C                   if        %found
008300000616     C                   movel(p)  CMD(1)        depcmd
008400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008500000616     C*
008600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008700011113     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    depcmd
009000000616     C                   PARM                    LENGH
009100000724     C*
009200000724     C                   endif
009300000724     C                   endif
009400000616     C*
009500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009600000616     C                   movel(p)  CMD(2)        depcmd
009700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009800000616     C*
009900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010000011113     C                   Z-ADD     150           LENGH            15 5
010100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010200000616     C                   PARM                    depcmd
010300000616     C                   PARM                    LENGH
010400000616     C*
010500070103     C                   if        not %open(titvw3ps)
010600070103     C                   open      titvw3ps
010700000616     C                   except    testdett
010800000613     C                   endif
010900000613     C*
011000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
011100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011200000613     C*
011300010202     C* Effettuo la chiamata al CLLE preposto
011400040506     C                   call(e)   'TITVVTC'
011500010202     C                   parm                    parccm
011600010202     C                   parm                    parmbr
011700010202     C                   parm      '2'           paropz
011800050201     C*
011900050201     C* Effettuo lancio TISI95 solo x chiusura
012000050201     C                   CLEAR                   TISI95DS
012100050201     C                   EVAL      I95TLA = 'C'
012200050201     C                   CALL      'TISI95R'
012300050201     C                   PARM                    TISI95DS
012400010202     C*
012500070103     C                   if        %open(titvw3ps)
012600000616     C                   except    findett
012700070103     C                   close     titvw3ps
012800000613     C                   endif
012900000616     C*
013000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    CMD(3)
013400000616     C                   PARM                    LENGH
013500000616     C*
013600000801     C
013700010201     C                   seton                                        LR
013800000613
013900000613
014000000613     C*--------------------------------------------------------
014100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014200000613     C*--------------------------------------------------------
014300000621     C     STPR          BEGSR
014400000613     C*
014500070103     C                   if        not %open(titvw3p)
014600070103     C                   open      titvw3p
014700990915     C                   endif
014800990915     C*
014900990915     C                   except    riepilogo
015000990915     C*
015100070103     C                   if        %open(titvw3p)
015200070103     C                   close     titvw3p
015300990914     C                   endif
015400990910     C*
015500000613     C                   ENDSR
015600000613     C***
015700990908
015800000801
015900910830     C*--------------------------------------------------------
016000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
016100910830     C*--------------------------------------------------------
016200040526     C     RWFILE        BEGSR
016300990910     C*
016400990914     C                   if        not %open(tivin00r)
016500990908     C                   open      tivin00r
016600990914     C                   endif
016700021113     C                   if        not %open(fivabwwr)
016800021113     C                   open      fivabwwr
016900990914     C                   endif
017000070103     C*
017100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017200020305     C                   exsr      prevat
017300010201     C*
017400010202     C                   if        chkcall = '0'
017500010202     C*
017600021113     C                   if        not %open(fivatwwr)
017700021113     C                   open      fivatwwr
017800010201     C                   endif
017900990910     C*
018000010201     C                   clear                   §CTROKVB          5 0
018100020305     C                   clear                   §CTROKVT          5 0
018200000801     C                   clear                   §CTRMO            5 0
018300000801     C                   clear                   §CTRNO            5 0
018400990910     C*
018500921023     C                   DO        *HIVAL
018600990913     C*
018700990915     C                   READ      tivin00r                               70
018800050627     C                   if        vindta > *blanks
018900000613     C                   add       1             rrnum
019000000801     C*
019100000801     C                   if        *in70 = *off
019200000801     C                             and
019300000801     C                             (vinflg = *blanks
019400000801     C                              or vinflg = '0'
019500000801     C                              or vinflg = '2')
019600000801     C*
019700000801     C                   clear                   vinmsg
019800000801     C                   eval      vinflg = '1'
019900070103     C*
020000070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
020100070103     C                   eval      curSped = %subst(vindta:303:16)
020200070103     C                   if        curSped <> depSped
020300050628     C*
020400100113     C                   if        §ctrokvb > *zeros
020500100113     C                   exsr      wrivab                                       => scarico VAB
020600100113     C                   endif
020700100113     C*
020800100113     C                   clear                   fivab000
020900100113     C                   clear                   fivat000
021000100113     C                   exsr      impvab                                       => carico  VAB
021100070103     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
021200070103     C                   exsr      exevatb
021300070103     C* Scrivo anche estensione "chi sono" - tipo record "E" => BARCODE ETICHETTA
021400070103     C                   exsr      exevate
021500070103     C* Salvo il raggruppamento spedizione cliente corrente
021600070103     C                   eval      depSped = curSped
021700100113     C*
021800070103     C                   else
021900100113     C*
022000070103     C* Scrivo anche estensione "chi sono" - tipo record "E" => BARCODE ETICHETTA
022100070103     C                   exsr      exevate
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
023200100113     C*
023300100113     C* Scarico l'ultima testata ancora "in canna"
023400100113     C                   exsr      wrivab                                       => scarico VAB
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
027200100113     C*
027300100113     C* Effettuo forzature specifiche sul codice tariffa in relazione alla tipologia colli
027400100113     C                   eval      VABNAS = wDS_NAS
027500100113     C                   select
027600100113     C                   when      wNAS_TNCL_B = VABNCL
027700100113     C                   eval      VABCTR = 149
027800100113     C                   when      wNAS_TNCL_C = VABNCL
027900100113     C                   eval      VABCTR = 105
028000100113     C                   other
028100100120     C                   eval      VABCTR = 199
028200100113     C                   endsl
028300010305     C*
028400060225     C* Quindi scarico il buffer del file d testata
028500021113     C                   write     fivab000                                     => scarico il VAB
028600010305     C*
028700010305     C                   ENDSR
028800990920
028900000801     C*----------------------------------------------------*
029000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029100000801     C*----------------------------------------------------*
029200010201     C     INZVAR        BEGSR
029300000801     C*
029400040802     C                   Z-ADD     *zeros        Num5_0            5 0
029500040802     C                   MOVEL     '0'           FlgCAS            1
029600100113     C*
029700100113     C                   RESET                   wDS_NAS
029800000801     C*
029900000801     C                   ENDSR
030000000801     C*----------------------------------------------------*
030100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
030200000801     C*----------------------------------------------------*
030300000801     C     DEFCAM        BEGSR
030400000801     C*
030500020619     C* Imposto i valori di default...
030600070103     C                   Z-ADD     0450195       VABCCM
030700070103     C                   Z-ADD     0450195       VATCCM
030800070103     C                   Z-ADD     045           VABLNP
030900070103     C                   Z-ADD     045           VATLNP
031000040714     C                   Z-ADD     000           VABCTR
031100070103     C                   MOVEL     '7Q'          VABCTM
031200040823     C                   MOVEL     '1'           VABCBO
031300020619     C* ... e poi verifico se sono stati passati come parametri
031400020619     C                   IF        vlrppt > *blanks
031500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
031600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031700020619     C                   EXSR      CHKNUM
031800020619     C                   IF        PiInt=*on
031900020619     C                   Z-ADD     PiVal         VABCCM
032000020619     C                   Z-ADD     PiVal         VATCCM
032100020619     C                   ENDIF
032200040506     C                   ENDIF
032300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
032400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032500020619     C                   EXSR      CHKNUM
032600020619     C                   IF        PiInt=*on
032700020619     C                   Z-ADD     PiVal         VABLNP
032800020619     C                   Z-ADD     PiVal         VATLNP
032900040506     C                   ENDIF
033000020619     C                   ENDIF
033100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
033200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033300020619     C                   EXSR      CHKNUM
033400020619     C                   IF        PiInt=*on
033500020619     C                   Z-ADD     PiVal         VABCTR
033600040506     C                   ENDIF
033700020619     C                   ENDIF
033800020619     C                   ENDIF
033900000801     C*
034000000801     C                   ENDSR
034100000801     C*----------------------------------------------------*
034200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034300000801     C*----------------------------------------------------*
034400040823     C     IMPVAB        BEGSR
034500040823     C*
034600020305     C                   EXSR      INZVAR
034700020305     C                   EXSR      DEFCAM
034800070103     C*
034900070103     C* NSP => Stacco un numeratore da AZNUM
035000070103     C                   clear                   TRUL33DS
035100070103     C                   eval      I33OPE = *zeros
035200070103     C                   eval      I33CNU = 302
035300070103     C                   eval      I33NUM = 1
035400070103     C                   movel     TRUL33DS      KPJBU
035500070103     C                   call      'TRUL33R'
035600070103     C                   parm                    KPJBA
035700070103     C                   movel     KPJBU         TRUL33DS
035800070103     C                   if        O33ERR = *zeros
035900070103     C                   z-add     O33NRF        VABNSP
036000070103     C                   z-add     O33NRF        VATNSP
036100070103     C                   else
036200070103     C                   Z-ADD     1             errore
036300070103     C                   EVAL      vinmsg = %trimr(vinmsg)
036400070103     C                             + ' ' + 'VABNSP VATNSP'
036500070103     C                   endif
036600010305     C*
036700000801     C                   Z-ADD     *zeros        errore            1 0
036800000830     C                   MOVEL     datcor        VABAAS
036900020305     C                   MOVEL     datcor        VATAAS
037000040526     C                   MOVE      datcor        VABMGS
037100040823     C                   MOVE(P)   vlrpoi        VABFGS
037200040823     C                   MOVE(P)   vlrpoi        VATFGS
037300050628     C*
037400070103     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
037500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
037700020117     C* ==
037800070103     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
037900070103     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
038000070103     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
038100070103     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
038200100113     C***                EVAL      VABNAS=%trim(%subst(vindta:145:20))
038300070103     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
038400070103     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:35))
038500070103     C                   IF        %subst(vindta:16:1) <> '0'
038600070103     C                   EVAL      VABFFD='S'
038700070102     C                   ENDIF
038800080128     C*
038900080128     C* Valorizzazione DS in RMO
039000080128     C                   EVAL      wRMO_TIPM=%trim(%subst(vindta:144:1))
039100080128     C                   EVAL      wRMO_CLIM=%trim(%subst(vindta:1:11))
039200080128     C                   EVAL      wRMO_TSER=%trim(%subst(vindta:12:1))
039300080128     C                   EVAL      VABRMO=wDS_RMO
039400080128     C*
039500050628     C* CAP
039600090203     C                   EVAL      VABCAD=%trim(%subst(vindta:107:5))
039700090202     C*                  EVAL      PiStr=%trim(%subst(vindta:107:5))
039800090202     C*                  EXSR      CHKNUM
039900090202     C*                  IF        PiInt=*on
040000090202     C*                  Z-ADD     PiVal         Num5_0
040100090202     C*                  MOVEL(P)  Num5_0        VABCAD
040200090202     C*                  ELSE
040300090202     C*                  ADD       1             errore
040400090202     C*                  EVAL      vinmsg = %trimr(vinmsg)
040500090202     C*                            + ' ' + 'VABCAD'
040600090202     C*                  ENDIF
040700090203     C*
040800090203     C* Gestione particolare x traffici export
040900090203     C                   IF        %subst(vindta:350:15) <> *blanks
041000090203     C                   EVAL      VABNZD=%trim(%subst(vindta:350:3))
041100090203     C                   EVAL      VABCAD=%trim(%subst(vindta:350+3+1:15-3-1))
041200090203     C                   ENDIF
041300090203     C*
041400040506     C* Reperisco la provincia dal CAP e dalla località
041500040526     C                   IF        VABCAD <> *blanks AND
041600040526     C                             VABPRD  = *blanks
041700040506     C                   CLEAR                   TISI95DS
041800040506     C                   EVAL      I95TCN = '3'
041900040506     C                   Z-ADD     datcor        I95DAT
042000040506     C                   EVAL      I95CAP = VABCAD
042100040506     C                   EVAL      I95LOC = VABLOD
042200050627     C                   EVAL      I95NAR = VABNZD
042300040506     C                   CALL      'TISI95R'
042400040506     C                   PARM                    TISI95DS
042500040506     C                   EVAL      VABPRD = O95PRV
042600040506     C                   ENDIF
042700060307     C* NCL
042800070103     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
042900060307     C                   EXSR      CHKNUM
043000060307     C                   IF        PiInt=*on
043100060307     C                   ADD       PiVal         VABNCL
043200060307     C                   ELSE
043300060307     C                   ADD       1             errore
043400060307     C                   EVAL      vinmsg = %trimr(vinmsg)
043500060307     C                             + ' ' + 'VABNCL'
043600060307     C                   ENDIF
043700040506     C* PKB
043800070103     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
043900010201     C                   EXSR      CHKNUM
044000010201     C                   IF        PiNum=*on
044100060316     C                   Z-ADD     PiVal         VABPKB
044200010201     C                   ELSE
044300010201     C                   ADD       1             errore
044400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
044500010201     C                             + ' ' + 'VABPKB'
044600010201     C                   ENDIF
044700070103     C* RMN
044800070103     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
044900050928     C                   EXSR      CHKNUM
045000050928     C                   IF        PiInt=*on
045100050928     C                   Z-ADD     PiVal         VABRMN
045200050928     C                   ELSE
045300050928     C                   ADD       1             errore
045400050928     C                   EVAL      vinmsg = %trimr(vinmsg)
045500070103     C                             + ' ' + 'VABRMN'
045600050928     C                   ENDIF
045700060307     C* CAS
045800070103     C                   IF        %subst(vindta:277:9) <> *all'0'
045900070102     C                   EVAL      VABVCA = 'EUR'
046000060307     C                   EVAL      FlgCAS = '1'
046100070103     C                   EVAL      PiStr=%trim(%subst(vindta:277:9))
046200060307     C                   EXSR      CHKNUM
046300060307     C                   IF        PiNum=*on
046400060316     C                   Z-ADD     PiVal         VABCAS
046500060307     C                   ELSE
046600060307     C                   ADD       1             errore
046700060307     C                   EVAL      vinmsg = %trimr(vinmsg)
046800060307     C                             + ' ' + 'VABCAS'
046900060307     C                   ENDIF
047000070102     C                   ENDIF
047100060316     C* IAS
047200070103     C                   IF        %subst(vindta:286:9) <> *all'0'
047300070102     C                   EVAL      VABVAS = 'EUR'
047400070103     C                   EVAL      PiStr=%trim(%subst(vindta:286:9))
047500060316     C                   EXSR      CHKNUM
047600060316     C                   IF        PiNum=*on
047700060316     C                   Z-ADD     PiVal         VABIAS
047800060316     C                   ELSE
047900060316     C                   ADD       1             errore
048000060316     C                   EVAL      vinmsg = %trimr(vinmsg)
048100060316     C                             + ' ' + 'VABIAS'
048200070102     C                   ENDIF
048300060316     C                   ENDIF
048400010205     C*
048500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
048600040802     C                   IF        FlgCAS <> '0'
048700070102     C                   IF        VABCBO = '1'
048800010205     C                   EVAL      VABCBO = '4'
048900010205     C                   ELSE
049000070102     C                   EVAL      VABCBO = '6'
049100070102     C                   ENDIF
049200010205     C                   ENDIF
049300020305     C*
049400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049500011113     C                   EXSR      CHKIMPDIV
049600010202     C*
049700000801     C* Ebbene...
049800000801     C                   ADD       1             §CTRMO
049900010201     C                   IF        errore <> *zeros
050000000801     C                   ADD       1             §CTRNO
050100000801     C                   EVAL      recko = vindta
050200000801     C                   EXCEPT    rigadett
050300000801     C                   EVAL      vinflg = '2'
050400000801     C                   ELSE
050500010201     C                   ADD       1             §CTROKVB
050600000801     C                   ENDIF
050700000801     C*
050800000801     C                   ENDSR
050900070102     C*----------------------------------------------------*
051000070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
051100070102     C*----------------------------------------------------*
051200070103     C     EXEVATE       BEGSR
051300070102     C*
051400070103     C                   EVAL      VATTRC='E'
051500080123     C*
051600080123     C* Considerazioni particolari x anomalia cliente in generazione file input bolle
051700080123     C* (talvolta "chi sono" sul file lungo 10 e talvolta invece lungo 15)
051800080123     C                   EVAL      VATNOT = %subst(vindta:308:5)  +
051900080123     C                                      %subst(vindta:319:26)
052000080123     C                   IF        %len(%trim(%subst(vindta:319:26))) = 10
052100080123     C                   IF        %subst(vindta:319:5)=%subst(vindta:308:5)
052200080123     C                   EVAL      VATNOT = %subst(vindta:319:26)
052300080123     C                   ENDIF
052400080123     C                   ENDIF
052500080123     C*
052600070102     C                   exsr      wrivat                                       => scarico VAT
052700100113     C*
052800100113     C* Effettuo conteggio tipologia colli
052900100113     C                   if        %subst(vindta:144:1) = 'B'
053000100113     C                   add       1             wNAS_TNCL_B
053100100113     C                   endif
053200100113     C                   if        %subst(vindta:144:1) = 'C'
053300100113     C                   add       1             wNAS_TNCL_C
053400100113     C                   endif
053500070102     C*
053600070102     C                   ENDSR
053700060307     C*----------------------------------------------------*
053800060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
053900060307     C*----------------------------------------------------*
054000060307     C     EXEVATB       BEGSR
054100060307     C*
054200060307     C                   EVAL      VATTRC='B'
054300070103     C                   EVAL      VATNOT = %trim(%subst(vindta:92:15))
054400060307     C                   exsr      wrivat                                       => scarico VAT
054500060307     C*
054600060307     C                   ENDSR
054700010201     C*----------------------------------------------------*
054800040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
054900010201     C*----------------------------------------------------*
055000020305     C     WRIVAT        BEGSR
055100050628     C*
055200060223     C* Scrivo solo se valorizzato qualcosa
055300060223     C                   IF        VATNOT <> *blanks
055400040802     C                   WRITE     FIVAT000
055500060223     C                   ENDIF
055600010201     C*
055700010201     C                   ENDSR
055800010202     C*----------------------------------------------------*
055900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
056000010202     C*----------------------------------------------------*
056100020305     C     PREVAT        BEGSR
056200010202     C*
056300021113     C* Compongo il nome del membro da dare al FIVATWWR
056400010202     C                   eval      parmbr = vlrhdl
056500010202     C                   movel     'M'           parmbr
056600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
056700010202     C                   eval      paropz = '1'
056800010202     C* Effettuo la chiamata al CLLE preposto
056900040506     C                   call(e)   'TITVVTC'
057000010202     C                   parm                    parccm
057100010202     C                   parm                    parmbr
057200010202     C                   parm                    paropz
057300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057400010202     C                   if        %error
057500010202     C                   movel     '1'           chkcall
057600010202     C                   else
057700010202     C                   movel     '0'           chkcall
057800010202     C                   endif
057900010202     C*
058000010202     C                   ENDSR
058100000801     C*----------------------------------------------------*
058200000801     C*  CONTROLLO NUMERICITA' CAMPI
058300000801     C*----------------------------------------------------*
058400000801     C     CHKNUM        BEGSR
058500000801     C*
058600000801     C                   call(e)   'ISNUMERIC'
058700000801     C                   PARM                    PiStr            30
058800070102     C                   PARM      '.'           PiDecChr          1
058900000801     C                   PARM      *ZEROS        PiVal            30 9
059000000801     C                   PARM      '0'           PiInt             1
059100000801     C                   PARM      '0'           PiNum             1
059200000801     C                   IF        %error
059300000801     C                   EVAL      PiInt=*off
059400000801     C                   ENDIF
059500000801     C*
059600000801     C                   ENDSR
059700000801     C***
059800000801
059900011113
060000011113     C*----------------------------------------------------*
060100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060200011113     C*----------------------------------------------------*
060300011113     C     CHKIMPDIV     BEGSR
060400011113     C*
060500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060600011113     C                   Z-ADD     *zeros        wrkDec            9 9
060700011113     C*
060800011113     C* Come prima cosa effettuo considerazioni sulla divisa
060900011113     C                   IF        vabIAS > *zeros
061000011113     C                   IF        vabVAS <> 'EUR'
061100011113     C                   EVAL      vabVAS =  'ITL'
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C                   IF        vabCAS > *zeros
061600011113     C                   IF        vabVCA <> 'EUR'
061700011113     C                   EVAL      vabVCA =  'ITL'
061800011113     C                   ENDIF
061900011113     C                   ENDIF
062000011113     C*
062100011113     C                   IF        vabVMD > *zeros
062200020305     C                   IF        vabVAD <> 'EUR'
062300011113     C                   EVAL      vabVAD =  'ITL'
062400011113     C                   ENDIF
062500011113     C                   ENDIF
062600011113     C*
062700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062800011113     C                   Z-ADD     vabIAS        wrkDec
062900011113     C                   IF        wrkDec > *zeros
063000011113     C                   IF        vabVAS = 'ITL'
063100011113     C                   EVAL      vabIAS = *zeros
063200011113     C                   ENDIF
063300011113     C                   ENDIF
063400011113     C*
063500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063600011113     C                   Z-ADD     vabCAS        wrkDec
063700011113     C                   IF        wrkDec > *zeros
063800011113     C                   IF        vabVCA = 'ITL'
063900011113     C                   EVAL      vabCAS = *zeros
064000011113     C                   ENDIF
064100011113     C                   ENDIF
064200011113     C*
064300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064400011113     C                   Z-ADD     vabVMD        wrkDec
064500011113     C                   IF        wrkDec > *zeros
064600011113     C                   IF        vabVAD = 'ITL'
064700011113     C                   EVAL      vabVMD = *zeros
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C                   ENDSR
065200011113     C***
065300011113
065400011113
065500000801
065600000801
065700990920      /TITLE Invio dei dati al punto operativo.
065800010202     C     invio         BEGSR
065900990920     C*
066000021113     C* 1° invio FIVAT
066100010201     C                   reset                   dscmz
066200010201     C                   move      vlrpoi        cmzdst
066300021113     C                   eval      cmzfld = 'FIVATWWR'
066400010201     C                   eval      cmzmbd = vlrhdl
066500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066600021009     C***                if        prmfir = *blanks
066700021113     C                   eval      cmzfla = 'FIVAT00F'
066800021113     C                   eval      cmzmba = 'FIVAT00F'
066900021009     C***                else
067000021009     C***                eval      cmzfla = prmfir
067100021009     C***                eval      cmzmba = prmfir
067200021009     C***                endif
067300010201     C                   eval      cmznrr = *zeros
067400020305     C                   move      §ctrokvt      cmznrr
067500021018     C                   eval      cmzlba = vlrfl1
067600010201     C                   call(e)   'TIS711C'
067700010201     C                   parm                    dscmz
067800010201     C                   parm      *blanks       esito
067900010205     C                   if        %error
068000010205     C                             or cmzerr = '1'
068100010205     C                             or esito  = '1'
068200010205     C                   eval      wrkesito = '3'
068300010205     C                   else
068400010201     C*
068500021113     C* 2° invio FIVAB
068600010201     C                   reset                   dscmz
068700010201     C                   move      vlrpoi        cmzdst
068800010201     C                   eval      cmzfld = vlrfou
068900010201     C                   eval      cmzmbd = vlrhdl
069000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069100021009     C***                if        prmfir = *blanks
069200021113     C                   eval      cmzfla = 'FIVAB00F'
069300021113     C                   eval      cmzmba = 'FIVAB00F'
069400021009     C***                else
069500021009     C***                eval      cmzfla = prmfir
069600021009     C***                eval      cmzmba = prmfir
069700021009     C***                endif
069800010201     C                   eval      cmznrr = *zeros
069900010201     C                   move      §ctrokvb      cmznrr
070000021018     C                   eval      cmzlba = vlrfl1
070100010201     C                   call(e)   'TIS711C'
070200010201     C                   parm                    dscmz
070300010201     C                   parm      *blanks       esito
070400010201     C                   if        %error
070500010201     C                             or cmzerr = '1'
070600010201     C                             or esito  = '1'
070700010201     C                   eval      wrkesito = '3'
070800010201     C                   endif
070900010205     C                   endif
071000990920     C*
071100000613     C                   ENDSR
071200000613     C***
071300070411
071400070411     C     *pssr         BEGSR
071500070411     C*
071600070411     C                   if        %open(tivin00r)
071700070411     C                   close     tivin00r
071800070411     C                   endif
071900070411     C                   if        %open(fivabwwr)
072000070411     C                   close     fivabwwr
072100070411     C                   endif
072200070411     C                   if        %open(fivatwwr)
072300070411     C                   close     fivatwwr
072400070411     C                   endif
072500070411     C*
072600070411     C* Effettuo la chiamata al CLLE preposto
072700070411     C                   call(e)   'TITVVTC'
072800070411     C                   parm                    parccm
072900070411     C                   parm                    parmbr
073000070411     C                   parm      '2'           paropz
073100070411     C*
073200070411     C                   eval      wrkesito = '2'
073300070411     C*
073400070411     C                   seton                                        LR
073500070411     C*
073600070411     C                   ENDSR     '*CANCL'
073700070411     C***
073800070411
073900990910
074000000613     C     *inzsr        BEGSR
074100990910     C*
074200990910     C     *entry        plist
074300990920     C                   parm                    tivlrds
074400990921     C                   parm      wrkesito      esito
074500000724     C                   parm                    prmlit
074600000710     C                   parm                    prmfir
074700000613     C*
074800000830     C* CALCOLA LA DATA CORRENTE
074900100113     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
075000100113     C                   eval      datcor = %dec(%date() : *ISO)
075100000830     C*
075200000613     C                   ENDSR
075300000613     C***
075400990908
075500070103     Otitvw3p   E            riepilogo         2
075600990915     O                                              'Upload via Internet'
075700990915     O                                           +1 'Traduzione TIVIN00R -'
075800021113     O                                           +1 'FIVABWWR/FIVATWWR'
075900010201     O                                           +1 'Report testate bolle'
076000990915     O          E            riepilogo   2
076100990915     O                       wrkdata
076200990915     O                       wrkora              +1
076300990915     O                       procname            +1
076400990915     O          E            riepilogo   2
076500990915     O                                              'Cliente..................:'
076600990915     O                       VABCCM        z     +1
076700990915     O          E            riepilogo   2
076800990920     O                                              'Riferimento Strategi.....:'
076900990920     O                       vlrhdl              +1
077000990915     O          E            riepilogo   2
077100990915     O                                              'Giusti...................:'
077200010201     O                       §CTROKVB      2   +  1
077300990915     O          E            riepilogo   2
077400990915     O                                              'Sbagliati e corretti.....:'
077500971022     O                       §CTRMO        2   +  1
077600990915     O          E            riepilogo   2
077700990915     O                                              'Sbagliati e scartati.....:'
077800971022     O                       §CTRNO        2   +  1
077900000613
078000070103     Otitvw3ps  E            testdett          2
078100000613     O                                              'Upload via Internet'
078200000613     O                                           +1 'Traduzione TIVIN00R -'
078300021113     O                                           +1 'FIVABWWR/FIVATWWR'
078400010201     O                                           +1 'Report testate bolle'
078500000616     O          E            testdett    3
078600000613     O                                           +2 'N° rec'
078700000613     O                                           +3 'Anteprima contenuto'
078800000616     O          E            rigadett    2
078900000613     O                       rrnum               +2
079000000621     O                       recko               +3
079100000616     O          E            findett     2
079200000613     O                       wrkdata
079300000613     O                       wrkora              +1
079400000613     O                       procname            +1
079500000616     O          E            findett     2
079600000613     O                                              'Cliente..................:'
079700000613     O                       VABCCM        z     +1
079800000616     O          E            findett     2
079900000613     O                                              'Riferimento Strategi.....:'
080000000613     O                       vlrhdl              +1
080100000616     O          E            findett     2
080200000613     O                                              'Giusti...................:'
080300010201     O                       §CTROKVB      2   +  1
080400000616     O          E            findett     2
080500000613     O                                              'Sbagliati e corretti.....:'
080600000613     O                       §CTRMO        2   +  1
080700000616     O          E            findett     2
080800000613     O                                              'Sbagliati e scartati.....:'
080900000613     O                       §CTRNO        2   +  1
081000000512** CMD - COMANDI CL
081100070103OVRPRTF FILE(TITVW3P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
081200070103OVRPRTF FILE(TITVW3PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
081300070103DLTOVR FILE(TITVW3P TITVW3PS) LVL(*)
081400000512
081500000512
