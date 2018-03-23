000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800070604     Ftitvx6p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070604     Ftitvx6ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200990908
001300000512     D*------------
001400000512     D* COMANDI
001500000512     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000801     D*----------------------------------------------------
001800000801     D* DICHIARAZIOINE VARIABILI DI WRK
001900000801     D*----------------------------------------------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300990920     D tivlrds       e ds                  extname(tivlr00f)
002400060307     D tisi95ds      e ds
002500990910     D esito           s              1
002600000724     D prmlit          s             10
002700000710     D prmfir          s             10
002800990921     D wrkesito        s                   like(esito)
002900990915     D wrkdata         s               d
003000990915     D wrkora          s               t
003100000613     D rrnum           s              6  0 INZ(*zeros)
003200000621     D recko           s            150    INZ(*blanks)
003300011113     D depcmd          s            150    INZ(*blanks)
003400010202     D parccm          s              8    INZ(*blanks)
003500010202     D parmbr          s             10    INZ(*blanks)
003600010202     D paropz          s              1    INZ(*blanks)
003700010202     D chkcall         s              1    INZ(*blanks)
003800000830
003900000830     D*------------------
004000000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004100000830     D*------------------
004200000830     D WLBDA8          DS                  INZ
004300000830     D  G08DAT                 1      8  0
004400000830     D  G08INV                 9     16  0
004500000830     D  G08ERR                17     17
004600000830     D  G08TGI                18     22  0
004700000830     D*
004800070709     D*-------------------
004900070709     D* COSTANTI
005000070709     D*-------------------
005100070710     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005200070710     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005300010201
005400010201
005500990915     C                   time                    wrkdata
005600990915     C                   time                    wrkora
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000000724     C*
006100000724     C* SE OCCORRE SPEDIRE IN FILIALE
006200000724     C                   if        vlrpoi <> *zeros
006300000724     C*
006400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006600000724     C     vlrpoi        chain     azorg01l
006700000724     C                   if        %found
006800000616     C                   movel(p)  CMD(1)        depcmd
006900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007000000616     C*
007100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007200011113     C                   Z-ADD     150           LENGH            15 5
007300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007400000616     C                   PARM                    depcmd
007500000616     C                   PARM                    LENGH
007600000724     C*
007700000724     C                   endif
007800000724     C                   endif
007900000616     C*
008000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008100000616     C                   movel(p)  CMD(2)        depcmd
008200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008300000616     C*
008400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000616     C                   PARM                    depcmd
008800000616     C                   PARM                    LENGH
008900000616     C*
009000070604     C                   if        not %open(titvx6ps)
009100070604     C                   open      titvx6ps
009200000616     C                   except    testdett
009300000613     C                   endif
009400000613     C*
009500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009700000613     C*
009800010202     C* Effettuo la chiamata al CLLE preposto
009900040506     C                   call(e)   'TITVVTC'
010000010202     C                   parm                    parccm
010100010202     C                   parm                    parmbr
010200010202     C                   parm      '2'           paropz
010300050201     C*
010400050201     C* Effettuo lancio TISI95 solo x chiusura
010500050201     C                   CLEAR                   TISI95DS
010600050201     C                   EVAL      I95TLA = 'C'
010700050201     C                   CALL      'TISI95R'
010800050201     C                   PARM                    TISI95DS
010900010202     C*
011000070604     C                   if        %open(titvx6ps)
011100000616     C                   except    findett
011200070604     C                   close     titvx6ps
011300000613     C                   endif
011400000616     C*
011500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011600011113     C                   Z-ADD     150           LENGH            15 5
011700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011800000616     C                   PARM                    CMD(3)
011900000616     C                   PARM                    LENGH
012000000616     C*
012100000801     C
012200010201     C                   seton                                        LR
012300000613
012400000613
012500000613     C*--------------------------------------------------------
012600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012700000613     C*--------------------------------------------------------
012800000621     C     STPR          BEGSR
012900000613     C*
013000070604     C                   if        not %open(titvx6p)
013100070604     C                   open      titvx6p
013200990915     C                   endif
013300990915     C*
013400990915     C                   except    riepilogo
013500990915     C*
013600070604     C                   if        %open(titvx6p)
013700070604     C                   close     titvx6p
013800990914     C                   endif
013900990910     C*
014000000613     C                   ENDSR
014100000613     C***
014200990908
014300000801
014400910830     C*--------------------------------------------------------
014500040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014600910830     C*--------------------------------------------------------
014700040526     C     RWFILE        BEGSR
014800990910     C*
014900990914     C                   if        not %open(tivin00r)
015000990908     C                   open      tivin00r
015100990914     C                   endif
015200021113     C                   if        not %open(fivabwwr)
015300021113     C                   open      fivabwwr
015400990914     C                   endif
015500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015600020305     C                   exsr      prevat
015700010201     C*
015800010202     C                   if        chkcall = '0'
015900010202     C*
016000021113     C                   if        not %open(fivatwwr)
016100021113     C                   open      fivatwwr
016200010201     C                   endif
016300990910     C*
016400010201     C                   clear                   §CTROKVB          5 0
016500020305     C                   clear                   §CTROKVT          5 0
016600000801     C                   clear                   §CTRMO            5 0
016700000801     C                   clear                   §CTRNO            5 0
016800990910     C*
016900921023     C                   DO        *HIVAL
017000990913     C*
017100990915     C                   READ      tivin00r                               70
017200050627     C                   if        vindta > *blanks
017300000613     C                   add       1             rrnum
017400000801     C*
017500000801     C                   if        *in70 = *off
017600000801     C                             and
017700000801     C                             (vinflg = *blanks
017800000801     C                              or vinflg = '0'
017900000801     C                              or vinflg = '2')
018000000801     C*
018100000801     C                   clear                   vinmsg
018200000801     C                   eval      vinflg = '1'
018300070402     C*
018400070604     C* Verifico il tipo record => se testata o dettaglio
018500070604     C                   if        %subst(vindta:194:12) <> *blanks
018600070402     C                   clear                   fivab000
018700070604     C                   clear                   fivat000
018800070604     C                   exsr      inzvar
018900070604     C                   exsr      defcam
019000070604     C                   exsr      impvab                                       => carico   VAB
019100070604     C                   exsr      wrivab                                       => scarico  VAB
019200070604     C                   else
019300070604     C                   exsr      exevate                                      => gestione VAT
019400070604     C                   endif
019500000905     C*
019600000905     C                   else
019700000905     C                   eval      vinflg = '1'
019800050628     C                   endif
019900000905     C                   endif
020000000905     C*
020100000905     C  N70              update    tivin000
020200000905     C*
020300991022     C  N70              ENDdo
020400010202     C*
020500010202     C                   endif
020600990910
020700990910     C* Se non ci sono record con errori ...
020800000710     C                   if        §ctrno = 0
020900990910     C* ... restituisco esito OK.
021000990921     C                   eval      wrkesito = '0'
021100990910     C                   else
021200010201     C                   if        §ctrokvb > 0
021300990921     C                   eval      wrkesito = '1'
021400000710     C                   else
021500000710     C                   eval      wrkesito = '2'
021600990910     C                   endif
021700000710     C                   endif
021800990910     C*
021900990914     C                   if        %open(tivin00r)
022000990908     C                   close     tivin00r
022100990914     C                   endif
022200021113     C                   if        %open(fivabwwr)
022300021113     C                   close     fivabwwr
022400990914     C                   endif
022500021113     C                   if        %open(fivatwwr)
022600021113     C                   close     fivatwwr
022700010201     C                   endif
022800990910     C*
022900010201     C                   if        §ctrokvb > 0
023000000724     C                             and vlrpoi <> *zeros
023100010202     C                   exsr      invio
023200990920     C                   endif
023300990920     C*
023400910830     C                   ENDSR
023500000613     C***
023600010305
023700010305     C*----------------------------------------------------*
023800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
023900010305     C*----------------------------------------------------*
024000020305     C     WRIVAB        BEGSR
024100010305     C*
024200060225     C* Quindi scarico il buffer del file d testata
024300021113     C                   write     fivab000                                     => scarico il VAB
024400010305     C*
024500010305     C                   ENDSR
024600990920
024700000801     C*----------------------------------------------------*
024800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024900000801     C*----------------------------------------------------*
025000010201     C     INZVAR        BEGSR
025100000801     C*
025200040802     C                   Z-ADD     *zeros        Num5_0            5 0
025300040802     C                   MOVEL     '0'           FlgCAS            1
025400000801     C*
025500000801     C                   ENDSR
025600000801     C*----------------------------------------------------*
025700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
025800000801     C*----------------------------------------------------*
025900000801     C     DEFCAM        BEGSR
026000000801     C*
026100020619     C* Imposto i valori di default...
026200070604     C                   Z-ADD     0013422       VABCCM
026300070604     C                   Z-ADD     0013422       VATCCM
026400070604     C                   Z-ADD     001           VABLNP
026500070604     C                   Z-ADD     001           VATLNP
026600040714     C                   Z-ADD     000           VABCTR
026700040823     C                   MOVEL     '1'           VABCBO
026800070402     C                   MOVEL     '7Q'          VABCTM
026900070726     C                   EVAL      VABRMO = 'LIMONI'
027000020619     C* ... e poi verifico se sono stati passati come parametri
027100020619     C                   IF        vlrppt > *blanks
027200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
027300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027400020619     C                   EXSR      CHKNUM
027500020619     C                   IF        PiInt=*on
027600020619     C                   Z-ADD     PiVal         VABCCM
027700020619     C                   Z-ADD     PiVal         VATCCM
027800020619     C                   ENDIF
027900040506     C                   ENDIF
028000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
028100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028200020619     C                   EXSR      CHKNUM
028300020619     C                   IF        PiInt=*on
028400020619     C                   Z-ADD     PiVal         VABLNP
028500020619     C                   Z-ADD     PiVal         VATLNP
028600040506     C                   ENDIF
028700020619     C                   ENDIF
028800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
028900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029000020619     C                   EXSR      CHKNUM
029100020619     C                   IF        PiInt=*on
029200020619     C                   Z-ADD     PiVal         VABCTR
029300040506     C                   ENDIF
029400020619     C                   ENDIF
029500020619     C                   ENDIF
029600000801     C*
029700000801     C                   ENDSR
029800000801     C*----------------------------------------------------*
029900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030000000801     C*----------------------------------------------------*
030100040823     C     IMPVAB        BEGSR
030200010305     C*
030300000801     C                   Z-ADD     *zeros        errore            1 0
030400000830     C                   MOVEL     datcor        VABAAS
030500020305     C                   MOVEL     datcor        VATAAS
030600040526     C                   MOVE      datcor        VABMGS
030700040823     C                   MOVE(P)   vlrpoi        VABFGS
030800040823     C                   MOVE(P)   vlrpoi        VATFGS
030900050628     C*
031000070604     C                   EVAL      VABRSD=%trim(%subst(vindta:6:30))
031100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031300020117     C* ==
031400070710     C*
031500070710     C* Porto la ragione sociale destinatario da minuscolo a MAIUSCOLO
031600070710     C     minu:maiu     XLATE     VABRSD        VABRSD                         *Minus -> Maiuscolo
031700070710     C*
031800070710     C* Tolgo i numeri all'inizio della ragione sociale e li metto nel RMA
031900070710     C                   Z-ADD     *zeros        I                 2 0
032000070710     C                   FOR       I = 1 TO %size(VABRSD)
032100070710     C                   IF        %subst(VABRSD:I:1) >= 'A' AND
032200070710     C                             %subst(VABRSD:I:1) <= 'Z'
032300070710     C                   LEAVE
032400070710     C                   ENDIF
032500070710     C                   ENDFOR
032600071214     C*
032700071214     C* Stabilisco il codice cliente secondo la seguente corrispondenza gestita dal cliente
032800071214     C                   IF        %trim(%subst(vindta:1:5))='00001'
032900090918     C***                EVAL      VABCCM=0013422
033000090918     C***                EVAL      VATCCM=0013422
033100090918     C                   EVAL      VABCCM=0014633
033200090918     C                   EVAL      VATCCM=0014633
033300071214     C                   ENDIF
033400071214     C                   IF        %trim(%subst(vindta:1:5))='00002'
033500090806     C***                EVAL      VABCCM=0013580
033600090806     C***                EVAL      VATCCM=0013580
033700090806     C                   EVAL      VABCCM=0013496
033800090806     C                   EVAL      VATCCM=0013496
033900071214     C                   ENDIF
034000090918     C***                IF        %trim(%subst(vindta:1:5))='00003'
034100090918     C***                EVAL      VABCCM=0261521
034200090918     C***                EVAL      VATCCM=0261521
034300090918     C***                ENDIF
034400090918     C***                IF        %trim(%subst(vindta:1:5))='00004'
034500090918     C***                EVAL      VABCCM=0261648
034600090918     C***                EVAL      VATCCM=0261648
034700090918     C***                ENDIF
034800071214     C*
034900070710     C                   EVAL      VABRMA = %subst(VABRSD:1:I-1)
035000070710     C                   EVAL      VABRSD = %subst(VABRSD:I)
035100070709     C* ==
035200070604     C                   EVAL      VABIND=%trim(%subst(vindta:36:30))
035300070604     C                   EVAL      VABLOD=%trim(%subst(vindta:66:20))
035400070604     C                   EVAL      VABPRD=%trim(%subst(vindta:91:2))
035500070710     C***                EVAL      VABRMA=%trim(%subst(vindta:1:5))
035600050628     C* CAP
035700070604     C                   EVAL      PiStr=%trim(%subst(vindta:86:5))
035800010201     C                   EXSR      CHKNUM
035900010201     C                   IF        PiInt=*on
036000010201     C                   Z-ADD     PiVal         Num5_0
036100040506     C                   MOVEL(P)  Num5_0        VABCAD
036200010201     C                   ELSE
036300070604     C                   ADD       1             errore
036400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036500040506     C                             + ' ' + 'VABCAD'
036600010201     C                   ENDIF
036700040506     C* Reperisco la provincia dal CAP e dalla località
036800040526     C                   IF        VABCAD <> *blanks AND
036900040526     C                             VABPRD  = *blanks
037000040506     C                   CLEAR                   TISI95DS
037100040506     C                   EVAL      I95TCN = '3'
037200040506     C                   Z-ADD     datcor        I95DAT
037300040506     C                   EVAL      I95CAP = VABCAD
037400040506     C                   EVAL      I95LOC = VABLOD
037500050627     C                   EVAL      I95NAR = VABNZD
037600040506     C                   CALL      'TISI95R'
037700040506     C                   PARM                    TISI95DS
037800040506     C                   EVAL      VABPRD = O95PRV
037900040506     C                   ENDIF
038000060307     C* NCL
038100070604     C                   EVAL      PiStr=%trim(%subst(vindta:129:5))
038200060307     C                   EXSR      CHKNUM
038300060307     C                   IF        PiInt=*on
038400070604     C                   Z-ADD     PiVal         VABNCL
038500060307     C                   ELSE
038600070604     C                   ADD       1             errore
038700060307     C                   EVAL      vinmsg = %trimr(vinmsg)
038800060307     C                             + ' ' + 'VABNCL'
038900060307     C                   ENDIF
039000040506     C* PKB
039100070604     C                   EVAL      PiStr=%trim(%subst(vindta:134:4))
039200010201     C                   EXSR      CHKNUM
039300010201     C                   IF        PiNum=*on
039400060316     C                   Z-ADD     PiVal         VABPKB
039500010201     C                   ELSE
039600070604     C                   ADD       1             errore
039700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
039800010201     C                             + ' ' + 'VABPKB'
039900010201     C                   ENDIF
040000070608     C* NSP
040100070604     C                   EVAL      PiStr=%trim(%subst(vindta:194:12))
040200050928     C                   EXSR      CHKNUM
040300050928     C                   IF        PiInt=*on
040400060316     C                   Z-ADD     PiVal         VABNSP
040500050928     C                   ELSE
040600050928     C                   ADD       1             errore
040700050928     C                   EVAL      vinmsg = %trimr(vinmsg)
040800070608     C                             + ' ' + 'VABNSP'
040900050928     C                   ENDIF
041000070608     C* RMN
041100070709     C***                EVAL      PiStr=%trim(%subst(vindta:1:5))
041200070709     C                   EVAL      PiStr=%trim(%subst(vindta:194:12))
041300070608     C                   EXSR      CHKNUM
041400070608     C                   IF        PiInt=*on
041500070608     C                   Z-ADD     PiVal         VABRMN
041600070608     C                   ELSE
041700070608     C                   ADD       1             errore
041800070608     C                   EVAL      vinmsg = %trimr(vinmsg)
041900070608     C                             + ' ' + 'VABRMN'
042000070608     C                   ENDIF
042100060307     C* CAS
042200070604     C                   IF        %trim(%subst(vindta:140:11)) <> *blanks
042300071214     C                   EVAL      VABTIC = 'BM'
042400070102     C                   EVAL      VABVCA = 'EUR'
042500060307     C                   EVAL      FlgCAS = '1'
042600070604     C                   EVAL      PiStr=%trim(%subst(vindta:140:11))
042700060307     C                   EXSR      CHKNUM
042800060307     C                   IF        PiNum=*on
042900070604     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
043000060316     C                   Z-ADD     PiVal         VABCAS
043100060307     C                   ELSE
043200070604     C                   ADD       1             errore
043300060307     C                   EVAL      vinmsg = %trimr(vinmsg)
043400060307     C                             + ' ' + 'VABCAS'
043500060307     C                   ENDIF
043600070102     C                   ENDIF
043700010205     C*
043800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043900040802     C                   IF        FlgCAS <> '0'
044000070102     C                   IF        VABCBO = '1'
044100010205     C                   EVAL      VABCBO = '4'
044200010205     C                   ELSE
044300070102     C                   EVAL      VABCBO = '6'
044400070102     C                   ENDIF
044500010205     C                   ENDIF
044600020305     C*
044700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044800011113     C                   EXSR      CHKIMPDIV
044900010202     C*
045000000801     C* Ebbene...
045100000801     C                   ADD       1             §CTRMO
045200010201     C                   IF        errore <> *zeros
045300000801     C                   ADD       1             §CTRNO
045400000801     C                   EVAL      recko = vindta
045500000801     C                   EXCEPT    rigadett
045600000801     C                   EVAL      vinflg = '2'
045700000801     C                   ELSE
045800010201     C                   ADD       1             §CTROKVB
045900000801     C                   ENDIF
046000000801     C*
046100000801     C                   ENDSR
046200070402     C*----------------------------------------------------*
046300070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
046400070402     C*----------------------------------------------------*
046500070402     C     EXEVATE       BEGSR
046600070402     C*
046700070402     C                   EVAL      VATTRC='E'
046800070604     C* NSP
046900070604     C                   EVAL      PiStr=%trim(%subst(vindta:17:12))
047000070604     C                   EXSR      CHKNUM
047100070604     C                   IF        PiInt=*on
047200070604     C                   Z-ADD     PiVal         VATNSP
047300070604     C                   ELSE
047400070604     C                   ADD       1             errore
047500070604     C                   Z-ADD     *zeros        VATNSP
047600070604     C                   EVAL      vinmsg = %trimr(vinmsg)
047700070604     C                             + ' ' + 'VATNSP'
047800070604     C                   ENDIF
047900070604     C*
048000070604     C                   EVAL      VATNOT = %trim(%subst(vindta:29:20))
048100070402     C                   exsr      wrivat                                       => scarico VAT
048200070402     C*
048300070402     C                   ENDSR
048400010201     C*----------------------------------------------------*
048500040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048600010201     C*----------------------------------------------------*
048700020305     C     WRIVAT        BEGSR
048800050628     C*
048900060223     C* Scrivo solo se valorizzato qualcosa
049000060223     C                   IF        VATNOT <> *blanks
049100040802     C                   WRITE     FIVAT000
049200060223     C                   ENDIF
049300010201     C*
049400010201     C                   ENDSR
049500010202     C*----------------------------------------------------*
049600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049700010202     C*----------------------------------------------------*
049800020305     C     PREVAT        BEGSR
049900010202     C*
050000021113     C* Compongo il nome del membro da dare al FIVATWWR
050100010202     C                   eval      parmbr = vlrhdl
050200010202     C                   movel     'M'           parmbr
050300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050400010202     C                   eval      paropz = '1'
050500010202     C* Effettuo la chiamata al CLLE preposto
050600040506     C                   call(e)   'TITVVTC'
050700010202     C                   parm                    parccm
050800010202     C                   parm                    parmbr
050900010202     C                   parm                    paropz
051000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051100010202     C                   if        %error
051200010202     C                   movel     '1'           chkcall
051300010202     C                   else
051400010202     C                   movel     '0'           chkcall
051500010202     C                   endif
051600010202     C*
051700010202     C                   ENDSR
051800000801     C*----------------------------------------------------*
051900000801     C*  CONTROLLO NUMERICITA' CAMPI
052000000801     C*----------------------------------------------------*
052100000801     C     CHKNUM        BEGSR
052200000801     C*
052300000801     C                   call(e)   'ISNUMERIC'
052400000801     C                   PARM                    PiStr            30
052500070102     C                   PARM      '.'           PiDecChr          1
052600000801     C                   PARM      *ZEROS        PiVal            30 9
052700000801     C                   PARM      '0'           PiInt             1
052800000801     C                   PARM      '0'           PiNum             1
052900000801     C                   IF        %error
053000000801     C                   EVAL      PiInt=*off
053100000801     C                   ENDIF
053200000801     C*
053300000801     C                   ENDSR
053400000801     C***
053500000801
053600011113
053700011113     C*----------------------------------------------------*
053800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053900011113     C*----------------------------------------------------*
054000011113     C     CHKIMPDIV     BEGSR
054100011113     C*
054200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054300011113     C                   Z-ADD     *zeros        wrkDec            9 9
054400011113     C*
054500011113     C* Come prima cosa effettuo considerazioni sulla divisa
054600011113     C                   IF        vabIAS > *zeros
054700011113     C                   IF        vabVAS <> 'EUR'
054800011113     C                   EVAL      vabVAS =  'ITL'
054900011113     C                   ENDIF
055000011113     C                   ENDIF
055100011113     C*
055200011113     C                   IF        vabCAS > *zeros
055300011113     C                   IF        vabVCA <> 'EUR'
055400011113     C                   EVAL      vabVCA =  'ITL'
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C                   IF        vabVMD > *zeros
055900020305     C                   IF        vabVAD <> 'EUR'
056000011113     C                   EVAL      vabVAD =  'ITL'
056100011113     C                   ENDIF
056200011113     C                   ENDIF
056300011113     C*
056400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056500011113     C                   Z-ADD     vabIAS        wrkDec
056600011113     C                   IF        wrkDec > *zeros
056700011113     C                   IF        vabVAS = 'ITL'
056800011113     C                   EVAL      vabIAS = *zeros
056900011113     C                   ENDIF
057000011113     C                   ENDIF
057100011113     C*
057200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057300011113     C                   Z-ADD     vabCAS        wrkDec
057400011113     C                   IF        wrkDec > *zeros
057500011113     C                   IF        vabVCA = 'ITL'
057600011113     C                   EVAL      vabCAS = *zeros
057700011113     C                   ENDIF
057800011113     C                   ENDIF
057900011113     C*
058000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058100011113     C                   Z-ADD     vabVMD        wrkDec
058200011113     C                   IF        wrkDec > *zeros
058300011113     C                   IF        vabVAD = 'ITL'
058400011113     C                   EVAL      vabVMD = *zeros
058500011113     C                   ENDIF
058600011113     C                   ENDIF
058700011113     C*
058800011113     C                   ENDSR
058900011113     C***
059000011113
059100011113
059200000801
059300000801
059400990920      /TITLE Invio dei dati al punto operativo.
059500010202     C     invio         BEGSR
059600990920     C*
059700021113     C* 1° invio FIVAT
059800010201     C                   reset                   dscmz
059900010201     C                   move      vlrpoi        cmzdst
060000021113     C                   eval      cmzfld = 'FIVATWWR'
060100010201     C                   eval      cmzmbd = vlrhdl
060200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060300021009     C***                if        prmfir = *blanks
060400021113     C                   eval      cmzfla = 'FIVAT00F'
060500021113     C                   eval      cmzmba = 'FIVAT00F'
060600021009     C***                else
060700021009     C***                eval      cmzfla = prmfir
060800021009     C***                eval      cmzmba = prmfir
060900021009     C***                endif
061000010201     C                   eval      cmznrr = *zeros
061100020305     C                   move      §ctrokvt      cmznrr
061200021018     C                   eval      cmzlba = vlrfl1
061300010201     C                   call(e)   'TIS711C'
061400010201     C                   parm                    dscmz
061500010201     C                   parm      *blanks       esito
061600010205     C                   if        %error
061700010205     C                             or cmzerr = '1'
061800010205     C                             or esito  = '1'
061900010205     C                   eval      wrkesito = '3'
062000010205     C                   else
062100010201     C*
062200021113     C* 2° invio FIVAB
062300010201     C                   reset                   dscmz
062400010201     C                   move      vlrpoi        cmzdst
062500010201     C                   eval      cmzfld = vlrfou
062600010201     C                   eval      cmzmbd = vlrhdl
062700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062800021009     C***                if        prmfir = *blanks
062900021113     C                   eval      cmzfla = 'FIVAB00F'
063000021113     C                   eval      cmzmba = 'FIVAB00F'
063100021009     C***                else
063200021009     C***                eval      cmzfla = prmfir
063300021009     C***                eval      cmzmba = prmfir
063400021009     C***                endif
063500010201     C                   eval      cmznrr = *zeros
063600010201     C                   move      §ctrokvb      cmznrr
063700021018     C                   eval      cmzlba = vlrfl1
063800010201     C                   call(e)   'TIS711C'
063900010201     C                   parm                    dscmz
064000010201     C                   parm      *blanks       esito
064100010201     C                   if        %error
064200010201     C                             or cmzerr = '1'
064300010201     C                             or esito  = '1'
064400010201     C                   eval      wrkesito = '3'
064500010201     C                   endif
064600010205     C                   endif
064700990920     C*
064800000613     C                   ENDSR
064900000613     C***
065000990910
065100000613     C     *inzsr        BEGSR
065200990910     C*
065300990910     C     *entry        plist
065400990920     C                   parm                    tivlrds
065500990921     C                   parm      wrkesito      esito
065600000724     C                   parm                    prmlit
065700000710     C                   parm                    prmfir
065800000613     C*
065900000830     C* CALCOLA LA DATA CORRENTE
066000000830     C                   time                    wn14             14 0
066100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066200000830     C                   z-add     wn8           g08dat
066300000830     C                   z-add     *zeros        g08inv
066400000830     C                   movel     '0'           g08err
066500000830     C                   call      'XSRDA8'
066600000830     C                   parm                    wlbda8
066700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066800000830     C*
066900000613     C                   ENDSR
067000000613     C***
067100990908
067200070604     Otitvx6p   E            riepilogo         2
067300990915     O                                              'Upload via Internet'
067400990915     O                                           +1 'Traduzione TIVIN00R -'
067500021113     O                                           +1 'FIVABWWR/FIVATWWR'
067600010201     O                                           +1 'Report testate bolle'
067700990915     O          E            riepilogo   2
067800990915     O                       wrkdata
067900990915     O                       wrkora              +1
068000990915     O                       procname            +1
068100990915     O          E            riepilogo   2
068200990915     O                                              'Cliente..................:'
068300990915     O                       VABCCM        z     +1
068400990915     O          E            riepilogo   2
068500990920     O                                              'Riferimento Strategi.....:'
068600990920     O                       vlrhdl              +1
068700990915     O          E            riepilogo   2
068800990915     O                                              'Giusti...................:'
068900010201     O                       §CTROKVB      2   +  1
069000990915     O          E            riepilogo   2
069100990915     O                                              'Sbagliati e corretti.....:'
069200971022     O                       §CTRMO        2   +  1
069300990915     O          E            riepilogo   2
069400990915     O                                              'Sbagliati e scartati.....:'
069500971022     O                       §CTRNO        2   +  1
069600000613
069700070604     Otitvx6ps  E            testdett          2
069800000613     O                                              'Upload via Internet'
069900000613     O                                           +1 'Traduzione TIVIN00R -'
070000021113     O                                           +1 'FIVABWWR/FIVATWWR'
070100010201     O                                           +1 'Report testate bolle'
070200000616     O          E            testdett    3
070300000613     O                                           +2 'N° rec'
070400000613     O                                           +3 'Anteprima contenuto'
070500000616     O          E            rigadett    2
070600000613     O                       rrnum               +2
070700000621     O                       recko               +3
070800000616     O          E            findett     2
070900000613     O                       wrkdata
071000000613     O                       wrkora              +1
071100000613     O                       procname            +1
071200000616     O          E            findett     2
071300000613     O                                              'Cliente..................:'
071400000613     O                       VABCCM        z     +1
071500000616     O          E            findett     2
071600000613     O                                              'Riferimento Strategi.....:'
071700000613     O                       vlrhdl              +1
071800000616     O          E            findett     2
071900000613     O                                              'Giusti...................:'
072000010201     O                       §CTROKVB      2   +  1
072100000616     O          E            findett     2
072200000613     O                                              'Sbagliati e corretti.....:'
072300000613     O                       §CTRMO        2   +  1
072400000616     O          E            findett     2
072500000613     O                                              'Sbagliati e scartati.....:'
072600000613     O                       §CTRNO        2   +  1
072700000512** CMD - COMANDI CL
072800070604OVRPRTF FILE(TITVX6P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
072900070604OVRPRTF FILE(TITVX6PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
073000070604DLTOVR FILE(TITVX6P TITVX6PS) LVL(*)
073100000512
073200000512
