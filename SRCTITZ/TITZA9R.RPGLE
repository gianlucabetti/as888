000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200140203      *
000300140203      * PARTICOLARITA':
000400140203      *
000500140203     H dftactgrp(*no) actgrp(*caller)
000600000313     F*
000700990910     Ftivin00r  uF   E             DISK    usropn
000800050408     FFIVABWWR  O    E             DISK    usropn
000900000313     D*
001000010330     D*----------------------------------------------------
001100010330     D* DICHIARAZIOINE VARIABILI DI WRK
001200010330     D*----------------------------------------------------
001300010330     D dscmz         e ds                  inz
001400010330     D psds           sds
001500010330     D  procname         *PROC
001600010330     D tivlrds       e ds                  extname(tivlr00f)
001700070502     D tisi95ds      e ds
001800010330     D esito           s              1
001900010330     D prmlit          s             10
002000010330     D prmfir          s             10
002100010330     D wrkesito        s                   like(esito)
002200010330     D rrnum           s              6  0 INZ(*zeros)
002300010330     D*------------------
002400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002500010330     D*------------------
002600010330     D WLBDA8          DS                  INZ
002700010330     D  G08DAT                 1      8  0
002800010330     D  G08INV                 9     16  0
002900010330     D  G08ERR                17     17
003000010330     D  G08TGI                18     22  0
003100010330
003200010330
003300000913     C                   reset                   rrnum
003400990921     C                   reset                   esito
003500990921     C                   reset                   wrkesito
003600010601     C*
003700010601     C                   exsr      opeini
003800010605     C                   exsr      rwvab
003900070502     C*
004000070502     C* Effettuo lancio TISI95 solo x chiusura
004100070502     C                   CLEAR                   TISI95DS
004200070502     C                   EVAL      I95TLA = 'C'
004300070502     C                   CALL      'TISI95R'
004400070502     C                   PARM                    TISI95DS
004500010601     C*
004600010601     C                   seton                                        lr
004700010601
004800010601
004900010601
005000010601
005100010601     C*--------------------------------------------------------
005200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005300010601     C*--------------------------------------------------------
005400010601     C     PREELA        BEGSR
005500010601     C*
005600010601     C* SE OCCORRE SPEDIRE IN FILIALE
005700010601     C                   if        invfil <> *zeros and
005800010601     C                             flgGiro = '0'
005900010601     C*
006000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006100010601     C                   eval      flgGiro = '1'
006200010601     C*
006300010601     C                   endif
006400010601     C*
006500010601     C                   ENDSR
006600010601     C***
006700010601
006800010601
006900010601
007000010601     C*--------------------------------------------------------
007100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007200010601     C*--------------------------------------------------------
007300010601     C     ENDELA        BEGSR
007400000616     C*
007500010601     C                   ENDSR
007600010601     C***
007700000613
007800010601
007900010601
008000010330     C*--------------------------------------------------------
008100050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
008200010330     C*--------------------------------------------------------
008300010605     C     RWVAB         BEGSR
008400050408     C*
008500010330     C                   if        not %open(tivin00r)
008600010330     C                   open      tivin00r
008700010330     C                   endif
008800050408     C                   if        not %open(fivabwwr)
008900050408     C                   open      fivabwwr
009000010330     C                   endif
009100010330     C*
009200010604     C                   clear                   §CTROK
009300010604     C                   clear                   §CTRMO
009400010604     C                   clear                   §CTRNO
009500010330     C*
009600010330     C                   DO        *HIVAL
009700010330     C*
009800010330     C                   READ      tivin00r                               70
009900010618     C*
010000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010100010618     C                   if        vinflg = '1'
010200010618     C                   eval      flgOk = '1'
010300010618     C                   endif
010400010618     C*
010500010330     C                   if        vindta > *blanks
010600010330     C                   add       1             rrnum
010700010330     C*
010800010601     C                   if        *in70 = *off and
010900010330     C                             (vinflg = *blanks
011000010330     C                              or vinflg = '0'
011100010330     C                              or vinflg = '2')
011200010330     C*
011300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011400010711     C                   if        vinflg = *blanks or vinflg = '0'
011500010711     C                   clear                   vinmsg
011600010711     C                   endif
011700070601     C*
011800070601     C                   exsr      inzvar
011900070601     C                   exsr      defcam
012000070601     C                   exsr      impvab
012100010601     C*
012200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012300010605     C                   eval      depfil = VABLNP
012400010601     C                   exsr      repfil
012500010601     C                   if        depfil = invfil
012600021025     C                   if        vlrpoi = 999
012700021025     C                   MOVE(P)   invfil        VABFGS
012800021025     C                   else
012900021025     C                   MOVE(P)   vlrpoi        VABFGS
013000021025     C                   endif
013100010601     C*
013200010601     C                   exsr      PREELA
013300010601     C*
013400010604     C* Ebbene...
013500010604     C*
013600010604     C  N31              ADD       1             §CTROK            7 0
013700010604     C   32              ADD       1             §CTRMO            7 0
013800010604     C   31              ADD       1             §CTRNO            7 0
013900071003     C                   exsr      wrivab                                       => scrivo VAB
014000010604     C*
014100020722     C                   endif
014200020722     C*
014300010604     C                   if        *in31 = *off and
014400010604     C                             *in32 = *off
014500010604     C                   eval      vinflg = '1'
014600010604     C                   else
014700010604     C                   eval      vinflg = '2'
014800010604     C                   endif
014900010604     C                   endif
015000010604     C*
015100010330     C                   else
015200010330     C                   eval      vinflg = '1'
015300010330     C                   endif
015400010601     C*
015500010601     C  N70              update    tivin000
015600010330     C*
015700010330     C  N70              ENDdo
015800010601     C*
015900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016000010601     C                   if        cntNonEl = *zeros or
016100010601     C                             flgMulti = '0'
016200010330     C* Se non ci sono record con errori ...
016300010601     C                   if        §ctrno = 0 and
016400010604     C                             §ctrmo = 0 and
016500010601     C                             flgStato <> '2'
016600010330     C* ... restituisco esito OK.
016700010330     C                   eval      wrkesito = '0'
016800010330     C                   else
016900010330     C                   if        §ctrok > 0
017000010330     C                   eval      wrkesito = '1'
017100010330     C                   else
017200010615     C                   if        flgOk = '0'
017300010615     C                   eval      wrkesito = '2'
017400010615     C                   else
017500010615     C                   eval      wrkesito = '6'
017600010615     C                   endif
017700010330     C                   endif
017800010330     C                   endif
017900010601     C                   else
018000010601     C                   eval      wrkesito = '9'
018100010601     C                   endif
018200010330     C*
018300010330     C                   if        %open(tivin00r)
018400010330     C                   close     tivin00r
018500010330     C                   endif
018600050408     C                   if        %open(fivabwwr)
018700050408     C                   close     fivabwwr
018800010330     C                   endif
018900010601     C*
019000010601     C                   if        vlrpoi <> 999
019100010601     C                   eval      invfil = vlrpoi
019200010601     C                   endif
019300010330     C*
019400010330     C                   if        §ctrok > 0
019500010601     C                             and invfil > *zeros
019600010330     C                   exsr      invio
019700010330     C                   endif
019800010601     C*
019900010618     C                   if        flgGiro = '1'
020000010601     C                   exsr      endela
020100010618     C                   endif
020200010330     C*
020300010330     C                   ENDSR
020400010330     C***
020500070601
020600070601     C*----------------------------------------------------*
020700070601     C*  SCARICAMENTO BUFFER RECORDS VAB
020800070601     C*----------------------------------------------------*
020900070601     C     WRIVAB        BEGSR
021000080321     C*
021100070601     C  N31              WRITE     FIVAB000
021200070601     C*
021300070601     C                   ENDSR
021400010601
021500010601
021600010601
021700010330     C*----------------------------------------------------*
021800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021900010330     C*----------------------------------------------------*
022000010330     C     INZVAR        BEGSR
022100010330     C*
022200020204     C                   Z-ADD     *zeros        Num5_0            5 0
022300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022400020322     C                   MOVEL     '0'           FlgCAS            1
022500010330     C*
022600010330     C                   ENDSR
022700010330     C*----------------------------------------------------*
022800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022900010330     C*----------------------------------------------------*
023000010330     C     DEFCAM        BEGSR
023100010330     C*
023200020204     C* Inizializzo il buffer del record da scrivere
023300050408     C                   CLEAR                   FIVAB000
023400020204     C* Imposto i valori di default...
023500140203     C                   EVAL      VABCCM = 0897599
023600140203     C                   EVAL      VABLNP = 089
023700140203     C                   EVAL      VABCTR = 003
023800070208     C                   EVAL      VABCBO = '1'
023900080410     C                   EVAL      VABCTM = '7Q'
024000020204     C* ... e poi verifico se sono stati passati come parametri
024100020204     C                   IF        vlrppt > *blanks
024200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
024300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024400020204     C                   EXSR      CHKNUM
024500020204     C                   IF        PiInt=*on
024600020204     C                   Z-ADD     PiVal         VABCCM
024700020204     C                   ENDIF
024800040301     C                   ENDIF
024900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
025000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025100020204     C                   EXSR      CHKNUM
025200020204     C                   IF        PiInt=*on
025300020204     C                   Z-ADD     PiVal         VABLNP
025400020204     C                   ENDIF
025500040301     C                   ENDIF
025600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
025700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025800020204     C                   EXSR      CHKNUM
025900020204     C                   IF        PiInt=*on
026000020204     C                   Z-ADD     PiVal         VABCTR
026100040301     C                   ENDIF
026200020204     C                   ENDIF
026300020204     C                   ENDIF
026400020204     C*
026500010330     C                   ENDSR
026600010607     C*----------------------------------------------------*
026700050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026800010607     C*----------------------------------------------------*
026900010607     C     IMPVAB        BEGSR
027000010607     C*
027100010607     C                   SETOFF                                       3132
027200010607     C*
027300010607     C* Reperimento campi ALFA
027400080410     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
027500080410     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
027600080410     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
027700080410     C                   EVAL      VABPRD=%trim(%subst(vindta:314:5))
027800080410     C                   EVAL      VABRMA=%trim(%subst(vindta:79:11))
027900140203     C                   EVAL      VABNOT=%trim(%subst(vindta:491:40))
028000140203     C                   EVAL      VABRMO=%trim(%subst(vindta:114:30))
028100140203     C                   EVAL      VABCMO=%trim(%subst(vindta:209:8))
028200140203     C***                EVAL      VABNZD=%trim(%subst(vindta:327:4))
028300080408     C*
028400080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
028500080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
028600080408     C     '@':'A'       XLATE     VABRSD        VABRSD
028700080408     C* ==
028800080321     C*
028900080410     C* GC1
029000080410     C                   IF        %subst(vindta:478:3) = 'LUN'
029100080410     C                   EVAL      VABGC1 = '1'
029200080321     C                   ENDIF
029300080410     C                   IF        %subst(vindta:478:3) = 'MAR'
029400080410     C                   EVAL      VABGC1 = '2'
029500080410     C                   ENDIF
029600080410     C                   IF        %subst(vindta:478:3) = 'MER'
029700080410     C                   EVAL      VABGC1 = '3'
029800080410     C                   ENDIF
029900080410     C                   IF        %subst(vindta:478:3) = 'GIO'
030000080410     C                   EVAL      VABGC1 = '4'
030100080410     C                   ENDIF
030200080410     C                   IF        %subst(vindta:478:3) = 'VEN'
030300080410     C                   EVAL      VABGC1 = '5'
030400080410     C                   ENDIF
030500080410     C                   IF        %subst(vindta:478:3) = 'SAB'
030600080410     C                   EVAL      VABGC1 = '6'
030700080410     C                   ENDIF
030800080410     C                   IF        %subst(vindta:478:3) = 'DOM'
030900080410     C                   EVAL      VABGC1 = '7'
031000080410     C                   ENDIF
031100080410     C* GC1
031200080410     C                   IF        %subst(vindta:481:1) = '1'
031300080410     C                   EVAL      %subst(VABGC1:2:1) = 'M'
031400080410     C                   ENDIF
031500080410     C                   IF        %subst(vindta:481:1) = '2'
031600080410     C                   EVAL      %subst(VABGC1:2:1) = 'P'
031700080410     C                   ENDIF
031800080410     C* CBO
031900140203     C                   IF        %subst(vindta:2:1) = '0' or
032000140203     C                             %subst(vindta:2:1) = '1' or
032100140203     C                             %subst(vindta:2:1) = '2' or
032200140203     C                             %subst(vindta:2:1) = '3' or
032300140203     C                             %subst(vindta:2:1) = '4'
032400080410     C                   EVAL      VABCBO = '1'
032500080410     C                   ENDIF
032600140203     C                   IF        %subst(vindta:2:1) = '5' or
032700140203     C                             %subst(vindta:2:1) = '6' or
032800140203     C                             %subst(vindta:2:1) = '7'
032900080410     C                   EVAL      VABCBO = '2'
033000080410     C                   ENDIF
033100060510     C*
033200060510     C* Reperimento campi NUMERICI
033300060510     C                   MOVEL     DATCOR        VABAAS
033400060510     C                   MOVE      DATCOR        VABMGS
033500080410     C* NSP
033600080410     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
033700060510     C                   EXSR      CHKNUM
033800060510     C                   IF        PiInt=*on
033900060510     C                   Z-ADD     PiVal         VABNSP
034000060404     C                   ELSE
034100070502     C                   SETON                                        31
034200060510     C                   Z-ADD     *zeros        VABNSP
034300060404     C                   EVAL      vinmsg = %trimr(vinmsg)
034400080410     C                             + ' ' + 'VABNSP'
034500060404     C                   ENDIF
034600080410     C* RMN
034700080410     C                   EVAL      PiStr=%trim(%subst(vindta:79:11))
034800080410     C                   EXSR      CHKNUM
034900080410     C                   IF        PiInt=*on
035000080410     C                   Z-ADD     PiVal         VABRMN
035100080410     C                   ELSE
035200080410     C                   SETON                                        31
035300080410     C                   Z-ADD     1             VABRMN
035400080410     C                   EVAL      vinmsg = %trimr(vinmsg)
035500080410     C                             + ' ' + 'VABRMN'
035600080410     C                   ENDIF
035700080408     C* CAD
035800080410     C                   EVAL      PiStr=%trim(%subst(vindta:319:8))
035900080408     C                   EXSR      CHKNUM
036000080408     C                   IF        PiInt=*on
036100080408     C                   Z-ADD     PiVal         Num5_0
036200080408     C                   MOVEL(p)  Num5_0        VABCAD
036300080408     C                   ELSE
036400080408     C                   SETON                                        32
036500080408     C                   EVAL      VABCAD = *zeros
036600080408     C                   EVAL      vinmsg = %trimr(vinmsg)
036700080408     C                             + ' ' + 'VABCAD'
036800080408     C                   ENDIF
036900080408     C* Reperisco la provincia dal CAP e dalla località
037000080408     C                   IF        VABCAD <> *blanks AND
037100080408     C                             VABPRD  = *blanks
037200080408     C                   CLEAR                   TISI95DS
037300080408     C                   EVAL      I95TCN = '3'
037400080408     C                   Z-ADD     datcor        I95DAT
037500080408     C                   EVAL      I95CAP = VABCAD
037600080408     C                   EVAL      I95LOC = VABLOD
037700080408     C                   EVAL      I95NAR = VABNZD
037800080408     C                   CALL      'TISI95R'
037900080408     C                   PARM                    TISI95DS
038000080408     C                   EVAL      VABPRD = O95PRV
038100080408     C                   ENDIF
038200080410     C* DCR/TC1
038300080410     C                   IF        %subst(vindta:490:1) = '1'
038400080410     C* DCR
038500080410     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
038600080320     C                   EXSR      CHKNUM
038700080320     C                   IF        PiInt=*on
038800080320     C                   Z-ADD     PiVal         VABDCR
038900080320     C                   ELSE
039000080320     C                   SETON                                        32
039100080320     C                   Z-ADD     *zeros        VABDCR
039200080320     C                   EVAL      vinmsg = %trimr(vinmsg)
039300080320     C                             + ' ' + 'VABDCR'
039400080320     C                   ENDIF
039500080410     C                   ELSE
039600080410     C* TC1
039700080410     C                   IF        %subst(vindta:490:1) = '2'
039800080410     C                   EVAL      VABTC1 = 'A'
039900080410     C                   ENDIF
040000080409     C                   ENDIF
040100080410     C* NCL
040200080410     C                   IF        %subst(vindta:352:5)<>'00000'
040300080410     C                   EVAL      PiStr=%trim(%subst(vindta:352:5))
040400080410     C                   EXSR      CHKNUM
040500080410     C                   IF        PiInt=*on
040600080410     C                   Z-ADD     PiVal         VABNCL
040700080410     C                   ELSE
040800080410     C                   SETON                                        32
040900080410     C                   Z-ADD     *zeros        VABNCL
041000080410     C                   EVAL      vinmsg = %trimr(vinmsg)
041100080410     C                             + ' ' + 'VABNCL'
041200080410     C                   ENDIF
041300080410     C                   ELSE
041400080410     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
041500080410     C                   EXSR      CHKNUM
041600080410     C                   IF        PiInt=*on
041700080410     C                   Z-ADD     PiVal         VABNCL
041800080410     C                   ELSE
041900080410     C                   SETON                                        32
042000080410     C                   Z-ADD     *zeros        VABNCL
042100080410     C                   EVAL      vinmsg = %trimr(vinmsg)
042200080410     C                             + ' ' + 'VABNCL'
042300080410     C                   ENDIF
042400080410     C                   ENDIF
042500071029     C* PKB
042600080410     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
042700010607     C                   EXSR      CHKNUM
042800010607     C                   IF        PiNum=*on
042900080408     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
043000071003     C                   Z-ADD(H)  PiVal         VABPKB
043100010607     C                   ELSE
043200010607     C                   SETON                                        32
043300010607     C                   Z-ADD     *zeros        VABPKB
043400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043500010607     C                             + ' ' + 'VABPKB'
043600010607     C                   ENDIF
043700080320     C* CAS
043800080410     C                   IF        %subst(vindta:388:13)<>'0000000000000'
043900080408     C                   EVAL      FlgCAS = '1'
044000080408     C                   EVAL      VABVCA = 'EUR'
044100080410     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
044200080320     C                   EXSR      CHKNUM
044300080320     C                   IF        PiNum=*on
044400080410     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
044500080410     C                   Z-ADD(H)  PiVal         VABCAS
044600080320     C                   ELSE
044700080320     C                   SETON                                        32
044800080320     C                   Z-ADD     *zeros        VABCAS
044900080320     C                   EVAL      vinmsg = %trimr(vinmsg)
045000080320     C                             + ' ' + 'VABCAS'
045100080408     C                   ENDIF
045200080320     C                   ENDIF
045300080410     C* IAS
045400080410     C                   IF        %subst(vindta:558:13)<>'0000000000000'
045500080410     C                   EVAL      VABVAS = 'EUR'
045600080410     C                   EVAL      PiStr=%trim(%subst(vindta:558:13))
045700080410     C                   EXSR      CHKNUM
045800080410     C                   IF        PiNum=*on
045900080410     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
046000080410     C                   Z-ADD(H)  PiVal         VABIAS
046100080410     C                   ELSE
046200080410     C                   SETON                                        32
046300080410     C                   Z-ADD     *zeros        VABIAS
046400080410     C                   EVAL      vinmsg = %trimr(vinmsg)
046500080410     C                             + ' ' + 'VABIAS'
046600080410     C                   ENDIF
046700080410     C                   ENDIF
046800020322     C*
046900020322     C* Considerazioni finali su CBO/CAS
047000050908     C                   IF        FlgCAS = '1'
047100040713     C                   IF        VABCBO = '1'
047200020322     C                   EVAL      VABCBO = '4'
047300040713     C                   ENDIF
047400040713     C                   IF        VABCBO = '2'
047500040713     C                   EVAL      VABCBO = '6'
047600040713     C                   ENDIF
047700020322     C                   ENDIF
047800020204     C*
047900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048000020204     C                   EXSR      CHKIMPDIV
048100020204     C*
048200010607     C                   ENDSR
048300010607     C*----------------------------------------------------*
048400010601
048500020204
048600020204     C*----------------------------------------------------*
048700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048800020204     C*----------------------------------------------------*
048900020204     C     CHKIMPDIV     BEGSR
049000020204     C*
049100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049200020204     C                   Z-ADD     *zeros        wrkDec            9 9
049300020204     C*
049400020204     C* Come prima cosa effettuo considerazioni sulla divisa
049500020204     C                   IF        vabIAS > *zeros
049600020204     C                   IF        vabVAS <> 'EUR'
049700020204     C                   EVAL      vabVAS =  'ITL'
049800020204     C                   ENDIF
049900020204     C                   ENDIF
050000020204     C*
050100020204     C                   IF        vabCAS > *zeros
050200020204     C                   IF        vabVCA <> 'EUR'
050300020204     C                   EVAL      vabVCA =  'ITL'
050400020204     C                   ENDIF
050500020204     C                   ENDIF
050600020204     C*
050700020204     C                   IF        vabVMD > *zeros
050800020321     C                   IF        vabVAD <> 'EUR'
050900020204     C                   EVAL      vabVAD =  'ITL'
051000020204     C                   ENDIF
051100020204     C                   ENDIF
051200020204     C*
051300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051400020204     C                   Z-ADD     vabIAS        wrkDec
051500020204     C                   IF        wrkDec > *zeros
051600020204     C                   IF        vabVAS = 'ITL'
051700020204     C                   EVAL      vabIAS = *zeros
051800020204     C                   ENDIF
051900020204     C                   ENDIF
052000020204     C*
052100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052200020204     C                   Z-ADD     vabCAS        wrkDec
052300020204     C                   IF        wrkDec > *zeros
052400020204     C                   IF        vabVCA = 'ITL'
052500020204     C                   EVAL      vabCAS = *zeros
052600020204     C                   ENDIF
052700020204     C                   ENDIF
052800020204     C*
052900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053000020204     C                   Z-ADD     vabVMD        wrkDec
053100020204     C                   IF        wrkDec > *zeros
053200020204     C                   IF        vabVAD = 'ITL'
053300020204     C                   EVAL      vabVMD = *zeros
053400020204     C                   ENDIF
053500020204     C                   ENDIF
053600020204     C*
053700020204     C                   ENDSR
053800020204     C***
053900020204
054000010330
054100010330
054200010330     C*----------------------------------------------------*
054300010330     C*  CONTROLLO NUMERICITA' CAMPI
054400010330     C*----------------------------------------------------*
054500010330     C     CHKNUM        BEGSR
054600010330     C*
054700010606     C                   IF        PiDecChr = *blanks
054800080409     C                   EVAL      PiDecChr = '.'
054900010606     C                   ENDIF
055000010606     C*
055100010606     C                   CALL(e)   'ISNUMERIC'
055200010330     C                   PARM                    PiStr            30
055300010606     C                   PARM                    PiDecChr          1
055400010330     C                   PARM      *ZEROS        PiVal            30 9
055500010330     C                   PARM      '0'           PiInt             1
055600010330     C                   PARM      '0'           PiNum             1
055700010330     C                   IF        %error
055800010606     C                   EVAL      PiNum=*off
055900010330     C                   ENDIF
056000010330     C*
056100010330     C                   ENDSR
056200010330     C***
056300010330
056400010601
056500010601
056600010601
056700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
056800010601     C     repfil        BEGSR
056900010601     C*
057000010601     C                   if        invfil = *zeros and
057100010601     C                             depfil > *zeros and
057200010629     C                             (vinflg = *blanks or
057300010629     C                              vinflg = *zeros)
057400010601     C
057500010601     C                   eval      invfil = depfil
057600010601     C                   endif
057700010601     C*
057800010601     C                   if        depfil <> invfil and
057900010601     C                             invfil > *zeros
058000010601     C                   eval      flgMulti = '1'
058100010601     C                   if        vinflg = *blanks
058200010601     C                   add       1             cntNonEl
058300010601     C                   endif
058400010601     C                   endif
058500010601     C*
058600010601     C                   if        vinflg = '2'
058700010601     C                   eval      flgStato = '2'
058800010601     C                   endif
058900010601     C*
059000010601     C                   ENDSR
059100010601     C***
059200010601
059300010601
059400010601
059500010330
059600010330
059700010330
059800990920      /TITLE Invio dei dati al punto operativo.
059900000613     C     invio         BEGSR
060000990920     C*
060100990920     C                   reset                   dscmz
060200010601     C                   move      invfil        cmzdst
060300990920     C                   eval      cmzfld = vlrfou
060400990920     C                   eval      cmzmbd = vlrhdl
060500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060600000710     C                   if        prmfir = *blanks
060700050408     C                   eval      cmzfla = 'FIVAB00F'
060800050408     C                   eval      cmzmba = 'FIVAB00F'
060900000710     C                   else
061000000710     C                   eval      cmzfla = prmfir
061100000710     C                   eval      cmzmba = prmfir
061200000710     C                   endif
061300990920     C                   eval      cmznrr = *zeros
061400990920     C                   move      §ctrok        cmznrr
061500021018     C                   eval      cmzlba = vlrfl1
061600990920     C                   call(e)   'TIS711C'
061700990920     C                   parm                    dscmz
061800990921     C                   parm      *blanks       esito
061900990920     C                   if        %error
062000990920     C                             or cmzerr = '1'
062100990921     C                             or esito  = '1'
062200000710     C                   eval      wrkesito = '3'
062300990920     C                   endif
062400990920     C*
062500000613     C                   ENDSR
062600990910
062700010601
062800010601
062900010601
063000010601
063100010601      /TITLE Invio dei dati al punto operativo.
063200010601     C     opeini        BEGSR
063300010601     C*
063400010601     C* Inizializzo flag e contatori operativi
063500010601     C                   movel     '0'           flgGiro           1
063600010601     C                   movel     '0'           flgMulti          1
063700010601     C                   movel     '1'           flgStato          1
063800010615     C                   movel     '0'           flgOk             1
063900010601     C                   z-add     *zeros        cntNonEl         10 0
064000010601     C                   z-add     *zeros        depfil            3 0
064100010601     C                   z-add     *zeros        invfil            3 0
064200010601     C*
064300010601     C                   ENDSR
064400010601     C***
064500010601
064600010601
064700010601
064800010330
064900010330
065000000613     C     *inzsr        BEGSR
065100990910     C*
065200990910     C     *entry        plist
065300990920     C                   parm                    tivlrds
065400990921     C                   parm      wrkesito      esito
065500000724     C                   parm                    prmlit
065600000710     C                   parm                    prmfir
065700010330     C*
065800010330     C* CALCOLA LA DATA CORRENTE
065900010330     C                   time                    wn14             14 0
066000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066100010330     C                   z-add     wn8           g08dat
066200010330     C                   z-add     *zeros        g08inv
066300010330     C                   movel     '0'           g08err
066400010330     C                   call      'XSRDA8'
066500010330     C                   parm                    wlbda8
066600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066700000613     C*
066800000613     C                   ENDSR
066900000613     C***
