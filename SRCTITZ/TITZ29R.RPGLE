000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200121029      *
000300121029      * PARTICOLARITA':
000400121029      * 12/10/29 a differenza dei traduttori di copia (TITV1F3R - TITV1F4R) la data consegna
000500121029      * deve essere sempre presa e non solo se il tipo consegna è = '1' ma rimane la gestione TC1
000600121029      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800000313     F*
000900990910     Ftivin00r  uF   E             DISK    usropn
001000050408     FFIVABWWR  O    E             DISK    usropn
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800070502     D tisi95ds      e ds
001900010330     D esito           s              1
002000010330     D prmlit          s             10
002100010330     D prmfir          s             10
002200010330     D wrkesito        s                   like(esito)
002300010330     D rrnum           s              6  0 INZ(*zeros)
002400010330     D*------------------
002500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002600010330     D*------------------
002700010330     D WLBDA8          DS                  INZ
002800010330     D  G08DAT                 1      8  0
002900010330     D  G08INV                 9     16  0
003000010330     D  G08ERR                17     17
003100010330     D  G08TGI                18     22  0
003200010330
003300010330
003400000913     C                   reset                   rrnum
003500990921     C                   reset                   esito
003600990921     C                   reset                   wrkesito
003700010601     C*
003800010601     C                   exsr      opeini
003900010605     C                   exsr      rwvab
004000070502     C*
004100070502     C* Effettuo lancio TISI95 solo x chiusura
004200070502     C                   CLEAR                   TISI95DS
004300070502     C                   EVAL      I95TLA = 'C'
004400070502     C                   CALL      'TISI95R'
004500070502     C                   PARM                    TISI95DS
004600010601     C*
004700010601     C                   seton                                        lr
004800010601
004900010601
005000010601
005100010601
005200010601     C*--------------------------------------------------------
005300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005400010601     C*--------------------------------------------------------
005500010601     C     PREELA        BEGSR
005600010601     C*
005700010601     C* SE OCCORRE SPEDIRE IN FILIALE
005800010601     C                   if        invfil <> *zeros and
005900010601     C                             flgGiro = '0'
006000010601     C*
006100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006200010601     C                   eval      flgGiro = '1'
006300010601     C*
006400010601     C                   endif
006500010601     C*
006600010601     C                   ENDSR
006700010601     C***
006800010601
006900010601
007000010601
007100010601     C*--------------------------------------------------------
007200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007300010601     C*--------------------------------------------------------
007400010601     C     ENDELA        BEGSR
007500000616     C*
007600010601     C                   ENDSR
007700010601     C***
007800120525
007900010330     C*--------------------------------------------------------
008000050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
008100010330     C*--------------------------------------------------------
008200010605     C     RWVAB         BEGSR
008300050408     C*
008400010330     C                   if        not %open(tivin00r)
008500010330     C                   open      tivin00r
008600010330     C                   endif
008700050408     C                   if        not %open(fivabwwr)
008800050408     C                   open      fivabwwr
008900010330     C                   endif
009000010330     C*
009100010604     C                   clear                   §CTROK
009200010604     C                   clear                   §CTRMO
009300010604     C                   clear                   §CTRNO
009400010330     C*
009500010330     C                   DO        *HIVAL
009600010330     C*
009700010330     C                   READ      tivin00r                               70
009800010618     C*
009900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010000010618     C                   if        vinflg = '1'
010100010618     C                   eval      flgOk = '1'
010200010618     C                   endif
010300010618     C*
010400010330     C                   if        vindta > *blanks
010500010330     C                   add       1             rrnum
010600010330     C*
010700010601     C                   if        *in70 = *off and
010800010330     C                             (vinflg = *blanks
010900010330     C                              or vinflg = '0'
011000010330     C                              or vinflg = '2')
011100010330     C*
011200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011300010711     C                   if        vinflg = *blanks or vinflg = '0'
011400010711     C                   clear                   vinmsg
011500010711     C                   endif
011600070601     C*
011700070601     C                   exsr      inzvar
011800070601     C                   exsr      defcam
011900070601     C                   exsr      impvab
012000010601     C*
012100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012200010605     C                   eval      depfil = VABLNP
012300010601     C                   exsr      repfil
012400010601     C                   if        depfil = invfil
012500021025     C                   if        vlrpoi = 999
012600021025     C                   MOVE(P)   invfil        VABFGS
012700021025     C                   else
012800021025     C                   MOVE(P)   vlrpoi        VABFGS
012900021025     C                   endif
013000010601     C*
013100010601     C                   exsr      PREELA
013200010601     C*
013300010604     C* Ebbene...
013400010604     C*
013500010604     C  N31              ADD       1             §CTROK            7 0
013600010604     C   32              ADD       1             §CTRMO            7 0
013700010604     C   31              ADD       1             §CTRNO            7 0
013800071003     C                   exsr      wrivab                                       => scrivo VAB
013900010604     C*
014000020722     C                   endif
014100020722     C*
014200010604     C                   if        *in31 = *off and
014300010604     C                             *in32 = *off
014400010604     C                   eval      vinflg = '1'
014500010604     C                   else
014600010604     C                   eval      vinflg = '2'
014700010604     C                   endif
014800010604     C                   endif
014900010604     C*
015000010330     C                   else
015100010330     C                   eval      vinflg = '1'
015200010330     C                   endif
015300010601     C*
015400010601     C  N70              update    tivin000
015500010330     C*
015600010330     C  N70              ENDdo
015700010601     C*
015800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015900010601     C                   if        cntNonEl = *zeros or
016000010601     C                             flgMulti = '0'
016100010330     C* Se non ci sono record con errori ...
016200010601     C                   if        §ctrno = 0 and
016300010604     C                             §ctrmo = 0 and
016400010601     C                             flgStato <> '2'
016500010330     C* ... restituisco esito OK.
016600010330     C                   eval      wrkesito = '0'
016700010330     C                   else
016800010330     C                   if        §ctrok > 0
016900010330     C                   eval      wrkesito = '1'
017000010330     C                   else
017100010615     C                   if        flgOk = '0'
017200010615     C                   eval      wrkesito = '2'
017300010615     C                   else
017400010615     C                   eval      wrkesito = '6'
017500010615     C                   endif
017600010330     C                   endif
017700010330     C                   endif
017800010601     C                   else
017900010601     C                   eval      wrkesito = '9'
018000010601     C                   endif
018100010330     C*
018200010330     C                   if        %open(tivin00r)
018300010330     C                   close     tivin00r
018400010330     C                   endif
018500050408     C                   if        %open(fivabwwr)
018600050408     C                   close     fivabwwr
018700010330     C                   endif
018800010601     C*
018900010601     C                   if        vlrpoi <> 999
019000010601     C                   eval      invfil = vlrpoi
019100010601     C                   endif
019200010330     C*
019300010330     C                   if        §ctrok > 0
019400010601     C                             and invfil > *zeros
019500010330     C                   exsr      invio
019600010330     C                   endif
019700010601     C*
019800010618     C                   if        flgGiro = '1'
019900010601     C                   exsr      endela
020000010618     C                   endif
020100010330     C*
020200010330     C                   ENDSR
020300010330     C***
020400070601
020500070601     C*----------------------------------------------------*
020600070601     C*  SCARICAMENTO BUFFER RECORDS VAB
020700070601     C*----------------------------------------------------*
020800070601     C     WRIVAB        BEGSR
020900080321     C*
021000070601     C  N31              WRITE     FIVAB000
021100070601     C*
021200070601     C                   ENDSR
021300010601
021400010601
021500010601
021600010330     C*----------------------------------------------------*
021700020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021800010330     C*----------------------------------------------------*
021900010330     C     INZVAR        BEGSR
022000010330     C*
022100020204     C                   Z-ADD     *zeros        Num5_0            5 0
022200020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022300020322     C                   MOVEL     '0'           FlgCAS            1
022400010330     C*
022500010330     C                   ENDSR
022600010330     C*----------------------------------------------------*
022700020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022800010330     C*----------------------------------------------------*
022900010330     C     DEFCAM        BEGSR
023000010330     C*
023100020204     C* Inizializzo il buffer del record da scrivere
023200050408     C                   CLEAR                   FIVAB000
023300020204     C* Imposto i valori di default...
023400120525     C                   EVAL      VABCCM = 0934685
023500120525     C                   EVAL      VABLNP = 093
023600120525     C                   EVAL      VABCTR = 000
023700070208     C                   EVAL      VABCBO = '1'
023800080410     C                   EVAL      VABCTM = '7Q'
023900020204     C* ... e poi verifico se sono stati passati come parametri
024000020204     C                   IF        vlrppt > *blanks
024100040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
024200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024300020204     C                   EXSR      CHKNUM
024400020204     C                   IF        PiInt=*on
024500020204     C                   Z-ADD     PiVal         VABCCM
024600020204     C                   ENDIF
024700040301     C                   ENDIF
024800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025000020204     C                   EXSR      CHKNUM
025100020204     C                   IF        PiInt=*on
025200020204     C                   Z-ADD     PiVal         VABLNP
025300020204     C                   ENDIF
025400040301     C                   ENDIF
025500040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
025600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025700020204     C                   EXSR      CHKNUM
025800020204     C                   IF        PiInt=*on
025900020204     C                   Z-ADD     PiVal         VABCTR
026000040301     C                   ENDIF
026100020204     C                   ENDIF
026200020204     C                   ENDIF
026300020204     C*
026400010330     C                   ENDSR
026500010607     C*----------------------------------------------------*
026600050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026700010607     C*----------------------------------------------------*
026800010607     C     IMPVAB        BEGSR
026900010607     C*
027000010607     C                   SETOFF                                       3132
027100010607     C*
027200010607     C* Reperimento campi ALFA
027300080410     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
027400080410     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
027500080410     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
027600121106     C                   EVAL      VABCAD=%trim(%subst(vindta:319:8))
027700080410     C                   EVAL      VABPRD=%trim(%subst(vindta:314:5))
027800120525     C                   EVAL      VABRMA=%trim(%subst(vindta:11:20))
027900080410     C                   EVAL      VABNOT=%trim(%subst(vindta:491:35))
028000080408     C*
028100080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
028200080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
028300080408     C     '@':'A'       XLATE     VABRSD        VABRSD
028400080408     C* ==
028500080321     C*
028600080410     C* GC1
028700080410     C                   IF        %subst(vindta:478:3) = 'LUN'
028800080410     C                   EVAL      VABGC1 = '1'
028900080321     C                   ENDIF
029000080410     C                   IF        %subst(vindta:478:3) = 'MAR'
029100080410     C                   EVAL      VABGC1 = '2'
029200080410     C                   ENDIF
029300080410     C                   IF        %subst(vindta:478:3) = 'MER'
029400080410     C                   EVAL      VABGC1 = '3'
029500080410     C                   ENDIF
029600080410     C                   IF        %subst(vindta:478:3) = 'GIO'
029700080410     C                   EVAL      VABGC1 = '4'
029800080410     C                   ENDIF
029900080410     C                   IF        %subst(vindta:478:3) = 'VEN'
030000080410     C                   EVAL      VABGC1 = '5'
030100080410     C                   ENDIF
030200080410     C                   IF        %subst(vindta:478:3) = 'SAB'
030300080410     C                   EVAL      VABGC1 = '6'
030400080410     C                   ENDIF
030500080410     C                   IF        %subst(vindta:478:3) = 'DOM'
030600080410     C                   EVAL      VABGC1 = '7'
030700080410     C                   ENDIF
030800080410     C* GC1
030900080410     C                   IF        %subst(vindta:481:1) = '1'
031000080410     C                   EVAL      %subst(VABGC1:2:1) = 'M'
031100080410     C                   ENDIF
031200080410     C                   IF        %subst(vindta:481:1) = '2'
031300080410     C                   EVAL      %subst(VABGC1:2:1) = 'P'
031400080410     C                   ENDIF
031500080410     C* CBO
031600080410     C                   IF        %subst(vindta:2:1) = '1'
031700080410     C                   EVAL      VABCBO = '1'
031800080410     C                   ENDIF
031900080410     C                   IF        %subst(vindta:2:1) = '6'
032000080410     C                   EVAL      VABCBO = '2'
032100080410     C                   ENDIF
032200060510     C*
032300060510     C* Reperimento campi NUMERICI
032400060510     C                   MOVEL     DATCOR        VABAAS
032500060510     C                   MOVE      DATCOR        VABMGS
032600080410     C* NSP
032700080410     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
032800060510     C                   EXSR      CHKNUM
032900060510     C                   IF        PiInt=*on
033000060510     C                   Z-ADD     PiVal         VABNSP
033100060404     C                   ELSE
033200070502     C                   SETON                                        31
033300060510     C                   Z-ADD     *zeros        VABNSP
033400060404     C                   EVAL      vinmsg = %trimr(vinmsg)
033500080410     C                             + ' ' + 'VABNSP'
033600060404     C                   ENDIF
033700080410     C* RMN
033800080410     C                   EVAL      PiStr=%trim(%subst(vindta:79:11))
033900080410     C                   EXSR      CHKNUM
034000080410     C                   IF        PiInt=*on
034100080410     C                   Z-ADD     PiVal         VABRMN
034200080410     C                   ELSE
034300080410     C                   SETON                                        31
034400080410     C                   Z-ADD     1             VABRMN
034500080410     C                   EVAL      vinmsg = %trimr(vinmsg)
034600080410     C                             + ' ' + 'VABRMN'
034700080410     C                   ENDIF
034800080408     C* CAD
034900121106     C***                EVAL      PiStr=%trim(%subst(vindta:319:8))
035000121106     C***                EXSR      CHKNUM
035100121106     C***                IF        PiInt=*on
035200121106     C***                Z-ADD     PiVal         Num5_0
035300121106     C***                MOVEL(p)  Num5_0        VABCAD
035400121106     C***                ELSE
035500121106     C***                SETON                                        32
035600121106     C***                EVAL      VABCAD = *zeros
035700121106     C***                EVAL      vinmsg = %trimr(vinmsg)
035800121106     C***                          + ' ' + 'VABCAD'
035900121106     C***                ENDIF
036000080408     C* Reperisco la provincia dal CAP e dalla località
036100080408     C                   IF        VABCAD <> *blanks AND
036200121106     C                             VABNZD  = *blanks AND
036300080408     C                             VABPRD  = *blanks
036400080408     C                   CLEAR                   TISI95DS
036500080408     C                   EVAL      I95TCN = '3'
036600080408     C                   Z-ADD     datcor        I95DAT
036700080408     C                   EVAL      I95CAP = VABCAD
036800080408     C                   EVAL      I95LOC = VABLOD
036900080408     C                   EVAL      I95NAR = VABNZD
037000080408     C                   CALL      'TISI95R'
037100080408     C                   PARM                    TISI95DS
037200080408     C                   EVAL      VABPRD = O95PRV
037300080408     C                   ENDIF
037400080410     C* DCR/TC1
037500121029     C***                IF        %subst(vindta:490:1) = '1'
037600080410     C* DCR
037700080410     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
037800080320     C                   EXSR      CHKNUM
037900080320     C                   IF        PiInt=*on
038000080320     C                   Z-ADD     PiVal         VABDCR
038100080320     C                   ELSE
038200080320     C                   SETON                                        32
038300080320     C                   Z-ADD     *zeros        VABDCR
038400080320     C                   EVAL      vinmsg = %trimr(vinmsg)
038500080320     C                             + ' ' + 'VABDCR'
038600080320     C                   ENDIF
038700121029     C***                ELSE
038800080410     C* TC1
038900080410     C                   IF        %subst(vindta:490:1) = '2'
039000080410     C                   EVAL      VABTC1 = 'A'
039100080410     C                   ENDIF
039200121029     C***                ENDIF
039300080410     C* NCL
039400080410     C                   IF        %subst(vindta:352:5)<>'00000'
039500080410     C                   EVAL      PiStr=%trim(%subst(vindta:352:5))
039600080410     C                   EXSR      CHKNUM
039700080410     C                   IF        PiInt=*on
039800080410     C                   Z-ADD     PiVal         VABNCL
039900080410     C                   ELSE
040000080410     C                   SETON                                        32
040100080410     C                   Z-ADD     *zeros        VABNCL
040200080410     C                   EVAL      vinmsg = %trimr(vinmsg)
040300080410     C                             + ' ' + 'VABNCL'
040400080410     C                   ENDIF
040500080410     C                   ELSE
040600080410     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
040700080410     C                   EXSR      CHKNUM
040800080410     C                   IF        PiInt=*on
040900080410     C                   Z-ADD     PiVal         VABNCL
041000080410     C                   ELSE
041100080410     C                   SETON                                        32
041200080410     C                   Z-ADD     *zeros        VABNCL
041300080410     C                   EVAL      vinmsg = %trimr(vinmsg)
041400080410     C                             + ' ' + 'VABNCL'
041500080410     C                   ENDIF
041600080410     C                   ENDIF
041700071029     C* PKB
041800080410     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
041900010607     C                   EXSR      CHKNUM
042000010607     C                   IF        PiNum=*on
042100080408     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
042200071003     C                   Z-ADD(H)  PiVal         VABPKB
042300010607     C                   ELSE
042400010607     C                   SETON                                        32
042500010607     C                   Z-ADD     *zeros        VABPKB
042600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042700010607     C                             + ' ' + 'VABPKB'
042800010607     C                   ENDIF
042900080320     C* CAS
043000080410     C                   IF        %subst(vindta:388:13)<>'0000000000000'
043100080408     C                   EVAL      FlgCAS = '1'
043200080408     C                   EVAL      VABVCA = 'EUR'
043300080410     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
043400080320     C                   EXSR      CHKNUM
043500080320     C                   IF        PiNum=*on
043600080410     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
043700080410     C                   Z-ADD(H)  PiVal         VABCAS
043800080320     C                   ELSE
043900080320     C                   SETON                                        32
044000080320     C                   Z-ADD     *zeros        VABCAS
044100080320     C                   EVAL      vinmsg = %trimr(vinmsg)
044200080320     C                             + ' ' + 'VABCAS'
044300080408     C                   ENDIF
044400080320     C                   ENDIF
044500080410     C* IAS
044600080410     C                   IF        %subst(vindta:558:13)<>'0000000000000'
044700080410     C                   EVAL      VABVAS = 'EUR'
044800080410     C                   EVAL      PiStr=%trim(%subst(vindta:558:13))
044900080410     C                   EXSR      CHKNUM
045000080410     C                   IF        PiNum=*on
045100080410     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
045200080410     C                   Z-ADD(H)  PiVal         VABIAS
045300080410     C                   ELSE
045400080410     C                   SETON                                        32
045500080410     C                   Z-ADD     *zeros        VABIAS
045600080410     C                   EVAL      vinmsg = %trimr(vinmsg)
045700080410     C                             + ' ' + 'VABIAS'
045800080410     C                   ENDIF
045900080410     C                   ENDIF
046000020322     C*
046100020322     C* Considerazioni finali su CBO/CAS
046200050908     C                   IF        FlgCAS = '1'
046300040713     C                   IF        VABCBO = '1'
046400020322     C                   EVAL      VABCBO = '4'
046500040713     C                   ENDIF
046600040713     C                   IF        VABCBO = '2'
046700040713     C                   EVAL      VABCBO = '6'
046800040713     C                   ENDIF
046900020322     C                   ENDIF
047000020204     C*
047100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047200020204     C                   EXSR      CHKIMPDIV
047300020204     C*
047400010607     C                   ENDSR
047500010607     C*----------------------------------------------------*
047600010601
047700020204
047800020204     C*----------------------------------------------------*
047900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048000020204     C*----------------------------------------------------*
048100020204     C     CHKIMPDIV     BEGSR
048200020204     C*
048300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048400020204     C                   Z-ADD     *zeros        wrkDec            9 9
048500020204     C*
048600020204     C* Come prima cosa effettuo considerazioni sulla divisa
048700020204     C                   IF        vabIAS > *zeros
048800020204     C                   IF        vabVAS <> 'EUR'
048900020204     C                   EVAL      vabVAS =  'ITL'
049000020204     C                   ENDIF
049100020204     C                   ENDIF
049200020204     C*
049300020204     C                   IF        vabCAS > *zeros
049400020204     C                   IF        vabVCA <> 'EUR'
049500020204     C                   EVAL      vabVCA =  'ITL'
049600020204     C                   ENDIF
049700020204     C                   ENDIF
049800020204     C*
049900020204     C                   IF        vabVMD > *zeros
050000020321     C                   IF        vabVAD <> 'EUR'
050100020204     C                   EVAL      vabVAD =  'ITL'
050200020204     C                   ENDIF
050300020204     C                   ENDIF
050400020204     C*
050500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050600020204     C                   Z-ADD     vabIAS        wrkDec
050700020204     C                   IF        wrkDec > *zeros
050800020204     C                   IF        vabVAS = 'ITL'
050900020204     C                   EVAL      vabIAS = *zeros
051000020204     C                   ENDIF
051100020204     C                   ENDIF
051200020204     C*
051300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
051400020204     C                   Z-ADD     vabCAS        wrkDec
051500020204     C                   IF        wrkDec > *zeros
051600020204     C                   IF        vabVCA = 'ITL'
051700020204     C                   EVAL      vabCAS = *zeros
051800020204     C                   ENDIF
051900020204     C                   ENDIF
052000020204     C*
052100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052200020204     C                   Z-ADD     vabVMD        wrkDec
052300020204     C                   IF        wrkDec > *zeros
052400020204     C                   IF        vabVAD = 'ITL'
052500020204     C                   EVAL      vabVMD = *zeros
052600020204     C                   ENDIF
052700020204     C                   ENDIF
052800020204     C*
052900020204     C                   ENDSR
053000020204     C***
053100020204
053200010330
053300010330
053400010330     C*----------------------------------------------------*
053500010330     C*  CONTROLLO NUMERICITA' CAMPI
053600010330     C*----------------------------------------------------*
053700010330     C     CHKNUM        BEGSR
053800010330     C*
053900010606     C                   IF        PiDecChr = *blanks
054000080409     C                   EVAL      PiDecChr = '.'
054100010606     C                   ENDIF
054200010606     C*
054300010606     C                   CALL(e)   'ISNUMERIC'
054400010330     C                   PARM                    PiStr            30
054500010606     C                   PARM                    PiDecChr          1
054600010330     C                   PARM      *ZEROS        PiVal            30 9
054700010330     C                   PARM      '0'           PiInt             1
054800010330     C                   PARM      '0'           PiNum             1
054900010330     C                   IF        %error
055000010606     C                   EVAL      PiNum=*off
055100010330     C                   ENDIF
055200010330     C*
055300010330     C                   ENDSR
055400010330     C***
055500010330
055600010601
055700010601
055800010601
055900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
056000010601     C     repfil        BEGSR
056100010601     C*
056200010601     C                   if        invfil = *zeros and
056300010601     C                             depfil > *zeros and
056400010629     C                             (vinflg = *blanks or
056500010629     C                              vinflg = *zeros)
056600010601     C
056700010601     C                   eval      invfil = depfil
056800010601     C                   endif
056900010601     C*
057000010601     C                   if        depfil <> invfil and
057100010601     C                             invfil > *zeros
057200010601     C                   eval      flgMulti = '1'
057300010601     C                   if        vinflg = *blanks
057400010601     C                   add       1             cntNonEl
057500010601     C                   endif
057600010601     C                   endif
057700010601     C*
057800010601     C                   if        vinflg = '2'
057900010601     C                   eval      flgStato = '2'
058000010601     C                   endif
058100010601     C*
058200010601     C                   ENDSR
058300010601     C***
058400010601
058500010601
058600010601
058700010330
058800010330
058900010330
059000990920      /TITLE Invio dei dati al punto operativo.
059100000613     C     invio         BEGSR
059200990920     C*
059300990920     C                   reset                   dscmz
059400010601     C                   move      invfil        cmzdst
059500990920     C                   eval      cmzfld = vlrfou
059600990920     C                   eval      cmzmbd = vlrhdl
059700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
059800000710     C                   if        prmfir = *blanks
059900050408     C                   eval      cmzfla = 'FIVAB00F'
060000050408     C                   eval      cmzmba = 'FIVAB00F'
060100000710     C                   else
060200000710     C                   eval      cmzfla = prmfir
060300000710     C                   eval      cmzmba = prmfir
060400000710     C                   endif
060500990920     C                   eval      cmznrr = *zeros
060600990920     C                   move      §ctrok        cmznrr
060700021018     C                   eval      cmzlba = vlrfl1
060800990920     C                   call(e)   'TIS711C'
060900990920     C                   parm                    dscmz
061000990921     C                   parm      *blanks       esito
061100990920     C                   if        %error
061200990920     C                             or cmzerr = '1'
061300990921     C                             or esito  = '1'
061400000710     C                   eval      wrkesito = '3'
061500990920     C                   endif
061600990920     C*
061700000613     C                   ENDSR
061800990910
061900010601
062000010601
062100010601
062200010601
062300010601      /TITLE Invio dei dati al punto operativo.
062400010601     C     opeini        BEGSR
062500010601     C*
062600010601     C* Inizializzo flag e contatori operativi
062700010601     C                   movel     '0'           flgGiro           1
062800010601     C                   movel     '0'           flgMulti          1
062900010601     C                   movel     '1'           flgStato          1
063000010615     C                   movel     '0'           flgOk             1
063100010601     C                   z-add     *zeros        cntNonEl         10 0
063200010601     C                   z-add     *zeros        depfil            3 0
063300010601     C                   z-add     *zeros        invfil            3 0
063400010601     C*
063500010601     C                   ENDSR
063600010601     C***
063700010601
063800010601
063900010601
064000010330
064100010330
064200000613     C     *inzsr        BEGSR
064300990910     C*
064400990910     C     *entry        plist
064500990920     C                   parm                    tivlrds
064600990921     C                   parm      wrkesito      esito
064700000724     C                   parm                    prmlit
064800000710     C                   parm                    prmfir
064900010330     C*
065000010330     C* CALCOLA LA DATA CORRENTE
065100010330     C                   time                    wn14             14 0
065200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
065300010330     C                   z-add     wn8           g08dat
065400010330     C                   z-add     *zeros        g08inv
065500010330     C                   movel     '0'           g08err
065600010330     C                   call      'XSRDA8'
065700010330     C                   parm                    wlbda8
065800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065900000613     C*
066000000613     C                   ENDSR
066100000613     C***
