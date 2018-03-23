000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200121029      *
000300121029      * PARTICOLARITA':
000400121029      * 12/10/29 a differenza dei traduttori di copia (TITV1F3R - TITV1F4R) la data consegna
000500121029      * deve essere sempre presa e non solo se il tipo consegna è = '1' ma rimane la gestione TC1
000600121029      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800000313     F*
000900120604     Ftabel00f  if   e           k disk
001000990910     Ftivin00r  uF   E             DISK    usropn
001100050408     FFIVABWWR  O    E             DISK    usropn
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900120604     D ds15          e ds
002000070502     D tisi95ds      e ds
002100010330     D esito           s              1
002200010330     D prmlit          s             10
002300010330     D prmfir          s             10
002400010330     D wrkesito        s                   like(esito)
002500010330     D rrnum           s              6  0 INZ(*zeros)
002600120604     D jNAZ            s              5  0 INZ(*zeros)
002700010330     D*------------------
002800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002900010330     D*------------------
003000010330     D WLBDA8          DS                  INZ
003100010330     D  G08DAT                 1      8  0
003200010330     D  G08INV                 9     16  0
003300010330     D  G08ERR                17     17
003400010330     D  G08TGI                18     22  0
003500120604
003600120604     D*------------
003700120604     D* SCHIERE
003800120604     D*------------
003900120604     D skNAZISO        S              3    DIM(1000)
004000120604     D skNAZBAR        S              3    DIM(1000)
004100010330
004200010330
004300000913     C                   reset                   rrnum
004400990921     C                   reset                   esito
004500990921     C                   reset                   wrkesito
004600010601     C*
004700120604     C                   EXSR      CARTAB                                       CARICA TABELLE
004800010601     C                   exsr      opeini
004900010605     C                   exsr      rwvab
005000070502     C*
005100070502     C* Effettuo lancio TISI95 solo x chiusura
005200070502     C                   CLEAR                   TISI95DS
005300070502     C                   EVAL      I95TLA = 'C'
005400070502     C                   CALL      'TISI95R'
005500070502     C                   PARM                    TISI95DS
005600010601     C*
005700010601     C                   seton                                        lr
005800010601
005900010601
006000010601
006100010601
006200010601     C*--------------------------------------------------------
006300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006400010601     C*--------------------------------------------------------
006500010601     C     PREELA        BEGSR
006600010601     C*
006700010601     C* SE OCCORRE SPEDIRE IN FILIALE
006800010601     C                   if        invfil <> *zeros and
006900010601     C                             flgGiro = '0'
007000010601     C*
007100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007200010601     C                   eval      flgGiro = '1'
007300010601     C*
007400010601     C                   endif
007500010601     C*
007600010601     C                   ENDSR
007700010601     C***
007800010601
007900010601
008000010601
008100010601     C*--------------------------------------------------------
008200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008300010601     C*--------------------------------------------------------
008400010601     C     ENDELA        BEGSR
008500000616     C*
008600010601     C                   ENDSR
008700010601     C***
008800120525
008900010330     C*--------------------------------------------------------
009000050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
009100010330     C*--------------------------------------------------------
009200010605     C     RWVAB         BEGSR
009300050408     C*
009400010330     C                   if        not %open(tivin00r)
009500010330     C                   open      tivin00r
009600010330     C                   endif
009700050408     C                   if        not %open(fivabwwr)
009800050408     C                   open      fivabwwr
009900010330     C                   endif
010000010330     C*
010100010604     C                   clear                   §CTROK
010200010604     C                   clear                   §CTRMO
010300010604     C                   clear                   §CTRNO
010400010330     C*
010500010330     C                   DO        *HIVAL
010600010330     C*
010700010330     C                   READ      tivin00r                               70
010800010618     C*
010900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011000010618     C                   if        vinflg = '1'
011100010618     C                   eval      flgOk = '1'
011200010618     C                   endif
011300010618     C*
011400010330     C                   if        vindta > *blanks
011500010330     C                   add       1             rrnum
011600010330     C*
011700010601     C                   if        *in70 = *off and
011800010330     C                             (vinflg = *blanks
011900010330     C                              or vinflg = '0'
012000010330     C                              or vinflg = '2')
012100010330     C*
012200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012300010711     C                   if        vinflg = *blanks or vinflg = '0'
012400010711     C                   clear                   vinmsg
012500010711     C                   endif
012600070601     C*
012700070601     C                   exsr      inzvar
012800070601     C                   exsr      defcam
012900070601     C                   exsr      impvab
013000010601     C*
013100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013200010605     C                   eval      depfil = VABLNP
013300010601     C                   exsr      repfil
013400010601     C                   if        depfil = invfil
013500021025     C                   if        vlrpoi = 999
013600021025     C                   MOVE(P)   invfil        VABFGS
013700021025     C                   else
013800021025     C                   MOVE(P)   vlrpoi        VABFGS
013900021025     C                   endif
014000010601     C*
014100010601     C                   exsr      PREELA
014200010601     C*
014300010604     C* Ebbene...
014400010604     C*
014500010604     C  N31              ADD       1             §CTROK            7 0
014600010604     C   32              ADD       1             §CTRMO            7 0
014700010604     C   31              ADD       1             §CTRNO            7 0
014800071003     C                   exsr      wrivab                                       => scrivo VAB
014900010604     C*
015000020722     C                   endif
015100020722     C*
015200010604     C                   if        *in31 = *off and
015300010604     C                             *in32 = *off
015400010604     C                   eval      vinflg = '1'
015500010604     C                   else
015600010604     C                   eval      vinflg = '2'
015700010604     C                   endif
015800010604     C                   endif
015900010604     C*
016000010330     C                   else
016100010330     C                   eval      vinflg = '1'
016200010330     C                   endif
016300010601     C*
016400010601     C  N70              update    tivin000
016500010330     C*
016600010330     C  N70              ENDdo
016700010601     C*
016800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016900010601     C                   if        cntNonEl = *zeros or
017000010601     C                             flgMulti = '0'
017100010330     C* Se non ci sono record con errori ...
017200010601     C                   if        §ctrno = 0 and
017300010604     C                             §ctrmo = 0 and
017400010601     C                             flgStato <> '2'
017500010330     C* ... restituisco esito OK.
017600010330     C                   eval      wrkesito = '0'
017700010330     C                   else
017800010330     C                   if        §ctrok > 0
017900010330     C                   eval      wrkesito = '1'
018000010330     C                   else
018100010615     C                   if        flgOk = '0'
018200010615     C                   eval      wrkesito = '2'
018300010615     C                   else
018400010615     C                   eval      wrkesito = '6'
018500010615     C                   endif
018600010330     C                   endif
018700010330     C                   endif
018800010601     C                   else
018900010601     C                   eval      wrkesito = '9'
019000010601     C                   endif
019100010330     C*
019200010330     C                   if        %open(tivin00r)
019300010330     C                   close     tivin00r
019400010330     C                   endif
019500050408     C                   if        %open(fivabwwr)
019600050408     C                   close     fivabwwr
019700010330     C                   endif
019800010601     C*
019900010601     C                   if        vlrpoi <> 999
020000010601     C                   eval      invfil = vlrpoi
020100010601     C                   endif
020200010330     C*
020300010330     C                   if        §ctrok > 0
020400010601     C                             and invfil > *zeros
020500010330     C                   exsr      invio
020600010330     C                   endif
020700010601     C*
020800010618     C                   if        flgGiro = '1'
020900010601     C                   exsr      endela
021000010618     C                   endif
021100010330     C*
021200010330     C                   ENDSR
021300010330     C***
021400070601
021500070601     C*----------------------------------------------------*
021600070601     C*  SCARICAMENTO BUFFER RECORDS VAB
021700070601     C*----------------------------------------------------*
021800070601     C     WRIVAB        BEGSR
021900080321     C*
022000070601     C  N31              WRITE     FIVAB000
022100070601     C*
022200070601     C                   ENDSR
022300010601
022400010601
022500010601
022600010330     C*----------------------------------------------------*
022700020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022800010330     C*----------------------------------------------------*
022900010330     C     INZVAR        BEGSR
023000010330     C*
023100020204     C                   Z-ADD     *zeros        Num5_0            5 0
023200020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
023300020322     C                   MOVEL     '0'           FlgCAS            1
023400010330     C*
023500010330     C                   ENDSR
023600010330     C*----------------------------------------------------*
023700020722     C*  IMPOSTAZIONE CAMPI COSTANTI
023800010330     C*----------------------------------------------------*
023900010330     C     DEFCAM        BEGSR
024000010330     C*
024100020204     C* Inizializzo il buffer del record da scrivere
024200050408     C                   CLEAR                   FIVAB000
024300020204     C* Imposto i valori di default...
024400120525     C                   EVAL      VABCCM = 0934683
024500120525     C                   EVAL      VABLNP = 093
024600120525     C                   EVAL      VABCTR = 001
024700070208     C                   EVAL      VABCBO = '1'
024800080410     C                   EVAL      VABCTM = '7Q'
024900020204     C* ... e poi verifico se sono stati passati come parametri
025000020204     C                   IF        vlrppt > *blanks
025100040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
025200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025300020204     C                   EXSR      CHKNUM
025400020204     C                   IF        PiInt=*on
025500020204     C                   Z-ADD     PiVal         VABCCM
025600020204     C                   ENDIF
025700040301     C                   ENDIF
025800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
025900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026000020204     C                   EXSR      CHKNUM
026100020204     C                   IF        PiInt=*on
026200020204     C                   Z-ADD     PiVal         VABLNP
026300020204     C                   ENDIF
026400040301     C                   ENDIF
026500040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
026600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026700020204     C                   EXSR      CHKNUM
026800020204     C                   IF        PiInt=*on
026900020204     C                   Z-ADD     PiVal         VABCTR
027000040301     C                   ENDIF
027100020204     C                   ENDIF
027200020204     C                   ENDIF
027300020204     C*
027400010330     C                   ENDSR
027500010607     C*----------------------------------------------------*
027600050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
027700010607     C*----------------------------------------------------*
027800010607     C     IMPVAB        BEGSR
027900010607     C*
028000010607     C                   SETOFF                                       3132
028100010607     C*
028200010607     C* Reperimento campi ALFA
028300080410     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
028400080410     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
028500080410     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
028600121106     C                   EVAL      VABCAD=%trim(%subst(vindta:319:8))
028700120604     C* per la nazione consideriamo il dato come la nazione ISO e poi la
028800120604     C* sostituiamo con la nostra
028900120604     C                   EVAL      VABNZD=%trim(%subst(vindta:314:5))
029000120604     C                   Z-ADD     1             jNAZ
029100120604     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
029200120604     C                   IF        %found
029300120604     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
029400120604     C                   ENDIF
029500120525     C                   EVAL      VABRMA=%trim(%subst(vindta:11:20))
029600080410     C                   EVAL      VABNOT=%trim(%subst(vindta:491:35))
029700080408     C*
029800080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
029900080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030000080408     C     '@':'A'       XLATE     VABRSD        VABRSD
030100080408     C* ==
030200080321     C*
030300080410     C* GC1
030400080410     C                   IF        %subst(vindta:478:3) = 'LUN'
030500080410     C                   EVAL      VABGC1 = '1'
030600080321     C                   ENDIF
030700080410     C                   IF        %subst(vindta:478:3) = 'MAR'
030800080410     C                   EVAL      VABGC1 = '2'
030900080410     C                   ENDIF
031000080410     C                   IF        %subst(vindta:478:3) = 'MER'
031100080410     C                   EVAL      VABGC1 = '3'
031200080410     C                   ENDIF
031300080410     C                   IF        %subst(vindta:478:3) = 'GIO'
031400080410     C                   EVAL      VABGC1 = '4'
031500080410     C                   ENDIF
031600080410     C                   IF        %subst(vindta:478:3) = 'VEN'
031700080410     C                   EVAL      VABGC1 = '5'
031800080410     C                   ENDIF
031900080410     C                   IF        %subst(vindta:478:3) = 'SAB'
032000080410     C                   EVAL      VABGC1 = '6'
032100080410     C                   ENDIF
032200080410     C                   IF        %subst(vindta:478:3) = 'DOM'
032300080410     C                   EVAL      VABGC1 = '7'
032400080410     C                   ENDIF
032500080410     C* GC1
032600080410     C                   IF        %subst(vindta:481:1) = '1'
032700080410     C                   EVAL      %subst(VABGC1:2:1) = 'M'
032800080410     C                   ENDIF
032900080410     C                   IF        %subst(vindta:481:1) = '2'
033000080410     C                   EVAL      %subst(VABGC1:2:1) = 'P'
033100080410     C                   ENDIF
033200080410     C* CBO
033300080410     C                   IF        %subst(vindta:2:1) = '1'
033400080410     C                   EVAL      VABCBO = '1'
033500080410     C                   ENDIF
033600080410     C                   IF        %subst(vindta:2:1) = '6'
033700080410     C                   EVAL      VABCBO = '2'
033800080410     C                   ENDIF
033900060510     C*
034000060510     C* Reperimento campi NUMERICI
034100060510     C                   MOVEL     DATCOR        VABAAS
034200060510     C                   MOVE      DATCOR        VABMGS
034300080410     C* NSP
034400080410     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
034500060510     C                   EXSR      CHKNUM
034600060510     C                   IF        PiInt=*on
034700060510     C                   Z-ADD     PiVal         VABNSP
034800060404     C                   ELSE
034900070502     C                   SETON                                        31
035000060510     C                   Z-ADD     *zeros        VABNSP
035100060404     C                   EVAL      vinmsg = %trimr(vinmsg)
035200080410     C                             + ' ' + 'VABNSP'
035300060404     C                   ENDIF
035400080410     C* RMN
035500080410     C                   EVAL      PiStr=%trim(%subst(vindta:79:11))
035600080410     C                   EXSR      CHKNUM
035700080410     C                   IF        PiInt=*on
035800080410     C                   Z-ADD     PiVal         VABRMN
035900080410     C                   ELSE
036000080410     C                   SETON                                        31
036100080410     C                   Z-ADD     1             VABRMN
036200080410     C                   EVAL      vinmsg = %trimr(vinmsg)
036300080410     C                             + ' ' + 'VABRMN'
036400080410     C                   ENDIF
036500080408     C* CAD
036600121106     C***                EVAL      PiStr=%trim(%subst(vindta:319:8))
036700121106     C***                EXSR      CHKNUM
036800121106     C***                IF        PiInt=*on
036900121106     C***                Z-ADD     PiVal         Num5_0
037000121106     C***                MOVEL(p)  Num5_0        VABCAD
037100121106     C***                ELSE
037200121106     C***                SETON                                        32
037300121106     C***                EVAL      VABCAD = *zeros
037400121106     C***                EVAL      vinmsg = %trimr(vinmsg)
037500121106     C***                          + ' ' + 'VABCAD'
037600121106     C***                ENDIF
037700080408     C* Reperisco la provincia dal CAP e dalla località
037800080408     C                   IF        VABCAD <> *blanks AND
037900121106     C                             VABNZD  = *blanks AND
038000080408     C                             VABPRD  = *blanks
038100080408     C                   CLEAR                   TISI95DS
038200080408     C                   EVAL      I95TCN = '3'
038300080408     C                   Z-ADD     datcor        I95DAT
038400080408     C                   EVAL      I95CAP = VABCAD
038500080408     C                   EVAL      I95LOC = VABLOD
038600080408     C                   EVAL      I95NAR = VABNZD
038700080408     C                   CALL      'TISI95R'
038800080408     C                   PARM                    TISI95DS
038900080408     C                   EVAL      VABPRD = O95PRV
039000080408     C                   ENDIF
039100121029     C* DCR/TC1
039200121029     C***                IF        %subst(vindta:490:1) = '1'
039300080410     C* DCR
039400080410     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
039500080320     C                   EXSR      CHKNUM
039600080320     C                   IF        PiInt=*on
039700080320     C                   Z-ADD     PiVal         VABDCR
039800080320     C                   ELSE
039900080320     C                   SETON                                        32
040000080320     C                   Z-ADD     *zeros        VABDCR
040100080320     C                   EVAL      vinmsg = %trimr(vinmsg)
040200080320     C                             + ' ' + 'VABDCR'
040300080320     C                   ENDIF
040400121029     C***                ELSE
040500080410     C* TC1
040600121029     C                   IF        %subst(vindta:490:1) = '2'
040700121029     C                   EVAL      VABTC1 = 'A'
040800121029     C                   ENDIF
040900121029     C***                ENDIF
041000080410     C* NCL
041100080410     C                   IF        %subst(vindta:352:5)<>'00000'
041200080410     C                   EVAL      PiStr=%trim(%subst(vindta:352:5))
041300080410     C                   EXSR      CHKNUM
041400080410     C                   IF        PiInt=*on
041500080410     C                   Z-ADD     PiVal         VABNCL
041600080410     C                   ELSE
041700080410     C                   SETON                                        32
041800080410     C                   Z-ADD     *zeros        VABNCL
041900080410     C                   EVAL      vinmsg = %trimr(vinmsg)
042000080410     C                             + ' ' + 'VABNCL'
042100080410     C                   ENDIF
042200080410     C                   ELSE
042300080410     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
042400080410     C                   EXSR      CHKNUM
042500080410     C                   IF        PiInt=*on
042600080410     C                   Z-ADD     PiVal         VABNCL
042700080410     C                   ELSE
042800080410     C                   SETON                                        32
042900080410     C                   Z-ADD     *zeros        VABNCL
043000080410     C                   EVAL      vinmsg = %trimr(vinmsg)
043100080410     C                             + ' ' + 'VABNCL'
043200080410     C                   ENDIF
043300080410     C                   ENDIF
043400071029     C* PKB
043500080410     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
043600010607     C                   EXSR      CHKNUM
043700010607     C                   IF        PiNum=*on
043800080408     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
043900071003     C                   Z-ADD(H)  PiVal         VABPKB
044000010607     C                   ELSE
044100010607     C                   SETON                                        32
044200010607     C                   Z-ADD     *zeros        VABPKB
044300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044400010607     C                             + ' ' + 'VABPKB'
044500010607     C                   ENDIF
044600080320     C* CAS
044700080410     C                   IF        %subst(vindta:388:13)<>'0000000000000'
044800080408     C                   EVAL      FlgCAS = '1'
044900080408     C                   EVAL      VABVCA = 'EUR'
045000080410     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
045100080320     C                   EXSR      CHKNUM
045200080320     C                   IF        PiNum=*on
045300080410     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
045400080410     C                   Z-ADD(H)  PiVal         VABCAS
045500080320     C                   ELSE
045600080320     C                   SETON                                        32
045700080320     C                   Z-ADD     *zeros        VABCAS
045800080320     C                   EVAL      vinmsg = %trimr(vinmsg)
045900080320     C                             + ' ' + 'VABCAS'
046000080408     C                   ENDIF
046100080320     C                   ENDIF
046200080410     C* IAS
046300080410     C                   IF        %subst(vindta:558:13)<>'0000000000000'
046400080410     C                   EVAL      VABVAS = 'EUR'
046500080410     C                   EVAL      PiStr=%trim(%subst(vindta:558:13))
046600080410     C                   EXSR      CHKNUM
046700080410     C                   IF        PiNum=*on
046800080410     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
046900080410     C                   Z-ADD(H)  PiVal         VABIAS
047000080410     C                   ELSE
047100080410     C                   SETON                                        32
047200080410     C                   Z-ADD     *zeros        VABIAS
047300080410     C                   EVAL      vinmsg = %trimr(vinmsg)
047400080410     C                             + ' ' + 'VABIAS'
047500080410     C                   ENDIF
047600080410     C                   ENDIF
047700020322     C*
047800020322     C* Considerazioni finali su CBO/CAS
047900050908     C                   IF        FlgCAS = '1'
048000040713     C                   IF        VABCBO = '1'
048100020322     C                   EVAL      VABCBO = '4'
048200040713     C                   ENDIF
048300040713     C                   IF        VABCBO = '2'
048400040713     C                   EVAL      VABCBO = '6'
048500040713     C                   ENDIF
048600020322     C                   ENDIF
048700020204     C*
048800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048900020204     C                   EXSR      CHKIMPDIV
049000020204     C*
049100010607     C                   ENDSR
049200010607     C*----------------------------------------------------*
049300010601
049400020204
049500020204     C*----------------------------------------------------*
049600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049700020204     C*----------------------------------------------------*
049800020204     C     CHKIMPDIV     BEGSR
049900020204     C*
050000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050100020204     C                   Z-ADD     *zeros        wrkDec            9 9
050200020204     C*
050300020204     C* Come prima cosa effettuo considerazioni sulla divisa
050400020204     C                   IF        vabIAS > *zeros
050500020204     C                   IF        vabVAS <> 'EUR'
050600020204     C                   EVAL      vabVAS =  'ITL'
050700020204     C                   ENDIF
050800020204     C                   ENDIF
050900020204     C*
051000020204     C                   IF        vabCAS > *zeros
051100020204     C                   IF        vabVCA <> 'EUR'
051200020204     C                   EVAL      vabVCA =  'ITL'
051300020204     C                   ENDIF
051400020204     C                   ENDIF
051500020204     C*
051600020204     C                   IF        vabVMD > *zeros
051700020321     C                   IF        vabVAD <> 'EUR'
051800020204     C                   EVAL      vabVAD =  'ITL'
051900020204     C                   ENDIF
052000020204     C                   ENDIF
052100020204     C*
052200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052300020204     C                   Z-ADD     vabIAS        wrkDec
052400020204     C                   IF        wrkDec > *zeros
052500020204     C                   IF        vabVAS = 'ITL'
052600020204     C                   EVAL      vabIAS = *zeros
052700020204     C                   ENDIF
052800020204     C                   ENDIF
052900020204     C*
053000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
053100020204     C                   Z-ADD     vabCAS        wrkDec
053200020204     C                   IF        wrkDec > *zeros
053300020204     C                   IF        vabVCA = 'ITL'
053400020204     C                   EVAL      vabCAS = *zeros
053500020204     C                   ENDIF
053600020204     C                   ENDIF
053700020204     C*
053800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053900020204     C                   Z-ADD     vabVMD        wrkDec
054000020204     C                   IF        wrkDec > *zeros
054100020204     C                   IF        vabVAD = 'ITL'
054200020204     C                   EVAL      vabVMD = *zeros
054300020204     C                   ENDIF
054400020204     C                   ENDIF
054500020204     C*
054600020204     C                   ENDSR
054700020204     C***
054800020204
054900010330
055000010330
055100010330     C*----------------------------------------------------*
055200010330     C*  CONTROLLO NUMERICITA' CAMPI
055300010330     C*----------------------------------------------------*
055400010330     C     CHKNUM        BEGSR
055500010330     C*
055600010606     C                   IF        PiDecChr = *blanks
055700080409     C                   EVAL      PiDecChr = '.'
055800010606     C                   ENDIF
055900010606     C*
056000010606     C                   CALL(e)   'ISNUMERIC'
056100010330     C                   PARM                    PiStr            30
056200010606     C                   PARM                    PiDecChr          1
056300010330     C                   PARM      *ZEROS        PiVal            30 9
056400010330     C                   PARM      '0'           PiInt             1
056500010330     C                   PARM      '0'           PiNum             1
056600010330     C                   IF        %error
056700010606     C                   EVAL      PiNum=*off
056800010330     C                   ENDIF
056900010330     C*
057000010330     C                   ENDSR
057100010330     C***
057200010330
057300010601
057400010601
057500010601
057600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
057700010601     C     repfil        BEGSR
057800010601     C*
057900010601     C                   if        invfil = *zeros and
058000010601     C                             depfil > *zeros and
058100010629     C                             (vinflg = *blanks or
058200010629     C                              vinflg = *zeros)
058300010601     C
058400010601     C                   eval      invfil = depfil
058500010601     C                   endif
058600010601     C*
058700010601     C                   if        depfil <> invfil and
058800010601     C                             invfil > *zeros
058900010601     C                   eval      flgMulti = '1'
059000010601     C                   if        vinflg = *blanks
059100010601     C                   add       1             cntNonEl
059200010601     C                   endif
059300010601     C                   endif
059400010601     C*
059500010601     C                   if        vinflg = '2'
059600010601     C                   eval      flgStato = '2'
059700010601     C                   endif
059800010601     C*
059900010601     C                   ENDSR
060000010601     C***
060100010601
060200010601
060300010601
060400010330
060500010330
060600010330
060700990920      /TITLE Invio dei dati al punto operativo.
060800000613     C     invio         BEGSR
060900990920     C*
061000990920     C                   reset                   dscmz
061100010601     C                   move      invfil        cmzdst
061200990920     C                   eval      cmzfld = vlrfou
061300990920     C                   eval      cmzmbd = vlrhdl
061400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
061500000710     C                   if        prmfir = *blanks
061600050408     C                   eval      cmzfla = 'FIVAB00F'
061700050408     C                   eval      cmzmba = 'FIVAB00F'
061800000710     C                   else
061900000710     C                   eval      cmzfla = prmfir
062000000710     C                   eval      cmzmba = prmfir
062100000710     C                   endif
062200990920     C                   eval      cmznrr = *zeros
062300990920     C                   move      §ctrok        cmznrr
062400021018     C                   eval      cmzlba = vlrfl1
062500990920     C                   call(e)   'TIS711C'
062600990920     C                   parm                    dscmz
062700990921     C                   parm      *blanks       esito
062800990920     C                   if        %error
062900990920     C                             or cmzerr = '1'
063000990921     C                             or esito  = '1'
063100000710     C                   eval      wrkesito = '3'
063200990920     C                   endif
063300990920     C*
063400000613     C                   ENDSR
063500990910
063600010601
063700120604
063800120604     C*--------------------------------------------------------
063900120604     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
064000120604     C*--------------------------------------------------------
064100120604     C     CARTAB        BEGSR
064200120604     C*
064300120604     C* TABELLA '15' - NAZIONI
064400120604     C                   clear                   skNAZISO
064500120604     C                   clear                   skNAZBAR
064600120604     C                   eval      tblKUT = 1
064700120604     C                   eval      tblCOD = '15'
064800120604     C     KEYtabP       setll     tabel00f
064900120604     C     KEYtabP       reade     tabel00f
065000120604     C                   dow       not %eof(tabel00f)
065100120604     C                   if        tblFLG = *blanks
065200120604     C                   movel(p)  tblUNI        ds15
065300120604     C                   add       1             jNAZ
065400120604     C                   eval      skNAZBAR(jNAZ) = tblKEY
065500120604     C                   eval      skNAZISO(jNAZ) = §15COD
065600120604     C                   endif
065700120604     C     KEYtabP       reade     tabel00f
065800120604     C                   enddo
065900120604     C*
066000120604     C                   ENDSR
066100120604     C***
066200010601
066300010601
066400010601
066500010601      /TITLE Invio dei dati al punto operativo.
066600010601     C     opeini        BEGSR
066700010601     C*
066800010601     C* Inizializzo flag e contatori operativi
066900010601     C                   movel     '0'           flgGiro           1
067000010601     C                   movel     '0'           flgMulti          1
067100010601     C                   movel     '1'           flgStato          1
067200010615     C                   movel     '0'           flgOk             1
067300010601     C                   z-add     *zeros        cntNonEl         10 0
067400010601     C                   z-add     *zeros        depfil            3 0
067500010601     C                   z-add     *zeros        invfil            3 0
067600010601     C*
067700010601     C                   ENDSR
067800010601     C***
067900010601
068000010601
068100010601
068200010330
068300010330
068400000613     C     *inzsr        BEGSR
068500990910     C*
068600990910     C     *entry        plist
068700990920     C                   parm                    tivlrds
068800990921     C                   parm      wrkesito      esito
068900000724     C                   parm                    prmlit
069000000710     C                   parm                    prmfir
069100010330     C*
069200010330     C* CALCOLA LA DATA CORRENTE
069300010330     C                   time                    wn14             14 0
069400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069500010330     C                   z-add     wn8           g08dat
069600010330     C                   z-add     *zeros        g08inv
069700010330     C                   movel     '0'           g08err
069800010330     C                   call      'XSRDA8'
069900010330     C                   parm                    wlbda8
070000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
070100120604     C*
070200120604     C* Chiave su TABEL00F - parziale
070300120604     C     KEYtabP       KLIST
070400120604     C                   KFLD                    tblKUT
070500120604     C                   KFLD                    tblCOD
070600000613     C*
070700000613     C                   ENDSR
070800000613     C***
