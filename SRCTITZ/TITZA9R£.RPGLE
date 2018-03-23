000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700080410     Ftitv1f3p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900080410     Ftitv1f3ps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300070502     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300010330     D*------------------
003400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003500010330     D*------------------
003600010330     D WLBDA8          DS                  INZ
003700010330     D  G08DAT                 1      8  0
003800010330     D  G08INV                 9     16  0
003900010330     D  G08ERR                17     17
004000010330     D  G08TGI                18     22  0
004100010330
004200010330
004300990915     C                   time                    wrkdata
004400990915     C                   time                    wrkora
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010601     C*
004900010601     C                   exsr      opeini
005000010605     C                   exsr      rwvab
005100070502     C*
005200070502     C* Effettuo lancio TISI95 solo x chiusura
005300070502     C                   CLEAR                   TISI95DS
005400070502     C                   EVAL      I95TLA = 'C'
005500070502     C                   CALL      'TISI95R'
005600070502     C                   PARM                    TISI95DS
005700010601     C*
005800010601     C                   seton                                        lr
005900010601
006000010601
006100010601
006200010601
006300010601     C*--------------------------------------------------------
006400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006500010601     C*--------------------------------------------------------
006600010601     C     PREELA        BEGSR
006700010601     C*
006800010601     C* SE OCCORRE SPEDIRE IN FILIALE
006900010601     C                   if        invfil <> *zeros and
007000010601     C                             flgGiro = '0'
007100010601     C*
007200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007300010601     C                   eval      flgGiro = '1'
007400000724     C*
007500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700010601     C     invfil        chain     azorg01l
007800000724     C                   if        %found
007900000724     C                   movel(p)  CMD(1)        depcmd
008000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008100000724     C*
008200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000724     C                   PARM                    depcmd
008600000724     C                   PARM                    LENGH
008700000724     C*
008800000724     C                   endif
008900000616     C*
009000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100000616     C                   movel(p)  CMD(2)        depcmd
009200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009500011113     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900000613     C*
010000080410     C                   if        not %open(titv1f3ps)
010100080410     C                   open      titv1f3ps
010200000616     C                   except    testdett
010300000613     C                   endif
010400010601     C*
010500010601     C                   endif
010600010601     C*
010700010601     C                   ENDSR
010800010601     C***
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011400010601     C*--------------------------------------------------------
011500010601     C     ENDELA        BEGSR
011600990915     C*
011700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011800000613     C*
011900080410     C                   if        %open(titv1f3ps)
012000000616     C                   except    findett
012100080410     C                   close     titv1f3ps
012200000613     C                   endif
012300000616     C*
012400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    CMD(3)
012800000616     C                   PARM                    LENGH
012900000616     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400000613
013500000613     C*--------------------------------------------------------
013600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013700000613     C*--------------------------------------------------------
013800000621     C     STPR          BEGSR
013900000613     C*
014000080410     C                   if        not %open(titv1f3p)
014100080410     C                   open      titv1f3p
014200990915     C                   endif
014300990915     C*
014400990915     C                   except    riepilogo
014500990915     C*
014600080410     C                   if        %open(titv1f3p)
014700080410     C                   close     titv1f3p
014800990914     C                   endif
014900000613     C*
015000000613     C                   ENDSR
015100000613     C***
015200000613
015300010601
015400010601
015500010330     C*--------------------------------------------------------
015600050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015700010330     C*--------------------------------------------------------
015800010605     C     RWVAB         BEGSR
015900050408     C*
016000010330     C                   if        not %open(tivin00r)
016100010330     C                   open      tivin00r
016200010330     C                   endif
016300050408     C                   if        not %open(fivabwwr)
016400050408     C                   open      fivabwwr
016500010330     C                   endif
016600010330     C*
016700010604     C                   clear                   §CTROK
016800010604     C                   clear                   §CTRMO
016900010604     C                   clear                   §CTRNO
017000010330     C*
017100010330     C                   DO        *HIVAL
017200010330     C*
017300010330     C                   READ      tivin00r                               70
017400010618     C*
017500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017600010618     C                   if        vinflg = '1'
017700010618     C                   eval      flgOk = '1'
017800010618     C                   endif
017900010618     C*
018000010330     C                   if        vindta > *blanks
018100010330     C                   add       1             rrnum
018200010330     C*
018300010601     C                   if        *in70 = *off and
018400010330     C                             (vinflg = *blanks
018500010330     C                              or vinflg = '0'
018600010330     C                              or vinflg = '2')
018700010330     C*
018800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018900010711     C                   if        vinflg = *blanks or vinflg = '0'
019000010711     C                   clear                   vinmsg
019100010711     C                   endif
019200070601     C*
019300070601     C                   exsr      inzvar
019400070601     C                   exsr      defcam
019500070601     C                   exsr      impvab
019600010601     C*
019700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019800010605     C                   eval      depfil = VABLNP
019900010601     C                   exsr      repfil
020000010601     C                   if        depfil = invfil
020100021025     C                   if        vlrpoi = 999
020200021025     C                   MOVE(P)   invfil        VABFGS
020300021025     C                   else
020400021025     C                   MOVE(P)   vlrpoi        VABFGS
020500021025     C                   endif
020600010601     C*
020700010601     C                   exsr      PREELA
020800010601     C*
020900010604     C* Ebbene...
021000010604     C*
021100010604     C  N31              ADD       1             §CTROK            7 0
021200010604     C   32              ADD       1             §CTRMO            7 0
021300010604     C   31              ADD       1             §CTRNO            7 0
021400071003     C                   exsr      wrivab                                       => scrivo VAB
021500010604     C*
021600020722     C                   endif
021700020722     C*
021800010604     C                   if        *in31 = *off and
021900010604     C                             *in32 = *off
022000010604     C                   eval      vinflg = '1'
022100010604     C                   else
022200010604     C                   eval      recko = vindta
022300010604     C                   except    rigadett
022400010604     C                   eval      vinflg = '2'
022500010604     C                   endif
022600010604     C                   endif
022700010604     C*
022800010330     C                   else
022900010330     C                   eval      vinflg = '1'
023000010330     C                   endif
023100010601     C*
023200010601     C  N70              update    tivin000
023300010330     C*
023400010330     C  N70              ENDdo
023500010601     C*
023600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023700010601     C                   if        cntNonEl = *zeros or
023800010601     C                             flgMulti = '0'
023900010330     C* Se non ci sono record con errori ...
024000010601     C                   if        §ctrno = 0 and
024100010604     C                             §ctrmo = 0 and
024200010601     C                             flgStato <> '2'
024300010330     C* ... restituisco esito OK.
024400010330     C                   eval      wrkesito = '0'
024500010330     C                   else
024600010330     C                   if        §ctrok > 0
024700010330     C                   eval      wrkesito = '1'
024800010330     C                   else
024900010615     C                   if        flgOk = '0'
025000010615     C                   eval      wrkesito = '2'
025100010615     C                   else
025200010615     C                   eval      wrkesito = '6'
025300010615     C                   endif
025400010330     C                   endif
025500010330     C                   endif
025600010601     C                   else
025700010601     C                   eval      wrkesito = '9'
025800010601     C                   endif
025900010330     C*
026000010330     C                   if        %open(tivin00r)
026100010330     C                   close     tivin00r
026200010330     C                   endif
026300050408     C                   if        %open(fivabwwr)
026400050408     C                   close     fivabwwr
026500010330     C                   endif
026600010601     C*
026700010601     C                   if        vlrpoi <> 999
026800010601     C                   eval      invfil = vlrpoi
026900010601     C                   endif
027000010330     C*
027100010330     C                   if        §ctrok > 0
027200010601     C                             and invfil > *zeros
027300010330     C                   exsr      invio
027400010330     C                   endif
027500010601     C*
027600010618     C                   if        flgGiro = '1'
027700010601     C                   exsr      endela
027800010618     C                   endif
027900010330     C*
028000010330     C                   ENDSR
028100010330     C***
028200070601
028300070601     C*----------------------------------------------------*
028400070601     C*  SCARICAMENTO BUFFER RECORDS VAB
028500070601     C*----------------------------------------------------*
028600070601     C     WRIVAB        BEGSR
028700080321     C*
028800070601     C  N31              WRITE     FIVAB000
028900070601     C*
029000070601     C                   ENDSR
029100010601
029200010601
029300010601
029400010330     C*----------------------------------------------------*
029500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029600010330     C*----------------------------------------------------*
029700010330     C     INZVAR        BEGSR
029800010330     C*
029900020204     C                   Z-ADD     *zeros        Num5_0            5 0
030000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030100020322     C                   MOVEL     '0'           FlgCAS            1
030200010330     C*
030300010330     C                   ENDSR
030400010330     C*----------------------------------------------------*
030500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030600010330     C*----------------------------------------------------*
030700010330     C     DEFCAM        BEGSR
030800010330     C*
030900020204     C* Inizializzo il buffer del record da scrivere
031000050408     C                   CLEAR                   FIVAB000
031100020204     C* Imposto i valori di default...
031200080410     C                   EVAL      VABCCM = 0503733
031300080410     C                   EVAL      VABLNP = 050
031400080410     C                   EVAL      VABCTR = 001
031500070208     C                   EVAL      VABCBO = '1'
031600080410     C                   EVAL      VABCTM = '7Q'
031700020204     C* ... e poi verifico se sono stati passati come parametri
031800020204     C                   IF        vlrppt > *blanks
031900040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABCCM
032400020204     C                   ENDIF
032500040301     C                   ENDIF
032600040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABLNP
033100020204     C                   ENDIF
033200040301     C                   ENDIF
033300040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033500020204     C                   EXSR      CHKNUM
033600020204     C                   IF        PiInt=*on
033700020204     C                   Z-ADD     PiVal         VABCTR
033800040301     C                   ENDIF
033900020204     C                   ENDIF
034000020204     C                   ENDIF
034100020204     C*
034200010330     C                   ENDSR
034300010607     C*----------------------------------------------------*
034400050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034500010607     C*----------------------------------------------------*
034600010607     C     IMPVAB        BEGSR
034700010607     C*
034800010607     C                   SETOFF                                       3132
034900010607     C*
035000010607     C* Reperimento campi ALFA
035100080410     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
035200080410     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
035300080410     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
035400080410     C                   EVAL      VABPRD=%trim(%subst(vindta:314:5))
035500080410     C                   EVAL      VABRMA=%trim(%subst(vindta:79:11))
035600080410     C                   EVAL      VABNOT=%trim(%subst(vindta:491:35))
035700080408     C*
035800080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
035900080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036000080408     C     '@':'A'       XLATE     VABRSD        VABRSD
036100080408     C* ==
036200080321     C*
036300080410     C* GC1
036400080410     C                   IF        %subst(vindta:478:3) = 'LUN'
036500080410     C                   EVAL      VABGC1 = '1'
036600080321     C                   ENDIF
036700080410     C                   IF        %subst(vindta:478:3) = 'MAR'
036800080410     C                   EVAL      VABGC1 = '2'
036900080410     C                   ENDIF
037000080410     C                   IF        %subst(vindta:478:3) = 'MER'
037100080410     C                   EVAL      VABGC1 = '3'
037200080410     C                   ENDIF
037300080410     C                   IF        %subst(vindta:478:3) = 'GIO'
037400080410     C                   EVAL      VABGC1 = '4'
037500080410     C                   ENDIF
037600080410     C                   IF        %subst(vindta:478:3) = 'VEN'
037700080410     C                   EVAL      VABGC1 = '5'
037800080410     C                   ENDIF
037900080410     C                   IF        %subst(vindta:478:3) = 'SAB'
038000080410     C                   EVAL      VABGC1 = '6'
038100080410     C                   ENDIF
038200080410     C                   IF        %subst(vindta:478:3) = 'DOM'
038300080410     C                   EVAL      VABGC1 = '7'
038400080410     C                   ENDIF
038500080410     C* GC1
038600080410     C                   IF        %subst(vindta:481:1) = '1'
038700080410     C                   EVAL      %subst(VABGC1:2:1) = 'M'
038800080410     C                   ENDIF
038900080410     C                   IF        %subst(vindta:481:1) = '2'
039000080410     C                   EVAL      %subst(VABGC1:2:1) = 'P'
039100080410     C                   ENDIF
039200080410     C* CBO
039300080410     C                   IF        %subst(vindta:2:1) = '1'
039400080410     C                   EVAL      VABCBO = '1'
039500080410     C                   ENDIF
039600080410     C                   IF        %subst(vindta:2:1) = '6'
039700080410     C                   EVAL      VABCBO = '2'
039800080410     C                   ENDIF
039900060510     C*
040000060510     C* Reperimento campi NUMERICI
040100060510     C                   MOVEL     DATCOR        VABAAS
040200060510     C                   MOVE      DATCOR        VABMGS
040300080410     C* NSP
040400080410     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
040500060510     C                   EXSR      CHKNUM
040600060510     C                   IF        PiInt=*on
040700060510     C                   Z-ADD     PiVal         VABNSP
040800060404     C                   ELSE
040900070502     C                   SETON                                        31
041000060510     C                   Z-ADD     *zeros        VABNSP
041100060404     C                   EVAL      vinmsg = %trimr(vinmsg)
041200080410     C                             + ' ' + 'VABNSP'
041300060404     C                   ENDIF
041400080410     C* RMN
041500080410     C                   EVAL      PiStr=%trim(%subst(vindta:79:11))
041600080410     C                   EXSR      CHKNUM
041700080410     C                   IF        PiInt=*on
041800080410     C                   Z-ADD     PiVal         VABRMN
041900080410     C                   ELSE
042000080410     C                   SETON                                        31
042100080410     C                   Z-ADD     1             VABRMN
042200080410     C                   EVAL      vinmsg = %trimr(vinmsg)
042300080410     C                             + ' ' + 'VABRMN'
042400080410     C                   ENDIF
042500080408     C* CAD
042600080410     C                   EVAL      PiStr=%trim(%subst(vindta:319:8))
042700080408     C                   EXSR      CHKNUM
042800080408     C                   IF        PiInt=*on
042900080408     C                   Z-ADD     PiVal         Num5_0
043000080408     C                   MOVEL(p)  Num5_0        VABCAD
043100080408     C                   ELSE
043200080408     C                   SETON                                        32
043300080408     C                   EVAL      VABCAD = *zeros
043400080408     C                   EVAL      vinmsg = %trimr(vinmsg)
043500080408     C                             + ' ' + 'VABCAD'
043600080408     C                   ENDIF
043700080408     C* Reperisco la provincia dal CAP e dalla località
043800080408     C                   IF        VABCAD <> *blanks AND
043900080408     C                             VABPRD  = *blanks
044000080408     C                   CLEAR                   TISI95DS
044100080408     C                   EVAL      I95TCN = '3'
044200080408     C                   Z-ADD     datcor        I95DAT
044300080408     C                   EVAL      I95CAP = VABCAD
044400080408     C                   EVAL      I95LOC = VABLOD
044500080408     C                   EVAL      I95NAR = VABNZD
044600080408     C                   CALL      'TISI95R'
044700080408     C                   PARM                    TISI95DS
044800080408     C                   EVAL      VABPRD = O95PRV
044900080408     C                   ENDIF
045000080410     C* DCR/TC1
045100080410     C                   IF        %subst(vindta:490:1) = '1'
045200080410     C* DCR
045300080410     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
045400080320     C                   EXSR      CHKNUM
045500080320     C                   IF        PiInt=*on
045600080320     C                   Z-ADD     PiVal         VABDCR
045700080320     C                   ELSE
045800080320     C                   SETON                                        32
045900080320     C                   Z-ADD     *zeros        VABDCR
046000080320     C                   EVAL      vinmsg = %trimr(vinmsg)
046100080320     C                             + ' ' + 'VABDCR'
046200080320     C                   ENDIF
046300080410     C                   ELSE
046400080410     C* TC1
046500080410     C                   IF        %subst(vindta:490:1) = '2'
046600080410     C                   EVAL      VABTC1 = 'A'
046700080410     C                   ENDIF
046800080409     C                   ENDIF
046900080410     C* NCL
047000080410     C                   IF        %subst(vindta:352:5)<>'00000'
047100080410     C                   EVAL      PiStr=%trim(%subst(vindta:352:5))
047200080410     C                   EXSR      CHKNUM
047300080410     C                   IF        PiInt=*on
047400080410     C                   Z-ADD     PiVal         VABNCL
047500080410     C                   ELSE
047600080410     C                   SETON                                        32
047700080410     C                   Z-ADD     *zeros        VABNCL
047800080410     C                   EVAL      vinmsg = %trimr(vinmsg)
047900080410     C                             + ' ' + 'VABNCL'
048000080410     C                   ENDIF
048100080410     C                   ELSE
048200080410     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
048300080410     C                   EXSR      CHKNUM
048400080410     C                   IF        PiInt=*on
048500080410     C                   Z-ADD     PiVal         VABNCL
048600080410     C                   ELSE
048700080410     C                   SETON                                        32
048800080410     C                   Z-ADD     *zeros        VABNCL
048900080410     C                   EVAL      vinmsg = %trimr(vinmsg)
049000080410     C                             + ' ' + 'VABNCL'
049100080410     C                   ENDIF
049200080410     C                   ENDIF
049300071029     C* PKB
049400080410     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
049500010607     C                   EXSR      CHKNUM
049600010607     C                   IF        PiNum=*on
049700080408     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
049800071003     C                   Z-ADD(H)  PiVal         VABPKB
049900010607     C                   ELSE
050000010607     C                   SETON                                        32
050100010607     C                   Z-ADD     *zeros        VABPKB
050200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
050300010607     C                             + ' ' + 'VABPKB'
050400010607     C                   ENDIF
050500080320     C* CAS
050600080410     C                   IF        %subst(vindta:388:13)<>'0000000000000'
050700080408     C                   EVAL      FlgCAS = '1'
050800080408     C                   EVAL      VABVCA = 'EUR'
050900080410     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
051000080320     C                   EXSR      CHKNUM
051100080320     C                   IF        PiNum=*on
051200080410     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
051300080410     C                   Z-ADD(H)  PiVal         VABCAS
051400080320     C                   ELSE
051500080320     C                   SETON                                        32
051600080320     C                   Z-ADD     *zeros        VABCAS
051700080320     C                   EVAL      vinmsg = %trimr(vinmsg)
051800080320     C                             + ' ' + 'VABCAS'
051900080408     C                   ENDIF
052000080320     C                   ENDIF
052100080410     C* IAS
052200080410     C                   IF        %subst(vindta:558:13)<>'0000000000000'
052300080410     C                   EVAL      VABVAS = 'EUR'
052400080410     C                   EVAL      PiStr=%trim(%subst(vindta:558:13))
052500080410     C                   EXSR      CHKNUM
052600080410     C                   IF        PiNum=*on
052700080410     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
052800080410     C                   Z-ADD(H)  PiVal         VABIAS
052900080410     C                   ELSE
053000080410     C                   SETON                                        32
053100080410     C                   Z-ADD     *zeros        VABIAS
053200080410     C                   EVAL      vinmsg = %trimr(vinmsg)
053300080410     C                             + ' ' + 'VABIAS'
053400080410     C                   ENDIF
053500080410     C                   ENDIF
053600020322     C*
053700020322     C* Considerazioni finali su CBO/CAS
053800050908     C                   IF        FlgCAS = '1'
053900040713     C                   IF        VABCBO = '1'
054000020322     C                   EVAL      VABCBO = '4'
054100040713     C                   ENDIF
054200040713     C                   IF        VABCBO = '2'
054300040713     C                   EVAL      VABCBO = '6'
054400040713     C                   ENDIF
054500020322     C                   ENDIF
054600020204     C*
054700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054800020204     C                   EXSR      CHKIMPDIV
054900020204     C*
055000010607     C                   ENDSR
055100010607     C*----------------------------------------------------*
055200010601
055300020204
055400020204     C*----------------------------------------------------*
055500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055600020204     C*----------------------------------------------------*
055700020204     C     CHKIMPDIV     BEGSR
055800020204     C*
055900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056000020204     C                   Z-ADD     *zeros        wrkDec            9 9
056100020204     C*
056200020204     C* Come prima cosa effettuo considerazioni sulla divisa
056300020204     C                   IF        vabIAS > *zeros
056400020204     C                   IF        vabVAS <> 'EUR'
056500020204     C                   EVAL      vabVAS =  'ITL'
056600020204     C                   ENDIF
056700020204     C                   ENDIF
056800020204     C*
056900020204     C                   IF        vabCAS > *zeros
057000020204     C                   IF        vabVCA <> 'EUR'
057100020204     C                   EVAL      vabVCA =  'ITL'
057200020204     C                   ENDIF
057300020204     C                   ENDIF
057400020204     C*
057500020204     C                   IF        vabVMD > *zeros
057600020321     C                   IF        vabVAD <> 'EUR'
057700020204     C                   EVAL      vabVAD =  'ITL'
057800020204     C                   ENDIF
057900020204     C                   ENDIF
058000020204     C*
058100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058200020204     C                   Z-ADD     vabIAS        wrkDec
058300020204     C                   IF        wrkDec > *zeros
058400020204     C                   IF        vabVAS = 'ITL'
058500020204     C                   EVAL      vabIAS = *zeros
058600020204     C                   ENDIF
058700020204     C                   ENDIF
058800020204     C*
058900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
059000020204     C                   Z-ADD     vabCAS        wrkDec
059100020204     C                   IF        wrkDec > *zeros
059200020204     C                   IF        vabVCA = 'ITL'
059300020204     C                   EVAL      vabCAS = *zeros
059400020204     C                   ENDIF
059500020204     C                   ENDIF
059600020204     C*
059700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059800020204     C                   Z-ADD     vabVMD        wrkDec
059900020204     C                   IF        wrkDec > *zeros
060000020204     C                   IF        vabVAD = 'ITL'
060100020204     C                   EVAL      vabVMD = *zeros
060200020204     C                   ENDIF
060300020204     C                   ENDIF
060400020204     C*
060500020204     C                   ENDSR
060600020204     C***
060700020204
060800010330
060900010330
061000010330     C*----------------------------------------------------*
061100010330     C*  CONTROLLO NUMERICITA' CAMPI
061200010330     C*----------------------------------------------------*
061300010330     C     CHKNUM        BEGSR
061400010330     C*
061500010606     C                   IF        PiDecChr = *blanks
061600080409     C                   EVAL      PiDecChr = '.'
061700010606     C                   ENDIF
061800010606     C*
061900010606     C                   CALL(e)   'ISNUMERIC'
062000010330     C                   PARM                    PiStr            30
062100010606     C                   PARM                    PiDecChr          1
062200010330     C                   PARM      *ZEROS        PiVal            30 9
062300010330     C                   PARM      '0'           PiInt             1
062400010330     C                   PARM      '0'           PiNum             1
062500010330     C                   IF        %error
062600010606     C                   EVAL      PiNum=*off
062700010330     C                   ENDIF
062800010330     C*
062900010330     C                   ENDSR
063000010330     C***
063100010330
063200010601
063300010601
063400010601
063500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
063600010601     C     repfil        BEGSR
063700010601     C*
063800010601     C                   if        invfil = *zeros and
063900010601     C                             depfil > *zeros and
064000010629     C                             (vinflg = *blanks or
064100010629     C                              vinflg = *zeros)
064200010601     C
064300010601     C                   eval      invfil = depfil
064400010601     C                   endif
064500010601     C*
064600010601     C                   if        depfil <> invfil and
064700010601     C                             invfil > *zeros
064800010601     C                   eval      flgMulti = '1'
064900010601     C                   if        vinflg = *blanks
065000010601     C                   add       1             cntNonEl
065100010601     C                   endif
065200010601     C                   endif
065300010601     C*
065400010601     C                   if        vinflg = '2'
065500010601     C                   eval      flgStato = '2'
065600010601     C                   endif
065700010601     C*
065800010601     C                   ENDSR
065900010601     C***
066000010601
066100010601
066200010601
066300010330
066400010330
066500010330
066600990920      /TITLE Invio dei dati al punto operativo.
066700000613     C     invio         BEGSR
066800990920     C*
066900990920     C                   reset                   dscmz
067000010601     C                   move      invfil        cmzdst
067100990920     C                   eval      cmzfld = vlrfou
067200990920     C                   eval      cmzmbd = vlrhdl
067300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
067400000710     C                   if        prmfir = *blanks
067500050408     C                   eval      cmzfla = 'FIVAB00F'
067600050408     C                   eval      cmzmba = 'FIVAB00F'
067700000710     C                   else
067800000710     C                   eval      cmzfla = prmfir
067900000710     C                   eval      cmzmba = prmfir
068000000710     C                   endif
068100990920     C                   eval      cmznrr = *zeros
068200990920     C                   move      §ctrok        cmznrr
068300021018     C                   eval      cmzlba = vlrfl1
068400990920     C                   call(e)   'TIS711C'
068500990920     C                   parm                    dscmz
068600990921     C                   parm      *blanks       esito
068700990920     C                   if        %error
068800990920     C                             or cmzerr = '1'
068900990921     C                             or esito  = '1'
069000000710     C                   eval      wrkesito = '3'
069100990920     C                   endif
069200990920     C*
069300000613     C                   ENDSR
069400990910
069500010601
069600010601
069700010601
069800010601
069900010601      /TITLE Invio dei dati al punto operativo.
070000010601     C     opeini        BEGSR
070100010601     C*
070200010601     C* Inizializzo flag e contatori operativi
070300010601     C                   movel     '0'           flgGiro           1
070400010601     C                   movel     '0'           flgMulti          1
070500010601     C                   movel     '1'           flgStato          1
070600010615     C                   movel     '0'           flgOk             1
070700010601     C                   z-add     *zeros        cntNonEl         10 0
070800010601     C                   z-add     *zeros        depfil            3 0
070900010601     C                   z-add     *zeros        invfil            3 0
071000010601     C*
071100010601     C                   ENDSR
071200010601     C***
071300010601
071400010601
071500010601
071600010330
071700010330
071800000613     C     *inzsr        BEGSR
071900990910     C*
072000990910     C     *entry        plist
072100990920     C                   parm                    tivlrds
072200990921     C                   parm      wrkesito      esito
072300000724     C                   parm                    prmlit
072400000710     C                   parm                    prmfir
072500010330     C*
072600010330     C* CALCOLA LA DATA CORRENTE
072700010330     C                   time                    wn14             14 0
072800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
072900010330     C                   z-add     wn8           g08dat
073000010330     C                   z-add     *zeros        g08inv
073100010330     C                   movel     '0'           g08err
073200010330     C                   call      'XSRDA8'
073300010330     C                   parm                    wlbda8
073400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
073500000613     C*
073600000613     C                   ENDSR
073700000613     C***
073800990908
073900080410     Otitv1f3p  E            riepilogo         2
074000990915     O                                              'Upload via Internet'
074100990915     O                                           +1 'Traduzione TIVIN00R -'
074200050408     O                                           +1 'FIVABWWR'
074300990915     O                                           +1 'Testate bolle'
074400990915     O          E            riepilogo   2
074500990915     O                       wrkdata
074600990915     O                       wrkora              +1
074700990915     O                       procname            +1
074800990915     O          E            riepilogo   2
074900990915     O                                              'Cliente..................:'
075000010605     O                       VABCCM        z     +1
075100990915     O          E            riepilogo   2
075200990920     O                                              'Riferimento Strategi.....:'
075300990920     O                       vlrhdl              +1
075400990915     O          E            riepilogo   2
075500990915     O                                              'Giusti...................:'
075600971022     O                       §CTROK        2   +  1
075700990915     O          E            riepilogo   2
075800990915     O                                              'Sbagliati e corretti.....:'
075900971022     O                       §CTRMO        2   +  1
076000990915     O          E            riepilogo   2
076100990915     O                                              'Sbagliati e scartati.....:'
076200971022     O                       §CTRNO        2   +  1
076300000613
076400080410     Otitv1f3ps E            testdett          2
076500000613     O                                              'Upload via Internet'
076600000613     O                                           +1 'Traduzione TIVIN00R -'
076700050408     O                                           +1 'FIVABWWR'
076800000613     O                                           +1 'Testate bolle'
076900000616     O          E            testdett    3
077000000613     O                                           +2 'N° rec'
077100000613     O                                           +3 'Anteprima contenuto'
077200000616     O          E            rigadett    2
077300000613     O                       rrnum               +2
077400000621     O                       recko               +3
077500000616     O          E            findett     2
077600000613     O                       wrkdata
077700000613     O                       wrkora              +1
077800000613     O                       procname            +1
077900000616     O          E            findett     2
078000000613     O                                              'Cliente..................:'
078100010605     O                       VABCCM        z     +1
078200000616     O          E            findett     2
078300000613     O                                              'Riferimento Strategi.....:'
078400000613     O                       vlrhdl              +1
078500000616     O          E            findett     2
078600000613     O                                              'Giusti...................:'
078700000613     O                       §CTROK        2   +  1
078800000616     O          E            findett     2
078900000613     O                                              'Sbagliati e corretti.....:'
079000000613     O                       §CTRMO        2   +  1
079100000616     O          E            findett     2
079200000613     O                                              'Sbagliati e scartati.....:'
079300000613     O                       §CTRNO        2   +  1
079400000613** CMD - COMANDI CL
079500080410OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1F3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
079600080410OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1F3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
079700080410DLTOVR FILE(TITV1F3P TITV1F3PS) LVL(*)
079800010606
079900000613
