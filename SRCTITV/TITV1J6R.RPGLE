000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200080611     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700081020     Ftitv1j6p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900081020     Ftitv1j6ps O    f  198        PRINTER usropn
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
004100080611     D*------------------
004200080611     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
004300080611     D*------------------
004400080611     D PiStr           S            100A   INZ
004500080611     D PiDecChr        S              1A   INZ
004600080611     D PiVal           S             63S30 INZ
004700080611     D PiInt           S               N   INZ
004800080611     D PiNum           S               N   INZ
004900080611
005000080611     D*------------------
005100080611     D* LINKING A DEFINIZIONI ESTERNE
005200080611     D*------------------
005300080611     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005400010330
005500010330
005600990915     C                   time                    wrkdata
005700990915     C                   time                    wrkora
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100010601     C*
006200010601     C                   exsr      opeini
006300010605     C                   exsr      rwvab
006400070502     C*
006500070502     C* Effettuo lancio TISI95 solo x chiusura
006600070502     C                   CLEAR                   TISI95DS
006700070502     C                   EVAL      I95TLA = 'C'
006800070502     C                   CALL      'TISI95R'
006900070502     C                   PARM                    TISI95DS
007000010601     C*
007100010601     C                   seton                                        lr
007200010601
007300010601
007400010601
007500010601
007600010601     C*--------------------------------------------------------
007700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007800010601     C*--------------------------------------------------------
007900010601     C     PREELA        BEGSR
008000010601     C*
008100010601     C* SE OCCORRE SPEDIRE IN FILIALE
008200010601     C                   if        invfil <> *zeros and
008300010601     C                             flgGiro = '0'
008400010601     C*
008500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008600010601     C                   eval      flgGiro = '1'
008700000724     C*
008800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009000010601     C     invfil        chain     azorg01l
009100000724     C                   if        %found
009200000724     C                   movel(p)  CMD(1)        depcmd
009300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009400000724     C*
009500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009600011113     C                   Z-ADD     150           LENGH            15 5
009700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009800000724     C                   PARM                    depcmd
009900000724     C                   PARM                    LENGH
010000000724     C*
010100000724     C                   endif
010200000616     C*
010300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010400000616     C                   movel(p)  CMD(2)        depcmd
010500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010600000616     C*
010700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010800011113     C                   Z-ADD     150           LENGH            15 5
010900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011000000616     C                   PARM                    depcmd
011100000616     C                   PARM                    LENGH
011200000613     C*
011300081020     C                   if        not %open(titv1j6ps)
011400081020     C                   open      titv1j6ps
011500000616     C                   except    testdett
011600000613     C                   endif
011700010601     C*
011800010601     C                   endif
011900010601     C*
012000010601     C                   ENDSR
012100010601     C***
012200010601
012300010601
012400010601
012500010601     C*--------------------------------------------------------
012600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012700010601     C*--------------------------------------------------------
012800010601     C     ENDELA        BEGSR
012900990915     C*
013000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013100000613     C*
013200081020     C                   if        %open(titv1j6ps)
013300000616     C                   except    findett
013400081020     C                   close     titv1j6ps
013500000613     C                   endif
013600000616     C*
013700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013800011113     C                   Z-ADD     150           LENGH            15 5
013900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014000000616     C                   PARM                    CMD(3)
014100000616     C                   PARM                    LENGH
014200000616     C*
014300010601     C                   ENDSR
014400010601     C***
014500010601
014600010601
014700000613
014800000613     C*--------------------------------------------------------
014900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015000000613     C*--------------------------------------------------------
015100000621     C     STPR          BEGSR
015200000613     C*
015300081020     C                   if        not %open(titv1j6p)
015400081020     C                   open      titv1j6p
015500990915     C                   endif
015600990915     C*
015700990915     C                   except    riepilogo
015800990915     C*
015900081020     C                   if        %open(titv1j6p)
016000081020     C                   close     titv1j6p
016100990914     C                   endif
016200000613     C*
016300000613     C                   ENDSR
016400000613     C***
016500000613
016600010601
016700010601
016800010330     C*--------------------------------------------------------
016900050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
017000010330     C*--------------------------------------------------------
017100010605     C     RWVAB         BEGSR
017200050408     C*
017300010330     C                   if        not %open(tivin00r)
017400010330     C                   open      tivin00r
017500010330     C                   endif
017600050408     C                   if        not %open(fivabwwr)
017700050408     C                   open      fivabwwr
017800010330     C                   endif
017900010330     C*
018000010604     C                   clear                   §CTROK
018100010604     C                   clear                   §CTRMO
018200010604     C                   clear                   §CTRNO
018300010330     C*
018400010330     C                   DO        *HIVAL
018500010330     C*
018600010330     C                   READ      tivin00r                               70
018700010618     C*
018800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018900010618     C                   if        vinflg = '1'
019000010618     C                   eval      flgOk = '1'
019100010618     C                   endif
019200010618     C*
019300010330     C                   if        vindta > *blanks
019400010330     C                   add       1             rrnum
019500010330     C*
019600010601     C                   if        *in70 = *off and
019700010330     C                             (vinflg = *blanks
019800010330     C                              or vinflg = '0'
019900010330     C                              or vinflg = '2')
020000010330     C*
020100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020200010711     C                   if        vinflg = *blanks or vinflg = '0'
020300010711     C                   clear                   vinmsg
020400010711     C                   endif
020500070601     C*
020600070601     C                   exsr      inzvar
020700070601     C                   exsr      defcam
020800070601     C                   exsr      impvab
020900010601     C*
021000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021100010605     C                   eval      depfil = VABLNP
021200010601     C                   exsr      repfil
021300010601     C                   if        depfil = invfil
021400021025     C                   if        vlrpoi = 999
021500021025     C                   MOVE(P)   invfil        VABFGS
021600021025     C                   else
021700021025     C                   MOVE(P)   vlrpoi        VABFGS
021800021025     C                   endif
021900010601     C*
022000010601     C                   exsr      PREELA
022100010601     C*
022200010604     C* Ebbene...
022300010604     C*
022400010604     C  N31              ADD       1             §CTROK            7 0
022500010604     C   32              ADD       1             §CTRMO            7 0
022600010604     C   31              ADD       1             §CTRNO            7 0
022700071003     C                   exsr      wrivab                                       => scrivo VAB
022800010604     C*
022900020722     C                   endif
023000020722     C*
023100010604     C                   if        *in31 = *off and
023200010604     C                             *in32 = *off
023300010604     C                   eval      vinflg = '1'
023400010604     C                   else
023500010604     C                   eval      recko = vindta
023600010604     C                   except    rigadett
023700010604     C                   eval      vinflg = '2'
023800010604     C                   endif
023900010604     C                   endif
024000010604     C*
024100010330     C                   else
024200010330     C                   eval      vinflg = '1'
024300010330     C                   endif
024400010601     C*
024500010601     C  N70              update    tivin000
024600010330     C*
024700010330     C  N70              ENDdo
024800010601     C*
024900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025000010601     C                   if        cntNonEl = *zeros or
025100010601     C                             flgMulti = '0'
025200010330     C* Se non ci sono record con errori ...
025300010601     C                   if        §ctrno = 0 and
025400010604     C                             §ctrmo = 0 and
025500010601     C                             flgStato <> '2'
025600010330     C* ... restituisco esito OK.
025700010330     C                   eval      wrkesito = '0'
025800010330     C                   else
025900010330     C                   if        §ctrok > 0
026000010330     C                   eval      wrkesito = '1'
026100010330     C                   else
026200010615     C                   if        flgOk = '0'
026300010615     C                   eval      wrkesito = '2'
026400010615     C                   else
026500010615     C                   eval      wrkesito = '6'
026600010615     C                   endif
026700010330     C                   endif
026800010330     C                   endif
026900010601     C                   else
027000010601     C                   eval      wrkesito = '9'
027100010601     C                   endif
027200010330     C*
027300010330     C                   if        %open(tivin00r)
027400010330     C                   close     tivin00r
027500010330     C                   endif
027600050408     C                   if        %open(fivabwwr)
027700050408     C                   close     fivabwwr
027800010330     C                   endif
027900010601     C*
028000010601     C                   if        vlrpoi <> 999
028100010601     C                   eval      invfil = vlrpoi
028200010601     C                   endif
028300010330     C*
028400010330     C                   if        §ctrok > 0
028500010601     C                             and invfil > *zeros
028600010330     C                   exsr      invio
028700010330     C                   endif
028800010601     C*
028900010618     C                   if        flgGiro = '1'
029000010601     C                   exsr      endela
029100010618     C                   endif
029200010330     C*
029300010330     C                   ENDSR
029400010330     C***
029500070601
029600070601     C*----------------------------------------------------*
029700070601     C*  SCARICAMENTO BUFFER RECORDS VAB
029800070601     C*----------------------------------------------------*
029900070601     C     WRIVAB        BEGSR
030000080321     C*
030100070601     C  N31              WRITE     FIVAB000
030200070601     C*
030300070601     C                   ENDSR
030400010601
030500010601
030600010601
030700010330     C*----------------------------------------------------*
030800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030900010330     C*----------------------------------------------------*
031000010330     C     INZVAR        BEGSR
031100010330     C*
031200020204     C                   Z-ADD     *zeros        Num5_0            5 0
031300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031400020322     C                   MOVEL     '0'           FlgCAS            1
031500010330     C*
031600010330     C                   ENDSR
031700010330     C*----------------------------------------------------*
031800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
031900010330     C*----------------------------------------------------*
032000010330     C     DEFCAM        BEGSR
032100010330     C*
032200020204     C* Inizializzo il buffer del record da scrivere
032300050408     C                   CLEAR                   FIVAB000
032400020204     C* Imposto i valori di default...
032500081020     C                   EVAL      VABCCM = 1201475
032600081020     C                   EVAL      VABLNP = 120
032700080903     C                   EVAL      VABCTR = 000
032800070208     C                   EVAL      VABCBO = '1'
032900020204     C* ... e poi verifico se sono stati passati come parametri
033000020204     C                   IF        vlrppt > *blanks
033100040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
033200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033300020204     C                   EXSR      CHKNUM
033400020204     C                   IF        PiInt=*on
033500020204     C                   Z-ADD     PiVal         VABCCM
033600020204     C                   ENDIF
033700040301     C                   ENDIF
033800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
033900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034000020204     C                   EXSR      CHKNUM
034100020204     C                   IF        PiInt=*on
034200020204     C                   Z-ADD     PiVal         VABLNP
034300020204     C                   ENDIF
034400040301     C                   ENDIF
034500040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
034600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034700020204     C                   EXSR      CHKNUM
034800020204     C                   IF        PiInt=*on
034900020204     C                   Z-ADD     PiVal         VABCTR
035000040301     C                   ENDIF
035100020204     C                   ENDIF
035200020204     C                   ENDIF
035300020204     C*
035400010330     C                   ENDSR
035500010607     C*----------------------------------------------------*
035600050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035700010607     C*----------------------------------------------------*
035800010607     C     IMPVAB        BEGSR
035900010607     C*
036000010607     C                   SETOFF                                       3132
036100010607     C*
036200010607     C* Reperimento campi ALFA
036300081020     C                   EVAL      VABRSD=%trim(%subst(vindta:15:45))
036400081020     C                   EVAL      VABIND=%trim(%subst(vindta:60:35))
036500081020     C                   EVAL      VABLOD=%trim(%subst(vindta:95:20))
036600081020     C                   EVAL      VABPRD=%trim(%subst(vindta:115:2))
036700081020     C                   EVAL      VABRMA=%trim(%subst(vindta:1:8))
036800081020     C                   MOVEL     *blanks       wNOTE            70
036900081020     C                   EVAL      wNOTE=%trim(%subst(vindta:154:40))+' '+
037000081020     C                                   %trim(%subst(vindta:240:30))
037100081020     C                   IF        %subst(vindta:228:1) = 'F'
037200081020     C                   EVAL      VABCBO='1'
037300081020     C                   ENDIF
037400081020     C                   IF        %subst(vindta:228:1) = 'A'
037500081020     C                   EVAL      VABCBO='2'
037600081020     C                   ENDIF
037700080408     C*
037800080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
037900080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
038000080408     C     '@':'A'       XLATE     VABRSD        VABRSD
038100080408     C* ==
038200060510     C*
038300060510     C* Reperimento campi NUMERICI
038400060510     C                   MOVEL     DATCOR        VABAAS
038500060510     C                   MOVE      DATCOR        VABMGS
038600080410     C* NSP
038700081020     C                   EVAL      PiStr=%trim(%subst(vindta:1:8))
038800060510     C                   EXSR      CHKNUM
038900060510     C                   IF        PiInt=*on
039000060510     C                   Z-ADD     PiVal         VABNSP
039100060404     C                   ELSE
039200070502     C                   SETON                                        31
039300060510     C                   Z-ADD     *zeros        VABNSP
039400060404     C                   EVAL      vinmsg = %trimr(vinmsg)
039500080410     C                             + ' ' + 'VABNSP'
039600060404     C                   ENDIF
039700080410     C* RMN
039800081020     C                   EVAL      PiStr=%trim(%subst(vindta:1:8))
039900080410     C                   EXSR      CHKNUM
040000080410     C                   IF        PiInt=*on
040100080410     C                   Z-ADD     PiVal         VABRMN
040200080410     C                   ELSE
040300080410     C                   SETON                                        31
040400080410     C                   Z-ADD     1             VABRMN
040500080410     C                   EVAL      vinmsg = %trimr(vinmsg)
040600080410     C                             + ' ' + 'VABRMN'
040700080410     C                   ENDIF
040800081020     C* CAD
040900081020     C                   EVAL      PiStr=%trim(%subst(vindta:229:5))
041000080408     C                   EXSR      CHKNUM
041100080408     C                   IF        PiInt=*on
041200080408     C                   Z-ADD     PiVal         Num5_0
041300080408     C                   MOVEL(p)  Num5_0        VABCAD
041400080408     C                   ELSE
041500080408     C                   SETON                                        32
041600080408     C                   EVAL      VABCAD = *zeros
041700080408     C                   EVAL      vinmsg = %trimr(vinmsg)
041800080408     C                             + ' ' + 'VABCAD'
041900080408     C                   ENDIF
042000080408     C* Reperisco la provincia dal CAP e dalla località
042100080408     C                   IF        VABCAD <> *blanks AND
042200080408     C                             VABPRD  = *blanks
042300080408     C                   CLEAR                   TISI95DS
042400080408     C                   EVAL      I95TCN = '3'
042500080408     C                   Z-ADD     datcor        I95DAT
042600080408     C                   EVAL      I95CAP = VABCAD
042700080408     C                   EVAL      I95LOC = VABLOD
042800080408     C                   EVAL      I95NAR = VABNZD
042900080408     C                   CALL      'TISI95R'
043000080408     C                   PARM                    TISI95DS
043100080408     C                   EVAL      VABPRD = O95PRV
043200080408     C                   ENDIF
043300080410     C* NCL
043400081020     C                   EVAL      PiStr=%trim(%subst(vindta:117:5))
043500080410     C                   EXSR      CHKNUM
043600080410     C                   IF        PiInt=*on
043700080410     C                   Z-ADD     PiVal         VABNCL
043800080410     C                   ELSE
043900080410     C                   SETON                                        32
044000080410     C                   Z-ADD     *zeros        VABNCL
044100080410     C                   EVAL      vinmsg = %trimr(vinmsg)
044200080410     C                             + ' ' + 'VABNCL'
044300080410     C                   ENDIF
044400071029     C* PKB
044500081020     C                   EVAL      PiStr=%trim(%subst(vindta:122:7))
044600010607     C                   EXSR      CHKNUM
044700010607     C                   IF        PiNum=*on
044800081020     C                   EVAL      PiVal = PiVal / 10                           * gestisco 1 dec.
044900080903     C                   Z-ADD(H)  PiVal         VABPKB
045000010607     C                   ELSE
045100010607     C                   SETON                                        32
045200010607     C                   Z-ADD     *zeros        VABPKB
045300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045400010607     C                             + ' ' + 'VABPKB'
045500010607     C                   ENDIF
045600081020     C* VLB
045700081020     C                   EVAL      PiStr=%trim(%subst(vindta:129:4))
045800081020     C                   EXSR      CHKNUM
045900081020     C                   IF        PiNum=*on
046000081020     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
046100081020     C                   Z-ADD(H)  PiVal         VABVLB
046200081020     C                   ELSE
046300081020     C                   SETON                                        32
046400081020     C                   Z-ADD     *zeros        VABVLB
046500081020     C                   EVAL      vinmsg = %trimr(vinmsg)
046600081020     C                             + ' ' + 'VABVLB'
046700081020     C                   ENDIF
046800080320     C* CAS
046900081020     C                   IF        %trim(%subst(vindta:133:8))<>*all'0' AND
047000081020     C                             %trim(%subst(vindta:133:8))<>*blanks
047100080408     C                   EVAL      FlgCAS = '1'
047200080903     C                   EVAL      VABVCA='EUR'
047300081020     C                   EVAL      PiStr=%trim(%subst(vindta:133:8))
047400080320     C                   EXSR      CHKNUM
047500080320     C                   IF        PiNum=*on
047600081020     C                   Z-ADD     PiVal         VABCAS
047700080320     C                   ELSE
047800080320     C                   SETON                                        32
047900080320     C                   Z-ADD     *zeros        VABCAS
048000080320     C                   EVAL      vinmsg = %trimr(vinmsg)
048100080320     C                             + ' ' + 'VABCAS'
048200080408     C                   ENDIF
048300080320     C                   ENDIF
048400020322     C*
048500020322     C* Considerazioni finali su CBO/CAS
048600050908     C                   IF        FlgCAS = '1'
048700040713     C                   IF        VABCBO = '1'
048800020322     C                   EVAL      VABCBO = '4'
048900040713     C                   ENDIF
049000040713     C                   IF        VABCBO = '2'
049100040713     C                   EVAL      VABCBO = '6'
049200040713     C                   ENDIF
049300020322     C                   ENDIF
049400020204     C*
049500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049600020204     C                   EXSR      CHKIMPDIV
049700020204     C*
049800010607     C                   ENDSR
049900010607     C*----------------------------------------------------*
050000010601
050100020204
050200020204     C*----------------------------------------------------*
050300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050400020204     C*----------------------------------------------------*
050500020204     C     CHKIMPDIV     BEGSR
050600020204     C*
050700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050800020204     C                   Z-ADD     *zeros        wrkDec            9 9
050900020204     C*
051000020204     C* Come prima cosa effettuo considerazioni sulla divisa
051100020204     C                   IF        vabIAS > *zeros
051200020204     C                   IF        vabVAS <> 'EUR'
051300020204     C                   EVAL      vabVAS =  'ITL'
051400020204     C                   ENDIF
051500020204     C                   ENDIF
051600020204     C*
051700020204     C                   IF        vabCAS > *zeros
051800020204     C                   IF        vabVCA <> 'EUR'
051900020204     C                   EVAL      vabVCA =  'ITL'
052000020204     C                   ENDIF
052100020204     C                   ENDIF
052200020204     C*
052300020204     C                   IF        vabVMD > *zeros
052400020321     C                   IF        vabVAD <> 'EUR'
052500020204     C                   EVAL      vabVAD =  'ITL'
052600020204     C                   ENDIF
052700020204     C                   ENDIF
052800020204     C*
052900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053000020204     C                   Z-ADD     vabIAS        wrkDec
053100020204     C                   IF        wrkDec > *zeros
053200020204     C                   IF        vabVAS = 'ITL'
053300020204     C                   EVAL      vabIAS = *zeros
053400020204     C                   ENDIF
053500020204     C                   ENDIF
053600020204     C*
053700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
053800020204     C                   Z-ADD     vabCAS        wrkDec
053900020204     C                   IF        wrkDec > *zeros
054000020204     C                   IF        vabVCA = 'ITL'
054100020204     C                   EVAL      vabCAS = *zeros
054200020204     C                   ENDIF
054300020204     C                   ENDIF
054400020204     C*
054500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054600020204     C                   Z-ADD     vabVMD        wrkDec
054700020204     C                   IF        wrkDec > *zeros
054800020204     C                   IF        vabVAD = 'ITL'
054900020204     C                   EVAL      vabVMD = *zeros
055000020204     C                   ENDIF
055100020204     C                   ENDIF
055200020204     C*
055300020204     C                   ENDSR
055400020204     C***
055500020204
055600010330
055700010330
055800010330     C*----------------------------------------------------*
055900010330     C*  CONTROLLO NUMERICITA' CAMPI
056000010330     C*----------------------------------------------------*
056100010330     C     CHKNUM        BEGSR
056200010330     C*
056300010606     C                   IF        PiDecChr = *blanks
056400080611     C                   EVAL      PiDecChr = ','
056500010606     C                   ENDIF
056600080611     C*
056700080611     C                   callp     UBISNUM_Check(PiStr
056800080611     C                                          :PiDecChr
056900080611     C                                          :PiVal
057000080611     C                                          :PiNum
057100080611     C                                          :PiInt)
057200010330     C*
057300010330     C                   ENDSR
057400010330     C***
057500010330
057600010601
057700010601
057800010601
057900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
058000010601     C     repfil        BEGSR
058100010601     C*
058200010601     C                   if        invfil = *zeros and
058300010601     C                             depfil > *zeros and
058400010629     C                             (vinflg = *blanks or
058500010629     C                              vinflg = *zeros)
058600010601     C
058700010601     C                   eval      invfil = depfil
058800010601     C                   endif
058900010601     C*
059000010601     C                   if        depfil <> invfil and
059100010601     C                             invfil > *zeros
059200010601     C                   eval      flgMulti = '1'
059300010601     C                   if        vinflg = *blanks
059400010601     C                   add       1             cntNonEl
059500010601     C                   endif
059600010601     C                   endif
059700010601     C*
059800010601     C                   if        vinflg = '2'
059900010601     C                   eval      flgStato = '2'
060000010601     C                   endif
060100010601     C*
060200010601     C                   ENDSR
060300010601     C***
060400010601
060500010601
060600010601
060700010330
060800010330
060900010330
061000990920      /TITLE Invio dei dati al punto operativo.
061100000613     C     invio         BEGSR
061200990920     C*
061300990920     C                   reset                   dscmz
061400010601     C                   move      invfil        cmzdst
061500990920     C                   eval      cmzfld = vlrfou
061600990920     C                   eval      cmzmbd = vlrhdl
061700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
061800000710     C                   if        prmfir = *blanks
061900050408     C                   eval      cmzfla = 'FIVAB00F'
062000050408     C                   eval      cmzmba = 'FIVAB00F'
062100000710     C                   else
062200000710     C                   eval      cmzfla = prmfir
062300000710     C                   eval      cmzmba = prmfir
062400000710     C                   endif
062500990920     C                   eval      cmznrr = *zeros
062600990920     C                   move      §ctrok        cmznrr
062700021018     C                   eval      cmzlba = vlrfl1
062800990920     C                   call(e)   'TIS711C'
062900990920     C                   parm                    dscmz
063000990921     C                   parm      *blanks       esito
063100990920     C                   if        %error
063200990920     C                             or cmzerr = '1'
063300990921     C                             or esito  = '1'
063400000710     C                   eval      wrkesito = '3'
063500990920     C                   endif
063600990920     C*
063700000613     C                   ENDSR
063800990910
063900010601
064000010601
064100010601
064200010601
064300010601      /TITLE Invio dei dati al punto operativo.
064400010601     C     opeini        BEGSR
064500010601     C*
064600010601     C* Inizializzo flag e contatori operativi
064700010601     C                   movel     '0'           flgGiro           1
064800010601     C                   movel     '0'           flgMulti          1
064900010601     C                   movel     '1'           flgStato          1
065000010615     C                   movel     '0'           flgOk             1
065100010601     C                   z-add     *zeros        cntNonEl         10 0
065200010601     C                   z-add     *zeros        depfil            3 0
065300010601     C                   z-add     *zeros        invfil            3 0
065400010601     C*
065500010601     C                   ENDSR
065600010601     C***
065700010601
065800010601
065900010601
066000010330
066100010330
066200000613     C     *inzsr        BEGSR
066300990910     C*
066400990910     C     *entry        plist
066500990920     C                   parm                    tivlrds
066600990921     C                   parm      wrkesito      esito
066700000724     C                   parm                    prmlit
066800000710     C                   parm                    prmfir
066900010330     C*
067000010330     C* CALCOLA LA DATA CORRENTE
067100010330     C                   time                    wn14             14 0
067200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067300010330     C                   z-add     wn8           g08dat
067400010330     C                   z-add     *zeros        g08inv
067500010330     C                   movel     '0'           g08err
067600010330     C                   call      'XSRDA8'
067700010330     C                   parm                    wlbda8
067800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067900000613     C*
068000000613     C                   ENDSR
068100000613     C***
068200990908
068300081020     Otitv1j6p  E            riepilogo         2
068400990915     O                                              'Upload via Internet'
068500990915     O                                           +1 'Traduzione TIVIN00R -'
068600050408     O                                           +1 'FIVABWWR'
068700990915     O                                           +1 'Testate bolle'
068800990915     O          E            riepilogo   2
068900990915     O                       wrkdata
069000990915     O                       wrkora              +1
069100990915     O                       procname            +1
069200990915     O          E            riepilogo   2
069300990915     O                                              'Cliente..................:'
069400010605     O                       VABCCM        z     +1
069500990915     O          E            riepilogo   2
069600990920     O                                              'Riferimento Strategi.....:'
069700990920     O                       vlrhdl              +1
069800990915     O          E            riepilogo   2
069900990915     O                                              'Giusti...................:'
070000971022     O                       §CTROK        2   +  1
070100990915     O          E            riepilogo   2
070200990915     O                                              'Sbagliati e corretti.....:'
070300971022     O                       §CTRMO        2   +  1
070400990915     O          E            riepilogo   2
070500990915     O                                              'Sbagliati e scartati.....:'
070600971022     O                       §CTRNO        2   +  1
070700000613
070800081020     Otitv1j6ps E            testdett          2
070900000613     O                                              'Upload via Internet'
071000000613     O                                           +1 'Traduzione TIVIN00R -'
071100050408     O                                           +1 'FIVABWWR'
071200000613     O                                           +1 'Testate bolle'
071300000616     O          E            testdett    3
071400000613     O                                           +2 'N° rec'
071500000613     O                                           +3 'Anteprima contenuto'
071600000616     O          E            rigadett    2
071700000613     O                       rrnum               +2
071800000621     O                       recko               +3
071900000616     O          E            findett     2
072000000613     O                       wrkdata
072100000613     O                       wrkora              +1
072200000613     O                       procname            +1
072300000616     O          E            findett     2
072400000613     O                                              'Cliente..................:'
072500010605     O                       VABCCM        z     +1
072600000616     O          E            findett     2
072700000613     O                                              'Riferimento Strategi.....:'
072800000613     O                       vlrhdl              +1
072900000616     O          E            findett     2
073000000613     O                                              'Giusti...................:'
073100000613     O                       §CTROK        2   +  1
073200000616     O          E            findett     2
073300000613     O                                              'Sbagliati e corretti.....:'
073400000613     O                       §CTRMO        2   +  1
073500000616     O          E            findett     2
073600000613     O                                              'Sbagliati e scartati.....:'
073700000613     O                       §CTRNO        2   +  1
073800000613** CMD - COMANDI CL
073900081020OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1J6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
074000081020OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1J6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
074100081020DLTOVR FILE(TITV1J6P TITV1J6PS) LVL(*)
074200010606
074300000613
