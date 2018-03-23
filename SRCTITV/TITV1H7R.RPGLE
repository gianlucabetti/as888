000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200080611     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700080707     Ftitv1h7p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900080707     Ftitv1h7ps O    f  198        PRINTER usropn
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
011300080707     C                   if        not %open(titv1h7ps)
011400080707     C                   open      titv1h7ps
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
013200080707     C                   if        %open(titv1h7ps)
013300000616     C                   except    findett
013400080707     C                   close     titv1h7ps
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
015300080707     C                   if        not %open(titv1h7p)
015400080707     C                   open      titv1h7p
015500990915     C                   endif
015600990915     C*
015700990915     C                   except    riepilogo
015800990915     C*
015900080707     C                   if        %open(titv1h7p)
016000080707     C                   close     titv1h7p
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
032500080707     C                   EVAL      VABCCM = 0894815
032600080707     C                   EVAL      VABLNP = 202
032700080611     C                   EVAL      VABCTR = 000
032800080707     C                   EVAL      VABTSP = 'C'
032900080707     C                   EVAL      VABNAS = 'CHIMICI'
033000070208     C                   EVAL      VABCBO = '1'
033100020204     C* ... e poi verifico se sono stati passati come parametri
033200020204     C                   IF        vlrppt > *blanks
033300040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
033400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033500020204     C                   EXSR      CHKNUM
033600020204     C                   IF        PiInt=*on
033700020204     C                   Z-ADD     PiVal         VABCCM
033800020204     C                   ENDIF
033900040301     C                   ENDIF
034000040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
034100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034200020204     C                   EXSR      CHKNUM
034300020204     C                   IF        PiInt=*on
034400020204     C                   Z-ADD     PiVal         VABLNP
034500020204     C                   ENDIF
034600040301     C                   ENDIF
034700040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
034800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034900020204     C                   EXSR      CHKNUM
035000020204     C                   IF        PiInt=*on
035100020204     C                   Z-ADD     PiVal         VABCTR
035200040301     C                   ENDIF
035300020204     C                   ENDIF
035400020204     C                   ENDIF
035500020204     C*
035600010330     C                   ENDSR
035700010607     C*----------------------------------------------------*
035800050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035900010607     C*----------------------------------------------------*
036000010607     C     IMPVAB        BEGSR
036100010607     C*
036200010607     C                   SETOFF                                       3132
036300010607     C*
036400010607     C* Reperimento campi ALFA
036500080707     C                   EVAL      VABRSD=%trim(%subst(vindta:24:40))
036600080707     C                   EVAL      VABIND=%trim(%subst(vindta:64:35))
036700080707     C                   EVAL      VABLOD=%trim(%subst(vindta:108:30))
036800080707     C                   EVAL      VABPRD=%trim(%subst(vindta:138:2))
036900080707     C                   EVAL      VABRMA=%trim(%subst(vindta:222:10))
037000080707     C                   EVAL      VABNOT=%trim(%subst(vindta:174:40))
037100080707     C                   IF        %subst(vindta:173:1) = 'S'
037200080707     C                   EVAL      VABTC1 = 'A'
037300080707     C                   ENDIF
037400080408     C*
037500080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
037600080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037700080408     C     '@':'A'       XLATE     VABRSD        VABRSD
037800080408     C* ==
037900060510     C*
038000060510     C* Reperimento campi NUMERICI
038100060510     C                   MOVEL     DATCOR        VABAAS
038200060510     C                   MOVE      DATCOR        VABMGS
038300080410     C* NSP
038400080707     C                   EVAL      PiStr=%trim(%subst(vindta:5:7))
038500060510     C                   EXSR      CHKNUM
038600060510     C                   IF        PiInt=*on
038700060510     C                   Z-ADD     PiVal         VABNSP
038800060404     C                   ELSE
038900070502     C                   SETON                                        31
039000060510     C                   Z-ADD     *zeros        VABNSP
039100060404     C                   EVAL      vinmsg = %trimr(vinmsg)
039200080410     C                             + ' ' + 'VABNSP'
039300060404     C                   ENDIF
039400080410     C* RMN
039500080708     C                   EVAL      PiStr=%trim(%subst(vindta:6:6))
039600080410     C                   EXSR      CHKNUM
039700080410     C                   IF        PiInt=*on
039800080410     C                   Z-ADD     PiVal         VABRMN
039900080410     C                   ELSE
040000080410     C                   SETON                                        31
040100080410     C                   Z-ADD     1             VABRMN
040200080410     C                   EVAL      vinmsg = %trimr(vinmsg)
040300080410     C                             + ' ' + 'VABRMN'
040400080410     C                   ENDIF
040500080408     C* CAD
040600080707     C                   EVAL      PiStr=%trim(%subst(vindta:99:9))
040700080408     C                   EXSR      CHKNUM
040800080408     C                   IF        PiInt=*on
040900080408     C                   Z-ADD     PiVal         Num5_0
041000080408     C                   MOVEL(p)  Num5_0        VABCAD
041100080408     C                   ELSE
041200080408     C                   SETON                                        32
041300080408     C                   EVAL      VABCAD = *zeros
041400080408     C                   EVAL      vinmsg = %trimr(vinmsg)
041500080408     C                             + ' ' + 'VABCAD'
041600080408     C                   ENDIF
041700080408     C* Reperisco la provincia dal CAP e dalla località
041800080408     C                   IF        VABCAD <> *blanks AND
041900080408     C                             VABPRD  = *blanks
042000080408     C                   CLEAR                   TISI95DS
042100080408     C                   EVAL      I95TCN = '3'
042200080408     C                   Z-ADD     datcor        I95DAT
042300080408     C                   EVAL      I95CAP = VABCAD
042400080408     C                   EVAL      I95LOC = VABLOD
042500080408     C                   EVAL      I95NAR = VABNZD
042600080408     C                   CALL      'TISI95R'
042700080408     C                   PARM                    TISI95DS
042800080408     C                   EVAL      VABPRD = O95PRV
042900080408     C                   ENDIF
043000080410     C* NCL
043100080707     C                   EVAL      PiStr=%trim(%subst(vindta:167:6))
043200080410     C                   EXSR      CHKNUM
043300080410     C                   IF        PiInt=*on
043400080410     C                   Z-ADD     PiVal         VABNCL
043500080410     C                   ELSE
043600080410     C                   SETON                                        32
043700080410     C                   Z-ADD     *zeros        VABNCL
043800080410     C                   EVAL      vinmsg = %trimr(vinmsg)
043900080410     C                             + ' ' + 'VABNCL'
044000080410     C                   ENDIF
044100071029     C* PKB
044200080707     C                   EVAL      PiStr=%trim(%subst(vindta:140:9))
044300010607     C                   EXSR      CHKNUM
044400010607     C                   IF        PiNum=*on
044500080707     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
044600080707     C                   Z-ADD(H)  PiVal         VABPKB
044700010607     C                   ELSE
044800010607     C                   SETON                                        32
044900010607     C                   Z-ADD     *zeros        VABPKB
045000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045100010607     C                             + ' ' + 'VABPKB'
045200010607     C                   ENDIF
045300080707     C* VLB
045400080707     C                   EVAL      PiStr=%trim(%subst(vindta:149:18))
045500080707     C                   EXSR      CHKNUM
045600080707     C                   IF        PiNum=*on
045700080707     C                   EVAL      PiVal = PiVal / 1000000                      * gestisco 6 dec.
045800080707     C                   Z-ADD(H)  PiVal         VABVLB
045900080707     C                   ELSE
046000080707     C                   SETON                                        32
046100080707     C                   Z-ADD     *zeros        VABVLB
046200080707     C                   EVAL      vinmsg = %trimr(vinmsg)
046300080707     C                             + ' ' + 'VABVLB'
046400080707     C                   ENDIF
046500080320     C* CAS
046600080707     C                   IF        %trim(%subst(vindta:232:18))<>*all'0'
046700080408     C                   EVAL      FlgCAS = '1'
046800080408     C                   EVAL      VABVCA = 'EUR'
046900080707     C                   EVAL      PiStr=%trim(%subst(vindta:232:18))
047000080320     C                   EXSR      CHKNUM
047100080320     C                   IF        PiNum=*on
047200080707     C                   EVAL      PiVal = PiVal / 10000                        * gestisco 4 dec.
047300080611     C                   Z-ADD     PiVal         VABCAS
047400080320     C                   ELSE
047500080320     C                   SETON                                        32
047600080320     C                   Z-ADD     *zeros        VABCAS
047700080320     C                   EVAL      vinmsg = %trimr(vinmsg)
047800080320     C                             + ' ' + 'VABCAS'
047900080408     C                   ENDIF
048000080320     C                   ENDIF
048100020322     C*
048200020322     C* Considerazioni finali su CBO/CAS
048300050908     C                   IF        FlgCAS = '1'
048400040713     C                   IF        VABCBO = '1'
048500020322     C                   EVAL      VABCBO = '4'
048600040713     C                   ENDIF
048700040713     C                   IF        VABCBO = '2'
048800040713     C                   EVAL      VABCBO = '6'
048900040713     C                   ENDIF
049000020322     C                   ENDIF
049100020204     C*
049200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049300020204     C                   EXSR      CHKIMPDIV
049400020204     C*
049500010607     C                   ENDSR
049600010607     C*----------------------------------------------------*
049700010601
049800020204
049900020204     C*----------------------------------------------------*
050000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050100020204     C*----------------------------------------------------*
050200020204     C     CHKIMPDIV     BEGSR
050300020204     C*
050400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050500020204     C                   Z-ADD     *zeros        wrkDec            9 9
050600020204     C*
050700020204     C* Come prima cosa effettuo considerazioni sulla divisa
050800020204     C                   IF        vabIAS > *zeros
050900020204     C                   IF        vabVAS <> 'EUR'
051000020204     C                   EVAL      vabVAS =  'ITL'
051100020204     C                   ENDIF
051200020204     C                   ENDIF
051300020204     C*
051400020204     C                   IF        vabCAS > *zeros
051500020204     C                   IF        vabVCA <> 'EUR'
051600020204     C                   EVAL      vabVCA =  'ITL'
051700020204     C                   ENDIF
051800020204     C                   ENDIF
051900020204     C*
052000020204     C                   IF        vabVMD > *zeros
052100020321     C                   IF        vabVAD <> 'EUR'
052200020204     C                   EVAL      vabVAD =  'ITL'
052300020204     C                   ENDIF
052400020204     C                   ENDIF
052500020204     C*
052600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052700020204     C                   Z-ADD     vabIAS        wrkDec
052800020204     C                   IF        wrkDec > *zeros
052900020204     C                   IF        vabVAS = 'ITL'
053000020204     C                   EVAL      vabIAS = *zeros
053100020204     C                   ENDIF
053200020204     C                   ENDIF
053300020204     C*
053400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
053500020204     C                   Z-ADD     vabCAS        wrkDec
053600020204     C                   IF        wrkDec > *zeros
053700020204     C                   IF        vabVCA = 'ITL'
053800020204     C                   EVAL      vabCAS = *zeros
053900020204     C                   ENDIF
054000020204     C                   ENDIF
054100020204     C*
054200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054300020204     C                   Z-ADD     vabVMD        wrkDec
054400020204     C                   IF        wrkDec > *zeros
054500020204     C                   IF        vabVAD = 'ITL'
054600020204     C                   EVAL      vabVMD = *zeros
054700020204     C                   ENDIF
054800020204     C                   ENDIF
054900020204     C*
055000020204     C                   ENDSR
055100020204     C***
055200020204
055300010330
055400010330
055500010330     C*----------------------------------------------------*
055600010330     C*  CONTROLLO NUMERICITA' CAMPI
055700010330     C*----------------------------------------------------*
055800010330     C     CHKNUM        BEGSR
055900010330     C*
056000010606     C                   IF        PiDecChr = *blanks
056100080611     C                   EVAL      PiDecChr = ','
056200010606     C                   ENDIF
056300080611     C*
056400080611     C                   callp     UBISNUM_Check(PiStr
056500080611     C                                          :PiDecChr
056600080611     C                                          :PiVal
056700080611     C                                          :PiNum
056800080611     C                                          :PiInt)
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
063700010601
063800010601
063900010601
064000010601      /TITLE Invio dei dati al punto operativo.
064100010601     C     opeini        BEGSR
064200010601     C*
064300010601     C* Inizializzo flag e contatori operativi
064400010601     C                   movel     '0'           flgGiro           1
064500010601     C                   movel     '0'           flgMulti          1
064600010601     C                   movel     '1'           flgStato          1
064700010615     C                   movel     '0'           flgOk             1
064800010601     C                   z-add     *zeros        cntNonEl         10 0
064900010601     C                   z-add     *zeros        depfil            3 0
065000010601     C                   z-add     *zeros        invfil            3 0
065100010601     C*
065200010601     C                   ENDSR
065300010601     C***
065400010601
065500010601
065600010601
065700010330
065800010330
065900000613     C     *inzsr        BEGSR
066000990910     C*
066100990910     C     *entry        plist
066200990920     C                   parm                    tivlrds
066300990921     C                   parm      wrkesito      esito
066400000724     C                   parm                    prmlit
066500000710     C                   parm                    prmfir
066600010330     C*
066700010330     C* CALCOLA LA DATA CORRENTE
066800010330     C                   time                    wn14             14 0
066900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067000010330     C                   z-add     wn8           g08dat
067100010330     C                   z-add     *zeros        g08inv
067200010330     C                   movel     '0'           g08err
067300010330     C                   call      'XSRDA8'
067400010330     C                   parm                    wlbda8
067500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067600000613     C*
067700000613     C                   ENDSR
067800000613     C***
067900990908
068000080707     Otitv1h7p  E            riepilogo         2
068100990915     O                                              'Upload via Internet'
068200990915     O                                           +1 'Traduzione TIVIN00R -'
068300050408     O                                           +1 'FIVABWWR'
068400990915     O                                           +1 'Testate bolle'
068500990915     O          E            riepilogo   2
068600990915     O                       wrkdata
068700990915     O                       wrkora              +1
068800990915     O                       procname            +1
068900990915     O          E            riepilogo   2
069000990915     O                                              'Cliente..................:'
069100010605     O                       VABCCM        z     +1
069200990915     O          E            riepilogo   2
069300990920     O                                              'Riferimento Strategi.....:'
069400990920     O                       vlrhdl              +1
069500990915     O          E            riepilogo   2
069600990915     O                                              'Giusti...................:'
069700971022     O                       §CTROK        2   +  1
069800990915     O          E            riepilogo   2
069900990915     O                                              'Sbagliati e corretti.....:'
070000971022     O                       §CTRMO        2   +  1
070100990915     O          E            riepilogo   2
070200990915     O                                              'Sbagliati e scartati.....:'
070300971022     O                       §CTRNO        2   +  1
070400000613
070500080707     Otitv1h7ps E            testdett          2
070600000613     O                                              'Upload via Internet'
070700000613     O                                           +1 'Traduzione TIVIN00R -'
070800050408     O                                           +1 'FIVABWWR'
070900000613     O                                           +1 'Testate bolle'
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
072200010605     O                       VABCCM        z     +1
072300000616     O          E            findett     2
072400000613     O                                              'Riferimento Strategi.....:'
072500000613     O                       vlrhdl              +1
072600000616     O          E            findett     2
072700000613     O                                              'Giusti...................:'
072800000613     O                       §CTROK        2   +  1
072900000616     O          E            findett     2
073000000613     O                                              'Sbagliati e corretti.....:'
073100000613     O                       §CTRMO        2   +  1
073200000616     O          E            findett     2
073300000613     O                                              'Sbagliati e scartati.....:'
073400000613     O                       §CTRNO        2   +  1
073500000613** CMD - COMANDI CL
073600080707OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1H7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
073700080707OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1H7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
073800080707DLTOVR FILE(TITV1H7P TITV1H7PS) LVL(*)
073900010606
074000000613
