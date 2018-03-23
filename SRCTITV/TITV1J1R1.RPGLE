000100081016      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200080611     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600081016     FEDIVABWR  O    E             DISK    usropn
000700081007     Ftitv1j1p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900081007     Ftitv1j1ps O    f  198        PRINTER usropn
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
003300081029     D wNomeFile       s             30    INZ(*blanks)
003400010330     D*------------------
003500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600010330     D*------------------
003700010330     D WLBDA8          DS                  INZ
003800010330     D  G08DAT                 1      8  0
003900010330     D  G08INV                 9     16  0
004000010330     D  G08ERR                17     17
004100010330     D  G08TGI                18     22  0
004200081007     D*------------------
004300081007     D* Costanti
004400081007     D*------------------
004500081007     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004600081007     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004700080611     D*------------------
004800080611     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
004900080611     D*------------------
005000080611     D PiStr           S            100A   INZ
005100080611     D PiDecChr        S              1A   INZ
005200080611     D PiVal           S             63S30 INZ
005300080611     D PiInt           S               N   INZ
005400080611     D PiNum           S               N   INZ
005500080611
005600080611     D*------------------
005700080611     D* LINKING A DEFINIZIONI ESTERNE
005800080611     D*------------------
005900080611     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006000010330
006100010330
006200990915     C                   time                    wrkdata
006300990915     C                   time                    wrkora
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700010601     C*
006800010601     C                   exsr      opeini
006900010605     C                   exsr      rwvab
007000070502     C*
007100070502     C* Effettuo lancio TISI95 solo x chiusura
007200070502     C                   CLEAR                   TISI95DS
007300070502     C                   EVAL      I95TLA = 'C'
007400070502     C                   CALL      'TISI95R'
007500070502     C                   PARM                    TISI95DS
007600081007     C*
007700081007     C                   exsr      endela
007800010601     C*
007900010601     C                   seton                                        lr
008000010601
008100010601
008200010601
008300010601
008400010601     C*--------------------------------------------------------
008500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008600010601     C*--------------------------------------------------------
008700010601     C     PREELA        BEGSR
008800010601     C*
008900010601     C* SE OCCORRE SPEDIRE IN FILIALE
009000010601     C                   if        invfil <> *zeros and
009100010601     C                             flgGiro = '0'
009200010601     C*
009300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009400010601     C                   eval      flgGiro = '1'
009500000724     C*
009600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009800010601     C     invfil        chain     azorg01l
009900000724     C                   if        %found
010000000724     C                   movel(p)  CMD(1)        depcmd
010100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010200000724     C*
010300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010400011113     C                   Z-ADD     150           LENGH            15 5
010500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010600000724     C                   PARM                    depcmd
010700000724     C                   PARM                    LENGH
010800000724     C*
010900000724     C                   endif
011000000616     C*
011100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011200000616     C                   movel(p)  CMD(2)        depcmd
011300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011400000616     C*
011500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011600011113     C                   Z-ADD     150           LENGH            15 5
011700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011800000616     C                   PARM                    depcmd
011900000616     C                   PARM                    LENGH
012000000613     C*
012100081007     C                   if        not %open(titv1j1ps)
012200081007     C                   open      titv1j1ps
012300000616     C                   except    testdett
012400000613     C                   endif
012500010601     C*
012600010601     C                   endif
012700010601     C*
012800010601     C                   ENDSR
012900010601     C***
013000010601
013100010601
013200010601
013300010601     C*--------------------------------------------------------
013400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013500010601     C*--------------------------------------------------------
013600010601     C     ENDELA        BEGSR
013700000613     C*
013800081007     C                   if        %open(titv1j1ps)
013900000616     C                   except    findett
014000081007     C                   close     titv1j1ps
014100000613     C                   endif
014200081007     C*
014300081007     C                   if        %open(tivin00r)
014400081007     C                   close     tivin00r
014500081007     C                   endif
014600081016     C                   if        %open(edivabwr)
014700081016     C                   close     edivabwr
014800081007     C                   endif
014900000616     C*
015000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015100011113     C                   Z-ADD     150           LENGH            15 5
015200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015300000616     C                   PARM                    CMD(3)
015400000616     C                   PARM                    LENGH
015500000616     C*
015600010601     C                   ENDSR
015700010601     C***
015800010601
015900010601
016000000613
016100000613     C*--------------------------------------------------------
016200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016300000613     C*--------------------------------------------------------
016400000621     C     STPR          BEGSR
016500000613     C*
016600081007     C                   if        not %open(titv1j1p)
016700081007     C                   open      titv1j1p
016800990915     C                   endif
016900990915     C*
017000990915     C                   except    riepilogo
017100990915     C*
017200081007     C                   if        %open(titv1j1p)
017300081007     C                   close     titv1j1p
017400990914     C                   endif
017500000613     C*
017600000613     C                   ENDSR
017700000613     C***
017800000613
017900010601
018000010601
018100010330     C*--------------------------------------------------------
018200081016     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
018300010330     C*--------------------------------------------------------
018400010605     C     RWVAB         BEGSR
018500050408     C*
018600010330     C                   if        not %open(tivin00r)
018700010330     C                   open      tivin00r
018800010330     C                   endif
018900081016     C                   if        not %open(edivabwr)
019000081016     C                   open      edivabwr
019100010330     C                   endif
019200010330     C*
019300010604     C                   clear                   §CTROK
019400010604     C                   clear                   §CTRMO
019500010604     C                   clear                   §CTRNO
019600010330     C*
019700010330     C                   DO        *HIVAL
019800010330     C*
019900010330     C                   READ      tivin00r                               70
020000010618     C*
020100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020200010618     C                   if        vinflg = '1'
020300010618     C                   eval      flgOk = '1'
020400010618     C                   endif
020500010618     C*
020600010330     C                   if        vindta > *blanks
020700010330     C                   add       1             rrnum
020800010330     C*
020900010601     C                   if        *in70 = *off and
021000010330     C                             (vinflg = *blanks
021100010330     C                              or vinflg = '0'
021200010330     C                              or vinflg = '2')
021300010330     C*
021400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021500010711     C                   if        vinflg = *blanks or vinflg = '0'
021600010711     C                   clear                   vinmsg
021700010711     C                   endif
021800070601     C*
021900070601     C                   exsr      inzvar
022000070601     C                   exsr      defcam
022100070601     C                   exsr      impvab
022200010601     C*
022300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022400010605     C                   eval      depfil = VABLNP
022500010601     C                   exsr      repfil
022600010601     C                   if        depfil = invfil
022700021025     C                   if        vlrpoi = 999
022800021025     C                   MOVE(P)   invfil        VABFGS
022900021025     C                   else
023000021025     C                   MOVE(P)   vlrpoi        VABFGS
023100021025     C                   endif
023200010601     C*
023300010601     C                   exsr      PREELA
023400010601     C*
023500010604     C* Ebbene...
023600010604     C*
023700010604     C  N31              ADD       1             §CTROK            7 0
023800010604     C   32              ADD       1             §CTRMO            7 0
023900010604     C   31              ADD       1             §CTRNO            7 0
024000071003     C                   exsr      wrivab                                       => scrivo VAB
024100010604     C*
024200020722     C                   endif
024300020722     C*
024400010604     C                   if        *in31 = *off and
024500010604     C                             *in32 = *off
024600010604     C                   eval      vinflg = '1'
024700010604     C                   else
024800010604     C                   eval      recko = vindta
024900010604     C                   except    rigadett
025000010604     C                   eval      vinflg = '2'
025100010604     C                   endif
025200010604     C                   endif
025300010604     C*
025400010330     C                   else
025500010330     C                   eval      vinflg = '1'
025600010330     C                   endif
025700010601     C*
025800010601     C  N70              update    tivin000
025900010330     C*
026000010330     C  N70              ENDdo
026100010601     C*
026200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026300010601     C                   if        cntNonEl = *zeros or
026400010601     C                             flgMulti = '0'
026500010330     C* Se non ci sono record con errori ...
026600010601     C                   if        §ctrno = 0 and
026700010604     C                             §ctrmo = 0 and
026800010601     C                             flgStato <> '2'
026900010330     C* ... restituisco esito OK.
027000010330     C                   eval      wrkesito = '0'
027100010330     C                   else
027200010330     C                   if        §ctrok > 0
027300010330     C                   eval      wrkesito = '1'
027400010330     C                   else
027500010615     C                   if        flgOk = '0'
027600010615     C                   eval      wrkesito = '2'
027700010615     C                   else
027800010615     C                   eval      wrkesito = '6'
027900010615     C                   endif
028000010330     C                   endif
028100010330     C                   endif
028200010601     C                   else
028300010601     C                   eval      wrkesito = '9'
028400010601     C                   endif
028500010601     C*
028600010601     C                   if        vlrpoi <> 999
028700010601     C                   eval      invfil = vlrpoi
028800010601     C                   endif
028900010330     C*
029000010330     C                   if        §ctrok > 0
029100010601     C                             and invfil > *zeros
029200010330     C                   exsr      invio
029300010330     C                   endif
029400010601     C*
029500010618     C                   if        flgGiro = '1'
029600081007     C                   exsr      STPR                                         OP.DI STAMPA RIEPIL.
029700010601     C                   exsr      endela
029800010618     C                   endif
029900010330     C*
030000010330     C                   ENDSR
030100010330     C***
030200070601
030300070601     C*----------------------------------------------------*
030400070601     C*  SCARICAMENTO BUFFER RECORDS VAB
030500070601     C*----------------------------------------------------*
030600070601     C     WRIVAB        BEGSR
030700081016     C*
030800081016     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
030900090310     C                   eval      VABCMR = %subst(wNomeFile:10:05)
031000081016     C                   EVAL      VABDCM = DATCOR
031100081016     C                   EVAL      VABDTS = DATCOR
031200081016     C                   EVAL      VABHMS = ORACOR
031300081016     C                   EVAL      VABCNT = 1
031400080321     C*
031500081016     C  N31              WRITE     EDIVAB00
031600070601     C*
031700070601     C                   ENDSR
031800010601
031900010601
032000010601
032100010330     C*----------------------------------------------------*
032200020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032300010330     C*----------------------------------------------------*
032400010330     C     INZVAR        BEGSR
032500010330     C*
032600020204     C                   Z-ADD     *zeros        Num5_0            5 0
032700020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032800020322     C                   MOVEL     '0'           FlgCAS            1
032900081007     C                   MOVEL     *blanks       wNOTE            70
033000010330     C*
033100010330     C                   ENDSR
033200010330     C*----------------------------------------------------*
033300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
033400010330     C*----------------------------------------------------*
033500010330     C     DEFCAM        BEGSR
033600010330     C*
033700020204     C* Inizializzo il buffer del record da scrivere
033800081016     C                   CLEAR                   EDIVAB00
033900020204     C* Imposto i valori di default...
034000081007     C                   EVAL      VABCCM = 2220929
034100081007     C                   EVAL      VABLNP = 222
034200081007     C                   EVAL      VABCTR = 001
034300081007     C                   EVAL      VABTSP = 'C'
034400070208     C                   EVAL      VABCBO = '1'
034500020204     C* ... e poi verifico se sono stati passati come parametri
034600020204     C                   IF        vlrppt > *blanks
034700040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
034800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034900020204     C                   EXSR      CHKNUM
035000020204     C                   IF        PiInt=*on
035100020204     C                   Z-ADD     PiVal         VABCCM
035200020204     C                   ENDIF
035300040301     C                   ENDIF
035400040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
035500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035600020204     C                   EXSR      CHKNUM
035700020204     C                   IF        PiInt=*on
035800020204     C                   Z-ADD     PiVal         VABLNP
035900020204     C                   ENDIF
036000040301     C                   ENDIF
036100040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
036200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036300020204     C                   EXSR      CHKNUM
036400020204     C                   IF        PiInt=*on
036500020204     C                   Z-ADD     PiVal         VABCTR
036600040301     C                   ENDIF
036700020204     C                   ENDIF
036800081007     C                   IF        %trim(%subst(vlrppt:14:2)) <> *blanks
036900081007     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
037000081007     C                   ENDIF
037100020204     C                   ENDIF
037200020204     C*
037300010330     C                   ENDSR
037400010607     C*----------------------------------------------------*
037500081016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
037600010607     C*----------------------------------------------------*
037700010607     C     IMPVAB        BEGSR
037800010607     C*
037900010607     C                   SETOFF                                       3132
038000081007     C*
038100081007     C* Porto tutto il buffer d input in maiuscolo
038200081007     C     minu:maiu     XLATE     vindta        vindta
038300010607     C*
038400010607     C* Reperimento campi ALFA
038500081007     C                   EVAL      VABRSD=%trim(%subst(vindta:74:35))
038600100531     C                   EVAL      VABIND=%trim(%subst(vindta:109:40))+' '+
038700100531     C                                    %trim(%subst(vindta:624:40))
038800081007     C                   EVAL      VABLOD=%trim(%subst(vindta:149:40))
038900081007     C                   EVAL      VABPRD=%trim(%subst(vindta:199:2))
039000081029     C***                EVAL      VABRMA=%trim(%subst(vindta:20:10))
039100081029     C                   EVAL      VABRMA=%trim(%subst(vindta:38:10))
039200081007     C                   IF        %trim(%subst(vindta:598:4)) = 'ZESP'
039300081007     C                   EVAL      VABTSP='E'
039400081007     C                   ENDIF
039500081007     C                   IF        %trim(%subst(vindta:235:1)) = 'F'
039600081007     C                   EVAL      VABCBO='1'
039700081007     C                   ENDIF
039800081007     C                   IF        %trim(%subst(vindta:235:1)) = 'A'
039900081007     C                   EVAL      VABCBO='2'
040000081007     C                   ENDIF
040100081007     C                   IF        %trim(%subst(vindta:225:10)) = 'TASSATIVA'
040200081007     C                   EVAL      VABTCR=*blanks
040300081007     C                   ENDIF
040400081007     C                   IF        %trim(%subst(vindta:225:10)) = 'ENTRO IL'
040500081007     C                   EVAL      VABTCR='P'
040600081007     C                   ENDIF
040700081007     C                   IF        %trim(%subst(vindta:225:10)) = 'DOPO IL'
040800081007     C                   EVAL      VABTCR='D'
040900081007     C                   ENDIF
041000081007     C                   EVAL      wNOTE=%trim(%subst(vindta:247:75))+
041100081007     C                                   %trim(%subst(vindta:322:75))+
041200081007     C                                   %trim(%subst(vindta:397:75))+
041300081007     C                                   %trim(%subst(vindta:472:75))
041400081007     C                   EVAL      VABNOT=%subst(wNOTE:01:35)
041500081007     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
041600080408     C*
041700080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
041800080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041900080408     C     '@':'A'       XLATE     VABRSD        VABRSD
042000080408     C* ==
042100060510     C*
042200060510     C* Reperimento campi NUMERICI
042300060510     C                   MOVEL     DATCOR        VABAAS
042400060510     C                   MOVE      DATCOR        VABMGS
042500080410     C* NSP
042600081009     C                   EVAL      PiStr=%trim(%subst(vindta:20:10))
042700060510     C                   EXSR      CHKNUM
042800060510     C                   IF        PiInt=*on
042900060510     C                   Z-ADD     PiVal         VABNSP
043000060404     C                   ELSE
043100070502     C                   SETON                                        31
043200060510     C                   Z-ADD     *zeros        VABNSP
043300060404     C                   EVAL      vinmsg = %trimr(vinmsg)
043400080410     C                             + ' ' + 'VABNSP'
043500060404     C                   ENDIF
043600080410     C* RMN
043700081029     C***                EVAL      PiStr=%trim(%subst(vindta:38:10))
043800081029     C                   EVAL      PiStr=%trim(%subst(vindta:20:10))
043900080410     C                   EXSR      CHKNUM
044000080410     C                   IF        PiInt=*on
044100080410     C                   Z-ADD     PiVal         VABRMN
044200080410     C                   ELSE
044300080410     C                   SETON                                        31
044400080410     C                   Z-ADD     1             VABRMN
044500080410     C                   EVAL      vinmsg = %trimr(vinmsg)
044600080410     C                             + ' ' + 'VABRMN'
044700080410     C                   ENDIF
044800080408     C* CAD
044900081007     C                   EVAL      PiStr=%trim(%subst(vindta:189:10))
045000080408     C                   EXSR      CHKNUM
045100080408     C                   IF        PiInt=*on
045200080408     C                   Z-ADD     PiVal         Num5_0
045300080408     C                   MOVEL(p)  Num5_0        VABCAD
045400080408     C                   ELSE
045500080408     C                   SETON                                        32
045600080408     C                   EVAL      VABCAD = *zeros
045700080408     C                   EVAL      vinmsg = %trimr(vinmsg)
045800080408     C                             + ' ' + 'VABCAD'
045900080408     C                   ENDIF
046000080408     C* Reperisco la provincia dal CAP e dalla località
046100080408     C                   IF        VABCAD <> *blanks AND
046200080408     C                             VABPRD  = *blanks
046300080408     C                   CLEAR                   TISI95DS
046400080408     C                   EVAL      I95TCN = '3'
046500080408     C                   Z-ADD     datcor        I95DAT
046600080408     C                   EVAL      I95CAP = VABCAD
046700080408     C                   EVAL      I95LOC = VABLOD
046800080408     C                   EVAL      I95NAR = VABNZD
046900080408     C                   CALL      'TISI95R'
047000080408     C                   PARM                    TISI95DS
047100080408     C                   EVAL      VABPRD = O95PRV
047200080408     C                   ENDIF
047300081007     C* DCR
047400081007     C                   EVAL      PiStr=%trim(%subst(vindta:217:8))
047500081007     C                   EXSR      CHKNUM
047600081007     C                   IF        PiInt=*on
047700081007     C                   Z-ADD     PiVal         VABDCR
047800081007     C                   ELSE
047900081007     C                   SETON                                        32
048000081007     C                   Z-ADD     *zeros        VABDCR
048100081007     C                   EVAL      vinmsg = %trimr(vinmsg)
048200081007     C                             + ' ' + 'VABDCR'
048300081007     C                   ENDIF
048400080410     C* NCL
048500081007     C                   Z-ADD     *zeros        wNCL_01           5 0
048600081007     C                   Z-ADD     *zeros        wNCL_02           5 0
048700081007     C                   Z-ADD     *zeros        wNCL_03           5 0
048800081007     C                   Z-ADD     *zeros        wNCL_04           5 0
048900081007     C*
049000081007     C                   EVAL      PiStr=%trim(%subst(vindta:547:6))
049100080410     C                   EXSR      CHKNUM
049200080410     C                   IF        PiInt=*on
049300081007     C                   Z-ADD     PiVal         wNCL_01
049400080410     C                   ELSE
049500080410     C                   SETON                                        32
049600081007     C                   Z-ADD     *zeros        wNCL_01
049700080410     C                   EVAL      vinmsg = %trimr(vinmsg)
049800080410     C                             + ' ' + 'VABNCL'
049900080410     C                   ENDIF
050000081007     C*
050100081007     C                   EVAL      PiStr=%trim(%subst(vindta:553:6))
050200081007     C                   EXSR      CHKNUM
050300081007     C                   IF        PiInt=*on
050400081007     C                   Z-ADD     PiVal         wNCL_02
050500081007     C                   ELSE
050600081007     C                   SETON                                        32
050700081007     C                   Z-ADD     *zeros        wNCL_02
050800081007     C                   EVAL      vinmsg = %trimr(vinmsg)
050900081007     C                             + ' ' + 'VABNCL'
051000081007     C                   ENDIF
051100081007     C*
051200081007     C                   EVAL      PiStr=%trim(%subst(vindta:559:6))
051300081007     C                   EXSR      CHKNUM
051400081007     C                   IF        PiInt=*on
051500081007     C                   Z-ADD     PiVal         wNCL_03
051600081007     C                   ELSE
051700081007     C                   SETON                                        32
051800081007     C                   Z-ADD     *zeros        wNCL_03
051900081007     C                   EVAL      vinmsg = %trimr(vinmsg)
052000081007     C                             + ' ' + 'VABNCL'
052100081007     C                   ENDIF
052200081007     C*
052300081007     C                   EVAL      PiStr=%trim(%subst(vindta:565:6))
052400081007     C                   EXSR      CHKNUM
052500081007     C                   IF        PiInt=*on
052600081007     C                   Z-ADD     PiVal         wNCL_04
052700081007     C                   ELSE
052800081007     C                   SETON                                        32
052900081007     C                   Z-ADD     *zeros        wNCL_04
053000081007     C                   EVAL      vinmsg = %trimr(vinmsg)
053100081007     C                             + ' ' + 'VABNCL'
053200081007     C                   ENDIF
053300081007     C*
053400081007     C                   EVAL      VABNCL = wNCL_01+wNCL_02+wNCL_03+wNCL_04
053500081007     C* PKB
053600081007     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
053700010607     C                   EXSR      CHKNUM
053800010607     C                   IF        PiNum=*on
053900081007     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
054000080903     C                   Z-ADD(H)  PiVal         VABPKB
054100010607     C                   ELSE
054200010607     C                   SETON                                        32
054300010607     C                   Z-ADD     *zeros        VABPKB
054400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
054500010607     C                             + ' ' + 'VABPKB'
054600010607     C                   ENDIF
054700080320     C* CAS
054800081007     C                   IF        %trim(%subst(vindta:236:11))<>*all'0' AND
054900081007     C                             %trim(%subst(vindta:236:11))<>*blanks
055000080408     C                   EVAL      FlgCAS = '1'
055100080903     C                   EVAL      VABVCA='EUR'
055200081007     C                   EVAL      PiStr=%trim(%subst(vindta:236:11))
055300080320     C                   EXSR      CHKNUM
055400080320     C                   IF        PiNum=*on
055500080903     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
055600080903     C                   Z-ADD(H)  PiVal         VABCAS
055700080320     C                   ELSE
055800080320     C                   SETON                                        32
055900080320     C                   Z-ADD     *zeros        VABCAS
056000080320     C                   EVAL      vinmsg = %trimr(vinmsg)
056100080320     C                             + ' ' + 'VABCAS'
056200080408     C                   ENDIF
056300080320     C                   ENDIF
056400020322     C*
056500020322     C* Considerazioni finali su CBO/CAS
056600050908     C                   IF        FlgCAS = '1'
056700040713     C                   IF        VABCBO = '1'
056800020322     C                   EVAL      VABCBO = '4'
056900040713     C                   ENDIF
057000040713     C                   IF        VABCBO = '2'
057100040713     C                   EVAL      VABCBO = '6'
057200040713     C                   ENDIF
057300020322     C                   ENDIF
057400020204     C*
057500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
057600020204     C                   EXSR      CHKIMPDIV
057700020204     C*
057800010607     C                   ENDSR
057900010607     C*----------------------------------------------------*
058000010601
058100020204
058200020204     C*----------------------------------------------------*
058300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058400020204     C*----------------------------------------------------*
058500020204     C     CHKIMPDIV     BEGSR
058600020204     C*
058700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058800020204     C                   Z-ADD     *zeros        wrkDec            9 9
058900020204     C*
059000020204     C* Come prima cosa effettuo considerazioni sulla divisa
059100020204     C                   IF        vabIAS > *zeros
059200020204     C                   IF        vabVAS <> 'EUR'
059300020204     C                   EVAL      vabVAS =  'ITL'
059400020204     C                   ENDIF
059500020204     C                   ENDIF
059600020204     C*
059700020204     C                   IF        vabCAS > *zeros
059800020204     C                   IF        vabVCA <> 'EUR'
059900020204     C                   EVAL      vabVCA =  'ITL'
060000020204     C                   ENDIF
060100020204     C                   ENDIF
060200020204     C*
060300020204     C                   IF        vabVMD > *zeros
060400020321     C                   IF        vabVAD <> 'EUR'
060500020204     C                   EVAL      vabVAD =  'ITL'
060600020204     C                   ENDIF
060700020204     C                   ENDIF
060800020204     C*
060900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061000020204     C                   Z-ADD     vabIAS        wrkDec
061100020204     C                   IF        wrkDec > *zeros
061200020204     C                   IF        vabVAS = 'ITL'
061300020204     C                   EVAL      vabIAS = *zeros
061400020204     C                   ENDIF
061500020204     C                   ENDIF
061600020204     C*
061700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
061800020204     C                   Z-ADD     vabCAS        wrkDec
061900020204     C                   IF        wrkDec > *zeros
062000020204     C                   IF        vabVCA = 'ITL'
062100020204     C                   EVAL      vabCAS = *zeros
062200020204     C                   ENDIF
062300020204     C                   ENDIF
062400020204     C*
062500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062600020204     C                   Z-ADD     vabVMD        wrkDec
062700020204     C                   IF        wrkDec > *zeros
062800020204     C                   IF        vabVAD = 'ITL'
062900020204     C                   EVAL      vabVMD = *zeros
063000020204     C                   ENDIF
063100020204     C                   ENDIF
063200020204     C*
063300020204     C                   ENDSR
063400020204     C***
063500020204
063600010330
063700010330
063800010330     C*----------------------------------------------------*
063900010330     C*  CONTROLLO NUMERICITA' CAMPI
064000010330     C*----------------------------------------------------*
064100010330     C     CHKNUM        BEGSR
064200010330     C*
064300010606     C                   IF        PiDecChr = *blanks
064400080611     C                   EVAL      PiDecChr = ','
064500010606     C                   ENDIF
064600080611     C*
064700080611     C                   callp     UBISNUM_Check(PiStr
064800080611     C                                          :PiDecChr
064900080611     C                                          :PiVal
065000080611     C                                          :PiNum
065100080611     C                                          :PiInt)
065200010330     C*
065300010330     C                   ENDSR
065400010330     C***
065500010330
065600010601
065700010601
065800010601
065900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
066000010601     C     repfil        BEGSR
066100010601     C*
066200010601     C                   if        invfil = *zeros and
066300010601     C                             depfil > *zeros and
066400010629     C                             (vinflg = *blanks or
066500010629     C                              vinflg = *zeros)
066600010601     C
066700010601     C                   eval      invfil = depfil
066800010601     C                   endif
066900010601     C*
067000010601     C                   if        depfil <> invfil and
067100010601     C                             invfil > *zeros
067200010601     C                   eval      flgMulti = '1'
067300010601     C                   if        vinflg = *blanks
067400010601     C                   add       1             cntNonEl
067500010601     C                   endif
067600010601     C                   endif
067700010601     C*
067800010601     C                   if        vinflg = '2'
067900010601     C                   eval      flgStato = '2'
068000010601     C                   endif
068100010601     C*
068200010601     C                   ENDSR
068300010601     C***
068400010601
068500010601
068600010601
068700010330
068800010330
068900010330
069000990920      /TITLE Invio dei dati al punto operativo.
069100000613     C     invio         BEGSR
069200990920     C*
069300990920     C                   reset                   dscmz
069400010601     C                   move      invfil        cmzdst
069500990920     C                   eval      cmzfld = vlrfou
069600990920     C                   eval      cmzmbd = vlrhdl
069700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
069800000710     C                   if        prmfir = *blanks
069900081016     C                   eval      cmzfla = 'EDIVAB0F'
070000081016     C                   eval      cmzmba = 'EDIVAB0F'
070100000710     C                   else
070200000710     C                   eval      cmzfla = prmfir
070300000710     C                   eval      cmzmba = prmfir
070400000710     C                   endif
070500990920     C                   eval      cmznrr = *zeros
070600990920     C                   move      §ctrok        cmznrr
070700021018     C                   eval      cmzlba = vlrfl1
070800990920     C                   call(e)   'TIS711C'
070900990920     C                   parm                    dscmz
071000990921     C                   parm      *blanks       esito
071100990920     C                   if        %error
071200990920     C                             or cmzerr = '1'
071300990921     C                             or esito  = '1'
071400000710     C                   eval      wrkesito = '3'
071500990920     C                   endif
071600990920     C*
071700000613     C                   ENDSR
071800990910
071900010601
072000010601
072100010601
072200010601
072300010601      /TITLE Invio dei dati al punto operativo.
072400010601     C     opeini        BEGSR
072500010601     C*
072600010601     C* Inizializzo flag e contatori operativi
072700010601     C                   movel     '0'           flgGiro           1
072800010601     C                   movel     '0'           flgMulti          1
072900010601     C                   movel     '1'           flgStato          1
073000010615     C                   movel     '0'           flgOk             1
073100010601     C                   z-add     *zeros        cntNonEl         10 0
073200010601     C                   z-add     *zeros        depfil            3 0
073300010601     C                   z-add     *zeros        invfil            3 0
073400010601     C*
073500010601     C                   ENDSR
073600010601     C***
073700010601
073800010601
073900010601
074000010330
074100010330
074200000613     C     *inzsr        BEGSR
074300990910     C*
074400990910     C     *entry        plist
074500990920     C                   parm                    tivlrds
074600990921     C                   parm      wrkesito      esito
074700000724     C                   parm                    prmlit
074800000710     C                   parm                    prmfir
074900081029     C*
075000081029     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
075100081029     C                   move(p)   vlrMSG        wNomeFile
075200010330     C*
075300010330     C* CALCOLA LA DATA CORRENTE
075400010330     C                   time                    wn14             14 0
075500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075600081016     C                   movel     wn14          oracor            6 0          *ORA
075700010330     C                   z-add     wn8           g08dat
075800010330     C                   z-add     *zeros        g08inv
075900010330     C                   movel     '0'           g08err
076000010330     C                   call      'XSRDA8'
076100010330     C                   parm                    wlbda8
076200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
076300000613     C*
076400000613     C                   ENDSR
076500000613     C***
076600990908
076700081007     Otitv1j1p  E            riepilogo         2
076800990915     O                                              'Upload via Internet'
076900990915     O                                           +1 'Traduzione TIVIN00R -'
077000081016     O                                           +1 'EDIVABWR'
077100990915     O                                           +1 'Testate bolle'
077200990915     O          E            riepilogo   2
077300990915     O                       wrkdata
077400990915     O                       wrkora              +1
077500990915     O                       procname            +1
077600990915     O          E            riepilogo   2
077700990915     O                                              'Cliente..................:'
077800010605     O                       VABCCM        z     +1
077900990915     O          E            riepilogo   2
078000990920     O                                              'Riferimento Strategi.....:'
078100990920     O                       vlrhdl              +1
078200990915     O          E            riepilogo   2
078300990915     O                                              'Giusti...................:'
078400971022     O                       §CTROK        2   +  1
078500990915     O          E            riepilogo   2
078600990915     O                                              'Sbagliati e corretti.....:'
078700971022     O                       §CTRMO        2   +  1
078800990915     O          E            riepilogo   2
078900990915     O                                              'Sbagliati e scartati.....:'
079000971022     O                       §CTRNO        2   +  1
079100000613
079200081007     Otitv1j1ps E            testdett          2
079300000613     O                                              'Upload via Internet'
079400000613     O                                           +1 'Traduzione TIVIN00R -'
079500081016     O                                           +1 'EDIVABWR'
079600000613     O                                           +1 'Testate bolle'
079700000616     O          E            testdett    3
079800000613     O                                           +2 'N° rec'
079900000613     O                                           +3 'Anteprima contenuto'
080000000616     O          E            rigadett    2
080100000613     O                       rrnum               +2
080200000621     O                       recko               +3
080300000616     O          E            findett     2
080400000613     O                       wrkdata
080500000613     O                       wrkora              +1
080600000613     O                       procname            +1
080700000616     O          E            findett     2
080800000613     O                                              'Cliente..................:'
080900010605     O                       VABCCM        z     +1
081000000616     O          E            findett     2
081100000613     O                                              'Riferimento Strategi.....:'
081200000613     O                       vlrhdl              +1
081300000616     O          E            findett     2
081400000613     O                                              'Giusti...................:'
081500000613     O                       §CTROK        2   +  1
081600000616     O          E            findett     2
081700000613     O                                              'Sbagliati e corretti.....:'
081800000613     O                       §CTRMO        2   +  1
081900000616     O          E            findett     2
082000000613     O                                              'Sbagliati e scartati.....:'
082100000613     O                       §CTRNO        2   +  1
082200000613** CMD - COMANDI CL
082300081007OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1J1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
082400081007OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1J1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
082500081007DLTOVR FILE(TITV1J1P TITV1J1PS) LVL(*)
082600010606
082700000613
