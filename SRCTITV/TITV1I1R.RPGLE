000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200080611     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700080731     Ftitv1i1p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900080731     Ftitv1i1ps O    f  198        PRINTER usropn
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
011300080731     C                   if        not %open(titv1i1ps)
011400080731     C                   open      titv1i1ps
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
013200080731     C                   if        %open(titv1i1ps)
013300000616     C                   except    findett
013400080731     C                   close     titv1i1ps
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
015300080731     C                   if        not %open(titv1i1p)
015400080731     C                   open      titv1i1p
015500990915     C                   endif
015600990915     C*
015700990915     C                   except    riepilogo
015800990915     C*
015900080731     C                   if        %open(titv1i1p)
016000080731     C                   close     titv1i1p
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
030100080902     C* Condizione particolare su richiesta del cliente =>
030200080902     C* => se colli a zero ignoro il record
030300080902     C                   IF        VABNCL = *zeros
030400080902     C                   ELSE
030500070601     C  N31              WRITE     FIVAB000
030600080902     C                   ENDIF
030700070601     C*
030800070601     C                   ENDSR
030900010601
031000010601
031100010601
031200010330     C*----------------------------------------------------*
031300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031400010330     C*----------------------------------------------------*
031500010330     C     INZVAR        BEGSR
031600010330     C*
031700020204     C                   Z-ADD     *zeros        Num5_0            5 0
031800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031900020322     C                   MOVEL     '0'           FlgCAS            1
032000010330     C*
032100010330     C                   ENDSR
032200010330     C*----------------------------------------------------*
032300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
032400010330     C*----------------------------------------------------*
032500010330     C     DEFCAM        BEGSR
032600010330     C*
032700020204     C* Inizializzo il buffer del record da scrivere
032800050408     C                   CLEAR                   FIVAB000
032900020204     C* Imposto i valori di default...
033000080731     C                   EVAL      VABCCM = 1161419
033100080731     C                   EVAL      VABLNP = 116
033200080731     C                   EVAL      VABCTR = 500
033300070208     C                   EVAL      VABCBO = '1'
033400020204     C* ... e poi verifico se sono stati passati come parametri
033500020204     C                   IF        vlrppt > *blanks
033600040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
033700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033800020204     C                   EXSR      CHKNUM
033900020204     C                   IF        PiInt=*on
034000020204     C                   Z-ADD     PiVal         VABCCM
034100020204     C                   ENDIF
034200040301     C                   ENDIF
034300040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
034400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034500020204     C                   EXSR      CHKNUM
034600020204     C                   IF        PiInt=*on
034700020204     C                   Z-ADD     PiVal         VABLNP
034800020204     C                   ENDIF
034900040301     C                   ENDIF
035000040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
035100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035200020204     C                   EXSR      CHKNUM
035300020204     C                   IF        PiInt=*on
035400020204     C                   Z-ADD     PiVal         VABCTR
035500040301     C                   ENDIF
035600020204     C                   ENDIF
035700020204     C                   ENDIF
035800020204     C*
035900010330     C                   ENDSR
036000010607     C*----------------------------------------------------*
036100050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
036200010607     C*----------------------------------------------------*
036300010607     C     IMPVAB        BEGSR
036400010607     C*
036500010607     C                   SETOFF                                       3132
036600010607     C*
036700010607     C* Reperimento campi ALFA
036800080731     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
036900080731     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
037000080731     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
037100080731     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
037200080731     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
037300080731     C                   IF        VABNZD = 'IT'
037400080731     C                   EVAL      VABNZD=*blanks
037500080731     C                   ENDIF
037600080731     C                   EVAL      VABRMA=%trim(%subst(vindta:477:10))
037700080731     C                   EVAL      VABVCA=%trim(%subst(vindta:227:4))
037800080731     C                   IF        VABVCA=*blanks OR VABVCA='Eur'
037900080731     C                   EVAL      VABVCA='EUR'
038000080731     C                   ENDIF
038100080731     C                   EVAL      VABNOT=%trim(%subst(vindta:427:35))
038200090121     C***                EVAL      VABNT2=%trim(%subst(vindta:427+35:50-35))
038300090121     C                   EVAL      VABNT2='ACCETTARE RESI SOLO SE AUTORIZZATI'
038400080408     C*
038500080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
038600080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
038700080408     C     '@':'A'       XLATE     VABRSD        VABRSD
038800080408     C* ==
038900060510     C*
039000060510     C* Reperimento campi NUMERICI
039100060510     C                   MOVEL     DATCOR        VABAAS
039200060510     C                   MOVE      DATCOR        VABMGS
039300080410     C* NSP
039400080731     C                   EVAL      PiStr=%trim(%subst(vindta:477:10))
039500060510     C                   EXSR      CHKNUM
039600060510     C                   IF        PiInt=*on
039700060510     C                   Z-ADD     PiVal         VABNSP
039800060404     C                   ELSE
039900070502     C                   SETON                                        31
040000060510     C                   Z-ADD     *zeros        VABNSP
040100060404     C                   EVAL      vinmsg = %trimr(vinmsg)
040200080410     C                             + ' ' + 'VABNSP'
040300060404     C                   ENDIF
040400080410     C* RMN
040500080731     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
040600080410     C                   EXSR      CHKNUM
040700080410     C                   IF        PiInt=*on
040800080410     C                   Z-ADD     PiVal         VABRMN
040900080410     C                   ELSE
041000080410     C                   SETON                                        31
041100080410     C                   Z-ADD     1             VABRMN
041200080410     C                   EVAL      vinmsg = %trimr(vinmsg)
041300080410     C                             + ' ' + 'VABRMN'
041400080410     C                   ENDIF
041500080408     C* CAD
041600080731     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
041700080408     C                   EXSR      CHKNUM
041800080408     C                   IF        PiInt=*on
041900080408     C                   Z-ADD     PiVal         Num5_0
042000080408     C                   MOVEL(p)  Num5_0        VABCAD
042100080408     C                   ELSE
042200080408     C                   SETON                                        32
042300080408     C                   EVAL      VABCAD = *zeros
042400080408     C                   EVAL      vinmsg = %trimr(vinmsg)
042500080408     C                             + ' ' + 'VABCAD'
042600080408     C                   ENDIF
042700080408     C* Reperisco la provincia dal CAP e dalla località
042800080408     C                   IF        VABCAD <> *blanks AND
042900080408     C                             VABPRD  = *blanks
043000080408     C                   CLEAR                   TISI95DS
043100080408     C                   EVAL      I95TCN = '3'
043200080408     C                   Z-ADD     datcor        I95DAT
043300080408     C                   EVAL      I95CAP = VABCAD
043400080408     C                   EVAL      I95LOC = VABLOD
043500080408     C                   EVAL      I95NAR = VABNZD
043600080408     C                   CALL      'TISI95R'
043700080408     C                   PARM                    TISI95DS
043800080408     C                   EVAL      VABPRD = O95PRV
043900080408     C                   ENDIF
044000080410     C* NCL
044100080731     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
044200080410     C                   EXSR      CHKNUM
044300080410     C                   IF        PiInt=*on
044400080410     C                   Z-ADD     PiVal         VABNCL
044500080410     C                   ELSE
044600080410     C                   SETON                                        32
044700080410     C                   Z-ADD     *zeros        VABNCL
044800080410     C                   EVAL      vinmsg = %trimr(vinmsg)
044900080410     C                             + ' ' + 'VABNCL'
045000080410     C                   ENDIF
045100071029     C* PKB
045200080731     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
045300010607     C                   EXSR      CHKNUM
045400010607     C                   IF        PiNum=*on
045500080715     C                   Z-ADD     PiVal         VABPKB
045600010607     C                   ELSE
045700010607     C                   SETON                                        32
045800010607     C                   Z-ADD     *zeros        VABPKB
045900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
046000010607     C                             + ' ' + 'VABPKB'
046100010607     C                   ENDIF
046200080902     C* VLB/QFT
046300080731     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
046400080731     C                   EXSR      CHKNUM
046500080731     C                   IF        PiNum=*on
046600080731     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
046700080731     C                   Z-ADD(H)  PiVal         VABVLB
046800080902     C                   Z-ADD(H)  PiVal         VABQFT
046900080731     C                   ELSE
047000080731     C                   SETON                                        32
047100080731     C                   Z-ADD     *zeros        VABVLB
047200080902     C                   Z-ADD     *zeros        VABQFT
047300080731     C                   EVAL      vinmsg = %trimr(vinmsg)
047400080902     C                             + ' ' + 'VABVLB VABQFT'
047500080731     C                   ENDIF
047600080320     C* CAS
047700080731     C                   IF        %trim(%subst(vindta:196:14))<>*all'0' AND
047800080731     C                             %trim(%subst(vindta:196:14))<>*blanks
047900080408     C                   EVAL      FlgCAS = '1'
048000080731     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
048100080320     C                   EXSR      CHKNUM
048200080320     C                   IF        PiNum=*on
048300080611     C                   Z-ADD     PiVal         VABCAS
048400080320     C                   ELSE
048500080320     C                   SETON                                        32
048600080320     C                   Z-ADD     *zeros        VABCAS
048700080320     C                   EVAL      vinmsg = %trimr(vinmsg)
048800080320     C                             + ' ' + 'VABCAS'
048900080408     C                   ENDIF
049000080320     C                   ENDIF
049100080731     C*
049200080731     C* Considerazioni su volume / tariffa
049300080801     C                   IF        VABVLB<0,005
049400080731     C                   EVAL      VABCTR=000
049500080731     C                   ENDIF
049600020322     C*
049700020322     C* Considerazioni finali su CBO/CAS
049800050908     C                   IF        FlgCAS = '1'
049900040713     C                   IF        VABCBO = '1'
050000020322     C                   EVAL      VABCBO = '4'
050100040713     C                   ENDIF
050200040713     C                   IF        VABCBO = '2'
050300040713     C                   EVAL      VABCBO = '6'
050400040713     C                   ENDIF
050500020322     C                   ENDIF
050600020204     C*
050700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050800020204     C                   EXSR      CHKIMPDIV
050900020204     C*
051000010607     C                   ENDSR
051100010607     C*----------------------------------------------------*
051200010601
051300020204
051400020204     C*----------------------------------------------------*
051500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051600020204     C*----------------------------------------------------*
051700020204     C     CHKIMPDIV     BEGSR
051800020204     C*
051900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052000020204     C                   Z-ADD     *zeros        wrkDec            9 9
052100020204     C*
052200020204     C* Come prima cosa effettuo considerazioni sulla divisa
052300020204     C                   IF        vabIAS > *zeros
052400020204     C                   IF        vabVAS <> 'EUR'
052500020204     C                   EVAL      vabVAS =  'ITL'
052600020204     C                   ENDIF
052700020204     C                   ENDIF
052800020204     C*
052900020204     C                   IF        vabCAS > *zeros
053000020204     C                   IF        vabVCA <> 'EUR'
053100020204     C                   EVAL      vabVCA =  'ITL'
053200020204     C                   ENDIF
053300020204     C                   ENDIF
053400020204     C*
053500020204     C                   IF        vabVMD > *zeros
053600020321     C                   IF        vabVAD <> 'EUR'
053700020204     C                   EVAL      vabVAD =  'ITL'
053800020204     C                   ENDIF
053900020204     C                   ENDIF
054000020204     C*
054100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054200020204     C                   Z-ADD     vabIAS        wrkDec
054300020204     C                   IF        wrkDec > *zeros
054400020204     C                   IF        vabVAS = 'ITL'
054500020204     C                   EVAL      vabIAS = *zeros
054600020204     C                   ENDIF
054700020204     C                   ENDIF
054800020204     C*
054900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
055000020204     C                   Z-ADD     vabCAS        wrkDec
055100020204     C                   IF        wrkDec > *zeros
055200020204     C                   IF        vabVCA = 'ITL'
055300020204     C                   EVAL      vabCAS = *zeros
055400020204     C                   ENDIF
055500020204     C                   ENDIF
055600020204     C*
055700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055800020204     C                   Z-ADD     vabVMD        wrkDec
055900020204     C                   IF        wrkDec > *zeros
056000020204     C                   IF        vabVAD = 'ITL'
056100020204     C                   EVAL      vabVMD = *zeros
056200020204     C                   ENDIF
056300020204     C                   ENDIF
056400020204     C*
056500020204     C                   ENDSR
056600020204     C***
056700020204
056800010330
056900010330
057000010330     C*----------------------------------------------------*
057100010330     C*  CONTROLLO NUMERICITA' CAMPI
057200010330     C*----------------------------------------------------*
057300010330     C     CHKNUM        BEGSR
057400010330     C*
057500010606     C                   IF        PiDecChr = *blanks
057600080611     C                   EVAL      PiDecChr = ','
057700010606     C                   ENDIF
057800080611     C*
057900080611     C                   callp     UBISNUM_Check(PiStr
058000080611     C                                          :PiDecChr
058100080611     C                                          :PiVal
058200080611     C                                          :PiNum
058300080611     C                                          :PiInt)
058400010330     C*
058500010330     C                   ENDSR
058600010330     C***
058700010330
058800010601
058900010601
059000010601
059100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
059200010601     C     repfil        BEGSR
059300010601     C*
059400010601     C                   if        invfil = *zeros and
059500010601     C                             depfil > *zeros and
059600010629     C                             (vinflg = *blanks or
059700010629     C                              vinflg = *zeros)
059800010601     C
059900010601     C                   eval      invfil = depfil
060000010601     C                   endif
060100010601     C*
060200010601     C                   if        depfil <> invfil and
060300010601     C                             invfil > *zeros
060400010601     C                   eval      flgMulti = '1'
060500010601     C                   if        vinflg = *blanks
060600010601     C                   add       1             cntNonEl
060700010601     C                   endif
060800010601     C                   endif
060900010601     C*
061000010601     C                   if        vinflg = '2'
061100010601     C                   eval      flgStato = '2'
061200010601     C                   endif
061300010601     C*
061400010601     C                   ENDSR
061500010601     C***
061600010601
061700010601
061800010601
061900010330
062000010330
062100010330
062200990920      /TITLE Invio dei dati al punto operativo.
062300000613     C     invio         BEGSR
062400990920     C*
062500990920     C                   reset                   dscmz
062600010601     C                   move      invfil        cmzdst
062700990920     C                   eval      cmzfld = vlrfou
062800990920     C                   eval      cmzmbd = vlrhdl
062900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
063000000710     C                   if        prmfir = *blanks
063100050408     C                   eval      cmzfla = 'FIVAB00F'
063200050408     C                   eval      cmzmba = 'FIVAB00F'
063300000710     C                   else
063400000710     C                   eval      cmzfla = prmfir
063500000710     C                   eval      cmzmba = prmfir
063600000710     C                   endif
063700990920     C                   eval      cmznrr = *zeros
063800990920     C                   move      §ctrok        cmznrr
063900021018     C                   eval      cmzlba = vlrfl1
064000990920     C                   call(e)   'TIS711C'
064100990920     C                   parm                    dscmz
064200990921     C                   parm      *blanks       esito
064300990920     C                   if        %error
064400990920     C                             or cmzerr = '1'
064500990921     C                             or esito  = '1'
064600000710     C                   eval      wrkesito = '3'
064700990920     C                   endif
064800990920     C*
064900000613     C                   ENDSR
065000990910
065100010601
065200010601
065300010601
065400010601
065500010601      /TITLE Invio dei dati al punto operativo.
065600010601     C     opeini        BEGSR
065700010601     C*
065800010601     C* Inizializzo flag e contatori operativi
065900010601     C                   movel     '0'           flgGiro           1
066000010601     C                   movel     '0'           flgMulti          1
066100010601     C                   movel     '1'           flgStato          1
066200010615     C                   movel     '0'           flgOk             1
066300010601     C                   z-add     *zeros        cntNonEl         10 0
066400010601     C                   z-add     *zeros        depfil            3 0
066500010601     C                   z-add     *zeros        invfil            3 0
066600010601     C*
066700010601     C                   ENDSR
066800010601     C***
066900010601
067000010601
067100010601
067200010330
067300010330
067400000613     C     *inzsr        BEGSR
067500990910     C*
067600990910     C     *entry        plist
067700990920     C                   parm                    tivlrds
067800990921     C                   parm      wrkesito      esito
067900000724     C                   parm                    prmlit
068000000710     C                   parm                    prmfir
068100010330     C*
068200010330     C* CALCOLA LA DATA CORRENTE
068300010330     C                   time                    wn14             14 0
068400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068500010330     C                   z-add     wn8           g08dat
068600010330     C                   z-add     *zeros        g08inv
068700010330     C                   movel     '0'           g08err
068800010330     C                   call      'XSRDA8'
068900010330     C                   parm                    wlbda8
069000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069100000613     C*
069200000613     C                   ENDSR
069300000613     C***
069400990908
069500080731     Otitv1i1p  E            riepilogo         2
069600990915     O                                              'Upload via Internet'
069700990915     O                                           +1 'Traduzione TIVIN00R -'
069800050408     O                                           +1 'FIVABWWR'
069900990915     O                                           +1 'Testate bolle'
070000990915     O          E            riepilogo   2
070100990915     O                       wrkdata
070200990915     O                       wrkora              +1
070300990915     O                       procname            +1
070400990915     O          E            riepilogo   2
070500990915     O                                              'Cliente..................:'
070600010605     O                       VABCCM        z     +1
070700990915     O          E            riepilogo   2
070800990920     O                                              'Riferimento Strategi.....:'
070900990920     O                       vlrhdl              +1
071000990915     O          E            riepilogo   2
071100990915     O                                              'Giusti...................:'
071200971022     O                       §CTROK        2   +  1
071300990915     O          E            riepilogo   2
071400990915     O                                              'Sbagliati e corretti.....:'
071500971022     O                       §CTRMO        2   +  1
071600990915     O          E            riepilogo   2
071700990915     O                                              'Sbagliati e scartati.....:'
071800971022     O                       §CTRNO        2   +  1
071900000613
072000080731     Otitv1i1ps E            testdett          2
072100000613     O                                              'Upload via Internet'
072200000613     O                                           +1 'Traduzione TIVIN00R -'
072300050408     O                                           +1 'FIVABWWR'
072400000613     O                                           +1 'Testate bolle'
072500000616     O          E            testdett    3
072600000613     O                                           +2 'N° rec'
072700000613     O                                           +3 'Anteprima contenuto'
072800000616     O          E            rigadett    2
072900000613     O                       rrnum               +2
073000000621     O                       recko               +3
073100000616     O          E            findett     2
073200000613     O                       wrkdata
073300000613     O                       wrkora              +1
073400000613     O                       procname            +1
073500000616     O          E            findett     2
073600000613     O                                              'Cliente..................:'
073700010605     O                       VABCCM        z     +1
073800000616     O          E            findett     2
073900000613     O                                              'Riferimento Strategi.....:'
074000000613     O                       vlrhdl              +1
074100000616     O          E            findett     2
074200000613     O                                              'Giusti...................:'
074300000613     O                       §CTROK        2   +  1
074400000616     O          E            findett     2
074500000613     O                                              'Sbagliati e corretti.....:'
074600000613     O                       §CTRMO        2   +  1
074700000616     O          E            findett     2
074800000613     O                                              'Sbagliati e scartati.....:'
074900000613     O                       §CTRNO        2   +  1
075000000613** CMD - COMANDI CL
075100080731OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1I1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
075200080731OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1I1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
075300080731DLTOVR FILE(TITV1I1P TITV1I1PS) LVL(*)
075400010606
075500000613
