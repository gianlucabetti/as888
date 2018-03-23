000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200080611     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700090326     Ftitv1o0p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900090326     Ftitv1o0ps O    f  198        PRINTER usropn
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
004100080611
004200080611     D*------------------
004300080611     D* LINKING A DEFINIZIONI ESTERNE
004400080611     D*------------------
004500080611     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004600090326     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004700010330
004800010330
004900990915     C                   time                    wrkdata
005000990915     C                   time                    wrkora
005100000913     C                   reset                   rrnum
005200990921     C                   reset                   esito
005300990921     C                   reset                   wrkesito
005400010601     C*
005500010601     C                   exsr      opeini
005600010605     C                   exsr      rwvab
005700070502     C*
005800070502     C* Effettuo lancio TISI95 solo x chiusura
005900070502     C                   CLEAR                   TISI95DS
006000070502     C                   EVAL      I95TLA = 'C'
006100070502     C                   CALL      'TISI95R'
006200070502     C                   PARM                    TISI95DS
006300010601     C*
006400010601     C                   seton                                        lr
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007100010601     C*--------------------------------------------------------
007200010601     C     PREELA        BEGSR
007300010601     C*
007400010601     C* SE OCCORRE SPEDIRE IN FILIALE
007500010601     C                   if        invfil <> *zeros and
007600010601     C                             flgGiro = '0'
007700010601     C*
007800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007900010601     C                   eval      flgGiro = '1'
008000000724     C*
008100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300010601     C     invfil        chain     azorg01l
008400000724     C                   if        %found
008500000724     C                   movel(p)  CMD(1)        depcmd
008600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008700000724     C*
008800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008900011113     C                   Z-ADD     150           LENGH            15 5
009000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000724     C                   PARM                    depcmd
009200000724     C                   PARM                    LENGH
009300000724     C*
009400000724     C                   endif
009500000616     C*
009600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009700000616     C                   movel(p)  CMD(2)        depcmd
009800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009900000616     C*
010000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010100011113     C                   Z-ADD     150           LENGH            15 5
010200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010300000616     C                   PARM                    depcmd
010400000616     C                   PARM                    LENGH
010500000613     C*
010600090326     C                   if        not %open(titv1o0ps)
010700090326     C                   open      titv1o0ps
010800000616     C                   except    testdett
010900000613     C                   endif
011000010601     C*
011100010601     C                   endif
011200010601     C*
011300010601     C                   ENDSR
011400010601     C***
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012000010601     C*--------------------------------------------------------
012100010601     C     ENDELA        BEGSR
012200990915     C*
012300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012400000613     C*
012500090326     C                   if        %open(titv1o0ps)
012600000616     C                   except    findett
012700090326     C                   close     titv1o0ps
012800000613     C                   endif
012900000616     C*
013000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    CMD(3)
013400000616     C                   PARM                    LENGH
013500000616     C*
013600010601     C                   ENDSR
013700010601     C***
013800010601
013900010601
014000000613
014100000613     C*--------------------------------------------------------
014200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014300000613     C*--------------------------------------------------------
014400000621     C     STPR          BEGSR
014500000613     C*
014600090326     C                   if        not %open(titv1o0p)
014700090326     C                   open      titv1o0p
014800990915     C                   endif
014900990915     C*
015000990915     C                   except    riepilogo
015100990915     C*
015200090326     C                   if        %open(titv1o0p)
015300090326     C                   close     titv1o0p
015400990914     C                   endif
015500000613     C*
015600000613     C                   ENDSR
015700000613     C***
015800000613
015900010601
016000010601
016100010330     C*--------------------------------------------------------
016200050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
016300010330     C*--------------------------------------------------------
016400010605     C     RWVAB         BEGSR
016500050408     C*
016600010330     C                   if        not %open(tivin00r)
016700010330     C                   open      tivin00r
016800010330     C                   endif
016900050408     C                   if        not %open(fivabwwr)
017000050408     C                   open      fivabwwr
017100010330     C                   endif
017200010330     C*
017300010604     C                   clear                   §CTROK
017400010604     C                   clear                   §CTRMO
017500010604     C                   clear                   §CTRNO
017600010330     C*
017700010330     C                   DO        *HIVAL
017800010330     C*
017900010330     C                   READ      tivin00r                               70
018000010618     C*
018100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018200010618     C                   if        vinflg = '1'
018300010618     C                   eval      flgOk = '1'
018400010618     C                   endif
018500010618     C*
018600010330     C                   if        vindta > *blanks
018700010330     C                   add       1             rrnum
018800010330     C*
018900010601     C                   if        *in70 = *off and
019000010330     C                             (vinflg = *blanks
019100010330     C                              or vinflg = '0'
019200010330     C                              or vinflg = '2')
019300010330     C*
019400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019500010711     C                   if        vinflg = *blanks or vinflg = '0'
019600010711     C                   clear                   vinmsg
019700010711     C                   endif
019800070601     C*
019900070601     C                   exsr      inzvar
020000070601     C                   exsr      defcam
020100070601     C                   exsr      impvab
020200010601     C*
020300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020400010605     C                   eval      depfil = VABLNP
020500010601     C                   exsr      repfil
020600010601     C                   if        depfil = invfil
020700021025     C                   if        vlrpoi = 999
020800021025     C                   MOVE(P)   invfil        VABFGS
020900021025     C                   else
021000021025     C                   MOVE(P)   vlrpoi        VABFGS
021100021025     C                   endif
021200010601     C*
021300010601     C                   exsr      PREELA
021400010601     C*
021500010604     C* Ebbene...
021600010604     C*
021700010604     C  N31              ADD       1             §CTROK            7 0
021800010604     C   32              ADD       1             §CTRMO            7 0
021900010604     C   31              ADD       1             §CTRNO            7 0
022000071003     C                   exsr      wrivab                                       => scrivo VAB
022100010604     C*
022200020722     C                   endif
022300020722     C*
022400010604     C                   if        *in31 = *off and
022500010604     C                             *in32 = *off
022600010604     C                   eval      vinflg = '1'
022700010604     C                   else
022800010604     C                   eval      recko = vindta
022900010604     C                   except    rigadett
023000010604     C                   eval      vinflg = '2'
023100010604     C                   endif
023200010604     C                   endif
023300010604     C*
023400010330     C                   else
023500010330     C                   eval      vinflg = '1'
023600010330     C                   endif
023700010601     C*
023800010601     C  N70              update    tivin000
023900010330     C*
024000010330     C  N70              ENDdo
024100010601     C*
024200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024300010601     C                   if        cntNonEl = *zeros or
024400010601     C                             flgMulti = '0'
024500010330     C* Se non ci sono record con errori ...
024600010601     C                   if        §ctrno = 0 and
024700010604     C                             §ctrmo = 0 and
024800010601     C                             flgStato <> '2'
024900010330     C* ... restituisco esito OK.
025000010330     C                   eval      wrkesito = '0'
025100010330     C                   else
025200010330     C                   if        §ctrok > 0
025300010330     C                   eval      wrkesito = '1'
025400010330     C                   else
025500010615     C                   if        flgOk = '0'
025600010615     C                   eval      wrkesito = '2'
025700010615     C                   else
025800010615     C                   eval      wrkesito = '6'
025900010615     C                   endif
026000010330     C                   endif
026100010330     C                   endif
026200010601     C                   else
026300010601     C                   eval      wrkesito = '9'
026400010601     C                   endif
026500010330     C*
026600010330     C                   if        %open(tivin00r)
026700010330     C                   close     tivin00r
026800010330     C                   endif
026900050408     C                   if        %open(fivabwwr)
027000050408     C                   close     fivabwwr
027100010330     C                   endif
027200010601     C*
027300010601     C                   if        vlrpoi <> 999
027400010601     C                   eval      invfil = vlrpoi
027500010601     C                   endif
027600010330     C*
027700010330     C                   if        §ctrok > 0
027800010601     C                             and invfil > *zeros
027900010330     C                   exsr      invio
028000010330     C                   endif
028100010601     C*
028200010618     C                   if        flgGiro = '1'
028300010601     C                   exsr      endela
028400010618     C                   endif
028500010330     C*
028600010330     C                   ENDSR
028700010330     C***
028800070601
028900070601     C*----------------------------------------------------*
029000070601     C*  SCARICAMENTO BUFFER RECORDS VAB
029100070601     C*----------------------------------------------------*
029200070601     C     WRIVAB        BEGSR
029300080321     C*
029400070601     C  N31              WRITE     FIVAB000
029500070601     C*
029600070601     C                   ENDSR
029700010601
029800010601
029900010601
030000010330     C*----------------------------------------------------*
030100020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030200010330     C*----------------------------------------------------*
030300010330     C     INZVAR        BEGSR
030400010330     C*
030500020204     C                   Z-ADD     *zeros        Num5_0            5 0
030600020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030700020322     C                   MOVEL     '0'           FlgCAS            1
030800010330     C*
030900010330     C                   ENDSR
031000010330     C*----------------------------------------------------*
031100020722     C*  IMPOSTAZIONE CAMPI COSTANTI
031200010330     C*----------------------------------------------------*
031300010330     C     DEFCAM        BEGSR
031400010330     C*
031500020204     C* Inizializzo il buffer del record da scrivere
031600050408     C                   CLEAR                   FIVAB000
031700020204     C* Imposto i valori di default...
031800090326     C                   EVAL      VABCCM = 1391055
031900090326     C                   EVAL      VABLNP = 139
032000080611     C                   EVAL      VABCTR = 000
032100070208     C                   EVAL      VABCBO = '1'
032200090326     C                   EVAL      VABNAS = 'ABBIGLIAMENTO'
032300020204     C* ... e poi verifico se sono stati passati come parametri
032400020204     C                   IF        vlrppt > *blanks
032500040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
032600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032700020204     C                   EXSR      CHKNUM
032800020204     C                   IF        PiInt=*on
032900020204     C                   Z-ADD     PiVal         VABCCM
033000020204     C                   ENDIF
033100040301     C                   ENDIF
033200040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
033300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033400020204     C                   EXSR      CHKNUM
033500020204     C                   IF        PiInt=*on
033600020204     C                   Z-ADD     PiVal         VABLNP
033700020204     C                   ENDIF
033800040301     C                   ENDIF
033900040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
034000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034100020204     C                   EXSR      CHKNUM
034200020204     C                   IF        PiInt=*on
034300020204     C                   Z-ADD     PiVal         VABCTR
034400040301     C                   ENDIF
034500020204     C                   ENDIF
034600020204     C                   ENDIF
034700020204     C*
034800010330     C                   ENDSR
034900010607     C*----------------------------------------------------*
035000050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035100010607     C*----------------------------------------------------*
035200010607     C     IMPVAB        BEGSR
035300010607     C*
035400010607     C                   SETOFF                                       3132
035500010607     C*
035600010607     C* Reperimento campi ALFA
035700080715     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
035800080715     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
035900080715     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
036000080715     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
036100080715     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
036200080715     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
036300080715     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
036400090326     C                   IF        %subst(vindta:207:1)='F'
036500090326     C                   EVAL      VABCBO='1'
036600090326     C                   ENDIF
036700090326     C                   IF        %subst(vindta:207:1)='A'
036800090326     C                   EVAL      VABCBO='2'
036900090326     C                   ENDIF
037000080408     C*
037100080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
037200080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037300080408     C     '@':'A'       XLATE     VABRSD        VABRSD
037400080408     C* ==
037500060510     C*
037600060510     C* Reperimento campi NUMERICI
037700060510     C                   MOVEL     DATCOR        VABAAS
037800060510     C                   MOVE      DATCOR        VABMGS
037900080410     C* NSP
038000080715     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
038100060510     C                   EXSR      CHKNUM
038200060510     C                   IF        PiInt=*on
038300060510     C                   Z-ADD     PiVal         VABNSP
038400060404     C                   ELSE
038500070502     C                   SETON                                        31
038600060510     C                   Z-ADD     *zeros        VABNSP
038700060404     C                   EVAL      vinmsg = %trimr(vinmsg)
038800080410     C                             + ' ' + 'VABNSP'
038900060404     C                   ENDIF
039000080410     C* RMN
039100080715     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
039200080410     C                   EXSR      CHKNUM
039300080410     C                   IF        PiInt=*on
039400080410     C                   Z-ADD     PiVal         VABRMN
039500080410     C                   ELSE
039600080410     C                   SETON                                        31
039700080410     C                   Z-ADD     1             VABRMN
039800080410     C                   EVAL      vinmsg = %trimr(vinmsg)
039900080410     C                             + ' ' + 'VABRMN'
040000080410     C                   ENDIF
040100080408     C* CAD
040200080715     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
040300080408     C                   EXSR      CHKNUM
040400080408     C                   IF        PiInt=*on
040500080408     C                   Z-ADD     PiVal         Num5_0
040600080408     C                   MOVEL(p)  Num5_0        VABCAD
040700080408     C                   ELSE
040800080408     C                   SETON                                        32
040900080408     C                   EVAL      VABCAD = *zeros
041000080408     C                   EVAL      vinmsg = %trimr(vinmsg)
041100080408     C                             + ' ' + 'VABCAD'
041200080408     C                   ENDIF
041300080408     C* Reperisco la provincia dal CAP e dalla località
041400080408     C                   IF        VABCAD <> *blanks AND
041500080408     C                             VABPRD  = *blanks
041600080408     C                   CLEAR                   TISI95DS
041700080408     C                   EVAL      I95TCN = '3'
041800080408     C                   Z-ADD     datcor        I95DAT
041900080408     C                   EVAL      I95CAP = VABCAD
042000080408     C                   EVAL      I95LOC = VABLOD
042100080408     C                   EVAL      I95NAR = VABNZD
042200080408     C                   CALL      'TISI95R'
042300080408     C                   PARM                    TISI95DS
042400080408     C                   EVAL      VABPRD = O95PRV
042500080408     C                   ENDIF
042600080410     C* NCL
042700080715     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
042800080410     C                   EXSR      CHKNUM
042900080410     C                   IF        PiInt=*on
043000080410     C                   Z-ADD     PiVal         VABNCL
043100080410     C                   ELSE
043200080410     C                   ENDIF
043300090326     C                   IF        VABNCL=*zeros
043400090326     C                   EVAL      PiStr=%trim(%subst(vindta:129:2))
043500090326     C                   EXSR      CHKNUM
043600090326     C                   IF        PiInt=*on
043700090326     C                   Z-ADD     PiVal         VABNCL
043800090326     C                   ELSE
043900090326     C                   SETON                                        32
044000090326     C                   Z-ADD     *zeros        VABNCL
044100090326     C                   EVAL      vinmsg = %trimr(vinmsg)
044200090326     C                             + ' ' + 'VABNCL'
044300090326     C                   ENDIF
044400090326     C                   ENDIF
044500071029     C* PKB
044600080715     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
044700010607     C                   EXSR      CHKNUM
044800010607     C                   IF        PiNum=*on
044900080715     C                   Z-ADD     PiVal         VABPKB
045000010607     C                   ELSE
045100010607     C                   SETON                                        32
045200010607     C                   Z-ADD     *zeros        VABPKB
045300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045400010607     C                             + ' ' + 'VABPKB'
045500010607     C                   ENDIF
045600080320     C* CAS
045700080715     C                   IF        %trim(%subst(vindta:137:10))<>*all'0' AND
045800080715     C                             %trim(%subst(vindta:137:10))<>*blanks AND
045900080715     C                             %trim(%subst(vindta:137:10))<>'0000000,00'
046000080408     C                   EVAL      FlgCAS = '1'
046100080408     C                   EVAL      VABVCA = 'EUR'
046200090327     C                   EVAL      VABTIC = 'BM'
046300080715     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
046400080320     C                   EXSR      CHKNUM
046500080320     C                   IF        PiNum=*on
046600080611     C                   Z-ADD     PiVal         VABCAS
046700080320     C                   ELSE
046800080320     C                   SETON                                        32
046900080320     C                   Z-ADD     *zeros        VABCAS
047000080320     C                   EVAL      vinmsg = %trimr(vinmsg)
047100080320     C                             + ' ' + 'VABCAS'
047200080408     C                   ENDIF
047300080320     C                   ENDIF
047400020322     C*
047500020322     C* Considerazioni finali su CBO/CAS
047600050908     C                   IF        FlgCAS = '1'
047700040713     C                   IF        VABCBO = '1'
047800020322     C                   EVAL      VABCBO = '4'
047900040713     C                   ENDIF
048000040713     C                   IF        VABCBO = '2'
048100040713     C                   EVAL      VABCBO = '6'
048200040713     C                   ENDIF
048300020322     C                   ENDIF
048400020204     C*
048500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048600020204     C                   EXSR      CHKIMPDIV
048700020204     C*
048800010607     C                   ENDSR
048900010607     C*----------------------------------------------------*
049000010601
049100020204
049200020204     C*----------------------------------------------------*
049300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049400020204     C*----------------------------------------------------*
049500020204     C     CHKIMPDIV     BEGSR
049600020204     C*
049700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049800020204     C                   Z-ADD     *zeros        wrkDec            9 9
049900020204     C*
050000020204     C* Come prima cosa effettuo considerazioni sulla divisa
050100020204     C                   IF        vabIAS > *zeros
050200020204     C                   IF        vabVAS <> 'EUR'
050300020204     C                   EVAL      vabVAS =  'ITL'
050400020204     C                   ENDIF
050500020204     C                   ENDIF
050600020204     C*
050700020204     C                   IF        vabCAS > *zeros
050800020204     C                   IF        vabVCA <> 'EUR'
050900020204     C                   EVAL      vabVCA =  'ITL'
051000020204     C                   ENDIF
051100020204     C                   ENDIF
051200020204     C*
051300020204     C                   IF        vabVMD > *zeros
051400020321     C                   IF        vabVAD <> 'EUR'
051500020204     C                   EVAL      vabVAD =  'ITL'
051600020204     C                   ENDIF
051700020204     C                   ENDIF
051800020204     C*
051900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052000020204     C                   Z-ADD     vabIAS        wrkDec
052100020204     C                   IF        wrkDec > *zeros
052200020204     C                   IF        vabVAS = 'ITL'
052300020204     C                   EVAL      vabIAS = *zeros
052400020204     C                   ENDIF
052500020204     C                   ENDIF
052600020204     C*
052700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052800020204     C                   Z-ADD     vabCAS        wrkDec
052900020204     C                   IF        wrkDec > *zeros
053000020204     C                   IF        vabVCA = 'ITL'
053100020204     C                   EVAL      vabCAS = *zeros
053200020204     C                   ENDIF
053300020204     C                   ENDIF
053400020204     C*
053500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053600020204     C                   Z-ADD     vabVMD        wrkDec
053700020204     C                   IF        wrkDec > *zeros
053800020204     C                   IF        vabVAD = 'ITL'
053900020204     C                   EVAL      vabVMD = *zeros
054000020204     C                   ENDIF
054100020204     C                   ENDIF
054200020204     C*
054300020204     C                   ENDSR
054400020204     C***
054500020204
054600010330
054700010330
054800010330     C*----------------------------------------------------*
054900010330     C*  CONTROLLO NUMERICITA' CAMPI
055000010330     C*----------------------------------------------------*
055100010330     C     CHKNUM        BEGSR
055200010330     C*
055300010606     C                   IF        PiDecChr = *blanks
055400080611     C                   EVAL      PiDecChr = ','
055500010606     C                   ENDIF
055600080611     C*
055700080611     C                   callp     UBISNUM_Check(PiStr
055800080611     C                                          :PiDecChr
055900080611     C                                          :PiVal
056000080611     C                                          :PiNum
056100080611     C                                          :PiInt)
056200010330     C*
056300010330     C                   ENDSR
056400010330     C***
056500010330
056600010601
056700010601
056800010601
056900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
057000010601     C     repfil        BEGSR
057100010601     C*
057200010601     C                   if        invfil = *zeros and
057300010601     C                             depfil > *zeros and
057400010629     C                             (vinflg = *blanks or
057500010629     C                              vinflg = *zeros)
057600010601     C
057700010601     C                   eval      invfil = depfil
057800010601     C                   endif
057900010601     C*
058000010601     C                   if        depfil <> invfil and
058100010601     C                             invfil > *zeros
058200010601     C                   eval      flgMulti = '1'
058300010601     C                   if        vinflg = *blanks
058400010601     C                   add       1             cntNonEl
058500010601     C                   endif
058600010601     C                   endif
058700010601     C*
058800010601     C                   if        vinflg = '2'
058900010601     C                   eval      flgStato = '2'
059000010601     C                   endif
059100010601     C*
059200010601     C                   ENDSR
059300010601     C***
059400010601
059500010601
059600010601
059700010330
059800010330
059900010330
060000990920      /TITLE Invio dei dati al punto operativo.
060100000613     C     invio         BEGSR
060200990920     C*
060300990920     C                   reset                   dscmz
060400010601     C                   move      invfil        cmzdst
060500990920     C                   eval      cmzfld = vlrfou
060600990920     C                   eval      cmzmbd = vlrhdl
060700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060800000710     C                   if        prmfir = *blanks
060900050408     C                   eval      cmzfla = 'FIVAB00F'
061000050408     C                   eval      cmzmba = 'FIVAB00F'
061100000710     C                   else
061200000710     C                   eval      cmzfla = prmfir
061300000710     C                   eval      cmzmba = prmfir
061400000710     C                   endif
061500990920     C                   eval      cmznrr = *zeros
061600990920     C                   move      §ctrok        cmznrr
061700021018     C                   eval      cmzlba = vlrfl1
061800990920     C                   call(e)   'TIS711C'
061900990920     C                   parm                    dscmz
062000990921     C                   parm      *blanks       esito
062100990920     C                   if        %error
062200990920     C                             or cmzerr = '1'
062300990921     C                             or esito  = '1'
062400000710     C                   eval      wrkesito = '3'
062500990920     C                   endif
062600990920     C*
062700000613     C                   ENDSR
062800990910
062900010601
063000010601
063100010601
063200010601
063300010601      /TITLE Invio dei dati al punto operativo.
063400010601     C     opeini        BEGSR
063500010601     C*
063600010601     C* Inizializzo flag e contatori operativi
063700010601     C                   movel     '0'           flgGiro           1
063800010601     C                   movel     '0'           flgMulti          1
063900010601     C                   movel     '1'           flgStato          1
064000010615     C                   movel     '0'           flgOk             1
064100010601     C                   z-add     *zeros        cntNonEl         10 0
064200010601     C                   z-add     *zeros        depfil            3 0
064300010601     C                   z-add     *zeros        invfil            3 0
064400010601     C*
064500010601     C                   ENDSR
064600010601     C***
064700010601
064800010601
064900010601
065000010330
065100010330
065200000613     C     *inzsr        BEGSR
065300990910     C*
065400990910     C     *entry        plist
065500990920     C                   parm                    tivlrds
065600990921     C                   parm      wrkesito      esito
065700000724     C                   parm                    prmlit
065800000710     C                   parm                    prmfir
065900010330     C*
066000010330     C* CALCOLA LA DATA CORRENTE
066100010330     C                   time                    wn14             14 0
066200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066300010330     C                   z-add     wn8           g08dat
066400010330     C                   z-add     *zeros        g08inv
066500010330     C                   movel     '0'           g08err
066600010330     C                   call      'XSRDA8'
066700010330     C                   parm                    wlbda8
066800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066900000613     C*
067000000613     C                   ENDSR
067100000613     C***
067200990908
067300090326     Otitv1o0p  E            riepilogo         2
067400990915     O                                              'Upload via Internet'
067500990915     O                                           +1 'Traduzione TIVIN00R -'
067600050408     O                                           +1 'FIVABWWR'
067700990915     O                                           +1 'Testate bolle'
067800990915     O          E            riepilogo   2
067900990915     O                       wrkdata
068000990915     O                       wrkora              +1
068100990915     O                       procname            +1
068200990915     O          E            riepilogo   2
068300990915     O                                              'Cliente..................:'
068400010605     O                       VABCCM        z     +1
068500990915     O          E            riepilogo   2
068600990920     O                                              'Riferimento Strategi.....:'
068700990920     O                       vlrhdl              +1
068800990915     O          E            riepilogo   2
068900990915     O                                              'Giusti...................:'
069000971022     O                       §CTROK        2   +  1
069100990915     O          E            riepilogo   2
069200990915     O                                              'Sbagliati e corretti.....:'
069300971022     O                       §CTRMO        2   +  1
069400990915     O          E            riepilogo   2
069500990915     O                                              'Sbagliati e scartati.....:'
069600971022     O                       §CTRNO        2   +  1
069700000613
069800090326     Otitv1o0ps E            testdett          2
069900000613     O                                              'Upload via Internet'
070000000613     O                                           +1 'Traduzione TIVIN00R -'
070100050408     O                                           +1 'FIVABWWR'
070200000613     O                                           +1 'Testate bolle'
070300000616     O          E            testdett    3
070400000613     O                                           +2 'N° rec'
070500000613     O                                           +3 'Anteprima contenuto'
070600000616     O          E            rigadett    2
070700000613     O                       rrnum               +2
070800000621     O                       recko               +3
070900000616     O          E            findett     2
071000000613     O                       wrkdata
071100000613     O                       wrkora              +1
071200000613     O                       procname            +1
071300000616     O          E            findett     2
071400000613     O                                              'Cliente..................:'
071500010605     O                       VABCCM        z     +1
071600000616     O          E            findett     2
071700000613     O                                              'Riferimento Strategi.....:'
071800000613     O                       vlrhdl              +1
071900000616     O          E            findett     2
072000000613     O                                              'Giusti...................:'
072100000613     O                       §CTROK        2   +  1
072200000616     O          E            findett     2
072300000613     O                                              'Sbagliati e corretti.....:'
072400000613     O                       §CTRMO        2   +  1
072500000616     O          E            findett     2
072600000613     O                                              'Sbagliati e scartati.....:'
072700000613     O                       §CTRNO        2   +  1
072800000613** CMD - COMANDI CL
072900090326OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1O0P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
073000090326OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1O0PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
073100090326DLTOVR FILE(TITV1O0P TITV1O0PS) LVL(*)
073200010606
073300000613
