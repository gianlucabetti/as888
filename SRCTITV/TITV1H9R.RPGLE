000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200080611     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700080715     Ftitv1h9p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900080715     Ftitv1h9ps O    f  198        PRINTER usropn
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
011300080715     C                   if        not %open(titv1h9ps)
011400080715     C                   open      titv1h9ps
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
013200080715     C                   if        %open(titv1h9ps)
013300000616     C                   except    findett
013400080715     C                   close     titv1h9ps
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
015300080715     C                   if        not %open(titv1h9p)
015400080715     C                   open      titv1h9p
015500990915     C                   endif
015600990915     C*
015700990915     C                   except    riepilogo
015800990915     C*
015900080715     C                   if        %open(titv1h9p)
016000080715     C                   close     titv1h9p
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
032500080715     C                   EVAL      VABCCM = 0054695
032600080715     C                   EVAL      VABLNP = 005
032700080611     C                   EVAL      VABCTR = 000
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
036300080715     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
036400080715     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
036500080715     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
036600080715     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
036700080715     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
036800080715     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
036900080715     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
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
043200080410     C                   SETON                                        32
043300080410     C                   Z-ADD     *zeros        VABNCL
043400080410     C                   EVAL      vinmsg = %trimr(vinmsg)
043500080410     C                             + ' ' + 'VABNCL'
043600080410     C                   ENDIF
043700071029     C* PKB
043800080715     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
043900010607     C                   EXSR      CHKNUM
044000010607     C                   IF        PiNum=*on
044100080715     C                   Z-ADD     PiVal         VABPKB
044200010607     C                   ELSE
044300010607     C                   SETON                                        32
044400010607     C                   Z-ADD     *zeros        VABPKB
044500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044600010607     C                             + ' ' + 'VABPKB'
044700010607     C                   ENDIF
044800080320     C* CAS
044900080715     C                   IF        %trim(%subst(vindta:137:10))<>*all'0' AND
045000080715     C                             %trim(%subst(vindta:137:10))<>*blanks AND
045100080715     C                             %trim(%subst(vindta:137:10))<>'0000000,00'
045200080408     C                   EVAL      FlgCAS = '1'
045300080408     C                   EVAL      VABVCA = 'EUR'
045400080919     C                   EVAL      VABTIC = 'BM'
045500080715     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
045600080320     C                   EXSR      CHKNUM
045700080320     C                   IF        PiNum=*on
045800080611     C                   Z-ADD     PiVal         VABCAS
045900080320     C                   ELSE
046000080320     C                   SETON                                        32
046100080320     C                   Z-ADD     *zeros        VABCAS
046200080320     C                   EVAL      vinmsg = %trimr(vinmsg)
046300080320     C                             + ' ' + 'VABCAS'
046400080408     C                   ENDIF
046500080320     C                   ENDIF
046600020322     C*
046700020322     C* Considerazioni finali su CBO/CAS
046800050908     C                   IF        FlgCAS = '1'
046900040713     C                   IF        VABCBO = '1'
047000020322     C                   EVAL      VABCBO = '4'
047100040713     C                   ENDIF
047200040713     C                   IF        VABCBO = '2'
047300040713     C                   EVAL      VABCBO = '6'
047400040713     C                   ENDIF
047500020322     C                   ENDIF
047600020204     C*
047700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047800020204     C                   EXSR      CHKIMPDIV
047900020204     C*
048000010607     C                   ENDSR
048100010607     C*----------------------------------------------------*
048200010601
048300020204
048400020204     C*----------------------------------------------------*
048500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048600020204     C*----------------------------------------------------*
048700020204     C     CHKIMPDIV     BEGSR
048800020204     C*
048900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049000020204     C                   Z-ADD     *zeros        wrkDec            9 9
049100020204     C*
049200020204     C* Come prima cosa effettuo considerazioni sulla divisa
049300020204     C                   IF        vabIAS > *zeros
049400020204     C                   IF        vabVAS <> 'EUR'
049500020204     C                   EVAL      vabVAS =  'ITL'
049600020204     C                   ENDIF
049700020204     C                   ENDIF
049800020204     C*
049900020204     C                   IF        vabCAS > *zeros
050000020204     C                   IF        vabVCA <> 'EUR'
050100020204     C                   EVAL      vabVCA =  'ITL'
050200020204     C                   ENDIF
050300020204     C                   ENDIF
050400020204     C*
050500020204     C                   IF        vabVMD > *zeros
050600020321     C                   IF        vabVAD <> 'EUR'
050700020204     C                   EVAL      vabVAD =  'ITL'
050800020204     C                   ENDIF
050900020204     C                   ENDIF
051000020204     C*
051100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051200020204     C                   Z-ADD     vabIAS        wrkDec
051300020204     C                   IF        wrkDec > *zeros
051400020204     C                   IF        vabVAS = 'ITL'
051500020204     C                   EVAL      vabIAS = *zeros
051600020204     C                   ENDIF
051700020204     C                   ENDIF
051800020204     C*
051900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052000020204     C                   Z-ADD     vabCAS        wrkDec
052100020204     C                   IF        wrkDec > *zeros
052200020204     C                   IF        vabVCA = 'ITL'
052300020204     C                   EVAL      vabCAS = *zeros
052400020204     C                   ENDIF
052500020204     C                   ENDIF
052600020204     C*
052700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052800020204     C                   Z-ADD     vabVMD        wrkDec
052900020204     C                   IF        wrkDec > *zeros
053000020204     C                   IF        vabVAD = 'ITL'
053100020204     C                   EVAL      vabVMD = *zeros
053200020204     C                   ENDIF
053300020204     C                   ENDIF
053400020204     C*
053500020204     C                   ENDSR
053600020204     C***
053700020204
053800010330
053900010330
054000010330     C*----------------------------------------------------*
054100010330     C*  CONTROLLO NUMERICITA' CAMPI
054200010330     C*----------------------------------------------------*
054300010330     C     CHKNUM        BEGSR
054400010330     C*
054500010606     C                   IF        PiDecChr = *blanks
054600080611     C                   EVAL      PiDecChr = ','
054700010606     C                   ENDIF
054800080611     C*
054900080611     C                   callp     UBISNUM_Check(PiStr
055000080611     C                                          :PiDecChr
055100080611     C                                          :PiVal
055200080611     C                                          :PiNum
055300080611     C                                          :PiInt)
055400010330     C*
055500010330     C                   ENDSR
055600010330     C***
055700010330
055800010601
055900010601
056000010601
056100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
056200010601     C     repfil        BEGSR
056300010601     C*
056400010601     C                   if        invfil = *zeros and
056500010601     C                             depfil > *zeros and
056600010629     C                             (vinflg = *blanks or
056700010629     C                              vinflg = *zeros)
056800010601     C
056900010601     C                   eval      invfil = depfil
057000010601     C                   endif
057100010601     C*
057200010601     C                   if        depfil <> invfil and
057300010601     C                             invfil > *zeros
057400010601     C                   eval      flgMulti = '1'
057500010601     C                   if        vinflg = *blanks
057600010601     C                   add       1             cntNonEl
057700010601     C                   endif
057800010601     C                   endif
057900010601     C*
058000010601     C                   if        vinflg = '2'
058100010601     C                   eval      flgStato = '2'
058200010601     C                   endif
058300010601     C*
058400010601     C                   ENDSR
058500010601     C***
058600010601
058700010601
058800010601
058900010330
059000010330
059100010330
059200990920      /TITLE Invio dei dati al punto operativo.
059300000613     C     invio         BEGSR
059400990920     C*
059500990920     C                   reset                   dscmz
059600010601     C                   move      invfil        cmzdst
059700990920     C                   eval      cmzfld = vlrfou
059800990920     C                   eval      cmzmbd = vlrhdl
059900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060000000710     C                   if        prmfir = *blanks
060100050408     C                   eval      cmzfla = 'FIVAB00F'
060200050408     C                   eval      cmzmba = 'FIVAB00F'
060300000710     C                   else
060400000710     C                   eval      cmzfla = prmfir
060500000710     C                   eval      cmzmba = prmfir
060600000710     C                   endif
060700990920     C                   eval      cmznrr = *zeros
060800990920     C                   move      §ctrok        cmznrr
060900021018     C                   eval      cmzlba = vlrfl1
061000990920     C                   call(e)   'TIS711C'
061100990920     C                   parm                    dscmz
061200990921     C                   parm      *blanks       esito
061300990920     C                   if        %error
061400990920     C                             or cmzerr = '1'
061500990921     C                             or esito  = '1'
061600000710     C                   eval      wrkesito = '3'
061700990920     C                   endif
061800990920     C*
061900000613     C                   ENDSR
062000990910
062100010601
062200010601
062300010601
062400010601
062500010601      /TITLE Invio dei dati al punto operativo.
062600010601     C     opeini        BEGSR
062700010601     C*
062800010601     C* Inizializzo flag e contatori operativi
062900010601     C                   movel     '0'           flgGiro           1
063000010601     C                   movel     '0'           flgMulti          1
063100010601     C                   movel     '1'           flgStato          1
063200010615     C                   movel     '0'           flgOk             1
063300010601     C                   z-add     *zeros        cntNonEl         10 0
063400010601     C                   z-add     *zeros        depfil            3 0
063500010601     C                   z-add     *zeros        invfil            3 0
063600010601     C*
063700010601     C                   ENDSR
063800010601     C***
063900010601
064000010601
064100010601
064200010330
064300010330
064400000613     C     *inzsr        BEGSR
064500990910     C*
064600990910     C     *entry        plist
064700990920     C                   parm                    tivlrds
064800990921     C                   parm      wrkesito      esito
064900000724     C                   parm                    prmlit
065000000710     C                   parm                    prmfir
065100010330     C*
065200010330     C* CALCOLA LA DATA CORRENTE
065300010330     C                   time                    wn14             14 0
065400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
065500010330     C                   z-add     wn8           g08dat
065600010330     C                   z-add     *zeros        g08inv
065700010330     C                   movel     '0'           g08err
065800010330     C                   call      'XSRDA8'
065900010330     C                   parm                    wlbda8
066000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066100000613     C*
066200000613     C                   ENDSR
066300000613     C***
066400990908
066500080715     Otitv1h9p  E            riepilogo         2
066600990915     O                                              'Upload via Internet'
066700990915     O                                           +1 'Traduzione TIVIN00R -'
066800050408     O                                           +1 'FIVABWWR'
066900990915     O                                           +1 'Testate bolle'
067000990915     O          E            riepilogo   2
067100990915     O                       wrkdata
067200990915     O                       wrkora              +1
067300990915     O                       procname            +1
067400990915     O          E            riepilogo   2
067500990915     O                                              'Cliente..................:'
067600010605     O                       VABCCM        z     +1
067700990915     O          E            riepilogo   2
067800990920     O                                              'Riferimento Strategi.....:'
067900990920     O                       vlrhdl              +1
068000990915     O          E            riepilogo   2
068100990915     O                                              'Giusti...................:'
068200971022     O                       §CTROK        2   +  1
068300990915     O          E            riepilogo   2
068400990915     O                                              'Sbagliati e corretti.....:'
068500971022     O                       §CTRMO        2   +  1
068600990915     O          E            riepilogo   2
068700990915     O                                              'Sbagliati e scartati.....:'
068800971022     O                       §CTRNO        2   +  1
068900000613
069000080715     Otitv1h9ps E            testdett          2
069100000613     O                                              'Upload via Internet'
069200000613     O                                           +1 'Traduzione TIVIN00R -'
069300050408     O                                           +1 'FIVABWWR'
069400000613     O                                           +1 'Testate bolle'
069500000616     O          E            testdett    3
069600000613     O                                           +2 'N° rec'
069700000613     O                                           +3 'Anteprima contenuto'
069800000616     O          E            rigadett    2
069900000613     O                       rrnum               +2
070000000621     O                       recko               +3
070100000616     O          E            findett     2
070200000613     O                       wrkdata
070300000613     O                       wrkora              +1
070400000613     O                       procname            +1
070500000616     O          E            findett     2
070600000613     O                                              'Cliente..................:'
070700010605     O                       VABCCM        z     +1
070800000616     O          E            findett     2
070900000613     O                                              'Riferimento Strategi.....:'
071000000613     O                       vlrhdl              +1
071100000616     O          E            findett     2
071200000613     O                                              'Giusti...................:'
071300000613     O                       §CTROK        2   +  1
071400000616     O          E            findett     2
071500000613     O                                              'Sbagliati e corretti.....:'
071600000613     O                       §CTRMO        2   +  1
071700000616     O          E            findett     2
071800000613     O                                              'Sbagliati e scartati.....:'
071900000613     O                       §CTRNO        2   +  1
072000000613** CMD - COMANDI CL
072100080715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1H9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
072200080715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1H9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
072300080715DLTOVR FILE(TITV1H9P TITV1H9PS) LVL(*)
072400010606
072500000613
