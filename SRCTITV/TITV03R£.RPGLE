000100021025      /TITLE Upload via Internet: traduzione in FIVABWWR. (x STEFANEL Cod. 0591072)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700020304     Ftitv03p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900020304     Ftitv03ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600050314     D*------------
001700050314     D* SCHIERE A RUN-TIME
001800050314     D*------------
001900050314     D* - "SERIE" CLIENTE ALFA
002000050314     D SERALF          S              1    DIM(10) CTDATA PERRCD(10)
002100050314     D* - "SERIE" CLIENTE NUMERICA
002200050314     D SERNUM          S              1    DIM(10) CTDATA PERRCD(10)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000010330     D esito           s              1
003100010330     D prmlit          s             10
003200010330     D prmfir          s             10
003300010330     D wrkesito        s                   like(esito)
003400010330     D wrkdata         s               d
003500010330     D wrkora          s               t
003600010330     D rrnum           s              6  0 INZ(*zeros)
003700010330     D recko           s            150    INZ(*blanks)
003800011113     D depcmd          s            150    INZ(*blanks)
003900010330     D*------------------
004000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004100010330     D*------------------
004200010330     D WLBDA8          DS                  INZ
004300010330     D  G08DAT                 1      8  0
004400010330     D  G08INV                 9     16  0
004500010330     D  G08ERR                17     17
004600010330     D  G08TGI                18     22  0
004700000313     D*
004800010330
004900010330
005000010330
005100990915     C                   time                    wrkdata
005200990915     C                   time                    wrkora
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600010601     C*
005700010601     C                   exsr      opeini
005800010605     C                   exsr      rwvab
005900010601     C*
006000010601     C                   seton                                        lr
006100010601
006200010601
006300010601
006400010601
006500010601     C*--------------------------------------------------------
006600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006700010601     C*--------------------------------------------------------
006800010601     C     PREELA        BEGSR
006900010601     C*
007000010601     C* SE OCCORRE SPEDIRE IN FILIALE
007100010601     C                   if        invfil <> *zeros and
007200010601     C                             flgGiro = '0'
007300010601     C*
007400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007500010601     C                   eval      flgGiro = '1'
007600000724     C*
007700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900010601     C     invfil        chain     azorg01l
008000000724     C                   if        %found
008100000724     C                   movel(p)  CMD(1)        depcmd
008200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008300000724     C*
008400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000724     C                   PARM                    depcmd
008800000724     C                   PARM                    LENGH
008900000724     C*
009000000724     C                   endif
009100000616     C*
009200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000616     C                   movel(p)  CMD(2)        depcmd
009400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009500000616     C*
009600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009700011113     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    depcmd
010000000616     C                   PARM                    LENGH
010100000613     C*
010200020304     C                   if        not %open(titv03ps)
010300020304     C                   open      titv03ps
010400000616     C                   except    testdett
010500000613     C                   endif
010600010601     C*
010700010601     C                   endif
010800010601     C*
010900010601     C                   ENDSR
011000010601     C***
011100010601
011200010601
011300010601
011400010601
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
012500020304     C                   if        %open(titv03ps)
012600000616     C                   except    findett
012700020304     C                   close     titv03ps
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
014000010601
014100010601
014200000613
014300000613     C*--------------------------------------------------------
014400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014500000613     C*--------------------------------------------------------
014600000621     C     STPR          BEGSR
014700000613     C*
014800020304     C                   if        not %open(titv03p)
014900020304     C                   open      titv03p
015000990915     C                   endif
015100990915     C*
015200990915     C                   except    riepilogo
015300990915     C*
015400020304     C                   if        %open(titv03p)
015500020304     C                   close     titv03p
015600990914     C                   endif
015700000613     C*
015800000613     C                   ENDSR
015900000613     C***
016000000613
016100010330
016200010601
016300010601
016400010601
016500010330     C*--------------------------------------------------------
016600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016700010330     C*--------------------------------------------------------
016800010605     C     RWVAB         BEGSR
016900010330     C*
017000010330     C                   if        not %open(tivin00r)
017100010330     C                   open      tivin00r
017200010330     C                   endif
017300021025     C                   if        not %open(fivabwwr)
017400021025     C                   open      fivabwwr
017500010330     C                   endif
017600010330     C*
017700010604     C                   clear                   §CTROK
017800010604     C                   clear                   §CTRMO
017900010604     C                   clear                   §CTRNO
018000010330     C*
018100010330     C                   DO        *HIVAL
018200010330     C*
018300010330     C                   READ      tivin00r                               70
018400010618     C*
018500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018600010618     C                   if        vinflg = '1'
018700010618     C                   eval      flgOk = '1'
018800010618     C                   endif
018900010618     C*
019000010330     C                   if        vindta > *blanks
019100010330     C                   add       1             rrnum
019200010330     C*
019300010601     C                   if        *in70 = *off and
019400010330     C                             (vinflg = *blanks
019500010330     C                              or vinflg = '0'
019600010330     C                              or vinflg = '2')
019700010330     C*
019800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019900010711     C                   if        vinflg = *blanks or vinflg = '0'
020000010711     C                   clear                   vinmsg
020100010711     C                   endif
020200010601     C*
020300010330     C                   exsr      inzvar
020400010330     C                   exsr      defcam
020500010605     C                   exsr      impvab
020600010601     C*
020700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020800010605     C                   eval      depfil = VABLNP
020900010601     C                   exsr      repfil
021000010601     C                   if        depfil = invfil
021100021025     C                   if        vlrpoi = 999
021200021025     C                   MOVE(P)   invfil        VABFGS
021300021025     C                   else
021400021025     C                   MOVE(P)   vlrpoi        VABFGS
021500021025     C                   endif
021600010601     C*
021700010601     C                   exsr      PREELA
021800010601     C*
021900010604     C* Ebbene...
022000010604     C*
022100010604     C  N31              ADD       1             §CTROK            7 0
022200010604     C   32              ADD       1             §CTRMO            7 0
022300010604     C   31              ADD       1             §CTRNO            7 0
022400020205     C*
022500020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
022600021025     C  N31              WRITE     FIVAB000
022700010604     C*
022800010604     C                   if        *in31 = *off and
022900010604     C                             *in32 = *off
023000010604     C                   eval      vinflg = '1'
023100010604     C                   else
023200010604     C                   eval      recko = vindta
023300010604     C                   except    rigadett
023400010604     C                   eval      vinflg = '2'
023500010604     C                   endif
023600010604     C                   endif
023700010601     C*
023800010604     C                   endif
023900010604     C*
024000010330     C                   else
024100010330     C                   eval      vinflg = '1'
024200010330     C                   endif
024300010601     C*
024400010601     C  N70              update    tivin000
024500010330     C*
024600010330     C  N70              ENDdo
024700010601     C
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
027600021025     C                   if        %open(fivabwwr)
027700021025     C                   close     fivabwwr
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
029500010330
029600010601
029700010601
029800010601
029900010601
030000010330     C*----------------------------------------------------*
030100010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030200010330     C*----------------------------------------------------*
030300010330     C     INZVAR        BEGSR
030400010330     C*
030500020204     C                   Z-ADD     *zeros        Num5_0            5 0
030600020213     C                   Z-ADD     *zeros        Num15_3          15 3
030700020204     C                   MOVEL     *blanks       WrkNote          70
030800010330     C*
030900010330     C                   ENDSR
031000010330     C*----------------------------------------------------*
031100010330     C*  IMPOSTAZIONE CAMPI COSTANTI
031200010330     C*----------------------------------------------------*
031300010330     C     DEFCAM        BEGSR
031400010330     C*
031500020204     C* Inizializzo il buffer del record da scrivere
031600021025     C                   CLEAR                   FIVAB000
031700020204     C* Imposto i valori di default...
031800020304     C                   EVAL      VABCCM = 0591072
031900020304     C                   EVAL      VABLNP = 059
032000110630     C                   EVAL      VABCTR = 300
032100020304     C                   EVAL      VABCTM = '7Q'
032200020204     C* ... e poi verifico se sono stati passati come parametri
032300020204     C                   IF        vlrppt > *blanks
032400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032500020204     C                   EXSR      CHKNUM
032600020204     C                   IF        PiInt=*on
032700020204     C                   Z-ADD     PiVal         VABCCM
032800020204     C                   ENDIF
032900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033000020204     C                   EXSR      CHKNUM
033100020204     C                   IF        PiInt=*on
033200020204     C                   Z-ADD     PiVal         VABLNP
033300020204     C                   ENDIF
033400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033500020204     C                   EXSR      CHKNUM
033600020204     C                   IF        PiInt=*on
033700020204     C                   Z-ADD     PiVal         VABCTR
033800020204     C                   ENDIF
033900020204     C                   ENDIF
034000020204     C*
034100010330     C                   ENDSR
034200010607     C*----------------------------------------------------*
034300050314     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034400010607     C*----------------------------------------------------*
034500010607     C     IMPVAB        BEGSR
034600010607     C*
034700010607     C                   SETOFF                                       3132
034800010607     C*
034900010607     C* Reperimento campi ALFA
035000010607     C*
035100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035200021104     C                   EVAL      VABRSD=%trim(%subst(vindta:21:35))
035300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035500020117     C* ==
035600021104     C                   EVAL      VABRD2='Tel. '+%trim(%subst(vindta:165:15))
035700021104     C                   EVAL      VABIND=%trim(%subst(vindta:71:60))
035800021104     C                   EVAL      VABLOD=%trim(%subst(vindta:136:25))
035900021104     C                   IF        %trim(%subst(vindta:161:4))='ROMA'
036000020321     C                   EVAL      VABPRD='RM'
036100020321     C                   ELSE
036200021104     C                   EVAL      VABPRD=%trim(%subst(vindta:161:4))
036300020321     C                   ENDIF
036400090422     C                   IF        %subst(vindta:256:15) = *blanks
036500090422     C                   EVAL      VABRMA=%trim(%subst(vindta:12:1))+
036600090422     C                                    %trim(%subst(vindta:14:7))
036700090422     C                   ELSE
036800090422     C                   EVAL      VABRMA=%trim(%subst(vindta:256:15))
036900090422     C                   ENDIF
037000090422     C                   EVAL      VABNOT=%trim(%subst(vindta:219:31)) + ' ' +
037100090422     C                                    %trim(%subst(vindta:271:50))
037200010607     C*
037300010607     C* Reperimento campi NUMERICI
037400021104     C                   EVAL      PiStr=%trim(%subst(vindta:4:4))
037500020304     C                   EXSR      CHKNUM
037600020304     C                   IF        PiInt=*on
037700020304     C                   Z-ADD     PiVal         VABAAS
037800020304     C                   ELSE
037900020304     C                   SETON                                        32
038000020304     C                   EVAL      VABAAS = *zeros
038100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
038200020304     C                             + ' ' + 'VABAAS'
038300020304     C                   ENDIF
038400020304     C*
038500021104     C                   EVAL      PiStr=%trim(%subst(vindta:8:4))
038600020304     C                   EXSR      CHKNUM
038700020304     C                   IF        PiInt=*on
038800020304     C                   Z-ADD     PiVal         VABMGS
038900020304     C                   ELSE
039000020304     C                   SETON                                        32
039100020304     C                   EVAL      VABMGS = *zeros
039200020304     C                   EVAL      vinmsg = %trimr(vinmsg)
039300020304     C                             + ' ' + 'VABMGS'
039400020304     C                   ENDIF
039500050314     C*
039600050314     C                   MOVEL     *blanks       wSerieAlfa        1
039700050314     C                   MOVEL     *zeros        wSerieNum         1
039800050314     C                   EVAL      wSerieAlfa=%subst(vindta:12:1)
039900050314     C                   Z-ADD     1             wInd              2 0
040000050314     C     wSerieAlfa    LOOKUP    SERALF(wInd)                           55
040100050314     C                   IF        %found
040200050314     C                   EVAL      wSerieNum = SERNUM(wInd)
040300050314     C                   ENDIF
040400050314     C*
040500021104     C                   EVAL      PiStr=%trim(%subst(vindta:14:7))
040600020304     C                   EXSR      CHKNUM
040700020304     C                   IF        PiInt=*on
040800020304     C                   Z-ADD     PiVal         VABNSP
040900050314     C                   MOVEL     wSerieNum     VABNSP
041000020304     C                   Z-ADD     PiVal         VABRMN
041100020304     C                   ELSE
041200020304     C                   SETON                                        32
041300020304     C                   Z-ADD     *zeros        VABNSP
041400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
041500020321     C                             + ' ' + 'VABNSP VABRMN'
041600020304     C                   ENDIF
041700020304     C*
041800021104     C                   EVAL      PiStr=%trim(%subst(vindta:131:5))
041900010607     C                   EXSR      CHKNUM
042000010607     C                   IF        PiInt=*on
042100020204     C                   Z-ADD     PiVal         Num5_0
042200020204     C                   MOVEL(p)  Num5_0        VABCAD
042300010607     C                   ELSE
042400010607     C                   SETON                                        32
042500020204     C                   EVAL      VABCAD = *zeros
042600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042700020204     C                             + ' ' + 'VABCAD'
042800010607     C                   ENDIF
042900010607     C*
043000021104     C                   EVAL      PiStr=%trim(%subst(vindta:180:7))
043100010607     C                   EXSR      CHKNUM
043200010607     C                   IF        PiInt=*on
043300010607     C                   Z-ADD     PiVal         VABNCL
043400010607     C                   ELSE
043500010607     C                   SETON                                        32
043600010607     C                   Z-ADD     *zeros        VABNCL
043700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043800010607     C                             + ' ' + 'VABNCL'
043900010607     C                   ENDIF
044000020304     C*
044100021104     C                   EVAL      PiStr=%trim(%subst(vindta:187:7))
044200020204     C                   EVAL      PiDecChr = ','
044300010607     C                   EXSR      CHKNUM
044400010607     C                   IF        PiNum=*on
044500020304     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
044600020304     C* riferisce ad un campo 7,3
044700020304     C                   EVAL(H)   VABPKB = PiVal/1000
044800010607     C                   ELSE
044900010607     C                   SETON                                        32
045000010607     C                   Z-ADD     *zeros        VABPKB
045100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045200010607     C                             + ' ' + 'VABPKB'
045300010607     C                   ENDIF
045400020304     C*
045500021104     C                   EVAL      PiStr=%trim(%subst(vindta:194:9))
045600020304     C                   EVAL      PiDecChr = ','
045700020304     C                   EXSR      CHKNUM
045800020304     C                   IF        PiNum=*on
045900020304     C* Divido x 100 perchè il campo di input non gestisce separatori decimali ma il valore si
046000020321     C* riferisce ad un campo 12,2
046100020304     C                   EVAL(H)   VABVLB = PiVal/100
046200020321     C** Divido ulteriormente x 1000 perchè il campo in input si riferisce DM cubi mentre il nostro
046300020321     C** si riferisce ad MC cubi
046400020321     C*                   EVAL(H)   VABVLB = VABVLB/1000
046500020304     C* Una volta determinato il giusto volume lo assegno anche al campo VABQFT x il tipo tariffa
046600110630     C***                Z-ADD     VABVLB        VABQFT
046700020304     C                   ELSE
046800020304     C                   SETON                                        32
046900020304     C                   Z-ADD     *zeros        VABPKB
047000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
047100110630     C***                          + ' ' + 'VABVLB VABQFT'
047200110630     C                             + ' ' + 'VABVLB'
047300020304     C                   ENDIF
047400020204     C*
047500020204     C* Considerazioni finali sui valori sopra ricavati
047600020204     C                   IF        (VABCAS = *zeros)
047700020204     C                   EVAL      VABCBO = '1 '
047800020204     C                   ELSE
047900020204     C                   EVAL      VABCBO = '4 '
048000020204     C                   ENDIF
048100020204     C*
048200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048300020204     C                   EXSR      CHKIMPDIV
048400020204     C*
048500010607     C                   ENDSR
048600010607     C*----------------------------------------------------*
048700010601
048800020204
048900020204     C*----------------------------------------------------*
049000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049100020204     C*----------------------------------------------------*
049200020204     C     CHKIMPDIV     BEGSR
049300020204     C*
049400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049500020204     C                   Z-ADD     *zeros        wrkDec            9 9
049600020204     C*
049700020204     C* Come prima cosa effettuo considerazioni sulla divisa
049800020204     C                   IF        vabIAS > *zeros
049900020204     C                   IF        vabVAS <> 'EUR'
050000020204     C                   EVAL      vabVAS =  'ITL'
050100020204     C                   ENDIF
050200020204     C                   ENDIF
050300020204     C*
050400020204     C                   IF        vabCAS > *zeros
050500020204     C                   IF        vabVCA <> 'EUR'
050600020204     C                   EVAL      vabVCA =  'ITL'
050700020204     C                   ENDIF
050800020204     C                   ENDIF
050900020204     C*
051000020204     C                   IF        vabVMD > *zeros
051100020321     C                   IF        vabVAD <> 'EUR'
051200020204     C                   EVAL      vabVAD =  'ITL'
051300020204     C                   ENDIF
051400020204     C                   ENDIF
051500020204     C*
051600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051700020204     C                   Z-ADD     vabIAS        wrkDec
051800020204     C                   IF        wrkDec > *zeros
051900020204     C                   IF        vabVAS = 'ITL'
052000020204     C                   EVAL      vabIAS = *zeros
052100020204     C                   ENDIF
052200020204     C                   ENDIF
052300020204     C*
052400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052500020204     C                   Z-ADD     vabCAS        wrkDec
052600020204     C                   IF        wrkDec > *zeros
052700020204     C                   IF        vabVCA = 'ITL'
052800020204     C                   EVAL      vabCAS = *zeros
052900020204     C                   ENDIF
053000020204     C                   ENDIF
053100020204     C*
053200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053300020204     C                   Z-ADD     vabVMD        wrkDec
053400020204     C                   IF        wrkDec > *zeros
053500020204     C                   IF        vabVAD = 'ITL'
053600020204     C                   EVAL      vabVMD = *zeros
053700020204     C                   ENDIF
053800020204     C                   ENDIF
053900020204     C*
054000020204     C                   ENDSR
054100020204     C***
054200020204
054300010330
054400010330
054500010330     C*----------------------------------------------------*
054600010330     C*  CONTROLLO NUMERICITA' CAMPI
054700010330     C*----------------------------------------------------*
054800010330     C     CHKNUM        BEGSR
054900010330     C*
055000010606     C                   IF        PiDecChr = *blanks
055100010606     C                   EVAL      PiDecChr = ','
055200010606     C                   ENDIF
055300010606     C*
055400010606     C                   CALL(e)   'ISNUMERIC'
055500010330     C                   PARM                    PiStr            30
055600010606     C                   PARM                    PiDecChr          1
055700010330     C                   PARM      *ZEROS        PiVal            30 9
055800010330     C                   PARM      '0'           PiInt             1
055900010330     C                   PARM      '0'           PiNum             1
056000010330     C                   IF        %error
056100010606     C                   EVAL      PiNum=*off
056200010330     C                   ENDIF
056300010330     C*
056400010330     C                   ENDSR
056500010330     C***
056600010330
056700010601
056800010601
056900010601
057000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
057100010601     C     repfil        BEGSR
057200010601     C*
057300010601     C                   if        invfil = *zeros and
057400010601     C                             depfil > *zeros and
057500010629     C                             (vinflg = *blanks or
057600010629     C                              vinflg = *zeros)
057700010601     C
057800010601     C                   eval      invfil = depfil
057900010601     C                   endif
058000010601     C*
058100010601     C                   if        depfil <> invfil and
058200010601     C                             invfil > *zeros
058300010601     C                   eval      flgMulti = '1'
058400010601     C                   if        vinflg = *blanks
058500010601     C                   add       1             cntNonEl
058600010601     C                   endif
058700010601     C                   endif
058800010601     C*
058900010601     C                   if        vinflg = '2'
059000010601     C                   eval      flgStato = '2'
059100010601     C                   endif
059200010601     C*
059300010601     C                   ENDSR
059400010601     C***
059500010601
059600010601
059700010601
059800010330
059900010330
060000010330
060100990920      /TITLE Invio dei dati al punto operativo.
060200000613     C     invio         BEGSR
060300990920     C*
060400990920     C                   reset                   dscmz
060500010601     C                   move      invfil        cmzdst
060600990920     C                   eval      cmzfld = vlrfou
060700990920     C                   eval      cmzmbd = vlrhdl
060800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060900000710     C                   if        prmfir = *blanks
061000021025     C                   eval      cmzfla = 'FIVAB00F'
061100021025     C                   eval      cmzmba = 'FIVAB00F'
061200000710     C                   else
061300000710     C                   eval      cmzfla = prmfir
061400000710     C                   eval      cmzmba = prmfir
061500000710     C                   endif
061600990920     C                   eval      cmznrr = *zeros
061700990920     C                   move      §ctrok        cmznrr
061800021018     C                   eval      cmzlba = vlrfl1
061900990920     C                   call(e)   'TIS711C'
062000990920     C                   parm                    dscmz
062100990921     C                   parm      *blanks       esito
062200990920     C                   if        %error
062300990920     C                             or cmzerr = '1'
062400990921     C                             or esito  = '1'
062500000710     C                   eval      wrkesito = '3'
062600990920     C                   endif
062700990920     C*
062800000613     C                   ENDSR
062900990910
063000010601
063100010601
063200010601
063300010601
063400010601      /TITLE Invio dei dati al punto operativo.
063500010601     C     opeini        BEGSR
063600010601     C*
063700010601     C* Inizializzo flag e contatori operativi
063800010601     C                   movel     '0'           flgGiro           1
063900010601     C                   movel     '0'           flgMulti          1
064000010601     C                   movel     '1'           flgStato          1
064100010615     C                   movel     '0'           flgOk             1
064200010601     C                   z-add     *zeros        cntNonEl         10 0
064300010601     C                   z-add     *zeros        depfil            3 0
064400010601     C                   z-add     *zeros        invfil            3 0
064500010601     C*
064600010601     C                   ENDSR
064700010601     C***
064800010601
064900010601
065000010601
065100010330
065200010330
065300000613     C     *inzsr        BEGSR
065400990910     C*
065500990910     C     *entry        plist
065600990920     C                   parm                    tivlrds
065700990921     C                   parm      wrkesito      esito
065800000724     C                   parm                    prmlit
065900000710     C                   parm                    prmfir
066000010330     C*
066100010330     C* CALCOLA LA DATA CORRENTE
066200010330     C                   time                    wn14             14 0
066300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066400010330     C                   z-add     wn8           g08dat
066500010330     C                   z-add     *zeros        g08inv
066600010330     C                   movel     '0'           g08err
066700010330     C                   call      'XSRDA8'
066800010330     C                   parm                    wlbda8
066900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067000000613     C*
067100000613     C                   ENDSR
067200000613     C***
067300990908
067400020304     Otitv03p   E            riepilogo         2
067500990915     O                                              'Upload via Internet'
067600990915     O                                           +1 'Traduzione TIVIN00R -'
067700021025     O                                           +1 'FIVABWWR'
067800990915     O                                           +1 'Testate bolle'
067900990915     O          E            riepilogo   2
068000990915     O                       wrkdata
068100990915     O                       wrkora              +1
068200990915     O                       procname            +1
068300990915     O          E            riepilogo   2
068400990915     O                                              'Cliente..................:'
068500010605     O                       VABCCM        z     +1
068600990915     O          E            riepilogo   2
068700990920     O                                              'Riferimento Strategi.....:'
068800990920     O                       vlrhdl              +1
068900990915     O          E            riepilogo   2
069000990915     O                                              'Giusti...................:'
069100971022     O                       §CTROK        2   +  1
069200990915     O          E            riepilogo   2
069300990915     O                                              'Sbagliati e corretti.....:'
069400971022     O                       §CTRMO        2   +  1
069500990915     O          E            riepilogo   2
069600990915     O                                              'Sbagliati e scartati.....:'
069700971022     O                       §CTRNO        2   +  1
069800000613
069900020304     Otitv03ps  E            testdett          2
070000000613     O                                              'Upload via Internet'
070100000613     O                                           +1 'Traduzione TIVIN00R -'
070200021025     O                                           +1 'FIVABWWR'
070300000613     O                                           +1 'Testate bolle'
070400000616     O          E            testdett    3
070500000613     O                                           +2 'N° rec'
070600000613     O                                           +3 'Anteprima contenuto'
070700000616     O          E            rigadett    2
070800000613     O                       rrnum               +2
070900000621     O                       recko               +3
071000000616     O          E            findett     2
071100000613     O                       wrkdata
071200000613     O                       wrkora              +1
071300000613     O                       procname            +1
071400000616     O          E            findett     2
071500000613     O                                              'Cliente..................:'
071600010605     O                       VABCCM        z     +1
071700000616     O          E            findett     2
071800000613     O                                              'Riferimento Strategi.....:'
071900000613     O                       vlrhdl              +1
072000000616     O          E            findett     2
072100000613     O                                              'Giusti...................:'
072200000613     O                       §CTROK        2   +  1
072300000616     O          E            findett     2
072400000613     O                                              'Sbagliati e corretti.....:'
072500000613     O                       §CTRMO        2   +  1
072600000616     O          E            findett     2
072700000613     O                                              'Sbagliati e scartati.....:'
072800000613     O                       §CTRNO        2   +  1
072900000613** CMD - COMANDI CL
073000020304OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV03P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
073100020304OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV03PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
073200020304DLTOVR FILE(TITV03P TITV03PS) LVL(*)
073300010606
073400000613
073500050314** "SERIE" CLIENTE ALFA **
073600050314CF
073700050314** "SERIE" CLIENTE NUMERICA **
07380005031436
