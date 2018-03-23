000100021025      /TITLE Upload via Internet: traduzione in FIVATWWR. (x STEFANEL Cod. 0591072)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVATwwr  O    E             DISK    usropn
000700020321     Ftitv07p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900020321     Ftitv07ps  O    f  198        PRINTER usropn
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
005800020321     C                   exsr      rwvat
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
010200020321     C                   if        not %open(titv07ps)
010300020321     C                   open      titv07ps
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
012500020321     C                   if        %open(titv07ps)
012600000616     C                   except    findett
012700020321     C                   close     titv07ps
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
014800020321     C                   if        not %open(titv07p)
014900020321     C                   open      titv07p
015000990915     C                   endif
015100990915     C*
015200990915     C                   except    riepilogo
015300990915     C*
015400020321     C                   if        %open(titv07p)
015500020321     C                   close     titv07p
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
016600021025     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
016700010330     C*--------------------------------------------------------
016800020321     C     RWVAT         BEGSR
016900010330     C*
017000010330     C                   if        not %open(tivin00r)
017100010330     C                   open      tivin00r
017200010330     C                   endif
017300021025     C                   if        not %open(fivatwwr)
017400021025     C                   open      fivatwwr
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
020500020321     C                   exsr      impvat
020600010601     C*
020700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020800020321     C                   eval      depfil = VATLNP
020900010601     C                   exsr      repfil
021000010601     C                   if        depfil = invfil
021100021025     C                   if        vlrpoi = 999
021200021025     C                   MOVE(P)   invfil        VATFGS
021300021025     C                   else
021400021025     C                   MOVE(P)   vlrpoi        VATFGS
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
022600021025     C  N31              WRITE     FIVAT000
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
027600021025     C                   if        %open(fivatwwr)
027700021025     C                   close     fivatwwr
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
030300060331     C     INZVAR        BEGSR
030400010330     C*
030500010330     C*
030600010330     C                   ENDSR
030700010330     C*----------------------------------------------------*
030800010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030900010330     C*----------------------------------------------------*
031000010330     C     DEFCAM        BEGSR
031100010330     C*
031200020204     C* Inizializzo il buffer del record da scrivere
031300021025     C                   CLEAR                   FIVAT000
031400020204     C* Imposto i valori di default...
031500020321     C                   EVAL      VATCCM = 0591072
031600020321     C                   EVAL      VATLNP = 059
031700020321     C                   EVAL      VATTRC = 'E'
031800020204     C* ... e poi verifico se sono stati passati come parametri
031900020204     C                   IF        vlrppt > *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020321     C                   Z-ADD     PiVal         VATCCM
032400020204     C                   ENDIF
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020321     C                   Z-ADD     PiVal         VATLNP
032900020204     C                   ENDIF
033000020321     C                   ENDIF
033100020204     C*
033200010330     C                   ENDSR
033300010607     C*----------------------------------------------------*
033400060331     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
033500010607     C*----------------------------------------------------*
033600020321     C     IMPVAT        BEGSR
033700010607     C*
033800010607     C                   SETOFF                                       3132
033900010607     C*
034000010607     C* Reperimento campi ALFA
034100060331     C***                EVAL      VATNOT='20'+%trim(%subst(vindta:17:2))+
034200060331     C***                                 %trim(%subst(vindta:21:8))
034300060331     C                   EVAL      VATNOT=%trim(%subst(vindta:17:12))
034400020403     C*
034500020403     C* Calcolo il check-digits sul barcode fin qui composto
034600020403     C                   EXSR      CHKDGT
034700010607     C*
034800010607     C* Reperimento campi NUMERICI
034900020321     C                   EVAL      PiStr=%trim(%subst(vindta:4:4))
035000020304     C                   EXSR      CHKNUM
035100020304     C                   IF        PiInt=*on
035200020321     C                   Z-ADD     PiVal         VATAAS
035300020304     C                   ELSE
035400020304     C                   SETON                                        32
035500020321     C                   EVAL      VATAAS = *zeros
035600020304     C                   EVAL      vinmsg = %trimr(vinmsg)
035700020321     C                             + ' ' + 'VATAAS'
035800020304     C                   ENDIF
035900020304     C*
036000050314     C                   MOVEL     *blanks       wSerieAlfa        1
036100050314     C                   MOVEL     *zeros        wSerieNum         1
036200050314     C                   EVAL      wSerieAlfa=%subst(vindta:8:1)
036300050314     C                   Z-ADD     1             wInd              2 0
036400050314     C     wSerieAlfa    LOOKUP    SERALF(wInd)                           55
036500050314     C                   IF        %found
036600050314     C                   EVAL      wSerieNum = SERNUM(wInd)
036700050314     C                   ENDIF
036800050314     C*
036900050314     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
037000020304     C                   EXSR      CHKNUM
037100020304     C                   IF        PiInt=*on
037200020321     C                   Z-ADD     PiVal         VATNSP
037300050314     C                   MOVEL     wSerieNum     VATNSP
037400020304     C                   ELSE
037500020304     C                   SETON                                        32
037600020321     C                   Z-ADD     *zeros        VATNSP
037700020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037800020321     C                             + ' ' + 'VATNSP'
037900020304     C                   ENDIF
038000020204     C*
038100010607     C                   ENDSR
038200010607     C*----------------------------------------------------*
038300010601
038400010330
038500010330     C*----------------------------------------------------*
038600010330     C*  CONTROLLO NUMERICITA' CAMPI
038700010330     C*----------------------------------------------------*
038800010330     C     CHKNUM        BEGSR
038900010330     C*
039000010606     C                   IF        PiDecChr = *blanks
039100010606     C                   EVAL      PiDecChr = ','
039200010606     C                   ENDIF
039300010606     C*
039400010606     C                   CALL(e)   'ISNUMERIC'
039500010330     C                   PARM                    PiStr            30
039600010606     C                   PARM                    PiDecChr          1
039700010330     C                   PARM      *ZEROS        PiVal            30 9
039800010330     C                   PARM      '0'           PiInt             1
039900010330     C                   PARM      '0'           PiNum             1
040000010330     C                   IF        %error
040100010606     C                   EVAL      PiNum=*off
040200010330     C                   ENDIF
040300010330     C*
040400010330     C                   ENDSR
040500010330     C***
040600010330
040700010601
040800020403
040900020403     C*----------------------------------------------------*
041000020403     C*  CALCOLO IL CHECK-DIGITS (EAN/UCC-13)
041100020403     C*----------------------------------------------------*
041200020403     C     CHKDGT        BEGSR
041300020403     C*
041400020403     C* Innanzitutto verifico che sia numerico
041500020403     C                   EVAL      PiStr=%trim(VATNOT)
041600020403     C                   EXSR      CHKNUM
041700020403     C                   IF        PiInt=*on
041800020403     C* Se è numerico... procedo con il calcolo del check-digits
041900020403     C                   Z-ADD     *ZEROS        SumPARI           3 0
042000020403     C                   Z-ADD     *ZEROS        SumDISPARI        3 0
042100020403     C                   Z-ADD     *ZEROS        SumTOT            3 0
042200020403     C                   MOVEL     *ZEROS        AddendoA          1
042300020403     C                   Z-ADD     *ZEROS        AddendoN          1 0
042400020403     C                   Z-ADD     *ZEROS        RestoA10          2 0
042500020403     C                   MOVEL     *BLANKS       ChkDigits         1
042600020403     C* Sommo le cifre in posizioni pari
042700020403     C                   EVAL      AddendoA = %subst(VATNOT:2:1)
042800020403     C                   MOVEL     AddendoA      AddendoN
042900020403     C                   ADD       AddendoN      SumPARI
043000020403     C                   EVAL      AddendoA = %subst(VATNOT:4:1)
043100020403     C                   MOVEL     AddendoA      AddendoN
043200020403     C                   ADD       AddendoN      SumPARI
043300020403     C                   EVAL      AddendoA = %subst(VATNOT:6:1)
043400020403     C                   MOVEL     AddendoA      AddendoN
043500020403     C                   ADD       AddendoN      SumPARI
043600020403     C                   EVAL      AddendoA = %subst(VATNOT:8:1)
043700020403     C                   MOVEL     AddendoA      AddendoN
043800020403     C                   ADD       AddendoN      SumPARI
043900020403     C                   EVAL      AddendoA = %subst(VATNOT:10:1)
044000020403     C                   MOVEL     AddendoA      AddendoN
044100020403     C                   ADD       AddendoN      SumPARI
044200020403     C                   EVAL      AddendoA = %subst(VATNOT:12:1)
044300020403     C                   MOVEL     AddendoA      AddendoN
044400020403     C                   ADD       AddendoN      SumPARI
044500020403     C* Moltiplico la somma ottenuta per 3
044600020403     C                   EVAL      SumPARI = SumPARI*3
044700020403     C* Sommo le cifre in posizioni dispari
044800020403     C                   EVAL      AddendoA = %subst(VATNOT:1:1)
044900020403     C                   MOVEL     AddendoA      AddendoN
045000020403     C                   ADD       AddendoN      SumDISPARI
045100020403     C                   EVAL      AddendoA = %subst(VATNOT:3:1)
045200020403     C                   MOVEL     AddendoA      AddendoN
045300020403     C                   ADD       AddendoN      SumDISPARI
045400020403     C                   EVAL      AddendoA = %subst(VATNOT:5:1)
045500020403     C                   MOVEL     AddendoA      AddendoN
045600020403     C                   ADD       AddendoN      SumDISPARI
045700020403     C                   EVAL      AddendoA = %subst(VATNOT:7:1)
045800020403     C                   MOVEL     AddendoA      AddendoN
045900020403     C                   ADD       AddendoN      SumDISPARI
046000020403     C                   EVAL      AddendoA = %subst(VATNOT:9:1)
046100020403     C                   MOVEL     AddendoA      AddendoN
046200020403     C                   ADD       AddendoN      SumDISPARI
046300020403     C                   EVAL      AddendoA = %subst(VATNOT:11:1)
046400020403     C                   MOVEL     AddendoA      AddendoN
046500020403     C                   ADD       AddendoN      SumDISPARI
046600020403     C* Aggiungo alla somma delle cifre pari moltiplicate per 3 alla somma di quelle dispari
046700020403     C                   EVAL      SumTOT = SumPARI + SumDISPARI
046800020403     C                   MOVE      SumTOT        AddendoN
046900020403     C                   EVAL      RestoA10 = 10 - AddendoN
047000020403     C                   MOVE      RestoA10      ChkDigits
047100020403     C* Infine compongo il barcode finale
047200020403     C                   EVAL      VATNOT = %trim(VATNOT) + ChkDigits
047300020403     C                   ENDIF
047400020403     C*
047500020403     C                   ENDSR
047600020403     C***
047700020403
047800020403
047900010601
048000010601
048100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
048200010601     C     repfil        BEGSR
048300010601     C*
048400010601     C                   if        invfil = *zeros and
048500010601     C                             depfil > *zeros and
048600010629     C                             (vinflg = *blanks or
048700010629     C                              vinflg = *zeros)
048800010601     C
048900010601     C                   eval      invfil = depfil
049000010601     C                   endif
049100010601     C*
049200010601     C                   if        depfil <> invfil and
049300010601     C                             invfil > *zeros
049400010601     C                   eval      flgMulti = '1'
049500010601     C                   if        vinflg = *blanks
049600010601     C                   add       1             cntNonEl
049700010601     C                   endif
049800010601     C                   endif
049900010601     C*
050000010601     C                   if        vinflg = '2'
050100010601     C                   eval      flgStato = '2'
050200010601     C                   endif
050300010601     C*
050400010601     C                   ENDSR
050500010601     C***
050600010601
050700010601
050800010601
050900010330
051000010330
051100010330
051200990920      /TITLE Invio dei dati al punto operativo.
051300000613     C     invio         BEGSR
051400990920     C*
051500990920     C                   reset                   dscmz
051600010601     C                   move      invfil        cmzdst
051700990920     C                   eval      cmzfld = vlrfou
051800990920     C                   eval      cmzmbd = vlrhdl
051900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
052000000710     C                   if        prmfir = *blanks
052100021025     C                   eval      cmzfla = 'FIVAT00F'
052200021025     C                   eval      cmzmba = 'FIVAT00F'
052300000710     C                   else
052400000710     C                   eval      cmzfla = prmfir
052500000710     C                   eval      cmzmba = prmfir
052600000710     C                   endif
052700990920     C                   eval      cmznrr = *zeros
052800990920     C                   move      §ctrok        cmznrr
052900021018     C                   eval      cmzlba = vlrfl1
053000990920     C                   call(e)   'TIS711C'
053100990920     C                   parm                    dscmz
053200990921     C                   parm      *blanks       esito
053300990920     C                   if        %error
053400990920     C                             or cmzerr = '1'
053500990921     C                             or esito  = '1'
053600000710     C                   eval      wrkesito = '3'
053700990920     C                   endif
053800990920     C*
053900000613     C                   ENDSR
054000990910
054100010601
054200010601
054300010601
054400010601
054500010601      /TITLE Invio dei dati al punto operativo.
054600010601     C     opeini        BEGSR
054700010601     C*
054800010601     C* Inizializzo flag e contatori operativi
054900010601     C                   movel     '0'           flgGiro           1
055000010601     C                   movel     '0'           flgMulti          1
055100010601     C                   movel     '1'           flgStato          1
055200010615     C                   movel     '0'           flgOk             1
055300010601     C                   z-add     *zeros        cntNonEl         10 0
055400010601     C                   z-add     *zeros        depfil            3 0
055500010601     C                   z-add     *zeros        invfil            3 0
055600010601     C*
055700010601     C                   ENDSR
055800010601     C***
055900010601
056000010601
056100010601
056200010330
056300010330
056400000613     C     *inzsr        BEGSR
056500990910     C*
056600990910     C     *entry        plist
056700990920     C                   parm                    tivlrds
056800990921     C                   parm      wrkesito      esito
056900000724     C                   parm                    prmlit
057000000710     C                   parm                    prmfir
057100010330     C*
057200010330     C* CALCOLA LA DATA CORRENTE
057300010330     C                   time                    wn14             14 0
057400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057500010330     C                   z-add     wn8           g08dat
057600010330     C                   z-add     *zeros        g08inv
057700010330     C                   movel     '0'           g08err
057800010330     C                   call      'XSRDA8'
057900010330     C                   parm                    wlbda8
058000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058100000613     C*
058200000613     C                   ENDSR
058300000613     C***
058400990908
058500020321     Otitv07p   E            riepilogo         2
058600990915     O                                              'Upload via Internet'
058700990915     O                                           +1 'Traduzione TIVIN00R -'
058800021025     O                                           +1 'FIVATWWR'
058900990915     O                                           +1 'Testate bolle'
059000990915     O          E            riepilogo   2
059100990915     O                       wrkdata
059200990915     O                       wrkora              +1
059300990915     O                       procname            +1
059400990915     O          E            riepilogo   2
059500990915     O                                              'Cliente..................:'
059600020321     O                       VATCCM        z     +1
059700990915     O          E            riepilogo   2
059800990920     O                                              'Riferimento Strategi.....:'
059900990920     O                       vlrhdl              +1
060000990915     O          E            riepilogo   2
060100990915     O                                              'Giusti...................:'
060200971022     O                       §CTROK        2   +  1
060300990915     O          E            riepilogo   2
060400990915     O                                              'Sbagliati e corretti.....:'
060500971022     O                       §CTRMO        2   +  1
060600990915     O          E            riepilogo   2
060700990915     O                                              'Sbagliati e scartati.....:'
060800971022     O                       §CTRNO        2   +  1
060900000613
061000020321     Otitv07ps  E            testdett          2
061100000613     O                                              'Upload via Internet'
061200000613     O                                           +1 'Traduzione TIVIN00R -'
061300021025     O                                           +1 'FIVATWWR'
061400000613     O                                           +1 'Testate bolle'
061500000616     O          E            testdett    3
061600000613     O                                           +2 'N° rec'
061700000613     O                                           +3 'Anteprima contenuto'
061800000616     O          E            rigadett    2
061900000613     O                       rrnum               +2
062000000621     O                       recko               +3
062100000616     O          E            findett     2
062200000613     O                       wrkdata
062300000613     O                       wrkora              +1
062400000613     O                       procname            +1
062500000616     O          E            findett     2
062600000613     O                                              'Cliente..................:'
062700020321     O                       VATCCM        z     +1
062800000616     O          E            findett     2
062900000613     O                                              'Riferimento Strategi.....:'
063000000613     O                       vlrhdl              +1
063100000616     O          E            findett     2
063200000613     O                                              'Giusti...................:'
063300000613     O                       §CTROK        2   +  1
063400000616     O          E            findett     2
063500000613     O                                              'Sbagliati e corretti.....:'
063600000613     O                       §CTRMO        2   +  1
063700000616     O          E            findett     2
063800000613     O                                              'Sbagliati e scartati.....:'
063900000613     O                       §CTRNO        2   +  1
064000000613** CMD - COMANDI CL
064100020321OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV07P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
064200020321OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV07PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
064300020321DLTOVR FILE(TITV07P TITV07PS) LVL(*)
064400010606
064500000613
064600050314** "SERIE" CLIENTE ALFA **
064700050314CF
064800050314** "SERIE" CLIENTE NUMERICA **
06490005031436
