000100060202      /TITLE Upload via Internet: traduzione in FIVABWWR. (x MATTEL Cod. 0390797)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700021014     Ftitv23p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900021014     Ftitv23ps  O    f  198        PRINTER usropn
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
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011113     D depcmd          s            150    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000000313     D*
004100010330
004200010330
004300010330
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100010605     C                   exsr      rwvab
005200010601     C*
005300010601     C                   seton                                        lr
005400010601
005500010601
005600010601
005700010601
005800010601     C*--------------------------------------------------------
005900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006000010601     C*--------------------------------------------------------
006100010601     C     PREELA        BEGSR
006200010601     C*
006300010601     C* SE OCCORRE SPEDIRE IN FILIALE
006400010601     C                   if        invfil <> *zeros and
006500010601     C                             flgGiro = '0'
006600010601     C*
006700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006800010601     C                   eval      flgGiro = '1'
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200010601     C     invfil        chain     azorg01l
007300000724     C                   if        %found
007400000724     C                   movel(p)  CMD(1)        depcmd
007500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000724     C                   PARM                    depcmd
008100000724     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000613     C*
009500021014     C                   if        not %open(titv23ps)
009600021014     C                   open      titv23ps
009700000616     C                   except    testdett
009800000613     C                   endif
009900010601     C*
010000010601     C                   endif
010100010601     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010601     C*--------------------------------------------------------
010800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010900010601     C*--------------------------------------------------------
011000010601     C     ENDELA        BEGSR
011100990915     C*
011200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011300000613     C*
011400021014     C                   if        %open(titv23ps)
011500000616     C                   except    findett
011600021014     C                   close     titv23ps
011700000613     C                   endif
011800000616     C*
011900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    CMD(3)
012300000616     C                   PARM                    LENGH
012400000616     C*
012500010601     C                   ENDSR
012600010601     C***
012700010601
012800010601
012900000613
013000000613     C*--------------------------------------------------------
013100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013200000613     C*--------------------------------------------------------
013300000621     C     STPR          BEGSR
013400000613     C*
013500021014     C                   if        not %open(titv23p)
013600021014     C                   open      titv23p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100021014     C                   if        %open(titv23p)
014200021014     C                   close     titv23p
014300990914     C                   endif
014400000613     C*
014500000613     C                   ENDSR
014600000613     C***
014700000613
014800010601
014900010601
015000010330     C*--------------------------------------------------------
015100021014 xxx C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015200010330     C*--------------------------------------------------------
015300010605     C     RWVAB         BEGSR
015400010330     C*
015500010330     C                   if        not %open(tivin00r)
015600010330     C                   open      tivin00r
015700010330     C                   endif
015800021014     C                   if        not %open(fivabwwr)
015900021014     C                   open      fivabwwr
016000010330     C                   endif
016100010330     C*
016200010604     C                   clear                   §CTROK
016300010604     C                   clear                   §CTRMO
016400010604     C                   clear                   §CTRNO
016500010330     C*
016600010330     C                   DO        *HIVAL
016700010330     C*
016800010330     C                   READ      tivin00r                               70
016900010618     C*
017000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017100010618     C                   if        vinflg = '1'
017200010618     C                   eval      flgOk = '1'
017300010618     C                   endif
017400010618     C*
017500010330     C                   if        vindta > *blanks
017600010330     C                   add       1             rrnum
017700010330     C*
017800010601     C                   if        *in70 = *off and
017900010330     C                             (vinflg = *blanks
018000010330     C                              or vinflg = '0'
018100010330     C                              or vinflg = '2')
018200010330     C*
018300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018400010711     C                   if        vinflg = *blanks or vinflg = '0'
018500010711     C                   clear                   vinmsg
018600010711     C                   endif
018700010601     C*
018800010330     C                   exsr      inzvar
018900010330     C                   exsr      defcam
019000010605     C                   exsr      impvab
019100010601     C*
019200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019300010605     C                   eval      depfil = VABLNP
019400010601     C                   exsr      repfil
019500010601     C                   if        depfil = invfil
019600021025     C                   if        vlrpoi = 999
019700021025     C                   MOVE(P)   invfil        VABFGS
019800021025     C                   else
019900021025     C                   MOVE(P)   vlrpoi        VABFGS
020000021025     C                   endif
020100010601     C*
020200010601     C                   exsr      PREELA
020300010601     C*
020400010604     C* Ebbene...
020500010604     C*
020600010604     C  N31              ADD       1             §CTROK            7 0
020700010604     C   32              ADD       1             §CTRMO            7 0
020800010604     C   31              ADD       1             §CTRNO            7 0
020900020205     C*
021000020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
021100021014     C  N31              WRITE     FIVAB000
021200010604     C*
021300020722     C                   endif
021400020722     C*
021500010604     C                   if        *in31 = *off and
021600010604     C                             *in32 = *off
021700010604     C                   eval      vinflg = '1'
021800010604     C                   else
021900010604     C                   eval      recko = vindta
022000010604     C                   except    rigadett
022100010604     C                   eval      vinflg = '2'
022200010604     C                   endif
022300010604     C                   endif
022400010604     C*
022500010330     C                   else
022600010330     C                   eval      vinflg = '1'
022700010330     C                   endif
022800010601     C*
022900010601     C  N70              update    tivin000
023000010330     C*
023100010330     C  N70              ENDdo
023200010601     C
023300010601     C*
023400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023500010601     C                   if        cntNonEl = *zeros or
023600010601     C                             flgMulti = '0'
023700010330     C* Se non ci sono record con errori ...
023800010601     C                   if        §ctrno = 0 and
023900010604     C                             §ctrmo = 0 and
024000010601     C                             flgStato <> '2'
024100010330     C* ... restituisco esito OK.
024200010330     C                   eval      wrkesito = '0'
024300010330     C                   else
024400010330     C                   if        §ctrok > 0
024500010330     C                   eval      wrkesito = '1'
024600010330     C                   else
024700010615     C                   if        flgOk = '0'
024800010615     C                   eval      wrkesito = '2'
024900010615     C                   else
025000010615     C                   eval      wrkesito = '6'
025100010615     C                   endif
025200010330     C                   endif
025300010330     C                   endif
025400010601     C                   else
025500010601     C                   eval      wrkesito = '9'
025600010601     C                   endif
025700010330     C*
025800010330     C                   if        %open(tivin00r)
025900010330     C                   close     tivin00r
026000010330     C                   endif
026100021014     C                   if        %open(fivabwwr)
026200021014     C                   close     fivabwwr
026300010330     C                   endif
026400010601     C*
026500010601     C                   if        vlrpoi <> 999
026600010601     C                   eval      invfil = vlrpoi
026700010601     C                   endif
026800010330     C*
026900010330     C                   if        §ctrok > 0
027000010601     C                             and invfil > *zeros
027100010330     C                   exsr      invio
027200010330     C                   endif
027300010601     C*
027400010618     C                   if        flgGiro = '1'
027500010601     C                   exsr      endela
027600010618     C                   endif
027700010330     C*
027800010330     C                   ENDSR
027900010330     C***
028000010330
028100010601
028200010601
028300010601
028400010601
028500010330     C*----------------------------------------------------*
028600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028700010330     C*----------------------------------------------------*
028800010330     C     INZVAR        BEGSR
028900010330     C*
029000020204     C                   Z-ADD     *zeros        Num5_0            5 0
029100020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029200020322     C                   MOVEL     '0'           FlgCAS            1
029300010330     C*
029400010330     C                   ENDSR
029500010330     C*----------------------------------------------------*
029600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029700010330     C*----------------------------------------------------*
029800010330     C     DEFCAM        BEGSR
029900010330     C*
030000020204     C* Inizializzo il buffer del record da scrivere
030100021014     C                   CLEAR                   FIVAB000
030200020204     C* Imposto i valori di default...
030300060202     C                   EVAL      VABCCM = 0390797
030400060202     C                   EVAL      VABLNP = 039
030500021014     C                   EVAL      VABCTR = 500
030600021023     C                   EVAL      VABCTM = 'DT'
030700020204     C* ... e poi verifico se sono stati passati come parametri
030800020204     C                   IF        vlrppt > *blanks
030900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031000020204     C                   EXSR      CHKNUM
031100020204     C                   IF        PiInt=*on
031200020204     C                   Z-ADD     PiVal         VABCCM
031300020204     C                   ENDIF
031400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031500020204     C                   EXSR      CHKNUM
031600020204     C                   IF        PiInt=*on
031700020204     C                   Z-ADD     PiVal         VABLNP
031800020204     C                   ENDIF
031900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032000020204     C                   EXSR      CHKNUM
032100020204     C                   IF        PiInt=*on
032200020204     C                   Z-ADD     PiVal         VABCTR
032300020204     C                   ENDIF
032400020204     C                   ENDIF
032500020204     C*
032600010330     C                   ENDSR
032700010607     C*----------------------------------------------------*
032800021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032900010607     C*----------------------------------------------------*
033000010607     C     IMPVAB        BEGSR
033100010607     C*
033200010607     C                   SETOFF                                       3132
033300010607     C*
033400010607     C* Reperimento campi ALFA
033500010607     C*
033600010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
033700021014     C                   EVAL      VABRSD=%trim(%subst(vindta:193:30))
033800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034000020117     C* ==
034100021014     C                   EVAL      VABIND=%trim(%subst(vindta:223:27))
034200021014     C                   EVAL      VABLOD=%trim(%subst(vindta:253:22))
034300021014     C                   EVAL      VABPRD=%trim(%subst(vindta:283:2))
034400021023     C                   EVAL      VABRMA=%trim(%subst(vindta:36:5))
034500021014     C                   EVAL      VABNOT=%trim(%subst(vindta:442:40))
034600010607     C*
034700010607     C* Reperimento campi NUMERICI
034800020722     C                   MOVEL     DATCOR        VABAAS
034900020722     C                   MOVE      DATCOR        VABMGS
035000020722     C*
035100031126     C                   EVAL      PiStr=%trim(%subst(vindta:34:7))
035200020304     C                   EXSR      CHKNUM
035300020304     C                   IF        PiInt=*on
035400020304     C                   Z-ADD     PiVal         VABNSP
035500020304     C                   ELSE
035600020722     C                   SETON                                        31
035700020304     C                   Z-ADD     *zeros        VABNSP
035800020304     C                   EVAL      vinmsg = %trimr(vinmsg)
035900021023     C                             + ' ' + 'VABNSP'
036000020304     C                   ENDIF
036100021023     C*
036200021023     C                   EVAL      PiStr=%trim(%subst(vindta:9:8))
036300021023     C                   EXSR      CHKNUM
036400021023     C                   IF        PiInt=*on
036500021023     C                   Z-ADD     PiVal         VABRMN
036600021023     C                   ELSE
036700021023     C                   SETON                                        31
036800021023     C                   Z-ADD     *zeros        VABRMN
036900021023     C                   EVAL      vinmsg = %trimr(vinmsg)
037000021023     C                             + ' ' + 'VABRMN'
037100021023     C                   ENDIF
037200020304     C*
037300021014     C                   EVAL      PiStr=%trim(%subst(vindta:286:5))
037400010607     C                   EXSR      CHKNUM
037500010607     C                   IF        PiInt=*on
037600020204     C                   Z-ADD     PiVal         Num5_0
037700020204     C                   MOVEL(p)  Num5_0        VABCAD
037800010607     C                   ELSE
037900010607     C                   SETON                                        32
038000020204     C                   EVAL      VABCAD = *zeros
038100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038200020204     C                             + ' ' + 'VABCAD'
038300010607     C                   ENDIF
038400010607     C*
038500021014     C                   EVAL      PiStr=%trim(%subst(vindta:328:3))
038600010607     C                   EXSR      CHKNUM
038700010607     C                   IF        PiInt=*on
038800010607     C                   Z-ADD     PiVal         VABNCL
038900010607     C                   ELSE
039000010607     C                   SETON                                        32
039100010607     C                   Z-ADD     *zeros        VABNCL
039200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039300010607     C                             + ' ' + 'VABNCL'
039400010607     C                   ENDIF
039500020304     C*
039600021014     C                   EVAL      PiStr=%trim(%subst(vindta:318:7))
039700020204     C                   EVAL      PiDecChr = ','
039800010607     C                   EXSR      CHKNUM
039900010607     C                   IF        PiNum=*on
040000020722     C                   EVAL      VABPKB = PiVal
040100010607     C                   ELSE
040200010607     C                   SETON                                        32
040300010607     C                   Z-ADD     *zeros        VABPKB
040400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040500010607     C                             + ' ' + 'VABPKB'
040600010607     C                   ENDIF
040700021014     C*
040800021014     C                   EVAL      PiStr=%trim(%subst(vindta:335:5))
040900021014     C                   EVAL      PiDecChr = ','
041000021014     C                   EXSR      CHKNUM
041100021014     C                   IF        PiNum=*on
041200040923     C                   EVAL      VABVLB = PiVal
041300021023     C                   EVAL      VABQFT = PiVal
041400021014     C                   ELSE
041500021014     C                   SETON                                        32
041600021014     C                   Z-ADD     *zeros        VABVLB
041700021023     C                   Z-ADD     *zeros        VABQFT
041800021014     C                   EVAL      vinmsg = %trimr(vinmsg)
041900021023     C                             + ' ' + 'VABVLB VABQFT'
042000021014     C                   ENDIF
042100020322     C*
042200020322     C* Considerazioni finali su CBO/CAS
042300020322     C                   IF        FlgCAS = '1'
042400020322     C                   EVAL      VABCBO = '4'
042500020322     C                   ELSE
042600020322     C                   EVAL      VABCBO = '1'
042700020322     C                   ENDIF
042800020204     C*
042900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043000020204     C                   EXSR      CHKIMPDIV
043100020204     C*
043200010607     C                   ENDSR
043300010607     C*----------------------------------------------------*
043400010601
043500020204
043600020204     C*----------------------------------------------------*
043700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043800020204     C*----------------------------------------------------*
043900020204     C     CHKIMPDIV     BEGSR
044000020204     C*
044100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044200020204     C                   Z-ADD     *zeros        wrkDec            9 9
044300020204     C*
044400020204     C* Come prima cosa effettuo considerazioni sulla divisa
044500020204     C                   IF        vabIAS > *zeros
044600020204     C                   IF        vabVAS <> 'EUR'
044700020204     C                   EVAL      vabVAS =  'ITL'
044800020204     C                   ENDIF
044900020204     C                   ENDIF
045000020204     C*
045100020204     C                   IF        vabCAS > *zeros
045200020204     C                   IF        vabVCA <> 'EUR'
045300020204     C                   EVAL      vabVCA =  'ITL'
045400020204     C                   ENDIF
045500020204     C                   ENDIF
045600020204     C*
045700020204     C                   IF        vabVMD > *zeros
045800020321     C                   IF        vabVAD <> 'EUR'
045900020204     C                   EVAL      vabVAD =  'ITL'
046000020204     C                   ENDIF
046100020204     C                   ENDIF
046200020204     C*
046300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046400020204     C                   Z-ADD     vabIAS        wrkDec
046500020204     C                   IF        wrkDec > *zeros
046600020204     C                   IF        vabVAS = 'ITL'
046700020204     C                   EVAL      vabIAS = *zeros
046800020204     C                   ENDIF
046900020204     C                   ENDIF
047000020204     C*
047100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047200020204     C                   Z-ADD     vabCAS        wrkDec
047300020204     C                   IF        wrkDec > *zeros
047400020204     C                   IF        vabVCA = 'ITL'
047500020204     C                   EVAL      vabCAS = *zeros
047600020204     C                   ENDIF
047700020204     C                   ENDIF
047800020204     C*
047900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048000020204     C                   Z-ADD     vabVMD        wrkDec
048100020204     C                   IF        wrkDec > *zeros
048200020204     C                   IF        vabVAD = 'ITL'
048300020204     C                   EVAL      vabVMD = *zeros
048400020204     C                   ENDIF
048500020204     C                   ENDIF
048600020204     C*
048700020204     C                   ENDSR
048800020204     C***
048900020204
049000010330
049100010330
049200010330     C*----------------------------------------------------*
049300010330     C*  CONTROLLO NUMERICITA' CAMPI
049400010330     C*----------------------------------------------------*
049500010330     C     CHKNUM        BEGSR
049600010330     C*
049700010606     C                   IF        PiDecChr = *blanks
049800010606     C                   EVAL      PiDecChr = ','
049900010606     C                   ENDIF
050000010606     C*
050100010606     C                   CALL(e)   'ISNUMERIC'
050200010330     C                   PARM                    PiStr            30
050300010606     C                   PARM                    PiDecChr          1
050400010330     C                   PARM      *ZEROS        PiVal            30 9
050500010330     C                   PARM      '0'           PiInt             1
050600010330     C                   PARM      '0'           PiNum             1
050700010330     C                   IF        %error
050800010606     C                   EVAL      PiNum=*off
050900010330     C                   ENDIF
051000010330     C*
051100010330     C                   ENDSR
051200010330     C***
051300010330
051400010601
051500010601
051600010601
051700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051800010601     C     repfil        BEGSR
051900010601     C*
052000010601     C                   if        invfil = *zeros and
052100010601     C                             depfil > *zeros and
052200010629     C                             (vinflg = *blanks or
052300010629     C                              vinflg = *zeros)
052400010601     C
052500010601     C                   eval      invfil = depfil
052600010601     C                   endif
052700010601     C*
052800010601     C                   if        depfil <> invfil and
052900010601     C                             invfil > *zeros
053000010601     C                   eval      flgMulti = '1'
053100010601     C                   if        vinflg = *blanks
053200010601     C                   add       1             cntNonEl
053300010601     C                   endif
053400010601     C                   endif
053500010601     C*
053600010601     C                   if        vinflg = '2'
053700010601     C                   eval      flgStato = '2'
053800010601     C                   endif
053900010601     C*
054000010601     C                   ENDSR
054100010601     C***
054200010601
054300010601
054400010601
054500010330
054600010330
054700010330
054800990920      /TITLE Invio dei dati al punto operativo.
054900000613     C     invio         BEGSR
055000990920     C*
055100990920     C                   reset                   dscmz
055200010601     C                   move      invfil        cmzdst
055300990920     C                   eval      cmzfld = vlrfou
055400990920     C                   eval      cmzmbd = vlrhdl
055500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
055600000710     C                   if        prmfir = *blanks
055700021014     C                   eval      cmzfla = 'FIVAB00F'
055800021014     C                   eval      cmzmba = 'FIVAB00F'
055900000710     C                   else
056000000710     C                   eval      cmzfla = prmfir
056100000710     C                   eval      cmzmba = prmfir
056200000710     C                   endif
056300990920     C                   eval      cmznrr = *zeros
056400990920     C                   move      §ctrok        cmznrr
056500021018     C                   eval      cmzlba = vlrfl1
056600990920     C                   call(e)   'TIS711C'
056700990920     C                   parm                    dscmz
056800990921     C                   parm      *blanks       esito
056900990920     C                   if        %error
057000990920     C                             or cmzerr = '1'
057100990921     C                             or esito  = '1'
057200000710     C                   eval      wrkesito = '3'
057300990920     C                   endif
057400990920     C*
057500000613     C                   ENDSR
057600990910
057700010601
057800010601
057900010601
058000010601
058100010601      /TITLE Invio dei dati al punto operativo.
058200010601     C     opeini        BEGSR
058300010601     C*
058400010601     C* Inizializzo flag e contatori operativi
058500010601     C                   movel     '0'           flgGiro           1
058600010601     C                   movel     '0'           flgMulti          1
058700010601     C                   movel     '1'           flgStato          1
058800010615     C                   movel     '0'           flgOk             1
058900010601     C                   z-add     *zeros        cntNonEl         10 0
059000010601     C                   z-add     *zeros        depfil            3 0
059100010601     C                   z-add     *zeros        invfil            3 0
059200010601     C*
059300010601     C                   ENDSR
059400010601     C***
059500010601
059600010601
059700010601
059800010330
059900010330
060000000613     C     *inzsr        BEGSR
060100990910     C*
060200990910     C     *entry        plist
060300990920     C                   parm                    tivlrds
060400990921     C                   parm      wrkesito      esito
060500000724     C                   parm                    prmlit
060600000710     C                   parm                    prmfir
060700010330     C*
060800010330     C* CALCOLA LA DATA CORRENTE
060900010330     C                   time                    wn14             14 0
061000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061100010330     C                   z-add     wn8           g08dat
061200010330     C                   z-add     *zeros        g08inv
061300010330     C                   movel     '0'           g08err
061400010330     C                   call      'XSRDA8'
061500010330     C                   parm                    wlbda8
061600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061700000613     C*
061800000613     C                   ENDSR
061900000613     C***
062000990908
062100021014     Otitv23p   E            riepilogo         2
062200990915     O                                              'Upload via Internet'
062300990915     O                                           +1 'Traduzione TIVIN00R -'
062400021014     O                                           +1 'FIVABWWR'
062500990915     O                                           +1 'Testate bolle'
062600990915     O          E            riepilogo   2
062700990915     O                       wrkdata
062800990915     O                       wrkora              +1
062900990915     O                       procname            +1
063000990915     O          E            riepilogo   2
063100990915     O                                              'Cliente..................:'
063200010605     O                       VABCCM        z     +1
063300990915     O          E            riepilogo   2
063400990920     O                                              'Riferimento Strategi.....:'
063500990920     O                       vlrhdl              +1
063600990915     O          E            riepilogo   2
063700990915     O                                              'Giusti...................:'
063800971022     O                       §CTROK        2   +  1
063900990915     O          E            riepilogo   2
064000990915     O                                              'Sbagliati e corretti.....:'
064100971022     O                       §CTRMO        2   +  1
064200990915     O          E            riepilogo   2
064300990915     O                                              'Sbagliati e scartati.....:'
064400971022     O                       §CTRNO        2   +  1
064500000613
064600021014     Otitv23ps  E            testdett          2
064700000613     O                                              'Upload via Internet'
064800000613     O                                           +1 'Traduzione TIVIN00R -'
064900021014     O                                           +1 'FIVABWWR'
065000000613     O                                           +1 'Testate bolle'
065100000616     O          E            testdett    3
065200000613     O                                           +2 'N° rec'
065300000613     O                                           +3 'Anteprima contenuto'
065400000616     O          E            rigadett    2
065500000613     O                       rrnum               +2
065600000621     O                       recko               +3
065700000616     O          E            findett     2
065800000613     O                       wrkdata
065900000613     O                       wrkora              +1
066000000613     O                       procname            +1
066100000616     O          E            findett     2
066200000613     O                                              'Cliente..................:'
066300010605     O                       VABCCM        z     +1
066400000616     O          E            findett     2
066500000613     O                                              'Riferimento Strategi.....:'
066600000613     O                       vlrhdl              +1
066700000616     O          E            findett     2
066800000613     O                                              'Giusti...................:'
066900000613     O                       §CTROK        2   +  1
067000000616     O          E            findett     2
067100000613     O                                              'Sbagliati e corretti.....:'
067200000613     O                       §CTRMO        2   +  1
067300000616     O          E            findett     2
067400000613     O                                              'Sbagliati e scartati.....:'
067500000613     O                       §CTRNO        2   +  1
067600000613** CMD - COMANDI CL
067700021014OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV23P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
067800021014OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV23PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
067900021014DLTOVR FILE(TITV23P TITV23PS) LVL(*)
068000010606
068100000613
