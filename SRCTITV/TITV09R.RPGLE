000100021025      /TITLE Upload via Internet: traduzione in FIVABWWR. (x NOTTINTON Cod. 0591254)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700020329     Ftitv09p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900020329     Ftitv09ps  O    f  198        PRINTER usropn
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
009500020329     C                   if        not %open(titv09ps)
009600020329     C                   open      titv09ps
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
010700010601
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011300010601     C*--------------------------------------------------------
011400010601     C     ENDELA        BEGSR
011500990915     C*
011600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011700000613     C*
011800020329     C                   if        %open(titv09ps)
011900000616     C                   except    findett
012000020329     C                   close     titv09ps
012100000613     C                   endif
012200000616     C*
012300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012400011113     C                   Z-ADD     150           LENGH            15 5
012500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    CMD(3)
012700000616     C                   PARM                    LENGH
012800000616     C*
012900010601     C                   ENDSR
013000010601     C***
013100010601
013200010601
013300010601
013400010601
013500000613
013600000613     C*--------------------------------------------------------
013700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013800000613     C*--------------------------------------------------------
013900000621     C     STPR          BEGSR
014000000613     C*
014100020329     C                   if        not %open(titv09p)
014200020329     C                   open      titv09p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700020329     C                   if        %open(titv09p)
014800020329     C                   close     titv09p
014900990914     C                   endif
015000000613     C*
015100000613     C                   ENDSR
015200000613     C***
015300000613
015400010330
015500010601
015600010601
015700010601
015800010330     C*--------------------------------------------------------
015900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600021025     C                   if        not %open(fivabwwr)
016700021025     C                   open      fivabwwr
016800010330     C                   endif
016900010330     C*
017000010604     C                   clear                   §CTROK
017100010604     C                   clear                   §CTRMO
017200010604     C                   clear                   §CTRNO
017300010330     C*
017400010330     C                   DO        *HIVAL
017500010330     C*
017600010330     C                   READ      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200010618     C*
018300010330     C                   if        vindta > *blanks
018400010330     C                   add       1             rrnum
018500010330     C*
018600010601     C                   if        *in70 = *off and
018700010330     C                             (vinflg = *blanks
018800010330     C                              or vinflg = '0'
018900010330     C                              or vinflg = '2')
019000010330     C*
019100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019200010711     C                   if        vinflg = *blanks or vinflg = '0'
019300010711     C                   clear                   vinmsg
019400010711     C                   endif
019500010601     C*
019600010330     C                   exsr      inzvar
019700010330     C                   exsr      defcam
019800010605     C                   exsr      impvab
019900010601     C*
020000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020100010605     C                   eval      depfil = VABLNP
020200010601     C                   exsr      repfil
020300010601     C                   if        depfil = invfil
020400021025     C                   if        vlrpoi = 999
020500021025     C                   MOVE(P)   invfil        VABFGS
020600021025     C                   else
020700021025     C                   MOVE(P)   vlrpoi        VABFGS
020800021025     C                   endif
020900010601     C*
021000010601     C                   exsr      PREELA
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             §CTROK            7 0
021500010604     C   32              ADD       1             §CTRMO            7 0
021600010604     C   31              ADD       1             §CTRNO            7 0
021700020205     C*
021800020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
021900021025     C  N31              WRITE     FIVAB000
022000010604     C*
022100010604     C                   if        *in31 = *off and
022200010604     C                             *in32 = *off
022300010604     C                   eval      vinflg = '1'
022400010604     C                   else
022500010604     C                   eval      recko = vindta
022600010604     C                   except    rigadett
022700010604     C                   eval      vinflg = '2'
022800010604     C                   endif
022900010604     C                   endif
023000010601     C*
023100010604     C                   endif
023200010604     C*
023300010330     C                   else
023400010330     C                   eval      vinflg = '1'
023500010330     C                   endif
023600010601     C*
023700010601     C  N70              update    tivin000
023800010330     C*
023900010330     C  N70              ENDdo
024000010601     C
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
026900021025     C                   if        %open(fivabwwr)
027000021025     C                   close     fivabwwr
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
028800010330
028900010601
029000010601
029100010601
029200010601
029300010330     C*----------------------------------------------------*
029400010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029500010330     C*----------------------------------------------------*
029600010330     C     INZVAR        BEGSR
029700010330     C*
029800020204     C                   Z-ADD     *zeros        Num5_0            5 0
029900020213     C                   Z-ADD     *zeros        Num15_3          15 3
030000020204     C                   MOVEL     *blanks       WrkNote          70
030100020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030200020322     C                   MOVEL     '0'           FlgCAS            1
030300010330     C*
030400010330     C                   ENDSR
030500010330     C*----------------------------------------------------*
030600020322 xxx C*  IMPOSTAZIONE CAMPI COSTANTI
030700010330     C*----------------------------------------------------*
030800010330     C     DEFCAM        BEGSR
030900010330     C*
031000020204     C* Inizializzo il buffer del record da scrivere
031100021025     C                   CLEAR                   FIVAB000
031200020204     C* Imposto i valori di default...
031300020403     C                   EVAL      VABCCM = 0591254
031400020403     C                   EVAL      VABCTM = '7Q'
031500020329     C                   EVAL      VABLNP = 059
031600020322     C                   EVAL      VABCTR = 000
031700020204     C* ... e poi verifico se sono stati passati come parametri
031800020204     C                   IF        vlrppt > *blanks
031900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032000020204     C                   EXSR      CHKNUM
032100020204     C                   IF        PiInt=*on
032200020204     C                   Z-ADD     PiVal         VABCCM
032300020204     C                   ENDIF
032400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032500020204     C                   EXSR      CHKNUM
032600020204     C                   IF        PiInt=*on
032700020204     C                   Z-ADD     PiVal         VABLNP
032800020204     C                   ENDIF
032900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033000020204     C                   EXSR      CHKNUM
033100020204     C                   IF        PiInt=*on
033200020204     C                   Z-ADD     PiVal         VABCTR
033300020204     C                   ENDIF
033400020204     C                   ENDIF
033500020204     C*
033600010330     C                   ENDSR
033700010607     C*----------------------------------------------------*
033800021025 xxx C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033900010607     C*----------------------------------------------------*
034000010607     C     IMPVAB        BEGSR
034100010607     C*
034200010607     C                   SETOFF                                       3132
034300010607     C*
034400010607     C* Reperimento campi ALFA
034500010607     C*
034600010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034700020329     C                   EVAL      VABRSD=%trim(%subst(vindta:186:30))
034800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035000020117     C* ==
035100020329     C                   EVAL      VABRD2=%trim(%subst(vindta:216:30))
035200020329     C                   EVAL      VABIND=%trim(%subst(vindta:246:30))
035300020329     C                   EVAL      VABLOD=%trim(%subst(vindta:323:30))
035400020329     C                   EVAL      VABPRD=%trim(%subst(vindta:353:2))
035500020329     C                   EVAL      VABNZD=%trim(%subst(vindta:355:3))
035600020403     C                   IF        %trim(VABNZD)='IT'
035700020403     C                   EVAL      VABNZD=*BLANKS
035800020403     C                   ENDIF
035900020329     C                   EVAL      VABRMA=%trim(%subst(vindta:367:5))
036000020329     C                   EVAL      VABNOT=%trim(%subst(vindta:901:50))
036100020329     C*
036200020329     C* Verifico se nel record nn c'è solo 1 DDT
036300020329     C                   IF        %trim(%subst(vindta:401:5)) > *ZEROS
036400020329     C                   SETON                                        32
036500020329     C                   EVAL      vinmsg = %trimr(vinmsg)
036600020329     C                             + ' ' + '+ DDT SUL RECORD'
036700020329     C                   ENDIF
036800020329     C*
036900020329     C                   MOVEL     DATCOR        VABAAS
037000020329     C                   MOVE      DATCOR        VABMGS
037100010607     C*
037200010607     C* Reperimento campi NUMERICI
037300030326     C                   EVAL      PiStr=%trim(%subst(vindta:178:6)+
037400030326     C                                         %subst(vindta:185:1))
037500020304     C                   EXSR      CHKNUM
037600020304     C                   IF        PiInt=*on
037700020304     C                   Z-ADD     PiVal         VABNSP
037800020304     C                   ELSE
037900020304     C                   SETON                                        32
038000020304     C                   Z-ADD     *zeros        VABNSP
038100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
038200030326     C                             + ' ' + 'VABNSP'
038300020304     C                   ENDIF
038400030326     C*
038500030326     C                   EVAL      PiStr=%trim(%subst(vindta:367:5))
038600030326     C                   EXSR      CHKNUM
038700030326     C                   IF        PiInt=*on
038800030326     C                   Z-ADD     PiVal         VABRMN
038900030326     C                   ELSE
039000030326     C                   SETON                                        32
039100030326     C                   Z-ADD     *zeros        VABRMN
039200030326     C                   EVAL      vinmsg = %trimr(vinmsg)
039300030326     C                             + ' ' + 'VABRMN'
039400030326     C                   ENDIF
039500020304     C*
039600020329     C                   EVAL      PiStr=%trim(%subst(vindta:306:5))
039700010607     C                   EXSR      CHKNUM
039800010607     C                   IF        PiInt=*on
039900020204     C                   Z-ADD     PiVal         Num5_0
040000020204     C                   MOVEL(p)  Num5_0        VABCAD
040100010607     C                   ELSE
040200010607     C                   SETON                                        32
040300020204     C                   EVAL      VABCAD = *zeros
040400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040500020204     C                             + ' ' + 'VABCAD'
040600010607     C                   ENDIF
040700010607     C*
040800020329     C                   EVAL      PiStr=%trim(%subst(vindta:868:5))
040900010607     C                   EXSR      CHKNUM
041000010607     C                   IF        PiInt=*on
041100010607     C                   Z-ADD     PiVal         VABNCL
041200010607     C                   ELSE
041300010607     C                   SETON                                        32
041400010607     C                   Z-ADD     *zeros        VABNCL
041500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041600010607     C                             + ' ' + 'VABNCL'
041700010607     C                   ENDIF
041800020304     C*
041900020329     C                   EVAL      PiStr=%trim(%subst(vindta:873:9))
042000020204     C                   EVAL      PiDecChr = ','
042100010607     C                   EXSR      CHKNUM
042200010607     C                   IF        PiNum=*on
042300020329     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
042400020329     C* riferisce ad un campo 9,3
042500020329     C                   EVAL(H)   VABPKB = PiVal/1000
042600010607     C                   ELSE
042700010607     C                   SETON                                        32
042800010607     C                   Z-ADD     *zeros        VABPKB
042900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043000010607     C                             + ' ' + 'VABPKB'
043100010607     C                   ENDIF
043200020304     C*
043300020329     C                   EVAL      PiStr=%trim(%subst(vindta:891:9))
043400020304     C                   EVAL      PiDecChr = ','
043500020304     C                   EXSR      CHKNUM
043600020304     C                   IF        PiNum=*on
043700020329     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
043800020329     C* riferisce ad un campo 9,3
043900021003     C                   EVAL(H)   VABVLB = PiVal/1000
044000020304     C                   ELSE
044100020304     C                   SETON                                        32
044200020329     C                   Z-ADD     *zeros        VABVLB
044300020304     C                   EVAL      vinmsg = %trimr(vinmsg)
044400020322     C                             + ' ' + 'VABVLB'
044500020304     C                   ENDIF
044600020322     C*
044700020322     C* Considerazioni finali su CBO/CAS
044800020322     C                   IF        FlgCAS = '1'
044900020322     C                   EVAL      VABCBO = '4'
045000020322     C                   ELSE
045100020322     C                   EVAL      VABCBO = '1'
045200020322     C                   ENDIF
045300020204     C*
045400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045500020204     C                   EXSR      CHKIMPDIV
045600020204     C*
045700010607     C                   ENDSR
045800010607     C*----------------------------------------------------*
045900010601
046000020204
046100020204     C*----------------------------------------------------*
046200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046300020204     C*----------------------------------------------------*
046400020204     C     CHKIMPDIV     BEGSR
046500020204     C*
046600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046700020204     C                   Z-ADD     *zeros        wrkDec            9 9
046800020204     C*
046900020204     C* Come prima cosa effettuo considerazioni sulla divisa
047000020204     C                   IF        vabIAS > *zeros
047100020204     C                   IF        vabVAS <> 'EUR'
047200020204     C                   EVAL      vabVAS =  'ITL'
047300020204     C                   ENDIF
047400020204     C                   ENDIF
047500020204     C*
047600020204     C                   IF        vabCAS > *zeros
047700020204     C                   IF        vabVCA <> 'EUR'
047800020204     C                   EVAL      vabVCA =  'ITL'
047900020204     C                   ENDIF
048000020204     C                   ENDIF
048100020204     C*
048200020204     C                   IF        vabVMD > *zeros
048300020321     C                   IF        vabVAD <> 'EUR'
048400020204     C                   EVAL      vabVAD =  'ITL'
048500020204     C                   ENDIF
048600020204     C                   ENDIF
048700020204     C*
048800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048900020204     C                   Z-ADD     vabIAS        wrkDec
049000020204     C                   IF        wrkDec > *zeros
049100020204     C                   IF        vabVAS = 'ITL'
049200020204     C                   EVAL      vabIAS = *zeros
049300020204     C                   ENDIF
049400020204     C                   ENDIF
049500020204     C*
049600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049700020204     C                   Z-ADD     vabCAS        wrkDec
049800020204     C                   IF        wrkDec > *zeros
049900020204     C                   IF        vabVCA = 'ITL'
050000020204     C                   EVAL      vabCAS = *zeros
050100020204     C                   ENDIF
050200020204     C                   ENDIF
050300020204     C*
050400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050500020204     C                   Z-ADD     vabVMD        wrkDec
050600020204     C                   IF        wrkDec > *zeros
050700020204     C                   IF        vabVAD = 'ITL'
050800020204     C                   EVAL      vabVMD = *zeros
050900020204     C                   ENDIF
051000020204     C                   ENDIF
051100020204     C*
051200020204     C                   ENDSR
051300020204     C***
051400020204
051500010330
051600010330
051700010330     C*----------------------------------------------------*
051800010330     C*  CONTROLLO NUMERICITA' CAMPI
051900010330     C*----------------------------------------------------*
052000010330     C     CHKNUM        BEGSR
052100010330     C*
052200010606     C                   IF        PiDecChr = *blanks
052300010606     C                   EVAL      PiDecChr = ','
052400010606     C                   ENDIF
052500010606     C*
052600010606     C                   CALL(e)   'ISNUMERIC'
052700010330     C                   PARM                    PiStr            30
052800010606     C                   PARM                    PiDecChr          1
052900010330     C                   PARM      *ZEROS        PiVal            30 9
053000010330     C                   PARM      '0'           PiInt             1
053100010330     C                   PARM      '0'           PiNum             1
053200010330     C                   IF        %error
053300010606     C                   EVAL      PiNum=*off
053400010330     C                   ENDIF
053500010330     C*
053600010330     C                   ENDSR
053700010330     C***
053800010330
053900010601
054000010601
054100010601
054200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
054300010601     C     repfil        BEGSR
054400010601     C*
054500010601     C                   if        invfil = *zeros and
054600010601     C                             depfil > *zeros and
054700010629     C                             (vinflg = *blanks or
054800010629     C                              vinflg = *zeros)
054900010601     C
055000010601     C                   eval      invfil = depfil
055100010601     C                   endif
055200010601     C*
055300010601     C                   if        depfil <> invfil and
055400010601     C                             invfil > *zeros
055500010601     C                   eval      flgMulti = '1'
055600010601     C                   if        vinflg = *blanks
055700010601     C                   add       1             cntNonEl
055800010601     C                   endif
055900010601     C                   endif
056000010601     C*
056100010601     C                   if        vinflg = '2'
056200010601     C                   eval      flgStato = '2'
056300010601     C                   endif
056400010601     C*
056500010601     C                   ENDSR
056600010601     C***
056700010601
056800010601
056900010601
057000010330
057100010330
057200010330
057300990920      /TITLE Invio dei dati al punto operativo.
057400000613     C     invio         BEGSR
057500990920     C*
057600990920     C                   reset                   dscmz
057700010601     C                   move      invfil        cmzdst
057800990920     C                   eval      cmzfld = vlrfou
057900990920     C                   eval      cmzmbd = vlrhdl
058000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
058100000710     C                   if        prmfir = *blanks
058200021025     C                   eval      cmzfla = 'FIVAB00F'
058300021025     C                   eval      cmzmba = 'FIVAB00F'
058400000710     C                   else
058500000710     C                   eval      cmzfla = prmfir
058600000710     C                   eval      cmzmba = prmfir
058700000710     C                   endif
058800990920     C                   eval      cmznrr = *zeros
058900990920     C                   move      §ctrok        cmznrr
059000021018     C                   eval      cmzlba = vlrfl1
059100990920     C                   call(e)   'TIS711C'
059200990920     C                   parm                    dscmz
059300990921     C                   parm      *blanks       esito
059400990920     C                   if        %error
059500990920     C                             or cmzerr = '1'
059600990921     C                             or esito  = '1'
059700000710     C                   eval      wrkesito = '3'
059800990920     C                   endif
059900990920     C*
060000000613     C                   ENDSR
060100990910
060200010601
060300010601
060400010601
060500010601
060600010601      /TITLE Invio dei dati al punto operativo.
060700010601     C     opeini        BEGSR
060800010601     C*
060900010601     C* Inizializzo flag e contatori operativi
061000010601     C                   movel     '0'           flgGiro           1
061100010601     C                   movel     '0'           flgMulti          1
061200010601     C                   movel     '1'           flgStato          1
061300010615     C                   movel     '0'           flgOk             1
061400010601     C                   z-add     *zeros        cntNonEl         10 0
061500010601     C                   z-add     *zeros        depfil            3 0
061600010601     C                   z-add     *zeros        invfil            3 0
061700010601     C*
061800010601     C                   ENDSR
061900010601     C***
062000010601
062100010601
062200010601
062300010330
062400010330
062500000613     C     *inzsr        BEGSR
062600990910     C*
062700990910     C     *entry        plist
062800990920     C                   parm                    tivlrds
062900990921     C                   parm      wrkesito      esito
063000000724     C                   parm                    prmlit
063100000710     C                   parm                    prmfir
063200010330     C*
063300010330     C* CALCOLA LA DATA CORRENTE
063400010330     C                   time                    wn14             14 0
063500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063600010330     C                   z-add     wn8           g08dat
063700010330     C                   z-add     *zeros        g08inv
063800010330     C                   movel     '0'           g08err
063900010330     C                   call      'XSRDA8'
064000010330     C                   parm                    wlbda8
064100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064200000613     C*
064300000613     C                   ENDSR
064400000613     C***
064500990908
064600020329     Otitv09p   E            riepilogo         2
064700990915     O                                              'Upload via Internet'
064800990915     O                                           +1 'Traduzione TIVIN00R -'
064900021025     O                                           +1 'FIVABWWR'
065000990915     O                                           +1 'Testate bolle'
065100990915     O          E            riepilogo   2
065200990915     O                       wrkdata
065300990915     O                       wrkora              +1
065400990915     O                       procname            +1
065500990915     O          E            riepilogo   2
065600990915     O                                              'Cliente..................:'
065700010605     O                       VABCCM        z     +1
065800990915     O          E            riepilogo   2
065900990920     O                                              'Riferimento Strategi.....:'
066000990920     O                       vlrhdl              +1
066100990915     O          E            riepilogo   2
066200990915     O                                              'Giusti...................:'
066300971022     O                       §CTROK        2   +  1
066400990915     O          E            riepilogo   2
066500990915     O                                              'Sbagliati e corretti.....:'
066600971022     O                       §CTRMO        2   +  1
066700990915     O          E            riepilogo   2
066800990915     O                                              'Sbagliati e scartati.....:'
066900971022     O                       §CTRNO        2   +  1
067000000613
067100020329     Otitv09ps  E            testdett          2
067200000613     O                                              'Upload via Internet'
067300000613     O                                           +1 'Traduzione TIVIN00R -'
067400021025     O                                           +1 'FIVABWWR'
067500000613     O                                           +1 'Testate bolle'
067600000616     O          E            testdett    3
067700000613     O                                           +2 'N° rec'
067800000613     O                                           +3 'Anteprima contenuto'
067900000616     O          E            rigadett    2
068000000613     O                       rrnum               +2
068100000621     O                       recko               +3
068200000616     O          E            findett     2
068300000613     O                       wrkdata
068400000613     O                       wrkora              +1
068500000613     O                       procname            +1
068600000616     O          E            findett     2
068700000613     O                                              'Cliente..................:'
068800010605     O                       VABCCM        z     +1
068900000616     O          E            findett     2
069000000613     O                                              'Riferimento Strategi.....:'
069100000613     O                       vlrhdl              +1
069200000616     O          E            findett     2
069300000613     O                                              'Giusti...................:'
069400000613     O                       §CTROK        2   +  1
069500000616     O          E            findett     2
069600000613     O                                              'Sbagliati e corretti.....:'
069700000613     O                       §CTRMO        2   +  1
069800000616     O          E            findett     2
069900000613     O                                              'Sbagliati e scartati.....:'
070000000613     O                       §CTRNO        2   +  1
070100000613** CMD - COMANDI CL
070200020329OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV09P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070300020329OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV09PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070400020329DLTOVR FILE(TITV09P TITV09PS) LVL(*)
070500010606
070600000613
