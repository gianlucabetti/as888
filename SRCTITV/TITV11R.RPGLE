000100090317      /TITLE Upload via Internet: traduzione in FIVABWWR. (x SPECCHIASOL Cod. 0433625)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700020409     Ftitv11p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900020409     Ftitv11ps  O    f  198        PRINTER usropn
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
009500020409     C                   if        not %open(titv11ps)
009600020409     C                   open      titv11ps
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
011800020409     C                   if        %open(titv11ps)
011900000616     C                   except    findett
012000020409     C                   close     titv11ps
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
014100020409     C                   if        not %open(titv11p)
014200020409     C                   open      titv11p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700020409     C                   if        %open(titv11p)
014800020409     C                   close     titv11p
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
030600090317     C*  IMPOSTAZIONE CAMPI COSTANTI
030700010330     C*----------------------------------------------------*
030800010330     C     DEFCAM        BEGSR
030900010330     C*
031000020204     C* Inizializzo il buffer del record da scrivere
031100021025     C                   CLEAR                   FIVAB000
031200020204     C* Imposto i valori di default...
031300020409     C                   EVAL      VABCCM = 0430946
031400020409     C                   EVAL      VABLNP = 043
031500020322     C                   EVAL      VABCTR = 000
031600020204     C* ... e poi verifico se sono stati passati come parametri
031700020204     C                   IF        vlrppt > *blanks
031800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031900020204     C                   EXSR      CHKNUM
032000020204     C                   IF        PiInt=*on
032100020204     C                   Z-ADD     PiVal         VABCCM
032200020204     C                   ENDIF
032300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032400020204     C                   EXSR      CHKNUM
032500020204     C                   IF        PiInt=*on
032600020204     C                   Z-ADD     PiVal         VABLNP
032700020204     C                   ENDIF
032800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032900020204     C                   EXSR      CHKNUM
033000020204     C                   IF        PiInt=*on
033100020204     C                   Z-ADD     PiVal         VABCTR
033200020204     C                   ENDIF
033300020204     C                   ENDIF
033400020204     C*
033500010330     C                   ENDSR
033600010607     C*----------------------------------------------------*
033700090317     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033800010607     C*----------------------------------------------------*
033900010607     C     IMPVAB        BEGSR
034000010607     C*
034100010607     C                   SETOFF                                       3132
034200010607     C*
034300010607     C* Reperimento campi ALFA
034400010607     C*
034500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034600020409     C                   EVAL      VABRSD=%trim(%subst(vindta:5:40))
034700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034900020117     C* ==
035000020409     C                   EVAL      VABIND=%trim(%subst(vindta:45:35))
035100020409     C                   EVAL      VABRD2=' Tel.'+%trim(%subst(vindta:80:15))
035200020409     C                   EVAL      VABLOD=%trim(%subst(vindta:100:30))
035300020409     C                   EVAL      VABPRD=%trim(%subst(vindta:130:2))
035400020409     C                   EVAL      VABNZD=*blanks
035500020409     C                   EVAL      VABRMA=%trim(%subst(vindta:307:6))
035600020410     C                   EVAL      VABVCA='EUR'
035700020409     C*
035800020409     C                   MOVEL     DATCOR        VABAAS
035900020409     C                   MOVE      DATCOR        VABMGS
036000010607     C*
036100010607     C* Reperimento campi NUMERICI
036200020409     C                   EVAL      PiStr=%trim(%subst(vindta:177:6))
036300020304     C                   EXSR      CHKNUM
036400020304     C                   IF        PiInt=*on
036500020409     C                   Z-ADD     PiVal         VABNSP
036600020304     C                   Z-ADD     PiVal         VABRMN
036700020304     C                   ELSE
036800020304     C                   SETON                                        32
036900020304     C                   Z-ADD     *zeros        VABNSP
037000020329     C                   Z-ADD     *zeros        VABRMN
037100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037200020321     C                             + ' ' + 'VABNSP VABRMN'
037300020304     C                   ENDIF
037400020304     C*
037500020409     C                   EVAL      PiStr=%trim(%subst(vindta:95:5))
037600010607     C                   EXSR      CHKNUM
037700010607     C                   IF        PiInt=*on
037800020204     C                   Z-ADD     PiVal         Num5_0
037900020204     C                   MOVEL(p)  Num5_0        VABCAD
038000010607     C                   ELSE
038100010607     C                   SETON                                        32
038200020204     C                   EVAL      VABCAD = *zeros
038300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038400020204     C                             + ' ' + 'VABCAD'
038500010607     C                   ENDIF
038600010607     C*
038700020409     C                   EVAL      PiStr=%trim(%subst(vindta:153:5))
038800010607     C                   EXSR      CHKNUM
038900010607     C                   IF        PiInt=*on
039000010607     C                   Z-ADD     PiVal         VABNCL
039100010607     C                   ELSE
039200010607     C                   SETON                                        32
039300010607     C                   Z-ADD     *zeros        VABNCL
039400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039500010607     C                             + ' ' + 'VABNCL'
039600010607     C                   ENDIF
039700020304     C*
039800020409     C                   EVAL      PiStr=%trim(%subst(vindta:158:8))
039900020204     C                   EVAL      PiDecChr = ','
040000010607     C                   EXSR      CHKNUM
040100010607     C                   IF        PiNum=*on
040200020409     C                   Z-ADD(H)  PiVal         VABPKB
040300010607     C                   ELSE
040400010607     C                   SETON                                        32
040500010607     C                   Z-ADD     *zeros        VABPKB
040600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040700010607     C                             + ' ' + 'VABPKB'
040800010607     C                   ENDIF
040900020304     C*
041000090320     C                   IF        %subst(vindta:268:15) <> *blanks AND
041100090320     C                             %subst(vindta:268:15) <> *all'0'
041200020409     C                   EVAL      PiStr=%trim(%subst(vindta:268:15))
041300020322     C* Detrmino che indipendentemente da giusto o sbagliato l'intenzione del cliente era d
041400020322     C* passare il contrassegno
041500020322     C                   MOVEL     '1'           FlgCAS
041600020322     C*
041700020322     C                   EXSR      CHKNUM
041800020322     C                   IF        PiNum=*on
041900020322     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
042000020409     C* riferisce ad un campo 15,3
042100090318     C***                EVAL(H)   VABCAS = PiVal/1000
042200090318     C                   EVAL      VABCAS = PiVal
042300020322     C                   ELSE
042400020322     C                   SETON                                        32
042500020322     C                   Z-ADD     *zeros        VABCAS
042600020322     C                   EVAL      vinmsg = %trimr(vinmsg)
042700020322     C                             + ' ' + 'VABCAS'
042800020322     C                   ENDIF
042900090320     C                   ENDIF
043000020322     C*
043100020322     C* Considerazioni finali su CBO/CAS
043200020322     C                   IF        FlgCAS = '1'
043300020322     C                   EVAL      VABCBO = '4'
043400020322     C                   ELSE
043500020322     C                   EVAL      VABCBO = '1'
043600020322     C                   ENDIF
043700020204     C*
043800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043900020204     C                   EXSR      CHKIMPDIV
044000020204     C*
044100010607     C                   ENDSR
044200010607     C*----------------------------------------------------*
044300010601
044400020204
044500020204     C*----------------------------------------------------*
044600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044700020204     C*----------------------------------------------------*
044800020204     C     CHKIMPDIV     BEGSR
044900020204     C*
045000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045100020204     C                   Z-ADD     *zeros        wrkDec            9 9
045200020204     C*
045300020204     C* Come prima cosa effettuo considerazioni sulla divisa
045400020204     C                   IF        vabIAS > *zeros
045500020204     C                   IF        vabVAS <> 'EUR'
045600020204     C                   EVAL      vabVAS =  'ITL'
045700020204     C                   ENDIF
045800020204     C                   ENDIF
045900020204     C*
046000020204     C                   IF        vabCAS > *zeros
046100020204     C                   IF        vabVCA <> 'EUR'
046200020204     C                   EVAL      vabVCA =  'ITL'
046300020204     C                   ENDIF
046400020204     C                   ENDIF
046500020204     C*
046600020204     C                   IF        vabVMD > *zeros
046700020321     C                   IF        vabVAD <> 'EUR'
046800020204     C                   EVAL      vabVAD =  'ITL'
046900020204     C                   ENDIF
047000020204     C                   ENDIF
047100020204     C*
047200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047300020204     C                   Z-ADD     vabIAS        wrkDec
047400020204     C                   IF        wrkDec > *zeros
047500020204     C                   IF        vabVAS = 'ITL'
047600020204     C                   EVAL      vabIAS = *zeros
047700020204     C                   ENDIF
047800020204     C                   ENDIF
047900020204     C*
048000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
048100020204     C                   Z-ADD     vabCAS        wrkDec
048200020204     C                   IF        wrkDec > *zeros
048300020204     C                   IF        vabVCA = 'ITL'
048400020204     C                   EVAL      vabCAS = *zeros
048500020204     C                   ENDIF
048600020204     C                   ENDIF
048700020204     C*
048800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048900020204     C                   Z-ADD     vabVMD        wrkDec
049000020204     C                   IF        wrkDec > *zeros
049100020204     C                   IF        vabVAD = 'ITL'
049200020204     C                   EVAL      vabVMD = *zeros
049300020204     C                   ENDIF
049400020204     C                   ENDIF
049500020204     C*
049600020204     C                   ENDSR
049700020204     C***
049800020204
049900010330
050000010330
050100010330     C*----------------------------------------------------*
050200010330     C*  CONTROLLO NUMERICITA' CAMPI
050300010330     C*----------------------------------------------------*
050400010330     C     CHKNUM        BEGSR
050500010330     C*
050600010606     C                   IF        PiDecChr = *blanks
050700010606     C                   EVAL      PiDecChr = ','
050800010606     C                   ENDIF
050900010606     C*
051000010606     C                   CALL(e)   'ISNUMERIC'
051100010330     C                   PARM                    PiStr            30
051200010606     C                   PARM                    PiDecChr          1
051300010330     C                   PARM      *ZEROS        PiVal            30 9
051400010330     C                   PARM      '0'           PiInt             1
051500010330     C                   PARM      '0'           PiNum             1
051600010330     C                   IF        %error
051700010606     C                   EVAL      PiNum=*off
051800010330     C                   ENDIF
051900010330     C*
052000010330     C                   ENDSR
052100010330     C***
052200010330
052300010601
052400010601
052500010601
052600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052700010601     C     repfil        BEGSR
052800010601     C*
052900010601     C                   if        invfil = *zeros and
053000010601     C                             depfil > *zeros and
053100010629     C                             (vinflg = *blanks or
053200010629     C                              vinflg = *zeros)
053300010601     C
053400010601     C                   eval      invfil = depfil
053500010601     C                   endif
053600010601     C*
053700010601     C                   if        depfil <> invfil and
053800010601     C                             invfil > *zeros
053900010601     C                   eval      flgMulti = '1'
054000010601     C                   if        vinflg = *blanks
054100010601     C                   add       1             cntNonEl
054200010601     C                   endif
054300010601     C                   endif
054400010601     C*
054500010601     C                   if        vinflg = '2'
054600010601     C                   eval      flgStato = '2'
054700010601     C                   endif
054800010601     C*
054900010601     C                   ENDSR
055000010601     C***
055100010601
055200010601
055300010601
055400010330
055500010330
055600010330
055700990920      /TITLE Invio dei dati al punto operativo.
055800000613     C     invio         BEGSR
055900990920     C*
056000990920     C                   reset                   dscmz
056100010601     C                   move      invfil        cmzdst
056200990920     C                   eval      cmzfld = vlrfou
056300990920     C                   eval      cmzmbd = vlrhdl
056400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056500000710     C                   if        prmfir = *blanks
056600021025     C                   eval      cmzfla = 'FIVAB00F'
056700021025     C                   eval      cmzmba = 'FIVAB00F'
056800000710     C                   else
056900000710     C                   eval      cmzfla = prmfir
057000000710     C                   eval      cmzmba = prmfir
057100000710     C                   endif
057200990920     C                   eval      cmznrr = *zeros
057300990920     C                   move      §ctrok        cmznrr
057400021018     C                   eval      cmzlba = vlrfl1
057500990920     C                   call(e)   'TIS711C'
057600990920     C                   parm                    dscmz
057700990921     C                   parm      *blanks       esito
057800990920     C                   if        %error
057900990920     C                             or cmzerr = '1'
058000990921     C                             or esito  = '1'
058100000710     C                   eval      wrkesito = '3'
058200990920     C                   endif
058300990920     C*
058400000613     C                   ENDSR
058500990910
058600010601
058700010601
058800010601
058900010601
059000010601      /TITLE Invio dei dati al punto operativo.
059100010601     C     opeini        BEGSR
059200010601     C*
059300010601     C* Inizializzo flag e contatori operativi
059400010601     C                   movel     '0'           flgGiro           1
059500010601     C                   movel     '0'           flgMulti          1
059600010601     C                   movel     '1'           flgStato          1
059700010615     C                   movel     '0'           flgOk             1
059800010601     C                   z-add     *zeros        cntNonEl         10 0
059900010601     C                   z-add     *zeros        depfil            3 0
060000010601     C                   z-add     *zeros        invfil            3 0
060100010601     C*
060200010601     C                   ENDSR
060300010601     C***
060400010601
060500010601
060600010601
060700010330
060800010330
060900000613     C     *inzsr        BEGSR
061000990910     C*
061100990910     C     *entry        plist
061200990920     C                   parm                    tivlrds
061300990921     C                   parm      wrkesito      esito
061400000724     C                   parm                    prmlit
061500000710     C                   parm                    prmfir
061600010330     C*
061700010330     C* CALCOLA LA DATA CORRENTE
061800010330     C                   time                    wn14             14 0
061900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062000010330     C                   z-add     wn8           g08dat
062100010330     C                   z-add     *zeros        g08inv
062200010330     C                   movel     '0'           g08err
062300010330     C                   call      'XSRDA8'
062400010330     C                   parm                    wlbda8
062500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062600000613     C*
062700000613     C                   ENDSR
062800000613     C***
062900990908
063000020409     Otitv11p   E            riepilogo         2
063100990915     O                                              'Upload via Internet'
063200990915     O                                           +1 'Traduzione TIVIN00R -'
063300021025     O                                           +1 'FIVABWWR'
063400990915     O                                           +1 'Testate bolle'
063500990915     O          E            riepilogo   2
063600990915     O                       wrkdata
063700990915     O                       wrkora              +1
063800990915     O                       procname            +1
063900990915     O          E            riepilogo   2
064000990915     O                                              'Cliente..................:'
064100010605     O                       VABCCM        z     +1
064200990915     O          E            riepilogo   2
064300990920     O                                              'Riferimento Strategi.....:'
064400990920     O                       vlrhdl              +1
064500990915     O          E            riepilogo   2
064600990915     O                                              'Giusti...................:'
064700971022     O                       §CTROK        2   +  1
064800990915     O          E            riepilogo   2
064900990915     O                                              'Sbagliati e corretti.....:'
065000971022     O                       §CTRMO        2   +  1
065100990915     O          E            riepilogo   2
065200990915     O                                              'Sbagliati e scartati.....:'
065300971022     O                       §CTRNO        2   +  1
065400000613
065500020409     Otitv11ps  E            testdett          2
065600000613     O                                              'Upload via Internet'
065700000613     O                                           +1 'Traduzione TIVIN00R -'
065800021025     O                                           +1 'FIVABWWR'
065900000613     O                                           +1 'Testate bolle'
066000000616     O          E            testdett    3
066100000613     O                                           +2 'N° rec'
066200000613     O                                           +3 'Anteprima contenuto'
066300000616     O          E            rigadett    2
066400000613     O                       rrnum               +2
066500000621     O                       recko               +3
066600000616     O          E            findett     2
066700000613     O                       wrkdata
066800000613     O                       wrkora              +1
066900000613     O                       procname            +1
067000000616     O          E            findett     2
067100000613     O                                              'Cliente..................:'
067200010605     O                       VABCCM        z     +1
067300000616     O          E            findett     2
067400000613     O                                              'Riferimento Strategi.....:'
067500000613     O                       vlrhdl              +1
067600000616     O          E            findett     2
067700000613     O                                              'Giusti...................:'
067800000613     O                       §CTROK        2   +  1
067900000616     O          E            findett     2
068000000613     O                                              'Sbagliati e corretti.....:'
068100000613     O                       §CTRMO        2   +  1
068200000616     O          E            findett     2
068300000613     O                                              'Sbagliati e scartati.....:'
068400000613     O                       §CTRNO        2   +  1
068500000613** CMD - COMANDI CL
068600020409OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV11P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068700020409OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV11PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068800020409DLTOVR FILE(TITV11P TITV11PS) LVL(*)
068900010606
069000000613
