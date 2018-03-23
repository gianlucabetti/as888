000100030211      /TITLE Upload via Internet: traduzione in FIVABWWR. (x COLORPLAST Cod. 0011639)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030211     FFIVABwwr  O    E             DISK    usropn
000700020722     Ftitv18p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900020722     Ftitv18ps  O    f  198        PRINTER usropn
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
009500020722     C                   if        not %open(titv18ps)
009600020722     C                   open      titv18ps
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
011800020722     C                   if        %open(titv18ps)
011900000616     C                   except    findett
012000020722     C                   close     titv18ps
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
014100020722     C                   if        not %open(titv18p)
014200020722     C                   open      titv18p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700020722     C                   if        %open(titv18p)
014800020722     C                   close     titv18p
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
015900030211     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600030211     C                   if        not %open(fivabwwr)
016700030211     C                   open      fivabwwr
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
020400030211     C                   if        vlrpoi = 999
020500030211     C                   MOVE(P)   invfil        VABFGS
020600030211     C                   else
020700030211     C                   MOVE(P)   vlrpoi        VABFGS
020800030211     C                   endif
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
021900030211     C  N31              WRITE     FIVAB000
022000020722     C*
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
026900030211     C                   if        %open(fivabwwr)
027000030211     C                   close     fivabwwr
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
029400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029500010330     C*----------------------------------------------------*
029600010330     C     INZVAR        BEGSR
029700010330     C*
029800020204     C                   Z-ADD     *zeros        Num5_0            5 0
029900020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030000020322     C                   MOVEL     '0'           FlgCAS            1
030100010330     C*
030200010330     C                   ENDSR
030300010330     C*----------------------------------------------------*
030400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030500010330     C*----------------------------------------------------*
030600010330     C     DEFCAM        BEGSR
030700010330     C*
030800020204     C* Inizializzo il buffer del record da scrivere
030900030211     C                   CLEAR                   FIVAB000
031000020204     C* Imposto i valori di default...
031100020722     C                   EVAL      VABCCM = 0011639
031200020722     C                   EVAL      VABLNP = 001
031300020322     C                   EVAL      VABCTR = 000
031400020204     C* ... e poi verifico se sono stati passati come parametri
031500020204     C                   IF        vlrppt > *blanks
031600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031700020204     C                   EXSR      CHKNUM
031800020204     C                   IF        PiInt=*on
031900020204     C                   Z-ADD     PiVal         VABCCM
032000020204     C                   ENDIF
032100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032200020204     C                   EXSR      CHKNUM
032300020204     C                   IF        PiInt=*on
032400020204     C                   Z-ADD     PiVal         VABLNP
032500020204     C                   ENDIF
032600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032700020204     C                   EXSR      CHKNUM
032800020204     C                   IF        PiInt=*on
032900020204     C                   Z-ADD     PiVal         VABCTR
033000020204     C                   ENDIF
033100020204     C                   ENDIF
033200020204     C*
033300010330     C                   ENDSR
033400010607     C*----------------------------------------------------*
033500030211     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033600010607     C*----------------------------------------------------*
033700010607     C     IMPVAB        BEGSR
033800010607     C*
033900010607     C                   SETOFF                                       3132
034000010607     C*
034100010607     C* Reperimento campi ALFA
034200010607     C*
034300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034400020722     C                   EVAL      VABRSD=%trim(%subst(vindta:71:35))
034500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034700020117     C* ==
034800020722     C                   EVAL      VABRD2=%trim(%subst(vindta:(71+35):35))
034900020722     C                   EVAL      VABIND=%trim(%subst(vindta:159:35))
035000020722     C                   EVAL      VABLOD=%trim(%subst(vindta:208:34))
035100020722     C                   EVAL      VABPRD=%trim(%subst(vindta:242:2))
035200020722     C                   EVAL      VABRMA=%trim(%subst(vindta:4:11))
035300020801     C                   EVAL      VABVCA=%trim(%subst(vindta:244:3))
035400020801     C                   EVAL      VABNOT=%trim(%subst(vindta:256:35))
035500010607     C*
035600010607     C* Reperimento campi NUMERICI
035700020722     C                   MOVEL     *blanks       wrkAAS            4
035800020722     C                   MOVEL     *blanks       wrkMGS            4
035900020722     C                   EVAL      wrkAAS = %subst(vindta:19:4)
036000020722     C                   EVAL      wrkMGS = %subst(vindta:17:2) +
036100020722     C                                      %subst(vindta:15:2)
036200020722     C                   MOVEL     wrkAAS        VABAAS
036300020722     C                   MOVEL     wrkMGS        VABMGS
036400020722     C*
036500020722     C                   EVAL      PiStr=%trim(%subst(vindta:4:11))
036600020304     C                   EXSR      CHKNUM
036700020304     C                   IF        PiInt=*on
036800020304     C                   Z-ADD     PiVal         VABNSP
036900020304     C                   Z-ADD     PiVal         VABRMN
037000020304     C                   ELSE
037100020722     C                   SETON                                        31
037200020304     C                   Z-ADD     *zeros        VABNSP
037300020329     C                   Z-ADD     *zeros        VABRMN
037400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037500020321     C                             + ' ' + 'VABNSP VABRMN'
037600020304     C                   ENDIF
037700020304     C*
037800020722     C                   EVAL      PiStr=%trim(%subst(vindta:203:5))
037900010607     C                   EXSR      CHKNUM
038000010607     C                   IF        PiInt=*on
038100020204     C                   Z-ADD     PiVal         Num5_0
038200020204     C                   MOVEL(p)  Num5_0        VABCAD
038300010607     C                   ELSE
038400010607     C                   SETON                                        32
038500020204     C                   EVAL      VABCAD = *zeros
038600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038700020204     C                             + ' ' + 'VABCAD'
038800010607     C                   ENDIF
038900010607     C*
039000020722     C                   EVAL      PiStr=%trim(%subst(vindta:34:5))
039100010607     C                   EXSR      CHKNUM
039200010607     C                   IF        PiInt=*on
039300010607     C                   Z-ADD     PiVal         VABNCL
039400010607     C                   ELSE
039500010607     C                   SETON                                        32
039600010607     C                   Z-ADD     *zeros        VABNCL
039700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039800010607     C                             + ' ' + 'VABNCL'
039900010607     C                   ENDIF
040000020304     C*
040100020722     C                   EVAL      PiStr=%trim(%subst(vindta:63:8))
040200020204     C                   EVAL      PiDecChr = ','
040300010607     C                   EXSR      CHKNUM
040400010607     C                   IF        PiNum=*on
040500020722     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
040600020722     C* riferisce ad un campo 8,3
040700020722     C                   EVAL(H)   VABPKB = PiVal/1000
040800010607     C                   ELSE
040900010607     C                   SETON                                        32
041000010607     C                   Z-ADD     *zeros        VABPKB
041100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041200010607     C                             + ' ' + 'VABPKB'
041300010607     C                   ENDIF
041400020722     C*
041500020801     C                   EVAL      PiStr=%trim(%subst(vindta:247:9))
041600020722     C* Detrmino se indipendentemente da giusto o sbagliato l'intenzione del cliente era d
041700020722     C* passare il contrassegno
041800020722     C                   IF        PiStr > *zeros
041900020722     C                   MOVEL     '1'           FlgCAS
042000020722     C                   ENDIF
042100020722     C*
042200020722     C                   EXSR      CHKNUM
042300020722     C                   IF        PiNum=*on
042400020722     C* Divido x 100 perchè il campo di input non gestisce separatori decimali ma il valore si
042500020722     C* riferisce ad un campo 9,2
042600020722     C                   EVAL(H)   VABCAS = PiVal/100
042700020722     C                   ELSE
042800020722     C                   SETON                                        32
042900020722     C                   Z-ADD     *zeros        VABCAS
043000020722     C                   EVAL      vinmsg = %trimr(vinmsg)
043100020722     C                             + ' ' + 'VABCAS'
043200020722     C                   ENDIF
043300020322     C*
043400020322     C* Considerazioni finali su CBO/CAS
043500020322     C                   IF        FlgCAS = '1'
043600020322     C                   EVAL      VABCBO = '4'
043700020322     C                   ELSE
043800020322     C                   EVAL      VABCBO = '1'
043900020322     C                   ENDIF
044000020204     C*
044100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044200020204     C                   EXSR      CHKIMPDIV
044300020204     C*
044400010607     C                   ENDSR
044500010607     C*----------------------------------------------------*
044600010601
044700020204
044800020204     C*----------------------------------------------------*
044900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045000020204     C*----------------------------------------------------*
045100020204     C     CHKIMPDIV     BEGSR
045200020204     C*
045300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045400020204     C                   Z-ADD     *zeros        wrkDec            9 9
045500020204     C*
045600020204     C* Come prima cosa effettuo considerazioni sulla divisa
045700020204     C                   IF        vabIAS > *zeros
045800020204     C                   IF        vabVAS <> 'EUR'
045900020204     C                   EVAL      vabVAS =  'ITL'
046000020204     C                   ENDIF
046100020204     C                   ENDIF
046200020204     C*
046300020204     C                   IF        vabCAS > *zeros
046400020204     C                   IF        vabVCA <> 'EUR'
046500020204     C                   EVAL      vabVCA =  'ITL'
046600020204     C                   ENDIF
046700020204     C                   ENDIF
046800020204     C*
046900020204     C                   IF        vabVMD > *zeros
047000020321     C                   IF        vabVAD <> 'EUR'
047100020204     C                   EVAL      vabVAD =  'ITL'
047200020204     C                   ENDIF
047300020204     C                   ENDIF
047400020204     C*
047500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047600020204     C                   Z-ADD     vabIAS        wrkDec
047700020204     C                   IF        wrkDec > *zeros
047800020204     C                   IF        vabVAS = 'ITL'
047900020204     C                   EVAL      vabIAS = *zeros
048000020204     C                   ENDIF
048100020204     C                   ENDIF
048200020204     C*
048300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
048400020204     C                   Z-ADD     vabCAS        wrkDec
048500020204     C                   IF        wrkDec > *zeros
048600020204     C                   IF        vabVCA = 'ITL'
048700020204     C                   EVAL      vabCAS = *zeros
048800020204     C                   ENDIF
048900020204     C                   ENDIF
049000020204     C*
049100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049200020204     C                   Z-ADD     vabVMD        wrkDec
049300020204     C                   IF        wrkDec > *zeros
049400020204     C                   IF        vabVAD = 'ITL'
049500020204     C                   EVAL      vabVMD = *zeros
049600020204     C                   ENDIF
049700020204     C                   ENDIF
049800020204     C*
049900020204     C                   ENDSR
050000020204     C***
050100020204
050200010330
050300010330
050400010330     C*----------------------------------------------------*
050500010330     C*  CONTROLLO NUMERICITA' CAMPI
050600010330     C*----------------------------------------------------*
050700010330     C     CHKNUM        BEGSR
050800010330     C*
050900010606     C                   IF        PiDecChr = *blanks
051000010606     C                   EVAL      PiDecChr = ','
051100010606     C                   ENDIF
051200010606     C*
051300010606     C                   CALL(e)   'ISNUMERIC'
051400010330     C                   PARM                    PiStr            30
051500010606     C                   PARM                    PiDecChr          1
051600010330     C                   PARM      *ZEROS        PiVal            30 9
051700010330     C                   PARM      '0'           PiInt             1
051800010330     C                   PARM      '0'           PiNum             1
051900010330     C                   IF        %error
052000010606     C                   EVAL      PiNum=*off
052100010330     C                   ENDIF
052200010330     C*
052300010330     C                   ENDSR
052400010330     C***
052500010330
052600010601
052700010601
052800010601
052900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053000010601     C     repfil        BEGSR
053100010601     C*
053200010601     C                   if        invfil = *zeros and
053300010601     C                             depfil > *zeros and
053400010629     C                             (vinflg = *blanks or
053500010629     C                              vinflg = *zeros)
053600010601     C
053700010601     C                   eval      invfil = depfil
053800010601     C                   endif
053900010601     C*
054000010601     C                   if        depfil <> invfil and
054100010601     C                             invfil > *zeros
054200010601     C                   eval      flgMulti = '1'
054300010601     C                   if        vinflg = *blanks
054400010601     C                   add       1             cntNonEl
054500010601     C                   endif
054600010601     C                   endif
054700010601     C*
054800010601     C                   if        vinflg = '2'
054900010601     C                   eval      flgStato = '2'
055000010601     C                   endif
055100010601     C*
055200010601     C                   ENDSR
055300010601     C***
055400010601
055500010601
055600010601
055700010330
055800010330
055900010330
056000990920      /TITLE Invio dei dati al punto operativo.
056100000613     C     invio         BEGSR
056200990920     C*
056300990920     C                   reset                   dscmz
056400010601     C                   move      invfil        cmzdst
056500990920     C                   eval      cmzfld = vlrfou
056600990920     C                   eval      cmzmbd = vlrhdl
056700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056800000710     C                   if        prmfir = *blanks
056900030211     C                   eval      cmzfla = 'FIVAB00F'
057000030211     C                   eval      cmzmba = 'FIVAB00F'
057100000710     C                   else
057200000710     C                   eval      cmzfla = prmfir
057300000710     C                   eval      cmzmba = prmfir
057400000710     C                   endif
057500990920     C                   eval      cmznrr = *zeros
057600990920     C                   move      §ctrok        cmznrr
057700021018     C                   eval      cmzlba = vlrfl1
057800990920     C                   call(e)   'TIS711C'
057900990920     C                   parm                    dscmz
058000990921     C                   parm      *blanks       esito
058100990920     C                   if        %error
058200990920     C                             or cmzerr = '1'
058300990921     C                             or esito  = '1'
058400000710     C                   eval      wrkesito = '3'
058500990920     C                   endif
058600990920     C*
058700000613     C                   ENDSR
058800990910
058900010601
059000010601
059100010601
059200010601
059300010601      /TITLE Invio dei dati al punto operativo.
059400010601     C     opeini        BEGSR
059500010601     C*
059600010601     C* Inizializzo flag e contatori operativi
059700010601     C                   movel     '0'           flgGiro           1
059800010601     C                   movel     '0'           flgMulti          1
059900010601     C                   movel     '1'           flgStato          1
060000010615     C                   movel     '0'           flgOk             1
060100010601     C                   z-add     *zeros        cntNonEl         10 0
060200010601     C                   z-add     *zeros        depfil            3 0
060300010601     C                   z-add     *zeros        invfil            3 0
060400010601     C*
060500010601     C                   ENDSR
060600010601     C***
060700010601
060800010601
060900010601
061000010330
061100010330
061200000613     C     *inzsr        BEGSR
061300990910     C*
061400990910     C     *entry        plist
061500990920     C                   parm                    tivlrds
061600990921     C                   parm      wrkesito      esito
061700000724     C                   parm                    prmlit
061800000710     C                   parm                    prmfir
061900010330     C*
062000010330     C* CALCOLA LA DATA CORRENTE
062100010330     C                   time                    wn14             14 0
062200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062300010330     C                   z-add     wn8           g08dat
062400010330     C                   z-add     *zeros        g08inv
062500010330     C                   movel     '0'           g08err
062600010330     C                   call      'XSRDA8'
062700010330     C                   parm                    wlbda8
062800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062900000613     C*
063000000613     C                   ENDSR
063100000613     C***
063200990908
063300020722     Otitv18p   E            riepilogo         2
063400990915     O                                              'Upload via Internet'
063500990915     O                                           +1 'Traduzione TIVIN00R -'
063600030211     O                                           +1 'FIVABWWR'
063700990915     O                                           +1 'Testate bolle'
063800990915     O          E            riepilogo   2
063900990915     O                       wrkdata
064000990915     O                       wrkora              +1
064100990915     O                       procname            +1
064200990915     O          E            riepilogo   2
064300990915     O                                              'Cliente..................:'
064400010605     O                       VABCCM        z     +1
064500990915     O          E            riepilogo   2
064600990920     O                                              'Riferimento Strategi.....:'
064700990920     O                       vlrhdl              +1
064800990915     O          E            riepilogo   2
064900990915     O                                              'Giusti...................:'
065000971022     O                       §CTROK        2   +  1
065100990915     O          E            riepilogo   2
065200990915     O                                              'Sbagliati e corretti.....:'
065300971022     O                       §CTRMO        2   +  1
065400990915     O          E            riepilogo   2
065500990915     O                                              'Sbagliati e scartati.....:'
065600971022     O                       §CTRNO        2   +  1
065700000613
065800020722     Otitv18ps  E            testdett          2
065900000613     O                                              'Upload via Internet'
066000000613     O                                           +1 'Traduzione TIVIN00R -'
066100030211     O                                           +1 'FIVABWWR'
066200000613     O                                           +1 'Testate bolle'
066300000616     O          E            testdett    3
066400000613     O                                           +2 'N° rec'
066500000613     O                                           +3 'Anteprima contenuto'
066600000616     O          E            rigadett    2
066700000613     O                       rrnum               +2
066800000621     O                       recko               +3
066900000616     O          E            findett     2
067000000613     O                       wrkdata
067100000613     O                       wrkora              +1
067200000613     O                       procname            +1
067300000616     O          E            findett     2
067400000613     O                                              'Cliente..................:'
067500010605     O                       VABCCM        z     +1
067600000616     O          E            findett     2
067700000613     O                                              'Riferimento Strategi.....:'
067800000613     O                       vlrhdl              +1
067900000616     O          E            findett     2
068000000613     O                                              'Giusti...................:'
068100000613     O                       §CTROK        2   +  1
068200000616     O          E            findett     2
068300000613     O                                              'Sbagliati e corretti.....:'
068400000613     O                       §CTRMO        2   +  1
068500000616     O          E            findett     2
068600000613     O                                              'Sbagliati e scartati.....:'
068700000613     O                       §CTRNO        2   +  1
068800000613** CMD - COMANDI CL
068900020722OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV18P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
069000020722OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV18PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
069100020722DLTOVR FILE(TITV18P TITV18PS) LVL(*)
069200010606
069300000613
