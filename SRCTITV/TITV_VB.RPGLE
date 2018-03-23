000100030211      /TITLE Upload via Internet: traduzione in FIVABWWR. (x AEROLOGISTIK Cod. 0011029)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030211     FFIVABwwr  O    E             DISK    usropn
000700020322     Ftitv08p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900020322     Ftitv08ps  O    f  198        PRINTER usropn
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
009500020322     C                   if        not %open(titv08ps)
009600020322     C                   open      titv08ps
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
011800020322     C                   if        %open(titv08ps)
011900000616     C                   except    findett
012000020322     C                   close     titv08ps
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
014100020322     C                   if        not %open(titv08p)
014200020322     C                   open      titv08p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700020322     C                   if        %open(titv08p)
014800020322     C                   close     titv08p
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
031100030211     C                   CLEAR                   FIVAB000
031200020204     C* Imposto i valori di default...
031300020322     C                   EVAL      VABCCM = 0011029
031400020322     C                   EVAL      VABLNP = 001
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
033700030211 xxx C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033800010607     C*----------------------------------------------------*
033900010607     C     IMPVAB        BEGSR
034000010607     C*
034100010607     C                   SETOFF                                       3132
034200010607     C*
034300010607     C* Reperimento campi ALFA
034400010607     C*
034500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034600020322     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
034700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034900020117     C* ==
035000020322     C                   EVAL      VABRMO=%trim(%subst(vindta:114:30))
035100020322     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
035200020322     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
035300020322     C                   IF        %trim(%subst(vindta:314:4))='ROMA'
035400020321     C                   EVAL      VABPRD='RM'
035500020321     C                   ELSE
035600020322     C                   EVAL      VABPRD=%trim(%subst(vindta:314:4))
035700020321     C                   ENDIF
035800020322     C                   EVAL      VABNZD=%trim(%subst(vindta:327:4))
035900020322     C                   EVAL      VABRMA=%trim(%subst(vindta:83:7))
036000020322     C                   EVAL      VABNOT=%trim(%subst(vindta:491:40))
036100020322     C                   EVAL      VABNAS=%trim(%subst(vindta:337:15))
036200020322     C                   EVAL      VABCBO=%trim(%subst(vindta:2:1))
036300020322     C                   EVAL      VABTSP=%trim(%subst(vindta:477:1))
036400020322     C                   EVAL      VABVCA=%trim(%subst(vindta:401:3))
036500020322     C                   EVAL      VABVAD=%trim(%subst(vindta:571:3))
036600010607     C*
036700010607     C* Reperimento campi NUMERICI
036800020322     C                   EVAL      PiStr=%trim(%subst(vindta:3:4))
036900020304     C                   EXSR      CHKNUM
037000020304     C                   IF        PiInt=*on
037100020304     C                   Z-ADD     PiVal         VABAAS
037200020304     C                   ELSE
037300020304     C                   SETON                                        32
037400020304     C                   EVAL      VABAAS = *zeros
037500020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037600020304     C                             + ' ' + 'VABAAS'
037700020304     C                   ENDIF
037800020304     C*
037900020322     C                   EVAL      PiStr=%trim(%subst(vindta:7:4))
038000020304     C                   EXSR      CHKNUM
038100020304     C                   IF        PiInt=*on
038200020304     C                   Z-ADD     PiVal         VABMGS
038300020304     C                   ELSE
038400020304     C                   SETON                                        32
038500020304     C                   EVAL      VABMGS = *zeros
038600020304     C                   EVAL      vinmsg = %trimr(vinmsg)
038700020304     C                             + ' ' + 'VABMGS'
038800020304     C                   ENDIF
038900020322     C*
039000020322     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
039100020322     C                   EXSR      CHKNUM
039200020322     C                   IF        PiInt=*on
039300020322     C                   Z-ADD     PiVal         VABDCR
039400020322     C                   ELSE
039500020322     C                   SETON                                        32
039600020322     C                   EVAL      VABMGS = *zeros
039700020322     C                   EVAL      vinmsg = %trimr(vinmsg)
039800020322     C                             + ' ' + 'VABDCR'
039900020322     C                   ENDIF
040000020304     C*
040100020322     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
040200020304     C                   EXSR      CHKNUM
040300020304     C                   IF        PiInt=*on
040400020304     C                   Z-ADD     PiVal         VABNSP
040500020304     C                   Z-ADD     PiVal         VABRMN
040600020304     C                   ELSE
040700020304     C                   SETON                                        32
040800020304     C                   Z-ADD     *zeros        VABNSP
040900020329     C                   Z-ADD     *zeros        VABRMN
041000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
041100020321     C                             + ' ' + 'VABNSP VABRMN'
041200020304     C                   ENDIF
041300020304     C*
041400020322     C                   EVAL      PiStr=%trim(%subst(vindta:319:5))
041500010607     C                   EXSR      CHKNUM
041600010607     C                   IF        PiInt=*on
041700020204     C                   Z-ADD     PiVal         Num5_0
041800020204     C                   MOVEL(p)  Num5_0        VABCAD
041900010607     C                   ELSE
042000010607     C                   SETON                                        32
042100020204     C                   EVAL      VABCAD = *zeros
042200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042300020204     C                             + ' ' + 'VABCAD'
042400010607     C                   ENDIF
042500010607     C*
042600020322     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
042700010607     C                   EXSR      CHKNUM
042800010607     C                   IF        PiInt=*on
042900010607     C                   Z-ADD     PiVal         VABNCL
043000010607     C                   ELSE
043100010607     C                   SETON                                        32
043200010607     C                   Z-ADD     *zeros        VABNCL
043300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043400010607     C                             + ' ' + 'VABNCL'
043500010607     C                   ENDIF
043600020304     C*
043700020322     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
043800020204     C                   EVAL      PiDecChr = ','
043900010607     C                   EXSR      CHKNUM
044000010607     C                   IF        PiNum=*on
044100020322     C* Divido x 100 perchè il campo di input non gestisce separatori decimali ma il valore si
044200020322     C* riferisce ad un campo 7,2
044300020322     C                   EVAL(H)   VABPKB = PiVal/100
044400010607     C                   ELSE
044500010607     C                   SETON                                        32
044600010607     C                   Z-ADD     *zeros        VABPKB
044700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044800010607     C                             + ' ' + 'VABPKB'
044900010607     C                   ENDIF
045000020304     C*
045100020322     C                   EVAL      PiStr=%trim(%subst(vindta:369:5))
045200020304     C                   EVAL      PiDecChr = ','
045300020304     C                   EXSR      CHKNUM
045400020304     C                   IF        PiNum=*on
045500020304     C* Divido x 100 perchè il campo di input non gestisce separatori decimali ma il valore si
045600020322     C* riferisce ad un campo 5,2
045700020304     C                   EVAL(H)   VABVLB = PiVal/100
045800020304     C                   ELSE
045900020304     C                   SETON                                        32
046000020329     C                   Z-ADD     *zeros        VABVLB
046100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
046200020322     C                             + ' ' + 'VABVLB'
046300020304     C                   ENDIF
046400020322     C*
046500020322     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
046600020322     C* Detrmino che indipendentemente da giusto o sbagliato l'intenzione del cliente era d
046700020322     C* passare il contrassegno
046800020402     C                   IF        PiStr > *zeros
046900020322     C                   MOVEL     '1'           FlgCAS
047000020322     C                   ENDIF
047100020322     C*
047200020322     C                   EXSR      CHKNUM
047300020322     C                   IF        PiNum=*on
047400020322     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
047500020322     C* riferisce ad un campo 13,3
047600020322     C                   EVAL(H)   VABCAS = PiVal/1000
047700020322     C                   ELSE
047800020322     C                   SETON                                        32
047900020322     C                   Z-ADD     *zeros        VABCAS
048000020322     C                   EVAL      vinmsg = %trimr(vinmsg)
048100020322     C                             + ' ' + 'VABCAS'
048200020322     C                   ENDIF
048300020322     C*
048400020322     C                   EVAL      PiStr=%trim(%subst(vindta:558:13))
048500020322     C                   EXSR      CHKNUM
048600020322     C                   IF        PiNum=*on
048700020322     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
048800020322     C* riferisce ad un campo 13,3
048900020322     C                   EVAL(H)   VABVMD = PiVal/1000
049000020322     C                   ELSE
049100020322     C                   SETON                                        32
049200020322     C                   Z-ADD     *zeros        VABVMD
049300020322     C                   EVAL      vinmsg = %trimr(vinmsg)
049400020322     C                             + ' ' + 'VABVMD'
049500020322     C                   ENDIF
049600020322     C*
049700020322     C* Considerazioni finali su CBO/CAS
049800020322     C                   IF        FlgCAS = '1'
049900020322     C                   EVAL      VABCBO = '4'
050000020322     C                   ELSE
050100020322     C                   EVAL      VABCBO = '1'
050200020322     C                   ENDIF
050300020204     C*
050400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050500020204     C                   EXSR      CHKIMPDIV
050600020204     C*
050700010607     C                   ENDSR
050800010607     C*----------------------------------------------------*
050900010601
051000020204
051100020204     C*----------------------------------------------------*
051200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051300020204     C*----------------------------------------------------*
051400020204     C     CHKIMPDIV     BEGSR
051500020204     C*
051600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051700020204     C                   Z-ADD     *zeros        wrkDec            9 9
051800020204     C*
051900020204     C* Come prima cosa effettuo considerazioni sulla divisa
052000020204     C                   IF        vabIAS > *zeros
052100020204     C                   IF        vabVAS <> 'EUR'
052200020204     C                   EVAL      vabVAS =  'ITL'
052300020204     C                   ENDIF
052400020204     C                   ENDIF
052500020204     C*
052600020204     C                   IF        vabCAS > *zeros
052700020204     C                   IF        vabVCA <> 'EUR'
052800020204     C                   EVAL      vabVCA =  'ITL'
052900020204     C                   ENDIF
053000020204     C                   ENDIF
053100020204     C*
053200020204     C                   IF        vabVMD > *zeros
053300020321     C                   IF        vabVAD <> 'EUR'
053400020204     C                   EVAL      vabVAD =  'ITL'
053500020204     C                   ENDIF
053600020204     C                   ENDIF
053700020204     C*
053800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053900020204     C                   Z-ADD     vabIAS        wrkDec
054000020204     C                   IF        wrkDec > *zeros
054100020204     C                   IF        vabVAS = 'ITL'
054200020204     C                   EVAL      vabIAS = *zeros
054300020204     C                   ENDIF
054400020204     C                   ENDIF
054500020204     C*
054600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
054700020204     C                   Z-ADD     vabCAS        wrkDec
054800020204     C                   IF        wrkDec > *zeros
054900020204     C                   IF        vabVCA = 'ITL'
055000020204     C                   EVAL      vabCAS = *zeros
055100020204     C                   ENDIF
055200020204     C                   ENDIF
055300020204     C*
055400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055500020204     C                   Z-ADD     vabVMD        wrkDec
055600020204     C                   IF        wrkDec > *zeros
055700020204     C                   IF        vabVAD = 'ITL'
055800020204     C                   EVAL      vabVMD = *zeros
055900020204     C                   ENDIF
056000020204     C                   ENDIF
056100020204     C*
056200020204     C                   ENDSR
056300020204     C***
056400020204
056500010330
056600010330
056700010330     C*----------------------------------------------------*
056800010330     C*  CONTROLLO NUMERICITA' CAMPI
056900010330     C*----------------------------------------------------*
057000010330     C     CHKNUM        BEGSR
057100010330     C*
057200010606     C                   IF        PiDecChr = *blanks
057300010606     C                   EVAL      PiDecChr = ','
057400010606     C                   ENDIF
057500010606     C*
057600010606     C                   CALL(e)   'ISNUMERIC'
057700010330     C                   PARM                    PiStr            30
057800010606     C                   PARM                    PiDecChr          1
057900010330     C                   PARM      *ZEROS        PiVal            30 9
058000010330     C                   PARM      '0'           PiInt             1
058100010330     C                   PARM      '0'           PiNum             1
058200010330     C                   IF        %error
058300010606     C                   EVAL      PiNum=*off
058400010330     C                   ENDIF
058500010330     C*
058600010330     C                   ENDSR
058700010330     C***
058800010330
058900010601
059000010601
059100010601
059200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
059300010601     C     repfil        BEGSR
059400010601     C*
059500010601     C                   if        invfil = *zeros and
059600010601     C                             depfil > *zeros and
059700010629     C                             (vinflg = *blanks or
059800010629     C                              vinflg = *zeros)
059900010601     C
060000010601     C                   eval      invfil = depfil
060100010601     C                   endif
060200010601     C*
060300010601     C                   if        depfil <> invfil and
060400010601     C                             invfil > *zeros
060500010601     C                   eval      flgMulti = '1'
060600010601     C                   if        vinflg = *blanks
060700010601     C                   add       1             cntNonEl
060800010601     C                   endif
060900010601     C                   endif
061000010601     C*
061100010601     C                   if        vinflg = '2'
061200010601     C                   eval      flgStato = '2'
061300010601     C                   endif
061400010601     C*
061500010601     C                   ENDSR
061600010601     C***
061700010601
061800010601
061900010601
062000010330
062100010330
062200010330
062300990920      /TITLE Invio dei dati al punto operativo.
062400000613     C     invio         BEGSR
062500990920     C*
062600990920     C                   reset                   dscmz
062700010601     C                   move      invfil        cmzdst
062800990920     C                   eval      cmzfld = vlrfou
062900990920     C                   eval      cmzmbd = vlrhdl
063000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
063100000710     C                   if        prmfir = *blanks
063200030211     C                   eval      cmzfla = 'FIVAB00F'
063300030211     C                   eval      cmzmba = 'FIVAB00F'
063400000710     C                   else
063500000710     C                   eval      cmzfla = prmfir
063600000710     C                   eval      cmzmba = prmfir
063700000710     C                   endif
063800990920     C                   eval      cmznrr = *zeros
063900990920     C                   move      §ctrok        cmznrr
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
069500020322     Otitv08p   E            riepilogo         2
069600990915     O                                              'Upload via Internet'
069700990915     O                                           +1 'Traduzione TIVIN00R -'
069800030211     O                                           +1 'FIVABWWR'
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
072000020322     Otitv08ps  E            testdett          2
072100000613     O                                              'Upload via Internet'
072200000613     O                                           +1 'Traduzione TIVIN00R -'
072300030211     O                                           +1 'FIVABWWR'
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
075100020322OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV08P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
075200020322OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV08PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
075300020322DLTOVR FILE(TITV08P TITV08PS) LVL(*)
075400010606
075500000613
