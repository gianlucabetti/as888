000100021025      /TITLE Upload via Internet: traduzione in FIVABWWR. (x AEROLOGISTIK Cod. 0011029)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
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
033700021025 xxx C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
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
035900020402     C                   IF        VABNZD=*zeros
036000020402     C                   EVAL      VABNZD=*blanks
036100020402     C                   ENDIF
036200020322     C                   EVAL      VABRMA=%trim(%subst(vindta:83:7))
036300020322     C                   EVAL      VABNOT=%trim(%subst(vindta:491:40))
036400020322     C                   EVAL      VABNAS=%trim(%subst(vindta:337:15))
036500020322     C                   EVAL      VABCBO=%trim(%subst(vindta:2:1))
036600020322     C                   EVAL      VABTSP=%trim(%subst(vindta:477:1))
036700020322     C                   EVAL      VABVCA=%trim(%subst(vindta:401:3))
036800020322     C                   EVAL      VABVAD=%trim(%subst(vindta:571:3))
036900010607     C*
037000010607     C* Reperimento campi NUMERICI
037100020322     C                   EVAL      PiStr=%trim(%subst(vindta:3:4))
037200020304     C                   EXSR      CHKNUM
037300020304     C                   IF        PiInt=*on
037400020304     C                   Z-ADD     PiVal         VABAAS
037500020304     C                   ELSE
037600020304     C                   SETON                                        32
037700020304     C                   EVAL      VABAAS = *zeros
037800020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037900020304     C                             + ' ' + 'VABAAS'
038000020304     C                   ENDIF
038100020304     C*
038200020322     C                   EVAL      PiStr=%trim(%subst(vindta:7:4))
038300020304     C                   EXSR      CHKNUM
038400020304     C                   IF        PiInt=*on
038500020304     C                   Z-ADD     PiVal         VABMGS
038600020304     C                   ELSE
038700020304     C                   SETON                                        32
038800020304     C                   EVAL      VABMGS = *zeros
038900020304     C                   EVAL      vinmsg = %trimr(vinmsg)
039000020304     C                             + ' ' + 'VABMGS'
039100020304     C                   ENDIF
039200020322     C*
039300020322     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
039400020322     C                   EXSR      CHKNUM
039500020322     C                   IF        PiInt=*on
039600020322     C                   Z-ADD     PiVal         VABDCR
039700020322     C                   ELSE
039800020322     C                   SETON                                        32
039900020722     C                   EVAL      VABDCR = *zeros
040000020322     C                   EVAL      vinmsg = %trimr(vinmsg)
040100020322     C                             + ' ' + 'VABDCR'
040200020322     C                   ENDIF
040300020304     C*
040400020322     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
040500020304     C                   EXSR      CHKNUM
040600020304     C                   IF        PiInt=*on
040700020304     C                   Z-ADD     PiVal         VABNSP
040800020304     C                   Z-ADD     PiVal         VABRMN
040900020304     C                   ELSE
041000020304     C                   SETON                                        32
041100020304     C                   Z-ADD     *zeros        VABNSP
041200020329     C                   Z-ADD     *zeros        VABRMN
041300020304     C                   EVAL      vinmsg = %trimr(vinmsg)
041400020321     C                             + ' ' + 'VABNSP VABRMN'
041500020304     C                   ENDIF
041600020304     C*
041700020322     C                   EVAL      PiStr=%trim(%subst(vindta:319:5))
041800010607     C                   EXSR      CHKNUM
041900010607     C                   IF        PiInt=*on
042000020204     C                   Z-ADD     PiVal         Num5_0
042100020204     C                   MOVEL(p)  Num5_0        VABCAD
042200010607     C                   ELSE
042300010607     C                   SETON                                        32
042400020204     C                   EVAL      VABCAD = *zeros
042500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042600020204     C                             + ' ' + 'VABCAD'
042700010607     C                   ENDIF
042800010607     C*
042900020322     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
043000010607     C                   EXSR      CHKNUM
043100010607     C                   IF        PiInt=*on
043200010607     C                   Z-ADD     PiVal         VABNCL
043300010607     C                   ELSE
043400010607     C                   SETON                                        32
043500010607     C                   Z-ADD     *zeros        VABNCL
043600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043700010607     C                             + ' ' + 'VABNCL'
043800010607     C                   ENDIF
043900020304     C*
044000020322     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
044100020204     C                   EVAL      PiDecChr = ','
044200010607     C                   EXSR      CHKNUM
044300010607     C                   IF        PiNum=*on
044400020322     C* Divido x 100 perchè il campo di input non gestisce separatori decimali ma il valore si
044500020322     C* riferisce ad un campo 7,2
044600020322     C                   EVAL(H)   VABPKB = PiVal/100
044700010607     C                   ELSE
044800010607     C                   SETON                                        32
044900010607     C                   Z-ADD     *zeros        VABPKB
045000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045100010607     C                             + ' ' + 'VABPKB'
045200010607     C                   ENDIF
045300020304     C*
045400020322     C                   EVAL      PiStr=%trim(%subst(vindta:369:5))
045500020304     C                   EVAL      PiDecChr = ','
045600020304     C                   EXSR      CHKNUM
045700020304     C                   IF        PiNum=*on
045800020304     C* Divido x 100 perchè il campo di input non gestisce separatori decimali ma il valore si
045900020322     C* riferisce ad un campo 5,2
046000020304     C                   EVAL(H)   VABVLB = PiVal/100
046100020304     C                   ELSE
046200020304     C                   SETON                                        32
046300020329     C                   Z-ADD     *zeros        VABVLB
046400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
046500020322     C                             + ' ' + 'VABVLB'
046600020304     C                   ENDIF
046700020322     C*
046800020322     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
046900020322     C* Detrmino che indipendentemente da giusto o sbagliato l'intenzione del cliente era d
047000020322     C* passare il contrassegno
047100020402     C                   IF        PiStr > *zeros
047200020322     C                   MOVEL     '1'           FlgCAS
047300020322     C                   ENDIF
047400020322     C*
047500020322     C                   EXSR      CHKNUM
047600020322     C                   IF        PiNum=*on
047700020322     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
047800020322     C* riferisce ad un campo 13,3
047900020322     C                   EVAL(H)   VABCAS = PiVal/1000
048000020322     C                   ELSE
048100020322     C                   SETON                                        32
048200020322     C                   Z-ADD     *zeros        VABCAS
048300020322     C                   EVAL      vinmsg = %trimr(vinmsg)
048400020322     C                             + ' ' + 'VABCAS'
048500020322     C                   ENDIF
048600020322     C*
048700020322     C                   EVAL      PiStr=%trim(%subst(vindta:558:13))
048800020322     C                   EXSR      CHKNUM
048900020322     C                   IF        PiNum=*on
049000020322     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
049100020322     C* riferisce ad un campo 13,3
049200020322     C                   EVAL(H)   VABVMD = PiVal/1000
049300020322     C                   ELSE
049400020322     C                   SETON                                        32
049500020322     C                   Z-ADD     *zeros        VABVMD
049600020322     C                   EVAL      vinmsg = %trimr(vinmsg)
049700020322     C                             + ' ' + 'VABVMD'
049800020322     C                   ENDIF
049900020322     C*
050000020322     C* Considerazioni finali su CBO/CAS
050100020322     C                   IF        FlgCAS = '1'
050200020322     C                   EVAL      VABCBO = '4'
050300020322     C                   ELSE
050400020322     C                   EVAL      VABCBO = '1'
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
057600010606     C                   EVAL      PiDecChr = ','
057700010606     C                   ENDIF
057800010606     C*
057900010606     C                   CALL(e)   'ISNUMERIC'
058000010330     C                   PARM                    PiStr            30
058100010606     C                   PARM                    PiDecChr          1
058200010330     C                   PARM      *ZEROS        PiVal            30 9
058300010330     C                   PARM      '0'           PiInt             1
058400010330     C                   PARM      '0'           PiNum             1
058500010330     C                   IF        %error
058600010606     C                   EVAL      PiNum=*off
058700010330     C                   ENDIF
058800010330     C*
058900010330     C                   ENDSR
059000010330     C***
059100010330
059200010601
059300010601
059400010601
059500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
059600010601     C     repfil        BEGSR
059700010601     C*
059800010601     C                   if        invfil = *zeros and
059900010601     C                             depfil > *zeros and
060000010629     C                             (vinflg = *blanks or
060100010629     C                              vinflg = *zeros)
060200010601     C
060300010601     C                   eval      invfil = depfil
060400010601     C                   endif
060500010601     C*
060600010601     C                   if        depfil <> invfil and
060700010601     C                             invfil > *zeros
060800010601     C                   eval      flgMulti = '1'
060900010601     C                   if        vinflg = *blanks
061000010601     C                   add       1             cntNonEl
061100010601     C                   endif
061200010601     C                   endif
061300010601     C*
061400010601     C                   if        vinflg = '2'
061500010601     C                   eval      flgStato = '2'
061600010601     C                   endif
061700010601     C*
061800010601     C                   ENDSR
061900010601     C***
062000010601
062100010601
062200010601
062300010330
062400010330
062500010330
062600990920      /TITLE Invio dei dati al punto operativo.
062700000613     C     invio         BEGSR
062800990920     C*
062900990920     C                   reset                   dscmz
063000010601     C                   move      invfil        cmzdst
063100990920     C                   eval      cmzfld = vlrfou
063200990920     C                   eval      cmzmbd = vlrhdl
063300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
063400000710     C                   if        prmfir = *blanks
063500021025     C                   eval      cmzfla = 'FIVAB00F'
063600021025     C                   eval      cmzmba = 'FIVAB00F'
063700000710     C                   else
063800000710     C                   eval      cmzfla = prmfir
063900000710     C                   eval      cmzmba = prmfir
064000000710     C                   endif
064100990920     C                   eval      cmznrr = *zeros
064200990920     C                   move      §ctrok        cmznrr
064300021018     C                   eval      cmzlba = vlrfl1
064400990920     C                   call(e)   'TIS711C'
064500990920     C                   parm                    dscmz
064600990921     C                   parm      *blanks       esito
064700990920     C                   if        %error
064800990920     C                             or cmzerr = '1'
064900990921     C                             or esito  = '1'
065000000710     C                   eval      wrkesito = '3'
065100990920     C                   endif
065200990920     C*
065300000613     C                   ENDSR
065400990910
065500010601
065600010601
065700010601
065800010601
065900010601      /TITLE Invio dei dati al punto operativo.
066000010601     C     opeini        BEGSR
066100010601     C*
066200010601     C* Inizializzo flag e contatori operativi
066300010601     C                   movel     '0'           flgGiro           1
066400010601     C                   movel     '0'           flgMulti          1
066500010601     C                   movel     '1'           flgStato          1
066600010615     C                   movel     '0'           flgOk             1
066700010601     C                   z-add     *zeros        cntNonEl         10 0
066800010601     C                   z-add     *zeros        depfil            3 0
066900010601     C                   z-add     *zeros        invfil            3 0
067000010601     C*
067100010601     C                   ENDSR
067200010601     C***
067300010601
067400010601
067500010601
067600010330
067700010330
067800000613     C     *inzsr        BEGSR
067900990910     C*
068000990910     C     *entry        plist
068100990920     C                   parm                    tivlrds
068200990921     C                   parm      wrkesito      esito
068300000724     C                   parm                    prmlit
068400000710     C                   parm                    prmfir
068500010330     C*
068600010330     C* CALCOLA LA DATA CORRENTE
068700010330     C                   time                    wn14             14 0
068800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068900010330     C                   z-add     wn8           g08dat
069000010330     C                   z-add     *zeros        g08inv
069100010330     C                   movel     '0'           g08err
069200010330     C                   call      'XSRDA8'
069300010330     C                   parm                    wlbda8
069400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069500000613     C*
069600000613     C                   ENDSR
069700000613     C***
069800990908
069900020322     Otitv08p   E            riepilogo         2
070000990915     O                                              'Upload via Internet'
070100990915     O                                           +1 'Traduzione TIVIN00R -'
070200021025     O                                           +1 'FIVABWWR'
070300990915     O                                           +1 'Testate bolle'
070400990915     O          E            riepilogo   2
070500990915     O                       wrkdata
070600990915     O                       wrkora              +1
070700990915     O                       procname            +1
070800990915     O          E            riepilogo   2
070900990915     O                                              'Cliente..................:'
071000010605     O                       VABCCM        z     +1
071100990915     O          E            riepilogo   2
071200990920     O                                              'Riferimento Strategi.....:'
071300990920     O                       vlrhdl              +1
071400990915     O          E            riepilogo   2
071500990915     O                                              'Giusti...................:'
071600971022     O                       §CTROK        2   +  1
071700990915     O          E            riepilogo   2
071800990915     O                                              'Sbagliati e corretti.....:'
071900971022     O                       §CTRMO        2   +  1
072000990915     O          E            riepilogo   2
072100990915     O                                              'Sbagliati e scartati.....:'
072200971022     O                       §CTRNO        2   +  1
072300000613
072400020322     Otitv08ps  E            testdett          2
072500000613     O                                              'Upload via Internet'
072600000613     O                                           +1 'Traduzione TIVIN00R -'
072700021025     O                                           +1 'FIVABWWR'
072800000613     O                                           +1 'Testate bolle'
072900000616     O          E            testdett    3
073000000613     O                                           +2 'N° rec'
073100000613     O                                           +3 'Anteprima contenuto'
073200000616     O          E            rigadett    2
073300000613     O                       rrnum               +2
073400000621     O                       recko               +3
073500000616     O          E            findett     2
073600000613     O                       wrkdata
073700000613     O                       wrkora              +1
073800000613     O                       procname            +1
073900000616     O          E            findett     2
074000000613     O                                              'Cliente..................:'
074100010605     O                       VABCCM        z     +1
074200000616     O          E            findett     2
074300000613     O                                              'Riferimento Strategi.....:'
074400000613     O                       vlrhdl              +1
074500000616     O          E            findett     2
074600000613     O                                              'Giusti...................:'
074700000613     O                       §CTROK        2   +  1
074800000616     O          E            findett     2
074900000613     O                                              'Sbagliati e corretti.....:'
075000000613     O                       §CTRMO        2   +  1
075100000616     O          E            findett     2
075200000613     O                                              'Sbagliati e scartati.....:'
075300000613     O                       §CTRNO        2   +  1
075400000613** CMD - COMANDI CL
075500020322OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV08P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
075600020322OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV08PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
075700020322DLTOVR FILE(TITV08P TITV08PS) LVL(*)
075800010606
075900000613
