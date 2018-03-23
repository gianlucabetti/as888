000100021025      /TITLE Upload via Internet: traduzione in FIVABWWR. (x HANS GHOHE Cod. 0760198)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700020722     Ftitv17p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900020722     Ftitv17ps  O    f  198        PRINTER usropn
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
009500020722     C                   if        not %open(titv17ps)
009600020722     C                   open      titv17ps
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
011800020722     C                   if        %open(titv17ps)
011900000616     C                   except    findett
012000020722     C                   close     titv17ps
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
014100020722     C                   if        not %open(titv17p)
014200020722     C                   open      titv17p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700020722     C                   if        %open(titv17p)
014800020722     C                   close     titv17p
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
015900021025 xxx C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
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
017300020722     C*
017400020722     C* Inizializzo un contatore x partire a tradurre dal 2° record
017500020722     C                   z-add     *zeros        cntREC            9 0
017600010330     C*
017700010330     C                   DO        *HIVAL
017800010330     C*
017900010330     C                   READ      tivin00r                               70
018000020722     C                   add       1             cntREC
018100020722     C*
018200020722     C* Traduco solo dal 2° record (compreso) in poi
018300020722     C                   if        cntREC > 1
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
022800020722     C                   endif
022900020722     C*
023000010604     C                   if        *in31 = *off and
023100010604     C                             *in32 = *off
023200010604     C                   eval      vinflg = '1'
023300010604     C                   else
023400010604     C                   eval      recko = vindta
023500010604     C                   except    rigadett
023600010604     C                   eval      vinflg = '2'
023700010604     C                   endif
023800010604     C                   endif
023900010601     C*
024000010604     C                   endif
024100010604     C*
024200010330     C                   else
024300010330     C                   eval      vinflg = '1'
024400010330     C                   endif
024500010601     C*
024600010601     C  N70              update    tivin000
024700010330     C*
024800010330     C  N70              ENDdo
024900010601     C
025000010601     C*
025100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025200010601     C                   if        cntNonEl = *zeros or
025300010601     C                             flgMulti = '0'
025400010330     C* Se non ci sono record con errori ...
025500010601     C                   if        §ctrno = 0 and
025600010604     C                             §ctrmo = 0 and
025700010601     C                             flgStato <> '2'
025800010330     C* ... restituisco esito OK.
025900010330     C                   eval      wrkesito = '0'
026000010330     C                   else
026100010330     C                   if        §ctrok > 0
026200010330     C                   eval      wrkesito = '1'
026300010330     C                   else
026400010615     C                   if        flgOk = '0'
026500010615     C                   eval      wrkesito = '2'
026600010615     C                   else
026700010615     C                   eval      wrkesito = '6'
026800010615     C                   endif
026900010330     C                   endif
027000010330     C                   endif
027100010601     C                   else
027200010601     C                   eval      wrkesito = '9'
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        %open(tivin00r)
027600010330     C                   close     tivin00r
027700010330     C                   endif
027800021025     C                   if        %open(fivabwwr)
027900021025     C                   close     fivabwwr
028000010330     C                   endif
028100010601     C*
028200010601     C                   if        vlrpoi <> 999
028300010601     C                   eval      invfil = vlrpoi
028400010601     C                   endif
028500010330     C*
028600010330     C                   if        §ctrok > 0
028700010601     C                             and invfil > *zeros
028800010330     C                   exsr      invio
028900010330     C                   endif
029000010601     C*
029100010618     C                   if        flgGiro = '1'
029200010601     C                   exsr      endela
029300010618     C                   endif
029400010330     C*
029500010330     C                   ENDSR
029600010330     C***
029700010330
029800010601
029900010601
030000010601
030100010601
030200010330     C*----------------------------------------------------*
030300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030400010330     C*----------------------------------------------------*
030500010330     C     INZVAR        BEGSR
030600010330     C*
030700020204     C                   Z-ADD     *zeros        Num5_0            5 0
030800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030900020322     C                   MOVEL     '0'           FlgCAS            1
031000010330     C*
031100010330     C                   ENDSR
031200010330     C*----------------------------------------------------*
031300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
031400010330     C*----------------------------------------------------*
031500010330     C     DEFCAM        BEGSR
031600010330     C*
031700020204     C* Inizializzo il buffer del record da scrivere
031800021025     C                   CLEAR                   FIVAB000
031900020204     C* Imposto i valori di default...
032000020722     C                   EVAL      VABCCM = 0760198
032100020722     C                   EVAL      VABLNP = 076
032200020322     C                   EVAL      VABCTR = 000
032300020204     C* ... e poi verifico se sono stati passati come parametri
032400020204     C                   IF        vlrppt > *blanks
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020204     C                   Z-ADD     PiVal         VABCCM
032900020204     C                   ENDIF
033000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033100020204     C                   EXSR      CHKNUM
033200020204     C                   IF        PiInt=*on
033300020204     C                   Z-ADD     PiVal         VABLNP
033400020204     C                   ENDIF
033500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033600020204     C                   EXSR      CHKNUM
033700020204     C                   IF        PiInt=*on
033800020204     C                   Z-ADD     PiVal         VABCTR
033900020204     C                   ENDIF
034000020204     C                   ENDIF
034100020204     C*
034200010330     C                   ENDSR
034300010607     C*----------------------------------------------------*
034400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034500010607     C*----------------------------------------------------*
034600010607     C     IMPVAB        BEGSR
034700010607     C*
034800010607     C                   SETOFF                                       3132
034900010607     C*
035000010607     C* Reperimento campi ALFA
035100010607     C*
035200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035300020722     C                   EVAL      VABRSD=%trim(%subst(vindta:36:35))
035400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035600020117     C* ==
035700020722     C                   EVAL      VABIND=%trim(%subst(vindta:71:35))
035800020722     C                   EVAL      VABLOD=%trim(%subst(vindta:106:40))
035900020722     C                   EVAL      VABPRD=%trim(%subst(vindta:151:2))
036000020722     C                   EVAL      VABRMA=%trim(%subst(vindta:1:35))
036100010607     C*
036200010607     C* Reperimento campi NUMERICI
036300020722     C                   MOVEL     DATCOR        VABAAS
036400020722     C                   MOVE      DATCOR        VABMGS
036500020722     C*
036600020722     C                   EVAL      PiStr=%trim(%subst(vindta:1:35))
036700020722     C* ...cerco e tolgo eventuali * presenti
036800020722     C     '*':' '       XLATE     PiStr         PiStr
036900020304     C                   EXSR      CHKNUM
037000020304     C                   IF        PiInt=*on
037100020304     C                   Z-ADD     PiVal         VABNSP
037200020304     C                   Z-ADD     PiVal         VABRMN
037300020304     C                   ELSE
037400020722     C                   SETON                                        31
037500020304     C                   Z-ADD     *zeros        VABNSP
037600020329     C                   Z-ADD     *zeros        VABRMN
037700020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037800020321     C                             + ' ' + 'VABNSP VABRMN'
037900020304     C                   ENDIF
038000020304     C*
038100020722     C                   EVAL      PiStr=%trim(%subst(vindta:146:5))
038200010607     C                   EXSR      CHKNUM
038300010607     C                   IF        PiInt=*on
038400020204     C                   Z-ADD     PiVal         Num5_0
038500020204     C                   MOVEL(p)  Num5_0        VABCAD
038600010607     C                   ELSE
038700010607     C                   SETON                                        32
038800020204     C                   EVAL      VABCAD = *zeros
038900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039000020204     C                             + ' ' + 'VABCAD'
039100010607     C                   ENDIF
039200010607     C*
039300020722     C                   EVAL      PiStr=%trim(%subst(vindta:177:4))
039400010607     C                   EXSR      CHKNUM
039500010607     C                   IF        PiInt=*on
039600010607     C                   Z-ADD     PiVal         VABNCL
039700010607     C                   ELSE
039800010607     C                   SETON                                        32
039900010607     C                   Z-ADD     *zeros        VABNCL
040000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040100010607     C                             + ' ' + 'VABNCL'
040200010607     C                   ENDIF
040300020304     C*
040400020722     C                   EVAL      PiStr=%trim(%subst(vindta:166:5))
040500020204     C                   EVAL      PiDecChr = ','
040600010607     C                   EXSR      CHKNUM
040700010607     C                   IF        PiNum=*on
040800020722     C                   EVAL      VABPKB = PiVal
040900010607     C                   ELSE
041000010607     C                   SETON                                        32
041100010607     C                   Z-ADD     *zeros        VABPKB
041200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041300010607     C                             + ' ' + 'VABPKB'
041400010607     C                   ENDIF
041500020322     C*
041600020322     C* Considerazioni finali su CBO/CAS
041700020322     C                   IF        FlgCAS = '1'
041800020322     C                   EVAL      VABCBO = '4'
041900020322     C                   ELSE
042000020322     C                   EVAL      VABCBO = '1'
042100020322     C                   ENDIF
042200020204     C*
042300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042400020204     C                   EXSR      CHKIMPDIV
042500020204     C*
042600010607     C                   ENDSR
042700010607     C*----------------------------------------------------*
042800010601
042900020204
043000020204     C*----------------------------------------------------*
043100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043200020204     C*----------------------------------------------------*
043300020204     C     CHKIMPDIV     BEGSR
043400020204     C*
043500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043600020204     C                   Z-ADD     *zeros        wrkDec            9 9
043700020204     C*
043800020204     C* Come prima cosa effettuo considerazioni sulla divisa
043900020204     C                   IF        vabIAS > *zeros
044000020204     C                   IF        vabVAS <> 'EUR'
044100020204     C                   EVAL      vabVAS =  'ITL'
044200020204     C                   ENDIF
044300020204     C                   ENDIF
044400020204     C*
044500020204     C                   IF        vabCAS > *zeros
044600020204     C                   IF        vabVCA <> 'EUR'
044700020204     C                   EVAL      vabVCA =  'ITL'
044800020204     C                   ENDIF
044900020204     C                   ENDIF
045000020204     C*
045100020204     C                   IF        vabVMD > *zeros
045200020321     C                   IF        vabVAD <> 'EUR'
045300020204     C                   EVAL      vabVAD =  'ITL'
045400020204     C                   ENDIF
045500020204     C                   ENDIF
045600020204     C*
045700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045800020204     C                   Z-ADD     vabIAS        wrkDec
045900020204     C                   IF        wrkDec > *zeros
046000020204     C                   IF        vabVAS = 'ITL'
046100020204     C                   EVAL      vabIAS = *zeros
046200020204     C                   ENDIF
046300020204     C                   ENDIF
046400020204     C*
046500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
046600020204     C                   Z-ADD     vabCAS        wrkDec
046700020204     C                   IF        wrkDec > *zeros
046800020204     C                   IF        vabVCA = 'ITL'
046900020204     C                   EVAL      vabCAS = *zeros
047000020204     C                   ENDIF
047100020204     C                   ENDIF
047200020204     C*
047300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047400020204     C                   Z-ADD     vabVMD        wrkDec
047500020204     C                   IF        wrkDec > *zeros
047600020204     C                   IF        vabVAD = 'ITL'
047700020204     C                   EVAL      vabVMD = *zeros
047800020204     C                   ENDIF
047900020204     C                   ENDIF
048000020204     C*
048100020204     C                   ENDSR
048200020204     C***
048300020204
048400010330
048500010330
048600010330     C*----------------------------------------------------*
048700010330     C*  CONTROLLO NUMERICITA' CAMPI
048800010330     C*----------------------------------------------------*
048900010330     C     CHKNUM        BEGSR
049000010330     C*
049100010606     C                   IF        PiDecChr = *blanks
049200010606     C                   EVAL      PiDecChr = ','
049300010606     C                   ENDIF
049400010606     C*
049500010606     C                   CALL(e)   'ISNUMERIC'
049600010330     C                   PARM                    PiStr            30
049700010606     C                   PARM                    PiDecChr          1
049800010330     C                   PARM      *ZEROS        PiVal            30 9
049900010330     C                   PARM      '0'           PiInt             1
050000010330     C                   PARM      '0'           PiNum             1
050100010330     C                   IF        %error
050200010606     C                   EVAL      PiNum=*off
050300010330     C                   ENDIF
050400010330     C*
050500010330     C                   ENDSR
050600010330     C***
050700010330
050800010601
050900010601
051000010601
051100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051200010601     C     repfil        BEGSR
051300010601     C*
051400010601     C                   if        invfil = *zeros and
051500010601     C                             depfil > *zeros and
051600010629     C                             (vinflg = *blanks or
051700010629     C                              vinflg = *zeros)
051800010601     C
051900010601     C                   eval      invfil = depfil
052000010601     C                   endif
052100010601     C*
052200010601     C                   if        depfil <> invfil and
052300010601     C                             invfil > *zeros
052400010601     C                   eval      flgMulti = '1'
052500010601     C                   if        vinflg = *blanks
052600010601     C                   add       1             cntNonEl
052700010601     C                   endif
052800010601     C                   endif
052900010601     C*
053000010601     C                   if        vinflg = '2'
053100010601     C                   eval      flgStato = '2'
053200010601     C                   endif
053300010601     C*
053400010601     C                   ENDSR
053500010601     C***
053600010601
053700010601
053800010601
053900010330
054000010330
054100010330
054200990920      /TITLE Invio dei dati al punto operativo.
054300000613     C     invio         BEGSR
054400990920     C*
054500990920     C                   reset                   dscmz
054600010601     C                   move      invfil        cmzdst
054700990920     C                   eval      cmzfld = vlrfou
054800990920     C                   eval      cmzmbd = vlrhdl
054900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
055000000710     C                   if        prmfir = *blanks
055100021025     C                   eval      cmzfla = 'FIVAB00F'
055200021025     C                   eval      cmzmba = 'FIVAB00F'
055300000710     C                   else
055400000710     C                   eval      cmzfla = prmfir
055500000710     C                   eval      cmzmba = prmfir
055600000710     C                   endif
055700990920     C                   eval      cmznrr = *zeros
055800990920     C                   move      §ctrok        cmznrr
055900021018     C                   eval      cmzlba = vlrfl1
056000990920     C                   call(e)   'TIS711C'
056100990920     C                   parm                    dscmz
056200990921     C                   parm      *blanks       esito
056300990920     C                   if        %error
056400990920     C                             or cmzerr = '1'
056500990921     C                             or esito  = '1'
056600000710     C                   eval      wrkesito = '3'
056700990920     C                   endif
056800990920     C*
056900000613     C                   ENDSR
057000990910
057100010601
057200010601
057300010601
057400010601
057500010601      /TITLE Invio dei dati al punto operativo.
057600010601     C     opeini        BEGSR
057700010601     C*
057800010601     C* Inizializzo flag e contatori operativi
057900010601     C                   movel     '0'           flgGiro           1
058000010601     C                   movel     '0'           flgMulti          1
058100010601     C                   movel     '1'           flgStato          1
058200010615     C                   movel     '0'           flgOk             1
058300010601     C                   z-add     *zeros        cntNonEl         10 0
058400010601     C                   z-add     *zeros        depfil            3 0
058500010601     C                   z-add     *zeros        invfil            3 0
058600010601     C*
058700010601     C                   ENDSR
058800010601     C***
058900010601
059000010601
059100010601
059200010330
059300010330
059400000613     C     *inzsr        BEGSR
059500990910     C*
059600990910     C     *entry        plist
059700990920     C                   parm                    tivlrds
059800990921     C                   parm      wrkesito      esito
059900000724     C                   parm                    prmlit
060000000710     C                   parm                    prmfir
060100010330     C*
060200010330     C* CALCOLA LA DATA CORRENTE
060300010330     C                   time                    wn14             14 0
060400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060500010330     C                   z-add     wn8           g08dat
060600010330     C                   z-add     *zeros        g08inv
060700010330     C                   movel     '0'           g08err
060800010330     C                   call      'XSRDA8'
060900010330     C                   parm                    wlbda8
061000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061100000613     C*
061200000613     C                   ENDSR
061300000613     C***
061400990908
061500020722     Otitv17p   E            riepilogo         2
061600990915     O                                              'Upload via Internet'
061700990915     O                                           +1 'Traduzione TIVIN00R -'
061800021025     O                                           +1 'FIVABWWR'
061900990915     O                                           +1 'Testate bolle'
062000990915     O          E            riepilogo   2
062100990915     O                       wrkdata
062200990915     O                       wrkora              +1
062300990915     O                       procname            +1
062400990915     O          E            riepilogo   2
062500990915     O                                              'Cliente..................:'
062600010605     O                       VABCCM        z     +1
062700990915     O          E            riepilogo   2
062800990920     O                                              'Riferimento Strategi.....:'
062900990920     O                       vlrhdl              +1
063000990915     O          E            riepilogo   2
063100990915     O                                              'Giusti...................:'
063200971022     O                       §CTROK        2   +  1
063300990915     O          E            riepilogo   2
063400990915     O                                              'Sbagliati e corretti.....:'
063500971022     O                       §CTRMO        2   +  1
063600990915     O          E            riepilogo   2
063700990915     O                                              'Sbagliati e scartati.....:'
063800971022     O                       §CTRNO        2   +  1
063900000613
064000020722     Otitv17ps  E            testdett          2
064100000613     O                                              'Upload via Internet'
064200000613     O                                           +1 'Traduzione TIVIN00R -'
064300021025     O                                           +1 'FIVABWWR'
064400000613     O                                           +1 'Testate bolle'
064500000616     O          E            testdett    3
064600000613     O                                           +2 'N° rec'
064700000613     O                                           +3 'Anteprima contenuto'
064800000616     O          E            rigadett    2
064900000613     O                       rrnum               +2
065000000621     O                       recko               +3
065100000616     O          E            findett     2
065200000613     O                       wrkdata
065300000613     O                       wrkora              +1
065400000613     O                       procname            +1
065500000616     O          E            findett     2
065600000613     O                                              'Cliente..................:'
065700010605     O                       VABCCM        z     +1
065800000616     O          E            findett     2
065900000613     O                                              'Riferimento Strategi.....:'
066000000613     O                       vlrhdl              +1
066100000616     O          E            findett     2
066200000613     O                                              'Giusti...................:'
066300000613     O                       §CTROK        2   +  1
066400000616     O          E            findett     2
066500000613     O                                              'Sbagliati e corretti.....:'
066600000613     O                       §CTRMO        2   +  1
066700000616     O          E            findett     2
066800000613     O                                              'Sbagliati e scartati.....:'
066900000613     O                       §CTRNO        2   +  1
067000000613** CMD - COMANDI CL
067100020722OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV17P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
067200020722OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV17PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
067300020722DLTOVR FILE(TITV17P TITV17PS) LVL(*)
067400010606
067500000613
