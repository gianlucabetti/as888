000100040723      /TITLE Upload via Internet: traduzione in FIVABWWR. (x FESTO Cod. 0891154)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040723     FFIVABwwr  O    E             DISK    usropn
000700040723     Ftitvd9p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040723     Ftitvd9ps  O    f  198        PRINTER usropn
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
002300040607     D tisi95ds      e ds
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
004100000313     D*
004200010330
004300010330
004400010330
004500990915     C                   time                    wrkdata
004600990915     C                   time                    wrkora
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000010601     C*
005100010601     C                   exsr      opeini
005200010605     C                   exsr      rwvab
005300040928     C*
005400040928     C* Esegue lancio TISI95R solo x chiusura
005500040928     C                   CLEAR                   TISI95DS
005600040928     C                   EVAL      I95TLA = 'C'
005700040928     C                   CALL      'TISI95R'
005800040928     C                   PARM                    TISI95DS
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
010200040723     C                   if        not %open(titvd9ps)
010300040723     C                   open      titvd9ps
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
012500040723     C                   if        %open(titvd9ps)
012600000616     C                   except    findett
012700040723     C                   close     titvd9ps
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
014800040723     C                   if        not %open(titvd9p)
014900040723     C                   open      titvd9p
015000990915     C                   endif
015100990915     C*
015200990915     C                   except    riepilogo
015300990915     C*
015400040723     C                   if        %open(titvd9p)
015500040723     C                   close     titvd9p
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
016600040723     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016700010330     C*--------------------------------------------------------
016800010605     C     RWVAB         BEGSR
016900010330     C*
017000010330     C                   if        not %open(tivin00r)
017100010330     C                   open      tivin00r
017200010330     C                   endif
017300040723     C                   if        not %open(fivabwwr)
017400040723     C                   open      fivabwwr
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
022500040723     C  N31              WRITE     FIVAB000
022600010604     C*
022700010604     C                   if        *in31 = *off and
022800010604     C                             *in32 = *off
022900010604     C                   eval      vinflg = '1'
023000010604     C                   else
023100010604     C                   eval      recko = vindta
023200010604     C                   except    rigadett
023300010604     C                   eval      vinflg = '2'
023400010604     C                   endif
023500010604     C                   endif
023600010601     C*
023700010604     C                   endif
023800010604     C*
023900010330     C                   else
024000010330     C                   eval      vinflg = '1'
024100010330     C                   endif
024200010601     C*
024300010601     C  N70              update    tivin000
024400010330     C*
024500010330     C  N70              ENDdo
024600010601     C
024700010601     C*
024800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024900010601     C                   if        cntNonEl = *zeros or
025000010601     C                             flgMulti = '0'
025100010330     C* Se non ci sono record con errori ...
025200010601     C                   if        §ctrno = 0 and
025300010604     C                             §ctrmo = 0 and
025400010601     C                             flgStato <> '2'
025500010330     C* ... restituisco esito OK.
025600010330     C                   eval      wrkesito = '0'
025700010330     C                   else
025800010330     C                   if        §ctrok > 0
025900010330     C                   eval      wrkesito = '1'
026000010330     C                   else
026100010615     C                   if        flgOk = '0'
026200010615     C                   eval      wrkesito = '2'
026300010615     C                   else
026400010615     C                   eval      wrkesito = '6'
026500010615     C                   endif
026600010330     C                   endif
026700010330     C                   endif
026800010601     C                   else
026900010601     C                   eval      wrkesito = '9'
027000010601     C                   endif
027100010330     C*
027200010330     C                   if        %open(tivin00r)
027300010330     C                   close     tivin00r
027400010330     C                   endif
027500040723     C                   if        %open(fivabwwr)
027600040723     C                   close     fivabwwr
027700010330     C                   endif
027800010601     C*
027900010601     C                   if        vlrpoi <> 999
028000010601     C                   eval      invfil = vlrpoi
028100010601     C                   endif
028200010330     C*
028300010330     C                   if        §ctrok > 0
028400010601     C                             and invfil > *zeros
028500010330     C                   exsr      invio
028600010330     C                   endif
028700010601     C*
028800010618     C                   if        flgGiro = '1'
028900010601     C                   exsr      endela
029000010618     C                   endif
029100010330     C*
029200010330     C                   ENDSR
029300010330     C***
029400010330
029500010601
029600010601
029700010601
029800010601
029900010330     C*----------------------------------------------------*
030000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030100010330     C*----------------------------------------------------*
030200040607     C     INZVAR        BEGSR
030300010330     C*
030400020204     C                   Z-ADD     *zeros        Num5_0            5 0
030500020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030600020322     C                   MOVEL     '0'           FlgCAS            1
030700010330     C*
030800010330     C                   ENDSR
030900010330     C*----------------------------------------------------*
031000040607     C*  IMPOSTAZIONE CAMPI COSTANTI
031100010330     C*----------------------------------------------------*
031200010330     C     DEFCAM        BEGSR
031300010330     C*
031400020204     C* Inizializzo il buffer del record da scrivere
031500040723     C                   CLEAR                   FIVAB000
031600020204     C* Imposto i valori di default...
031700040607     C                   EVAL      VABCCM = 0891154
031800040607     C                   EVAL      VABLNP = 089
031900020322     C                   EVAL      VABCTR = 000
032000040607     C                   EVAL      VABTSP = 'C'
032100020204     C* ... e poi verifico se sono stati passati come parametri
032200020204     C                   IF        vlrppt > *blanks
032300040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
032400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032500020204     C                   EXSR      CHKNUM
032600020204     C                   IF        PiInt=*on
032700020204     C                   Z-ADD     PiVal         VABCCM
032800020204     C                   ENDIF
032900040607     C                   ENDIF
033000040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
033100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033200020204     C                   EXSR      CHKNUM
033300020204     C                   IF        PiInt=*on
033400020204     C                   Z-ADD     PiVal         VABLNP
033500020204     C                   ENDIF
033600040607     C                   ENDIF
033700040607     C                   IF        %subst(vlrppt:11:3) <> *blanks
033800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033900020204     C                   EXSR      CHKNUM
034000020204     C                   IF        PiInt=*on
034100020204     C                   Z-ADD     PiVal         VABCTR
034200020204     C                   ENDIF
034300040607     C                   ENDIF
034400020204     C                   ENDIF
034500020204     C*
034600010330     C                   ENDSR
034700010607     C*----------------------------------------------------*
034800040723     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034900010607     C*----------------------------------------------------*
035000010607     C     IMPVAB        BEGSR
035100010607     C*
035200010607     C                   SETOFF                                       3132
035300010607     C*
035400010607     C* Reperimento campi ALFA
035500010607     C*
035600010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035700040607     C                   EVAL      VABRSD=%trim(%subst(vindta:164:30))
035800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
036000020117     C* ==
036100040607     C                   EVAL      VABRD2=%trim(%subst(vindta:194:30))
036200040607     C                   EVAL      VABIND=%trim(%subst(vindta:224:25))
036300040607     C                   EVAL      VABLOD=%trim(%subst(vindta:254:25))
036400040607     C                   EVAL      VABRMA=%trim(%subst(vindta:10:7))
036500020329     C*
036600020329     C                   MOVEL     DATCOR        VABAAS
036700020329     C                   MOVE      DATCOR        VABMGS
036800010607     C*
036900010607     C* Reperimento campi NUMERICI
037000040607     C* NSP/RMN
037100040607     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
037200020304     C                   EXSR      CHKNUM
037300020304     C                   IF        PiInt=*on
037400020304     C                   Z-ADD     PiVal         VABNSP
037500040607     C                   Z-ADD     PiVal         VABRMN
037600020304     C                   ELSE
037700020304     C                   SETON                                        32
037800020304     C                   Z-ADD     *zeros        VABNSP
037900040607     C                   Z-ADD     1             VABRMN
038000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
038100040607     C                             + ' ' + 'VABNSP VABRMN'
038200020304     C                   ENDIF
038300040607     C* CAD
038400040607     C                   EVAL      PiStr=%trim(%subst(vindta:249:5))
038500010607     C                   EXSR      CHKNUM
038600010607     C                   IF        PiInt=*on
038700020204     C                   Z-ADD     PiVal         Num5_0
038800020204     C                   MOVEL(p)  Num5_0        VABCAD
038900010607     C                   ELSE
039000010607     C                   SETON                                        32
039100020204     C                   EVAL      VABCAD = *zeros
039200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039300020204     C                             + ' ' + 'VABCAD'
039400010607     C                   ENDIF
039500040607     C* Reperisco la provincia dal CAP e dalla località
039600040607     C                   IF        VABCAD <> *blanks
039700040607     C                   CLEAR                   TISI95DS
039800040607     C                   EVAL      I95TCN = '3'
039900040607     C                   Z-ADD     datcor        I95DAT
040000040607     C                   EVAL      I95CAP = VABCAD
040100040607     C                   EVAL      I95LOC = VABLOD
040200040607     C                   CALL      'TISI95R'
040300040607     C                   PARM                    TISI95DS
040400040607     C                   EVAL      VABPRD = O95PRV
040500040607     C                   ENDIF
040600040607     C* NCL
040700040607     C                   EVAL      PiStr=%trim(%subst(vindta:557:4))
040800010607     C                   EXSR      CHKNUM
040900010607     C                   IF        PiInt=*on
041000010607     C                   Z-ADD     PiVal         VABNCL
041100010607     C                   ELSE
041200010607     C                   SETON                                        32
041300010607     C                   Z-ADD     *zeros        VABNCL
041400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041500010607     C                             + ' ' + 'VABNCL'
041600010607     C                   ENDIF
041700040607     C* PKB
041800040607     C                   EVAL      PiStr=%trim(%subst(vindta:561:7))
041900020204     C                   EVAL      PiDecChr = ','
042000010607     C                   EXSR      CHKNUM
042100010607     C                   IF        PiNum=*on
042200040607     C                   Z-ADD     PiVal         VABPKB
042300010607     C                   ELSE
042400010607     C                   SETON                                        32
042500010607     C                   Z-ADD     *zeros        VABPKB
042600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042700010607     C                             + ' ' + 'VABPKB'
042800010607     C                   ENDIF
042900020322     C*
043000020322     C* Considerazioni finali su CBO/CAS
043100020322     C                   IF        FlgCAS = '1'
043200020322     C                   EVAL      VABCBO = '4'
043300020322     C                   ELSE
043400020322     C                   EVAL      VABCBO = '1'
043500020322     C                   ENDIF
043600020204     C*
043700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043800020204     C                   EXSR      CHKIMPDIV
043900020204     C*
044000010607     C                   ENDSR
044100010607     C*----------------------------------------------------*
044200010601
044300020204
044400020204     C*----------------------------------------------------*
044500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044600020204     C*----------------------------------------------------*
044700020204     C     CHKIMPDIV     BEGSR
044800020204     C*
044900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045000020204     C                   Z-ADD     *zeros        wrkDec            9 9
045100020204     C*
045200020204     C* Come prima cosa effettuo considerazioni sulla divisa
045300020204     C                   IF        vabIAS > *zeros
045400020204     C                   IF        vabVAS <> 'EUR'
045500020204     C                   EVAL      vabVAS =  'ITL'
045600020204     C                   ENDIF
045700020204     C                   ENDIF
045800020204     C*
045900020204     C                   IF        vabCAS > *zeros
046000020204     C                   IF        vabVCA <> 'EUR'
046100020204     C                   EVAL      vabVCA =  'ITL'
046200020204     C                   ENDIF
046300020204     C                   ENDIF
046400020204     C*
046500020204     C                   IF        vabVMD > *zeros
046600020321     C                   IF        vabVAD <> 'EUR'
046700020204     C                   EVAL      vabVAD =  'ITL'
046800020204     C                   ENDIF
046900020204     C                   ENDIF
047000020204     C*
047100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047200020204     C                   Z-ADD     vabIAS        wrkDec
047300020204     C                   IF        wrkDec > *zeros
047400020204     C                   IF        vabVAS = 'ITL'
047500020204     C                   EVAL      vabIAS = *zeros
047600020204     C                   ENDIF
047700020204     C                   ENDIF
047800020204     C*
047900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
048000020204     C                   Z-ADD     vabCAS        wrkDec
048100020204     C                   IF        wrkDec > *zeros
048200020204     C                   IF        vabVCA = 'ITL'
048300020204     C                   EVAL      vabCAS = *zeros
048400020204     C                   ENDIF
048500020204     C                   ENDIF
048600020204     C*
048700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048800020204     C                   Z-ADD     vabVMD        wrkDec
048900020204     C                   IF        wrkDec > *zeros
049000020204     C                   IF        vabVAD = 'ITL'
049100020204     C                   EVAL      vabVMD = *zeros
049200020204     C                   ENDIF
049300020204     C                   ENDIF
049400020204     C*
049500020204     C                   ENDSR
049600020204     C***
049700020204
049800010330
049900010330
050000010330     C*----------------------------------------------------*
050100010330     C*  CONTROLLO NUMERICITA' CAMPI
050200010330     C*----------------------------------------------------*
050300010330     C     CHKNUM        BEGSR
050400010330     C*
050500010606     C                   IF        PiDecChr = *blanks
050600010606     C                   EVAL      PiDecChr = ','
050700010606     C                   ENDIF
050800010606     C*
050900010606     C                   CALL(e)   'ISNUMERIC'
051000010330     C                   PARM                    PiStr            30
051100010606     C                   PARM                    PiDecChr          1
051200010330     C                   PARM      *ZEROS        PiVal            30 9
051300010330     C                   PARM      '0'           PiInt             1
051400010330     C                   PARM      '0'           PiNum             1
051500010330     C                   IF        %error
051600010606     C                   EVAL      PiNum=*off
051700010330     C                   ENDIF
051800010330     C*
051900010330     C                   ENDSR
052000010330     C***
052100010330
052200010601
052300010601
052400010601
052500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052600010601     C     repfil        BEGSR
052700010601     C*
052800010601     C                   if        invfil = *zeros and
052900010601     C                             depfil > *zeros and
053000010629     C                             (vinflg = *blanks or
053100010629     C                              vinflg = *zeros)
053200010601     C
053300010601     C                   eval      invfil = depfil
053400010601     C                   endif
053500010601     C*
053600010601     C                   if        depfil <> invfil and
053700010601     C                             invfil > *zeros
053800010601     C                   eval      flgMulti = '1'
053900010601     C                   if        vinflg = *blanks
054000010601     C                   add       1             cntNonEl
054100010601     C                   endif
054200010601     C                   endif
054300010601     C*
054400010601     C                   if        vinflg = '2'
054500010601     C                   eval      flgStato = '2'
054600010601     C                   endif
054700010601     C*
054800010601     C                   ENDSR
054900010601     C***
055000010601
055100010601
055200010601
055300010330
055400010330
055500010330
055600990920      /TITLE Invio dei dati al punto operativo.
055700000613     C     invio         BEGSR
055800990920     C*
055900990920     C                   reset                   dscmz
056000010601     C                   move      invfil        cmzdst
056100990920     C                   eval      cmzfld = vlrfou
056200990920     C                   eval      cmzmbd = vlrhdl
056300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056400000710     C                   if        prmfir = *blanks
056500040723     C                   eval      cmzfla = 'FIVAB00F'
056600040723     C                   eval      cmzmba = 'FIVAB00F'
056700000710     C                   else
056800000710     C                   eval      cmzfla = prmfir
056900000710     C                   eval      cmzmba = prmfir
057000000710     C                   endif
057100990920     C                   eval      cmznrr = *zeros
057200990920     C                   move      §ctrok        cmznrr
057300021018     C                   eval      cmzlba = vlrfl1
057400990920     C                   call(e)   'TIS711C'
057500990920     C                   parm                    dscmz
057600990921     C                   parm      *blanks       esito
057700990920     C                   if        %error
057800990920     C                             or cmzerr = '1'
057900990921     C                             or esito  = '1'
058000000710     C                   eval      wrkesito = '3'
058100990920     C                   endif
058200990920     C*
058300000613     C                   ENDSR
058400990910
058500010601
058600010601
058700010601
058800010601
058900010601      /TITLE Invio dei dati al punto operativo.
059000010601     C     opeini        BEGSR
059100010601     C*
059200010601     C* Inizializzo flag e contatori operativi
059300010601     C                   movel     '0'           flgGiro           1
059400010601     C                   movel     '0'           flgMulti          1
059500010601     C                   movel     '1'           flgStato          1
059600010615     C                   movel     '0'           flgOk             1
059700010601     C                   z-add     *zeros        cntNonEl         10 0
059800010601     C                   z-add     *zeros        depfil            3 0
059900010601     C                   z-add     *zeros        invfil            3 0
060000010601     C*
060100010601     C                   ENDSR
060200010601     C***
060300010601
060400010601
060500010601
060600010330
060700010330
060800000613     C     *inzsr        BEGSR
060900990910     C*
061000990910     C     *entry        plist
061100990920     C                   parm                    tivlrds
061200990921     C                   parm      wrkesito      esito
061300000724     C                   parm                    prmlit
061400000710     C                   parm                    prmfir
061500010330     C*
061600010330     C* CALCOLA LA DATA CORRENTE
061700010330     C                   time                    wn14             14 0
061800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061900010330     C                   z-add     wn8           g08dat
062000010330     C                   z-add     *zeros        g08inv
062100010330     C                   movel     '0'           g08err
062200010330     C                   call      'XSRDA8'
062300010330     C                   parm                    wlbda8
062400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062500000613     C*
062600000613     C                   ENDSR
062700000613     C***
062800990908
062900040723     Otitvd9p   E            riepilogo         2
063000990915     O                                              'Upload via Internet'
063100990915     O                                           +1 'Traduzione TIVIN00R -'
063200040723     O                                           +1 'FIVABWWR'
063300990915     O                                           +1 'Testate bolle'
063400990915     O          E            riepilogo   2
063500990915     O                       wrkdata
063600990915     O                       wrkora              +1
063700990915     O                       procname            +1
063800990915     O          E            riepilogo   2
063900990915     O                                              'Cliente..................:'
064000010605     O                       VABCCM        z     +1
064100990915     O          E            riepilogo   2
064200990920     O                                              'Riferimento Strategi.....:'
064300990920     O                       vlrhdl              +1
064400990915     O          E            riepilogo   2
064500990915     O                                              'Giusti...................:'
064600971022     O                       §CTROK        2   +  1
064700990915     O          E            riepilogo   2
064800990915     O                                              'Sbagliati e corretti.....:'
064900971022     O                       §CTRMO        2   +  1
065000990915     O          E            riepilogo   2
065100990915     O                                              'Sbagliati e scartati.....:'
065200971022     O                       §CTRNO        2   +  1
065300000613
065400040723     Otitvd9ps  E            testdett          2
065500000613     O                                              'Upload via Internet'
065600000613     O                                           +1 'Traduzione TIVIN00R -'
065700040723     O                                           +1 'FIVABWWR'
065800000613     O                                           +1 'Testate bolle'
065900000616     O          E            testdett    3
066000000613     O                                           +2 'N° rec'
066100000613     O                                           +3 'Anteprima contenuto'
066200000616     O          E            rigadett    2
066300000613     O                       rrnum               +2
066400000621     O                       recko               +3
066500000616     O          E            findett     2
066600000613     O                       wrkdata
066700000613     O                       wrkora              +1
066800000613     O                       procname            +1
066900000616     O          E            findett     2
067000000613     O                                              'Cliente..................:'
067100010605     O                       VABCCM        z     +1
067200000616     O          E            findett     2
067300000613     O                                              'Riferimento Strategi.....:'
067400000613     O                       vlrhdl              +1
067500000616     O          E            findett     2
067600000613     O                                              'Giusti...................:'
067700000613     O                       §CTROK        2   +  1
067800000616     O          E            findett     2
067900000613     O                                              'Sbagliati e corretti.....:'
068000000613     O                       §CTRMO        2   +  1
068100000616     O          E            findett     2
068200000613     O                                              'Sbagliati e scartati.....:'
068300000613     O                       §CTRNO        2   +  1
068400000613** CMD - COMANDI CL
068500040723OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVD9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068600040723OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVD9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068700040723DLTOVR FILE(TITVD9P TITVD9PS) LVL(*)
068800010606
068900000613
