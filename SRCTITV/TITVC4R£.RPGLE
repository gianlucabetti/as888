000100040607      /TITLE Upload via Internet: traduzione in EDIVABWR. (x FESTO Cod. 0891154)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600031121     FEDIVABwr  O    E             DISK    usropn
000700040607     Ftitvc4p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040607     Ftitvc4ps  O    f  198        PRINTER usropn
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
003300031121     D wNomeFile       s             30    INZ(*blanks)
003400010330     D*------------------
003500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600010330     D*------------------
003700010330     D WLBDA8          DS                  INZ
003800010330     D  G08DAT                 1      8  0
003900010330     D  G08INV                 9     16  0
004000010330     D  G08ERR                17     17
004100010330     D  G08TGI                18     22  0
004200000313     D*
004300010330
004400010330
004500010330
004600990915     C                   time                    wrkdata
004700990915     C                   time                    wrkora
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100010601     C*
005200010601     C                   exsr      opeini
005300010605     C                   exsr      rwvab
005400010601     C*
005500010601     C                   seton                                        lr
005600010601
005700010601
005800010601
005900010601
006000010601     C*--------------------------------------------------------
006100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006200010601     C*--------------------------------------------------------
006300010601     C     PREELA        BEGSR
006400010601     C*
006500010601     C* SE OCCORRE SPEDIRE IN FILIALE
006600010601     C                   if        invfil <> *zeros and
006700010601     C                             flgGiro = '0'
006800010601     C*
006900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007000010601     C                   eval      flgGiro = '1'
007100000724     C*
007200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007400010601     C     invfil        chain     azorg01l
007500000724     C                   if        %found
007600000724     C                   movel(p)  CMD(1)        depcmd
007700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007800000724     C*
007900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008000011113     C                   Z-ADD     150           LENGH            15 5
008100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200000724     C                   PARM                    depcmd
008300000724     C                   PARM                    LENGH
008400000724     C*
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000613     C*
009700040607     C                   if        not %open(titvc4ps)
009800040607     C                   open      titvc4ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100010601     C*
010200010601     C                   endif
010300010601     C*
010400010601     C                   ENDSR
010500010601     C***
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011500010601     C*--------------------------------------------------------
011600010601     C     ENDELA        BEGSR
011700990915     C*
011800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011900050131     C*
012000050131     C* Effettuo lancio TISI95 solo x chiusura
012100050131     C                   CLEAR                   TISI95DS
012200050131     C                   EVAL      I95TLA = 'C'
012300050131     C                   CALL      'TISI95R'
012400050131     C                   PARM                    TISI95DS
012500000613     C*
012600040607     C                   if        %open(titvc4ps)
012700000616     C                   except    findett
012800040607     C                   close     titvc4ps
012900000613     C                   endif
013000000616     C*
013100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013400000616     C                   PARM                    CMD(3)
013500000616     C                   PARM                    LENGH
013600000616     C*
013700010601     C                   ENDSR
013800010601     C***
013900010601
014000010601
014100010601
014200010601
014300000613
014400000613     C*--------------------------------------------------------
014500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014600000613     C*--------------------------------------------------------
014700000621     C     STPR          BEGSR
014800000613     C*
014900040607     C                   if        not %open(titvc4p)
015000040607     C                   open      titvc4p
015100990915     C                   endif
015200990915     C*
015300990915     C                   except    riepilogo
015400990915     C*
015500040607     C                   if        %open(titvc4p)
015600040607     C                   close     titvc4p
015700990914     C                   endif
015800000613     C*
015900000613     C                   ENDSR
016000000613     C***
016100000613
016200010330
016300010601
016400010601
016500010601
016600010330     C*--------------------------------------------------------
016700031121     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
016800010330     C*--------------------------------------------------------
016900010605     C     RWVAB         BEGSR
017000010330     C*
017100010330     C                   if        not %open(tivin00r)
017200010330     C                   open      tivin00r
017300010330     C                   endif
017400031121     C                   if        not %open(edivabwr)
017500031121     C                   open      edivabwr
017600010330     C                   endif
017700010330     C*
017800010604     C                   clear                   §CTROK
017900010604     C                   clear                   §CTRMO
018000010604     C                   clear                   §CTRNO
018100010330     C*
018200010330     C                   DO        *HIVAL
018300010330     C*
018400010330     C                   READ      tivin00r                               70
018500010618     C*
018600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018700010618     C                   if        vinflg = '1'
018800010618     C                   eval      flgOk = '1'
018900010618     C                   endif
019000010618     C*
019100010330     C                   if        vindta > *blanks
019200010330     C                   add       1             rrnum
019300010330     C*
019400010601     C                   if        *in70 = *off and
019500010330     C                             (vinflg = *blanks
019600010330     C                              or vinflg = '0'
019700010330     C                              or vinflg = '2')
019800010330     C*
019900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020000010711     C                   if        vinflg = *blanks or vinflg = '0'
020100010711     C                   clear                   vinmsg
020200010711     C                   endif
020300010601     C*
020400010330     C                   exsr      inzvar
020500010330     C                   exsr      defcam
020600010605     C                   exsr      impvab
020700010601     C*
020800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020900010605     C                   eval      depfil = VABLNP
021000010601     C                   exsr      repfil
021100010601     C                   if        depfil = invfil
021200021025     C                   if        vlrpoi = 999
021300021025     C                   MOVE(P)   invfil        VABFGS
021400021025     C                   else
021500021025     C                   MOVE(P)   vlrpoi        VABFGS
021600021025     C                   endif
021700010601     C*
021800010601     C                   exsr      PREELA
021900010601     C*
022000010604     C* Ebbene...
022100010604     C*
022200010604     C  N31              ADD       1             §CTROK            7 0
022300010604     C   32              ADD       1             §CTRMO            7 0
022400010604     C   31              ADD       1             §CTRNO            7 0
022500020205     C*
022600020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
022700031121     C  N31              WRITE     EDIVAB00
022800010604     C*
022900010604     C                   if        *in31 = *off and
023000010604     C                             *in32 = *off
023100010604     C                   eval      vinflg = '1'
023200010604     C                   else
023300010604     C                   eval      recko = vindta
023400010604     C                   except    rigadett
023500010604     C                   eval      vinflg = '2'
023600010604     C                   endif
023700010604     C                   endif
023800010601     C*
023900010604     C                   endif
024000010604     C*
024100010330     C                   else
024200010330     C                   eval      vinflg = '1'
024300010330     C                   endif
024400010601     C*
024500010601     C  N70              update    tivin000
024600010330     C*
024700010330     C  N70              ENDdo
024800010601     C
024900010601     C*
025000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025100010601     C                   if        cntNonEl = *zeros or
025200010601     C                             flgMulti = '0'
025300010330     C* Se non ci sono record con errori ...
025400010601     C                   if        §ctrno = 0 and
025500010604     C                             §ctrmo = 0 and
025600010601     C                             flgStato <> '2'
025700010330     C* ... restituisco esito OK.
025800010330     C                   eval      wrkesito = '0'
025900010330     C                   else
026000010330     C                   if        §ctrok > 0
026100010330     C                   eval      wrkesito = '1'
026200010330     C                   else
026300010615     C                   if        flgOk = '0'
026400010615     C                   eval      wrkesito = '2'
026500010615     C                   else
026600010615     C                   eval      wrkesito = '6'
026700010615     C                   endif
026800010330     C                   endif
026900010330     C                   endif
027000010601     C                   else
027100010601     C                   eval      wrkesito = '9'
027200010601     C                   endif
027300010330     C*
027400010330     C                   if        %open(tivin00r)
027500010330     C                   close     tivin00r
027600010330     C                   endif
027700031121     C                   if        %open(edivabwr)
027800031121     C                   close     edivabwr
027900010330     C                   endif
028000010601     C*
028100010601     C                   if        vlrpoi <> 999
028200010601     C                   eval      invfil = vlrpoi
028300010601     C                   endif
028400010330     C*
028500010330     C                   if        §ctrok > 0
028600010601     C                             and invfil > *zeros
028700010330     C                   exsr      invio
028800010330     C                   endif
028900010601     C*
029000010618     C                   if        flgGiro = '1'
029100010601     C                   exsr      endela
029200010618     C                   endif
029300010330     C*
029400010330     C                   ENDSR
029500010330     C***
029600010330
029700010601
029800010601
029900010601
030000010601
030100010330     C*----------------------------------------------------*
030200010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030300010330     C*----------------------------------------------------*
030400040607     C     INZVAR        BEGSR
030500010330     C*
030600020204     C                   Z-ADD     *zeros        Num5_0            5 0
030700020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030800020322     C                   MOVEL     '0'           FlgCAS            1
030900010330     C*
031000010330     C                   ENDSR
031100010330     C*----------------------------------------------------*
031200040607     C*  IMPOSTAZIONE CAMPI COSTANTI
031300010330     C*----------------------------------------------------*
031400010330     C     DEFCAM        BEGSR
031500010330     C*
031600020204     C* Inizializzo il buffer del record da scrivere
031700031121     C                   CLEAR                   EDIVAB00
031800020204     C* Imposto i valori di default...
031900040607     C                   EVAL      VABCCM = 0891154
032000020403     C                   EVAL      VABCTM = '7Q'
032100040607     C                   EVAL      VABLNP = 089
032200020322     C                   EVAL      VABCTR = 000
032300040607     C                   EVAL      VABTSP = 'C'
032400020204     C* ... e poi verifico se sono stati passati come parametri
032500020204     C                   IF        vlrppt > *blanks
032600040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABCCM
033100020204     C                   ENDIF
033200040607     C                   ENDIF
033300040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
033400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033500020204     C                   EXSR      CHKNUM
033600020204     C                   IF        PiInt=*on
033700020204     C                   Z-ADD     PiVal         VABLNP
033800020204     C                   ENDIF
033900040607     C                   ENDIF
034000040607     C                   IF        %subst(vlrppt:11:3) <> *blanks
034100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034200020204     C                   EXSR      CHKNUM
034300020204     C                   IF        PiInt=*on
034400020204     C                   Z-ADD     PiVal         VABCTR
034500020204     C                   ENDIF
034600040607     C                   ENDIF
034700020204     C                   ENDIF
034800020204     C*
034900010330     C                   ENDSR
035000010607     C*----------------------------------------------------*
035100031121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
035200010607     C*----------------------------------------------------*
035300010607     C     IMPVAB        BEGSR
035400010607     C*
035500010607     C                   SETOFF                                       3132
035600010607     C*
035700010607     C* Reperimento campi ALFA
035800010607     C*
035900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
036000040607     C                   EVAL      VABRSD=%trim(%subst(vindta:164:30))
036100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
036300020117     C* ==
036400040607     C                   EVAL      VABRD2=%trim(%subst(vindta:194:30))
036500040607     C                   EVAL      VABIND=%trim(%subst(vindta:224:25))
036600040607     C                   EVAL      VABLOD=%trim(%subst(vindta:254:25))
036700040607     C                   EVAL      VABRMA=%trim(%subst(vindta:10:7))
036800020329     C*
036900020329     C                   MOVEL     DATCOR        VABAAS
037000020329     C                   MOVE      DATCOR        VABMGS
037100010607     C*
037200010607     C* Reperimento campi NUMERICI
037300040607     C* NSP/RMN
037400040607     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
037500020304     C                   EXSR      CHKNUM
037600020304     C                   IF        PiInt=*on
037700020304     C                   Z-ADD     PiVal         VABNSP
037800040607     C                   Z-ADD     PiVal         VABRMN
037900020304     C                   ELSE
038000020304     C                   SETON                                        32
038100020304     C                   Z-ADD     *zeros        VABNSP
038200040607     C                   Z-ADD     1             VABRMN
038300020304     C                   EVAL      vinmsg = %trimr(vinmsg)
038400040607     C                             + ' ' + 'VABNSP VABRMN'
038500020304     C                   ENDIF
038600040607     C* CAD
038700040607     C                   EVAL      PiStr=%trim(%subst(vindta:249:5))
038800010607     C                   EXSR      CHKNUM
038900010607     C                   IF        PiInt=*on
039000020204     C                   Z-ADD     PiVal         Num5_0
039100020204     C                   MOVEL(p)  Num5_0        VABCAD
039200010607     C                   ELSE
039300010607     C                   SETON                                        32
039400020204     C                   EVAL      VABCAD = *zeros
039500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039600020204     C                             + ' ' + 'VABCAD'
039700010607     C                   ENDIF
039800040607     C* Reperisco la provincia dal CAP e dalla località
039900040607     C                   IF        VABCAD <> *blanks
040000040607     C                   CLEAR                   TISI95DS
040100040607     C                   EVAL      I95TCN = '3'
040200040607     C                   Z-ADD     datcor        I95DAT
040300040607     C                   EVAL      I95CAP = VABCAD
040400040607     C                   EVAL      I95LOC = VABLOD
040500040607     C                   CALL      'TISI95R'
040600040607     C                   PARM                    TISI95DS
040700040607     C                   EVAL      VABPRD = O95PRV
040800040607     C                   ENDIF
040900040607     C* NCL
041000040607     C                   EVAL      PiStr=%trim(%subst(vindta:557:4))
041100010607     C                   EXSR      CHKNUM
041200010607     C                   IF        PiInt=*on
041300010607     C                   Z-ADD     PiVal         VABNCL
041400010607     C                   ELSE
041500010607     C                   SETON                                        32
041600010607     C                   Z-ADD     *zeros        VABNCL
041700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041800010607     C                             + ' ' + 'VABNCL'
041900010607     C                   ENDIF
042000040607     C* PKB
042100040607     C                   EVAL      PiStr=%trim(%subst(vindta:561:7))
042200020204     C                   EVAL      PiDecChr = ','
042300010607     C                   EXSR      CHKNUM
042400010607     C                   IF        PiNum=*on
042500040607     C                   Z-ADD     PiVal         VABPKB
042600010607     C                   ELSE
042700010607     C                   SETON                                        32
042800010607     C                   Z-ADD     *zeros        VABPKB
042900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043000010607     C                             + ' ' + 'VABPKB'
043100010607     C                   ENDIF
043200020322     C*
043300020322     C* Considerazioni finali su CBO/CAS
043400020322     C                   IF        FlgCAS = '1'
043500020322     C                   EVAL      VABCBO = '4'
043600020322     C                   ELSE
043700020322     C                   EVAL      VABCBO = '1'
043800020322     C                   ENDIF
043900020204     C*
044000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044100020204     C                   EXSR      CHKIMPDIV
044200031121     C*
044300031121     C* VALORIZZO CAMPI RELATIVI AL "CMR"
044400031121     C                   MOVE(P)   vlrMSG        wNomeFile
044500040607     C                   EVAL      VABCMR = %subst(wNomeFile:3:6)
044600031121     C                   EVAL      VABDCM = DATCOR
044700031121     C                   EVAL      VABDTS = DATCOR
044800031121     C                   EVAL      VABCNT = 1
044900020204     C*
045000010607     C                   ENDSR
045100010607     C*----------------------------------------------------*
045200010601
045300020204
045400020204     C*----------------------------------------------------*
045500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045600020204     C*----------------------------------------------------*
045700020204     C     CHKIMPDIV     BEGSR
045800020204     C*
045900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046000020204     C                   Z-ADD     *zeros        wrkDec            9 9
046100020204     C*
046200020204     C* Come prima cosa effettuo considerazioni sulla divisa
046300020204     C                   IF        vabIAS > *zeros
046400020204     C                   IF        vabVAS <> 'EUR'
046500020204     C                   EVAL      vabVAS =  'ITL'
046600020204     C                   ENDIF
046700020204     C                   ENDIF
046800020204     C*
046900020204     C                   IF        vabCAS > *zeros
047000020204     C                   IF        vabVCA <> 'EUR'
047100020204     C                   EVAL      vabVCA =  'ITL'
047200020204     C                   ENDIF
047300020204     C                   ENDIF
047400020204     C*
047500020204     C                   IF        vabVMD > *zeros
047600020321     C                   IF        vabVAD <> 'EUR'
047700020204     C                   EVAL      vabVAD =  'ITL'
047800020204     C                   ENDIF
047900020204     C                   ENDIF
048000020204     C*
048100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048200020204     C                   Z-ADD     vabIAS        wrkDec
048300020204     C                   IF        wrkDec > *zeros
048400020204     C                   IF        vabVAS = 'ITL'
048500020204     C                   EVAL      vabIAS = *zeros
048600020204     C                   ENDIF
048700020204     C                   ENDIF
048800020204     C*
048900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049000020204     C                   Z-ADD     vabCAS        wrkDec
049100020204     C                   IF        wrkDec > *zeros
049200020204     C                   IF        vabVCA = 'ITL'
049300020204     C                   EVAL      vabCAS = *zeros
049400020204     C                   ENDIF
049500020204     C                   ENDIF
049600020204     C*
049700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049800020204     C                   Z-ADD     vabVMD        wrkDec
049900020204     C                   IF        wrkDec > *zeros
050000020204     C                   IF        vabVAD = 'ITL'
050100020204     C                   EVAL      vabVMD = *zeros
050200020204     C                   ENDIF
050300020204     C                   ENDIF
050400020204     C*
050500020204     C                   ENDSR
050600020204     C***
050700020204
050800010330
050900010330
051000010330     C*----------------------------------------------------*
051100010330     C*  CONTROLLO NUMERICITA' CAMPI
051200010330     C*----------------------------------------------------*
051300010330     C     CHKNUM        BEGSR
051400010330     C*
051500010606     C                   IF        PiDecChr = *blanks
051600010606     C                   EVAL      PiDecChr = ','
051700010606     C                   ENDIF
051800010606     C*
051900010606     C                   CALL(e)   'ISNUMERIC'
052000010330     C                   PARM                    PiStr            30
052100010606     C                   PARM                    PiDecChr          1
052200010330     C                   PARM      *ZEROS        PiVal            30 9
052300010330     C                   PARM      '0'           PiInt             1
052400010330     C                   PARM      '0'           PiNum             1
052500010330     C                   IF        %error
052600010606     C                   EVAL      PiNum=*off
052700010330     C                   ENDIF
052800010330     C*
052900010330     C                   ENDSR
053000010330     C***
053100010330
053200010601
053300010601
053400010601
053500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053600010601     C     repfil        BEGSR
053700010601     C*
053800010601     C                   if        invfil = *zeros and
053900010601     C                             depfil > *zeros and
054000010629     C                             (vinflg = *blanks or
054100010629     C                              vinflg = *zeros)
054200010601     C
054300010601     C                   eval      invfil = depfil
054400010601     C                   endif
054500010601     C*
054600010601     C                   if        depfil <> invfil and
054700010601     C                             invfil > *zeros
054800010601     C                   eval      flgMulti = '1'
054900010601     C                   if        vinflg = *blanks
055000010601     C                   add       1             cntNonEl
055100010601     C                   endif
055200010601     C                   endif
055300010601     C*
055400010601     C                   if        vinflg = '2'
055500010601     C                   eval      flgStato = '2'
055600010601     C                   endif
055700010601     C*
055800010601     C                   ENDSR
055900010601     C***
056000010601
056100010601
056200010601
056300010330
056400010330
056500010330
056600990920      /TITLE Invio dei dati al punto operativo.
056700000613     C     invio         BEGSR
056800990920     C*
056900990920     C                   reset                   dscmz
057000010601     C                   move      invfil        cmzdst
057100990920     C                   eval      cmzfld = vlrfou
057200990920     C                   eval      cmzmbd = vlrhdl
057300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
057400000710     C                   if        prmfir = *blanks
057500031121     C                   eval      cmzfla = 'EDIVAB0F'
057600031121     C                   eval      cmzmba = 'EDIVAB0F'
057700000710     C                   else
057800000710     C                   eval      cmzfla = prmfir
057900000710     C                   eval      cmzmba = prmfir
058000000710     C                   endif
058100990920     C                   eval      cmznrr = *zeros
058200990920     C                   move      §ctrok        cmznrr
058300021018     C                   eval      cmzlba = vlrfl1
058400990920     C                   call(e)   'TIS711C'
058500990920     C                   parm                    dscmz
058600990921     C                   parm      *blanks       esito
058700990920     C                   if        %error
058800990920     C                             or cmzerr = '1'
058900990921     C                             or esito  = '1'
059000000710     C                   eval      wrkesito = '3'
059100990920     C                   endif
059200990920     C*
059300000613     C                   ENDSR
059400990910
059500010601
059600010601
059700010601
059800010601
059900010601      /TITLE Invio dei dati al punto operativo.
060000010601     C     opeini        BEGSR
060100010601     C*
060200010601     C* Inizializzo flag e contatori operativi
060300010601     C                   movel     '0'           flgGiro           1
060400010601     C                   movel     '0'           flgMulti          1
060500010601     C                   movel     '1'           flgStato          1
060600010615     C                   movel     '0'           flgOk             1
060700010601     C                   z-add     *zeros        cntNonEl         10 0
060800010601     C                   z-add     *zeros        depfil            3 0
060900010601     C                   z-add     *zeros        invfil            3 0
061000010601     C*
061100010601     C                   ENDSR
061200010601     C***
061300010601
061400010601
061500010601
061600010330
061700010330
061800000613     C     *inzsr        BEGSR
061900990910     C*
062000990910     C     *entry        plist
062100990920     C                   parm                    tivlrds
062200990921     C                   parm      wrkesito      esito
062300000724     C                   parm                    prmlit
062400000710     C                   parm                    prmfir
062500010330     C*
062600010330     C* CALCOLA LA DATA CORRENTE
062700010330     C                   time                    wn14             14 0
062800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062900010330     C                   z-add     wn8           g08dat
063000010330     C                   z-add     *zeros        g08inv
063100010330     C                   movel     '0'           g08err
063200010330     C                   call      'XSRDA8'
063300010330     C                   parm                    wlbda8
063400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063500000613     C*
063600000613     C                   ENDSR
063700000613     C***
063800990908
063900040607     Otitvc4p   E            riepilogo         2
064000990915     O                                              'Upload via Internet'
064100990915     O                                           +1 'Traduzione TIVIN00R -'
064200031121     O                                           +1 'EDIVABWR'
064300990915     O                                           +1 'Testate bolle'
064400990915     O          E            riepilogo   2
064500990915     O                       wrkdata
064600990915     O                       wrkora              +1
064700990915     O                       procname            +1
064800990915     O          E            riepilogo   2
064900990915     O                                              'Cliente..................:'
065000010605     O                       VABCCM        z     +1
065100990915     O          E            riepilogo   2
065200990920     O                                              'Riferimento Strategi.....:'
065300990920     O                       vlrhdl              +1
065400990915     O          E            riepilogo   2
065500990915     O                                              'Giusti...................:'
065600971022     O                       §CTROK        2   +  1
065700990915     O          E            riepilogo   2
065800990915     O                                              'Sbagliati e corretti.....:'
065900971022     O                       §CTRMO        2   +  1
066000990915     O          E            riepilogo   2
066100990915     O                                              'Sbagliati e scartati.....:'
066200971022     O                       §CTRNO        2   +  1
066300000613
066400040607     Otitvc4ps  E            testdett          2
066500000613     O                                              'Upload via Internet'
066600000613     O                                           +1 'Traduzione TIVIN00R -'
066700031121     O                                           +1 'EDIVABWR'
066800000613     O                                           +1 'Testate bolle'
066900000616     O          E            testdett    3
067000000613     O                                           +2 'N° rec'
067100000613     O                                           +3 'Anteprima contenuto'
067200000616     O          E            rigadett    2
067300000613     O                       rrnum               +2
067400000621     O                       recko               +3
067500000616     O          E            findett     2
067600000613     O                       wrkdata
067700000613     O                       wrkora              +1
067800000613     O                       procname            +1
067900000616     O          E            findett     2
068000000613     O                                              'Cliente..................:'
068100010605     O                       VABCCM        z     +1
068200000616     O          E            findett     2
068300000613     O                                              'Riferimento Strategi.....:'
068400000613     O                       vlrhdl              +1
068500000616     O          E            findett     2
068600000613     O                                              'Giusti...................:'
068700000613     O                       §CTROK        2   +  1
068800000616     O          E            findett     2
068900000613     O                                              'Sbagliati e corretti.....:'
069000000613     O                       §CTRMO        2   +  1
069100000616     O          E            findett     2
069200000613     O                                              'Sbagliati e scartati.....:'
069300000613     O                       §CTRNO        2   +  1
069400000613** CMD - COMANDI CL
069500040607OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVC4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
069600040607OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVC4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
069700040607DLTOVR FILE(TITVC4P TITVC4PS) LVL(*)
069800010606
069900000613
