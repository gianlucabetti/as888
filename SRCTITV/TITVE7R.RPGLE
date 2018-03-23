000100040923      /TITLE Upload via Internet: traduzione in FIVABWWR. (x ROCHER Cod. 0500147)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700040923     Ftitve7p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040923     Ftitve7ps  O    f  198        PRINTER usropn
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
009500040923     C                   if        not %open(titve7ps)
009600040923     C                   open      titve7ps
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
011400040923     C                   if        %open(titve7ps)
011500000616     C                   except    findett
011600040923     C                   close     titve7ps
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
013500040923     C                   if        not %open(titve7p)
013600040923     C                   open      titve7p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100040923     C                   if        %open(titve7p)
014200040923     C                   close     titve7p
014300990914     C                   endif
014400000613     C*
014500000613     C                   ENDSR
014600000613     C***
014700000613
014800010601
014900010601
015000010330     C*--------------------------------------------------------
015100040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
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
021000040301     C* Scarico il buffer del FIVAB
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
030300040923     C                   EVAL      VABCCM = 0500147
030400040923     C                   EVAL      VABLNP = 050
030500040713     C                   EVAL      VABCTR = 000
030600040923     C                   EVAL      VABCBO = '1'
030700020204     C* ... e poi verifico se sono stati passati come parametri
030800020204     C                   IF        vlrppt > *blanks
030900040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031100020204     C                   EXSR      CHKNUM
031200020204     C                   IF        PiInt=*on
031300020204     C                   Z-ADD     PiVal         VABCCM
031400020204     C                   ENDIF
031500040301     C                   ENDIF
031600040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031800020204     C                   EXSR      CHKNUM
031900020204     C                   IF        PiInt=*on
032000020204     C                   Z-ADD     PiVal         VABLNP
032100020204     C                   ENDIF
032200040301     C                   ENDIF
032300040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032500020204     C                   EXSR      CHKNUM
032600020204     C                   IF        PiInt=*on
032700020204     C                   Z-ADD     PiVal         VABCTR
032800040301     C                   ENDIF
032900020204     C                   ENDIF
033000020204     C                   ENDIF
033100020204     C*
033200010330     C                   ENDSR
033300010607     C*----------------------------------------------------*
033400021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033500010607     C*----------------------------------------------------*
033600010607     C     IMPVAB        BEGSR
033700010607     C*
033800010607     C                   SETOFF                                       3132
033900010607     C*
034000010607     C* Reperimento campi ALFA
034100010607     C*
034200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034300040923     C                   EVAL      VABRSD=%trim(%subst(vindta:18:30))
034400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034600020117     C* ==
034700040923     C                   EVAL      VABIND=%trim(%subst(vindta:48:30))
034800040923     C                   EVAL      VABLOD=%trim(%subst(vindta:78:30))
034900040923     C                   EVAL      VABPRD=%trim(%subst(vindta:108:2))
035000040923     C                   EVAL      VABRMA=%trim(%subst(vindta:3:9))
035100010607     C*
035200010607     C* Reperimento campi NUMERICI
035300020722     C                   MOVEL     DATCOR        VABAAS
035400020722     C                   MOVE      DATCOR        VABMGS
035500040420     C* NSP/RMN
035600040923     C                   EVAL      PiStr=%trim(%subst(vindta:3:9))
035700020304     C                   EXSR      CHKNUM
035800020304     C                   IF        PiInt=*on
035900040301     C                   Z-ADD     PiVal         VABNSP
036000040301     C                   Z-ADD     PiVal         VABRMN
036100020304     C                   ELSE
036200020722     C                   SETON                                        31
036300020304     C                   Z-ADD     *zeros        VABNSP
036400040301     C                   Z-ADD     *zeros        VABRMN
036500020304     C                   EVAL      vinmsg = %trimr(vinmsg)
036600040301     C                             + ' ' + 'VABNSP VABRMN'
036700020304     C                   ENDIF
036800040420     C* CAD
036900040923     C                   EVAL      PiStr=%trim(%subst(vindta:110:5))
037000010607     C                   EXSR      CHKNUM
037100010607     C                   IF        PiInt=*on
037200020204     C                   Z-ADD     PiVal         Num5_0
037300020204     C                   MOVEL(p)  Num5_0        VABCAD
037400010607     C                   ELSE
037500010607     C                   SETON                                        32
037600020204     C                   EVAL      VABCAD = *zeros
037700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037800020204     C                             + ' ' + 'VABCAD'
037900010607     C                   ENDIF
038000040420     C* NCL
038100040923     C                   EVAL      PiStr=%trim(%subst(vindta:115:5))
038200010607     C                   EXSR      CHKNUM
038300010607     C                   IF        PiInt=*on
038400010607     C                   Z-ADD     PiVal         VABNCL
038500010607     C                   ELSE
038600010607     C                   SETON                                        32
038700010607     C                   Z-ADD     *zeros        VABNCL
038800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038900010607     C                             + ' ' + 'VABNCL'
039000010607     C                   ENDIF
039100040420     C* PKB
039200040923     C                   EVAL      PiStr=%trim(%subst(vindta:120:7))
039300010607     C                   EXSR      CHKNUM
039400010607     C                   IF        PiNum=*on
039500040420     C                   Z-ADD     PiVal         VABPKB
039600010607     C                   ELSE
039700010607     C                   SETON                                        32
039800010607     C                   Z-ADD     *zeros        VABPKB
039900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040000010607     C                             + ' ' + 'VABPKB'
040100010607     C                   ENDIF
040200020322     C*
040300020322     C* Considerazioni finali su CBO/CAS
040400020322     C                   IF        FlgCAS = '1'
040500040713     C                   IF        VABCBO = '1'
040600020322     C                   EVAL      VABCBO = '4'
040700040713     C                   ENDIF
040800040713     C                   IF        VABCBO = '2'
040900040713     C                   EVAL      VABCBO = '6'
041000040713     C                   ENDIF
041100020322     C                   ENDIF
041200020204     C*
041300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041400020204     C                   EXSR      CHKIMPDIV
041500020204     C*
041600010607     C                   ENDSR
041700010607     C*----------------------------------------------------*
041800010601
041900020204
042000020204     C*----------------------------------------------------*
042100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
042200020204     C*----------------------------------------------------*
042300020204     C     CHKIMPDIV     BEGSR
042400020204     C*
042500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
042600020204     C                   Z-ADD     *zeros        wrkDec            9 9
042700020204     C*
042800020204     C* Come prima cosa effettuo considerazioni sulla divisa
042900020204     C                   IF        vabIAS > *zeros
043000020204     C                   IF        vabVAS <> 'EUR'
043100020204     C                   EVAL      vabVAS =  'ITL'
043200020204     C                   ENDIF
043300020204     C                   ENDIF
043400020204     C*
043500020204     C                   IF        vabCAS > *zeros
043600020204     C                   IF        vabVCA <> 'EUR'
043700020204     C                   EVAL      vabVCA =  'ITL'
043800020204     C                   ENDIF
043900020204     C                   ENDIF
044000020204     C*
044100020204     C                   IF        vabVMD > *zeros
044200020321     C                   IF        vabVAD <> 'EUR'
044300020204     C                   EVAL      vabVAD =  'ITL'
044400020204     C                   ENDIF
044500020204     C                   ENDIF
044600020204     C*
044700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
044800020204     C                   Z-ADD     vabIAS        wrkDec
044900020204     C                   IF        wrkDec > *zeros
045000020204     C                   IF        vabVAS = 'ITL'
045100020204     C                   EVAL      vabIAS = *zeros
045200020204     C                   ENDIF
045300020204     C                   ENDIF
045400020204     C*
045500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
045600020204     C                   Z-ADD     vabCAS        wrkDec
045700020204     C                   IF        wrkDec > *zeros
045800020204     C                   IF        vabVCA = 'ITL'
045900020204     C                   EVAL      vabCAS = *zeros
046000020204     C                   ENDIF
046100020204     C                   ENDIF
046200020204     C*
046300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
046400020204     C                   Z-ADD     vabVMD        wrkDec
046500020204     C                   IF        wrkDec > *zeros
046600020204     C                   IF        vabVAD = 'ITL'
046700020204     C                   EVAL      vabVMD = *zeros
046800020204     C                   ENDIF
046900020204     C                   ENDIF
047000020204     C*
047100020204     C                   ENDSR
047200020204     C***
047300020204
047400010330
047500010330
047600010330     C*----------------------------------------------------*
047700010330     C*  CONTROLLO NUMERICITA' CAMPI
047800010330     C*----------------------------------------------------*
047900010330     C     CHKNUM        BEGSR
048000010330     C*
048100010606     C                   IF        PiDecChr = *blanks
048200010606     C                   EVAL      PiDecChr = ','
048300010606     C                   ENDIF
048400010606     C*
048500010606     C                   CALL(e)   'ISNUMERIC'
048600010330     C                   PARM                    PiStr            30
048700010606     C                   PARM                    PiDecChr          1
048800010330     C                   PARM      *ZEROS        PiVal            30 9
048900010330     C                   PARM      '0'           PiInt             1
049000010330     C                   PARM      '0'           PiNum             1
049100010330     C                   IF        %error
049200010606     C                   EVAL      PiNum=*off
049300010330     C                   ENDIF
049400010330     C*
049500010330     C                   ENDSR
049600010330     C***
049700010330
049800010601
049900010601
050000010601
050100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
050200010601     C     repfil        BEGSR
050300010601     C*
050400010601     C                   if        invfil = *zeros and
050500010601     C                             depfil > *zeros and
050600010629     C                             (vinflg = *blanks or
050700010629     C                              vinflg = *zeros)
050800010601     C
050900010601     C                   eval      invfil = depfil
051000010601     C                   endif
051100010601     C*
051200010601     C                   if        depfil <> invfil and
051300010601     C                             invfil > *zeros
051400010601     C                   eval      flgMulti = '1'
051500010601     C                   if        vinflg = *blanks
051600010601     C                   add       1             cntNonEl
051700010601     C                   endif
051800010601     C                   endif
051900010601     C*
052000010601     C                   if        vinflg = '2'
052100010601     C                   eval      flgStato = '2'
052200010601     C                   endif
052300010601     C*
052400010601     C                   ENDSR
052500010601     C***
052600010601
052700010601
052800010601
052900010330
053000010330
053100010330
053200990920      /TITLE Invio dei dati al punto operativo.
053300000613     C     invio         BEGSR
053400990920     C*
053500990920     C                   reset                   dscmz
053600010601     C                   move      invfil        cmzdst
053700990920     C                   eval      cmzfld = vlrfou
053800990920     C                   eval      cmzmbd = vlrhdl
053900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
054000000710     C                   if        prmfir = *blanks
054100021014     C                   eval      cmzfla = 'FIVAB00F'
054200021014     C                   eval      cmzmba = 'FIVAB00F'
054300000710     C                   else
054400000710     C                   eval      cmzfla = prmfir
054500000710     C                   eval      cmzmba = prmfir
054600000710     C                   endif
054700990920     C                   eval      cmznrr = *zeros
054800990920     C                   move      §ctrok        cmznrr
054900021018     C                   eval      cmzlba = vlrfl1
055000990920     C                   call(e)   'TIS711C'
055100990920     C                   parm                    dscmz
055200990921     C                   parm      *blanks       esito
055300990920     C                   if        %error
055400990920     C                             or cmzerr = '1'
055500990921     C                             or esito  = '1'
055600000710     C                   eval      wrkesito = '3'
055700990920     C                   endif
055800990920     C*
055900000613     C                   ENDSR
056000990910
056100010601
056200010601
056300010601
056400010601
056500010601      /TITLE Invio dei dati al punto operativo.
056600010601     C     opeini        BEGSR
056700010601     C*
056800010601     C* Inizializzo flag e contatori operativi
056900010601     C                   movel     '0'           flgGiro           1
057000010601     C                   movel     '0'           flgMulti          1
057100010601     C                   movel     '1'           flgStato          1
057200010615     C                   movel     '0'           flgOk             1
057300010601     C                   z-add     *zeros        cntNonEl         10 0
057400010601     C                   z-add     *zeros        depfil            3 0
057500010601     C                   z-add     *zeros        invfil            3 0
057600010601     C*
057700010601     C                   ENDSR
057800010601     C***
057900010601
058000010601
058100010601
058200010330
058300010330
058400000613     C     *inzsr        BEGSR
058500990910     C*
058600990910     C     *entry        plist
058700990920     C                   parm                    tivlrds
058800990921     C                   parm      wrkesito      esito
058900000724     C                   parm                    prmlit
059000000710     C                   parm                    prmfir
059100010330     C*
059200010330     C* CALCOLA LA DATA CORRENTE
059300010330     C                   time                    wn14             14 0
059400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059500010330     C                   z-add     wn8           g08dat
059600010330     C                   z-add     *zeros        g08inv
059700010330     C                   movel     '0'           g08err
059800010330     C                   call      'XSRDA8'
059900010330     C                   parm                    wlbda8
060000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060100000613     C*
060200000613     C                   ENDSR
060300000613     C***
060400990908
060500040923     Otitve7p   E            riepilogo         2
060600990915     O                                              'Upload via Internet'
060700990915     O                                           +1 'Traduzione TIVIN00R -'
060800021014     O                                           +1 'FIVABWWR'
060900990915     O                                           +1 'Testate bolle'
061000990915     O          E            riepilogo   2
061100990915     O                       wrkdata
061200990915     O                       wrkora              +1
061300990915     O                       procname            +1
061400990915     O          E            riepilogo   2
061500990915     O                                              'Cliente..................:'
061600010605     O                       VABCCM        z     +1
061700990915     O          E            riepilogo   2
061800990920     O                                              'Riferimento Strategi.....:'
061900990920     O                       vlrhdl              +1
062000990915     O          E            riepilogo   2
062100990915     O                                              'Giusti...................:'
062200971022     O                       §CTROK        2   +  1
062300990915     O          E            riepilogo   2
062400990915     O                                              'Sbagliati e corretti.....:'
062500971022     O                       §CTRMO        2   +  1
062600990915     O          E            riepilogo   2
062700990915     O                                              'Sbagliati e scartati.....:'
062800971022     O                       §CTRNO        2   +  1
062900000613
063000040923     Otitve7ps  E            testdett          2
063100000613     O                                              'Upload via Internet'
063200000613     O                                           +1 'Traduzione TIVIN00R -'
063300021014     O                                           +1 'FIVABWWR'
063400000613     O                                           +1 'Testate bolle'
063500000616     O          E            testdett    3
063600000613     O                                           +2 'N° rec'
063700000613     O                                           +3 'Anteprima contenuto'
063800000616     O          E            rigadett    2
063900000613     O                       rrnum               +2
064000000621     O                       recko               +3
064100000616     O          E            findett     2
064200000613     O                       wrkdata
064300000613     O                       wrkora              +1
064400000613     O                       procname            +1
064500000616     O          E            findett     2
064600000613     O                                              'Cliente..................:'
064700010605     O                       VABCCM        z     +1
064800000616     O          E            findett     2
064900000613     O                                              'Riferimento Strategi.....:'
065000000613     O                       vlrhdl              +1
065100000616     O          E            findett     2
065200000613     O                                              'Giusti...................:'
065300000613     O                       §CTROK        2   +  1
065400000616     O          E            findett     2
065500000613     O                                              'Sbagliati e corretti.....:'
065600000613     O                       §CTRMO        2   +  1
065700000616     O          E            findett     2
065800000613     O                                              'Sbagliati e scartati.....:'
065900000613     O                       §CTRNO        2   +  1
066000000613** CMD - COMANDI CL
066100040923OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVE7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
066200040923OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVE7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
066300040923DLTOVR FILE(TITVE7P TITVE7PS) LVL(*)
066400010606
066500000613
