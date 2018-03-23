000100050121      /TITLE Upload via Internet: traduzione in FIVABWWR. (x SNAIDERO Cod. 0980747)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700050121     Ftitvh1p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050121     Ftitvh1ps  O    f  198        PRINTER usropn
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
004300990915     C                   time                    wrkdata
004400990915     C                   time                    wrkora
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010601     C*
004900010601     C                   exsr      opeini
005000010605     C                   exsr      rwvab
005100010601     C*
005200010601     C                   seton                                        lr
005300010601
005400010601
005500010601
005600010601
005700010601     C*--------------------------------------------------------
005800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005900010601     C*--------------------------------------------------------
006000010601     C     PREELA        BEGSR
006100010601     C*
006200010601     C* SE OCCORRE SPEDIRE IN FILIALE
006300010601     C                   if        invfil <> *zeros and
006400010601     C                             flgGiro = '0'
006500010601     C*
006600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006700010601     C                   eval      flgGiro = '1'
006800000724     C*
006900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007100010601     C     invfil        chain     azorg01l
007200000724     C                   if        %found
007300000724     C                   movel(p)  CMD(1)        depcmd
007400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007500000724     C*
007600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007700011113     C                   Z-ADD     150           LENGH            15 5
007800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007900000724     C                   PARM                    depcmd
008000000724     C                   PARM                    LENGH
008100000724     C*
008200000724     C                   endif
008300000616     C*
008400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008500000616     C                   movel(p)  CMD(2)        depcmd
008600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008700000616     C*
008800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008900011113     C                   Z-ADD     150           LENGH            15 5
009000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000616     C                   PARM                    depcmd
009200000616     C                   PARM                    LENGH
009300000613     C*
009400050121     C                   if        not %open(titvh1ps)
009500050121     C                   open      titvh1ps
009600000616     C                   except    testdett
009700000613     C                   endif
009800010601     C*
009900010601     C                   endif
010000010601     C*
010100010601     C                   ENDSR
010200010601     C***
010300010601
010400010601
010500010601
010600010601     C*--------------------------------------------------------
010700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010800010601     C*--------------------------------------------------------
010900010601     C     ENDELA        BEGSR
011000990915     C*
011100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011200000613     C*
011300050121     C                   if        %open(titvh1ps)
011400000616     C                   except    findett
011500050121     C                   close     titvh1ps
011600000613     C                   endif
011700000616     C*
011800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    CMD(3)
012200000616     C                   PARM                    LENGH
012300000616     C*
012400010601     C                   ENDSR
012500010601     C***
012600010601
012700010601
012800000613
012900000613     C*--------------------------------------------------------
013000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013100000613     C*--------------------------------------------------------
013200000621     C     STPR          BEGSR
013300000613     C*
013400050121     C                   if        not %open(titvh1p)
013500050121     C                   open      titvh1p
013600990915     C                   endif
013700990915     C*
013800990915     C                   except    riepilogo
013900990915     C*
014000050121     C                   if        %open(titvh1p)
014100050121     C                   close     titvh1p
014200990914     C                   endif
014300000613     C*
014400000613     C                   ENDSR
014500000613     C***
014600000613
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015100010330     C*--------------------------------------------------------
015200010605     C     RWVAB         BEGSR
015300010330     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700021014     C                   if        not %open(fivabwwr)
015800021014     C                   open      fivabwwr
015900010330     C                   endif
016000010330     C*
016100010604     C                   clear                   §CTROK
016200010604     C                   clear                   §CTRMO
016300010604     C                   clear                   §CTRNO
016400010330     C*
016500010330     C                   DO        *HIVAL
016600010330     C*
016700010330     C                   READ      tivin00r                               70
016800010618     C*
016900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017000010618     C                   if        vinflg = '1'
017100010618     C                   eval      flgOk = '1'
017200010618     C                   endif
017300010618     C*
017400010330     C                   if        vindta > *blanks
017500010330     C                   add       1             rrnum
017600010330     C*
017700010601     C                   if        *in70 = *off and
017800010330     C                             (vinflg = *blanks
017900010330     C                              or vinflg = '0'
018000010330     C                              or vinflg = '2')
018100010330     C*
018200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018300010711     C                   if        vinflg = *blanks or vinflg = '0'
018400010711     C                   clear                   vinmsg
018500010711     C                   endif
018600010601     C*
018700010330     C                   exsr      inzvar
018800010330     C                   exsr      defcam
018900010605     C                   exsr      impvab
019000010601     C*
019100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019200010605     C                   eval      depfil = VABLNP
019300010601     C                   exsr      repfil
019400010601     C                   if        depfil = invfil
019500021025     C                   if        vlrpoi = 999
019600021025     C                   MOVE(P)   invfil        VABFGS
019700021025     C                   else
019800021025     C                   MOVE(P)   vlrpoi        VABFGS
019900021025     C                   endif
020000050114     C                   endif
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
022300010604     C*
022400010330     C                   else
022500010330     C                   eval      vinflg = '1'
022600010330     C                   endif
022700010601     C*
022800010601     C  N70              update    tivin000
022900010330     C*
023000010330     C  N70              ENDdo
023100010601     C
023200010601     C*
023300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023400010601     C                   if        cntNonEl = *zeros or
023500010601     C                             flgMulti = '0'
023600010330     C* Se non ci sono record con errori ...
023700010601     C                   if        §ctrno = 0 and
023800010604     C                             §ctrmo = 0 and
023900010601     C                             flgStato <> '2'
024000010330     C* ... restituisco esito OK.
024100010330     C                   eval      wrkesito = '0'
024200010330     C                   else
024300010330     C                   if        §ctrok > 0
024400010330     C                   eval      wrkesito = '1'
024500010330     C                   else
024600010615     C                   if        flgOk = '0'
024700010615     C                   eval      wrkesito = '2'
024800010615     C                   else
024900010615     C                   eval      wrkesito = '6'
025000010615     C                   endif
025100010330     C                   endif
025200010330     C                   endif
025300010601     C                   else
025400010601     C                   eval      wrkesito = '9'
025500010601     C                   endif
025600010330     C*
025700010330     C                   if        %open(tivin00r)
025800010330     C                   close     tivin00r
025900010330     C                   endif
026000021014     C                   if        %open(fivabwwr)
026100021014     C                   close     fivabwwr
026200010330     C                   endif
026300010601     C*
026400010601     C                   if        vlrpoi <> 999
026500010601     C                   eval      invfil = vlrpoi
026600010601     C                   endif
026700010330     C*
026800010330     C                   if        §ctrok > 0
026900010601     C                             and invfil > *zeros
027000010330     C                   exsr      invio
027100010330     C                   endif
027200010601     C*
027300010618     C                   if        flgGiro = '1'
027400010601     C                   exsr      endela
027500010618     C                   endif
027600010330     C*
027700010330     C                   ENDSR
027800010330     C***
027900010330
028000010601
028100010601
028200010601
028300010601
028400010330     C*----------------------------------------------------*
028500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028600010330     C*----------------------------------------------------*
028700010330     C     INZVAR        BEGSR
028800010330     C*
028900020204     C                   Z-ADD     *zeros        Num5_0            5 0
029000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029100020322     C                   MOVEL     '0'           FlgCAS            1
029200010330     C*
029300010330     C                   ENDSR
029400010330     C*----------------------------------------------------*
029500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029600010330     C*----------------------------------------------------*
029700010330     C     DEFCAM        BEGSR
029800010330     C*
029900020204     C* Inizializzo il buffer del record da scrivere
030000021014     C                   CLEAR                   FIVAB000
030100020204     C* Imposto i valori di default...
030200050121     C                   EVAL      VABCCM = 0980747
030300050121     C                   EVAL      VABLNP = 098
030400040713     C                   EVAL      VABCTR = 000
030500041108     C                   EVAL      VABCBO = '1'
030600020204     C* ... e poi verifico se sono stati passati come parametri
030700020204     C                   IF        vlrppt > *blanks
030800040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
030900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031000020204     C                   EXSR      CHKNUM
031100020204     C                   IF        PiInt=*on
031200020204     C                   Z-ADD     PiVal         VABCCM
031300020204     C                   ENDIF
031400040301     C                   ENDIF
031500040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031700020204     C                   EXSR      CHKNUM
031800020204     C                   IF        PiInt=*on
031900020204     C                   Z-ADD     PiVal         VABLNP
032000020204     C                   ENDIF
032100040301     C                   ENDIF
032200040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032400020204     C                   EXSR      CHKNUM
032500020204     C                   IF        PiInt=*on
032600020204     C                   Z-ADD     PiVal         VABCTR
032700040301     C                   ENDIF
032800020204     C                   ENDIF
032900071002     C                   IF        %trim(%subst(vlrppt:14:2)) <> *blanks
033000071002     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
033100071002     C                   ENDIF
033200020204     C                   ENDIF
033300020204     C*
033400010330     C                   ENDSR
033500010607     C*----------------------------------------------------*
033600021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033700010607     C*----------------------------------------------------*
033800010607     C     IMPVAB        BEGSR
033900010607     C*
034000010607     C                   SETOFF                                       3132
034100010607     C*
034200010607     C* Reperimento campi ALFA
034300010607     C*
034400010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034500050121     C                   EVAL      VABRSD=%trim(%subst(vindta:2+6:40-6))
034600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034800020117     C* ==
034900050121     C                   EVAL      VABIND=%trim(%subst(vindta:45:40))
035000050121     C                   EVAL      VABLOD=%trim(%subst(vindta:88:20))
035100050121     C                   EVAL      VABPRD=%trim(%subst(vindta:111:2))
035200050121     C                   EVAL      VABRMA=%trim(%subst(vindta:155:10))
035300010607     C*
035400010607     C* Reperimento campi NUMERICI
035500020722     C                   MOVEL     DATCOR        VABAAS
035600020722     C                   MOVE      DATCOR        VABMGS
035700040420     C* NSP/RMN
035800050121     C                   EVAL      PiStr=%trim(%subst(vindta:155+1:10-1))
035900020304     C                   EXSR      CHKNUM
036000020304     C                   IF        PiInt=*on
036100040301     C                   Z-ADD     PiVal         VABNSP
036200040301     C                   Z-ADD     PiVal         VABRMN
036300020304     C                   ELSE
036400020722     C                   SETON                                        31
036500020304     C                   Z-ADD     *zeros        VABNSP
036600040301     C                   Z-ADD     *zeros        VABRMN
036700020304     C                   EVAL      vinmsg = %trimr(vinmsg)
036800040301     C                             + ' ' + 'VABNSP VABRMN'
036900020304     C                   ENDIF
037000040420     C* CAD
037100050121     C                   EVAL      PiStr=%trim(%subst(vindta:116:5))
037200010607     C                   EXSR      CHKNUM
037300010607     C                   IF        PiInt=*on
037400020204     C                   Z-ADD     PiVal         Num5_0
037500020204     C                   MOVEL(p)  Num5_0        VABCAD
037600010607     C                   ELSE
037700010607     C                   SETON                                        32
037800020204     C                   EVAL      VABCAD = *zeros
037900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038000020204     C                             + ' ' + 'VABCAD'
038100010607     C                   ENDIF
038200040420     C* NCL
038300050121     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
038400010607     C                   EXSR      CHKNUM
038500010607     C                   IF        PiInt=*on
038600010607     C                   Z-ADD     PiVal         VABNCL
038700010607     C                   ELSE
038800010607     C                   SETON                                        32
038900010607     C                   Z-ADD     *zeros        VABNCL
039000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039100010607     C                             + ' ' + 'VABNCL'
039200010607     C                   ENDIF
039300040420     C* PKB
039400050121     C                   EVAL      PiStr=%trim(%subst(vindta:168:5))
039500010607     C                   EXSR      CHKNUM
039600010607     C                   IF        PiNum=*on
039700050114     C                   Z-ADD     PiVal         VABPKB
039800010607     C                   ELSE
039900010607     C                   SETON                                        32
040000010607     C                   Z-ADD     *zeros        VABPKB
040100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040200010607     C                             + ' ' + 'VABPKB'
040300010607     C                   ENDIF
040400020322     C*
040500020322     C* Considerazioni finali su CBO/CAS
040600020322     C                   IF        FlgCAS = '1'
040700040713     C                   IF        VABCBO = '1'
040800020322     C                   EVAL      VABCBO = '4'
040900040713     C                   ENDIF
041000040713     C                   IF        VABCBO = '2'
041100040713     C                   EVAL      VABCBO = '6'
041200040713     C                   ENDIF
041300020322     C                   ENDIF
041400020204     C*
041500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041600020204     C                   EXSR      CHKIMPDIV
041700020204     C*
041800010607     C                   ENDSR
041900010607     C*----------------------------------------------------*
042000010601
042100020204
042200020204     C*----------------------------------------------------*
042300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
042400020204     C*----------------------------------------------------*
042500020204     C     CHKIMPDIV     BEGSR
042600020204     C*
042700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
042800020204     C                   Z-ADD     *zeros        wrkDec            9 9
042900020204     C*
043000020204     C* Come prima cosa effettuo considerazioni sulla divisa
043100020204     C                   IF        vabIAS > *zeros
043200020204     C                   IF        vabVAS <> 'EUR'
043300020204     C                   EVAL      vabVAS =  'ITL'
043400020204     C                   ENDIF
043500020204     C                   ENDIF
043600020204     C*
043700020204     C                   IF        vabCAS > *zeros
043800020204     C                   IF        vabVCA <> 'EUR'
043900020204     C                   EVAL      vabVCA =  'ITL'
044000020204     C                   ENDIF
044100020204     C                   ENDIF
044200020204     C*
044300020204     C                   IF        vabVMD > *zeros
044400020321     C                   IF        vabVAD <> 'EUR'
044500020204     C                   EVAL      vabVAD =  'ITL'
044600020204     C                   ENDIF
044700020204     C                   ENDIF
044800020204     C*
044900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045000020204     C                   Z-ADD     vabIAS        wrkDec
045100020204     C                   IF        wrkDec > *zeros
045200020204     C                   IF        vabVAS = 'ITL'
045300020204     C                   EVAL      vabIAS = *zeros
045400020204     C                   ENDIF
045500020204     C                   ENDIF
045600020204     C*
045700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
045800020204     C                   Z-ADD     vabCAS        wrkDec
045900020204     C                   IF        wrkDec > *zeros
046000020204     C                   IF        vabVCA = 'ITL'
046100020204     C                   EVAL      vabCAS = *zeros
046200020204     C                   ENDIF
046300020204     C                   ENDIF
046400020204     C*
046500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
046600020204     C                   Z-ADD     vabVMD        wrkDec
046700020204     C                   IF        wrkDec > *zeros
046800020204     C                   IF        vabVAD = 'ITL'
046900020204     C                   EVAL      vabVMD = *zeros
047000020204     C                   ENDIF
047100020204     C                   ENDIF
047200020204     C*
047300020204     C                   ENDSR
047400020204     C***
047500020204
047600010330
047700010330
047800010330     C*----------------------------------------------------*
047900010330     C*  CONTROLLO NUMERICITA' CAMPI
048000010330     C*----------------------------------------------------*
048100010330     C     CHKNUM        BEGSR
048200010330     C*
048300010606     C                   IF        PiDecChr = *blanks
048400010606     C                   EVAL      PiDecChr = ','
048500010606     C                   ENDIF
048600010606     C*
048700010606     C                   CALL(e)   'ISNUMERIC'
048800010330     C                   PARM                    PiStr            30
048900010606     C                   PARM                    PiDecChr          1
049000010330     C                   PARM      *ZEROS        PiVal            30 9
049100010330     C                   PARM      '0'           PiInt             1
049200010330     C                   PARM      '0'           PiNum             1
049300010330     C                   IF        %error
049400010606     C                   EVAL      PiNum=*off
049500010330     C                   ENDIF
049600010330     C*
049700010330     C                   ENDSR
049800010330     C***
049900010330
050000010601
050100010601
050200010601
050300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
050400010601     C     repfil        BEGSR
050500010601     C*
050600010601     C                   if        invfil = *zeros and
050700010601     C                             depfil > *zeros and
050800010629     C                             (vinflg = *blanks or
050900010629     C                              vinflg = *zeros)
051000010601     C
051100010601     C                   eval      invfil = depfil
051200010601     C                   endif
051300010601     C*
051400010601     C                   if        depfil <> invfil and
051500010601     C                             invfil > *zeros
051600010601     C                   eval      flgMulti = '1'
051700010601     C                   if        vinflg = *blanks
051800010601     C                   add       1             cntNonEl
051900010601     C                   endif
052000010601     C                   endif
052100010601     C*
052200010601     C                   if        vinflg = '2'
052300010601     C                   eval      flgStato = '2'
052400010601     C                   endif
052500010601     C*
052600010601     C                   ENDSR
052700010601     C***
052800010601
052900010601
053000010601
053100010330
053200010330
053300010330
053400990920      /TITLE Invio dei dati al punto operativo.
053500000613     C     invio         BEGSR
053600990920     C*
053700990920     C                   reset                   dscmz
053800010601     C                   move      invfil        cmzdst
053900990920     C                   eval      cmzfld = vlrfou
054000990920     C                   eval      cmzmbd = vlrhdl
054100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
054200000710     C                   if        prmfir = *blanks
054300021014     C                   eval      cmzfla = 'FIVAB00F'
054400021014     C                   eval      cmzmba = 'FIVAB00F'
054500000710     C                   else
054600000710     C                   eval      cmzfla = prmfir
054700000710     C                   eval      cmzmba = prmfir
054800000710     C                   endif
054900990920     C                   eval      cmznrr = *zeros
055000990920     C                   move      §ctrok        cmznrr
055100021018     C                   eval      cmzlba = vlrfl1
055200990920     C                   call(e)   'TIS711C'
055300990920     C                   parm                    dscmz
055400990921     C                   parm      *blanks       esito
055500990920     C                   if        %error
055600990920     C                             or cmzerr = '1'
055700990921     C                             or esito  = '1'
055800000710     C                   eval      wrkesito = '3'
055900990920     C                   endif
056000990920     C*
056100000613     C                   ENDSR
056200990910
056300010601
056400010601
056500010601
056600010601
056700010601      /TITLE Invio dei dati al punto operativo.
056800010601     C     opeini        BEGSR
056900010601     C*
057000010601     C* Inizializzo flag e contatori operativi
057100010601     C                   movel     '0'           flgGiro           1
057200010601     C                   movel     '0'           flgMulti          1
057300010601     C                   movel     '1'           flgStato          1
057400010615     C                   movel     '0'           flgOk             1
057500010601     C                   z-add     *zeros        cntNonEl         10 0
057600010601     C                   z-add     *zeros        depfil            3 0
057700010601     C                   z-add     *zeros        invfil            3 0
057800010601     C*
057900010601     C                   ENDSR
058000010601     C***
058100010601
058200010601
058300010601
058400010330
058500010330
058600000613     C     *inzsr        BEGSR
058700990910     C*
058800990910     C     *entry        plist
058900990920     C                   parm                    tivlrds
059000990921     C                   parm      wrkesito      esito
059100000724     C                   parm                    prmlit
059200000710     C                   parm                    prmfir
059300010330     C*
059400010330     C* CALCOLA LA DATA CORRENTE
059500010330     C                   time                    wn14             14 0
059600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059700010330     C                   z-add     wn8           g08dat
059800010330     C                   z-add     *zeros        g08inv
059900010330     C                   movel     '0'           g08err
060000010330     C                   call      'XSRDA8'
060100010330     C                   parm                    wlbda8
060200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060300000613     C*
060400000613     C                   ENDSR
060500000613     C***
060600990908
060700050121     Otitvh1p   E            riepilogo         2
060800990915     O                                              'Upload via Internet'
060900990915     O                                           +1 'Traduzione TIVIN00R -'
061000021014     O                                           +1 'FIVABWWR'
061100990915     O                                           +1 'Testate bolle'
061200990915     O          E            riepilogo   2
061300990915     O                       wrkdata
061400990915     O                       wrkora              +1
061500990915     O                       procname            +1
061600990915     O          E            riepilogo   2
061700990915     O                                              'Cliente..................:'
061800010605     O                       VABCCM        z     +1
061900990915     O          E            riepilogo   2
062000990920     O                                              'Riferimento Strategi.....:'
062100990920     O                       vlrhdl              +1
062200990915     O          E            riepilogo   2
062300990915     O                                              'Giusti...................:'
062400971022     O                       §CTROK        2   +  1
062500990915     O          E            riepilogo   2
062600990915     O                                              'Sbagliati e corretti.....:'
062700971022     O                       §CTRMO        2   +  1
062800990915     O          E            riepilogo   2
062900990915     O                                              'Sbagliati e scartati.....:'
063000971022     O                       §CTRNO        2   +  1
063100000613
063200050121     Otitvh1ps  E            testdett          2
063300000613     O                                              'Upload via Internet'
063400000613     O                                           +1 'Traduzione TIVIN00R -'
063500021014     O                                           +1 'FIVABWWR'
063600000613     O                                           +1 'Testate bolle'
063700000616     O          E            testdett    3
063800000613     O                                           +2 'N° rec'
063900000613     O                                           +3 'Anteprima contenuto'
064000000616     O          E            rigadett    2
064100000613     O                       rrnum               +2
064200000621     O                       recko               +3
064300000616     O          E            findett     2
064400000613     O                       wrkdata
064500000613     O                       wrkora              +1
064600000613     O                       procname            +1
064700000616     O          E            findett     2
064800000613     O                                              'Cliente..................:'
064900010605     O                       VABCCM        z     +1
065000000616     O          E            findett     2
065100000613     O                                              'Riferimento Strategi.....:'
065200000613     O                       vlrhdl              +1
065300000616     O          E            findett     2
065400000613     O                                              'Giusti...................:'
065500000613     O                       §CTROK        2   +  1
065600000616     O          E            findett     2
065700000613     O                                              'Sbagliati e corretti.....:'
065800000613     O                       §CTRMO        2   +  1
065900000616     O          E            findett     2
066000000613     O                                              'Sbagliati e scartati.....:'
066100000613     O                       §CTRNO        2   +  1
066200000613** CMD - COMANDI CL
066300050121OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVH1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
066400050121OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVH1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
066500050121DLTOVR FILE(TITVH1P TITVH1PS) LVL(*)
066600010606
066700000613
