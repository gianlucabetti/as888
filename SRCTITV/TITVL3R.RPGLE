000100050321      /TITLE Upload via Internet: traduzione in FIVABWWR. (x KRINO - Cod. 0830465)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700050321     Ftitvl3p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050321     Ftitvl3ps  O    f  198        PRINTER usropn
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
009400050321     C                   if        not %open(titvl3ps)
009500050321     C                   open      titvl3ps
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
011300050321     C                   if        %open(titvl3ps)
011400000616     C                   except    findett
011500050321     C                   close     titvl3ps
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
013400050321     C                   if        not %open(titvl3p)
013500050321     C                   open      titvl3p
013600990915     C                   endif
013700990915     C*
013800990915     C                   except    riepilogo
013900990915     C*
014000050321     C                   if        %open(titvl3p)
014100050321     C                   close     titvl3p
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
016100010604     C                   clear                   低TROK
016200010604     C                   clear                   低TRMO
016300010604     C                   clear                   低TRNO
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
018100050217     C*
018200050217     C                   clear                   FIVAB000
018300010330     C*
018400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018500010711     C                   if        vinflg = *blanks or vinflg = '0'
018600010711     C                   clear                   vinmsg
018700010711     C                   endif
018800010601     C*
018900010330     C                   exsr      inzvar
019000010330     C                   exsr      defcam
019100010605     C                   exsr      impvab
019200010601     C*
019300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019400010605     C                   eval      depfil = VABLNP
019500010601     C                   exsr      repfil
019600010601     C                   if        depfil = invfil
019700021025     C                   if        vlrpoi = 999
019800021025     C                   MOVE(P)   invfil        VABFGS
019900021025     C                   else
020000021025     C                   MOVE(P)   vlrpoi        VABFGS
020100021025     C                   endif
020200050114     C                   endif
020300010601     C*
020400010601     C                   exsr      PREELA
020500010601     C*
020600010604     C* Ebbene...
020700010604     C*
020800010604     C  N31              ADD       1             低TROK            7 0
020900010604     C   32              ADD       1             低TRMO            7 0
021000010604     C   31              ADD       1             低TRNO            7 0
021100020205     C*
021200040301     C* Scarico il buffer del FIVAB
021300021014     C  N31              WRITE     FIVAB000
021400010604     C*
021500020722     C                   endif
021600020722     C*
021700010604     C                   if        *in31 = *off and
021800010604     C                             *in32 = *off
021900010604     C                   eval      vinflg = '1'
022000010604     C                   else
022100010604     C                   eval      recko = vindta
022200010604     C                   except    rigadett
022300010604     C                   eval      vinflg = '2'
022400010604     C                   endif
022500010604     C*
022600010330     C                   else
022700010330     C                   eval      vinflg = '1'
022800010330     C                   endif
022900010601     C*
023000010601     C  N70              update    tivin000
023100010330     C*
023200010330     C  N70              ENDdo
023300010601     C
023400010601     C*
023500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023600010601     C                   if        cntNonEl = *zeros or
023700010601     C                             flgMulti = '0'
023800010330     C* Se non ci sono record con errori ...
023900010601     C                   if        呃trno = 0 and
024000010604     C                             呃trmo = 0 and
024100010601     C                             flgStato <> '2'
024200010330     C* ... restituisco esito OK.
024300010330     C                   eval      wrkesito = '0'
024400010330     C                   else
024500010330     C                   if        呃trok > 0
024600010330     C                   eval      wrkesito = '1'
024700010330     C                   else
024800010615     C                   if        flgOk = '0'
024900010615     C                   eval      wrkesito = '2'
025000010615     C                   else
025100010615     C                   eval      wrkesito = '6'
025200010615     C                   endif
025300010330     C                   endif
025400010330     C                   endif
025500010601     C                   else
025600010601     C                   eval      wrkesito = '9'
025700010601     C                   endif
025800010330     C*
025900010330     C                   if        %open(tivin00r)
026000010330     C                   close     tivin00r
026100010330     C                   endif
026200021014     C                   if        %open(fivabwwr)
026300021014     C                   close     fivabwwr
026400010330     C                   endif
026500010601     C*
026600010601     C                   if        vlrpoi <> 999
026700010601     C                   eval      invfil = vlrpoi
026800010601     C                   endif
026900010330     C*
027000010330     C                   if        呃trok > 0
027100010601     C                             and invfil > *zeros
027200010330     C                   exsr      invio
027300010330     C                   endif
027400010601     C*
027500010618     C                   if        flgGiro = '1'
027600010601     C                   exsr      endela
027700010618     C                   endif
027800010330     C*
027900010330     C                   ENDSR
028000010330     C***
028100010330
028200010601
028300010601
028400010601
028500010601
028600010330     C*----------------------------------------------------*
028700020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028800010330     C*----------------------------------------------------*
028900010330     C     INZVAR        BEGSR
029000010330     C*
029100020204     C                   Z-ADD     *zeros        Num5_0            5 0
029200020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029300020322     C                   MOVEL     '0'           FlgCAS            1
029400010330     C*
029500010330     C                   ENDSR
029600010330     C*----------------------------------------------------*
029700020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029800010330     C*----------------------------------------------------*
029900010330     C     DEFCAM        BEGSR
030000010330     C*
030100020204     C* Inizializzo il buffer del record da scrivere
030200021014     C                   CLEAR                   FIVAB000
030300020204     C* Imposto i valori di default...
030400050321     C                   EVAL      VABCCM = 0830465
030500050321     C                   EVAL      VABLNP = 083
030600050223     C                   EVAL      VABCTR = 000
030700041108     C                   EVAL      VABCBO = '1'
030800050218     C                   EVAL      VABTSP = 'C'
030900020204     C* ... e poi verifico se sono stati passati come parametri
031000020204     C                   IF        vlrppt > *blanks
031100040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031300020204     C                   EXSR      CHKNUM
031400020204     C                   IF        PiInt=*on
031500020204     C                   Z-ADD     PiVal         VABCCM
031600020204     C                   ENDIF
031700040301     C                   ENDIF
031800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032000020204     C                   EXSR      CHKNUM
032100020204     C                   IF        PiInt=*on
032200020204     C                   Z-ADD     PiVal         VABLNP
032300020204     C                   ENDIF
032400040301     C                   ENDIF
032500040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032700020204     C                   EXSR      CHKNUM
032800020204     C                   IF        PiInt=*on
032900020204     C                   Z-ADD     PiVal         VABCTR
033000040301     C                   ENDIF
033100020204     C                   ENDIF
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
034500050321     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
034600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'� diventa A ==
034700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034800020117     C* ==
034900050321     C                   EVAL      VABRD2=%trim(%subst(vindta:92:15))
035000050321     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
035100050321     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
035200050321     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
035300050321     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
035400050321     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
035500050321     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
035600050321     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
035700050321     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
035800010607     C*
035900010607     C* Reperimento campi NUMERICI
036000020722     C                   MOVEL     DATCOR        VABAAS
036100020722     C                   MOVE      DATCOR        VABMGS
036200040420     C* NSP/RMN
036300050321     C                   EVAL      PiStr=%trim(%subst(vindta:189:5))
036400020304     C                   EXSR      CHKNUM
036500020304     C                   IF        PiInt=*on
036600040301     C                   Z-ADD     PiVal         VABNSP
036700040301     C                   Z-ADD     PiVal         VABRMN
036800020304     C                   ELSE
036900050217     C                   SETON                                        31
037000050217     C                   Z-ADD     *zeros        VABNSP
037100050217     C                   Z-ADD     *zeros        VABRMN
037200020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037300040301     C                             + ' ' + 'VABNSP VABRMN'
037400020304     C                   ENDIF
037500050128     C* CAD
037600050321     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
037700050128     C                   EXSR      CHKNUM
037800050128     C                   IF        PiInt=*on
037900050128     C                   Z-ADD     PiVal         Num5_0
038000050128     C                   MOVEL(p)  Num5_0        VABCAD
038100050128     C                   ELSE
038200050128     C                   SETON                                        32
038300050128     C                   EVAL      VABCAD = *zeros
038400050128     C                   EVAL      vinmsg = %trimr(vinmsg)
038500050128     C                             + ' ' + 'VABCAD'
038600050128     C                   ENDIF
038700040420     C* NCL
038800050321     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
038900010607     C                   EXSR      CHKNUM
039000010607     C                   IF        PiInt=*on
039100010607     C                   Z-ADD     PiVal         VABNCL
039200010607     C                   ELSE
039300010607     C                   SETON                                        32
039400010607     C                   Z-ADD     *zeros        VABNCL
039500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039600010607     C                             + ' ' + 'VABNCL'
039700010607     C                   ENDIF
039800040420     C* PKB
039900050321     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
040000010607     C                   EXSR      CHKNUM
040100010607     C                   IF        PiNum=*on
040200050114     C                   Z-ADD     PiVal         VABPKB
040300010607     C                   ELSE
040400010607     C                   SETON                                        32
040500010607     C                   Z-ADD     *zeros        VABPKB
040600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040700010607     C                             + ' ' + 'VABPKB'
040800010607     C                   ENDIF
040900050321     C* CAS
041000050321     C                   IF        %subst(vindta:280:12)<>'000000000,00'
041100050321     C                   EVAL      FlgCAS = '1'
041200050321     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
041300050321     C                   EXSR      CHKNUM
041400050321     C                   IF        PiNum=*on
041500060713     C                   Z-ADD     PiVal         VABCAS
041600050321     C                   ELSE
041700050321     C                   SETON                                        32
041800060713     C                   Z-ADD     *zeros        VABCAS
041900050321     C                   EVAL      vinmsg = %trimr(vinmsg)
042000060713     C                             + ' ' + 'VABCAS'
042100050321     C                   ENDIF
042200050321     C                   ENDIF
042300020322     C*
042400020322     C* Considerazioni finali su CBO/CAS
042500020322     C                   IF        FlgCAS = '1'
042600040713     C                   IF        VABCBO = '1'
042700020322     C                   EVAL      VABCBO = '4'
042800040713     C                   ENDIF
042900040713     C                   IF        VABCBO = '2'
043000040713     C                   EVAL      VABCBO = '6'
043100040713     C                   ENDIF
043200020322     C                   ENDIF
043300020204     C*
043400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043500020204     C                   EXSR      CHKIMPDIV
043600020204     C*
043700010607     C                   ENDSR
043800010607     C*----------------------------------------------------*
043900010601
044000020204
044100020204     C*----------------------------------------------------*
044200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044300020204     C*----------------------------------------------------*
044400020204     C     CHKIMPDIV     BEGSR
044500020204     C*
044600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilit�
044700020204     C                   Z-ADD     *zeros        wrkDec            9 9
044800020204     C*
044900020204     C* Come prima cosa effettuo considerazioni sulla divisa
045000020204     C                   IF        vabIAS > *zeros
045100020204     C                   IF        vabVAS <> 'EUR'
045200020204     C                   EVAL      vabVAS =  'ITL'
045300020204     C                   ENDIF
045400020204     C                   ENDIF
045500020204     C*
045600020204     C                   IF        vabCAS > *zeros
045700020204     C                   IF        vabVCA <> 'EUR'
045800020204     C                   EVAL      vabVCA =  'ITL'
045900020204     C                   ENDIF
046000020204     C                   ENDIF
046100020204     C*
046200020204     C                   IF        vabVMD > *zeros
046300020321     C                   IF        vabVAD <> 'EUR'
046400020204     C                   EVAL      vabVAD =  'ITL'
046500020204     C                   ENDIF
046600020204     C                   ENDIF
046700020204     C*
046800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046900020204     C                   Z-ADD     vabIAS        wrkDec
047000020204     C                   IF        wrkDec > *zeros
047100020204     C                   IF        vabVAS = 'ITL'
047200020204     C                   EVAL      vabIAS = *zeros
047300020204     C                   ENDIF
047400020204     C                   ENDIF
047500020204     C*
047600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047700020204     C                   Z-ADD     vabCAS        wrkDec
047800020204     C                   IF        wrkDec > *zeros
047900020204     C                   IF        vabVCA = 'ITL'
048000020204     C                   EVAL      vabCAS = *zeros
048100020204     C                   ENDIF
048200020204     C                   ENDIF
048300020204     C*
048400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048500020204     C                   Z-ADD     vabVMD        wrkDec
048600020204     C                   IF        wrkDec > *zeros
048700020204     C                   IF        vabVAD = 'ITL'
048800020204     C                   EVAL      vabVMD = *zeros
048900020204     C                   ENDIF
049000020204     C                   ENDIF
049100020204     C*
049200020204     C                   ENDSR
049300020204     C***
049400020204
049500010330
049600010330
049700010330     C*----------------------------------------------------*
049800010330     C*  CONTROLLO NUMERICITA' CAMPI
049900010330     C*----------------------------------------------------*
050000010330     C     CHKNUM        BEGSR
050100010330     C*
050200010606     C                   IF        PiDecChr = *blanks
050300050321     C                   EVAL      PiDecChr = ','
050400010606     C                   ENDIF
050500010606     C*
050600010606     C                   CALL(e)   'ISNUMERIC'
050700010330     C                   PARM                    PiStr            30
050800010606     C                   PARM                    PiDecChr          1
050900010330     C                   PARM      *ZEROS        PiVal            30 9
051000010330     C                   PARM      '0'           PiInt             1
051100010330     C                   PARM      '0'           PiNum             1
051200010330     C                   IF        %error
051300010606     C                   EVAL      PiNum=*off
051400010330     C                   ENDIF
051500010330     C*
051600010330     C                   ENDSR
051700010330     C***
051800010330
051900010601
052000010601
052100010601
052200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052300010601     C     repfil        BEGSR
052400010601     C*
052500010601     C                   if        invfil = *zeros and
052600010601     C                             depfil > *zeros and
052700010629     C                             (vinflg = *blanks or
052800010629     C                              vinflg = *zeros)
052900010601     C
053000010601     C                   eval      invfil = depfil
053100010601     C                   endif
053200010601     C*
053300010601     C                   if        depfil <> invfil and
053400010601     C                             invfil > *zeros
053500010601     C                   eval      flgMulti = '1'
053600010601     C                   if        vinflg = *blanks
053700010601     C                   add       1             cntNonEl
053800010601     C                   endif
053900010601     C                   endif
054000010601     C*
054100010601     C                   if        vinflg = '2'
054200010601     C                   eval      flgStato = '2'
054300010601     C                   endif
054400010601     C*
054500010601     C                   ENDSR
054600010601     C***
054700050217
054800050217
054900050217
055000050217
055100050217      /TITLE Invio dei dati al punto operativo.
055200050217     C     invio         BEGSR
055300050217     C*
055400050217     C                   reset                   dscmz
055500050217     C                   move      vlrpoi        cmzdst
055600050217     C                   eval      cmzfld = vlrfou
055700050217     C                   eval      cmzmbd = vlrhdl
055800050217     C                   eval      %subst(cmzmbd:1:1) = 'M'
055900050217     C***                if        prmfir = *blanks
056000050217     C                   eval      cmzfla = 'FIVAB00F'
056100050217     C                   eval      cmzmba = 'FIVAB00F'
056200050217     C***                else
056300050217     C***                eval      cmzfla = prmfir
056400050217     C***                eval      cmzmba = prmfir
056500050217     C***                endif
056600050217     C                   eval      cmznrr = *zeros
056700050217     C                   move      呃trok        cmznrr
056800050217     C                   eval      cmzlba = vlrfl1
056900050217     C                   call(e)   'TIS711C'
057000050217     C                   parm                    dscmz
057100050217     C                   parm      *blanks       esito
057200050217     C                   if        %error
057300050217     C                             or cmzerr = '1'
057400050217     C                             or esito  = '1'
057500050217     C                   eval      wrkesito = '3'
057600050217     C                   endif
057700050217     C*
057800050217     C                   ENDSR
057900050217     C***
058000050217
058100050217
058200050217
058300010601
058400010601
058500010601      /TITLE Invio dei dati al punto operativo.
058600010601     C     opeini        BEGSR
058700010601     C*
058800010601     C* Inizializzo flag e contatori operativi
058900010601     C                   movel     '0'           flgGiro           1
059000010601     C                   movel     '0'           flgMulti          1
059100010601     C                   movel     '1'           flgStato          1
059200010615     C                   movel     '0'           flgOk             1
059300010601     C                   z-add     *zeros        cntNonEl         10 0
059400010601     C                   z-add     *zeros        depfil            3 0
059500010601     C                   z-add     *zeros        invfil            3 0
059600010601     C*
059700010601     C                   ENDSR
059800010601     C***
059900010601
060000010601
060100010601
060200010330
060300010330
060400000613     C     *inzsr        BEGSR
060500990910     C*
060600990910     C     *entry        plist
060700990920     C                   parm                    tivlrds
060800990921     C                   parm      wrkesito      esito
060900000724     C                   parm                    prmlit
061000000710     C                   parm                    prmfir
061100010330     C*
061200010330     C* CALCOLA LA DATA CORRENTE
061300010330     C                   time                    wn14             14 0
061400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061500010330     C                   z-add     wn8           g08dat
061600010330     C                   z-add     *zeros        g08inv
061700010330     C                   movel     '0'           g08err
061800010330     C                   call      'XSRDA8'
061900010330     C                   parm                    wlbda8
062000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062100000613     C*
062200000613     C                   ENDSR
062300000613     C***
062400990908
062500050321     Otitvl3p   E            riepilogo         2
062600990915     O                                              'Upload via Internet'
062700990915     O                                           +1 'Traduzione TIVIN00R -'
062800021014     O                                           +1 'FIVABWWR'
062900990915     O                                           +1 'Testate bolle'
063000990915     O          E            riepilogo   2
063100990915     O                       wrkdata
063200990915     O                       wrkora              +1
063300990915     O                       procname            +1
063400990915     O          E            riepilogo   2
063500990915     O                                              'Cliente..................:'
063600010605     O                       VABCCM        z     +1
063700990915     O          E            riepilogo   2
063800990920     O                                              'Riferimento Strategi.....:'
063900990920     O                       vlrhdl              +1
064000990915     O          E            riepilogo   2
064100990915     O                                              'Giusti...................:'
064200971022     O                       低TROK        2   +  1
064300990915     O          E            riepilogo   2
064400990915     O                                              'Sbagliati e corretti.....:'
064500971022     O                       低TRMO        2   +  1
064600990915     O          E            riepilogo   2
064700990915     O                                              'Sbagliati e scartati.....:'
064800971022     O                       低TRNO        2   +  1
064900000613
065000050321     Otitvl3ps  E            testdett          2
065100000613     O                                              'Upload via Internet'
065200000613     O                                           +1 'Traduzione TIVIN00R -'
065300021014     O                                           +1 'FIVABWWR'
065400000613     O                                           +1 'Testate bolle'
065500000616     O          E            testdett    3
065600000613     O                                           +2 'N� rec'
065700000613     O                                           +3 'Anteprima contenuto'
065800000616     O          E            rigadett    2
065900000613     O                       rrnum               +2
066000000621     O                       recko               +3
066100000616     O          E            findett     2
066200000613     O                       wrkdata
066300000613     O                       wrkora              +1
066400000613     O                       procname            +1
066500000616     O          E            findett     2
066600000613     O                                              'Cliente..................:'
066700010605     O                       VABCCM        z     +1
066800000616     O          E            findett     2
066900000613     O                                              'Riferimento Strategi.....:'
067000000613     O                       vlrhdl              +1
067100000616     O          E            findett     2
067200000613     O                                              'Giusti...................:'
067300000613     O                       低TROK        2   +  1
067400000616     O          E            findett     2
067500000613     O                                              'Sbagliati e corretti.....:'
067600000613     O                       低TRMO        2   +  1
067700000616     O          E            findett     2
067800000613     O                                              'Sbagliati e scartati.....:'
067900000613     O                       低TRNO        2   +  1
068000000613** CMD - COMANDI CL
068100050321OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068200050321OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068300050321DLTOVR FILE(TITVL3P TITVL3PS) LVL(*)
068400010606
068500000613
