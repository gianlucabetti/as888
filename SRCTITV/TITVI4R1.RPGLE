000100060201      /TITLE Upload via Internet: traduzione in FIVABWWR. (x LVMH - KENZO Cod. 0053449)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700050223     Ftitvi4p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050223     Ftitvi4ps  O    f  198        PRINTER usropn
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
002300050223     D tisi95ds      e ds
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
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100010605     C                   exsr      rwvab
005200050223     C*
005300050223     C* Effettuo lancio TISI95 solo x chiusura
005400050223     C                   CLEAR                   TISI95DS
005500050223     C                   EVAL      I95TLA = 'C'
005600050223     C                   CALL      'TISI95R'
005700050223     C                   PARM                    TISI95DS
005800010601     C*
005900010601     C                   seton                                        lr
006000010601
006100010601
006200010601
006300010601
006400010601     C*--------------------------------------------------------
006500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006600010601     C*--------------------------------------------------------
006700010601     C     PREELA        BEGSR
006800010601     C*
006900010601     C* SE OCCORRE SPEDIRE IN FILIALE
007000010601     C                   if        invfil <> *zeros and
007100010601     C                             flgGiro = '0'
007200010601     C*
007300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007400010601     C                   eval      flgGiro = '1'
007500000724     C*
007600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007800010601     C     invfil        chain     azorg01l
007900000724     C                   if        %found
008000000724     C                   movel(p)  CMD(1)        depcmd
008100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008200000724     C*
008300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008400011113     C                   Z-ADD     150           LENGH            15 5
008500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008600000724     C                   PARM                    depcmd
008700000724     C                   PARM                    LENGH
008800000724     C*
008900000724     C                   endif
009000000616     C*
009100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009200000616     C                   movel(p)  CMD(2)        depcmd
009300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009400000616     C*
009500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009600011113     C                   Z-ADD     150           LENGH            15 5
009700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009800000616     C                   PARM                    depcmd
009900000616     C                   PARM                    LENGH
010000000613     C*
010100050223     C                   if        not %open(titvi4ps)
010200050223     C                   open      titvi4ps
010300000616     C                   except    testdett
010400000613     C                   endif
010500010601     C*
010600010601     C                   endif
010700010601     C*
010800010601     C                   ENDSR
010900010601     C***
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011500010601     C*--------------------------------------------------------
011600010601     C     ENDELA        BEGSR
011700990915     C*
011800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011900000613     C*
012000050223     C                   if        %open(titvi4ps)
012100000616     C                   except    findett
012200050223     C                   close     titvi4ps
012300000613     C                   endif
012400000616     C*
012500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012600011113     C                   Z-ADD     150           LENGH            15 5
012700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012800000616     C                   PARM                    CMD(3)
012900000616     C                   PARM                    LENGH
013000000616     C*
013100010601     C                   ENDSR
013200010601     C***
013300010601
013400010601
013500000613
013600000613     C*--------------------------------------------------------
013700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013800000613     C*--------------------------------------------------------
013900000621     C     STPR          BEGSR
014000000613     C*
014100050223     C                   if        not %open(titvi4p)
014200050223     C                   open      titvi4p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700050223     C                   if        %open(titvi4p)
014800050223     C                   close     titvi4p
014900990914     C                   endif
015000000613     C*
015100000613     C                   ENDSR
015200000613     C***
015300000613
015400010601
015500010601
015600010330     C*--------------------------------------------------------
015700040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015800010330     C*--------------------------------------------------------
015900010605     C     RWVAB         BEGSR
016000010330     C*
016100010330     C                   if        not %open(tivin00r)
016200010330     C                   open      tivin00r
016300010330     C                   endif
016400021014     C                   if        not %open(fivabwwr)
016500021014     C                   open      fivabwwr
016600010330     C                   endif
016700010330     C*
016800010604     C                   clear                   §CTROK
016900010604     C                   clear                   §CTRMO
017000010604     C                   clear                   §CTRNO
017100010330     C*
017200010330     C                   DO        *HIVAL
017300010330     C*
017400010330     C                   READ      tivin00r                               70
017500010618     C*
017600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017700010618     C                   if        vinflg = '1'
017800010618     C                   eval      flgOk = '1'
017900010618     C                   endif
018000010618     C*
018100010330     C                   if        vindta > *blanks
018200010330     C                   add       1             rrnum
018300010330     C*
018400010601     C                   if        *in70 = *off and
018500010330     C                             (vinflg = *blanks
018600010330     C                              or vinflg = '0'
018700010330     C                              or vinflg = '2')
018800050217     C*
018900050217     C                   clear                   FIVAB000
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
020900050114     C                   endif
021000010601     C*
021100010601     C                   exsr      PREELA
021200010601     C*
021300010604     C* Ebbene...
021400010604     C*
021500010604     C  N31              ADD       1             §CTROK            7 0
021600010604     C   32              ADD       1             §CTRMO            7 0
021700010604     C   31              ADD       1             §CTRNO            7 0
021800020205     C*
021900040301     C* Scarico il buffer del FIVAB
022000021014     C  N31              WRITE     FIVAB000
022100010604     C*
022200020722     C                   endif
022300020722     C*
022400010604     C                   if        *in31 = *off and
022500010604     C                             *in32 = *off
022600010604     C                   eval      vinflg = '1'
022700010604     C                   else
022800010604     C                   eval      recko = vindta
022900010604     C                   except    rigadett
023000010604     C                   eval      vinflg = '2'
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
026900021014     C                   if        %open(fivabwwr)
027000021014     C                   close     fivabwwr
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
030900021014     C                   CLEAR                   FIVAB000
031000020204     C* Imposto i valori di default...
031100060201     C                   EVAL      VABCCM = 0053449
031200060201     C                   EVAL      VABLNP = 005
031300050223     C                   EVAL      VABCTR = 000
031400041108     C                   EVAL      VABCBO = '1'
031500050218     C                   EVAL      VABTSP = 'C'
031501070711     C                   EVAL      VABNAS = 'PROFUMERIA'
031600020204     C* ... e poi verifico se sono stati passati come parametri
031700020204     C                   IF        vlrppt > *blanks
031800040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032000020204     C                   EXSR      CHKNUM
032100020204     C                   IF        PiInt=*on
032200020204     C                   Z-ADD     PiVal         VABCCM
032300020204     C                   ENDIF
032400040301     C                   ENDIF
032500040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032700020204     C                   EXSR      CHKNUM
032800020204     C                   IF        PiInt=*on
032900020204     C                   Z-ADD     PiVal         VABLNP
033000020204     C                   ENDIF
033100040301     C                   ENDIF
033200040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033400020204     C                   EXSR      CHKNUM
033500020204     C                   IF        PiInt=*on
033600020204     C                   Z-ADD     PiVal         VABCTR
033700040301     C                   ENDIF
033800020204     C                   ENDIF
033900020204     C                   ENDIF
034000020204     C*
034100010330     C                   ENDSR
034200010607     C*----------------------------------------------------*
034300021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034400010607     C*----------------------------------------------------*
034500010607     C     IMPVAB        BEGSR
034600010607     C*
034700010607     C                   SETOFF                                       3132
034800010607     C*
034900010607     C* Reperimento campi ALFA
035000010607     C*
035100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035200070711     C                   EVAL      VABRSD=%trim(%subst(vindta:71:32))
035300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035500020117     C* ==
035600070711     C                   EVAL      VABRD2=%trim(%subst(vindta:103:32))
035700070711     C                   EVAL      VABIND=%trim(%subst(vindta:135:32))
035800070711     C                   EVAL      VABLOD=%trim(%subst(vindta:208:32))
035900070711     C                   EVAL      VABRMA=%trim(%subst(vindta:51:10))
036000070711     C                   EVAL      VABNOT=%trim(%subst(vindta:344:35))
036001070711     C                   EVAL      VABNT2=%trim(%subst(vindta:379:35))
036100010607     C*
036200010607     C* Reperimento campi NUMERICI
036300020722     C                   MOVEL     DATCOR        VABAAS
036400020722     C                   MOVE      DATCOR        VABMGS
036500040420     C* NSP/RMN
036600070711     C                   EVAL      PiStr=%trim(%subst(vindta:29:11))
036700020304     C                   EXSR      CHKNUM
036800020304     C                   IF        PiInt=*on
036900040301     C                   Z-ADD     PiVal         VABNSP
037000040301     C                   Z-ADD     PiVal         VABRMN
037100020304     C                   ELSE
037200050217     C                   SETON                                        31
037300050217     C                   Z-ADD     *zeros        VABNSP
037400050217     C                   Z-ADD     *zeros        VABRMN
037500020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037600040301     C                             + ' ' + 'VABNSP VABRMN'
037700020304     C                   ENDIF
037800050128     C* CAD
037900070711     C                   EVAL      PiStr=%trim(%subst(vindta:199:9))
038000050128     C                   EXSR      CHKNUM
038100050128     C                   IF        PiInt=*on
038200050128     C                   Z-ADD     PiVal         Num5_0
038300050128     C                   MOVEL(p)  Num5_0        VABCAD
038400050128     C                   ELSE
038500050128     C                   SETON                                        32
038600050128     C                   EVAL      VABCAD = *zeros
038700050128     C                   EVAL      vinmsg = %trimr(vinmsg)
038800050128     C                             + ' ' + 'VABCAD'
038900050128     C                   ENDIF
039000050223     C* PRD (reperita mediante TISI95)
039100050223     C                   IF        VABCAD <> *blanks
039200050223     C                   CLEAR                   TISI95DS
039300050223     C                   EVAL      I95TCN = '3'
039400050223     C                   Z-ADD     datcor        I95DAT
039500050223     C                   EVAL      I95CAP = VABCAD
039600050223     C                   EVAL      I95LOC = VABLOD
039700050223     C                   CALL      'TISI95R'
039800050223     C                   PARM                    TISI95DS
039900050223     C                   EVAL      VABPRD = O95PRV
040000050223     C                   ENDIF
040100040420     C* NCL
040200070711     C                   EVAL      PiStr=%trim(%subst(vindta:280:5))
040300010607     C                   EXSR      CHKNUM
040400010607     C                   IF        PiInt=*on
040500010607     C                   Z-ADD     PiVal         VABNCL
040600010607     C                   ELSE
040700010607     C                   SETON                                        32
040800010607     C                   Z-ADD     *zeros        VABNCL
040900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041000010607     C                             + ' ' + 'VABNCL'
041100010607     C                   ENDIF
041200040420     C* PKB
041300070711     C                   EVAL      PiStr=%trim(%subst(vindta:288:8))
041400010607     C                   EXSR      CHKNUM
041500010607     C                   IF        PiNum=*on
041600050114     C                   Z-ADD     PiVal         VABPKB
041700010607     C                   ELSE
041800010607     C                   SETON                                        32
041900010607     C                   Z-ADD     *zeros        VABPKB
042000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042100010607     C                             + ' ' + 'VABPKB'
042200010607     C                   ENDIF
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
044600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
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
050300050304     C                   EVAL      PiDecChr = '.'
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
056700050217     C                   move      §ctrok        cmznrr
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
062500050223     Otitvi4p   E            riepilogo         2
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
064200971022     O                       §CTROK        2   +  1
064300990915     O          E            riepilogo   2
064400990915     O                                              'Sbagliati e corretti.....:'
064500971022     O                       §CTRMO        2   +  1
064600990915     O          E            riepilogo   2
064700990915     O                                              'Sbagliati e scartati.....:'
064800971022     O                       §CTRNO        2   +  1
064900000613
065000050223     Otitvi4ps  E            testdett          2
065100000613     O                                              'Upload via Internet'
065200000613     O                                           +1 'Traduzione TIVIN00R -'
065300021014     O                                           +1 'FIVABWWR'
065400000613     O                                           +1 'Testate bolle'
065500000616     O          E            testdett    3
065600000613     O                                           +2 'N° rec'
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
067300000613     O                       §CTROK        2   +  1
067400000616     O          E            findett     2
067500000613     O                                              'Sbagliati e corretti.....:'
067600000613     O                       §CTRMO        2   +  1
067700000616     O          E            findett     2
067800000613     O                                              'Sbagliati e scartati.....:'
067900000613     O                       §CTRNO        2   +  1
068000000613** CMD - COMANDI CL
068100050223OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068200050223OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068300050223DLTOVR FILE(TITVI4P TITVI4PS) LVL(*)
068400010606
068500000613
