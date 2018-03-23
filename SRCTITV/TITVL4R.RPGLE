000100050322      /TITLE Upload via Internet: traduzione in FIVABWWR. (x PCS Cod. 0011108)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700050322     Ftitvl4p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050322     Ftitvl4ps  O    f  198        PRINTER usropn
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
003200050322     D wdcr            s              8  0
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
004300041103     D trul13ds      e ds                  inz
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
009700050322     C                   if        not %open(titvl4ps)
009800050322     C                   open      titvl4ps
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
010900010601     C*--------------------------------------------------------
011000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011100010601     C*--------------------------------------------------------
011200010601     C     ENDELA        BEGSR
011300990915     C*
011400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011500000613     C*
011600050322     C                   if        %open(titvl4ps)
011700000616     C                   except    findett
011800050322     C                   close     titvl4ps
011900000613     C                   endif
012000000616     C*
012100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    CMD(3)
012500000616     C                   PARM                    LENGH
012600000616     C*
012700010601     C                   ENDSR
012800010601     C***
012900010601
013000010601
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700050322     C                   if        not %open(titvl4p)
013800050322     C                   open      titvl4p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300050322     C                   if        %open(titvl4p)
014400050322     C                   close     titvl4p
014500990914     C                   endif
014600000613     C*
014700000613     C                   ENDSR
014800000613     C***
014900000613
015000010601
015100010601
015200010330     C*--------------------------------------------------------
015300040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015400010330     C*--------------------------------------------------------
015500010605     C     RWVAB         BEGSR
015600010330     C*
015700010330     C                   if        not %open(tivin00r)
015800010330     C                   open      tivin00r
015900010330     C                   endif
016000021014     C                   if        not %open(fivabwwr)
016100021014     C                   open      fivabwwr
016200010330     C                   endif
016300010330     C*
016400010604     C                   clear                   §CTROK
016500010604     C                   clear                   §CTRMO
016600010604     C                   clear                   §CTRNO
016700010330     C*
016800010330     C                   DO        *HIVAL
016900010330     C*
017000010330     C                   READ      tivin00r                               70
017100010618     C*
017200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017300010618     C                   if        vinflg = '1'
017400010618     C                   eval      flgOk = '1'
017500010618     C                   endif
017600010618     C*
017700010330     C                   if        vindta > *blanks
017800010330     C                   add       1             rrnum
017900010330     C*
018000010601     C                   if        *in70 = *off and
018100010330     C                             (vinflg = *blanks
018200010330     C                              or vinflg = '0'
018300010330     C                              or vinflg = '2')
018400010330     C*
018500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018600010711     C                   if        vinflg = *blanks or vinflg = '0'
018700010711     C                   clear                   vinmsg
018800010711     C                   endif
018900010601     C*
019000010330     C                   exsr      inzvar
019100010330     C                   exsr      defcam
019200010605     C                   exsr      impvab
019300010601     C*
019400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019500010605     C                   eval      depfil = VABLNP
019600010601     C                   exsr      repfil
019700010601     C                   if        depfil = invfil
019800021025     C                   if        vlrpoi = 999
019900021025     C                   MOVE(P)   invfil        VABFGS
020000021025     C                   else
020100021025     C                   MOVE(P)   vlrpoi        VABFGS
020200021025     C                   endif
020300010601     C*
020400010601     C                   exsr      PREELA
020500010601     C*
020600010604     C* Ebbene...
020700010604     C*
020800010604     C  N31              ADD       1             §CTROK            7 0
020900010604     C   32              ADD       1             §CTRMO            7 0
021000010604     C   31              ADD       1             §CTRNO            7 0
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
022500010604     C                   endif
022600010604     C*
022700010330     C                   else
022800010330     C                   eval      vinflg = '1'
022900010330     C                   endif
023000010601     C*
023100010601     C  N70              update    tivin000
023200010330     C*
023300010330     C  N70              ENDdo
023400010601     C
023500010601     C*
023600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023700010601     C                   if        cntNonEl = *zeros or
023800010601     C                             flgMulti = '0'
023900010330     C* Se non ci sono record con errori ...
024000010601     C                   if        §ctrno = 0 and
024100010604     C                             §ctrmo = 0 and
024200010601     C                             flgStato <> '2'
024300010330     C* ... restituisco esito OK.
024400010330     C                   eval      wrkesito = '0'
024500010330     C                   else
024600010330     C                   if        §ctrok > 0
024700010330     C                   eval      wrkesito = '1'
024800010330     C                   else
024900010615     C                   if        flgOk = '0'
025000010615     C                   eval      wrkesito = '2'
025100010615     C                   else
025200010615     C                   eval      wrkesito = '6'
025300010615     C                   endif
025400010330     C                   endif
025500010330     C                   endif
025600010601     C                   else
025700010601     C                   eval      wrkesito = '9'
025800010601     C                   endif
025900010330     C*
026000010330     C                   if        %open(tivin00r)
026100010330     C                   close     tivin00r
026200010330     C                   endif
026300021014     C                   if        %open(fivabwwr)
026400021014     C                   close     fivabwwr
026500010330     C                   endif
026600010601     C*
026700010601     C                   if        vlrpoi <> 999
026800010601     C                   eval      invfil = vlrpoi
026900010601     C                   endif
027000010330     C*
027100010330     C                   if        §ctrok > 0
027200010601     C                             and invfil > *zeros
027300010330     C                   exsr      invio
027400010330     C                   endif
027500010601     C*
027600010618     C                   if        flgGiro = '1'
027700010601     C                   exsr      endela
027800010618     C                   endif
027900010330     C*
028000010330     C                   ENDSR
028100010330     C***
028200010330
028300010601
028400010601
028500010601
028600010601
028700010330     C*----------------------------------------------------*
028800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028900010330     C*----------------------------------------------------*
029000010330     C     INZVAR        BEGSR
029100010330     C*
029200020204     C                   Z-ADD     *zeros        Num5_0            5 0
029300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029400020322     C                   MOVEL     '0'           FlgCAS            1
029500010330     C*
029600010330     C                   ENDSR
029700010330     C*----------------------------------------------------*
029800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029900010330     C*----------------------------------------------------*
030000010330     C     DEFCAM        BEGSR
030100010330     C*
030200020204     C* Inizializzo il buffer del record da scrivere
030300021014     C                   CLEAR                   FIVAB000
030400020204     C* Imposto i valori di default...
030500050322     C                   EVAL      VABCCM = 0011108
030600041103     C                   EVAL      VABLNP = 001
030700040713     C                   EVAL      VABCTR = 000
030800041108     C                   EVAL      VABCBO = '1'
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
034500041103     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
034600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034800020117     C* ==
034900041103     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
035000041103     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
035100041103     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
035200041103     C                   EVAL      VABRMA=%trim(%subst(vindta:114:4))
035300050322     C                   IF        %subst(vindta:207:1)='F'
035400050322     C                   EVAL      VABCBO='1'
035500050322     C                   ENDIF
035600050322     C                   IF        %subst(vindta:207:1)='A'
035700050322     C                   EVAL      VABCBO='2'
035800050322     C                   ENDIF
035900010607     C*
036000010607     C* Reperimento campi NUMERICI
036100020722     C                   MOVEL     DATCOR        VABAAS
036200020722     C                   MOVE      DATCOR        VABMGS
036300040420     C* NSP/RMN
036400041122     C                   EVAL      PiStr=%trim(%subst(vindta:113:5))
036500020304     C                   EXSR      CHKNUM
036600020304     C                   IF        PiInt=*on
036700040301     C                   Z-ADD     PiVal         VABNSP
036800040301     C                   Z-ADD     PiVal         VABRMN
036900020304     C                   ELSE
037000020722     C                   SETON                                        31
037100020304     C                   Z-ADD     *zeros        VABNSP
037200040301     C                   Z-ADD     *zeros        VABRMN
037300020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037400040301     C                             + ' ' + 'VABNSP VABRMN'
037500020304     C                   ENDIF
037600040420     C* CAD
037700041103     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
037800010607     C                   EXSR      CHKNUM
037900010607     C                   IF        PiInt=*on
038000020204     C                   Z-ADD     PiVal         Num5_0
038100020204     C                   MOVEL(p)  Num5_0        VABCAD
038200010607     C                   ELSE
038300010607     C                   SETON                                        32
038400020204     C                   EVAL      VABCAD = *zeros
038500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038600020204     C                             + ' ' + 'VABCAD'
038700010607     C                   ENDIF
038800040420     C* NCL
038900041103     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
039000010607     C                   EXSR      CHKNUM
039100010607     C                   IF        PiInt=*on
039200010607     C                   Z-ADD     PiVal         VABNCL
039300010607     C                   ELSE
039400010607     C                   SETON                                        32
039500010607     C                   Z-ADD     *zeros        VABNCL
039600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039700010607     C                             + ' ' + 'VABNCL'
039800010607     C                   ENDIF
039900040420     C* PKB
040000041103     C                   EVAL      PiStr=%trim(%subst(vindta:129:8))
040100010607     C                   EXSR      CHKNUM
040200010607     C                   IF        PiNum=*on
040300040420     C                   Z-ADD     PiVal         VABPKB
040400010607     C                   ELSE
040500010607     C                   SETON                                        32
040600010607     C                   Z-ADD     *zeros        VABPKB
040700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040800010607     C                             + ' ' + 'VABPKB'
040900010607     C                   ENDIF
041000041103     C* CAS
041100041103     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
041200041103     C                   EXSR      CHKNUM
041300041103     C                   IF        PiNum=*on
041400041103     C                   Z-ADD     PiVal         VABCAS
041500041108     C                   EVAL      VABVCA = 'EUR'
041600041103     C                   ELSE
041700041103     C                   SETON                                        32
041800041103     C                   Z-ADD     *zeros        VABCAS
041900041103     C                   EVAL      vinmsg = %trimr(vinmsg)
042000041103     C                             + ' ' + 'VABCAS'
042100041103     C                   ENDIF
042200050322     C*
042300050322     C* Verifico alcune particolarità che condizionano in special modo il contrassegno (e quindi
042400050322     C* anche il tipo bolla)
042500050322     C                   IF        %trim(%subst(vindta:208:3)) = '010' OR
042600050322     C                             %trim(%subst(vindta:208:3)) = '011'
042700050322     C                   MOVEL     '1'           FlgCAS
042800050322     C                   EVAL      VABTIC = 'BM'
042900050322     C                   ELSE
043000061024     C                   IF        %trim(%subst(vindta:208:3)) = '002'
043100061024     C                   MOVEL     '1'           FlgCAS
043200061024     C                   ELSE
043300050322     C                   Z-ADD     *zeros        VABCAS
043400061024     C                   ENDIF
043500050322     C                   ENDIF
043600050322     D* DCR
043700041108     C                   IF        %trim(%subst(vindta:208:3)) = '003'
043800050322     C                   EVAL      VABDCR = wdcr
043900041103     C                   ENDIF
044000020322     C*
044100020322     C* Considerazioni finali su CBO/CAS
044200020322     C                   IF        FlgCAS = '1'
044300040713     C                   IF        VABCBO = '1'
044400020322     C                   EVAL      VABCBO = '4'
044500040713     C                   ENDIF
044600040713     C                   IF        VABCBO = '2'
044700040713     C                   EVAL      VABCBO = '6'
044800040713     C                   ENDIF
044900020322     C                   ENDIF
045000020204     C*
045100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045200020204     C                   EXSR      CHKIMPDIV
045300020204     C*
045400010607     C                   ENDSR
045500010607     C*----------------------------------------------------*
045600010601
045700020204
045800020204     C*----------------------------------------------------*
045900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046000020204     C*----------------------------------------------------*
046100020204     C     CHKIMPDIV     BEGSR
046200020204     C*
046300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046400020204     C                   Z-ADD     *zeros        wrkDec            9 9
046500020204     C*
046600020204     C* Come prima cosa effettuo considerazioni sulla divisa
046700020204     C                   IF        vabIAS > *zeros
046800020204     C                   IF        vabVAS <> 'EUR'
046900020204     C                   EVAL      vabVAS =  'ITL'
047000020204     C                   ENDIF
047100020204     C                   ENDIF
047200020204     C*
047300020204     C                   IF        vabCAS > *zeros
047400020204     C                   IF        vabVCA <> 'EUR'
047500020204     C                   EVAL      vabVCA =  'ITL'
047600020204     C                   ENDIF
047700020204     C                   ENDIF
047800020204     C*
047900020204     C                   IF        vabVMD > *zeros
048000020321     C                   IF        vabVAD <> 'EUR'
048100020204     C                   EVAL      vabVAD =  'ITL'
048200020204     C                   ENDIF
048300020204     C                   ENDIF
048400020204     C*
048500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048600020204     C                   Z-ADD     vabIAS        wrkDec
048700020204     C                   IF        wrkDec > *zeros
048800020204     C                   IF        vabVAS = 'ITL'
048900020204     C                   EVAL      vabIAS = *zeros
049000020204     C                   ENDIF
049100020204     C                   ENDIF
049200020204     C*
049300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049400020204     C                   Z-ADD     vabCAS        wrkDec
049500020204     C                   IF        wrkDec > *zeros
049600020204     C                   IF        vabVCA = 'ITL'
049700020204     C                   EVAL      vabCAS = *zeros
049800020204     C                   ENDIF
049900020204     C                   ENDIF
050000020204     C*
050100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050200020204     C                   Z-ADD     vabVMD        wrkDec
050300020204     C                   IF        wrkDec > *zeros
050400020204     C                   IF        vabVAD = 'ITL'
050500020204     C                   EVAL      vabVMD = *zeros
050600020204     C                   ENDIF
050700020204     C                   ENDIF
050800020204     C*
050900020204     C                   ENDSR
051000020204     C***
051100020204
051200010330
051300010330
051400010330     C*----------------------------------------------------*
051500010330     C*  CONTROLLO NUMERICITA' CAMPI
051600010330     C*----------------------------------------------------*
051700010330     C     CHKNUM        BEGSR
051800010330     C*
051900010606     C                   IF        PiDecChr = *blanks
052000010606     C                   EVAL      PiDecChr = ','
052100010606     C                   ENDIF
052200010606     C*
052300010606     C                   CALL(e)   'ISNUMERIC'
052400010330     C                   PARM                    PiStr            30
052500010606     C                   PARM                    PiDecChr          1
052600010330     C                   PARM      *ZEROS        PiVal            30 9
052700010330     C                   PARM      '0'           PiInt             1
052800010330     C                   PARM      '0'           PiNum             1
052900010330     C                   IF        %error
053000010606     C                   EVAL      PiNum=*off
053100010330     C                   ENDIF
053200010330     C*
053300010330     C                   ENDSR
053400010330     C***
053500010330
053600010601
053700010601
053800010601
053900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
054000010601     C     repfil        BEGSR
054100010601     C*
054200010601     C                   if        invfil = *zeros and
054300010601     C                             depfil > *zeros and
054400010629     C                             (vinflg = *blanks or
054500010629     C                              vinflg = *zeros)
054600010601     C
054700010601     C                   eval      invfil = depfil
054800010601     C                   endif
054900010601     C*
055000010601     C                   if        depfil <> invfil and
055100010601     C                             invfil > *zeros
055200010601     C                   eval      flgMulti = '1'
055300010601     C                   if        vinflg = *blanks
055400010601     C                   add       1             cntNonEl
055500010601     C                   endif
055600010601     C                   endif
055700010601     C*
055800010601     C                   if        vinflg = '2'
055900010601     C                   eval      flgStato = '2'
056000010601     C                   endif
056100010601     C*
056200010601     C                   ENDSR
056300010601     C***
056400010601
056500010601
056600010601
056700010330
056800010330
056900010330
057000990920      /TITLE Invio dei dati al punto operativo.
057100000613     C     invio         BEGSR
057200990920     C*
057300990920     C                   reset                   dscmz
057400010601     C                   move      invfil        cmzdst
057500990920     C                   eval      cmzfld = vlrfou
057600990920     C                   eval      cmzmbd = vlrhdl
057700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
057800000710     C                   if        prmfir = *blanks
057900021014     C                   eval      cmzfla = 'FIVAB00F'
058000021014     C                   eval      cmzmba = 'FIVAB00F'
058100000710     C                   else
058200000710     C                   eval      cmzfla = prmfir
058300000710     C                   eval      cmzmba = prmfir
058400000710     C                   endif
058500990920     C                   eval      cmznrr = *zeros
058600990920     C                   move      §ctrok        cmznrr
058700021018     C                   eval      cmzlba = vlrfl1
058800990920     C                   call(e)   'TIS711C'
058900990920     C                   parm                    dscmz
059000990921     C                   parm      *blanks       esito
059100990920     C                   if        %error
059200990920     C                             or cmzerr = '1'
059300990921     C                             or esito  = '1'
059400000710     C                   eval      wrkesito = '3'
059500990920     C                   endif
059600990920     C*
059700000613     C                   ENDSR
059800990910
059900010601
060000010601
060100010601
060200010601
060300010601      /TITLE Invio dei dati al punto operativo.
060400010601     C     opeini        BEGSR
060500010601     C*
060600010601     C* Inizializzo flag e contatori operativi
060700010601     C                   movel     '0'           flgGiro           1
060800010601     C                   movel     '0'           flgMulti          1
060900010601     C                   movel     '1'           flgStato          1
061000010615     C                   movel     '0'           flgOk             1
061100010601     C                   z-add     *zeros        cntNonEl         10 0
061200010601     C                   z-add     *zeros        depfil            3 0
061300010601     C                   z-add     *zeros        invfil            3 0
061400010601     C*
061500010601     C                   ENDSR
061600010601     C***
061700010601
061800010601
061900010601
062000010330
062100010330
062200000613     C     *inzsr        BEGSR
062300990910     C*
062400990910     C     *entry        plist
062500990920     C                   parm                    tivlrds
062600990921     C                   parm      wrkesito      esito
062700000724     C                   parm                    prmlit
062800000710     C                   parm                    prmfir
062900010330     C*
063000010330     C* CALCOLA LA DATA CORRENTE
063100010330     C                   time                    wn14             14 0
063200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063300010330     C                   z-add     wn8           g08dat
063400010330     C                   z-add     *zeros        g08inv
063500010330     C                   movel     '0'           g08err
063600010330     C                   call      'XSRDA8'
063700010330     C                   parm                    wlbda8
063800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063900050322     C
064000050322     C* Calcolo già oggi + 1 gg lavorativo per la data consegna richiesta
064100041103     C                   clear                   TRUL13DS
064200041103     C                   eval      I13MOD = 'P'
064300041103     C                   eval      I13FIL = 001
064400041103     C                   eval      I13DIN = datcor
064500041103     C                   Movel     wn14          I13HIN
064600041103     C                   eval      I13GIO = 1
064700041103     C                   eval      I13ORE = *zeros
064800041103     C                   call      'TRUL13R'
064900041103     C                   parm                    TRUL13DS
065000041103     C                   if        O13ERR = *blanks
065100041103     C                   eval      wdcr  = O13DFI
065200041103     C                   else
065300041103     C                   eval      wdcr  = datcor
065400041103     C                   endif
065500000613     C*
065600000613     C                   ENDSR
065700000613     C***
065800990908
065900050322     Otitvl4p   E            riepilogo         2
066000990915     O                                              'Upload via Internet'
066100990915     O                                           +1 'Traduzione TIVIN00R -'
066200021014     O                                           +1 'FIVABWWR'
066300990915     O                                           +1 'Testate bolle'
066400990915     O          E            riepilogo   2
066500990915     O                       wrkdata
066600990915     O                       wrkora              +1
066700990915     O                       procname            +1
066800990915     O          E            riepilogo   2
066900990915     O                                              'Cliente..................:'
067000010605     O                       VABCCM        z     +1
067100990915     O          E            riepilogo   2
067200990920     O                                              'Riferimento Strategi.....:'
067300990920     O                       vlrhdl              +1
067400990915     O          E            riepilogo   2
067500990915     O                                              'Giusti...................:'
067600971022     O                       §CTROK        2   +  1
067700990915     O          E            riepilogo   2
067800990915     O                                              'Sbagliati e corretti.....:'
067900971022     O                       §CTRMO        2   +  1
068000990915     O          E            riepilogo   2
068100990915     O                                              'Sbagliati e scartati.....:'
068200971022     O                       §CTRNO        2   +  1
068300000613
068400050322     Otitvl4ps  E            testdett          2
068500000613     O                                              'Upload via Internet'
068600000613     O                                           +1 'Traduzione TIVIN00R -'
068700021014     O                                           +1 'FIVABWWR'
068800000613     O                                           +1 'Testate bolle'
068900000616     O          E            testdett    3
069000000613     O                                           +2 'N° rec'
069100000613     O                                           +3 'Anteprima contenuto'
069200000616     O          E            rigadett    2
069300000613     O                       rrnum               +2
069400000621     O                       recko               +3
069500000616     O          E            findett     2
069600000613     O                       wrkdata
069700000613     O                       wrkora              +1
069800000613     O                       procname            +1
069900000616     O          E            findett     2
070000000613     O                                              'Cliente..................:'
070100010605     O                       VABCCM        z     +1
070200000616     O          E            findett     2
070300000613     O                                              'Riferimento Strategi.....:'
070400000613     O                       vlrhdl              +1
070500000616     O          E            findett     2
070600000613     O                                              'Giusti...................:'
070700000613     O                       §CTROK        2   +  1
070800000616     O          E            findett     2
070900000613     O                                              'Sbagliati e corretti.....:'
071000000613     O                       §CTRMO        2   +  1
071100000616     O          E            findett     2
071200000613     O                                              'Sbagliati e scartati.....:'
071300000613     O                       §CTRNO        2   +  1
071400000613** CMD - COMANDI CL
071500050322OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
071600050322OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
071700050322DLTOVR FILE(TITVL4P TITVL4PS) LVL(*)
071800010606
071900000613
