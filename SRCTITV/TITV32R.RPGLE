000100030130      /TITLE Upload via Internet: traduzione in FIVABWWR. (x LUDOSTORE Cod. 0011462/0011463)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030130     FFIVABwwr  O    E             DISK    usropn
000700030130     Ftitv32p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030130     Ftitv32ps  O    f  198        PRINTER usropn
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
009500030130     C                   if        not %open(titv32ps)
009600030130     C                   open      titv32ps
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
011800030130     C                   if        %open(titv32ps)
011900000616     C                   except    findett
012000030130     C                   close     titv32ps
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
014100030130     C                   if        not %open(titv32p)
014200030130     C                   open      titv32p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700030130     C                   if        %open(titv32p)
014800030130     C                   close     titv32p
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
015900030130     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600030130     C                   if        not %open(fivabwwr)
016700030130     C                   open      fivabwwr
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
020400030130     C                   if        vlrpoi = 999
020500030130     C                   MOVE(P)   invfil        VABFGS
020600030130     C                   else
020700030130     C                   MOVE(P)   vlrpoi        VABFGS
020800030130     C                   endif
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
021900030130     C  N31              WRITE     FIVAB000
022000020722     C*
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
026900030130     C                   if        %open(fivabwwr)
027000030130     C                   close     fivabwwr
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
030900030130     C                   CLEAR                   FIVAB000
031000020204     C* Imposto i valori di default...
031100030130     C                   EVAL      VABCCM = 0011462
031200020722     C                   EVAL      VABLNP = 001
031300020322     C                   EVAL      VABCTR = 000
031400020204     C* ... e poi verifico se sono stati passati come parametri
031500020204     C                   IF        vlrppt > *blanks
031600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031700020204     C                   EXSR      CHKNUM
031800020204     C                   IF        PiInt=*on
031900020204     C                   Z-ADD     PiVal         VABCCM
032000020204     C                   ENDIF
032100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032200020204     C                   EXSR      CHKNUM
032300020204     C                   IF        PiInt=*on
032400020204     C                   Z-ADD     PiVal         VABLNP
032500020204     C                   ENDIF
032600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032700020204     C                   EXSR      CHKNUM
032800020204     C                   IF        PiInt=*on
032900020204     C                   Z-ADD     PiVal         VABCTR
033000020204     C                   ENDIF
033100020204     C                   ENDIF
033200020204     C*
033300010330     C                   ENDSR
033400010607     C*----------------------------------------------------*
033500030130     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033600010607     C*----------------------------------------------------*
033700010607     C     IMPVAB        BEGSR
033800010607     C*
033900010607     C                   SETOFF                                       3132
034000010607     C*
034100010607     C* Reperimento campi ALFA
034200010607     C*
034300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034400030130     C                   EVAL      VABRSD=%trim(%subst(vindta:25:35))
034500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034700020117     C* ==
034800030130     C                   EVAL      VABRD2=%trim(%subst(vindta:(25+35):(60-35)))
034900030130     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
035000030130     C                   EVAL      VABLOD=%trim(%subst(vindta:125:40))
035100030130     C                   EVAL      VABPRD=%trim(%subst(vindta:165:2))
035200030130     C                   EVAL      VABRMA=%trim(%subst(vindta:18:7))
035300030130     C                   EVAL      VABVCA=%trim(%subst(vindta:176:3))
035400040202     C                   EVAL      VABVAS=VABVCA
035500030130     C                   EVAL      VABNOT=%trim(%subst(vindta:185:35))
035600030130     C                   EVAL      VABNT2=%trim(%subst(vindta:(185+35):
035700030130     C                                                        (100-35)))
035800010607     C*
035900010607     C* Reperimento campi NUMERICI
036000030130     C                   MOVEL     datcor        VABAAS
036100030130     C                   MOVE      datcor        VABMGS
036200030130     C* CCM
036300030130     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
036400020304     C                   EXSR      CHKNUM
036500020304     C                   IF        PiInt=*on
036600030130     C                   Z-ADD     PiVal         VABCCM
036700020304     C                   ELSE
036800020722     C                   SETON                                        31
036900020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037000030130     C                             + ' ' + 'VABCCM'
037100020304     C                   ENDIF
037200030130     C* NSP/RMN
037300030130     C                   EVAL      PiStr=%trim(%subst(vindta:18:7))
037400030130     C                   EXSR      CHKNUM
037500030130     C                   IF        PiInt=*on
037600030130     C                   Z-ADD     PiVal         VABNSP
037700030130     C                   Z-ADD     PiVal         VABRMN
037800030130     C                   ELSE
037900030130     C                   SETON                                        31
038000030130     C                   Z-ADD     *zeros        VABNSP
038100030130     C                   Z-ADD     *zeros        VABRMN
038200030130     C                   EVAL      vinmsg = %trimr(vinmsg)
038300030130     C                             + ' ' + 'VABNSP VABRMN'
038400030130     C                   ENDIF
038500030130     C* CAD
038600030130     C                   EVAL      PiStr=%trim(%subst(vindta:120:5))
038700010607     C                   EXSR      CHKNUM
038800010607     C                   IF        PiInt=*on
038900020204     C                   Z-ADD     PiVal         Num5_0
039000020204     C                   MOVEL(p)  Num5_0        VABCAD
039100010607     C                   ELSE
039200010607     C                   SETON                                        32
039300020204     C                   EVAL      VABCAD = *zeros
039400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039500020204     C                             + ' ' + 'VABCAD'
039600010607     C                   ENDIF
039700030130     C* NCL
039800030130     C                   EVAL      PiStr=%trim(%subst(vindta:179:3))
039900010607     C                   EXSR      CHKNUM
040000010607     C                   IF        PiInt=*on
040100010607     C                   Z-ADD     PiVal         VABNCL
040200010607     C                   ELSE
040300010607     C                   SETON                                        32
040400010607     C                   Z-ADD     *zeros        VABNCL
040500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040600010607     C                             + ' ' + 'VABNCL'
040700010607     C                   ENDIF
040800030130     C* PKB
040900030130     C                   EVAL      PiStr=%trim(%subst(vindta:182:3))
041000030130     C                   EXSR      CHKNUM
041100030210     C                   IF        PiNum=*on
041200030130     C                   Z-ADD     PiVal         VABPKB
041300030130     C                   ELSE
041400030130     C                   SETON                                        32
041500030130     C                   Z-ADD     *zeros        VABPKB
041600030130     C                   EVAL      vinmsg = %trimr(vinmsg)
041700030130     C                             + ' ' + 'VABPKB'
041800030130     C                   ENDIF
041900020722     C*
042000030130     C                   EVAL      PiStr=%trim(%subst(vindta:167:9))
042100040128     C* CAS
042200040128     C* Detrmino se indipendentemente da giusto o sbagliato l'intenzione del cliente era d
042300020722     C* passare il contrassegno
042400020722     C                   IF        PiStr > *zeros
042500020722     C                   MOVEL     '1'           FlgCAS
042600020722     C                   ENDIF
042700020722     C*
042800020722     C                   EXSR      CHKNUM
042900020722     C                   IF        PiNum=*on
043000030130     C                   Z-ADD     PiVal         VABCAS
043100020722     C                   ELSE
043200020722     C                   SETON                                        32
043300020722     C                   Z-ADD     *zeros        VABCAS
043400020722     C                   EVAL      vinmsg = %trimr(vinmsg)
043500020722     C                             + ' ' + 'VABCAS'
043600020722     C                   ENDIF
043700040128     C* IAS
043800040128     C                   EVAL      PiStr=%trim(%subst(vindta:285:9))
043900040128     C                   EXSR      CHKNUM
044000040128     C                   IF        PiNum=*on
044100040128     C                   Z-ADD     PiVal         VABIAS
044200040128     C                   ELSE
044300040128     C                   SETON                                        32
044400040128     C                   Z-ADD     *zeros        VABIAS
044500040128     C                   EVAL      vinmsg = %trimr(vinmsg)
044600040128     C                             + ' ' + 'VABIAS'
044700040128     C                   ENDIF
044800020322     C*
044900020322     C* Considerazioni finali su CBO/CAS
045000020322     C                   IF        FlgCAS = '1'
045100020322     C                   EVAL      VABCBO = '4'
045200040128     C* Se indicato il contrassegno determino anche il tipo incasso richiesto dal cliente
045300040128     C                   IF        %trim(%subst(vindta:294:1)) = '1'
045400040128     C                   EVAL      VABTIC = *blanks
045500040128     C                   ENDIF
045600040128     C                   IF        %trim(%subst(vindta:294:1)) = '2'
045700040128     C                   EVAL      VABTIC = 'BM'
045800040128     C                   ENDIF
045900040128     C                   IF        %trim(%subst(vindta:294:1)) = '3'
046000040128     C                   EVAL      VABTIC = 'CM'
046100040128     C                   ENDIF
046200020322     C                   ELSE
046300020322     C                   EVAL      VABCBO = '1'
046400020322     C                   ENDIF
046500020204     C*
046600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046700020204     C                   EXSR      CHKIMPDIV
046800020204     C*
046900010607     C                   ENDSR
047000010607     C*----------------------------------------------------*
047100010601
047200020204
047300020204     C*----------------------------------------------------*
047400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047500020204     C*----------------------------------------------------*
047600020204     C     CHKIMPDIV     BEGSR
047700020204     C*
047800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047900020204     C                   Z-ADD     *zeros        wrkDec            9 9
048000020204     C*
048100020204     C* Come prima cosa effettuo considerazioni sulla divisa
048200020204     C                   IF        vabIAS > *zeros
048300020204     C                   IF        vabVAS <> 'EUR'
048400020204     C                   EVAL      vabVAS =  'ITL'
048500020204     C                   ENDIF
048600020204     C                   ENDIF
048700020204     C*
048800020204     C                   IF        vabCAS > *zeros
048900020204     C                   IF        vabVCA <> 'EUR'
049000020204     C                   EVAL      vabVCA =  'ITL'
049100020204     C                   ENDIF
049200020204     C                   ENDIF
049300020204     C*
049400020204     C                   IF        vabVMD > *zeros
049500020321     C                   IF        vabVAD <> 'EUR'
049600020204     C                   EVAL      vabVAD =  'ITL'
049700020204     C                   ENDIF
049800020204     C                   ENDIF
049900020204     C*
050000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050100020204     C                   Z-ADD     vabIAS        wrkDec
050200020204     C                   IF        wrkDec > *zeros
050300020204     C                   IF        vabVAS = 'ITL'
050400020204     C                   EVAL      vabIAS = *zeros
050500020204     C                   ENDIF
050600020204     C                   ENDIF
050700020204     C*
050800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
050900020204     C                   Z-ADD     vabCAS        wrkDec
051000020204     C                   IF        wrkDec > *zeros
051100020204     C                   IF        vabVCA = 'ITL'
051200020204     C                   EVAL      vabCAS = *zeros
051300020204     C                   ENDIF
051400020204     C                   ENDIF
051500020204     C*
051600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051700020204     C                   Z-ADD     vabVMD        wrkDec
051800020204     C                   IF        wrkDec > *zeros
051900020204     C                   IF        vabVAD = 'ITL'
052000020204     C                   EVAL      vabVMD = *zeros
052100020204     C                   ENDIF
052200020204     C                   ENDIF
052300020204     C*
052400020204     C                   ENDSR
052500020204     C***
052600020204
052700010330
052800010330
052900010330     C*----------------------------------------------------*
053000010330     C*  CONTROLLO NUMERICITA' CAMPI
053100010330     C*----------------------------------------------------*
053200010330     C     CHKNUM        BEGSR
053300010330     C*
053400010606     C                   IF        PiDecChr = *blanks
053500030130     C                   EVAL      PiDecChr = '.'
053600010606     C                   ENDIF
053700010606     C*
053800010606     C                   CALL(e)   'ISNUMERIC'
053900010330     C                   PARM                    PiStr            30
054000010606     C                   PARM                    PiDecChr          1
054100010330     C                   PARM      *ZEROS        PiVal            30 9
054200010330     C                   PARM      '0'           PiInt             1
054300010330     C                   PARM      '0'           PiNum             1
054400010330     C                   IF        %error
054500010606     C                   EVAL      PiNum=*off
054600010330     C                   ENDIF
054700010330     C*
054800010330     C                   ENDSR
054900010330     C***
055000010330
055100010601
055200010601
055300010601
055400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
055500010601     C     repfil        BEGSR
055600010601     C*
055700010601     C                   if        invfil = *zeros and
055800010601     C                             depfil > *zeros and
055900010629     C                             (vinflg = *blanks or
056000010629     C                              vinflg = *zeros)
056100010601     C
056200010601     C                   eval      invfil = depfil
056300010601     C                   endif
056400010601     C*
056500010601     C                   if        depfil <> invfil and
056600010601     C                             invfil > *zeros
056700010601     C                   eval      flgMulti = '1'
056800010601     C                   if        vinflg = *blanks
056900010601     C                   add       1             cntNonEl
057000010601     C                   endif
057100010601     C                   endif
057200010601     C*
057300010601     C                   if        vinflg = '2'
057400010601     C                   eval      flgStato = '2'
057500010601     C                   endif
057600010601     C*
057700010601     C                   ENDSR
057800010601     C***
057900010601
058000010601
058100010601
058200010330
058300010330
058400010330
058500990920      /TITLE Invio dei dati al punto operativo.
058600000613     C     invio         BEGSR
058700990920     C*
058800990920     C                   reset                   dscmz
058900010601     C                   move      invfil        cmzdst
059000990920     C                   eval      cmzfld = vlrfou
059100990920     C                   eval      cmzmbd = vlrhdl
059200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
059300000710     C                   if        prmfir = *blanks
059400030130     C                   eval      cmzfla = 'FIVAB00F'
059500030130     C                   eval      cmzmba = 'FIVAB00F'
059600000710     C                   else
059700000710     C                   eval      cmzfla = prmfir
059800000710     C                   eval      cmzmba = prmfir
059900000710     C                   endif
060000990920     C                   eval      cmznrr = *zeros
060100990920     C                   move      §ctrok        cmznrr
060200021018     C                   eval      cmzlba = vlrfl1
060300990920     C                   call(e)   'TIS711C'
060400990920     C                   parm                    dscmz
060500990921     C                   parm      *blanks       esito
060600990920     C                   if        %error
060700990920     C                             or cmzerr = '1'
060800990921     C                             or esito  = '1'
060900000710     C                   eval      wrkesito = '3'
061000990920     C                   endif
061100990920     C*
061200000613     C                   ENDSR
061300990910
061400010601
061500010601
061600010601
061700010601
061800010601      /TITLE Invio dei dati al punto operativo.
061900010601     C     opeini        BEGSR
062000010601     C*
062100010601     C* Inizializzo flag e contatori operativi
062200010601     C                   movel     '0'           flgGiro           1
062300010601     C                   movel     '0'           flgMulti          1
062400010601     C                   movel     '1'           flgStato          1
062500010615     C                   movel     '0'           flgOk             1
062600010601     C                   z-add     *zeros        cntNonEl         10 0
062700010601     C                   z-add     *zeros        depfil            3 0
062800010601     C                   z-add     *zeros        invfil            3 0
062900010601     C*
063000010601     C                   ENDSR
063100010601     C***
063200010601
063300010601
063400010601
063500010330
063600010330
063700000613     C     *inzsr        BEGSR
063800990910     C*
063900990910     C     *entry        plist
064000990920     C                   parm                    tivlrds
064100990921     C                   parm      wrkesito      esito
064200000724     C                   parm                    prmlit
064300000710     C                   parm                    prmfir
064400010330     C*
064500010330     C* CALCOLA LA DATA CORRENTE
064600010330     C                   time                    wn14             14 0
064700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064800010330     C                   z-add     wn8           g08dat
064900010330     C                   z-add     *zeros        g08inv
065000010330     C                   movel     '0'           g08err
065100010330     C                   call      'XSRDA8'
065200010330     C                   parm                    wlbda8
065300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065400000613     C*
065500000613     C                   ENDSR
065600000613     C***
065700990908
065800030130     Otitv32p   E            riepilogo         2
065900990915     O                                              'Upload via Internet'
066000990915     O                                           +1 'Traduzione TIVIN00R -'
066100030130     O                                           +1 'FIVABWWR'
066200990915     O                                           +1 'Testate bolle'
066300990915     O          E            riepilogo   2
066400990915     O                       wrkdata
066500990915     O                       wrkora              +1
066600990915     O                       procname            +1
066700990915     O          E            riepilogo   2
066800990915     O                                              'Cliente..................:'
066900010605     O                       VABCCM        z     +1
067000990915     O          E            riepilogo   2
067100990920     O                                              'Riferimento Strategi.....:'
067200990920     O                       vlrhdl              +1
067300990915     O          E            riepilogo   2
067400990915     O                                              'Giusti...................:'
067500971022     O                       §CTROK        2   +  1
067600990915     O          E            riepilogo   2
067700990915     O                                              'Sbagliati e corretti.....:'
067800971022     O                       §CTRMO        2   +  1
067900990915     O          E            riepilogo   2
068000990915     O                                              'Sbagliati e scartati.....:'
068100971022     O                       §CTRNO        2   +  1
068200000613
068300030130     Otitv32ps  E            testdett          2
068400000613     O                                              'Upload via Internet'
068500000613     O                                           +1 'Traduzione TIVIN00R -'
068600030130     O                                           +1 'FIVABWWR'
068700000613     O                                           +1 'Testate bolle'
068800000616     O          E            testdett    3
068900000613     O                                           +2 'N° rec'
069000000613     O                                           +3 'Anteprima contenuto'
069100000616     O          E            rigadett    2
069200000613     O                       rrnum               +2
069300000621     O                       recko               +3
069400000616     O          E            findett     2
069500000613     O                       wrkdata
069600000613     O                       wrkora              +1
069700000613     O                       procname            +1
069800000616     O          E            findett     2
069900000613     O                                              'Cliente..................:'
070000010605     O                       VABCCM        z     +1
070100000616     O          E            findett     2
070200000613     O                                              'Riferimento Strategi.....:'
070300000613     O                       vlrhdl              +1
070400000616     O          E            findett     2
070500000613     O                                              'Giusti...................:'
070600000613     O                       §CTROK        2   +  1
070700000616     O          E            findett     2
070800000613     O                                              'Sbagliati e corretti.....:'
070900000613     O                       §CTRMO        2   +  1
071000000616     O          E            findett     2
071100000613     O                                              'Sbagliati e scartati.....:'
071200000613     O                       §CTRNO        2   +  1
071300000613** CMD - COMANDI CL
071400030130OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV32P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
071500030130OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV32PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
071600030130DLTOVR FILE(TITV32P TITV32PS) LVL(*)
071700010606
071800000613
