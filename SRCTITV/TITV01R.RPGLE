000100021025      /TITLE Upload via Internet: traduzione in FIVABWWR. (x BENLOG/BENETTON - Cod. 0590586/0590803)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700020204     Ftitvdrp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900020204     Ftitvdrps  O    f  198        PRINTER usropn
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
009500020204     C                   if        not %open(titvdrps)
009600020204     C                   open      titvdrps
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
011800020204     C                   if        %open(titvdrps)
011900000616     C                   except    findett
012000020204     C                   close     titvdrps
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
014100020204     C                   if        not %open(titvdrp)
014200020204     C                   open      titvdrp
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700020204     C                   if        %open(titvdrp)
014800020204     C                   close     titvdrp
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
021900020205     C                   if        *in88 = *off
022000021025     C  N31              WRITE     FIVAB000
022100020205     C                   endif
022200010604     C*
022300010604     C                   if        *in31 = *off and
022400010604     C                             *in32 = *off
022500010604     C                   eval      vinflg = '1'
022600010604     C                   else
022700010604     C                   eval      recko = vindta
022800010604     C                   except    rigadett
022900010604     C                   eval      vinflg = '2'
023000010604     C                   endif
023100010604     C                   endif
023200010601     C*
023300010604     C                   endif
023400010604     C*
023500010330     C                   else
023600010330     C                   eval      vinflg = '1'
023700010330     C                   endif
023800010601     C*
023900010601     C  N70              update    tivin000
024000010330     C*
024100010330     C  N70              ENDdo
024200010601     C
024300010601     C*
024400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024500010601     C                   if        cntNonEl = *zeros or
024600010601     C                             flgMulti = '0'
024700010330     C* Se non ci sono record con errori ...
024800010601     C                   if        §ctrno = 0 and
024900010604     C                             §ctrmo = 0 and
025000010601     C                             flgStato <> '2'
025100010330     C* ... restituisco esito OK.
025200010330     C                   eval      wrkesito = '0'
025300010330     C                   else
025400010330     C                   if        §ctrok > 0
025500010330     C                   eval      wrkesito = '1'
025600010330     C                   else
025700010615     C                   if        flgOk = '0'
025800010615     C                   eval      wrkesito = '2'
025900010615     C                   else
026000010615     C                   eval      wrkesito = '6'
026100010615     C                   endif
026200010330     C                   endif
026300010330     C                   endif
026400010601     C                   else
026500010601     C                   eval      wrkesito = '9'
026600010601     C                   endif
026700010330     C*
026800010330     C                   if        %open(tivin00r)
026900010330     C                   close     tivin00r
027000010330     C                   endif
027100021025     C                   if        %open(fivabwwr)
027200021025     C                   close     fivabwwr
027300010330     C                   endif
027400010601     C*
027500010601     C                   if        vlrpoi <> 999
027600010601     C                   eval      invfil = vlrpoi
027700010601     C                   endif
027800010330     C*
027900010330     C                   if        §ctrok > 0
028000010601     C                             and invfil > *zeros
028100010330     C                   exsr      invio
028200010330     C                   endif
028300010601     C*
028400010618     C                   if        flgGiro = '1'
028500010601     C                   exsr      endela
028600010618     C                   endif
028700010330     C*
028800010330     C                   ENDSR
028900010330     C***
029000010330
029100010601
029200010601
029300010601
029400010601
029500010330     C*----------------------------------------------------*
029600010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029700010330     C*----------------------------------------------------*
029800010330     C     INZVAR        BEGSR
029900010330     C*
030000020204     C                   Z-ADD     *zeros        Num5_0            5 0
030100020204     C                   MOVEL     *blanks       WrkNote          70
030200010330     C*
030300010330     C                   ENDSR
030400010330     C*----------------------------------------------------*
030500010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030600010330     C*----------------------------------------------------*
030700010330     C     DEFCAM        BEGSR
030800010330     C*
030900020204     C* Inizializzo il buffer del record da scrivere
031000021025     C                   CLEAR                   FIVAB000
031100020204     C* Imposto i valori di default...
031200020204     C                   EVAL      VABCCM = 0160052
031300020204     C                   EVAL      VABLNP = 016
031400020204     C                   EVAL      VABCTR = 000
031500020204     C* ... e poi verifico se sono stati passati come parametri
031600020204     C                   IF        vlrppt > *blanks
031700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031800020204     C                   EXSR      CHKNUM
031900020204     C                   IF        PiInt=*on
032000020204     C                   Z-ADD     PiVal         VABCCM
032100020204     C                   ENDIF
032200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032300020204     C                   EXSR      CHKNUM
032400020204     C                   IF        PiInt=*on
032500020204     C                   Z-ADD     PiVal         VABLNP
032600020204     C                   ENDIF
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABCTR
033100020204     C                   ENDIF
033200020204     C                   ENDIF
033300020204     C*
033400010330     C                   ENDSR
033500010607     C*----------------------------------------------------*
033600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033700010607     C*----------------------------------------------------*
033800010607     C     IMPVAB        BEGSR
033900010607     C*
034000010607     C                   SETOFF                                       3132
034100010607     C                   MOVEL     datcor        VABAAS
034200010607     C                   MOVE      datcor        VABMGS
034300010607     C*
034400010607     C* Reperimento campi ALFA
034500010607     C*
034600010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034700020204     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
034800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035000020117     C* ==
035100020204     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
035200020204     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
035300020204     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
035400020204     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
035500020204     C                   IF        %subst(vindta:16:1) = '1'
035600020204     C                   EVAL      VABFFD='S'
035700020204     C                   ENDIF
035800020204     C                   IF        %subst(vindta:92:15) > *blanks
035900020204     C                   EVAL      WrkNote = 'Tel. '          +
036000020204     C                             %trim(%subst(vindta:92:15))
036100020204     C                   ENDIF
036200020204     C                   EVAL      WrkNote = %trim(WrkNote) + ' ' +
036300020204     C                                       %trim(%subst(vindta:197:80))
036400020204     C                   EVAL      VABNOT = %subst(WrkNote:1:35)
036500020204     C                   EVAL      VABNT2 = %subst(WrkNote:36:35)
036600010607     C*
036700010607     C* Reperimento campi NUMERICI
036800020204     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
036900010607     C                   EXSR      CHKNUM
037000010607     C                   IF        PiInt=*on
037100020204     C                   Z-ADD     PiVal         Num5_0
037200020204     C                   MOVEL(p)  Num5_0        VABCAD
037300010607     C                   ELSE
037400010607     C                   SETON                                        32
037500020204     C                   EVAL      VABCAD = *zeros
037600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037700020204     C                             + ' ' + 'VABCAD'
037800010607     C                   ENDIF
037900011023     C*
038000020204     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
038100011023     C                   EXSR      CHKNUM
038200011023     C                   IF        PiInt=*on
038300011023     C                   Z-ADD     PiVal         VABRMN
038400011023     C                   ELSE
038500020205     C                   EVAL      VABRMN = 1
038600011023     C                   ENDIF
038700010607     C*
038800020204     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
038900010607     C                   EXSR      CHKNUM
039000010607     C                   IF        PiInt=*on
039100010607     C                   Z-ADD     PiVal         VABNCL
039200010607     C                   ELSE
039300010607     C                   SETON                                        32
039400010607     C                   Z-ADD     *zeros        VABNCL
039500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039600010607     C                             + ' ' + 'VABNCL'
039700010607     C                   ENDIF
039800010607     C*
039900020204     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
040000020204     C                   EVAL      PiDecChr = ','
040100010607     C                   EXSR      CHKNUM
040200010607     C                   IF        PiNum=*on
040300010607     C                   Z-ADD(H)  PiVal         VABPKB
040400010607     C                   ELSE
040500010607     C                   SETON                                        32
040600010607     C                   Z-ADD     *zeros        VABPKB
040700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040800010607     C                             + ' ' + 'VABPKB'
040900010607     C                   ENDIF
041000020204     C*
041100020205     C* La DA ROLD passa a volte dei record con colli e peso a *ZEROS che in realta
041200020205     C* sono già inclusi in altra spedizione della transazione => in questo caso
041300020205     C* non considero la bolla.
041400020205     C                   SETOFF                                       88
041500020205     C                   IF        VABPKB = *zeros AND
041600020205     C                             VABNCL = *zeros
041700020205     C                   SETON                                        88
041800020205     C                   ENDIF
041900020205     C*
042000020204     C                   IF        %subst(vindta:280:12) = *blanks
042100020204     C                   EVAL      PiStr=*all'0'
042200020204     C                   ELSE
042300020204     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
042400020204     C                   ENDIF
042500020204     C                   EVAL      PiDecChr=','
042600020204     C                   EXSR      CHKNUM
042700020204     C                   IF        PiNum=*on
042800020204     C                   Z-ADD     PiVal         VABCAS
042900020204     C                   ELSE
043000020204     C                   SETON                                        32
043100020204     C                   Z-ADD     *zeros        VABCAS
043200020204     C                   EVAL      vinmsg = %trimr(vinmsg)
043300020204     C                             + ' ' + 'VABCAS'
043400020204     C                   ENDIF
043500020204     C*
043600020204     C                   IF        %subst(vindta:295:12) = *blanks
043700020204     C                   EVAL      PiStr=*all'0'
043800020204     C                   ELSE
043900020204     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
044000020204     C                   ENDIF
044100020204     C                   EVAL      PiDecChr=','
044200020204     C                   EXSR      CHKNUM
044300020204     C                   IF        PiNum=*on
044400020204     C                   Z-ADD     PiVal         VABIAS
044500020204     C                   ELSE
044600020204     C                   SETON                                        32
044700020204     C                   Z-ADD     *zeros        VABIAS
044800020204     C                   EVAL      vinmsg = %trimr(vinmsg)
044900020204     C                             + ' ' + 'VABIAS'
045000020204     C                   ENDIF
045100020204     C* In questo caso il cliente passa già il tipo porto x cui lo imposto come valore di default
045200020204     C* ... ma poi lo verifico!!!
045300020204     C                   EVAL      VABCBO = %subst(vindta:390:1)
045400020204     C*
045500020204     C* Considerazioni finali sui valori sopra ricavati
045600020204     C                   IF        (VABCAS = *zeros)
045700020204     C                   EVAL      VABCBO = '1 '
045800020204     C                   ELSE
045900020204     C                   EVAL      VABCBO = '4 '
046000020204     C                   EVAL      VABVCA = %trim(%subst(vindta:277:3))
046100020204     C                   ENDIF
046200020204     C*
046300020204     C                   IF        (VABIAS > *zeros)
046400020204     C                   EVAL      VABVCA = %trim(%subst(vindta:292:3))
046500020204     C                   ENDIF
046600020204     C*
046700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046800020204     C                   EXSR      CHKIMPDIV
046900020204     C*
047000010607     C                   ENDSR
047100010607     C*----------------------------------------------------*
047200010601
047300020204
047400020204     C*----------------------------------------------------*
047500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047600020204     C*----------------------------------------------------*
047700020204     C     CHKIMPDIV     BEGSR
047800020204     C*
047900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048000020204     C                   Z-ADD     *zeros        wrkDec            9 9
048100020204     C*
048200020204     C* Come prima cosa effettuo considerazioni sulla divisa
048300020204     C                   IF        vabIAS > *zeros
048400020204     C                   IF        vabVAS <> 'EUR'
048500020204     C                   EVAL      vabVAS =  'ITL'
048600020204     C                   ENDIF
048700020204     C                   ENDIF
048800020204     C*
048900020204     C                   IF        vabCAS > *zeros
049000020204     C                   IF        vabVCA <> 'EUR'
049100020204     C                   EVAL      vabVCA =  'ITL'
049200020204     C                   ENDIF
049300020204     C                   ENDIF
049400020204     C*
049500020204     C                   IF        vabVMD > *zeros
049600020204     C                   IF        vabVAD <> 'EUR'
049700020204     C                   EVAL      vabVAD =  'ITL'
049800020204     C                   ENDIF
049900020204     C                   ENDIF
050000020204     C*
050100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050200020204     C                   Z-ADD     vabIAS        wrkDec
050300020204     C                   IF        wrkDec > *zeros
050400020204     C                   IF        vabVAS = 'ITL'
050500020204     C                   EVAL      vabIAS = *zeros
050600020204     C                   ENDIF
050700020204     C                   ENDIF
050800020204     C*
050900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
051000020204     C                   Z-ADD     vabCAS        wrkDec
051100020204     C                   IF        wrkDec > *zeros
051200020204     C                   IF        vabVCA = 'ITL'
051300020204     C                   EVAL      vabCAS = *zeros
051400020204     C                   ENDIF
051500020204     C                   ENDIF
051600020204     C*
051700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051800020204     C                   Z-ADD     vabVMD        wrkDec
051900020204     C                   IF        wrkDec > *zeros
052000020204     C                   IF        vabVAD = 'ITL'
052100020204     C                   EVAL      vabVMD = *zeros
052200020204     C                   ENDIF
052300020204     C                   ENDIF
052400020204     C*
052500020204     C                   ENDSR
052600020204     C***
052700020204
052800010330
052900010330
053000010330     C*----------------------------------------------------*
053100010330     C*  CONTROLLO NUMERICITA' CAMPI
053200010330     C*----------------------------------------------------*
053300010330     C     CHKNUM        BEGSR
053400010330     C*
053500010606     C                   IF        PiDecChr = *blanks
053600010606     C                   EVAL      PiDecChr = ','
053700010606     C                   ENDIF
053800010606     C*
053900010606     C                   CALL(e)   'ISNUMERIC'
054000010330     C                   PARM                    PiStr            30
054100010606     C                   PARM                    PiDecChr          1
054200010330     C                   PARM      *ZEROS        PiVal            30 9
054300010330     C                   PARM      '0'           PiInt             1
054400010330     C                   PARM      '0'           PiNum             1
054500010330     C                   IF        %error
054600010606     C                   EVAL      PiNum=*off
054700010330     C                   ENDIF
054800010330     C*
054900010330     C                   ENDSR
055000010330     C***
055100010330
055200010601
055300010601
055400010601
055500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
055600010601     C     repfil        BEGSR
055700010601     C*
055800010601     C                   if        invfil = *zeros and
055900010601     C                             depfil > *zeros and
056000010629     C                             (vinflg = *blanks or
056100010629     C                              vinflg = *zeros)
056200010601     C
056300010601     C                   eval      invfil = depfil
056400010601     C                   endif
056500010601     C*
056600010601     C                   if        depfil <> invfil and
056700010601     C                             invfil > *zeros
056800010601     C                   eval      flgMulti = '1'
056900010601     C                   if        vinflg = *blanks
057000010601     C                   add       1             cntNonEl
057100010601     C                   endif
057200010601     C                   endif
057300010601     C*
057400010601     C                   if        vinflg = '2'
057500010601     C                   eval      flgStato = '2'
057600010601     C                   endif
057700010601     C*
057800010601     C                   ENDSR
057900010601     C***
058000010601
058100010601
058200010601
058300010330
058400010330
058500010330
058600990920      /TITLE Invio dei dati al punto operativo.
058700000613     C     invio         BEGSR
058800990920     C*
058900990920     C                   reset                   dscmz
059000010601     C                   move      invfil        cmzdst
059100990920     C                   eval      cmzfld = vlrfou
059200990920     C                   eval      cmzmbd = vlrhdl
059300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
059400000710     C                   if        prmfir = *blanks
059500021025     C                   eval      cmzfla = 'FIVAB00F'
059600021025     C                   eval      cmzmba = 'FIVAB00F'
059700000710     C                   else
059800000710     C                   eval      cmzfla = prmfir
059900000710     C                   eval      cmzmba = prmfir
060000000710     C                   endif
060100990920     C                   eval      cmznrr = *zeros
060200990920     C                   move      §ctrok        cmznrr
060300021018     C                   eval      cmzlba = vlrfl1
060400990920     C                   call(e)   'TIS711C'
060500990920     C                   parm                    dscmz
060600990921     C                   parm      *blanks       esito
060700990920     C                   if        %error
060800990920     C                             or cmzerr = '1'
060900990921     C                             or esito  = '1'
061000000710     C                   eval      wrkesito = '3'
061100990920     C                   endif
061200990920     C*
061300000613     C                   ENDSR
061400990910
061500010601
061600010601
061700010601
061800010601
061900010601      /TITLE Invio dei dati al punto operativo.
062000010601     C     opeini        BEGSR
062100010601     C*
062200010601     C* Inizializzo flag e contatori operativi
062300010601     C                   movel     '0'           flgGiro           1
062400010601     C                   movel     '0'           flgMulti          1
062500010601     C                   movel     '1'           flgStato          1
062600010615     C                   movel     '0'           flgOk             1
062700010601     C                   z-add     *zeros        cntNonEl         10 0
062800010601     C                   z-add     *zeros        depfil            3 0
062900010601     C                   z-add     *zeros        invfil            3 0
063000010601     C*
063100010601     C                   ENDSR
063200010601     C***
063300010601
063400010601
063500010601
063600010330
063700010330
063800000613     C     *inzsr        BEGSR
063900990910     C*
064000990910     C     *entry        plist
064100990920     C                   parm                    tivlrds
064200990921     C                   parm      wrkesito      esito
064300000724     C                   parm                    prmlit
064400000710     C                   parm                    prmfir
064500010330     C*
064600010330     C* CALCOLA LA DATA CORRENTE
064700010330     C                   time                    wn14             14 0
064800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064900010330     C                   z-add     wn8           g08dat
065000010330     C                   z-add     *zeros        g08inv
065100010330     C                   movel     '0'           g08err
065200010330     C                   call      'XSRDA8'
065300010330     C                   parm                    wlbda8
065400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065500000613     C*
065600000613     C                   ENDSR
065700000613     C***
065800990908
065900020204     Otitvdrp   E            riepilogo         2
066000990915     O                                              'Upload via Internet'
066100990915     O                                           +1 'Traduzione TIVIN00R -'
066200021025     O                                           +1 'FIVABWWR'
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
068400020204     Otitvdrps  E            testdett          2
068500000613     O                                              'Upload via Internet'
068600000613     O                                           +1 'Traduzione TIVIN00R -'
068700021025     O                                           +1 'FIVABWWR'
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
071500020204OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVDRP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
071600020204OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVDRPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
071700020204DLTOVR FILE(TITVDRP TITVDRPS) LVL(*)
071800010606
071900000613
