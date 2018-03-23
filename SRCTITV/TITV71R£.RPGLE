000100031017      /TITLE Upload via Internet: traduzione in FIVABWWR. (x TRANS. OCHOA - Cod. 0501586)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700031017     Ftitv71p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900031017     Ftitv71ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900031020     D tisi95ds      e ds
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
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
005300010601     C*
005400010601     C                   seton                                        lr
005500010601
005600010601
005700010601
005800010601
005900010601     C*--------------------------------------------------------
006000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006100010601     C*--------------------------------------------------------
006200010601     C     PREELA        BEGSR
006300010601     C*
006400010601     C* SE OCCORRE SPEDIRE IN FILIALE
006500010601     C                   if        invfil <> *zeros and
006600010601     C                             flgGiro = '0'
006700010601     C*
006800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006900010601     C                   eval      flgGiro = '1'
007000000724     C*
007100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007300010601     C     invfil        chain     azorg01l
007400000724     C                   if        %found
007500000724     C                   movel(p)  CMD(1)        depcmd
007600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007700000724     C*
007800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007900011113     C                   Z-ADD     150           LENGH            15 5
008000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008100000724     C                   PARM                    depcmd
008200000724     C                   PARM                    LENGH
008300000724     C*
008400000724     C                   endif
008500000616     C*
008600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008700000616     C                   movel(p)  CMD(2)        depcmd
008800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008900000616     C*
009000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009100011113     C                   Z-ADD     150           LENGH            15 5
009200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009300000616     C                   PARM                    depcmd
009400000616     C                   PARM                    LENGH
009500000613     C*
009600031017     C                   if        not %open(titv71ps)
009700031017     C                   open      titv71ps
009800000616     C                   except    testdett
009900000613     C                   endif
010000010601     C*
010100010601     C                   endif
010200010601     C*
010300010601     C                   ENDSR
010400010601     C***
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011400010601     C*--------------------------------------------------------
011500010601     C     ENDELA        BEGSR
011600990915     C*
011700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011800050201     C*
011900050201     C* Effettuo lancio TISI95 solo x chiusura
012000050201     C                   CLEAR                   TISI95DS
012100050201     C                   EVAL      I95TLA = 'C'
012200050201     C                   CALL      'TISI95R'
012300050201     C                   PARM                    TISI95DS
012400000613     C*
012500031017     C                   if        %open(titv71ps)
012600000616     C                   except    findett
012700031017     C                   close     titv71ps
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
014800031017     C                   if        not %open(titv71p)
014900031017     C                   open      titv71p
015000990915     C                   endif
015100990915     C*
015200990915     C                   except    riepilogo
015300990915     C*
015400031017     C                   if        %open(titv71p)
015500031017     C                   close     titv71p
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
016600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016700010330     C*--------------------------------------------------------
016800010605     C     RWVAB         BEGSR
016900010330     C*
017000010330     C                   if        not %open(tivin00r)
017100010330     C                   open      tivin00r
017200010330     C                   endif
017300021025     C                   if        not %open(fivabwwr)
017400021025     C                   open      fivabwwr
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
022400021025     C  N31              WRITE     FIVAB000
022500010604     C*
022600010604     C                   if        *in31 = *off and
022700010604     C                             *in32 = *off
022800010604     C                   eval      vinflg = '1'
022900010604     C                   else
023000010604     C                   eval      recko = vindta
023100010604     C                   except    rigadett
023200010604     C                   eval      vinflg = '2'
023300010604     C                   endif
023400010604     C                   endif
023500010601     C*
023600010604     C                   endif
023700010604     C*
023800010330     C                   else
023900010330     C                   eval      vinflg = '1'
024000010330     C                   endif
024100010601     C*
024200010601     C  N70              update    tivin000
024300010330     C*
024400010330     C  N70              ENDdo
024500010601     C
024600010601     C*
024700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024800010601     C                   if        cntNonEl = *zeros or
024900010601     C                             flgMulti = '0'
025000010330     C* Se non ci sono record con errori ...
025100010601     C                   if        §ctrno = 0 and
025200010604     C                             §ctrmo = 0 and
025300010601     C                             flgStato <> '2'
025400010330     C* ... restituisco esito OK.
025500010330     C                   eval      wrkesito = '0'
025600010330     C                   else
025700010330     C                   if        §ctrok > 0
025800010330     C                   eval      wrkesito = '1'
025900010330     C                   else
026000010615     C                   if        flgOk = '0'
026100010615     C                   eval      wrkesito = '2'
026200010615     C                   else
026300010615     C                   eval      wrkesito = '6'
026400010615     C                   endif
026500010330     C                   endif
026600010330     C                   endif
026700010601     C                   else
026800010601     C                   eval      wrkesito = '9'
026900010601     C                   endif
027000010330     C*
027100010330     C                   if        %open(tivin00r)
027200010330     C                   close     tivin00r
027300010330     C                   endif
027400021025     C                   if        %open(fivabwwr)
027500021025     C                   close     fivabwwr
027600010330     C                   endif
027700010601     C*
027800010601     C                   if        vlrpoi <> 999
027900010601     C                   eval      invfil = vlrpoi
028000010601     C                   endif
028100010330     C*
028200010330     C                   if        §ctrok > 0
028300010601     C                             and invfil > *zeros
028400010330     C                   exsr      invio
028500010330     C                   endif
028600010601     C*
028700010618     C                   if        flgGiro = '1'
028800010601     C                   exsr      endela
028900010618     C                   endif
029000010330     C*
029100010330     C                   ENDSR
029200010330     C***
029300010601
029400010601
029500010601
029600010330     C*----------------------------------------------------*
029700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029800010330     C*----------------------------------------------------*
029900010330     C     INZVAR        BEGSR
030000010330     C*
030100020204     C                   Z-ADD     *zeros        Num5_0            5 0
030200030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030300030606     C                   MOVEL     '0'           FlgCAS            1
030400010330     C*
030500010330     C                   ENDSR
030600010330     C*----------------------------------------------------*
030700010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030800010330     C*----------------------------------------------------*
030900010330     C     DEFCAM        BEGSR
031000010330     C*
031100020204     C* Inizializzo il buffer del record da scrivere
031200021025     C                   CLEAR                   FIVAB000
031300020204     C* Imposto i valori di default...
031400031017     C                   EVAL      VABCCM = 0501586
031500031017     C                   EVAL      VABLNP = 050
031600031013     C                   EVAL      VABCTR = 000
031700020204     C* ... e poi verifico se sono stati passati come parametri
031800020204     C                   IF        vlrppt > *blanks
031900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032000020204     C                   EXSR      CHKNUM
032100020204     C                   IF        PiInt=*on
032200020204     C                   Z-ADD     PiVal         VABCCM
032300020204     C                   ENDIF
032400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032500020204     C                   EXSR      CHKNUM
032600020204     C                   IF        PiInt=*on
032700020204     C                   Z-ADD     PiVal         VABLNP
032800020204     C                   ENDIF
032900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033000020204     C                   EXSR      CHKNUM
033100020204     C                   IF        PiInt=*on
033200020204     C                   Z-ADD     PiVal         VABCTR
033300020204     C                   ENDIF
033400020204     C                   ENDIF
033500020204     C*
033600010330     C                   ENDSR
033700010607     C*----------------------------------------------------*
033800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033900010607     C*----------------------------------------------------*
034000010607     C     IMPVAB        BEGSR
034100010607     C*
034200010607     C                   SETOFF                                       3132
034300030515     C*
034400010607     C                   MOVEL     datcor        VABAAS
034500010607     C                   MOVE      datcor        VABMGS
034600010607     C*
034700010607     C* Reperimento campi ALFA
034800010607     C*
034900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035000031017     C                   EVAL      VABRSD=%trim(%subst(vindta:280:40))
035100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035300020117     C* ==
035400031017     C                   EVAL      VABRD2=%trim(%subst(vindta:320:40))
035500031017     C                   EVAL      VABIND=%trim(%subst(vindta:360:40))
035600031107     C                   EVAL      VABLOD=%trim(%subst(vindta:440:30))
035700031107     C*
035800031107     C* Effettuo alcune considerazioni sulla località x estrapolare la provincia racchiusa tra ()
035900031107     C                   Z-ADD     *zeros        pos               3 0
036000031107     C                   EVAL      pos = %scan('(':VABLOD)
036100031107     C                   IF        pos > *zeros
036200031107     C                   EVAL      VABPRD=%trim(%subst(vindta:(440+pos):2))
036300031107     C                   EVAL      VABLOD=%trim(%subst(vindta:440:(pos-1)))
036400031107     C                   ENDIF
036500031107     C*
036600031017     C                   EVAL      VABNOT='Tel: '+%trim(%subst(vindta:477:15))
036700031017     C                   EVAL      VABRMA=%trim(%subst(vindta:528:12))
036800010607     C*
036900010607     C* Reperimento campi NUMERICI
037000020604     C*
037100030515     C* CAD
037200031017     C                   EVAL      PiStr=%trim(%subst(vindta:470:7))
037300010607     C                   EXSR      CHKNUM
037400010607     C                   IF        PiInt=*on
037500020204     C                   Z-ADD     PiVal         Num5_0
037600020204     C                   MOVEL(p)  Num5_0        VABCAD
037700010607     C                   ELSE
037800010607     C                   SETON                                        32
037900020204     C                   EVAL      VABCAD = *zeros
038000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038100020204     C                             + ' ' + 'VABCAD'
038200010607     C                   ENDIF
038300031107     C* Se la provincia nn è ancora valorizzata reperisco la provincia dal CAP e dalla località
038400031107     C                   IF        VABPRD =  *blanks
038500031013     C                   IF        VABCAD <> *blanks
038600031013     C                   CLEAR                   TISI95DS
038700031013     C                   EVAL      I95TCN = '3'
038800031013     C                   Z-ADD     datcor        I95DAT
038900031013     C                   EVAL      I95CAP = VABCAD
039000031013     C                   EVAL      I95LOC = VABLOD
039100031013     C                   CALL      'TISI95R'
039200031013     C                   PARM                    TISI95DS
039300031013     C                   EVAL      VABPRD = O95PRV
039400031013     C                   ENDIF
039500031107     C                   ENDIF
039600030515     C* RMN/NSP
039700031017     C                   EVAL      PiStr=%trim(%subst(vindta:528:12))
039800011023     C                   EXSR      CHKNUM
039900011023     C                   IF        PiInt=*on
040000030225     C                   Z-ADD     PiVal         VABRMN
040100031013     C                   MOVE(P)   VABRMN        VABNSP
040200011023     C                   ELSE
040300020205     C                   EVAL      VABRMN = 1
040400011023     C                   ENDIF
040500030515     C* NCL
040600031017     C                   EVAL      PiStr=%trim(%subst(vindta:575:5))
040700010607     C                   EXSR      CHKNUM
040800010607     C                   IF        PiInt=*on
040900010607     C                   Z-ADD     PiVal         VABNCL
041000010607     C                   ELSE
041100010607     C                   SETON                                        32
041200010607     C                   Z-ADD     *zeros        VABNCL
041300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041400010607     C                             + ' ' + 'VABNCL'
041500010607     C                   ENDIF
041600031013     C* PKB
041700031017     C                   EVAL      PiStr=%trim(%subst(vindta:569:6))
041800031013     C                   EXSR      CHKNUM
041900031017     C                   IF        PiNum=*on
042000031017     C                   EVAL(H)   VABPKB = PiVal/10                            * gestisco 1 dec.
042100031013     C                   ELSE
042200031013     C                   SETON                                        32
042300031013     C                   Z-ADD     *zeros        VABPKB
042400031013     C                   EVAL      vinmsg = %trimr(vinmsg)
042500031013     C                             + ' ' + 'VABPKB'
042600031013     C                   ENDIF
042700031017     C* VLB
042800031017     C                   EVAL      PiStr=%trim(%subst(vindta:580:4))
042900031017     C                   EXSR      CHKNUM
043000031017     C                   IF        PiNum=*on
043100031017     C                   EVAL(H)   VABVLB = PiVal/100                           * gestisco 2 dec.
043200031017     C                   ELSE
043300031017     C                   SETON                                        32
043400031017     C                   Z-ADD     *zeros        VABVLB
043500031017     C                   EVAL      vinmsg = %trimr(vinmsg)
043600031017     C                             + ' ' + 'VABVLB'
043700031017     C                   ENDIF
043800031017     C* CAS
043900031020     C                   IF        %subst(vindta:597:9) <> *zeros
044000031020     C                   EVAL      PiStr=%trim(%subst(vindta:597:9))
044100031017     C                   EVAL      FlgCAS = '1'
044200031017     C                   EXSR      CHKNUM
044300031017     C                   IF        PiNum=*on
044400031017     C                   EVAL      VABCAS = PiVal/100                           * gestisco 2 dec.
044500031029     C                   EVAL      VABVCA = 'EUR'
044600031017     C                   ELSE
044700031017     C                   SETON                                        32
044800031017     C                   Z-ADD     *zeros        VABCAS
044900031017     C                   EVAL      vinmsg = %trimr(vinmsg)
045000031017     C                             + ' ' + 'VABCAS'
045100031017     C                   ENDIF
045200031017     C                   ENDIF
045300030929     C*
045400030929     C* Considerazioni finali su CBO/CAS
045500030929     C                   IF        FlgCAS = '1'
045600030929     C                   EVAL      VABCBO = '4'
045700030929     C                   ELSE
045800030929     C                   EVAL      VABCBO = '1'
045900030929     C                   ENDIF
046000020204     C*
046100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046200020204     C                   EXSR      CHKIMPDIV
046300020204     C*
046400010607     C                   ENDSR
046500010607     C*----------------------------------------------------*
046600010601
046700020204
046800020204     C*----------------------------------------------------*
046900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047000020204     C*----------------------------------------------------*
047100020204     C     CHKIMPDIV     BEGSR
047200020204     C*
047300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047400020204     C                   Z-ADD     *zeros        wrkDec            9 9
047500020204     C*
047600020204     C* Come prima cosa effettuo considerazioni sulla divisa
047700020204     C                   IF        vabIAS > *zeros
047800020204     C                   IF        vabVAS <> 'EUR'
047900020204     C                   EVAL      vabVAS =  'ITL'
048000020204     C                   ENDIF
048100020204     C                   ENDIF
048200020204     C*
048300020204     C                   IF        vabCAS > *zeros
048400020204     C                   IF        vabVCA <> 'EUR'
048500020204     C                   EVAL      vabVCA =  'ITL'
048600020204     C                   ENDIF
048700020204     C                   ENDIF
048800020204     C*
048900020204     C                   IF        vabVMD > *zeros
049000020204     C                   IF        vabVAD <> 'EUR'
049100020204     C                   EVAL      vabVAD =  'ITL'
049200020204     C                   ENDIF
049300020204     C                   ENDIF
049400020204     C*
049500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049600020204     C                   Z-ADD     vabIAS        wrkDec
049700020204     C                   IF        wrkDec > *zeros
049800020204     C                   IF        vabVAS = 'ITL'
049900020204     C                   EVAL      vabIAS = *zeros
050000020204     C                   ENDIF
050100020204     C                   ENDIF
050200020204     C*
050300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
050400020204     C                   Z-ADD     vabCAS        wrkDec
050500020204     C                   IF        wrkDec > *zeros
050600020204     C                   IF        vabVCA = 'ITL'
050700020204     C                   EVAL      vabCAS = *zeros
050800020204     C                   ENDIF
050900020204     C                   ENDIF
051000020204     C*
051100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051200020204     C                   Z-ADD     vabVMD        wrkDec
051300020204     C                   IF        wrkDec > *zeros
051400020204     C                   IF        vabVAD = 'ITL'
051500020204     C                   EVAL      vabVMD = *zeros
051600020204     C                   ENDIF
051700020204     C                   ENDIF
051800020204     C*
051900020204     C                   ENDSR
052000020204     C***
052100020204
052200010330
052300010330
052400010330     C*----------------------------------------------------*
052500010330     C*  CONTROLLO NUMERICITA' CAMPI
052600010330     C*----------------------------------------------------*
052700010330     C     CHKNUM        BEGSR
052800010330     C*
052900010606     C                   IF        PiDecChr = *blanks
053000030606     C                   EVAL      PiDecChr = ','
053100010606     C                   ENDIF
053200010606     C*
053300010606     C                   CALL(e)   'ISNUMERIC'
053400010330     C                   PARM                    PiStr            30
053500010606     C                   PARM                    PiDecChr          1
053600010330     C                   PARM      *ZEROS        PiVal            30 9
053700010330     C                   PARM      '0'           PiInt             1
053800010330     C                   PARM      '0'           PiNum             1
053900010330     C                   IF        %error
054000010606     C                   EVAL      PiNum=*off
054100010330     C                   ENDIF
054200010330     C*
054300010330     C                   ENDSR
054400010330     C***
054500010330
054600010601
054700010601
054800010601
054900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
055000010601     C     repfil        BEGSR
055100010601     C*
055200010601     C                   if        invfil = *zeros and
055300010601     C                             depfil > *zeros and
055400010629     C                             (vinflg = *blanks or
055500010629     C                              vinflg = *zeros)
055600010601     C
055700010601     C                   eval      invfil = depfil
055800010601     C                   endif
055900010601     C*
056000010601     C                   if        depfil <> invfil and
056100010601     C                             invfil > *zeros
056200010601     C                   eval      flgMulti = '1'
056300010601     C                   if        vinflg = *blanks
056400010601     C                   add       1             cntNonEl
056500010601     C                   endif
056600010601     C                   endif
056700010601     C*
056800010601     C                   if        vinflg = '2'
056900010601     C                   eval      flgStato = '2'
057000010601     C                   endif
057100010601     C*
057200010601     C                   ENDSR
057300010601     C***
057400010601
057500010601
057600010601
057700010330
057800010330
057900010330
058000990920      /TITLE Invio dei dati al punto operativo.
058100000613     C     invio         BEGSR
058200990920     C*
058300990920     C                   reset                   dscmz
058400010601     C                   move      invfil        cmzdst
058500990920     C                   eval      cmzfld = vlrfou
058600990920     C                   eval      cmzmbd = vlrhdl
058700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
058800000710     C                   if        prmfir = *blanks
058900021025     C                   eval      cmzfla = 'FIVAB00F'
059000021025     C                   eval      cmzmba = 'FIVAB00F'
059100000710     C                   else
059200000710     C                   eval      cmzfla = prmfir
059300000710     C                   eval      cmzmba = prmfir
059400000710     C                   endif
059500990920     C                   eval      cmznrr = *zeros
059600990920     C                   move      §ctrok        cmznrr
059700021018     C                   eval      cmzlba = vlrfl1
059800990920     C                   call(e)   'TIS711C'
059900990920     C                   parm                    dscmz
060000990921     C                   parm      *blanks       esito
060100990920     C                   if        %error
060200990920     C                             or cmzerr = '1'
060300990921     C                             or esito  = '1'
060400000710     C                   eval      wrkesito = '3'
060500990920     C                   endif
060600990920     C*
060700000613     C                   ENDSR
060800990910
060900010601
061000010601
061100010601
061200010601
061300010601      /TITLE Invio dei dati al punto operativo.
061400010601     C     opeini        BEGSR
061500010601     C*
061600010601     C* Inizializzo flag e contatori operativi
061700010601     C                   movel     '0'           flgGiro           1
061800010601     C                   movel     '0'           flgMulti          1
061900010601     C                   movel     '1'           flgStato          1
062000010615     C                   movel     '0'           flgOk             1
062100010601     C                   z-add     *zeros        cntNonEl         10 0
062200010601     C                   z-add     *zeros        depfil            3 0
062300010601     C                   z-add     *zeros        invfil            3 0
062400010601     C*
062500010601     C                   ENDSR
062600010601     C***
062700010601
062800010601
062900010601
063000010330
063100010330
063200000613     C     *inzsr        BEGSR
063300990910     C*
063400990910     C     *entry        plist
063500990920     C                   parm                    tivlrds
063600990921     C                   parm      wrkesito      esito
063700000724     C                   parm                    prmlit
063800000710     C                   parm                    prmfir
063900010330     C*
064000010330     C* CALCOLA LA DATA CORRENTE
064100010330     C                   time                    wn14             14 0
064200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064300010330     C                   z-add     wn8           g08dat
064400010330     C                   z-add     *zeros        g08inv
064500010330     C                   movel     '0'           g08err
064600010330     C                   call      'XSRDA8'
064700010330     C                   parm                    wlbda8
064800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064900000613     C*
065000000613     C                   ENDSR
065100000613     C***
065200990908
065300031017     Otitv71p   E            riepilogo         2
065400990915     O                                              'Upload via Internet'
065500990915     O                                           +1 'Traduzione TIVIN00R -'
065600021025     O                                           +1 'FIVABWWR'
065700990915     O                                           +1 'Testate bolle'
065800990915     O          E            riepilogo   2
065900990915     O                       wrkdata
066000990915     O                       wrkora              +1
066100990915     O                       procname            +1
066200990915     O          E            riepilogo   2
066300990915     O                                              'Cliente..................:'
066400010605     O                       VABCCM        z     +1
066500990915     O          E            riepilogo   2
066600990920     O                                              'Riferimento Strategi.....:'
066700990920     O                       vlrhdl              +1
066800990915     O          E            riepilogo   2
066900990915     O                                              'Giusti...................:'
067000971022     O                       §CTROK        2   +  1
067100990915     O          E            riepilogo   2
067200990915     O                                              'Sbagliati e corretti.....:'
067300971022     O                       §CTRMO        2   +  1
067400990915     O          E            riepilogo   2
067500990915     O                                              'Sbagliati e scartati.....:'
067600971022     O                       §CTRNO        2   +  1
067700000613
067800031017     Otitv71ps  E            testdett          2
067900000613     O                                              'Upload via Internet'
068000000613     O                                           +1 'Traduzione TIVIN00R -'
068100021025     O                                           +1 'FIVABWWR'
068200000613     O                                           +1 'Testate bolle'
068300000616     O          E            testdett    3
068400000613     O                                           +2 'N° rec'
068500000613     O                                           +3 'Anteprima contenuto'
068600000616     O          E            rigadett    2
068700000613     O                       rrnum               +2
068800000621     O                       recko               +3
068900000616     O          E            findett     2
069000000613     O                       wrkdata
069100000613     O                       wrkora              +1
069200000613     O                       procname            +1
069300000616     O          E            findett     2
069400000613     O                                              'Cliente..................:'
069500010605     O                       VABCCM        z     +1
069600000616     O          E            findett     2
069700000613     O                                              'Riferimento Strategi.....:'
069800000613     O                       vlrhdl              +1
069900000616     O          E            findett     2
070000000613     O                                              'Giusti...................:'
070100000613     O                       §CTROK        2   +  1
070200000616     O          E            findett     2
070300000613     O                                              'Sbagliati e corretti.....:'
070400000613     O                       §CTRMO        2   +  1
070500000616     O          E            findett     2
070600000613     O                                              'Sbagliati e scartati.....:'
070700000613     O                       §CTRNO        2   +  1
070800000613** CMD - COMANDI CL
070900031017OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV71P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
071000031017OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV71PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
071100031017DLTOVR FILE(TITV71P TITV71PS) LVL(*)
071200010606
071300000613
