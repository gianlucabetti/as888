000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700060630     Ftitvt7p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900060630     Ftitvt7ps  O    f  198        PRINTER usropn
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
004000010330
004100010330
004200990915     C                   time                    wrkdata
004300990915     C                   time                    wrkora
004400000913     C                   reset                   rrnum
004500990921     C                   reset                   esito
004600990921     C                   reset                   wrkesito
004700010601     C*
004800010601     C                   exsr      opeini
004900010605     C                   exsr      rwvab
005000010601     C*
005100010601     C                   seton                                        lr
005200010601
005300010601
005400010601
005500010601
005600010601     C*--------------------------------------------------------
005700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005800010601     C*--------------------------------------------------------
005900010601     C     PREELA        BEGSR
006000010601     C*
006100010601     C* SE OCCORRE SPEDIRE IN FILIALE
006200010601     C                   if        invfil <> *zeros and
006300010601     C                             flgGiro = '0'
006400010601     C*
006500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006600010601     C                   eval      flgGiro = '1'
006700000724     C*
006800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007000010601     C     invfil        chain     azorg01l
007100000724     C                   if        %found
007200000724     C                   movel(p)  CMD(1)        depcmd
007300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007400000724     C*
007500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007600011113     C                   Z-ADD     150           LENGH            15 5
007700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007800000724     C                   PARM                    depcmd
007900000724     C                   PARM                    LENGH
008000000724     C*
008100000724     C                   endif
008200000616     C*
008300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008400000616     C                   movel(p)  CMD(2)        depcmd
008500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008600000616     C*
008700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008800011113     C                   Z-ADD     150           LENGH            15 5
008900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009000000616     C                   PARM                    depcmd
009100000616     C                   PARM                    LENGH
009200000613     C*
009300060630     C                   if        not %open(titvt7ps)
009400060630     C                   open      titvt7ps
009500000616     C                   except    testdett
009600000613     C                   endif
009700010601     C*
009800010601     C                   endif
009900010601     C*
010000010601     C                   ENDSR
010100010601     C***
010200010601
010300010601
010400010601
010500010601     C*--------------------------------------------------------
010600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010700010601     C*--------------------------------------------------------
010800010601     C     ENDELA        BEGSR
010900990915     C*
011000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011100000613     C*
011200060630     C                   if        %open(titvt7ps)
011300000616     C                   except    findett
011400060630     C                   close     titvt7ps
011500000613     C                   endif
011600000616     C*
011700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011800011113     C                   Z-ADD     150           LENGH            15 5
011900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012000000616     C                   PARM                    CMD(3)
012100000616     C                   PARM                    LENGH
012200000616     C*
012300010601     C                   ENDSR
012400010601     C***
012500010601
012600010601
012700000613
012800000613     C*--------------------------------------------------------
012900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013000000613     C*--------------------------------------------------------
013100000621     C     STPR          BEGSR
013200000613     C*
013300060630     C                   if        not %open(titvt7p)
013400060630     C                   open      titvt7p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900060630     C                   if        %open(titvt7p)
014000060630     C                   close     titvt7p
014100990914     C                   endif
014200000613     C*
014300000613     C                   ENDSR
014400000613     C***
014500000613
014600010601
014700010601
014800010330     C*--------------------------------------------------------
014900050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015000010330     C*--------------------------------------------------------
015100010605     C     RWVAB         BEGSR
015200050408     C*
015300010330     C                   if        not %open(tivin00r)
015400010330     C                   open      tivin00r
015500010330     C                   endif
015600050408     C                   if        not %open(fivabwwr)
015700050408     C                   open      fivabwwr
015800010330     C                   endif
015900010330     C*
016000010604     C                   clear                   §CTROK
016100010604     C                   clear                   §CTRMO
016200010604     C                   clear                   §CTRNO
016300010330     C*
016400010330     C                   DO        *HIVAL
016500010330     C*
016600010330     C                   READ      tivin00r                               70
016700010618     C*
016800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016900010618     C                   if        vinflg = '1'
017000010618     C                   eval      flgOk = '1'
017100010618     C                   endif
017200010618     C*
017300010330     C                   if        vindta > *blanks
017400010330     C                   add       1             rrnum
017500010330     C*
017600010601     C                   if        *in70 = *off and
017700010330     C                             (vinflg = *blanks
017800010330     C                              or vinflg = '0'
017900010330     C                              or vinflg = '2')
018000010330     C*
018100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018200010711     C                   if        vinflg = *blanks or vinflg = '0'
018300010711     C                   clear                   vinmsg
018400010711     C                   endif
018500010601     C*
018600010330     C                   exsr      inzvar
018700010330     C                   exsr      defcam
018800010605     C                   exsr      impvab
018900010601     C*
019000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019100010605     C                   eval      depfil = VABLNP
019200010601     C                   exsr      repfil
019300010601     C                   if        depfil = invfil
019400021025     C                   if        vlrpoi = 999
019500021025     C                   MOVE(P)   invfil        VABFGS
019600021025     C                   else
019700021025     C                   MOVE(P)   vlrpoi        VABFGS
019800021025     C                   endif
019900010601     C*
020000010601     C                   exsr      PREELA
020100010601     C*
020200010604     C* Ebbene...
020300010604     C*
020400010604     C  N31              ADD       1             §CTROK            7 0
020500010604     C   32              ADD       1             §CTRMO            7 0
020600010604     C   31              ADD       1             §CTRNO            7 0
020700020205     C*
020800050408     C* Scarico il buffer del FIVAB
020900050408     C  N31              WRITE     FIVAB000
021000010604     C*
021100020722     C                   endif
021200020722     C*
021300010604     C                   if        *in31 = *off and
021400010604     C                             *in32 = *off
021500010604     C                   eval      vinflg = '1'
021600010604     C                   else
021700010604     C                   eval      recko = vindta
021800010604     C                   except    rigadett
021900010604     C                   eval      vinflg = '2'
022000010604     C                   endif
022100010604     C                   endif
022200010604     C*
022300010330     C                   else
022400010330     C                   eval      vinflg = '1'
022500010330     C                   endif
022600010601     C*
022700010601     C  N70              update    tivin000
022800010330     C*
022900010330     C  N70              ENDdo
023000010601     C*
023100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023200010601     C                   if        cntNonEl = *zeros or
023300010601     C                             flgMulti = '0'
023400010330     C* Se non ci sono record con errori ...
023500010601     C                   if        §ctrno = 0 and
023600010604     C                             §ctrmo = 0 and
023700010601     C                             flgStato <> '2'
023800010330     C* ... restituisco esito OK.
023900010330     C                   eval      wrkesito = '0'
024000010330     C                   else
024100010330     C                   if        §ctrok > 0
024200010330     C                   eval      wrkesito = '1'
024300010330     C                   else
024400010615     C                   if        flgOk = '0'
024500010615     C                   eval      wrkesito = '2'
024600010615     C                   else
024700010615     C                   eval      wrkesito = '6'
024800010615     C                   endif
024900010330     C                   endif
025000010330     C                   endif
025100010601     C                   else
025200010601     C                   eval      wrkesito = '9'
025300010601     C                   endif
025400010330     C*
025500010330     C                   if        %open(tivin00r)
025600010330     C                   close     tivin00r
025700010330     C                   endif
025800050408     C                   if        %open(fivabwwr)
025900050408     C                   close     fivabwwr
026000010330     C                   endif
026100010601     C*
026200010601     C                   if        vlrpoi <> 999
026300010601     C                   eval      invfil = vlrpoi
026400010601     C                   endif
026500010330     C*
026600010330     C                   if        §ctrok > 0
026700010601     C                             and invfil > *zeros
026800010330     C                   exsr      invio
026900010330     C                   endif
027000010601     C*
027100010618     C                   if        flgGiro = '1'
027200010601     C                   exsr      endela
027300010618     C                   endif
027400010330     C*
027500010330     C                   ENDSR
027600010330     C***
027700010330
027800010601
027900010601
028000010601
028100010601
028200010330     C*----------------------------------------------------*
028300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028400010330     C*----------------------------------------------------*
028500010330     C     INZVAR        BEGSR
028600010330     C*
028700020204     C                   Z-ADD     *zeros        Num5_0            5 0
028800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028900020322     C                   MOVEL     '0'           FlgCAS            1
029000010330     C*
029100010330     C                   ENDSR
029200010330     C*----------------------------------------------------*
029300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029400010330     C*----------------------------------------------------*
029500010330     C     DEFCAM        BEGSR
029600010330     C*
029700020204     C* Inizializzo il buffer del record da scrivere
029800050408     C                   CLEAR                   FIVAB000
029900020204     C* Imposto i valori di default...
030000060630     C                   EVAL      VABCCM = 0053845
030100060630     C                   EVAL      VABLNP = 005
030200060105     C                   EVAL      VABCTR = 000
030300060630     C                   EVAL      VABCTM = '7Q'
030400020204     C* ... e poi verifico se sono stati passati come parametri
030500020204     C                   IF        vlrppt > *blanks
030600040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
030700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030800020204     C                   EXSR      CHKNUM
030900020204     C                   IF        PiInt=*on
031000020204     C                   Z-ADD     PiVal         VABCCM
031100020204     C                   ENDIF
031200040301     C                   ENDIF
031300040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031500020204     C                   EXSR      CHKNUM
031600020204     C                   IF        PiInt=*on
031700020204     C                   Z-ADD     PiVal         VABLNP
031800020204     C                   ENDIF
031900040301     C                   ENDIF
032000040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032200020204     C                   EXSR      CHKNUM
032300020204     C                   IF        PiInt=*on
032400020204     C                   Z-ADD     PiVal         VABCTR
032500040301     C                   ENDIF
032600020204     C                   ENDIF
032700020204     C                   ENDIF
032800020204     C*
032900010330     C                   ENDSR
033000010607     C*----------------------------------------------------*
033100050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033200010607     C*----------------------------------------------------*
033300010607     C     IMPVAB        BEGSR
033400010607     C*
033500010607     C                   SETOFF                                       3132
033600010607     C*
033700010607     C* Reperimento campi ALFA
033800010607     C*
033900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034000060630     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
034100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034300020117     C* ==
034400060630     C                   EVAL      VABRD2='Ordine Canon-'+
034500060713     C                                    %trim(%subst(vindta:35:15))
034600060630     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
034700060630     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
034800060630     C                   EVAL      VABPRD=%trim(%subst(vindta:314:2))
034900060630     C                   EVAL      VABRMA=%trim(%subst(vindta:60:11))
035000060630     C                   EVAL      VABNOT=%trim(%subst(vindta:491:35))
035100060630     C                   EVAL      VABNT2=%trim(%subst(vindta:491+35:40-35))
035200060630     C                   EVAL      VABCBO='1'
035300060630     C                   IF        %trim(%subst(vindta:2:1)) = '7'
035400060630     C                   EVAL      VABCBO='2'
035500060630     C                   ENDIF
035600060630     C                   EVAL      VABVCA=%trim(%subst(vindta:401:3))
035700060510     C*
035800060510     C* Reperimento campi NUMERICI
035900060510     C                   MOVEL     DATCOR        VABAAS
036000060510     C                   MOVE      DATCOR        VABMGS
036100060510     C* NSP/RMN
036200060630     C                   EVAL      PiStr=%trim(%subst(vindta:11:20))
036300060510     C                   EXSR      CHKNUM
036400060510     C                   IF        PiInt=*on
036500060510     C                   Z-ADD     PiVal         VABNSP
036600060510     C                   Z-ADD     PiVal         VABRMN
036700060404     C                   ELSE
036800060404     C                   SETON                                        32
036900060510     C                   Z-ADD     *zeros        VABNSP
037000060510     C                   Z-ADD     1             VABRMN
037100060404     C                   EVAL      vinmsg = %trimr(vinmsg)
037200060510     C                             + ' ' + 'VABNSP VABRMN'
037300060404     C                   ENDIF
037400040420     C* CAD
037500060630     C                   EVAL      PiStr=%trim(%subst(vindta:319:8))
037600010607     C                   EXSR      CHKNUM
037700010607     C                   IF        PiInt=*on
037800020204     C                   Z-ADD     PiVal         Num5_0
037900020204     C                   MOVEL(p)  Num5_0        VABCAD
038000010607     C                   ELSE
038100010607     C                   SETON                                        32
038200020204     C                   EVAL      VABCAD = *zeros
038300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038400020204     C                             + ' ' + 'VABCAD'
038500060316     C                   ENDIF
038600040420     C* NCL
038700060630     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
038800010607     C                   EXSR      CHKNUM
038900010607     C                   IF        PiInt=*on
039000010607     C                   Z-ADD     PiVal         VABNCL
039100010607     C                   ELSE
039200010607     C                   SETON                                        32
039300010607     C                   Z-ADD     *zeros        VABNCL
039400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039500010607     C                             + ' ' + 'VABNCL'
039600010607     C                   ENDIF
039700060630     C* DCR
039800060630     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
039900060630     C                   EXSR      CHKNUM
040000060630     C                   IF        PiInt=*on
040100060630     C                   Z-ADD     PiVal         VABDCR
040200060630     C                   ELSE
040300060630     C                   SETON                                        32
040400060630     C                   Z-ADD     *zeros        VABDCR
040500060630     C                   EVAL      vinmsg = %trimr(vinmsg)
040600060630     C                             + ' ' + 'VABDCR'
040700060630     C                   ENDIF
040800040420     C* PKB
040900060630     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
041000010607     C                   EXSR      CHKNUM
041100010607     C                   IF        PiNum=*on
041200060630     C                   EVAL(H)   PiVal = PiVal / 100                          * gest. 2 decimali
041300060630     C                   Z-ADD     PiVal         VABPKB
041400010607     C                   ELSE
041500010607     C                   SETON                                        32
041600010607     C                   Z-ADD     *zeros        VABPKB
041700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041800010607     C                             + ' ' + 'VABPKB'
041900010607     C                   ENDIF
042000060516     C* CAS
042100060630     C                   IF        %trim(%subst(vindta:388:13)) <> *all'0'
042200060516     C                   EVAL      FlgCAS = '1'
042300060630     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
042400060516     C                   EXSR      CHKNUM
042500060516     C                   IF        PiNum=*on
042600060630     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
042700060516     C                   Z-ADD     PiVal         VABCAS
042800060516     C                   ELSE
042900060516     C                   SETON                                        32
043000060629     C                   Z-ADD     *zeros        VABCAS
043100060516     C                   EVAL      vinmsg = %trimr(vinmsg)
043200060516     C                             + ' ' + 'VABCAS'
043300060516     C                   ENDIF
043400060516     C                   ENDIF
043500020322     C*
043600020322     C* Considerazioni finali su CBO/CAS
043700050908     C                   IF        FlgCAS = '1'
043800040713     C                   IF        VABCBO = '1'
043900020322     C                   EVAL      VABCBO = '4'
044000040713     C                   ENDIF
044100040713     C                   IF        VABCBO = '2'
044200040713     C                   EVAL      VABCBO = '6'
044300040713     C                   ENDIF
044400020322     C                   ENDIF
044500020204     C*
044600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044700020204     C                   EXSR      CHKIMPDIV
044800020204     C*
044900010607     C                   ENDSR
045000010607     C*----------------------------------------------------*
045100010601
045200020204
045300020204     C*----------------------------------------------------*
045400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045500020204     C*----------------------------------------------------*
045600020204     C     CHKIMPDIV     BEGSR
045700020204     C*
045800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045900020204     C                   Z-ADD     *zeros        wrkDec            9 9
046000020204     C*
046100020204     C* Come prima cosa effettuo considerazioni sulla divisa
046200020204     C                   IF        vabIAS > *zeros
046300020204     C                   IF        vabVAS <> 'EUR'
046400020204     C                   EVAL      vabVAS =  'ITL'
046500020204     C                   ENDIF
046600020204     C                   ENDIF
046700020204     C*
046800020204     C                   IF        vabCAS > *zeros
046900020204     C                   IF        vabVCA <> 'EUR'
047000020204     C                   EVAL      vabVCA =  'ITL'
047100020204     C                   ENDIF
047200020204     C                   ENDIF
047300020204     C*
047400020204     C                   IF        vabVMD > *zeros
047500020321     C                   IF        vabVAD <> 'EUR'
047600020204     C                   EVAL      vabVAD =  'ITL'
047700020204     C                   ENDIF
047800020204     C                   ENDIF
047900020204     C*
048000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048100020204     C                   Z-ADD     vabIAS        wrkDec
048200020204     C                   IF        wrkDec > *zeros
048300020204     C                   IF        vabVAS = 'ITL'
048400020204     C                   EVAL      vabIAS = *zeros
048500020204     C                   ENDIF
048600020204     C                   ENDIF
048700020204     C*
048800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
048900020204     C                   Z-ADD     vabCAS        wrkDec
049000020204     C                   IF        wrkDec > *zeros
049100020204     C                   IF        vabVCA = 'ITL'
049200020204     C                   EVAL      vabCAS = *zeros
049300020204     C                   ENDIF
049400020204     C                   ENDIF
049500020204     C*
049600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049700020204     C                   Z-ADD     vabVMD        wrkDec
049800020204     C                   IF        wrkDec > *zeros
049900020204     C                   IF        vabVAD = 'ITL'
050000020204     C                   EVAL      vabVMD = *zeros
050100020204     C                   ENDIF
050200020204     C                   ENDIF
050300020204     C*
050400020204     C                   ENDSR
050500020204     C***
050600020204
050700010330
050800010330
050900010330     C*----------------------------------------------------*
051000010330     C*  CONTROLLO NUMERICITA' CAMPI
051100010330     C*----------------------------------------------------*
051200010330     C     CHKNUM        BEGSR
051300010330     C*
051400010606     C                   IF        PiDecChr = *blanks
051500060510     C                   EVAL      PiDecChr = ','
051600010606     C                   ENDIF
051700010606     C*
051800010606     C                   CALL(e)   'ISNUMERIC'
051900010330     C                   PARM                    PiStr            30
052000010606     C                   PARM                    PiDecChr          1
052100010330     C                   PARM      *ZEROS        PiVal            30 9
052200010330     C                   PARM      '0'           PiInt             1
052300010330     C                   PARM      '0'           PiNum             1
052400010330     C                   IF        %error
052500010606     C                   EVAL      PiNum=*off
052600010330     C                   ENDIF
052700010330     C*
052800010330     C                   ENDSR
052900010330     C***
053000010330
053100010601
053200010601
053300010601
053400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053500010601     C     repfil        BEGSR
053600010601     C*
053700010601     C                   if        invfil = *zeros and
053800010601     C                             depfil > *zeros and
053900010629     C                             (vinflg = *blanks or
054000010629     C                              vinflg = *zeros)
054100010601     C
054200010601     C                   eval      invfil = depfil
054300010601     C                   endif
054400010601     C*
054500010601     C                   if        depfil <> invfil and
054600010601     C                             invfil > *zeros
054700010601     C                   eval      flgMulti = '1'
054800010601     C                   if        vinflg = *blanks
054900010601     C                   add       1             cntNonEl
055000010601     C                   endif
055100010601     C                   endif
055200010601     C*
055300010601     C                   if        vinflg = '2'
055400010601     C                   eval      flgStato = '2'
055500010601     C                   endif
055600010601     C*
055700010601     C                   ENDSR
055800010601     C***
055900010601
056000010601
056100010601
056200010330
056300010330
056400010330
056500990920      /TITLE Invio dei dati al punto operativo.
056600000613     C     invio         BEGSR
056700990920     C*
056800990920     C                   reset                   dscmz
056900010601     C                   move      invfil        cmzdst
057000990920     C                   eval      cmzfld = vlrfou
057100990920     C                   eval      cmzmbd = vlrhdl
057200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
057300000710     C                   if        prmfir = *blanks
057400050408     C                   eval      cmzfla = 'FIVAB00F'
057500050408     C                   eval      cmzmba = 'FIVAB00F'
057600000710     C                   else
057700000710     C                   eval      cmzfla = prmfir
057800000710     C                   eval      cmzmba = prmfir
057900000710     C                   endif
058000990920     C                   eval      cmznrr = *zeros
058100990920     C                   move      §ctrok        cmznrr
058200021018     C                   eval      cmzlba = vlrfl1
058300990920     C                   call(e)   'TIS711C'
058400990920     C                   parm                    dscmz
058500990921     C                   parm      *blanks       esito
058600990920     C                   if        %error
058700990920     C                             or cmzerr = '1'
058800990921     C                             or esito  = '1'
058900000710     C                   eval      wrkesito = '3'
059000990920     C                   endif
059100990920     C*
059200000613     C                   ENDSR
059300990910
059400010601
059500010601
059600010601
059700010601
059800010601      /TITLE Invio dei dati al punto operativo.
059900010601     C     opeini        BEGSR
060000010601     C*
060100010601     C* Inizializzo flag e contatori operativi
060200010601     C                   movel     '0'           flgGiro           1
060300010601     C                   movel     '0'           flgMulti          1
060400010601     C                   movel     '1'           flgStato          1
060500010615     C                   movel     '0'           flgOk             1
060600010601     C                   z-add     *zeros        cntNonEl         10 0
060700010601     C                   z-add     *zeros        depfil            3 0
060800010601     C                   z-add     *zeros        invfil            3 0
060900010601     C*
061000010601     C                   ENDSR
061100010601     C***
061200010601
061300010601
061400010601
061500010330
061600010330
061700000613     C     *inzsr        BEGSR
061800990910     C*
061900990910     C     *entry        plist
062000990920     C                   parm                    tivlrds
062100990921     C                   parm      wrkesito      esito
062200000724     C                   parm                    prmlit
062300000710     C                   parm                    prmfir
062400010330     C*
062500010330     C* CALCOLA LA DATA CORRENTE
062600010330     C                   time                    wn14             14 0
062700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062800010330     C                   z-add     wn8           g08dat
062900010330     C                   z-add     *zeros        g08inv
063000010330     C                   movel     '0'           g08err
063100010330     C                   call      'XSRDA8'
063200010330     C                   parm                    wlbda8
063300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063400000613     C*
063500000613     C                   ENDSR
063600000613     C***
063700990908
063800060630     Otitvt7p   E            riepilogo         2
063900990915     O                                              'Upload via Internet'
064000990915     O                                           +1 'Traduzione TIVIN00R -'
064100050408     O                                           +1 'FIVABWWR'
064200990915     O                                           +1 'Testate bolle'
064300990915     O          E            riepilogo   2
064400990915     O                       wrkdata
064500990915     O                       wrkora              +1
064600990915     O                       procname            +1
064700990915     O          E            riepilogo   2
064800990915     O                                              'Cliente..................:'
064900010605     O                       VABCCM        z     +1
065000990915     O          E            riepilogo   2
065100990920     O                                              'Riferimento Strategi.....:'
065200990920     O                       vlrhdl              +1
065300990915     O          E            riepilogo   2
065400990915     O                                              'Giusti...................:'
065500971022     O                       §CTROK        2   +  1
065600990915     O          E            riepilogo   2
065700990915     O                                              'Sbagliati e corretti.....:'
065800971022     O                       §CTRMO        2   +  1
065900990915     O          E            riepilogo   2
066000990915     O                                              'Sbagliati e scartati.....:'
066100971022     O                       §CTRNO        2   +  1
066200000613
066300060630     Otitvt7ps  E            testdett          2
066400000613     O                                              'Upload via Internet'
066500000613     O                                           +1 'Traduzione TIVIN00R -'
066600050408     O                                           +1 'FIVABWWR'
066700000613     O                                           +1 'Testate bolle'
066800000616     O          E            testdett    3
066900000613     O                                           +2 'N° rec'
067000000613     O                                           +3 'Anteprima contenuto'
067100000616     O          E            rigadett    2
067200000613     O                       rrnum               +2
067300000621     O                       recko               +3
067400000616     O          E            findett     2
067500000613     O                       wrkdata
067600000613     O                       wrkora              +1
067700000613     O                       procname            +1
067800000616     O          E            findett     2
067900000613     O                                              'Cliente..................:'
068000010605     O                       VABCCM        z     +1
068100000616     O          E            findett     2
068200000613     O                                              'Riferimento Strategi.....:'
068300000613     O                       vlrhdl              +1
068400000616     O          E            findett     2
068500000613     O                                              'Giusti...................:'
068600000613     O                       §CTROK        2   +  1
068700000616     O          E            findett     2
068800000613     O                                              'Sbagliati e corretti.....:'
068900000613     O                       §CTRMO        2   +  1
069000000616     O          E            findett     2
069100000613     O                                              'Sbagliati e scartati.....:'
069200000613     O                       §CTRNO        2   +  1
069300000613** CMD - COMANDI CL
069400060630OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
069500060630OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
069600060630DLTOVR FILE(TITVT7P TITVT7PS) LVL(*)
069700010606
069800000613
